LIS_CurScrnGrp	DW	(?)		; Screengroup
LIS_SubScrnGrp	DW	(?)		; Subscreen group
;;LIS_ForgndFlg	DW	(?)		; Current process is in foreground
LIS_Fgnd	DW	(?)		; ;      SCCSID = @(#)linmgr1.asm	10.1 87/02/26	
PAGE	,132
TITLE	LINMGR1 - Linear Memory Manager
NAME	LINMGR1

;*
;*  Title:
;*
;*	LINMGR1
;*
;*
;*  Author:
;*
;*	Pete Stewart
;*	(c) Microsoft Corporation
;*	1987
;*
;*
;*  Description:
;*
;*	LINMGR1 contains the following externally visible
;*	Linear Memory Manager functions:
;*
;*		PhysAlloc
;*		PhysFree
;*		PhysRealloc
;*
;*	It also contains the worker functions used by these
;*	routines.
;*
;*
;*  Modification History:
;*

.xlist
include dosseg.inc
include dosmac.inc
include pmode.inc
include page.inc
include mhandle.inc
include linmgr.inc
include sysinvar.inc
.list
.386p


	EXTRNFAR	NPX287Wait		; Wait for exception if any
	EXTRNFAR	PageControl		; Page control function
	EXTRNFAR	PageMovePTE		; Move page table entries


	ReferHighGlobal GDT_LinearSpace,QWORD	; Linear memory descriptor


;	Some of the following data items are assumed to be zero initialized.
;	The sysinit boot loader ensures that unitialized data is zeroed.
;	NOTE: ADTable must not be at offset 0 in the ARENADATA segment.

ARENADATA segment
	ASSUME	DS:ARENADATA

	PUBLIC	ADFreeList, ADFirst, ADLast, ADFirstFree, ADLastFree
	PUBLIC	ADMap, ADAllocBits

ADMap		dw	MAXLINEARPAGE dup(?) ; Linear address translation table
ADTable		db	MAXARENADESC*size ArenaDescriptor dup(?)
					; Arena Descriptors
ADFreeList	dw	1 dup(?)	; Free descriptor list
ADFirst		dw	1 dup(?)	; First descriptor in list
ADLast		dw	1 dup(?)	; Last descriptor in list
					; (Always points to end sentinel)
ADFirstFree	dw	1 dup(?)	; First free segment descriptor
ADLastFree	dw	1 dup(?)	; Last free segment descriptor
					; (Always points to end sentinel)
ADAllocBits	dw	1 dup(?)	; Allocation force bits

ARENADATA ends


	FARCODE INIT
INITCODE segment


;***	LinInitFreeList - initialize list of free descriptors
;
;	This function is called during system initialization
;	to link all of the descriptors in ADTable onto the
;	ADFreeList.
;
;	ENTRY	DS = ARENADATA
;	EXIT	NONE
;	USES	AX, BX, CX, Flags

Procedure LinInitFreeList,near
	ASSUME	DS:ARENADATA, ES:NOTHING, SS:NOTHING

	mov	ax,offset ARENADATA:ADTable ; DS:AX = first descriptor
	mov	cx,MAXARENADESC		; CX = no. of descriptors
	mov	ADFreeList,ax		; Initialize free list pointer
	jmp	short aif1		; Jump into loop
aif0:	mov	[bx].ad_next,ax		; Set link to next descriptor
aif1:	mov	bx,ax			; BX = pointer to next descriptor
	add	ax,size ArenaDescriptor	; AX = following descriptor
	loop	aif0			; Loop until CX = 0
	mov	[bx].ad_next,cx		; Mark end of list
	ret

EndProc LinInitFreeList


;***	PhysInit - initialize linear memory arena
;
;	This procedure is called to initialize the linear
;	memory arena.  It also initializes the data
;	structures and variables used by the Linear
;	Memory Manager.
;
;	ENTRY	(ES:DI) = pointer to array describing arena
;			  Each entry consists of a DWORD
;			  containing the linear address,
;			  a WORD containing flags, and a
;			  WORD containing a selector.
;		(AX) = initial handle for all blocks
;		(BX) = initial owner for all blocks
;	EXIT	NONE
;	USES	Flags
;
;	PROCEDURE PhysInit(arena: POINTER; handle: WORD; owner: WORD);
;	  BEGIN
;	    prev := NULL;
;	    prevfree := NULL;
;	    LinInitFreeList;
;	    REPEAT
;	      BEGIN
;		desc := LinAllocDescriptor(arena^.a_paddr);
;		IF prev <> NULL THEN
;		  prev^.ad_next := desc
;		ELSE ADFirst := desc;
;		desc^.ad_prev := prev;
;		prev := desc;
;		IF (arena^.a_flags & A_FREE) <> 0 THEN
;		  BEGIN
;		    desc^.ad_flags := AD_FREE;
;		    IF prevfree <> NULL THEN
;		      prevfree^.ad_nextfree := desc;
;		    ELSE ADFirstFree := desc;
;		    desc^.ad_prevfree := prevfree;
;		    prevfree := desc;
;		  END
;		ELSE
;		  BEGIN
;		    desc^.ad_flags := arena^.a_flags;
;		    desc^.ad_flags := desc^.ad_flags &
;		      (AD_FIXED + AD_SWAPPABLE + AD_DISCARDABLE + AD_SHARED);
;		    IF (desc^.ad_flags &
;		      (AD_FIXED + ADSWAPPABLE + AD_DISCARDABLE)) = 0 THEN
;		      desc^.ad_flags := desc^.ad_flags | AD_RESIDENT;
;		    desc^.ad_handle := handle;
;		    desc^.ad_owner := owner;
;		  END;
;		arena := arena + 1;
;	      END
;	    UNTIL ((arena - 1)^.a_flags & A_END) <> 0;
;	    ADLast := prev;
;	    prev^.ad_next := NULL;
;	    IF (prev^.ad_linaddr & (PAGESIZE - 1)) != 0 THEN
;	      InternalError('End sentinel not page-aligned');
;	    ADMap[prev^.ad_linaddr >> PAGESHIFT] := 0;
;	    ADLastFree := prevfree;
;	    IF prevfree <> NULL THEN
;	      prevfree^.ad_nextfree := NULL;
;	  END;

Procedure PhysInit,near
	ASSUME	DS:NOTHING, ES:NOTHING, SS:NOTHING

	localvar li_handle,WORD		; handle = (AX)
	localvar li_owner,WORD		; owner = (BX)
	localvar li_prev,WORD		; previous descriptor = 0
	localvar li_prevfree,WORD	; previous free descriptor = 0

	EnterProc <ax,bx,0,0>
	pushad				; Save ALL registers
	push	ds
	push	es
	ArenaContext ds
	call	LinInitFreeList		; Build free descriptor list

;	Loop to build initial arena
;
;	Allocate descriptor and link block into list.

li0:	mov	eax,es:[di].a_paddr	; (EAX) = linear address
	callfar	LinAllocDescriptor	; (BX) = descriptor for block
	mov	si,li_prev		; (SI) = descriptor for previous block
	or	si,si			; Is there a previous block?
	jnz	short li1		;  yes, skip ahead
	mov	ADFirst,bx		; Head of list
	jmp	short li2
li1:	mov	[si].ad_next,bx		; Set next link in previous block
li2:	mov	[bx].ad_prev,si		; Set previous link in this block
	mov	li_prev,bx		; New previous block

;	If block is free, link it into the free list.

	mov	ax,es:[di].a_flags	; (AX) = flags
	test	ax,A_FREE		; Is block free?
	jz	short li5		;  no, skip ahead
	mov	[bx].ad_flags,AD_FREE	; Mark block as free
	mov	si,li_prevfree		; (SI) = previous free block descriptor
	or	si,si			; Is there a previous free block?
	jnz	short li3		;  yes, skip ahead
	mov	ADFirstFree,bx		; Head of list
	jmp	short li4
li3:	mov	[si].ad_nextfree,bx	; Set next link in previous block
li4:	mov	[bx].ad_prevfree,si	; Set previous link in this block
	mov	li_prevfree,bx		; New previous free block
	jmp	short li7

;	Else set flags, handle, owner.

li5:	and	ax,AD_FIXED+AD_SWAPPABLE+AD_DISCARDABLE+AD_SHARED
	test	ax,AD_FIXED+AD_SWAPPABLE+AD_DISCARDABLE ; Removable or fixed?
	jnz	short li6		;  yes, skip ahead
	or	ax,AD_RESIDENT		; Set resident flag
li6:	mov	[bx].ad_flags,ax	; Set flags
	mov	ax,li_handle		; Set handle
	mov	[bx].ad_handle,ax
	mov	ax,li_owner		; Set owner
	mov	[bx].ad_owner,ax

;	Check if end of array reached.  If not, loop.

li7:	add	di,size arena		; Increment pointer
	test	es:[di - (size arena)].a_flags,A_END ; End sentinel found?
	jz	short li0		;  no, loop

;	Finish list initialization.

	mov	bx,li_prev		; (BX) = descriptor of end sentinel
ifndef PRODUCTION
	test	[bx].ad_flags,AD_FREE	; Free block?
	jnz	short liie		;  yes, internal error
endif
	mov	ADLast,bx		; Set pointer to last block
	mov	[bx].ad_next,0		; End sentinel has no next link
	mov	eax,[bx].ad_linaddr	; (EAX) = end sentinel linear address
ifndef PRODUCTION
	test	ax,PAGESIZE - 1		; Page-aligned?
	jnz	short liie		;  no, internal error
endif
	shld	ebx,eax,33 - PAGESHIFT	; (BX) = linear page number * 2
	mov	ADMap[bx],0		; End sentinel cannot be mapped
	mov	bx,li_prevfree		; (BX) = end of free list descriptor
	mov	ADLastFree,bx		; Set pointer to last free block
	or	bx,bx			; Free list empty?
	jz	short li8		;  yes, skip ahead
	mov	[bx].ad_nextfree,0	; Mark end of free list

;	Exit.

li8:	pop	es			; Restore ALL registers
	pop	ds
	popad
	LeaveProc
Entry PhysReinit			; For compatibility
	ret

ifndef PRODUCTION
liie:	InternalError <LinInit>
endif

EndProc PhysInit


;***	PhysEnterMem - add a block of linear memory to the arena
;
;	This function is called to add a block of linear memory
;	to the arena.  There are several possible cases:
;
;	1) The new block is below the first block in the arena.
;	2) The new block is above the last block in the arena.
;	3) The new block is a portion of an existing block;
;	   it is either:
;	   (a) the beginning,
;	   (b) the middle,
;	   (c) the end, or
;	   (d) the entire block.
;	4) The new block spans several existing blocks.
;
;	Each case has its odd twists, though in practical use, (3b)
;	is probably the most common.  Since this function is complicated,
;	nonessential, and since the very need for it at all could have
;	been eliminated if a little more care had gone into the design
;	of those components that use it, we will punt it entirely for
;	now and always return successfully without actually doing
;	anything.  By the way, the function is hybrid because its users
;	make far calls to it even though they are in the same code
;	segment.  Thanks, T.M. and I.S.!
;
;	ENTRY	(AX:BX) = linear address
;		(CX:DX) = size
;	EXIT	Carry clear
;	USES	Flags
;
;	FUNCTION PhysEnterMem(linaddr, size: DWORD): BOOL;
;	  BEGIN
;	    RETURN TRUE;
;	  END;

Procedure PhysEnterMem,hybrid
	ASSUME	DS:NOTHING, ES:NOTHING, SS:NOTHING

	clc
	ret

EndProc PhysEnterMem


INITCODE ends


	FARCODE	HIGH
HIGHCODE segment


;***	LinVerifyBlockAddress - verify that address corresponds to a block
;
;	This function takes a linear address and returns a pointer
;	to the descriptor that describes that block of memory.  It
;	is an internal error to pass in a linear address that does
;	not have a descriptor associated with it.
;
;	ENTRY	EAX = linear address
;		DS = arena segment selector
;	EXIT	Carry clear
;		    DS:BX = pointer to arena descriptor
;		Carry set
;		    Linear address is invalid
;	USES	EBX, Flags
;
;	FUNCTION LinVerifyBlockAddress(linaddr: DWORD): POINTER;
;	  BEGIN
;	    IF (linaddr & (PAGESIZE - 1)) != 0 THEN
;	      desc := NULL;
;	    ELSE
;	      desc := ADMap[linaddr >> PAGESHIFT];
;	    IF (desc <> NULL) AND ((desc^.ad_flags & AD_FREE) = 0) THEN
;	      RETURN desc;
;	    ELSE RETURN NULL;
;	  END;

Procedure LinVerifyBlockAddress,near
	ASSUME	DS:ARENADATA, ES:NOTHING, SS:NOTHING

	test	ax,PAGESIZE - 1		; Segment is page-aligned?
	stc				;  (assume no)
	jnz	short lvba0		;  no, return carry set
	shld	ebx,eax,33 - PAGESHIFT	; (BX) = linear page number * 2
	mov	bx,ADMap[bx]		; (BX) = descriptor offset
	cmp	bx,1			; Does mapping exist?
	jc	lvba0			;  no, return carry set
	bt	[bx].ad_flags,12	; Load carry with AD_FREE flag
	.errnz	(AD_FREE - (1 SHL 12))	; (Berify bit number)
lvba0:	ret				; DS:BX = desired descriptor

EndProc LinVerifyBlockAddress


;***	LinMapDescriptor - map a linear address to an arena descriptor
;
;	This function takes a linear address and returns a pointer
;	to the descriptor that describes that block of memory.  It
;	is an internal error to pass in a linear address that does
;	not have a descriptor associated with it or that maps to a
;	free block.
;
;	ENTRY	EAX = linear address
;		DS = arena segment selector
;	EXIT	DS:BX = pointer to arena descriptor
;	USES	EBX, Flags
;
;	FUNCTION LinMapDescriptor(linaddr: DWORD): POINTER;
;	  BEGIN
;	    desc := LinVerifyBlockAddress(linaddr);
;	    if (desc = NULL) THEN
;	      InternalError('Bad address');
;	    RETURN desc;
;	  END;

Procedure LinMapDescriptor,hybrid
	ASSUME	DS:ARENADATA, ES:NOTHING, SS:NOTHING

	call	LinVerifyBlockAddress	; Verify address corresponds to block
	jc	short lmdie		;  internal error if it does not
	ret				; DS:BX = desired descriptor

lmdie:	InternalError <LinMapDescriptor>

EndProc LinMapDescriptor


;***	LinAllocDescriptor - allocate arena descriptor
;
;	This function allocates an arena descriptor
;	given the linear address of the start of the block.
;	It is an internal error if there are no free descriptors.
;	The linear address field in the descriptor is filled in,
;	and if the linear address is page-aligned, then the
;	mapping from linear address to descriptor is set up.
;
;	ENTRY	EAX = linear address 
;		DS = arena segment selector
;	EXIT	DS:BX = pointer to descriptor
;	USES	BX, ESI, Flags
;
;	FUNCTION LinAllocDescriptor(linaddr: DWORD): POINTER;
;	  BEGIN
;	    descriptor := ADFreeList;
;	    IF descriptor = 0 THEN InternalError('No free descriptors');
;	    ADFreeList := ADFreeList^.ad_next;
;	    descriptor^.ad_linaddr := linaddr;
;	    ADMap[linaddr >> PAGESHIFT] := descriptor;
;	    RETURN descriptor;
;	  END;

Procedure LinAllocDescriptor,hybrid
	ASSUME	DS:ARENADATA, ES:NOTHING, SS:NOTHING

	mov	bx,ADFreeList		; BX = offset of free descriptor
	or	bx,bx			; Free list empty?
	jz	short ladie		;  yes, internal error
	mov	si,[bx].ad_next		; SI = offset of next free descriptor
	mov	ADFreeList,si		; Unlink descriptor from free list
	mov	[bx].ad_linaddr,eax	; Set linear address in descriptor
ifndef PRODUCTION
	test	ax,PAGESIZE - 1		; Segment is page-aligned?
	jnz	short ladie		;  no, internal error
endif
	shld	esi,eax,33 - PAGESHIFT	; SI = linear page number * 2
ifndef PRODUCTION
	cmp	si,MAXLINEARPAGE*2	; Page number out of range
	jae	short ladie		;  yes, internal error
	cmp	ADMap[si],0		; Existing mapping?
	jnz	short ladie		;  yes, internal error
endif
	mov	ADMap[si],bx		; Set mapping
	ret

ladie:	InternalError <LinAllocDescriptor>

EndProc LinAllocDescriptor


;***	LinFreeDescriptor - free arena descriptor
;
;	This function frees an arena descriptor
;	given a pointer to it.  If the linear address
;	is page-aligned, then the mapping from linear
;	address to descriptor is broken.
;
;	ENTRY	DS:BX = pointer to descriptor
;	EXIT	NONE
;	USES	EAX, EBX, Flags
;
;	PROCEDURE LinFreeDescriptor(descriptor: POINTER);
;	  BEGIN
;	    ADMap[descriptor^.ad_linaddr >> PAGESHIFT] := 0;
;	    descriptor^.ad_next := freeList;
;	    freeList := descriptor;
;	  END;

Procedure LinFreeDescriptor,near
	ASSUME	DS:ARENADATA, ES:NOTHING, SS:NOTHING

	mov	ax,bx			; Link descriptor into free list
	xchg	ax,ADFreeList
	mov	[bx].ad_next,ax
	mov	eax,[bx].ad_linaddr	; EAX = linear address of segment
ifndef PRODUCTION
	test	ax,PAGESIZE - 1		; Page-aligned?
	jnz	short lfdie		;  no, exit
endif
	shld	ebx,eax,33 - PAGESHIFT	; BX = linear page number * 2
ifndef PRODUCTION
	cmp	bx,MAXLINEARPAGE*2	; Page number out of range?
	jae	short lfdie		;  yes, internal error
	cmp	ADMap[bx],0		; Mapping exists?
	jz	short lfdie		;  no, internal error
endif
	mov	ADMap[bx],0		; Break mapping
	ret

ifndef PRODUCTION
lfdie:	InternalError <LinFreeDescriptor>
endif

EndProc LinFreeDescriptor


;***	LinFreeSearchUp - search for free block from bottom to top
;
;	This function is called to find a free block whose
;	size is greater than or equal to the given size.
;	It searches from the first (lowest) free block to
;	the last (highest), and, optionally, it will stop
;	searching when the one megabyte border is crossed.
;
;	ENTRY	EAX = size of block needed
;		DS = ARENADATA
;		CX = allocation flags
;	EXIT	Carry clear
;		    DS:BX = pointer to free block arena descriptor
;		    DS:SI = pointer to descriptor of following block
;		    EDX = size of free block
;		Carry set
;		    Could not satisfy request
;	USES	BX, EDX, SI, Flags
;
;	FUNCTION LinFreeSearchUp(sizereq: DWORD; flags: WORD):
;	  (POINTER,POINTER,DWORD);
;	  BEGIN
;	    desc := ADFirstFree;
;	    WHILE desc <> 0 DO
;	      BEGIN
;		IF (desc^.ad_flags & AD_FREE) = 0 THEN
;		  InternalError('Busy block on free list');
;		IF ((flags & AD_LOWMEM) <> 0) AND
;		  (desc^.ad_next^.ad_linaddr > 100000h) THEN BREAK
;		ELSE
;		  BEGIN
;		    size := desc^.ad_next^.ad_linaddr -
;		      desc^.ad_linaddr;
;		    IF size >= sizereq THEN RETURN (desc, desc^.ad_next, size)
;		    ELSE desc := desc^.ad_nextfree;
;		  END;
;	      END;
;	    RETURN (NULL, NULL, 0);
;	  END;

Procedure LinFreeSearchUp,near
	ASSUME	DS:ARENADATA, ES:NOTHING, SS:NOTHING

	mov	bx,ADFirstFree		; BX = descriptor of 1st free block
lfsu0:	or	bx,bx			; End of list?
	jz	short lfsu2		;  yes, exit loop
ifndef PRODUCTION
	test	[bx].ad_flags,AD_FREE	; Block free?
	jz	short lfsuie		;  no, internal error
endif
	mov	si,[bx].ad_next		; SI = descriptor of following block
ifndef PRODUCTION
	or	si,si			; Null?
	jz	short lfsuie		;  yes, internal error
endif
	test	cx,AD_LOWMEM		; Quit at 1 meg?
	jz	short lfsu1		;  no, skip ahead
	cmp	[si].ad_linaddr,100000h	; At one meg yet?
	ja	short lfsu2		;  yes, exit loop
lfsu1:	mov	edx,[si].ad_linaddr	; EDX = address of following block
	sub	edx,[bx].ad_linaddr	; EDX = length of block
ifndef PRODUCTION
	jbe	short lfsuie		;  internal error if zero or underflow
endif
	cmp	edx,eax			; Is free block large enough?
	jnc	short lfsu3		;  yes, exit
	mov	bx,[bx].ad_nextfree	; BX = descriptor of next free block
	jmp	short lfsu0		; loop
lfsu2:	stc				; Failure
lfsu3:	ret

ifndef PRODUCTION
lfsuie:	InternalError <LinFreeSearchUp>
endif

EndProc LinFreeSearchUp


;***	LinFreeSearchDown - search for free block from top to bottom
;
;	This function is called to find a free block whose
;	size is greater than or equal to the given size.
;	It searches from the last (highest) free block to
;	the first (lowest), and, optionally, it will stop
;	searching when the one megabyte border is crossed.
;
;	ENTRY	EAX = size of block needed
;		DS = ARENADATA
;		CX = allocation flags
;	EXIT	Carry clear
;		    DS:BX = pointer to free block arena descriptor
;		    DS:SI = pointer to descriptor of following block
;		    EDX = size of free block
;		Carry set
;		    Could not satisfy request
;	USES	BX, EDX, SI, Flags
;
;	FUNCTION LinFreeSearchDown(sizereq: DWORD; flags: WORD):
;	  (POINTER,POINTER,DWORD);
;	  BEGIN
;	    desc := ADLastFree;
;	    WHILE desc <> 0 DO
;	      BEGIN
;		IF (desc^.ad_flags & AD_FREE) = 0 THEN
;		  InternalError('Busy block on free list');
;		IF ((flags & AD_HIGHMEM) <> 0) AND
;		  (desc^.ad_linaddr < 100000h) THEN BREAK
;		ELSE
;		  BEGIN
;		    size := desc^.ad_next^.ad_linaddr -
;		      desc^.ad_linaddr;
;		    IF size >= sizereq THEN RETURN (desc, desc^.ad_next, size)
;		    ELSE desc := desc^.ad_prevfree;
;		  END;
;	      END;
;	    RETURN (NULL, NULL, 0);
;	  END;

Procedure LinFreeSearchDown,near
	ASSUME	DS:ARENADATA, ES:NOTHING, SS:NOTHING

	mov	bx,ADLastFree		; BX = descriptor of last free block
lfsd0:	cmp	bx,1			; End of list?
	jc	short lfsd2		;  yes, failure
ifndef PRODUCTION
	test	[bx].ad_flags,AD_FREE	; Is block free?
	jz	short lfsdie		;  no, internal error
endif
	test	cx,AD_HIGHMEM		; Quit at one meg?
	jz	short lfsd1		;  no, skip ahead
	cmp	[bx].ad_linaddr,100000h	; Below one meg?
	jc	short lfsd2		;  yes, failure
lfsd1:	mov	si,[bx].ad_next		; SI = descriptor of following block
	mov	edx,[si].ad_linaddr	; EDX = address of following block
	sub	edx,[bx].ad_linaddr	; EDX = length of free block
ifndef PRODUCTION
	jbe	short lfsdie		;  internal error if zero or underflow
endif
	cmp	edx,eax			; Is block large enough?
	jnc	short lfsd2		;  yes, success
	mov	bx,[bx].ad_prevfree	; BX = previous free block descriptor
	jmp	short lfsd0		; Loop
lfsd2:	ret

ifndef PRODUCTION
lfsdie:	InternalError <LinFreeSearchDown>
endif

EndProc LinFreeSearchDown


;***	LinInsertFree - insert descriptor in free block list
;
;	This procedure is called to place the descriptor for
;	a free block in the list of free blocks.
;
;	ENTRY	DS:BX = pointer to descriptor of free block
;	EXIT	NONE
;	USES	DI, SI, Flags
;
;	PROCEDURE LinInsertFree(desc: POINTER);
;	  BEGIN
;	    prev := desc^.ad_prev;
;	    WHILE prev <> 0 DO
;	      BEGIN
;		IF (prev^.ad_flags & AD_FREE) <> 0 THEN BREAK
;		ELSE prev := prev^.ad_prev;
;	      END;
;	    desc^.ad_prevfree := prev;
;	    IF prev = 0 THEN
;	      BEGIN
;		desc^.ad_nextfree := ADFirstFree;
;		ADFirstFree := desc;
;	      END
;	    ELSE
;	      BEGIN
;		desc^.ad_nextfree := prev^.ad_nextfree;
;		prev^.ad_nextfree := desc;
;	      END;
;	    IF desc^.ad_nextfree = 0 THEN
;	      ADLastFree := desc;
;	    ELSE desc^.ad_nextfree^.ad_prevfree := desc;
;	  END;

Procedure LinInsertFree,near
	ASSUME	DS:ARENADATA, ES:NOTHING, SS:NOTHING

	mov	di,bx			; (DI) = descriptor of free block
lif0:	mov	di,[di].ad_prev		; (DI) = descriptor of previous block
	or	di,di			; End of list?
	jz	short lif1		;  yes, exit loop
	test	[di].ad_flags,AD_FREE	; Free block found?
	jz	short lif0		;  no, loop
lif1:	mov	[bx].ad_prevfree,di	; Set previous free link
	mov	si,bx			; (SI) = original descriptor
	or	di,di			; Previous free block?
	jnz	short lif2		;  yes, skip ahead
	xchg	si,ADFirstFree		; New head of free list
	jmp	short lif3
lif2:	xchg	si,[di].ad_nextfree	; Insert in middle of list
lif3:	mov	[bx].ad_nextfree,si
	or	si,si			; End of free list?
	jnz	short lif4		;  no, skip ahead
	mov	ADLastFree,bx		; New tail of free list
	jmp	short lif5
lif4:	mov	[si].ad_prevfree,bx
lif5:	ret

ifndef PRODUCTION
lifie:	InternalError <LinInsertFree>
endif

EndProc LinInsertFree


;***	LinBreakBlock - break a block into two blocks
;
;	This function breaks the specified block at the
;	specified linear address.  The new block is linked
;	into the list of blocks, and if it is to be a free
;	block, it is linked into the list of free blocks
;	as well.
;
;	ENTRY	DS:BX = pointer to arena descriptor of block to break
;		EAX = linear address of new block
;		CX = flags for new block
;	EXIT	DS:BX = pointer to arena descriptor of new block
;	USES	BX, DI, ESI, Flags
;
;	FUNCTION LinBreakBlock(olddesc: POINTER;
;			       linaddr: DWORD;
;			       flags: WORD): POINTER;
;	  BEGIN
;	    IF (linaddr <= olddesc^.ad_linaddr) OR
;	      (linaddr >= olddesc^.ad_next^.ad_linaddr) THEN
;	      InternalError('Linear address out of range');
;	    newdesc := LinAllocDescriptor(linaddr);
;	    newdesc^.ad_prev := olddesc;
;	    newdesc^.ad_next := olddesc^.ad_next;
;	    olddesc^.ad_next := newdesc;
;	    newdesc^.ad_next^.ad_prev := newdesc;
;	    newdesc^.ad_flags := flags;
;	    IF (flags & AD_FREE) <> 0 THEN
;	      LinInsertFree(newdesc);
;	    RETURN newdesc;
;	  END;

Procedure LinBreakBlock,near
	ASSUME	DS:ARENADATA, ES:NOTHING, SS:NOTHING

	mov	si,[bx].ad_next		; SI = descriptor of next block
ifndef PRODUCTION
	or	si,si			; Is there one?
	jz	short lbbie		;  no, internal error
	cmp	eax,[bx].ad_linaddr	; Range check new block address
	jbe	short lbbie
	cmp	eax,[si].ad_linaddr
	jae	short lbbie
endif
	mov	di,bx			; DI = "old" block descriptor
	push	si
	call	LinAllocDescriptor	; BX = "new" block descriptor
	pop	si
	mov	[bx].ad_prev,di		; Link new block into list
	mov	[bx].ad_next,si
	mov	[di].ad_next,bx
	mov	[si].ad_prev,bx
	mov	[bx].ad_flags,cx	; Set flags
	test	cx,AD_FREE		; Is new block free?
	jz	short lbb0		;  no, exit
	call	LinInsertFree		; Put descriptor in free block list
lbb0:	ret

ifndef PRODUCTION
lbbie:	InternalError <LinBreakBlock>
endif

EndProc LinBreakBlock


;***	LinAdjustSize - round size up to page multiple
;
;	This function rounds the specified size up to
;	the nearest multiple of a page.
;
;	ENTRY	AX:BX = size in bytes
;	EXIT	EAX = adjusted size in bytes
;	USES	EAX, Flags
;
;	FUNCTION LinAdjustSize(size: DWORD): DWORD;
;	  BEGIN
;	    RETURN (size + PAGESIZE - 1) & ~(PAGESIZE - 1);
;	  END;

Procedure LinAdjustSize,near
	ASSUME	DS:NOTHING, ES:NOTHING, SS:NOTHING

	shl	eax,16			; EAX = size
	mov	ax,bx
	add	eax,PAGESIZE - 1	; Round size up
	and	ax,NOT (PAGESIZE - 1)
	ret

EndProc LinAdjustSize


;***	LinRemFreeList - remove block from list of free blocks
;
;	This procedure is called to delete the specified
;	block from the list of free blocks.
;
;	ENTRY	DS:BX = pointer to descriptor for block
;	EXIT	NONE
;	USES	BX, SI, Flags
;
;	PROCEDURE LinRemFreeList(descriptor: POINTER);
;	  BEGIN
;	    IF desc^.ad_nextfree = 0 THEN
;	      ADLastFree := desc^.ad_prevfree;
;	    ELSE desc^.ad_nextfree^.ad_prevfree := desc^.ad_prevfree;
;	    IF desc^.ad_prevfree = 0 THEN
;	      ADFirstFree := desc^.ad_nextfree
;	    ELSE desc^.ad_prevfree^.ad_nextfree :=
;	      desc^.ad_nextfree;
;	  END;

Procedure LinRemFreeList,near
	ASSUME	DS:ARENADATA, ES:NOTHING, SS:NOTHING

ifndef PRODUCTION
	test	[bx].ad_flags,AD_FREE	; Is block free?
	jz	short lrflie		;  no, internal error
endif
	mov	si,[bx].ad_nextfree	; SI = next free block descriptor
	mov	bx,[bx].ad_prevfree	; BX = previous free block descriptor
	or	si,si			; Is there a next free block?
	jnz	short lrfl0		;  yes, skip ahead
	mov	ADLastFree,bx		; New tail of free list
	jmp	short lrfl1
lrfl0:	mov	[si].ad_prevfree,bx	; Set previous link
lrfl1:	or	bx,bx			; Is there a previous free block?
	jnz	short lrfl2		;  yes, skip ahead
	mov	ADFirstFree,si		; New head of list
	jmp	short lrfl3
lrfl2:	mov	[bx].ad_nextfree,si	; Set next link
lrfl3:	ret

ifndef PRODUCTION
lrflie:	InternalError <LinRemFreeList>
endif

EndProc LinRemFreeList


;***	LinZeroInit - zero the specified area of linear memory
;
;	This procedure zeroes the specified area of linear
;	memory.
;
;	NOTE: this procedure may block if any of the pages
;	it accesses are not present.
;
;	ENTRY	EDI = offset in linear memory to begin zeroing
;		ECX = number of bytes to initialize
;	EXIT	NONE
;	USES	EAX, ECX, EDI, ES, Flags
;
;	PROCEDURE LinZeroInit(pb: BYTEPTR; cb: DWORD);
;	  BEGIN
;	    WHILE cb <> 0 DO
;	      BEGIN
;		cb := cb - 1;
;		pb^ := 0;
;		pb := pb + 1;
;	      END;
;	  END;

Procedure LinZeroInit,near
	ASSUME	DS:NOTHING, ES:NOTHING, SS:NOTHING

ifndef PRODUCTION
	test	cl,3		; Integral number of DWORDs?
	jnz	short lziie	;  no, internal error
endif
	shr	ecx,2		; ECX = number of DWORDs to zero
ifndef PRODUCTION
	test	ecx,3FFF0000h	; Any high bits set?
	jnz	lziie		;  yes, internal error
endif
	push	GSEL GDT_LinearSpace
	pop	es		; ES:EDI = pointer to block to zero
	xor	eax,eax
	cld
	rep stos dword ptr es:[edi] ; Zero the memory
	ret

ifndef PRODUCTION
lziie:	InternalError <LinZeroInit>
endif

EndProc LinZeroInit


;***	LinCopyBlock - copy data from one block to another
;
;	This procedure copies data from one block to another.
;	It assumes that the block do not overlap.
;
;	NOTE: this procedure may block if any of the pages
;	it accesses are not present.
;
;	ENTRY	(ESI) = linear address of source block
;		(EDI) = linear address of destination block
;		(ECX) = number of bytes to copy
;	EXIT	NONE
;	USES	ECX, EDI, ESI, ES, Flags
;
;	PROCEDURE LinCopyBlock(src, dst: BYTEPTR; len: DWORD);
;	  BEGIN
;	    WHILE len <> 0 DO
;	      BEGIN
;		len := len - 1;
;		dst^ := src^;
;		dst := dst + 1;
;		src := src + 1;
;	      END;
;	  END;

Procedure LinCopyBlock,near
	ASSUME	DS:NOTHING, ES:NOTHING, SS:NOTHING

ifndef PRODUCTION
	test	cl,3			; Integral number of DWORDs?
	jnz	short lcbie		;  no, internal error
endif
	shr	ecx,2			; Convert byte count to DWORD count
ifndef PRODUCTION
	test	ecx,3FFF0000h		; High bits set?
	jnz	short lcbie		;  yes, internal error
endif
	push	ds
	push	GSEL GDT_LinearSpace
	pop	ds
	push	ds
	pop	es
	cld
	rep movs dword ptr es:[edi],ds:[esi] ; Copy data
	pop	ds
	ret

ifndef PRODUCTION
lcbie:	InternalError <LinCopyBlock>
endif

EndProc LinCopyBlock


;***	LinAllocBlock - allocate a block of linear memory
;
;	This function allocates a block of linear memory.
;
;	ENTRY	EAX = Size of block to allocate
;		CX = Flags
;		    AD_FIXED = Segment is fixed
;
;		    NOTE: if a segment is fixed, then we
;		    lock its pages and make them contiguous.
;
;		    AD_SHARED = Segment is shared
;		    AD_LOWMEM = Segment must be in low linear memory
;		    AD_HIGHMEM = Segment must be in high memory
;		DI = Owner
;		SI = Handle
;		DS = ARENADATA
;	EXIT	Carry clear
;		    DS:BX = pointer to descriptor for new block
;		Carry set
;		    Could not satisfy request
;	USES	BX, EDX, DI, ESI, ES, Flags
;
;	FUNCTION LinAllocBlock(sizereq: DWORD;
;			       flags: WORD;
;			       owner: WORD;
;			       handle: WORD): POINTER;
;	  BEGIN
;	    flags := flags | ADAllocBits;
;	    IF (flags & AD_LOWMEM) <> 0 THEN
;	      (desc, next, size) := LinFreeSearchUp(sizereq,flags)
;	    ELSE (desc, next, size) := LinFreeSearchDown(sizereq,flags);
;	    IF desc <> 0 THEN
;	      BEGIN
;		IF (size = sizereq) OR ((flags & AD_LOWMEM) <> 0) THEN
;		  BEGIN
;		    IF size <> sizereq THEN
;		      LinBreakBlock(desc,desc^.ad_linaddr + sizereq,AD_FREE);
;		    LinRemFreeList(desc);
;		    desc^.ad_flags := flags;
;		  END
;		ELSE desc :=
;		  LinBreakBlock(desc,next^.ad_linaddr - sizereq,flags);
;		desc^.ad_handle := handle;
;		desc^.ad_owner := owner;
;		IF (flags & AD_FIXED) <> 0 THEN
;		  PageControl(PC_LOCK,desc^.ad_linaddr,sizereq);
;	      END;
;	    RETURN desc;
;	  END;

Procedure LinAllocBlock,near
	ASSUME	DS:ARENADATA, ES:NOTHING, SS:NOTHING

	localvar lab_handle,WORD	; handle = SI
	localvar lab_owner,WORD		; owner = DI

	EnterProc <si,di>
	or	cx,ADAllocBits		; Set forced allocation bits
	mov	bx,offset DosHighCode:LinFreeSearchUp ; Assume low mem wanted
	test	cx,AD_LOWMEM		; Low memory wanted?
	jnz	short lab0		;  yes, skip ahead
	mov	bx,offset DosHighCode:LinFreeSearchDown ; Look for high memory
lab0:	call	bx			; Find free block
	jc	short lab5		;  failure if carry set

;	Free block found:
;	DS:BX = pointer to free block's descriptor
;	DS:SI = pointer to descriptor of block following free block
;	CX = flags
;	EDX = size of free block

	cmp	eax,edx			; Sizes match?
	je	short lab1		;  yes, skip ahead
	test	cx,AD_LOWMEM		; Low memory wanted?
	jz	short lab2		;  no, skip ahead

;	We are taking the low portion of a free block, so
;	we want to break the block from the bottom, and
;	the new portion will be free.

	push	eax
	push	bx
	push	cx
	add	eax,[bx].ad_linaddr	; EAX = linear address of new block
	mov	cx,AD_FREE		; Make new block free
	call	LinBreakBlock		; Make new block
	pop	cx			; CX = allocation flags
	pop	bx
	pop	eax			; EAX = block size
lab1:	push	bx
	call	LinRemFreeList		; Remove block from free list
	pop	bx
	mov	[bx].ad_flags,cx	; Set flags
	jmp	short lab3

;	We are taking the high portion of a free block, so
;	we want to break the block from the top, and the new
;	portion will be busy.

lab2:	push	eax
	sub	eax,[si].ad_linaddr	; EAX = linear address of new block
	neg	eax
	call	LinBreakBlock		; BX = descriptor of new block
	pop	eax			; EAX = size of new block

;	Finish initialization of newly allocated block.

lab3:	mov	dx,lab_handle		; Set handle
	mov	[bx].ad_handle,dx
	mov	dx,lab_owner		; Set owner
	mov	[bx].ad_owner,dx

;	If we succeeded in allocating fixed memory, then
;	we must force it to be contiguous in physical memory
;	because someone may do DMA to it without locking it
;	first.

	test	cx,AD_FIXED		; Fixed memory?
	jz	short lab4		;  no, skip ahead
	push	eax
	push	bx
	push	cx
	push	ds
	push	eax			; Push size
	push	[bx].ad_linaddr		; Push LINEAR address of block
	push	PC_LOCK			; Make locked and contiguous
	callfar	PageControl		; PageControl(PC_LOCK,linaddr,size)
	add	sp,10			; Clean off parameters
	pop	ds
	pop	cx
	pop	bx
	pop	eax

;	Exit successfully

lab4:	clc				; Success
lab5:	LeaveProc
	ret

EndProc LinAllocBlock


;***	PhysAlloc - allocate linear memory
;
;	This function allocates a block of linear memory
;	having the size and characteristics described by
;	the input parameters which are dicussed below.
;	Note that even though this function may block
;	during zero-initialization (because of page faults),
;	we do not need to set the AD_BUSY bit since the
;	newly allocated block is not externally visible
;	until we return through the Virtual Memory Manager.
;
;	ENTRY	AX:BX = Size of block to allocate
;		CX = Flags
;		    AD_FIXED = Segment is fixed
;		    AD_SWAPPABLE = Segment is swappable
;		    AD_DISCARDABLE = Segment is discardable
;
;		    NOTE: if a segment is fixed or if it is not
;		    swappable and not discardable, then we mark
;		    it fixed, lock its pages, and make them contiguous.
;		    The AD_RESIDENT bit is set for segments that
;		    are neither swappable nor discardable, but
;		    are not specifically fixed.
;
;		    AD_SHARED = Segment is shared
;		    AD_LOWMEM = Segment must be in low linear memory
;		    AD_ZEROINIT = Segment is zero-initialized
;		    AD_HIGHMEM = Segment must be in high memory
;		DI = Owner
;		SI = Handle
;	EXIT	Carry clear
;		    AX:BX = linear address of new segment
;		Carry set
;		    Could not satisfy request
;	USES	EAX, BX, ECX, EDX, EDI, ESI, DS, ES, Flags
;
;	FUNCTION PhysAlloc(sizereq: DWORD;
;			   flags: WORD;
;			   owner: WORD;
;			   handle: WORD): DWORD;
;	  BEGIN
;	    sizereq := LinAdjustSize(sizereq);
;	    IF sizereq = 0 THEN
;	      InternalError('Bad size');
;	    IF ((flags & (AD_LOWMEM + AD_HIGHMEM)) <> 0) AND
;	      ((flags & AD_FIXED) = 0) THEN
;	      InternalError('Not allowed for the sake of compatibility');
;	    IF ((flags & AD_LOWMEM) <> 0) AND (ADAllocBits <> 0) THEN
;	      InternalError('Illegal request for low memory');
;	    IF (flags & (AD_SWAPPABLE + AD_DISCARDABLE + AD_FIXED)) = 0 THEN
;	      flags := flags | (AD_FIXED + AD_RESIDENT);
;	    desc := LinAllocBlock(sizereq,flags,owner,handle);
;	    IF desc <> 0 THEN
;	      BEGIN
;		IF (flags & AD_ZEROINIT) <> 0 THEN
;		  LinZeroInit(desc^.ad_linaddr,sizereq);
;		retval := desc^.ad_linaddr;
;	      END
;	    ELSE retval := 0;
;	    RETURN retval;
;	  END;

Procedure PhysAlloc,hybrid
	ASSUME	DS:NOTHING, ES:NOTHING, SS:NOTHING

	ArenaContext ds			; DS = ARENADATA
	call	LinAdjustSize		; EAX = adjusted request size
ifndef PRODUCTION
	or	eax,eax			; Zero bytes requested?
	jz	short laie		;  yes, internal error
	test	cx,AD_LOWMEM+AD_HIGHMEM	; Low or high memory wanted?
	jz	short la0		;  no, skip ahead
	test	cx,AD_FIXED		; Fixed memory wanted?
	jz	short laie		;  no, internal error
	test	cx,AD_LOWMEM		; Low memory wanted?
	jz	short la0		;  no, skip ahead
	cmp	ADAllocBits,0		; Too late for low memory?
	jnz	short laie		;  yes, internal error
la0:
endif

;	Check if the block is to be swappable or discardable
;	or fixed.  If it is to be none of these, then it will be movable,
;	but it must always be resident.  Most of the time, we
;	will simply treat it as fixed, but we set the AD_RESIDENT
;	bit so that we can distinguish the block from an ordinary
;	fixed block.

	test	cx,AD_SWAPPABLE+AD_DISCARDABLE+AD_FIXED
	jnz	short la1
	or	cx,AD_FIXED+AD_RESIDENT	; Special fixed memory
la1:	call	LinAllocBlock		; DS:BX = block descriptor
	jc	short la3		;  return if allocation failed

;	Zero-initialize the memory if requested.

	test	cx,AD_ZEROINIT		; Zero-initialize?
	jz	short la2		;  no, skip ahead
	mov	edi,[bx].ad_linaddr	; EDI = linear address
	mov	ecx,eax			; ECX = size in bytes
	call	LinZeroInit		; Zero-initialize

;	Exit successfully

la2:	mov	ax,word ptr [bx].ad_linaddr+2 ; AX:BX = linear address
	mov	bx,word ptr [bx].ad_linaddr
	clc				; Success
la3:	ret

ifndef PRODUCTION
laie:	InternalError <LinAlloc>
endif

EndProc PhysAlloc


;***	LinFreeBlock - free a block of linear memory
;
;	This procedure causes the specified block of linear
;	memory to be freed.  If there are adjacent free blocks,
;	then it coalesces the new free block with them.
;	Otherwise, it calls LinInsertFree to place the new free
;	block on the list of free blocks.
;
;	ENTRY	DS:BX = pointer to descriptor of block to free
;	EXIT	NONE
;	USES	EAX, EBX, CX, DX, DI, SI, ES, Flags
;
;	PROCEDURE LinFreeBlock(desc: POINTER);
;	  BEGIN
;	    IF (desc^.ad_flags & (AD_FIXED + AD_LOCKED)) <> 0 THEN
;	      PageControl(PC_UNLOCK,desc^.ad_linaddr,
;		desc^.ad_next^.ad_linaddr - desc^.ad_linaddr);
;	    PageControl(PC_FREE,desc^.ad_linaddr,
;	      desc^.ad_next^.ad_linaddr - desc^.ad_linaddr);
;	    tmp := desc^.ad_next;
;	    IF (tmp^.ad_flags & AD_FREE) <> 0 THEN
;	      BEGIN
;		desc^.ad_flags := AD_FREE;
;		desc^.ad_nextfree := tmp^.ad_nextfree;
;		IF desc^.ad_nextfree = 0 THEN
;		  ADLastFree := desc
;		ELSE desc^.ad_nextfree^.ad_prevfree := desc;
;		desc^.ad_prevfree := tmp^.ad_prevfree;
;		IF desc^.ad_prevfree = 0 THEN
;		  ADFirstFree := desc
;		ELSE desc^.ad_prevfree^.ad_nextfree := desc;
;		desc^.ad_next := tmp^.ad_next;
;		desc^.ad_next^.ad_prev := desc;
;		LinFreeDescriptor(tmp);
;	      END;
;	    tmp := desc^.ad_prev;
;	    IF (tmp <> 0) AND ((tmp^.ad_flags & AD_FREE) <> 0) THEN
;	      BEGIN
;		IF (desc^.ad_flags & AD_FREE) <> 0 THEN
;		  BEGIN
;		    tmp^.ad_nextfree := desc^.ad_nextfree;
;		    IF tmp^.ad_nextfree = 0 THEN
;		      ADLastFree := tmp
;		    ELSE tmp^.ad_nextfree^.ad_prevfree := tmp;
;		  END;
;		tmp^.ad_next := desc^.ad_next;
;		tmp^.ad_next^.ad_prev := tmp;
;		LinFreeDescriptor(desc);
;		desc := tmp;
;	      END;
;	    IF (desc^.ad_flags & AD_FREE) = 0 THEN
;	      BEGIN
;		desc^.ad_flags := AD_FREE;
;		LinInsertFree(desc);
;	      END;
;	  END;

Procedure LinFreeBlock,near
	ASSUME	DS:ARENADATA, ES:NOTHING, SS:NOTHING

	mov	si,[bx].ad_next		; SI = descriptor of next block
ifndef PRODUCTION
	or	si,si			; Null?
	jz	lfbie			;  yes, internal error
endif
	mov	eax,[si].ad_linaddr	; EAX = size of block
	sub	eax,[bx].ad_linaddr
ifndef PRODUCTION
	jbe	lfbie			;  internal error if zero or underflow
endif
	test	[bx].ad_flags,AD_FIXED+AD_LOCKED ; Any pages to unlock?
	jz	short lfb0		;  no, skip ahead

;	Unlock pages.

	push	eax
	push	bx
	push	ds
	push	eax			; Push size
	push	[bx].ad_linaddr		; Push linear address
	push	PC_UNLOCK		; Push function number
	callfar	PageControl		; Unlock the pages
	add	sp,10			; Clean up stack
	pop	ds
	pop	bx
	pop	eax

;	Deallocate pages.

lfb0:	push	bx
	push	ds
	push	eax			; Push size
	push	[bx].ad_linaddr		; Push linear address
	push	PC_FREE			; Push function code
	callfar	PageControl		; Deallocate pages
	add	sp,10			; Clean up stack
	pop	ds
	pop	bx

;	Free the block.

	test	[si].ad_flags,AD_FREE	; Is next block free?
	jz	short lfb5		;  no, cannot coalesce

;	Coalesce new free block with the free block
;	that immediately follows it.

	mov	[bx].ad_flags,AD_FREE	; Mark block free
	mov	di,[si].ad_nextfree	; DI = descriptor of next free block
	mov	[bx].ad_nextfree,di	; Set links to and from next free block
	or	di,di			; End of list?
	jnz	short lfb1		;  no, skip ahead
	mov	ADLastFree,bx		; New tail of list
	jmp	short lfb2
lfb1:	mov	[di].ad_prevfree,bx
lfb2:	mov	di,[si].ad_prevfree	; DI = previous free block descriptor
	mov	[bx].ad_prevfree,di	; Set links to and from prev free block
	or	di,di			; Is there a previous free block?
	jnz	short lfb3		;  yes, skip ahead
	mov	ADFirstFree,bx		; New head of list
	jmp	short lfb4
lfb3:	mov	[di].ad_nextfree,bx
lfb4:	mov	di,[si].ad_next		; DI = descriptor of next block
ifndef PRODUCTION
	or	di,di			; Null?
	jz	short lfbie		;  yes, internal error
endif
	mov	[bx].ad_next,di		; Set links to and from next block
	mov	[di].ad_prev,bx
	push	bx
	mov	bx,si
	call	LinFreeDescriptor	; Free descriptor of absorbed block
	pop	bx

;	Attempt to coalesce free block with previous block.

lfb5:	mov	si,[bx].ad_prev		; SI = descriptor of previous block
	or	si,si			; End of list?
	jz	short lfb8		;  yes, cannot coalesce
	test	[si].ad_flags,AD_FREE	; Is block free?
	jz	short lfb8		;  no, cannot coalesce
	test	[bx].ad_flags,AD_FREE	; Is new free block on free list?
	jz	short lfb7		;  no, skip ahead
	mov	di,[bx].ad_nextfree	; DI = descriptor of next free block
	mov	[si].ad_nextfree,di	; Set links to and from next free block
	or	di,di			; End of list?
	jnz	short lfb6		;  no, skip ahead
	mov	ADLastFree,si		; New tail of list
	jmp	short lfb7
lfb6:	mov	[di].ad_prevfree,si	; Set previous link
lfb7:	mov	di,[bx].ad_next		; DI = descriptor of next block
ifndef PRODUCTION
	or	di,di			; Null?
	jz	short lfbie		;  yes, internal error
endif
	mov	[si].ad_next,di		; Set links to and from next block
	mov	[di].ad_prev,si
	call	LinFreeDescriptor	; Free descriptor of absorbed block
	jmp	short lfb9		; Exit

;	Insert new free block on free list if it was not
;	coalesced with an adjacent free block.

lfb8:	test	[bx].ad_flags,AD_FREE	; Was block coalesced?
	jnz	short lfb9		;  yes, exit
	mov	[bx].ad_flags,AD_FREE	; Set free bit
	call	LinInsertFree		; Insert descriptor in free block list
lfb9:	ret

ifndef PRODUCTION
lfbie:	InternalError <LinFreeBlock>
endif

EndProc LinFreeBlock


;***	PhysFree - free a block of linear memory
;
;	This procedure frees the specified block of linear memory.
;	If the AD_BUSY flag is set for the block, this function
;	merely sets the AD_FREEPENDING bit in the flags and returns.
;	It is the responsibility of the procedure that sets the
;	AD_BUSY bit to check for the free-pending bit when it
;	clears the busy bit.  If there is a free pending, then
;	it is actually done when the busy bit is cleared.  We do
;	not block the thread trying to do the free, because
;	potentially we could block more than one thread on the
;	same block in this fashion, and after one of the blocked
;	threads gets control and frees the block, the others would
;	cause unpredictable results (probably internal errors!).
;
;	ENTRY	AX:BX = linear address of block to free
;	EXIT	NONE
;	USES	EAX, EBX, CX, DX, DI, SI, DS, ES, Flags
;
;	PROCEDURE PhysFree(linaddr: DWORD);
;	  BEGIN
;	    NPX287Wait;
;	    desc := LinMapDescriptor(linaddr);
;	    IF (desc^.ad_flags & AD_LOCKED) <> 0 THEN
;	      InternalError('Freeing locked segment');
;	    IF (desc^.ad_linaddr < 640*ONEK) AND (ADAllocBits <> 0) THEN
;	      InternalError('Illegal free of low memory');
;	    IF (desc^.ad_flags & (AD_BUSY + AD_LOCKED)) <> 0 THEN
;	      desc^.ad_flags := desc^.ad_flags | AD_FREEPENDING
;	    ELSE LinFreeBlock(desc);
;	  END;

Procedure PhysFree,hybrid
	ASSUME	DS:NOTHING, ES:NOTHING, SS:NOTHING

	callfar	NPX287Wait
	shl	eax,16			; EAX = linear address
	mov	ax,bx
	ArenaContext ds			; DS = ARENADATA
	call	LinMapDescriptor	; DS:BX = descriptor of block to free
ifndef PRODUCTION
	test	[bx].ad_flags,AD_LOCKED	; Is block locked?
	jnz	short lfie		;  yes, internal error
	cmp	[bx].ad_linaddr,640*ONEK; Is block in low memory?
	jae	short lf0		;  no, skip ahead
	cmp	ADAllocBits,0		; Too late to free this guy?
	jnz	short lfie		;  yes, internal error
endif
lf0:	test	[bx].ad_flags,AD_BUSY	; Can we free it now?
	jz	short lf1		;  yes, skip ahead
	or	[bx].ad_flags,AD_FREEPENDING ; Free is pending
	jmp	short lf2		; (OR clears carry)
lf1:	call	LinFreeBlock		; Free the block
	clc				; Just to be safe
lf2:	ret

ifndef PRODUCTION
lfie:	InternalError <LinFree>
endif

EndProc PhysFree


;***	LinGrowUp - grow block into next adjacent free block
;
;	This function grows the specified block into the block
;	immediately following it which is assumed to be free
;	and sufficiently large.
;
;	ENTRY	(DS:BX) = pointer to descriptor of block to grow
;		(DS:SI) = pointer to next block
;		(EAX) = size increment
;		(ECX) = size of next block
;		(EDX) = original size of block to grow
;	EXIT	(DS:BX) = pointer to descriptor of grown block
;	USES	EBX high, DI, SI, Flags
;
;	FUNCTION LinGrowUp(desc, next: POINTER;
;			   increment, nextsize, oldsize: DWORD): POINTER;
;	  BEGIN
;	    IF increment = nextsize THEN
;	      BEGIN
;		LinRemFreeList(next);
;		desc^.ad_next := next^.ad_next;
;		desc^.ad_next^.ad_prev := desc;
;		LinFreeDescriptor(next);
;	      END
;	    ELSE
;	      BEGIN
;		ADMap[next^.ad_linaddr >> PAGESHIFT] := 0;
;		next^.ad_linaddr := desc^.ad_linaddr + newsize;
;		ADMap[next^.ad_linaddr >> PAGESHIFT] := next;
;	      END;
;	  END;

Procedure LinGrowUp,near
	ASSUME	DS:ARENADATA, ES:NOTHING, SS:NOTHING

	cmp	ecx,eax			; Free block equals increment?
	ja	short lgu0		;  no, larger, skip ahead
ifndef PRODUCTION
	jb	short lguie		;  internal error if smaller
endif

;	Free block is just the right size
;
;	(EAX) = size needed
;	(ECX) = size of free block (equals (EAX))
;	(EDX) = original size of block being grown
;	(BX) = descriptor of block to grow
;	(SI) = descriptor of adjacent free block

	push	bx
	push	si
	mov	bx,si			; (BX) = descriptor of free block
	call	LinRemFreeList		; Take block off free list
	pop	si
	pop	bx			; (BX) = descriptor of block to grow
	mov	di,[si].ad_next		; Remove free block from block list
	mov	[bx].ad_next,di
	mov	[di].ad_prev,bx
	push	eax
	push	bx
	mov	bx,si			; (BX) = removed descriptor
	call	LinFreeDescriptor	; Free the descriptor
	pop	bx			; (BX) = descriptor of grown block
	pop	eax
	jmp	short lgu1		; Return

;	Free block has more than enough space
;
;	(EAX) = size needed
;	(ECX) = size of free block
;	(EDX) = original size of block being grown
;	(BX) = descriptor of block to grow
;	(SI) = descriptor of adjacent free block

lgu0:	push	eax
	push	ebx
	add	eax,[si].ad_linaddr	; (EAX) = new address of free block
ifndef PRODUCTION
	test	ax,PAGESIZE - 1		; Page-aligned?
	jnz	short lguie		;  no, internal error
endif
	shld	ebx,eax,33 - PAGESHIFT	; (BX) = new linear page number * 2
ifndef PRODUCTION
	cmp	ADMap[bx],0		; Mapping exists?
	jnz	short lguie		;  yes, internal error
endif
	mov	ADMap[bx],si		; Set new mapping
	xchg	eax,[si].ad_linaddr	; Set address, (EAX) = old address
ifndef PRODUCTION
	test	ax,PAGESIZE - 1		; Page-aligned?
	jnz	short lguie		;  no, internal error
endif
	shld	ebx,eax,33 - PAGESHIFT	; (BX) = old linear page number * 2
ifndef PRODUCTION
	cmp	ADMap[bx],0		; Mapping exists?
	jz	short lguie		;  no, internal error
endif
	mov	ADMap[bx],0		; Break old mapping
	pop	ebx
	pop	eax
lgu1:	ret

ifndef PRODUCTION
lguie:	InternalError <LinGrowUp>
endif

EndProc LinGrowUp


;***	LinGrowDown - grow block into preceding adjacent free block
;
;	This function is called to grow a block by moving it down
;	into all or part of the preceding block.  It is assumed
;	that the preceding block is free and of sufficient size.
;
;	ENTRY	(DS:BX) = pointer to descriptor of block to grow
;		(DS:SI) = pointer to descriptor of preceding block
;		(EAX) = size increment
;		(EDX) = size of block to grow
;	EXIT	(DS:BX) = pointer to descriptor of grown (and moved) block
;	USES	EBX high, ECX, EDI, ESI, ES, Flags
;
;	FUNCTION LinGrowDown(desc, prev: POINTER;
;			     increment, oldsize: DWORD): POINTER;
;	  BEGIN
;	    ADMap[desc^.ad_linaddr >> PAGESHIFT] := 0;
;	    desc^.ad_linaddr := desc^.ad_linaddr - increment;
;	    MarkSegMoved(desc^.ad_linaddr,desc^.ad_handle,desc^.ad_owner);
;	    PageMovePTE(desc^.ad_linaddr + increment,desc^.ad_linaddr,oldsize);
;	    IF increment = prevsize THEN
;	      BEGIN
;		LinRemFreeList(prev);
;		desc^.ad_prev := prev^.ad_prev;
;		IF desc^.ad_prev = NULL THEN
;		  ADFirst := desc
;		ELSE
;		  desc^.ad_prev^.ad_next := desc;
;		LinFreeDescriptor(prev);
;	      END;
;	    ADMap[desc^.ad_linaddr >> PAGESHIFT] := desc;
;	    RETURN desc;
;	  END;

Procedure LinGrowDown,near
	ASSUME	DS:ARENADATA, ES:NOTHING, SS:NOTHING

	mov	ecx,[bx].ad_linaddr	; (ECX) = old linear address
ifndef PRODUCTION
	test	cx,PAGESIZE - 1		; Page-aligned?
	jnz	lgdie			;  no, internal error
endif
	shld	edi,ecx,33 - PAGESHIFT	; DI = linear page number * 2
ifndef PRODUCTION
	cmp	ADMap[di],0		; Mapping exists?
	jz	lgdie			;  no, internal error
endif
	mov	ADMap[di],0		; Break existing mapping
	sub	ecx,eax			; (ECX) = new linear address
ifndef PRODUCTION
	jb	short lgdie		;  internal error if underflow
endif
	mov	[bx].ad_linaddr,ecx	; Set new linear address
	push	bx
	push	si
	push	ds
	push	eax
	push	ecx
	push	edx
	mov	dx,cx			; (CX:DX) = new linear address
	shr	ecx,16
	mov	si,[bx].ad_handle	; (SI) = handle
	mov	di,[bx].ad_owner	; (DI) = owner
	invoke	MarkSegMoved		; Cause selectors to be updated
    ASSUME	DS:NOTHING
ifndef PRODUCTION
	jc	short lgdie		;  internal error if failure
endif
	pop	edx
	pop	ecx
	pop	eax
	push	eax
	push	ecx
	push	edx
	push	edx			; Push size
	push	ecx			; Push destination address
	add	ecx,eax			; (ECX) = source address
	push	ecx			; Push source address
	callfar	PageMovePTE		; Move the page table entries
	add	sp,12			; Clean the stack
	pop	edx
	pop	ecx			; (ECX) = new linear address
	pop	eax
	pop	ds
    ASSUME	DS:ARENADATA
	pop	si
	pop	bx
	cmp	ecx,[si].ad_linaddr	; Whole free block consumed?
	ja	short lgd2		;  no, skip ahead
ifndef PRODUCTION
	jb	short lgdie		;  internal error if underflow
endif
	mov	di,[si].ad_prev		; (DI) = previous block's previous
	mov	[bx].ad_prev,di		; Remove previous block from list
	or	di,di			; Null link?
	jnz	short lgd0		;  no, skip ahead
	mov	ADFirst,bx		; New head of list
	jmp	short lgd1

lgd0:	mov	[di].ad_next,bx		; Set new link
lgd1:	push	eax
	push	bx
	mov	bx,si			; (BX) = previous block's descriptor
	push	bx
	call	LinRemFreeList		; Remove it from free list
	pop	bx
	call	LinFreeDescriptor	; And put it back on free list
	pop	bx			; (BX) = grown block's descriptor
	pop	eax
lgd2:	shld	esi,ecx,33 - PAGESHIFT	; (SI) = linear page number * 2
ifndef PRODUCTION
	cmp	ADMap[si],0		; Mapping exists?
	jnz	short lgdie		;  yes, internal error
endif
	mov	ADMap[si],bx		; Set new mapping
	ret

ifndef PRODUCTION
lgdie:	InternalError <LinGrowDown>
endif

EndProc LinGrowDown


;***	LinMoveBlock - grow block by moving it in linear memory
;
;	This function is called to grow a block by moving it to a
;	new location in linear memory.
;
;	ENTRY	(DS:BX) = pointer to descriptor of block to grow
;		(EAX) = size increment
;		(EDX) = old size of block
;	EXIT	Carry clear
;		    (DS:BX) = pointer to descriptor of grown (and moved) block
;		Carry set
;		    Could not satisfy request
;	USES	BX, CX, DI, SI, ES, Flags
;
;	FUNCTION LinMoveBlock(desc: POINTER; oldsize, newsize: DWORD): POINTER;
;	  BEGIN
;	    tmp := LinAllocBlock(newsize,desc^.ad_flags,
;	      desc^.ad_owner,desc^.ad_handle);
;	    IF tmp <> NULL THEN
;	      BEGIN
;		PageMovePTE(desc^.ad_linaddr,tmp^.ad_linaddr,oldsize);
;		LinFreeBlock(desc);
;		MarkSegMoved(tmp^.ad_linaddr,tmp^.ad_handle,tmp^.ad_owner);
;	      END;
;	    RETURN tmp;
;	  END;

Procedure LinMoveBlock,near
	ASSUME	DS:ARENADATA, ES:NOTHING, SS:NOTHING

	push	eax
	add	eax,edx			; (EAX) = new size
	mov	cx,[bx].ad_flags	; (CX) = block flags
	mov	di,[bx].ad_owner	; (DI) = owner
	mov	si,[bx].ad_handle	; (SI) = handle
	push	bx
	push	edx
	call	LinAllocBlock		; (BX) = new block descriptor
	pop	edx
	pop	si			; (SI) = original block descriptor
	jc	short lmb0		; Return failure if allocation fails

;	Allocation succeeded
;
;	(EDX) = old size
;	(BX) = descriptor of new block
;	(SI) = descriptor of old block

	push	edx
	push	bx
	push	si
	push	ds
	push	edx			; Push old size
	push	[bx].ad_linaddr		; Push new linear address
	push	[si].ad_linaddr		; Push old linear address
	callfar	PageMovePTE		; Move pte's
	add	sp,12			; Clean up stack
	pop	ds
	pop	bx			; (BX) = original descriptor
	call	LinFreeBlock		; Free original block
	pop	bx			; (BX) = new descriptor
	push	bx
	push	ds
	mov	dx,word ptr [bx].ad_linaddr ; (CX:DX) = new linear address
	mov	cx,word ptr [bx].ad_linaddr+2
	mov	si,[bx].ad_handle	; (SI) = handle
	mov	di,[bx].ad_owner	; (DI) = owner
	invoke	MarkSegMoved		; Cause selectors to be updated
ifndef PRODUCTION
	jc	short lmbie		;  internal error if failure
endif
	pop	ds
	pop	bx
	pop	edx
	clc
lmb0:	pop	eax			; (EAX) = size increment
	ret

ifndef PRODUCTION
lmbie:	InternalError <LinMoveBlock>
endif

EndProc LinMoveBlock


;***	LinGrowResident - grow a resident block
;
;	This function attempts to grow a resident block by
;	allocating a new block of the requested size,
;	copying the data from the old block to the new,
;	and freeing the old block.  Since this function
;	MAY BLOCK while allocating a new block of fixed,
;	contiguous physical memory, it must use the
;	in-transition flag, and check for a pending free
;	after it may have blocked.
;
;	ENTRY	(DS:BX) = pointer to block descriptor
;		(EAX) = new size
;		(EDX) = current size
;	EXIT	Carry clear
;		    (DS:BX) = pointer to new block descriptor
;		Carry set
;		    Unable to satisfy request.
;	USES	EBX, ECX, EDI, ESI, ES, Flags
;
;	FUNCTION LinGrowResident(desc: POINTER;
;				 oldsize: DWORD;
;				 newsize: DWORD): POINTER;
;	  BEGIN
;	    desc^.ad_flags := desc^.ad_flags | AD_BUSY;
;	    tmp := LinAllocBlock(newsize,desc^.ad_flags & ~AD_BUSY,
;	      desc^.ad_owner,desc^.ad_handle);
;	    IF tmp <> NULL THEN
;	      BEGIN
;		IF (desc^.ad_flags & AD_FREEPENDING) <> 0 THEN
;		  BEGIN
;		    LinFreeBlock(desc);
;		    LinFreeBlock(tmp);
;		    desc := NULL;
;		  END
;		ELSE
;		  BEGIN
;		    LinCopyBlock(desc^.ad_linaddr,tmp^.ad_linaddr,oldsize);
;		    LinFreeBlock(desc);
;		    desc := tmp;
;		    MarkSegMoved(desc^.ad_linaddr,desc^.ad_handle,
;		      desc^.ad_owner);
;		  END;
;	      END
;	    ELSE 
;	      BEGIN
;		IF (desc^.ad_flags & AD_FREEPENDING) <> 0 THEN
;		  LinFreeBlock(desc);
;		ELSE desc^.ad_flags := desc^.ad_flags & ~AD_BUSY;
;		desc := NULL;
;	      END;
;	    RETURN desc;
;	  END;

Procedure LinGrowResident,near
	ASSUME	DS:ARENADATA, ES:NOTHING, SS:NOTHING

	mov	cx,[bx].ad_flags	; (CX) = flags
	mov	di,[bx].ad_owner	; (DI) = owner
	mov	si,[bx].ad_handle	; (SI) = handle
	or	[bx].ad_flags,AD_BUSY	; We could block
	push	bx
	push	edx
	call	LinAllocBlock		; (BX) = new block descriptor
	pop	edx
	pop	si			; (SI) = original descriptor
	jc	short lgr1		;  jump if allocation failed

;	Allocation succeeded
;
;	(EAX) = new size
;	(EDX) = old size
;	(BX) = descriptor of new block
;	(SI) = descriptor of old block

	test	[si].ad_flags,AD_FREEPENDING ; Were we freed?
	jz	short lgr0		;  no, skip ahead

;	Free pending

	push	si
	call	LinFreeBlock		; Free new block
	pop	bx			; (BX) = original descriptor
	call	LinFreeBlock		; Free original block
	jmp	short lgr2		; Exit with error

;	No free pending

lgr0:	mov	edi,[bx].ad_linaddr	; (EDI) = new block linear address
	push	eax
	push	edx
	push	bx
	push	si
	mov	esi,[si].ad_linaddr	; (ESI) = old block linear address
	mov	ecx,edx			; (ECX) = old size
	call	LinCopyBlock		; Copy old to new
	pop	bx			; (BX) = old block descriptor
	call	LinFreeBlock		; Free the old block
	pop	bx			; (BX) = new block descriptor
	push	bx
	push	ds
	mov	dx,word ptr [bx].ad_linaddr ; (CX:DX) = new linear address
	mov	cx,word ptr [bx].ad_linaddr+2
	mov	si,[bx].ad_handle	; (SI) = handle
	mov	di,[bx].ad_owner	; (DI) = owner
	invoke	MarkSegMoved		; Cause selectors to be updated
ifndef PRODUCTION
	jc	short lgrie
endif
	pop	ds
	pop	bx
	pop	edx
	pop	eax
	jmp	short lgr3

;	Allocation failed
;
;	(EAX) = new size
;	(EDX) = old size
;	(SI) = descriptor of old block

lgr1:	and	[si].ad_flags,NOT AD_BUSY ; Clear the busy flag
	test	[si].ad_flags,AD_FREEPENDING ; Were we freed?
	jz	short lgr2		;  no, return failure

;	Free pending

	mov	bx,si			; (BX) = original block descriptor
	call	LinFreeBlock

;	Failure

lgr2:	stc

;	Return

lgr3:	ret

ifndef PRODUCTION
lgrie:	InternalError <LinGrowResident>
endif

EndProc LinGrowResident


;***	LinGrowBlock - make a block of linear memory larger
;
;	This function is called to increase the size of a block of
;	linear memory.  There are five cases:
;
;	1) RESIDENT blocks
;
;	Blocks that can be neither swapped nor discarded but are
;	not explicitly fixed must receive special treatment.  Unlike
;	genuine fixed blocks which cannot be grown, these blocks
;	must be growable.  For the most part, we treat these blocks
;	as fixed, since that is the only mechanism we have for
;	preventing their pages from being stolen.  Like swappable
;	blocks, these blocks do not need to be contiguous unless
;	they are locked.  However, we make them contiguous to
;	avoid complicating the process of locking such a block.
;	In order to grow one of these blocks, we allocate a new
;	RESIDENT block of the appropriate size.  We may block
;	during this allocation; thus, we must mark the original
;	block as busy first, and check for pending frees afterwards.
;	If the allocation succeeds, we have two fixed blocks, so
;	there is no worry about blocking while we copy that data
;	from the first to the second; after copying the data, we
;	free the original block.  If the allocation fails, we
;	return an error.
;
;	2) FIXED blocks
;
;	As was mentioned above, we do not allow fixed blocks to
;	grow.  It is an internal error to attempt to grow one.
;
;	3) Adjacent block not free or not large enough
;
;	If the block following the one we are trying to go is
;	either not free or not large enough, then we allocate
;	a new block of the appropriate size, map the existing
;	physical pages (if any) to the new linear addresses,
;	and free the original block.
;
;	4) Adjacent block is free and is exactly the right size
;
;	If the following block is a free block of a size exactly
;	equal to the amount of additional space we need, then
;	we remove the block from the free list, remove its
;	arena descriptor from the block list, and free the
;	descriptor.
;
;	5) Adjacent block is free and is larger than needed
;
;	If the following block is free and is larger than needed,
;	we increment its linear addressby the amount we need and
;	update the ADMap table.
;	
;	ENTRY	(DS:BX) = pointer to block descriptor
;		(EAX) = new size
;		(EDX) = current size
;	EXIT	Carry clear
;		    (DS:BX) = pointer to (possibly different) block descriptor
;		Carry set
;		    Unable to satisfy request.
;	USES	EAX, EBX, ECX, DX, EDI, ESI, ES, Flags
;
;	FUNCTION LinGrowBlock(desc: POINTER;
;			      oldsize: DWORD;
;			      newsize: DWORD): POINTER;
;	  BEGIN
;	    IF (desc^.ad_flags & AD_RESIDENT) <> 0 THEN
;	      BEGIN
;	        desc := LinGrowResident(desc,oldsize,newsize);
;		IF desc = NULL THEN
;		  RETURN NULL;
;	      END
;	    ELSE
;	      BEGIN
;		IF (desc^.ad_flags & AD_FIXED) <> 0 THEN
;		  InternalError('Cannot grow fixed block');
;		tmp := desc^.ad_next;
;		IF ((tmp^.ad_flags & AD_FREE) <> 0) AND
;		  (tmp^.ad_next^.ad_linaddr - tmp^.ad_linaddr >=
;		  newsize - oldsize) THEN
;		  desc := LinGrowUp(desc,tmp,newsize - oldsize,
;		    tmp^.ad_next^.ad_linaddr - tmp^.ad_linaddr,oldsize)
;		ELSE IF (desc^.ad_prev <> NULL) AND
;		  ((desc^.ad_prev^.ad_flags & AD_FREE) <> 0) AND
;		  (desc^.ad_linaddr - desc^.ad_prev^.ad_linaddr >=
;		  newsize - oldsize) THEN
;		  desc := LinGrowDown(desc,desc^.ad_prev,oldsize,newsize)
;		ELSE
;		  BEGIN
;		    desc := LinMoveBlock(desc,newsize - oldsize,newsize);
;		    IF desc = NULL THEN
;		      RETURN NULL;
;		  END;
;	      END;
;	    IF (desc^.ad_flags & AD_ZEROINIT) <> 0 THEN
;	      BEGIN
;		desc^.ad_flags := desc^.ad_flags | AD_BUSY;
;		LinZeroInit(desc^.ad_linaddr + oldsize,newsize - oldsize);
;		desc^.ad_flags := desc^.ad_flags & ~AD_BUSY;
;		IF (desc^.ad_flags & AD_FREEPENDING) THEN
;		  BEGIN
;		    LinFreeBlock(desc);
;		    RETURN NULL;
;		  END;
;	      END;
;	    RETURN desc;
;	  END;

Procedure LinGrowBlock,near
	ASSUME	DS:ARENADATA, ES:NOTHING, SS:NOTHING

;	Handle resident blocks

	test	[bx].ad_flags,AD_RESIDENT ; Resident memory?
	jz	short lgb0		;  no, thank God!
	call	LinGrowResident		; Grow a resident block
	jc	lgb4			;  return error if failure
	sub	eax,edx			; (EAX) = size increment
	jmp	short lgb3		; Go do zero initialization

;	Check for fixed blocks

lgb0:	test	[bx].ad_flags,AD_FIXED	; Is block fixed?
	jnz	short lgbie		;  yes, internal error

;	Try to grow into next block if free and big enough.

	sub	eax,edx			; (EAX) = amount to grow by
ifndef PRODUCTION
	jbe	short lgbie		;  internal error if zero or underflow
endif
	mov	si,[bx].ad_next		; (SI) = next block descriptor
	test	[si].ad_flags,AD_FREE	; Is next block free?
	jz	short lgb1		;  no, skip ahead
	mov	di,[si].ad_next		; (DI) = block after next descriptor
ifndef PRODUCTION
	or	di,di			; End sentinel free?
	jz	short lgbie		;  yes, internal error
endif
	mov	ecx,[di].ad_linaddr	; (ECX) = size of next block
	sub	ecx,[si].ad_linaddr
ifndef PRODUCTION
	jbe	short lgbie		;  internal error if zero or underflow
endif
	cmp	ecx,eax			; Free block large enough?
	jb	short lgb1		;  no, skip ahead
	call	LinGrowUp		; Grow into next block
	jmp	short lgb3		; Go do zero initialization

;	Try to grow into previous block if it exists and
;	if it is free and if it is large enough.

lgb1:	mov	si,[bx].ad_prev		; (SI) = previous block's descriptor
	or	si,si			; Is there a previous block?
	jz	short lgb2		;  no, skip ahead
	test	[si].ad_flags,AD_FREE	; Is the previous block free?
	jz	short lgb2		;  no, skip ahead
	mov	ecx,[bx].ad_linaddr	; (ECX) = address of block to grow
	sub	ecx,[si].ad_linaddr	; (ECX) = size of previous block
ifndef PRODUCTION
	jbe	short lgbie		;  internal error if zero or underflow
endif
	cmp	ecx,eax			; Is block large enough?
	jb	short lgb2		;  no, skip ahead
	call	LinGrowDown		; Grow into previous block
	jmp	short lgb3		; Go do zero initialization

;	Try to move the block in the linear address space.
;
;	(EAX) = size increment
;	(EDX) = old size
;	(BX) = descriptor of block to grow

lgb2:	call	LinMoveBlock		; Try to move the block
	jc	short lgb4		;  failure if carry set

;	Do zero-initialization if needed
;
;	(EAX) = size delta
;	(BX) = descriptor of grown block
;	(EDX) = old size of block

lgb3:	test	[bx].ad_flags,AD_ZEROINIT ; Zero-initialize?
	jz	short lgb4		;  no, exit (TEST clears carry)
	or	[bx].ad_flags,AD_BUSY	; Set busy bit
	mov	edi,[bx].ad_linaddr	; (EDI) = address of uninit'ed part
	add	edi,edx
	mov	ecx,eax			; (ECX) = # of bytes to zero
	call	LinZeroInit		; Do zero-initialization
	and	[bx].ad_flags,NOT AD_BUSY ; Clear busy bit
	test	[bx].ad_flags,AD_FREEPENDING ; Were we freed?
	jz	short lgb4		;  no, exit (TEST clears carry)
	call	LinFreeBlock		; After all this, free the block!
	stc				; Failure
lgb4:	ret

lgbie:	InternalError <LinGrowBlock>

EndProc LinGrowBlock


;***	LinShrinkBlock - make a block of linear memory smaller
;
;	This function is called to decrease the size of a block of
;	linear memory.  There are two cases:
;
;	1) Adjacent block is free
;
;	This case is very similar to case (5) for LinGrowBlock;
;	the free block's linear address is decremented rather
;	than incremented.
;
;	2) Adjacent block is busy
;
;	A new free block is created from the memory released
;	by shrinking the block.
;
;	If we are shrinking a fixed block, then we call
;	PageControl to unlock the pages being freed, and
;	we call PageControl to free the pages.
;
;	ENTRY	(EAX) = new size
;		(EDX) = old size
;		(DS:BX) = pointer to descriptor of block to shrink
;	EXIT	(DS:BX) = pointer to shrunken block
;	USES	EAX, CX, EDX, EDI, ESI, ES, Flags
;
;	FUNCTION LinShrinkBlock(desc: POINTER;
;				oldsize: DWORD;
;				newsize: DWORD): POINTER;
;	  BEGIN
;	    IF (desc^.ad_flags & AD_FIXED) <> 0 THEN
;	      PageControl(PC_UNLOCK,desc^.ad_linaddr + newsize,
;		oldsize - newsize);
;	    PageControl(PC_FREE,desc^.ad_linaddr + newsize,oldsize - newsize);
;	    IF ((desc^.ad_next^.ad_flags & AD_FREE) <> 0) THEN
;	      BEGIN
;		ADMap[desc^.ad_next^.ad_linaddr >> PAGESHIFT] := 0;
;		desc^.ad_next^.ad_linaddr := desc^.ad_linaddr + newsize;
;		ADMap[desc^.ad_next^.ad_linaddr >> PAGESHIFT] := desc^.ad_next;
;	      END
;	    ELSE
;	      LinBreakBlock(desc,desc^.ad_linaddr + newsize,AD_FREE);
;	    RETURN desc;
;	  END;

Procedure LinShrinkBlock,near
	ASSUME	DS:ARENADATA, ES:NOTHING, SS:NOTHING

	sub	edx,eax			; (EDX) = size delta
ifndef PRODUCTION
	jbe	lsbie			;  internal error if zero or underflow
endif
	add	eax,[bx].ad_linaddr	; (EAX) = free area linear address
	push	bx
	push	ds
	test	[bx].ad_flags,AD_FIXED	; Shrinking fixed block?
	jz	short lsb0		;  no, skip ahead

;	Shrinking fixed block.  Unlock discarded pages.
;
;	(EAX) = linear address of new free area
;	(EDX) = size of new free area

	push	eax
	push	edx
	push	edx			; Push size
	push	eax			; Push linear address
	push	PC_UNLOCK		; Push function code
	callfar	PageControl		; Unlock the pages
    ASSUME	DS:NOTHING
	add	sp,10			; Clean the stack
	pop	edx
	pop	eax

;	Release discarded pages.
;
;	(EAX) = linear address of new free area
;	(EDX) = size of new free area

lsb0:	push	eax
	push	edx
	push	edx			; Push size
	push	eax			; Push linear address
	push	PC_FREE			; Push function code
	callfar	PageControl		; Free the pages
    ASSUME	DS:NOTHING
	add	sp,10			; Clean the stack
	pop	edx
	pop	eax
	pop	ds
    ASSUME	DS:ARENADATA
	pop	bx
	mov	si,[bx].ad_next		; (SI) = descriptor of next block
	test	[si].ad_flags,AD_FREE	; Is next block free?
	jz	short lsb1		;  no, skip ahead

;	Coalesce new free area with adjacent free block.
;
;	(BX) = descriptor of shrunken block
;	(SI) = descriptor of adjacent free block
;	(EAX) = new linear address for free block

ifndef PRODUCTION
	test	ax,PAGESIZE - 1		; Is linear address page-aligned?
	jnz	short lsbie		;  no, internal error
endif
	shld	edi,eax,33 - PAGESHIFT	; (DI) = new linear page number * 2
ifndef PRODUCTION
	cmp	ADMap[di],0		; Mapping exists?
	jnz	short lsbie		;  yes, internal error
endif
	mov	ADMap[di],si		; Set new mapping
	xchg	eax,[si].ad_linaddr	; Set address, (EAX) = old address
ifndef PRODUCTION
	test	ax,PAGESIZE - 1		; Page-aligned?
	jnz	short lsbie		;  no, internal error
endif
	shld	edi,eax,33 - PAGESHIFT	; (DI) = old linear page number * 2
ifndef PRODUCTION
	cmp	ADMap[di],0		; Mapping exists?
	jz	short lsbie		;  no, internal error
endif
	mov	ADMap[di],0		; Break old mapping
	jmp	short lsb2		; Exit

;	Make new free block.
;
;	(EAX) = linear address of new free block
;	(BX) = descriptor of block from which new block is carved

lsb1:	push	bx
	mov	cx,AD_FREE		; (CX) = free block indicator
	call	LinBreakBlock		; Make new free block
	pop	bx
lsb2:	clc				; Always successful
	ret

ifndef PRODUCTION
lsbie:	InternalError <LinShrinkBlock>
endif

EndProc LinShrinkBlock


;***	PhysRealloc - change the size of a block of memory
;
;	This function changes the size of a block of memory.
;	There are three cases:
;
;	1) The block is growing
;
;	This is the hard case.  LinGrowBlock does all the work.
;
;	2) The block is shrinking
;
;	This case is straightforward.  LinShrinkBlock does all
;	the work.
;
;	3) The size of the block is not changing
;
;	This is the trivial case.  It is handled in this
;	function.
;
;	Cases (2) and (3) can never fail.  But case (1) can!
;
;	ENTRY	(AX:BX) = new size for block
;		(CX:DX) = linear address of block
;	EXIT	Carry clear
;		    (AX:BX) = address of reallocated block
;		Carry set
;		    Cannot satisfy request
;	USES	EAX, EBX, CX, EDX, EDI, ESI, DS, ES, Flags
;
;	FUNCTION PhysRealloc(linaddr: DWORD; newsize: DWORD): DWORD;
;	  BEGIN
;	    newsize := LinAdjustSize(newsize);
;	    IF newsize = 0 THEN
;	      InternalError('Bad size');
;	    desc := LinMapDescriptor(linaddr);
;	    IF (desc^.ad_linaddr < 640*ONEK) AND (ADAllocBits <> 0) THEN
;	      InternalError('Illegal realloc of low memory');
;	    oldsize := desc^.ad_next^.ad_linaddr - linaddr;
;	    IF newsize <> oldsize THEN
;	      BEGIN
;		IF (desc^.ad_flags & (AD_BUSY + AD_LOCKED)) <> 0 THEN
;		  RETURN 0;
;		IF newsize > oldsize THEN
;		  desc := LinGrowBlock(desc,oldsize,newsize)
;		ELSE
;		  desc := LinShrinkBlock(desc,oldsize,newsize);
;		RETURN desc^.ad_linaddr;
;	      END
;	    ELSE RETURN linaddr;
;	  END;

Procedure PhysRealloc,hybrid
	ASSUME	DS:NOTHING, ES:NOTHING, SS:NOTHING

	call	LinAdjustSize		; (EAX) = new size
ifndef PRODUCTION
	or	eax,eax			; Size is zero?
	jz	short lrie		;  yes, internal error
endif
	ArenaContext ds
	push	eax
	mov	ax,cx			; (EAX) = linear address
	shl	eax,16
	mov	ax,dx
	call	LinMapDescriptor	; (BX) = descriptor for block
	pop	eax			; (EAX) = new size for block
ifndef PRODUCTION
	cmp	[bx].ad_linaddr,640*ONEK; Block in low memory?
	jae	short lr0		;  no, skip ahead
	cmp	ADAllocBits,0		; Too late to realloc low memory?
	jnz	short lrie		;  yes, internal error
endif
lr0:	test	[bx].ad_flags,AD_BUSY+AD_LOCKED ; Can we reallocate now?
	stc				;  (assume failure)
	jnz	short lr3		;  no, error
	mov	si,[bx].ad_next		; (SI) = descriptor for next block
	mov	edx,[si].ad_linaddr	; (EDX) = current size of block
	sub	edx,[bx].ad_linaddr
ifndef PRODUCTION
	jbe	short lrie		;  internal error if zero or underflow
endif
	cmp	eax,edx			; Compare sizes
	je	short lr2		;  do nothing if same
	mov	cx,offset DosHighCode:LinShrinkBlock ; Assume shrink request
	jb	lr1			;  jump if shrink
	mov	cx,offset DosHighCode:LinGrowBlock ; Actually a grow request
lr1:	call	cx			; (BX) = descriptor of changed block
	jc	lr3
lr2:	mov	ax,word ptr [bx].ad_linaddr+2 ; (AX:BX) = linear address
	mov	bx,word ptr [bx].ad_linaddr
lr3:	ret

ifndef PRODUCTION
lrie:	InternalError <LinRealloc>
endif

EndProc PhysRealloc


HIGHCODE ends


	END