PAGE	,132
TITLE	LOADSTE - segment loading routines
NAME	LOADSTE

;***	MT-MSDOS Segment loading routines
;
;	Copyright (c) 1983, 1984 by Microsoft
;
;	SCCSID = @(#)loadste.asm	10.3 87/02/26
;
;	The segment loading routines handle the loading of
;	new .EXE file format segments.
;
;	The module table entry is allocated in the file LOAD.ASM.


.XLIST
	INCLUDE dosseg.inc
	INCLUDE dosmac.inc
	INCLUDE error.inc
	INCLUDE pmode.inc
	INCLUDE mhandle.inc
	INCLUDE filemode.inc
	INCLUDE dossymb.inc
	INCLUDE tcbdata.inc
.LIST
	INCLUDE segmngr.inc
	INCLUDE loadvars.inc


	ModName loadste

CONSTANTS	SEGMENT
	EXTRN	doscall1:byte
NonResNamTblHdl dw	(0)
	PUBLIC	DosModHdl
DosModHdl	dw	0
	PUBLIC	IoplEnabled
IoplEnabled	db	1

IOPLTable	db	8,"DOSCALL1"
		db	8,"BVSCALLS"
		db	0		; null terminates list of names
CONSTANTS	ENDS


        ReferTask   	CurrTCB,WORD
	ReferTask	ptda_handle,WORD
	ReferTask	ptda_child,WORD
	ReferTask	TempBuf,BYTE

	EXTRN	AllocCallGate:near
	EXTRN	AsciiToInt:near
	EXTRN	ClearLdrSem:near
	EXTRN	EachSegEntry:near
	EXTRN	EditProlog:near
	EXTRN	ex_HugeAlloc:near
	EXTRN	ex_LSeek:near
	EXTRN	ex_LSeekN:near
	EXTRN	ex_Read:near
	EXTRN	ex_ReadN:near
	EXTRN	ex_SegAlloc:near
	EXTRN	ex_SegAttach:near
	EXTRN	GetCallGate:near
	EXTRN	GetLdrSem:near
	EXTRN	get_mte:near
	EXTRN	load_error:near
	EXTRN	SetupGlblErrTxt:near
	EXTRN	InvGlblErrTxt:near

	EXTRN	MemAlloc:near
	EXTRN	MemFree:near
	EXTRN	MemMapAlias:near
	EXTRN	MemUnmapAlias:near
	EXTRN	num_to_ste:near
	EXTRN	ResetUsedBit:near
	EXTRN	scan_ptda_mods:near
	EXTRN	MteMap:near
	EXTRN	TmpMap:near
	EXTRN	TmpUnMap:near
	EXTRN	SegClear:near
	EXTRN	GetDescInfo:near

	EXTRNFAR	Clear_Kernel_Sem
	EXTRNFAR	Take_Kernel_Sem


	FARCODE HIGH
HIGHCODE segment

BREAK	<Read in a new .EXE segments>


;***	alloc_segments - allocate segments/selectors for requested mte.
;
;	Each segment in the module which requests to be
;	preloaded is allocated and all selectors for the
;	module are allocated for the child task.
;
;	ENTRY	(BP) = base of loader stack frame variables
;
;	EXIT	None
;
;	USES	All except BP

    ASSUME DS:NOTHING,ES:NOTHING,SS:TaskArea

	PUBLIC	alloc_segments
alloc_segments PROC NEAR
	PMONLY
	mov	ex_segno,1
	mov	ds,ex_mte_sel

als1:	call	CheckSig		; check if loader should abort
	mov	ax,ex_segno
	cmp	ax,ds:mte_segtbl_cnt	; for each segment in module
	ja	alsx			; end of segment table

	call	num_to_ste		; (SI) = offset to ste

;	(DS) = module table entry selector
;	(SI) = segment table entry offset
;	(BP) = base of loader stack frame variables
;
;	Compute segment allocation size and allocate memory for
;	the segment.  

als3:	call	get_alloc_siz		; LOADING flag is set by this call to
					; indicate the segment is being loaded
als4:	inc	ex_segno		; next segment
	jmp	als1

alsx:	return
alloc_segments ENDP


;***	load_module - Load a new exe file for given module table entry.
;
;	Each segment in the module which has not been
;	previously initialized will be initialized and
;	loaded as required.
;
;	ENTRY	(BP) = base of loader stack frame variables
;
;	EXIT	None
;
;	USES	All except BP

    ASSUME DS:NOTHING,ES:NOTHING,SS:TaskArea

	PUBLIC	load_module
load_module PROC NEAR			; (fh, mte)
	PMONLY

	mov	ds,ex_mte_sel		; (DS) = mte selector

	mov	cx,ds:mte_modref_cnt
	jcxz	lmd7

;	Must attach to each module referenced to get valid selectors
;	for each segment we may reference.  This is required since
;	demand loaded shared segments may reference a new module. If
;	the new module is loaded by another process our selectors would
;	not be updated.

lmd1:	push	cx
	mov	bx,cx
	dec	bx
	shl	bx,1			; (BX) = module index
	mov	si,ds:mte_modptrs	; (SI) = offset to modptrs table

	mov	si,ds:[si+bx]		; (SI) = mte handle
	or	si,si			; is mte handle valid
	jnz	lmd3			;  yes, attach to module
	mov	si,ds:mte_modref_off
	mov	si,ds:[si+bx]		; get offset to module name
	add	si,ds:mte_importnam_off ; (DS:SI) -> module name string
lmd2:	xor	ch,ch
	lodsb
	mov	cl,al			; (CX) = pathname length
	jmp	short lmd4

lmd3:	xor	cx,cx
	mov	ds,cx			; (DS) = 0 indicates (SI) = mod handle
	dec	cx			; (CX) = -1 (to set up glbl err txt)

lmd4:	mov	al,EXT_LIBRARY
	call	get_mte
	or	ax,ax			; check if error occured
	ljnz	load_error

;	(BX) = module table entry handle

	mov	si,bx
;;	mov	ax,LD_MTESEL
	call	MteMap			; map referenced mte segment
	mov	ds,ax
	test	ds:mte_flags,DOSMOD	; is this the DOSCALLS module?
	jz	lmd5			;  no, save module handle
	DosContext ds,,NOASSUME
	mov	si,offset DosGroup:doscall1
					; (DS:SI) -> DOSCALL1 module name string
	jmp	short lmd2		; load DOSCALL1 module

lmd5:	mov	si,ex_mte
;;	mov	ax,LD_MTESEL
	call	MteMap			; remap mte segment
	mov	ds,ax
;	mov	ex_mte_sel,ax
	pop	cx

	cmp	bx,ex_mte		; is this our self (occurs for DOSCALL1)
	je	lmd6			;  yes, don't save handle
	mov	si,cx			; get table index
	dec	si			; normalize index which is 1 based
	shl	si,1			; (SI) = module index
	add	si,ds:mte_modptrs	; compute offset into pointer table
	mov	ds:[si],bx		; save handle to module table entry

lmd6:	loop	lmd1			; get next module referenced


;	all segments in the module have been initialized
;	now load the required segments


lmd7:	mov	ex_segno,1

lmd8:	mov	ax,ex_segno
	cmp	ax,ds:mte_segtbl_cnt	; for each segment in module
	ja	lmdx			; end of segment table
	call	num_to_ste		; get offset to ste
	test	[si].ste_flags,LOADING	; does the segment need to be loaded?
	jz	lmd9			;  no, check the next segment
	call	load_segment		; load in the segment

lmd9:	inc	ex_segno		; next segment
	jmp	short lmd8

;	all required code and data segments have been loaded

lmdx:	or	ds:mte_flags,MTELOADED	; indicate segments have been loaded
	return
load_module ENDP


;***	load_segment - Load segment within module.
;
;		Reads the segment from the file and does the
;		relocation fixups.
;
;	ENTRY	(DS) = module table entry selector
;		(SI) = offset to segment table entry
;		(BP) = base of loader stack frame variables
;		ex_segno and ex_mte have been setup
;
;	EXIT	(DS) = module table entry selector
;		(SI) = offset to segment table entry
;
;	USES	All except DS, SI, BP

    ASSUME DS:NOTHING,ES:NOTHING,SS:TaskArea

	PUBLIC	load_segment
load_segment PROC NEAR
	PMONLY
	call	CheckSig		; check if the loader should abort
	mov	ex_ste,si		; setup segment table entry pointer
	test	[si].ste_flags,LOADED	; is the segment already loaded?
	ljnz	lsgx			;  yes, nothing to do

;	get alias GDT selector for the segment

	push	si			; save SI
	mov	si,[si].ste_seghdl
	call	MemMapAlias		; ring 0 selector returned in AX
	mov	ex_seg_alias,ax
	pop	si			; restore SI

	mov	dx,[si].ste_offset
	or	dx,dx			; zero offset indicates no file data

;	(DS:SI) -> seg tbl entry in the mte

	ljz	lsg4a			;  no file data, skip read
	xor	ax,ax

;	If !(RESOURCE segment)
;	  alignment-shift-count = mte_alignshft
;	else
;	  alignment-shift-count = mte_rsrc_alignshft

	mov	cx,ds:mte_alignshft	; get segment alignment shift count
ifndef WINTHORN
	test	[si].ste_flags,RSRCSEG
	jz	lsg1

	mov	cx,ds:mte_rsrc_alignshft
endif
lsg1:	shl	dx,1			; convert alignment offset
	rcl	ax,1			; to byte offset
	loop	lsg1

	mov	cx,ax			; (CX:DX) = file offset of segment data
	call	ex_LSeek

	mov	si,ex_ste
	mov	ds,ex_mte_sel
	test	[si].ste_flags,ITERATED
	jz	lsg4			; the data is not interated
	push	ss
	pop	ds			; (DS) = parent's PTDA selector
	mov	cx,4			; read iterations and bytes_of_data
	sub	sp,cx			; temp space to read iteration values
	mov	dx,sp
	call	ex_Read			; (fh, #iterations, 4);
	mov	si,ex_ste
	mov	ds,ex_mte_sel
	mov	cx,[si].ste_size	; get segment data
	sub	cx,4

	mov	ds,ex_seg_alias
	xor	dx,dx			; (DS:DX) -> address to read in to
	call	ex_Read			; (fh, seg_addr, segdata->size);

;	the following two words were read on to stack earlier...
;	that is why we are popping off stack... don't look for
;	corresponding pushes on stack.. they don't exist..

	pop	ax			; (AX) = number of iterations
	pop	di			; (DI) = number bytes of iterated data
	mov	es,ex_seg_alias		; (ES:DI) -> end of data

;	duplicate the iterated data for the required number of iterations

	mul	di			; compute #bytes of iterated data
	or	dx,dx			; did it overflow 64K?
	jz	lsg3			;  no, size is ok

	mov	ax,ERROR_ITERATED_DATA_EXCEEDS_64k
	jmp	lsg16

;	(DI) = #bytes of iterated data
;	     = ste_size - 4

lsg3:	or	ax,ax
	jz	lsg5			; nothing to do if result is zero
	mov	si,ex_ste
	mov	ds,ex_mte_sel
	mov	cx,[si].ste_minsiz
	jcxz	lsg3a
mov	ds,ex_seg_alias		; just read in the segment
	xor	dx,dx
	push	si
	call	ex_Read			; (fh, seg_addr, segdata->size);
	pop	si
	pop	ds			; (DS:SI) -> seg tbl entry in the mte
lsg4a:	mov	di,[si].ste_size
	jmp short lsg5a

;	(DI) -> byte after filedata in the segment

lsg5:	mov	ds,ex_mte_sel
	mov	si,ex_ste		; (DS:SI) -> seg tbl entry in the mte
lsg5a:	call	clear_segment		; uses all registers except BP

;	do relocation fixups for segment

	mov	si,ex_ste
	mov	ds,ex_mte_sel
	test	[si]jz	lsg27			; segment has no relocation items
	push	ss
	pop	ds			; (DS) -> PTDA stack
	push	cx			; get temp space on stack
	mov	dx,sp
	mov	cx,2			; read number of relocation items
	call	ex_Read
	pop	cx			; (CX) = number of relocation items
	mov	di,offset TaskArea:TempBuf+256

;	process each relocation item

lsg7:	or	cx,cx
	ljz	lsg27			; all done check thunks
	push	cx			; save number of relocation items

	cmp	di,offset TaskArea:TempBuf+256
	jb	lsg7a

;	If no type flags are set the item is skipped.  This allows
;	a blank relocation item to exist in the relocation records.

	push	ss
	pop	ds
	mov	dx,offset TaskArea:TempBuf
	mov	cx,(SIZE ri_s * 32)
	errnz	<(SIZE ri_s * 32) - 256>; use 256 bytes of TempBuf
	call	ex_Read			; read relocation records

	mov	di,offset TaskArea:TempBuf
lsg7a:	mov	ex_reloc_rec,di		; save pointer to current record

	mov	ds,ex_mte_sel
	mov	al,ss:[di].ri_flags
	and	al,TARGET_MASK		; (AL) = relocation target type
	cmp	al,INTERNALREF
	jne	lsg9			; not internal, check for import

	mov	ax,ss:[di].ri_target_seg
	xor	ah,ah
	cmp	al,0FFh			; is the segment movable?

;	the internal segment could be movable or fixed.. that
;	should not matter... the only difference is that the
;	ste_flags for the segment would specify not-movable
;	and at allocation time, the memory manager would be requested
;	to allocate a fixed segment in memory...
;	however, linker will generate the relocation record in that
;	format... hence we need to extract the segment number...

	jne	lsg8			; no, get fixed segment address
	mov	ex_tgtmte_sel,ds
	mov	bx,ex_mte		; (BX) = target mte handle

;	note that (DS) contains mte selector [not mte handle]

	jmp	lsg18

lsg8:	call	num_to_ste		; get offset to ste
	mov	bx,ss:[di].ri_target_off

;	the above target address should be a selector:offset pair
;	in the case of internal reference,
;	get the selector from the segment table entry.

	mov	dx,[si].ste_selector	; (DX:BX) = target address
	jmp	lsg25			; do relocation fixups

lsg9:	cmp	al,IMPORTNAME		; is target offset an import name?
	je	lsg10			;  yes, get module table entry
	cmp	al,IMPORTORDINAL	; is target offset an import ordinal #?
	ljne	lsg26			;  no, skip to next item

lsg10:	mov	si,ex_ste
	mov	si,[si].ste_flags
	and	si,SEGDPL		; privilege level mask = 0C00h
	cmp	si,RING_3		; RING_3 = 0C00h
	je	lsg10a

	mov	ax,ERROR_DYNLINK_FROM_INVALID_RING ; disallow dynlinks from ...
	jmp	lsg16			;  ... segments other than ring 3

lsg10a: mov	si,ss:[di].ri_target_modnam  ; get table index
	dec	si			; normalize index which is 1 based
	shl	si,1			; compute offset into pointer table
	add	si,ds:mte_modptrs
	mov	bx,ds:[si]		; get handle to module table entry
	or	bx,bx			; is this module DOSCALL1 reference?
	jz	lsg10b			;  yes, can only reference DOSCALLS

	mov	si,bx
	mov	ax,LD_TMPSEL
	call	TmpMap
	mov	es,ax
	mov	ex_tgtmte_sel,ax	; save target module selector

;	If this module is DOSCALL1 the reference is to DOSCALLS and
;	the DOSCALLS module must be searched first.

	test	es:mte_flags,DOSLIB	; is this DOSCALL1?
	jz	lsg12			;  no, go find procedure entry

lsg10b: DosContext ds
	mov	bx,ds:DosModHdl		; (BX) = handle to DOSCALLS mte

;	get the address of the target module

lsg11:	mov	si,bx			; (BX) = target module handle
	mov	ax,LD_TMPSEL
	call	TmpMap
	mov	ex_tgtmte_sel,ax	; save target module selector

;	(BX) = target module table entry handle

lsg12:	mov	di,ex_reloc_rec		; (SS:DI) -> relocation record
	mov	al,ss:[di].ri_flags
	and	al,TARGET_MASK		; (AL) = relocation target type
	cmp	al,IMPORTNAME		; is target offset an import name?
	jne	lsg18			;  no, target is an ordinal#

;	set (ES:DI) -> imported procedure name

	mov	es,ex_mte_sel
	mov	di,ss:[di].ri_target_impnam
	add	di,es:mte_importnam_off

;	if the input string starts with '#', then we have an ASCII ordinal

	xor	ch,ch
	mov	cl,es:[di]		; (CX) = string length
	mov	al,es:[di+1]
	cmp	al,'#'			; is this an ASCII ordinal?
	jne	lsg12a

	inc	di			; skip over string length
	inc	di			; (ES:DI) -> ASCII ordinal number
	dec	cx			; adjust for '#'

	call	AsciiToInt		; returns integer in AX
	jmp	short lsg19		; (AX) = ordinal #

;	(BX) = target mte handle
;	find proc name in target mte's resident name table

lsg12a: mov	ds,ex_tgtmte_sel	; (DS:0) -> traget module table entry
	push	es
	push	di			; (ES:DI) -> procname (length & name)
	call	get_ordnlnum		; get ordinal number
	pop	di
	pop	es			; (ES:DI) -> procname (length & name)

;	(AX) = errorcode / procedure_ordinal#

	jc	lsg15			; proc name not found

	jmp	short lsg19

;	if the module is DOSCALLS point to the DOSCALL1 string and
;	try again.
;
;	The DOSCALLS module has two module table entries, a resident
;	portion and a non-resident portion.  The resident module table
;	entry is part of the DOS kernel and remains in memory at all
;	times. It defines the direct links to the exported O.S. entry
;	points.	 The non-resident module table entry is kept on disk
;	in a module called DOSCALL1.

lsg15:	test	ds:mte_flags,DOSMOD	; is this the DOSCALLS module?
	jz	lsg16			;  no, report an error

	mov	ds,ex_mte_sel
	mov	bx,ex_reloc_rec		; (SS:BX) -> relocation record
	mov	si,ss:[bx].ri_target_modnam  ; get table index
	dec	si			; normalize index which is 1 based
	shl	si,1			; compute offset into pointer table
	add	si,ds:mte_modptrs
	mov	bx,ds:[si]		; (BX) = module table entry handle
	or	bx,bx			; is this module DOSCALL1?
	jz	lsg16			;  yes, report an error

	jmp	lsg11			; try DOSCALL1 module

;	(AX) = errorcode
;	if (AX) == ERROR_PROC_NOT_FOUND, (ES:DI) -> procname (length & name)
;		In load_error:
;		If (AX) == ERROR_PROC_NOT_FOUND
;		  ex_tgtmte_sel -> target mte

lsg16:	jmp	load_error

;	target offset is procedure ordinal number
;	(SS:DI) -> relocation record

lsg18:	mov	ax,ss:[di].ri_target_off

;	(BX:0) -> target module table entry handle
;	setup link to procedure call thunk using ordinal#

lsg19:	mov	si,ex_ste
	mov	ds,ex_mte_sel
	mov	cx,[si].ste_flags
	and	cx,SEGDPL		; (CX) = source segment SEGDPL flags
	mov	ds,ex_tgtmte_sel	; (DS:0) -> target module table entry

;	setup global error text

	xor	si,si
	push	cx			; (CX) = source segment SEGDPL flags
	mov	cx,-1			; indicate type of global error text
	call	SetupGlblErrTxt
	pop	cx			; (CX) = source segment SEGDPL flags

	call	get_entaddr		; get entry point address
	jc	lsg16

;	clear out global error text

	call	InvGlblErrTxt

	mov	bx,ax			; (DX:BX) = target address
lsg25:	mov	di,ex_reloc_rec		; (SS:DI) -> current relocation record
	call	dofixups		; do relocation fixups
	jc	lsg16			; CARRY set if there was error

;	(SS:DI) -> current relocation record

lsg26:	add	di,SIZE ri_s		; point to next relocation record
	pop	cx
	dec	cx
	jmp	lsg7			; get next relocation item

lsg27:	mov	ds,ex_mte_sel		; check thunk entries
	mov	dx,ex_segno
	mov	bx,offset DosHighCode:EditProlog
	call	EachSegEntry		; edit prolog for windows compatability

	mov	ds,ex_mte_sel
	mov	si,ex_ste
	or	[si].ste_flags,LOADED	; mark segmeata offset


;  32 bit linear address composition:
;
;  dddddddddd				10 bit page directory entry index
;	      tttttttttt		10 bit nt as loaded

	mov	ax,LD_TMPSEL
	call	TmpUnMap

;	release alias selector for the segment we just loaded

	xor	ax,ax
	xchg	ax,ex_seg_alias
	call	MemUnmapAlias

;	restore SI [so that (DS:SI) -> segment table entry in mte]

	mov	si,ex_ste
lsgx:	ret
load_segment ENDP


;**	clear_segment - fills segment with zero
;
;	ENTRY	(DI) = offset into seg at which clearing should start
;		(DS:SI) -> segment table entry in the mte
;		ex_seg_alias set up
;
;	EXIT	segment cleared
;
;	USES	AX,BX,CX,DX,DS,ES,SI,DI,FLAGS

	PUBLIC	clear_segment

clear_segment	PROC NEAR
	mov	bx,[si].ste_minsiz	; assume this is not auto data segment
	mov	dx,ex_segno
	cmp	dx,ds:mte_autods	; is this auto data segment
	jne	cs8			; this is not the auto data segment

	push	di		; offset into seg at which clearing should start
	mov	di,ex_seg_alias
	call	GetDescInfo

;	if (CARRY clear)
;	  (CX:DX) = segment size
;
;	(CX) == 1 & (DX) == 0 means size is 64k
;	when (CX) == 0, (DX) contains size

	mov	bx,dx			; (BX) = size, assuming CARRY clear
	pop	di		; offset into seg at which clearing should start
	jnc	cs8

	InternalError <clear_segment: GetDescInfo on alias selector failed>

;	(BX) = segment size

cs8:	xor	ax,ax
	or	bx,bx			; (BX) == 0 means segment size is 64k
	jz	cs12

;	(BX) = segment size
;	(DI) = offset into segment at which clearing should start

	sub	bx,di			; (AX:BX) = byte count
	or	bx,bx			; if (BX) == 0, there is no ...
	jz	csx			; ... clearing to do

	jmp short cs16			; clear segment

cs12:	dec	bx			; (BX) = FFFF [hex]
	sub	bx,di
	inc	bx
	or	bx,bx			; if (BX) == 0 segment size is 64k
	jnz	cs16

	inc	ax			; (AX:BX) = byte count
cs16:	mov	es,ex_seg_alias

;	(ES:DI) -> portion of segment to be cleared
;	(AX:BX) = byte count

	call	SegClear
csx:	ret
clear_segment	ENDP


;**	discard_segment - determine if a segment can be discarded
;
;	This routine is called by the memory manager to determine 
;	if a segment can be discarded, freed or swapped. It checks
;	the status of the segment. If the segment is currently being 
;	loaded, it tells the caller that the segment cannot be 
;	discarded, or freed, or swapped. If the segment is already 
;	loaded, and if the request is for discard, it marks the 
;	segment NOT loaded and tells the caller that the segment 
;	can be discarded. If the segment is already loaded, and 
;	the request is for swap, it notifies the caller that the 
;	segment can be swapped. If the request is for freeing the 
;	segment, this routine marks the segment NOT loaded, sets 
;	the segment handle in ste to zero, and notifies the caller 
;	that the segment can be freed.
;
;
;	ENTRY	(AX) = mte handle
;		(BX) = selector
;		(CX) = 0 for swap
;		     = 1 for discard
;		     = 2 for free
;
;	EXIT	CARRY clear
;		  go ahead & discard the segment
;		CARRY set
;		  cannot discard the segment now, try again later
;
;	USES	AX, CX, DX, SI, DI, DS, ES, FLAGS
;		BX is preserved

    ASSUME DS:NOTHING,ES:NOTHING,SS:TaskArea

Procedure discard_segment
	PMONLY

	push	cx			; save discard flag
	call	sel_to_segnum		; get the ste corresponding to selector
	pop	cx			; restore discard flag
	push	si			; save ste offset
	mov	si,ax			; (SI)=mte handle(param to MemMapAlias)
	call	MemMapAlias		; make mte addressable
	mov	ds,ax			; (DS) -> mte
	pop	si			; (DS:SI) -> ste

;	if (LOADING is set in ste_flags)
;	  set CARRY indicating that the segment cannot be discarded now
;	else
;	  {clear LOADED bit in ste_flags;
;	   clear CARRY indicating that the segment can be discarded}

	test	[si].ste_flags,LOADING	; is the segment BEING loaded?
	stc				; assume LOADING
	jnz	dsg4

	jcxz	dsg3			; (CX) is 0 if request for swap

	and	[si].ste_flags,NOT LOADED ; mark segment NOT loaded

	dec	cx
	jz	dsg3			; (CX) was 1 if request for discard
					; (CX) was 2 if request for free

	mov	[si].ste_seghdl,0	; mark segment freed

dsg3:	clc				; seg can be swapped/discarded/freed
dsg4:	pushf				; (AX) = sel (param to MemUnMapAlias)
	call	MemUnMapAlias		; free selector used for addressing mte
	popf
	ret
EndProc discard_segment


;**	get_segment - Demand load a segment the for module.
;
;	Setup the required parameters and load the segment by 
;	calling the load_segment routine.
;
;	WARNING:
;
;	To demand load a segment a set of local variables are
;	allocated on the stack.	 These variables are accessed
;	relative to the BP register and are defined using equated
;	variable names with an offset and BP base.  Therefore BP
;	must be preserved by all levels of the routines called
;	from this loader and must not be altered at any level if
;	access to these local variables is desired.
;
;	ENTRY	(AX) = owner, current PTDA handle, else mte handle
;		(BX) = selector
;		(SI) = segment handle
;
;	EXIT	(AX) = 0 if no error, else error code
;
;	USES	All except BP

    ASSUME DS:NOTHING,ES:NOTHING,SS:TaskArea

Procedure get_segment
	PMONLY	TASKTIME

	CALLFAR Take_Kernel_Sem		; grab thread serialization semaphore
	call	get_segment_sub		; get_segment(ax=mte,bx=selector)
	CALLFAR Clear_Kernel_Sem	; release thread serialization semaphore
	ret
EndProc get_segment

gsgie:	InternalError <get_segment: bad sel>

    ASSUME DS:NOTHING,ES:NOTHING,SS:TaskArea

Procedure get_segment_sub,,local
	enter	NEXE_VAR_LEN,0		; setup frame, allocate local variables

	push	si			; save segment handle
	push	bx			; save selector
	push	ax			; save owner
	call	GetLdrSem		; get semaphore to prevent race cond.
	pop	ax			; restore owner
	pop	bx			; restore selector
	pop	si			; restore segment handle
	jc	gsgx			; jmp if interrupted by a signal

	push	si			; save segment handle
	cmp	[ptda_handle],ax	; is it the current PTDA?
	jnz	gsg1

;	Scan modules referenced by this process to find the
;	module table entry containing this selector.

	push	bp
	push	bx			; selector to search for
	push	0			; space to return mte handle
	push	offset cs:scan_ste
	mov	bp,sp
	TaskContext es			; scan for current task
	call	scan_ptda_mods
	call	ResetUsedBit		; clear USED bit in mte list
	pop	ax			; discard routine address
	pop	ax			; (AX) = mte handle
	pop	bx			; (BX) = selector
	pop	bp

	or	ax,ax			; was the segment found?
	jz	gsgie			;  no, report internal error

;	(AX) = mte handle
;	(BX) = selector

gsg1:	call	sel_to_segnum		; find ste corresponding to selector

	mov	ex_mte,ax		; init module table entry handle
	xor	dh,dh
	mov	ex_segno,dx
	mov	ex_ste,si

	xor	ax,ax
	mov	ex_sfn,ax		; initialize stack variables
	mov	ex_seg_alias,ax
	mov	ex_type,al

	mov	si,ex_mte
;;	mov	ax,LD_MTESEL
	call	MteMap
	mov	ds,ax			; get a pointer to the pathname
	mov	ex_mte_sel,ax

	mov	ax,ds:mte_sfn		; setup existing system file number
	mov	ex_sfn,ax		; to load segment

	mov	si,ex_ste
	pop	[si].ste_seghdl		; set segment handle saved at entry

	or	[si].ste_flags,LOADING
	call	load_segment		; load in the segment

	and	[si].ste_flags,NOT LOADING
	test	[si].ste_flags,SHARED
	jnz	gsg2
	and	[si].ste_flags,NOT LOADED

gsg2:	
;;	mov	ax,LD_MTESEL
;;	call	TmpUnMap

	call	ClearLdrSem		; release loader semaphore

	xor	ax,ax			; return successful
gsgx:	leave				; release local variables
	ret
EndProc get_segment_sub


;**	scan_ste - Scan segment table for selector.
;
;	This subroutine is supplied to scan_ptda_mods.
;
;	This routine scans the segment table, looking for the 
;	desired selector. If it is found, the mte handle is saved 
;	for the caller and the carry flag is set to abort further 
;	scanning of the process modules.
;
;	ENTRY	(DS) = module table entry
;		(DX) = module table entry handle
;		(SS:BP+4) -> selector to search for
;		(SS:BP+2) -> location to save mte handle
;
;	EXIT	Carry set if selector was found
;		Carry clear if scan should continue
;
;	USES	AX, CX, SI, Flags

    assume DS:NOTHING,ES:NOTHING,SS:TaskArea

scan_ste PROC NEAR
	mov	ax,[bp+4]		; get selector from stack
	mov	cx,ds:mte_segtbl_cnt	; for each segment in module
	mov	si,ds:mte_segtbl_off
	jcxz	scs2

scs1:	cmp	[si].ste_selector,ax	; does the selector match?
	je	scs3			;  yes, save mte handle

	add	si,SIZE ste_s		; point at next ste
	loop	scs1

scs2:	clc				; allow scan to continue
	ret

scs3:	mov	[bp+2],dx		; save module handle for caller
	stc				; abort further scanning
	ret
scan_ste ENDP


;**	sel_to_segnum - given the selector, locates the ste
;
;	In the given mte, searches for the ste that contains the
;	given selector
;
;	ENTRY	(AX) = mte handle
;		(BX) = selector
;
;	EXIT	  (DL) = segment number
;		  (DI) = segment handle
;		  (SI) = ste offset
;
;	USES	CX, DX, DS, SI, DI, FLAGS
;		AX, BX are preserved

    ASSUME DS:NOTHING,ES:NOTHING,SS:TaskArea

sel_to_segnum PROC NEAR
	PMONLY

	push	ax			; (AX) = mte handle

	mov	si,ax			; (SI) set up to call MemMapAlias
	call	MemMapAlias		; make the mte addressable
	mov	ds,ax			; (DS) -> mte

	cmp	ds:mte_signature,EXE_VALID_NEWSIGNATURE
	je	stn2

	InternalError <sel_to_segnum: bad mte>

;	For (segno = 1; segno <= mte_segtbl_cnt; segno++)
;	  {find the entry for this segno;
;	   if (selector_in == ste.selector)
;	     {DL = segno;
;	      DI = ste.seghndl;
;	      break;
;	     }
;	  }

stn2:	xor	dx,dx			; initialise DX
	mov	cx,1			; (CX) = segment number
stn4:	cmp	cx,ds:mte_segtbl_cnt	; for each segment in module
	ja	stn8			; end of segment table

	mov	ax,cx			; set AX = segno (to call num_to_ste)
	call	num_to_ste		; (SI) = offset to segment table entry
	cmp	bx,[si].ste_selector	; is this the ste we are looking for?
	jne	stn6

;	found the ste containing the input selector

	mov	dl,cl			; (DL) = segment number
	mov	di,[si].ste_seghdl	; (DI) = segment handle
	jmp	short stn10

stn6:	inc	cx			; next segment
	jmp	short stn4

stn8:	pop	ax			; clean up the stack
	InternalError <sel_to_segnum: bad selector>

;	  (SI) = offset to ste containing input selector
;	  (DL) = segment number
;	  (DI) = segment handle

stn10:	push	si			; save ste offset
	mov	ax,ds			; (AX)=sel (parameter to MemUnMapAlias)
	call	MemUnMapAlias		; free selector used for addressing mte
	pop	si			; (SI) = ste offset
	pop	ax			; (AX) = mte handle
	ret
sel_to_segnum ENDP


;***	get_entaddr - get entry point address
;
;	For a given ordinal number, this routine locates the
;	appropriate entry in the thunk segment contained in
;	the module table entry. It builds the 32 bit pointer
;	which will be the entry point address of the function
;	(for which the ordinal number was specified).
;
;	ENTRY	(AX) = ordinal number
;		(CX) = source segment SEGDPL flags
;		(DS) = selector for module table entry
;
;	EXIT	Carry Flag Clear [Success]
;		  (DX:AX) = entry point address
;		Carry Flag set [Error]
;		  (AX) = ERROR_INVALID_ORDINAL
;			 ERROR_INVALID_SEGMENT_NUMBER
;
;	USES	AX, BX, CX, DX, DS, SI, ES, DI, Flags

    ASSUME DS:NOTHING,ES:NOTHING,SS:TaskArea

	PUBLIC	get_entaddr
get_entaddr	PROC	NEAR
	PMONLY
	or	ax,ax			; ordinal# should be non-zero
	ljz	gea5a			; report an error

	dec	ax
	mov	si,ds:mte_entrytbl_off	; (DS:SI) -> export entry table

	test	ds:mte_flags,DOSMOD	; Is target module DOSCALLS?
	jz	gea1			;  if not, skip ahead

	test	ax,DOSCALL1_ORD		; is this DOSCALL1 ordinal?
	jnz	gea0h

	mov	dx,3
	mul	dx
	cmp	ds:mte_entrytbl_len,ax
	jbe	gea5a			; ERROR_INVALID_ORDINAL

	add	si,ax
	lodsb				; get FLAG to check if absolute symbol
	or	al,al
	jz	gea0d

	xor	dx,dx			; selector part of target = null
	mov	ax,ds:[si]		; (AX) = HUGESHIFT/HUGEINCR
	jmp	geax

gea0d:	mov	dx,ds:[si]		; (DX) = GDT call gate selector
	xor	ax,ax			; (DX:AX) = target address
	jmp	geax

gea0h:	and	ax,NOT DOSCALL1_ORD	 ; clear MSB of DOSCALL1 ordinal

;	loop to determine which bundle the ordinal number belongs

gea1:	xor	bx,bx			; scan the entry table
	mov	bl,ds:byte ptr [si]	; (BX) = # entries in this bundle
	inc	si
	or	bx,bx			; is this the end of the table?
	jz	gea5a			;  yes, report an error

	cmp	ax,bx			; is the ordinal# in this bundle?
	jb	gea4			;  yes, index into bundle

	sub	ax,bx
	push	ax
	xor	ax,ax
	lodsb
	or	al,al			; is the bundle unused? (AL = 0)
	jz	gea3			;  yes, skip to next bundle

	inc	al			; is the bundle movable segs? (AL = FF)
	mov	ax,SIZE ent_s		; (AX) = fixed entry size
	jnz	gea2			;  no, use fixed size

	mov	ax,SIZE cte_s		; (AX) = movable entry size
gea2:	mul	bx
	add	si,ax
gea3:	pop	ax			; (DS:SI) -> next bundle
	jmp	short gea1

gea4:	mov	bx,ax			; found the bundle
	lodsb
	cmp	al,0FFh			; is the bundle movable segments?
	jne	gea5			;  no, it's a fixed bundle

	mov	ax,SIZE cte_s		; (AX) = movable entry size
	mul	bx
	add	si,ax			; (DS:SI) -> thunk entry
	mov	di,si
	mov	bx,[di].cte_off		; (BX) = offset of entry point
	mov	al,[di].cte_seg		; (AX) = segment number
	call	num_to_ste		; return ste offset for seg
	mov	ax,[si].ste_flags
	and	ax,SEGDPL
	cmp	ax,RING_3		; is this a ring 3 segment?
	je	gea7			;  yes, get segment selector

;	(CX) = source segment SEGDPL flags from entry param

	cmp	ax,cx			; is source = target dpl?
	je	gea7			;  yes, use segment address

	call	AllocCallGate		; allocate or attach to call gate
	mov	dx,[di].cte_sel
	xor	ax,ax			; (DX:AX) = entry point call gate
	jmp	short geax		; return - Carry is cleared

gea5:	or	al,al			; is the segment no. valid?
	jnz	gea6			;  yes,

	stc
	mov	ax,ERROR_INVALID_SEGMENT_NUMBER
	jmp	short geax

gea5a:	stc				; return an error
	mov	ax,ERROR_INVALID_ORDINAL
	jmp	short geax

gea6:	xor	ah,ah			; (AX) = segment number
	dec	ax
	shl	ax,2
	mov	di,ax
	shl	ax,1
	add	di,ax			; (DI) = (AX) * 12
	errnz	<(SIZE ste_s)-12>
	add	di,ds:mte_segtbl_off

	mov	ax,SIZE ent_s		; (AX) = fixed entry size
	mul	bx
	add	si,ax			; (DS:SI) -> fixed segment entry
	xchg	di,si
	mov	bx,[di].ent_off

;	(DS:SI) -> segment table entry
;	(DS:DI) -> entry table flags
;	(BX) = offset to entry point

gea7:	mov	dx,[si].ste_selector
	mov	ax,bx			; (DX:AX) = target address
	clc				; clear Carry flag (no error)

geax:	ret
get_entaddr	ENDP


;***	get_ordnlnum - get the ordinal number
;
;	given a proc name and a module table, checks to see if
;	the proc is in the resident/nonresident table. If the
;	proc is found in either of the tables, get_ordnlnum
;	returns the corresponding ordinal number. It returns
;	an error if the proc is found in neither of the tables.
;
;	ENTRY	(BX)		= handle to module table entry
;		(DS)		-> module table entry
;		(ES:DI)		-> proc name [length + name]
;
;	EXIT	CARRY Clear [success]
;		  (AX) = ordinal number
;		CARRY Set   [error]
;		  (AX) = errorcode
;			 ERROR_NOT_ENOUGH_MEMORY
;			 ERROR_PROC_NOT_FOUND
;
;	USES	AX, BX, CX, DX, ES, SI, DI, Flags
;		DS preserved

    ASSUME DS:NOTHING,ES:NOTHING,SS:TaskArea

	?frame = 0
	localvar mte_sel,WORD
	localvar procname,DWORD
	localvar nonres_sel,WORD
	FRAME_SIZ = ?frame

	PUBLIC	get_ordnlnum
get_ordnlnum	PROC	NEAR
	PMONLY

	push	bp
	mov	bp,sp
	sub	sp,FRAME_SIZ		; allocate local space

	mov	mte_sel,ds		; save mte selector
	mov	procnameh,es		; save procname selector
	mov	procnamel,di		; save procname offset

;	first, search the resident name table

	mov	si,ds:mte_resnamtbl_off ; DS:SI -> resident name table
	call	get_ordnlnum_sub
	ljnc	ordx

;	if the module is DOSCALLS module, no need to go on
;	because, there is no nonresident nametable for DOSCALLS

	test	ds:mte_flags,DOSMOD	; is this DOSCALLS module
	stc				;CARRY got cleared by TEST inst; set it
	ljnz	ordx

;	proc name was not found in the resident name table
;	need to search the non-resident name table
;
;	DosGroup:NonResNamTblHdl holds the handle to the first
;	nonresident name table in the list of nonresident name tables
;	linked by the handles. First word in each of the segments contain
;	the handle to the mte to which the nonresident name table belongs.
;
;	The structure of the segment is as follows:
;	    mte_handle	dw	?
;	    next_nrnseg dw	?
;	    nonresident nametable from .EXE file

	DosContext ds
	mov	si,ds:NonResNamTblHdl	; (SI) = handle to first nonres namseg

;	(BX) = mte handle
;	(SI) = handle to nonresident nametable segment
;
;	walk down the list of nonresident nametable segments
;	find the segment that has the matching mte

ord4:	or	si,si			; is this end of nonres namtbl list?
	jz	ord8

	call	MemMapAlias
	mov	nonres_sel,ax		; save selector to nonres namtbl seg
	mov	ds,ax			; (DS) -> nonresident nametable seg
	xor	si,si			; (DS:SI) -> nonresident nametable seg
	lodsw				; (AX)=hndl of mte to which seg belongs
	cmp	ax,bx			; is this the nonres namtbl seg we want
	je	ord16

	mov	si,ds:[si]		; get handle to next nonres namtbl seg
	push	si
	mov	ax,ds			; (AX) = selector to unmap
	call	MemUnmapAlias
	pop	si			; (SI) = handle to nonres namtbl seg
	jmp	ord4

;	(BX) = mte handle
;
;	the nonresident nametable seg we are looking for is not found in
;	the linked list. allocate memory and read in the nonres namtbl:
;
;	size of memory to allocate =
;		2 bytes (to hold mte handle)
;		2 bytes (link to next nonres namtbl segment)
;		mte_nonresnam_len  (size of nonres namtbl in .EXE file)

ord8:	push	bx			; (BX) = mte handle
	mov	ds,mte_sel		; (DS) -> mte
	mov	bx,ds:mte_nonresnam_len ;(BX) = num bytes of nonres tbl in file
	add	bx,4			;2 bytes each: mte hndl, lnk to next seg
	xor	ax,ax			; (AX:BX) = size of mem to allocate
	xor	cx,cx			; allocate as movable memory
	mov	di,SYSTEMOWNER		; system is the owner
	call	MemAlloc		; allocate memory for nonres namtbl seg
	pop	bx			; (BX) = mte handle
	jnc	ord12

	mov	ax,ERROR_NOT_ENOUGH_MEMORY	; (AX) = errorcode
	jmp	short ord32

;	(AX) = handle to allocated memory
;	(BX) = mte handle

ord12:	mov	si,ax			; (SI) = handle to allocated memory
	push	ax			; (AX) = handle to nonres tbl seg
	call	MemMapAlias
	mov	nonres_sel,ax		; save selector to nonres namtbl seg
	mov	es,ax
	xor	di,di			;(ES:DI) -> allocated nonres namtbl seg
	mov	es:[di],bx		; save mte handle in the nonres tbl seg
	pop	ax			; (AX) = handle to nonres tbl seg

;	(AX) = handle to nonresident nametable segment
;	link the new nonres namtbl segment to the list

	DosContext ds
	mov	bx,ds:NonResNamTblHdl	; link the . . .
	mov	ds:NonResNamTblHdl,ax	; . . . new segment . . .
	mov	es:2[di],bx		; . . . to the list

	mov	ds,mte_sel		; (DS) -> mte

;	seek to the beginning of the nonresident nametable in the file

	mov	dx,ds:word ptr mte_nonresnam_off
	mov	cx,ds:word ptr mte_nonresnam_off+2;(CX:DX) = offset to seek to
	mov	bx,ds:mte_sfn		; (BX) = system file number
	push	ds			; (DS) -> mte
	call	ex_LSeekN
	pop	es			; (ES) -> mte
	jc	ord28			; (AX) = errorcode if CARRY set

;	read the non-resident nametable from the file into nonres namtbl seg

	mov	cx,es:mte_nonresnam_len ;(CX) = size in bytes to read from file
	mov	dx,4			; skip over mte handle word & link word
	mov	ds,nonres_sel		; (DS:DX) = address to read into
	mov	bx,es:mte_sfn		; (BX) = system file number
	push	ds
	call	ex_ReadN
	pop	ds			; (DS) -> nonres namtbl seg
	jc	ord28			; (AX) = errorcode if CARRY set

;	(DS) -> nonres namtbl seg
;	call get_ordnlnum_sub to check if the procname is in the nonres namtbl

ord16:	mov	si,4			; skip over mtehndl & link words
	les	di,procname		; (ES:DI) -> procname
	call	get_ordnlnum_sub

;	(AX) = ordinal#/errorcode
;	CARRY must be preserved
;	unmap selector to nonres namtbl segment

ord28:	push	ax
	pushf
	mov	ax,nonres_sel		; (AX) = selector to nonres namtbl seg
	call	MemUnMapAlias
	popf
	pop	ax			; (AX) = ordinal# if CARRY is clear

ord32:	mov	ds,mte_sel		; (DS) -> mte
ordx:	mov	sp,bp			; release localbuf space
	pop	bp
	ret
get_ordnlnum	ENDP


;***	get_ordnlnum_sub - get the ordinal number
;
;	given a proc name and a resident/nonresident table
;	containing the procs exported by a module, checks to see
;	if the proc is in the table. If the proc is found
;	in the table, get_ordnlnum_sub gets the corresponding ordinal
;	number. It returns an error if the proc is not found
;	in the table.
;
;	ENTRY	(DS:SI)		-> (resident/nonresident) name table
;		(ES:DI)		=  pointer to proc name
;
;	EXIT	CARRY Clear [success]
;		  (AX) = ordinal number
;		CARRY Set   [error]
;		  (AX) = ERROR_PROC_NOT_FOUND
;
;	USES	AX, CX, DX, SI, DI, Flags
;		ES, DS preserved

    ASSUME DS:NOTHING,ES:NOTHING,SS:TaskArea

	PUBLIC	get_ordnlnum_sub
get_ordnlnum_sub PROC	NEAR
	PMONLY
	mov	dx,di			; (DX) = offset to proc name string
	cld				; for string comparison
	xor	ax,ax
	lodsb				; (AL) = length of module name
	add	si,ax			; skip over module name entry
	inc	si
	inc	si			; skip unused ordinal field
ords1:	xor	cx,cx
	mov	cl,ds:[si]		; (CX) = length of string to compare
	jcxz	ords4			; (CX) = 0 if end of table

	inc	cx			; include length byte in compare
	repe cmpsb			; compare the length, name string
	je	ords3			; name strings match, leave the loop

	mov	di,dx			; restore DI, (ES:DI) = ptr to proc name
ords2:	add	si,cx			; skip over remainder of name string
	inc	si
	inc	si			; skip over ordinal# to next name string
	jmp	short ords1		; try next name string

ords3:	lodsw				; (AX) = procedure ordinal number
	clc				; clear Carry flag (no error)
	jmp short ordsx

ords4:	mov	ax,ERROR_PROC_NOT_FOUND
	stc				; set Carry flag (error)
ordsx:	ret
get_ordnlnum_sub ENDP


;***	FreeNonResNamSegs - free nonresident nametable segments
;
;	walks down the list of nonresident nametable segments and
;	frees them up.
;
;	ENTRY	None
;
;	EXIT	all nonresident nametable segments are freed
;
;	USES	AX,BX,CX,DX,DS,ES,DI,SI,Flags


    ASSUME DS:NOTHING,ES:NOTHING,SS:TaskArea

	PUBLIC	FreeNonResNamSegs
FreeNonResNamSegs PROC	NEAR
	PMONLY

	DosContext ds
	mov	si,ds:NonResNamTblHdl

fnrn4:	or	si,si			; is this end of nonres namtbl list?
	jz	fnrn12

	push	si
	call	MemMapAlias		; make the segment addressable
	pop	si			; (SI) = handle to nonres name seg
	mov	es,ax
	xor	di,di			; (ES:DI) -> nonresident nametable seg
	mov	bx,es:2[di]		; get handle to next nonres namtbl seg

	mov	ax,es			; (AX) = alias selector to unmap
	push	si
	call	MemUnmapAlias		; free the alias selector
	pop	si			; (SI) = handle of segment to be freed
	push	bx			; (BX) = hndl to next nonres namtbl seg
	call	MemFree			; free up the segment
	pop	si			; (SI) = hndl to next nonres namtbl seg
	jnc	fnrn8

	InternalError <FreeNonResNamSegs: error, bad handle>
fnrn8:	jmp	fnrn4

fnrn12: DosContext ds
	xor	ax,ax
	mov	ds:NonResNamTblHdl,ax	; set head pointer to NIL
	ret
FreeNonResNamSegs ENDP


;***	get_alloc_siz - verify allocation size and allocate memory
;
;	If the segment's file size is larger than the minimum
;	allocation size an error is reported.  If this is an
;	automatic data segment the heap and stack size are
;	added to the minimum allocation size and the
;	resulting memory size is allocated.  Both the
;	allocation size and the segment handle are saved in
;	the segment table entry. Also indicate the segment
;	is being loaded so that it's memory will not be
;	discarded during the load.
;
;	ENTRY	(DS) = module table entry selector
;		(SI) = segment table entry offset
;		(BP) = base of loader stack frame variables
;
;	EXIT	(DS:SI) preserved
;
;	USES	All except DS, SI, BP
;
;	COMMENT
;		get_alloc_siz is called from alloc_segments

    ASSUME DS:NOTHING,ES:NOTHING,SS:TaskArea

	PUBLIC	get_alloc_siz
get_alloc_siz PROC NEAR
	PMONLY
	push	ds			; save ste pointer it's destroyed
	push	si			; during SegAlloc and SegAttach
	mov	ax,[si].ste_flags	; (AX) = segment flags in ste
	xor	cx,cx
	mov	ex_malloc_flags,cx	; init handle/memory allocation flags

;	(AX) = segment flags in ste
; 	(CX) = 0 

	test	ax,PRELOAD		; is preloading requested?
	jz	gas0			;  no, segment will be demand loaded

	or	ch,D_PRES		; preload segment
	or	[si].ste_flags,LOADING	; indicate segment is being loaded
	test	ax,SHARED		; is segment shared?
	jz	gas0			;  no, don't turn off PRELOAD
	and	[si].ste_flags,not PRELOAD ; only preload once

;	(AX) = segment flags in ste
;	(CH) = access flags
;	(CL) = 0
;
;	check the privilege level specified for the segment and
;	set the privilege level in CH.
;	allowing only levels 2 & 3....

gas0:	and	ax,SEGDPL		; privilege level mask = 0c00h
	cmp	ax,RING_2		; RING_2 = 0800h
	jne	gas1d

	or	ch,D_DPL2		; ring 2
	DosContext es,,NOASSUME
	test	es:IoplEnabled,1	; are IOPL segments allowed?
	jnz	gas2			;  yes, allocate it

	call	ChkIOPLTable		; check if module is in IOPL table
	jnc	gas2			;  yes, ok to allocate segment

	mov	ax,ERROR_IOPL_NOT_ENABLED
	jmp	load_error		; report an error

;	(AX) = (ste_flags) & SEGDPL
;	(CH) = access flags	
;	(CL) = 0

gas1d:	cmp	ax,RING_3		; RING_3 = 0c00h
	mov	ax,ERROR_INVALID_SEGDPL ; assume error
	jne	gas3			; error

	or	ch,D_DPL3		; ring 3

;	(CH) = access flags	
;	(CL) = 0

gas2:	mov	bx,[si].ste_minsiz
	or	bx,bx			; is the request for 64K
	jz	gas4

	cmp	bx,[si].ste_size	; is the minimum size valid?
	jae	gas4			;  yes, the size is ok

	mov	ax,ERROR_INVALID_MINALLOCSIZE

;	(AX) = error code

gas3:	jmp	load_error

;	(CH) = access flags	
;	(CL) = 0

gas4:	mov	dx,ex_segno		; (DX) = segment number being allocated
	cmp	dx,ds:mte_autods
	jne	gas5			; this is not the auto data segment

;	(BX) = segment minimum allocation size specified in .EXE file
;
;	We have an AUTO DATA segment.
;
;	We must have ((BX) + heapsize + stacksize) <= 64k

	xor	dx,dx			; (DX) = 0
	xor	ax,ax			; (AX) = 0
	or	bx,bx			; (BX) == 0 means size is 64k
	jnz	gas4a

	inc	dx			; we have size already = 64k
gas4a:	add	bx,ds:mte_heapsiz	; (BX) = (BX) + heapsize
	adc	dx,ax			; (DX) = (DX) + CARRY
	add	bx,ds:mte_stacksiz	; (BX) = (BX) + stacksize
	adc	dx,ax			; (DX) = (DX) + CARRY

;	if ((DX) > 1)
;	  ERROR
;	else
;	  {				; (DX) == 0 or (DX) == 1
;	   if (DX == 1)
;	     if ((BX) != 0)
;	       ERROR
;	  }

	mov	ax,ERROR_AUTODATASEG_EXCEEDS_64k	; assume error
	cmp	dx,1
	ja	gas3

	jne	gas5

	or	bx,bx
	jnz	gas3

;	(BX) = segment size 
;	(CH) = access flags
;	(CL) = 0

gas5:	mov	cl,SA_LDT		; allocate LDT selector
	test	ds:mte_flags,LIBRARYMOD
	jz	gas6			; program modules use private selector

	or	cl,SA_SHARED		; library modules use shared selector

;	(BX) = segment size 
;	(CH) = access flags
;	(CL) = selector allocation flags
;	ex_malloc_flags = 0

gas6:	mov	ax,[si].ste_flags
	test	ax,SHARED
	jz	gas7			; segment in not shared

	or	ex_malloc_flags,HF_SHARED	; segment is shared

;	(AX) = ste_flags
;	(BX) = segment size 
;	(CH) = access flags
;	(CL) = selector allocation flags

gas7:	test	ax,ERONLY
	jnz	gas8

	or	ch,D_W			; data is writeable, code is readable

gas8:	and	ax,TYPE_MASK
	cmp	ax,CODE_TYPE		; is this a code segment?
	jne	gas9			;  no, it is data

	or	ch,D_CODE		;  set code access type

;	if (code_segment or shared_data_segment) 
;	  set owner = mte_handle		
;	else
;	  set owner = child_task's_PTDA_handle

gas9:	test	ex_malloc_flags,HF_SHARED
	jz	gas15			; segment is non-shared, allocate it

	mov	di,ex_mte		; mte is shared segment owner
	mov	ex_seg_owner,di
	mov	ax,[si].ste_seghdl
	or	ax,ax			; has the segment been allocated?
	jz	gas25			;  no, go allocate it

	xchg	si,ax			; (SI) = segment handle
	mov	dx,[ptda_child]		; (DX) = child task's PTDA
	call	ex_SegAttach
	jmp	short gas28		; go restore DS, SI & return

;	(BX) = segment size 
;	(CH) = access flags
;	(CL) = selector allocation flags

gas15:	mov	di,[ptda_child]		; segment not shared
	or	di,di
	jnz	gas20

	mov	di,[ptda_handle]
gas20:	mov	ex_seg_owner,di		; ptda is the owner of segment
	mov	di,[si].ste_selector	; non-shared segment
	or	di,di			; has the segment been allocated?
	jz	gas25

;   	(DI) = selector to use

	or	cl,SA_SPECIFIC		; allocate specific selector

;	(BX) = segment size 
;	(CH) = access flags
;	(CL) = selector allocation flags
;	(DI) = selector to use if SA_SPECIFIC is set
;	ex_seg_owner set up

gas25:	mov	ax,[si].ste_flags
	test	ax,MOVABLE
	jz	gas26

	or	ex_malloc_flags,HF_SWAPPABLE	; segment is swappable
gas26:	test	ax,DISCARD		; is discardable flag set?
	jz	gas27			; no, segment isn't discardable

	or	ex_malloc_flags,HF_DISCARDABLE
	and	ex_malloc_flags,(NOT HF_SWAPPABLE) ;can't have discard & swap

;	(DS) -> mte

gas27:	mov	ax,ex_segno		; (AX) = segment number
	call	IsHuge			; if CARRY set, we have huge seg and...
					; ...(AX) = number of segments
;	if (we have huge seg)
;	  {ex_segno = ex_segno - 1 + num_segs
;	   call ex_HugeAlloc}
;	else
;	  {set AX = 0
;	   call ex_SegAlloc}

	mov	dx,[ptda_child]		; (DX) = child task's PTDA handle
	mov	si,ex_malloc_flags	; (SI) = memory/handle allocation flags
	jnc	gas27b

	push	ex_segno		; save starting segment number of huge
	dec	ex_segno		; ex_segno = ex_segno - 1 +
	add	ex_segno,ax		;	     num_segs
	xchg	ax,bx			; (BX) = number of segments in huge seg
	pop	ax			; (AX) = starting segment number
	mov	di,ex_seg_owner		; (DI) = segment owner

;	(AX) = starting segment number
;	(BX) = number of segments in huge segment
;	(CH) = access flags
;	(CL) = selector allocation flags
;	(DX) = child task's PTDA handle
;	(DI) = segment owner
;	(SI) = memory/handle allocation flags
;	(DS) -> mte

gas27a: call	ex_HugeAlloc
	pop	si
	pop	ds			;(DS:SI) -> ste of starting seg in huge
	jmp	short gas30

;	(BX) = segment size 
;	(CH) = access flags
;	(CL) = selector allocation flags
;	(DX) = child task's PTDA handle
;	(DI) = selector to use, if SA_SPECIFIC is set 
;	(SI) = memory/handle allocation flags
;	ex_seg_owner = segment owner

gas27b: xor	ax,ax
	call	ex_SegAlloc

;	we get here also after returning from ex_segattach... in that case
;	the only purpose for getting here is to restore DS and SI

gas28:	pop	si
	pop	ds			; (DS:SI) = seg table entry in mte
	mov	[si].ste_seghdl,ax	; set segment handle.

	mov	ax,[si].ste_flags
	and	ax,SEGDPL		; privilege level mask = 0c00h
	mov	dx,ax
	errnz	<SEGDPL-0C00h>
	shr	dx,10			; convert SEGDPL bits to RPL bits
	and	bx,RPL_CLR		; clear ring bits
	or	bx,dx			; set RPL bits
	mov	[si].ste_selector,bx	; set segment selector.

	cmp	ax,RING_3		; RING_3 = 0C00h
	je	gas30			; no callgates for ring 3

	test	[si].ste_flags,MOVABLE
	mov	ax,ERROR_RING2SEG_MUST_BE_MOVABLE
	ljz	gas3			; ring 2 segments can't be fixed

	push	si
	mov	dx,ex_segno
	mov	bx,offset DosHighCode:GetCallGate
	call	EachSegEntry		; allocates or attach to callgates
	pop	si

gas30:	ret
get_alloc_siz ENDP


;***	ChkIOPLTable - Check if module is in IOPL table.
;
;	Search table of module names which are always allowed
;	to load IOPL segments.	The IOPL module name table
;	is a series of strings with a length byte followed by
;	the module name and is terminated by a null length
;	byte.
;
;	ENTRY	(DS) = module table entry selector
;		(ES) = DosGroup
;		(BP) = base of loader stack frame variables
;
;	EXIT	CARRY clear if module is in table
;		CARRY set if not in table
;
;	USES	DI, Flags

    ASSUME DS:NOTHING,ES:NOTHING,SS:TaskArea

ChkIOPLTable PROC NEAR
	PMONLY
	push	cx
	push	si
	mov	si,ds:mte_resnamtbl_off ; point to module name entry
	mov	di,offset DosGroup:IOPLTable
	xor	cx,cx

cit2:	add	di,cx
	mov	cl,es:[di]
	stc				; assume name is not in table
	jcxz	citx			; zero indicates end of table

	inc	cx			; include length byte in compare
	push	si
	repe cmpsb			; compare the length, name string
	pop	si
	jne	cit2
					; return, carry clear, name in table
citx:	pop	si
	pop	cx
	ret
ChkIOPLTable ENDP


;**	IsHuge - determines if we have a huge segment
;
;	ENTRY	(DS) -> mte
;		(AX) = starting segment number
;
;	EXIT	CARRY set if we have a huge segment
;			(AX) = number of segments in the huge segment
;		CARRY clear if we don't have a huge segment
;			(AX) = 1
;
;	USES	AX, FLAGS
;		DS is preserved

    ASSUME DS:NOTHING,ES:NOTHING,SS:TaskArea

	PUBLIC	IsHuge
IsHuge PROC NEAR
	push	bx
	push	cx
	push	dx
	push	si

;	make the ste of starting segment number addressable

	push	ax
	call	num_to_ste		; returns (DS:SI) -> segment's ste
	pop	ax			; (AX) = starting segment number
	mov	dx,1			; initialise number of segments

	mov	cx,ds:[si].ste_flags	; get this segment's flags
	and	cx,NOT(RELOCINFO OR LOADING) ;mask relocation and loading flags

;	while (NOT End_of_Segment_Table) do
;	  if (the size is 64k)
;	    {look at next segment's ste 
;	     if flags are equal		; RELOCINFO masked out from the flag
;	       num_segs +=1;		; we have a huge segment
;	     else
;	       break;
;	    }

ih4:	mov	bx,ds:[si].ste_minsiz	; get this segment's size
	or	bx,bx			; (BX) == 0 if size is 64k
	jnz	ih8

	inc	ax			; look at next segment's ste
	cmp	ax,ds:mte_segtbl_cnt	; is this end of segment table?
	ja	ih8

	add	si,(SIZE ste_s)		; (DS:SI) -> segment's ste

	mov	bx,ds:[si].ste_flags	; get this segment's flags
	and	bx,NOT RELOCINFO	; mask out relocation flag

	cmp	cx,bx			; are the flags equal?
	jne	ih8

	inc	dx			; increment num_segs
	jmp	ih4

;	if (num_segs > 1)
;	  set CARRY to indicate we have a huge segment
;	else
;	  clear CARRY to indicate we don't have a huge segment

ih8:	cmp	dx,1
	stc				; assume huge segment
	jne	ih12

	clc				; indicate we don't have huge segment
ih12:	mov	ax,dx			; (AX) = number of segments in huge seg
	pop	si
	pop	dx
	pop	cx
	pop	bx
	ret
IsHuge ENDP


;***	dofixups - Do the fixups following the source chain.
;
;	ENTRY	(DX:BX) = target address
;		(SS:DI) -> relocation record structure (ri_s)
;		(BP) = base of loader stack frame variables
;
;	EXIT	CARRY clear if Success
;		CARRY set if Error
;		  (AX) = ERROR_RELOC_CHAIN_XEEDS_SEGLIM
;			 ERROR_INFLOOP_IN_RELOC_CHAIN
;
;	USES	AX, CX, SI, DS

    ASSUME DS:NOTHING,ES:NOTHING,SS:TaskArea

	PUBLIC	dofixups
dofixups PROC NEAR
	PMONLY
	mov	si,ss:[di].ri_source
	mov	ds,ex_seg_alias		; (DS:SI) = source address
	lsl	ax,ex_seg_alias		; get the segment limit from descriptor
	jnz	dfui

;	(AX) = segment limit

dfu2:	mov	cx,ax			; (CX) = segment limit
	shr	ax,1			; (AX) = number of words in the segment

	test	ss:[di].ri_flags,ADDITIVE
	jz	dfu8			; follow chain of fixups

	cmp	cx,si			; CX must be > SI, otherwise Error
	jbe	dfu16

	call	fixup			; (DX:BX) = target address
	clc				; Success
	jmp	short dfux

;	(AX) = number of words in the segment (source_count)
;	(CX) = segment limit

dfu8:	cmp	cx,si			; CX must be > SI, otherwise Error
	jbe	dfu16

;	Infinite loop in relocation source chain is detected as follows:
;
;	    source_count--;
;	    if (source_count == 0)
;	      {				/* ERROR */
;	       return (ERROR_INFLOOP_IN_RELOC_CHAIN);
;	      }

	dec	ax			; decrement source_count
	jz	dfu12

	push	[si]			; save next source address
	push	ax			; (AX) = source_count
	call	fixup			; (DX:BX) = target address
	pop	ax
	pop	si			; get next source address
	cmp	si,-1			; is this the end of the source chain?
	jne	dfu8			;  no, fixup new source address

	clc				; Success
	jmp	short dfux

dfu12:	mov	ax,ERROR_INFLOOP_IN_RELOC_CHAIN ; set errorcode
	jmp	short dfu20

dfu16:	mov	ax,ERROR_RELOC_CHAIN_XEEDS_SEGLIM
dfu20:	stc				; Error
dfux:	ret

dfui:	InternalError <dofixups: LSL instruction failed>

dofixups ENDP


;***	fixup - Fixup source address.
;
;	Fixes up the source address defined by the source type.
;	The source address offset may be modified.
;
;	ENTRY	(DS:SI) = source address
;		(DX:BX) = target address
;		(SS:DI) -> relocation record structure (ri_s)
;		(BP) = base of loader stack frame variables
;
;	EXIT	NONE
;
;	USES	AX, SI
;

    ASSUME DS:NOTHING,ES:NOTHING,SS:TaskArea

	PUBLIC	fixup
fixup PROC NEAR
	PMONLY
	mov	al,ss:[di].ri_stype
	and	al,SOURCE_MASK
	cmp	al,FAR_ADR		; if source type is FAR or OFFSET
	je	fix1			;  do an offset fixup
	cmp	al,OFF_ADR
	jne	fix4

;	do the offset fixup and increment the source pointer
;	for the far address segment fixup

fix1:	test	ss:[di].ri_flags,ADDITIVE
	jz	fix2			; no additive fixup
	add	[si],bx			; add target to source contents
	jmp	short fix3
fix2:	mov	[si],bx			; replace source contents with target
fix3:	add	si,2			; increment pointer to segment value

fix4:	cmp	al,FAR_ADR		; if source type is FAR or SEGMENT
	je	fix5			;  do segment fixup
	cmp	al,SEG_ADR
	jne	fixx

fix5:	test	ss:[di].ri_flags,ADDITIVE
	jz	fix6			; no additive fixup
	add	[si],dx			; add target to source contents
	jmp	short fixx
fix6:	mov	[si],dx			; replace source contents with target

fixx:	ret
fixup ENDP


;***	CheckSig - check if the loader should abort
;
;	Check the DESTROY (FF_DES) and SIGNALPENDING (FF_SIG) flags
;	of the parent. If either of these flags are ON, set error
;	code and jump to load_error
;
;	ENTRY	NONE
;
;	EXIT	if (FF_DES OR FF_SIG) 
;	          {
;	          set AX = ERROR_INTERRUPT
;	          jump to load_error
;	          }
;
;	USES	AX,BX,FLAGS

    ASSUME DS:NOTHING,ES:NOTHING,SS:TaskArea

CheckSig PROC NEAR
	mov	bx,ss:CurrTCB
	test	ss:[bx].TCBForce_Flg,FF_DES+FF_SIG ; if DESTROY set OR ...
	jz	chksx			; ... SIGNALPENDING set, abort loading

	mov	ax,ERROR_INTERRUPT
	jmp	load_error

chksx:	ret
CheckSig ENDP


HIGHCODE ends

	END
