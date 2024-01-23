PAGE    ,132
TITLE   V86Mode - Modes switching support for V86 mode
NAME    V86Mode

;       SCCSID = @(#)v86mode.asm	10.5 87/02/27
;*
;*  Title:
;*      V86Mode
;*
;*  Author:
;*      Benjamin W. Slivka
;*      (c) Microsoft Corporation
;*      1987
;*
;*  Description:
;*      Support for switching between V86 and protected modes on the 80386.
;*
;*  Modification History:
;*      11 Jan 1987 - BWS
;*          Initial version.
;*      13 Jan 1987 - BWS
;*          Added V86ToProt implementation.
;*      14 Jan 1987 - BWS
;*          Put V86 Stack in PTDA.
;*      16 Jan 1987 - BWS
;*          Remove V86Mode flag; figured out programmatic way to distinguish
;*          between V86 and PROT modes!
;*      17 Jan 1987 - BWS
;*          Added V86GPFault handler.
;*      20 Jan 1987 - BWS
;*          Added V86Services to support both V86ToProt and loadall386.
;*      22 Jan 1987 - BWS
;*          Got loadall386 working.
;*      23 Jan 1987 - BWS
;*          Load segment registers to point to not-present pages in ProtToV86.
;*      25 Jan 1987 - BWS
;*          Add V86PageFault to handle page faults in V86 mode (do IntErr).
;*      27 Jan 1987 - BWS
;*          Add V86Trap1 and V86Trap3 to reflect V86 traps 1 and 3.

.xlist
include dosseg.inc
include dossymb.inc
include dmavirt.inc
include loadall.inc
include mi.inc
include page.inc
include pmode.inc
include tcbdata.inc
include v86mode.inc
.list

.386p

        modname v86mode

        ReferGlobal     Loadall386Seg,WORD
        ReferGlobal     Seg3xBoxPTDA,WORD
        ReferGlobal     TSS,WORD
	ReferGlobal	Direct_IO,byte		; !0 if someone is trying to
	ReferGlobal	V86BoxFG,BYTE

        ReferTask       CurrTCB,WORD
        ReferTask       V86ModeCount,BYTE
        ReferTask       V86Stack,WORD
	ReferTask	V86LinearAlias,DWORD
	ReferTask	V86EgaIndexReg,BYTE
	ReferTask	V86EgaRegister,BYTE

	ReferHighGlobal GDT_3xBoxPTDA,WORD
        ReferHighGlobal GDT_INTSTACK,WORD       ; Interrupt stack descriptor
        ReferHighGlobal GDT_LinearSpace,WORD    ; 4Gbyte Linear space
        ReferHighGlobal GDT_PTDA,WORD           ; Current Task's PTDA
	ReferHighGlobal GDT_PhysicalEga,WORD
	ReferHighGlobal GDT_VirtualEga,WORD
	ReferHighGlobal V86CurLinAlias,DWORD	; Current V86LinearAlias
	ReferHighGlobal V86SGtoPTH,WORD 	; Map SG to PT handle
	ReferHighGlobal V86FGContext,WORD	; Foreground 3xBox PT handle
	ReferHighGlobal _V86Context,WORD	; Current 3xBox PT handle

	extrn		LinToVirtDM_CALL:NEAR	; Calls V86Services for loadall
	extrn		_V86SetPageTable:far
	extrn		_V86SetVirtualEga:far
	extrn		_V86SetPhysicalEga:far


        NEARCODE
CODE SEGMENT

BREAK <V86ToProt - Switch from v86 mode to Prot mode>
;***    V86ToProt - Switch from v86 mode to Prot mode
;
;       ENTRY:  (SS) = DosGroup -or- TaskArea
;
;       EXIT:   (CS) = DosCode
;               (SS) = Selector
;               (DS) = 0
;               (ES) = 0
;               (FS) = 0
;               (GS) = 0
;               Interrupt and arithmetic flags left alone
;               NMI and 287 unaffected
;
;       USES:   VM flag,DS,ES,FS,GS

    assume  ds:nothing,es:nothing,ss:nothing

Procedure   V86ToProt,near
	pushf
	cli
entry   V86ToProt_CALL
        int     INT_V86Services         ; Switch to Prot Mode

;*  ENTRY:    Interrupts Off
;             (SS:BP) = top of V86 stack frame (caused by software interrupt)
;             (SS:SP) = V86 stack frame with AX, BX, and BP saved on top of it.
;
;                          <- Top of V86Stack (TSS Ring 0 Stack)
;             (0) (old GS)    (SS:BP+32)
;             (0) (old FS)    (SS:BP+28)
;             (0) (old DS)    (SS:BP+24)
;             (0) (old ES)    (SS:BP+20)
;             (0) (old SS)    (SS:BP+16)
;                (old ESP)    (SS:BP+12)
;             (old EFLAGS)    (SS:BP+ 8)
;             (0) (old CS)    (SS:BP+ 4)
;                (old EIP) <- (SS:BP)
;                 (old AX)
;                 (old BX)
;                 (old BP) <- (SS:SP)

entry V86ToProt_HANDLE

        PMONLY

	DosContext <ds>

        mov     ax,[bp].vf_SS.LowWord   ; (ax) = old stack paragraph address
        mov     bx,GSEL GDT_INTSTACK    ; (bx) = interrupt stack selector
        cmp     ax,DosGroup             ; DosGroup => Interrupt stack
        jz      short vtp1              ; Use GDT_INTSTACK

        mov     bx,GSEL GDT_PTDA	; (bx) = PTDA selector
	cmp	ax,[Seg3xBoxPTDA]	; assume running on v86 PTDA
	jz	short vtp1		; jmp if so

        InternalError <V86ToProt: bad stack>

;*  ENTRY:  (BX) = selector of old stack
;           (BP) = V86 interrupt stack frame
;	    (SP) = saved AX,BX,BP
;
;   NOTE:   We move old SS:SP to V86 stack, restore registers, and switch
;	    back to old stack.	This picture show how we edit the stack:
;
;            Before                               After
;   --------------------------           ------------------------
;   (0) (old CS)    (SS:BP+4)		   (old SS') <- (SS:BP+4)
;      (old EIP) <- (SS:BP)	 ====>	    (old SP) <- (SS:BP+2)
;       (old AX)                            (old AX)
;       (old BX)                            (old BX)
;       (old BP) <- (SS:SP)                 (old BP) <- (SS:SP)

vtp1:	mov	ax,[bp].vf_ESP.LowWord	; (ax) = old sp
	mov	[bp],ax
	mov	[bp+2],bx

;*  Revert to old TSS Ring 0 stack, if appropriate

        mov     ax,GSEL GDT_PTDA
	mov	es,ax
    assume  es:TaskArea

        sub     V86ModeCount,1
        jae     short vtp2              ; V86 stack remains as TSS Ring 0 stack

        mov     bx,CurrTCB              ; (bx) = current TCB
        lea     ax,[bx].TCBPMStack      ; (ax) = top of TCB stack
	mov	[TSS].tss_sp0,ax	; Set new ring 0 stack

;*  Trash DS, clean up stack, and return to caller.

vtp2:	xor	ax,ax
        mov     ds,ax                   ; (ds) = unusable
    assume  ds:nothing

        pop     bp
        pop     bx
        pop     ax
	lss	sp,dword ptr ss:[esp]	; Switch to old stack
	popf				; Restore flags
	ret				; Return to caller

EndProc     V86ToProt


BREAK <ProtToV86 - Switch from prot mode to v86 mode>
;***    ProtToV86 - Switch from prot mode to v86 mode
;
;       ENTRY:  (SS) = low-memory stack
;
;       EXIT:   (CS) = DosCode
;               (SS) = Segment
;               (DS) = 64K segment with not-present pages
;               (ES) = 64K segment with not-present pages
;               (FS) = 64K segment with not-present pages
;               (GS) = 64K segment with not-present pages
;               IOPL=3
;               VM bit set
;
;       USES:   VM flag,DS,ES,FS,GS
;
;       WARNING:    DosCode MUST BE TILED!

    assume  ds:nothing,es:nothing,ss:nothing

Procedure   ProtToV86,near

        PMONLY

        pushf
        cli                             ; Just in case
        SaveReg <ax,bx>

;*  Ensure TSS Ring 0 stack is V86Stack for current PTDA

        mov     ax,GSEL GDT_PTDA
        mov     ds,ax
    assume  ds:TaskArea

        inc     V86ModeCount
        jnz     short ptv1              ; Ring 0 TSS is already V86 Stack

        DosContext <ds>
        mov     bx,offset DosGroup:TSS  ; (ds:bx) = TSS
        mov     ax,offset TaskArea:V86Stack ; (ax) = V86Stack
        mov     [bx].tss_sp0,ax         ; Set new ring 0 stack

ptv1:   mov     bx,ss                   ; (BX) = selector for stack
        invoke  SelToSeg                ; (AX) = segment number for SS
        jc      short ptvie             ; Couldn't convert to segment

        mov     bx,sp                   ; (BX) = Save SP for entry to V86 mode
        pushd   BAD_V86_SEG             ; (New GS)
        pushd   BAD_V86_SEG             ; (New FS)
        pushd   BAD_V86_SEG             ; (New DS)
        pushd   BAD_V86_SEG             ; (New ES)
        pushd   0,ax                    ; (SS)
        pushd   0,bx                    ; (SP)
        pushfd                          ; (EFLAGS)
        pushd   0,cs                    ; (CS) ; must be tiled!
        pushd   0,<offset cs:ptv2>      ; (EIP) ; transfer control here

        or      [esp].vf_EFLAGS.HighWord,f_VM    ; Set VM bit
        or      [esp].vf_EFLAGS.LowWord,f_IOPL3  ; Ensure IOPL=3
        and     [esp].vf_EFLAGS.LowWord,NOT f_NT ; Clear NT bit
        iretd                           ; Go to V86 mode
    assume  ds:nothing

ptv2:   RestoreReg  <bx,ax>
        popf                            ; Restore flags
	ret

ptvie:  InternalError <ProtToV86: stack in high memory>

EndProc     ProtToV86


BREAK <V86WhatMode - set carry and zero flags according to CPU mode>
;***    V86WhatMode - set carry and zero flags according to CPU mode
;
;       ENTRY:  REAL mode -or-
;               PROT mode ring 0 -or-
;               V86 mode
;
;       EXIT:   Use following conditional jumps to test state:
;               JC  foo     ; PROT mode
;               JZ  foo     ; REAL mode
;               JA  foo     ; V86 mode
;               JNC foo     ; Not PROT mode
;
;       USES:   AX,flags
;
;       WARNING:    THE SYSTEM MUST NEVER SET IOPL=1.
;
;       HOW:    if PE bit clear then
;                   REAL mode
;               else if IOPL can be changed then
;                   PROT mode
;               else
;                   V86 mode
;               endif

    assume  ds:nothing,es:nothing,ss:nothing

Procedure   V86WhatMode,near

        smsw    ax                      ;; (ax) = .... .... .... ...P
        shr     ax,1                    ;; (CF) = P
        mov     ax,1                    ;; (ax) = REAL mode
        jnc     short wm1               ;; CPU is in real mode

;;* Distinguish between V86 and PROT modes

        pushf                           ;; Save original flags
        cli                             ;; We are changing IOPL, don't allow ints
        pushf
        pop     ax                      ;; (ax) = ..IO .... .... ....
        and     ax,NOT f_IOPLmask       ;; (ax) = ..00 .... .... ....
        or      ax,f_IOPL1              ;; (ax) = ..01 .... .... ....
        push    ax
        popf                            ;; Try to change IOPL
        pushf
        pop     ax                      ;; (ax) = ..IO .... .... ....
        popf                            ;; Restore original IOPL and IF
        and     ax,f_IOPLmask           ;; (ax) = 00IO 0000 0000 0000
        sub     ax,f_IOPL1              ;; (ax) = 0 if PROT mode
        je      short wm1               ;; CPU is in protected mode

        mov     ax,2                    ;; (ax) = V86 mode

wm1:    cmp     ax,1                    ;; Set flags
	ret

EndProc     V86WhatMode

CODE    ENDS


	  FARCODE HIGH
HIGHCODE    segment

;***	V86EnsureContext - Swap 3xBox context if necessary
;
;	ENTRY:	(ax) = PT handle of 3xBox that needs to be mapped in
;		CLI (Interrrupts must be off!)
;
;	EXIT:	none
;
;	USES:	All but BP
;
;	EFFECTS Specified 3xBox mapped in.

    assume  ds:nothing,es:nothing,ss:nothing
Procedure   V86EnsureContext,hybrid
	PMONLY

	cmp	ax,-1
	je	short vecx		; 3xBox background/not present

	DosHighContext <ds>
	cmp	ax,_V86Context
	je	short vec1		; This 3xBox is already mapped in

	SaveReg <ax>
	call	V86SwapContext		; Map in new 3xBox
	RestoreReg <ax> 		; (ax) = current PT handle

vec1:	sub	ax,V86FGContext 	; (ax) = 0 => mapped 3xBox is foreground
					;   (ax) == 0	    (ax) != 0
	cmp	ax,1			;   (CF) == 1	    (CF) == 0
	sbb	ax,ax			;   (ax) == -1	    (ax) == 0
	DosContext <es>
	mov	V86BoxFG,al		; Indicate if this 3xBox is foreground

vecx:	ret

EndProc     V86EnsureContext

BREAK <V86SwapContext - Swap 3xBox context>
;***	V86SwapContext - Swap 3xBox context
;
;	ENTRY:	(ax) = pt handle
;		(ds) = DGroup
;		GDT_3xBoxPTDA maps new PTDA
;		CLI (Interrupts must be off!)
;
;	EXIT:	none
;
;	USES:	All but BP,DS
;
;	EFFECTS Specified 3xBox mapped in.

    assume  ds:DGROUP,es:nothing,ss:nothing
Procedure   V86SwapContext
	PMONLY

	cld				; Ensure state of DF
	push	ax			; New PT handle (for _V86SetPageTable)
	call	_V86SetPageTable	; V86SetPageTable(ax=pthandle)
	add	sp,2			; Clean PT handle off stack
	call	V86PropagateData	; Copy global data to new 3xBox
	ret

EndProc     V86SwapContext


BREAK <V86PropagateData - copy data on switch to different 3xBox>
;***	V86PropagateData - copy data on switch to different 3xBox
;
;	ENTRY:	(ds) = new 3xBox PTDA
;		V86CurLinAlias = V86LinearAlias of previous mapped 3xBox
;		CLI (Interrupts must be off!)
;
;	EXIT:	Global data propagated from old to new V86 linear space.
;		V86CurLinAlias = new V86LinearAlias
;
;	USES:	All but BP,DS

vcc_s	struc
vcc_offst   dd	?	    ; Offset in linear space
vcc_len     dd	?	    ; Length in bytes of region to copy
vcc_s	ends

;*  Table of global data areas to copy from old to new V86 linear space.
;   These numbers came from ../src/bios/ibmrom.inc;  We made intelligent
;   guesses - may have to be revised.

V86CopyCtl  label   word
    vcc_s   <043Fh, 000Ah>  ; Motor_Status, Motor_Count, Diskette_Status,
			    ; CMD_BLOCK, HD_ERROR, NEC_Status
    vcc_s   <0474h, 0001h>  ; Disk_Status1
    vcc_s   <0490h, 0006h>  ; DSK_STATE, DSK_OP_STATE, DSK_TRK
    vcc_s   <-1   ,    -1>  ; [End of table]

    assume  ds:DGROUP,es:nothing,ss:nothing
Procedure   V86PropagateData
	PMONLY

	SaveReg <bp>
	mov	ax,GSEL GDT_3xBoxPTDA
	mov	es,ax
	mov	edx,es:V86LinearAlias	; (edx) = offset of new V86 linear space
	mov	ebx,V86CurLinAlias	; (ebx) = offset of old V86 linear space
	SaveReg <ds>
	mov	ax,GSEL GDT_LinearSpace
	mov	ds,ax			; (ds) = linear space
	mov	es,ax			; (es) = linear space
    assume  ds:nothing,es:nothing

	mov	bp,offset DosHighCode:V86CopyCtl ; (cs:bp) = control table

vsd1:	mov	esi,cs:[bp].vcc_offst	; (esi) = offset from V86 LinAddr 0
	mov	edi,esi 		; (edi) = offset from V86 LinAddr 0
	add	esi,ebx 		; (esi) = old V86 space
	add	edi,edx 		; (edi) = new V86 space
	mov	ecx,cs:[bp].vcc_len	; (ecx) = number of bytes to copy
	rep movs byte ptr es:[edi],byte ptr ds:[esi] ; Propogate global data
	add	bp,size vcc_s		; (bp) = next control entry
	cmp	cs:[bp].vcc_offst,-1
	jne	vsd1			; Do next entry

	RestoreReg <ds>
    assume  ds:DGROUP
	mov	V86CurLinAlias,edx	; Set new current Linear Alias
    assume  ds:nothing
	RestoreReg <bp>
	ret
EndProc     V86PropagateData


;***	V86EgaBackground - Change specified 3xBox to use logical video buffer
;
;	ENTRY:	(ax) = pt handle
;		CLI (Interrupts must be off!)
;		CLD
;
;	EXIT:	none
;
;	USES:	All but BP

    assume  ds:nothing,es:nothing,ss:nothing
Procedure   V86EgaBackground

	DosHighContext <ds>
	push	ax			; (ax) = pt handle
	call	_V86SetVirtualEga	; V86SetVirtualEga(ax=pthandle)
	add	sp,2			; Clean pthandle off stack

	push	GSEL GDT_PhysicalEga
	pop	ds			; (ds) = copy from physical buffer
	push	GSEL GDT_VirtualEga
	pop	es			; (es) = copy to virtual bufffer
	call	V86CopyEgaMem		; Copy phys to virt buffer
	ret
EndProc     V86EgaBackground


;***	V86EgaForeground - Change specified 3xBox to use physical EGA RAM
;
;	Restore video RAM and cursor data.
;
;	ENTRY:	(ax) = pt handle
;		CLI (Interrupts must be off!)
;		CLD
;
;	EXIT:	none
;
;	USES:	All but BP

    assume  ds:nothing,es:nothing,ss:nothing
Procedure   V86EgaForeground
	push	GSEL GDT_VirtualEga
	pop	ds			; (ds) = copy from virtual bufffer
	push	GSEL GDT_PhysicalEga
	pop	es			; (es) = copy to physical buffer
	call	V86CopyEgaMem		; Copy virt to phys buffer

	DosHighContext <ds>
	push	ax			; (ax) = pt handle
	call	_V86SetPhysicalEga	; V86SetPhysicalEga(ax=pthandle)
	add	sp,2			; Clean pthandle off stack

	push	GSEL GDT_3xBoxPTDA
	pop	ds
    assume  ds:TaskArea
	mov	al,CRT_CURSOR_HIGH	; (al) = cursor position registers
	mov	bx,word ptr V86EgaRegister[CRT_CURSOR_HIGH] ; (bx) = data
	call	Out6845Word

	mov	al,CRT_CURSOR_START	; (al) = cursor shape registers
	mov	bx,word ptr V86EgaRegister[CRT_CURSOR_START] ; (bx) = data
	call	Out6845Word
	ret
EndProc     V86EgaForeground


;***	V86CopyEgaMem - Copy EGA video RAM to/from logical buffer
;
;	ENTRY:	(ds) = source buffer selector
;		(es) = destination buffer selector
;		CLD
;
;	EXIT:	none
;
;	USES:	All but BP

    assume  ds:nothing,es:nothing,ss:nothing
Procedure   V86CopyEgaMem
	xor	si,si			; (ds:si) = source
	mov	di,si			; (es:di) = destination
	mov	cx,V86EGASIZE SHR 2	; (cx) = number of dwords to copy
	rep movsd			; Copy words
	ret
EndProc     V86CopyEgaMem


;***	Out6845Word - Send a word to two successive 6845 registers
;
;	Output the low and high bytes of (BX) to two successive 6845 registers.
;
;	ENTRY:	(AL) = index of lower-numbered 6845 register
;		(BL) = byte to output to 6845 register (AL)
;		(BH) = byte to output to 6845 register (AL+1)
;
;	EXIT:	Word set in two successive 6845 registers.
;
;	USES:	AX,DX

Procedure   Out6845Word

	mov	ah,bl			; (ah) = high byte
	mov	dx,ADDR_6845_INDEX	; (dx) = 6845 port address
	out	dx,ax			; Set high byte
	inc	al			; (al) = 6845 low register
	mov	ah,bh			; (ah) = low byte
	out	dx,ax			; Set low byte
	ret
EndProc     Out6845Word

HIGHCODE    ends


          FARCODE HIGH2
HIGH2CODE segment

V86SM      struc
vsm_caller  dw      ?                   ; Offset in DosCode of caller
vsm_handler dw      ?                   ; Far ptr of handler
            dw      ?
V86SM      ends

V86ServiceMap   label   word
        V86SM   <offset DosCode:V86ToProt_CALL, offset DosCode:V86ToProt_HANDLE, DosCode>
        V86SM   <offset DosCode:LinToVirtDM_CALL, offset HIGH2CODE:LinToVirtDM_HANDLE, HIGH2CODE>
        V86SM   <-1,-1,-1>


Break <V86Services - Ring 3 386 Interrupt Gate entry>
;***    V86Services - Ring 3 386 Interrupt Gate entry
;
;       We get control here when an "int INT_V86Services" is executed in V86
;       mode.  We consult a table to determine if the interrupt was issued
;       by the kernel to get a particular service.  If so, we dispatch to the
;       handler.  Otherwise, a 3xBox issued the interrupt for its own purpose,
;       so we reflect the interrupt.
;
;       Otherwise, we assume that the instruction was executed by user code,
;       so we reflect the interrupt to the 3xBox interrupt handler.
;
;       ENTRY:  Interrupts Off
;               (SS:SP) = V86 Stack in 3xBox PTDA
;
;                            <- Top of V86Stack (TSS Ring 0 Stack)
;               (0) (old GS)    (SS:SP+32)
;               (0) (old FS)    (SS:SP+28)
;               (0) (old DS)    (SS:SP+24)
;               (0) (old ES)    (SS:SP+20)
;               (0) (old SS)    (SS:SP+16)
;                  (old ESP)    (SS:SP+12)
;               (old EFLAGS)    (SS:SP+ 8)
;               (0) (old CS)    (SS:SP+ 4)
;                  (old EIP) <- (SS:SP) at entry from interrupt
;
;       EXIT:   Interrupts Off
;
;               (SS:BP) = top of V86 stack frame (caused by software interrupt)
;               (SS:SP) = V86 stack frame with AX, BX, and BP saved on top.
;
;                            <- Top of V86Stack (TSS Ring 0 Stack)
;               (0) (old GS)    (SS:BP+32)
;               (0) (old FS)    (SS:BP+28)
;               (0) (old DS)    (SS:BP+24)
;               (0) (old ES)    (SS:BP+20)
;               (0) (old SS)    (SS:BP+16)
;                  (old ESP)    (SS:BP+12)
;               (old EFLAGS)    (SS:BP+ 8)
;               (0) (old CS)    (SS:BP+ 4)
;                  (old EIP) <- (SS:BP)
;                   (old AX)
;                   (old BX)
;                   (old BP) <- (SS:SP)
;
;       USES:   flags

    assume  ds:nothing,es:nothing,ss:nothing

V86Services_SaveSize    equ     6       ; Size of pushes below

Procedure   V86Services

        push    ax
        push    bx
        push    bp
        mov     bp,sp                   ; (bp) = (sp)
        add     bp,V86Services_SaveSize ; (bp) = (sp) at procedure entry

;*  CS:IP points to instruction following INT INT_V86Services.
;   Adjust IP to point to the INT instruction.

        sub     [bp].vf_EIP.LowWord,2   ; Point IP to INT instruction

        cmp     word ptr [bp].vf_CS,DosCode
        jne     short vs2               ; Not V86Services call

;*  Scan V86 Services map table for IP match

        mov     ax,[bp].vf_EIP.LowWord  ; (ax) = IP of INT instruciton
        mov     bx,offset HIGH2CODE:V86ServiceMap ; (cs:bx) = map table

vs1:    cmp     ax,cs:[bx].vsm_caller   ; Check caller IP
        je      short vs3               ; Valid caller, dispatch

        add     bx,size V86SM           ; (cs:bx) = next entry
        cmp     word ptr cs:[bx],-1
        jne     short vs1               ; Another entry to test

;*  Interrupt not for V86Services, so reflect
;
;   NOTE:   We adjusted the IP in the V86 frame to point to the INT
;           instruction.  This is necessary for V86Reflect, since a software
;           interrupt puts CS:EIP pointing at the following instruction.

vs2:    pop     bp                      ; Restore registers
        pop     bx
        pop     ax
        sub     sp,VGFA                 ; Adjust for DWORD errcd and IP
        push    eax
        mov     ah,INT_V86Services      ; (ah) = interrupt number
        transfer V86Reflect             ; Go reflect interrupt

;*  Transfer control to handler

vs3:    jmp     dword ptr cs:[bx].vsm_handler

EndProc     V86Services


BREAK <LinToVirtDM_HANDLE - finish loadall for LinToVirtDM>
;***    LinToVirtDM_HANDLE - finish loadall for LinToVirtDM
;
;       ENTRY:  Interrupts Off
;               (SS:BP) = top of V86 stack frame (caused by software interrupt)
;               (SS:SP) = V86 stack frame with AX, BX, and BP saved on top of it.
;
;                            <- Top of V86Stack (TSS Ring 0 Stack)
;               (0) (old GS)    (SS:BP+32)
;               (0) (old FS)    (SS:BP+28)
;               (0) (old DS)    (SS:BP+24)
;               (0) (old ES)    (SS:BP+20)
;               (0) (old SS)    (SS:BP+16)
;                  (old ESP)    (SS:BP+12)
;               (old EFLAGS)    (SS:BP+ 8)
;               (0) (old CS)    (SS:BP+ 4)
;                  (old EIP) <- (SS:BP)
;                   (old AX)
;                   (old BX)
;                   (old BP) <- (SS:SP)
;
;       EXIT:   Via 386 loadall back to V86 mode.
;
;       USES:   All

Procedure   LinToVirtDM_HANDLE

        PMONLY

        DosContext <ds>
        movzx   edi,Loadall386Seg       ; (edi) = RM segment of loadall buffer
        shl     edi,4                   ; (edi) = offset of loadall buffer
        mov     ax,GSEL GDT_LinearSpace
        mov     ds,ax                   ; (ds:edi) = loadall buffer
    assume  ds:nothing

;*  Preserve Debug control registers (DR0-3 are unaffected by LOADALL)

        mov     eax,dr6
        mov     dword ptr [edi].ll_DR6,eax
        mov     eax,dr7
        mov     dword ptr [edi].ll_DR7,eax

;*  Make sure flags and CR0 are correct

        or      [edi].ll_EFLAGS.HighWord,f_VM       ; Set VM bit in flags
        and     [edi].ll_EFLAGS.LowWord,NOT f_NT    ; Clear NT bit

        or      [edi].ll_CR0,MSW_PE                 ; Set PE bit
        and     [edi].ll_CR0,NOT MSW_TS             ; Clear TS bit
        mov     [edi].ll_CR0.HighWord,8000h         ; Set PG bit

;*  Store LDT info in loadall cache

        sldt    bx                      ; (bx) = LDT Register
        mov     [edi].ll_LDTR,bx        ; Store LDTR in loadall buffer
        DosHighContext <es>,ax          ; (es:0) = GDT
        mov     ax,es:[bx].d_limit      ; (ax) = LDT limit
        mov     ds:[edi].ll_LDTcache.dc_LIMITlo,ax
        mov     ax,es:[bx].d_loaddr                 ; (ax) = bits 0..15 of base addr
        mov     ds:[edi].ll_LDTcache.dc_BASElo,ax
        mov     al,es:[bx].d_hiaddr                 ; (al) = bits 16..23 of base addr
        mov     ah,es:[bx].d_extaddr                ; (ax) = bits 16..31 of base addr
        mov     ds:[edi].ll_LDTcache.dc_BASEhi,ax
        mov     al,es:[bx].d_access                 ; (al) = access byte
        mov     ds:[edi].ll_LDTcache.dc_AR2,al

;*  Do the LOADALL. The loadall buffer is statically initialized with a
;   return CS:IP pointing to "loadall_ret".  We come back to this code here
;   immediately after the loadall.  Then we do the ret back to the caller.
;   This is quicker than formatting up CS:IP in the loadall buffer every time
;   to point back to the caller.

        mov     ax,ds
        mov     es,ax                   ; (es:edi) = loadall buffer
    assume  es:nothing
        loadall386                      ; Execution continues at loadall_ret in
                                        ; LinToVirtDM.
EndProc     LinToVirtDM_HANDLE

BREAK <dtB,dtS,dtE,dtDisp - Dispatch Table Macros>

;***	dtB - BEGIN dispatch table
;
;	ENTRY:	name = name of dt table
;		badAddr = local label for unspecified entries
;	EXIT:	Table name declared.
;		?sIndex = -1, indicates no entries specified yet
;		?badAddr set to default handler for unspecified entries
dtB    macro   name,badAddr	       ;; Beginning of Dispatch Table
    ?sIndex = -1
    ?badAddr	=   badAddr
    &name&DispTable label   word	;; Start of dispatch table
endm


;***	dtS - SPECIFIC dispatch table entry
;
;	ENTRY:	name  = name of dt table
;		index = index of entry
;		addr  = address of handler
;	EXIT:	Unspecified entries prior to "index" filled in;
;		Specified entry filled in.
;		?sIndex = next index to fill in;
;		?Low&name = lowest specified index, if first dtS
dtS	macro	name,index,addr 	;; Specific entry in Dispatch Table
    if ?sIndex eq -1			;; First entry in table
	Low&name    equ &index		;; Set lowest index
	?sIndex = index
    endif
    if	?sIndex gt index
	.err	;dtS: index out of order
    else
	if ?sIndex lt index		;; Fill unspecified entries
	    rept    index - ?sIndex
		dw  ?badAddr
	    endm
	endif
	dw	addr			;; Specified entry
	?sIndex = index+1		;; Set new start index
    endif
endm


;***	dtE - END dispatch table
;
;	ENTRY:	name  = name of dt table
;	EXIT:	?High&name defined as highest specified index
dtE	macro	name			;; End of Dispatch Table
    High&name	equ ?sIndex-1		;; Set highest iIndex
endm


;***	dtDisp - Make dispatch using dt table
;
;	ENTRY:	name = name of dt table
;		badIndex = local label if index not in table range
;		(BX) = index of handler to call
;
;	EXIT:	IF index in "reg" is in range THEN
;		    call handler in table
;		ELSE
;		    transfer to "badIndex"
;
;	USES:	BX,flags

dtDisp	macro	name,badIndex
	sub	bx,Low&Name		;; (bx) = index into table
	jb	short badIndex		;; Index is too small
	cmp	bx,High&Name - Low&Name ;;
	ja	short badIndex		;; Index is too large
	shl	bx,1
	call	cs:&name&DispTable[bx]	;; Call handler
endm


;*  GPDispTable - Dispatch table for GP-Fault, indexed by opcode byte

	dtB   GP,	     GPUnexpected
	dtS   GP,mi_INT,     GPInt
	dtS   GP,mi_IN_port, GPInByte
	dtS   GP,mi_OUT_port,GPOutByte
	dtS   GP,mi_IN_DX,   GPInDXByte
	dtS   GP,mi_OUT_DX,  GPOutDXByte
	dtE   GP

;*  InDispTable - Dispatch table for IN instructions, indexed by port address

	dtB   In,		 InUnexpected
	dtS   In,ADDR_6845_DATA, InCrtData
	dtE   In

;*  OutDispTable - Dispatch table for OUT instructions, indexed by port address

	dtB   Out,		  OutUnexpected
	dtS   Out,ADDR_6845_INDEX,OutCrtIndex
	dtS   Out,ADDR_6845_DATA, OutCrtData
	dtS   Out,FD_PDAT,	  OutFloppy
	dtE   Out


BREAK <V86GPFault - check for (and handle) GP-fault from V86 mode>
;***    V86GPFault - check for (and handle) GP-fault from V86 mode
;
;	ENTRY:	Interrupts OFF;
;		From trap_0d handler in (trap286.asm)
;		(0) (old GS)
;		(0) (old FS)
;		(0) (old DS)
;		(0) (old ES)
;		(0) (old SS)
;		   (old ESP)
;		(old EFLAGS)
;		(0) (old CS)
;		   (old EIP)
;		(0)(errcode)
;		    (ret IP) <- (SS:SP) at entry from interrupt
;
;	EXIT:	IF expected GP fault THEN
;		    [See individual GPXxx rountines]
;		ELSE
;		    Return to caller
;
;	USES:	[See individual GPXxx rountines]

VGFA	equ	3*2			; Add this to SP to point to EIP
VGFB	equ	VGFA+(3*4+2)		; Adjustment to BP to use vf struc

vgfFrame    struc	    ; The following frame reflects the stack after
vgf_BP	    dw	    ?	    ; registers are saved at entry to V86GPFault.
vgf_EDX     dd	    ?	    ;
vgf_EBX     dd	    ?	    ;
vgf_EAX     dd	    ?
vgf_ret     dw	    ?
vgf_errcd   dd	    ?
vgfFrame    ends

    assume  ds:nothing,es:nothing,ss:nothing
Procedure   V86GPFault,near
        PMONLY

        test    [esp].VGFA.vf_EFLAGS.HighWord,f_VM ; Check VM bit in EFLAGS
	jz	short vgfx		; Not a V86 GP-fault

	cld				; Establish assumed string direction
        push    eax                     ; 4 bytes
        push    ebx                     ; 4 bytes
	push	edx			; 4 bytes
        push    bp                      ; 2 bytes
	mov	bp,sp			; (ss:bp) = vgfFrame

        mov     ax,GSEL GDT_LinearSpace
        mov     ds,ax                   ; (ds) = maps all of linear space
        mov     es,ax                   ; (es) = maps all of linear space
	movzx	eax,[bp].VGFB.vf_cs	; (eax) = old CS (paragraph)
        shl     eax,4                   ; (eax) = old CS (bytes)
	movzx	ebx,[bp].VGFB.vf_eip.LowWord ; (ebx) = old IP (bytes)
        add     ebx,eax                 ; (ds:ebx) = old CS:IP

        mov     ax,ds:[ebx]             ; (ax) = faluting instruction
	movzx	bx,al			; (bx) = opcode
	dtDisp	GP,vgf1 		; Dispatch based on opcode
GPReturn:
	pop	bp			; Restore registers
	pop	edx
	pop	ebx
	pop	eax
	add	sp,VGFA 		; Remove caller's IP and error code
	iretd				; Return to V86 mode

;*  Got unexpected GP fault from V86 mode.
;   Return to default GP-Fault handler to display registers.

GPUnexpected:
	pop	ax			; Discard ret addr from call in dtDisp
vgf1:	pop	bp			; dtDisp jumps here if opcode out of range
	pop	edx
	pop	ebx
	pop	eax
vgfx:	ret				; Return to standard GP-Fault handler

EndProc     V86GPFault


;** GPInt - Handle faulting INT nn instruction
;
;	ENTRY:	(ah) = 2nd byte of instruction (interrupt number)
;		(dx) = original DX
;		(ds) = GDT_LinearSpace
;		(es) = GDT_LinearSpace
;
;		 [V86 Frame] (see V86Frame in v86mode.inc)
;		(0)(errcode)
;		    (ret IP)
;		       (EAX)
;		       (EBX)
;		       (EDX)
;			(BP)
;		  (GPReturn) <- (SS:SP) at entry from V86GPFault
;
;	EXIT:	Build IRET frame on original stack
;		Change V86 frame to point to INT handler
;
;	USES:	flags

Procedure   GPInt
        SaveReg <edi>
        movzx   ebx,ah                  ; (ebx) = int number
        shl     ebx,2                   ; (ebx) = 4*(int number)
                                        ; (ds:ebx) = V86 IntVec table entry
	sub	[bp].VGFB.vf_esp.LowWord,6 ; Make room for IRET frame on old stack
	movzx	eax,[bp].VGFB.vf_ss	; (eax) = old SS (paragraph)
        shl     eax,4                   ; (eax) = old SS (bytes)
	movzx	edi,[bp].VGFB.vf_esp.LowWord ; (esi) = old SP (bytes)
        add     edi,eax                 ; (ds:edi) = old SS:SP

;*  (1) Build IRET frame on old stack
;   (2) Change (CS:EIP) in V86 frame to point to V86 int handler
;   (3) Clear interrupt flag in EFLAGS in V86 frame, since software interrupt
;       handlers are entered with interrupts disabled.

        mov     ax,ds:[ebx].Offst       ; (ax) = int handler IP
	xchg	ax,[bp].VGFB.vf_eip.LowWord ; (ax) = old IP; Set handler IP
        add     ax,2                    ; (ax) = IP of next instruction
	stos	word ptr es:[edi]	; Put (IP) on old stack

        mov     ax,ds:[ebx].Segmt
	xchg	ax,[bp].VGFB.vf_cs	; (ax) = old CS; Set handler CS
	stos	word ptr es:[edi]	; Put (CS) on old stack

	mov	ax,[bp].VGFB.vf_eflags.LowWord ; (ax) = old flags
	stos	word ptr es:[edi]	; Put (FLAGS) on old stack

;*  Disable interrupts and turn off NT bit for V86 interrupt handler

	and	[bp].VGFB.vf_eflags.LowWord,(not f_Interrupt) and (not f_NT)

        RestoreReg  <edi>
	ret				; Return to GPFault
EndProc     GPInt


;***	GPInByte - Handle faulting "OUT n,AL"
;***	GPInDXByte - Handle faulting "OUT DX,AL"
;
;	ENTRY:	[See GPInt]
;
;	EXIT:	???
;
;	USES:	flags

Procedure   GPInByte
	inc	[bp].VGFB.vf_eip.LowWord ; Return address is after this instruction
	movzx	dx,ah			; (DX) = port number

entry	GPInDXByte

	DosContext <ds>,,noassume
	push	GSEL GDT_3xBoxPTDA
	pop	fs			; (fs) = 3xBox PTDA
	inc	[bp].VGFB.vf_eip.LowWord ; Return address is after this instruction
	mov	bx,dx			; (bx) = port address
	dtDisp	IN,gib2 		; Dispatch based on port address
gib1:	mov	byte ptr [bp].vgf_EAX,al ; Return AL
	ret				; Return to GPFault

gib2:	push	offset cs:gib1		; Put return address on stack
;;	jmp	short InUnexpected	; Fall through to InUnexpected
if2
.errnz	$-InUnexpected
endif

EndProc     GPInByte


;***	InUnexpected - Handle faulting IN that was not expected
;
;	ENTRY:	(dx) = original DX
;		(ds) = DosGroup
;		(es) = GDT_LinearSpace
;		(fs) = 3xBox PTDA
;		[See GPInt for stack frame]
;
;	EXIT:	(al) = physical or virtual contents of requested CRT register
;
;	USES:	flags

    assume  ds:DosGroup,es:nothing,fs:TaskArea,ss:nothing
Procedure   InUnexpected
	in	al,dx			; Do the operation
	ret				; Return to GPInByte/GPInDXByte
EndProc     InUnexpected


;***	GPOutByte - Handle faulting "OUT n,AL"
;***	GPOutDXByte - Handle faulting "OUT DX,AL"
;
;	ENTRY:	[See GPInt]
;
;	EXIT:	???
;
;	USES:	flags

Procedure   GPOutByte
	inc	[bp].VGFB.vf_eip.LowWord ; Return address is after this instruction
	movzx	dx,ah			; (DX) = port number

entry	GPOutDXByte

	DosContext <ds>,,noassume
	push	GSEL GDT_3xBoxPTDA
	pop	fs			; (fs) = 3xBox PTDA
	inc	[bp].VGFB.vf_eip.LowWord ; Return address is after this instruction
	mov	al,byte ptr [bp].vgf_EAX ; (al) = byte to output
	mov	bx,dx			; (bx) = port address
	dtDisp	OUT,OutUnexpected	; Dispatch based on port address
	ret				; Return to GPFault

;*  NOTE:  The use of dtDisp above is a speed hack.  If the port address is
;	   not in the specified range, then dtDisp JUMPs to OutUnexpected.
;	   OutUnexpected does the OUT, then RETs to the caller of GPOutByte!

EndProc     GPOutByte


;***	OutUnexpected - Handle faulting OUT that was not expected
;
;	ENTRY:	(al) = byte to output
;		(dx) = original DX
;		(ds) = DosGroup
;		(es) = GDT_LinearSpace
;		(fs) = 3xBox PTDA
;		[See GPInt for stack frame]
;
;	EXIT:	none
;
;	USES:	flags

    assume  ds:DosGroup,es:nothing,fs:TaskArea,ss:nothing
Procedure   OutUnexpected
	out	dx,al			; Do the operation
	ret				; Return to GPOutByte/GPOutDXByte
EndProc     OutUnexpected


;** OutFloppy - Handle faulting OUT to floppy controller
;
;	If READ command to floppy controller, reprogram the DMA controller
;	with the address of our temporary buffer.
;
;	ENTRY:	[See OutUnexpected]
;
;	EXIT:	Byte sent to floppy controller
;		If read request, DMA controller reprogrammed with correct
;		physical address.
;
;	USES:	AX,flags

    assume  ds:DosGroup,es:nothing,fs:TaskArea,ss:nothing
Procedure   OutFloppy
	test	[Direct_IO],-1
	jnz	short ofx		; The DMA controller had not been programmed

	mov	ax,[bp].vgf_EAX.LowWord ; Get original AX
	mov	ah,al			; Put command code into AH for testing
	and	ah,4FH			; Get command bits
	cmp	ah,FD_READ
	jnz	short ofx		; We are not READing off the disk

	SaveReg <dx>
	invoke	SaveDMAState		; Get the DMA registers
	invoke	SetNewDMAState		; Reprogram the DMA
	invoke	UserToTempBuf		; Copy user data to temp buffer
	inc	[Direct_IO]
	RestoreReg <dx>
ofx:	jmp	short OutUnexpected	; Do the OUT
EndProc     OutFloppy


;***	InCrtData - Handle faulting IN from CRT DATA register
;
;	ENTRY:	[See InUnexpected]
;
;	EXIT:	(al) = physical or virtual contents of requested CRT register
;
;	USES:	BX,flags

    assume  ds:DosGroup,es:nothing,fs:TaskArea,ss:nothing
Procedure   InCrtData
	test	V86BoxFG,-1		; Is this 3xBox foreground?
	jnz	short InUnexpected	;   Yes, do real IN

	movzx	bx,V86EgaIndexReg	; (bx) = current index register
	mov	al,V86EgaRegister[bx]	; (al) = virtual register contents
	ret				; Return to GPInByte/GPInDXByte
EndProc     InCrtData


;***	OutCrtIndex - Handle faulting OUT to CRT INDEX register
;
;	ENTRY:	[See OutFloppy]
;
;	EXIT:	V86EgaIndexReg updated
;		Physical CRT Index register updated if 3xBox is foreground
;
;	USES:	flags

    assume  ds:DosGroup,es:nothing,fs:TaskArea,ss:nothing
Procedure   OutCrtIndex
	mov	V86EgaIndexReg,al	; Store index register
	test	V86BoxFG,-1		; Is this 3xBox foreground?
	jnz	short OutUnexpected	;   Yes, do real OUT
	ret				; Return to GPOutByte/GPOutDXByte
EndProc     OutCrtIndex


;***	OutCrtData - Handle faulting OUT to CRT DATA register
;
;	ENTRY:	[See OutFloppy]
;
;	EXIT:	V86EgaRegisters[V86EgaIndexReg] updated
;		Physical CRT Data register updated if 3xBox is foreground
;
;	USES:	BX,flags

    assume  ds:DosGroup,es:nothing,fs:TaskArea,ss:nothing
Procedure   OutCrtData
	movzx	bx,V86EgaIndexReg	; (bx) = current index register
	mov	V86EgaRegister[bx],al	; Save virtual register contents
	test	V86BoxFG,-1		; Is this 3xBox foreground?
	jnz	short OutUnexpected	;   Yes, do real OUT
	ret				; Return to GPOutByte/GPOutDXByte
EndProc     OutCrtData


BREAK <V86Reflect - Reflect interrupt to V86 mode>
;***    V86Reflect - Reflect interrupt to V86 mode
;
;       ENTRY:  eax on stack
;               (ah) = interrupt number
;	EXIT:	[See V86GPInt]
;
;       USES:   none

    assume  ds:nothing,es:nothing,ss:nothing
Procedure   V86Reflect
        push    ebx                     ; 4 bytes
	push	edx			; 4 bytes
        push    bp                      ; 2 bytes
        mov     bp,sp
        mov     bx,GSEL GDT_LinearSpace
        mov     ds,bx                   ; (ds) = maps all of linear space
        mov     es,bx                   ; (es) = maps all of linear space
	push	offset CS:GPReturn	; Put return address (to V86GPFault)
	transfer  GPInt
EndProc     V86Reflect


BREAK <V86Trap0 - reflect trap 0 if from V86 mode>
;***    V86Trap0 - reflect trap 0 if from V86 mode
;
;       ENTRY: Interrupts OFF;
;              From trap00 handler in (trap286.asm)
;              (0) (old GS)
;              (0) (old FS)
;              (0) (old DS)
;              (0) (old ES)
;              (0) (old SS)
;                 (old ESP)
;              (old EFLAGS)
;              (0) (old CS)
;                 (old EIP)
;              (0)(errcode)
;                  (ret IP) <- (SS:SP) at entry from interrupt
;
;       EXIT:  IF fault was in V86 mode THEN
;                  Reflect
;              ELSE
;                  return to caller
;
;       USES:  flags

VT0A    equ     6                       ; Bytes on top of V86 frame

    assume  ds:nothing,es:nothing,ss:nothing

Procedure   V86Trap0,near

        PMONLY

        test    [esp].VT0A.vf_eflags.HighWord,f_VM ; Check VM bit in EFLAGS
        jnz     short vt01              ; A V86 fault

	ret				; Return to GP-Fault handler

vt01:   push    eax
        mov     ah,0                    ; (ah) = interrupt number
        transfer V86Reflect             ; Go reflect interrupt

EndProc     V86Trap0


BREAK <V86Trap4 - reflect trap 4 if from V86 mode>
;***    V86Trap4 - reflect trap 4 if from V86 mode
;
;       ENTRY: Interrupts OFF;
;              From trap04 handler in (trap286.asm)
;              (0) (old GS)
;              (0) (old FS)
;              (0) (old DS)
;              (0) (old ES)
;              (0) (old SS)
;                 (old ESP)
;              (old EFLAGS)
;              (0) (old CS)
;                 (old EIP)
;              (0)(errcode)
;                  (ret IP) <- (SS:SP) at entry from interrupt
;
;       EXIT:  IF fault was in V86 mode THEN
;                  Reflect
;              ELSE
;                  return to caller
;
;       USES:  flags

VT4A    equ     6                       ; Bytes on top of V86 frame

    assume  ds:nothing,es:nothing,ss:nothing

Procedure   V86Trap4,near

        PMONLY

        test    [esp].VT4A.vf_eflags.HighWord,f_VM ; Check VM bit in EFLAGS
        jz      short vt4x              ; Not a V86 fault

        push    eax
        mov     ah,4                    ; (ah) = interrupt number
        transfer V86Reflect             ; Go reflect interrupt

vt4x:	ret				; Return to GP-Fault handler

EndProc     V86Trap4


BREAK <V86Trap1 - reflect trap 1 if from V86 mode>
;***    V86Trap1 - reflect trap 1 if from V86 mode
;
;       ENTRY: Interrupts OFF;
;              From trap01 handler in (trap286.asm)
;              (0) (old GS)
;              (0) (old FS)
;              (0) (old DS)
;              (0) (old ES)
;              (0) (old SS)
;                 (old ESP)
;              (old EFLAGS)
;              (0) (old CS)
;                 (old EIP)
;                  (ret IP) <- (SS:SP) at entry from interrupt
;
;       EXIT:  IF fault was in V86 mode AND vector hooked by 3xBox app THEN
;                  Reflect
;              ELSE
;                  return to caller
;
;       USES:  flags

VT1A    equ     2                       ; Bytes on top of V86 frame

    assume  ds:nothing,es:nothing,ss:nothing

Procedure   V86Trap1,near

        PMONLY

        test    [esp].VT1A.vf_eflags.HighWord,f_VM ; Check VM bit in EFLAGS
        jz      short vt1x              ; Not a V86 fault

	sub	sp,VGFA-VT1A		; Adjust SP for GPInt
        push    eax
        mov     ax,(1 shl 8) + ((not (f_Trace shr 8)) and 00FFh)
                                        ; (ah) = interrupt number
                                        ; (al) = flags mask (turn off trace)
        transfer  V86TrapSpecial        ; Reflect or return to caller

vt1x:	ret				; Return to trap handler

EndProc     V86Trap1


BREAK <V86Trap3 - reflect trap 3 if from V86 mode>
;***    V86Trap3 - reflect trap 3 if from V86 mode
;
;       ENTRY: Interrupts OFF;
;              From trap03 handler in (trap286.asm)
;              (0) (old GS)
;              (0) (old FS)
;              (0) (old DS)
;              (0) (old ES)
;              (0) (old SS)
;                 (old ESP)
;              (old EFLAGS)
;              (0) (old CS)
;                 (old EIP)
;                  (ret IP) <- (SS:SP) at entry from interrupt
;
;       EXIT:  IF fault was in V86 mode AND vector hooked by 3xBox app THEN
;                  Reflect
;              ELSE
;                  return to caller
;
;       USES:  flags

VT3A    equ     2                       ; Bytes on top of V86 frame

    assume  ds:nothing,es:nothing,ss:nothing

Procedure   V86Trap3,near

        PMONLY

        test    [esp].VT3A.vf_eflags.HighWord,f_VM ; Check VM bit in EFLAGS
        jz      short vt3x              ; Not a V86 fault

	sub	sp,VGFA-VT1A		; Adjust SP for GPInt
        push    eax
        mov     ax,(3 shl 8) + 0FFh     ; (ah) = interrupt number
                                        ; (al) = flags mask (no change)
        transfer  V86TrapSpecial        ; Reflect or return to caller

vt3x:	ret				; Return to trap handler

EndProc     V86Trap3


BREAK <V86TrapSpecial - reflect trap if for 3xBox>
;***    V86TrapSpecial - reflect trap if for 3xBox
;
;       For Trap 1 (trace) and Trap 3 (break point), we need to dispatch
;       to the 3xBox if a 3xBox application has hooked the vector.  Otherwise,
;       we want to give control to the normal handler.  This allows us to
;       to use the kernel debugger AND symdeb concurrently.  Furthermore,
;       some copy-protected applications (Lotus 1-2-3) plays games with int 3
;       to make it harder for someone to use a debugger to break the copy-
;       protection.
;
;       ENTRY: (ah) = trap (interrupt) number;
;              (al) = mask to AND with byte 1 (3210) of EFLAGS;
;              Caller already determined that Trap was from V86 mode;
;              Interrupts OFF;
;              (0) (old GS)
;              (0) (old FS)
;              (0) (old DS)
;              (0) (old ES)
;              (0) (old SS)
;                 (old ESP)
;              (old EFLAGS)
;              (0) (old CS)
;                 (old EIP)
;                  (ret IP) <- call from trap0x in trap286.asm
;		  (padding) <- adjusts frame for GPInt
;                     (EAX) <- (SS:SP) at entry from V86Trap1/V86Trap3
;
;       EXIT:  IF vector hooked by 3xBox app THEN
;                  Reflect
;              ELSE
;                  return to caller
;
;       USES:  flags

VTSA    equ     2                       ; Bytes on top of V86 frame
.errnz  VTSA-VT1A
.errnz  VTSA-VT3A

    assume  ds:nothing,es:nothing,ss:nothing

Procedure   V86TrapSpecial

;*  Set up stack for transfer to GPInt

        push    ebx                     ; 4 bytes
	push	edx			; 4 bytes
        push    bp                      ; 2 bytes
        mov     bp,sp
        mov     bx,GSEL GDT_LinearSpace
        mov     ds,bx                   ; (ds) = maps all of linear space
        mov     es,bx                   ; (es) = maps all of linear space

;*  Check if 3xBox has vector hooked

        movzx   bx,ah                   ; (bx) = interrupt number
        shl     bx,2                    ; (ds:bx) = interrupt handler
        mov     bx,ds:[bx].Segmt        ; (bx) = segment of interrupt handler
        DosContext <fs>
        cmp     bx,Seg3xBoxPTDA         ; Compare with low end of 3xBox
        jb      short vts1              ; Not hooked by 3xBox

        cmp     bx,SEG_END_3XBOX        ; Compare with high end of 3xBox
        ja      short vts1              ; Not hooked by 3xBox

        and     byte ptr [bp].vf_eflags.LowWord[1],al ; Adjust flags

	push	offset CS:GPReturn	; Put return address (to V86GPFault)
	transfer  GPInt     ; Reflect trap

vts1:   pop     bp                      ; Restore registers
	pop	edx
        pop     ebx
        pop     eax
	add	sp,VGFA-VTSA		; Remove GPInt adjustment
	ret

EndProc     V86TrapSpecial


BREAK <V86PageFault - check for page fault from V86 mode>
;***    V86PageFault - check for page fault from V86 mode
;
;       ENTRY: Interrupts OFF;
;              From trap_0e handler in (trap286.asm)
;              (0) (old GS)
;              (0) (old FS)
;              (0) (old DS)
;              (0) (old ES)
;              (0) (old SS)
;                 (old ESP)
;              (old EFLAGS)
;              (0) (old CS)
;                 (old EIP)
;              (0)(errcode)
;                  (ret IP) <- (SS:SP) at entry from interrupt
;
;       EXIT:  IF fault was in V86 mode THEN
;                  InternalError
;              ELSE
;                  return to caller
;
;       USES:  flags

VPFA    equ     6                       ; Bytes on top of V86 frame

    assume  ds:nothing,es:nothing,ss:nothing

Procedure   V86PageFault,near

        PMONLY

        test    [esp].VPFA.vf_eflags.HighWord,f_VM ; Check VM bit in EFLAGS
        jnz     short vpfie             ; A V86 page fault

	ret				; Return to GP-Fault handler

vpfie:  InternalError <V86PageFault: Page Fault in V86 mode>

EndProc     V86PageFault

HIGH2CODE ends


        FARCODE INIT
INITCODE segment

BREAK <V86LoadallGdtIdt - Setup IDT and GDT data in 386 loadall buffer>
;***    V86LoadallGdtIdt - Setup IDT and GDT data in 386 loadall buffer
;
;       ENTRY:  none
;
;       EXIT:   none
;
;       USES:   CX,DX

    assume ds:nothing,es:nothing,ss:nothing

Procedure   V86LoadallGdtIdt

        PMONLY

        SaveReg <ds>
        SaveReg <eax,edi,esi>
        DosContext <ds>
        movzx   edi,Loadall386Seg       ; (esi) = RM segment of loadall buffer
        shl     edi,4                   ; (esi) = offset of loadall buffer
        mov     ax,GSEL GDT_LinearSpace
        mov     ds,ax                   ; (ds:esi) = start of cache entry
    assume  ds:nothing

        mov     si,ll_IDTcache
        sidt    qword ptr ds:[edi].ll_IDTcache  ; store IDT base and limit
        call    V86EditLoadallCache             ; Modify loadall buffer

        mov     si,ll_GDTcache
        sgdt    qword ptr ds:[edi].ll_GDTcache  ; store GDT base and limit
        call    V86EditLoadallCache             ; Modify loadall buffer

        RestoreReg <esi,edi,eax>
        RestoreReg <ds>
	ret

EndProc     V86LoadallGdtIdt


BREAK <V86EditLoadallCache - Edit cache entry in 386 Loadall buffer>
;***    V86EditLoadallCache - Edit cache entry in 386 Loadall buffer
;
;       ENTRY:  (si) = offset of cache entry in 386 loadall buffer, filled with
;                      data from SGDT/SIDT.
;               (ds:edi) = 386 loadall buffer
;
;       EXIT:   Cache entry reformatted for loadall.
;
;       USES:   AX,CX,DX,ESI
;
;       WARNING: THIS ROUTINE ONLY SUPPORTS PHYSICAL ADDRESSES OF 24 BITS!

    assume  ds:nothing,es:nothing,ss:nothing

Procedure   V86EditLoadallCache,near,local

        PMONLY

        movzx   esi,si                  ; (esi) = offset in loadall buffer
        add     esi,edi                 ; (ds:esi) = loadall cache entry
        mov     cx,ds:[esi].d_limit     ; (cx) = limit
        mov     al,ds:[esi].d_hiaddr    ; (al) = high 8 bits of base
        mov     dx,ds:[esi].d_loaddr    ; (dx) = low 16 bits of base
        mov     ds:[esi].dc_LIMITlo,cx  ; Set limit in loadall buffer
        mov     ds:[esi].dc_BASElo,dx   ; Set low 16 bits of base
        xor     ah,ah                   ; (ax) = high 16 bits of base
        mov     ds:[esi].dc_BASEhi,ax   ; Set high 16 bits of base
        xor     eax,eax                 ; (eax) = 0
        mov     dword ptr ds:[esi].dc_AR1,eax ; Clear AR cache entry
        mov     ds:[esi].dc_AR2,D_DATA3 ; Set access rights
	ret

EndProc     V86EditLoadallCache

INITCODE ends

        END
hread ID

