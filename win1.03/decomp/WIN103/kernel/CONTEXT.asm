

;
; External Entry #30 into the Module
; Attributes (0001): Fixed Exported
;
; [0000003E BYTES: COLLAPSED FUNCTION WAITEVENT. PRESS CTRL-NUMPAD+ TO EXPAND]
;
; External Entry #29 into the Module
; Attributes (0001): Fixed Exported
;

; =============== S U B R O U T I N E =======================================

; Attributes: bp-based frame

                public YIELD
YIELD           proc far                ; CODE XREF: STARTTASK+69↑p
                inc     bp              ; KERNEL_29
                push    bp
                mov     bp, sp
                push    ds
                xor     ax, ax
                cmp     cs:INSCHEDULER, al
                jnz     short loc_3C8C
                mov     ds, cs:CURTDB   ; get the current task data block
                cmp     word ptr ds:7Eh, 4454h ; is 0x7E in the TDB 'MZ' header?
                jnz     short not_task_handle
                mov     ax, 3C85h
                inc     word ptr ds:6
                push    cs
                push    ax
                jmp     near ptr RESCHEDULE
; ---------------------------------------------------------------------------

not_task_handle:                        ; CODE XREF: YIELD+19↑j
                mov     ax, 301h
                push    ax
                mov     ax, offset SZERRYIELDINVALIDTASKHANDLE ; "YIELD: Invalid task handle"
                push    cs
                push    ax
                xor     ax, ax
                push    ax
                push    ax
                call    KERNELERROR
                jmp     short loc_3C85
; ---------------------------------------------------------------------------
SZERRYIELDINVALIDTASKHANDLE db 'YIELD: Invalid task handle',0
                                        ; DATA XREF: YIELD+2B↑o
                db 24h
; ---------------------------------------------------------------------------

loc_3C85:                               ; CODE XREF: YIELD+37↑j
                dec     word ptr ds:6
                mov     ax, 0FFFFh

loc_3C8C:                               ; CODE XREF: YIELD+C↑j
                sub     bp, 2
                mov     sp, bp
                pop     ds
                pop     bp
                dec     bp
                retf
YIELD           endp ; sp-analysis failed


; =============== S U B R O U T I N E =======================================


GETTASKHANDLE_VARIANT_UNDOCUMENTED proc near ; CODE XREF: SETTASKQUEUE↓p
                                        ; SETPRIORITY↓p
                mov     bx, sp
                mov     ax, ss:[bx+8]
                mov     bx, ss:[bx+6]
                jmp     short GETTASKHANDLE
GETTASKHANDLE_VARIANT_UNDOCUMENTED endp


; =============== S U B R O U T I N E =======================================


GETTASKHANDLE_VARIANT_UNDOCUMENTED_2 proc near ; CODE XREF: POSTEVENT↓p
                                        ; GETTASKQUEUE↓p
                mov     bx, sp
                mov     ax, ss:[bx+6]
GETTASKHANDLE_VARIANT_UNDOCUMENTED_2 endp


; =============== S U B R O U T I N E =======================================


GETTASKHANDLE   proc near               ; CODE XREF: WAITEVENT+8↑p
                                        ; GETTASKHANDLE_VARIANT_UNDOCUMENTED+A↑j ...
                or      ax, ax
                jnz     short loc_3CAF
                mov     ax, cs:CURTDB

loc_3CAF:                               ; CODE XREF: GETTASKHANDLE+2↑j
                mov     es, ax
                cmp     word ptr es:7Eh, 4454h
                jnz     short loc_3CBB
                retn
; ---------------------------------------------------------------------------

loc_3CBB:                               ; CODE XREF: GETTASKHANDLE+11↑j
                mov     ax, 301h
                push    ax
                mov     ax, offset SZERRINVALIDTASKHANDLE ; "GetTaskHandle: Invalid task handle"
                push    cs
                push    ax
                xor     ax, ax
                push    ax
                push    ax
                call    KERNELERROR
                jmp     short locret_3CF1
; ---------------------------------------------------------------------------
SZERRINVALIDTASKHANDLE db 'GetTaskHandle: Invalid task handle',0
                                        ; DATA XREF: GETTASKHANDLE+18↑o
                db 24h
; ---------------------------------------------------------------------------

locret_3CF1:                            ; CODE XREF: GETTASKHANDLE+24↑j
                retn
GETTASKHANDLE   endp

;
; External Entry #31 into the Module
; Attributes (0001): Fixed Exported
;

; =============== S U B R O U T I N E =======================================


                public POSTEVENT
POSTEVENT       proc far
                call    GETTASKHANDLE_VARIANT_UNDOCUMENTED_2 ; KERNEL_31
                inc     word ptr es:6
                retf    2
POSTEVENT       endp

;
; External Entry #35 into the Module
; Attributes (0001): Fixed Exported
;

; =============== S U B R O U T I N E =======================================


                public GETTASKQUEUE
GETTASKQUEUE    proc far
                call    GETTASKHANDLE_VARIANT_UNDOCUMENTED_2 ; KERNEL_35
                mov     ax, es:12h
                retf    2
GETTASKQUEUE    endp

; ---------------------------------------------------------------------------
;
; External Entry #38 into the Module
; Attributes (0001): Fixed Exported
;

                public SETTASKSIGNALPROC
SETTASKSIGNALPROC:
                mov     cl, 14h
; ---------------------------------------------------------------------------
                db 0BBh
; ---------------------------------------------------------------------------
;
; External Entry #40 into the Module
; Attributes (0001): Fixed Exported
;

                public SETTASKINTERCHANGE
SETTASKINTERCHANGE:
                mov     cl, 1Ch
; ---------------------------------------------------------------------------
                db 0BBh
;
; External Entry #39 into the Module
; Attributes (0001): Fixed Exported
;

; =============== S U B R O U T I N E =======================================


                public SETTASKSWITCHPROC
SETTASKSWITCHPROC proc far
                mov     cl, 18h
                mov     bx, sp
                mov     ax, ss:[bx+8]
                call    GETTASKHANDLE
                mov     ax, ss:[bx+4]
                mov     dx, ss:[bx+6]
                xor     bx, bx
                mov     bl, cl
                xchg    ax, es:[bx]
                xchg    dx, es:[bx+2]
                retf    6
SETTASKSWITCHPROC endp

;
; External Entry #34 into the Module
; Attributes (0001): Fixed Exported
;

; =============== S U B R O U T I N E =======================================


                public SETTASKQUEUE
SETTASKQUEUE    proc far
                call    GETTASKHANDLE_VARIANT_UNDOCUMENTED ; KERNEL_34
                mov     ax, bx
                xchg    ax, es:12h
                retf    4
SETTASKQUEUE    endp

;
; External Entry #32 into the Module
; Attributes (0001): Fixed Exported
;

; =============== S U B R O U T I N E =======================================


; Function Name: SetPriority(hTask, nChangeAmount)
;
; Purpose: Modifies the priority of task hTask (pointer to which is represented by the hTask handle) by amount nChangeAmount.
; Priority must be between -15 and 15. Yes, this is a signed value.
;
; Notes: Incredibly hacky. They delete the task and re-create it. Wtf?

                public SETPRIORITY
SETPRIORITY     proc far
                call    GETTASKHANDLE_VARIANT_UNDOCUMENTED ; get the task from hTask, this will fatalexit if the task is wrong
                add     bl, es:8                    ; bl = old task priority, es:0008 = nChangeAmount
                cmp     bl, 0E0h                    ; is new priority above or equal to -15?
                jge     short check_priority_high   ; branch   
                mov     bl, 0E0h                    ; set it to -15 if it's below -15 

check_priority_high:                                ; CODE XREF: SETPRIORITY+B↑j
                cmp     bl, 0Fh                     ; Is priority above 15?
                jle     short set_priority          ; no, branch
                mov     bl, 0Fh                     ; now, go

set_priority:                               ; CODE XREF: SETPRIORITY+12↑j
                push    bx                          ; bx contains task priority
                inc     bx                          ; increment by 1
                mov     es:8, bl                    ; nChangeAmount -> new pri 
                push    es                          ; push ?????? 
                push    es                          ; push task (why are there two pushes here)
                call    DELETETASK                  ; delete the entire god damn task
                push    ax                          ; ax->new process priority for inserttask call
                call    INSERTTASK                  ; recreate the task
                pop     es                          ; get new task 
                dec     byte ptr es:8               ; decrement nChangeAmount by 1 for some reason (we incremented it ealrier)
                pop     ax                          ; return value from INSERTTASK 
                cbw                                 ; sign extend it
                retf    4                           ; ok
SETPRIORITY     endp

; =============== S U B R O U T I N E =======================================
; context.asm?
; or maybe ld.asm

                public GETVERSION
GETVERSION      proc far                ; CODE XREF: RETTHUNK+6B↓p
                mov     ax, 301h        ; 0103h = Windows 1.03
                retf
GETVERSION      endp
