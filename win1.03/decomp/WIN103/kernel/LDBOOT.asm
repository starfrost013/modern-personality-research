; ---------------------------------------------------------------------------
HINITMEM        dw 0                    ; DATA XREF: BOOTSTRAP+CC↓w
                                        ; BOOTSTRAP:loc_8271↓r ...
SEGINITMEM      dw 0                    ; DATA XREF: BOOTSTRAP+25↓w
                                        ; BOOTSTRAP+B0↓r ...
CPSHRUNK        dw 0                    ; DATA XREF: BOOTSTRAP+17↓w
                                        ; FINDFREESEG+91↓w ...
LPBOOTAPP       dd 0                    ; DATA XREF: SLOWBOOT+17↓w
                                        ; SLOWBOOT:loc_8412↓w ...
BOOTEXECBLOCK   db 0Eh dup(0)           ; DATA XREF: SLOWBOOT+29↓w
                                        ; SLOWBOOT+41↓w ...
WIN_SHOW        db    2                 ; DATA XREF: SLOWBOOT↓o
                                        ; FASTBOOT+3FD↓o
                db    0
                db    1
                db    0
SZKERNELNAME    db 'KERNEL',0
SZENABLEHEAPCHECKING db 'EnableHeapChecking',0
SZFREEZEGLOBALMOTION db 'FreezeGlobalMotion',0
SZLRUSWEEPFREQUENCY db 'LRUSweepFrequency',0
word_7F8C       dw 0                    ; DATA XREF: BOOTSTRAP+39↓w
                                        ; BOOTSTRAP+1B2↓r

; =============== S U B R O U T I N E =======================================


BOOTDONE        proc far                ; CODE XREF: SLOWBOOT+A4↓j
                mov     ax, 7F4Dh
                mov     bx, 7F54h
                xor     cx, cx
                push    cs
                push    ax
                push    cs
                push    bx
                push    cx
                nop
                push    cs
                call    near ptr GETPROFILEINT
                mov     es, cs:PGLOBALHEAP
                assume es:nothing
                mov     es:0, ax
                mov     ax, 7F4Dh
                mov     bx, 7F67h
                xor     cx, cx
                push    cs
                push    ax
                push    cs
                push    bx
                push    cx
                nop
                push    cs
                call    near ptr GETPROFILEINT
                mov     es, cs:PGLOBALHEAP
                mov     es:2, ax
                mov     ax, 7F4Dh
                mov     bx, 7F7Ah
                mov     cx, 0FAh
                push    cs
                push    ax
                push    cs
                push    bx
                push    cx
                nop
                push    cs
                call    near ptr GETPROFILEINT
                or      ax, ax
                jz      short loc_7FE6
                mov     bx, 6922h
                push    ax
                push    cs
                push    bx
                call    cs:PTIMERPROC

loc_7FE6:                               ; CODE XREF: BOOTDONE+4B↑j
                nop
                push    cs
                call    near ptr VALIDATECODESEGMENTS
                push    ss
                call    DELETETASK
                xor     dx, dx
                mov     ss:7Eh, dx
                mov     cs:CURTDB, dx
                mov     cx, 7F31h
                push    cs
                push    dx
                push    cx
                push    dx
                push    cs
                mov     ax, 3B0Ch
                push    ax
                jmp     near ptr GLOBALREALLOC
BOOTDONE        endp

                assume ss:cseg01

; =============== S U B R O U T I N E =======================================


                public BOOTSTRAP
BOOTSTRAP       proc far
                mov     cs:TOPPDB, es
                mov     cs:HEADPDB, es
                mov     word ptr es:42h, 0
                mov     ax, cx
                mov     cl, 4
                shr     ax, cl
                mov     cs:CPSHRUNK, ax
                mov     bx, sp
                mov     cl, 4
                shr     bx, cl
                mov     ax, ss
                add     ax, bx
                mov     cs:SEGINITMEM, ax
                mov     ax, cs
                mov     bx, 93F0h
                mov     si, 9670h
                sub     si, bx
                mov     cl, 4
                shr     bx, cl
                add     ax, bx
                mov     cs:word_7F8C, bx
                cli
                mov     ss, ax
                assume ss:nothing
                mov     sp, si
                sti
                xor     bp, bp
                mov     ss:word_93FE, si
                mov     ss:word_93FC, sp
                sub     si, 200h
                mov     ss:word_93FA, si
                mov     ax, es:0FEh
                cmp     ax, 5758h
                jz      short loc_8076
                cmp     ax, 5747h
                jnz     short loc_8081
                inc     cs:FWINX

loc_8076:                               ; CODE XREF: BOOTSTRAP+60↑j
                inc     cs:FWINX
                jmp     short loc_80AF
; ---------------------------------------------------------------------------
                align 2

loc_807E:                               ; CODE XREF: BOOTSTRAP+CA↓j
                jmp     loc_8296
; ---------------------------------------------------------------------------

loc_8081:                               ; CODE XREF: BOOTSTRAP+65↑j
                push    ds
                xor     ax, ax
                cld
                mov     es, cs:TOPPDB
                mov     si, 80h
                lods    byte ptr es:[si]
                add     si, ax
                lea     di, [si+1]
                push    cs
                pop     ds
                assume ds:cseg01
                mov     cx, 8296h
                mov     si, 828Bh
                sub     cx, si
                mov     ax, cx
                add     al, 2
                stosb
                mov     ah, 19h
                int     21h             ; DOS - GET DEFAULT DISK NUMBER
                add     al, 41h ; 'A'
                mov     ah, 3Ah ; ':'
                stosw
                rep movsb
                pop     ds
                assume ds:nothing

loc_80AF:                               ; CODE XREF: BOOTSTRAP+71↑j
                cld
                mov     es, cs:TOPPDB
                mov     ax, es
                add     ax, 10h
                mov     bx, cs:SEGINITMEM
                mov     cx, es:2
                mov     es, bx
                mov     dx, es:10h
                push    dx
                push    bx
                push    ax
                push    cx
                nop
                push    cs
                call    near ptr GLOBALINIT
                jcxz    short loc_807E
                mov     cs:HINITMEM, ax
                call    DEBUGINIT
                call    INITDOSVARP
                mov     bx, 3FC5h
                mov     word ptr cs:PEXITPROC, bx
                mov     word ptr cs:PEXITPROC+2, cs
                push    ds
                mov     ax, cs
                mov     ds, ax
                assume ds:cseg01
                mov     si, 1Eh
                fninit
                fnstcw  word ptr [si]
                jmp     short $+2
; ---------------------------------------------------------------------------

loc_80FB:                               ; CODE XREF: BOOTSTRAP+EF↑j
                mov     ax, [si]
                and     ax, 37Fh
                cmp     ax, 37Fh
                jz      short loc_8109
                mov     word ptr [si], 0

loc_8109:                               ; CODE XREF: BOOTSTRAP+F9↑j
                pop     ds
                assume ds:nothing
                mov     es, cs:TOPPDB
                mov     si, 80h
                xor     ax, ax
                lods    byte ptr es:[si]
                add     si, ax
                inc     si
                lods    byte ptr es:[si]
                add     si, ax
                mov     word ptr es:[si-4], 564Fh
                mov     byte ptr es:[si-2], 4Ch ; 'L'
                sub     si, ax
                sub     sp, 80h
                mov     di, sp
                mov     bx, 4000h
                push    es
                push    es
                push    si
                push    ss
                push    di
                push    bx
                nop
                push    cs
                call    near ptr OPENFILE
                inc     ax
                pop     es
                jnz     short loc_815A
                inc     byte ptr es:[si]
                mov     bx, 4000h
                push    es
                push    es
                push    si
                push    ss
                push    di
                push    bx
                nop
                push    cs
                call    near ptr OPENFILE
                pop     es
                inc     ax
                jnz     short loc_815A

loc_8157:                               ; CODE XREF: BOOTSTRAP+166↓j
                jmp     loc_8296
; ---------------------------------------------------------------------------

loc_815A:                               ; CODE XREF: BOOTSTRAP+136↑j
                                        ; BOOTSTRAP+14B↑j
                mov     di, sp
                mov     ax, 0FFFFh
                push    cs:SEGINITMEM
                push    ax
                push    ss
                push    di
                call    LOADEXEHEADER
                add     sp, 80h
                or      ax, ax
                jz      short loc_8157
                mov     es, ax
                mov     cs:HEXEHEAD, ax
                mov     si, es:22h
                add     word ptr es:[si+6], 800h
                inc     word ptr es:2
                push    es
                call    ALLOCALLSEGS
                mov     es, cs:HEXEHEAD
                mov     si, 1
                mov     ax, 0FFFFh
                push    cs:HEXEHEAD
                push    si
                push    cs
                push    ax
                call    LOADSEGMENT
                or      ax, ax
                jnz     short loc_81A9
                jmp     loc_8296
; ---------------------------------------------------------------------------

loc_81A9:                               ; CODE XREF: BOOTSTRAP+19A↑j
                mov     cx, 93F0h
                mov     si, 9670h
                sub     si, cx
                add     si, 800h
                mov     es, cs:HEXEHEAD
                mov     bx, ax
                add     bx, cs:word_7F8C
                test    bl, 1
                jnz     short loc_81C7
                dec     bx

loc_81C7:                               ; CODE XREF: BOOTSTRAP+1BA↑j
                cli
                mov     ss, bx
                assume ss:nothing
                mov     sp, si
                sti
                push    ax
                mov     es, bx
                xor     di, di
                mov     cx, 80h
                xor     ax, ax
                cld
                rep stosb
                pop     ax
                xor     bp, bp
                mov     ss:0Eh, si
                mov     ss:0Ch, sp
                sub     si, 0A00h
                mov     ss:0Ah, si
                sub     sp, 16h
                mov     word ptr ss:4, ss
                mov     ss:2, sp
                mov     cx, cs:TOPPDB
                mov     ss:32h, cx
                mov     word ptr ss:34h, 80h
                mov     ss:36h, cx
                mov     word ptr ss:6, 1
                mov     word ptr ss:7Eh, 4454h
                les     bx, ss:2
                mov     es:[bx+10h], bp
                mov     es:[bx+0Eh], bp
                push    ax
                mov     ax, 8234h
                push    ax
                retf
; ---------------------------------------------------------------------------
                push    ss
                call    SAVESTATE
                mov     cs:CURTDB, ss
                mov     cs:HEADTDB, ss
                xor     ax, ax
                mov     es, ax
                assume es:cseg01
                mov     bx, 0FCh
                mov     ax, 180Ah
                xchg    ax, es:[bx]
                mov     word ptr cs:PREVINT3FPROC, ax
                mov     ax, cs
                xchg    ax, es:[bx+2]
                mov     word ptr cs:PREVINT3FPROC+2, ax
                mov     es, cs:SEGINITMEM
                assume es:nothing
                cmp     word ptr es:0Ah, 0
                jnz     short loc_8271
                mov     ax, 7F8Eh
                push    ax
                jmp     FASTBOOT
; ---------------------------------------------------------------------------

loc_8271:                               ; CODE XREF: BOOTSTRAP+25E↑j
                push    cs:HINITMEM
                nop
                push    cs
                call    near ptr GLOBALFREE
                mov     cs:HINITMEM, ax
                push    cs:TOPPDB
                mov     ax, 83DDh
                push    ax
                jmp     INITLOADER
; ---------------------------------------------------------------------------
SZWINPACKFILE   db 'WIN100.BIN',0
; ---------------------------------------------------------------------------

loc_8296:                               ; CODE XREF: BOOTSTRAP:loc_807E↑j
                                        ; BOOTSTRAP:loc_8157↑j ...
                mov     al, 1
                push    ax
                nop
                push    cs
                call    EXITKERNEL
                push    bx
                pop     cx
                push    bx
                push    sp
                inc     bp
                dec     bp
                db      2Eh
                inc     sp
                push    dx
                push    si
                add     [bp+di+45h], cl
                pop     cx
                inc     dx
                dec     di
                inc     cx
                push    dx
                inc     sp
                db      2Eh
                inc     sp
                push    dx
                push    si
                add     [di+4Fh], cl
                push    bp
                push    bx
                inc     bp
                db      2Eh
                inc     sp
                push    dx
                push    si
                add     [si+49h], al
                push    bx
                push    ax
                dec     sp
                inc     cx
                pop     cx
                db      2Eh
                inc     sp
                push    dx
                push    si
                add     [bp+di+4Fh], dl
                push    bp
                dec     si
                inc     sp
                db      2Eh
                inc     sp
                push    dx
                push    si
                add     [bp+di+4Fh], al
                dec     bp
                dec     bp
                db      2Eh
                inc     sp
                push    dx
                push    si
                add     [bp+4Fh], al
                dec     si
                push    sp
                push    bx
                db      2Eh
                inc     si
                dec     di
                dec     si
                add     [bx+44h], al
                dec     cx
                db      2Eh
                inc     bp
                pop     ax
                inc     bp
                add     [di+53h], dl
                inc     bp
                push    dx
                db      2Eh
                inc     bp
                pop     ax
                inc     bp
                add     [di+53h], cl
                inc     sp
                dec     di
                push    bx
                inc     sp
                db      2Eh
                inc     bp
                pop     ax
                inc     bp

loc_8304:                               ; DATA XREF: SLOWBOOT:loc_8412↓o
                add     [di+53h], cl
                inc     sp
                dec     di
                push    bx
                db      2Eh
                inc     bp
                pop     ax
                inc     bp
; ---------------------------------------------------------------------------
                db 0
; ---------------------------------------------------------------------------

loc_830F:                               ; CODE XREF: SLOWBOOT+51↓p
                                        ; SLOWBOOT+73↓p
                push    bp
                mov     bp, sp
                push    si
                push    di
                xor     ax, ax
                push    cs
                push    word ptr [bp+4]
                push    ax
                push    ax
                nop
                push    cs
                call    near ptr LOADMODULE
                cmp     ax, 2
                jnz     short loc_8360
                cmp     word ptr [bp+4], 82FAh
                jnb     short loc_8396
                mov     ax, 401h
                push    ax
                mov     ax, offset SZBOOTCANNOTFINDFILE ; "BOOT: Unable to find file - "
                push    cs
                push    ax
                push    cs
                push    word ptr [bp+4]
                call    KERNELERROR
                jmp     short loc_835D
; ---------------------------------------------------------------------------
SZBOOTCANNOTFINDFILE db 'BOOT: Unable to find file - ',0
                                        ; DATA XREF: BOOTSTRAP+327↑o
                db 24h
; ---------------------------------------------------------------------------

loc_835D:                               ; CODE XREF: BOOTSTRAP+333↑j
                jmp     short loc_83C5
; ---------------------------------------------------------------------------
                align 2

loc_8360:                               ; CODE XREF: BOOTSTRAP+31A↑j
                cmp     ax, 0Bh
                jnz     short loc_8396
                mov     ax, 401h
                push    ax
                mov     ax, offset SZBOOTINVALIDEXE ; "BOOT: Invalid .EXE file - "
                push    cs
                push    ax
                push    cs
                push    word ptr [bp+4]
                call    KERNELERROR
                jmp     short loc_8393
; ---------------------------------------------------------------------------
SZBOOTINVALIDEXE db 'BOOT: Invalid .EXE file - ',0
                                        ; DATA XREF: BOOTSTRAP+35F↑o
                db 24h
; ---------------------------------------------------------------------------

loc_8393:                               ; CODE XREF: BOOTSTRAP+36B↑j
                jmp     short loc_83C5
; ---------------------------------------------------------------------------
                align 2

loc_8396:                               ; CODE XREF: BOOTSTRAP+321↑j
                                        ; BOOTSTRAP+359↑j
                or      ax, ax
                jnz     short loc_83D5
                mov     ax, 401h
                push    ax
                mov     ax, offset SZBOOTCANNOTLOAD ; "BOOT: Unable to load - "
                push    cs
                push    ax
                push    cs
                push    word ptr [bp+4]
                call    KERNELERROR
                jmp     short loc_83C5
; ---------------------------------------------------------------------------
SZBOOTCANNOTLOAD db 'BOOT: Unable to load - ',0
                                        ; DATA XREF: BOOTSTRAP+394↑o
                db 24h
; ---------------------------------------------------------------------------

loc_83C5:                               ; CODE XREF: BOOTSTRAP:loc_835D↑j
                                        ; BOOTSTRAP:loc_8393↑j ...
                cmp     word ptr [bp+4], 82FAh
                jnb     short loc_83D5
                mov     ax, 1
                push    ax
                nop
                push    cs
                call    EXITKERNEL

loc_83D5:                               ; CODE XREF: BOOTSTRAP+38E↑j
                                        ; BOOTSTRAP+3C0↑j
                pop     di
                pop     si
                mov     sp, bp
                pop     bp
                retn    2
BOOTSTRAP       endp ; sp-analysis failed


; =============== S U B R O U T I N E =======================================


SLOWBOOT        proc far
                mov     word ptr cs:BOOTEXECBLOCK+6, offset WIN_SHOW
                mov     word ptr cs:BOOTEXECBLOCK+8, cs
                mov     es, cs:TOPPDB
                or      ax, ax
                jz      short loc_8412
                mov     di, ax
                mov     word ptr cs:LPBOOTAPP, di
                mov     word ptr cs:LPBOOTAPP+2, es
                xor     ax, ax
                mov     cx, 0FFFFh
                cld
                repne scasb
                mov     word ptr cs:BOOTEXECBLOCK+2, di
                mov     word ptr cs:BOOTEXECBLOCK+4, es
                jmp     short loc_842A
; ---------------------------------------------------------------------------

loc_8412:                               ; CODE XREF: SLOWBOOT+13↑j
                mov     word ptr cs:LPBOOTAPP, (offset loc_8304+1)
                mov     word ptr cs:LPBOOTAPP+2, cs
                mov     word ptr cs:BOOTEXECBLOCK+2, 80h
                mov     word ptr cs:BOOTEXECBLOCK+4, es

loc_842A:                               ; CODE XREF: SLOWBOOT+33↑j
                mov     di, 829Eh

loc_842D:                               ; CODE XREF: SLOWBOOT+62↓j
                push    di
                call    loc_830F
                push    cs
                pop     es
                assume es:cseg01
                mov     cx, 0FFFFh
                xor     ax, ax
                cld
                repne scasb
                cmp     di, 82F1h
                jb      short loc_842D
                cmp     word ptr cs:BOOTEXECBLOCK+2, 80h
                jz      short loc_844F
                call    near ptr INITFWDREF
                jmp     short loc_8469
; ---------------------------------------------------------------------------

loc_844F:                               ; CODE XREF: SLOWBOOT+6B↑j
                                        ; SLOWBOOT+84↓j
                push    di
                call    loc_830F
                push    cs
                pop     es
                mov     cx, 0FFFFh
                xor     ax, ax
                cld
                repne scasb
                cmp     di, 8305h
                jb      short loc_844F
                call    near ptr INITFWDREF
                call    ENABLEINT21

loc_8469:                               ; CODE XREF: SLOWBOOT+70↑j
                mov     ax, 7F3Bh
                push    word ptr cs:LPBOOTAPP+2
                push    word ptr cs:LPBOOTAPP
                push    cs
                push    ax
                nop
                push    cs
                call    near ptr LOADMODULE
                or      ax, ax
                jz      short loc_8484
                jmp     near ptr BOOTDONE
; ---------------------------------------------------------------------------

loc_8484:                               ; CODE XREF: SLOWBOOT+A2↑j
                mov     ax, 401h
                push    ax
                mov     ax, offset SZBOOTCANNOTLOADFILE ; "BOOT: unable to load - \x00$"
                push    cs
                push    ax
                push    word ptr cs:LPBOOTAPP+2
                push    word ptr cs:LPBOOTAPP
                call    KERNELERROR
                jmp     short near ptr LPRETURNONSLOWBOOTERROR
SLOWBOOT        endp ; sp-analysis failed

; ---------------------------------------------------------------------------
SZBOOTCANNOTLOADFILE db 'BOOT: unable to load - ',0,'$'
                                        ; DATA XREF: SLOWBOOT+AB↑o
LPRETURNONSLOWBOOTERROR db 0E9h, 0DEh   ; CODE XREF: SLOWBOOT+BD↑j
                db 0FDh
word_84B8       dw 0                    ; DATA XREF: FINDFREESEG:loc_853E↓r
                                        ; FINDFREESEG+8C↓w ...
                db 0Eh dup(0)

                
; =============== S U B R O U T I N E =======================================


BOOTSCHEDULE    proc far                ; CODE XREF: BOOTSCHEDULE+A↓j
                                        ; BOOTSCHEDULE+E3↓j ...
                mov     ax, cs:HEADTDB

loc_3B10:                               ; CODE XREF: BOOTSCHEDULE+16↓j
                                        ; BOOTSCHEDULE+51↓j
                or      ax, ax
                jnz     short loc_3B18
                int     28h             ; DOS 2+ internal - KEYBOARD BUSY LOOP
                jmp     short near ptr BOOTSCHEDULE
; ---------------------------------------------------------------------------

loc_3B18:                               ; CODE XREF: BOOTSCHEDULE+6↑j
                mov     ds, ax
                mov     ax, ds:0
                cmp     word ptr ds:6, 0
                jz      short loc_3B10
                mov     di, ds
                mov     si, cs:CURTDB
                cmp     di, si
                jnz     short loc_3B36
                pop     ax
                pop     di
                pop     si
                pop     ds
                pop     bp
                dec     bp
                retf
; ---------------------------------------------------------------------------

loc_3B36:                               ; CODE XREF: BOOTSCHEDULE+21↑j
                push    cx
                mov     cx, cs:LOCKTDB
                jcxz    short loc_3B42
                cmp     cx, di
                jnz     short loc_3BB5

loc_3B42:                               ; CODE XREF: BOOTSCHEDULE+30↑j
                push    es
                push    bx
                les     bx, cs:PINDOS
                cmp     byte ptr es:[bx], 0
                jnz     short loc_3B5A
                les     bx, cs:PERRMODE
                cmp     byte ptr es:[bx], 0
                jz      short loc_3B5F

loc_3B5A:                               ; CODE XREF: BOOTSCHEDULE+41↑j
                pop     bx
                pop     es
                pop     cx
                jmp     short loc_3B10
; ---------------------------------------------------------------------------

loc_3B5F:                               ; CODE XREF: BOOTSCHEDULE+4C↑j
                inc     cs:INSCHEDULER
                push    dx
                inc     byte ptr ds:8
                push    ds
                call    DELETETASK
                push    ds
                call    INSERTTASK
                dec     byte ptr ds:8
                cli
                mov     es, si
                xor     si, si
                cmp     word ptr es:7Eh, 4454h
                jnz     short loc_3B93
                mov     word ptr es:4, ss
                mov     es:2, sp
                mov     si, es
                push    si
                call    SAVESTATE

loc_3B93:                               ; CODE XREF: BOOTSCHEDULE+75↑j
                push    ds
                push    si
                call    RESTORESTATE
                mov     ss, word ptr ds:4
                mov     sp, ds:2
                mov     cs:CURTDB, ds
                dec     cs:INSCHEDULER
                sti
                cmp     word ptr ds:16h, 0
                jnz     short loc_3BBD

loc_3BB2:                               ; CODE XREF: BOOTSCHEDULE+D5↓j
                pop     dx
                pop     bx
                pop     es

loc_3BB5:                               ; CODE XREF: BOOTSCHEDULE+34↑j
                pop     cx
                pop     ax
                pop     di
                pop     si
                pop     ds
                pop     bp
                dec     bp
                retf
; ---------------------------------------------------------------------------

loc_3BBD:                               ; CODE XREF: BOOTSCHEDULE+A4↑j
                mov     ax, 10h
                mov     bp, sp
                add     bp, 10h
                mov     cx, 1
                xchg    cx, ds:6
                dec     cx
                push    cx
                push    ds
                push    ax
                push    si
                push    cx
                push    word ptr ds:12h
                call    dword ptr ds:14h
                pop     cx
                add     ds:6, cx
                or      ax, ax
                jz      short loc_3BB2
                push    ds
                call    DELETETASK
                push    ds
                call    INSERTTASK
                pop     dx
                pop     bx
                pop     es
                pop     cx
                jmp     near ptr BOOTSCHEDULE
BOOTSCHEDULE    endp ; sp-analysis failed
