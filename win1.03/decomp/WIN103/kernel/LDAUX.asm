
; =============== S U B R O U T I N E =======================================

; Attributes: bp-based frame

FINDORDINAL     proc near               ; CODE XREF: SEGRELOC+AF↓p
                                        ; GETPROCADDRESS+3A↓p

arg_0           = dword ptr  4
arg_4           = word ptr  8

                push    bp
                mov     bp, sp
                push    si
                push    di
                les     si, [bp+arg_0]
                cmp     byte ptr es:[si+1], 23h ; '#'
                jz      short loc_91C

loc_8E7:                                ; CODE XREF: FINDORDINAL+56↓j
                push    ds
                mov     ds, [bp+arg_4]
                mov     si, ds:26h
                cld

loc_8F0:                                ; CODE XREF: FINDORDINAL+8A↓j
                xor     ax, ax
                lodsb
                add     si, ax
                add     si, 2

loc_8F8:                                ; CODE XREF: FINDORDINAL+42↓j
                lodsb
                les     di, [bp+arg_0]
                mov     cx, ax
                jcxz    short loc_93E
                cmp     es:[di], al
                jnz     short loc_915
                inc     di
                repe cmpsb
                jnz     short loc_915
                lodsw
                mov     bx, ds
                pop     ds
                cmp     [bp+arg_4], bx
                jz      short invalid_procedure_name
                jmp     short loc_965
; ---------------------------------------------------------------------------

loc_915:                                ; CODE XREF: FINDORDINAL+2B↑j
                                        ; FINDORDINAL+30↑j
                add     si, cx
                add     si, 2
                jmp     short loc_8F8
; ---------------------------------------------------------------------------

loc_91C:                                ; CODE XREF: FINDORDINAL+D↑j
                lods    byte ptr es:[si]
                mov     cl, al
                xor     ch, ch
                dec     cx
                inc     si
                xor     ax, ax

loc_926:                                ; CODE XREF: FINDORDINAL+62↓j
                mov     dx, ax
                lods    byte ptr es:[si]
                sub     al, 30h ; '0'
                cmp     al, 9
                ja      short loc_8E7
                xor     ah, ah
                mov     bx, ax
                mov     al, 0Ah
                mul     dx
                add     ax, bx
                loop    loc_926
                jmp     short invalid_procedure_name
; ---------------------------------------------------------------------------

loc_93E:                                ; CODE XREF: FINDORDINAL+26↑j
                mov     bx, ds
                pop     ds
                cmp     [bp+arg_4], bx
                jnz     short loc_965
                mov     bx, 0FFFFh
                mov     es, [bp+arg_4]
                mov     ax, 2Ch ; ','
                mov     dx, es:20h
                push    es
                push    bx
                push    ax
                push    dx
                call    LOADNRTABLE
                push    ds
                mov     ds, dx
                mov     si, ax
                or      ax, dx
                jnz     short loc_8F0
                pop     ds

loc_965:                                ; CODE XREF: FINDORDINAL+3B↑j
                                        ; FINDORDINAL+6C↑j
                push    ax
                mov     ax, 2Ch ; ','
                push    [bp+arg_4]
                push    ax
                call    FREENRTABLE
                pop     ax

invalid_procedure_name:                 ; CODE XREF: FINDORDINAL+39↑j
                                        ; FINDORDINAL+64↑j
                or      ax, ax
                jnz     short loc_9A4
                les     bx, [bp+arg_0]
                inc     bx
                mov     ax, 404h
                push    ax
                mov     ax, offset SZINVALIDPROC ; "Invalid procedure name "
                push    cs
                push    ax
                push    es
                push    bx
                call    KERNELERROR
                jmp     short loc_9A2
; ---------------------------------------------------------------------------
SZINVALIDPROC   db 'Invalid procedure name ',0
                                        ; DATA XREF: FINDORDINAL+A5↑o
                db 24h
; ---------------------------------------------------------------------------

loc_9A2:                                ; CODE XREF: FINDORDINAL+AF↑j
                xor     ax, ax

loc_9A4:                                ; CODE XREF: FINDORDINAL+9B↑j
                pop     di
                pop     si
                mov     sp, bp
                pop     bp
                retn    6
FINDORDINAL     endp



; =============== S U B R O U T I N E =======================================

; Attributes: bp-based frame

FINDEXEINFO     proc near               ; CODE XREF: LOADMODULE+76↑p
                                        ; LOADMODULE+1CF↑p ...

arg_0           = word ptr  4
arg_2           = dword ptr  6

                push    bp
                mov     bp, sp
                push    si
                push    di
                push    ds
                mov     bx, [bp+arg_0]
                mov     ax, cs:HEXEHEAD

loc_7EF:                                ; CODE XREF: FINDEXEINFO+2B↓j
                or      ax, ax
                jz      short loc_80F
                mov     es, ax
                mov     di, es:26h
                cmp     es:[di], bl
                jnz     short loc_809
                inc     di
                lds     si, [bp+arg_2]
                mov     cx, bx
                repe cmpsb
                jz      short loc_80F

loc_809:                                ; CODE XREF: FINDEXEINFO+1B↑j
                mov     ax, es:6
                jmp     short loc_7EF
; ---------------------------------------------------------------------------

loc_80F:                                ; CODE XREF: FINDEXEINFO+F↑j
                                        ; FINDEXEINFO+25↑j
                pop     ds
                pop     di
                pop     si
                mov     sp, bp
                pop     bp
                retn    6
FINDEXEINFO     endp


