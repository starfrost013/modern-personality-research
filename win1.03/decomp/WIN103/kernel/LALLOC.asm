; =============== S U B R O U T I N E =======================================


LJOIN           proc near               ; CODE XREF: LFREE+22↓p
                                        ; LFREE+2E↓p ...
                dec     word ptr [di+4]
                mov     bx, [bx+2]
                and     word ptr [bx], 3
                or      [bx], si
                mov     [si+2], bx
                retn
LJOIN           endp


; =============== S U B R O U T I N E =======================================


LREPSETUP       proc near               ; CODE XREF: LZERO+C↓p
                                        ; LOCALREALLOC+15E↓p ...
                shr     cx, 1
                push    ds
                pop     es
                cld
                retn
LREPSETUP       endp


; =============== S U B R O U T I N E =======================================


LZERO           proc near               ; CODE XREF: LALLOC+B2↓p
                                        ; LOCALREALLOC+119↓p
                push    di
                mov     di, cx
                sub     cx, bx
                jnb     short loc_5422
                neg     cx
                push    ax
                xor     ax, ax
                call    LREPSETUP
                rep stosw
                pop     ax

loc_5422:                               ; CODE XREF: LZERO+5↑j
                pop     di
                retn
LZERO           endp


; =============== S U B R O U T I N E =======================================


LALLOC          proc near               ; CODE XREF: LALLOC+3D↓j
                                        ; LCOMPACT+15F↓p ...
                push    si
                push    ax
                push    bx
                mov     cx, [di+4]
                test    al, 2
                jnz     short loc_5479
                add     bx, 4
                call    LALIGN
                mov     bx, [di+6]
                mov     si, 2

loc_543A:                               ; CODE XREF: LALLOC+27↓j
                mov     ax, [bx+si]
                test    byte ptr [bx], 1
                jnz     short loc_5449
                sub     ax, bx
                cmp     ax, dx
                jnb     short loc_5466
                mov     ax, [bx+si]

loc_5449:                               ; CODE XREF: LALLOC+1B↑j
                mov     bx, ax
                loop    loc_543A
                call    LCOMPACT
                cmp     ax, dx
                jnb     short loc_5466

loc_5454:                               ; CODE XREF: LALLOC+78↓j
                push    ax
                xor     bx, bx
                xchg    ax, bx
                mov     cx, dx
                call    LNOTIFY
                pop     dx
                pop     bx
                pop     ax
                pop     si
                jnz     short LALLOC
                xor     ax, ax
                retn
; ---------------------------------------------------------------------------

loc_5466:                               ; CODE XREF: LALLOC+21↑j
                                        ; LALLOC+2E↑j
                mov     ax, 4
                mov     cx, [bx+si]
                sub     cx, 8
                mov     si, bx
                add     si, dx
                cmp     si, cx
                jnb     short loc_54C5
                push    bx
                jmp     short loc_54AF
; ---------------------------------------------------------------------------

loc_5479:                               ; CODE XREF: LALLOC+8↑j
                add     bx, 6
                call    LALIGN
                mov     bx, [di+8]

loc_5482:                               ; CODE XREF: LALLOC+71↓j
                mov     ax, [bx]
                test    al, 1
                jnz     short loc_5491
                mov     si, [bx+2]
                sub     si, bx
                cmp     si, dx
                ja      short loc_54A0

loc_5491:                               ; CODE XREF: LALLOC+62↑j
                and     al, 0FCh
                mov     bx, ax
                loop    loc_5482
                call    LCOMPACT
                cmp     ax, dx
                jb      short loc_5454
                mov     si, ax

loc_54A0:                               ; CODE XREF: LALLOC+6B↑j
                mov     ax, 6
                sub     si, dx
                mov     cx, si
                add     si, bx
                cmp     cx, 8
                jb      short loc_54C5
                push    si

loc_54AF:                               ; CODE XREF: LALLOC+53↑j
                mov     cx, si
                xchg    cx, [bx+2]
                mov     [si], bx
                mov     [si+2], cx
                xchg    si, cx
                and     word ptr [si], 3
                or      [si], cx
                inc     word ptr [di+4]
                pop     bx

loc_54C5:                               ; CODE XREF: LALLOC+50↑j
                                        ; LALLOC+88↑j
                add     ax, bx
                or      byte ptr [bx], 1
                pop     dx
                pop     dx
                test    dl, 40h
                jz      short loc_54D9
                mov     cx, ax
                mov     bx, [bx+2]
                call    LZERO

loc_54D9:                               ; CODE XREF: LALLOC+AB↑j
                pop     si
                or      ax, ax
                retn
LALLOC          endp


; =============== S U B R O U T I N E =======================================


LFREE           proc near               ; CODE XREF: LCOMPACT+61↓p
                                        ; LCOMPACT+149↓p ...
                mov     si, bx
                or      si, si
                jz      short loc_5511
                push    dx
                mov     dx, 3
                and     dx, [bx]
                xor     [bx], dx
                and     dl, 2
                jz      short loc_54F5
                xor     dx, dx
                xchg    dx, [bx+4]

loc_54F5:                               ; CODE XREF: LFREE+11↑j
                mov     si, [bx+2]
                test    byte ptr [si], 1
                jnz     short loc_5504
                xchg    bx, si
                call    LJOIN
                mov     bx, si

loc_5504:                               ; CODE XREF: LFREE+1E↑j
                mov     si, [bx]
                test    byte ptr [si], 1
                jnz     short loc_550E
                call    LJOIN

loc_550E:                               ; CODE XREF: LFREE+2C↑j
                mov     si, dx
                pop     dx

loc_5511:                               ; CODE XREF: LFREE+4↑j
                or      si, si
                retn
LFREE           endp


; =============== S U B R O U T I N E =======================================


LMOVE           proc near               ; CODE XREF: LCOMPACT+9A↓p
                                        ; LCOMPACT+105↓p
                mov     cx, [si+2]
                sub     cx, si
                add     si, cx
                mov     di, [bx+2]
                cmp     si, bx
                jz      short loc_5535
                mov     ax, di
                sub     ax, cx
                sub     ax, 8
                cmp     ax, bx
                jnb     short loc_5535
                mov     ax, di
                mov     di, bx
                add     di, cx
                jmp     short loc_5537
; ---------------------------------------------------------------------------

loc_5535:                               ; CODE XREF: LMOVE+C↑j
                                        ; LMOVE+17↑j
                mov     ax, di

loc_5537:                               ; CODE XREF: LMOVE+1F↑j
                dec     si
                dec     si
                dec     di
                dec     di
                shr     cx, 1
                push    ds
                pop     es
                std
                rep movsw
                cld
                inc     si
                inc     si
                inc     di
                inc     di
                retn
LMOVE           endp


; =============== S U B R O U T I N E =======================================


LBESTFIT        proc near               ; CODE XREF: LCOMPACT:loc_5685↓p
                push    bx
                push    cx
                push    dx
                xor     si, si
                push    si
                mov     dx, [bx+2]
                sub     dx, bx

loc_5553:                               ; CODE XREF: LBESTFIT+3C↓j
                mov     ax, [bx]
                test    al, 1
                jz      short loc_5580
                test    al, 2
                jz      short loc_5580
                mov     si, [bx+4]
                cmp     byte ptr [si+3], 0
                jnz     short loc_5580
                mov     ax, [bx+2]
                sub     ax, bx
                cmp     ax, dx
                ja      short loc_557E
                pop     si
                or      si, si
                jz      short loc_557B
                add     ax, si
                cmp     ax, [si+2]
                jbe     short loc_557D

loc_557B:                               ; CODE XREF: LBESTFIT+2A↑j
                mov     si, bx

loc_557D:                               ; CODE XREF: LBESTFIT+31↑j
                push    si

loc_557E:                               ; CODE XREF: LBESTFIT+25↑j
                mov     ax, [bx]

loc_5580:                               ; CODE XREF: LBESTFIT+F↑j
                                        ; LBESTFIT+13↑j ...
                and     al, 0FCh
                mov     bx, ax
                loop    loc_5553
                pop     si
                pop     dx
                pop     cx
                pop     bx
                retn
LBESTFIT        endp


; =============== S U B R O U T I N E =======================================


LCOMPACT        proc near               ; CODE XREF: LALLOC+29↑p
                                        ; LALLOC+73↑p ...
                push    si
                mov     word ptr [di+0Ah], 1001h
                cmp     word ptr [di+2], 0
                jz      short loc_559A
                dec     byte ptr [di+0Ah]

loc_559A:                               ; CODE XREF: LCOMPACT+A↑j
                                        ; LCOMPACT+7E↓j
                push    dx
                xor     ax, ax
                push    ax
                mov     bx, [di+8]
                mov     cx, [di+4]

loc_55A4:                               ; CODE XREF: LCOMPACT:loc_55AE↓j
                mov     ax, [bx]
                test    al, 1
                jz      short loc_560B
                and     al, 0FCh
                mov     bx, ax

loc_55AE:                               ; CODE XREF: LCOMPACT:loc_565D↓j
                loop    loc_55A4

loc_55B0:                               ; CODE XREF: LCOMPACT+42↓j
                                        ; LCOMPACT+76↓j
                pop     bx
                pop     dx
                mov     ax, bx
                or      ax, ax
                jz      short loc_55C6
                sub     ax, [bx+2]
                neg     ax
                dec     byte ptr [di+0Ah]
                jl      short loc_55C6
                cmp     ax, dx
                jb      short loc_55C8

loc_55C6:                               ; CODE XREF: LCOMPACT+2B↑j
                                        ; LCOMPACT+35↑j
                pop     si
                retn
; ---------------------------------------------------------------------------

loc_55C8:                               ; CODE XREF: LCOMPACT+39↑j
                push    dx
                push    bx
                dec     byte ptr [di+0Bh]
                jz      short loc_55B0
                inc     byte ptr [di+0Ah]
                xor     si, si

loc_55D4:                               ; CODE XREF: LCOMPACT+5A↓j
                                        ; LCOMPACT+70↓j
                call    HENUM
                jz      short loc_55FD
                push    cx
                mov     cl, 2
                xchg    ax, cx
                mov     bx, si
                call    LNOTIFY
                pop     cx
                or      ax, ax
                jz      short loc_55D4
                mov     bx, [si]
                sub     bx, 6
                call    LFREE
                xor     ax, ax
                mov     [si], ax
                or      byte ptr [si+2], 40h
                or      byte ptr [di+0Ah], 80h
                jmp     short loc_55D4
; ---------------------------------------------------------------------------

loc_55FD:                               ; CODE XREF: LCOMPACT+4C↑j
                test    byte ptr [di+0Ah], 80h
                jz      short loc_55B0
                xor     byte ptr [di+0Ah], 80h
                pop     bx
                pop     dx
                jmp     short loc_559A
; ---------------------------------------------------------------------------

loc_560B:                               ; CODE XREF: LCOMPACT+1D↑j
                cmp     word ptr [di+2], 0
                jnz     short loc_5660
                mov     si, ax
                test    byte ptr [si], 2
                jz      short loc_5685
                mov     si, [si+4]
                cmp     byte ptr [si+3], 0
                jnz     short loc_5685
                push    cx
                push    di
                mov     si, ax
                call    LMOVE
                mov     [di], si
                or      byte ptr [di], 3
                mov     bx, ax
                mov     [di+2], bx
                and     word ptr [bx], 3
                or      [bx], di
                mov     [si+2], di
                and     byte ptr [si], 0FCh
                mov     bx, [di+4]
                lea     ax, [di+6]
                mov     [bx], ax
                pop     di
                mov     al, 1
                lea     cx, [si+6]
                call    LNOTIFY
                pop     cx
                mov     bx, si
                mov     si, [bx]
                test    byte ptr [si], 1
                jnz     short loc_565D
                call    LJOIN
                dec     cx

loc_565D:                               ; CODE XREF: LCOMPACT+CC↑j
                                        ; LCOMPACT+F8↓j
                jmp     loc_55AE
; ---------------------------------------------------------------------------

loc_5660:                               ; CODE XREF: LCOMPACT+84↑j
                                        ; LCOMPACT+FF↓j
                pop     si
                cmp     si, bx
                jz      short loc_567D
                test    word ptr [bx], 1
                jnz     short loc_567D
                or      si, si
                jz      short loc_567B
                mov     ax, [si+2]
                sub     ax, si
                add     ax, bx
                cmp     [bx+2], ax
                jbe     short loc_567D

loc_567B:                               ; CODE XREF: LCOMPACT+E2↑j
                mov     si, bx

loc_567D:                               ; CODE XREF: LCOMPACT+D8↑j
                                        ; LCOMPACT+DE↑j ...
                push    si

loc_567E:                               ; CODE XREF: LCOMPACT+153↓j
                mov     bx, [bx]
                and     bl, 0FCh
                jmp     short loc_565D
; ---------------------------------------------------------------------------

loc_5685:                               ; CODE XREF: LCOMPACT+8B↑j
                                        ; LCOMPACT+94↑j
                call    LBESTFIT
                or      si, si
                jz      short loc_5660
                push    cx
                push    di
                push    word ptr [bx]
                call    LMOVE
                pop     cx
                cmp     bx, di
                jz      short loc_569D
                mov     cx, bx
                mov     [bx+2], di

loc_569D:                               ; CODE XREF: LCOMPACT+10B↑j
                mov     [di], cx
                or      byte ptr [di], 3
                mov     [di+2], ax
                xchg    ax, di
                and     word ptr [di], 3
                or      [di], ax
                xchg    ax, di
                lea     cx, [di+6]
                cmp     bx, di
                mov     bx, di
                mov     di, [di+4]
                xchg    cx, [di]
                pop     di
                pop     ax
                jz      short loc_56C1
                inc     ax
                inc     word ptr [di+4]

loc_56C1:                               ; CODE XREF: LCOMPACT+130↑j
                push    bx
                push    ax
                mov     al, 1
                mov     bx, [bx+4]
                call    LNOTIFY
                pop     cx
                and     byte ptr [si], 0FDh
                mov     bx, si
                push    word ptr [di+4]
                call    LFREE
                pop     si
                sub     si, [di+4]
                sub     cx, si
                pop     bx
                jmp     short loc_567E
; ---------------------------------------------------------------------------
                xor     ax, ax
                mov     bx, cx
                inc     bx
                shl     bx, 1
                shl     bx, 1
                push    cx
                call    LALLOC
                pop     cx
                jz      short loc_5702
                mov     bx, ax
                xchg    bx, [di+0Eh]
                push    di
                mov     di, ax
                mov     [di], cx
                inc     di
                inc     di
                call    HTHREAD
                mov     [di], bx
                pop     di

loc_5702:                               ; CODE XREF: LCOMPACT+163↑j
                mov     cx, ax
                retn
LCOMPACT        endp


; =============== S U B R O U T I N E =======================================


LENTER          proc near               ; CODE XREF: LOCALALLOC:loc_5849↓p
                                        ; LOCALREALLOC:loc_58EA↓p ...
                mov     di, ds:6
                mov     cx, 1
                xchg    cx, [di+1Ah]
                jcxz    short locret_5742
                xor     ax, ax
                mov     ax, 140h
                push    ax
                mov     ax, offset SZERRORENTERCRIT ; "EnterCrit: local heap is busy"
                push    cs
                push    ax
                push    ax
                push    cx
                call    KERNELERROR
                jmp     short locret_5742
; ---------------------------------------------------------------------------
SZERRORENTERCRIT db 'EnterCrit: local heap is busy',0
                                        ; DATA XREF: LENTER+12↑o
                db 24h
; ---------------------------------------------------------------------------

locret_5742:                            ; CODE XREF: LENTER+A↑j
                                        ; LENTER+1C↑j
                retn
LENTER          endp


; =============== S U B R O U T I N E =======================================


LLEAVE          proc near               ; CODE XREF: LOCALALLOC:loc_5896↓p
                                        ; LOCALREALLOC:loc_5A28↓p ...
                mov     di, ds:6
                xor     cx, cx
                xchg    cx, [di+1Ah]
                jcxz    short loc_574F
                retn
; ---------------------------------------------------------------------------

loc_574F:                               ; CODE XREF: LLEAVE+9↑j
                mov     ax, 140h
                push    ax
                mov     ax, offset SZLEAVECRITNOTBUSYHEAP ; "LeaveCrit: local heap is NOT busy"
                push    cs
                push    ax
                xor     ax, ax
                push    ax
                push    ax
                call    KERNELERROR
                jmp     short LALIGN
LLEAVE          endp

; ---------------------------------------------------------------------------
SZLEAVECRITNOTBUSYHEAP db 'LeaveCrit: local heap is NOT busy',0
                                        ; DATA XREF: LLEAVE+10↑o
                db 24h

; =============== S U B R O U T I N E =======================================


LALIGN          proc near               ; CODE XREF: LALLOC+D↑p
                                        ; LALLOC+58↑p ...
                jnb     short loc_5789
                mov     bx, 0FFFCh

loc_5789:                               ; CODE XREF: LALIGN↑j
                lea     dx, [bx+3]
                and     dl, 0FCh
                cmp     dx, bx
                jnb     short locret_5796
                mov     dx, 0FFFCh

locret_5796:                            ; CODE XREF: LALIGN+D↑j
                retn
LALIGN          endp


; =============== S U B R O U T I N E =======================================


LDREF           proc near               ; CODE XREF: LOCALREALLOC+52↓p
                                        ; LOCALREALLOC+153↓p ...
                xor     cx, cx
                mov     ax, si
                test    al, 2
                jnz     short loc_57AE
                xor     si, si
                or      ax, ax
                jz      short locret_57AD
                mov     bx, ax
                and     bl, 0FCh
                sub     bx, 4

locret_57AD:                            ; CODE XREF: LDREF+C↑j
                retn
; ---------------------------------------------------------------------------

loc_57AE:                               ; CODE XREF: LDREF+6↑j
                call    HDREF
                test    cl, 40h
                jnz     short loc_57F5
                or      ax, ax
                jz      short loc_57C4
                mov     bx, ax
                sub     bx, 6
                cmp     [bx+4], si
                jz      short loc_57F5

loc_57C4:                               ; CODE XREF: LDREF+21↑j
                xor     ax, ax
                mov     ax, 180h
                push    ax
                mov     ax, offset SZERRLDREF ; "LDREF: Invalid local handle"
                push    cs
                push    ax
                push    ax
                push    si
                call    KERNELERROR
                jmp     short loc_57F3
; ---------------------------------------------------------------------------
SZERRLDREF      db 'LDREF: Invalid local handle',0
                                        ; DATA XREF: LDREF+33↑o
                db 24h
; ---------------------------------------------------------------------------

loc_57F3:                               ; CODE XREF: LDREF+3D↑j
                xor     ax, ax

loc_57F5:                               ; CODE XREF: LDREF+1D↑j
                                        ; LDREF+2B↑j
                or      ax, ax
                retn
LDREF           endp


; =============== S U B R O U T I N E =======================================


LNOTIFY         proc near               ; CODE XREF: LALLOC+36↑p
                                        ; LCOMPACT+54↑p ...
                cmp     word ptr [di+18h], 0
                jz      short loc_5806
                xor     ah, ah
                push    ax
                push    bx
                push    cx
                call    dword ptr [di+16h]

loc_5806:                               ; CODE XREF: LNOTIFY+4↑j
                or      ax, ax
                retn
LNOTIFY         endp
