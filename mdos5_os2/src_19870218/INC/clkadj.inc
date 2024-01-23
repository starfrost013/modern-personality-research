;      SCCSID = @(#)clkadj.inc	8.2 86/10/05
;**     CLKADJ.INC - Adjustable Clock Parameters
;
;       This file contains the equates which determine clock frequency
;       and those paramters used by the CMOS clock driver that depend
;       on clock frequency.  It is used by IBMBIO.ASM and also by
;       INFOSEG.INC.

;  The following equates control or are dependent on the clock interrupt
;  frequency.  The three most likely frequencies have been identified to
;  make changes easier during development.

DV32K   EQU     00100000B               ; 32.768KHz Base divider rate

IN016   EQU     00001100B               ;  16Hz int rate, period = 62.5ms
IN032   EQU     00001011B               ;  32Hz int rate, period = 31.25ms
IN064   EQU     00001010B               ;  64Hz int rate, period = 16ms
IN512   EQU     00000111B               ; 512Hz int rate, period = 1.95ms

ifdef profile
RABYTE  EQU     (DV32K OR IN512)        ; Alter this equ to change clock freq
else
RABYTE  EQU     (DV32K OR IN032)        ; Alter this equ to change clock freq
endif

INTRVL  EQU     31                      ; Value of timer interval (msecs)
                                        ; 16Hz = 62
                                        ; 32Hz = 31
                                        ; 64Hz = 16

HUNCNT  EQU     3                       ; Value to inc hundredths of sec count
                                        ;  16Hz = 6
                                        ;  32Hz = 3
                                        ;  64Hz = 1

HUNCMP  EQU     8                       ; Number of ints before extra count
                                        ;  16Hz = 4
                                        ;  32Hz = 8
                                        ;  64Hz = 5

HUNAMT  EQU     1                       ; Amount of extra increment
                                        ;  16Hz = 1
                                        ;  32Hz = 1
                                        ;  64Hz = 3
