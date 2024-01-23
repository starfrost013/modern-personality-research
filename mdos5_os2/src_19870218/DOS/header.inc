;	SCCSID = @(#)header.inc	10.20 87/02/26
	PUBLIC	Header, HeaderLen
Header	LABEL	BYTE
IF	Debug
	DB	13,10,"Debugging CP-DOS version 1.0 "
	DB	13,10
ENDIF

	DB	13,10,"CP-DOS version 1.0 "

	DB	13,10, "Copyright 1986,1987  Microsoft Corp.",13,10

	DB	13,10, "PIGSKIN Internal revision 7.68.17, 87/02/26",13,10

HeaderLen equ	$ - Header
	DB	"$"