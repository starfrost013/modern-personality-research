<DPB structure>
;*	Drive Parameter Block
;
;	SCCSID = @(#)dpb.inc	8.1 86/08/30
;
;	Vital information about the format of a disk drive.

;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;	     C	A  V  E  A  T	  P  R	O  G  R  A  M  M  E  R		   ;
;									   ;

dpb	STRUC
dpb_drive	DB	?	; Logical drive # assoc with DPB (A=0,B=1,...)
dpb_unit	DB	?	; Driver unit number of DPB
dpb_driver_addr DD	?	; Pointer to driver
dpb_next_dpb	DD	?	; Pointer to next Drive parameter block
dpb_cbSector	dw	?	; sector size (for volume checking)
dpb_first_FAT	dw	?	; sector of first FAT (for ancient device drivers)
dpb_toggle_time dd	?	; time of last drive toggle
dpb_hVPB	dw	?	; handle of volume currently in drive
dpb_media	db	?	; most recent media that was in drive
dpb_flags	DB	?	; synchronization flags (see below)
dpb_drive_lock	DW	?	; Contains pid if drive locked by process
dpb	ENDS

DPBCHECK	equ    10h	; disk in drive is being removed/checked for VPB

;									   ;
;	     C	A  V  E  A  T	  P  R	O  G  R  A  M  M  E  R		   ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;