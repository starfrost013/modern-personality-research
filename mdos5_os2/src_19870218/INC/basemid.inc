;      SCCSID = @(#)basemid.inc	8.1 86/09/19
MSG_RESPONSE_DATA       EQU      0      ; data for valid responses (Y N A R I)
MSG_INVALID_FUNCTION    EQU      1      ; Invalid function
MSG_FILE_NOT_FOUND      EQU      2      ; File not found
MSG_PATH_NOT_FOUND      EQU      3      ; Path not found
MSG_OUT_OF_HANDLES      EQU      4      ; Too many open files, no handles left
MSG_ACCESS_DENIED       EQU      5      ; Access denied
MSG_INVALID_HANDLE      EQU      6      ; Invalid handle
MSG_MEMORY_BLOCKS_BAD   EQU      7      ; Memory control blocks destroyed
MSG_NO_MEMORY           EQU      8      ; Insufficient memory
MSG_INVALID_MEM_ADDR    EQU      9      ; Invalid memory block address
MSG_INVALID_ENVIRON     EQU     10      ; Invalid environment
MSG_INVALID_FORMAT      EQU     11      ; Invalid format
MSG_INVALID_ACC_CODE    EQU     12      ; Invalid access code
MSG_INVALID_DATA        EQU     13      ; Invalid data
MSG_14                  EQU     14      ; Reserved
MSG_INVALID_DRIVE       EQU     15      ; Invalid drive
MSG_ATT_RD_CURDIR       EQU     16      ; Attempt to remove current directory
MSG_NOT_SAME_DEVICE     EQU     17      ; Not same device
MSG_NO_MORE_FILES       EQU     18      ; No more files
MSG_ATT_WRITE_PROT      EQU     19      ; Attempted write on protected disk
MSG_UNKNOWN_UNIT        EQU     20      ; Unknown unit
MSG_DRIVE_NOT_READY     EQU     21      ; Drive not ready
MSG_UNKNOWN_COMMAND     EQU     22      ; Unknown command
MSG_DATA_ERROR          EQU     23      ; Data error (CRC)
MSG_BAD_REQ_STRUCTURE   EQU     24      ; Bad request structure length
MSG_SEEK_ERROR          EQU     25      ; Seek error
MSG_UNKNOWN_MEDIA       EQU     26      ; Unknown media type
MSG_SECTOR_NOT_FOUND    EQU     27      ; Sector not found
MSG_OUT_OF_PAPER        EQU     28      ; Printer out of paper
MSG_WRITE_FAULT         EQU     29      ; Write fault
MSG_READ_FAULT          EQU     30      ; Read fault
MSG_GENERAL_FAILURE     EQU     31      ; General failure
SHARING_VIOLATION   EQU     32      ; Sharing violation
MSG_SHAR_VIOLAT_FIND    EQU     32      ; sharing violation find
MSG_LOCK_VIOLATION      EQU     33      ; Lock violation
MSG_INVALID_DISK_CHANGE EQU     34      ; Invalid disk change
MSG_35                  EQU     35      ; FCB unavailable
MSG_SHARING_BUFF_OFLOW  EQU     36      ; Sharing buffer overflow
; msg 37 - 49 reserved
; msg 50 - 72 network related
; msg 73 - 79 reserved
MSG_FILE_EXISTS         EQU     80
; msg 81 reserved
MSG_CANNOT_MAKE         EQU     82
; msg 83 - 88 network related
MSG_DISK_CHANGE         EQU     107
MSG_DRIVE_LOCKED        EQU     108
MSG_ERROR_OPEN_FAILED   EQU     110     ; error open failed
MSG_DISK_FULL           EQU     112
MSG_NO_SEARCH_HANDLES   EQU     113
MSG_ERR_INV_TAR_HANDLE  EQU     114     ; error invalid target handle
MSG_BAD_DRIVER_LEVEL    EQU     119     ; bad driver level
MSG_INVALID_NAME        EQU     123
MSG_NO_VOLUME_LABEL     EQU     125
;
MSG_JOIN_ON_DRIV_IS_TAR     EQU  133    ; directory on drive is target of a jo
MSG_JOIN_DRIVE_IS           EQU  134    ; drive is joined
MSG_SUB_DRIVE_IS            EQU  135    ; drive is substed
MSG_DRIVE_IS_NOT_JOINED     EQU  136    ; drive is not joined
MSG_DRIVE_NOT_SUBSTED       EQU  137    ; drive is not substitued - subst
MSG_JOIN_CANNOT_JOIN_DRIVE  EQU  138    ; cannot join to a joined drive
MSG_SUB_CANNOT_SUBST_DRIVE  EQU  139    ; cannot sub to sub drive
MSG_JOIN_CANNOT_SUB_DRIVE   EQU  140    ; cannot join to a substed drive
MSG_SUB_CANNOT_JOIN_DRIVE   EQU  141    ; cannot sub to a joined drive
MSG_DRIVE_IS_BUSY           EQU  142    ; drive is busy - join
MSG_JOIN_SUB_SAME_DRIVE     EQU  143    ; cannot join or subst a drive to dir
MSG_DIRECT_IS_NOT_SUBDIR    EQU  144    ; directory is not subdirectory of root
MSG_DIRECT_IS_NOT_EMPTY     EQU  145    ; directory is not empty  -join
MSG_PATH_USED_SUBST_JOIN    EQU  146    ; path used in subst, join - join sub
MSG_JOIN_PATH_SPECIFIED     EQU  147    ; path specified is being joined
MSG_PATH_BUSY               EQU  148    ; path is busy - join
MSG_SUB_ON_DRIVE_IS_JOIN    EQU  149    ; directory on drive is target of a st
;
MSG_VOLUME_TOO_LONG     EQU     154
;
MSG_MR_CANT_FORMAT      EQU     317
MSG_MR_NOT_FOUND        EQU     318
MSG_MR_READ_ERROR       EQU     319
MSG_MR_UN_PERFORM       EQU     321
;
MSG_DIS_ERROR           EQU     355
MSG_NO_COUNTRY_SYS      EQU     396
MSG_OPEN_COUNTRY_SYS    EQU     397
MSG_COUNTRY_NO_TYPE     EQU     401
;
MSG_SWAP_INVALID_DRIVE      EQU  1470   ; Invalid drive %1 specified SWAPPATH
MSG_SWAP_INVALID_PATH       EQU  1471   ; Invalid path %1 specified in SWAPPATH
MSG_SWAP_CANNOT_CREATE      EQU  1472   ; Cannot create swap file %1
MSG_SWAP_DISABLED           EQU  1473   ; Segment swapping is disabled
MSG_SWAP_CANT_INIT          EQU  1474   ; Cannot initialize swapper
MSG_SWAP_NOT_READY          EQU  1500   ; diskette containing swap file not rdy
MSG_SWAP_WRITE_PROTECT      EQU  1501   ; Diskette containing swap file wrpro
MSG_SWAP_IN_ERROR           EQU  1502   ; I/O error on swap file
MSG_SWAP_IO_ERROR           EQU  1502   ; I/O error on swap file
MSG_SWAP_FILE_FULL          EQU  1503   ; Swap file is full
MSG_SWAP_TABLE_FULL         EQU  1504   ; Swap control table full
;
MSG_SYSINIT_UNREC_COM       EQU 1195    ; Unrecognized command in Config.sys
MSG_SYSINIT_INVAL_PARM      EQU 1196    ; Invalid parameter
MSG_SYSINIT_MISS_PARM       EQU 1197    ; Missing parameter
MSG_SYSINIT_BAD_MISS        EQU 1198    ; Bad or missing parameter
MSG_SYSINIT_CANT_TRACE      EQU 1199    ; Cannot TRACE buffer
MSG_SYSINIT_CANT_CREATE     EQU 1200    ; Cannot create 3X box
MSG_SYSINIT_CANT_STAR_SHEL  EQU 1201    ; Cannot start shell
MSG_SYSINIT_TRACEBUF_INVAL  EQU 1202    ; TRACEBUF size invalid
MSG_SYSINIT_TRACEBUF_IGNOR  EQU 1203    ; TRACEBUF command ignored
MSG_SYSINIT_INSTALL_REFUS   EQU 1204    ; Installation refused
MSG_SYSINIT_BOX_TRASHED     EQU 1205    ; 3X box trashed by device
MSG_SYSINIT_OUT_OF_MEM      EQU 1206    ; Out of mem loading old device
MSG_SYSINIT_INIT_ERROR      EQU 1207    ; Initialization error
