SCCSID = @(#)utlerror.inc	8.1 86/09/19
;
; error codes for utilities
;

ERROR_INVALID_DOSVER            EQU     1000    ; invalid dos version
ERROR_LANGUAGE_NOT_SUPPORTED    EQU     1001    ; language not supported
ERROR_INVALID_FILENAME          EQU     1002    ; invalid file name
ERROR_MSGFILE_BAD_FORMAT        EQU     1010    ; bad message file format
ERROR_MSGFILE_BAD_MID           EQU     1011    ; message file has bad mid
ERROR_MSGFILE_OUTERROR          EQU     1012    ; error writing output file
ERROR_MSGFILE_INPERROR          EQU     1013    ; error reading input file
ERROR_MSGFILE_SYNTAX            EQU     1014    ; syntax error
ERROR_MSGFILE_MSG_TOO_BIG       EQU     1015    ; message exceeds system limit
ERROR_INSTALL_FAILED            EQU     1016    ; install failed
ERROR_INVALID_VOL               EQU     1017    ; no volume with /s option
ERROR_INVALID_INPUT_PARM        EQU     1018    ; invalid input parameter
ERROR_FILE_SPEC_REQUIRED        EQU     1019    ; file specification required
ERROR_SORT_FILE_TOO_BIG         EQU     1020    ; file size to big to sort
ERROR_SORT_INVALID_COL          EQU     1021    ; invalid column number for sort
ERROR_CHK_BAD_FAT               EQU     1022    ; fat bad on specified drive
ERROR_CHK_BAD_ROOT              EQU     1023    ; root bad on specified drive
ERROR_INVALID_PARM_NUMBER       EQU     1024    ; invalid parameter number
ERROR_PARM_SYNTAX               EQU     1025    ; invalid parameter syntax
ERROR_UTIL_TERMINATED           EQU     1026    ; program terminated by user
ERROR_REST_NO_BACKUP            EQU     1027    ; source does not contain backup file
ERROR_REST_SEQUENCE_ERROR       EQU     1028    ; file sequence error
ERROR_REST_FILE_CREATE          EQU     1029    ; file creation error
ERROR_FDISK_ERR_WRITE           EQU     1030    ; error writing fixed disk
ERROR_FDISK_ERR_READ            EQU     1031    ; error reading fixed disk
ERROR_FDISK_ERR_NOFDISKS        EQU     1032    ; no fixed disk present
ERROR_PATCH_NO_CTL_FILE         EQU     1033    ; cannot open patch control file
ERROR_PATCH_NO_EXE_FILE         EQU     1034    ; cannot open file to patch
ERROR_PATCH_CANT_ALLOC          EQU     1035    ; insufficient memory to alloc patch
ERROR_PATCH_INV_CMD_COMBO       EQU     1036    ; bad command sequence
ERROR_PATCH_INV_OFFSET          EQU     1037    ; invalid offset
ERROR_PATCH_INV_BYTES           EQU     1038    ; invalid byte string
ERROR_PATCH_TOO_NEAR_EOF        EQU     1039    ; too close to end of file
ERROR_PATCH_VERIFY_FAILED       EQU     1040    ; verification failed
ERROR_PATCH_INC_VERIFY          EQU     1041    ; offset verification failed
ERROR_SPOOL_INVAL_DEVICE        EQU     1042    ; invalid device
ERROR_SPOOL_INVAL_IN_DEV_PAR    EQU     1043    ; invalid input device parm
ERROR_SPOOL_INVAL_OUT_DEV_PAR   EQU     1044    ; invalid output device parm
ERROR_SPOOL_INVAL_SUB           EQU     1045    ; invalid subdirectory
ERROR_SPOOL_INTERN_ERROR        EQU     1046    ; internal error
ERROR_SPOOL_DISK_FULL           EQU     1047    ; spool disk full
ERROR_SPOOL_CANNOT_PT_NXT_FI    EQU     1048    ; cannot print next file
ERROR_SPOOL_CANNOT_PTR_NOW_FI   EQU     1049    ; cannot print now
ERROR_XCOPY_CANNOT_COPY_SUB     EQU     1050    ; cannot copy subdirectory
ERROR_XCOPY_ITSELF              EQU     1051    ; cannot copy to itself
ERROR_DISKC_DRIVE_SPEC          EQU     1052    ; drive mismatch in diskcomp/copy
ERROR_FORMAT_FAIL               EQU     1053    ; format failed
ERROR_FORMAT_INV_MEDIA          EQU     1054    ; format had invalid media
ERROR_RAS_STCP                  EQU     1055    ; system trace command processor error
ERROR_RAS_CREATEDD              EQU     1056    ; error in create dump diskette
ERROR_XCOPY_UNAB_CREATE_DIR     EQU     1057    ; unable to create directory
ERROR_XCOPY_PATH_TOO_LONG       EQU     1058    ; path to long
ERROR_XCOPY_CANNOT_TO_RES_DEV   EQU     1059    ; can't copy to a reserved name
ERROR_XCOPY_CANNOT_COPY_RES     EQU     1060    ; can't copy from a reserved name
ERROR_XCOPY_INVAL_DATE          EQU     1061    ; invalid date
ERROR_XCOPY_INTERNAL            EQU     1062    ; xcopy internal error
ERROR_PRINT_INVALID_PARAMETER   EQU     1063    ; invalid parameter
ERROR_PRINT_FILE_NOT_FOUND      EQU     1064    ; file not found
ERROR_PRINT_INVALID_DRIVE       EQU     1065    ; invalid drive
ERROR_PRINT_INVALID_DOSVER      EQU     1066    ; invalid DOS version
ERROR_PRINT_BAD_ENVIRONMENT     EQU     1067    ; bad environment
ERROR_PRINT_MSGFILE_BAD_MID     EQU     1068    ; bad message ID
ERROR_PRINT_SYS_INTERNAL        EQU     1069    ; PRINT internal error
ERROR_SYS_SYS_INTERNAL          EQU     1070    ; SYS internal error
ERROR_SYS_MSGFILE_BAD_MID       EQU     1071    ; bad message ID
ERROR_SYS_INVALID_DOSVER        EQU     1072    ; invalid DOS version
ERROR_SYS_INVALID_DRIVE         EQU     1073    ; invalid drive
ERROR_SYS_INVALID_MEDIA         EQU     1074    ; invalid media
ERROR_SYS_INVALID_PARM          EQU     1076    ; invalid parameter
ERROR_FORMAT_INTERRUPT          EQU     1077    ; format interrupted
ERROR_FORMAT_NO_SYSXFER         EQU     1078    ; error in system xfer
ERROR_FORMAT_USER_TERM          EQU     1079    ; terminated by N resp
ERROR_BACK_NO_FILES             EQU     1080    ; no files found for backup
ERROR_BACK_SHARE_ERROR          EQU     1081    ; sharing error during backup
ERROR_BACK_FDISKFULL            EQU     1082    ; fixed disk is full during backup
ERROR_BACK_INVTARGET            EQU     1083    ; invalid backup target disk
ERROR_BACK_INVTIME              EQU     1084    ; invalid time parameter
ERROR_BACK_NOSOURCE             EQU     1085    ; no source backup files
ERROR_BACK_NOTARGET             EQU     1086    ; no target backup files
ERROR_BACK_SRC_TGT_SAME         EQU     1087    ; backup source and target same