;      SCCSID = @(#)utilmid.inc	8.4 86/10/10

MSG_REC_FILE_NOT_FOUND  EQU      2      ; file not found

MSG_DCOMP_INSUFF_MEM    EQU      8      ; insufficient storage - diskcomp
MSG_DCOPY_INSUFF_MEM    EQU      8      ; insufficient storage - diskcopy
MSG_CHK_MEMORY_UNAVAIL  EQU      8      ; memory unavailable - chkdsk
MSG_REST_INSUF_MEM      EQU      8      ; insufficient memory
MSG_BACK_INSUF_MEM      EQU      8      ; insufficient mem
MSG_REC_UNABLE_ALL_MEM  EQU      8      ; unable to allocate memory
;
MSG_SORT_INVAL_PARA         EQU  1005   ; sort invalid parameter
MSG_SORT_NOT_ENOUGH_MEMORY  EQU  1006   ; sort file size too big
MSG_SORT_INSUF_DISK_SPACE   EQU  1008   ; insufficient disk space, no space ava
MSG_SORT_READ_ERROR         EQU  1009   ; error reading from disk/diskette
; 1010 - 1014 unused
MSG_REC_ENT_BEG_REC         EQU  1015   ; Press Enter to being recover drive %1
MSG_REC_ERROR_DRIVE         EQU  1016   ; %1 error %2 drive %3
MSG_REC_INVAL_PARAMETER     EQU  1018   ; Invalid parameter
MSG_REC_INVAL_DRIVE         EQU  1019   ; invalid drive
MSG_REC_INVAL_DRIVE_FILE    EQU  1021   ; invalid drive or filename
MSG_REC_BYTES_RECOVERED     EQU  1023   ; %1 of %2 bytes recovered
MSG_REC_WARN_DIR_FULL       EQU  1024   ; warning directory full
MSG_REC_FILES_RECOVERED     EQU  1025   ; %1 files recovered
MSG_REC_TOO_MANY_PARAMETERS EQU  1026   ; Too many parameters entered
; 1027 - 1030 unused
MSG_REN_INVAL_NUM_PARA      EQU  1031   ; Rename invalid number of parameters
MSG_STRIKE_ANY_KEY          EQU  1032   ; press any key when ready
MSG_DIR_INVALID_DIR         EQU  1033   ; Dir invalid directory
MSG_COM_SEARCH_DIR_BAD      EQU  1034   ; specified command search dir bad
MSG_REN_INVAL_PATH_FILENAME EQU  1035   ; rename invalid path or filename
MSG_DATE_INVALID            EQU  1036   ; invalid date
MSG_NO_BAT_LABEL            EQU  1039   ; batch label not found
MSG_DIR_BAD_COMMAND_OR_FILE EQU  1041   ; bad command or filename
MSG_RMDIR_INVALID_PATH      EQU  1042   ; rmdir is bad
MSG_REN_INVALID_PARAMETER   EQU  1043   ; rename invalid parameter
MSG_REN_INVALID_TIME        EQU  1044   ; rename invalid time
MSG_BAD_DOS_VER             EQU  1045   ; CMD and DOS incompat
MSG_VER_SPEC_ON_OFF         EQU  1046   ; verify and break on or off
MSG_COMM_VER                EQU  1047   ; command version 5.0
MSG_C                       EQU  1048   : c acknowledgement
MSG_FILES_COPIED            EQU  1049   ; % files copied
MSG_CURRENT_DATE            EQU  1050   ; current date is
MSG_CURRENT_TIME            EQU  1051   ; current time is
MSG_DOSWRITE_ERROR_TO_DEV   EQU  1052   ; doswrite error writing to device
MSG_DIR_OF                  EQU  1053   ; directory of %1
MSG_DIR                     EQU  1054   ; dir
MSG_OUT_OF_ENVIRON_SPACE    EQU  1056   ; out of environment space
MSG_ERROR_IN_EXEC_FILE      EQU  1058   ; error in exec file
MSG_EXEC_FAILURE            EQU  1059   ; EXEC failure
MSG_FILES_FREE              EQU  1060   ; %1 files %2 bytes free
MSG_FILE_CREATION_ERROR     EQU  1061   ; file creation error
MSG_RDR_FILE_CREATE         EQU  1062   ; Redir file create err
MSG_RDR_NOT_FOUND           EQU  1063   ; Redir file not found
MSG_LINES_TOO_LONG          EQU  1065   ; lines too long
MSG_CONT_LOST_BEF_COPY      EQU  1066   ; content of destination lost before
MSG_INSRT_DISK_BAT          EQU  1067   ; insert disk with batch file press key
MSG_ENTER_NEW_DATE          EQU  1068   ; Enter new date
MSG_SYNERR_NLN              EQU  1069   ; newline
MSG_ENTER_NEW_TIME          EQU  1070   ; enter new line
MSG_RDR_HNDL_CREATE         EQU  1071   ; Handle creation error
MSG_ECHO_OFF                EQU  1074   ; ECHO is off
MSG_ECHO_ON                 EQU  1075   ; ECHO is on
MSG_VERIFY_OFF              EQU  1076   ; Verify is off
MSG_VERIFY_ON               EQU  1077   ; Verify is on
MSG_CANNOT_COPIED_ONTO_SELF EQU  1078   ; message cannot be copied onto self
MSG_SYNERR_GENL             EQU  1079   ; syntax error general
MSG_TOP_LEVEL_PROCESS_CAN   EQU  1081   ; top level process aborted, cannot con
MSG_PID_IS                  EQU  1082   ; pid is %1
MSG_DUP_FILENAME_OR_NOT_FD  EQU  1083   ; dup file name or file not found
MSG_ARE_YOU_SURE            EQU  1084   ; Are you sure (Y/N)?
MSG_SET_SYNTAX_ERROR        EQU  1085   ; syntax error
MSG_TOKEN_TOO_LONG          EQU  1086   ; token too long
MSG_PROG_TOO_BIG_MEM        EQU  1087   ; program is too big to fit into memory
MSG_MS_DOS_VERSION          EQU  1090   ; cp-dos version %1 %2
MSG_PIPE_FAILURE            EQU  1092   ; pipeline failure %1
MSG_MS_MORE                 EQU  1093   ; more?
; 1094 - 1099 unused
MSG_JOIN_SUBST              EQU  1100   ; %1 => %2
MSG_BAD_VERSION             EQU  1210   ; bad dos version - join
MSG_BAD_JOIN_SYNTAX         EQU  1101   ; bad join syntax
MSG_BAD_PARAMETER           EQU  1102   ; bad parameter - join
MSG_INCORRECT_NUM_PARAM     EQU  1103   ; incorrect number parameter - join
MSG_CANNOT_CREATE_DIR       EQU  1104   ; cannot create directory - join
MSG_DIRECT_DOES_NOT_EXIST   EQU  1105   ; directory does not exist - join
MSG_DOS_INTERNAL_ERROR      EQU  1106   ; internal dos error
MSG_INCOR_SUB_FUNC_REQUEST  EQU  1113   ; incorrect sub-function request

MSG_APPEND                  EQU  1123   ; append
MSG_APPEND_NO_DIR           EQU  1124   ; no append directories
MSG_APPEND_OUT_OF_MEM       EQU  1125   ; out of memory
MSG_APPEND_TOO_LONG         EQU  1126   ; data path too long
MSG_ASSIGN_OUT_MEM          EQU  1127   ; out of memory assign
;1128 - 1135 unused
MSG_REP_INCOR_VERSION       EQU  1135   ; incorrect dos version replace
MSG_REP_INCOR_PARA          EQU  1136   ; incorrect paramters replace
MSG_REP_SOUR_PATH_REQ       EQU  1137   ; source path required replace
MSG_REP_PARA_NOT_COMP       EQU  1138   ; parameters not complatable
MSG_REP_ENTER_ADD_FILE      EQU  1139   ; press enter to add file
MSG_REP_ENTER_REPLACE_FILE  EQU  1140   ; press enter replace file
MSG_REP_NO_FILES_FOUND      EQU  1141   ; no files found replace
MSG_REP_FILE_NOT_COPY_SELF  EQU  1142   ; file cannot be copied to self %1
MSG_REP_NOT_FOUND           EQU  1143   ; file not found %1
MSG_REP_PATH_REQ_NOT_FOUND  EQU  1144   ; path requested not found
MSG_REP_ACCESS_DENIED       EQU  1145   ; access to %1 denied
MSG_REP_DRIVE               EQU  1146   ; drive rep incorrect
MSG_REP_UNEXEP_ERROR        EQU  1147   ; unexpected DOS error rep
MSG_REP_NO_FILES_ADDED      EQU  1148   ; no files added.
MSG_REP_FILES_ADDED         EQU  1149   ; files %1 added.
MSG_REP_NO_FILES_REP        EQU  1150   ; no files replaced
MSG_REP_FILES_REP           EQU  1151   ; files %1 replaced
MSG_REP_ADD                 EQU  1152   ; add %1 y/n
MSG_REP_FILES               EQU  1153   ; want to replace %1
MSG_REP_ADDING_FILES        EQU  1154   ; adding %1 files
MSG_REP                     EQU  1155   ; replacing %1 files
;1156 - 1160 unused
MSG_COMP_COMPARE_MORE       EQU  1161   ; compare more files
MSG_COMP_ENTER_FILE_1ST     EQU  1162   ; enter primary filename
MSG_COMP_ENTER_FILE_2ND     EQU  1163   ; enter 2nd filename or drive letter
MSG_COMP_COMPARE_OK         EQU  1164   ; files compare ok
MSG_COMP_END_COMPARE        EQU  1165   ; 10 mismatches ending compare
MSG_COMP_FILE2_BYTE         EQU  1166   ; file2 = %0
MSG_COMP_FILE1_BYTE         EQU  1167   ; file 1 = %0
MSG_COMP_FILE_OFFSET        EQU  1168   ; compare error at offset %0
MSG_COMP_LENGTH_MISMATCH    EQU  1169   ; files are different sizes
MSG_COMP_1ST_2ND_FILENAMES  EQU  1170   ; %1 and %2
MSG_COMP_INVALID_PATH       EQU  1171   ; invalid path
MSG_COMP_FILE_NOT_FOUND     EQU  1490    ; %1 file not found
MSG_COMP_INVALID_DRIVE      EQU  1173   ; invalid drive
MSG_COMP_INVALID_DOSVER     EQU  1210    ; invalid dos version
;1175 - 1179 unused
MSG_MOUSE_PARA_MOD          EQU  1180   ; parameters modified
MSG_MOUSE_NOT_LOADED        EQU  1181   ; mouse device driver version not load
MSG_MOUSE_DEINSTALLED       EQU  1182   ; mouse device driver %1 %2 deinstalled
MSG_MOUSE_LOADED            EQU  1183   ; mouse loaded
MSG_XCOPY_ITSELF            EQU  1184   ; cannot copy itself
MSG_XCOPY_INTERNAL_ERROR    EQU  1185   ; internal error
MSG_XCOPY_NO_OPEN_SOURCE    EQU  1186   ; no source open
MSG_XCOPY_NO_OPEN_TARGET    EQU  1187   ; no target open
MSG_XCOPY_NO_READ_SOURCE    EQU  1188   ; no read source
MSG_XCOPY_NO_CLOSE_SOURCE   EQU  1189   ; close source
MSG_XCOPY_NO_CLOSE_TARGET   EQU  1190   ; no close target
MSG_XCOPY_NO_WRITE_TARGET   EQU  1191   ; no write target
MSG_XCOPY_NO_ACCESS_SOURCE  EQU  1192   ; no access source
MSG_XCOPY_NO_ACCESS_TARGET  EQU  1193   ; no acess target
MSG_XCOPY_INVALID_DATE      EQU  1194   ; invalid date
;1195 - 1207 basemid
MSG_ENTER_JAPAN_DATE        EQU  1208   ; enter the new date yy-mm-dd
MSG_ENTER_DEF_DATE          EQU  1209   ; enter the new date dd-mm-yy

MSG_INCORRECT_DOSVER        EQU  1210    ; incorrect dos version
MSG_SAD_INV_DOS             EQU  1210    ; invalid dos version - createdd
MSG_STCP_INV_DOS            EQU  1210    ; trace only in >= dos  - trace
MSG_INVALID_DOS_PRINT       EQU  1210    ; incorrect dos version
MSG_TREE_INV_DOSVER         EQU  1210    ; incorrect DOS version
MSG_INVAL_DOS_VERSION       EQU  1210    ; invalid DOS version
MSG_REST_INVAL_VERS         EQU  1210    ; invalid dos version not 1.0
MSG_BACK_INCOR_DOS_VER      EQU  1210    ; inval dos ver
MSG_REP_INCOR_DOS_VERSION   EQU  1210    ; incorrect dos version replace
MSG_ABORT_RETRY_IGNORE      EQU  1211    ; abort, retry, or ignore
MSG_REC_ABORT_RETRY_IGNORE  EQU  1211    ; abort, retry, ignore
MSG_PRESS_ANY_KEY           EQU  1212    ; press any key when ready
MSG_SAV_TIME                EQU  1213    ; process timed-out save and is suspend
MSG_RESTO_TIME              EQU  1214    ; process timed-out restore is suspend
MSG_CHG_PAP                 EQU  1215    ; Change paper, press Enter to continue
MSG_DCOMP_INV_PARM          EQU  1230    ; invalid parameter - diskcomp
MSG_DCOMP_INV_DRIVE         EQU  1231    ; invalid drive - diskcomp
MSG_DCOPY_INV_DRV           EQU  1231    ; invalid drive - diskcopy
MSG_DCOMP_INSERT_FIRST      EQU  1232    ; insert first disk - diskcomp
MSG_DCOPY_INSERT_SRC        EQU  1232    ; insert soruce disk - diskcopy
MSG_DCOMP_INSERT_SECOND     EQU  1233    ; insert second disk - diskcomp
MSG_DCOPY_INSERT_TARG       EQU  1233    ; insert target disk - diskcopy
MSG_DCOMP_DISK1_BAD         EQU  1234    ; first disk bad - diskcomp
MSG_DCOMP_DISK2_BAD         EQU  1235    ; second disk bad - diskcomp
MSG_DCOMP_DRV_NOT_RDY       EQU  1236    ; drive not ready - diskcomp
MSG_DCOPY_DRV_NOT_RDY       EQU  1236    ; drive not ready - diskcopy
MSG_DCOMP_TARGET_WP         EQU  1237    ; target write protected - diskcomp
MSG_DCOMP_REPEAT_COMP       EQU  1238    ; compare another? - diskcomp
MSG_DCOMP_COMPARING         EQU  1239    ; comparing ...  - diskcomp
MSG_DCOMP_DRV_INCOMPAT      EQU  1240    ; drive incompatible - diskcomp
MSG_DCOPY_COPYING           EQU  1240    ; copying ...  - diskcopy
MSG_DCOMP_COMPARE_ERROR     EQU  1242    ; compare error - diskcomp
MSG_DCOMP_UNREC_RDERR       EQU  1243    ; unrecoverable read error - diskcomp
MSG_DCOMP_COMPARE_ENDED     EQU  1244    ; compare ended - diskcomp
MSG_DCOMP_COMPARE_OK        EQU  1245    ; compare ok - diskcomp
MSG_DCOMP_ENTER_SRC         EQU  1246    ; source drive letter - diskcomp/copy
MSG_DCOMP_ENTER_TARG        EQU  1247    ; target drive letter - diskcomp/copy
MSG_DCOPY_VOL_SER           EQU  1248    ; volume number for dcopy
MSG_DCOPY_FORMATTING        EQU  1252    ; copying while formatting - diskcopy
MSG_DCOPY_TARG_NOUSE        EQU  1255    ; target disk possibly bad - diskcopy
MSG_DCOPY_UNUSABLE          EQU  1256    ; target disk bad - diskcopy
MSG_DCOPY_COPY_ANOTHER      EQU  1259    ; copy another?  - diskcopy
MSG_DCOPY_COPY_TRACKS       EQU  1260    ; copy tracks
MSG_DCOPY_ERR_ON_DRV        EQU  1262    ; error on drive - diskcopy
MSG_DCOPY_READ_ERROR        EQU  1264    ; read error - diskcopy
MSG_DCOPY_UNREC_RDERR       EQU  1265    ; unrecoverable read error - diskcopy
MSG_DCOPY_COPY_ENDED        EQU  1266    ; copy ended - diskcopy
MSG_DCOPY_SOURCE_BAD        EQU  1267    ; source disk bad - diskcopy
MSG_DCOPY_TARGET_BAD        EQU  1268    ; target disk bad - diskcopy
MSG_DCOPY_CONTINUE          EQU  1269    ; continue?  - diskcopy
MSG_FMT_INS_NEW_DISK        EQU  1270    ; insert new disk - format
MSG_FMT_DISK_WARNING        EQU  1271    ; fixed disk warning - format
MSG_FMT_SYS_XFERED          EQU  1272    ; system transferred - format
MSG_FMT_ANOTHER             EQU  1273    ; format another - format
MSG_FMT_INV_VOLUME          EQU  1274    ; invalid volume name - format
MSG_FMT_INV_DRV             EQU  1275    ; invalid drive - format
MSG_FMT_INV_PARAMETER       EQU  1276    ; invalid parameter - format
MSG_FMT_REINSERT_DISK       EQU  1277    ; reinsert target disk - format
MSG_FMT_INSERT_DOS          EQU  1278    ; reinsert system disk - format
MSG_FMT_FORMAT_FAIL         EQU  1279    ; format failure - format
MSG_FMT_DISK_UNSUIT         EQU  1280    ; disk unsuitable for system - format
MSG_FMT_INV_MEDIA           EQU  1281    ; invalid media - format
MSG_FMT_INSUFF_STORAGE      EQU  1282    ; insufficient storage - format
MSG_FMT_DISK_SPACE          EQU  1283    ; disk space available - format
MSG_FMT_BYTES_USED          EQU  1284    ; bytes used - format
MSG_FMT_BYTES_BAD           EQU  1285    ; bytes in bad sectors - format
MSG_FMT_BYTES_AVAIL         EQU  1286    ; bytes available - format
MSG_FMT_ASSIGNED_DRV        EQU  1287    ; cannot format assigned drive - format
MSG_FMT_VOLUME_PROMPT       EQU  1288    ; enter volume name - format
MSG_FMT_DRIVE_LOCKED        EQU  1289    ; drive locked - format
MSG_FMT_NO_SYS_TRANS        EQU  1290    ; system cannot transfer
MSG_FMT_DOS_DISK_ERR        EQU  1291    ; dos disk error - format
MSG_FMT_NONSYS_DISK         EQU  1292    ; non-system disk - format
MSG_FMT_FORMATTING          EQU  1293    ; formatting ...  - format
MSG_FMT_COMPLETE            EQU  1294    ; format complete - format
MSG_FMT_WP_VIOLATION        EQU  1295    ; write protect violation - format
MSG_FMT_NO_WRITE_BOOT       EQU  1296    ; unable to write boot - format
MSG_FMT_INCOMPAT_DISK       EQU  1297    ; incompatible parms for disk - format
MSG_FMT_PARM_INCOMPAT       EQU  1298    ; incompatible parameter - format
MSG_FMT_DRV_NOT_READY       EQU  1299    ; drive not ready - format
MSG_FMT_DISK_INCOMPAT       EQU  1300    ; incompatible disk - format
MSG_FMT_RESTARTED           EQU  1301    ; format restarted - format
MSG_FMT_HEAD_CYL            EQU  1302    ; head/cylinder - format
MSG_FMT_NOT_SUPPORTED       EQU  1303    ; format not supported - format
MSG_FMT_INV_DEV_PARM        EQU  1304    ; invalid device parms - format
MSG_FMT_IOCTL_ERROR         EQU  1305    ; error in ioctl call - format
MSG_FMT_FIX_NOT_BOOT        EQU  1306    ; fixed disk will not boot run fdisk
MSG_FMT_FAT_ERROR           EQU  1307    ; error writing fat - format
MSG_FMT_DIR_WRTERR          EQU  1308    ; error writing directory - format
MSG_FMT_DRIVE_LETTER        EQU  1310    ; drive letter must be specified
MSG_FMT_SYS_FILES           EQU  1311    ; cannot find system files - format
MSG_FMT_OPEN_FILES          EQU  1312    ; too many open files - format
MSG_FMT_BAD_PARTITION       EQU  1313    ; bad partition table - format
MSG_FMT_TABLE_READ          EQU  1314    ; error reading partition table- forma
MSG_FMT_TABLE_WRITE         EQU  1315    ; error writing partition table- format
MSG_FMT_UNSUPP_PARMS        EQU  1316    ; parameters not supported - format
MSG_FMT_WHAT_IS_VOLID       EQU  1318    ; enter current volume label for dr %1
MSG_FMT_BAD_VOLID           EQU  1319    ; incorrect volume label for drive %1
MSG_FMT_INTERR              EQU  1320    ; format interrupted
MSG_FMT_TRANSFER            EQU  1321    ; format transferred
MSG_FMT_HEAD                EQU  1322    ; head
MSG_FMT_CYLINDER            EQU  1323    ; cylinder
MSG_CHK_REAL_MODE_MEM_RPT   EQU  1327    ; real mode memory report
MSG_CHK_NON_CONT_BLOCKS     EQU  1328    ; non-contig blocks
MSG_CHK_SPEC_FILE_CONT      EQU  1329    ; specified files are contig
MSG_CHK_INVALID_PARM        EQU  1330    ; invalid parameter - chkdsk
MSG_CHK_DISK_OPEN_ERR       EQU  1331    ; cannot open disk error - chkdsk
MSG_CHK_DISK_LOCK_ERR       EQU  1332    ; cannot lock disk error - chkdsk
MSG_CHK_INVALID_DRIVE       EQU  1333    ; invalid drive specification - chkdsk
MSG_CHK_FAT_READ            EQU  1336    ; error reading FAT - chkdsk
MSG_CHK_FAT_WRITE           EQU  1337    ; error writing FAT - chkdsk
MSG_CHK_DIRECTORY           EQU  1338    ; directory - chkdsk
MSG_CHK_NO_SLASHF           EQU  1339    ; errors found, no /F - chkdsk
MSG_CHK_INV_CLUSTER         EQU  1340    ; invalid cluster - chkdsk
MSG_CHK_INV_CURDIR          EQU  1341    ; invalid current dir - chkdsk
MSG_CHK_ALLOC_ERR           EQU  1342    ; allocation error - chkdsk
MSG_CHK_CROSSLINK           EQU  1343    ; files cross linked - chkdsk
MSG_CHK_1ST_CLUSTER         EQU  1344    ; first cluster bad - chkdsk
MSG_CHK_UNREC_DIRERR        EQU  1345    ; unrecoverable dir error - chkdsk
MSG_CHK_CONVERT             EQU  1346    ; conver directory? - chkdsk
MSG_CHK_DIR_EMPTY           EQU  1347    ; directory empty - chkdsk
MSG_CHK_INV_SUBDIR          EQU  1349    ; invalid subdirectory - chkdsk
MSG_CHK_NO_REC_DOT          EQU  1350    ; can't recover . entry - chkdsk
MSG_CHK_NO_REC_DOTDOT       EQU  1351    ; can't recover .. entry - chkdsk
MSG_CHK_BAD_LINK            EQU  1352    ; bad link - chkdsk
MSG_CHK_BAD_ATTRIB          EQU  1353    ; bad file attribute - chkdsk
MSG_CHK_BAD_SIZE            EQU  1354    ; bad file size - chkdsk
MSG_CHK_NOT_EXIST           EQU  1355    ; file does not exist - chkdsk
MSG_CHK_LOST_CLUSTERS       EQU  1356    ; lost clusters - chkdsk
MSG_CHK_SPACE_FREED         EQU  1358    ; space freed - chkdsk
MSG_CHK_SPACE_POSSIBLE      EQU  1359    ; space available - chkdsk
MSG_CHK_NOROOM_ROOT         EQU  1360    ; no room in root - chkdsk
MSG_CHK_DISK_TOTAL          EQU  1361    ; total space on disk - chkdsk
MSG_CHK_BAD_SECTORS         EQU  1362    ; bad sectors - chkdsk
MSG_CHK_HIDDEN_TOTAL        EQU  1363    ; hidden file total - chkdsk
MSG_CHK_DIR_TOTAL           EQU  1364    ; directory total - chkdsk
MSG_CHK_FILE_TOTAL          EQU  1365    ; file total - chkdsk
MSG_CHK_RECOVER_TOTAL       EQU  1366    ; recovered total - chkdsk
MSG_CHK_BYTES_POSSIBLE      EQU  1367    ; bytes possible - chkdsk
MSG_CHK_BYTES_AVAIL         EQU  1368    ; bytes available - chkdsk
MSG_CHK_PROCESS_STOP        EQU  1373    ; can't continue - chkdsk
MSG_CHK_FAT_BAD             EQU  1374    ; bad fat - chkdsk
MSG_CHK_VOL_CREATE          EQU  1375    ; can't create volume - chkdsk
MSG_CHK_ERR_WRITE_DIR       EQU  1376    ; can't write directory - chkdsk
MSG_CHK_ROOT_BAD_DRV        EQU  1377    ; root directory bad - chkdsk
MSG_CHK_NON_DOS             EQU  1379    ; probable non-dos disk - chkdsk
MSG_CHK_ERR_READ_DIR        EQU  1380    ; can't read dir - chkdsk
MSG_SAD_DUMPING             EQU  1381    ; dump in process
MSG_SAD_INSERT_NEXT         EQU  1382    ; current dump diskette filled
MSG_SAD_DISK_ERROR          EQU  1383    ; error with diskette, insert another
MSG_SAD_REINSERT            EQU  1384    ; insert dump diskette #1
MSG_SAD_DISK_OVERWRITE      EQU  1385    ; disk will be overwritten - createdd
MSG_SAD_API_ERR             EQU  1386    ; api error - createdd
MSG_SAD_INV_DRIVE           EQU  1388    ; invalid drive parm - createdd
MSG_SAD_HIGH_CAPACITY       EQU  1389    ; high capacity diskette
MSG_SAD_PRO_TERM_USER       EQU  1390    ; progm is end by user - createdd
MSG_SAD_COMPLETE            EQU  1391    ; dump complete remove and reboot
MSG_SAD_NOT_CONTIG          EQU  1392    ; not enough contiguous - createdd
MSG_SAD_MEM_RANGE           EQU  1393    ; memory address ranges on this disk ar
MSG_SAD_INSERT_DUMP         EQU  1395    ; insert dump disk - rasmsg
MSG_SAD_INSERT_NEW          EQU  1396    ; insert new dump disk - rascrv
MSG_STCP_NOT_EXIST          EQU  1397    ; sys trace does not exist - trace
MSG_STCP_INV_ON_OFF         EQU  1399    ; invalid or missing on/off - trace
MSG_STCP_EC_RANGE           EQU  1400    ; out of range event code - trace
MSG_STCP_EC_INVALID         EQU  1401    ; event codes invalid - trace
MSG_DFMT_BAD_INPUT          EQU  1402    ; incorrect starting or ending address
MSG_DFMT_DRV_PROMPT         EQU  1403    ; drive prompt - dumpfmtr
MSG_DFMT_BAD_DRIVE          EQU  1404    ; invalid drive specd - dumpfmtr
MSG_DFMT_RETRY_PROMPT       EQU  1405    ; retry/abort prompt - dumpfmtr
MSG_DFMT_ENTER              EQU  1406    ; enter to continue - dumpfmtr
MSG_DFMT_INS_DISK           EQU  1407    ; insert diskette - dumpfmtr
MSG_DFMT_OPEN_ERROR         EQU  1408    ; error on open - dumpfmtr
MSG_DFMT_READ_ERROR         EQU  1409    ; error on read - dumpfmtr
MSG_DFMT_BAD_ADDRESS        EQU  1410    ; address table on disk 1 - dumpfmtr
MSG_DFMT_INS_LAST           EQU  1411    ; insert last successful disk - dumpfmtr
MSG_DFMT_IMPROP_DISK        EQU  1412    ; improper disk inserted - dumpfmtr
MSG_DFMT_INS_ADDRESS        EQU  1413    ; address table on insert bad -dumpfmtr
MSG_DFMT_ENTER_START        EQU  1415    ; enter starting address - dumpfmtr
MSG_DFMT_ENTER_END          EQU  1417    ; enter ending address - dumpfmtr
MSG_DFMT_FORMAT_DUMP        EQU  1418    ; formatted dump - dumpfmtr
MSG_DFMT_BAD_START          EQU  1419    ; improper starting address
MSG_DFMT_BAD_END            EQU  1420    ; improper ending address
MSG_DFMT_EMPTY_DRIVE        EQU  1421    ; cannot access drive properly
MSG_DFMT_DUP_LINES          EQU  1422    ; total duplicate lines - dumpfmtr
MSG_SAD_INS_PROPER          EQU  1423    ; diskette error, ensure diskette inser
MSG_SAD_WRITE_PROTECT       EQU  1424    ; diskette error, write protect tab
MSG_SPOOL_WTRUNC_ERROR      EQU  1425    ; cannot release filling file - print
MSG_SPOOL_OPEN_ERROR        EQU  1426    ; cannot delete filling file - print
MSG_SPOOL_SIG_ERROR         EQU  1427    ; cannot delete printing file - print
MSG_SPOOL_MACH_ERROR        EQU  1428    ; cannot hold filling file - print
MSG_SPOOL_RUN_PRINT_DATA    EQU  1429    ; spool running with print data redir
MSG_SPOOL_RUN_DEVICE        EQU  1430    ; spool print run on device
MSG_SPOOL_INV_PARA          EQU  1431    ; spooler invalid parameter
MSG_SPOOL_INV_DEVICE        EQU  1432    ; spool invalid device
MSG_SPOOL_INV_INPAR         EQU  1433    ; spool inval input device para
MSG_SPOOL_INV_OUTPAR        EQU  1434    ; spool inval output device para
MSG_SPOOL_INV_DIRPARM       EQU  1435    ; spool inval subdirectory
MSG_SPOOL_INT_ERROR         EQU  1436    ; spool invernal error
MSG_SPOOL_DISK_FULL         EQU  1437    ; spool disk full
MSG_SPOOL_CURR_CAN_OPER     EQU  1438    ; spooler current cancelled by operator
MSG_SPOOL_ALL_CAN_OPER      EQU  1439    ; spooler cancelled by operator
MSG_SPOOL_DATA_DISCARD      EQU  1440    ; spooler has discarded data
MSG_SPOOL_SHAR_VIOLAT       EQU  1441    ; spool share violation
MSG_SPOOL_ALREADY_RUN       EQU  1442    ; spool is already running for device
MSG_SPOOL_DISK_IS_FULL      EQU  1443    ; spool disk is full action different
MSG_SPOOL_DK_FULL           EQU  1444    ; spool disk is full action different
MSG_INST_DCOPY_ERROR        EQU  1445    ; diskcopy error - install
MSG_INST_CRTDD_ERROR        EQU  1446    ; createdd error - install
MSG_INST_FDISK_ERROR        EQU  1447    ; fdisk error - install
MSG_INST_FORMAT_ERROR       EQU  1448    ; format error - install
MSG_INST_SYS_ERROR          EQU  1449    ; sys error - install
MSG_INST_UNRECOVER          EQU  1450    ; unrecoverable error - install
MSG_INST_COPY_ERROR         EQU  1451    ; copy error - install
MSG_KEY_LANG_NOT_SUPP       EQU  1454    ; language not supported
MSG_SYS_INV_MEDIA           EQU  1455    ; invalid target media
MSG_SYS_NO_ROOM             EQU  1456    ; no room for system on disk - sys
MSG_SYS_BAD_TRANS           EQU  1457    ; could not transfer files - sys
MSG_SYS_NO_SYSTEM           EQU  1458    ; no system on default drive - sys
MSG_SYS_UNSUITABLE          EQU  1459    ; disk unsuitable for system - sys
MSG_SYS_INV_PARM            EQU  1460    ; invalid parameter - sys
MSG_SYS_INV_DRIVE           EQU  1461    ; invalid drive - sys
MSG_SYS_TRANSFERRED         EQU  1462    ; system transferred - sys
MSG_VDISK_INV_PARM          EQU  1465    ; invalid parameter - vdisk
MSG_VDISK_INSUFF_MEM        EQU  1466    ; insufficient memory - vdisk
MSG_VDISK_REPORT            EQU  1467    ; vdisk summary - vdisk
MSG_SES_MGR_TERM            EQU  1468    ; session manager terminate ignore
MSG_SES_MGR_MENU            EQU  1469    ; session manager menu
;
; 1470-1474 are for swapper and are in basemid.inc
;
MSG_TREE_INV_PARM           EQU  1475    ; invalid parameter
MSG_TREE_INV_DRIVE          EQU  1476    ; invalid drive specification
MSG_TREE_INV_PATH           EQU  1478    ; invalid path
MSG_TREE_TOP_HEADER         EQU  1479    ; DIRECTORY PATH LISTING
MSG_TREE_PATHNAME           EQU  1480    ; Path: %1
MSG_TREE_SUBDIR_HEADER      EQU  1481    ; Sub-directories:  %1
MSG_TREE_FILE_HEADER        EQU  1482    ; Filies:           %1
MSG_TREE_NONE_FILE          EQU  1483    ; Filies:           None
MSG_TREE_NONE_SUBDIR        EQU  1484    ; Sub-directories:  None
MSG_TREE_FILENAME           EQU  1485    ;                   %1
MSG_TREE_NO_SUBDIR_EXIST    EQU  1486    ; No sub-directories exist
MSG_HELP_BAD_MID            EQU  1487    ; Invalid message id
MSG_HELP_MID_LARGE          EQU  1488    ; Message id too large
MSG_HELP_SYNTAX             EQU  1489    ; Syntax error
MSG_FIND_FILE_NOT_FOUND     EQU  1490    ; FIND file not found %1
MSG_SYNTAX_ERR_FIND         EQU  1491    ; FIND syntax error
MSG_INVALID_P_NUM           EQU  1492    ; invalid number of parameters
MSG_HELP_NO_HELP            EQU  1493    ; No help available
MSG_READ_ERROR_FIND         EQU  1494    ; FIND read error
MSG_INVALID_PARM_FIND       EQU  1495    ; FIND invalid parameter
MSG_TOO_MANY_OPEN_FIND      EQU  1496    ; too many open files
MSG_PATH_NOT_FIND           EQU  1497    ; path not found
MSG_INVALID_ACCESS_FIND     EQU  1498    ; invalid access to
MSG_SHAR_BUF_EXD_FIND       EQU  1499    ; sharing buffer exceeded
;
; 1505 - 1508 unused
MSG_INVALID_DR_LABEL        EQU  1509    ; invalid drive specification %1
; 1510- 1511 unused
MSG_INVALID_LABEL           EQU  1512    ; inval charact. in volume label
; 1513 unused
MSG_HAS_NO_LABEL            EQU  1514    ; volume in drive x has no label
MSG_GET_NEW_LABEL           EQU  1515    ; volume label (11 char) ent for none %
MSG_DR_VOL_LABEL            EQU  1516    ; volume in drive X is
; 1517 - 1523 in basemid.inc
MSG_PRINT_INV_PARM          EQU  1525    ; invalid command line parameter
MSG_PRINT_TOO_MANY          EQU  1526    ; too many command line param entered
MSG_PRINT_READ_ERROR        EQU  1528    ; error occurred reading file to print
MSG_PRINT_INV_DEVICE        EQU  1529    ; invalid printer device
MSG_PRINT_WRITE_ERROR       EQU  1530    ; error occurred on the printer
MSG_PRINT_NO_SPOOL          EQU  1531    ; spooler not running
MSG_PRINT_REAL_MODE         EQU  1532    ; spooler runs only in protect mode
MSG_PRINT_FILE_NOT_FOUND    EQU  1533    ; file not found
MSG_FDISK_MAIN_INTRO        EQU  1534    ; fdisk setup
MSG_FDISK_MAIN_MENU         EQU  1535    ; FDISK options
MSG_FDISK_MAIN_NEXTFDISK    EQU  1536    ; 5. Select next fixed disk drive
MSG_FDISK_MAIN_PROMPT       EQU  1537    ; Enter choice:
MSG_FDISK_CHANGE_CANTACT    EQU  1538    ; table partition can't be made active
MSG_FDISK_CHANGE_CURACT     EQU  1539    ; current active partition is %1
MSG_FDISK_CHANGE_DONE       EQU  1540    ; partition %1 made active
MSG_FDISK_CHANGE_NOPART     EQU  1541    ; No partitions to make active
MSG_FDISK_CHANGE_1PART      EQU  1542    ; Partition 1 is already active
MSG_FDISK_CHANGE_PROMPT     EQU  1543    ; Enter the number of the partition you
MSG_FDISK_CHANGE_TITLE      EQU  1544    ; change active partition
MSG_FDISK_CREATE_DONE       EQU  1545    ; DOS partition created
MSG_FDISK_CREATE_NOSPACE    EQU  1546    ; No space for a %1 partition
MSG_FDISK_CREATE_NOSPATHE   EQU  1547    ; No space for %1 cyclinder at cyl%2
MSG_FDISK_CREATE_NOSPAFDOS  EQU  1548    ; No space to create a DOS partition
MSG_FDISK_CREATE_PARTEXISTS EQU  1549    ; fixed disk already has a dos partit
MSG_FDISK_CREATE_SIZEPROMPT EQU  1550    ; Enter partition size...
MSG_FDISK_CREATE_SPACEAT    EQU  1551    ; Max avail space is %1 cycl at %2.
MSG_FDISK_CREATE_STARTPROMP EQU  1552    ; enter starting cylinder number...
MSG_FDISK_CREATE_TITLE      EQU  1553    ; create DOS partition
MSG_FDISK_CREATE_WHOLEDISK  EQU  1554    ; Do you want to use the entire fd y/n
MSG_FDISK_DELETE_DONE       EQU  1555    ; DOS partition deleted
MSG_FDISK_DELETE_NOPART     EQU  1556    ; No DOS partition to delete
MSG_FDISK_DELETE_PROMPT     EQU  1557    ; Warning! Data in the DOS part lost
MSG_FDISK_DELETE_TITLE      EQU  1558    ; Delete DOS partition yn
MSG_FDISK_DISPLAY_FDISKSIZE EQU  1559    ; Total disk space is %1 cyliners.
MSG_FDISK_DISPLAY_NOPARTS   EQU  1560    ; No partitions defined
MSG_FDISK_DISPLAY_PARTINFO  EQU  1561    ; Partition status type start endsize
MSG_FDISK_DISPLAY_TITLE     EQU  1562    ; Display partition information
MSG_FDISK_ERR_INVALIDNUMBER EQU  1563    ; %1 is not a choice.  Please enter a c
MSG_FDISK_ERR_INVALIDYN     EQU  1564    ; %1 is not a choice.  Please enter yn
MSG_FDISK_ERR_NOFDISKS      EQU  1565    ; no fixed disks present
MSG_FDISK_ERR_READ          EQU  1566    ; error reading fixed disk
MSG_FDISK_ERR_WRITE         EQU  1567    ; error writing fixed disk
MSG_FDISK_CURRENTDISK       EQU  1568    ; current fixed disk drive %1
MSG_FDISK_PRESSESC          EQU  1569    ; press esc to return to fdisk options
MSG_FDISK_MUST_RESTART      EQU  1570    ; must restart
MSG_FDISK_IN_USE            EQU  1571    ; fixed disk in use
; 1572 - 1574 unused
MSG_PATCH_INV_NUM_PARMS     EQU  1575     ;invalid number of parameters
MSG_PATCH_INV_PARM          EQU  1576    ; invalid parameter %1%2
MSG_PATCH_NO_CTL            EQU  1577    ; cannot open patch control file %1
MSG_PATCH_NO_EXE_FILE       EQU  1578    ; cannot open %1 to patch
MSG_PATCH_PATCHING          EQU  1579    ; patching %1
MSG_PATCH_CONTINUE          EQU  1580    ; continue patching %1? Y/N
MSG_PATCH_NO_PATCHES        EQU  1581    ; No patches applied
MSG_PATCH_PATCHES_ENTERED   EQU  1582    ; Patches entered for %1
MSG_PATCH_OK_TO_PATCH       EQU  1583    ; should these patches be appl to %1?
MSG_PATCH_APPLIED           EQU  1584    ; patches applied to %1
MSG_PATCH_NOT_APPLIED       EQU  1585    ; No patches applied to %1
MSG_PATCH_CURRENT_EOF       EQU  1586    ; Current end of file is at %1
MSG_PATCH_OFFSET_PROMPT     EQU  1587    ; enter offset in hex of patch >
MSG_PATCH_PAST_EOF_INT      EQU  1588    ; offset is past end of file
MSG_PATCH_CANT_ALLOC        EQU  1589    ; Insufficient memory to save pat inf
MSG_PATCH_NO_FILE           EQU  1590    ; no file to patch fond in pat con %1
MSG_PATCH_INV_CMD_COMBO     EQU  1591    ; %1 command found without a %2 cmd
MSG_PATCH_INV_OFFSET        EQU  1592    ; %1 is not a valid offset for file %2
MSG_PATCH_INV_BYTES         EQU  1593    ; %1 is not a valid %2 byte stg for%3
MSG_PATCH_TOO_NEAR_EOF      EQU  1594    ; offset %1 for %2 too near orpst%3%4
MSG_PATCH_VERIFY_FAILED     EQU  1595    ; verification failed for %1
MSG_PATCH_INC_VERIFY        EQU  1596    ; incorrect verify bytes ofst %1in%2
MSG_PATCH_PAST_EOF_AUTO     EQU  1597    ; ofst given %1 ofr %2 eof %3
MSG_PATCH_UNKNOWN_CMD       EQU  1598    ; unknown command
MSG_PATCH_CANT_APPEND       EQU  1599    ; cannot append
MSG_PATCH_NO_REQUEST        EQU  1600    ; no patch request specified
MSG_MODE_INVAL_PARMS        EQU  1601    ; invalid number of parameters
MSG_MODE_CPL_SET            EQU  1602    ; characters per line has been set
MSG_MODE_LPI_SET            EQU  1603    ; lines per inch have been set
MSG_MODE_INF_RET_SET        EQU  1604    ; infinite has been set
MSG_MORE                    EQU  1605    ; -- More --
MSG_MODE_INF_RET_RESET      EQU  1606    ; infinite retry reset
MSG_MODE_DEVICE_NAME        EQU  1607    ; invalid device name
MSG_PRINT_ERROR             EQU  1608    ; printer error
MSG_MODE_ASYNC_SET          EQU  1609    ; async protocol set
MSG_MODE_INF_NOT_SUP        EQU  1610    ; infinite retry not support real mode
MSG_MODE_OPEN_ERROR         EQU  1612    ; open error on device
MSG_INVALID_PARITY          EQU  1613    ; open parity
MSG_INVALID_DATABITS        EQU  1614    ; invalid databits
MSG_INVALID_STOPBITS        EQU  1615    ; invalid stopbits
MSG_INVALID_BAUD_RATE       EQU  1616    ; invalid baud rate
MSG_INVALID_CHAR_PER_LINE   EQU  1617    ; invalid characters per line
MSG_INVALID_LINES_PER_INCH  EQU  1618    ; invalid lines per inch
MSG_COMM_PORT_ERROR         EQU  1619    ; communication port error
MSG_INVALID_SYNTAX          EQU  1620    ; invalid syntax
MSG_DISPLAY_MODE_ERROR      EQU  1621    ; display mode error
; 1623 - 1624 unused
MSG_INVAL_PARAMETER         EQU  1625    ; invalid parameter attrib
MSG_FILEN_REQUIRED          EQU  1626    ; filename required attrib
; 1627 unused
MSG_UNEX_DOS_ERROR          EQU  1628    ; unexpected dos error
; 1729 - 1633 unused
MSG_REST_FILE_FROM          EQU  1634    ; restore file from % 1 %2
MSG_INS_BACKUP_DISK         EQU  1635    ; insert backup %1 in drive %2
MSG_REST_DISK_OUT_SEQ       EQU  1636    ; diskette out of seq press key
MSG_REST_FILE_READ          EQU  1637    ; Warning file %1 read only
MSG_REST_CHNG_REPL          EQU  1638    ; file was changed after backup
MSG_REST_SYS                EQU  1639    ; system files restored
MSG_REST_FILE_BACKUP        EQU  1640    ; files were backed up
MSG_REST_FILENAME           EQU  1641    ; filename restored
MSG_REST_SOUR_TARG_SAME     EQU  1642    ; source and target are same
MSG_REST_NUM_INVAL_PARA     EQU  1643    ; invalid number of para restore
MSG_REST_INVAL_SPEC         EQU  1644    ; invalid drive
MSG_REST_NO_FILE_FOUND      EQU  1645    ; not files found to restore
MSG_REST_INVAL_PARA         EQU  1646    ; invalid parameter %1
MSG_REST_FILE_SHAR          EQU  1647    ; sharing conflict
MSG_REST_LAST_FILE_NOT      EQU  1648    ; last file not restored
MSG_REST_SOURCE_NO_BACK     EQU  1649    ; source does not contain backup files
MSG_REST_FILE_SEQ_ERROR     EQU  1651    ; sequence file out of sequence
MSG_REST_FILE_CREAT_ERROR   EQU  1652    ; file creation error
MSG_REST_TARG_FULL          EQU  1653    ; Target is full
MSG_REST_CANNOT_REST_FILE   EQU  1655    ; cannot restore file
MSG_REST_DISKETTE           EQU  1656    ; diskette %1
MSG_REST_TARG_DISK          EQU  1657    ; insert targ disk in drive %1
; 1658 - 1663 unused
MSG_BACK_ERROR_LOG_FILE     EQU  1664    ; error writing log file
MSG_BACK_NUM_INVAL_PARA     EQU  1665    ; inval number of parameters
MSG_BACK_INVAL_PARA         EQU  1666    ; inval parameter
MSG_BACK_NO_DRIV_SPEC       EQU  1667    ; no drive spec
MSG_BACK_NO_TARG_SPEC       EQU  1668    ; no target specified
MSG_BACK_INVAL_PATH         EQU  1670    ; inval path
MSG_BACK_SOUR_TARG_SAME     EQU  1671    ; source target same
MSG_BACK_ERR_FORMAT_UTIL    EQU  1672    ; error executing format utility
MSG_BACK_INVAL_DATE         EQU  1673    ; inval date
MSG_BACK_INVAL_TIME         EQU  1674    ; inval time
MSG_BACK_INVAL_DRIV_SPEC    EQU  1675    ; inval drive spec
MSG_BACK_CANNOT_FIND_FORM   EQU  1676    ; cannot find format util
MSG_BACK_ERROR_OPEN_LOGFILE EQU  1677    ; error open logfile
MSG_BACK_CAN_FORM_UNREM_DR  EQU  1678    ; cannot form unremovable drive
MSG_BACK_LAST_NOT_INSERT    EQU  1679    ; last disk not inserted
MSG_BACK_LOG_TO_FILE        EQU  1680    ; log to file %2
MSG_BACK_FILE_TARG_DRIVE    EQU  1681    ; warning file in targeg drive %2
MSG_BACK_FILE_BACK_ERASE    EQU  1682    ; files in target drive %c
MSG_BACK_FILE_TO_DRIVE      EQU  1683    ; backup up files to drive d
MSG_BACK_DISK_NUM           EQU  1684    ; diskette number %2
MSG_BACK_WARN_NO_FIL_FND    EQU  1685    ; warning no files were found to backup
MSG_BACK_INSERT_SOURCE      EQU  1686    ; insert backup source diskette in dr d
MSG_BACK_INSERT_BACKUP      EQU  1687    ; insert backup source disk %2 in dr
MSG_BACK_UNABLE_TO_BKUP     EQU  1688    ; not able to backup file
MSG_BACK_LAST_DISK_DRIVE    EQU  1689    ; insert last backup diskette in drive
MSG_BACK_TARG_NOT_USE_BKUP  EQU  1690    ; target cannot be used for backup
MSG_BACK_LAST_FILE_NO_BKUP  EQU  1691    ; last file not backed up
MSG_BACK_DEVICE_D_IS_FULL   EQU  1692    ; fixed bacup device d is full
MSG_XCOPY_UNAB_CREATE_DIR   EQU  1693    ; unable to create directory
MSG_XCOPY_PATH_TOO_LONG     EQU  1694    ; path too long
MSG_XCOPY_BLANK_YN          EQU  1695    ; blank y/n
MSG_XCOPY_CANNOT_TO_RES_DEV EQU  1696    ; cannot xcopy to a reserved device
MSG_XCOPY_READ_SOURCE_FILE  EQU  1697    ; reading source file
MSG_XCOPY_FILES_COPIED      EQU  1698    ; % files copied
MSG_XCOPY_FILE_NOT_FOUND    EQU  1699    ; % file not found
MSG_XCOPY_DOES_SPEC_FILENA  EQU  1700    ; Does % specify a filename
MSG_XCOPY_CANNOT_COPY_SUB   EQU  1701    ; cannot copy using /s to subdirectory
MSG_XCOPY_CANNOT_COPY_RES   EQU  1702    ; cannot copy from a reserved device
MSG_XCOPY_INVAL_PARA        EQU  1703    ; invalid parameter xcopy
MSG_XCOPY_INVAL_NUM_PAR     EQU  1704    ; invalid number of parameters xcopy
MSG_ANSI_EXT_SCR_KEY_ON     EQU  1705    ; ansi extended screen and keyboard on
MSG_ANSI_EXT_SCR_KEY_OFF    EQU  1706    ; ansi extended screen and keyboard off
MSG_ANSI_INVAL_PARA         EQU  1707    ; ansi invalid parameter
; 1708 - 1714 unused

