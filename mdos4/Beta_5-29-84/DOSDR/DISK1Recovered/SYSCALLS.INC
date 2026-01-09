BREAK <system call definitions>

;***	SYSCALLS.INC - System Call Definitions


ABORT				EQU 0	;  0	  0
STD_CON_INPUT			EQU 1	;  1	  1
STD_CON_OUTPUT			EQU 2	;  2	  2
STD_AUX_INPUT			EQU 3	;  3	  3
STD_AUX_OUTPUT			EQU 4	;  4	  4
STD_PRINTER_OUTPUT		EQU 5	;  5	  5
RAW_CON_IO			EQU 6	;  6	  6
RAW_CON_INPUT			EQU 7	;  7	  7
STD_CON_INPUT_NO_ECHO		EQU 8	;  8	  8
STD_CON_STRING_OUTPUT		EQU 9	;  9	  9
STD_CON_STRING_INPUT		EQU 10	; 10	  A
STD_CON_INPUT_STATUS		EQU 11	; 11	  B
STD_CON_INPUT_FLUSH		EQU 12	; 12	  C
DISK_RESET			EQU 13	; 13	  D
SET_DEFAULT_DRIVE		EQU 14	; 14	  E
FCB_OPEN			EQU 15	; 15	  F
FCB_CLOSE			EQU 16	; 16	 10
DIR_SEARCH_FIRST		EQU 17	; 17	 11
DIR_SEARCH_NEXT			EQU 18	; 18	 12
FCB_DELETE			EQU 19	; 19	 13
FCB_SEQ_READ			EQU 20	; 20	 14
FCB_SEQ_WRITE			EQU 21	; 21	 15
FCB_CREATE			EQU 22	; 22	 16
FCB_RENAME			EQU 23	; 23	 17
GET_DEFAULT_DRIVE		EQU 25	; 25	 19
SET_DMA				EQU 26	; 26	 1A
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;	     C	A  V  E	 A  T	  P  R	O  G  R	 A  M  M  E  R		   ;
;									   ;
GET_DEFAULT_DPB			EQU 31	; 31	 1F
;									   ;
;	     C	A  V  E	 A  T	  P  R	O  G  R	 A  M  M  E  R		   ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
FCB_RANDOM_READ			EQU 33	; 33	 21
FCB_RANDOM_WRITE		EQU 34	; 34	 22
GET_FCB_FILE_LENGTH		EQU 35	; 35	 23
GET_FCB_POSITION		EQU 36	; 36	 24
SET_INTERRUPT_VECTOR		EQU 37	; 37	 25
CREATE_PROCESS_DATA_BLOCK	EQU 38	; 38	 26
FCB_RANDOM_READ_BLOCK		EQU 39	; 39	 27
FCB_RANDOM_WRITE_BLOCK		EQU 40	; 40	 28
PARSE_FILE_DESCRIPTOR		EQU 41	; 41	 29
GET_DATE			EQU 42	; 42	 2A
SET_DATE			EQU 43	; 43	 2B
GET_TIME			EQU 44	; 44	 2C
SET_TIME			EQU 45	; 45	 2D
SET_VERIFY_ON_WRITE		EQU 46	; 46	 2E
; Extended functionality group
GET_DMA				EQU 47	; 47	 2F
GET_VERSION			EQU 48	; 48	 30
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;	     C	A  V  E	 A  T	  P  R	O  G  R	 A  M  M  E  R		   ;
;									   ;
GET_DPB				EQU 50	; 50	 32
;									   ;
;	     C	A  V  E	 A  T	  P  R	O  G  R	 A  M  M  E  R		   ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
SET_CTRL_C_TRAPPING		EQU 51	; 51	 33
GET_INDOS_FLAG			EQU 52	; 52	 34
GET_INTERRUPT_VECTOR		EQU 53	; 53	 35
GET_DRIVE_FREESPACE		EQU 54	; 54	 36
CHAR_OPER			EQU 55	; 55	 37
INTERNATIONAL			EQU 56	; 56	 38
; XENIX CALLS
;   Directory Group
MKDIR				EQU 57	; 57	 39
RMDIR				EQU 58	; 58	 3A
CHDIR				EQU 59	; 59	 3B
;   File Group
CREAT				EQU 60	; 60	 3C
OPEN				EQU 61	; 61	 3D
CLOSE				EQU 62	; 62	 3E
READ				EQU 63	; 63	 3F
WRITE				EQU 64	; 64	 40
UNLINK				EQU 65	; 65	 41
LSEEK				EQU 66	; 66	 42
CHMOD				EQU 67	; 67	 43
IOCTL				EQU 68	; 68	 44
XDUP				EQU 69	; 69	 45
XDUP2				EQU 70	; 70	 46
CURRENT_DIR			EQU 71	; 71	 47
;    Memory Group
ALLOC				EQU 72	; 72	 48
DEALLOC				EQU 73	; 73	 49
SETBLOCK			EQU 74	; 74	 4A
;    Process Group
EXEC				EQU 75	; 75	 4B
WAIT				EQU 77	; 77	 4D
FIND_FIRST			EQU 78	; 78	 4E
;   Special Group
FIND_NEXT			EQU 79	; 79	 4F
; SPECIAL SYSTEM GROUP
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;	     C	A  V  E	 A  T	  P  R	O  G  R	 A  M  M  E  R		   ;
;									   ;
SET_CURRENT_PDB			EQU 80	; 80	 50
GET_CURRENT_PDB			EQU 81	; 81	 51
GET_IN_VARS			EQU 82	; 82	 52
SETDPB				EQU 83	; 83	 53
;									   ;
;	     C	A  V  E	 A  T	  P  R	O  G  R	 A  M  M  E  R		   ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
GET_VERIFY_ON_WRITE		EQU 84	; 84	 54
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;	     C	A  V  E	 A  T	  P  R	O  G  R	 A  M  M  E  R		   ;
;									   ;
DUP_PDB				EQU 85	; 85	 55
;									   ;
;	     C	A  V  E	 A  T	  P  R	O  G  R	 A  M  M  E  R		   ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
RENAME				EQU 86	; 86	 56
FILE_TIMES			EQU 87	; 87	 57
AllocOper			EQU 88	; 88	 58
; Network extention system calls
GetExtendedError		EQU 89	; 89	 59
CreateTempFile			EQU 90	; 90	 5A
CreateNewFile			EQU 91	; 91	 5B
LockOper			EQU 92	; 92	 5C Lock and Unlock
ServerCall			EQU 93	; 93	 5D CommitAll, ServerDOSCall,
					;	    CloseByName, CloseUser,
					;	    CloseUserProcess,
					;	    GetOpenFileList
UserIDOper			EQU 94	; 94	 5E Get and Set
AssignOper			EQU 95	; 95	 5F On, Off, Get, Set, Cancel

; New 3.0 multitasking calls
;
FreezeProcess			EQU 96	; 96	 60
ThawProcess			EQU 97	; 97	 61
Sleep				EQU 98	; 98	 62
KillProcess			EQU 99	; 99	 63
Priority			EQU 100 ; 100	 64
GetGlobalVector			EQU 101 ; 101	 65
SetGlobalVector			EQU 102 ; 102	 66
GetLock				EQU 103 ; 103	 67
SchedParam			EQU 104 ; 104	 68
FinalArena			EQU 105 ; 105	 69

OEM_C1				EQU 249 ; 249	 F9
OEM_C2				EQU 250 ; 250	 FA
OEM_C3				EQU 251 ; 251	 FB
OEM_C4				EQU 252 ; 252	 FC
OEM_C5				EQU 253 ; 253	 FD
OEM_C6				EQU 254 ; 254	 FE
OEM_C7				EQU 255 ; 255	 FF