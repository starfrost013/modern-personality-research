rem	SCCSID = @(#)ibmrgrs.bat	8.1 86/09/20
rem	execute ibm cp/dos regression tests

datetime

curdisk

dosfind *.exe

dosmkdir testdir

dosio1 testfile

msptest

swapregr

ironment