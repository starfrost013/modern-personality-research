rem 	*** MS REGRESSION TESTS ***
diskinfo %1
infoseg %1
vm %1
rem if stderr (handle 2) is redirected to a file, testqht will fail
testall %1
del longwrit.txt
del \zz.zz
ft1 ft*.exe %1
ft3 ft*.exe %1
exectest %1
switch
dynmain
diropen a: %1
dirftest a: %1
dirbwrt a: %1
rem	*** END OF TEST ***