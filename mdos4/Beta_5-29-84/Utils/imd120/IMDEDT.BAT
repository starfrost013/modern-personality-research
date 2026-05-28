@echo off
if .%1 == . goto help
imdu %1.IMD ec=%TEMP%$IMDTMP$.TXT
if errorlevel 1 goto quit
rem *** Replace EDT in the line below with your favorite editor ***
edt %TEMP%$IMDTMP$.TXT
imdu %1 %TEMP%$IMDTMP$.IMD rc=%TEMP%$IMDTMP$.TXT
if errorlevel 1 goto quit
copy %TEMP%$IMDTMP$.IMD %1.IMD
del %TEMP%$IMDTMP$.IMD
del %TEMP%$IMDTMP$.TXT
goto quit
:help
echo.
echo Use: IMDEDT imdfile
echo.
echo Extracts, Edits and Replaces the comment from a .IMD file.
echo.
echo Environment variable TEMP can be used to place temporary files in
echo a specific location (I use a RAMdisk). It is prepended as a raw
echo prefix to the filenames, so it should include the trailing '\' if
echo it specifies a directory - eg: set TEMP=R:\
echo.
echo If you are NOT using my EDT editor - edit this file and change the
echo editor command - look for the '***' comment.
echo.
echo Do NOT specify the .IMD extension on the imdfile argument.
echo.
echo Dave Dunfield - Feb 6, 2012
echo.
:quit
