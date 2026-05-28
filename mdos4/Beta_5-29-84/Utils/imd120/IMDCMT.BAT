@echo OFF
if .%2 == . goto help
dmk2imd nul o=%1 /c%2
goto quit
:help
echo.
echo Use: IMDCMT imdfile commentfile
echo.
echo Creates an empty .IMD file with a preset comment from a file.
echo You can 'R'ead on-top of this file with ImageDisk to have the
echo comment preset in the ImageDisk comment entry field.
echo.
echo Dave Dunfield - Feb 6, 2012
echo.
:quit
