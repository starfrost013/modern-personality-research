echo off
rem
rem Microsoft Windows 1.0 Development Release.  11-1-84.
rem
echo #
echo # You will be installing the Microsoft Windows 1.0 Development Release #5
echo # The following equipment is required:
echo #
echo #             o IBM XT or AT
echo #             o 512 K memory.
echo #             o Floppy disk configured as DRIVE A:.
echo #             o Hard disk configured as DRIVE C:.
echo #             o Color graphics card.
echo #             o External console connected to COM1: ( debugging ).
echo #             o Microsoft mouse ( bus or serial ).
echo #             o Timer.
echo #
echo # The following software is required:
echo #
echo #             o 2.x or 3.0 MS-DOS
echo #
echo # If you do not have the equipment or software above, abort this batch file.
echo #
echo # This file should have been copied to your hard disk.  If not, then
echo # abort this batch file and read the README.DOC file on the first disk.
echo #
pause
echo #
if "%1"=="" goto g0
goto g1
:g0
cd c:\
echo # Your beta installation will take place in the 'c:\windows' directory.
echo # If this is not what you want, rerun this batch file with a directory
echo # parameter.
goto g2
:g1
cd %1
echo # Your beta installation will take place below the '%1\windows' directory.
:g2
echo #
pause
echo #
echo # The following directories will be updated.  This is a complete release,
echo # so any existing files with the same name will be replaced.  If you
echo # have such files, abort this batch file, back them up, and retry the
echo # installation.
echo #
echo # \lib
echo # %1\windows
echo # %1\windows\doc
echo # %1\windows\lib
echo # %1\windows\bin
echo # %1\windows\test
echo #
pause
echo #   Erasing all files from \lib directory
mkdir \lib
erase \lib\*.*
echo #
echo #   Erasing all files from %1\windows directory structure
echo #
mkdir %1\windows
mkdir %1\windows\doc
mkdir %1\windows\bin
mkdir %1\windows\test
erase %1\windows\doc\*.*
erase %1\windows\bin\*.*
erase %1\windows\test\*.*
erase %1\windows\lib\*.*
rmdir %1\windows\lib
echo #
echo #   Installation will take approximately 7 minutes.
echo #   Reading disk ( 1 of 3 ) in drive A: ( Windows 1.0 Release #5 )
echo #
pause
copy a:install.bat      %1\windows
copy a:remove.bat       %1\windows
copy a:readme.doc       %1\windows
copy a:*.exe            %1\windows\test
copy a:*.map            %1\windows\test
copy a:windows.fon      %1\windows\test
copy a:mb.sym           %1\windows\test
copy a:*.bin            %1\windows\test
copy a:*.cin            %1\windows\test
copy a:*.din            %1\windows\test
copy a:ibmcolor.cin     %1\windows\test\cursor.exe
copy a:ibmcolor.din     %1\windows\test\display.exe
copy a:*.res            %1\windows\test

echo #
echo #   Place disk ( 2 of 3 ) in drive A: ( Windows 1.0 Release #5 )
echo #
pause
copy a:*.*              %1\windows\test
echo #
echo #   Place disk ( 3 of 3 ) in drive A: ( Windows 1.0 Release #5 )
echo #
pause
copy a:*.lib        \lib
copy a:windows.h    \lib
copy a:cmacros.inc  \lib
copy a:*.doc        %1\windows\doc
copy a:*.exe        %1\windows\bin
rem  mc.bat and all .bat files
copy a:*.?at        %1\windows\bin
echo #
echo # Installation of windows complete
echo # For development work, you must install the C compiler release and
echo # the assembler and debugger, putting all libraries and include files
echo # in the \lib directory.
echo #
echo # About to change PATH to point to installed executables, in order
echo # to run Windows.
pause
path %1\windows\bin
cd %1\windows\test
echo #
echo # To run windows, type "mswin"
