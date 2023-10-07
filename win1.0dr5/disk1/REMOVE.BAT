echo off
rem
rem Microsoft windows 1.0 Development Release.  5-29-84.
rem
echo #
echo #  This batch file deletes the directories created by the installation
echo #  utility for the Microsoft windows 1.0 Development Release.  They are:
echo #
echo #  \lib
echo #  %1\windows
echo #      %1\windows\doc
echo #      %1\windows\bin
echo #      %1\windows\test
echo #
echo #  Abort this batch file if you do not wish to continue.
echo #
cd %1
pause
erase \lib\*.*
erase %1\windows\*.*
erase %1\windows\doc\*.*
erase %1\windows\bin\*.*
erase %1\windows\test\*.*
rmdir \lib
rmdir %1\windows\test
rmdir %1\windows\bin
rmdir %1\windows\doc
rmdir %1\windows
