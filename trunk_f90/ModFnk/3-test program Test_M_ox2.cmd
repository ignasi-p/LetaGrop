@echo off
echo Minimalist GNU for Windows with Fortran compiler
set PATH=C:\bin\mingw64\bin;%PATH%
rem echo %PATH%

mingw32-make test

echo 0.7682 > deleteme.
echo 0.4097 >> deleteme.

echo.
echo "Test_M_ox2.exe" with [HNO3]tot = 0.7682  and  [H2ox]tot = 0.4097
Test_M_ox2.exe < deleteme.
del deleteme.
pause
