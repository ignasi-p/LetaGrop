@echo off
echo Minimalist GNU for Windows with Fortran compiler
set PATH=C:\bin\mingw64\bin;%PATH%
rem echo %PATH%

REM the "obj" directory must exist
if NOT exist "obj%" (
    echo.creating folder "obj"
	mkdir "obj"
	if ERRORLEVEL 1 (echo.can NOT create folder. Terminating... & goto xit)
)

mingw32-make

pause
