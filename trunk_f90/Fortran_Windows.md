# Fortran on Windows 10

Text inspired by: https://github.com/WataruTakahagi/SUPCRT_win10

-----------------------------------------------------------

Use
[`gfortran`](https://en.wikipedia.org/wiki/GNU_Fortran),
a free Fortran compiler environment for Windows in [`MinGW64`](https://www.mingw-w64.org/) for 64 bit Windows. See also:  
https://gcc.gnu.org/fortran/  
https://gcc.gnu.org/wiki/GFortran  
https://gcc.gnu.org/onlinedocs/gfortran/index.html  
https://github.com/WataruTakahagi/SUPCRT_win10  

## Download and Install

[`MinGW64`](https://www.mingw-w64.org/downloads/) points to
both [`MSYS2`](https://www.msys2.org/) and to
[`Mingw-builds-binaries`](https://github.com/niXman/mingw-builds-binaries).  
We will use [`Mingw-builds-binaries`](https://github.com/niXman/mingw-builds-binaries).

**Alternative 1:** Go to [`Mingw-builds-binaries`](https://github.com/niXman/mingw-builds-binaries/releases)
where several different install packages are listed.
The _sjlj_, _dwarf_, _seh_, etc, are different exception handling
systems. The _msvcrt_ and _ucrt_ are two variants of the
C standard library in Windows.

Download (for example) the file:  
"`x86_64-14.2.0-release-posix-seh-ucrt-rt_v12-rev0.7z"`.

Extract the file with [`7-Zip`](https://www.7-zip.org/) to a
folder such as "`C:\bin`".

**Alternative 2:** Get the online installer
"**`mingwInstaller.exe`**" from [`Mingw-builds-binaries`](https://github.com/niXman/mingw-builds-binaries).
Run the installer. Select: _64bit_,
[`posix`](https://en.wikipedia.org/wiki/POSIX),
the _latest_ revision, _ucrt_ (as C-runtime library), a
folder for the instalation (such as "`C:/bin/`") and click
`Process` to start downloading, and then `Finish`.

### Create a command (batch) file

Create a file with a text editor named "**`mingw64.cmd`**" in
any folder you like (such as `C:\bin`) with the following
contents:    

```
@echo off  
echo Minimalist GNU for Windows-64 with Fortran compiler  
set PATH=C:\...\mingw64\bin;%PATH%
rem cd "C:\"
"C:\WINDOWS\system32\cmd.exe"
  
```  

where "`C:\...\mingw64`" is the installation folder.
You can copy this file to any directory where you want to
compile Fortran programs.

## To compile a program

Double-click on the file "**`mingw64.cmd`**" that you created
above. The Fortran window opens.

```
>gfortran
```

Hit enter

```
gfortran: fatal error: no input files
```

If so, it's OK!

To use "`make`", create a `makefile` and type  

```
>mingw32-make
```

## To un-install

Just delete the installation folder.
