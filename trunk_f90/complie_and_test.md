# LetaGrop - Compile and Test

The file `Fortran_Win10.md` describes how to download and install
the [`MinGW64`](https://www.mingw-w64.org/) Fortran compiler for
64 bit Windows. It is expected that the installation folder
for the [GNU compiler collection](https://en.wikipedia.org/wiki/GNU_Compiler_Collection)
is `C:\bin\mingw64`. If not, you will need to
modify the `*.cmd` files with a text editor.

The source Fortran code and test files are found in folder `trunk_f90`.
Several sub-folders contain the different versions of LetaGrop:

- `NyTit` for data from potentiometric titrations.
- `Spefo` for data from spectrophotometry.
- `Distr` for data from solvent extraction.
- `ModFnk` (Model Functions) allows to fit model parameters to
experimental data for a user-programmed function.

Each of these sub-folders contains two files to create the Windows
binary (executable) file: `1-compile.cmd` and `makefile`.
Running `1-compile.cmd` will create the executable file based on the instructions
in `makefile`.

Each of these sub-folders also contains a file named `2-test.cmd` that
will run a test of the corresponding LetaGrop version (NyTit, Spefo, etc).

The sub-folder `trunk_f90\LetaGrop` contains the Fortran code that is common
to all LetaGrop versions.

To [build](https://en.wikipedia.org/wiki/Software_build) a specific version
of LetaGrop, let us say **Spefo**, go to the folder `trunk_f90\Spefo` and double-click
on "`1-compile.bat`". This will open a [command prompt](https://en.wikipedia.org/wiki/Cmd.exe)
in a [Windows console](https://en.wikipedia.org/wiki/Windows_Console) where the
build process takes place.

To test the programs, double-click on the file `2-test.cmd` found in each sub-folder.
This will run the program in a Windows console, and output information to both the
console and to the output file.