# Running LetaGrop

The file `trunk_f90\Fortran_Windows.md` describes how to download and install
the [`MinGW64`](https://www.mingw-w64.org/) Fortran compiler for
64 bit Windows.

The file `trunk_f90\compile_and_test.md` describes how to build and test the
different LetaGrop versions.

LetaGrop is a [command-line](https://en.wikipedia.org/wiki/Command-line_interface)
Fortran program. The user provides the names for the input- and output-files.
While performing the calculations, output is written to both the
[Windows console](https://en.wikipedia.org/wiki/Windows_Console) and to the
output file.

## The input file

The input file contains the experimental data, often collected in "groups" or sets
of data, such as titrations or wavelengths.

Instructions are then given in the input file on how to fit the
[stability constants of complexes](https://en.wikipedia.org/wiki/Stability_constants_of_complexes)
to the experimental data. These instructions are referred to as "dagens spaning"
(Swedish: today's scouting; from a time when a calculation was performed in a computer
central once a day by submitting [punched cards](https://en.wikipedia.org/wiki/Punched_card),
and collecting the output as printed sheets, perhaps just to find that you had misplaced
a comma, and had to submit the job again next day).

Some times the speciation is not known, that is, one does not know what complexes
are formed by the equilibrium reactions. In such a case different sets of reactions
are fitted to the experimental data.

The instructions in the input file consist of control numbers (called _Rurik_ numbers)
often followed by additional parameters.

A list of _Rurik_ numbers and other data may be found in folder `Docs`,
files `RURIK.txt` and `RURIK.doc`.
