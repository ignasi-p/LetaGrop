# LetaGrop

**LetaGrop** (Swedish: search for a pit, or hollow) is a family of old
[command-line](https://en.wikipedia.org/wiki/Command-line_interface) programs
developed to [determine](https://en.wikipedia.org/wiki/Determination_of_equilibrium_constants)
the [stability constants of complexes](https://en.wikipedia.org/wiki/Stability_constants_of_complexes)
from experimental data using a least-squares method. Because they were extensively
tested and well adapted to use experimental laboratory data, they are still being used.

LetaGrop was developed by Lars Gunnar Sillén and his co-workers at the Department of
Inorganic Chemistry, Royal Institute of Technology ([KTH](https://www.kth.se/en)),
Stockholm, Sweden.

It was initially developed using the [Ferranti-Mercury autocode](https://en.wikipedia.org/wiki/Ferranti_Mercury)
and then translated into [Algol](https://en.wikipedia.org/wiki/ALGOL)
and finally to [Fortran](https://en.wikipedia.org/wiki/Fortran).

The initial Fortran version by [Ekelund et al. (1970)][1] is here translated
into **Fortran 90**. A list of publications describing LetaGrop is given at the
end of this document. The references published in _Arkiv för Kemi_, which are
practically impossible to get, are included in this site as PDF-files.
Instructions are also given here on how to download the [`MinGW64`](https://www.mingw-w64.org/)
compiler, how to create the LetaGrop Windows binaries, how to test
them and how to run LetaGrop.

### The LetaGrop family of programs

Different LetaGrop program versions were developed at KTH to obtain chemical equilibrium constants from different kinds of experimental data. The programs included here are:

- **NyTit** for data from [potentiometric titrations](https://en.wikipedia.org/wiki/Determination_of_equilibrium_constants#Potentiometric_measurements).
- **Spefo** for data from [spectrophotometry](https://en.wikipedia.org/wiki/Determination_of_equilibrium_constants#Spectrophotometric_measurements).
- **Distr** for data from [solvent extraction](https://simple.wikipedia.org/wiki/Solvent_extraction) ([liquid-liquid distribution](https://en.wikipedia.org/wiki/Equilibrium_chemistry#Partition)).
- **ModFnk** (Model Functions) allows obtaining parameters for a user-defined function.

### Alternative software

See for example:
https://en.wikipedia.org/wiki/Determination_of_equilibrium_constants#Implementations

## Download
All files are available from the [releases section][2].

## See also
Ignasi's page on [water chemistry][3].

## References

- Dyrssen, D., Ingri, N., Sillén, L.G., 1961. “Pit-mapping” - a general approach
for computer refining of equilibrium constants. Acta Chem. Scand. 15, 694–696.
https://doi.org/10.3891/acta.chem.scand.15-0694

- Sillén, L.G., 1962. High-speed computers as a supplement to graphical methods I.
Functional behavior of the error square sum. Acta Chemica Scandinavica 16, 159–172.
https://doi.org/10.3891/acta.chem.scand.16-0159

- Ingri, N., Sillén, L.G., 1962. High-speed computers as a supplement to graphical
methods II. Some computer programs for studies of complex formation equilibria.
Acta Chemica Scandinavica 16, 173–191. https://doi.org/10.3891/acta.chem.scand.16-0173

- Sillén, L.G., 1964. High-speed computers as a supplement to graphical methods III.
Twist matrix methods for minimizing the error-square sum in problems with many unknown
constants. Acta Chemica Scandinavica 18, 1085–1098.
https://doi.org/10.3891/acta.chem.scand.18-1085

- Ingri, N., Sillén, L.G., 1964. High-speed computers as a supplement to graphical
methods IV. An ALGOL version of LETAGROP VRID. Arkiv för Kemi 23, 97–121.

- Sillén, L.G., Warnqvist, B., 1968. Equilibrium constants and model testing from
spectrophotometric data, using LETAGROP. Acta Chem. Scand. 22, 3032–3034. https://doi.org/10.3891/acta.chem.scand.22-3032

- Sillén, L.G., Warnqvist, B., 1969. High-speed computers as a supplement to
graphical methods. 6 A strategy for two-level LETAGROP adjustment of common and “group”
parameters. Some features that avoid divergence. Arkiv för Kemi 31, 315–339.

- Sillén, L.G., Warnqvist, B., 1969. High-speed computers as a supplement to graphical
methods. 7 Model selection and rejection with LETAGROP. Elimination of species with
negative or “insignificant” equilibrium constants. Arkiv för Kemi 31, 341–351.

- Arnek, R., Sillén, L.G., Wahlberg, O., 1969. High-speed computers as a supplement to
graphical methods. 8. Some devices to speed up computations on chemical equilibria and
simplify programming for LETAGROP. Application to complex formation.
Arkiv för Kemi 31, 353–363.

- Brauner, P., Sillén, L.G., Whiteker, R., 1969. High-speed computers as a supplement to
graphical methods. 9 Adjustment for systematic experimental errors and other
“group parameters” in LETAGROP. Apllications to potentiometric titrations.
Arkiv för Kemi 31, 365–376.

- Sillén, L.G., Warnqvist, B., 1969. High-speed computers as a supplement to
graphical methods. 10 Application of LETAGROP to spectrophotometric data, for
testing models and adjusting equilibrium constants. Arkiv för Kemi 31, 377–390.

- Arnek, R., 1970. High-speed computers as a supplement to graphical methods 11.
Application of LETAGROP to calorimetric titrations. Arkiv för Kemi 32, 81–88.

- Ekelund, R., Sillén, L.G., Wahlberg, O., 1970. Fortran editions of Haltafall and
Letagrop. Acta Chemica Scandinavica 24, 3073.
https://doi.org/10.3891/acta.chem.scand.24-3073

- Liem, D.H., 1971. High-speed computers as a supplement to graphical methods 12.
Application of LETAGROP to data for liquid-liquid distribution equilibria.
Acta Chemica Scandinavica 25, 1521–1534. https://doi.org/10.3891/acta.chem.scand.25-1521

- Liem, D.H., Ekelund, R., 1979. New types of input in DISTR. Application of LETAGROP
for analysis of liquid-liquid distribution equilibria data.
Acta Chemica Scandinavica A 33, 481–482. https://doi.org/10.3891/acta.chem.scand.33a-0481


[1]: http://actachemscand.org/doi/10.3891/acta.chem.scand.24-3073
[2]: https://github.com/ignasi-p/letaGrop/releases/latest
[3]: https://sites.google.com/view/groundwatergeochemistry
