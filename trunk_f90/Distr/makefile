# compiler
FC = gfortran

# compile flags
FCFLAGS = -I ./obj -J ./obj -fbounds-check -fbacktrace -W -Wunused -ffpe-trap=invalid,zero,overflow
# Using optimization (flag "-O") may give quite different results...
# link flags
FLFLAGS = -I ./obj -static

# "-I dir" Affects interpretation of the INCLUDE directive. This path is also used to
#   search for .mod files when previously compiled modules are required by a USE statement
# "-J dir" This option specifies where to put .mod files for compiled modules. It is
#   also added to the list of directories to searched by an USE statement.
#
# Note: hyphens not allowed in the "exe" name

DISTR: Modules DISTR.exe

Modules:  ./obj/Read_IR.o ./obj/DIMS_Module.o ./obj/Lcommon.o ./obj/Lcommon4.o

DISTR.exe :  ./obj/L3.o ./obj/L2.o ./obj/Miko.o ./obj/Sark.o \
             ./obj/BDTV.o ./obj/BDistr.o ./obj/Data.o ./obj/MAIN.o ./obj/READ_IR.o
	$(FC) $(FLFLAGS) -o DISTR.exe ./obj/BDistr.o \
        ./obj/MAIN.o ./obj/L2.o ./obj/L3.o ./obj/Miko.o ./obj/Data.o ./obj/Sark.o ./obj/BDTV.o \
        ./obj/READ_IR.o ./obj/DIMS_Module.o ./obj/Lcommon.o ./obj/Lcommon4.o

./obj/BDistr.o : ./src/BDistr.f90 ../LetaGrop/src/Lcommon.f90
	$(FC) $(FCFLAGS) -o ./obj/BDistr.o -c ./src/BDistr.f90

./obj/DIMS_Module.o : ../LetaGrop/src/DIMS_Module.f90
	$(FC) $(FCFLAGS) -o ./obj/DIMS_Module.o -c ../LetaGrop/src/DIMS_Module.f90

./obj/Lcommon.o : ../LetaGrop/src/Lcommon.f90
	$(FC) $(FCFLAGS) -o ./obj/Lcommon.o -c ../LetaGrop/src/Lcommon.f90

./obj/Lcommon4.o : ../LetaGrop/src/Lcommon4.f90
	$(FC) $(FCFLAGS) -o ./obj/Lcommon4.o -c ../LetaGrop/src/Lcommon4.f90

./obj/MAIN.o : ../LetaGrop/src/Letagrop.f90 ../LetaGrop/src/Lcommon.f90
	$(FC) $(FCFLAGS) -o ./obj/MAIN.o -c ../LetaGrop/src/Letagrop.f90

./obj/L2.o : ../LetaGrop/src/L2.f90 ../LetaGrop/src/Lcommon.f90
	$(FC) $(FCFLAGS)  -o ./obj/L2.o -c ../LetaGrop/src/L2.f90

./obj/L3.o : ../LetaGrop/src/L3.f90 ../LetaGrop/src/Lcommon.f90
	$(FC) $(FCFLAGS)  -o ./obj/L3.o -c ../LetaGrop/src/L3.f90

./obj/Miko.o : ../LetaGrop/src/Miko.f90 ../LetaGrop/src/Lcommon.f90
	$(FC) $(FCFLAGS)  -o ./obj/Miko.o -c ../LetaGrop/src/Miko.f90

./obj/Data.o : ../LetaGrop/src/Data.f90 ../LetaGrop/src/Lcommon.f90
	$(FC) $(FCFLAGS)  -o ./obj/Data.o -c ../LetaGrop/src/Data.f90

./obj/Sark.o : ../LetaGrop/src/Sark.f90 ../LetaGrop/src/Sark.f90
	$(FC) $(FCFLAGS)  -o ./obj/Sark.o -c ../LetaGrop/src/Sark.f90

./obj/BDTV.o : ../LetaGrop/src/BDTV.f90 ../LetaGrop/src/Lcommon.f90
	$(FC) $(FCFLAGS)  -o ./obj/BDTV.o -c ../LetaGrop/src/BDTV.f90

./obj/Read_IR.o : ../LetaGrop/lib/Read_IR.f90
	$(FC) $(FCFLAGS)  -o ./obj/Read_IR.o -c ../LetaGrop/lib/Read_IR.f90
