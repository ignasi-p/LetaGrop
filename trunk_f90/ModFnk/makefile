# compiler
FC = gfortran

# compile flags
FCFLAGS = -I ./obj -J ./obj -fbounds-check -fbacktrace -W -Wunused -ffpe-trap=invalid,zero,overflow -ffpe-summary=none
# Using optimization (flag "-O") may give quite different results...
# link flags
FLFLAGS = -I ./obj -static

# "-I dir" Affects interpretation of the INCLUDE directive. This path is also used to
#   search for .mod files when previously compiled modules are required by a USE statement
# "-J dir" This option specifies where to put .mod files for compiled modules. It is
#   also added to the list of directories to searched by an USE statement.
#
# Note: hyphens not allowed in the "exe" name

ModFNK: Modules ModFNK.exe

test : Test_M_ox2.exe

Test_M_ox2.exe : ./obj/Test_M_ox2.o ./obj/Chem_Module.o ./obj/HaltaFall.o ./obj/Factor_Module.o ./obj/SIT.o 
	$(FC) $(FLFLAGS) -o Test_M_ox2.exe ./obj/Test_M_ox2.o \
                ./obj/Chem_Module.o ./obj/HaltaFall.o ./obj/Factor_Module.o ./obj/SIT.o ./obj/Read_IR.o

./obj/Test_M_ox2.o : ./src/Test_M_ox2.f90 ./obj/Chem_Module.o  ./obj/SIT.o  ./obj/Factor_Module.o  ./obj/HaltaFall.o
	$(FC) $(FCFLAGS) -o ./obj/Test_M_ox2.o -c ./src/Test_M_ox2.f90

Modules:  ./obj/Read_IR.o ./obj/DIMS_Module.o ./obj/Lcommon.o ./obj/Chem_Module.o  ./obj/SIT.o  ./obj/Factor_Module.o  ./obj/HaltaFall.o

ModFNK.exe :  ./obj/L3.o ./obj/L2.o ./obj/Miko.o ./obj/Read_IR.o \
              ./obj/ModFNK.o ./obj/Chem_Module.o ./obj/HaltaFall.o ./obj/Factor_Module.o ./obj/SIT.o \
              ./obj/Sark.o ./obj/Data.o ./obj/Letagrop.o
	$(FC) $(FLFLAGS) -o ModFNK.exe ./obj/ModFNK.o \
                ./obj/Chem_Module.o ./obj/HaltaFall.o ./obj/Factor_Module.o ./obj/SIT.o \
                ./obj/Letagrop.o ./obj/L2.o ./obj/L3.o ./obj/Sark.o ./obj/Miko.o ./obj/Data.o \
                ./obj/Read_IR.o ./obj/DIMS_Module.o ./obj/Lcommon.o

./obj/ModFNK.o : ./src/ModFNK.f90 ../LetaGrop/src/Lcommon.f90  ./obj/Chem_Module.o  ./obj/SIT.o  ./obj/Factor_Module.o  ./obj/HaltaFall.o
	$(FC) $(FCFLAGS) -o ./obj/ModFNK.o -c ./src/ModFNK.f90

./obj/Chem_Module.o : ../../../HaltaFall/src/Chem_Module.f90 ./obj/Read_IR.o
	$(FC) $(FCFLAGS) -o ./obj/Chem_Module.o -c ../../../HaltaFall/src/Chem_Module.f90

./obj/HaltaFall.o : ../../../HaltaFall/src/HaltaFall.f90 ./obj/Chem_Module.o ./obj/Factor_Module.o
	$(FC) $(FCFLAGS) -o ./obj/HaltaFall.o -c ../../../HaltaFall/src/HaltaFall.f90

./obj/SIT.o : ../../../HaltaFall/src/SIT.f90 ./obj/Read_IR.o ./obj/Chem_Module.o
	$(FC) $(FCFLAGS) -o ./obj/SIT.o -c ../../../HaltaFall/src/SIT.f90

./obj/Factor_Module.o : ../../../HaltaFall/src/Factor_Module.f90 ./obj/Chem_Module.o ./obj/SIT.o
	$(FC) $(FCFLAGS) -o ./obj/Factor_Module.o -c ../../../HaltaFall/src/Factor_Module.f90

./obj/DIMS_Module.o : ../LetaGrop/src/DIMS_Module.f90
	$(FC) $(FCFLAGS) -o ./obj/DIMS_Module.o -c ../LetaGrop/src/DIMS_Module.f90

./obj/Lcommon.o : ../LetaGrop/src/Lcommon.f90
	$(FC) $(FCFLAGS) -o ./obj/Lcommon.o -c ../LetaGrop/src/Lcommon.f90

./obj/Ycalc.o : ./src/Ycalc.for ./src/Hdims.for
	$(FC) $(FCFLAGS) -o ./obj/Ycalc.o -c ./src/Ycalc.for

./obj/Letagrop.o : ../LetaGrop/src/Letagrop.f90 ../LetaGrop/src/Lcommon.f90
	$(FC) $(FCFLAGS) -o ./obj/Letagrop.o -c ../LetaGrop/src/Letagrop.f90

./obj/L2.o : ../LetaGrop/src/L2.f90 ../LetaGrop/src/Lcommon.f90
	$(FC) $(FCFLAGS)  -o ./obj/L2.o -c ../LetaGrop/src/L2.f90

./obj/L3.o : ../LetaGrop/src/L3.f90 ../LetaGrop/src/Lcommon.f90
	$(FC) $(FCFLAGS)  -o ./obj/L3.o -c ../LetaGrop/src/L3.f90

./obj/Miko.o : ../LetaGrop/src/Miko.f90 ../LetaGrop/src/Lcommon.f90
	$(FC) $(FCFLAGS)  -o ./obj/Miko.o -c ../LetaGrop/src/Miko.f90

./obj/Sark.o : ../LetaGrop/src/Sark.f90 ../LetaGrop/src/Lcommon.f90
	$(FC) $(FCFLAGS)  -o ./obj/Sark.o -c ../LetaGrop/src/Sark.f90

./obj/Data.o : ../LetaGrop/src/Data.f90 ../LetaGrop/src/Lcommon.f90
	$(FC) $(FCFLAGS)  -o ./obj/Data.o -c ../LetaGrop/src/Data.f90

./obj/Read_IR.o : ../LetaGrop/lib/Read_IR.f90
	$(FC) $(FCFLAGS)  -o ./obj/Read_IR.o -c ../LetaGrop/lib/Read_IR.f90

