@echo off
echo.
echo Testing LetaGrop-ModFnk
echo on

ModFNK Test_files_ModFnk\Constant_Cp\Constant_Delta-Cp.dat Test_files_ModFnk\Constant_Cp\Constant_Delta-Cp.out Test_files_ModFnk\Constant_Cp\Constant_Delta-Cp.txt

ModFNK Test_files_ModFnk\Solubility_H4edta\EDTA.dat Test_files_ModFnk\Solubility_H4edta\EDTA.out  Test_files_ModFnk\Solubility_H4edta\EDTA.txt

cd Test_files_ModFnk\Line_fit
..\..\ModFNK Line_fit.dat Line_fit.out Line_fit.txt
cd ..\..

cd Test_files_ModFnk\Solubility_M(ox)2
..\..\ModFNK  Pu_ox2_49REA.dat  Pu_ox2_49REA.out  Pu_ox2_49REA.txt
cd ..\..

:xit
pause
