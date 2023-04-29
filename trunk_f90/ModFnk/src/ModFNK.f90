MODULE PutsUbbe
! LETAGROP MOD/85 (IBM XT FEBRUARY 1986***/FORTRAN)
! ModFKN (Model Functions)
USE LetaGropModule
USE IO, ONLY : ErrStop, PauseMe, getParent
USE READIR, ONLY : INFL, INFL2, firstInput
Implicit NONE
Logical, PRIVATE :: firstTime = .true., firstInSet
Integer, PARAMETER :: IDEBG = 11, UTTBL = 12
Integer :: OUTfiles(2)
Character(len=200) :: inFilePath = ""

SAVE

CONTAINS

!-----------------------------------------------------------------------------
SUBROUTINE NAMN
  Implicit NONE
  Character(len=200) :: FILIN="LetaGrop.dat",FILOUT="LetaGrop.out",FILTBL="LetaGrop.txt",TEMP1,TEMP2,TEMP3
  Character(len=1) :: BLANK=" "
  Integer :: IOERR
  Character(len=30) TSTR,DSTR
  SAVE

  WRITE(*,1)
  1 FORMAT (" *** LETAGROP - for the IBM-PC ***",27x,"(I.Puigdomenech)",/," *** ModFKN (Model Functions) April 1986 ***",/)
  WRITE(*,'(4x,"Input and output file specification",/)')
  call getcmd (TEMP1,TEMP2,TEMP3)
! Open Input-Output files for LETAGROP
  Do While (.true.)
    If(TEMP1 == BLANK) Then
        100 WRITE(*,'(/,"Enter the name of the INPUT file  [",A,"]:")') trim(FILIN)
        READ(*,'(A)',ERR=100,END=9999) TEMP1
    EndIf
    If(TEMP1 /= BLANK) FILIN=TEMP1
    OPEN(INFL,FILE=FILIN,STATUS='OLD',IOSTAT=IOERR)
    If(IOERR == 0) Exit ! do while true
    WRITE(*,'("?? Could not open file:",/,A)') trim(FILIN)
    TEMP1 = BLANK
  EndDo ! While true

  Do While (.true.)
    If(TEMP2 == BLANK) Then
        200 WRITE(*,'("Enter the name of the OUTPUT file  [",A,"]:")') trim(FILOUT)
        READ(*,'(A)',ERR=200,END=9999) TEMP2
    EndIf
    IF(TEMP2 /= BLANK) FILOUT=TEMP2
    OPEN(UT,FILE=FILOUT,STATUS='UNKNOWN',IOSTAT=IOERR)
    IF(IOERR == 0) Exit ! do while true
    WRITE(*,'("?? Could not open file:",/,A)') trim(FILOUT)
    TEMP2 = BLANK
  EndDo ! While true
  Do While (.true.)
    If(TEMP3 == BLANK) Then
        300 WRITE(*,'("Enter the name of the output TABLE file  [",A,"]:")') trim(FILTBL)
        READ(*,'(A)',ERR=300,END=9999) TEMP3
    EndIf
    IF(TEMP3 /= BLANK) FILTBL=TEMP3
    OPEN(UTTBL,FILE=FILTBL,STATUS='UNKNOWN',IOSTAT=IOERR)
    IF(IOERR == 0) Exit ! do while true
    WRITE(*,'("?? Could not open file:",/,A)') trim(FILTBL)
    TEMP3 = BLANK
  EndDo ! While true
!
!  CALL DATE AND TIME
!
WRITE(UT,1)
Call TimDat (DSTR,TSTR)

Write (*,841)  trim(FILIN),trim(FILOUT),trim(FILTBL),trim(DSTR),trim(TSTR)
Write (UT,841) trim(FILIN),trim(FILOUT),trim(FILTBL),trim(DSTR),trim(TSTR)
841 FORMAT ("Input, output and table files:",/,A,/,A,/,A,/,58x,A,1x,A)
Write(UT,'(1X)')

Call getParent(FILIN,inFilePath)

OUTfiles(1) = UT; OUTfiles(2) = UTTBL

RETURN

9999  call ErrStop

END SUBROUTINE NAMN

!-----------------------------------------------------------------------------
SUBROUTINE CloseFiles
INTEGER :: IOERR
Close (Unit=INFL,iostat=IOERR)
Close (Unit=INFL2,iostat=IOERR)
Close (Unit=UT,iostat=IOERR)
Close (Unit=UTTBL,iostat=IOERR)
Close (Unit=IDEBG,iostat=IOERR)
RETURN
END SUBROUTINE CloseFiles

!-----------------------------------------------------------------------------
SUBROUTINE TimDat (date,time)
!Return the current value of date and time
!  in character strings of any length
Implicit NONE
Character (LEN=*), INTENT(OUT)   :: date, time
! Minimalistic GNU for Windows begins
integer val(8), i
CHARACTER*30 dag, tid
call date_and_time(VALUES=val)
dag = ' '
tid = ' '
write(dag,'(I4,"-",I2,"-",I2)') val(1),val(2),val(3)
write(tid,'(I2,":",I2,":",I2)') val(5),val(6),val(7)
Do i = 1, LEN_TRIM(dag)
    if(dag(i:i)==" ") dag(i:i)="0"
    end do
Do i = 1, LEN_TRIM(tid)
    if(tid(i:i)==" ") tid(i:i)="0"
    end do
date = trim(dag)
time = trim(tid)
! Minimalistic GNU for Windows ends
RETURN
END SUBROUTINE TimDat

!-----------------------------------------------------------------------------
SUBROUTINE GetCmd (fname1,fname2,fname3)
  Implicit NONE
  Character (LEN=*), INTENT(OUT) :: fname1,fname2,fname3
  Integer :: count
  fname1 = " "
  fname2 = " "
  fname3 = " "
! Minimalist Gnu for Windows
  count = command_argument_count()
  if(count >= 1) then
    call get_command_argument(1, fname1)
    if(count >= 2) then
        call get_command_argument(2, fname2)
        if(count >= 3) then
            call get_command_argument(3, fname3)
        endif
    endif
  endif
Return
END SUBROUTINE GetCmd

!-----------------------------------------------------------------------------
SUBROUTINE PUTS
  RETURN
END SUBROUTINE PUTS

!-----------------------------------------------------------------------------
SUBROUTINE UBBE
  !USE LetaGropModule
  USE L3, ONLY : SATSUT, UVAR
  Implicit NONE
  Real(dp) :: FEL(4),XINPUT(20),WEIGHT,YBER,YSUM,USUM,W
  Logical :: ret
  Integer :: IDUM, IIP, point, iOut
  Character(len=200) :: caption
  Character(len=2) :: Xi
  if(lgDbg) Write(*,'("UBBE in, RURIK=",I0)') RURIK

  U=0.D0
  Rf=0.D0
  YSUM=0.D0
  USUM=0.D0

  IF(.not.TAGE) RSL = 1 ! RS = RS1 ! Tage=false means upper level adjustment of "k" ! modified Aug.2021
  RS = RSI(RSL)   ! added Aug.2021

  DO WHILE (.true.) ! loop through the sets of data (RSL)

    ! NEW SET

    If(RURIK == 2) Then
    ! --- RUBRIK-SATSA
        If(NAP > 2) Then
            caption = ""
            point = 6
            Do IIP = 1,NAP-1
                Write(Xi,'(i0)') IIP
                Xi = adjustL(Xi)
                caption(point:) = "X" // Xi
                point = point + 11
            EndDo
            point = point+1
            caption(point:) = "Y        Y-calc      Error"
        Else
            Write(caption,'(5x,"X",10x,"Y",8x,"Y-calc",6x,"Error")')
        EndIf
        Do iOut = 1,2
            CALL SATSUT (OUTfiles(iOut))
            IF(VAL == 1) WRITE(OUTfiles(iOut),126)
            IF(VAL == 2) WRITE(OUTfiles(iOut),127)
            IF(VAL == 3) WRITE(OUTfiles(iOut),128)
            IF(VAL == 4) WRITE(OUTfiles(iOut),129)
            WRITE(OUTfiles(iOut),'(A)') trim(caption)
        End Do !iOut
        IF(VAL == 1) WRITE(*,126)
        IF(VAL == 2) WRITE(*,127)
        IF(VAL == 3) WRITE(*,128)
        IF(VAL == 4) WRITE(*,129)
        WRITE(*,'(A)') trim(caption)
        126 FORMAT(/,"Error = (Ycalc - Y);   U = Sum(Error^2)   (unweighted)")
        127 FORMAT(/,"Error = (Ycalc - Y) / Y;   U = Sum(Error^2)   (unweighted)")
        128 FORMAT(/,"Error = (Ycalc - Y);   U = Sum((Error/X[1])^2)   (weighted)")
        129 FORMAT(/,"Error = (Ycalc - Y) / Y;   U = Sum((Error/X[1])^2)   (weighted)")
    EndIf

    CELL = APCELL(RS) - NAPA
    firstInSet = .true. ! begin new set of data

    DO RP = 1,NP(RS)
    !-- NEW POINT
        CELL = CELL + NAPA
        ! NAP = number of experimental  quantities
        DO IIP=1,6
            XINPUT(IIP) = 0.D0
        END DO
        DO IIP=1,NAP-1
            IDUM = CELL +1 + IIP
            XINPUT(IIP) = AP(IDUM)
        END DO

        CALL YCALC (WEIGHT,YBER,XINPUT)
        ! error calculation
        Y = AP(CELL+1)
        !if(lgDbg) Write(*,'("YCALC returns, Y=",1pE11.3,"  YBER=",E11.3,"  XINPUT(1)=",E11.3)') Y,YBER,XINPUT(1)
        FEL(1) = YBER - Y
        FEL(2) = 9.99D+199
        If(abs(Y) > 1.D-199) Then
            FEL(2) = FEL(1)/Y
        Else
            Write(UT,138) RP,RS
            Write(*,138)  RP,RS
            138 FORMAT("Y = 0 for point ",i0," in SET ",i0,"; can NOT calculate error = (Ycalc - Y) / Y.")
            call ErrStop
        EndIf
        FEL(3) = FEL(1);  FEL(4) = FEL(2)
        ! Calculation of U (sum of square errors)
        ! Note that if VAL > 4 there will be a run-time error
        if(VAL <= 2) then          ! unweighted
            W = FEL(VAL)*FEL(VAL)
            USUM = USUM + (FEL(1)*FEL(1))
            ! calculation of ySum (sum of square Y)
            YSUM = YSUM + (Y*Y)
        else                       ! weighted
            W = WEIGHT * FEL(VAL)*FEL(VAL)
            USUM = USUM + (WEIGHT * FEL(1)*FEL(1))
            ! calculation of ySum (sum of square Y)
            YSUM = YSUM + (WEIGHT * Y*Y)
        endif
        U = U + W
        !if(lgDbg) Write(*,'("ok, RURIK=",i0," RS=",i0," NP(RS)=",i0," RP=",i0)') RURIK,RS,NP(RS),RP

        IF(RURIK == 2) Then
            !  UTTAG  (PRINT-OUT)
            IDUM=NAP-1
            WRITE (UT,142)  (XINPUT(IIP),IIP=1,IDUM),Y,YBER,FEL(VAL)
            WRITE (UTTBL,142) (XINPUT(IIP),IIP=1,IDUM),Y,YBER,FEL(VAL)
            WRITE (*,142)   (XINPUT(IIP),IIP=1,IDUM),Y,YBER,FEL(VAL)
            142 FORMAT (1X,1P,99E11.3)
        EndIf
        firstInSet = .false.

    END DO ! RP (to new point)

    IF(RSL >= RSN .or. TAGE) THEN ! IF(RS2 <= RS .or. TAGE) THEN  !  changed Aug.2021
        !if(lgDbg) Write(*,'("ok, RS=",i0)') RS
        IF(abs(YSUM) > 1.D-100) Rf = DSQRT(USUM/YSUM) ! R-factor
        IF(RURIK == 2) Then
            WRITE (UT,*)
            WRITE (UTTBL,*)
            WRITE (*,*)
        EndIf
        IF(RURIK /= 2 .and. RURIK /= 1) Then
            ret = .true.
            IF(SKRIUT==1 .or. (SKRIUT==0 .and. .not. TAGE &
                .or.(PROV.and. .not. (TAGE .and. SKRIUT < 0)))) ret = .false.
            if(ret) RETURN
        EndIf
        Call UVAR (UT)
        If(RURIK == 2) Call UVAR (UTTBL)
        IF(PROV) THEN
            WRITE (UT,*)
            WRITE (*,*)
        ENDIF
        RETURN
    ENDIF

    RSL = RSL+1 ! RS=RS+1 ! change Aug.2021
    RS = RSI(RSL)
    !if(lgDbg) Write(*,'("New RS=",i0)') RS

END DO ! while true (to new set of data)

END SUBROUTINE UBBE

!-----------------------------------------------------------------------------
SUBROUTINE Ycalc (WEIGHT,Y,X)
Implicit NONE
Real(dp), INTENT(IN) :: X(:)
Real(dp), INTENT(OUT) :: Y, WEIGHT
! TYP = 3
Real(dp) :: TK
Real(dp), PARAMETER :: Tr = 298.15D0, R = 8.31446261815324D0
! TYP = 5
Real(dp) :: OHtot, Hplus, OH, Xminus
Real(dp) :: H0, Ht, V0,Vt,Vtot
Real(dp) :: E0, dHo, dHt, cHXall, g
Real(dp) :: EjH, EjOH, KHX, Kw
! TYP = 6
Real(dp) :: Solub,Htot
Real(dp) :: logKs2,logKs3,logKs4,logKs5,logKs6
Integer :: I

  !if(lgDbg) Write(*,'("Ycalc, X(1)=",1PE11.3)') X(1)
  WEIGHT=1.D0
  Y=0.D0
! Function:   1  2  3  4  5  6  7  8  9  10  11  12
  !GO TO (10,20,30,40,50,60,70,80,90,100,110),TYP
  SELECT CASE (TYP)

  CASE (1)
    !--- linear fit
    Y = K(1) + K(2)*X(1)
  CASE (2)
    !--- linear fit with weights
    Y = K(1) + K(2)*X(1)
    if(X(2) > 0.d0) WEIGHT = 1.0D0/(X(2)*X(2))
  CASE (3)
    !--- Constant heat capacity equation:
    !        Y = logKeq(T)
    !        X(1) = temperature in celsius
    !        X(2) = uncertainty
    !        K(2) = enthalpy of reaction in J/mol
    !        K(3) = heat capacity of reaction in J/(mol K)
    TK = X(1) + 273.15D0
    Y = K(1) + ( K(2)*1000.D0*( (1.D0/Tr) - (1.D0/TK) ) &
             + K(3)*( (Tr/TK) - 1.D0 + log(TK/Tr) ) ) / (ln10*R)
    if(X(2) > 0.d0) WEIGHT = 1.0D0/(X(2)*X(2))
  CASE (4)
    !--- Polynomial function
    Y = 0.d0
    weight = 1.d0
    Do i=1,NK
        if(abs(K(i)) > 1.d-200) Y= Y +K(i)*(x(1)**ak(i,1))
    End Do
  CASE (5)
    !---  E0 titration
    Kw  = K(4)*10**AK(4,1)
    EjH = K(1)*10**AK(1,1)
    EjOH= K(2)*10**AK(2,1)
    KHX = K(3)*10**AK(3,1)

    H0 =  AS(RS,1)
    Ht =  AS(RS,2)
    V0 =  AS(RS,3)

    E0  = KS(RS,1)
    dHo = KS(RS,3)
    dHt = KS(RS,4)

    cHXall = KS(RS,7)

    g   = KS(RS,8)

    Vt = X(1)
    Vtot = V0 + Vt
    Hplus = (V0*(H0+dHo) + Vt*(Ht+dHt))/Vtot
    OH = 0.D0
    If(Hplus <= 0.D0) Then
        OHtot = -Hplus
        Hplus = Kw / OHtot
        Xminus =  cHXall * KHX / (KHX + Hplus)
        DO i=1,3
            OH = OHtot - Xminus
            Hplus = Kw / OH
            Xminus = cHXall * KHX / (KHX + Hplus)
        END DO
        OH = OHtot - Xminus
        Hplus = Kw / OH
    EndIf
    Y = E0 + g*log(Hplus)/ln10 + EjH*Hplus + EjOH*OH

  CASE (6)
    !--- H4edta(cr) solubility versus [H+]tot
    logKs6 = -9999.D0; if(K(1) > 0.D0) logKs6 = log(K(1))/ln10 + AK(1,1)
    logKs5 = -9999.D0; if(K(2) > 0.D0) logKs5 = log(K(2))/ln10 + AK(2,1)
    logKs4 = -9999.D0; if(K(3) > 0.D0) logKs4 = log(K(3))/ln10 + AK(3,1)
    logKs3 = -9999.D0; if(K(4) > 0.D0) logKs3 = log(K(4))/ln10 + AK(4,1)
    logKs2 = -9999.D0; if(K(5) > 0.D0) logKs2 = log(K(5))/ln10 + AK(5,1)
    Htot = X(1)
    call edta(Htot,logKs6,logKs5,logKs4,logKs3,logKs2,Solub)
    Y = Solub
    firstTime = .false.

  CASE (10)
    ! M(ox)2.6H2O(s) solubility versus [H+]tot,[H2ox]tot
    ! components:  H+, Na+, ClO4-, NO3-, H2ox, M(ox)2.H2O(s),
    ! complexes:   Hox-,ox-2,M+4,MOH+3,M(OH)2+2,MNO3+3,M(ox)+2,M(ox)2,M(ox)3-2
    !
    ! Note: The first five complexes must be: Hox-, ox-2, Mp+4, MOH+3, M(OH)2+2, MNO3+3
    !       for MOH+3, M(OH)2+2 and MNO3+3 the constants correspond to:
    !          M+4 + H2O = MOH+3 + H+
    !          M+4 + 2 H2O = M(OH)2+2 + 2H+
    !  and     M+4 + NO3- = MNO3+3

    call M_ox2 (Solub,X)
    Y = Solub

  CASE (12)
    !call H_Na_Ca_Mg_La_SO4_Cl_ClO4_H2O (Y,X)
    call SIT (Y,X)
    if(X(1) > 0.d0) WEIGHT = 1.0D0/(X(1)*X(1))
    firstTime = .false.

  CASE (14)
    call SIT (Y,X)
    if(X(1) > 0.d0) WEIGHT = 1.0D0/(X(1)*X(1))
    firstTime = .false.

  CASE (15)
    call SIT (Y,X)
    if(X(1) > 0.d0) WEIGHT = 1.0D0/(X(1)*X(1))
    firstTime = .false.

  CASE DEFAULT
    WRITE(*,'("Programming error at ''ModFKN.Ycalc'': TYP=",I0)') TYP
    call ErrStop
  END SELECT

  RETURN

END SUBROUTINE Ycalc

!-----------------------------------------------------------------------------
SUBROUTINE EDTA (Htot,logKs6,logKs5,logKs4,logKs3,logKs2,Solubility)
! calculates the solubility of edta when the [H+]tot is known
!    H4edta(s) + 2 H+ = H6edta+2   logKs6
!      ...
!    H4edta(s) - 2 H+ = H2edta-2   logKs2
USE CHEM
USE HALTAF_Module
Implicit NONE
Real(dp), INTENT(IN) :: Htot,logKs6,logKs5,logKs4,logKs3,logKs2
Real(dp), INTENT(OUT) :: Solubility
Integer :: I
!-------------------
    IF(firstTime) THEN
        NA = 2  ! chemical components
        NX = 5  ! soluble reaction products
        MS = NA+NX  !species = components + reaction products
        MSOL = 0 ! no solids
        ! allocate memory arrays
        Call CHEM_MEM_ALLOC
        Call HALTAF_MEM_ALLOC
        DO I=1,MS
            NOLL(I)=.FALSE.
        EndDo
        ! do not consider the formation of H4edta(cr) in aqueous solution
        NOLL(2)=.true.
        IDENTC(1)="H+"; Z(1) = +1; IDENTC(2)="H4edta(s)"; Z(2) = 0
        IDENT(1:NA) = IDENTC(1:NA)
        ! stoichiometry: 1st index is complex nbr, 2nd index component
        !    1st component: H+, 2nd component: H4edta(s)
        ! H6edta+2
        A(1,1)= 2.d0; A(1,2)=1.d0; IDENT(NA+1) = "H6edta2+"; Z(NA+1) = +2
        ! H5edta+
        A(2,1)= 1.d0; A(2,2)=1.d0; IDENT(NA+2) = "H5edta+"; Z(NA+2) = +1
        ! H4edta(aq)
        A(3,1)= 0.d0; A(3,2)=1.d0; IDENT(NA+3) = "H4edta(aq)"; Z(NA+3) = 0
        ! H3edta-
        A(4,1)=-1.d0; A(4,2)=1.d0; IDENT(NA+4) = "H3edta-"; Z(NA+4) = -1
        ! H2edta-2
        A(5,1)=-2.d0; A(5,2)=1.d0; IDENT(NA+5) = "H2edta-2"; Z(NA+5) = -2
        !  1st component: H+, 2nd component: H4edta(s)
        !     KH=1: total concentrations given
        !     KH=2: log activity given
        KH(1)=1
        KH(2)=2
        JWATER = -1 ! no H2O
        TOL=1.D-6  ! tolerance when solving mass-balance equation
        DBG=1      ! report errors only
        IOUT = 0   ! debug printout to the terminal
        !IOUT = UT  ! debug printout to the output file
        activityCoeffsModel = -1; ionicStr = 0.d0 ! no activity coefficient calculations
        ! Call printChemSystem (IOUT)
    ENDIF
    LBETA(1) = logKs6
    LBETA(2) = logKs5
    LBETA(3) = logKs4
    LBETA(4) = logKs3
    LBETA(5) = logKs2
    ! total concentrations for the components
    TOT(1)= Htot
    LOGA(2) = 0.d0
    CONT=.FALSE.
!---------------------
    CALL HaltaCalc
!---------------------

    IF(errFlags /= 0) THEN !------ Error Reporting --------------
        WRITE(*,991) Htot,trim(errFlagsToString()), trim(errFlagsGetMessages())
        991 FORMAT("Equilibrium calculations in HaltaFall failed for Htot=",1PE13.5,1x,A,/,A)
        Solubility = -1.D0
    ELSE
        Solubility = TOT(2)
    ENDIF

  RETURN

END SUBROUTINE EDTA

!-----------------------------------------------------------------------------
SUBROUTINE SIT (Y,X)
USE CHEM
USE HALTAF_Module
USE FACTOR_Module, ONLY : factorSetPath, temperature, pressure, factorPrint, fDbg
USE SIT
Implicit NONE
Real(dp), INTENT(IN) :: X(:)
Real(dp), INTENT(OUT) :: Y
Character(len=200) :: dataFile = "-?-"
Integer :: ioErr, ierr, ia,ix, icat,ian, zi
!Integer :: icat2,ian2, ik2, isp  ! ## 2022-07
Integer :: n, iCa, iSO4, setType, flag
Integer :: TYP0 = -1, NK0 = -1
Real(dp) :: nu, saltConc, logGpm, logC, logQ, nu_ion, q
Real(dp), SAVE :: tolHalta0, tolLogF0
Logical :: containsCa, containsSO4, isCa, isSO4, calcFailed
Character(len=200) :: errMsg
Character(LEN=400) :: TEXT
!Logical :: neutral  ! ## 2022-07
!Character(len=MXID) :: Species1, Species2

!-------------------
    IF(TYP /= TYP0) THEN  ! ## 2022-07
        !Write(UT,*) "TYP=",TYP," TYP0=",TYP0
        IOUT = UT
        if(TYP == 12) dataFile = "H_Na_Ca_Mg_La_SO4_Cl_ClO4_H2O.dat"
        if(TYP == 14) dataFile = "H_Na_Ca_SO4_Cl_CO3_H2O.dat"
        if(TYP == 15) dataFile = "H_Na_Ca_SO4_Cl_HCO3_H2O.dat"
        ! --- Read the chemical system previously created with Spana (or Medusa)
        !     "dataFile" = the data file describing the chemical system
        if(len_trim(inFilePath)>0) dataFile = trim(inFilePath) // trim(dataFile)
        ! for READIR, save the input file number
        INFL2 = min(INFL+1,7) ! get a new input file number
        ! open the input file for reading with READIR
        OPEN (UNIT=INFL2,FILE=dataFile,STATUS="OLD",IOSTAT=ioErr)
        If(ioErr /= 0) Then
            WRITE(*,1031) ioErr,Trim(dataFile),INFL2
            1031 FORMAT('? Error Nbr.',I6,/2X,'Could NOT open disk file: ',/5x,'"',A,'"',/,"on unit ",i0)
            Call ErrStop
        EndIf
        firstInput = .false. ! make module READIR use unit INFL2
        ! input file - read and allocate memory for the arrays
        Call ReadChemSystem (temperature, pressure) ! the data is now stored in module CHEM
        ! for READIR, restore reading from the first input file (INFL),
        ! so that LetaGrop can continue reading data from the LetaGrop input file
        firstInput = .true.
        CLOSE(UNIT=INFL2)

        ! --- Prepare the HaltaFall calculations
        activityCoeffsModel = 1  ! set the model to SIT
        tol = 1e-6     ! tolerance for the mass-balance
        tolLogF = 1e-4  ! tolerance for activity coeffs.
        tolHalta0 = tol;  tolLogF0 = tolLogF
        !    debug:
        dbg = nint(ag(1))
        ! paths where to search for file "SIT-coefficients.dta"
        ! call factorSetPath (inFilePath)

        ! --- allocate memory for working arrays
        call HALTAF_MEM_ALLOC
        ! --- allocate memory for the specific interaction coefficients
        call SIT_MEM_ALLOC(nIon)
        ! do not set defaults... ## 2022-07
        !Do i=1, nIon+2
        !    Do j=1, nIon+2
        !        call setEps(i,j, 0.d0); !,0,0);
        !    EndDo
        !End Do
        ! --- Read epsilon values from file "SIT-coefficients.dta"
        firstInput = .false. ! make module READIR use unit INFL2
        call readSITdata (inFilePath, UT)
        ! for READIR, restore reading from the first input file (INFL),
        ! so that LetaGrop can continue reading data from the LetaGrop input file
        firstInput = .true.
        !call setDefaultSITvalues(UT)
        call setEps(nIon+1,nIon+2,0.0446d0); ! eps(Na+,Cl-)

        !    set the initial concentration parameters
        Do ia = 1, Na
            if(ia == jWater) then
                kh(ia)=2; logA(ia) = 0.d0;
            else
                kh(ia)=1; tot(ia) = 0.d0;
            endif
        EndDo

        TYP0 = TYP
        firstTime = .true.  ! firstTime is used for printout of information only once
    ENDIF ! (TYP /= TYP0)

    IF(TYP /= TYP0 .OR. NK0 /= NK) THEN  ! ## 2022-07
        NK0 = NK
        ! --- check that the equilibrium constants and eps-values are ok
        !     (this is done only once, at the beginning when TYP or NK has been changed)
        errMsg = ""; ierr = -1;
        Do ik = 1, NK
            ! the AK are:
            ! the AK are:    ! ## 2022-07
            !   - ak[ik][1] = power of 10
            !   - ak[ik][2] a flag that should be either zero or 99
            !      * if ak[ik][2] = 0:  K(ik) = K(i), ak[ik][1] = power of 10,
            !        and ak[ik][3] = i (reaction number 1 to Nx+mSol),
            !        ak[ik][4] = electric charge of "i" (used as a check to avoid mistakes)
            !      * if ak[ik][2] = 99:  K(ik) = eps(i,j) and ak[ik][3] = i, ak[ik][4] = j.
            !        Neutral and charged species are treated equally
            flag = nint(ak(ik,2));
            IF(flag /= 99) Then ! k is an equilibrium constant
                ix = nint(ak(ik,3));
                if(ix <=0 .or. ix > Nx+mSol) then
                    write(errMsg,'("bad species ''",i0,"'', must be < ",i0)') ix, (Nx+mSol)
                    ierr = ik; exit;
                endif
                zi = nint(ak(ik,4));
                if(ix <= Nx) then ! logK for an aqueous species, check charge
                    if(zi /= z(Na+ix)) then
                        write(errMsg,'("bad charge ''",i0,"'' for species ''",i0,"''")') zi,ix
                        ierr = ik; exit;
                    endif
                else ! logK for a solid, check that charge is zero
                    if(zi /= 0) then
                        write(errMsg,'("bad charge ''",i0,"'' for species ''",i0,"'' (should be =0)")') zi,ix
                        ierr = ik; exit;
                    endif
                endif
            Else ! an eps-value
                icat = nint(ak(ik,3)); ian = nint(ak(ik,4));
                if(icat <=0 .or. icat > (Na+Nx) .or. ian <=0 .or. ian > (Na+nx)) then ! ian <=0 ! ## 2022-07
                    write(errMsg,'("bad species nbr for eps(",i0,",",i0,")")') icat,ian
                    ierr = ik; exit;
                endif
                if(icat == jWater .or. ian == jWater) then
                    write(errMsg,'("H2O as species for eps(",i0,",",i0,")")') icat,ian
                    ierr = ik; exit;
                endif
                !neutral = .false.; ! ## 2022-07
                !if(z(icat) == 0) neutral = .true.
                !if(.not.neutral) then
                !    if(ian == 0) then
                !        write(errMsg,'("eps(",i0,",",i0,"), cation = ''",a,"'', anion is wrong")') icat,ian,trim(ident(icat))
                !        ierr = ik; exit;
                !    endif
                !    if(z(icat) < 0) then
                !        write(errMsg,'("eps(",i0,",",i0,"), cation = ''",a,"'' has negative charge")') icat,ian,trim(ident(icat))
                !        ierr = ik; exit;
                !    endif
                !    if(z(ian) > 0) then
                !        write(errMsg,'("eps(",i0,",",i0,"), anion = ''",a,"'' has positive charge")') icat,ian,trim(ident(ian))
                !        ierr = ik; exit;
                !    endif
                !else ! neutral
                !    if(ian /= icat .and. ian /= 0) then
                !        write(errMsg,'("eps(",i0,",",i0,"), 2nd species = ''",a,"'' is wrong")') icat,ian,trim(ident(ian))
                !        ierr = ik; exit;
                !    endif
                !endif ! neutral?
            End If ! ak[ik][2] == 99?
        EndDo ! for ik
        if(ierr > -1) then
            write(*,'("Error --- ",A,", with k(",i0,")=",1PE11.3, &
                " ak(i,)=",0p,10F7.2)') &
                trim(errMsg),(ierr+1),k(ierr),ak(ierr,1),ak(ierr,2),ak(ierr,3),ak(ierr,4)
            Call ErrStop
        endif
        firstTime = .true.  ! firstTime is used for printout of information only once
        NK0 = NK
    ENDIF  !(TYP /= TYP0 .OR. NK0 /= NK)

    ! --- get the equilibrium constants and epsilon values being adjusted
    ! the AK are:    ! ## 2022-07
    !   - ak[ik][1] = power of 10
    !   - ak[ik][2] a flag that should be either zero or 99
    !      * if ak[ik][2] = 0:  K(ik) = K(i), ak[ik][1] = power of 10,
    !        and ak[ik][3] = i (reaction number 1 to Nx+mSol),
    !        ak[ik][4] = electric charge of "i" (used as a check to avoid mistakes)
    !      * if ak[ik][2] = 99:  K(ik) = eps(i,j) and ak[ik][3] = i, ak[ik][4] = j.
    !        Neutral and charged species are treated equally
    Do ik = 1, Nk
        flag = nint(ak(ik,2));  ix = nint(ak(ik,3));
        if(flag /= 99) then ! k is an equilibrium constant
            if(k(ik)> 0.d0) then
                lBeta(ix) = log10(k(ik)) + ak(ik,1);
            else
                lBeta(ix) = -99999.d0
            endif
        else ! an eps-value
            icat = ix; ian = nint(ak(ik,4));
            n = nint(ak(ik,1))  ! ## 2022-07
            q = k(ik)
            if(n /= 0) q = q * (10.d0 ** n)
            call setEps(icat, ian, q);   ! ## 2022-07
            if(bareNameOf(ident(icat)) == "Na") then
                call setEps(Na+Nx+1, ian, q)
                if(bareNameOf(ident(ian)) == "Cl") call setEps(Na+Nx+1, Na+Nx+2, q)
            endif
            if(bareNameOf(ident(ian)) == "Cl") call setEps(icat ,Na+Nx+2, q)
        endif ! ak[ik][2] = 99 ?
    EndDo ! for ik

    if(firstTime) then
        call printChemSystem (UT)
        call factorPrint (UT)
    endif

    if(x(1) <= 0.d0) then ! x(1) is the uncertainty in Y
        Write(*,'("Error: rs=",i0," rp=",i0," x(1)=",1PE11.3," must be >0")') rs,rp,x(1)
        call ErrStop
    endif
    do ia = 1, Na
        if(ia == jWater) cycle;
        if(x(1+ia) < 0.d0) then ! x(1+ia) is the total concentration of chemical component 'ia' must be >=0
            Write(*,'("Error: rs=",i0," rp=",i0," x(1+",i0,")=",1PE11.3," must be >=0")') rs,rp,ia,x(1+ia)
            Write(*,'("X(ix)=",5g11.3,10(:/6x,5g11.3))') (x(ix),ix=1,Na)
            call ErrStop
        endif
    enddo

    ! setType = AS(1): -1= log(act coeff) data, -2= osmotic coeff, n= gypsum (CaSO4.2H2O) solubility, solid reaction nbr "n"
    setType = nint(as(rs,1))
    if(setType < -2 .or. setType == 0 .or. (setType > 0 .and. (setType <= (Na+Nx) .or. setType > (Na+Nx+mSol)))) then
        Write(*,'("Error: wrong setType = AS(rs,1) = ",i0," for rs=",i0," Na+Nx=",i0," Na+Nx+mSol=",i0)') &
                   setType, rs, (Na+Nx), (Na+Nx+mSol)
        call ErrStop
    endif

    saltConc = -1.d0 ! needed for activity coeffs. (setType= -1)
    IF(setType == -1) THEN ! act.coeffs. data
        n = 0;     ! n = number of components in the salt, must be =2 for mean ionic activity coeffs.
        nu = 0.d0  ! nu = nu+ + nu-
        ! for act.coeffs, check that the stoichiometry agrees with given concentrations
        Do ia = 1, Na
            if(as(rs,ia+1) <0.d0) then
                Write(*,'("Error: rs=",i0," setType = AS(rs,1) = ",i0,", nu = AS(rs,",i0,") = ",1pE11.3," must be >=0")') &
                    rs,setType,(ia),as(rs,ia+1)
                call ErrStop
            endif
            if((x(ia+1) <= 0.d0 .and. as(rs,ia+1) >0.d0) .or. (x(ia+1) > 0.d0 .and. as(rs,ia+1) <=0.d0)) then
                Write(*,'("Error: rs=",i0," rp=",i0,", setType = AS(rs,1) = ",i0,", tot.conc(",i0,")=",1PE11.3,", nu=",E11.3)') &
                    rs,rp,setType,(ia),x(ia+1),as(rs,ia+1)
                Write(*,'("X(ix)=",5g11.3,10(:/6x,5g11.3))') (x(ix),ix=1,Na)
                call ErrStop
            endif
            if(as(rs,ia+1) >0) then ! stoichiometry > 0
                n = n+1;
                nu = nu + as(rs,ia+1);
                if(saltConc < 0.d0) then
                    if(x(ia+1) >0.d0) saltConc = x(ia+1) / as(rs,ia+1)
                else
                    if(x(ia+1) >0.d0 .and. (abs(saltConc - (x(ia+1) / as(rs,ia+1))) > saltConc * 1.d-5)) then
                        Write(*,'("Error: rs=",i0," setType=",i0," tot.conc(",i0,")=",1PE11.3," nu=",E11.3, &
                                    " saltConc=",E11.3)') &
                                rs,setType,(ia),x(ia+1),as(rs,ia+1),saltConc
                        Write(*,'("X(ix)=",5g11.3,10(:/6x,5g11.3))') (x(ix),ix=1,Na)
                        call ErrStop
                    endif
                endif
            endif  ! stoichiometry > 0
        EndDo
        ! n = number of components in the salt
        if(n /= 2) then ! mean ionic activity coefficients defined for (M_nu+ Y_nu-): two ions
            Write(*,'("Error: rs=",i0," rp=",i0," n=",i0,",", &
                  /7x,"setType = -1 (log act.coeff. data) and n != 2 (nr of components in the salt)?")') rs,rp,n
            call ErrStop
        endif
        if(saltConc < 0.d0) then
            Write(*,'("Error: rs=",i0,", setType=",i0,", saltConc=",1PE11.3," (< 0?)")') rs,setType,saltConc;
            call ErrStop
        endif
    END IF ! setType = -1 ?

    containsCa = .false.; containsSO4 = .false.; ! needed for gypsum (CaSO4.2H2O) solubility data
    iCa = -1; iSO4 = -1;
    Do ia = 1, Na
        if(ia == jWater) then
            logA(ia) = 0.d0;
            Cycle
        endif
        ! set total concentrations
        tot(ia) = 0.d0;
        isCa =  bareNameOf(identC(ia)) == "Ca"
        isSO4 = bareNameOf(identC(ia)) == "SO4"
        if(setType > 0) then  ! gypsum (CaSO4.2H2O) solubility
            if(ident(setType)(1:5) == "CaSO4") then ! data set with gypsum (CaSO4.2H2O) solubility data
                ! add some CaSO4
                if(isSO4) then
                    iSO4 = ia;  tot(ia) = 0.2d0
                endif
                if(isCa) then
                    iCa = ia;  tot(ia) = 0.2d0
                endif
            endif
        endif  ! gypsum (CaSO4.2H2O) solubility
        if(x(ia+1) > 0.d0) then
            tot(ia) = tot(ia) + x(ia+1);
            if(isCa) containsCa = .true.;
            if(isSO4) containsSO4 = .true.;
        endif
    End Do

    if(firstInSet) then
        !write(UT,'("firstInSet = true")')
        CONT = .false.
    endif

    ! --- Do the calculation with HALTAFALL
    activityCoeffsModel = 1 ! set the model to SIT
    ionicStr = -1  ! calculate the ionic strength
    !if(lgDbg) Write(*,'("call HaltaCalc")')
    fdbg = .false.
    !if(lgDbg .and. abs(x(2)-18.d0) < 0.001) fdbg = .true.
    calcFailed = .false.
    TOL = tolHalta0; tolLogF = tolLogF0
    call HaltaCalc
    !if(lgDbg) Write(*,'("call HaltaReturns")')
    ! check for errors from HaltaFall
    if(errFlags > 0) then
        !   1 = The numerical solution is uncertain (round-off errors)
        !   2 = Too many iterations when solving the mass balance equations
        !   3 = Failed to find a satisfactory combination of solids
        !   4 = Too many iterations trying to find the solids at equilibrium
        !   5 = Some aqueous concentration(s) too large (>50): uncertain activity coefficients
        !   6 = Activity factors did not converge
        !   7 = Calculation interrupted by the user
        calcFailed = (isErrFlagSet(2) .or. isErrFlagSet(3) .or. isErrFlagSet(4) .or. isErrFlagSet(6))
        if(calcFailed) then
            Cont=.false.
            do while (.true.)
                if(.not.calcFailed .or. TOL <= 1.d-9) exit ! do while (true)
                tol = tol * 0.1D0 ! decrease tolerance and try again
                CALL HaltaCalc
                calcFailed = (isErrFlagSet(2) .or. isErrFlagSet(3) .or. isErrFlagSet(4) .or. isErrFlagSet(6))
            enddo
            Cont=.false.
            if(errFlags > 0) then ! success?
                errMsg = errFlagsToString()
                TEXT = errFlagsGetMessages()
                write(*,'(a,/,a,/,"tol=",1pe11.3)') trim(errMsg),trim(TEXT),tol
                write(UT,'(a,/,a,/,"tol=",1pe11.3)') trim(errMsg),trim(TEXT),tol
                write(*, '("K=",1p,9(10E12.5,:/))') (K(i), i=1,nk)
                write(UT,'("K=",1p,9(10E12.5,:/))') (K(i), i=1,nk)
                write(*,'("X=",1p,20E11.3)') (X(i), i=1,NA)
                write(UT,'("X=",1p,20E11.3)') (X(i), i=1,NA)
                write(*,'(a)') trim(TEXT)
                call printChemSystem (UT)
                call factorPrint (UT)
                tol = tolHalta0; tolLogF = tolLogF0
                dbg=5
                Cont=.false.
                CALL HaltaCalc
                call ErrStop
            endif
            tol = tolHalta0; tolLogF = tolLogF0
        endif
    endif

    if(RURIK == 2 .and. firstTime) then
        !call printChemSystem (UT)
        !call factorPrint (UT)
        call printConcs (UT)
    endif

    ! if(RURIK == 2 .and. &
            ! ((abs(x(3)-6.4d-2)<0.001d0 .and. abs(x(7)-3.2d-2)<0.001d0) &
            ! .or. (abs(x(3)-2.08d-1)<0.001d0 .and. abs(x(7)-1.04d-1)<0.001d0)) &
            ! ) then
        ! call factorPrint (UT)
        ! call printConcs (UT)
    ! endif

    ! setType = AS(1): -1= log(act coeff) data, -2= osmotic coeff, n= gypsum (CaSO4.2H2O) solubility, reaction nbr "n"
    IF(setType > 0) THEN ! get the gypsum (CaSO4.2H2O) solubility
        if(calcFailed) then
            Y = 50.d0
            RETURN
        endif
        if((.not.containsSO4 .and. .not.containsCa) .or. &  ! an electrolyte such as NaCl or
              containsSO4) then   ! an electrolyte such as H2SO4
            Y = solub(iCa);
        else if(containsCa) then  ! an electrolyte such as CaCl2
            Y = solub(iSO4);
        else
            write(*,'("Error: containsCa and containsSO4 are both true?")')
            call ErrStop
        endif
    ELSE IF(setType == -2) THEN ! get the mean ionic osmotic coefficient
        if(calcFailed) then
            Y = 100.d0
            RETURN
        endif
        saltConc = 0.d0
        Do ia = 1, Na
            if(ia == jWater) cycle
            saltConc = saltConc + x(ia+1)
        EndDo
        Y =  phi * sumM / saltConc;
    ELSE ! setType == -1
        ! get the mean ionic activity coefficient
        ! (defined only for electrolytes containing a single cation and a single anion)
        if(calcFailed) then
            Y = 1000.d0
            RETURN
        endif
        logGpm = 0.d0; logC = 0.d0; logQ = 0.d0
        Do ia = 1, Na
            if(ia == jWater) Cycle
            nu_ion = as(rs,ia+1);
            if(nu_ion > 0.d0) then
                logGpm = logGpm + nu_ion * logf(ia);
                if(C(ia) <= 0.d0) then
                    write(*,'("ia=",i0,", rs=",i0,", nu_ion=",1PE11.3,", C(",a,")=",E11.3)') ia,rs,nu_ion,trim(identC(ia)),C(ia)
                    logC = logC + nu_ion * (-40.d0);
                else
                    logC = logC + nu_ion * log10(C(ia));
                endif
                logQ = logQ + nu_ion * log10(nu_ion);
            endif
        EndDo
        Y = ((logGpm + logC - logQ) / nu) - log10(saltConc);
    ENDIF ! setType

  RETURN

END SUBROUTINE SIT

!-----------------------------------------------------------------------------
SUBROUTINE M_ox2 (Solubility,X)
! calculates the solubility of M(ox)2.6H2O when
!               [HX]_added, [HNO3]_added,
!               [NaX]_added and [H2ox]_added are known
!  (X = ClO4)
!
! components:   H+, Na+, ClO4-, NO3-, H2ox, M(ox)2.6H2O(s)
! complexes:
! Hox-,ox-2,Np+4,NpOH+3,Np(OH)2+2,NpNO3+3,Np(ox)+2,Np(ox)2,Np(ox)3-2
USE CHEM
USE HALTAF_Module
USE FACTOR_Module, ONLY : temperature, pressure, factorPrint
USE SIT
Implicit NONE
Real(dp), INTENT(IN) :: X(:)
Real(dp), INTENT(OUT) :: Solubility

Real(dp) :: aH2O, w
Integer :: i,j,li,ix, Na2, zCat, zAn
SAVE

!-------------------
    IF(firstTime) THEN
        Na=6 ! Na=number components
        Na2=Na+2
        Nx=0 ! Nx = complex number
        ! the electric charge is given as ak(i,Na+2)
        ! if ak(i,Na+2)=99, then K(i) is the epsilon
        ! for the preceding complex with the background electrolyte
        Do  i=1,NK
            if(nint(AK(i,Na2)) /= 99) Nx = Nx+1
        End Do
        if(Nx < 5) then
            Write(UT,100) Nx,ix
            Write(*,100) Nx,ix
            call ErrStop
        endif
        nIon = NA + Nx
        MSOL = 0 ! no solids
        ! MS = total nr of species (components (soluble or solid) + soluble complexes + solid reaction products)
        MS = NA + NX + MSOL
        ! allocate memory arrays
        Call CHEM_MEM_ALLOC
        Call HALTAF_MEM_ALLOC

        ! debug:
        DBG=nint(AG(2))
        !DBG=1      ! report errors only
        if(dbg > 0) open(unit=IDEBG,file='sit_dbg.out',status='unknown')

        ix = 0
        Do  i=1,NK
            ! the electric charge is given as ak(i,Na+2)
            ! if ak(i,Na+2)=99, then K(i) is the epsilon
            ! for the preceding complex with the background electrolyte
            if(nint(AK(i,Na2)) == 99) Cycle
            ix = ix+1
            ! get the stoichiometry; 1st index is complex nbr,  2nd index is component
            Do li=1,Na
                a(ix,li) = ak(i,li+1)
            End Do
            if(dbg > 0) write(IDEBG,'(a,i2,a,20F6.2)',Advance='No')' a(',ix,',*)=',(a(ix,li),li=1,Na)
            ! get electric charge of complexes
            j = Na + ix
            Z(j) = nint(AK(i,Na2))
            if(dbg > 0) write(IDEBG,'(a,i2,a,i3)') ' Z(',j,')=',z(j)
        End Do
        Z(nIon+1)=1
        Z(nIon+2)=-1
        ! electric charges for components: H+, Na+, ClO4-, NO3-, H2ox, M(ox)2.6H2O(s)
        Z(1:NA)= (/ +1, +1, -1, -1, 0, 0 /)
        IDENTC(1)="H+"; IDENTC(2)="Na+"; IDENTC(3)="ClO4-"; IDENTC(4)="NO3-"; IDENTC(5)="H2ox"; IDENTC(6)="M(ox)2.6H2O(s)"
        IDENT(1:NA) = IDENTC(1:NA)
        !Concentrations that are given:
        !     KH=1: total concentrations given
        !     KH=2: log activity given
        ! components:   H+, Na+, ClO4-, NO3-, H2ox, M(ox)2.6H2O(s)
        KH(1:NA)= (/ 1, 1, 1, 1, 1, 2 /)
        ! do not consider the formation of M(ox)2.6H2O(s) in aqueous solution
        Do i=1,NA+Nx
            Noll(i)=.false.
        End Do
        Noll(Na)=.true.
        JWATER = -1 ! no H2O
        TOL=1.D-8  ! tolerance when solving mass-balance equation
        tolLogF = 1E-6 ! tolerance when iterating activity coefficients
        IOUT = 0   ! debug printout to the terminal
        !IOUT = UT  ! debug printout to the output file
        ! Activity coefficients:
        ! -1 = ideal solutions
        !  0 = Davies Eqn.
        !  1 = SIT (specific ion interaction model)
        activityCoeffsModel = nint(AG(1));
        ionicStr = -1.d0 ! calculate the ionic strength
        Temperature = 25.d0; Pressure = 1.d0
        if(activityCoeffsModel == 1) then !SIT
            ! --- allocate memory
            call SIT_MEM_ALLOC (nIon)
            ! ------ Set default values for the specific interaction coefficient.
            !call setDefaultSITvalues (UT)
            DO i=1,nIon+2
                IF(i<=nIon) THEN
                    IF(NOLL(i) .or. Z(i) == 0) Then
                        Do j=1,nIon+2
                            call setEps(i,j,0.d0)
                        EndDo
                    Cycle
                    End If
                ENDIF
                DO j=1,nIon+2
                    call setEps(i,j,0.d0)
                    if(Z(i)*Z(j) >= 0) Cycle
                    if(Z(i) >0) then
                        zCat = Z(i)-1
                        zAn  = Z(j)+1
                    else
                        zAn  = Z(i)+1
                        zCat = Z(j)-1
                    endif
                    call setEps(i,j, 0.150001d0 + 0.15d0 * real((zAn+zCat),dp))
                END DO
            END DO
            call setEps(nIon+1,nIon+2,0.03d0) ! eps(Na+,Cl-)
            ! species:  H+, Na+, ClO4-, NO3-, H2ox, M(ox)2.6H2O(s),
            !           1    2     3     4     5     6
            ! Hox-,ox-2,Np+4,NpOH+3,Np(OH)2+2,NpNO3+3,Np(ox)+2,Np(ox)2,Np(ox)3-2,Np(ox)4-4
            !  7    8    9     10     11       12       13      14      15        16
            IDENT(Na+1)="Hox-";    IDENT(Na+2)="ox-2";     IDENT(Na+3)="M+4";
            IDENT(Na+4)="MOH+3";   IDENT(Na+5)="M(OH)2+2"; IDENT(Na+6)="MNO3+3";
            IDENT(Na+7)="M(ox)+2"; IDENT(Na+8)="M(ox)2";   IDENT(Na+9)="M(ox)3-2";
            IDENT(Na+10)="M(ox)4-4";
            !--- Np(ox)3-2,Np(ox)4-4
            !call setEps(1,15,-0.50d0)
            !call setEps(1,16,-0.50d0)
            !call setEps(2,15,-0.20d0)
            !call setEps(2,16,-0.20d0)
            !--- H+: ClO4-, NO3-
            call setEps(1,3, 0.14d0)
            call setEps(1,4, 0.07d0)
            !--- Na+: ClO4-, NO3-
            call setEps(2,3,-0.08d0)
            call setEps(2,4,-0.06d0)
            !--- Hox-: H+
            call setEps(1,7, 0.15d0)
            !--- Hox-: Na+/NH4+
            call setEps(2,7,-0.07d0)
            !-- ox-2: Na+/NH4+
            call setEps(2,8,-0.08d0)
            !--- Np+4: ClO4-, NO3-
            call setEps(3,9, 0.84d0)
            call setEps(4,9, 0.84d0)
            !--- NpOH+3: ClO4-, NO3-
            call setEps(3,10, 0.50d0)
            call setEps(4,10, 0.50d0)
            !--- Np(OH)2+2: ClO4-, NO3-
            call setEps(3,11, 0.30d0)
            call setEps(4,11, 0.30d0)
            !--- NpNO3+3: ClO4-, NO3-
            call setEps(3,12, 0.70d0)
            call setEps(4,12, 0.70d0)

            if(dbg > 0) then
                call factorPrint (IDEBG)
            endif

        endif ! activityCoeffsModel = 1 (SIT)

    ENDIF ! firstTime

    ix=0 ! ix = complex number
    Do  i=1,NK
        if(nint(AK(i,Na2)) == 99) then
            ! the electric charge is given as ak(i,Na+2)
            ! if ak(i,Na+2)=99, then K(i) is the epsilon
            ! for the preceding complex with the background electrolyte
            if(ix >= 1 .and. abs(K(i)) >= 1.d-24 .and. activityCoeffsModel == 1) then
                j=ix+Na
                if(Z(j) > 0) then
                    call setEps(j,4,K(i)) ! eps(j,NO3-)
                    call setEps(j,3,K(i)) ! eps(j,ClO4-)
                else if(Z(j) < 0) then
                    call setEps(j,1,K(i)) ! eps(j,H+)
                    call setEps(j,2,K(i)) ! eps(j,Na+)
                endif
            endif
            Cycle
        endif
        ix = ix+1
        lBeta(ix)=-99999.D0; if(K(i) > 0.D0) lBeta(ix)= log(K(i))/ln10 + AK(i,1)
        if(dbg > 0 .and. firstTime) write(IDEBG,'(a,i2,a,f15.10)')" logK(",ix,")=",lBeta(ix)
    End Do
    If(ix /= Nx) Then
        Write(UT,100) Nx,ix
        Write(*,100) Nx,ix
        100 FORMAT("Error: number of complexes has changed. Nx = ",I0,", now found ",I0)
        call ErrStop
    EndIf
    ! for MNO3+3, MOH+3, and M(OH)2+2  the constants correspond to:
    !         M+4 + NO3- = MNO3+3    &   Np+4 + H2O =NpOH+3 + H+
    !                                &   Np+4 + 2 H2O =Np(OH)2+2 + 2H+
    ! change to: M(ox)2(s) + NO3- + 4 H+ = MNO3+3 + 2H2ox    &
    !               M(ox)2(s) + H2O + 3 H+ = MOH+3 + 2H2ox, etc
    lBeta(4) = lBeta(4) + lBeta(3)
    lBeta(5) = lBeta(5) + lBeta(3)
    lBeta(6) = lBeta(6) + lBeta(3)
    ! make corrections for the activity of water
    aH2O = X(6);
    Do i=1,Nx
        if(activityCoeffsModel >= 0 .and. a(i,Na) > 0d0 .and. aH2O > 0.d0) then
            !if(dbg > 0 .and. firstTime) write(IDEBG,'("a(",i2,",Na)=",F8.3," aH2O=",F8.3)') i,a(i,Na),aH2O
            lBeta(i) = lBeta(i)-6.d0*log10(aH2O)*a(i,Na)
        endif
        if(dbg > 0 .and. firstTime) write(IDEBG,'(a,i2,a,f15.10)')" logK(",i,")=",lBeta(i)
    End Do

    if(dbg > 0 .and. firstTime .and. activityCoeffsModel == 1) then !SIT
        write(IDEBG,*)"species:  H+, Na+, ClO4-, NO3-, H2ox, M(ox)2.6H2O(s),"
        write(IDEBG,*)"          1    2     3     4      5      6"
        write(IDEBG,*)" 7    8    9   10     11       12     13      14     15"
        write(IDEBG,'(1x,a,i3)') "Nx=",Nx
        write(IDEBG,'(1x,a,25i3)') "z=",(z(i),i=1,Na)
        write(IDEBG,'(1x,25i3)') (z(i),i=Na+1,Na+Nx)
        Do i=1,Na+Nx+2
            if(i <= Na+Nx) then
                write(IDEBG,'(a,i2)') " i=",i
            else
                if(i == Na+Nx+1) write(IDEBG,*) "for electrical imbalance: Na+"
                if(i == Na+Nx+2) write(IDEBG,*) "for electrical imbalance: Cl-"
            endif
            do j=1,Na+Nx+2
                w = getEps(i,j)
                if(abs(w)>1.d-10 .and. w < 1000.d0) write(IDEBG,'(a,i2,2i3,f10.6)') " j,z(i),z(j),eps =",j,z(i),z(j),w
            end do
        End Do
    endif

    ! total concentrations for the components
    ! Components:   H+, Na+, ClO4-, NO3-, H2ox, M(ox)2.6H2O(s)
    tot(1)= x(1);  if(x(1) > 1.E-20) logA(1) = log10(x(1))  ! H+
    tot(2)= x(2);  if(x(2) > 1.E-20) logA(2) = log10(x(2))/ln10  ! Na+
    tot(3)= x(3);  if(x(3) > 1.E-20) logA(3) = log10(x(3))/ln10  ! X-
    tot(4)= x(4);  if(x(4) > 1.E-20) logA(4) = log10(x(4))/ln10  ! NO3-
    tot(5)= x(5);  if(x(5) > 1.E-20) logA(5) = log10(x(5))/ln10  ! H2ox
    logA(6) = 0.d0 ! M(ox)2.6H2O(s)

    if(dbg > 0 .and. firstTime) Call printChemSystem (IDEBG)

    CONT=.false.
    !---------------------
    Call HaltaCalc
    !---------------------

    IF(errFlags /= 0) THEN !------ Error Reporting --------------
        WRITE(*,991) tot(1),trim(errFlagsToString()), trim(errFlagsGetMessages())
        991 FORMAT("Equilibrium calculations in HaltaFall failed for Htot=",1PE13.5,1x,A,/,A)
        solubility = -1.D0
    ELSE
        solubility = tot(6)
        if(dbg > 0 .and. firstTime) call printConcs (IDEBG)
    ENDIF

    if(dbg > 0 .and. firstTime) close(UNIT=IDEBG)
    firstTime = .false.
    RETURN

END SUBROUTINE M_ox2

END MODULE PutsUbbe