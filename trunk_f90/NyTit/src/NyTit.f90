MODULE PutsUbbe
! LETAGROP MOD/85 (IBM XT FEBRUARY 1986***/FORTRAN H)
! SPEFO for IBM-PC
! References:
! - Brauner P., Sillén L.G., Whiteker R. (1969) High-speed computers
!   as a supplement to graphical methods. 9. Adjustment for systematic
!   experimental errors and other "group parameters" in LETAGROP:
!   Applications to potentiometric titrations. Arkiv Kemi 31, 365.
! - Arnek R., Sillén L.G., Wahlberg O. (1969) High-speed computers as
!   a supplement to graphical methods. 8. Some devices to speed up
!   computations on chemical equilibria and simplify proramming for
!   LETAGROP. Application to complex formation. Arkiv Kemi 31, 353.
! - Sillén L.G., Warnqvist B. (1969) High-speed computers as a supplement
!   to graphical methods. 7. Model selection and rejection with LETAGROP:
!   Elimination of species with negative or "insignificant" equilibrium
!   constants. Arkiv Kemi 31, 341.
! - Sillén L.G., Warnqvist B. (1969) High-speed computers as a supplement
!   to graphical methods. 6. A strategy for two-level LETAGROP adjustment
!   of common and "group" parameters. Some features that avoid divergence.
!   Arkiv Kemi 31, 315.

USE READIR, ONLY: INFL
USE LetaGropModule, ONLY : dp, UT
Implicit NONE

SAVE

CONTAINS

!-----------------------------------------------------------------------------
SUBROUTINE NAMN
  USE IO, ONLY : ErrStop
  Character(len=64) :: FILIN="LetaGrop.dat",FILOUT="LetaGrop.out",TEMP1,TEMP2
  Character(len=1) :: BLANK=" "
  Integer :: IOERR
  Character(len=30) TSTR,DSTR
  SAVE
  WRITE(*,1)
  1 FORMAT (" *** LETAGROP - for the IBM-PC ***",27x,"(I.Puigdomenech)",/," *** NYTIT Oct-1975. VERSIO PUIG ANTICH 79 ***",/)
  WRITE(*,'(4x,"Input and output file specification",/)')
  call getcmd (TEMP1,TEMP2)
! Open Input-Output files for LETAGROP
  Do While (.true.)
    If(TEMP1 == BLANK) Then
        100 WRITE(*,'(/,"Enter the name of the input file  [",A,"]:")') trim(FILIN)
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
        200 WRITE(*,'("Enter the name of the output file  [",A,"]:")') trim(FILOUT)
        READ(*,'(A)',ERR=200,END=9999) TEMP2
    EndIf
    IF(TEMP2 /= BLANK) FILOUT=TEMP2
    OPEN(UT,FILE=FILOUT,STATUS='UNKNOWN',IOSTAT=IOERR)
    IF(IOERR == 0) Exit ! do while true
    WRITE(*,'("?? Could not open file:",/,A)') trim(FILOUT)
    TEMP2 = BLANK
  EndDo ! While true
!
!  CALL DATE AND TIME
!
WRITE(UT,1)
Call TimDat (DSTR,TSTR)
Write (*,841)  FILIN,trim(DSTR),FILOUT,trim(TSTR)
Write (UT,841) FILIN,trim(DSTR),FILOUT,trim(TSTR)
841 FORMAT (1X,A,"DATE ",A,/,1X,A,"TIME ",A)
Write(UT,'(1X)')

RETURN

9999  call ErrStop

END SUBROUTINE NAMN

!-----------------------------------------------------------------------------
SUBROUTINE CloseFiles
INTEGER :: ioErr
Close (Unit=INFL,iostat=ioErr)
Close (Unit=UT,iostat=ioErr)
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
SUBROUTINE GetCmd (fname1,fname2)
  Implicit NONE
  Character (LEN=*), INTENT(OUT) :: fname1,fname2
  Integer :: count
  fname1 = " "
  fname2 = " "
! Minimalist Gnu for Windows
  count = command_argument_count()
  if(count >= 1) then
    call get_command_argument(1, fname1)
    if(count >= 2) then
        call get_command_argument(2, fname2)
    endif
  endif
Return
END SUBROUTINE GetCmd

!-----------------------------------------------------------------------------
SUBROUTINE PUTS
  USE LetaGropModule
  Implicit NONE
  Real(dp) :: QZ
  Integer :: LRP, LNPRS
      IF(TYP > 3) GO TO 17
      IF(RS /= 0) GO TO 10
      QZ=0.5*(NAS-2.+0.05)
      IF(QZ < 0.) GO TO 3
      NKOM=INT(QZ)
      GO TO 6
    3 QZ=ABS(QZ)
      NKOM=INT(QZ)
      IF(abs(NKOM-QZ) < 1.D-10) GO TO 5 !IF(NKOM == QZ) GO TO 5
      NKOM=-NKOM-1
      GO TO 6
    5 NKOM=-NKOM
    6 CONTINUE
      IF(TYP /= 3) GO TO 8
      NAPA=4+2*NKOM
      ARUM=5+NKOM
      GO TO 9
    8 NAPA=3+2*NKOM
      ARUM=4+NKOM
    9 CONTINUE
      RETURN

   10 IF(TYP /= 3) GO TO 12
      CALL TITER (3)
      GO TO 13
   12 CALL TITER (2)
   13 IF(TYP /= 2) RETURN
      M=APCELL(RS)+ARUM-NAPA
      LNPRS=NP(RS)
      DO 15 LRP=1,LNPRS
      M=M+NAPA
      AP(M)=-16.
   15 CONTINUE
      RETURN

! FLEBUR
   17 IF(RS /= 0) GO TO 18
      NKOM=AG(3)
      GO TO 20
   18 M=NAPA-2*NKOM-1
      CALL TINBUR(M)
      IF(TYP /= 5) GO TO 20
      M=APCELL(RS)+ARUM+NAPA
      LNPRS=NP(RS)
      DO 19  LRP=1,LNPRS
      M=M+NAPA
      AP(M)=-16
   19 CONTINUE
   20 CONTINUE
      RETURN
END SUBROUTINE PUTS

!-----------------------------------------------------------------------------
SUBROUTINE TITER (TRUM)
  USE LetaGropModule
  Implicit NONE
  Integer, INTENT(IN) :: TRUM
  Real(dp) :: V0,V00,VFAK,VT,VTOT,AT(5),A0V0(5),MOLA(5)
  Integer :: LI,LJ, LQ, LZ, LRP, NPRS1, NKOM1
      V0=AS(RS,2*NKOM+1)
      V00=V0
      VFAK=AS(RS,2*NKOM+2)
      DO 1 LI=1,NKOM
      AT(LI)=AS(RS,NKOM+LI)
      A0V0(LI)=V0*AS(RS,LI)
      MOLA(LI)=A0V0(LI)
    1 CONTINUE
      CELL=APCELL(RS)
      LRP=1
  100 IF(LRP < 1) RETURN
      VT=AP(CELL+1)
      IF(VT < 0.) GO TO 50
      VTOT=V0+VT*VFAK   !### Warning: 'v0' may be used uninitialized in this function
      LQ=CELL+TRUM+NKOM+1
      AP(LQ)=V00/VTOT
      LQ=CELL+TRUM
      DO 2 LI=1,NKOM
        MOLA(LI)=A0V0(LI)+VT*AT(LI)
        AP(LQ+LI)=MOLA(LI)/VTOT
    2 CONTINUE
      GO TO 9999
! BYT
   50 J=1
      M=0
      NKOM1=NKOM+1
      DO 7 LI=1,NKOM1
      IF(J+1 <= NAP) GO TO 4
      J=0
      M=M+1
    4 J=J+1
      LQ=CELL+M*NAPA+J
      W=AP(LQ)
      LQ=CELL+M*NAP+J
      AP(LQ)=W
      IF(LI /= NKOM1) GO TO 6
      VFAK=W
      GO TO 7
    6 AT(LI)=W
    7 CONTINUE
      IF(M <= 0) GO TO 9999
      NPRS1=NP(RS)-LRP-1
      IF(NPRS1 < 1) GO TO 91
      DO 9 LJ=1,NPRS1
        DO 8 LI=1,NAP
          LQ=CELL+LJ*NAPA+LI
          LZ=CELL+(LJ+M)*NAPA+LI
          AP(LQ)=AP(LZ)
    8   CONTINUE
    9 CONTINUE
   91 CONTINUE
      NP(RS)=NP(RS)-M-1
      LRP=LRP-1
      DO 10 LI=1,NKOM
        A0V0(LI)=MOLA(LI)
   10 CONTINUE
      V0=VTOT
! SLUT
 9999 CELL=CELL+NAPA
      IF(LRP >= NP(RS)) RETURN
      LRP=LRP+1
      GO TO 100
END SUBROUTINE TITER

!-----------------------------------------------------------------------------
SUBROUTINE UBBE
  USE LetaGropModule
  USE BDTV
  USE L3, ONLY : SATSUT, UVAR
  Implicit NONE
  Real(dp) :: ATOTU, ULOLN
  Integer :: LI,LQ, LIX, LIBUR, NKOM1,NKOM2, NBUR1

  U=0
  IF(.not.TAGE) RSL = 1 ! RS = RS1 ! Tage=false means upper level adjustment of "k" ! modified Aug.2021
  RS = RSI(RSL)   ! added Aug.2021
  GO TO 340
! NYSA
  310 RP=0
      IF(TYP > 3) CALL NYNAPA
      CELL=APCELL(RS)-NAPA
      IF(RURIK == 2) GO TO 700
      GO TO 500
! NYP
  320 RP=RP+1
      CELL=CELL+NAPA
      IF(RP <= NP(RS)) GO TO 6
      IF(RSL >= RSN .or. TAGE) GO TO 3 ! IF(RS2<=RS .OR. TAGE) GO TO 3  !  changed Aug.2021
      RSL = RSL+1 ! RS=RS+1 ! change Aug.2021
      RS = RSI(RSL)         ! added Aug.2021
      GO TO 310
    3 IF(SKRIUT == 1.OR.(SKRIUT == 0.AND..NOT.TAGE).OR.(PROV.AND..NOT.(TAGE.AND.SKRIUT < 0))) GO TO 4
      GO TO 5
    4 CALL UVAR (UT)
      IF(PROV) WRITE (UT,45)
      IF(PROV) WRITE (*,45)
    5 CONTINUE
      INDIC=1700
      RETURN
    6 GO TO 800
! UBER
  330 U=U+FEL(VAL)**2
      IF(RURIK == 2) GO TO 900
      GO TO 320
! KAG
  340 TEMP=AG(1)
      EFAK=ROF*(TEMP+273.15D0)
      IF(TYP /= 1 .AND. TYP /= 4) EBFAK=EFAK/AG(2)
      IF(TYP >= 4) GO TO 8
      NBUR=1
      TRUM=2
      IF(TYP == 3) TRUM=3
    8 JAC=K(1)
      IF(AG(4) >= 0.5) GO TO 66
      KHX=K(3)
      I1=4
      I2=NK
      CALL BETAIN
      IF(.NOT.INDIK) GO TO 65
      INDIC=9
      RETURN
   66 JB=K(3)
      JC=K(4)
      JAC2=K(4)
      KHX=K(5)
      I1=6
      I2=NK
      CALL BETAIN
      IF(.NOT.INDIK) GO TO 65
      INDIC=9
      RETURN
   65 KWJALK=K(2)*BETA(1)
      DIRT=1
      GO TO 310
! SATSA1
  410 WRITE (UT,46)
      WRITE (*,46)
      GO TO 500
! SATSA2
  420 WRITE (UT,47)
      WRITE (*,47)
      GO TO 500
! SATSA3
  430 WRITE (UT,48)
      WRITE (*,48)
      GO TO 500
! SATSA4
  440 GO TO 410
! SATSA5
  450 GO TO 420
! SATSA6
  460 GO TO 430
! ASOKS
  500 E0=KS(RS,1)
      E0B=KS(RS,2)
      DA0=KS(RS,3)
      DAT=KS(RS,4)
      DA=KS(RS,5)
      CHX=KS(RS,6)
      CHXALL=KS(RS,7)
      BFAK=1+KS(RS,8)
      IF(TYP > 3) CELL=CELL+2*NAPA
      GO TO 320
! HUVUD
  700 CALL SATSUT (UT)
      IF(TYP <= 3) GO TO 11
      M=APCELL(RS)-NKOM-1
      WRITE (UT,49)
      WRITE (*,49)
      NKOM1=NKOM+1
      DO 10 LIBUR=1,NBUR
      M=M+NKOM+1
      LI=M+1
      NKOM2=M+NKOM1
      WRITE (UT,50) (AP(LQ),LQ=LI,NKOM2)
      WRITE (*,50) (AP(LQ),LQ=LI,NKOM2)
   10 CONTINUE
   11 GO TO (410,420,430,440,450,460),TYP
! APIN
  800 CALL TITUT
      IF(INDIK) GO TO 320
      ATOT=ATOT+DA0*DIL+DAT*(1-DIL)+DA
      BTOT=BTOT*BFAK
      GO TO (810,820,830,840,850,860),TYP
! APFEL1
  810 E=AP(CELL+NBUR+1)
      IF(VAL /= 4) GO TO 13
      BA=3
      CALL VALHAL
      GO TO 14
   13 CALL HURE
      LNA=LNH
      BA=2
      CALL VALHAL
   14 IF(VAL >= 4 .AND. RURIK /= 2) GO TO 21
      X=LNA
      DO 56 LIX=1,NX
        TAL(LIX)=P(LIX)
   56 CONTINUE
      CALL TOTBER
      CALL DIRTY
      I=IDINT(AG(8)+1.D-1)
      IF(I /= 1) GO TO 70
      NKOM2=2*NKOM
      Y=Y+DEXP(X)*((AS(RS,NKOM2+3)+AP(CELL+NBUR))/AS(RS,NKOM2+1)-1.D0)
! FOR TWO-PHASE EMF DATA
   70 FEL(1)=1000*(Y-ATOT)
      IF(BTOT <= 0.) GO TO 17
      FEL(2)=FEL(1)/BTOT
      GO TO 18
   17 FEL(2)=0.
   18 IF(NKOM <= 2 .OR. CTOT <= 0.) GO TO 20
      FEL(3)=FEL(1)/CTOT
      GO TO 21
   20 FEL(3)=0.
   21 IF(VAL /= 4 .AND. RURIK /= 2) GO TO 23
      CALL EJBER
      Y=E0+EFAK*LNA+EJ
      FEL(4)=Y-E
   23 GO TO 330
! APFEL2
  820 EB=AP(CELL+NBUR+1)
      IF(VAL /= 1) GO TO 25
      LNA=AP(CELL+ARUM)
      CALL EJBER
      LNB=(EB-E0B-EJ)/EBFAK
      BA=1
      CALL VALHAL
      GO TO 26
   25 BA=3
      CALL VALHAL
   26 IF(VAL /= 1 .AND. RURIK /= 2) GO TO 28
      X=LNB
      DO 57 LIX=1,NX
        TAL(LIX)=Q(LIX)
   57 CONTINUE
      CALL TOTBER
      FEL(1)=1000*(Y-BTOT)
   28 IF(VAL /= 2 .AND. RURIK /= 2) GO TO 30
      CALL EJBER
      Y=E0B+EBFAK*LNB+EJ
      FEL(2)=Y-EB
   30 GO TO 330
! APFEL3
  830 E=AP(CELL+NBUR+1)
      EB=AP(CELL+NBUR+2)
      IF(VAL >= 3 .AND. VAL /= 5 .AND. VAL < 7) GO TO 32
      CALL HURE
      LNA=LNH
   32 IF(VAL >= 5.AND.VAL < 7) GO TO 36
      IF(VAL > 2.AND.VAL < 7) LNA=AP(CELL+ARUM)
      CALL EJBER
      LNB=(EB-E0B-EJ)/EBFAK
      IF(VAL >= 3.AND.VAL < 7) GO TO 35
      BA=0
      CALL VALHAL
      GO TO 36
   35 BA=1
      CALL VALHAL
   36 IF(VAL /= 5) GO TO 59
      BA=2
      CALL VALHAL
   59 IF(VAL /= 6) GO TO 61
      BA=3
      CALL VALHAL
   61 IF(VAL /= 1 .AND. VAL /= 7 .AND. RURIK /= 2) GO TO 38
      X=LNA
      DO 62 LIX=1,NX
        TAL(LIX)=P(LIX)
   62 CONTINUE
      CALL TOTBER
      CALL DIRTY
      FEL(1)=1000*(Y-ATOT)
      IF(BTOT <= 0.) GO TO 67
      FEL(7)=FEL(1)/BTOT
      GO TO 38
   67 FEL(7)=0.
   38 IF(VAL /= 2 .AND. VAL /= 4 .AND. VAL /= 8 .AND. RURIK /= 2) GO TO 40
      X=LNB
      DO 63 LIX=1,NX
        TAL(LIX)=Q(LIX)
   63 CONTINUE
      CALL TOTBER
      FEL(2)=1000*(Y-BTOT)
      FEL(4)=FEL(2)
      IF(BTOT <= 0.) GO TO 68
      FEL(8)=FEL(2)/BTOT
      GO TO 40
   68 FEL(8)=0.
   40 IF(VAL /= 3 .AND. RURIK /= 2) GO TO 42
      CALL EJBER
      Y=E0+EFAK*LNA+EJ
      FEL(3)=Y-E
   42 IF(VAL /= 5 .AND. VAL /= 6 .AND. RURIK /= 2) GO TO 44
      CALL EJBER
      Y=E0B+EBFAK*LNB+EJ
      FEL(5)=Y-EB
      FEL(6)=FEL(5)
   44 GO TO 330
! APFEL4
  840 GO TO 810
! APFEL5
  850 GO TO 820
! APFEL6
  860 GO TO 830
! UTTA
  900 CALL ZETA
      LI=CELL+1
      NBUR1=CELL+NBUR
      IF(TYP > 3) WRITE (UT,51) (AP(LQ),LQ=LI,NBUR1)
      IF(TYP > 3) WRITE (*,51) (AP(LQ),LQ=LI,NBUR1)
      ATOTU=1000*ATOT
      ULOLN=LOGE*LNA
      GO TO (910,920,930,940,950,960),TYP
! UTTAG1
  910 WRITE (UT,52) AP(CELL+1),E,ATOTU,ULOLN,ZAB,ZAC,ETA,FEL(1),FEL(2),FEL(3),FEL(4)
      WRITE (*,52) AP(CELL+1),E,ATOTU,ULOLN,ZAB,ZAC,ETA,FEL(1),FEL(2),FEL(3),FEL(4)
      GO TO 320
! UTTAG2
  920 WRITE (UT,53) AP(CELL+1),EB,ATOTU,ULOLN,ZAB,ZAC,ETA,FEL(1),FEL(2)
      WRITE (*,53) AP(CELL+1),EB,ATOTU,ULOLN,ZAB,ZAC,ETA,FEL(1),FEL(2)
      GO TO 320
! UTTAG3
  930 WRITE (UT,54) AP(CELL+1),E,EB,ATOTU,ULOLN,ZAB,ZAC,ETA,FEL(1),FEL(7),FEL(3),FEL(2),FEL(8),FEL(5)
      WRITE (*,54) AP(CELL+1),E,EB,ATOTU,ULOLN,ZAB,ZAC,ETA,FEL(1),FEL(7),FEL(3),FEL(2),FEL(8),FEL(5)
      GO TO 320
! UTTAG4
  940 GO TO 910
! UTTAG5
  950 GO TO 920
! UTTAG6
  960 GO TO 930
   45 FORMAT (/)
   46 FORMAT (//4X,"V",5X,"EA(MV)",1X,"ATOT(MM)",1X,"LOGA",2X,"Z A/B", &
        2X,"Z A/C",2X,"ETA",5X,"DA",5X,"DA/B",3X,"DA/C",3X,"DEA")
   47 FORMAT (//4X,"V",5X,"EB(MV)",1X,"ATOT(MM)",1X,"LOGA",2X,"Z A/B", &
        2X,"Z A/C",2X,"ETA",5X,"DB",5X,"DEB")
   48 FORMAT (//4X,"V",5X,"EA(MV)",1X,"EB(MV)",1X,"ATOT(MM)",2X,"LOGA", &
        4X,"Z A/B",3X,"Z A/C",3X,"ETA",6X,"DA",5X,"DA/B",3X,"DA", &
        4X,"DB",5X,"DB/B",3X,"DEB")
   49 FORMAT (1X,"HALTER I BYRETTER+VFAK")
   50 FORMAT (2X,7F10.6)
   51 FORMAT (4X,6F7.2)
   52 FORMAT (1X,F7.3,F8.2,F7.2,4F7.3,4F7.2)
   53 FORMAT (1X,F7.3,F8.2,F7.2,4F7.3,2F7.2)
   54 FORMAT (1X,F7.3,3F8.2,4F8.4,6F7.2)
END SUBROUTINE UBBE

!-----------------------------------------------------------------------------
SUBROUTINE EJBER
  USE LetaGropModule
  USE BDTV
  Implicit NONE
  Integer :: EJVAL
  Real(dp) :: SWVAL
      HFRI=DEXP(LNA)
      SWVAL=AG(4)+1.
      EJVAL=INT(SWVAL+0.5)
      GO TO (751,752,753,754),EJVAL
! EJ1
  751 EJ=JAC*HFRI+KWJALK/HFRI
      GO TO 760
! EJ2
  752 EJ=JAC*HFRI+KWJALK/HFRI+JB*DEXP(LNB)+JC*DEXP(LNC)
      GO TO 760
! EJ3
  753 EJ=JAC*HFRI+KWJALK/HFRI+JB*BTOT+JAC2*HFRI*HFRI
      GO TO 760
! EJ4
  754 EJ=JAC*HFRI+KWJALK/HFRI+JB*BTOT+JC*CTOT
! EJUT
  760 CONTINUE
      RETURN
END SUBROUTINE EJBER

!-----------------------------------------------------------------------------
SUBROUTINE HURE
  USE LetaGropModule
  USE BDTV
  Implicit NONE
      EJ=0.
! HOPPE
   25 LNH=(E-E0-EJ)/EFAK
      HFRI=DEXP(LNH)
      LNA=LNH
      CALL EJBER
      EBER=E0+EFAK*LNH+EJ
      IF(DABS(E-EBER) > 0.002) GO TO 25
      RETURN
END SUBROUTINE HURE

!-----------------------------------------------------------------------------
SUBROUTINE NYNAPA
  USE LetaGropModule
  USE BDTV
  Implicit NONE
      NBUR=AS(RS,NKOM+2)
      NAPA=NBUR+2*NKOM+2
      IF(TYP == 6) NAPA=NAPA+1
      ARUM=NAPA-NKOM+1
      TRUM=NBUR+1
      IF(TYP == 6) TRUM=TRUM+1
      RETURN
END SUBROUTINE NYNAPA

!-----------------------------------------------------------------------------
SUBROUTINE TITUT
  USE LetaGropModule
  USE BDTV
  Implicit NONE
  Integer :: LI,LQ,NKOM2
      INDIK=.FALSE.
      M=CELL+TRUM
      IF(AP(CELL+1) >= 0) GO TO 2
      LI=CELL+2
      NKOM2=CELL+2+NKOM
      IF(RURIK == 2) WRITE (UT,3) (AP(LQ),LQ=LI,NKOM2)
      IF(RURIK == 2) WRITE (*,3) (AP(LQ),LQ=LI,NKOM2)
      RP=RP-1
      INDIK=.TRUE.
      RETURN
    2 ATOT=AP(M+1)
      IF(NKOM > 1) BTOT=AP(M+2)
      IF(NKOM > 2) CTOT=AP(M+3)
      IF(NKOM > 3) LTOT=AP(M+4)
      DIL=AP(M+NKOM+1)
      RETURN
    3 FORMAT (/1X,10HBYTE AV AT,10F10.5)
END SUBROUTINE TITUT

!-----------------------------------------------------------------------------
SUBROUTINE ZETA
  USE LetaGropModule
  USE BDTV
  Implicit NONE
  Integer :: NKOM2
      I=IDINT(AG(8)+1.D-1)
      IF(I /= 1) GO TO 10
      NKOM2=2*NKOM
      ABUN=ATOT-DEXP(LNA)*(AS(RS,NKOM2+3)+AP(CELL+NBUR))/AS(RS,NKOM2+1)
      IF(BTOT > 0.) GO TO 1
   10 ABUN=ATOT-DEXP(LNA)
      IF(BTOT <= 0.) GO TO 2
    1 ZAB=ABUN/BTOT
      GO TO 3
    2 ZAB=0.
    3 IF(CTOT <= 0. .OR. NKOM <= 2) GO TO 5
      ZAC=ABUN/CTOT
      GO TO 6
    5 ZAC=0.
    6 IF(BTOT <= 0.) GO TO 8
      ETA=LOGE*(DLOG(BTOT)-LNB)
      GO TO 9
    8 ETA=0.
    9 CONTINUE
      RETURN
END SUBROUTINE ZETA

!-----------------------------------------------------------------------------
SUBROUTINE TINBUR (TRUM)
  USE LetaGropModule
  Implicit NONE
  Integer, INTENT(IN) :: TRUM
  Real(dp) VO,DIL,VTOT,AT(6,4),AOVO(4),MOLA(4),VFAK(6),VT(6)
  Integer :: NBUR
  Integer :: LI,LJ,LQ,LRP,LZ,LNPRS, LIBUR, NKOM2
      NBUR=AS(RS,NKOM+2)
      CELL=APCELL(RS)+NAP
      M=CELL
      DO 2  LI=1,NKOM
      M=M+NAPA-NAP
        DO 1  LJ=1,NAP
          M=M+1
          CELL=CELL+1
          AP(CELL)=AP(M)
    1   CONTINUE
    2 CONTINUE
      CELL=APCELL(RS)+NAPA+NAP
      M=NAPA*(NKOM-1)
      NKOM2=NKOM+2
      LNPRS=NP(RS)
      DO 4  LI=NKOM2,LNPRS
      CELL=CELL+NAPA-NAP
        DO 3  LJ=1,NAP
          CELL=CELL+1
          AP(CELL)=AP(CELL+M)
    3   CONTINUE
    4 CONTINUE
      NP(RS)=NP(RS)-1-NKOM
      M=APCELL(RS)
      DO 6  LIBUR=1,NBUR
        DO 5  LJ=1,NKOM
          M=M+1
          AT(LIBUR,LJ)=AP(M)
    5   CONTINUE
      M=M+1
      VFAK(LIBUR)=AP(M)
    6 CONTINUE
      VO=AS(RS,NKOM+1)
      DO 7  LI=1,NKOM
      AOVO(LI)=VO*AS(RS,LI)
      !## the original line was _after_ the do-loop
      !## changed in 2020 to be inside the loop
      MOLA(LI)=AOVO(LI)
    7 CONTINUE
      CELL=APCELL(RS)+2*NAPA
      DO 11  LRP=1,LNPRS
      VTOT=VO
        DO 8  LIBUR=1,NBUR
          VT(LIBUR)=AP(CELL+LIBUR)
          VTOT=VTOT+VT(LIBUR)*VFAK(LIBUR)
    8   CONTINUE
        LQ=CELL+TRUM+NKOM+1
        AP(LQ)=VO/VTOT
        DIL=AP(LQ)
        DO 10  LI=1,NKOM
          MOLA(LI)=AOVO(LI)
          DO 9  LIBUR=1,NBUR
            MOLA(LI)=MOLA(LI)+VT(LIBUR)*AT(LIBUR,LI)
    9     CONTINUE
          LZ=CELL+TRUM+LI
          AP(LZ)=MOLA(LI)/VTOT
   10   CONTINUE
      CELL=CELL+NAPA
   11 CONTINUE
      RETURN
END SUBROUTINE TINBUR

END MODULE PutsUbbe
