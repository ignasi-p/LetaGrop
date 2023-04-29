MODULE PutsUbbe
! LETAGROP MOD/85 (IBM XT FEBRUARY 1986***/FORTRAN H)
! SPEFO for IBM-PC
! References:
! - Sillén L.G., Warnqvist B. (1969) High-speed computers as a supplement
!   to graphical methods. 10. Application of LETAGROP to spectrophotometric
!   data, for testing models and adjusting equilibrium constants.
!   Arkiv Kemi 31, 377.
! - Sillén L.G., Warnqvist B. (1968) Equilibrium constants and model testing
!   from spectrophotometric data, using LETAGROP. Acta Chem. Scand. 22, 3032.
USE READIR, ONLY: INFL
USE LetaGropModule, ONLY : dp, UT
Implicit NONE
Integer, PRIVATE :: NLAM,NSOLN,NTAB,RLAM,RSOLN
Logical, PRIVATE :: MOLEX
Real(dp), PRIVATE :: FELUT(25),EBERT(25), ELIM

SAVE

CONTAINS

!-----------------------------------------------------------------------------
SUBROUTINE NAMN
  USE IO, ONLY : ErrStop
  Character(len=64) :: FILIN="LetaGrop.dat",FILOUT="LetaGrop.out",TEMP1,TEMP2
  Character(len=1) :: BLANK=" "
  Integer :: UT = 26, IOERR
  Character(len=30) TSTR,DSTR
  SAVE
  WRITE(*,1)
  1 FORMAT ("*** LETAGROP - for the IBM-PC ***",27x,"(I.Puigdomenech)",/,"*** SPEFO (IBM XT VERSION) FEBRUARY-86 ***",/)
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
SUBROUTINE UBBE ! for SPEFO
  USE LetaGropModule
  USE BDTV
  USE L3, ONLY : UVAR
  Implicit NONE
  Real(dp) :: LNBA(25)
  Integer ::  LI, LJ, LJK, LIK

  PrgTyp = 1 !## program type = SPEFO
  NLAM=AG(1)+0.01D0
  NKOM=3
  IF(TYP.EQ.1) NKOM=2
  MOLEX=.TRUE.
  IF(AG(2).LT.1.D0) MOLEX=.FALSE.
  ELIM=AG(3)
  NSOLN=AG(4)+0.01D0
  NTAB=AG(5)+0.01D0
  DIRT=0
  IF(TAGE) GO TO 10
  IF(N.LT.1) GO TO 49
  DO I=1,N,1
    IK=IVAR(I)
    IF(ORVAR.NE.9 .OR. K(IK).GE.100.D0*AG(7)) GO TO 22
    ORVAR=8
    K(IK)=0.D0
22  IF( K(IK).GT. 0.D0) GO TO 32
    K(IK)=0.0005D0*KC(I)
    IF(N.EQ.1) AG(7)= K(IK)
32  CONTINUE
  END DO
49 I1=1
  I2=NK
  CALL BETAIN
  IF(.NOT.INDIK) GO TO 7
  INDIC=9
  RETURN

7 CONTINUE
  KOKS=.TRUE.
  IF(ORVAR.EQ.0 .OR. ORVAR.EQ.-2) CALL TABKOL
  NSKOTT=1
  IF(ORVAR.EQ.0) NSKOTT=2
  IF(RURIK.NE.2) GO TO 62
  RSOLN=0
  GO TO(140,150,160,170,180),TYP
62 CALL WERDA
  RSOLN=0
  GO TO 30

! ALL TABS COME HERE TO 8 OR 9
8 ARUM=NAPA-2
  CTOT=AP(CELL+3)
9 CALL VALHAL
  CALL KOMIN
! NYL
30 RSOLN=RSOLN+1
  CELL=(RSOLN-1)*NAPA
  RP=RSOLN
  IF(RSOLN.GT.NSOLN) GO TO 72
  GO TO( 90,100,110,120,130),TYP
72 CONTINUE
  IF(RURIK.EQ.2) GO TO 82
! GO TO SARK
  INDIC=1600
  RETURN

82 RSOLN=0
  U=0.D0
! SKRIV
60 RSOLN=RSOLN+1
  IF(RSOLN.GT.NSOLN) GO TO 70
  RP=RSOLN
  CELL=(RSOLN-1)*NAPA
  DO RLAM=1,NLAM
    CALL ESPEK
    EBERT(RLAM)=EBER
    FELUT(RLAM)=FEL(VAL)
    U=U+FEL(VAL)**2
  ENDDO
  GO TO 80
! TAB1
90 ATOT=AP(CELL+1)
  BTOT=AP(CELL+2)
  BA=3
  ARUM=NAPA-1
  GO TO 9
! TAB2
100 LNA=LN10*AP(CELL+1)
  BTOT=AP(CELL+2)
  BA=2
  GO TO 8
! TAB3
110 ATOT=AP(CELL+1)
  BTOT=AP(CELL+2)
  BA=3
  GO TO 8
! TAB4
120 ATOT=AP(CELL+1)
  LNB=LN10*AP(CELL+2)
  BA=1
  GO TO 8
! TAB5
130 LNA=LN10*AP(CELL+1)
  LNB=LN10*AP(CELL+2)
  BA=0
  GO TO 8
! HUVUD1
140 WRITE(UT,145)
  WRITE(*,145)
145 FORMAT(5X,"ATOT",7X,"BTOT",/,5X,"E",11X,"EBER",7X,"FEL(VAL)")
  GO TO 30
! HUVUD2
150 WRITE(UT,155)
  WRITE(*,155)
  WRITE(UT,195)
  WRITE(*,195)
155 FORMAT(5X,"LOGA",8X,"BTOT",8X,"CTOT")
  GO TO 190
! HUVUD3
160 WRITE(UT,165)
  WRITE(*,165)
  WRITE(UT,195)
  WRITE(*,195)
165 FORMAT(5X,"ATOT",8X,"BTOT",8X,"CTOT")
  GO TO 190
! HUVUD4
170 WRITE(UT,175)
  WRITE(*,175)
  WRITE(UT,195)
  WRITE(*,195)
175 FORMAT(5X,"ATOT",8X,"LOGB",8X,"CTOT")
  GO TO 190
! HUVUD5
180 WRITE(UT,185)
  WRITE(*,185)
  WRITE(UT,195)
  WRITE(*,195)
185 FORMAT(5X,"LOGA",8X,"LOGB",8X,"CTOT")
! COMMON OUTPUT FOR HUVUD2 - HUVUD5
195 FORMAT(5X,"E",11X,"EBER",7X,"FEL(VAL)")
190 CONTINUE
  GO TO 30
! UTTAG
80 M=NAPA-16-NLAM
  LIK=CELL+M
  DO RLAM=1,NLAM,1
    LI=LIK+RLAM
    LNBA(RLAM)=AP(LI)
  END DO
  LJ=CELL+1
  LJK=CELL+NKOM
  WRITE(UT,85) (AP(LI),LI=LJ,LJK)
  WRITE(*,85) (AP(LI),LI=LJ,LJK)
85 FORMAT(1X,3F12.6)
  WRITE(UT,86)(LNBA(RLAM),EBERT(RLAM),FELUT(RLAM),RLAM=1,NLAM)
  WRITE(*,86)(LNBA(RLAM),EBERT(RLAM),FELUT(RLAM),RLAM=1,NLAM)
86 FORMAT(2X,9F11.3)
  GO TO 60
! UBER
10 U=0.D0
  RLAM=RS
  DO RSOLN=1,NSOLN,1
    CALL ESPEK
    U=U+FEL(VAL)**2
  END DO
! SLUT
70 IF(SKRIUT == 1 .or. SKRIUT == 0 .and. .not.TAGE .or. RURIK == 2 .or. RURIK == 1) GO TO 101
  IF(.not.PROV .or. TAGE .and. SKRIUT < 0) GO TO 102
101 CALL UVAR (UT)
  IF(PROV) WRITE(UT,85)
  IF(PROV) WRITE(*,85)
! GO TO UBBE UT
102 INDIC=1700
  RETURN
END SUBROUTINE UBBE ! for SPEFO

!-----------------------------------------------------------------------------
SUBROUTINE ESPEK
  USE LetaGropModule
  USE BDTV
  Implicit NONE
  Integer :: LI, LIK

  M=RSOLN*NAPA-16
  LIK=M-NAPA+18
  IF(TYP.NE.5) GO TO 2
  CTOT=AP(LIK+1)
  GO TO 3
2 BTOT=AP(LIK)
3 EBER=0.D0
  LIK=M-NLAM+RLAM
  E=AP(LIK)
  IF(.NOT.POSKIS.AND.E.GE.-9.9D5.OR.POSKIS.AND.E.GE.0.D0) GO TO 12
  EBER=E
  GO TO 13
12 LI=NX+NKOM
  DO IX=1,LI,1
    LIK=M+IX
    EBER=EBER+AP(LIK)*KS(RLAM,IX)
  END DO
  IF(.NOT.MOLEX.OR. TYP.NE.5.OR.CTOT.LE.0.D0) GO TO 22
  EBER=EBER/CTOT
  GO TO 23
22 IF(MOLEX .AND. BTOT.GT.0) EBER=EBER/BTOT
23 CONTINUE
13 FEL(1)=EBER-E
  IF(E.LE.ELIM) GO TO 42
  FEL(2)=FEL(1)/E
  RETURN
42 FEL(2)=FEL(1)/ELIM
  RETURN
END SUBROUTINE ESPEK

!-----------------------------------------------------------------------------
SUBROUTINE KOMIN
  USE LetaGropModule
  USE BDTV
  Implicit NONE
  Integer :: LI, LIK

  C(NX+1)=DEXP(LNA)
  IF(NKOM > 1) C(NX+2)=DEXP(LNB)
  IF(NKOM > 2) C(NX+3)=DEXP(LNC)
  CELL=RSOLN*NAPA-16
  LI=NX+NKOM
  DO IX=1,LI,1
    LIK=CELL+IX
    AP(LIK)=C(IX)
  END DO
  RETURN
END SUBROUTINE KOMIN

!-----------------------------------------------------------------------------
SUBROUTINE TABKOL
! TabKoll: find out which values of epsilon_pqr are already assumed
!   to be known, so that they are not adjusted
  USE LetaGropModule
  USE BDTV
  Implicit NONE
  Real(dp) :: DSG
  Integer :: LI, LIK

  NKS=NX+NKOM
  DO I=1,NKOM
    LIK=NX+I
    P(LIK)=0
    Q(LIK)=0
    R(LIK)=0
    BETA(LIK)=1.D0
  END DO
  P(NX+1)=1
  IF(NKOM.GT.1) Q(NX+2)=1
  IF(NKOM.GT.2) R(NX+3)=1
  LI=NX+NKOM
  DO 14 IX=1,LI
  BRA=.FALSE.
  AK(IX,6)=-1.D0
  IF(NTAB.LT.1) GO TO 14
  DO M=1,NTAB
    CELL=NAPA*(NSOLN+M-1)
    DSG=0.01D0
     IF(AP(CELL+1).LT.0.D0) DSG=-0.01D0
     IF(IDINT(AP(CELL+1)+DSG).EQ.P(IX)) BRA=.TRUE.
     IF(NKOM.GT.1.AND.IDINT(AP(CELL+2)+0.01D0).NE.Q(IX))BRA=.FALSE.
     IF(NKOM.GT.2.AND.IDINT(AP(CELL+3)+0.01D0).NE.R(IX))BRA=.FALSE.
     IF(BRA) THEN
       AK(IX,6)=1.D0
       DO RLAM=1,NLAM
         LIK=CELL+NKOM+RLAM
         KS(RLAM,IX)=AP(LIK)
       END DO
       BRA=.FALSE.
     END IF
  END DO
14 CONTINUE
  BRA=.TRUE.
  RETURN
END SUBROUTINE TABKOL

!-----------------------------------------------------------------------------
SUBROUTINE WERDA
! Check to prevent adjusting values of epsilon_pqr for species with
!  beta equal to zero
  USE LetaGropModule
  USE BDTV
  Implicit NONE
  Integer :: LI

  DO RLAM=1,NLAM
    NIKS(RLAM)=0
  END DO
  M=0
  LI=NX+NKOM
  DO IX=1,LI
    BRA=.FALSE.
    IF(LNBETA(IX).LE. -400.D0) BRA=.TRUE.
    IF(AK(IX,6).GT. 0.D0) BRA=.TRUE.
    IF(.not.BRA) THEN
       M=M+1
       DO RLAM=1,NLAM
         VAKS(RLAM,M)=IX
         NIKS(RLAM)=M
       END DO
       GO TO 23
    END IF ! not BRA
23  CONTINUE
  END DO
  NPUNKT=AG(6)+0.01D0
  RETURN
END SUBROUTINE WERDA

END MODULE PutsUbbe
