MODULE PutsUbbe
! DISTR for IBM-PC
! References:
! - Liem D.H., Ekelund R. (1979) New types of input in DISTR. Application
!   of LETAGROP for analysis of liquid-liquid distribution equilibria data.
!   Acta Chem. Scand. A33, 481.
! - Liem D.H. (1971) High-speed computers as a supplement to graphical
!   methods. 12. Application of LETAGROP to data for liquid-liquid
!   distribution equilibria. Acta Chem. Scand. 25, 1521.
USE READIR, ONLY : INFL
USE LetaGropModule, ONLY : UT
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
  1 FORMAT ("*** LETAGROP - for the IBM-PC ***",27x,"(I.Puigdomenech)",/,"*** DISTR6 (IBM XT VERSION) FEBRUARY-86 ***",/)
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
  Real(dp) :: D,I0,I1,LAMBDA=0.d0,LNA,LNV,TAU
  Integer :: LZ,LQ, LRP, LNPRS

  GO TO (120,120,340,340,560,560,780,780),TYP
! PUTS1
! PUTS2
  120 IF(RS /= 0) GO TO 3
      IF(TYP == 1) GO TO 1
      NAPA=12
      ARUM=9
      GO TO 2
    1 NAPA=10
      ARUM=8
    2 FAS2=.TRUE.
      RETURN
    3 CELL=APCELL(RS)
      W=AG(1)
      TAU=AG(2)
      IF(W > 0.D0) LAMBDA=W
      AS(RS,2)=DLOG(AS(RS,1))
      LNPRS=NP(RS)
      DO 8 LRP=1,LNPRS
      IF(W > 0.D0) GO TO 4
      M=CELL+NAP-2
      GO TO 5
    4 M=CELL+NAP-1
    5 I0=AP(M)
      I1=AP(M+1)
      IF(W < 0.D0) GO TO 6
      AP(M+2)=LAMBDA
      GO TO 7
    6 LAMBDA=AP(M+2)
    7 LQ=CELL+ARUM
      AP(LQ)=LN10*AP(CELL+1)
      LNA=AP(LQ)
      D=LAMBDA*(I1+TAU*I1*I1)/(I0+TAU*I0*I0)
      AP(LQ-1)=D
      CELL =CELL+NAPA
    8 CONTINUE
      RETURN
! PUTS3
! PUTS4
  340 IF(RS /= 0) GO TO 11
      IF(TYP == 3) GO TO 9
      NAPA=11
      ARUM=7
      GO TO 10
    9 NAPA=9
      ARUM=6
   10 FAS2=.TRUE.
      RETURN
   11 CELL=APCELL(RS)
      LNPRS=NP(RS)
      DO 12 LRP=1,LNPRS
      LQ=CELL+ARUM+TYP
      LZ=CELL+TYP+1
      AP(LQ)=DLOG(AP(LZ))
      LNV=AP(LQ)
      LQ=LQ-TYP
      AP(LQ)=LN10*AP(CELL+1)
      LNA=AP(LQ)
      CELL=CELL+NAPA
   12 CONTINUE
      RETURN
! PUTS5
! PUTS6
  560 IF(RS /= 0) GO TO 15
      IF(TYP == 5) GO TO 13
      NAPA=14
      ARUM=10
      GO TO 14
   13 NAPA=12
      ARUM=9
   14 FAS2=.TRUE.
      RETURN

   15 CELL=APCELL(RS)
      W=AG(1)
      TAU=AG(2)
      IF(W > 0.D0) LAMBDA=W
      LNPRS=NP(RS)
      DO LRP=1,LNPRS
      LQ=CELL+NAPA
      LZ=CELL+TYP-1
      AP(LQ)=DLOG(AP(LZ))
      LNV=AP(LQ)
      IF(W > 0.D0) GO TO 16
      M=CELL+NAP-2
      GO TO 17
   16 M=CELL+NAP-1
   17 I0=AP(M)
      I1=AP(M+1)
      IF(W < 0.D0) GO TO 18
      AP(M+2)=LAMBDA
      GO TO 19
   18 LAMBDA=AP(M+2)
   19 LQ=CELL+ARUM
      AP(LQ)=LN10*AP(CELL+1)
      LNA=AP(LQ)
      D=LAMBDA*(I1+TAU*I1*I1)/(I0+TAU*I0*I0)
      AP(LQ-1)=D
      CELL=CELL+NAPA
      ENDDO
      RETURN
! PUTS7
! PUTS8
  780 IF(RS /= 0) GO TO 23
      IF(TYP == 7) GO TO 21
      NAPA=11
      ARUM=7
      GO TO 22
   21 NAPA=9
      ARUM=6
   22 FAS2=.TRUE.
      RETURN
   23 CELL=APCELL(RS)
      LNPRS=NP(RS)
      DO 24 LRP=1,LNPRS
      LQ=CELL+ARUM+TYP-4
      LZ=CELL+TYP-3
      AP(LQ)=DLOG(AP(LZ))
      LNV=AP(LQ)
      LQ=CELL+ARUM
      AP(LQ)=LN10*AP(CELL+1)
      LNA=AP(LQ)
      CELL=CELL+NAPA
   24 CONTINUE
      RETURN
END SUBROUTINE PUTS

!-----------------------------------------------------------------------------
SUBROUTINE UBBE
  USE LetaGropModule
  USE BDTV
  USE L3, ONLY : SATSUT, UVAR
  Implicit NONE
  Real(dp) :: B0,B1,C0,C1,D,DEX=0.d0,IBER,IEXP,LAMBDA,VFAK=0.d0,FAKI,ULD,ULLNB,ULLNC,ULLNL,ULDEX
  Integer :: LQ, LIX
  U=0.D0
  IF(.not.TAGE) RSL = 1 ! RS = RS1 ! Tage=false means upper level adjustment of "k" ! modified Aug.2021
  RS = RSI(RSL)   ! added Aug.2021
  GO TO 340
! NYSA
  310 RP=0
      CELL=APCELL(RS)-NAPA
      IF(RURIK == 2) GO TO (410,420,430,440,450,460,470,480),TYP
      GO TO 500
! NYP
  320 RP=RP+1
      CELL=CELL+NAPA
      IF(RP <= NP(RS)) GO TO 4
      IF(RSL < RSN .AND. .NOT.TAGE) GO TO 3 ! IF(RS2 > RS .AND. .NOT.TAGE) GO TO 3  !  changed Aug.2021
      IF(SKRIUT == 1.OR.(SKRIUT == 0.AND..NOT.TAGE).or.(PROV.AND..NOT.(TAGE.AND.SKRIUT < 0))) GO TO 1
      GO TO 2
    1 CALL UVAR (UT)
      IF(PROV) WRITE (UT,45)
      IF(PROV) WRITE (*,45)
    2 CONTINUE
      INDIC=1700
      RETURN
    3 CONTINUE
      RSL = RSL+1 ! RS=RS+1 ! change Aug.2021
      RS = RSI(RSL)         ! added Aug.2021
      GO TO 310
    4 GO TO (810,820,830,840,850,860,870,880),TYP
! UBER
  330 U=U+FEL(VAL)*FEL(VAL)
      IF(RURIK /= 2) GO TO 320
      ULDEX=LOGE*DLOG(DEX)
      ULLNB=LOGE*LNB
      ULLNC=LOGE*LNC
      GO TO (910,920,930,940,950,960,970,980),TYP
! KAG
  340 DIRT=0
      I1=1
      I2=NK
      CALL BETAIN
      IF(.NOT.INDIK) GO TO 310
      INDIC=9
      RETURN
! SATSA1
  410 CALL SATSUT (UT)
      WRITE (UT,46)
      WRITE (*,46)
      GO TO 500
! SATSA2
  420 CALL SATSUT (UT)
      WRITE (UT,47)
      WRITE (*,47)
      GO TO 500
! SATSA3
  430 CALL SATSUT (UT)
      WRITE (UT,48)
      WRITE (*,48)
      GO TO 500
! SATSA4
  440 CALL SATSUT (UT)
      WRITE (UT,49)
      WRITE (*,49)
      GO TO 500
! SATSA5
  450 CALL SATSUT (UT)
      WRITE (UT,54)
      WRITE (*,54)
      GO TO 500
! SATSA6
  460 CALL SATSUT (UT)
      WRITE (UT,55)
      WRITE (*,55)
      GO TO 500
! SATSA7
  470 CALL SATSUT (UT)
      WRITE (UT,60)
      WRITE (*,60)
      GO TO 500
! SATSA8
  480 CALL SATSUT (UT)
      WRITE (UT,61)
      WRITE (*,61)
! ASOKS
  500 IF(TYP < 3) LNV=AS(RS,2)
      GO TO 320
! APFEL1
  810 BTOT=AP(CELL+2)
      CTOT=AP(CELL+3)
      LQ=CELL+ARUM-1
      DEX=AP(LQ)
      LNA=AP(LQ+1)
      BA=2
      CALL VALHAL
      GO TO 990
! APFEL2
  820 LTOT=AP(CELL+4)
      GO TO 810
! APFEL3
  830 LQ=CELL+NAPA
      LNV=AP(LQ)
      GO TO 810
! APFEL4
  840 LTOT=AP(CELL+4)
      GO TO 830
! APFEL5
  850 GO TO 830
! APFEL6
  860 GO TO 840
! APFEL7
  870 LQ=CELL+ARUM-2
      VFAK=AP(LQ)
      GO TO 830
! APFEL8
  880 LQ=CELL+ARUM-2
      VFAK=AP(LQ)
      GO TO 840
! UTTAG1
  910 IF(AG(1) > 0.D0) GO TO 5
      LAMBDA=AP(CELL+6)
      GO TO 6
    5 LAMBDA=AG(1)
    6 WRITE (UT,50) AP(CELL+1),AP(CELL+2),AP(CELL+3),AP(CELL+4),AP(CELL+5),LAMBDA,ULDEX,ULLNB,ULLNC,FEL(1),FEL(2)
      WRITE (*,50)  AP(CELL+1),AP(CELL+2),AP(CELL+3),AP(CELL+4),AP(CELL+5),LAMBDA,ULDEX,ULLNB,ULLNC,FEL(1),FEL(2)
      GO TO 320
! UTTAG2
  920 IF(AG(1) > 0.D0) GO TO 7
      LAMBDA=AP(CELL+7)
      GO TO 8
    7 LAMBDA=AG(1)
    8 ULLNL=LOGE*LNL
      WRITE (UT,51) AP(CELL+1),AP(CELL+2),AP(CELL+3),AP(CELL+4),AP(CELL+5),AP(CELL+6), &
            LAMBDA,ULDEX,ULLNB,ULLNC,ULLNL,FEL(1),FEL(2)
      WRITE (*,51)  AP(CELL+1),AP(CELL+2),AP(CELL+3),AP(CELL+4),AP(CELL+5),AP(CELL+6), &
            LAMBDA,ULDEX,ULLNB,ULLNC,ULLNL,FEL(1),FEL(2)
      GO TO 320
! UTTAG3
  930 WRITE (UT,52) AP(CELL+1),AP(CELL+2),AP(CELL+3),AP(CELL+4),AP(CELL+5),ULDEX,ULLNB,ULLNC,FEL(1),FEL(2),FEL(3)
      WRITE (*,52)  AP(CELL+1),AP(CELL+2),AP(CELL+3),AP(CELL+4),AP(CELL+5),ULDEX,ULLNB,ULLNC,FEL(1),FEL(2),FEL(3)
      GO TO 320
! UTTAG4
  940 ULLNL=LOGE*LNL
      WRITE (UT,53) AP(CELL+1),AP(CELL+2),AP(CELL+3),AP(CELL+4),AP(CELL+5),AP(CELL+6),ULDEX,ULLNB,ULLNC,ULLNL, &
                    FEL(1),FEL(2),FEL(3)
      WRITE (*,53)  AP(CELL+1),AP(CELL+2),AP(CELL+3),AP(CELL+4),AP(CELL+5),AP(CELL+6),ULDEX,ULLNB,ULLNC,ULLNL, &
                    FEL(1),FEL(2),FEL(3)
      GO TO 320
! UTTAG5
  950 IF(AG(1) > 0.D0) GO TO 12
      LAMBDA=AP(CELL+7)
      GO TO 13
   12 LAMBDA=AG(1)
   13 WRITE (UT,56) AP(CELL+1),AP(CELL+2),AP(CELL+3),AP(CELL+4),AP(CELL+5),AP(CELL+6),LAMBDA,ULDEX,ULLNB,ULLNC,FEL(1),FEL(2)
      WRITE (*,56)  AP(CELL+1),AP(CELL+2),AP(CELL+3),AP(CELL+4),AP(CELL+5),AP(CELL+6),LAMBDA,ULDEX,ULLNB,ULLNC,FEL(1),FEL(2)
      GO TO 320
! UTTAG6
  960 IF(AG(1) > 0.D0) GO TO 14
      LAMBDA=AP(CELL+8)
      GO TO 15
   14 LAMBDA=AG(1)
   15 ULLNL=LOGE*LNL
      WRITE (UT,57) AP(CELL+1),AP(CELL+2),AP(CELL+3),AP(CELL+4),AP(CELL+5),AP(CELL+6),AP(CELL+7), &
            LAMBDA,ULDEX,ULLNB,ULLNC,ULLNL, FEL(1),FEL(2)
      WRITE (*,57)  AP(CELL+1),AP(CELL+2),AP(CELL+3),AP(CELL+4),AP(CELL+5),AP(CELL+6),AP(CELL+7), &
            LAMBDA,ULDEX,ULLNB,ULLNC,ULLNL, FEL(1),FEL(2)
      GO TO 320
! UTTAG7
  970 ULD=LOGE*DLOG(D)
      WRITE (UT,58) AP(CELL+1),AP(CELL+2),AP(CELL+3),AP(CELL+4),AP(CELL+5),IBER,D,ULD,ULLNB,ULLNC,FEL(1),FEL(2),FEL(3)
      WRITE (*,58)  AP(CELL+1),AP(CELL+2),AP(CELL+3),AP(CELL+4),AP(CELL+5),IBER,D,ULD,ULLNB,ULLNC,FEL(1),FEL(2),FEL(3)
      GO TO 320
! UTTAG8
  980 ULD=LOGE*DLOG(D)
      ULLNL=LOGE*LNL
      WRITE (UT,59) AP(CELL+1),AP(CELL+2),AP(CELL+3),AP(CELL+4),AP(CELL+5),AP(CELL+6),IBER,ULD,ULLNB,ULLNC,ULLNL, &
                    FEL(1),FEL(2),FEL(3)
      WRITE (*,59)  AP(CELL+1),AP(CELL+2),AP(CELL+3),AP(CELL+4),AP(CELL+5),AP(CELL+6),IBER,ULD,ULLNB,ULLNC,ULLNL, &
                    FEL(1),FEL(2),FEL(3)
      GO TO 320
! FELBER
! - - - - - - - - - - - - - - - - - -
!   SUBROUTINE DBER
  990 B0=0.D0
      B1=0.D0
      DO 9 LIX=1,NX
      W=Q(LIX)*C(LIX)
      B0=B0+(1-FAS(LIX))*W
      B1=B1+FAS(LIX)*W
    9 CONTINUE
      IF(B0 > 0.D0) GO TO 10
      D=DEXP(100.D0)
      GO TO 11
   10 D=(B1/B0)/DEXP(LNV)
      IF(D <= 0.D0) D=1.0D-30
   11 IF(TYP < 7) GO TO 17
!     END
      C0=0.D0
      C1=0.D0
      DO 16 LIX=1,NX
      W1=P(LIX)*C(LIX)
      C0=C0+(1-FAS(LIX))*W1
      C1=C1+FAS(LIX)*W1
   16 CONTINUE
      IBER=C0+C1*VFAK
      IEXP=DEX
      FAKI=DABS(IBER/IEXP)
      IF(abs(FAKI) < 1.D-30) FAKI=1.0D-30
      FEL(1)=LOGE*DLOG(FAKI)
      FEL(2)=IEXP/IBER-1.D0
      FEL(3)=IEXP-IBER
      GO TO 330
   17 CONTINUE
      FEL(1)=LOGE*DLOG(D/DEX)
      FEL(2)=DEX/D-1.D0
      FEL(3)=D/DEX-1.D0
      GO TO 330
   45 FORMAT (1X)
   46 FORMAT (3X,"LOGH",6X,"BTOT",9X,"CTOT",9X,"IAQ",6X,"IORG",4X,"LAMBDA",2X,"LOGDEXP",3X,"LOGB",5X,"LOGC",2X, &
                "LG(DB/DX)",1X,"DX/DB-1")
   47 FORMAT (3X,"LOGH",6X,"BTOT",9X,"CTOT",9X,"LTOT",9X,"IAQ",6X,"IORG",4X,"LAMBDA",2X,"LOGDEXP",3X,"LOGB",5X, &
                "LOGC",5X,"LOGL",2X,"LG(DB/DX)",1X,"DX/DB-1")
   48 FORMAT (3X,"LOGH",6X,"BTOT",9X,"CTOT",9X,"V1/V0",4X,"DEXP",3X,"LOGDEXP",4X,"LOGB",5X,"LOGC",2X,"LG(DB/DX)",1X, &
                "DX/DB-1",2X,"DB/DX-1")
   49 FORMAT (3X,"LOGH",6X,"BTOT",9X,"CTOT",9X,"LTOT",9X,"V1/V0",4X,"DEXP",3X,"LOGDEXP",4X,"LOGB",5X,"LOGC",5X, &
                "LOGL",2X,"LG(DB/DX)",1X,"DX/DB-1",2X,"DB/DX-1")
   50 FORMAT (1X,F8.4,1P,2E13.4,0P,2F10.1,F7.4,4F9.4,F8.4)
   51 FORMAT (1X,F8.4,1P,3E13.4,0P,2F10.1,F7.4,5F9.4,F8.4)
   52 FORMAT (1X,F8.4,1P,2E13.4,0P,F10.4,7F9.4)
   53 FORMAT (1X,F8.4,1P,3E13.4,0P,F10.4,8F9.4)
   54 FORMAT (3X,"LOGH",6X,"BTOT",9X,"CTOT",9X,"V1/V0",5X,"IAQ",6X,"IORG",4X,"LAMBDA",2X,"LOGDEXP",3X,"LOGB",5X, &
                "LOGC",2X,"LG(DB/DX)",1X,"DX/DB-1")
   55 FORMAT (3X,"LOGH",5X,"BTOT",7X,"CTOT",7X,"LTOT",8X,"V1/V0",5X,"IAQ",6X,"IORG",4X,"LAMBDA",2X,"LOGDEXP",3X, &
                "LOGB",5X,"LOGC",5X,"LOGL",2X,"LG(DB/DX)",1X,"DX/DB-1")
   56 FORMAT (1X,F8.4,1P,2E13.4,0P,F10.4,2F10.1,F7.4,4F9.4,F8.4)
   57 FORMAT (1X,F8.4,1P,3E11.4,0P,F10.4,2F10.1,F7.4,5F9.4,F8.4)
   59 FORMAT (1X,F8.4,1P,3E11.4,0P,F10.4,9F9.4)
   58 FORMAT (1X,F8.4,1P,2E13.4,0P,F10.4,9F9.4)
   60 FORMAT (3X,"LOGH",6X,"BTOT",9X,"CTOT",9X,"V1/V0",4X,"IEXP",5X,"IBER",5X,"DBER",3X,"LOGDBER",4X,"LOGB",5X, &
                "LOGC",2X,"LG(IB/IX)",1X,"IX/IB-1",3X,"IX-IB")
   61 FORMAT (3X,"LOGH",5X,"BTOT",7X,"CTOT",7X,"LTOT",8X,"V1/V0",4X,"IEXP",5X,"IBER",3X,"LOGDBER",4X,"LOGB",5X, &
                "LOGC",5X,"LOGL",2X,"LG(IB/IX)",1X,"IX/IB-1",3X,"IX-IB")
END SUBROUTINE UBBE

END MODULE PutsUbbe