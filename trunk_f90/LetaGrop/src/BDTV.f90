MODULE BDTV ! Betain, Dirty, TotBer and Valhal
! From:
! Arnek R., Sillén L.G., Wahlberg O. (1969) High-speed computers as a
! supplement to graphical methods. 8. Some devices to speed up computations
! on chemical equilibria and simplify proramming for LETAGROP. Application
! to complex formation. Arkiv Kemi 31, 353.

USE LetaGropModule
USE DIMS_Module, ONLY : MXK
!Integer, PARAMETER :: MXK = 40
Implicit NONE

  Real(dp) :: ATOT,B2HX,BTOT,BZ,CHX,CHXALL,CTOT,DIL,E,E0,EBER,EFAK,EJ,HFRI,JAC,KHX,KWJALK, &
        LNA,LNB,LNC,LNH,LNL,LNV,LTOT,TEMP,JAC2,JB,JC,BETA(MXK),C(MXK),LNBETA(MXK),FEL(8)
  Integer  :: BA,DIRT,IX,I1,I2,FAS(MXK),P(MXK),Q(MXK),R(MXK),T(MXK),TAL(MXK)
  Logical  :: YKSI,INDIK
  Real(dp) :: ABUN,BFAK,DA,DA0,DAT,DZ,EB,E0B,EBFAK,ETA,ZAB,ZAC
  Integer  :: IBUR,NBUR,NE,TRUM

SAVE

CONTAINS

!-----------------------------------------------------------------------------
SUBROUTINE BETAIN
  Implicit NONE
  Integer :: LIK, I, ierr
      INDIK=.FALSE.
      IX=0
      IF(.NOT.FAS2) GO TO 2
      NKOM=NAK-2
      GO TO 3
    2 NKOM=NAK-1
    3 YKSI=.TRUE.
      IF(I1.GT.I2) GO TO 10
      DO 9 LIK=I1,I2
      IX=IX+1
      POT(IX)=AK(LIK,1)
      IF(K(LIK).LE.0.D0) GO TO 5
      LNBETA(IX)=DLOG(K(LIK))+LN10*POT(IX)
      GO TO 6
    5 LNBETA(IX)=-1000.D0
    6 P(IX)=AK(LIK,2)
      IF(NKOM.LE.1) GO TO 8
      Q(IX)=AK(LIK,3)
      IF(Q(IX).NE.0.AND.Q(IX).NE.1) YKSI=.FALSE.
    8 IF(NKOM.GT.2) R(IX)=AK(LIK,4)
      IF(NKOM.GT.3) T(IX)=AK(LIK,5)
      IF(FAS2) FAS(IX)=AK(LIK,NAK)
      IF(IX.GT.1) GO TO 9
      BETA(IX) = 0.d0 ! change in 2021. Might not be necessary
      if(LNBETA(IX) > 600.d0) then
        BETA(IX) = huge(1.d0)
        WRITE (UT,'("---- Note: LNBETA(",I0,")=",1PE11.3," (too large.)")') IX,LNBETA(IX)
        WRITE (*,'("---- Note: LNBETA(",I0,")=",1PE11.3," (too large.)")') IX,LNBETA(IX)
        WRITE (*, '("Press Enter to continue ...")', Advance='No') ! pause
        READ (*,'(I5)',iostat=ierr) I
      else if(LNBETA(IX) > -600.d0) then
        BETA(IX)=DEXP(LNBETA(IX)) ! original line
      endif ! change in 2021 ends
    9 CONTINUE
      NX=IX
      RETURN

   10 WRITE (UT,11)
      WRITE (*,11)
   11 FORMAT (" NX=0  PLEASE ADD COMPLEX")
      INDIK=.TRUE.
      RETURN

END SUBROUTINE BETAIN

!-----------------------------------------------------------------------------
SUBROUTINE DIRTY
  Implicit NONE
      IF(DIRT.LE.0) GO TO 2
      HFRI = 0.d0 ! change in 2021
      if(LNA > 231.d0) then
        HFRI = 2.D+100
      else if(LNA > -600.d0) then
        HFRI=DEXP(LNA) ! original line
      endif ! change in 2021 ends
      W=CHX*DIL+CHXALL
      IF(DIRT.EQ.1) Y=Y-W*KHX/(KHX+HFRI)
      IF(DIRT.EQ.2) Y=Y-W*(2*B2HX+KHX*HFRI)/(HFRI*(HFRI+KHX)+B2HX)
    2 CONTINUE
      RETURN
END SUBROUTINE DIRTY

!-----------------------------------------------------------------------------
SUBROUTINE TOTBER
  Implicit NONE
  Integer :: LIX
      IF(.NOT.FAS2) GO TO 2
      Y=0.D0
      GO TO 3
    2 CONTINUE
      Y = 0.d0 ! change in 2021
      if(X > 231.d0) then
        Y = 2.D+100
      else if(X > -600.d0) then
        Y=DEXP(X) ! original line
      endif ! change in 2021 ends
    3 DO 4 LIX=1,NX
      Y=Y+C(LIX)*TAL(LIX)
    4 CONTINUE
      RETURN
END SUBROUTINE TOTBER

!-----------------------------------------------------------------------------
SUBROUTINE VALHAL
  USE LetaGropModule_4
  Implicit NONE
  Integer :: LIX
      IF(BA.NE.1.AND.BA.NE.3) GO TO 2
      AVAR=.TRUE.
      GO TO 3
    2 AVAR=.FALSE.
    3 IF(BA.LE.1) GO TO 5
      BVAR=.TRUE.
      GO TO 6
    5 BVAR=.FALSE.
    6 NVAR=NKOM
      IF(.NOT.AVAR) NVAR=NVAR-1
      IF(NKOM.GT.1.AND..NOT.BVAR) NVAR=NVAR-1
      IF(.NOT.BVAR.OR.BTOT.GT.0) GO TO 8
      LNB=-1000.D0
      BVAR=.FALSE.
      NVAR=NVAR-1
    8 IF(ORVAR.EQ.0.AND.(.NOT.TAGE.OR.(TAGE.AND.ORVAKO.EQ.0))) GO TO 9
      GO TO 10
    9 NYTT=.TRUE.
      GO TO 11
   10 NYTT=.FALSE.
   11 TOLA=TOL(NKOM)
      IF(BTOT.LE.0.D0) GO TO 13
      TOLB=BTOT*TOL(1)
      GO TO 14
   13 TOLB=TOL(1)
   14 IF(NKOM.LE.2) GO TO 22
      IF(CTOT.LE.0.D0) GO TO 17
      TOLC=CTOT*TOL(2)
      GO TO 18
   17 TOLC=TOL(2)
   18 IF(NKOM.LE.3) GO TO 22
      IF(LTOT.LE.0.D0) GO TO 21
      TOLL=LTOT*TOL(3)
      GO TO 22
   21 TOLL=TOL(3)
   22 CONTINUE
      IF(.NOT.BVAR) GO TO 30
      TOLY=TOLB
      IF(NVAR.NE.2.OR..NOT.AVAR) GO TO 25
      TOLYI(1)=TOLA
      GO TO 26
   25 TOLYI(1)=TOLC
   26 IF(NVAR.LE.2) GO TO 30
      IF(.NOT.AVAR) GO TO 29
      TOLYI(2)=TOLA
      GO TO 30
   29 TOLYI(2)=TOLL
   30 CONTINUE
      IF(BVAR) GO TO 39
      IF(NVAR.NE.1.OR..NOT.AVAR) GO TO 33
      TOLY=TOLA
      GO TO 34
   33 TOLY=TOLC
   34 IF(NVAR.LE.1) GO TO 39
      IF(.NOT.AVAR.OR.NVAR.NE.2) GO TO 37
      TOLYI(1)=TOLA
      GO TO 38
   37 TOLYI(1)=TOLL
   38 IF(NVAR.EQ.3) TOLYI(2)=TOLA
   39 CONTINUE
      VARV=0
      M=CELL+ARUM
      STEP0=0.4D0*STEGBY
      IF(.NOT.NYTT) GO TO 41
      IF(BVAR) LNB=START(1)
      IF(NKOM.GT.2) LNC=START(2)
      IF(NKOM.GT.3) LNL=START(3)
      IF(AVAR) LNA=START(NKOM)
      IF(RP.EQ.1) STEP0=1.D0
      GO TO 42
   41 IF(AVAR) LNA=AP(M)
      IF(BVAR) LNB=AP(M+1)
      IF(NKOM.GT.2) LNC=AP(M+2)
      IF(NKOM.GT.3) LNL=AP(M+3)
   42 STEP=STEP0
      IF(NVAR.LE.1) GO TO 45
      KAL(1)=1
      STEGI(1)=STEP0
      IF(NVAR.LE.2) GO TO 45
      KAL(2)=1
      STEGI(2)=STEP0
   45 CONTINUE
      DO 56 LIX=1,NX
      LNBA1(LIX)=LNBETA(LIX)
   56 CONTINUE
      IF(.NOT.FAS2) GO TO 61
      DO 59 LIX=1,NX
      LNBA1(LIX)=LNBA1(LIX)+FAS(LIX)*LNV
   59 CONTINUE
   61 IF(AVAR) GO TO 65
      DO 63 LIX=1,NX
      LNBA1(LIX)=LNBA1(LIX)+P(LIX)*LNA
   63 CONTINUE
   65 IF(NKOM.LE.1.OR.BVAR) GO TO 1700
      DO 67 LIX=1,NX
      LNBA1(LIX)=LNBA1(LIX)+Q(LIX)*LNB
   67 CONTINUE
! LNBAS
 1700 DO 460 LIX=1,NX
      LNBA(LIX)=LNBA1(LIX)
  460 CONTINUE
      IF(NVAR.EQ.0) GO TO 2500
      IF(AVAR.AND.NVAR.EQ.1) GO TO 1800
      IF(.NOT.AVAR) GO TO 72
      DO 70 LIX=1,NX
      LNBA(LIX)=LNBA(LIX)+P(LIX)*LNA
   70 CONTINUE
   72 IF(NKOM.LE.2.OR..NOT.BVAR) GO TO 76
      DO 74 LIX=1,NX
      LNBA(LIX)=LNBA(LIX)+R(LIX)*LNC
   74 CONTINUE
   76 IF(NKOM.LE.3) GO TO 80
      DO 78 LIX=1,NX
      LNBA(LIX)=LNBA(LIX)+T(LIX)*LNL
   78 CONTINUE
   80 IF(BVAR) GO TO 2000
      GO TO 2200
! ARUNT
 1800 KARL=1
      X=LNA
! TJATA
 1900 DO 81 LIX=1,NX
      W=LNBA(LIX)+P(LIX)*X
      !IF(W > 2.D0) W=2.D0
      C(LIX) = 0.d0 ! added in 2021
      if(W > 231.d0) then
        C(LIX) = 2.D+100
      else if(W > -600.d0) then
        C(LIX)=DEXP(W) ! original line
      endif ! change in 2021 ends
   81 CONTINUE
      I=0
      CALL APROV
      IF(INDIK) GO TO 2500
      GO TO 1900
! BRUNT
 2000 IF(YKSI) CALL BYKSIS
      Y0=BTOT
      KARL=1
      X=LNB
! TJATB
 2100 LNB=X
      Y=0.D0
      IF(.NOT.FAS2) THEN
        Y = 0.d0 ! added in 2021
        if(X > 231.d0) then
            Y = 2.D+100
        else if(X > -600.d0) then
            Y=DEXP(X) ! original line
        endif ! change in 2021 ends
      ENDIF
      DO 82 LIX=1,NX
      W=LNBA(LIX)+Q(LIX)*X
      !IF(W.GT.2.D0) W=2.D0
      C(LIX) = 0.d0 ! added in 2021
      if(W > 231.d0) then
        C(LIX) = 2.D+100
      else if(W > -600.d0) then
        C(LIX)=DEXP(W) ! original line
      endif ! change in 2021 ends
      Y=Y+C(LIX)*Q(LIX)
   82 CONTINUE
      CALL KALLE
      IF(INDIK) GO TO 2500
      IF(.NOT.BRA) GO TO 2100
      IF(NVAR.EQ.1) GO TO 2500
      GO TO 2400
! CRUNT
 2200 KARL=1
      X=LNC
! TJATC
 2300 DO 83 LIX=1,NX
      W=LNBA(LIX)+R(LIX)*X
      !IF(W.GT.2.D0) W=2.D0
      C(LIX) = 0.d0 ! added in 2021
      if(W > 231.) then
        C(LIX) = 2.D+100
      else if(W > -600.d0) then
        C(LIX)=DEXP(W) ! original line
      endif ! change in 2021 ends
   83 CONTINUE
      I=0
      CALL CPROV
      IF(INDIK) GO TO 2500
      IF(.NOT.BRA) GO TO 2300
      GO TO 2400
! KLIVUT
 2400 IF(NVAR.NE.2.OR..NOT.AVAR) GO TO 47
      I=1
      CALL APROV
      IF(INDIK) GO TO 2500
      GO TO 1700
   47 IF(.NOT.BVAR) GO TO 49
      I=1
      CALL CPROV
      IF(INDIK) GO TO 2500
      GO TO 50
   49 I=1
      CALL LPROV
      IF(INDIK) GO TO 2500
   50 IF(.NOT.BRA) GO TO 1700
      KAL(1)=1
      IF(.NOT.AVAR) GO TO 52
      I=2
      CALL APROV
      IF(INDIK) GO TO 2500
      GO TO 53
   52 I=2
      CALL LPROV
      IF(INDIK) GO TO 2500
   53 GO TO 1700
! NOG
 2500 IF(.NOT.NYTT) GO TO 55
      IF(BTOT.GT.0) START(1)=LNB
      IF(NKOM.GT.2.AND.CTOT.GT.0) START(2)=LNC
      IF(NKOM.GT.3.AND.LTOT.GT.0) START(3)=LNL
      START(NKOM)=LNA
   55 AP(M)=LNA
      IF(NKOM.GT.1) AP(M+1)=LNB
      IF(NKOM.GT.2) AP(M+2)=LNC
      IF(NKOM.GT.3) AP(M+3)=LNL
      RETURN

END SUBROUTINE VALHAL

!-----------------------------------------------------------------------------
SUBROUTINE APROV
  USE LetaGropModule_4
  Implicit NONE
  Integer :: NKOM2, LIX, IL
      INDIK=.FALSE.
      X=LNA
      Y0=ATOT
      Y=0.D0
      IF(.NOT.FAS2) THEN
        Y = 0.d0 ! added in 2021
        if(X > 231.d0) then
            Y = 2.D+100
        else if(X > -600.d0) then
            Y=DEXP(X) ! original line
        endif ! change in 2021 ends
      ENDIF
      IL=IDINT(AG(8)+1.D-1)
      NKOM2=2*NKOM
      IF(IL.EQ.1) THEN
        Y = 0.d0 ! added in 2021
        if(X > 231.d0) then
            Y = 2.D+100
        else if(X > -600.d0) then
            Y=DEXP(X)
        endif
        Y=Y*(AS(RS,NKOM2+3)+AP(CELL+NBUR))/AS(RS,NKOM2+1)
      ! Y=DEXP(X)*(AS(RS,NKOM2+3)+AP(CELL+NBUR))/AS(RS,NKOM2+1) ! original line
      ENDIF  ! change in 2021 ends
      DO 7 LIX=1,NX
      Y=Y+C(LIX)*P(LIX)
    7 CONTINUE
      CALL DIRTY
      IF(I.NE.0) GO TO 2
      CALL KALLE
      IF(INDIK) GO TO 6
      GO TO 3
    2 II=I
      CALL KILLE
    3 IF(BRA) GO TO 5
      LNA=X
      RETURN
    5 CONTINUE
      INDIK=.TRUE.
    6 CONTINUE
      RETURN
END SUBROUTINE APROV

!-----------------------------------------------------------------------------
SUBROUTINE BYKSIS
  USE LetaGropModule_4
  Implicit NONE
  Integer :: LIX
  Real(dp) :: WW
      IF(.NOT.FAS2) GO TO 2
      W=0.D0
      GO TO 3
    2 W=1.D0
    3 DO 4 LIX=1,NX
      WW = 0.d0 ! added in 2021
      if(LNBA(LIX) > 231.d0) then
        WW = 2.D+100
      else if(LNBA(LIX) > -600.d0) then
        WW=DEXP(LNBA(LIX))
      endif
      W=W+Q(LIX)*WW  ! change in 2021 ends
      !W=W+Q(LIX)*DEXP(LNBA(LIX))  ! original line
    4 CONTINUE
      LNB=DLOG(BTOT/W)
      X=LNB
      RETURN
END SUBROUTINE BYKSIS

!-----------------------------------------------------------------------------
SUBROUTINE CPROV
  USE LetaGropModule_4
  Implicit NONE
  Integer :: LIX
      INDIK=.FALSE.
      X=LNC
      Y0=CTOT
      Y=0.D0
      IF(.NOT.FAS2) THEN
        Y = 0.d0 ! added in 2021
        if(X > 231.d0) then
            Y = 2.D+100
        else if(X > -600.d0) then
            Y=DEXP(X) ! original line
        endif ! change in 2021 ends
      ENDIF
      DO 6 LIX=1,NX
      Y=Y+C(LIX)*R(LIX)
    6 CONTINUE
      IF(I.NE.0) GO TO 2
      CALL KALLE
      IF(INDIK) GO TO 5
      GO TO 3
    2 II=I
      CALL KILLE
    3 IF(.NOT.BRA) LNC=X
      IF(.NOT.BRA.OR.NVAR.NE.I+1) GO TO 5
      INDIK=.TRUE.
      RETURN
    5 CONTINUE
      RETURN
END SUBROUTINE CPROV

!-----------------------------------------------------------------------------
SUBROUTINE KALLE
  USE LetaGropModule_4
  Implicit NONE
      INDIK=.FALSE.
      W=DABS(Y-Y0)
      IF(TOLY.LE.W) GO TO 2
      BRA=.TRUE.
      GO TO 9999
    2 BRA=.FALSE.
      IF(Y.LE.Y0) GO TO 4
      X2=X
      Y2=Y
      GO TO 5
    4 X1=X
      Y1=Y
    5 VARV=VARV+1
      IF(VARV.LE.VMAX-1) GO TO 8
      IF(VARV.EQ.VMAX) WRITE (UT,14) VARV,RS,RP,Y0
      IF(VARV.EQ.VMAX) WRITE (*,14) VARV,RS,RP,Y0
      WRITE (UT,15) X,Y
      WRITE (*,15) X,Y
      IF(VARV.LE.VMAX+30) GO TO 8
      BRA=.TRUE.
      INDIK=.TRUE.
      RETURN
    8 CONTINUE
      GO TO (1301,1302,1303,1304),KARL
! ADA1
 1301 FAK=2.D0
      IF(Y.LE.Y0) GO TO 10
      KARL=3
      GO TO 11
   10 KARL=2
   11 STEP=0.5D0*STEP
      GO TO 1360
! ADA2
 1302 IF(Y.GT.Y0) GO TO 1350
      GO TO 1360
! ADA3
 1303 IF(Y.LT.Y0) GO TO 1350
      GO TO 1360
! GREPP
 1350 FAK=0.5D0
      KARL=4
! ADA4
 1304 IF(STEP.LT.STEGBY) GO TO 1370
! KLIV
 1360 STEP=FAK*STEP
      IF(Y.LE.Y0) GO TO 13
      X=X-STEP
      GO TO 9999
   13 X=X+STEP
      GO TO 9999
! KORDA
 1370 W=(X2-X1)/(Y2-Y1)
      X=X1+W*(Y0-Y1)
      GO TO 9999
! SLUT
 9999 CONTINUE
      RETURN
   14 FORMAT (1X,"VARV",I4,1X,"SATS",I3,1X,"PUNKT",I3,1X,"Y0=",E15.7)
   15 FORMAT (1X,"X=",F13.7,"Y=",E15.7)
END SUBROUTINE KALLE

!-----------------------------------------------------------------------------
SUBROUTINE KILLE
  USE LetaGropModule_4
  Implicit NONE
  Integer :: ADAX, LII
      LII=II
      W=DABS(Y-Y0)
      IF(TOLYI(LII).LE.W) GO TO 2
      BRA=.TRUE.
      GO TO 9999
    2 BRA=.FALSE.
      IF(Y.LE.Y0) GO TO 4
      X2I(LII)=X
      Y2I(LII)=Y
      GO TO 5
    4 X1I(LII)=X
      Y1I(LII)=Y
    5 ADAX=KAL(LII)
      GO TO (1301,1302,1303,1304),ADAX
! ADA1
 1301 FAKI(LII)=2
      IF(Y.LE.Y0) GO TO 7
      KAL(LII)=3
      GO TO 8
 1302 IF(Y.GT.Y0) GO TO 1350
      GO TO 1360
    7 KAL(LII)=2
    8 STEGI(LII)=0.5D0*STEGI(LII)
      GO TO 1360
! ADA2
! ADA3
 1303 IF(Y.LT.Y0) GO TO 1350
      GO TO 1360
! GREPP
 1350 FAKI(LII)=0.5D0
      KAL(LII)=4
! ADA4
 1304 IF(STEGI(LII).LT.STEGBY) GO TO 1370
! KLIV
 1360 STEGI(LII)=FAKI(LII)*STEGI(LII)
      IF(Y.LE.Y0) GO TO 10
      X=X-STEGI(LII)
      GO TO 9999
   10 X=X+STEGI(LII)
      GO TO 9999
! KORDA
 1370 W=(X2I(LII)-X1I(LII))/(Y2I(LII)-Y1I(LII))
      X=X1I(LII)+W*(Y0-Y1I(LII))
      GO TO 9999
! SLUT
 9999 CONTINUE
      RETURN
END SUBROUTINE KILLE

!-----------------------------------------------------------------------------
SUBROUTINE LPROV
  USE LetaGropModule_4
  Implicit NONE
  Integer :: LIX
      INDIK=.FALSE.
      X=LNL
      Y0=LTOT
      Y=0.D0
      IF(.NOT.FAS2) THEN
        Y = 0.d0 ! added in 2021
        if(X > 231.d0) then
            Y = 2.D+100
        else if(X > -600.d0) then
            Y=DEXP(X) ! original line
        endif ! change in 2021 ends
      ENDIF
      DO 3 LIX=1,NX
      Y=Y+C(LIX)*T(LIX)
    3 CONTINUE
      II=I
      CALL KILLE
      IF(.NOT.BRA) LNL=X
      IF(.NOT.BRA.OR.NVAR.NE.I+1) GO TO 2
      INDIK=.TRUE.
      RETURN
    2 CONTINUE
      RETURN
END SUBROUTINE LPROV

END MODULE BDTV