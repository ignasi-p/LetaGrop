MODULE L2
USE LetaGropModule
USE L3, ONLY : PLUSKA, MINUT, MININ
Implicit NONE

SAVE

CONTAINS

!-----------------------------------------------------------------------------
SUBROUTINE DARRA(II)
  Implicit NONE
  Integer, INTENT(IN) :: II
  Integer :: LII
  if(lgDbg) write(*,'("DARRA(",I0,") in")') II
  LII=II
  IF(.NOT.TAGE) GO TO 2
  DARKS(RS,IK)=DARR1
  DARKS2(RS,IK)=DARR2
  IF(SKRIUT > -1) WRITE (UT,4) RS,IK,KBOM(LII),DARR1,DARR2
  IF(SKRIUT > -1) WRITE (*,4) RS,IK,KBOM(LII),DARR1,DARR2
  GO TO 3
  2 DARK(IK)=DARR1
  DARK2(IK)=DARR2
  WRITE (UT,5) IK,KBOM(LII),DARR1,DARR2,(AK(IK,J),J=1,NAK)
  WRITE (*,5) IK,KBOM(LII),DARR1,DARR2,(AK(IK,J),J=1,NAK)
  3 CONTINUE
  RETURN

  4 FORMAT (2X,"SET ",I2,2X,"KS",I2,"=",1PE13.5,2X,"SIGMA=",2E11.3)
  5 FORMAT ("K",I2,"=",1PE13.5,3X,"SIGMA1=",E11.3,3X,"SIGMA2=",E11.3,:3X,"AK=",0P10F6.2)
END SUBROUTINE DARRA

!-----------------------------------------------------------------------------
SUBROUTINE ENSAM
  USE L3, ONLY : PROVAR
  Implicit NONE
  if(lgDbg) write(*,'("ENSAM in, ORVAR=",I0)') ORVAR
  IF((ORVAR == 1.OR.UMIN > U).AND..NOT.PROV) CALL MINUT
  IK=IVAR(1)
  IF(ORVAR == -1) ORVAR=1
  GO TO (3001,3002,3003,3004,3005,3006,3007,3008,3009,3010),ORVAR
! ENS1
3001 UC=U
  KV(1)=KC(1)+STEK(1)
  ORVAR=2
  GO TO 3400
! ENS2
3002 U1=U
  KV(1)=KC(1)-STEK(1)
  ORVAR=3
  GO TO 3400
! ENS3
3003 U2=U
  GO TO 3200
! ENS4
3004 CONTINUE
  PROV=.FALSE.
  IF(U <= UMIN+UMIN*TOLU .OR. (TAGE.AND.POSKIS)) GO TO 3300
  MINK=.FALSE.
  IF(UC > U2) GO TO 3011
  IF(UC > U1) GO TO 3012
  STEK(1)=0.5D0*STEK(1)
  KV(1)=KC(1)+STEK(1)
  ORVAR=6
  GO TO 3400
! ENS5
3005 U1=U
  GO TO 3200
! ENS6
3006 IF(U >= UC) THEN
        U1=U
        KV(1)=KC(1)-STEK(1)
        ORVAR=7
        GO TO 3400
    ENDIF
    U2=UC
    UC=U
    KC(1)=KV(1)
    GO TO 3200
! ENS7
3007 IF(U >= UC) THEN
        U2=U
        !## IF(KBOM(1) == KC(1)) THEN
        IF((abs(KBOM(1)) < 1.d-300 .and. abs(KC(1)) < 1.d-300) .or.  &
           (abs(KC(1)) >= 1.d-300 .and.  &
                  abs((KBOM(1)-KC(1))/KC(1)) <= 1.d-12)  &
          ) THEN
            UMIN = U
            ! CALL MINUT
            CALL MININ
            GO TO 3300
        ENDIF
    ELSE
        U1=UC
        UC=U
        KC(1)=KV(1)
    ENDIF
    IF((U1+U2-2.D0*UC) >= 21.0D0*TOLU*UC) GO TO 3200
    IF(DABS(U-UC) >=      10.5D0*TOLU*UC) GO TO 3200
    IF(DABS(U-UMIN) >=    10.5D0*TOLU*UMIN) GO TO 3200
    UMIN = U
    ! CALL MINUT
    CALL MININ
    GO TO 3300
! ENS8
3008 U2=U
  KV(1)=KC(1)
  ORVAR=9
  GO TO 3400
! ENS9
3009 UC=U
  GO TO 3200
! ENS10
3010 UC=U
  KV(1)=KC(1)+STEK(1)
  ORVAR=5
  GO TO 3400
! ENS11
3011 IF(POSK(IK).AND.KC(1) < 3.D0*STEK(1).AND..NOT.TAGE) GO TO 3013
  UC=U2
  KC(1)=KC(1)-STEK(1)
  STEK(1)=2.D0*STEK(1)
  KV(1)=KC(1)-STEK(1)
  ORVAR=3
  GO TO 3400
! ENS12
3012 UC=U1
  KC(1)=KC(1)+STEK(1)
  STEK(1)=2.D0*STEK(1)
  KV(1)=KC(1)+STEK(1)
  ORVAR=5
  GO TO 3400
! ENS13
3013 KC(1)=0.5D0*KC(1)
  STEK(1)=KC(1)
  U1=UC
  KV(1)=0.D0
  ORVAR=8
  GO TO 3400
! ENS14
3014 IF(U1 < (U2+W1)) GO TO 3500
  KC(1)=0.5D0*KC(1)
  KV(1)=KC(1)
  STEK(1)=KC(1)
  U1=UC
  GO TO 3400
! GROPEN
3200 CONTINUE
  X=0.5D0*(U1+U2)-UC
  W1=10.D0*UC*TOLU
  IF(X >= W1) GO TO 9
  IF(ORVAR == 9) GO TO 3014
  IF(U1 > (U2+W1)) GO TO 3011
  GO TO 3012
9 VBOM(1)=-0.25D0*(U1-U2)/X
  KBOM(1)=KC(1)+STEK(1)*VBOM(1)
  UNO=UC-X*VBOM(1)**2
  CALL SIGGE
  IF(SIG2Y <= 0.D0) GO TO 11
  DARR2=STEK(1)*DSQRT(SIG2Y/X)
  DARR1=DARR2
  GO TO 12
11 DARR2=-DABS(STEK(1))
  DARR1=DARR2
12 CALL DARRA (1)
  IF(TAGE .OR. .NOT.POSK(IK) .OR. KBOM(1) >= 0.D0) GO TO 19
  KV(1)=0.D0
  WRITE (UT,28)
  WRITE (*,28)
  28 FORMAT (" MIKO (Negative parameter)")
  IF(MINK) GO TO 15
  MINK=.TRUE.
  GO TO 16
15 MINK=.FALSE.
16 IF(ORVAR /= 9.AND.MINK) GO TO 20
  U=U2
  K(IK)=KV(1)
  GO TO 3004
19 KV(1)=KBOM(1)
  MINK=.FALSE.
20 IF(TAGE.AND.POSKIS.AND.KV(1) < 0.D0) KV(1)=0.D0
  IF(.NOT.(KOKS.AND.SKRIUT < 0)) CALL PROVAR
  ORVAR=4
  PROV=.TRUE.
  GO TO 3400
! TRAFFEN
3300 CALL MINUT
  IF(.NOT.MINK) GO TO 22
  U2=U
  STEK(1)=0.5D0*DABS(DARK(IK))
  KC(1)=STEK(1)
  KV(1)=KC(1)
  ORVAR=10
  GO TO 3400
22 ORVAR=1
  UC=U
  KC(1)=KV(1)
  IF(.NOT.KASSA.OR.TAGE) GO TO 24
  STYR=8
  INDIC=2100
  RETURN
24 INDIC=2000
  RETURN
! UT
3400  IF(.NOT.TAGE) GO TO 26
  KS(RS,IK)=KV(1)
  GO TO 27
26 K(IK)=KV(1)
27 INDIC=1317
  RETURN
! KASSO
3500 WRITE (UT,29)
  WRITE (*,29)
  29 FORMAT (" KONVEX TILL NOLL (Convex to zero)")
  UNO=UMIN
  CALL SIGGE
  DARR2=-2.D0*KC(1)*SIG2Y/(U1-U2)
  DARR1=DARR2
  CALL DARRA (1)
  MINK=.FALSE.
  ORVAR=1
  CALL MININ
  GO TO 24
END SUBROUTINE ENSAM

!-----------------------------------------------------------------------------
SUBROUTINE GROP
  USE DIMS_Module, ONLY : MXK
  !Integer, PARAMETER :: MXK = 40
  Implicit NONE
  Real(dp) :: DIA1(MXK),DIA2(MXK),RUT1(MXK,MXK),RUTINV(MXK,MXK),VRI(MXK,MXK)
  Integer :: RV, LQ, LI, LJ, LM
  if(lgDbg) write(*,'("GROP in")')
  DO LI=1,N
   DO LJ=1,N
    RUTINV(LI,LJ)=RUTA(LI,LJ)
   END DO
  END DO
  CALL INVERT (N,RUTINV,INDIC)
  IF(INDIC == 1) GO TO 4800
  CALL PINUS (PINNE,RUTINV,N,VBOM,1)
  UNO=UC
  DO LI=1,N
  UNO=UNO-PINNE(LI)*VBOM(LI)
  END DO
  CALL SIGGE
  IF(.NOT.(TAGE.AND.SKRIUT < 0)) WRITE (UT,24)
  IF(.NOT.(TAGE.AND.SKRIUT < 0)) WRITE (*,24)
  CALL PINUS (VBOM,SH,N,KBOM,-1)
  CALL MULLE (SH,RUTINV,N,N,N,RUT1,1)
  DO LI=1,N
  KBOM(LI)=KBOM(LI)+KC(LI)
  DIA1(LI)=RUTINV(LI,LI)
  DIA2(LI)=0.D0
  DO LM=1,N
   DIA2(LI)=DIA2(LI)+RUT1(LI,LM)*SH(LI,LM)
  END DO
  END DO
  DO LI=1,N
  IK=IVAR(LI)
  IF(TAGE.AND..NOT.POSKIS) KS(RS,IK)=KBOM(LI)
  IF(DIA1(LI)<=0.D0 .or. UNO<=0.D0) GO TO 5
  DARR1=DABS(DSQRT(SIG2Y*DIA1(LI))*SH(LI,LI))
  GO TO 6
5 DARR1=-DABS(STEK(LI))
6 IF(DIA2(LI)<=0.D0 .or. UNO<=0.D0) GO TO 8
  DARR2=DSQRT(SIG2Y*DIA2(LI))
  GO TO 9
8 DARR2=-DABS(STEK(LI))
9 CALL DARRA (LI)
  END DO
! VRID
  DO LI=1,N
   DO LJ=1,N
    VRI(LI,LJ)=0.D0
   END DO
   VRI(LI,LI)=1.D0
  END DO
  RV=N
! VAND
4500 RV=RV-1
  DO LI=1,RV
  DO LJ=1,RV
  RUTINV(LI,LJ)=RUTA(LI,LJ)
  END DO
  END DO
  CALL INVERT (RV,RUTINV,INDIC)
  IF(INDIC == 1) GO TO 4800
  DO LI=1,RV
  W=0.D0
  DO LM=1,RV
   W=W-RUTINV(LI,LM)*RUTA(LM,RV+1)
  END DO
  VRI(LI,RV+1)=W
  END DO
  IF(RV > 2) GO TO 4500
  VRI(1,2)=-RUTA(1,2)/RUTA(1,1)
  DO LI=1,N
   DO LJ=1,N
    RUT1(LI,LJ)=VRI(LI,LJ)/SH(LJ,LJ)
   END DO
  END DO
  CALL MULLE (SH,RUT1,N,N,N,S,1)
  M=0
  IF(.NOT.TAGE) GO TO 19
  IF(POSKIS) GO TO 28
  INDIC=1900
  RETURN
19 IF(SKRIUT > -1 .OR.(SKRIUT == -1 .AND. KOKS)) WRITE (UT,25)
  IF(SKRIUT > -1 .OR. (SKRIUT == -1 .AND. KOKS)) WRITE (*,25)
  IF(N < 1) GO TO 28
  DO LI=1,N
    LQ=LI+1
    IF(LQ > N) GO TO 231
    DO LJ=LQ,N
      IK=IVAR(LI)
      JK=IVAR(LJ)
      SK(IK,JK)=S(LI,LJ)
      IF(SKRIUT > -1 .OR. (SKRIUT == -1 .AND. KOKS)) GO TO 20
      GO TO 23
20     M=M+1
      IF(M /= 6) GO TO 22
      M=1
22     WRITE (UT,27) IK,JK,SK(IK,JK)
      WRITE (*,27) IK,JK,SK(IK,JK)
23     CONTINUE
    END DO
231   CONTINUE
  END DO
28 INDIC=2200
  RETURN
! SING(IN)
4800 INDIC=2300
  RETURN
24 FORMAT (1X,"KBOM (Parameters in the calculated U-minimum)") !##
25 FORMAT (1X,"SK - TWIST MATRIX (SIK)")
27 FORMAT (4X,1P,3(2I3,E11.2,2X),:/4X,3(2I3,E11.2,2X))
END SUBROUTINE GROP

!-----------------------------------------------------------------------------
SUBROUTINE KOMNER
  Implicit NONE
  Real(dp) :: MAX,SLASK,UMI
  Integer :: JMAX, LI, LJ, LM
  if(lgDbg) write(*,'("KOMNER in")')
  BRA=.TRUE.
  IF(NIDO /= 0) GO TO 2
  UMI=UMIN
  GO TO 3
2 UMI=UC
3 IF(SIG2Y <= 0.D0) GO TO 6
  X=(U-UMI)/SIG2Y
  IF(X <= 2.D0) GO TO 6
  W=0.7D0/DSQRT(X)
  BRA=.FALSE.
6 IF(SIG2Y >= 0.D0) GO TO 9
  X=U/UMI-1.D0
  IF(X <= 0.04D0) GO TO 9
  W=0.1D0/DSQRT(X)
  BRA=.FALSE.
9 IF(BRA) GO TO 99
  IF(SKRIUT > -2) Then
    WRITE (UT,17)
    WRITE (*,17)
    End If
  DO LM=1,N
    IK=IVAR(LM)
    IF(KMIN(LM) >= K(IK)) GO TO 15
    MAX=0.D0
    JMAX = -1
    DO LJ=1,N
        SLASK=DABS(S(LM,LJ)*STEK(LJ))
        If(SLASK > MAX) Then
            MAX=SLASK
            JMAX=LJ
        EndIf
    END DO
    IF(JMAX /= LM) GO TO 14
    STEK(LM)=W*STEK(LM)
    GO TO 15
    14 S(LM,JMAX)=W*S(LM,JMAX)
    15 CONTINUE
  END DO
  CALL MININ
  DO LI=1,N
   CALL PLUSKA (LI)
  END DO
! SLUT
99 CONTINUE
  RETURN
17 FORMAT (3X,"KOM NER  (Come down = calculated U-minimum too high)")
END SUBROUTINE KOMNER

!-----------------------------------------------------------------------------
SUBROUTINE LETA
    Implicit NONE
    Real(dp) :: DUM1,DUM2
    Integer :: LI, LJ
    if(lgDbg) write(*,'("LETA in")')
    DO LI=1,N
        V(LI)=0.D0
    END DO
    IF(ORVAR == 1) GO TO 5000
    IF(ORVAR /= -5) GO TO 3
    CALL KOMNER
    IF(.NOT.BRA) GO TO 6000
    CALL SHBER
    UC=U
    GO TO 5700
  3 CONTINUE
    IF(ORVAR == -4) GO TO 5200
    IF(ORVAR == -3) GO TO 5300
    IF(ORVAR /= -1) GO TO 7
    CALL KOMNER
    IF(BRA) GO TO 6
    ORVAR=-2
    GO TO 6000
  6 ORVAR=1
  7 CONTINUE
    IF(U > UMIN) GO TO (5100,5200,5300,5400),ORVAR
    IF(.NOT.TAGE.OR..NOT.POSKIS) GO TO 5000
    DO LI=1,N
        IK=IVAR(LI)
        IF(KS(RS,IK) < 0.D0) GO TO (5100,5200,5300,5400),ORVAR
    END DO
! LETA0
5000 CONTINUE
    CALL MINUT
    IF(SLUSK /= 0) GO TO 13
    IF(ORVAR == 1) NIDO=0
    IF(ORVAR /= 2 .AND. ORVAR /= 3) GO TO 11
    NIDO=1
    IDO(1)=RI
 11 IF(ORVAR /= 4) GO TO 13
    NIDO=2
    IDO(1)=RJ
    IDO(2)=RI
 13 IF(.NOT.TAGE) SIG2Y=U/NPUNKT
    GO TO (5100,5200,5300,5400),ORVAR
! LETA1
5100 CONTINUE
    UC=U
    DO LI=1,N
        DO LJ=1,N
            RUTA(LI,LJ)=0.D0
        END DO
    END DO
    CALL SHBER
    RI=0
    GO TO 5700
! LETA2
5200 CONTINUE
    U1=U
    V(RI)=-1.D0
    IF(ORVAR /= -4) GO TO 16
    ORVAR=-3
    GO TO 5800
 16 ORVAR=3
    GO TO 5800
! LETA3
5300 CONTINUE
    U2=U
    IF(.NOT.TAGE) GO TO 18
    M=NP(RS)
    GO TO 19
 18 M=NPUNKT
 19 IK=IVAR(RI)
    DUM1=DABS(U1-UC)
    DUM2=DABS(U2-UC)
    IF(TOLU >= 0.0001D0) GO TO 21
    W=10.D0*TOLU*UC
    GO TO 22
 21 W=0.001D0*UC
 22 CONTINUE
    W1=0.5D0*(U2+U1)-UC
    IF(W1*M  <= 2.D0*UC) GO TO 34
    CALL MINSKA
    GO TO 5700
 34 CONTINUE
    IF(DUM1 >= W .OR. DUM2 >= W) GO TO 36
    CALL STEGUP
    IF(ORVAR == -5) GO TO 6000
    IF(ORVAR /= -3) GO TO 5700
 36 IF(ORVAR /= -3) GO TO 24
    U=UC
    GO TO 5100
 24 IF(U2 >= U1) GO TO 26
    U2=U1
    U1=U
    STEK(RI)=-STEK(RI)
    CALL SHBER
 26 PINNE(RI)=0.25D0*(U2-U1)
    RUTA(RI,RI)=W1
    ORVAR=4
    GO TO 5600
! LETA4
5400 CONTINUE
    W=0.5D0*(U-UC)+PINNE(RI)+PINNE(RJ)-0.5D0*(RUTA(RI,RI)+RUTA(RJ,RJ))
    RUTA(RJ,RI)=W
    RUTA(RI,RJ)=W
! J UPP
5600 CONTINUE
    RJ=RJ+1
    IF(RJ >= RI) GO TO 5700
    V(RI)=1.D0
    V(RJ)=1.D0
    GO TO 5800
! UPP I
5700 CONTINUE
    RI=RI+1
    IF(RI > N) GO TO 5900
    RJ=0
    V(RI)=1.D0
    IF(ORVAR /= -5) GO TO 28
    ORVAR=-4
    GO TO 5800
 28 ORVAR=2
! KV BER
5800 CONTINUE
    CALL PINUS (V,SH,N,KV,-1)
    DO 31 LI=1,N
        IK=IVAR(LI)
        KV(LI)=KV(LI)+KC(LI)
        IF(.NOT.TAGE) GO TO 30
        KS(RS,IK)=KV(LI)
        GO TO 31
     30 K(IK)=KV(LI)
 31 CONTINUE
    if(lgDbg) write(*,'("LETA goto 6000")')
    GO TO 6000
! LETA SLUT
5900 CONTINUE
    RI=0
    RJ=0
    ORVAR=1
    GO TO 6100
! KALL UBBE
6000 CONTINUE
    INDIC=1317
    RETURN
! KALL GROP
6100 CONTINUE
    INDIC=4
    RETURN
END SUBROUTINE LETA

!-----------------------------------------------------------------------------
SUBROUTINE LOGKIK
  Implicit NONE
  Real(dp) :: expn
  if(lgDbg) write(*,'("LOGKIK in")')
  X=K(IK)
  Y=DARK2(IK)
  expn = 0.d0
  if(NAK > 0) expn = AK(IK,1)
  IF(Y > 0.D0) GO TO 2
  IF(X <= 0.D0) GO TO 5
  W= DLOG(X)*LOGE + expn
  WRITE (UT,10) IK,W
  WRITE (*,10) IK,W
  10 FORMAT (" LOG K",I2," =",F9.4,"  WITHOUT SIGMA")
  RETURN
2 IF(Y >= 0.2D0*X) GO TO 4
  W= DLOG(X)*LOGE + expn
  W1=1.5D0*(DLOG(X+Y)-DLOG(X-Y))*LOGE
  WRITE (UT,11) IK,W,W1
  WRITE (*,11) IK,W,W1
  11 FORMAT (" LOG K",I2," =",F9.4,"  +-  ",F7.4)
  RETURN
4 W= X + 3.d0*Y
  IF(W > 0.D0) GO TO 6
5 WRITE (UT,12) IK
  WRITE (*,12) IK
  12 FORMAT (" K",I2,"  ZERO OR NEGATIVE VALUE")
  RETURN
6 W1= DLOG(W)*LOGE + expn
  IF(X <= 0.D0) GO TO 8
  W= DLOG(X)*LOGE + expn
  WRITE (UT,13) IK,W,W1
  WRITE (*,13) IK,W,W1
  13 FORMAT (" LOG K",I2," =",F9.4,"  MAX=",F9.4)
  RETURN
8 WRITE (UT,14) IK,W1
  WRITE (*,14) IK,W1
  14 FORMAT (" K",I2,"  MAX=",F9.4)
  RETURN
END SUBROUTINE LOGKIK

!-----------------------------------------------------------------------------
SUBROUTINE MINSKA
  Implicit NONE
  Integer :: LI, LJ
  if(lgDbg) write(*,'("MINSKA in")')
  W=DSQRT(UC/(W1*M))
  STEK(RI)=W*STEK(RI)
  IF(SKRIUT > -1.OR.(SKRIUT == -1.AND.KOKS.AND..NOT.TAGE)) WRITE (UT,1) IK,STEK(RI)
  IF(SKRIUT > -1.OR.(SKRIUT == -1.AND.KOKS.AND..NOT.TAGE)) WRITE (*,1) IK,STEK(RI)
  DO LI=1,N
   DO LJ=1,N
    SH(LI,LJ)=S(LI,LJ)*STEK(LJ)
   END DO
  END DO
  RI=RI-1
  RETURN
1 FORMAT (3X,"MINSKA STEG (Decreasing Step)",I3,"  new STEK=",1PE10.3)
END SUBROUTINE MINSKA

!-----------------------------------------------------------------------------
SUBROUTINE SHBER
  Implicit NONE
  Integer :: LI, LJ
  if(lgDbg) write(*,'("SHBER in")')
  DO LI=1,N
   DO LJ=1,N
    SH(LI,LJ)=S(LI,LJ)*STEK(LJ)
   END DO
  END DO
  RETURN
END SUBROUTINE SHBER

!-----------------------------------------------------------------------------
SUBROUTINE SIGGE
  Implicit NONE
  Real(dp) :: SIGY
  Integer :: LRS
  if(lgDbg) write(*,'("SIGGE in")')
  IF(UNO >= 0.D0) GO TO 2
  SIG2Y=-1.D0
  WRITE (UT,12)
  WRITE (*,12)
  GO TO 11
2  CONTINUE
  IF(.NOT.TAGE)M=NPUNKT
  IF(TAGE)M=NP(RS)
  IF(.NOT.KOKS.OR.TAGE) GO TO 8
  !modified Aug.2021
  !DO LRS=RS1,RS2
  !  M=M-NIKS(LRS)
  !END DO
  DO LRS=1,RSN
    M = M - NIKS(RSI(LRS))
  END DO

8  CONTINUE
  IF(M-N > 0) GO TO 10
  WRITE (UT,13)
  WRITE (*,13)
  SIG2Y=-1.D0
  GO TO 11
10 SIG2Y=UNO/(M-N)
  SIGY=DSQRT(SIG2Y)
  IF(.NOT.(TAGE.AND.SKRIUT < 0)) WRITE (UT,14) SIGY
  IF(.NOT.(TAGE.AND.SKRIUT < 0)) WRITE (*,14) SIGY
! SLUT
11 CONTINUE
  RETURN
12 FORMAT ("MINUSGROP  (Calc. minimum U is negative)") !##
13 FORMAT ("PUNKTBRIST  (Too few experimental points = zero degrees of freedom")
14 FORMAT ("SIGMA(Y)=",F11.5)
END SUBROUTINE SIGGE

!-----------------------------------------------------------------------------
SUBROUTINE STEGUP
  Implicit NONE
  Integer :: LI, LJ
  if(lgDbg) write(*,'("STEGUP in, STEK(",i0,")=",1pE10.3)') RI,STEK(RI)
  !### if(STEK(RI) /= 0)
  IF(abs(STEK(RI)) > 0.d0) Then
    STEK(RI)=10.D0*STEK(RI)
  Else
    STEK(RI)=K(IK)*0.001D0
  EndIf
  IF(SKRIUT > -1 .or.(SKRIUT == -1 .and. KOKS .and. .not.TAGE)) WRITE (UT,7) IK, STEK(RI)
  IF(SKRIUT > -1 .or.(SKRIUT == -1 .and. KOKS .and. .not.TAGE)) WRITE (*,7) IK, STEK(RI)
  IF(TAGE) GO TO 6
    DO LI=1,N
        IK=IVAR(LI)
        K(IK)=KC(LI)
        CALL PLUSKA (LI)
    END DO
6 DO LI=1,N
   DO LJ=1,N
    SH(LI,LJ)=S(LI,LJ)*STEK(LJ)
   END DO
  END DO
  RI =RI-1
  RETURN
7 FORMAT (3X,"STEG UPP (Increasing Step)",I3,"  new STEK=",1PE10.3)
END SUBROUTINE STEGUP

!-----------------------------------------------------------------------------
SUBROUTINE INVERT(N,A,INDIK)
! MATRIX INVERSION
  USE DIMS_Module, ONLY : MXK
  !Integer, PARAMETER :: MXK = 40
! - number of constants increased to 30
  Real(dp), INTENT(IN OUT) :: A(MXK,MXK)
  Integer, INTENT(IN) :: N
  Integer, INTENT(OUT) :: INDIK
  Real(dp) :: PIVOT(MXK)
  Integer ::  IPIVOT(MXK),INDX(MXK,2)
  Real(dp) :: SWAP,AMAX,T
  Integer ::  J,I,K,L,L1, ICOLUM, IROW, JCOLUM, JROW
  if(lgDbg) write(*,'("INVERT in, N=",I0)') N

  ! INITIALIZATION
  DO J=1,N
    IPIVOT(J)=0
  END DO

  DO I=1,N  !1

    ! SEARCH FOR PIVOT ELEMENT
    AMAX=0.0D0; IROW = -1; ICOLUM = -1
    DO J=1,N  !2
        IF(IPIVOT(J) == 1) GO TO 105
        DO K=1,N  !3
            IF(IPIVOT(K) == 1) GO TO 100
            IF(IPIVOT(K) > 1) GO TO 320
            IF(DABS(AMAX) > DABS(A(J,K))) GO TO 100
            IROW=J
            ICOLUM=K
            AMAX=A(J,K)
            100 CONTINUE
        END DO !3
        105 CONTINUE
    END DO !2
    IPIVOT(ICOLUM) = IPIVOT(ICOLUM) + 1

    ! INTERCHANGE ROWS TO PUT PIVOT ELEMENT ON DIAGONAL
    IF(IROW == ICOLUM) GO TO 260
    DO L=1,N !4
        SWAP=A(IROW,L)
        A(IROW,L)=A(ICOLUM,L)
        A(ICOLUM,L)=SWAP
    END DO  !4
    260 INDX(I,1)=IROW
    INDX(I,2)=ICOLUM
    PIVOT(I)=A(ICOLUM,ICOLUM)

    ! DIVIDE PIVOT ROW BY PIVOT ELEMENT
    IF(DABS(AMAX) > 0.d0) GO TO 330
        320 INDIK=1
        RETURN
    330 A(ICOLUM,ICOLUM)=1.0d0
    DO L=1,N  !5
        A(ICOLUM,L)=A(ICOLUM,L)/PIVOT(I)
    END DO   !5

    ! REDUCE NON-PIVOT ROWS
    DO L1=1,N  !6
        IF(L1 == ICOLUM) GO TO 550
        T=A(L1,ICOLUM)
        A(L1,ICOLUM)=0.0D0
        DO L=1,N  !7
            A(L1,L)=A(L1,L)-A(ICOLUM,L)*T
        END DO  !7
        550 CONTINUE
    END DO  !6

  END DO  !1

  ! INTERCHANGE COLUMNS
  DO I=1,N
    L=N+1-I
    IF(INDX(L,1) == INDX(L,2)) GO TO 710
    JROW=INDX(L,1)
    JCOLUM=INDX(L,2)
    DO K=1,N
        SWAP=A(K,JROW)
        A(K,JROW)=A(K,JCOLUM)
        A(K,JCOLUM)=SWAP
    END DO
    710 CONTINUE
  END DO
  RETURN
END SUBROUTINE INVERT

!-----------------------------------------------------------------------------
SUBROUTINE MULLE (MAT1,MAT2,NRAD,NMEL,NKOL,MAT3,FRAM)
  USE DIMS_Module, ONLY : MXK
  !Integer, PARAMETER :: MXK = 40
  Real(dp), INTENT(IN OUT) :: MAT1(MXK,MXK),MAT2(MXK,MXK),MAT3(MXK,MXK)
  Integer, INTENT(IN) :: NRAD,NMEL,NKOL,FRAM
  Real(dp) :: W
  Integer :: LI, LJ, LM
  if(lgDbg) write(*,'("MULLE in")')
  DO LI=1,NRAD
   DO LJ=1,NKOL
    W=0.D0
    DO LM=1,NMEL
     IF(FRAM == 1) THEN
      W=W+MAT1(LI,LM)*MAT2(LM,LJ)
     ELSE
      W=W+MAT1(LM,LI)*MAT2(LM,LJ)
     END IF
    END DO
    MAT3(LI,LJ)=W
   END DO
  END DO
  RETURN
END SUBROUTINE MULLE

!-----------------------------------------------------------------------------
SUBROUTINE PINUS (PINNE1,MAT,N,PINNE2,FRAM)
  USE DIMS_Module, ONLY : MXK
  !Integer, PARAMETER :: MXK = 40
  Integer, INTENT(IN) :: N,FRAM
  Real(dp), INTENT(IN OUT) :: MAT(MXK,MXK),PINNE1(MXK),PINNE2(MXK)
  Real(dp) :: W
  Integer :: LI, LJ
  if(lgDbg) write(*,'("PINUS in")')
  DO LI=1,N
   W=0.D0
    DO LJ=1,N
     IF(FRAM == 1) THEN
      W=W+PINNE1(LJ)*MAT(LJ,LI)
     ELSE
      W=W+MAT(LI,LJ)*PINNE1(LJ)
     END IF
    END DO
   PINNE2(LI)=W
  END DO
  RETURN
END SUBROUTINE PINUS

END MODULE L2