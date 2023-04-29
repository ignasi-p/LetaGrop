MODULE MIKO_Module
USE DIMS_Module
!Integer, PARAMETER :: MXK = 40, MXAP = 10000, MXS = 25, MXKS = 13, MXAK = 12, MXAS = 12
USE LetaGropModule
Implicit NONE

PRIVATE

Real(dp) :: UNOREM,UNSPAR,ARE(MXK,MXK),DARD(MXK),DARM(MXK),DKBOM(MXK), &
    KBRED(MXK),KREM(MXK),PINA(MXK),RUCKA(MXK,MXK),RUT3(MXK,MXK),SHINV(MXK,MXK)
Integer :: NIMI,NIMMAX,NIMREM,NIPLUS,IMI(MXK),IMIREM(MXK),IMIX(MXK),IPLUS(MXK),IVAL(MXK)
Logical :: MINX(MXK)

Public MIKO

CONTAINS

!-----------------------------------------------------------------------------
SUBROUTINE MIKO
  USE L2, ONLY: INVERT, MULLE
  Implicit NONE
  Integer :: LQ,LI,LJ
  if(lgDbg) write(*,'("Miko in")')
  NIMI=0
  NIPLUS=N
  MINK=.FALSE.
  DO LI=1,N
    IK=IVAR(LI)
    MINX(LI)=.FALSE.
    IF(TAGE) THEN
        KS(RS,IK)=KBOM(LI)
        KBRED(LI)=KBOM(LI)
    ELSE
        K(IK)=KBOM(LI)
        KBRED(LI)=KBOM(LI)
    END IF
    IPLUS(LI)=LI
    IF(TAGE) THEN
        DARD(LI)=DARKS2(RS,IK)
    ELSE
        DARD(LI)=DARK2(IK)
    END IF
  END DO
  CALL GROPPR
  IF(.NOT.BRA) GO TO 14
  IF(.NOT.KASSA.OR.TAGE.OR.SLUSK /= 0) GO TO 13
  DO LI=1,N
    LQ=IVAR(LI)
    IF(DARK2(LQ) < 0.D0) BRA=.FALSE.
  END DO
  IF(BRA) THEN
    STYR=8
  ELSE
    STYR=6
  END IF
  IF(BRA) KASDAR=.TRUE.
  KASSA=.FALSE.
13 GO TO 9900
14 UNSPAR=UNO
  UNOREM=UNO
  DO LI=1,N
    DO LJ=1,N
        SHINV(LI,LJ)=SH(LI,LJ)
    END DO
  END DO
  CALL INVERT (N,SHINV,INDIC)
  IF(INDIC == 1) GO TO 9800
  CALL MULLE (SHINV,RUTA,N,N,N,RUT3,-1)
  CALL MULLE (RUT3,SHINV,N,N,N,RUCKA,1)
! MIKO1
9100 CONTINUE
  CALL INMIX
  CALL INPLUS
  CALL REDGRO
  IF(INDIC == 1) GO TO 9800
  CALL GROPPR
  CALL SKRIV
  IF(.NOT.BRA) GO TO 9100
  MINK=.TRUE.
  CALL REM
  NIMI=0
! UPPMI
9200 NIMI=NIMI+1
  IF(NIMI == NIMMAX) GO TO 9600
  DO LI=1,NIMI
    IVAL(LI)=LI
  END DO
  IVAL(NIMI+1)=0
  GO TO 9500
! RUNT
9300 I=0
! HOPPMI
9400 I=I+1
  IF(IVAL(I) == NIMMAX) GO TO 9200
  IF(IVAL(I+1) == IVAL(I)+1) GO TO 9400
  IVAL(I)=IVAL(I)+1
  LQ=I-1
  IF(LQ < 1) GO TO 9500
  DO LJ=1,LQ
    IVAL(LJ)=LJ
  END DO
! MIKONY
9500 CONTINUE
  DO LI=1,NIMI
    LQ=IVAL(LI)
    IMI(LI)=IMIX(LQ)
  END DO
  CALL INPLUS
  CALL REDGRO
  IF(INDIC == 1) GO TO 9800
  CALL GROPPR
  IF(.NOT.MINK) GO TO 9100
  IF(.NOT.BRA) GO TO 20
  CALL SKRIV
  IF(UNO < UNOREM) CALL REM
20 GO TO 9300
! MIKOUT
9600 CONTINUE
  DO LI=1,N
    IK=IVAR(LI)
    IF(TAGE) THEN
        KS(RS,IK)=KREM(LI)
        DARKS2(RS,IK)=DARM(LI)
    ELSE
        K(IK)=KREM(LI)
        DARK2(IK)=DARM(LI)
    END IF
  END DO
  MINK=.FALSE.
  IF(SLUSK <= 2) GO TO 26
  N=NIDO
  DO LI=1,N
    LQ=IDO(LI)
    IVAR(LI)=IVARGE(LQ)
  END DO
  GO TO 9900
26 CONTINUE
  IF(TAGE.OR..NOT.KASSA) GO TO 9900
  NIMI=NIMREM
  DO LI=1,NIMREM
    IMI(LI)=IMIREM(LI)
  END DO
  CALL INPLUS
  BRA=.TRUE.
  KASDAR=.FALSE.
  W=NIMI*SIGFAK**2*SIG2Y/UNO
  IF(UNOREM > UMIN+W*UMIN) BRA=.FALSE.
  DO LI=1,N
    IF(KREM(LI) > 0.D0  .AND. DARM(LI) < 0.D0) BRA=.FALSE.
    IF(KREM(LI) <= 0.D0 .AND. DARM(LI) > 0.D0) KASDAR=.TRUE.
  END DO
  KASSA=.FALSE.
  IF(.NOT.BRA) THEN
  STYR=6
  KASDAR=.FALSE.
  GO TO 9900
  END IF
  STYR=7
  RI=0
  NIMI=NIMREM-1
  NIPLUS=NIPLUS+1
! UPPRI
9700 RI=RI+1
  J=0
  DO LI=1,NIMREM
    J=J+1
    IF(LI == RI) THEN
        J=J-1
    ELSE
        IMI(J)=IMIREM(LI)
    END IF
  END DO
  WRITE (UT,34)
  WRITE (*,34)
  IPLUS(NIPLUS)=IMIREM(RI)
  RJ=IMIREM(RI)
  CALL REDGRO
  IF(INDIC == 1) GO TO 9800
  CALL SKRIV
  IK=IVAR(RJ)
  K(IK)=KBRED(NIPLUS)
  DARK2(IK)=DARD(NIPLUS)
  KASS(IK)=.TRUE.
  WRITE (UT,35) K(IK),DARK2(IK),(AK(IK,LI),LI=1,NAK)
  WRITE (*,35) K(IK),DARK2(IK),(AK(IK,LI),LI=1,NAK)
  IF(RI < NIMREM) GO TO 9700
  INDIC=2100
  RETURN
! SING(IN)
9800 INDIC=2300
  RETURN
! PROVIN(IN)
9900 INDIC=1900
  RETURN
34 FORMAT(/1X,"DARR AV KASSERADE K(IK) I GROP  (Sigma of rejected K(IK))") !##
35 FORMAT(/22X,"KASS",1PE13.5,3X,"SIGMA=",E11.3,:3X,"AK=",0P10F6.2)
END SUBROUTINE MIKO

!-----------------------------------------------------------------------------
SUBROUTINE GROPPR
  USE DIMS_Module, ONLY : MXK
  !Integer, PARAMETER :: MXK = 40
  Implicit NONE
  Integer :: LQ,LI, NIMI0,NIMI1,IVARUT(MXK)
  if(lgDbg) write(*,'("GroppR in")')
  BRA=.TRUE.
  NIMI0=NIMI
  IF(.NOT.(TAGE.AND.SKRIUT < 0).AND.NIPLUS == 0) WRITE (UT,11)
  IF(.NOT.(TAGE.AND.SKRIUT < 0).AND.NIPLUS == 0) WRITE (*,11)
  IF(NIPLUS >= 1) THEN
  DO LI=1,NIPLUS
    J=IPLUS(LI)
    IK=IVAR(J)
    IF((TAGE.AND.KBRED(LI) < 0.D0).OR.(POSK(IK).AND..NOT.TAGE.AND. & 
        (KBRED(LI) < 0.D0 .OR.(KASSA.AND.DARD(LI) > 0.D0 .AND.KBRED(LI) < SIGFAK*DARD(LI))))) GO TO 1
    GO TO 4
    1 BRA=.FALSE.
    IF(MINX(J)) GO TO 3
    MINX(J)=.TRUE.
    MINK=.FALSE.
    3 NIMI=NIMI+1
    IMI(NIMI)=J
    4 CONTINUE
  END DO
  END IF
  IF(BRA) RETURN
  IF(TAGE.AND.SKRIUT < 0) GO TO 100
  IF(NIMI0 > 0) THEN
    DO LI=1,NIMI0
        LQ=IMI(LI)
        IVARUT(LI)=IVAR(LQ)
    END DO
    WRITE (UT,12) (IVARUT(LI),LI=1,NIMI0)
    WRITE (*,12) (IVARUT(LI),LI=1,NIMI0)
  END IF
  NIMI1=NIMI0+1
  IF(NIMI1 > NIMI) GO TO 100
  DO LI=NIMI1,NIMI
    LQ=IMI(LI)
    IVARUT(LI)=IVAR(LQ)
  END DO
  WRITE (UT,13) (IVARUT(LI),LI=NIMI1,NIMI)
  WRITE (*,13) (IVARUT(LI),LI=NIMI1,NIMI)
! TIG
100 IF(MINK) NIMI=NIMI0
  RETURN
11 FORMAT (/,"ALLES TOT  (ALL OUT = all parameters negative)") !##
12 FORMAT ("UT  (Eliminating)",10I3)
13 FORMAT (/,"MIKO  (Negative parameters)",10I3)
END SUBROUTINE GROPPR

!-----------------------------------------------------------------------------
SUBROUTINE INMIX
  Implicit NONE
  Integer :: LJ
  if(lgDbg) write(*,'("InMix in")')
  I=0
  DO LJ=1,N
    IF(MINX(LJ)) THEN
        I=I+1
        IMI(I)=LJ
        IMIX(I)=LJ
    END IF
  END DO
  NIMMAX=I
  NIMI=I
  RETURN
END SUBROUTINE INMIX

!-----------------------------------------------------------------------------
SUBROUTINE INPLUS
  Implicit NONE
  Logical :: PLUS(12)
  Integer :: LQ,LI
  if(lgDbg) write(*,'("InPlus in")')
  DO LI=1,N
    PLUS(LI)=.TRUE.
  END DO
  DO LI=1,NIMI
    LQ=IMI(LI)
    PLUS(LQ)=.FALSE.
  END DO
  NIPLUS=0
  DO LI=1,N
    IF(PLUS(LI)) THEN
        NIPLUS=NIPLUS+1
        IPLUS(NIPLUS)=LI
    END IF
  END DO
  RETURN
END SUBROUTINE INPLUS

!-----------------------------------------------------------------------------
SUBROUTINE REDGRO
  USE L2, ONLY : INVERT, SIGGE
  Implicit NONE
  Integer :: LI,LJ,LQ,LZ
  if(lgDbg) write(*,'("RedGro in")')
  IF(NIPLUS < 1) GO TO 61
  DO LI=1,NIPLUS
    DO LJ=1,NIPLUS
        LQ=IPLUS(LI)
        LZ=IPLUS(LJ)
        ARE(LI,LJ)=RUCKA(LQ,LZ)
    END DO
  END DO
  CALL INVERT (NIPLUS,ARE,INDIC)
  IF(INDIC == 1) RETURN
  DO LJ=1,NIPLUS
    PINA(LJ)=0.D0
    DO LI=1,NIMI
        LQ=IMI(LI)
        LZ=IPLUS(LJ)
        PINA(LJ)=PINA(LJ)+KBOM(LQ)*RUCKA(LQ,LZ)
    END DO
  END DO
  DO LJ=1,NIPLUS
    W=0.D0
    DO LI=1,NIPLUS
        W=W+PINA(LI)*ARE(LI,LJ)
    END DO
    LZ=IPLUS(LJ)
    KBRED(LJ)=KBOM(LZ)+W
    DKBOM(LZ)=W
  END DO
61 CONTINUE
  UNO=UNSPAR
  DO LI=1,NIMI
    DO LJ=1,NIMI
        LQ=IMI(LI)
        LZ=IMI(LJ)
        UNO=UNO+KBOM(LQ)*KBOM(LZ)*RUCKA(LQ,LZ)
    END DO
  END DO
  IF(NIPLUS >= 1) THEN
    DO LI=1,NIPLUS
        DO LJ=1,NIPLUS
            LQ=IPLUS(LI)
            LZ=IPLUS(LJ)
            UNO=UNO-DKBOM(LQ)*DKBOM(LZ)*RUCKA(LQ,LZ)
        END DO
    END DO
  END IF
  CALL SIGGE
  IF(NIPLUS < 1) RETURN
  DO LI=1,NIPLUS
    J=IPLUS(LI)
    IK=IVAR(J)
    W=SIG2Y*ARE(LI,LI)
    IF(W > 0.D0) THEN
        DARR2=DSQRT(W)
    ELSE
        DARR2=-DABS(STEK(J))
    END IF
    DARD(LI)=DARR2
  END DO
  RETURN
END SUBROUTINE REDGRO

!-----------------------------------------------------------------------------
SUBROUTINE REM
  Implicit NONE
  Integer :: LI
  if(lgDbg) write(*,'("Rem in")')
  UNOREM=UNO
  NIMREM=NIMI
  DO LI=1,NIMI
    IMIREM(LI)=IMI(LI)
    J=IMI(LI)
    IK=IVAR(J)
    KREM(J)=0.D0
    IF(TAGE) THEN
        DARM(J)=DARKS2(RS,IK)
    ELSE
        DARM(J)=DARK2(IK)
    END IF
  END DO
  IF(NIPLUS < 1) RETURN
  DO LI=1,NIPLUS
    J=IPLUS(LI)
    KREM(J)=KBRED(LI)
    DARM(J)=DARD(LI)
  END DO
  RETURN
END SUBROUTINE REM

!-----------------------------------------------------------------------------
SUBROUTINE SKRIV
  Implicit NONE
  Integer :: LI,LJ,LQ
  if(lgDbg) write(*,'("Skriv in")')
  IF(TAGE.AND.SKRIUT < 0) GO TO 100
  WRITE (UT,6) UNO
  WRITE (*,6) UNO
  IF(.NOT.TAGE) GO TO 2
  M=0
  GO TO 4
2 M=NAK
  IF(NIPLUS >= 1) THEN
  DO LI=1,NIPLUS
    LQ=IPLUS(LI)
    IK=IVAR(LQ)
    WRITE (UT,7) IK,KBRED(LI),DARD(LI),(AK(IK,LJ),LJ=1,M)
    WRITE (*,7) IK,KBRED(LI),DARD(LI),(AK(IK,LJ),LJ=1,M)
  END DO
  END IF
  GO TO 100
4 IF(NIPLUS >= 1) THEN
  DO LI=1,NIPLUS
    LQ=IPLUS(LI)
    IK=IVAR(LQ)
    WRITE (UT,7) IK,KBRED(LI),DARD(LI)
    WRITE (*,7) IK,KBRED(LI),DARD(LI)
  END DO
  ENDIF
! TIG
100 CONTINUE
  RETURN
6 FORMAT ("UNO=",1PE14.6)
7 FORMAT (" K",I3," =",1PE11.3,2X,"SIGMA2=",E11.3,:2X,"AK=",0P10F6.2)
END SUBROUTINE SKRIV

END MODULE MIKO_Module