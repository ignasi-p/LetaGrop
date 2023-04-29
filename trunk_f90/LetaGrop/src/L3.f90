MODULE L3
USE LetaGropModule
Implicit NONE

SAVE

CONTAINS
!-----------------------------------------------------------------------------
! First the three subroutines that read data from the input file:
! LASK, STEG and STYRE
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
SUBROUTINE LASK
! Reads the constants ("Läs K")
USE READIR
USE DIMS_Module, ONLY : MXK
!Integer, PARAMETER :: MXK = 40
Implicit NONE
  Integer :: NBYK,NBYKS,NEGK,SKIN,INTD,  LM,LI,LIK,LJK
  nowReading = "NK"
  CALL READI (M)
  nowReading = "NBYK"
  CALL READI (NBYK)
  IF(NBYK /= -1) GO TO 3
  nowReading = "NEGK"
  CALL READI (NEGK)
  DO LI=1,NEGK
    nowReading = "IK (for POSK)"
    CALL READI (IK)
    POSK(IK)=.FALSE.
  END DO
  nowReading = ""
  RETURN

3 NK=M
  IF(NBYK >= NK) GO TO 7
  IF(NBYK < 1) GO TO 7
  DO LM=1,NBYK
    nowReading = "IK"
    CALL READI (IK)
    Write(nowReading,'("K(",I0,")")') IK
    CALL READR (K(IK))
    DO LI=1,NAK
        Write(nowReading,'("AK(",I0,",",I0,")")') IK,LI
        CALL READR (AK(IK,LI))
    END DO
    DARK(IK)=-1.D0
    DARK2(IK)=-1.D0
    POSK(IK)=.TRUE.
  END DO
7 IF(NBYK /= NK) GO TO 12
  nowReading = "NAK"
  CALL READI (NAK)
  DO LIK=1,NK
    Write(nowReading,'("K(",I0,")")') IK
    CALL READR (K(LIK))
    IF(NAK >= 1) Then
        DO LI=1,NAK
            Write(nowReading,'("AK(",I0,",",I0,")")') IK,LI
            CALL READR (AK(LIK,LI))
        END DO
    EndIf
  END DO
  DO LIK=1,MXK
    DO LJK=1,MXK
        SK(LIK,LJK)=0.D0
    END DO
  END DO
  DO IK=1,MXK
    SK(IK,IK)=1.D0
    DARK(IK)=-1.D0
    DARK2(IK)=-1.D0
    POSK(IK)=.TRUE.
  END DO
12 CONTINUE
  nowReading = "NKS"
  CALL READI (NKS)
  nowReading = "NBYKS"
  CALL READI (NBYKS)
  IF(NBYKS /= -1) GO TO 15
  nowReading = "M"
  CALL READI (M)
  DO LI=1,M
    nowReading = "IK"
    CALL READI (IK)
    DO RS=1,NS
        KS(RS,IK)=0.D0
        DARKS(RS,IK)=-1.D0
        DARKS2(RS,IK)=-1.D0
    END DO
  END DO
15 IF(NBYKS /= 0 .or. NKS == 0) GO TO 20
  nowReading = "M"
  CALL READI (M)
  IF(M < 1) GO TO 171
  DO LI=1,M
    Write(nowReading,'("NIKS(",I0,")")') LI
    CALL READI (INTD)
    NIKS(LI)=INTD
  END DO
171 CONTINUE
  DO RS=1,NS
    DO IK=1,NKS
        KS(RS,IK)=0.D0
        DARKS(RS,IK)=-1.D0
        DARKS2(RS,IK)=-1.D0
    END DO
    IF(M >= 1) Then
        DO LI=1,M
            Write(nowReading,'("KS(",I0,",",I0,")")') RS,NIKS(LI)
            CALL READR (KS(RS,NIKS(LI)))
        END DO
    EndIf
  END DO
20 IF(NBYKS /= NKS) GO TO 23
  IF(NKS == 0) GO TO 26
  DO RS=1,NS
    DO IK=1,NKS
        Write(nowReading,'("KS(",I0,",",I0,")")') RS,IK
        CALL READR (KS(RS,IK))
        DARKS(RS,IK)=-1.D0
        DARKS2(RS,IK)=-1.D0
    END DO
  END DO
  GO TO 26
23 IF(NBYKS /= 1) GO TO 26
  nowReading = "IK"
  CALL READI (IK)
  DO RS=1,NS
    Write(nowReading,'("KS(",I0,",",I0,")")') RS,IK
    CALL READR (KS(RS,IK))
    DARKS(RS,IK)=-1.D0
    DARKS2(RS,IK)=-1.D0
  END DO
26 CONTINUE
  nowReading = "SKIN"
  CALL READI (SKIN)
  IF(SKIN < 1) GO TO 28
  DO LM=1,SKIN
    nowReading = "SKIN - IK"
    CALL READI (IK)
    nowReading = "SKIN - JK"
    CALL READI (JK)
    Write(nowReading,'("SK(",I0,",",I0,")")') IK,JK
    CALL READR (SK(IK,JK))
  END DO
28 CONTINUE
  nowReading = ""
  CALL SKRIK
  RETURN
END SUBROUTINE LASK

!-----------------------------------------------------------------------------
SUBROUTINE STEG
  USE READIR
  Implicit NONE
  Integer :: NVAKS,  LI,LJ,LRS
  nowReading = "N (in STEG)"
  CALL READI (N)
  IF(N >= 1) THEN
    DO LI=1,N
        nowReading = "IK (in STEG)"
        CALL READI (IK)
        Write(nowReading,'("DARK(",I0,") in STEG")') IK
        CALL READR (W)
        IVAR(LI)=IK
        IF(W > 0.D0) DARK(IK)=-W
    END DO
  END IF
  nowReading = ""
  IF(ORVAR == 1) CALL MINUT
  IF(.NOT.KOKS) RETURN
  nowReading = "NSKOTT (in STEG)"
  CALL READI (NSKOTT)
  nowReading = "NVAKS (in STEG)"
  CALL READI (NVAKS)
  IF(RURIK /= 19) GO TO 5
  !DO LRS=RS1,RS2      ! change Aug.2021
  !  NIKS(LRS)=NVAKS
  !END DO
  DO LRS=1,RSN         ! change Aug.2021
    LI = RSI(LRS)
    NIKS(LI)=NVAKS
  END DO
  DO LI=1,NVAKS
    nowReading = "IK (in STEG)"
    CALL READI (IK)
    Write(nowReading,'("DARKS(*,",I0,") in STEG")') IK
    CALL READR (W)
    !DO LRS=RS1,RS2          ! change Aug.2021
    !    VAKS(LRS,LI)=IK
    !    IF(W > 0.D0) DARKS(LRS,IK)=-W
    !END DO
    DO LJ=1,RSN              ! change Aug.2021
        LRS = RSI(LJ)
        VAKS(LRS,LI)=IK
        IF(W > 0.D0) DARKS(LRS,IK)=-W
    END DO
  END DO
5 IF(RURIK /= 20) GO TO 9
  !DO LRS=RS1,RS2    ! change Aug.2021
  !  NIKS(LRS)=0
  !END DO
  DO LJ=1,RSN    ! change Aug.2021
    LRS = RSI(LJ)
    NIKS(LRS)=0
  END DO
  DO LJ=1,NVAKS
    Write(nowReading,'("RS for NVAKS=",I0," (in STEG)")') LJ
    CALL READI (RS)
    Write(nowReading,'("IK for NVAKS=",I0," (in STEG)")') LJ
    CALL READI (IK)
    Write(nowReading,'("DARKS(",I0,",",I0,") in STEG")') RS,IK
    CALL READR (W)
    NIKS(RS)=NIKS(RS)+1
    I=NIKS(RS)
    VAKS(RS,I)=IK
    IF(W > 0.D0) DARKS(RS,IK)=-W
  END DO
9 CONTINUE
  !DO LRS=RS1,RS2    ! change Aug.2021
  !  IF(NIKS(LRS) >= NP(LRS)) NIKS(LRS)=0
  !END DO
  DO LJ=1,RSN    ! change Aug.2021
    LRS = RSI(LJ)
    IF(NIKS(LRS) >= NP(LRS)) NIKS(LRS)=0
  END DO
  nowReading = ""
  RETURN
END SUBROUTINE STEG

!-----------------------------------------------------------------------------
SUBROUTINE STYRE
  USE READIR
  USE IO, ONLY : ErrStop
  Implicit NONE
  Integer :: NIN,NVAR,  LIK,LI,LJ,LQ
  if(lgDbg) write(*,'("STYRE in, STYR=",I0)') STYR
  GO TO (13100,13200,13300,13400,13500,13600,13700,13800),STYR
! STYR1
13100 KLAR=.FALSE.
  DO LIK=1,NK
    KASS(LIK)=.FALSE.
    VAR(LIK)=.FALSE.
  END DO
  nowReading = "NVAR (in STYRE)"
  CALL READI (NVAR)
  ! added lines in 2020
  WRITE(UT,'(" NVAR=",I3)') NVAR
  WRITE(*, '(" NVAR=",I3)') NVAR
  DO LI=1,NVAR
    Write(nowReading,'("IK(",I0,") (in STYRE)")') LI
    CALL READI (IK)
    VAR(IK)=.TRUE.
  END DO
  ! added lines in 2020
  WRITE(UT,'("  IK=",20I3)')  (LI, LI=1,NK)
  WRITE(*, '("  IK=",20I3)')  (LI, LI=1,NK)
  WRITE(UT,'(" VAR=",20L3)')  (VAR(LI), LI=1,NK)
  WRITE(*, '(" VAR=",20L3)')  (VAR(LI), LI=1,NK)

  NOTVAR=0
  DO LIK=1,NK
    IF(VAR(LIK)) GO TO 4
    NOTVAR=NOTVAR+1
    IF(LIK > NOTVAR) CALL BYTA(NOTVAR,LIK)
    4 CONTINUE
  END DO
  nowReading = "NIN in STYRE"
  CALL READI (NIN)
  ! added lines in 2020
  WRITE (UT,'(/," NIN=",I3)') NIN
  WRITE (*, '(/," NIN=",I3)') NIN
  NTOT=NK+NIN
  DO LI=1,NIN
    IK=NK+LI
    Write(nowReading,'("K(",I0,") in STYRE")') IK
    CALL READR (K(IK))
    DO LJ=1,NAK
        Write(nowReading,'("AK(",I0,",",I0,") in STYRE")') IK,LJ
        CALL READR (AK(IK,LJ))
    END DO
    Write(nowReading,'("DARK(",I0,") in STYRE")') IK
    CALL READR (W)
    DARK(IK)=-DABS(W)
    if(DARK(IK) >=0.d0) then
        Write(*,'("Error: negative dark[",i0,"].")') ik
        call ErrStop
    endif
    POSK(IK)=.TRUE.
    VAR(IK)=.TRUE.
    KASS(IK)=.FALSE.
    ! added lines in 2020
    WRITE (UT,'(" NEW K",I2," = ",1PE13.5," DARK=",E13.5," AK=",0P10F6.2)') &
                IK,K(IK),DARK(IK),(AK(IK,LJ), LJ=1,NAK)
    WRITE (*,'(" NEW K",I2," = ",1PE13.5," DARK=",E13.5," AK=",0P10F6.2)') &
                IK,K(IK),DARK(IK),(AK(IK,LJ), LJ=1,NAK)
  END DO
  nowReading = ""
  CALL STUVA
! STYR2
13200 IF(KLAR) GO TO 13900
  NK=NK+1
  N=1
  I=1
  IK=NK
  IVAR(1)=NK
  CALL STEKA
  KMIN(1)=0.D0
  CALL PLUSKA (1)
  IF(ORVAR == 1) ORVAR=-2
  WRITE (UT,1111)
  WRITE (*,1111)
1111  FORMAT(/39(' -')/)
  WRITE (UT,14) (AK(IK,LI),LI=1,NAK)
  WRITE (*,14) (AK(IK,LI),LI=1,NAK)
  STYR=3
  INDIC=1800
  RETURN
! STYR3
13300 N=NK-NOTVAR
  NGE=N
  DO LI=1,N
    IVAR(LI)=NOTVAR+LI
    IVARGE(LI)=IVAR(LI)
  END DO
  CALL MINUT
  CALL SIKIN
  STYR=4
  RSKYTT=NSKYTT+1
! STYR4
13400 CONTINUE
  RSKYTT=RSKYTT-1
  IF(RSKYTT == 1) KASSA=.TRUE.
  CALL SKOTT
  DO I=1,N
    IK=IVAR(I)
    CALL STEKA
  END DO
  DO LI=1,N
    CALL PLUSKA (LI)
  END DO
  INDIC=1800
  RETURN
! STYR5
13500 KLAR=.FALSE.
  IF(NTOT == NK) GO TO 13900
  M=NK
  J=NTOT+1
! RUNT
13550 M=M+1
  J=J-1
  IF(J <= M) GO TO 11
  CALL BYTA (M,J)
  GO TO 13550
11 GO TO 13200
! STYR6
13600 STEKFA=0.5D0*STEKFA
  RSKYTT=NSKYTT+1
  STYR=4
  GO TO 13400
! STYR7
13700 CALL STUVA
  N=0
  STYR=8
  ORVAR=0
  INDIC=1900
  RETURN
! STYR8
13800 CALL MINUT
  KASDAR=.FALSE.
  IF(KASS(NK+1).OR.NK == NTOT) KLAR=.TRUE.
  GO TO 13200
! STYRUT
13900 STYR=0
  N=NK-NOTVAR
  WRITE (UT,1111)
  WRITE (*,1111)
  LQ=NK+1
  IF(LQ > NTOT) GO TO 13
  DO LIK=LQ,NTOT
  WRITE (UT,15) K(LIK),DARK2(LIK),(AK(LIK,LI),LI=1,NAK)
  WRITE (*,15) K(LIK),DARK2(LIK),(AK(LIK,LI),LI=1,NAK)
  KASS(LIK)=.FALSE.
  END DO
13 CALL SKRIK
  INDIC=1000
  RETURN
14 FORMAT(/,2X,"NOW TRYING",2X,"AK=",3X,10F6.2)
15 FORMAT(1X,"KASS",1PE13.5,2X,"SIGMA=",E13.5,2X,:"AK=",0P10F6.2)
END SUBROUTINE STYRE

!-----------------------------------------------------------------------------
SUBROUTINE BYTA (I1,I2)
  USE DIMS_Module, ONLY : MXK
  !Integer, PARAMETER :: MXK = 40
  Implicit NONE
  Integer, INTENT(IN) :: I1,I2
  Integer :: LI
  if(lgDbg) write(*,'("BYTA(",I0,",",I0,") in")') I1,I2
  W=DARK(I1)
  DARK(I1)=DARK(I2)
  DARK(I2)=W
  W=DARK2(I1)
  DARK2(I1)=DARK2(I2)
  DARK2(I2)=W
  W=K(I1)
  K(I1)=K(I2)
  K(I2)=W
  BRA=KASS(I1)
  KASS(I1)=KASS(I2)
  KASS(I2)=BRA
  BRA=POSK(I1)
  POSK(I1)=POSK(I2)
  POSK(I2)=BRA
  BRA=VAR(I1)
  VAR(I1)=VAR(I2)
  VAR(I2)=BRA
  DO LI=1,NAK
    W=AK(I1,LI)
    AK(I1,LI)=AK(I2,LI)
    AK(I2,LI)=W
  END DO
  DO LI=1,MXK
    W=SK(I1,LI)
    SK(I1,LI)=SK(I2,LI)
    SK(I2,LI)=W
  END DO
  DO LI=1,MXK
    W=SK(LI,I1)
    SK(LI,I1)=SK(LI,I2)
    SK(LI,I2)=W
  END DO
  RETURN
END SUBROUTINE BYTA

!-----------------------------------------------------------------------------
SUBROUTINE MININ
  Implicit NONE
  Integer :: LI, LRS, LIK
  if(lgDbg) write(*,'("MININ in")')
  U=UMIN
  UC=U
  IF(N < 1) GO TO 31
  DO LI=1,N
    IK=IVAR(LI)
    KC(LI)=KMIN(LI)
    KV(LI)=KC(LI)
    IF(.NOT.TAGE) GO TO 2
    KS(RS,IK)=KMIN(LI)
    GO TO 3
    2 K(IK)=KMIN(LI)
    3 CONTINUE
  END DO
31 CONTINUE
  IF(.NOT.KOKS.OR.TAGE) GO TO 6
  !DO LRS=RS1,RS2     ! change Aug.2021
  !  DO LIK=1,NKS
  !      KS(LRS,LIK)=KSMIN(LRS,LIK)
  !  END DO
  !END DO
  DO LI=1,RSN         ! change Aug.2021
    LRS = RSI(LI)
    DO LIK=1,NKS
        KS(LRS,LIK)=KSMIN(LRS,LIK)
    END DO
  END DO
6 CONTINUE
  RETURN
END SUBROUTINE MININ

!-----------------------------------------------------------------------------
SUBROUTINE MINUT
  Implicit NONE
  Integer :: LI, LIK, LRS
  if(lgDbg) write(*,'("MINUT in")')
  UMIN=U
  IF(N < 1) GO TO 31
  DO LI=1,N
    IK=IVAR(LI)
    IF(.NOT.TAGE) GO TO 2
        KMIN(LI)=KS(RS,IK)
    GO TO 3
    2 KMIN(LI)=K(IK)
    3 CONTINUE
  END DO
31 CONTINUE
  IF(.NOT.KOKS.OR.TAGE) GO TO 6
  !DO LRS=RS1,RS2         ! change Aug.2021
  !  DO LIK=1,NKS
  !      KSMIN(LRS,LIK)=KS(LRS,LIK)
  !  END DO
  !END DO
  DO LI=1,RSN             ! change Aug.2021
    LRS = RSI(LI)
    DO LIK=1,NKS
        KSMIN(LRS,LIK)=KS(LRS,LIK)
    END DO
  END DO
6 CONTINUE
  RETURN
END SUBROUTINE MINUT

!-----------------------------------------------------------------------------
SUBROUTINE PLUSKA(II)
  Implicit NONE
  Integer, INTENT(IN) :: II
  Integer :: LII, LJ
  Real(dp) :: MAX
  if(lgDbg) write(*,'("PLUSKA (",I0,") IN")') II
  LII=II
  IK=IVAR(LII)
  IF(.not.POSK(IK)) GO TO 3
  MAX=0.D0
  DO LJ=1,N
    MAX=MAX+DABS(S(LII,LJ)*STEK(LJ))
  END DO
  IF(MAX <= K(IK)) GO TO 3
  K(IK)=MAX
  IF(ORVAR == 1) ORVAR=-2
  IF(ORVAR>1 .or. ORVAR == -3) ORVAR=-5
! SLUT
3 KC(LII)=K(IK)
  KV(LII)=K(IK)
  RETURN
END SUBROUTINE PLUSKA

!-----------------------------------------------------------------------------
SUBROUTINE PROVA
  Implicit NONE
  Integer :: LI,LQ
  if(lgDbg) write(*,'("PROVA (IN)")')
  PROV=.FALSE.
  IF(.NOT.KASDAR.OR.TAGE) GO TO 2
  INDIC=2100
  RETURN
2 IF(U <= UMIN.OR.(TAGE.AND.POSKIS)) GO TO 7000
  IF(NIDO /= 0) GO TO 4
  IF(.NOT.(TAGE.AND.SKRIUT < 0)) WRITE (UT,21)
  IF(.NOT.(TAGE.AND.SKRIUT < 0)) WRITE (*,21)
  SLUSK=0
  GO TO 12
4 IF(SLUSK /= 0) GO TO 7
  SLUSK=1
  RIDO=0
  NGE=N
  DO LI=1,NGE
    IVARGE(LI)=IVAR(LI)
  END DO
7 IF(TAGE.AND.SKRIUT == -2) GO TO 12
  WRITE (UT,22)
  WRITE (*,22)
  IF(SLUSK /= 1) GO TO 11
  DO LI=1,NIDO
    LQ=IDO(LI)
    WRITE (UT,23) IVAR(LQ)
    WRITE (*,23) IVAR(LQ)
  END DO
11 WRITE (UT,24) UMIN
  WRITE (*,24) UMIN
  WRITE (UT,25) (KMIN(LI),LI=1,N)
  WRITE (*,25) (KMIN(LI),LI=1,N)
12 CALL MININ
  GO TO 16
! TRAFF
7000 CALL MINUT
  IF(N >= 1) THEN
    DO LI=1,N
        KC(LI)=KMIN(LI)
    END DO
  END IF
  UC=UMIN
  IF(SLUSK == 0) NIDO=0
  IF(.NOT.RAKT) GO TO 16
  RAKT=.FALSE.
  N=NGE
16 CONTINUE
  RETURN
21 FORMAT ("GAMLA KONSTANTER (earlier parameters give better U)")
22 FORMAT ("SLUMPSKOTT (minimum U was hit by chance)")
23 FORMAT (1X,I3)
24 FORMAT ("U-MIN=",1PE12.6)
25 FORMAT (13X,"K-MIN=",1P5E13.6,:(/19X,5E13.6))
END SUBROUTINE PROVA

!-----------------------------------------------------------------------------
SUBROUTINE PROVAR
  Implicit NONE
  Integer :: LI
  if(lgDbg) write(*,'("PROVAR in")')
  IF(.NOT.TAGE) GO TO 2
  WRITE (UT,3) RS,(IVAR(LI),LI=1,N)
  WRITE (*,3) RS,(IVAR(LI),LI=1,N)
  3 FORMAT (" SET",I2," PROVA (Check)",3X,I2,9(9X,I2),:/(22x,I2,9(9x,I2)))
  RETURN
2 WRITE (UT,4) (IVAR(LI),LI=1,N)
  WRITE (*,4) (IVAR(LI),LI=1,N)
  4 FORMAT (" PROVA (Check)",8X,I2,9(9X,I2),:/(22x,I2,9(9x,I2)))
  RETURN
END SUBROUTINE PROVAR

!-----------------------------------------------------------------------------
SUBROUTINE SATSUT (outNr)
  Implicit NONE
  Integer, INTENT(IN) :: outNr
  Integer :: LJ
  if(lgDbg) write(*,'("SATSUT in")')
  WRITE (outNr,1) RS
  IF(outNr == UT) WRITE (*,1) RS
1 FORMAT (/,"SET ",I2)
  IF(NAS >= 1) THEN
    WRITE (outNr,2) (AS(RS,LJ),LJ=1,NAS)
    IF(outNr == UT) WRITE (*,2) (AS(RS,LJ),LJ=1,NAS)
    2 FORMAT (/,("AS=",99F12.6))
  ENDIF
  IF(NKS >= 1) THEN
    WRITE (outNr,3) (KS(RS,LJ),LJ=1,NKS)
    IF(outNr == UT) WRITE (*,3) (KS(RS,LJ),LJ=1,NKS)
    3 FORMAT (/,("KS=",99E13.5))
  ENDIF
  RETURN
END SUBROUTINE SATSUT

!-----------------------------------------------------------------------------
SUBROUTINE SIKIN
  Implicit NONE
  Integer :: ik,jk, LI, LJ
  if(lgDbg) write(*,'("SIKIN in")')
  DO LI=1,N
      ik=IVAR(LI)
      DO LJ=1,N
        jk=IVAR(LJ)
        If(LI <= LJ) Then
            S(LI,LJ)=SK(ik,jk)
        Else
            S(LI,LJ)=0.d0
        EndIf
      END DO
  END DO
  RETURN
END SUBROUTINE SIKIN

!-----------------------------------------------------------------------------
SUBROUTINE SIKREN (MM)
  Implicit NONE
  Integer, INTENT(IN) :: MM
  Integer :: LI, LJ, LMM
  if(lgDbg) write(*,'("SIKREN in")')
  IF(MM < 1) RETURN
  LMM=MM
  DO LI=1,LMM
    DO LJ=1,LMM
        S(LI,LJ)=0.D0
    END DO
    S(LI,LI)=1.D0
  END DO
  RETURN
END SUBROUTINE SIKREN

!-----------------------------------------------------------------------------
SUBROUTINE SKOTT
  Implicit NONE
  Integer :: LI
  if(lgDbg) write(*,'("SKOTT in")')
  IF(.NOT.TAGE) GO TO 2
  IF(N < 1) GO TO 3
  WRITE (UT,6) RS,(IVAR(LI),LI=1,N)
  WRITE (*,6) RS,(IVAR(LI),LI=1,N)
  GO TO 5
2 IF(N < 1) GO TO 4
  WRITE (UT,7) (IVAR(LI),LI=1,N)
  WRITE (*,7) (IVAR(LI),LI=1,N)
  GO TO 5
3 WRITE (UT,6) RS
  WRITE (*,6) RS
  GO TO 5
4 WRITE (UT,7)
  WRITE (*,7)
5 CONTINUE
  NUVAR = 0 ! check the number of times U= is printed
  LI = max(N,1)
  NuvarMAX = NINT(300.0 * SQRT(0.5*REAL(LI))) ! maximum number of times U= is printed
  RETURN
6 FORMAT (/2X,"SET ",I2,2X,"SHOT",10I11,:/(14x,10I11))
7 FORMAT (10X,"SHOT",10I11,:/(14x,10I11))
END SUBROUTINE SKOTT

!-----------------------------------------------------------------------------
SUBROUTINE SKRIK
  Implicit NONE
  Integer :: LI, LIK, LRS
  if(lgDbg) write(*,'("SKRIK in")')
  WRITE (UT,3)
  WRITE (*,3)
  DO LIK=1,NK
    WRITE (UT,4) LIK,K(LIK),DARK2(LIK),(AK(LIK,LI),LI=1,NAK)
    WRITE (*,4) LIK,K(LIK),DARK2(LIK),(AK(LIK,LI),LI=1,NAK)
  END DO
  IF(NKS <= 0 .OR. NS < 1)  GO TO 21
  DO LRS=1,NS
    WRITE (UT,5) LRS
    WRITE (*,5) LRS
    WRITE (UT,6) (LI,KS(LRS,LI),DARKS2(LRS,LI),LI=1,NKS)
    WRITE (*,6) (LI,KS(LRS,LI),DARKS2(LRS,LI),LI=1,NKS)
  END DO
21  CONTINUE
  WRITE (UT,1111)
  WRITE (*,1111)
1111  FORMAT(/39(' -')/)
  RETURN
3 FORMAT (/,"K(IK)=")
4 FORMAT ("K",I2,"=",1PE13.5,"   SIGMA=",E11.3,:"   AK=",0P10F6.2)
5 FORMAT (/1X,"SET ",I2)
6 FORMAT (/,2(:"KS",I2,"=",1PE12.5,"   SIGMA=",E10.3,5X))
END SUBROUTINE SKRIK

!-----------------------------------------------------------------------------
SUBROUTINE SLUSKA (L)
  Implicit NONE
  Integer, INTENT(IN) :: L
  if(lgDbg) write(*,'("SLUSKA (",I0,") in")') L
  I=L
  M=IDO(RIDO)
  IVAR(I)=IVARGE(M)
  IK=IVAR(I)
  CALL STEKA
  IF(TAGE) THEN
    KMIN(I)=KS(RS,IK)
    KC(I)=KMIN(I)
  ELSE
    KMIN(I)=K(IK)
    CALL PLUSKA (I)
  ENDIF
  RETURN
END SUBROUTINE SLUSKA

!-----------------------------------------------------------------------------
SUBROUTINE SLUSS
  Implicit NONE
  Integer :: LI
  if(lgDbg) write(*,'("SLUSS in, SLUSK=",I0)') SLUSK
  GO TO (6001,6002,6003,6004),SLUSK
! SLUSK1
6001 RIDO=RIDO+1
  IF(RIDO <= NIDO) GO TO 2
  IF(NIDO == 1) GO TO 6004
  GO TO 6002
2 N=1
  CALL SLUSKA (1)
  RETURN
! SLUSK2
6002 CONTINUE
  N=2
  RIDO=1
  CALL SLUSKA (1)
  RIDO=2
  CALL SLUSKA (2)
  CALL SIKREN (2)
  SLUSK=3
  RETURN
! SLUSK3
6003 CONTINUE
  RIDO=1
  CALL SLUSKA (1)
  RIDO=2
  CALL SLUSKA (2)
  SLUSK=4
  RETURN
! SLUSK4
6004 CONTINUE
  SLUSK=0
  N=NGE
  NIDO=0
  DO LI=1,N
    IVAR(LI)=IVARGE(LI)
  END DO
  CALL MINUT
  INDIC=2001
  RETURN
END SUBROUTINE SLUSS

!-----------------------------------------------------------------------------
SUBROUTINE STEKA
  Implicit NONE
  if(lgDbg) write(*,'("STEKA in")')
  IF(.NOT.TAGE) GO TO 2
  W=DARKS(RS,IK)
  GO TO 3
2 W=DARK(IK)
3 IF(W <= 0.D0) GO TO 5
  STEK(I)=STEKFA*W
  GO TO 6
5 STEK(I)=-W
6 CONTINUE
  RETURN
END SUBROUTINE STEKA

!-----------------------------------------------------------------------------
SUBROUTINE STUVA
  Implicit NONE
  Integer :: RN, LIK, LQ
  if(lgDbg) write(*,'("STUVA in")')
  M=0
  LQ=NOTVAR+1
  IF(LQ > NK) GO TO 11
  DO LIK=LQ,NK
    IF(KASS(LIK)) M=M+1
  END DO
11 NK=NK-M
  RN=NTOT
! NED
12000 RN=RN-1
  IF(RN == NOTVAR) GO TO 12900
  IK=RN
! UPP
12500 IF(.NOT.KASS(IK).OR.KASS(IK+1)) GO TO 4
  CALL BYTA (IK,IK+1)
  IF(IK >= NTOT-1) GO TO 4
  IK=IK+1
  GO TO 12500
4 GO TO 12000
! SLUT
12900 IF(KASS(NK+1)) KLAR=.TRUE.
  RETURN
END SUBROUTINE STUVA

!-----------------------------------------------------------------------------
SUBROUTINE UVAR (outNr)
  USE DIMS_Module, ONLY : MXK
  Implicit NONE
  !Integer, PARAMETER :: MXK = 40
  Integer, INTENT(IN) :: outNr
  Real(dp) :: KUT(MXK)
  Integer :: LI, LQ
  if(lgDbg) write(*,'("UVAR in")')
  NUVAR = NUVAR +1
  IF(N < 1) GO TO 5
  IF(.NOT.TAGE) GO TO 3
  DO LI=1,N
    LQ=IVAR(LI)
    KUT(LI)=KS(RS,LQ)
  END DO
  WRITE (outNr,6) U,(KUT(LI),LI=1,N)
 IF(outNr == UT)  WRITE (*,6) U,(KUT(LI),LI=1,N)
  RETURN
3 DO LI=1,N
    LQ=IVAR(LI)
    KUT(LI)=K(LQ)
  END DO
  WRITE (outNr,7) U,(KUT(LI),LI=1,N)
  WRITE (*,7) U,(KUT(LI),LI=1,N)
  RETURN
5 WRITE (outNr,8) U
  IF(outNr == UT) WRITE (*,8) U
  IF(outNr == UT) THEN 
    IF(Rf < 1.D0 .AND. Rf > 0.D0) WRITE (UT,9) Rf
    IF(Rf < 1.D0 .AND. Rf > 0.D0) WRITE (*,9) Rf
  ENDIF
  RETURN
6 FORMAT ("U= ",1PE12.6,1X,"KS=",10E11.4,:/19X,10E11.4)
7 FORMAT ("U= ",1PE12.6,1X,"KV=",10E11.4,:/19X,10E11.4)
8 FORMAT ("U= ",1PE12.6)
9 FORMAT ("Weighted R-factor: ",1PE12.4,/)
END SUBROUTINE UVAR

END MODULE L3