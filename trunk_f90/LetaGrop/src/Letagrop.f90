!LETAGROP MOD/85 (VERSION IBM XT/FORTRAN H)
PROGRAM LetaGROP
USE IO, ONLY : Quit
USE LetaGropModule
USE PutsUbbe
USE READIR
USE L2
USE L3
USE MIKO_Module
USE LetaGropData
USE SARK_Module
Implicit NONE
Character(len=150) :: HUVUD
Real(dp) :: RLIM
Integer :: li, lrs

Logical, PARAMETER :: extraLineRurik14 = .false.

SAVE

! open input and output files, etc, in module PutsUbbe
Call NAMN

VMAX=120
ORVAR=0
N=0
NIDO=0
RS=0
SLUSK=0
STYR=0
FAS2=.false.
KASSA=.false.
KASDAR=.false.
KOKS=.false.
MINK=.false.
PROV=.false.
RAKT=.false.
TAGE=.false.
SKRIUT=1
STEKFA=0.5D0
TOLU=0.000001D0
Rf=1.0D0
RLIM=1.0D-25
NUVAR = 0;  NuvarMAX = 200
! Program type 0: standard, 1: SPEFO, 2: ModFnk
PrgTyp = 0
WRITE (UT,*)
WRITE (*,*)
! ---- KNUT
1000 CONTINUE
    nowReading = "RURIK"
    CALL READI (RURIK);  nowReading = ""
    IF(RURIK /= 14) WRITE (UT,152) RURIK
    IF(RURIK /= 14) WRITE (*,152)  RURIK
    152 FORMAT ("*** RURIK=",I2)

    IF(RURIK < 1 .OR. RURIK > 21) GO TO 99999 ! change Aug.21 (rurik = 21)
    GO TO (1001,1002,1003,1004,1005,1006,1007,1008,1009,1010, &
           1011,1012,1013,1014,1015,1016,1017,1018,1019,1020,1021),RURIK ! change Aug.21 (rurik = 21)
! RUR1
 1001 CONTINUE
      IF(.not.(.not.KOKS.or.ORVAR /= 1)) THEN
        RAKT=.TRUE.
        NGE=N
      END IF
      N=0
      GO TO 1800 !// LETA
! RUR2
 1002 WRITE (UT,100)
      WRITE (*,100)
      100 FORMAT(/,39(' -')/)
      WRITE (UT,160)
      WRITE (*,160)
      160 FORMAT (10X,"PRINT-OUT (UTTAG)")
      GO TO 1001
! RUR3
 1003 WRITE (UT,100)
      WRITE (*,100)
      KOKS=.FALSE.
      CALL STEG
      GO TO 1000
! RUR4
 1004 CONTINUE
      nowReading = "STEKFA / TOLU"
      CALL READR (W)
      IF(W > 0) THEN
        STEKFA=W
      ELSE
        TOLU=-W
      END IF
      WRITE (UT,161) STEKFA,TOLU
      WRITE (*,161) STEKFA,TOLU
      161 FORMAT ("STEP-FACTOR (STEKFAK)=",F8.3,5X,"U-TOL=",1PE11.1)
      GO TO 1000
! RUR5
 1005 CONTINUE
      IF(Rf < RLIM) THEN
        WRITE (UT,167)
        WRITE (*,167)
        167 FORMAT ("Cannot converge anymore") !##
        GO TO 1000
      END IF
      DO I=1,N
        IK=IVAR(I)
        CALL STEKA
      END DO
      CALL SIKIN
      DO LI=1,N
        CALL PLUSKA (LI)
      END DO
      CALL SKOTT
      GO TO 1800 !// LETA
! RUR6
 1006 RS=0
      ORVAR=0
      KLAR=.FALSE.
      Call DATA
      GO TO 1000
! RUR7
 1007 ORVAR=0
      CALL LASK
      GO TO 1000
! RUR8
 1008 CONTINUE
      IF(PrgTyp == 2) THEN
        WRITE (UT,'(" (warning: Rurik 8 not used in LetaGrop-ModFnk)")')
        WRITE (*, '(" (warning: Rurik 8 not used in LetaGrop-ModFnk)")')
      ENDIF
      nowReading = "NOK"
      CALL READI (NOK)
      nowReading = "STEGBY"
      CALL READR (STEGBY)
      DO LI=1,NOK
        Write(nowReading,'("START(",I0,")")') LI
        CALL READR (START(LI))
        Write(nowReading,'("TOL(",I0,")")') LI
        CALL READR (TOL(LI))
      END DO
      WRITE (UT,162) STEGBY,(TOL(LI),LI=1,NOK)
      WRITE (*,162)  STEGBY,(TOL(LI),LI=1,NOK)
      162 FORMAT ("STEGBYT=",F8.3,5X,"TOL =",1P6E11.1)
      GO TO 1000
! RUR9
 1009 CONTINUE
      nowReading = "TYP"
      CALL READI (TYP)
      VAL=1
      KOKS=.FALSE.
      ORVAR=0
      POSKIS=.FALSE.
      WRITE (UT,153) TYP
      WRITE (*,153) TYP
      153 FORMAT ("TYPE=",I2)
      WRITE (UT,154) VAL
      WRITE (*,154) VAL
      154 FORMAT ("VAL=",I2)
      GO TO 1000
! RUR10
 1010 CONTINUE
      IF(PrgTyp == 2) THEN
        WRITE (UT,'(" (warning: Rurik 10 not used in LetaGrop-ModFnk)")')
        WRITE (*, '(" (warning: Rurik 10 not used in LetaGrop-ModFnk)")')
      ENDIF
      nowReading = "VMAX"
      CALL READI (VMAX)
      GO TO 1000
! RUR11
 1011 CONTINUE
      nowReading = "RS1"
      CALL READI (RS1)
      nowReading = "RS2"
      CALL READI (RS2)
      nowReading = ""
      IF(RS1 < 1 .OR. RS1 > NS .OR. RS2 < 1 .OR. RS2 > NS) THEN
        WRITE(UT,150) RS1,RS2,NS
        WRITE(*,150) RS1,RS2,NS
        150 FORMAT("ERROR: RS1=",I0," RS2=",I0,", both must be >0 and <=",I0)
        GOTO 99999
      ENDIF
      RSN = RS2-RS1+1
      DO LRS=1,RSN
        RSI(LRS) = RS1 + LRS -1
      END DO
      KOKS=.FALSE.
      ORVAR=0
      NPUNKT=0
      DO LRS=RS1,RS2
         NPUNKT=NPUNKT+NP(LRS)
      END DO
      WRITE (UT,155) RSN, RS1,RS2,NPUNKT
      WRITE (*,155) RSN, RS1,RS2,NPUNKT
      155 FORMAT (I0," SETS: ",I0," - ",I0,";  ",I0," POINTS")
      GO TO 1000
! RUR21  ! added Aug.2021 (Rurik = 21)
 1021 CONTINUE
      KOKS=.FALSE.
      ORVAR=0
      NPUNKT=0
      nowReading = "RSN"
      CALL READI (RSN)
      DO LRS=1,RSN
        write(nowReading,'("RSI(",i0,")")') LRS
        CALL READI (IK)
        RSI(LRS) = IK
        IF(IK < 1 .OR. IK > NS) THEN
            WRITE(UT,151) LRS,IK,NS
            WRITE(*,151) LRS,IK,NS
            151 FORMAT("ERROR: RSI(",I0,")=",I0," must be >0 and <=",I0)
            GOTO 99999
        ENDIF
        NPUNKT=NPUNKT+NP(IK)
      END DO
      nowReading = ""
      DO LRS=2,RSN
        IK = RSI(LRS)
        DO LI = 1,LRS-1
            IF(IK == RSI(LI)) THEN
                WRITE(UT,170)  RSN,(RSI(I),I=1,RSN)
                WRITE(*,170)  RSN,(RSI(I),I=1,RSN)
                170 FORMAT("RSN = ",I0,", SETS: ",20(10I4,:/6x))
                WRITE(UT,171) IK
                WRITE(*,171) IK
                171 FORMAT("ERROR: duplicated set number: ",I0)
                GOTO 99999
            ENDIF
        END DO
      END DO
      WRITE (UT,156) RSN,NPUNKT,(RSI(LRS),LRS=1,RSN)
      WRITE (*,156) RSN,NPUNKT,(RSI(LRS),LRS=1,RSN)
      156 FORMAT (I0," SETS  ",I0," POINTS",/,"SETS: ",20(10I4,:/6x))
      GO TO 1000
! RUR12
 1012 CONTINUE
      nowReading = "SKRIUT"
      CALL READI (SKRIUT)
      WRITE (UT,163) SKRIUT
      WRITE (*,163) SKRIUT
      163 FORMAT ("SKRIKUT  (Output suppression) = ",I0) !##
      GO TO 1000
! RUR13
 1013 CALL SKRIK
      GO TO 1000
! RUR14
 1014 CONTINUE
      nowReading = ""
      READ(INFL,'(A)') HUVUD
      WRITE (UT,157) trim(HUVUD)
      WRITE (*,157)  trim(HUVUD)
      157 FORMAT (A)
      if(extraLineRurik14) then
        WRITE (UT,*)
        WRITE (*,*)
      endif
      GO TO 1000
! RUR15
 1015 CONTINUE
      DO IK=1,NK
        IF(POSK(IK)) CALL LOGKIK
      END DO
      WRITE (UT,*)
      WRITE (*,*)
      GO TO 1000
! RUR16
 1016 WRITE (UT,159)
      WRITE (*,159)
      159 FORMAT (////)
      GO TO 1000
! RUR17
 1017 CONTINUE
      nowReading = "STYR"
      CALL READI (STYR)
      nowReading = "SIGFAK"
      CALL READR (SIGFAK)
      nowReading = "NSKYTT"
      CALL READI (NSKYTT)
      nowReading = ""
      WRITE (UT,100)
      WRITE (*,100)
      WRITE (UT,166) STYR,SIGFAK
      WRITE (*,166)  STYR,SIGFAK
      166 FORMAT (10X,"STYR=",I2,3X,"SIGFAK=",F5.1)
      GO TO 2100 !// STYRE
! RUR18
 1018 CONTINUE
      nowReading = "VAL"
      CALL READI (VAL)
      WRITE (UT,100)
      WRITE (*,100)
      WRITE (UT,154) VAL
      WRITE (*,154) VAL
      ORVAR=0
      GO TO 1000
! RUR19
 1019 GO TO 1020
! RUR20
 1020 KOKS=.TRUE.
      ORVAR=0
      WRITE (UT,100)
      WRITE (*,100)
      WRITE (UT,164)
      WRITE (*,164)
      164 FORMAT (22X,"KOKS  (Adjusting both K and KS)")  !##
      CALL STEG
      GO TO 1000
! KNUT SLUT

! SARK(IN)
 1600 CONTINUE
      CALL SARK
      IF(INDIC == 1800) GO TO 1800 !// LETA
! UBBEUT
 1700 CONTINUE
      IF(ORVAR == -2) ORVAR=-1
      IF(ORVAR == 0) ORVAR=1
      IF(N == 0) THEN
        PROV=.TRUE.
        CALL MINUT
      ENDIF
      IF(N == 1) GO TO 1801 !// ENSAM
      IF(PROV) GO TO 1950 !// PROVA
      GO TO 1800 !// LETA
! LETA(IN)
 1800 CONTINUE
      if(lgDbg) write(*,'("LETA(IN)  N=",I0,", ORVAR=",I0)') N, ORVAR
      IF(N == 0) GO TO 1317 !// UBBEIN
      IF(ORVAR == 0 .OR. ORVAR == -2) GO TO 1317 !// UBBEIN
      IF(N == 1) GO TO 1801 !// ENSAM
      GO TO 1802
! ENSAM(IN)
 1801 CONTINUE
      CALL ENSAM
      IF(INDIC == 1317) GO TO 1317 !// UBBEIN
      IF(INDIC == 2000) GO TO 2000 !// PROVUT
      GO TO 2100 !// STYRE
 1802 CONTINUE
      CALL LETA
      if(lgDbg) write(*,'("LETA ut, INDIC=",I0)') INDIC
      IF(INDIC == 1317) GO TO 1317 !// UBBEIN
      CALL GROP
      if(lgDbg) write(*,'("GROP ut, INDIC=",I0)') INDIC
      IF(INDIC == 2200) GO TO 2200 !// MYKO
      IF(INDIC == 1900) GO TO 1900 !// PROVIN
      GO TO 2300 !// SING
! PROVIN
 1900 CONTINUE
      PROV=.TRUE.
      IF(.NOT.(KOKS.AND.SKRIUT < 0)) CALL PROVAR
      GO TO 1317 !// UBBEIN
! PROVA(IN)
 1950 CONTINUE
      CALL PROVA
      IF(INDIC == 2100) GO TO 2100 !// STYRE
      GO TO 2000 !// PROVUT
! PROVUT
 2000 CONTINUE
      IF(SLUSK <= 0) GO TO 2001 !// PROVUT1
      CALL SLUSS
      IF(INDIC == 2001) GO TO 2001 !// PROVUT1
      GO TO 1800 !// LETA
! PROVUT1
 2001 CONTINUE
      IF(TAGE) GO TO 1600 !// SARK
      IF(STYR > 0) GO TO 2100 !// STYRE
      IF(Rf <= RLIM) THEN
        WRITE (UT,169)
        WRITE (*,169)
        169 FORMAT ("CONVERGED")
      ENDIF
      GO TO 1000
! STYRE(IN)
 2100 CONTINUE
      CALL STYRE
      IF(INDIC == 1800) GO TO 1800 !// LETA
      IF(INDIC == 1900) GO TO 1900 !// PROVIN
      GO TO 1000
! MIKO(IN)
 2200 CONTINUE
      CALL MIKO
      IF(INDIC == 1900) GO TO 1900 !// PROVIN
      IF(INDIC == 2100) GO TO 2100 !// STYRE
! SING
 2300 WRITE (UT,165)
      WRITE (*,165)
      165 FORMAT (//,"SING  (Singular matrix)",//) !##
      GO TO 1000
! UBBE(IN)
 1317 CONTINUE
! NOT FOR SPEFO
      IF(PrgTyp /= 1 .and. (KOKS .and. .not.TAGE .and. .not.RAKT)) GO TO 1600 !// SARK
      CALL UBBE
      if(lgDbg) Write(*,'("UBBE UT, INDIC=",i0)') INDIC
      IF(INDIC == 9) GO TO 99999
      IF(NUVAR >= NuvarMAX) THEN
          WRITE (*,'(1x,"Something is wrong.  NUVAR=",i0," NuvarMAX=",i0," N=",i0)') NUVAR, NuvarMAX, N !##
          WRITE (UT,'(1x,"Something is wrong.  NUVAR=",i0," NuvarMAX=",i0," N=",i0)') NUVAR, NuvarMAX, N
          GO TO 99999 !! avoid infinite loops
      END IF
! FOR SPEFO ONLY
      IF(PrgTyp == 1 .AND. INDIC == 1600) GO TO 1600 !// SARK
      GO TO 1700 !// UBBEUT

! FINAL
99999 CONTINUE
      call CloseFiles ! in module PutsUbbe
      call Quit

END PROGRAM LetaGROP
