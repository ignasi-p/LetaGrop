MODULE LetaGropData
USE LetaGropModule
USE READIR
USE PutsUbbe, ONLY : PUTS
Implicit NONE

SAVE

CONTAINS

! -----------------------------------------------------------------------------
SUBROUTINE DATA
  Implicit NONE
  Integer :: LI,NAG,INTD,LRS,LRP,LNPRS,IDUM1,IDUM2
  if(lgDbg) write(*,'("DATA in")')
  nowReading = "NS"; CALL READI (NS)
  nowReading = "NAG"; CALL READI (NAG)
  nowReading = "NAS"; CALL READI (NAS)
  nowReading = "NAP"; CALL READI (NAP)
  WRITE (UT,100) NS
  WRITE (*,100) NS
  WRITE (UT,101) NAG
  WRITE (*,101) NAG
  WRITE (UT,102) NAS
  WRITE (*,102) NAS
  WRITE (UT,103) NAP
  WRITE (*,103) NAP
  100 FORMAT (2X,"# SETS OF EXP. POINTS, NS=",I3)
  101 FORMAT (2X,"# COMMON CONSTANTS, NAG=",I3)
  102 FORMAT (2X,"# SET-CONSTANTS, NAS=",I3)
  103 FORMAT (2X,"# EXP. QUANTITIES, NAP=",I3)
  NAPA=NAP
  IF(NAG >= 1) THEN
    WRITE (UT,109)
    WRITE (*,109)
    109 FORMAT ("COMMON CONSTANTS: AG=")
    DO LI=1,NAG
        write(nowReading,'("AG(",I0,")")') LI; CALL READR (AG(LI))
        WRITE (UT,110) AG(LI)
        WRITE (*,110) AG(LI)
        110 FORMAT (1P10E14.5)
    END DO
  ENDIF
  KLAR=.TRUE.
  CALL PUTS

2 CONTINUE
  RS=RS+1
  IF(RS <= NS) GO TO 3
  RS1=1
  RS2=NS
  NPUNKT=0
  DO LRS=1,NS ! DO LRS=RS1,RS2 ! change Aug.2021
    NPUNKT=NPUNKT+NP(LRS)
  END DO
  RSN = NS        ! added Aug.2021
  DO LRS=1,RSN    ! added Aug.2021
     RSI(LRS) = LRS
  END DO
  IDUM1=CELL+NAPA
  WRITE (UT,111) RS1,RS2,NPUNKT, IDUM1
  WRITE (*,111) RS1,RS2,NPUNKT, IDUM1
  111 FORMAT ("SETS ",I0,"-",I0,";  ",I0," POINTS;  max.AP(",I0,")")
  nowReading = ""
  RETURN

3 CONTINUE
  write(nowReading,'("NP(",I0,")")') RS; 
  CALL READI (INTD)
  NP(RS)=INTD
  WRITE (UT,104) RS,NP(RS)
  WRITE (*,104) RS,NP(RS)
  104 FORMAT (2X,"SET #",I2,",  MATRIX OF ",I4," EXP. POINTS")
  IF(RS == 1) GO TO 4
  APCELL(RS)=CELL
  GO TO 5
4 APCELL(RS)=0
  CELL=0
5 CONTINUE
  IF(NAS < 1) GO TO 15
  DO LI=1,NAS
    write(nowReading,'("AS(",I0,",",I0,")")') RS,LI;
    CALL READR (AS(RS,LI))
  END DO
  WRITE (UT,105) (AS(RS,LI),LI=1,NAS)
  WRITE (*,105) (AS(RS,LI),LI=1,NAS)
  105 FORMAT (2X,"SET-CONSTANTS:  AS=",/,3X,1P,99E12.3)
15 CONTINUE
  WRITE (UT,112)
  WRITE (*,112)
  112 FORMAT (8X,"Y",11X,"X ...")
  LNPRS=NP(RS)
  DO LRP=1,LNPRS
    DO LI=1,NAP
        write(nowReading,'("AP, set: ",I0,", point: ",I0,", nAP:",I0)') RS,LNPRS,LI;
        CALL READR (AP(CELL+LI))
    END DO
    IDUM1=CELL+1
    IDUM2=CELL+NAP
    WRITE (UT,110) (AP(LI),LI=IDUM1,IDUM2)
    WRITE (*,110) (AP(LI),LI=IDUM1,IDUM2)
    CELL=CELL+NAPA
  END DO
  CALL PUTS
  GO TO 2

END SUBROUTINE DATA

END MODULE LetaGropData