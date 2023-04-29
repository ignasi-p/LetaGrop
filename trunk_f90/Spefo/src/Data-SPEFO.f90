MODULE LetaGropData  ! for SPEFO
USE LetaGropModule
USE READIR
! USE PutsUbbe, ONLY : PUTS ! SPEFO does not use PUTS
Implicit NONE

SAVE

CONTAINS

!-----------------------------------------------------------------------------
SUBROUTINE DATA ! for SPEFO
  USE IO, ONLY : ErrStop
  Implicit NONE
  Integer :: NLAM,NSOLN,NTAB,RSOLN
  Integer :: NAG, IDUM1, LI, LNPRS, LRP, LRS
  PrgTyp = 1 !## program type = SPEFO
  nowReading = "NS"
  CALL READI (NS)
  nowReading = "NAG"
  CALL READI (NAG)
  nowReading = "NAS"
  CALL READI (NAS)
  nowReading = "NAP"
  CALL READI (NAP)
  WRITE(UT,100) NS
  WRITE(*,100) NS
  WRITE(UT,101) NAG
  WRITE(*,101) NAG
  WRITE(UT,102) NAS
  WRITE(*,102) NAS
  WRITE(UT,103) NAP
  WRITE(*,103) NAP
  NAPA=NAP
  IF(NAG >= 1) Then
    WRITE (UT,109)
    WRITE (*,109)
    DO LI=1,NAG
      Write(nowReading,'("AG(",I0,")")') LI
      CALL READR (AG(LI))
      WRITE (UT,110) AG(LI)
      WRITE (*,110) AG(LI)
    END DO
  EndIf
!change by I.Puigdomenech
  if(AG(1) > 25.d0) then
      write(UT,115) AG(1)
      write(*,115)  AG(1)
      115 FORMAT (1X,"AG(1)=",F6.1," (max. nbr. wavelengths = 25)")
      call ErrStop
  endif
  KLAR=.TRUE.

  NAPA=NAP+16
! FOR SPEFO ONLY
200 CONTINUE
  RS=RS+1
  IF(RS.LE.2) GO TO 300

! ORIGINAL PUTS-CODE INSERTED HERE

  IF(AG(1).LE.0.D0) THEN
    AG(1)=-AG(1)
  ELSE
    POSKIS=.TRUE.
  ENDIF
  RS1=1
  RS2=AG(1)+0.01D0
  NS=RS2
  RSN = NS        ! added Aug.2021
  DO LRS=1,RSN    ! added Aug.2021
     RSI(LRS) = LRS
  END DO
  NLAM=RS2
  NSOLN=NP(1)
  AG(4)=NSOLN
  NTAB=NP(2)
  AG(5)=NTAB
  AG(6)=0.D0
  DO RS=1,RSN
    NP(RS)=0
    DO RSOLN=1,NSOLN
        LRS=NAPA*RSOLN-16-NLAM+RS
        IF(AP(LRS) < 0.D0 .AND. POSKIS .OR. AP(LRS) <= -1.D6 .AND. .NOT.POSKIS) CYCLE
        NP(RS)=NP(RS)+1
        AG(6)=AG(6)+1.D0
    END DO
  END DO
  DO RS=1,RSN
    DO I=1, 13
        KS(RS,I)=0.D0
        DARKS(RS,I)=-1.D0
        DARKS2(RS,I)=-1.D0
    EndDo
  ENDDO
  NPUNKT=AG(6)+0.01D0

! END OF INSERTED CODE

  IDUM1=CELL+NAPA
  WRITE (UT,111) RS1,RS2,NPUNKT, IDUM1
  WRITE (*,111) RS1,RS2,NPUNKT, IDUM1
  nowReading = ""
  RETURN

300 CONTINUE
  Write(nowReading,'("NP(",I0,")")') RS
  CALL READI (NP(RS))
  IF(RS.NE.1) THEN
    APCELL(RS)=CELL
  ELSE
    APCELL(RS)=0
    CELL=0
  ENDIF
  IF(NAS >= 1) THEN
    DO LI=1,NAS
        Write(nowReading,'("AS(",I0,",",I0,")")') RS,LI
        CALL READR (AS(RS,LI))
    END DO
  ENDIF
  LNPRS=NP(RS)
  DO LRP=1,LNPRS
    DO LI=1,NAP
      Write(nowReading,'("AP(",I0,",",I0,")")') LRP,LI
      CALL READR (AP(CELL+LI))
    END DO
    CELL=CELL+NAPA
  END DO
  GO TO 200

100 FORMAT (3X,"# SETS OF EXP. POINTS, NS=",I3)
101 FORMAT (3X,"# COMMON CONSTANTS, NAG=",I3)
102 FORMAT (3X,"# SET-CONSTANTS, NAS=",I3)
103 FORMAT (3X,"# EXP. QUANTITIES, NAP=",I3)
109 FORMAT (8X,"COMMON CONSTANTS: AG=")
110 FORMAT (6X,1P10E14.5)
111 FORMAT (1X,"SETS",I3,"-",I3,3X,I4," POINTS;",5X," MAX. AP(",I0,")")

END SUBROUTINE DATA ! for SPEFO

END MODULE LetaGropData