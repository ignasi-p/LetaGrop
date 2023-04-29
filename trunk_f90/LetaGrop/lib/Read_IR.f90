MODULE IO ! Input-Output
   Implicit NONE
   Integer, PARAMETER :: Max_Path = 260 ! max length of a file full path name
   Private
   Public Max_Path, ErrStop, Quit, PauseMe, UpCase, getExt, getParent, TimDat
   SAVE

CONTAINS
!-----------------------------------------------------------------------------
    SUBROUTINE ErrStop
    Implicit NONE
    call PauseMe
    Stop 1
    END SUBROUTINE ErrStop
!-----------------------------------------------------------------------------
    SUBROUTINE Quit
    Implicit NONE
    call PauseMe
    Stop
    END SUBROUTINE Quit
!-----------------------------------------------------------------------------
    SUBROUTINE PauseMe
    Implicit NONE
    Integer :: I, ierr
    WRITE (*, '("Press Enter to continue ...")', Advance='No')
    READ (*,'(I5)',iostat=ierr) I
    RETURN
    END SUBROUTINE PauseMe

!-----------------------------------------------------------------------------
SUBROUTINE UpCase (I1)
    !Convert a character variable to upper-case
    Implicit NONE
    Character, INTENT(IN OUT) :: I1*(*)
    Character (LEN=1) :: CH
    Integer :: I,J,NCHR
    NCHR = len_trim(I1)
    DO I=1,NCHR
      CH=I1(I:I)
      J=iachar(CH)
      IF((J >= 97).AND.(J <= 122)) Then
          I1(I:I)=CHAR(J-32)
          ! Next lines for console applications (based on ASCII character set)
          ! Only a few ASCII "accented" letters have upper-case version
      Else If(J==134) then     !å
          I1(I:I)=ACHAR(143)
      Else If(J==132) then     !ä
          I1(I:I)=ACHAR(142)
      Else If(J==148) then     !ö
          I1(I:I)=ACHAR(153)
      Else If(J==145) then     !æ
          I1(I:I)=ACHAR(146)
      Else If(J==135) then     !ç
          I1(I:I)=ACHAR(128)
      Else If(J==129) then     !ü
          I1(I:I)=ACHAR(154)
      Else If(J==130) then     !é
          I1(I:I)=ACHAR(144)
      Else if(J==164) Then     !ñ
          I1(I:I)=ACHAR(165)
      EndIf
    END DO
    RETURN
    END SUBROUTINE UpCase

!-----------------------------------------------------------------------------
SUBROUTINE getExt (fName,EXT)
! Extract from "fName" the extension, which is returned in "EXT"
! while the file name without the extension is returned in "fName"
!   fName = On input contains a (long) file name like: 'file.dat'
!           or 'c:\data\file.dat', 'dk0:[data]file.dat', etc.
!           On output contains the file name without extension
!           (either 'file' or 'c:\data\file', etc).
!           If the size of the the variable "fName" is too short
!           for an extension to be added (which requires 4 characters)
!           then on output fName is returned as blank.
!   EXT   = on output contains the extension (e.g. 'dat')
! If fName on input does not contain an extension, for example it is
! '..\..\file1', then on output EXT is blank.
! It is assumed that the extension is 2 or 3 letter long.
!     fName = 'data.old', then on output: fName='data' and EXT='old'
!     fName = 'data.ps',  then on output: fName='data' and EXT='ps'
!     fName = 'data.obsolete' on input, then EXT=' ' and fName='data.obsolete'
!     fName = 'data.1' on input, then EXT=' ' and fName='data.1'
Implicit NONE
Character, INTENT(IN OUT)  :: fName*(*)
Character, INTENT(OUT) :: EXT*(*)
Character (LEN=1) :: CH1
Integer :: FN_length, FN_len, ndot
EXT = ' '
FN_length = len_trim(fName)
if(FN_length <= 0) Return
ndot = Index(fName,'.',Back=.true.) ! find the position of the last '.'
FN_len = Len(fName)
If(ndot < FN_len) Then     ! is it followed by a '\'? as in '.\d\file'?
   CH1 = fName(ndot+1:ndot+1)
   If(CH1=='\'.or.CH1=='/'.or.CH1==':') ndot=0
   EndIf
if(ndot > 1) then       ! is it preceeded by a '\'? as in '.\.file'?
   CH1 = fName(ndot-1:ndot-1)
   If(CH1=='\'.or.CH1=='/'.or.CH1==':') ndot=0
   End If
!only extns that are 2 or 3 chars long
If(FN_length-ndot /= 3 .and. FN_length-ndot /= 2) ndot=0
! is variable fName too short to accept a 3-letter extension at the end?
If(ndot > FN_len - 3 .or. (ndot <=1 .and. FN_length > FN_len - 4)) Then
   Write(*,1001) fName(1:FN_length), (FN_len-4)
   1001 Format (' ? File name:',/1x,'"',A,'"',/1x,'? Name too long. Should be max.',I4,' characters.')
   fName = ' '
   Return
   EndIf
if(ndot > 1) then    ! ndot>1: fName can not be empty
                     ! (in case fName='.dat' on input)
      EXT = fName(ndot+1:FN_length)
      fName = fName(1:ndot-1)
   EndIf
Return
END SUBROUTINE getExt

!-----------------------------------------------------------------------------
SUBROUTINE getParent (fName,PATH)
! Extract from "fName" the path for the file, which is returned in "PATH"
! while the file name without the path is returned in "fName"
!   fName = On input contains a (long) file name like: 'file.dat',
!            'c:file.dat' or 'c:\data\file.dat', '..\..\file.dat', etc.
!           On output contains the file name without the path
!           ('file.dat').
!   PATH  = on output contains the path (e.g. 'dk0:\data\')
! If fName on input does not contain a path, for example it is 'file1',
!    then on output PATH is blank.
! If fName on input does not contain a file name, for example it is
!    'c:\', then on output fName is blank.
Implicit NONE
Character, INTENT(IN OUT)  :: fName*(*)
Character, INTENT(OUT) :: PATH*(*)
Character (LEN=1), PARAMETER :: SLASH = "\"
Integer :: FN_length, nbSLASH, nColon
PATH = ""
FN_length = len_trim(fName)
if(FN_length <= 0) Return ! empty file name
nbSLASH = Index(fName,SLASH,Back=.true.) ! find the position of the last slash
if(nbSLASH <= 0) then ! no back slash (\) found
    nColon = Index(fName,':') ! find the position of the first colon (:)
    if(nColon <=0) Return ! no colon (:) found
    if(nColon >= FN_length) then
        Write(*,100) trim(fName)
        100 Format ('"getParent": File name:',/1x,'"',A,'"',/,'Contains only a path?')
        PATH = fName(1:nColon)
        fName = ''
        Return
    endif
    PATH = fName(1:nColon)
    fName = fName(nColon+1:)
    Return
endif
! found the last position of slash
if(nbSLASH >= FN_length) then
   Write(*,100) trim(fName)
   PATH = fName(1:nbSLASH)
   fName = ''
   Return
endif
PATH = fName(1:nbSLASH)
fName = fName(nbSLASH+1:)
Return
END SUBROUTINE getParent

!-----------------------------------------------------------------------------
SUBROUTINE TimDat (date,time)
!Return the current value of DATE and TIME
!  in character strings of any length
Implicit NONE
Character (LEN=*), INTENT(OUT)   :: date, time
! Minimalistic GNU for Windows begins
integer val(8), i
CHARACTER*30 dag, tid
call date_and_time(VALUES=val)
dag = ''
tid = ''
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

END MODULE IO

!-----------------------------------------------------------------------------
! Subroutine Library with routines  (READA, READI and READR) to read
! a character, an integer or a real (** DOUBLE PRECISION **)
!
! Version: Febr.1992   FORTRAN-77
! Code converted to F90;  Date: May-2007
! Revised: 2020 for compilation with MinGW Fortran
! Revised: 2021 to read from two input files simultaneously
!-----------------------------------------------------------------------------
! Purpose: to read data from an opened input file (unit=INFL or INFL2),
!          where the input data is separated by either of:
!          1) commas, 2) spaces (blank characters), or 3) return (new line)
! Usage (for a single input file):
!    USE IO
!    USE READIR
!    DOUBLE PRECISION D
!    Integer I
!    CHARACTER*10 CH
!    INFL = 40
!    OPEN(INFL,FILE=NAME,STATUS="OLD")
!    CALL READI(I)
!    CALL READR(D)
!    CALL READA(CH)
!  The input file might be one line long:
!         55,2.3E-5,banana
!    or several lines long:
!         55
!         0.000023
!         banana
!  Comments may be written at the end of lines following white space
!  and slash "/",  for example:
!         55,2.3E-5 / This is a comment
!         banana      / this is a fruit
!--------------------------------------------------------------------------
! Data is read to a file opened on unit "INFL".
! With two different input files: open the two files on
! units INFL and INFL2, and then set variable "firstInput" as follows:
!    USE IO
!    USE READIR
!    DOUBLE PRECISION D
!    Integer I
!    CHARACTER*10 CH
!    INFL = 40; INFL2 = 41
!    OPEN(INFL, FILE="file1.dat",STATUS="OLD")
!    OPEN(INFL2,FILE="file2.dat",STATUS="OLD")
!    CALL READI(I) ! read from file INFL (by default)
!    firstInput = .false.
!    CALL READR(D) ! read from file INFL2
!    firstInput = .true.
!    CALL READA(CH) ! read again from file INFL
!--------------------------------------------------------------------------
MODULE READIR
   !--------------------------------------------------------------------------
   !  Reading is performed on unit = INFL
   !  Input lines might be 200 characters long.
   !  ReadR returns a Double Precision
   !--------------------------------------------------------------------------
   USE IO, ONLY: ErrStop
   Implicit NONE
   ! default numbers for the input files (max two)
   Integer :: INFL=20, INFL2=22
   Integer, PARAMETER :: LINEL=200
   Integer, PARAMETER :: LINE2=LINEL+2
   Character (LEN=1)  :: TOTC*(LINEL), BUFFER(LINE2)
   Character (LEN=75) :: nowReading = ""
   Character (LEN=LINEL) :: fileName

   Character (LEN=1) :: CHARAC(19)
   !            1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19
   DATA CHARAC/"+","-","E",".",","," ","0","1","2","3","4","5","6","7","8","9","D","e","d"/

   Integer :: TACCINA(4,2), TESTADA(4,2)
   DATA TACCINA/5,1,2,4,   3,2,2,3/
   DATA TESTADA/0,1,2,0,   0,2,2,0/
   Integer :: TACCINI(8,4), TESTADI(8,4)
   DATA TACCINI/1,2,5,5,5,1,3,6,      5,5,5,5,5,1,3,6,  &
                5,5,5,5,4,4,3,4,      5,5,5,5,7,1,5,7/
   DATA TESTADI/2,2,0,0,0,1,3,0,      0,0,0,0,0,2,3,0,  &
                0,0,0,0,5,4,3,5,      0,0,0,0,5,4,0,5/
   Integer :: TACCINR(8,9), TESTADR(8,9)
   DATA TACCINR/ 1, 2, 1, 1,10, 1, 3,11,   10,10, 1, 1,10, 1, 3,11,  &
                10,10, 1, 1, 4, 5, 3, 4,   10,10, 1,10, 4, 5, 6, 4,  &
                10,10, 1,10, 4, 5, 6, 4,    1, 7,10,10,10,10, 8,11,  &
                10,10,10,10,10,10, 8,11,   10,10,10,10, 4, 5, 8, 4,  &
                10,10,10,10, 9, 1,10, 9/
   DATA TESTADR/ 2, 2, 6, 4, 0, 1, 3, 0,    0, 0, 6, 4, 0, 2, 3, 0,  &
                 0, 0, 6, 4,10, 9, 3,10,    0, 0, 6, 0,10, 9, 5,10,  &
                 0, 0, 6, 0,10, 9, 5,10,    7, 7, 0, 0, 0, 0, 8, 0,  &
                 0, 0, 0, 0, 0, 0, 8, 0,    0, 0, 0, 0,10, 9, 8,10,  &
                 0, 0, 0, 0,10, 9, 0,10/

   ! if "firstInput" = 'true', input is from unit =INFL
   ! if 'false' input is from unit =INFL2
   Logical :: firstInput = .true.
   Character (LEN=1)  :: BUFFERA(LINE2)

   Private
   Public INFL, INFL2, READA, READI, READR, nowReading, firstInput, ChangeInput
   SAVE

CONTAINS

!-----------------------------------------------------------------------------
SUBROUTINE ChangeInput
! toggles the value of "firstInput"
firstInput = .not. firstInput
END SUBROUTINE ChangeInput

!-----------------------------------------------------------------------------
SUBROUTINE READA (ALFA)
Implicit NONE
Character, INTENT(OUT) :: ALFA*(*)
if(firstInput) then
    Call READA1(ALFA)
else
    Call READA2(ALFA)
endif
END SUBROUTINE READA

!-----------------------------------------------------------------------------
SUBROUTINE READA1 (ALFA)
  Implicit NONE
  Character, INTENT(OUT) :: ALFA*(*)
  Integer :: I, POINT, ESTADO, TIPO, ACCION
  Character (LEN=1) :: CHR, BUF(LINEL)
  SAVE

  BUFFER(LINE2)   = "0"
  BUFFER(LINE2-1) = "1"

  ALFA=" "
  DO  POINT = 1, LINEL
    BUF(POINT)=" "
  END DO
  POINT=0; ESTADO=1
80 CALL GETCH1 (CHR,TIPO)
  IF(TIPO == 8) TIPO=4
  !                               TIPO=1 when CHR= comma
  !                               TIPO=2 when CHR= blank
  !                               TIPO=3 when CHR= any other character
  !                               TIPO=4 when reading after End-Of-Data
  ACCION = TACCINA(TIPO,ESTADO)
  SELECT CASE ( ACCION )
    CASE (    1)
        ESTADO = TESTADA(TIPO,ESTADO)
        GO TO 80
    CASE (    2)
        POINT=POINT+1
        BUF(POINT)=CHR
        ESTADO = TESTADA(TIPO,ESTADO)
        GO TO 80
    CASE (    3)
        WRITE(TOTC,31) BUF
        31 FORMAT(800A1)
        READ (TOTC,32,ERR=50) ALFA
        32 FORMAT(A)
        GO TO 60
    CASE (    4)
        ALFA=" "
        GO TO 60
    CASE (    5)
        GO TO 50
  END SELECT

 50 CONTINUE
    WRITE(*,'("? Alphanumeric data error:")')
    if(len_trim(nowReading) > 1) WRITE(*,'(3x,"while reading: ''",a,"''")') trim(nowReading)
    INQUIRE (UNIT=INFL,NAME=fileName)
    WRITE(*,'(3x,"file: ",a,/3x,"line: ",75A1)') trim(fileName), (BUFFER(I),I=1,75)
    Call ErrStop

 60 CONTINUE
    BUFFER(LINE2-1) = "0"
    RETURN

END SUBROUTINE READA1

!-----------------------------------------------------------------------------
SUBROUTINE READA2 (ALFA)
  Implicit NONE
  Character, INTENT(OUT) :: ALFA*(*)
  Integer :: I, POINT, ESTADO, TIPO, ACCION
  Character (LEN=1) :: CHR, BUF(LINEL)
  SAVE

  BUFFERA(LINE2)   = "0"
  BUFFERA(LINE2-1) = "1"

  ALFA=" "
  DO  POINT = 1, LINEL
    BUF(POINT)=" "
  END DO
  POINT=0; ESTADO=1
80 CALL GETCH2 (CHR,TIPO)
  IF(TIPO == 8) TIPO=4
  !                               TIPO=1 when CHR= comma
  !                               TIPO=2 when CHR= blank
  !                               TIPO=3 when CHR= any other character
  !                               TIPO=4 when reading after End-Of-Data
  ACCION = TACCINA(TIPO,ESTADO)
  SELECT CASE ( ACCION )
    CASE (    1)
        ESTADO = TESTADA(TIPO,ESTADO)
        GO TO 80
    CASE (    2)
        POINT=POINT+1
        BUF(POINT)=CHR
        ESTADO = TESTADA(TIPO,ESTADO)
        GO TO 80
    CASE (    3)
        WRITE(TOTC,31) BUF
        31 FORMAT(800A1)
        READ (TOTC,32,ERR=50) ALFA
        32 FORMAT(A)
        GO TO 60
    CASE (    4)
        ALFA=" "
        GO TO 60
    CASE (    5)
        GO TO 50
  END SELECT

 50 CONTINUE
    WRITE(*,'("? Alphanumeric data error:")')
    if(len_trim(nowReading) > 1) WRITE(*,'(3x,"while reading: ''",a,"''")') trim(nowReading)
    INQUIRE (UNIT=INFL2,NAME=fileName)
    WRITE(*,'(3x,"file: ",a,/3x,"line: ",75A1)') trim(fileName), (BUFFERA(I),I=1,75)
    Call ErrStop

 60 CONTINUE
    BUFFERA(LINE2-1) = "0"
    RETURN

END SUBROUTINE READA2

!-----------------------------------------------------------------------------
SUBROUTINE READI (INTGER)
Implicit NONE
Integer, INTENT(OUT)  :: INTGER
if(firstInput) then
    Call READI1(INTGER)
else
    Call READI2(INTGER)
endif
END SUBROUTINE READI

!-----------------------------------------------------------------------------
SUBROUTINE READI1 (INTGER)
  Implicit NONE
  Integer, INTENT(OUT)  :: INTGER
  Integer, PARAMETER :: MAXI=Huge(1)

  Integer :: I, INTG, ESTADO, SIGNO, TIPO, ACCION
  Character (LEN=1) :: CHR
  Integer :: IJ
  SAVE

  BUFFER(LINE2-1) = "0"
  BUFFER(LINE2)   = "0"
  INTG=0;  ESTADO=1;  SIGNO=1
5 CALL GETCH1 (CHR,TIPO)
  ACCION = TACCINI(TIPO,ESTADO)
  SELECT CASE ( ACCION )
    CASE (    1)
        GO TO 80
    CASE (    2)
        SIGNO=-1
        GO TO 80
    CASE (    3)
        !INTG=INTG*10+CHR
        read(CHR,'(i1)') ij
        INTG=INTG*10+ij
        GO TO 80
    CASE (    4)
        INTG=INTG*SIGNO
        IF(TIPO == 6) GO TO 80
        GO TO 70
    CASE (    5)
        WRITE(*,'(" ? Integer data error:")')
        if(len_trim(nowReading) > 1) WRITE(*,'(3x,"while reading: ''",a,"''")') trim(nowReading)
        INQUIRE (UNIT=INFL,NAME=fileName)
        WRITE(*,'(3x,"file: ",a,/3x,"line: ",75A1)') trim(fileName), (BUFFER(I),I=1,75)
        Call ErrStop
    CASE (    6)
        INTGER=0
        GO TO 70
    CASE (    7)
        GO TO 70
  END SELECT

  !return the maximum integer (with sign) if overflow.
70 INTGER=INT(ISIGN(MAXI,INTG))
  IF(INTG <= MAXI .AND. INTG >= -MAXI) INTGER = INT(INTG)
  RETURN

80 ESTADO=TESTADI(TIPO,ESTADO)
  GO TO 5

END SUBROUTINE READI1

!-----------------------------------------------------------------------------
SUBROUTINE READI2 (INTGER)
  Implicit NONE
  Integer, INTENT(OUT)  :: INTGER
  Integer, PARAMETER :: MAXI=Huge(1)

  Integer :: I, INTG, ESTADO, SIGNO, TIPO, ACCION
  Character (LEN=1) :: CHR
  Integer :: IJ
  SAVE

  BUFFERA(LINE2-1) = "0"
  BUFFERA(LINE2)   = "0"
  INTG=0;  ESTADO=1;  SIGNO=1
5 CALL GETCH2 (CHR,TIPO)
  ACCION = TACCINI(TIPO,ESTADO)
  SELECT CASE ( ACCION )
    CASE (    1)
        GO TO 80
    CASE (    2)
        SIGNO=-1
        GO TO 80
    CASE (    3)
        !INTG=INTG*10+CHR
        read(CHR,'(i1)') ij
        INTG=INTG*10+ij
        GO TO 80
    CASE (    4)
        INTG=INTG*SIGNO
        IF(TIPO == 6) GO TO 80
        GO TO 70
    CASE (    5)
        WRITE(*,'(" ? Integer data error:")')
        if(len_trim(nowReading) > 1) WRITE(*,'(3x,"while reading: ''",a,"''")') trim(nowReading)
        INQUIRE (UNIT=INFL2,NAME=fileName)
        WRITE(*,'(3x,"file: ",a,/3x,"line: ",75A1)') trim(fileName), (BUFFERA(I),I=1,75)
        Call ErrStop
    CASE (    6)
        INTGER=0
        GO TO 70
    CASE (    7)
        GO TO 70
  END SELECT

  !return the maximum integer (with sign) if overflow.
70 INTGER=INT(ISIGN(MAXI,INTG))
  IF(INTG <= MAXI .AND. INTG >= -MAXI) INTGER = INT(INTG)
  RETURN

80 ESTADO=TESTADI(TIPO,ESTADO)
  GO TO 5

END SUBROUTINE READI2

!-----------------------------------------------------------------------------
SUBROUTINE READR (VALUE)
Implicit NONE
Real(KIND(1.D0)), INTENT(OUT)  :: VALUE
if(firstInput) then
    Call READR1(VALUE)
else
    Call READR2(VALUE)
endif
END SUBROUTINE READR


!-----------------------------------------------------------------------------
SUBROUTINE READR1 (VALUE)
  ! read a double precision value
  Implicit NONE
  Real(KIND(1.D0)), INTENT(OUT)  :: VALUE

  Integer :: TIPO, I, EXPON, SIGMNT, SIGEXP, CONDEC, ESTADO, ACCION
  Real(KIND(1.D0)) :: PFREAL, PEREAL
  Character (LEN=1) :: CHR
  Integer :: IJ
  SAVE

  BUFFER(LINE2-1) = "0"
  BUFFER(LINE2)   = "0"
  EXPON=0;  SIGMNT=1;  SIGEXP=1
  VALUE=0.D0;  PEREAL=0.D0;  PFREAL=0.D0
  CONDEC=0;  ESTADO=1
5 CALL GETCH1 (CHR,TIPO)
  ACCION=TACCINR(TIPO,ESTADO)
  SELECT CASE ( ACCION )
    CASE (    1)
        GO TO 120
    CASE (    2)
        SIGMNT=-1
        GO TO 120
    CASE (    3)
        IF(PEREAL >= 1.D15) THEN
            WRITE(*,'("? Real data error: too many digits left of decimal point")')
            GO TO 100
            ENDIF
        read(CHR,'(i1)') IJ
        PEREAL=PEREAL*10.D0+DFLOAT(IJ)
        GO TO 120
    CASE (    4)
        VALUE=(SIGMNT*(PEREAL+PFREAL))*10.D0**(SIGEXP*EXPON)
        RETURN
    CASE (    5)
        VALUE=(SIGMNT*(PEREAL+PFREAL))*10.D0**(SIGEXP*EXPON)
        GO TO 120
    CASE (    6)
        CONDEC=CONDEC+1
        read(CHR,'(i1)') IJ
        PFREAL=PFREAL+DFLOAT(IJ)/10.D0**CONDEC
        GO TO 120
    CASE (    7)
        SIGEXP=-1
        GO TO 120
    CASE (    8)
        read(CHR,'(i1)') ij
        EXPON=EXPON*10+ij
        GO TO 120
    CASE (    9)
        RETURN
    CASE (   10)
        Go To 100
    CASE (   11)
        VALUE=0.D0
        RETURN
  END SELECT

100 CONTINUE
  WRITE(*,'(" ? Real data error:")')
  if(len_trim(nowReading) > 1) WRITE(*,'(3x,"while reading: ''",a,"''")') trim(nowReading)
  INQUIRE (UNIT=INFL,NAME=fileName)
  WRITE(*,'(3x,"file: ",a,/3x,"line: ",75A1)') trim(fileName), (BUFFER(I),I=1,75)
  Call ErrStop

120 ESTADO=TESTADR(TIPO,ESTADO)
  GO TO 5

END SUBROUTINE READR1

!-----------------------------------------------------------------------------
SUBROUTINE READR2 (VALUE)
  ! read a double precision value
  Implicit NONE
  Real(KIND(1.D0)), INTENT(OUT)  :: VALUE

  Integer :: TIPO, I, EXPON, SIGMNT, SIGEXP, CONDEC, ESTADO, ACCION
  Real(KIND(1.D0)) :: PFREAL, PEREAL
  Character (LEN=1) :: CHR
  Integer :: IJ
  SAVE

  BUFFERA(LINE2-1) = "0"
  BUFFERA(LINE2)   = "0"
  EXPON=0;  SIGMNT=1;  SIGEXP=1
  VALUE=0.D0;  PEREAL=0.D0;  PFREAL=0.D0
  CONDEC=0;  ESTADO=1
5 CALL GETCH2 (CHR,TIPO)
  ACCION=TACCINR(TIPO,ESTADO)
  SELECT CASE ( ACCION )
    CASE (    1)
        GO TO 120
    CASE (    2)
        SIGMNT=-1
        GO TO 120
    CASE (    3)
        IF(PEREAL >= 1.D15) THEN
            WRITE(*,'("? Real data error: too many digits left of decimal point")')
            GO TO 100
            ENDIF
        read(CHR,'(i1)') IJ
        PEREAL=PEREAL*10.D0+DFLOAT(IJ)
        GO TO 120
    CASE (    4)
        VALUE=(SIGMNT*(PEREAL+PFREAL))*10.D0**(SIGEXP*EXPON)
        RETURN
    CASE (    5)
        VALUE=(SIGMNT*(PEREAL+PFREAL))*10.D0**(SIGEXP*EXPON)
        GO TO 120
    CASE (    6)
        CONDEC=CONDEC+1
        read(CHR,'(i1)') IJ
        PFREAL=PFREAL+DFLOAT(IJ)/10.D0**CONDEC
        GO TO 120
    CASE (    7)
        SIGEXP=-1
        GO TO 120
    CASE (    8)
        read(CHR,'(i1)') ij
        EXPON=EXPON*10+ij
        GO TO 120
    CASE (    9)
        RETURN
    CASE (   10)
        Go To 100
    CASE (   11)
        VALUE=0.D0
        RETURN
  END SELECT

100 CONTINUE
  WRITE(*,'(" ? Real data error:")')
  if(len_trim(nowReading) > 1) WRITE(*,'(3x,"while reading: ''",a,"''")') trim(nowReading)
  INQUIRE (UNIT=INFL2,NAME=fileName)
  WRITE(*,'(3x,"file: ",a,/3x,"line: ",75A1)') trim(fileName), (BUFFERA(I),I=1,75)
  Call ErrStop

120 ESTADO=TESTADR(TIPO,ESTADO)
  GO TO 5

END SUBROUTINE READR2

!-----------------------------------------------------------------------------
SUBROUTINE GETCH1 (CHR,TIPO)
  ! main subroutine: read next character from the current line (read a new
  !                  line if the last character in the line is reached)
  ! TIPO indicates type of character, as follows:
  !   for character variables (calling subr. READA)
  !        TIPO=1, CHR = comma        TIPO=2, CHR = space
  !        TIPO=3, CHR = any other character
  !
  !   for integer and double precision variables (calling READI or READR)
  !        TIPO= 1 - 6,  CHR = + - E . , space
  !        TIPO=7,  CHR = an integer value (0 - 9)
  Implicit NONE
  Character (LEN=1), INTENT(OUT)  :: CHR
  Integer, INTENT(OUT)  :: TIPO

  Integer :: I,J, LENGTH
  Character (LEN=1) :: NEXT
  Logical :: FIRST, ENDOFF
  Integer :: POINTR
  SAVE
  DATA POINTR /1/

  DATA FIRST/.TRUE./, ENDOFF/.FALSE./, LENGTH/LINE2/

  IF(.NOT.FIRST .AND. POINTR < LINEL .AND. POINTR <= LENGTH) GO TO 55
  FIRST=.FALSE.
  TIPO=8

  ! read a line of text from the input file
  IF(ENDOFF) THEN
      TIPO=8
      CHR=" "
      RETURN
  END IF
  READ(INFL,'(300A1)',ERR=45,END=2) (BUFFER(I),I=1,LINEL)
  GO TO 4

2 ENDOFF=.TRUE.
  INQUIRE (UNIT=INFL,NAME=fileName)
  WRITE(*,'("? Reading after end-of-file",/,2x,"file: ",a)') trim(fileName)
  Call ErrStop

  ! strip-out comments at the end of lines (any character following a " /")
4 DO I=1,LINEL
    IF(BUFFER(I) < " ") BUFFER(I) = " "
    IF(BUFFER(I) /= "/") CYCLE
    IF(I >= 2) THEN
        J = I-1
        IF(BUFFER(J) /= "," .AND. BUFFER(J) /= " ") CYCLE
    END IF
    ! replace "/" with space, and set blank space after the "/"
    DO J=I,LINEL
        BUFFER(J)=" "
    END DO
    EXIT
  END DO
  ! find last non-blank character in the line
45 CONTINUE
  DO I=1,LINEL
    LENGTH=LINEL+1-I
    IF(BUFFER(LENGTH) /= " ") EXIT ! do
  END DO
  ! if last character is not a comma, and the line is not blank,
  ! set a comma at the end of the line
  IF(BUFFER(LENGTH) == ",") GO TO 52
  IF(LENGTH == 1 .AND. BUFFER(LENGTH) == " ") GO TO 52
  LENGTH=LENGTH+1
  BUFFER(LENGTH)=","
52 POINTR=0

  ! read next character

55 POINTR=POINTR+1
  CHR=BUFFER(POINTR)
  IF(BUFFER(LINE2-1) /= "1") GO TO 60

  ! if the variable to read is a character variable
  TIPO=3
  IF(CHR == ",") TIPO=1
  IF(CHR == " ") TIPO=2
  RETURN

  ! if the variable to read is integer or double precision
60 IF(BUFFER(LINE2) == "1") THEN
    ! replace a space with a comma
    ! if next character is (not a space & not a comma)
    NEXT=BUFFER(POINTR+1)
    IF(CHR == " " .and. NEXT /= " " .and. NEXT /= ",")   CHR=","
  ENDIF
  DO TIPO=1,19  ! check that the character is a valid one (it must be in the list CHARAC)
    IF(CHR == CHARAC(TIPO)) GO TO 70
  END DO
  INQUIRE (UNIT=INFL,NAME=fileName)
  WRITE(*,66) CHR, trim(fileName), (BUFFER(I),I=1,75)
  66 FORMAT("? Either end-of-file, or non-valid character: '",A1,"'",/3x, &
               "file: ",A,/3X,"line: ",75A1)
   Call ErrStop

70 CONTINUE
  IF(TIPO == 17 .or. TIPO == 18 .or. TIPO == 19) TIPO=3 ! change D,d or e into "E"
  IF(TIPO <= 6) RETURN
  BUFFER(LINE2)="1"
  !CHR = TIPO-7
  WRITE(CHR,'(i1)') (TIPO-7)
  TIPO = 7
  RETURN

END SUBROUTINE GETCH1

!-----------------------------------------------------------------------------
SUBROUTINE GETCH2 (CHR,TIPO)
  ! main subroutine: read next character from the current line (read a new
  !                  line if the last character in the line is reached)
  ! TIPO indicates type of character, as follows:
  !   for character variables (calling subr. READA)
  !        TIPO=1, CHR = comma        TIPO=2, CHR = space
  !        TIPO=3, CHR = any other character
  !
  !   for integer and double precision variables (calling READI or READR)
  !        TIPO= 1 - 6,  CHR = + - E . , space
  !        TIPO=7,  CHR = an integer value (0 - 9)
  Implicit NONE
  Character (LEN=1), INTENT(OUT)  :: CHR
  Integer, INTENT(OUT)  :: TIPO

  Integer :: I,J, LENGTH
  Character (LEN=1) :: NEXT
  Logical :: FIRST, ENDOFF
  Integer :: POINTR
  SAVE
  DATA POINTR /1/

  DATA FIRST/.TRUE./, ENDOFF/.FALSE./, LENGTH/LINE2/

  IF(.NOT.FIRST .AND. POINTR < LINEL .AND. POINTR <= LENGTH) GO TO 55
  FIRST=.FALSE.
  TIPO=8

  ! read a line of text from the input file
  IF(ENDOFF) THEN
      TIPO=8
      CHR=" "
      RETURN
  END IF
  READ(INFL2,'(300A1)',ERR=45,END=2) (BUFFERA(I),I=1,LINEL)
  GO TO 4

2 ENDOFF=.TRUE.
  INQUIRE (UNIT=INFL2,NAME=fileName)
  WRITE(*,'("? Reading after end-of-file",/,2x,"file: ",a)') trim(fileName)
  Call ErrStop

  ! strip-out comments at the end of lines (any character following a " /")
4 DO I=1,LINEL
    IF(BUFFERA(I) < " ") BUFFERA(I) = " "
    IF(BUFFERA(I) /= "/") CYCLE
    IF(I >= 2) THEN
        J = I-1
        IF(BUFFERA(J) /= "," .AND. BUFFERA(J) /= " ") CYCLE
    END IF
    ! replace "/" with space, and set blank space after the "/"
    DO J=I,LINEL
        BUFFERA(J)=" "
    END DO
    EXIT
  END DO
  ! find last non-blank character in the line
45 CONTINUE
  DO I=1,LINEL
    LENGTH=LINEL+1-I
    IF(BUFFERA(LENGTH) /= " ") EXIT ! do
  END DO
  ! if last character is not a comma, and the line is not blank,
  ! set a comma at the end of the line
  IF(BUFFERA(LENGTH) == ",") GO TO 52
  IF(LENGTH == 1 .AND. BUFFERA(LENGTH) == " ") GO TO 52
  LENGTH=LENGTH+1
  BUFFERA(LENGTH)=","
52 POINTR=0

  ! read next character

55 POINTR=POINTR+1
  CHR=BUFFERA(POINTR)
  IF(BUFFERA(LINE2-1) /= "1") GO TO 60

  ! if the variable to read is a character variable
  TIPO=3
  IF(CHR == ",") TIPO=1
  IF(CHR == " ") TIPO=2
  RETURN

  ! if the variable to read is integer or double precision
60 IF(BUFFERA(LINE2) == "1") THEN
    ! replace a space with a comma
    ! if next character is (not a space & not a comma)
    NEXT=BUFFERA(POINTR+1)
    IF(CHR == " " .and. NEXT /= " " .and. NEXT /= ",")   CHR=","
  ENDIF
  DO TIPO=1,19  ! check that the character is a valid one (it must be in the list CHARAC)
    IF(CHR == CHARAC(TIPO)) GO TO 70
  END DO
  INQUIRE (UNIT=INFL2,NAME=fileName)
  WRITE(*,66) CHR, trim(fileName), (BUFFERA(I),I=1,75)
  66 FORMAT("? Either end-of-file, or non-valid character: '",A1,"'",/3x, &
               "file: ",A,/3X,"line: ",75A1)
   Call ErrStop

70 CONTINUE
  IF(TIPO == 17 .or. TIPO == 18 .or. TIPO == 19) TIPO=3 ! change D,d or e into "E"
  IF(TIPO <= 6) RETURN
  BUFFERA(LINE2)="1"
  !CHR = TIPO-7
  WRITE(CHR,'(i1)') (TIPO-7)
  TIPO = 7
  RETURN

END SUBROUTINE GETCH2

END MODULE READIR
