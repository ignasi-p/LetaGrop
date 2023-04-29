MODULE LetaGropModule_4
USE DIMS_Module, ONLY : MXK
!Integer, PARAMETER :: MXK = 40
USE LetaGropModule, ONLY : dp
Implicit NONE
  Real(dp) :: FAK,STEP,STEP0,TOLA,TOLB,TOLC,TOLL,TOLY,X1,X2,Y0,Y1, &
              LNBA(MXK),LNBA1(MXK),FAKI(5),STEGI(5),TOLYI(5),X1I(5),Y2, &
              X2I(5),Y1I(5),Y2I(5)
  Integer :: II,KARL,NVAR,VARV,KAL(5)
  Logical :: AVAR,BVAR,NYTT

SAVE
END MODULE LetaGropModule_4
