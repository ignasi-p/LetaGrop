MODULE LetaGropModule
USE DIMS_Module
!Integer, PARAMETER :: MXK = 40, MXAP = 10000, MXS = 25, MXKS = 13, MXAK = 12, MXAS = 12
Implicit NONE
Integer, PARAMETER :: dp = kind(0.d0) ! double precision
REAL(dp) :: DARR1,DARR2,DET,SIGFAK,SIG2Y,Rf, &
    STEGBY,STEKFA,TOLU,U,U1,U2,UC,UMIN,UNO,W,W1,X,Y, &
    AG(10),AK(MXK,MXAK),AP(MXAP),AS(MXS,MXAS),DARK(MXK),DARK2(MXK), &
    DARKS(MXS,MXKS),DARKS2(MXS,MXKS),K(MXK),KBOM(MXK),KC(MXK),KMIN(MXK),POT(MXK), &
    KS(MXS,MXKS),KSMIN(MXS,MXKS),KV(MXK),PINNE(MXK),RUTA(MXK,MXK),S(MXK,MXK), &
    SH(MXK,MXK),SK(MXK,MXK),START(5),STEK(MXK),TOL(5),V(MXK),VBOM(MXK)
Real(dp), PARAMETER :: ln10 = log(10.d0), logE = 1.d0/ln10
!     ROF=0.086167D0
Real(dp), PARAMETER :: ROF=1000.D0 * 8.314510D0 / 96485.309D0 ! Change by I.Puigdomenech 2000-Dec-18
!Real(dp), PARAMETER :: ROF=1000.d0 * 8.314462618153d0 / 96485.33212d0 ! Change by I.Puigdomenech 2021

! the output file unit
Integer, PARAMETER :: UT = 26

Integer :: RURIK,TYP,ORVAR,ARUM,CELL,I,IK,J,JK,M,N,NAK,NAP,NAPA, &
    NAS,NGE,NK,NKOM,NKS,NOK,NOTVAR,NPUNKT,NS,NSKOTT,NSKYTT,NTOT,NX,RI, &
    RIDO,RJ,RP,RS,RS1,RS2,RSKOTT,RSKYTT,SKRIUT,SLUSK,STYR,VAL,VMAX, &
    APCELL(MXS),IDO(5),IVAR(MXK),IVARGE(MXK),NIKS(MXS),NP(MXS), &
    VAKS(MXS,5),INDIC,NIDO,ORVAKO,NUVAR,NuvarMAX
! Change in Aug.2021
! The selected sets are now not controlled by the interval RS1 to RS2,
! (that were given through rurik 11) but instead the user may now
! select a number RSN of random sets through rurik 21.
! The RSN selected sets are stored in RSI(), in any order.
! The variable RSL is used to loop through the selected RSN sets,
! for example in routines Sark and Ubbe.
Integer :: RSI(MXS),RSN,RSL ! added Aug.2021 by I.Puigdomenech

Logical :: BRA,FAS2,KASSA,KASDAR,KLAR,KOKS,MINK,POSKIS,PROV, &
    RAKT,TAGE,KASS(MXK),POSK(MXK),VAR(MXK)

Logical :: lgDbg = .false.

! - Program type: 0= stadard, 1= SPEFO, 2= ModFnk (added in 2020)
Integer :: PrgTyp

SAVE

END MODULE LetaGropModule