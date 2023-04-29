!------------------------------------------------------------------
program main
Implicit NONE
Real*8 :: Hadd,Oxadd,Solub,K(30),ak(30,8), ag(2)
Integer :: i
Integer, PARAMETER :: IDEBG = 11, NK = 10, UT = 6
Real*8 :: X(6)
Real*8 :: eps(20)
Real*8, parameter :: ln10=log(10.d0)
Logical :: firstTime = .true.
Data eps/20*0.d0/, X/6*0.d0/
! components:   H+, Na+, ClO4-, NO3-, H2ox, M(ox)2.6H2O(s)
! complexes:
! Hox-,ox-2,Np+4,NpOH+3,NpNO3+3,Np(ox)+2,Np(ox)2,Np(ox)3-2
                          !  E+nn  H+  Na+ ClO4- NO3- H2ox, M(ox)2.6H2O(s), Z(i)
       data (ak(1,i),i=1,8)/   0.d0,   -1.d0, 0,   0,   0,    1.d0,   0.d0,   -1/  !1 Hox-
       data (ak(2,i),i=1,8)/  -6.d0,   -2.d0, 0,   0,   0,    1.d0,   0.d0,   -2/  !2 ox-2
       data (ak(3,i),i=1,8)/ -14.d0,    4.d0, 0,   0,   0,   -2.d0,   1.d0,   +4/  !3 Pu+4
       data (ak(4,i),i=1,8)/   0.d0,    3.d0, 0,   0,   0,   -2.d0,   1.d0,   +3/  !4 PuOH+3
       data (ak(5,i),i=1,8)/   0.d0,    2.d0, 0,   0,   0,   -2.d0,   1.d0,   +2/  !5 Pu(OH)2+2
       data (ak(6,i),i=1,8)/   0.d0,    4.d0, 0,   0,   1,   -2.d0,   1.d0,   +3/  !6 PuNO3+3 /
       data (ak(7,i),i=1,8)/  -9.d0,    2.d0, 0,   0,   0,   -1.d0,   1.d0,   +2/  !7 Pu(ox)+2
       data (ak(8,i),i=1,8)/  -5.d0,    0.d0, 0,   0,   0,    0.d0,   1.d0,    0/  !8 Pu(ox)2
       data (ak(9,i),i=1,8)/  -5.d0,   -2.d0, 0,   0,   0,    1.d0,   1.d0,   -2/  !9 Pu(ox)3-2
       data (ak(10,i),i=1,8)/ -99.d0,  -4.d0, 0,   0,   0,    2.d0,   1.d0,   -4/  !10 Pu(ox)4-4

       data (K(i),i=1,10)  /  0.0398d0,  2.24d0,  1.d0  ,  3.981d0,  3.981d0, &
                            89.13d0,  7.9d0,  2.6d0,  5.4d0,  8.3d0 /
        data (ag(i),i=1,2) /1,1/
        write(*,*) 'Enter [HNO3]tot'
        read(*,*) Hadd
        write(*,*) 'Enter [H2ox]tot'
        read(*,*) Oxadd
        X(1) = Hadd; X(4) =  Hadd
        X(5) = Oxadd
        X(2)=0.D0; X(3)=0.D0
        call M_ox2 (Solub,X)
        write (*,'(a,1p,g10.4)') ' calc.solub.=',Solub

CONTAINS

!----------------------------------------------------------------------------
SUBROUTINE ErrStop
    Implicit NONE
    Integer :: ierr, i
    WRITE(*, '("Press Enter to continue ...")', Advance='No')
    READ(*,'(I5)',iostat=ierr) i
    STOP 1
END SUBROUTINE ErrStop

!-----------------------------------------------------------------------------
SUBROUTINE M_ox2 (Solubility,X)
! calculates the solubility of M(ox)2.6H2O when
!               [HX]_added, [HNO3]_added,
!               [NaX]_added and [H2ox]_added are known
!  (X = ClO4)
!
! components:   H+, Na+, ClO4-, NO3-, H2ox, M(ox)2.6H2O(s)
! complexes:
! Hox-,ox-2,Np+4,NpOH+3,Np(OH)2+2,NpNO3+3,Np(ox)+2,Np(ox)2,Np(ox)3-2
USE CHEM
USE HALTAF_Module
USE FACTOR_Module, ONLY : activityCoeffsModel, ionicStr, temperature, pressure, factorPrint
USE SIT
Implicit NONE
Real(dp), INTENT(IN) :: X(:)
Real(dp), INTENT(OUT) :: Solubility

Real(dp) :: aH2O, w
Integer :: i,j,li,ix, Na2, zCat, zAn
SAVE

!-------------------
    IF(firstTime) THEN
        ! debug:
        DBG=nint(AG(2))
        !DBG=1      ! report errors only
        if(dbg > 0) open(unit=IDEBG,file='sit_dbg.out',status='unknown')

        ! Na=number components
        Na=6
        Na2=Na+2

        ! Nx = complex number
        Nx=0
        ! the electric charge is given as ak(i,Na+2)
        ! if ak(i,Na+2)=99, then K(i) is the epsilon
        ! for the preceding complex with the background electrolyte
        Do  i=1,NK
            if(nint(AK(i,Na2)) /= 99) Nx = Nx+1
        End Do
        nIon = NA + Nx

        MSOL = 0 ! no solids
        ! MS = total nr of species (components (soluble or solid) + soluble complexes + solid reaction products)
        MS = NA + NX + MSOL
        ! allocate memory arrays
        Call CHEM_MEM_ALLOC
        Call HALTAF_MEM_ALLOC
        DO I=1,MS
            NOLL(I)=.FALSE.
        EndDo

        ix = 0
        Do  i=1,NK
            ! the electric charge is given as ak(i,Na+2)
            ! if ak(i,Na+2)=99, then K(i) is the epsilon
            ! for the preceding complex with the background electrolyte
            if(nint(AK(i,Na2)) == 99) Cycle
            ix = ix+1
            ! get the stoichiometry; 1st index is complex nbr,  2nd index is component
            Do li=1,Na
                a(ix,li) = ak(i,li+1)
            End Do
            if(dbg > 0) write(IDEBG,'(a,i2,a,20F6.2)',Advance='No')' a(',ix,',*)=',(a(ix,li),li=1,Na)
            ! get electric charge of complexes
            j = Na + ix
            Z(j) = nint(AK(i,Na2))
            if(dbg > 0) write(IDEBG,'(a,i2,a,i3)') ' Z(',j,')=',z(j)
        End Do
        Z(nIon+1)=1
        Z(nIon+2)=-1
        ! electric charges for components: H+, Na+, ClO4-, NO3-, H2ox, M(ox)2.6H2O(s)
        Z(1:NA)= (/ +1, +1, -1, -1, 0, 0 /)
        IDENTC(1)="H+"; IDENTC(2)="Na+"; IDENTC(3)="ClO4-"; IDENTC(4)="NO3-"; IDENTC(5)="H2ox"; IDENTC(6)="M(ox)2.6H2O(s)"
        IDENT(1:NA) = IDENTC(1:NA)
        !Concentrations that are given:
        !     KH=1: total concentrations given
        !     KH=2: log activity given
        ! components:   H+, Na+, ClO4-, NO3-, H2ox, M(ox)2.6H2O(s)
        KH(1:NA)= (/ 1, 1, 1, 1, 1, 2 /)
        ! do not consider the formation of M(ox)2.6H2O(s) in aqueous solution
        Do i=1,NA+Nx
            Noll(i)=.false.
        End Do
        Noll(6)=.true.
        JWATER = -1 ! no H2O
        TOL=1.D-8  ! tolerance when solving mass-balance equation
        tolLogF = 1E-6 ! tolerance when iterating activity coefficients
        IOUT = 0   ! debug printout to the terminal
        !IOUT = UT  ! debug printout to the output file
        ! Activity coefficients:
        ! -1 = ideal solutions
        !  0 = Davies Eqn.
        !  1 = SIT (specific ion interaction model)
        activityCoeffsModel = nint(AG(1)); ionicStr = -1.d0
        Temperature = 25.d0; Pressure = 1.d0
        if(activityCoeffsModel == 1) then !SIT
            call SIT_MEM_ALLOC (nIon)
            !call setDefaultSITvalues (UT)
            ! Set default values for the specific interaction coefficient.
            DO i=1,nIon+2
                IF(i<=nIon) THEN
                    IF(NOLL(i) .or. Z(i) == 0) Then
                        Do j=1,nIon+2
                            call setEps(i,j,0.d0)
                        EndDo
                    Cycle
                    End If
                ENDIF
                DO j=1,nIon+2
                    !call setEps(i,j,0.d0)
                    if(Z(i)*Z(j) >= 0) Cycle
                    if(Z(i) >0) then
                        zCat = Z(i)-1
                        zAn  = Z(j)+1
                    else
                        zAn  = Z(i)+1
                        zCat = Z(j)-1
                    endif
                    call setEps(i,j, 0.150001d0 + 0.15d0 * real((zAn+zCat),dp))
                END DO
            END DO
            call setEps(nIon+1,nIon+2,0.03d0)
            ! species:  H+, Na+, ClO4-, NO3-, H2ox, M(ox)2.6H2O(s),
            !           1    2     3     4     5     6
            ! Hox-,ox-2,Np+4,NpOH+3,Np(OH)2+2,NpNO3+3,Np(ox)+2,Np(ox)2,Np(ox)3-2,Np(ox)4-4
            !  7    8    9     10     11       12       13      14      15        16
            IDENT(Na+1)="Hox-";    IDENT(Na+2)="ox-2";     IDENT(Na+3)="M+4";
            IDENT(Na+4)="MOH+3";   IDENT(Na+5)="M(OH)2+2"; IDENT(Na+6)="MNO3+3";
            IDENT(Na+7)="M(ox)+2"; IDENT(Na+8)="M(ox)2";   IDENT(Na+9)="M(ox)3-2";
            IDENT(Na+10)="M(ox)4-4";
            !--- Np(ox)3-2,Np(ox)4-4
            !call setEps(1,15,-0.50d0)
            !call setEps(1,16,-0.50d0)
            !call setEps(2,15,-0.20d0)
            !call setEps(2,16,-0.20d0)
            !--- H+: ClO4-, NO3-
            call setEps(1,3, 0.14d0)
            call setEps(1,4, 0.07d0)
            !--- Na+: ClO4-, NO3-
            call setEps(2,3,-0.08d0)
            call setEps(2,4,-0.06d0)
            !--- Hox-: H+
            call setEps(1,7, 0.15d0)
            !--- Hox-: Na+/NH4+
            call setEps(2,7,-0.07d0)
            !-- ox-2: Na+/NH4+
            call setEps(2,8,-0.08d0)
            !--- Np+4: ClO4-, NO3-
            call setEps(3,9, 0.84d0)
            call setEps(4,9, 0.84d0)
            !--- NpOH+3: ClO4-, NO3-
            call setEps(3,10, 0.50d0)
            call setEps(4,10, 0.50d0)
            !--- Np(OH)2+2: ClO4-, NO3-
            call setEps(3,11, 0.30d0)
            call setEps(4,11, 0.30d0)
            !--- NpNO3+3: ClO4-, NO3-
            call setEps(3,12, 0.70d0)
            call setEps(4,12, 0.70d0)

            if(dbg > 0) then
                call factorPrint (IDEBG)
                call printEps (IDEBG)
            endif

        endif ! activityCoeffsModel = 1 (SIT)

    ENDIF ! firstTime

    ! ix = complex number
    ix=0
    Do  i=1,NK
        if(nint(AK(i,Na2)) == 99) then
            ! the electric charge is given as ak(i,Na+2)
            ! if ak(i,Na+2)=99, then K(i) is the epsilon
            ! for the preceding complex with the background electrolyte
            if(ix >= 1 .and. abs(K(i)).ge.1.d-24 .and. activityCoeffsModel == 1) then
                j=ix+Na
                if(Z(j) > 0) then
                    call setEps(j,4,K(i)) ! eps(j,NO3-)
                    call setEps(j,3,K(i)) ! eps(j,ClO4-)
                else if(Z(j) < 0) then
                    call setEps(j,1,K(i)) ! eps(j,H+)
                    call setEps(j,2,K(i)) ! eps(j,Na+)
                endif
            endif
            Cycle
        endif
        ix = ix+1
        lBeta(ix)=-99999.D0; if(K(i) > 0.D0) lBeta(ix)= log(K(i))/ln10 + AK(i,1)
        if(dbg > 0 .and. firstTime) write(IDEBG,'(a,i2,a,f15.10)')" logK(",ix,")=",lBeta(ix)
    End Do
    If(ix /= Nx) Then
        Write(UT,100) Nx,ix
        Write(*,100) Nx,ix
        100 FORMAT("Error: number of complexes has changed. Nx = ",I0,", now found ",I0)
        call ErrStop
    EndIf
    ! for MNO3+3, MOH+3, and M(OH)2+2  the constants correspond to:
    !         M+4 + NO3- = MNO3+3    &   Np+4 + H2O =NpOH+3 + H+
    !                                &   Np+4 + 2 H2O =Np(OH)2+2 + 2H+
    ! change to: M(ox)2(s) + NO3- + 4 H+ = MNO3+3 + 2H2ox    &
    !               M(ox)2(s) + H2O + 3 H+ = MOH+3 + 2H2ox, etc
    lBeta(4) = lBeta(4) + lBeta(3)
    lBeta(5) = lBeta(5) + lBeta(3)
    lBeta(6) = lBeta(6) + lBeta(3)
    ! make corrections for the activity of water
    aH2O = 0.964!X(6);
    Do i=1,Nx
        if(nint(AG(1)) /= 0 .and. a(i,Na) > 0d0 .and. aH2O > 0.d0) lBeta(i) = lBeta(i)-6*log(aH2O)/ln10
        if(dbg > 0 .and. firstTime) write(IDEBG,'(a,i2,a,f15.10)')" logK(",i,")=",lBeta(i)
    End Do

    if(dbg > 0 .and. firstTime .and. activityCoeffsModel == 1) then !SIT
        write(IDEBG,*)'species:  H+, Na+, ClO4-, NO3-, H2ox, M(ox)2.6H2O(s),'
        write(IDEBG,*)'          1    2     3     4      5      6'
        write(IDEBG,*)'Hox-,ox-2,M+4,MOH+3,M(OH)2+2,MNO3+3,M(ox)+2,M(ox)2,M(ox)3-2,'
        write(IDEBG,*)' 7    8    9   10     11       12     13      14     15'
        write(IDEBG,'(1x,a,i3)') 'Nx=',Nx
        write(IDEBG,'(1x,a,25i3)') 'z=',(z(i),i=1,Na)
        write(IDEBG,'(1x,25i3)') (z(i),i=Na+1,Na+Nx)
        Do i=1,Na+Nx+2
            if(i.le.Na+Nx) then
                write(IDEBG,'(a,i2)') ' i=',i
            else
                if(i.eq.Na+Nx+1) write(IDEBG,*) 'for electrical imbalance: Na+'
                if(i.eq.Na+Nx+2) write(IDEBG,*) 'for electrical imbalance: Cl-'
            endif
            do j=1,Na+Nx+2
                w = getEps(i,j)
                if(abs(w)>1.d-10 .and. w < 1000.d0) write(IDEBG,'(a,i2,2i3,f10.6)') ' j,z(i),z(j),eps =',j,z(i),z(j),w
            end do
        End Do
    endif

    ! total concentrations for the components
    !Components:   H+, Na+, ClO4-, NO3-, H2ox, M(ox)2.6H2O(s)
    TOT(1)= X(1);  if(tot(1) > 1.E-10) LOGA(1) = log(Tot(1))/ln10  ! H+
    TOT(2)= X(2);  if(tot(2) > 1.E-10) LOGA(2) = log(Tot(2))/ln10  ! Na+
    TOT(3)= X(3);  if(tot(3) > 1.E-10) LOGA(3) = log(Tot(3))/ln10  ! X-
    TOT(4)= X(4);  if(tot(4) > 1.E-10) LOGA(4) = log(Tot(4))/ln10  ! NO3-
    TOT(5)= X(5);  if(tot(5) > 1.E-10) LOGA(5) = log(Tot(5))/ln10  ! H2ox
    LOGA(6) = 0.d0 ! M(ox)2.6H2O(s)

    if(dbg > 0 .and. firstTime) Call printChemSystem (IDEBG)

    CONT=.FALSE.
    !---------------------
    Call HaltaCalc
    !---------------------

    IF(errFlags /= 0) THEN !------ Error Reporting --------------
        WRITE(*,991) TOT(1),trim(errFlagsToString()), trim(errFlagsGetMessages())
        991 FORMAT("Equilibrium calculations in HaltaFall failed for Htot=",1PE13.5,1x,A,/,A)
        Solubility = -1.D0
    ELSE
        Solubility = TOT(6)
        if(dbg > 0 .and. firstTime) call printConcs (IDEBG)
    ENDIF

    if(dbg > 0 .and. firstTime) close(UNIT=IDEBG)
    firstTime = .false.
    RETURN

END SUBROUTINE M_ox2

END PROGRAM MAIN
