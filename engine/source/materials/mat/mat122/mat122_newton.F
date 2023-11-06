Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
Chd|====================================================================
Chd|  MAT122_NEWTON                 source/materials/mat/mat122/mat122_newton.F
Chd|-- called by -----------
Chd|        SIGEPS122                     source/materials/mat/mat122/sigeps122.F
Chd|-- calls ---------------
Chd|        VINTER                        source/tools/curve/vinter.F
Chd|====================================================================
      SUBROUTINE MAT122_NEWTON(
     1     NEL     ,NUPARAM ,NUVAR   ,UPARAM  ,UVAR    ,RHO0    ,
     2     EPSXX   ,EPSYY   ,EPSZZ   ,PLA     ,DPLA    ,
     3     DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     4     SIGOXX  ,SIGOYY  ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX  ,
     5     SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     6     OFF     ,SIGY    ,ET      ,DMG     ,SEQ     ,EPSD    ,
     7     SOUNDSP ,NFUNC   ,IFUNC   ,NPF     ,TF      ,NVARTMP ,
     8     VARTMP  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C O M M O N
C-----------------------------------------------
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: NEL,NUPARAM,NUVAR,
     .     NFUNC,IFUNC(NFUNC),NPF(SNPC),NVARTMP
        my_real, INTENT(IN) ::
     .     UPARAM(NUPARAM),TF(STF)
        my_real,DIMENSION(NEL), INTENT(IN)     ::
     .     RHO0,EPSXX,EPSYY,EPSZZ,
     .     DEPSXX,DEPSYY,DEPSZZ,DEPSXY,DEPSYZ,DEPSZX,
     .     SIGOXX,SIGOYY,SIGOZZ,SIGOXY,SIGOYZ,SIGOZX
        my_real ,DIMENSION(NEL), INTENT(OUT)   ::
     .     SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX,
     .     SOUNDSP,SIGY,ET
        my_real ,DIMENSION(NEL), INTENT(INOUT)       ::
     .     PLA,DPLA,EPSD,OFF,DMG,SEQ
        my_real ,DIMENSION(NEL,NUVAR), INTENT(INOUT) ::
     .     UVAR
        INTEGER, DIMENSION(NEL,NVARTMP), INTENT(INOUT) ::
     .     VARTMP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER
     .     I,II,J,NITER,ITER,NINDX,INDEX(NEL),
     .     ISH,ITR,IRES,IBUCK,ICOMP,LTYPE11,LTYPE12,
     .     LTYPER0,IPOS(NEL),ILEN(NEL),IAD(NEL)
        my_real
     .     E10,E20,E30,NU12,NU21,NU23,NU32,NU13,NU31,
     .     G120,G230,G310,E1C,GAMMA,SIGY0,BETA,M,A,EFTI0,
     .     EFTU0,DFTU,EFCI0,EFCU0,DFCU,DSAT1,Y00,YC0,B,
     .     DMAX,YR,YSP,DSAT2,Y0P0,YCP0,DSAT2C,Y0PC0,YCPC0,
     .     EPSD11,D11,N11,D11U,N11U,EPSD12,D22,N22,D12,
     .     N12,EPSDR0,DR0,NR0
        my_real
     .     DDEP,DFDSIG2,DLAM,NORMYY,NORMZZ,NORMXY,NORMYZ,
     .     NORMZX,SIG_DFDSIG,H(NEL),DPYY(NEL),DPZZ(NEL),
     .     DPXY(NEL),DPYZ(NEL),DPZX(NEL),PHI(NEL),DPLA_DLAM(NEL),
     .     DPHI_DLAM(NEL),DYDX(NEL),S11(NEL),S12(NEL),S13(NEL),
     .     S22(NEL),S23(NEL),S33(NEL),C11,C12,C13,C22,C23,C33,
     .     DETC,DFT(NEL),DFC(NEL),E1(NEL),E2(NEL),EPSF_EQ,ZD(NEL),
     .     ZDP(NEL),Y(NEL),YP(NEL),D(NEL),DP(NEL),DF(NEL),
     .     G12(NEL),EFTI(NEL),EFTU(NEL),EFCI(NEL),EFCU(NEL),
     .     F11(NEL),F22(NEL),F12(NEL),F11R(NEL),FR0(NEL),E3(NEL),
     .     Y0(NEL),YC(NEL),Y0P(NEL),YCP(NEL),Y0PC(NEL),YCPC(NEL),
     .     DPY(NEL),DPZ(NEL),G23(NEL),G31(NEL),VAR(NEL)
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
C
        ! Elastic parameters
        E10     = UPARAM(1)
        E20     = UPARAM(2)
        E30     = UPARAM(3)
        NU12    = UPARAM(4)
        NU21    = UPARAM(5)
        NU13    = UPARAM(6)
        NU31    = UPARAM(7)
        NU23    = UPARAM(8)
        NU32    = UPARAM(9)
        G120    = UPARAM(10)
        G230    = UPARAM(11)
        G310    = UPARAM(12)
        E1C     = UPARAM(13)
        GAMMA   = UPARAM(14)
        ISH     = NINT(UPARAM(15))
        ITR     = NINT(UPARAM(16))
        IRES    = NINT(UPARAM(17))
        SIGY0   = UPARAM(18)
        BETA    = UPARAM(19)
        M       = UPARAM(20)
        A       = UPARAM(21)
        EFTI0   = UPARAM(22)
        EFTU0   = UPARAM(23)
        DFTU    = UPARAM(24)
        EFCI0   = UPARAM(25)
        EFCU0   = UPARAM(26)
        DFCU    = UPARAM(27)
        IBUCK   = NINT(UPARAM(28))
        DSAT1   = UPARAM(29)
        Y00     = UPARAM(30)
        YC0     = UPARAM(31)
        B       = UPARAM(32)
        DMAX    = UPARAM(33)
        YR      = UPARAM(34)
        YSP     = UPARAM(35)
        DSAT2   = UPARAM(36)
        Y0P0    = UPARAM(37)
        YCP0    = UPARAM(38)
        DSAT2C  = UPARAM(39)
        Y0PC0   = UPARAM(40)
        YCPC0   = UPARAM(41)
        EPSD11  = UPARAM(42)
        D11     = UPARAM(43)
        N11     = UPARAM(44)
        D11U    = UPARAM(45)
        N11U    = UPARAM(46)
        EPSD12  = UPARAM(47)
        D22     = UPARAM(48)
        N22     = UPARAM(49)
        D12     = UPARAM(50)
        N12     = UPARAM(51)
        EPSDR0  = UPARAM(52)
        DR0     = UPARAM(53)
        NR0     = UPARAM(54)
        LTYPE11 = NINT(UPARAM(55))
        LTYPE12 = NINT(UPARAM(56))
        LTYPER0 = NINT(UPARAM(57))
C
        ! Recovering internal variables
        DO I=1,NEL
          ! Checking deletion flag value
          IF (OFF(I) < ONE)  OFF(I) = FOUR_OVER_5*OFF(I)
          IF (OFF(I) < EM01) OFF(I) = ZERO
          ! Hourglass coefficient
          ET(I)   = ONE
          H(I)    = ZERO
          ! Plastic strain increment
          DPLA(I) = ZERO
          DPYY(I) = ZERO
          DPZZ(I) = ZERO
          DPXY(I) = ZERO
          DPYZ(I) = ZERO
          DPZX(I) = ZERO
          ! Damage variables
          DF(I)   = UVAR(I,1)
          D(I)    = UVAR(I,2)
          DP(I)   = UVAR(I,3)
          Y(I)    = UVAR(I,4)
          YP(I)   = UVAR(I,5)
          DFT(I)  = UVAR(I,6)
          DFC(I)  = UVAR(I,7)
          DPY(I)  = UVAR(I,19)
          DPZ(I)  = UVAR(I,20)
        ENDDO
C
        ! Compute strain rate dependency factor
        DO I = 1,NEL
          ! Rate dependency in fiber direction 1 for Young modulus
          IF (LTYPE11 == 1) THEN
            F11(I) = D11*(ABS(EPSD(I))/EPSD11)**N11
          ELSEIF (LTYPE11 == 2) THEN
            F11(I) = D11*(ABS(EPSD(I))/EPSD11) + N11
          ELSEIF (LTYPE11 == 3) THEN
            F11(I) = D11*LOG(MAX(ABS(EPSD(I))/EPSD11,ONE)) + LOG(N11)
          ELSEIF (LTYPE11 == 4) THEN
            F11(I) = D11*TANH(N11*(MAX(ABS(EPSD(I))-EPSD11,ZERO)))
          ENDIF
          ! Rate dependency in matrix transverse direction 2 for Young modulus
          IF (LTYPE12 == 1) THEN
            F22(I) = D22*(ABS(EPSD(I))/EPSD12)**N22
          ELSEIF (LTYPE12 == 2) THEN
            F22(I) = D22*(ABS(EPSD(I))/EPSD12) + N22
          ELSEIF (LTYPE12 == 3) THEN
            F22(I) = D22*LOG(MAX(ABS(EPSD(I))/EPSD12,ONE)) + LOG(N22)
          ELSEIF (LTYPE12 == 4) THEN
            F22(I) = D22*TANH(N22*(MAX(ABS(EPSD(I))-EPSD12,ZERO)))
          ENDIF
          ! Rate dependency for shear plane 12 modulus
          IF (LTYPE12 == 1) THEN
            F12(I) = D12*(ABS(EPSD(I))/EPSD12)**N12
          ELSEIF (LTYPE12 == 2) THEN
            F12(I) = D12*(ABS(EPSD(I))/EPSD12) + N12
          ELSEIF (LTYPE12 == 3) THEN
            F12(I) = D12*LOG(MAX(ABS(EPSD(I))/EPSD12,ONE)) + LOG(N12)
          ELSEIF (LTYPE12 == 4) THEN
            F12(I) = D12*TANH(N12*(MAX(ABS(EPSD(I))-EPSD12,ZERO)))
          ENDIF
          ! Rate dependency in fiber direction 1 for Young modulus
          IF (LTYPE11 == 1) THEN
            F11R(I) = D11U*(ABS(EPSD(I))/EPSD11)**N11U
          ELSEIF (LTYPE11 == 2) THEN
            F11R(I) = D11U*(ABS(EPSD(I))/EPSD11) + N11U
          ELSEIF (LTYPE11 == 3) THEN
            F11R(I) = D11U*LOG(MAX(ABS(EPSD(I))/EPSD11,ONE)) + LOG(N11U)
          ELSEIF (LTYPE11 == 4) THEN
            F11R(I) = D11U*TANH(N11U*(MAX(ABS(EPSD(I))-EPSD11,ZERO)))
          ENDIF
          ! Rate dependency in fiber direction 1 for Young modulus
          IF (LTYPER0 == 1) THEN
            FR0(I) = DR0*(ABS(EPSD(I))/EPSDR0)**NR0
          ELSEIF (LTYPER0 == 2) THEN
            FR0(I) = DR0*(ABS(EPSD(I))/EPSDR0) + NR0
          ELSEIF (LTYPER0 == 3) THEN
            FR0(I) = DR0*LOG(MAX(ABS(EPSD(I))/EPSDR0,ONE)) + LOG(NR0)
          ELSEIF (LTYPER0 == 4) THEN
            FR0(I) = DR0*TANH(NR0*(MAX(ABS(EPSD(I))-EPSDR0,ZERO)))
          ENDIF
        ENDDO
c
        ! Elastic parameters, yield stress and strain rate dependency
        DO I = 1,NEL
          ! Fiber (direction 1)
          ! -> Tension
          IF (EPSXX(I) >= ZERO) THEN
            E1(I) = E10
            ! -> Compression
          ELSE
            E1(I) = E1C/(ONE + GAMMA*ABS(EPSXX(I)))
          ENDIF
          E1(I)  = E1(I)*(ONE + F11(I))
          ! Matrix (direction 2)
          E2(I)  = E20*(ONE + F22(I))
          ! Matrix (direction 3)
          E3(I)  = E30*(ONE + F22(I))
          ! Shear moduli
          G12(I) = G120*(ONE + F12(I))
          G23(I) = G230*(ONE + F12(I))
          G31(I) = G310*(ONE + F12(I))
          ! Compliance matrix for 3D
          C11  =   ONE/E1(I)
          C22  =   ONE/E2(I)
          C33  =   ONE/E3(I)
          C12  = -NU12/E1(I)
          C13  = -NU31/E3(I)
          C23  = -NU23/E2(I)
          DETC =  C11*C22*C33-C11*C23*C23-C12*C12*C33+C12*C13*C23
     .          + C13*C12*C23-C13*C22*C13
          ! Stiffness matrix for 3D
          S11(I) =  (C22*C33-C23*C23)/DETC
          S12(I) = -(C12*C33-C13*C23)/DETC
          S13(I) =  (C12*C23-C13*C22)/DETC
          S22(I) =  (C11*C33-C13*C13)/DETC
          S23(I) = -(C11*C23-C13*C12)/DETC
          S33(I) =  (C11*C22-C12*C12)/DETC
          ! Yield stress
          SIGY(I) = SIGY0*(ONE + FR0(I)) + BETA*EXP(M*LOG(PLA(I)+EM20))
          ! Rate dependency on fiber failure
          EFTI(I) = UVAR(I,8)
          IF (UVAR(I,8) == ZERO)  EFTI(I) = EFTI0*(ONE + F11R(I))
          EFTU(I) = UVAR(I,9)
          IF (UVAR(I,9) == ZERO)  EFTU(I) = EFTU0*(ONE + F11R(I))
          EFCI(I) = UVAR(I,10)
          IF (UVAR(I,10) == ZERO) EFCI(I) = EFCI0*(ONE + F11R(I))
          EFCU(I) = UVAR(I,11)
          IF (UVAR(I,11) == ZERO) EFCU(I) = EFCU0*(ONE + F11R(I))
          ! Rate dependency on matrix failure
          Y0(I) = UVAR(I,12)
          IF (UVAR(I,12) == ZERO) Y0(I)  = Y00*SQRT(ONE + F12(I))
          YC(I) = UVAR(I,13)
          IF (UVAR(I,13) == ZERO) YC(I)  = YC0*SQRT(ONE + F12(I))
          Y0P(I) = UVAR(I,14)
          IF (UVAR(I,14) == ZERO) Y0P(I) = Y0P0*SQRT(ONE + F22(I))
          YCP(I) = UVAR(I,15)
          IF (UVAR(I,15) == ZERO) YCP(I) = YCP0*SQRT(ONE + F22(I))
          Y0PC(I) = UVAR(I,16)
          IF (UVAR(I,16) == ZERO) Y0PC(I) = Y0PC0*SQRT(ONE + F22(I))
          YCPC(I) = UVAR(I,17)
          IF (UVAR(I,17) == ZERO) YCPC(I) = YCPC0*SQRT(ONE + F22(I))
        ENDDO
c
        !========================================================================
        ! - COMPUTATION OF TRIAL VALUES
        !========================================================================
        DO I=1,NEL
c
          ! Computation of the trial stress tensor
          SIGNXX(I) = SIGOXX(I)/MAX((ONE-DF(I)),EM20)
     .                   + S11(I)*DEPSXX(I) + S12(I)*DEPSYY(I) + S13(I)*DEPSZZ(I)
          SIGNYY(I) = SIGOYY(I)/MAX((ONE-DPY(I)),EM20)
     .                   + S12(I)*DEPSXX(I) + S22(I)*DEPSYY(I) + S23(I)*DEPSZZ(I)
          SIGNZZ(I) = SIGOZZ(I)/MAX((ONE-DPZ(I)),EM20)
     .                   + S13(I)*DEPSXX(I) + S23(I)*DEPSYY(I) + S33(I)*DEPSZZ(I)
          SIGNXY(I) = SIGOXY(I)/MAX((ONE- D(I)),EM20) + G12(I)*DEPSXY(I)
          SIGNYZ(I) = SIGOYZ(I)/MAX((ONE- D(I)),EM20) + G23(I)*DEPSYZ(I)
          SIGNZX(I) = SIGOZX(I)/MAX((ONE- D(I)),EM20) + G31(I)*DEPSZX(I)
C
          ! Equivalent stress
          SEQ(I) = SQRT((SIGNXY(I))**2 + (SIGNYZ(I))**2 + (SIGNZX(I))**2 +
     .                A*(SIGNYY(I))**2 + A*(SIGNZZ(I))**2)
C
        ENDDO
c
        !========================================================================
        ! - COMPUTATION OF YIELD FONCTION
        !========================================================================
        PHI(1:NEL) = SEQ(1:NEL) - SIGY(1:NEL)
        ! Checking plastic behavior for all elements
        NINDX = 0
        INDEX(1:NEL) = 0
        DO I=1,NEL
          IF ((PHI(I)>ZERO).AND.(OFF(I) == ONE)) THEN
            NINDX = NINDX+1
            INDEX(NINDX) = I
          ENDIF
        ENDDO
c
        !====================================================================
        ! - PLASTIC CORRECTION WITH CUTTING PLANE ALGORITHM (NEWTON ITERATION)
        !====================================================================
c
        ! Number of maximum Newton iterations
        NITER = 3
c
        ! Loop over the iterations
        DO ITER = 1, NITER
#include "vectorize.inc"
          ! Loop over yielding elements
          DO II=1,NINDX
            I = INDEX(II)
c
            ! Note     : in this part, the purpose is to compute for each iteration
            ! a plastic multiplier allowing to update internal variables to satisfy
            ! the consistency condition using the cutting plane semi-implicit method
            ! Its expression at each iteration is : DLAMBDA = - PHI/DPHI_DLAMBDA
            ! -> PHI          : current value of yield function (known)
            ! -> DPHI_DLAMBDA : derivative of PHI with respect to DLAMBDA by taking
            !                   into account of internal variables kinetic (to compute)
c
            ! 1 - Computation of DPHI_DSIG the normal to the yield surface
            !-------------------------------------------------------------
            NORMYY = A*SIGNYY(I)/MAX(SEQ(I),EM20)
            NORMZZ = A*SIGNZZ(I)/MAX(SEQ(I),EM20)
            NORMXY =   SIGNXY(I)/MAX(SEQ(I),EM20)
            NORMYZ =   SIGNYZ(I)/MAX(SEQ(I),EM20)
            NORMZX =   SIGNZX(I)/MAX(SEQ(I),EM20)
c
            ! 2 - Computation of DPHI_DLAMBDA
            !---------------------------------------------------------
c
            !   a) Derivative with respect stress increments tensor DSIG
            !   --------------------------------------------------------
            DFDSIG2 = NORMYY*(S22(I)*NORMYY + S23(I)*NORMZZ)
     .              + NORMZZ*(S23(I)*NORMYY + S33(I)*NORMZZ)
     .              + NORMXY*NORMXY*G12(I)
     .              + NORMYZ*NORMYZ*G23(I)
     .              + NORMZX*NORMZX*G31(I)
c
            !   b) Derivatives with respect to plastic strain P
            !   ------------------------------------------------
c
            !     i) Derivative of the yield stress with respect to plastic strain dSIGY / dPLA
            !     ----------------------------------------------------------------------------
            H(I) = BETA*M*EXP((M-1)*LOG(PLA(I)+EM20))
            H(I) = MIN(H(I),MAX(TWO*G120,E20))
c
            !     ii) Derivative of dPLA with respect to DLAM
            !     -------------------------------------------
            SIG_DFDSIG = SIGNYY(I) * NORMYY
     .                 + SIGNZZ(I) * NORMZZ
     .                 + SIGNXY(I) * NORMXY
     .                 + SIGNYZ(I) * NORMYZ
     .                 + SIGNZX(I) * NORMZX
            DPLA_DLAM(I) = SIG_DFDSIG/MAX(SIGY(I),EM20)
c
            ! 3 - Computation of plastic multiplier and variables update
            !----------------------------------------------------------
c
            ! Derivative of PHI with respect to DLAM
            DPHI_DLAM(I) = - DFDSIG2 - H(I)*DPLA_DLAM(I)
            DPHI_DLAM(I) = SIGN(MAX(ABS(DPHI_DLAM(I)),EM20),DPHI_DLAM(I))
c
            ! Computation of the plastic multiplier
            DLAM = -PHI(I)/DPHI_DLAM(I)
c
            ! Plastic strains tensor increment
            DPYY(I) = DLAM*NORMYY
            DPZZ(I) = DLAM*NORMZZ
            DPXY(I) = DLAM*NORMXY
            DPYZ(I) = DLAM*NORMYZ
            DPZX(I) = DLAM*NORMZX
c
            ! Elasto-plastic stresses update
            SIGNXX(I) = SIGNXX(I) - (S12(I)*DPYY(I) + S13(I)*DPZZ(I))
            SIGNYY(I) = SIGNYY(I) - (S22(I)*DPYY(I) + S23(I)*DPZZ(I))
            SIGNZZ(I) = SIGNZZ(I) - (S23(I)*DPYY(I) + S33(I)*DPZZ(I))
            SIGNXY(I) = SIGNXY(I) - DPXY(I)*G12(I)
            SIGNYZ(I) = SIGNYZ(I) - DPYZ(I)*G23(I)
            SIGNZX(I) = SIGNZX(I) - DPZX(I)*G31(I)
c
            ! Cumulated plastic strain and strain rate update
            DDEP    = DLAM*DPLA_DLAM(I)
            DPLA(I) = MAX(ZERO, DPLA(I) + DDEP)
            PLA(I)  = PLA(I) + DDEP
c
            ! Update equivalent stress
            SEQ(I) = SQRT((SIGNXY(I))**2 + (SIGNYZ(I))**2 + (SIGNZX(I))**2 +
     .                  A*(SIGNYY(I))**2 + A*(SIGNZZ(I))**2)
c
            ! Update the hardening yield stress
            SIGY(I) = SIGY(I) + H(I)*DLAM*DPLA_DLAM(I)
c
            ! Update yield function value
            PHI(I)  = SEQ(I) - SIGY(I)
c
          ENDDO
          ! End of the loop over the yielding elements
c
        ENDDO
        ! End of the loop over the iterations
        !===================================================================
        ! - END OF PLASTIC CORRECTION WITH CUTTING PLANE ALGORITHM
        !===================================================================
c
        !===================================================================
        ! - DAMAGE VARIABLES COMPUTATION AND UPDATE STRESS TENSOR
        !===================================================================
        DO I = 1,NEL
c
          ! Fiber damage (direction 1)
          ! ------------------------------------------
          ! -> Fiber equivalent strain
          EPSF_EQ = (ONE - NU23*NU32)*EPSXX(I) + (NU23*NU31 + NU21)*EPSYY(I) +
     .              (NU21*NU32 + NU31)*EPSZZ(I)
          ! -> Tension
          IF (EPSF_EQ >= ZERO) THEN
            IF (EPSF_EQ < EFTI(I)) THEN
              DFT(I) = MAX(ZERO,DFT(I))
            ELSEIF ((EPSF_EQ >= EFTI(I)).AND.(EPSF_EQ < EFTU(I))) THEN
              DFT(I) = MAX(DFTU*((EPSF_EQ-EFTI(I))/(EFTU(I)-EFTI(I))),DFT(I))
              ! Save damage threshold in case of strain rate dependency
              IF (UVAR(I,8) == ZERO) UVAR(I,8) = EFTI(I)
              IF (UVAR(I,9) == ZERO) UVAR(I,9) = EFTU(I)
            ELSEIF (EPSF_EQ >= EFTU(I)) THEN
              DFT(I) = MAX(ONE - (ONE - DFTU)*(EFTU(I)/EPSF_EQ),DFT(I))
            ENDIF
            DFT(I) = MAX(DFT(I),ZERO)
            DFT(I) = MIN(DFT(I),ONE)
            DF(I)  = DFT(I)
            ! -> Compression
          ELSEIF ((EPSF_EQ < ZERO).AND.(IBUCK > 1)) THEN
            IF (ABS(EPSF_EQ) < EFCI(I)) THEN
              DFC(I) = MAX(ZERO,DFC(I))
            ELSEIF ((ABS(EPSF_EQ) >= EFCI(I)).AND.(ABS(EPSF_EQ) < EFCU(I))) THEN
              DFC(I) = MAX(DFCU*((ABS(EPSF_EQ)-EFCI(I))/(EFCU(I)-EFCI(I))),DFC(I))
              ! Save damage threshold in case of strain rate dependency
              IF (UVAR(I,10) == ZERO) UVAR(I,10) = EFCI(I)
              IF (UVAR(I,11) == ZERO) UVAR(I,11) = EFCU(I)
            ELSEIF (ABS(EPSF_EQ) >= EFCU(I)) THEN
              DFC(I) = MAX(ONE - (ONE - DFCU)*(EFCU(I)/ABS(EPSF_EQ)),DFC(I))
            ENDIF
            DFC(I) = MAX(DFC(I),ZERO)
            DFC(I) = MIN(DFC(I),ONE)
            DF(I)  = DFC(I)
          ENDIF
c
          ! Matrix damage (direction 2 and 3)
          ! ------------------------------------------
          ! -> Damage functions (derivatives of elastic energy)
          ZD(I)  = HALF*((SIGNXY(I)**2/G120) + (SIGNYZ(I)**2/G230) + (SIGNZX(I)**2/G310))
          ZDP(I) = HALF*((((MAX(SIGNYY(I),ZERO))**2)/E20) + (((MAX(SIGNZZ(I),ZERO))**2)/E30))
          ! -> Damage evolution functions
          Y(I)  = MAX(Y(I) ,SQRT(ZD(I)+B*ZDP(I)))
          YP(I) = MAX(YP(I),SQRT(ZDP(I)))
        ENDDO
c
        ! -> Shear damage
        !    Linear
        IF (ISH == 1) THEN
          DO I = 1,NEL
            IF (Y(I)<Y0(I)) THEN
              D(I) = ZERO
            ELSEIF ((D(I)<DMAX).AND.(Y(I)<YSP).AND.(Y(I)<YR)) THEN
              D(I) = MAX(Y(I)-Y0(I),ZERO)/MAX(YC(I),EM20)
              D(I) = MIN(D(I),DMAX)
              ! Save damage threshold in case of strain rate dependency
              IF (UVAR(I,12) == ZERO) UVAR(I,12) = Y0(I)
              IF (UVAR(I,13) == ZERO) UVAR(I,13) = YC(I)
            ELSE
              D(I) = ONE - (ONE - DMAX)*UVAR(I,4)/MAX(Y(I),EM20)
            ENDIF
            D(I) = MAX(D(I),ZERO)
            D(I) = MIN(D(I), ONE)
          ENDDO
          !    Exponential
        ELSEIF (ISH == 2) THEN
          DO I = 1,NEL
            IF (Y(I)>Y0(I)) THEN
              D(I) = DSAT1*(ONE - EXP((Y0(I)-Y(I))/MAX(YC(I),EM20)))
              ! Save damage threshold in case of strain rate dependency
              IF (UVAR(I,12) == ZERO) UVAR(I,12) = Y0(I)
              IF (UVAR(I,13) == ZERO) UVAR(I,13) = YC(I)
            ELSE
              D(I) = ZERO
            ENDIF
            D(I) = MAX(D(I),ZERO)
            D(I) = MIN(D(I), ONE)
          ENDDO
          !    Tabulated function
        ELSEIF (ISH == 3) THEN
          IPOS(1:NEL) = VARTMP(1:NEL,1)
          IAD (1:NEL) = NPF(IFUNC(1)) / 2 + 1
          ILEN(1:NEL) = NPF(IFUNC(1)+1) / 2 - IAD(1:NEL) - IPOS(1:NEL)
          DO I = 1,NEL
            VAR(I) = Y(I)/Y0(I)
          ENDDO
          CALL VINTER(TF,IAD,IPOS,ILEN,NEL,VAR,DYDX,D)
          VARTMP(1:NEL,1) = IPOS(1:NEL)
          DO I = 1,NEL
            ! Save damage threshold in case of strain rate dependency
            IF (UVAR(I,12) == ZERO .AND. D(I) /= ZERO) UVAR(I,12) = Y0(I)
            D(I) = MAX(D(I),ZERO)
            D(I) = MIN(D(I), ONE)
          ENDDO
        ENDIF
c
        ! -> Transverse damage
        !    Linear
        IF (ITR == 1) THEN
          DO I = 1,NEL
            IF (YP(I)<Y0P(I)) THEN
              DP(I) = ZERO
            ELSEIF ((DP(I)<DMAX).AND.(YP(I)<YSP).AND.(YP(I)<YR)) THEN
              DP(I) = MAX(YP(I)-Y0P(I),ZERO)/MAX(YCP(I),EM20)
              DP(I) = MIN(DP(I),DMAX)
              ! Save damage threshold in case of strain rate dependency
              IF (UVAR(I,14) == ZERO) UVAR(I,14) = Y0P(I)
              IF (UVAR(I,15) == ZERO) UVAR(I,15) = YCP(I)
            ELSE
              DP(I) = ONE - (ONE - DMAX)*UVAR(I,5)/MAX(YP(I),EM20)
            ENDIF
            DP(I) = MAX(DP(I),ZERO)
            DP(I) = MIN(DP(I), ONE)
          ENDDO
          !    Exponential
        ELSEIF (ITR == 2) THEN
          DO I = 1,NEL
            IF (YP(I)>Y0P(I)) THEN
              DP(I) = DSAT2*(ONE - EXP((Y0P(I)-YP(I))/MAX(YCP(I),EM20)))
              ! Save damage threshold in case of strain rate dependency
              IF (UVAR(I,14) == ZERO) UVAR(I,14) = Y0P(I)
              IF (UVAR(I,15) == ZERO) UVAR(I,15) = YCP(I)
            ELSE
              DP(I) = ZERO
            ENDIF
            DP(I) = MAX(DP(I),ZERO)
            DP(I) = MIN(DP(I), ONE)
          ENDDO
          !    Tabulated function
        ELSEIF (ITR == 3) THEN
          IPOS(1:NEL) = VARTMP(1:NEL,2)
          IAD (1:NEL) = NPF(IFUNC(2)) / 2 + 1
          ILEN(1:NEL) = NPF(IFUNC(2)+1) / 2 - IAD(1:NEL) - IPOS(1:NEL)
          DO I = 1,NEL
            VAR(I) = YP(I)/Y0P(I)
          ENDDO
          CALL VINTER(TF,IAD,IPOS,ILEN,NEL,VAR,DYDX,DP)
          VARTMP(1:NEL,2) = IPOS(1:NEL)
          DO I = 1,NEL
            ! Save damage threshold in case of strain rate dependency
            IF (UVAR(I,14) == ZERO .AND. DP(I) /= ZERO) UVAR(I,14) = Y0P(I)
            DP(I) = MAX(DP(I),ZERO)
            DP(I) = MIN(DP(I), ONE)
          ENDDO
        ENDIF
c
        DO I = 1,NEL
          ! Computation of effective DP for each transverse directions
          IF (EPSYY(I) >= ZERO) THEN
            DPY(I) = DP(I)
          ELSE
            DPY(I) = ZERO
          ENDIF
          IF (EPSZZ(I) >= ZERO) THEN
            DPZ(I) = DP(I)
          ELSE
            DPZ(I) = ZERO
          ENDIF
c
          ! Update stresses with damage softening effect
          ! --------------------------------------------
          SIGNXX(I) = SIGNXX(I)*(ONE - DF(I))
          SIGNYY(I) = SIGNYY(I)*(ONE - DPY(I))
          SIGNZZ(I) = SIGNZZ(I)*(ONE - DPZ(I))
          SIGNXY(I) = SIGNXY(I)*(ONE -  D(I))
          SIGNYZ(I) = SIGNYZ(I)*(ONE -  D(I))
          SIGNZX(I) = SIGNZX(I)*(ONE -  D(I))
c
          ! -> Store internal variables
          ! ------------------------------------------
          UVAR(I,1)  = DF(I)
          UVAR(I,2)  = D(I)
          UVAR(I,3)  = DP(I)
          UVAR(I,4)  = Y(I)
          UVAR(I,5)  = YP(I)
          UVAR(I,6)  = DFT(I)
          UVAR(I,7)  = DFC(I)
          UVAR(I,19) = DPY(I)
          UVAR(I,20) = DPZ(I)
          DMG(I) = MAX(DF(I),D(I),DP(I))
c
        ENDDO
        !===================================================================
c
        ! Sound-speed and thickness update
        DO I=1,NEL
          ! Sound-speed
          SOUNDSP(I) = SQRT(MAX(S11(I),S22(I),S33(I),
     .                      TWO*G12(I),TWO*G23(I),TWO*G31(I))/RHO0(I))
          ! Coefficient for hourglass
          IF (DPLA(I)>ZERO) THEN
            ET(I) = H(I) / (H(I) + MAX(E1(I),E2(I),E3(I)))
          ELSE
            ET(I) = ONE
          ENDIF
        ENDDO
C
      END
C
