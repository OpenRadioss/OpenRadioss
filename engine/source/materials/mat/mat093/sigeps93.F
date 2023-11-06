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
C Orthotropic Hill
Chd|====================================================================
Chd|  SIGEPS93                      source/materials/mat/mat093/sigeps93.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        MSTRAIN_RATE                  source/materials/mat_share/mstrain_rate.F
Chd|        VINTER                        source/tools/curve/vinter.F
Chd|====================================================================
      SUBROUTINE SIGEPS93(
     1     NEL      ,NUPARAM  ,NUVAR    ,MFUNC    ,KFUNC    ,
     2     NPF      ,TF       ,TIME     ,TIMESTEP ,UPARAM   ,
     3     EPSPXX   ,EPSPYY   ,EPSPZZ   ,EPSPXY   ,EPSPYZ   ,EPSPZX   ,
     4     DEPSXX   ,DEPSYY   ,DEPSZZ   ,DEPSXY   ,DEPSYZ   ,DEPSZX   ,
     5     SIGOXX   ,SIGOYY   ,SIGOZZ   ,SIGOXY   ,SIGOYZ   ,SIGOZX   ,
     6     SIGNXX   ,SIGNYY   ,SIGNZZ   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     7     SOUNDSP  ,PLA      ,UVAR     ,RHO0     ,OFF      ,
     8     ET       ,YLD      ,SEQ      ,EPSP     ,ASRATE   ,
     9     NVARTMP  ,VARTMP   ,DPLA     )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
        INTEGER NEL,NUPARAM,NUVAR,NVARTMP
        my_real
     .        TIME,TIMESTEP,UPARAM(NUPARAM),
     .        RHO0(NEL),PLA(NEL),
     .        EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL),
     .        EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .        DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .        DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .        SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .        SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .        SEQ(NEL),EPSP(NEL),ASRATE

        INTEGER, INTENT(INOUT) :: VARTMP(NEL,NVARTMP)
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
        my_real
     .        SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .        SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .        SOUNDSP(NEL),ET(NEL),DPLA(NEL)
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
        my_real
     .        UVAR(NEL,NUVAR),OFF(NEL),YLD(NEL)
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION
C----------------------------------------------------------------
        INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
        my_real
     .          TF(*)
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
        INTEGER  I,II,J,NITER,ITER,NINDX,INDEX(NEL),
     .           J1,J2,ITAB,JJ(NEL),VP,IDEV,ISRATE,
     .           IAD1(NEL),IPOS1(NEL),ILEN1(NEL),
     .           IAD2(NEL),IPOS2(NEL),ILEN2(NEL)
        my_real
     .   D11,D12,D13,D22,D23,D33,G12,G23,G13,E11,E22,E33,
     .   FF,GG,HH,LL,MM,NN,SIGY,QR1,QR2,CR1,CR2,FAC,
     .   NORMXX,NORMYY,NORMXY,NORMZZ,NORMZX,NORMYZ,
     .   DLAM,DDEP,SIG_DFDSIG,DFDSIG2,DPDT
        my_real
     .   SIGHL(NEL),H(NEL),Y1(NEL),Y2(NEL),
     .   DPXX(NEL),DPYY(NEL),DPZZ(NEL),DPXY(NEL),
     .   DPZX(NEL),DPYZ(NEL),DPLA_DLAM(NEL),PHI(NEL),
     .   DYDX1(NEL),DYDX2(NEL),DPHI_DLAM(NEL)
        my_real,
     .     DIMENSION(:), ALLOCATABLE :: RATE, YFAC
C----------------------------------------------------------------
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
C
        ! Elastic parameters
        D11  = UPARAM(4)  ! 1st Diag. component of plane stress elastic matrix
        D12  = UPARAM(5)  ! 2nd Diag. component of plane stress elastic matrix
        D13  = UPARAM(6)  ! Non Diag. component of plane stress elastic matrix
        D22  = UPARAM(7)  ! Component 13 of the compliance matrix
        D23  = UPARAM(8)  ! Component 23 of the compliance matrix
        D33  = UPARAM(9)  ! Component 33 of the compliance matrix
        G12  = UPARAM(10) ! Shear modulus in 12
        G13  = UPARAM(11) ! Shear modulus in 13
        G23  = UPARAM(12) ! Shear modulus in 23
        E11  = UPARAM(13) ! Young modulus in 11
        E22  = UPARAM(14) ! Young modulus in 22
        E33  = UPARAM(15) ! Young modulus in 33
        ! Hill yield criterion coefficient
        FF   = UPARAM(19) ! First  Hill coefficient
        GG   = UPARAM(20) ! Second Hill coefficient
        HH   = UPARAM(21) ! Third  Hill coefficient
        LL   = UPARAM(22) ! Fourth Hill coefficient
        MM   = UPARAM(23) ! Fifth  Hill coefficient
        NN   = UPARAM(24) ! Sixth  Hill coefficient
        ! Continuous hardening yield stress
        SIGY = UPARAM(25) ! Initial yield stress
        QR1  = UPARAM(26) ! Voce 1 first parameter
        CR1  = UPARAM(27) ! Voce 1 second parameter
        QR2  = UPARAM(28) ! Voce 2 first parameter
        CR2  = UPARAM(29) ! Voce 2 second parameter
        ! Strain-rate computation flag
        VP   = NINT(UPARAM(30))
        ! Tabulated hardening yield stress
        ITAB = 0
        IF (MFUNC > 0) THEN
          ITAB = 1
          ALLOCATE(RATE(MFUNC),YFAC(MFUNC))
          DO I = 1,MFUNC
            RATE(I) = UPARAM(30+I)
            YFAC(I) = UPARAM(30+MFUNC+I)
          ENDDO
        ENDIF
C
        ! Total or deviatoric strain-rate computation
        IF ((VP == 2) .OR. (VP == 3)) THEN
          IDEV   = VP-2
          ISRATE = 1
          CALL MSTRAIN_RATE(NEL    ,ISRATE ,ASRATE ,EPSP   ,IDEV   ,
     .                      EPSPXX ,EPSPYY ,EPSPZZ ,EPSPXY ,EPSPYZ ,
     .                      EPSPZX )
        ENDIF
C
        ! Recovering internal variables
        DO I=1,NEL
          ! Checking deletion flag value
          IF (OFF(I) < ONE)   OFF(I) = FOUR_OVER_5*OFF(I)
          IF (OFF(I) < EM01) OFF(I) = ZERO
          ! Hourglass coefficient
          ET(I) = ONE
          H(I)  = ZERO
          ! Plastic strain increment
          DPLA(I) = ZERO
          DPXX(I) = ZERO
          DPYY(I) = ZERO
          DPZZ(I) = ZERO
          DPXY(I) = ZERO
          DPYZ(I) = ZERO
          DPZX(I) = ZERO
          ! Computing strain-rate (only if more than 1 strain-rate function is indicated in the input deck)
          IF ((ITAB == 1).AND.(MFUNC > 1)) THEN
            ! Plastic strain-rate
            IF (VP == 1) EPSP(I)   = UVAR(I,1)
            ! Looking for corresponding rate in the tables
            JJ(I) = 1
            DO J = 2,MFUNC-1
              IF (EPSP(I) >= RATE(J)) JJ(I) = J
            ENDDO
          ENDIF
        ENDDO
C
        ! Computing yield stress
        !  -> Continuous yield stress
        IF (ITAB == 0) THEN
          DO I = 1,NEL
            YLD(I) = SIGY
     .             + QR1*(ONE - EXP(-CR1*PLA(I)))
     .             + QR2*(ONE - EXP(-CR2*PLA(I)))
            H(I) = QR1*CR1*EXP(-CR1*PLA(I)) + QR2*CR2*EXP(-CR2*PLA(I))
          ENDDO
        ELSE
          !  -> Tabulated yield stress
          ! ->  No strain-rate effect
          IF (MFUNC == 1) THEN
            ! Recovering position tables
            IPOS1(1:NEL) = VARTMP(1:NEL,1)
            IAD1 (1:NEL) = NPF(KFUNC(1)) / 2 + 1
            ILEN1(1:NEL) = NPF(KFUNC(1)+1) / 2 - IAD1(1:NEL) - IPOS1(1:NEL)
            ! Computing the plastic strain interpolation
            CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,PLA,DYDX1,Y1)
            ! Storing the position
            VARTMP(1:NEL,1) = IPOS1(1:NEL)
            ! Computing the yield stress and its derivative
            YLD(1:NEL) = YFAC(1)*Y1(1:NEL)
            H(1:NEL)   = YFAC(1)*DYDX1(1:NEL)
            ! ->  Strain-rate effect
          ELSE
            DO I=1,NEL
              J1 = JJ(I)
              J2 = J1 + 1
              ! Recovering position tables for the first rate
              IPOS1(I) = VARTMP(I,J1)
              IAD1 (I) = NPF(KFUNC(J1)) / 2 + 1
              ILEN1(I) = NPF(KFUNC(J1)+1) / 2 - IAD1(I) - IPOS1(I)
              ! Recovering position tables for the second rate
              IPOS2(I) = VARTMP(I,J2)
              IAD2 (I) = NPF(KFUNC(J2)) / 2 + 1
              ILEN2(I) = NPF(KFUNC(J2)+1) / 2 - IAD2(I) - IPOS2(I)
            ENDDO
            ! Computing the plastic strain interpolation
            CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,PLA,DYDX1,Y1)
            CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL,PLA,DYDX2,Y2)
            ! Storing new positions and computing yield stress
            DO I=1,NEL
              J1 = JJ(I)
              J2 = J1+1
              ! Hardening yield stress
              Y1(I)  = Y1(I)*YFAC(J1)
              Y2(I)  = Y2(I)*YFAC(J2)
              FAC    = (EPSP(I) - RATE(J1))/(RATE(J2) - RATE(J1))
              YLD(I) = Y1(I) + FAC*(Y2(I)-Y1(I))
              YLD(I) = MAX(YLD(I),EM20)
              ! Derivatives of yield stress
              DYDX1(I) = DYDX1(I)*YFAC(J1)
              DYDX2(I) = DYDX2(I)*YFAC(J2)
              H(I)     = DYDX1(I)+FAC*(DYDX2(I)-DYDX1(I))
              ! New positions
              VARTMP(I,J1) = IPOS1(I)
              VARTMP(I,J2) = IPOS2(I)
            ENDDO
          ENDIF
        ENDIF
c
        !========================================================================
        ! - COMPUTATION OF TRIAL VALUES
        !========================================================================
        DO I=1,NEL
c
          ! Computation of the trial stress tensor
          SIGNXX(I) = SIGOXX(I) + D11*DEPSXX(I) + D12*DEPSYY(I) + D13*DEPSZZ(I)
          SIGNYY(I) = SIGOYY(I) + D12*DEPSXX(I) + D22*DEPSYY(I) + D23*DEPSZZ(I)
          SIGNZZ(I) = SIGOZZ(I) + D13*DEPSXX(I) + D23*DEPSYY(I) + D33*DEPSZZ(I)
          SIGNXY(I) = SIGOXY(I) + G12*DEPSXY(I)
          SIGNYZ(I) = SIGOYZ(I) + G23*DEPSYZ(I)
          SIGNZX(I) = SIGOZX(I) + G13*DEPSZX(I)
C
          ! Hill equivalent stress
          SIGHL(I) = FF*(SIGNYY(I) - SIGNZZ(I))**2 + GG*(SIGNZZ(I) - SIGNXX(I))**2 +
     .               HH*(SIGNXX(I) - SIGNYY(I))**2 + TWO*LL*SIGNYZ(I)**2 +
     .               TWO*MM*SIGNZX(I)**2 + TWO*NN*SIGNXY(I)**2
          SIGHL(I) = SQRT(MAX(ZERO,SIGHL(I)))
        ENDDO
c
        !========================================================================
        ! - COMPUTATION OF YIELD FONCTION
        !========================================================================
        PHI(1:NEL) = SIGHL(1:NEL) - YLD(1:NEL)
        ! Checking plastic behavior for all elements
        NINDX = 0
        INDEX = 0
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
            ! the consistency condition using the backward Euler implicit method
            ! with a Newton-Raphson iterative procedure
            ! Its expression at each iteration is : DLAMBDA = - PHI/DPHI_DLAMBDA
            ! -> PHI          : current value of yield function (known)
            ! -> DPHI_DLAMBDA : derivative of PHI with respect to DLAMBDA by taking
            !                   into account of internal variables kinetic :
            !                   plasticity, temperature and damage (to compute)
c
            ! 1 - Computation of DPHI_DSIG the normal to the yield surface
            !-------------------------------------------------------------
            NORMXX = (GG*(SIGNXX(I)-SIGNZZ(I)) + HH*(SIGNXX(I)-SIGNYY(I)))/(MAX(SIGHL(I),EM20))
            NORMYY = (FF*(SIGNYY(I)-SIGNZZ(I)) + HH*(SIGNYY(I)-SIGNXX(I)))/(MAX(SIGHL(I),EM20))
            NORMZZ = (FF*(SIGNZZ(I)-SIGNYY(I)) + GG*(SIGNZZ(I)-SIGNXX(I)))/(MAX(SIGHL(I),EM20))
            NORMXY = TWO*NN*SIGNXY(I)/(MAX(SIGHL(I),EM20))
            NORMYZ = TWO*LL*SIGNYZ(I)/(MAX(SIGHL(I),EM20))
            NORMZX = TWO*MM*SIGNZX(I)/(MAX(SIGHL(I),EM20))
c
            ! 2 - Computation of DPHI_DLAMBDA
            !---------------------------------------------------------
c
            !   a) Derivative with respect stress increments tensor DSIG
            !   --------------------------------------------------------
            DFDSIG2 = NORMXX * (D11 * NORMXX + D12 * NORMYY + D13 * NORMZZ )
     .              + NORMYY * (D12 * NORMXX + D22 * NORMYY + D23 * NORMZZ )
     .              + NORMZZ * (D13 * NORMXX + D23 * NORMYY + D33 * NORMZZ )
     .              + NORMXY * NORMXY * G12
     .              + NORMYZ * NORMYZ * G23
     .              + NORMZX * NORMZX * G13
c
            !   b) Derivatives with respect to plastic strain P
            !   ------------------------------------------------
c
            !     i) Derivative of the yield stress with respect to plastic strain dYLD / dPLA
            !     ----------------------------------------------------------------------------
            ! Already computed
c
            !     ii) Derivative of dPLA with respect to DLAM
            !     -------------------------------------------
            SIG_DFDSIG = SIGNXX(I) * NORMXX
     .                 + SIGNYY(I) * NORMYY
     .                 + SIGNZZ(I) * NORMZZ
     .                 + SIGNXY(I) * NORMXY
     .                 + SIGNYZ(I) * NORMYZ
     .                 + SIGNZX(I) * NORMZX
            DPLA_DLAM(I) = SIG_DFDSIG / MAX(YLD(I),EM20)
c
            ! 3 - Computation of plastic multiplier and variables update
            !----------------------------------------------------------
c
            ! Derivative of PHI with respect to DLAM
            DPHI_DLAM(I) = - DFDSIG2 - H(I)*DPLA_DLAM(I)
            DPHI_DLAM(I) = SIGN(MAX(ABS(DPHI_DLAM(I)),EM20) ,DPHI_DLAM(I))
c
            ! Computation of the plastic multiplier
            DLAM = -PHI(I)/DPHI_DLAM(I)
c
            ! Plastic strains tensor update
            DPXX(I) = DLAM * NORMXX
            DPYY(I) = DLAM * NORMYY
            DPZZ(I) = DLAM * NORMZZ
            DPXY(I) = DLAM * NORMXY
            DPYZ(I) = DLAM * NORMYZ
            DPZX(I) = DLAM * NORMZX
c
            ! Elasto-plastic stresses update
            SIGNXX(I) = SIGNXX(I) - (D11*DPXX(I) + D12*DPYY(I) + D13*DPZZ(I))
            SIGNYY(I) = SIGNYY(I) - (D12*DPXX(I) + D22*DPYY(I) + D23*DPZZ(I))
            SIGNZZ(I) = SIGNZZ(I) - (D13*DPXX(I) + D23*DPYY(I) + D33*DPZZ(I))
            SIGNXY(I) = SIGNXY(I) - DPXY(I)*G12
            SIGNYZ(I) = SIGNYZ(I) - DPYZ(I)*G23
            SIGNZX(I) = SIGNZX(I) - DPZX(I)*G13
c
            ! Cumulated plastic strain and strain rate update
            DDEP    = DLAM*DPLA_DLAM(I)
            DPLA(I) = MAX(ZERO, DPLA(I) + DDEP)
            PLA(I)  = PLA(I) + DDEP
c
            ! Update Hill equivalent stress
            SIGHL(I) = FF*(SIGNYY(I) - SIGNZZ(I))**2 + GG*(SIGNZZ(I) - SIGNXX(I))**2 +
     .                 HH*(SIGNXX(I) - SIGNYY(I))**2 + TWO*LL*SIGNYZ(I)**2 +
     .                 TWO*MM*SIGNZX(I)**2 + TWO*NN*SIGNXY(I)**2
            SIGHL(I) = SQRT(MAX(SIGHL(I),ZERO))
c
            ! If the continuous hardening yield stress is chosen
            IF (ITAB == 0) THEN
              ! Update the hardening yield stress
              YLD(I) = SIGY
     .               + QR1*(ONE - EXP(-CR1*PLA(I)))
     .               + QR2*(ONE - EXP(-CR2*PLA(I)))
              H(I)   = QR1*CR1*EXP(-CR1*PLA(I)) + QR2*CR2*EXP(-CR2*PLA(I))
c
              ! Update yield function value
              PHI(I)   = SIGHL(I) - YLD(I)
            ENDIF
c
          ENDDO
          ! End of the loop over the yielding elements
c
          ! If the tabulated yield stress is chosen
          IF (ITAB == 1) THEN
            ! ->  No strain-rate effect
            IF (MFUNC == 1) THEN
              ! Recovering position tables
              IPOS1(1:NEL) = VARTMP(1:NEL,1)
              IAD1 (1:NEL) = NPF(KFUNC(1)) / 2 + 1
              ILEN1(1:NEL) = NPF(KFUNC(1)+1) / 2 - IAD1(1:NEL) - IPOS1(1:NEL)
              ! Computing the plastic strain interpolation
              CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,PLA,DYDX1,Y1)
              ! Storing the position
              VARTMP(1:NEL,1) = IPOS1(1:NEL)
              ! Computing the yield stress and its derivative
              YLD(1:NEL)   =  YFAC(1)*Y1(1:NEL)
              H(1:NEL)     =  YFAC(1)*DYDX1(1:NEL)
              ! Update the yield function
              PHI(1:NEL)   = SIGHL(1:NEL) - YLD(1:NEL)
              ! ->  Strain-rate effect
            ELSE
              DO I=1,NEL
                J1 = JJ(I)
                J2 = J1 + 1
                ! Recovering position tables for the first rate
                IPOS1(I) = VARTMP(I,J1)
                IAD1 (I) = NPF(KFUNC(J1)) / 2 + 1
                ILEN1(I) = NPF(KFUNC(J1)+1) / 2 - IAD1(I) - IPOS1(I)
                ! Recovering position tables for the second rate
                IPOS2(I) = VARTMP(I,J2)
                IAD2 (I) = NPF(KFUNC(J2)) / 2 + 1
                ILEN2(I) = NPF(KFUNC(J2)+1) / 2 - IAD2(I) - IPOS2(I)
              ENDDO
              ! Computing the plastic strain interpolation
              CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,PLA,DYDX1,Y1)
              CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL,PLA,DYDX2,Y2)
              ! Storing new positions and computing yield stress
              DO I=1,NEL
                J1 = JJ(I)
                J2 = J1+1
                ! Hardening yield stress
                Y1(I)  = Y1(I)*YFAC(J1)
                Y2(I)  = Y2(I)*YFAC(J2)
                FAC    = (EPSP(I) - RATE(J1))/(RATE(J2) - RATE(J1))
                YLD(I) = Y1(I) + FAC*(Y2(I)-Y1(I))
                YLD(I) = MAX(YLD(I),EM20)
                ! Derivatives of yield stress
                DYDX1(I) = DYDX1(I)*YFAC(J1)
                DYDX2(I) = DYDX2(I)*YFAC(J2)
                H(I)     = DYDX1(I)+FAC*(DYDX2(I)-DYDX1(I))
                ! New positions
                VARTMP(I,J1) = IPOS1(I)
                VARTMP(I,J2) = IPOS2(I)
                ! Update the yield function
                PHI(I)   = SIGHL(I) - YLD(I)
              ENDDO
            ENDIF
          ENDIF
c
        ENDDO
        ! End of the loop over the iterations
        !===================================================================
        ! - END OF PLASTIC CORRECTION WITH CUTTING PLANE ALGORITHM
        !===================================================================
C
        ! Update and store new internal variables
        DO I=1,NEL
          ! Equivalent stress
          SEQ(I)     = SIGHL(I)
          ! Sound-speed
          SOUNDSP(I) = SQRT(MAX(D11,D22,D33)/RHO0(I))
          ! Coefficient for hourglass
          IF (DPLA(I)>ZERO) THEN
            ET(I)    = H(I) / (H(I) + MAX(E11,E22,E33))
          ELSE
            ET(I)  = ONE
          ENDIF
          ! Plastic strain-rate filtering (if needed)
          IF ((ITAB == 1).AND.(MFUNC > 1).AND.(VP == 1)) THEN
            DPDT      = DPLA(I)/MAX(EM20,TIMESTEP)
            UVAR(I,1) = ASRATE * DPDT + (ONE - ASRATE) * UVAR(I,1)
            EPSP(I)   = UVAR(I,1)
          ENDIF
        ENDDO
        IF (ALLOCATED(RATE)) DEALLOCATE(RATE)
        IF (ALLOCATED(YFAC)) DEALLOCATE(YFAC)
C
      END
