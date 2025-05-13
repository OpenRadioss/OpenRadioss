Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    powder_burn        ../common_source/eos/powder_burn.F
      !||--- called by ------------------------------------------------------
      !||    eosmain            ../common_source/eos/eosmain.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod       ../common_source/modules/constant_mod.F
      !||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||====================================================================
      SUBROUTINE POWDER_BURN    (
     1                            IFLAG , NEL    ,PM    ,OFF       , EINT  ,MU          ,MU2 ,
     2                            ESPE  , DVOL   ,DF    ,VNEW      , MAT   ,PSH         ,
     3                            PNEW  , DPDM   ,DPDE  ,MAT_PARAM ,
     4                            VAREOS, NVAREOS,NPROPM,NUMMAT    ,DT1   ,RHO0   ,BFRAC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This is a template to introduce numerical solving of Deflagration EoS (experimental) based on LAW105 Implementation
!----------------------------------------------------------------------------
!! \details STAGGERED SCHEME IS EXECUTED IN TWO PASSES IN EOSMAIN : IFLG=0 THEN IFLG=1
!! \details COLLOCATED SCHEME IS DOING A SINGLE PASS : IFLG=2
!! \details
!! \details  STAGGERED SCHEME
!! \details     EOSMAIN / IFLG = 0 : DERIVATIVE CALCULATION FOR SOUND SPEED ESTIMATION c[n+1] REQUIRED FOR PSEUDO-VISCOSITY (DPDE:partial derivative, DPDM:total derivative)
!! \details     MQVISCB            : PSEUDO-VISCOSITY Q[n+1]
!! \details     MEINT              : INTERNAL ENERGY INTEGRATION FOR E[n+1] : FIRST PART USING P[n], Q[n], and Q[n+1] CONTRIBUTIONS
!! \details     EOSMAIN / IFLG = 1 : UPDATE P[n+1], T[N+1]
!! \details                          INTERNAL ENERGY INTEGRATION FOR E[n+1] : LAST PART USING P[n+1] CONTRIBUTION
!! \details                            (second order integration dE = -P.dV where P = 0.5(P[n+1] + P[n]) )
!! \details  COLLOCATED SCHEME
!! \details     EOSMAIN / IFLG = 2 : SINGLE PASS FOR P[n+1] AND DERIVATIVES
!----------------------------------------------------------------------------
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD, ONLY : MATPARAM_STRUCT_
      use constant_mod , only : zero, one, zep07, three100, em04
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      implicit none
C-----------------------------------------------
C   I n c l u d e   F i l e s
C-----------------------------------------------
#include      "my_real.inc"
#include      "comlock.inc"
#include      "com06_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NVAREOS,NUMMAT,NPROPM
      INTEGER MAT(NEL), IFLAG, NEL
      my_real,INTENT(IN) :: PM(NPROPM,NUMMAT),OFF(NEL),MU(NEL),MU2(NEL),DVOL(NEL),DF(NEL),VNEW(NEL)
      my_real,INTENT(INOUT) :: PNEW(NEL),DPDM(NEL),DPDE(NEL),EINT(NEL),RHO0(NEL),ESPE(NEL)
      my_real,INTENT(INOUT) :: VAREOS(NEL,NVAREOS),BFRAC(NEL)
      my_real,INTENT(IN) :: DT1
      TYPE(MATPARAM_STRUCT_), INTENT(IN) :: MAT_PARAM !< material data structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real :: bulk,P0,PSH,DD,EG,Gr,CC,ALPHA,FSCALE_B,FSCALE_P,FSCALE_G,FSCALE_RHO, C1,C2
      my_real :: TMP, TMP2
      my_real :: compac
      my_real :: TOTAL_BFRAC
      my_real :: DPDM_gas, DPDM_powder
      my_real :: MASS,PS,PG,RHO_S, RHO_G,POLD
      my_real :: RHO(NEL)
      INTEGER :: funcb,funcg
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      my_real,EXTERNAL :: FINTER
      ! EXTERNAL FINTER
      !      Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
      !      Y       : y = f(x)
      !      X       : x
      !      DYDX    : f'(x) = dy/dx
      !      IFUNC(J): FUNCTION INDEX
      !            J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
      !      NPF,TF  : FUNCTION PARAMETER
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------

      ! --- STAGGERED SCHEME IFLG=0
      IF(IFLAG == 0) THEN
        BULK       = MAT_PARAM%EOS%UPARAM(01)
        P0         = MAT_PARAM%EOS%UPARAM(02)
        PSH        = MAT_PARAM%EOS%UPARAM(03)
        DD         = MAT_PARAM%EOS%UPARAM(04)
        EG         = MAT_PARAM%EOS%UPARAM(05)
        Gr         = MAT_PARAM%EOS%UPARAM(06)
        CC         = MAT_PARAM%EOS%UPARAM(07)
        ALPHA      = MAT_PARAM%EOS%UPARAM(08)
        FSCALE_B   = MAT_PARAM%EOS%UPARAM(09)
        FSCALE_P   = MAT_PARAM%EOS%UPARAM(10)
        FSCALE_G   = MAT_PARAM%EOS%UPARAM(11)
        FSCALE_RHO = MAT_PARAM%EOS%UPARAM(12)
        C1         = MAT_PARAM%EOS%UPARAM(13)
        C2         = MAT_PARAM%EOS%UPARAM(14)
        funcb      = MAT_PARAM%EOS%FUNC(1)
        funcg      = MAT_PARAM%EOS%FUNC(2)
        compac = one - zep07  !1-0.07 = 0.93
        TOTAL_BFRAC = zero
        IF(DT1 == ZERO)THEN
          DO I=1,NEL
            ESPE(I) = EG
            EINT(I) = EG*RHO0(I)*VNEW(I)
            VAREOS(I,1) = P0                 !PS
            VAREOS(I,2) = ZERO               !PG
            VAREOS(I,3) = RHO0(I)/compac     !RHO_S
            VAREOS(I,4) = ZERO               !RHO_G
            VAREOS(I,5) = P0                 !POLD
            VAREOS(I,6) = ZERO               !F(t_old)
            VAREOS(I,7) = RHO0(I)*VNEW(I)    !Mass0
            DPDM(I) = BULK
            dPdE(I) = ZERO
          ENDDO
        ENDIF
        DO I=1,NEL
          !--------------------------------!
          ! INIT.                          !
          !--------------------------------!
          RHO(I) = RHO0(I) * (ONE + MU(I))
          MASS = RHO(I)*VNEW(I)
          ESPE = EINT(I)/MASS
          PS   =VAREOS(I,1)
          PG   =VAREOS(I,2)
          RHO_S=VAREOS(I,3)
          RHO_G=VAREOS(I,4)
          POLD =VAREOS(I,5)
          !--------------------------------------------------------------------!
          ! SOUND SPEED DERIVATIVE                                             !
          !  DPDM ! dP/dmu total derivative                                    !
          !  dPdE ! partial derivative (where E=Eint/V0 , and rho0V0=rho.V)    !
          !--------------------------------------------------------------------!
           TMP = (one+mu(i))*rho0(i)/DD
           TMP2 = (one+mu(i)) ; TMP2=TMP2*TMP2
           DPDM_gas = EG*exp(TMP)*(one+TMP) + Pg/TMP2*(one+mu(i))*exp(TMP)
           DPDM_powder = BULK
           DPDM(I) = TOTAL_BFRAC * DPDM_gas + (one-TOTAL_BFRAC)*DPDM_powder   !total derivative of mixture
           dPdE(I) = TOTAL_BFRAC * (one+mu(i)*TMP)                            !partial derivative of mixture
        ENDDO

      ELSEIF(IFLAG == 1) THEN

      ELSEIF (IFLAG == 2) THEN
        DO I=1, NEL
          IF (VNEW(I) > ZERO) THEN
             PNEW(I) = zero
             DPDM(I) = zero !total derivative
             DPDE(I) = zero !partial derivative
          ENDIF
        ENDDO

      ENDIF
C-----------------------------------------------
      RETURN
      END
