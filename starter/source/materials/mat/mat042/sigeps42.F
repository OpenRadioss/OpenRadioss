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
!||    sigeps42           ../starter/source/materials/mat/mat042/sigeps42.F
!||--- called by ------------------------------------------------------
!||    mulaw              ../starter/source/materials/mat_share/mulaw.F
!||--- calls      -----------------------------------------------------
!||    finter             ../starter/source/tools/curve/finter.F
!||    valpvec            ../starter/source/materials/tools/matrix.F
!||    valpvecdp          ../starter/source/materials/tools/matrix.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE SIGEPS42(
     1      MAT_PARAM,NEL      ,NUVAR    ,NFUNC    ,IFUNC    ,NPF      ,
     2      TF       ,TIME     ,TIMESTEP ,RHO0     ,RHO      ,
     3      EPSPXX   ,EPSPYY   ,EPSPZZ   ,EPSPXY   ,EPSPYZ   ,EPSPZX   ,
     4      DEPSXX   ,DEPSYY   ,DEPSZZ   ,DEPSXY   ,DEPSYZ   ,DEPSZX   , 
     5      EPSXX    ,EPSYY    ,EPSZZ    ,EPSXY    ,EPSYZ    ,EPSZX    ,
     6      SIGOXX   ,SIGOYY   ,SIGOZZ   ,SIGOXY   ,SIGOYZ   ,SIGOZX   ,
     7      SIGNXX   ,SIGNYY   ,SIGNZZ   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     8      SOUNDSP  ,VISCMAX  ,UVAR     ,OFF      ,WXXDT    ,WYYDT    ,
     A      WZZDT    ,ISMSTR   ,MFXX     ,MFXY     ,MFXZ     ,MFYX     ,
     B      MFYY     ,MFYZ     ,MFZX     ,MFZY     ,MFZZ     )              
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include "mvsiz_p.inc"
C-----------------------------------------------
C   C O M M O N 
C-----------------------------------------------
#include "scr05_c.inc"
#include "tabsiz_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER, INTENT(IN) :: NEL,NUVAR,ISMSTR
      my_real, INTENT(IN) ::
     .      TIME       , TIMESTEP   ,
     .      RHO   (NEL), RHO0  (NEL), 
     .      EPSPXX(NEL), EPSPYY(NEL), EPSPZZ(NEL),
     .      EPSPXY(NEL), EPSPYZ(NEL), EPSPZX(NEL),
     .      DEPSXX(NEL), DEPSYY(NEL), DEPSZZ(NEL),
     .      DEPSXY(NEL), DEPSYZ(NEL), DEPSZX(NEL),
     .      EPSXX (NEL), EPSYY (NEL), EPSZZ (NEL),
     .      EPSXY (NEL), EPSYZ (NEL), EPSZX (NEL),
     .      SIGOXX(NEL), SIGOYY(NEL), SIGOZZ(NEL),
     .      SIGOXY(NEL), SIGOYZ(NEL), SIGOZX(NEL),
     .      MFXX(NEL)  ,   MFXY(NEL),   MFXZ(NEL),
     .      MFYX(NEL)  ,   MFYY(NEL),   MFYZ(NEL),
     .      MFZX(NEL)  ,   MFZY(NEL),   MFZZ(NEL),    
     .      WZZDT(MVSIZ),WYYDT(MVSIZ),WXXDT(MVSIZ)
      TYPE(MATPARAM_STRUCT_) ,INTENT(INOUT) :: MAT_PARAM
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real, INTENT(OUT) ::
     .      SIGNXX (NEL), SIGNYY (NEL), SIGNZZ(NEL),
     .      SIGNXY (NEL), SIGNYZ (NEL), SIGNZX(NEL),
     .      SOUNDSP(NEL), VISCMAX(NEL)
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real, INTENT(INOUT) :: 
     .      UVAR(NEL,NUVAR), OFF(NEL) 
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION 
C----------------------------------------------------------------
      INTEGER NPF(SNPC),NFUNC,IFUNC(NFUNC)
      my_real FINTER,TF(STF)
      EXTERNAL FINTER
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER I,J,K,KFP,IFORM,NORDER
      my_real :: TENSIONCUT,GMAX,FFAC
      my_real, DIMENSION(10) :: MU,AL
      my_real
     .  SIGPRV(3,MVSIZ),EV(3,MVSIZ),EVM(3,MVSIZ),DWDL(3,MVSIZ),
     .  T1(3,MVSIZ),SUMDWDL(MVSIZ),RV(MVSIZ),P(MVSIZ),
     .  DWDRV(MVSIZ),RBULK,DPDMU,AV(6,MVSIZ),EVV(3,MVSIZ),
     .  DIRPRV(3,3,MVSIZ),A(3,3),DPRA(3,3),EIGEN(3)                
C----------------------------------------------------------------
      !=======================================================================
      ! - RECOVERING MATERIAL PARAMETERS
      !=======================================================================
       
      NORDER = MAT_PARAM%IPARAM(1)   ! number of Ogden terms
      IFORM  = MAT_PARAM%IPARAM(3)   ! Flag for strain energy density formulation
!
      ! Shear hyperelastic modulus parameters and material exponents
      DO I=1,NORDER
        MU(I) = MAT_PARAM%UPARAM(I)
        AL(I) = MAT_PARAM%UPARAM(10+I)
      END DO
      ! Shear modulus
      GMAX = ZERO
      DO I=1,NORDER
        GMAX = GMAX + MU(I) * AL(I)
      END DO
      ! Bulk modulus
      RBULK = MAT_PARAM%UPARAM(21)
      ! Cutoff stress in tension
      TENSIONCUT = MAT_PARAM%UPARAM(23)
      ! Bulk function scale factor
      FFAC = MAT_PARAM%UPARAM(24)
      ! Tabulated bulk function ID
      KFP = IFUNC(1)
c
      ! Fill strain tensor 
      DO I = 1,NEL
        AV(1,I) = EPSXX(I)
        AV(2,I) = EPSYY(I)
        AV(3,I) = EPSZZ(I)
        AV(4,I) = EPSXY(I)*HALF
        AV(5,I) = EPSYZ(I)*HALF
        AV(6,I) = EPSZX(I)*HALF
      ENDDO
c 
      ! Compute principal strains 
      ! Note: in simple precision, principal strains are computed
      ! with double precision
      IF (IRESP == 1) THEN
        CALL VALPVECDP(AV,EVV,DIRPRV,NEL)
      ELSE
        CALL VALPVEC(AV,EVV,DIRPRV,NEL)
      ENDIF
c
      ! Compute principal stretches depending on strain formulation
      ! (Principal stretches = lambda_i) 
      DO I = 1,NEL
        ! -> Logarithmic strains
        IF (ISMSTR == 0 .OR. ISMSTR == 2 .OR. ISMSTR == 4) THEN
          EV(1,I) = EXP(EVV(1,I))
          EV(2,I) = EXP(EVV(2,I))
          EV(3,I) = EXP(EVV(3,I))
        ! -> Green-Lagrange strains
        ELSEIF (ISMSTR == 10 .OR. ISMSTR == 12) THEN
          EV(1,I) = SQRT(EVV(1,I)+ ONE)
          EV(2,I) = SQRT(EVV(2,I)+ ONE)
          EV(3,I) = SQRT(EVV(3,I)+ ONE)
        ! -> Engineering strains  
        ELSE
          EV(1,I) = EVV(1,I)+ ONE
          EV(2,I) = EVV(2,I)+ ONE
          EV(3,I) = EVV(3,I)+ ONE  
        ENDIF
      ENDDO
c 
      ! Relative volume computation 
      ! (RHO/RHO0) = def(F) with F = Grad(Strain)
      DO I = 1,NEL
        RV(I) = (EV(1,I)*EV(2,I)*EV(3,I))
      ENDDO
c
      ! Compute normalized (deviatoric) stretches and bulk modulus
      ! (Deviatoric principal stretches = lambda_bar_i)
      DO I = 1,NEL
        ! Deviatoric stretches
        EVM(1,I) = EV(1,I)*RV(I)**(-THIRD)
        EVM(2,I) = EV(2,I)*RV(I)**(-THIRD)
        EVM(3,I) = EV(3,I)*RV(I)**(-THIRD)
c
        ! Tabulated bulk modulus with respect to relative volume        
        IF (KFP /= 0) THEN
          P(I) = RBULK*FFAC*FINTER(KFP,RV(I),NPF,TF,DPDMU)
        ! Constant bulk modulus
        ELSE
          P(I) = RBULK
        ENDIF
      ENDDO
c
      !=======================================================================
      ! - STRESS TENSOR COMPUTATION
      !=======================================================================
c 
      ! Isochoric strain energy density derivation
      ! Note: here, the table DWDL(:,J) corresponds to the product EVM(J)*DWDEVM(:,J)
      DO I=1,NEL
        DWDL(1,I) = ZERO          
        DWDL(2,I) = ZERO          
        DWDL(3,I) = ZERO          
        DO J=1,NORDER                                                
          DWDL(1,I) = DWDL(1,I) + MU(J)*(EVM(1,I)**AL(J) - ONE)        
          DWDL(2,I) = DWDL(2,I) + MU(J)*(EVM(2,I)**AL(J) - ONE)         
          DWDL(3,I) = DWDL(3,I) + MU(J)*(EVM(3,I)**AL(J) - ONE) 
        END DO        
      ENDDO
c 
      ! Volumic strain energy density derivation + trace of isochoric derivative
      ! (Volumic strain energy density equation depending on formulation flag)
      DO I=1,NEL
        ! -> Standard strain energy density 
        IF (IFORM == 1) THEN 
          DWDRV(I) = P(I)*(RV(I) - ONE)
        ! -> Modified strain energy density
        ELSEIF (IFORM == 2) THEN 
          DWDRV(I) = P(I)*(ONE - ONE/RV(I))
        ENDIF
        SUMDWDL(I) = (DWDL(1,I) + DWDL(2,I) + DWDL(3,I))*THIRD
      ENDDO
c
      ! Cauchy principal stresses
      DO I = 1,NEL
        DO J = 1,3
          SIGPRV(J,I) = (DWDL(J,I)-(SUMDWDL(I)-RV(I)*DWDRV(I)))/RV(I)
        ENDDO
      ENDDO
c
      ! Biot stress tensor for tension cutoff only
      DO I=1,NEL
        T1(1,I) = RV(I)*SIGPRV(1,I)/EV(1,I)
        T1(2,I) = RV(I)*SIGPRV(2,I)/EV(2,I)
        T1(3,I) = RV(I)*SIGPRV(3,I)/EV(3,I)
      ENDDO
c 
      ! Tension cutoff stress
      DO I=1,NEL
        DO J=1,3
          IF (OFF(I) == ZERO .OR. T1(J,I) > ABS(TENSIONCUT)) THEN
            SIGPRV(1,I) = ZERO
            SIGPRV(2,I) = ZERO
            SIGPRV(3,I) = ZERO
            OFF(I) = ZERO
          ENDIF
        ENDDO
      ENDDO
c
      ! Stress tensor, soundspeed and user-variables
      DO I=1,NEL
c        
        ! Transform principal Cauchy stresses to Global directions
        SIGNXX(I) = DIRPRV(1,1,I)*DIRPRV(1,1,I)*SIGPRV(1,I)
     .            + DIRPRV(1,2,I)*DIRPRV(1,2,I)*SIGPRV(2,I)
     .            + DIRPRV(1,3,I)*DIRPRV(1,3,I)*SIGPRV(3,I)
c
        SIGNYY(I) = DIRPRV(2,2,I)*DIRPRV(2,2,I)*SIGPRV(2,I)
     .            + DIRPRV(2,3,I)*DIRPRV(2,3,I)*SIGPRV(3,I)
     .            + DIRPRV(2,1,I)*DIRPRV(2,1,I)*SIGPRV(1,I)
c
        SIGNZZ(I) = DIRPRV(3,3,I)*DIRPRV(3,3,I)*SIGPRV(3,I)
     .            + DIRPRV(3,1,I)*DIRPRV(3,1,I)*SIGPRV(1,I)
     .            + DIRPRV(3,2,I)*DIRPRV(3,2,I)*SIGPRV(2,I)
c
        SIGNXY(I) = DIRPRV(1,1,I)*DIRPRV(2,1,I)*SIGPRV(1,I)
     .            + DIRPRV(1,2,I)*DIRPRV(2,2,I)*SIGPRV(2,I)
     .            + DIRPRV(1,3,I)*DIRPRV(2,3,I)*SIGPRV(3,I)
c
        SIGNYZ(I) = DIRPRV(2,2,I)*DIRPRV(3,2,I)*SIGPRV(2,I)
     .            + DIRPRV(2,3,I)*DIRPRV(3,3,I)*SIGPRV(3,I)
     .            + DIRPRV(2,1,I)*DIRPRV(3,1,I)*SIGPRV(1,I)
c
        SIGNZX(I) = DIRPRV(3,3,I)*DIRPRV(1,3,I)*SIGPRV(3,I)
     .            + DIRPRV(3,1,I)*DIRPRV(1,1,I)*SIGPRV(1,I)
     .            + DIRPRV(3,2,I)*DIRPRV(1,2,I)*SIGPRV(2,I)
c
        ! Save user variables for post-processing     
        UVAR(I,1) = MAX(SIGPRV(1,I),SIGPRV(2,I),SIGPRV(3,I))
        UVAR(I,2) = MIN(SIGPRV(1,I),SIGPRV(2,I),SIGPRV(3,I))
        UVAR(I,3) = OFF(I)
        ! Soundspeed computation
        ! Note: for Iform = 2, the constant bulk is assumed for the soundspeed computation
        ! (if stability issue is observed, switch to non-linear bulk)
        SOUNDSP(I) = SQRT((TWO_THIRD*GMAX+P(I))/RHO(I))
        ! Viscosity parameter set to zero
        VISCMAX(I) = ZERO
      ENDDO
      END

