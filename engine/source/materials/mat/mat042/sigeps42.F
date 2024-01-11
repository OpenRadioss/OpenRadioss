Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  SIGEPS42                      source/materials/mat/mat042/sigeps42.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|-- calls ---------------
Chd|        PRODAAT                       source/materials/tools/prodAAT.F
Chd|        PRODMAT                       source/materials/tools/prodmat.F
Chd|        VALPVECDP_V                   source/materials/mat/mat033/sigeps33.F
Chd|        VALPVEC_V                     source/materials/mat/mat033/sigeps33.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS42(
     1      NEL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,NPF     ,
     2      TF      ,TIME    ,TIMESTEP,UPARAM  ,RHO0    ,RHO     ,
     3      VOLUME  ,EINT    ,UVAR    ,OFF     ,OFFG    ,SOUNDSP ,
     4      EPSP1   ,EPSP2   ,EPSP3   ,EPSP4   ,EPSP5   ,EPSP6   ,
     5      EPSXX   ,EPSYY   ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX   ,
     6      SIGNXX  ,SIGNYY  ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7      MFXX    ,MFXY    ,MFXZ    ,MFYX    ,MFYY    ,MFYZ    ,  
     8      MFZX    ,MFZY    ,MFZZ    ,VISCMAX ,ISMSTR  ,ET      ,
     9      IHET    ,EPSTH3  ,IEXPAN  )
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
#include "scr17_c.inc"
#include "impl1_c.inc"
#include "tabsiz_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER, INTENT(IN) :: NEL,NUPARAM,NUVAR,ISMSTR,IHET,IEXPAN
      my_real, INTENT(IN) :: TIME, TIMESTEP
      my_real, DIMENSION(NUPARAM), INTENT(IN) :: UPARAM(NUPARAM)
      my_real, DIMENSION(NEL), INTENT(IN) ::RHO,RHO0,VOLUME,EINT,
     .  OFFG,EPSTH3,EPSP1,EPSP2,EPSP3,EPSP4,EPSP5,EPSP6,                  
     .  EPSXX,EPSYY,EPSZZ,EPSXY,EPSYZ,EPSZX,               
     .  MFXX,MFXY,MFXZ,MFYX,MFYY,MFYZ,MFZX,MFZY,MFZZ         
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real ,DIMENSION(NEL), INTENT(OUT) :: SOUNDSP,VISCMAX,
     .  SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
            my_real, DIMENSION(NEL,NUVAR), INTENT(INOUT) :: UVAR(NEL,NUVAR)
      my_real, DIMENSION(NEL), INTENT(INOUT) :: OFF(NEL),ET(NEL)
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION 
C----------------------------------------------------------------
      INTEGER NPF(SNPC),NFUNC,IFUNC(NFUNC)
      my_real FINTER,TF(STF)
      EXTERNAL FINTER
C----------------------------------------------------------------
C  L O C A L  V A R I A B L E S
C----------------------------------------------------------------
      INTEGER I,II,J,K,KFP,NPRONY,IFORM
      my_real TENSIONCUT,AMAX,GVMAX,GMAX,FFAC,TRACE,RBULK,DPDMU,
     .  AA,CC,FAC,INVE,ETV,RV_PUI,AMIN,NU_1
      my_real, DIMENSION(3) :: LAM_AL,FFT
      my_real, DIMENSION(5) :: AL_TAB,MU_TAB
      my_real, DIMENSION(100) :: GI,TAUX
      my_real, DIMENSION(NEL) :: SUMDWDL,RV,J2THIRD,P,DWDRV,ETI,GTMAX 
      my_real, DIMENSION(3,3) :: A
      my_real, DIMENSION(NEL,3) :: T1,SIGPRV,EV,EVM,DWDL,CII
      my_real, DIMENSION(NEL,6) :: DOTB,SVISC,C0,C1
      my_real, DIMENSION(NEL,3,3) :: F,LL,BB,LB,BLT
      my_real, DIMENSION(NEL,6,100) :: H0, H
      my_real, DIMENSION(MVSIZ,3) :: EVV 
      my_real, DIMENSION(MVSIZ,6) :: AV
      my_real, DIMENSION(MVSIZ,3,3) :: DIRPRV
      my_real, DIMENSION(NEL) :: P_FAC
      DOUBLE PRECISION AMAX1
      !=======================================================================
c  
      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering model parameters
      GMAX = ZERO
      DO I=1,5
        !  -> Shear hyperelastic modulus parameters
        MU_TAB(I) = UPARAM(I)
        !  -> Material exponents
        AL_TAB(I) = UPARAM(5+I)
        !  -> Shear modulus
        GMAX = GMAX + MU_TAB(I)*AL_TAB(I)
      ENDDO
      ! Bulk modulus
      RBULK = UPARAM(11)
      ! Cutoff stress in tension
      TENSIONCUT = UPARAM(12)
      ! Bulk function scale factor
      FFAC = UPARAM(15)
      ! Number of Prony Series
      NPRONY = UPARAM(16)
      ! Flag for strain energy density formulation
      IFORM  = UPARAM(17)
      ! Tabulated bulk function ID
      KFP   = IFUNC(1)
      ! Prony series parameters
      GVMAX = ZERO
      ETV   = ZERO
      IF (NPRONY > 0) THEN
        DO I=1,NPRONY
          TAUX(I) = UPARAM(17 + NPRONY + I)
          GI(I)   = UPARAM(17 + I)
          GVMAX   = GVMAX + GI(I)
        ENDDO
        ETV = MIN(GVMAX,RBULK)/GMAX
      ENDIF
c
      ! Fill strain tensor 
      DO I=1,NEL
        AV(I,1) = EPSXX(I)
        AV(I,2) = EPSYY(I)
        AV(I,3) = EPSZZ(I)
        AV(I,4) = EPSXY(I) * HALF
        AV(I,5) = EPSYZ(I) * HALF
        AV(I,6) = EPSZX(I) * HALF
      ENDDO
c 
      ! Compute principal strains 
      ! Note: in simple precision, principal strains are computed
      ! with double precision
      IF (IRESP == 1) THEN
        CALL VALPVECDP_V(AV,EVV,DIRPRV,NEL)
      ELSE
        CALL VALPVEC_V(AV,EVV,DIRPRV,NEL)
      ENDIF
c
      ! Compute principal stretches depending on strain formulation
      ! (Principal stretches = lambda_i) 
      ! -> Logarithmic strains
      IF (ISMSTR == 0 .OR. ISMSTR == 2 .OR. ISMSTR == 4) THEN
        DO I=1,NEL
          EV(I,1) = EXP(EVV(I,1))
          EV(I,2) = EXP(EVV(I,2))
          EV(I,3) = EXP(EVV(I,3))
        ENDDO 
      ! -> Green-Lagrange strains
      ELSEIF (ISMSTR == 10 .OR. ISMSTR == 12) THEN
        DO I =1,NEL
          IF (OFFG(I)<=ONE) THEN
            EV(I,1) = SQRT(EVV(I,1) + ONE)
            EV(I,2) = SQRT(EVV(I,2) + ONE)
            EV(I,3) = SQRT(EVV(I,3) + ONE)
              ELSE
            EV(I,1) = EVV(I,1) + ONE
            EV(I,2) = EVV(I,2) + ONE
            EV(I,3) = EVV(I,3) + ONE
          END IF
        ENDDO 
      ! -> Engineering strains
      ELSE
        DO I=1,NEL
          EV(I,1) = EVV(I,1) + ONE
          EV(I,2) = EVV(I,2) + ONE
          EV(I,3) = EVV(I,3) + ONE
        ENDDO 
      ENDIF
c
      ! Implicit simulation - Hourglass for HEPH
      IF (IMPL_S > 0 .OR. IHET > 1) THEN
        DO J = 1,3
          ! ETI = sum[MU(i)*AL(i)*AMAX**(AL(i)-1)] (i=1,5)
          ! AMAX = 0 --> ETI = 0
          ! else     --> ETI = sum[MU(i)*AL(i)*exp( (AL(i)-1)*ln(AMAX) ) ] ; (i=1,5)
          ETI(1:NEL) = ZERO
          DO II = 1,5
           IF(MU_TAB(II)*AL_TAB(II)/=ZERO) THEN
              DO I=1,NEL
                AMAX = EV(I,J)                
                IF(AMAX/=ZERO) THEN
                  IF((AL_TAB(II)-ONE)/=ZERO) THEN
                    ETI(I) = ETI(I) + MU_TAB(II)*AL_TAB(II) * 
     .                    EXP((AL_TAB(II)-ONE)*LOG(AMAX))
                  ELSE
                    ETI(I) = ETI(I) + MU_TAB(II)*AL_TAB(II)
                  ENDIF
                ENDIF
              ENDDO
           ENDIF
          ENDDO
          ET(1:NEL)= MAX(ETI(1:NEL),ET(1:NEL))
        ENDDO
        DO I=1,NEL
          ET(I) = MAX(ONE,ET(I)/GMAX)
          ET(I)= ET(I)+ETV
        ENDDO
      ENDIF
c 
      ! Relative volume computation 
      ! (RHO/RHO0) = def(F) with F = Grad(Strain)
      DO I=1,NEL
        RV(I) = (EV(I,1)*EV(I,2)*EV(I,3)) 
      ENDDO

      ! Considering thermal expansion
      IF (IEXPAN > 0 .AND. (ISMSTR == 10 .OR. ISMSTR == 11 .OR. ISMSTR == 12)) THEN
        DO I=1,NEL
          RV(I) = RV(I) - EPSTH3(I) 
        ENDDO
      ENDIF
c--- avoid buckling
      P_FAC(1:NEL) = ONE
      NU_1 = FOURTY*(HALF-(3*RBULK-GMAX)/(6*RBULK+GMAX))
      DO I=1,NEL
        AMIN = MIN(EV(I,1),EV(I,2),EV(I,3))
        IF (AMIN<ZEP2) P_FAC(I) = MAX(ONE,NU_1/MAX(EM20,AMIN))
      ENDDO
!        
c 
      ! Compute normalized (deviatoric) stretches and bulk modulus
      ! (Deviatoric principal stretches = lambda_bar_i)
      DO I=1,NEL
c
        ! Deviatoric stretches computed only if relative volume is positive
        IF (RV(I) > ZERO) THEN
          RV_PUI = EXP((-THIRD)*LOG(RV(I))) ! -> J^(-1/3)
          J2THIRD(I) = RV_PUI**2            ! -> (J^(-1/3))^2
        ELSE
          RV_PUI     = ZERO
          J2THIRD(I) = ZERO
        ENDIF
        EVM(I,1) = EV(I,1)*RV_PUI 
        EVM(I,2) = EV(I,2)*RV_PUI
        EVM(I,3) = EV(I,3)*RV_PUI
c
        ! Tabulated bulk modulus with respect to relative volume
        IF (KFP > 0) THEN
          P(I) = RBULK*FFAC*FINTER(KFP,RV(I),NPF,TF,DPDMU)
        ! Constant bulk modulus
        ELSE
          P(I) = P_FAC(I)*RBULK
        ENDIF
      ENDDO
c
      ! For HEPH Hourglass treatment and soundspeed computation
      GTMAX(1:NEL) = GMAX + GVMAX
      ETI(1:NEL) = ZERO
      CII(1:NEL,1:3) = ZERO
      DO II = 1,5
        IF (MU_TAB(II)*AL_TAB(II) /= ZERO) THEN
          DO I=1,NEL
            LAM_AL(1:3) = EXP(AL_TAB(II)*LOG(EVM(I,1:3)))
            AMAX = THIRD*(LAM_AL(1)+LAM_AL(2)+LAM_AL(3))
            CII(I,1:3) = CII(I,1:3) + MU_TAB(II)*AL_TAB(II)*(LAM_AL(1:3)+AMAX)
          ENDDO
        ENDIF
      ENDDO
      ! Note: 2GT=3/2 Cii (factor 3 already inside CII)      
      AMAX1 = 0.81*HALF/GMAX
      DO I = 1,NEL
        AMAX = AMAX1*MAX(CII(I,1),CII(I,2),CII(I,3))
        ETI(I) = MAX(ONE,AMAX) 
        GTMAX(I) = GMAX*ETI(I) + GVMAX
        ET(I) = ETI(I)+ETV
      ENDDO
c
      !=======================================================================
      ! - NEW STRESS TENSOR COMPUTATION
      !=======================================================================
c 
      ! Isochoric strain energy density derivation
      ! Note: here, the table DWDL(:,J) corresponds to the product EVM(J)*DWDEVM(:,J)
      DO J=1,3
        DWDL(1:NEL,J)=ZERO
        DO II=1,5
          IF(MU_TAB(II)/=ZERO) THEN
            DO I=1,NEL
              IF(EVM(I,J)/=ZERO) THEN
                IF(AL_TAB(II)/=ZERO) THEN
                  DWDL(I,J) = DWDL(I,J) + MU_TAB(II) *
     .                        EXP(AL_TAB(II)*LOG(EVM(I,J)))
                ELSE
                  DWDL(I,J) = DWDL(I,J) + MU_TAB(II)
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO
c 
      ! Volumic strain energy density derivation + trace of isochoric derivative
      ! (Volumic strain energy density equation depending on formulation flag)
      DO I=1,NEL
        ! -> Standard strain energy density 
        IF (IFORM == 1) THEN 
          DWDRV(I) = P(I)*(RV(I)- ONE)
        ! -> Modified strain energy density
        ELSEIF (IFORM == 2) THEN 
          DWDRV(I) = P(I)*(ONE - ONE/RV(I))
        ENDIF
        SUMDWDL(I)=(DWDL(I,1)+DWDL(I,2)+DWDL(I,3))*THIRD
      ENDDO
c
      ! Cauchy principal stresses
      DO J=1,3
        DO I=1,NEL
          INVE = ONE/RV(I)
          SIGPRV(I,J) = (DWDL(I,J)-(SUMDWDL(I)-RV(I)*DWDRV(I)))*INVE
        ENDDO
      ENDDO
c
      ! Biot stress tensor for tension cutoff only
      DO I=1,NEL
        T1(I,1) = RV(I)*SIGPRV(I,1)/EV(I,1)
        T1(I,2) = RV(I)*SIGPRV(I,2)/EV(I,2)
        T1(I,3) = RV(I)*SIGPRV(I,3)/EV(I,3)
      ENDDO
c 
      ! Tension cutoff stress
      DO J=1,3
        DO I=1,NEL
          IF(OFF(I)==ZERO.OR.T1(I,J)>ABS(TENSIONCUT))THEN
            SIGPRV(I,1)=ZERO
            SIGPRV(I,2)=ZERO
            SIGPRV(I,3)=ZERO
            IF(OFF(I)/=ZERO) IDEL7NOK = 1
            OFF(I)=ZERO
          ENDIF
        ENDDO
      ENDDO
c
      ! New stress tensor, soundspeed and user-variables
      DO I=1,NEL
c        
        ! Transform principal Cauchy stresses to Global directions
        SIGNXX(I) = DIRPRV(I,1,1)*DIRPRV(I,1,1)*SIGPRV(I,1)
     .            + DIRPRV(I,1,2)*DIRPRV(I,1,2)*SIGPRV(I,2)
     .            + DIRPRV(I,1,3)*DIRPRV(I,1,3)*SIGPRV(I,3)
c     
        SIGNYY(I) = DIRPRV(I,2,2)*DIRPRV(I,2,2)*SIGPRV(I,2)
     .            + DIRPRV(I,2,3)*DIRPRV(I,2,3)*SIGPRV(I,3)
     .            + DIRPRV(I,2,1)*DIRPRV(I,2,1)*SIGPRV(I,1)
c     
        SIGNZZ(I) = DIRPRV(I,3,3)*DIRPRV(I,3,3)*SIGPRV(I,3)
     .            + DIRPRV(I,3,1)*DIRPRV(I,3,1)*SIGPRV(I,1)
     .            + DIRPRV(I,3,2)*DIRPRV(I,3,2)*SIGPRV(I,2)
c     
        SIGNXY(I) = DIRPRV(I,1,1)*DIRPRV(I,2,1)*SIGPRV(I,1)
     .            + DIRPRV(I,1,2)*DIRPRV(I,2,2)*SIGPRV(I,2)
     .            + DIRPRV(I,1,3)*DIRPRV(I,2,3)*SIGPRV(I,3)
c     
        SIGNYZ(I) = DIRPRV(I,2,2)*DIRPRV(I,3,2)*SIGPRV(I,2)
     .            + DIRPRV(I,2,3)*DIRPRV(I,3,3)*SIGPRV(I,3)
     .            + DIRPRV(I,2,1)*DIRPRV(I,3,1)*SIGPRV(I,1)
c     
        SIGNZX(I) = DIRPRV(I,3,3)*DIRPRV(I,1,3)*SIGPRV(I,3)
     .            + DIRPRV(I,3,1)*DIRPRV(I,1,1)*SIGPRV(I,1)
     .            + DIRPRV(I,3,2)*DIRPRV(I,1,2)*SIGPRV(I,2)
c
        ! Save user variables for post-processing
        UVAR(I,1)  = MAX(SIGPRV(I,1),SIGPRV(I,2),SIGPRV(I,3))
        UVAR(I,2)  = MIN(SIGPRV(I,1),SIGPRV(I,2),SIGPRV(I,3))
        UVAR(I,3)  = OFF(I)
        ! Soundspeed computation
        ! Note: for Iform = 2, the constant bulk is assumed for the soundspeed computation
        ! (if stability issue is observed, switch to non-linear bulk)
        SOUNDSP(I) = SQRT((TWO_THIRD*GTMAX(I)+P(I))/RHO(I))
        ! Viscosity parameter set to zero
        VISCMAX(I) = ZERO
      ENDDO
c
      !=======================================================================
      ! - VISCOUS STRESSES COMPUTATION (PRONY SERIES)
      !=======================================================================  
      IF (NPRONY > 0) THEN  
        IF (ISMSTR == 10 .or. ISMSTR == 12) THEN     ! New formulation, using Dot(B)   
c---
c         F = deformation gradient
c         L = velocity gradient
c         L = D + W = sym(L) + skw(L) => LL = sym(L)
c
          DO I=1,NEL
            LL(I,1,1) = EPSP1(I)           ! EPSPXX
            LL(I,2,2) = EPSP2(I)           ! EPSPYY
            LL(I,3,3) = EPSP3(I)           ! EPSPZZ
            LL(I,1,2) = EPSP4(I) * HALF    !(EPSPXY(I) + EPSPYX(I))/2
            LL(I,2,3) = EPSP5(I) * HALF    !(EPSPYZ(I) + EPSPZY(I))/2
            LL(I,1,3) = EPSP6(I) * HALF    !(EPSPZX(I) + EPSPXZ(I))/2
            LL(I,2,1) = LL(I,1,2)
            LL(I,3,1) = LL(I,1,3)
            LL(I,3,2) = LL(I,2,3)
c
            F(I,1,1)  = ONE + MFXX(I)
            F(I,2,2)  = ONE + MFYY(I)
            F(I,3,3)  = ONE + MFZZ(I)
            F(I,1,2)  = MFXY(I)
            F(I,2,3)  = MFYZ(I)
            F(I,3,1)  = MFZX(I)      
            F(I,2,1)  = MFYX(I)
            F(I,3,2)  = MFZY(I)
            F(I,1,3)  = MFXZ(I)     
          ENDDO
c
          CALL PRODAAT(F,BB,NEL)     ! B = F * FT
c
c         Dev(B) = B * J^(-2/3),  J = det(F)
c
          DO I=1,NEL
            BB(I,1,1) = BB(I,1,1) * J2THIRD(I)
            BB(I,2,2) = BB(I,2,2) * J2THIRD(I)
            BB(I,3,3) = BB(I,3,3) * J2THIRD(I)
            BB(I,1,2) = BB(I,1,2) * J2THIRD(I)
            BB(I,2,3) = BB(I,2,3) * J2THIRD(I)
            BB(I,1,3) = BB(I,1,3) * J2THIRD(I)
            BB(I,2,1) = BB(I,1,2)
            BB(I,3,2) = BB(I,2,3)
            BB(I,3,1) = BB(I,1,3)
          ENDDO
c
          CALL PRODMAT(LL,BB,LB,NEL)     ! LB = L * B          
c
          CALL PRODMAT(BB,LL,BLT,NEL)    ! BLT = B * LT
c
          DO I=1,NEL
            DOTB(I,1) = LB(I,1,1) + BLT(I,1,1)  ! xx
            DOTB(I,2) = LB(I,2,2) + BLT(I,2,2)  ! yy
            DOTB(I,3) = LB(I,3,3) + BLT(I,3,3)  ! zz
            DOTB(I,4) = LB(I,1,2) + BLT(I,1,2)  ! xy = yx
            DOTB(I,5) = LB(I,2,3) + BLT(I,2,3)  ! yz = zy
            DOTB(I,6) = LB(I,1,3) + BLT(I,1,3)  ! xz = zx
          ENDDO
c
          DO J=1,6
            SVISC(1:NEL,J) = ZERO
            DO II= 1,NPRONY 
              FAC= -TIMESTEP/TAUX(II) 
              DO I=1,NEL
                H0(I,J,II) = UVAR(I, 6 + (II - 1)*6 + J)
                H(I,J,II)  = EXP(FAC)*H0(I,J,II) +
     .                       EXP(HALF*FAC)*DOTB(I,J)*TIMESTEP
                UVAR(I,  6 + (II - 1)*6 + J)=  H(I,J,II)
              ENDDO
            ENDDO
c
c          Kirchoff viscous stress
c             
            DO II = 1,NPRONY
              DO I=1,NEL
                SVISC(I,J) = SVISC(I,J) + GI(II)*H(I,J,II)
              ENDDO         
            ENDDO  
          ENDDO     
c
c         Kirchoff -> Cauchy visc stress : sig = T / J
c
          DO I=1,NEL
            INVE = ONE/RV(I)      
            SVISC(I,1) = SVISC(I,1)*INVE
            SVISC(I,2) = SVISC(I,2)*INVE
            SVISC(I,3) = SVISC(I,3)*INVE       
            SVISC(I,4) = SVISC(I,4)*INVE
            SVISC(I,5) = SVISC(I,5)*INVE
            SVISC(I,6) = SVISC(I,6)*INVE
c           deviatoric part of visc stress
            TRACE = (SVISC(I,1) + SVISC(I,2) + SVISC(I,3)) * THIRD
            SVISC(I,1) = SVISC(I,1) - TRACE
            SVISC(I,2) = SVISC(I,2) - TRACE
            SVISC(I,3) = SVISC(I,3) - TRACE      
c           total stress
            SIGNXX(I) = SIGNXX(I) + SVISC(I,1)
            SIGNYY(I) = SIGNYY(I) + SVISC(I,2)
            SIGNZZ(I) = SIGNZZ(I) + SVISC(I,3)
            SIGNXY(I) = SIGNXY(I) + SVISC(I,4)
            SIGNYZ(I) = SIGNYZ(I) + SVISC(I,5)
            SIGNZX(I) = SIGNZX(I) + SVISC(I,6) 
          ENDDO 
c
        ELSE  ! ISMSTR /= 10
c
          DO I=1,NEL 
C           
            C0(I,1) = UVAR(I,4)
            C0(I,2) = UVAR(I,5)
            C0(I,3) = UVAR(I,6)  
            C0(I,4) = UVAR(I,7)
            C0(I,5) = UVAR(I,8)
            C0(I,6) = UVAR(I,9)
C            
            CC = THIRD*(EVM(I,1)**2 +  EVM(I,2)**2 + EVM(I,3)**2)
            FFT(1) =  EVM(I,1)**2 - CC
            FFT(2) =  EVM(I,2)**2 - CC
            FFT(3) =  EVM(I,3)**2 - CC   
c
            C1(I,1) = DIRPRV(I,1,1)*DIRPRV(I,1,1)*FFT(1)
     .            + DIRPRV(I,1,2)*DIRPRV(I,1,2)*FFT(2)
     .            + DIRPRV(I,1,3)*DIRPRV(I,1,3)*FFT(3)
c     
            C1(I,2) = DIRPRV(I,2,2)*DIRPRV(I,2,2)*FFT(2)
     .            + DIRPRV(I,2,3)*DIRPRV(I,2,3)*FFT(3)
     .            + DIRPRV(I,2,1)*DIRPRV(I,2,1)*FFT(1)
c     
            C1(I,3) = DIRPRV(I,3,3)*DIRPRV(I,3,3)*FFT(3)
     .            + DIRPRV(I,3,1)*DIRPRV(I,3,1)*FFT(1)
     .            + DIRPRV(I,3,2)*DIRPRV(I,3,2)*FFT(2)
c     
            C1(I,4) = DIRPRV(I,1,1)*DIRPRV(I,2,1)*FFT(1)
     .            + DIRPRV(I,1,2)*DIRPRV(I,2,2)*FFT(2)
     .            + DIRPRV(I,1,3)*DIRPRV(I,2,3)*FFT(3)
c     
            C1(I,5) = DIRPRV(I,2,2)*DIRPRV(I,3,2)*FFT(2)
     .            + DIRPRV(I,2,3)*DIRPRV(I,3,3)*FFT(3)
     .            + DIRPRV(I,2,1)*DIRPRV(I,3,1)*FFT(1)
c     
            C1(I,6) = DIRPRV(I,3,3)*DIRPRV(I,1,3)*FFT(3)
     .            + DIRPRV(I,3,1)*DIRPRV(I,1,1)*FFT(1)
     .            + DIRPRV(I,3,2)*DIRPRV(I,1,2)*FFT(2)  
C            
            UVAR(I,4) = C1(I,1)   
            UVAR(I,5) = C1(I,2)
            UVAR(I,6) = C1(I,3) 
            UVAR(I,7) = C1(I,4)  
            UVAR(I,8) = C1(I,5)
            UVAR(I,9) = C1(I,6)
          ENDDO
C           
          ! Viscous stresses computation
          DO J=1,6
            SVISC(1:NEL,J) = ZERO
            DO II= 1,NPRONY 
              FAC= -TIMESTEP/TAUX(II) 
              DO I=1,NEL
                H0(I,J,II) = UVAR(I, 12 + (II - 1)*6 + J)
                H(I,J,II)  = EXP(FAC)*H0(I,J,II) +
     .               EXP(HALF*FAC)*(C1(I,J) - C0(I,J))
                UVAR(I,  12 + (II - 1)*6 + J)=  H(I,J,II)
              ENDDO
            ENDDO
C
            DO II = 1,NPRONY
              DO I=1,NEL
                SVISC(I,J) = SVISC(I,J) + GI(II)*H(I,J,II)                              
              ENDDO         
            ENDDO  
          ENDDO     
c
          ! Add viscous stresses to the stress tensor
          DO I=1,NEL                        
            INVE = ONE/RV(I)                 
            SVISC(I,1) = SVISC(I,1)*INVE          
            SVISC(I,2) = SVISC(I,2)*INVE          
            SVISC(I,3) = SVISC(I,3)*INVE          
            SVISC(I,4) = SVISC(I,4)*INVE          
            SVISC(I,5) = SVISC(I,5)*INVE          
            SVISC(I,6) = SVISC(I,6)*INVE          
C                                           
            SIGNXX(I) = SIGNXX(I) + SVISC(I,1) 
            SIGNYY(I) = SIGNYY(I) + SVISC(I,2) 
            SIGNZZ(I) = SIGNZZ(I) + SVISC(I,3) 
            SIGNXY(I) = SIGNXY(I) + SVISC(I,4) 
            SIGNYZ(I) = SIGNYZ(I) + SVISC(I,5) 
            SIGNZX(I) = SIGNZX(I) + SVISC(I,6)  
          ENDDO
c
        ENDIF  ! ISMSTR   
      ENDIF
C-----------      
      END

