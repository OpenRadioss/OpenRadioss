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
!||    m51init                ../starter/source/materials/mat/mat051/m51init.F
!||--- called by ------------------------------------------------------
!||    matini                 ../starter/source/materials/mat_share/matini.F
!||--- calls      -----------------------------------------------------
!||    m5in2                  ../starter/source/initial_conditions/detonation/m5in2.F
!||    m5in3                  ../starter/source/initial_conditions/detonation/m5in3.F
!||    nrf51ini               ../starter/source/materials/mat/mat051/nrf51ini.F
!||--- uses       -----------------------------------------------------
!||    detonators_mod         ../starter/share/modules1/detonators_mod.F
!||====================================================================
      SUBROUTINE M51INIT(
     .               IPM                ,DETONATORS ,PM      ,TB      ,
     .               NUVAR              ,UVAR       ,UPARAM  ,X       ,
     .               MAT                ,IPARG      ,IFORM   ,IX      ,NIX       ,
     .               ALE_CONNECTIVITY   ,BUFMAT     ,RHO0    ,
     .               GBUF               ,NEL        ,SIG     ,MAT_PARAM , NPF, TF )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
      USE DETONATORS_MOD
      USE ALE_CONNECTIVITY_MOD
      USE MULTIMAT_PARAM_MOD , ONLY : M51_N0PHAS, M51_NVPHAS
      USE MATPARAM_DEF_MOD , ONLY : MATPARAM_STRUCT_
      USE EOSMAIN_MOD , ONLY : EOSMAIN
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IPM(NPROPMI,NUMMAT),MAT(NEL), IPARG(NPARG),IFORM,NIX,IX(NIX,*)
      my_real ::  PM(NPROPM,NUMMAT),UPARAM(*), X(3,NUMNOD), BUFMAT(*), RHO0
      my_real , TARGET :: UVAR(NEL,NUVAR)
      INTEGER,INTENT(IN) :: NEL
      my_real,INTENT(INOUT) :: SIG(NEL,6)
      TYPE(G_BUFEL_), INTENT(INOUT),TARGET :: GBUF
      TYPE(DETONATORS_STRUCT_) :: DETONATORS
      TYPE(T_ALE_CONNECTIVITY), INTENT(INOUT) :: ALE_CONNECTIVITY
      TYPE (MATPARAM_STRUCT_), DIMENSION(NUMMAT), INTENT(INOUT) :: MAT_PARAM
      INTEGER,INTENT(IN) :: NPF(*)
      my_real,INTENT(IN) :: TF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,NUVAR, ISFLUID
      INTEGER :: NPH,IFLG,NV46
      INTEGER :: IMAT,MID
      INTEGER :: KK,NUMEL
      INTEGER :: IDX_YIELD(4)
      INTEGER :: EOSTYPE, MATLAW
      INTEGER :: NBMAT
      my_real :: GG1, GG2, GG3, GG4
      my_real :: VOLD,P0_(4),T0_(4),PMIN_(4),E0_(4),RHO0_(4),VF(4),PEXT,PRES_0,SSP_(4),GG_(4),DPDM_(4)
      my_real :: C0_(4),C1_(4),C2_(4),C3_(4),C4_(4),C5_(4)
      my_real,intent(inout) :: tb(nel)
      my_real,DIMENSION(1) :: OFF, EINT, RHO, RHO_0, MU, MU2, ESPE, MUOLD, BURNFRAC
      my_real,DIMENSION(1) :: DVOL, DF, VOL, PRES, SSP, DPDE, DPDM, THETA, PSH
      INTEGER :: MAT_(1), ISUB
      my_real :: SPH1, SPH2, SPH3, SPH4
      my_real :: V1, V2, V3, V4, K_mix, SSP_mix, RHO0_MIX, E0_mix
      my_real,DIMENSION(NEL,6) :: SIGOLD
      my_real :: PMIN, DENOM
      LOGICAL :: IS_IFORM12
      integer, parameter :: nvartmp = 3
      integer, parameter :: nvareos = 6
      my_real,DIMENSION(1,nvareos) :: VAREOS ! pointer to UVAR
      integer :: vartmp(1, nvartmp)
C-----------------------------------------------

      IFLG  = NINT(UPARAM(55))
      IFORM = NINT(UPARAM(31))
      SPH1  = UPARAM(112)
      SPH2  = UPARAM(162)
      SPH3  = UPARAM(212)
      SPH4  = UPARAM(262)

      IDX_YIELD(1:4) = (/100,150,200,250/)

      !need to initialize element buffer with initial density for this specific case.
      IS_IFORM12 = .FALSE.
      IF(  NINT(UPARAM(76)) == 12) IS_IFORM12 = .TRUE.
      IF(IFORM == 12)THEN
        UPARAM(31) = 1
        IFORM = 1
      ENDIF

      !---------------------------------!
      ! Test if only fluids are defined !
      !---------------------------------!
      GG1 = UPARAM(101)
      GG2 = UPARAM(151)
      GG3 = UPARAM(201)
      GG4 = ZERO
      GG_(1:4) = (/GG1,GG2,GG3,GG4/)
      ISFLUID = 0
      IF (GG1 == ZERO .AND. GG2 == ZERO .AND. GG3 == ZERO) ISFLUID=1
      IF (ISFLUID==1) THEN 
        IPARG(15) = 1  !  FLAG REZONE SIG                                 =>  KEPT TO 1 FOR BACKWARD COMPATIBILITY
        IPARG(16) = 1  !  FLAG REZONE EPS PLAST.                          =>  KEPT TO 1 FOR BACKWARD COMPATIBILITY
        IPARG(63) = 1  !  FLAG FOR FLUID MATERIAL
        IPARG(64) = 0  !  FLAG FOR SILENT BOUNDARY FORMULATION
      ENDIF 

      !---------------------------------!
      ! Test if NRF formulation         !
      !---------------------------------!      
      IF(IFORM == 2 .OR. IFORM == 3 .OR. IFORM == 4 .OR. IFORM==5 .OR. IFORM == 6) THEN
        IPARG(15) = 0  !  FLAG REZONE SIG
        IPARG(16) = 0  !  FLAG REZONE EPS PLAST.
        IPARG(63) = 1  !  FLAG FOR FLUID MATERIAL
        IPARG(64) = 1  !  FLAG FOR SILENT BOUNDARY FORMULATION        
      ENDIF

      !---------------------------------!
      ! Compute Burning Time            !
      !---------------------------------!
      IF(IFLG == 1 .AND. IPARG(64) == 0)THEN   !unplug for NRF
        NPH = 1
        IF(N2D == 0)THEN
          CALL M5IN3 (PM,MAT,0,DETONATORS,TB,IPARG,X,IX,NIX)
        ELSE
          CALL M5IN2 (PM,MAT,0,DETONATORS,TB,X,IX,NIX)
        ENDIF
      ENDIF

      !C==========================================
      !C     USER VARIABLES INITIALIZATION T=0
      !C==========================================
      C0_(1:4)  = (/UPARAM(35:37),UPARAM(49)/)
      C1_(1:4)  = (/UPARAM(12:14),UPARAM(50)/)
      C2_(1:4)  = (/UPARAM(15:17),ZERO/)
      C3_(1:4)  = (/UPARAM(18),UPARAM(20:21),ZERO/)
      C4_(1:4)  = (/UPARAM(22:24),ZERO/)
      C5_(1:4)  = (/UPARAM(25:27),ZERO/)
      E0_(1:4)   = (/UPARAM(32:34),UPARAM(48)/)
      T0_(1:4)   = (/UPARAM(113),UPARAM(163),UPARAM(213),UPARAM(263)/)
      RHO0_(1:4) = (/UPARAM(09:11),UPARAM(47)/)
      VF(1:4)    = (/UPARAM(04:06),UPARAM(46)/)
      PEXT       = UPARAM(8)
      P0_(1:4)   = (/UPARAM(57:60)/)
      PMIN_(1:4) = (/UPARAM(39),UPARAM(40),UPARAM(41),UPARAM(56)/)
      T0_(1:4)   = (/UPARAM(IDX_YIELD(1)+13),UPARAM(IDX_YIELD(1)+13),UPARAM(IDX_YIELD(1)+13),UPARAM(IDX_YIELD(1)+13)/)

      ! INITIAL SOUND SPEED (mu=0,E=E0)

      IF(.NOT. IS_IFORM12)THEN

        ! old way with embedded polynomial EoS (backward compatibility)
        P0_(1)     = C0_(1)+C4_(1)*E0_(1)
        P0_(2)     = C0_(2)+C4_(2)*E0_(2)
        P0_(3)     = C0_(3)+C4_(3)*E0_(3)
        P0_(4)     = C0_(4)
        DPDM_(1)  = C1_(1)+C5_(1)*E0_(1)+P0_(1)*C4_(1)
        DPDM_(2)  = C1_(2)+C5_(2)*E0_(2)+P0_(2)*C4_(2)
        DPDM_(3)  = C1_(3)+C5_(3)*E0_(3)+P0_(3)*C4_(3)
        DPDM_(4)  = C1_(4)+C5_(4)*E0_(4)+P0_(4)*C4_(4)
        SSP_(1:4)=ZERO
        IF(RHO0_(1) > ZERO)SSP_(1) = SQRT( (ABS(DPDM_(1))+TWO*TWO_THIRD*GG1)/RHO0_(1) )
        IF(RHO0_(2) > ZERO)SSP_(2) = SQRT( (ABS(DPDM_(2))+TWO*TWO_THIRD*GG2)/RHO0_(2) )
        IF(RHO0_(3) > ZERO)SSP_(3) = SQRT( (ABS(DPDM_(3))+TWO*TWO_THIRD*GG3)/RHO0_(3) )
        IF(RHO0_(4) > ZERO)SSP_(4) = SQRT( (ABS(DPDM_(4))+TWO*TWO_THIRD*GG4)/RHO0_(4) )

      ELSE

        NBMAT = MAT_PARAM(MAT(1))%MULTIMAT%NB
        VAREOS(1,1:6) = ZERO
        SSP_(1:4)=ZERO
        DO IMAT=1,NBMAT
          MID = MAT_PARAM(MAT(1))%MULTIMAT%MID(IMAT)
          ISUB = NINT(UPARAM(276+IMAT))  !convert IMAT in 1..3 -> 1..4
          IF(MID == 0) CYCLE
            off(1) = one
            mat_(1) = mid
            eint(1) = E0_(ISUB)
            rho(1)  = RHO0_(ISUB)
            rho_0(1) = RHO0_(ISUB)
            mu(1)   = ZERO
            mu2(1)  = ZERO
            espe(1) = E0_(ISUB)
            DVOL(1) = ZERO
            DF(1)   = ONE
            VOL(1)  = ONE !iflag=2 in EOSMAIN : vnew > 0.
            PRES(1) = ZERO
            SSP(1)  = ZERO
            DPDE(1) = ZERO
            DPDM(1) = ZERO
            THETA(1)= T0_(ISUB)
            SIGOLD(1,1:3) = - MAT_PARAM(MAT(1))%MULTIMAT%pEOS(IMAT)%EOS%P0   !ideal gas vt only
            SIGOLD(1,4:6) = ZERO
            MUOLD(1)      = ZERO
            BURNFRAC(1) = ZERO
            PSH(1) = MAT_PARAM(MAT(1))%MULTIMAT%pEOS(IMAT)%EOS%PSH
            MATLAW = IPM(2,MID)
            pmin = PMIN_(ISUB)
            EOSTYPE = MAT_PARAM(MAT(1))%MULTIMAT%pEOS(IMAT)%EOS%EOSTYPE
            if(MATLAW == 5) EOSTYPE = 15
            vartmp(1,1:nvartmp) = 1
            CALL EOSMAIN(2       , 1          , EOSTYPE   , PM         , OFF  , EINT,
     .                   RHO     , RHO_0      , MU        , MU2        , ESPE ,
     .                   DVOL    , DF         , VOL       , MAT_       , PSH  ,
     .                   PRES    , DPDM       , DPDE      , THETA      ,
     .                   BUFMAT  , SIGOLD     , MUOLD     , MATLAW     ,
     .                   NPF     , TF         , VAREOS    , 6, MAT_PARAM(MID),
     .                   BURNFRAC, NVARTMP    , VARTMP)
            IF(MATLAW ==5) THEN
              !bfrac=0
              DPDM(1) = MAX(PM(44,MID) , DPDM(1))
              PRES(1) = PM(31,MID)
            ENDIF

            IF(RHO0_(ISUB) > ZERO)SSP_(ISUB) = SQRT( ABS(DPDM(1))+TWO*TWO_THIRD*GG_(ISUB)/RHO0_(ISUB) )
            KK = M51_N0PHAS + (ISUB-1)*M51_NVPHAS
            DO I=1,NEL
              UVAR(I,KK+24:KK+24-1+nvareos) = VAREOS(1, 1:6)
            ENDDO
            P0_(ISUB) = PRES(1)
         ENDDO ! next IMAT

      ENDIF

                                                                        
      !===========================!                                     
      ! material IMAT : TIME=0    ! 
      ! default :                 !                                    
      ! if no /INIGRAV option     !                                     
      !===========================!                                     
      DO IMAT=1,4                                                       
        DO I=1,NEL
           VOLD          = GBUF%VOL(I)*VF(IMAT)
           KK            = M51_N0PHAS + (IMAT-1)*M51_NVPHAS
           UVAR(I,1+KK)  = VF(IMAT)
           UVAR(I,2+KK)  = ZERO
           UVAR(I,3+KK)  = ZERO
           UVAR(I,4+KK)  = ZERO
           UVAR(I,5+KK)  = ZERO
           UVAR(I,6+KK)  = ZERO
           UVAR(I,7+KK)  = ZERO
           UVAR(I,8+KK)  = E0_(IMAT)   ! rho.e for ACONVE
           UVAR(I,9+KK)  = RHO0_(IMAT)  ! rho for ACONVE
           UVAR(I,10+KK) = ZERO
           UVAR(I,11+KK) = VOLD
           UVAR(I,12+KK) = RHO0_(IMAT)          ! rho_old IN rho OUT
           UVAR(I,14+KK) = SSP_(IMAT)
           UVAR(I,15+KK) = ZERO                ! Plastic EPS
           UVAR(I,16+KK) = T0_(IMAT)            ! temperature
           UVAR(I,17+KK) = ZERO                ! diffuse heat
           UVAR(I,18+KK) = P0_(IMAT)
           UVAR(I,19+KK) = ZERO
           UVAR(I,20+KK) = RHO0_(IMAT)  !rho0 initial state is element dependent and can be updated by /INIGRAV
           UVAR(I,21+KK) = E0_(IMAT)    !E0
           UVAR(I,22+KK) = ZERO !SSP0 to introduce if needed
           UVAR(I,23+KK) = VF(IMAT)            !alpha0
           !24 : vareos : defined above
        ENDDO
      ENDDO 
      !particular case of submaterial 4 (burning time to initialize)
      IMAT              = 4
      KK                = M51_N0PHAS+(IMAT-1)*M51_NVPHAS
      UVAR(1:NEL,15+KK) = UVAR(1:NEL,1) ! Tdet
      !
      DO I=1,NEL
        PRES_0 = VF(1)*P0_(1)+VF(2)*P0_(2)+VF(3)*P0_(3)+VF(4)*P0_(4)
        UVAR(I,4) = PRES_0
        PM(31,MAT(I)) = PRES_0
        PM(104,MAT(I)) = PRES_0
        SIG(I,1) = -PRES_0      
        SIG(I,2) = -PRES_0      
        SIG(I,3) = -PRES_0                      
      ENDDO

      IF(IS_IFORM12)THEN

        RHO0_MIX = VF(1)*RHO0_(1)+VF(2)*RHO0_(2)+VF(3)*RHO0_(3)+VF(4)*RHO0_(4)
        E0_MIX = VF(1)*E0_(1)+VF(2)*E0_(2)+VF(3)*E0_(3)+VF(4)*E0_(4)
        !Sound Speed
        K_mix = zero
        if (RHO0_(1) > zero .and. SSP_(1) > zero) K_mix = K_mix + VF(1) / ( RHO0_(1) * SSP_(1)**2 )
        if (RHO0_(2) > zero .and. SSP_(2) > zero) K_mix = K_mix + VF(2) / ( RHO0_(2) * SSP_(2)**2 )
        if (RHO0_(3) > zero .and. SSP_(3) > zero) K_mix = K_mix + VF(3) / ( RHO0_(3) * SSP_(3)**2 )
        if (RHO0_(4) > zero .and. SSP_(4) > zero) K_mix = K_mix + VF(4) / ( RHO0_(4) * SSP_(4)**2 )
        If(K_mix > zero)K_mix=one/K_mix
        SSP_MIX=zero
        if(RHO0_MIX > zero) SSP_mix = SQRT(K_mix / RHO0_MIX)
        PM(27,MAT(1)) = SSP_mix

        DO I=1,NEL
          GBUF%RHO(I) = RHO0_MIX
          GBUF%EINT(I) = E0_MIX
          !Temperature
          V1=GBUF%VOL(I)*VF(1)
          V2=GBUF%VOL(I)*VF(2)
          V3=GBUF%VOL(I)*VF(3)
          V4=GBUF%VOL(I)*VF(4)
          DENOM = (SPH1*V1+SPH2*V2+SPH3*V3+SPH4*V4)
          IF(DENOM /= ZERO)THEN
            GBUF%TEMP(I) = (SPH1*V1*T0_(1)+SPH2*V2*T0_(2)+SPH3*V3*T0_(3)+SPH4*V4*T0_(4)) / DENOM
          ENDIF
        ENDDO
      ENDIF

      !---------------------------------!
      ! IFORM 6                         !
      ! Initialize initial def param    !
      !---------------------------------!      
      IF(IFORM == 6) THEN
        NV46 = 4
        NUMEL=NUMELQ+NUMELTG
        IF(N2D==0)THEN
          NV46 = 6
          NUMEL=NUMELS
        ENDIF
        CALL NRF51INI (
     .                  IPM              , PM    , X     , NIX     ,  IX,
     .                  ALE_CONNECTIVITY , BUFMAT, UPARAM, RHO0    ,
     .                  UVAR             , NUVAR , NEL   , GBUF%RHO, NUMEL
     .                )
      ENDIF      
C---
      RETURN
      END SUBROUTINE M51INIT
