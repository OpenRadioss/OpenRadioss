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
     .               MAT                ,IPARG      ,IFORM   ,IX      ,NIX     ,
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
      my_real :: GG1, GG2, GG3
      my_real :: VOLD,P0_(4),T0_(4),PMIN_(4),E0_(4),RHO0_(4),VF(4),PEXT,PRES_0
      my_real,intent(inout) :: tb(nel)
      my_real,DIMENSION(NEL) :: OFF, EINT, RHO, RHO_0, MU, MU2, ESPE, MUOLD, BURNFRAC
      my_real,DIMENSION(NEL) :: DVOL, DF, VOL, PRES, SSP, DPDE, THETA, PSH
      my_real,DIMENSION(NEL,6) :: SIGOLD
      my_real :: PMIN
      LOGICAL :: IS_IFORM12
      integer, parameter :: nvartmp = 3
      integer, parameter :: nvareos = 6
      my_real,DIMENSION(nel,nvareos) :: VAREOS ! pointer to UVAR
      integer :: vartmp(nel, nvartmp)
C-----------------------------------------------

      IFLG  = NINT(UPARAM(55))
      IFORM = NINT(UPARAM(31))

      IDX_YIELD(1:4) = (/100,150,200,250/)

      !need to initialize element buffer with initial density for this specific case.
      IS_IFORM12 = .FALSE.
      IF(IFORM == 12)THEN
        UPARAM(31) = 1
        !UPARAM(55) = 1
        IFORM = 1
        !IFLG = 1
        IS_IFORM12 = .TRUE.
      ENDIF

      !---------------------------------!
      ! Test if only fluids are defined !
      !---------------------------------!
      GG1 = UPARAM(101)
      GG2 = UPARAM(151)
      GG3 = UPARAM(201)
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
      E0_(1:4)   = (/UPARAM(32:34),UPARAM(48)/)
      T0_(1:4)   = (/UPARAM(113),UPARAM(163),UPARAM(213),UPARAM(263)/)
      RHO0_(1:4) = (/UPARAM(09:11),UPARAM(47)/)
      VF(1:4)    = (/UPARAM(04:06),UPARAM(46)/)
      PEXT       = UPARAM(8)
      P0_(1:4)   = (/UPARAM(57:60)/)
      PMIN_(1:4) = (/UPARAM(39),UPARAM(40),UPARAM(41),UPARAM(56)/)
      T0_(1:4)   = (/UPARAM(IDX_YIELD(1)+13),UPARAM(IDX_YIELD(1)+13),UPARAM(IDX_YIELD(1)+13),UPARAM(IDX_YIELD(1)+13)/)
      IF(IS_IFORM12)THEN

       NBMAT = MAT_PARAM(MAT(1))%MULTIMAT%NB
       VAREOS(1:NEL,1:6) = ZERO

       DO IMAT=1,NBMAT
         DO I=1,NEL
            KK = M51_N0PHAS + (IMAT-1)*M51_NVPHAS
            VAREOS(I, 1:6) = UVAR(I,KK+24:KK-1+nvareos) ! extra eos variables for material imat
         ENDDO
       ENDDO


        DO IMAT=1,NBMAT
          MID = MAT_PARAM(MAT(1))%MULTIMAT%MID(IMAT)
          IF(MID == 0) CYCLE
          DO I=1,NEL
            off(1:nel) = ZERO
            eint(1:nel) = E0_(IMAT)
            rho(1:nel)  = RHO0_(IMAT)
            rho_0(1:nel) = RHO0_(IMAT)
            mu(1:nel)   = ZERO
            mu2(1:nel)  = ZERO
            espe(1:nel) = E0_(IMAT)
            DVOL(1:nel) = ZERO
            DF(1:nel)   = ONE
            VOL(1:nel)  = GBUF%VOL(1:nel)
            PRES(1:nel) = ZERO
            SSP(1:nel)  = ZERO
            DPDE(1:nel) = ZERO
            THETA(1:nel)= T0_(IMAT)
            SIGOLD(1:nel,1:3) = - MAT_PARAM(MAT(1))%MULTIMAT%pEOS(IMAT)%EOS%P0   !ideal gas vt only
            SIGOLD(1:nel,4:6) = ZERO
            MUOLD(1:nel)      = ZERO
            BURNFRAC(1:nel) = ZERO
            PSH(1:NEL) = MAT_PARAM(MAT(1))%MULTIMAT%pEOS(IMAT)%EOS%PSH
            MATLAW = IPM(2,MID)
            pmin = PMIN_(IMAT)
            EOSTYPE = MAT_PARAM(MAT(1))%MULTIMAT%pEOS(IMAT)%EOS%EOSTYPE
            vartmp(1:nel,1:nvartmp) = 1
            CALL EOSMAIN(2       , NEL        , EOSTYPE   , PM     , OFF  , EINT,
     .                   RHO     , RHO_0      , MU        , MU2    , ESPE ,
     .                   DVOL    , DF         , VOL       , MAT    , PSH  ,
     .                   PRES    , SSP        , DPDE      , THETA  ,
     .                   BUFMAT  , SIGOLD     , MUOLD     , MATLAW ,

     .                   NPF     , TF         , VAREOS    , 6, MAT_PARAM(MID),
     .                   BURNFRAC, NVARTMP    , VARTMP)

          ENDDO ! next I
        ENDDO ! next IMAT
        DO IMAT=1,NBMAT
          DO I=1,NEL
             KK = M51_N0PHAS + (IMAT-1)*M51_NVPHAS
             UVAR(I,KK+24:KK-1+nvareos) = VAREOS(I, 1:6)
          ENDDO
        ENDDO
        DO IMAT=NBMAT+1,4
          DO I=1,NEL
             KK = M51_N0PHAS + (IMAT-1)*M51_NVPHAS
             UVAR(I,KK+24:KK-1+nvareos) = ZERO
          ENDDO
        ENDDO
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
           UVAR(I,15+KK) = ZERO                ! Plastic EPS
           UVAR(I,16+KK) = T0_(IMAT)            ! temperature
           UVAR(I,17+KK) = ZERO                ! diffuse heat
           UVAR(I,18+KK) = P0_(IMAT)
           UVAR(I,19+KK) = ZERO
           UVAR(I,20+KK) = RHO0_(IMAT)  !rho0 initial state is element dependent and can be updated by /INIGRAV
           UVAR(I,21+KK) = E0_(IMAT)    !E0
           UVAR(I,22+KK) = ZERO !SSP0 to introduce if needed
           UVAR(I,23+KK) = VF(IMAT)            !alpha0
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
        DO I=1,NEL
          GBUF%RHO(I) = VF(1)*RHO0_(1)+VF(2)*RHO0_(2)+VF(3)*RHO0_(3)+VF(4)*RHO0_(4)
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
