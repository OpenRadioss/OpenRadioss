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
     .               LBUF               ,GBUF       ,NEL     ,SIG     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
      USE DETONATORS_MOD
      USE ALE_CONNECTIVITY_MOD
      USE MULTIMAT_PARAM_MOD , ONLY : M51_N0PHAS, M51_NVPHAS
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
      INTEGER IPM(NPROPMI,NUMMAT),MAT(NEL), IPARG(NPARG),IFORM,NIX,IX(NIX,*)
      my_real PM(NPROPM,NUMMAT),UVAR(NEL,NUVAR),UPARAM(*), X(3,NUMNOD), BUFMAT(*), RHO0
      INTEGER,INTENT(IN) :: NEL
      my_real,INTENT(INOUT) :: SIG(NEL,6)
      TYPE(G_BUFEL_), INTENT(INOUT),TARGET :: GBUF
      TYPE(L_BUFEL_), INTENT(INOUT),TARGET :: LBUF
      TYPE(DETONATORS_STRUCT_) :: DETONATORS
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NUVAR, ISFLUID
      INTEGER NPH,IFLG,NV46
      INTEGER IMAT
      INTEGER KK,NUMEL
      my_real VOLD,T0_(4),C0_(4),C1_(4),C2_(4),C3_(4),C4_(4),C5_(4),E0_(4),RHO0_(4),VF(4),P0(4),PEXT,PRES_0
      my_real,intent(inout) :: TB(NEL)
      my_real :: SPH1,SPH2,SPH3,SPH4
      my_real :: V1,V2,V3,V4,DENOM
      my_real :: GG1,GG2,GG3,GG4
      my_real :: DPDM(4),SSP(4),K_mix
      LOGICAL IS_IFORM12
C-----------------------------------------------

      IFLG  = NINT(UPARAM(55))
      IFORM = NINT(UPARAM(31))
      SPH1  = UPARAM(112)
      SPH2  = UPARAM(162)
      SPH3  = UPARAM(212)
      SPH4  = UPARAM(262)

      !need to initialize element buffer with initial density for this specific case.
      IS_IFORM12 = .FALSE.
      IF(IFORM == 12)THEN
        UPARAM(31) = 1
        UPARAM(55) = 1
        IFORM = 1
        IFLG = 1
        IS_IFORM12 = .TRUE.
      ENDIF

      !---------------------------------!
      ! Test if only fluids are defined !
      !---------------------------------!
      GG1 = UPARAM(101)
      GG2 = UPARAM(151)
      GG3 = UPARAM(201)
      GG4 = ZERO
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
        IF(N2D==0)THEN
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
      E0_(1:4)  = (/UPARAM(32:34),UPARAM(48)/)                                
      T0_(1:4)  = (/UPARAM(113),UPARAM(163),UPARAM(213),UPARAM(263)/)   
      RHO0_(1:4)= (/UPARAM(09:11),UPARAM(47)/)                          
      VF(1:4)   = (/UPARAM(04:06),UPARAM(46)/)                          
      PEXT      = UPARAM(8)                                             
      P0(1)     = C0_(1)+C4_(1)*E0_(1)                                  
      P0(2)     = C0_(2)+C4_(2)*E0_(2)                                  
      P0(3)     = C0_(3)+C4_(3)*E0_(3)                                  
      P0(4)     = C0_(4)
      DPDM(1)   = C1_(1)+C5_(1)*E0_(1)+P0(1)*C4_(1)
      DPDM(2)   = C1_(2)+C5_(2)*E0_(2)+P0(2)*C4_(2)
      DPDM(3)   = C1_(3)+C5_(3)*E0_(3)+P0(3)*C4_(3)
      DPDM(4)   = C1_(4)+C5_(4)*E0_(4)+P0(4)*C4_(4)
      SSP(1:4)=ZERO
      IF(RHO0_(1) > ZERO)SSP(1) = SQRT( (ABS(DPDM(1))+TWO*TWO_THIRD*GG1)/RHO0_(1) )
      IF(RHO0_(2) > ZERO)SSP(2) = SQRT( (ABS(DPDM(2))+TWO*TWO_THIRD*GG2)/RHO0_(2) )
      IF(RHO0_(3) > ZERO)SSP(3) = SQRT( (ABS(DPDM(3))+TWO*TWO_THIRD*GG3)/RHO0_(3) )
      IF(RHO0_(4) > ZERO)SSP(4) = SQRT( (ABS(DPDM(4))+TWO*TWO_THIRD*GG4)/RHO0_(4) )

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
        UVAR(I,14+KK) = SSP(IMAT)
        UVAR(I,15+KK) = ZERO                ! Plastic EPS             
        UVAR(I,16+KK) = T0_(IMAT)            ! temperature              
        UVAR(I,17+KK) = ZERO                ! diffuse heat          
        UVAR(I,18+KK) = C0_(IMAT)+C4_(IMAT)*E0_(IMAT)                   
        UVAR(I,19+KK) = ZERO 
        UVAR(I,20+KK) = RHO0_(IMAT)  !rho0 initial state is element dependent and can be updated by /INIGRAV
        UVAR(I,21+KK) = E0_(IMAT)    !E0
        UVAR(I,22+KK) = ZERO !SSP0 to introduce if needed
        UVAR(I,23+KK) = VF(IMAT)                      !alpha0
       ENDDO                                                            
      ENDDO 
      !particular case of submaterial 4 (burning time to initialize)
      IMAT              = 4
      KK                = M51_N0PHAS+(IMAT-1)*M51_NVPHAS
      UVAR(1:NEL,15+KK) = UVAR(1:NEL,1) ! Tdet
      !
      DO I=1,NEL
        P0(1)     = C0_(1)+C4_(1)*E0_(1)                                  
        P0(2)     = C0_(2)+C4_(2)*E0_(2)                                  
        P0(3)     = C0_(3)+C4_(3)*E0_(3)                                  
        P0(4)     = C0_(4)  
        PRES_0 = VF(1)*P0(1)+VF(2)*P0(2)+VF(3)*P0(3)+VF(4)*P0(4)
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
          GBUF%EINT(I) = VF(1)*E0_(1)+VF(2)*E0_(2)+VF(3)*E0_(3)+VF(4)*E0_(4)
          !Temperature
          V1=GBUF%VOL(I)*VF(1)
          V2=GBUF%VOL(I)*VF(2)
          V3=GBUF%VOL(I)*VF(3)
          V4=GBUF%VOL(I)*VF(4)
          DENOM = (SPH1*V1+SPH2*V2+SPH3*V3+SPH4*V4)
          IF(DENOM /= ZERO)THEN
            GBUF%TEMP(I) = (SPH1*V1*T0_(1)+SPH2*V2*T0_(2)+SPH3*V3*T0_(3)+SPH4*V4*T0_(4)) / DENOM
          ENDIF
          !Sound Speed
          K_mix = zero
          if (RHO0_(1) > zero .and. SSP(1) > zero) K_mix = K_mix + VF(1) / ( RHO0_(1) * SSP(1)**2 )
          if (RHO0_(2) > zero .and. SSP(2) > zero) K_mix = K_mix + VF(2) / ( RHO0_(2) * SSP(2)**2 )
          if (RHO0_(3) > zero .and. SSP(3) > zero) K_mix = K_mix + VF(3) / ( RHO0_(3) * SSP(3)**2 )
          if (RHO0_(4) > zero .and. SSP(4) > zero) K_mix = K_mix + VF(4) / ( RHO0_(4) * SSP(4)**2 )
          If(K_mix > zero)K_mix=one/K_mix
          if(gbuf%rho(i) > zero) LBUF%SSP(I) = SQRT(K_mix / GBUF%RHO(I))
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
