Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    sigeps117   ../engine/source/materials/mat/mat117/sigeps117.F
!||--- called by ------------------------------------------------------
!||    suser43     ../engine/source/elements/solid/sconnect/suser43.F
!||--- calls      -----------------------------------------------------
!||    finter      ../engine/source/tools/curve/finter.F
!||====================================================================
      SUBROUTINE SIGEPS117(
     1     NEL     ,NUPARAM ,NUVAR   ,JSMS    ,TIME    ,TIMESTEP,
     2     UPARAM  ,UVAR    ,AREA    ,OFF     ,OFFL    ,
     3     EPSZZ   ,EPSYZ   ,EPSZX   ,DEPSZZ  ,DEPSYZ  ,DEPSZX  ,
     4     SIGNZZ  ,SIGNYZ  ,SIGNZX  ,STIFM   ,DMELS   ,DMG     ,
     5     IPG     ,NFAIL   ,NGL     ,NFUNC   ,IFUNC   ,NPF      ,TF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C----------------------------------------------------------
#include "units_c.inc"
#include "comlock.inc"
#include "sms_c.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s  
C----------------------------------------------------------
      INTEGER ,INTENT(IN)  :: NEL,NUPARAM,NUVAR,JSMS,IPG
      INTEGER ,INTENT(OUT) :: NFAIL
      INTEGER ,DIMENSION(NEL) ,INTENT(IN)  :: NGL
      my_real ,INTENT(IN)  :: TIME,TIMESTEP
      my_real ,DIMENSION(NEL) :: OFF,OFFL,AREA,DMELS,
     .   EPSZZ,EPSYZ,EPSZX,DEPSZZ,DEPSYZ,DEPSZX,SIGNZZ,SIGNYZ,SIGNZX
      my_real ,DIMENSION(NEL)  ,INTENT(OUT)   :: DMG
      my_real ,DIMENSION(NEL)  ,INTENT(INOUT) :: STIFM
      my_real ,DIMENSION(NUPARAM)   ,INTENT(IN)    :: UPARAM
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: UVAR
C-----------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,II, IRUPT, NINDXF, ICRITTYP, IEVOSHAP, ISYM
      INTEGER ,DIMENSION(NEL)  :: INDXF
      my_real ::   DM,   DAM,  STF,DTB, ALPHA, DENOM1, DENOM2, U_NORM,
     .   E_ELAS_N,DTINV , T_N,T_S, EXP_ALPHA,COEF0, FUNC_ALPHA,
     .   EXP_G   ,     EXP_BK,   DELTA0S_CST, DELTA0N_CST,D_F,
     .   GIC     ,     GIIC  ,  E_ELAS_S,  UND_CST,  UTD_CST,GAMA,
     .   TERM1, TERM2, RATIO, COS2, SIN2, XI, A_COEF, FADE,
     .   EPSMMAX_PREV, DELTA_PREV, DAM_ENR, E_ELAS, DDELTA
c
      my_real, DIMENSION(NEL) :: EPSM,DELTA0M,  BETA, TMAX_MIX,
     .   DELTAFMAX,EPST,FAC1, FAC2,FAC3,EPSMMAX ,EPSN,
     .   DELTA0N,DELTA0S,UND,UTD,DYDX1,DYDX2,LENGTH,TMAX_N,TMAX_S,
     .   GN,GS,G0,GC, DGI, DGII, DGIII, GTOT
      
c-----------------------------------------------

      
C=======================================================================
C     INPUT PARAMETERS INITIALIZATION
C=======================================================================
      
      E_ELAS_N  =  UPARAM(1)  ! Normal elastic stiffness (EN)
      E_ELAS_S  =  UPARAM(2)  ! Tangential elastic stiffness (ET)
      GAMA      =  UPARAM(3)  ! Mode coupling parameter (GAMMA)
      T_N       =  UPARAM(4)  ! Normal stress (TN)
      T_S       =  UPARAM(5)  ! Tangential shear stress (TT)
      IRUPT     =  INT(UPARAM(6)  ) ! Model (1 = Power law, 2 = BK METHOD) (IRUPT)
      GIC       =  UPARAM(7)  ! Critical fracture energy - mode I (GIC)
      GIIC      =  UPARAM(8)  ! Critical fracture energy - mode II (GIIC)
      NFAIL     =  INT(UPARAM(9)) ! IDEL (dft = 1 INT)
      EXP_G     =  UPARAM(10)  
      EXP_BK    =  UPARAM(11) ! Benzeggage-Kenane exponent for the mixed mode (EXP_BK)
      DELTA0N_CST   =  UPARAM(13)        !         DISP puremode N - initial normal displacement
      DELTA0S_CST   =  UPARAM(14)        !         DISP puremode S - initial tangential displacement
      UND_CST       =  UPARAM(15)   ! ultimate displacement in normal direction (UND)
      UTD_CST       =  UPARAM(16)   ! ultimate displacement in tangential direction (UTD)
      
      ICRITTYP  = INT(UPARAM(18)) ! 1=displacement-based, 2=energy-based
      IEVOSHAP  = INT(UPARAM(19)) ! 1=linear, 2=exponential
      ALPHA     = UPARAM(20)
      ISYM      = INT(UPARAM(21)) ! 0=asymmetric (tension only), 1=symmetric tension/compression
c
      DTINV   = ONE / (MAX(TIMESTEP, EM20))
      STF     = E_ELAS_N + E_ELAS_S
      NINDXF  = 0

C=======================================================================
C     UVAR FIELDS DEFINITION
C=======================================================================
c     UVAR(1)  = EPSZZ 
c     UVAR(2)  = EPSZX
c     UVAR(3)  = EPSYZ
c     UVAR(4)  = EPSMMAX
c     UVAR(5)  = DMG
c     UVAR(6)  = SIGNZZ   
c     UVAR(7)  = SIGNZX
c     UVAR(8)  = SIGNYZ 
c     UVAR(9)  = DELTA0M
c     UVAR(10) = BETA
c     UVAR(11) = 
c     UVAR(12) = DELTAFMAX
c     UVAR(13) = GN
c     UVAR(14) = GS
c     UVAR(15) = GC

C=======================================================================
C     INIT FIELDS OR RECOVER HISTORY
C=======================================================================
      IF (TIME == ZERO) THEN 
          DO I=1,NEL
              SIGNZZ(I) = ZERO
              SIGNZX(I) = ZERO
              SIGNYZ(I) = ZERO
              
              EPSMMAX(I) = ZERO
              
              UVAR(I,11) = ZERO
              
              GN(I) = ZERO
              GS(I) = ZERO
              
              DMG(I) = ZERO
          ENDDO
          
      ELSE
          DO I=1,NEL
              SIGNZZ(I) = UVAR(I,6)
              SIGNZX(I) = UVAR(I,7)
              SIGNYZ(I) = UVAR(I,8)
              
              EPSMMAX(I) = UVAR(I,4)
              
              DMG(I) = UVAR(I,5)

              ! Get energy
              GN(I) = UVAR(I,13)
              GS(I) = UVAR(I,14)
          ENDDO
      ENDIF
      
C=======================================================================
C     COMPUTE DELTA KNOWN AS EPSMMAX (i.e. MAX STRAIN)
C=======================================================================
      DO I=1, NEL
          ! Compute norm and tang strain
          IF (ISYM == 1) THEN
              EPSN(I) = ABS(EPSZZ(I)) ! Symmetric: compression drives damage like tension
          ELSE
              EPSN(I) = MAX(EPSZZ(I) , ZERO) ! Ignore compressive state
          ENDIF
          EPST(I) = SQRT(EPSYZ(I)**2  + EPSZX(I)**2) ! Combine both plans
          ! Compute mixt stain state
          EPSM(I) = SQRT( EPSN(I)**2  + EPST(I)**2) ! Mixt strain
          
          ! Compute DELTA_MAX
          EPSMMAX(I) = MAX(EPSM(I), EPSMMAX(I)) ! 
      ENDDO ! I=1,NEL
 
C=======================================================================
C     COMPUTE DELTA0 (INITATION DISPLACMENT) and DELTAFMAX (FAILURE DISPLACEMENT)
C=======================================================================
      ! Init params for DELTA_FAILURE calculation
      IF (IEVOSHAP /= 3) THEN
          EXP_ALPHA  = EXP(-ALPHA)
          DENOM1     = ONE - EXP_ALPHA
          FUNC_ALPHA = (ONE - (ONE + ALPHA) * EXP_ALPHA)/(ALPHA * DENOM1) 
      ENDIF
      
      ! IF Fct_TN !=0 or Fct_TT != 0 starts mesh correction on TN and TT (max traction)
      IF ( IFUNC(1) /= 0.OR. IFUNC(2) /= 0) THEN
          LENGTH(1:NEL) = SQRT(AREA(1:NEL))
      ENDIF
      
      IF ( IFUNC(1) /= 0) THEN
          DO I=1, NEL
             TMAX_N(I) = T_N * FINTER(IFUNC(1),LENGTH(I),NPF,TF,DYDX1(I))
             
             IF (IEVOSHAP == 1) THEN
                 DELTA0N(I) = TMAX_N(I) / E_ELAS_N
                 UND(I) = TWO*GIC/TMAX_N(I)
             ELSE IF (IEVOSHAP == 2) THEN
                 DELTA0N(I) = TMAX_N(I) / E_ELAS_N
                 COEF0  = GIC - TMAX_N(I) * DELTA0N(I) * HALF
                 UND(I) = DELTA0N(I) + COEF0 / (FUNC_ALPHA*TMAX_N(I))
             ELSE IF (IEVOSHAP == 3) THEN
                 DELTA0N(I) = GIC / (EXP(ONE) * TMAX_N(I))
                 UND(I)     = -DELTA0N(I) * LOG(EM04)
             ENDIF
          ENDDO ! I=1,NEL
      ELSE ! No TN correction
          DO I=1,NEL
              DELTA0N(I) =  DELTA0N_CST
              UND(I)     =  UND_CST
              TMAX_N(I)  = T_N
          ENDDO
      ENDIF
      
      IF ( IFUNC(2) /= 0) THEN
          DO I=1,NEL             
              TMAX_S(I) = T_S * FINTER(IFUNC(2),LENGTH(I),NPF,TF,DYDX2(I))
           
              IF (IEVOSHAP == 1) THEN
                  DELTA0S(I) = TMAX_S(I) /E_ELAS_S
                  UTD(I) = TWO*GIIC/TMAX_S(I)
              ELSE IF (IEVOSHAP == 2) THEN
                  DELTA0S(I) = TMAX_S(I) /E_ELAS_S
                  COEF0 = GIIC - TMAX_S(I) * DELTA0S(I) * HALF
                  UTD(I) = DELTA0S(I) + COEF0 / (FUNC_ALPHA*TMAX_S(I)) 
              ELSE IF (IEVOSHAP == 3) THEN
                 DELTA0S(I) = GIIC / (EXP(ONE) * TMAX_S(I))
                 UTD(I)     = -DELTA0S(I) * LOG(EM03)                
              ENDIF
          ENDDO !I=1,NEL
      ELSE      
          DO I=1,NEL             
              DELTA0S(I) = DELTA0S_CST
              UTD(I)     = UTD_CST
              TMAX_S(I)  = T_S
          ENDDO  
      ENDIF
      
      ! Compute DELTA0M (i.e. DELTA0 for the mix mode)
      DO I=1,NEL
          IF (EPST(I) == ZERO) THEN
              ! Pure mode I
              DELTA0M(I)  = DELTA0N(I)
              TMAX_MIX(I) = TMAX_N(I)
              BETA(I) = ZERO
          ELSE IF (EPSN(I) == ZERO) THEN
              ! Pure mode II    
              DELTA0M(I)  = DELTA0S(I)
              TMAX_MIX(I) = TMAX_S(I)
              BETA(I) = ZERO
          ELSE
              BETA(I)     = ABS(EPST(I) / EPSN(I))
              DELTA0M(I)  = DELTA0S(I)* DELTA0N(I)*SQRT((ONE + BETA(I)**2)/
     .                    ((DELTA0S(I)**2) + (BETA(I)* DELTA0N(I))**2))
              ! We should be consistent with the energy G0
              ! Tmax_mix = T_eff = DELTA_0^m * \mathcal{F}_1
              TMAX_MIX(I) = DELTA0M(I) * (E_ELAS_N + E_ELAS_S*BETA(I)**2) /
     .                    (ONE + BETA(I)**2)
          ENDIF
          ENDDO !I=1,NEL
      
      ! Compute DELTA0FMAX
      IF (IEVOSHAP == 3) THEN
          ! Xu-Needleman: delta_f = 9.21 * delta_p_mix (D_crit = 0.9999)
          DO I=1,NEL
              DELTAFMAX(I) = -DELTA0M(I) * LOG(EM04)
          ENDDO
      ELSE IF (IRUPT == 2) THEN ! IRUPT =2  = BK method
          DO I=1, NEL
              IF (EPST(I) == ZERO) THEN
                  DELTAFMAX(I) = UND(I)
              ELSE IF (EPSN(I) == ZERO) THEN
                  DELTAFMAX(I) = UTD(I)
              ELSE ! Mix mode
                  FAC1(I) = (E_ELAS_N**GAMA)/(ONE + BETA(I)**2)
                  FAC2(I) = (E_ELAS_S**GAMA)*(BETA(I)**2)/(ONE + BETA(I)**2)
                  FAC3(I) = (FAC1(I) + FAC2(I) )**(ONE/GAMA)
                  
                  DELTAFMAX(I) = TWO/(DELTA0M(I)*  FAC3(I)   )*
     .                           (GIC + (GIIC - GIC)*((E_ELAS_S*BETA(I)**2)/
     .                           (E_ELAS_N + E_ELAS_S*BETA(I)**2))**ABS(EXP_BK))     
              ENDIF
          ENDDO ! I=1,NEL
      ELSE ! IRUPT =1  = power law method-- default
          DO I=1,NEL
              IF (EPST(I) == ZERO) THEN
                  DELTAFMAX(I)= UND(I)
              ELSE IF (EPSN(I) == ZERO) THEN
                  DELTAFMAX(I)= UTD(I)
              ELSE      ! mixed mode 
                  FAC1(I) = TWO*((ONE+BETA(I)**2))/DELTA0M(I)
            
                  DELTAFMAX(I) = FAC1(I)*((E_ELAS_N/GIC)**EXP_G + 
     .                          (E_ELAS_S*(BETA(I)**2)/GIIC)**EXP_G)**(-ONE/EXP_G)
              ENDIF
          ENDDO ! I=1,NEL
      ENDIF
      
C=======================================================================
C     ENERGY COMPUTATION (ONLY FOR THE ENERGY-BASED CRITERION)
C=======================================================================
      IF (ICRITTYP == 2) THEN
          
          DO I=1, NEL
              ! Compute the elastic-energy
              G0(I) = HALF * TMAX_MIX(I) * DELTA0M(I)
              
              ! Compute the energy-increment with forward Euler             
              DGI(I) = ZERO
              IF (ISYM == 1) THEN
                  DGI(I) = SIGNZZ(I) * DEPSZZ(I)
              ELSE IF (EPSZZ(I) > ZERO) THEN
                  DGI(I) = SIGNZZ(I) * DEPSZZ(I)
              ENDIF
                
              DGII(I) = (SIGNYZ(I)*DEPSYZ(I) + SIGNZX(I)*DEPSZX(I))
                
              ! Update the energy
              GN(I) = MAX(GN(I) + DGI(I), ZERO)
              GS(I) = MAX(GS(I) + DGII(I), ZERO)
              
              ! Compute the total energy
              GTOT(I) = GN(I) + GS(I)
              
              ! Compute RATIO 
              IF (GTOT(I) > ZERO) THEN
                  RATIO = GS(I) / GTOT(I)
              ELSE
                  RATIO = ZERO
              ENDIF
              RATIO = MAX(ZERO, MIN(ONE, RATIO))
              
              ! Critical energy - GC
              IF (IRUPT == 1) THEN ! Power law model
                  !TERM1 = (MAX(EPSN(I),ZERO) / EPSMMAX(I))**(2*EXP_G) * (ONE/GIC) ** EXP_G
                  !TERM2 = (EPST(I)/EPSMMAX(I)) ** (2*EXP_G) * (ONE/GIIC) ** EXP_G
                  
                  !GC(I) = (TERM1 + TERM2)**(-ONE/EXP_G)

                  GC(I) = ( ((ONE - RATIO) / GIC) ** EXP_G + (RATIO/GIIC)**EXP_G)**(-ONE/EXP_G)
              ELSE IF (IRUPT == 2) then ! BK model
                  GC(I) = GIC + (GIIC - GIC) * (RATIO ** EXP_BK)
              ELSE
                  GC(I) = GIC
              ENDIF
              
          ENDDO ! I=1,NEL
      ENDIF ! ICRITTYP == 2
      
C=======================================================================
C     COMPUTE DAMAGE (2nd part of the Traction-separation curve
C=======================================================================    
      IF (ICRITTYP == 1) THEN ! Displacement-based
          DO I=1,NEL
              IF ( EPSMMAX(I) > DELTA0M(I) .AND. EPSMMAX(I) /= 0 .AND. IEVOSHAP /= 3) THEN
                  IF (IEVOSHAP == 1) THEN ! Linear
                      DAM = (DELTAFMAX(I) / EPSMMAX(I)) *
     .                      (EPSMMAX(I)   - DELTA0M(I)) /
     .                  MAX((DELTAFMAX(I) - DELTA0M(I)), EM20)
                  ELSE IF (IEVOSHAP == 2) THEN ! Exponential
                      U_NORM =    (EPSMMAX(I) - DELTA0M(I)) /
     .                      MAX((DELTAFMAX(I) - DELTA0M(I)), EM20)
                      
                      DAM    =          ONE - (DELTA0M(I)/EPSMMAX(I)) *
     .                  (EXP(-ALPHA*U_NORM) - EXP_ALPHA) / DENOM1       
                  ENDIF
                  DMG(I) = MAX(DMG(I), DAM) ! Enforce damage monotonicity
                  DMG(I) = MIN(DMG(I), ONE) ! Saturated at 1 by definition
                  
                  ! Deactivate EF (Displament Criterion)
                  IF (OFFL(I) == ONE.AND.EPSMMAX(I) > DELTAFMAX(I)) THEN
                      NINDXF = NINDXF+1 ! Count
                      INDXF(NINDXF) = I
                      OFFL(I) = ZERO
                  ENDIF
              ENDIF
              
              IF (IEVOSHAP == 3) THEN
                  ! Xu-Needleman: D = 1 - exp(-delta_max / delta_p)
                  ! No initiation threshold (damage starts from delta=0)
                  DAM = ONE - EXP(-EPSMMAX(I) / MAX(DELTA0M(I), EM20))
                  DMG(I) = MAX(DMG(I), DAM)
                  DMG(I) = MIN(DMG(I), ONE)
                  ! Deletion: EPSMMAX > DELTAFMAX (= 6.908 * delta_p)
                  IF (OFFL(I) == ONE .AND. EPSMMAX(I) > DELTAFMAX(I)) THEN
                      NINDXF = NINDXF + 1
                      INDXF(NINDXF) = I
                      OFFL(I) = ZERO
                  ENDIF
              END IF
          ENDDO ! I=1,NEL
      ELSE IF (ICRITTYP == 2) THEN ! Energy-based
          DO I=1,NEL
              
              D_F = TWO * GC(I) / TMAX_MIX(I) ! From the OS doc
              
              IF ( EPSMMAX(I) > DELTA0M(I) .AND. EPSMMAX(I) /= 0 ) THEN
                  IF (IEVOSHAP == 1) THEN ! Linear
                      DAM = (D_F/EPSMMAX(I)) * (EPSMMAX(I) - DELTA0M(I)) /
     .                                            MAX((D_F - DELTA0M(I)), EM20)
                      DMG(I) = MAX(DMG(I), DAM)
                      DMG(I) = MIN(DMG(I), ONE)
                  ELSE IF (IEVOSHAP == 2) THEN ! Exponential law - integration
                      
                      !
                      DENOM2 = MAX(GC(I) - G0(I), EM20)
                      
                      EPSMMAX_PREV = UVAR(I,4)
                      DELTA_PREV   = MAX(EPSMMAX_PREV, DELTA0M(I))
                      
                      IF (EPSMMAX(I) > ZERO) THEN
                          COS2 = EPSN(I)**2 / EPSMMAX(I)**2
                          SIN2 = EPST(I)**2 / EPSMMAX(I)**2
                          E_ELAS = E_ELAS_N * COS2 + E_ELAS_S * SIN2
                      ELSE
                          E_ELAS = E_ELAS_N
                      ENDIF
                      
                      ! Implicit ODE
                      DELTA_PREV = UVAR(I,4)
                      DDELTA = MAX(EPSMMAX(I) - DELTA_PREV, ZERO)
                      IF (DDELTA > ZERO .AND. EPSMMAX(I) > DELTA0M(I)) THEN
                         ! A = K * delta^(n+1) / (Gc^(n+1) - G0) * Ddelta
                         A_COEF = E_ELAS * EPSMMAX(I) * DDELTA
     .                          / MAX(GC(I) - G0(I), EM20)
                         ! Schema implicite exact : D = (D^n + A) / (1 + A)
                         DAM_ENR = (DMG(I) + A_COEF) / (ONE + A_COEF)
                      ELSE
                         DAM_ENR = DMG(I)
                      ENDIF
                      
                      DMG(I)  = MAX(DMG(I), DAM_ENR)
                      DMG(I)  = MIN(DMG(I), ONE)
                  ENDIF
              ENDIF
              
              IF (OFFL(I) == ONE .AND. (GTOT(I) .GE. GC(I) .OR. DMG(I) .GE. ONE )) THEN
                  NINDXF = NINDXF + 1
                  INDXF(NINDXF) = I
                  OFFL(I) = ZERO
              ENDIF 
          ENDDO ! I=1,NEL
      ENDIF ! ICRITTYP == 1 OR 2

C=======================================================================
C     STRESS FIELD UPDATE
C======================================================================= 
      DO I=1,NEL
          ! New field
          IF (ISYM == 1) THEN
              ! Symmetric: damage degrades both tension and compression
              SIGNZZ(I) = (ONE-DMG(I))*E_ELAS_N*EPSZZ(I)
          ELSE IF (EPSZZ(I) < ZERO )  THEN 
              SIGNZZ(I) = E_ELAS_N*EPSZZ(I) 
          ELSE
              SIGNZZ(I) = (ONE-DMG(I))*E_ELAS_N*EPSZZ(I)
          ENDIF
          
          SIGNYZ(I) = (ONE-DMG(I))*E_ELAS_S*EPSYZ(I)
          SIGNZX(I) = (ONE-DMG(I))*E_ELAS_S*EPSZX(I)
          
          ! Smooth C^1 fade-out near deletion (energy-based criterion)
          ! 3-2 Hermite step on D in [D_fade=0.95, D_crit=0.999]
          IF (ICRITTYP == 2 .AND. DMG(I) > ZEP95) THEN
              XI = MIN(MAX((DMG(I) - ZEP95) / (ZEP999 - ZEP95), ZERO), ONE)
              FADE = ONE - XI*XI*(THREE - TWO*XI)
              SIGNZZ(I) = SIGNZZ(I) * FADE
              SIGNYZ(I) = SIGNYZ(I) * FADE
              SIGNZX(I) = SIGNZX(I) * FADE
          ENDIF
      ENDDO ! I=1,NEL
      
C=======================================================================
C     SAVE UVAR FIELDS
C======================================================================= 
      DO I=1, NEL
          UVAR(I,1) = EPSZZ(I) ! Normal strain
          UVAR(I,2) = EPSZX(I) ! Shear strain X
          UVAR(I,3) = EPSYZ(I) ! Shear strain Y
          
          UVAR(I,4) = EPSMMAX(I) ! Maximum strain (history)
          
          UVAR(I,5) = DMG(I)
          
          UVAR(I,6) = SIGNZZ(I) ! Normal stress
          UVAR(I,7) = SIGNZX(I) ! Stress X
          UVAR(I,8) = SIGNYZ(I) ! Stress Y
          UVAR(I,9) = DELTA0M(I) ! delta_mix_0
          UVAR(I,10)= BETA(I) ! Mixed-mode ratio
        
          UVAR(I,12)= DELTAFMAX(I) ! delta_f_max
          
          IF (ICRITTYP == 2) THEN
              UVAR(I,13)= GN(I)
              UVAR(I,14)= GS(I)
              UVAR(I,15)= GC(I)
          ENDIF
      ENDDO ! I=1, NEL
      
c-----------------------------------------------------      
c-----------------------------------------------------      
c     omega = sqrt(2k/2*dmels), dt=2/omega, 2*dmels=dt**2 * 2k / 4
      IF (IDTMINS==2 .AND. JSMS/=0) THEN
          ! DTB = (target dt / scaling factor)**2
          DTB = (DTMINS/DTFACS)**2
          ! For each FE, scale mass by DMELS(I) or HALF*DTB*STF*AREA(I)*OFF(I)
          ! OFF(I) is included to avoid scaling failed elements
          ! HALF*DTB*STF*AREA(I) unit: s2.N/m3 * m2 -> s2 N/m = MASS
          DO I=1,NEL                                                 
              DMELS(I) = MAX(DMELS(I),HALF*DTB*STF*AREA(I)*OFF(I))
          ENDDO                                                        
      END IF
      
    ! Update FE stiffness accounting for cohesive stiffness.
      STIFM(1:NEL) = STIFM(1:NEL) + STF*AREA(1:NEL)*OFF(1:NEL)    
c-----------------------------------------------------     
    ! If elements have failed, print a message.
      IF (NINDXF > 0) THEN
          DO II=1,NINDXF
              I = INDXF(II)
#include "lockon.inc"
              WRITE(IOUT ,1000) NGL(I),IPG,EPSM(I)
              WRITE(ISTDO,1100) NGL(I),IPG,EPSM(I),TIME
#include "lockoff.inc"
          END DO
      END IF
c-----------------------------------------------------      
 1000 FORMAT(5X,'FAILURE COHESIVE ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2,', MIXED MODE STRAIN=',1PE16.9)
 1100 FORMAT(5X,'FAILURE COHESIVE ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2,', MIXED MODE STRAIN=',1PE16.9,
     .          ' AT TIME ',1PE16.9)
c-----------
      RETURN
      END
          