!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
      !||====================================================================
      !||    multi_propergol_ebcs   ../engine/source/multifluid/multi_propergol_ebcs.F90
      !||--- called by ------------------------------------------------------
      !||    multi_ebcs             ../engine/source/multifluid/multi_ebcs.F
      !||--- calls      -----------------------------------------------------
      !||    arret                  ../engine/source/system/arret.F
      !||    multi_submatlaw        ../engine/source/multifluid/multi_submatlaw.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod           ../common_source/modules/constant_mod.F
      !||    ebcs_mod               ../common_source/modules/boundary_conditions/ebcs_mod.F90
      !||    matparam_def_mod       ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    multi_fvm_mod          ../common_source/modules/ale/multi_fvm_mod.F90
      !||    th_surf_mod            ../common_source/modules/interfaces/th_surf_mod.F
      !||====================================================================
      SUBROUTINE MULTI_PROPERGOL_EBCS(ITASK, EBCS_ID, MULTI_FVM, NELEM, ELEM_LIST, FACE_LIST, &
                                FVM_INLET_DATA, IXS, IXQ, IXTG, XGRID, WGRID, IPM, PM, FUNC_VALUE, &
                                EBCS,NPF,TF,FSAVSURF,TIMESTEP, NIXS, NIXQ, NIXTG, NPROPMI, NPROPM, NSURF, STF, SNPC, &
                                NUMELS, NUMELQ, NUMELTG, NUMNOD, NCYCLE, NUMMAT, MATPARAM)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      USE MULTI_FVM_MOD
      USE EBCS_MOD
      USE TH_SURF_MOD , only : TH_SURF_NUM_CHANNEL
      USE CONSTANT_MOD , ONLY : EM20, EP20, ONE, ZERO, HALF
      USE MATPARAM_DEF_MOD , ONLY :  MATPARAM_STRUCT_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Include files
! ----------------------------------------------------------------------------------------------------------------------
#include      "my_real.inc"
#include      "task_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      INTEGER,INTENT(IN) :: NIXS, NIXQ, NIXTG, NPROPMI, NPROPM, NSURF, STF, SNPC, NUMMAT !< array size
      INTEGER,INTENT(IN) :: NUMELS, NUMELQ, NUMELTG, NUMNOD !< array size
      INTEGER,INTENT(IN) :: NCYCLE
      my_real,INTENT(INOUT) :: FSAVSURF(TH_SURF_NUM_CHANNEL,NSURF)
      INTEGER, INTENT(IN) :: EBCS_ID
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      INTEGER, INTENT(IN) :: ITASK, NELEM, ELEM_LIST(NELEM), FACE_LIST(NELEM)
      INTEGER, INTENT(IN) :: IXS(NIXS, NUMELS), IXQ(NIXQ, NUMELQ), IXTG(NIXTG, NUMELTG)
      my_real, INTENT(IN) :: XGRID(3, NUMNOD), WGRID(3, NUMNOD)
      INTEGER, INTENT(IN) :: IPM(NPROPMI, NUMMAT)
      my_real, INTENT(IN) :: PM(NPROPM, NUMMAT), FUNC_VALUE(*)
      TYPE(FVM_INLET_DATA_STRUCT), INTENT(IN) :: FVM_INLET_DATA
      TYPE(t_ebcs_nrf), INTENT(INOUT) :: EBCS
      INTEGER, INTENT(IN) :: NPF(SNPC)
      my_real, INTENT(IN) :: TF(STF), TIMESTEP
      TYPE(MATPARAM_STRUCT_), DIMENSION(NUMMAT), INTENT(IN) :: MATPARAM !material data structure
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
      INTEGER :: IELEM, ELEMID, NODE_ID
      INTEGER :: KFACE, NB_NOD_FOUND, KNOD, KK, JJ
      my_real :: X1(3), X2(3), X3(3), X4(3), P, NORMVEL, VX, VY, VZ, SSP, SURF, NX, NY, NZ
      my_real :: RHO, SSTAR, PSTAR, PFAC, SL, SR, WFAC(3), VII(5),  VEL2, NORMALW
      my_real :: FII(5), FJJ(5), VIISTAR(5),  FIISTAR(5), FJJSTAR(5), PP(5)
      INTEGER :: NODE1, NODE2, IMAT, NBMAT
      INTEGER :: MATLAW(MULTI_FVM%NBMAT), LOCAL_MATID(MULTI_FVM%NBMAT)
      my_real :: PHASE_RHOII(MULTI_FVM%NBMAT), PHASE_PRESII(MULTI_FVM%NBMAT)
      my_real :: PHASE_EINTII(MULTI_FVM%NBMAT), PHASE_SSPII(MULTI_FVM%NBMAT)
      my_real :: PHASE_ALPHAII(MULTI_FVM%NBMAT), PHASE_RHOJJ(MULTI_FVM%NBMAT)
      my_real :: PHASE_PRESJJ(MULTI_FVM%NBMAT), PHASE_EINTJJ(MULTI_FVM%NBMAT)
      my_real :: PHASE_SSPJJ(MULTI_FVM%NBMAT), PHASE_ALPHAJJ(MULTI_FVM%NBMAT)
      my_real :: DUMMY(6), RHOII, PII, EINTII, VXII, VYII, VZII, SSPII, NORMAL_VELII, RHOJJ, SSPJJ
      my_real :: P_JJ, NORMAL_VELJJ, VXJJ, VYJJ, VZJJ, VELII2, ALPHAII, SUB_RHOII, SUB_RHOEINTII, SUB_VIISTAR(3)
      my_real :: SUB_FIISTAR(3), ALPHASTAR, SUB_RHOSTAR, SUB_PII, VELJJ2, SUB_ESTAR, EINTJJ
      INTEGER :: IELEM_START, IELEM_END, ID_SURF
      my_real :: TCAR_P, TCAR_VF,ALPHA,BETA,DP0,Vnew,Vold,Pvois,POld,MACH,RHOC2,ROC,PSURF
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      TCAR_P = EBCS%TCAR_P
      TCAR_VF = EBCS%TCAR_VF
      ID_SURF = EBCS%SURF_ID

      if(1==2)print *, XGRID(1,1),WGRID(1,1),EBCS_ID,func_value(1)
      
      ALPHA = TIMESTEP/TCAR_P
      BETA = TIMESTEP/MAX(TIMESTEP,TCAR_VF)
      IF(TCAR_VF>=EP20)BETA=ZERO
      IF(TIMESTEP == ZERO)THEN
        ALPHA = ONE !zero
        BETA = ONE
      ENDIF
      
      
      NBMAT = MULTI_FVM%NBMAT
      IELEM_START = 1 + ITASK * NELEM / NTHREAD 
      IELEM_END = (1 + ITASK) * NELEM / NTHREAD
      DUMMY(1:6)=ZERO
      DO IELEM = IELEM_START, IELEM_END
         ELEMID = ELEM_LIST(IELEM)
         IF (ELEMID  <=  NUMELS) THEN
            DO IMAT = 1, NBMAT
               LOCAL_MATID(IMAT) = IPM(20 + IMAT, IXS(1, ELEMID))
               MATLAW(IMAT) = IPM(2, LOCAL_MATID(IMAT))
            ENDDO
         ELSE IF (ELEMID  <=  NUMELS + NUMELQ) THEN
            DO IMAT = 1, NBMAT
               LOCAL_MATID(IMAT) = IPM(20 + IMAT, IXQ(1, ELEMID))
               MATLAW(IMAT) = IPM(2, LOCAL_MATID(IMAT))
            ENDDO
         ELSE
            DO IMAT = 1, NBMAT
               LOCAL_MATID(IMAT) = IPM(20 + IMAT, IXTG(1, ELEMID))
               MATLAW(IMAT) = IPM(2, LOCAL_MATID(IMAT))
            ENDDO
         ENDIF

!     Face of the element
         KFACE = FACE_LIST(IELEM)
         
         NX = MULTI_FVM%FACE_DATA%NORMAL(1, KFACE, ELEMID)
         NY = MULTI_FVM%FACE_DATA%NORMAL(2, KFACE, ELEMID)
         NZ = MULTI_FVM%FACE_DATA%NORMAL(3, KFACE, ELEMID)
         SURF = MULTI_FVM%FACE_DATA%SURF(KFACE, ELEMID)
         WFAC(1:3) = MULTI_FVM%FACE_DATA%WFAC(1:3, KFACE, ELEMID) 
!     Normal grid velocity
         NORMALW = WFAC(1) * NX + WFAC(2) * NY + WFAC(3) * NZ   

!     Current element
         RHOII = MULTI_FVM%RHO(ELEMID)
         PII = MULTI_FVM%PRES(ELEMID)
         EINTII = MULTI_FVM%EINT(ELEMID)
         VXII = MULTI_FVM%VEL(1, ELEMID)
         VYII = MULTI_FVM%VEL(2, ELEMID)
         VZII = MULTI_FVM%VEL(3, ELEMID)
         VELII2 = VXII**2 + VYII**2 + VZII**2
         IF (NBMAT  >  1) THEN
           DO IMAT = 1, NBMAT
             PHASE_ALPHAII(IMAT) = MULTI_FVM%PHASE_ALPHA(IMAT, ELEMID)
             PHASE_EINTII(IMAT) = MULTI_FVM%PHASE_EINT(IMAT, ELEMID)
             PHASE_RHOII(IMAT) = MULTI_FVM%PHASE_RHO(IMAT, ELEMID)
             PHASE_PRESII(IMAT) = MULTI_FVM%PHASE_PRES(IMAT, ELEMID)
           ENDDO
         ELSE
           PHASE_ALPHAII(1) = ONE
           PHASE_EINTII(1) = EINTII
           PHASE_RHOII(1) = RHOII
           PHASE_PRESII(1) = PII
         ENDIF
         SSPII = MULTI_FVM%SOUND_SPEED(ELEMID)     
         NORMAL_VELII = VXII * NX + VYII * NY + VZII * NZ
         
!     Boundary "GHOST" element
         RHOJJ = ZERO
         DO IMAT = 1, NBMAT
            PHASE_RHOJJ(IMAT) = FVM_INLET_DATA%VAL_RHO(IMAT)
               PHASE_RHOJJ(IMAT) = PHASE_RHOII(IMAT)
            PHASE_ALPHAJJ(IMAT) = FVM_INLET_DATA%VAL_ALPHA(IMAT)
               PHASE_ALPHAJJ(IMAT) = PHASE_ALPHAII(IMAT)
            RHOJJ = RHOJJ + PHASE_RHOJJ(IMAT) * PHASE_ALPHAJJ(IMAT)
         ENDDO

!     VE formulation
            SSPJJ = ZERO
            P_JJ = ZERO
            EINTJJ = ZERO
            DO IMAT = 1, NBMAT
               PHASE_EINTJJ(IMAT) = FVM_INLET_DATA%VAL_PRES(IMAT)
               PHASE_EINTJJ(IMAT) = PHASE_EINTII(IMAT)
               IF (PHASE_ALPHAJJ(IMAT)  >  ZERO) THEN
                 CALL MULTI_SUBMATLAW( &
                        0,                  MATLAW(IMAT),                 LOCAL_MATID(IMAT),       1, &
                        PHASE_EINTJJ(IMAT), PHASE_PRESJJ(IMAT),           PHASE_RHOJJ(IMAT),       PHASE_SSPJJ(IMAT), &
                        ONE,                DUMMY,                        PM,                      IPM, &
                        NPROPM,             NPROPMI,                      DUMMY,                   ONE, &
                        DUMMY,              MULTI_FVM%BFRAC(IMAT,ELEMID), MULTI_FVM%TBURN(ELEMID), DUMMY, &
                        DUMMY,              DUMMY,                        NPF,                     TF, &
                        DUMMY,              1,                            MATPARAM(LOCAL_MATID(IMAT)))
                 SSPJJ = SSPJJ + PHASE_ALPHAJJ(IMAT) * PHASE_RHOJJ(IMAT) *  MAX(EM20, PHASE_SSPJJ(IMAT))
                 P_JJ = P_JJ + PHASE_PRESJJ(IMAT) * PHASE_ALPHAJJ(IMAT)
                 EINTJJ = EINTJJ + PHASE_ALPHAJJ(IMAT) * PHASE_EINTJJ(IMAT)
               ENDIF
            ENDDO           

            Vnew = NORMAL_VELII
            Pvois = P_JJ
            MACH = ABS(NORMAL_VELII / SSPII)
            Pold = EBCS%pold(IELEM)
            DP0 = EBCS%DP0(IELEM)
            IF(NCYCLE == 1)Pold=Pvois+DP0
            Vold = EBCS%vold(IELEM)
            IF(NCYCLE == 1) Vold=NORMAL_VELII
            EBCS%vold(IELEM) = Vnew
            
            IF(MACH >= ONE .AND. Vnew > ZERO)THEN
              !vitesse sortante supersonique : etat = etat voisin
              PP = Pvois
            ELSE
              RHOC2 = RHOJJ*SSPII*SSPII 
              ROC   = SQRT(RHOJJ*RHOC2)
              P_JJ  = ONE/(ONE+ALPHA)*(Pold+ROC*(Vnew-Vold))+ALPHA*(Pvois+DP0)/(ALPHA + ONE)
            ENDIF

            EBCS%pold(IELEM) = P_JJ
            
            IF (SSPJJ / RHOJJ  >  ZERO) THEN
               SSPJJ = SQRT(SSPJJ / RHOJJ)
            ELSE
               SSPJJ = MULTI_FVM%SOUND_SPEED(ELEMID)
            ENDIF

           !velocity continuity
            VXJJ = FVM_INLET_DATA%VAL_VEL(1)
               VXJJ = VXII
            VYJJ = FVM_INLET_DATA%VAL_VEL(2)
               VYJJ = VYII
            VZJJ = FVM_INLET_DATA%VAL_VEL(3)
               VZJJ = VZII
            NORMAL_VELJJ = VXJJ * NX + VYJJ * NY + VZJJ * NZ

         VELJJ2 = VXJJ**2 + VYJJ**2 + VZJJ**2
!     HLL wave speed estimates
         SL = MIN(NORMAL_VELII - SSPII, NORMAL_VELJJ - SSPJJ)
         SR = MAX(NORMAL_VELII + SSPII, NORMAL_VELJJ + SSPJJ)

!     Intermediate wave speed
         SSTAR = P_JJ - PII + RHOII * NORMAL_VELII * (SL - NORMAL_VELII) - RHOJJ * NORMAL_VELJJ * (SR - NORMAL_VELJJ)
         SSTAR = SSTAR / (RHOII * (SL - NORMAL_VELII) - RHOJJ * (SR - NORMAL_VELJJ))

         PSTAR = PII + RHOII * (SSTAR - NORMAL_VELII) * (SL - NORMAL_VELII)
         PP(1) = ZERO
         PP(2) = PSTAR * NX
         PP(3) = PSTAR * NY
         PP(4) = PSTAR * NZ   
         PP(5) = SSTAR * PSTAR

         IF (SL  >  NORMALW) THEN
            VII(1) = RHOII
            VII(2) = RHOII * VXII
            VII(3) = RHOII * VYII
            VII(4) = RHOII * VZII
            VII(5) = EINTII + HALF * RHOII * VELII2
!     Normal physical flux current element
            FII(1) = VII(1) * NORMAL_VELII
            FII(2) = VII(2) * NORMAL_VELII + PII * NX
            FII(3) = VII(3) * NORMAL_VELII + PII * NY
            FII(4) = VII(4) * NORMAL_VELII + PII * NZ
            FII(5) = (VII(5) + PII) * NORMAL_VELII
!     /th/surf : pressure
            PSURF = PII
!     Take the fluxes of cell II
!     ===
!     Global fluxes
            MULTI_FVM%FLUXES(1:5, KFACE, ELEMID) = (FII(1:5) - NORMALW * VII(1:5)) * SURF
            MULTI_FVM%FLUXES(6, KFACE, ELEMID) = NORMAL_VELII * SURF
!     ===
!     Submaterial fluxes
            IF (NBMAT  >  1) THEN
               DO IMAT = 1, NBMAT
                  ALPHAII = PHASE_ALPHAII(IMAT)
                  SUB_RHOII = PHASE_RHOII(IMAT) ! ALPHA_RHO
                  SUB_RHOEINTII = PHASE_EINTII(IMAT)
                  SUB_VIISTAR(1) = ALPHAII
                  SUB_VIISTAR(2) = ALPHAII * SUB_RHOII ! ALPHA_RHO
                  SUB_VIISTAR(3) = ALPHAII * SUB_RHOEINTII
                  SUB_FIISTAR(1:3) = SUB_VIISTAR(1:3) * NORMAL_VELII
                  MULTI_FVM%SUBVOL_FLUXES(IMAT, KFACE, ELEMID) =  (SUB_FIISTAR(1) - NORMALW * SUB_VIISTAR(1)) * SURF
                  MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE, ELEMID) = (SUB_FIISTAR(2) - NORMALW * SUB_VIISTAR(2)) * SURF
                  MULTI_FVM%SUBENER_FLUXES(IMAT, KFACE, ELEMID) = (SUB_FIISTAR(3) - NORMALW * SUB_VIISTAR(3)) * SURF
               ENDDO
            ENDIF
!     
         ELSEIF (SL  <=  NORMALW .AND. NORMALW  <=  SSTAR) THEN
            VII(1) = RHOII
            VII(2) = RHOII * VXII
            VII(3) = RHOII * VYII
            VII(4) = RHOII * VZII
            VII(5) = EINTII + HALF * RHOII * VELII2
!     Normal physical flux current element
            FII(1) = VII(1) * NORMAL_VELII
            FII(2) = VII(2) * NORMAL_VELII + PII * NX
            FII(3) = VII(3) * NORMAL_VELII + PII * NY
            FII(4) = VII(4) * NORMAL_VELII + PII * NZ
            FII(5) = (VII(5) + PII) * NORMAL_VELII
!     /th/surf : pressure
            PSURF = PII
!     Take intermediate state flux (HLLC scheme)
!     ===
!     Global fluxes
            VIISTAR(1:5) = FII(1:5) - (SL) * VII(1:5) - PP(1:5)
            VIISTAR(1:5) = VIISTAR(1:5) / (SSTAR - SL)
            FIISTAR(1:5) = VIISTAR(1:5) * SSTAR + PP(1:5) 
            MULTI_FVM%FLUXES(1:5, KFACE, ELEMID) = (FIISTAR(1:5) - NORMALW * VIISTAR(1:5)) * SURF
            MULTI_FVM%FLUXES(6, KFACE, ELEMID) = SSTAR * SURF
!     ===
!     Submaterial fluxes
            IF (NBMAT  >  1) THEN
               DO IMAT = 1, NBMAT
                  MATLAW(IMAT) = IPM(2, LOCAL_MATID(IMAT))
                  ALPHASTAR = PHASE_ALPHAII(IMAT)
                  SUB_RHOSTAR = PHASE_RHOII(IMAT) * (NORMAL_VELII - SL) / (SSTAR - SL)
                  IF (ALPHASTAR  >  ZERO) THEN
                     SUB_RHOII = PHASE_RHOII(IMAT)
                     SUB_RHOEINTII = PHASE_EINTII(IMAT)
                     SUB_PII = PHASE_PRESII(IMAT)
                     SUB_ESTAR = PHASE_EINTII(IMAT) / PHASE_RHOII(IMAT) - &
                          PHASE_PRESII(IMAT) * (ONE / SUB_RHOSTAR - ONE / PHASE_RHOII(IMAT))

                     IF (SUB_ESTAR  <  ZERO) THEN
                        SUB_ESTAR = ZERO
                     ENDIF
                  ELSE
                     SUB_ESTAR = ZERO
                  ENDIF
                  SUB_VIISTAR(1) = ALPHASTAR
                  SUB_VIISTAR(2) = ALPHASTAR * SUB_RHOSTAR
                  SUB_VIISTAR(3) = ALPHASTAR * SUB_RHOSTAR * SUB_ESTAR

                  SUB_FIISTAR(1:3) = SUB_VIISTAR(1:3) * SSTAR
                  MULTI_FVM%SUBVOL_FLUXES(IMAT, KFACE, ELEMID) = (SUB_FIISTAR(1) - NORMALW * SUB_VIISTAR(1)) * SURF
                  MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE, ELEMID) = (SUB_FIISTAR(2) - NORMALW * SUB_VIISTAR(2)) * SURF
                  MULTI_FVM%SUBENER_FLUXES(IMAT, KFACE, ELEMID) = (SUB_FIISTAR(3) - NORMALW * SUB_VIISTAR(3)) * SURF
               ENDDO
            ENDIF
            
         ELSE IF (SSTAR  <  NORMALW .AND. NORMALW  <=  SR) THEN
            VII(1) = RHOJJ
            VII(2) = RHOJJ * VXJJ
            VII(3) = RHOJJ * VYJJ
            VII(4) = RHOJJ * VZJJ
            VII(5) = EINTJJ + HALF * RHOJJ * VELJJ2
!     Normal physical flux current element
            FII(1) = VII(1) * NORMAL_VELJJ
            FII(2) = VII(2) * NORMAL_VELJJ + P_JJ * NX
            FII(3) = VII(3) * NORMAL_VELJJ + P_JJ * NY
            FII(4) = VII(4) * NORMAL_VELJJ + P_JJ * NZ
            FII(5) = (VII(5) + P_JJ) * NORMAL_VELJJ
!     /th/surf : pressure
            PSURF = P_JJ
!     Take intermediate state flux (HLLC scheme)
!     ===
!     Global fluxes
            VIISTAR(1:5) = FII(1:5) - (SR) * VII(1:5) - PP(1:5)
            VIISTAR(1:5) = VIISTAR(1:5) / (SSTAR - SR)
            FIISTAR(1:5) = VIISTAR(1:5) * SSTAR + PP(1:5) 
            MULTI_FVM%FLUXES(1:5, KFACE, ELEMID) =  (FIISTAR(1:5) - NORMALW * VIISTAR(1:5)) * SURF
            MULTI_FVM%FLUXES(6, KFACE, ELEMID) = SSTAR * SURF
!     ===
!     Submaterial fluxes
            IF (NBMAT  >  1) THEN
               DO IMAT = 1, NBMAT
                  MATLAW(IMAT) = IPM(2, LOCAL_MATID(IMAT))
                  ALPHASTAR = PHASE_ALPHAJJ(IMAT)
                  SUB_RHOSTAR = PHASE_RHOJJ(IMAT) * (NORMAL_VELJJ - SR) / (SSTAR - SR)
                  IF (ALPHASTAR  >  ZERO) THEN
                     SUB_RHOII = PHASE_RHOJJ(IMAT)
                     SUB_RHOEINTII = PHASE_EINTJJ(IMAT)
                     SUB_PII = PHASE_PRESJJ(IMAT)
                     SUB_ESTAR = PHASE_EINTJJ(IMAT) / PHASE_RHOJJ(IMAT) - &
                          PHASE_PRESJJ(IMAT) * (ONE / SUB_RHOSTAR - ONE / PHASE_RHOJJ(IMAT))
                     IF (SUB_ESTAR  <  ZERO) THEN
                        SUB_ESTAR = ZERO
                     ENDIF
                  ELSE
                     SUB_ESTAR = ZERO
                  ENDIF

                  SUB_VIISTAR(1) = PHASE_ALPHAJJ(IMAT)
                  SUB_VIISTAR(2) = PHASE_ALPHAJJ(IMAT) * SUB_RHOSTAR
                  SUB_VIISTAR(3) = PHASE_ALPHAJJ(IMAT) * SUB_RHOSTAR * SUB_ESTAR
                  SUB_FIISTAR(1:3) = SUB_VIISTAR(1:3) * SSTAR
                  MULTI_FVM%SUBVOL_FLUXES(IMAT, KFACE, ELEMID) = (SUB_FIISTAR(1) - NORMALW * SUB_VIISTAR(1)) * SURF
                  MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE, ELEMID) = (SUB_FIISTAR(2) - NORMALW * SUB_VIISTAR(2)) * SURF
                  MULTI_FVM%SUBENER_FLUXES(IMAT, KFACE, ELEMID) = (SUB_FIISTAR(3) - NORMALW * SUB_VIISTAR(3)) * SURF
               ENDDO
            ENDIF
         ELSE IF (SR  <  NORMALW) THEN
            VII(1) = RHOJJ
            VII(2) = RHOJJ * VXJJ
            VII(3) = RHOJJ * VYJJ
            VII(4) = RHOJJ * VZJJ
            VII(5) = EINTJJ + HALF * RHOJJ * VELJJ2
!     Normal physical flux current element
            FII(1) = VII(1) * NORMAL_VELJJ
            FII(2) = VII(2) * NORMAL_VELJJ + P_JJ * NX
            FII(3) = VII(3) * NORMAL_VELJJ + P_JJ * NY
            FII(4) = VII(4) * NORMAL_VELJJ + P_JJ * NZ
            FII(5) = (VII(5) + P_JJ) * NORMAL_VELJJ
!     /th/surf : pressure
            PSURF = P_JJ
!     Take the fluxes of cell II
!     ===
!     Global fluxes
            MULTI_FVM%FLUXES(1:5, KFACE, ELEMID) = (FII(1:5) - NORMALW * VII(1:5)) * SURF
            MULTI_FVM%FLUXES(6, KFACE, ELEMID) = NORMAL_VELJJ * SURF
!     ===
!     Submaterial fluxes
            IF (NBMAT  >  1) THEN
               DO IMAT = 1, NBMAT
                  ALPHAII = PHASE_ALPHAJJ(IMAT)
                  SUB_RHOII = PHASE_RHOJJ(IMAT) ! ALPHA_RHO
                  SUB_RHOEINTII = PHASE_EINTJJ(IMAT)
                  SUB_VIISTAR(1) = ALPHAII
                  SUB_VIISTAR(2) = ALPHAII * SUB_RHOII ! ALPHA_RHO
                  SUB_VIISTAR(3) = ALPHAII * SUB_RHOEINTII
                  SUB_FIISTAR(1:3) = SUB_VIISTAR(1:3) * NORMAL_VELJJ
                  MULTI_FVM%SUBVOL_FLUXES(IMAT, KFACE, ELEMID) = (SUB_FIISTAR(1) - NORMALW * SUB_VIISTAR(1)) * SURF
                  MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE, ELEMID) = (SUB_FIISTAR(2) - NORMALW * SUB_VIISTAR(2)) * SURF
                  MULTI_FVM%SUBENER_FLUXES(IMAT, KFACE, ELEMID) = (SUB_FIISTAR(3) - NORMALW * SUB_VIISTAR(3)) * SURF
               ENDDO
            ENDIF

         ELSE
            CALL ARRET(2)
!     
         ENDIF

         ! /TH/SURF (massflow,velocity,pressure,acceleration)
          FSAVSURF(2,ID_SURF) = FSAVSURF(2,ID_SURF) + MULTI_FVM%FLUXES(1, KFACE, ELEMID)            ! += rho.S.un
          FSAVSURF(3,ID_SURF) = FSAVSURF(3,ID_SURF) + MULTI_FVM%FLUXES(1, KFACE, ELEMID) / VII(1)   ! += S.un
          FSAVSURF(4,ID_SURF) = FSAVSURF(4,ID_SURF) + SURF*PSURF                                    ! += S.P
          FSAVSURF(6,ID_SURF) = FSAVSURF(6,ID_SURF) + MULTI_FVM%FLUXES(1, KFACE, ELEMID)            ! m <- m+dm (cumulative value)

      ENDDO !next IELEM

!-----------------------------------------------

      END SUBROUTINE MULTI_PROPERGOL_EBCS

