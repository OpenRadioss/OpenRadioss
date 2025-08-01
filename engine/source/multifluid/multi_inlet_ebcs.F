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
!||    multi_inlet_ebcs_mod   ../engine/source/multifluid/multi_inlet_ebcs.F
!||--- called by ------------------------------------------------------
!||    multi_ebcs             ../engine/source/multifluid/multi_ebcs.F
!||====================================================================
      MODULE MULTI_INLET_EBCS_MOD
      CONTAINS
!||====================================================================
!||    multi_inlet_ebcs       ../engine/source/multifluid/multi_inlet_ebcs.F
!||--- called by ------------------------------------------------------
!||    multi_ebcs             ../engine/source/multifluid/multi_ebcs.F
!||--- calls      -----------------------------------------------------
!||    arret                  ../engine/source/system/arret.F
!||    multi_solve_eint       ../engine/source/multifluid/multi_solve_eint.F90
!||    multi_submatlaw        ../engine/source/multifluid/multi_submatlaw.F
!||--- uses       -----------------------------------------------------
!||    matparam_def_mod       ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    multi_fvm_mod          ../common_source/modules/ale/multi_fvm_mod.F90
!||    multi_solve_eint_mod   ../engine/source/multifluid/multi_solve_eint.F90
!||    multi_submatlaw_mod    ../engine/source/multifluid/multi_submatlaw.F
!||    th_surf_mod            ../common_source/modules/interfaces/th_surf_mod.F
!||====================================================================
      SUBROUTINE MULTI_INLET_EBCS(ITASK, EBCS_ID, MULTI_FVM, NELEM, ELEM_LIST, FACE_LIST, 
     .                            FVM_INLET_DATA, IXS, IXQ, IXTG, XGRID, WGRID, IPM, PM,
     .                            FUNC_VALUE, ID_SURF, NPF,TF,FSAVSURF, TIMESTEP, MATPARAM)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE MULTI_FVM_MOD
      USE TH_SURF_MOD , only : TH_SURF_NUM_CHANNEL
      USE MATPARAM_DEF_MOD , ONLY : MATPARAM_STRUCT_
      USE MULTI_SOLVE_EINT_MOD , ONLY : MULTI_SOLVE_EINT
      USE MULTI_SUBMATLAW_MOD , ONLY : MULTI_SUBMATLAW
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------  
! NIXS
#include      "param_c.inc"
! ISPMD
#include      "task_c.inc"
! ALE
! NUMELS, NUMELQ, NUMELTG
#include      "com04_c.inc"
! SNPC,STF
      COMMON /TABLESIZF/ STF,SNPC
      INTEGER STF,SNPC
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: EBCS_ID
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      INTEGER, INTENT(IN) :: ITASK, NELEM, ELEM_LIST(NELEM), FACE_LIST(NELEM)
      INTEGER, INTENT(IN) :: IXS(NIXS, *), IXQ(NIXQ, *), IXTG(NIXTG, *)
      my_real, INTENT(IN) :: XGRID(3, *), WGRID(3, *)
      INTEGER, INTENT(IN) :: IPM(NPROPMI, NUMMAT)
      my_real, INTENT(IN) :: PM(NPROPM, NUMMAT), FUNC_VALUE(*)
      TYPE(FVM_INLET_DATA_STRUCT), INTENT(IN) :: FVM_INLET_DATA
      INTEGER, INTENT(IN) :: NPF(SNPC), ID_SURF
      my_real, INTENT(IN) :: TF(STF), TIMESTEP
      my_real, INTENT(INOUT) :: FSAVSURF(TH_SURF_NUM_CHANNEL,NSURF)
      TYPE(MATPARAM_STRUCT_), DIMENSION(NUMMAT),INTENT(IN) :: MATPARAM !material data structure
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IELEM, ELEMID, NODE_ID
      INTEGER :: KFACE, NB_NOD_FOUND, KK, JJ
      my_real :: X1(3), X2(3), X3(3), X4(3), P, NORMVEL, VX, VY, VZ, SSP, SURF, NX, NY, NZ
      my_real :: RHO, SSTAR, PSTAR, PFAC, SL, SR, WFAC(3), VII(5), VJJ(5), VEL2, NORMALW
      my_real :: FII(5), FJJ(5), VIISTAR(5), VJJSTAR(5), FIISTAR(5), FJJSTAR(5), PP(5)
      INTEGER :: NODE1, NODE2, IMAT, NBMAT
      INTEGER :: MATLAW(MULTI_FVM%NBMAT), LOCAL_MATID(MULTI_FVM%NBMAT)
      my_real :: PHASE_RHOII(MULTI_FVM%NBMAT), PHASE_PRESII(MULTI_FVM%NBMAT), 
     .           PHASE_EINTII(MULTI_FVM%NBMAT), PHASE_SSPII(MULTI_FVM%NBMAT),
     .           PHASE_ALPHAII(MULTI_FVM%NBMAT), PHASE_RHOJJ(MULTI_FVM%NBMAT),
     .           PHASE_PRESJJ(MULTI_FVM%NBMAT), PHASE_EINTJJ(MULTI_FVM%NBMAT),
     .           PHASE_SSPJJ(MULTI_FVM%NBMAT), PHASE_ALPHAJJ(MULTI_FVM%NBMAT)
      my_real :: DUMMY(6), DUMMY2(1), RHOII, PII, EINTII, VXII, VYII, VZII, SSPII, NORMAL_VELII, RHOJJ, SSPJJ,
     .           PJJ, NORMAL_VELJJ, VXJJ, VYJJ, VZJJ, VELII2, ALPHAII, SUB_RHOII, SUB_RHOEINTII, SUB_VIISTAR(3),
     .           SUB_FIISTAR(3), ALPHASTAR, SUB_RHOSTAR, SUB_PII, VELJJ2, SUB_ESTAR, EINTJJ, PSURF
      INTEGER :: IFUNC, IELEM_START, IELEM_END
      my_real :: ABURN(1) !< afterburning
      INTEGER :: VARTMP_EOS(1,128) !nel=1
      INTEGER :: NVARTMP_EOS !upper bound
C-----------------------------------------------
C     B o d y
C-----------------------------------------------
      NVARTMP_EOS = 128
      VARTMP_EOS(1,:) = 1
      NBMAT = MULTI_FVM%NBMAT
      IELEM_START = 1 + ITASK * NELEM / NTHREAD 
      IELEM_END = (1 + ITASK) * NELEM / NTHREAD
      DUMMY(1:6) = ZERO
      DUMMY2(1) = ONE
      ABURN(:) = ZERO
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

C     Face of the element
         KFACE = FACE_LIST(IELEM)
         
         NX = MULTI_FVM%FACE_DATA%NORMAL(1, KFACE, ELEMID)
         NY = MULTI_FVM%FACE_DATA%NORMAL(2, KFACE, ELEMID)
         NZ = MULTI_FVM%FACE_DATA%NORMAL(3, KFACE, ELEMID)
         SURF = MULTI_FVM%FACE_DATA%SURF(KFACE, ELEMID)
         WFAC(1:3) = MULTI_FVM%FACE_DATA%WFAC(1:3, KFACE, ELEMID) 
C     Normal grid velocity
         NORMALW = WFAC(1) * NX + WFAC(2) * NY + WFAC(3) * NZ   

C     Current element
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
         
C     Boundary "GHOST" element
         RHOJJ = ZERO
         DO IMAT = 1, NBMAT
            PHASE_RHOJJ(IMAT) = FVM_INLET_DATA%VAL_RHO(IMAT)
            IFUNC = FVM_INLET_DATA%FUNC_RHO(IMAT)
            IF (IFUNC  >  0) THEN
               PHASE_RHOJJ(IMAT) = PHASE_RHOJJ(IMAT) * FUNC_VALUE(IFUNC)
            ELSEIF (IFUNC  <= 0) THEN
               PHASE_RHOJJ(IMAT) = PHASE_RHOII(IMAT)
            ENDIF
            PHASE_ALPHAJJ(IMAT) = FVM_INLET_DATA%VAL_ALPHA(IMAT)
            IFUNC = FVM_INLET_DATA%FUNC_ALPHA(IMAT)
            IF (IFUNC  >  0) THEN
               PHASE_ALPHAJJ(IMAT) = PHASE_ALPHAJJ(IMAT) * FUNC_VALUE(IFUNC)
            ELSEIF (IFUNC  <= 0) THEN
               PHASE_ALPHAJJ(IMAT) = PHASE_ALPHAII(IMAT)
            ENDIF
            RHOJJ = RHOJJ + PHASE_RHOJJ(IMAT) * PHASE_ALPHAJJ(IMAT)
         ENDDO
         IF (FVM_INLET_DATA%FORMULATION  ==  2) THEN
C     VE formulation
            SSPJJ = ZERO
            PJJ = ZERO
            EINTJJ = ZERO
            DO IMAT = 1, NBMAT
               PHASE_EINTJJ(IMAT) = FVM_INLET_DATA%VAL_PRES(IMAT)
               IFUNC = FVM_INLET_DATA%FUNC_PRES(IMAT)
               IF (IFUNC  >  0) THEN
                  PHASE_EINTJJ(IMAT) = PHASE_EINTJJ(IMAT) * FUNC_VALUE(IFUNC)
               ELSEIF (IFUNC <= 0) THEN
                  PHASE_EINTJJ(IMAT) = PHASE_EINTII(IMAT)
               ENDIF
               IF (PHASE_ALPHAJJ(IMAT)  >  ZERO) THEN
                  CALL MULTI_SUBMATLAW(
     1                    0,                  MATLAW(IMAT),                 LOCAL_MATID(IMAT),       1,
     2                    PHASE_EINTJJ(IMAT), PHASE_PRESJJ(IMAT),           PHASE_RHOJJ(IMAT),       PHASE_SSPJJ(IMAT),
     3                    DUMMY2,             DUMMY,                        PM,                      IPM,
     4                    NPROPM,             NPROPMI,                      DUMMY,                   DUMMY2,
     5                    DUMMY,              MULTI_FVM%BFRAC(IMAT,ELEMID), MULTI_FVM%TBURN(ELEMID), DUMMY ,
     6                    DUMMY(1),           DUMMY,                        SNPC                   , STF   ,
     7                    NPF,                TF,                           DUMMY(1)               , 1     ,
     8                    MATPARAM(LOCAL_MATID(IMAT)),NVARTMP_EOS,         VARTMP_EOS              ,NUMMAT ,
     9                    ABURN)
                  SSPJJ = SSPJJ + PHASE_ALPHAJJ(IMAT) * PHASE_RHOJJ(IMAT) * MAX(EM20, PHASE_SSPJJ(IMAT))
                  PJJ = PJJ + PHASE_PRESJJ(IMAT) * PHASE_ALPHAJJ(IMAT)
                  EINTJJ = EINTJJ + PHASE_ALPHAJJ(IMAT) * PHASE_EINTJJ(IMAT)
               ENDIF
            ENDDO
            
         ELSEIF (FVM_INLET_DATA%FORMULATION  ==  1) THEN
C     VP formulation
            SSPJJ = ZERO
            PJJ = ZERO
            EINTJJ = ZERO
            DO IMAT = 1, NBMAT
               PHASE_PRESJJ(IMAT) = FVM_INLET_DATA%VAL_PRES(IMAT)
               IFUNC = FVM_INLET_DATA%FUNC_PRES(IMAT)
               IF (IFUNC  >  0) THEN
                  PHASE_PRESJJ(IMAT) = PHASE_PRESJJ(IMAT) * FUNC_VALUE(IFUNC)
               ELSEIF (IFUNC <= 0) THEN
                  PHASE_PRESJJ(IMAT) = PHASE_PRESII(IMAT)
               ENDIF
               PJJ = PJJ + PHASE_PRESJJ(IMAT) * PHASE_ALPHAJJ(IMAT)
C     Need to solve p(RHO, EINT) = PHASE_PRESJJ(IMAT) for EINT
C     INITIALIZE INTERNAL ENERGY
               PHASE_EINTJJ(IMAT) = PM(23, LOCAL_MATID(IMAT))
               CALL MULTI_SOLVE_EINT(MATLAW(IMAT), LOCAL_MATID(IMAT), PM, IPM, NPROPM, NPROPMI,
     .              PHASE_EINTJJ(IMAT), PHASE_RHOJJ(IMAT), PHASE_PRESJJ(IMAT), PHASE_SSPJJ(IMAT),
     .              MULTI_FVM%BFRAC(IMAT, ELEMID), MULTI_FVM%TBURN(ELEMID), DUMMY(1), DUMMY(1),
     .              DUMMY, DUMMY2, SNPC, STF, NPF, TF, DUMMY, 1, MATPARAM(LOCAL_MATID(IMAT)),
     ?              NVARTMP_EOS, VARTMP_EOS, NUMMAT, ABURN)
               
               SSPJJ = SSPJJ + PHASE_ALPHAJJ(IMAT) * PHASE_RHOJJ(IMAT) * MAX(EM20, PHASE_SSPJJ(IMAT))
               EINTJJ = EINTJJ + PHASE_ALPHAJJ(IMAT) * PHASE_EINTJJ(IMAT)
            ENDDO
         ENDIF
         IF (SSPJJ / RHOJJ  >  ZERO) THEN
            SSPJJ = SQRT(SSPJJ / RHOJJ)
         ELSE
            SSPJJ = MULTI_FVM%SOUND_SPEED(ELEMID)
         ENDIF
         IF (FVM_INLET_DATA%VECTOR_VELOCITY  ==  0) THEN
C     Normal velocity imposed
            NORMAL_VELJJ = -FVM_INLET_DATA%VAL_VEL(1)
            IFUNC = FVM_INLET_DATA%FUNC_VEL(1)
            IF (IFUNC  >  0) THEN
               NORMAL_VELJJ = NORMAL_VELJJ * FUNC_VALUE(IFUNC)
            ELSEIF (IFUNC <= 0) THEN
               NORMAL_VELJJ = NORMAL_VELII
            ENDIF
C           NORMAL_VELJJ = NORMAL_VELII

            VXJJ = NORMAL_VELJJ * NX
            VYJJ = NORMAL_VELJJ * NY
            VZJJ = NORMAL_VELJJ * NZ
         ELSE
            VXJJ = FVM_INLET_DATA%VAL_VEL(1)
            IFUNC = FVM_INLET_DATA%FUNC_VEL(1)
            IF (IFUNC  >  0) THEN
               VXJJ = VXJJ * FUNC_VALUE(IFUNC)
            ELSEIF (IFUNC <= 0) THEN
               VXJJ = VXII
            ENDIF
            VYJJ = FVM_INLET_DATA%VAL_VEL(2)
            IFUNC = FVM_INLET_DATA%FUNC_VEL(2)
            IF (IFUNC  >  0) THEN
               VYJJ = VYJJ * FUNC_VALUE(IFUNC)
            ELSEIF (IFUNC <= 0) THEN
               VYJJ = VYII
            ENDIF
            VZJJ = FVM_INLET_DATA%VAL_VEL(3)
            IFUNC = FVM_INLET_DATA%FUNC_VEL(3)
            IF (IFUNC  >  0) THEN
               VZJJ = VZJJ * FUNC_VALUE(IFUNC)
            ELSEIF (IFUNC <= 0) THEN
               VZJJ = VZII
            ENDIF
            NORMAL_VELJJ = VXJJ * NX + VYJJ * NY + VZJJ * NZ
         ENDIF
         VELJJ2 = VXJJ**2 + VYJJ**2 + VZJJ**2
C     HLL wave speed estimates
         SL = MIN(NORMAL_VELII - SSPII, NORMAL_VELJJ - SSPJJ)
         SR = MAX(NORMAL_VELII + SSPII, NORMAL_VELJJ + SSPJJ)

C     Intermediate wave speed
         SSTAR = PJJ - PII + RHOII * NORMAL_VELII * (SL - NORMAL_VELII) - RHOJJ * NORMAL_VELJJ * (SR - NORMAL_VELJJ)
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
C     Take the fluxes of cell II
C     ===
C     Global fluxes
            MULTI_FVM%FLUXES(1:5, KFACE, ELEMID) = (FII(1:5) - NORMALW * VII(1:5)) * SURF
            MULTI_FVM%FLUXES(6, KFACE, ELEMID) = NORMAL_VELII * SURF
C     ===
C     Submaterial fluxes
            IF (NBMAT  >  1) THEN
               DO IMAT = 1, NBMAT
                  ALPHAII = PHASE_ALPHAII(IMAT)
                  SUB_RHOII = PHASE_RHOII(IMAT) ! ALPHA_RHO
                  SUB_RHOEINTII = PHASE_EINTII(IMAT)
                  SUB_VIISTAR(1) = ALPHAII
                  SUB_VIISTAR(2) = ALPHAII * SUB_RHOII ! ALPHA_RHO
                  SUB_VIISTAR(3) = ALPHAII * SUB_RHOEINTII
                  SUB_FIISTAR(1:3) = SUB_VIISTAR(1:3) * NORMAL_VELII
                  MULTI_FVM%SUBVOL_FLUXES(IMAT, KFACE, ELEMID) = (SUB_FIISTAR(1) - NORMALW * SUB_VIISTAR(1)) * SURF
                  MULTI_FVM%SUBMASS_FLUXES(IMAT, KFACE, ELEMID) = (SUB_FIISTAR(2) - NORMALW * SUB_VIISTAR(2)) * SURF
                  MULTI_FVM%SUBENER_FLUXES(IMAT, KFACE, ELEMID) = (SUB_FIISTAR(3) - NORMALW * SUB_VIISTAR(3)) * SURF
               ENDDO
            ENDIF
C     
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
C     Take intermediate state flux (HLLC scheme)
C     ===
C     Global fluxes
            VIISTAR(1:5) = FII(1:5) - (SL) * VII(1:5) - PP(1:5)
            VIISTAR(1:5) = VIISTAR(1:5) / (SSTAR - SL)
            FIISTAR(1:5) = VIISTAR(1:5) * SSTAR + PP(1:5) 
            MULTI_FVM%FLUXES(1:5, KFACE, ELEMID) = (FIISTAR(1:5) - NORMALW * VIISTAR(1:5)) * SURF
            MULTI_FVM%FLUXES(6, KFACE, ELEMID) = SSTAR * SURF
C     ===
C     Submaterial fluxes
            IF (NBMAT  >  1) THEN
               DO IMAT = 1, NBMAT
                  MATLAW(IMAT) = IPM(2, LOCAL_MATID(IMAT))
                  ALPHASTAR = PHASE_ALPHAII(IMAT)
                  SUB_RHOSTAR = PHASE_RHOII(IMAT) * (NORMAL_VELII - SL) / (SSTAR - SL)
                  IF (ALPHASTAR  >  ZERO) THEN
                     SUB_RHOII = PHASE_RHOII(IMAT)
                     SUB_RHOEINTII = PHASE_EINTII(IMAT)
                     SUB_PII = PHASE_PRESII(IMAT)
                     SUB_ESTAR = PHASE_EINTII(IMAT) / PHASE_RHOII(IMAT) - 
     .                    PHASE_PRESII(IMAT) * (ONE / SUB_RHOSTAR - ONE / PHASE_RHOII(IMAT)) 

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
            FII(2) = VII(2) * NORMAL_VELJJ + PJJ * NX
            FII(3) = VII(3) * NORMAL_VELJJ + PJJ * NY
            FII(4) = VII(4) * NORMAL_VELJJ + PJJ * NZ
            FII(5) = (VII(5) + PJJ) * NORMAL_VELJJ
!     /th/surf : pressure
            PSURF = PJJ
C     Take intermediate state flux (HLLC scheme)
C     ===
C     Global fluxes
            VIISTAR(1:5) = FII(1:5) - (SR) * VII(1:5) - PP(1:5)
            VIISTAR(1:5) = VIISTAR(1:5) / (SSTAR - SR)
            FIISTAR(1:5) = VIISTAR(1:5) * SSTAR + PP(1:5) 
            MULTI_FVM%FLUXES(1:5, KFACE, ELEMID) = (FIISTAR(1:5) - NORMALW * VIISTAR(1:5)) * SURF
            MULTI_FVM%FLUXES(6, KFACE, ELEMID) = SSTAR * SURF
C     ===
C     Submaterial fluxes
            IF (NBMAT  >  1) THEN
               DO IMAT = 1, NBMAT
                  MATLAW(IMAT) = IPM(2, LOCAL_MATID(IMAT))
                  ALPHASTAR = PHASE_ALPHAJJ(IMAT)
                  SUB_RHOSTAR = PHASE_RHOJJ(IMAT) * (NORMAL_VELJJ - SR) / (SSTAR - SR)
                  IF (ALPHASTAR  >  ZERO) THEN
                     SUB_RHOII = PHASE_RHOJJ(IMAT)
                     SUB_RHOEINTII = PHASE_EINTJJ(IMAT)
                     SUB_PII = PHASE_PRESJJ(IMAT)
                     SUB_ESTAR = PHASE_EINTJJ(IMAT) / PHASE_RHOJJ(IMAT) - 
     .                    PHASE_PRESJJ(IMAT) * (ONE / SUB_RHOSTAR - ONE / PHASE_RHOJJ(IMAT)) 
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
            FII(2) = VII(2) * NORMAL_VELJJ + PJJ * NX
            FII(3) = VII(3) * NORMAL_VELJJ + PJJ * NY
            FII(4) = VII(4) * NORMAL_VELJJ + PJJ * NZ
            FII(5) = (VII(5) + PJJ) * NORMAL_VELJJ
!     /th/surf : pressure
            PSURF = PJJ
C     Take the fluxes of cell II
C     ===
C     Global fluxes
            MULTI_FVM%FLUXES(1:5, KFACE, ELEMID) = (FII(1:5) - NORMALW * VII(1:5)) * SURF
            MULTI_FVM%FLUXES(6, KFACE, ELEMID) = NORMAL_VELJJ * SURF
C     ===
C     Submaterial fluxes
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
C     
         ENDIF

         ! /TH/SURF (massflow,velocity,pressure,acceleration)
         FSAVSURF(2,ID_SURF) = FSAVSURF(2,ID_SURF) + MULTI_FVM%FLUXES(1, KFACE, ELEMID)            ! += rho.S.un
         FSAVSURF(3,ID_SURF) = FSAVSURF(3,ID_SURF) + MULTI_FVM%FLUXES(1, KFACE, ELEMID) / VII(1)   ! += S.un
         FSAVSURF(4,ID_SURF) = FSAVSURF(4,ID_SURF) + SURF*PSURF                                    ! += S.P
         FSAVSURF(6,ID_SURF) = FSAVSURF(6,ID_SURF) + MULTI_FVM%FLUXES(1, KFACE, ELEMID) * TIMESTEP ! m <- m+dm (cumulative value)
      ENDDO
C-----------------------------------------------
C     E n d   o f   s u b r o u t i n e 
C-----------------------------------------------
      END SUBROUTINE MULTI_INLET_EBCS
      END MODULE MULTI_INLET_EBCS_MOD
