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
      !||    multi_timeevolution                  ../engine/source/multifluid/multi_timeevolution.F
      !||--- called by ------------------------------------------------------
      !||    alemain                              ../engine/source/ale/alemain.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                               ../engine/source/output/message/message.F
      !||    arret                                ../engine/source/system/arret.F
      !||    multi_computevolume                  ../engine/source/multifluid/multi_computevolume.F
      !||    multi_ebcs                           ../engine/source/multifluid/multi_ebcs.F
      !||    multi_evolve_global                  ../engine/source/multifluid/multi_evolve_global.F
      !||    multi_evolve_partial                 ../engine/source/multifluid/multi_evolve_partial.F
      !||    multi_face_elem_data                 ../engine/source/multifluid/multi_face_data_elem.F
      !||    multi_fluxes_computation             ../engine/source/multifluid/multi_fluxes_computation.F
      !||    multi_muscl_fluxes_computation       ../engine/source/multifluid/multi_muscl_fluxes_computation.F
      !||    multi_muscl_gradients                ../engine/source/multifluid/multi_muscl_gradients.F
      !||    multi_update_global                  ../engine/source/multifluid/multi_update_global.F
      !||    multi_update_partial                 ../engine/source/multifluid/multi_update_partial.F
      !||    my_barrier                           ../engine/source/system/machine.F
      !||    spmd_e1vois                          ../engine/source/mpi/fluid/spmd_cfd.F
      !||    spmd_envois                          ../engine/source/mpi/fluid/spmd_cfd.F
      !||    startime                             ../engine/source/system/timer_mod.F90
      !||    stoptime                             ../engine/source/system/timer_mod.F90
      !||--- uses       -----------------------------------------------------
      !||    ale_connectivity_mod                 ../common_source/modules/ale/ale_connectivity_mod.F
      !||    alefvm_mod                           ../common_source/modules/ale/alefvm_mod.F
      !||    ebcs_mod                             ../common_source/modules/boundary_conditions/ebcs_mod.F90
      !||    elbufdef_mod                         ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    initbuf_mod                          ../engine/share/resol/initbuf.F
      !||    matparam_def_mod                     ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    message_mod                          ../engine/share/message_module/message_mod.F
      !||    multi_fvm_mod                        ../common_source/modules/ale/multi_fvm_mod.F90
      !||    multi_muscl_fluxes_computation_mod   ../engine/source/multifluid/multi_muscl_fluxes_computation.F
      !||    th_surf_mod                          ../common_source/modules/interfaces/th_surf_mod.F
      !||    timer_mod                            ../engine/source/system/timer_mod.F90
      !||====================================================================
      SUBROUTINE MULTI_TIMEEVOLUTION(TIMERS,TIMESTEP, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG,
     .     PM, IPM, MULTI_FVM, ALE_CONNECTIVITY, VEL, ACCELE, WGRID, XGRID, DNOD, NALE,
     .     PARTSAV, IPARTS, GRESAV, IGRTH, GRTH, 
     .     NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, 
     .     ITAB, ITABM1, CURRENT_TIME,
     .     STIFN, FSKY, IADS, FSKYM, 
     .     CONDN, CONDNSKY, BUFMAT, FUNC_VALUE, PRED,ID_GLOBAL_VOIS,FACE_VOIS,EBCS_TAB,NPF,TF,FSAVSURF,MATPARAM,
     .     WFEXT)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE TIMER_MOD
      USE INITBUF_MOD      
      USE ELBUFDEF_MOD
      USE MULTI_FVM_MOD
      USE ALEFVM_MOD
      USE MESSAGE_MOD
      USE EBCS_MOD
      USE ALE_CONNECTIVITY_MOD
      USE TH_SURF_MOD , only : TH_SURF_NUM_CHANNEL
      USE MATPARAM_DEF_MOD, ONLY : MATPARAM_STRUCT_
      USE MULTI_MUSCL_FLUXES_COMPUTATION_MOD , ONLY : MULTI_MUSCL_FLUXES_COMPUTATION
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "mvsiz_p.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TIMER_), INTENT(INOUT) :: TIMERS
      TYPE(MATPARAM_STRUCT_),DIMENSION(NUMMAT),INTENT(IN) :: MATPARAM !< material buffer
      my_real,INTENT(INOUT) :: FSAVSURF(TH_SURF_NUM_CHANNEL,NSURF)
      my_real, INTENT(IN) :: TIMESTEP
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      INTEGER, INTENT(IN) :: IPARG(NPARG, *), IADS(8, *)
      INTEGER, INTENT(IN) :: ITASK ! SMP TASK
      INTEGER, INTENT(IN), TARGET :: IXS(NIXS, *), IXQ(NIXQ, *), IXTG(NIXTG, *)
      INTEGER, INTENT(IN) :: IPM(NPROPMI, *)
      my_real, INTENT(IN) :: PM(NPROPM, *)
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      ! for parith/on : ID_GLOBAL_VOIS --> user id ; FACE_VOIS --> face of the remote element
      INTEGER, INTENT(IN) :: ID_GLOBAL_VOIS(*),FACE_VOIS(*)
      my_real, INTENT(INOUT) :: VEL(3, *), ACCELE(3, *)
      my_real, INTENT(IN) :: WGRID(3, *)
      my_real, INTENT(INOUT) :: XGRID(3, *), DNOD(3, *)
      INTEGER, INTENT(IN) :: NALE(*)
      my_real, INTENT(INOUT) :: PARTSAV(NPSAV, *), GRESAV(*)
      INTEGER, INTENT(IN) :: IPARTS(*), IGRTH(*), GRTH(*)
      INTEGER, INTENT(IN) :: NERCVOIS(*), NESDVOIS(*), LERCVOIS(*), LESDVOIS(*)
      INTEGER, INTENT(IN) :: ITAB(*), ITABM1(*)
      my_real, INTENT(IN) :: CURRENT_TIME
      my_real, INTENT(INOUT) :: FSKYM(*), STIFN(*), FSKY(*),
     .     CONDN(*), CONDNSKY(*), BUFMAT(*)
      LOGICAL, INTENT(IN) :: PRED
      my_real, INTENT(IN) :: FUNC_VALUE(*)
      TYPE(t_ebcs_tab), INTENT(IN) :: EBCS_TAB
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
      INTEGER, INTENT(IN) :: NPF(SNPC)
      my_real, INTENT(IN) :: TF(STF)
      DOUBLE PRECISION,INTENT(INOUT) :: WFEXT
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NG, NEL, II, I
      INTEGER :: LOCAL_MATID
      INTEGER :: NBMAT, IMAT, NIX
      INTEGER :: LENCOM
      INTEGER :: NODE_ID
      INTEGER, DIMENSION(:, :), POINTER :: IX
      INTEGER :: MATLAW, NFT, ITY, ELEMTYP
      my_real :: VDT
      my_real :: VOLNEW(MVSIZ)
      TYPE(G_BUFEL_), POINTER :: GBUF
C     ------------------------- 
C     Deplacement des noeuds
C     -------------------------  
      IF (IALE /= 0 .AND. .NOT. PRED) THEN     ! IF CORRECTION : maillage en configuration tn+1/2 pour les flux a tn+1/2
         DO NODE_ID = 1 + ITASK, NUMNOD, NTHREAD
            VDT = HALF * TIMESTEP * WGRID(1, NODE_ID)
            DNOD(1, NODE_ID) = DNOD(1, NODE_ID) + VDT
            XGRID(1, NODE_ID) = XGRID(1, NODE_ID) + VDT
            
            VDT = HALF * TIMESTEP * WGRID(2,NODE_ID)
            DNOD(2, NODE_ID) = DNOD(2, NODE_ID) + VDT
            XGRID(2, NODE_ID) = XGRID(2, NODE_ID) + VDT

            VDT = HALF * TIMESTEP * WGRID(3,NODE_ID)
            DNOD(3, NODE_ID) = DNOD(3, NODE_ID) + VDT
            XGRID(3, NODE_ID) = XGRID(3, NODE_ID) + VDT
         ENDDO
      ENDIF

      CALL MY_BARRIER
C     ==========================================
C     Face data computation (NX, NY, NZ, SURFACE)
C     ==========================================
      IF (IALE /= 0) THEN
C     Only ALE, since nodes have moved
         CALL MULTI_FACE_ELEM_DATA(ITASK, IPARG, IXS, IXQ, IXTG, XGRID, WGRID, MULTI_FVM)
      ENDIF
      CALL MY_BARRIER
C------------------
C     MPI COM
C------------------
      IF (NSPMD > 1) THEN
!$OMP SINGLE
         LENCOM = NERCVOIS(NSPMD + 1) + NESDVOIS(NSPMD + 1)
C     Temporary vars used for fluxes computation
         CALL SPMD_E1VOIS(MULTI_FVM%RHO, 
     .        NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
         CALL SPMD_E1VOIS(MULTI_FVM%PRES, 
     .        NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
         CALL SPMD_E1VOIS(MULTI_FVM%EINT, 
     .        NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
C     Velocity
         CALL SPMD_E1VOIS(MULTI_FVM%VEL(1, :), 
     .        NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
         CALL SPMD_E1VOIS(MULTI_FVM%VEL(2, :), 
     .        NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
         CALL SPMD_E1VOIS(MULTI_FVM%VEL(3, :), 
     .        NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
C     Sound speed
         CALL SPMD_E1VOIS(MULTI_FVM%SOUND_SPEED, 
     .        NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
C     Burnt fraction 
         CALL SPMD_ENVOIS(MULTI_FVM%NBMAT, MULTI_FVM%BFRAC, 
     .        NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
C     SUBMAT VARTIABLES
         IF (MULTI_FVM%NBMAT > 1) THEN
            CALL SPMD_ENVOIS(MULTI_FVM%NBMAT, MULTI_FVM%PHASE_ALPHA, 
     .           NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
            CALL SPMD_ENVOIS(MULTI_FVM%NBMAT, MULTI_FVM%PHASE_RHO, 
     .           NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
            CALL SPMD_ENVOIS(MULTI_FVM%NBMAT, MULTI_FVM%PHASE_EINT, 
     .           NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
            CALL SPMD_ENVOIS(MULTI_FVM%NBMAT, MULTI_FVM%PHASE_PRES, 
     .           NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
         ENDIF

         IF (MULTI_FVM%MUSCL > 0) THEN
            IF (IALE /= 0) THEN
               CALL SPMD_ENVOIS(3, MULTI_FVM%ELEM_DATA%CENTROID, 
     .              NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
            ENDIF
            ! for parith/on, need to communicate FACE_DATA%CENTROID
            ! in order to use the same data
            CALL SPMD_ENVOIS(18, MULTI_FVM%FACE_DATA%CENTROID, 
     .              NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
         ENDIF
!$OMP END SINGLE
         CALL MY_BARRIER
      ENDIF
C------------------
C     END MPI COM
C------------------
C     ==============================
C     gradients computation for MUSCL
C     ==============================
      IF (MULTI_FVM%MUSCL > 0) THEN
         CALL MULTI_MUSCL_GRADIENTS(TIMERS,ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG,
     .        PM, IPM, MULTI_FVM, ALE_CONNECTIVITY, WGRID, XGRID, ITAB, MULTI_FVM%NBMAT, 
     .        CURRENT_TIME, BUFMAT)
      ENDIF
      CALL MY_BARRIER    

C     Gradient communication
      IF (NSPMD > 1 .AND. MULTI_FVM%MUSCL > 0) THEN
!$OMP SINGLE
         CALL STARTIME(TIMERS,TIMER_SPMDCFD)
         LENCOM = NERCVOIS(NSPMD + 1) + NESDVOIS(NSPMD + 1)
         IF (MULTI_FVM%MUSCL == 1) THEN
C     Velocity gradients
            CALL SPMD_ENVOIS(3, MULTI_FVM%GRAD_U, 
     .           NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
            CALL SPMD_ENVOIS(3, MULTI_FVM%GRAD_V, 
     .           NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
            CALL SPMD_ENVOIS(3, MULTI_FVM%GRAD_W, 
     .           NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
         ENDIF
C     SUBMAT gradients
         IF (MULTI_FVM%NBMAT == 1) THEN
            IF (MULTI_FVM%MUSCL == 1) THEN
               CALL SPMD_ENVOIS(3, MULTI_FVM%GRAD_RHO, 
     .              NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
               CALL SPMD_ENVOIS(3, MULTI_FVM%GRAD_PRES, 
     .              NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
            ENDIF
         ELSE
            CALL SPMD_ENVOIS(3 * MULTI_FVM%NBMAT, MULTI_FVM%PHASE_GRAD_ALPHA, 
     .           NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
            IF (MULTI_FVM%MUSCL == 1) THEN
               CALL SPMD_ENVOIS(3 * MULTI_FVM%NBMAT, MULTI_FVM%PHASE_GRAD_RHO, 
     .              NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
               CALL SPMD_ENVOIS(3 * MULTI_FVM%NBMAT, MULTI_FVM%PHASE_GRAD_PRES, 
     .              NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
            ENDIF
         ENDIF
         CALL STOPTIME(TIMERS,TIMER_SPMDCFD)
!$OMP END SINGLE
         CALL MY_BARRIER
      ENDIF

      DO NG = ITASK + 1, NGROUP, NTHREAD
         MATLAW = IPARG(1, NG)
         IF (MATLAW == 151) THEN
            NEL = IPARG(2, NG)
            NFT = IPARG(3, NG)
            ITY = IPARG(5, NG)
            GBUF => ELBUF_TAB(NG)%GBUF
            IF (MULTI_FVM%MUSCL > 0) THEN
               CALL MULTI_MUSCL_FLUXES_COMPUTATION(NG, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG,
     .              PM, IPM, MULTI_FVM, ALE_CONNECTIVITY, WGRID, XGRID, ITAB, 
     .              MULTI_FVM%NBMAT, CURRENT_TIME, BUFMAT,
     .              ID_GLOBAL_VOIS,FACE_VOIS,NPF,TF,ISPMD, MATPARAM)
            ELSE
               CALL MULTI_FLUXES_COMPUTATION(NG, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG,
     .              PM, IPM, MULTI_FVM, ALE_CONNECTIVITY, WGRID, XGRID, ITAB, 
     .              MULTI_FVM%NBMAT, CURRENT_TIME, BUFMAT,
     .              ID_GLOBAL_VOIS,NPF,TF)
            ENDIF
         ENDIF
      ENDDO                     ! DO NG = ITASK + 1, NGROUP, NTHREAD
C---------------------
C     Boundary conditions
C---------------------

      IF (EBCS_TAB%nebcs_fvm > 0) THEN
         CALL MY_BARRIER
         CALL MULTI_EBCS(ITASK, MULTI_FVM, IXS, IXQ, IXTG, XGRID, WGRID, IPM, PM, FUNC_VALUE, 
     .        EBCS_TAB,NPF,TF,FSAVSURF,NSURF,TIMESTEP,
     .        NUMELS, NUMELQ, NUMELTG, NUMNOD, NCYCLE, NUMMAT, MATPARAM)
         CALL MY_BARRIER
      ENDIF
C     ------------------------- 
C     Deplacement des noeuds
C     -------------------------  
      IF (IALE /= 0 .AND. .NOT. PRED) THEN    ! IF CORRECTION : maillage en configuration tn+1 pour les volumes
         DO NODE_ID = 1 + ITASK, NUMNOD, NTHREAD
            VDT = HALF * TIMESTEP * WGRID(1, NODE_ID)
            DNOD(1, NODE_ID) = DNOD(1, NODE_ID) + VDT
            XGRID(1, NODE_ID) = XGRID(1, NODE_ID) + VDT
            
            VDT = HALF * TIMESTEP * WGRID(2,NODE_ID)
            DNOD(2, NODE_ID) = DNOD(2, NODE_ID) + VDT
            XGRID(2, NODE_ID) = XGRID(2, NODE_ID) + VDT

            VDT = HALF * TIMESTEP * WGRID(3,NODE_ID)
            DNOD(3, NODE_ID) = DNOD(3, NODE_ID) + VDT
            XGRID(3, NODE_ID) = XGRID(3, NODE_ID) + VDT
         ENDDO
      ENDIF
      
      CALL MY_BARRIER

C---------------------  
      NBMAT = MULTI_FVM%NBMAT

      DO NG = ITASK + 1, NGROUP, NTHREAD
         MATLAW = IPARG(1, NG)
         IF (MATLAW == 151) THEN
C     -------------------------  
C     Multifluid law: update global values
C     -------------------------
            CALL MULTI_EVOLVE_GLOBAL(TIMESTEP, NG, ELBUF_TAB, 
     .           IPARG, ITASK, IXS, IXQ, IXTG, MULTI_FVM, ALEFVM_Buffer%VERTEX, WFEXT)
            IF (NBMAT > 1) THEN
C     -------------------------  
C     Multifluid law: evolve partial values
C     -------------------------
               CALL MULTI_EVOLVE_PARTIAL(TIMESTEP, NG, ELBUF_TAB, 
     .              IPARG, ITASK, IXS, IXQ, IXTG,  MULTI_FVM, 
     .              PM, IPM, ALEFVM_Buffer%VERTEX, CURRENT_TIME)                  
            ENDIF
         ENDIF                  !IF (MATLAW == 151) THEN
      ENDDO                     !NG = ITASK + 1, NGROUP, NTHREAD    

      DO NG = ITASK + 1, NGROUP, NTHREAD
         MATLAW = IPARG(1, NG)
         IF (MATLAW == 151) THEN
            NEL = IPARG(2, NG)
            NFT = IPARG(3, NG)
            ITY = IPARG(5, NG)
            GBUF => ELBUF_TAB(NG)%GBUF
C     -------------------------  
C     Volume computation
C     -------------------------
            VOLNEW(1:MVSIZ) = ZERO
            CALL MULTI_COMPUTEVOLUME(NEL, NG, IPARG, MULTI_FVM%SYM, 
     .           ELBUF_TAB, IXS, IXQ, IXTG, VOLNEW, XGRID)
C     -------------------------  
C     Multifluid law: update global values
C     -------------------------
            CALL MULTI_UPDATE_GLOBAL(NG, ELBUF_TAB, IPARG, ITASK, MULTI_FVM, 
     .           PARTSAV, IPARTS, GRESAV, IGRTH, GRTH, VOLNEW(1:NEL), PRED, TIMESTEP)

            IF (MULTI_FVM%SYM == 0) THEN
               IX => IXS(1:NIXS, 1 + NFT:NEL + NFT)
               NIX = NIXS
               ELEMtYP = 0
            ELSEIF (ITY == 2) THEN
C     QUADS
               IX => IXQ(1:NIXQ, 1 + NFT:NEL + NFT)
               NIX = NIXQ
               ELEMTYP = 1
            ELSEIF (ITY == 7) THEN
C     TRIANGLES
               IX => IXTG(1:NIXTG, 1 + NFT:NEL + NFT)
               NIX = NIXTG
               ELEMTYP = 2
            ENDIF
C     Check if rho is negative, then stop RADIOSS instance
            DO II = 1, NEL
               I = II + NFT
               IF (MULTI_FVM%RHO(I) <= ZERO) THEN
                  IF (ELEMTYP == 0) THEN
                     CALL ANCMSG(MSGID = 167, ANMODE = ANINFO, I1 = IX(NIX, II), 
     .                    R1 = MULTI_FVM%RHO(I))
                  ELSE
                     CALL ANCMSG(MSGID = 12, ANMODE = ANINFO, I1 = IX(NIX, II), 
     .                    R1 = MULTI_FVM%RHO(I))
                  ENDIF
                  CALL ARRET(2)
               ENDIF
            ENDDO

            IF (NBMAT > 1) THEN
C     -------------------------  
C     Multifluid law: evolve partial values
C     -------------------------
               DO IMAT = 1, NBMAT
                  LOCAL_MATID = IPM(20 + IMAT, IX(1, 1))
                  CALL MULTI_UPDATE_PARTIAL(LOCAL_MATID, PM, IMAT, NG, 
     .                 ELBUF_TAB, IPARG, ITASK, MULTI_FVM, VOLNEW)
               ENDDO
            ENDIF
C     -------------------------  
C     Update volumes
C     -------------------------  
            IF (.NOT. PRED) THEN
               DO II = 1, NEL
                  GBUF%VOL(II) = VOLNEW(II)
               ENDDO
               IF (MULTI_FVM%NS_DIFF) THEN
                  DO II = 1, NEL
                     I = II + NFT
                     MULTI_FVM%VOL(I) = VOLNEW(II)
                  ENDDO
               ENDIF
            ENDIF
         ENDIF                  !IF (MATLAW == 151) THEN
      ENDDO                     !NG = ITASK + 1, NGROUP, NTHREAD
      
      CALL MY_BARRIER

      END SUBROUTINE MULTI_TIMEEVOLUTION
