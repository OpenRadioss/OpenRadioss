Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  MULTI_TIMEEVOLUTION           source/multifluid/multi_timeevolution.F
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MULTI_COMPUTEVOLUME           source/multifluid/multi_computevolume.F
Chd|        MULTI_EBCS                    source/multifluid/multi_ebcs.F
Chd|        MULTI_EVOLVE_GLOBAL           source/multifluid/multi_evolve_global.F
Chd|        MULTI_EVOLVE_PARTIAL          source/multifluid/multi_evolve_partial.F
Chd|        MULTI_FACE_ELEM_DATA          source/multifluid/multi_face_data_elem.F
Chd|        MULTI_FLUXES_COMPUTATION      source/multifluid/multi_fluxes_computation.F
Chd|        MULTI_MUSCL_FLUXES_COMPUTATIONsource/multifluid/multi_muscl_fluxes_computation.F
Chd|        MULTI_MUSCL_GRADIENTS         source/multifluid/multi_muscl_gradients.F
Chd|        MULTI_UPDATE_GLOBAL           source/multifluid/multi_update_global.F
Chd|        MULTI_UPDATE_PARTIAL          source/multifluid/multi_update_partial.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_E1VOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_ENVOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        ALEFVM_MOD                    ../common_source/modules/alefvm_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        EBCS_MOD                      ../common_source/modules/ebcs_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTI_TIMEEVOLUTION(TIMESTEP, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG,
     .     PM, IPM, MULTI_FVM, ALE_CONNECTIVITY, VEL, ACCELE, WGRID, XGRID, DNOD, NALE,
     .     PARTSAV, IPARTS, GRESAV, IGRTH, GRTH, 
     .     NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, 
     .     ITAB, ITABM1, CURRENT_TIME,
     .     STIFN, FSKY, IADS, FSKYM, 
     .     CONDN, CONDNSKY, BUFMAT, FUNC_VALUE, PRED,ID_GLOBAL_VOIS,FACE_VOIS,EBCS_TAB,NPF,TF)
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD      
      USE ELBUFDEF_MOD
      USE MULTI_FVM_MOD
      USE ALEFVM_MOD
      USE MESSAGE_MOD
      USE EBCS_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
!     NEBCS
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "mvsiz_p.inc"
!     IPARIT
#include      "parit_c.inc"
!     DTFAC1
#include      "scr18_c.inc" 
!     SMPD
#include "spmd_c.inc"
!     SNPC,STF
#include "tabsiz_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
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
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(G_BUFEL_), POINTER :: GBUF
      TYPE(L_BUFEL_), POINTER :: LBUF
      INTEGER :: NG, NEL, II, JJ, KFACE, I, J
      INTEGER :: IPLA, LOCAL_MATID
      INTEGER :: NBMAT, IMAT, NIX
      INTEGER :: NODE1, NODE2, NODE3, NODE4, 
     .     NODE5, NODE6, NODE7, NODE8
      my_real :: W1(3), W2(3), W3(3), W4(3), 
     .     W5(3), W6(3), W7(3), W8(3)
      my_real :: X1(3), X2(3), X3(3), X4(3), 
     .     X5(3), X6(3), X7(3), X8(3)
      my_real :: NORM(MVSIZ, 6, 3)
      my_real :: WFAC(MVSIZ, 6, 3)
      my_real :: LAMBDAII, LAMBDAF, NORMUII, NORMUJJ
      my_real :: FII(5), FJJ(5), NORMAL_VEL, NORMAL_VEL2, VII(5), VJJ(5), VEL2, SURF
      my_real :: VOLNEW(MVSIZ)
      my_real :: RHO, VOLFRAC, MASS
      INTEGER :: LENCOM
C     TEMPORAIRE
      INTEGER :: K, K2, NODE_ID
      my_real :: VDT
      my_real :: FAC
      INTEGER :: NFIRST, NLAST
      INTEGER, DIMENSION(:, :), POINTER :: IX
      INTEGER :: IBID
      my_real
     .     AR, FR_WAVE, FTHE, FTHESKY, FFSKY, SUMFLUX, MASS1, MASS2
      my_real :: sum1, sum2
      INTEGER :: MATLAW, NFT, ITY, ELEMTYP
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
         CALL MULTI_MUSCL_GRADIENTS(ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG,
     .        PM, IPM, MULTI_FVM, ALE_CONNECTIVITY, WGRID, XGRID, ITAB, MULTI_FVM%NBMAT, 
     .        CURRENT_TIME, BUFMAT) 
      ENDIF
      CALL MY_BARRIER    

C     Gradient communication
      IF (NSPMD > 1 .AND. MULTI_FVM%MUSCL > 0) THEN
!$OMP SINGLE
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
     .              ID_GLOBAL_VOIS,FACE_VOIS,NPF,TF)
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
     .        EBCS_TAB,NPF,TF)
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
     .           IPARG, ITASK, IXS, IXQ, IXTG, MULTI_FVM, VERTEX)  
            IF (NBMAT > 1) THEN
C     -------------------------  
C     Multifluid law: evolve partial values
C     -------------------------
               CALL MULTI_EVOLVE_PARTIAL(TIMESTEP, NG, ELBUF_TAB, 
     .              IPARG, ITASK, IXS, IXQ, IXTG,  MULTI_FVM, 
     .              PM, IPM, VERTEX, CURRENT_TIME)                  
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
            CALL MULTI_COMPUTEVOLUME(NEL, NG, IPARG, MULTI_FVM%SYM, 
     .           ELBUF_TAB, IXS, IXQ, IXTG, VOLNEW, XGRID)
C     -------------------------  
C     Multifluid law: update global values
C     -------------------------
            CALL MULTI_UPDATE_GLOBAL(NG, ELBUF_TAB, IPARG, ITASK, MULTI_FVM, 
     .           PARTSAV, IPARTS, GRESAV, IGRTH, GRTH, VOLNEW(1:NEL), PRED) 

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
