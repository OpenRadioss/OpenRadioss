!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!Copyright>        Commercial Alternative: Altair Radioss
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!||====================================================================
!||    q1np_forc3                       ../engine/source/elements/solid/solid_q1np/q1np_forc3.F90
!||--- called by ------------------------------------------------------
!||    forint                           ../engine/source/elements/forint.F
!||--- calls      -----------------------------------------------------
!||    ig3daverage                      ../engine/source/elements/ige3d/ig3daverage.F
!||    ige3dbilan                       ../engine/source/elements/ige3d/ige3dbilan.F
!||    mmain                            ../engine/source/materials/mat_share/mmain.F90
!||    q1np_get_knot_vectors            ../common_source/modules/q1np_geom_mod.F90
!||    q1np_build_gauss_scheme          ../common_source/modules/q1np_restart_mod.F90
!||    q1np_jacobian                    ../common_source/modules/q1np_geom_mod.F90
!||    q1np_shape_functions             ../common_source/modules/q1np_geom_mod.F90
!||    smalla3                          ../engine/source/elements/solid/solide/smalla3.F
!||    smallb3                          ../engine/source/elements/solid/solide/smallb3.F
!||    srho3                            ../engine/source/elements/solid/solide/srho3.F
!||    srota3                           ../engine/source/elements/solid/solide/srota3.F
!||    sstra3                           ../engine/source/elements/solid/solide/sstra3.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod             ../common_source/modules/ale/ale_connectivity_mod.F
!||    com08_mod                        ../engine/share/modules/com08_mod.F
!||    constant_mod                     ../common_source/modules/constant_mod.F
!||    debug_mod                        ../engine/share/modules/debug_mod.F
!||    dt_mod                           ../engine/source/modules/dt_mod.F
!||    elbufdef_mod                     ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    element_mod                      ../common_source/modules/elements/element_mod.F90
!||    glob_therm_mod                   ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    mat_elem_mod                     ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    mmain_mod                        ../engine/source/materials/mat_share/mmain.F90
!||    nlocal_reg_mod                   ../common_source/modules/nlocal_reg_mod.F
!||    output_mod                       ../common_source/modules/output/output_mod.F90
!||    param_c_mod                      ../engine/share/modules/param_c_mod.F
!||    q1np_geom_mod                    ../common_source/modules/q1np_geom_mod.F90
!||    q1np_restart_mod                 ../common_source/modules/q1np_restart_mod.F90
!||    restmod                          ../engine/share/modules/restart_mod.F
!||    sensor_mod                       ../common_source/modules/sensor_mod.F90
!||    table_mod                        ../engine/share/modules/table_mod.F
!||    timer_mod                        ../engine/source/system/timer_mod.F90
!||====================================================================
! Calculation of the internal forces for Q1NP elements
!=======================================================================
      SUBROUTINE Q1NP_FORC3(TIMERS, OUTPUT, &
     &                      ELBUF_STR, PM, GEO, IXS, IGEO, X, A, V, W, &
     &                      FV, ALE_CONNECT, IPARG, TF, NPF, BUFMAT, PARTSAV, NPART, &
     &                      NLOC_DMG, STIFN, OFFSET, IPARTS, NEL, DT2T, NELTST, &
     &                      ITYPTST, IPM, ITASK, GRESAV, GRTH, IGRTH, MSSA, DMELS, &
     &                      TABLE, IPRI, MAT_ELEM, NG, H3D_STRAIN, SVIS, GLOB_THERM, &
     &                      SNPC, NUMGEO, NUMNOD, NUMELS, NUMELQ, NGROUP, SBUFMAT, STF, &
     &                      NUMMAT, NTABLE, NSVOIS, IRESP, IDEL7NOK, MAXFUNC, USERL_AVAIL, &
     &                      IMON_MAT, IMPL_S, IDYNA, IDTMIN, DT, SENSORS)
  !-----------------------------------------------
  !   M o d u l e s
  !-----------------------------------------------
      USE TIMER_MOD
      USE OUTPUT_MOD, ONLY : OUTPUT_
      USE ALE_CONNECTIVITY_MOD
      USE ELBUFDEF_MOD
      USE MAT_ELEM_MOD
      USE MMAIN_MOD
      USE NLOCAL_REG_MOD
      USE TABLE_MOD
      USE DT_MOD
      USE GLOB_THERM_MOD
      USE CONSTANT_MOD, ONLY : ZERO, ONE, HALF, FOURTH
      USE Q1NP_RESTART_MOD
      USE Q1NP_GEOM_MOD
      USE SENSOR_MOD
      USE PARAM_C_MOD
      USE COM08_MOD
      USE ELEMENT_MOD, ONLY : NIXS
      USE PRECISION_MOD, ONLY : WP
      USE MY_ALLOC_MOD, ONLY : MY_ALLOC
      USE MY_DEALLOC_MOD, ONLY : MY_DEALLOC

      IMPLICIT NONE
  !=======================================================================
  !  Q1NP_FORC3: internal forces calculation for Q1NP solids
  !
  !  Sequential hierarchy 
  !    1) Identify Q1NP element IDs for the group and derive degrees (P,Q)
  !    2) Reconstruct Q1NP parametric grid + initialize Gauss scheme
  !    3) Allocate and initialize vectorized work arrays
  !    4) (TT=0) snapshot Gauss volumes (VOL -> VOL0DP) for restart/diagnostics
  !    5) Build node/group bookkeeping and gather X/V into element-local arrays
  !    6) Initialize element fields (OFF, RHO0, DELTAX, buffers)
  !    7) SMALLA3: small-rotation coordinate transform for small strain storage
  !    8) Gauss integration loop (IU,IV,IT):
  !         - reset per-GP fields
  !         - geometry (DN, detJ, VOLN/VOLG, etc.)
  !         - material/strain update through engine pipeline
  !         - accumulate nodal internal forces in element-local buffers
  !    9) SMALLB3: update OFF/GBUF%OFF scaling after integration
  !   10) Assemble element-local forces into global A + update dt + stress averages
  !=======================================================================

  !-----------------------------------------------
  !   G l o b a l   P a r a m e t e r s
  !-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "my_real.inc"
#include      "vect01_ff.inc"
  !-----------------------------------------------
  !   D u m m y   A r g u m e n t s
  !-----------------------------------------------
      TYPE(TIMER_),  INTENT(INOUT) :: TIMERS
      TYPE(OUTPUT_), INTENT(INOUT) :: OUTPUT
      TYPE(ELBUF_STRUCT_), TARGET, INTENT(INOUT) :: ELBUF_STR
      INTEGER,            INTENT(IN)    :: NG
      INTEGER,            INTENT(IN)    :: H3D_STRAIN
      INTEGER,            INTENT(IN)    :: SNPC
      INTEGER,            INTENT(IN)    :: NUMGEO
      INTEGER,            INTENT(IN)    :: NUMNOD
      INTEGER,            INTENT(IN)    :: NUMELS
      INTEGER,            INTENT(IN)    :: NUMELQ
      INTEGER,            INTENT(IN)    :: NGROUP
      INTEGER,            INTENT(IN)    :: SBUFMAT
      INTEGER,            INTENT(IN)    :: STF
      INTEGER,            INTENT(IN)    :: NUMMAT
      INTEGER,            INTENT(IN)    :: NTABLE
      INTEGER,            INTENT(IN)    :: NSVOIS
      INTEGER,            INTENT(IN)    :: IRESP
      INTEGER,            INTENT(IN)    :: MAXFUNC
      INTEGER,            INTENT(IN)    :: USERL_AVAIL
      INTEGER,            INTENT(IN)    :: IMON_MAT
      INTEGER,            INTENT(IN)    :: IMPL_S
      INTEGER,            INTENT(IN)    :: IDYNA
      INTEGER,            INTENT(IN)    :: IDTMIN(102)
      INTEGER,            INTENT(IN)    :: NEL, ITASK, NPART
      INTEGER,            INTENT(INOUT) :: NELTST, ITYPTST
      INTEGER,            INTENT(IN)    :: OFFSET, IPRI
      INTEGER,            INTENT(INOUT) :: IDEL7NOK
      INTEGER,            INTENT(IN)    :: NPF(SNPC)
      INTEGER,            INTENT(IN)    :: IPARTS(*) !SIZE NEL?
      INTEGER,            INTENT(IN)    :: IPM(NPROPMI,NUMMAT)
      INTEGER,            INTENT(IN)    :: IXS(NIXS,NUMELS)
      INTEGER,            INTENT(IN)    :: IPARG(NPARG,NGROUP)
      INTEGER,            INTENT(IN)    :: IGEO(NPROPGI,NUMGEO)
      INTEGER,            INTENT(INOUT) :: IGRTH(*), GRTH(*)
      my_real,            INTENT(INOUT) :: PM(NPROPM,NUMMAT)
      my_real,            INTENT(INOUT) :: GEO(NPROPG,NUMGEO)
      my_real,            INTENT(IN)    :: X(3,NUMNOD)
      my_real,            INTENT(INOUT) :: A(3,NUMNOD)
      my_real,            INTENT(INOUT) :: V(3,NUMNOD)
      my_real,            INTENT(IN)    :: W(3,NUMNOD)
      my_real,            INTENT(INOUT) :: STIFN(NUMNOD)
      my_real,            INTENT(INOUT) :: TF(STF)
      my_real,            INTENT(INOUT) :: BUFMAT(SBUFMAT)
      my_real,            INTENT(INOUT) :: FV(*) !SIZE MAXFUNC?
      my_real,            INTENT(INOUT) :: PARTSAV(NPSAV,NPART)
      my_real,            INTENT(INOUT) :: GRESAV(*)
      my_real,            INTENT(INOUT) :: DT2T
      my_real,            INTENT(INOUT) :: MSSA(*)
      my_real,            INTENT(INOUT) :: DMELS(*)
      TYPE(TTABLE),       INTENT(INOUT) :: TABLE(*)
      TYPE(MAT_ELEM_),    INTENT(INOUT) :: MAT_ELEM
      TYPE(NLOCAL_STR_),  INTENT(INOUT) :: NLOC_DMG
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      my_real, DIMENSION(MVSIZ,6), INTENT(INOUT) :: SVIS
      TYPE(GLOB_THERM_),  INTENT(INOUT) :: GLOB_THERM
      TYPE(DT_),          INTENT(IN)    :: DT
      TYPE(SENSORS_),     INTENT(INOUT) :: SENSORS
  !-----------------------------------------------
  !   L o c a l   V a r i a b l e s
  !-----------------------------------------------
      INTEGER :: I, J, K, IU, IV, IT, IEL, IQ1NP
      INTEGER :: NFT_G, MAX_NNODE, TOTAL_NODE_REF, U_LEN, V_LEN
      INTEGER :: P_MAX, Q_MAX, P_CUR, Q_CUR
      INTEGER :: NX_FOUND, NY_FOUND, NX_CAND, NY_CAND
      INTEGER :: NKNOT_U, NKNOT_V, NGP_Q1NP, IPT_Q1NP, IERR
      INTEGER :: NUM_GROUP_NODE, GPOS, LOCAL_ID
      INTEGER :: KNOT_SET_ID
!     Thread-local Gauss integration scheme (replaces the shared module
!     globals Q1NP_NP_*_G / Q1NP_GP_*_G / Q1NP_GW_*_G, which were not safe
!     to write from the parallel FORINT group loop).
      INTEGER :: NP_U_L, NP_V_L, NP_T_L
      REAL(KIND=WP), ALLOCATABLE :: GP_U_L(:), GW_U_L(:)
      REAL(KIND=WP), ALLOCATABLE :: GP_V_L(:), GW_V_L(:)
      REAL(KIND=WP), ALLOCATABLE :: GP_T_L(:), GW_T_L(:)
      INTEGER :: IEXPAN, ISTRAIN, ILAY, SZ_IX, TH_STRAIN
      INTEGER :: Q1NP_IDS(MVSIZ), II(6)
      INTEGER, ALLOCATABLE :: NCTRL_ELEM(:)
      INTEGER, ALLOCATABLE :: U_LEN_EL(:), V_LEN_EL(:)
      INTEGER, ALLOCATABLE :: ELEM_U(:), ELEM_V(:), PID_ELEM(:)
      INTEGER, ALLOCATABLE :: MAT_ID_ELEM(:), NGL_ELEM(:)
      INTEGER, ALLOCATABLE :: NODE_GID(:,:), NODE_LID(:,:), NODE_POS(:,:)
      INTEGER, ALLOCATABLE :: GROUP_GID(:), GROUP_LID(:)
      INTEGER :: IPARG_LOCAL(NPARG,1)
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF
      TYPE(ELBUF_STRUCT_) :: ELBUF_TAB_LOCAL(1)
      my_real :: XI, ETA, ZETA, GPW
      my_real, ALLOCATABLE :: X_ELEM(:,:,:), V_ELEM(:,:,:)
      my_real, ALLOCATABLE :: F_INT_ELEM(:,:,:)
      my_real, ALLOCATABLE :: MASS_ELEM(:,:), STIG_ELEM(:,:)
      my_real, ALLOCATABLE :: U_KNOT(:,:), V_KNOT(:,:)
      my_real, ALLOCATABLE :: NVAL(:), DN_LOCAL(:,:), DN_GLOBAL(:,:)
      my_real, ALLOCATABLE :: MATB_GP(:,:)
      my_real, ALLOCATABLE :: VX_BAL(:,:), VY_BAL(:,:), VZ_BAL(:,:)
      my_real, ALLOCATABLE :: XX_BAL(:,:), YY_BAL(:,:), ZZ_BAL(:,:)
      my_real, ALLOCATABLE :: VGAUSS(:,:)
      my_real :: STI(MVSIZ), OFF(MVSIZ), RHO0(MVSIZ)
      my_real :: VOLN(MVSIZ), VOLG(MVSIZ), DVOL(MVSIZ), VD2(MVSIZ)
      my_real :: DELTAX(MVSIZ), DIVDE(MVSIZ)
      my_real :: VIS(MVSIZ), QVIS(MVSIZ), CXX(MVSIZ)
      my_real :: S1(MVSIZ), S2(MVSIZ), S3(MVSIZ), S4(MVSIZ), S5(MVSIZ), S6(MVSIZ)
      my_real :: DXX(MVSIZ), DYY(MVSIZ), DZZ(MVSIZ)
      my_real :: DXY(MVSIZ), DYX(MVSIZ), DYZ(MVSIZ), DZY(MVSIZ), DZX(MVSIZ), DXZ(MVSIZ)
      my_real :: D4(MVSIZ), D5(MVSIZ), D6(MVSIZ)
      my_real :: WXX(MVSIZ), WYY(MVSIZ), WZZ(MVSIZ)
      my_real :: AJ1(MVSIZ), AJ2(MVSIZ), AJ3(MVSIZ), AJ4(MVSIZ), AJ5(MVSIZ), AJ6(MVSIZ)
      my_real :: VDX(MVSIZ), VDY(MVSIZ), VDZ(MVSIZ), MUVOID(MVSIZ)
      my_real :: SSP_EQ(MVSIZ), AIRE(MVSIZ), SIGY(MVSIZ), ET(MVSIZ)
      my_real :: BUFVOIS(MVSIZ), R3_DAM(MVSIZ), AMU(MVSIZ)
      my_real :: MFXX(MVSIZ), MFXY(MVSIZ), MFXZ(MVSIZ), MFYX(MVSIZ), MFYY(MVSIZ)
      my_real :: MFYZ(MVSIZ), MFZX(MVSIZ), MFZY(MVSIZ), MFZZ(MVSIZ)
      my_real :: GAMA(MVSIZ,6), FR_WAV(MVSIZ), TEMPEL(MVSIZ), DIE(MVSIZ)
      my_real :: VARNL(MVSIZ), CONDE(MVSIZ)
      my_real :: FVD2(MVSIZ), FDELTAX(MVSIZ), FSSP(MVSIZ), FQVIS(MVSIZ)
      my_real :: FHEAT(MVSIZ)
      DOUBLE PRECISION :: VOLDP(MVSIZ)
      my_real :: DUMMY_FLUX(1,1)
      my_real :: DTFAC1(102), DTMIN1(102), PERCENT_ADDMASS
      my_real :: DT_STOP_PERCENT_ADDMASS, MASS0_START, PERCENT_ADDMASS_OLD
      LOGICAL :: Q1NP_MAP_ERROR
      INTEGER, SAVE, ALLOCATABLE :: Q1NP_BULK_FIX_IDS(:,:)
      LOGICAL, SAVE, ALLOCATABLE :: Q1NP_BULK_FIX_READY(:)
      COMMON /SCR18R/ DTFAC1, DTMIN1, PERCENT_ADDMASS, &
     &                DT_STOP_PERCENT_ADDMASS, MASS0_START, PERCENT_ADDMASS_OLD

  !=======================================================================
  !   S o u r c e  L i n e s
  !=======================================================================
  !-----------------------------------------------------------------------
  !  (1) Identify Q1NP element IDs for the group and derive degrees (P,Q)
  !-----------------------------------------------------------------------
      IF (NEL > MVSIZ) THEN
        WRITE(*,'(A,I8,A,I8,A,I8)') 'Q1NP ERROR: NEL exceeds MVSIZ: NG=', &
     &    NG, ' NEL=', NEL, ' MVSIZ=', MVSIZ
        RETURN
      END IF

      NFT_G = IPARG(3,NG)
      MAX_NNODE = 0
      TOTAL_NODE_REF = 0
      P_MAX = 0
      Q_MAX = 0

      DO K = 1, NEL
        IQ1NP = KQ1NP_TAB_INV(NFT_G + K)
        Q1NP_IDS(K) = IQ1NP
        MAX_NNODE = MAX(MAX_NNODE, KQ1NP_TAB(3,IQ1NP) + 4)
        TOTAL_NODE_REF = TOTAL_NODE_REF + KQ1NP_TAB(3,IQ1NP) + 4
        P_CUR = KQ1NP_TAB(8,IQ1NP)
        Q_CUR = KQ1NP_TAB(9,IQ1NP)
        P_MAX = MAX(P_MAX, P_CUR)
        Q_MAX = MAX(Q_MAX, Q_CUR)
      END DO

  !-----------------------------------------------------------------------
  !  (2) Reconstruct Q1NP parametric grid + initialize Gauss scheme
  !-----------------------------------------------------------------------
      CALL MY_ALLOC(U_LEN_EL, NEL, "U_LEN_EL")
      CALL MY_ALLOC(V_LEN_EL, NEL, "V_LEN_EL")
      U_LEN = 0
      V_LEN = 0
      DO K = 1, NEL
        KNOT_SET_ID = KQ1NP_TAB(15, Q1NP_IDS(K))
        P_CUR = KQ1NP_TAB(8, Q1NP_IDS(K))
        Q_CUR = KQ1NP_TAB(9, Q1NP_IDS(K))
        IF (KQ1NP_TAB(12, Q1NP_IDS(K)) > 0 .AND. &
     &      KQ1NP_TAB(13, Q1NP_IDS(K)) > 0) THEN
          NX_CAND = KQ1NP_TAB(12, Q1NP_IDS(K))
          NY_CAND = KQ1NP_TAB(13, Q1NP_IDS(K))
        ELSE IF (Q1NP_NKNOT_SETS_G > 0) THEN
          IF (KNOT_SET_ID <= 0 .OR. KNOT_SET_ID > Q1NP_NKNOT_SETS_G) KNOT_SET_ID = 1
          NX_CAND = Q1NP_NX_SET_G(KNOT_SET_ID)
          NY_CAND = Q1NP_NY_SET_G(KNOT_SET_ID)
        ELSE
          NX_CAND = Q1NP_NX_G
          NY_CAND = Q1NP_NY_G
        END IF
        U_LEN_EL(K) = NX_CAND + 2*P_CUR + 1
        V_LEN_EL(K) = NY_CAND + 2*Q_CUR + 1
        U_LEN = MAX(U_LEN, U_LEN_EL(K))
        V_LEN = MAX(V_LEN, V_LEN_EL(K))
      END DO

  !   Build this group's Gauss scheme into thread-local arrays. This
  !   replaces the former shared module-global scheme, which was written
  !   (and reallocated) by every thread of the FORINT !$OMP DO group loop
  !   and read throughout the routine body -> a data race. Local
  !   allocatables are automatically thread-private per invocation.
      NP_U_L = P_MAX + 1
      NP_V_L = Q_MAX + 1
      NP_T_L = 2
      CALL Q1NP_BUILD_GAUSS_SCHEME(NP_U_L, NP_V_L, NP_T_L, &
     &     GP_U_L, GW_U_L, GP_V_L, GW_V_L, GP_T_L, GW_T_L)

  !   Q1NP_FORC3 is invoked from FORINT inside an !$OMP DO over element
  !   groups, so several threads may reach this point concurrently.
!$OMP CRITICAL(Q1NP_BULK_FIX_ALLOC)
      IF (.NOT. ALLOCATED(Q1NP_BULK_FIX_IDS) .OR. .NOT. ALLOCATED(Q1NP_BULK_FIX_READY)) THEN
        CALL MY_ALLOC(Q1NP_BULK_FIX_IDS, 4, MAX(NUMELQ1NP_G,1), "Q1NP_BULK_FIX_IDS")
        CALL MY_ALLOC(Q1NP_BULK_FIX_READY, MAX(NUMELQ1NP_G,1), "Q1NP_BULK_FIX_READY")
        Q1NP_BULK_FIX_IDS = 0
        Q1NP_BULK_FIX_READY = .FALSE.
      ELSE IF (SIZE(Q1NP_BULK_FIX_IDS,2) /= MAX(NUMELQ1NP_G,1) .OR. &
     &         SIZE(Q1NP_BULK_FIX_READY) /= MAX(NUMELQ1NP_G,1)) THEN
        CALL MY_DEALLOC(Q1NP_BULK_FIX_IDS)
        CALL MY_DEALLOC(Q1NP_BULK_FIX_READY)
        CALL MY_ALLOC(Q1NP_BULK_FIX_IDS, 4, MAX(NUMELQ1NP_G,1), "Q1NP_BULK_FIX_IDS")
        CALL MY_ALLOC(Q1NP_BULK_FIX_READY, MAX(NUMELQ1NP_G,1), "Q1NP_BULK_FIX_READY")
        Q1NP_BULK_FIX_IDS = 0
        Q1NP_BULK_FIX_READY = .FALSE.
      END IF
!$OMP END CRITICAL(Q1NP_BULK_FIX_ALLOC)

      ! Total Number of Gauss points in the Q1NP element
      NGP_Q1NP = NP_U_L * NP_V_L * NP_T_L

  !-----------------------------------------------------------------------
  !  (3) Allocate and initialize vectorized work arrays
  !-----------------------------------------------------------------------
      GBUF => ELBUF_STR%GBUF
      ELBUF_TAB_LOCAL(1) = ELBUF_STR
      IPARG_LOCAL(:,1) = IPARG(:,NG)

      CALL MY_ALLOC(NCTRL_ELEM, NEL, "NCTRL_ELEM")
      CALL MY_ALLOC(ELEM_U, NEL, "ELEM_U")
      CALL MY_ALLOC(ELEM_V, NEL, "ELEM_V")
      CALL MY_ALLOC(PID_ELEM, NEL, "PID_ELEM")
      CALL MY_ALLOC(MAT_ID_ELEM, NEL, "MAT_ID_ELEM")
      CALL MY_ALLOC(NGL_ELEM, NEL, "NGL_ELEM")
      CALL MY_ALLOC(NODE_GID, MAX_NNODE, NEL, "NODE_GID")
      CALL MY_ALLOC(NODE_LID, MAX_NNODE, NEL, "NODE_LID")
      CALL MY_ALLOC(NODE_POS, MAX_NNODE, NEL, "NODE_POS")
      CALL MY_ALLOC(GROUP_GID, TOTAL_NODE_REF, "GROUP_GID")
      CALL MY_ALLOC(GROUP_LID, TOTAL_NODE_REF, "GROUP_LID")
      CALL MY_ALLOC(X_ELEM, 3, MAX_NNODE, NEL, "X_ELEM")
      CALL MY_ALLOC(V_ELEM, 3, MAX_NNODE, NEL, "V_ELEM")
      CALL MY_ALLOC(F_INT_ELEM, 3, MAX_NNODE, NEL, "F_INT_ELEM")
      CALL MY_ALLOC(MASS_ELEM, MAX_NNODE, NEL, "MASS_ELEM")
      CALL MY_ALLOC(STIG_ELEM, MAX_NNODE, NEL, "STIG_ELEM")
      CALL MY_ALLOC(U_KNOT, U_LEN, NEL, "U_KNOT")
      CALL MY_ALLOC(V_KNOT, V_LEN, NEL, "V_KNOT")
      CALL MY_ALLOC(NVAL, MAX_NNODE, "NVAL")
      CALL MY_ALLOC(DN_LOCAL, MAX_NNODE, 3, "DN_LOCAL")
      CALL MY_ALLOC(DN_GLOBAL, MAX_NNODE, 3, "DN_GLOBAL")
      CALL MY_ALLOC(MATB_GP, 3*MAX_NNODE, NEL, "MATB_GP")
      CALL MY_ALLOC(VGAUSS, NGP_Q1NP, NEL, "VGAUSS")
      PID_ELEM = 0
      MAT_ID_ELEM = 0
      NGL_ELEM = 0

  !-----------------------------------------------------------------------
  !  (4) Snapshot the Gauss-point volume into VOL0DP
  !-----------------------------------------------------------------------
      IF (TT == ZERO) THEN
        DO IT = 1, NP_T_L
          DO IV = 1, NP_V_L
            DO IU = 1, NP_U_L
              LBUF => ELBUF_STR%BUFLY(1)%LBUF(IU,IV,IT)
              LBUF%VOL0DP(1:NEL) = LBUF%VOL(1:NEL)
            END DO
          END DO
        END DO
      END IF

  !-----------------------------------------------------------------------
  !  (5) Build node/group bookkeeping and gather X/V into element-local arrays
  !-----------------------------------------------------------------------
      Q1NP_MAP_ERROR = .FALSE.
      CALL Q1NP_INIT_NODE_MAP()
      IF (Q1NP_MAP_ERROR) RETURN

  !-----------------------------------------------------------------------
  !  (6) Initialize element fields (OFF, RHO0, DELTAX, buffers, etc.)
  !-----------------------------------------------------------------------
      CALL Q1NP_INIT_ELEM_FIELDS()

  !-----------------------------------------------------------------------
  !  (7) Small-rotation coordinate transform for small strain storage
  !-----------------------------------------------------------------------
      CALL SMALLA3(GBUF%SMSTR, GBUF%OFF, OFF, WXX, WYY, WZZ, NEL, ISMSTR, JLAG)

  !-----------------------------------------------------------------------
  !  (8) Full Gauss integration (IU,IV,IT) over all lanes (IEL=1..NEL)
  !     Each GP contributes: geometry + material law + internal force accumulation
  !-----------------------------------------------------------------------

      IPT_Q1NP = 0
      DO IT = 1, NP_T_L
        ZETA = GP_T_L(IT)
        DO IV = 1, NP_V_L
          ETA = GP_V_L(IV)
          DO IU = 1, NP_U_L
            XI = GP_U_L(IU)

            ! Gauss point weight
            GPW = GW_U_L(IU) * GW_V_L(IV) * GW_T_L(IT)
            IPT_Q1NP = IPT_Q1NP + 1

            ! Reset the Gauss point fields
            CALL Q1NP_RESET_GP()

            ! Compute the geometry terms (derivatives + gauss point volume)
            CALL Q1NP_GP_GEOM(XI, ETA, ZETA, GPW, IERR)
            IF (IERR /= 0) RETURN

            ! Compute the material and strain evaluation
            CALL Q1NP_GP_MAT(IU, IV, IT)

            ! Integrate the Gauss-point stress contributions into the internal force buffers
            CALL Q1NP_ACCUM_NFORCE(IU, IV, IT)
          END DO
        END DO
      END DO

  !-----------------------------------------------------------------------
  !  (9) Update OFF/GBUF%OFF scaling after Gauss integration
  !-----------------------------------------------------------------------
      CALL SMALLB3(GBUF%OFF, OFF, NEL, ISMSTR)

  !-----------------------------------------------------------------------
  !  (10) Assemble element-local forces into global A + update dt + stress averages
  !-----------------------------------------------------------------------
      CALL Q1NP_ASSEMBLE_FINT() ! Assemble internal forces into the global array A
      CALL Q1NP_AVG_SIG_BILAN() ! Update the element-local stress tensor and compute the stress average and bilans

      CONTAINS

!=======================================================================
! Node/group bookkeeping and gather X/V into element-local arrays
!=======================================================================
        SUBROUTINE Q1NP_INIT_NODE_MAP()
          INTEGER :: BULK_NODE_IDS(4)
          NODE_GID = 0 ! Global node ID
          NODE_LID = 0 ! Local node ID
          NODE_POS = 0 ! Position of the node in the group
          GROUP_GID = 0 ! Global group ID
          GROUP_LID = 0 ! Local group ID
          X_ELEM = ZERO ! Element-local coordinate
          V_ELEM = ZERO ! Element-local velocity
          F_INT_ELEM = ZERO ! Element-local internal force
          MASS_ELEM = ZERO ! Element-local mass
          STIG_ELEM = ZERO ! Element-local stiffness
          U_KNOT = ZERO ! Element-local knot vector
          V_KNOT = ZERO ! Element-local knot vector
          MATB_GP = ZERO ! Material matrix for the Gauss point
          VGAUSS = ZERO ! Gauss point values
          NUM_GROUP_NODE = 0 ! Number of group nodes

          ! Build the element-local node lists (control points + 4 bulk nodes)
          DO IEL = 1, NEL
            IQ1NP = Q1NP_IDS(IEL)
            NCTRL_ELEM(IEL) = KQ1NP_TAB(3,IQ1NP)

            ELEM_U(IEL) = KQ1NP_TAB(6,IQ1NP)
            ELEM_V(IEL) = KQ1NP_TAB(7,IQ1NP)

            PID_ELEM(IEL) = KQ1NP_TAB(2,IQ1NP)
            MAT_ID_ELEM(IEL) = KQ1NP_TAB(1,IQ1NP)

            NGL_ELEM(IEL) = KQ1NP_TAB(5,IQ1NP)

            ! Per-element knot vectors (heterogeneous NX/NY via knot_set_id in KQ1NP_TAB(15,:)).
            KNOT_SET_ID = KQ1NP_TAB(15, IQ1NP)
            IF (Q1NP_NKNOT_SETS_G > 0 .AND. KNOT_SET_ID > 0 .AND. &
     &          KNOT_SET_ID <= Q1NP_NKNOT_SETS_G) THEN
              U_KNOT(1:U_LEN_EL(IEL),IEL) = &
     &            Q1NP_KTAB(Q1NP_KTAB_OFF_G(KNOT_SET_ID) : &
     &                     Q1NP_KTAB_OFF_G(KNOT_SET_ID) + U_LEN_EL(IEL) - 1)
              V_KNOT(1:V_LEN_EL(IEL),IEL) = &
     &            Q1NP_KTAB(Q1NP_KTAB_OFF_G(KNOT_SET_ID) + U_LEN_EL(IEL) : &
     &                     Q1NP_KTAB_OFF_G(KNOT_SET_ID) + U_LEN_EL(IEL) + V_LEN_EL(IEL) - 1)
            ELSE
              U_KNOT(1:U_LEN_EL(IEL),IEL) = Q1NP_KTAB(1:U_LEN_EL(IEL))
              V_KNOT(1:V_LEN_EL(IEL),IEL) = Q1NP_KTAB(U_LEN_EL(IEL)+1:U_LEN_EL(IEL)+V_LEN_EL(IEL))
            ENDIF

            ! Get the control point IDs
            DO K = 1, NCTRL_ELEM(IEL)
              NODE_GID(K,IEL) = IQ1NP_TAB(KQ1NP_TAB(4,IQ1NP) + K - 1)
            END DO
            CALL Q1NP_GET_BULK_NODE_IDS(IEL, IQ1NP, BULK_NODE_IDS)
            DO K = 1, 4
              NODE_GID(NCTRL_ELEM(IEL) + K, IEL) = BULK_NODE_IDS(K)
            END DO

            ! Get the group node IDs
            DO K = 1, NCTRL_ELEM(IEL) + 4
              GPOS = FIND_GROUP_NODE(NODE_GID(K,IEL), GROUP_GID, NUM_GROUP_NODE)
              IF (GPOS <= 0) THEN
                NUM_GROUP_NODE = NUM_GROUP_NODE + 1
                GROUP_GID(NUM_GROUP_NODE) = NODE_GID(K,IEL)
                GPOS = NUM_GROUP_NODE
              END IF
              NODE_POS(K,IEL) = GPOS
            END DO
          END DO

          ! IQ1NP_TAB / IQ1NP_BULK_TAB store engine-local node indices after
          ! control-point promotion in starter. Do not remap them via ITAB_DEBUG.
          DO I = 1, NUM_GROUP_NODE
            GROUP_LID(I) = GROUP_GID(I)
          END DO

          ! Element coordinate and velocity arrays
          DO IEL = 1, NEL
            DO K = 1, NCTRL_ELEM(IEL) + 4
              LOCAL_ID = GROUP_LID(NODE_POS(K,IEL))
              IF (LOCAL_ID <= 0 .OR. LOCAL_ID > NUMNOD) THEN
                WRITE(*,'(A,I10,A,I10,A,I10,A,I10,A,I10)') &
     &            'Q1NP ERROR: invalid local node id for element ', NGL_ELEM(IEL), &
     &            ' lane ', IEL, ' slot ', K, ' raw id ', NODE_GID(K,IEL), &
     &            ' local id ', LOCAL_ID
                Q1NP_MAP_ERROR = .TRUE.
                RETURN
              END IF
              NODE_LID(K,IEL) = LOCAL_ID
              X_ELEM(1,K,IEL) = X(1,LOCAL_ID)
              X_ELEM(2,K,IEL) = X(2,LOCAL_ID)
              X_ELEM(3,K,IEL) = X(3,LOCAL_ID)
              V_ELEM(1,K,IEL) = V(1,LOCAL_ID)
              V_ELEM(2,K,IEL) = V(2,LOCAL_ID)
              V_ELEM(3,K,IEL) = V(3,LOCAL_ID)
            END DO
          END DO

        END SUBROUTINE Q1NP_INIT_NODE_MAP

!=======================================================================
! Resolve the 4 bulk nodes for one Q1NP element.
!=======================================================================
        SUBROUTINE Q1NP_GET_BULK_NODE_IDS(IEL_LOCAL, IQ1NP_LOCAL, BULK_NODE_IDS_OUT)
          INTEGER, INTENT(IN)  :: IEL_LOCAL, IQ1NP_LOCAL
          INTEGER, INTENT(OUT) :: BULK_NODE_IDS_OUT(4)
          INTEGER :: STORED_BULK(4), REBUILT_BULK(4), CENTROID_BULK(4)
          INTEGER :: K_LOCAL, IERR_LOCAL, IERR_CENTROID, OFF14, TOP_FACE
          INTEGER :: STORED_SORTED(4), REBUILT_SORTED(4), CENTROID_SORTED(4)
          LOGICAL :: BULK_SETS_MATCH, BULK_VS_CENTROID_MATCH
          my_real :: MATCH_SCORE, CENTROID_DIST

          OFF14 = KQ1NP_TAB(14,IQ1NP_LOCAL)
          DO K_LOCAL = 1, 4
            STORED_BULK(K_LOCAL) = IQ1NP_BULK_TAB(OFF14 + K_LOCAL - 1)
          END DO

          IF (.NOT. Q1NP_BULK_FIX_READY(IQ1NP_LOCAL)) THEN
            ! Try CENTROID-based reconstruction first: pick HEX8 face whose
            ! centroid is closest to the top-patch centroid. This is much
            ! more robust on strongly distorted bricks than the original
            ! corner-distance scoring (which can pick a face that has a
            ! HEX corner accidentally near the patch centroid).
            CALL Q1NP_REBUILD_BULK_BY_CENTROID(IEL_LOCAL, IQ1NP_LOCAL, &
     &                                          CENTROID_BULK, TOP_FACE, &
     &                                          CENTROID_DIST, IERR_CENTROID)

            ! Also call the legacy corner-score rebuild for cross-checking.
            CALL Q1NP_REBUILD_BULK_FROM_HEX(IEL_LOCAL, IQ1NP_LOCAL, &
     &                                      REBUILT_BULK, MATCH_SCORE, IERR_LOCAL)

            IF (MINVAL(STORED_BULK) <= 0 .OR. &
     &          MAXVAL(STORED_BULK) > NUMNOD) THEN
              ! Stored bulk invalid: prefer centroid result, fall back to corner.
              IF (IERR_CENTROID == 0) THEN
                Q1NP_BULK_FIX_IDS(1:4,IQ1NP_LOCAL) = CENTROID_BULK(1:4)
              ELSE IF (IERR_LOCAL == 0) THEN
                Q1NP_BULK_FIX_IDS(1:4,IQ1NP_LOCAL) = REBUILT_BULK(1:4)
              ELSE
                Q1NP_BULK_FIX_IDS(1:4,IQ1NP_LOCAL) = STORED_BULK(1:4)
              END IF
            ELSE
              ! Stored bulk has valid IDs. Cross-check STORED vs both
              ! reconstructions. We trust the CENTROID result over STORED
              ! when they disagree (Starter's corner-score matcher is the
              ! same algorithm that built STORED and tends to repeat the
              ! same wrong face on distorted bricks).
              IF (IERR_CENTROID == 0) THEN
                STORED_SORTED   = STORED_BULK
                CENTROID_SORTED = CENTROID_BULK
                CALL Q1NP_SORT4_ASC(STORED_SORTED)
                CALL Q1NP_SORT4_ASC(CENTROID_SORTED)
                BULK_VS_CENTROID_MATCH = .TRUE.
                DO K_LOCAL = 1, 4
                  IF (STORED_SORTED(K_LOCAL) /= CENTROID_SORTED(K_LOCAL)) THEN
                    BULK_VS_CENTROID_MATCH = .FALSE.
                    EXIT
                  END IF
                END DO
                IF (BULK_VS_CENTROID_MATCH) THEN
                  ! Same set; keep stored order.
                  Q1NP_BULK_FIX_IDS(1:4,IQ1NP_LOCAL) = STORED_BULK(1:4)
                ELSE
                  ! Centroid result differs: override.
                  Q1NP_BULK_FIX_IDS(1:4,IQ1NP_LOCAL) = CENTROID_BULK(1:4)
                  WRITE(*,'(A,I10,A,4I8,A,4I8,A,I2,A,1PE10.3)') &
     &              ' Q1NP NOTE (centroid override) elem_uid=', KQ1NP_TAB(5,IQ1NP_LOCAL), &
     &              ' stored=', STORED_BULK, &
     &              ' centroid_bulk=', CENTROID_BULK, &
     &              ' top_face=', TOP_FACE, ' dist=', CENTROID_DIST
                END IF
              ELSE IF (IERR_LOCAL == 0) THEN
                ! Centroid failed but corner-score worked: cross-check that.
                STORED_SORTED  = STORED_BULK
                REBUILT_SORTED = REBUILT_BULK
                CALL Q1NP_SORT4_ASC(STORED_SORTED)
                CALL Q1NP_SORT4_ASC(REBUILT_SORTED)
                BULK_SETS_MATCH = .TRUE.
                DO K_LOCAL = 1, 4
                  IF (STORED_SORTED(K_LOCAL) /= REBUILT_SORTED(K_LOCAL)) THEN
                    BULK_SETS_MATCH = .FALSE.
                    EXIT
                  END IF
                END DO
                IF (BULK_SETS_MATCH) THEN
                  Q1NP_BULK_FIX_IDS(1:4,IQ1NP_LOCAL) = STORED_BULK(1:4)
                ELSE
                  Q1NP_BULK_FIX_IDS(1:4,IQ1NP_LOCAL) = REBUILT_BULK(1:4)
                  WRITE(*,'(A,I10,A,4I8,A,4I8)') &
     &              ' Q1NP NOTE: stored bulk differs from HEX rebuild for elem_uid=', KQ1NP_TAB(5,IQ1NP_LOCAL), &
     &              ' stored=', STORED_BULK, &
     &              ' rebuilt=', REBUILT_BULK
                END IF
              ELSE
                ! Both reconstructions failed: keep stored.
                Q1NP_BULK_FIX_IDS(1:4,IQ1NP_LOCAL) = STORED_BULK(1:4)
              END IF
            END IF
            Q1NP_BULK_FIX_READY(IQ1NP_LOCAL) = .TRUE.
          END IF

          BULK_NODE_IDS_OUT(1:4) = Q1NP_BULK_FIX_IDS(1:4,IQ1NP_LOCAL)
        END SUBROUTINE Q1NP_GET_BULK_NODE_IDS

!=======================================================================
! Sort an INTEGER(4) array ascending in place (insertion sort).
!=======================================================================
        SUBROUTINE Q1NP_SORT4_ASC(ARR)
          INTEGER, INTENT(INOUT) :: ARR(4)
          INTEGER :: I_LOC, J_LOC, KEY_LOC
          DO I_LOC = 2, 4
            KEY_LOC = ARR(I_LOC)
            J_LOC = I_LOC - 1
            DO WHILE (J_LOC >= 1)
              IF (ARR(J_LOC) <= KEY_LOC) EXIT
              ARR(J_LOC + 1) = ARR(J_LOC)
              J_LOC = J_LOC - 1
            END DO
            ARR(J_LOC + 1) = KEY_LOC
          END DO
        END SUBROUTINE Q1NP_SORT4_ASC

!=======================================================================
! Recover the opposite HEX8 face by matching the fitted Q1NP top patch to
! the parent brick faces.
!=======================================================================
        SUBROUTINE Q1NP_REBUILD_BULK_FROM_HEX(IEL_LOCAL, IQ1NP_LOCAL, BULK_NODE_IDS_OUT, MATCH_SCORE_OUT, IERR_OUT)
          INTEGER, INTENT(IN)  :: IEL_LOCAL, IQ1NP_LOCAL
          INTEGER, INTENT(OUT) :: BULK_NODE_IDS_OUT(4)
          my_real, INTENT(OUT) :: MATCH_SCORE_OUT
          INTEGER, INTENT(OUT) :: IERR_OUT
          INTEGER :: IEL_HEX8, IFACE, IFOPP, IPERM, K_LOCAL, J_LOCAL
          INTEGER :: FACE_IXS(4,6), OPPOSITE(6), OPP_PAIR(4,6), CORNER_PERM(4,8)
          INTEGER :: NODES_FACE(4), NODES_OPP(4), CANDIDATE_BULK(4)
          my_real :: TOP_CORNER(3,4), DIFF_LOCAL(3), SCORE_LOCAL, BEST_SCORE
          DATA FACE_IXS / &
     &      2,3,4,5, 6,7,8,9, 2,3,7,6, 3,4,8,7, 4,5,9,8, 5,2,6,9 /
          DATA OPPOSITE / 2,1,5,6,3,4 /
          DATA OPP_PAIR / &
     &      1,2,3,4, 1,2,3,4, 2,1,4,3, 2,1,4,3, 2,1,4,3, 2,1,4,3 /
          DATA CORNER_PERM / &
     &      1,2,3,4, &
     &      2,3,4,1, &
     &      3,4,1,2, &
     &      4,1,2,3, &
     &      1,4,3,2, &
     &      4,3,2,1, &
     &      3,2,1,4, &
     &      2,1,4,3 /

          BULK_NODE_IDS_OUT = 0
          MATCH_SCORE_OUT = HUGE(ONE)
          IERR_OUT = 1

          IEL_HEX8 = KQ1NP_TAB(10,IQ1NP_LOCAL)
          IF (IEL_HEX8 > 0 .AND. IEL_HEX8 <= NUMELS) THEN
            IF (IXS(NIXS,IEL_HEX8) /= KQ1NP_TAB(5,IQ1NP_LOCAL)) IEL_HEX8 = 0
          ELSE
            IEL_HEX8 = 0
          END IF
          IF (IEL_HEX8 <= 0) THEN
            ! Direct O(1) recovery: the group loop maps in-group position
            ! IEL_LOCAL to Q1NP element IQ1NP_LOCAL via KQ1NP_TAB_INV, so the
            ! parent HEX8 is local solid element NFT_G + IEL_LOCAL.
            IEL_HEX8 = NFT_G + IEL_LOCAL
            IF (IEL_HEX8 > 0 .AND. IEL_HEX8 <= NUMELS) THEN
              IF (IXS(NIXS,IEL_HEX8) == KQ1NP_TAB(5,IQ1NP_LOCAL)) THEN
                KQ1NP_TAB(10,IQ1NP_LOCAL) = IEL_HEX8
              ELSE
                IEL_HEX8 = 0
              END IF
            ELSE
              IEL_HEX8 = 0
            END IF
          END IF
          IF (IEL_HEX8 <= 0 .OR. IEL_HEX8 > NUMELS) RETURN

          CALL Q1NP_EVAL_TOP_SURF_POINT(IEL_LOCAL, -ONE, -ONE, TOP_CORNER(1:3,1))
          CALL Q1NP_EVAL_TOP_SURF_POINT(IEL_LOCAL,  ONE, -ONE, TOP_CORNER(1:3,2))
          CALL Q1NP_EVAL_TOP_SURF_POINT(IEL_LOCAL,  ONE,  ONE, TOP_CORNER(1:3,3))
          CALL Q1NP_EVAL_TOP_SURF_POINT(IEL_LOCAL, -ONE,  ONE, TOP_CORNER(1:3,4))

          BEST_SCORE = HUGE(ONE)
          DO IFACE = 1, 6
            IFOPP = OPPOSITE(IFACE)
            DO K_LOCAL = 1, 4
              NODES_FACE(K_LOCAL) = IXS(FACE_IXS(K_LOCAL,IFACE), IEL_HEX8)
              NODES_OPP(K_LOCAL) = IXS(FACE_IXS(K_LOCAL,IFOPP), IEL_HEX8)
            END DO

            DO IPERM = 1, 8
              SCORE_LOCAL = ZERO
              DO K_LOCAL = 1, 4
                J_LOCAL = CORNER_PERM(K_LOCAL,IPERM)
                IF (NODES_FACE(J_LOCAL) <= 0 .OR. NODES_FACE(J_LOCAL) > NUMNOD) THEN
                  SCORE_LOCAL = HUGE(ONE)
                  EXIT
                END IF

                DIFF_LOCAL(1:3) = TOP_CORNER(1:3,K_LOCAL) - X(1:3,NODES_FACE(J_LOCAL))
                SCORE_LOCAL = SCORE_LOCAL + DOT_PRODUCT(DIFF_LOCAL, DIFF_LOCAL)
                CANDIDATE_BULK(K_LOCAL) = NODES_OPP(OPP_PAIR(J_LOCAL,IFACE))
              END DO

              IF (SCORE_LOCAL < BEST_SCORE) THEN
                BEST_SCORE = SCORE_LOCAL
                BULK_NODE_IDS_OUT(1:4) = CANDIDATE_BULK(1:4)
              END IF
            END DO
          END DO

          MATCH_SCORE_OUT = BEST_SCORE
          IF (MINVAL(BULK_NODE_IDS_OUT) <= 0 .OR. MAXVAL(BULK_NODE_IDS_OUT) > NUMNOD) RETURN

          IERR_OUT = 0
        END SUBROUTINE Q1NP_REBUILD_BULK_FROM_HEX

!=======================================================================
! Alternative bulk reconstruction using CENTROID matching.
! For each of the 6 HEX8 faces, compute its centroid and pick the face
! whose centroid is closest to the NURBS top-patch centroid. The bulk
! face is the OPPOSITE HEX8 face; the 4 bulk nodes are then ordered
! so each one is paired with the geometrically closest TOP_CORNER.
! This is much more robust than corner-distance scoring on strongly
! distorted bricks where one HEX corner accidentally lives near the
! center of the patch.
!=======================================================================
        SUBROUTINE Q1NP_REBUILD_BULK_BY_CENTROID(IEL_LOCAL, IQ1NP_LOCAL, &
     &                                          BULK_NODE_IDS_OUT, TOP_FACE_OUT, &
     &                                          DIST_OUT, IERR_OUT)
          INTEGER, INTENT(IN)  :: IEL_LOCAL, IQ1NP_LOCAL
          INTEGER, INTENT(OUT) :: BULK_NODE_IDS_OUT(4)
          INTEGER, INTENT(OUT) :: TOP_FACE_OUT
          my_real, INTENT(OUT) :: DIST_OUT
          INTEGER, INTENT(OUT) :: IERR_OUT
          INTEGER :: IEL_HEX8, IFACE, IFOPP, K_LOCAL, J_LOCAL, IBEST
          INTEGER :: FACE_IXS(4,6), OPPOSITE(6)
          INTEGER :: NODES_FACE_LOCAL(4,6), NODES_OPP_LOCAL(4,6)
          my_real :: FACE_CENTROID(3,6), TOP_CORNER(3,4), TOP_CENTROID(3)
          my_real :: D_LOCAL(3), DIST_LOCAL, BEST_DIST
          INTEGER :: USED_LOCAL(4), IBEST_NODE
          my_real :: BEST_NODE_DIST, D_CORNER(3), DC_NORM
          DATA FACE_IXS / &
     &      2,3,4,5, 6,7,8,9, 2,3,7,6, 3,4,8,7, 4,5,9,8, 5,2,6,9 /
          DATA OPPOSITE / 2,1,5,6,3,4 /

          BULK_NODE_IDS_OUT = 0
          TOP_FACE_OUT = 0
          DIST_OUT = HUGE(ONE)
          IERR_OUT = 1

          IEL_HEX8 = KQ1NP_TAB(10,IQ1NP_LOCAL)
          IF (IEL_HEX8 > 0 .AND. IEL_HEX8 <= NUMELS) THEN
            IF (IXS(NIXS,IEL_HEX8) /= KQ1NP_TAB(5,IQ1NP_LOCAL)) IEL_HEX8 = 0
          ELSE
            IEL_HEX8 = 0
          END IF
          IF (IEL_HEX8 <= 0) THEN
            ! Direct O(1) recovery: the group loop maps in-group position
            ! IEL_LOCAL to Q1NP element IQ1NP_LOCAL via KQ1NP_TAB_INV, so the
            ! parent HEX8 is local solid element NFT_G + IEL_LOCAL.
            IEL_HEX8 = NFT_G + IEL_LOCAL
            IF (IEL_HEX8 > 0 .AND. IEL_HEX8 <= NUMELS) THEN
              IF (IXS(NIXS,IEL_HEX8) == KQ1NP_TAB(5,IQ1NP_LOCAL)) THEN
                KQ1NP_TAB(10,IQ1NP_LOCAL) = IEL_HEX8
              ELSE
                IEL_HEX8 = 0
              END IF
            ELSE
              IEL_HEX8 = 0
            END IF
          END IF
          IF (IEL_HEX8 <= 0 .OR. IEL_HEX8 > NUMELS) RETURN

          ! Top-patch corners and centroid (parametric (-1,-1)...(-1,+1)).
          CALL Q1NP_EVAL_TOP_SURF_POINT(IEL_LOCAL, -ONE, -ONE, TOP_CORNER(1:3,1))
          CALL Q1NP_EVAL_TOP_SURF_POINT(IEL_LOCAL,  ONE, -ONE, TOP_CORNER(1:3,2))
          CALL Q1NP_EVAL_TOP_SURF_POINT(IEL_LOCAL,  ONE,  ONE, TOP_CORNER(1:3,3))
          CALL Q1NP_EVAL_TOP_SURF_POINT(IEL_LOCAL, -ONE,  ONE, TOP_CORNER(1:3,4))
          TOP_CENTROID(1:3) = FOURTH * (TOP_CORNER(1:3,1) + TOP_CORNER(1:3,2) + &
     &                                  TOP_CORNER(1:3,3) + TOP_CORNER(1:3,4))

          ! Cache face nodes and compute face centroids.
          DO IFACE = 1, 6
            DO K_LOCAL = 1, 4
              NODES_FACE_LOCAL(K_LOCAL,IFACE) = IXS(FACE_IXS(K_LOCAL,IFACE),IEL_HEX8)
            END DO
            IFOPP = OPPOSITE(IFACE)
            DO K_LOCAL = 1, 4
              NODES_OPP_LOCAL(K_LOCAL,IFACE) = IXS(FACE_IXS(K_LOCAL,IFOPP),IEL_HEX8)
            END DO

            IF (MINVAL(NODES_FACE_LOCAL(1:4,IFACE)) <= 0 .OR. &
     &          MAXVAL(NODES_FACE_LOCAL(1:4,IFACE)) > NUMNOD) THEN
              FACE_CENTROID(1:3,IFACE) = HUGE(ONE)
              CYCLE
            END IF
            FACE_CENTROID(1:3,IFACE) = FOURTH * ( &
     &        X(1:3,NODES_FACE_LOCAL(1,IFACE)) + &
     &        X(1:3,NODES_FACE_LOCAL(2,IFACE)) + &
     &        X(1:3,NODES_FACE_LOCAL(3,IFACE)) + &
     &        X(1:3,NODES_FACE_LOCAL(4,IFACE)))
          END DO

          ! Pick the HEX8 face whose centroid is closest to the top-patch centroid.
          BEST_DIST = HUGE(ONE)
          IBEST = 0
          DO IFACE = 1, 6
            IF (FACE_CENTROID(1,IFACE) >= HUGE(ONE)*HALF) CYCLE
            D_LOCAL(1:3) = FACE_CENTROID(1:3,IFACE) - TOP_CENTROID(1:3)
            DIST_LOCAL = SQRT(DOT_PRODUCT(D_LOCAL,D_LOCAL))
            IF (DIST_LOCAL < BEST_DIST) THEN
              BEST_DIST = DIST_LOCAL
              IBEST = IFACE
            END IF
          END DO
          IF (IBEST <= 0) RETURN

          TOP_FACE_OUT = IBEST
          DIST_OUT = BEST_DIST

          ! Bulk is the OPPOSITE face; order the 4 bulk nodes so that
          ! bulk(k) is the node closest to TOP_CORNER(k).
          USED_LOCAL = 0
          DO K_LOCAL = 1, 4
            BEST_NODE_DIST = HUGE(ONE)
            IBEST_NODE = 0
            DO J_LOCAL = 1, 4
              IF (USED_LOCAL(J_LOCAL) == 1) CYCLE
              D_CORNER(1:3) = TOP_CORNER(1:3,K_LOCAL) - &
     &                        X(1:3,NODES_OPP_LOCAL(J_LOCAL,IBEST))
              DC_NORM = DOT_PRODUCT(D_CORNER, D_CORNER)
              IF (DC_NORM < BEST_NODE_DIST) THEN
                BEST_NODE_DIST = DC_NORM
                IBEST_NODE = J_LOCAL
              END IF
            END DO
            IF (IBEST_NODE <= 0) RETURN
            BULK_NODE_IDS_OUT(K_LOCAL) = NODES_OPP_LOCAL(IBEST_NODE,IBEST)
            USED_LOCAL(IBEST_NODE) = 1
          END DO

          IF (MINVAL(BULK_NODE_IDS_OUT) <= 0 .OR. MAXVAL(BULK_NODE_IDS_OUT) > NUMNOD) RETURN
          IERR_OUT = 0
        END SUBROUTINE Q1NP_REBUILD_BULK_BY_CENTROID

!=======================================================================
! Evaluate one top-surface point using only the Q1NP control points.
!=======================================================================
        SUBROUTINE Q1NP_EVAL_TOP_SURF_POINT(IEL_LOCAL, XI_LOCAL, ETA_LOCAL, XYZ_OUT)
          INTEGER, INTENT(IN) :: IEL_LOCAL
          my_real, INTENT(IN) :: XI_LOCAL, ETA_LOCAL
          my_real, INTENT(OUT) :: XYZ_OUT(3)
          INTEGER :: IQ1NP_LOCAL, P_LOCAL, Q_LOCAL
          INTEGER :: NNODE_LOCAL, K_LOCAL, LOCAL_ID
          my_real :: NVAL_LOCAL(MAX_NNODE)
          my_real :: DN_TMP(MAX_NNODE,3)

          IQ1NP_LOCAL = Q1NP_IDS(IEL_LOCAL)
          P_LOCAL = KQ1NP_TAB(8,IQ1NP_LOCAL)
          Q_LOCAL = KQ1NP_TAB(9,IQ1NP_LOCAL)
          NNODE_LOCAL = NCTRL_ELEM(IEL_LOCAL) + 4

          CALL Q1NP_SHAPE_FUNCTIONS(XI_LOCAL, ETA_LOCAL, ONE, P_LOCAL, Q_LOCAL, &
     &                              U_KNOT(1:U_LEN_EL(IEL_LOCAL),IEL_LOCAL), &
     &                              V_KNOT(1:V_LEN_EL(IEL_LOCAL),IEL_LOCAL), &
     &                              ELEM_U(IEL_LOCAL), ELEM_V(IEL_LOCAL), &
     &                              NVAL_LOCAL(1:NNODE_LOCAL), DN_TMP(1:NNODE_LOCAL,1:3))

          XYZ_OUT = ZERO
          DO K_LOCAL = 1, NCTRL_ELEM(IEL_LOCAL)
            LOCAL_ID = NODE_GID(K_LOCAL,IEL_LOCAL)
            IF (LOCAL_ID <= 0 .OR. LOCAL_ID > NUMNOD) CYCLE
            XYZ_OUT(1:3) = XYZ_OUT(1:3) + NVAL_LOCAL(K_LOCAL) * X(1:3,LOCAL_ID)
          END DO
        END SUBROUTINE Q1NP_EVAL_TOP_SURF_POINT

!=======================================================================
! Initialize element fields (OFF, RHO0, DELTAX, buffers, etc.)
! Compute characteristic length per element that is later reused in material law and time-step logic.
!=======================================================================
        SUBROUTINE Q1NP_INIT_ELEM_FIELDS()
          INTEGER :: I_INIT, IEL_LOCAL

          ! Initialize the arrays for the Q1NP element
          DO I_INIT = 1, 6
            II(I_INIT) = NEL * (I_INIT - 1)
          END DO

          ISTRAIN = IPARG(44,NG)
          ILAY = 1
          IEXPAN = IPARG(49,NG)
          TH_STRAIN = 0
          SZ_IX = NUMELQ + NUMELS + NSVOIS

          STI = ZERO
          OFF = ONE
          RHO0 = ZERO
          VOLN = ZERO
          VOLG = ZERO
          DVOL = ZERO
          VD2 = ZERO
          DELTAX = ZERO
          DIVDE = ZERO
          VIS = ZERO
          QVIS = ZERO
          CXX = ZERO
          S1 = ZERO
          S2 = ZERO
          S3 = ZERO
          S4 = ZERO
          S5 = ZERO
          S6 = ZERO
          DXX = ZERO
          DYY = ZERO
          DZZ = ZERO
          DXY = ZERO
          DYX = ZERO
          DYZ = ZERO
          DZY = ZERO
          DZX = ZERO
          DXZ = ZERO
          D4 = ZERO
          D5 = ZERO
          D6 = ZERO
          WXX = ZERO
          WYY = ZERO
          WZZ = ZERO
          AJ1 = ZERO
          AJ2 = ZERO
          AJ3 = ZERO
          AJ4 = ZERO
          AJ5 = ZERO
          AJ6 = ZERO
          VDX = ZERO
          VDY = ZERO
          VDZ = ZERO
          MUVOID = ZERO
          SSP_EQ = ZERO
          AIRE = ZERO
          SIGY = ZERO
          ET = ZERO
          BUFVOIS = ZERO
          R3_DAM = ZERO
          AMU = ZERO
          MFXX = ZERO
          MFXY = ZERO
          MFXZ = ZERO
          MFYX = ZERO
          MFYY = ZERO
          MFYZ = ZERO
          MFZX = ZERO
          MFZY = ZERO
          MFZZ = ZERO
          GAMA = ZERO
          FR_WAV = ZERO
          TEMPEL = ZERO
          DIE = ZERO
          FHEAT = ZERO
          VARNL = ZERO
          CONDE = ZERO
          FVD2 = ZERO
          FDELTAX = ZERO
          FSSP = ZERO
          FQVIS = ZERO
          DUMMY_FLUX = ZERO

          IF (ASSOCIATED(GBUF%SIG)) GBUF%SIG(1:6*NEL) = ZERO
          IF (ASSOCIATED(GBUF%EINT)) GBUF%EINT(1:NEL) = ZERO
          IF (ASSOCIATED(GBUF%RHO)) GBUF%RHO(1:NEL) = ZERO
          IF (ASSOCIATED(GBUF%QVIS)) GBUF%QVIS(1:NEL) = ZERO
          IF (GBUF%G_PLA > 0 .AND. ASSOCIATED(GBUF%PLA)) GBUF%PLA(1:NEL) = ZERO
          IF (IPARG(40,NG) > 0 .AND. ASSOCIATED(GBUF%EPSD)) GBUF%EPSD(1:NEL) = ZERO

          DO IEL_LOCAL = 1, NEL
            OFF(IEL_LOCAL) = GBUF%OFF(IEL_LOCAL)
            RHO0(IEL_LOCAL) = PM(1,MAT_ID_ELEM(IEL_LOCAL))
            CALL Q1NP_CHAR_LEN(IEL_LOCAL, DELTAX(IEL_LOCAL))
!            IF (ASSOCIATED(GBUF%DELTAX)) THEN
!              IF (TT == ZERO .OR. GBUF%DELTAX(IEL_LOCAL) <= EPSILON(ONE)) THEN
!                GBUF%DELTAX(IEL_LOCAL) = DELTAX(IEL_LOCAL)
!              ELSE
!                DELTAX(IEL_LOCAL) = MIN(DELTAX(IEL_LOCAL), GBUF%DELTAX(IEL_LOCAL))
!              END IF
!            END IF
          END DO
        END SUBROUTINE Q1NP_INIT_ELEM_FIELDS

!=======================================================================
! Reset Gauss point fields to zero
!=======================================================================
        SUBROUTINE Q1NP_RESET_GP()
          VIS(1:NEL) = ZERO
          QVIS(1:NEL) = ZERO
          CXX(1:NEL) = ZERO
          MATB_GP(1:3*MAX_NNODE,1:NEL) = ZERO
          S1(1:NEL) = ZERO
          S2(1:NEL) = ZERO
          S3(1:NEL) = ZERO
          S4(1:NEL) = ZERO
          S5(1:NEL) = ZERO
          S6(1:NEL) = ZERO
          DXX(1:NEL) = ZERO
          DYY(1:NEL) = ZERO
          DZZ(1:NEL) = ZERO
          DXY(1:NEL) = ZERO
          DYX(1:NEL) = ZERO
          DYZ(1:NEL) = ZERO
          DZY(1:NEL) = ZERO
          DZX(1:NEL) = ZERO
          DXZ(1:NEL) = ZERO
          D4(1:NEL) = ZERO
          D5(1:NEL) = ZERO
          D6(1:NEL) = ZERO
          WXX(1:NEL) = ZERO
          WYY(1:NEL) = ZERO
          WZZ(1:NEL) = ZERO
          AJ1(1:NEL) = ZERO
          AJ2(1:NEL) = ZERO
          AJ3(1:NEL) = ZERO
          AJ4(1:NEL) = ZERO
          AJ5(1:NEL) = ZERO
          AJ6(1:NEL) = ZERO
          VDX(1:NEL) = ZERO
          VDY(1:NEL) = ZERO
          VDZ(1:NEL) = ZERO
          MUVOID(1:NEL) = ZERO
          SSP_EQ(1:NEL) = ZERO
          AIRE(1:NEL) = ZERO
          SIGY(1:NEL) = ZERO
          ET(1:NEL) = ZERO
          BUFVOIS(1:NEL) = ZERO
          R3_DAM(1:NEL) = ZERO
          AMU(1:NEL) = ZERO
          MFXX(1:NEL) = ZERO
          MFXY(1:NEL) = ZERO
          MFXZ(1:NEL) = ZERO
          MFYX(1:NEL) = ZERO
          MFYY(1:NEL) = ZERO
          MFYZ(1:NEL) = ZERO
          MFZX(1:NEL) = ZERO
          MFZY(1:NEL) = ZERO
          MFZZ(1:NEL) = ZERO
          GAMA(1:NEL,1:6) = ZERO
          FR_WAV(1:NEL) = ZERO
          TEMPEL(1:NEL) = ZERO
          DIE(1:NEL) = ZERO
          FHEAT(1:NEL) = ZERO
          VARNL(1:NEL) = ZERO
          CONDE(1:NEL) = ZERO
          FVD2(1:NEL) = ZERO
          FDELTAX(1:NEL) = ZERO
          FSSP(1:NEL) = ZERO
          FQVIS(1:NEL) = ZERO
          STI(1:NEL) = ZERO
        END SUBROUTINE Q1NP_RESET_GP

!=======================================================================
! Geometry terms (derivatives + Gauss point volume)
!=======================================================================
        SUBROUTINE Q1NP_GP_GEOM(XI, ETA, ZETA, GPW, IERR_OUT)
          my_real, INTENT(IN)  :: XI, ETA, ZETA, GPW
          INTEGER, INTENT(OUT) :: IERR_OUT
          INTEGER :: IEL_LOCAL, NNODE_LOCAL, K_LOCAL, P_LOCAL, Q_LOCAL
          my_real :: JMAT_LOCAL(3,3), JINV_LOCAL(3,3), DETJ_LOCAL
          INTEGER :: IERR_LOCAL
          INTEGER :: IEL_HEX8_DBG, K_DBG, IQ1NP_DBG

          IERR_OUT = 0

          DO IEL_LOCAL = 1, NEL
            NNODE_LOCAL = NCTRL_ELEM(IEL_LOCAL) + 4
            P_LOCAL = KQ1NP_TAB(8, Q1NP_IDS(IEL_LOCAL))
            Q_LOCAL = KQ1NP_TAB(9, Q1NP_IDS(IEL_LOCAL))

            CALL Q1NP_SHAPE_FUNCTIONS(XI, ETA, ZETA, P_LOCAL, Q_LOCAL, &
     &                                  U_KNOT(1:U_LEN_EL(IEL_LOCAL),IEL_LOCAL), &
     &                                  V_KNOT(1:V_LEN_EL(IEL_LOCAL),IEL_LOCAL), &
     &                                  ELEM_U(IEL_LOCAL), ELEM_V(IEL_LOCAL), &
     &                                  NVAL(1:NNODE_LOCAL), DN_LOCAL(1:NNODE_LOCAL,1:3))

            CALL Q1NP_JACOBIAN(DN_LOCAL(1:NNODE_LOCAL,1:3), X_ELEM(1:3,1:NNODE_LOCAL,IEL_LOCAL), &
     &                           NNODE_LOCAL, JMAT_LOCAL, DETJ_LOCAL, JINV_LOCAL, DN_GLOBAL(1:NNODE_LOCAL,1:3), IERR_LOCAL)

            IF (IERR_LOCAL /= 0) THEN
              IQ1NP_DBG = Q1NP_IDS(IEL_LOCAL)
              IEL_HEX8_DBG = KQ1NP_TAB(10,IQ1NP_DBG)
              WRITE(*,'(A,I10,A,I4,A,I4,A,I4,A,I4,A,1PE12.4)')                &
     &          ' Q1NP Bad Jacobian: elem_uid=', NGL_ELEM(IEL_LOCAL),         &
     &          ' lane=', IEL_LOCAL, ' nnode=', NNODE_LOCAL,                  &
     &          ' p=', P_LOCAL, ' q=', Q_LOCAL, ' detJ=', DETJ_LOCAL
              WRITE(*,'(A,3(1PE12.4))') '   xi,eta,zeta=', XI, ETA, ZETA
              WRITE(*,'(A,I4,A,I4,A,I4)') '   elem_u=', ELEM_U(IEL_LOCAL),    &
     &          ' elem_v=', ELEM_V(IEL_LOCAL),                                &
     &          ' nctrl=', NCTRL_ELEM(IEL_LOCAL)
              WRITE(*,'(A,8(1X,1PE10.3))') '   U_KNOT=',                      &
     &          U_KNOT(1:MIN(U_LEN_EL(IEL_LOCAL),8),IEL_LOCAL)
              WRITE(*,'(A,8(1X,1PE10.3))') '   V_KNOT=',                      &
     &          V_KNOT(1:MIN(V_LEN_EL(IEL_LOCAL),8),IEL_LOCAL)
              IF (IEL_HEX8_DBG > 0 .AND. IEL_HEX8_DBG <= NUMELS) THEN
                WRITE(*,'(A,I10,A,8I8)')                                      &
     &            '   HEX8 (iel=', IEL_HEX8_DBG,                              &
     &            ') IXS(2:9)=', (IXS(K_DBG,IEL_HEX8_DBG), K_DBG=2,9)
                DO K_DBG = 2, 9
                  IF (IXS(K_DBG,IEL_HEX8_DBG) > 0 .AND.                       &
     &                IXS(K_DBG,IEL_HEX8_DBG) <= NUMNOD) THEN
                    WRITE(*,'(A,I1,A,I8,A,3(1X,1PE12.4))')                    &
     &                '   IXS(', K_DBG, ')=', IXS(K_DBG,IEL_HEX8_DBG),        &
     &                ' X=', X(1,IXS(K_DBG,IEL_HEX8_DBG)),                    &
     &                      X(2,IXS(K_DBG,IEL_HEX8_DBG)),                     &
     &                      X(3,IXS(K_DBG,IEL_HEX8_DBG))
                  END IF
                END DO
                WRITE(*,'(A,I8,A,I8)')                                        &
     &            '   KQ1NP_TAB(5)=', KQ1NP_TAB(5,IQ1NP_DBG),                 &
     &            ' KQ1NP_TAB(10)=', KQ1NP_TAB(10,IQ1NP_DBG)
              END IF
              DO K_LOCAL = 1, NNODE_LOCAL
                WRITE(*,'(A,I4,A,3(1X,1PE12.4),A,I10)')                       &
     &            '   X_ELEM(', K_LOCAL, ')=',                                &
     &            X_ELEM(1,K_LOCAL,IEL_LOCAL),                                &
     &            X_ELEM(2,K_LOCAL,IEL_LOCAL),                                &
     &            X_ELEM(3,K_LOCAL,IEL_LOCAL),                                &
     &            ' nid=', NODE_LID(K_LOCAL,IEL_LOCAL)
              END DO
              IERR_OUT = IERR_LOCAL
              RETURN
            END IF

            VOLN(IEL_LOCAL) = GPW * DETJ_LOCAL ! Gauss point volume
            VOLDP(IEL_LOCAL) = DBLE(VOLN(IEL_LOCAL)) ! Double precision Gauss point volume
            VGAUSS(IPT_Q1NP,IEL_LOCAL) = VOLN(IEL_LOCAL) ! Gauss point volume
            VOLG(IEL_LOCAL) = VOLG(IEL_LOCAL) + VOLN(IEL_LOCAL) ! Total Gauss point volume

            IF (IDTMIN(101) == 1 .AND. Q1NP_IS_ACTIVE(IEL_LOCAL)) THEN
              DO K_LOCAL = 1, NNODE_LOCAL
                MASS_ELEM(K_LOCAL,IEL_LOCAL) = MASS_ELEM(K_LOCAL,IEL_LOCAL) + &
     &                                        PM(89,MAT_ID_ELEM(IEL_LOCAL)) * NVAL(K_LOCAL) * VOLN(IEL_LOCAL)
              END DO
            END IF

            CALL Q1NP_FILL_MATB(IEL_LOCAL, NNODE_LOCAL, DN_GLOBAL(1:NNODE_LOCAL,1:3))
          END DO

          CALL Q1NP_EVAL_DEF()
        END SUBROUTINE Q1NP_GP_GEOM

!=======================================================================
! Material law + strain/stress evaluation at the Gauss point
!=======================================================================
        SUBROUTINE Q1NP_GP_MAT(IU, IV, IT)
          INTEGER, INTENT(IN) :: IU, IV, IT

          LBUF => ELBUF_STR%BUFLY(1)%LBUF(IU,IV,IT)

          ! rotates/updates stress with spin terms (WXX/WYY/WZZ)
          CALL SROTA3(LBUF%SIG, S1, S2, S3, S4, S5, S6, WXX, WYY, WZZ, &
     &                  NEL, MTN, IPARG(9,NG))

          ! volumetric strain increment
          DIVDE(1:NEL) = DT1 * (DXX(1:NEL) + DYY(1:NEL) + DZZ(1:NEL))

          ! call the density update
          CALL SRHO3(PM, LBUF%VOL, LBUF%RHO, LBUF%EINT, DIVDE, DUMMY_FLUX, FV, &
     &                 VOLN, DVOL, NGL_ELEM, MAT_ID_ELEM, OFF, IPARG(64,NG), &
     &                 GBUF%TAG22, VOLDP, LBUF%VOL0DP, AMU, GBUF%OFF, NEL, &
     &                 MTN, JALE, ISMSTR, JEUL, JLAG, 1, 1, 0)


          ! main material-law driver
          CALL MMAIN(TIMERS, OUTPUT, ELBUF_TAB_LOCAL, 1, PM, GEO, ALE_CONNECT, IXS, IPARG_LOCAL, &
     &                 V, TF, NPF, BUFMAT, STI, X, DT2T, NELTST, ITYPTST, OFFSET, NEL, W, &
     &                 OFF, PID_ELEM, MAT_ID_ELEM, NGL_ELEM, VOLN, VD2, DVOL, DELTAX, VIS, &
     &                 QVIS, CXX, S1, S2, S3, S4, S5, S6, DXX, DYY, DZZ, D4, D5, D6, WXX, &
     &                 WYY, WZZ, AJ1, AJ2, AJ3, AJ4, AJ5, AJ6, VDX, VDY, VDZ, MUVOID, SSP_EQ, &
     &                 AIRE, SIGY, ET, BUFVOIS, LBUF%PLA, R3_DAM, AMU, MFXX, MFXY, MFXZ, MFYX, &
     &                 MFYY, MFYZ, MFZX, MFZY, MFZZ, IPM, GAMA, FR_WAV, DXY, DYX, DYZ, DZY, &
     &                 DZX, DXZ, ISTRAIN, TEMPEL, DIE, IEXPAN, ILAY, MSSA, DMELS, IU, IV, IT, &
     &                 TABLE, FVD2, FDELTAX, FSSP, FQVIS, IPARG_LOCAL(:,1), IGEO, CONDE, ITASK, &
     &                 NLOC_DMG, VARNL, MAT_ELEM, H3D_STRAIN, JPLASOL, JSPH, MVSIZ, &
     &                 SNPC, STF, SBUFMAT, GLOB_THERM, SVIS, SZ_IX, IRESP, 0, TH_STRAIN, &
     &                 1, TT, DT1, NTABLE, NUMELQ, NUMMAT, NUMGEO, NUMNOD, NUMELS, &
     &                 IDEL7NOK, IDTMIN, MAXFUNC, IMON_MAT, USERL_AVAIL, IMPL_S, IDYNA, DT, &
     &                 FHEAT, SENSORS)

          ! strain history update routine. Used to store the small-strain tensor
          ! Update LBUF%STRA (stored strain components) from current deformation-rate terms
          IF (ISTRAIN == 1) THEN
            CALL SSTRA3(DXX, DYY, DZZ, D4, D5, D6, LBUF%STRA, WXX, WYY, WZZ, OFF, NEL, JCVT)
          END IF
        END SUBROUTINE Q1NP_GP_MAT

!=======================================================================
! Fill the element-local material basis matrix
!=======================================================================
        SUBROUTINE Q1NP_FILL_MATB(IEL_LOCAL, NNODE_LOCAL, DRDX_LOCAL) 
          INTEGER, INTENT(IN) :: IEL_LOCAL, NNODE_LOCAL
          my_real, INTENT(IN) :: DRDX_LOCAL(NNODE_LOCAL,3)
          INTEGER :: K_LOCAL, IAD_LOCAL

          DO K_LOCAL = 1, NNODE_LOCAL
            IAD_LOCAL = (K_LOCAL - 1) * 3
            MATB_GP(IAD_LOCAL + 1,IEL_LOCAL) = DRDX_LOCAL(K_LOCAL,1)
            MATB_GP(IAD_LOCAL + 2,IEL_LOCAL) = DRDX_LOCAL(K_LOCAL,2)
            MATB_GP(IAD_LOCAL + 3,IEL_LOCAL) = DRDX_LOCAL(K_LOCAL,3)
          END DO
        END SUBROUTINE Q1NP_FILL_MATB

!=======================================================================  
! Evaluate deformation-rate/kinematic terms used for stress update
! computes deformation-rates (DXX, DYY, DZZ, DXY, DYX, DYZ, DZY, DZX, DXZ)
! applies time-step corrections (DT1D2_LOCAL)
!=======================================================================
        SUBROUTINE Q1NP_EVAL_DEF()
          INTEGER :: IEL_LOCAL, K_LOCAL, IAD_LOCAL
          my_real :: DT1D2_LOCAL, AAA_LOCAL

          DO IEL_LOCAL = 1, NEL
            IF (.NOT. Q1NP_IS_ACTIVE(IEL_LOCAL)) CYCLE

            DXX(IEL_LOCAL) = ZERO
            DYY(IEL_LOCAL) = ZERO
            DZZ(IEL_LOCAL) = ZERO
            DXY(IEL_LOCAL) = ZERO
            DYX(IEL_LOCAL) = ZERO
            DYZ(IEL_LOCAL) = ZERO
            DZY(IEL_LOCAL) = ZERO
            DZX(IEL_LOCAL) = ZERO
            DXZ(IEL_LOCAL) = ZERO
            WXX(IEL_LOCAL) = ZERO
            WYY(IEL_LOCAL) = ZERO
            WZZ(IEL_LOCAL) = ZERO
            D4(IEL_LOCAL) = ZERO
            D5(IEL_LOCAL) = ZERO
            D6(IEL_LOCAL) = ZERO

            DO K_LOCAL = 1, NCTRL_ELEM(IEL_LOCAL) + 4
              IAD_LOCAL = (K_LOCAL - 1) * 3
              DXX(IEL_LOCAL) = DXX(IEL_LOCAL) + MATB_GP(IAD_LOCAL + 1,IEL_LOCAL) * V_ELEM(1,K_LOCAL,IEL_LOCAL)
              DYY(IEL_LOCAL) = DYY(IEL_LOCAL) + MATB_GP(IAD_LOCAL + 2,IEL_LOCAL) * V_ELEM(2,K_LOCAL,IEL_LOCAL)
              DZZ(IEL_LOCAL) = DZZ(IEL_LOCAL) + MATB_GP(IAD_LOCAL + 3,IEL_LOCAL) * V_ELEM(3,K_LOCAL,IEL_LOCAL)
              DXY(IEL_LOCAL) = DXY(IEL_LOCAL) + MATB_GP(IAD_LOCAL + 2,IEL_LOCAL) * V_ELEM(1,K_LOCAL,IEL_LOCAL)
              DYX(IEL_LOCAL) = DYX(IEL_LOCAL) + MATB_GP(IAD_LOCAL + 1,IEL_LOCAL) * V_ELEM(2,K_LOCAL,IEL_LOCAL)
              DYZ(IEL_LOCAL) = DYZ(IEL_LOCAL) + MATB_GP(IAD_LOCAL + 3,IEL_LOCAL) * V_ELEM(2,K_LOCAL,IEL_LOCAL)
              DZY(IEL_LOCAL) = DZY(IEL_LOCAL) + MATB_GP(IAD_LOCAL + 2,IEL_LOCAL) * V_ELEM(3,K_LOCAL,IEL_LOCAL)
              DZX(IEL_LOCAL) = DZX(IEL_LOCAL) + MATB_GP(IAD_LOCAL + 1,IEL_LOCAL) * V_ELEM(3,K_LOCAL,IEL_LOCAL)
              DXZ(IEL_LOCAL) = DXZ(IEL_LOCAL) + MATB_GP(IAD_LOCAL + 3,IEL_LOCAL) * V_ELEM(1,K_LOCAL,IEL_LOCAL)
            END DO
          END DO

          DT1D2_LOCAL = HALF * DT1
          DO IEL_LOCAL = 1, NEL
            IF (.NOT. Q1NP_IS_ACTIVE(IEL_LOCAL)) CYCLE

            DXX(IEL_LOCAL) = DXX(IEL_LOCAL) - DT1D2_LOCAL * &
     &                       (DXX(IEL_LOCAL)*DXX(IEL_LOCAL) + DYX(IEL_LOCAL)*DYX(IEL_LOCAL) + DZX(IEL_LOCAL)*DZX(IEL_LOCAL))
            DYY(IEL_LOCAL) = DYY(IEL_LOCAL) - DT1D2_LOCAL * &
     &                       (DYY(IEL_LOCAL)*DYY(IEL_LOCAL) + DZY(IEL_LOCAL)*DZY(IEL_LOCAL) + DXY(IEL_LOCAL)*DXY(IEL_LOCAL))
            DZZ(IEL_LOCAL) = DZZ(IEL_LOCAL) - DT1D2_LOCAL * &
     &                       (DZZ(IEL_LOCAL)*DZZ(IEL_LOCAL) + DXZ(IEL_LOCAL)*DXZ(IEL_LOCAL) + DYZ(IEL_LOCAL)*DYZ(IEL_LOCAL))

            AAA_LOCAL = DT1D2_LOCAL * &
     &                  (DXX(IEL_LOCAL)*DXY(IEL_LOCAL) + DYX(IEL_LOCAL)*DYY(IEL_LOCAL) + DZX(IEL_LOCAL)*DZY(IEL_LOCAL))
            DXY(IEL_LOCAL) = DXY(IEL_LOCAL) - AAA_LOCAL
            DYX(IEL_LOCAL) = DYX(IEL_LOCAL) - AAA_LOCAL
            D4(IEL_LOCAL) = DXY(IEL_LOCAL) + DYX(IEL_LOCAL)

            AAA_LOCAL = DT1D2_LOCAL * &
     &                  (DYY(IEL_LOCAL)*DYZ(IEL_LOCAL) + DZY(IEL_LOCAL)*DZZ(IEL_LOCAL) + DXY(IEL_LOCAL)*DXZ(IEL_LOCAL))
            DYZ(IEL_LOCAL) = DYZ(IEL_LOCAL) - AAA_LOCAL
            DZY(IEL_LOCAL) = DZY(IEL_LOCAL) - AAA_LOCAL
            D5(IEL_LOCAL) = DYZ(IEL_LOCAL) + DZY(IEL_LOCAL)

            AAA_LOCAL = DT1D2_LOCAL * &
     &                  (DZZ(IEL_LOCAL)*DZX(IEL_LOCAL) + DXZ(IEL_LOCAL)*DXX(IEL_LOCAL) + DYZ(IEL_LOCAL)*DYX(IEL_LOCAL))
            DXZ(IEL_LOCAL) = DXZ(IEL_LOCAL) - AAA_LOCAL
            DZX(IEL_LOCAL) = DZX(IEL_LOCAL) - AAA_LOCAL
            D6(IEL_LOCAL) = DXZ(IEL_LOCAL) + DZX(IEL_LOCAL)

            WXX(IEL_LOCAL) = DT1D2_LOCAL * (DZY(IEL_LOCAL) - DYZ(IEL_LOCAL))
            WYY(IEL_LOCAL) = DT1D2_LOCAL * (DXZ(IEL_LOCAL) - DZX(IEL_LOCAL))
            WZZ(IEL_LOCAL) = DT1D2_LOCAL * (DYX(IEL_LOCAL) - DXY(IEL_LOCAL))
          END DO
        END SUBROUTINE Q1NP_EVAL_DEF

!=======================================================================
! Integrate nodal internal forces from stresses into element-local buffers
!=======================================================================
        SUBROUTINE Q1NP_ACCUM_FINT(IEL_LOCAL, SIG1_IN, SIG2_IN, SIG3_IN, SIG4_IN, SIG5_IN, SIG6_IN, &
     &                                 FX_SUM_OUT, FY_SUM_OUT, FZ_SUM_OUT)
          INTEGER, INTENT(IN) :: IEL_LOCAL
          my_real, INTENT(IN) :: SIG1_IN, SIG2_IN, SIG3_IN, SIG4_IN, SIG5_IN, SIG6_IN
          my_real, INTENT(OUT) :: FX_SUM_OUT, FY_SUM_OUT, FZ_SUM_OUT
          INTEGER :: K_LOCAL, IAD_LOCAL
          my_real :: SUMX_LOCAL, SUMY_LOCAL, SUMZ_LOCAL
          my_real :: FX_LOCAL, FY_LOCAL, FZ_LOCAL
          my_real :: STIN_LOCAL, AA_LOCAL
          my_real :: FCOMP_LOCAL(3,6)

          FX_SUM_OUT = ZERO
          FY_SUM_OUT = ZERO
          FZ_SUM_OUT = ZERO
          SUMX_LOCAL = ZERO
          SUMY_LOCAL = ZERO
          SUMZ_LOCAL = ZERO

          DO K_LOCAL = 1, NCTRL_ELEM(IEL_LOCAL) + 4
            IAD_LOCAL = (K_LOCAL - 1) * 3
            SUMX_LOCAL = SUMX_LOCAL + ABS(MATB_GP(IAD_LOCAL + 1,IEL_LOCAL))
            SUMY_LOCAL = SUMY_LOCAL + ABS(MATB_GP(IAD_LOCAL + 2,IEL_LOCAL))
            SUMZ_LOCAL = SUMZ_LOCAL + ABS(MATB_GP(IAD_LOCAL + 3,IEL_LOCAL))
          END DO

          AA_LOCAL = PM(89,MAT_ID_ELEM(IEL_LOCAL)) * SSP_EQ(IEL_LOCAL) * SSP_EQ(IEL_LOCAL)

          DO K_LOCAL = 1, NCTRL_ELEM(IEL_LOCAL) + 4
            IAD_LOCAL = (K_LOCAL - 1) * 3
            FCOMP_LOCAL = ZERO
            FCOMP_LOCAL(1,1) = -VOLN(IEL_LOCAL) * MATB_GP(IAD_LOCAL + 1,IEL_LOCAL) * SIG1_IN
            FCOMP_LOCAL(2,2) = -VOLN(IEL_LOCAL) * MATB_GP(IAD_LOCAL + 2,IEL_LOCAL) * SIG2_IN
            FCOMP_LOCAL(3,3) = -VOLN(IEL_LOCAL) * MATB_GP(IAD_LOCAL + 3,IEL_LOCAL) * SIG3_IN
            FCOMP_LOCAL(1,4) = -VOLN(IEL_LOCAL) * MATB_GP(IAD_LOCAL + 2,IEL_LOCAL) * SIG4_IN
            FCOMP_LOCAL(2,4) = -VOLN(IEL_LOCAL) * MATB_GP(IAD_LOCAL + 1,IEL_LOCAL) * SIG4_IN
            FCOMP_LOCAL(2,5) = -VOLN(IEL_LOCAL) * MATB_GP(IAD_LOCAL + 3,IEL_LOCAL) * SIG5_IN
            FCOMP_LOCAL(3,5) = -VOLN(IEL_LOCAL) * MATB_GP(IAD_LOCAL + 2,IEL_LOCAL) * SIG5_IN
            FCOMP_LOCAL(1,6) = -VOLN(IEL_LOCAL) * MATB_GP(IAD_LOCAL + 3,IEL_LOCAL) * SIG6_IN
            FCOMP_LOCAL(3,6) = -VOLN(IEL_LOCAL) * MATB_GP(IAD_LOCAL + 1,IEL_LOCAL) * SIG6_IN

            FX_LOCAL = SUM(FCOMP_LOCAL(1,1:6))
            FY_LOCAL = SUM(FCOMP_LOCAL(2,1:6))
            FZ_LOCAL = SUM(FCOMP_LOCAL(3,1:6))

            F_INT_ELEM(1,K_LOCAL,IEL_LOCAL) = F_INT_ELEM(1,K_LOCAL,IEL_LOCAL) + FX_LOCAL
            F_INT_ELEM(2,K_LOCAL,IEL_LOCAL) = F_INT_ELEM(2,K_LOCAL,IEL_LOCAL) + FY_LOCAL
            F_INT_ELEM(3,K_LOCAL,IEL_LOCAL) = F_INT_ELEM(3,K_LOCAL,IEL_LOCAL) + FZ_LOCAL
            FX_SUM_OUT = FX_SUM_OUT + FX_LOCAL
            FY_SUM_OUT = FY_SUM_OUT + FY_LOCAL
            FZ_SUM_OUT = FZ_SUM_OUT + FZ_LOCAL

            STIN_LOCAL = HALF * VOLN(IEL_LOCAL) * &
     &                   (ABS(MATB_GP(IAD_LOCAL + 1,IEL_LOCAL)) * SUMX_LOCAL + &
     &                    ABS(MATB_GP(IAD_LOCAL + 2,IEL_LOCAL)) * SUMY_LOCAL + &
     &                    ABS(MATB_GP(IAD_LOCAL + 3,IEL_LOCAL)) * SUMZ_LOCAL)
            STIG_ELEM(K_LOCAL,IEL_LOCAL) = STIG_ELEM(K_LOCAL,IEL_LOCAL) + STIN_LOCAL * AA_LOCAL

          END DO
        END SUBROUTINE Q1NP_ACCUM_FINT

!=======================================================================
! Uses stress (SIG1..SIG6) to compute and accumulate internal nodal forces via Q1NP_ACCUM_FIN
!=======================================================================
        SUBROUTINE Q1NP_ACCUM_NFORCE(IU, IV, IT)
          INTEGER, INTENT(IN) :: IU, IV, IT
          INTEGER :: IEL_LOCAL
          my_real :: SIG1_LOCAL, SIG2_LOCAL, SIG3_LOCAL
          my_real :: SIG4_LOCAL, SIG5_LOCAL, SIG6_LOCAL
          my_real :: FX_SUM, FY_SUM, FZ_SUM

          LBUF => ELBUF_STR%BUFLY(1)%LBUF(IU,IV,IT)

          DO IEL_LOCAL = 1, NEL
            IF (.NOT. Q1NP_IS_ACTIVE(IEL_LOCAL)) CYCLE

            CALL Q1NP_BUILD_SIG(IEL_LOCAL, SIG1_LOCAL, SIG2_LOCAL, SIG3_LOCAL, &
     &                                      SIG4_LOCAL, SIG5_LOCAL, SIG6_LOCAL)

            CALL Q1NP_ACCUM_FINT(IEL_LOCAL, SIG1_LOCAL, SIG2_LOCAL, SIG3_LOCAL, SIG4_LOCAL, SIG5_LOCAL, &
     &                               SIG6_LOCAL, FX_SUM, FY_SUM, FZ_SUM)
          END DO
        END SUBROUTINE Q1NP_ACCUM_NFORCE

!=======================================================================
! Assemble element-local internal forces into the global array A
!=======================================================================
        SUBROUTINE Q1NP_ASSEMBLE_FINT()
          INTEGER :: IEL_LOCAL, K_LOCAL, LOCAL_ID_LOCAL

          DO IEL_LOCAL = 1, NEL
            IF (.NOT. Q1NP_IS_ACTIVE(IEL_LOCAL)) CYCLE

            DO K_LOCAL = 1, NCTRL_ELEM(IEL_LOCAL) + 4
              LOCAL_ID_LOCAL = NODE_LID(K_LOCAL,IEL_LOCAL)
              A(1,LOCAL_ID_LOCAL) = A(1,LOCAL_ID_LOCAL) + F_INT_ELEM(1,K_LOCAL,IEL_LOCAL)
              A(2,LOCAL_ID_LOCAL) = A(2,LOCAL_ID_LOCAL) + F_INT_ELEM(2,K_LOCAL,IEL_LOCAL)
              A(3,LOCAL_ID_LOCAL) = A(3,LOCAL_ID_LOCAL) + F_INT_ELEM(3,K_LOCAL,IEL_LOCAL)
              STIFN(LOCAL_ID_LOCAL) = STIFN(LOCAL_ID_LOCAL) + STIG_ELEM(K_LOCAL,IEL_LOCAL)
            END DO
          END DO
        END SUBROUTINE Q1NP_ASSEMBLE_FINT
       
!=======================================================================
! Gauss stress average + optional bilans + small-strain housekeeping
! TODO: #REVIEW: Q1NP_AVG_SIG_BILAN
!=======================================================================
        SUBROUTINE Q1NP_AVG_SIG_BILAN()
          INTEGER :: IU_LOCAL, IV_LOCAL, IT_LOCAL
          INTEGER :: IEL_LOCAL, K_LOCAL
          INTEGER :: IPT_Q1NP_LOCAL
          LOGICAL :: CAN_BILAN_LOCAL

          IPT_Q1NP_LOCAL = 0
          DO IT_LOCAL = 1, NP_T_L
            DO IV_LOCAL = 1, NP_V_L
              DO IU_LOCAL = 1, NP_U_L
                IPT_Q1NP_LOCAL = IPT_Q1NP_LOCAL + 1
                LBUF => ELBUF_STR%BUFLY(1)%LBUF(IU_LOCAL,IV_LOCAL,IT_LOCAL)
                CALL IG3DAVERAGE(LBUF%SIG, GBUF%SIG, LBUF%VOL, GBUF%VOL, LBUF%RHO, LBUF%EINT, &
     &                             GBUF%EINT, GBUF%RHO, VGAUSS(IPT_Q1NP_LOCAL,:), VOLG, LBUF%PLA, GBUF%PLA, &
     &                             GBUF%G_PLA, LBUF%EPSD, GBUF%EPSD, NEL, IPARG(40,NG))
              END DO
            END DO
          END DO

          CAN_BILAN_LOCAL = .TRUE.
          DO IEL_LOCAL = 2, NEL
            IF (NCTRL_ELEM(IEL_LOCAL) /= NCTRL_ELEM(1)) THEN
              CAN_BILAN_LOCAL = .FALSE.
              EXIT
            END IF
          END DO

          IF (CAN_BILAN_LOCAL .AND. IPRI > 0) THEN
            CALL MY_ALLOC(VX_BAL, NCTRL_ELEM(1) + 4, MVSIZ, "VX_BAL")
            CALL MY_ALLOC(VY_BAL, NCTRL_ELEM(1) + 4, MVSIZ, "VY_BAL")
            CALL MY_ALLOC(VZ_BAL, NCTRL_ELEM(1) + 4, MVSIZ, "VZ_BAL")
            CALL MY_ALLOC(XX_BAL, NCTRL_ELEM(1) + 4, MVSIZ, "XX_BAL")
            CALL MY_ALLOC(YY_BAL, NCTRL_ELEM(1) + 4, MVSIZ, "YY_BAL")
            CALL MY_ALLOC(ZZ_BAL, NCTRL_ELEM(1) + 4, MVSIZ, "ZZ_BAL")

            DO IEL_LOCAL = 1, NEL
              DO K_LOCAL = 1, NCTRL_ELEM(IEL_LOCAL) + 4
                VX_BAL(K_LOCAL,IEL_LOCAL) = V_ELEM(1,K_LOCAL,IEL_LOCAL)
                VY_BAL(K_LOCAL,IEL_LOCAL) = V_ELEM(2,K_LOCAL,IEL_LOCAL)
                VZ_BAL(K_LOCAL,IEL_LOCAL) = V_ELEM(3,K_LOCAL,IEL_LOCAL)
                XX_BAL(K_LOCAL,IEL_LOCAL) = X_ELEM(1,K_LOCAL,IEL_LOCAL)
                YY_BAL(K_LOCAL,IEL_LOCAL) = X_ELEM(2,K_LOCAL,IEL_LOCAL)
                ZZ_BAL(K_LOCAL,IEL_LOCAL) = X_ELEM(3,K_LOCAL,IEL_LOCAL)
              END DO
            END DO
            ! NOTE: BILAN is currently disabled.
           IF (.FALSE.) THEN
             CALL IGE3DBILAN(PARTSAV, GBUF%EINT, GBUF%RHO, VOLG, VX_BAL, VY_BAL, VZ_BAL, IPARTS, &
    &                      GBUF%VOL, GRESAV, GRTH, IGRTH, XX_BAL, YY_BAL, ZZ_BAL, NCTRL_ELEM(1) + 4, &
    &                      ITASK, IPARG(1,NG), SENSORS)
           END IF

            CALL MY_DEALLOC(VX_BAL)
            CALL MY_DEALLOC(VY_BAL)
            CALL MY_DEALLOC(VZ_BAL)
            CALL MY_DEALLOC(XX_BAL)
            CALL MY_DEALLOC(YY_BAL)
            CALL MY_DEALLOC(ZZ_BAL)
          END IF
        END SUBROUTINE Q1NP_AVG_SIG_BILAN

!=======================================================================
! Build the element-local stress tensor
!=======================================================================
        SUBROUTINE Q1NP_BUILD_SIG(IEL_LOCAL, SIG1_OUT, SIG2_OUT, SIG3_OUT, SIG4_OUT, SIG5_OUT, SIG6_OUT)
          INTEGER, INTENT(IN) :: IEL_LOCAL
          my_real, INTENT(OUT) :: SIG1_OUT, SIG2_OUT, SIG3_OUT, SIG4_OUT, SIG5_OUT, SIG6_OUT
          SIG1_OUT = LBUF%SIG(II(1)+IEL_LOCAL) + SVIS(IEL_LOCAL,1) - QVIS(IEL_LOCAL)
          SIG2_OUT = LBUF%SIG(II(2)+IEL_LOCAL) + SVIS(IEL_LOCAL,2) - QVIS(IEL_LOCAL)
          SIG3_OUT = LBUF%SIG(II(3)+IEL_LOCAL) + SVIS(IEL_LOCAL,3) - QVIS(IEL_LOCAL)
          SIG4_OUT = LBUF%SIG(II(4)+IEL_LOCAL) + SVIS(IEL_LOCAL,4)
          SIG5_OUT = LBUF%SIG(II(5)+IEL_LOCAL) + SVIS(IEL_LOCAL,5)
          SIG6_OUT = LBUF%SIG(II(6)+IEL_LOCAL) + SVIS(IEL_LOCAL,6)
        END SUBROUTINE Q1NP_BUILD_SIG

!=======================================================================
! Check if the element is active
!=======================================================================
        LOGICAL FUNCTION Q1NP_IS_ACTIVE(IEL_LOCAL)
          INTEGER, INTENT(IN) :: IEL_LOCAL
          Q1NP_IS_ACTIVE = .TRUE.
          IF (GBUF%OFF(IEL_LOCAL) <= ZERO) Q1NP_IS_ACTIVE = .FALSE.
          IF (OFF(IEL_LOCAL) <= ZERO) Q1NP_IS_ACTIVE = .FALSE.
        END FUNCTION Q1NP_IS_ACTIVE

!=======================================================================
! Calculate the characteristic length of the element
! Used in MMAIN_Q1NP to update the element time-step
!=======================================================================
        SUBROUTINE Q1NP_CHAR_LEN(IEL_LOCAL, DELTAX_OUT)
          INTEGER, INTENT(IN) :: IEL_LOCAL
          my_real, INTENT(OUT) :: DELTAX_OUT
          my_real, PARAMETER :: SPAN_SCALE = 0.2
          my_real :: TOP_1_XYZ(3), TOP_2_XYZ(3), TOP_3_XYZ(3), TOP_4_XYZ(3)
          my_real :: BOT_1_XYZ(3), BOT_2_XYZ(3), BOT_3_XYZ(3), BOT_4_XYZ(3)
          my_real :: LU_TOP_1, LU_TOP_2, LV_TOP_1, LV_TOP_2
          my_real :: LU_BOT_1, LU_BOT_2, LV_BOT_1, LV_BOT_2
          my_real :: LT_1, LT_2, LT_3, LT_4
          my_real :: SPAN_U, SPAN_V, SPAN_T

          CALL Q1NP_EVAL_PHYS_POINT(IEL_LOCAL, -ONE, -ONE,  ONE, TOP_1_XYZ)
          CALL Q1NP_EVAL_PHYS_POINT(IEL_LOCAL,  ONE, -ONE,  ONE, TOP_2_XYZ)
          CALL Q1NP_EVAL_PHYS_POINT(IEL_LOCAL,  ONE,  ONE,  ONE, TOP_3_XYZ)
          CALL Q1NP_EVAL_PHYS_POINT(IEL_LOCAL, -ONE,  ONE,  ONE, TOP_4_XYZ)
          CALL Q1NP_EVAL_PHYS_POINT(IEL_LOCAL, -ONE, -ONE, -ONE, BOT_1_XYZ)
          CALL Q1NP_EVAL_PHYS_POINT(IEL_LOCAL,  ONE, -ONE, -ONE, BOT_2_XYZ)
          CALL Q1NP_EVAL_PHYS_POINT(IEL_LOCAL,  ONE,  ONE, -ONE, BOT_3_XYZ)
          CALL Q1NP_EVAL_PHYS_POINT(IEL_LOCAL, -ONE,  ONE, -ONE, BOT_4_XYZ)

          LU_TOP_1 = Q1NP_POINT_DIST(TOP_1_XYZ, TOP_2_XYZ)
          LU_TOP_2 = Q1NP_POINT_DIST(TOP_4_XYZ, TOP_3_XYZ)
          LV_TOP_1 = Q1NP_POINT_DIST(TOP_1_XYZ, TOP_4_XYZ)
          LV_TOP_2 = Q1NP_POINT_DIST(TOP_2_XYZ, TOP_3_XYZ)

          LU_BOT_1 = Q1NP_POINT_DIST(BOT_1_XYZ, BOT_2_XYZ)
          LU_BOT_2 = Q1NP_POINT_DIST(BOT_4_XYZ, BOT_3_XYZ)
          LV_BOT_1 = Q1NP_POINT_DIST(BOT_1_XYZ, BOT_4_XYZ)
          LV_BOT_2 = Q1NP_POINT_DIST(BOT_2_XYZ, BOT_3_XYZ)

          LT_1 = Q1NP_POINT_DIST(TOP_1_XYZ, BOT_1_XYZ)
          LT_2 = Q1NP_POINT_DIST(TOP_2_XYZ, BOT_2_XYZ)
          LT_3 = Q1NP_POINT_DIST(TOP_3_XYZ, BOT_3_XYZ)
          LT_4 = Q1NP_POINT_DIST(TOP_4_XYZ, BOT_4_XYZ)

          SPAN_U = SPAN_SCALE * (LU_TOP_1 + LU_TOP_2 + LU_BOT_1 + LU_BOT_2)
          SPAN_V = SPAN_SCALE * (LV_TOP_1 + LV_TOP_2 + LV_BOT_1 + LV_BOT_2)
          SPAN_T = SPAN_SCALE * (LT_1 + LT_2 + LT_3 + LT_4)

          DELTAX_OUT = MIN(SPAN_U, MIN(SPAN_V, SPAN_T))

        END SUBROUTINE Q1NP_CHAR_LEN

!=======================================================================
! Evaluate the current physical point of the Q1NP element at parent coordinates
!=======================================================================
        SUBROUTINE Q1NP_EVAL_PHYS_POINT(IEL_LOCAL, XI_LOCAL, ETA_LOCAL, ZETA_LOCAL, XYZ_OUT)
          INTEGER, INTENT(IN) :: IEL_LOCAL
          my_real, INTENT(IN) :: XI_LOCAL, ETA_LOCAL, ZETA_LOCAL
          my_real, INTENT(OUT) :: XYZ_OUT(3)
          INTEGER :: IQ1NP_LOCAL, P_LOCAL, Q_LOCAL
          INTEGER :: NNODE_LOCAL, K_LOCAL
          my_real :: NVAL_LOCAL(MAX_NNODE)
          my_real :: DN_TMP(MAX_NNODE,3)

          IQ1NP_LOCAL = Q1NP_IDS(IEL_LOCAL)
          P_LOCAL = KQ1NP_TAB(8,IQ1NP_LOCAL)
          Q_LOCAL = KQ1NP_TAB(9,IQ1NP_LOCAL)
          NNODE_LOCAL = NCTRL_ELEM(IEL_LOCAL) + 4

          CALL Q1NP_SHAPE_FUNCTIONS(XI_LOCAL, ETA_LOCAL, ZETA_LOCAL, P_LOCAL, Q_LOCAL, &
     &                              U_KNOT(1:U_LEN_EL(IEL_LOCAL),IEL_LOCAL), &
     &                              V_KNOT(1:V_LEN_EL(IEL_LOCAL),IEL_LOCAL), &
     &                              ELEM_U(IEL_LOCAL), ELEM_V(IEL_LOCAL), &
     &                              NVAL_LOCAL(1:NNODE_LOCAL), DN_TMP(1:NNODE_LOCAL,1:3))

          XYZ_OUT = ZERO
          DO K_LOCAL = 1, NNODE_LOCAL
            XYZ_OUT(1:3) = XYZ_OUT(1:3) + NVAL_LOCAL(K_LOCAL) * X_ELEM(1:3,K_LOCAL,IEL_LOCAL)
          END DO
        END SUBROUTINE Q1NP_EVAL_PHYS_POINT

!=======================================================================
! Calculate the distance between two physical points
!=======================================================================
        my_real FUNCTION Q1NP_POINT_DIST(POINT_A, POINT_B)
          my_real, INTENT(IN) :: POINT_A(3), POINT_B(3)
          my_real :: DX_LOCAL, DY_LOCAL, DZ_LOCAL

          DX_LOCAL = POINT_B(1) - POINT_A(1)
          DY_LOCAL = POINT_B(2) - POINT_A(2)
          DZ_LOCAL = POINT_B(3) - POINT_A(3)

          Q1NP_POINT_DIST = SQRT(DX_LOCAL*DX_LOCAL + DY_LOCAL*DY_LOCAL + DZ_LOCAL*DZ_LOCAL)
        END FUNCTION Q1NP_POINT_DIST

        ! ----------------------------------------------------------------------
        ! Find the group node ID for a given node ID
        ! ----------------------------------------------------------------------
        INTEGER FUNCTION FIND_GROUP_NODE(GID, GIDS, NGID)
          INTEGER, INTENT(IN) :: GID, NGID
          INTEGER, INTENT(IN) :: GIDS(:)
          INTEGER :: ITMP
          FIND_GROUP_NODE = 0
          DO ITMP = 1, NGID
            IF (GIDS(ITMP) == GID) THEN
              FIND_GROUP_NODE = ITMP
              RETURN
            END IF
          END DO
        END FUNCTION FIND_GROUP_NODE

!=======================================================================
! Rebuild the grid for a given number of control points
!=======================================================================
        SUBROUTINE Q1NP_REBUILD_GRID(NX_OUT, NY_OUT, P_IN, Q_IN)
          INTEGER, INTENT(OUT) :: NX_OUT, NY_OUT
          INTEGER, INTENT(IN)  :: P_IN, Q_IN
          NX_OUT = 0
          NY_OUT = 0
          IF (SQ1NPCTRL_SHARED_G <= 0 .OR. SQ1NPKNOT_L_G <= 0) RETURN
          DO NX_CAND = 1, SQ1NPCTRL_SHARED_G
            IF (MOD(SQ1NPCTRL_SHARED_G, NX_CAND) /= 0) CYCLE
            NY_CAND = SQ1NPCTRL_SHARED_G / NX_CAND
            NX_FOUND = NX_CAND - P_IN
            NY_FOUND = NY_CAND - Q_IN
            IF (NX_FOUND <= 0 .OR. NY_FOUND <= 0) CYCLE
            NKNOT_U = NX_FOUND + 2*P_IN + 1
            NKNOT_V = NY_FOUND + 2*Q_IN + 1
            IF (NKNOT_U + NKNOT_V == SQ1NPKNOT_L_G) THEN
              NX_OUT = NX_FOUND
              NY_OUT = NY_FOUND
              RETURN
            END IF
          END DO
        END SUBROUTINE Q1NP_REBUILD_GRID

      END SUBROUTINE Q1NP_FORC3
