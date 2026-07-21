!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!
!   Q1NP NURBS contact: broad phase, narrow phase, and penalty forces.
!
!   Builds point clouds on the NURBS top surfaces of two Q1NP element
!   sets (identified by knot_set_id = 1 and 2), performs voxel-based
!   proximity detection, Newton projection to find penetrating pairs,
!   and scatters normal penalty forces onto NURBS control-point nodes.
!
!||====================================================================
!||    q1np_contact_algorithms_mod         ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_driver_mod             ../engine/source/interfaces/ists_q1np/q1np_contact_driver.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                        ../common_source/modules/constant_mod.F
!||    my_alloc_mod                        ../common_source/tools/memory/my_alloc.F90
!||    my_dealloc_mod                      ../common_source/tools/memory/my_dealloc.F90
!||    precision_mod                       ../common_source/modules/precision_mod.F90
!||    q1np_contact_export_mod             ../engine/source/interfaces/ists_q1np/q1np_contact_export.F90
!||    q1np_nurbs_surface_evaluation_mod   ../engine/source/elements/solid/solid_q1np/q1np_nurbs_surface_eval_mod.F90
!||    q1np_restart_mod                    ../common_source/modules/q1np_restart_mod.F90
!||====================================================================
      MODULE Q1NP_CONTACT_ALGORITHMS_MOD
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        USE PRECISION_MOD, ONLY : WP
        USE CONSTANT_MOD , ONLY : ZERO, ONE, TWO, THREE, TEN, HALF
        USE MY_ALLOC_MOD, ONLY : MY_ALLOC
        USE MY_DEALLOC_MOD, ONLY : MY_DEALLOC
        USE Q1NP_RESTART_MOD
        USE Q1NP_CONTACT_EXPORT_MOD, ONLY : Q1NP_CONTACT_EXPORT_ACCUMULATE
        USE Q1NP_NURBS_SURFACE_EVALUATION_MOD, ONLY : &
     &      Q1NP_EVALUATE_NURBS_TOP_SURFACE_POINT, &
     &      Q1NP_EVALUATE_NURBS_TOP_SURFACE_POINT_AND_DERIVS, &
     &      Q1NP_EVALUATE_NURBS_SHAPE_VALUES
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
        IMPLICIT NONE
        PRIVATE
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Tuning parameters
! ----------------------------------------------------------------------------------------------------------------------
!       Broad-phase trigger tolerance (voxel-search).
!       Runtime trigger tolerance is derived from the interface GAP value.
!       Conservative cap for voxel-derived A-candidates stored per B point.
!       If the local neighborhood exceeds this cap, narrow phase falls back
!       to the original global nearest search for that B point.
        INTEGER, PARAMETER :: Q1NP_CONTACT_MAX_CANDIDATES_PER_B = 32

!       Number of sample points per parametric direction on each element surface.
!       These samples are used as interior quadrature-style points, not
!       nodal/end-point samples, to avoid duplicate contact pairs on
!       shared element edges and corners.
        INTEGER, PARAMETER :: Q1NP_CONTACT_BP_NGP_U = 3
        INTEGER, PARAMETER :: Q1NP_CONTACT_BP_NGP_V = 3

!       Gap-dependent penalty tuning for NURBS contact.
!       Lower bound used when interface GAP is zero/too small.
        REAL(KIND=WP), PARAMETER :: Q1NP_CONTACT_GAP_FALLBACK = 1.0E-6_WP
!       Temporary debug switch: if enabled, only control-point based
!       secondary stiffness is used (no bulk/nearest fallback path).
        LOGICAL, PARAMETER :: Q1NP_CONTACT_DISABLE_SECONDARY_FALLBACK = .FALSE.

! ----------------------------------------------------------------------------------------------------------------------
!                                                   KQ1NP_TAB column indices
! ----------------------------------------------------------------------------------------------------------------------
        INTEGER, PARAMETER :: KQ1NP_NCTRL    = 3
        INTEGER, PARAMETER :: KQ1NP_CP_OFF   = 4
        INTEGER, PARAMETER :: KQ1NP_ELEM_U   = 6
        INTEGER, PARAMETER :: KQ1NP_ELEM_V   = 7
        INTEGER, PARAMETER :: KQ1NP_P        = 8
        INTEGER, PARAMETER :: KQ1NP_Q        = 9
        INTEGER, PARAMETER :: KQ1NP_IXS_IDX  = 10
        INTEGER, PARAMETER :: KQ1NP_NX       = 12
        INTEGER, PARAMETER :: KQ1NP_NY       = 13
        INTEGER, PARAMETER :: KQ1NP_BULK_OFF = 14
        INTEGER, PARAMETER :: KQ1NP_KNOT_SET = 15

! ----------------------------------------------------------------------------------------------------------------------
!       FCONT grid-node lookup
!       Original surface FEM node IDs per Q1NP element, built once from IXS.
! ----------------------------------------------------------------------------------------------------------------------
        INTEGER, ALLOCATABLE, SAVE :: Q1NP_FCONT_GRID_IDS(:,:)
        !Check if the FCONT grid-node lookup is ready
        LOGICAL, SAVE :: Q1NP_FCONT_GRID_READY = .FALSE.

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Contact pair type
! ----------------------------------------------------------------------------------------------------------------------
        TYPE Q1NP_CONTACT_PAIR
          REAL(KIND=WP) :: PENETRATION
          REAL(KIND=WP) :: NORMAL(3)
          REAL(KIND=WP) :: XI_PROJ, ETA_PROJ
          INTEGER :: ELEM_A
          REAL(KIND=WP) :: XI_SRC, ETA_SRC
          INTEGER :: ELEM_B
        END TYPE Q1NP_CONTACT_PAIR

! ----------------------------------------------------------------------------------------------------------------------
!                                            Broad-phase workspace
!       Holds point clouds, parametric coordinates, and voxel candidate
!       lists built by the broad phase and consumed by the narrow phase.
! ----------------------------------------------------------------------------------------------------------------------
        TYPE Q1NP_CONTACT_WORKSPACE
          REAL(KIND=WP), ALLOCATABLE :: SURF_POINTS_A(:,:)
          REAL(KIND=WP), ALLOCATABLE :: SURF_POINTS_B(:,:)
          INTEGER, ALLOCATABLE       :: ELEM_IDS_A(:), ELEM_IDS_B(:)
          REAL(KIND=WP), ALLOCATABLE :: XI_A(:), ETA_A(:)
          REAL(KIND=WP), ALLOCATABLE :: XI_B(:), ETA_B(:)
          INTEGER, ALLOCATABLE       :: CANDIDATE_IA(:,:)
          INTEGER, ALLOCATABLE       :: CANDIDATE_COUNT(:)
          LOGICAL, ALLOCATABLE       :: CANDIDATE_OVERFLOW(:)
          INTEGER :: NPTS_A = 0, NPTS_B = 0
        END TYPE Q1NP_CONTACT_WORKSPACE

        PUBLIC :: Q1NP_CONTACT_PAIR
        PUBLIC :: Q1NP_CONTACT_WORKSPACE
        PUBLIC :: Q1NP_CONTACT_BROAD_PHASE
        PUBLIC :: Q1NP_CONTACT_NARROW_PHASE
        PUBLIC :: Q1NP_CONTACT_FORCE_ASSEMBLY
        PUBLIC :: Q1NP_CONTACT_WORKSPACE_FREE
        PUBLIC :: Q1NP_CONTACT_INIT_GRID_NODES

      CONTAINS

!=======================================================================
!   Q1NP_CONTACT_BROAD_PHASE
!
!   Builds both surface point clouds and runs voxel-based proximity
!   detection. Returns the filled workspace (point clouds + candidates)
!   and the minimum distance D_MIN for adaptive-skip scheduling.
!=======================================================================
!||====================================================================
!||    q1np_contact_broad_phase                      ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_driver_int7                      ../engine/source/interfaces/ists_q1np/q1np_contact_driver.F90
!||--- calls      -----------------------------------------------------
!||    q1np_contact_bp_build_surface_points          ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_broad_phase_voxel_min_distance   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||====================================================================
        SUBROUTINE Q1NP_CONTACT_BROAD_PHASE( &
     &      KQ1NP_TAB, IQ1NP_TAB, Q1NP_KTAB, X_COORDS, NUMNOD, &
     &      NUMELQ1NP, GAP, WS, D_MIN)
          INTEGER, INTENT(IN) :: KQ1NP_TAB(:,:)
          INTEGER, INTENT(IN) :: IQ1NP_TAB(:)
          REAL(KIND=WP), INTENT(IN) :: Q1NP_KTAB(:)
          REAL(KIND=WP), INTENT(IN) :: X_COORDS(3,NUMNOD)
          INTEGER, INTENT(IN) :: NUMNOD, NUMELQ1NP
          REAL(KIND=WP), INTENT(IN) :: GAP
          TYPE(Q1NP_CONTACT_WORKSPACE), INTENT(INOUT) :: WS
          REAL(KIND=WP), INTENT(INOUT) :: D_MIN

          INTEGER :: IDX_A, IDX_B, MAX_PTS
          REAL(KIND=WP) :: GAP_CONTACT

          WS%NPTS_A = 0
          WS%NPTS_B = 0
          D_MIN = HUGE(ONE)
          GAP_CONTACT = MAX(Q1NP_CONTACT_GAP_FALLBACK, GAP)

          IF (NUMELQ1NP <= 0) RETURN
          IF (Q1NP_NKNOT_SETS_G < 2) RETURN

! ----------------------------------------------------------------------------------------------------------------------
!                                                BUILD POINT CLOUDS
! ----------------------------------------------------------------------------------------------------------------------
          MAX_PTS = NUMELQ1NP * Q1NP_CONTACT_BP_NGP_U &
     &                        * Q1NP_CONTACT_BP_NGP_V
          CALL MY_ALLOC(WS%SURF_POINTS_A, 3, MAX_PTS, "SURF_POINTS_A")
          CALL MY_ALLOC(WS%SURF_POINTS_B, 3, MAX_PTS, "SURF_POINTS_B")
          CALL MY_ALLOC(WS%ELEM_IDS_A, MAX_PTS, "ELEM_IDS_A")
          CALL MY_ALLOC(WS%XI_A, MAX_PTS, "XI_A")
          CALL MY_ALLOC(WS%ETA_A, MAX_PTS, "ETA_A")
          CALL MY_ALLOC(WS%ELEM_IDS_B, MAX_PTS, "ELEM_IDS_B")
          CALL MY_ALLOC(WS%XI_B, MAX_PTS, "XI_B")
          CALL MY_ALLOC(WS%ETA_B, MAX_PTS, "ETA_B")

          CALL Q1NP_CONTACT_BP_BUILD_SURFACE_POINTS( &
     &        KQ1NP_TAB, IQ1NP_TAB, Q1NP_KTAB, NUMELQ1NP,    &
     &        X_COORDS, NUMNOD, 1,                             &
     &        Q1NP_CONTACT_BP_NGP_U,            &
     &        Q1NP_CONTACT_BP_NGP_V,            &
     &        WS%SURF_POINTS_A, WS%NPTS_A,                    &
     &        WS%ELEM_IDS_A, WS%XI_A, WS%ETA_A, MAX_PTS)

          CALL Q1NP_CONTACT_BP_BUILD_SURFACE_POINTS( &
     &        KQ1NP_TAB, IQ1NP_TAB, Q1NP_KTAB, NUMELQ1NP,    &
     &        X_COORDS, NUMNOD, 2,                             &
     &        Q1NP_CONTACT_BP_NGP_U,            &
     &        Q1NP_CONTACT_BP_NGP_V,            &
     &        WS%SURF_POINTS_B, WS%NPTS_B,                    &
     &        WS%ELEM_IDS_B, WS%XI_B, WS%ETA_B, MAX_PTS)

          CALL MY_ALLOC(WS%CANDIDATE_IA, &
     &      Q1NP_CONTACT_MAX_CANDIDATES_PER_B, MAX(1, WS%NPTS_B), "CANDIDATE_IA")
          CALL MY_ALLOC(WS%CANDIDATE_COUNT, MAX(1, WS%NPTS_B), "CANDIDATE_COUNT")
          CALL MY_ALLOC(WS%CANDIDATE_OVERFLOW, MAX(1, WS%NPTS_B), "CANDIDATE_OVERFLOW")
          WS%CANDIDATE_IA(:,:) = 0
          WS%CANDIDATE_COUNT(:) = 0
          WS%CANDIDATE_OVERFLOW(:) = .FALSE.

! ----------------------------------------------------------------------------------------------------------------------
!                                                VOXEL SEARCH
! ----------------------------------------------------------------------------------------------------------------------
          CALL Q1NP_CONTACT_BROAD_PHASE_VOXEL_MIN_DISTANCE( &
     &        WS%SURF_POINTS_A, WS%NPTS_A, &
     &        WS%SURF_POINTS_B, WS%NPTS_B, &
     &        GAP_CONTACT, D_MIN, IDX_A, IDX_B, &
     &        WS%CANDIDATE_IA, WS%CANDIDATE_COUNT, &
     &        WS%CANDIDATE_OVERFLOW)

        END SUBROUTINE Q1NP_CONTACT_BROAD_PHASE

!=======================================================================
!   Q1NP_CONTACT_NARROW_PHASE
!
!   NURBS-to-NURBS projection on the point clouds from the broad phase.
!   Returns penetrating contact pairs.
!=======================================================================
!||====================================================================
!||    q1np_contact_narrow_phase           ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_driver_int7            ../engine/source/interfaces/ists_q1np/q1np_contact_driver.F90
!||--- calls      -----------------------------------------------------
!||    q1np_contact_narrow_phase_project   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||====================================================================
        SUBROUTINE Q1NP_CONTACT_NARROW_PHASE( &
     &      WS, KQ1NP_TAB, IQ1NP_TAB, Q1NP_KTAB, X_COORDS, &
     &      NUMNOD, GAP, PAIRS, N_PAIRS)
          TYPE(Q1NP_CONTACT_WORKSPACE), INTENT(IN) :: WS
          INTEGER, INTENT(IN) :: KQ1NP_TAB(:,:)
          INTEGER, INTENT(IN) :: IQ1NP_TAB(:)
          REAL(KIND=WP), INTENT(IN) :: Q1NP_KTAB(:)
          REAL(KIND=WP), INTENT(IN) :: X_COORDS(3,NUMNOD)
          INTEGER, INTENT(IN) :: NUMNOD
          REAL(KIND=WP), INTENT(IN) :: GAP
          TYPE(Q1NP_CONTACT_PAIR), ALLOCATABLE, INTENT(INOUT) :: PAIRS(:)
          INTEGER, INTENT(INOUT) :: N_PAIRS

          REAL(KIND=WP) :: GAP_CONTACT

          N_PAIRS = 0
          GAP_CONTACT = MAX(Q1NP_CONTACT_GAP_FALLBACK, ABS(GAP))

          IF (ALLOCATED(PAIRS)) DEALLOCATE(PAIRS)
          ALLOCATE(PAIRS(MAX(1, WS%NPTS_B)))

          CALL Q1NP_CONTACT_NARROW_PHASE_PROJECT( &
     &        KQ1NP_TAB, IQ1NP_TAB, Q1NP_KTAB, X_COORDS, &
     &        NUMNOD, GAP_CONTACT, &
        &        WS%NPTS_A, &
     &        WS%ELEM_IDS_A, WS%XI_A, WS%ETA_A, &
     &        WS%SURF_POINTS_B, WS%NPTS_B, &
     &        WS%ELEM_IDS_B, WS%XI_B, WS%ETA_B, &
     &        WS%CANDIDATE_IA, WS%CANDIDATE_COUNT, &
     &        PAIRS, N_PAIRS)

        END SUBROUTINE Q1NP_CONTACT_NARROW_PHASE

!=======================================================================
!   Q1NP_CONTACT_FORCE_ASSEMBLY
!
!   Penalty force computation and FCONT scatter for penetrating pairs.
!=======================================================================
!||====================================================================
!||    q1np_contact_force_assembly           ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_driver_int7              ../engine/source/interfaces/ists_q1np/q1np_contact_driver.F90
!||--- calls      -----------------------------------------------------
!||    q1np_contact_compute_penalty_forces   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||====================================================================
        SUBROUTINE Q1NP_CONTACT_FORCE_ASSEMBLY( &
     &      PAIRS, N_PAIRS, &
     &      KQ1NP_TAB, IQ1NP_TAB, IQ1NP_BULK_TAB, IRECTM, Q1NP_KTAB, &
     &      X_COORDS, NUMNOD, GAP, A, STIFN, IGSTI, KMIN, KMAX, &
     &      NSV, STFNS, NSN, STFM, NRTM, FCONT, DO_FCONT)
          TYPE(Q1NP_CONTACT_PAIR), INTENT(IN) :: PAIRS(:)
          INTEGER, INTENT(IN) :: N_PAIRS
          INTEGER, INTENT(IN) :: KQ1NP_TAB(:,:)
          INTEGER, INTENT(IN) :: IQ1NP_TAB(:)
          INTEGER, INTENT(IN) :: IQ1NP_BULK_TAB(:)
          INTEGER, INTENT(IN) :: IRECTM(:)
          INTEGER, INTENT(IN) :: NSV(:)
          REAL(KIND=WP), INTENT(IN) :: Q1NP_KTAB(:)
          INTEGER, INTENT(IN) :: NUMNOD, IGSTI, NSN, NRTM
          REAL(KIND=WP), INTENT(IN) :: GAP, KMIN, KMAX
          REAL(KIND=WP), INTENT(IN) :: STFNS(:), STFM(:)
          REAL(KIND=WP), INTENT(IN) :: X_COORDS(3,NUMNOD)
          REAL(KIND=WP), INTENT(INOUT) :: A(3,NUMNOD)
          REAL(KIND=WP), INTENT(INOUT) :: STIFN(NUMNOD)
          REAL(KIND=WP), INTENT(INOUT) :: FCONT(3,NUMNOD)
          LOGICAL, INTENT(IN) :: DO_FCONT

          REAL(KIND=WP) :: GAP_CONTACT

          GAP_CONTACT = MAX(Q1NP_CONTACT_GAP_FALLBACK, ABS(GAP))

          CALL Q1NP_CONTACT_COMPUTE_PENALTY_FORCES( &
     &        PAIRS, N_PAIRS, &
     &        KQ1NP_TAB, IQ1NP_TAB, IQ1NP_BULK_TAB, IRECTM, &
     &        Q1NP_KTAB, X_COORDS, &
     &        NUMNOD, GAP_CONTACT, A, STIFN, IGSTI, KMIN, KMAX, &
     &        NSV, STFNS, NSN, STFM, NRTM, FCONT, DO_FCONT)

        END SUBROUTINE Q1NP_CONTACT_FORCE_ASSEMBLY

!=======================================================================
!   Q1NP_CONTACT_WORKSPACE_FREE
!
!   Deallocate all arrays in the broad-phase workspace.
!=======================================================================
!||====================================================================
!||    q1np_contact_workspace_free   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_driver_int7      ../engine/source/interfaces/ists_q1np/q1np_contact_driver.F90
!||--- calls      -----------------------------------------------------
!||====================================================================
        SUBROUTINE Q1NP_CONTACT_WORKSPACE_FREE(WS)
          TYPE(Q1NP_CONTACT_WORKSPACE), INTENT(INOUT) :: WS
          IF (ALLOCATED(WS%SURF_POINTS_A)) CALL MY_DEALLOC(WS%SURF_POINTS_A)
          IF (ALLOCATED(WS%SURF_POINTS_B)) CALL MY_DEALLOC(WS%SURF_POINTS_B)
          IF (ALLOCATED(WS%ELEM_IDS_A)) CALL MY_DEALLOC(WS%ELEM_IDS_A)
          IF (ALLOCATED(WS%ELEM_IDS_B)) CALL MY_DEALLOC(WS%ELEM_IDS_B)
          IF (ALLOCATED(WS%XI_A)) CALL MY_DEALLOC(WS%XI_A)
          IF (ALLOCATED(WS%ETA_A)) CALL MY_DEALLOC(WS%ETA_A)
          IF (ALLOCATED(WS%XI_B)) CALL MY_DEALLOC(WS%XI_B)
          IF (ALLOCATED(WS%ETA_B)) CALL MY_DEALLOC(WS%ETA_B)
          IF (ALLOCATED(WS%CANDIDATE_IA)) CALL MY_DEALLOC(WS%CANDIDATE_IA)
          IF (ALLOCATED(WS%CANDIDATE_COUNT)) CALL MY_DEALLOC(WS%CANDIDATE_COUNT)
          IF (ALLOCATED(WS%CANDIDATE_OVERFLOW)) &
     &      CALL MY_DEALLOC(WS%CANDIDATE_OVERFLOW)
          WS%NPTS_A = 0
          WS%NPTS_B = 0
        END SUBROUTINE Q1NP_CONTACT_WORKSPACE_FREE

!=======================================================================
!   Q1NP_CONTACT_BP_BUILD_SURFACE_POINTS
!
!   Build a point cloud on the NURBS top surface of all Q1NP elements
!   belonging to a given knot set (i.e. one of the two contact surfaces).
!
!=======================================================================
!||====================================================================
!||    q1np_contact_bp_build_surface_points    ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_broad_phase                ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- calls      -----------------------------------------------------
!||    q1np_contact_extract_elem_data          ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_max_knot_len               ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_evaluate_nurbs_top_surface_point   ../engine/source/elements/solid/solid_q1np/q1np_nurbs_surface_eval_mod.F90
!||====================================================================
        SUBROUTINE Q1NP_CONTACT_BP_BUILD_SURFACE_POINTS( &
     &      KQ1NP_TAB, IQ1NP_TAB, Q1NP_KTAB, NUMELQ1NP,         &
     &      X_COORDS, NUMNOD, KNOT_SET_ID_FILTER,                 &
     &      NGP_SURF_U, NGP_SURF_V, SURF_POINTS, NPTS_TOTAL,     &
     &      PT_ELEM_IDS, PT_XI, PT_ETA, MAX_PTS)
!C----------------------------------------------------------------------
!C   D u m m y   A r g u m e n t s
!C----------------------------------------------------------------------
!C     SURF_POINTS(3,:)        - output point cloud coordinates                 (OUT)
!C     NPTS_TOTAL              - number of points actually written              (OUT)
!C     PT_ELEM_IDS(optional)   - element index for each point (optional)        (OUT)
!C     PT_XI(optional)         - xi  parametric coord for each point (optional) (OUT)
!C     PT_ETA(optional)        - eta parametric coord for each point (optional) (OUT)
!C----------------------------------------------------------------------
          INTEGER, INTENT(IN) :: KQ1NP_TAB(:,:)
          INTEGER, INTENT(IN) :: IQ1NP_TAB(:)
          INTEGER, INTENT(IN) :: MAX_PTS
          REAL(KIND=WP), INTENT(IN) :: Q1NP_KTAB(:)
          INTEGER, INTENT(IN) :: NUMELQ1NP, NUMNOD
          INTEGER, INTENT(IN) :: KNOT_SET_ID_FILTER
          INTEGER, INTENT(IN) :: NGP_SURF_U, NGP_SURF_V
          REAL(KIND=WP), INTENT(IN)  :: X_COORDS(3,NUMNOD)
          REAL(KIND=WP), INTENT(INOUT) :: SURF_POINTS(3,MAX_PTS)
          INTEGER, INTENT(INOUT) :: NPTS_TOTAL
          INTEGER, INTENT(INOUT), OPTIONAL :: PT_ELEM_IDS(:)
          REAL(KIND=WP), INTENT(INOUT), OPTIONAL :: PT_XI(:), PT_ETA(:)
!C----------------------------------------------------------------------
!C   L o c a l   V a r i a b l e s
!C----------------------------------------------------------------------
          INTEGER :: IEL, IU, IV, U_LEN, V_LEN
          INTEGER :: U_MAX, V_MAX
          INTEGER :: P_CUR, Q_CUR, NCTRL
          INTEGER :: ELEM_U_IDX, ELEM_V_IDX
          INTEGER, PARAMETER :: MAX_CTRL = 50
          INTEGER :: CTRL_IDS(MAX_CTRL)
          REAL(KIND=WP) :: XI_SAMPLE, ETA_SAMPLE
          REAL(KIND=WP) :: XYZ(3)
          REAL(KIND=WP), ALLOCATABLE :: U_KNOT_WS(:), V_KNOT_WS(:)

          NPTS_TOTAL = 0

          ! Compute the maximum knot vector length in U and V
          CALL Q1NP_CONTACT_MAX_KNOT_LEN(KQ1NP_TAB, NUMELQ1NP, U_MAX, V_MAX)
          CALL MY_ALLOC(U_KNOT_WS, U_MAX, "U_KNOT_WS")
          CALL MY_ALLOC(V_KNOT_WS, V_MAX, "V_KNOT_WS")

          DO IEL = 1, NUMELQ1NP
            IF (KQ1NP_TAB(KQ1NP_KNOT_SET, IEL) /= KNOT_SET_ID_FILTER) CYCLE
            
            ! Extract the element data
            CALL Q1NP_CONTACT_EXTRACT_ELEM_DATA( &
     &        IEL, KQ1NP_TAB, IQ1NP_TAB, Q1NP_KTAB, &
     &        P_CUR, Q_CUR, NCTRL, ELEM_U_IDX, ELEM_V_IDX, &
     &        CTRL_IDS, U_KNOT_WS, V_KNOT_WS, U_LEN, V_LEN)

            ! Sample the surface of the element at the Gauss sample points
            DO IV = 1, NGP_SURF_V
              ETA_SAMPLE = Q1NP_GAUSS(IV, NGP_SURF_V)
              DO IU = 1, NGP_SURF_U
                XI_SAMPLE = Q1NP_GAUSS(IU, NGP_SURF_U)

                ! Evaluate the NURBS surface point at the sample point
                CALL Q1NP_EVALUATE_NURBS_TOP_SURFACE_POINT( &
     &              XI_SAMPLE, ETA_SAMPLE,                   &
     &              P_CUR, Q_CUR,                            &
     &              ELEM_U_IDX, ELEM_V_IDX,                  &
     &              U_KNOT_WS(1:U_LEN), V_KNOT_WS(1:V_LEN), &
     &              MIN(NCTRL, MAX_CTRL), CTRL_IDS,          &
     &              X_COORDS, NUMNOD, XYZ)

                NPTS_TOTAL = NPTS_TOTAL + 1
                SURF_POINTS(1:3, NPTS_TOTAL) = XYZ(1:3)
                IF (PRESENT(PT_ELEM_IDS)) PT_ELEM_IDS(NPTS_TOTAL) = IEL
                IF (PRESENT(PT_XI))       PT_XI(NPTS_TOTAL)  = XI_SAMPLE
                IF (PRESENT(PT_ETA))      PT_ETA(NPTS_TOTAL) = ETA_SAMPLE
              END DO
            END DO
          END DO

          CALL MY_DEALLOC(U_KNOT_WS)
          CALL MY_DEALLOC(V_KNOT_WS)

        END SUBROUTINE Q1NP_CONTACT_BP_BUILD_SURFACE_POINTS

!=======================================================================
!   Q1NP_CONTACT_BROAD_PHASE_VOXEL_MIN_DISTANCE
!
!   Voxel minimum Euclidean distance between two 3D point clouds.
!
!   Algorithm:
!     1. Compute axis-aligned bounding box (AABB) of point cloud A and B.
!     2. Choose a conservative B-box padding and voxel size based on the trigger tolerance.
!     3. Compute a coarse AABB-to-AABB distance and return early when
!        the point clouds are clearly farther apart than the padded
!        search window.
!     4. Build a uniform B-point voxel grid over the padded box.
!     5. For every A-point that lies inside that grid,
!        look up the 3x3x3 cell neighborhood and find the closest B-point.
!=======================================================================
!||====================================================================
!||    q1np_contact_broad_phase_voxel_min_distance   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_broad_phase                      ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- calls      -----------------------------------------------------
!||====================================================================
        SUBROUTINE Q1NP_CONTACT_BROAD_PHASE_VOXEL_MIN_DISTANCE( &
     &      SURF_POINTS_A, NPTS_A, SURF_POINTS_B, NPTS_B,       &
     &      TRIGGER_TOL,                                         &
     &      D_MIN_OUT, IDX_A_OUT, IDX_B_OUT,                    &
     &      CANDIDATE_IA, CANDIDATE_COUNT, CANDIDATE_OVERFLOW)
!C----------------------------------------------------------------------
!C   D u m m y   A r g u m e n t s
!C----------------------------------------------------------------------
!C     SURF_POINTS_A(3,NPTS_A)  - point cloud of surface A              (IN)
!C     NPTS_A                   - number of points in cloud A           (IN)
!C     SURF_POINTS_B(3,NPTS_B)  - point cloud of surface B              (IN)
!C     NPTS_B                   - number of points in cloud B           (IN)
!C     D_MIN_OUT                - minimum Euclidean distance            (OUT)
!C     IDX_A_OUT                - index in A of the closest pair        (OUT)
!C     IDX_B_OUT                - index in B of the closest pair        (OUT)
!C     CANDIDATE_IA(:,IB)       - voxel-derived A-candidates for B      (OUT)
!C     CANDIDATE_COUNT(IB)      - number of stored candidates for B     (OUT)
!C     CANDIDATE_OVERFLOW(IB)   - candidate list exceeded storage       (OUT)
!C----------------------------------------------------------------------
          INTEGER, INTENT(IN)  :: NPTS_A, NPTS_B
          REAL(KIND=WP), INTENT(IN)  :: SURF_POINTS_A(3, NPTS_A)
          REAL(KIND=WP), INTENT(IN)  :: SURF_POINTS_B(3, NPTS_B)
          REAL(KIND=WP), INTENT(IN)  :: TRIGGER_TOL
          REAL(KIND=WP), INTENT(INOUT) :: D_MIN_OUT
          INTEGER, INTENT(INOUT) :: IDX_A_OUT, IDX_B_OUT
          INTEGER, INTENT(INOUT) :: CANDIDATE_IA(:,:)
          INTEGER, INTENT(INOUT) :: CANDIDATE_COUNT(:)
          LOGICAL, INTENT(INOUT) :: CANDIDATE_OVERFLOW(:)
!C----------------------------------------------------------------------
!C   L o c a l   V a r i a b l e s
!C----------------------------------------------------------------------
          REAL(KIND=WP) :: XMIN_A, XMAX_A, YMIN_A, YMAX_A
          REAL(KIND=WP) :: ZMIN_A, ZMAX_A
          REAL(KIND=WP) :: XMIN_B, XMAX_B, YMIN_B, YMAX_B
          REAL(KIND=WP) :: ZMIN_B, ZMAX_B
          REAL(KIND=WP) :: CELL_SIZE, SEARCH_PADDING
          REAL(KIND=WP) :: SPAN_X, SPAN_Y, SPAN_Z
          INTEGER :: NBX, NBY, NBZ, NVOXELS
          INTEGER, ALLOCATABLE :: VOXEL(:), NEXT_PT(:)
          INTEGER :: IB, IA, IX, IY, IZ, CELLID, JJ
          INTEGER :: IX_LO, IX_HI, IY_LO, IY_HI, IZ_LO, IZ_HI
          INTEGER :: JX, JY, JZ
          REAL(KIND=WP) :: DX, DY, DZ, DIST_SQ, D_MIN_SQ
          REAL(KIND=WP), PARAMETER :: EPS_SPAN = 1.0E-12_WP
          REAL(KIND=WP) :: SPAN_X_SAFE, SPAN_Y_SAFE, SPAN_Z_SAFE
          REAL(KIND=WP) :: RX, RY, RZ
          INTEGER :: NCAND

          D_MIN_SQ  = HUGE(ONE)
          IDX_A_OUT = 0
          IDX_B_OUT = 0
          CANDIDATE_IA(:,:) = 0
          CANDIDATE_COUNT(:) = 0
          CANDIDATE_OVERFLOW(:) = .FALSE.

!   ----- Step 1a: bounding box of A points -----
          XMIN_A = SURF_POINTS_A(1, 1)
          XMAX_A = SURF_POINTS_A(1, 1)
          YMIN_A = SURF_POINTS_A(2, 1)
          YMAX_A = SURF_POINTS_A(2, 1)
          ZMIN_A = SURF_POINTS_A(3, 1)
          ZMAX_A = SURF_POINTS_A(3, 1)
          DO IA = 2, NPTS_A
            IF (SURF_POINTS_A(1,IA) < XMIN_A) XMIN_A = SURF_POINTS_A(1,IA)
            IF (SURF_POINTS_A(1,IA) > XMAX_A) XMAX_A = SURF_POINTS_A(1,IA)
            IF (SURF_POINTS_A(2,IA) < YMIN_A) YMIN_A = SURF_POINTS_A(2,IA)
            IF (SURF_POINTS_A(2,IA) > YMAX_A) YMAX_A = SURF_POINTS_A(2,IA)
            IF (SURF_POINTS_A(3,IA) < ZMIN_A) ZMIN_A = SURF_POINTS_A(3,IA)
            IF (SURF_POINTS_A(3,IA) > ZMAX_A) ZMAX_A = SURF_POINTS_A(3,IA)
          END DO

!   ----- Step 1b: bounding box of B points -----
          XMIN_B = SURF_POINTS_B(1, 1)
          XMAX_B = SURF_POINTS_B(1, 1)
          YMIN_B = SURF_POINTS_B(2, 1)
          YMAX_B = SURF_POINTS_B(2, 1)
          ZMIN_B = SURF_POINTS_B(3, 1)
          ZMAX_B = SURF_POINTS_B(3, 1)
          DO IB = 2, NPTS_B
            IF (SURF_POINTS_B(1,IB) < XMIN_B) XMIN_B = SURF_POINTS_B(1,IB)
            IF (SURF_POINTS_B(1,IB) > XMAX_B) XMAX_B = SURF_POINTS_B(1,IB)
            IF (SURF_POINTS_B(2,IB) < YMIN_B) YMIN_B = SURF_POINTS_B(2,IB)
            IF (SURF_POINTS_B(2,IB) > YMAX_B) YMAX_B = SURF_POINTS_B(2,IB)
            IF (SURF_POINTS_B(3,IB) < ZMIN_B) ZMIN_B = SURF_POINTS_B(3,IB)
            IF (SURF_POINTS_B(3,IB) > ZMAX_B) ZMAX_B = SURF_POINTS_B(3,IB)
          END DO
!   ----- Step 2: choose padding / cell size from the trigger tolerance -----
          CELL_SIZE = 1.25 * TRIGGER_TOL
          SEARCH_PADDING = 1.25 * TRIGGER_TOL
          ! Pad the bounding box of point cloud B by the search padding
          XMIN_B = XMIN_B - SEARCH_PADDING
          XMAX_B = XMAX_B + SEARCH_PADDING
          YMIN_B = YMIN_B - SEARCH_PADDING
          YMAX_B = YMAX_B + SEARCH_PADDING
          ZMIN_B = ZMIN_B - SEARCH_PADDING
          ZMAX_B = ZMAX_B + SEARCH_PADDING

!   ----- Step 3: coarse AABB rejection before voxel allocation -----
          DX = MAX(ZERO, MAX(XMIN_A - (XMAX_B - SEARCH_PADDING), &
     &                       (XMIN_B + SEARCH_PADDING) - XMAX_A))
          DY = MAX(ZERO, MAX(YMIN_A - (YMAX_B - SEARCH_PADDING), &
     &                       (YMIN_B + SEARCH_PADDING) - YMAX_A))
          DZ = MAX(ZERO, MAX(ZMIN_A - (ZMAX_B - SEARCH_PADDING), &
     &                       (ZMIN_B + SEARCH_PADDING) - ZMAX_A))
          D_MIN_OUT = SQRT(DX*DX + DY*DY + DZ*DZ)

!         If the sampled point-cloud AABBs are already farther apart
!         than the padded search window, skip the voxel grid build.
          IF (D_MIN_OUT > SEARCH_PADDING) THEN
            RETURN
          END IF

!   ----- Step 4: voxel grid dimensions -----
          SPAN_X = XMAX_B - XMIN_B
          SPAN_Y = YMAX_B - YMIN_B
          SPAN_Z = ZMAX_B - ZMIN_B
          SPAN_X_SAFE = MAX(SPAN_X, EPS_SPAN)
          SPAN_Y_SAFE = MAX(SPAN_Y, EPS_SPAN)
          SPAN_Z_SAFE = MAX(SPAN_Z, EPS_SPAN)
          ! Compute the number of voxels in each direction
          NBX = MAX(1, CEILING(SPAN_X / CELL_SIZE))
          NBY = MAX(1, CEILING(SPAN_Y / CELL_SIZE))
          NBZ = MAX(1, CEILING(SPAN_Z / CELL_SIZE))
          NVOXELS = NBX * NBY * NBZ

!   ----- Step 5: allocate and fill voxel grid with B points -----
          CALL MY_ALLOC(VOXEL, NVOXELS, "VOXEL")
          CALL MY_ALLOC(NEXT_PT, NPTS_B, "NEXT_PT")
          VOXEL(1:NVOXELS) = 0
          NEXT_PT(1:NPTS_B) = 0

          ! Insert every B-point into its voxel cell
          DO IB = 1, NPTS_B
            RX = REAL(NBX, WP) * (SURF_POINTS_B(1,IB) - XMIN_B) / SPAN_X_SAFE
            RY = REAL(NBY, WP) * (SURF_POINTS_B(2,IB) - YMIN_B) / SPAN_Y_SAFE
            RZ = REAL(NBZ, WP) * (SURF_POINTS_B(3,IB) - ZMIN_B) / SPAN_Z_SAFE
            IF (RX /= RX) RX = ZERO
            IF (RY /= RY) RY = ZERO
            IF (RZ /= RZ) RZ = ZERO
            IX = INT(RX)
            IY = INT(RY)
            IZ = INT(RZ)
            IX = MAX(0, MIN(NBX - 1, IX))
            IY = MAX(0, MIN(NBY - 1, IY))
            IZ = MAX(0, MIN(NBZ - 1, IZ))
            CELLID = IZ * NBX * NBY + IY * NBX + IX + 1
            CELLID = MAX(1, MIN(NVOXELS, CELLID))
            NEXT_PT(IB) = VOXEL(CELLID)
            VOXEL(CELLID) = IB
          END DO

!   ----- Step 6: query -- for each A-point, check 3x3x3 neighborhood -----
          DO IA = 1, NPTS_A
!           Skip A-points outside the padded B bounding box
            IF (SURF_POINTS_A(1,IA) < XMIN_B) CYCLE
            IF (SURF_POINTS_A(1,IA) > XMAX_B) CYCLE
            IF (SURF_POINTS_A(2,IA) < YMIN_B) CYCLE
            IF (SURF_POINTS_A(2,IA) > YMAX_B) CYCLE
            IF (SURF_POINTS_A(3,IA) < ZMIN_B) CYCLE
            IF (SURF_POINTS_A(3,IA) > ZMAX_B) CYCLE

            RX = REAL(NBX, WP) * (SURF_POINTS_A(1,IA) - XMIN_B) / SPAN_X_SAFE
            RY = REAL(NBY, WP) * (SURF_POINTS_A(2,IA) - YMIN_B) / SPAN_Y_SAFE
            RZ = REAL(NBZ, WP) * (SURF_POINTS_A(3,IA) - ZMIN_B) / SPAN_Z_SAFE
            IF (RX /= RX) RX = ZERO
            IF (RY /= RY) RY = ZERO
            IF (RZ /= RZ) RZ = ZERO
            IX = INT(RX)
            IY = INT(RY)
            IZ = INT(RZ)
            IX = MAX(0, MIN(NBX - 1, IX))
            IY = MAX(0, MIN(NBY - 1, IY))
            IZ = MAX(0, MIN(NBZ - 1, IZ))

            IX_LO = MAX(0, IX - 1)
            IX_HI = MIN(NBX - 1, IX + 1)
            IY_LO = MAX(0, IY - 1)
            IY_HI = MIN(NBY - 1, IY + 1)
            IZ_LO = MAX(0, IZ - 1)
            IZ_HI = MIN(NBZ - 1, IZ + 1)

            DO JZ = IZ_LO, IZ_HI
              DO JY = IY_LO, IY_HI
                DO JX = IX_LO, IX_HI
                  CELLID = JZ * NBX * NBY + JY * NBX + JX + 1
                  CELLID = MAX(1, MIN(NVOXELS, CELLID))
                  JJ = VOXEL(CELLID)
                  DO WHILE (JJ > 0)
                    IF (JJ > NPTS_B) EXIT
                    NCAND = CANDIDATE_COUNT(JJ)
                    IF (NCAND < SIZE(CANDIDATE_IA, 1)) THEN
                      CANDIDATE_COUNT(JJ) = NCAND + 1
                      CANDIDATE_IA(NCAND + 1, JJ) = IA
                    ELSE
                      CANDIDATE_OVERFLOW(JJ) = .TRUE.
                    END IF
                    DX = SURF_POINTS_A(1,IA) - SURF_POINTS_B(1,JJ)
                    DY = SURF_POINTS_A(2,IA) - SURF_POINTS_B(2,JJ)
                    DZ = SURF_POINTS_A(3,IA) - SURF_POINTS_B(3,JJ)
                    DIST_SQ = DX*DX + DY*DY + DZ*DZ
                    IF (DIST_SQ < D_MIN_SQ) THEN
                      D_MIN_SQ  = DIST_SQ 
                      ! IDX_A_OUT: index of the closest point in surface A
                      ! IDX_B_OUT: index of the closest point in surface B
                      IDX_A_OUT = IA
                      IDX_B_OUT = JJ
                    END IF
                    JJ = NEXT_PT(JJ)
                    IF (JJ < 0) EXIT
                  END DO
                END DO
              END DO
            END DO
          END DO

          CALL MY_DEALLOC(VOXEL)
          CALL MY_DEALLOC(NEXT_PT)

        END SUBROUTINE Q1NP_CONTACT_BROAD_PHASE_VOXEL_MIN_DISTANCE

!=======================================================================
!   Q1NP_CONTACT_NARROW_PHASE_PROJECT
!
!   Narrow phase projection:
!     For each set-2 sample point, test all voxel-derived candidates
!     (or fall back to the global nearest search), keep the best valid
!     penetrating projection, and store one contact pair per B point.
!=======================================================================
!||====================================================================
!||    q1np_contact_narrow_phase_project   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_narrow_phase           ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- calls      -----------------------------------------------------
!||    q1np_contact_max_knot_len           ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_project_point_newton   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||====================================================================
        SUBROUTINE Q1NP_CONTACT_NARROW_PHASE_PROJECT( &
     &      KQ1NP_TAB, IQ1NP_TAB, Q1NP_KTAB, X_COORDS, &
     &      NUMNOD, GAP_CONTACT, &
     &      NPTS_A,                                      &
     &      ELEM_IDS_A, XI_A, ETA_A,                     &
     &      SURF_POINTS_B, NPTS_B,                       &
     &      ELEM_IDS_B, XI_B, ETA_B,                     &
     &      CANDIDATE_IA, CANDIDATE_COUNT,               &
     &      CONTACT_PAIRS, N_PAIRS)
          INTEGER, INTENT(IN) :: KQ1NP_TAB(:,:)
          INTEGER, INTENT(IN) :: IQ1NP_TAB(:)
          REAL(KIND=WP), INTENT(IN) :: Q1NP_KTAB(:)
          REAL(KIND=WP), INTENT(IN) :: X_COORDS(3,NUMNOD)
          INTEGER, INTENT(IN) :: NUMNOD
          REAL(KIND=WP), INTENT(IN) :: GAP_CONTACT
          INTEGER, INTENT(IN) :: NPTS_A
          INTEGER, INTENT(IN) :: ELEM_IDS_A(:)
          REAL(KIND=WP), INTENT(IN) :: XI_A(:), ETA_A(:)
          REAL(KIND=WP), INTENT(IN) :: SURF_POINTS_B(3,NPTS_B)
          INTEGER, INTENT(IN) :: NPTS_B
          INTEGER, INTENT(IN) :: ELEM_IDS_B(:)
          REAL(KIND=WP), INTENT(IN) :: XI_B(:), ETA_B(:)
          INTEGER, INTENT(IN) :: CANDIDATE_IA(:,:)
          INTEGER, INTENT(IN) :: CANDIDATE_COUNT(:)
          TYPE(Q1NP_CONTACT_PAIR), INTENT(INOUT) :: CONTACT_PAIRS(:)
          INTEGER, INTENT(INOUT) :: N_PAIRS

          INTEGER :: IA, IB, ICAND
          INTEGER :: BEST_IA, U_MAX, V_MAX
          REAL(KIND=WP) :: X_SRC(3)
          REAL(KIND=WP), ALLOCATABLE :: U_KNOT_WS(:), V_KNOT_WS(:)

          REAL(KIND=WP) :: XI_PROJ, ETA_PROJ, XYZ_PROJ(3), PROJ_DIST
          REAL(KIND=WP) :: SIGNED_PENETRATION
          REAL(KIND=WP) :: NORMAL_VEC(3)
          REAL(KIND=WP) :: RESIDUAL
          LOGICAL :: PROJ_VALID
          INTEGER :: N_ITER
          REAL(KIND=WP) :: BEST_XI_PROJ, BEST_ETA_PROJ
          REAL(KIND=WP) :: BEST_SIGNED_PENETRATION
          REAL(KIND=WP) :: BEST_NORMAL_VEC(3)

          N_PAIRS = 0
          IF (NPTS_A < 1 .OR. NPTS_B < 1) RETURN

          CALL Q1NP_CONTACT_MAX_KNOT_LEN( &
     &      KQ1NP_TAB, SIZE(KQ1NP_TAB, 2), U_MAX, V_MAX)
          CALL MY_ALLOC(U_KNOT_WS, U_MAX, "U_KNOT_WS")
          CALL MY_ALLOC(V_KNOT_WS, V_MAX, "V_KNOT_WS")

          DO IB = 1, NPTS_B
            X_SRC(1:3) = SURF_POINTS_B(1:3, IB)

            BEST_IA = 0
            BEST_SIGNED_PENETRATION = HUGE(ONE)

            DO ICAND = 1, CANDIDATE_COUNT(IB)
              IA = CANDIDATE_IA(ICAND, IB)
              IF (IA < 1 .OR. IA > NPTS_A) CYCLE

              CALL Q1NP_CONTACT_PROJECT_POINT_NEWTON( &
     &          X_SRC, KQ1NP_TAB, IQ1NP_TAB, Q1NP_KTAB, &
     &          X_COORDS, NUMNOD, ELEM_IDS_A(IA),       &
     &          XI_A(IA), ETA_A(IA),                    &
     &          XI_PROJ, ETA_PROJ, XYZ_PROJ, PROJ_DIST, &
     &          SIGNED_PENETRATION, NORMAL_VEC,         &
     &          RESIDUAL, N_ITER, PROJ_VALID,           &
     &          U_KNOT_WS, V_KNOT_WS)
              IF (.NOT. PROJ_VALID) CYCLE
              IF (SIGNED_PENETRATION >= GAP_CONTACT) CYCLE

              IF (SIGNED_PENETRATION < BEST_SIGNED_PENETRATION) THEN
                BEST_IA = IA
                BEST_XI_PROJ = XI_PROJ
                BEST_ETA_PROJ = ETA_PROJ
                BEST_SIGNED_PENETRATION = SIGNED_PENETRATION
                BEST_NORMAL_VEC(1:3) = NORMAL_VEC(1:3)
              END IF
            END DO

            IF (BEST_IA == 0) CYCLE
            IF (N_PAIRS >= SIZE(CONTACT_PAIRS)) EXIT

            N_PAIRS = N_PAIRS + 1
            CONTACT_PAIRS(N_PAIRS)%PENETRATION = BEST_SIGNED_PENETRATION
            CONTACT_PAIRS(N_PAIRS)%NORMAL(1:3) = BEST_NORMAL_VEC(1:3)
            CONTACT_PAIRS(N_PAIRS)%XI_PROJ     = BEST_XI_PROJ
            CONTACT_PAIRS(N_PAIRS)%ETA_PROJ    = BEST_ETA_PROJ
            CONTACT_PAIRS(N_PAIRS)%ELEM_A      = ELEM_IDS_A(BEST_IA)
            CONTACT_PAIRS(N_PAIRS)%XI_SRC      = XI_B(IB)
            CONTACT_PAIRS(N_PAIRS)%ETA_SRC     = ETA_B(IB)
            CONTACT_PAIRS(N_PAIRS)%ELEM_B      = ELEM_IDS_B(IB)
          END DO

          CALL MY_DEALLOC(U_KNOT_WS)
          CALL MY_DEALLOC(V_KNOT_WS)

        END SUBROUTINE Q1NP_CONTACT_NARROW_PHASE_PROJECT

!=======================================================================
!   Q1NP_CONTACT_COMPUTE_PENALTY_FORCES
!
!   For each penetrating contact pair, evaluate NURBS shape functions
!   on both surfaces and scatter penalty forces to control-point nodes.
!   Pair stiffness is evaluated directly from starter-side contact data.
!   Direct node/segment maps are used when available; otherwise the
!   evaluated Q1NP contact point is matched geometrically against the
!   starter NSV/STFNS and IRECTM/STFM data
!
!=======================================================================
!||====================================================================
!||    q1np_contact_compute_penalty_forces   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_force_assembly           ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- calls      -----------------------------------------------------
!||    q1np_contact_assign_pair_stiffness    ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_bilinear_weights         ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_build_node_to_stfns      ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_build_primary_seg_map    ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_evaluate_side            ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_export_accumulate        ../engine/source/interfaces/ists_q1np/q1np_contact_export.F90
!||    q1np_contact_max_knot_len             ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_node_map_size            ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||====================================================================
        SUBROUTINE Q1NP_CONTACT_COMPUTE_PENALTY_FORCES( &
     &      CONTACT_PAIRS, N_PAIRS, &
     &      KQ1NP_TAB, IQ1NP_TAB, IQ1NP_BULK_TAB, IRECTM, Q1NP_KTAB, X_COORDS, &
     &      NUMNOD, GAP_CONTACT, A, STIFN, IGSTI, KMIN, KMAX, &
     &      NSV, STFNS, NSN, STFM, NRTM, FCONT, DO_FCONT)
          TYPE(Q1NP_CONTACT_PAIR), INTENT(IN) :: CONTACT_PAIRS(:)
          INTEGER, INTENT(IN) :: N_PAIRS
          INTEGER, INTENT(IN) :: KQ1NP_TAB(:,:)
          INTEGER, INTENT(IN) :: IQ1NP_TAB(:)
          INTEGER, INTENT(IN) :: IQ1NP_BULK_TAB(:)
          INTEGER, INTENT(IN) :: IRECTM(:)
          INTEGER, INTENT(IN) :: NSV(:)
          REAL(KIND=WP), INTENT(IN)    :: Q1NP_KTAB(:)
          INTEGER, INTENT(IN)          :: NUMNOD, IGSTI, NSN, NRTM
          REAL(KIND=WP), INTENT(IN)    :: GAP_CONTACT, KMIN, KMAX
          REAL(KIND=WP), INTENT(IN)    :: X_COORDS(3,NUMNOD)
          REAL(KIND=WP), INTENT(IN)    :: STFNS(:), STFM(:)
          REAL(KIND=WP), INTENT(INOUT) :: A(3,NUMNOD)
          REAL(KIND=WP), INTENT(INOUT) :: STIFN(NUMNOD)
          REAL(KIND=WP), INTENT(INOUT) :: FCONT(3,NUMNOD)
          LOGICAL, INTENT(IN) :: DO_FCONT

          INTEGER, PARAMETER :: MAX_CTRL = 50
          REAL(KIND=WP), PARAMETER :: EPS = 1.0E-10_WP
          REAL(KIND=WP), PARAMETER :: EPS_STIFF = 1.0E-30_WP
          INTEGER :: IP, K, GID, U_MAX, V_MAX
          INTEGER :: NSN_EFF, NRTM_EFF, NODE_MAP_SIZE
          INTEGER :: NCTRL_A_EFF, NCTRL_B_EFF
          INTEGER :: CTRL_IDS_A(MAX_CTRL), CTRL_IDS_B(MAX_CTRL)
          INTEGER, ALLOCATABLE :: NODE_TO_STFNS(:)
          INTEGER, ALLOCATABLE :: ELEM_TO_STFM_SEG(:)
          REAL(KIND=WP), ALLOCATABLE :: U_KNOT_WS(:), V_KNOT_WS(:)
          REAL(KIND=WP) :: NVAL_A(MAX_CTRL), NVAL_B(MAX_CTRL)
          REAL(KIND=WP) :: F_PEN(3), F_NEG(3), F_MAG
          REAL(KIND=WP) :: F_MAX, PENETR_MAX
          REAL(KIND=WP) :: PEN_ABS, FAC, D1
          REAL(KIND=WP) :: K_A_PRIMARY, K_A_SECONDARY
          REAL(KIND=WP) :: K_B_PRIMARY, K_B_SECONDARY
          REAL(KIND=WP) :: K_PAIR, GAP_REF
          REAL(KIND=WP) :: K_PRIMARY, K_SECONDARY
          REAL(KIND=WP) :: PAIR_WEIGHT, D1_WEIGHTED
          INTEGER :: FEM_IDS_A(4), FEM_IDS_B(4)
          REAL(KIND=WP) :: BIL_W_A(4), BIL_W_B(4)
          LOGICAL :: HAS_FEM_A, HAS_FEM_B

          IF (N_PAIRS < 1) RETURN

          F_MAX     = ZERO
          PENETR_MAX = ZERO

          GAP_REF = MAX(Q1NP_CONTACT_GAP_FALLBACK, GAP_CONTACT)
          PAIR_WEIGHT = ONE / (Q1NP_CONTACT_BP_NGP_U * Q1NP_CONTACT_BP_NGP_V)

          NSN_EFF = MIN(NSN, SIZE(NSV), SIZE(STFNS))
          NRTM_EFF = MIN(NRTM, SIZE(STFM), SIZE(IRECTM) / 4)

          NODE_MAP_SIZE = Q1NP_CONTACT_NODE_MAP_SIZE(NUMNOD, NSV, NSN_EFF, IQ1NP_BULK_TAB)
          CALL MY_ALLOC(NODE_TO_STFNS, NODE_MAP_SIZE, "NODE_TO_STFNS")
          ! Build the node to STFNS map
          CALL Q1NP_CONTACT_BUILD_NODE_TO_STFNS(NSV, NSN_EFF, NODE_TO_STFNS)

          CALL MY_ALLOC(ELEM_TO_STFM_SEG, MAX(1, SIZE(KQ1NP_TAB, 2)), "ELEM_TO_STFM_SEG")  
          CALL Q1NP_CONTACT_BUILD_PRIMARY_SEG_MAP(KQ1NP_TAB, IQ1NP_BULK_TAB, IRECTM, NRTM_EFF, ELEM_TO_STFM_SEG)

          ! Compute the maximum knot vector length in U and V
          CALL Q1NP_CONTACT_MAX_KNOT_LEN( &
     &      KQ1NP_TAB, SIZE(KQ1NP_TAB, 2), U_MAX, V_MAX)
          CALL MY_ALLOC(U_KNOT_WS, U_MAX, "U_KNOT_WS")
          CALL MY_ALLOC(V_KNOT_WS, V_MAX, "V_KNOT_WS")

          DO IP = 1, N_PAIRS
            PEN_ABS = GAP_REF - CONTACT_PAIRS(IP)%PENETRATION
            IF (PEN_ABS <= ZERO) CYCLE

!           Evaluate both contact sides A/B.
            CALL Q1NP_CONTACT_EVALUATE_SIDE( &
     &        CONTACT_PAIRS(IP)%ELEM_A, CONTACT_PAIRS(IP)%XI_PROJ, CONTACT_PAIRS(IP)%ETA_PROJ, &
     &        KQ1NP_TAB, IQ1NP_TAB, IQ1NP_BULK_TAB, Q1NP_KTAB, &
     &        X_COORDS, NUMNOD, NODE_TO_STFNS, NSV, STFNS, NSN_EFF, &
     &        ELEM_TO_STFM_SEG, STFM, NRTM_EFF, IRECTM, &
     &        CTRL_IDS_A, NVAL_A, NCTRL_A_EFF, K_A_PRIMARY, K_A_SECONDARY, &
     &        U_KNOT_WS, V_KNOT_WS)

            CALL Q1NP_CONTACT_EVALUATE_SIDE( &
     &        CONTACT_PAIRS(IP)%ELEM_B, CONTACT_PAIRS(IP)%XI_SRC, CONTACT_PAIRS(IP)%ETA_SRC, &
     &        KQ1NP_TAB, IQ1NP_TAB, IQ1NP_BULK_TAB, Q1NP_KTAB, &
     &        X_COORDS, NUMNOD, NODE_TO_STFNS, NSV, STFNS, NSN_EFF, &
     &        ELEM_TO_STFM_SEG, STFM, NRTM_EFF, IRECTM, &
     &        CTRL_IDS_B, NVAL_B, NCTRL_B_EFF, K_B_PRIMARY, K_B_SECONDARY, &
     &        U_KNOT_WS, V_KNOT_WS)

!           --- Prepare FCONT scatter to HEX8 top-face grid nodes ---
            HAS_FEM_A = .FALSE.
            HAS_FEM_B = .FALSE.
            IF (DO_FCONT .AND. Q1NP_FCONT_GRID_READY) THEN
              IF (CONTACT_PAIRS(IP)%ELEM_A > 0 .AND. &
     &            CONTACT_PAIRS(IP)%ELEM_A <= SIZE(Q1NP_FCONT_GRID_IDS,2)) THEN
                FEM_IDS_A(1:4) = Q1NP_FCONT_GRID_IDS(1:4, &
     &            CONTACT_PAIRS(IP)%ELEM_A)
                HAS_FEM_A = ALL(FEM_IDS_A(1:4) > 0)
                IF (HAS_FEM_A) CALL Q1NP_CONTACT_BILINEAR_WEIGHTS( &
     &            CONTACT_PAIRS(IP)%XI_PROJ, &
     &            CONTACT_PAIRS(IP)%ETA_PROJ, BIL_W_A)
              END IF
              IF (CONTACT_PAIRS(IP)%ELEM_B > 0 .AND. &
     &            CONTACT_PAIRS(IP)%ELEM_B <= SIZE(Q1NP_FCONT_GRID_IDS,2)) THEN
                FEM_IDS_B(1:4) = Q1NP_FCONT_GRID_IDS(1:4, &
     &            CONTACT_PAIRS(IP)%ELEM_B)
                HAS_FEM_B = ALL(FEM_IDS_B(1:4) > 0)
                IF (HAS_FEM_B) CALL Q1NP_CONTACT_BILINEAR_WEIGHTS( &
     &            CONTACT_PAIRS(IP)%XI_SRC, &
     &            CONTACT_PAIRS(IP)%ETA_SRC, BIL_W_B)
              END IF
            END IF

!           --- Pair stiffness selection ---
            CALL Q1NP_CONTACT_ASSIGN_PAIR_STIFFNESS( &
     &        CONTACT_PAIRS(IP)%ELEM_A, K_A_PRIMARY, K_A_SECONDARY, &
     &        CONTACT_PAIRS(IP)%ELEM_B, K_B_PRIMARY, K_B_SECONDARY, &
     &        KQ1NP_TAB, K_PRIMARY, K_SECONDARY)
            IF (K_PRIMARY <= EPS_STIFF .OR. K_SECONDARY <= EPS_STIFF) CYCLE

            K_PAIR = Q1NP_CONTACT_STIFFNESS(K_PRIMARY, K_SECONDARY, IGSTI, KMIN, KMAX)

!           --- gap scaling ---
            FAC = 1.0_WP
            IF (GAP_REF > EPS .AND. PEN_ABS < GAP_REF) THEN
              FAC = GAP_REF / MAX(EPS, (GAP_REF - PEN_ABS))
            END IF

            D1 = 0.5_WP * K_PAIR * FAC

            D1_WEIGHTED = PAIR_WEIGHT * D1
            F_MAG = D1_WEIGHTED * PEN_ABS
            F_PEN(1:3) = F_MAG * CONTACT_PAIRS(IP)%NORMAL(1:3)

            IF (F_MAG > F_MAX) F_MAX = F_MAG
            IF (PEN_ABS > PENETR_MAX) PENETR_MAX = PEN_ABS

            F_NEG(1:3) = (-ONE) * F_PEN(1:3)
            CALL Q1NP_CONTACT_EXPORT_ACCUMULATE( &
     &          CONTACT_PAIRS(IP)%ELEM_A, F_NEG, PEN_ABS)
            CALL Q1NP_CONTACT_EXPORT_ACCUMULATE( &
     &          CONTACT_PAIRS(IP)%ELEM_B,  F_PEN, PEN_ABS)

!           --- Scatter to A control points: -N_k * F ---
            DO K = 1, NCTRL_A_EFF
              GID = CTRL_IDS_A(K)
              IF (GID <= 0 .OR. GID > NUMNOD) CYCLE
              A(1, GID) = A(1, GID) - NVAL_A(K) * F_PEN(1)
              A(2, GID) = A(2, GID) - NVAL_A(K) * F_PEN(2)
              A(3, GID) = A(3, GID) - NVAL_A(K) * F_PEN(3)
              STIFN(GID) = STIFN(GID) + D1_WEIGHTED * ABS(NVAL_A(K))
            END DO

!           --- FCONT scatter to FEM nodes for side A ---
            IF (HAS_FEM_A) THEN
              DO K = 1, 4
                GID = FEM_IDS_A(K)
                IF (GID <= 0 .OR. GID > NUMNOD) CYCLE
                FCONT(1, GID) = FCONT(1, GID) - BIL_W_A(K) * F_PEN(1)
                FCONT(2, GID) = FCONT(2, GID) - BIL_W_A(K) * F_PEN(2)
                FCONT(3, GID) = FCONT(3, GID) - BIL_W_A(K) * F_PEN(3)
              END DO
            END IF

!           --- Scatter to B control points: +N_j * F ---
            DO K = 1, NCTRL_B_EFF
              GID = CTRL_IDS_B(K)
              IF (GID <= 0 .OR. GID > NUMNOD) CYCLE
              A(1, GID) = A(1, GID) + NVAL_B(K) * F_PEN(1)
              A(2, GID) = A(2, GID) + NVAL_B(K) * F_PEN(2)
              A(3, GID) = A(3, GID) + NVAL_B(K) * F_PEN(3)
              STIFN(GID) = STIFN(GID) + D1_WEIGHTED * ABS(NVAL_B(K))
            END DO

!           --- FCONT scatter to FEM nodes for side B ---
            IF (HAS_FEM_B) THEN
              DO K = 1, 4
                GID = FEM_IDS_B(K)
                IF (GID <= 0 .OR. GID > NUMNOD) CYCLE
                FCONT(1, GID) = FCONT(1, GID) + BIL_W_B(K) * F_PEN(1)
                FCONT(2, GID) = FCONT(2, GID) + BIL_W_B(K) * F_PEN(2)
                FCONT(3, GID) = FCONT(3, GID) + BIL_W_B(K) * F_PEN(3)
              END DO
            END IF
          END DO

          CALL MY_DEALLOC(NODE_TO_STFNS)
          CALL MY_DEALLOC(ELEM_TO_STFM_SEG)
          CALL MY_DEALLOC(U_KNOT_WS)
          CALL MY_DEALLOC(V_KNOT_WS)

        END SUBROUTINE Q1NP_CONTACT_COMPUTE_PENALTY_FORCES

!=======================================================================
!   Q1NP_CONTACT_EVALUATE_SIDE
!
!   Evaluate one contact side (A or B) for a given element and parametric
!   point. This shared helper avoids duplicated logic while keeping the
!   side-specific inputs explicit at the call site.
!=======================================================================
!||====================================================================
!||    q1np_contact_evaluate_side            ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_compute_penalty_forces   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- calls      -----------------------------------------------------
!||    q1np_contact_extract_elem_data        ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_interpolate_point        ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_evaluate_nurbs_shape_values      ../engine/source/elements/solid/solid_q1np/q1np_nurbs_surface_eval_mod.F90
!||====================================================================
        SUBROUTINE Q1NP_CONTACT_EVALUATE_SIDE( &
     &      ELEM_IDX, XI, ETA, &
     &      KQ1NP_TAB, IQ1NP_TAB, IQ1NP_BULK_TAB, Q1NP_KTAB, &
     &      X_COORDS, NUMNOD, NODE_TO_STFNS, NSV, STFNS, NSN_EFF, &
     &      ELEM_TO_STFM_SEG, STFM, NRTM_EFF, IRECTM, &
     &      CTRL_IDS, NVALS, NCTRL_EFF, K_PRIMARY, K_SECONDARY, &
     &      U_KNOT_WS, V_KNOT_WS)
          INTEGER, INTENT(IN) :: ELEM_IDX, NUMNOD, NSN_EFF, NRTM_EFF
          INTEGER, INTENT(IN) :: KQ1NP_TAB(:,:), IQ1NP_TAB(:), IQ1NP_BULK_TAB(:)
          INTEGER, INTENT(IN) :: NODE_TO_STFNS(:), NSV(:), ELEM_TO_STFM_SEG(:), IRECTM(:)
          REAL(KIND=WP), INTENT(IN) :: Q1NP_KTAB(:), X_COORDS(3,NUMNOD), STFNS(:), STFM(:)
          REAL(KIND=WP), INTENT(IN) :: XI, ETA
          INTEGER, INTENT(INOUT) :: CTRL_IDS(50), NCTRL_EFF
          REAL(KIND=WP), INTENT(INOUT) :: NVALS(50), K_PRIMARY, K_SECONDARY
          REAL(KIND=WP), INTENT(INOUT) :: U_KNOT_WS(:), V_KNOT_WS(:)

          INTEGER :: P_CUR, Q_CUR, NCTRL, ELEM_U_IDX, ELEM_V_IDX
          INTEGER :: U_LEN, V_LEN
          REAL(KIND=WP) :: X_PAIR(3)

          NVALS(1:50) = ZERO
          CTRL_IDS(1:50) = 0

          CALL Q1NP_CONTACT_EXTRACT_ELEM_DATA( &
     &      ELEM_IDX, KQ1NP_TAB, IQ1NP_TAB, Q1NP_KTAB, &
     &      P_CUR, Q_CUR, NCTRL, ELEM_U_IDX, ELEM_V_IDX, &
     &      CTRL_IDS, U_KNOT_WS, V_KNOT_WS, U_LEN, V_LEN)

          NCTRL_EFF = MIN(NCTRL, 50)
          CALL Q1NP_EVALUATE_NURBS_SHAPE_VALUES( &
     &      XI, ETA, P_CUR, Q_CUR, ELEM_U_IDX, ELEM_V_IDX, &
     &      U_KNOT_WS(1:U_LEN), V_KNOT_WS(1:V_LEN), &
     &      NCTRL_EFF, NVALS)

          CALL Q1NP_CONTACT_INTERPOLATE_POINT( &
     &      CTRL_IDS, NVALS, NCTRL_EFF, X_COORDS, NUMNOD, X_PAIR)

          K_SECONDARY = Q1NP_CONTACT_SECONDARY_SIDE_STIFFNESS( &
     &      ELEM_IDX, XI, ETA, &
     &      KQ1NP_TAB, IQ1NP_BULK_TAB, NODE_TO_STFNS, NSV, STFNS, NSN_EFF, &
     &      CTRL_IDS, NVALS, NCTRL_EFF, X_PAIR, X_COORDS, NUMNOD)

          K_PRIMARY = Q1NP_CONTACT_PRIMARY_SIDE_STIFFNESS( &
     &      ELEM_IDX, ELEM_TO_STFM_SEG, STFM, NRTM_EFF, IRECTM, X_PAIR, X_COORDS, NUMNOD)
        END SUBROUTINE Q1NP_CONTACT_EVALUATE_SIDE

!=======================================================================
!   Q1NP_CONTACT_NODE_MAP_SIZE
!=======================================================================
!||====================================================================
!||    q1np_contact_node_map_size            ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_compute_penalty_forces   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||====================================================================
        INTEGER FUNCTION Q1NP_CONTACT_NODE_MAP_SIZE( &
     &      NUMNOD, NSV, NSN_EFF, IQ1NP_BULK_TAB)
          INTEGER, INTENT(IN) :: NUMNOD, NSN_EFF
          INTEGER, INTENT(IN) :: NSV(:)
          INTEGER, INTENT(IN) :: IQ1NP_BULK_TAB(:)

          INTEGER :: I, NEFF, NODE_ID

          Q1NP_CONTACT_NODE_MAP_SIZE = MAX(1, NUMNOD)

          NEFF = MIN(NSN_EFF, SIZE(NSV))
          DO I = 1, NEFF
            NODE_ID = ABS(NSV(I))
            IF (NODE_ID > Q1NP_CONTACT_NODE_MAP_SIZE) THEN
              Q1NP_CONTACT_NODE_MAP_SIZE = NODE_ID
            END IF
          END DO

          DO I = 1, SIZE(IQ1NP_BULK_TAB)
            NODE_ID = ABS(IQ1NP_BULK_TAB(I))
            IF (NODE_ID > Q1NP_CONTACT_NODE_MAP_SIZE) THEN
              Q1NP_CONTACT_NODE_MAP_SIZE = NODE_ID
            END IF
          END DO
        END FUNCTION Q1NP_CONTACT_NODE_MAP_SIZE

!=======================================================================
!   Q1NP_CONTACT_BUILD_NODE_TO_STFNS
!=======================================================================
        !-----------------------------------------------------------------------
        ! Q1NP_CONTACT_BUILD_NODE_TO_STFNS
        !
        ! Build an index array mapping node IDs (from NSV) to their position
        ! in the surface node vector (NSV).
        !
        ! This mapping allows fast lookup: node_id --> associated NSV index.
        !
        !-----------------------------------------------------------------------
!||====================================================================
!||    q1np_contact_build_node_to_stfns      ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_compute_penalty_forces   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||====================================================================
        SUBROUTINE Q1NP_CONTACT_BUILD_NODE_TO_STFNS(NSV, NSN_EFF, NODE_TO_STFNS)
          INTEGER, INTENT(IN) :: NSV(:) ! NSV is the surface node indices
          INTEGER, INTENT(IN) :: NSN_EFF ! NSN_EFF is the number of active nodes in NSV
          ! NODE_TO_STFNS maps node IDs to their position in NSV.
          INTEGER, INTENT(INOUT) :: NODE_TO_STFNS(:)

          INTEGER :: I, NODE_ID, NEFF

          ! Initialize the map to zero (no node mapped)
          NODE_TO_STFNS(:) = 0
          NEFF = MIN(NSN_EFF, SIZE(NSV))
          DO I = 1, NEFF
            NODE_ID = NSV(I)
            ! Only assign if node_id is positive and in range
            IF (NODE_ID > 0 .AND. NODE_ID <= SIZE(NODE_TO_STFNS)) THEN
              NODE_TO_STFNS(NODE_ID) = I
            ENDIF
          END DO
        END SUBROUTINE Q1NP_CONTACT_BUILD_NODE_TO_STFNS
   

!=======================================================================
!   Q1NP_CONTACT_BUILD_PRIMARY_SEG_MAP
!
!   Build a Q1NP element -> INT7 primary segment map once per force call.
!   The candidate surface-local segment order is checked first, then a
!   full node-set search keeps the mapping independent of Q1NP grid order.
!=======================================================================
!||====================================================================
!||    q1np_contact_build_primary_seg_map    ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_compute_penalty_forces   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- calls      -----------------------------------------------------
!||    q1np_contact_bilinear_weights         ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_bulk_nodes               ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_find_primary_segment     ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_knot_set_id              ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||====================================================================
        SUBROUTINE Q1NP_CONTACT_BUILD_PRIMARY_SEG_MAP( &
     &      KQ1NP_TAB, IQ1NP_BULK_TAB, IRECTM, NRTM_EFF, ELEM_TO_STFM_SEG)
          INTEGER, INTENT(IN) :: KQ1NP_TAB(:,:)
          INTEGER, INTENT(IN) :: IQ1NP_BULK_TAB(:)
          INTEGER, INTENT(IN) :: IRECTM(:)
          INTEGER, INTENT(IN) :: NRTM_EFF
          INTEGER, INTENT(INOUT) :: ELEM_TO_STFM_SEG(:)

          INTEGER :: ELEM_IDX, SEG_GUESS, NELEM

          ELEM_TO_STFM_SEG(:) = 0
          IF (NRTM_EFF <= 0) RETURN

          SEG_GUESS = 0
          NELEM = MIN(SIZE(KQ1NP_TAB, 2), SIZE(ELEM_TO_STFM_SEG))
          DO ELEM_IDX = 1, NELEM
            IF (Q1NP_CONTACT_KNOT_SET_ID(ELEM_IDX, KQ1NP_TAB) == 2) THEN
              SEG_GUESS = SEG_GUESS + 1
            END IF
            ELEM_TO_STFM_SEG(ELEM_IDX) = Q1NP_CONTACT_FIND_PRIMARY_SEGMENT( &
     &        ELEM_IDX, KQ1NP_TAB, IQ1NP_BULK_TAB, IRECTM, NRTM_EFF, SEG_GUESS)
          END DO
        END SUBROUTINE Q1NP_CONTACT_BUILD_PRIMARY_SEG_MAP

!=======================================================================
!   Q1NP_CONTACT_SECONDARY_SIDE_STIFFNESS
!=======================================================================
        REAL(KIND=WP) FUNCTION Q1NP_CONTACT_SECONDARY_SIDE_STIFFNESS( &
     &      ELEM_IDX, XI, ETA, KQ1NP_TAB, IQ1NP_BULK_TAB, &
     &      NODE_TO_STFNS, NSV, STFNS, NSN_EFF, &
     &      CTRL_IDS, NVALS, NCTRL_EFF, X_PAIR, X_COORDS, NUMNOD)
          INTEGER, INTENT(IN) :: ELEM_IDX, NSN_EFF, NCTRL_EFF, NUMNOD
          INTEGER, INTENT(IN) :: KQ1NP_TAB(:,:)
          INTEGER, INTENT(IN) :: IQ1NP_BULK_TAB(:)
          INTEGER, INTENT(IN) :: NODE_TO_STFNS(:)
          INTEGER, INTENT(IN) :: NSV(:)
          INTEGER, INTENT(IN) :: CTRL_IDS(:)
          REAL(KIND=WP), INTENT(IN) :: STFNS(:)
          REAL(KIND=WP), INTENT(IN) :: NVALS(:)
          REAL(KIND=WP), INTENT(IN) :: X_PAIR(3)
          REAL(KIND=WP), INTENT(IN) :: X_COORDS(3,NUMNOD)
          REAL(KIND=WP), INTENT(IN) :: XI, ETA

          INTEGER :: K, IDX, NODE_ID, MATCH_COUNT
          INTEGER :: NODE_IDS(4)
          LOGICAL :: HAS_NODES
          REAL(KIND=WP) :: K_NODE, K_SUM, WEIGHT_SUM, K_CTRL
          REAL(KIND=WP) :: WEIGHT(4)
          REAL(KIND=WP), PARAMETER :: EPS_STIFF = 1.0E-30_WP

          Q1NP_CONTACT_SECONDARY_SIDE_STIFFNESS = ZERO

!         First try the projected Q1NP control-point set.
!         This keeps the secondary lookup consistent with the actual
!         NURBS evaluation used for force projection.
          K_CTRL = Q1NP_CONTACT_CONTROL_POINT_SECONDARY_STIFFNESS( &
     &      CTRL_IDS, NVALS, NCTRL_EFF, NODE_TO_STFNS, STFNS, NSN_EFF)
          IF (K_CTRL > EPS_STIFF) THEN
            Q1NP_CONTACT_SECONDARY_SIDE_STIFFNESS = K_CTRL
            RETURN
          END IF
          IF (Q1NP_CONTACT_DISABLE_SECONDARY_FALLBACK) RETURN

          CALL Q1NP_CONTACT_BULK_NODES( &
     &      ELEM_IDX, KQ1NP_TAB, IQ1NP_BULK_TAB, NODE_IDS, HAS_NODES)

          IF (HAS_NODES) THEN
            CALL Q1NP_CONTACT_BILINEAR_WEIGHTS(XI, ETA, WEIGHT)
            K_SUM = ZERO
            WEIGHT_SUM = ZERO
            MATCH_COUNT = 0
            DO K = 1, 4
              NODE_ID = NODE_IDS(K)
              IF (NODE_ID > 0 .AND. NODE_ID <= SIZE(NODE_TO_STFNS)) THEN
                IDX = NODE_TO_STFNS(NODE_ID)
                IF (IDX > 0 .AND. IDX <= NSN_EFF .AND. IDX <= SIZE(STFNS)) THEN
                  MATCH_COUNT = MATCH_COUNT + 1
                  K_NODE = ABS(STFNS(IDX))
                  IF (K_NODE > EPS_STIFF .AND. WEIGHT(K) > EPS_STIFF) THEN
                    K_SUM = K_SUM + WEIGHT(K) * K_NODE
                    WEIGHT_SUM = WEIGHT_SUM + WEIGHT(K)
                  END IF
                END IF
              END IF
            END DO

            IF (MATCH_COUNT == 4 .AND. WEIGHT_SUM > EPS_STIFF) THEN
              Q1NP_CONTACT_SECONDARY_SIDE_STIFFNESS = K_SUM / WEIGHT_SUM
              RETURN
            END IF
          END IF

!         IQ1NP_BULK_TAB holds the reconstructed HEX face, which can be the
!         opposite side of the Q1NP contact surface. If none of those nodes is
!         in NSV, use the evaluated contact point to sample local STFNS values.
          Q1NP_CONTACT_SECONDARY_SIDE_STIFFNESS = &
     &      Q1NP_CONTACT_NEAREST_SECONDARY_STIFFNESS( &
     &        X_PAIR, X_COORDS, NUMNOD, NSV, STFNS, NSN_EFF)
        END FUNCTION Q1NP_CONTACT_SECONDARY_SIDE_STIFFNESS

!=======================================================================
!   Q1NP_CONTACT_PRIMARY_SIDE_STIFFNESS
!=======================================================================
        REAL(KIND=WP) FUNCTION Q1NP_CONTACT_PRIMARY_SIDE_STIFFNESS( &
     &      ELEM_IDX, ELEM_TO_STFM_SEG, STFM, NRTM_EFF, &
     &      IRECTM, X_PAIR, X_COORDS, NUMNOD)
          INTEGER, INTENT(IN) :: ELEM_IDX, NRTM_EFF, NUMNOD
          INTEGER, INTENT(IN) :: ELEM_TO_STFM_SEG(:)
          INTEGER, INTENT(IN) :: IRECTM(:)
          REAL(KIND=WP), INTENT(IN) :: STFM(:)
          REAL(KIND=WP), INTENT(IN) :: X_PAIR(3)
          REAL(KIND=WP), INTENT(IN) :: X_COORDS(3,NUMNOD)

          INTEGER :: SEG
          REAL(KIND=WP), PARAMETER :: EPS_STIFF = 1.0E-30_WP

          Q1NP_CONTACT_PRIMARY_SIDE_STIFFNESS = ZERO
          IF (ELEM_IDX <= 0 .OR. ELEM_IDX > SIZE(ELEM_TO_STFM_SEG)) RETURN
          SEG = ELEM_TO_STFM_SEG(ELEM_IDX)
          IF (SEG > 0 .AND. SEG <= NRTM_EFF .AND. SEG <= SIZE(STFM)) THEN
            Q1NP_CONTACT_PRIMARY_SIDE_STIFFNESS = ABS(STFM(SEG))
            IF (Q1NP_CONTACT_PRIMARY_SIDE_STIFFNESS > EPS_STIFF) RETURN
          END IF

!         The direct map can miss for the same reason as the secondary
!         lookup: Q1NP bulk nodes are not always the actual contact segment.
          Q1NP_CONTACT_PRIMARY_SIDE_STIFFNESS = &
     &      Q1NP_CONTACT_NEAREST_PRIMARY_STIFFNESS( &
     &        X_PAIR, IRECTM, STFM, NRTM_EFF, X_COORDS, NUMNOD)
        END FUNCTION Q1NP_CONTACT_PRIMARY_SIDE_STIFFNESS

!=======================================================================
!   Q1NP_CONTACT_CONTROL_POINT_SECONDARY_STIFFNESS
!=======================================================================
        REAL(KIND=WP) FUNCTION Q1NP_CONTACT_CONTROL_POINT_SECONDARY_STIFFNESS( &
     &      CTRL_IDS, NVALS, NCTRL_EFF, NODE_TO_STFNS, STFNS, NSN_EFF)
          INTEGER, INTENT(IN) :: CTRL_IDS(:)
          REAL(KIND=WP), INTENT(IN) :: NVALS(:)
          INTEGER, INTENT(IN) :: NCTRL_EFF, NSN_EFF
          INTEGER, INTENT(IN) :: NODE_TO_STFNS(:)
          REAL(KIND=WP), INTENT(IN) :: STFNS(:)

          INTEGER :: I, NEFF, NODE_ID, IDX
          REAL(KIND=WP) :: K_NODE, WEIGHT, K_SUM, WEIGHT_SUM
          REAL(KIND=WP), PARAMETER :: EPS_STIFF = 1.0E-30_WP

          Q1NP_CONTACT_CONTROL_POINT_SECONDARY_STIFFNESS = ZERO
          K_SUM = ZERO
          WEIGHT_SUM = ZERO
          NEFF = MIN(NCTRL_EFF, SIZE(CTRL_IDS), SIZE(NVALS))

          DO I = 1, NEFF
            NODE_ID = CTRL_IDS(I)
            IF (NODE_ID <= 0 .OR. NODE_ID > SIZE(NODE_TO_STFNS)) CYCLE

            IDX = NODE_TO_STFNS(NODE_ID)
            IF (IDX <= 0 .OR. IDX > NSN_EFF .OR. IDX > SIZE(STFNS)) CYCLE

            K_NODE = ABS(STFNS(IDX))
            WEIGHT = ABS(NVALS(I))
            IF (K_NODE <= EPS_STIFF .OR. WEIGHT <= EPS_STIFF) CYCLE

            K_SUM = K_SUM + WEIGHT * K_NODE
            WEIGHT_SUM = WEIGHT_SUM + WEIGHT
          END DO

          IF (WEIGHT_SUM > EPS_STIFF) THEN
            Q1NP_CONTACT_CONTROL_POINT_SECONDARY_STIFFNESS = K_SUM / WEIGHT_SUM
          END IF
        END FUNCTION Q1NP_CONTACT_CONTROL_POINT_SECONDARY_STIFFNESS

!=======================================================================
!   Q1NP_CONTACT_INTERPOLATE_POINT
!=======================================================================
!||====================================================================
!||    q1np_contact_interpolate_point   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_evaluate_side       ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- calls      -----------------------------------------------------
!||    q1np_contact_bilinear_weights    ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||====================================================================
        SUBROUTINE Q1NP_CONTACT_INTERPOLATE_POINT( &
     &      CTRL_IDS, NVALS, NVAL, X_COORDS, NUMNOD, X_PAIR)
          INTEGER, INTENT(IN) :: CTRL_IDS(:)
          INTEGER, INTENT(IN) :: NVAL, NUMNOD
          REAL(KIND=WP), INTENT(IN) :: NVALS(:)
          REAL(KIND=WP), INTENT(IN) :: X_COORDS(3,NUMNOD)
          REAL(KIND=WP), INTENT(INOUT) :: X_PAIR(3)

          INTEGER :: K, GID, NEFF
          REAL(KIND=WP) :: WEIGHT, WSUM
          REAL(KIND=WP), PARAMETER :: EPS_WEIGHT = 1.0E-30_WP

          X_PAIR(1:3) = ZERO
          WSUM = ZERO
          NEFF = MIN(NVAL, SIZE(CTRL_IDS), SIZE(NVALS))

          DO K = 1, NEFF
            GID = CTRL_IDS(K)
            IF (GID <= 0 .OR. GID > NUMNOD) CYCLE
            WEIGHT = NVALS(K)
            X_PAIR(1:3) = X_PAIR(1:3) + WEIGHT * X_COORDS(1:3, GID)
            WSUM = WSUM + WEIGHT
          END DO

!         NURBS shape values should sum to one; this only compensates for
!         skipped invalid control ids and keeps the geometric lookup stable.
          IF (ABS(WSUM) > EPS_WEIGHT) THEN
            X_PAIR(1:3) = X_PAIR(1:3) / WSUM
          END IF
        END SUBROUTINE Q1NP_CONTACT_INTERPOLATE_POINT

!=======================================================================
!   Q1NP_CONTACT_NEAREST_SECONDARY_STIFFNESS
!=======================================================================
        REAL(KIND=WP) FUNCTION Q1NP_CONTACT_NEAREST_SECONDARY_STIFFNESS( &
     &      X_PAIR, X_COORDS, NUMNOD, NSV, STFNS, NSN_EFF)
          REAL(KIND=WP), INTENT(IN) :: X_PAIR(3)
          REAL(KIND=WP), INTENT(IN) :: X_COORDS(3,NUMNOD)
          INTEGER, INTENT(IN) :: NUMNOD, NSN_EFF
          INTEGER, INTENT(IN) :: NSV(:)
          REAL(KIND=WP), INTENT(IN) :: STFNS(:)

          INTEGER, PARAMETER :: N_PICK = 4
          REAL(KIND=WP), PARAMETER :: EPS_DIST2 = 1.0E-24_WP
          REAL(KIND=WP), PARAMETER :: EPS_STIFF = 1.0E-30_WP
          INTEGER :: I, J, SLOT, NODE_ID, NEFF
          REAL(KIND=WP) :: DIST2, K_NODE, WEIGHT
          REAL(KIND=WP) :: K_SUM, WEIGHT_SUM
          REAL(KIND=WP) :: BEST_DIST2(N_PICK), BEST_K(N_PICK)

          Q1NP_CONTACT_NEAREST_SECONDARY_STIFFNESS = ZERO
          BEST_DIST2(1:N_PICK) = HUGE(ONE)
          BEST_K(1:N_PICK) = ZERO
          NEFF = MIN(NSN_EFF, SIZE(NSV), SIZE(STFNS))

          DO I = 1, NEFF
            NODE_ID = NSV(I)
            IF (NODE_ID <= 0 .OR. NODE_ID > NUMNOD) CYCLE
            K_NODE = ABS(STFNS(I))
            IF (K_NODE <= EPS_STIFF) CYCLE

            DIST2 = (X_COORDS(1, NODE_ID) - X_PAIR(1))**2 + &
     &              (X_COORDS(2, NODE_ID) - X_PAIR(2))**2 + &
     &              (X_COORDS(3, NODE_ID) - X_PAIR(3))**2
            IF (DIST2 <= EPS_DIST2) THEN
              Q1NP_CONTACT_NEAREST_SECONDARY_STIFFNESS = K_NODE
              RETURN
            END IF

            DO SLOT = 1, N_PICK
              IF (DIST2 < BEST_DIST2(SLOT)) THEN
                DO J = N_PICK, SLOT + 1, -1
                  BEST_DIST2(J) = BEST_DIST2(J - 1)
                  BEST_K(J) = BEST_K(J - 1)
                END DO
                BEST_DIST2(SLOT) = DIST2
                BEST_K(SLOT) = K_NODE
                EXIT
              END IF
            END DO
          END DO

          K_SUM = ZERO
          WEIGHT_SUM = ZERO
          DO I = 1, N_PICK
            IF (BEST_K(I) <= EPS_STIFF) CYCLE
            WEIGHT = ONE / MAX(BEST_DIST2(I), EPS_DIST2)
            K_SUM = K_SUM + WEIGHT * BEST_K(I)
            WEIGHT_SUM = WEIGHT_SUM + WEIGHT
          END DO

          IF (WEIGHT_SUM > EPS_STIFF) THEN
            Q1NP_CONTACT_NEAREST_SECONDARY_STIFFNESS = K_SUM / WEIGHT_SUM
          END IF
        END FUNCTION Q1NP_CONTACT_NEAREST_SECONDARY_STIFFNESS

!=======================================================================
!   Q1NP_CONTACT_NEAREST_PRIMARY_STIFFNESS
!=======================================================================
        REAL(KIND=WP) FUNCTION Q1NP_CONTACT_NEAREST_PRIMARY_STIFFNESS( &
     &      X_PAIR, IRECTM, STFM, NRTM_EFF, X_COORDS, NUMNOD)
          REAL(KIND=WP), INTENT(IN) :: X_PAIR(3)
          INTEGER, INTENT(IN) :: IRECTM(:)
          REAL(KIND=WP), INTENT(IN) :: STFM(:)
          INTEGER, INTENT(IN) :: NRTM_EFF, NUMNOD
          REAL(KIND=WP), INTENT(IN) :: X_COORDS(3,NUMNOD)

          INTEGER :: SEG, BEST_SEG, NEFF
          REAL(KIND=WP) :: DIST2, BEST_DIST2
          REAL(KIND=WP), PARAMETER :: EPS_STIFF = 1.0E-30_WP

          Q1NP_CONTACT_NEAREST_PRIMARY_STIFFNESS = ZERO
          BEST_SEG = 0
          BEST_DIST2 = HUGE(ONE)
          NEFF = MIN(NRTM_EFF, SIZE(STFM), SIZE(IRECTM) / 4)

          DO SEG = 1, NEFF
            IF (ABS(STFM(SEG)) <= EPS_STIFF) CYCLE
            DIST2 = Q1NP_CONTACT_PRIMARY_SEGMENT_DISTANCE2( &
     &        X_PAIR, IRECTM, SEG, X_COORDS, NUMNOD)
            IF (DIST2 < BEST_DIST2) THEN
              BEST_DIST2 = DIST2
              BEST_SEG = SEG
            END IF
          END DO

          IF (BEST_SEG > 0) THEN
            Q1NP_CONTACT_NEAREST_PRIMARY_STIFFNESS = ABS(STFM(BEST_SEG))
          END IF
        END FUNCTION Q1NP_CONTACT_NEAREST_PRIMARY_STIFFNESS

!=======================================================================
!   Q1NP_CONTACT_PRIMARY_SEGMENT_DISTANCE2
!=======================================================================
        REAL(KIND=WP) FUNCTION Q1NP_CONTACT_PRIMARY_SEGMENT_DISTANCE2( &
     &      X_PAIR, IRECTM, SEG, X_COORDS, NUMNOD)
          REAL(KIND=WP), INTENT(IN) :: X_PAIR(3)
          INTEGER, INTENT(IN) :: IRECTM(:)
          INTEGER, INTENT(IN) :: SEG, NUMNOD
          REAL(KIND=WP), INTENT(IN) :: X_COORDS(3,NUMNOD)

          INTEGER :: BASE, IXI, IETA, K, NODE_ID
          REAL(KIND=WP) :: XI, ETA, DIST2
          REAL(KIND=WP) :: WEIGHT(4), X_SEG(3)

          Q1NP_CONTACT_PRIMARY_SEGMENT_DISTANCE2 = HUGE(ONE)
          BASE = 4 * (SEG - 1)
          IF (SEG <= 0 .OR. BASE + 4 > SIZE(IRECTM)) RETURN

          DO K = 1, 4
            NODE_ID = IRECTM(BASE + K)
            IF (NODE_ID <= 0 .OR. NODE_ID > NUMNOD) RETURN
          END DO

!         A small bilinear sample avoids choosing a neighboring large segment
!         only by center distance while keeping the lookup deterministic.
          DO IETA = 0, 2
            ETA = -ONE + REAL(IETA, WP)
            DO IXI = 0, 2
              XI = -ONE + REAL(IXI, WP)
              CALL Q1NP_CONTACT_BILINEAR_WEIGHTS(XI, ETA, WEIGHT)

              X_SEG(1:3) = ZERO
              DO K = 1, 4
                NODE_ID = IRECTM(BASE + K)
                X_SEG(1:3) = X_SEG(1:3) + WEIGHT(K) * X_COORDS(1:3, NODE_ID)
              END DO

              DIST2 = (X_SEG(1) - X_PAIR(1))**2 + &
     &                (X_SEG(2) - X_PAIR(2))**2 + &
     &                (X_SEG(3) - X_PAIR(3))**2
              Q1NP_CONTACT_PRIMARY_SEGMENT_DISTANCE2 = &
     &          MIN(Q1NP_CONTACT_PRIMARY_SEGMENT_DISTANCE2, DIST2)
            END DO
          END DO
        END FUNCTION Q1NP_CONTACT_PRIMARY_SEGMENT_DISTANCE2

!=======================================================================
!   Q1NP_CONTACT_ASSIGN_PAIR_STIFFNESS
!   Assign the pair stiffness based on the primary and secondary side stiffnesses.
!=======================================================================
!||====================================================================
!||    q1np_contact_assign_pair_stiffness    ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_compute_penalty_forces   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- calls      -----------------------------------------------------
!||    q1np_contact_knot_set_id              ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||====================================================================
        SUBROUTINE Q1NP_CONTACT_ASSIGN_PAIR_STIFFNESS( &
     &      ELEM_A, K_A_PRIMARY, K_A_SECONDARY, &
     &      ELEM_B, K_B_PRIMARY, K_B_SECONDARY, &
     &      KQ1NP_TAB, K_PRIMARY, K_SECONDARY)
          INTEGER, INTENT(IN) :: ELEM_A, ELEM_B
          INTEGER, INTENT(IN) :: KQ1NP_TAB(:,:)
          REAL(KIND=WP), INTENT(IN) :: K_A_PRIMARY, K_A_SECONDARY
          REAL(KIND=WP), INTENT(IN) :: K_B_PRIMARY, K_B_SECONDARY
          REAL(KIND=WP), INTENT(INOUT) :: K_PRIMARY, K_SECONDARY

          INTEGER :: KNOT_A, KNOT_B
          REAL(KIND=WP), PARAMETER :: EPS_STIFF = 1.0E-30_WP

          K_PRIMARY = ZERO
          K_SECONDARY = ZERO
          KNOT_A = Q1NP_CONTACT_KNOT_SET_ID(ELEM_A, KQ1NP_TAB)
          KNOT_B = Q1NP_CONTACT_KNOT_SET_ID(ELEM_B, KQ1NP_TAB)

!         Prefer the historical prototype mapping when both data maps are valid.
          IF (KNOT_A == 1 .AND. KNOT_B == 2 .AND. &
     &        K_A_SECONDARY > EPS_STIFF .AND. K_B_PRIMARY > EPS_STIFF) THEN
            K_SECONDARY = K_A_SECONDARY
            K_PRIMARY = K_B_PRIMARY
          ELSEIF (KNOT_A == 2 .AND. KNOT_B == 1 .AND. &
     &            K_A_PRIMARY > EPS_STIFF .AND. K_B_SECONDARY > EPS_STIFF) THEN
            K_PRIMARY = K_A_PRIMARY
            K_SECONDARY = K_B_SECONDARY
          ELSEIF (K_A_SECONDARY > EPS_STIFF .AND. K_B_PRIMARY > EPS_STIFF) THEN
            K_SECONDARY = K_A_SECONDARY
            K_PRIMARY = K_B_PRIMARY
          ELSEIF (K_A_PRIMARY > EPS_STIFF .AND. K_B_SECONDARY > EPS_STIFF) THEN
            K_PRIMARY = K_A_PRIMARY
            K_SECONDARY = K_B_SECONDARY
          END IF
        END SUBROUTINE Q1NP_CONTACT_ASSIGN_PAIR_STIFFNESS

!=======================================================================
!   Q1NP_CONTACT_FIND_PRIMARY_SEGMENT
!=======================================================================
!||====================================================================
!||    q1np_contact_find_primary_segment    ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_build_primary_seg_map   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- calls      -----------------------------------------------------
!||    q1np_contact_bulk_nodes              ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_segment_matches         ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||====================================================================
        INTEGER FUNCTION Q1NP_CONTACT_FIND_PRIMARY_SEGMENT( &
     &      ELEM_IDX, KQ1NP_TAB, IQ1NP_BULK_TAB, IRECTM, NRTM_EFF, SEG_GUESS)
          INTEGER, INTENT(IN) :: ELEM_IDX, NRTM_EFF, SEG_GUESS
          INTEGER, INTENT(IN) :: KQ1NP_TAB(:,:)
          INTEGER, INTENT(IN) :: IQ1NP_BULK_TAB(:)
          INTEGER, INTENT(IN) :: IRECTM(:)

          INTEGER :: SEG
          INTEGER :: NODE_IDS(4)
          LOGICAL :: HAS_NODES

          Q1NP_CONTACT_FIND_PRIMARY_SEGMENT = 0
          CALL Q1NP_CONTACT_BULK_NODES( &
     &      ELEM_IDX, KQ1NP_TAB, IQ1NP_BULK_TAB, NODE_IDS, HAS_NODES)
          IF (.NOT. HAS_NODES) RETURN

          IF (SEG_GUESS > 0 .AND. SEG_GUESS <= NRTM_EFF) THEN
            IF (Q1NP_CONTACT_SEGMENT_MATCHES(NODE_IDS, IRECTM, SEG_GUESS)) THEN
              Q1NP_CONTACT_FIND_PRIMARY_SEGMENT = SEG_GUESS
              RETURN
            END IF
          END IF

          DO SEG = 1, NRTM_EFF
            IF (SEG == SEG_GUESS) CYCLE
            IF (Q1NP_CONTACT_SEGMENT_MATCHES(NODE_IDS, IRECTM, SEG)) THEN
              Q1NP_CONTACT_FIND_PRIMARY_SEGMENT = SEG
              RETURN
            END IF
          END DO
        END FUNCTION Q1NP_CONTACT_FIND_PRIMARY_SEGMENT

!=======================================================================
!   Q1NP_CONTACT_SEGMENT_MATCHES
!=======================================================================
!||====================================================================
!||    q1np_contact_segment_matches        ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_find_primary_segment   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||====================================================================
        LOGICAL FUNCTION Q1NP_CONTACT_SEGMENT_MATCHES(NODE_IDS, IRECTM, SEG)
          INTEGER, INTENT(IN) :: NODE_IDS(4)
          INTEGER, INTENT(IN) :: IRECTM(:)
          INTEGER, INTENT(IN) :: SEG

          INTEGER :: I, J, BASE
          LOGICAL :: FOUND

          Q1NP_CONTACT_SEGMENT_MATCHES = .FALSE.
          BASE = 4 * (SEG - 1)
          IF (SEG <= 0 .OR. BASE + 4 > SIZE(IRECTM)) RETURN

          DO I = 1, 4
            IF (NODE_IDS(I) <= 0) RETURN
            FOUND = .FALSE.
            DO J = 1, 4
              IF (NODE_IDS(I) == IRECTM(BASE + J)) THEN
                FOUND = .TRUE.
                EXIT
              END IF
            END DO
            IF (.NOT. FOUND) RETURN
          END DO
          Q1NP_CONTACT_SEGMENT_MATCHES = .TRUE.
        END FUNCTION Q1NP_CONTACT_SEGMENT_MATCHES

!=======================================================================
!   Q1NP_CONTACT_BULK_NODES
!=======================================================================
!||====================================================================
!||    q1np_contact_bulk_nodes              ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_build_primary_seg_map   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_find_primary_segment    ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||====================================================================
        SUBROUTINE Q1NP_CONTACT_BULK_NODES( &
     &      ELEM_IDX, KQ1NP_TAB, IQ1NP_BULK_TAB, NODE_IDS, HAS_NODES)
          INTEGER, INTENT(IN) :: ELEM_IDX
          INTEGER, INTENT(IN) :: KQ1NP_TAB(:,:)
          INTEGER, INTENT(IN) :: IQ1NP_BULK_TAB(:)
          INTEGER, INTENT(INOUT) :: NODE_IDS(4)
          LOGICAL, INTENT(INOUT) :: HAS_NODES

          INTEGER :: OFF_BULK

          NODE_IDS(:) = 0
          HAS_NODES = .FALSE.
          IF (ELEM_IDX <= 0 .OR. ELEM_IDX > SIZE(KQ1NP_TAB, 2)) RETURN

          OFF_BULK = KQ1NP_TAB(KQ1NP_BULK_OFF, ELEM_IDX)
          IF (OFF_BULK <= 0 .OR. OFF_BULK + 3 > SIZE(IQ1NP_BULK_TAB)) RETURN

          NODE_IDS(1:4) = IQ1NP_BULK_TAB(OFF_BULK:OFF_BULK+3)
          HAS_NODES = ALL(NODE_IDS(1:4) > 0)
        END SUBROUTINE Q1NP_CONTACT_BULK_NODES

!=======================================================================
!   Q1NP_CONTACT_BILINEAR_WEIGHTS
!=======================================================================
!||====================================================================
!||    q1np_contact_bilinear_weights         ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_build_primary_seg_map    ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_compute_penalty_forces   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_interpolate_point        ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||====================================================================
        SUBROUTINE Q1NP_CONTACT_BILINEAR_WEIGHTS(XI, ETA, WEIGHT)
          REAL(KIND=WP), INTENT(IN) :: XI, ETA
          REAL(KIND=WP), INTENT(INOUT) :: WEIGHT(4)

          REAL(KIND=WP) :: XI_CLAMP, ETA_CLAMP, WSUM

          XI_CLAMP = MAX(-ONE, MIN(ONE, XI))
          ETA_CLAMP = MAX(-ONE, MIN(ONE, ETA))

          WEIGHT(1) = 0.25_WP * (ONE - XI_CLAMP) * (ONE - ETA_CLAMP)
          WEIGHT(2) = 0.25_WP * (ONE + XI_CLAMP) * (ONE - ETA_CLAMP)
          WEIGHT(3) = 0.25_WP * (ONE + XI_CLAMP) * (ONE + ETA_CLAMP)
          WEIGHT(4) = 0.25_WP * (ONE - XI_CLAMP) * (ONE + ETA_CLAMP)
          WEIGHT(1:4) = MAX(ZERO, WEIGHT(1:4))

          WSUM = SUM(WEIGHT(1:4))
          IF (WSUM > ZERO) THEN
            WEIGHT(1:4) = WEIGHT(1:4) / WSUM
          END IF
        END SUBROUTINE Q1NP_CONTACT_BILINEAR_WEIGHTS

!=======================================================================
!   Q1NP_GAUSS
!
!   Return the Gauss-Legendre abscissa for sample point IGP out of NGP.
!=======================================================================
        REAL(KIND=WP) FUNCTION Q1NP_GAUSS(IGP, NGP)
          INTEGER, INTENT(IN) :: IGP, NGP

          SELECT CASE (NGP)
          CASE (1)
            Q1NP_GAUSS = ZERO
          CASE (2)
            Q1NP_GAUSS = MERGE( &
     &        -ONE / SQRT(THREE), ONE / SQRT(THREE), IGP == 1)
          CASE (3)
            IF (IGP == 1) THEN
              Q1NP_GAUSS = -SQRT(3.0_WP / 5.0_WP)
            ELSEIF (IGP == 2) THEN
              Q1NP_GAUSS = ZERO
            ELSE
              Q1NP_GAUSS =  SQRT(3.0_WP / 5.0_WP)
            END IF
          CASE DEFAULT
            Q1NP_GAUSS = -ONE + TWO * &
     &        (REAL(IGP, WP) - 0.5_WP) / REAL(NGP, WP)
          END SELECT
        END FUNCTION Q1NP_GAUSS

!=======================================================================
!   Q1NP_CONTACT_KNOT_SET_ID
!=======================================================================
!||====================================================================
!||    q1np_contact_knot_set_id             ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_assign_pair_stiffness   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_build_primary_seg_map   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||====================================================================
        INTEGER FUNCTION Q1NP_CONTACT_KNOT_SET_ID(ELEM_IDX, KQ1NP_TAB)
          INTEGER, INTENT(IN) :: ELEM_IDX
          INTEGER, INTENT(IN) :: KQ1NP_TAB(:,:)

          Q1NP_CONTACT_KNOT_SET_ID = 0
          IF (ELEM_IDX <= 0 .OR. ELEM_IDX > SIZE(KQ1NP_TAB, 2)) RETURN
          IF (SIZE(KQ1NP_TAB, 1) < KQ1NP_KNOT_SET) RETURN
          Q1NP_CONTACT_KNOT_SET_ID = KQ1NP_TAB(KQ1NP_KNOT_SET, ELEM_IDX)
        END FUNCTION Q1NP_CONTACT_KNOT_SET_ID

!=======================================================================
!   Q1NP_CONTACT_STIFFNESS
!=======================================================================
        REAL(KIND=WP) FUNCTION Q1NP_CONTACT_STIFFNESS( &
     &      K_PRIMARY, K_SECONDARY, IGSTI, KMIN, KMAX)
          REAL(KIND=WP), INTENT(IN) :: K_PRIMARY, K_SECONDARY
          REAL(KIND=WP), INTENT(IN) :: KMIN, KMAX
          INTEGER, INTENT(IN) :: IGSTI
          REAL(KIND=WP), PARAMETER :: EPS_DEN = 1.0E-30_WP

          SELECT CASE (IGSTI)
          CASE (:1)
            Q1NP_CONTACT_STIFFNESS = K_PRIMARY * ABS(K_SECONDARY)
          CASE (2)
            Q1NP_CONTACT_STIFFNESS = MAX(KMIN, MIN( &
     &        0.5_WP * (K_PRIMARY + ABS(K_SECONDARY)), KMAX))
          CASE (3)
            Q1NP_CONTACT_STIFFNESS = MAX(KMIN, MIN( &
     &        MAX(K_PRIMARY, ABS(K_SECONDARY)), KMAX))
          CASE (4)
            Q1NP_CONTACT_STIFFNESS = MAX(KMIN, MIN( &
     &        MIN(K_PRIMARY, ABS(K_SECONDARY)), KMAX))
          CASE (5)
            Q1NP_CONTACT_STIFFNESS = MAX(KMIN, MIN( &
     &        K_PRIMARY * ABS(K_SECONDARY) / &
     &        MAX(EPS_DEN, K_PRIMARY + ABS(K_SECONDARY)), KMAX))
          CASE DEFAULT
            Q1NP_CONTACT_STIFFNESS = K_PRIMARY * ABS(K_SECONDARY)
          END SELECT
        END FUNCTION Q1NP_CONTACT_STIFFNESS

!=======================================================================
!   Q1NP_CONTACT_EXTRACT_ELEM_DATA
!
!   Helper: extract element metadata, control-point IDs, and knot
!   vectors for a given Q1NP element index.  The caller provides
!   pre-allocated workspace arrays U_KNOT_OUT / V_KNOT_OUT that
!   must be large enough (use Q1NP_CONTACT_MAX_KNOT_LEN to size).
!   U_LEN_OUT / V_LEN_OUT return the number of entries written.
!=======================================================================
!||====================================================================
!||    q1np_contact_extract_elem_data         ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_bp_build_surface_points   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_evaluate_side             ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_project_point_newton      ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||====================================================================
        SUBROUTINE Q1NP_CONTACT_EXTRACT_ELEM_DATA( &
     &      ELEM_IDX, KQ1NP_TAB, IQ1NP_TAB, Q1NP_KTAB, &
     &      P_OUT, Q_OUT, NCTRL_OUT, &
     &      ELEM_U_IDX_OUT, ELEM_V_IDX_OUT, &
     &      CTRL_IDS_OUT, U_KNOT_OUT, V_KNOT_OUT, &
     &      U_LEN_OUT, V_LEN_OUT)
          INTEGER, INTENT(IN) :: ELEM_IDX
          INTEGER, INTENT(IN) :: KQ1NP_TAB(:,:)
          INTEGER, INTENT(IN) :: IQ1NP_TAB(:)
          REAL(KIND=WP), INTENT(IN) :: Q1NP_KTAB(:)
          INTEGER, INTENT(INOUT) :: P_OUT, Q_OUT, NCTRL_OUT
          INTEGER, INTENT(INOUT) :: ELEM_U_IDX_OUT, ELEM_V_IDX_OUT
          INTEGER, PARAMETER :: MAX_CTRL = 50
          INTEGER, INTENT(INOUT) :: CTRL_IDS_OUT(MAX_CTRL)
          REAL(KIND=WP), INTENT(INOUT) :: U_KNOT_OUT(:)
          REAL(KIND=WP), INTENT(INOUT) :: V_KNOT_OUT(:)
          INTEGER, INTENT(INOUT) :: U_LEN_OUT, V_LEN_OUT

          INTEGER :: CP_OFFSET, KNOT_SET_ID
          INTEGER :: NX_CUR, NY_CUR, IPT

          P_OUT          = KQ1NP_TAB(KQ1NP_P, ELEM_IDX)
          Q_OUT          = KQ1NP_TAB(KQ1NP_Q, ELEM_IDX)
          NCTRL_OUT      = KQ1NP_TAB(KQ1NP_NCTRL, ELEM_IDX)
          CP_OFFSET      = KQ1NP_TAB(KQ1NP_CP_OFF, ELEM_IDX)
          ELEM_U_IDX_OUT = KQ1NP_TAB(KQ1NP_ELEM_U, ELEM_IDX)
          ELEM_V_IDX_OUT = KQ1NP_TAB(KQ1NP_ELEM_V, ELEM_IDX)
          KNOT_SET_ID    = KQ1NP_TAB(KQ1NP_KNOT_SET, ELEM_IDX)

          NX_CUR = KQ1NP_TAB(KQ1NP_NX, ELEM_IDX)
          NY_CUR = KQ1NP_TAB(KQ1NP_NY, ELEM_IDX)
          IF (NX_CUR <= 0 .OR. NY_CUR <= 0) THEN
            IF (Q1NP_NKNOT_SETS_G > 0 .AND. &
     &          KNOT_SET_ID > 0 .AND. &
     &          KNOT_SET_ID <= Q1NP_NKNOT_SETS_G) THEN
              NX_CUR = Q1NP_NX_SET_G(KNOT_SET_ID)
              NY_CUR = Q1NP_NY_SET_G(KNOT_SET_ID)
            ELSE
              NX_CUR = Q1NP_NX_G
              NY_CUR = Q1NP_NY_G
            END IF
          END IF

          U_LEN_OUT = NX_CUR + 2*P_OUT + 1
          V_LEN_OUT = NY_CUR + 2*Q_OUT + 1

          IF (Q1NP_NKNOT_SETS_G > 0 .AND. &
     &        KNOT_SET_ID > 0 .AND. &
     &        KNOT_SET_ID <= Q1NP_NKNOT_SETS_G .AND. &
     &        ALLOCATED(Q1NP_KTAB_OFF_G)) THEN
            U_KNOT_OUT(1:U_LEN_OUT) = &
     &          Q1NP_KTAB(Q1NP_KTAB_OFF_G(KNOT_SET_ID) : &
     &                    Q1NP_KTAB_OFF_G(KNOT_SET_ID) + U_LEN_OUT - 1)
            V_KNOT_OUT(1:V_LEN_OUT) = &
     &          Q1NP_KTAB(Q1NP_KTAB_OFF_G(KNOT_SET_ID) + U_LEN_OUT : &
     &                    Q1NP_KTAB_OFF_G(KNOT_SET_ID) + U_LEN_OUT + V_LEN_OUT - 1)
          ELSE
            U_KNOT_OUT(1:U_LEN_OUT) = Q1NP_KTAB(1:U_LEN_OUT)
            V_KNOT_OUT(1:V_LEN_OUT) = &
     &          Q1NP_KTAB(U_LEN_OUT+1:U_LEN_OUT+V_LEN_OUT)
          END IF

          DO IPT = 1, MIN(NCTRL_OUT, MAX_CTRL)
            CTRL_IDS_OUT(IPT) = IQ1NP_TAB(CP_OFFSET + IPT - 1)
          END DO

        END SUBROUTINE Q1NP_CONTACT_EXTRACT_ELEM_DATA

!=======================================================================
!   Q1NP_CONTACT_MAX_KNOT_LEN
!
!   Scan KQ1NP_TAB to find the maximum knot vector length in U and V
!   across all elements. Used to size pre-allocated workspace arrays.
!=======================================================================
!||====================================================================
!||    q1np_contact_max_knot_len              ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_bp_build_surface_points   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_compute_penalty_forces    ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_narrow_phase_project      ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||====================================================================
        SUBROUTINE Q1NP_CONTACT_MAX_KNOT_LEN( &
     &      KQ1NP_TAB, NUMELQ1NP, U_MAX_OUT, V_MAX_OUT)
          INTEGER, INTENT(IN) :: KQ1NP_TAB(:,:)
          INTEGER, INTENT(IN) :: NUMELQ1NP
          INTEGER, INTENT(INOUT) :: U_MAX_OUT, V_MAX_OUT

          INTEGER :: IEL, P_CUR, Q_CUR, NX_CUR, NY_CUR
          INTEGER :: KNOT_SET_ID, U_LEN, V_LEN

          U_MAX_OUT = 1
          V_MAX_OUT = 1

          DO IEL = 1, NUMELQ1NP
            P_CUR       = KQ1NP_TAB(KQ1NP_P, IEL)
            Q_CUR       = KQ1NP_TAB(KQ1NP_Q, IEL)
            KNOT_SET_ID = KQ1NP_TAB(KQ1NP_KNOT_SET, IEL)
            NX_CUR      = KQ1NP_TAB(KQ1NP_NX, IEL)
            NY_CUR      = KQ1NP_TAB(KQ1NP_NY, IEL)

            IF (NX_CUR <= 0 .OR. NY_CUR <= 0) THEN
              IF (Q1NP_NKNOT_SETS_G > 0 .AND. &
     &            KNOT_SET_ID > 0 .AND. &
     &            KNOT_SET_ID <= Q1NP_NKNOT_SETS_G) THEN
                NX_CUR = Q1NP_NX_SET_G(KNOT_SET_ID)
                NY_CUR = Q1NP_NY_SET_G(KNOT_SET_ID)
              ELSE
                NX_CUR = Q1NP_NX_G
                NY_CUR = Q1NP_NY_G
              END IF
            END IF

            U_LEN = NX_CUR + 2*P_CUR + 1
            V_LEN = NY_CUR + 2*Q_CUR + 1
            IF (U_LEN > U_MAX_OUT) U_MAX_OUT = U_LEN
            IF (V_LEN > V_MAX_OUT) V_MAX_OUT = V_LEN
          END DO
        END SUBROUTINE Q1NP_CONTACT_MAX_KNOT_LEN

!=======================================================================
!   Q1NP_CONTACT_PROJECT_POINT_NEWTON
!
!   Project a source point X_SRC onto the NURBS top surface of a
!   specific Q1NP element using Newton iteration on the orthogonality
!   conditions:  f1 = (S - X_SRC) . dS/dXI  = 0
!                f2 = (S - X_SRC) . dS/dETA = 0
!
!   Returns parametric coordinates, projected point, distance,
!   signed penetration, unit normal, residual, and validity flag.
!=======================================================================
!||====================================================================
!||    q1np_contact_project_point_newton                  ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_narrow_phase_project                  ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- calls      -----------------------------------------------------
!||    q1np_contact_extract_elem_data                     ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_evaluate_nurbs_top_surface_point_and_derivs   ../engine/source/elements/solid/solid_q1np/q1np_nurbs_surface_eval_mod.F90
!||====================================================================
        SUBROUTINE Q1NP_CONTACT_PROJECT_POINT_NEWTON( &
     &      X_SRC, KQ1NP_TAB, IQ1NP_TAB, Q1NP_KTAB, &
     &      X_COORDS, NUMNOD, ELEM_IDX, &
     &      XI_SEED, ETA_SEED, &
     &      XI_OUT, ETA_OUT, XYZ_PROJ, PROJ_DIST, &
     &      SIGNED_PENETRATION_OUT, NORMAL_OUT, &
     &      RESIDUAL_OUT, N_ITER_OUT, VALID, &
     &      U_KNOT_WS, V_KNOT_WS)
          REAL(KIND=WP), INTENT(IN)  :: X_SRC(3)
          INTEGER, INTENT(IN) :: KQ1NP_TAB(:,:)
          INTEGER, INTENT(IN) :: IQ1NP_TAB(:)
          REAL(KIND=WP), INTENT(IN) :: Q1NP_KTAB(:)
          REAL(KIND=WP), INTENT(IN) :: X_COORDS(3,NUMNOD)
          INTEGER, INTENT(IN) :: NUMNOD, ELEM_IDX
          REAL(KIND=WP), INTENT(IN)  :: XI_SEED, ETA_SEED
          REAL(KIND=WP), INTENT(INOUT) :: XI_OUT, ETA_OUT
          REAL(KIND=WP), INTENT(INOUT) :: XYZ_PROJ(3), PROJ_DIST
          REAL(KIND=WP), INTENT(INOUT) :: SIGNED_PENETRATION_OUT
          REAL(KIND=WP), INTENT(INOUT) :: NORMAL_OUT(3)
          REAL(KIND=WP), INTENT(INOUT) :: RESIDUAL_OUT
          INTEGER, INTENT(INOUT) :: N_ITER_OUT
          LOGICAL, INTENT(INOUT) :: VALID
          REAL(KIND=WP), INTENT(INOUT) :: U_KNOT_WS(:), V_KNOT_WS(:)

          INTEGER, PARAMETER :: MAX_ITER = 5
          REAL(KIND=WP), PARAMETER :: NEWTON_TOL = 1.0E-10_WP
          INTEGER, PARAMETER :: MAX_CTRL = 50

          INTEGER :: ITER, U_LEN, V_LEN
          INTEGER :: P_CUR, Q_CUR, NCTRL
          INTEGER :: ELEM_U_IDX, ELEM_V_IDX
          INTEGER :: CTRL_IDS(MAX_CTRL)
          REAL(KIND=WP) :: XI, ETA
          REAL(KIND=WP) :: S(3), SU(3), SV(3), DIFF(3)
          REAL(KIND=WP) :: NORMAL(3), NORM_N
          REAL(KIND=WP) :: F1, F2, RES_NORM
          REAL(KIND=WP) :: A11, A12, A21, A22, DET_J
          REAL(KIND=WP) :: D_XI, D_ETA

          VALID = .FALSE.

          CALL Q1NP_CONTACT_EXTRACT_ELEM_DATA( &
     &      ELEM_IDX, KQ1NP_TAB, IQ1NP_TAB, Q1NP_KTAB, &
     &      P_CUR, Q_CUR, NCTRL, ELEM_U_IDX, ELEM_V_IDX, &
     &      CTRL_IDS, U_KNOT_WS, V_KNOT_WS, U_LEN, V_LEN)

          XI  = XI_SEED
          ETA = ETA_SEED

          DO ITER = 1, MAX_ITER
            CALL Q1NP_EVALUATE_NURBS_TOP_SURFACE_POINT_AND_DERIVS( &
     &          XI, ETA, P_CUR, Q_CUR, &
     &          ELEM_U_IDX, ELEM_V_IDX, &
     &          U_KNOT_WS(1:U_LEN), V_KNOT_WS(1:V_LEN), &
     &          MIN(NCTRL, MAX_CTRL), CTRL_IDS, &
     &          X_COORDS, NUMNOD, S, SU, SV)

            DIFF(1:3) = S(1:3) - X_SRC(1:3)

!           Orthogonality residuals
            F1 = DIFF(1)*SU(1) + DIFF(2)*SU(2) + DIFF(3)*SU(3)
            F2 = DIFF(1)*SV(1) + DIFF(2)*SV(2) + DIFF(3)*SV(3)

            RES_NORM = SQRT(F1*F1 + F2*F2)
            IF (RES_NORM < NEWTON_TOL) EXIT

!           Jacobian of the residual system (first-order approximation):
!             A11 = Su . Su,  A12 = Su . Sv
!             A21 = Sv . Su,  A22 = Sv . Sv
            A11 = SU(1)*SU(1) + SU(2)*SU(2) + SU(3)*SU(3)
            A12 = SU(1)*SV(1) + SU(2)*SV(2) + SU(3)*SV(3)
            A21 = A12
            A22 = SV(1)*SV(1) + SV(2)*SV(2) + SV(3)*SV(3)

            DET_J = A11*A22 - A12*A21
            IF (ABS(DET_J) < 1.0E-30_WP) EXIT

!           Solve 2x2 system: [A] * [d_xi, d_eta]^T = -[f1, f2]^T
            D_XI  = -(A22*F1 - A12*F2) / DET_J
            D_ETA = -(A11*F2 - A21*F1) / DET_J

            XI  = XI  + D_XI
            ETA = ETA + D_ETA

!           Clamp to parent domain [-1, +1]
            XI  = MAX(-ONE, MIN(ONE, XI))
            ETA = MAX(-ONE, MIN(ONE, ETA))
          END DO

!         Final evaluation at converged point
          CALL Q1NP_EVALUATE_NURBS_TOP_SURFACE_POINT_AND_DERIVS( &
     &        XI, ETA, P_CUR, Q_CUR, &
     &        ELEM_U_IDX, ELEM_V_IDX, &
     &        U_KNOT_WS(1:U_LEN), V_KNOT_WS(1:V_LEN), &
     &        MIN(NCTRL, MAX_CTRL), CTRL_IDS, &
     &        X_COORDS, NUMNOD, S, SU, SV)

          DIFF(1:3) = S(1:3) - X_SRC(1:3)
          F1 = DIFF(1)*SU(1) + DIFF(2)*SU(2) + DIFF(3)*SU(3)
          F2 = DIFF(1)*SV(1) + DIFF(2)*SV(2) + DIFF(3)*SV(3)
          RES_NORM = SQRT(F1*F1 + F2*F2)

          XI_OUT   = XI
          ETA_OUT  = ETA
          XYZ_PROJ = S
          PROJ_DIST = SQRT(DIFF(1)**2 + DIFF(2)**2 + DIFF(3)**2)
          NORMAL(1) = SU(2)*SV(3) - SU(3)*SV(2)
          NORMAL(2) = SU(3)*SV(1) - SU(1)*SV(3)
          NORMAL(3) = SU(1)*SV(2) - SU(2)*SV(1)
          NORM_N = SQRT(NORMAL(1)**2 + NORMAL(2)**2 + NORMAL(3)**2)
          IF (NORM_N > 1.0E-30_WP) THEN
            NORMAL(1:3) = NORMAL(1:3) / NORM_N
            SIGNED_PENETRATION_OUT = (X_SRC(1)-S(1))*NORMAL(1) + &
     &                               (X_SRC(2)-S(2))*NORMAL(2) + &
     &                               (X_SRC(3)-S(3))*NORMAL(3)
          ELSE
            SIGNED_PENETRATION_OUT = ZERO
          END IF
          NORMAL_OUT(1:3) = NORMAL(1:3)
          RESIDUAL_OUT = RES_NORM
          N_ITER_OUT   = ITER

!         Converged = residual small AND parametric coords inside domain
          VALID = (RES_NORM < NEWTON_TOL * 1.0E3_WP) .AND. &
     &            (ABS(XI) <= ONE) .AND. (ABS(ETA) <= ONE)

        END SUBROUTINE Q1NP_CONTACT_PROJECT_POINT_NEWTON

!=======================================================================
!   Q1NP_CONTACT_INIT_GRID_NODES
!   Build Q1NP_FCONT_GRID_IDS(4, NUMELQ1NP_G) from IXS once.
!   For each Q1NP element, finds the HEX8 top-face nodes (the ones
!   visible in H3D) by identifying which HEX8 face matches the 4 bulk
!   nodes and taking the opposite face.
!=======================================================================
!||====================================================================
!||    q1np_contact_init_grid_nodes   ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- called by ------------------------------------------------------
!||    ists_mainf                     ../engine/source/interfaces/ists/ists_mainf.F90
!||--- calls      -----------------------------------------------------
!||====================================================================
        SUBROUTINE Q1NP_CONTACT_INIT_GRID_NODES(IXS, NIXS_IN, NUMELS_IN)
          INTEGER, INTENT(IN) :: NIXS_IN, NUMELS_IN
          INTEGER, INTENT(IN) :: IXS(NIXS_IN, NUMELS_IN)

          INTEGER :: IEL, IEL_HEX8, IFACE, K, J2
          INTEGER :: HEX_NODES(8), BULK_IDS(4), OFF_BULK
          INTEGER, PARAMETER :: FACE_LOC(4,6) = RESHAPE( (/ &
        &      1,2,3,4, 5,6,7,8, 1,2,6,5, 2,3,7,6, &
        &      3,4,8,7, 4,1,5,8 /), (/ 4,6 /) )
          INTEGER, PARAMETER :: OPP(6) = (/ 2,1,5,6,3,4 /)
          LOGICAL :: MATCH

          IF (Q1NP_FCONT_GRID_READY) RETURN
          IF (NUMELQ1NP_G <= 0) RETURN
          IF (.NOT. ALLOCATED(KQ1NP_TAB)) RETURN
          IF (.NOT. ALLOCATED(IQ1NP_BULK_TAB)) RETURN

          IF (ALLOCATED(Q1NP_FCONT_GRID_IDS)) CALL MY_DEALLOC(Q1NP_FCONT_GRID_IDS)
          CALL MY_ALLOC(Q1NP_FCONT_GRID_IDS, 4, NUMELQ1NP_G, "Q1NP_FCONT_GRID_IDS")
          Q1NP_FCONT_GRID_IDS(:,:) = 0

          DO IEL = 1, NUMELQ1NP_G
            IEL_HEX8 = KQ1NP_TAB(KQ1NP_IXS_IDX, IEL)
            IF (IEL_HEX8 <= 0 .OR. IEL_HEX8 > NUMELS_IN) CYCLE

            HEX_NODES(1:8) = IXS(2:9, IEL_HEX8)

            OFF_BULK = KQ1NP_TAB(KQ1NP_BULK_OFF, IEL)
            IF (OFF_BULK <= 0 .OR. OFF_BULK + 3 > SIZE(IQ1NP_BULK_TAB)) CYCLE
            BULK_IDS(1:4) = IQ1NP_BULK_TAB(OFF_BULK:OFF_BULK+3)

            DO IFACE = 1, 6
              MATCH = .TRUE.
              DO K = 1, 4
                IF (.NOT. ANY(BULK_IDS(1:4) == &
     &              HEX_NODES(FACE_LOC(K, IFACE)))) THEN
                  MATCH = .FALSE.
                  EXIT
                END IF
              END DO
              IF (MATCH) THEN
                J2 = OPP(IFACE)
                Q1NP_FCONT_GRID_IDS(1:4, IEL) = &
     &            HEX_NODES(FACE_LOC(1:4, J2))
                EXIT
              END IF
            END DO
          END DO
          Q1NP_FCONT_GRID_READY = .TRUE.
        END SUBROUTINE Q1NP_CONTACT_INIT_GRID_NODES

      END MODULE Q1NP_CONTACT_ALGORITHMS_MOD
