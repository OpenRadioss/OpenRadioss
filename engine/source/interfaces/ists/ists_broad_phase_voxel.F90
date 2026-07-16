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
!||====================================================================
!||    sts_broad_phase_voxel_mod   ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||--- uses       -----------------------------------------------------
!||    contact_broad_phase_tol_mod   ../engine/source/interfaces/ists/contact_broad_phase_tol_mod.F90
!||    ists_sts_voxel_grid_mod        ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||--- called by ------------------------------------------------------
!||    ists_mainf              ../engine/source/interfaces/ists/ists_mainf.F
!||====================================================================
!
!   STS voxel broad-phase: produce (master_seg, secondary_seg) candidate
!   pairs from two surfaces (IGRSURF entries) without using the legacy
!   INT7 candidate arrays.
!
!   Algorithm mirrors Q1NP's voxel kernel:
!     1) Sample NSAMPLES_PER_SEG points per segment of each surface
!        (4 corners + centroid by default).
!     2) Build a uniform voxel grid over the (padded) master point cloud.
!     3) For every secondary point that lies inside the padded box, query
!        the 3x3x3 cell neighborhood and emit unique (mst_seg, sec_seg)
!        pairs into the existing STS_CONTACTS_ASSEMBLE arrays.
!
!   Output formats match what STS_REMAP_SEGMENTS produces today, so
!   downstream STS_CONTACTS_ASSEMBLE consumes the data unchanged:
!     CAND_SEC_SEG_ID(I, 1)   = secondary segment index in IGRSURF(SEC)
!     CAND_SEC_SEG_ID(I, 2:5) = secondary segment node IDs
!     CAND_MST_SEG_ID(I, 1)   = master   segment index in IGRSURF(MST)
!     CAND_MST_SEG_ID(I, 2:5) = master   segment node IDs
!     CONT_ELEMENT(I, 1:3, 1:4) = master   (primary)   coordinates
!     CONT_ELEMENT(I, 1:3, 5:8) = secondary            coordinates
!
!||====================================================================
!||    sts_broad_phase_voxel_mod     ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||--- called by ------------------------------------------------------
!||    ists_mainf                    ../engine/source/interfaces/ists/ists_mainf.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                  ../common_source/modules/constant_mod.F
!||    contact_broad_phase_tol_mod   ../engine/source/interfaces/ists/contact_broad_phase_tol_mod.F90
!||    groupdef_mod                  ../common_source/modules/groupdef_mod.F
!||    ists_sts_voxel_grid_mod       ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||    my_alloc_mod                  ../common_source/tools/memory/my_alloc.F90
!||    my_dealloc_mod                ../common_source/tools/memory/my_dealloc.F90
!||    precision_mod                 ../common_source/modules/precision_mod.F90
!||====================================================================
      MODULE STS_BROAD_PHASE_VOXEL_MOD
        USE PRECISION_MOD, ONLY : WP
        USE CONSTANT_MOD,  ONLY : ZERO, ONE, THREE
        USE GROUPDEF_MOD,  ONLY : SURF_
        USE CONTACT_BROAD_PHASE_TOL_MOD
        USE ISTS_STS_VOXEL_GRID_MOD
        USE MY_ALLOC_MOD
        USE MY_DEALLOC_MOD, ONLY : MY_DEALLOC
        IMPLICIT NONE
        PRIVATE
!-----------------------------------------------------------------------
!       Tuning constants
!-----------------------------------------------------------------------
!       Default number of samples per segment (4 corners + centroid).
        INTEGER, PARAMETER, PUBLIC :: STS_VOXEL_NSAMPLES_PER_SEG = 5

!       Hard cap on voxel cells to avoid OOM / pathological runtimes.
        INTEGER, PARAMETER :: STS_VOXEL_HASH_MIN_SLOTS = 1024
        REAL(KIND=WP), PARAMETER :: STS_VOXEL_EPS_SPAN = 1.0E-12_WP
        PUBLIC :: STS_VOXEL_BROAD_PHASE
!
      CONTAINS
!=======================================================================
!   STS_VOXEL_BROAD_PHASE
!
!   Top-level entry: builds segment sample clouds, runs voxel search,
!   emits unique segment pairs and corresponding 8-node coordinate
!   block. Returns COUNT = number of valid pairs (clamped to
!   MAX_STS_SIZE_ACTUAL). OVERFLOW is set to .TRUE. if the storage was
!   saturated and not all candidate pairs could be stored.
!=======================================================================
!||====================================================================
!||    sts_voxel_broad_phase                ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||--- called by ------------------------------------------------------
!||    ists_mainf                           ../engine/source/interfaces/ists/ists_mainf.F90
!||--- calls      -----------------------------------------------------
!||    ists_sts_voxel_grid_get_tol_static   ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||    ists_sts_voxel_grid_is_ready         ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||    ists_sts_voxel_grid_update_dynamic   ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||    sts_voxel_build_seg_points           ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||    sts_voxel_init_grid_params           ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||    sts_voxel_pair_search                ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||    sts_voxel_sort_pairs                 ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||====================================================================
        SUBROUTINE STS_VOXEL_BROAD_PHASE( &
     &      NIN, IGRSURF, NSURF, SEC_SURF_IDX, MST_SURF_IDX, &
     &      X, V, NUMNOD, GAP, DT1, MAX_STS_SIZE_ACTUAL, &
     &      CAND_SEC_SEG_ID, CAND_MST_SEG_ID, CONT_ELEMENT, &
     &      COUNT, OVERFLOW, D_MIN)
!-----------------------------------------------
!         Dummy arguments
!-----------------------------------------------
          INTEGER, INTENT(IN)  :: NIN
          INTEGER, INTENT(IN)  :: NSURF
          TYPE(SURF_), DIMENSION(NSURF), INTENT(IN) :: IGRSURF
          INTEGER, INTENT(IN)  :: SEC_SURF_IDX, MST_SURF_IDX
          INTEGER, INTENT(IN)  :: NUMNOD
          REAL(KIND=WP), INTENT(IN) :: X(3, NUMNOD), V(3, NUMNOD)
          REAL(KIND=WP), INTENT(IN) :: GAP, DT1
          INTEGER, INTENT(IN)  :: MAX_STS_SIZE_ACTUAL
          INTEGER, INTENT(INOUT) :: CAND_SEC_SEG_ID(MAX_STS_SIZE_ACTUAL, 5)
          INTEGER, INTENT(INOUT) :: CAND_MST_SEG_ID(MAX_STS_SIZE_ACTUAL, 5)
          REAL(KIND=WP), INTENT(INOUT) :: CONT_ELEMENT(MAX_STS_SIZE_ACTUAL, 3, 8)
          INTEGER, INTENT(INOUT) :: COUNT
          LOGICAL, INTENT(INOUT) :: OVERFLOW
          REAL(KIND=WP), INTENT(INOUT) :: D_MIN
!-----------------------------------------------
!         Local variables
!-----------------------------------------------
          INTEGER :: NSEG_SEC, NSEG_MST
          INTEGER :: NPTS_S, NPTS_M
          REAL(KIND=WP) :: VMAXDT, TOL_STATIC, SEARCH_PADDING
          REAL(KIND=WP), ALLOCATABLE :: PTS_S(:,:), PTS_M(:,:)
          INTEGER, ALLOCATABLE :: SEG_OF_PT_S(:), SEG_OF_PT_M(:)
!-----------------------------------------------
!         Initialization
!-----------------------------------------------
          COUNT = 0
          OVERFLOW = .FALSE.
          D_MIN = HUGE(ONE)
!
          IF (SEC_SURF_IDX <= 0 .OR. SEC_SURF_IDX > NSURF) RETURN
          IF (MST_SURF_IDX <= 0 .OR. MST_SURF_IDX > NSURF) RETURN
          IF (MAX_STS_SIZE_ACTUAL <= 0) RETURN
!
          NSEG_SEC = IGRSURF(SEC_SURF_IDX)%NSEG
          NSEG_MST = IGRSURF(MST_SURF_IDX)%NSEG
          IF (NSEG_SEC <= 0 .OR. NSEG_MST <= 0) RETURN
          IF (.NOT. ALLOCATED(IGRSURF(SEC_SURF_IDX)%NODES)) RETURN
          IF (.NOT. ALLOCATED(IGRSURF(MST_SURF_IDX)%NODES)) RETURN
!
!-----------------------------------------------
!         Build sample point clouds
!-----------------------------------------------
          CALL MY_ALLOC(PTS_S, 3, NSEG_SEC * STS_VOXEL_NSAMPLES_PER_SEG, "PTS_S")
          CALL MY_ALLOC(SEG_OF_PT_S, NSEG_SEC * STS_VOXEL_NSAMPLES_PER_SEG, "SEG_OF_PT_S")
          CALL STS_VOXEL_BUILD_SEG_POINTS( &
     &        IGRSURF, NSURF, SEC_SURF_IDX, X, NUMNOD, &
     &        STS_VOXEL_NSAMPLES_PER_SEG, &
     &        PTS_S, SEG_OF_PT_S, NPTS_S)
!
          CALL MY_ALLOC(PTS_M, 3, NSEG_MST * STS_VOXEL_NSAMPLES_PER_SEG, "PTS_M")
          CALL MY_ALLOC(SEG_OF_PT_M, NSEG_MST * STS_VOXEL_NSAMPLES_PER_SEG, "SEG_OF_PT_M")
          CALL STS_VOXEL_BUILD_SEG_POINTS( &
     &        IGRSURF, NSURF, MST_SURF_IDX, X, NUMNOD, &
     &        STS_VOXEL_NSAMPLES_PER_SEG, &
     &        PTS_M, SEG_OF_PT_M, NPTS_M)
!
          IF (NPTS_S <= 0 .OR. NPTS_M <= 0) THEN
            CALL MY_DEALLOC(PTS_S)
            CALL MY_DEALLOC(PTS_M)
            CALL MY_DEALLOC(SEG_OF_PT_S)
            CALL MY_DEALLOC(SEG_OF_PT_M)
            RETURN
          END IF

!-----------------------------------------------
!         static mesh/gap + VMAXDT
!-----------------------------------------------
          IF (.NOT. ISTS_STS_VOXEL_GRID_IS_READY(NIN)) THEN
            CALL STS_VOXEL_INIT_GRID_PARAMS( &
     &          NIN, IGRSURF, NSURF, SEC_SURF_IDX, MST_SURF_IDX, &
     &          X, NUMNOD, GAP)
          END IF

          VMAXDT = INTER_BP_TOL_VMAXDT_SURF( &
     &        V, IGRSURF, NSURF, SEC_SURF_IDX, MST_SURF_IDX, NUMNOD, DT1)
          CALL ISTS_STS_VOXEL_GRID_GET_TOL_STATIC(NIN, TOL_STATIC)
          SEARCH_PADDING = TOL_STATIC + VMAXDT
          CALL ISTS_STS_VOXEL_GRID_UPDATE_DYNAMIC(NIN, SEARCH_PADDING)

!-----------------------------------------------
!         Voxel pair search and pair emission
!-----------------------------------------------
          CALL STS_VOXEL_PAIR_SEARCH( &
     &        NIN, IGRSURF, NSURF, SEC_SURF_IDX, MST_SURF_IDX, X, NUMNOD, &
     &        PTS_S, SEG_OF_PT_S, NPTS_S, &
     &        PTS_M, SEG_OF_PT_M, NPTS_M, NSEG_SEC, &
     &        MAX_STS_SIZE_ACTUAL, &
     &        CAND_SEC_SEG_ID, CAND_MST_SEG_ID, CONT_ELEMENT, &
     &        COUNT, OVERFLOW, D_MIN)

!         Keep pair ordering across cycles so pair-indexed
!         history (friction) stays aligned.
          IF (COUNT > 1) THEN
            CALL STS_VOXEL_SORT_PAIRS( &
     &          CAND_SEC_SEG_ID, CAND_MST_SEG_ID, CONT_ELEMENT, &
     &          COUNT, MAX_STS_SIZE_ACTUAL)
          END IF

          CALL MY_DEALLOC(PTS_S)
          CALL MY_DEALLOC(PTS_M)
          CALL MY_DEALLOC(SEG_OF_PT_S)
          CALL MY_DEALLOC(SEG_OF_PT_M)
!
        END SUBROUTINE STS_VOXEL_BROAD_PHASE
!=======================================================================
!   STS_VOXEL_BUILD_SEG_POINTS
!
!   Sample NSAMPLES_PER_SEG points per segment of IGRSURF(SURF_IDX).
!   Layout per segment: 4 corner nodes followed by the centroid, so
!   NSAMPLES_PER_SEG must be either 4 or 5. Any other value falls back
!   to corners-only and the centroid sample is skipped.
!=======================================================================
!||====================================================================
!||    sts_voxel_build_seg_points   ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||--- called by ------------------------------------------------------
!||    sts_voxel_broad_phase        ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||====================================================================
        SUBROUTINE STS_VOXEL_BUILD_SEG_POINTS( &
     &      IGRSURF, NSURF, SURF_IDX, X, NUMNOD, &
     &      NSAMPLES_PER_SEG, PTS, SEG_OF_PT, NPTS_OUT)
          INTEGER, INTENT(IN)  :: NSURF, SURF_IDX
          TYPE(SURF_), DIMENSION(NSURF), INTENT(IN) :: IGRSURF
          INTEGER, INTENT(IN)  :: NUMNOD, NSAMPLES_PER_SEG
          REAL(KIND=WP), INTENT(IN) :: X(3, NUMNOD)
          REAL(KIND=WP), INTENT(INOUT) :: PTS(:, :)
          INTEGER, INTENT(INOUT) :: SEG_OF_PT(:)
          INTEGER, INTENT(INOUT) :: NPTS_OUT
!
          INTEGER :: ISEG, NSEG, K, NID, NVALID
          REAL(KIND=WP) :: XCENT(3)
!
          NPTS_OUT = 0
          NSEG = IGRSURF(SURF_IDX)%NSEG
          IF (NSEG <= 0) RETURN
!
          DO ISEG = 1, NSEG
            XCENT = ZERO
            NVALID = 0
!
!           4 corner samples
            DO K = 1, 4
              NID = IGRSURF(SURF_IDX)%NODES(ISEG, K)
              IF (NID <= 0 .OR. NID > NUMNOD) CYCLE
              NPTS_OUT = NPTS_OUT + 1
              PTS(1, NPTS_OUT) = X(1, NID)
              PTS(2, NPTS_OUT) = X(2, NID)
              PTS(3, NPTS_OUT) = X(3, NID)
              SEG_OF_PT(NPTS_OUT) = ISEG
              XCENT(1) = XCENT(1) + X(1, NID)
              XCENT(2) = XCENT(2) + X(2, NID)
              XCENT(3) = XCENT(3) + X(3, NID)
              NVALID = NVALID + 1
            END DO
!
!           Centroid sample (only if requested and at least one valid corner).
            IF (NSAMPLES_PER_SEG >= 5 .AND. NVALID > 0) THEN
              NPTS_OUT = NPTS_OUT + 1
              PTS(1, NPTS_OUT) = XCENT(1) / REAL(NVALID, WP)
              PTS(2, NPTS_OUT) = XCENT(2) / REAL(NVALID, WP)
              PTS(3, NPTS_OUT) = XCENT(3) / REAL(NVALID, WP)
              SEG_OF_PT(NPTS_OUT) = ISEG
            END IF
          END DO
!
        END SUBROUTINE STS_VOXEL_BUILD_SEG_POINTS
!=======================================================================
!   STS_VOXEL_AABB
!   Axis-aligned bounding box of a 3 x NPTS point cloud.
!=======================================================================
!||====================================================================
!||    sts_voxel_aabb          ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||--- called by ------------------------------------------------------
!||    sts_voxel_pair_search   ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||====================================================================
        SUBROUTINE STS_VOXEL_AABB(PTS, NPTS, XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX)
          INTEGER, INTENT(IN) :: NPTS
          REAL(KIND=WP), INTENT(IN) :: PTS(3, NPTS)
          REAL(KIND=WP), INTENT(INOUT) :: XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX
          INTEGER :: I

          XMIN = PTS(1, 1)
          XMAX = PTS(1, 1)
          YMIN = PTS(2, 1)
          YMAX = PTS(2, 1)
          ZMIN = PTS(3, 1)
          ZMAX = PTS(3, 1)
          DO I = 2, NPTS
            IF (PTS(1, I) < XMIN) XMIN = PTS(1, I)
            IF (PTS(1, I) > XMAX) XMAX = PTS(1, I)
            IF (PTS(2, I) < YMIN) YMIN = PTS(2, I)
            IF (PTS(2, I) > YMAX) YMAX = PTS(2, I)
            IF (PTS(3, I) < ZMIN) ZMIN = PTS(3, I)
            IF (PTS(3, I) > ZMAX) ZMAX = PTS(3, I)
          END DO
        END SUBROUTINE STS_VOXEL_AABB
!=======================================================================
!   STS_VOXEL_PT_TO_CELL
!   Map a point into clamped voxel indices and a linear cell id.
!=======================================================================
!||====================================================================
!||    sts_voxel_pt_to_cell    ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||--- called by ------------------------------------------------------
!||    sts_voxel_pair_search   ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||====================================================================
        SUBROUTINE STS_VOXEL_PT_TO_CELL( &
     &      PX, PY, PZ, XMIN, YMIN, ZMIN, &
     &      NBX, NBY, NBZ, SPAN_X, SPAN_Y, SPAN_Z, NVOXELS, &
     &      IX, IY, IZ, CELLID)
          REAL(KIND=WP), INTENT(IN) :: PX, PY, PZ
          REAL(KIND=WP), INTENT(IN) :: XMIN, YMIN, ZMIN
          REAL(KIND=WP), INTENT(IN) :: SPAN_X, SPAN_Y, SPAN_Z
          INTEGER, INTENT(IN) :: NBX, NBY, NBZ, NVOXELS
          INTEGER, INTENT(INOUT) :: IX, IY, IZ, CELLID
          REAL(KIND=WP) :: RX, RY, RZ

          RX = REAL(NBX, WP) * (PX - XMIN) / MAX(SPAN_X, STS_VOXEL_EPS_SPAN)
          RY = REAL(NBY, WP) * (PY - YMIN) / MAX(SPAN_Y, STS_VOXEL_EPS_SPAN)
          RZ = REAL(NBZ, WP) * (PZ - ZMIN) / MAX(SPAN_Z, STS_VOXEL_EPS_SPAN)
          IF (RX /= RX) RX = ZERO
          IF (RY /= RY) RY = ZERO
          IF (RZ /= RZ) RZ = ZERO
          IX = MAX(0, MIN(NBX - 1, INT(RX)))
          IY = MAX(0, MIN(NBY - 1, INT(RY)))
          IZ = MAX(0, MIN(NBZ - 1, INT(RZ)))
          CELLID = MAX(1, MIN(NVOXELS, IZ * NBX * NBY + IY * NBX + IX + 1))
        END SUBROUTINE STS_VOXEL_PT_TO_CELL
!=======================================================================
!   STS_VOXEL_INIT_GRID_PARAMS
!
!   Compute static (GAP + mesh margin) and CELL_SIZE once per
!   interface; kinematic VMAXDT is added each cycle in broad phase.
!=======================================================================
!||====================================================================
!||    sts_voxel_init_grid_params     ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||--- called by ------------------------------------------------------
!||    sts_voxel_broad_phase          ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||--- calls      -----------------------------------------------------
!||    ists_sts_voxel_grid_init       ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||    ists_sts_voxel_grid_is_ready   ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||====================================================================
        SUBROUTINE STS_VOXEL_INIT_GRID_PARAMS( &
     &      NIN, IGRSURF, NSURF, SEC_SURF_IDX, MST_SURF_IDX, &
     &      X, NUMNOD, GAP)
          INTEGER, INTENT(IN) :: NIN, NSURF, SEC_SURF_IDX, MST_SURF_IDX
          INTEGER, INTENT(IN) :: NUMNOD
          TYPE(SURF_), DIMENSION(NSURF), INTENT(IN) :: IGRSURF
          REAL(KIND=WP), INTENT(IN) :: X(3, NUMNOD)
          REAL(KIND=WP), INTENT(IN) :: GAP
!
          REAL(KIND=WP) :: CELL_SIZE, TOL_STATIC
          REAL(KIND=WP) :: H_MESH_S, H_MESH_M, H_MESH
!
          IF (ISTS_STS_VOXEL_GRID_IS_READY(NIN)) RETURN
!
          H_MESH_S = INTER_BP_TOL_MESH_SCALE_SURF( &
     &        IGRSURF, NSURF, SEC_SURF_IDX, X, NUMNOD)
          H_MESH_M = INTER_BP_TOL_MESH_SCALE_SURF( &
     &        IGRSURF, NSURF, MST_SURF_IDX, X, NUMNOD)
          H_MESH = MAX(H_MESH_S, H_MESH_M, INTER_BP_TOL_GAP_FALLBACK)
          TOL_STATIC = INTER_BP_TOL_TZINF(GAP, H_MESH, ZERO)
          CELL_SIZE = TOL_STATIC
!
          CALL ISTS_STS_VOXEL_GRID_INIT(NIN, TOL_STATIC, CELL_SIZE)
!
        END SUBROUTINE STS_VOXEL_INIT_GRID_PARAMS
!=======================================================================
!   STS_VOXEL_PAIR_SEARCH
!
!   Voxel grid neighborhood search:
!     1) Compute master AABB and pad it by SEARCH_PADDING.
!     2) Coarse AABB-vs-AABB rejection: if minimum distance > padding,
!        return COUNT = 0.
!     3) Build a linked-list voxel grid containing master sample points.
!     4) For each secondary point inside the padded box, look up the
!        3x3x3 cell neighborhood and emit unique (mst_seg, sec_seg)
!        pairs into the output arrays.
!=======================================================================
!||====================================================================
!||    sts_voxel_pair_search     ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||--- called by ------------------------------------------------------
!||    sts_voxel_broad_phase     ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||--- calls      -----------------------------------------------------
!||    ists_sts_voxel_grid_get   ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||    sts_voxel_aabb            ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||    sts_voxel_emit_pair       ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||    sts_voxel_pt_to_cell      ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||====================================================================
        SUBROUTINE STS_VOXEL_PAIR_SEARCH( &
     &      NIN, IGRSURF, NSURF, SEC_SURF_IDX, MST_SURF_IDX, X, NUMNOD, &
     &      PTS_S, SEG_OF_PT_S, NPTS_S, &
     &      PTS_M, SEG_OF_PT_M, NPTS_M, NSEG_SEC, &
     &      MAX_STS_SIZE_ACTUAL, &
     &      CAND_SEC_SEG_ID, CAND_MST_SEG_ID, CONT_ELEMENT, &
     &      COUNT, OVERFLOW, D_MIN_OUT)
          INTEGER, INTENT(IN)  :: NIN, NSURF, SEC_SURF_IDX, MST_SURF_IDX
          TYPE(SURF_), DIMENSION(NSURF), INTENT(IN) :: IGRSURF
          INTEGER, INTENT(IN)  :: NUMNOD
          REAL(KIND=WP), INTENT(IN) :: X(3, NUMNOD)
          INTEGER, INTENT(IN)  :: NPTS_S, NPTS_M
          REAL(KIND=WP), INTENT(IN) :: PTS_S(3, NPTS_S), PTS_M(3, NPTS_M)
          INTEGER, INTENT(IN)  :: SEG_OF_PT_S(NPTS_S), SEG_OF_PT_M(NPTS_M)
          INTEGER, INTENT(IN)  :: NSEG_SEC, MAX_STS_SIZE_ACTUAL
          INTEGER, INTENT(INOUT) :: CAND_SEC_SEG_ID(MAX_STS_SIZE_ACTUAL, 5)
          INTEGER, INTENT(INOUT) :: CAND_MST_SEG_ID(MAX_STS_SIZE_ACTUAL, 5)
          REAL(KIND=WP), INTENT(INOUT) :: CONT_ELEMENT(MAX_STS_SIZE_ACTUAL, 3, 8)
          INTEGER, INTENT(INOUT) :: COUNT
          LOGICAL, INTENT(INOUT) :: OVERFLOW
          REAL(KIND=WP), INTENT(INOUT) :: D_MIN_OUT
!
          REAL(KIND=WP) :: XMIN_M, XMAX_M, YMIN_M, YMAX_M, ZMIN_M, ZMAX_M
          REAL(KIND=WP) :: XMIN_S, XMAX_S, YMIN_S, YMAX_S, ZMIN_S, ZMAX_S
          REAL(KIND=WP) :: CELL_SIZE, SEARCH_PADDING, SPAN_X, SPAN_Y, SPAN_Z
          REAL(KIND=WP) :: DX, DY, DZ, AABB_DIST, DIST_SQ, PAD_SQ, D_MIN_SQ
          INTEGER :: NBX, NBY, NBZ, NVOXELS, N_CELL_RADIUS
          INTEGER :: IM, IS, IX, IY, IZ, JX, JY, JZ, CELLID, JJ
          INTEGER :: IX_LO, IX_HI, IY_LO, IY_HI, IZ_LO, IZ_HI
          INTEGER :: HASH_SIZE, HASH_STRIDE
          INTEGER, ALLOCATABLE :: VOXEL(:), NEXT_PT(:)
          INTEGER(KIND=8), ALLOCATABLE :: HASH_KEYS(:)
          INTEGER :: SEG_S, SEG_M
!
          CALL ISTS_STS_VOXEL_GRID_GET(NIN, CELL_SIZE, SEARCH_PADDING, &
     &        PAD_SQ, N_CELL_RADIUS)
!
          ! Compute the AABB of the master and secondary points
          CALL STS_VOXEL_AABB(PTS_M, NPTS_M, &
     &        XMIN_M, XMAX_M, YMIN_M, YMAX_M, ZMIN_M, ZMAX_M)
          CALL STS_VOXEL_AABB(PTS_S, NPTS_S, &
     &        XMIN_S, XMAX_S, YMIN_S, YMAX_S, ZMIN_S, ZMAX_S)
!
!         Coarse AABB rejection (unpadded primary vs secondary AABB).
          DX = MAX(ZERO, MAX(XMIN_S - XMAX_M, XMIN_M - XMAX_S))
          DY = MAX(ZERO, MAX(YMIN_S - YMAX_M, YMIN_M - YMAX_S))
          DZ = MAX(ZERO, MAX(ZMIN_S - ZMAX_M, ZMIN_M - ZMAX_S))
          AABB_DIST = SQRT(DX*DX + DY*DY + DZ*DZ)
          
          ! If the AABB distance is greater than the search padding, return
          IF (AABB_DIST > SEARCH_PADDING) THEN
            D_MIN_OUT = AABB_DIST
            RETURN
          END IF

          D_MIN_SQ = AABB_DIST * AABB_DIST
!
          ! Pad the master and secondary AABB
          XMIN_M = XMIN_M - SEARCH_PADDING
          XMAX_M = XMAX_M + SEARCH_PADDING
          YMIN_M = YMIN_M - SEARCH_PADDING
          YMAX_M = YMAX_M + SEARCH_PADDING
          ZMIN_M = ZMIN_M - SEARCH_PADDING
          ZMAX_M = ZMAX_M + SEARCH_PADDING
!
          ! Compute the span of the padded master and secondary AABB
          SPAN_X = XMAX_M - XMIN_M
          SPAN_Y = YMAX_M - YMIN_M
          SPAN_Z = ZMAX_M - ZMIN_M
!
          ! Compute the number of cells in the voxel grid
          NBX = MAX(1, CEILING(SPAN_X / CELL_SIZE))
          NBY = MAX(1, CEILING(SPAN_Y / CELL_SIZE))
          NBZ = MAX(1, CEILING(SPAN_Z / CELL_SIZE))
          NVOXELS = NBX * NBY * NBZ
!
          ! Compute the size of the hash table (needed for the linked-list per cell)
          HASH_SIZE = MIN(HUGE(1), MAX(STS_VOXEL_HASH_MIN_SLOTS, 2 * MAX_STS_SIZE_ACTUAL + 1))
          HASH_STRIDE = MAX(NSEG_SEC, 1) + 1
          CALL MY_ALLOC(VOXEL, NVOXELS, "VOXEL")
          CALL MY_ALLOC(NEXT_PT, NPTS_M, "NEXT_PT")
          ALLOCATE(HASH_KEYS(HASH_SIZE))
          ! Initialize the voxel grid and the linked-list per cell
          VOXEL = 0
          NEXT_PT = 0
          HASH_KEYS = 0_8
!
          ! Fill voxel grid with master points (linked-list per cell)
          DO IM = 1, NPTS_M
            CALL STS_VOXEL_PT_TO_CELL( &
     &          PTS_M(1, IM), PTS_M(2, IM), PTS_M(3, IM), &
     &          XMIN_M, YMIN_M, ZMIN_M, &
     &          NBX, NBY, NBZ, SPAN_X, SPAN_Y, SPAN_Z, NVOXELS, &
     &          IX, IY, IZ, CELLID)
            NEXT_PT(IM) = VOXEL(CELLID)
            VOXEL(CELLID) = IM
          END DO
!
          ! Query secondary points inside the padded master box
          DO IS = 1, NPTS_S
            IF (PTS_S(1, IS) < XMIN_M .OR. PTS_S(1, IS) > XMAX_M) CYCLE
            IF (PTS_S(2, IS) < YMIN_M .OR. PTS_S(2, IS) > YMAX_M) CYCLE
            IF (PTS_S(3, IS) < ZMIN_M .OR. PTS_S(3, IS) > ZMAX_M) CYCLE
!
            ! Map the secondary point to the voxel grid (to find the cell)
            CALL STS_VOXEL_PT_TO_CELL( &
     &          PTS_S(1, IS), PTS_S(2, IS), PTS_S(3, IS), &
     &          XMIN_M, YMIN_M, ZMIN_M, &
     &          NBX, NBY, NBZ, SPAN_X, SPAN_Y, SPAN_Z, NVOXELS, &
     &          IX, IY, IZ, CELLID)
!
            IX_LO = MAX(0, IX - N_CELL_RADIUS)
            IX_HI = MIN(NBX - 1, IX + N_CELL_RADIUS)
            IY_LO = MAX(0, IY - N_CELL_RADIUS)
            IY_HI = MIN(NBY - 1, IY + N_CELL_RADIUS)
            IZ_LO = MAX(0, IZ - N_CELL_RADIUS)
            IZ_HI = MIN(NBZ - 1, IZ + N_CELL_RADIUS)
!
            SEG_S = SEG_OF_PT_S(IS)
!
            DO JZ = IZ_LO, IZ_HI
              DO JY = IY_LO, IY_HI
                DO JX = IX_LO, IX_HI
                  CELLID = MAX(1, MIN(NVOXELS, &
     &                JZ * NBX * NBY + JY * NBX + JX + 1))
                  JJ = VOXEL(CELLID)
                  DO WHILE (JJ > 0)
                    IF (JJ > NPTS_M) EXIT
                    SEG_M = SEG_OF_PT_M(JJ)
                    IF (SEG_S > 0 .AND. SEG_M > 0) THEN
                      DX = PTS_S(1, IS) - PTS_M(1, JJ)
                      DY = PTS_S(2, IS) - PTS_M(2, JJ)
                      DZ = PTS_S(3, IS) - PTS_M(3, JJ)
                      DIST_SQ = DX*DX + DY*DY + DZ*DZ
                      IF (DIST_SQ < D_MIN_SQ) D_MIN_SQ = DIST_SQ
                      ! If the distance is less than the search padding, emit the pair
                      IF (DIST_SQ <= PAD_SQ) THEN
                        ! Appends a single (sec_seg, mst_seg) pair to the output arrays unless it already exists
                        CALL STS_VOXEL_EMIT_PAIR( &
     &                    IGRSURF, NSURF, SEC_SURF_IDX, MST_SURF_IDX, &
     &                    X, NUMNOD, SEG_S, SEG_M, HASH_STRIDE, &
     &                    HASH_SIZE, HASH_KEYS, MAX_STS_SIZE_ACTUAL, &
     &                    CAND_SEC_SEG_ID, CAND_MST_SEG_ID, &
     &                    CONT_ELEMENT, COUNT, OVERFLOW)
                      !  IF (OVERFLOW) EXIT
                      END IF
                    END IF
                    JJ = NEXT_PT(JJ)
                    IF (JJ < 0) EXIT
                  END DO
                  ! IF (OVERFLOW) EXIT
                END DO
                !IF (OVERFLOW) EXIT
              END DO
              !IF (OVERFLOW) EXIT
            END DO
            IF (OVERFLOW) EXIT
          END DO
!
          CALL MY_DEALLOC(VOXEL)
          CALL MY_DEALLOC(NEXT_PT)
          DEALLOCATE(HASH_KEYS)
          D_MIN_OUT = SQRT(D_MIN_SQ)
!
        END SUBROUTINE STS_VOXEL_PAIR_SEARCH
!=======================================================================
!   STS_VOXEL_EMIT_PAIR
!
!   Append a single (sec_seg, mst_seg) pair to the output arrays unless
!   it already exists. On overflow (COUNT == MAX_STS_SIZE_ACTUAL) the
!   OVERFLOW flag is set and no further pairs are stored.
!=======================================================================
!||====================================================================
!||    sts_voxel_emit_pair     ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||--- called by ------------------------------------------------------
!||    sts_voxel_pair_search   ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||====================================================================
        SUBROUTINE STS_VOXEL_EMIT_PAIR( &
     &      IGRSURF, NSURF, SEC_SURF_IDX, MST_SURF_IDX, X, NUMNOD, &
     &      SEC_SEG, MST_SEG, HASH_STRIDE, HASH_SIZE, HASH_KEYS, &
     &      MAX_STS_SIZE_ACTUAL, &
     &      CAND_SEC_SEG_ID, CAND_MST_SEG_ID, CONT_ELEMENT, &
     &      COUNT, OVERFLOW)
          INTEGER, INTENT(IN) :: NSURF, SEC_SURF_IDX, MST_SURF_IDX
          TYPE(SURF_), DIMENSION(NSURF), INTENT(IN) :: IGRSURF
          INTEGER, INTENT(IN) :: NUMNOD
          REAL(KIND=WP), INTENT(IN) :: X(3, NUMNOD)
          INTEGER, INTENT(IN) :: SEC_SEG, MST_SEG, HASH_STRIDE, HASH_SIZE
          INTEGER(KIND=8), INTENT(INOUT) :: HASH_KEYS(HASH_SIZE)
          INTEGER, INTENT(IN) :: MAX_STS_SIZE_ACTUAL
          INTEGER, INTENT(INOUT) :: CAND_SEC_SEG_ID(MAX_STS_SIZE_ACTUAL, 5)
          INTEGER, INTENT(INOUT) :: CAND_MST_SEG_ID(MAX_STS_SIZE_ACTUAL, 5)
          REAL(KIND=WP), INTENT(INOUT) :: CONT_ELEMENT(MAX_STS_SIZE_ACTUAL, 3, 8)
          INTEGER, INTENT(INOUT) :: COUNT
          LOGICAL, INTENT(INOUT) :: OVERFLOW
!
          INTEGER :: K, J, NID, IH, PROBE
          INTEGER(KIND=8) :: PAIR_KEY
!
          IF (OVERFLOW) RETURN
!
!         Open-addressing hash lookup for duplicate (sec_seg, mst_seg) pairs.
          PAIR_KEY = INT(MST_SEG, KIND=8) * INT(HASH_STRIDE, KIND=8) + &
     &      INT(SEC_SEG, KIND=8)
          IH = INT(MOD(PAIR_KEY, INT(HASH_SIZE, KIND=8))) + 1
          PROBE = 0
          DO WHILE (HASH_KEYS(IH) /= 0_8)
            IF (HASH_KEYS(IH) == PAIR_KEY) RETURN
            IH = IH + 1
            IF (IH > HASH_SIZE) IH = 1
            PROBE = PROBE + 1
            IF (PROBE >= HASH_SIZE) RETURN
          END DO
!
          IF (COUNT >= MAX_STS_SIZE_ACTUAL) THEN
            OVERFLOW = .TRUE.
            RETURN
          END IF
!
          COUNT = COUNT + 1
          HASH_KEYS(IH) = PAIR_KEY
!
!         Secondary segment metadata
          CAND_SEC_SEG_ID(COUNT, 1) = SEC_SEG
          DO K = 1, 4
            CAND_SEC_SEG_ID(COUNT, K + 1) = &
     &          IGRSURF(SEC_SURF_IDX)%NODES(SEC_SEG, K)
          END DO
!
!         Master segment metadata
          CAND_MST_SEG_ID(COUNT, 1) = MST_SEG
          DO K = 1, 4
            CAND_MST_SEG_ID(COUNT, K + 1) = &
     &          IGRSURF(MST_SURF_IDX)%NODES(MST_SEG, K)
          END DO
!
!         Master (primary) coordinates -> CONT_ELEMENT(I, 1:3, 1:4)
          J = 1
          DO K = 2, 5
            NID = CAND_MST_SEG_ID(COUNT, K)
            IF (NID > 0 .AND. NID <= NUMNOD) THEN
              CONT_ELEMENT(COUNT, 1, J) = X(1, NID)
              CONT_ELEMENT(COUNT, 2, J) = X(2, NID)
              CONT_ELEMENT(COUNT, 3, J) = X(3, NID)
            ELSE
              CONT_ELEMENT(COUNT, 1:3, J) = ZERO
            END IF
            J = J + 1
          END DO
!
!         Secondary coordinates -> CONT_ELEMENT(I, 1:3, 5:8)
          J = 5
          DO K = 2, 5
            NID = CAND_SEC_SEG_ID(COUNT, K)
            IF (NID > 0 .AND. NID <= NUMNOD) THEN
              CONT_ELEMENT(COUNT, 1, J) = X(1, NID)
              CONT_ELEMENT(COUNT, 2, J) = X(2, NID)
              CONT_ELEMENT(COUNT, 3, J) = X(3, NID)
            ELSE
              CONT_ELEMENT(COUNT, 1:3, J) = ZERO
            END IF
            J = J + 1
          END DO
!
        END SUBROUTINE STS_VOXEL_EMIT_PAIR

!=======================================================================
!   STS_VOXEL_SORT_PAIRS
!   Heap sort by (mst_seg, sec_seg) for stable pair-indexed friction history.
!=======================================================================
!||====================================================================
!||    sts_voxel_sort_pairs    ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||--- called by ------------------------------------------------------
!||    sts_voxel_broad_phase   ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||--- calls      -----------------------------------------------------
!||    sts_voxel_sift_down     ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||    sts_voxel_swap_pair     ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||====================================================================
        SUBROUTINE STS_VOXEL_SORT_PAIRS( &
     &      CAND_SEC_SEG_ID, CAND_MST_SEG_ID, CONT_ELEMENT, &
     &      COUNT, MAX_STS_SIZE_ACTUAL)
          INTEGER, INTENT(IN) :: COUNT, MAX_STS_SIZE_ACTUAL
          INTEGER, INTENT(INOUT) :: CAND_SEC_SEG_ID(MAX_STS_SIZE_ACTUAL, 5)
          INTEGER, INTENT(INOUT) :: CAND_MST_SEG_ID(MAX_STS_SIZE_ACTUAL, 5)
          REAL(KIND=WP), INTENT(INOUT) :: CONT_ELEMENT(MAX_STS_SIZE_ACTUAL, 3, 8)

          INTEGER :: START, FINISH

          IF (COUNT <= 1) RETURN

          DO START = COUNT / 2, 1, -1
            CALL STS_VOXEL_SIFT_DOWN(CAND_SEC_SEG_ID, CAND_MST_SEG_ID, &
     &        CONT_ELEMENT, START, COUNT, MAX_STS_SIZE_ACTUAL)
          END DO

          DO FINISH = COUNT, 2, -1
            CALL STS_VOXEL_SWAP_PAIR(CAND_SEC_SEG_ID, CAND_MST_SEG_ID, &
     &        CONT_ELEMENT, 1, FINISH, MAX_STS_SIZE_ACTUAL)
            CALL STS_VOXEL_SIFT_DOWN(CAND_SEC_SEG_ID, CAND_MST_SEG_ID, &
     &        CONT_ELEMENT, 1, FINISH - 1, MAX_STS_SIZE_ACTUAL)
          END DO
        END SUBROUTINE STS_VOXEL_SORT_PAIRS

!=======================================================================
!   STS_VOXEL_SIFT_DOWN
!   Sift down the root of the heap
!=======================================================================
!||====================================================================
!||    sts_voxel_sift_down    ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||--- called by ------------------------------------------------------
!||    sts_voxel_sort_pairs   ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||--- calls      -----------------------------------------------------
!||    sts_voxel_pair_less    ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||    sts_voxel_swap_pair    ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||====================================================================
        SUBROUTINE STS_VOXEL_SIFT_DOWN( &
     &      CAND_SEC_SEG_ID, CAND_MST_SEG_ID, CONT_ELEMENT, &
     &      START, FINISH, MAX_STS_SIZE_ACTUAL)
          INTEGER, INTENT(IN) :: START, FINISH, MAX_STS_SIZE_ACTUAL
          INTEGER, INTENT(INOUT) :: CAND_SEC_SEG_ID(MAX_STS_SIZE_ACTUAL, 5)
          INTEGER, INTENT(INOUT) :: CAND_MST_SEG_ID(MAX_STS_SIZE_ACTUAL, 5)
          REAL(KIND=WP), INTENT(INOUT) :: CONT_ELEMENT(MAX_STS_SIZE_ACTUAL, 3, 8)
          INTEGER :: ROOT, CHILD, SWAP_IDX

          ROOT = START
          DO WHILE (ROOT * 2 <= FINISH)
            CHILD = ROOT * 2
            SWAP_IDX = ROOT

            IF (STS_VOXEL_PAIR_LESS(CAND_SEC_SEG_ID, CAND_MST_SEG_ID, &
     &          SWAP_IDX, CHILD, MAX_STS_SIZE_ACTUAL)) THEN
              SWAP_IDX = CHILD
            END IF

            IF (CHILD + 1 <= FINISH) THEN
              IF (STS_VOXEL_PAIR_LESS(CAND_SEC_SEG_ID, CAND_MST_SEG_ID, &
     &            SWAP_IDX, CHILD + 1, MAX_STS_SIZE_ACTUAL)) THEN
                SWAP_IDX = CHILD + 1
              END IF
            END IF

            IF (SWAP_IDX == ROOT) RETURN

            CALL STS_VOXEL_SWAP_PAIR(CAND_SEC_SEG_ID, CAND_MST_SEG_ID, &
     &        CONT_ELEMENT, ROOT, SWAP_IDX, MAX_STS_SIZE_ACTUAL)
            ROOT = SWAP_IDX
          END DO
        END SUBROUTINE STS_VOXEL_SIFT_DOWN

!=======================================================================
!   STS_VOXEL_PAIR_LESS
!   Compare two pairs by primary segment index
!=======================================================================
!||====================================================================
!||    sts_voxel_pair_less   ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||--- called by ------------------------------------------------------
!||    sts_voxel_sift_down   ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||====================================================================
        LOGICAL FUNCTION STS_VOXEL_PAIR_LESS( &
     &      CAND_SEC_SEG_ID, CAND_MST_SEG_ID, I, J, MAX_STS_SIZE_ACTUAL)
          INTEGER, INTENT(IN) :: I, J, MAX_STS_SIZE_ACTUAL
          INTEGER, INTENT(IN) :: CAND_SEC_SEG_ID(MAX_STS_SIZE_ACTUAL, 5)
          INTEGER, INTENT(IN) :: CAND_MST_SEG_ID(MAX_STS_SIZE_ACTUAL, 5)

          IF (CAND_MST_SEG_ID(I,1) == CAND_MST_SEG_ID(J,1)) THEN
            STS_VOXEL_PAIR_LESS = CAND_SEC_SEG_ID(I,1) < &
     &                             CAND_SEC_SEG_ID(J,1)
          ELSE
            STS_VOXEL_PAIR_LESS = CAND_MST_SEG_ID(I,1) < &
     &                             CAND_MST_SEG_ID(J,1)
          END IF
        END FUNCTION STS_VOXEL_PAIR_LESS

!=======================================================================
!   STS_VOXEL_SWAP_PAIR
!   Swap two pairs
!=======================================================================
!||====================================================================
!||    sts_voxel_swap_pair    ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||--- called by ------------------------------------------------------
!||    sts_voxel_sift_down    ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||    sts_voxel_sort_pairs   ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||====================================================================
        SUBROUTINE STS_VOXEL_SWAP_PAIR( &
     &      CAND_SEC_SEG_ID, CAND_MST_SEG_ID, CONT_ELEMENT, &
     &      I, J, MAX_STS_SIZE_ACTUAL)
          INTEGER, INTENT(IN) :: I, J, MAX_STS_SIZE_ACTUAL
          INTEGER, INTENT(INOUT) :: CAND_SEC_SEG_ID(MAX_STS_SIZE_ACTUAL, 5)
          INTEGER, INTENT(INOUT) :: CAND_MST_SEG_ID(MAX_STS_SIZE_ACTUAL, 5)
          REAL(KIND=WP), INTENT(INOUT) :: CONT_ELEMENT(MAX_STS_SIZE_ACTUAL, 3, 8)
          INTEGER :: SEC_TMP(5), MST_TMP(5)
          REAL(KIND=WP) :: XYZ_TMP(3, 8)

          IF (I == J) RETURN

          SEC_TMP(1:5) = CAND_SEC_SEG_ID(I, 1:5)
          MST_TMP(1:5) = CAND_MST_SEG_ID(I, 1:5)
          XYZ_TMP(1:3, 1:8) = CONT_ELEMENT(I, 1:3, 1:8)

          CAND_SEC_SEG_ID(I, 1:5) = CAND_SEC_SEG_ID(J, 1:5)
          CAND_MST_SEG_ID(I, 1:5) = CAND_MST_SEG_ID(J, 1:5)
          CONT_ELEMENT(I, 1:3, 1:8) = CONT_ELEMENT(J, 1:3, 1:8)

          CAND_SEC_SEG_ID(J, 1:5) = SEC_TMP(1:5)
          CAND_MST_SEG_ID(J, 1:5) = MST_TMP(1:5)
          CONT_ELEMENT(J, 1:3, 1:8) = XYZ_TMP(1:3, 1:8)
        END SUBROUTINE STS_VOXEL_SWAP_PAIR
!
      END MODULE STS_BROAD_PHASE_VOXEL_MOD
