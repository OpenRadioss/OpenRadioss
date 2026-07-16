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
!||    sts_remap_segments                      ../engine/source/interfaces/ists/ists_remap_segments.F90
!||--- called by ------------------------------------------------------
!||    sts_int7_bucket_broad_phase             ../engine/source/interfaces/ists/ists_broad_phase_int7_bucket.F90
!||--- calls      -----------------------------------------------------
!||    sts_remap_add_sec_segs_for_node_patch   ../engine/source/interfaces/ists/ists_remap_segments.F90
!||    sts_remap_build_master_neighbors        ../engine/source/interfaces/ists/ists_remap_segments.F90
!||    sts_remap_build_master_node_adj         ../engine/source/interfaces/ists/ists_remap_segments.F90
!||    sts_remap_build_node_seg_adj            ../engine/source/interfaces/ists/ists_remap_segments.F90
!||    sts_remap_clear_topo_cache              ../engine/source/interfaces/ists/ists_remap_segments.F90
!||    sts_remap_deallocate_work               ../engine/source/interfaces/ists/ists_remap_segments.F90
!||    sts_remap_nearest_sec_seg               ../engine/source/interfaces/ists/ists_remap_segments.F90
!||    sts_remap_try_add_pair                  ../engine/source/interfaces/ists/ists_remap_segments.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                            ../common_source/modules/constant_mod.F
!||    groupdef_mod                            ../common_source/modules/groupdef_mod.F
!||    intbufdef_mod                           ../common_source/modules/interfaces/intbufdef_mod.F90
!||    my_alloc_mod                            ../common_source/tools/memory/my_alloc.F90
!||    my_dealloc_mod                          ../common_source/tools/memory/my_dealloc.F90
!||    precision_mod                           ../common_source/modules/precision_mod.F90
!||====================================================================
      SUBROUTINE STS_REMAP_SEGMENTS(INTBUF_TAB, X, NUMNOD, NRTM, NSN, CAND_SEC_SEG, &
     &  JLT, CAND_N_CUR, CAND_E_CUR, IRECT, CONT_ELEMENT, COUNT, &
     &  IGRSURF, CAND_SEC_SEG_ID, CAND_MST_SEG_ID, &
     &  CAND_SEC_GP_MASK, &
     &  MAX_STS_SIZE_ACTUAL, NSURF_LOCAL, SEC_SURF_ID, MST_SURF_ID)
!-----------------------------------------------
!   M o d u l e s
!----------------------------------------------- 
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
!-----------------------------------------------
!   M o d u l e s   /   I m p l i c i t   T y p e s
!-----------------------------------------------
      use constant_mod
      use my_alloc_mod, only : my_alloc
      use my_dealloc_mod, only : my_dealloc
      use precision_mod, only : WP
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
#include      "mvsiz_p.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      TYPE(INTBUF_STRUCT_), INTENT(IN) :: INTBUF_TAB
      TYPE(SURF_), DIMENSION(NSURF_LOCAL), INTENT(IN) :: IGRSURF
      INTEGER, INTENT(IN)    :: JLT, NUMNOD, NRTM, NSN
      INTEGER, INTENT(IN)    :: CAND_N_CUR(JLT), CAND_E_CUR(JLT)
      INTEGER, INTENT(IN)    :: IRECT(4,NRTM)
      real(kind=WP), INTENT(IN)    :: X(3,NUMNOD)
      INTEGER, INTENT(INOUT) :: CAND_SEC_SEG(MAX_STS_SIZE_ACTUAL)
      INTEGER, INTENT(INOUT) :: CAND_SEC_SEG_ID(MAX_STS_SIZE_ACTUAL,5)
      INTEGER, INTENT(INOUT) :: CAND_MST_SEG_ID(MAX_STS_SIZE_ACTUAL,5)
      INTEGER, INTENT(INOUT) :: CAND_SEC_GP_MASK(MAX_STS_SIZE_ACTUAL,4)
      real(kind=WP), INTENT(INOUT) :: CONT_ELEMENT(MAX_STS_SIZE_ACTUAL,3,8)
      INTEGER, INTENT(INOUT) :: COUNT
      INTEGER, INTENT(IN)    :: MAX_STS_SIZE_ACTUAL
      INTEGER, INTENT(IN)    :: NSURF_LOCAL, SEC_SURF_ID, MST_SURF_ID
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER CAND_MST_SEG(MAX_STS_SIZE_ACTUAL)
      INTEGER I, J, K, NI
      INTEGER candidate, candidateM, sec_seg
      INTEGER SEC_SURF_IDX, NSEG, NSEC_BOUNDS
      INTEGER pair_index
      LOGICAL :: found_corner, pair_added, capacity_full
      LOGICAL :: rebuild_topology
      INTEGER, ALLOCATABLE, SAVE :: IGRSURF_S_TEMP(:,:)
      INTEGER, ALLOCATABLE, SAVE :: SEC_NODE_SEG_COUNT(:)
      INTEGER, ALLOCATABLE, SAVE :: SEC_NODE_SEG_OFF(:)
      INTEGER, ALLOCATABLE, SAVE :: SEC_NODE_SEG_LIST(:)
      INTEGER, ALLOCATABLE, SAVE :: MST_NODE_SEG_COUNT(:)
      INTEGER, ALLOCATABLE, SAVE :: MST_NODE_SEG_OFF(:)
      INTEGER, ALLOCATABLE, SAVE :: MST_NODE_SEG_LIST(:)
      INTEGER, ALLOCATABLE, SAVE :: MST_SEG_NEI_COUNT(:)
      INTEGER, ALLOCATABLE, SAVE :: MST_SEG_NEI_OFF(:)
      INTEGER, ALLOCATABLE, SAVE :: MST_SEG_NEI_LIST(:)
      INTEGER, ALLOCATABLE :: PAIR_HASH_SEC(:)
      INTEGER, ALLOCATABLE :: PAIR_HASH_MST(:)
      INTEGER, ALLOCATABLE :: PAIR_HASH_INDEX(:)
      INTEGER, ALLOCATABLE :: NEAREST_SEC_CACHE(:)
      INTEGER :: PAIR_HASH_SIZE
      INTEGER, SAVE :: TOPO_CACHE_NUMNOD = 0
      INTEGER, SAVE :: TOPO_CACHE_NSEG = 0
      INTEGER, SAVE :: TOPO_CACHE_NRTM = 0
      INTEGER, SAVE :: TOPO_CACHE_SEC_SURF = 0
      INTEGER, SAVE :: TOPO_CACHE_MST_SURF = 0
      LOGICAL, SAVE :: TOPO_CACHE_READY = .FALSE.
      LOGICAL, PARAMETER :: EXPAND_MASTER_PATCH = .TRUE.
!-----------------------------------------------
!   S o u r c e   L i n e s
!-----------------------------------------------
      SEC_SURF_IDX = SEC_SURF_ID
      NSEC_BOUNDS = NSN
      IF (INTBUF_TAB%S_NSV > 0) THEN
        NSEC_BOUNDS = INTBUF_TAB%S_NSV
      ENDIF
      IF (JLT <= 0) THEN
        COUNT = 0
        RETURN
      END IF
      IF (SEC_SURF_IDX <= 0 .OR. SEC_SURF_IDX > NSURF_LOCAL) THEN
        COUNT = 0
        RETURN
      END IF
      IF (MST_SURF_ID <= 0 .OR. MST_SURF_ID > NSURF_LOCAL) THEN
        COUNT = 0
        RETURN
      END IF
!     A valid secondary surface node table is required for node-to-segment
!     remapping.
      IF (IGRSURF(SEC_SURF_IDX)%NSEG <= 0) THEN
        COUNT = 0
        RETURN
      END IF
      IF (.NOT. ALLOCATED(IGRSURF(SEC_SURF_IDX)%NODES)) THEN
        COUNT = 0
        RETURN
      END IF

      NSEG = IGRSURF(SEC_SURF_IDX)%NSEG

      rebuild_topology = .TRUE.
      IF (TOPO_CACHE_READY) THEN
        rebuild_topology = TOPO_CACHE_NUMNOD /= NUMNOD .OR. &
     &    TOPO_CACHE_NSEG /= NSEG .OR. TOPO_CACHE_NRTM /= NRTM .OR. &
     &    TOPO_CACHE_SEC_SURF /= SEC_SURF_IDX .OR. &
     &    TOPO_CACHE_MST_SURF /= MST_SURF_ID
      ENDIF

      IF (rebuild_topology) THEN
        CALL STS_REMAP_CLEAR_TOPO_CACHE()
        CALL MY_ALLOC(IGRSURF_S_TEMP, IGRSURF(SEC_SURF_IDX)%NSEG, 4, "IGRSURF_S_TEMP")
        IGRSURF_S_TEMP = IGRSURF(SEC_SURF_IDX)%NODES

        CALL STS_REMAP_BUILD_NODE_SEG_ADJ(IGRSURF_S_TEMP, NSEG, NUMNOD, &
     &    SEC_NODE_SEG_COUNT, SEC_NODE_SEG_OFF, SEC_NODE_SEG_LIST)
        CALL STS_REMAP_BUILD_MASTER_NODE_ADJ(IRECT, NRTM, NUMNOD, &
     &    MST_NODE_SEG_COUNT, MST_NODE_SEG_OFF, MST_NODE_SEG_LIST)
        CALL STS_REMAP_BUILD_MASTER_NEIGHBORS(IRECT, NRTM, &
     &    MST_NODE_SEG_COUNT, MST_NODE_SEG_OFF, MST_NODE_SEG_LIST, &
     &    MST_SEG_NEI_COUNT, MST_SEG_NEI_OFF, MST_SEG_NEI_LIST)

        TOPO_CACHE_NUMNOD = NUMNOD
        TOPO_CACHE_NSEG = NSEG
        TOPO_CACHE_NRTM = NRTM
        TOPO_CACHE_SEC_SURF = SEC_SURF_IDX
        TOPO_CACHE_MST_SURF = MST_SURF_ID
        TOPO_CACHE_READY = .TRUE.
      ENDIF

      PAIR_HASH_SIZE = MAX(17, 4 * MAX_STS_SIZE_ACTUAL + 1)
      CALL MY_ALLOC(PAIR_HASH_SEC, PAIR_HASH_SIZE, "PAIR_HASH_SEC")
      CALL MY_ALLOC(PAIR_HASH_MST, PAIR_HASH_SIZE, "PAIR_HASH_MST")
      CALL MY_ALLOC(PAIR_HASH_INDEX, PAIR_HASH_SIZE, "PAIR_HASH_INDEX")
      CALL MY_ALLOC(NEAREST_SEC_CACHE, MAX(1, NRTM), "NEAREST_SEC_CACHE")
      PAIR_HASH_SEC = 0
      PAIR_HASH_MST = 0
      PAIR_HASH_INDEX = 0
      NEAREST_SEC_CACHE = 0

!     Convert the compacted active INT7 node/segment candidates into STS
!     secondary-segment/master-segment candidate pairs.
      COUNT = 0

      ! Map candidate nodes to segment pairs
      DO I = 1, JLT
        candidateM = CAND_E_CUR(I)
        IF (candidateM <= 0 .OR. candidateM > NRTM) CYCLE

        found_corner = .FALSE.
        capacity_full = .FALSE.
        IF (CAND_N_CUR(I) > 0 .AND. CAND_N_CUR(I) <= NSEC_BOUNDS) THEN
          candidate = INTBUF_TAB%NSV(CAND_N_CUR(I))
          IF (candidate > 0 .AND. candidate <= NUMNOD) THEN
            CALL STS_REMAP_ADD_SEC_SEGS_FOR_NODE_PATCH( &
     &        NSEG, candidate, candidateM, &
     &        SEC_NODE_SEG_COUNT, SEC_NODE_SEG_OFF, SEC_NODE_SEG_LIST, &
     &        MST_SEG_NEI_COUNT, MST_SEG_NEI_OFF, MST_SEG_NEI_LIST, &
     &        COUNT, CAND_SEC_SEG, CAND_MST_SEG, CAND_SEC_GP_MASK, &
     &        MAX_STS_SIZE_ACTUAL, found_corner, capacity_full)
          ENDIF
        ENDIF
        IF (capacity_full) EXIT

!       Active INT7 node candidates may miss /SURF corner nodes after
!       separation. Fall back to the secondary segment whose centroid is
!       closest to the INT7 main-segment centroid.
        IF (.NOT. found_corner) THEN
          IF (NEAREST_SEC_CACHE(candidateM) == 0) THEN
            NEAREST_SEC_CACHE(candidateM) = &
     &        STS_REMAP_NEAREST_SEC_SEG(IGRSURF_S_TEMP, NSEG, &
     &          IRECT, NRTM, candidateM, NUMNOD, X)
            IF (NEAREST_SEC_CACHE(candidateM) <= 0) THEN
              NEAREST_SEC_CACHE(candidateM) = -1
            ENDIF
          ENDIF
          sec_seg = NEAREST_SEC_CACHE(candidateM)
          IF (sec_seg > 0) THEN
            CALL STS_REMAP_TRY_ADD_PAIR(sec_seg, candidateM, COUNT, &
     &        CAND_SEC_SEG, CAND_MST_SEG, MAX_STS_SIZE_ACTUAL, &
     &        pair_added, pair_index)
            IF (pair_added .AND. pair_index > 0) THEN
              CAND_SEC_GP_MASK(pair_index, 1:4) = 1
            END IF
            IF (.NOT. pair_added) EXIT
          END IF
        END IF
      END DO

      IF (COUNT <= 0) THEN
        CALL STS_REMAP_DEALLOCATE_WORK()
        RETURN
      END IF

      DO I = 1, COUNT
        CAND_SEC_SEG_ID(I, 1) = CAND_SEC_SEG(I)
        CAND_SEC_SEG_ID(I, 2:5) = IGRSURF(SEC_SURF_IDX)%NODES(CAND_SEC_SEG(I), 1:4)
        CAND_MST_SEG_ID(I, 1) = CAND_MST_SEG(I)
        CAND_MST_SEG_ID(I, 2:5) = IRECT(1:4, CAND_MST_SEG(I))
      END DO

!     Store current coordinates for primary nodes 1:4 and secondary nodes 5:8.
      DO I = 1, COUNT
        J = 1
        DO K = 2, 5
          NI = CAND_MST_SEG_ID(I, K)
          CONT_ELEMENT(I, 1, J) = X(1, NI)  ! X
          CONT_ELEMENT(I, 2, J) = X(2, NI)  ! Y
          CONT_ELEMENT(I, 3, J) = X(3, NI)  ! Z
          J = J + 1
        END DO
      END DO

      DO I = 1, COUNT
        J = 5
        DO K = 2, 5
          NI = CAND_SEC_SEG_ID(I, K)
          CONT_ELEMENT(I, 1, J) = X(1, NI)  ! X
          CONT_ELEMENT(I, 2, J) = X(2, NI)  ! Y
          CONT_ELEMENT(I, 3, J) = X(3, NI)  ! Z
          J = J + 1
        END DO
      END DO

      CALL STS_REMAP_DEALLOCATE_WORK()

      RETURN
      CONTAINS

      !=======================================================================
      ! STS_REMAP_TRY_ADD_PAIR
      !
      ! Try to add a segment pair to the candidate list.
      !=======================================================================
!||====================================================================
!||    sts_remap_try_add_pair            ../engine/source/interfaces/ists/ists_remap_segments.F90
!||--- called by ------------------------------------------------------
!||    sts_remap_add_sec_segs_for_node   ../engine/source/interfaces/ists/ists_remap_segments.F90
!||    sts_remap_segments                ../engine/source/interfaces/ists/ists_remap_segments.F90
!||====================================================================
        SUBROUTINE STS_REMAP_TRY_ADD_PAIR(SEC_SEG_IN, MST_SEG_IN, &
     &    COUNT_INOUT, CAND_SEC, CAND_MST, CAPACITY, PAIR_ADDED, &
     &    PAIR_INDEX)
          INTEGER, INTENT(IN) :: SEC_SEG_IN, MST_SEG_IN, CAPACITY
          INTEGER, INTENT(INOUT) :: COUNT_INOUT
          INTEGER, INTENT(INOUT) :: CAND_SEC(CAPACITY)
          INTEGER, INTENT(INOUT) :: CAND_MST(CAPACITY)
          LOGICAL, INTENT(INOUT) :: PAIR_ADDED
          INTEGER, INTENT(INOUT) :: PAIR_INDEX
          INTEGER :: IDX, PROBE
          INTEGER(KIND=8) :: HKEY

          PAIR_ADDED = .FALSE.
          PAIR_INDEX = 0
          IF (SEC_SEG_IN <= 0 .OR. MST_SEG_IN <= 0) RETURN

          HKEY = INT(SEC_SEG_IN, KIND=8) * INT(1000003, KIND=8) + &
     &      INT(MST_SEG_IN, KIND=8)
          IDX = INT(MOD(HKEY, INT(PAIR_HASH_SIZE, KIND=8))) + 1
          DO PROBE = 1, PAIR_HASH_SIZE
            IF (PAIR_HASH_INDEX(IDX) == 0) EXIT
            IF (PAIR_HASH_SEC(IDX) == SEC_SEG_IN .AND. &
     &          PAIR_HASH_MST(IDX) == MST_SEG_IN) THEN
              PAIR_ADDED = .TRUE.
              PAIR_INDEX = PAIR_HASH_INDEX(IDX)
              RETURN
            ENDIF
            IDX = IDX + 1
            IF (IDX > PAIR_HASH_SIZE) IDX = 1
          ENDDO

          IF (COUNT_INOUT >= CAPACITY) RETURN

          COUNT_INOUT = COUNT_INOUT + 1
          CAND_SEC(COUNT_INOUT) = SEC_SEG_IN
          CAND_MST(COUNT_INOUT) = MST_SEG_IN
          PAIR_HASH_SEC(IDX) = SEC_SEG_IN
          PAIR_HASH_MST(IDX) = MST_SEG_IN
          PAIR_HASH_INDEX(IDX) = COUNT_INOUT
          PAIR_INDEX = COUNT_INOUT
          PAIR_ADDED = .TRUE.
        END SUBROUTINE STS_REMAP_TRY_ADD_PAIR

        !=======================================================================
        ! STS_REMAP_ADD_SEC_SEGS_FOR_NODE_PATCH
        !
        ! INT7 stores one master segment per active secondary node. STS projects
        ! all secondary Lobatto points of the remapped segment, so adjacent
        ! master facets are required when the projected point lies across the
        ! original INT7 facet edge.
        !=======================================================================
!||====================================================================
!||    sts_remap_add_sec_segs_for_node_patch   ../engine/source/interfaces/ists/ists_remap_segments.F90
!||--- called by ------------------------------------------------------
!||    sts_remap_segments                      ../engine/source/interfaces/ists/ists_remap_segments.F90
!||--- calls      -----------------------------------------------------
!||    sts_remap_add_sec_segs_for_node         ../engine/source/interfaces/ists/ists_remap_segments.F90
!||====================================================================
        SUBROUTINE STS_REMAP_ADD_SEC_SEGS_FOR_NODE_PATCH( &
     &    NSEG_IN, SEC_NODE_IN, MST_SEG_IN, &
     &    SEC_COUNT, SEC_OFF, SEC_LIST, MST_NEI_COUNT, MST_NEI_OFF, &
     &    MST_NEI_LIST, COUNT_INOUT, CAND_SEC, CAND_MST, GP_MASK, &
     &    CAPACITY, FOUND_ANY, CAPACITY_FULL)
          INTEGER, INTENT(IN) :: NSEG_IN, SEC_NODE_IN, MST_SEG_IN
          INTEGER, INTENT(IN) :: CAPACITY
          INTEGER, INTENT(IN) :: SEC_COUNT(:), SEC_OFF(:), SEC_LIST(:)
          INTEGER, INTENT(IN) :: MST_NEI_COUNT(:), MST_NEI_OFF(:)
          INTEGER, INTENT(IN) :: MST_NEI_LIST(:)
          INTEGER, INTENT(INOUT) :: COUNT_INOUT
          INTEGER, INTENT(INOUT) :: CAND_SEC(CAPACITY)
          INTEGER, INTENT(INOUT) :: CAND_MST(CAPACITY)
          INTEGER, INTENT(INOUT) :: GP_MASK(CAPACITY, 4)
          LOGICAL, INTENT(INOUT) :: FOUND_ANY, CAPACITY_FULL
          INTEGER :: MSEG, P, P0, P1
          LOGICAL :: FOUND_LOCAL, CAPACITY_LOCAL

          FOUND_ANY = .FALSE.
          CAPACITY_FULL = .FALSE.
          IF (MST_SEG_IN <= 0 .OR. MST_SEG_IN > SIZE(MST_NEI_COUNT)) RETURN

          CALL STS_REMAP_ADD_SEC_SEGS_FOR_NODE( &
     &      NSEG_IN, SEC_NODE_IN, MST_SEG_IN, SEC_COUNT, SEC_OFF, &
     &      SEC_LIST, COUNT_INOUT, CAND_SEC, CAND_MST, GP_MASK, &
     &      CAPACITY, FOUND_LOCAL, CAPACITY_LOCAL)
          FOUND_ANY = FOUND_ANY .OR. FOUND_LOCAL
          IF (CAPACITY_LOCAL) THEN
            CAPACITY_FULL = .TRUE.
            RETURN
          ENDIF

          IF (.NOT. EXPAND_MASTER_PATCH) RETURN

          P0 = MST_NEI_OFF(MST_SEG_IN)
          P1 = MST_NEI_OFF(MST_SEG_IN + 1) - 1
          DO P = P0, P1
            MSEG = MST_NEI_LIST(P)
            CALL STS_REMAP_ADD_SEC_SEGS_FOR_NODE( &
     &        NSEG_IN, SEC_NODE_IN, MSEG, SEC_COUNT, SEC_OFF, &
     &        SEC_LIST, COUNT_INOUT, CAND_SEC, CAND_MST, GP_MASK, &
     &        CAPACITY, FOUND_LOCAL, CAPACITY_LOCAL)
            FOUND_ANY = FOUND_ANY .OR. FOUND_LOCAL
            IF (CAPACITY_LOCAL) THEN
              CAPACITY_FULL = .TRUE.
              EXIT
            ENDIF
          ENDDO
        END SUBROUTINE STS_REMAP_ADD_SEC_SEGS_FOR_NODE_PATCH

        !=======================================================================
        ! STS_REMAP_ADD_SEC_SEGS_FOR_NODE
        !
        ! A legacy INT7 bucket hit is node-based.  For STS this node represents
        ! every secondary surface segment sharing it; keeping only one segment
        ! makes the integrated contact patch too sparse on curved surfaces.
        !=======================================================================
!||====================================================================
!||    sts_remap_add_sec_segs_for_node         ../engine/source/interfaces/ists/ists_remap_segments.F90
!||--- called by ------------------------------------------------------
!||    sts_remap_add_sec_segs_for_node_patch   ../engine/source/interfaces/ists/ists_remap_segments.F90
!||--- calls      -----------------------------------------------------
!||    sts_remap_try_add_pair                  ../engine/source/interfaces/ists/ists_remap_segments.F90
!||====================================================================
        SUBROUTINE STS_REMAP_ADD_SEC_SEGS_FOR_NODE( &
     &    NSEG_IN, SEC_NODE_IN, MST_SEG_IN, SEC_COUNT, SEC_OFF, &
     &    SEC_LIST, COUNT_INOUT, CAND_SEC, CAND_MST, GP_MASK, &
     &    CAPACITY, FOUND_ANY, CAPACITY_FULL)
          INTEGER, INTENT(IN) :: NSEG_IN, SEC_NODE_IN, MST_SEG_IN
          INTEGER, INTENT(IN) :: CAPACITY
          INTEGER, INTENT(IN) :: SEC_COUNT(:), SEC_OFF(:), SEC_LIST(:)
          INTEGER, INTENT(INOUT) :: COUNT_INOUT
          INTEGER, INTENT(INOUT) :: CAND_SEC(CAPACITY)
          INTEGER, INTENT(INOUT) :: CAND_MST(CAPACITY)
          INTEGER, INTENT(INOUT) :: GP_MASK(CAPACITY, 4)
          LOGICAL, INTENT(INOUT) :: FOUND_ANY, CAPACITY_FULL
          INTEGER :: J, P, P0, P1, PAIR_INDEX
          LOGICAL :: PAIR_ADDED

          FOUND_ANY = .FALSE.
          CAPACITY_FULL = .FALSE.
          IF (MST_SEG_IN <= 0) RETURN
          IF (SEC_NODE_IN <= 0) RETURN
          IF (SEC_NODE_IN > SIZE(SEC_COUNT)) RETURN
          IF (NSEG_IN <= 0) RETURN
          IF (SEC_COUNT(SEC_NODE_IN) <= 0) RETURN

          P0 = SEC_OFF(SEC_NODE_IN)
          P1 = SEC_OFF(SEC_NODE_IN + 1) - 1
          DO P = P0, P1
            J = SEC_LIST(P)
            IF (J <= 0 .OR. J > NSEG_IN) CYCLE
            FOUND_ANY = .TRUE.
            CALL STS_REMAP_TRY_ADD_PAIR(J, MST_SEG_IN, COUNT_INOUT, &
     &        CAND_SEC, CAND_MST, CAPACITY, PAIR_ADDED, PAIR_INDEX)
            IF (PAIR_ADDED .AND. PAIR_INDEX > 0) THEN
!             INT7 gives a node hit, but STS integrates segment pairs.
!             Activating only the matching corner under-integrates curved
!             contact patches and can let bodies pass through.
              GP_MASK(PAIR_INDEX, 1:4) = 1
            ELSE
              CAPACITY_FULL = .TRUE.
              EXIT
            END IF
          END DO
        END SUBROUTINE STS_REMAP_ADD_SEC_SEGS_FOR_NODE

        !=======================================================================
        ! STS_REMAP_BUILD_NODE_SEG_ADJ
        !
        ! Build a compressed node-to-secondary-segment adjacency table.
        !=======================================================================
!||====================================================================
!||    sts_remap_build_node_seg_adj   ../engine/source/interfaces/ists/ists_remap_segments.F90
!||--- called by ------------------------------------------------------
!||    sts_remap_segments             ../engine/source/interfaces/ists/ists_remap_segments.F90
!||--- calls      -----------------------------------------------------
!||====================================================================
        SUBROUTINE STS_REMAP_BUILD_NODE_SEG_ADJ(SEG_NODES, NSEG_IN, &
     &    NUMNOD_IN, NODE_COUNT, NODE_OFF, NODE_LIST)
          INTEGER, INTENT(IN) :: NSEG_IN, NUMNOD_IN
          INTEGER, INTENT(IN) :: SEG_NODES(NSEG_IN, 4)
          INTEGER, ALLOCATABLE, INTENT(INOUT) :: NODE_COUNT(:)
          INTEGER, ALLOCATABLE, INTENT(INOUT) :: NODE_OFF(:)
          INTEGER, ALLOCATABLE, INTENT(INOUT) :: NODE_LIST(:)
          INTEGER :: SEG, C, NID, TOTAL, POS

          IF (ALLOCATED(NODE_COUNT)) DEALLOCATE(NODE_COUNT)
          IF (ALLOCATED(NODE_OFF))   DEALLOCATE(NODE_OFF)
          IF (ALLOCATED(NODE_LIST))  DEALLOCATE(NODE_LIST)
          CALL MY_ALLOC(NODE_COUNT, NUMNOD_IN, "NODE_COUNT")
          CALL MY_ALLOC(NODE_OFF, NUMNOD_IN + 1, "NODE_OFF")
          NODE_COUNT = 0

          DO SEG = 1, NSEG_IN
            DO C = 1, 4
              NID = SEG_NODES(SEG, C)
              IF (NID <= 0 .OR. NID > NUMNOD_IN) CYCLE
              NODE_COUNT(NID) = NODE_COUNT(NID) + 1
            ENDDO
          ENDDO

          NODE_OFF(1) = 1
          DO NID = 1, NUMNOD_IN
            NODE_OFF(NID + 1) = NODE_OFF(NID) + NODE_COUNT(NID)
          ENDDO
          TOTAL = NODE_OFF(NUMNOD_IN + 1) - 1
          CALL MY_ALLOC(NODE_LIST, MAX(1, TOTAL), "NODE_LIST")

          NODE_COUNT = 0
          DO SEG = 1, NSEG_IN
            DO C = 1, 4
              NID = SEG_NODES(SEG, C)
              IF (NID <= 0 .OR. NID > NUMNOD_IN) CYCLE
              POS = NODE_OFF(NID) + NODE_COUNT(NID)
              NODE_LIST(POS) = SEG
              NODE_COUNT(NID) = NODE_COUNT(NID) + 1
            ENDDO
          ENDDO
        END SUBROUTINE STS_REMAP_BUILD_NODE_SEG_ADJ

        !=======================================================================
        ! STS_REMAP_BUILD_MASTER_NODE_ADJ
        !
        ! Build a compressed node-to-master-segment adjacency table.
        !=======================================================================
!||====================================================================
!||    sts_remap_build_master_node_adj   ../engine/source/interfaces/ists/ists_remap_segments.F90
!||--- called by ------------------------------------------------------
!||    sts_remap_segments                ../engine/source/interfaces/ists/ists_remap_segments.F90
!||--- calls      -----------------------------------------------------
!||====================================================================
        SUBROUTINE STS_REMAP_BUILD_MASTER_NODE_ADJ(IRECT_IN, NRTM_IN, &
     &    NUMNOD_IN, NODE_COUNT, NODE_OFF, NODE_LIST)
          INTEGER, INTENT(IN) :: NRTM_IN, NUMNOD_IN
          INTEGER, INTENT(IN) :: IRECT_IN(4, NRTM_IN)
          INTEGER, ALLOCATABLE, INTENT(INOUT) :: NODE_COUNT(:)
          INTEGER, ALLOCATABLE, INTENT(INOUT) :: NODE_OFF(:)
          INTEGER, ALLOCATABLE, INTENT(INOUT) :: NODE_LIST(:)
          INTEGER :: SEG, C, NID, TOTAL, POS

          IF (ALLOCATED(NODE_COUNT)) DEALLOCATE(NODE_COUNT)
          IF (ALLOCATED(NODE_OFF))   DEALLOCATE(NODE_OFF)
          IF (ALLOCATED(NODE_LIST))  DEALLOCATE(NODE_LIST)
          CALL MY_ALLOC(NODE_COUNT, NUMNOD_IN, "NODE_COUNT")
          CALL MY_ALLOC(NODE_OFF, NUMNOD_IN + 1, "NODE_OFF")
          NODE_COUNT = 0

          DO SEG = 1, NRTM_IN
            DO C = 1, 4
              NID = IRECT_IN(C, SEG)
              IF (NID <= 0 .OR. NID > NUMNOD_IN) CYCLE
              NODE_COUNT(NID) = NODE_COUNT(NID) + 1
            ENDDO
          ENDDO

          NODE_OFF(1) = 1
          DO NID = 1, NUMNOD_IN
            NODE_OFF(NID + 1) = NODE_OFF(NID) + NODE_COUNT(NID)
          ENDDO
          TOTAL = NODE_OFF(NUMNOD_IN + 1) - 1
          CALL MY_ALLOC(NODE_LIST, MAX(1, TOTAL), "NODE_LIST")

          NODE_COUNT = 0
          DO SEG = 1, NRTM_IN
            DO C = 1, 4
              NID = IRECT_IN(C, SEG)
              IF (NID <= 0 .OR. NID > NUMNOD_IN) CYCLE
              POS = NODE_OFF(NID) + NODE_COUNT(NID)
              NODE_LIST(POS) = SEG
              NODE_COUNT(NID) = NODE_COUNT(NID) + 1
            ENDDO
          ENDDO
        END SUBROUTINE STS_REMAP_BUILD_MASTER_NODE_ADJ

        !=======================================================================
        ! STS_REMAP_BUILD_MASTER_NEIGHBORS
        !
        ! Build sorted master-segment neighbor lists by shared corner nodes.
        !=======================================================================
!||====================================================================
!||    sts_remap_build_master_neighbors   ../engine/source/interfaces/ists/ists_remap_segments.F90
!||--- called by ------------------------------------------------------
!||    sts_remap_segments                 ../engine/source/interfaces/ists/ists_remap_segments.F90
!||--- calls      -----------------------------------------------------
!||    sts_remap_sort_int                 ../engine/source/interfaces/ists/ists_remap_segments.F90
!||====================================================================
        SUBROUTINE STS_REMAP_BUILD_MASTER_NEIGHBORS(IRECT_IN, NRTM_IN, &
     &    NODE_COUNT, NODE_OFF, NODE_LIST, SEG_COUNT, SEG_OFF, SEG_LIST)
          INTEGER, INTENT(IN) :: NRTM_IN
          INTEGER, INTENT(IN) :: IRECT_IN(4, NRTM_IN)
          INTEGER, INTENT(IN) :: NODE_COUNT(:), NODE_OFF(:), NODE_LIST(:)
          INTEGER, ALLOCATABLE, INTENT(INOUT) :: SEG_COUNT(:)
          INTEGER, ALLOCATABLE, INTENT(INOUT) :: SEG_OFF(:)
          INTEGER, ALLOCATABLE, INTENT(INOUT) :: SEG_LIST(:)
          INTEGER, ALLOCATABLE :: MARK(:), TMP(:)
          INTEGER :: SEG, C, NID, P, P0, P1, OTHER, TOTAL, NFOUND, ILOC

          IF (ALLOCATED(SEG_COUNT)) DEALLOCATE(SEG_COUNT)
          IF (ALLOCATED(SEG_OFF))   DEALLOCATE(SEG_OFF)
          IF (ALLOCATED(SEG_LIST))  DEALLOCATE(SEG_LIST)
          CALL MY_ALLOC(SEG_COUNT, NRTM_IN, "SEG_COUNT")
          CALL MY_ALLOC(SEG_OFF, NRTM_IN + 1, "SEG_OFF")
          CALL MY_ALLOC(MARK, NRTM_IN, "MARK")
          CALL MY_ALLOC(TMP, MAX(1, NRTM_IN), "TMP")
          SEG_COUNT = 0
          MARK = 0

          DO SEG = 1, NRTM_IN
            NFOUND = 0
            DO C = 1, 4
              NID = IRECT_IN(C, SEG)
              IF (NID <= 0 .OR. NID > SIZE(NODE_COUNT)) CYCLE
              IF (NODE_COUNT(NID) <= 0) CYCLE
              P0 = NODE_OFF(NID)
              P1 = NODE_OFF(NID + 1) - 1
              DO P = P0, P1
                OTHER = NODE_LIST(P)
                IF (OTHER <= 0 .OR. OTHER > NRTM_IN) CYCLE
                IF (OTHER == SEG) CYCLE
                IF (MARK(OTHER) == SEG) CYCLE
                MARK(OTHER) = SEG
                NFOUND = NFOUND + 1
              ENDDO
            ENDDO
            SEG_COUNT(SEG) = NFOUND
          ENDDO

          SEG_OFF(1) = 1
          DO SEG = 1, NRTM_IN
            SEG_OFF(SEG + 1) = SEG_OFF(SEG) + SEG_COUNT(SEG)
          ENDDO
          TOTAL = SEG_OFF(NRTM_IN + 1) - 1
          CALL MY_ALLOC(SEG_LIST, MAX(1, TOTAL), "SEG_LIST")

          MARK = 0
          DO SEG = 1, NRTM_IN
            NFOUND = 0
            DO C = 1, 4
              NID = IRECT_IN(C, SEG)
              IF (NID <= 0 .OR. NID > SIZE(NODE_COUNT)) CYCLE
              IF (NODE_COUNT(NID) <= 0) CYCLE
              P0 = NODE_OFF(NID)
              P1 = NODE_OFF(NID + 1) - 1
              DO P = P0, P1
                OTHER = NODE_LIST(P)
                IF (OTHER <= 0 .OR. OTHER > NRTM_IN) CYCLE
                IF (OTHER == SEG) CYCLE
                IF (MARK(OTHER) == SEG) CYCLE
                MARK(OTHER) = SEG
                NFOUND = NFOUND + 1
                TMP(NFOUND) = OTHER
              ENDDO
            ENDDO
            CALL STS_REMAP_SORT_INT(TMP, NFOUND)
            DO ILOC = 1, NFOUND
              SEG_LIST(SEG_OFF(SEG) + ILOC - 1) = TMP(ILOC)
            ENDDO
          ENDDO

          CALL MY_DEALLOC(MARK)
          CALL MY_DEALLOC(TMP)
        END SUBROUTINE STS_REMAP_BUILD_MASTER_NEIGHBORS

        !=======================================================================
        ! STS_REMAP_SORT_INT
        !
        ! Sort a short integer work array in ascending order.
        !=======================================================================
!||====================================================================
!||    sts_remap_sort_int                 ../engine/source/interfaces/ists/ists_remap_segments.F90
!||--- called by ------------------------------------------------------
!||    sts_remap_build_master_neighbors   ../engine/source/interfaces/ists/ists_remap_segments.F90
!||====================================================================
        SUBROUTINE STS_REMAP_SORT_INT(ARR, N)
          INTEGER, INTENT(INOUT) :: ARR(:)
          INTEGER, INTENT(IN) :: N
          INTEGER :: ILOC, JLOC, KEY

          DO ILOC = 2, N
            KEY = ARR(ILOC)
            JLOC = ILOC - 1
            DO WHILE (JLOC >= 1 .AND. ARR(JLOC) > KEY)
              ARR(JLOC + 1) = ARR(JLOC)
              JLOC = JLOC - 1
            ENDDO
            ARR(JLOC + 1) = KEY
          ENDDO
        END SUBROUTINE STS_REMAP_SORT_INT

        !=======================================================================
        ! STS_REMAP_DEALLOCATE_WORK
        !
        ! Release per-call hash and nearest-segment work arrays.
        !=======================================================================
!||====================================================================
!||    sts_remap_deallocate_work   ../engine/source/interfaces/ists/ists_remap_segments.F90
!||--- called by ------------------------------------------------------
!||    sts_remap_segments          ../engine/source/interfaces/ists/ists_remap_segments.F90
!||--- calls      -----------------------------------------------------
!||====================================================================
        SUBROUTINE STS_REMAP_DEALLOCATE_WORK()
          IF (ALLOCATED(PAIR_HASH_SEC)) CALL MY_DEALLOC(PAIR_HASH_SEC)
          IF (ALLOCATED(PAIR_HASH_MST)) CALL MY_DEALLOC(PAIR_HASH_MST)
          IF (ALLOCATED(PAIR_HASH_INDEX)) CALL MY_DEALLOC(PAIR_HASH_INDEX)
          IF (ALLOCATED(NEAREST_SEC_CACHE)) CALL MY_DEALLOC(NEAREST_SEC_CACHE)
        END SUBROUTINE STS_REMAP_DEALLOCATE_WORK

        !=======================================================================
        ! STS_REMAP_CLEAR_TOPO_CACHE
        !
        ! Release cached surface topology adjacency tables.
        !=======================================================================
!||====================================================================
!||    sts_remap_clear_topo_cache   ../engine/source/interfaces/ists/ists_remap_segments.F90
!||--- called by ------------------------------------------------------
!||    sts_remap_segments           ../engine/source/interfaces/ists/ists_remap_segments.F90
!||--- calls      -----------------------------------------------------
!||====================================================================
        SUBROUTINE STS_REMAP_CLEAR_TOPO_CACHE()
          IF (ALLOCATED(IGRSURF_S_TEMP)) CALL MY_DEALLOC(IGRSURF_S_TEMP)
          IF (ALLOCATED(SEC_NODE_SEG_COUNT)) CALL MY_DEALLOC(SEC_NODE_SEG_COUNT)
          IF (ALLOCATED(SEC_NODE_SEG_OFF)) CALL MY_DEALLOC(SEC_NODE_SEG_OFF)
          IF (ALLOCATED(SEC_NODE_SEG_LIST)) CALL MY_DEALLOC(SEC_NODE_SEG_LIST)
          IF (ALLOCATED(MST_NODE_SEG_COUNT)) CALL MY_DEALLOC(MST_NODE_SEG_COUNT)
          IF (ALLOCATED(MST_NODE_SEG_OFF)) CALL MY_DEALLOC(MST_NODE_SEG_OFF)
          IF (ALLOCATED(MST_NODE_SEG_LIST)) CALL MY_DEALLOC(MST_NODE_SEG_LIST)
          IF (ALLOCATED(MST_SEG_NEI_COUNT)) CALL MY_DEALLOC(MST_SEG_NEI_COUNT)
          IF (ALLOCATED(MST_SEG_NEI_OFF)) CALL MY_DEALLOC(MST_SEG_NEI_OFF)
          IF (ALLOCATED(MST_SEG_NEI_LIST)) CALL MY_DEALLOC(MST_SEG_NEI_LIST)
          TOPO_CACHE_READY = .FALSE.
        END SUBROUTINE STS_REMAP_CLEAR_TOPO_CACHE

        !=======================================================================
        ! STS_REMAP_NEAREST_SEC_SEG
        !
        ! Find the secondary segment that is closest to the main segment centroid.
        !=======================================================================
!||====================================================================
!||    sts_remap_nearest_sec_seg   ../engine/source/interfaces/ists/ists_remap_segments.F90
!||--- called by ------------------------------------------------------
!||    sts_remap_segments          ../engine/source/interfaces/ists/ists_remap_segments.F90
!||====================================================================
        INTEGER FUNCTION STS_REMAP_NEAREST_SEC_SEG(IGRSURF_NODES, &
     &    NSEG_IN, IRECT_IN, NRTM_IN, MST_SEG_IN, NUMNOD_IN, X_IN)
          INTEGER, INTENT(IN) :: NSEG_IN, NRTM_IN, MST_SEG_IN, NUMNOD_IN
          INTEGER, INTENT(IN) :: IGRSURF_NODES(NSEG_IN, 4)
          INTEGER, INTENT(IN) :: IRECT_IN(4, NRTM_IN)
          real(kind=WP), INTENT(IN) :: X_IN(3, NUMNOD_IN)
          INTEGER :: J, K, NID, NC
          real(kind=WP) :: XM(3), XS(3), DIST2, BEST

          STS_REMAP_NEAREST_SEC_SEG = 0
          IF (MST_SEG_IN <= 0 .OR. MST_SEG_IN > NRTM_IN) RETURN
          IF (NSEG_IN <= 0) RETURN

          XM = ZERO
          NC = 0
          DO K = 1, 4
            NID = IRECT_IN(K, MST_SEG_IN)
            IF (NID <= 0 .OR. NID > NUMNOD_IN) CYCLE
            NC = NC + 1
            XM(1) = XM(1) + X_IN(1, NID)
            XM(2) = XM(2) + X_IN(2, NID)
            XM(3) = XM(3) + X_IN(3, NID)
          END DO
          IF (NC <= 0) RETURN
          XM(1) = XM(1) / NC
          XM(2) = XM(2) / NC
          XM(3) = XM(3) / NC

          BEST = HUGE(BEST)
          DO J = 1, NSEG_IN
            XS = ZERO
            NC = 0
            DO K = 1, 4
              NID = IGRSURF_NODES(J, K)
              IF (NID <= 0 .OR. NID > NUMNOD_IN) CYCLE
              NC = NC + 1
              XS(1) = XS(1) + X_IN(1, NID)
              XS(2) = XS(2) + X_IN(2, NID)
              XS(3) = XS(3) + X_IN(3, NID)
            END DO
            IF (NC <= 0) CYCLE
            XS(1) = XS(1) / NC
            XS(2) = XS(2) / NC
            XS(3) = XS(3) / NC
            DIST2 = (XS(1) - XM(1))**2 + (XS(2) - XM(2))**2 + &
     &              (XS(3) - XM(3))**2
            IF (DIST2 < BEST) THEN
              BEST = DIST2
              STS_REMAP_NEAREST_SEC_SEG = J
            END IF
          END DO
        END FUNCTION STS_REMAP_NEAREST_SEC_SEG

      END SUBROUTINE STS_REMAP_SEGMENTS
