!||====================================================================
!||    sts_gp_state_mod  ../engine/source/interfaces/ists/ists_gp_state_mod.F90
!||--------------------------------------------------------------------
!||  Global Gauss point state for STS contact (segment-keyed hash slots).
!||====================================================================
      MODULE sts_gp_state_mod

      USE my_alloc_mod, ONLY : my_alloc
      USE my_dealloc_mod, ONLY : my_dealloc

      IMPLICIT NONE

      INTEGER :: MAX_GLOBAL_GP = 0

      REAL*8 , DIMENSION(:), ALLOCATABLE, SAVE :: GP_XI1_GLOBAL
      REAL*8 , DIMENSION(:), ALLOCATABLE, SAVE :: GP_XI2_GLOBAL
      REAL*8 , DIMENSION(:), ALLOCATABLE, SAVE :: GP_XI1_GLOBAL_PREV
      REAL*8 , DIMENSION(:), ALLOCATABLE, SAVE :: GP_XI2_GLOBAL_PREV

      INTEGER, DIMENSION(:), ALLOCATABLE, SAVE :: GP_XI1_PERIOD
      INTEGER, DIMENSION(:), ALLOCATABLE, SAVE :: GP_XI2_PERIOD

      REAL*8 , DIMENSION(:), ALLOCATABLE, SAVE :: GP_TTRIAL1_HIST
      REAL*8 , DIMENSION(:), ALLOCATABLE, SAVE :: GP_TTRIAL2_HIST

      LOGICAL, DIMENSION(:), ALLOCATABLE, SAVE :: GP_IS_STICKING
      LOGICAL, DIMENSION(:), ALLOCATABLE, SAVE :: GP_INITIALIZED

      INTEGER, DIMENSION(:,:), ALLOCATABLE, SAVE :: GP_KEY_MST
      INTEGER, DIMENSION(:,:), ALLOCATABLE, SAVE :: GP_KEY_SEC
      INTEGER, DIMENSION(:),   ALLOCATABLE, SAVE :: GP_KEY_Z
      INTEGER, DIMENSION(:),   ALLOCATABLE, SAVE :: GP_KEY_Q
      INTEGER, DIMENSION(:),   ALLOCATABLE, SAVE :: GP_KEY_QUAD
      LOGICAL, DIMENSION(:),   ALLOCATABLE, SAVE :: GP_SLOT_OCCUPIED
      LOGICAL, DIMENSION(:),   ALLOCATABLE, SAVE :: GP_ACTIVE_CYCLE

      LOGICAL, SAVE :: STS_GP_TABLE_FULL_WARN_DONE = .FALSE.

      CONTAINS

      !=======================================================================
      !   STS_GP_DEALLOCATE_ALL
      !=======================================================================
      ! Deallocate all global GP state variables
      SUBROUTINE sts_gp_deallocate_all()
      IF (ALLOCATED(GP_XI1_GLOBAL)) THEN
        CALL MY_DEALLOC(GP_XI1_GLOBAL)
        CALL MY_DEALLOC(GP_XI2_GLOBAL)
        CALL MY_DEALLOC(GP_XI1_GLOBAL_PREV)
        CALL MY_DEALLOC(GP_XI2_GLOBAL_PREV)
        CALL MY_DEALLOC(GP_XI1_PERIOD)
        CALL MY_DEALLOC(GP_XI2_PERIOD)
        CALL MY_DEALLOC(GP_TTRIAL1_HIST)
        CALL MY_DEALLOC(GP_TTRIAL2_HIST)
        CALL MY_DEALLOC(GP_IS_STICKING)
        CALL MY_DEALLOC(GP_INITIALIZED)
        CALL MY_DEALLOC(GP_KEY_MST)
        CALL MY_DEALLOC(GP_KEY_SEC)
        CALL MY_DEALLOC(GP_KEY_Z)
        CALL MY_DEALLOC(GP_KEY_Q)
        CALL MY_DEALLOC(GP_KEY_QUAD)
        CALL MY_DEALLOC(GP_SLOT_OCCUPIED)
        CALL MY_DEALLOC(GP_ACTIVE_CYCLE)
      END IF
      END SUBROUTINE sts_gp_deallocate_all

      !=======================================================================
      !   STS_GP_CANONICAL_NODES
      !=======================================================================
      ! Canonicalize the node IDs for the current Gauss/Lobatto point
      ! This ensures that the node IDs are in the same order for all Gauss/Lobatto points
      SUBROUTINE sts_gp_canonical_nodes(NODES_IN, NODES_OUT)
      INTEGER, INTENT(IN)  :: NODES_IN(4)
      INTEGER, INTENT(OUT) :: NODES_OUT(4)
      INTEGER :: I, J, TMP
      NODES_OUT(:) = NODES_IN(:)
      DO I = 1, 3
        DO J = I + 1, 4
          IF (NODES_OUT(J) < NODES_OUT(I)) THEN
            TMP = NODES_OUT(I)
            NODES_OUT(I) = NODES_OUT(J)
            NODES_OUT(J) = TMP
          END IF
        END DO
      END DO
      END SUBROUTINE sts_gp_canonical_nodes

      !=======================================================================
      !   STS_GP_CANONICAL_PAIR_KEY
      !=======================================================================
      ! Canonicalize the pair key for the current Gauss/Lobatto point
      ! This ensures that the pair key is in the same order for all Gauss/Lobatto points
      SUBROUTINE sts_gp_canonical_pair_key(NODE_IDS, MST_KEY, SEC_KEY)
      INTEGER, INTENT(IN)  :: NODE_IDS(8)
      INTEGER, INTENT(OUT) :: MST_KEY(4), SEC_KEY(4)
      CALL sts_gp_canonical_nodes(NODE_IDS(1:4), MST_KEY)
      CALL sts_gp_canonical_nodes(NODE_IDS(5:8), SEC_KEY)
      END SUBROUTINE sts_gp_canonical_pair_key

      LOGICAL FUNCTION sts_gp_keys_match(SLOT, MST_KEY, SEC_KEY, Z, Q, QUAD)
      INTEGER, INTENT(IN) :: SLOT, MST_KEY(4), SEC_KEY(4), Z, Q, QUAD
      INTEGER :: I
      sts_gp_keys_match = .FALSE.
      IF (GP_KEY_Z(SLOT) /= Z) RETURN
      IF (GP_KEY_Q(SLOT) /= Q) RETURN
      IF (GP_KEY_QUAD(SLOT) /= QUAD) RETURN
      DO I = 1, 4
        IF (GP_KEY_MST(I, SLOT) /= MST_KEY(I)) RETURN
        IF (GP_KEY_SEC(I, SLOT) /= SEC_KEY(I)) RETURN
      END DO
      sts_gp_keys_match = .TRUE.
      END FUNCTION sts_gp_keys_match

      !=======================================================================
      !   STS_GP_STORE_KEY
      !=======================================================================
      ! Store the key for the current Gauss/Lobatto point
      ! This ensures that the key is stored in the same order for all Gauss/Lobatto points
      SUBROUTINE sts_gp_store_key(SLOT, MST_KEY, SEC_KEY, Z, Q, QUAD)
      INTEGER, INTENT(IN) :: SLOT, MST_KEY(4), SEC_KEY(4), Z, Q, QUAD
      INTEGER :: I
      DO I = 1, 4
        GP_KEY_MST(I, SLOT) = MST_KEY(I)
        GP_KEY_SEC(I, SLOT) = SEC_KEY(I)
      END DO
      GP_KEY_Z(SLOT) = Z
      GP_KEY_Q(SLOT) = Q
      GP_KEY_QUAD(SLOT) = QUAD
      END SUBROUTINE sts_gp_store_key

      !=======================================================================
      !   STS_GP_CLEAR_STATE
      !=======================================================================
      ! Clear the state for the current Gauss/Lobatto point
      ! This ensures that the state is cleared for all Gauss/Lobatto points
      SUBROUTINE sts_gp_clear_state(SLOT)
      INTEGER, INTENT(IN) :: SLOT
      IF (SLOT <= 0 .OR. SLOT > MAX_GLOBAL_GP) RETURN
      GP_XI1_GLOBAL(SLOT)       = 0.0D0
      GP_XI2_GLOBAL(SLOT)       = 0.0D0
      GP_XI1_GLOBAL_PREV(SLOT)  = 0.0D0
      GP_XI2_GLOBAL_PREV(SLOT)  = 0.0D0
      GP_XI1_PERIOD(SLOT)       = 0
      GP_XI2_PERIOD(SLOT)       = 0
      GP_TTRIAL1_HIST(SLOT)     = 0.0D0
      GP_TTRIAL2_HIST(SLOT)     = 0.0D0
      GP_IS_STICKING(SLOT)      = .FALSE.
      GP_INITIALIZED(SLOT)      = .FALSE.
      END SUBROUTINE sts_gp_clear_state

      !=======================================================================
      !   STS_GP_SLOT_HASH
      !=======================================================================
      ! Calculate the hash for the current Gauss/Lobatto point
      ! This ensures that the hash is calculated in the same way for all Gauss/Lobatto points
      INTEGER FUNCTION sts_gp_slot_hash(MST_KEY, SEC_KEY, Z, Q, QUAD, HASH_SIZE)
      INTEGER, INTENT(IN) :: MST_KEY(4), SEC_KEY(4), Z, Q, QUAD, HASH_SIZE
      INTEGER :: I
      INTEGER(KIND=8) :: H
      H = 0_8
      DO I = 1, 4
        H = MOD(H * INT(1315423911, KIND=8) + &
     &      INT(MST_KEY(I), KIND=8), INT(HASH_SIZE, KIND=8))
      END DO
      DO I = 1, 4
        H = MOD(H * INT(1315423911, KIND=8) + &
     &      INT(SEC_KEY(I), KIND=8), INT(HASH_SIZE, KIND=8))
      END DO
      H = MOD(H * INT(1315423911, KIND=8) + INT(Z, KIND=8), &
     &    INT(HASH_SIZE, KIND=8))
      H = MOD(H * INT(1315423911, KIND=8) + INT(Q, KIND=8), &
     &    INT(HASH_SIZE, KIND=8))
      H = MOD(H * INT(1315423911, KIND=8) + INT(QUAD, KIND=8), &
     &    INT(HASH_SIZE, KIND=8))
      sts_gp_slot_hash = INT(H) + 1
      END FUNCTION sts_gp_slot_hash

      !=======================================================================
      !   STS_GP_RESET_SLOT
      !=======================================================================
      ! Reset the slot for the current Gauss/Lobatto point
      ! This ensures that the slot is reset for all Gauss/Lobatto points
      SUBROUTINE sts_gp_reset_slot(GP_SLOT)
      INTEGER, INTENT(IN) :: GP_SLOT
      CALL sts_gp_clear_state(GP_SLOT)
      END SUBROUTINE sts_gp_reset_slot

      !=======================================================================
      !   STS_GP_ACQUIRE_SLOT
      !=======================================================================
      ! Acquire the slot for the current Gauss/Lobatto point
      ! This ensures that the slot is acquired for all Gauss/Lobatto points
      SUBROUTINE sts_gp_acquire_slot(MST_KEY, SEC_KEY, Z, Q, QUAD, GP_SLOT)
      INTEGER, INTENT(IN)  :: MST_KEY(4), SEC_KEY(4), Z, Q, QUAD
      INTEGER, INTENT(OUT) :: GP_SLOT
      INTEGER :: IH, PROBE, START_IH
      LOGICAL :: KEY_MATCH
      GP_SLOT = 0
      IF (MAX_GLOBAL_GP <= 0) RETURN
      START_IH = sts_gp_slot_hash(MST_KEY, SEC_KEY, Z, Q, QUAD, MAX_GLOBAL_GP)
      IH = START_IH
      PROBE = 0
      DO WHILE (PROBE < MAX_GLOBAL_GP)
        IF (.NOT. GP_SLOT_OCCUPIED(IH)) THEN
          CALL sts_gp_clear_state(IH)
          CALL sts_gp_store_key(IH, MST_KEY, SEC_KEY, Z, Q, QUAD)
          GP_SLOT_OCCUPIED(IH) = .TRUE.
          GP_ACTIVE_CYCLE(IH) = .TRUE.
          GP_SLOT = IH
          RETURN
        END IF
        KEY_MATCH = sts_gp_keys_match(IH, MST_KEY, SEC_KEY, Z, Q, QUAD)
        IF (KEY_MATCH) THEN
          GP_ACTIVE_CYCLE(IH) = .TRUE.
          GP_SLOT = IH
          RETURN
        END IF
        IH = IH + 1
        IF (IH > MAX_GLOBAL_GP) IH = 1
        PROBE = PROBE + 1
      END DO
      IF (.NOT. STS_GP_TABLE_FULL_WARN_DONE) THEN
        WRITE(*,'(A)') &
     &    ' WARNING: STS GP state table full; friction history skipped.'
        STS_GP_TABLE_FULL_WARN_DONE = .TRUE.
      END IF
      END SUBROUTINE sts_gp_acquire_slot

      !=======================================================================
      !   STS_GP_CYCLE_BEGIN
      !=======================================================================
      ! Begin the cycle for the current Gauss/Lobatto point
      ! This ensures that the cycle is begun for all Gauss/Lobatto points
      SUBROUTINE sts_gp_cycle_begin()
      IF (.NOT. ALLOCATED(GP_ACTIVE_CYCLE)) RETURN
      IF (MAX_GLOBAL_GP > 0) GP_ACTIVE_CYCLE = .FALSE.
      END SUBROUTINE sts_gp_cycle_begin

      !=======================================================================
      !   STS_GP_CYCLE_END
      !=======================================================================
      ! End the cycle for the current Gauss/Lobatto point
      ! This ensures that the cycle is ended for all Gauss/Lobatto points
      SUBROUTINE sts_gp_cycle_end()
      INTEGER :: SLOT
      IF (.NOT. ALLOCATED(GP_SLOT_OCCUPIED)) RETURN
      DO SLOT = 1, MAX_GLOBAL_GP
        IF (.NOT. GP_SLOT_OCCUPIED(SLOT)) CYCLE
        IF (GP_ACTIVE_CYCLE(SLOT)) CYCLE
        CALL sts_gp_clear_state(SLOT)
        GP_SLOT_OCCUPIED(SLOT) = .FALSE.
      END DO
      END SUBROUTINE sts_gp_cycle_end

      !=======================================================================
      !   STS_GP_STATE_INIT
      !=======================================================================
      ! Initialize the state for the current Gauss/Lobatto point
      ! This ensures that the state is initialized for all Gauss/Lobatto points
      SUBROUTINE sts_gp_state_init(MAX_STS_SIZE_ACTUAL, IP_MAX)
      INTEGER, INTENT(IN) :: MAX_STS_SIZE_ACTUAL, IP_MAX
      INTEGER :: NEW_MAX_GLOBAL_GP
      NEW_MAX_GLOBAL_GP = MAX_STS_SIZE_ACTUAL * IP_MAX * IP_MAX * 2
      IF (NEW_MAX_GLOBAL_GP <= 0) THEN
        CALL sts_gp_deallocate_all()
        MAX_GLOBAL_GP = 0
        RETURN
      END IF
      IF (.NOT. ALLOCATED(GP_XI1_GLOBAL) .OR. &
     &    NEW_MAX_GLOBAL_GP /= MAX_GLOBAL_GP) THEN
        CALL sts_gp_deallocate_all()
        MAX_GLOBAL_GP = NEW_MAX_GLOBAL_GP
        CALL MY_ALLOC(GP_XI1_GLOBAL, MAX_GLOBAL_GP, "GP_XI1_GLOBAL")
        CALL MY_ALLOC(GP_XI2_GLOBAL, MAX_GLOBAL_GP, "GP_XI2_GLOBAL")
        CALL MY_ALLOC(GP_XI1_GLOBAL_PREV, MAX_GLOBAL_GP, "GP_XI1_GLOBAL_PREV")
        CALL MY_ALLOC(GP_XI2_GLOBAL_PREV, MAX_GLOBAL_GP, "GP_XI2_GLOBAL_PREV")
        CALL MY_ALLOC(GP_XI1_PERIOD, MAX_GLOBAL_GP, "GP_XI1_PERIOD")
        CALL MY_ALLOC(GP_XI2_PERIOD, MAX_GLOBAL_GP, "GP_XI2_PERIOD")
        CALL MY_ALLOC(GP_TTRIAL1_HIST, MAX_GLOBAL_GP, "GP_TTRIAL1_HIST")
        CALL MY_ALLOC(GP_TTRIAL2_HIST, MAX_GLOBAL_GP, "GP_TTRIAL2_HIST")
        CALL MY_ALLOC(GP_IS_STICKING, MAX_GLOBAL_GP, "GP_IS_STICKING")
        CALL MY_ALLOC(GP_INITIALIZED, MAX_GLOBAL_GP, "GP_INITIALIZED")
        CALL MY_ALLOC(GP_KEY_MST, 4, MAX_GLOBAL_GP, "GP_KEY_MST")
        CALL MY_ALLOC(GP_KEY_SEC, 4, MAX_GLOBAL_GP, "GP_KEY_SEC")
        CALL MY_ALLOC(GP_KEY_Z, MAX_GLOBAL_GP, "GP_KEY_Z")
        CALL MY_ALLOC(GP_KEY_Q, MAX_GLOBAL_GP, "GP_KEY_Q")
        CALL MY_ALLOC(GP_KEY_QUAD, MAX_GLOBAL_GP, "GP_KEY_QUAD")
        CALL MY_ALLOC(GP_SLOT_OCCUPIED, MAX_GLOBAL_GP, "GP_SLOT_OCCUPIED")
        CALL MY_ALLOC(GP_ACTIVE_CYCLE, MAX_GLOBAL_GP, "GP_ACTIVE_CYCLE")
        GP_XI1_GLOBAL       = 0.0D0
        GP_XI2_GLOBAL       = 0.0D0
        GP_XI1_GLOBAL_PREV  = 0.0D0
        GP_XI2_GLOBAL_PREV  = 0.0D0
        GP_XI1_PERIOD       = 0
        GP_XI2_PERIOD       = 0
        GP_TTRIAL1_HIST     = 0.0D0
        GP_TTRIAL2_HIST     = 0.0D0
        GP_IS_STICKING      = .FALSE.
        GP_INITIALIZED      = .FALSE.
        GP_KEY_MST          = 0
        GP_KEY_SEC          = 0
        GP_KEY_Z            = 0
        GP_KEY_Q            = 0
        GP_KEY_QUAD         = 0
        GP_SLOT_OCCUPIED    = .FALSE.
        GP_ACTIVE_CYCLE     = .FALSE.
        STS_GP_TABLE_FULL_WARN_DONE = .FALSE.
      END IF
      END SUBROUTINE sts_gp_state_init

      END MODULE sts_gp_state_mod
