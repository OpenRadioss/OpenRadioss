!||====================================================================
!||    ists_sts_capacity_mod  ../engine/source/interfaces/ists/ists_sts_capacity_mod.F90
!||--- called by ------------------------------------------------------
!||    ists_mainf              ../engine/source/interfaces/ists/ists_mainf.F
!||====================================================================
      MODULE ISTS_STS_CAPACITY_MOD
#include      "my_real.inc"
        IMPLICIT NONE
        PRIVATE
        PUBLIC :: ISTS_STS_INIT_CAPACITY
        PUBLIC :: ISTS_STS_ENSURE_BUFFERS
        PUBLIC :: ISTS_STS_TRY_GROW_CAPACITY

      CONTAINS

!=======================================================================
!   ISTS_STS_INIT_CAPACITY
!   Initial STS pair-buffer capacity and hard cap from segment counts.
!=======================================================================
        SUBROUTINE ISTS_STS_INIT_CAPACITY(NSEG_SEC, NSEG_MST, &
     &    STS_WB_CAPACITY, MAX_STS_SIZE_ACTUAL, STS_CAP_LIMIT)
          INTEGER, INTENT(IN) :: NSEG_SEC, NSEG_MST
          INTEGER, INTENT(IN) :: STS_WB_CAPACITY
          INTEGER, INTENT(OUT) :: MAX_STS_SIZE_ACTUAL, STS_CAP_LIMIT
          INTEGER :: CAP_INIT
          INTEGER, PARAMETER :: ISTS_STS_CAP_MIN_INIT = 10

          STS_CAP_LIMIT = NSEG_SEC * NSEG_MST

          CAP_INIT = MAX(ISTS_STS_CAP_MIN_INIT, MIN(NSEG_SEC, NSEG_MST))

          MAX_STS_SIZE_ACTUAL = MIN(CAP_INIT, STS_CAP_LIMIT)

          IF (STS_WB_CAPACITY > MAX_STS_SIZE_ACTUAL) THEN
            MAX_STS_SIZE_ACTUAL = MIN(STS_WB_CAPACITY, STS_CAP_LIMIT)
          ENDIF
        END SUBROUTINE ISTS_STS_INIT_CAPACITY

!=======================================================================
!   ISTS_STS_ENSURE_BUFFERS
!   (Re)allocate persistent STS working buffers when capacity grows.
!=======================================================================
        SUBROUTINE ISTS_STS_ENSURE_BUFFERS(CAPACITY, WB_CAPACITY, &
     &    CONT_ELEMENT, load_arr, STS_STIF, &
     &    CAND_SEC_SEG_ID, CAND_MST_SEG_ID, CAND_SEC_GP_MASK, &
     &    node_id_load, STS_IFPEN)
          use my_alloc_mod, only : my_alloc
          use my_dealloc_mod, only : my_dealloc
          INTEGER, INTENT(IN) :: CAPACITY
          INTEGER, INTENT(INOUT) :: WB_CAPACITY
          my_real, ALLOCATABLE, INTENT(INOUT) :: CONT_ELEMENT(:,:,:)
          my_real, ALLOCATABLE, INTENT(INOUT) :: load_arr(:,:,:)
          my_real, ALLOCATABLE, INTENT(INOUT) :: STS_STIF(:)
          INTEGER, ALLOCATABLE, INTENT(INOUT) :: CAND_SEC_SEG_ID(:,:)
          INTEGER, ALLOCATABLE, INTENT(INOUT) :: CAND_MST_SEG_ID(:,:)
          INTEGER, ALLOCATABLE, INTENT(INOUT) :: CAND_SEC_GP_MASK(:,:)
          INTEGER, ALLOCATABLE, INTENT(INOUT) :: node_id_load(:)
          INTEGER, ALLOCATABLE, INTENT(INOUT) :: STS_IFPEN(:)

          IF (.NOT. ALLOCATED(CONT_ELEMENT) .OR. &
     &        .NOT. ALLOCATED(STS_IFPEN) .OR. &
     &        WB_CAPACITY < CAPACITY) THEN
            IF (ALLOCATED(CONT_ELEMENT)) THEN
              CALL MY_DEALLOC(CONT_ELEMENT)
              CALL MY_DEALLOC(load_arr)
              CALL MY_DEALLOC(STS_STIF)
              CALL MY_DEALLOC(CAND_SEC_SEG_ID)
              CALL MY_DEALLOC(CAND_MST_SEG_ID)
              CALL MY_DEALLOC(CAND_SEC_GP_MASK)
              CALL MY_DEALLOC(node_id_load)
            ENDIF
            IF (ALLOCATED(STS_IFPEN)) CALL MY_DEALLOC(STS_IFPEN)
            CALL MY_ALLOC(CONT_ELEMENT, CAPACITY, 3, 8, "CONT_ELEMENT")
            CALL MY_ALLOC(load_arr, CAPACITY, 8, 4, "LOAD_ARR")
            CALL MY_ALLOC(STS_STIF, CAPACITY, "STS_STIF")
            CALL MY_ALLOC(CAND_SEC_SEG_ID, CAPACITY, 5, "CAND_SEC_SEG_ID")
            CALL MY_ALLOC(CAND_MST_SEG_ID, CAPACITY, 5, "CAND_MST_SEG_ID")
            CALL MY_ALLOC(CAND_SEC_GP_MASK, CAPACITY, 4, "CAND_SEC_GP_MASK")
            CALL MY_ALLOC(node_id_load, CAPACITY*8, "NODE_ID_LOAD")
            CALL MY_ALLOC(STS_IFPEN, CAPACITY, "STS_IFPEN")
            WB_CAPACITY = CAPACITY
          ENDIF
        END SUBROUTINE ISTS_STS_ENSURE_BUFFERS

!=======================================================================
!   ISTS_STS_TRY_GROW_CAPACITY
!   Double pair capacity on broad-phase overflow, up to STS_CAP_LIMIT.
!=======================================================================
        SUBROUTINE ISTS_STS_TRY_GROW_CAPACITY(BP_OVERFLOW, &
     &    MAX_STS_SIZE_ACTUAL, STS_CAP_LIMIT, RETRY)
          LOGICAL, INTENT(IN) :: BP_OVERFLOW
          INTEGER, INTENT(INOUT) :: MAX_STS_SIZE_ACTUAL
          INTEGER, INTENT(IN) :: STS_CAP_LIMIT
          LOGICAL, INTENT(OUT) :: RETRY

          RETRY = .FALSE.
          IF (.NOT. BP_OVERFLOW) RETURN
          IF (MAX_STS_SIZE_ACTUAL >= STS_CAP_LIMIT) RETURN
          MAX_STS_SIZE_ACTUAL = MIN(STS_CAP_LIMIT,&
     &        MAX_STS_SIZE_ACTUAL + MAX_STS_SIZE_ACTUAL)
          RETRY = .TRUE.
        END SUBROUTINE ISTS_STS_TRY_GROW_CAPACITY

      END MODULE ISTS_STS_CAPACITY_MOD
