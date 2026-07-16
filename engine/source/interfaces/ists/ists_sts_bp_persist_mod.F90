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
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!||====================================================================
!||    ists_sts_bp_persist_mod  ../engine/source/interfaces/ists/ists_sts_bp_persist_mod.F90
!||--- called by ------------------------------------------------------
!||    sts_int7_bucket_broad_phase  ../engine/source/interfaces/ists/ists_broad_phase_int7_bucket.F90
!||====================================================================
!
!   Cache the last successful STS segment pairs for the INT7-bucket path.
!   When I7TRC invalidates all INT7 candidates (N_VALID=0) while the
!   surfaces are still within the STS search gap, reuse the cached pairs
!   with refreshed node coordinates so narrow phase can continue.
!
      MODULE ISTS_STS_BP_PERSIST_MOD
        USE PRECISION_MOD, ONLY : WP
        IMPLICIT NONE
        PRIVATE

        TYPE, PRIVATE :: STS_BP_PERSIST_SLOT
          LOGICAL :: ACTIVE = .FALSE.
          INTEGER :: COUNT_STORED = 0
          INTEGER :: LAST_SAVE_NCYCLE = 0
          INTEGER, ALLOCATABLE :: SEC_ID(:,:)
          INTEGER, ALLOCATABLE :: MST_ID(:,:)
          INTEGER, ALLOCATABLE :: SEC_GP_MASK(:,:)
        END TYPE STS_BP_PERSIST_SLOT

        INTEGER, SAVE :: PERSIST_NCYCLE = 0
!       Switch for the INT7-bucket pair cache. The cache is only used when
!       no fresh INT7 bucket remap is available for the current cycle.
        LOGICAL, PARAMETER :: STS_BP_PERSIST_ENABLED = .TRUE.
!       Keep the last healthy STS segment patch long enough to bridge
!       transient INT7 bucket candidate invalidation during deep contact.
!       The STS narrow phase remains the geometric filter for restored
!       pairs, so stale pairs can stay listed without necessarily loading.
        INTEGER, PARAMETER :: PERSIST_GRACE_CYCLES = 5000

        TYPE(STS_BP_PERSIST_SLOT), ALLOCATABLE, SAVE :: PERSIST_SLOT(:)

        PUBLIC :: ISTS_STS_BP_PERSIST_SAVE
        PUBLIC :: ISTS_STS_BP_PERSIST_TRY_RESTORE
        PUBLIC :: ISTS_STS_BP_PERSIST_CLEAR
        PUBLIC :: ISTS_STS_BP_PERSIST_SET_NCYCLE

      CONTAINS

        !=======================================================================
        ! ISTS_STS_BP_PERSIST_SET_NCYCLE
        !
        ! Set the current cycle number for the cached STS segment pairs.
        !=======================================================================
        SUBROUTINE ISTS_STS_BP_PERSIST_SET_NCYCLE(NCYCLE_IN)
          INTEGER, INTENT(IN) :: NCYCLE_IN

          IF (.NOT. STS_BP_PERSIST_ENABLED) RETURN
          PERSIST_NCYCLE = NCYCLE_IN
        END SUBROUTINE ISTS_STS_BP_PERSIST_SET_NCYCLE

        !=======================================================================
        ! ISTS_STS_BP_PERSIST_ENSURE_SIZE
        !
        ! Ensure the cached STS segment pairs array is large enough to store the given number of pairs.
        !=======================================================================
        SUBROUTINE ISTS_STS_BP_PERSIST_ENSURE_SIZE(NIN)
          INTEGER, INTENT(IN) :: NIN
          TYPE(STS_BP_PERSIST_SLOT), ALLOCATABLE :: TMP(:)
          INTEGER :: OLD_SIZE, I

          IF (.NOT. STS_BP_PERSIST_ENABLED) RETURN
          IF (NIN <= 0) RETURN
          IF (.NOT. ALLOCATED(PERSIST_SLOT)) THEN
            ALLOCATE(PERSIST_SLOT(NIN))
            DO I = 1, NIN
              PERSIST_SLOT(I)%ACTIVE = .FALSE.
              PERSIST_SLOT(I)%COUNT_STORED = 0
            END DO
            RETURN
          END IF
          OLD_SIZE = SIZE(PERSIST_SLOT)
          IF (NIN <= OLD_SIZE) RETURN
          ALLOCATE(TMP(NIN))
          TMP(1:OLD_SIZE) = PERSIST_SLOT(1:OLD_SIZE)
          DO I = OLD_SIZE + 1, NIN
            TMP(I)%ACTIVE = .FALSE.
            TMP(I)%COUNT_STORED = 0
          END DO
          DEALLOCATE(PERSIST_SLOT)
          CALL MOVE_ALLOC(TMP, PERSIST_SLOT)
        END SUBROUTINE ISTS_STS_BP_PERSIST_ENSURE_SIZE

        !=======================================================================
        ! ISTS_STS_BP_PERSIST_CLEAR
        !
        ! Clear the cached STS segment pairs for a given index.
        !=======================================================================
        SUBROUTINE ISTS_STS_BP_PERSIST_CLEAR(NIN)
          INTEGER, INTENT(IN) :: NIN

          IF (.NOT. STS_BP_PERSIST_ENABLED) RETURN
          IF (NIN <= 0) RETURN
          CALL ISTS_STS_BP_PERSIST_ENSURE_SIZE(NIN)
          PERSIST_SLOT(NIN)%ACTIVE = .FALSE.
          PERSIST_SLOT(NIN)%COUNT_STORED = 0
          PERSIST_SLOT(NIN)%LAST_SAVE_NCYCLE = 0
        END SUBROUTINE ISTS_STS_BP_PERSIST_CLEAR

        !=======================================================================
        ! ISTS_STS_BP_PERSIST_REFRESH_CONT
        !
        ! Refresh the contact element coordinates for the cached STS segment pairs.
        !=======================================================================
        SUBROUTINE ISTS_STS_BP_PERSIST_REFRESH_CONT( &
     &    COUNT_IN, SEC_ID, MST_ID, X, NUMNOD, CONT_ELEMENT, CAPACITY)
          INTEGER, INTENT(IN) :: COUNT_IN, NUMNOD, CAPACITY
          INTEGER, INTENT(IN) :: SEC_ID(CAPACITY, 5), MST_ID(CAPACITY, 5)
          real(kind=WP), INTENT(IN) :: X(3, NUMNOD)
          real(kind=WP), INTENT(INOUT) :: CONT_ELEMENT(CAPACITY, 3, 8)
          INTEGER :: I, J, K, NI

          DO I = 1, COUNT_IN
            J = 1
            DO K = 2, 5
              NI = MST_ID(I, K)
              CONT_ELEMENT(I, 1, J) = X(1, NI)
              CONT_ELEMENT(I, 2, J) = X(2, NI)
              CONT_ELEMENT(I, 3, J) = X(3, NI)
              J = J + 1
            END DO
          END DO

          DO I = 1, COUNT_IN
            J = 5
            DO K = 2, 5
              NI = SEC_ID(I, K)
              CONT_ELEMENT(I, 1, J) = X(1, NI)
              CONT_ELEMENT(I, 2, J) = X(2, NI)
              CONT_ELEMENT(I, 3, J) = X(3, NI)
              J = J + 1
            END DO
          END DO
        END SUBROUTINE ISTS_STS_BP_PERSIST_REFRESH_CONT

!=======================================================================
!   ISTS_STS_BP_PERSIST_SAVE
!
!   Store the current STS segment patch and active secondary GP mask.
!=======================================================================
        SUBROUTINE ISTS_STS_BP_PERSIST_SAVE(NIN, COUNT_IN, SEC_ID, MST_ID, &
     &    SEC_GP_MASK, CAPACITY)
          USE MY_ALLOC_MOD, ONLY : MY_ALLOC
          USE MY_DEALLOC_MOD, ONLY : MY_DEALLOC
          INTEGER, INTENT(IN) :: NIN, COUNT_IN, CAPACITY
          INTEGER, INTENT(IN) :: SEC_ID(CAPACITY, 5), MST_ID(CAPACITY, 5)
          INTEGER, INTENT(IN) :: SEC_GP_MASK(CAPACITY, 4)
          INTEGER :: I

          IF (.NOT. STS_BP_PERSIST_ENABLED) RETURN
          IF (NIN <= 0) RETURN
          IF (COUNT_IN <= 0) RETURN
          IF (COUNT_IN > CAPACITY) RETURN

          CALL ISTS_STS_BP_PERSIST_ENSURE_SIZE(NIN)

          IF (ALLOCATED(PERSIST_SLOT(NIN)%SEC_ID)) THEN
            IF (SIZE(PERSIST_SLOT(NIN)%SEC_ID, 1) < COUNT_IN) THEN
              CALL MY_DEALLOC(PERSIST_SLOT(NIN)%SEC_ID)
              CALL MY_DEALLOC(PERSIST_SLOT(NIN)%MST_ID)
              IF (ALLOCATED(PERSIST_SLOT(NIN)%SEC_GP_MASK)) &
     &          CALL MY_DEALLOC(PERSIST_SLOT(NIN)%SEC_GP_MASK)
            END IF
          END IF
          IF (.NOT. ALLOCATED(PERSIST_SLOT(NIN)%SEC_ID)) THEN
            CALL MY_ALLOC(PERSIST_SLOT(NIN)%SEC_ID, COUNT_IN, 5, "SEC_ID")
            CALL MY_ALLOC(PERSIST_SLOT(NIN)%MST_ID, COUNT_IN, 5, "MST_ID")
            CALL MY_ALLOC(PERSIST_SLOT(NIN)%SEC_GP_MASK, COUNT_IN, 4, "SEC_GP_MASK")
          END IF

          DO I = 1, COUNT_IN
            PERSIST_SLOT(NIN)%SEC_ID(I, 1:5) = SEC_ID(I, 1:5)
            PERSIST_SLOT(NIN)%MST_ID(I, 1:5) = MST_ID(I, 1:5)
            PERSIST_SLOT(NIN)%SEC_GP_MASK(I, 1:4) = SEC_GP_MASK(I, 1:4)
          END DO
          PERSIST_SLOT(NIN)%COUNT_STORED = COUNT_IN
          PERSIST_SLOT(NIN)%LAST_SAVE_NCYCLE = PERSIST_NCYCLE
          PERSIST_SLOT(NIN)%ACTIVE = .TRUE.
        END SUBROUTINE ISTS_STS_BP_PERSIST_SAVE

!=======================================================================
!   ISTS_STS_BP_PERSIST_TRY_RESTORE
!
!   Restore a cached STS segment patch within the grace window and refresh
!   its coordinates from the current nodal positions.
!=======================================================================
        SUBROUTINE ISTS_STS_BP_PERSIST_TRY_RESTORE(NIN, X, NUMNOD, &
     &    CAPACITY, COUNT_OUT, SEC_ID, MST_ID, SEC_GP_MASK, &
     &    CONT_ELEMENT, RESTORED)
          INTEGER, INTENT(IN) :: NIN, NUMNOD, CAPACITY
          real(kind=WP), INTENT(IN) :: X(3, NUMNOD)
          INTEGER, INTENT(INOUT) :: COUNT_OUT
          INTEGER, INTENT(INOUT) :: SEC_ID(CAPACITY, 5)
          INTEGER, INTENT(INOUT) :: MST_ID(CAPACITY, 5)
          INTEGER, INTENT(INOUT) :: SEC_GP_MASK(CAPACITY, 4)
          real(kind=WP), INTENT(INOUT) :: CONT_ELEMENT(CAPACITY, 3, 8)
          LOGICAL, INTENT(INOUT) :: RESTORED
          INTEGER :: I, N_STORED, AGE

          COUNT_OUT = 0
          RESTORED = .FALSE.
          SEC_GP_MASK = 0

          IF (.NOT. STS_BP_PERSIST_ENABLED) RETURN
          IF (NIN <= 0) RETURN
          IF (CAPACITY <= 0) RETURN
          IF (.NOT. ALLOCATED(PERSIST_SLOT)) RETURN
          IF (NIN > SIZE(PERSIST_SLOT)) RETURN
          IF (.NOT. PERSIST_SLOT(NIN)%ACTIVE) RETURN

          IF (PERSIST_NCYCLE > 0 .AND. &
     &        PERSIST_SLOT(NIN)%LAST_SAVE_NCYCLE > 0) THEN
            AGE = PERSIST_NCYCLE - PERSIST_SLOT(NIN)%LAST_SAVE_NCYCLE
            IF (AGE > PERSIST_GRACE_CYCLES) THEN
              PERSIST_SLOT(NIN)%ACTIVE = .FALSE.
              RETURN
            END IF
          END IF

          N_STORED = PERSIST_SLOT(NIN)%COUNT_STORED
          IF (N_STORED <= 0) RETURN
          IF (N_STORED > CAPACITY) RETURN
          IF (.NOT. ALLOCATED(PERSIST_SLOT(NIN)%SEC_ID)) RETURN
          IF (.NOT. ALLOCATED(PERSIST_SLOT(NIN)%MST_ID)) RETURN

!         Do not distance-cull the cached STS segment pairs here.  Corner
!         node distance is misleading for STS patches: a valid
!         Gauss/Lobatto projection can be within GAP while every
!         corner-corner distance is larger.  The grace window limits stale
!         restores; the narrow phase remains the authoritative geometric
!         filter and will return IMPACT=0 for stale pairs.

          DO I = 1, N_STORED
            SEC_ID(I, 1:5) = PERSIST_SLOT(NIN)%SEC_ID(I, 1:5)
            MST_ID(I, 1:5) = PERSIST_SLOT(NIN)%MST_ID(I, 1:5)
            IF (ALLOCATED(PERSIST_SLOT(NIN)%SEC_GP_MASK)) THEN
              SEC_GP_MASK(I, 1:4) = PERSIST_SLOT(NIN)%SEC_GP_MASK(I, 1:4)
            ELSE
              SEC_GP_MASK(I, 1:4) = 1
            ENDIF
          END DO
          COUNT_OUT = N_STORED

          CALL ISTS_STS_BP_PERSIST_REFRESH_CONT( &
     &        COUNT_OUT, SEC_ID, MST_ID, X, NUMNOD, CONT_ELEMENT, CAPACITY)

          RESTORED = .TRUE.
        END SUBROUTINE ISTS_STS_BP_PERSIST_TRY_RESTORE

      END MODULE ISTS_STS_BP_PERSIST_MOD
