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
!||    ists_sts_skip_mod  ../engine/source/interfaces/ists/ists_sts_skip_mod.F90
!||--- called by ------------------------------------------------------
!||    ists_mainf              ../engine/source/interfaces/ists/ists_mainf.F
!||====================================================================
!
!   Adaptive broad-phase skip for STS voxel contact (aligned with Q1NP).
!   When surfaces are far apart and no contact was found, skip the
!   expensive broad/narrow phase for several subsequent cycles.
!
      MODULE ISTS_STS_SKIP_MOD
        USE PRECISION_MOD, ONLY : WP
        IMPLICIT NONE
        PRIVATE

!       Adaptive skip settings (so broad/narrow phase is checked less often when surfaces are far).
        LOGICAL, PARAMETER, PUBLIC :: STS_SKIP_ENABLED  = .TRUE. ! Activated
        REAL(KIND=WP), PARAMETER, PUBLIC :: STS_SKIP_SCALE    = 8.0_WP
        REAL(KIND=WP), PARAMETER, PUBLIC :: STS_SKIP_EXPONENT = 1.5_WP
        INTEGER, PARAMETER, PUBLIC :: STS_SKIP_MAX      = 200

        REAL(KIND=WP), PARAMETER :: STS_SKIP_RATIO_OFFSET = 1.0_WP

        INTEGER, ALLOCATABLE, SAVE :: STS_SKIP_REMAINING(:)

        PUBLIC :: ISTS_STS_SKIP_TICK
        PUBLIC :: ISTS_STS_SKIP_UPDATE

      CONTAINS

        SUBROUTINE ISTS_STS_SKIP_ENSURE_SIZE(NIN)
          USE MY_ALLOC_MOD, ONLY : MY_ALLOC
          USE MY_DEALLOC_MOD, ONLY : MY_DEALLOC
          INTEGER, INTENT(IN) :: NIN
          INTEGER, ALLOCATABLE :: TMP(:)
          INTEGER :: OLD_SIZE, I

          IF (NIN <= 0) RETURN
          IF (.NOT. ALLOCATED(STS_SKIP_REMAINING)) THEN
            CALL MY_ALLOC(STS_SKIP_REMAINING, NIN, "STS_SKIP_REMAINING")
            STS_SKIP_REMAINING(1:NIN) = 0
            RETURN
          END IF
          OLD_SIZE = SIZE(STS_SKIP_REMAINING)
          IF (NIN <= OLD_SIZE) RETURN
          CALL MY_ALLOC(TMP, NIN, "TMP")
          IF (OLD_SIZE > 0) TMP(1:OLD_SIZE) = STS_SKIP_REMAINING(1:OLD_SIZE)
          DO I = OLD_SIZE + 1, NIN
            TMP(I) = 0
          END DO
          CALL MY_DEALLOC(STS_SKIP_REMAINING)
          CALL MOVE_ALLOC(TMP, STS_SKIP_REMAINING)
        END SUBROUTINE ISTS_STS_SKIP_ENSURE_SIZE

!=======================================================================
!   ISTS_STS_SKIP_TICK
!   If skip is active for NIN, decrement counter and set DO_SKIP.
!=======================================================================
        SUBROUTINE ISTS_STS_SKIP_TICK(NIN, DO_SKIP)
          INTEGER, INTENT(IN) :: NIN
          LOGICAL, INTENT(OUT) :: DO_SKIP

          DO_SKIP = .FALSE.
          IF (NIN <= 0) RETURN
          IF (.NOT. STS_SKIP_ENABLED) RETURN
          CALL ISTS_STS_SKIP_ENSURE_SIZE(NIN)
          IF (STS_SKIP_REMAINING(NIN) <= 0) RETURN
          STS_SKIP_REMAINING(NIN) = STS_SKIP_REMAINING(NIN) - 1
          DO_SKIP = .TRUE.
        END SUBROUTINE ISTS_STS_SKIP_TICK

!=======================================================================
!   ISTS_STS_SKIP_UPDATE
!   After a full STS contact pass, set skip counter from min distance.
!=======================================================================
        SUBROUTINE ISTS_STS_SKIP_UPDATE(NIN, D_MIN, SEARCH_PADDING, &
     &    HAS_CONTACT)
          INTEGER, INTENT(IN) :: NIN
          REAL(KIND=WP), INTENT(IN) :: D_MIN, SEARCH_PADDING
          LOGICAL, INTENT(IN) :: HAS_CONTACT
          REAL(KIND=WP) :: DIST_RATIO

          IF (NIN <= 0) RETURN
          CALL ISTS_STS_SKIP_ENSURE_SIZE(NIN)

          IF (.NOT. STS_SKIP_ENABLED) THEN
            STS_SKIP_REMAINING(NIN) = 0
            RETURN
          END IF

          IF (HAS_CONTACT) THEN
            STS_SKIP_REMAINING(NIN) = 0
            RETURN
          END IF

          DIST_RATIO = D_MIN / SEARCH_PADDING
          IF (DIST_RATIO <= STS_SKIP_RATIO_OFFSET) THEN
            STS_SKIP_REMAINING(NIN) = 0
          ELSE
            STS_SKIP_REMAINING(NIN) = MIN(STS_SKIP_MAX, &
     &        INT(STS_SKIP_SCALE * &
     &        (DIST_RATIO - STS_SKIP_RATIO_OFFSET)**STS_SKIP_EXPONENT))
          END IF
        END SUBROUTINE ISTS_STS_SKIP_UPDATE

      END MODULE ISTS_STS_SKIP_MOD
