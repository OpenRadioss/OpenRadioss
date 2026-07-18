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
!
!   Hardcoded broad-phase algorithm selector for STS contact.
!
!   STS_BP_ALGO_VOXEL       : use the STS-native voxel broad phase
!                             (STS_VOXEL_BROAD_PHASE).
!   STS_BP_ALGO_INT7_BUCKET : reuse the legacy INT7 bucket sorting
!                             (I7BUCE/I7TRI) candidate arrays and map
!                             them to STS segment pairs via
!                             STS_REMAP_SEGMENTS.
!
!   STS_BP_ALGO is a PARAMETER so it is patched at build time by the
!   study toolchain (patch_engine_constants.py); no input/deck change
!   is required to switch between the two paths.
!
!||====================================================================
!||    ists_sts_bp_algo_mod   ../engine/source/interfaces/ists/ists_sts_bp_algo_mod.F90
!||--- called by ------------------------------------------------------
!||    i7main_tri             ../engine/source/interfaces/intsort/i7main_tri.F
!||    i7trc                  ../engine/source/interfaces/intsort/i7trc.F
!||    inter_check_sort       ../engine/source/interfaces/generic/inter_check_sort.F
!||    inter_sort_07          ../engine/source/interfaces/int07/inter_sort_07.F
!||    ists_mainf             ../engine/source/interfaces/ists/ists_mainf.F90
!||====================================================================
      MODULE ISTS_STS_BP_ALGO_MOD
        IMPLICIT NONE
        PRIVATE

        INTEGER, PARAMETER, PUBLIC :: STS_BP_ALGO_VOXEL       = 0
        INTEGER, PARAMETER, PUBLIC :: STS_BP_ALGO_INT7_BUCKET = 1

!       Compile-time STS broad-phase selector.
!       Set here the option to use for the broad-phase algorithm.
        INTEGER, PARAMETER, PUBLIC :: STS_BP_ALGO = STS_BP_ALGO_INT7_BUCKET

        LOGICAL, PARAMETER, PUBLIC :: STS_INT7_SORT_REUSE_ENABLED = .TRUE.
        INTEGER, PARAMETER, PUBLIC :: STS_INT7_SORT_REUSE_PERIOD = 5

        PUBLIC :: STS_INT7_BUCKET_SORT_DECIDE

      CONTAINS

!=======================================================================
!   STS_INT7_BUCKET_SORT_DECIDE
!
!   Decide whether Type 7 sorting must rebuild INT7 bucket candidates for
!   the STS INT7-bucket broad phase on the current cycle.
!=======================================================================
!||====================================================================
!||    sts_int7_bucket_sort_decide   ../engine/source/interfaces/ists/ists_sts_bp_algo_mod.F90
!||--- called by ------------------------------------------------------
!||    i7main_tri                    ../engine/source/interfaces/intsort/i7main_tri.F
!||    inter_check_sort              ../engine/source/interfaces/generic/inter_check_sort.F
!||    inter_sort_07                 ../engine/source/interfaces/int07/inter_sort_07.F
!||====================================================================
        SUBROUTINE STS_INT7_BUCKET_SORT_DECIDE(NCYCLE_IN, &
     &    TIME_ACTIVE, IPSTS, BP_ALGO_IN, CAND_COUNT, INACTI, IFQ, &
     &    NUM_IMP, ITIED, NEED_SORT)
          INTEGER, INTENT(IN) :: NCYCLE_IN, IPSTS, BP_ALGO_IN
          INTEGER, INTENT(IN) :: CAND_COUNT, INACTI, IFQ
          INTEGER, INTENT(IN) :: NUM_IMP, ITIED
          LOGICAL, INTENT(IN) :: TIME_ACTIVE
          LOGICAL, INTENT(INOUT) :: NEED_SORT

          IF (IPSTS /= 1) RETURN
          IF (BP_ALGO_IN /= STS_BP_ALGO_INT7_BUCKET) RETURN

          IF (.NOT. STS_INT7_SORT_REUSE_ENABLED) THEN
            NEED_SORT = TIME_ACTIVE
            RETURN
          ENDIF

          IF (.NOT. TIME_ACTIVE) THEN
            NEED_SORT = .FALSE.
            RETURN
          ENDIF

          IF (CAND_COUNT <= 0 .OR. NCYCLE_IN <= 1) THEN
            NEED_SORT = .TRUE.
            RETURN
          ENDIF

!         Stateful Type 7 options need the legacy candidate refresh path.
          IF (INACTI == 5 .OR. INACTI == 6 .OR. INACTI == 7 .OR. &
     &      IFQ > 0 .OR. NUM_IMP > 0 .OR. ITIED /= 0) THEN
            NEED_SORT = .TRUE.
            RETURN
          ENDIF

!         Reuse the previous INT7 bucket candidates between rebuild cycles.
          NEED_SORT = MOD(NCYCLE_IN, STS_INT7_SORT_REUSE_PERIOD) == 0
        END SUBROUTINE STS_INT7_BUCKET_SORT_DECIDE

      END MODULE ISTS_STS_BP_ALGO_MOD

!
!   Short-lived activity cache for conservative STS Lobatto candidate
!   lists.  Empty remap pairs are rechecked after a few cycles, while
!   active or near-gap pairs remain evaluated every cycle.
!
!||====================================================================
!||    ists_sts_pair_activity_mod   ../engine/source/interfaces/ists/ists_sts_bp_algo_mod.F90
!||--- called by ------------------------------------------------------
!||    sts_contacts_assemble        ../engine/source/interfaces/ists/ists_contacts_assemble.F90
!||====================================================================
      MODULE ISTS_STS_PAIR_ACTIVITY_MOD
        IMPLICIT NONE
        PRIVATE

        LOGICAL, PARAMETER :: STS_PAIR_SKIP_ENABLED = .TRUE.
        INTEGER, PARAMETER :: STS_PAIR_SKIP_CYCLES = 5
        INTEGER, PARAMETER :: STS_PAIR_HASH_SIZE = 131071
        REAL*8, PARAMETER :: STS_PAIR_CLOSE_GAP_FRAC = 5.0D-2
        REAL*8, PARAMETER :: STS_PAIR_CLOSE_ABS = 1.0D-12

        INTEGER, SAVE :: HASH_IF(STS_PAIR_HASH_SIZE) = 0
        INTEGER, SAVE :: HASH_SEC(STS_PAIR_HASH_SIZE) = 0
        INTEGER, SAVE :: HASH_MST(STS_PAIR_HASH_SIZE) = 0
        INTEGER, SAVE :: HASH_OPTION(STS_PAIR_HASH_SIZE) = 0
        INTEGER, SAVE :: HASH_SKIP_UNTIL(STS_PAIR_HASH_SIZE) = 0

        PUBLIC :: STS_PAIR_ACTIVITY_SHOULD_SKIP
        PUBLIC :: STS_PAIR_ACTIVITY_UPDATE

      CONTAINS

!=======================================================================
!   STS_PAIR_ACTIVITY_FIND
!
!   Find or reserve a hash slot for one STS segment pair and quadrature
!   option in the short-lived pair activity cache.
!=======================================================================
!||====================================================================
!||    sts_pair_activity_find          ../engine/source/interfaces/ists/ists_sts_bp_algo_mod.F90
!||--- called by ------------------------------------------------------
!||    sts_pair_activity_should_skip   ../engine/source/interfaces/ists/ists_sts_bp_algo_mod.F90
!||    sts_pair_activity_update        ../engine/source/interfaces/ists/ists_sts_bp_algo_mod.F90
!||====================================================================
        SUBROUTINE STS_PAIR_ACTIVITY_FIND(STS_INTERFACE_ID, SEC_SEG, &
     &    MST_SEG, OPTION, SLOT, FOUND)
          INTEGER, INTENT(IN) :: STS_INTERFACE_ID, SEC_SEG
          INTEGER, INTENT(IN) :: MST_SEG, OPTION
          INTEGER, INTENT(INOUT) :: SLOT
          LOGICAL, INTENT(INOUT) :: FOUND
          INTEGER :: IDX, PROBE
          INTEGER(KIND=8) :: HKEY

          SLOT = 0
          FOUND = .FALSE.
          IF (STS_INTERFACE_ID <= 0 .OR. SEC_SEG <= 0 .OR. &
     &      MST_SEG <= 0) RETURN

          HKEY = INT(STS_INTERFACE_ID, KIND=8) * &
     &      INT(1000003, KIND=8) + &
     &      INT(SEC_SEG, KIND=8) * INT(9176, KIND=8) + &
     &      INT(MST_SEG, KIND=8) * INT(6361, KIND=8) + &
     &      INT(OPTION, KIND=8)
          IDX = INT(MOD(ABS(HKEY), INT(STS_PAIR_HASH_SIZE, KIND=8))) + 1

          DO PROBE = 1, STS_PAIR_HASH_SIZE
            IF (HASH_IF(IDX) == 0) THEN
              SLOT = IDX
              RETURN
            ENDIF
            IF (HASH_IF(IDX) == STS_INTERFACE_ID .AND. &
     &        HASH_SEC(IDX) == SEC_SEG .AND. &
     &        HASH_MST(IDX) == MST_SEG .AND. &
     &        HASH_OPTION(IDX) == OPTION) THEN
              SLOT = IDX
              FOUND = .TRUE.
              RETURN
            ENDIF
            IDX = IDX + 1
            IF (IDX > STS_PAIR_HASH_SIZE) IDX = 1
          ENDDO
        END SUBROUTINE STS_PAIR_ACTIVITY_FIND

!=======================================================================
!   STS_PAIR_ACTIVITY_SHOULD_SKIP
!
!   Return whether an inactive Lobatto pair can be skipped this cycle.
!=======================================================================
!||====================================================================
!||    sts_pair_activity_should_skip   ../engine/source/interfaces/ists/ists_sts_bp_algo_mod.F90
!||--- called by ------------------------------------------------------
!||    sts_contacts_assemble           ../engine/source/interfaces/ists/ists_contacts_assemble.F90
!||--- calls      -----------------------------------------------------
!||    sts_pair_activity_find          ../engine/source/interfaces/ists/ists_sts_bp_algo_mod.F90
!||====================================================================
        SUBROUTINE STS_PAIR_ACTIVITY_SHOULD_SKIP(NCYCLE_IN, &
     &    STS_INTERFACE_ID, SEC_SEG, MST_SEG, OPTION, DO_SKIP)
          INTEGER, INTENT(IN) :: NCYCLE_IN, STS_INTERFACE_ID
          INTEGER, INTENT(IN) :: SEC_SEG, MST_SEG, OPTION
          LOGICAL, INTENT(INOUT) :: DO_SKIP
          INTEGER :: SLOT
          LOGICAL :: FOUND

          DO_SKIP = .FALSE.
          IF (.NOT. STS_PAIR_SKIP_ENABLED) RETURN
          IF (NCYCLE_IN <= 0) RETURN
          IF (OPTION /= 1) RETURN

          CALL STS_PAIR_ACTIVITY_FIND(STS_INTERFACE_ID, SEC_SEG, &
     &      MST_SEG, OPTION, SLOT, FOUND)
          IF (.NOT. FOUND) RETURN

          DO_SKIP = NCYCLE_IN <= HASH_SKIP_UNTIL(SLOT)
        END SUBROUTINE STS_PAIR_ACTIVITY_SHOULD_SKIP

!=======================================================================
!   STS_PAIR_ACTIVITY_UPDATE
!
!   Update the skip horizon for one pair after narrow-phase evaluation.
!=======================================================================
!||====================================================================
!||    sts_pair_activity_update   ../engine/source/interfaces/ists/ists_sts_bp_algo_mod.F90
!||--- called by ------------------------------------------------------
!||    sts_contacts_assemble      ../engine/source/interfaces/ists/ists_contacts_assemble.F90
!||--- calls      -----------------------------------------------------
!||    sts_pair_activity_find     ../engine/source/interfaces/ists/ists_sts_bp_algo_mod.F90
!||====================================================================
        SUBROUTINE STS_PAIR_ACTIVITY_UPDATE(NCYCLE_IN, &
     &    STS_INTERFACE_ID, SEC_SEG, MST_SEG, OPTION, IMPACT, &
     &    VALID_GP, MIN_PENE, GAP)
          INTEGER, INTENT(IN) :: NCYCLE_IN, STS_INTERFACE_ID
          INTEGER, INTENT(IN) :: SEC_SEG, MST_SEG, OPTION
          INTEGER, INTENT(IN) :: IMPACT, VALID_GP
          REAL*8, INTENT(IN) :: MIN_PENE, GAP
          INTEGER :: SLOT
          LOGICAL :: FOUND
          REAL*8 :: CLOSE_MARGIN

          IF (.NOT. STS_PAIR_SKIP_ENABLED) RETURN
          IF (NCYCLE_IN <= 0) RETURN
          IF (OPTION /= 1) RETURN

          CALL STS_PAIR_ACTIVITY_FIND(STS_INTERFACE_ID, SEC_SEG, &
     &      MST_SEG, OPTION, SLOT, FOUND)
          IF (SLOT <= 0) RETURN

          IF (.NOT. FOUND) THEN
            HASH_IF(SLOT) = STS_INTERFACE_ID
            HASH_SEC(SLOT) = SEC_SEG
            HASH_MST(SLOT) = MST_SEG
            HASH_OPTION(SLOT) = OPTION
          ENDIF

          IF (IMPACT == 1) THEN
            HASH_SKIP_UNTIL(SLOT) = NCYCLE_IN
            RETURN
          ENDIF

          CLOSE_MARGIN = MAX(DABS(GAP) * STS_PAIR_CLOSE_GAP_FRAC, &
     &      STS_PAIR_CLOSE_ABS)
          IF (VALID_GP > 0 .AND. MIN_PENE <= CLOSE_MARGIN) THEN
            HASH_SKIP_UNTIL(SLOT) = NCYCLE_IN
          ELSE
            HASH_SKIP_UNTIL(SLOT) = NCYCLE_IN + STS_PAIR_SKIP_CYCLES
          ENDIF
        END SUBROUTINE STS_PAIR_ACTIVITY_UPDATE

      END MODULE ISTS_STS_PAIR_ACTIVITY_MOD
