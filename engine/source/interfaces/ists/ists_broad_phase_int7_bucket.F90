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
!||    sts_broad_phase_int7_bucket_mod   ../engine/source/interfaces/ists/ists_broad_phase_int7_bucket.F90
!||--- called by ------------------------------------------------------
!||    ists_mainf              ../engine/source/interfaces/ists/ists_mainf.F
!||--- calls      -----------------------------------------------------
!||    sts_remap_segments      ../engine/source/interfaces/ists/ists_remap_segments.F90
!||--- uses       -----------------------------------------------------
!||    intbufdef_mod           ../common_source/modules/interfaces/intbufdef_mod.F90
!||    groupdef_mod            ../engine/share/modules/groupdef_mod.F
!||====================================================================
!
!   Legacy INT7 bucket broad-phase adapter for STS contact.
!
!   Instead of running the STS-native voxel search, this path consumes
!   the candidate pairs that the legacy INT7 sorting (I7BUCE/I7TRI) has
!   already stored in INTBUF_TAB%CAND_N / CAND_E (node <-> main segment)
!   and maps them to STS segment pairs through STS_REMAP_SEGMENTS.
!
!   The output (CAND_SEC_SEG_ID, CAND_MST_SEG_ID, CONT_ELEMENT, COUNT)
!   matches STS_VOXEL_BROAD_PHASE so the downstream STS pipeline
!   (STS_CONTACT_STIFFNESS, STS_CONTACTS_ASSEMBLE) is unchanged.
!
      MODULE STS_BROAD_PHASE_INT7_BUCKET_MOD
        USE INTBUFDEF_MOD, ONLY : INTBUF_STRUCT_
        USE GROUPDEF_MOD,  ONLY : SURF_
        USE CONSTANT_MOD,  ONLY : ZERO
        USE ISTS_STS_BP_PERSIST_MOD, ONLY : ISTS_STS_BP_PERSIST_SAVE, &
     &    ISTS_STS_BP_PERSIST_TRY_RESTORE
        USE MY_ALLOC_MOD, ONLY : MY_ALLOC
        USE MY_DEALLOC_MOD, ONLY : MY_DEALLOC
        USE PRECISION_MOD, ONLY : WP
        IMPLICIT NONE
        PRIVATE
        PUBLIC :: STS_INT7_BUCKET_BROAD_PHASE
      CONTAINS
!=======================================================================
!   STS_INT7_BUCKET_BROAD_PHASE
!
!   Build STS segment pairs from the legacy INT7 candidate arrays.
!   COUNT is clamped to MAX_STS_SIZE_ACTUAL; OVERFLOW is set when the
!   storage saturated so the caller can grow capacity and retry.
!=======================================================================
        SUBROUTINE STS_INT7_BUCKET_BROAD_PHASE( &
     &      NIN, INTBUF_TAB, IGRSURF, NSURF, SEC_SURF_IDX, MST_SURF_IDX, &
     &      X, NUMNOD, NSN, NRTM, MAX_STS_SIZE_ACTUAL, &
     &      CAND_SEC_SEG_ID, CAND_MST_SEG_ID, CAND_SEC_GP_MASK, &
     &      CONT_ELEMENT, COUNT, OVERFLOW, D_MIN)
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          INTEGER, INTENT(IN)  :: NIN
          TYPE(INTBUF_STRUCT_), INTENT(IN) :: INTBUF_TAB
          INTEGER, INTENT(IN)  :: NSURF
          TYPE(SURF_), DIMENSION(NSURF), INTENT(IN) :: IGRSURF
          INTEGER, INTENT(IN)  :: SEC_SURF_IDX, MST_SURF_IDX
          INTEGER, INTENT(IN)  :: NUMNOD
          INTEGER, INTENT(IN)  :: NSN
          INTEGER, INTENT(IN)  :: NRTM
          INTEGER, INTENT(IN)  :: MAX_STS_SIZE_ACTUAL
          real(kind=WP), INTENT(IN)  :: X(3, NUMNOD)
          INTEGER, INTENT(INOUT) :: CAND_SEC_SEG_ID(MAX_STS_SIZE_ACTUAL, 5)
          INTEGER, INTENT(INOUT) :: CAND_MST_SEG_ID(MAX_STS_SIZE_ACTUAL, 5)
          INTEGER, INTENT(INOUT) :: CAND_SEC_GP_MASK(MAX_STS_SIZE_ACTUAL, 4)
          real(kind=WP), INTENT(INOUT) :: CONT_ELEMENT(MAX_STS_SIZE_ACTUAL, 3, 8)
          INTEGER, INTENT(INOUT) :: COUNT
          LOGICAL, INTENT(INOUT) :: OVERFLOW
          real(kind=WP), INTENT(INOUT) :: D_MIN
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          INTEGER :: I_STOK, I, NSEC_BOUNDS, CAND_N_ABS, N_VALID
          INTEGER :: COUNT_FRESH, COUNT_PERSIST, PERSIST_I, FRESH_I
          INTEGER :: OVERLAP_COUNT, MISSING_COUNT, FINAL_LIMIT
          INTEGER :: FRESH_HASH_SIZE, OUT_HASH_SIZE, HASH_INDEX
          INTEGER, ALLOCATABLE :: CAND_SEC_SEG(:)
          INTEGER, ALLOCATABLE :: CAND_N_COMPACT(:), CAND_E_COMPACT(:)
          INTEGER, ALLOCATABLE :: FRESH_SEC_ID(:,:)
          INTEGER, ALLOCATABLE :: FRESH_MST_ID(:,:)
          INTEGER, ALLOCATABLE :: FRESH_GP_MASK(:,:)
          INTEGER, ALLOCATABLE :: FRESH_HASH_SEC(:)
          INTEGER, ALLOCATABLE :: FRESH_HASH_MST(:)
          INTEGER, ALLOCATABLE :: FRESH_HASH_INDEX(:)
          INTEGER, ALLOCATABLE :: OUT_HASH_SEC(:)
          INTEGER, ALLOCATABLE :: OUT_HASH_MST(:)
          INTEGER, ALLOCATABLE :: OUT_HASH_INDEX(:)
          INTEGER, ALLOCATABLE :: PERSIST_SEC_ID(:,:)
          INTEGER, ALLOCATABLE :: PERSIST_MST_ID(:,:)
          INTEGER, ALLOCATABLE :: PERSIST_GP_MASK(:,:)
          real(kind=WP), ALLOCATABLE :: FRESH_CONT_ELEMENT(:,:,:)
          real(kind=WP), ALLOCATABLE :: PERSIST_CONT_ELEMENT(:,:,:)
          LOGICAL :: PERSIST_RESTORED
          LOGICAL :: PERSIST_STABILIZE, PAIR_EXISTS
!-----------------------------------------------
!   S o u r c e   L i n e s
!-----------------------------------------------
          COUNT = 0
          N_VALID = 0
          OVERFLOW = .FALSE.
!         Skip is disabled for the legacy path; D_MIN is only used by the
!         voxel adaptive-skip logic, so a neutral value is returned.
          D_MIN = ZERO
!
          IF (MAX_STS_SIZE_ACTUAL <= 0) RETURN
          IF (SEC_SURF_IDX <= 0 .OR. SEC_SURF_IDX > NSURF) RETURN
          IF (MST_SURF_IDX <= 0 .OR. MST_SURF_IDX > NSURF) RETURN
!
!         Number of candidate (node, main segment) pairs produced by the
!         INT7 sorting for this cycle. If the sorting was skipped this
!         cycle (e.g. distance criterion), no fresh candidates exist.
          I_STOK = INTBUF_TAB%I_STOK(1)
          IF (I_STOK <= 0) THEN
!           Reuse the last successful STS patch when no fresh INT7
!           candidates are available.
            CALL ISTS_STS_BP_PERSIST_TRY_RESTORE( &
     &          NIN, X, NUMNOD, MAX_STS_SIZE_ACTUAL, &
     &          COUNT, CAND_SEC_SEG_ID, CAND_MST_SEG_ID, &
     &          CAND_SEC_GP_MASK, CONT_ELEMENT, PERSIST_RESTORED)
            IF (PERSIST_RESTORED) THEN
              IF (COUNT >= MAX_STS_SIZE_ACTUAL) OVERFLOW = .TRUE.
              RETURN
            END IF
            RETURN
          ENDIF
!
          NSEC_BOUNDS = NSN
          IF (INTBUF_TAB%S_NSV > 0) THEN
            NSEC_BOUNDS = INTBUF_TAB%S_NSV
          ENDIF
!
!         Keep only INT7 bucket slots that reached the legacy active state.
!         Positive bucket slots are broad candidates only; remapping them
!         directly to STS segment pairs over-couples curved contact patches.
          N_VALID = 0
          CALL MY_ALLOC(CAND_N_COMPACT, I_STOK, "CAND_N_COMPACT")
          CALL MY_ALLOC(CAND_E_COMPACT, I_STOK, "CAND_E_COMPACT")
          DO I = 1, I_STOK
            IF (INTBUF_TAB%CAND_E(I) <= 0 .OR. &
     &          INTBUF_TAB%CAND_E(I) > NRTM) CYCLE
            IF (INTBUF_TAB%CAND_N(I) >= 0) CYCLE
            CAND_N_ABS = -INTBUF_TAB%CAND_N(I)
            IF (CAND_N_ABS > 0 .AND. &
     &              CAND_N_ABS <= NSEC_BOUNDS) THEN
              N_VALID = N_VALID + 1
              CAND_N_COMPACT(N_VALID) = CAND_N_ABS
            ELSE
              CYCLE
            ENDIF
            CAND_E_COMPACT(N_VALID) = INTBUF_TAB%CAND_E(I)
          END DO
          IF (N_VALID <= 0) THEN
            CALL MY_DEALLOC(CAND_N_COMPACT)
            CALL MY_DEALLOC(CAND_E_COMPACT)
!           Reuse the last successful STS patch when INT7 produced no
!           active node/segment candidates.
            CALL ISTS_STS_BP_PERSIST_TRY_RESTORE( &
     &          NIN, X, NUMNOD, MAX_STS_SIZE_ACTUAL, &
     &          COUNT, CAND_SEC_SEG_ID, CAND_MST_SEG_ID, &
     &          CAND_SEC_GP_MASK, CONT_ELEMENT, PERSIST_RESTORED)
            IF (PERSIST_RESTORED) THEN
              IF (COUNT >= MAX_STS_SIZE_ACTUAL) OVERFLOW = .TRUE.
              RETURN
            END IF
            RETURN
          ENDIF
!
          CALL MY_ALLOC(CAND_SEC_SEG, MAX_STS_SIZE_ACTUAL, "CAND_SEC_SEG")
!
          CALL STS_REMAP_SEGMENTS( &
     &        INTBUF_TAB, X, NUMNOD, NRTM, NSN, CAND_SEC_SEG, &
     &        N_VALID, CAND_N_COMPACT, CAND_E_COMPACT, &
     &        INTBUF_TAB%IRECTM, CONT_ELEMENT, COUNT, &
     &        IGRSURF, CAND_SEC_SEG_ID, CAND_MST_SEG_ID, &
     &        CAND_SEC_GP_MASK, &
     &        MAX_STS_SIZE_ACTUAL, NSURF, SEC_SURF_IDX, MST_SURF_IDX)

          COUNT_FRESH = COUNT
          IF (COUNT_FRESH > 0) THEN
            CALL MY_ALLOC(PERSIST_SEC_ID, MAX_STS_SIZE_ACTUAL, 5, "PERSIST_SEC_ID")
            CALL MY_ALLOC(PERSIST_MST_ID, MAX_STS_SIZE_ACTUAL, 5, "PERSIST_MST_ID")
            CALL MY_ALLOC(PERSIST_GP_MASK, MAX_STS_SIZE_ACTUAL, 4, "PERSIST_GP_MASK")
            CALL MY_ALLOC(PERSIST_CONT_ELEMENT, MAX_STS_SIZE_ACTUAL, 3, 8, "PERSIST_CONT_ELEMENT")
            CALL ISTS_STS_BP_PERSIST_TRY_RESTORE( &
     &        NIN, X, NUMNOD, MAX_STS_SIZE_ACTUAL, &
     &        COUNT_PERSIST, PERSIST_SEC_ID, PERSIST_MST_ID, &
     &        PERSIST_GP_MASK, PERSIST_CONT_ELEMENT, PERSIST_RESTORED)

!           A partial INT7 candidate wipe is as harmful to STS as a full
!           wipe because the integrated patch can lose support abruptly.
!           Keep cached segment pairs when fresh active INT7 candidates
!           drop existing partners, but do not limit legitimate patch
!           growth during impact.
            IF (PERSIST_RESTORED .AND. COUNT_PERSIST > 0) THEN
              FRESH_HASH_SIZE = MAX(17, 4 * COUNT_FRESH + 1)
              CALL MY_ALLOC(FRESH_HASH_SEC, FRESH_HASH_SIZE, "FRESH_HASH_SEC")
              CALL MY_ALLOC(FRESH_HASH_MST, FRESH_HASH_SIZE, "FRESH_HASH_MST")
              CALL MY_ALLOC(FRESH_HASH_INDEX, FRESH_HASH_SIZE, "FRESH_HASH_INDEX")
              FRESH_HASH_SEC = 0
              FRESH_HASH_MST = 0
              FRESH_HASH_INDEX = 0
              DO FRESH_I = 1, COUNT_FRESH
                CALL STS_PAIR_HASH_INSERT(CAND_SEC_SEG_ID(FRESH_I, 1), &
     &            CAND_MST_SEG_ID(FRESH_I, 1), FRESH_I, &
     &            FRESH_HASH_SIZE, FRESH_HASH_SEC, FRESH_HASH_MST, &
     &            FRESH_HASH_INDEX, HASH_INDEX)
              ENDDO

              OVERLAP_COUNT = 0
              MISSING_COUNT = 0
              DO PERSIST_I = 1, COUNT_PERSIST
                PAIR_EXISTS = STS_PAIR_HASH_CONTAINS( &
     &            PERSIST_SEC_ID(PERSIST_I, 1), &
     &            PERSIST_MST_ID(PERSIST_I, 1), FRESH_HASH_SIZE, &
     &            FRESH_HASH_SEC, FRESH_HASH_MST, FRESH_HASH_INDEX)
                IF (PAIR_EXISTS) THEN
                  OVERLAP_COUNT = OVERLAP_COUNT + 1
                ELSE
                  MISSING_COUNT = MISSING_COUNT + 1
                ENDIF
              ENDDO

              PERSIST_STABILIZE = MISSING_COUNT > 0
              IF (PERSIST_STABILIZE .AND. OVERLAP_COUNT > 0) THEN
                CALL MY_ALLOC(FRESH_SEC_ID, COUNT_FRESH, 5, "FRESH_SEC_ID")
                CALL MY_ALLOC(FRESH_MST_ID, COUNT_FRESH, 5, "FRESH_MST_ID")
                CALL MY_ALLOC(FRESH_GP_MASK, COUNT_FRESH, 4, "FRESH_GP_MASK")
                CALL MY_ALLOC(FRESH_CONT_ELEMENT, COUNT_FRESH, 3, 8, "FRESH_CONT_ELEMENT")

                FRESH_SEC_ID(1:COUNT_FRESH, 1:5) = &
     &            CAND_SEC_SEG_ID(1:COUNT_FRESH, 1:5)
                FRESH_MST_ID(1:COUNT_FRESH, 1:5) = &
     &            CAND_MST_SEG_ID(1:COUNT_FRESH, 1:5)
                FRESH_GP_MASK(1:COUNT_FRESH, 1:4) = &
     &            CAND_SEC_GP_MASK(1:COUNT_FRESH, 1:4)
                FRESH_CONT_ELEMENT(1:COUNT_FRESH, 1:3, 1:8) = &
     &            CONT_ELEMENT(1:COUNT_FRESH, 1:3, 1:8)

                COUNT = 0
                FINAL_LIMIT = MAX_STS_SIZE_ACTUAL
                OUT_HASH_SIZE = MAX(17, 4 * FINAL_LIMIT + 1)
                CALL MY_ALLOC(OUT_HASH_SEC, OUT_HASH_SIZE, "OUT_HASH_SEC")
                CALL MY_ALLOC(OUT_HASH_MST, OUT_HASH_SIZE, "OUT_HASH_MST")
                CALL MY_ALLOC(OUT_HASH_INDEX, OUT_HASH_SIZE, "OUT_HASH_INDEX")
                OUT_HASH_SEC = 0
                OUT_HASH_MST = 0
                OUT_HASH_INDEX = 0

                DO PERSIST_I = 1, COUNT_PERSIST
                  IF (COUNT >= MAX_STS_SIZE_ACTUAL) THEN
                    OVERFLOW = .TRUE.
                    EXIT
                  ENDIF
                  COUNT = COUNT + 1
                  CAND_SEC_SEG_ID(COUNT, 1:5) = &
     &              PERSIST_SEC_ID(PERSIST_I, 1:5)
                  CAND_MST_SEG_ID(COUNT, 1:5) = &
     &              PERSIST_MST_ID(PERSIST_I, 1:5)
                  CAND_SEC_GP_MASK(COUNT, 1:4) = &
     &              PERSIST_GP_MASK(PERSIST_I, 1:4)
                  CONT_ELEMENT(COUNT, 1:3, 1:8) = &
     &              PERSIST_CONT_ELEMENT(PERSIST_I, 1:3, 1:8)
                  CALL STS_PAIR_HASH_INSERT(CAND_SEC_SEG_ID(COUNT, 1), &
     &              CAND_MST_SEG_ID(COUNT, 1), COUNT, OUT_HASH_SIZE, &
     &              OUT_HASH_SEC, OUT_HASH_MST, OUT_HASH_INDEX, &
     &              HASH_INDEX)
                ENDDO

                DO FRESH_I = 1, COUNT_FRESH
                  IF (COUNT >= FINAL_LIMIT) EXIT
                  PAIR_EXISTS = STS_PAIR_HASH_CONTAINS( &
     &              FRESH_SEC_ID(FRESH_I, 1), FRESH_MST_ID(FRESH_I, 1), &
     &              OUT_HASH_SIZE, OUT_HASH_SEC, OUT_HASH_MST, &
     &              OUT_HASH_INDEX)
                  IF (PAIR_EXISTS) CYCLE

                  COUNT = COUNT + 1
                  CAND_SEC_SEG_ID(COUNT, 1:5) = &
     &              FRESH_SEC_ID(FRESH_I, 1:5)
                  CAND_MST_SEG_ID(COUNT, 1:5) = &
     &              FRESH_MST_ID(FRESH_I, 1:5)
                  CAND_SEC_GP_MASK(COUNT, 1:4) = &
     &              FRESH_GP_MASK(FRESH_I, 1:4)
                  CONT_ELEMENT(COUNT, 1:3, 1:8) = &
     &              FRESH_CONT_ELEMENT(FRESH_I, 1:3, 1:8)
                  CALL STS_PAIR_HASH_INSERT(CAND_SEC_SEG_ID(COUNT, 1), &
     &              CAND_MST_SEG_ID(COUNT, 1), COUNT, OUT_HASH_SIZE, &
     &              OUT_HASH_SEC, OUT_HASH_MST, OUT_HASH_INDEX, &
     &              HASH_INDEX)
                ENDDO

                CALL MY_DEALLOC(OUT_HASH_SEC)
                CALL MY_DEALLOC(OUT_HASH_MST)
                CALL MY_DEALLOC(OUT_HASH_INDEX)
                CALL MY_DEALLOC(FRESH_SEC_ID)
                CALL MY_DEALLOC(FRESH_MST_ID)
                CALL MY_DEALLOC(FRESH_GP_MASK)
                CALL MY_DEALLOC(FRESH_CONT_ELEMENT)
              ENDIF
              CALL MY_DEALLOC(FRESH_HASH_SEC)
              CALL MY_DEALLOC(FRESH_HASH_MST)
              CALL MY_DEALLOC(FRESH_HASH_INDEX)
            ENDIF

            CALL MY_DEALLOC(PERSIST_SEC_ID)
            CALL MY_DEALLOC(PERSIST_MST_ID)
            CALL MY_DEALLOC(PERSIST_GP_MASK)
            CALL MY_DEALLOC(PERSIST_CONT_ELEMENT)
          ENDIF
!
          CALL MY_DEALLOC(CAND_N_COMPACT)
          CALL MY_DEALLOC(CAND_E_COMPACT)
!
!         Saturated storage: signal the caller to grow capacity and retry.
          IF (COUNT >= MAX_STS_SIZE_ACTUAL) OVERFLOW = .TRUE.
!
          IF (COUNT > 0) THEN
!           Save the current STS segment patch for later candidate dropouts.
            CALL ISTS_STS_BP_PERSIST_SAVE(NIN, COUNT, CAND_SEC_SEG_ID, &
     &          CAND_MST_SEG_ID, CAND_SEC_GP_MASK, MAX_STS_SIZE_ACTUAL)
          ELSE
!           Fresh remap produced no pairs; try the last successful STS patch.
            CALL ISTS_STS_BP_PERSIST_TRY_RESTORE( &
     &          NIN, X, NUMNOD, MAX_STS_SIZE_ACTUAL, &
     &          COUNT, CAND_SEC_SEG_ID, CAND_MST_SEG_ID, &
     &          CAND_SEC_GP_MASK, CONT_ELEMENT, PERSIST_RESTORED)
            IF (PERSIST_RESTORED) THEN
              IF (COUNT >= MAX_STS_SIZE_ACTUAL) OVERFLOW = .TRUE.
              CALL MY_DEALLOC(CAND_SEC_SEG)
              RETURN
            END IF
          END IF
!
          CALL MY_DEALLOC(CAND_SEC_SEG)
        END SUBROUTINE STS_INT7_BUCKET_BROAD_PHASE

!=======================================================================
!   STS_PAIR_HASH_INSERT
!
!   Insert a secondary/master segment pair into an open-addressed hash.
!=======================================================================
        SUBROUTINE STS_PAIR_HASH_INSERT(SEC_SEG, MST_SEG, PAIR_INDEX_IN, &
     &    HASH_SIZE, HASH_SEC, HASH_MST, HASH_INDEX, HASH_INDEX_OUT)
          INTEGER, INTENT(IN) :: SEC_SEG, MST_SEG, PAIR_INDEX_IN
          INTEGER, INTENT(IN) :: HASH_SIZE
          INTEGER, INTENT(INOUT) :: HASH_SEC(HASH_SIZE)
          INTEGER, INTENT(INOUT) :: HASH_MST(HASH_SIZE)
          INTEGER, INTENT(INOUT) :: HASH_INDEX(HASH_SIZE)
          INTEGER, INTENT(INOUT) :: HASH_INDEX_OUT
          INTEGER :: IDX, PROBE
          INTEGER(KIND=8) :: HKEY

          HASH_INDEX_OUT = 0
          IF (SEC_SEG <= 0 .OR. MST_SEG <= 0) RETURN
          IF (HASH_SIZE <= 0) RETURN

          HKEY = INT(SEC_SEG, KIND=8) * INT(1000003, KIND=8) + &
     &      INT(MST_SEG, KIND=8)
          IDX = INT(MOD(HKEY, INT(HASH_SIZE, KIND=8))) + 1

          DO PROBE = 1, HASH_SIZE
            IF (HASH_INDEX(IDX) == 0) THEN
              HASH_SEC(IDX) = SEC_SEG
              HASH_MST(IDX) = MST_SEG
              HASH_INDEX(IDX) = PAIR_INDEX_IN
              HASH_INDEX_OUT = IDX
              RETURN
            ENDIF
            IF (HASH_SEC(IDX) == SEC_SEG .AND. &
     &          HASH_MST(IDX) == MST_SEG) THEN
              HASH_INDEX_OUT = IDX
              RETURN
            ENDIF
            IDX = IDX + 1
            IF (IDX > HASH_SIZE) IDX = 1
          ENDDO
        END SUBROUTINE STS_PAIR_HASH_INSERT

!=======================================================================
!   STS_PAIR_HASH_CONTAINS
!
!   Return whether an open-addressed hash contains a segment pair.
!=======================================================================
        LOGICAL FUNCTION STS_PAIR_HASH_CONTAINS(SEC_SEG, MST_SEG, &
     &    HASH_SIZE, HASH_SEC, HASH_MST, HASH_INDEX)
          INTEGER, INTENT(IN) :: SEC_SEG, MST_SEG, HASH_SIZE
          INTEGER, INTENT(IN) :: HASH_SEC(HASH_SIZE)
          INTEGER, INTENT(IN) :: HASH_MST(HASH_SIZE)
          INTEGER, INTENT(IN) :: HASH_INDEX(HASH_SIZE)
          INTEGER :: IDX, PROBE
          INTEGER(KIND=8) :: HKEY

          STS_PAIR_HASH_CONTAINS = .FALSE.
          IF (SEC_SEG <= 0 .OR. MST_SEG <= 0) RETURN
          IF (HASH_SIZE <= 0) RETURN

          HKEY = INT(SEC_SEG, KIND=8) * INT(1000003, KIND=8) + &
     &      INT(MST_SEG, KIND=8)
          IDX = INT(MOD(HKEY, INT(HASH_SIZE, KIND=8))) + 1

          DO PROBE = 1, HASH_SIZE
            IF (HASH_INDEX(IDX) == 0) RETURN
            IF (HASH_SEC(IDX) == SEC_SEG .AND. &
     &          HASH_MST(IDX) == MST_SEG) THEN
              STS_PAIR_HASH_CONTAINS = .TRUE.
              RETURN
            ENDIF
            IDX = IDX + 1
            IF (IDX > HASH_SIZE) IDX = 1
          ENDDO
        END FUNCTION STS_PAIR_HASH_CONTAINS
      END MODULE STS_BROAD_PHASE_INT7_BUCKET_MOD
