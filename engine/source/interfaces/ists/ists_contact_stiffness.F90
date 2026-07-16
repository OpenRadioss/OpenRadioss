!||====================================================================
!||    STS_CONTACT_STIFFNESS  ../engine/source/interfaces/ists/ists_contact_stiffness.F90
!||--- called by ------------------------------------------------------
!||    i7mainf                ../engine/source/interfaces/int07/i7mainf.F
!||====================================================================
      MODULE STS_CONTACT_STIFFNESS_MOD
        USE PRECISION_MOD, ONLY : WP
        USE MY_ALLOC_MOD, ONLY : MY_ALLOC
        USE MY_DEALLOC_MOD, ONLY : MY_DEALLOC
        IMPLICIT NONE
        PRIVATE

        INTEGER, PARAMETER :: STS_HASH_MIN_SLOTS = 1024
        REAL(KIND=WP), PARAMETER :: STS_EPS_STIFF = 1.0E-30_WP

        INTEGER, ALLOCATABLE, SAVE :: NSV_CACHE(:)
        INTEGER, ALLOCATABLE, SAVE :: NODE_TO_STFNS_CACHE(:)
        INTEGER, SAVE :: NODE_MAP_NUMNOD = 0
        INTEGER, SAVE :: NODE_MAP_NSN = 0
        LOGICAL, SAVE :: NODE_MAP_READY = .FALSE.

        PUBLIC :: STS_CONTACT_STIFFNESS

      CONTAINS

!=======================================================================
!   STS_CONTACT_STIFFNESS
!
!   Compute one penalty stiffness per STS candidate pair from primary
!   segment stiffness and active secondary nodal stiffness values.
!=======================================================================
        SUBROUTINE STS_CONTACT_STIFFNESS( &
     &      CAND_MST_SEG_ID, CAND_SEC_SEG_ID, COUNT, MAX_STS_SIZE, &
     &      CAND_SEC_GP_MASK, &
     &      IRECTM, STFM, NRTM, NSV, STFNS, NSN, NUMNOD, &
     &      IGSTI, KMIN, KMAX, STIGLO, STIF)
          INTEGER, INTENT(IN) :: COUNT, MAX_STS_SIZE
          INTEGER, INTENT(IN) :: NRTM, NSN, NUMNOD, IGSTI
          INTEGER, INTENT(IN) :: CAND_MST_SEG_ID(MAX_STS_SIZE,5)
          INTEGER, INTENT(IN) :: CAND_SEC_SEG_ID(MAX_STS_SIZE,5)
          INTEGER, INTENT(IN) :: CAND_SEC_GP_MASK(MAX_STS_SIZE,4)
          INTEGER, INTENT(IN) :: IRECTM(:)
          INTEGER, INTENT(IN) :: NSV(:)
          REAL(KIND=WP), INTENT(IN) :: STFM(:)
          REAL(KIND=WP), INTENT(IN) :: STFNS(:)
          REAL(KIND=WP), INTENT(IN) :: KMIN, KMAX, STIGLO
          REAL(KIND=WP), INTENT(INOUT) :: STIF(MAX_STS_SIZE)

          INTEGER :: I, MST_SEG
          INTEGER :: NSN_EFF, NRTM_EFF, NPAIR_EFF, HASH_SIZE
          INTEGER, ALLOCATABLE :: SEG_HASH_KEYS(:,:), SEG_HASH_VALS(:)
          REAL(KIND=WP) :: K_PRIMARY, K_SECONDARY, SECONDARY_FALLBACK
          LOGICAL :: NEED_PRIMARY_LOOKUP

          NPAIR_EFF = MIN(COUNT, MAX_STS_SIZE)
          IF (NPAIR_EFF <= 0) RETURN
          STIF(1:NPAIR_EFF) = 0.0_WP

          ! Istf=1: constant interface stiffness (STIGLO = -STFAC > 0 in engine).
          ! eval_pair uses d1 = 0.5*STIF*FAC; store 2*STIGLO to match NTS (STIGLO*FAC).
          IF (STIGLO > STS_EPS_STIFF) THEN
            STIF(1:NPAIR_EFF) = 2.0_WP * STIGLO
            RETURN
          END IF

          NSN_EFF = MIN(NSN, SIZE(NSV), SIZE(STFNS))
          NRTM_EFF = MIN(NRTM, SIZE(STFM), SIZE(IRECTM) / 4)
          SECONDARY_FALLBACK = STS_CONTACT_AVERAGE_POSITIVE( &
     &      STFNS, NSN_EFF, ABS(STIGLO))

          NEED_PRIMARY_LOOKUP = .FALSE.
          DO I = 1, NPAIR_EFF
            MST_SEG = CAND_MST_SEG_ID(I, 1)
            IF (MST_SEG <= 0 .OR. MST_SEG > NRTM_EFF) THEN
              NEED_PRIMARY_LOOKUP = .TRUE.
              EXIT
            ENDIF
          ENDDO

          HASH_SIZE = 0
          IF (NEED_PRIMARY_LOOKUP) THEN
            HASH_SIZE = MAX(STS_HASH_MIN_SLOTS, &
     &        2 * MAX(1, NRTM_EFF) + 1)
            CALL MY_ALLOC(SEG_HASH_KEYS, 4, HASH_SIZE, "SEG_HASH_KEYS")
            CALL MY_ALLOC(SEG_HASH_VALS, HASH_SIZE, "SEG_HASH_VALS")
            CALL STS_CONTACT_BUILD_SEG_HASH( &
     &        IRECTM, NRTM_EFF, SEG_HASH_KEYS, SEG_HASH_VALS, HASH_SIZE)
          ENDIF

          CALL STS_CONTACT_ENSURE_NODE_MAP(NSV, NSN_EFF, NUMNOD)

          DO I = 1, NPAIR_EFF
            MST_SEG = CAND_MST_SEG_ID(I, 1)
            IF (MST_SEG <= 0 .OR. MST_SEG > NRTM_EFF) THEN
              MST_SEG = STS_CONTACT_FIND_PRIMARY_SEG_HASH( &
     &          CAND_MST_SEG_ID(I, 2:5), CAND_MST_SEG_ID(I, 1), &
     &          IRECTM, NRTM_EFF, SEG_HASH_KEYS, SEG_HASH_VALS, &
     &          HASH_SIZE)
            ENDIF
            
            K_PRIMARY = STS_CONTACT_PRIMARY_STIFFNESS( &
     &        MST_SEG, STFM, NRTM_EFF)
            IF (K_PRIMARY <= STS_EPS_STIFF) CYCLE

            K_SECONDARY = STS_CONTACT_SECONDARY_STIFFNESS( &
     &        CAND_SEC_SEG_ID(I, 2:5), CAND_SEC_GP_MASK(I, 1:4), &
     &        NODE_TO_STFNS_CACHE, STFNS, &
     &        NSN_EFF, NUMNOD, SECONDARY_FALLBACK)
            IF (K_SECONDARY <= STS_EPS_STIFF) CYCLE
            
            STIF(I) = STS_CONTACT_STIFFNESS_VALUE( &
     &        K_PRIMARY, K_SECONDARY, IGSTI, KMIN, KMAX)
          END DO

          IF (NEED_PRIMARY_LOOKUP) THEN
            CALL MY_DEALLOC(SEG_HASH_KEYS)
            CALL MY_DEALLOC(SEG_HASH_VALS)
          ENDIF
        END SUBROUTINE STS_CONTACT_STIFFNESS

!=======================================================================
!   STS_CONTACT_ENSURE_NODE_MAP
!
!   Cache the NSV node-id to STFNS index map while the secondary node
!   list is unchanged.
!=======================================================================
        SUBROUTINE STS_CONTACT_ENSURE_NODE_MAP(NSV, NSN_EFF, NUMNOD)
          INTEGER, INTENT(IN) :: NSV(:)
          INTEGER, INTENT(IN) :: NSN_EFF, NUMNOD

          INTEGER :: I, NODE_ID
          LOGICAL :: REBUILD

          REBUILD = .NOT. NODE_MAP_READY
          IF (.NOT. REBUILD) THEN
            REBUILD = NODE_MAP_NUMNOD /= NUMNOD .OR. &
     &        NODE_MAP_NSN /= NSN_EFF
          ENDIF
          IF (.NOT. REBUILD .AND. NSN_EFF > 0) THEN
            REBUILD = ANY(NSV_CACHE(1:NSN_EFF) /= NSV(1:NSN_EFF))
          ENDIF
          IF (.NOT. REBUILD) RETURN

          IF (ALLOCATED(NSV_CACHE)) CALL MY_DEALLOC(NSV_CACHE)
          IF (ALLOCATED(NODE_TO_STFNS_CACHE)) CALL MY_DEALLOC(NODE_TO_STFNS_CACHE)
          CALL MY_ALLOC(NSV_CACHE, MAX(1, NSN_EFF), "NSV_CACHE")
          CALL MY_ALLOC(NODE_TO_STFNS_CACHE, MAX(1, NUMNOD), "NODE_TO_STFNS_CACHE")
          NSV_CACHE = 0
          NODE_TO_STFNS_CACHE = 0

          IF (NSN_EFF > 0) THEN
            NSV_CACHE(1:NSN_EFF) = NSV(1:NSN_EFF)
          ENDIF

          DO I = 1, NSN_EFF
            NODE_ID = NSV(I)
            IF (NODE_ID > 0 .AND. NODE_ID <= NUMNOD) THEN
              NODE_TO_STFNS_CACHE(NODE_ID) = I
            ENDIF
          ENDDO

          NODE_MAP_NUMNOD = NUMNOD
          NODE_MAP_NSN = NSN_EFF
          NODE_MAP_READY = .TRUE.
        END SUBROUTINE STS_CONTACT_ENSURE_NODE_MAP

!=======================================================================
!   STS_CONTACT_PRIMARY_STIFFNESS
!
!   Return primary segment stiffness for a valid master segment id.
!=======================================================================
        REAL(KIND=WP) FUNCTION STS_CONTACT_PRIMARY_STIFFNESS( &
     &      MST_SEG, STFM, NRTM_EFF)
          INTEGER, INTENT(IN) :: MST_SEG, NRTM_EFF
          REAL(KIND=WP), INTENT(IN) :: STFM(:)

          STS_CONTACT_PRIMARY_STIFFNESS = 0.0_WP
          IF (MST_SEG > 0 .AND. MST_SEG <= NRTM_EFF) THEN
            STS_CONTACT_PRIMARY_STIFFNESS = STFM(MST_SEG)
          END IF
        END FUNCTION STS_CONTACT_PRIMARY_STIFFNESS

!=======================================================================
!   STS_CONTACT_SECONDARY_STIFFNESS
!   Average of positive mapped STFNS values on the active secondary nodes.
!   A zero mask falls back to the legacy all-corner average.
!=======================================================================
        REAL(KIND=WP) FUNCTION STS_CONTACT_SECONDARY_STIFFNESS( &
     &      SEC_NODE_IDS, ACTIVE_MASK, NODE_TO_STFNS, STFNS, &
     &      NSN_EFF, NUMNOD, FALLBACK)
          INTEGER, INTENT(IN) :: SEC_NODE_IDS(4)
          INTEGER, INTENT(IN) :: ACTIVE_MASK(4)
          INTEGER, INTENT(IN) :: NODE_TO_STFNS(:)
          REAL(KIND=WP), INTENT(IN) :: STFNS(:)
          INTEGER, INTENT(IN) :: NSN_EFF, NUMNOD
          REAL(KIND=WP), INTENT(IN) :: FALLBACK

          INTEGER :: J, IDX, NODE_ID, NVALID, N_ACTIVE
          REAL(KIND=WP) :: K_SUM, K_NODE

          K_SUM = 0.0_WP
          NVALID = 0
          N_ACTIVE = COUNT(ACTIVE_MASK(:) /= 0)
          ! NVALID is the number of valid mapped secondary stiffness values.
          DO J = 1, 4
            IF (N_ACTIVE > 0 .AND. ACTIVE_MASK(J) == 0) CYCLE
            NODE_ID = SEC_NODE_IDS(J)
            IF (NODE_ID <= 0 .OR. NODE_ID > NUMNOD) CYCLE

            IDX = NODE_TO_STFNS(NODE_ID)
            IF (IDX <= 0 .OR. IDX > NSN_EFF) CYCLE

            K_NODE = ABS(STFNS(IDX))
            IF (K_NODE <= STS_EPS_STIFF) CYCLE
            
            ! Sum only valid mapped secondary stiffness values.
            K_SUM = K_SUM + K_NODE
            NVALID = NVALID + 1
          END DO

          IF (NVALID > 0) THEN
            ! Average the valid mapped secondary stiffness values.
            STS_CONTACT_SECONDARY_STIFFNESS = K_SUM / REAL(NVALID, WP)
          ELSE
            ! If no valid mapped secondary stiffness values, use the fallback value.
            STS_CONTACT_SECONDARY_STIFFNESS = FALLBACK
          END IF
        END FUNCTION STS_CONTACT_SECONDARY_STIFFNESS

!=======================================================================
!   STS_CONTACT_BUILD_SEG_HASH
!
!   Build a hash from canonical master-segment node ids to segment id.
!=======================================================================
        SUBROUTINE STS_CONTACT_BUILD_SEG_HASH( &
     &      IRECTM, NRTM, HASH_KEYS, HASH_VALS, HASH_SIZE)
          INTEGER, INTENT(IN) :: IRECTM(:)
          INTEGER, INTENT(IN) :: NRTM, HASH_SIZE
          INTEGER, INTENT(INOUT) :: HASH_KEYS(4, HASH_SIZE)
          INTEGER, INTENT(INOUT) :: HASH_VALS(HASH_SIZE)

          INTEGER :: SEG, K, IH
          INTEGER :: KEY(4), KEY_SORTED(4)
          LOGICAL :: KEY_MATCH

          HASH_KEYS = 0
          HASH_VALS = 0
          IF (NRTM <= 0 .OR. HASH_SIZE <= 0) RETURN

          DO SEG = 1, NRTM
            DO K = 1, 4
              KEY(K) = IRECTM(4 * (SEG - 1) + K)
            END DO
            CALL STS_CONTACT_CANONICAL_NODES(KEY, KEY_SORTED)
            IF (.NOT. STS_CONTACT_VALID_SEG_KEY(KEY_SORTED)) CYCLE

            CALL STS_CONTACT_HASH_PROBE(KEY_SORTED, HASH_SIZE, HASH_KEYS, &
     &        HASH_VALS, IH, KEY_MATCH)
            IF (KEY_MATCH) CYCLE

            IF (HASH_VALS(IH) == 0) THEN
              HASH_KEYS(:, IH) = KEY_SORTED(:)
              HASH_VALS(IH) = SEG
            END IF
          END DO
        END SUBROUTINE STS_CONTACT_BUILD_SEG_HASH

!=======================================================================
!   STS_CONTACT_FIND_PRIMARY_SEG_HASH
!
!   Resolve a primary segment id from candidate node ids using the hash,
!   with a linear fallback for unexpected hash misses.
!=======================================================================
        INTEGER FUNCTION STS_CONTACT_FIND_PRIMARY_SEG_HASH( &
     &      PRIMARY_NODES, CAND_SEG, IRECTM, NRTM, HASH_KEYS, &
     &      HASH_VALS, HASH_SIZE)
          INTEGER, INTENT(IN) :: PRIMARY_NODES(4)
          INTEGER, INTENT(IN) :: CAND_SEG, NRTM, HASH_SIZE
          INTEGER, INTENT(IN) :: IRECTM(:)
          INTEGER, INTENT(IN) :: HASH_KEYS(4, HASH_SIZE)
          INTEGER, INTENT(IN) :: HASH_VALS(HASH_SIZE)

          INTEGER :: KEY(4), IH
          LOGICAL :: KEY_MATCH

          STS_CONTACT_FIND_PRIMARY_SEG_HASH = 0

          IF (CAND_SEG > 0 .AND. CAND_SEG <= NRTM) THEN
            IF (STS_CONTACT_SAME_SEG_NODES(PRIMARY_NODES, IRECTM, CAND_SEG)) THEN
              STS_CONTACT_FIND_PRIMARY_SEG_HASH = CAND_SEG
              RETURN
            END IF
          END IF

          IF (NRTM <= 0 .OR. HASH_SIZE <= 0) RETURN
          CALL STS_CONTACT_CANONICAL_NODES(PRIMARY_NODES, KEY)
          IF (.NOT. STS_CONTACT_VALID_SEG_KEY(KEY)) RETURN

          CALL STS_CONTACT_HASH_PROBE(KEY, HASH_SIZE, HASH_KEYS, &
     &      HASH_VALS, IH, KEY_MATCH)
          IF (KEY_MATCH) THEN
            STS_CONTACT_FIND_PRIMARY_SEG_HASH = HASH_VALS(IH)
            RETURN
          END IF

          STS_CONTACT_FIND_PRIMARY_SEG_HASH = STS_CONTACT_FIND_PRIMARY_SEG( &
     &      PRIMARY_NODES, CAND_SEG, IRECTM, NRTM)
        END FUNCTION STS_CONTACT_FIND_PRIMARY_SEG_HASH

!=======================================================================
!   STS_CONTACT_HASH_PROBE
!   Open-addressing lookup: KEY_MATCH=.TRUE. when slot holds KEY.
!=======================================================================
        SUBROUTINE STS_CONTACT_HASH_PROBE(KEY, HASH_SIZE, HASH_KEYS, &
     &      HASH_VALS, IH, KEY_MATCH)
          INTEGER, INTENT(IN) :: KEY(4), HASH_SIZE
          INTEGER, INTENT(IN) :: HASH_KEYS(4, HASH_SIZE)
          INTEGER, INTENT(IN) :: HASH_VALS(HASH_SIZE)
          INTEGER, INTENT(INOUT) :: IH
          LOGICAL, INTENT(INOUT) :: KEY_MATCH

          INTEGER :: PROBE

          IH = STS_CONTACT_SEG_HASH(KEY, HASH_SIZE)
          KEY_MATCH = .FALSE.
          PROBE = 0
          DO WHILE (HASH_VALS(IH) /= 0 .AND. PROBE < HASH_SIZE)
            IF (ALL(KEY(:) == HASH_KEYS(:, IH))) THEN
              KEY_MATCH = .TRUE.
              RETURN
            END IF
            IH = IH + 1
            IF (IH > HASH_SIZE) IH = 1
            PROBE = PROBE + 1
          END DO
        END SUBROUTINE STS_CONTACT_HASH_PROBE

!=======================================================================
!   STS_CONTACT_CANONICAL_NODES
!
!   Sort four node ids so segment-node comparison is order independent.
!=======================================================================
        SUBROUTINE STS_CONTACT_CANONICAL_NODES(NODES_IN, NODES_OUT)
          INTEGER, INTENT(IN) :: NODES_IN(4)
          INTEGER, INTENT(INOUT) :: NODES_OUT(4)
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
        END SUBROUTINE STS_CONTACT_CANONICAL_NODES

!=======================================================================
!   STS_CONTACT_VALID_SEG_KEY
!
!   Return true when all segment key node ids are positive.
!=======================================================================
        LOGICAL FUNCTION STS_CONTACT_VALID_SEG_KEY(KEY)
          INTEGER, INTENT(IN) :: KEY(4)

          STS_CONTACT_VALID_SEG_KEY = ALL(KEY(:) > 0)
        END FUNCTION STS_CONTACT_VALID_SEG_KEY

!=======================================================================
!   STS_CONTACT_SEG_HASH
!
!   Compute a 1-based open-addressing hash slot for a canonical key.
!=======================================================================
        INTEGER FUNCTION STS_CONTACT_SEG_HASH(KEY, HASH_SIZE)
          INTEGER, INTENT(IN) :: KEY(4), HASH_SIZE
          INTEGER :: I
          INTEGER(KIND=8) :: H

          H = 0_8
          DO I = 1, 4
            H = MOD(H * INT(1315423911, KIND=8) + &
     &        INT(KEY(I), KIND=8), INT(HASH_SIZE, KIND=8))
          END DO
          STS_CONTACT_SEG_HASH = INT(H) + 1
        END FUNCTION STS_CONTACT_SEG_HASH

!=======================================================================
!   STS_CONTACT_FIND_PRIMARY_SEG
!
!   Linear fallback that searches IRECTM for a segment with the same nodes.
!=======================================================================
        INTEGER FUNCTION STS_CONTACT_FIND_PRIMARY_SEG( &
     &      PRIMARY_NODES, CAND_SEG, IRECTM, NRTM)
          INTEGER, INTENT(IN) :: PRIMARY_NODES(4)
          INTEGER, INTENT(IN) :: CAND_SEG, NRTM
          INTEGER, INTENT(IN) :: IRECTM(:)
          INTEGER :: SEG

          STS_CONTACT_FIND_PRIMARY_SEG = 0

          IF (CAND_SEG > 0 .AND. CAND_SEG <= NRTM) THEN
            IF (STS_CONTACT_SAME_SEG_NODES(PRIMARY_NODES, IRECTM, CAND_SEG)) THEN
              STS_CONTACT_FIND_PRIMARY_SEG = CAND_SEG
              RETURN
            END IF
          END IF

          DO SEG = 1, NRTM
            IF (SEG == CAND_SEG) CYCLE
            IF (STS_CONTACT_SAME_SEG_NODES(PRIMARY_NODES, IRECTM, SEG)) THEN
              STS_CONTACT_FIND_PRIMARY_SEG = SEG
              RETURN
            END IF
          END DO
        END FUNCTION STS_CONTACT_FIND_PRIMARY_SEG

!=======================================================================
!   STS_CONTACT_SAME_SEG_NODES
!
!   Return true when a master segment contains exactly the given node ids.
!=======================================================================
        LOGICAL FUNCTION STS_CONTACT_SAME_SEG_NODES(PRIMARY_NODES, IRECTM, SEG)
          INTEGER, INTENT(IN) :: PRIMARY_NODES(4)
          INTEGER, INTENT(IN) :: IRECTM(:)
          INTEGER, INTENT(IN) :: SEG
          INTEGER :: I, J, NODE_ID
          LOGICAL :: FOUND

          STS_CONTACT_SAME_SEG_NODES = .FALSE.

          DO I = 1, 4
            NODE_ID = PRIMARY_NODES(I)
            IF (NODE_ID <= 0) RETURN
            FOUND = .FALSE.
            DO J = 1, 4
              IF (NODE_ID == IRECTM(4 * (SEG - 1) + J)) THEN
                FOUND = .TRUE.
                EXIT
              END IF
            END DO
            IF (.NOT. FOUND) RETURN
          END DO

          STS_CONTACT_SAME_SEG_NODES = .TRUE.
        END FUNCTION STS_CONTACT_SAME_SEG_NODES

!=======================================================================
!   STS_CONTACT_STIFFNESS_VALUE
!   INT7 IGSTI policy (aligned with Q1NP_CONTACT_STIFFNESS).
!=======================================================================
        REAL(KIND=WP) FUNCTION STS_CONTACT_STIFFNESS_VALUE(K_PRIMARY, K_SECONDARY, IGSTI, KMIN, KMAX)
          REAL(KIND=WP), INTENT(IN) :: K_PRIMARY, K_SECONDARY
          REAL(KIND=WP), INTENT(IN) :: KMIN, KMAX
          INTEGER, INTENT(IN) :: IGSTI
          REAL(KIND=WP) :: K_SEC

          K_SEC = ABS(K_SECONDARY)
          
          ! IGSTI is the interface stiffness selection flag (how primary and secondary are combined).
          ! IGSTI = 1: product
          ! IGSTI = 2: average
          ! IGSTI = 3: maximum
          ! IGSTI = 4: minimum
          ! IGSTI = 5: harmonic-like blend
          SELECT CASE (IGSTI)
          CASE (:1)
            STS_CONTACT_STIFFNESS_VALUE = K_PRIMARY * K_SEC
          CASE (2)
            STS_CONTACT_STIFFNESS_VALUE = MAX(KMIN, MIN(0.5_WP * (K_PRIMARY + K_SEC), KMAX))
          CASE (3)
            STS_CONTACT_STIFFNESS_VALUE = MAX(KMIN, MIN(MAX(K_PRIMARY, K_SEC), KMAX))
          CASE (4)
            STS_CONTACT_STIFFNESS_VALUE = MAX(KMIN, MIN(MIN(K_PRIMARY, K_SEC), KMAX))
          CASE (5)
            STS_CONTACT_STIFFNESS_VALUE = MAX(KMIN, MIN( &
     &        K_PRIMARY * K_SEC / MAX(STS_EPS_STIFF, K_PRIMARY + K_SEC), KMAX))
          CASE DEFAULT
            STS_CONTACT_STIFFNESS_VALUE = K_PRIMARY * K_SEC
          END SELECT
        END FUNCTION STS_CONTACT_STIFFNESS_VALUE

!=======================================================================
!   STS_CONTACT_AVERAGE_POSITIVE
!
!   Average positive absolute values, or return FALLBACK when none exist.
!=======================================================================
        REAL(KIND=WP) FUNCTION STS_CONTACT_AVERAGE_POSITIVE( &
     &      VALUES, N_EFF, FALLBACK)
          REAL(KIND=WP), INTENT(IN) :: VALUES(:)
          INTEGER, INTENT(IN) :: N_EFF
          REAL(KIND=WP), INTENT(IN) :: FALLBACK

          INTEGER :: I, NLOC, COUNT_POS
          REAL(KIND=WP) :: SUM_POS, VABS

          STS_CONTACT_AVERAGE_POSITIVE = FALLBACK
          NLOC = MIN(N_EFF, SIZE(VALUES))
          IF (NLOC <= 0) RETURN

          SUM_POS = 0.0_WP
          COUNT_POS = 0
          DO I = 1, NLOC
            VABS = ABS(VALUES(I))
            IF (VABS <= STS_EPS_STIFF) CYCLE
            SUM_POS = SUM_POS + VABS
            COUNT_POS = COUNT_POS + 1
          END DO

          IF (COUNT_POS > 0) THEN
            STS_CONTACT_AVERAGE_POSITIVE = SUM_POS / REAL(COUNT_POS, WP)
          END IF
        END FUNCTION STS_CONTACT_AVERAGE_POSITIVE

      END MODULE STS_CONTACT_STIFFNESS_MOD
