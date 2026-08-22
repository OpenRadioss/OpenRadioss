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
! Dump Q1NP bulk and control-point node coordinates to CSV (debug / post-processing).
!=======================================================================
!||====================================================================
!||    q1np_dump_hist_state           ../engine/source/elements/solid/solid_q1np/q1np_dump_hist_state.F90
!||--- called by ------------------------------------------------------
!||    forint                           ../engine/source/elements/forint.F
!||--- uses       ------------------------------------------------------
!||    debug_mod                        ../engine/share/modules/debug_mod.F
!||    q1np_restart_mod                 ../common_source/modules/q1np_restart_mod.F90
!||====================================================================
! Dump Q1NP CP/bulk/HEX coordinates to CSV (debug / post-processing).
!=======================================================================
      SUBROUTINE Q1NP_DUMP_HIST_STATE(TIME_CUR, X, NUMNOD, IXS, NIXS, NUMELS, IPARTS)
!-----------------------------------------------------------------------
!   Dump Q1NP geometry history at animation / H3D frames:
!
!   - q1np_bulk_history_<surf>.csv : bulk-node coordinates
!   - q1np_cp_history_<surf>.csv   : control-point coordinates
!   - hex8_elements_<surf>.csv     : HEX8 connectivity (once, excl. Q1NP parents)
!   - hex8_nodes_<surf>.csv        : HEX8 reference nodes (once)
!   - hex8_history_<surf>.csv      : moving HEX node coordinates
!   - q1np_nurbs_info.csv          : NURBS metadata (once)
!
!   Surface grouping uses KQ1NP_TAB(16,*) for the Q1NP CP/bulk history.
!   The HEX topology/history mirrors the starter export in
!   q1np_export_csv.F90: every distinct 8-node brick that is NOT the parent
!   HEX8 of a Q1NP element (KQ1NP_TAB(10,*)) is written. The same full HEX
!   set is written to each surface file; the presenter splits the bodies by
!   connectivity (flood fill from the surface bulk nodes).
!-----------------------------------------------------------------------
      USE Q1NP_RESTART_MOD
      USE DEBUG_MOD, ONLY: ITAB_DEBUG
      USE MY_ALLOC_MOD, ONLY: MY_ALLOC
      USE MY_DEALLOC_MOD, ONLY: MY_DEALLOC
      USE PRECISION_MOD, ONLY : WP

      IMPLICIT NONE
#include "my_real.inc"

      my_real, INTENT(IN) :: TIME_CUR
      INTEGER, INTENT(IN) :: NUMNOD
      INTEGER, INTENT(IN) :: NIXS
      INTEGER, INTENT(IN) :: NUMELS
      my_real, INTENT(IN) :: X(3,NUMNOD)
      INTEGER, INTENT(IN) :: IXS(NIXS,*)
      INTEGER, INTENT(IN) :: IPARTS(*)

      LOGICAL, PARAMETER :: Q1NP_DUMP_HIST_ENABLED = .FALSE.
      LOGICAL, SAVE :: FIRST_CALL = .TRUE.
      LOGICAL, SAVE :: NURBS_WRITTEN = .FALSE.
      LOGICAL, SAVE :: HEX_TOPO_WRITTEN = .FALSE.

      INTEGER, ALLOCATABLE, SAVE :: SEEN_CP(:)
      INTEGER, ALLOCATABLE, SAVE :: SEEN_BULK(:)
      INTEGER, SAVE :: SEEN_SIZE = 0
      INTEGER, SAVE :: SEEN_TAG = 0

      INTEGER :: IEL_Q1NP, IEL, SURF_ID
      INTEGER :: NCTRL, OFFSET_CTRL, OFFSET_BULK
      INTEGER :: K, LID, GID, ITMP, OUT_ID, MAX_GID
      INTEGER :: NSURF, ISURF, LUX, IOS, II, JJ
      INTEGER :: N8(8)
      INTEGER :: P, Q, NX_LOC, NY_LOC, NX_FOUND, NY_FOUND
      INTEGER :: NX_CAND, NY_CAND, NKNOT_U, NKNOT_V
      INTEGER :: SURF_LIST(64)
      INTEGER :: IEL_HEX
      INTEGER, ALLOCATABLE :: NODE_MARK(:)
      INTEGER, ALLOCATABLE :: SKIP_UNDER(:)
      CHARACTER(LEN=64) :: FNAME
      LOGICAL :: IS_BRICK8
      LOGICAL :: SURF_MATCH

      IF (.NOT. Q1NP_DUMP_HIST_ENABLED) RETURN
      IF (NUMELQ1NP_G <= 0) RETURN
      IF (.NOT. ALLOCATED(KQ1NP_TAB)) RETURN

      NSURF = 0
      DO IEL_Q1NP = 1, NUMELQ1NP_G
         SURF_ID = KQ1NP_TAB(16, IEL_Q1NP)
         IF (SURF_ID <= 0) SURF_ID = 0
         SURF_MATCH = .FALSE.
         DO ISURF = 1, NSURF
            IF (SURF_LIST(ISURF) == SURF_ID) THEN
               SURF_MATCH = .TRUE.
               EXIT
            END IF
         END DO
         IF (.NOT. SURF_MATCH .AND. NSURF < SIZE(SURF_LIST)) THEN
            NSURF = NSURF + 1
            SURF_LIST(NSURF) = SURF_ID
         END IF
      END DO
      IF (NSURF <= 0) THEN
         NSURF = 1
         SURF_LIST(1) = 0
      END IF

      IF (FIRST_CALL) THEN
         DO ISURF = 1, NSURF
            SURF_ID = SURF_LIST(ISURF)
            CALL Q1NP_HIST_OPEN_HEADER(SURF_ID, 'q1np_bulk_history', LUX)
            CALL Q1NP_HIST_OPEN_HEADER(SURF_ID, 'q1np_cp_history', LUX)
            CALL Q1NP_HIST_OPEN_HEADER(SURF_ID, 'hex8_history', LUX)
         END DO
         FIRST_CALL = .FALSE.
      END IF

      IF (.NOT. NURBS_WRITTEN) THEN
         IF (Q1NP_NX_G <= 0 .OR. Q1NP_NY_G <= 0) THEN
            P = KQ1NP_TAB(8,1)
            Q = KQ1NP_TAB(9,1)
            NX_FOUND = 0
            NY_FOUND = 0
            IF (SQ1NPCTRL_SHARED_G > 0 .AND. SQ1NPKNOT_L_G > 0) THEN
               DO NX_CAND = 1, SQ1NPCTRL_SHARED_G
                  IF (MOD(SQ1NPCTRL_SHARED_G, NX_CAND) /= 0) CYCLE
                  NY_CAND = SQ1NPCTRL_SHARED_G / NX_CAND
                  NX_LOC  = NX_CAND - P
                  NY_LOC  = NY_CAND - Q
                  IF (NX_LOC <= 0 .OR. NY_LOC <= 0) CYCLE
                  NKNOT_U = NX_LOC + 2*P + 1
                  NKNOT_V = NY_LOC + 2*Q + 1
                  IF (NKNOT_U + NKNOT_V == SQ1NPKNOT_L_G) THEN
                     NX_FOUND = NX_LOC
                     NY_FOUND = NY_LOC
                     EXIT
                  END IF
               END DO
            END IF
            IF (NX_FOUND > 0 .AND. NY_FOUND > 0) THEN
               Q1NP_NX_G = NX_FOUND
               Q1NP_NY_G = NY_FOUND
            END IF
         END IF

         OPEN(NEWUNIT=LUX, FILE='q1np_nurbs_info.csv', STATUS='REPLACE', &
              ACTION='WRITE', IOSTAT=IOS)
         IF (IOS == 0) THEN
            P = KQ1NP_TAB(8,1)
            Q = KQ1NP_TAB(9,1)
            IF (Q1NP_NKNOT_SETS_G > 0 .AND. ALLOCATED(Q1NP_NX_SET_G)) THEN
               WRITE(LUX,'(A)') &
                    'knot_set_id,NX,NY,P,Q,KTAB_OFF,KTAB_LEN,surface_id'
               DO ITMP = 1, Q1NP_NKNOT_SETS_G
                  SURF_ID = 0
                  DO IEL_Q1NP = 1, NUMELQ1NP_G
                     IF (KQ1NP_TAB(15, IEL_Q1NP) == ITMP) THEN
                        SURF_ID = KQ1NP_TAB(16, IEL_Q1NP)
                        EXIT
                     END IF
                  END DO
                  WRITE(LUX,'(I0,",",I0,",",I0,",",I0,",",I0,",",I0,",",I0,",",I0)') &
                       ITMP, Q1NP_NX_SET_G(ITMP), Q1NP_NY_SET_G(ITMP), &
                       P, Q, Q1NP_KTAB_OFF_G(ITMP), Q1NP_KTAB_LEN_G(ITMP), SURF_ID
               END DO
            ELSE
               WRITE(LUX,'(A)') &
                    'NX,NY,P,Q,NUMELQ1NP,SQ1NPCTRL_SHARED,SQ1NPBULK,SQ1NPKNOT_L'
               WRITE(LUX,'(I0,",",I0,",",I0,",",I0,",",I0,",",I0,",",I0,",",I0)') &
                    Q1NP_NX_G, Q1NP_NY_G, P, Q, &
                    NUMELQ1NP_G, SQ1NPCTRL_SHARED_G, SQ1NPBULK_G, SQ1NPKNOT_L_G
            END IF
            CLOSE(LUX)
         END IF
         NURBS_WRITTEN = .TRUE.
      END IF

      MAX_GID = 0
      DO IEL_Q1NP = 1, NUMELQ1NP_G
         NCTRL       = KQ1NP_TAB(3, IEL_Q1NP)
         OFFSET_CTRL = KQ1NP_TAB(4, IEL_Q1NP)
         OFFSET_BULK = KQ1NP_TAB(14,IEL_Q1NP)
         IF (NCTRL > 0) THEN
            DO K = 0, NCTRL-1
               GID = IQ1NP_TAB(OFFSET_CTRL + K)
               IF (GID > MAX_GID) MAX_GID = GID
            END DO
         END IF
         DO K = 0, 3
            GID = IQ1NP_BULK_TAB(OFFSET_BULK + K)
            IF (GID > MAX_GID) MAX_GID = GID
         END DO
      END DO

      IF (MAX_GID > SEEN_SIZE) THEN
         IF (ALLOCATED(SEEN_CP)) CALL MY_DEALLOC(SEEN_CP)
         IF (ALLOCATED(SEEN_BULK)) CALL MY_DEALLOC(SEEN_BULK)
         CALL MY_ALLOC(SEEN_CP,   MAX_GID, "SEEN_CP")
         CALL MY_ALLOC(SEEN_BULK, MAX_GID, "SEEN_BULK")
         SEEN_CP   = 0
         SEEN_BULK = 0
         SEEN_SIZE = MAX_GID
      END IF

!     Flag the HEX8 elements that are the parent of a Q1NP element so we can
!     omit them (they are represented by the NURBS band in the presenter).
      CALL MY_ALLOC(SKIP_UNDER, MAX(1,NUMELS), "SKIP_UNDER")
      SKIP_UNDER = 0
      DO IEL_Q1NP = 1, NUMELQ1NP_G
         IEL_HEX = KQ1NP_TAB(10, IEL_Q1NP)
         IF (IEL_HEX >= 1 .AND. IEL_HEX <= NUMELS) SKIP_UNDER(IEL_HEX) = 1
      END DO

!     Global HEX node set: every distinct 8-node brick that is not a Q1NP
!     parent. This is identical for all surfaces; the presenter separates the
!     bodies by connectivity from the per-surface bulk nodes.
      CALL MY_ALLOC(NODE_MARK, NUMNOD, "NODE_MARK")
      NODE_MARK = 0
      DO IEL = 1, NUMELS
         IF (SKIP_UNDER(IEL) == 1) CYCLE
         IS_BRICK8 = (NIXS >= 9)
         IF (IS_BRICK8) THEN
            DO II = 1, 8
               N8(II) = IXS(1 + II, IEL)
               IF (N8(II) <= 0 .OR. N8(II) > NUMNOD) IS_BRICK8 = .FALSE.
            END DO
         END IF
         IF (IS_BRICK8) THEN
            DO II = 1, 8
               DO JJ = II + 1, 8
                  IF (N8(II) == N8(JJ)) IS_BRICK8 = .FALSE.
               END DO
            END DO
         END IF
         IF (.NOT. IS_BRICK8) CYCLE
         DO II = 1, 8
            NODE_MARK(N8(II)) = 1
         END DO
      END DO

      DO ISURF = 1, NSURF
         SURF_ID = SURF_LIST(ISURF)
         SEEN_TAG = SEEN_TAG + 1
         IF (SEEN_TAG == HUGE(SEEN_TAG)) THEN
            IF (SEEN_SIZE > 0) THEN
               SEEN_CP   = 0
               SEEN_BULK = 0
            END IF
            SEEN_TAG = 1
         END IF

         CALL Q1NP_HIST_OPEN_APPEND(SURF_ID, 'q1np_cp_history', LUX, IOS)
         IF (IOS == 0) THEN
            DO IEL_Q1NP = 1, NUMELQ1NP_G
               SURF_MATCH = (KQ1NP_TAB(16, IEL_Q1NP) == SURF_ID) .OR. &
                            (SURF_ID == 0 .AND. KQ1NP_TAB(16, IEL_Q1NP) <= 0)
               IF (.NOT. SURF_MATCH) CYCLE
               NCTRL       = KQ1NP_TAB(3, IEL_Q1NP)
               OFFSET_CTRL = KQ1NP_TAB(4, IEL_Q1NP)
               IF (NCTRL <= 0) CYCLE
               DO K = 0, NCTRL-1
                  GID = IQ1NP_TAB(OFFSET_CTRL + K)
                  IF (GID <= 0 .OR. GID > NUMNOD) CYCLE
                  IF (GID <= SEEN_SIZE) THEN
                     IF (SEEN_CP(GID) == SEEN_TAG) CYCLE
                     SEEN_CP(GID) = SEEN_TAG
                  END IF
                  OUT_ID = GID
                  IF (ALLOCATED(ITAB_DEBUG)) THEN
                     IF (GID <= SIZE(ITAB_DEBUG)) OUT_ID = ITAB_DEBUG(GID)
                  END IF
                  WRITE(LUX,'(ES23.15,",",I0,",",ES23.15,",",ES23.15,",",ES23.15)') &
                       TIME_CUR, OUT_ID, X(1,GID), X(2,GID), X(3,GID)
               END DO
            END DO
            CALL FLUSH(LUX)
            CLOSE(LUX)
         END IF

         CALL Q1NP_HIST_OPEN_APPEND(SURF_ID, 'q1np_bulk_history', LUX, IOS)
         IF (IOS == 0) THEN
            DO IEL_Q1NP = 1, NUMELQ1NP_G
               SURF_MATCH = (KQ1NP_TAB(16, IEL_Q1NP) == SURF_ID) .OR. &
                            (SURF_ID == 0 .AND. KQ1NP_TAB(16, IEL_Q1NP) <= 0)
               IF (.NOT. SURF_MATCH) CYCLE
               OFFSET_BULK = KQ1NP_TAB(14, IEL_Q1NP)
               DO K = 0, 3
                  GID = IQ1NP_BULK_TAB(OFFSET_BULK + K)
                  IF (GID <= 0 .OR. GID > NUMNOD) CYCLE
                  IF (GID <= SEEN_SIZE) THEN
                     IF (SEEN_BULK(GID) == SEEN_TAG) CYCLE
                     SEEN_BULK(GID) = SEEN_TAG
                  END IF
                  OUT_ID = GID
                  IF (ALLOCATED(ITAB_DEBUG)) THEN
                     IF (GID <= SIZE(ITAB_DEBUG)) OUT_ID = ITAB_DEBUG(GID)
                  END IF
                  WRITE(LUX,'(ES23.15,",",I0,",",ES23.15,",",ES23.15,",",ES23.15)') &
                       TIME_CUR, OUT_ID, X(1,GID), X(2,GID), X(3,GID)
               END DO
            END DO
            CALL FLUSH(LUX)
            CLOSE(LUX)
         END IF

         IF (.NOT. HEX_TOPO_WRITTEN) THEN
            CALL Q1NP_HIST_NAME(SURF_ID, 'hex8_elements', FNAME)
            OPEN(NEWUNIT=LUX, FILE=TRIM(FNAME), STATUS='REPLACE', &
                 ACTION='WRITE', IOSTAT=IOS)
            IF (IOS == 0) THEN
               WRITE(LUX,'(A)') 'iel,elem_id,n1,n2,n3,n4,n5,n6,n7,n8'
               DO IEL = 1, NUMELS
                  IF (SKIP_UNDER(IEL) == 1) CYCLE

                  IS_BRICK8 = (NIXS >= 9)
                  IF (IS_BRICK8) THEN
                     DO II = 1, 8
                        N8(II) = IXS(1 + II, IEL)
                        IF (N8(II) <= 0 .OR. N8(II) > NUMNOD) IS_BRICK8 = .FALSE.
                     END DO
                  END IF
                  IF (IS_BRICK8) THEN
                     DO II = 1, 8
                        DO JJ = II + 1, 8
                           IF (N8(II) == N8(JJ)) IS_BRICK8 = .FALSE.
                        END DO
                     END DO
                  END IF
                  IF (.NOT. IS_BRICK8) CYCLE

!                 Write INTERNAL node numbers (not ITAB user ids) so the
!                 connectivity matches hex8_nodes / q1np_bulk_nodes, which the
!                 starter also exports with internal ids. IXS(11) stays the
!                 user element id (used only as a label by the presenter).
                  WRITE(LUX,'(I0,",",I0,8(",",I0))') &
                       IEL, IXS(11, IEL), &
                       N8(1), N8(2), N8(3), N8(4), N8(5), N8(6), N8(7), N8(8)
               END DO
               CLOSE(LUX)
            END IF

            CALL Q1NP_HIST_NAME(SURF_ID, 'hex8_nodes', FNAME)
            OPEN(NEWUNIT=LUX, FILE=TRIM(FNAME), STATUS='REPLACE', &
                 ACTION='WRITE', IOSTAT=IOS)
            IF (IOS == 0) THEN
               WRITE(LUX,'(A)') 'node_id,x,y,z'
               DO LID = 1, NUMNOD
                  IF (NODE_MARK(LID) == 0) CYCLE
!                 Internal node id (matches hex8_elements and q1np_bulk_nodes).
                  WRITE(LUX,'(I0,",",ES23.15,",",ES23.15,",",ES23.15)') &
                       LID, X(1,LID), X(2,LID), X(3,LID)
               END DO
               CLOSE(LUX)
            END IF
         END IF

         CALL Q1NP_HIST_OPEN_APPEND(SURF_ID, 'hex8_history', LUX, IOS)
         IF (IOS == 0) THEN
            DO LID = 1, NUMNOD
               IF (NODE_MARK(LID) == 0) CYCLE
!              Internal node id (matches hex8_nodes / hex8_elements).
               WRITE(LUX,'(ES23.15,",",I0,",",ES23.15,",",ES23.15,",",ES23.15)') &
                    TIME_CUR, LID, X(1,LID), X(2,LID), X(3,LID)
            END DO
            CALL FLUSH(LUX)
            CLOSE(LUX)
         END IF
      END DO

      HEX_TOPO_WRITTEN = .TRUE.
      CALL MY_DEALLOC(SKIP_UNDER)
      CALL MY_DEALLOC(NODE_MARK)
      RETURN
      END SUBROUTINE Q1NP_DUMP_HIST_STATE


!=======================================================================
      SUBROUTINE Q1NP_HIST_NAME(SURF_ID, PREFIX, FNAME)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: SURF_ID
      CHARACTER(LEN=*), INTENT(IN) :: PREFIX
      CHARACTER(LEN=*), INTENT(OUT) :: FNAME
      CHARACTER(LEN=32) :: SUFFIX
      IF (SURF_ID > 0) THEN
         WRITE(SUFFIX,'(I0)') SURF_ID
         FNAME = TRIM(PREFIX)//'_'//TRIM(SUFFIX)//'.csv'
      ELSE
         FNAME = TRIM(PREFIX)//'.csv'
      END IF
      END SUBROUTINE Q1NP_HIST_NAME


!=======================================================================
      SUBROUTINE Q1NP_HIST_OPEN_HEADER(SURF_ID, PREFIX, LUX)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: SURF_ID
      CHARACTER(LEN=*), INTENT(IN) :: PREFIX
      INTEGER, INTENT(OUT) :: LUX
      CHARACTER(LEN=64) :: FNAME
      INTEGER :: IOS
      CALL Q1NP_HIST_NAME(SURF_ID, PREFIX, FNAME)
      OPEN(NEWUNIT=LUX, FILE=TRIM(FNAME), STATUS='REPLACE', &
           ACTION='WRITE', IOSTAT=IOS)
      IF (IOS == 0) THEN
         WRITE(LUX,'(A)') 'time,node_id,x,y,z'
         CALL FLUSH(LUX)
         CLOSE(LUX)
      END IF
      LUX = -1
      END SUBROUTINE Q1NP_HIST_OPEN_HEADER


!=======================================================================
      SUBROUTINE Q1NP_HIST_OPEN_APPEND(SURF_ID, PREFIX, LUX, IOS)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: SURF_ID
      CHARACTER(LEN=*), INTENT(IN) :: PREFIX
      INTEGER, INTENT(OUT) :: LUX
      INTEGER, INTENT(OUT) :: IOS
      CHARACTER(LEN=64) :: FNAME
      CALL Q1NP_HIST_NAME(SURF_ID, PREFIX, FNAME)
      OPEN(NEWUNIT=LUX, FILE=TRIM(FNAME), STATUS='OLD', &
           ACTION='WRITE', POSITION='APPEND', IOSTAT=IOS)
      IF (IOS /= 0) THEN
         OPEN(NEWUNIT=LUX, FILE=TRIM(FNAME), STATUS='REPLACE', &
              ACTION='WRITE', IOSTAT=IOS)
         IF (IOS == 0) WRITE(LUX,'(A)') 'time,node_id,x,y,z'
      END IF
      END SUBROUTINE Q1NP_HIST_OPEN_APPEND
