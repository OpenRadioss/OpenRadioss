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
!Copyright>        Commercial Alternative: Altair Radioss
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!||====================================================================
!||    q1np_dump_hist_state           ../engine/source/elements/solid/solid_q1np/q1np_dump_hist_state.F90
!||--- called by ------------------------------------------------------
!||    forint                           ../engine/source/elements/forint.F
!||--- uses       ------------------------------------------------------
!||    debug_mod                        ../engine/share/modules/debug_mod.F
!||    q1np_restart_mod                 ../common_source/modules/q1np_restart_mod.F90
!||    restmod                          ../engine/share/modules/restart_mod.F
!||====================================================================
! Dump Q1NP bulk and control-point node coordinates to CSV (debug / post-processing).
!=======================================================================
      SUBROUTINE Q1NP_DUMP_HIST_STATE(TIME_CUR, X, NUMNOD)
!-----------------------------------------------------------------------
!   Dump Q1NP bulk and control-point node coordinates to CSV files.
!   Only used for post-processing and debugging.
!
!   - q1np_bulk_history.csv : time, node_id, x, y, z for bulk nodes
!   - q1np_cp_history.csv   : time, node_id, x, y, z for control points
!   - q1np_nurbs_info.csv   : basic NURBS grid/degree information (once)
!
!   TIME_CUR : current physical time
!   X        : current nodal coordinates, dimension (3,NUMNOD)
!-----------------------------------------------------------------------
      USE Q1NP_RESTART_MOD
      USE DEBUG_MOD, ONLY: ITAB_DEBUG
      USE MY_ALLOC_MOD, ONLY: MY_ALLOC
      USE MY_DEALLOC_MOD, ONLY: MY_DEALLOC
      USE PRECISION_MOD, ONLY : WP


      implicit none
      real(kind=WP), INTENT(IN) :: TIME_CUR
      INTEGER, INTENT(IN) :: NUMNOD
      real(kind=WP), INTENT(IN) :: X(3,NUMNOD)
      INTEGER :: LUX_BULK
      INTEGER :: LUX_CP
      LOGICAL, SAVE :: FIRST_CALL     = .TRUE.
      LOGICAL, SAVE :: NURBS_WRITTEN  = .FALSE.
      INTEGER, ALLOCATABLE, SAVE :: SEEN_CP(:)
      INTEGER, ALLOCATABLE, SAVE :: SEEN_BULK(:)
      INTEGER, SAVE :: SEEN_SIZE = 0
      INTEGER, SAVE :: SEEN_TAG  = 0

      INTEGER :: IEL_Q1NP
      INTEGER :: P, Q, NCTRL, OFFSET_CTRL, OFFSET_BULK
      INTEGER :: K, LID, GID, ITMP, OUT_ID
      INTEGER :: MAX_GID

      INTEGER :: NX_LOC, NY_LOC
      INTEGER :: NX_FOUND, NY_FOUND
      INTEGER :: NX_CAND, NY_CAND
      INTEGER :: NKNOT_U, NKNOT_V

!     Initialize files and NURBS info on first call
      IF (FIRST_CALL) THEN
         OPEN(NEWUNIT=LUX_BULK, FILE='q1np_bulk_history.csv', &
              STATUS='REPLACE', ACTION='WRITE')
         WRITE(LUX_BULK,'(A)') 'time,node_id,x,y,z'
         CALL FLUSH(LUX_BULK)
         CLOSE(LUX_BULK)

         OPEN(NEWUNIT=LUX_CP, FILE='q1np_cp_history.csv', &
              STATUS='REPLACE', ACTION='WRITE')
         WRITE(LUX_CP,'(A)') 'time,node_id,x,y,z'
         CALL FLUSH(LUX_CP)
         CLOSE(LUX_CP)

         FIRST_CALL = .FALSE.
      ENDIF

!     Open CSV files for appending on every call to ensure data reaches disk
      OPEN(NEWUNIT=LUX_BULK, FILE='q1np_bulk_history.csv', &
           STATUS='OLD', ACTION='WRITE', POSITION='APPEND')
      OPEN(NEWUNIT=LUX_CP, FILE='q1np_cp_history.csv', &
           STATUS='OLD', ACTION='WRITE', POSITION='APPEND')

!     Write basic NURBS/grid info once (overwrite file if exists)
      IF (.NOT. NURBS_WRITTEN) THEN

!        Reconstruct NX,NY if missing, using same logic as Q1NP_FORC3
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

         OPEN(UNIT=99, FILE='q1np_nurbs_info.csv', &
              STATUS='REPLACE', ACTION='WRITE')
         P = KQ1NP_TAB(8,1)
         Q = KQ1NP_TAB(9,1)

         IF (Q1NP_NKNOT_SETS_G > 0 .AND. ALLOCATED(Q1NP_NX_SET_G)) THEN
            WRITE(99,'(A)') 'knot_set_id,NX,NY,P,Q,KTAB_OFF,KTAB_LEN'
            DO ITMP = 1, Q1NP_NKNOT_SETS_G
               WRITE(99,'(I10,1X,I10,1X,I10,1X,I5,1X,I5,1X,I10,1X,I10)') &
                    ITMP, Q1NP_NX_SET_G(ITMP), Q1NP_NY_SET_G(ITMP), P, Q, &
                    Q1NP_KTAB_OFF_G(ITMP), Q1NP_KTAB_LEN_G(ITMP)
            END DO
         ELSE
         WRITE(99,'(A)') 'NX,NY,P,Q,NUMELQ1NP,SQ1NPCTRL_SHARED,SQ1NPBULK,'// &
                         'SQ1NPKNOT_L'
         WRITE(99,'(I10,1X,I10,1X,I5,1X,I5,1X,I10,1X,I10,1X,I10,1X,I10)') &
              Q1NP_NX_G, Q1NP_NY_G, P, Q, &
              NUMELQ1NP_G, SQ1NPCTRL_SHARED_G, SQ1NPBULK_G, SQ1NPKNOT_L_G
         END IF
         CLOSE(99)

         NURBS_WRITTEN = .TRUE.
      ENDIF

!     Prepare per-call duplicate filters
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

      IF (SEEN_SIZE > 0) THEN
         SEEN_TAG = SEEN_TAG + 1
         IF (SEEN_TAG == HUGE(SEEN_TAG)) THEN
            SEEN_CP   = 0
            SEEN_BULK = 0
            SEEN_TAG  = 1
         END IF
      END IF

!     Loop over all Q1NP elements and dump CP + bulk coordinates
      DO IEL_Q1NP = 1, NUMELQ1NP_G

         P           = KQ1NP_TAB(8, IEL_Q1NP)
         Q           = KQ1NP_TAB(9, IEL_Q1NP)
         NCTRL       = KQ1NP_TAB(3, IEL_Q1NP)
         OFFSET_CTRL = KQ1NP_TAB(4, IEL_Q1NP)
         OFFSET_BULK = KQ1NP_TAB(14,IEL_Q1NP)

!        Control points for this element
         IF (NCTRL > 0) THEN
            DO K = 0, NCTRL-1
               GID = IQ1NP_TAB(OFFSET_CTRL + K)
               IF (GID <= 0) CYCLE
               IF (GID <= SEEN_SIZE) THEN
                  IF (SEEN_CP(GID) == SEEN_TAG) CYCLE
                  SEEN_CP(GID) = SEEN_TAG
               END IF

!              IQ1NP connectivity stores engine-local node indices.
               LID = GID
               IF (LID <= 0) CYCLE
               OUT_ID = GID
               IF (ALLOCATED(ITAB_DEBUG)) THEN
                  IF (LID <= SIZE(ITAB_DEBUG)) OUT_ID = ITAB_DEBUG(LID)
                     END IF

               WRITE(LUX_CP, &
                    '(ES23.15,1X,I10,3(1X,ES23.15))') &
                    TIME_CUR, OUT_ID, &
                    X(1,LID), X(2,LID), X(3,LID)
            END DO
         END IF

!        Bulk nodes for this element (4 hexa corners)
         DO K = 0, 3
            GID = IQ1NP_BULK_TAB(OFFSET_BULK + K)
            IF (GID <= 0) CYCLE
            IF (GID <= SEEN_SIZE) THEN
               IF (SEEN_BULK(GID) == SEEN_TAG) CYCLE
               SEEN_BULK(GID) = SEEN_TAG
            END IF

!           IQ1NP bulk connectivity stores engine-local node indices.
            LID = GID
            IF (LID <= 0) CYCLE
            OUT_ID = GID
            IF (ALLOCATED(ITAB_DEBUG)) THEN
               IF (LID <= SIZE(ITAB_DEBUG)) OUT_ID = ITAB_DEBUG(LID)
                  END IF

            WRITE(LUX_BULK, &
                 '(ES23.15,1X,I10,3(1X,ES23.15))') &
                 TIME_CUR, OUT_ID, &
                 X(1,LID), X(2,LID), X(3,LID)
         END DO

      END DO

!     Flush to ensure data is written
      CALL FLUSH(LUX_BULK)
      CALL FLUSH(LUX_CP)
      CLOSE(LUX_BULK)
      CLOSE(LUX_CP)

      RETURN
      END SUBROUTINE Q1NP_DUMP_HIST_STATE
