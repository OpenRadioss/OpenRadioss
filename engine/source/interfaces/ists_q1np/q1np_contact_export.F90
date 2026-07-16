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
!||    q1np_contact_export              ../engine/source/interfaces/ists_q1np/q1np_contact_export.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_driver              ../engine/source/interfaces/ists_q1np/q1np_contact_driver.F90
!||    q1np_contact_algorithms          ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    resol                            ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    precision_mod                    ../common_source/modules/precision_mod.F90
!||====================================================================
      MODULE Q1NP_CONTACT_EXPORT_MOD
        USE PRECISION_MOD, ONLY : WP
        USE MY_ALLOC_MOD, ONLY : MY_ALLOC
        USE MY_DEALLOC_MOD, ONLY : MY_DEALLOC
        IMPLICIT NONE
        PRIVATE

        CHARACTER(LEN=*), PARAMETER :: Q1NP_CONTACT_CSV_FILE = 'q1np_contact_forces.csv'

        REAL(KIND=WP), ALLOCATABLE, SAVE :: Q1NP_CONTACT_FORCE_SUM(:,:)
        INTEGER, ALLOCATABLE, SAVE :: Q1NP_CONTACT_PAIR_COUNT(:)
        REAL(KIND=WP), ALLOCATABLE, SAVE :: Q1NP_CONTACT_MAX_PENETRATION(:)

        INTEGER, SAVE :: Q1NP_CONTACT_EXPORT_NUMEL = 0
        INTEGER, SAVE :: Q1NP_CONTACT_EXPORT_LAST_BEGIN_CYCLE = -1
        INTEGER, SAVE :: Q1NP_CONTACT_EXPORT_LAST_FLUSH_CYCLE = -1
        LOGICAL, SAVE :: Q1NP_CONTACT_EXPORT_FILE_INITIALIZED = .FALSE.
        LOGICAL, SAVE :: Q1NP_CONTACT_EXPORT_ENABLED = .FALSE.

        PUBLIC :: Q1NP_CONTACT_EXPORT_ENABLED
        PUBLIC :: Q1NP_CONTACT_EXPORT_BEGIN_CYCLE
        PUBLIC :: Q1NP_CONTACT_EXPORT_ACCUMULATE
        PUBLIC :: Q1NP_CONTACT_EXPORT_FLUSH

      CONTAINS

!=======================================================================
!   Ensure export buffers match the current number of Q1NP elements.
!=======================================================================
        SUBROUTINE Q1NP_CONTACT_EXPORT_RESIZE(NUMELQ1NP)
          INTEGER, INTENT(IN) :: NUMELQ1NP

          IF (NUMELQ1NP <= 0) THEN
            IF (ALLOCATED(Q1NP_CONTACT_FORCE_SUM)) CALL MY_DEALLOC(Q1NP_CONTACT_FORCE_SUM)
            IF (ALLOCATED(Q1NP_CONTACT_PAIR_COUNT)) CALL MY_DEALLOC(Q1NP_CONTACT_PAIR_COUNT)
            IF (ALLOCATED(Q1NP_CONTACT_MAX_PENETRATION)) CALL MY_DEALLOC(Q1NP_CONTACT_MAX_PENETRATION)
            Q1NP_CONTACT_EXPORT_NUMEL = 0
            RETURN
          END IF

          IF (ALLOCATED(Q1NP_CONTACT_FORCE_SUM)) THEN
            IF (SIZE(Q1NP_CONTACT_FORCE_SUM, DIM=2) == NUMELQ1NP) RETURN
            CALL MY_DEALLOC(Q1NP_CONTACT_FORCE_SUM)
          END IF
          IF (ALLOCATED(Q1NP_CONTACT_PAIR_COUNT)) CALL MY_DEALLOC(Q1NP_CONTACT_PAIR_COUNT)
          IF (ALLOCATED(Q1NP_CONTACT_MAX_PENETRATION)) CALL MY_DEALLOC(Q1NP_CONTACT_MAX_PENETRATION)

          CALL MY_ALLOC(Q1NP_CONTACT_FORCE_SUM, 3, NUMELQ1NP, "Q1NP_CONTACT_FORCE_SUM")
          CALL MY_ALLOC(Q1NP_CONTACT_PAIR_COUNT, NUMELQ1NP, "Q1NP_CONTACT_PAIR_COUNT")
          CALL MY_ALLOC(Q1NP_CONTACT_MAX_PENETRATION, NUMELQ1NP, "Q1NP_CONTACT_MAX_PENETRATION")

          Q1NP_CONTACT_EXPORT_NUMEL = NUMELQ1NP
        END SUBROUTINE Q1NP_CONTACT_EXPORT_RESIZE

!=======================================================================
!   Reset per-cycle accumulators once per NCYCLE.
!=======================================================================
        SUBROUTINE Q1NP_CONTACT_EXPORT_BEGIN_CYCLE(NCYCLE, NUMELQ1NP)
          INTEGER, INTENT(IN) :: NCYCLE, NUMELQ1NP

          IF (.NOT. Q1NP_CONTACT_EXPORT_ENABLED) RETURN

          CALL Q1NP_CONTACT_EXPORT_RESIZE(NUMELQ1NP)

          IF (NUMELQ1NP <= 0) THEN
            Q1NP_CONTACT_EXPORT_LAST_BEGIN_CYCLE = NCYCLE
            RETURN
          END IF

          IF (NCYCLE == Q1NP_CONTACT_EXPORT_LAST_BEGIN_CYCLE .AND. &
     &        Q1NP_CONTACT_EXPORT_NUMEL == NUMELQ1NP) RETURN

          Q1NP_CONTACT_FORCE_SUM(:,:) = 0.0_WP
          Q1NP_CONTACT_PAIR_COUNT(:) = 0
          Q1NP_CONTACT_MAX_PENETRATION(:) = 0.0_WP
          Q1NP_CONTACT_EXPORT_LAST_BEGIN_CYCLE = NCYCLE
        END SUBROUTINE Q1NP_CONTACT_EXPORT_BEGIN_CYCLE

!=======================================================================
!   Accumulate one signed element force contribution.
!=======================================================================
        SUBROUTINE Q1NP_CONTACT_EXPORT_ACCUMULATE(ELEM_IDX, FORCE_VEC, PENETRATION_ABS)
          INTEGER, INTENT(IN) :: ELEM_IDX
          REAL(KIND=WP), INTENT(IN) :: FORCE_VEC(3)
          REAL(KIND=WP), INTENT(IN) :: PENETRATION_ABS

          IF (.NOT. Q1NP_CONTACT_EXPORT_ENABLED) RETURN
          IF (.NOT. ALLOCATED(Q1NP_CONTACT_FORCE_SUM)) RETURN
          IF (ELEM_IDX < 1 .OR. ELEM_IDX > Q1NP_CONTACT_EXPORT_NUMEL) RETURN

          Q1NP_CONTACT_FORCE_SUM(1:3, ELEM_IDX) = &
     &      Q1NP_CONTACT_FORCE_SUM(1:3, ELEM_IDX) + FORCE_VEC(1:3)
          Q1NP_CONTACT_PAIR_COUNT(ELEM_IDX) = Q1NP_CONTACT_PAIR_COUNT(ELEM_IDX) + 1
          Q1NP_CONTACT_MAX_PENETRATION(ELEM_IDX) = &
     &      MAX(Q1NP_CONTACT_MAX_PENETRATION(ELEM_IDX), PENETRATION_ABS)
        END SUBROUTINE Q1NP_CONTACT_EXPORT_ACCUMULATE

!=======================================================================
!   Append one TH-synchronized CSV snapshot.
!=======================================================================
        SUBROUTINE Q1NP_CONTACT_EXPORT_FLUSH(TIME_CUR, KQ1NP_TAB)
          REAL(KIND=WP), INTENT(IN) :: TIME_CUR
          INTEGER, INTENT(IN) :: KQ1NP_TAB(:,:)

          INTEGER :: IEL, LUX, NUMELQ1NP, CYCLE_TO_WRITE
          REAL(KIND=WP) :: FORCE_NORM
          REAL(KIND=WP) :: FN_APPROX, FT_APPROX
          LOGICAL :: FILE_EXISTS

          IF (.NOT. Q1NP_CONTACT_EXPORT_ENABLED) RETURN

          NUMELQ1NP = SIZE(KQ1NP_TAB, DIM=2)
          IF (NUMELQ1NP <= 0) RETURN
          IF (Q1NP_CONTACT_EXPORT_LAST_BEGIN_CYCLE < 0) RETURN

!         Flush the last fully accumulated cycle, not necessarily the current NCYCLE.
          CYCLE_TO_WRITE = Q1NP_CONTACT_EXPORT_LAST_BEGIN_CYCLE
          IF (CYCLE_TO_WRITE == Q1NP_CONTACT_EXPORT_LAST_FLUSH_CYCLE) RETURN

          CALL Q1NP_CONTACT_EXPORT_RESIZE(NUMELQ1NP)
          IF (.NOT. ALLOCATED(Q1NP_CONTACT_FORCE_SUM)) RETURN

          IF (.NOT. Q1NP_CONTACT_EXPORT_FILE_INITIALIZED) THEN
            INQUIRE(FILE=Q1NP_CONTACT_CSV_FILE, EXIST=FILE_EXISTS)
            IF (FILE_EXISTS) THEN
              OPEN(NEWUNIT=LUX, FILE=Q1NP_CONTACT_CSV_FILE, &
     &             STATUS='OLD', ACTION='WRITE', POSITION='APPEND')
            ELSE
              OPEN(NEWUNIT=LUX, FILE=Q1NP_CONTACT_CSV_FILE, &
     &             STATUS='NEW', ACTION='WRITE')
              WRITE(LUX,'(A)') &
     &          'cycle,time,interface_id,entity_id,fx,fy,fz,force_norm,'// &
     &          'fn,ft,n_pairs,max_penetration'
            END IF
            Q1NP_CONTACT_EXPORT_FILE_INITIALIZED = .TRUE.
          ELSE
            OPEN(NEWUNIT=LUX, FILE=Q1NP_CONTACT_CSV_FILE, &
     &           STATUS='OLD', ACTION='WRITE', POSITION='APPEND')
          END IF

          DO IEL = 1, NUMELQ1NP
            IF (Q1NP_CONTACT_PAIR_COUNT(IEL) <= 0) CYCLE
            FORCE_NORM = SQRT( &
     &          Q1NP_CONTACT_FORCE_SUM(1, IEL) * Q1NP_CONTACT_FORCE_SUM(1, IEL) + &
     &          Q1NP_CONTACT_FORCE_SUM(2, IEL) * Q1NP_CONTACT_FORCE_SUM(2, IEL) + &
     &          Q1NP_CONTACT_FORCE_SUM(3, IEL) * Q1NP_CONTACT_FORCE_SUM(3, IEL))
            FN_APPROX = FORCE_NORM
            FT_APPROX = 0.0_WP

            WRITE(LUX, &
     &        '(I0,'','',ES23.15,'','',I0,'','',I0)', &
     &        ADVANCE='NO') &
     &        CYCLE_TO_WRITE, TIME_CUR, KQ1NP_TAB(15, IEL), KQ1NP_TAB(5, IEL)
            WRITE(LUX, &
     &        '('','',ES23.15,'','',ES23.15,'','',ES23.15,'','',ES23.15,'','',ES23.15,'','',ES23.15,'','',I0,'','',ES23.15)') &
     &        Q1NP_CONTACT_FORCE_SUM(1, IEL), Q1NP_CONTACT_FORCE_SUM(2, IEL), &
     &        Q1NP_CONTACT_FORCE_SUM(3, IEL), FORCE_NORM, FN_APPROX, FT_APPROX, &
     &        Q1NP_CONTACT_PAIR_COUNT(IEL), Q1NP_CONTACT_MAX_PENETRATION(IEL)
          END DO

          CLOSE(LUX)
          Q1NP_CONTACT_EXPORT_LAST_FLUSH_CYCLE = CYCLE_TO_WRITE
        END SUBROUTINE Q1NP_CONTACT_EXPORT_FLUSH

      END MODULE Q1NP_CONTACT_EXPORT_MOD
