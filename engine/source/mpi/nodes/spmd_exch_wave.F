Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!||====================================================================
!||    spmd_exch_wave_mod   ../engine/source/mpi/nodes/spmd_exch_wave.F
!||--- called by ------------------------------------------------------
!||    resol                ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    precision_mod        ../common_source/modules/precision_mod.F90
!||====================================================================
      MODULE SPMD_EXCH_WAVE_MOD
        ! types containing buffer and request for wave exchange
        USE PRECISION_MOD, ONLY: WP
        TYPE FR_WAVE_
          REAL(KIND=WP), DIMENSION(:), allocatable :: RBUF
          REAL(KIND=WP), DIMENSION(:), allocatable :: SBUF
          INTEGER, DIMENSION(:), allocatable :: IAD_SEND
          INTEGER, DIMENSION(:), allocatable :: IAD_RECV
          INTEGER, DIMENSION(:), allocatable :: REQ_R
          INTEGER, DIMENSION(:), allocatable :: REQ_S
          integer :: SIZE
          integer :: LENR
        END TYPE FR_WAVE_

      contains
!||====================================================================
!||    spmd_exch_wave_init   ../engine/source/mpi/nodes/spmd_exch_wave.F
!||--- called by ------------------------------------------------------
!||    resol                 ../engine/source/engine/resol.F
!||====================================================================
        SUBROUTINE SPMD_EXCH_WAVE_INIT(FR_WAVE_BUF, SIZ, LENR, NSPMD)
          implicit none
          TYPE(FR_WAVE_), INTENT(INOUT) :: FR_WAVE_BUF
          INTEGER, INTENT(IN) :: SIZ, LENR, NSPMD

          !deallocate if size changed
          IF(ALLOCATED(FR_WAVE_BUF%RBUF) .AND. size(FR_WAVE_BUF%RBUF)<SIZ*LENR) THEN
            DEALLOCATE(FR_WAVE_BUF%RBUF)
            DEALLOCATE(FR_WAVE_BUF%SBUF)
          END IF

          IF(.NOT.ALLOCATED(FR_WAVE_BUF%RBUF)) THEN
            ALLOCATE(FR_WAVE_BUF%RBUF(SIZ*LENR))
            ALLOCATE(FR_WAVE_BUF%SBUF(SIZ*LENR))
          END IF

          IF(.NOT.ALLOCATED(FR_WAVE_BUF%IAD_SEND)) THEN
            ALLOCATE(FR_WAVE_BUF%IAD_SEND(NSPMD+1))
            ALLOCATE(FR_WAVE_BUF%IAD_RECV(NSPMD+1))
            ALLOCATE(FR_WAVE_BUF%REQ_R(NSPMD))
            ALLOCATE(FR_WAVE_BUF%REQ_S(NSPMD))
          END IF
          FR_WAVE_BUF%SIZE = SIZ
          FR_WAVE_BUF%LENR = LENR
          RETURN
        END SUBROUTINE SPMD_EXCH_WAVE_INIT
!||====================================================================
!||    spmd_exch_wave   ../engine/source/mpi/nodes/spmd_exch_wave.F
!||--- called by ------------------------------------------------------
!||    resol            ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    spmd_wait        ../engine/source/mpi/spmd_wait.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod     ../common_source/modules/constant_mod.F
!||    precision_mod    ../common_source/modules/precision_mod.F90
!||    spmd_mod         ../engine/source/mpi/spmd_mod.F90
!||====================================================================
        SUBROUTINE SPMD_EXCH_WAVE(FR_WAVE,IAD_ELEM,FR_ELEM,FR_WAVE_BUF,MODE,ISPMD,NSPMD)
          USE SPMD_MOD
          USE PRECISION_MOD, ONLY: WP
          USE CONSTANT_MOD, ONLY: ZERO
          IMPLICIT NONE
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
          TYPE(FR_WAVE_), INTENT(INOUT) :: FR_WAVE_BUF
          INTEGER, INTENT(INOUT) :: IAD_ELEM(2,*), FR_ELEM(*)
          real(kind=WP), INTENT(INOUT) :: FR_WAVE(*)
          INTEGER, INTENT(IN) :: MODE
          INTEGER, INTENT(IN) :: ISPMD, NSPMD
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
          INTEGER MSGTYP,I,NOD,LOC_PROC, MSGOFF,
     .            SIZ,J,L,NB_NOD,P
          DATA MSGOFF/128/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
          LOC_PROC = ISPMD + 1
          L = 1
          FR_WAVE_BUF%IAD_RECV(1) = 1
          IF(MODE == 1) THEN
            !initialize requests to SPMD_REQUEST_NULL
            FR_WAVE_BUF%REQ_R = SPMD_REQUEST_NULL
            FR_WAVE_BUF%REQ_S = SPMD_REQUEST_NULL
            DO I=1,NSPMD
              SIZ = FR_WAVE_BUF%SIZE*(IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
              IF(SIZ/=0)THEN
                MSGTYP = MSGOFF
                P = I - 1
                CALL SPMD_IRECV(
     S            FR_WAVE_BUF%RBUF(L),SIZ,P,MSGTYP,
     G            FR_WAVE_BUF%REQ_R(I))
                L = L + SIZ
              ENDIF
              FR_WAVE_BUF%IAD_RECV(I+1) = L
            END DO
            L = 1
            FR_WAVE_BUF%IAD_SEND(1) = 1
            DO I=1,NSPMD
              DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                NOD = FR_ELEM(J)
                IF(FR_WAVE(NOD)<0)THEN
                  FR_WAVE_BUF%SBUF(L  ) =  FR_WAVE(NOD)
                ELSE
                  FR_WAVE_BUF%SBUF(L) = ZERO
                END IF
                L = L + FR_WAVE_BUF%SIZE
              END DO
              FR_WAVE_BUF%IAD_SEND(I+1) = L
            ENDDO
C
C   exchange
C
            DO I=1,NSPMD
C--------------------------------------------------------------------
              IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)THEN
                MSGTYP = MSGOFF
                SIZ = FR_WAVE_BUF%IAD_SEND(I+1)-FR_WAVE_BUF%IAD_SEND(I)
                L = FR_WAVE_BUF%IAD_SEND(I)
                P = I - 1
                CALL SPMD_ISEND(
     S            FR_WAVE_BUF%SBUF(L),SIZ,P,MSGTYP,
     G            FR_WAVE_BUF%REQ_S(I))
              ENDIF
C--------------------------------------------------------------------
            ENDDO
C
          ELSEIF(MODE == 2) THEN
            DO I = 1, NSPMD
              NB_NOD = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
              IF(NB_NOD>0)THEN
                CALL SPMD_WAIT(FR_WAVE_BUF%REQ_R(I))
                L = FR_WAVE_BUF%IAD_RECV(I)
                DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
                  NOD = FR_ELEM(J)
                  IF(FR_WAVE_BUF%RBUF(L)<ZERO) THEN
                    FR_WAVE(NOD) = FR_WAVE_BUF%RBUF(L)
                  END IF
                  L = L + FR_WAVE_BUF%SIZE
                END DO
              ENDIF
            END DO
C
            DO I = 1, NSPMD
              IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)THEN
                CALL SPMD_WAIT(FR_WAVE_BUF%REQ_S(I))
              ENDIF
            ENDDO
          ENDIF
C
#endif
          RETURN
        END

      END MODULE SPMD_EXCH_WAVE_MOD
