Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|====================================================================
Chd|  SPMD_CHECK_ALE_NEIGHBOUR      source/mpi/fluid/spmd_check_ale_neighbour.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE SPMD_CHECK_ALE_NEIGHBOUR(ACTIVE_ELEMENT,NB_RCV_NEIGH,NB_SEND_NEIGH,
     .                                      INDEX_RCV_NEIGH,INDEX_SEND_NEIGH,LENCOM,
     .                                      TMP_NB_RCV_NEIGH,TMP_NB_SEND_NEIGH,
     .                                      TMP_INDEX_RCV_NEIGH,TMP_INDEX_SEND_NEIGH)
!$COMMENT
!       SPMD_CHECK_ALE_NEIGHBOUR description
!           SPMD_CHECK_ALE_NEIGHBOUR exchange the deactivated ALE elements
!       SPMD_CHECK_ALE_NEIGHBOUR organization
!           The neighbourhood is re-built in this routine
!           * a deactivated element is removed from the list of neighbour
!           * neighbourhood array is saved before its modification
!           * saved neighbourhood array is written in the restart file
!
!$ENDCOMMENT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "com04_c.inc"
#include "task_c.inc"
#include "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, DIMENSION(SNESDVOIS), INTENT(IN) :: NB_SEND_NEIGH
        INTEGER, DIMENSION(SNERCVOIS), INTENT(IN) :: NB_RCV_NEIGH
        INTEGER, DIMENSION(SLESDVOIS), INTENT(IN) :: INDEX_SEND_NEIGH
        INTEGER, DIMENSION(SLERCVOIS), INTENT(IN) :: INDEX_RCV_NEIGH

        INTEGER, DIMENSION(SNESDVOIS), INTENT(INOUT) :: TMP_NB_SEND_NEIGH
        INTEGER, DIMENSION(SNERCVOIS), INTENT(INOUT) :: TMP_NB_RCV_NEIGH
        INTEGER, DIMENSION(SLESDVOIS), INTENT(INOUT) :: TMP_INDEX_SEND_NEIGH
        INTEGER, DIMENSION(SLERCVOIS), INTENT(INOUT) :: TMP_INDEX_RCV_NEIGH
        INTEGER, INTENT(IN) :: LENCOM
        LOGICAL, DIMENSION(NUMELS+NUMELQ+NUMELTG), INTENT(IN) :: ACTIVE_ELEMENT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
        INTEGER I, IDEB, IDEB2, MSGOFF, IERROR,MSGTYP,IAD_RECV(NSPMD),
     .        STATUS(MPI_STATUS_SIZE), REQ_S(NSPMD), REQ_R(NSPMD),
     .        LOC_PROC, N, NN, NBIRECV, IRINDEX(NSPMD), II, INDEX,
     .        LEN
        DATA MSGOFF/3003/
        LOGICAL, DIMENSION(:), ALLOCATABLE :: WA
C-----------------------------------------------
        ALLOCATE( WA(LENCOM) )

        ! -------------------
        ! receive the data 
        LOC_PROC = ISPMD+1
        IDEB = 0
        IDEB2 = 0
        NBIRECV = 0
        DO I = 1, NSPMD
            MSGTYP = MSGOFF 
            IAD_RECV(I) = IDEB2+1
            IF(NB_RCV_NEIGH(I)>0) THEN
                NBIRECV = NBIRECV + 1
                IRINDEX(NBIRECV) = I
                LEN = NB_RCV_NEIGH(I)
                CALL MPI_IRECV( WA(IDEB2+1),LEN,MPI_LOGICAL,IT_SPMD(I),MSGTYP,
     .                          MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
                IDEB2 = IDEB2 + LEN
            ENDIF
        ENDDO
        ! -------------------

        ! -------------------
        ! send the data & build the deactivated element neighbours for send
        IDEB = 0
        DO I = 1, NSPMD
            MSGTYP = MSGOFF 
            LEN = NB_SEND_NEIGH(I)
            TMP_NB_SEND_NEIGH(I) = 0
            IF(LEN>0) THEN
                DO N = 1, LEN
                    NN = INDEX_SEND_NEIGH(IDEB+N)
                    WA(IDEB2+N) = ACTIVE_ELEMENT(NN)
                    IF(WA(IDEB2+N)) THEN
                        TMP_NB_SEND_NEIGH(I) = TMP_NB_SEND_NEIGH(I) + 1
                        TMP_INDEX_SEND_NEIGH(IDEB+TMP_NB_SEND_NEIGH(I)) = INDEX_SEND_NEIGH(IDEB+N)
                    ENDIF
                ENDDO
                CALL MPI_ISEND( WA(IDEB2+1),LEN,MPI_LOGICAL,IT_SPMD(I),MSGTYP,
     .                          MPI_COMM_WORLD,REQ_S(I),IERROR)
                IDEB = IDEB + LEN
                IDEB2 = IDEB2 + LEN
            ENDIF
        ENDDO
        ! -------------------

        ! -------------------
        ! wait the R message & build the deactivated element neighbours for rcv
        DO II = 1, NBIRECV
            CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,ierror)
            I = IRINDEX(INDEX)
            TMP_NB_RCV_NEIGH(I) = 0
            IDEB = IAD_RECV(I)-1
            DO N = 1, NB_RCV_NEIGH(I)
                NN = INDEX_RCV_NEIGH(IDEB+N)
                IF(WA(IDEB+N)) THEN
                    TMP_NB_RCV_NEIGH(I) = TMP_NB_RCV_NEIGH(I) + 1
                    TMP_INDEX_RCV_NEIGH(IDEB+TMP_NB_RCV_NEIGH(I)) = INDEX_RCV_NEIGH(IDEB+N)
                ENDIF
            ENDDO
        ENDDO
        ! -------------------

        ! -------------------
        ! wait the S message
        DO I = 1, NSPMD
            IF(NB_SEND_NEIGH(I)>0) THEN
                CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
            ENDIF
        ENDDO
        ! -------------------
        DEALLOCATE( WA )
#endif
      RETURN
      END SUBROUTINE SPMD_CHECK_ALE_NEIGHBOUR
