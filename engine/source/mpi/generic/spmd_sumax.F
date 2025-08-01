Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
C
!||====================================================================
!||    spmd_sumax            ../engine/source/mpi/generic/spmd_sumax.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_SUMAX(V,LEN1,LEN)
C gather tableau V de taille LEN de type my_real
C max des valeurs absolues sur LEN1
C somme des valeurs sur LEN2 = LEN - LEN1
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN,LEN1
      my_real
     .        V(LEN)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGTYP,I,K
      DATA MSGOFF/107/
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
      my_real
     .        VTMP(LEN)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF (ISPMD/=0) THEN
        MSGTYP=MSGOFF
        CALL MPI_SEND(V,LEN,REAL,IT_SPMD(1),MSGTYP,
     .                SPMD_COMM_WORLD,IERROR)
      ELSE
        DO K=2,NSPMD
          MSGTYP=MSGOFF
          CALL MPI_RECV(VTMP,LEN,REAL,IT_SPMD(K),MSGTYP,
     .                  SPMD_COMM_WORLD,STATUS,IERROR)
          DO I=1,LEN1
            IF(ABS(V(I))<ABS(VTMP(I))) THEN
              V(I) = VTMP(I)
            ENDIF
          ENDDO
C
          DO I=LEN1+1,LEN
            V(I) = V(I)+VTMP(I)
          ENDDO
        ENDDO
      ENDIF
C
#endif
      RETURN
      END
