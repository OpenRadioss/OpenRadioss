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
C
Chd|====================================================================
Chd|  SPMD_INIT_IDEL                source/mpi/interfaces/spmd_init_idel.F
Chd|-- called by -----------
Chd|        CHKLOAD                       source/interfaces/chkload.F   
Chd|        CHKSTFN3N                     source/interfaces/interf/chkstfn3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_INIT_IDEL(NINDEX,IRSIZE, IRECV,IAD_ELEM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINDEX, IRSIZE, IRECV(*)
      INTEGER, DIMENSION(2,NSPMD+1), INTENT(in) :: IAD_ELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF, MSGTYP, LOC_PROC,IERROR,I,
     .        REQ_S(NSPMD),STATUS(MPI_STATUS_SIZE)
      INTEGER :: SIZ
      DATA MSGOFF/106/
C-----------------------------------------------
      LOC_PROC = ISPMD+1
      DO I = 1, NSPMD
        SIZ = (IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
        IF(I.NE.LOC_PROC.AND.SIZ>0) THEN
          MSGTYP = MSGOFF
          CALL MPI_ISEND(
     C      NINDEX,1,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
        ENDIF
      ENDDO
      IRSIZE = 0
      DO I = 1, NSPMD
        SIZ = (IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
        IF(I.NE.LOC_PROC.AND.SIZ>0) THEN
          MSGTYP = MSGOFF
          CALL MPI_RECV(
     .     IRECV(I),1,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .     MPI_COMM_WORLD,STATUS,IERROR)
          IRSIZE = IRSIZE + IRECV(I)
        ELSE
          IRECV(I) = 0
        ENDIF
      ENDDO
C
C Test reception envoi NINDEX
C
      DO I = 1, NSPMD
        SIZ = (IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
        IF(I.NE.LOC_PROC.AND.SIZ>0) THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
#endif
      RETURN
      END
