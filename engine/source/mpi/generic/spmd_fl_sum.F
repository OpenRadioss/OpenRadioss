Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
C
C
!||====================================================================
!||    spmd_fl_sum           ../engine/source/mpi/generic/spmd_fl_sum.F
!||--- called by ------------------------------------------------------
!||    bemsolvp              ../engine/source/fluid/bemsolvp.F
!||    daaacc                ../engine/source/fluid/daaacc.F
!||    daasolvp              ../engine/source/fluid/daasolvp.F
!||    incpflow              ../engine/source/fluid/incpflow.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_FL_SUM(LSUM, LEN, LSUMT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_MOD
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
      INTEGER LEN
      my_real
     .        LSUM(*), LSUMT(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF, MSGOFF2,ITAG, REQ(2), I, II, REQ1(NSPMD-1), J
      my_real
     .        LSUMC(LEN)
C
      DATA MSGOFF/186/
      DATA MSGOFF2/187/
C
      IF (ISPMD/=0) THEN
         ITAG=MSGOFF
         CALL SPMD_ISEND(LSUM(1:LEN), LEN, IT_SPMD(1), ITAG, REQ(1))
C
         ITAG=MSGOFF2
         CALL SPMD_IRECV(LSUMT(1:LEN), LEN, IT_SPMD(1), ITAG, REQ(2))
C
         CALL SPMD_WAITALL(REQ, 2)
      ELSE
         DO I=1,LEN
            LSUMT(I)=LSUM(I)
         ENDDO
C
         DO I=1,NSPMD-1
            II=I+1
            ITAG=MSGOFF
            CALL SPMD_IRECV(LSUMC(1:LEN), LEN, IT_SPMD(II), ITAG,
     .                      REQ1(1))
            CALL SPMD_WAIT(REQ1(1))
C
            DO J=1,LEN
               LSUMT(J)=LSUMT(J)+LSUMC(J)
            ENDDO
         ENDDO
C
         DO I=1,NSPMD-1
            II=I+1
            ITAG=MSGOFF2
            CALL SPMD_ISEND(LSUMT(1:LEN), LEN, IT_SPMD(II), ITAG,
     .                      REQ1(I))
         ENDDO
C
         CALL SPMD_WAITALL(REQ1, NSPMD-1)
      ENDIF
C
#endif
      RETURN
      END
