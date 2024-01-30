Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  SPMD_FR_POFF                  source/mpi/kinematic_conditions/spmd_fr_poff.F
Chd|-- called by -----------
Chd|        DAMPING_VREF                  source/assembly/damping_vref.F
Chd|        MV_MATV                       source/airbag/monv_imp0.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_FR_POFF(FR_WALL,FS,LEN)
C cumul force sur frontiere en parith/Off
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
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN, FR_WALL(*)
      my_real
     .        FS(LEN)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGOFF2,MSGTYP,INFO,P,PMAIN, LOC_PROC, J
      DATA MSGOFF/110/
      DATA MSGOFF2/111/
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
      my_real
     .        FTMP(LEN)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      PMAIN = FR_WALL(NSPMD+2)
      LOC_PROC = ISPMD+1
      IF(LOC_PROC==PMAIN) THEN
        DO P = 1, NSPMD
          IF(LOC_PROC/=P.AND.FR_WALL(P)/=0)THEN
            MSGTYP = MSGOFF
            CALL MPI_RECV(FTMP  ,LEN           ,REAL  ,IT_SPMD(P),
     .                    MSGTYP,MPI_COMM_WORLD,STATUS,IERROR    )
C
            DO J = 1, LEN
              FS(J) = FS(J) + FTMP(J)
            END DO
          ENDIF
        ENDDO
C
        DO P = 1, NSPMD
          IF(LOC_PROC/=P.AND.FR_WALL(P)/=0)THEN
            MSGTYP = MSGOFF2
            CALL MPI_SEND(FS    ,LEN           ,REAL  ,IT_SPMD(P),
     .                    MSGTYP,MPI_COMM_WORLD,IERROR)
          ENDIF
        ENDDO
C
      ELSE
        MSGTYP = MSGOFF
        CALL MPI_SEND(FS    ,LEN           ,REAL  ,IT_SPMD(PMAIN),
     .                MSGTYP,MPI_COMM_WORLD,IERROR)
        MSGTYP = MSGOFF2
        CALL MPI_RECV(FS    ,LEN           ,REAL  ,IT_SPMD(PMAIN),
     .                MSGTYP,MPI_COMM_WORLD,STATUS,IERROR    )
      ENDIF
C
#endif
      RETURN
      END
