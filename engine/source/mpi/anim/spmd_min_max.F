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
Chd|====================================================================
Chd|  SPMD_MIN_MAX                  source/mpi/anim/spmd_min_max.F
Chd|-- called by -----------
Chd|        SCANOR                        source/output/anim/generate/scanor.F
Chd|        SPHPREP                       source/elements/sph/sphprep.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_MIN_MAX(X1,Y1,Z1,X2,Y2,Z2)
C spmd_min_max calcule les minimum des reels X1, Y1, Z1 et les
C maximum des reels X2, Y2, Z2 sur p0
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C   REAL*4 ou REAL*8
      my_real
     .  X1, Y1, Z1, X2, Y2, Z2
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER INFO,K,I,ATID,ATAG,ALEN
C  REAL*4 ou REAL*8
      my_real
     .  TMP(6)

      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
C  REAL*4 ou REAL*8
      my_real
     .  TMP1(3),TMPO(3)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      TMP1(1) = X1
      TMP1(2) = Y1
      TMP1(3) = Z1
      call MPI_ALLREDUCE(TMP1,TMPO,3,REAL,
     .                   MPI_MIN,
     .                   MPI_COMM_WORLD,ierror)
      X1 = TMPO(1)
      Y1 = TMPO(2)
      Z1 = TMPO(3)
C
      TMP1(1)=X2
      TMP1(2)=Y2
      TMP1(3)=Z2
      call MPI_ALLREDUCE(TMP1,TMPO,3,REAL,
     .                   MPI_MAX,
     .                   MPI_COMM_WORLD,ierror)
      X2 = TMPO(1)
      Y2 = TMPO(2)
      Z2 = TMPO(3)

C
#endif
      RETURN
      END
