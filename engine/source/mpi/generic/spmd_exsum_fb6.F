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
Chd|  SPMD_EXSUM_FB6                source/mpi/generic/spmd_exsum_fb6.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXSUM_FB6(DIM6,DIM_EXCH,FB6)
C     sommation + distribution de fb6 via MPI_AllReduce
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER DIM6,DIM_EXCH
      DOUBLE PRECISION
     .        FB6(DIM6,6,DIM_EXCH)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I,J,DIM
      INTEGER IERROR
      DOUBLE PRECISION
     .        BUF6(DIM6,6,DIM_EXCH)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      BUF6(1:DIM6,1:6,1:DIM_EXCH)=FB6(1:DIM6,1:6,1:DIM_EXCH)
      DIM=DIM6*6*DIM_EXCH
C
      CALL MPI_ALLREDUCE(BUF6,FB6,DIM,MPI_DOUBLE_PRECISION,   
     .                   MPI_SUM,MPI_COMM_WORLD,IERROR)
C
#endif
      RETURN
      END
