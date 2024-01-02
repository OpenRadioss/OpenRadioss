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
Chd|  SPMD_IALLREDUCE_INT_COMM      source/mpi/generic/spmd_iallreduce_int_comm.F
Chd|-- called by -----------
Chd|        SPMD_GET_INACTI_GLOBAL        source/mpi/interfaces/spmd_get_inacti_global.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE SPMD_IALLREDUCE_INT_COMM(VALUE,RES,MY_SIZE,MY_OPERATION,MY_COMM,MY_REQUEST)
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
        CHARACTER(len=4), INTENT(in) :: MY_OPERATION
        INTEGER, INTENT(in) :: MY_SIZE,MY_COMM,MY_REQUEST
        INTEGER, DIMENSION(MY_SIZE), INTENT(in) :: VALUE
        INTEGER, DIMENSION(MY_SIZE), INTENT(out) :: RES

#ifdef MPI
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER :: error
        INTEGER :: MPI_OPERATION
!       ----------------------------------------
        
        IF(MY_OPERATION(1:3)=="SUM") THEN
                MPI_OPERATION=MPI_SUM
        ELSEIF(MY_OPERATION(1:4)=="PROD") THEN
                MPI_OPERATION=MPI_PROD
        ELSEIF(MY_OPERATION(1:3)=="MAX") THEN
                MPI_OPERATION=MPI_MAX
        ELSEIF(MY_OPERATION(1:3)=="MIN") THEN
                MPI_OPERATION=MPI_MIN               
        ENDIF

!       ------------------
!       mpi comm iallreduce
        CALL MPI_IALLREDUCE(VALUE,RES,MY_SIZE,MPI_INTEGER,MPI_OPERATION,MY_COMM,MY_REQUEST,error)
!       ------------------
#else
        RES = 0
#endif
        RETURN
        END SUBROUTINE SPMD_IALLREDUCE_INT_COMM
!       ----------------------------------------
!       ----------------------------------------

