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
#ifdef MPI

Chd|====================================================================
Chd|  SPMD_KILL                     source/mpi/init/spmd_kill.F   
Chd|-- called by -----------
Chd|        ARRET                         source/system/arret.F         
Chd|-- calls ---------------
Chd|        MY_EXIT                       source/system/my_exit.c       
Chd|====================================================================
      SUBROUTINE SPMD_KILL(ICODE)
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
      INTEGER ICODE
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER INFO,LOC_PROC,I
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      CALL MPI_ABORT(MPI_COMM_WORLD,ICODE,INFO)
      CALL MY_EXIT(ICODE)

      RETURN
      END

#elif 1 
C
Chd|====================================================================
Chd|  SPMD_KILL                     source/mpi/init/spmd_kill.F   
Chd|-- called by -----------
Chd|        ARRET                         source/system/arret.F         
Chd|-- calls ---------------
Chd|        MY_EXIT                       source/system/my_exit.c       
Chd|====================================================================
      SUBROUTINE SPMD_KILL(ICODE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICODE
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      CALL MY_EXIT(ICODE)
      END


#endif


