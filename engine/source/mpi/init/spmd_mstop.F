Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  SPMD_MSTOP                    source/mpi/init/spmd_mstop.F  
Chd|-- called by -----------
Chd|        EIGBUCKP                      com/eig/eigbuckp.F            
Chd|        EIGSOLP                       com/eig/eigsolp.F             
Chd|        IMP_SOL_INIT                  source/implicit/imp_sol_init.F
Chd|-- calls ---------------
Chd|        MY_EXIT                       source/system/my_exit.c       
Chd|====================================================================
      SUBROUTINE SPMD_MSTOP()
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER ierror
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      call MPI_BARRIER(MPI_COMM_WORLD,ierror)
      CALL MPI_FINALIZE(ierror)
      IF(ISPMD.NE.0)THEN
        CALL MY_EXIT(0)
      ENDIF
#endif
      RETURN
      END

