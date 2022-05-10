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
Chd|  SPMD_GLOB_IMAX9               source/mpi/generic/spmd_glob_imax9.F
Chd|-- called by -----------
Chd|        FIND_DT_FOR_TARGETED_ADDED_MASSsource/time_step/find_dt_for_targeted_added_mass.F
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        SMS_INI_KIN_1                 source/ams/sms_init.F         
Chd|        SMS_INI_KIN_2                 source/ams/sms_init.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_GLOB_IMAX9(V,LEN)
C gather tableau V de taille LEN de type integer
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
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN, V(LEN)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGTYP,INFO,I,K,ATID,ATAG,ALEN,IERROR,
     .        VTMP(LEN),STATUS(MPI_STATUS_SIZE)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF (LEN .GT. 0) THEN
        CALL MPI_REDUCE(V,VTMP,LEN,
     .                  MPI_INTEGER,MPI_MAX,IT_SPMD(1),
     .                  MPI_COMM_WORLD,IERROR)
      ENDIF
      IF (ISPMD.EQ.0) THEN
        DO I=1,LEN
          V(I) = VTMP(I)
        END DO
      ENDIF
C
#endif
      RETURN
      END
