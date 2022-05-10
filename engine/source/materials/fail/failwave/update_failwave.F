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
Chd|  UPDATE_FAILWAVE               source/materials/fail/failwave/update_failwave.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        SPMD_EXCH_FAILWAVE            source/mpi/output/spmd_exch_failwave.F
Chd|        DEBUG_MOD                     share/modules/debug_mod.F     
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|====================================================================
      SUBROUTINE UPDATE_FAILWAVE(FAILWAVE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAILWAVE_MOD
      USE DEBUG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "com04_c.inc"
#include "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (FAILWAVE_STR_) ,TARGET :: FAILWAVE 
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,NNOD,NDDL,LEVEL
c=======================================================================
      NNOD = FAILWAVE%NNOD 
      NDDL = FAILWAVE%NDDL 
      FAILWAVE%MAXLEV(1:NNOD) = FAILWAVE%MAXLEV_STACK(1:NNOD)
      FAILWAVE%MAXLEV_STACK(1:NNOD) = 0
c
      DO I=1,NNOD
         LEVEL = MIN(FAILWAVE%MAXLEV(I), FAILWAVE%SIZE)
         DO J=1,LEVEL
            DO K=1,NDDL
              FAILWAVE%FWAVE_NOD(K,I,J) = FAILWAVE%FWAVE_NOD_STACK(K,I,J)
            ENDDO
         ENDDO
      ENDDO
      IF (NSPMD > 1 )THEN
        CALL SPMD_EXCH_FAILWAVE(FAILWAVE)
      ENDIF
c----------
      END
