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
Chd|====================================================================
Chd|  INIT_INTERF_SORTING_STRATEGY  source/interfaces/init_interf_sorting_strategy.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        METRIC_MOD                    ../common_source/modules/metric_mod.F
Chd|====================================================================
      SUBROUTINE INIT_INTERF_SORTING_STRATEGY(INTBUF_TAB,NINTER)
C Bucket or voxel, depending on /PERF/ in 1.rad
C By default, both will be tried, the best will be kept
C for the rest of simulation
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD  
      USE METRIC_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NINTER
      TYPE(INTBUF_STRUCT_),DIMENSION(NINTER), INTENT(INOUT) :: INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I
      DO I=1,NINTER
        INTBUF_TAB(I)%METRIC%TIC = 0 
        INTBUF_TAB(I)%METRIC%TOC = 0 
        INTBUF_TAB(I)%METRIC%TOLD = 0 
        INTBUF_TAB(I)%METRIC%CYCLE0 = -1 
        IF(GLOBAL_INTERFACE_SORTING_ALGO == 0) THEN
          INTBUF_TAB(I)%METRIC%ALGO = TRY_ALGO_VOXEL
        ELSEIF( GLOBAL_INTERFACE_SORTING_ALGO == 2) THEN
          INTBUF_TAB(I)%METRIC%ALGO = ALGO_BUCKET
        ELSE
          INTBUF_TAB(I)%METRIC%ALGO = ALGO_VOXEL
        ENDIF
      ENDDO
      END


