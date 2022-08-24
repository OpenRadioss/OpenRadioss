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
Chd|  HM_READ_ALE_CLOSE             source/ale/hm_read_ale_close.F
Chd|-- called by -----------
Chd|        HM_READ_PROP14F               source/properties/solid/hm_read_prop14.F
Chd|        HM_READ_PROP15                source/properties/solid/hm_read_prop15.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_ALE_CLOSE(UNITAB, LSUBMODEL, GEO)
C-----------------------------------------------
C   Description
C-----------------------------------------------
C This subroutine is reading option /ALE/CLOSE
C  in Starter input file
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(UNIT_TYPE_), INTENT(IN) :: UNITAB
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      my_real, DIMENSION(NPROPG), INTENT(INOUT) :: GEO
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: HTEST, HCLOSE
      LOGICAL :: IS_AVAILABLE1, IS_AVAILABLE2
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      ICLOSE = 0
      CALL HM_GET_FLOATV('htest', HTEST, IS_AVAILABLE1, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('hclose', HCLOSE, IS_AVAILABLE2, LSUBMODEL, UNITAB)
      IF (IS_AVAILABLE1 .OR. IS_AVAILABLE2) THEN
         ICLOSE = 1
         IF (HTEST == ZERO) HTEST = EM20
         IF (HCLOSE == ZERO) HCLOSE = HTEST * EM01
         HCLOSE = MIN(HTEST, HCLOSE)
         GEO(129) = HCLOSE
         GEO(130) = HTEST
         WRITE(IOUT, 2010) HTEST, HCLOSE
      ENDIF
      RETURN
C-----------------------------------------------
 2010 FORMAT(
     & 5X,'    ELEMENT CLOSURE  ',/,
     & 5X,'    ---------------  ',/,
     & 5X,'MAXIMUM LENGTH FOR ACTIVATION . . . . .=',1PG20.13/,
     & 5X,'MAXIMUM LENGTH FOR RESISTANCE . . . . .=',1PG20.13//)
C-----------------------------------------------
      END SUBROUTINE HM_READ_ALE_CLOSE
