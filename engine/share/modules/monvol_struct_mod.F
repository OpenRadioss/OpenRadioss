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
Chd|  MONVOL_STRUCT_MOD             share/modules/monvol_struct_mod.F
Chd|-- called by -----------
Chd|        MONVOL0                       source/airbag/monvol0.F       
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESOL_HEAD                    source/engine/resol_head.F    
Chd|        VOLUM0                        source/airbag/volum0.F        
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE MONVOL_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "my_real.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
      TYPE MONVOL_STRUCT_
!     Automatic surface hole filling
      INTEGER :: NB_FILL_TRI
      INTEGER, DIMENSION(:), ALLOCATABLE :: FILL_TRI
      END TYPE MONVOL_STRUCT_
      
      CONTAINS
Chd|====================================================================
Chd|  MONVOL_DEALLOCATE             share/modules/monvol_struct_mod.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MONVOL_DEALLOCATE(NVOLU, T_MONVOL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"    
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------   
      INTEGER, INTENT(IN) :: NVOLU
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(INOUT) :: T_MONVOL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II
      
      DO II = 1, NVOLU
         IF (ALLOCATED(T_MONVOL(II)%FILL_TRI)) DEALLOCATE(T_MONVOL(II)%FILL_TRI)
      ENDDO
      END SUBROUTINE MONVOL_DEALLOCATE
      END
