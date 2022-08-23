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
Chd|  ALEMUSCL_DEALLOCATE           source/ale/alemuscl/alemuscl_deallocate.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ALEMUSCL_MOD                  ../common_source/modules/alemuscl_mod.F
Chd|====================================================================
      SUBROUTINE ALEMUSCL_DEALLOCATE()
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE ALEMUSCL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
      IF (ALLOCATED(VOLUME_FRACTION)) DEALLOCATE(VOLUME_FRACTION)
      IF (ALLOCATED(NODE_MAX_VALUE)) DEALLOCATE(NODE_MAX_VALUE)
      IF (ALLOCATED(NODE_MIN_VALUE)) DEALLOCATE(NODE_MIN_VALUE)
      IF (ALLOCATED(GRAD)) DEALLOCATE(GRAD)
      IF (ALLOCATED(ELCENTER)) DEALLOCATE(ELCENTER)
C-----------------------------------------------
      END SUBROUTINE ALEMUSCL_DEALLOCATE
