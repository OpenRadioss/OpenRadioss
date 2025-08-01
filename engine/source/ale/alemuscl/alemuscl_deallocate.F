Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    alemuscl_deallocate   ../engine/source/ale/alemuscl/alemuscl_deallocate.F
!||--- called by ------------------------------------------------------
!||    resol                 ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    alemuscl_mod          ../common_source/modules/ale/alemuscl_mod.F
!||====================================================================
      SUBROUTINE ALEMUSCL_DEALLOCATE()
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE ALEMUSCL_MOD , only:ALEMUSCL_Buffer
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
      IF (ALLOCATED(ALEMUSCL_Buffer%VOLUME_FRACTION)) DEALLOCATE(ALEMUSCL_Buffer%VOLUME_FRACTION)
      IF (ALLOCATED(ALEMUSCL_Buffer%NODE_MAX_VALUE)) DEALLOCATE(ALEMUSCL_Buffer%NODE_MAX_VALUE)
      IF (ALLOCATED(ALEMUSCL_Buffer%NODE_MIN_VALUE)) DEALLOCATE(ALEMUSCL_Buffer%NODE_MIN_VALUE)
      IF (ALLOCATED(ALEMUSCL_Buffer%GRAD)) DEALLOCATE(ALEMUSCL_Buffer%GRAD)
      IF (ALLOCATED(ALEMUSCL_Buffer%ELCENTER)) DEALLOCATE(ALEMUSCL_Buffer%ELCENTER)
C-----------------------------------------------
      END SUBROUTINE ALEMUSCL_DEALLOCATE
