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
!||    h3d_write_vector     ../engine/source/output/h3d/h3d_results/h3d_write_vector.F
!||--- called by ------------------------------------------------------
!||    h3d_nodal_vector     ../engine/source/output/h3d/h3d_results/h3d_nodal_vector.F
!||    h3d_oned_vector      ../engine/source/output/h3d/h3d_results/h3d_oned_vector.F
!||    h3d_shell_vector_1   ../engine/source/output/h3d/h3d_results/h3d_shell_vector_1.F
!||    h3d_solid_vector     ../engine/source/output/h3d/h3d_results/h3d_solid_vector.F
!||    h3d_velvecc22        ../engine/source/output/h3d/h3d_results/h3d_velvecc22.F
!||    h3d_velvecz22        ../engine/source/output/h3d/h3d_results/h3d_velvecz22.F
!||====================================================================
      SUBROUTINE H3D_WRITE_VECTOR(
     .                  IOK_PART  ,IS_WRITTEN ,VECTOR ,I ,OFFSET ,NFT,
     .                  VALUE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   VECTOR(3,*),VALUE(3)
      INTEGER IOK_PART(*),IS_WRITTEN(*)
      INTEGER I,OFFSET,NFT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
      VECTOR(1,OFFSET+NFT+I) = VALUE(1)
      VECTOR(2,OFFSET+NFT+I) = VALUE(2)
      VECTOR(3,OFFSET+NFT+I) = VALUE(3)
      IF(IOK_PART(I) == 1 ) IS_WRITTEN(OFFSET+NFT+I)= 1
 
      RETURN
      END
