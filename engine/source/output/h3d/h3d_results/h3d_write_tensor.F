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
!||    h3d_write_tensor     ../engine/source/output/h3d/h3d_results/h3d_write_tensor.F
!||--- called by ------------------------------------------------------
!||    h3d_nodal_tensor     ../engine/source/output/h3d/h3d_results/h3d_nodal_tensor.F
!||    h3d_oned_tensor      ../engine/source/output/h3d/h3d_results/h3d_oned_tensor.F
!||    h3d_quad_tensor      ../engine/source/output/h3d/h3d_results/h3d_quad_tensor.F
!||    h3d_solid_tensor_1   ../engine/source/output/h3d/h3d_results/h3d_solid_tensor_1.F
!||    h3d_sph_tensor       ../engine/source/output/h3d/h3d_results/h3d_sph_tensor.F
!||====================================================================
        SUBROUTINE H3D_WRITE_TENSOR(
     .                          IOK_PART  ,IS_WRITTEN ,TENSOR ,NEL ,OFFSET ,NFT,
     .                          VALUE    ,IS_WRITTEN_TENSOR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        my_real, DIMENSION(6,*), INTENT(INOUT) :: VALUE
        my_real, DIMENSION(6,*), INTENT(OUT) :: TENSOR
        INTEGER, INTENT(IN) :: NEL,OFFSET,NFT
        INTEGER, DIMENSION(*), INTENT(IN) :: IOK_PART
        INTEGER, DIMENSION(*), INTENT(OUT) :: IS_WRITTEN
        INTEGER, DIMENSION(*), INTENT(IN) :: IS_WRITTEN_TENSOR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I
C-----------------------------------------------
        DO I=1,NEL
          TENSOR(1,OFFSET+NFT+I) = VALUE(1,I)
          TENSOR(2,OFFSET+NFT+I) = VALUE(2,I)
          TENSOR(3,OFFSET+NFT+I) = VALUE(3,I)
          TENSOR(4,OFFSET+NFT+I) = VALUE(4,I)
          TENSOR(5,OFFSET+NFT+I) = VALUE(5,I)
          TENSOR(6,OFFSET+NFT+I) = VALUE(6,I)
          IF(IOK_PART(I) == 1  .AND. IS_WRITTEN_TENSOR(I) == 1) IS_WRITTEN(OFFSET+NFT+I)= 1
          VALUE(1:6,I) = ZERO
        ENDDO

        RETURN
        END SUBROUTINE H3D_WRITE_TENSOR
