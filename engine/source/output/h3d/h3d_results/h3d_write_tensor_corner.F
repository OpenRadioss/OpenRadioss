Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  H3D_WRITE_TENSOR_CORNER       source/output/h3d/h3d_results/h3d_write_tensor_corner.F
Chd|-- called by -----------
Chd|        H3D_SOLID_TENSOR_1            source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE H3D_WRITE_TENSOR_CORNER(
     .                          IOK_PART  ,IS_WRITTEN ,TENSOR_CORNER ,NEL ,OFFSET ,NFT,
     .                          VALUE     ,NNOD  ,IS_WRITTEN_TENSOR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        my_real, DIMENSION(6,*), INTENT(INOUT) :: VALUE
        my_real, DIMENSION(6,*), INTENT(OUT) :: TENSOR_CORNER
        INTEGER, INTENT(IN) :: NEL,OFFSET,NFT,NNOD
        INTEGER, DIMENSION(*), INTENT(IN) :: IOK_PART
        INTEGER, DIMENSION(*), INTENT(OUT) :: IS_WRITTEN
        INTEGER, DIMENSION(*), INTENT(IN) :: IS_WRITTEN_TENSOR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,J
C-----------------------------------------------
C
        DO I=1,NEL
          DO J=1,NNOD
            TENSOR_CORNER(1,NNOD*(OFFSET+NFT+I-1)+J) = VALUE(1,20*(I-1)+J)
            TENSOR_CORNER(2,NNOD*(OFFSET+NFT+I-1)+J) = VALUE(2,20*(I-1)+J)
            TENSOR_CORNER(3,NNOD*(OFFSET+NFT+I-1)+J) = VALUE(3,20*(I-1)+J)
            TENSOR_CORNER(4,NNOD*(OFFSET+NFT+I-1)+J) = VALUE(4,20*(I-1)+J)
            TENSOR_CORNER(5,NNOD*(OFFSET+NFT+I-1)+J) = VALUE(5,20*(I-1)+J)
            TENSOR_CORNER(6,NNOD*(OFFSET+NFT+I-1)+J) = VALUE(6,20*(I-1)+J)
            IF(IOK_PART(I) == 1  .AND. IS_WRITTEN_TENSOR(I) == 1) IS_WRITTEN(OFFSET+NFT+I)= 1
            VALUE(1:6,20*(I-1)+J) = ZERO
          ENDDO
        ENDDO

        RETURN
        END SUBROUTINE H3D_WRITE_TENSOR_CORNER
