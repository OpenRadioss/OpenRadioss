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
Chd|  H3D_WRITE_SH_TENSOR_ARRAY     source/output/h3d/h3d_results/h3d_write_sh_tensor_array.F
Chd|-- called by -----------
Chd|        H3D_SHELL_TENSOR              source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE H3D_WRITE_SH_TENSOR_ARRAY(
     .           IOK_PART  ,ISELECT   ,NEL       ,OFFSET    ,NFT       ,
     .           IS_WRITTEN,TENSOR    ,VALUE     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,OFFSET,NFT,ISELECT
      INTEGER ,DIMENSION(NEL)   ,INTENT(IN)    :: IOK_PART
      my_real ,DIMENSION(NEL,3) ,INTENT(INOUT) :: VALUE
      my_real ,DIMENSION(3,NUMELC+NUMELTG) ,INTENT(INOUT) :: TENSOR
      INTEGER ,DIMENSION(NUMELC+NUMELTG)   ,INTENT(INOUT) :: IS_WRITTEN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
c=======================================================================
      DO I = 1,NEL
        TENSOR(1,OFFSET+NFT+I) = VALUE(I,1)
        TENSOR(2,OFFSET+NFT+I) = VALUE(I,2)
        TENSOR(3,OFFSET+NFT+I) = VALUE(I,3)
        IF (IOK_PART(I) == 1 .AND. ISELECT == 1) IS_WRITTEN(OFFSET+NFT+I) = 1
      END DO
      VALUE(:,:) = ZERO
c----------- 
      RETURN
      END
