Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    h3d_write_sh_tensor   ../engine/source/output/h3d/h3d_results/h3d_write_sh_tensor.F
!||--- called by ------------------------------------------------------
!||    h3d_shell_tensor      ../engine/source/output/h3d/h3d_results/h3d_shell_tensor.F
!||====================================================================
      SUBROUTINE H3D_WRITE_SH_TENSOR(
     .                  IOK_PART  ,ISELECT, IS_WRITTEN ,TENSOR ,I ,OFFSET ,NFT,
     .                  VALUE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   TENSOR(3,*),VALUE(5)
      INTEGER IOK_PART(*),IS_WRITTEN(*)
      INTEGER I,OFFSET,NFT,ISELECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      TENSOR(1,OFFSET+NFT+I) = VALUE(1)
      TENSOR(2,OFFSET+NFT+I) = VALUE(2)
      TENSOR(3,OFFSET+NFT+I) = VALUE(3)
      IF(IOK_PART(I) == 1 .AND. ISELECT == 1) IS_WRITTEN(OFFSET+NFT+I)= 1
      VALUE(1:5) = ZERO
 
      RETURN
      END

!||====================================================================
!||    h3d_write_sh_tensor   ../engine/source/output/h3d/h3d_results/h3d_write_sh_tensor.F
!||--- called by ------------------------------------------------------
!||    h3d_shell_tensor      ../engine/source/output/h3d/h3d_results/h3d_shell_tensor.F
!||====================================================================
      SUBROUTINE H3D_WRITE_SH_TENSOR_STACK(
     .                  IOK_PART  ,ISELECT, IS_WRITTEN_SHELL ,TENSOR ,I ,OFFSET ,NFT,
     .                  TENS_VALUE,SHELL_SCALAR_STACK,MAX_SHELL_STACKSIZE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAX_SHELL_STACKSIZE
      INTEGER SHELL_SCALAR_STACK
      REAL(KIND=4) :: TENSOR(3,MAX_SHELL_STACKSIZE)
      my_real TENS_VALUE(5)
      INTEGER IOK_PART(*),IS_WRITTEN_SHELL(*)
      INTEGER I,OFFSET,NFT,ISELECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      IF(IOK_PART(I) == 1 .AND. ISELECT == 1) THEN
            SHELL_SCALAR_STACK = SHELL_SCALAR_STACK + 1
            IS_WRITTEN_SHELL(SHELL_SCALAR_STACK)= OFFSET+NFT+I
            TENSOR(1,SHELL_SCALAR_STACK) = REAL(TENS_VALUE(1), KIND=4)
            TENSOR(2,SHELL_SCALAR_STACK) = REAL(TENS_VALUE(2), KIND=4)
            TENSOR(3,SHELL_SCALAR_STACK) = REAL(TENS_VALUE(3), KIND=4)
      ENDIF      
      TENS_VALUE(1:5) = ZERO
 
      RETURN
      END
