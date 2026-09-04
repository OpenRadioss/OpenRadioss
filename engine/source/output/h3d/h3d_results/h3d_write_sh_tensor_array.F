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
!||    h3d_write_sh_tensor_array   ../engine/source/output/h3d/h3d_results/h3d_write_sh_tensor_array.F
!||--- called by ------------------------------------------------------
!||    h3d_shell_tensor            ../engine/source/output/h3d/h3d_results/h3d_shell_tensor.F
!||====================================================================
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

!||====================================================================
!||    h3d_write_sh_tensor   ../engine/source/output/h3d/h3d_results/h3d_write_sh_tensor.F
!||--- called by ------------------------------------------------------
!||    h3d_shell_tensor      ../engine/source/output/h3d/h3d_results/h3d_shell_tensor.F
!||====================================================================
      SUBROUTINE H3D_WRITE_SH_TENSOR_ARRAY_STACK(
     .                  IOK_PART,         ISELECT, NEL,        OFFSET,          NFT,
     .                  IS_WRITTEN_SHELL, TENSOR,  TENS_VALUE, SHELL_STACKSIZE, MAX_SHELL_STACKSIZE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAX_SHELL_STACKSIZE
      INTEGER SHELL_STACKSIZE
      REAL(KIND=4) :: TENSOR(3,MAX_SHELL_STACKSIZE)
      my_real TENS_VALUE(3,NEL)
      INTEGER IOK_PART(*),IS_WRITTEN_SHELL(*)
      INTEGER NEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,OFFSET,NFT,ISELECT
C-----------------------------------------------
      DO I=1,NEL
       IF(IOK_PART(I) == 1 .AND. ISELECT == 1) THEN
            SHELL_STACKSIZE = SHELL_STACKSIZE + 1
            IS_WRITTEN_SHELL(SHELL_STACKSIZE)= OFFSET+NFT+I
            TENSOR(1,SHELL_STACKSIZE) = REAL(TENS_VALUE(I,1), KIND=4)
            TENSOR(2,SHELL_STACKSIZE) = REAL(TENS_VALUE(I,2), KIND=4)
            TENSOR(3,SHELL_STACKSIZE) = REAL(TENS_VALUE(I,3), KIND=4)
       ENDIF
      END DO
      TENS_VALUE(1:NEL,1:3) = ZERO
 
      RETURN
      END

