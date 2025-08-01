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
C Computes AINV(NEL,3,3) which is the inverse of MAT(NEL,3,3)
!||====================================================================
!||    kmatinv3    ../engine/source/materials/tools/kmatinv.F
!||--- called by ------------------------------------------------------
!||    calcmatb    ../engine/source/materials/mat/mat100/calcmatb.F
!||    sigeps100   ../engine/source/materials/mat/mat100/sigeps100.F90
!||    sigeps95    ../engine/source/materials/mat/mat095/sigeps95.F
!||====================================================================
       SUBROUTINE KMATINV3(MAT, AINV  ,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------

      INTEGER I,NEL,N
C-----------------------------------------------

      my_real  FAC,DETA, MAT(NEL,3,3),AINV(NEL,3,3)
C-----------------------------------------------
C
        DO I= 1,NEL
          DETA=MAT(I,1,1)*MAT(I,2,2)*MAT(I,3,3) -MAT(I,1,1)*MAT(I,2,3)*MAT(I,3,2) -
     .         MAT(I,3,3)*MAT(I,1,2)*MAT(I,2,1) +MAT(I,1,2)*MAT(I,2,3)*MAT(I,3,1) +
     .         MAT(I,2,1)*MAT(I,3,2)*MAT(I,1,3) -MAT(I,2,2)*MAT(I,3,1)*MAT(I,1,3)
          IF(DETA == ZERO)DETA = EM20
          FAC=ONE/ DETA
          !transpose(comatrice)/det
          AINV(I,1,1)=FAC*(MAT(I,2,2)*MAT(I,3,3)-MAT(I,2,3)*MAT(I,3,2))
          AINV(I,2,2)=FAC*(MAT(I,1,1)*MAT(I,3,3)-MAT(I,3,1)*MAT(I,1,3))
          AINV(I,3,3)=FAC*(MAT(I,1,1)*MAT(I,2,2)-MAT(I,1,2)*MAT(I,2,1))
          AINV(I,1,2)=FAC*(MAT(I,3,2)*MAT(I,1,3)-MAT(I,3,3)*MAT(I,1,2))
          AINV(I,2,3)=FAC*(MAT(I,2,1)*MAT(I,1,3)-MAT(I,1,1)*MAT(I,2,3))
          AINV(I,3,1)=FAC*(MAT(I,2,1)*MAT(I,3,2)-MAT(I,2,2)*MAT(I,3,1))
          AINV(I,2,1)=FAC*(MAT(I,2,3)*MAT(I,3,1)-MAT(I,3,3)*MAT(I,2,1))
          AINV(I,3,2)=FAC*(MAT(I,1,2)*MAT(I,3,1)-MAT(I,1,1)*MAT(I,3,2))
          AINV(I,1,3)=FAC*(MAT(I,1,2)*MAT(I,2,3)-MAT(I,2,2)*MAT(I,1,3))
        
        ENDDO  
C
      RETURN
      END      
