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
C
!||====================================================================
!||    m24anim   ../engine/source/materials/mat/mat024/m24anim.F
!||--- called by ------------------------------------------------------
!||    m24law    ../engine/source/materials/mat/mat024/m24law.F
!||--- calls      -----------------------------------------------------
!||    aglo24    ../engine/source/materials/mat/mat024/aglo24.F
!||    udam24    ../engine/source/materials/mat/mat024/udam24.F
!||====================================================================
      SUBROUTINE M24ANIM(DAM,ANG,DAMGLO,NEL,
     .                   R11,R12,R13,R21,R22,
     .                   R23,R31,R32,R33)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   DAMGLO(NEL,6), ANG(NEL,6), DAM(NEL,3)
      my_real, DIMENSION(NEL), INTENT(IN) :: R11,R12,R13,R21,R22,R23,R31,R32,R33
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C=======================================================================
      DO I=1,NEL
         DAMGLO(I,1)=DAM(I,1)
         DAMGLO(I,2)=DAM(I,2)
         DAMGLO(I,3)=DAM(I,3)
         DAMGLO(I,4)=ZERO
         DAMGLO(I,5)=ZERO
         DAMGLO(I,6)=ZERO
      ENDDO
c
      CALL UDAM24(DAMGLO,ANG,NEL)
c
      CALL AGLO24(NEL,DAMGLO,R11,R12,R13,
     .            R21,R22,R23,R31,R32,R33)
c-----------
      RETURN
      END
