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
Chd|  AGLO24                        source/materials/mat/mat024/aglo24.F
Chd|-- called by -----------
Chd|        CONC24                        source/materials/mat/mat024/conc24.F
Chd|        M24ANIM                       source/materials/mat/mat024/m24anim.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE AGLO24(NEL,SIG,R11,R12,R13,
     .                  R21,R22,R23,R31,R32,R33)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real, DIMENSION(NEL), INTENT(IN) :: R11,R12,R13,R21,R22,R23,R31,R32,R33
      my_real SIG(NEL,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real, DIMENSION(NEL) :: A11,A12,A13,A21,A22,A23,A31,A32,A33
C=======================================================================
      DO I=1,NEL
        A11(I)=SIG(I,1)*R11(I)+SIG(I,4)*R12(I)+SIG(I,6)*R13(I)
        A12(I)=SIG(I,1)*R21(I)+SIG(I,4)*R22(I)+SIG(I,6)*R23(I)
        A13(I)=SIG(I,1)*R31(I)+SIG(I,4)*R32(I)+SIG(I,6)*R33(I)
        A21(I)=SIG(I,4)*R11(I)+SIG(I,2)*R12(I)+SIG(I,5)*R13(I)
        A22(I)=SIG(I,4)*R21(I)+SIG(I,2)*R22(I)+SIG(I,5)*R23(I)
        A23(I)=SIG(I,4)*R31(I)+SIG(I,2)*R32(I)+SIG(I,5)*R33(I)
        A31(I)=SIG(I,6)*R11(I)+SIG(I,5)*R12(I)+SIG(I,3)*R13(I)
        A32(I)=SIG(I,6)*R21(I)+SIG(I,5)*R22(I)+SIG(I,3)*R23(I)
        A33(I)=SIG(I,6)*R31(I)+SIG(I,5)*R32(I)+SIG(I,3)*R33(I)
      ENDDO
c
      DO I=1,NEL
        SIG(I,1)=R11(I)*A11(I)+R12(I)*A21(I)+R13(I)*A31(I)
        SIG(I,2)=R21(I)*A12(I)+R22(I)*A22(I)+R23(I)*A32(I)
        SIG(I,3)=R31(I)*A13(I)+R32(I)*A23(I)+R33(I)*A33(I)
        SIG(I,4)=R11(I)*A12(I)+R12(I)*A22(I)+R13(I)*A32(I)
        SIG(I,5)=R21(I)*A13(I)+R22(I)*A23(I)+R23(I)*A33(I)
        SIG(I,6)=R11(I)*A13(I)+R12(I)*A23(I)+R13(I)*A33(I)
      ENDDO
c-----------
      RETURN
      END
