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
Chd|  IDEGE                         source/elements/solid/solide/idege.F
Chd|-- called by -----------
Chd|        SLDEGE                        source/elements/solid/solide/sldege.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IDEGE(X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,A,AMAX,FAC,
     .                IT4,IT,INDX,N_INDX)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   X1(*), X2(*), X3(*), X4(*), Y1(*), Y2(*), Y3(*), Y4(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*),A(MVSIZ),AMAX(*),FAC(MVSIZ)
      INTEGER, INTENT(IN) ::N_INDX
      INTEGER, DIMENSION(MVSIZ), INTENT(IN) :: INDX
      INTEGER IT4(MVSIZ),IT(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IDE(MVSIZ),I,J
      my_real
     .   X12,Y12,Z12,X23,Y23,Z23,X34,Y34,Z34,X41,Y41,Z41
C-----------------------------------------------
        IDE(1:MVSIZ) = 0
#include      "vectorize.inc"
        DO J=1,N_INDX
                I = INDX(J)
                X12 = X2(I) - X1(I)
                Y12 = Y2(I) - Y1(I)
                Z12 = Z2(I) - Z1(I)
                IF (X12==ZERO.AND.Y12==ZERO.AND.Z12==ZERO) IDE(I) = 1
C       
                X23 = X2(I) - X3(I)
                Y23 = Y2(I) - Y3(I)
                Z23 = Z2(I) - Z3(I)
                IF (X23==ZERO.AND.Y23==ZERO.AND.Z23==ZERO) IDE(I) = 1
C       
                X34 = X4(I) - X3(I)
                Y34 = Y4(I) - Y3(I)
                Z34 = Z4(I) - Z3(I)
                IF (X34==ZERO.AND.Y34==ZERO.AND.Z34==ZERO) IDE(I) = 1
C       
                X41 = X4(I) - X1(I)
                Y41 = Y4(I) - Y1(I)
                Z41 = Z4(I) - Z1(I)
                IF (X41==ZERO.AND.Y41==ZERO.AND.Z41==ZERO) IDE(I) = 1
                IF (IDE(I)==0) IT4(I) =0
                IF (A(I) > AMAX(I) )THEN
                        IT(I)=IDE(I)
                        AMAX(I) = A(I)
                END IF
        ENDDO
C       
      RETURN
      END
