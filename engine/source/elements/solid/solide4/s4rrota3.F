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
Chd|  S4RROTA3                      source/elements/solid/solide4/s4rrota3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S4RROTA3(
     1   OFF,     R11,     R12,     R13,
     2   R21,     R22,     R23,     R31,
     3   R32,     R33,     X1,      X2,
     4   X3,      X4,      Y1,      Y2,
     5   Y3,      Y4,      Z1,      Z2,
     6   Z3,      Z4,      NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      my_real
     .   R11(*), R12(*), R13(*),
     .   R21(*), R22(*), R23(*),
     .   R31(*), R32(*), R33(*),OFF(*),
     .   X1(*), X2(*), X3(*), X4(*), 
     .   Y1(*), Y2(*), Y3(*), Y4(*),   
     .   Z1(*), Z2(*), Z3(*), Z4(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   X,Y,Z
C-----------------------------------------------
C   X=RX' <=> X'=t(R)X chgt de base.
C-  to do X'=RtX, just input Rt instead of R
      DO I=1,NEL
       IF (OFF(I) <= ONE) CYCLE
       X=R11(I)*X1(I)+R21(I)*Y1(I)+R31(I)*Z1(I)
       Y=R12(I)*X1(I)+R22(I)*Y1(I)+R32(I)*Z1(I)
       Z=R13(I)*X1(I)+R23(I)*Y1(I)+R33(I)*Z1(I)
       X1(I)=X
       Y1(I)=Y
       Z1(I)=Z
       X=R11(I)*X2(I)+R21(I)*Y2(I)+R31(I)*Z2(I)
       Y=R12(I)*X2(I)+R22(I)*Y2(I)+R32(I)*Z2(I)
       Z=R13(I)*X2(I)+R23(I)*Y2(I)+R33(I)*Z2(I)
       X2(I)=X
       Y2(I)=Y
       Z2(I)=Z
       X=R11(I)*X3(I)+R21(I)*Y3(I)+R31(I)*Z3(I)
       Y=R12(I)*X3(I)+R22(I)*Y3(I)+R32(I)*Z3(I)
       Z=R13(I)*X3(I)+R23(I)*Y3(I)+R33(I)*Z3(I)
       X3(I)=X
       Y3(I)=Y
       Z3(I)=Z
       X=R11(I)*X4(I)+R21(I)*Y4(I)+R31(I)*Z4(I)
       Y=R12(I)*X4(I)+R22(I)*Y4(I)+R32(I)*Z4(I)
       Z=R13(I)*X4(I)+R23(I)*Y4(I)+R33(I)*Z4(I)
       X4(I)=X
       Y4(I)=Y
       Z4(I)=Z
      END DO !I=1,NEL
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      RETURN
      END
