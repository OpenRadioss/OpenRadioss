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
Chd|  S10RROTA3                     source/elements/solid/solide10/s10rrota3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10RROTA3(
     1   OFF,     R11,     R12,     R13,
     2   R21,     R22,     R23,     R31,
     3   R32,     R33,     XX,      YY,
     4   ZZ,      NEL)
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
      INTEGER, INTENT(IN) :: NEL
C     REAL
      my_real
     .   R11(*), R12(*), R13(*),
     .   R21(*), R22(*), R23(*),
     .   R31(*), R32(*), R33(*),OFF(*),
     .   XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N
C     REAL
      my_real
     .   X,Y,Z
C-----------------------------------------------
C   X=RX' <=> X'=t(R)X chgt de base.
C-  to do X'=RtX, just input Rt instead of R
      DO I=1,NEL
       DO N =1,10
       IF (OFF(I) <= ONE) CYCLE
       X=R11(I)*XX(I,N)+R21(I)*YY(I,N)+R31(I)*ZZ(I,N)
       Y=R12(I)*XX(I,N)+R22(I)*YY(I,N)+R32(I)*ZZ(I,N)
       Z=R13(I)*XX(I,N)+R23(I)*YY(I,N)+R33(I)*ZZ(I,N)
       XX(I,N)=X
       YY(I,N)=Y
       ZZ(I,N)=Z
       END DO !I=1,NEL
      END DO !I=1,NEL
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      RETURN
      END
