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
C                                                                     12
Chd|====================================================================
Chd|  SROTO3                        source/elements/solid/solidez/sroto3.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SROTO3(JFT,JLT,SIG,SIGN,
     .                 G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,NEL
C     REAL
      my_real
     .   SIG(NEL,6),SIGN(NEL,6), G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   SX,SY,SZ
C-----------------------------------------------
      DO I=JFT,JLT
         SX = SIG(I,1)*G1X(I)+SIG(I,4)*G1Y(I)+SIG(I,6)*G1Z(I)
         SY = SIG(I,4)*G1X(I)+SIG(I,2)*G1Y(I)+SIG(I,5)*G1Z(I)
         SZ = SIG(I,6)*G1X(I)+SIG(I,5)*G1Y(I)+SIG(I,3)*G1Z(I)
         SIGN(I,1) = SX*G1X(I)+SY*G1Y(I)+SZ*G1Z(I)
         SIGN(I,4) = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         SIGN(I,6) = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         SX = SIG(I,1)*G2X(I)+SIG(I,4)*G2Y(I)+SIG(I,6)*G2Z(I)
         SY = SIG(I,4)*G2X(I)+SIG(I,2)*G2Y(I)+SIG(I,5)*G2Z(I)
         SZ = SIG(I,6)*G2X(I)+SIG(I,5)*G2Y(I)+SIG(I,3)*G2Z(I)
         SIGN(I,2) = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         SIGN(I,5) = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         SX = SIG(I,1)*G3X(I)+SIG(I,4)*G3Y(I)+SIG(I,6)*G3Z(I)
         SY = SIG(I,4)*G3X(I)+SIG(I,2)*G3Y(I)+SIG(I,5)*G3Z(I)
         SZ = SIG(I,6)*G3X(I)+SIG(I,5)*G3Y(I)+SIG(I,3)*G3Z(I)
         SIGN(I,3) = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
      ENDDO
C-----------
      RETURN
      END
