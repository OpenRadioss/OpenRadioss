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
Chd|  SROTO12_SIG                   source/elements/solid/solidez/sroto12_sig.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SROTO12_SIG(JFT,JLT,SIG,NEL,
     .                       G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z,OFF)
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
     .   SIG(NEL,6), G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*),OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   SX,SY,SZ,SIGN(NEL,6)
C-----------------------------------------------
      DO I=JFT,JLT
       IF (OFF(I) <=ONE) CYCLE
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
         SIG(I,1) = SIGN(I,1) 
         SIG(I,2) = SIGN(I,2) 
         SIG(I,3) = SIGN(I,3)   
         SIG(I,4) = SIGN(I,4) 
         SIG(I,5) = SIGN(I,5) 
         SIG(I,6) = SIGN(I,6)   
      ENDDO
C-----------
      RETURN
      END
