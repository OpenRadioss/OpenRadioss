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
Chd|  SORDEFT12                     source/elements/solid/solidez/sordeft12.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SORDEFT12(JFT,JLT,MXX, MXY, MXZ,  
     .                     MYX, MYY, MYZ,MZX, MZY, MZZ,
     .                     G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z,OFF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
C     REAL
      my_real
     .   MXX(*), MXY(*), MXZ(*),MYX(*), MYY(*), MYZ(*),  
     .   MZX(*), MZY(*), MZZ(*),G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*),OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   SX,SY,SZ,FXX,FXY,FXZ,FYX,FYY,FYZ,FZX,FZY,FZZ
C-----------------------------------------------
      DO I=JFT,JLT
       IF (OFF(I) <=ONE) CYCLE
         SX = MXX(I)*G1X(I)+MYX(I)*G1Y(I)+MZX(I)*G1Z(I)
         SY = MXY(I)*G1X(I)+MYY(I)*G1Y(I)+MZY(I)*G1Z(I)
         SZ = MXZ(I)*G1X(I)+MYZ(I)*G1Y(I)+MZZ(I)*G1Z(I)
         FXX = SX*G1X(I)+SY*G1Y(I)+SZ*G1Z(I)
         FXY = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         FXZ = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         SX = MXX(I)*G2X(I)+MYX(I)*G2Y(I)+MZX(I)*G2Z(I)
         SY = MXY(I)*G2X(I)+MYY(I)*G2Y(I)+MZY(I)*G2Z(I)
         SZ = MXZ(I)*G2X(I)+MYZ(I)*G2Y(I)+MZZ(I)*G2Z(I)
         FYX = SX*G1X(I)+SY*G1Y(I)+SZ*G1Z(I)
         FYY = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         FYZ = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         SX = MXX(I)*G3X(I)+MYX(I)*G3Y(I)+MZX(I)*G3Z(I)
         SY = MXY(I)*G3X(I)+MYY(I)*G3Y(I)+MZY(I)*G3Z(I)
         SZ = MXZ(I)*G3X(I)+MYZ(I)*G3Y(I)+MZZ(I)*G3Z(I)
         FZX = SX*G1X(I)+SY*G1Y(I)+SZ*G1Z(I)
         FZY = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         FZZ = SX*G3X(I)+SY*G3Y(I)+SZ*G3Z(I)
         MXX(I)=FXX
         MXY(I)=FXY
         MXZ(I)=FXZ
         MYX(I)=FYX
         MYY(I)=FYY
         MYZ(I)=FYZ
         MZX(I)=FZX
         MZY(I)=FZY
         MZZ(I)=FZZ
      ENDDO
C-----------
      RETURN
      END
