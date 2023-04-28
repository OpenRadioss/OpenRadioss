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
Chd|  ROT3DTO2D                     source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|-- called by -----------
Chd|        TENS3DTO2D                    source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ROT3DTO2D(NEL,SIG3D,SIG2D,
     .                     G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   SIG3D(6,*),SIG2D(3,*), G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   SX,SY,SZ
C-----------------------------------------------
      DO I=1,NEL
         SX = SIG3D(1,I)*G1X(I)+SIG3D(4,I)*G1Y(I)+SIG3D(6,I)*G1Z(I)
         SY = SIG3D(4,I)*G1X(I)+SIG3D(2,I)*G1Y(I)+SIG3D(5,I)*G1Z(I)
         SZ = SIG3D(6,I)*G1X(I)+SIG3D(5,I)*G1Y(I)+SIG3D(3,I)*G1Z(I)
         SIG2D(1,I) = SX*G1X(I)+SY*G1Y(I)+SZ*G1Z(I)
         SIG2D(3,I) = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
         SX = SIG3D(1,I)*G2X(I)+SIG3D(4,I)*G2Y(I)+SIG3D(6,I)*G2Z(I)
         SY = SIG3D(4,I)*G2X(I)+SIG3D(2,I)*G2Y(I)+SIG3D(5,I)*G2Z(I)
         SZ = SIG3D(6,I)*G2X(I)+SIG3D(5,I)*G2Y(I)+SIG3D(3,I)*G2Z(I)
         SIG2D(2,I) = SX*G2X(I)+SY*G2Y(I)+SZ*G2Z(I)
      ENDDO
C-----------
      RETURN
      END
