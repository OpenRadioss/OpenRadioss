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
!||====================================================================
!||    s8zkeh3    ../engine/source/elements/solid/solide8z/s8zkeh3.F
!||--- called by ------------------------------------------------------
!||    s4keh3     ../engine/source/elements/solid/solide4/s4keh3.F
!||    s8zkgeo3   ../engine/source/elements/solid/solide8z/s8zkgeo3.F
!||====================================================================
      SUBROUTINE S8ZKEH3(LFT,LLT,
     .   PXI, PYI ,PZI ,PXJ, PYJ, PZJ, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT
C     REAL
      my_real
     .   PXI(*), PXJ(*),PYI(*), PYJ(*), PZI(*), PZJ(*),  
     .   FXX(*),FYY(*),FZZ(*),FXY(*),FXZ(*),FYZ(*),HIJ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
C-----------------------------------------------
      DO I=LFT,LLT
        HIJ(I)=FXX(I)*PXI(I)*PXJ(I)+FYY(I)*PYI(I)*PYJ(I)+
     .         FZZ(I)*PZI(I)*PZJ(I)+
     .         FXY(I)*(PXI(I)*PYJ(I)+PYI(I)*PXJ(I))+
     .         FYZ(I)*(PYI(I)*PZJ(I)+PZI(I)*PYJ(I))+
     .         FXZ(I)*(PXI(I)*PZJ(I)+PZI(I)*PXJ(I))
      ENDDO
C
      RETURN
      END
