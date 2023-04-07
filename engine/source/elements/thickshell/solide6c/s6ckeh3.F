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
Chd|  S6CKEH3                       source/elements/thickshell/solide6c/s6cke3.F
Chd|-- called by -----------
Chd|        S6CKGEO3                      source/elements/thickshell/solide6c/s6cke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S6CKEH3(LFT,LLT,
     .   PXI, PYI ,PXJ, PYJ ,FXX, FYY ,FXY, KIJ)
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
      INTEGER LFT,LLT
C     REAL
      my_real
     .   PXI(*), PXJ(*),PYI(*), PYJ(*),  
     .   FXX(*),FYY(*),FXY(*),KIJ(3,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C     REAL
      my_real
     .   HIJ(MVSIZ) 
C-----------------------------------------------
      DO I=LFT,LLT
        HIJ(I)=FXX(I)*PXI(I)*PXJ(I)+FYY(I)*PYI(I)*PYJ(I)+
     .         FXY(I)*(PXI(I)*PYJ(I)+PYI(I)*PXJ(I))
      ENDDO
C
       DO J = 1,3
        DO I=LFT,LLT
         KIJ(J,J,I)=KIJ(J,J,I)+HIJ(I)
        ENDDO
       ENDDO
C
      RETURN
      END
