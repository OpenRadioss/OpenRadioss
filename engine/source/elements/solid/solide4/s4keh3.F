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
Chd|  S4KEH3                        source/elements/solid/solide4/s4ke3.F
Chd|-- called by -----------
Chd|        S10KGEO3                      source/elements/solid/solide10/s10ke3.F
Chd|        S20KGEO3                      source/elements/solid/solide20/s20ke3.F
Chd|        S4KGEO3                       source/elements/solid/solide4/s4ke3.F
Chd|-- calls ---------------
Chd|        S8ZKEH3                       source/elements/solid/solide8z/s8zke3.F
Chd|====================================================================
      SUBROUTINE S4KEH3(LFT,LLT,
     .   PXI, PYI ,PZI ,PXJ, PYJ, PZJ, 
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,KIJ )
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
     .   PXI(*), PXJ(*),PYI(*), PYJ(*), PZI(*), PZJ(*),  
     .   FXX(*),FYY(*),FZZ(*),FXY(*),FXZ(*),FYZ(*),KIJ(3,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C     REAL
      my_real
     .   HIJ(MVSIZ) 
C-----------------------------------------------
      CALL S8ZKEH3(LFT,LLT,
     .   PXI, PYI ,PZI ,PXJ, PYJ, PZJ, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
C
       DO J = 1,3
        DO I=LFT,LLT
         KIJ(J,J,I)=KIJ(J,J,I)+HIJ(I)
        ENDDO
       ENDDO
C
      RETURN
      END
