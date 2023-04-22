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
Chd|  S8ZKED3                       source/elements/solid/solide8z/s8zked3.F
Chd|-- called by -----------
Chd|        S10CUMG3                      source/elements/solid/solide10/s10cumg3.F
Chd|        S20CUMG3                      source/elements/solid/solide20/s20cumg3.F
Chd|        S4CUMG3                       source/elements/solid/solide4/s4cumg3.F
Chd|        S8ZLKE3                       source/elements/solid/solide8z/s8zlke3.F
Chd|        S8ZLKEP3                      source/elements/solid/solide8z/s8zlkep3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZKED3(LFT,LLT,
     .   PXI, PYI ,PZI ,PXJ, PYJ, PZJ, DD, KIJ,IS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT,IS
C     REAL
      my_real
     .   PXI(*), PXJ(*),PYI(*), PYJ(*), PZI(*), PZJ(*),  
     .   DD(3,3,*),KIJ(3,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
C-----------------------------------------------
      IF (IS==1) THEN
       DO I=LFT,LLT
        KIJ(1,1,I)=KIJ(1,1,I)+PXI(I)*DD(1,1,I)*PXJ(I)
        KIJ(2,2,I)=KIJ(2,2,I)+PYI(I)*DD(2,2,I)*PYJ(I)
        KIJ(3,3,I)=KIJ(3,3,I)+PZI(I)*DD(3,3,I)*PZJ(I)
        KIJ(1,2,I)=KIJ(1,2,I)+PXI(I)*DD(1,2,I)*PYJ(I)
        KIJ(1,3,I)=KIJ(1,3,I)+PXI(I)*DD(1,3,I)*PZJ(I)
        KIJ(2,3,I)=KIJ(2,3,I)+PYI(I)*DD(2,3,I)*PZJ(I)
       ENDDO
      ELSE
       DO I=LFT,LLT
        KIJ(1,1,I)=KIJ(1,1,I)+PXI(I)*DD(1,1,I)*PXJ(I)
        KIJ(2,2,I)=KIJ(2,2,I)+PYI(I)*DD(2,2,I)*PYJ(I)
        KIJ(3,3,I)=KIJ(3,3,I)+PZI(I)*DD(3,3,I)*PZJ(I)
        KIJ(1,2,I)=KIJ(1,2,I)+PXI(I)*DD(1,2,I)*PYJ(I)
        KIJ(2,1,I)=KIJ(2,1,I)+PXJ(I)*DD(2,1,I)*PYI(I)
        KIJ(1,3,I)=KIJ(1,3,I)+PXI(I)*DD(1,3,I)*PZJ(I)
        KIJ(3,1,I)=KIJ(3,1,I)+PXJ(I)*DD(3,1,I)*PZI(I)
        KIJ(2,3,I)=KIJ(2,3,I)+PYI(I)*DD(2,3,I)*PZJ(I)
        KIJ(3,2,I)=KIJ(3,2,I)+PYJ(I)*DD(3,2,I)*PZI(I)
       ENDDO
      ENDIF 
      RETURN
      END
