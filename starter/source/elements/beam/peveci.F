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
Chd|  PEVECI                        source/elements/beam/peveci.F 
Chd|-- called by -----------
Chd|        PINIT3                        source/elements/beam/pinit3.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PEVECI(RLOC, X1,X3, Y1,Y3, Z1,Z3) 
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
C     REAL
      my_real
     .   RLOC(3,*),
     .   X1(MVSIZ), X3(MVSIZ), 
     .   Y1(MVSIZ), Y3(MVSIZ),
     .   Z1(MVSIZ), Z3(MVSIZ), 
     .   E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   SUM
C---------------------------------------------------------------------
      DO I=LFT,LLT
        E2X(I)=X3(I)-X1(I)
        E2Y(I)=Y3(I)-Y1(I)
        E2Z(I)=Z3(I)-Z1(I)
C       LE MESSAGE D'ERREUR CORRESPONDANT EST DANS PCOORI
        IF (SQRT(E2X(I)**2 + E2Y(I)**2 + E2Z(I)**2) < EM20) RETURN
      ENDDO
C
      DO I=LFT,LLT
        SUM   =SQRT(E2X(I)*E2X(I)+E2Y(I)*E2Y(I)+E2Z(I)*E2Z(I))
        E2X(I)=E2X(I)/SUM
        E2Y(I)=E2Y(I)/SUM
        E2Z(I)=E2Z(I)/SUM
      ENDDO
C
      DO I=LFT,LLT
        RLOC(1,I)=E2X(I)
        RLOC(2,I)=E2Y(I)
        RLOC(3,I)=E2Z(I)
      ENDDO
C-----------
      RETURN
      END
