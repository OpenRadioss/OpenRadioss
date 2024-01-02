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
C
Chd|====================================================================
Chd|  TCOORI                        source/elements/truss/tcoori.F
Chd|-- called by -----------
Chd|        INIVOID                       source/elements/initia/inivoid.F
Chd|        TINIT3                        source/elements/truss/tinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TCOORI(X,NCP,MXT, MXG, NC1, NC2, 
     .     X1, X2, Y1, Y2, Z1, Z2)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NCP(5,*), MXT(*), MXG(*), NC1(*), NC2(*)
      my_real
     .     X(3,*), X1(*), X2(*), Y1(*), Y2(*), Z1(*), Z2(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   XNOR
C--------------------------------------------------
C     CONNECTIVITES ET MATERIEL                   |
C--------------------------------------------------
      DO I=LFT,LLT
        MXT(I)=NCP(1,I)
        NC1(I)=NCP(2,I)
        NC2(I)=NCP(3,I)
        MXG(I)=NCP(4,I)
      END DO
C
C----------------------------
C     COORDONNEES           |
C----------------------------
      DO I=LFT,LLT
        X1(I)=X(1,NC1(I))
        Y1(I)=X(2,NC1(I))
        Z1(I)=X(3,NC1(I))
        X2(I)=X(1,NC2(I))
        Y2(I)=X(2,NC2(I))
        Z2(I)=X(3,NC2(I))
      END DO

      RETURN
      END
