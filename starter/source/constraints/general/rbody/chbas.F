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
C
Chd|====================================================================
Chd|  CHBAS                         source/constraints/general/rbody/chbas.F
Chd|-- called by -----------
Chd|        INIRBY                        source/constraints/general/rbody/inirby.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CHBAS(A,M)
C
C    ENTREES A MATRICE DE PASSAGE X'=AX
C            M MATRICE DANS LA BASE X'
C    SORTIES M MATRICE DANS LA BASE X
C               M=A-1*M*A=AT*M*A
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   A(3,*), M(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K
C     REAL
      my_real
     .   C(3,3)
        DO 30 I=1,3
          DO 20 J=1,3
            C(I,J)=0.
            DO 10 K=1,3
              C(I,J)=C(I,J)+A(I,K)*M(K,J)
 10         CONTINUE
 20       CONTINUE
 30     CONTINUE
        DO 60 I=1,3
          DO 50 J=1,3
            M(I,J)=0.
            DO 40 K=1,3
              M(I,J)=M(I,J)+C(I,K)*A(J,K)
 40         CONTINUE
 50       CONTINUE
 60     CONTINUE
C
      RETURN
      END
