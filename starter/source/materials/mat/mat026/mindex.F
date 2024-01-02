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
Chd|  MINDEX                        source/materials/mat/mat026/mindex.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      INTEGER FUNCTION MINDEX(AA,NI,NJ,A)
C     RECHERCHE D'INDEX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NI, NJ
C     REAL
      my_real
     .   AA(NI,NJ), A
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I1, I2, I3
C     REAL
      my_real
     .   A1, A2, A3
      I1 = 1
      I2 = NJ
      A1 = AA(1,I1)
      A2 = AA(1,I2)
      IF(A2>=A1)THEN
 1      CONTINUE
         I3 = (I1 + I2)/2
         A3 = AA(1,I3)
         IF(A3>=A)THEN
           I2 = I3
           A2 = A3
         ELSE
           I1 = I3
           A1 = A3
         ENDIF
       IF(I2-I1>1) GOTO 1
      ELSE
 2      CONTINUE
         I3 = (I1 + I2)/2
         A3 = AA(1,I3)
         IF(A3<A)THEN
           I2 = I3
           A2 = A3
         ELSE
           I1 = I3
           A1 = A3
         ENDIF
       IF(I2-I1>1) GOTO 2
      ENDIF
C
      MINDEX=I1
      RETURN
      END
