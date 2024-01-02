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
C                                                                      12
Chd|====================================================================
Chd|  STORTH3                       source/elements/solid/solidez/storth3.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE STORTH3(
     1   JFT,     JLT,     NEL,     G1X,
     2   G1Y,     G1Z,     G2X,     G2Y,
     3   G2Z,     G3X,     G3Y,     G3Z,
     4   GAMA,    ISORTH)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C----------------------------------------------- 
      INTEGER, INTENT(IN) :: ISORTH
      INTEGER JFT, JLT ,NEL
C     REAL
      my_real
     .   G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*),GAMA(NEL,6)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C=======================================================================
      IF (ISORTH == 1) THEN 
      DO I=JFT,JLT
         G1X(I)=GAMA(I,1)
         G1Y(I)=GAMA(I,2)
         G1Z(I)=GAMA(I,3)
         G2X(I)=GAMA(I,4)
         G2Y(I)=GAMA(I,5)
         G2Z(I)=GAMA(I,6)
         G3X(I)=G1Y(I)*G2Z(I)-G1Z(I)*G2Y(I)
         G3Y(I)=G1Z(I)*G2X(I)-G1X(I)*G2Z(I)
         G3Z(I)=G1X(I)*G2Y(I)-G1Y(I)*G2X(I)
       ENDDO
        ELSE
       DO I=JFT,JLT
         G1X(I)=ONE
         G1Y(I)=ZERO
         G1Z(I)=ZERO
         G2X(I)=ZERO
         G2Y(I)=ONE
         G2Z(I)=ZERO
         G3X(I)=ZERO
         G3Y(I)=ZERO
         G3Z(I)=ONE       
       ENDDO
        ENDIF 
C-----------
      RETURN
      END
