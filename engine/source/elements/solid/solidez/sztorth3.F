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
Chd|====================================================================
Chd|  SZTORTH3                      source/elements/solid/solidez/sztorth3.F
Chd|-- called by -----------
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SZTORTH3(JFT,JLT,ISORTH,NEL,
     .                   E1X, E2X, E3X, E1Y, E2Y, E3Y, E1Z, E2Z, E3Z ,
     .                   G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z,GAMA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C----------------------------------------------- 
      INTEGER JFT, JLT,ISORTH,NEL
C     REAL
      my_real
     .   E1X(*),E1Y(*),E1Z(*),
     .   E2X(*),E2Y(*),E2Z(*),E3X(*),E3Y(*),E3Z(*),
     .   G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*),GAMA(NEL,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33
C=======================================================================
      IF (ISORTH > 0) THEN 
       DO I=JFT,JLT
         R11=GAMA(I,1)
         R12=GAMA(I,2)
         R13=GAMA(I,3)
         R21=GAMA(I,4)
         R22=GAMA(I,5)
         R23=GAMA(I,6)
         R31=R12*R23-R13*R22
         R32=R13*R21-R11*R23
         R33=R11*R22-R12*R21
         G1X(I)=R11*E1X(I)+R12*E2X(I)+R13*E3X(I)
         G1Y(I)=R11*E1Y(I)+R12*E2Y(I)+R13*E3Y(I)
         G1Z(I)=R11*E1Z(I)+R12*E2Z(I)+R13*E3Z(I)
         G2X(I)=R21*E1X(I)+R22*E2X(I)+R23*E3X(I)
         G2Y(I)=R21*E1Y(I)+R22*E2Y(I)+R23*E3Y(I)
         G2Z(I)=R21*E1Z(I)+R22*E2Z(I)+R23*E3Z(I)
         G3X(I)=R31*E1X(I)+R32*E2X(I)+R33*E3X(I)
         G3Y(I)=R31*E1Y(I)+R32*E2Y(I)+R33*E3Y(I)
         G3Z(I)=R31*E1Z(I)+R32*E2Z(I)+R33*E3Z(I)    
       ENDDO
      ELSE
       DO I=JFT,JLT
         G1X(I)=E1X(I)
         G1Y(I)=E1Y(I)
         G1Z(I)=E1Z(I)
         G2X(I)=E2X(I)
         G2Y(I)=E2Y(I)
         G2Z(I)=E2Z(I)
         G3X(I)=E3X(I)
         G3Y(I)=E3Y(I)
         G3Z(I)=E3Z(I)    
       ENDDO
      ENDIF 
C-----------
      RETURN
      END
