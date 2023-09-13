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
Chd|  N2EDGE3L                      source/interfaces/int24/n2edge3l.F
Chd|-- called by -----------
Chd|        INTERSECSH                    source/interfaces/int24/i24dst3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE N2EDGE3L(
     1                           XXI    ,YYI      ,ZZI    ,
     2                           XXJ    ,YYJ      ,ZZJ    ,
     3                           NX     ,NY       ,NZ     ,
     4                           XI     ,YI       ,ZI     ,BBB )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real , INTENT(IN)     :: 
     1                           XXI    ,YYI      ,ZZI    ,
     2                           XXJ    ,YYJ      ,ZZJ    ,
     3                           NX     ,NY       ,NZ     ,
     4                           XI     ,YI       ,ZI     
      my_real , INTENT(OUT)    :: BBB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I
      my_real
     .     XMIJ(3),NIJ(3),DI(3),AA,NORM
C-----------------------------------------------
             XMIJ(1)=XXJ-XXI
             XMIJ(2)=YYJ-YYI
             XMIJ(3)=ZZJ-ZZI
             NIJ(1)= NY*XMIJ(3) - NZ*XMIJ(2)
             NIJ(2)= NZ*XMIJ(1) - NX*XMIJ(3)
             NIJ(3)= NX*XMIJ(2) - NY*XMIJ(1)
             AA = NIJ(1)*NIJ(1)+NIJ(2)*NIJ(2)+NIJ(3)*NIJ(3)
             NORM=MAX(EM20,SQRT(AA))
             DI(1)=HALF*(XXI+XXJ)-XI
             DI(2)=HALF*(YYI+YYJ)-YI
             DI(3)=HALF*(ZZI+ZZJ)-ZI
             BBB = (DI(1)*NIJ(1)+DI(2)*NIJ(2)+DI(3)*NIJ(3))/NORM
             IF (ABS(BBB)<EM3) then
               XMIJ(1)=XXJ-XI
               XMIJ(2)=YYJ-YI
               XMIJ(3)=ZZJ-ZI
               AA = DI(1)*XMIJ(1)+DI(2)*XMIJ(2)+DI(3)*XMIJ(3)
               IF (AA>ABS(BBB)) BBB = AA
             END IF
C
      RETURN
      END
    
