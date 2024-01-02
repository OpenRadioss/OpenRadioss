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
Chd|  SDLENMAX                      source/elements/solid/solide/sdlenmax.F
Chd|-- called by -----------
Chd|        S20FORC3                      source/elements/solid/solide20/s20forc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SDLENMAX(LM_MAX, 
     1   X1,      X2,      X3,      X4,      
     2   X5,      X6,      X7,      X8,      
     3   Y1,      Y2,      Y3,      Y4,      
     4   Y5,      Y6,      Y7,      Y8,      
     5   Z1,      Z2,      Z3,      Z4,      
     6   Z5,      Z6,      Z7,      Z8,      
     7   NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      my_real, DIMENSION(MVSIZ) , INTENT(OUT)  :: LM_MAX 
      my_real, DIMENSION(MVSIZ) , INTENT(IN)  ::  
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,  
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N
      my_real
     .   RX, RY, RZ, SX, SY, SZ, TX, TY, TZ,NORM
C=======================================================================
      DO I=1,NEL
        RX = X1(I)+X2(I)+X5(I)+X6(I)-X3(I)-X4(I)-X7(I)-X8(I)
        RY = Y1(I)+Y2(I)+Y5(I)+Y6(I)-Y3(I)-Y4(I)-Y7(I)-Y8(I)
        RZ = Z1(I)+Z2(I)+Z5(I)+Z6(I)-Z3(I)-Z4(I)-Z7(I)-Z8(I)
        SX = X5(I)+X6(I)+X7(I)+X8(I)-X1(I)-X2(I)-X3(I)-X4(I)
        SY = Y5(I)+Y6(I)+Y7(I)+Y8(I)-Y1(I)-Y2(I)-Y3(I)-Y4(I)
        SZ = Z5(I)+Z6(I)+Z7(I)+Z8(I)-Z1(I)-Z2(I)-Z3(I)-Z4(I)
        TX = X3(I)+X2(I)+X7(I)+X6(I)-X1(I)-X4(I)-X5(I)-X8(I)
        TY = Y3(I)+Y2(I)+Y7(I)+Y6(I)-Y1(I)-Y4(I)-Y5(I)-Y8(I)
        TZ = Z3(I)+Z2(I)+Z7(I)+Z6(I)-Z1(I)-Z4(I)-Z5(I)-Z8(I)
        NORM = RX*RX + RY*RY + RZ*RZ
        LM_MAX(I) = NORM
        NORM = SX*SX + SY*SY + SZ*SZ
        LM_MAX(I) = MAX(LM_MAX(I),NORM)
        NORM = TX*TX + TY*TY + TZ*TZ
        LM_MAX(I) = MAX(LM_MAX(I),NORM)
      ENDDO
      DO I=1,NEL
        LM_MAX(I) = FOURTH*SQRT(LM_MAX(I))
      ENDDO 
C
      RETURN
      END
