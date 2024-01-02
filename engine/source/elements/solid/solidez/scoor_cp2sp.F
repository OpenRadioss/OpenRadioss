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
Chd|  SCOOR_CP2SP                   source/elements/solid/solidez/scoor_cp2sp.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SCOOR_CP2SP(
     1   X0,      Y0,      Z0,      X1,
     2   X2,      X3,      X4,      X5,
     3   X6,      X7,      X8,      Y1,
     4   Y2,      Y3,      Y4,      Y5,
     5   Y6,      Y7,      Y8,      Z1,
     6   Z2,      Z3,      Z4,      Z5,
     7   Z6,      Z7,      Z8,      NEL)
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
      INTEGER, INTENT(IN) :: NEL
C     REAL
      my_real
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)
      DOUBLE PRECISION 
     .   X0(MVSIZ,8),Y0(MVSIZ,8),Z0(MVSIZ,8)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, IPT
C-----------------------------------------------
        DO I=1,NEL
        X1(I)= X0(I,1) 
        Y1(I)= Y0(I,1) 
        Z1(I)= Z0(I,1)
        X2(I)= X0(I,2) 
        Y2(I)= Y0(I,2) 
        Z2(I)= Z0(I,2)
        X3(I)= X0(I,3) 
        Y3(I)= Y0(I,3) 
        Z3(I)= Z0(I,3)
        X4(I)= X0(I,4) 
        Y4(I)= Y0(I,4) 
        Z4(I)= Z0(I,4) 
        X5(I)= X0(I,5) 
        Y5(I)= Y0(I,5)
        Z5(I)= Z0(I,5) 
        X6(I)= X0(I,6) 
        Y6(I)= Y0(I,6) 
        Z6(I)= Z0(I,6) 
        X7(I)= X0(I,7) 
        Y7(I)= Y0(I,7) 
        Z7(I)= Z0(I,7) 
        X8(I)= X0(I,8) 
        Y8(I)= Y0(I,8) 
        Z8(I)= Z0(I,8) 
        ENDDO    
C-----------
      RETURN
      END
