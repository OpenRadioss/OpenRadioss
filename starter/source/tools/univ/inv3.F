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
Chd|  INV3                          source/tools/univ/inv3.F      
Chd|-- called by -----------
Chd|        I2LOCEQ                       ../common_source/interf/i2loceq.F
Chd|        I2LOCEQ_27                    ../common_source/interf/i2loceq.F
Chd|        SMORTH3                       source/elements/solid/solide/smorth3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INV3(A,B)
C----------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C----------------------------------------------------------
C   D u m m y   A r g u m e n t s   a n d   F u n c t i o n
C----------------------------------------------------------
      my_real A(9),B(9)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real DET
C=======================================================================

      DET = A(1)*A(5)*A(9)+A(4)*A(8)*A(3)+A(7)*A(2)*A(6)
     .    - A(4)*A(2)*A(9)-A(1)*A(8)*A(6)-A(7)*A(5)*A(3)

      B(1) =  (A(5)*A(9)-A(6)*A(8)) / DET
      B(4) = -(A(4)*A(9)-A(6)*A(7)) / DET
      B(7) =  (A(4)*A(8)-A(5)*A(7)) / DET

      B(2) = -(A(2)*A(9)-A(3)*A(8)) / DET
      B(5) =  (A(1)*A(9)-A(3)*A(7)) / DET
      B(8) = -(A(1)*A(8)-A(2)*A(7)) / DET

      B(3) =  (A(2)*A(6)-A(3)*A(5)) / DET
      B(6) = -(A(1)*A(6)-A(3)*A(4)) / DET
      B(9) =  (A(1)*A(5)-A(2)*A(4)) / DET
C-----------------------------------------------
      RETURN
      END SUBROUTINE INV3
