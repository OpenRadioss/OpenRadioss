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
Chd|  VOLINT                        source/interfaces/inter3d1/volint.F
Chd|-- called by -----------
Chd|        I11STI3                       source/interfaces/inter3d1/i11sti3.F
Chd|        I20STI3                       source/interfaces/inter3d1/i20sti3.F
Chd|        I20STI3E                      source/interfaces/inter3d1/i20sti3.F
Chd|        I24GAPM                       source/interfaces/inter3d1/i24sti3.F
Chd|        I24LL_KG                      source/interfaces/inter3d1/i24sti3.F
Chd|        I25GAPM                       source/interfaces/inter3d1/i25sti3.F
Chd|        I2BUC1                        source/interfaces/inter3d1/i2buc1.F
Chd|        I2COR3                        source/interfaces/inter3d1/i2cor3.F
Chd|        I3STI3                        source/interfaces/inter3d1/i3sti3.F
Chd|        I7STI3                        source/interfaces/inter3d1/i7sti3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VOLINT(VOL)
C-----------------------------------------------
C   COMPUTE HEXAHEDRON VOLUME
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT(OUT) :: VOL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real X17, X28, X35, X46,
     .        Y17, Y28, Y35, Y46,
     .        Z17, Z28, Z35, Z46
      my_real A17, A28, B17, B28, C17, C28 
      my_real JAC1, JAC2, JAC3, JAC4, JAC5, JAC6, JAC7, JAC8, JAC9 
      my_real JAC_59_68, JAC_67_49, JAC_48_57 
C-------------------------------------------------------
      X17 = XC(7)-XC(1)
      X28 = XC(8)-XC(2)
      X35 = XC(5)-XC(3)
      X46 = XC(6)-XC(4)
C
      Y17 = YC(7)-YC(1)
      Y28 = YC(8)-YC(2)
      Y35 = YC(5)-YC(3)
      Y46 = YC(6)-YC(4)
C
      Z17 = ZC(7)-ZC(1)
      Z28 = ZC(8)-ZC(2)
      Z35 = ZC(5)-ZC(3)
      Z46 = ZC(6)-ZC(4)
C
      JAC4 = X17 + X28 - X35 - X46 
      JAC5 = Y17 + Y28 - Y35 - Y46 
      JAC6 = Z17 + Z28 - Z35 - Z46 
C
      A17 = X17 + X46 
      A28 = X28 + X35 
      B17 = Y17 + Y46 
      B28 = Y28 + Y35 
      C17 = Z17 + Z46 
      C28 = Z28 + Z35 
C
      JAC7 = A17 + A28 
      JAC8 = B17 + B28 
      JAC9 = C17 + C28 
      JAC1 = A17 - A28 
      JAC2 = B17 - B28 
      JAC3 = C17 - C28 
C
      JAC_59_68 = JAC5 * JAC9 - JAC6 * JAC8 
      JAC_67_49 = JAC6 * JAC7 - JAC4 * JAC9 
      JAC_48_57 = JAC4 * JAC8 - JAC5 * JAC7 
C
      VOL =  ZEP015625 * (JAC1 * JAC_59_68 + JAC2 * JAC_67_49 + JAC3 * JAC_48_57)

      RETURN
      END
