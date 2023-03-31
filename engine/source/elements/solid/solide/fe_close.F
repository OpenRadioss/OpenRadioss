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
Chd|  FE_CLOSE                      source/elements/solid/solide/fe_close.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FE_CLOSE(
     1   HH,      RHO,     VOLN,    VX1,
     2   VX2,     VX3,     VX4,     VX5,
     3   VX6,     VX7,     VX8,     VY1,
     4   VY2,     VY3,     VY4,     VY5,
     5   VY6,     VY7,     VY8,     VZ1,
     6   VZ2,     VZ3,     VZ4,     VZ5,
     7   VZ6,     VZ7,     VZ8,     F11,
     8   F21,     F31,     F12,     F22,
     9   F32,     F13,     F23,     F33,
     A   F14,     F24,     F34,     F15,
     B   F25,     F35,     F16,     F26,
     C   F36,     F17,     F27,     F37,
     D   F18,     F28,     F38,     NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      my_real
     .   F11(MVSIZ),F21(MVSIZ),F31(MVSIZ),
     .   F12(MVSIZ),F22(MVSIZ),F32(MVSIZ),
     .   F13(MVSIZ),F23(MVSIZ),F33(MVSIZ),
     .   F14(MVSIZ),F24(MVSIZ),F34(MVSIZ),
     .   F15(MVSIZ),F25(MVSIZ),F35(MVSIZ),
     .   F16(MVSIZ),F26(MVSIZ),F36(MVSIZ),
     .   F17(MVSIZ),F27(MVSIZ),F37(MVSIZ),
     .   F18(MVSIZ),F28(MVSIZ),F38(MVSIZ),
     .   HH(*),RHO(*),VOLN(*),
     .   VX1(*),VX2(*),VX3(*),VX4(*),VX5(*),VX6(*),VX7(*),VX8(*),
     .   VY1(*),VY2(*),VY3(*),VY4(*),VY5(*),VY6(*),VY7(*),VY8(*),
     .   VZ1(*),VZ2(*),VZ3(*),VZ4(*),VZ5(*),VZ6(*),VZ7(*),VZ8(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     . FAC  
C      
      IF(DT1==ZERO)RETURN
      DO I=1,NEL
        FAC=-ONE_OVER_8*HH(I)*RHO(I)*VOLN(I)/DT1
 	F11(I) = F11(I) + FAC*VX1(I)
 	F12(I) = F12(I) + FAC*VX2(I)
 	F13(I) = F13(I) + FAC*VX3(I)
 	F14(I) = F14(I) + FAC*VX4(I)
 	F15(I) = F15(I) + FAC*VX5(I)
 	F16(I) = F16(I) + FAC*VX6(I)
 	F17(I) = F17(I) + FAC*VX7(I)
 	F18(I) = F18(I) + FAC*VX8(I)

 	F21(I) = F21(I) + FAC*VY1(I)
 	F22(I) = F22(I) + FAC*VY2(I)
 	F23(I) = F23(I) + FAC*VY3(I)
 	F24(I) = F24(I) + FAC*VY4(I)
 	F25(I) = F25(I) + FAC*VY5(I)
 	F26(I) = F26(I) + FAC*VY6(I)
 	F27(I) = F27(I) + FAC*VY7(I)
 	F28(I) = F28(I) + FAC*VY8(I)

 	F31(I) = F31(I) + FAC*VZ1(I)
 	F32(I) = F32(I) + FAC*VZ2(I)
 	F33(I) = F33(I) + FAC*VZ3(I)
 	F34(I) = F34(I) + FAC*VZ4(I)
 	F35(I) = F35(I) + FAC*VZ5(I)
 	F36(I) = F36(I) + FAC*VZ6(I)
 	F37(I) = F37(I) + FAC*VZ7(I)
 	F38(I) = F38(I) + FAC*VZ8(I)
      ENDDO
      RETURN
      END		
	
