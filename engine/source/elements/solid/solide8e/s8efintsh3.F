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
Chd|  S8EFINTSH3                    source/elements/solid/solide8e/s8efintsh3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EFINTSH3(
     1   SIG,     PX1,     PX2,     PX3,
     2   PX4,     PY1,     PY2,     PY3,
     3   PY4,     PZ1,     PZ2,     PZ3,
     4   PZ4,     F11,     F21,     F31,
     5   F12,     F22,     F32,     F13,
     6   F23,     F33,     F14,     F24,
     7   F34,     F15,     F25,     F35,
     8   F16,     F26,     F36,     F17,
     9   F27,     F37,     F18,     F28,
     A   F38,     VOL,     NEL)
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
#include      "nsvis_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   SIG(NEL,6),
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*),
     .   VOL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .   S4(MVSIZ), S5(MVSIZ), S6(MVSIZ),FINT,FVOL
C-----------------------------------------------
      DO I=1,NEL
        S4(I)=(SIG(I,4)+SVIS(I,4))*VOL(I)
        S5(I)=(SIG(I,5)+SVIS(I,5))*VOL(I)
        S6(I)=(SIG(I,6)+SVIS(I,6))*VOL(I)
      ENDDO
C      
       DO I=1,NEL
          FINT=S4(I)*PY1(I)+S6(I)*PZ1(I)
          F11(I)=F11(I)-FINT
          F17(I)=F17(I)+FINT
          FINT=S4(I)*PX1(I)+S5(I)*PZ1(I)
          F21(I)=F21(I)-FINT
          F27(I)=F27(I)+FINT
          FINT=S6(I)*PX1(I)+S5(I)*PY1(I)
          F31(I)=F31(I)-FINT
          F37(I)=F37(I)+FINT
          FINT=S4(I)*PY2(I)+S6(I)*PZ2(I)
          F12(I)=F12(I)-FINT
          F18(I)=F18(I)+FINT
          FINT=S4(I)*PX2(I)+S5(I)*PZ2(I)
          F22(I)=F22(I)-FINT
          F28(I)=F28(I)+FINT
          FINT=S6(I)*PX2(I)+S5(I)*PY2(I)
          F32(I)=F32(I)-FINT
          F38(I)=F38(I)+FINT
          FINT=S4(I)*PY3(I)+S6(I)*PZ3(I)
          F13(I)=F13(I)-FINT
          F15(I)=F15(I)+FINT
          FINT=S4(I)*PX3(I)+S5(I)*PZ3(I)
          F23(I)=F23(I)-FINT
          F25(I)=F25(I)+FINT
          FINT=S6(I)*PX3(I)+S5(I)*PY3(I)
          F33(I)=F33(I)-FINT
          F35(I)=F35(I)+FINT
          FINT=S4(I)*PY4(I)+S6(I)*PZ4(I)
          F14(I)=F14(I)-FINT
          F16(I)=F16(I)+FINT
          FINT=S4(I)*PX4(I)+S5(I)*PZ4(I)
          F24(I)=F24(I)-FINT
          F26(I)=F26(I)+FINT
          FINT=S6(I)*PX4(I)+S5(I)*PY4(I)
          F34(I)=F34(I)-FINT
          F36(I)=F36(I)+FINT
       ENDDO
C ---------------
      RETURN
      END
