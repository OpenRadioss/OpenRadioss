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
Chd|  S8ZFINTP3                     source/elements/solid/solide8z/s8zfintp3.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZFINTP3(
     1   PX1,     PX2,     PX3,     PX4,
     2   PY1,     PY2,     PY3,     PY4,
     3   PZ1,     PZ2,     PZ3,     PZ4,
     4   F11,     F21,     F31,     F12,
     5   F22,     F32,     F13,     F23,
     6   F33,     F14,     F24,     F34,
     7   F15,     F25,     F35,     F16,
     8   F26,     F36,     F17,     F27,
     9   F37,     F18,     F28,     F38,
     A   VOL,     PP,      IDEGE,   NEL)
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
      INTEGER IDEGE(*)
C     REAL
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*),
     .   VOL(*),PP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .   SP,SX,SY,SZ
C-----------------------------------------------
       DO I=1,NEL
        IF (IDEGE(I)>10) CYCLE
        SP =PP(I)*VOL(I)
        SX =SP*PX1(I)
        SY =SP*PY1(I)
        SZ =SP*PZ1(I)
        F11(I)=F11(I)-SX
        F17(I)=F17(I)+SX
        F21(I)=F21(I)-SY
        F27(I)=F27(I)+SY
        F31(I)=F31(I)-SZ
        F37(I)=F37(I)+SZ
        SX =SP*PX2(I)
        SY =SP*PY2(I)
        SZ =SP*PZ2(I)
        F12(I)=F12(I)-SX
        F18(I)=F18(I)+SX
        F22(I)=F22(I)-SY
        F28(I)=F28(I)+SY
        F32(I)=F32(I)-SZ
        F38(I)=F38(I)+SZ
        SX =SP*PX3(I)
        SY =SP*PY3(I)
        SZ =SP*PZ3(I)
        F13(I)=F13(I)-SX
        F15(I)=F15(I)+SX
        F23(I)=F23(I)-SY
        F25(I)=F25(I)+SY
        F33(I)=F33(I)-SZ
        F35(I)=F35(I)+SZ
        SX =SP*PX4(I)
        SY =SP*PY4(I)
        SZ =SP*PZ4(I)
        F14(I)=F14(I)-SX
        F16(I)=F16(I)+SX
        F24(I)=F24(I)-SY
        F26(I)=F26(I)+SY
        F34(I)=F34(I)-SZ
        F36(I)=F36(I)+SZ
       ENDDO
c-----------
      RETURN
      END
