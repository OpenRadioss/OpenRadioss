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
Chd|  S10KGEO3                      source/elements/solid/solide10/s10kgeo3.F
Chd|-- called by -----------
Chd|        S10KE3                        source/elements/solid/solide10/s10ke3.F
Chd|-- calls ---------------
Chd|        S4KEH3                        source/elements/solid/solide4/s4keh3.F
Chd|====================================================================
      SUBROUTINE S10KGEO3(
     1   SIG,     VOL,     PX,      PY,
     2   PZ,      K11,     K12,     K13,
     3   K14,     K15,     K16,     K17,
     4   K18,     K19,     K10,     K22,
     5   K23,     K24,     K25,     K26,
     6   K27,     K28,     K29,     K20,
     7   K33,     K34,     K35,     K36,
     8   K37,     K38,     K39,     K30,
     9   K44,     K45,     K46,     K47,
     A   K48,     K49,     K40,     K55,
     B   K56,     K57,     K58,     K59,
     C   K50,     K66,     K67,     K68,
     D   K69,     K60,     K77,     K78,
     E   K79,     K70,     K88,     K89,
     F   K80,     K99,     K90,     K00,
     G   NEL)
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
      INTEGER NEL
C     REAL
      my_real
     .  PX(MVSIZ,10),PY(MVSIZ,10),PZ(MVSIZ,10),
     .  VOL(*),SIG(NEL,6)
      my_real
     .   K11(9,*)   ,K12(9,*)   ,K13(9,*)    ,K14(9,*)    ,K15(9,*)   ,
     .   K16(9,*)   ,K17(9,*)   ,K18(9,*)    ,K19(9,*)    ,K10(9,*)   ,
     .   K22(9,*)   ,K23(9,*)   ,K24(9,*)    ,K25(9,*)    ,K26(9,*)   ,
     .   K27(9,*)   ,K28(9,*)   ,K29(9,*)    ,K20(9,*)    ,K33(9,*)   ,
     .   K34(9,*)   ,K35(9,*)   ,K36(9,*)    ,K37(9,*)    ,K38(9,*)   ,
     .   K39(9,*)   ,K30(9,*)   ,K44(9,*)   ,K45(9,*)    ,K46(9,*)    ,
     .   K47(9,*)   ,K48(9,*)   ,K49(9,*)   ,K40(9,*)    ,K55(9,*)   ,
     .   K56(9,*)   ,K57(9,*)   ,K58(9,*)   ,K59(9,*)    ,K50(9,*)   ,
     .   K66(9,*)   ,K67(9,*)   ,K68(9,*)   ,K69(9,*)    ,K60(9,*)   ,
     .   K77(9,*)   ,K78(9,*)   ,K79(9,*)   ,K70(9,*)    ,K88(9,*)   ,
     .   K89(9,*)   ,K80(9,*)   ,K99(9,*)   ,K90(9,*)    ,K00(9,*)   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real
     .   FXX(MVSIZ),FYY(MVSIZ),FZZ(MVSIZ),FXY(MVSIZ),FXZ(MVSIZ),
     .   FYZ(MVSIZ)
C-----------------------------------------------
       DO I=1,NEL
        FXX(I)=SIG(I,1)*VOL(I)
        FYY(I)=SIG(I,2)*VOL(I)
        FZZ(I)=SIG(I,3)*VOL(I)
        FXY(I)=SIG(I,4)*VOL(I)
        FYZ(I)=SIG(I,5)*VOL(I)
        FXZ(I)=SIG(I,6)*VOL(I)
       ENDDO
C -----------------------------------------------
      CALL S4KEH3(1,NEL,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,1),PY(1,1),PZ(1,1),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K11 )
      CALL S4KEH3(1,NEL,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,2),PY(1,2),PZ(1,2),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K22 )
      CALL S4KEH3(1,NEL,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,3),PY(1,3),PZ(1,3),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K33 )
      CALL S4KEH3(1,NEL,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,4),PY(1,4),PZ(1,4),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K44 )
      CALL S4KEH3(1,NEL,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,5),PY(1,5),PZ(1,5),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K55 )
      CALL S4KEH3(1,NEL,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,6),PY(1,6),PZ(1,6),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K66 )
      CALL S4KEH3(1,NEL,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,7),PY(1,7),PZ(1,7),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K77 )
      CALL S4KEH3(1,NEL,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K88 )
      CALL S4KEH3(1,NEL,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K99 )
      CALL S4KEH3(1,NEL,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K00 )
C
      CALL S4KEH3(1,NEL,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,2),PY(1,2),PZ(1,2),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K12 )
      CALL S4KEH3(1,NEL,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,3),PY(1,3),PZ(1,3),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K13 )
      CALL S4KEH3(1,NEL,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,4),PY(1,4),PZ(1,4),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K14 )
      CALL S4KEH3(1,NEL,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,5),PY(1,5),PZ(1,5),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K15 )
      CALL S4KEH3(1,NEL,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,6),PY(1,6),PZ(1,6),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K16 )
      CALL S4KEH3(1,NEL,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,7),PY(1,7),PZ(1,7),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K17 )
      CALL S4KEH3(1,NEL,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K18 )
      CALL S4KEH3(1,NEL,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K19 )
      CALL S4KEH3(1,NEL,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K10 )
      CALL S4KEH3(1,NEL,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,3),PY(1,3),PZ(1,3),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K23 )
      CALL S4KEH3(1,NEL,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,4),PY(1,4),PZ(1,4),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K24 )
      CALL S4KEH3(1,NEL,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,5),PY(1,5),PZ(1,5),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K25 )
      CALL S4KEH3(1,NEL,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,6),PY(1,6),PZ(1,6),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K26 )
      CALL S4KEH3(1,NEL,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,7),PY(1,7),PZ(1,7),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K27 )
      CALL S4KEH3(1,NEL,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K28 )
      CALL S4KEH3(1,NEL,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K29 )
      CALL S4KEH3(1,NEL,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K20 )
      CALL S4KEH3(1,NEL,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,4),PY(1,4),PZ(1,4),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K34 )
      CALL S4KEH3(1,NEL,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,5),PY(1,5),PZ(1,5),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K35 )
      CALL S4KEH3(1,NEL,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,6),PY(1,6),PZ(1,6),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K36 )
      CALL S4KEH3(1,NEL,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,7),PY(1,7),PZ(1,7),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K37 )
      CALL S4KEH3(1,NEL,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K38 )
      CALL S4KEH3(1,NEL,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K39 )
      CALL S4KEH3(1,NEL,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K30 )
      CALL S4KEH3(1,NEL,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,5),PY(1,5),PZ(1,5),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K45 )
      CALL S4KEH3(1,NEL,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,6),PY(1,6),PZ(1,6),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K46 )
      CALL S4KEH3(1,NEL,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,7),PY(1,7),PZ(1,7),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K47 )
      CALL S4KEH3(1,NEL,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K48 )
      CALL S4KEH3(1,NEL,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K49 )
      CALL S4KEH3(1,NEL,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K40 )
      CALL S4KEH3(1,NEL,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,6),PY(1,6),PZ(1,6),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K56 )
      CALL S4KEH3(1,NEL,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,7),PY(1,7),PZ(1,7),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K57 )
      CALL S4KEH3(1,NEL,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K58 )
      CALL S4KEH3(1,NEL,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K59 )
      CALL S4KEH3(1,NEL,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K50 )
      CALL S4KEH3(1,NEL,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,7),PY(1,7),PZ(1,7),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K67 )
      CALL S4KEH3(1,NEL,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K68 )
      CALL S4KEH3(1,NEL,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K69 )
      CALL S4KEH3(1,NEL,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K60 )
      CALL S4KEH3(1,NEL,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K78 )
      CALL S4KEH3(1,NEL,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K79 )
      CALL S4KEH3(1,NEL,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K70 )
      CALL S4KEH3(1,NEL,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K89 )
      CALL S4KEH3(1,NEL,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K80 )
      CALL S4KEH3(1,NEL,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K90 )
C
      RETURN
      END
