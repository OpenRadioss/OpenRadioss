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
Chd|  S20KGEO3                      source/elements/solid/solide20/s20ke3.F
Chd|-- called by -----------
Chd|        S20KE3                        source/elements/solid/solide20/s20ke3.F
Chd|-- calls ---------------
Chd|        S4KEH3                        source/elements/solid/solide4/s4ke3.F
Chd|====================================================================
      SUBROUTINE S20KGEO3(
     1   SIG,     VOL,     PX,      PY,
     2   PZ,      K11,     K12,     K13,
     3   K14,     K15,     K16,     K17,
     4   K18,     K19,     K110,    K111,
     5   K112,    K113,    K114,    K115,
     6   K116,    K117,    K118,    K119,
     7   K120,    K22,     K23,     K24,
     8   K25,     K26,     K27,     K28,
     9   K29,     K210,    K211,    K212,
     A   K213,    K214,    K215,    K216,
     B   K217,    K218,    K219,    K220,
     C   K33,     K34,     K35,     K36,
     D   K37,     K38,     K39,     K310,
     E   K311,    K312,    K313,    K314,
     F   K315,    K316,    K317,    K318,
     G   K319,    K320,    K44,     K45,
     H   K46,     K47,     K48,     K49,
     I   K410,    K411,    K412,    K413,
     J   K414,    K415,    K416,    K417,
     K   K418,    K419,    K420,    K55,
     L   K56,     K57,     K58,     K59,
     M   K510,    K511,    K512,    K513,
     N   K514,    K515,    K516,    K517,
     O   K518,    K519,    K520,    K66,
     P   K67,     K68,     K69,     K610,
     Q   K611,    K612,    K613,    K614,
     R   K615,    K616,    K617,    K618,
     S   K619,    K620,    K77,     K78,
     T   K79,     K710,    K711,    K712,
     U   K713,    K714,    K715,    K716,
     V   K717,    K718,    K719,    K720,
     W   K88,     K89,     K810,    K811,
     X   K812,    K813,    K814,    K815,
     Y   K816,    K817,    K818,    K819,
     Z   K820,    K99,     K910,    K911,
     1   K912,    K913,    K914,    K915,
     2   K916,    K917,    K918,    K919,
     3   K920,    K1010,   K1011,   K1012,
     4   K1013,   K1014,   K1015,   K1016,
     5   K1017,   K1018,   K1019,   K1020,
     6   K1111,   K1112,   K1113,   K1114,
     7   K1115,   K1116,   K1117,   K1118,
     8   K1119,   K1120,   K1212,   K1213,
     9   K1214,   K1215,   K1216,   K1217,
     A   K1218,   K1219,   K1220,   K1313,
     B   K1314,   K1315,   K1316,   K1317,
     C   K1318,   K1319,   K1320,   K1414,
     D   K1415,   K1416,   K1417,   K1418,
     E   K1419,   K1420,   K1515,   K1516,
     F   K1517,   K1518,   K1519,   K1520,
     G   K1616,   K1617,   K1618,   K1619,
     H   K1620,   K1717,   K1718,   K1719,
     I   K1720,   K1818,   K1819,   K1820,
     J   K1919,   K1920,   K2020,   NEL)
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
     .  PX(MVSIZ,20),PY(MVSIZ,20),PZ(MVSIZ,20),
     .  VOL(*),SIG(NEL,6)
      my_real
     .   K11(9,*)   ,K12(9,*)   ,K13(9,*)   ,K14(9,*)   ,K15(9,*)   ,
     .   K16(9,*)   ,K17(9,*)   ,K18(9,*)   ,K19(9,*)   ,K110(9,*)  ,
     .   K111(9,*)  ,K112(9,*)  ,K113(9,*)  ,K114(9,*)  ,K115(9,*)  ,
     .   K116(9,*)  ,K117(9,*)  ,K118(9,*)  ,K119(9,*)  ,K120(9,*)  ,
     .   K22(9,*)   ,K23(9,*)   ,K24(9,*)   ,K25(9,*)   ,K26(9,*)   ,
     .   K27(9,*)   ,K28(9,*)   ,K29(9,*)   ,K210(9,*)  ,K211(9,*)  ,
     .   K212(9,*)  ,K213(9,*)  ,K214(9,*)  ,K215(9,*)  ,K216(9,*)  ,
     .   K217(9,*)  ,K218(9,*)  ,K219(9,*)  ,K220(9,*)  ,K33(9,*)   ,
     .   K34(9,*)   ,K35(9,*)   ,K36(9,*)   ,K37(9,*)   ,K38(9,*)   ,
     .   K39(9,*)   ,K310(9,*)  ,K311(9,*)  ,K312(9,*)  ,K313(9,*)  ,
     .   K314(9,*)  ,K315(9,*)  ,K316(9,*)  ,K317(9,*)  ,K318(9,*)  ,
     .   K319(9,*)  ,K320(9,*)  ,K44(9,*)   ,K45(9,*)   ,K46(9,*)   ,
     .   K47(9,*)   ,K48(9,*)   ,K49(9,*)   ,K410(9,*)  ,K411(9,*)  ,
     .   K412(9,*)  ,K413(9,*)  ,K414(9,*)  ,K415(9,*)  ,K416(9,*)  ,
     .   K417(9,*)  ,K418(9,*)  ,K419(9,*)  ,K420(9,*)  ,K55(9,*)   ,
     .   K56(9,*)   ,K57(9,*)   ,K58(9,*)   ,K59(9,*)   ,K510(9,*)  ,
     .   K511(9,*)  ,K512(9,*)  ,K513(9,*)  ,K514(9,*)  ,K515(9,*)  ,
     .   K516(9,*)  ,K517(9,*)  ,K518(9,*)  ,K519(9,*)  ,K520(9,*)  ,
     .   K66(9,*)   ,K67(9,*)   ,K68(9,*)   ,K69(9,*)   ,K610(9,*)  ,
     .   K611(9,*)  ,K612(9,*)  ,K613(9,*)  ,K614(9,*)  ,K615(9,*)  ,
     .   K616(9,*)  ,K617(9,*)  ,K618(9,*)  ,K619(9,*)  ,K620(9,*)  ,
     .   K77(9,*)   ,K78(9,*)   ,K79(9,*)   ,K710(9,*)  ,K711(9,*)  ,
     .   K712(9,*)  ,K713(9,*)  ,K714(9,*)  ,K715(9,*)  ,K716(9,*)  ,
     .   K717(9,*)  ,K718(9,*)  ,K719(9,*)  ,K720(9,*)  ,K88(9,*)   ,
     .   K89(9,*)   ,K810(9,*)  ,K811(9,*)  ,K812(9,*)  ,K813(9,*)  ,
     .   K814(9,*)  ,K815(9,*)  ,K816(9,*)  ,K817(9,*)  ,K818(9,*)  ,
     .   K819(9,*)  ,K820(9,*)  ,K99(9,*)   ,K910(9,*)  ,K911(9,*)  ,
     .   K912(9,*)  ,K913(9,*)  ,K914(9,*)  ,K915(9,*)  ,K916(9,*)  ,
     .   K917(9,*)  ,K918(9,*)  ,K919(9,*)  ,K920(9,*)  ,K1010(9,*) ,
     .   K1011(9,*) ,K1012(9,*) ,K1013(9,*) ,K1014(9,*) ,K1015(9,*) ,
     .   K1016(9,*) ,K1017(9,*) ,K1018(9,*) ,K1019(9,*) ,K1020(9,*) ,
     .   K1111(9,*) ,K1112(9,*) ,K1113(9,*) ,K1114(9,*) ,K1115(9,*) ,
     .   K1116(9,*) ,K1117(9,*) ,K1118(9,*) ,K1119(9,*) ,K1120(9,*) ,
     .   K1212(9,*) ,K1213(9,*) ,K1214(9,*) ,K1215(9,*) ,K1216(9,*) ,
     .   K1217(9,*) ,K1218(9,*) ,K1219(9,*) ,K1220(9,*) ,K1313(9,*) ,
     .   K1314(9,*) ,K1315(9,*) ,K1316(9,*) ,K1317(9,*) ,K1318(9,*) ,
     .   K1319(9,*) ,K1320(9,*) ,K1414(9,*) ,K1415(9,*) ,K1416(9,*) ,
     .   K1417(9,*) ,K1418(9,*) ,K1419(9,*) ,K1420(9,*) ,K1515(9,*) ,
     .   K1516(9,*) ,K1517(9,*) ,K1518(9,*) ,K1519(9,*) ,K1520(9,*) ,
     .   K1616(9,*) ,K1617(9,*) ,K1618(9,*) ,K1619(9,*) ,K1620(9,*) ,
     .   K1717(9,*) ,K1718(9,*) ,K1719(9,*) ,K1720(9,*) ,K1818(9,*) ,
     .   K1819(9,*) ,K1820(9,*) ,K1919(9,*) ,K1920(9,*) ,K2020(9,*) 
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
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1010 )
      CALL S4KEH3(1,NEL,
     .   PX(1,11),PY(1,11),PZ(1,11),PX(1,11),PY(1,11),PZ(1,11),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1111 )
      CALL S4KEH3(1,NEL,
     .   PX(1,12),PY(1,12),PZ(1,12),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1212 )
      CALL S4KEH3(1,NEL,
     .   PX(1,13),PY(1,13),PZ(1,13),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1313 )
      CALL S4KEH3(1,NEL,
     .   PX(1,14),PY(1,14),PZ(1,14),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1414 )
      CALL S4KEH3(1,NEL,
     .   PX(1,15),PY(1,15),PZ(1,15),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1515 )
      CALL S4KEH3(1,NEL,
     .   PX(1,16),PY(1,16),PZ(1,16),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1616 )
      CALL S4KEH3(1,NEL,
     .   PX(1,17),PY(1,17),PZ(1,17),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1717 )
      CALL S4KEH3(1,NEL,
     .   PX(1,18),PY(1,18),PZ(1,18),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1818 )
      CALL S4KEH3(1,NEL,
     .   PX(1,19),PY(1,19),PZ(1,19),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1919 )
      CALL S4KEH3(1,NEL,
     .   PX(1,20),PY(1,20),PZ(1,20),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K2020 )
     
     
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
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K110 )
      CALL S4KEH3(1,NEL,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,11),PY(1,11),PZ(1,11),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K111 )
      CALL S4KEH3(1,NEL,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K112 )
      CALL S4KEH3(1,NEL,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K113 )
      CALL S4KEH3(1,NEL,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K114 )
      CALL S4KEH3(1,NEL,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K115 )
      CALL S4KEH3(1,NEL,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K116 )
      CALL S4KEH3(1,NEL,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K117 )
      CALL S4KEH3(1,NEL,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K118 )
      CALL S4KEH3(1,NEL,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K119 )
      CALL S4KEH3(1,NEL,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K120 )
     
     
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
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K210 )
      CALL S4KEH3(1,NEL,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,11),PY(1,11),PZ(1,11),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K211 )
      CALL S4KEH3(1,NEL,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K212 )
      CALL S4KEH3(1,NEL,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K213 )
      CALL S4KEH3(1,NEL,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K214 )
      CALL S4KEH3(1,NEL,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K215 )
      CALL S4KEH3(1,NEL,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K216 )
      CALL S4KEH3(1,NEL,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K217 )
      CALL S4KEH3(1,NEL,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K218 )
      CALL S4KEH3(1,NEL,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K219 )
      CALL S4KEH3(1,NEL,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K220 )
     
     
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
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K310 )
      CALL S4KEH3(1,NEL,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,11),PY(1,11),PZ(1,11),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K311 )
      CALL S4KEH3(1,NEL,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K312 )
      CALL S4KEH3(1,NEL,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K313 )
      CALL S4KEH3(1,NEL,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K314 )
      CALL S4KEH3(1,NEL,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K315 )
      CALL S4KEH3(1,NEL,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K316 )
      CALL S4KEH3(1,NEL,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K317 )
      CALL S4KEH3(1,NEL,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K318 )
      CALL S4KEH3(1,NEL,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K319 )
      CALL S4KEH3(1,NEL,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K320 )
     
     
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
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K410 )
      CALL S4KEH3(1,NEL,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,11),PY(1,11),PZ(1,11),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K411 )
      CALL S4KEH3(1,NEL,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K412 )
      CALL S4KEH3(1,NEL,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K413 )
      CALL S4KEH3(1,NEL,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K414 )
      CALL S4KEH3(1,NEL,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K415 )
      CALL S4KEH3(1,NEL,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K416 )
      CALL S4KEH3(1,NEL,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K417 )
      CALL S4KEH3(1,NEL,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K418 )
      CALL S4KEH3(1,NEL,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K419 )
      CALL S4KEH3(1,NEL,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K420 )
     
     
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
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K510 )
      CALL S4KEH3(1,NEL,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,11),PY(1,11),PZ(1,11),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K511 )
      CALL S4KEH3(1,NEL,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K512 )
      CALL S4KEH3(1,NEL,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K513 )
      CALL S4KEH3(1,NEL,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K514 )
      CALL S4KEH3(1,NEL,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K515 )
      CALL S4KEH3(1,NEL,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K516 )
      CALL S4KEH3(1,NEL,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K517 )
      CALL S4KEH3(1,NEL,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K518 )
      CALL S4KEH3(1,NEL,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K519 )
      CALL S4KEH3(1,NEL,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K520 )
     
     
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
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K610 )
      CALL S4KEH3(1,NEL,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,11),PY(1,11),PZ(1,11),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K611 )
      CALL S4KEH3(1,NEL,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K612 )
      CALL S4KEH3(1,NEL,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K613 )
      CALL S4KEH3(1,NEL,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K614 )
      CALL S4KEH3(1,NEL,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K615 )
      CALL S4KEH3(1,NEL,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K616 )
      CALL S4KEH3(1,NEL,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K617 )
      CALL S4KEH3(1,NEL,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K618 )
      CALL S4KEH3(1,NEL,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K619 )
      CALL S4KEH3(1,NEL,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K620 )
     
     
      CALL S4KEH3(1,NEL,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K78 )
      CALL S4KEH3(1,NEL,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K79 )
      CALL S4KEH3(1,NEL,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K710 )
      CALL S4KEH3(1,NEL,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,11),PY(1,11),PZ(1,11),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K711 )
      CALL S4KEH3(1,NEL,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K712 )
      CALL S4KEH3(1,NEL,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K713 )
      CALL S4KEH3(1,NEL,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K714 )
      CALL S4KEH3(1,NEL,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K715 )
      CALL S4KEH3(1,NEL,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K716 )
      CALL S4KEH3(1,NEL,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K717 )
      CALL S4KEH3(1,NEL,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K718 )
      CALL S4KEH3(1,NEL,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K719 )
      CALL S4KEH3(1,NEL,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K720 )
     
     
      CALL S4KEH3(1,NEL,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K89 )
      CALL S4KEH3(1,NEL,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K810 )
      CALL S4KEH3(1,NEL,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,11),PY(1,11),PZ(1,11),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K811 )
      CALL S4KEH3(1,NEL,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K812 )
      CALL S4KEH3(1,NEL,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K813 )
      CALL S4KEH3(1,NEL,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K814 )
      CALL S4KEH3(1,NEL,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K815 )
      CALL S4KEH3(1,NEL,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K816 )
      CALL S4KEH3(1,NEL,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K817 )
      CALL S4KEH3(1,NEL,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K818 )
      CALL S4KEH3(1,NEL,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K819 )
      CALL S4KEH3(1,NEL,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K820 )
     
     
      CALL S4KEH3(1,NEL,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K910 )
      CALL S4KEH3(1,NEL,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,11),PY(1,11),PZ(1,11),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K911 )
      CALL S4KEH3(1,NEL,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K912 )
      CALL S4KEH3(1,NEL,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K913 )
      CALL S4KEH3(1,NEL,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K914 )
      CALL S4KEH3(1,NEL,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K915 )
      CALL S4KEH3(1,NEL,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K916 )
      CALL S4KEH3(1,NEL,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K917 )
      CALL S4KEH3(1,NEL,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K918 )
      CALL S4KEH3(1,NEL,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K919 )
      CALL S4KEH3(1,NEL,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K920 )
     
     
           CALL S4KEH3(1,NEL,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,11),PY(1,11),PZ(1,11),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1011 )
      CALL S4KEH3(1,NEL,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1012 )
      CALL S4KEH3(1,NEL,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1013 )
      CALL S4KEH3(1,NEL,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1014 )
      CALL S4KEH3(1,NEL,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1015 )
      CALL S4KEH3(1,NEL,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1016 )
      CALL S4KEH3(1,NEL,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1017 )
      CALL S4KEH3(1,NEL,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1018 )
      CALL S4KEH3(1,NEL,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1019 )
      CALL S4KEH3(1,NEL,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1020 )


      CALL S4KEH3(1,NEL,
     .   PX(1,11),PY(1,11),PZ(1,11),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1112 )
      CALL S4KEH3(1,NEL,
     .   PX(1,11),PY(1,11),PZ(1,11),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1113 )
      CALL S4KEH3(1,NEL,
     .   PX(1,11),PY(1,11),PZ(1,11),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1114 )
      CALL S4KEH3(1,NEL,
     .   PX(1,11),PY(1,11),PZ(1,11),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1115 )
      CALL S4KEH3(1,NEL,
     .   PX(1,11),PY(1,11),PZ(1,11),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1116 )
      CALL S4KEH3(1,NEL,
     .   PX(1,11),PY(1,11),PZ(1,11),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1117 )
      CALL S4KEH3(1,NEL,
     .   PX(1,11),PY(1,11),PZ(1,11),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1118 )
      CALL S4KEH3(1,NEL,
     .   PX(1,11),PY(1,11),PZ(1,11),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1119 )
      CALL S4KEH3(1,NEL,
     .   PX(1,11),PY(1,11),PZ(1,11),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1120 )


      CALL S4KEH3(1,NEL,
     .   PX(1,12),PY(1,12),PZ(1,12),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1213 )
      CALL S4KEH3(1,NEL,
     .   PX(1,12),PY(1,12),PZ(1,12),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1214 )
      CALL S4KEH3(1,NEL,
     .   PX(1,12),PY(1,12),PZ(1,12),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1215 )
      CALL S4KEH3(1,NEL,
     .   PX(1,12),PY(1,12),PZ(1,12),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1216 )
      CALL S4KEH3(1,NEL,
     .   PX(1,12),PY(1,12),PZ(1,12),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1217 )
      CALL S4KEH3(1,NEL,
     .   PX(1,12),PY(1,12),PZ(1,12),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1218 )
      CALL S4KEH3(1,NEL,
     .   PX(1,12),PY(1,12),PZ(1,12),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1219 )
      CALL S4KEH3(1,NEL,
     .   PX(1,12),PY(1,12),PZ(1,12),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1220 )


      CALL S4KEH3(1,NEL,
     .   PX(1,13),PY(1,13),PZ(1,13),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1314 )
      CALL S4KEH3(1,NEL,
     .   PX(1,13),PY(1,13),PZ(1,13),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1315 )
      CALL S4KEH3(1,NEL,
     .   PX(1,13),PY(1,13),PZ(1,13),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1316 )
      CALL S4KEH3(1,NEL,
     .   PX(1,13),PY(1,13),PZ(1,13),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1317 )
      CALL S4KEH3(1,NEL,
     .   PX(1,13),PY(1,13),PZ(1,13),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1318 )
      CALL S4KEH3(1,NEL,
     .   PX(1,13),PY(1,13),PZ(1,13),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1319 )
      CALL S4KEH3(1,NEL,
     .   PX(1,13),PY(1,13),PZ(1,13),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1320 )



      CALL S4KEH3(1,NEL,
     .   PX(1,14),PY(1,14),PZ(1,14),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1415 )
      CALL S4KEH3(1,NEL,
     .   PX(1,14),PY(1,14),PZ(1,14),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1416 )
      CALL S4KEH3(1,NEL,
     .   PX(1,14),PY(1,14),PZ(1,14),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1417 )
      CALL S4KEH3(1,NEL,
     .   PX(1,14),PY(1,14),PZ(1,14),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1418 )
      CALL S4KEH3(1,NEL,
     .   PX(1,14),PY(1,14),PZ(1,14),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1419 )
      CALL S4KEH3(1,NEL,
     .   PX(1,14),PY(1,14),PZ(1,14),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1420 )
     

      CALL S4KEH3(1,NEL,
     .   PX(1,15),PY(1,15),PZ(1,15),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1516 )
      CALL S4KEH3(1,NEL,
     .   PX(1,15),PY(1,15),PZ(1,15),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1517 )
      CALL S4KEH3(1,NEL,
     .   PX(1,15),PY(1,15),PZ(1,15),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1518 )
      CALL S4KEH3(1,NEL,
     .   PX(1,15),PY(1,15),PZ(1,15),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1519 )
      CALL S4KEH3(1,NEL,
     .   PX(1,15),PY(1,15),PZ(1,15),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1520 )



      CALL S4KEH3(1,NEL,
     .   PX(1,16),PY(1,16),PZ(1,16),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1617 )
      CALL S4KEH3(1,NEL,
     .   PX(1,16),PY(1,16),PZ(1,16),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1618 )
      CALL S4KEH3(1,NEL,
     .   PX(1,16),PY(1,16),PZ(1,16),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1619 )
      CALL S4KEH3(1,NEL,
     .   PX(1,16),PY(1,16),PZ(1,16),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1620 )
     
      CALL S4KEH3(1,NEL,
     .   PX(1,17),PY(1,17),PZ(1,17),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1718 )
      CALL S4KEH3(1,NEL,
     .   PX(1,17),PY(1,17),PZ(1,17),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1719 )
      CALL S4KEH3(1,NEL,
     .   PX(1,17),PY(1,17),PZ(1,17),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1720 )

     
      CALL S4KEH3(1,NEL,
     .   PX(1,18),PY(1,18),PZ(1,18),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1819 )
      CALL S4KEH3(1,NEL,
     .   PX(1,18),PY(1,18),PZ(1,18),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1820 )
     

      CALL S4KEH3(1,NEL,
     .   PX(1,19),PY(1,19),PZ(1,19),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1920 )
                    
C
      RETURN
      END
