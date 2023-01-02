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
Chd|  S20CUMG3                      source/elements/solid/solide20/s20cumg3.F
Chd|-- called by -----------
Chd|        S20KE3                        source/elements/solid/solide20/s20ke3.F
Chd|-- calls ---------------
Chd|        S8ZKEBG1                      source/elements/solid/solide8z/s8zlke3.F
Chd|        S8ZKED3                       source/elements/solid/solide8z/s8zlke3.F
Chd|        S8ZKEG3                       source/elements/solid/solide8z/s8zlke3.F
Chd|        S8ZKEG33                      source/elements/solid/solide8z/s8zlke3.F
Chd|====================================================================
      SUBROUTINE S20CUMG3(
     .   PX, PY, PZ, DD ,GG ,DG ,G33,IKSUP, 
     .   K11  ,K12  ,K13  ,K14  ,K15  ,K16  ,K17  ,K18  ,K19  ,K110 ,
     .   K111 ,K112 ,K113 ,K114 ,K115 ,K116 ,K117 ,K118 ,K119 ,K120 ,
     .   K22  ,K23  ,K24  ,K25  ,K26  ,K27  ,K28  ,K29  ,K210 ,K211 ,
     .   K212 ,K213 ,K214 ,K215 ,K216 ,K217 ,K218 ,K219 ,K220 ,K33  ,
     .   K34  ,K35  ,K36  ,K37  ,K38  ,K39  ,K310 ,K311 ,K312 ,K313 ,
     .   K314 ,K315 ,K316 ,K317 ,K318 ,K319 ,K320 ,K44  ,K45  ,K46  ,
     .   K47  ,K48  ,K49  ,K410 ,K411 ,K412 ,K413 ,K414 ,K415 ,K416 ,
     .   K417 ,K418 ,K419 ,K420 ,K55  ,K56  ,K57  ,K58  ,K59  ,K510 ,
     .   K511 ,K512 ,K513 ,K514 ,K515 ,K516 ,K517 ,K518 ,K519 ,K520 ,
     .   K66  ,K67  ,K68  ,K69  ,K610 ,K611 ,K612 ,K613 ,K614 ,K615 ,
     .   K616 ,K617 ,K618 ,K619 ,K620 ,K77  ,K78  ,K79  ,K710 ,K711 ,
     .   K712 ,K713 ,K714 ,K715 ,K716 ,K717 ,K718 ,K719 ,K720 ,K88  ,
     .   K89  ,K810 ,K811 ,K812 ,K813 ,K814 ,K815 ,K816 ,K817 ,K818 ,
     .   K819 ,K820 ,K99  ,K910 ,K911 ,K912 ,K913 ,K914 ,K915 ,K916 ,
     .   K917 ,K918 ,K919 ,K920 ,K1010,K1011,K1012,K1013,K1014,K1015,
     .   K1016,K1017,K1018,K1019,K1020,K1111,K1112,K1113,K1114,K1115,
     .   K1116,K1117,K1118,K1119,K1120,K1212,K1213,K1214,K1215,K1216,
     .   K1217,K1218,K1219,K1220,K1313,K1314,K1315,K1316,K1317,K1318,
     .   K1319,K1320,K1414,K1415,K1416,K1417,K1418,K1419,K1420,K1515,
     .   K1516,K1517,K1518,K1519,K1520,K1616,K1617,K1618,K1619,K1620,
     .   K1717,K1718,K1719,K1720,K1818,K1819,K1820,K1919,K1920,K2020)
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IKSUP
C     REAL
      my_real
     .  PX(MVSIZ,20),PY(MVSIZ,20),PZ(MVSIZ,20),
     .   DD(3,3,*),GG(*),DG(9,*)  ,G33(9,*)
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,IS
C     REAL
C-----------------------------------------------
C  -------symm(diag)------
      IS =1
C                                                                     12
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,1),PY(1,1),PZ(1,1),
     .             DD, K11,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,2),PY(1,2),PZ(1,2),PX(1,2),PY(1,2),PZ(1,2),
     .             DD, K22,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,3),PY(1,3),PZ(1,3),
     .             DD, K33,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,4),PY(1,4),PZ(1,4),
     .             DD, K44,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,5),PY(1,5),PZ(1,5),
     .             DD, K55,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,6),PY(1,6),PZ(1,6),
     .             DD, K66,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,7),PY(1,7),PZ(1,7),
     .             DD, K77,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,8),PY(1,8),PZ(1,8),
     .             DD, K88,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,9),PY(1,9),PZ(1,9),
     .             DD, K99,IS)
       CALL S8ZKED3(LFT,LLT,
     .       PX(1,10),PY(1,10),PZ(1,10),PX(1,10),PY(1,10),PZ(1,10),
     .       DD, K1010,IS)
       CALL S8ZKED3(LFT,LLT,
     .       PX(1,11),PY(1,11),PZ(1,11),PX(1,11),PY(1,11),PZ(1,11),
     .       DD, K1111,IS)
       CALL S8ZKED3(LFT,LLT,
     .       PX(1,12),PY(1,12),PZ(1,12),PX(1,12),PY(1,12),PZ(1,12),
     .       DD, K1212,IS)
       CALL S8ZKED3(LFT,LLT,
     .       PX(1,13),PY(1,13),PZ(1,13),PX(1,13),PY(1,13),PZ(1,13),
     .       DD, K1313,IS)
       CALL S8ZKED3(LFT,LLT,
     .       PX(1,14),PY(1,14),PZ(1,14),PX(1,14),PY(1,14),PZ(1,14),
     .       DD, K1414,IS)
       CALL S8ZKED3(LFT,LLT,
     .       PX(1,15),PY(1,15),PZ(1,15),PX(1,15),PY(1,15),PZ(1,15),
     .       DD, K1515,IS)          
       CALL S8ZKED3(LFT,LLT,
     .       PX(1,16),PY(1,16),PZ(1,16),PX(1,16),PY(1,16),PZ(1,16),
     .       DD, K1616,IS)
            CALL S8ZKED3(LFT,LLT,
     .       PX(1,17),PY(1,17),PZ(1,17),PX(1,17),PY(1,17),PZ(1,17),
     .       DD, K1717,IS)
            CALL S8ZKED3(LFT,LLT,
     .       PX(1,18),PY(1,18),PZ(1,18),PX(1,18),PY(1,18),PZ(1,18),
     .       DD, K1818,IS)
            CALL S8ZKED3(LFT,LLT,
     .       PX(1,19),PY(1,19),PZ(1,19),PX(1,19),PY(1,19),PZ(1,19),
     .       DD, K1919,IS)
            CALL S8ZKED3(LFT,LLT,
     .       PX(1,20),PY(1,20),PZ(1,20),PX(1,20),PY(1,20),PZ(1,20),
     .       DD, K2020,IS)
C
      IF (IKSUP==0) THEN
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             GG,K11,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             GG,K22,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             GG,K33,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             GG,K44,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             GG,K55,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             GG,K66,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             GG,K77,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             GG,K88,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             GG,K99,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .      PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .      PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .      GG,K1010,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .      PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .      PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .      GG,K1111,IS)
       CALL S8ZKEG3(LFT,LLT,
     .      PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .      PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .      GG,K1212,IS)
       CALL S8ZKEG3(LFT,LLT,
     .      PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .      PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .      GG,K1313,IS)
       CALL S8ZKEG3(LFT,LLT,
     .      PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .      PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .      GG,K1414,IS)
       CALL S8ZKEG3(LFT,LLT,
     .      PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .      PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .      GG,K1515,IS)
       CALL S8ZKEG3(LFT,LLT,
     .      PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .      PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .      GG,K1616,IS)
       CALL S8ZKEG3(LFT,LLT,
     .      PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .      PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .      GG,K1717,IS)
       CALL S8ZKEG3(LFT,LLT,
     .      PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .      PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .      GG,K1818,IS)
       CALL S8ZKEG3(LFT,LLT,
     .      PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .      PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .      GG,K1919,IS)
       CALL S8ZKEG3(LFT,LLT,
     .      PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .      PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .      GG,K2020,IS)
     
      ENDIF 
C  -------non symm------
      IS =0
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,2),PY(1,2),PZ(1,2),
     .              DD, K12,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,3),PY(1,3),PZ(1,3),
     .              DD, K13,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,4),PY(1,4),PZ(1,4),
     .              DD, K14,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,5),PY(1,5),PZ(1,5),
     .              DD, K15,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,6),PY(1,6),PZ(1,6),
     .              DD, K16,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,7),PY(1,7),PZ(1,7),
     .              DD, K17,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,8),PY(1,8),PZ(1,8),
     .              DD, K18,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,9),PY(1,9),PZ(1,9),
     .              DD, K19,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,10),PY(1,10),PZ(1,10),
     .              DD, K110,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,11),PY(1,11),PZ(1,11),
     .              DD, K111,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,12),PY(1,12),PZ(1,12),
     .              DD, K112,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,13),PY(1,13),PZ(1,13),
     .              DD, K113,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,14),PY(1,14),PZ(1,14),
     .              DD, K114,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,15),PY(1,15),PZ(1,15),
     .              DD, K115,IS)     
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,16),PY(1,16),PZ(1,16),
     .              DD, K116,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,17),PY(1,17),PZ(1,17),
     .              DD, K117,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,18),PY(1,18),PZ(1,18),
     .              DD, K118,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,19),PY(1,19),PZ(1,19),
     .              DD, K119,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,20),PY(1,20),PZ(1,20),
     .              DD, K120,IS)
     
     
     
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,3),PY(1,3),PZ(1,3),
     .              DD, K23,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,4),PY(1,4),PZ(1,4),
     .              DD, K24,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,5),PY(1,5),PZ(1,5),
     .              DD, K25,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,6),PY(1,6),PZ(1,6),
     .              DD, K26,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,7),PY(1,7),PZ(1,7),
     .              DD, K27,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,8),PY(1,8),PZ(1,8),
     .              DD, K28,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,9),PY(1,9),PZ(1,9),
     .              DD, K29,IS)
       CALL S8ZKED3(LFT,LLT,
     .          PX(1,2),PY(1,2),PZ(1,2),PX(1,10),PY(1,10),PZ(1,10),
     .          DD, K210,IS)
       CALL S8ZKED3(LFT,LLT,
     .          PX(1,2),PY(1,2),PZ(1,2),PX(1,11),PY(1,11),PZ(1,11),
     .          DD, K211,IS)
       CALL S8ZKED3(LFT,LLT,
     .          PX(1,2),PY(1,2),PZ(1,2),PX(1,12),PY(1,12),PZ(1,12),
     .          DD, K212,IS)
       CALL S8ZKED3(LFT,LLT,
     .          PX(1,2),PY(1,2),PZ(1,2),PX(1,13),PY(1,13),PZ(1,13),
     .          DD, K213,IS)
       CALL S8ZKED3(LFT,LLT,
     .          PX(1,2),PY(1,2),PZ(1,2),PX(1,14),PY(1,14),PZ(1,14),
     .          DD, K214,IS)
       CALL S8ZKED3(LFT,LLT,
     .          PX(1,2),PY(1,2),PZ(1,2),PX(1,15),PY(1,15),PZ(1,15),
     .          DD, K215,IS)
       CALL S8ZKED3(LFT,LLT,
     .          PX(1,2),PY(1,2),PZ(1,2),PX(1,16),PY(1,16),PZ(1,16),
     .          DD, K216,IS)
       CALL S8ZKED3(LFT,LLT,
     .          PX(1,2),PY(1,2),PZ(1,2),PX(1,17),PY(1,17),PZ(1,17),
     .          DD, K217,IS)
       CALL S8ZKED3(LFT,LLT,
     .          PX(1,2),PY(1,2),PZ(1,2),PX(1,18),PY(1,18),PZ(1,18),
     .          DD, K218,IS)
       CALL S8ZKED3(LFT,LLT,
     .          PX(1,2),PY(1,2),PZ(1,2),PX(1,19),PY(1,19),PZ(1,19),
     .          DD, K219,IS)
       CALL S8ZKED3(LFT,LLT,
     .          PX(1,2),PY(1,2),PZ(1,2),PX(1,20),PY(1,20),PZ(1,20),
     .          DD, K220,IS)
     
     
     
      CALL S8ZKED3(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,4),PY(1,4),PZ(1,4),
     .              DD, K34,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,5),PY(1,5),PZ(1,5),
     .              DD, K35,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,6),PY(1,6),PZ(1,6),
     .              DD, K36,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,7),PY(1,7),PZ(1,7),
     .              DD, K37,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,8),PY(1,8),PZ(1,8),
     .              DD, K38,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,9),PY(1,9),PZ(1,9),
     .              DD, K39,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,10),PY(1,10),PZ(1,10),
     .              DD, K310,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,11),PY(1,11),PZ(1,11),
     .              DD, K311,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,12),PY(1,12),PZ(1,12),
     .              DD, K312,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,13),PY(1,13),PZ(1,13),
     .              DD, K313,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,14),PY(1,14),PZ(1,14),
     .              DD, K314,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,15),PY(1,15),PZ(1,15),
     .              DD, K315,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,16),PY(1,16),PZ(1,16),
     .              DD, K316,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,17),PY(1,17),PZ(1,17),
     .              DD, K317,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,18),PY(1,18),PZ(1,18),
     .              DD, K318,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,19),PY(1,19),PZ(1,19),
     .              DD, K319,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,20),PY(1,20),PZ(1,20),
     .              DD, K320,IS)
     
     
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,4),PY(1,4),PZ(1,4),PX(1,5),PY(1,5),PZ(1,5),
     .              DD, K45,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,4),PY(1,4),PZ(1,4),PX(1,6),PY(1,6),PZ(1,6),
     .              DD, K46,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,4),PY(1,4),PZ(1,4),PX(1,7),PY(1,7),PZ(1,7),
     .              DD, K47,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,4),PY(1,4),PZ(1,4),PX(1,8),PY(1,8),PZ(1,8),
     .              DD, K48,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,4),PY(1,4),PZ(1,4),PX(1,9),PY(1,9),PZ(1,9),
     .              DD, K49,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,10),PY(1,10),PZ(1,10),
     .              DD, K410,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,11),PY(1,11),PZ(1,11),
     .              DD, K411,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,12),PY(1,12),PZ(1,12),
     .              DD, K412,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,13),PY(1,13),PZ(1,13),
     .              DD, K413,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,14),PY(1,14),PZ(1,14),
     .              DD, K414,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,15),PY(1,15),PZ(1,15),
     .              DD, K415,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,16),PY(1,16),PZ(1,16),
     .              DD, K416,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,17),PY(1,17),PZ(1,17),
     .              DD, K417,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,18),PY(1,18),PZ(1,18),
     .              DD, K418,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,19),PY(1,19),PZ(1,19),
     .              DD, K419,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,20),PY(1,20),PZ(1,20),
     .              DD, K420,IS)
     
     
     
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,5),PY(1,5),PZ(1,5),PX(1,6),PY(1,6),PZ(1,6),
     .              DD, K56,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,5),PY(1,5),PZ(1,5),PX(1,7),PY(1,7),PZ(1,7),
     .              DD, K57,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,5),PY(1,5),PZ(1,5),PX(1,8),PY(1,8),PZ(1,8),
     .              DD, K58,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,5),PY(1,5),PZ(1,5),PX(1,9),PY(1,9),PZ(1,9),
     .              DD, K59,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,10),PY(1,10),PZ(1,10),
     .              DD, K510,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,11),PY(1,11),PZ(1,11),
     .              DD, K511,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,12),PY(1,12),PZ(1,12),
     .              DD, K512,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,13),PY(1,13),PZ(1,13),
     .              DD, K513,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,14),PY(1,14),PZ(1,14),
     .              DD, K514,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,15),PY(1,15),PZ(1,15),
     .              DD, K515,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,16),PY(1,16),PZ(1,16),
     .              DD, K516,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,17),PY(1,17),PZ(1,17),
     .              DD, K517,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,18),PY(1,18),PZ(1,18),
     .              DD, K518,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,19),PY(1,19),PZ(1,19),
     .              DD, K519,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,20),PY(1,20),PZ(1,20),
     .              DD, K520,IS)
     
     
     
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,6),PY(1,6),PZ(1,6),PX(1,7),PY(1,7),PZ(1,7),
     .              DD, K67,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,6),PY(1,6),PZ(1,6),PX(1,8),PY(1,8),PZ(1,8),
     .              DD, K68,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,6),PY(1,6),PZ(1,6),PX(1,9),PY(1,9),PZ(1,9),
     .              DD, K69,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,10),PY(1,10),PZ(1,10),
     .              DD, K610,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,11),PY(1,11),PZ(1,11),
     .              DD, K611,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,12),PY(1,12),PZ(1,12),
     .              DD, K612,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,13),PY(1,13),PZ(1,13),
     .              DD, K613,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,14),PY(1,14),PZ(1,14),
     .              DD, K614,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,15),PY(1,15),PZ(1,15),
     .              DD, K615,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,16),PY(1,16),PZ(1,16),
     .              DD, K616,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,17),PY(1,17),PZ(1,17),
     .              DD, K617,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,18),PY(1,18),PZ(1,18),
     .              DD, K618,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,19),PY(1,19),PZ(1,19),
     .              DD, K619,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,20),PY(1,20),PZ(1,20),
     .              DD, K620,IS)
     
     
     
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,7),PY(1,7),PZ(1,7),PX(1,8),PY(1,8),PZ(1,8),
     .              DD, K78,IS)
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,7),PY(1,7),PZ(1,7),PX(1,9),PY(1,9),PZ(1,9),
     .              DD, K79,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,10),PY(1,10),PZ(1,10),
     .              DD, K710,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,11),PY(1,11),PZ(1,11),
     .              DD, K711,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,12),PY(1,12),PZ(1,12),
     .              DD, K712,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,13),PY(1,13),PZ(1,13),
     .              DD, K713,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,14),PY(1,14),PZ(1,14),
     .              DD, K714,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,15),PY(1,15),PZ(1,15),
     .              DD, K715,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,16),PY(1,16),PZ(1,16),
     .              DD, K716,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,17),PY(1,17),PZ(1,17),
     .              DD, K717,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,18),PY(1,18),PZ(1,18),
     .              DD, K718,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,19),PY(1,19),PZ(1,19),
     .              DD, K719,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,20),PY(1,20),PZ(1,20),
     .              DD, K720,IS)
     
     
       CALL S8ZKED3(LFT,LLT,
     .              PX(1,8),PY(1,8),PZ(1,8),PX(1,9),PY(1,9),PZ(1,9),
     .              DD, K89,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,10),PY(1,10),PZ(1,10),
     .              DD, K810,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,11),PY(1,11),PZ(1,11),
     .              DD, K811,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,12),PY(1,12),PZ(1,12),
     .              DD, K812,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,13),PY(1,13),PZ(1,13),
     .              DD, K813,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,14),PY(1,14),PZ(1,14),
     .              DD, K814,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,15),PY(1,15),PZ(1,15),
     .              DD, K815,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,16),PY(1,16),PZ(1,16),
     .              DD, K816,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,17),PY(1,17),PZ(1,17),
     .              DD, K817,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,18),PY(1,18),PZ(1,18),
     .              DD, K818,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,19),PY(1,19),PZ(1,19),
     .              DD, K819,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,20),PY(1,20),PZ(1,20),
     .              DD, K820,IS)

     
     
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,10),PY(1,10),PZ(1,10),
     .              DD, K910,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,11),PY(1,11),PZ(1,11),
     .              DD, K911,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,12),PY(1,12),PZ(1,12),
     .              DD, K912,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,13),PY(1,13),PZ(1,13),
     .              DD, K913,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,14),PY(1,14),PZ(1,14),
     .              DD, K914,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,15),PY(1,15),PZ(1,15),
     .              DD, K915,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,16),PY(1,16),PZ(1,16),
     .              DD, K916,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,17),PY(1,17),PZ(1,17),
     .              DD, K917,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,18),PY(1,18),PZ(1,18),
     .              DD, K918,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,19),PY(1,19),PZ(1,19),
     .              DD, K919,IS)
       CALL S8ZKED3(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,20),PY(1,20),PZ(1,20),
     .              DD, K920,IS)
     
     
     
       CALL S8ZKED3(LFT,LLT,
     .          PX(1,10),PY(1,10),PZ(1,10),PX(1,11),PY(1,11),PZ(1,11),
     .          DD, K1011,IS)
       CALL S8ZKED3(LFT,LLT,
     .          PX(1,10),PY(1,10),PZ(1,10),PX(1,12),PY(1,12),PZ(1,12),
     .          DD, K1012,IS)
       CALL S8ZKED3(LFT,LLT,
     .          PX(1,10),PY(1,10),PZ(1,10),PX(1,13),PY(1,13),PZ(1,13),
     .          DD, K1013,IS)
       CALL S8ZKED3(LFT,LLT,
     .          PX(1,10),PY(1,10),PZ(1,10),PX(1,14),PY(1,14),PZ(1,14),
     .          DD, K1014,IS)
       CALL S8ZKED3(LFT,LLT,
     .          PX(1,10),PY(1,10),PZ(1,10),PX(1,15),PY(1,15),PZ(1,15),
     .          DD, K1015,IS)
       CALL S8ZKED3(LFT,LLT,
     .          PX(1,10),PY(1,10),PZ(1,10),PX(1,16),PY(1,16),PZ(1,16),
     .          DD, K1016,IS)
       CALL S8ZKED3(LFT,LLT,
     .          PX(1,10),PY(1,10),PZ(1,10),PX(1,17),PY(1,17),PZ(1,17),
     .          DD, K1017,IS)
       CALL S8ZKED3(LFT,LLT,
     .          PX(1,10),PY(1,10),PZ(1,10),PX(1,18),PY(1,18),PZ(1,18),
     .          DD, K1018,IS)
       CALL S8ZKED3(LFT,LLT,
     .          PX(1,10),PY(1,10),PZ(1,10),PX(1,19),PY(1,19),PZ(1,19),
     .          DD, K1019,IS)
       CALL S8ZKED3(LFT,LLT,
     .          PX(1,10),PY(1,10),PZ(1,10),PX(1,20),PY(1,20),PZ(1,20),
     .          DD, K1020,IS)
     
     
     
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,11),PY(1,11),PZ(1,11),PX(1,12),PY(1,12),PZ(1,12),
     .         DD, K1112,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,11),PY(1,11),PZ(1,11),PX(1,13),PY(1,13),PZ(1,13),
     .         DD, K1113,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,11),PY(1,11),PZ(1,11),PX(1,14),PY(1,14),PZ(1,14),
     .         DD, K1114,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,11),PY(1,11),PZ(1,11),PX(1,15),PY(1,15),PZ(1,15),
     .         DD, K1115,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,11),PY(1,11),PZ(1,11),PX(1,16),PY(1,16),PZ(1,16),
     .         DD, K1116,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,11),PY(1,11),PZ(1,11),PX(1,17),PY(1,17),PZ(1,17),
     .         DD, K1117,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,11),PY(1,11),PZ(1,11),PX(1,18),PY(1,18),PZ(1,18),
     .         DD, K1118,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,11),PY(1,11),PZ(1,11),PX(1,19),PY(1,19),PZ(1,19),
     .         DD, K1119,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,11),PY(1,11),PZ(1,11),PX(1,20),PY(1,20),PZ(1,20),
     .         DD, K1120,IS)
     

       CALL S8ZKED3(LFT,LLT,
     .         PX(1,12),PY(1,12),PZ(1,12),PX(1,13),PY(1,13),PZ(1,13),
     .         DD, K1213,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,12),PY(1,12),PZ(1,12),PX(1,14),PY(1,14),PZ(1,14),
     .         DD, K1214,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,12),PY(1,12),PZ(1,12),PX(1,15),PY(1,15),PZ(1,15),
     .         DD, K1215,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,12),PY(1,12),PZ(1,12),PX(1,16),PY(1,16),PZ(1,16),
     .         DD, K1216,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,12),PY(1,12),PZ(1,12),PX(1,17),PY(1,17),PZ(1,17),
     .         DD, K1217,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,12),PY(1,12),PZ(1,12),PX(1,18),PY(1,18),PZ(1,18),
     .         DD, K1218,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,12),PY(1,12),PZ(1,12),PX(1,19),PY(1,19),PZ(1,19),
     .         DD, K1219,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,12),PY(1,12),PZ(1,12),PX(1,20),PY(1,20),PZ(1,20),
     .         DD, K1220,IS)
     
     
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,13),PY(1,13),PZ(1,13),PX(1,14),PY(1,14),PZ(1,14),
     .         DD, K1314,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,13),PY(1,13),PZ(1,13),PX(1,15),PY(1,15),PZ(1,15),
     .         DD, K1315,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,13),PY(1,13),PZ(1,13),PX(1,16),PY(1,16),PZ(1,16),
     .         DD, K1316,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,13),PY(1,13),PZ(1,13),PX(1,17),PY(1,17),PZ(1,17),
     .         DD, K1317,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,13),PY(1,13),PZ(1,13),PX(1,18),PY(1,18),PZ(1,18),
     .         DD, K1318,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,13),PY(1,13),PZ(1,13),PX(1,19),PY(1,19),PZ(1,19),
     .         DD, K1319,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,13),PY(1,13),PZ(1,13),PX(1,20),PY(1,20),PZ(1,20),
     .         DD, K1320,IS)



       CALL S8ZKED3(LFT,LLT,
     .         PX(1,14),PY(1,14),PZ(1,14),PX(1,15),PY(1,15),PZ(1,15),
     .         DD, K1415,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,14),PY(1,14),PZ(1,14),PX(1,16),PY(1,16),PZ(1,16),
     .         DD, K1416,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,14),PY(1,14),PZ(1,14),PX(1,17),PY(1,17),PZ(1,17),
     .         DD, K1417,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,14),PY(1,14),PZ(1,14),PX(1,18),PY(1,18),PZ(1,18),
     .         DD, K1418,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,14),PY(1,14),PZ(1,14),PX(1,19),PY(1,19),PZ(1,19),
     .         DD, K1419,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,14),PY(1,14),PZ(1,14),PX(1,20),PY(1,20),PZ(1,20),
     .         DD, K1420,IS)



       CALL S8ZKED3(LFT,LLT,
     .         PX(1,15),PY(1,15),PZ(1,15),PX(1,16),PY(1,16),PZ(1,16),
     .         DD, K1516,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,15),PY(1,15),PZ(1,15),PX(1,17),PY(1,17),PZ(1,17),
     .         DD, K1517,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,15),PY(1,15),PZ(1,15),PX(1,18),PY(1,18),PZ(1,18),
     .         DD, K1518,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,15),PY(1,15),PZ(1,15),PX(1,19),PY(1,19),PZ(1,19),
     .         DD, K1519,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,15),PY(1,15),PZ(1,15),PX(1,20),PY(1,20),PZ(1,20),
     .         DD, K1520,IS)
     


       CALL S8ZKED3(LFT,LLT,
     .         PX(1,16),PY(1,16),PZ(1,16),PX(1,17),PY(1,17),PZ(1,17),
     .         DD, K1617,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,16),PY(1,16),PZ(1,16),PX(1,18),PY(1,18),PZ(1,18),
     .         DD, K1618,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,16),PY(1,16),PZ(1,16),PX(1,19),PY(1,19),PZ(1,19),
     .         DD, K1619,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,16),PY(1,16),PZ(1,16),PX(1,20),PY(1,20),PZ(1,20),
     .         DD, K1620,IS)



       CALL S8ZKED3(LFT,LLT,
     .         PX(1,17),PY(1,17),PZ(1,17),PX(1,18),PY(1,18),PZ(1,18),
     .         DD, K1718,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,17),PY(1,17),PZ(1,17),PX(1,19),PY(1,19),PZ(1,19),
     .         DD, K1719,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,17),PY(1,17),PZ(1,17),PX(1,20),PY(1,20),PZ(1,20),
     .         DD, K1720,IS)
     


       CALL S8ZKED3(LFT,LLT,
     .         PX(1,18),PY(1,18),PZ(1,18),PX(1,19),PY(1,19),PZ(1,19),
     .         DD, K1819,IS)
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,18),PY(1,18),PZ(1,18),PX(1,20),PY(1,20),PZ(1,20),
     .         DD, K1820,IS)

          
       CALL S8ZKED3(LFT,LLT,
     .         PX(1,19),PY(1,19),PZ(1,19),PX(1,20),PY(1,20),PZ(1,20),
     .         DD, K1920,IS)


     
C
      IF (IKSUP==0) THEN
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             GG,K12,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             GG,K13,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             GG,K14,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             GG,K15,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             GG,K16,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             GG,K17,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             GG,K18,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             GG,K19,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          GG,K110,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          GG,K111,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          GG,K112,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          GG,K113,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          GG,K114,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          GG,K115,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          GG,K116,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          GG,K117,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          GG,K118,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          GG,K119,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          GG,K120,IS)     
     
     
     
     
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             GG,K23,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             GG,K24,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             GG,K25,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             GG,K26,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             GG,K27,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             GG,K28,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             GG,K29,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          GG,K210,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          GG,K211,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          GG,K212,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          GG,K213,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          GG,K214,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          GG,K215,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          GG,K216,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          GG,K217,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          GG,K218,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          GG,K219,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          GG,K220,IS)     
     
     
     
     
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             GG,K34,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             GG,K35,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             GG,K36,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             GG,K37,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             GG,K38,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             GG,K39,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          GG,K310,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          GG,K311,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          GG,K312,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          GG,K313,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          GG,K314,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          GG,K315,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          GG,K316,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          GG,K317,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          GG,K318,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          GG,K319,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          GG,K320,IS)     
     
     
     
     
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             GG,K45,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             GG,K46,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             GG,K47,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             GG,K48,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             GG,K49,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          GG,K410,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          GG,K411,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          GG,K412,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          GG,K413,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          GG,K414,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          GG,K415,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          GG,K416,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          GG,K417,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          GG,K418,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          GG,K419,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          GG,K420,IS)     
     
     
     
     
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             GG,K56,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             GG,K57,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             GG,K58,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             GG,K59,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          GG,K510,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          GG,K511,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          GG,K512,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          GG,K513,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          GG,K514,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          GG,K515,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          GG,K516,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          GG,K517,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          GG,K518,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          GG,K519,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          GG,K520,IS)     
     
     
     
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             GG,K67,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             GG,K68,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             GG,K69,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          GG,K610,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          GG,K611,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          GG,K612,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          GG,K613,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          GG,K614,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          GG,K615,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          GG,K616,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          GG,K617,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          GG,K618,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          GG,K619,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          GG,K620,IS)     
       
     
      
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             GG,K78,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             GG,K79,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          GG,K710,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          GG,K711,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          GG,K712,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          GG,K713,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          GG,K714,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          GG,K715,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          GG,K716,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          GG,K717,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          GG,K718,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          GG,K719,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          GG,K720,IS)     
     
     
      
       CALL S8ZKEG3(LFT,LLT,
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             GG,K89,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          GG,K810,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          GG,K811,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          GG,K812,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          GG,K813,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          GG,K814,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          GG,K815,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          GG,K816,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          GG,K817,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          GG,K818,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          GG,K819,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          GG,K820,IS)     
    
     
     
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          GG,K910,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          GG,K911,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          GG,K912,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          GG,K913,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          GG,K914,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          GG,K915,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          GG,K916,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          GG,K917,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          GG,K918,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          GG,K919,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          GG,K920,IS)  
     
     
     
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          GG,K1011,IS) 
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          GG,K1012,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          GG,K1013,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          GG,K1014,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          GG,K1015,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          GG,K1016,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          GG,K1017,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          GG,K1018,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          GG,K1019,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          GG,K1020,IS)     


       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          GG,K1112,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          GG,K1113,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          GG,K1114,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          GG,K1115,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          GG,K1116,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          GG,K1117,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          GG,K1118,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          GG,K1119,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          GG,K1120,IS)     


       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          GG,K1213,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          GG,K1214,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          GG,K1215,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          GG,K1216,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          GG,K1217,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          GG,K1218,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          GG,K1219,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          GG,K1220,IS)     


       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          GG,K1314,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          GG,K1315,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          GG,K1316,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          GG,K1317,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          GG,K1318,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          GG,K1319,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          GG,K1320,IS)     



       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          GG,K1415,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          GG,K1416,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          GG,K1417,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          GG,K1418,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          GG,K1419,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          GG,K1420,IS)     


       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          GG,K1516,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          GG,K1517,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          GG,K1518,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          GG,K1519,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          GG,K1520,IS)     


       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          GG,K1617,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          GG,K1618,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          GG,K1619,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          GG,K1620,IS)     


       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          GG,K1718,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          GG,K1719,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          GG,K1720,IS)     


       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          GG,K1819,IS)
       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          GG,K1820,IS)     


       CALL S8ZKEG3(LFT,LLT,
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          GG,K1920,IS)     
        
     
      ELSE 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             G33,K12,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             G33,K13,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             G33,K14,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             G33,K15,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             G33,K16,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             G33,K17,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             G33,K18,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             G33,K19,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          G33,K110,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          G33,K111,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          G33,K112,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          G33,K113,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          G33,K114,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          G33,K115,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          G33,K116,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          G33,K117,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          G33,K118,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          G33,K119,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          G33,K120,IS)     
     
     
      
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             G33,K23,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             G33,K24,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             G33,K25,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             G33,K26,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             G33,K27,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             G33,K28,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             G33,K29,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          G33,K210,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          G33,K211,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          G33,K212,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          G33,K213,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          G33,K214,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          G33,K215,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          G33,K216,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          G33,K217,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          G33,K218,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          G33,K219,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          G33,K220,IS)     
     
     
     
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             G33,K34,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             G33,K35,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             G33,K36,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             G33,K37,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             G33,K38,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             G33,K39,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          G33,K310,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          G33,K311,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          G33,K312,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          G33,K313,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          G33,K314,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          G33,K315,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          G33,K316,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          G33,K317,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          G33,K318,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          G33,K319,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          G33,K320,IS)     
     
     
     
      
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             G33,K45,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             G33,K46,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             G33,K47,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             G33,K48,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             G33,K49,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          G33,K410,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          G33,K411,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          G33,K412,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          G33,K413,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          G33,K414,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          G33,K415,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          G33,K416,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          G33,K417,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          G33,K418,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          G33,K419,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          G33,K420,IS)          
     
     
     
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             G33,K56,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             G33,K57,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             G33,K58,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             G33,K59,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          G33,K510,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          G33,K511,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          G33,K512,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          G33,K513,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          G33,K514,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          G33,K515,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          G33,K516,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          G33,K517,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          G33,K518,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          G33,K519,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          G33,K520,IS)     
     
     
     
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             G33,K67,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             G33,K68,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             G33,K69,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          G33,K610,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          G33,K611,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          G33,K612,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          G33,K613,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          G33,K614,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          G33,K615,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          G33,K616,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          G33,K617,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          G33,K618,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          G33,K619,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          G33,K620,IS)     
     
     
     
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             G33,K78,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             G33,K79,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          G33,K710,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          G33,K711,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          G33,K712,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          G33,K713,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          G33,K714,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          G33,K715,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          G33,K716,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          G33,K717,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          G33,K718,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          G33,K719,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          G33,K720,IS)     
     
     
     
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             G33,K89,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          G33,K810,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          G33,K811,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          G33,K812,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          G33,K813,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          G33,K814,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          G33,K815,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          G33,K816,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          G33,K817,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          G33,K818,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          G33,K819,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          G33,K820,IS)     
     
     
     
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          G33,K910,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          G33,K911,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          G33,K912,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          G33,K913,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          G33,K914,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          G33,K915,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          G33,K916,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          G33,K917,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          G33,K918,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          G33,K919,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          G33,K920,IS)


       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          G33,K1011,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          G33,K1012,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          G33,K1013,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          G33,K1014,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          G33,K1015,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          G33,K1016,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          G33,K1017,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          G33,K1018,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          G33,K1019,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          G33,K1020,IS)     



       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          G33,K1112,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          G33,K1113,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          G33,K1114,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          G33,K1115,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          G33,K1116,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          G33,K1117,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          G33,K1118,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          G33,K1119,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          G33,K1120,IS)     



       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          G33,K1213,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          G33,K1214,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          G33,K1215,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          G33,K1216,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          G33,K1217,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          G33,K1218,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          G33,K1219,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          G33,K1220,IS)     
          


       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          G33,K1314,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          G33,K1315,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          G33,K1316,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          G33,K1317,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          G33,K1318,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          G33,K1319,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          G33,K1320,IS)     


       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          G33,K1415,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          G33,K1416,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          G33,K1417,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          G33,K1418,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          G33,K1419,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          G33,K1420,IS)     


       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          G33,K1516,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          G33,K1517,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          G33,K1518,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          G33,K1519,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          G33,K1520,IS)     



       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          G33,K1617,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          G33,K1618,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          G33,K1619,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          G33,K1620,IS)     


       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          G33,K1718,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          G33,K1719,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          G33,K1720,IS)     


       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          G33,K1819,IS)
       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          G33,K1820,IS)     


       CALL S8ZKEG33(LFT,LLT,
     .          PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .          PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .          G33,K1920,IS)     
     
      
C
      
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,2),PY(1,2),PZ(1,2),
     .              DG, K12,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,3),PY(1,3),PZ(1,3),
     .              DG, K13,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,4),PY(1,4),PZ(1,4),
     .              DG, K14,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,5),PY(1,5),PZ(1,5),
     .              DG, K15,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,6),PY(1,6),PZ(1,6),
     .              DG, K16,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,7),PY(1,7),PZ(1,7),
     .              DG, K17,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,8),PY(1,8),PZ(1,8),
     .              DG, K18,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,1),PY(1,1),PZ(1,1),PX(1,9),PY(1,9),PZ(1,9),
     .              DG, K19,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,10),PY(1,10),PZ(1,10),
     .              DG, K110,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,11),PY(1,11),PZ(1,11),
     .              DG, K111,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,12),PY(1,12),PZ(1,12),
     .              DG, K112,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,13),PY(1,13),PZ(1,13),
     .              DG, K113,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,14),PY(1,14),PZ(1,14),
     .              DG, K114,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,15),PY(1,15),PZ(1,15),
     .              DG, K115,IS)     
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,16),PY(1,16),PZ(1,16),
     .              DG, K116,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,17),PY(1,17),PZ(1,17),
     .              DG, K117,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,18),PY(1,18),PZ(1,18),
     .              DG, K118,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,19),PY(1,19),PZ(1,19),
     .              DG, K119,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,20),PY(1,20),PZ(1,20),
     .              DG, K120,IS)     
     
     
     
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,3),PY(1,3),PZ(1,3),
     .              DG, K23,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,4),PY(1,4),PZ(1,4),
     .              DG, K24,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,5),PY(1,5),PZ(1,5),
     .              DG, K25,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,6),PY(1,6),PZ(1,6),
     .              DG, K26,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,7),PY(1,7),PZ(1,7),
     .              DG, K27,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,8),PY(1,8),PZ(1,8),
     .              DG, K28,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,2),PY(1,2),PZ(1,2),PX(1,9),PY(1,9),PZ(1,9),
     .              DG, K29,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,2),PY(1,2),PZ(1,2),PX(1,10),PY(1,10),PZ(1,10),
     .              DG, K210,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,2),PY(1,2),PZ(1,2),PX(1,11),PY(1,11),PZ(1,11),
     .              DG, K211,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,2),PY(1,2),PZ(1,2),PX(1,12),PY(1,12),PZ(1,12),
     .              DG, K212,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,2),PY(1,2),PZ(1,2),PX(1,13),PY(1,13),PZ(1,13),
     .              DG, K213,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,2),PY(1,2),PZ(1,2),PX(1,14),PY(1,14),PZ(1,14),
     .              DG, K214,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,2),PY(1,2),PZ(1,2),PX(1,15),PY(1,15),PZ(1,15),
     .              DG, K215,IS)     
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,2),PY(1,2),PZ(1,2),PX(1,16),PY(1,16),PZ(1,16),
     .              DG, K216,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,2),PY(1,2),PZ(1,2),PX(1,17),PY(1,17),PZ(1,17),
     .              DG, K217,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,2),PY(1,2),PZ(1,2),PX(1,18),PY(1,18),PZ(1,18),
     .              DG, K218,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,2),PY(1,2),PZ(1,2),PX(1,19),PY(1,19),PZ(1,19),
     .              DG, K219,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,2),PY(1,2),PZ(1,2),PX(1,20),PY(1,20),PZ(1,20),
     .              DG, K220,IS)
     
     
     
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,4),PY(1,4),PZ(1,4),
     .              DG, K34,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,5),PY(1,5),PZ(1,5),
     .              DG, K35,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,6),PY(1,6),PZ(1,6),
     .              DG, K36,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,7),PY(1,7),PZ(1,7),
     .              DG, K37,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,8),PY(1,8),PZ(1,8),
     .              DG, K38,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,3),PY(1,3),PZ(1,3),PX(1,9),PY(1,9),PZ(1,9),
     .              DG, K39,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,10),PY(1,10),PZ(1,10),
     .              DG, K310,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,11),PY(1,11),PZ(1,11),
     .              DG, K311,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,12),PY(1,12),PZ(1,12),
     .              DG, K312,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,13),PY(1,13),PZ(1,13),
     .              DG, K313,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,14),PY(1,14),PZ(1,14),
     .              DG, K314,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,15),PY(1,15),PZ(1,15),
     .              DG, K315,IS)     
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,16),PY(1,16),PZ(1,16),
     .              DG, K316,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,17),PY(1,17),PZ(1,17),
     .              DG, K317,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,18),PY(1,18),PZ(1,18),
     .              DG, K318,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,19),PY(1,19),PZ(1,19),
     .              DG, K319,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,20),PY(1,20),PZ(1,20),
     .              DG, K320,IS)
     
    
     
     
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,4),PY(1,4),PZ(1,4),PX(1,5),PY(1,5),PZ(1,5),
     .              DG, K45,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,4),PY(1,4),PZ(1,4),PX(1,6),PY(1,6),PZ(1,6),
     .              DG, K46,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,4),PY(1,4),PZ(1,4),PX(1,7),PY(1,7),PZ(1,7),
     .              DG, K47,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,4),PY(1,4),PZ(1,4),PX(1,8),PY(1,8),PZ(1,8),
     .              DG, K48,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,4),PY(1,4),PZ(1,4),PX(1,9),PY(1,9),PZ(1,9),
     .              DG, K49,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,10),PY(1,10),PZ(1,10),
     .              DG, K410,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,11),PY(1,11),PZ(1,11),
     .              DG, K411,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,12),PY(1,12),PZ(1,12),
     .              DG, K412,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,13),PY(1,13),PZ(1,13),
     .              DG, K413,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,14),PY(1,14),PZ(1,14),
     .              DG, K414,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,15),PY(1,15),PZ(1,15),
     .              DG, K415,IS)     
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,16),PY(1,16),PZ(1,16),
     .              DG, K416,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,17),PY(1,17),PZ(1,17),
     .              DG, K417,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,18),PY(1,18),PZ(1,18),
     .              DG, K418,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,19),PY(1,19),PZ(1,19),
     .              DG, K419,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,20),PY(1,20),PZ(1,20),
     .              DG, K420,IS)
     
     
     
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,5),PY(1,5),PZ(1,5),PX(1,6),PY(1,6),PZ(1,6),
     .              DG, K56,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,5),PY(1,5),PZ(1,5),PX(1,7),PY(1,7),PZ(1,7),
     .              DG, K57,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,5),PY(1,5),PZ(1,5),PX(1,8),PY(1,8),PZ(1,8),
     .              DG, K58,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,5),PY(1,5),PZ(1,5),PX(1,9),PY(1,9),PZ(1,9),
     .              DG, K59,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,10),PY(1,10),PZ(1,10),
     .              DG, K510,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,11),PY(1,11),PZ(1,11),
     .              DG, K511,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,12),PY(1,12),PZ(1,12),
     .              DG, K512,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,13),PY(1,13),PZ(1,13),
     .              DG, K513,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,14),PY(1,14),PZ(1,14),
     .              DG, K514,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,15),PY(1,15),PZ(1,15),
     .              DG, K515,IS)     
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,16),PY(1,16),PZ(1,16),
     .              DG, K516,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,17),PY(1,17),PZ(1,17),
     .              DG, K517,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,18),PY(1,18),PZ(1,18),
     .              DG, K518,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,19),PY(1,19),PZ(1,19),
     .              DG, K519,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,20),PY(1,20),PZ(1,20),
     .              DG, K520,IS)
     
     
     
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,6),PY(1,6),PZ(1,6),PX(1,7),PY(1,7),PZ(1,7),
     .              DG, K67,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,6),PY(1,6),PZ(1,6),PX(1,8),PY(1,8),PZ(1,8),
     .              DG, K68,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,6),PY(1,6),PZ(1,6),PX(1,9),PY(1,9),PZ(1,9),
     .              DG, K69,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,10),PY(1,10),PZ(1,10),
     .              DG, K610,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,11),PY(1,11),PZ(1,11),
     .              DG, K611,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,12),PY(1,12),PZ(1,12),
     .              DG, K612,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,13),PY(1,13),PZ(1,13),
     .              DG, K613,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,14),PY(1,14),PZ(1,14),
     .              DG, K614,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,15),PY(1,15),PZ(1,15),
     .              DG, K615,IS)     
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,16),PY(1,16),PZ(1,16),
     .              DG, K616,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,17),PY(1,17),PZ(1,17),
     .              DG, K617,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,18),PY(1,18),PZ(1,18),
     .              DG, K618,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,19),PY(1,19),PZ(1,19),
     .              DG, K619,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,20),PY(1,20),PZ(1,20),
     .              DG, K620,IS)
     
     
     
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,7),PY(1,7),PZ(1,7),PX(1,8),PY(1,8),PZ(1,8),
     .              DG, K78,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,7),PY(1,7),PZ(1,7),PX(1,9),PY(1,9),PZ(1,9),
     .              DG, K79,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,10),PY(1,10),PZ(1,10),
     .              DG, K710,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,11),PY(1,11),PZ(1,11),
     .              DG, K711,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,12),PY(1,12),PZ(1,12),
     .              DG, K712,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,13),PY(1,13),PZ(1,13),
     .              DG, K713,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,14),PY(1,14),PZ(1,14),
     .              DG, K714,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,15),PY(1,15),PZ(1,15),
     .              DG, K715,IS)     
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,16),PY(1,16),PZ(1,16),
     .              DG, K716,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,17),PY(1,17),PZ(1,17),
     .              DG, K717,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,18),PY(1,18),PZ(1,18),
     .              DG, K718,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,19),PY(1,19),PZ(1,19),
     .              DG, K719,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,20),PY(1,20),PZ(1,20),
     .              DG, K720,IS)
     
     
     
       CALL S8ZKEBG1(LFT,LLT,
     .              PX(1,8),PY(1,8),PZ(1,8),PX(1,9),PY(1,9),PZ(1,9),
     .              DG, K89,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,10),PY(1,10),PZ(1,10),
     .              DG, K810,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,11),PY(1,11),PZ(1,11),
     .              DG, K811,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,12),PY(1,12),PZ(1,12),
     .              DG, K812,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,13),PY(1,13),PZ(1,13),
     .              DG, K813,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,14),PY(1,14),PZ(1,14),
     .              DG, K814,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,15),PY(1,15),PZ(1,15),
     .              DG, K815,IS)     
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,16),PY(1,16),PZ(1,16),
     .              DG, K816,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,17),PY(1,17),PZ(1,17),
     .              DG, K817,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,18),PY(1,18),PZ(1,18),
     .              DG, K818,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,19),PY(1,19),PZ(1,19),
     .              DG, K819,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,20),PY(1,20),PZ(1,20),
     .              DG, K820,IS)
     
     
     
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,10),PY(1,10),PZ(1,10),
     .              DG, K910,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,11),PY(1,11),PZ(1,11),
     .              DG, K911,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,12),PY(1,12),PZ(1,12),
     .              DG, K912,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,13),PY(1,13),PZ(1,13),
     .              DG, K913,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,14),PY(1,14),PZ(1,14),
     .              DG, K914,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,15),PY(1,15),PZ(1,15),
     .              DG, K915,IS)     
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,16),PY(1,16),PZ(1,16),
     .              DG, K916,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,17),PY(1,17),PZ(1,17),
     .              DG, K917,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,18),PY(1,18),PZ(1,18),
     .              DG, K918,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,19),PY(1,19),PZ(1,19),
     .              DG, K919,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,20),PY(1,20),PZ(1,20),
     .              DG, K920,IS)


       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,10),PY(1,10),PZ(1,10),PX(1,11),PY(1,11),PZ(1,11),
     .          DG, K1011,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,10),PY(1,10),PZ(1,10),PX(1,12),PY(1,12),PZ(1,12),
     .          DG, K1012,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,10),PY(1,10),PZ(1,10),PX(1,13),PY(1,13),PZ(1,13),
     .          DG, K1013,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,10),PY(1,10),PZ(1,10),PX(1,14),PY(1,14),PZ(1,14),
     .          DG, K1014,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,10),PY(1,10),PZ(1,10),PX(1,15),PY(1,15),PZ(1,15),
     .          DG, K1015,IS)     
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,10),PY(1,10),PZ(1,10),PX(1,16),PY(1,16),PZ(1,16),
     .          DG, K1016,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,10),PY(1,10),PZ(1,10),PX(1,17),PY(1,17),PZ(1,17),
     .          DG, K1017,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,10),PY(1,10),PZ(1,10),PX(1,18),PY(1,18),PZ(1,18),
     .          DG, K1018,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,10),PY(1,10),PZ(1,10),PX(1,19),PY(1,19),PZ(1,19),
     .          DG, K1019,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,10),PY(1,10),PZ(1,10),PX(1,20),PY(1,20),PZ(1,20),
     .          DG, K1020,IS)


       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,11),PY(1,11),PZ(1,11),PX(1,12),PY(1,12),PZ(1,12),
     .          DG, K1112,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,11),PY(1,11),PZ(1,11),PX(1,13),PY(1,13),PZ(1,13),
     .          DG, K1113,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,11),PY(1,11),PZ(1,11),PX(1,14),PY(1,14),PZ(1,14),
     .          DG, K1114,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,11),PY(1,11),PZ(1,11),PX(1,15),PY(1,15),PZ(1,15),
     .          DG, K1115,IS)     
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,11),PY(1,11),PZ(1,11),PX(1,16),PY(1,16),PZ(1,16),
     .          DG, K1116,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,11),PY(1,11),PZ(1,11),PX(1,17),PY(1,17),PZ(1,17),
     .          DG, K1117,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,11),PY(1,11),PZ(1,11),PX(1,18),PY(1,18),PZ(1,18),
     .          DG, K1118,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,11),PY(1,11),PZ(1,11),PX(1,19),PY(1,19),PZ(1,19),
     .          DG, K1119,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,11),PY(1,11),PZ(1,11),PX(1,20),PY(1,20),PZ(1,20),
     .          DG, K1120,IS)


       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,12),PY(1,12),PZ(1,12),PX(1,13),PY(1,13),PZ(1,13),
     .          DG, K1213,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,12),PY(1,12),PZ(1,12),PX(1,14),PY(1,14),PZ(1,14),
     .          DG, K1214,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,12),PY(1,12),PZ(1,12),PX(1,15),PY(1,15),PZ(1,15),
     .          DG, K1215,IS)     
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,12),PY(1,12),PZ(1,12),PX(1,16),PY(1,16),PZ(1,16),
     .          DG, K1216,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,12),PY(1,12),PZ(1,12),PX(1,17),PY(1,17),PZ(1,17),
     .          DG, K1217,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,12),PY(1,12),PZ(1,12),PX(1,18),PY(1,18),PZ(1,18),
     .          DG, K1218,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,12),PY(1,12),PZ(1,12),PX(1,19),PY(1,19),PZ(1,19),
     .          DG, K1219,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,12),PY(1,12),PZ(1,12),PX(1,20),PY(1,20),PZ(1,20),
     .          DG, K1220,IS)


       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,13),PY(1,13),PZ(1,13),PX(1,14),PY(1,14),PZ(1,14),
     .          DG, K1314,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,13),PY(1,13),PZ(1,13),PX(1,15),PY(1,15),PZ(1,15),
     .          DG, K1315,IS)     
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,13),PY(1,13),PZ(1,13),PX(1,16),PY(1,16),PZ(1,16),
     .          DG, K1316,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,13),PY(1,13),PZ(1,13),PX(1,17),PY(1,17),PZ(1,17),
     .          DG, K1317,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,13),PY(1,13),PZ(1,13),PX(1,18),PY(1,18),PZ(1,18),
     .          DG, K1318,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,13),PY(1,13),PZ(1,13),PX(1,19),PY(1,19),PZ(1,19),
     .          DG, K1319,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,13),PY(1,13),PZ(1,13),PX(1,20),PY(1,20),PZ(1,20),
     .          DG, K1320,IS)



       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,14),PY(1,14),PZ(1,14),PX(1,15),PY(1,15),PZ(1,15),
     .          DG, K1415,IS)     
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,14),PY(1,14),PZ(1,14),PX(1,16),PY(1,16),PZ(1,16),
     .          DG, K1416,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,14),PY(1,14),PZ(1,14),PX(1,17),PY(1,17),PZ(1,17),
     .          DG, K1417,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,14),PY(1,14),PZ(1,14),PX(1,18),PY(1,18),PZ(1,18),
     .          DG, K1418,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,14),PY(1,14),PZ(1,14),PX(1,19),PY(1,19),PZ(1,19),
     .          DG, K1419,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,14),PY(1,14),PZ(1,14),PX(1,20),PY(1,20),PZ(1,20),
     .          DG, K1420,IS)


       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,15),PY(1,15),PZ(1,15),PX(1,16),PY(1,16),PZ(1,16),
     .          DG, K1516,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,15),PY(1,15),PZ(1,15),PX(1,17),PY(1,17),PZ(1,17),
     .          DG, K1517,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,15),PY(1,15),PZ(1,15),PX(1,18),PY(1,18),PZ(1,18),
     .          DG, K1518,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,15),PY(1,15),PZ(1,15),PX(1,19),PY(1,19),PZ(1,19),
     .          DG, K1519,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,15),PY(1,15),PZ(1,15),PX(1,20),PY(1,20),PZ(1,20),
     .          DG, K1520,IS)


       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,16),PY(1,16),PZ(1,16),PX(1,17),PY(1,17),PZ(1,17),
     .          DG, K1617,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,16),PY(1,16),PZ(1,16),PX(1,18),PY(1,18),PZ(1,18),
     .          DG, K1618,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,16),PY(1,16),PZ(1,16),PX(1,19),PY(1,19),PZ(1,19),
     .          DG, K1619,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,16),PY(1,16),PZ(1,16),PX(1,20),PY(1,20),PZ(1,20),
     .          DG, K1620,IS)


       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,17),PY(1,17),PZ(1,17),PX(1,18),PY(1,18),PZ(1,18),
     .          DG, K1718,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,17),PY(1,17),PZ(1,17),PX(1,19),PY(1,19),PZ(1,19),
     .          DG, K1719,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,17),PY(1,17),PZ(1,17),PX(1,20),PY(1,20),PZ(1,20),
     .          DG, K1720,IS)


       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,18),PY(1,18),PZ(1,18),PX(1,19),PY(1,19),PZ(1,19),
     .          DG, K1819,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,18),PY(1,18),PZ(1,18),PX(1,20),PY(1,20),PZ(1,20),
     .          DG, K1820,IS)


       CALL S8ZKEBG1(LFT,LLT,
     .          PX(1,19),PY(1,19),PZ(1,19),PX(1,20),PY(1,20),PZ(1,20),
     .          DG, K1920,IS)
     
     
C     
       IS =1
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,1),PY(1,1),PZ(1,1),PX(1,1),PY(1,1),PZ(1,1),
     .             DG, K11,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,2),PY(1,2),PZ(1,2),PX(1,2),PY(1,2),PZ(1,2),
     .             DG, K22,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,3),PY(1,3),PZ(1,3),PX(1,3),PY(1,3),PZ(1,3),
     .             DG, K33,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,4),PY(1,4),PZ(1,4),PX(1,4),PY(1,4),PZ(1,4),
     .             DG, K44,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,5),PY(1,5),PZ(1,5),PX(1,5),PY(1,5),PZ(1,5),
     .             DG, K55,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,6),PY(1,6),PZ(1,6),PX(1,6),PY(1,6),PZ(1,6),
     .             DG, K66,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,7),PY(1,7),PZ(1,7),PX(1,7),PY(1,7),PZ(1,7),
     .             DG, K77,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,8),PY(1,8),PZ(1,8),PX(1,8),PY(1,8),PZ(1,8),
     .             DG, K88,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .             PX(1,9),PY(1,9),PZ(1,9),PX(1,9),PY(1,9),PZ(1,9),
     .             DG, K99,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .       PX(1,10),PY(1,10),PZ(1,10),PX(1,10),PY(1,10),PZ(1,10),
     .       DG, K1010,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .       PX(1,11),PY(1,11),PZ(1,11),PX(1,11),PY(1,11),PZ(1,11),
     .       DG, K1111,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .       PX(1,12),PY(1,12),PZ(1,12),PX(1,12),PY(1,12),PZ(1,12),
     .       DG, K1212,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .       PX(1,13),PY(1,13),PZ(1,13),PX(1,13),PY(1,13),PZ(1,13),
     .       DG, K1313,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .       PX(1,14),PY(1,14),PZ(1,14),PX(1,14),PY(1,14),PZ(1,14),
     .       DG, K1414,IS)
       CALL S8ZKEBG1(LFT,LLT,
     .       PX(1,15),PY(1,15),PZ(1,15),PX(1,15),PY(1,15),PZ(1,15),
     .       DG, K1515,IS)          
       CALL S8ZKEBG1(LFT,LLT,
     .       PX(1,16),PY(1,16),PZ(1,16),PX(1,16),PY(1,16),PZ(1,16),
     .       DG, K1616,IS)
            CALL S8ZKEBG1(LFT,LLT,
     .       PX(1,17),PY(1,17),PZ(1,17),PX(1,17),PY(1,17),PZ(1,17),
     .       DG, K1717,IS)
            CALL S8ZKEBG1(LFT,LLT,
     .       PX(1,18),PY(1,18),PZ(1,18),PX(1,18),PY(1,18),PZ(1,18),
     .       DG, K1818,IS)
            CALL S8ZKEBG1(LFT,LLT,
     .       PX(1,19),PY(1,19),PZ(1,19),PX(1,19),PY(1,19),PZ(1,19),
     .       DG, K1919,IS)
            CALL S8ZKEBG1(LFT,LLT,
     .       PX(1,20),PY(1,20),PZ(1,20),PX(1,20),PY(1,20),PZ(1,20),
     .       DG, K2020,IS)
     

C     
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             PY(1,1),PX(1,1),PZ(1,1),PX(1,1),PZ(1,1),PY(1,1),
     .             G33,K11,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             PY(1,2),PX(1,2),PZ(1,2),PX(1,2),PZ(1,2),PY(1,2),
     .             G33,K22,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             PY(1,3),PX(1,3),PZ(1,3),PX(1,3),PZ(1,3),PY(1,3),
     .             G33,K33,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             PY(1,4),PX(1,4),PZ(1,4),PX(1,4),PZ(1,4),PY(1,4),
     .             G33,K44,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             PY(1,5),PX(1,5),PZ(1,5),PX(1,5),PZ(1,5),PY(1,5),
     .             G33,K55,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             PY(1,6),PX(1,6),PZ(1,6),PX(1,6),PZ(1,6),PY(1,6),
     .             G33,K66,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             PY(1,7),PX(1,7),PZ(1,7),PX(1,7),PZ(1,7),PY(1,7),
     .             G33,K77,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             PY(1,8),PX(1,8),PZ(1,8),PX(1,8),PZ(1,8),PY(1,8),
     .             G33,K88,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             PY(1,9),PX(1,9),PZ(1,9),PX(1,9),PZ(1,9),PY(1,9),
     .             G33,K99,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .      PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .      PY(1,10),PX(1,10),PZ(1,10),PX(1,10),PZ(1,10),PY(1,10),
     .      G33,K1010,IS) 
       CALL S8ZKEG33(LFT,LLT,
     .      PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .      PY(1,11),PX(1,11),PZ(1,11),PX(1,11),PZ(1,11),PY(1,11),
     .      G33,K1111,IS)
       CALL S8ZKEG33(LFT,LLT,
     .      PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .      PY(1,12),PX(1,12),PZ(1,12),PX(1,12),PZ(1,12),PY(1,12),
     .      G33,K1212,IS)
       CALL S8ZKEG33(LFT,LLT,
     .      PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .      PY(1,13),PX(1,13),PZ(1,13),PX(1,13),PZ(1,13),PY(1,13),
     .      G33,K1313,IS)
       CALL S8ZKEG33(LFT,LLT,
     .      PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .      PY(1,14),PX(1,14),PZ(1,14),PX(1,14),PZ(1,14),PY(1,14),
     .      G33,K1414,IS)
       CALL S8ZKEG33(LFT,LLT,
     .      PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .      PY(1,15),PX(1,15),PZ(1,15),PX(1,15),PZ(1,15),PY(1,15),
     .      G33,K1515,IS)
       CALL S8ZKEG33(LFT,LLT,
     .      PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .      PY(1,16),PX(1,16),PZ(1,16),PX(1,16),PZ(1,16),PY(1,16),
     .      G33,K1616,IS)
       CALL S8ZKEG33(LFT,LLT,
     .      PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .      PY(1,17),PX(1,17),PZ(1,17),PX(1,17),PZ(1,17),PY(1,17),
     .      G33,K1717,IS)
       CALL S8ZKEG33(LFT,LLT,
     .      PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .      PY(1,18),PX(1,18),PZ(1,18),PX(1,18),PZ(1,18),PY(1,18),
     .      G33,K1818,IS)
       CALL S8ZKEG33(LFT,LLT,
     .      PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .      PY(1,19),PX(1,19),PZ(1,19),PX(1,19),PZ(1,19),PY(1,19),
     .      G33,K1919,IS)
       CALL S8ZKEG33(LFT,LLT,
     .      PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .      PY(1,20),PX(1,20),PZ(1,20),PX(1,20),PZ(1,20),PY(1,20),
     .      G33,K2020,IS)
     
     
      ENDIF 

C
      RETURN
      END
