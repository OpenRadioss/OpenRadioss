Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  ASSEM_S20                     source/implicit/assem_s20.F   
Chd|-- called by -----------
Chd|        S20KE3                        source/elements/solid/solide20/s20ke3.F
Chd|-- calls ---------------
Chd|        ASSEM_KII                     source/implicit/imp_glob_k.F  
Chd|        ASSEM_KIJ                     source/implicit/imp_glob_k.F  
Chd|====================================================================
      SUBROUTINE ASSEM_S20(
     1                   IXS   ,IXS20 , NEL  ,IDDL  ,NDOF  ,K_DIAG,
     2                   K_LT  ,IADK  ,JDIK  ,OFF   ,
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
C----6---------------------------------------------------------------7---------8
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
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IXS20(12,*),NEL   ,IDDL(*)  ,NDOF(*)  ,
     .         IADK(*)  ,JDIK(*)
C     REAL
      my_real
     .   K_DIAG(*) ,K_LT(*)   ,OFF(*)
      my_real
     . K11(3,3,*) ,K12(3,3,*) ,K13(3,3,*) ,K14(3,3,*) ,K15(3,3,*) ,
     . K16(3,3,*) ,K17(3,3,*) ,K18(3,3,*) ,K19(3,3,*) ,K110(3,3,*),
     . K111(3,3,*),K112(3,3,*),K113(3,3,*),K114(3,3,*),K115(3,3,*),
     . K116(3,3,*),K117(3,3,*),K118(3,3,*),K119(3,3,*),K120(3,3,*),
     . K22(3,3,*) ,K23(3,3,*) ,K24(3,3,*) ,K25(3,3,*) ,K26(3,3,*) ,
     . K27(3,3,*) ,K28(3,3,*) ,K29(3,3,*) ,K210(3,3,*),K211(3,3,*),
     . K212(3,3,*),K213(3,3,*),K214(3,3,*),K215(3,3,*),K216(3,3,*),
     . K217(3,3,*),K218(3,3,*),K219(3,3,*),K220(3,3,*),K33(3,3,*) ,
     . K34(3,3,*) ,K35(3,3,*) ,K36(3,3,*) ,K37(3,3,*) ,K38(3,3,*) ,
     . K39(3,3,*) ,K310(3,3,*),K311(3,3,*),K312(3,3,*),K313(3,3,*),
     . K314(3,3,*),K315(3,3,*),K316(3,3,*),K317(3,3,*),K318(3,3,*),
     . K319(3,3,*),K320(3,3,*),K44(3,3,*) ,K45(3,3,*) ,K46(3,3,*) ,
     . K47(3,3,*) ,K48(3,3,*) ,K49(3,3,*) ,K410(3,3,*),K411(3,3,*),
     . K412(3,3,*),K413(3,3,*),K414(3,3,*),K415(3,3,*),K416(3,3,*),
     . K417(3,3,*),K418(3,3,*),K419(3,3,*),K420(3,3,*),K55(3,3,*) ,
     . K56(3,3,*) ,K57(3,3,*) ,K58(3,3,*) ,K59(3,3,*) ,K510(3,3,*),
     . K511(3,3,*),K512(3,3,*),K513(3,3,*),K514(3,3,*),K515(3,3,*),
     . K516(3,3,*),K517(3,3,*),K518(3,3,*),K519(3,3,*),K520(3,3,*),
     . K66(3,3,*) ,K67(3,3,*) ,K68(3,3,*) ,K69(3,3,*) ,K610(3,3,*),
     . K611(3,3,*),K612(3,3,*),K613(3,3,*),K614(3,3,*),K615(3,3,*),
     . K616(3,3,*),K617(3,3,*),K618(3,3,*),K619(3,3,*),K620(3,3,*),
     . K77(3,3,*) ,K78(3,3,*) ,K79(3,3,*) ,K710(3,3,*),K711(3,3,*),
     . K712(3,3,*),K713(3,3,*),K714(3,3,*),K715(3,3,*),K716(3,3,*),
     . K717(3,3,*),K718(3,3,*),K719(3,3,*),K720(3,3,*),K88(3,3,*) ,
     . K89(3,3,*) ,K810(3,3,*),K811(3,3,*),K812(3,3,*),K813(3,3,*),
     . K814(3,3,*),K815(3,3,*),K816(3,3,*),K817(3,3,*),K818(3,3,*),
     . K819(3,3,*),K820(3,3,*),K99(3,3,*) ,K910(3,3,*),K911(3,3,*),
     . K912(3,3,*),K913(3,3,*),K914(3,3,*),K915(3,3,*),K916(3,3,*),
     . K917(3,3,*),K918(3,3,*),K919(3,3,*),K920(3,3,*),K1010(3,3,*),
     . K1011(3,3,*),K1012(3,3,*),K1013(3,3,*),K1014(3,3,*),K1015(3,3,*),
     . K1016(3,3,*),K1017(3,3,*),K1018(3,3,*),K1019(3,3,*),K1020(3,3,*),
     . K1111(3,3,*),K1112(3,3,*),K1113(3,3,*),K1114(3,3,*),K1115(3,3,*),
     . K1116(3,3,*),K1117(3,3,*),K1118(3,3,*),K1119(3,3,*),K1120(3,3,*),
     . K1212(3,3,*),K1213(3,3,*),K1214(3,3,*),K1215(3,3,*),K1216(3,3,*),
     . K1217(3,3,*),K1218(3,3,*),K1219(3,3,*),K1220(3,3,*),K1313(3,3,*),
     . K1314(3,3,*),K1315(3,3,*),K1316(3,3,*),K1317(3,3,*),K1318(3,3,*),
     . K1319(3,3,*),K1320(3,3,*),K1414(3,3,*),K1415(3,3,*),K1416(3,3,*),
     . K1417(3,3,*),K1418(3,3,*),K1419(3,3,*),K1420(3,3,*),K1515(3,3,*),
     . K1516(3,3,*),K1517(3,3,*),K1518(3,3,*),K1519(3,3,*),K1520(3,3,*),
     . K1616(3,3,*),K1617(3,3,*),K1618(3,3,*),K1619(3,3,*),K1620(3,3,*),
     . K1717(3,3,*),K1718(3,3,*),K1719(3,3,*),K1720(3,3,*),K1818(3,3,*),
     . K1819(3,3,*),K1820(3,3,*),K1919(3,3,*),K1920(3,3,*),K2020(3,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,EP,NN(MVSIZ,20),J,I
C----6---------------------------------------------------------------7---------8
       DO K = 1,8
        DO EP = 1,NEL
         NN(EP,K)=IXS(K+1,EP)
        ENDDO
       ENDDO

       DO K = 9,20
        DO EP = 1,NEL
         NN(EP,K)=IXS20(K-8,EP)
        ENDDO
       ENDDO

C-------block diagonal k11,k22...k88-----
       CALL ASSEM_KII(NN(1,1),NEL,IDDL,IADK,K_DIAG,K_LT ,K11,3,OFF)
       CALL ASSEM_KII(NN(1,2),NEL,IDDL,IADK,K_DIAG,K_LT ,K22,3,OFF)
       CALL ASSEM_KII(NN(1,3),NEL,IDDL,IADK,K_DIAG,K_LT ,K33,3,OFF)
       CALL ASSEM_KII(NN(1,4),NEL,IDDL,IADK,K_DIAG,K_LT ,K44,3,OFF)
       CALL ASSEM_KII(NN(1,5),NEL,IDDL,IADK,K_DIAG,K_LT ,K55,3,OFF)
       CALL ASSEM_KII(NN(1,6),NEL,IDDL,IADK,K_DIAG,K_LT ,K66,3,OFF)
       CALL ASSEM_KII(NN(1,7),NEL,IDDL,IADK,K_DIAG,K_LT ,K77,3,OFF)
       CALL ASSEM_KII(NN(1,8),NEL,IDDL,IADK,K_DIAG,K_LT ,K88,3,OFF)
       CALL ASSEM_KII(NN(1,9),NEL,IDDL,IADK,K_DIAG,K_LT ,K99,3,OFF)
       CALL ASSEM_KII(NN(1,10),NEL,IDDL,IADK,K_DIAG,K_LT ,K1010,3,OFF)
       CALL ASSEM_KII(NN(1,11),NEL,IDDL,IADK,K_DIAG,K_LT ,K1111,3,OFF)
       CALL ASSEM_KII(NN(1,12),NEL,IDDL,IADK,K_DIAG,K_LT ,K1212,3,OFF)
       CALL ASSEM_KII(NN(1,13),NEL,IDDL,IADK,K_DIAG,K_LT ,K1313,3,OFF)
       CALL ASSEM_KII(NN(1,14),NEL,IDDL,IADK,K_DIAG,K_LT ,K1414,3,OFF)
       CALL ASSEM_KII(NN(1,15),NEL,IDDL,IADK,K_DIAG,K_LT ,K1515,3,OFF)
       CALL ASSEM_KII(NN(1,16),NEL,IDDL,IADK,K_DIAG,K_LT ,K1616,3,OFF)
       CALL ASSEM_KII(NN(1,17),NEL,IDDL,IADK,K_DIAG,K_LT ,K1717,3,OFF)
       CALL ASSEM_KII(NN(1,18),NEL,IDDL,IADK,K_DIAG,K_LT ,K1818,3,OFF)
       CALL ASSEM_KII(NN(1,19),NEL,IDDL,IADK,K_DIAG,K_LT ,K1919,3,OFF)
       CALL ASSEM_KII(NN(1,20),NEL,IDDL,IADK,K_DIAG,K_LT ,K2020,3,OFF)
C-------non diag kij-----
       CALL ASSEM_KIJ(NN(1,1),NN(1,2),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K12,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,3),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K13,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,4),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K14,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,5),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K15,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,6),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K16,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,7),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K17,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,8),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K18,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,9),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K19,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,10),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K110,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,11),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K111,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,12),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K112,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,13),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K113,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,14),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K114,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,15),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K115,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,16),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K116,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,17),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K117,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,18),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K118,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,19),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K119,3,OFF)
       CALL ASSEM_KIJ(NN(1,1),NN(1,20),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K120,3,OFF)



       CALL ASSEM_KIJ(NN(1,2),NN(1,3),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K23,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,4),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K24,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,5),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K25,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,6),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K26,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,7),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K27,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,8),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K28,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,9),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K29,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,10),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K210,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,11),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K211,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,12),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K212,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,13),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K213,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,14),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K214,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,15),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K215,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,16),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K216,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,17),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K217,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,18),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K218,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,19),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K219,3,OFF)
       CALL ASSEM_KIJ(NN(1,2),NN(1,20),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K220,3,OFF)


       CALL ASSEM_KIJ(NN(1,3),NN(1,4),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K34,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,5),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K35,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,6),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K36,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,7),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K37,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,8),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K38,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,9),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K39,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,10),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K310,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,11),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K311,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,12),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K312,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,13),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K313,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,14),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K314,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,15),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K315,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,16),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K316,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,17),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K317,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,18),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K318,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,19),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K319,3,OFF)
       CALL ASSEM_KIJ(NN(1,3),NN(1,20),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K320,3,OFF)



       CALL ASSEM_KIJ(NN(1,4),NN(1,5),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K45,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,6),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K46,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,7),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K47,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,8),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K48,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,9),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K49,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,10),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K410,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,11),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K411,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,12),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K412,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,13),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K413,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,14),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K414,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,15),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K415,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,16),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K416,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,17),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K417,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,18),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K418,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,19),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K419,3,OFF)
       CALL ASSEM_KIJ(NN(1,4),NN(1,20),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K420,3,OFF)



       CALL ASSEM_KIJ(NN(1,5),NN(1,6),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K56,3,OFF)
       CALL ASSEM_KIJ(NN(1,5),NN(1,7),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K57,3,OFF)
       CALL ASSEM_KIJ(NN(1,5),NN(1,8),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K58,3,OFF)
       CALL ASSEM_KIJ(NN(1,5),NN(1,9),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K59,3,OFF)
       CALL ASSEM_KIJ(NN(1,5),NN(1,10),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K510,3,OFF)
       CALL ASSEM_KIJ(NN(1,5),NN(1,11),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K511,3,OFF)
       CALL ASSEM_KIJ(NN(1,5),NN(1,12),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K512,3,OFF)
       CALL ASSEM_KIJ(NN(1,5),NN(1,13),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K513,3,OFF)
       CALL ASSEM_KIJ(NN(1,5),NN(1,14),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K514,3,OFF)
       CALL ASSEM_KIJ(NN(1,5),NN(1,15),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K515,3,OFF)
       CALL ASSEM_KIJ(NN(1,5),NN(1,16),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K516,3,OFF)
       CALL ASSEM_KIJ(NN(1,5),NN(1,17),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K517,3,OFF)
       CALL ASSEM_KIJ(NN(1,5),NN(1,18),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K518,3,OFF)
       CALL ASSEM_KIJ(NN(1,5),NN(1,19),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K519,3,OFF)
       CALL ASSEM_KIJ(NN(1,5),NN(1,20),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K520,3,OFF)



       CALL ASSEM_KIJ(NN(1,6),NN(1,7),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K67,3,OFF)
       CALL ASSEM_KIJ(NN(1,6),NN(1,8),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K68,3,OFF)
       CALL ASSEM_KIJ(NN(1,6),NN(1,9),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K69,3,OFF)
       CALL ASSEM_KIJ(NN(1,6),NN(1,10),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K610,3,OFF)
       CALL ASSEM_KIJ(NN(1,6),NN(1,11),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K611,3,OFF)
       CALL ASSEM_KIJ(NN(1,6),NN(1,12),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K612,3,OFF)
       CALL ASSEM_KIJ(NN(1,6),NN(1,13),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K613,3,OFF)
       CALL ASSEM_KIJ(NN(1,6),NN(1,14),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K614,3,OFF)
       CALL ASSEM_KIJ(NN(1,6),NN(1,15),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K615,3,OFF)
       CALL ASSEM_KIJ(NN(1,6),NN(1,16),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K616,3,OFF)
       CALL ASSEM_KIJ(NN(1,6),NN(1,17),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K617,3,OFF)
       CALL ASSEM_KIJ(NN(1,6),NN(1,18),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K618,3,OFF)
       CALL ASSEM_KIJ(NN(1,6),NN(1,19),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K619,3,OFF)
       CALL ASSEM_KIJ(NN(1,6),NN(1,20),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K620,3,OFF)




       CALL ASSEM_KIJ(NN(1,7),NN(1,8),NEL,IDDL,IADK,JDIK,
     .                K_DIAG, K_LT,K78,3,OFF)
       CALL ASSEM_KIJ(NN(1,7),NN(1,9),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K79,3,OFF)
       CALL ASSEM_KIJ(NN(1,7),NN(1,10),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K710,3,OFF)
       CALL ASSEM_KIJ(NN(1,7),NN(1,11),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K711,3,OFF)
       CALL ASSEM_KIJ(NN(1,7),NN(1,12),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K712,3,OFF)
       CALL ASSEM_KIJ(NN(1,7),NN(1,13),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K713,3,OFF)
       CALL ASSEM_KIJ(NN(1,7),NN(1,14),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K714,3,OFF)
       CALL ASSEM_KIJ(NN(1,7),NN(1,15),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K715,3,OFF)
       CALL ASSEM_KIJ(NN(1,7),NN(1,16),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K716,3,OFF)
       CALL ASSEM_KIJ(NN(1,7),NN(1,17),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K717,3,OFF)
       CALL ASSEM_KIJ(NN(1,7),NN(1,18),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K718,3,OFF)
       CALL ASSEM_KIJ(NN(1,7),NN(1,19),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K719,3,OFF)
       CALL ASSEM_KIJ(NN(1,7),NN(1,20),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K720,3,OFF)


       CALL ASSEM_KIJ(NN(1,8),NN(1,9),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K89,3,OFF)
       CALL ASSEM_KIJ(NN(1,8),NN(1,10),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K810,3,OFF)
       CALL ASSEM_KIJ(NN(1,8),NN(1,11),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K811,3,OFF)
       CALL ASSEM_KIJ(NN(1,8),NN(1,12),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K812,3,OFF)
       CALL ASSEM_KIJ(NN(1,8),NN(1,13),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K813,3,OFF)
       CALL ASSEM_KIJ(NN(1,8),NN(1,14),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K814,3,OFF)
       CALL ASSEM_KIJ(NN(1,8),NN(1,15),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K815,3,OFF)
       CALL ASSEM_KIJ(NN(1,8),NN(1,16),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K816,3,OFF)
       CALL ASSEM_KIJ(NN(1,8),NN(1,17),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K817,3,OFF)
       CALL ASSEM_KIJ(NN(1,8),NN(1,18),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K818,3,OFF)
       CALL ASSEM_KIJ(NN(1,8),NN(1,19),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K819,3,OFF)
       CALL ASSEM_KIJ(NN(1,8),NN(1,20),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K820,3,OFF)


       CALL ASSEM_KIJ(NN(1,9),NN(1,10),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K910,3,OFF)
       CALL ASSEM_KIJ(NN(1,9),NN(1,11),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K911,3,OFF)
       CALL ASSEM_KIJ(NN(1,9),NN(1,12),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K912,3,OFF)
       CALL ASSEM_KIJ(NN(1,9),NN(1,13),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K913,3,OFF)
       CALL ASSEM_KIJ(NN(1,9),NN(1,14),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K914,3,OFF)
       CALL ASSEM_KIJ(NN(1,9),NN(1,15),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K915,3,OFF)
       CALL ASSEM_KIJ(NN(1,9),NN(1,16),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K916,3,OFF)
       CALL ASSEM_KIJ(NN(1,9),NN(1,17),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K917,3,OFF)
       CALL ASSEM_KIJ(NN(1,9),NN(1,18),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K918,3,OFF)
       CALL ASSEM_KIJ(NN(1,9),NN(1,19),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K919,3,OFF)
       CALL ASSEM_KIJ(NN(1,9),NN(1,20),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K920,3,OFF)


       CALL ASSEM_KIJ(NN(1,10),NN(1,11),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1011,3,OFF)
       CALL ASSEM_KIJ(NN(1,10),NN(1,12),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1012,3,OFF)
       CALL ASSEM_KIJ(NN(1,10),NN(1,13),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1013,3,OFF)
       CALL ASSEM_KIJ(NN(1,10),NN(1,14),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1014,3,OFF)
       CALL ASSEM_KIJ(NN(1,10),NN(1,15),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1015,3,OFF)
       CALL ASSEM_KIJ(NN(1,10),NN(1,16),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1016,3,OFF)
       CALL ASSEM_KIJ(NN(1,10),NN(1,17),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1017,3,OFF)
       CALL ASSEM_KIJ(NN(1,10),NN(1,18),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1018,3,OFF)
       CALL ASSEM_KIJ(NN(1,10),NN(1,19),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1019,3,OFF)
       CALL ASSEM_KIJ(NN(1,10),NN(1,20),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1020,3,OFF)


       CALL ASSEM_KIJ(NN(1,11),NN(1,12),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1112,3,OFF)
       CALL ASSEM_KIJ(NN(1,11),NN(1,13),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1113,3,OFF)
       CALL ASSEM_KIJ(NN(1,11),NN(1,14),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1114,3,OFF)
       CALL ASSEM_KIJ(NN(1,11),NN(1,15),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1115,3,OFF)
       CALL ASSEM_KIJ(NN(1,11),NN(1,16),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1116,3,OFF)
       CALL ASSEM_KIJ(NN(1,11),NN(1,17),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1117,3,OFF)
       CALL ASSEM_KIJ(NN(1,11),NN(1,18),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1118,3,OFF)
       CALL ASSEM_KIJ(NN(1,11),NN(1,19),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1119,3,OFF)
       CALL ASSEM_KIJ(NN(1,11),NN(1,20),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1120,3,OFF)



       CALL ASSEM_KIJ(NN(1,12),NN(1,13),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1213,3,OFF)
       CALL ASSEM_KIJ(NN(1,12),NN(1,14),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1214,3,OFF)
       CALL ASSEM_KIJ(NN(1,12),NN(1,15),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1215,3,OFF)
       CALL ASSEM_KIJ(NN(1,12),NN(1,16),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1216,3,OFF)
       CALL ASSEM_KIJ(NN(1,12),NN(1,17),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1217,3,OFF)
       CALL ASSEM_KIJ(NN(1,12),NN(1,18),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1218,3,OFF)
       CALL ASSEM_KIJ(NN(1,12),NN(1,19),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1219,3,OFF)
       CALL ASSEM_KIJ(NN(1,12),NN(1,20),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1220,3,OFF)


       CALL ASSEM_KIJ(NN(1,13),NN(1,14),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1314,3,OFF)
       CALL ASSEM_KIJ(NN(1,13),NN(1,15),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1315,3,OFF)
       CALL ASSEM_KIJ(NN(1,13),NN(1,16),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1316,3,OFF)
       CALL ASSEM_KIJ(NN(1,13),NN(1,17),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1317,3,OFF)
       CALL ASSEM_KIJ(NN(1,13),NN(1,18),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1318,3,OFF)
       CALL ASSEM_KIJ(NN(1,13),NN(1,19),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1319,3,OFF)
       CALL ASSEM_KIJ(NN(1,13),NN(1,20),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1320,3,OFF)


       CALL ASSEM_KIJ(NN(1,14),NN(1,15),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1415,3,OFF)
       CALL ASSEM_KIJ(NN(1,14),NN(1,16),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1416,3,OFF)
       CALL ASSEM_KIJ(NN(1,14),NN(1,17),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1417,3,OFF)
       CALL ASSEM_KIJ(NN(1,14),NN(1,18),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1418,3,OFF)
       CALL ASSEM_KIJ(NN(1,14),NN(1,19),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1419,3,OFF)
       CALL ASSEM_KIJ(NN(1,14),NN(1,20),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1420,3,OFF)


       CALL ASSEM_KIJ(NN(1,15),NN(1,16),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1516,3,OFF)
       CALL ASSEM_KIJ(NN(1,15),NN(1,17),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1517,3,OFF)
       CALL ASSEM_KIJ(NN(1,15),NN(1,18),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1518,3,OFF)
       CALL ASSEM_KIJ(NN(1,15),NN(1,19),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1519,3,OFF)
       CALL ASSEM_KIJ(NN(1,15),NN(1,20),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1520,3,OFF)


       CALL ASSEM_KIJ(NN(1,16),NN(1,17),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1617,3,OFF)
       CALL ASSEM_KIJ(NN(1,16),NN(1,18),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1618,3,OFF)
       CALL ASSEM_KIJ(NN(1,16),NN(1,19),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1619,3,OFF)
       CALL ASSEM_KIJ(NN(1,16),NN(1,20),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1620,3,OFF)


       CALL ASSEM_KIJ(NN(1,17),NN(1,18),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1718,3,OFF)
       CALL ASSEM_KIJ(NN(1,17),NN(1,19),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1719,3,OFF)
       CALL ASSEM_KIJ(NN(1,17),NN(1,20),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1720,3,OFF)


       CALL ASSEM_KIJ(NN(1,18),NN(1,19),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1819,3,OFF)
       CALL ASSEM_KIJ(NN(1,18),NN(1,20),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1820,3,OFF)


       CALL ASSEM_KIJ(NN(1,19),NN(1,20),NEL,IDDL,IADK,JDIK,
     .                 K_DIAG,K_LT,K1920,3,OFF)

C
C----6---------------------------------------------------------------7---------8
      RETURN
      END
