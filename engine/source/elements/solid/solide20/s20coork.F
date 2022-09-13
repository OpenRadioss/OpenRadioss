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
Chd|  S20COORK                      source/elements/solid/solide20/s20coork.F
Chd|-- called by -----------
Chd|        S20KE3                        source/elements/solid/solide20/s20ke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S20COORK(X,IXS,IXS20,NPE,
     .   XX, YY, ZZ ,
     .   OFFG,OFF,SAV,
     .   NC,NGL,MXT,NGEO,NEL,
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
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   X(3,*),
     .  XX(MVSIZ,20), YY(MVSIZ,20), ZZ(MVSIZ,20),
     .   OFFG(*), OFF(*), SAV(NEL,30)
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
       INTEGER NC(MVSIZ,20),MXT(*), NGL(*),NGEO(*)
      INTEGER IXS(NIXS,*), IXS20(12,*),NPE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IPERM1(20),IPERM2(20),N,N1,N2,NN,IUN,J,MXT_1
      DATA IPERM1/0,0,0,0,0,0,0,0,1,2,3,4,1,2,3,4,5,6,7,8/
      DATA IPERM2/0,0,0,0,0,0,0,0,2,3,4,1,5,6,7,8,6,7,8,5/
C-----------------------------------------------
      IUN=1
C
      MXT_1 = IXS(1,LFT)
      DO I=LFT,LLT
      NGEO(I)  =IXS(10,I)
      NGL(I)   =IXS(11,I)
      MXT(I)   =MXT_1
      NC(I,1)  =IXS(2,I)
      NC(I,2)  =IXS(3,I)
      NC(I,3)  =IXS(4,I)
      NC(I,4)  =IXS(5,I)
      NC(I,5)  =IXS(6,I)
      NC(I,6)  =IXS(7,I)
      NC(I,7)  =IXS(8,I)
      NC(I,8)  =IXS(9,I)
      NC(I,9)  =IXS20(1,I)
      NC(I,10) =IXS20(2,I)
      NC(I,11) =IXS20(3,I)
      NC(I,12) =IXS20(4,I)
      NC(I,13) =IXS20(5,I)
      NC(I,14) =IXS20(6,I)
      NC(I,15) =IXS20(7,I)
      NC(I,16) =IXS20(8,I)
      NC(I,17) =IXS20(9,I)
      NC(I,18) =IXS20(10,I)
      NC(I,19) =IXS20(11,I)
      NC(I,20) =IXS20(12,I)
      ENDDO

C----------------------------
C     NODAL COORDINATES     |
C----------------------------
      DO N=1,NPE
      
        IF(ISMSTR==1.OR.(ISMSTR==2.AND.IDTMIN(1)==3))THEN
C
          DO I=LFT,LLT
            IF(OFFG(I)>ONE)THEN
              XX(I,N)=SAV(I,N)
              YY(I,N)=SAV(I,N+NPE)
              ZZ(I,N)=SAV(I,N+2*NPE)
              OFF(I) = OFFG(I)-ONE
            ELSE
              NN = MAX(IUN,NC(I,N))
              XX(I,N)=X(1,NN)
              YY(I,N)=X(2,NN)
              ZZ(I,N)=X(3,NN)
              SAV(I,N)=XX(I,N)
              SAV(I,N+NPE)=YY(I,N)
              SAV(I,N+NPE*2)=ZZ(I,N)
              OFF(I) = OFFG(I)
            ENDIF
          ENDDO
C
        ELSE
C
          DO I=LFT,LLT
              NN = MAX(IUN,NC(I,N))
              XX(I,N)=X(1,NN)
              YY(I,N)=X(2,NN)
              ZZ(I,N)=X(3,NN)
              OFF(I) = MIN(ONE,ABS(OFFG(I)))
          ENDDO
        ENDIF
      ENDDO
C

      DO N=9,NPE
        N1=IPERM1(N)
        N2=IPERM2(N)
        DO I=LFT,LLT
          IF(NC(I,N)==0)THEN
            XX(I,N) = HALF*(XX(I,N1)+XX(I,N2))
            YY(I,N) = HALF*(YY(I,N1)+YY(I,N2))
            ZZ(I,N) = HALF*(ZZ(I,N1)+ZZ(I,N2))
          ENDIF
        ENDDO
      ENDDO
      

C-----------
      DO I=LFT,LLT      
      DO J=1,9
        K11(J,I)  =ZERO
        K12(J,I)  =ZERO
        K13(J,I)  =ZERO
        K14(J,I)  =ZERO
        K15(J,I)  =ZERO
        K16(J,I)  =ZERO
        K17(J,I)  =ZERO
        K18(J,I)  =ZERO
        K19(J,I)  =ZERO
        K110(J,I) =ZERO
        K111(J,I) =ZERO
        K112(J,I) =ZERO
        K113(J,I) =ZERO
        K114(J,I) =ZERO
        K115(J,I) =ZERO
        K116(J,I) =ZERO
        K117(J,I) =ZERO
        K118(J,I) =ZERO
        K119(J,I) =ZERO
        K120(J,I) =ZERO
        K22(J,I)  =ZERO
        K23(J,I)  =ZERO
        K24(J,I)  =ZERO
        K25(J,I)  =ZERO
        K26(J,I)  =ZERO
        K27(J,I)  =ZERO
        K28(J,I)  =ZERO
        K29(J,I)  =ZERO
        K210(J,I) =ZERO
        K211(J,I) =ZERO
        K212(J,I) =ZERO
        K213(J,I) =ZERO
        K214(J,I) =ZERO
        K215(J,I) =ZERO
        K216(J,I) =ZERO
        K217(J,I) =ZERO
        K218(J,I) =ZERO
        K219(J,I) =ZERO
        K220(J,I) =ZERO
        K33(J,I)  =ZERO
        K34(J,I)  =ZERO
        K35(J,I)  =ZERO
        K36(J,I)  =ZERO
        K37(J,I)  =ZERO
        K38(J,I)  =ZERO
        K39(J,I)  =ZERO
        K310(J,I) =ZERO
        K311(J,I) =ZERO
        K312(J,I) =ZERO
        K313(J,I) =ZERO
        K314(J,I) =ZERO
        K315(J,I) =ZERO
        K316(J,I) =ZERO
        K317(J,I) =ZERO
        K318(J,I) =ZERO
        K319(J,I) =ZERO
        K320(J,I) =ZERO
        K44(J,I)  =ZERO
        K45(J,I)  =ZERO
        K46(J,I)  =ZERO
        K47(J,I)  =ZERO
        K48(J,I)  =ZERO
        K49(J,I)  =ZERO
        K410(J,I) =ZERO
        K411(J,I) =ZERO
        K412(J,I) =ZERO
        K413(J,I) =ZERO
        K414(J,I) =ZERO
        K415(J,I) =ZERO
        K416(J,I) =ZERO
        K417(J,I) =ZERO
        K418(J,I) =ZERO
        K419(J,I) =ZERO
        K420(J,I) =ZERO
        K55(J,I)  =ZERO
        K56(J,I)  =ZERO
        K57(J,I)  =ZERO
        K58(J,I)  =ZERO
        K59(J,I)  =ZERO
        K510(J,I) =ZERO
        K511(J,I) =ZERO
        K512(J,I) =ZERO
        K513(J,I) =ZERO
        K514(J,I) =ZERO
        K515(J,I) =ZERO
        K516(J,I) =ZERO
        K517(J,I) =ZERO
        K518(J,I) =ZERO
        K519(J,I) =ZERO
        K520(J,I) =ZERO
        K66(J,I)  =ZERO
        K67(J,I)  =ZERO
        K68(J,I)  =ZERO
        K69(J,I)  =ZERO
        K610(J,I) =ZERO
        K611(J,I) =ZERO
        K612(J,I) =ZERO
        K613(J,I) =ZERO
        K614(J,I) =ZERO
        K615(J,I) =ZERO
        K616(J,I) =ZERO
        K617(J,I) =ZERO
        K618(J,I) =ZERO
        K619(J,I) =ZERO
        K620(J,I) =ZERO
        K77(J,I)  =ZERO
        K78(J,I)  =ZERO
        K79(J,I)  =ZERO
        K710(J,I) =ZERO
        K711(J,I) =ZERO
        K712(J,I) =ZERO
        K713(J,I) =ZERO
        K714(J,I) =ZERO
        K715(J,I) =ZERO
        K716(J,I) =ZERO
        K717(J,I) =ZERO
        K718(J,I) =ZERO
        K719(J,I) =ZERO
        K720(J,I) =ZERO
        K88(J,I)  =ZERO
        K89(J,I)  =ZERO
        K810(J,I) =ZERO
        K811(J,I) =ZERO
        K812(J,I) =ZERO
        K813(J,I) =ZERO
        K814(J,I) =ZERO
        K815(J,I) =ZERO
        K816(J,I) =ZERO
        K817(J,I) =ZERO
        K818(J,I) =ZERO
        K819(J,I) =ZERO
        K820(J,I) =ZERO
        K99(J,I)  =ZERO
        K910(J,I) =ZERO
        K911(J,I) =ZERO
        K912(J,I) =ZERO
        K913(J,I) =ZERO
        K914(J,I) =ZERO
        K915(J,I) =ZERO
        K916(J,I) =ZERO
        K917(J,I) =ZERO
        K918(J,I) =ZERO
        K919(J,I) =ZERO
        K920(J,I) =ZERO
        K1010(J,I)=ZERO
        K1011(J,I)=ZERO
        K1012(J,I)=ZERO
        K1013(J,I)=ZERO
        K1014(J,I)=ZERO
        K1015(J,I)=ZERO
        K1016(J,I)=ZERO
        K1017(J,I)=ZERO
        K1018(J,I)=ZERO
        K1019(J,I)=ZERO
        K1020(J,I)=ZERO
        K1111(J,I)=ZERO
        K1112(J,I)=ZERO
        K1113(J,I)=ZERO
        K1114(J,I)=ZERO
        K1115(J,I)=ZERO
        K1116(J,I)=ZERO
        K1117(J,I)=ZERO
        K1118(J,I)=ZERO
        K1119(J,I)=ZERO
        K1120(J,I)=ZERO
        K1212(J,I)=ZERO
        K1213(J,I)=ZERO
        K1214(J,I)=ZERO
        K1215(J,I)=ZERO
        K1216(J,I)=ZERO
        K1217(J,I)=ZERO
        K1218(J,I)=ZERO
        K1219(J,I)=ZERO
        K1220(J,I)=ZERO
        K1313(J,I)=ZERO
        K1314(J,I)=ZERO
        K1315(J,I)=ZERO
        K1316(J,I)=ZERO
        K1317(J,I)=ZERO
        K1318(J,I)=ZERO
        K1319(J,I)=ZERO
        K1320(J,I)=ZERO
        K1414(J,I)=ZERO
        K1415(J,I)=ZERO
        K1416(J,I)=ZERO
        K1417(J,I)=ZERO
        K1418(J,I)=ZERO
        K1419(J,I)=ZERO
        K1420(J,I)=ZERO
        K1515(J,I)=ZERO
        K1516(J,I)=ZERO
        K1517(J,I)=ZERO
        K1518(J,I)=ZERO
        K1519(J,I)=ZERO
        K1520(J,I)=ZERO
        K1616(J,I)=ZERO
        K1617(J,I)=ZERO
        K1618(J,I)=ZERO
        K1619(J,I)=ZERO
        K1620(J,I)=ZERO
        K1717(J,I)=ZERO
        K1718(J,I)=ZERO
        K1719(J,I)=ZERO
        K1720(J,I)=ZERO
        K1818(J,I)=ZERO
        K1819(J,I)=ZERO
        K1820(J,I)=ZERO
        K1919(J,I)=ZERO
        K1920(J,I)=ZERO
        K2020(J,I)=ZERO
      ENDDO
      ENDDO
C-----------
      RETURN
      END
