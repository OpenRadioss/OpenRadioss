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
Chd|  S20KE3                        source/elements/solid/solide20/s20ke3.F
Chd|-- called by -----------
Chd|        IMP_GLOB_K                    source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|        ASSEM_S20                     source/implicit/assem_s20.F   
Chd|        MMATS                         source/elements/solid/solide8z/mmats.F
Chd|        MMSTIFS                       source/elements/solid/solide8z/mmats.F
Chd|        S20COORK                      source/elements/solid/solide20/s20coork.F
Chd|        S20CUMG3                      source/elements/solid/solide20/s20cumg3.F
Chd|        S20DERI3                      source/elements/solid/solide20/s20deri3.F
Chd|        S20EOFF                       source/elements/solid/solide20/s20eoff.F
Chd|        S20KGEO3                      source/elements/solid/solide20/s20ke3.F
Chd|        S20RST                        source/elements/solid/solide20/s20deri3.F
Chd|        SORTHDIR3                     source/elements/solid/solide/sorthdir3.F
Chd|        SREPLOC3                      source/elements/solid/solide/sreploc3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE S20KE3(PM    ,GEO   ,IXS   ,IXS20 ,X    ,
     1                  ELBUF_STR,ETAG  ,IDDL  ,
     2                  NDOF  ,K_DIAG,K_LT  ,IADK  ,JDIK  ,
     3                  NEL   ,IPM   ,IGEO  ,IKGEO ,BUFMAT)
C----------------------------------------------- 
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr01_c.inc"
#include      "scr03_c.inc"
#include      "scr06_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IXS20(12,*), IKGEO
      INTEGER NEL ,IPM(NPROPMI,*),IGEO(NPROPGI,*)      
C
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*), X(*), 
     .   K11(9,MVSIZ)   ,K12(9,MVSIZ)   ,K13(9,MVSIZ)   ,
     .   K14(9,MVSIZ)   ,K15(9,MVSIZ)   ,
     .   K16(9,MVSIZ)   ,K17(9,MVSIZ)   ,K18(9,MVSIZ)   ,
     .   K19(9,MVSIZ)   ,K110(9,MVSIZ)  ,
     .   K111(9,MVSIZ)  ,K112(9,MVSIZ)  ,K113(9,MVSIZ)  ,
     .   K114(9,MVSIZ)  ,K115(9,MVSIZ)  ,
     .   K116(9,MVSIZ)  ,K117(9,MVSIZ)  ,K118(9,MVSIZ)  ,
     .   K119(9,MVSIZ)  ,K120(9,MVSIZ)  ,
     .   K22(9,MVSIZ)   ,K23(9,MVSIZ)   ,K24(9,MVSIZ)   ,
     .   K25(9,MVSIZ)   ,K26(9,MVSIZ)   ,
     .   K27(9,MVSIZ)   ,K28(9,MVSIZ)   ,K29(9,MVSIZ)   ,
     .   K210(9,MVSIZ)  ,K211(9,MVSIZ)  ,
     .   K212(9,MVSIZ)  ,K213(9,MVSIZ)  ,K214(9,MVSIZ)  ,
     .   K215(9,MVSIZ)  ,K216(9,MVSIZ)  ,
     .   K217(9,MVSIZ)  ,K218(9,MVSIZ)  ,K219(9,MVSIZ)  ,
     .   K220(9,MVSIZ)  ,K33(9,MVSIZ)   ,
     .   K34(9,MVSIZ)   ,K35(9,MVSIZ)   ,K36(9,MVSIZ)   ,
     .   K37(9,MVSIZ)   ,K38(9,MVSIZ)   ,
     .   K39(9,MVSIZ)   ,K310(9,MVSIZ)  ,K311(9,MVSIZ)  ,
     .   K312(9,MVSIZ)  ,K313(9,MVSIZ)  ,
     .   K314(9,MVSIZ)  ,K315(9,MVSIZ)  ,K316(9,MVSIZ)  ,
     .   K317(9,MVSIZ)  ,K318(9,MVSIZ)  ,
     .   K319(9,MVSIZ)  ,K320(9,MVSIZ)  ,K44(9,MVSIZ)   ,
     .   K45(9,MVSIZ)   ,K46(9,MVSIZ)   ,
     .   K47(9,MVSIZ)   ,K48(9,MVSIZ)   ,K49(9,MVSIZ)   ,
     .   K410(9,MVSIZ)  ,K411(9,MVSIZ)  ,
     .   K412(9,MVSIZ)  ,K413(9,MVSIZ)  ,K414(9,MVSIZ)  ,
     .   K415(9,MVSIZ)  ,K416(9,MVSIZ)  ,
     .   K417(9,MVSIZ)  ,K418(9,MVSIZ)  ,K419(9,MVSIZ)  ,
     .   K420(9,MVSIZ)  ,K55(9,MVSIZ)   ,
     .   K56(9,MVSIZ)   ,K57(9,MVSIZ)   ,K58(9,MVSIZ)   ,
     .   K59(9,MVSIZ)   ,K510(9,MVSIZ)  ,
     .   K511(9,MVSIZ)  ,K512(9,MVSIZ)  ,K513(9,MVSIZ)  ,
     .   K514(9,MVSIZ)  ,K515(9,MVSIZ)  ,
     .   K516(9,MVSIZ)  ,K517(9,MVSIZ)  ,K518(9,MVSIZ)  ,
     .   K519(9,MVSIZ)  ,K520(9,MVSIZ)  ,
     .   K66(9,MVSIZ)   ,K67(9,MVSIZ)   ,K68(9,MVSIZ)   ,
     .   K69(9,MVSIZ)   ,K610(9,MVSIZ)  ,
     .   K611(9,MVSIZ)  ,K612(9,MVSIZ)  ,K613(9,MVSIZ)  ,
     .   K614(9,MVSIZ)  ,K615(9,MVSIZ)  ,
     .   K616(9,MVSIZ)  ,K617(9,MVSIZ)  ,K618(9,MVSIZ)  ,
     .   K619(9,MVSIZ)  ,K620(9,MVSIZ)  ,
     .   K77(9,MVSIZ)   ,K78(9,MVSIZ)   ,K79(9,MVSIZ)   ,
     .   K710(9,MVSIZ)  ,K711(9,MVSIZ)  ,
     .   K712(9,MVSIZ)  ,K713(9,MVSIZ)  ,K714(9,MVSIZ)  ,
     .   K715(9,MVSIZ)  ,K716(9,MVSIZ)  ,
     .   K717(9,MVSIZ)  ,K718(9,MVSIZ)  ,K719(9,MVSIZ)  ,
     .   K720(9,MVSIZ)  ,K88(9,MVSIZ)   ,
     .   K89(9,MVSIZ)   ,K810(9,MVSIZ)  ,K811(9,MVSIZ)  ,
     .   K812(9,MVSIZ)  ,K813(9,MVSIZ)  ,
     .   K814(9,MVSIZ)  ,K815(9,MVSIZ)  ,K816(9,MVSIZ)  ,
     .   K817(9,MVSIZ)  ,K818(9,MVSIZ)  ,
     .   K819(9,MVSIZ)  ,K820(9,MVSIZ)  ,K99(9,MVSIZ)   ,
     .   K910(9,MVSIZ)  ,K911(9,MVSIZ)  ,
     .   K912(9,MVSIZ)  ,K913(9,MVSIZ)  ,K914(9,MVSIZ)  ,
     .   K915(9,MVSIZ)  ,K916(9,MVSIZ)  ,
     .   K917(9,MVSIZ)  ,K918(9,MVSIZ)  ,K919(9,MVSIZ)  ,
     .   K920(9,MVSIZ)  ,K1010(9,MVSIZ) ,
     .   K1011(9,MVSIZ) ,K1012(9,MVSIZ) ,K1013(9,MVSIZ) ,
     .   K1014(9,MVSIZ) ,K1015(9,MVSIZ) ,
     .   K1016(9,MVSIZ) ,K1017(9,MVSIZ) ,K1018(9,MVSIZ) ,
     .   K1019(9,MVSIZ) ,K1020(9,MVSIZ) ,
     .   K1111(9,MVSIZ) ,K1112(9,MVSIZ) ,K1113(9,MVSIZ) ,
     .   K1114(9,MVSIZ) ,K1115(9,MVSIZ) ,
     .   K1116(9,MVSIZ) ,K1117(9,MVSIZ) ,K1118(9,MVSIZ) ,
     .   K1119(9,MVSIZ) ,K1120(9,MVSIZ) ,
     .   K1212(9,MVSIZ) ,K1213(9,MVSIZ) ,K1214(9,MVSIZ) ,
     .   K1215(9,MVSIZ) ,K1216(9,MVSIZ) ,
     .   K1217(9,MVSIZ) ,K1218(9,MVSIZ) ,K1219(9,MVSIZ) ,
     .   K1220(9,MVSIZ) ,K1313(9,MVSIZ) ,
     .   K1314(9,MVSIZ) ,K1315(9,MVSIZ) ,K1316(9,MVSIZ) ,
     .   K1317(9,MVSIZ) ,K1318(9,MVSIZ) ,
     .   K1319(9,MVSIZ) ,K1320(9,MVSIZ) ,K1414(9,MVSIZ) ,
     .   K1415(9,MVSIZ) ,K1416(9,MVSIZ) ,
     .   K1417(9,MVSIZ) ,K1418(9,MVSIZ) ,K1419(9,MVSIZ) ,
     .   K1420(9,MVSIZ) ,K1515(9,MVSIZ) ,
     .   K1516(9,MVSIZ) ,K1517(9,MVSIZ) ,K1518(9,MVSIZ) ,
     .   K1519(9,MVSIZ) ,K1520(9,MVSIZ) ,
     .   K1616(9,MVSIZ) ,K1617(9,MVSIZ) ,K1618(9,MVSIZ) ,
     .   K1619(9,MVSIZ) ,K1620(9,MVSIZ) ,
     .   K1717(9,MVSIZ) ,K1718(9,MVSIZ) ,K1719(9,MVSIZ) ,
     .   K1720(9,MVSIZ) ,K1818(9,MVSIZ) ,
     .   K1819(9,MVSIZ) ,K1820(9,MVSIZ) ,K1919(9,MVSIZ) ,
     .   K1920(9,MVSIZ) ,K2020(9,MVSIZ) ,
     .   BUFMAT(*)
     
      INTEGER ETAG(*) , IDDL(*) ,NDOF(*) , IADK(*) ,JDIK(*) 
     
      my_real
     .   K_DIAG(*) , K_LT(*)   
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIPMAX,NPE, NF1, NF2, IADBUF,IBID,
     . J,I,N,K
      PARAMETER (NIPMAX=81)
      PARAMETER (NPE=20)
      
    
      INTEGER NGL(MVSIZ), IR, IS, IT, ILAY, NPTT ,NPTS ,NPTR, NLAY, IP,
     .  MXT(MVSIZ),IKORTH, NC(MVSIZ,20), NGEO(MVSIZ),NPTG
      my_real
     .  XX(MVSIZ,20), YY(MVSIZ,20), ZZ(MVSIZ,20),HH(2,MVSIZ),
     .  DNIDR(MVSIZ,NPE),DNIDS(MVSIZ,NPE),DNIDT(MVSIZ,NPE),
     .  RX(MVSIZ,NIPMAX) , RY(MVSIZ,NIPMAX) , RZ(MVSIZ,NIPMAX) ,
     .  SX(MVSIZ,NIPMAX) , SY(MVSIZ,NIPMAX) , SZ(MVSIZ,NIPMAX) ,
     .  TX(MVSIZ,NIPMAX) , TY(MVSIZ,NIPMAX) , TZ(MVSIZ,NIPMAX) ,
     ,  E1X (MVSIZ,NIPMAX) ,E2X (MVSIZ,NIPMAX) ,E3X (MVSIZ,NIPMAX) ,
     .  E1Y (MVSIZ,NIPMAX) ,E2Y (MVSIZ,NIPMAX) ,E3Y (MVSIZ,NIPMAX) ,
     .  E1Z (MVSIZ,NIPMAX) ,E2Z (MVSIZ,NIPMAX) ,E3Z (MVSIZ,NIPMAX) , 
     .  PX(MVSIZ,NPE,NIPMAX),PY(MVSIZ,NPE,NIPMAX),PZ(MVSIZ,NPE,NIPMAX),
     .  OFF(MVSIZ), WI, VOLNP(MVSIZ,NIPMAX),STIN(MVSIZ,NPE),
     .  NI(NPE,NIPMAX), VOLG(MVSIZ),UL(MVSIZ,NPE),GAMA(MVSIZ,6),
     .  DM(9,MVSIZ), DGM(9,MVSIZ),GM(9,MVSIZ), DELTAX(MVSIZ),BID(1),
     .  DD(9,MVSIZ),GG(MVSIZ),DG(9,MVSIZ),G33(9,MVSIZ),OFFG(MVSIZ)          
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      DOUBLE PRECISION
     .  VOLDP(MVSIZ)
C----------------

      my_real
     .  W_GAUSS(9,9),A_GAUSS(9,9)
      DATA W_GAUSS / 
     1 2.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 1.               ,1.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 0.555555555555556,0.888888888888889,0.555555555555556,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 0.347854845137454,0.652145154862546,0.652145154862546,
     4 0.347854845137454,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 0.236926885056189,0.478628670499366,0.568888888888889,
     5 0.478628670499366,0.236926885056189,0.               ,
     5 0.               ,0.               ,0.               ,
     6 0.171324492379170,0.360761573048139,0.467913934572691,
     6 0.467913934572691,0.360761573048139,0.171324492379170,
     6 0.               ,0.               ,0.               ,
     7 0.129484966168870,0.279705391489277,0.381830050505119,
     7 0.417959183673469,0.381830050505119,0.279705391489277,
     7 0.129484966168870,0.               ,0.               ,
     8 0.101228536290376,0.222381034453374,0.313706645877887,
     8 0.362683783378362,0.362683783378362,0.313706645877887,
     8 0.222381034453374,0.101228536290376,0.               ,
     9 0.081274388361574,0.180648160694857,0.260610696402935,
     9 0.312347077040003,0.330239355001260,0.312347077040003,
     9 0.260610696402935,0.180648160694857,0.081274388361574/
      DATA A_GAUSS / 
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -.577350269189626,0.577350269189626,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -.774596669241483,0.               ,0.774596669241483,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -.861136311594053,-.339981043584856,0.339981043584856,
     4 0.861136311594053,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -.906179845938664,-.538469310105683,0.               ,
     5 0.538469310105683,0.906179845938664,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -.932469514203152,-.661209386466265,-.238619186083197,
     6 0.238619186083197,0.661209386466265,0.932469514203152,
     6 0.               ,0.               ,0.               ,
     7 -.949107912342759,-.741531185599394,-.405845151377397,
     7 0.               ,0.405845151377397,0.741531185599394,
     7 0.949107912342759,0.               ,0.               ,
     8 -.960289856497536,-.796666477413627,-.525532409916329,
     8 -.183434642495650,0.183434642495650,0.525532409916329,
     8 0.796666477413627,0.960289856497536,0.               ,
     9 -.968160239507626,-.836031107326636,-.613371432700590,
     9 -.324253423403809,0.               ,0.324253423403809,
     9 0.613371432700590,0.836031107326636,0.968160239507626/

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      GBUF => ELBUF_STR%GBUF
      NF1=NFT+1
      NF2=NF1-(NUMELS8+NUMELS10)
      
            
      IF (IGTYP == 21.OR.IGTYP == 22) THEN
       IKORTH=2
      ELSEIF (ISORTH>0) THEN
       IKORTH=1
      ELSE
       IKORTH=0
      ENDIF
C-----------
      CALL S20COORK(X,IXS(1,NF1),IXS20(1,NF2),NPE,
     .   XX, YY, ZZ,
     .   GBUF%OFF,OFFG,GBUF%SMSTR,
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
C-----------------------------

      DO N=1,NPE
        DO I=LFT,LLT
          UL(I,N) = ZERO
        ENDDO
      ENDDO
      DO I=LFT,LLT
       VOLG(I) = ZERO
      ENDDO
     
C-----------------------------
C     POINTS D' INTEGRATION 
C-----------------------------
      NLAY = ELBUF_STR%NLAY
      NPTR = ELBUF_STR%NPTR
      NPTS = ELBUF_STR%NPTS
      NPTT = ELBUF_STR%NPTT
      NPTG=NPTT*NPTS*NPTR*NLAY
      IF(NPTG.GT.NIPMAX)STOP 933
      DO IT=1,NPTT
       DO IS=1,NPTS
        DO IR=1,NPTR
C-----------
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
         WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*W_GAUSS(IT,NPTT)
         CALL S20RST(
     1      A_GAUSS(IR,NPTR),A_GAUSS(IS,NPTS),A_GAUSS(IT,NPTT),NI(1,IP),
     2      DNIDR         ,DNIDS         ,DNIDT         )
C
C
         CALL S20DERI3(
     1   NGL,OFFG,A_GAUSS(IR,NPTR),A_GAUSS(IS,NPTS),A_GAUSS(IT,NPTT),WI,
     2    DNIDR    ,DNIDS    ,DNIDT   ,RX(1,IP)  ,RY(1,IP)  ,RZ(1,IP)  ,
     3    SX(1,IP) ,SY(1,IP) ,SZ(1,IP),TX(1,IP)  ,TY(1,IP)  ,TZ(1,IP)  ,
     4    XX       ,YY       ,ZZ      ,PX(1,1,IP),PY(1,1,IP),PZ(1,1,IP),
     5    VOLNP(1,IP),DELTAX ,STIN    ,NI(1,IP)  ,VOLG      ,UL        ,
     6    IR       ,IS       ,IT      ,VOLDP )
     
        CALL SREPLOC3(
     .       RX(1,IP)   ,RY(1,IP)   ,RZ(1,IP)   ,
     .       SX(1,IP)   ,SY(1,IP)   ,SZ(1,IP)   ,
     .       TX(1,IP)   ,TY(1,IP)   ,TZ(1,IP)   ,
     .       E1X(1,IP)  ,E2X (1,IP) ,E3X(1,IP)  ,
     .       E1Y(1,IP)  ,E2Y (1,IP) ,E3Y(1,IP)  ,
     .       E1Z (1,IP) ,E2Z(1,IP)  ,E3Z(1,IP) ) 
      IF (ISORTH == 0) THEN            
        DO I=LFT,LLT                                            
          GAMA(I,1) = UN                               
          GAMA(I,2) = ZERO                                
          GAMA(I,3) = ZERO             
          GAMA(I,4) = ZERO                                
          GAMA(I,5) = UN                                
          GAMA(I,6) = ZERO             
        ENDDO                          
      ELSE                             
        CALL SORTHDIR3(
     .       RX(1,IP)   ,RY(1,IP)   ,RZ(1,IP)   ,
     .       SX(1,IP)   ,SY(1,IP)   ,SZ(1,IP)   ,
     .       TX(1,IP)   ,TY(1,IP)   ,TZ(1,IP)   ,
     .       E1X(1,IP)  ,E2X (1,IP) ,E3X(1,IP)  ,
     .       E1Y(1,IP)  ,E2Y (1,IP) ,E3Y(1,IP)  ,
     .       E1Z (1,IP) ,E2Z(1,IP)  ,E3Z(1,IP), GBUF%GAMA,GAMA )
     
      ENDIF                             
C
        ENDDO
       ENDDO
      ENDDO
      

C-----------      
      IF (MTN.GE.28) THEN
       IADBUF = IPM(7,MXT(1))
      ELSE
       IADBUF = 0
      ENDIF   
      
        CALL MMATS(LFT     ,LLT   ,PM    ,MXT    ,HH    ,
     .            MTN    ,IKORTH  ,IPM   ,IGEO   ,GAMA  ,
     .            BUFMAT(IADBUF)  ,DM    ,DGM    ,GM    ,
     .            JHBE  ,GBUF%SIG ,BID   ,NPTG   ,NEL   )  
     
    
      IBID = 0
C-----------Begin integrating points-----
      ILAY = 1
      DO IR=1,NPTR
       DO IS=1,NPTS
        DO IT=1,NPTT
         LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IT,IS,IT)
C-----------
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
         WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*W_GAUSS(IT,NPTT)
C
         CALL MMSTIFS(PM   ,MXT  ,HH   ,VOLNP(1,IP)  ,IBID ,
     .              DD   ,GG   ,DG    ,G33   ,DM    ,
     .              GM   ,DGM  ,IKORTH,GBUF%SIG,IR  ,
     .              IS   ,IT   ,NEL   )
     

     
         CALL S20CUMG3(
     .   PX(1,1,IP), PY(1,1,IP), PZ(1,1,IP), 
     .   DD  ,GG   ,DG   ,G33  ,IKORTH,
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
     
         IF (IKGEO.GT.0) THEN
         CALL S20KGEO3(LBUF%SIG,VOLNP(1,IP) ,
     .    PX(1,1,IP), PY(1,1,IP), PZ(1,1,IP), 
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
     .   K1717,K1718,K1719,K1720,K1818,K1819,K1820,K1919,K1920,K2020,
     .   NEL  )
         ENDIF 
     
        ENDDO 
       ENDDO 
      ENDDO


      IF (NEIG.GT.0) CALL S20EOFF(
     1   LFT, LLT, IXS(1,NF1),IXS20(1,NF2), ETAG, OFFG)
      CALL ASSEM_S20(
     1     IXS(1,NF1),IXS20(1,NF2),NEL,IDDL  ,NDOF  ,
     2     K_DIAG,K_LT  ,IADK  ,JDIK  ,OFFG  ,    
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
     

C    
      RETURN
      END
      
Chd|====================================================================
Chd|  S20KGEO3                      source/elements/solid/solide20/s20ke3.F
Chd|-- called by -----------
Chd|        S20KE3                        source/elements/solid/solide20/s20ke3.F
Chd|-- calls ---------------
Chd|        S4KEH3                        source/elements/solid/solide4/s4ke3.F
Chd|====================================================================
      SUBROUTINE S20KGEO3(SIG , VOL,
     .   PX, PY, PZ, 
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
     .   K1717,K1718,K1719,K1720,K1818,K1819,K1820,K1919,K1920,K2020,
     .   NEL  )
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
      INTEGER NEL
C     REAL ou REAL*8
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C     REAL ou REAL*8
      my_real
     .   FXX(MVSIZ),FYY(MVSIZ),FZZ(MVSIZ),FXY(MVSIZ),FXZ(MVSIZ),
     .   FYZ(MVSIZ)
C-----------------------------------------------
       DO I=LFT,LLT
        FXX(I)=SIG(I,1)*VOL(I)
        FYY(I)=SIG(I,2)*VOL(I)
        FZZ(I)=SIG(I,3)*VOL(I)
        FXY(I)=SIG(I,4)*VOL(I)
        FYZ(I)=SIG(I,5)*VOL(I)
        FXZ(I)=SIG(I,6)*VOL(I)
       ENDDO
C -----------------------------------------------
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,1),PY(1,1),PZ(1,1),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K11 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,2),PY(1,2),PZ(1,2),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K22 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,3),PY(1,3),PZ(1,3),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K33 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,4),PY(1,4),PZ(1,4),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K44 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,5),PY(1,5),PZ(1,5),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K55 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,6),PY(1,6),PZ(1,6),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K66 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,7),PY(1,7),PZ(1,7),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K77 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K88 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K99 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1010 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,11),PY(1,11),PZ(1,11),PX(1,11),PY(1,11),PZ(1,11),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1111 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,12),PY(1,12),PZ(1,12),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1212 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,13),PY(1,13),PZ(1,13),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1313 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,14),PY(1,14),PZ(1,14),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1414 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,15),PY(1,15),PZ(1,15),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1515 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,16),PY(1,16),PZ(1,16),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1616 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,17),PY(1,17),PZ(1,17),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1717 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,18),PY(1,18),PZ(1,18),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1818 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,19),PY(1,19),PZ(1,19),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1919 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,20),PY(1,20),PZ(1,20),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K2020 )
     
     
C
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,2),PY(1,2),PZ(1,2),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K12 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,3),PY(1,3),PZ(1,3),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K13 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,4),PY(1,4),PZ(1,4),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K14 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,5),PY(1,5),PZ(1,5),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K15 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,6),PY(1,6),PZ(1,6),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K16 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,7),PY(1,7),PZ(1,7),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K17 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K18 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K19 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K110 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,11),PY(1,11),PZ(1,11),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K111 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K112 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K113 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K114 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K115 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K116 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K117 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K118 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K119 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,1),PY(1,1),PZ(1,1),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K120 )
     
     
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,3),PY(1,3),PZ(1,3),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K23 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,4),PY(1,4),PZ(1,4),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K24 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,5),PY(1,5),PZ(1,5),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K25 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,6),PY(1,6),PZ(1,6),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K26 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,7),PY(1,7),PZ(1,7),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K27 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K28 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K29 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K210 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,11),PY(1,11),PZ(1,11),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K211 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K212 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K213 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K214 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K215 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K216 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K217 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K218 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K219 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,2),PY(1,2),PZ(1,2),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K220 )
     
     
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,4),PY(1,4),PZ(1,4),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K34 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,5),PY(1,5),PZ(1,5),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K35 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,6),PY(1,6),PZ(1,6),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K36 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,7),PY(1,7),PZ(1,7),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K37 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K38 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K39 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K310 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,11),PY(1,11),PZ(1,11),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K311 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K312 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K313 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K314 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K315 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K316 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K317 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K318 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K319 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,3),PY(1,3),PZ(1,3),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K320 )
     
     
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,5),PY(1,5),PZ(1,5),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K45 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,6),PY(1,6),PZ(1,6),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K46 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,7),PY(1,7),PZ(1,7),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K47 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K48 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K49 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K410 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,11),PY(1,11),PZ(1,11),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K411 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K412 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K413 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K414 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K415 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K416 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K417 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K418 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K419 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,4),PY(1,4),PZ(1,4),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K420 )
     
     
      CALL S4KEH3(LFT,LLT,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,6),PY(1,6),PZ(1,6),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K56 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,7),PY(1,7),PZ(1,7),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K57 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K58 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K59 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K510 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,11),PY(1,11),PZ(1,11),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K511 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K512 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K513 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K514 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K515 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K516 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K517 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K518 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K519 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,5),PY(1,5),PZ(1,5),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K520 )
     
     
      CALL S4KEH3(LFT,LLT,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,7),PY(1,7),PZ(1,7),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K67 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K68 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K69 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K610 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,11),PY(1,11),PZ(1,11),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K611 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K612 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K613 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K614 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K615 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K616 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K617 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K618 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K619 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,6),PY(1,6),PZ(1,6),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K620 )
     
     
      CALL S4KEH3(LFT,LLT,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,8),PY(1,8),PZ(1,8),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K78 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K79 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K710 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,11),PY(1,11),PZ(1,11),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K711 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K712 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K713 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K714 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K715 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K716 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K717 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K718 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K719 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,7),PY(1,7),PZ(1,7),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K720 )
     
     
      CALL S4KEH3(LFT,LLT,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,9),PY(1,9),PZ(1,9),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K89 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K810 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,11),PY(1,11),PZ(1,11),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K811 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K812 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K813 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K814 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K815 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K816 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K817 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K818 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K819 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,8),PY(1,8),PZ(1,8),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K820 )
     
     
      CALL S4KEH3(LFT,LLT,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,10),PY(1,10),PZ(1,10),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K910 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,11),PY(1,11),PZ(1,11),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K911 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K912 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K913 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K914 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K915 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K916 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K917 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K918 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K919 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,9),PY(1,9),PZ(1,9),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K920 )
     
     
           CALL S4KEH3(LFT,LLT,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,11),PY(1,11),PZ(1,11),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1011 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1012 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1013 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1014 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1015 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1016 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1017 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1018 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1019 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,10),PY(1,10),PZ(1,10),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1020 )


      CALL S4KEH3(LFT,LLT,
     .   PX(1,11),PY(1,11),PZ(1,11),PX(1,12),PY(1,12),PZ(1,12),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1112 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,11),PY(1,11),PZ(1,11),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1113 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,11),PY(1,11),PZ(1,11),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1114 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,11),PY(1,11),PZ(1,11),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1115 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,11),PY(1,11),PZ(1,11),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1116 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,11),PY(1,11),PZ(1,11),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1117 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,11),PY(1,11),PZ(1,11),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1118 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,11),PY(1,11),PZ(1,11),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1119 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,11),PY(1,11),PZ(1,11),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1120 )


      CALL S4KEH3(LFT,LLT,
     .   PX(1,12),PY(1,12),PZ(1,12),PX(1,13),PY(1,13),PZ(1,13),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1213 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,12),PY(1,12),PZ(1,12),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1214 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,12),PY(1,12),PZ(1,12),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1215 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,12),PY(1,12),PZ(1,12),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1216 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,12),PY(1,12),PZ(1,12),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1217 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,12),PY(1,12),PZ(1,12),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1218 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,12),PY(1,12),PZ(1,12),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1219 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,12),PY(1,12),PZ(1,12),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1220 )


      CALL S4KEH3(LFT,LLT,
     .   PX(1,13),PY(1,13),PZ(1,13),PX(1,14),PY(1,14),PZ(1,14),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1314 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,13),PY(1,13),PZ(1,13),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1315 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,13),PY(1,13),PZ(1,13),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1316 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,13),PY(1,13),PZ(1,13),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1317 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,13),PY(1,13),PZ(1,13),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1318 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,13),PY(1,13),PZ(1,13),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1319 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,13),PY(1,13),PZ(1,13),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1320 )



      CALL S4KEH3(LFT,LLT,
     .   PX(1,14),PY(1,14),PZ(1,14),PX(1,15),PY(1,15),PZ(1,15),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1415 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,14),PY(1,14),PZ(1,14),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1416 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,14),PY(1,14),PZ(1,14),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1417 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,14),PY(1,14),PZ(1,14),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1418 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,14),PY(1,14),PZ(1,14),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1419 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,14),PY(1,14),PZ(1,14),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1420 )
     

      CALL S4KEH3(LFT,LLT,
     .   PX(1,15),PY(1,15),PZ(1,15),PX(1,16),PY(1,16),PZ(1,16),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1516 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,15),PY(1,15),PZ(1,15),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1517 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,15),PY(1,15),PZ(1,15),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1518 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,15),PY(1,15),PZ(1,15),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1519 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,15),PY(1,15),PZ(1,15),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1520 )



      CALL S4KEH3(LFT,LLT,
     .   PX(1,16),PY(1,16),PZ(1,16),PX(1,17),PY(1,17),PZ(1,17),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1617 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,16),PY(1,16),PZ(1,16),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1618 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,16),PY(1,16),PZ(1,16),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1619 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,16),PY(1,16),PZ(1,16),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1620 )
     
      CALL S4KEH3(LFT,LLT,
     .   PX(1,17),PY(1,17),PZ(1,17),PX(1,18),PY(1,18),PZ(1,18),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1718 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,17),PY(1,17),PZ(1,17),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1719 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,17),PY(1,17),PZ(1,17),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1720 )

     
      CALL S4KEH3(LFT,LLT,
     .   PX(1,18),PY(1,18),PZ(1,18),PX(1,19),PY(1,19),PZ(1,19),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1819 )
      CALL S4KEH3(LFT,LLT,
     .   PX(1,18),PY(1,18),PZ(1,18),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1820 )
     

      CALL S4KEH3(LFT,LLT,
     .   PX(1,19),PY(1,19),PZ(1,19),PX(1,20),PY(1,20),PZ(1,20),
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ,K1920 )
                    
C
      RETURN
      END
      
