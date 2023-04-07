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
      SUBROUTINE S20KE3(
     1   PM,       GEO,      IXS,      IXS20,
     2   X,        ELBUF_STR,ETAG,     IDDL,
     3   NDOF,     K_DIAG,   K_LT,     IADK,
     4   JDIK,     NEL,      IPM,      IGEO,
     5   IKGEO,    BUFMAT,   NFT,      MTN,
     6   ISMSTR,   JHBE,     IREP,     IGTYP,
     7   ISORTH)
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
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NFT
      INTEGER, INTENT(IN) :: MTN
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JHBE
      INTEGER, INTENT(IN) :: IREP
      INTEGER, INTENT(IN) :: IGTYP
      INTEGER, INTENT(IN) :: ISORTH
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
      CALL S20COORK(
     1   X,           IXS(1,NF1),  IXS20(1,NF2),NPE,
     2   XX,          YY,          ZZ,          GBUF%OFF,
     3   OFFG,        GBUF%SMSTR,  NC,          NGL,
     4   MXT,         NGEO,        NEL,         K11,
     5   K12,         K13,         K14,         K15,
     6   K16,         K17,         K18,         K19,
     7   K110,        K111,        K112,        K113,
     8   K114,        K115,        K116,        K117,
     9   K118,        K119,        K120,        K22,
     A   K23,         K24,         K25,         K26,
     B   K27,         K28,         K29,         K210,
     C   K211,        K212,        K213,        K214,
     D   K215,        K216,        K217,        K218,
     E   K219,        K220,        K33,         K34,
     F   K35,         K36,         K37,         K38,
     G   K39,         K310,        K311,        K312,
     H   K313,        K314,        K315,        K316,
     I   K317,        K318,        K319,        K320,
     J   K44,         K45,         K46,         K47,
     K   K48,         K49,         K410,        K411,
     L   K412,        K413,        K414,        K415,
     M   K416,        K417,        K418,        K419,
     N   K420,        K55,         K56,         K57,
     O   K58,         K59,         K510,        K511,
     P   K512,        K513,        K514,        K515,
     Q   K516,        K517,        K518,        K519,
     R   K520,        K66,         K67,         K68,
     S   K69,         K610,        K611,        K612,
     T   K613,        K614,        K615,        K616,
     U   K617,        K618,        K619,        K620,
     V   K77,         K78,         K79,         K710,
     W   K711,        K712,        K713,        K714,
     X   K715,        K716,        K717,        K718,
     Y   K719,        K720,        K88,         K89,
     Z   K810,        K811,        K812,        K813,
     1   K814,        K815,        K816,        K817,
     2   K818,        K819,        K820,        K99,
     3   K910,        K911,        K912,        K913,
     4   K914,        K915,        K916,        K917,
     5   K918,        K919,        K920,        K1010,
     6   K1011,       K1012,       K1013,       K1014,
     7   K1015,       K1016,       K1017,       K1018,
     8   K1019,       K1020,       K1111,       K1112,
     9   K1113,       K1114,       K1115,       K1116,
     A   K1117,       K1118,       K1119,       K1120,
     B   K1212,       K1213,       K1214,       K1215,
     C   K1216,       K1217,       K1218,       K1219,
     D   K1220,       K1313,       K1314,       K1315,
     E   K1316,       K1317,       K1318,       K1319,
     F   K1320,       K1414,       K1415,       K1416,
     G   K1417,       K1418,       K1419,       K1420,
     H   K1515,       K1516,       K1517,       K1518,
     I   K1519,       K1520,       K1616,       K1617,
     J   K1618,       K1619,       K1620,       K1717,
     K   K1718,       K1719,       K1720,       K1818,
     L   K1819,       K1820,       K1919,       K1920,
     M   K2020,       ISMSTR)
C-----------------------------

      DO N=1,NPE
        DO I=1,NEL
          UL(I,N) = ZERO
        ENDDO
      ENDDO
      DO I=1,NEL
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
      IF(NPTG>NIPMAX)STOP 933
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
     1   NGL,             OFFG,            A_GAUSS(IR,NPTR),A_GAUSS(IS,NPTS),
     2   A_GAUSS(IT,NPTT),WI,              DNIDR,           DNIDS,
     3   DNIDT,           RX(1,IP),        RY(1,IP),        RZ(1,IP),
     4   SX(1,IP),        SY(1,IP),        SZ(1,IP),        TX(1,IP),
     5   TY(1,IP),        TZ(1,IP),        XX,              YY,
     6   ZZ,              PX(1,1,IP),      PY(1,1,IP),      PZ(1,1,IP),
     7   VOLNP(1,IP),     DELTAX,          STIN,            NI(1,IP),
     8   VOLG,            UL,              IR,              IS,
     9   IT,              VOLDP,           NEL)
     
        CALL SREPLOC3(
     1   RX(1,IP), RY(1,IP), RZ(1,IP), SX(1,IP),
     2   SY(1,IP), SZ(1,IP), TX(1,IP), TY(1,IP),
     3   TZ(1,IP), E1X(1,IP),E2X(1,IP),E3X(1,IP),
     4   E1Y(1,IP),E2Y(1,IP),E3Y(1,IP),E1Z(1,IP),
     5   E2Z(1,IP),E3Z(1,IP),NEL)
      IF (ISORTH == 0) THEN            
        DO I=1,NEL                                            
          GAMA(I,1) = ONE                               
          GAMA(I,2) = ZERO                                
          GAMA(I,3) = ZERO             
          GAMA(I,4) = ZERO                                
          GAMA(I,5) = ONE                                
          GAMA(I,6) = ZERO             
        ENDDO                          
      ELSE                             
        CALL SORTHDIR3(
     1   RX(1,IP), RY(1,IP), RZ(1,IP), SX(1,IP),
     2   SY(1,IP), SZ(1,IP), TX(1,IP), TY(1,IP),
     3   TZ(1,IP), E1X(1,IP),E2X(1,IP),E3X(1,IP),
     4   E1Y(1,IP),E2Y(1,IP),E3Y(1,IP),E1Z(1,IP),
     5   E2Z(1,IP),E3Z(1,IP),GBUF%GAMA,GAMA,
     6   NEL,      IREP)
     
      ENDIF                             
C
        ENDDO
       ENDDO
      ENDDO
      

C-----------      
      IF (MTN>=28) THEN
       IADBUF = IPM(7,MXT(1))
      ELSE
       IADBUF = 0
      ENDIF   
      
        CALL MMATS(1     ,NEL   ,PM    ,MXT    ,HH    ,
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
         CALL MMSTIFS(
     1   PM,         MXT,        HH,         VOLNP(1,IP),
     2   IBID,       DD,         GG,         DG,
     3   G33,        DM,         GM,         DGM,
     4   IKORTH,     GBUF%SIG,   IR,         IS,
     5   IT,         NEL,        JHBE,       MTN)
     

     
         CALL S20CUMG3(
     1   PX(1,1,IP),PY(1,1,IP),PZ(1,1,IP),DD,
     2   GG,        DG,        G33,       IKORTH,
     3   K11,       K12,       K13,       K14,
     4   K15,       K16,       K17,       K18,
     5   K19,       K110,      K111,      K112,
     6   K113,      K114,      K115,      K116,
     7   K117,      K118,      K119,      K120,
     8   K22,       K23,       K24,       K25,
     9   K26,       K27,       K28,       K29,
     A   K210,      K211,      K212,      K213,
     B   K214,      K215,      K216,      K217,
     C   K218,      K219,      K220,      K33,
     D   K34,       K35,       K36,       K37,
     E   K38,       K39,       K310,      K311,
     F   K312,      K313,      K314,      K315,
     G   K316,      K317,      K318,      K319,
     H   K320,      K44,       K45,       K46,
     I   K47,       K48,       K49,       K410,
     J   K411,      K412,      K413,      K414,
     K   K415,      K416,      K417,      K418,
     L   K419,      K420,      K55,       K56,
     M   K57,       K58,       K59,       K510,
     N   K511,      K512,      K513,      K514,
     O   K515,      K516,      K517,      K518,
     P   K519,      K520,      K66,       K67,
     Q   K68,       K69,       K610,      K611,
     R   K612,      K613,      K614,      K615,
     S   K616,      K617,      K618,      K619,
     T   K620,      K77,       K78,       K79,
     U   K710,      K711,      K712,      K713,
     V   K714,      K715,      K716,      K717,
     W   K718,      K719,      K720,      K88,
     X   K89,       K810,      K811,      K812,
     Y   K813,      K814,      K815,      K816,
     Z   K817,      K818,      K819,      K820,
     1   K99,       K910,      K911,      K912,
     2   K913,      K914,      K915,      K916,
     3   K917,      K918,      K919,      K920,
     4   K1010,     K1011,     K1012,     K1013,
     5   K1014,     K1015,     K1016,     K1017,
     6   K1018,     K1019,     K1020,     K1111,
     7   K1112,     K1113,     K1114,     K1115,
     8   K1116,     K1117,     K1118,     K1119,
     9   K1120,     K1212,     K1213,     K1214,
     A   K1215,     K1216,     K1217,     K1218,
     B   K1219,     K1220,     K1313,     K1314,
     C   K1315,     K1316,     K1317,     K1318,
     D   K1319,     K1320,     K1414,     K1415,
     E   K1416,     K1417,     K1418,     K1419,
     F   K1420,     K1515,     K1516,     K1517,
     G   K1518,     K1519,     K1520,     K1616,
     H   K1617,     K1618,     K1619,     K1620,
     I   K1717,     K1718,     K1719,     K1720,
     J   K1818,     K1819,     K1820,     K1919,
     K   K1920,     K2020,     NEL)
     
         IF (IKGEO>0) THEN
         CALL S20KGEO3(
     1   LBUF%SIG,   VOLNP(1,IP),PX(1,1,IP), PY(1,1,IP),
     2   PZ(1,1,IP), K11,        K12,        K13,
     3   K14,        K15,        K16,        K17,
     4   K18,        K19,        K110,       K111,
     5   K112,       K113,       K114,       K115,
     6   K116,       K117,       K118,       K119,
     7   K120,       K22,        K23,        K24,
     8   K25,        K26,        K27,        K28,
     9   K29,        K210,       K211,       K212,
     A   K213,       K214,       K215,       K216,
     B   K217,       K218,       K219,       K220,
     C   K33,        K34,        K35,        K36,
     D   K37,        K38,        K39,        K310,
     E   K311,       K312,       K313,       K314,
     F   K315,       K316,       K317,       K318,
     G   K319,       K320,       K44,        K45,
     H   K46,        K47,        K48,        K49,
     I   K410,       K411,       K412,       K413,
     J   K414,       K415,       K416,       K417,
     K   K418,       K419,       K420,       K55,
     L   K56,        K57,        K58,        K59,
     M   K510,       K511,       K512,       K513,
     N   K514,       K515,       K516,       K517,
     O   K518,       K519,       K520,       K66,
     P   K67,        K68,        K69,        K610,
     Q   K611,       K612,       K613,       K614,
     R   K615,       K616,       K617,       K618,
     S   K619,       K620,       K77,        K78,
     T   K79,        K710,       K711,       K712,
     U   K713,       K714,       K715,       K716,
     V   K717,       K718,       K719,       K720,
     W   K88,        K89,        K810,       K811,
     X   K812,       K813,       K814,       K815,
     Y   K816,       K817,       K818,       K819,
     Z   K820,       K99,        K910,       K911,
     1   K912,       K913,       K914,       K915,
     2   K916,       K917,       K918,       K919,
     3   K920,       K1010,      K1011,      K1012,
     4   K1013,      K1014,      K1015,      K1016,
     5   K1017,      K1018,      K1019,      K1020,
     6   K1111,      K1112,      K1113,      K1114,
     7   K1115,      K1116,      K1117,      K1118,
     8   K1119,      K1120,      K1212,      K1213,
     9   K1214,      K1215,      K1216,      K1217,
     A   K1218,      K1219,      K1220,      K1313,
     B   K1314,      K1315,      K1316,      K1317,
     C   K1318,      K1319,      K1320,      K1414,
     D   K1415,      K1416,      K1417,      K1418,
     E   K1419,      K1420,      K1515,      K1516,
     F   K1517,      K1518,      K1519,      K1520,
     G   K1616,      K1617,      K1618,      K1619,
     H   K1620,      K1717,      K1718,      K1719,
     I   K1720,      K1818,      K1819,      K1820,
     J   K1919,      K1920,      K2020,      NEL)
         ENDIF 
     
        ENDDO 
       ENDDO 
      ENDDO


      IF (NEIG>0) CALL S20EOFF(
     1   1, NEL, IXS(1,NF1),IXS20(1,NF2), ETAG, OFFG)
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
