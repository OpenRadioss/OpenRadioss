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
Chd|  I24DST3                       source/interfaces/int24/i24dst3.F
Chd|-- called by -----------
Chd|        I24MAINF                      source/interfaces/int24/i24main.F
Chd|-- calls ---------------
Chd|        I24NEXTTRIA                   source/interfaces/int24/i24dst3.F
Chd|        INTERSECSH                    source/interfaces/int24/i24dst3.F
Chd|        INTERSECV                     source/interfaces/int24/i24dst3.F
Chd|        INTERSECV0                    source/interfaces/int24/i24dst3.F
Chd|        S_IN_M4                       source/interfaces/int24/i24dst3.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24DST3(
     1                  JLT    ,CAND_N ,CAND_E ,CN_LOC ,CE_LOC ,
     2                  X1     ,X2     ,X3     ,X4     ,Y1     ,
     3                  Y2     ,Y3     ,Y4     ,Z1     ,Z2     ,
     4                  Z3     ,Z4     ,XI     ,YI     ,ZI     ,
     5                  VX1    ,VX2    ,VX3    ,VX4    ,VXI    ,
     6                  VY1    ,VY2    ,VY3    ,VY4    ,VYI    ,
     7                  VZ1    ,VZ2    ,VZ3    ,VZ4    ,VZI    ,
     8                  N1     ,N2     ,N3     ,H1     ,H2     ,
     9                  H3     ,H4     ,NIN    ,NSN    ,IX1    ,
     A                  IX2    ,IX3    ,IX4    ,NSVG   ,STIF   ,
     B                  JLT_NEW,INACTI ,MSEGLO ,GAPS   ,GAP_NM ,
     C                  KINI   ,IRECT  ,IRTLM  ,TIME_S ,SUBTRIA,
     D                  INTTH  ,NSMS   ,PENE   ,XX0    ,YY0    ,
     E                  ZZ0    ,VX     ,VY     ,VZ     ,IXX    ,
     F                  MVOISIN,PMAX_GAP,SECND_FR,GAP_M ,PENE_OLD,
     G                  STIF_OLD,ITRIV ,ITAB   ,CAND_T ,IEDGE  ,
     H                  NFT     ,PENMIN,EPS0   ,NM1    ,NM2    ,
     I                  NM3     ,INTPLY ,DGAP_NM,ICONT_I,MARGE ,
     J                  NSNE    ,ISPT2  ,IZERO ,IKNON  ,PENREF ) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "impl1_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, JLT_NEW,NIN,NSN,INACTI,IEDGE, NFT,
     .        CAND_N(*),CN_LOC(MVSIZ),
     .        CAND_E(*),CE_LOC(MVSIZ), NSVG(MVSIZ), KINI(*), CAND_T(*)
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        INTTH,MSEGLO(*),IRTLM(2,NSN) ,SUBTRIA(MVSIZ),
     .        NSMS(*),IXX(MVSIZ,13),MVOISIN(4,*),ITRIV(4,MVSIZ),
     .        INTPLY ,NSNE,ISPT2(*),IZERO
      my_real
     .     N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PMAX_GAP,
     .     H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .     KB1(MVSIZ), KB2(MVSIZ), KB3(MVSIZ), KB4(MVSIZ),
     .     KC1(MVSIZ), KC2(MVSIZ), KC3(MVSIZ), KC4(MVSIZ),
     .     X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .     Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .     Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .     XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .     VX1(MVSIZ),VX2(MVSIZ),VX3(MVSIZ),VX4(MVSIZ),VY1(MVSIZ),
     .     VY2(MVSIZ),VY3(MVSIZ),VY4(MVSIZ),VZ1(MVSIZ),VZ2(MVSIZ),
     .     VZ3(MVSIZ),VZ4(MVSIZ),VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),
     .     TIME_S(NSN),PENE(MVSIZ),SECND_FR(6,NSN),
     .     XX0(MVSIZ,17),YY0(MVSIZ,17),ZZ0(MVSIZ,17),GAP_M(*),
     .     VX(MVSIZ,17),VY(MVSIZ,17),VZ(MVSIZ,17),GAPS(*),GAP_NM(12,*),
     .     PENE_OLD(5,NSN),STIF_OLD(2,NSN),PENMIN,EPS0,
     .     NM1(MVSIZ), NM2(MVSIZ), NM3(MVSIZ),DGAP_NM(4,*),MARGE
      INTEGER IRECT(4,*),ITAB(*),ICONT_I(*)
      INTEGER, DIMENSION(MVSIZ), INTENT(IN)  :: IKNON
      my_real, DIMENSION(MVSIZ), INTENT(OUT) :: PENREF
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG, J, K, L, INTERSECT,MG,N,II,NSLID,ITR,ITS,ITT,ITQ,NS
      my_real
     .     X5(MVSIZ), Y5(MVSIZ), Z5(MVSIZ),
     .     VX5(MVSIZ), VY5(MVSIZ), VZ5(MVSIZ),
     .     AL1(MVSIZ),  AL2(MVSIZ),  AL3(MVSIZ), AL4(MVSIZ),
     .     NXX(MVSIZ,17),NYY(MVSIZ,17),NZZ(MVSIZ,17),
     .     X51,  X52,  X53, X54,
     .     Y51,  Y52,  Y53, Y54,
     .     Z51,  Z52,  Z53, Z54,
     .     XO16, XO17, XO15, XO14, X163, X153, X152, X142, X141, 
     .     YO16, YO17, YO15, YO14, Y163, Y153, Y152, Y142, Y141, 
     .     ZO16, ZO17, ZO15, ZO14, Z163, Z153, Z152, Z142, Z141, 
     .     X164, X174, X171, 
     .     Y164, Y174, Y171, 
     .     Z164, Z174, Z171, 
     .     XI1,  XI2,  XI3,  XI4,  XI5,
     .     YI1,  YI2,  YI3,  YI4,  YI5,
     .     ZI1,  ZI2,  ZI3,  ZI4,  ZI5,
     .     XIA,  XIB,  XIC,
     .     YIA,  YIB,  YIC,
     .     ZIA,  ZIB,  ZIC,
     .     XO1, XO2, XO3, XO4, XO5, XOI,
     .     YO1, YO2, YO3, YO4, YO5, YOI,
     .     ZO1, ZO2, ZO3, ZO4, ZO5, ZOI,XS,YS,ZS,
     .     XM1, XM2, XM3, XM4, XM5, 
     .     YM1, YM2, YM3, YM4, YM5, 
     .     ZM1, ZM2, ZM3, ZM4, ZM5
      my_real
     .     NX(MVSIZ), NY(MVSIZ), NZ(MVSIZ), 
     .     SX125(MVSIZ),SX235(MVSIZ),SX345(MVSIZ),SX415(MVSIZ),
     .     SY125(MVSIZ),SY235(MVSIZ),SY345(MVSIZ),SY415(MVSIZ),
     .     SZ125(MVSIZ),SZ235(MVSIZ),SZ345(MVSIZ),SZ415(MVSIZ),
     .     SX1250(MVSIZ),SX2350(MVSIZ),SX3450(MVSIZ),SX4150(MVSIZ),
     .     SY1250(MVSIZ),SY2350(MVSIZ),SY3450(MVSIZ),SY4150(MVSIZ),
     .     SZ1250(MVSIZ),SZ2350(MVSIZ),SZ3450(MVSIZ),SZ4150(MVSIZ),
     .     SX2114(MVSIZ),SX3215(MVSIZ),SX4316(MVSIZ),SX1417(MVSIZ),
     .     SY2114(MVSIZ),SY3215(MVSIZ),SY4316(MVSIZ),SY1417(MVSIZ),
     .     SZ2114(MVSIZ),SZ3215(MVSIZ),SZ4316(MVSIZ),SZ1417(MVSIZ),
     .     SX21140(MVSIZ),SX32150(MVSIZ),SX43160(MVSIZ),SX14170(MVSIZ),
     .     SY21140(MVSIZ),SY32150(MVSIZ),SY43160(MVSIZ),SY14170(MVSIZ),
     .     SZ21140(MVSIZ),SZ32150(MVSIZ),SZ43160(MVSIZ),SZ14170(MVSIZ),
     .     LA(MVSIZ),LB(MVSIZ), LC(MVSIZ), 
     .     XA(MVSIZ),YA(MVSIZ),ZA(MVSIZ),
     .     XB(MVSIZ),YB(MVSIZ),ZB(MVSIZ),
     .     XC(MVSIZ),YC(MVSIZ),ZC(MVSIZ),
     .     XD(MVSIZ),YD(MVSIZ),ZD(MVSIZ),
     .     XE(MVSIZ),YE(MVSIZ),ZE(MVSIZ),
     .     XF(MVSIZ),YF(MVSIZ),ZF(MVSIZ),ADT0(MVSIZ),
     .     XX(MVSIZ,17),YY(MVSIZ,17),ZZ(MVSIZ,17),
     .     XXL(17),YYL(17),ZZL(17)
      my_real
     .     S2,D1,D2,D3,D4,UNP,ZEROM,H0,NQX,NQY,NQZ,LA2,LB2,LC2,
     .     SX,SX1,SX2,SX3,SX4,SX5,SX0,
     .     SY,SY1,SY2,SY3,SY4,SY5,SY0,
     .     SZ,SZ1,SZ2,SZ3,SZ4,SZ5,SZ0,
     .     V12,V23,V34,V41,
     .     NX1, NX2, NX3, NX4,
     .     NY1, NY2, NY3, NY4,
     .     NZ1, NZ2, NZ3, NZ4,
     .     NX14, NX15, NX16, NX17,
     .     NY14, NY15, NY16, NY17,
     .     NZ14, NZ15, NZ16, NZ17,
     .     SOX125,SOX235,SOX345,SOX415,
     .     SOY125,SOY235,SOY345,SOY415,
     .     SOZ125,SOZ235,SOZ345,SOZ415,
     .     SOX2114,SOX3215,SOX4316,SOX1417,
     .     SOY2114,SOY3215,SOY4316,SOY1417,
     .     SOZ2114,SOZ3215,SOZ4316,SOZ1417,
     .     XO12,XO23,XO34,XO41,SXO1,SXO2,SXO3,SXO4,SXO5,
     .     YO12,YO23,YO34,YO41,SYO1,SYO2,SYO3,SYO4,SYO5,
     .     ZO12,ZO23,ZO34,ZO41,SZO1,SZO2,SZO3,SZO4,SZO5,
     .     SXO12,SXO23,SXO34,SXO41,VO12,VO23,VO34,VO41,
     .     SYO12,SYO23,SYO34,SYO41,
     .     SZO12,SZO23,SZO34,SZO41,
     .     X13_2,Y13_2,Z13_2,X7_3 ,Y7_3 ,Z7_3 ,
     .     X9_4 ,Y9_4 ,Z9_4 ,X11_1,Y11_1,Z11_1,
     .     X6_4 ,Y6_4 ,Z6_4 ,X8_1 ,Y8_1 ,Z8_1 ,
     .     X10_2,Y10_2,Z10_2,X12_3,Y12_3,Z12_3,
     .     PRX,PRY,PRZ,PSX,PSY,PSZ,PTX,PTY,PTZ,
     .     NQRX,NQRY,NQRZ,NQSX,NQSY,NQSZ,NQTX,NQTY,NQTZ,
     .     NRX,NRY,NRZ,NSX,NSY,NSZ,NTX,NTY,NTZ,
     .     NAX,NAY,NAZ,NBX,NBY,NBZ,NCX,NCY,NCZ,
     .     SAX,SAY,SAZ,SBX,SBY,SBZ,SCX,SCY,SCZ,
     .     TRX,TRY,TRZ,TSX,TSY,TSZ,TTX,TTY,TTZ,
     .     TR2,TS2,TT2,AAA,BBB,VR,VS,VT,NNX,NNY,NNZ,
     .     XAB,XBC,XCA,YAB,YBC,YCA,ZAB,ZBC,ZCA,
     .     XCE,YCE,ZCE,XAF,YAF,ZAF,
     .     XPA,YPA,ZPA,XPB,YPB,ZPB,XPC,YPC,ZPC,XPI,YPI,ZPI,
     .     ABX,BCX,CAX,ABY,BCY,CAY,ABZ,BCZ,CAZ,XBD,YBD,ZBD
      my_real
     .     GAP2, DS2,T1,T2,T3,XXX,YYY,ZZZ,NNI,NI2,
     .     AL1NUM,AL2NUM,AL3NUM,AL4NUM,AL1DEN,AL2DEN,AL3DEN,AL4DEN,
     .     X23D,Y23D,Z23D,X34D,Y34D,Z34D,X41D,Y41D,Z41D,
     .     X12D,Y12D,Z12D,GAP2D,XI5D,YI5D,ZI5D,S2D, LA0,LB0,LC0,
     .     HLA, ADT,OVERW,EPS,PENE_SH,OVERW0,VOL,EPS2,DGAP,RX,RY,RZ,
     .     PENE_TM(MVSIZ),LA_TM(MVSIZ),LB_TM(MVSIZ),N_TM(3,MVSIZ),
     .     AREA(MVSIZ)
      INTEGER ICONTACT(MVSIZ),IT0(3,20),IT(3,20,MVSIZ),IC(10,20),
     .     ISTEP(MVSIZ),IRTLM_OLD(MVSIZ),SUBTRIA_N(MVSIZ),
     .     NSS,MGLOB,ibug,JG,IZLIM,ip,IKEEP,LARGEP,IPEN0(MVSIZ),IER,
     .     NOD1,NOD2,NOR_OLD,ISHEL(MVSIZ),ICONT_R(MVSIZ),
     .     SUBTRIA_TM(MVSIZ),IFIRST
      my_real
     .     UNPT,ZEROMT,EPS02,TOLE,EPSEG,MARGE025,TOL_INTS,TOL_B,EPSEXT
! +----+----------+----------+-------------+
! | IC                                     |
! +----+----------+----------+-------------+
! | ST | noeuds T | noeuds TV| noeuds Quad |
! +----+----------+----------+-------------+        11-------10       
! |  1 |  5  1  2 | 14  3  4 |  3  4  1  2 |         |\ 19  /|        
! |  2 |  5  2  3 | 15  4  1 |  4  1  2  3 |         | \   / |                                    
! |  3 |  5  3  4 | 16  1  2 |  1  2  3  4 |         |  \ /  |           
! |  4 |  5  4  1 | 17  2  3 |  2  3  4  1 |         |  16   |           
! +----+----------+----------+-------------+         |15/ \11|        
! |  5 | 14  2  1 |  5  6  7 |  6  7  2  1 |         | /   \ |      
! |  6 | 15  3  2 |  5  8  9 |  8  9  3  2 |         |/  7  \|       
! |  7 | 16  4  3 |  5 10 11 | 10 11  4  3 |12-------4-------3-------9
! |  8 | 17  1  4 |  5 12 13 | 12 13  1  4 | |\ 12  /|\     /|\ 14  /|
! +----+----------+----------+-------------+ | \   / | \ 3 / | \   / |
! |  9 | 14  1  6 |(13) 7  2 |  7  2  1  6 | |  \ /  |  \ /2 |6 \ /18|
! | 10 | 15  2  8 |( 7) 9  3 |  9  3  2  8 | |  17   |   5   |  15   |
! | 11 | 16  3 10 |( 9)11  4 | 11  4  3 10 | |20/ \ 8| 4/ \  |  / \  |
! | 12 | 17  4 12 |(11)13  1 | 13  1  4 12 | | /   \ | / 1 \ | /   \ |
! +----+----------+----------+-------------+ |/ 16  \|/     \|/ 10  \|
! | 13 | 14  7  2 |( 8) 1  6 |  1  6  7  2 |13-------1-------2-------8
! | 14 | 15  9  3 |(10) 2  8 |  2  8  9  3 |         |\  5  /|       
! | 15 | 16 11  4 |(12) 3 10 |  3 10 11  4 |         | \   / |      
! | 16 | 17 13  1 |( 6) 4 12 |  4 12 13  1 |         |9 \ /13|    
! +----+----------+----------+-------------+         |  14   |    
! | 17 | 14  6  7 |  -  2  1 |  2  1  6  7 |         |  / \  |   
! | 18 | 15  8  9 |  -  3  2 |  3  2  8  9 |         | /   \ |  
! | 19 | 16 10 11 |  -  4  3 |  4  3 10 11 |         |/ 17  \| 
! | 20 | 17 12 13 |  -  1  4 |  1  4 12 13 |         6-------7
! +----+----------+----------+-------------+
      DATA IC / 
     1    5, 1, 2 , 14, 3, 4 ,  3, 4, 1, 2, 
     2    5, 2, 3 , 15, 4, 1 ,  4, 1, 2, 3,
     3    5, 3, 4 , 16, 1, 2 ,  1, 2, 3, 4,
     4    5, 4, 1 , 17, 2, 3 ,  2, 3, 4, 1,
     5   14, 2, 1 ,  5, 6, 7 ,  6, 7, 2, 1,
     6   15, 3, 2 ,  5, 8, 9 ,  8, 9, 3, 2,
     7   16, 4, 3 ,  5,10,11 , 10,11, 4, 3,
     8   17, 1, 4 ,  5,12,13 , 12,13, 1, 4,
     9   14, 1, 6 ,  0, 7, 2 ,  7, 2, 1, 6,
     .   15, 2, 8 ,  0, 9, 3 ,  9, 3, 2, 8,
     1   16, 3,10 ,  0,11, 4 , 11, 4, 3,10,
     2   17, 4,12 ,  0,13, 1 , 13, 1, 4,12,
     3   14, 7, 2 ,  0, 1, 6 ,  1, 6, 7, 2,
     4   15, 9, 3 ,  0, 2, 8 ,  2, 8, 9, 3,
     5   16,11, 4 ,  0, 3,10 ,  3,10,11, 4,
     6   17,13, 1 ,  0, 4,12 ,  4,12,13, 1,
     7   14, 6, 7 ,  0, 2, 1 ,  2, 1, 6, 7,
     8   15, 8, 9 ,  0, 3, 2 ,  3, 2, 8, 9,
     9   16,10,11 ,  0, 4, 3 ,  4, 3,10,11,
     .   17,12,13 ,  0, 1, 4 ,  1, 4,12,13/

! +------+-------------------------------------------+
! |      |       sous-triangle voisins               |
! |sous  +----------+----------+---------------------+
! |trian |       n3/=n4        |             n3=n4   |
! |      +----------+----------+----------+----------+
! |      |   IT0    | 2,4,6,8=0|          | 2,4,6,8=0|
! +------+----------+----------+----------+----------+
! |   1  |  5  2  4 |  5  2  4 |  5  6  8 |  5  6  8 |
! |   2  |  6  3  1 |  6  3  1 |  -  -  - |  -  -  - |
! |   3  |  7  4  2 |  7  4  2 |  -  -  - |  -  -  - |
! |   4  |  8  1  3 |  8  1  3 |  -  -  - |  -  -  - |
! +------+----------+----------+----------+----------+
! |   5  |  1  9 13 |  1 -1 -1 |  1  9 13 |  1 -1 -1 |
! |   6  |  2 10 14 |  2 -1 -1 |  1 10 14 |  1 -1 -1 |
! |   7  |  3 11 15 |  3 -1 -1 |  -  -  - |  -  -  - |
! |   8  |  4 12 16 |  4 -1 -1 |  1 12 16 |  1 -1 -1 |
! +------+----------+----------+----------+----------+
! |   9  | -1 17  5 |  -  -  - | -1 17  5 |  -  -  - |
! |  10  | -1 18  6 |  -  -  - | -1 18  6 |  -  -  - |
! |  11  | -1 19  7 |  -  -  - |  -  -  - |  -  -  - |
! |  12  | -1 20  8 |  -  -  - | -1 20  8 |  -  -  - |
! +------+----------+----------+----------+----------+
! |  13  | -1  5 17 |  -  -  - | -1  5 17 |  -  -  - |
! |  14  | -1  6 18 |  -  -  - | -1  6 18 |  -  -  - |
! |  15  | -1  7 19 |  -  -  - |  -  -  - |  -  -  - |
! |  16  | -1  8 20 |  -  -  - | -1  8 20 |  -  -  - |
! +------+----------+----------+----------+----------+
! |  17  | -1  9 13 |  -  -  - | -1  9 13 |  -  -  - |
! |  18  | -1 10 14 |  -  -  - | -1 10 14 |  -  -  - |
! |  19  | -1 11 15 |  -  -  - |  -  -  - |  -  -  - |
! |  20  | -1 12 16 |  -  -  - | -1 12 16 |  -  -  - |
! +------+----------+----------+----------+----------+
      DATA IT0 / 
     1   5, 2, 4,
     2   6, 3, 1,
     3   7, 4, 2,
     4   8, 1, 3,
     5   1, 9,13,
     6   2,10,14,
     7   3,11,15,
     8   4,12,16,
     9  -1,17, 5,
     .  -1,18, 6,
     1  -1,19, 7,
     2  -1,20, 8,
     3  -1, 5,17,
     4  -1, 6,18,
     5  -1, 7,19,
     6  -1, 8,20,
     7  -1, 9,13,
     8  -1,10,14,
     9  -1,11,15,
     .  -1,12,16/
C-----------------------------------------------
c      if(nft==45952)then
c        ibug=1
c      endif
      NOR_OLD = 0  
      UNP   = ONE + EM4
      ZEROM = ZERO - EM4
      EPS = (TWO+HALF)/HUNDRED
      UNPT   = ONE + EPS
      ZEROMT = ZERO - EPS
C------used for high penetration case (extension criterion)
      EPS2=EPS*EPS
C------0.1%----      
      EPS = EM3
      EPS02=EPS*EPS
      EPS = EPS0
      OVERW0=ONE
      MARGE025=FOURTH*MARGE
C--- 
C--- TOL_INTS,TOL_B : tols for intersection w/ tiny shell, for returning old seg w/ higher pene
      IF (IRESP==1) THEN
       TOL_INTS = TWO*EM3
       TOL_B = TWO*EM6
      ELSE
       TOL_INTS = EM08
       TOL_B = EM08
      END IF
      NOD1=0
      NOD2=0
c      write(iout,*)'t=',tt,'i24dst3 1'
c      if (ncycle>=0) ip=1
c      if (ncycle<100) write(iout,*)'JLT,ncycle=',JLT,ncycle
      DO I=1,JLT
        N=CAND_N(I)
c       IRTLM(1,N)>0 node previously impacted on global MAIN IRTLM(1,N)
c          treat one contact but no new intersection
c       IRTLM(1,N)<0 node currently impacted on global MAIN -IRTLM(1,N)
c          treat multiple contact during one cycle
c       IRTLM(1,N)==0 node not impacted 
        IPEN0(I)=0 
        ICONT_R(I) = 0        
        IF(N <= NSN)THEN
          ICONTACT(I) = IRTLM(1,N)
          SUBTRIA(I)=IRTLM(2,N)
        ELSE
          ICONTACT(I) = IRTLM_FI(NIN)%P(1,N-NSN)
          SUBTRIA(I)  = IRTLM_FI(NIN)%P(2,N-NSN)
        ENDIF
        IRTLM_OLD(I) = ICONTACT(I)

        IF(ICONTACT(I) <= ZERO)THEN
         ISTEP(I) = 2
         ICONTACT(I) = 0
        ELSEIF(ICONTACT(I) == MSEGLO(CAND_E(I)))THEN
         ISTEP(I) = 1
        ELSE
         ICONTACT(I) = 0
         ISTEP(I) = 0
        ENDIF
        IF(STIF(I) == ZERO)THEN
          ICONTACT(I) = 0
          ISTEP(I) = 0
        ENDIF
        IF(N <= NSN)THEN
          IF (PENE_OLD(5,N)> ZERO.AND.ISTEP(I) == 1) IPEN0(I)=1
        ELSE
          IF (PENE_OLDFI(NIN)%P(5,N-NSN)> ZERO.AND.ISTEP(I) == 1) IPEN0(I)=1
        ENDIF
C-----special Case plan/plan         
        IF(ISTEP(I) == 1.AND.SUBTRIA(I) > 20) THEN
          SUBTRIA(I)  = SUBTRIA(I) - 20
          ISTEP(I) = 6
        ENDIF
        IF(IEDGE/=0)THEN
          IF(CAND_T(I)==2)THEN
c CAND_T(I)==2 => only edge candidate
            ICONTACT(I) = 0
            ISTEP(I) = 0
          ENDIF
        ENDIF
        NM1(I) = ZERO
        NM2(I) = ZERO
        NM3(I) = ZERO
      ENDDO
C--------------------------------------------------------
C   ISTEP
c      0 : rien  faire (>STIF=0...)
c      1 : calcul Hi,pene... si ancien contact
c                             si change de segment ou soustrianle st => ISTEP=3
c                             sinon (ISTEP=0) plus rien  faire
c      2 : algo d'intersection si pas de contact prcdent
c                             si intersection => ISTEP=3
c                             sinon plus rien  faire
c      3 : algo de glissement
c                             si ne glisse pas                => ISTEP=5 
c                             si glisse mais pas hors surface => ISTEP=4 
c                             si glisse hors surface          => ISTEP=0 plus rien  faire
c      4 : algo de glissement !!!! not used
c                             si ne glisse pas hors surface => ISTEP=5 
c                             si glisse mais pas hors surface => ISTEP=4 
c                             si glisse hors surface          => ISTEP=0 plus rien  faire
c      5 : calcul Hi,pene ... pour nouveau contact ou ancien avec glissement
c      
c      6 : special shell plane/shell plance case: treated by node/edge(line) with Iedge=1
C--------------------------------------------------------

C--------------------------------------------------------
C     0 common initialization node 1 to 5
C--------------------------------------------------------

      DO I=1,JLT
       ISHEL(I)=0
       IF(ISTEP(I) == 0)CYCLE

c      write(iout,*)'i=',i,'i24dst3 21'
       XO14 = XX0(I,14)
       YO14 = YY0(I,14)
       ZO14 = ZZ0(I,14)

       XO15 = XX0(I,15)
       YO15 = YY0(I,15)
       ZO15 = ZZ0(I,15)

       XO16 = XX0(I,16)
       YO16 = YY0(I,16)
       ZO16 = ZZ0(I,16)

       XO17 = XX0(I,17)
       YO17 = YY0(I,17)
       ZO17 = ZZ0(I,17)

       X51 = XX0(I,1) - XX0(I,5)
       Y51 = YY0(I,1) - YY0(I,5)
       Z51 = ZZ0(I,1) - ZZ0(I,5)
 
       X52 = XX0(I,2) - XX0(I,5)
       Y52 = YY0(I,2) - YY0(I,5)
       Z52 = ZZ0(I,2) - ZZ0(I,5)
 
       X53 = XX0(I,3) - XX0(I,5)
       Y53 = YY0(I,3) - YY0(I,5)
       Z53 = ZZ0(I,3) - ZZ0(I,5)
 
       X54 = XX0(I,4) - XX0(I,5)
       Y54 = YY0(I,4) - YY0(I,5)
       Z54 = ZZ0(I,4) - ZZ0(I,5)
 

       SX1250(I) = Y51*Z52 - Z51*Y52
       SY1250(I) = Z51*X52 - X51*Z52
       SZ1250(I) = X51*Y52 - Y51*X52

       SX2350(I) = Y52*Z53 - Z52*Y53
       SY2350(I) = Z52*X53 - X52*Z53
       SZ2350(I) = X52*Y53 - Y52*X53

       SX3450(I) = Y53*Z54 - Z53*Y54
       SY3450(I) = Z53*X54 - X53*Z54
       SZ3450(I) = X53*Y54 - Y53*X54

       SX4150(I) = Y54*Z51 - Z54*Y51
       SY4150(I) = Z54*X51 - X54*Z51
       SZ4150(I) = X54*Y51 - Y54*X51


       X141 = XX0(I,1) - XX0(I,14)
       Y141 = YY0(I,1) - YY0(I,14)
       Z141 = ZZ0(I,1) - ZZ0(I,14)
 
       X142 = XX0(I,2) - XX0(I,14)
       Y142 = YY0(I,2) - YY0(I,14)
       Z142 = ZZ0(I,2) - ZZ0(I,14)
 
       X152 = XX0(I,2) - XX0(I,15)
       Y152 = YY0(I,2) - YY0(I,15)
       Z152 = ZZ0(I,2) - ZZ0(I,15)
 
       X153 = XX0(I,3) - XX0(I,15)
       Y153 = YY0(I,3) - YY0(I,15)
       Z153 = ZZ0(I,3) - ZZ0(I,15)
 
       X163 = XX0(I,3) - XX0(I,16)
       Y163 = YY0(I,3) - YY0(I,16)
       Z163 = ZZ0(I,3) - ZZ0(I,16)
 
       X164 = XX0(I,4) - XX0(I,16)
       Y164 = YY0(I,4) - YY0(I,16)
       Z164 = ZZ0(I,4) - ZZ0(I,16)

       X174 = XX0(I,4) - XX0(I,17)
       Y174 = YY0(I,4) - YY0(I,17)
       Z174 = ZZ0(I,4) - ZZ0(I,17)

       X171 = XX0(I,1) - XX0(I,17)
       Y171 = YY0(I,1) - YY0(I,17)
       Z171 = ZZ0(I,1) - ZZ0(I,17)


       X13_2 = XX0(I,2) - XX0(I,13)
       Y13_2 = YY0(I,2) - YY0(I,13)
       Z13_2 = ZZ0(I,2) - ZZ0(I,13)

       X7_3  = XX0(I,3) - XX0(I,7)
       Y7_3  = YY0(I,3) - YY0(I,7)
       Z7_3  = ZZ0(I,3) - ZZ0(I,7)

       X9_4  = XX0(I,4) - XX0(I,9)
       Y9_4  = YY0(I,4) - YY0(I,9)
       Z9_4  = ZZ0(I,4) - ZZ0(I,9)

       X11_1 = XX0(I,1) - XX0(I,11)
       Y11_1 = YY0(I,1) - YY0(I,11)
       Z11_1 = ZZ0(I,1) - ZZ0(I,11)

       X6_4  = XX0(I,4) - XX0(I,6)
       Y6_4  = YY0(I,4) - YY0(I,6)
       Z6_4  = ZZ0(I,4) - ZZ0(I,6)

       X8_1  = XX0(I,1) - XX0(I,8)
       Y8_1  = YY0(I,1) - YY0(I,8)
       Z8_1  = ZZ0(I,1) - ZZ0(I,8)

       X10_2 = XX0(I,2) - XX0(I,10)
       Y10_2 = YY0(I,2) - YY0(I,10)
       Z10_2 = ZZ0(I,2) - ZZ0(I,10)

       X12_3 = XX0(I,3) - XX0(I,12)
       Y12_3 = YY0(I,3) - YY0(I,12)
       Z12_3 = ZZ0(I,3) - ZZ0(I,12)

c      write(iout,*)'i=',i,'i24dst3 22'

       IF(MVOISIN(1,CAND_E(I))/=0)THEN       
         SX21140(I) = Y142*Z141 - Z142*Y141  
         SY21140(I) = Z142*X141 - X142*Z141  
         SZ21140(I) = X142*Y141 - Y142*X141  
       ELSE                                  
         SX21140(I) = SX1250(I)              
         SY21140(I) = SY1250(I)              
         SZ21140(I) = SZ1250(I)              
       ENDIF                                 

c      write(iout,*)'i=',i,'i24dst3 23'
       IF(IXX(I,3) /= IXX(I,4))THEN
         IF(MVOISIN(2,CAND_E(I))/=0)THEN
           SX32150(I) = Y153*Z152 - Z153*Y152
           SY32150(I) = Z153*X152 - X153*Z152
           SZ32150(I) = X153*Y152 - Y153*X152
         ELSE
           SX32150(I) = SX2350(I)
           SY32150(I) = SY2350(I)
           SZ32150(I) = SZ2350(I)
         ENDIF

         IF(MVOISIN(3,CAND_E(I))/=0)THEN
           SX43160(I) = Y164*Z163 - Z164*Y163
           SY43160(I) = Z164*X163 - X164*Z163
           SZ43160(I) = X164*Y163 - Y164*X163
         ELSE
           SX43160(I) = SX3450(I)
           SY43160(I) = SY3450(I)
           SZ43160(I) = SZ3450(I)
         ENDIF

         IF(MVOISIN(4,CAND_E(I))/=0)THEN
           SX14170(I) = Y171*Z174 - Z171*Y174
           SY14170(I) = Z171*X174 - X171*Z174
           SZ14170(I) = X171*Y174 - Y171*X174
         ELSE
           SX14170(I) = SX4150(I)
           SY14170(I) = SY4150(I)
           SZ14170(I) = SZ4150(I)
         ENDIF


         NXX(I,1) = Y13_2 * Z6_4  - Z13_2 * Y6_4
         NYY(I,1) = Z13_2 * X6_4  - X13_2 * Z6_4
         NZZ(I,1) = X13_2 * Y6_4  - Y13_2 * X6_4

         NXX(I,2) = Y7_3  * Z8_1  - Z7_3  * Y8_1
         NYY(I,2) = Z7_3  * X8_1  - X7_3  * Z8_1
         NZZ(I,2) = X7_3  * Y8_1  - Y7_3  * X8_1

         NXX(I,3) = Y9_4  * Z10_2 - Z9_4  * Y10_2
         NYY(I,3) = Z9_4  * X10_2 - X9_4  * Z10_2
         NZZ(I,3) = X9_4  * Y10_2 - Y9_4  * X10_2

         NXX(I,4) = Y11_1 * Z12_3 - Z11_1 * Y12_3
         NYY(I,4) = Z11_1 * X12_3 - X11_1 * Z12_3
         NZZ(I,4) = X11_1 * Y12_3 - Y11_1 * X12_3

         NXX(I,5) = FOURTH*(NXX(I,1)+NXX(I,2)+NXX(I,3)+NXX(I,4))
         NYY(I,5) = FOURTH*(NYY(I,1)+NYY(I,2)+NYY(I,3)+NYY(I,4))
         NZZ(I,5) = FOURTH*(NZZ(I,1)+NZZ(I,2)+NZZ(I,3)+NZZ(I,4))

       ELSE

         IF(MVOISIN(2,CAND_E(I))/=0)THEN
           SX32150(I) = Y153*Z152 - Z153*Y152
           SY32150(I) = Z153*X152 - X153*Z152
           SZ32150(I) = X153*Y152 - Y153*X152
         ELSE
           SX32150(I) = SX1250(I)
           SY32150(I) = SY1250(I)
           SZ32150(I) = SZ1250(I)
         ENDIF

         IF(MVOISIN(3,CAND_E(I))/=0)THEN
           SX43160(I) = Y164*Z163 - Z164*Y163
           SY43160(I) = Z164*X163 - X164*Z163
           SZ43160(I) = X164*Y163 - Y164*X163
         ELSE
           SX43160(I) = SX1250(I)
           SY43160(I) = SY1250(I)
           SZ43160(I) = SZ1250(I)
         ENDIF

         IF(MVOISIN(4,CAND_E(I))/=0)THEN
           SX14170(I) = Y171*Z174 - Z171*Y174
           SY14170(I) = Z171*X174 - X171*Z174
           SZ14170(I) = X171*Y174 - Y171*X174
         ELSE
           SX14170(I) = SX1250(I)
           SY14170(I) = SY1250(I)
           SZ14170(I) = SZ1250(I)       
         ENDIF

         NXX(I,1) =SX1250(I)+SX14170(I)+SX21140(I)
         NYY(I,1) =SY1250(I)+SY14170(I)+SY21140(I)
         NZZ(I,1) =SZ1250(I)+SZ14170(I)+SZ21140(I)

         NXX(I,2) =SX1250(I)+SX21140(I)+SX32150(I)
         NYY(I,2) =SY1250(I)+SY21140(I)+SY32150(I)
         NZZ(I,2) =SZ1250(I)+SZ21140(I)+SZ32150(I)

         NXX(I,3) =SX1250(I)+SX32150(I)+SX14170(I)
         NYY(I,3) =SY1250(I)+SY32150(I)+SY14170(I)
         NZZ(I,3) =SZ1250(I)+SZ32150(I)+SZ14170(I)

         NXX(I,4) =NXX(I,3)
         NYY(I,4) =NYY(I,3)
         NZZ(I,4) =NZZ(I,3)

         NXX(I,5) =NXX(I,3)
         NYY(I,5) =NYY(I,3)
         NZZ(I,5) =NZZ(I,3)

       ENDIF

c       GAP : coordinates correction

       N=CAND_N(I)
       L=CAND_E(I)
c       IF(GAPS(I)+GAP_M(CAND_E(I))
c     .         +GAP_NM(5,CAND_E(I))+GAP_NM(6,CAND_E(I))
c     .         +GAP_NM(7,CAND_E(I))+GAP_NM(8,CAND_E(I))
c     .         +GAP_NM(9,CAND_E(I))+GAP_NM(10,CAND_E(I))
c     .        +GAP_NM(11,CAND_E(I))+GAP_NM(12,CAND_E(I)) /= ZERO)THEN
       IF(GAPS(I)>ZERO.OR.GAP_M(L)>ZERO) THEN
         ISHEL(I)=1
       ELSEIF(GAP_NM(5,L)>ZERO.OR.GAP_NM(6,L)>ZERO.OR.GAP_NM(7,L)>ZERO) THEN
         ISHEL(I)=1
       ELSEIF(GAP_NM(8,L)>ZERO.OR.GAP_NM(9,L)>ZERO.OR.GAP_NM(10,L)>ZERO) THEN
         ISHEL(I)=1
       ELSEIF(GAP_NM(11,L)>ZERO.OR.GAP_NM(12,L)>ZERO) THEN
         ISHEL(I)=1
       END IF
C------taking into account Igap0=1      
       IF(GAPS(I)>=ZERO.AND.GAP_M(L)>ZERO) ISHEL(I)=2
       IF(ISHEL(I)>0) THEN
         NX1 = NXX(I,1)
         NY1 = NYY(I,1)
         NZ1 = NZZ(I,1)
         AAA = GAPS(I)+GAP_NM(1,L)
         AAA = AAA/SQRT(NX1*NX1+NY1*NY1+NZ1*NZ1)
         NX1 = NX1 * AAA
         NY1 = NY1 * AAA
         NZ1 = NZ1 * AAA

         NX2 = NXX(I,2)
         NY2 = NYY(I,2)
         NZ2 = NZZ(I,2)
         AAA = GAPS(I)+GAP_NM(2,L)
         AAA = AAA/SQRT(NX2*NX2+NY2*NY2+NZ2*NZ2)
         NX2 = NX2 * AAA
         NY2 = NY2 * AAA
         NZ2 = NZ2 * AAA

         IF(IXX(I,3) /= IXX(I,4))THEN
           NX3 = NXX(I,3)
           NY3 = NYY(I,3)
           NZ3 = NZZ(I,3)
           AAA = GAPS(I)+GAP_NM(3,L)
           AAA = AAA/SQRT(NX3*NX3+NY3*NY3+NZ3*NZ3)
           NX3 = NX3 * AAA
           NY3 = NY3 * AAA
           NZ3 = NZ3 * AAA

           NX4 = NXX(I,4)
           NY4 = NYY(I,4)
           NZ4 = NZZ(I,4)
           AAA = GAPS(I)+GAP_NM(4,L)
           AAA = AAA/SQRT(NX4*NX4+NY4*NY4+NZ4*NZ4)
           NX4 = NX4 * AAA
           NY4 = NY4 * AAA
           NZ4 = NZ4 * AAA

           XX(I,1) = XX0(I,1) + NX1
           YY(I,1) = YY0(I,1) + NY1
           ZZ(I,1) = ZZ0(I,1) + NZ1

           XX(I,2) = XX0(I,2) + NX2
           YY(I,2) = YY0(I,2) + NY2
           ZZ(I,2) = ZZ0(I,2) + NZ2

           XX(I,3) = XX0(I,3) + NX3
           YY(I,3) = YY0(I,3) + NY3
           ZZ(I,3) = ZZ0(I,3) + NZ3

           XX(I,4) = XX0(I,4) + NX4
           YY(I,4) = YY0(I,4) + NY4
           ZZ(I,4) = ZZ0(I,4) + NZ4

           XX(I,5) = FOURTH*(XX(I,1)+XX(I,2)+XX(I,3)+XX(I,4))
           YY(I,5) = FOURTH*(YY(I,1)+YY(I,2)+YY(I,3)+YY(I,4))
           ZZ(I,5) = FOURTH*(ZZ(I,1)+ZZ(I,2)+ZZ(I,3)+ZZ(I,4))
           IF(IXX(I,10)==IXX(I,11))THEN
             XX(I,16) = XX0(I,10)
             YY(I,16) = YY0(I,10)
             ZZ(I,16) = ZZ0(I,10)
           ELSE
             XX(I,16) = FOURTH*(XX0(I,4)+XX0(I,3)+XX0(I,10)+XX0(I,11))
             YY(I,16) = FOURTH*(YY0(I,4)+YY0(I,3)+YY0(I,10)+YY0(I,11))
             ZZ(I,16) = FOURTH*(ZZ0(I,4)+ZZ0(I,3)+ZZ0(I,10)+ZZ0(I,11))
           ENDIF
           NX16 = SX43160(I)
           NY16 = SY43160(I)
           NZ16 = SZ43160(I)
           AAA = GAPS(I)+FOURTH*(
     .             GAP_NM(3,L)+GAP_NM(4,L)
     .           + GAP_NM(9,L)+GAP_NM(10,L) )
           AAA = AAA/SQRT(NX16*NX16+NY16*NY16+NZ16*NZ16)
           XX(I,16) = XX(I,16) + NX16 * AAA
           YY(I,16) = YY(I,16) + NY16 * AAA
           ZZ(I,16) = ZZ(I,16) + NZ16 * AAA
         ELSE
           NX3 = NXX(I,3)
           NY3 = NYY(I,3)
           NZ3 = NZZ(I,3)
           AAA = GAPS(I)+GAP_NM(3,L)
           AAA = AAA/SQRT(NX3*NX3+NY3*NY3+NZ3*NZ3)
           NX3 = NX3 * AAA
           NY3 = NY3 * AAA
           NZ3 = NZ3 * AAA

           NX4 = NXX(I,4)
           NY4 = NYY(I,4)
           NZ4 = NZZ(I,4)

           XX(I,1) = XX0(I,1) + NX1
           YY(I,1) = YY0(I,1) + NY1
           ZZ(I,1) = ZZ0(I,1) + NZ1

           XX(I,2) = XX0(I,2) + NX2
           YY(I,2) = YY0(I,2) + NY2
           ZZ(I,2) = ZZ0(I,2) + NZ2

           XX(I,3) = XX0(I,3) + NX3
           YY(I,3) = YY0(I,3) + NY3
           ZZ(I,3) = ZZ0(I,3) + NZ3

           XX(I,4) =  XX(I,3)
           YY(I,4) =  YY(I,3)
           ZZ(I,4) =  ZZ(I,3)

           XX(I,5) =  XX(I,3)
           YY(I,5) =  YY(I,3)
           ZZ(I,5) =  ZZ(I,3)
         ENDIF
         IF(IXX(I,6) == IXX(I,7))THEN
           XX(I,14) = XX0(I,6)
           YY(I,14) = YY0(I,6)
           ZZ(I,14) = ZZ0(I,6)
         ELSE
           XX(I,14) = FOURTH*(XX0(I,2)+XX0(I,1)+XX0(I,6)+XX0(I,7))
           YY(I,14) = FOURTH*(YY0(I,2)+YY0(I,1)+YY0(I,6)+YY0(I,7))
           ZZ(I,14) = FOURTH*(ZZ0(I,2)+ZZ0(I,1)+ZZ0(I,6)+ZZ0(I,7))
         ENDIF
         NX14 = SX21140(I)
         NY14 = SY21140(I)
         NZ14 = SZ21140(I)
         AAA = GAPS(I)+FOURTH*(
     .           GAP_NM(1,L)+GAP_NM(2,L)
     .         + GAP_NM(5,L)+GAP_NM(6,L) )
         AAA = AAA/SQRT(NX14*NX14+NY14*NY14+NZ14*NZ14)
         XX(I,14) = XX(I,14) + NX14 * AAA
         YY(I,14) = YY(I,14) + NY14 * AAA
         ZZ(I,14) = ZZ(I,14) + NZ14 * AAA
         IF(IXX(I,8)==IXX(I,9))THEN
           XX(I,15) = XX0(I,8)
           YY(I,15) = YY0(I,8)
           ZZ(I,15) = ZZ0(I,8)
         ELSE
           XX(I,15) = FOURTH*(XX0(I,3)+XX0(I,2)+XX0(I,8)+XX0(I,9))
           YY(I,15) = FOURTH*(YY0(I,3)+YY0(I,2)+YY0(I,8)+YY0(I,9))
           ZZ(I,15) = FOURTH*(ZZ0(I,3)+ZZ0(I,2)+ZZ0(I,8)+ZZ0(I,9))
         ENDIF
         NX15 = SX32150(I)
         NY15 = SY32150(I)
         NZ15 = SZ32150(I)
         AAA = GAPS(I)+FOURTH*(
     .           GAP_NM(2,L)+GAP_NM(3,L)
     .         + GAP_NM(7,L)+GAP_NM(8,L) )
         AAA = AAA/SQRT(NX15*NX15+NY15*NY15+NZ15*NZ15)
         XX(I,15) = XX(I,15) + NX15 * AAA
         YY(I,15) = YY(I,15) + NY15 * AAA
         ZZ(I,15) = ZZ(I,15) + NZ15 * AAA
         IF(IXX(I,12)==IXX(I,13))THEN
           XX(I,17) = XX0(I,12)
           YY(I,17) = YY0(I,12)
           ZZ(I,17) = ZZ0(I,12)
         ELSE
           XX(I,17) = FOURTH*(XX0(I,1)+XX0(I,4)+XX0(I,12)+XX0(I,13))
           YY(I,17) = FOURTH*(YY0(I,1)+YY0(I,4)+YY0(I,12)+YY0(I,13))
           ZZ(I,17) = FOURTH*(ZZ0(I,1)+ZZ0(I,4)+ZZ0(I,12)+ZZ0(I,13))
         ENDIF
         NX17 = SX14170(I)
         NY17 = SY14170(I)
         NZ17 = SZ14170(I)
         AAA = GAPS(I)+FOURTH*(
     .           GAP_NM(4,L)+GAP_NM(1,L)
     .         + GAP_NM(11,L)+GAP_NM(12,L) )
         AAA = AAA/SQRT(NX17*NX17+NY17*NY17+NZ17*NZ17)
         XX(I,17) = XX(I,17) + NX17 * AAA
         YY(I,17) = YY(I,17) + NY17 * AAA
         ZZ(I,17) = ZZ(I,17) + NZ17 * AAA
 
         X51 = XX(I,1) - XX(I,5)
         Y51 = YY(I,1) - YY(I,5)
         Z51 = ZZ(I,1) - ZZ(I,5)
      
         X52 = XX(I,2) - XX(I,5)
         Y52 = YY(I,2) - YY(I,5)
         Z52 = ZZ(I,2) - ZZ(I,5)
      
         X53 = XX(I,3) - XX(I,5)
         Y53 = YY(I,3) - YY(I,5)
         Z53 = ZZ(I,3) - ZZ(I,5)
      
         X54 = XX(I,4) - XX(I,5)
         Y54 = YY(I,4) - YY(I,5)
         Z54 = ZZ(I,4) - ZZ(I,5)
      

         SX125(I) = Y51*Z52 - Z51*Y52
         SY125(I) = Z51*X52 - X51*Z52
         SZ125(I) = X51*Y52 - Y51*X52

         SX235(I) = Y52*Z53 - Z52*Y53
         SY235(I) = Z52*X53 - X52*Z53
         SZ235(I) = X52*Y53 - Y52*X53

         SX345(I) = Y53*Z54 - Z53*Y54
         SY345(I) = Z53*X54 - X53*Z54
         SZ345(I) = X53*Y54 - Y53*X54

         SX415(I) = Y54*Z51 - Z54*Y51
         SY415(I) = Z54*X51 - X54*Z51
         SZ415(I) = X54*Y51 - Y54*X51

       ELSE
         
         XX(I,1:17) = XX0(I,1:17)
         YY(I,1:17) = YY0(I,1:17)
         ZZ(I,1:17) = ZZ0(I,1:17)

         SX125(I)=SX1250(I)
         SY125(I)=SY1250(I)
         SZ125(I)=SZ1250(I)
         
         SX235(I) = SX2350(I)
         SY235(I) = SY2350(I)
         SZ235(I) = SZ2350(I)
         
         SX345(I) = SX3450(I)
         SY345(I) = SY3450(I)
         SZ345(I) = SZ3450(I)

         SX415(I) = SX4150(I)
         SY415(I) = SY4150(I)
         SZ415(I) = SZ4150(I)
       ENDIF
c      write(iout,*)'i=',i,'i24dst3 212'

      ENDDO
C--------------------------------------------------------
C     1 Previous contact compute Hi pene ...
C--------------------------------------------------------
c      write(iout,*)'jlt=',jlt,'i24dst3 3'

      DO I=1,JLT
       PENE(I) = ZERO
       PENE_TM(I) = ZERO
       IF(ISTEP(I) == 0)CYCLE
       IF(ISTEP(I) /= 1.AND.TT /= ZERO)CYCLE
        
C
        IF (TT==ZERO) THEN
         ITQ = 1
        ELSE
         ITQ = SUBTRIA(I)
        END IF
        K = IC(1,ITQ)
        XA(I) = XX(I,K)   
        YA(I) = YY(I,K)    
        ZA(I) = ZZ(I,K) 

        K = IC(2,ITQ)
        XB(I) = XX(I,K)   
        YB(I) = YY(I,K)    
        ZB(I) = ZZ(I,K)   

        K = IC(3,ITQ)
        XC(I) = XX(I,K)   
        YC(I) = YY(I,K)    
        ZC(I) = ZZ(I,K)   

        K = IC(4,ITQ)
        XD(I) = XX(I,K)   
        YD(I) = YY(I,K)    
        ZD(I) = ZZ(I,K)   

        K = 15 ! only used for triangles
        XE(I) = XX(I,K)   
        YE(I) = YY(I,K)    
        ZE(I) = ZZ(I,K)   

        K = 17 ! only used for triangles
        XF(I) = XX(I,K)   
        YF(I) = YY(I,K)    
        ZF(I) = ZZ(I,K)   

        IF(ITQ ==1)THEN
         NQX = SX125(I)
         NQY = SY125(I)
         NQZ = SZ125(I)
        ELSEIF(ITQ ==2)THEN
         NQX = SX235(I)
         NQY = SY235(I)
         NQZ = SZ235(I)
        ELSEIF(ITQ ==3)THEN
         NQX = SX345(I)
         NQY = SY345(I)
         NQZ = SZ345(I)
        ELSEIF(ITQ ==4)THEN
         NQX = SX415(I)
         NQY = SY415(I)
         NQZ = SZ415(I)
        ENDIF
        
        S2 = MAX(EM30,SQRT(NQX*NQX + NQY*NQY + NQZ*NQZ))
        AREA(I) = S2
        AAA = ONE/S2
        NQX = NQX*AAA
        NQY = NQY*AAA
        NQZ = NQZ*AAA

        BBB = (XI(I)-XA(I))*NQX+(YI(I)-YA(I))*NQY+(ZI(I)-ZA(I))*NQZ
C------Add Tiny contact here (replace the starter part)        
        IF (TT == ZERO .AND. ISTEP(I)/= 1) THEN
         IF (ABS(BBB) < PENMIN.AND.BBB/=ZERO) THEN
C--------------if SECONDARY (projected) is inside segment         
          CALL S_IN_M4(XX(I,1),YY(I,1),ZZ(I,1),XX(I,2),YY(I,2),ZZ(I,2),
     1                 XX(I,3),YY(I,3),ZZ(I,3),XX(I,4),YY(I,4),ZZ(I,4),
     1                 XI(I),YI(I),ZI(I),IER)
          IF (IER ==0) THEN
           PENE(I) = ABS(BBB)
           N=CAND_N(I)
#include "lockon.inc"
           IF(N <= NSN)THEN
            IF(IRTLM(1,N)==0)THEN
              IRTLM(1,N)=-MSEGLO(CAND_E(I))
              IRTLM(2,N)=1
              TIME_S(N) = PENE(I)
            ELSE
               IF(  TIME_S(N)<PENE(I) .OR.
     *              (TIME_S(N)==PENE(I).AND.-IRTLM(1,N) < MSEGLO(CAND_E(I)) ) )THEN
                     IRTLM(1,N)=-MSEGLO(CAND_E(I))
                     IRTLM(2,N)=1
                     TIME_S(N) = PENE(I)
               ENDIF
            ENDIF 
           ELSE
            IF(IRTLM_FI(NIN)%P(1,N-NSN) == 0)THEN 
               IRTLM_FI(NIN)%P(1,N-NSN)=-MSEGLO(CAND_E(I))
               IRTLM_FI(NIN)%P(2,N-NSN)=1
               TIME_SFI(NIN)%P(N-NSN) = PENE(I)
            ELSE
               IF(  TIME_SFI(NIN)%P(N-NSN)<PENE(I) .OR.
     *              (TIME_SFI(NIN)%P(N-NSN)==PENE(I).AND.-IRTLM_FI(NIN)%P(1,N-NSN) < MSEGLO(CAND_E(I)) )  )THEN
                     IRTLM_FI(NIN)%P(1,N-NSN)=-MSEGLO(CAND_E(I))
                     IRTLM_FI(NIN)%P(2,N-NSN)=1
                     TIME_SFI(NIN)%P(N-NSN) = PENE(I)
               ENDIF
            ENDIF
           ENDIF
#include "lockoff.inc"
           ISTEP(I)= 1
           SUBTRIA(I) =1
          ELSE
           ISTEP(I)= 0
           CYCLE
          END IF
         ELSE
          CYCLE
         END IF 
        ELSE
         PENE(I) = -MIN(ZERO,BBB)
        END IF
        LARGEP = 0
        IF(PENE(I)*PENE(I) > EPS2*S2.AND.IPEN0(I)==0) LARGEP = 1
        PENE_TM(I) =PENE(I)
c        PMAX_GAP = MAX(PMAX_GAP,ABS(PENE(I)))

        N1(I) = NQX
        N2(I) = NQY
        N3(I) = NQZ
        N_TM(1,I) = N1(I) 
        N_TM(2,I) = N2(I) 
        N_TM(3,I) = N3(I) 

c project triangle on SECONDARY node
        

  !                  a
  !                 / \
  !                /   \           a,b,c position at current time
  !               /   A \          A,B,C projected on SECONDARY node 
  !              /   /\  \
  !             /   /  \  \
  !            /   /    \  \    normal at point a:      
  !           /   /      \  \       Na = a - A
  !          /   /        \  \
  !         /   /          \  \
  !        /   /            \  \
  !       /   B--------------C  \
  !      /                       \
  !     b-------------------------c 
  !                  
  
        XAB = XB(I)-XA(I)
        YAB = YB(I)-YA(I)
        ZAB = ZB(I)-ZA(I)
        XBC = XC(I)-XB(I)
        YBC = YC(I)-YB(I)
        ZBC = ZC(I)-ZB(I)
        XCA = XA(I)-XC(I)
        YCA = YA(I)-YC(I)
        ZCA = ZA(I)-ZC(I)

        XBD = XD(I)-XB(I)
        YBD = YD(I)-YB(I)
        ZBD = ZD(I)-ZB(I)
        XCE = XE(I)-XC(I)
        YCE = YE(I)-YC(I)
        ZCE = ZE(I)-ZC(I)
        XAF = XF(I)-XA(I)
        YAF = YF(I)-YA(I)
        ZAF = ZF(I)-ZA(I)

        XIA = XA(I)-XI(I)
        YIA = YA(I)-YI(I)
        ZIA = ZA(I)-ZI(I)
        XIB = XB(I)-XI(I)
        YIB = YB(I)-YI(I)
        ZIB = ZB(I)-ZI(I)
        XIC = XC(I)-XI(I)
        YIC = YC(I)-YI(I)
        ZIC = ZC(I)-ZI(I)
        SX = - YAB*ZCA + ZAB*YCA
        SY = - ZAB*XCA + XAB*ZCA
        SZ = - XAB*YCA + YAB*XCA
        S2 = SX*SX+SY*SY+SZ*SZ
        SAX = YIB*ZIC - ZIB*YIC
        SAY = ZIB*XIC - XIB*ZIC
        SAZ = XIB*YIC - YIB*XIC
        LA(I) = (SX*SAX+SY*SAY+SZ*SAZ)/S2
        SBX = YIC*ZIA - ZIC*YIA
        SBY = ZIC*XIA - XIC*ZIA
        SBZ = XIC*YIA - YIC*XIA
        LB(I) = (SX*SBX+SY*SBY+SZ*SBZ)/S2
        LC(I) = ONE - LA(I) - LB(I)
        IZLIM=0
        SUBTRIA_TM(I) = SUBTRIA(I)
        LA_TM(I) = LA(I)
        LB_TM(I) = LB(I)
        EPSEG =EPS
        EPSEXT = EPS
C------NSNE: IEDGE4, remove all SECONDARY nodes external extension if Iedge=1        
        IF (NSNE >0) EPSEXT=EM04
        IF (ISPT2(I) >0) EPSEG=EM04
        IF(IXX(I,3) /= IXX(I,4))THEN
c------------------------------------------------------
c         quadrangles
c           check if outside side CA
          AAA = ZEROM
          IF(LB(I)<AAA)THEN
              ISTEP(I)=3
              SUBTRIA_N(I)=IT0(2,ITQ)
              SUBTRIA  (I)=IT0(2,ITQ)
              XXL(1:17)=XX(I,1:17)
              YYL(1:17)=YY(I,1:17)
              ZZL(1:17)=ZZ(I,1:17)
              IZLIM=-1
              GAP2 = GAPS(I)+GAP_M(CAND_E(I))
              CALL I24NEXTTRIA(
     1          IZLIM   ,ISTEP(I),SUBTRIA_N(I),SUBTRIA(I),
     2          LARGEP  ,PENE(I) ,XXL     ,YYL     ,ZZL     ,
     3          SX125(I),SY125(I),SZ125(I),SX235(I),SY235(I),
     4          SZ235(I),SX345(I),SY345(I),SZ345(I),SX415(I),
     5          SY415(I),SZ415(I),XI(I)   ,YI(I)   ,ZI(I)   ,
     6          N1(I)   ,N2(I)   ,N3(I)   ,LA(I)   ,LB(I)   ,
     7          LC(I)   ,GAP2    ,EPSEG   )
              SUBTRIA_TM(I) = SUBTRIA(I)
c         check if outside side AB
          ELSEIF(LC(I)<AAA)THEN
              ISTEP(I)=3
              SUBTRIA_N(I)=IT0(3,ITQ)
              XXL(1:17)=XX(I,1:17)
              YYL(1:17)=YY(I,1:17)
              ZZL(1:17)=ZZ(I,1:17)
              IZLIM=-1
              GAP2 = GAPS(I)+GAP_M(CAND_E(I))
              CALL I24NEXTTRIA(
     1          IZLIM   ,ISTEP(I),SUBTRIA_N(I),SUBTRIA(I),
     2          LARGEP  ,PENE(I) ,XXL     ,YYL     ,ZZL     ,
     3          SX125(I),SY125(I),SZ125(I),SX235(I),SY235(I),
     4          SZ235(I),SX345(I),SY345(I),SZ345(I),SX415(I),
     5          SY415(I),SZ415(I),XI(I)   ,YI(I)   ,ZI(I)   ,
     6          N1(I)   ,N2(I)   ,N3(I)   ,LA(I)   ,LB(I)   ,
     7          LC(I)   ,GAP2    ,EPSEG   )
              SUBTRIA_TM(I) = SUBTRIA(I)
c         check if outside side BC, or when there is rebound and strictly outside
          ELSEIF(LA(I)<-EPSEXT.OR.(PENE(I)==ZERO.AND.LA(I)<ZERO))THEN
c hors zone d'extension de surface
            ISTEP(I)=3
            SUBTRIA_N(I)=IT0(1,ITQ)
            IZLIM=-1
          ELSEIF(LA(I)<EPSEG)THEN
c zone limite interpolation des normales si convex
            VOL = N1(I)*XBD + N2(I)*YBD + N3(I)*ZBD
            GAP2 = GAPS(I)+GAP_M(CAND_E(I))
           IF (GAP2<EPSEG.AND.(LB(I)<EPSEG.OR.LC(I)<EPSEG))VOL=-ABS(VOL)
            IF(LARGEP == 1)THEN
c             large penetration inside +- extension zone
              ISTEP(I)=3
              SUBTRIA_N(I)=IT0(1,ITQ)
              IZLIM = -1
            ELSEIF(VOL < ZERO)THEN
c             convex
              IZLIM = 1
            ELSEIF(LA(I)<ZEROM)THEN
              ISTEP(I)=3
              SUBTRIA_N(I)=IT0(1,ITQ)
              IZLIM = -1
            ENDIF
          ENDIF
          IF(IZLIM == -1)CYCLE

        ELSE
c------------------------------------------------------
c         triangles
          IF(LA(I)<-EPSEXT.OR.(PENE(I)==ZERO.AND.LA(I)<ZERO))THEN
            ISTEP(I)=3
            SUBTRIA_N(I)=5
            IZLIM = -1
          ELSEIF(LA(I)<EPSEG)THEN
c zone limite interpolation des normales si convex
            VOL = N1(I)*XBD + N2(I)*YBD + N3(I)*ZBD
            GAP2 = GAPS(I)+GAP_M(CAND_E(I))
           IF (GAP2<EPSEG.AND.(LB(I)<EPSEG.OR.LC(I)<EPSEG))VOL=-ABS(VOL)
            IF(LARGEP == 1)THEN
c             large penetration inside +- extension zone
              ISTEP(I)=3
              SUBTRIA_N(I)=5
              IZLIM = -1
            ELSEIF(VOL < ZERO)THEN
c             convex
              IZLIM=1
            ELSEIF(LA(I)<ZEROM)THEN
              ISTEP(I)=3
              SUBTRIA_N(I)=5
              IZLIM = -1
            ENDIF
          ENDIF
          IF(LB(I)<-EPSEXT.OR.(PENE(I)==ZERO.AND.LB(I)<ZERO))THEN
            ISTEP(I)=3
            IF(LB(I) < LA(I)) SUBTRIA_N(I)=6
            IZLIM = -1
          ELSEIF(LB(I)<EPSEG)THEN
c zone limite interpolation des normales si convex
            VOL = N1(I)*XCE + N2(I)*YCE + N3(I)*ZCE
            GAP2 = GAPS(I)+GAP_M(CAND_E(I))
           IF (GAP2<EPSEG.AND.(LB(I)<EPSEG.OR.LC(I)<EPSEG))VOL=-ABS(VOL)
            IF(LARGEP == 1)THEN
c             large penetration inside +- extension zone
              ISTEP(I)=3
              IF(LB(I) < LA(I).OR.IZLIM==1) SUBTRIA_N(I)=6
              IZLIM = -1
            ELSEIF(VOL < ZERO)THEN
c             convex
              IZLIM=1
            ELSEIF(LB(I)<ZEROM)THEN
              ISTEP(I)=3
              IF(LB(I) < LA(I).OR.IZLIM==1) SUBTRIA_N(I)=6
              IZLIM = -1
            ENDIF
          ENDIF
          IF(LC(I)<-EPSEXT.OR.(PENE(I)==ZERO.AND.LC(I)<ZERO))THEN
            ISTEP(I)=3
            IF(LC(I) < LA(I).AND.LC(I) < LB(I)) SUBTRIA_N(I)=8
            IZLIM = -1
          ELSEIF(LC(I)<EPSEG)THEN
c zone limite interpolation des normales si convex
            VOL = N1(I)*XAF + N2(I)*YAF + N3(I)*ZAF
            GAP2 = GAPS(I)+GAP_M(CAND_E(I))
           IF (GAP2<EPSEG.AND.(LB(I)<EPSEG.OR.LC(I)<EPSEG))VOL=-ABS(VOL)
            IF(LARGEP == 1)THEN
c             large penetration inside +- extension zone
              ISTEP(I)=3
              IF((LC(I) < LA(I).AND.LC(I) < LB(I)).OR.IZLIM==1) SUBTRIA_N(I)=8
              IZLIM = -1
            ELSEIF(VOL < ZERO)THEN
c             convex
              IZLIM=1
            ELSEIF(LC(I)<ZEROM)THEN
              ISTEP(I)=3
              IF((LC(I) < LA(I).AND.LC(I) < LB(I)).OR.IZLIM==1) SUBTRIA_N(I)=8
              IZLIM = -1
            ENDIF
          ENDIF
          IF(IZLIM == -1)CYCLE
        ENDIF

        LA_TM(I) = LA(I)
        LB_TM(I) = LB(I)
        IF(LA(I)<ZERO)THEN
          IF(LB(I)<ZERO)THEN
            LA(I) = ZERO
            LB(I) = ZERO
            LC(I) = ONE
          ELSEIF(LC(I)<ZERO)THEN
            LC(I) = ZERO
            LA(I) = ZERO
            LB(I) = ONE
          ELSE
            LA(I) = ZERO
            AAA = LB(I) + LC(I)
            LB(I) = LB(I)/AAA
            LC(I) = LC(I)/AAA
          ENDIF
        ELSEIF(LB(I)<ZERO)THEN
          IF(LC(I)<ZERO)THEN
            LB(I) = ZERO
            LC(I) = ZERO
            LA(I) = ONE
          ELSE
            LB(I) = ZERO
            AAA = LC(I) + LA(I)
            LC(I) = LC(I)/AAA
            LA(I) = LA(I)/AAA
          ENDIF
        ELSEIF(LC(I)<ZERO)THEN
            LC(I) = ZERO
            AAA = LA(I) + LB(I)
            LA(I) = LA(I)/AAA
            LB(I) = LB(I)/AAA
        ENDIF

c interpolation des normales si impact dans la zone
c   +/- extension de surface
        IF(IZLIM==1 .OR. ISPT2(I)>0)THEN
C--------------- Use for calculate consisting Dpene in i24forc3       
            NM1(I) = N1(I)
            NM2(I) = N2(I)
            NM3(I) = N3(I)
            K = IC(1,ITQ)
            NAX = NXX(I,K)
            NAY = NYY(I,K)
            NAZ = NZZ(I,K)
C         
            K = IC(2,ITQ)
            NBX = NXX(I,K)
            NBY = NYY(I,K)
            NBZ = NZZ(I,K)
C         
            K = IC(3,ITQ)
            NCX = NXX(I,K)
            NCY = NYY(I,K)
            NCZ = NZZ(I,K)
            N1(I) = LA(I)*NAX + LB(I)*NBX + LC(I)*NCX
            N2(I) = LA(I)*NAY + LB(I)*NBY + LC(I)*NCY
            N3(I) = LA(I)*NAZ + LB(I)*NBZ + LC(I)*NCZ

        
            NNI = NQX*N1(I) + NQY*N2(I) + NQZ*N3(I)
            NI2  = N1(I)*N1(I) + N2(I)*N2(I) + N3(I)*N3(I)
            IF(TWO*NNI*NNI < NI2)THEN
c             scharp angle bound nodal normal to 45 from segment normal
              AAA = SQRT(NI2-NNI*NNI) - NNI
              N1(I) = N1(I) + AAA*NQX
              N2(I) = N2(I) + AAA*NQY
              N3(I) = N3(I) + AAA*NQZ
              NI2  = N1(I)*N1(I) + N2(I)*N2(I) + N3(I)*N3(I)
             ENDIF
            S2 = ONE/MAX(EM30,SQRT(NI2))
            N1(I) = N1(I)*S2
            N2(I) = N2(I)*S2
            N3(I) = N3(I)*S2
C-------special case, skew mesh------------            
            NNI = NQX*N1(I) + NQY*N2(I) + NQZ*N3(I)
           IF (NNI<EM20) THEN
            N1(I) = NQX
            N2(I) = NQY
            N3(I) = NQZ
           END IF            
C            NNI = NQX*N1(I) + NQY*N2(I) + NQZ*N3(I)
C            AAA = MAX(ONE,ONE/MAX(NNI,EM20))
C            PENE(I) = PENE(I)*AAA 
           
        ENDIF

        IF(PENE(I) == ZERO)THEN
          ICONTACT(I) = 0
          ISTEP(I) = 0
          CYCLE
        ENDIF

        IX1(I) = IXX(I,IC( 7,ITQ))
        IX2(I) = IXX(I,IC( 8,ITQ))
        IX3(I) = IXX(I,IC( 9,ITQ))
        IX4(I) = IXX(I,IC(10,ITQ))
        X1(I) = XX0(I,IC( 7,ITQ))
        X2(I) = XX0(I,IC( 8,ITQ))
        X3(I) = XX0(I,IC( 9,ITQ))
        X4(I) = XX0(I,IC(10,ITQ))
        Y1(I) = YY0(I,IC( 7,ITQ))
        Y2(I) = YY0(I,IC( 8,ITQ))
        Y3(I) = YY0(I,IC( 9,ITQ))
        Y4(I) = YY0(I,IC(10,ITQ))
        Z1(I) = ZZ0(I,IC( 7,ITQ))
        Z2(I) = ZZ0(I,IC( 8,ITQ))
        Z3(I) = ZZ0(I,IC( 9,ITQ))
        Z4(I) = ZZ0(I,IC(10,ITQ))

        VX1(I) = VX(I,IC( 7,ITQ))
        VX2(I) = VX(I,IC( 8,ITQ))
        VX3(I) = VX(I,IC( 9,ITQ))
        VX4(I) = VX(I,IC(10,ITQ))
        VY1(I) = VY(I,IC( 7,ITQ))
        VY2(I) = VY(I,IC( 8,ITQ))
        VY3(I) = VY(I,IC( 9,ITQ))
        VY4(I) = VY(I,IC(10,ITQ))
        VZ1(I) = VZ(I,IC( 7,ITQ))
        VZ2(I) = VZ(I,IC( 8,ITQ))
        VZ3(I) = VZ(I,IC( 9,ITQ))
        VZ4(I) = VZ(I,IC(10,ITQ))

        IF    (IX1(I) == IX2(I))THEN
          H1(I) = LA(I)
          H2(I) = ZERO
          H3(I) = LB(I) 
          H4(I) = LC(I)
        ELSEIF(IX2(I) == IX3(I))THEN
          H1(I) = LA(I)
          H2(I) = LB(I)
          H3(I) = ZERO 
          H4(I) = LC(I)
c       ELSEIF(IX3(I) == IX4(I))THEN impossible
        ELSEIF(IX4(I) == IX1(I))THEN
          H1(I) = LC(I)
          H2(I) = LA(I)
          H3(I) = LB(I) 
          H4(I) = ZERO
        ELSE
          H0    = FOURTH * LA(I)
          H1(I) = H0
          H2(I) = H0 
          H3(I) = H0 + LB(I)
          H4(I) = H0 + LC(I) 
        ENDIF
      ENDDO
C--------------------------------------------------------
C     2 Search intersection
C--------------------------------------------------------
c      write(iout,*)'i24dst3 5'
      DO I=1,JLT
       IF(ISTEP(I) /= 2)CYCLE ! previous contact
c       
  
         XI5 = XX(I,5) - XI(I)
         YI5 = YY(I,5) - YI(I)
         ZI5 = ZZ(I,5) - ZI(I)

         V12  = SX125(I)*XI5 + SY125(I)*YI5 + SZ125(I)*ZI5 
         V23  = SX235(I)*XI5 + SY235(I)*YI5 + SZ235(I)*ZI5
         V34  = SX345(I)*XI5 + SY345(I)*YI5 + SZ345(I)*ZI5 
         V41  = SX415(I)*XI5 + SY415(I)*YI5 + SZ415(I)*ZI5      
         
         IF(V12 < ZERO .and. V23 < ZERO .and. 
     .      V34 < ZERO .and. V41 < ZERO ) THEN
c          INTERSECTION IMPOSSIBLE
           CYCLE
         ENDIF

         N=CAND_N(I)

c        POSSIBLE INTERSECTION

C--------------------------------------------------------
C  OLD COORDINATES
C--------------------------------------------------------
         IF(INTPLY > 0) THEN
C         
           DGAP = DGAP_NM(1,CAND_E(I))               
           NX1 = NXX(I,1)                            
           NY1 = NYY(I,1)                            
           NZ1 = NZZ(I,1)                            
           AAA = ONE/SQRT(NX1*NX1+NY1*NY1+NZ1*NZ1)    
           NX1 = NX1 * AAA                           
           NY1 = NY1 * AAA                           
           NZ1 = NZ1 * AAA                           
C         
           XO1 = XX(I,1) - DT1*VX(I,1) - DGAP*NX1    
           YO1 = YY(I,1) - DT1*VY(I,1) - DGAP*NY1    
           ZO1 = ZZ(I,1) - DT1*VZ(I,1) - DGAP*NZ1    
           DGAP = DGAP_NM(2,CAND_E(I))               
           NX1 = NXX(I,2)                           
           NY1 = NYY(I,2)                           
           NZ1 = NZZ(I,2)                           
           AAA = ONE/SQRT(NX1*NX1+NY1*NY1+NZ1*NZ1)   
           NX1 = NX1 * AAA                          
           NY1 = NY1 * AAA                          
           NZ1 = NZ1 * AAA                          
C
           XO2 = XX(I,2) - DT1*VX(I,2)- DGAP*NX1     
           YO2 = YY(I,2) - DT1*VY(I,2)- DGAP*NY1     
           ZO2 = ZZ(I,2) - DT1*VZ(I,2)- DGAP*NZ1     
           DGAP = DGAP_NM(3,CAND_E(I))              
           NX1 = NXX(I,3)                           
           NY1 = NYY(I,3)                           
           NZ1 = NZZ(I,3)                           
           AAA = ONE/SQRT(NX1*NX1+NY1*NY1+NZ1*NZ1)   
           NX1 = NX1 * AAA                          
           NY1 = NY1 * AAA                          
           NZ1 = NZ1 * AAA                          
C                                                     
           XO3 = XX(I,3) - DT1*VX(I,3)- DGAP*NX1     
           YO3 = YY(I,3) - DT1*VY(I,3)- DGAP*NY1     
           ZO3 = ZZ(I,3) - DT1*VZ(I,3)- DGAP*NZ1     
           DGAP = DGAP_NM(4,CAND_E(I))               
           NX1 = NXX(I,4)                           
           NY1 = NYY(I,4)                           
           NZ1 = NZZ(I,4)                           
           AAA = ONE/SQRT(NX1*NX1+NY1*NY1+NZ1*NZ1)   
           NX1 = NX1 * AAA                          
           NY1 = NY1 * AAA                          
           NZ1 = NZ1 * AAA                          
C
           XO4 = XX(I,4) - DT1*VX(I,4)- DGAP*NX1    
           YO4 = YY(I,4) - DT1*VY(I,4)- DGAP*NY1    
           ZO4 = ZZ(I,4) - DT1*VZ(I,4)- DGAP*NZ1    
         ELSE
           XO1 = XX(I,1) - DT1*VX(I,1)    
           YO1 = YY(I,1) - DT1*VY(I,1)    
           ZO1 = ZZ(I,1) - DT1*VZ(I,1) 
C             
           XO2 = XX(I,2) - DT1*VX(I,2)     
           YO2 = YY(I,2) - DT1*VY(I,2)     
           ZO2 = ZZ(I,2) - DT1*VZ(I,2)     
c           
           XO3 = XX(I,3) - DT1*VX(I,3)   
           YO3 = YY(I,3) - DT1*VY(I,3)   
           ZO3 = ZZ(I,3) - DT1*VZ(I,3)
C          
           XO4 = XX(I,4) - DT1*VX(I,4) 
           YO4 = YY(I,4) - DT1*VY(I,4) 
           ZO4 = ZZ(I,4) - DT1*VZ(I,4)
         ENDIF      

         XOI = XI(I) - DT1*VXI(I)
         YOI = YI(I) - DT1*VYI(I)
         ZOI = ZI(I) - DT1*VZI(I) 

         IF(IXX(I,3) /= IXX(I,4))THEN
           XO5 = FOURTH*(XO1+XO2+XO3+XO4)
           YO5 = FOURTH*(YO1+YO2+YO3+YO4)
           ZO5 = FOURTH*(ZO1+ZO2+ZO3+ZO4) 
         ELSE
           XO5 = XO3
           YO5 = YO3
           ZO5 = ZO3
         ENDIF

c        compute volumes at previous time step

         X51 = XO1 - XO5
         Y51 = YO1 - YO5
         Z51 = ZO1 - ZO5
 
         X52 = XO2 - XO5
         Y52 = YO2 - YO5
         Z52 = ZO2 - ZO5
 
         X53 = XO3 - XO5
         Y53 = YO3 - YO5
         Z53 = ZO3 - ZO5
 
         X54 = XO4 - XO5
         Y54 = YO4 - YO5
         Z54 = ZO4 - ZO5
 
         XI5 = XO5 - XOI
         YI5 = YO5 - YOI
         ZI5 = ZO5 - ZOI

         SOX125 = Y51*Z52 - Z51*Y52
         SOY125 = Z51*X52 - X51*Z52
         SOZ125 = X51*Y52 - Y51*X52
         VO12  = SOX125*XI5 + SOY125*YI5 + SOZ125*ZI5 

         SOX235 = Y52*Z53 - Z52*Y53
         SOY235 = Z52*X53 - X52*Z53
         SOZ235 = X52*Y53 - Y52*X53
         VO23  = SOX235*XI5 + SOY235*YI5 + SOZ235*ZI5

         SOX345 = Y53*Z54 - Z53*Y54
         SOY345 = Z53*X54 - X53*Z54
         SOZ345 = X53*Y54 - Y53*X54
         VO34  = SOX345*XI5 + SOY345*YI5 + SOZ345*ZI5 

         SOX415 = Y54*Z51 - Z54*Y51
         SOY415 = Z54*X51 - X54*Z51
         SOZ415 = X54*Y51 - Y54*X51
         VO41  = SOX415*XI5 + SOY415*YI5 + SOZ415*ZI5 

         ADT0(I)   = -ONE

c        compute intersection time (linear approximation)
c        compute coordinates at intersection time
c        intersection time can be different for each sub-triangle
 
         SUBTRIA(I)= 0 ! subtriangle 0 => no contact

         INTERSECT = 0
         CALL INTERSECV(
     1                    ISTEP(I)  ,SUBTRIA(I),IXX(I,3) ,IXX(I,4),
     1                    INTERSECT,V12   ,V23    ,V34   ,V41    ,
     2                    VO12     ,VO23  ,VO34   ,VO41  ,XX(I,1),
     3                    YY(I,1)  ,ZZ(I,1),XX(I,2),YY(I,2),ZZ(I,2) ,
     4                    XX(I,3 ) ,YY(I,3),ZZ(I,3),XX(I,4),YY(I,4) ,
     5                    ZZ(I,4) ,XX(I,5),YY(I,5),ZZ(I,5),ADT0(I) ,
     6                    VXI(I)  ,VYI(I) ,VZI(I) ,XO1   ,XO2    ,
     7                    XO3     ,XO4    ,XO5    ,YO1   ,YO2    ,
     8                    YO3     ,YO4    ,YO5    ,ZO1   ,ZO2    ,
     9                    ZO3     ,ZO4    ,ZO5    ,NX(I) ,NY(I)  ,
     A                    NZ(I)   ,SX125(I),SX235(I),SX345(I),SX415(I),
     B                    SY125(I) ,SY235(I),SY345(I),SY415(I),SZ125(I),
     C                    SZ235(I) ,SZ345(I),SZ415(I),XI(I)   ,YI(I)   ,
     D                    ZI(I)    ,ZEROM   ,UNP    ,ZEROMT,UNPT   )
        IF(INTERSECT == 0.AND.ISHEL(I)>1.AND.ISPT2(I)>0)THEN
         CALL INTERSECSH(
     1                    ISTEP(I)  ,SUBTRIA(I),IXX(I,3) ,IXX(I,4),
     1                    INTERSECT,XX(I,1),
     3                    YY(I,1)  ,ZZ(I,1),XX(I,2),YY(I,2),ZZ(I,2) ,
     4                    XX(I,3 ) ,YY(I,3),ZZ(I,3),XX(I,4),YY(I,4) ,
     5                    ZZ(I,4) ,XX(I,5),YY(I,5),ZZ(I,5),ADT0(I) ,
     6                    XOI     ,YOI    ,ZOI    ,XO1   ,XO2    ,
     7                    XO3     ,XO4    ,XO5    ,YO1   ,YO2    ,
     8                    YO3     ,YO4    ,YO5    ,ZO1   ,ZO2    ,
     9                    ZO3     ,ZO4    ,ZO5    ,NX(I) ,NY(I)  ,
     A                    NZ(I)   ,SX125(I),SX235(I),SX345(I),SX415(I),
     B                    SY125(I) ,SY235(I),SY345(I),SY415(I),SZ125(I),
     C                    SZ235(I) ,SZ345(I),SZ415(I),XI(I)   ,YI(I)   ,
     D                    ZI(I)    ,SOX125  ,SOX235 ,SOX345 ,SOX415  ,
     B                    SOY125 ,SOY235  ,SOY345 ,SOY415 ,SOZ125  ,
     C                    SOZ235 ,SOZ345  ,SOZ415 ,MVOISIN(1,CAND_E(I)) )
        END IF
        IKEEP = 1
        IF(INTERSECT == 0.AND.N <= NSN)THEN
          IF (ICONT_I(N) < 0 ) IKEEP = 0
        ELSEIF(INTERSECT == 0) THEN
          IF (ICONT_I_FI(NIN)%P(N-NSN) < 0 ) IKEEP = 0
        ENDIF
C----- Inacti=5 all 1er contact has 0 force therefore no risk
        IF(IKEEP == 1.AND.ISHEL(I)>0.AND.ISPT2(I)==0.AND.INACTI/=5)THEN
         IF ((GAPS(I)+GAP_M(CAND_E(I)))>TOL_INTS) IKEEP = 0
        END IF
        IF(IKEEP == 1.AND.INTERSECT == 0)THEN
         CALL INTERSECV0(
     1                    ISTEP(I)  ,SUBTRIA(I),IXX(I,3) ,IXX(I,4),
     1                    INTERSECT,V12   ,V23    ,V34   ,V41    ,
     2                    VO12     ,VO23  ,VO34   ,VO41  ,XX(I,1),
     3                    YY(I,1)  ,ZZ(I,1),XX(I,2),YY(I,2),ZZ(I,2) ,
     4                    XX(I,3 ) ,YY(I,3),ZZ(I,3),XX(I,4),YY(I,4) ,
     5                    ZZ(I,4) ,XX(I,5),YY(I,5),ZZ(I,5),ADT0(I) ,
     6                    XOI     ,YOI    ,ZOI    ,XO1   ,XO2    ,
     7                    XO3     ,XO4    ,XO5    ,YO1   ,YO2    ,
     8                    YO3     ,YO4    ,YO5    ,ZO1   ,ZO2    ,
     9                    ZO3     ,ZO4    ,ZO5    ,NX(I) ,NY(I)  ,
     A                    NZ(I)   ,SX125(I),SX235(I),SX345(I),SX415(I),
     B                    SY125(I) ,SY235(I),SY345(I),SY415(I),SZ125(I),
     C                    SZ235(I) ,SZ345(I),SZ415(I),XI(I)   ,YI(I)   ,
     D                    ZI(I)    ,ZEROM   ,UNP    )
C----- due to different algorithms used in starter/engine, it's more assurant to do the correction here     
         IF (TT==ZERO.AND.INTERSECT>0.AND.INACTI==0) THEN
          IF(N <= NSN)THEN
           ICONT_I(N) = -CAND_E(I)
          ELSE
           ICONT_I_FI(NIN)%P(N-NSN) = -CAND_E(I)
          ENDIF
          INTERSECT = 0
          ISTEP(I) = 2
         END IF
         ICONT_R(I) = INTERSECT
        END IF!(IKEEP == 1)THEN
C
      ENDDO
!                numerotation des sous-triangles
!
!                                      
!                 6o------o5           
!                  |\ 19 /|                              
!                  | \  / |            
!                  |  \/  |                7,8=0o------3=4 
!                  |15/\11|                     |     /|\
!                  | /  \ |                     | 8  / | \
!                  |/ 7  \|                     |   /  |  \
!          7o------4------3------o4             |  /   |   \
!           |\ 12 /|\    /|\ 14 /|              | /  1 | 6  \
!           | \  / | \3 / | \  / |              |/     |     \
!           |  \/  |  \/2 |6 \/18|              1------2------o3,4=0
!           |20/\ 8| 4/\  |  /\  |              |     /           
!           | /  \ | / 1\ | /  \ |              |  5 /    
!           |/ 16 \|/    \|/ 10 \|              |   /     
!          8o------1------2------o3             |  /     
!                  |\  5 /|                     | /   
!                  | \  / |                     |/
!                  |9 \/13|                    1o2=0
!                  |  /\  | 
!                  | /  \ | 
!                  |/ 17 \| 
!                 1o------o2
!
! +------+-------------------------------------------+
! |      |       sous-triangle voisins               |
! |sous  +----------+----------+---------------------+
! |trian |       n3/=n4        |             n3=n4   |
! |      +----------+----------+----------+----------+
! |      |   IT0    | 2,4,6,8=0|          | 2,4,6,8=0|
! +------+----------+----------+----------+----------+
! |   1  |  5  2  4 |  5  2  4 |  5  6  8 |  5  6  8 |
! |   2  |  6  3  1 |  6  3  1 |  -  -  - |  -  -  - |
! |   3  |  7  4  2 |  7  4  2 |  -  -  - |  -  -  - |
! |   4  |  8  1  3 |  8  1  3 |  -  -  - |  -  -  - |
! +------+----------+----------+----------+----------+
! |   5  |  1  9 13 |  1 -1 -1 |  1  9 13 |  1 -1 -1 |
! |   6  |  2 10 14 |  2 -1 -1 |  1 10 14 |  1 -1 -1 |
! |   7  |  3 11 15 |  3 -1 -1 |  -  -  - |  -  -  - |
! |   8  |  4 12 16 |  4 -1 -1 |  1 12 16 |  1 -1 -1 |
! +------+----------+----------+----------+----------+
! |   9  |  5 -1 17 |  -  -  - |  5 -1 17 |  -  -  - |
! |  10  |  6 -1 18 |  -  -  - |  6 -1 18 |  -  -  - |
! |  11  |  7 -1 19 |  -  -  - |  -  -  - |  -  -  - |
! |  12  |  8 -1 20 |  -  -  - |  8 -1 20 |  -  -  - |
! +------+----------+----------+----------+----------+
! |  13  |  5 17 -1 |  -  -  - |  5 17 -1 |  -  -  - |
! |  14  |  6 18 -1 |  -  -  - |  6 18 -1 |  -  -  - |
! |  15  |  7 19 -1 |  -  -  - |  -  -  - |  -  -  - |
! |  16  |  8 20 -1 |  -  -  - |  8 20 -1 |  -  -  - |
! +------+----------+----------+----------+----------+
! |  17  |  9 13 -1 |  -  -  - |  9 13 -1 |  -  -  - |
! |  18  | 10 14 -1 |  -  -  - | 10 14 -1 |  -  -  - |
! |  19  | 11 15 -1 |  -  -  - |  -  -  - |  -  -  - |
! |  20  | 12 16 -1 |  -  -  - | 12 16 -1 |  -  -  - |
! +------+----------+----------+----------+----------+
!        
!
!                connetivite des sous-triangles IC(10,*)
!
!
! +----+----------+----------+-------------+
! | ST | noeuds T | noeuds TV| noeuds Quad |
! +----+----------+----------+-------------+          
! |  1 |  5  1  2 | 14  3  4 |  3  4  1  2 |
! |  2 |  5  2  3 | 15  4  1 |  4  1  2  3 |        11-------10       
! |  3 |  5  3  4 | 16  1  2 |  1  2  3  4 |         |\ 19  /|        
! |  4 |  5  4  1 | 17  2  3 |  2  3  4  1 |         | \   / |                                    
! +----+----------+----------+-------------+         |  \ /  |           
! |  5 | 14  2  1 |  5  6  7 |  6  7  2  1 |         |  16   |           
! |  6 | 15  3  2 |  5  8  9 |  8  9  3  2 |         |15/ \11|        
! |  7 | 16  4  3 |  5 10 11 | 10 11  4  3 |         | /   \ |      
! |  8 | 17  1  4 |  5 12 13 | 12 13  1  4 |         |/  7  \|       
! +----+----------+----------+-------------+12-------4-------3-------9
! |  9 | 14  1  6 |(13) 7  2 |  7  2  1  6 | |\ 12  /|\     /|\ 14  /|
! | 10 | 15  2  8 |( 7) 9  3 |  9  3  2  8 | | \   / | \ 3 / | \   / |
! | 11 | 16  3 10 |( 9)11  4 | 11  4  3 10 | |  \ /  |  \ /2 |6 \ /18|
! | 12 | 17  4 12 |(11)13  1 | 13  1  4 12 | |  17   |   5   |  15   |
! +----+----------+----------+-------------+ |20/ \ 8| 4/ \  |  / \  |
! | 13 | 14  7  2 |( 8) 1  6 |  1  6  7  2 | | /   \ | / 1 \ | /   \ |
! | 14 | 15  9  3 |(10) 2  8 |  2  8  9  3 | |/ 16  \|/     \|/ 10  \|
! | 15 | 16 11  4 |(12) 3 10 |  3 10 11  4 |13-------1-------2-------8
! | 16 | 17 13  1 |( 6) 4 12 |  4 12 13  1 |         |\  5  /|       
! +----+----------+----------+-------------+         | \   / |      
! | 17 | 14  6  7 |  -  2  1 |  2  1  6  7 |         |9 \ /13|    
! | 18 | 15  8  9 |  -  3  2 |  3  2  8  9 |         |  14   |    
! | 19 | 16 10 11 |  -  4  3 |  4  3 10 11 |         |  / \  |   
! | 20 | 17 12 13 |  -  1  4 |  1  4 12 13 |         | /   \ |  
! +----+----------+----------+-------------+         |/ 17  \| 
!                                                    6-------7
! 
C=======================================================================
c    sliding
C=======================================================================

C--------------------------------------------------------
C     0 common initialization node 6 to 17
C--------------------------------------------------------

      DO I=1,JLT

       IF(ISTEP(I) /= 3)CYCLE


       NXX(I,6) =SX21140(I)
       NYY(I,6) =SY21140(I)
       NZZ(I,6) =SZ21140(I)

       NXX(I,7) =SX21140(I)
       NYY(I,7) =SY21140(I)
       NZZ(I,7) =SZ21140(I)

       NXX(I,8) =SX32150(I)
       NYY(I,8) =SY32150(I)
       NZZ(I,8) =SZ32150(I)

       NXX(I,9) =SX32150(I)
       NYY(I,9) =SY32150(I)
       NZZ(I,9) =SZ32150(I)

       NXX(I,15)=SX32150(I)
       NYY(I,15)=SY32150(I)
       NZZ(I,15)=SZ32150(I)

       NXX(I,10)=SX43160(I)
       NYY(I,10)=SY43160(I)
       NZZ(I,10)=SZ43160(I)

       NXX(I,11)=SX43160(I)
       NYY(I,11)=SY43160(I)
       NZZ(I,11)=SZ43160(I)

       NXX(I,14)=SX21140(I)
       NYY(I,14)=SY21140(I)
       NZZ(I,14)=SZ21140(I)

       NXX(I,16)=SX43160(I)
       NYY(I,16)=SY43160(I)
       NZZ(I,16)=SZ43160(I)

       NXX(I,12)=SX14170(I)
       NYY(I,12)=SY14170(I)
       NZZ(I,12)=SZ14170(I)

       NXX(I,13)=SX14170(I)
       NYY(I,13)=SY14170(I)
       NZZ(I,13)=SZ14170(I)

       NXX(I,17)=SX14170(I)
       NYY(I,17)=SY14170(I)
       NZZ(I,17)=SZ14170(I)

c       GAP : coordinates correction

       N=CAND_N(I)

c       IF(GAPS(I)+GAP_M(CAND_E(I))
c     .         +GAP_NM(5,CAND_E(I))+GAP_NM(6,CAND_E(I))
c     .         +GAP_NM(7,CAND_E(I))+GAP_NM(8,CAND_E(I))
c     .         +GAP_NM(9,CAND_E(I))+GAP_NM(10,CAND_E(I))
c     .        +GAP_NM(11,CAND_E(I))+GAP_NM(12,CAND_E(I)) /= ZERO)THEN
        IF(ISHEL(I)>0) THEN

c        la normale sur les point 6 a 17 est approche
c        pour la correction du gap (utilise pendant un seul cycle)

         BBB = SQRT(SX21140(I)*SX21140(I)
     +             +SY21140(I)*SY21140(I)
     +             +SZ21140(I)*SZ21140(I))
         AAA = GAPS(I)+GAP_NM(5,CAND_E(I))
         AAA = AAA/BBB
         XX(I,6) = XX0(I,6) + SX21140(I) * AAA
         YY(I,6) = YY0(I,6) + SY21140(I) * AAA
         ZZ(I,6) = ZZ0(I,6) + SZ21140(I) * AAA
         AAA = GAPS(I)+GAP_NM(6,CAND_E(I))
         AAA = AAA/BBB
         XX(I,7) = XX0(I,7) + SX21140(I) * AAA
         YY(I,7) = YY0(I,7) + SY21140(I) * AAA
         ZZ(I,7) = ZZ0(I,7) + SZ21140(I) * AAA

         BBB = SQRT(SX32150(I)*SX32150(I)
     +             +SY32150(I)*SY32150(I)
     +             +SZ32150(I)*SZ32150(I))
         AAA = GAPS(I)+GAP_NM(7,CAND_E(I))
         AAA = AAA/BBB
         XX(I,8) = XX0(I,8) + SX32150(I) * AAA
         YY(I,8) = YY0(I,8) + SY32150(I) * AAA
         ZZ(I,8) = ZZ0(I,8) + SZ32150(I) * AAA
         AAA = GAPS(I)+GAP_NM(8,CAND_E(I))
         AAA = AAA/BBB
         XX(I,9) = XX0(I,9) + SX32150(I) * AAA
         YY(I,9) = YY0(I,9) + SY32150(I) * AAA
         ZZ(I,9) = ZZ0(I,9) + SZ32150(I) * AAA

         IF(IXX(I,3) == IXX(I,4))THEN

           XX(I,10) = XX0(I,10) 
           YY(I,10) = YY0(I,10) 
           ZZ(I,10) = ZZ0(I,10)
 
           XX(I,11) = XX0(I,11) 
           YY(I,11) = YY0(I,11) 
           ZZ(I,11) = ZZ0(I,11) 

         ELSE

           BBB = SQRT(SX43160(I)*SX43160(I)
     +               +SY43160(I)*SY43160(I)
     +               +SZ43160(I)*SZ43160(I))
           AAA = GAPS(I)+GAP_NM(9,CAND_E(I))
           AAA = AAA/BBB
           XX(I,10) = XX0(I,10) + SX43160(I) * AAA
           YY(I,10) = YY0(I,10) + SY43160(I) * AAA
           ZZ(I,10) = ZZ0(I,10) + SZ43160(I) * AAA
           AAA = GAPS(I)+GAP_NM(10,CAND_E(I))
           AAA = AAA/BBB
           XX(I,11) = XX0(I,11) + SX43160(I) * AAA
           YY(I,11) = YY0(I,11) + SY43160(I) * AAA
           ZZ(I,11) = ZZ0(I,11) + SZ43160(I) * AAA
         ENDIF

         BBB = SQRT(SX14170(I)*SX14170(I)
     +             +SY14170(I)*SY14170(I)
     +             +SZ14170(I)*SZ14170(I))
         AAA = GAPS(I)+GAP_NM(11,CAND_E(I))
         AAA = AAA/BBB
         XX(I,12) = XX0(I,12) + SX14170(I) * AAA
         YY(I,12) = YY0(I,12) + SY14170(I) * AAA
         ZZ(I,12) = ZZ0(I,12) + SZ14170(I) * AAA
         AAA = GAPS(I)+GAP_NM(12,CAND_E(I))
         AAA = AAA/BBB
         XX(I,13) = XX0(I,13) + SX14170(I) * AAA
         YY(I,13) = YY0(I,13) + SY14170(I) * AAA
         ZZ(I,13) = ZZ0(I,13) + SZ14170(I) * AAA

c      write(iout,*)'i=',i,'i24dst3 29'
         IF(IXX(I,6) == IXX(I,7))THEN
           XX(I,14) = XX(I,6)
           YY(I,14) = YY(I,6)
           ZZ(I,14) = ZZ(I,6)
         ELSE
           XX(I,14) = FOURTH*(XX(I,2)+XX(I,1)+XX(I,6)+XX(I,7))
           YY(I,14) = FOURTH*(YY(I,2)+YY(I,1)+YY(I,6)+YY(I,7))
           ZZ(I,14) = FOURTH*(ZZ(I,2)+ZZ(I,1)+ZZ(I,6)+ZZ(I,7))
         ENDIF
         IF(IXX(I,8)==IXX(I,9))THEN
           XX(I,15) = XX(I,8)
           YY(I,15) = YY(I,8)
           ZZ(I,15) = ZZ(I,8)
         ELSE
           XX(I,15) = FOURTH*(XX(I,3)+XX(I,2)+XX(I,8)+XX(I,9))
           YY(I,15) = FOURTH*(YY(I,3)+YY(I,2)+YY(I,8)+YY(I,9))
           ZZ(I,15) = FOURTH*(ZZ(I,3)+ZZ(I,2)+ZZ(I,8)+ZZ(I,9))
         ENDIF
         IF(IXX(I,10)==IXX(I,11))THEN
           XX(I,16) = XX(I,10)
           YY(I,16) = YY(I,10)
           ZZ(I,16) = ZZ(I,10)
         ELSE
           XX(I,16) = FOURTH*(XX(I,4)+XX(I,3)+XX(I,10)+XX(I,11))
           YY(I,16) = FOURTH*(YY(I,4)+YY(I,3)+YY(I,10)+YY(I,11))
           ZZ(I,16) = FOURTH*(ZZ(I,4)+ZZ(I,3)+ZZ(I,10)+ZZ(I,11))
         ENDIF
         IF(IXX(I,12)==IXX(I,13))THEN
           XX(I,17) = XX(I,12)
           YY(I,17) = YY(I,12)
           ZZ(I,17) = ZZ(I,12)
         ELSE
           XX(I,17) = FOURTH*(XX(I,1)+XX(I,4)+XX(I,12)+XX(I,13))
           YY(I,17) = FOURTH*(YY(I,1)+YY(I,4)+YY(I,12)+YY(I,13))
           ZZ(I,17) = FOURTH*(ZZ(I,1)+ZZ(I,4)+ZZ(I,12)+ZZ(I,13))
         ENDIF

         X141 = XX(I,1) - XX(I,14)
         Y141 = YY(I,1) - YY(I,14)
         Z141 = ZZ(I,1) - ZZ(I,14)
 
         X142 = XX(I,2) - XX(I,14)
         Y142 = YY(I,2) - YY(I,14)
         Z142 = ZZ(I,2) - ZZ(I,14)
 
         X152 = XX(I,2) - XX(I,15)
         Y152 = YY(I,2) - YY(I,15)
         Z152 = ZZ(I,2) - ZZ(I,15)
 
         X153 = XX(I,3) - XX(I,15)
         Y153 = YY(I,3) - YY(I,15)
         Z153 = ZZ(I,3) - ZZ(I,15)
 
         X163 = XX(I,3) - XX(I,16)
         Y163 = YY(I,3) - YY(I,16)
         Z163 = ZZ(I,3) - ZZ(I,16)
 
         X164 = XX(I,4) - XX(I,16)
         Y164 = YY(I,4) - YY(I,16)
         Z164 = ZZ(I,4) - ZZ(I,16)

         X174 = XX(I,4) - XX(I,17)
         Y174 = YY(I,4) - YY(I,17)
         Z174 = ZZ(I,4) - ZZ(I,17)

         X171 = XX(I,1) - XX(I,17)
         Y171 = YY(I,1) - YY(I,17)
         Z171 = ZZ(I,1) - ZZ(I,17)

         IF(MVOISIN(1,CAND_E(I))/=0)THEN
           SX2114(I) = Y142*Z141 - Z142*Y141
           SY2114(I) = Z142*X141 - X142*Z141
           SZ2114(I) = X142*Y141 - Y142*X141
         ELSE
           SX2114(I) = SX125(I)
           SY2114(I) = SY125(I)
           SZ2114(I) = SZ125(I)
         ENDIF

         IF(MVOISIN(2,CAND_E(I))/=0)THEN
           SX3215(I) = Y153*Z152 - Z153*Y152
           SY3215(I) = Z153*X152 - X153*Z152
           SZ3215(I) = X153*Y152 - Y153*X152
         ELSE
           SX3215(I) = SX235(I)
           SY3215(I) = SY235(I)
           SZ3215(I) = SZ235(I)
         ENDIF

         IF(MVOISIN(3,CAND_E(I))/=0)THEN
           SX4316(I) = Y164*Z163 - Z164*Y163
           SY4316(I) = Z164*X163 - X164*Z163
           SZ4316(I) = X164*Y163 - Y164*X163
         ELSE
           SX4316(I) = SX345(I)
           SY4316(I) = SY345(I)
           SZ4316(I) = SZ345(I)
         ENDIF

         IF(MVOISIN(4,CAND_E(I))/=0)THEN
           SX1417(I) = Y171*Z174 - Z171*Y174
           SY1417(I) = Z171*X174 - X171*Z174
           SZ1417(I) = X171*Y174 - Y171*X174
         ELSE
           SX1417(I) = SX415(I)
           SY1417(I) = SY415(I)
           SZ1417(I) = SZ415(I)
         ENDIF

       ELSE
         
         XX(I,6:17) = XX0(I,6:17)
         YY(I,6:17) = YY0(I,6:17)
         ZZ(I,6:17) = ZZ0(I,6:17)
         
         SX2114(I) = SX21140(I)
         SY2114(I) = SY21140(I)
         SZ2114(I) = SZ21140(I)
         
         SX3215(I) = SX32150(I)
         SY3215(I) = SY32150(I)
         SZ3215(I) = SZ32150(I)
         
         SX4316(I) = SX43160(I)
         SY4316(I) = SY43160(I)
         SZ4316(I) = SZ43160(I)
         
         SX1417(I) = SX14170(I)
         SY1417(I) = SY14170(I)
         SZ1417(I) = SZ14170(I)

       ENDIF
c      write(iout,*)'i=',i,'i24dst3 212'
      ENDDO



C=======================================================================
c   3 first sliding 
C=======================================================================
c      write(iout,*)'i24dst3 6'
      NSLID = 0
      DO I=1,JLT
       IF(ISTEP(I) /= 3)CYCLE
       ISTEP(I) = 5
! +------+-------------------------------------------+
! |      |       sous-triangle voisins               |
! |sous  +----------+----------+---------------------+
! |trian |       n3/=n4        |             n3=n4   |
! |      +----------+----------+----------+----------+
! |      |   IT0    | 2,4,6,8=0|          | 2,4,6,8=0|
! +------+----------+----------+----------+----------+
! |   1  |  5  2  4 |  5  2  4 |  5  6  8 |  5  6  8 |
! |   2  |  6  3  1 |  6  3  1 |  -  -  - |  -  -  - |
! |   3  |  7  4  2 |  7  4  2 |  -  -  - |  -  -  - |
! |   4  |  8  1  3 |  8  1  3 |  -  -  - |  -  -  - |
! +------+----------+----------+----------+----------+
! |   5  |  1  9 13 |  1 -1 -1 |  1  9 13 |  1 -1 -1 |
! |   6  |  2 10 14 |  2 -1 -1 |  1 10 14 |  1 -1 -1 |
! |   7  |  3 11 15 |  3 -1 -1 |  -  -  - |  -  -  - |
! |   8  |  4 12 16 |  4 -1 -1 |  1 12 16 |  1 -1 -1 |
! +------+----------+----------+----------+----------+
! |   9  | -1 17  5 |  -  -  - | -1 17  5 |  -  -  - |
! |  10  | -1 18  6 |  -  -  - | -1 18  6 |  -  -  - |
! |  11  | -1 19  7 |  -  -  - |  -  -  - |  -  -  - |
! |  12  | -1 20  8 |  -  -  - | -1 20  8 |  -  -  - |
! +------+----------+----------+----------+----------+
! |  13  | -1  5 17 |  -  -  - | -1  5 17 |  -  -  - |
! |  14  | -1  6 18 |  -  -  - | -1  6 18 |  -  -  - |
! |  15  | -1  7 19 |  -  -  - |  -  -  - |  -  -  - |
! |  16  | -1  8 20 |  -  -  - | -1  8 20 |  -  -  - |
! +------+----------+----------+----------+----------+
! |  17  | -1  9 13 |  -  -  - | -1  9 13 |  -  -  - |
! |  18  | -1 10 14 |  -  -  - | -1 10 14 |  -  -  - |
! |  19  | -1 11 15 |  -  -  - |  -  -  - |  -  -  - |
! |  20  | -1 12 16 |  -  -  - | -1 12 16 |  -  -  - |
! +------+----------+----------+----------+----------+
        IT(1:3,1:20,I)=IT0(1:3,1:20)

        IF(IXX(I,3) /= IXX(I,4))THEN
          IF(MVOISIN(1,CAND_E(I))==0)IT( 1, 1,I) = 0
          IF(MVOISIN(2,CAND_E(I))==0)IT( 1, 2,I) = 0
          IF(MVOISIN(3,CAND_E(I))==0)IT( 1, 3,I) = 0
          IF(MVOISIN(4,CAND_E(I))==0)IT( 1, 4,I) = 0
        ELSE
          IT(2,1,I)=6
          IT(3,1,I)=8
          IT(1,5,I)=1
          IT(1,6,I)=1
          IT(1,8,I)=1
          IF(MVOISIN(1,CAND_E(I))==0)IT( 1, 1,I) = 0
          IF(MVOISIN(2,CAND_E(I))==0)IT( 2, 1,I) = 0
          IF(MVOISIN(3,CAND_E(I))==0)IT( 3, 1,I) = 0
c a vrifier          4,IF(MVOISIN(4,CAND_E(I))==0)IT( 3, 1,I) = 0
        ENDIF
        IF(IXX(I,6)==IXX(I,7))THEN
           IT(2,5,I)=-1
           IT(3,5,I)=-1
        ENDIF
        IF(IXX(I,8)==IXX(I,9))THEN
           IT(2,6,I)=-1
           IT(3,6,I)=-1
        ENDIF
        IF(IXX(I,10)==IXX(I,11))THEN
           IT(2,7,I)=-1
           IT(3,7,I)=-1
        ENDIF
        IF(IXX(I,12)==IXX(I,13))THEN
           IT(2,8,I)=-1
           IT(3,8,I)=-1
        ENDIF

        IF(IXX(I,3) /= IXX(I,4))THEN
          
          IF(SUBTRIA(I)==1)THEN
            XA(I) = XX(I,5)   !         f-----------a-----------e 
            YA(I) = YY(I,5)   !          \         / \         /
            ZA(I) = ZZ(I,5)   !           \   T   /   \   S   / 
            XB(I) = XX(I,1)   !            \     /     \     /  
            YB(I) = YY(I,1)   !             \   /   Q   \   /   
            ZB(I) = ZZ(I,1)   !              \ /         \ /    
            XC(I) = XX(I,2)   !               b-----------c
            YC(I) = YY(I,2)   !                \         /           
            ZC(I) = ZZ(I,2)   !                 \   R   /           
                              !                  \     /            
            NQX = SX125(I)    !                   \   /        
            NQY = SY125(I)    !                    \ /        
            NQZ = SZ125(I)    !                     d
            NSX = SX235(I)
            NSY = SY235(I) 
            NSZ = SZ235(I) 
            NTX = SX415(I)
            NTY = SY415(I)
            NTZ = SZ415(I)
            NRX = SX2114(I)
            NRY = SY2114(I)
            NRZ = SZ2114(I)
            ITR = 5
            IF(MVOISIN(1,CAND_E(I))==0)THEN
              ITR = -ITR
              NRX = NQX
              NRY = NQY
              NRZ = NQZ
            ENDIF
            ITS = 2
            ITT = 4    
          ELSEIF(SUBTRIA(I)==2)THEN
            XA(I) = XX(I,5)   
            YA(I) = YY(I,5)   
            ZA(I) = ZZ(I,5)   
            XB(I) = XX(I,2)   
            YB(I) = YY(I,2)   
            ZB(I) = ZZ(I,2)   
            XC(I) = XX(I,3)   
            YC(I) = YY(I,3)   
            ZC(I) = ZZ(I,3)   
            NQX = SX235(I) 
            NQY = SY235(I) 
            NQZ = SZ235(I) 
            NSX = SX345(I) 
            NSY = SY345(I) 
            NSZ = SZ345(I) 
            NTX = SX125(I) 
            NTY = SY125(I) 
            NTZ = SZ125(I) 
            NRX = SX3215(I)
            NRY = SY3215(I)
            NRZ = SZ3215(I)
            ITR = 6
            IF(MVOISIN(2,CAND_E(I))==0)THEN
              ITR = -ITR
              NRX = NQX
              NRY = NQY
              NRZ = NQZ
            ENDIF
            ITS = 3
            ITT = 1    
          ELSEIF(SUBTRIA(I)==3)THEN
            XA(I) = XX(I,5)   
            YA(I) = YY(I,5)   
            ZA(I) = ZZ(I,5)   
            XB(I) = XX(I,3)   
            YB(I) = YY(I,3)   
            ZB(I) = ZZ(I,3)   
            XC(I) = XX(I,4)   
            YC(I) = YY(I,4)   
            ZC(I) = ZZ(I,4)   
            NQX = SX345(I) 
            NQY = SY345(I) 
            NQZ = SZ345(I) 
            NSX = SX415(I)
            NSY = SY415(I)
            NSZ = SZ415(I)
            NTX = SX235(I) 
            NTY = SY235(I) 
            NTZ = SZ235(I) 
            NRX = SX4316(I)
            NRY = SY4316(I)
            NRZ = SZ4316(I)
            ITR = 7
            IF(MVOISIN(3,CAND_E(I))==0)THEN
              ITR = -ITR
              NRX = NQX
              NRY = NQY
              NRZ = NQZ
            ENDIF
            ITS = 4
            ITT = 2    
          ELSEIF(SUBTRIA(I)==4)THEN
            XA(I) = XX(I,5)   
            YA(I) = YY(I,5)   
            ZA(I) = ZZ(I,5)   
            XB(I) = XX(I,4)   
            YB(I) = YY(I,4)   
            ZB(I) = ZZ(I,4)   
            XC(I) = XX(I,1)   
            YC(I) = YY(I,1)   
            ZC(I) = ZZ(I,1)   
            NQX = SX415(I)
            NQY = SY415(I)
            NQZ = SZ415(I)
            NSX = SX125(I) 
            NSY = SY125(I) 
            NSZ = SZ125(I) 
            NTX = SX345(I) 
            NTY = SY345(I) 
            NTZ = SZ345(I) 
            NRX = SX1417(I)
            NRY = SY1417(I)
            NRZ = SZ1417(I)
            ITR = 8
            IF(MVOISIN(4,CAND_E(I))==0)THEN
              ITR = -ITR
              NRX = NQX
              NRY = NQY
              NRZ = NQZ
            ENDIF
            ITS = 1
            ITT = 3    
          ENDIF
        ELSE!IF(IXX(I,3) /= IXX(I,4)))THEN
            XA(I) = XX(I,3)   
            YA(I) = YY(I,3)   
            ZA(I) = ZZ(I,3)   
            XB(I) = XX(I,1)    !         f-----------a-----------e 
            YB(I) = YY(I,1)    !          \         / \         /
            ZB(I) = ZZ(I,1)    !           \   T   /   \   S   / 
            XC(I) = XX(I,2)    !            \     /     \     /  
            YC(I) = YY(I,2)    !             \   /   Q   \   /         
            ZC(I) = ZZ(I,2)    !              \ /         \ /         
            NQX = SX125(I)     !               b-----------c  
            NQY = SY125(I)     !                \         /   
            NQZ = SZ125(I)     !                 \   R   /    
            NSX = SX3215(I)    !                  \     /     
            NSY = SY3215(I)    !                   \   /      
            NSZ = SZ3215(I)    !                    \ /       
            NTX = SX1417(I)    !                     d        
            NTY = SY1417(I)                                   
            NTZ = SZ1417(I)
            NRX = SX2114(I)
            NRY = SY2114(I)
            NRZ = SZ2114(I)
            ITR = 5
            IF(MVOISIN(1,CAND_E(I))==0)THEN
              ITR = -ITR
              NRX = NQX
              NRY = NQY
              NRZ = NQZ
            ENDIF
            ITS = 6
            IF(MVOISIN(2,CAND_E(I))==0)THEN
              ITS = -ITS
              NSX = NQX
              NSY = NQY
              NSZ = NQZ
            ENDIF
            ITT = 8    
            IF(MVOISIN(4,CAND_E(I))==0)THEN
              ITT = -ITT
              NTX = NQX
              NTY = NQY
              NTZ = NQZ
            ENDIF
        ENDIF

        NX(I) = NQX
        NY(I) = NQY
        NZ(I) = NQZ

c       calcul des plans bisecteurs
        NQRX = NRX + NQX
        NQRY = NRY + NQY
        NQRZ = NRZ + NQZ
        NQSX = NSX + NQX
        NQSY = NSY + NQY
        NQSZ = NSZ + NQZ
        NQTX = NTX + NQX
        NQTY = NTY + NQY
        NQTZ = NTZ + NQZ
        IF(ABS(ITR)==SUBTRIA_N(I))THEN
          PRX = NQRY*(ZC(I)-ZB(I)) - NQRZ*(YC(I)-YB(I))
          PRY = NQRZ*(XC(I)-XB(I)) - NQRX*(ZC(I)-ZB(I))
          PRZ = NQRX*(YC(I)-YB(I)) - NQRY*(XC(I)-XB(I))
          VR = PRX*(XI(I)-XB(I))+ PRY*(YI(I)-YB(I))+ PRZ*(ZI(I)-ZB(I))
          IF(VR < ZERO)THEN
            IF(ITR>0)THEN
              ISTEP(I) = 5
              NSLID = NSLID+1
              SUBTRIA(I) = ITR
            ELSE
c             sliding out of the surface              
              ICONTACT(I) = 0
              ISTEP(I) = 0
              SUBTRIA(I) = 0
            ENDIF
            NX(I) = NRX
            NY(I) = NRY
            NZ(I) = NRZ
          ENDIF
        ELSEIF(ABS(ITS)==SUBTRIA_N(I))THEN
          PSX = NQSY*(ZA(I)-ZC(I)) - NQSZ*(YA(I)-YC(I))
          PSY = NQSZ*(XA(I)-XC(I)) - NQSX*(ZA(I)-ZC(I))
          PSZ = NQSX*(YA(I)-YC(I)) - NQSY*(XA(I)-XC(I))
          VS = PSX*(XI(I)-XC(I))+ PSY*(YI(I)-YC(I))+ PSZ*(ZI(I)-ZC(I))
          IF(VS < ZERO)THEN
            IF(ITS>0)THEN
              ISTEP(I) = 5
              NSLID = NSLID+1
              SUBTRIA(I) = ITS
            ELSE
c             sliding out of the surface              
              ICONTACT(I) = 0
              ISTEP(I) = 0
              SUBTRIA(I) = 0
            ENDIF
            NX(I) = NSX
            NY(I) = NSY
            NZ(I) = NSZ
          ENDIF
        ELSEIF(ABS(ITT)==SUBTRIA_N(I))THEN
          PTX = NQTY*(ZB(I)-ZA(I)) - NQTZ*(YB(I)-YA(I))
          PTY = NQTZ*(XB(I)-XA(I)) - NQTX*(ZB(I)-ZA(I))
          PTZ = NQTX*(YB(I)-YA(I)) - NQTY*(XB(I)-XA(I))
          VT = PTX*(XI(I)-XA(I))+ PTY*(YI(I)-YA(I))+ PTZ*(ZI(I)-ZA(I))
          IF(VT < ZERO)THEN
            IF(ITT>0)THEN
              ISTEP(I) = 5
              NSLID = NSLID+1
              SUBTRIA(I) = ITT
            ELSE
c             sliding out of the surface              
              ICONTACT(I) = 0
              ISTEP(I) = 0
              SUBTRIA(I) = 0
            ENDIF
            NX(I) = NTX
            NY(I) = NTY
            NZ(I) = NTZ
          ENDIF
        ENDIF
      ENDDO
C=======================================================================
c  4 next sliding
C=======================================================================
c      write(iout,*)'NSLID=',NSLID,'i24dst3 7'
      NSS = 1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      NSLID=0 ! debranchement
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C=======================================================================
c  5 surface coordinates
C=======================================================================
#include "lockon.inc"
c      write(iout,*)'i24dst3 8'
      DO I=1,JLT
       IF(ISTEP(I) /= 1)PENE(I) = ZERO
       IF(ISTEP(I) < 4 .OR.ISTEP(I) == 6)CYCLE
        ITQ = SUBTRIA(I) 
        K = IC(1,ITQ)
        XA(I) = XX(I,K)   
        YA(I) = YY(I,K)    
        ZA(I) = ZZ(I,K) 
        NAX = NXX(I,K)
        NAY = NYY(I,K)
        NAZ = NZZ(I,K)

        K = IC(2,ITQ)
        XB(I) = XX(I,K)   
        YB(I) = YY(I,K)    
        ZB(I) = ZZ(I,K)   
        NBX = NXX(I,K)
        NBY = NYY(I,K)
        NBZ = NZZ(I,K)

        K = IC(3,ITQ)
        XC(I) = XX(I,K)   
        YC(I) = YY(I,K)    
        ZC(I) = ZZ(I,K)   
        NCX = NXX(I,K)
        NCY = NYY(I,K)
        NCZ = NZZ(I,K)

        NQX = NX(I)
        NQY = NY(I)
        NQZ = NZ(I)
        
        AAA = MAX(EM30,SQRT(NQX*NQX + NQY*NQY + NQZ*NQZ))
        AREA(I) = AAA
        S2 = ONE/AAA
        NQX = NQX*S2
        NQY = NQY*S2
        NQZ = NQZ*S2

        BBB = (XI(I)-XA(I))*NQX+(YI(I)-YA(I))*NQY+(ZI(I)-ZA(I))*NQZ
        IKEEP=0
C---------------change NSNE to global flag after---   
C------debug P/ON RD-5326 limiting this no-physical case (remaining contact)----     
        IF (BBB>=ZERO .AND.BBB<MARGE025.AND.EPS>EM02 .AND. IMPL_S==0
     +                .AND. NSNE==0) THEN
         PENE(I) = EM15
         IKEEP=1
        ELSE
         PENE(I) = -MIN(ZERO,BBB)
        END IF
c        PMAX_GAP = MAX(PMAX_GAP,ABS(PENE(I)))

C--------exclude high pene in cas of ICONT_R
        IF(ICONT_R(I) >0)THEN
         TOLE =EPS02*AAA
         GAP2 = GAPS(I)+GAP_M(CAND_E(I))
         IF (GAP2 >ZERO) TOLE =MIN(TOLE,GAP2*GAP2)
         IF(PENE(I)*PENE(I) > TOLE) THEN
          PENE(I) = ZERO
         END IF
        END IF!(ICONT_R(I) >0)THEN

        IF(PENE(I) == ZERO)THEN
          ICONTACT(I) = 0
          CYCLE
        ENDIF

c        N1(I) = NQX
c        N2(I) = NQY
c        N3(I) = NQZ

c project triangle on SECONDARY node
        

  !                  a
  !                 / \
  !                /   \           a,b,c position at current time
  !               /   A \          A,B,C projected on SECONDARY node 
  !              /   /\  \
  !             /   /  \  \
  !            /   /    \  \    normal at point a:      
  !           /   /      \  \       Na = a - A
  !          /   /        \  \
  !         /   /          \  \
  !        /   /            \  \
  !       /   B--------------C  \
  !      /                       \
  !     b-------------------------c 
  !                  
  
        NNI = (NQX*NAX + NQY*NAY + NQZ*NAZ)
        NI2  = NAX*NAX + NAY*NAY + NAZ*NAZ
        IF(TWO*NNI*NNI < NI2)THEN
c         scharp angle bound nodal normal to 45 from segment normal
          AAA = SQRT(NI2-NNI*NNI) - NNI
          NAX = NAX + AAA*NQX
          NAY = NAY + AAA*NQY
          NAZ = NAZ + AAA*NQZ
          NNI = (NQX*NAX + NQY*NAY + NQZ*NAZ)
        ENDIF

        AAA = PENE(I)/NNI
        NAX = NAX*AAA
        NAY = NAY*AAA
        NAZ = NAZ*AAA

        XPA = XA(I) - NAX
        YPA = YA(I) - NAY
        ZPA = ZA(I) - NAZ

        NNI = (NQX*NBX + NQY*NBY + NQZ*NBZ)
        NI2  = NBX*NBX + NBY*NBY + NBZ*NBZ
        IF(TWO*NNI*NNI < NI2)THEN
c         scharp angle bound nodal normal to 45 from segment normal
          AAA = SQRT(NI2-NNI*NNI) - NNI
          NBX = NBX + AAA*NQX
          NBY = NBY + AAA*NQY
          NBZ = NBZ + AAA*NQZ
          NNI = (NQX*NBX + NQY*NBY + NQZ*NBZ)
        ENDIF

        AAA = PENE(I)/NNI
        NBX = NBX*AAA
        NBY = NBY*AAA
        NBZ = NBZ*AAA

        XPB = XB(I) - NBX
        YPB = YB(I) - NBY
        ZPB = ZB(I) - NBZ

        NNI = (NQX*NCX + NQY*NCY + NQZ*NCZ)
        NI2  = NCX*NCX + NCY*NCY + NCZ*NCZ
        IF(TWO*NNI*NNI < NI2)THEN
c         scharp angle bound nodal normal to 45 from segment normal
          AAA = SQRT(NI2-NNI*NNI) - NNI
          NCX = NCX + AAA*NQX
          NCY = NCY + AAA*NQY
          NCZ = NCZ + AAA*NQZ
          NNI = (NQX*NCX + NQY*NCY + NQZ*NCZ)
        ENDIF

        AAA = PENE(I)/NNI
        NCX = NCX*AAA
        NCY = NCY*AAA
        NCZ = NCZ*AAA

        XPC = XC(I) - NCX
        YPC = YC(I) - NCY
        ZPC = ZC(I) - NCZ

        NNX=(YPB-YPA)*(ZPC-ZPA) - (ZPB-ZPA)*(YPC-YPA)
        NNY=(ZPB-ZPA)*(XPC-XPA) - (XPB-XPA)*(ZPC-ZPA)
        NNZ=(XPB-XPA)*(YPC-YPA) - (YPB-YPA)*(XPC-XPA)
        AAA = NNX*NQX + NNY*NQY + NNZ*NQZ
        IF(AAA <= ZERO)THEN
          NAX = NQX*PENE(I)
          NAY = NQY*PENE(I)
          NAZ = NQZ*PENE(I)
          NBX = NAX
          NBY = NAY
          NBZ = NAZ
          NCX = NAX
          NCY = NAY
          NCZ = NAZ
          XPA = XA(I) - NAX
          YPA = YA(I) - NAY
          ZPA = ZA(I) - NAZ
          XPB = XB(I) - NBX
          YPB = YB(I) - NBY
          ZPB = ZB(I) - NBZ
          XPC = XC(I) - NCX
          YPC = YC(I) - NCY
          ZPC = ZC(I) - NCZ
        ENDIF

        XAB = XPB-XPA
        YAB = YPB-YPA
        ZAB = ZPB-ZPA
        XBC = XPC-XPB
        YBC = YPC-YPB
        ZBC = ZPC-ZPB
        XCA = XPA-XPC
        YCA = YPA-YPC
        ZCA = ZPA-ZPC

        XIA = XPA-XI(I)
        YIA = YPA-YI(I)
        ZIA = ZPA-ZI(I)
        XIB = XPB-XI(I)
        YIB = YPB-YI(I)
        ZIB = ZPB-ZI(I)
        XIC = XPC-XI(I)
        YIC = YPC-YI(I)
        ZIC = ZPC-ZI(I)
        SX = - YAB*ZCA + ZAB*YCA
        SY = - ZAB*XCA + XAB*ZCA
        SZ = - XAB*YCA + YAB*XCA
        S2 = SX*SX+SY*SY+SZ*SZ
        SAX = YIB*ZIC - ZIB*YIC
        SAY = ZIB*XIC - XIB*ZIC
        SAZ = XIB*YIC - YIB*XIC
        LA(I) = (SX*SAX+SY*SAY+SZ*SAZ)/S2
        SBX = YIC*ZIA - ZIC*YIA
        SBY = ZIC*XIA - XIC*ZIA
        SBZ = XIC*YIA - YIC*XIA
        LB(I) = (SX*SBX+SY*SBY+SZ*SBZ)/S2
        LC(I) = ONE - LA(I) - LB(I)

        IF(LA(I)>=ZERO.and.LB(I)>=ZERO.and.LC(I)>=ZERO)THEN
c interpolation des normales
c avec surponderation du point central (quadrangles)
c avec surponderation de la facette (triangles)
c        goto 2345
c          add ici
 2345     continue
          IF (IKEEP==1) THEN
           PENE(I) = ZERO
           ICONTACT(I) = 0
           CYCLE
          END IF
         N1(I) = NQX
         N2(I) = NQY
         N3(I) = NQZ
        ELSE
C--------return to seg before sliding +
         BBB = PENE_TM(I)-PENE(I)
         IF (BBB>TOL_B.AND.LA(I)<ZERO) THEN
          PENE(I) = PENE_TM(I)
          ITQ = SUBTRIA_TM(I)
C---------for implicit          
          SUBTRIA(I) = ITQ
          IX1(I) = IXX(I,IC( 7,ITQ))
          IX2(I) = IXX(I,IC( 8,ITQ))
          IX3(I) = IXX(I,IC( 9,ITQ))
          IX4(I) = IXX(I,IC(10,ITQ))
          X1(I) = XX0(I,IC( 7,ITQ))
          X2(I) = XX0(I,IC( 8,ITQ))
          X3(I) = XX0(I,IC( 9,ITQ))
          X4(I) = XX0(I,IC(10,ITQ))
          Y1(I) = YY0(I,IC( 7,ITQ))
          Y2(I) = YY0(I,IC( 8,ITQ))
          Y3(I) = YY0(I,IC( 9,ITQ))
          Y4(I) = YY0(I,IC(10,ITQ))
          Z1(I) = ZZ0(I,IC( 7,ITQ))
          Z2(I) = ZZ0(I,IC( 8,ITQ))
          Z3(I) = ZZ0(I,IC( 9,ITQ))
          Z4(I) = ZZ0(I,IC(10,ITQ))
          
          VX1(I) = VX(I,IC( 7,ITQ))
          VX2(I) = VX(I,IC( 8,ITQ))
          VX3(I) = VX(I,IC( 9,ITQ))
          VX4(I) = VX(I,IC(10,ITQ))
          VY1(I) = VY(I,IC( 7,ITQ))
          VY2(I) = VY(I,IC( 8,ITQ))
          VY3(I) = VY(I,IC( 9,ITQ))
          VY4(I) = VY(I,IC(10,ITQ))
          VZ1(I) = VZ(I,IC( 7,ITQ))
          VZ2(I) = VZ(I,IC( 8,ITQ))
          VZ3(I) = VZ(I,IC( 9,ITQ))
          VZ4(I) = VZ(I,IC(10,ITQ))

          LA(I) = LA_TM(I) 
          LB(I) = LB_TM(I) 
          LC(I) = ONE - LA(I) - LB(I)
          IF(LA(I)<ZERO)THEN
            IF(LB(I)<ZERO)THEN
              LA(I) = ZERO
              LB(I) = ZERO
              LC(I) = ONE
            ELSEIF(LC(I)<ZERO)THEN
              LC(I) = ZERO
              LA(I) = ZERO
              LB(I) = ONE
            ELSE
              LA(I) = ZERO
              AAA = LB(I) + LC(I)
              LB(I) = LB(I)/AAA
              LC(I) = LC(I)/AAA
            ENDIF
          ELSEIF(LB(I)<ZERO)THEN
            IF(LC(I)<ZERO)THEN
              LB(I) = ZERO
              LC(I) = ZERO
              LA(I) = ONE
            ELSE
              LB(I) = ZERO
              AAA = LC(I) + LA(I)
              LC(I) = LC(I)/AAA
              LA(I) = LA(I)/AAA
            ENDIF
          ELSEIF(LC(I)<ZERO)THEN
              LC(I) = ZERO
              AAA = LA(I) + LB(I)
              LA(I) = LA(I)/AAA
              LB(I) = LB(I)/AAA
          ENDIF
          
          IF    (IX1(I) == IX2(I))THEN
            H1(I) = LA(I)
            H2(I) = ZERO
            H3(I) = LB(I) 
            H4(I) = LC(I)
          ELSEIF(IX2(I) == IX3(I))THEN
            H1(I) = LA(I)
            H2(I) = LB(I)
            H3(I) = ZERO 
            H4(I) = LC(I)
c          ELSEIF(IX3(I) == IX4(I))THEN impossible
          ELSEIF(IX4(I) == IX1(I))THEN
            H1(I) = LC(I)
            H2(I) = LA(I)
            H3(I) = LB(I) 
            H4(I) = ZERO
          ELSE
            H0    = FOURTH * LA(I)
            H1(I) = H0
            H2(I) = H0 
            H3(I) = H0 + LB(I)
            H4(I) = H0 + LC(I) 
          ENDIF
           N1(I) = N_TM(1,I) 
           N2(I) = N_TM(2,I) 
           N3(I) = N_TM(3,I) 
          CYCLE
         END IF          
         N1(I) = NQX
         N2(I) = NQY
         N3(I) = NQZ
          IF(LA(I)<ZERO.and.LB(I)<ZERO)THEN
            LA(I) = ZERO
            LB(I) = ZERO
            LC(I) = ONE
          ELSEIF(LB(I)<ZERO.and.LC(I)<ZERO)THEN
            LB(I) = ZERO
            LC(I) = ZERO
            LA(I) = ONE
          ELSEIF(LC(I)<ZERO.and.LA(I)<ZERO)THEN
            LC(I) = ZERO
            LA(I) = ZERO
            LB(I) = ONE
          ELSEIF(LA(I)<ZERO)THEN
            LA(I) = ZERO
            AAA = LB(I) + LC(I)
            LB(I) = LB(I)/AAA
            LC(I) = LC(I)/AAA
          ELSEIF(LB(I)<ZERO)THEN
            LB(I) = ZERO
            AAA = LC(I) + LA(I)
            LC(I) = LC(I)/AAA
            LA(I) = LA(I)/AAA
          ELSEIF(LC(I)<ZERO)THEN
            LC(I) = ZERO
            AAA = LA(I) + LB(I)
            LA(I) = LA(I)/AAA
            LB(I) = LB(I)/AAA
          ENDIF
          goto 3456
          XPI = LA(I)*XA(I) + LB(I)*XB(I) + LC(I)*XC(I)
          YPI = LA(I)*YA(I) + LB(I)*YB(I) + LC(I)*YC(I)
          ZPI = LA(I)*ZA(I) + LB(I)*ZB(I) + LC(I)*ZC(I)
          N1(I) = XPI - XI(I)
          N2(I) = YPI - YI(I)
          N3(I) = ZPI - ZI(I)
          PENE(I) = SQRT(N1(I)**2 + N2(I)**2 + N3(I)**2)
          S2 = ONE/MAX(EM30,PENE(I))
          N1(I) = N1(I)*S2
          N2(I) = N2(I)*S2
          N3(I) = N3(I)*S2
 3456     continue
        ENDIF
c interpolation des normales seulement en glissement et en option avec implicite
C------issue w/ sp too small value of S2----
        IF ((IMPL_S>0 .AND. ITTOFF>0))  THEN
C        IF (ISPT2(I)>0.OR.(IMPL_S>0 .AND. ITTOFF>0))  THEN
c          IF ((ITQ==1 .or. ITQ==2  .or. ITQ==3 .or. ITQ==4).and.
c     .        (IXX(I,3) /= IXX(I,4)))THEN
            OVERW = OVERW0
            N1(I) = LA(I)*NAX + LB(I)*NBX + LC(I)*NCX
            N2(I) = LA(I)*NAY + LB(I)*NBY + LC(I)*NCY
            N3(I) = LA(I)*NAZ + LB(I)*NBZ + LC(I)*NCZ
c          ELSE
c            OVERW = OVERW0*LA(I)*LB(I)*LC(I)
c            N1(I) = OVERW*NQX + LA(I)*NAX + LB(I)*NBX + LC(I)*NCX  
c            N2(I) = OVERW*NQY + LA(I)*NAY + LB(I)*NBY + LC(I)*NCY
c            N3(I) = OVERW*NQZ + LA(I)*NAZ + LB(I)*NBZ + LC(I)*NCZ
c          ENDIF

          S2 = ONE/MAX(EM30,SQRT(N1(I)**2 + N2(I)**2 + N3(I)**2))
          N1(I) = N1(I)*S2
          N2(I) = N2(I)*S2
          N3(I) = N3(I)*S2
        
          AAA = NQX*N1(I) + NQY*N2(I) + NQZ*N3(I)
          AAA = MAX(ONE,ONE/MAX(AAA,EM20))
          PENE(I) = PENE(I)*AAA

c        if(pene(i) > 1.e17)then
c          ibug=1
c          stop 9876
c        endif

        END IF !(IMPL_S>0 .AND. ITTOFF>0)  THEN

        IX1(I) = IXX(I,IC( 7,ITQ))
        IX2(I) = IXX(I,IC( 8,ITQ))
        IX3(I) = IXX(I,IC( 9,ITQ))
        IX4(I) = IXX(I,IC(10,ITQ))

        X1(I)  = XX0(I,IC( 7,ITQ))
        X2(I)  = XX0(I,IC( 8,ITQ))
        X3(I)  = XX0(I,IC( 9,ITQ))
        X4(I)  = XX0(I,IC(10,ITQ))
        Y1(I)  = YY0(I,IC( 7,ITQ))
        Y2(I)  = YY0(I,IC( 8,ITQ))
        Y3(I)  = YY0(I,IC( 9,ITQ))
        Y4(I)  = YY0(I,IC(10,ITQ))
        Z1(I)  = ZZ0(I,IC( 7,ITQ))
        Z2(I)  = ZZ0(I,IC( 8,ITQ))
        Z3(I)  = ZZ0(I,IC( 9,ITQ))
        Z4(I)  = ZZ0(I,IC(10,ITQ))

        VX1(I) = VX(I,IC( 7,ITQ))
        VX2(I) = VX(I,IC( 8,ITQ))
        VX3(I) = VX(I,IC( 9,ITQ))
        VX4(I) = VX(I,IC(10,ITQ))
        VY1(I) = VY(I,IC( 7,ITQ))
        VY2(I) = VY(I,IC( 8,ITQ))
        VY3(I) = VY(I,IC( 9,ITQ))
        VY4(I) = VY(I,IC(10,ITQ))
        VZ1(I) = VZ(I,IC( 7,ITQ))
        VZ2(I) = VZ(I,IC( 8,ITQ))
        VZ3(I) = VZ(I,IC( 9,ITQ))
        VZ4(I) = VZ(I,IC(10,ITQ))

        IF    (IX1(I) == IX2(I))THEN
          H1(I) = LA(I)
          H2(I) = ZERO
          H3(I) = LB(I) 
          H4(I) = LC(I)
        ELSEIF(IX2(I) == IX3(I))THEN
          H1(I) = LA(I)
          H2(I) = LB(I)
          H3(I) = ZERO 
          H4(I) = LC(I)
c       ELSEIF(IX3(I) == IX4(I))THEN impossible
        ELSEIF(IX4(I) == IX1(I))THEN
          H1(I) = LC(I)
          H2(I) = LA(I)
          H3(I) = LB(I) 
          H4(I) = ZERO
        ELSE
          H0    = FOURTH * LA(I)
          H1(I) = H0
          H2(I) = H0 
          H3(I) = H0 + LB(I)
          H4(I) = H0 + LC(I) 
        ENDIF
C          IF (INCONV /= 1) CYCLE

        N=CAND_N(I)

!        if(nsvg(i) == ix1(i).or.nsvg(i) == ix2(i).or.
!     .     nsvg(i) == ix3(i).or.nsvg(i) == ix4(i))then
!            write(iout,*)'1 t=',tt,'i=',i,'nsvg(i)',nsvg(i)
!            write(iout,*)'ix1=',ix1(i),'ix2=',ix2(i)
!            write(iout,*)'ix3=',ix3(i),'ix4=',ix4(i)
!            pene(i) = zero
!            H1(I) = zero
!            H2(I) = zero
!            H3(I) = zero
!            H4(I) = zero 
!            ICONTACT(I) = 0
!            cycle
!        endif

c        IF(N <= NSN)THEN
c           MGLOB = IRTLM(1,N)
c        ELSE
c           MGLOB = IRTLM_FI(NIN)%P(1,N-NSN)
c        ENDIF
c        IF(ICONTACT(I)==MGLOB)THEN
c       ICONTACT(I) == IRTLM > 0 old contact, new segment
c       ICONTACT(I) == IRTLM < 0 new contact, new segment, retained provisional  
c       ICONTACT(I) /= IRTLM < 0 new contact, new segment, not retained
        MGLOB = MSEGLO(CAND_E(I))
        IF    (ITQ==5 .or. ITQ==9  .or. ITQ==13 .or. ITQ==17)THEN
            MGLOB = MVOISIN(1,CAND_E(I))
            ITQ = ITRIV(1,I)
        ELSEIF(ITQ==6 .or. ITQ==10 .or. ITQ==14 .or. ITQ==18)THEN
            MGLOB = MVOISIN(2,CAND_E(I))
            ITQ = ITRIV(2,I)
        ELSEIF(ITQ==7 .or. ITQ==11 .or. ITQ==15 .or. ITQ==19)THEN
            MGLOB = MVOISIN(3,CAND_E(I))
            ITQ = ITRIV(3,I)
        ELSEIF(ITQ==8 .or. ITQ==12 .or. ITQ==16 .or. ITQ==20)THEN
            MGLOB = MVOISIN(4,CAND_E(I))
            ITQ = ITRIV(4,I)
        ENDIF
        IF(N <= NSN)THEN
         IF(IRTLM(1,N)>0) THEN
            IRTLM(1,N) = MGLOB
            TIME_S(N) = -EP20
            IRTLM(2,N)= ITQ   !sub triangle
c         ELSEIF((TIME_S(N) < ADT0(I)) .or.  ! multi or new intersection
c     .          (TIME_S(N) == ADT0(I) .and.  ! multi at same time
c     .         -IRTLM(1,N) < MGLOB ) )THEN
c            IRTLM(2,N)= ITQ   !sub triangle
c            IRTLM(1,N) = -MGLOB
c            TIME_S(N) = ADT0(I)
         ELSEIF((TIME_S(N) < PENE(I)) .or.  ! multi or new intersection
     .          (TIME_S(N) == PENE(I) .and.  ! multi at same time
     .         -IRTLM(1,N) < MGLOB ) )THEN
            IRTLM(2,N)= ITQ   !sub triangle
            IRTLM(1,N) = -MGLOB
            TIME_S(N) = PENE(I)
         ENDIF
        ELSE         ! Remote nodes in SPMD
          II=N-NSN
          IF(IRTLM_FI(NIN)%P(1,II) > 0)THEN
            IRTLM_FI(NIN)%P(1,II) = MGLOB
            TIME_SFI(NIN)%P(II) = -EP20
            IRTLM_FI(NIN)%P(2,II) = ITQ
          ELSEIF((TIME_SFI(NIN)%P(II) < PENE(I)).or.
     .           (TIME_SFI(NIN)%P(II)== PENE(I).and.
     .           -IRTLM_FI(NIN)%P(1,II) < MGLOB) )THEN
              IRTLM_FI(NIN)%P(2,II) = ITQ 
              IRTLM_FI(NIN)%P(1,II) = -MGLOB
              TIME_SFI(NIN)%P(II) = PENE(I)
          ENDIF
        ENDIF
      ENDDO

c debug
!!!      goto 123      
 123  continue
C=======================================================================
c       special plan/plan: node/edge
C=======================================================================
! old contacts look at if sliding on another edge
! +----+----------+----------+-------------+
! | IC                                     |
! +----+----------+----------+-------------+
! | ST | noeuds T | noeuds TV| noeuds Quad |
! +----+----------+----------+-------------+        11-------10       
! |  1 |  5  1  2 | 14  3  4 |  3  4  1  2 |         |\ 19  /|        
! |  2 |  5  2  3 | 15  4  1 |  4  1  2  3 |         | \   / |                                    
! |  3 |  5  3  4 | 16  1  2 |  1  2  3  4 |         |  \ /  |           
! |  4 |  5  4  1 | 17  2  3 |  2  3  4  1 |         |  16   |           
! +----+----------+----------+-------------+         |15/ \11|        
! |  5 | 14  2  1 |  5  6  7 |  6  7  2  1 |         | /   \ |      
! |  6 | 15  3  2 |  5  8  9 |  8  9  3  2 |         |/  7  \|       
! |  7 | 16  4  3 |  5 10 11 | 10 11  4  3 |12-------4-------3-------9
! |  8 | 17  1  4 |  5 12 13 | 12 13  1  4 | |\ 12  /|\     /|\ 14  /|
! +----+----------+----------+-------------+ | \   / | \ 3 / | \   / |
! |  9 | 14  1  6 |(13) 7  2 |  7  2  1  6 | |  \ /  |  \ /2 |6 \ /18|
! | 10 | 15  2  8 |( 7) 9  3 |  9  3  2  8 | |  17   |   5   |  15   |
! | 11 | 16  3 10 |( 9)11  4 | 11  4  3 10 | |20/ \ 8| 4/ \  |  / \  |
! | 12 | 17  4 12 |(11)13  1 | 13  1  4 12 | | /   \ | / 1 \ | /   \ |
! +----+----------+----------+-------------+ |/ 16  \|/     \|/ 10  \|
! | 13 | 14  7  2 |( 8) 1  6 |  1  6  7  2 |13-------1-------2-------8
! | 14 | 15  9  3 |(10) 2  8 |  2  8  9  3 |         |\  5  /|       
! | 15 | 16 11  4 |(12) 3 10 |  3 10 11  4 |         | \   / |      
! | 16 | 17 13  1 |( 6) 4 12 |  4 12 13  1 |         |9 \ /13|    
! +----+----------+----------+-------------+         |  14   |    
! | 17 | 14  6  7 |  -  2  1 |  2  1  6  7 |         |  / \  |   
! | 18 | 15  8  9 |  -  3  2 |  3  2  8  9 |         | /   \ |  
! | 19 | 16 10 11 |  -  4  3 |  4  3 10 11 |         |/ 17  \| 
! | 20 | 17 12 13 |  -  1  4 |  1  4 12 13 |         6-------7
! +----+----------+----------+-------------+
!   sliding of old contacts, SUBTRIA(I)>20: new ones
      DO I=1,JLT
       IF(ISTEP(I) /= 6 .OR. SUBTRIA(I)>20)CYCLE
C old contact ITQ =1-4, edge BC determine if FB BC CG as final one
        ITQ = SUBTRIA(I)
        IF (IXX(I,4)==IXX(I,3)) ITQ =1
        L = CAND_E(I)
        K = IC(1,ITQ)  ! 5
        XA(I) = XX0(I,K)   
        YA(I) = YY0(I,K)    
        ZA(I) = ZZ0(I,K) 

        K = IC(2,ITQ)  ! N1
        XB(I) = XX0(I,K)   
        YB(I) = YY0(I,K)    
        ZB(I) = ZZ0(I,K)   

        K = IC(3,ITQ)  ! N2
        XC(I) = XX0(I,K)   
        YC(I) = YY0(I,K)    
        ZC(I) = ZZ0(I,K) 
C        
        K = IC(5,ITQ)  ! N3
        XD(I) = XX0(I,K)   
        YD(I) = YY0(I,K)    
        ZD(I) = ZZ0(I,K)   

        K = IC(6,ITQ)  ! N4
        XE(I) = XX0(I,K)   
        YE(I) = YY0(I,K)    
        ZE(I) = ZZ0(I,K)   
        XIB = XB(I)-XI(I)
        YIB = YB(I)-YI(I)
        ZIB = ZB(I)-ZI(I)
        XBC = XC(I)-XB(I)
        YBC = YC(I)-YB(I)
        ZBC = ZC(I)-ZB(I)
        AAA = XIB*XBC+YIB*YBC+ZIB*ZBC
        XBD = XD(I)-XB(I)
        YBD = YD(I)-YB(I)
        ZBD = ZD(I)-ZB(I)
        XCE = XE(I)-XC(I)
        YCE = YE(I)-YC(I)
        ZCE = ZE(I)-ZC(I)
        BBB = XIB*XBD+YIB*YBD+ZIB*ZBD
        SUBTRIA_N(I) = 0
C----- sliding around B
        IF (AAA>ZERO) THEN
          IF(ITQ ==1.AND.MVOISIN(4,L)>0)THEN
            SUBTRIA_N(I) = 16
          ELSEIF(ITQ ==2.AND.MVOISIN(1,L)>0)THEN
            SUBTRIA_N(I) = 13
          ELSEIF(ITQ ==3.AND.MVOISIN(2,L)>0)THEN
            SUBTRIA_N(I) = 14
          ELSEIF(ITQ ==4.AND.MVOISIN(3,L)>0)THEN
            SUBTRIA_N(I) = 15
C----- sliding out
          ELSE
            ICONTACT(I) = 0
            ISTEP(I) = 0
            CYCLE
          ENDIF
C----- internal sliding
        ELSEIF (BBB<ZERO) THEN
          NNI =ZERO
          IF(ITQ ==1.AND.MVOISIN(4,L)==0)THEN
            NQX = SX4150(I)
            NQY = SY4150(I)
            NQZ = SZ4150(I)
            NNI = (NQY*ZBD - NQZ*YBD)*XIB+(NQZ*XBD - NQX*ZBD)*YIB+
     .            (NQX*YBD - NQY*XBD)*ZIB
            IF (NNI<ZERO.AND.BBB<AAA) SUBTRIA_N(I) = 4
          ELSEIF(ITQ ==2.AND.MVOISIN(1,L)==0)THEN
            NQX = SX1250(I)
            NQY = SY1250(I)
            NQZ = SZ1250(I)
            NNI = (NQY*ZBD - NQZ*YBD)*XIB+(NQZ*XBD - NQX*ZBD)*YIB+
     .            (NQX*YBD - NQY*XBD)*ZIB
            IF (NNI<ZERO.AND.BBB<AAA) SUBTRIA_N(I) = 1
          ELSEIF(ITQ ==3.AND.MVOISIN(2,L)==0)THEN
            NQX = SX3450(I)
            NQY = SY3450(I)
            NQZ = SZ3450(I)
            NNI = (NQY*ZBD - NQZ*YBD)*XIB+(NQZ*XBD - NQX*ZBD)*YIB+
     .            (NQX*YBD - NQY*XBD)*ZIB
            IF (NNI<ZERO.AND.BBB<AAA) SUBTRIA_N(I) = 2
          ELSEIF(ITQ ==4.AND.MVOISIN(3,L)==0)THEN
            NQX = SX2350(I)
            NQY = SY2350(I)
            NQZ = SZ2350(I)
            NNI = (NQY*ZBD - NQZ*YBD)*XIB+(NQZ*XBD - NQX*ZBD)*YIB+
     .            (NQX*YBD - NQY*XBD)*ZIB
            IF (NNI<ZERO.AND.BBB<AAA) SUBTRIA_N(I) = 3
          ENDIF
          IF (NNI>ZERO) THEN
            ICONTACT(I) = 0
            ISTEP(I) = 0
            CYCLE
          END IF
        END IF 
C-
        XIC = XC(I)-XI(I)
        YIC = YC(I)-YI(I)
        ZIC = ZC(I)-ZI(I)
        AAA = XIC*XBC+YIC*YBC+ZIC*ZBC
        BBB = XIC*XCE+YIC*YCE+ZIC*ZCE
C----- sliding around C
        IF (AAA<ZERO.AND.SUBTRIA_N(I)==0) THEN
          IF(ITQ ==1.AND.MVOISIN(2,L)>0)THEN
            SUBTRIA_N(I) = 10
          ELSEIF(ITQ ==2.AND.MVOISIN(3,L)>0)THEN
            SUBTRIA_N(I) = 11
          ELSEIF(ITQ ==3.AND.MVOISIN(4,L)>0)THEN
            SUBTRIA_N(I) = 12
          ELSEIF(ITQ ==4.AND.MVOISIN(1,L)>0)THEN
            SUBTRIA_N(I) = 9
C----- sliding out
          ELSE
            ICONTACT(I) = 0
            ISTEP(I) = 0
            CYCLE
          ENDIF
C----- internal sliding
        ELSEIF (BBB>ZERO) THEN
          NNI =ZERO
          IF(ITQ ==1.AND.MVOISIN(2,L)==0)THEN
            NQX = SX2350(I)
            NQY = SY2350(I)
            NQZ = SZ2350(I)
            NNI = (NQY*ZCE - NQZ*YCE)*XIC+(NQZ*XCE - NQX*ZCE)*YIC+
     .            (NQX*YCE - NQY*XCE)*ZIC
            IF (NNI<ZERO.AND.BBB>AAA) SUBTRIA_N(I) = 2
          ELSEIF(ITQ ==2.AND.MVOISIN(1,L)==0)THEN
            NQX = SX3450(I)
            NQY = SY3450(I)
            NQZ = SZ3450(I)
            NNI = (NQY*ZCE - NQZ*YCE)*XIC+(NQZ*XCE - NQX*ZCE)*YIC+
     .            (NQX*YCE - NQY*XCE)*ZIC
            IF (NNI<ZERO.AND.BBB>AAA) SUBTRIA_N(I) = 3
          ELSEIF(ITQ ==3.AND.MVOISIN(2,L)==0)THEN
            NQX = SX4150(I)
            NQY = SY4150(I)
            NQZ = SZ4150(I)
            NNI = (NQY*ZCE - NQZ*YCE)*XIC+(NQZ*XCE - NQX*ZCE)*YIC+
     .            (NQX*YCE - NQY*XCE)*ZIC
            IF (NNI<ZERO.AND.BBB>AAA) SUBTRIA_N(I) = 4
          ELSEIF(ITQ ==4.AND.MVOISIN(3,L)==0)THEN
            NQX = SX1250(I)
            NQY = SY1250(I)
            NQZ = SZ1250(I)
            NNI = (NQY*ZCE - NQZ*YCE)*XIC+(NQZ*XCE - NQX*ZCE)*YIC+
     .            (NQX*YCE - NQY*XCE)*ZIC
            IF (NNI<ZERO.AND.BBB>AAA) SUBTRIA_N(I) = 1
          ENDIF
          IF (NNI>ZERO) THEN
            ICONTACT(I) = 0
            ISTEP(I) = 0
            CYCLE
          END IF
        END IF
        IF (SUBTRIA_N(I)>0) SUBTRIA(I) = SUBTRIA_N(I)        
        SUBTRIA_N(I) = ITQ        
      END DO 
C------possible nodal normal with interpolation after
      DO I=1,JLT
       IF(ISTEP(I) /= 6 )CYCLE
C----   finalizing penetration for node/edge possible multi-contactcase for new contact      
        ITQ = SUBTRIA(I)
       IF (ITQ>20) ITQ = ITQ -20        
C       EDGE : BC
        K = IC(1,ITQ)  ! 5
        XA(I) = XX0(I,K)   
        YA(I) = YY0(I,K)    
        ZA(I) = ZZ0(I,K) 

        K = IC(2,ITQ)  ! N1
        XB(I) = XX0(I,K)   
        YB(I) = YY0(I,K)    
        ZB(I) = ZZ0(I,K)   

        K = IC(3,ITQ)  ! N2
        XC(I) = XX0(I,K)   
        YC(I) = YY0(I,K)    
        ZC(I) = ZZ0(I,K)   
C--------
        XBC = XC(I)-XB(I)
        YBC = YC(I)-YB(I)
        ZBC = ZC(I)-ZB(I)
C--- edge normal
        IF(ITQ ==1)THEN
         NQX = SX1250(I)
         NQY = SY1250(I)
         NQZ = SZ1250(I)
        ELSEIF(ITQ ==2)THEN
         NQX = SX2350(I)
         NQY = SY2350(I)
         NQZ = SZ2350(I)
        ELSEIF(ITQ ==3)THEN
         NQX = SX3450(I)
         NQY = SY3450(I)
         NQZ = SZ3450(I)
        ELSEIF(ITQ ==4)THEN
         NQX = SX4150(I)
         NQY = SY4150(I)
         NQZ = SZ4150(I)
        ELSE
C--- re-compute seg normal for sliding case
         XAB = XB(I)-XA(I)
         YAB = YB(I)-YA(I)
         ZAB = ZB(I)-ZA(I)
         NQX = YAB*ZBC - ZAB*YBC
         NQY = ZAB*XBC - XAB*ZBC
         NQZ = XAB*YBC - YAB*XBC
        ENDIF
        NX(I) = NQY*ZBC - NQZ*YBC
        NY(I) = NQZ*XBC - NQX*ZBC
        NZ(I) = NQX*YBC - NQY*XBC
C       in right direction
        NQX = -NX(I)
        NQY = -NY(I)
        NQZ = -NZ(I)
        
        AAA = MAX(EM30,SQRT(NQX*NQX + NQY*NQY + NQZ*NQZ))
        S2 = ONE/AAA
        NQX = NQX*S2
        NQY = NQY*S2
        NQZ = NQZ*S2

        BBB = (XI(I)-XB(I))*NQX+(YI(I)-YB(I))*NQY+(ZI(I)-ZB(I))*NQZ
        PENE(I) = -MIN(ZERO,BBB)
        IF(PENE(I) == ZERO)THEN
          ICONTACT(I) = 0
          CYCLE
        ENDIF
        N1(I) = NQX
        N2(I) = NQY
        N3(I) = NQZ
        IX1(I) = IXX(I,IC( 7,ITQ))
        IX2(I) = IXX(I,IC( 8,ITQ))
        IX3(I) = IXX(I,IC( 9,ITQ))
        IX4(I) = IXX(I,IC(10,ITQ))

        X1(I)  = XX0(I,IC( 7,ITQ))
        X2(I)  = XX0(I,IC( 8,ITQ))
        X3(I)  = XX0(I,IC( 9,ITQ))
        X4(I)  = XX0(I,IC(10,ITQ))
        Y1(I)  = YY0(I,IC( 7,ITQ))
        Y2(I)  = YY0(I,IC( 8,ITQ))
        Y3(I)  = YY0(I,IC( 9,ITQ))
        Y4(I)  = YY0(I,IC(10,ITQ))
        Z1(I)  = ZZ0(I,IC( 7,ITQ))
        Z2(I)  = ZZ0(I,IC( 8,ITQ))
        Z3(I)  = ZZ0(I,IC( 9,ITQ))
        Z4(I)  = ZZ0(I,IC(10,ITQ))

        VX1(I) = VX(I,IC( 7,ITQ))
        VX2(I) = VX(I,IC( 8,ITQ))
        VX3(I) = VX(I,IC( 9,ITQ))
        VX4(I) = VX(I,IC(10,ITQ))
        VY1(I) = VY(I,IC( 7,ITQ))
        VY2(I) = VY(I,IC( 8,ITQ))
        VY3(I) = VY(I,IC( 9,ITQ))
        VY4(I) = VY(I,IC(10,ITQ))
        VZ1(I) = VZ(I,IC( 7,ITQ))
        VZ2(I) = VZ(I,IC( 8,ITQ))
        VZ3(I) = VZ(I,IC( 9,ITQ))
        VZ4(I) = VZ(I,IC(10,ITQ))
        H1(I) = ZERO
        H2(I) = ZERO
        H3(I) = HALF 
        H4(I) = HALF

        N=CAND_N(I)
        MGLOB = MSEGLO(CAND_E(I))
        IF    (ITQ==5 .or. ITQ==9  .or. ITQ==13 .or. ITQ==17)THEN
            MGLOB = MVOISIN(1,CAND_E(I))
c            ITQ = ITRIV(1,I)
        ELSEIF(ITQ==6 .or. ITQ==10 .or. ITQ==14 .or. ITQ==18)THEN
            MGLOB = MVOISIN(2,CAND_E(I))
c            ITQ = ITRIV(2,I)
        ELSEIF(ITQ==7 .or. ITQ==11 .or. ITQ==15 .or. ITQ==19)THEN
            MGLOB = MVOISIN(3,CAND_E(I))
c            ITQ = ITRIV(3,I)
        ELSEIF(ITQ==8 .or. ITQ==12 .or. ITQ==16 .or. ITQ==20)THEN
            MGLOB = MVOISIN(4,CAND_E(I))
c            ITQ = ITRIV(4,I)
        ENDIF
        IF  (ITQ>8) ITQ = SUBTRIA_N(I)

        IF(N <= NSN)THEN
         IF(IRTLM(1,N)>0) THEN
            IRTLM(1,N) = MGLOB
            TIME_S(N) = -EP20
            IRTLM(2,N)= ITQ+20   !sub triangle
         ELSEIF((TIME_S(N) < PENE(I)) .or.  ! multi or new intersection
     .          (TIME_S(N) == PENE(I) .and.  ! multi at same time
     .         -IRTLM(1,N) < MGLOB ) )THEN
            IRTLM(2,N)= ITQ+20   !sub triangle
            IRTLM(1,N) = -MGLOB
            TIME_S(N) = PENE(I)
         ENDIF
        ELSE         ! Remote nodes in SPMD
          II=N-NSN
          IF(IRTLM_FI(NIN)%P(1,II) > 0)THEN
            IRTLM_FI(NIN)%P(1,II) = MGLOB
            TIME_SFI(NIN)%P(II) = -EP20
            IRTLM_FI(NIN)%P(2,II) = ITQ+20
          ELSEIF((TIME_SFI(NIN)%P(II) < PENE(I)).or.
     .           (TIME_SFI(NIN)%P(II)== PENE(I).and.
     .           -IRTLM_FI(NIN)%P(1,II) < MGLOB) )THEN
              IRTLM_FI(NIN)%P(2,II) = ITQ+20 
              IRTLM_FI(NIN)%P(1,II) = -MGLOB
              TIME_SFI(NIN)%P(II) = PENE(I)
          ENDIF
        ENDIF
      END DO 
c      write(iout,*)'i24dst3 9'
C=======================================================================
c  6 Reset IRTLM (only for old contact: IRTLM(1,N)>0)
c       and define PENE_OLD(3,N) = PENE-GAP or dist form SECONDARY to surf
C=======================================================================
C        IF (INCONV==1) THEN
      DO I=1,JLT
       CE_LOC(I) =  CAND_E(I)
       CN_LOC(I) =  CAND_N(I)
       IF(PENE(I) == 0 )THEN

         N=CAND_N(I)
         IF(N <= NSN)THEN
c           IF(IRTLM(1,N)==MSEGLO(CAND_E(I)).and.
c     .        IRTLM(1,N)==IRTLM_OLD(I))THEN
           IF(IRTLM(1,N) > 0)THEN
             IRTLM(1,N)=0
             TIME_S(N) = -EP20
             SECND_FR(4,N)= ZERO
             SECND_FR(5,N)= ZERO
             SECND_FR(6,N)= ZERO
             IF (IMCONV==1) PENE_OLD(2,N) = ZERO
             PENE_OLD(5,N) = ZERO
             IF (IMCONV==1) STIF_OLD(2,N) = ZERO
           ENDIF
         ELSE
           IF(IRTLM_FI(NIN)%P(1,N-NSN) > 0)THEN
             IRTLM_FI(NIN)%P(1,N-NSN)=0
             TIME_SFI(NIN)%P(N-NSN) = -EP20
             SECND_FRFI(NIN)%P(4,N-NSN)= ZERO
             SECND_FRFI(NIN)%P(5,N-NSN)= ZERO
             SECND_FRFI(NIN)%P(6,N-NSN)= ZERO
             IF (IMCONV==1) PENE_OLDFI(NIN)%P(2,N-NSN) = ZERO
             PENE_OLDFI(NIN)%P(5,N-NSN) = ZERO
             IF (IMCONV==1) STIF_OLDFI(NIN)%P(2,N-NSN) = ZERO 
           ENDIF
         ENDIF
       ELSE
         NS=NSVG(I)
         IF (NM1(I)==ZERO.AND.NM2(I)==ZERO.AND.NM3(I)==ZERO) THEN
          NM1(I) = N1(I)
          NM2(I) = N2(I)
          NM3(I) = N3(I)
         END IF
C------case huge warped element       
        IF    (IX1(I) == IX2(I))THEN
        ELSEIF(IX2(I) == IX3(I))THEN
c       ELSEIF(IX3(I) == IX4(I))THEN impossible
        ELSEIF(IX4(I) == IX1(I))THEN
        ELSE
C -------Calculate Nz,Area,Z1 IF Z1*Z1/AREA > f(angle 45) CYCLE   
        ENDIF
         XS = H1(I)*X1(I) + H2(I)*X2(I) + H3(I)*X3(I) + H4(I)*X4(I)
         YS = H1(I)*Y1(I) + H2(I)*Y2(I) + H3(I)*Y3(I) + H4(I)*Y4(I)
         ZS = H1(I)*Z1(I) + H2(I)*Z2(I) + H3(I)*Z3(I) + H4(I)*Z4(I)
         XS = XS - XI(I)
         YS = YS - YI(I) 
         ZS = ZS - ZI(I) 
 
         AAA = XS*N1(I) + YS*N2(I) + ZS*N3(I)

         IF(AAA > ZERO)THEN

           AAA = XS**2 + YS**2 + ZS**2
           AAA = ONEP01*SQRT(AAA) 
           PMAX_GAP = MAX(PMAX_GAP,AAA)

           N=CAND_N(I)
           IF(N <= NSN)THEN
             PENE_OLD(3,N) = MAX(PENE_OLD(3,N),AAA)
           ELSE
             PENE_OLDFI(NIN)%P(3,N-NSN) = 
     .                      MAX(PENE_OLDFI(NIN)%P(3,N-NSN),AAA)
           ENDIF
         ENDIF
       ENDIF
      ENDDO
#include "lockoff.inc"
C        END IF !(INCONV==1) THEN
C-----Correction due to difference between starter/engine 
      IF (INACTI==5) THEN
       IF (TT==ZERO) THEN
        DO I=1,JLT
         IF(PENE(I) == ZERO)CYCLE
         JG = NSVG(I)
         N  = CAND_N(I)
         IF(JG > 0)THEN
#include "lockon.inc"
          PENE_OLD(5,N) = MAX( PENE(I) ,PENE_OLD(5,N) )
          STIF_OLD(1,N) = MAX( STIF(I) ,STIF_OLD(1,N) )
#include "lockoff.inc"
         ELSE
          JG = -JG
#include "lockon.inc"
          PENE_OLDFI(NIN)%P(5,JG) = MAX( PENE(I),PENE_OLDFI(NIN)%P(5,JG))
          STIF_OLDFI(NIN)%P(1,JG) = MAX( STIF(I),STIF_OLDFI(NIN)%P(1,JG))
#include "lockoff.inc"
         ENDIF
        ENDDO
C-------zero force for all first contact		
       ELSE
        DO I=1,JLT
         IF(PENE(I) == ZERO)CYCLE
         JG = NSVG(I)
         N  = CAND_N(I)
         IF(JG > 0)THEN
          IF (IPEN0(I)==0)THEN
#include "lockon.inc"
          PENE_OLD(5,N) = MAX( PENE(I) ,PENE_OLD(5,N) )
          STIF_OLD(1,N) = MAX( STIF(I) ,STIF_OLD(1,N) )
#include "lockoff.inc"
          END IF 
         ELSE
          JG = -JG
          IF (IPEN0(I)==0)THEN
#include "lockon.inc"
          PENE_OLDFI(NIN)%P(5,JG) = MAX( PENE(I),PENE_OLDFI(NIN)%P(5,JG))
          STIF_OLDFI(NIN)%P(1,JG) = MAX( STIF(I),STIF_OLDFI(NIN)%P(1,JG))
#include "lockoff.inc"
          END IF 
         ENDIF
        ENDDO
       END IF !(TT==ZERO) THEN
C---------PENE -> PENE_Offset        
       DO I=1,JLT
        IF(PENE(I) == ZERO)CYCLE
        JG = NSVG(I)
        N  = CAND_N(I)
        IF(JG > 0)THEN
          PENE_SH = MAX(ZERO,PENE(I)-PENE_OLD(5,N))
        ELSE
          JG = -JG
          PENE_SH = MAX(ZERO,PENE(I)-PENE_OLDFI(NIN)%P(5,JG))
        ENDIF
C---------PENE -> updated bt PENE_Offset        
        PENE(I) = PENE_SH
       ENDDO
      END IF !IF (INACTI==5)
C-----peneref for nonlinear stif (quadratic...)
      PENREF(1:JLT) = ZERO 
      IF (INACTI==-1) THEN
        DO I=1,JLT
          IF(PENE(I) == ZERO.OR.IKNON(I)==0) CYCLE
          PENREF(I) = SQRT(EPS2*AREA(I))
          JG = NSVG(I)
          N  = CAND_N(I)
          IF(JG > 0)THEN
            PENE_SH = EM01*PENE_OLD(5,N)
          ELSE
            JG = -JG
            PENE_SH = EM01*PENE_OLDFI(NIN)%P(5,JG)
          ENDIF
          PENREF(I) = MAX(PENREF(I),PENE_SH)
        ENDDO
      ELSE
        DO I=1,JLT
          IF(PENE(I) == ZERO) CYCLE
          PENREF(I) = ONE_FIFTH*SQRT(AREA(I))
          JG = NSVG(I)
          N  = CAND_N(I)
          IF(JG > 0)THEN
            PENE_SH = PENE_OLD(5,N)
          ELSE
            JG = -JG
            PENE_SH = PENE_OLDFI(NIN)%P(5,JG)
          ENDIF
          PENREF(I) = MAX(PENREF(I),PENE_SH)
        ENDDO
      END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  I24NEXTTRIA                   source/interfaces/int24/i24dst3.F
Chd|-- called by -----------
Chd|        I24DST3                       source/interfaces/int24/i24dst3.F
Chd|-- calls ---------------
Chd|        I24NEXTTRIA2                  source/interfaces/int24/i24dst3.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24NEXTTRIA(
     1          IZLIM,ISTEP,SUBTRIA_N,SUBTRIA,
     2          LARGEP,PENE ,XXL  ,YYL  ,ZZL  ,
     3          SX125,SY125,SZ125,SX235,SY235,
     4          SZ235,SX345,SY345,SZ345,SX415,
     5          SY415,SZ415,XI   ,YI   ,ZI   ,
     6          N1   ,N2   ,N3   ,LA   ,LB   ,
     7          LC   ,GAP2 ,EPS0 )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISTEP,SUBTRIA_N,SUBTRIA,IZLIM,LARGEP
      my_real
     .     PENE,XXL(17),YYL(17),ZZL(17),XI   ,YI   ,ZI,
     .     SX125,SY125,SZ125,SX235,SY235,
     .     SZ235,SX345,SY345,SZ345,SX415,
     .     SY415,SZ415,N1,N2,N3,LA,LB,LC,GAP2,EPS0
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K,ITQ
      my_real
     .     AAA,BBB,VOL,XA,YA,ZA,XB,YB,ZB,XC,YC,ZC,XD,YD,ZD,
     .     XAB,YAB,ZAB,XBC,YBC,ZBC,XCA,YCA,ZCA,XBD,YBD,ZBD,
     .     XIA,YIA,ZIA,XIB,YIB,ZIB,XIC,YIC,ZIC,SX ,SY, SZ, 
     .     S2 ,SAX,SAY,SAZ,SBX,SBY,SBZ,UNP,ZEROM,EPS
      INTEGER IT0(3,20),IC(10,20)
! +----+----------+----------+-------------+
! | IC                                     |
! +----+----------+----------+-------------+
! | ST | noeuds T | noeuds TV| noeuds Quad |
! +----+----------+----------+-------------+        11-------10       
! |  1 |  5  1  2 | 14  3  4 |  3  4  1  2 |         |\ 19  /|        
! |  2 |  5  2  3 | 15  4  1 |  4  1  2  3 |         | \   / |                                    
! |  3 |  5  3  4 | 16  1  2 |  1  2  3  4 |         |  \ /  |           
! |  4 |  5  4  1 | 17  2  3 |  2  3  4  1 |         |  16   |           
! +----+----------+----------+-------------+         |15/ \11|        
! |  5 | 14  2  1 |  5  6  7 |  6  7  2  1 |         | /   \ |      
! |  6 | 15  3  2 |  5  8  9 |  8  9  3  2 |         |/  7  \|       
! |  7 | 16  4  3 |  5 10 11 | 10 11  4  3 |12-------4-------3-------9
! |  8 | 17  1  4 |  5 12 13 | 12 13  1  4 | |\ 12  /|\     /|\ 14  /|
! +----+----------+----------+-------------+ | \   / | \ 3 / | \   / |
! |  9 | 14  1  6 |(13) 7  2 |  7  2  1  6 | |  \ /  |  \ /2 |6 \ /18|
! | 10 | 15  2  8 |( 7) 9  3 |  9  3  2  8 | |  17   |   5   |  15   |
! | 11 | 16  3 10 |( 9)11  4 | 11  4  3 10 | |20/ \ 8| 4/ \  |  / \  |
! | 12 | 17  4 12 |(11)13  1 | 13  1  4 12 | | /   \ | / 1 \ | /   \ |
! +----+----------+----------+-------------+ |/ 16  \|/     \|/ 10  \|
! | 13 | 14  7  2 |( 8) 1  6 |  1  6  7  2 |13-------1-------2-------8
! | 14 | 15  9  3 |(10) 2  8 |  2  8  9  3 |         |\  5  /|       
! | 15 | 16 11  4 |(12) 3 10 |  3 10 11  4 |         | \   / |      
! | 16 | 17 13  1 |( 6) 4 12 |  4 12 13  1 |         |9 \ /13|    
! +----+----------+----------+-------------+         |  14   |    
! | 17 | 14  6  7 |  -  2  1 |  2  1  6  7 |         |  / \  |   
! | 18 | 15  8  9 |  -  3  2 |  3  2  8  9 |         | /   \ |  
! | 19 | 16 10 11 |  -  4  3 |  4  3 10 11 |         |/ 17  \| 
! | 20 | 17 12 13 |  -  1  4 |  1  4 12 13 |         6-------7
! +----+----------+----------+-------------+
      DATA IC / 
     1    5, 1, 2 , 14, 3, 4 ,  3, 4, 1, 2, 
     2    5, 2, 3 , 15, 4, 1 ,  4, 1, 2, 3,
     3    5, 3, 4 , 16, 1, 2 ,  1, 2, 3, 4,
     4    5, 4, 1 , 17, 2, 3 ,  2, 3, 4, 1,
     5   14, 2, 1 ,  5, 6, 7 ,  6, 7, 2, 1,
     6   15, 3, 2 ,  5, 8, 9 ,  8, 9, 3, 2,
     7   16, 4, 3 ,  5,10,11 , 10,11, 4, 3,
     8   17, 1, 4 ,  5,12,13 , 12,13, 1, 4,
     9   14, 1, 6 ,  0, 7, 2 ,  7, 2, 1, 6,
     .   15, 2, 8 ,  0, 9, 3 ,  9, 3, 2, 8,
     1   16, 3,10 ,  0,11, 4 , 11, 4, 3,10,
     2   17, 4,12 ,  0,13, 1 , 13, 1, 4,12,
     3   14, 7, 2 ,  0, 1, 6 ,  1, 6, 7, 2,
     4   15, 9, 3 ,  0, 2, 8 ,  2, 8, 9, 3,
     5   16,11, 4 ,  0, 3,10 ,  3,10,11, 4,
     6   17,13, 1 ,  0, 4,12 ,  4,12,13, 1,
     7   14, 6, 7 ,  0, 2, 1 ,  2, 1, 6, 7,
     8   15, 8, 9 ,  0, 3, 2 ,  3, 2, 8, 9,
     9   16,10,11 ,  0, 4, 3 ,  4, 3,10,11,
     .   17,12,13 ,  0, 1, 4 ,  1, 4,12,13/

! +------+-------------------------------------------+
! |      |       sous-triangle voisins               |
! |sous  +----------+----------+---------------------+
! |trian |       n3/=n4        |             n3=n4   |
! |      +----------+----------+----------+----------+
! |      |   IT0    | 2,4,6,8=0|          | 2,4,6,8=0|
! +------+----------+----------+----------+----------+
! |   1  |  5  2  4 |  5  2  4 |  5  6  8 |  5  6  8 |
! |   2  |  6  3  1 |  6  3  1 |  -  -  - |  -  -  - |
! |   3  |  7  4  2 |  7  4  2 |  -  -  - |  -  -  - |
! |   4  |  8  1  3 |  8  1  3 |  -  -  - |  -  -  - |
! +------+----------+----------+----------+----------+
! |   5  |  1  9 13 |  1 -1 -1 |  1  9 13 |  1 -1 -1 |
! |   6  |  2 10 14 |  2 -1 -1 |  1 10 14 |  1 -1 -1 |
! |   7  |  3 11 15 |  3 -1 -1 |  -  -  - |  -  -  - |
! |   8  |  4 12 16 |  4 -1 -1 |  1 12 16 |  1 -1 -1 |
! +------+----------+----------+----------+----------+
! |   9  | -1 17  5 |  -  -  - | -1 17  5 |  -  -  - |
! |  10  | -1 18  6 |  -  -  - | -1 18  6 |  -  -  - |
! |  11  | -1 19  7 |  -  -  - |  -  -  - |  -  -  - |
! |  12  | -1 20  8 |  -  -  - | -1 20  8 |  -  -  - |
! +------+----------+----------+----------+----------+
! |  13  | -1  5 17 |  -  -  - | -1  5 17 |  -  -  - |
! |  14  | -1  6 18 |  -  -  - | -1  6 18 |  -  -  - |
! |  15  | -1  7 19 |  -  -  - |  -  -  - |  -  -  - |
! |  16  | -1  8 20 |  -  -  - | -1  8 20 |  -  -  - |
! +------+----------+----------+----------+----------+
! |  17  | -1  9 13 |  -  -  - | -1  9 13 |  -  -  - |
! |  18  | -1 10 14 |  -  -  - | -1 10 14 |  -  -  - |
! |  19  | -1 11 15 |  -  -  - |  -  -  - |  -  -  - |
! |  20  | -1 12 16 |  -  -  - | -1 12 16 |  -  -  - |
! +------+----------+----------+----------+----------+
      DATA IT0 / 
     1   5, 2, 4,
     2   6, 3, 1,
     3   7, 4, 2,
     4   8, 1, 3,
     5   1, 9,13,
     6   2,10,14,
     7   3,11,15,
     8   4,12,16,
     9  -1,17, 5,
     .  -1,18, 6,
     1  -1,19, 7,
     2  -1,20, 8,
     3  -1, 5,17,
     4  -1, 6,18,
     5  -1, 7,19,
     6  -1, 8,20,
     7  -1, 9,13,
     8  -1,10,14,
     9  -1,11,15,
     .  -1,12,16/

      UNP   = ONE + EM4
      ZEROM = ZERO - EM4
      EPS = EPS0

      ITQ = SUBTRIA
      K = IC(1,ITQ)
      XA = XXL(K)   
      YA = YYL(K)    
      ZA = ZZL(K) 

      K = IC(2,ITQ)
      XB = XXL(K)   
      YB = YYL(K)    
      ZB = ZZL(K)   

      K = IC(3,ITQ)
      XC = XXL(K)   
      YC = YYL(K)    
      ZC = ZZL(K)   

      K = IC(4,ITQ)
      XD = XXL(K)   
      YD = YYL(K)    
      ZD = ZZL(K)   

      IF(ITQ ==1)THEN
       N1 = SX125
       N2 = SY125
       N3 = SZ125
      ELSEIF(ITQ ==2)THEN
       N1 = SX235
       N2 = SY235
       N3 = SZ235
      ELSEIF(ITQ ==3)THEN
       N1 = SX345
       N2 = SY345
       N3 = SZ345
      ELSEIF(ITQ ==4)THEN
       N1 = SX415
       N2 = SY415
       N3 = SZ415
      ENDIF
      
      S2 = ONE/MAX(EM30,SQRT(N1*N1 + N2*N2 + N3*N3))
      N1 = N1*S2
      N2 = N2*S2
      N3 = N3*S2

      BBB = (XI-XA)*N1+(YI-YA)*N2+(ZI-ZA)*N3
      PENE = -MIN(ZERO,BBB)

      XAB = XB-XA
      YAB = YB-YA
      ZAB = ZB-ZA
      XBC = XC-XB
      YBC = YC-YB
      ZBC = ZC-ZB
      XCA = XA-XC
      YCA = YA-YC
      ZCA = ZA-ZC

      XBD = XD-XB
      YBD = YD-YB
      ZBD = ZD-ZB

      XIA = XA-XI
      YIA = YA-YI
      ZIA = ZA-ZI
      XIB = XB-XI
      YIB = YB-YI
      ZIB = ZB-ZI
      XIC = XC-XI
      YIC = YC-YI
      ZIC = ZC-ZI
      SX = - YAB*ZCA + ZAB*YCA
      SY = - ZAB*XCA + XAB*ZCA
      SZ = - XAB*YCA + YAB*XCA
      S2 = SX*SX+SY*SY+SZ*SZ
      SAX = YIB*ZIC - ZIB*YIC
      SAY = ZIB*XIC - XIB*ZIC
      SAZ = XIB*YIC - YIB*XIC
      LA = (SX*SAX+SY*SAY+SZ*SAZ)/S2
      SBX = YIC*ZIA - ZIC*YIA
      SBY = ZIC*XIA - XIC*ZIA
      SBZ = XIC*YIA - YIC*XIA
      LB = (SX*SBX+SY*SBY+SZ*SBZ)/S2
      LC = ONE - LA - LB

c------------------------------------------------------
c         quadrangles
c       check if outside side CA
      AAA = ZEROM
      IF(LB<AAA)THEN
          ISTEP=3
          SUBTRIA_N=IT0(2,ITQ)
          SUBTRIA  =IT0(2,ITQ)
          IZLIM=-1
          CALL I24NEXTTRIA2(
     1      IZLIM,ISTEP,SUBTRIA_N,SUBTRIA,
     2      LARGEP,PENE ,XXL  ,YYL  ,ZZL  ,
     3      SX125,SY125,SZ125,SX235,SY235,
     4      SZ235,SX345,SY345,SZ345,SX415,
     5      SY415,SZ415,XI   ,YI   ,ZI   ,
     6      N1   ,N2   ,N3   ,LA   ,LB   ,
     7      LC   ,GAP2 ,EPS  )
c       check if outside side AB
      ELSEIF(LC<AAA)THEN
          ISTEP=3
          SUBTRIA_N=IT0(3,ITQ)
          SUBTRIA  =IT0(3,ITQ)
          IZLIM=-1
          CALL I24NEXTTRIA2(
     1      IZLIM,ISTEP,SUBTRIA_N,SUBTRIA,
     2      LARGEP,PENE ,XXL  ,YYL  ,ZZL  ,
     3      SX125,SY125,SZ125,SX235,SY235,
     4      SZ235,SX345,SY345,SZ345,SX415,
     5      SY415,SZ415,XI   ,YI   ,ZI   ,
     6      N1   ,N2   ,N3   ,LA   ,LB   ,
     7      LC   ,GAP2 ,EPS  )
c         check if outside side BC
      ELSEIF(LA<-EPS)THEN
c hors zone d'extension de surface
            ISTEP=3
            SUBTRIA_N=IT0(1,ITQ)
            IZLIM=-1
      ELSEIF(LA<EPS)THEN
c zone limite interpolation des normales si convex
        VOL = N1*XBD + N2*YBD + N3*ZBD
        IF (GAP2<EPS.AND.(LB<EPS.OR.LC<EPS)) VOL=-ABS(VOL)
        IF(LARGEP == 1)THEN
c         large penetration inside +- extension zone
          ISTEP=3
          SUBTRIA_N=IT0(1,ITQ)
          IZLIM = -1
        ELSEIF(VOL < ZERO)THEN
c         convex
          IZLIM=1
        ELSEIF(LA<ZEROM)THEN
          ISTEP=3
          SUBTRIA_N=IT0(1,ITQ)
          IZLIM=-1
        ENDIF
      ENDIF

      RETURN
      END
Chd|====================================================================
Chd|  I24NEXTTRIA2                  source/interfaces/int24/i24dst3.F
Chd|-- called by -----------
Chd|        I24NEXTTRIA                   source/interfaces/int24/i24dst3.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I24NEXTTRIA2(
     1          IZLIM,ISTEP,SUBTRIA_N,SUBTRIA,
     2          LARGEP,PENE ,XXL  ,YYL  ,ZZL  ,
     3          SX125,SY125,SZ125,SX235,SY235,
     4          SZ235,SX345,SY345,SZ345,SX415,
     5          SY415,SZ415,XI   ,YI   ,ZI   ,
     6          N1   ,N2   ,N3   ,LA   ,LB   ,
     7          LC   ,GAP2 ,EPS  )
C-----------------------------------------------
C*******************************************
C
C    THIS SUBROUTINE IS EXACTLY THE SAME
C    THAN I24NEXTTRIA2 EXCEPT THE CALL TO I24NEXTTRIA2
C
C*******************************************
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISTEP,SUBTRIA_N,SUBTRIA,IZLIM,LARGEP
      my_real
     .     PENE,XXL(17),YYL(17),ZZL(17),XI   ,YI   ,ZI,
     .     SX125,SY125,SZ125,SX235,SY235,
     .     SZ235,SX345,SY345,SZ345,SX415,
     .     SY415,SZ415,N1,N2,N3,LA,LB,LC,GAP2,EPS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K,ITQ
      my_real
     .     AAA,BBB,VOL,XA,YA,ZA,XB,YB,ZB,XC,YC,ZC,XD,YD,ZD,
     .     XAB,YAB,ZAB,XBC,YBC,ZBC,XCA,YCA,ZCA,XBD,YBD,ZBD,
     .     XIA,YIA,ZIA,XIB,YIB,ZIB,XIC,YIC,ZIC,SX ,SY, SZ, 
     .     S2 ,SAX,SAY,SAZ,SBX,SBY,SBZ,UNP,ZEROM
      INTEGER IT0(3,20),IC(10,20)
! +----+----------+----------+-------------+
! | IC                                     |
! +----+----------+----------+-------------+
! | ST | noeuds T | noeuds TV| noeuds Quad |
! +----+----------+----------+-------------+        11-------10       
! |  1 |  5  1  2 | 14  3  4 |  3  4  1  2 |         |\ 19  /|        
! |  2 |  5  2  3 | 15  4  1 |  4  1  2  3 |         | \   / |                                    
! |  3 |  5  3  4 | 16  1  2 |  1  2  3  4 |         |  \ /  |           
! |  4 |  5  4  1 | 17  2  3 |  2  3  4  1 |         |  16   |           
! +----+----------+----------+-------------+         |15/ \11|        
! |  5 | 14  2  1 |  5  6  7 |  6  7  2  1 |         | /   \ |      
! |  6 | 15  3  2 |  5  8  9 |  8  9  3  2 |         |/  7  \|       
! |  7 | 16  4  3 |  5 10 11 | 10 11  4  3 |12-------4-------3-------9
! |  8 | 17  1  4 |  5 12 13 | 12 13  1  4 | |\ 12  /|\     /|\ 14  /|
! +----+----------+----------+-------------+ | \   / | \ 3 / | \   / |
! |  9 | 14  1  6 |(13) 7  2 |  7  2  1  6 | |  \ /  |  \ /2 |6 \ /18|
! | 10 | 15  2  8 |( 7) 9  3 |  9  3  2  8 | |  17   |   5   |  15   |
! | 11 | 16  3 10 |( 9)11  4 | 11  4  3 10 | |20/ \ 8| 4/ \  |  / \  |
! | 12 | 17  4 12 |(11)13  1 | 13  1  4 12 | | /   \ | / 1 \ | /   \ |
! +----+----------+----------+-------------+ |/ 16  \|/     \|/ 10  \|
! | 13 | 14  7  2 |( 8) 1  6 |  1  6  7  2 |13-------1-------2-------8
! | 14 | 15  9  3 |(10) 2  8 |  2  8  9  3 |         |\  5  /|       
! | 15 | 16 11  4 |(12) 3 10 |  3 10 11  4 |         | \   / |      
! | 16 | 17 13  1 |( 6) 4 12 |  4 12 13  1 |         |9 \ /13|    
! +----+----------+----------+-------------+         |  14   |    
! | 17 | 14  6  7 |  -  2  1 |  2  1  6  7 |         |  / \  |   
! | 18 | 15  8  9 |  -  3  2 |  3  2  8  9 |         | /   \ |  
! | 19 | 16 10 11 |  -  4  3 |  4  3 10 11 |         |/ 17  \| 
! | 20 | 17 12 13 |  -  1  4 |  1  4 12 13 |         6-------7
! +----+----------+----------+-------------+
      DATA IC / 
     1    5, 1, 2 , 14, 3, 4 ,  3, 4, 1, 2, 
     2    5, 2, 3 , 15, 4, 1 ,  4, 1, 2, 3,
     3    5, 3, 4 , 16, 1, 2 ,  1, 2, 3, 4,
     4    5, 4, 1 , 17, 2, 3 ,  2, 3, 4, 1,
     5   14, 2, 1 ,  5, 6, 7 ,  6, 7, 2, 1,
     6   15, 3, 2 ,  5, 8, 9 ,  8, 9, 3, 2,
     7   16, 4, 3 ,  5,10,11 , 10,11, 4, 3,
     8   17, 1, 4 ,  5,12,13 , 12,13, 1, 4,
     9   14, 1, 6 ,  0, 7, 2 ,  7, 2, 1, 6,
     .   15, 2, 8 ,  0, 9, 3 ,  9, 3, 2, 8,
     1   16, 3,10 ,  0,11, 4 , 11, 4, 3,10,
     2   17, 4,12 ,  0,13, 1 , 13, 1, 4,12,
     3   14, 7, 2 ,  0, 1, 6 ,  1, 6, 7, 2,
     4   15, 9, 3 ,  0, 2, 8 ,  2, 8, 9, 3,
     5   16,11, 4 ,  0, 3,10 ,  3,10,11, 4,
     6   17,13, 1 ,  0, 4,12 ,  4,12,13, 1,
     7   14, 6, 7 ,  0, 2, 1 ,  2, 1, 6, 7,
     8   15, 8, 9 ,  0, 3, 2 ,  3, 2, 8, 9,
     9   16,10,11 ,  0, 4, 3 ,  4, 3,10,11,
     .   17,12,13 ,  0, 1, 4 ,  1, 4,12,13/

! +------+-------------------------------------------+
! |      |       sous-triangle voisins               |
! |sous  +----------+----------+---------------------+
! |trian |       n3/=n4        |             n3=n4   |
! |      +----------+----------+----------+----------+
! |      |   IT0    | 2,4,6,8=0|          | 2,4,6,8=0|
! +------+----------+----------+----------+----------+
! |   1  |  5  2  4 |  5  2  4 |  5  6  8 |  5  6  8 |
! |   2  |  6  3  1 |  6  3  1 |  -  -  - |  -  -  - |
! |   3  |  7  4  2 |  7  4  2 |  -  -  - |  -  -  - |
! |   4  |  8  1  3 |  8  1  3 |  -  -  - |  -  -  - |
! +------+----------+----------+----------+----------+
! |   5  |  1  9 13 |  1 -1 -1 |  1  9 13 |  1 -1 -1 |
! |   6  |  2 10 14 |  2 -1 -1 |  1 10 14 |  1 -1 -1 |
! |   7  |  3 11 15 |  3 -1 -1 |  -  -  - |  -  -  - |
! |   8  |  4 12 16 |  4 -1 -1 |  1 12 16 |  1 -1 -1 |
! +------+----------+----------+----------+----------+
! |   9  | -1 17  5 |  -  -  - | -1 17  5 |  -  -  - |
! |  10  | -1 18  6 |  -  -  - | -1 18  6 |  -  -  - |
! |  11  | -1 19  7 |  -  -  - |  -  -  - |  -  -  - |
! |  12  | -1 20  8 |  -  -  - | -1 20  8 |  -  -  - |
! +------+----------+----------+----------+----------+
! |  13  | -1  5 17 |  -  -  - | -1  5 17 |  -  -  - |
! |  14  | -1  6 18 |  -  -  - | -1  6 18 |  -  -  - |
! |  15  | -1  7 19 |  -  -  - |  -  -  - |  -  -  - |
! |  16  | -1  8 20 |  -  -  - | -1  8 20 |  -  -  - |
! +------+----------+----------+----------+----------+
! |  17  | -1  9 13 |  -  -  - | -1  9 13 |  -  -  - |
! |  18  | -1 10 14 |  -  -  - | -1 10 14 |  -  -  - |
! |  19  | -1 11 15 |  -  -  - |  -  -  - |  -  -  - |
! |  20  | -1 12 16 |  -  -  - | -1 12 16 |  -  -  - |
! +------+----------+----------+----------+----------+
      DATA IT0 / 
     1   5, 2, 4,
     2   6, 3, 1,
     3   7, 4, 2,
     4   8, 1, 3,
     5   1, 9,13,
     6   2,10,14,
     7   3,11,15,
     8   4,12,16,
     9  -1,17, 5,
     .  -1,18, 6,
     1  -1,19, 7,
     2  -1,20, 8,
     3  -1, 5,17,
     4  -1, 6,18,
     5  -1, 7,19,
     6  -1, 8,20,
     7  -1, 9,13,
     8  -1,10,14,
     9  -1,11,15,
     .  -1,12,16/

      UNP   = ONE + EM4
      ZEROM = ZERO - EM4
c      EPS = (TWO+HALF)/HUNDRED

      ITQ = SUBTRIA
      K = IC(1,ITQ)
      XA = XXL(K)   
      YA = YYL(K)    
      ZA = ZZL(K) 

      K = IC(2,ITQ)
      XB = XXL(K)   
      YB = YYL(K)    
      ZB = ZZL(K)   

      K = IC(3,ITQ)
      XC = XXL(K)   
      YC = YYL(K)    
      ZC = ZZL(K)   

      K = IC(4,ITQ)
      XD = XXL(K)   
      YD = YYL(K)    
      ZD = ZZL(K)   

      IF(ITQ ==1)THEN
       N1 = SX125
       N2 = SY125
       N3 = SZ125
      ELSEIF(ITQ ==2)THEN
       N1 = SX235
       N2 = SY235
       N3 = SZ235
      ELSEIF(ITQ ==3)THEN
       N1 = SX345
       N2 = SY345
       N3 = SZ345
      ELSEIF(ITQ ==4)THEN
       N1 = SX415
       N2 = SY415
       N3 = SZ415
      ENDIF
      
      S2 = ONE/MAX(EM30,SQRT(N1*N1 + N2*N2 + N3*N3))
      N1 = N1*S2
      N2 = N2*S2
      N3 = N3*S2

      BBB = (XI-XA)*N1+(YI-YA)*N2+(ZI-ZA)*N3
      PENE = -MIN(ZERO,BBB)

      XAB = XB-XA
      YAB = YB-YA
      ZAB = ZB-ZA
      XBC = XC-XB
      YBC = YC-YB
      ZBC = ZC-ZB
      XCA = XA-XC
      YCA = YA-YC
      ZCA = ZA-ZC

      XBD = XD-XB
      YBD = YD-YB
      ZBD = ZD-ZB

      XIA = XA-XI
      YIA = YA-YI
      ZIA = ZA-ZI
      XIB = XB-XI
      YIB = YB-YI
      ZIB = ZB-ZI
      XIC = XC-XI
      YIC = YC-YI
      ZIC = ZC-ZI
      SX = - YAB*ZCA + ZAB*YCA
      SY = - ZAB*XCA + XAB*ZCA
      SZ = - XAB*YCA + YAB*XCA
      S2 = SX*SX+SY*SY+SZ*SZ
      SAX = YIB*ZIC - ZIB*YIC
      SAY = ZIB*XIC - XIB*ZIC
      SAZ = XIB*YIC - YIB*XIC
      LA = (SX*SAX+SY*SAY+SZ*SAZ)/S2
      SBX = YIC*ZIA - ZIC*YIA
      SBY = ZIC*XIA - XIC*ZIA
      SBZ = XIC*YIA - YIC*XIA
      LB = (SX*SBX+SY*SBY+SZ*SBZ)/S2
      LC = ONE - LA - LB

c------------------------------------------------------
c         quadrangles
      AAA = ZEROM
      IF(LB<AAA)THEN
c         outside side CA
          ISTEP=3
          SUBTRIA_N=IT0(2,ITQ)
          SUBTRIA  =IT0(2,ITQ)
          IZLIM=-1
c          CALL I24NEXTTRIA2(
c     1      IZLIM,ISTEP,SUBTRIA_N,SUBTRIA,
c     2      PENE ,XXL  ,YYL  ,ZZL  ,
c     3      SX125,SY125,SZ125,SX235,SY235,
c     4      SZ235,SX345,SY345,SZ345,SX415,
c     5      SY415,SZ415,XI   ,YI   ,ZI   ,
c     6      N1   ,N2   ,N3   ,LA   ,LB   ,
c     7      LC   )
      ELSEIF(LC<AAA)THEN
c         outside side AB
          ISTEP=3
          SUBTRIA_N=IT0(3,ITQ)
          SUBTRIA  =IT0(3,ITQ)
          IZLIM=-1
c          CALL I24NEXTTRIA2(
c     1      IZLIM,ISTEP,SUBTRIA_N,SUBTRIA,
c     2      PENE ,XXL  ,YYL  ,ZZL  ,
c     3      SX125,SY125,SZ125,SX235,SY235,
c     4      SZ235,SX345,SY345,SZ345,SX415,
c     5      SY415,SZ415,XI   ,YI   ,ZI   ,
c     6      N1   ,N2   ,N3   ,LA   ,LB   ,
c     7      LC   )
c         check if outside side BC
      ELSEIF(LA<-EPS)THEN
c hors zone d'extension de surface
            ISTEP=3
            SUBTRIA_N=IT0(1,ITQ)
            IZLIM=-1
      ELSEIF(LA<EPS)THEN
c zone limite interpolation des normales si convex
        VOL = N1*XBD + N2*YBD + N3*ZBD
        IF (GAP2<EPS.AND.(LB<EPS.OR.LC<EPS)) VOL=-ABS(VOL)
        IF(LARGEP == 1)THEN
c         large penetration inside +- extension zone
          ISTEP=3
          SUBTRIA_N=IT0(1,ITQ)
          IZLIM = -1
        ELSEIF(VOL < ZERO)THEN
c         convex
          IZLIM=1
        ELSEIF(LA<ZEROM)THEN
          ISTEP=3
          SUBTRIA_N=IT0(1,ITQ)
          IZLIM=-1
        ENDIF
      ENDIF

      RETURN
      END
Chd|====================================================================
Chd|  S_IN_M4                       source/interfaces/int24/i24dst3.F
Chd|-- called by -----------
Chd|        I24DST3                       source/interfaces/int24/i24dst3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S_IN_M4(X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,
     1                   XI,YI,ZI,IER)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IER
C     REAL
      my_real
     .   X1,X2,X3,X4,
     .   Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,XI,YI,ZI
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   X0, Y0, Z0, XL1, XL2, XL3, XL4, YY1, YY2, YY3, YY4,
     .   ZZ1, ZZ2, ZZ3, ZZ4, XI1, XI2, XI3, XI4, YI1, YI2, YI3, YI4,
     .   ZI1, ZI2, ZI3, ZI4, XN1, YN1, ZN1, XN2, YN2, ZN2, XN3, YN3,
     .   ZN3, XN4, YN4, ZN4, AN, AREA, A12, A23, A34, A41, B12, B23,
     .   B34, B41, AB1, AB2, TP, TM, SP, SM, N1,N2,N3,ALP,
     .   NN(3), SSC, TTC
C------------------------------------------------
      IER = 0
      ALP=EM8
      X0 = FOURTH*(X1+X2+X3+X4)
      Y0 = FOURTH*(Y1+Y2+Y3+Y4)
      Z0 = FOURTH*(Z1+Z2+Z3+Z4)
C
      XL1 = X1-X0
      XL2 = X2-X0
      XL3 = X3-X0
      XL4 = X4-X0
      YY1 = Y1-Y0
      YY2 = Y2-Y0
      YY3 = Y3-Y0
      YY4 = Y4-Y0
      ZZ1 = Z1-Z0
      ZZ2 = Z2-Z0
      ZZ3 = Z3-Z0
      ZZ4 = Z4-Z0
C
      XI1 = X1-XI
      XI2 = X2-XI
      XI3 = X3-XI
      XI4 = X4-XI
      YI1 = Y1-YI
      YI2 = Y2-YI
      YI3 = Y3-YI
      YI4 = Y4-YI
      ZI1 = Z1-ZI
      ZI2 = Z2-ZI
      ZI3 = Z3-ZI
      ZI4 = Z4-ZI
C
      XN1 = YY1*ZZ2 - YY2*ZZ1
      YN1 = ZZ1*XL2 - ZZ2*XL1
      ZN1 = XL1*YY2 - XL2*YY1
      N1=XN1
      N2=YN1
      N3=ZN1
C
      XN2 = YY2*ZZ3 - YY3*ZZ2
      YN2 = ZZ2*XL3 - ZZ3*XL2
      ZN2 = XL2*YY3 - XL3*YY2
      N1=N1+XN2
      N2=N2+YN2
      N3=N3+ZN2
C
      XN3 = YY3*ZZ4 - YY4*ZZ3
      YN3 = ZZ3*XL4 - ZZ4*XL3
      ZN3 = XL3*YY4 - XL4*YY3
      N1=N1+XN3
      N2=N2+YN3
      N3=N3+ZN3
C
      XN4 = YY4*ZZ1 - YY1*ZZ4
      YN4 = ZZ4*XL1 - ZZ1*XL4
      ZN4 = XL4*YY1 - XL1*YY4
      N1=N1+XN4
      N2=N2+YN4
      N3=N3+ZN4
C
      AN= MAX(EM20,SQRT(N1*N1+N2*N2+N3*N3))
      N1=N1/AN
      N2=N2/AN
      N3=N3/AN
      NN(1)=N1
      NN(2)=N2
      NN(3)=N3
      IF(AN<=EM19) THEN
        IER=-1
        RETURN
      ENDIF
      AREA=HALF*AN
C
      A12=(N1*XN1+N2*YN1+N3*ZN1)
      A23=(N1*XN2+N2*YN2+N3*ZN2)
      A34=(N1*XN3+N2*YN3+N3*ZN3)
      A41=(N1*XN4+N2*YN4+N3*ZN4)
C
      XN1 = YI1*ZI2 - YI2*ZI1
      YN1 = ZI1*XI2 - ZI2*XI1
      ZN1 = XI1*YI2 - XI2*YI1
      B12=(N1*XN1+N2*YN1+N3*ZN1)
C
      XN2 = YI2*ZI3 - YI3*ZI2
      YN2 = ZI2*XI3 - ZI3*XI2
      ZN2 = XI2*YI3 - XI3*YI2
      B23=(N1*XN2+N2*YN2+N3*ZN2)
C
      XN3 = YI3*ZI4 - YI4*ZI3
      YN3 = ZI3*XI4 - ZI4*XI3
      ZN3 = XI3*YI4 - XI4*YI3
      B34=(N1*XN3+N2*YN3+N3*ZN3)
C
      XN4 = YI4*ZI1 - YI1*ZI4
      YN4 = ZI4*XI1 - ZI1*XI4
      ZN4 = XI4*YI1 - XI1*YI4
      B41=(N1*XN4+N2*YN4+N3*ZN4)
C
      AB1=A23*B41
      AB2=B23*A41
C
      IF(ABS(AB1+AB2)/AREA>EM10)THEN
       SSC=(AB1-AB2)/(AB1+AB2)
      ELSE
       SSC=ZERO
      ENDIF
      IF(ABS(A34/AREA)>EM10)THEN
       AB1=B12*A34
       AB2=B34*A12
       TTC=(AB1-AB2)/(AB1+AB2)
      ELSE
       TTC=B12/A12-ONE
       IF(B23<=ZERO.AND.B41<=ZERO)THEN
         IF(-B23/A12<=ALP.AND.-B41/A12<=ALP)SSC=ZERO
       ELSEIF(B23<=ZERO)THEN
         IF(-B23/A12<=ALP) THEN
          SSC=ONE
         ELSE
          SSC=TWO
         END IF
       ELSEIF(B41<=ZERO)THEN
         IF(-B41/A12<=ALP) THEN
          SSC=-ONE
         ELSE
          SSC=-TWO
         END IF
       ENDIF
      ENDIF
C
      IF(ABS(SSC)>ONE+ALP.OR.ABS(TTC)>ONE+ALP) THEN
        IER=1
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  INTERSECV                     source/interfaces/int24/i24dst3.F
Chd|-- called by -----------
Chd|        I24DST3                       source/interfaces/int24/i24dst3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTERSECV(
     1                    ISTEP    ,SUBTRIA,IXX3  ,IXX4  ,
     1                    INTERSECT,V12   ,V23    ,V34   ,V41    ,
     2                    VO12     ,VO23  ,VO34   ,VO41  ,XX1    ,
     3                    YY1      ,ZZ1   ,XX2    ,YY2   ,ZZ2    ,
     4                    XX3      ,YY3   ,ZZ3    ,XX4   ,YY4    ,
     5                    ZZ4      ,XX5   ,YY5    ,ZZ5   ,ADT0   ,
     6                    VXI     ,VYI    ,VZI    ,XO1   ,XO2    ,
     7                    XO3     ,XO4    ,XO5    ,YO1   ,YO2    ,
     8                    YO3     ,YO4    ,YO5    ,ZO1   ,ZO2    ,
     9                    ZO3     ,ZO4    ,ZO5    ,NX    ,NY     ,
     A                    NZ      ,SX125  ,SX235  ,SX345 ,SX415  ,
     B                    SY125   ,SY235  ,SY345  ,SY415 ,SZ125  ,
     C                    SZ235  ,SZ345   ,SZ415  ,XI    ,YI     ,
     D                    ZI     ,ZEROM   ,UNP    ,ZEROMT,UNPT   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISTEP    ,SUBTRIA,IXX3  ,IXX4  ,INTERSECT
C     REAL
      my_real
     1     V12   ,V23    ,V34   ,V41    ,
     2     VO12     ,VO23  ,VO34   ,VO41  ,XX1    ,
     3     YY1      ,ZZ1   ,XX2    ,YY2   ,ZZ2    ,
     4     XX3      ,YY3   ,ZZ3    ,XX4   ,YY4    ,
     5     ZZ4      ,XX5   ,YY5    ,ZZ5   ,ADT0   ,
     6     VXI     ,VYI    ,VZI    ,XO1   ,XO2    ,
     7     XO3     ,XO4    ,XO5    ,YO1   ,YO2    ,
     8     YO3     ,YO4    ,YO5    ,ZO1   ,ZO2    ,
     9     ZO3     ,ZO4    ,ZO5    ,NX    ,NY     ,
     A     NZ      ,SX125  ,SX235  ,SX345 ,SX415  ,
     B     SY125   ,SY235  ,SY345  ,SY415 ,SZ125  ,
     C     SZ235  ,SZ345   ,SZ415  ,ZEROM ,UNP    ,
     D     ZEROMT,UNPT   ,XI,YI,ZI
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .     X51,  X52,  X53, X54,
     .     Y51,  Y52,  Y53, Y54,
     .     Z51,  Z52,  Z53, Z54,
     .     XI1,  XI2,  XI3,  XI4,  XI5,
     .     YI1,  YI2,  YI3,  YI4,  YI5,
     .     ZI1,  ZI2,  ZI3,  ZI4,  ZI5,
     .     XIA,  XIB,  XIC,
     .     YIA,  YIB,  YIC,
     .     ZIA,  ZIB,  ZIC,
     .     XOI,YOI,ZOI,XS,YS,ZS,
     .     XM1, XM2, XM3, XM4, XM5, 
     .     YM1, YM2, YM3, YM4, YM5, 
     .     ZM1, ZM2, ZM3, ZM4, ZM5,LA0,LB0,LC0
      my_real
     .     SX,SX1,SX2,SX3,SX4,SX5,SX0,
     .     SY,SY1,SY2,SY3,SY4,SY5,SY0,
     .     SZ,SZ1,SZ2,SZ3,SZ4,SZ5,SZ0,AAA,ADT,S2
C------------------------------------------------
         IF(VO12 <= ZERO .and. V12 >= ZERO )THEN
          IF ((ABS(SX125)+ABS(SY125)+ABS(SZ125))>EM20) THEN
           IF(ABS(VO12) < EM20)THEN
             AAA = ONE
           ELSEIF(ABS(V12) < EM20)THEN
             AAA = ZERO
           ELSE
             AAA = V12 / (V12-VO12)
           ENDIF

           ADT = AAA*DT1

           XM1 = XX1 + AAA*(XO1-XX1)
           YM1 = YY1 + AAA*(YO1-YY1)
           ZM1 = ZZ1 + AAA*(ZO1-ZZ1) 
         
           XM2 = XX2 + AAA*(XO2-XX2)
           YM2 = YY2 + AAA*(YO2-YY2)
           ZM2 = ZZ2 + AAA*(ZO2-ZZ2) 
         
           XOI = XI - ADT*VXI
           YOI = YI - ADT*VYI
           ZOI = ZI - ADT*VZI

           XM5 = XX5 + AAA*(XO5-XX5)
           YM5 = YY5 + AAA*(YO5-YY5)
           ZM5 = ZZ5 + AAA*(ZO5-ZZ5)
         
           X51 = XM1 - XM5
           Y51 = YM1 - YM5
           Z51 = ZM1 - ZM5
 
           X52 = XM2 - XM5
           Y52 = YM2 - YM5
           Z52 = ZM2 - ZM5
 
           XI1 = XM1 - XOI
           YI1 = YM1 - YOI
           ZI1 = ZM1 - ZOI
 
           XI2 = XM2 - XOI
           YI2 = YM2 - YOI
           ZI2 = ZM2 - ZOI
         
           XI5 = XM5 - XOI
           YI5 = YM5 - YOI
           ZI5 = ZM5 - ZOI

           SX0 = Y51*Z52 - Z51*Y52
           SY0 = Z51*X52 - X51*Z52
           SZ0 = X51*Y52 - Y51*X52

           SX1 = YI5*ZI1 - ZI5*YI1
           SY1 = ZI5*XI1 - XI5*ZI1
           SZ1 = XI5*YI1 - YI5*XI1

           SX5 = YI1*ZI2 - ZI1*YI2
           SY5 = ZI1*XI2 - XI1*ZI2
           SZ5 = XI1*YI2 - YI1*XI2

           SX2 = YI2*ZI5 - ZI2*YI5
           SY2 = ZI2*XI5 - XI2*ZI5
           SZ2 = XI2*YI5 - YI2*XI5
           
           S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
           LB0 =  (SX0*SX2 + SY0*SY2 + SZ0*SZ2) * S2
           LC0 =  (SX0*SX1 + SY0*SY1 + SZ0*SZ1) * S2
           LA0 =  ONE-LB0-LC0 

          IF(IXX3 /= IXX4)THEN
           IF(LB0 >= ZEROM .and. LB0 <= UNP .and.
     .        LC0 >= ZEROM .and. LC0 <= UNP .and.
     .        LA0 >= ZEROM .and. LA0 <= UNP)THEN
             INTERSECT = 1
             SUBTRIA= 1 ! subtriangle
             ISTEP = 5
             ADT0= ADT
             NX = SX125
             NY = SY125
             NZ = SZ125
           ENDIF
C----------loose tolerance for tria           
          ELSE
           IF(LB0 >= ZEROMT .AND. LB0 <= UNPT .AND.
     .        LC0 >= ZEROMT .AND. LC0 <= UNPT .AND.
     .        LA0 >= ZEROMT .AND. LA0 <= UNPT)THEN
             INTERSECT = 1
             SUBTRIA= 1 ! subtriangle
             ISTEP = 5
             ADT0= ADT
             NX = SX125
             NY = SY125
             NZ = SZ125
           ENDIF
          END IF! (IXX(I,3) /= IXX(I,4))THEN
          END IF! ((ABS(SX125)+ABS(SY125)+ABS(SZ125))>EM20) THEN
         ENDIF
         IF(IXX3 /= IXX4)THEN
           IF(VO23 <= ZERO .and. V23 >= ZERO )THEN
            IF ((ABS(SX235)+ABS(SY235)+ABS(SZ235))>EM20) THEN
             IF(ABS(VO23) < EM20)THEN
               AAA = ONE
             ELSEIF(ABS(V23) < EM20)THEN
               AAA = ZERO
             ELSE
               AAA = V23 / (V23-VO23)
             ENDIF

             ADT = AAA*DT1

             XM2 = XX2 + AAA*(XO2-XX2)
             YM2 = YY2 + AAA*(YO2-YY2)
             ZM2 = ZZ2 + AAA*(ZO2-ZZ2) 
         
             XM3 = XX3 + AAA*(XO3-XX3)
             YM3 = YY3 + AAA*(YO3-YY3)
             ZM3 = ZZ3 + AAA*(ZO3-ZZ3) 
         
             XOI = XI - ADT*VXI
             YOI = YI - ADT*VYI
             ZOI = ZI - ADT*VZI

             XM5 = XX5 + AAA*(XO5-XX5)
             YM5 = YY5 + AAA*(YO5-YY5)
             ZM5 = ZZ5 + AAA*(ZO5-ZZ5)
         
             X52 = XM2 - XM5
             Y52 = YM2 - YM5
             Z52 = ZM2 - ZM5
 
             X53 = XM3 - XM5
             Y53 = YM3 - YM5
             Z53 = ZM3 - ZM5
 
             XI2 = XM2 - XOI
             YI2 = YM2 - YOI
             ZI2 = ZM2 - ZOI
 
             XI3 = XM3 - XOI
             YI3 = YM3 - YOI
             ZI3 = ZM3 - ZOI
         
             XI5 = XM5 - XOI
             YI5 = YM5 - YOI
             ZI5 = ZM5 - ZOI

             SX0 = Y52*Z53 - Z52*Y53
             SY0 = Z52*X53 - X52*Z53
             SZ0 = X52*Y53 - Y52*X53

             SX2 = YI5*ZI2 - ZI5*YI2
             SY2 = ZI5*XI2 - XI5*ZI2
             SZ2 = XI5*YI2 - YI5*XI2

             SX5 = YI2*ZI3 - ZI2*YI3
             SY5 = ZI2*XI3 - XI2*ZI3
             SZ5 = XI2*YI3 - YI2*XI3

             SX3 = YI3*ZI5 - ZI3*YI5
             SY3 = ZI3*XI5 - XI3*ZI5
             SZ3 = XI3*YI5 - YI3*XI5
             
             S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
             LB0 =  (SX0*SX3 + SY0*SY3 + SZ0*SZ3) * S2
             LC0 =  (SX0*SX2 + SY0*SY2 + SZ0*SZ2) * S2
             LA0 =  ONE-LB0-LC0 

             IF(LB0 >= ZEROM .and. LB0 <= UNP .and.
     .          LC0 >= ZEROM .and. LC0 <= UNP .and.
     .          LA0 >= ZEROM .and. LA0 <= UNP)THEN
               INTERSECT = 1

               IF(ADT > ADT0)THEN
                 SUBTRIA= 2 ! subtriangle
                 ISTEP = 5
                 ADT0= ADT
                 NX = SX235
                 NY = SY235
                 NZ = SZ235
               ENDIF
             ENDIF
            END IF !((ABS(SX235)+ABS(SY235)+ABS(SZ235))>EM20) THEN
          ENDIF
          IF(VO34 <= ZERO .and. V34 >= ZERO )THEN
           IF ((ABS(SX345)+ABS(SY345)+ABS(SZ345))>EM20) THEN
            IF(ABS(VO34) < EM20)THEN
              AAA = ONE
            ELSEIF(ABS(V34) < EM20)THEN
              AAA = ZERO
            ELSE
              AAA = V34 / (V34-VO34)
            ENDIF

            ADT = AAA*DT1

            XM3 = XX3 + AAA*(XO3-XX3)
            YM3 = YY3 + AAA*(YO3-YY3)
            ZM3 = ZZ3 + AAA*(ZO3-ZZ3) 
         
            XM4 = XX4 + AAA*(XO4-XX4)
            YM4 = YY4 + AAA*(YO4-YY4)
            ZM4 = ZZ4 + AAA*(ZO4-ZZ4)
         
            XOI = XI - ADT*VXI
            YOI = YI - ADT*VYI
            ZOI = ZI - ADT*VZI

            XM5 = XX5 + AAA*(XO5-XX5)
            YM5 = YY5 + AAA*(YO5-YY5)
            ZM5 = ZZ5 + AAA*(ZO5-ZZ5)
         
            X53 = XM3 - XM5
            Y53 = YM3 - YM5
            Z53 = ZM3 - ZM5
 
            X54 = XM4 - XM5
            Y54 = YM4 - YM5
            Z54 = ZM4 - ZM5
 
            XI3 = XM3 - XOI
            YI3 = YM3 - YOI
            ZI3 = ZM3 - ZOI
 
            XI4 = XM4 - XOI
            YI4 = YM4 - YOI
            ZI4 = ZM4 - ZOI
         
            XI5 = XM5 - XOI
            YI5 = YM5 - YOI
            ZI5 = ZM5 - ZOI

            SX0 = Y53*Z54 - Z53*Y54
            SY0 = Z53*X54 - X53*Z54
            SZ0 = X53*Y54 - Y53*X54

            SX3 = YI5*ZI3 - ZI5*YI3
            SY3 = ZI5*XI3 - XI5*ZI3
            SZ3 = XI5*YI3 - YI5*XI3

            SX5 = YI3*ZI4 - ZI3*YI4
            SY5 = ZI3*XI4 - XI3*ZI4
            SZ5 = XI3*YI4 - YI3*XI4

            SX4 = YI4*ZI5 - ZI4*YI5
            SY4 = ZI4*XI5 - XI4*ZI5
            SZ4 = XI4*YI5 - YI4*XI5
            
            S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
            LB0 =  (SX0*SX4 + SY0*SY4 + SZ0*SZ4) * S2
            LC0 =  (SX0*SX3 + SY0*SY3 + SZ0*SZ3) * S2
            LA0 =  ONE-LB0-LC0 

           IF(LB0 >= ZEROM .and. LB0 <= UNP .and.
     .         LC0 >= ZEROM .and. LC0 <= UNP .and.
     .         LA0 >= ZEROM .and. LA0 <= UNP)THEN
               INTERSECT = 1
               IF(ADT > ADT0)THEN
                 SUBTRIA= 3 ! subtriangle
                 ISTEP = 5
                 ADT0= ADT
                 NX = SX345
                 NY = SY345
                 NZ = SZ345
               ENDIF
            ENDIF
           END IF !((ABS(SX345)+ABS(SY345)+ABS(SZ345))>EM20) THEN
          ENDIF

          IF(VO41 <= ZERO .and. V41 >= ZERO )THEN
           IF ((ABS(SX415)+ABS(SY415)+ABS(SZ415))>EM20) THEN
            IF(ABS(VO41) < EM20)THEN
              AAA = ONE
            ELSEIF(ABS(V41) < EM20)THEN
              AAA = ZERO
            ELSE
              AAA = V41 / (V41-VO41)
            ENDIF

            ADT = AAA*DT1

            XM4 = XX4 + AAA*(XO4-XX4)
            YM4 = YY4 + AAA*(YO4-YY4)
            ZM4 = ZZ4 + AAA*(ZO4-ZZ4) 
         
            XM1 = XX1 + AAA*(XO1-XX1)
            YM1 = YY1 + AAA*(YO1-YY1)
            ZM1 = ZZ1 + AAA*(ZO1-ZZ1) 
         
            XOI = XI - ADT*VXI
            YOI = YI - ADT*VYI
            ZOI = ZI - ADT*VZI

            XM5 = XX5 + AAA*(XO5-XX5)
            YM5 = YY5 + AAA*(YO5-YY5)
            ZM5 = ZZ5 + AAA*(ZO5-ZZ5)
         
            X54 = XM4 - XM5
            Y54 = YM4 - YM5
            Z54 = ZM4 - ZM5
 
            X51 = XM1 - XM5
            Y51 = YM1 - YM5
            Z51 = ZM1 - ZM5
 
            XI4 = XM4 - XOI
            YI4 = YM4 - YOI
            ZI4 = ZM4 - ZOI
 
            XI1 = XM1 - XOI
            YI1 = YM1 - YOI
            ZI1 = ZM1 - ZOI
         
            XI5 = XM5 - XOI
            YI5 = YM5 - YOI
            ZI5 = ZM5 - ZOI

            SX0 = Y54*Z51 - Z54*Y51
            SY0 = Z54*X51 - X54*Z51
            SZ0 = X54*Y51 - Y54*X51

            SX4 = YI5*ZI4 - ZI5*YI4
            SY4 = ZI5*XI4 - XI5*ZI4
            SZ4 = XI5*YI4 - YI5*XI4

            SX5 = YI4*ZI1 - ZI4*YI1
            SY5 = ZI4*XI1 - XI4*ZI1
            SZ5 = XI4*YI1 - YI4*XI1

            SX1 = YI1*ZI5 - ZI1*YI5
            SY1 = ZI1*XI5 - XI1*ZI5
            SZ1 = XI1*YI5 - YI1*XI5
            
            S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
            LB0 =  (SX0*SX1 + SY0*SY1 + SZ0*SZ1) * S2
            LC0 =  (SX0*SX4 + SY0*SY4 + SZ0*SZ4) * S2
            LA0 =  ONE-LB0-LC0 

            IF(LB0 >= ZEROM .and. LB0 <= UNP .and.
     .         LC0 >= ZEROM .and. LC0 <= UNP .and.
     .         LA0 >= ZEROM .and. LA0 <= UNP)THEN
               INTERSECT = 1
               IF(ADT > ADT0)THEN
                 SUBTRIA= 4 ! subtriangle
                 ISTEP = 5
                 ADT0= ADT
                 NX = SX415
                 NY = SY415
                 NZ = SZ415
               ENDIF
            ENDIF
           END IF !((ABS(SX415)+ABS(SY415)+ABS(SZ415))>EM20) THEN
          ENDIF
         ENDIF !IF(IXX3 /= IXX4)THEN
C
      RETURN
      END
Chd|====================================================================
Chd|  INTERSECV0                    source/interfaces/int24/i24dst3.F
Chd|-- called by -----------
Chd|        I24DST3                       source/interfaces/int24/i24dst3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTERSECV0(
     1                    ISTEP    ,SUBTRIA,IXX3  ,IXX4  ,
     1                    INTERSECT,V12   ,V23    ,V34   ,V41    ,
     2                    VO12     ,VO23  ,VO34   ,VO41  ,XX1    ,
     3                    YY1      ,ZZ1   ,XX2    ,YY2   ,ZZ2    ,
     4                    XX3      ,YY3   ,ZZ3    ,XX4   ,YY4    ,
     5                    ZZ4      ,XX5   ,YY5    ,ZZ5   ,ADT0   ,
     6                    XOI      ,YOI   ,ZOI    ,XO1   ,XO2    ,
     7                    XO3     ,XO4    ,XO5    ,YO1   ,YO2    ,
     8                    YO3     ,YO4    ,YO5    ,ZO1   ,ZO2    ,
     9                    ZO3     ,ZO4    ,ZO5    ,NX    ,NY     ,
     A                    NZ      ,SX125  ,SX235  ,SX345 ,SX415  ,
     B                    SY125   ,SY235  ,SY345  ,SY415 ,SZ125  ,
     C                    SZ235  ,SZ345   ,SZ415  ,XI    ,YI     ,
     D                    ZI     ,ZEROMT  ,UNPT   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISTEP    ,SUBTRIA,IXX3  ,IXX4  ,INTERSECT
C     REAL
      my_real
     1     V12   ,V23    ,V34   ,V41    ,
     2     VO12     ,VO23  ,VO34   ,VO41  ,XX1    ,
     3     YY1      ,ZZ1   ,XX2    ,YY2   ,ZZ2    ,
     4     XX3      ,YY3   ,ZZ3    ,XX4   ,YY4    ,
     5     ZZ4      ,XX5   ,YY5    ,ZZ5   ,ADT0   ,
     6     XOI      ,YOI   ,ZOI    ,XO1   ,XO2    ,
     7     XO3     ,XO4    ,XO5    ,YO1   ,YO2    ,
     8     YO3     ,YO4    ,YO5    ,ZO1   ,ZO2    ,
     9     ZO3     ,ZO4    ,ZO5    ,NX    ,NY     ,
     A     NZ      ,SX125  ,SX235  ,SX345 ,SX415  ,
     B     SY125   ,SY235  ,SY345  ,SY415 ,SZ125  ,
     C     SZ235  ,SZ345   ,SZ415  ,
     D     ZEROMT,UNPT   ,XI,YI,ZI
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .     X51,  X52,  X53, X54,
     .     Y51,  Y52,  Y53, Y54,
     .     Z51,  Z52,  Z53, Z54,
     .     XI1,  XI2,  XI3,  XI4,  XI5,
     .     YI1,  YI2,  YI3,  YI4,  YI5,
     .     ZI1,  ZI2,  ZI3,  ZI4,  ZI5,
     .     XIA,  XIB,  XIC,
     .     YIA,  YIB,  YIC,
     .     ZIA,  ZIB,  ZIC,
     .     XS,YS,ZS,
     .     XM1, XM2, XM3, XM4, XM5, 
     .     YM1, YM2, YM3, YM4, YM5, 
     .     ZM1, ZM2, ZM3, ZM4, ZM5,LA0,LB0,LC0
      my_real
     .     SX,SX1,SX2,SX3,SX4,SX5,SX0,
     .     SY,SY1,SY2,SY3,SY4,SY5,SY0,
     .     SZ,SZ1,SZ2,SZ3,SZ4,SZ5,SZ0,AAA,ADT,S2
C------------------------------------------------
C---------------- cas V>0 V_old>0
         IF(VO12 > ZERO .and. V12 >= ZERO )THEN
          IF ((ABS(SX125)+ABS(SY125)+ABS(SZ125))>EM20) THEN
           AAA = ONE
           ADT = DT1

           XM1 = XO1
           YM1 = YO1
           ZM1 = ZO1 
         
           XM2 = XO2
           YM2 = YO2
           ZM2 = ZO2 
         
c           XOI = XI - ADT*VXI
c           YOI = YI - ADT*VYI
c           ZOI = ZI - ADT*VZI

           XM5 = XO5
           YM5 = YO5
           ZM5 = ZO5
         
           X51 = XM1 - XM5
           Y51 = YM1 - YM5
           Z51 = ZM1 - ZM5
 
           X52 = XM2 - XM5
           Y52 = YM2 - YM5
           Z52 = ZM2 - ZM5
 
           XI1 = XM1 - XOI
           YI1 = YM1 - YOI
           ZI1 = ZM1 - ZOI
 
           XI2 = XM2 - XOI
           YI2 = YM2 - YOI
           ZI2 = ZM2 - ZOI
         
           XI5 = XM5 - XOI
           YI5 = YM5 - YOI
           ZI5 = ZM5 - ZOI

           SX0 = Y51*Z52 - Z51*Y52
           SY0 = Z51*X52 - X51*Z52
           SZ0 = X51*Y52 - Y51*X52

           SX1 = YI5*ZI1 - ZI5*YI1
           SY1 = ZI5*XI1 - XI5*ZI1
           SZ1 = XI5*YI1 - YI5*XI1

           SX5 = YI1*ZI2 - ZI1*YI2
           SY5 = ZI1*XI2 - XI1*ZI2
           SZ5 = XI1*YI2 - YI1*XI2

           SX2 = YI2*ZI5 - ZI2*YI5
           SY2 = ZI2*XI5 - XI2*ZI5
           SZ2 = XI2*YI5 - YI2*XI5
           
           S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
           LB0 =  (SX0*SX2 + SY0*SY2 + SZ0*SZ2) * S2
           LC0 =  (SX0*SX1 + SY0*SY1 + SZ0*SZ1) * S2
           LA0 =  ONE-LB0-LC0 

           IF(LB0 >= ZEROMT .AND. LB0 <= UNPT .AND.
     .        LC0 >= ZEROMT .AND. LC0 <= UNPT .AND.
     .        LA0 >= ZEROMT .AND. LA0 <= UNPT)THEN
             INTERSECT = 1
             SUBTRIA= 1 ! subtriangle
             ISTEP = 5
             ADT0= ADT
             NX = SX125
             NY = SY125
             NZ = SZ125
           ENDIF
          END IF !((ABS(SX125)+ABS(SY125)+ABS(SZ125))>EM20) THEN
         ENDIF
         IF(IXX3 /= IXX4)THEN
           IF(VO23 > ZERO .and. V23 >= ZERO )THEN
              IF ((ABS(SX235)+ABS(SY235)+ABS(SZ235))>EM20) THEN
                AAA = ONE
                ADT = DT1
              
                XM2 = XO2
                YM2 = YO2
                ZM2 = ZO2 
              
                XM3 = XO3
                YM3 = YO3
                ZM3 = ZO3 
              
c                XOI = XI - ADT*VXI
c                YOI = YI - ADT*VYI
c                ZOI = ZI - ADT*VZI
              
                XM5 = XO5
                YM5 = YO5
                ZM5 = ZO5
              
                X52 = XM2 - XM5
                Y52 = YM2 - YM5
                Z52 = ZM2 - ZM5
              
                X53 = XM3 - XM5
                Y53 = YM3 - YM5
                Z53 = ZM3 - ZM5
              
                XI2 = XM2 - XOI
                YI2 = YM2 - YOI
                ZI2 = ZM2 - ZOI
              
                XI3 = XM3 - XOI
                YI3 = YM3 - YOI
                ZI3 = ZM3 - ZOI
              
                XI5 = XM5 - XOI
                YI5 = YM5 - YOI
                ZI5 = ZM5 - ZOI
              
                SX0 = Y52*Z53 - Z52*Y53
                SY0 = Z52*X53 - X52*Z53
                SZ0 = X52*Y53 - Y52*X53
              
                SX2 = YI5*ZI2 - ZI5*YI2
                SY2 = ZI5*XI2 - XI5*ZI2
                SZ2 = XI5*YI2 - YI5*XI2
              
                SX5 = YI2*ZI3 - ZI2*YI3
                SY5 = ZI2*XI3 - XI2*ZI3
                SZ5 = XI2*YI3 - YI2*XI3
              
                SX3 = YI3*ZI5 - ZI3*YI5
                SY3 = ZI3*XI5 - XI3*ZI5
                SZ3 = XI3*YI5 - YI3*XI5
                
                S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
                LB0 =  (SX0*SX3 + SY0*SY3 + SZ0*SZ3) * S2
                LC0 =  (SX0*SX2 + SY0*SY2 + SZ0*SZ2) * S2
                LA0 =  ONE-LB0-LC0 
              
                IF(LB0 >= ZEROMT.and. LB0 <= UNPT .and.
     .             LC0 >= ZEROMT.and. LC0 <= UNPT .and.
     .             LA0 >= ZEROMT.and. LA0 <= UNPT)THEN
                  INTERSECT = 1
              
                  IF(ADT > ADT0)THEN
                    SUBTRIA= 2 ! subtriangle
                    ISTEP = 5
                    ADT0= ADT
                    NX = SX235
                    NY = SY235
                    NZ = SZ235
                  ENDIF
                ENDIF
              END IF !((ABS(SX235)+ABS(SY235)+ABS(SZ235))>EM20) THEN
           END IF!(VO23 > ZERO .and. V23 >= ZERO )THEN
          IF(VO34 > ZERO .and. V34 >= ZERO )THEN
           IF ((ABS(SX345)+ABS(SY345)+ABS(SZ345))>EM20) THEN
            AAA = ONE
            ADT = DT1

            XM3 = XO3
            YM3 = YO3
            ZM3 = ZO3 
         
            XM4 = XO4
            YM4 = YO4
            ZM4 = ZO4
         
c            XOI = XI - ADT*VXI
c            YOI = YI - ADT*VYI
c            ZOI = ZI - ADT*VZI

            XM5 = XO5
            YM5 = YO5
            ZM5 = ZO5
         
            X53 = XM3 - XM5
            Y53 = YM3 - YM5
            Z53 = ZM3 - ZM5
 
            X54 = XM4 - XM5
            Y54 = YM4 - YM5
            Z54 = ZM4 - ZM5
 
            XI3 = XM3 - XOI
            YI3 = YM3 - YOI
            ZI3 = ZM3 - ZOI
 
            XI4 = XM4 - XOI
            YI4 = YM4 - YOI
            ZI4 = ZM4 - ZOI
         
            XI5 = XM5 - XOI
            YI5 = YM5 - YOI
            ZI5 = ZM5 - ZOI

            SX0 = Y53*Z54 - Z53*Y54
            SY0 = Z53*X54 - X53*Z54
            SZ0 = X53*Y54 - Y53*X54

            SX3 = YI5*ZI3 - ZI5*YI3
            SY3 = ZI5*XI3 - XI5*ZI3
            SZ3 = XI5*YI3 - YI5*XI3

            SX5 = YI3*ZI4 - ZI3*YI4
            SY5 = ZI3*XI4 - XI3*ZI4
            SZ5 = XI3*YI4 - YI3*XI4

            SX4 = YI4*ZI5 - ZI4*YI5
            SY4 = ZI4*XI5 - XI4*ZI5
            SZ4 = XI4*YI5 - YI4*XI5
            
            S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
            LB0 =  (SX0*SX4 + SY0*SY4 + SZ0*SZ4) * S2
            LC0 =  (SX0*SX3 + SY0*SY3 + SZ0*SZ3) * S2
            LA0 =  ONE-LB0-LC0 

           IF(LB0  >= ZEROMT.and. LB0 <= UNPT .and.
     .         LC0 >= ZEROMT.and. LC0 <= UNPT .and.
     .         LA0 >= ZEROMT.and. LA0 <= UNPT)THEN
               INTERSECT = 1
               IF(ADT > ADT0)THEN
                 SUBTRIA= 3 ! subtriangle
                 ISTEP = 5
                 ADT0= ADT
                 NX = SX345
                 NY = SY345
                 NZ = SZ345
               ENDIF
            ENDIF
           END IF !((ABS(SX345)+ABS(SY345)+ABS(SZ345))>EM20) THEN
          ENDIF

          IF(VO41 > ZERO .and. V41 >= ZERO )THEN
           IF ((ABS(SX415)+ABS(SY415)+ABS(SZ415))>EM20) THEN
            AAA = ONE
            ADT = DT1

            XM4 = XO4
            YM4 = YO4
            ZM4 = ZO4 
         
            XM1 = XO1
            YM1 = YO1
            ZM1 = ZO1 
         
c            XOI = XI - ADT*VXI
c            YOI = YI - ADT*VYI
c            ZOI = ZI - ADT*VZI

            XM5 = XO5
            YM5 = YO5
            ZM5 = ZO5
         
            X54 = XM4 - XM5
            Y54 = YM4 - YM5
            Z54 = ZM4 - ZM5
 
            X51 = XM1 - XM5
            Y51 = YM1 - YM5
            Z51 = ZM1 - ZM5
 
            XI4 = XM4 - XOI
            YI4 = YM4 - YOI
            ZI4 = ZM4 - ZOI
 
            XI1 = XM1 - XOI
            YI1 = YM1 - YOI
            ZI1 = ZM1 - ZOI
         
            XI5 = XM5 - XOI
            YI5 = YM5 - YOI
            ZI5 = ZM5 - ZOI

            SX0 = Y54*Z51 - Z54*Y51
            SY0 = Z54*X51 - X54*Z51
            SZ0 = X54*Y51 - Y54*X51

            SX4 = YI5*ZI4 - ZI5*YI4
            SY4 = ZI5*XI4 - XI5*ZI4
            SZ4 = XI5*YI4 - YI5*XI4

            SX5 = YI4*ZI1 - ZI4*YI1
            SY5 = ZI4*XI1 - XI4*ZI1
            SZ5 = XI4*YI1 - YI4*XI1

            SX1 = YI1*ZI5 - ZI1*YI5
            SY1 = ZI1*XI5 - XI1*ZI5
            SZ1 = XI1*YI5 - YI1*XI5
            
            S2 = ONE/MAX(EM30,SX0*SX0 + SY0*SY0 + SZ0*SZ0)
            LB0 =  (SX0*SX1 + SY0*SY1 + SZ0*SZ1) * S2
            LC0 =  (SX0*SX4 + SY0*SY4 + SZ0*SZ4) * S2
            LA0 =  ONE-LB0-LC0 

            IF(LB0 >= ZEROMT.and. LB0 <= UNPT .and.
     .         LC0 >= ZEROMT.and. LC0 <= UNPT .and.
     .         LA0 >= ZEROMT.and. LA0 <= UNPT)THEN
               INTERSECT = 1
               IF(ADT > ADT0)THEN
                 SUBTRIA= 4 ! subtriangle
                 ISTEP = 5
                 ADT0= ADT
                 NX = SX415
                 NY = SY415
                 NZ = SZ415
               ENDIF
            ENDIF
           END IF !((ABS(SX345)+ABS(SY345)+ABS(SZ345))>EM20) THEN
          ENDIF
         END IF!(IXX3 /= IXX4)THEN
C
      RETURN
      END
Chd|====================================================================
Chd|  INTERSECSH                    source/interfaces/int24/i24dst3.F
Chd|-- called by -----------
Chd|        I24DST3                       source/interfaces/int24/i24dst3.F
Chd|-- calls ---------------
Chd|        N2EDGE3                       source/interfaces/int24/i24dst3.F
Chd|====================================================================
      SUBROUTINE INTERSECSH(
     1                    ISTEP    ,SUBTRIA0,IXX3  ,IXX4  ,
     1                    INTERSECT,XX1    ,
     3                    YY1      ,ZZ1   ,XX2    ,YY2   ,ZZ2    ,
     4                    XX3      ,YY3   ,ZZ3    ,XX4   ,YY4    ,
     5                    ZZ4      ,XX5   ,YY5    ,ZZ5   ,ADT0   ,
     6                    XOI      ,YOI   ,ZOI    ,XO1   ,XO2    ,
     7                    XO3     ,XO4    ,XO5    ,YO1   ,YO2    ,
     8                    YO3     ,YO4    ,YO5    ,ZO1   ,ZO2    ,
     9                    ZO3     ,ZO4    ,ZO5    ,NX    ,NY     ,
     A                    NZ      ,SX125  ,SX235  ,SX345 ,SX415  ,
     B                    SY125   ,SY235  ,SY345  ,SY415 ,SZ125  ,
     C                    SZ235  ,SZ345   ,SZ415  ,XI    ,YI     ,
     D                    ZI     ,SOX125  ,SOX235 ,SOX345 ,SOX415  ,
     B                    SOY125 ,SOY235  ,SOY345 ,SOY415 ,SOZ125  ,
     C                    SOZ235 ,SOZ345  ,SOZ415 ,MVOISIN )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISTEP    ,SUBTRIA0,IXX3  ,IXX4  ,INTERSECT,MVOISIN(4)
C     REAL
      my_real
     1     XX1    ,
     3     YY1      ,ZZ1   ,XX2    ,YY2   ,ZZ2    ,
     4     XX3      ,YY3   ,ZZ3    ,XX4   ,YY4    ,
     5     ZZ4      ,XX5   ,YY5    ,ZZ5   ,ADT0   ,
     6     XOI      ,YOI   ,ZOI    ,XO1   ,XO2    ,
     7     XO3     ,XO4    ,XO5    ,YO1   ,YO2    ,
     8     YO3     ,YO4    ,YO5    ,ZO1   ,ZO2    ,
     9     ZO3     ,ZO4    ,ZO5    ,NX    ,NY     ,
     A     NZ      ,SX125  ,SX235  ,SX345 ,SX415  ,
     B     SY125   ,SY235  ,SY345  ,SY415 ,SZ125  ,
     C     SZ235  ,SZ345   ,SZ415  ,
     D     SOX125  ,SOX235 ,SOX345 ,SOX415  ,
     B     SOY125 ,SOY235  ,SOY345 ,SOY415 ,SOZ125  ,
     C     SOZ235 ,SOZ345  ,SOZ415 ,
     D     XI,YI,ZI
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IEG,I,J,ipr,NEG,SUBTRIA
      my_real
     .     X51,  X52,  X53, X54,
     .     Y51,  Y52,  Y53, Y54,
     .     Z51,  Z52,  Z53, Z54,
     .     XI1,  XI2,  XI3,  XI4,  XI5,
     .     YI1,  YI2,  YI3,  YI4,  YI5,
     .     ZI1,  ZI2,  ZI3,  ZI4,  ZI5,
     .     XIA,  XIB,  XIC,
     .     YIA,  YIB,  YIC,
     .     ZIA,  ZIB,  ZIC,
     .     XS,YS,ZS,
     .     XM1, XM2, XM3, XM4, XM5, 
     .     YM1, YM2, YM3, YM4, YM5, 
     .     ZM1, ZM2, ZM3, ZM4, ZM5,LA0,LB0,LC0
      my_real
     .     SX,SX1,SX2,SX3,SX4,SX5,SX0,
     .     SY,SY1,SY2,SY3,SY4,SY5,SY0,
     .     SZ,SZ1,SZ2,SZ3,SZ4,SZ5,SZ0,AAA,ADT,S2,
     .     DNO(4),DMIN,PIJ,PIK,PIJ0
C------------------------------------------------
! 4-------3
! |\     /|
! | \ 3 / |
! |  \ /2 |
! |   5   |
! | 4/ \  |
! | / 1 \ |
! |/     \|
! 1-------2
!C-----case secnd shell node/edge shell (12,23,34,41)
C--------edge selection: first look at smallest distance DNO,then final intersection
         NEG = 0
         DO J=1,4
          IF (MVOISIN(J)==0) NEG=NEG+1
         END DO
         IF (NEG==0) RETURN
         DNO(1) = (XO1-XOI)*(XO1-XOI)+(YO1-YOI)*(YO1-YOI)+(ZO1-ZOI)*(ZO1-ZOI)
         IF (MVOISIN(1)/=0.AND.MVOISIN(4)/=0) DNO(1)=EP10
         DNO(2) = (XO2-XOI)*(XO2-XOI)+(YO2-YOI)*(YO2-YOI)+(ZO2-ZOI)*(ZO2-ZOI)
         IF (MVOISIN(1)/=0.AND.MVOISIN(2)/=0) DNO(2)=EP10
         DNO(3) = (XO3-XOI)*(XO3-XOI)+(YO3-YOI)*(YO3-YOI)+(ZO3-ZOI)*(ZO3-ZOI)
         IF (MVOISIN(3)/=0.AND.MVOISIN(2)/=0) DNO(3)=EP10
         IF(IXX3 /= IXX4)THEN
           DNO(4) = (XO4-XOI)*(XO4-XOI)+(YO4-YOI)*(YO4-YOI)+(ZO4-ZOI)*(ZO4-ZOI)
           IF (MVOISIN(3)/=0.AND.MVOISIN(4)/=0) DNO(4)=EP10
         ELSE
           DNO(4) = DNO(3)
         END IF
         DMIN = EP20
         IEG =0 
         DO J=1,4
          IF (DNO(J)<DMIN) THEN
           DMIN = DNO(J)
           IEG = J
          END IF
         END DO
C--------edge selection: smallest pen between IJ,IK
         PIJ = -EP10
         PIK = ZERO
         SELECT CASE (IEG)
           CASE (1) ! 12,41
            IF (MVOISIN(1)==0) THEN
             CALL N2EDGE3(XX1    ,YY1      ,ZZ1    ,
     2                    XX2    ,YY2      ,ZZ2    ,
     3                    SX125  ,SY125    ,SZ125  ,
     4                    XI     ,YI       ,ZI     ,PIJ )
             IF (PIJ<ZERO) THEN 
               CALL N2EDGE3(XO1    ,YO1      ,ZO1    ,
     2                      XO2    ,YO2      ,ZO2    ,
     3                      SOX125 ,SOY125   ,SOZ125  ,
     4                      XOI    ,YOI      ,ZOI    ,PIJ0 )
               IF (PIJ0>ZERO) THEN 
                INTERSECT = 1
                SUBTRIA= 20 + 1
               END IF
             END IF
            END IF !(MVOISIN(1)==0) THEN
            IF (MVOISIN(4)==0) THEN
             CALL N2EDGE3(XX4    ,YY4      ,ZZ4    ,
     2                    XX1    ,YY1      ,ZZ1    ,
     3                    SX415  ,SY415    ,SZ415  ,
     4                    XI     ,YI       ,ZI     ,PIK )
             IF (PIK<ZERO.AND.PIK>PIJ) THEN
               CALL N2EDGE3(XO4    ,YO4      ,ZO4    ,
     2                      XO1    ,YO1      ,ZO1    ,
     3                      SOX415 ,SOY415   ,SOZ415  ,
     4                      XOI    ,YOI      ,ZOI    ,PIJ0 )
               IF (PIJ0>ZERO) THEN 
                INTERSECT = 1
                SUBTRIA= 20 + 4
               END IF
             END IF
            END IF 
           CASE (2) ! 23,12
            IF (MVOISIN(2)==0) THEN
             CALL N2EDGE3(XX2    ,YY2      ,ZZ2    ,
     2                    XX3    ,YY3      ,ZZ3    ,
     3                    SX235  ,SY235    ,SZ235  ,
     4                    XI     ,YI       ,ZI     ,PIJ )
             IF (PIJ<ZERO) THEN 
               CALL N2EDGE3(XO2    ,YO2      ,ZO2    ,
     2                      XO3    ,YO3      ,ZO3    ,
     3                      SOX235 ,SOY235   ,SOZ235  ,
     4                      XOI    ,YOI      ,ZOI    ,PIJ0 )
               IF (PIJ0>ZERO) THEN 
                INTERSECT = 1
                SUBTRIA= 20 + 2
               END IF
             END IF
            END IF 
            IF (MVOISIN(1)==0) THEN
             CALL N2EDGE3(XX1    ,YY1      ,ZZ1    ,
     2                    XX2    ,YY2      ,ZZ2    ,
     3                    SX125  ,SY125    ,SZ125  ,
     4                    XI     ,YI       ,ZI     ,PIK )
             IF (PIK<ZERO.AND.PIK>PIJ) THEN
               CALL N2EDGE3(XO1    ,YO1      ,ZO1    ,
     2                      XO2    ,YO2      ,ZO2    ,
     3                      SOX125  ,SOY125    ,SOZ125  ,
     4                      XOI    ,YOI      ,ZOI    ,PIJ0 )
               IF (PIJ0>ZERO) THEN 
                INTERSECT = 1
                SUBTRIA= 20 + 1
               END IF
             END IF
            END IF 
           CASE (3) ! 34,23
             IF(IXX3 /= IXX4)THEN
               IF(MVOISIN(3)==0)THEN
                CALL N2EDGE3(XX3    ,YY3      ,ZZ3    ,
     2                       XX4    ,YY4      ,ZZ4    ,
     3                       SX345  ,SY345    ,SZ345  ,
     4                       XI     ,YI       ,ZI     ,PIJ )
                IF (PIJ<ZERO) THEN 
                  CALL N2EDGE3(XO3    ,YO3      ,ZO3    ,
     2                         XO4    ,YO4      ,ZO4    ,
     3                         SOX345  ,SOY345    ,SOZ345  ,
     4                         XOI    ,YOI      ,ZOI    ,PIJ0)
                  IF (PIJ0>ZERO) THEN 
                   INTERSECT = 1
                   SUBTRIA= 20 + 3
                  END IF
                END IF
               END IF
             ELSE   ! 3N : 41,23
              IF (MVOISIN(4)==0) THEN
                CALL N2EDGE3(XX4    ,YY4      ,ZZ4    ,
     2                       XX1    ,YY1      ,ZZ1    ,
     3                       SX415  ,SY415    ,SZ415  ,
     4                       XI     ,YI       ,ZI     ,PIJ )
                IF (PIJ<ZERO) THEN
                  CALL N2EDGE3(XO4    ,YO4      ,ZO4    ,
     2                      XO1    ,YO1      ,ZO1    ,
     3                      SOX415  ,SOY415    ,SOZ415  ,
     4                      XOI    ,YOI      ,ZOI    ,PIJ0 )
                  IF (PIJ0>ZERO) THEN 
                   INTERSECT = 1
                   SUBTRIA= 20 + 4
                  END IF
                END IF
              END IF 
             END IF 
C--------------  23 for both 3N/4N           
              IF(MVOISIN(2)==0)THEN !23
                CALL N2EDGE3(XX2    ,YY2      ,ZZ2    ,
     2                       XX3    ,YY3      ,ZZ3    ,
     3                       SX235  ,SY235    ,SZ235  ,
     4                       XI     ,YI       ,ZI     ,PIK )
                IF (PIK<ZERO.AND.PIK>PIJ) THEN
                  CALL N2EDGE3(XO2    ,YO2      ,ZO2    ,
     2                      XO3    ,YO3      ,ZO3    ,
     3                      SOX235  ,SOY235    ,SOZ235  ,
     4                      XOI    ,YOI      ,ZOI    ,PIJ0 )
                   IF (PIJ0>ZERO) THEN 
                    INTERSECT = 1
                    SUBTRIA= 20 + 2
                   END IF
                 END IF
              END IF
           CASE (4) ! 41,34
            IF(MVOISIN(4)==0)THEN 
             CALL N2EDGE3(XX4    ,YY4      ,ZZ4    ,
     2                    XX1    ,YY1      ,ZZ1    ,
     3                    SX415  ,SY415    ,SZ415  ,
     4                    XI     ,YI       ,ZI     ,PIJ )
             IF (PIJ<ZERO) THEN
               CALL N2EDGE3(XO4    ,YO4      ,ZO4    ,
     2                      XO1    ,YO1      ,ZO1    ,
     3                      SOX415  ,SOY415    ,SOZ415  ,
     4                      XOI    ,YOI      ,ZOI    ,PIJ0 )
               IF (PIJ0>ZERO) THEN 
                INTERSECT = 1
                SUBTRIA= 20 + 4
               END IF
             END IF
            END IF 
            IF(IXX3 /= IXX4.AND.MVOISIN(3)==0)THEN
             CALL N2EDGE3(XX3    ,YY3      ,ZZ3    ,
     2                    XX4    ,YY4      ,ZZ4    ,
     3                    SX345  ,SY345    ,SZ345  ,
     4                    XI     ,YI       ,ZI     ,PIK )
                IF (PIK<ZERO.AND.PIK>PIJ) THEN 
                  CALL N2EDGE3(XO3    ,YO3      ,ZO3    ,
     2                         XO4    ,YO4      ,ZO4    ,
     3                         SOX345  ,SOY345    ,SOZ345  ,
     4                         XOI    ,YOI      ,ZOI    ,PIJ0)
                  IF (PIJ0>ZERO) THEN 
                   INTERSECT = 1
                   SUBTRIA= 20 + 3
                  END IF
                END IF
            END IF 
         END SELECT
C------------out of seg     
         IF (INTERSECT == 1.AND.PIK>ZERO) INTERSECT = 0
         IF(INTERSECT == 1) THEN
          SUBTRIA0 = SUBTRIA
          IF(IXX3 == IXX4) SUBTRIA0= 20 + 1
          ISTEP = 6
          ADT0 = DT1
         END IF 
C
      RETURN
      END
Chd|====================================================================
Chd|  N2EDGE3                       source/interfaces/int24/i24dst3.F
Chd|-- called by -----------
Chd|        INTERSECSH                    source/interfaces/int24/i24dst3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE N2EDGE3(
     1                           XXI    ,YYI      ,ZZI    ,
     2                           XXJ    ,YYJ      ,ZZJ    ,
     3                           NX     ,NY       ,NZ     ,
     4                           XI     ,YI       ,ZI     ,BBB )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     1                           XXI    ,YYI      ,ZZI    ,
     2                           XXJ    ,YYJ      ,ZZJ    ,
     3                           NX     ,NY       ,NZ     ,
     4                           XI     ,YI       ,ZI     ,BBB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I
      my_real
     .     XMIJ(3),NIJ(3),DI(3),AA,NORM
C-----------------------------------------------
             XMIJ(1)=XXJ-XXI
             XMIJ(2)=YYJ-YYI
             XMIJ(3)=ZZJ-ZZI
             NIJ(1)= NY*XMIJ(3) - NZ*XMIJ(2)
             NIJ(2)= NZ*XMIJ(1) - NX*XMIJ(3)
             NIJ(3)= NX*XMIJ(2) - NY*XMIJ(1)
             AA = NIJ(1)*NIJ(1)+NIJ(2)*NIJ(2)+NIJ(3)*NIJ(3)
             NORM=MAX(EM20,SQRT(AA))
             DI(1)=HALF*(XXI+XXJ)-XI
             DI(2)=HALF*(YYI+YYJ)-YI
             DI(3)=HALF*(ZZI+ZZJ)-ZI
             BBB = (DI(1)*NIJ(1)+DI(2)*NIJ(2)+DI(3)*NIJ(3))/NORM
C
      RETURN
      END
