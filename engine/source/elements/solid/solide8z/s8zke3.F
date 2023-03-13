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
Chd|  S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|-- called by -----------
Chd|        IMP_GLOB_K                    source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|        ASSEM_S8                      source/implicit/assem_s8.F    
Chd|        MMATS                         source/elements/solid/solide8z/mmats.F
Chd|        MMAT_H1                       source/elements/solid/solide8z/s8zke3.F
Chd|        MMSTIFS                       source/elements/solid/solide8z/mmats.F
Chd|        S8EOFF                        source/elements/solid/solide8/s8eoff.F
Chd|        S8ZCUMG3                      source/elements/solid/solide8z/s8zcumg3.F
Chd|        S8ZDERICT3                    source/elements/solid/solide8z/s8zderi3.F
Chd|        S8ZDERII3                     source/elements/solid/solide8z/s8zderi3.F
Chd|        S8ZKGEI3                      source/elements/solid/solide8z/s8zke3.F
Chd|        S8ZKGEO3                      source/elements/solid/solide8z/s8zke3.F
Chd|        S8ZLKE3                       source/elements/solid/solide8z/s8zlke3.F
Chd|        S8ZLKEP3                      source/elements/solid/solide8z/s8zke3.F
Chd|        S8ZSIGP3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SRCOORK                       source/elements/solid/solide8z/srcoork.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE S8ZKE3 (PM     ,GEO    ,IXS    ,X      ,ELBUF_STR,
     1                   NEL    ,ICP    ,ICSIG  ,ETAG   ,
     2                   IDDL   ,NDOF   ,K_DIAG ,K_LT   ,IADK     ,
     3                   JDIK   ,MPT    ,IPM    ,IGEO   ,IKGEO  ,
     4                   BUFMAT )
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
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NEL ,ICP, ICSIG,IKGEO,MPT
      INTEGER NDOF(*)  ,IADK(*) ,JDIK(*),
     .   IXS(NIXS,*),IPM(NPROPMI,*),IGEO(NPROPGI,*),ETAG(*),IDDL(*)  
C
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*), X(*),
     .   K11(9,MVSIZ) , K12(9,MVSIZ)  ,K13(9,MVSIZ) ,K14(9,MVSIZ)  ,
     .   K15(9,MVSIZ) , K16(9,MVSIZ)  ,K17(9,MVSIZ) ,K18(9,MVSIZ)  ,
     .   K22(9,MVSIZ)  ,K23(9,MVSIZ)  ,K24(9,MVSIZ) ,K25(9,MVSIZ) ,
     .   K26(9,MVSIZ)  ,K27(9,MVSIZ)  ,K28(9,MVSIZ) ,K33(9,MVSIZ) ,
     .   K34(9,MVSIZ)  ,K35(9,MVSIZ)  ,K36(9,MVSIZ) ,K37(9,MVSIZ) ,
     .   K38(9,MVSIZ)  ,K44(9,MVSIZ)  ,K45(9,MVSIZ) ,K46(9,MVSIZ) ,
     .   K47(9,MVSIZ)  ,K48(9,MVSIZ)  ,K55(9,MVSIZ) ,K56(9,MVSIZ) ,
     .   K57(9,MVSIZ)  ,K58(9,MVSIZ)  ,K66(9,MVSIZ) ,K67(9,MVSIZ) ,
     .   K68(9,MVSIZ)  ,K77(9,MVSIZ)  ,K78(9,MVSIZ) ,K88(9,MVSIZ) ,
     .   OFFG(MVSIZ)    ,BUFMAT(*)   ,K_DIAG(*)   ,K_LT(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LCO,NF1,IFLAG,NB3S,I,J,IKSUP,IKORTH,IADBUF,IPREDU,
     .  IR, IS, IT,IL,IP,ICR,ICS,ICT,ICPG,NPTR,NPTS,NPTT,NLAY,NPPT,
     .  IAD0,PID
      INTEGER MXT(MVSIZ),NGL(MVSIZ),NGEO(MVSIZ),IDEG(MVSIZ)
      my_real
     . VOLN(MVSIZ), DELTAX(MVSIZ),
     . AJC1(MVSIZ) , AJC2(MVSIZ) , AJC3(MVSIZ) ,
     . AJC4(MVSIZ) , AJC5(MVSIZ) , AJC6(MVSIZ) ,
     . AJC7(MVSIZ) , AJC8(MVSIZ) , AJC9(MVSIZ) ,
     . AJ1(MVSIZ) , AJ2(MVSIZ) , AJ3(MVSIZ) ,
     . AJ4(MVSIZ) , AJ5(MVSIZ) , AJ6(MVSIZ) , 
     . AJ7(MVSIZ) , AJ8(MVSIZ) , AJ9(MVSIZ) 
C----------------
      INTEGER NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ), 
     .        NC5(MVSIZ), NC6(MVSIZ), NC7(MVSIZ), NC8(MVSIZ)
      my_real
     .   OFF(MVSIZ) , RHOO(MVSIZ),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   X5(MVSIZ), X6(MVSIZ), X7(MVSIZ), X8(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Y5(MVSIZ), Y6(MVSIZ), Y7(MVSIZ), Y8(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   Z5(MVSIZ), Z6(MVSIZ), Z7(MVSIZ), Z8(MVSIZ),
     .  VX1(MVSIZ),VX2(MVSIZ),VX3(MVSIZ),VX4(MVSIZ),
     .  VX5(MVSIZ),VX6(MVSIZ),VX7(MVSIZ),VX8(MVSIZ),
     .  VY1(MVSIZ),VY2(MVSIZ),VY3(MVSIZ),VY4(MVSIZ),
     .  VY5(MVSIZ),VY6(MVSIZ),VY7(MVSIZ),VY8(MVSIZ),
     .  VZ1(MVSIZ),VZ2(MVSIZ),VZ3(MVSIZ),VZ4(MVSIZ),
     .  VZ5(MVSIZ),VZ6(MVSIZ),VZ7(MVSIZ),VZ8(MVSIZ),
     .  HX1(MVSIZ),HX2(MVSIZ),HX3(MVSIZ),HX4(MVSIZ),
     .  HY1(MVSIZ),HY2(MVSIZ),HY3(MVSIZ),HY4(MVSIZ),
     .  HZ1(MVSIZ),HZ2(MVSIZ),HZ3(MVSIZ),HZ4(MVSIZ),
     .  PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),PX4(MVSIZ),
     .  PX5(MVSIZ),PX6(MVSIZ),PX7(MVSIZ),PX8(MVSIZ),
     .  PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),
     .  PY5(MVSIZ),PY6(MVSIZ),PY7(MVSIZ),PY8(MVSIZ),
     .  PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ),
     .  PZ5(MVSIZ),PZ6(MVSIZ),PZ7(MVSIZ),PZ8(MVSIZ),
     .  PXY1(MVSIZ),PXY2(MVSIZ),PXY3(MVSIZ),PXY4(MVSIZ),
     .  PXY5(MVSIZ),PXY6(MVSIZ),PXY7(MVSIZ),PXY8(MVSIZ),
     .  PYX1(MVSIZ),PYX2(MVSIZ),PYX3(MVSIZ),PYX4(MVSIZ),
     .  PYX5(MVSIZ),PYX6(MVSIZ),PYX7(MVSIZ),PYX8(MVSIZ),
     .  PXZ1(MVSIZ),PXZ2(MVSIZ),PXZ3(MVSIZ),PXZ4(MVSIZ),
     .  PXZ5(MVSIZ),PXZ6(MVSIZ),PXZ7(MVSIZ),PXZ8(MVSIZ),
     .  PZX1(MVSIZ),PZX2(MVSIZ),PZX3(MVSIZ),PZX4(MVSIZ),
     .  PZX5(MVSIZ),PZX6(MVSIZ),PZX7(MVSIZ),PZX8(MVSIZ),
     .  PYZ1(MVSIZ),PYZ2(MVSIZ),PYZ3(MVSIZ),PYZ4(MVSIZ),
     .  PYZ5(MVSIZ),PYZ6(MVSIZ),PYZ7(MVSIZ),PYZ8(MVSIZ),
     .  PZY1(MVSIZ),PZY2(MVSIZ),PZY3(MVSIZ),PZY4(MVSIZ),
     .  PZY5(MVSIZ),PZY6(MVSIZ),PZY7(MVSIZ),PZY8(MVSIZ),
     .  PXC1(MVSIZ),PXC2(MVSIZ),PXC3(MVSIZ),PXC4(MVSIZ),
     .  PYC1(MVSIZ),PYC2(MVSIZ),PYC3(MVSIZ),PYC4(MVSIZ),
     .  PZC1(MVSIZ),PZC2(MVSIZ),PZC3(MVSIZ),PZC4(MVSIZ),
     .  BXY1(MVSIZ),BXY2(MVSIZ),BXY3(MVSIZ),BXY4(MVSIZ),
     .  BXY5(MVSIZ),BXY6(MVSIZ),BXY7(MVSIZ),BXY8(MVSIZ),
     .  BYX1(MVSIZ),BYX2(MVSIZ),BYX3(MVSIZ),BYX4(MVSIZ),
     .  BYX5(MVSIZ),BYX6(MVSIZ),BYX7(MVSIZ),BYX8(MVSIZ),
     .  BXZ1(MVSIZ),BXZ2(MVSIZ),BXZ3(MVSIZ),BXZ4(MVSIZ),
     .  BXZ5(MVSIZ),BXZ6(MVSIZ),BXZ7(MVSIZ),BXZ8(MVSIZ),
     .  BZX1(MVSIZ),BZX2(MVSIZ),BZX3(MVSIZ),BZX4(MVSIZ),
     .  BZX5(MVSIZ),BZX6(MVSIZ),BZX7(MVSIZ),BZX8(MVSIZ),
     .  BYZ1(MVSIZ),BYZ2(MVSIZ),BYZ3(MVSIZ),BYZ4(MVSIZ),
     .  BYZ5(MVSIZ),BYZ6(MVSIZ),BYZ7(MVSIZ),BYZ8(MVSIZ),
     .  BZY1(MVSIZ),BZY2(MVSIZ),BZY3(MVSIZ),BZY4(MVSIZ),
     .  BZY5(MVSIZ),BZY6(MVSIZ),BZY7(MVSIZ),BZY8(MVSIZ),
     .  PX1H1(MVSIZ),PX2H1(MVSIZ),PX3H1(MVSIZ),PX4H1(MVSIZ),
     .  PX1H2(MVSIZ),PX2H2(MVSIZ),PX3H2(MVSIZ),PX4H2(MVSIZ),
     .  PX1H3(MVSIZ),PX2H3(MVSIZ),PX3H3(MVSIZ),PX4H3(MVSIZ),
     .  PX1H4(MVSIZ),PX2H4(MVSIZ),PX3H4(MVSIZ),PX4H4(MVSIZ),
     .   HX(MVSIZ,4), HY(MVSIZ,4),  HZ(MVSIZ,4) 
      my_real
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   G1X(MVSIZ),G2X(MVSIZ),G3X(MVSIZ),
     .   G1Y(MVSIZ),G2Y(MVSIZ),G3Y(MVSIZ),
     .   G1Z(MVSIZ),G2Z(MVSIZ),G3Z(MVSIZ)
      my_real
     .   BID,WI,SMAX(MVSIZ),VOLG(MVSIZ),NU(MVSIZ),PP(MVSIZ),USB(MVSIZ),
     .   VOLM(MVSIZ),SIGM(MVSIZ),NU1(MVSIZ),GAMA(MVSIZ,6)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
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

       my_real
     .      HH(2,MVSIZ),FAC(MVSIZ),C1,E0(MVSIZ),DD(9,MVSIZ),GG(MVSIZ),
     .      DM(9,MVSIZ),GM(9,MVSIZ),DGM(9,MVSIZ),DG(9,MVSIZ),
     .      G33(9,MVSIZ),HH1(2,MVSIZ)
C----HH(1,):lamda,HH(2,):G; HH1 :effective lamda,g for Icpre=1
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      GBUF => ELBUF_STR%GBUF
      NLAY = ELBUF_STR%NLAY
      IF (MPT == 222) THEN
        NPTR = 2
        NPTS = 2
        NPTT = 2
      ELSE
        NPTR = ELBUF_STR%NPTR
        NPTS = ELBUF_STR%NPTS
        NPTT = ELBUF_STR%NPTT
      ENDIF
      IAD0 = 1
      IF (ISORTH > 0) IAD0 = 1 + 6*NEL
      IF (IGTYP == 21.OR.IGTYP == 22) THEN
       IKORTH=2
      ELSEIF (ISORTH>0) THEN
       IKORTH=1
      ELSE
       IKORTH=0
      ENDIF
C-----------
      NF1=NFT+1
C-----------
      CALL SRCOORK(X,IXS(1,NF1),
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   GBUF%OFF,OFFG,GBUF%SMSTR,
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33, 
     .   NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,NGL,MXT,NGEO,
     .   K11,K12,K13,K14,K15,K16,K17,K18,K22,K23,
     .   K24,K25,K26,K27,K28,K33,K34,K35,K36,K37,
     .   K38,K44,K45,K46,K47,K48,K55,K56,K57,K58,
     .   K66,K67,K68,K77,K78,K88,JHBE,GBUF%GAMA,GAMA,
     .   NEL)
C
      PID=NGEO(LFT)
C      IGTYP=NINT(GEO(12,PID))
C      CALL GET_GAMA(MTN,NEL,NBGAMA,JCVT,PM,IXS(1,NF1),IGTYP)
      DO I=LFT,LLT
        NU(I)=MIN(HALF,PM(21,MXT(I)))
        C1 =PM(32,MXT(I))
        E0(I) =THREE*(ONE-TWO*NU(I))*C1
C        USB(I)=EM01/C1
        DELTAX(I)=EP30
	      	IDEG(I)=0
      ENDDO
      IF (JHBE/=14.AND.JHBE<20) THEN
       DO I=LFT,LLT
        NU(I)=ZERO
       ENDDO
      ELSE
      ENDIF 
      ICPG = ICP
      IF (JHBE/=14.AND.JHBE/=17.AND.JHBE/=24) THEN
       ICPG = 1
      ENDIF 
c
      CALL S8ZDERICT3(OFFG,VOLG,NGL,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   PXC1, PXC2, PXC3, PXC4,
     .   PYC1, PYC2, PYC3, PYC4,
     .   PZC1, PZC2, PZC3, PZC4,
     .   PX1H1, PX1H2, PX1H3, PX1H4,
     .   PX2H1, PX2H2, PX2H3, PX2H4,
     .   PX3H1, PX3H2, PX3H3, PX3H4,
     .   PX4H1, PX4H2, PX4H3, PX4H4,
     .   HX,   HY,   HZ,   
     .   AJC1,AJC2,AJC3,
     .   AJC4,AJC5,AJC6,
     .   AJC7,AJC8,AJC9,SMAX)
      IF (ICPG==2) THEN
       CALL S8ZSIGP3(LFT  ,LLT       ,GBUF%SIG,E0  ,GBUF%PLA,
     2               FAC  ,GBUF%G_PLA,NEL     )
       DO I=LFT,LLT
        NU1(I)=NU(I)+(HALF-NU(I))*FAC(I)
       ENDDO
      ELSEIF (JHBE==17.AND.ICPG==1.AND.ISMSTR==10) THEN
       DO I=LFT,LLT
        FAC(I)=TWO*NU(I)
        NU1(I)=NU(I)
       ENDDO
      ELSE
       DO I=LFT,LLT
        NU1(I)=NU(I)
       ENDDO
      ENDIF 
      IF (MTN>=28) THEN
       IADBUF = IPM(7,MXT(1))
      ELSE
       IADBUF = 0
      ENDIF
      NPPT = NPTS*NPTT*NPTR*NLAY
      CALL MMATS(LFT    ,LLT     ,PM    ,MXT    ,HH    ,
     .           MTN    ,IKORTH  ,IPM   ,IGEO   ,GAMA  ,
     .           BUFMAT(IADBUF)  ,DM    ,DGM    ,GM    ,
     .           JHBE  ,GBUF%SIG ,BID   ,NPPT   ,NEL   )
      CALL MMAT_H1(HH    ,HH1   ,FAC  ,ICPG  ,IPREDU)

C-----------Matrix B----------
C-----------Begin integrating points-----
      IL = 1
      DO IR=1,NPTR
       DO IS=1,NPTS
        DO IT=1,NPTT
         IF (JHBE == 14.OR.JHBE == 12.OR.JHBE == 17) THEN
           LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)
         ELSE
           LBUF => ELBUF_STR%BUFLY(IL)%LBUF(1,1,1)
         ENDIF
C-----------
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
         WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*W_GAUSS(IT,NPTT)
C
         CALL S8ZDERII3(OFFG,OFF,VOLN,NGL,
     .    A_GAUSS(IR,NPTR),A_GAUSS(IS,NPTS),A_GAUSS(IT,NPTT),WI,
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4,
     .    PX1H1, PX1H2, PX1H3, PX1H4,
     .    PX2H1, PX2H2, PX2H3, PX2H4,
     .    PX3H1, PX3H2, PX3H3, PX3H4,
     .    PX4H1, PX4H2, PX4H3, PX4H4,
     .    HX,   HY,   HZ,   
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .    PXY1, PXY2, PXY3, PXY4, PXY5, PXY6, PXY7, PXY8,
     .    PYX1, PYX2, PYX3, PYX4, PYX5, PYX6, PYX7, PYX8,
     .    PXZ1, PXZ2, PXZ3, PXZ4, PXZ5, PXZ6, PXZ7, PXZ8,
     .    PZX1, PZX2, PZX3, PZX4, PZX5, PZX6, PZX7, PZX8,
     .    PYZ1, PYZ2, PYZ3, PYZ4, PYZ5, PYZ6, PYZ7, PYZ8,
     .    PZY1, PZY2, PZY3, PZY4, PZY5, PZY6, PZY7, PZY8,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    AJC1,AJC2,AJC3,
     .    AJC4,AJC5,AJC6,
     .    AJC7,AJC8,AJC9,
     .    AJ1,AJ2,AJ3,
     .    AJ4,AJ5,AJ6,
     .    SMAX,DELTAX,NU1,ICPG,IDEG)
       CALL MMSTIFS(PM   ,MXT  ,HH1   ,VOLN  ,ICSIG ,
     .              DD   ,GG   ,DG    ,G33   ,DM    ,
     .              GM   ,DGM  ,IKORTH,LBUF%SIG,IR   ,
     .              IS   ,IT   ,NEL   )
C----------- check MPT sont memes entre KE et forc3----     
       CALL S8ZLKE3(
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   PX5, PX6, PX7, PX8,
     .   PY5, PY6, PY7, PY8,
     .   PZ5, PZ6, PZ7, PZ8,
     .   PXY1, PXY2, PXY3, PXY4, PXY5, PXY6, PXY7, PXY8,
     .   PYX1, PYX2, PYX3, PYX4, PYX5, PYX6, PYX7, PYX8,
     .   PXZ1, PXZ2, PXZ3, PXZ4, PXZ5, PXZ6, PXZ7, PXZ8,
     .   PZX1, PZX2, PZX3, PZX4, PZX5, PZX6, PZX7, PZX8,
     .   PYZ1, PYZ2, PYZ3, PYZ4, PYZ5, PYZ6, PYZ7, PYZ8,
     .   PZY1, PZY2, PZY3, PZY4, PZY5, PZY6, PZY7, PZY8,
     .   BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .   BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .   BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .   BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .   BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .   BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .   K11,K12,K13,K14,K15,K16,K17,K18,K22,K23,
     .   K24,K25,K26,K27,K28,K33,K34,K35,K36,K37,
     .   K38,K44,K45,K46,K47,K48,K55,K56,K57,K58,
     .   K66,K67,K68,K77,K78,K88,
     .   DD ,GG,ICPG,OFFG,IKORTH,DG ,G33)
      IF (IKGEO<0.AND.JHBE==14) THEN
       CALL S8ZKGEI3(LBUF%SIG, VOLN,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   PX5, PX6, PX7, PX8,
     .   PY5, PY6, PY7, PY8,
     .   PZ5, PZ6, PZ7, PZ8,
     .   K11,K12,K13,K14,K15,K16,K17,K18,K22,K23,
     .   K24,K25,K26,K27,K28,K33,K34,K35,K36,K37,
     .   K38,K44,K45,K46,K47,K48,K55,K56,K57,K58,
     .   K66,K67,K68,K77,K78,K88,NEL)
      ENDIF 
        ENDDO 
       ENDDO 
      ENDDO
C-------------      
      IF (IPREDU > 0) THEN
       CALL S8ZLKEP3(
     .    PXC1, PXC2, PXC3, PXC4,
     .    PYC1, PYC2, PYC3, PYC4,
     .    PZC1, PZC2, PZC3, PZC4,
     .    K11,K12,K13,K14,K15,K16,K17,K18,K22,K23,
     .    K24,K25,K26,K27,K28,K33,K34,K35,K36,K37,
     .    K38,K44,K45,K46,K47,K48,K55,K56,K57,K58,
     .    K66,K67,K68,K77,K78,K88,
     .    HH ,VOLG,FAC  ,ICPG)
      END IF 
C------due to the generic form, only the mean geometrical stiffness is considered
      IF (IKGEO>0) THEN
       CALL S8ZKGEO3(GBUF%SIG,VOLG ,
     .   PXC1, PXC2, PXC3, PXC4,
     .   PYC1, PYC2, PYC3, PYC4,
     .   PZC1, PZC2, PZC3, PZC4,
     .   K11,K12,K13,K14,K15,K16,K17,K18,K22,K23,
     .   K24,K25,K26,K27,K28,K33,K34,K35,K36,K37,
     .   K38,K44,K45,K46,K47,K48,K55,K56,K57,K58,
     .   K66,K67,K68,K77,K78,K88,NEL)
      ENDIF 
C----------------------------
C     CONVECTE --> GLOBAL.
C----------------------------
      CALL S8ZCUMG3(
     .   R11, R21, R31, R12, R22, R32, R13, R23, R33,
     .   K11,K12,K13,K14,K15,K16,K17,K18,K22,K23,
     .   K24,K25,K26,K27,K28,K33,K34,K35,K36,K37,
     .   K38,K44,K45,K46,K47,K48,K55,K56,K57,K58,
     .   K66,K67,K68,K77,K78,K88)
     
      IF (NEIG>0) CALL S8EOFF(
     1   LFT, LLT, IXS(1,NF1), ETAG, OFFG)
      CALL ASSEM_S8(
     1    IXS(1,NF1),NEL   ,IDDL  ,NDOF  ,K_DIAG,
     2    K_LT  ,IADK  ,JDIK  ,K11   ,K12   ,     
     3    K13   ,K14   ,K15   ,K16   ,K17   ,
     4    K18   ,K22   ,K23   ,K24   ,K25   ,
     5    K26   ,K27   ,K28   ,K33   ,K34   ,
     6    K35   ,K36   ,K37   ,K38   ,K44   ,
     7    K45   ,K46   ,K47   ,K48   ,K55   ,
     8    K56   ,K57   ,K58   ,K66   ,K67   ,
     9    K68   ,K77   ,K78   ,K88   ,OFFG   )     
C
      RETURN
      END
Chd|====================================================================
Chd|  S8ZKGEO3                      source/elements/solid/solide8z/s8zke3.F
Chd|-- called by -----------
Chd|        S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|        S8ZKEH3                       source/elements/solid/solide8z/s8zke3.F
Chd|====================================================================
      SUBROUTINE S8ZKGEO3(SIG , VOL,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   K11,K12,K13,K14,K15,K16,K17,K18,K22,K23,
     .   K24,K25,K26,K27,K28,K33,K34,K35,K36,K37,
     .   K38,K44,K45,K46,K47,K48,K55,K56,K57,K58,
     .   K66,K67,K68,K77,K78,K88,NEL)
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
C     REAL
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   K11(3,3,*),K12(3,3,*),K13(3,3,*),K14(3,3,*) ,K15(3,3,*)   ,
     .   K16(3,3,*),K17(3,3,*),K18(3,3,*),K22(3,3,*) ,K23(3,3,*)   ,
     .   K24(3,3,*),K25(3,3,*),K26(3,3,*),K27(3,3,*) ,K28(3,3,*)   ,
     .   K33(3,3,*),K34(3,3,*),K35(3,3,*),K36(3,3,*) ,K37(3,3,*)   ,
     .   K38(3,3,*),K44(3,3,*),K45(3,3,*),K46(3,3,*) ,K47(3,3,*)   ,
     .   K48(3,3,*),K55(3,3,*),K56(3,3,*),K57(3,3,*) ,K58(3,3,*)   ,
     .   K66(3,3,*),K67(3,3,*),K68(3,3,*),K77(3,3,*) ,K78(3,3,*)   ,
     .   K88(3,3,*),VOL(*),SIG(NEL,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C     REAL
      my_real
     .   FXX(MVSIZ),FYY(MVSIZ),FZZ(MVSIZ),FXY(MVSIZ),FXZ(MVSIZ),
     .   FYZ(MVSIZ),HIJ(MVSIZ)
C-----------------------------------------------
       DO I=LFT,LLT
        FXX(I)=SIG(I,1)*VOL(I)
        FYY(I)=SIG(I,2)*VOL(I)
        FZZ(I)=SIG(I,3)*VOL(I)
        FXY(I)=SIG(I,4)*VOL(I)
        FYZ(I)=SIG(I,5)*VOL(I)
        FXZ(I)=SIG(I,6)*VOL(I)
       ENDDO
C------------K11,K17,K77
       CALL S8ZKEH3(LFT,LLT,
     .   PX1, PY1 ,PZ1 ,PX1, PY1, PZ1, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       DO J = 1,3
        DO I=LFT,LLT
         K11(J,J,I)=K11(J,J,I)+HIJ(I)
         K77(J,J,I)=K77(J,J,I)+HIJ(I)
         K17(J,J,I)=K17(J,J,I)-HIJ(I)
        ENDDO
       ENDDO
C------------K12,K18,K27,K78
       CALL S8ZKEH3(LFT,LLT,
     .   PX1, PY1 ,PZ1 ,PX2, PY2, PZ2, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       DO J = 1,3
        DO I=LFT,LLT
         K12(J,J,I)=K12(J,J,I)+HIJ(I)
         K18(J,J,I)=K18(J,J,I)-HIJ(I)
         K27(J,J,I)=K27(J,J,I)-HIJ(I)
         K78(J,J,I)=K78(J,J,I)+HIJ(I)
        ENDDO
       ENDDO
C------------K13,K15,K37,K57
       CALL S8ZKEH3(LFT,LLT,
     .   PX1, PY1 ,PZ1 ,PX3, PY3, PZ3, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       DO J = 1,3
        DO I=LFT,LLT
         K13(J,J,I)=K13(J,J,I)+HIJ(I)
         K15(J,J,I)=K15(J,J,I)-HIJ(I)
         K37(J,J,I)=K37(J,J,I)-HIJ(I)
         K57(J,J,I)=K57(J,J,I)+HIJ(I)
        ENDDO
       ENDDO
C------------K14,K16,K47,K67
       CALL S8ZKEH3(LFT,LLT,
     .   PX1, PY1 ,PZ1 ,PX4, PY4, PZ4, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       DO J = 1,3
        DO I=LFT,LLT
         K14(J,J,I)=K14(J,J,I)+HIJ(I)
         K16(J,J,I)=K16(J,J,I)-HIJ(I)
         K47(J,J,I)=K47(J,J,I)-HIJ(I)
         K67(J,J,I)=K67(J,J,I)+HIJ(I)
        ENDDO
       ENDDO
C------------K22,K28,K88
       CALL S8ZKEH3(LFT,LLT,
     .   PX2, PY2 ,PZ2 ,PX2, PY2, PZ2, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       DO J = 1,3
        DO I=LFT,LLT
         K22(J,J,I)=K22(J,J,I)+HIJ(I)
         K88(J,J,I)=K88(J,J,I)+HIJ(I)
         K28(J,J,I)=K28(J,J,I)-HIJ(I)
        ENDDO
       ENDDO
C------------K23,K25,K38,K58
       CALL S8ZKEH3(LFT,LLT,
     .   PX2, PY2 ,PZ2 ,PX3, PY3, PZ3, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       DO J = 1,3
        DO I=LFT,LLT
         K23(J,J,I)=K23(J,J,I)+HIJ(I)
         K25(J,J,I)=K25(J,J,I)-HIJ(I)
         K38(J,J,I)=K38(J,J,I)-HIJ(I)
         K58(J,J,I)=K58(J,J,I)+HIJ(I)
        ENDDO
       ENDDO
C------------K24,K26,K48,K68
       CALL S8ZKEH3(LFT,LLT,
     .   PX2, PY2 ,PZ2 ,PX4, PY4, PZ4, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       DO J = 1,3
        DO I=LFT,LLT
         K24(J,J,I)=K24(J,J,I)+HIJ(I)
         K26(J,J,I)=K26(J,J,I)-HIJ(I)
         K48(J,J,I)=K48(J,J,I)-HIJ(I)
         K68(J,J,I)=K68(J,J,I)+HIJ(I)
        ENDDO
       ENDDO
C------------K33,K35,K55
       CALL S8ZKEH3(LFT,LLT,
     .   PX3, PY3 ,PZ3 ,PX3, PY3, PZ3, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       DO J = 1,3
        DO I=LFT,LLT
         K33(J,J,I)=K33(J,J,I)+HIJ(I)
         K55(J,J,I)=K55(J,J,I)+HIJ(I)
         K35(J,J,I)=K35(J,J,I)-HIJ(I)
        ENDDO
       ENDDO
C------------K34,K36,K56,K45
       CALL S8ZKEH3(LFT,LLT,
     .   PX3, PY3 ,PZ3 ,PX4, PY4, PZ4, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       DO J = 1,3
        DO I=LFT,LLT
         K34(J,J,I)=K34(J,J,I)+HIJ(I)
         K36(J,J,I)=K36(J,J,I)-HIJ(I)
         K56(J,J,I)=K56(J,J,I)+HIJ(I)
         K45(J,J,I)=K45(J,J,I)-HIJ(I)
        ENDDO
       ENDDO
C------------K44,K46,K66
       CALL S8ZKEH3(LFT,LLT,
     .   PX4, PY4 ,PZ4 ,PX4, PY4, PZ4, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       DO J = 1,3
        DO I=LFT,LLT
         K44(J,J,I)=K44(J,J,I)+HIJ(I)
         K66(J,J,I)=K66(J,J,I)+HIJ(I)
         K46(J,J,I)=K46(J,J,I)-HIJ(I)
        ENDDO
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  S8ZKEH3                       source/elements/solid/solide8z/s8zke3.F
Chd|-- called by -----------
Chd|        S4KEH3                        source/elements/solid/solide4/s4ke3.F
Chd|        S8ZKGEO3                      source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZKEH3(LFT,LLT,
     .   PXI, PYI ,PZI ,PXJ, PYJ, PZJ, HIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT
C     REAL
      my_real
     .   PXI(*), PXJ(*),PYI(*), PYJ(*), PZI(*), PZJ(*),  
     .   FXX(*),FYY(*),FZZ(*),FXY(*),FXZ(*),FYZ(*),HIJ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
C-----------------------------------------------
      DO I=LFT,LLT
        HIJ(I)=FXX(I)*PXI(I)*PXJ(I)+FYY(I)*PYI(I)*PYJ(I)+
     .         FZZ(I)*PZI(I)*PZJ(I)+
     .         FXY(I)*(PXI(I)*PYJ(I)+PYI(I)*PXJ(I))+
     .         FYZ(I)*(PYI(I)*PZJ(I)+PZI(I)*PYJ(I))+
     .         FXZ(I)*(PXI(I)*PZJ(I)+PZI(I)*PXJ(I))
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  S8ZKGEI3                      source/elements/solid/solide8z/s8zke3.F
Chd|-- called by -----------
Chd|        S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|        S8ZKEHIJ                      source/elements/solid/solide8z/s8zke3.F
Chd|====================================================================
      SUBROUTINE S8ZKGEI3(SIG , VOL,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   PX5, PX6, PX7, PX8,
     .   PY5, PY6, PY7, PY8,
     .   PZ5, PZ6, PZ7, PZ8,
     .   K11,K12,K13,K14,K15,K16,K17,K18,K22,K23,
     .   K24,K25,K26,K27,K28,K33,K34,K35,K36,K37,
     .   K38,K44,K45,K46,K47,K48,K55,K56,K57,K58,
     .   K66,K67,K68,K77,K78,K88,NEL)
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
C     REAL
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   PX5(*), PX6(*), PX7(*), PX8(*),  
     .   PY5(*), PY6(*), PY7(*), PY8(*),  
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),  
     .   K11(3,3,*),K12(3,3,*),K13(3,3,*),K14(3,3,*) ,K15(3,3,*)   ,
     .   K16(3,3,*),K17(3,3,*),K18(3,3,*),K22(3,3,*) ,K23(3,3,*)   ,
     .   K24(3,3,*),K25(3,3,*),K26(3,3,*),K27(3,3,*) ,K28(3,3,*)   ,
     .   K33(3,3,*),K34(3,3,*),K35(3,3,*),K36(3,3,*) ,K37(3,3,*)   ,
     .   K38(3,3,*),K44(3,3,*),K45(3,3,*),K46(3,3,*) ,K47(3,3,*)   ,
     .   K48(3,3,*),K55(3,3,*),K56(3,3,*),K57(3,3,*) ,K58(3,3,*)   ,
     .   K66(3,3,*),K67(3,3,*),K68(3,3,*),K77(3,3,*) ,K78(3,3,*)   ,
     .   K88(3,3,*),VOL(*),SIG(NEL,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C     REAL
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
C------------K11,K17,K77
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX1, PY1 ,PZ1 ,PX1, PY1, PZ1, K11 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX1, PY1 ,PZ1 ,PX7, PY7, PZ7, K17 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX7, PY7, PZ7 ,PX7, PY7, PZ7, K77 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
C------------K12,K18,K27,K78
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX1, PY1 ,PZ1 ,PX2, PY2, PZ2, K12 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX1, PY1 ,PZ1 ,PX8, PY8, PZ8, K18 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX2, PY2 ,PZ2 ,PX7, PY7, PZ7, K27 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX7, PY7 ,PZ7 ,PX8, PY8, PZ8, K78 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
C------------K13,K15,K37,K57
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX1, PY1 ,PZ1 ,PX3, PY3, PZ3, K13 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX1, PY1 ,PZ1 ,PX5, PY5, PZ5, K15 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX3, PY3 ,PZ3 ,PX7, PY7, PZ7, K37 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX5, PY5 ,PZ5 ,PX7, PY7, PZ7, K57 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
C------------K14,K16,K47,K67
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX1, PY1 ,PZ1 ,PX4, PY4, PZ4, K14 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX1, PY1 ,PZ1 ,PX6, PY6, PZ6, K16 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX4, PY4 ,PZ4 ,PX7, PY7, PZ7, K47 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX6, PY6 ,PZ6 ,PX7, PY7, PZ7, K67 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
C------------K22,K28,K88
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX2, PY2 ,PZ2 ,PX2, PY2, PZ2, K22 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX2, PY2 ,PZ2 ,PX8, PY8, PZ8, K28 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX8, PY8 ,PZ8 ,PX8, PY8, PZ8, K88 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
C------------K23,K25,K38,K58
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX2, PY2 ,PZ2 ,PX3, PY3, PZ3, K23 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX2, PY2 ,PZ2 ,PX5, PY5, PZ5, K25 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX3, PY3 ,PZ3 ,PX8, PY8, PZ8, K38 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX5, PY5 ,PZ5 ,PX8, PY8, PZ8, K58 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
C------------K24,K26,K48,K68
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX2, PY2 ,PZ2 ,PX4, PY4, PZ4, K24 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX2, PY2 ,PZ2 ,PX6, PY6, PZ6, K26 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX4, PY4 ,PZ4 ,PX8, PY8, PZ8, K48 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX6, PY6 ,PZ6 ,PX8, PY8, PZ8, K68 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
C------------K33,K35,K55
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX3, PY3 ,PZ3 ,PX3, PY3, PZ3, K33 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX3, PY3 ,PZ3 ,PX5, PY5, PZ5, K35 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX5, PY5 ,PZ5 ,PX5, PY5, PZ5, K55 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
C------------K34,K36,K56,K45
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX3, PY3 ,PZ3 ,PX4, PY4, PZ4, K34 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX3, PY3 ,PZ3 ,PX6, PY6, PZ6, K36 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX5, PY5 ,PZ5 ,PX6, PY6, PZ6, K56 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX4, PY4 ,PZ4 ,PX5, PY5, PZ5, K45 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
C------------K44,K46,K66
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX4, PY4 ,PZ4 ,PX4, PY4, PZ4, K44 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX4, PY4 ,PZ4 ,PX6, PY6, PZ6, K46 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
       CALL S8ZKEHIJ(LFT,LLT,
     .   PX6, PY6 ,PZ6 ,PX6, PY6, PZ6, K66 ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
C
      RETURN
      END
Chd|====================================================================
Chd|  S8ZKEHIJ                      source/elements/solid/solide8z/s8zke3.F
Chd|-- called by -----------
Chd|        S8ZKGEI3                      source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZKEHIJ(LFT,LLT,
     .   PXI, PYI ,PZI ,PXJ, PYJ, PZJ, KIJ ,
     .   FXX, FYY ,FZZ ,FXY, FYZ, FXZ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT
C     REAL
      my_real
     .   PXI(*), PXJ(*),PYI(*), PYJ(*), PZI(*), PZJ(*),  
     .   FXX(*),FYY(*),FZZ(*),FXY(*),FXZ(*),FYZ(*),KIJ(3,3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C     REAL
      my_real
     .   HIJ(MVSIZ)
C-----------------------------------------------
      DO I=LFT,LLT
        HIJ(I)=FXX(I)*PXI(I)*PXJ(I)+FYY(I)*PYI(I)*PYJ(I)+
     .         FZZ(I)*PZI(I)*PZJ(I)+
     .         FXY(I)*(PXI(I)*PYJ(I)+PYI(I)*PXJ(I))+
     .         FYZ(I)*(PYI(I)*PZJ(I)+PZI(I)*PYJ(I))+
     .         FXZ(I)*(PXI(I)*PZJ(I)+PZI(I)*PXJ(I))
      ENDDO
       DO J = 1,3
        DO I=LFT,LLT
         KIJ(J,J,I)=KIJ(J,J,I)+HIJ(I)
        ENDDO
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  MMAT_H1                       source/elements/solid/solide8z/s8zke3.F
Chd|-- called by -----------
Chd|        Q4KE2                         source/elements/solid_2d/quad4/q4ke2.F
Chd|        S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MMAT_H1(HH    ,HH1   ,FAC  ,ICP   ,IPREDU )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICP,IPREDU
C     REAL
      my_real
     .   HH(2,*),HH1(2,*),FAC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IFP
C     REAL
C-----------------------------------------------
       IPREDU=0
       IF (ICP==1.OR.(ICP==2.AND.JHBE==17)) IPREDU=1
C-----exclude nonlinear pressure laws (don't use p=c1*rho/(rho0-1)--hyperelastic first       
       IF (MTN==42.OR.MTN==62.OR.MTN==82) IPREDU=0
       IFP = 0
       IF (ICP==1.AND.JHBE==17.AND.ISMSTR==10) IFP=1
       IF (IFP==1) IPREDU=1
C       
        IF (IPREDU == 0) THEN
          DO I=LFT,LLT
           HH1(1,I)= HH(1,I)
           HH1(2,I) =HH(2,I)
          ENDDO
        ELSEIF (ICP==2.AND.JHBE==17.OR.IFP==1) THEN
C-----------constant pressure-----  
         DO I=LFT,LLT
          HH1(1,I)=HH(2,I)*(ONE-FAC(I))-TWO_THIRD*HH(2,I)*FAC(I)
          HH1(2,I) = HH(2,I)
         ENDDO
        ELSEIF (ICP == 1) THEN
C-----------constant pressure-----  
         DO I=LFT,LLT
          HH1(1,I)=-TWO_THIRD*HH(2,I)
          HH1(2,I) = HH(2,I)
         ENDDO
        END IF !(IPREDU == 0) THEN
C
      RETURN
      END
Chd|====================================================================
Chd|  S8ZLKEP3                      source/elements/solid/solide8z/s8zke3.F
Chd|-- called by -----------
Chd|        S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|        S8ZKE3                        source/elements/solid/solide8z/s8zke3.F
Chd|-- calls ---------------
Chd|        S8ZKED3                       source/elements/solid/solide8z/s8zlke3.F
Chd|====================================================================
      SUBROUTINE S8ZLKEP3(
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   K11,K12,K13,K14,K15,K16,K17,K18,K22,K23,
     .   K24,K25,K26,K27,K28,K33,K34,K35,K36,K37,
     .   K38,K44,K45,K46,K47,K48,K55,K56,K57,K58,
     .   K66,K67,K68,K77,K78,K88,
     .   HH ,VOL,FAC,ICP)
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
      INTEGER ICP
C     REAL
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   K11(9,*)   ,K12(9,*)   ,K13(9,*)    ,K14(9,*)    ,K15(9,*)   ,
     .   K16(9,*)   ,K17(9,*)   ,K18(9,*)    ,K22(9,*)    ,K23(9,*)   ,
     .   K24(9,*)   ,K25(9,*)   ,K26(9,*)    ,K27(9,*)    ,K28(9,*)   ,
     .   K33(9,*)   ,K34(9,*)   ,K35(9,*)    ,K36(9,*)    ,K37(9,*)   ,
     .   K38(9,*)   ,K44(9,*)   ,K45(9,*)    ,K46(9,*)    ,K47(9,*)   ,
     .   K48(9,*)   ,K55(9,*)   ,K56(9,*)    ,K57(9,*)    ,K58(9,*)   ,
     .   K66(9,*)   ,K67(9,*)   ,K68(9,*)    ,K77(9,*)    ,K78(9,*)   ,
     .   K88(9,*)   ,HH(2,*)    ,VOL(*)      ,FAC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,IS
C     REAL
      my_real
     .   TT,TV,DD(3,3,MVSIZ),
     .   PX5(MVSIZ), PX6(MVSIZ), PX7(MVSIZ), PX8(MVSIZ),  
     .   PY5(MVSIZ), PY6(MVSIZ), PY7(MVSIZ), PY8(MVSIZ),  
     .   PZ5(MVSIZ), PZ6(MVSIZ), PZ7(MVSIZ), PZ8(MVSIZ)  
C-----------------------------------------------
       IF (ICP==1) THEN
        DO I=LFT,LLT
         TV=(HH(1,I)+TWO_THIRD*HH(2,I))*VOL(I)
         TT=TV
         DD(1,1,I)=TT
         DD(2,2,I)=TT
         DD(3,3,I)=TT
         DD(1,2,I)=TV
         DD(1,3,I)=TV
         DD(2,1,I)=TV
         DD(2,3,I)=TV
         DD(3,1,I)=TV
         DD(3,2,I)=TV
        ENDDO
       ELSE
        DO I=LFT,LLT
         TV=(HH(1,I)*FAC(I)+TWO_THIRD*HH(2,I)*(ONE-FAC(I)))*VOL(I)
         TT=TV
         DD(1,1,I)=TT
         DD(2,2,I)=TT
         DD(3,3,I)=TT
         DD(1,2,I)=TV
         DD(1,3,I)=TV
         DD(2,1,I)=TV
         DD(2,3,I)=TV
         DD(3,1,I)=TV
         DD(3,2,I)=TV
        ENDDO
       END IF !(ICP==1) THEN
C       
       DO I=LFT,LLT
         PX5(I)=-PX3(I)
         PY5(I)=-PY3(I)
         PZ5(I)=-PZ3(I)
         PX6(I)=-PX4(I)
         PY6(I)=-PY4(I)
         PZ6(I)=-PZ4(I)
         PX7(I)=-PX1(I)
         PY7(I)=-PY1(I)
         PZ7(I)=-PZ1(I)
         PX8(I)=-PX2(I)
         PY8(I)=-PY2(I)
         PZ8(I)=-PZ2(I)
       ENDDO
C  -------symm(diag)------
      IS =1
C      
       CALL S8ZKED3(LFT,LLT,PX1, PY1 ,PZ1 ,PX1, PY1, PZ1, DD, K11,IS)
       CALL S8ZKED3(LFT,LLT,PX2, PY2 ,PZ2 ,PX2, PY2, PZ2, DD, K22,IS)
       CALL S8ZKED3(LFT,LLT,PX3, PY3 ,PZ3 ,PX3, PY3, PZ3, DD, K33,IS)
       CALL S8ZKED3(LFT,LLT,PX4, PY4 ,PZ4 ,PX4, PY4, PZ4, DD, K44,IS)
       CALL S8ZKED3(LFT,LLT,PX5, PY5 ,PZ5 ,PX5, PY5, PZ5, DD, K55,IS)
       CALL S8ZKED3(LFT,LLT,PX6, PY6 ,PZ6 ,PX6, PY6, PZ6, DD, K66,IS)
       CALL S8ZKED3(LFT,LLT,PX7, PY7 ,PZ7 ,PX7, PY7, PZ7, DD, K77,IS)
       CALL S8ZKED3(LFT,LLT,PX8, PY8 ,PZ8 ,PX8, PY8, PZ8, DD, K88,IS)
C  -------non symm------
      IS =0
C      
       CALL S8ZKED3(LFT,LLT,PX1, PY1 ,PZ1 ,PX2, PY2, PZ2, DD, K12,IS)
       CALL S8ZKED3(LFT,LLT,PX1, PY1 ,PZ1 ,PX3, PY3, PZ3, DD, K13,IS)
       CALL S8ZKED3(LFT,LLT,PX1, PY1 ,PZ1 ,PX4, PY4, PZ4, DD, K14,IS)
       CALL S8ZKED3(LFT,LLT,PX1, PY1 ,PZ1 ,PX5, PY5, PZ5, DD, K15,IS)
       CALL S8ZKED3(LFT,LLT,PX1, PY1 ,PZ1 ,PX6, PY6, PZ6, DD, K16,IS)
       CALL S8ZKED3(LFT,LLT,PX1, PY1 ,PZ1 ,PX7, PY7, PZ7, DD, K17,IS)
       CALL S8ZKED3(LFT,LLT,PX1, PY1 ,PZ1 ,PX8, PY8, PZ8, DD, K18,IS)
       CALL S8ZKED3(LFT,LLT,PX2, PY2 ,PZ2 ,PX3, PY3, PZ3, DD, K23,IS)
       CALL S8ZKED3(LFT,LLT,PX2, PY2 ,PZ2 ,PX4, PY4, PZ4, DD, K24,IS)
       CALL S8ZKED3(LFT,LLT,PX2, PY2 ,PZ2 ,PX5, PY5, PZ5, DD, K25,IS)
       CALL S8ZKED3(LFT,LLT,PX2, PY2 ,PZ2 ,PX6, PY6, PZ6, DD, K26,IS)
       CALL S8ZKED3(LFT,LLT,PX2, PY2 ,PZ2 ,PX7, PY7, PZ7, DD, K27,IS)
       CALL S8ZKED3(LFT,LLT,PX2, PY2 ,PZ2 ,PX8, PY8, PZ8, DD, K28,IS)
       CALL S8ZKED3(LFT,LLT,PX3, PY3 ,PZ3 ,PX4, PY4, PZ4, DD, K34,IS)
       CALL S8ZKED3(LFT,LLT,PX3, PY3 ,PZ3 ,PX5, PY5, PZ5, DD, K35,IS)
       CALL S8ZKED3(LFT,LLT,PX3, PY3 ,PZ3 ,PX6, PY6, PZ6, DD, K36,IS)
       CALL S8ZKED3(LFT,LLT,PX3, PY3 ,PZ3 ,PX7, PY7, PZ7, DD, K37,IS)
       CALL S8ZKED3(LFT,LLT,PX3, PY3 ,PZ3 ,PX8, PY8, PZ8, DD, K38,IS)
       CALL S8ZKED3(LFT,LLT,PX4, PY4 ,PZ4 ,PX5, PY5, PZ5, DD, K45,IS)
       CALL S8ZKED3(LFT,LLT,PX4, PY4 ,PZ4 ,PX6, PY6, PZ6, DD, K46,IS)
       CALL S8ZKED3(LFT,LLT,PX4, PY4 ,PZ4 ,PX7, PY7, PZ7, DD, K47,IS)
       CALL S8ZKED3(LFT,LLT,PX4, PY4 ,PZ4 ,PX8, PY8, PZ8, DD, K48,IS)
       CALL S8ZKED3(LFT,LLT,PX5, PY5 ,PZ5 ,PX6, PY6, PZ6, DD, K56,IS)
       CALL S8ZKED3(LFT,LLT,PX5, PY5 ,PZ5 ,PX7, PY7, PZ7, DD, K57,IS)
       CALL S8ZKED3(LFT,LLT,PX5, PY5 ,PZ5 ,PX8, PY8, PZ8, DD, K58,IS)
       CALL S8ZKED3(LFT,LLT,PX6, PY6 ,PZ6 ,PX7, PY7, PZ7, DD, K67,IS)
       CALL S8ZKED3(LFT,LLT,PX6, PY6 ,PZ6 ,PX8, PY8, PZ8, DD, K68,IS)
       CALL S8ZKED3(LFT,LLT,PX7, PY7 ,PZ7 ,PX8, PY8, PZ8, DD, K78,IS)
C
C   ---------------
      RETURN
      END
