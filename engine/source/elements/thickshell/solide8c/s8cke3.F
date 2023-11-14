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
Chd|  S8CKE3                        source/elements/thickshell/solide8c/s8cke3.F
Chd|-- called by -----------
Chd|        IMP_GLOB_K                    source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|        ASSEM_S8                      source/implicit/assem_s8.F    
Chd|        MMATS                         source/elements/solid/solide8z/mmats.F
Chd|        MMAT_H1                       source/elements/solid/solide8z/mmat_h1.F
Chd|        MMSTIFS                       source/elements/solid/solide8z/mmstifs.F
Chd|        S8EOFF                        source/elements/solid/solide8/s8eoff.F
Chd|        S8ZCUMG3                      source/elements/solid/solide8z/s8zcumg3.F
Chd|        S8ZDERI3                      source/elements/solid/solide8z/s8zderi3.F
Chd|        S8ZDERICT3                    source/elements/solid/solide8z/s8zderict3.F
Chd|        S8ZKGEI3                      source/elements/solid/solide8z/s8zkgei3.F
Chd|        S8ZKGEO3                      source/elements/solid/solide8z/s8zkgeo3.F
Chd|        S8ZLKE3                       source/elements/solid/solide8z/s8zlke3.F
Chd|        S8ZLKEP3                      source/elements/solid/solide8z/s8zlkep3.F
Chd|        S8ZSIGP3                      source/elements/solid/solide8z/s8zsigp3.F
Chd|        SRCOORK                       source/elements/solid/solide8z/srcoork.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE S8CKE3(
     1   PM,       GEO,      IXS,      X,
     2   ELBUF_STR,NEL,      ICP,      ICSIG,
     3   ETAG,     IDDL,     NDOF,     K_DIAG,
     4   K_LT,     IADK,     JDIK,     IPM,
     5   IGEO,     IKGEO,    BUFMAT,   NFT,
     6   MTN,      JHBE,     JCVT,     IGTYP,
     7   ISORTH,   IREP,     ISMSTR)
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
      INTEGER, INTENT(IN) :: IREP
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: NFT
      INTEGER, INTENT(IN) :: MTN
      INTEGER, INTENT(IN) :: JHBE
      INTEGER, INTENT(IN) :: JCVT
      INTEGER, INTENT(IN) :: IGTYP
      INTEGER, INTENT(IN) :: ISORTH
      INTEGER  NEL ,ICP, ICSIG,IKGEO
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
      INTEGER LCO,NF1,IFLAG,NB3S,I,J,IKSUP,IKORTH,IADBUF,IPREDU
      INTEGER IR, IS, IT,IL,IP,ICR,ICS,ICT,ICPG,PID,
     .     NPTR,NPTS,NPTT,NLAY,IAD0,NPPT,MX
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
      DOUBLE PRECISION 
     .   VOLDP(MVSIZ)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      GBUF => ELBUF_STR%GBUF
      NLAY = ELBUF_STR%NLAY
      NPTR = ELBUF_STR%NPTR
      NPTS = ELBUF_STR%NPTS
      NPTT = ELBUF_STR%NPTT
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
      CALL SRCOORK(
     1   X,         IXS(1,NF1),X1,        X2,
     2   X3,        X4,        X5,        X6,
     3   X7,        X8,        Y1,        Y2,
     4   Y3,        Y4,        Y5,        Y6,
     5   Y7,        Y8,        Z1,        Z2,
     6   Z3,        Z4,        Z5,        Z6,
     7   Z7,        Z8,        GBUF%OFF,  OFFG,
     8   GBUF%SMSTR,R11,       R12,       R13,
     9   R21,       R22,       R23,       R31,
     A   R32,       R33,       NC1,       NC2,
     B   NC3,       NC4,       NC5,       NC6,
     C   NC7,       NC8,       NGL,       MXT,
     D   NGEO,      K11,       K12,       K13,
     E   K14,       K15,       K16,       K17,
     F   K18,       K22,       K23,       K24,
     G   K25,       K26,       K27,       K28,
     H   K33,       K34,       K35,       K36,
     I   K37,       K38,       K44,       K45,
     J   K46,       K47,       K48,       K55,
     K   K56,       K57,       K58,       K66,
     L   K67,       K68,       K77,       K78,
     M   K88,       JHBE,      GBUF%GAMA, GAMA,
     N   NEL,       ISMSTR,    JHBE,      JCVT,
     O   IREP,      IGTYP,     ISORTH)
C
      PID=NGEO(1)
C      IGTYP=NINT(GEO(12,PID))
C      CALL GET_GAMA(MTN,NEL,NBGAMA,JCVT,PM,IXS(1,NF1),IGTYP)
      MX = MXT(1)
      DO I=1,NEL
        NU(I)=MIN(HALF,PM(21,MX))
        C1 =PM(32,MX)
        E0(I) =THREE*(ONE-TWO*NU(1))*C1
        DELTAX(I)=EP30
                IDEG(I)=0
      ENDDO
      IF (JHBE/=14.AND.JHBE<20) THEN
        DO I=1,NEL
          NU(I)=ZERO
        ENDDO
      ENDIF 
      ICPG = ICP
      IF (JHBE/=14.AND.JHBE/=17.AND.JHBE/=24) THEN
        ICPG = 1
      ENDIF 
c
      CALL S8ZDERICT3(
     1   OFFG,    VOLG,    NGL,     X1,
     2   X2,      X3,      X4,      X5,
     3   X6,      X7,      X8,      Y1,
     4   Y2,      Y3,      Y4,      Y5,
     5   Y6,      Y7,      Y8,      Z1,
     6   Z2,      Z3,      Z4,      Z5,
     7   Z6,      Z7,      Z8,      PXC1,
     8   PXC2,    PXC3,    PXC4,    PYC1,
     9   PYC2,    PYC3,    PYC4,    PZC1,
     A   PZC2,    PZC3,    PZC4,    PX1H1,
     B   PX1H2,   PX1H3,   PX1H4,   PX2H1,
     C   PX2H2,   PX2H3,   PX2H4,   PX3H1,
     D   PX3H2,   PX3H3,   PX3H4,   PX4H1,
     E   PX4H2,   PX4H3,   PX4H4,   HX,
     F   HY,      HZ,      AJC1,    AJC2,
     G   AJC3,    AJC4,    AJC5,    AJC6,
     H   AJC7,    AJC8,    AJC9,    SMAX,
     I   NEL)
      IF (ICPG==2) THEN
       CALL S8ZSIGP3(1  ,NEL       ,GBUF%SIG,E0  ,GBUF%PLA,
     2               FAC  ,GBUF%G_PLA,NEL     )
       DO I=1,NEL
        NU1(I)=NU(I)+(HALF-NU(I))*FAC(I)
       ENDDO
      ELSE
       DO I=1,NEL
        NU1(I)=NU(I)
       ENDDO
      ENDIF 
      IF (MTN>=28) THEN
       IADBUF = IPM(7,MXT(1))
      ELSE
       IADBUF = 0
      ENDIF
      NPPT = NPTS*NPTT*NPTR*NLAY
      CALL MMATS(1    ,NEL     ,PM    ,MXT    ,HH    ,
     .           MTN    ,IKORTH  ,IPM   ,IGEO   ,GAMA  ,
     .           BUFMAT(IADBUF)  ,DM    ,DGM    ,GM    ,
     .           JHBE  ,GBUF%SIG ,BID   ,NPPT   ,NEL   )
      CALL MMAT_H1(
     1   HH,      HH1,     FAC,     ICPG,
     2   IPREDU,  NEL,     MTN,     ISMSTR,
     3   JHBE)

C-----------Matrix B----------
C-----------Begin integrating points-----
      IT = 1
      DO IR=1,NPTR
       DO IS=1,NPTS
        DO IL=1,NLAY
         LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)
C-----------
         IP = IR + ( (IS-1) + (IL-1)*NPTS )*NPTR
         WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)*W_GAUSS(IL,NPTT)
C
         CALL S8ZDERI3(
     1   OFFG,            OFF,             VOLN,            NGL,
     2   A_GAUSS(IR,NPTR),A_GAUSS(IS,NPTS),A_GAUSS(IL,NPTT),WI,
     3   PXC1,            PXC2,            PXC3,            PXC4,
     4   PYC1,            PYC2,            PYC3,            PYC4,
     5   PZC1,            PZC2,            PZC3,            PZC4,
     6   PX1H1,           PX1H2,           PX1H3,           PX1H4,
     7   PX2H1,           PX2H2,           PX2H3,           PX2H4,
     8   PX3H1,           PX3H2,           PX3H3,           PX3H4,
     9   PX4H1,           PX4H2,           PX4H3,           PX4H4,
     A   HX,              HY,              HZ,              PX1,
     B   PX2,             PX3,             PX4,             PX5,
     C   PX6,             PX7,             PX8,             PY1,
     D   PY2,             PY3,             PY4,             PY5,
     E   PY6,             PY7,             PY8,             PZ1,
     F   PZ2,             PZ3,             PZ4,             PZ5,
     G   PZ6,             PZ7,             PZ8,             PXY1,
     H   PXY2,            PXY3,            PXY4,            PXY5,
     I   PXY6,            PXY7,            PXY8,            PYX1,
     J   PYX2,            PYX3,            PYX4,            PYX5,
     K   PYX6,            PYX7,            PYX8,            PXZ1,
     L   PXZ2,            PXZ3,            PXZ4,            PXZ5,
     M   PXZ6,            PXZ7,            PXZ8,            PZX1,
     N   PZX2,            PZX3,            PZX4,            PZX5,
     O   PZX6,            PZX7,            PZX8,            PYZ1,
     P   PYZ2,            PYZ3,            PYZ4,            PYZ5,
     Q   PYZ6,            PYZ7,            PYZ8,            PZY1,
     R   PZY2,            PZY3,            PZY4,            PZY5,
     S   PZY6,            PZY7,            PZY8,            BXY1,
     T   BXY2,            BXY3,            BXY4,            BXY5,
     U   BXY6,            BXY7,            BXY8,            BYX1,
     V   BYX2,            BYX3,            BYX4,            BYX5,
     W   BYX6,            BYX7,            BYX8,            BXZ1,
     X   BXZ2,            BXZ3,            BXZ4,            BXZ5,
     Y   BXZ6,            BXZ7,            BXZ8,            BZX1,
     Z   BZX2,            BZX3,            BZX4,            BZX5,
     1   BZX6,            BZX7,            BZX8,            BYZ1,
     2   BYZ2,            BYZ3,            BYZ4,            BYZ5,
     3   BYZ6,            BYZ7,            BYZ8,            BZY1,
     4   BZY2,            BZY3,            BZY4,            BZY5,
     5   BZY6,            BZY7,            BZY8,            AJC1,
     6   AJC2,            AJC3,            AJC4,            AJC5,
     7   AJC6,            AJC7,            AJC8,            AJC9,
     8   AJ1,             AJ2,             AJ3,             AJ4,
     9   AJ5,             AJ6,             SMAX,            DELTAX,
     A   NU1,             ICPG,            IDEG,            VOLDP,
     B   NEL,             MTN,             ISMSTR,          JHBE)
       CALL MMSTIFS(
     1   PM,      MXT,     HH1,     VOLN,
     2   ICSIG,   DD,      GG,      DG,
     3   G33,     DM,      GM,      DGM,
     4   IKORTH,  LBUF%SIG,IR,      IS,
     5   IL,      NEL,     JHBE,    MTN)
C-----------  check MPT sont memes entre KE et forc3----     
       CALL S8ZLKE3(
     1   PX1,     PX2,     PX3,     PX4,
     2   PY1,     PY2,     PY3,     PY4,
     3   PZ1,     PZ2,     PZ3,     PZ4,
     4   PX5,     PX6,     PX7,     PX8,
     5   PY5,     PY6,     PY7,     PY8,
     6   PZ5,     PZ6,     PZ7,     PZ8,
     7   PXY1,    PXY2,    PXY3,    PXY4,
     8   PXY5,    PXY6,    PXY7,    PXY8,
     9   PYX1,    PYX2,    PYX3,    PYX4,
     A   PYX5,    PYX6,    PYX7,    PYX8,
     B   PXZ1,    PXZ2,    PXZ3,    PXZ4,
     C   PXZ5,    PXZ6,    PXZ7,    PXZ8,
     D   PZX1,    PZX2,    PZX3,    PZX4,
     E   PZX5,    PZX6,    PZX7,    PZX8,
     F   PYZ1,    PYZ2,    PYZ3,    PYZ4,
     G   PYZ5,    PYZ6,    PYZ7,    PYZ8,
     H   PZY1,    PZY2,    PZY3,    PZY4,
     I   PZY5,    PZY6,    PZY7,    PZY8,
     J   BXY1,    BXY2,    BXY3,    BXY4,
     K   BXY5,    BXY6,    BXY7,    BXY8,
     L   BYX1,    BYX2,    BYX3,    BYX4,
     M   BYX5,    BYX6,    BYX7,    BYX8,
     N   BXZ1,    BXZ2,    BXZ3,    BXZ4,
     O   BXZ5,    BXZ6,    BXZ7,    BXZ8,
     P   BZX1,    BZX2,    BZX3,    BZX4,
     Q   BZX5,    BZX6,    BZX7,    BZX8,
     R   BYZ1,    BYZ2,    BYZ3,    BYZ4,
     S   BYZ5,    BYZ6,    BYZ7,    BYZ8,
     T   BZY1,    BZY2,    BZY3,    BZY4,
     U   BZY5,    BZY6,    BZY7,    BZY8,
     V   K11,     K12,     K13,     K14,
     W   K15,     K16,     K17,     K18,
     X   K22,     K23,     K24,     K25,
     Y   K26,     K27,     K28,     K33,
     Z   K34,     K35,     K36,     K37,
     1   K38,     K44,     K45,     K46,
     2   K47,     K48,     K55,     K56,
     3   K57,     K58,     K66,     K67,
     4   K68,     K77,     K78,     K88,
     5   DD,      GG,      ICPG,    OFFG,
     6   IKORTH,  DG,      G33,     NEL)
      IF (IKGEO<0.AND.JHBE==14) THEN
       CALL S8ZKGEI3(
     1   LBUF%SIG,VOLN,    PX1,     PX2,
     2   PX3,     PX4,     PY1,     PY2,
     3   PY3,     PY4,     PZ1,     PZ2,
     4   PZ3,     PZ4,     PX5,     PX6,
     5   PX7,     PX8,     PY5,     PY6,
     6   PY7,     PY8,     PZ5,     PZ6,
     7   PZ7,     PZ8,     K11,     K12,
     8   K13,     K14,     K15,     K16,
     9   K17,     K18,     K22,     K23,
     A   K24,     K25,     K26,     K27,
     B   K28,     K33,     K34,     K35,
     C   K36,     K37,     K38,     K44,
     D   K45,     K46,     K47,     K48,
     E   K55,     K56,     K57,     K58,
     F   K66,     K67,     K68,     K77,
     G   K78,     K88,     NEL)
      ENDIF 
        ENDDO 
       ENDDO 
      ENDDO
C-------------      
      IF (IPREDU > 0) THEN
       CALL S8ZLKEP3(
     1   PXC1,    PXC2,    PXC3,    PXC4,
     2   PYC1,    PYC2,    PYC3,    PYC4,
     3   PZC1,    PZC2,    PZC3,    PZC4,
     4   K11,     K12,     K13,     K14,
     5   K15,     K16,     K17,     K18,
     6   K22,     K23,     K24,     K25,
     7   K26,     K27,     K28,     K33,
     8   K34,     K35,     K36,     K37,
     9   K38,     K44,     K45,     K46,
     A   K47,     K48,     K55,     K56,
     B   K57,     K58,     K66,     K67,
     C   K68,     K77,     K78,     K88,
     D   HH,      VOLG,    FAC,     ICPG,
     E   NEL)
      END IF 
C------due to the generic form, only the mean geometrical stiffness is considered
      IF (IKGEO>0) THEN
       CALL S8ZKGEO3(
     1   GBUF%SIG,VOLG,    PXC1,    PXC2,
     2   PXC3,    PXC4,    PYC1,    PYC2,
     3   PYC3,    PYC4,    PZC1,    PZC2,
     4   PZC3,    PZC4,    K11,     K12,
     5   K13,     K14,     K15,     K16,
     6   K17,     K18,     K22,     K23,
     7   K24,     K25,     K26,     K27,
     8   K28,     K33,     K34,     K35,
     9   K36,     K37,     K38,     K44,
     A   K45,     K46,     K47,     K48,
     B   K55,     K56,     K57,     K58,
     C   K66,     K67,     K68,     K77,
     D   K78,     K88,     NEL)
      ENDIF 
C----------------------------
C     CONVECTE --> GLOBAL.
C----------------------------
      CALL S8ZCUMG3(
     1   R11,     R21,     R31,     R12,
     2   R22,     R32,     R13,     R23,
     3   R33,     K11,     K12,     K13,
     4   K14,     K15,     K16,     K17,
     5   K18,     K22,     K23,     K24,
     6   K25,     K26,     K27,     K28,
     7   K33,     K34,     K35,     K36,
     8   K37,     K38,     K44,     K45,
     9   K46,     K47,     K48,     K55,
     A   K56,     K57,     K58,     K66,
     B   K67,     K68,     K77,     K78,
     C   K88,     NEL)
     
      IF (NEIG>0) CALL S8EOFF(
     1   1, NEL, IXS(1,NF1), ETAG, OFFG)
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
C-----------
      RETURN
      END
