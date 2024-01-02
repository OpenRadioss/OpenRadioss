Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  S6CKE3                        source/elements/thickshell/solide6c/s6cke3.F
Chd|-- called by -----------
Chd|        IMP_GLOB_K                    source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|        ASSEM_S6                      source/implicit/assem_s6.F    
Chd|        MMATS                         source/elements/solid/solide8z/mmats.F
Chd|        MMSTIFS                       source/elements/solid/solide8z/mmstifs.F
Chd|        S6CCUMG3                      source/elements/thickshell/solide6c/s6ccumg3.F
Chd|        S6CDERI3                      source/elements/thickshell/solide6c/s6cderi3.F
Chd|        S6CKGEO3                      source/elements/thickshell/solide6c/s6ckgeo3.F
Chd|        S6CLKE3                       source/elements/thickshell/solide6c/s6clke3.F
Chd|        S6RCOORK                      source/elements/thickshell/solide6c/s6rcoork.F
Chd|        S8EOFF                        source/elements/solid/solide8/s8eoff.F
Chd|        S8ZSIGP3                      source/elements/solid/solide8z/s8zsigp3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE S6CKE3(
     1   PM,       GEO,      IXS,      X,
     2   ELBUF_STR,ETAG,     IDDL,     NDOF,
     3   K_DIAG,   K_LT,     IADK,     JDIK,
     4   NEL,      ICP,      ICSIG,    IPM,
     5   IGEO,     IKGEO,    BUFMAT,   NFT,
     6   MTN,      JHBE,     ISORTH,   ISORTHG,
     7   ISMSTR)
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
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: NFT
      INTEGER, INTENT(IN) :: MTN
      INTEGER, INTENT(IN) :: JHBE
      INTEGER, INTENT(IN) :: ISORTH
      INTEGER, INTENT(INOUT) :: ISORTHG
      INTEGER  ICP, ICSIG,IKGEO,NEL ,NPG
      INTEGER ETAG(*),IDDL(*)  ,NDOF(*)  ,IADK(*) ,JDIK(*),
     .   IXS(NIXS,*),IPM(NPROPMI,*),IGEO(NPROPGI,*)
C
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*), X(*),
     . K11(9,MVSIZ),K12(9,MVSIZ),K13(9,MVSIZ),K14(9,MVSIZ),K15(9,MVSIZ),
     . K16(9,MVSIZ),K22(9,MVSIZ),K23(9,MVSIZ),K24(9,MVSIZ),K25(9,MVSIZ),
     . K26(9,MVSIZ),K33(9,MVSIZ),K34(9,MVSIZ),K35(9,MVSIZ),K36(9,MVSIZ),
     . K44(9,MVSIZ),K45(9,MVSIZ),K46(9,MVSIZ),K55(9,MVSIZ),K56(9,MVSIZ),
     .   K66(9,MVSIZ)   ,OFFG(MVSIZ)  ,BUFMAT(*),K_DIAG(*)   ,K_LT(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LCO, NF1, IFLAG, NB3S, I,IS,IAD0
      INTEGER IADBUF,IKORTH,IBID,IUN,IP,NLAY
      INTEGER MXT(MVSIZ),NGL(MVSIZ),NGEO(MVSIZ),PID
      my_real
     . VOLN(MVSIZ), DELTAX(MVSIZ),
     . AJ1(MVSIZ) , AJ2(MVSIZ) , AJ3(MVSIZ) ,
     . AJ4(MVSIZ) , AJ5(MVSIZ) , AJ6(MVSIZ) , 
     . AJ7(MVSIZ) , AJ8(MVSIZ) , AJ9(MVSIZ) 
C----------------
      INTEGER NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ), 
     .        NC5(MVSIZ), NC6(MVSIZ), NC7(MVSIZ), NC8(MVSIZ)
      my_real
     .   OFF(MVSIZ) ,BID(1),  
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   X5(MVSIZ), X6(MVSIZ), 
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Y5(MVSIZ), Y6(MVSIZ), 
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   Z5(MVSIZ), Z6(MVSIZ), 
     .  PXC1(MVSIZ),PXC2(MVSIZ),PXC3(MVSIZ),PXC4(MVSIZ),
     .  PYC1(MVSIZ),PYC2(MVSIZ),PYC3(MVSIZ),PYC4(MVSIZ),
     .  PZC1(MVSIZ),PZC2(MVSIZ),PZC3(MVSIZ),PZC4(MVSIZ),
     .  PX1H(MVSIZ),PX2H(MVSIZ),PX3H(MVSIZ),
     .  PY1H(MVSIZ),PY2H(MVSIZ),PY3H(MVSIZ),
     .  PZ1H(MVSIZ),PZ2H(MVSIZ),PZ3H(MVSIZ)
      my_real
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),GAMA(MVSIZ,6)
C                                                                     12
      my_real
     .   VOLG(MVSIZ),
     .   B1122(MVSIZ),B1221(MVSIZ),B2212(MVSIZ),B1121(MVSIZ),
     .   B1122H(MVSIZ),B1221H(MVSIZ),B2212H(MVSIZ),B1121H(MVSIZ),
     .   B1X(MVSIZ,2),B1Y(MVSIZ,2),B2X(MVSIZ,2),B2Y(MVSIZ,2),
     .   B1XH(MVSIZ,2),B1YH(MVSIZ,2),B2XH(MVSIZ,2),B2YH(MVSIZ,2),
     .   VZL(MVSIZ),JI33(MVSIZ)
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
     .  NU(MVSIZ),NU1(MVSIZ),HH(2,MVSIZ),FAC(MVSIZ),C1,E0(MVSIZ),
     .  DD(9,MVSIZ),GG(MVSIZ),DM(9,MVSIZ),GM(9,MVSIZ),DGM(9,MVSIZ),
     .  DG(9,MVSIZ),G33(9,MVSIZ)
C----HH(1,):lamda,HH(2,):G
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      GBUF => ELBUF_STR%GBUF
      NLAY = ELBUF_STR%NLAY
      IAD0 = 1
      IF (ISORTH > 0) IAD0 = 1 + 6*NEL
      ISORTHG = 0
      IKORTH=0
C-----------
      NF1=NFT+1
C-----------
      CALL S6RCOORK(
     1   X,         IXS(1,NF1),X1,        X2,
     2   X3,        X4,        X5,        X6,
     3   Y1,        Y2,        Y3,        Y4,
     4   Y5,        Y6,        Z1,        Z2,
     5   Z3,        Z4,        Z5,        Z6,
     6   GBUF%OFF,  OFFG,      GBUF%SMSTR,R11,
     7   R12,       R13,       R21,       R22,
     8   R23,       R31,       R32,       R33,
     9   NC1,       NC2,       NC3,       NC4,
     A   NC5,       NC6,       NGL,       MXT,
     B   NGEO,      K11,       K12,       K13,
     C   K14,       K15,       K16,       K22,
     D   K23,       K24,       K25,       K26,
     E   K33,       K34,       K35,       K36,
     F   K44,       K45,       K46,       K55,
     G   K56,       K66,       NEL,       ISMSTR)
C
C----------------------
       DO I=1,NEL
        NU(I)=MIN(HALF,PM(21,MXT(I)))
        C1 =PM(32,MXT(I))
        E0(I) =THREE*(ONE-TWO*NU(I))*C1
       ENDDO
C
        IF (ICP==1) THEN
          DO I=1,NEL
           NU1(I)=HALF
          ENDDO
        ELSEIF (ICP==2) THEN
          CALL S8ZSIGP3(1  ,NEL       ,GBUF%SIG,E0,GBUF%PLA,
     2                  FAC  ,GBUF%G_PLA,NEL     )
          DO I=1,NEL
           NU1(I)=NU(I)+(HALF-NU(I))*FAC(I)
          ENDDO
        ELSE
          DO I=1,NEL
           NU1(I) =NU(I)
          ENDDO
        ENDIF
      CALL S6CDERI3(
     1   OFFG,      VOLN,      NGL,       X1,
     2   X2,        X3,        X4,        X5,
     3   X6,        Y1,        Y2,        Y3,
     4   Y4,        Y5,        Y6,        Z1,
     5   Z2,        Z3,        Z4,        Z5,
     6   Z6,        PXC1,      PXC2,      PXC3,
     7   PXC4,      PYC1,      PYC2,      PYC3,
     8   PYC4,      PZC1,      PZC2,      PZC3,
     9   PZC4,      PX1H,      PX2H,      PX3H,
     A   PY1H,      PY2H,      PY3H,      PZ1H,
     B   PZ2H,      PZ3H,      AJ1,       AJ2,
     C   AJ3,       AJ4,       AJ5,       AJ6,
     D   JI33,      B1X,       B1Y,       B2Y,
     E   B2X,       B1122,     B1221,     B2212,
     F   B1121,     B1XH,      B1YH,      B2XH,
     G   B2YH,      B1122H,    B1221H,    B2212H,
     H   B1121H,    VZL,       VOLG,      GBUF%SMSTR,
     I   GBUF%OFF,  NEL,       ISMSTR)
      IF (MTN>=28) THEN
       IADBUF = IPM(7,MXT(1))
      ELSE
       IADBUF = 0
      ENDIF
C
      CALL MMATS(1    ,NEL     ,PM    ,MXT    ,HH    ,
     .           MTN    ,IKORTH  ,IPM   ,IGEO   ,GAMA  ,
     .           BUFMAT(IADBUF)  ,DM    ,DGM    ,GM    ,
     .           JHBE  ,GBUF%SIG ,BID   ,NLAY   ,NEL   )
C-----------Begin integrating points----
      IBID=0
      IUN = 1
      DO IS=1,NLAY
        LBUF => ELBUF_STR%BUFLY(IS)%LBUF(1,1,1)
       DO I=1,NEL
       VOLN(I)=HALF*W_GAUSS(IS,NLAY)*(VOLG(I)+VZL(I)*A_GAUSS(IS,NLAY))
       ENDDO
       CALL MMSTIFS(
     1   PM,      MXT,     HH,      VOLN,
     2   IBID,    DD,      GG,      DG,
     3   G33,     DM,      GM,      DGM,
     4   IKORTH,  LBUF%SIG,IUN,     IUN,
     5   IS,      NEL,     JHBE,    MTN)
       CALL S6CLKE3(
     1   PXC1,            PXC2,            PXC3,            PXC4,
     2   PYC1,            PYC2,            PYC3,            PYC4,
     3   PZC1,            PZC2,            PZC3,            PZC4,
     4   PX1H,            PX2H,            PX3H,            PY1H,
     5   PY2H,            PY3H,            PZ1H,            PZ2H,
     6   PZ3H,            JI33,            B1X,             B1Y,
     7   B2Y,             B2X,             B1122,           B1221,
     8   B2212,           B1121,           B1XH,            B1YH,
     9   B2XH,            B2YH,            B1122H,          B1221H,
     A   B2212H,          B1121H,          DD,              GG,
     B   VOLN,            A_GAUSS(IS,NLAY),W_GAUSS(IS,NLAY),NU1,
     C   K11,             K12,             K13,             K14,
     D   K15,             K16,             K22,             K23,
     E   K24,             K25,             K26,             K33,
     F   K34,             K35,             K36,             K44,
     G   K45,             K46,             K55,             K56,
     H   K66,             NEL)
      ENDDO
C----------------------------
C     geometrical stiffness (membrane only)
C----------------------------
      IF (IKGEO>0) THEN
       CALL S6CKGEO3(
     1   GBUF%SIG,VOLG,    PXC1,    PXC2,
     2   PXC3,    PXC4,    PYC1,    PYC2,
     3   PYC3,    PYC4,    K11,     K12,
     4   K13,     K14,     K15,     K16,
     5   K22,     K23,     K24,     K25,
     6   K26,     K33,     K34,     K35,
     7   K36,     K44,     K45,     K46,
     8   K55,     K56,     K66,     NEL)
      ENDIF 
C----------------------------
C     CONVECTE --> GLOBAL.
C----------------------------
      CALL S6CCUMG3(
     1   R11,     R21,     R31,     R12,
     2   R22,     R32,     R13,     R23,
     3   R33,     K11,     K12,     K13,
     4   K14,     K15,     K16,     K22,
     5   K23,     K24,     K25,     K26,
     6   K33,     K34,     K35,     K36,
     7   K44,     K45,     K46,     K55,
     8   K56,     K66,     X1,      X2,
     9   X3,      X4,      X5,      X6,
     A   Y1,      Y2,      Y3,      Y4,
     B   Y5,      Y6,      Z1,      Z2,
     C   Z3,      Z4,      Z5,      Z6,
     D   NEL)
     
      IF (NEIG>0) CALL S8EOFF(
     1   1, NEL, IXS(1,NF1), ETAG, OFFG)
      CALL ASSEM_S6(
     1                   IXS(1,NF1),NEL   ,IDDL  ,NDOF  ,K_DIAG,
     2                   K_LT  ,IADK  ,JDIK  ,K11   ,K12   ,     
     3                   K13   ,K14   ,K15   ,K16   ,K22   ,
     4                   K23   ,K24   ,K25   ,K26   ,K33   ,
     5                   K34   ,K35   ,K36   ,K44   ,K45   ,
     6                   K46   ,K55   ,K56   ,K66   ,OFFG   )
C
      RETURN
      END
