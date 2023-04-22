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
Chd|  S8SKE3                        source/elements/solid/solide8s/s8ske3.F
Chd|-- called by -----------
Chd|        IMP_GLOB_K                    source/implicit/imp_glob_k.F  
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|        ASSEM_S8                      source/implicit/assem_s8.F    
Chd|        S8EOFF                        source/elements/solid/solide8/s8eoff.F
Chd|        S8SCOORK_IMP                  source/elements/solid/solide8s/srcoork_imp.F
Chd|        S8SDERI3                      source/elements/solid/solide8s/s8sderi3.F
Chd|        S8SKSIG                       source/elements/solid/solide8s/s8sksig.F
Chd|        S8SLKE3                       source/elements/solid/solide8s/s8slke3.F
Chd|        TRANSK                        source/elements/solid/solide8s/transk.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE S8SKE3(
     1   PM,       GEO,      IXS,      X,
     2   ELBUF_STR,NEL,      ICP,      ICSIG,
     3   ETAG,     IDDL,     NDOF,     K_DIAG,
     4   K_LT,     IADK,     JDIK,     MPT,
     5   IPM,      IGEO,     IKGEO,    BUFMAT,
     6   NFT,      MTN,      JHBE,     JCVT,
     7   IGTYP,    ISORTH)
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
      INTEGER, INTENT(IN) :: JHBE
      INTEGER, INTENT(IN) :: JCVT
      INTEGER, INTENT(IN) :: IGTYP
      INTEGER, INTENT(IN) :: ISORTH
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
     .  IAD0,PID  , L, M, N,IJ,JJ,K
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
     .   Z5(MVSIZ), Z6(MVSIZ), Z7(MVSIZ), Z8(MVSIZ), R
c
      my_real
!     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
!     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
!     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   G1X(MVSIZ),G2X(MVSIZ),G3X(MVSIZ),
     .   G1Y(MVSIZ),G2Y(MVSIZ),G3Y(MVSIZ),
     .   G1Z(MVSIZ),G2Z(MVSIZ),G3Z(MVSIZ)
      my_real
     .   BID,WI,SMAX(MVSIZ),VOLG(MVSIZ),PP(MVSIZ),USB(MVSIZ),!NU(MVSIZ),
     .   VOLM(MVSIZ),SIGM(MVSIZ),NU1(MVSIZ),GAMA(MVSIZ,6)
      DOUBLE PRECISION
     .   KL(24,24,NEL),KTEMP(24,24),KS(24,24,NEL),  TRM(NEL,24,24),
     .   DN_X(MVSIZ,8),DN_Y(MVSIZ,8),DN_Z(MVSIZ,8),
     .   DN_R(8),DN_S(8),DN_T(8),INVJ(9,MVSIZ),
     .   V1(MVSIZ,9), V2(MVSIZ,9), V3(MVSIZ,9), V4(MVSIZ,9),
     .   V5(MVSIZ,9), V6(MVSIZ,9), V7(MVSIZ,9), V8(MVSIZ,9)
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
     .      HH(2,MVSIZ),FAC(MVSIZ),C1,E0(MVSIZ),DD(9,MVSIZ),!GG(MVSIZ),
     .      DM(9,MVSIZ),GM(9,MVSIZ),DGM(9,MVSIZ),DG(9,MVSIZ),
     .      G33(9,MVSIZ),HH1(2,MVSIZ),LAMDA,NU,GG
       my_real
     .   A11(MVSIZ), A12(MVSIZ), A13(MVSIZ), 
     .   A21(MVSIZ), A22(MVSIZ), A23(MVSIZ), 
     .   A31(MVSIZ), A32(MVSIZ), A33(MVSIZ)      
     
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
      CALL S8SCOORK_IMP(X,IXS(1,NF1),
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .   GBUF%OFF,OFFG,GBUF%SMSTR, GBUF%COR_FR,
!     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   V1,V2,V3,V4,V5,V6,V7,V8,
     .   NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,NGL,MXT,NGEO,
     .   K11,K12,K13,K14,K15,K16,K17,K18,K22,K23,
     .   K24,K25,K26,K27,K28,K33,K34,K35,K36,K37,
     .   K38,K44,K45,K46,K47,K48,K55,K56,K57,K58,
     .   K66,K67,K68,K77,K78,K88,JHBE,GBUF%GAMA,GAMA,
     .   NEL,TRM,KL)       !,TRM
C
      PID=NGEO(1)
C      IGTYP=NINT(GEO(12,PID))
C      CALL GET_GAMA(MTN,NEL,NBGAMA,JCVT,PM,IXS(1,NF1),IGTYP)
      DO I=1,NEL
        NU =PM(21,MXT(I))
        C1 =THREE*PM(32,MXT(I))/(ONE+NU)
        LAMDA=C1*NU
        GG =C1*(ONE-TWO*NU)
        HH(1,I)=LAMDA
        HH(2,I)=GG*HALF
      ENDDO
      ICPG = ICP
      IF (JHBE/=14.AND.JHBE/=17.AND.JHBE/=24) THEN
       ICPG = 1
      ENDIF 
c
     
C-----------Matrix B----------
C-----------Begin integrating points-----
      IL = 1
!      DO IR=1,NPTR
!       DO IS=1,NPTS
      DO IS=1,NPTS
       DO IR=1,NPTR
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
! used for full integration
!         CALL S8SDERI3(OFFG,OFF,VOLN,NGL,
!     .    A_GAUSS(IR,NPTR),A_GAUSS(IS,NPTS),A_GAUSS(IT,NPTT),WI,
!     .    X1,X2,X3,X4,X5,X6,X7,X8,
!     .    Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,
!     .    Z1,Z2,Z3,Z4,Z5,Z6,Z7,Z8,
!     .    DN_X,DN_Y,DN_Z,VOLM)
! used for ANS formulation
         CALL S8SDERI3(
     1   OFFG,            OFF,             VOLN,            NGL,
     2   A_GAUSS(IT,NPTT),A_GAUSS(IR,NPTR),A_GAUSS(IS,NPTS),WI,
     3   X1,              X2,              X3,              X4,
     4   X5,              X6,              X7,              X8,
     5   Y1,              Y2,              Y3,              Y4,
     6   Y5,              Y6,              Y7,              Y8,
     7   Z1,              Z2,              Z3,              Z4,
     8   Z5,              Z6,              Z7,              Z8,
     9   A11,             A12,             A13,             A21,
     A   A22,             A23,             A31,             A32,
     B   A33,             DN_R,            DN_S,            DN_T,
     C   INVJ,            DN_X,            DN_Y,            DN_Z,
     D   VOLM,            NEL)
         CALL S8SLKE3(
     1   KL,              HH,              VOLN,            !DN_X,
     2   DN_Y,            DN_Z,            A_GAUSS(IT,NPTT),A_GAUSS(IR,NPTR),
     3   A_GAUSS(IS,NPTS),DN_R,            DN_S,            DN_T,
     4   INVJ,            X1,              X2,              X3,
     5   X4,              X5,              X6,              X7,
     6   X8,              Y1,              Y2,              Y3,
     7   Y4,              Y5,              Y6,              Y7,
     8   Y8,              Z1,              Z2,              Z3,
     9   Z4,              Z5,              Z6,              Z7,
     A   Z8,              A11,             A12,             A13,
     B   A21,             A22,             A23,             A31,
     C   A32,             A33,             NEL)
        ENDDO 
       ENDDO 
      ENDDO
      
      CALL TRANSK(
     1   KL,      TRM,     NEL)
C-------------      
      CALL S8SKSIG(X,IXS(1,NF1),NEL,
     .             GBUF%COR_NF,KS,V1,V2,V3,V4,V5,V6,V7,V8,
     .             NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8)
      DO K=1,NEL
       DO J=1,24
        DO I=1,24
         KL(I,J,K)=KL(I,J,K)+KS(I,J,K)
        ENDDO
       ENDDO
      ENDDO
C----------------------------
C     CONVECTE --> GLOBAL.
C----------------------------     
      IF (NEIG>0) CALL S8EOFF(
     1   1, NEL, IXS(1,NF1), ETAG, OFFG)
     
      DO K=1,NEL
       DO J=1,3
        JJ=3*(J-1)
        DO I=1,3
         IJ= I+JJ
         K11(IJ,K)=KL(I,J,K)
         K22(IJ,K)=KL(I+3,J+3,K)
         K33(IJ,K)=KL(I+6,J+6,K)
         K44(IJ,K)=KL(I+9,J+9,K)
         K55(IJ,K)=KL(I+12,J+12,K)
         K66(IJ,K)=KL(I+15,J+15,K)
         K77(IJ,K)=KL(I+18,J+18,K)
         K88(IJ,K)=KL(I+21,J+21,K)
         K12(IJ,K)=KL(I,J+3,K)
         K13(IJ,K)=KL(I,J+6,K)
         K14(IJ,K)=KL(I,J+9,K)
         K15(IJ,K)=KL(I,J+12,K)
         K16(IJ,K)=KL(I,J+15,K)
         K17(IJ,K)=KL(I,J+18,K)
         K18(IJ,K)=KL(I,J+21,K)
         K23(IJ,K)=KL(I+3,J+6,K)
         K24(IJ,K)=KL(I+3,J+9,K)
         K25(IJ,K)=KL(I+3,J+12,K)
         K26(IJ,K)=KL(I+3,J+15,K)
         K27(IJ,K)=KL(I+3,J+18,K)
         K28(IJ,K)=KL(I+3,J+21,K)
         K34(IJ,K)=KL(I+6,J+9,K)
         K35(IJ,K)=KL(I+6,J+12,K)
         K36(IJ,K)=KL(I+6,J+15,K)
         K37(IJ,K)=KL(I+6,J+18,K)
         K38(IJ,K)=KL(I+6,J+21,K)
         K45(IJ,K)=KL(I+9,J+12,K)
         K46(IJ,K)=KL(I+9,J+15,K)
         K47(IJ,K)=KL(I+9,J+18,K)
         K48(IJ,K)=KL(I+9,J+21,K)
         K56(IJ,K)=KL(I+12,J+15,K)
         K57(IJ,K)=KL(I+12,J+18,K)
         K58(IJ,K)=KL(I+12,J+21,K)
         K67(IJ,K)=KL(I+15,J+18,K)
         K68(IJ,K)=KL(I+15,J+21,K)
         K78(IJ,K)=KL(I+18,J+21,K)
        ENDDO
       ENDDO
      ENDDO
      
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
