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
Chd|  Q4KE2                         source/elements/solid_2d/quad4/q4ke2.F
Chd|-- called by -----------
Chd|        IMP_GLOB_K0                   source/implicit/imp_glob_k.F  
Chd|-- calls ---------------
Chd|        ASSEM_Q4                      source/implicit/assem_q4.F    
Chd|        MMATS                         source/elements/solid/solide8z/mmats.F
Chd|        MMAT_H1                       source/elements/solid/solide8z/s8zke3.F
Chd|        MMSTIFS                       source/elements/solid/solide8z/mmats.F
Chd|        Q4DERI2                       source/elements/solid_2d/quad4/q4deri2.F
Chd|        Q4DERIC2                      source/elements/solid_2d/quad4/q4deric2.F
Chd|        Q4KEGA2                       source/elements/solid_2d/quad4/q4kega2.F
Chd|        Q4KEL2                        source/elements/solid_2d/quad4/q4kel2.F
Chd|        Q4KEP2                        source/elements/solid_2d/quad4/q4kep2.F
Chd|        Q4KEROT2                      source/elements/solid_2d/quad4/q4kerot2.F
Chd|        QCOOR2                        source/elements/solid_2d/quad/qcoor2.F
Chd|        QRCOOR2                       source/elements/solid_2d/quad/qrcoor2.F
Chd|        QVOLU2                        source/elements/solid_2d/quad/qvolu2.F
Chd|        S8ZSIGP3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE Q4KE2 (PM     ,GEO    ,IXQ    ,X      ,ELBUF_STR,
     1                  NEL    ,LIAD   ,ICP    ,ICSIG  ,ETAG   ,
     2                  IDDL   ,NDOF   ,K_DIAG ,K_LT   ,IADK   ,
     3                  JDIK   ,NPG    ,IPM    ,IGEO   ,IKGEO  ,
     4                  BUFMAT )
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXQ(NIXQ,*), ICP, ICSIG, IKGEO
      INTEGER NEL, LIAD, NPG,
     .   IPM(NPROPMI,*), IGEO(NPROPGI,*), ETAG(*), IDDL(*),
     .   NDOF(*), IADK(*), JDIK(*)
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*), X(3,*),
     .   BUFMAT(*), K_DIAG(*), K_LT(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
c FUNCTION: elemental stiffness of Q4 element
c ARGUMENTS:  (I: input, O: output, IO: input & output, W: workspace)
c TYPE NAME                FUNCTION
c  I   PM(),GEO()          material and geometrical property data
c  I   IXQ(7,NUM_QUAD)     connectivity and mid,pid integer data
c  I   X(3,NUMNOD)         coordinates
c  I   NEL                 number of quad element in this group
c  I   ICP                 flag for constant pressure
c  I   ^ICSG                flag for solid shell usage
c  I   ^ETAG()              for eigein value computation usage
c  I   IDDL()              position for nodal DOFs
c  I   NDOF()              number of nodal DOFs
c  I   K_DIAG()            diagonal K
c  I   K_LT()              off-diagonal K
c  I   IADK()              position for K
c  I   JDIK()              position for K
c  I   NPG                 flag for number of integration points
c  I   IPM(NPROPMI,*)      material property data (integer)
c  I   IGEO(NPROPGI,*)     geometrical property data (integer)
c  I   IKGEO               flag for geometric stiffness computation method
c  I   BUFMAT()            buffer for material constitution computation
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,EP
C
      INTEGER IKORTH,IADBUF,ICPG,IPREDU
C
      INTEGER IAD0
C     INDEX OF ELEMENT INFORMATION IN GLOBAL TABLE "IXQ"
      INTEGER LCO
C     SN OF THE FIRST ELEMENT OF THE GROUP IN GLOBAL STORAGE
      INTEGER NF1
C     MATERIAL SN, CONNECTIVITY, ID, PROPERTY SN OF ELEMENTS
      INTEGER MXT(MVSIZ),
     +    NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ),NC4(MVSIZ),
     +    NGL(MVSIZ),NGEO(MVSIZ)
C     NUMBER AND INDEXES OF INTEGRATION POINTS
      INTEGER NNPT,NPTR,NPTS,IR,IS,IT,IP
C
C     ELEMENT D/A FLAG
C     NODAL COORDINATES (t+i)
C     DIFFERENCES OF "Y", "Z"
C     SUMMERIES OF "Y"
C     SHAPE DERIVATIVES (dNi/dY, dNi/dZ, Ni/r) AT CENTER
C     AREA, VOLUME/(THICKNESS .OR. 2*PI)
C     SHAPE DERIVATIVES (dNi/dY, dNi/dZ) AT INTEGRATION POINT
C     (W*|J| .OR. r'* W*|J|) AT INTEGRATION POINT
C     STIFFNESS MATRIX
C     TRANSFORMATION MATRIX [R] FOR CO-ROTATIONAL CASE
C     {X}=[R]{X'} <=> {X'}=T([R]){X}
C     [K]=[R][K']T([R])
      my_real
     +    OFFG(MVSIZ),OFF(MVSIZ),GAMA(MVSIZ,6),
     +    Y1(MVSIZ),Y2(MVSIZ),Y3(MVSIZ),Y4(MVSIZ),
     +    Z1(MVSIZ),Z2(MVSIZ),Z3(MVSIZ),Z4(MVSIZ),
     +    Y12(MVSIZ),Y34(MVSIZ),Y13(MVSIZ),Y24(MVSIZ),
     +    Y14(MVSIZ),Y23(MVSIZ),
     +    Z12(MVSIZ),Z34(MVSIZ),Z13(MVSIZ),Z24(MVSIZ),
     +    Z14(MVSIZ),Z23(MVSIZ),
     +    Y234(MVSIZ),Y124(MVSIZ),YAVG(MVSIZ),
     +    PYC1(MVSIZ),PYC2(MVSIZ),PZC1(MVSIZ),PZC2(MVSIZ),
     +    AY(MVSIZ),AY1(MVSIZ),AY2(MVSIZ),AY3(MVSIZ),AY4(MVSIZ),
     +    AIRE(MVSIZ),VOLU(MVSIZ),
     +    PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),
     +    PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ),
     +    AIRN(MVSIZ),VOLN(MVSIZ),
     +    K11(2,2,MVSIZ),K12(2,2,MVSIZ),K13(2,2,MVSIZ),K14(2,2,MVSIZ),
     +    K22(2,2,MVSIZ),K23(2,2,MVSIZ),K24(2,2,MVSIZ),
     +    K33(2,2,MVSIZ),K34(2,2,MVSIZ),K44(2,2,MVSIZ),
     + K11U(2,2,MVSIZ),K12U(2,2,MVSIZ),K13U(2,2,MVSIZ),K14U(2,2,MVSIZ),
     + K22U(2,2,MVSIZ),K23U(2,2,MVSIZ),K24U(2,2,MVSIZ),
     + K33U(2,2,MVSIZ),K34U(2,2,MVSIZ),K44U(2,2,MVSIZ),
     + K11L(2,2,MVSIZ),K12L(2,2,MVSIZ),K13L(2,2,MVSIZ),K14L(2,2,MVSIZ),
     + K22L(2,2,MVSIZ),K23L(2,2,MVSIZ),K24L(2,2,MVSIZ),
     + K33L(2,2,MVSIZ),K34L(2,2,MVSIZ),K44L(2,2,MVSIZ),
     +    R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     +    R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     +    R31(MVSIZ),R32(MVSIZ),R33(MVSIZ)
C
C     MATERIAL PARAMETERS
      my_real
     .    NU(MVSIZ),C1,E0(MVSIZ),FAC(MVSIZ),BID(1),
     .    HH(2,MVSIZ),HH1(2,MVSIZ),
     .    DM(9,MVSIZ),DGM(9,MVSIZ),GM(9,MVSIZ),
     .    DD(9,MVSIZ),DG(9,MVSIZ),GG(MVSIZ),G33(9,MVSIZ),
     .    BYZ1(MVSIZ),BYZ2(MVSIZ),BYZ3(MVSIZ),BYZ4(MVSIZ),
     .    BZY1(MVSIZ),BZY2(MVSIZ),BZY3(MVSIZ),BZY4(MVSIZ),
     +    NUU(MVSIZ)
C
C     USED ONLY FOR SUBROUTINE INTERFACE
      my_real
     +    VD2(MVSIZ),VIS(MVSIZ),
     +    RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),
     +    SX(MVSIZ),SY(MVSIZ),SZ(MVSIZ),
     +    TX(MVSIZ),TY(MVSIZ),TZ(MVSIZ)
      my_real
     +    AA,BB
C
      my_real
     +  WI,KSI,ETA
      my_real
     +  W_GAUSS(9,9),A_GAUSS(9,9)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
c
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
      IF (ISORTH == 0) THEN
        DO I=LFT,LLT
          GAMA(I,1) = ONE
          GAMA(I,2) = ZERO
          GAMA(I,3) = ZERO
          GAMA(I,4) = ZERO
          GAMA(I,5) = ONE
          GAMA(I,6) = ZERO
        ENDDO
      ELSE
        DO I=LFT,LLT                                            
          GAMA(I,1) = GBUF%GAMA(I        )
          GAMA(I,2) = GBUF%GAMA(I +   NEL)
          GAMA(I,3) = GBUF%GAMA(I + 2*NEL)
          GAMA(I,4) = GBUF%GAMA(I + 3*NEL)
          GAMA(I,5) = GBUF%GAMA(I + 4*NEL)
          GAMA(I,6) = GBUF%GAMA(I + 5*NEL)
        ENDDO
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
C
      LCO = 1 + NIXQ*NFT
      NF1 = 1 + NFT
C
C     GATHER NODAL INFORMATION & 
C     COMPUTE INTRINSIC ROTATION FOR CO-ROTATIONAL CASE & 
C     PROJECT NODAL COORDINATES INTO CO-ROTATIONAL SYSTEM
      IF (JCVT==0) THEN
        CALL QCOOR2(
     1   X,         IXQ(1,NF1),Y1,        Y2,
     2   Y3,        Y4,        Z1,        Z2,
     3   Z3,        Z4,        NC1,       NC2,
     4   NC3,       NC4,       NGL,       MXT,
     5   NGEO,      VD2,       VIS,       NEL)
      ELSE
        CALL QRCOOR2(
     1   X,         IXQ(1,NF1),Y1,        Y2,
     2   Y3,        Y4,        Z1,        Z2,
     3   Z3,        Z4,        NC1,       NC2,
     4   NC3,       NC4,       NGL,       MXT,
     5   NGEO,      VD2,       R11,       R12,
     6   R13,       R21,       R22,       R23,
     7   R31,       R32,       R33,       GAMA,
     8   Y234,      Y124,      VIS,       NEL,
     9   ISORTH)
      ENDIF
C
C     COMPUTE FAC(*) FOR PRESSURE ACCORDING TO PLASTICITY STATE
C---- now assumed strain is used, no effect for Isolid17
        DO I=LFT,LLT
          NU(I)=MIN(HALF,PM(21,MXT(I)))
          C1 =PM(32,MXT(I))
          E0(I) =THREE*(ONE-TWO*NU(I))*C1
        ENDDO
      IF(ICP==2) THEN
        CALL S8ZSIGP3(LFT  ,LLT       ,GBUF%SIG,E0  ,GBUF%PLA,
     2                FAC  ,GBUF%G_PLA,NEL     )
        DO I=LFT,LLT
          NUU(I)=NU(I)+(HALF-NU(I))*FAC(I)
        ENDDO
      ELSEIF(ICP==1) THEN
        DO I=LFT,LLT
          NUU(I)=HALF
        ENDDO
      ELSE
        DO I=LFT,LLT
          NUU(I)=ZERO
        ENDDO
      ENDIF 
C     COMPUTE AREA & VOLUME/THICKNESS
      CALL QVOLU2(GBUF%OFF,AIRE,VOLU,NGL,
     2            Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     3            Y234,Y124)
C
      IF(N2D==1) THEN
        DO I=LFT,LLT
         YAVG(I) = X(2,NC1(I))+X(2,NC2(I))+X(2,NC3(I))+X(2,NC4(I))
        ENDDO
      ENDIF
C     COMPUTE SHAPE DERIVATIVES AT ELEMENT CENTER
      CALL Q4DERIC2(
     1   Y1,      Y2,      Y3,      Y4,
     2   Z1,      Z2,      Z3,      Z4,
     3   Y12,     Y34,     Y13,     Y24,
     4   Y14,     Y23,     Z12,     Z34,
     5   Z13,     Z24,     Z14,     Z23,
     6   PYC1,    PYC2,    PZC1,    PZC2,
     7   AIRE,    VOLU,    YAVG,    RX,
     8   RY,      RZ,      SX,      SY,
     9   SZ,      NEL,     JHBE)
C
C     COMPUTE VOLUME/(2*PI) FOR AXISYMMETRIC CASE
C     
C
      ICPG = 0
      IF(ICPG==2) ICPG = 1
C
C
      NPTR = 2
      NPTS = 2
      NNPT = NPTR*NPTS
C
      IF (MTN>=28) THEN
        IADBUF = IPM(7,MXT(1))
      ELSE
        IADBUF = 0
      ENDIF
      CALL MMATS(LFT    ,LLT     ,PM    ,MXT    ,HH    ,
     .           MTN    ,IKORTH  ,IPM   ,IGEO   ,GAMA  ,
     .           BUFMAT(IADBUF)  ,DM    ,DGM    ,GM    ,
     .           JHBE  ,GBUF%SIG ,BID   ,NNPT   ,NEL   )
      CALL MMAT_H1(HH    ,HH1   ,FAC  ,ICPG  ,IPREDU)
C
      DO I=LFT,LLT
        OFFG(I) = GBUF%OFF(I)
      ENDDO
C
      DO EP=LFT,LLT
      DO J=1,2
      DO I=1,2
        K11(I,J,EP)=ZERO
        K12(I,J,EP)=ZERO
        K13(I,J,EP)=ZERO
        K14(I,J,EP)=ZERO
        K22(I,J,EP)=ZERO
        K23(I,J,EP)=ZERO
        K24(I,J,EP)=ZERO
        K33(I,J,EP)=ZERO
        K34(I,J,EP)=ZERO
        K44(I,J,EP)=ZERO
        K11U(I,J,EP)=ZERO
        K12U(I,J,EP)=ZERO
        K13U(I,J,EP)=ZERO
        K14U(I,J,EP)=ZERO
        K22U(I,J,EP)=ZERO
        K23U(I,J,EP)=ZERO
        K24U(I,J,EP)=ZERO
        K33U(I,J,EP)=ZERO
        K34U(I,J,EP)=ZERO
        K44U(I,J,EP)=ZERO
        K11L(I,J,EP)=ZERO
        K12L(I,J,EP)=ZERO
        K13L(I,J,EP)=ZERO
        K14L(I,J,EP)=ZERO
        K22L(I,J,EP)=ZERO
        K23L(I,J,EP)=ZERO
        K24L(I,J,EP)=ZERO
        K33L(I,J,EP)=ZERO
        K34L(I,J,EP)=ZERO
        K44L(I,J,EP)=ZERO
      ENDDO
      ENDDO
      ENDDO
C
C     ENTER THE INTEGRATION POINTS LOOP -->
      IT = 1
      DO 100 IR=1,NPTR
      DO 200 IS=1,NPTS
        LBUF => ELBUF_STR%BUFLY(1)%LBUF(IR,IS,1)
C
C     INITIALIZE WEIGHTING FACTORS
      IP = IR + (IS-1)*NPTR
      KSI = A_GAUSS(IR,NPTR)
      ETA = A_GAUSS(IS,NPTS)
      WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)
C
C     COMPUTE JACOBIAN & SHAPE DERIVATIVES AT INTEGRATION POINT
        CALL Q4DERI2(
     1   OFFG,    OFF,     KSI,     ETA,
     2   WI,      YAVG,    Y12,     Y34,
     3   Y13,     Y24,     Y14,     Y23,
     4   Z12,     Z34,     Z13,     Z24,
     5   Z14,     Z23,     PY1,     PY2,
     6   PY3,     PY4,     PZ1,     PZ2,
     7   PZ3,     PZ4,     PYC1,    PYC2,
     8   PZC1,    PZC2,    BYZ1,    BYZ2,
     9   BYZ3,    BYZ4,    BZY1,    BZY2,
     A   BZY3,    BZY4,    AIRN,    VOLN,
     B   NUU,     NEL,     JHBE)
C-------------add BYZj... later
C
      CALL MMSTIFS(PM   ,MXT  ,HH1   ,VOLN  ,ICSIG ,
     .             DD   ,GG   ,DG    ,G33   ,DM    ,
     .             GM   ,DGM  ,IKORTH,LBUF%SIG,IR  ,
     .             IS   ,IT   ,NEL   )
C
      CALL Q4KEL2(
     1            PY1,PY2,PY3,PY4,
     2            PZ1,PZ2,PZ3,PZ4,
     3            PYC1,PYC2,PZC1,PZC2,AY,R22,R23,
     4            K11,K12,K13,K14,K22,
     5            K23,K24,K33,K34,K44,
     4            K11U,K12U,K13U,K14U,K22U,
     5            K23U,K24U,K33U,K34U,K44U,
     4            K11L,K12L,K13L,K14L,K22L,
     5            K23L,K24L,K33L,K34L,K44L,
     6            DD,GG,DG,G33,IKORTH,ICPG,OFFG)
C
c      IF (IKGEO<0) THEN
c        CALL Q4KEG2(
c     1              PY1,PY2,PY3,PY4,
c     2              PZ1,PZ2,PZ3,PZ4,AY,
c     3              K11,K12,K13,K14,K22,
c     4              K23,K24,K33,K34,K44,
c     5              EV(NB2),VOLN,OFFG)
c      ENDIF
C
200   CONTINUE
100   CONTINUE
C     EXIT THE INTEGRATION POINTS LOOP <--
C
      IF (IPREDU > 0) THEN
        CALL Q4KEP2(
     1              PYC1,PYC2,PZC1,PZC2,AY,R22,R23,
     2              K11,K12,K13,K14,K22,
     3              K23,K24,K33,K34,K44,
     4              HH,VOLU,FAC,ICPG,OFFG)
      ENDIF
C
c      IF (IKGEO>0) THEN
      IF (IKGEO/=0) THEN
        CALL Q4KEGA2(
     1               PYC1,PYC2,PZC1,PZC2,AY,
     2               K11,K12,K13,K14,K22,
     3               K23,K24,K33,K34,K44,
     4               GBUF%SIG,VOLU,OFFG,NEL)
      ENDIF
C
c      IF (N2D==1 .AND. JHBE==17) THEN
c        CALL Q4ASYM(LFT, LLT, K11, K11U, K11L)
c        CALL Q4ASYM(LFT, LLT, K22, K22U, K22L)
c        CALL Q4ASYM(LFT, LLT, K33, K33U, K33L)
c        CALL Q4ASYM(LFT, LLT, K44, K44U, K44L)
c        CALL Q4ASYM(LFT, LLT, K12, K12U, K12L)
c        CALL Q4ASYM(LFT, LLT, K13, K13U, K13L)
c        CALL Q4ASYM(LFT, LLT, K14, K14U, K14L)
c        CALL Q4ASYM(LFT, LLT, K23, K23U, K23L)
c        CALL Q4ASYM(LFT, LLT, K24, K24U, K24L)
c        CALL Q4ASYM(LFT, LLT, K34, K34U, K34L)
c      ENDIF
C
      IF (JCVT/=0) THEN
        CALL Q4KEROT2(
     1                R22,R32,R23,R33,
     2                K11,K12,K13,K14,K22,
     3                K23,K24,K33,K34,K44)
      ENDIF
C
C     IF (NEIG>0) THEN
C       CALL Q4EOFF(LFT, LLT, IXQ(1,NF1), ETAG, OFFG)
C     ENDIF
C
      CALL ASSEM_Q4(
     1    IXQ(1,NF1),NEL   ,IDDL  ,NDOF  ,K_DIAG,
     2    K_LT  ,IADK  ,JDIK  ,K11   ,K12   ,
     3    K13   ,K14   ,K22   ,K23   ,K24   ,
     4    K33   ,K34   ,K44   ,OFFG  )
C
      RETURN
      END
Chd|====================================================================
Chd|  Q4ASYM                        source/elements/solid_2d/quad4/q4ke2.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE Q4ASYM (LFT, LLT, KIJ, KIJU, KIJL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT
      my_real
     .   KIJ(2,2,*), KIJU(2,2,*), KIJL(2,2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER EP,I,J
      my_real
     .    AA,BB
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO EP=LFT,LLT
      DO J=1,2
      DO I=1,2
        AA = KIJ(I,J,EP) + KIJU(I,J,EP)
        BB = KIJ(I,J,EP) + KIJL(I,J,EP)
        IF (AA<ZERO .AND. BB<ZERO) THEN
c          KIJ(I,J,EP) = 0.5*(AA+BB)
c          KIJ(I,J,EP) = MIN(AA,BB)
c          KIJ(I,J,EP) = MAX(AA,BB)
        ELSEIF (AA>ZERO .AND. BB>ZERO) THEN
c          KIJ(I,J,EP) = 0.5*(AA+BB)
c          KIJ(I,J,EP) = MIN(AA,BB)
c          KIJ(I,J,EP) = MAX(AA,BB)
        ELSE
c          KIJ(I,J,EP) = 0.5*(AA+BB)
c          KIJ(I,J,EP) = MIN(AA,BB)
c          KIJ(I,J,EP) = MAX(AA,BB)
        ENDIF
      ENDDO
      ENDDO
      ENDDO
C
      RETURN
      END

