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
Chd|  Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|-- called by -----------
Chd|        FORINT                        source/elements/forint.F      
Chd|-- calls ---------------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        Q4COOR2                       source/elements/solid_2d/quad4/q4coor2.F
Chd|        Q4CUMU2                       source/elements/solid_2d/quad4/q4cumu2.F
Chd|        Q4CUMU2P                      source/elements/solid_2d/quad4/q4cumu2.F
Chd|        Q4DEFO2                       source/elements/solid_2d/quad4/q4defo2.F
Chd|        Q4DEFOC2                      source/elements/solid_2d/quad4/q4defoc2.F
Chd|        Q4DERI2                       source/elements/solid_2d/quad4/q4deri2.F
Chd|        Q4DERIC2                      source/elements/solid_2d/quad4/q4deric2.F
Chd|        Q4FINT2                       source/elements/solid_2d/quad4/q4fint2.F
Chd|        Q4FINTC2                      source/elements/solid_2d/quad4/q4fintc2.F
Chd|        Q4RCOOR2                      source/elements/solid_2d/quad4/q4rcoor2.F
Chd|        Q4RROTA2                      source/elements/solid_2d/quad4/q4rrota2.F
Chd|        Q4VIS2                        source/elements/solid_2d/quad4/q4forc2.F
Chd|        Q4ZERO2                       source/elements/solid_2d/quad4/q4zero2.F
Chd|        QBILAN                        source/elements/solid_2d/quad/qbilan.F
Chd|        QDLEN2                        source/elements/solid_2d/quad/qdlen2.F
Chd|        QLAGR2                        source/elements/solid_2d/quad/qlagr2.F
Chd|        QMASS2                        source/elements/solid_2d/quad/qmass2.F
Chd|        QMASS2P                       source/elements/solid_2d/quad/qmass2.F
Chd|        QMASSREAL2                    source/elements/solid_2d/quad/qmass2.F
Chd|        QMASSREAL2P                   source/elements/solid_2d/quad/qmass2.F
Chd|        QROTA2                        source/elements/solid_2d/quad/qrota2.F
Chd|        QVOLU2                        source/elements/solid_2d/quad/qvolu2.F
Chd|        S8EFMOY3                      source/elements/solid/solide8e/s8efint3.F
Chd|        S8ZSIGP3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SMALLB3                       source/elements/solid/solide/small3.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MMAIN_MOD                     source/materials/mat_share/mmain.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE Q4FORC2(PM    ,GEO    ,IC     ,X     ,A      ,
     2                   V     ,MS     ,W      ,FLUX  ,FLU1  ,
     3                   VEUL   ,FV     ,ALE_CONNECT ,IPARG  ,NLOC_DMG,
     4                   ELBUF_TAB,TF  ,NPF    ,BUFMAT,PARTSAV,
     5                   DT2T  ,NELTST ,ITYPTST,STIFN ,OFFSET ,
     6                   EANI  ,IPARTQ ,NEL    ,IADQ  ,FSKY   ,
     7                   ICP   ,NG     ,
     8                   IPM   ,BUFVOIS,QMV    ,GRESAV,GRTH   ,
     9                   IGRTH ,TABLE  ,IGEO   ,ITASK ,IEXPAN ,
     A                   MS_2D ,FSKYM  ,IOUTPRT,MATPARAM_TAB,H3D_STRAIN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MMAIN_MOD
      USE TABLE_MOD
      USE ELBUFDEF_MOD            
      USE NLOCAL_REG_MOD
      USE ALE_CONNECTIVITY_MOD
      USE MATPARAM_DEF_MOD
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
#include      "parit_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IC(*), IPARG(NPARG,*), NPF(*), IPARTQ(*), 
     +  IPM(*), GRTH(*), IGRTH(*), IGEO(*), IADQ(4,*),ITASK,IOUTPRT
      INTEGER OFFSET, NEL, NELTST, ITYPTST, ICP, NG, IEXPAN,H3D_STRAIN
      my_real
     +  DT2T
      my_real
     +  PM(NPROPM,*), GEO(*), X(*),A(*),V(3,*),MS(*), W(*),PARTSAV(*), 
     +  FLUX(4,*),FLU1(*),VEUL(*),FV(*), TF(*),BUFMAT(*), 
     +  FSKY(*),STIFN(*),EANI(*),BUFVOIS(6,*),QMV(8,*),GRESAV(*),MS_2D(*),
     +  FSKYM(*)
      TYPE (TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (NLOCAL_STR_)  , TARGET :: NLOC_DMG 
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE(MATPARAM_STRUCT_)  , DIMENSION(NUMMAT) :: MATPARAM_TAB
C-----------------------------------------------
c FUNCTION: Internal force computation of fully-integrated 2D Quad4 element
c ARGUMENTS:  (I: input, O: output, IO: input & output, W: workspace)
c TYPE NAME                FUNCTION
c  I   PM ,GEO             Material and geometrical property data
c  I   IC(7,NUM_QUAD)      connectivity and mid,pid integer data
c  I   X(3,NUMNOD)         co-ordinate 
c  IO  A(3,NUMNOD)         nodal internal force
c  I   V(3,NUMNOD)         nodal velocity
c  IO  MS(NUMNOD)          nodal masse
c  I   FLUX(4,NEL)         flux at each side used w/ ALE or EULER
c  I   FLU1,VEUL,IELVS     used w/ ALE or EULER 
c  I   IPARG(NG)           element group data
c  I   ELBUF()             internal element(material) data used w/ ALE or EULER
c  I   TF() ,NPF()         Radioss function (x=Time) data
c  I   BUFMAT()            internal material data
c  IO  PARTSAV()           output use per part
c  IO  DT2T                smallest elementary time step
c  O   NELTST,ITYPTST      element type (property type for spring) which determine DT2T
c  IO  STIFN(NUMNOD)       nodal stiffness to calcul nodal time step
c  IO  EANI()              anim output vector
c  I   IPARTQ()            quad element group data (output)
c  I   NEL                 nb of quad element in this group
c  I   IADQ(),FSKY()       work arrays for special option of internal force assemlage   
c  I   ICP                 flag for constant pressure
c  I   IPM(NPROPMI,*)      MATERIAL DATA (INTEGER)
c  I   BUFVOIS()           work table for fluid w/ SPMD 
c  I   QMV(8,)             work table used w/ ALE or EULER
c  I   GRESAV,GRTH,IGRTH   work table used for TH (time history) output
c  I   TABLE               new alternative Radioss function(table) data
c  I   IGEO                geometrical property integer data
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,IFLAG,IOFFS,ICPG,ISTRAIN
C     FOR THE USAGE OF EMPTY INPUT
      INTEGER IBID,IBIDON(1),ipr,enum
C     INDEX OF ELEMENT INFORMATION IN GLOBAL TABLE "IC"
      INTEGER LCO
C     SN OF THE FIRST ELEMENT OF THE GROUP IN GLOBAL STORAGE
      INTEGER NF1
C     MATERIAL SN, CONNECTIVITY, ID, PROPERTY SN OF ELEMENTS
      INTEGER MAT(MVSIZ),
     +    NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ),NC4(MVSIZ),
     +    NGL(MVSIZ),NGEO(MVSIZ)
C     NUMBER AND INDEXES OF INTEGRATION POINTS
      INTEGER NPTR,NPTS,IR,IS
C
      my_real, 
     .  DIMENSION(:), POINTER :: EINT
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
C     FOR THE USAGE OF EMPTY INPUT
      my_real
     +    BID(MVSIZ),MBID(MVSIZ),EHOU(MVSIZ)
C
C     LOCAL DIRECTIONS FOR ORTHOTROPIC MATERIAL USAGE
C     STIFNESS AT INTEGRATION POINT AND OF THE ELEMENT
C     NODAL COORDINATES (t+dt)
C     NODAL VELOCITIES (t+dt/2)
C     DIFFERENCES OF "Y", "Z"
C     SUMMERIES OF "Y"
C     AREA, VOLUME/(THICKNESS .OR. 2*PI), CHARACTERISTIC LENGTH
C     TRANSFORMATION MATRIX [R] FOR CO-ROTATIONAL CASE
C     {X}=[R]{X'} <=> {X'}=T([R]){X}
      my_real
     +    OFFG(MVSIZ),OFFS(MVSIZ),OFF(MVSIZ),
     +    GAMA(MVSIZ,6),
     +    STI(MVSIZ),STIM(MVSIZ),
     +    Y1(MVSIZ),Y2(MVSIZ),Y3(MVSIZ),Y4(MVSIZ),
     +    Z1(MVSIZ),Z2(MVSIZ),Z3(MVSIZ),Z4(MVSIZ),
     +    VY1(MVSIZ),VY2(MVSIZ),VY3(MVSIZ),VY4(MVSIZ),
     +    VZ1(MVSIZ),VZ2(MVSIZ),VZ3(MVSIZ),VZ4(MVSIZ),
     +    Y12(MVSIZ),Y34(MVSIZ),Y13(MVSIZ),Y24(MVSIZ),
     +    Y14(MVSIZ),Y23(MVSIZ),
     +    Z12(MVSIZ),Z34(MVSIZ),Z13(MVSIZ),Z24(MVSIZ),
     +    Z14(MVSIZ),Z23(MVSIZ),
     +    Y234(MVSIZ),Y124(MVSIZ),YAVG(MVSIZ),
     +    AIRE(MVSIZ),VOLU(MVSIZ),DELTAX(MVSIZ),
     +    R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     +    R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     +    R31(MVSIZ),R32(MVSIZ),R33(MVSIZ)
C
C     USED WITH ALE OR EULER
C     VISCOUS PRESSURE (SPHERICAL, POSITIVE IF PRESSURE, OUTPUT OF "MMAIN")
C     SHAPE DERIVATIVES (dNi/dY, dNi/dZ) AT CENTER
C     Ni/r AT CENTER
C     SHAPE DERIVATIVES (dNi/dY, dNi/dZ) AT INTEGRATION POINT
C     JACOBIAN MATRIX [J] AT INTEGRATION POINT
C     (W*|J| OR r'* W*|J|) AT INTEGRATION POINT
C     (RHO*VOL-RHO0*VOL0')/RHO == RHO0*(VOL0-VOL0')/RHO
C     PLASTICITY STATE INFORMATION FOR PRESSURE COMPUTATION
C     RATE OF STRAIN FOR STRESS UPDATE
C     COMPONENTS OF ROTATION TENSOR
C     COMPONENTS OF OLD STRESS
C     INTERNAL FORCE IN LOCAL STORAGE
C     ELEMENT AVERAGE PRESSURE
      my_real
     +    VDY(MVSIZ),VDZ(MVSIZ),VDX(MVSIZ),VD2(MVSIZ),
     +    MUVOID(MVSIZ),
     +    VIS(MVSIZ),
     +    QVIS(MVSIZ),
     +    SSP(MVSIZ),
     +    SSP_EQ(MVSIZ),
     +    SIGY(MVSIZ),ET(MVSIZ),
c     +    DEFP(MVSIZ),
     +    R3_FREE(MVSIZ),R4_FREE(MVSIZ),
     +    PYC1(MVSIZ),PYC2(MVSIZ),PZC1(MVSIZ),PZC2(MVSIZ),
     +    AY(MVSIZ),
     +    PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),
     +    PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ),
     +    RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),
     +    SX(MVSIZ),SY(MVSIZ),SZ(MVSIZ),
     +    TX(MVSIZ),TY(MVSIZ),TZ(MVSIZ),
     +    AIRN(MVSIZ),VOLN(MVSIZ),
     +    DVOL(MVSIZ),
     +    NU(MVSIZ),E0(MVSIZ),C1,FAC(MVSIZ),
     +    EYY(MVSIZ),EZZ(MVSIZ),EXX(MVSIZ),
     +    EYZ(MVSIZ),EZX(MVSIZ),EXY(MVSIZ),
     +    WYY(MVSIZ),WZZ(MVSIZ),WXX(MVSIZ),
     +    S1(MVSIZ),S2(MVSIZ),S3(MVSIZ),
     +    S4(MVSIZ),S5(MVSIZ),S6(MVSIZ),
     +    FY1(MVSIZ),FZ1(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     +    FY3(MVSIZ),FZ3(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     +    FAY(MVSIZ),FAZ(MVSIZ),
     +    PP(MVSIZ),DSV(MVSIZ)
C
      my_real
     +    AY1(MVSIZ),AY2(MVSIZ),AY3(MVSIZ),AY4(MVSIZ),YH(MVSIZ),
     +    FAY1(MVSIZ),FAY2(MVSIZ),FAY3(MVSIZ),FAY4(MVSIZ),
     +    DET(MVSIZ), 
     .    BYZ1(MVSIZ),BYZ2(MVSIZ),BYZ3(MVSIZ),BYZ4(MVSIZ),
     .    BZY1(MVSIZ),BZY2(MVSIZ),BZY3(MVSIZ),BZY4(MVSIZ),
     +    QN1,QN2,QN3,QN4,NUU(MVSIZ)
C
      my_real VARNL(NEL)
      my_real
     +  WI,KSI,ETA
      my_real
     +  W_GAUSS(9,9),A_GAUSS(9,9)
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
C
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      GBUF => ELBUF_TAB(NG)%GBUF
      IBID = 0
      IBIDON(1) = 0
C
      DO I=LFT,LLT
        EZX(I)=ZERO
        EXY(I)=ZERO
        WYY(I)=ZERO
        WZZ(I)=ZERO
        VDY(I)=ZERO
        VDZ(I)=ZERO
        VDX(I)=ZERO
      ENDDO
C
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
      ISORTHG = 0
      ISTRAIN = IPARG(44,NG)                                 
C
C     GATHER NODAL INFORMATION & 
C     COMPUTE INTRINSIC ROTATION FOR CO-ROTATIONAL CASE & 
C     PROJECT NODAL COORDINATES AND VELOCITES INTO CO-ROTATIONAL SYSTEM
      LCO = 1 + 7*NFT
      NF1 = 1 + NFT
      IF(JCVT==0) THEN
        CALL Q4COOR2(X,IC(LCO),
     2              Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     3              NC1,NC2,NC3,NC4,
     4              NGL,MAT,NGEO,VD2,VIS,
     5              V       ,VY1     ,VY2     ,VY3     ,
     6              VY4     ,VZ1     ,VZ2     ,VZ3     ,VZ4     ,
     7              YAVG    ,AY      ,EXX    )                   
      ELSE
C----EXX,YAVG ,AY are calculated in global system anyway, more efficient eo be done here
C----for the moment constant in element
        CALL Q4RCOOR2(
     1   X,       IC(LCO), Y1,      Y2,
     2   Y3,      Y4,      Z1,      Z2,
     3   Z3,      Z4,      NC1,     NC2,
     4   NC3,     NC4,     NGL,     MAT,
     5   NGEO,    VD2,     R11,     R12,
     6   R13,     R21,     R22,     R23,
     7   R31,     R32,     R33,     GAMA,
     8   Y234,    Y124,    VIS,     V,
     9   VY1,     VY2,     VY3,     VY4,
     A   VZ1,     VZ2,     VZ3,     VZ4,
     B   YAVG,    AY,      EXX,     NEL)
      ENDIF
C     COMPUTE FAC(*) FOR PRESSURE ACCORDING TO PLASTICITY STATE
C---- now assumed strain is used, no effect for Isolid17
        DO I=LFT,LLT
          NU(I)=MIN(HALF,PM(21,MAT(I)))
          C1 =PM(32,MAT(I))
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
C     COMPUTE AREA & VOLUME
      CALL QVOLU2(GBUF%OFF,AIRE,VOLU,NGL,
     2            Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     3            Y234,Y124)
C
C     COMPUTE CHARACTERISTIC LENGTH OF EACH ELEMENT FOR DETERMINING TIME STEP 
      CALL QDLEN2(Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,AIRE,DELTAX,IPARG(63,NG))
C
C     COMPUTE SHAPE DERIVATIVES AT ELEMENT CENTER
C
      CALL Q4DERIC2(Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     2              Y12,Y34,Y13,Y24,Y14,Y23,
     3              Z12,Z34,Z13,Z24,Z14,Z23,
     4              PYC1,PYC2,PZC1,PZC2,AIRE,
     5              VOLU,YAVG,RX ,RY ,RZ  ,
     6              SX  ,SY ,SZ )
C
C -----ICPG could be cleaned after---
      ICPG = 0
C      IF(ICPG==2) ICPG = 1
C
C     COMPUTE SHEAR & VOLUMETRIC STRAIN RATE AT ELEMENT CENTER
C     WITH B(t+dt), V(t+dt/2) & CORRECTION
      CALL Q4DEFOC2(VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,
     2              PYC1,PYC2,PZC1,PZC2,AIRE,
     3              EYZ,EXX,DSV,ICPG)
C
C
      IOFFS = 0
      DO I=LFT,LLT
        OFFS(I) = EP20
        OFFG(I) = GBUF%OFF(I)
      ENDDO
C
C
C     INITIALIZATION BEFORE INTEGRATION LOOP
      CALL Q4ZERO2(
     +  FY1, FZ1, FY2, FZ2, FY3, FZ3, FY4, FZ4,  
     +  FAY, FAZ, FAY1, FAY2, FAY3, FAY4,
     +  GBUF%SIG,GBUF%EINT,GBUF%RHO,GBUF%QVIS,GBUF%PLA,
     +  GBUF%EPSD,STIM,PP,GBUF%G_PLA,GBUF%G_EPSD,NEL)
C
C     ENTER THE INTEGRATION POINTS LOOP -->
      NPTR = 2
      NPTS = 2
      DO 100 IR=1,NPTR
        DO 200 IS=1,NPTS
          LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,1)
C
C       INITIALIZE WEIGHTING FACTORS
        KSI = A_GAUSS(IR,NPTR)
        ETA = A_GAUSS(IS,NPTS)
        WI = W_GAUSS(IR,NPTR)*W_GAUSS(IS,NPTS)
C
C       INITIALIZE INDEX OF ELEMENT DATA ARRAY
C
C       COMPUTE JACOBIAN & SHAPE DERIVATIVES AT INTEGRATION POINT
        CALL Q4DERI2(OFFG,OFF,KSI,ETA,WI,YAVG,
     2               Y12,Y34,Y13,Y24,Y14,Y23,
     3               Z12,Z34,Z13,Z24,Z14,Z23,
     4               PY1,PY2,PY3,PY4,
     5               PZ1,PZ2,PZ3,PZ4,
     7               PYC1,PYC2,PZC1,PZC2,
     8               BYZ1,BYZ2,BYZ3,BYZ4,
     9               BZY1,BZY2,BZY3,BZY4,
     B               AIRN,VOLN,NUU)
C
C       COMPUTE RATE OF STRAIN AT INTEGRATION POINT WITH B(t+dt),V(t+dt/2) & CORRECTION
C       MODIFY INITIAL VOLUME & INTERNAL ENERGY DENSITY
        CALL Q4DEFO2(PY1,PY2,PY3,PY4,
     2               PZ1,PZ2,PZ3,PZ4,
     8               BYZ1,BYZ2,BYZ3,BYZ4,
     9               BZY1,BZY2,BZY3,BZY4,
     3               VY1,VY2,VY3,VY4,
     4               VZ1,VZ2,VZ3,VZ4,
     5               EYZ,EYY,EZZ,EXX,WXX,
     .               R22,R23,
     6               AY,OFF,GBUF%OFF,LBUF%VOL,
     7               LBUF%EINT,DSV,ICPG,FAC)
C
C
C       COMPUTE ADDITIONAL VOLUME CHANGE & MODIFY CURRENT DENSITY
C       DVOL = (RHO*VOL-RHO0*VOL0')/RHO = RHO0*(VOL0-VOL0')/RHO
C       RHO' = RHO0*VOL0'/VOL
        CALL QLAGR2(
     1   PM,       LBUF%VOL, LBUF%RHO, LBUF%EINT,
     2   VOLN,     DVOL,     MAT,      NEL)
C
C       COPY OLD STRESS AND DO NECESSARY TREATMENT
        CALL QROTA2(LBUF%SIG,S1,S2,S3,S4,S5,S6,WXX,NEL)
C
C       UPDATE STRESS
        CALL MMAIN(
     1     ELBUF_TAB,NG     ,PM     ,GEO        ,FV	    ,ALE_CONNECT  ,
     2     IC       ,IPARG  ,V      ,TF         ,NPF	    ,BUFMAT ,
     3     STI      ,X      ,DT2T   ,NELTST     ,ITYPTST    ,OFFSET ,
     4     NEL      ,W      ,OFF    ,NGEO       ,
     6     MAT      ,NGL    ,VOLN   ,VD2        ,DVOL	    ,DELTAX ,
     7     VIS      ,QVIS   ,SSP    ,S1         ,S2	    ,S3     ,
     8     S4       ,S5     ,S6     ,EYY        ,EZZ	    ,EXX    ,
     9     EYZ      ,EZX    ,EXY    ,WYY        ,WZZ	    ,WXX    ,
     A     RX       ,RY     ,RZ     ,SX         ,SY	    ,SZ     ,
     B     VDY      ,VDZ    ,VDX    ,MUVOID     ,SSP_EQ     ,AIRE   ,
     C     SIGY     ,ET     ,BUFVOIS,LBUF%PLA   ,R3_FREE    ,R4_FREE,
     D     EYY      ,EZZ    ,EXX    ,EYZ        ,EZX	    ,EXY    ,
     E     WYY      ,WZZ    ,WXX    ,IPM        ,GAMA	    ,BID    ,
     F     BID      ,BID    ,BID    ,BID        ,BID	    ,BID    ,
     G     ISTRAIN  ,BID    ,BID    ,IBIDON(1)  ,1	    ,MBID   ,
     H     MBID     ,IR     ,IS     ,1          ,TABLE      ,BID    ,
     I     BID      ,BID    ,BID    ,IPARG(1,NG),IGEO   ,BID      ,
     J     ITASK    ,NLOC_DMG,VARNL ,MATPARAM_TAB,H3D_STRAIN)
C
C       COMPUTE NODAL INTERNAL FORCE
        CALL Q4FINT2(LBUF%SIG,AY,FAY,FAZ,
     2               PY1,PY2,PY3,PY4,
     3               PZ1,PZ2,PZ3,PZ4,
     8               BYZ1,BYZ2,BYZ3,BYZ4,
     9               BZY1,BZY2,BZY3,BZY4,
     5               FY1,FZ1,FY2,FZ2,
     6               FY3,FZ3,FY4,FZ4,
     7               R22,R23,R32,R33,
     8               AIRN,VOLN,QVIS,ICPG,
     9               NEL)
C
C       COMPUTE ELEMENT AVERAGE & SUMMERY DATA
        CALL S8EFMOY3(LBUF%SIG ,
     2       VOLN    ,QVIS    ,PP      ,LBUF%EINT ,LBUF%RHO ,LBUF%QVIS ,       
     3       LBUF%PLA,LBUF%EPSD,GBUF%EPSD,GBUF%SIG,GBUF%EINT,GBUF%RHO,       
     4       GBUF%QVIS,GBUF%PLA,VOLU   ,STI     ,STIM    ,ICPG    ,       
     5       OFF     ,LBUF%VOL,GBUF%VOL,GBUF%G_PLA,GBUF%G_EPSD ,
     6       LBUF%EINTTH,GBUF%EINTTH,IEXPAN ,NEL,BID,BID)                                 
C
         DO I=LFT,LLT
           OFFG(I)=MIN(OFFG(I),OFF(I))
           IF (LBUF%OFF(I) > ONE .AND. GBUF%OFF(I) == ONE) THEN
             OFFS(I) = MIN(LBUF%OFF(I),OFFS(I))
             IOFFS = 1
           ENDIF
         ENDDO
C
200     CONTINUE
100   CONTINUE
C     EXIT THE INTEGRATION POINTS LOOP <--
C
      IF(IOFFS==1)THEN
        DO I=LFT,LLT
          IF(OFFS(I)<=TWO) THEN
            GBUF%OFF(I) = OFFS(I)
          ENDIF
        ENDDO
        DO IR=1,NPTR
          DO IS=1,NPTS
            LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,1)
            DO I=LFT,LLT
              IF (GBUF%OFF(I) > ONE) LBUF%OFF(I) = GBUF%OFF(I)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      CALL SMALLB3(GBUF%OFF,OFFG)
C
C     ADD NODAL INTERNAL FORCE FOR CONSTANT PRESSURE AND CONSTANT SHEAR STRAIN
      CALL Q4FINTC2(PYC1,PYC2,PZC1,PZC2,AY,FAY,
     2              FY1,FZ1,FY2,FZ2,
     3              FY3,FZ3,FY4,FZ4,
     4              AIRE,VOLU,GBUF%SIG,PP,ICPG,NEL)
C
      IF(N2D==1.AND.JHBE==17) THEN
         CALL Q4VIS2(PM,GBUF%OFF,GBUF%RHO,
     .                  Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .                  VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,
     .                  PY1,PY2,PZ1,PZ2,
     .                  FY1,FY2,FY3,FY4,FZ1,FZ2,FZ3,FZ4,
     .                  AIRE,SSP)
      ENDIF
C     OUTPUT ELEMENT INFORMATION TO BALANCE TABLE
      IFLAG=MOD(NCYCLE,NCPRI)
      IF(IOUTPRT>0)THEN
c
        IF (MTN == 11) THEN                        
          EINT => ELBUF_TAB(NG)%GBUF%EINS(1:NEL)   
        ELSE                                       
          EINT => ELBUF_TAB(NG)%GBUF%EINT(1:NEL)   
        ENDIF                                      
        CALL QBILAN(PARTSAV,GBUF%OFF,EINT,GBUF%RHO,GBUF%RK,
     2              GBUF%VOL,VY1,VY2,VY3,VY4,VZ1,VZ2,
     3              VZ3,VZ4,VOLU,IPARTQ,EHOU,R22,
     4              R23,R32,R33,GRESAV,GRTH,IGRTH,
     5              IBIDON(1),GBUF%EINTTH,ITASK)
      ENDIF
C
C     PROJECT NODAL INTERNAL FORCE INTO GLOBAL SYSTEM FOR CO-ROTATIONAL CASE
      IF(JCVT/=0) THEN
        CALL Q4RROTA2(
     1   R22,     R32,     R23,     R33,
     2   FY1,     FY2,     FY3,     FY4,
     3   FZ1,     FZ2,     FZ3,     FZ4,
     4   NEL)
      ENDIF
C
C     ADD TERMS OF INTERNAL FORCE FOR AXISYMMETRIC CASE ONLY
      IF(N2D==1.AND.JHBE==17) THEN
          DO I=LFT,LLT
            FY1(I) = FY1(I) + FAY(I)
            FY2(I) = FY2(I) + FAY(I)
            FY3(I) = FY3(I) + FAY(I)
            FY4(I) = FY4(I) + FAY(I)
            FZ1(I) = FZ1(I) + FAZ(I)
            FZ2(I) = FZ2(I) + FAZ(I)
            FZ3(I) = FZ3(I) + FAZ(I)
            FZ4(I) = FZ4(I) + FAZ(I)
          ENDDO
      ENDIF
C
C     COMPUTE MASS STIFFNESS
      IF(N2D==1.AND.JHBE==17) THEN
        IF(IPARIT==0) THEN
          CALL QMASS2(GBUF%OFF,GBUF%RHO,MS,AIRE,NC1,NC2,NC3,NC4)
        ELSE
          CALL QMASS2P(GBUF%OFF,GBUF%RHO,AIRE,FSKY,FSKY,IADQ)
        ENDIF
      ELSE
        IF(IPARIT==0) THEN
          CALL QMASS2(GBUF%OFF,GBUF%RHO,MS,VOLU,NC1,NC2,NC3,NC4)
        ELSE
          CALL QMASS2P(GBUF%OFF,GBUF%RHO,VOLU,FSKY,FSKY,IADQ)
        ENDIF
      ENDIF
C
C--------------------------
C     UPDATE OF MASSES : ALE physical masses
C----------------------------  
      IF (JALE+JEUL > 0 )THEN
         IF (IPARIT == 0)THEN
          CALL QMASSREAL2(GBUF%OFF,GBUF%RHO,MS_2D,
     .                  VOLU,NC1,NC2,NC3,NC4)
         ELSE
          CALL QMASSREAL2P(GBUF%OFF,GBUF%RHO,VOLU,FSKYM,IADQ)
         ENDIF
      ENDIF
C
C     ASSEMBLE NODAL INTERNAL FORCE INTO GLOBAL STORAGE
      IF(IPARIT==0) THEN
        CALL Q4CUMU2(A,STIFN,
     2               NC1,NC2,NC3,NC4,
     3               FY1,FZ1,FY2,FZ2,
     4               FY3,FZ3,FY4,FZ4,
     5               STIM)
      ELSE
        CALL Q4CUMU2P(FSKY,FSKY,IADQ,
     2                FY1,FZ1,FY2,FZ2,
     3                FY3,FZ3,FY4,FZ4,
     4                STIM)
      ENDIF
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  Q4VIS2                        source/elements/solid_2d/quad4/q4forc2.F
Chd|-- called by -----------
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE Q4VIS2(PM,OFF,RHO,
     .                  Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .                  VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,
     .                  PY1,PY2,PZ1,PZ2,
     .                  T11,T12,T13,T14,T21,T22,T23,T24,
     .                  AREA,CXX)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   PM(NPROPM,*),OFF(*), RHO(*),
     .   Y1(*),Y2(*),Y3(*),Y4(*),Z1(*),Z2(*),Z3(*),Z4(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VZ1(*), VZ2(*), VZ3(*),
     .   PY1(*), PY2(*), PZ1(*), PZ2(*),
     .   T11(*), T12(*), T13(*), T14(*), T21(*), T22(*), T23(*), T24(*),
     .   VZ4(*), AREA(*), CXX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
      my_real
     .   CAQ(MVSIZ),FCL(MVSIZ) , 
     .   G11(MVSIZ) , G21(MVSIZ) , G31(MVSIZ) , G41(MVSIZ),
     .   HGY(MVSIZ), HGZ(MVSIZ), 
     .   HY,HZ,FAC,PX1H1,PX2H1, ARE
C-----------------------------------------------
C 
c      will use CAQ if necessary CAQ=0.001 (Quad BELYTSCHKO CAQ=0.1)
C
      DO I=LFT,LLT
       FAC = EM03*OFF(I)
       ARE = MAX(AREA(I),EM20)
       FCL(I)=HALF*FAC*RHO(I)*SQRT(ARE)*CXX(I)
      END DO 
C
C
C HOURGLASS BELYTSCHKO
C
        DO I=LFT,LLT
         HY=Y1(I)-Y2(I)+Y3(I)-Y4(I)
         HZ=Z1(I)-Z2(I)+Z3(I)-Z4(I)
         FAC=ONE/MAX(EM20,AREA(I))
         PX1H1=FAC*(PY1(I)*HY+PZ1(I)*HZ)
         PX2H1=FAC*(PY2(I)*HY+PZ2(I)*HZ)
         G11(I)= ONE -PX1H1
         G21(I)=-ONE -PX2H1
         G31(I)= ONE +PX1H1
         G41(I)=-ONE +PX2H1 
        END DO 
        DO I=LFT,LLT
         HGY(I)=FCL(I)*(
     &    G11(I)*VY1(I)+G21(I)*VY2(I)+G31(I)*VY3(I)+G41(I)*VY4(I))
         HGZ(I)=FCL(I)*(
     &    G11(I)*VZ1(I)+G21(I)*VZ2(I)+G31(I)*VZ3(I)+G41(I)*VZ4(I))
        END DO 
        DO I=LFT,LLT
         T11(I) =T11(I) - G11(I)*HGY(I)
         T12(I) =T12(I) - G21(I)*HGY(I)
         T13(I) =T13(I) - G31(I)*HGY(I)
         T14(I) =T14(I) - G41(I)*HGY(I)
         T21(I) =T21(I) - G11(I)*HGZ(I)
         T22(I) =T22(I) - G21(I)*HGZ(I)
         T23(I) =T23(I) - G31(I)*HGZ(I)
         T24(I) =T24(I) - G41(I)*HGZ(I)
        END DO 
C
      RETURN
      END
