Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    qforc2                 ../engine/source/elements/solid_2d/quad/qforc2.F
!||--- called by ------------------------------------------------------
!||    alemain                ../engine/source/ale/alemain.F
!||    forint                 ../engine/source/elements/forint.F
!||--- calls      -----------------------------------------------------
!||    alero2                 ../engine/source/ale/ale2d/alero2.F
!||    amomt2                 ../engine/source/ale/ale2d/amomt2.F
!||    check_off_ale          ../engine/source/elements/solid/solide/check_off_ale.F
!||    ede112                 ../engine/source/ale/euler2d/ede112.F
!||    edefo2                 ../engine/source/ale/euler2d/edefo2.F
!||    emomt2                 ../engine/source/ale/euler2d/emomt2.F
!||    eulro2                 ../engine/source/ale/euler2d/eulro2.F
!||    mmain                  ../engine/source/materials/mat_share/mmain.F90
!||    qbilan                 ../engine/source/elements/solid_2d/quad/qbilan.F
!||    qcoor2                 ../engine/source/elements/solid_2d/quad/qcoor2.F
!||    qcumu2                 ../engine/source/elements/solid_2d/quad/qcumu2.F
!||    qcumu2p                ../engine/source/elements/solid_2d/quad/qcumu2p.F
!||    qdefo2                 ../engine/source/elements/solid_2d/quad/qdefo2.F
!||    qdlen2                 ../engine/source/elements/solid_2d/quad/qdlen2.F
!||    qfint2                 ../engine/source/elements/solid_2d/quad/qfint2.F
!||    qhvis2                 ../engine/source/elements/solid_2d/quad/qhvis2.F
!||    qlagr2                 ../engine/source/elements/solid_2d/quad/qlagr2.F
!||    qmass2                 ../engine/source/elements/solid_2d/quad/qmass2.F
!||    qmass2p                ../engine/source/elements/solid_2d/quad/qmass2p.F
!||    qmassreal2             ../engine/source/elements/solid_2d/quad/qmassreal2.F
!||    qmassreal2p            ../engine/source/elements/solid_2d/quad/qmassreal2p.F
!||    qrcoor2                ../engine/source/elements/solid_2d/quad/qrcoor2.F
!||    qrdefo2                ../engine/source/elements/solid_2d/quad/qrdefo2.F
!||    qrota2                 ../engine/source/elements/solid_2d/quad/qrota2.F
!||    qrrota2                ../engine/source/elements/solid_2d/quad/qrrota2.F
!||    qvolu2                 ../engine/source/elements/solid_2d/quad/qvolu2.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod   ../common_source/modules/ale/ale_connectivity_mod.F
!||    ale_mod                ../common_source/modules/ale/ale_mod.F
!||    dt_mod                 ../engine/source/modules/dt_mod.F
!||    elbufdef_mod           ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    glob_therm_mod         ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    mat_elem_mod           ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    mmain_mod              ../engine/source/materials/mat_share/mmain.F90
!||    nlocal_reg_mod         ../common_source/modules/nlocal_reg_mod.F
!||    sensor_mod             ../common_source/modules/sensor_mod.F90
!||    table_mod              ../engine/share/modules/table_mod.F
!||    timer_mod              ../engine/source/system/timer_mod.F90
!||====================================================================
      SUBROUTINE QFORC2(TIMERS, ELBUF_TAB  ,NG          ,
     1                  PM         ,GEO         ,IC          ,X        ,A           ,
     2                  V          ,MS          ,W           ,FLUX     ,FLU1        ,
     3                  VEUL       ,ALE_CONNECT ,IPARG       ,NLOC_DMG ,
     4                  TF         ,NPF         ,BUFMAT      ,PARTSAV  ,
     5                  DT2T       ,NELTST      ,ITYPTST     ,STIFN    ,OFFSET      ,
     6                  EANI       ,IPARTQ      ,NEL         ,IADQ     ,FSKY        ,
     9                  IPM        ,BUFVOIS     ,QMV         ,
     A                  GRESAV     ,GRTH        ,IGRTH       ,TABLE    ,IGEO        ,
     B                  VOLN       ,ITASK       ,MS_2D       ,FSKYM    ,IOUTPRT     ,
     C                  MAT_ELEM   ,H3D_STRAIN  ,SZ_BUFVOIS  ,SNPC     ,STF         ,SBUFMAT ,
     D                  SVIS       ,NSVOIS      ,IRESP       ,TT       ,DT1         ,
     .                  IDEL7NOK   ,IDTMIN      ,MAXFUNC     ,
     .                  IMON_MAT   ,USERL_AVAIL ,impl_s   ,idyna       ,
     .                  DT         ,GLOB_THERM,SENSORS)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
c FUNCTION: Internal force compute of Quad element
c
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   PM ,GEO             Material and geometrical property data
c  I   IC(7,NUM_QUAD)      connectivity and mid,pid integer data
c  I   X(3,NUMNOD)         co-ordinate
c  IO  A(3,NUMNOD)         nodal internal force
c  I   V(3,NUMNOD)         nodal velocity
c  IO  MS(NUMNOD)          nodal masse
c  I   EV()                internal element(material) data
c  I   FLUX(4,NEL)         flux at each side used w/ ALE or EULER
c  I   FLU1  ,VEUL  ,IELVS used w/ ALE or EULER
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
c  I   IADQ() ,FSKY()      work arrays for special option of internal force assemlage
c  IO  XPHI,FPHI,VPHI,MSPHI,PV ,
c      X0PHI ,EVD : variables for sensibility(opt) no more used
c  I   BUFVOIS()           work table for fluide w/ SPMD
c  I   QMV(8,)             work table used w/ ALE or EULER
c  I   GRESAV,GRTH,IGRTH   work table used for TH (time history) output
c  I   TABLE               new alternative Radioss function(table) data
c  I   IGEO                geometrical property integer data
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TIMER_MOD
      USE MMAIN_MOD
      USE TABLE_MOD
      USE MAT_ELEM_MOD
      USE NLOCAL_REG_MOD
      USE ALE_CONNECTIVITY_MOD
      USE ALE_MOD , ONLY : ALE
      USE ELBUFDEF_MOD
      USE DT_MOD
      use glob_therm_mod
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TIMER_), INTENT(INOUT) :: TIMERS
      my_real, INTENT(IN) :: DT1
      my_real, INTENT(IN) :: TT
      INTEGER,INTENT(IN) :: SNPC
      INTEGER,INTENT(IN) :: STF
      INTEGER, INTENT(IN) :: SBUFMAT
      INTEGER, INTENT(IN) :: NSVOIS
      INTEGER ,INTENT(IN) :: IRESP
      INTEGER ,INTENT(INOUT) :: IDEL7NOK
      integer,dimension(102) :: IDTMIN
      INTEGER ,INTENT(IN) :: MAXFUNC
      INTEGER, INTENT(IN) :: IMPL_S
      INTEGER, INTENT(IN) :: IDYNA
      INTEGER, INTENT(IN) :: USERL_AVAIL
      INTEGER, INTENT(IN) :: IMON_MAT
      INTEGER IC(*), IPARG(NPARG,NGROUP), NPF(*),IPARTQ(NUMELQ),
     .        IPM(*), GRTH(*),IGRTH(*),IGEO(*), IADQ(4,*), ITASK
      INTEGER, INTENT(IN) :: SZ_BUFVOIS
      INTEGER OFFSET,NEL,NG,NELTST,ITYPTST,IOUTPRT,H3D_STRAIN
      my_real DT2T
      my_real PM(*), GEO(*), X(*), A(*), V(*), MS(*), W(*), FLUX(4,*),
     .        FLU1(*), VEUL(*), TF(*), BUFMAT(*), FSKY(*),
     .        PARTSAV(*), STIFN(*),EANI(*), BUFVOIS(6,*),QMV(8,*),GRESAV(*),VOLN(MVSIZ),
     .        MS_2D(*),FSKYM(*)
      my_real, DIMENSION(MVSIZ,6), INTENT(INOUT) :: SVIS
      TYPE(TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_)  , TARGET :: NLOC_DMG
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE (MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM
      TYPE (DT_), INTENT(IN) :: DT
      type (glob_therm_) ,intent(inout)   :: glob_therm
      type (sensors_),INTENT(INOUT) :: SENSORS
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LCO, NF1, IFLAG,I,IPTR,IPTS,IPTT,ILAY,ISTRAIN
      INTEGER IBIDON(1),IBID,SZ_IX,DUMMY_IPARG1(NPARG)

      my_real RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),SX(MVSIZ),SY(MVSIZ),SZ(MVSIZ),TX(MVSIZ),TY(MVSIZ),TZ(MVSIZ)

      INTEGER, DIMENSION(MVSIZ) :: MAT,NC1,NC2,NC3,NC4,NGL,NGEO
      INTEGER :: NUM_ELEM_ALE
      my_real, DIMENSION(MVSIZ) :: F11, F12, F21, F22
      my_real, DIMENSION(MVSIZ) :: AX1, AX2
      my_real, DIMENSION(MVSIZ) :: T11, T12, T13, T14, T21, T22, T23, T24
      my_real, DIMENSION(MVSIZ) :: Y1, Y2, Y3, Y4, Z1, Z2, Z3, Z4
      my_real, DIMENSION(MVSIZ) :: VY1, VY2, VY3, VY4, VZ1, VZ2, VZ3, VZ4
      my_real, DIMENSION(MVSIZ) :: PY1, PY2, PZ1, PZ2
      my_real, DIMENSION(MVSIZ) :: AIRE,AIREM,QVIS,VIS,WYZ
      my_real, DIMENSION(MVSIZ) :: S1,S2,S3,S4,S5,S6
      my_real, DIMENSION(MVSIZ) :: VD2,DVOL,DELTAX
      my_real, DIMENSION(MVSIZ) :: DYZ,DZY,SSP
      my_real, DIMENSION(MVSIZ) :: EYY,EZZ,ETT,EYZ,EYT,EZT
      my_real, DIMENSION(MVSIZ) :: VDY, VDZ
      my_real EHOU(MVSIZ),SSP_EQ(MVSIZ)
      my_real WYY(MVSIZ),WZZ(MVSIZ),VDX(MVSIZ)

      ! SPH case
      my_real MUVOID(MVSIZ), STI(MVSIZ),BID(MVSIZ), MBID(MVSIZ)
      ! void MMAIN
      my_real SIGY(MVSIZ),ET(MVSIZ),GAMA(MVSIZ,6),R3_FREE(MVSIZ),R4_FREE(MVSIZ)
      my_real  R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .         R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .         R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .         Y234(MVSIZ),Y124(MVSIZ),BIDM(MVSIZ)
      my_real VARNL(NEL)
      my_real, DIMENSION(:), POINTER :: EINT

      ! ale grid formulation 7 (flow-tracking)
      my_real :: ELEM_MASS
      my_real :: SUM_EPS(9),SUM_M,SUM_VOL

      TYPE(G_BUFEL_) ,POINTER :: GBUF
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      GBUF => ELBUF_TAB(NG)%GBUF
c
      IBIDON=0
      IBID = 0
      SZ_IX=NUMELQ+NUMELS+NSVOIS    ! Size of IX array (either IXS+NSVOIS or IXQ)
      BIDM(1:MVSIZ) = ZERO
      MBID(1:MVSIZ) = ZERO
      BID(:) = ZERO
      DO I=1,NEL
        WYY(I)=ZERO
        WZZ(I)=ZERO
        VDX(I)=ZERO
      ENDDO
C
      IF (ISORTH == 0) THEN
C       propriete isotrope
        DO I=1,NEL
          GAMA(I,1) = ONE
          GAMA(I,2) = ZERO
          GAMA(I,3) = ZERO
          GAMA(I,4) = ZERO
          GAMA(I,5) = ONE
          GAMA(I,6) = ZERO
        ENDDO
      ELSE
        DO I=1,NEL
          GAMA(I,1) = GBUF%GAMA(I        )
          GAMA(I,2) = GBUF%GAMA(I +   NEL)
          GAMA(I,3) = GBUF%GAMA(I + 2*NEL)
          GAMA(I,4) = GBUF%GAMA(I + 3*NEL)
          GAMA(I,5) = GBUF%GAMA(I + 4*NEL)
          GAMA(I,6) = GBUF%GAMA(I + 5*NEL)
        ENDDO
      ENDIF
      ISTRAIN = IPARG(44,NG)
C
      LCO=1+7*NFT
      NF1=NFT+1
C
      IF (JCVT == 0)THEN
        CALL QCOOR2(
     1   X,       IC(LCO), Y1,      Y2,
     2   Y3,      Y4,      Z1,      Z2,
     3   Z3,      Z4,      NC1,     NC2,
     4   NC3,     NC4,     NGL,     MAT,
     5   NGEO,    VD2,     VIS,     NEL)
      ELSE
C------ Co-rotational system (convective local system)
C       JCVT/=0 => JLAG/=0
        CALL QRCOOR2(
     1   X,       IC(LCO), Y1,      Y2,
     2   Y3,      Y4,      Z1,      Z2,
     3   Z3,      Z4,      NC1,     NC2,
     4   NC3,     NC4,     NGL,     MAT,
     5   NGEO,    VD2,     R11,     R12,
     6   R13,     R21,     R22,     R23,
     7   R31,     R32,     R33,     GAMA,
     8   Y234,    Y124,    VIS,     NEL,
     9   ISORTH)
      END IF
c
      IF (JLAG/=0) THEN
C--------------
C      LAGRANGE, VOLUME and CHARACTERISTIC length (for DT) compute
C--------------
        CALL QVOLU2(
     1   GBUF%OFF,AIRE,    VOLN,    NGL,
     2   Y1,      Y2,      Y3,      Y4,
     3   Z1,      Z2,      Z3,      Z4,
     4   Y234,    Y124,    NEL,     JMULT,
     5   JCVT)
        CALL QDLEN2(Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,AIRE,DELTAX,IPARG(63,NG))
        IF (JCVT == 0) THEN
          CALL QDEFO2(
     1   V,       V,       Y1,      Y2,
     2   Y3,      Y4,      Z1,      Z2,
     3   Z3,      Z4,      VY1,     VY2,
     4   VY3,     VY4,     VZ1,     VZ2,
     5   VZ3,     VZ4,     PY1,     PY2,
     6   PZ1,     PZ2,     WYZ,     DYZ,
     7   DZY,     EYY,     EZZ,     ETT,
     8   EYZ,     EYT,     EZT,     RX,
     9   RY,      RZ,      SX,      SY,
     A   SZ,      TX,      TY,      TZ,
     B   VOLN,    AIRE,    AIREM,   NC1,
     C   NC2,     NC3,     NC4,     NEL)
        ELSE
          CALL QRDEFO2(
     1   V,       Y1,      Y2,      Y3,
     2   Y4,      Z1,      Z2,      Z3,
     3   Z4,      VY1,     VY2,     VY3,
     4   VY4,     VZ1,     VZ2,     VZ3,
     5   VZ4,     PY1,     PY2,     PZ1,
     6   PZ2,     WYZ,     DYZ,     DZY,
     7   EYY,     EZZ,     ETT,     EYZ,
     8   EYT,     EZT,     RX,      RY,
     9   RZ,      SX,      SY,      SZ,
     A   TX,      TY,      TZ,      VOLN,
     B   AIRE,    AIREM,   NC1,     NC2,
     C   NC3,     NC4,     R22,     R23,
     D   R32,     R33,     NEL,     JCVT)
        END IF
c
        CALL QLAGR2(
     1   PM,       GBUF%VOL, GBUF%RHO, GBUF%EINT,
     2   VOLN,     DVOL,     MAT,      NEL)
        CALL QROTA2(
     1   GBUF%SIG,S1,      S2,      S3,
     2   S4,      S5,      S6,      WYZ,
     3   NEL,     JCVT)
      ELSEIF (JALE/=0) THEN
C------------
C      A.L.E.
C------------
        CALL QVOLU2(
     1   GBUF%OFF,AIRE,    VOLN,    NGL,
     2   Y1,      Y2,      Y3,      Y4,
     3   Z1,      Z2,      Z3,      Z4,
     4   BID,     BID,     NEL,     JMULT,
     5   JCVT)
        CALL QDLEN2(Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,AIRE,DELTAX,IPARG(63,NG))
        CALL QDEFO2(
     1   V,       W,       Y1,      Y2,
     2   Y3,      Y4,      Z1,      Z2,
     3   Z3,      Z4,      VY1,     VY2,
     4   VY3,     VY4,     VZ1,     VZ2,
     5   VZ3,     VZ4,     PY1,     PY2,
     6   PZ1,     PZ2,     WYZ,     DYZ,
     7   DZY,     EYY,     EZZ,     ETT,
     8   EYZ,     EYT,     EZT,     RX,
     9   RY,      RZ,      SX,      SY,
     A   SZ,      TX,      TY,      TZ,
     B   VOLN,    AIRE,    AIREM,   NC1,
     C   NC2,     NC3,     NC4,     NEL)
        CALL ALERO2(
     1   GBUF%OFF,   GBUF%VOL,   GBUF%RHO,   FLUX(1,NF1),
     2   FLU1(NF1),  W,          VY1,        VY2,
     3   VY3,        VY4,        VZ1,        VZ2,
     4   VZ3,        VZ4,        VOLN,       DVOL,
     5   VD2,        NC1,        NC2,        NC3,
     6   NC4,        NGL)
        CALL QROTA2(
     1   GBUF%SIG,S1,      S2,      S3,
     2   S4,      S5,      S6,      WYZ,
     3   NEL,     JCVT)
      ELSEIF (JEUL/=0) THEN
C-----------
C      EULER
C-----------
        IF (MTN == 11) CALL EDE112(
     1   PM,         V,          X,
     2   IC,         ALE_CONNECT,WYZ,        DYZ,
     3   DZY,        EYY,        EZZ)
        CALL EDEFO2(
     1   GBUF%VOL,V,       VEUL,    Y1,
     2   Y2,      Y3,      Y4,      Z1,
     3   Z2,      Z3,      Z4,      VY1,
     4   VY2,     VY3,     VY4,     VZ1,
     5   VZ2,     VZ3,     VZ4,     PY1,
     6   PY2,     PZ1,     PZ2,     WYZ,
     7   DYZ,     DZY,     EYY,     EZZ,
     8   ETT,     EYZ,     EYT,     EZT,
     9   VOLN,    AIRE,    DELTAX,  VDY,
     A   VDZ,     VD2,     NC1,     NC2,
     B   NC3,     NC4)
        CALL EULRO2(
     1   GBUF%RHO,   FLUX(1,NF1),FLU1(NF1),
     2   VOLN,       DVOL,       NGL)
        CALL QROTA2(
     1   GBUF%SIG,S1,      S2,      S3,
     2   S4,      S5,      S6,      WYZ,
     3   NEL,     JCVT)
      ENDIF
C-----------------------------------------------------
C     STRESS CALCULATION (Constitutive laws)
C-----------------------------------------------------
C SPMD + FLUID : BUFVOIS LOI11
c
      ILAY = 1
      IPTR = 1
      IPTS = 1
      IPTT = 1
c
      CALL MMAIN(TIMERS,
     1   ELBUF_TAB,   NG,          PM,          GEO,
     2                ALE_CONNECT, IC,          IPARG,
     3   V,           TF,          NPF,         BUFMAT,
     4   STI,         X,           DT2T,        NELTST,
     5   ITYPTST,     OFFSET,      NEL,         W,
     6   GBUF%OFF,    NGEO,        MAT,         NGL,
     7   VOLN,        VD2,         DVOL,        DELTAX,
     8   VIS,         QVIS,        SSP,         S1,
     9   S2,          S3,          S4,          S5,
     A   S6,          EYY,         EZZ,         ETT,
     B   EYZ,         EYT,         EZT,         WYY,
     C   WZZ,         WYZ,         RX,          RY,
     D   RZ,          SX,          SY,          SZ,
     E   VDX,         VDY,         VDZ,         MUVOID,
     F   SSP_EQ,      AIRE,        SIGY,        ET,
     G   BUFVOIS,     GBUF%PLA,    R3_FREE,     R4_FREE,
     H   EYY,         EZZ,         ETT,         EYZ,
     I   EYT,         EZT,         WYY,         WZZ,
     J   WYZ,         IPM,         GAMA,        BID,
     K   MBID,        MBID,        MBID,        MBID,
     L   BID,         BID,         ISTRAIN,     BID,
     M   BID,         IBIDON(1),   ILAY,        MBID,
     N   MBID,        IPTR,        IPTS,        IPTT,
     O   TABLE,       BID,         BID,         BID,
     P   BID,         DUMMY_IPARG1,IGEO,        BID,
     Q   ITASK,       NLOC_DMG,    VARNL,       MAT_ELEM,
     R   H3D_STRAIN,  JPLASOL,     JSPH,        SZ_BUFVOIS,
     S   SNPC,        STF,         SBUFMAT,     GLOB_THERM,
     T   SVIS,        SZ_IX,       IRESP,
     U   N2D,         th_strain,   NGROUP,      TT, 
     .   DT1,         NTABLE,      NUMELQ,      nummat,
     .   numgeo,      numnod,      numels,            
     .   IDEL7NOK,    IDTMIN,      MAXFUNC,
     .   IMON_MAT,    USERL_AVAIL, impl_s,
     .   idyna,       DT         , BID      ,SENSORS)
C--------------------------
C       SYNTHESIS PER MATERIAL (thermics)
C--------------------------
      IF(JLAG+JALE+JEUL == 0)THEN
        IFLAG=MOD(NCYCLE,NCPRI)
        IF(IOUTPRT>0)THEN
c
           IF (MTN == 11) THEN
             EINT => ELBUF_TAB(NG)%GBUF%EINS(1:NEL)
           ELSE
             EINT => ELBUF_TAB(NG)%GBUF%EINT(1:NEL)
           ENDIF
          CALL QBILAN(
     1   PARTSAV,    GBUF%OFF,   EINT,       GBUF%RHO,
     2   GBUF%RK,    GBUF%VOL,   VY1,        VY2,
     3   VY3,        VY4,        VZ1,        VZ2,
     4   VZ3,        VZ4,        VOLN,       IPARTQ,
     5   EHOU,       R22,        R23,        R32,
     6   R33,        GRESAV,     GRTH,       IGRTH,
     7   IBIDON(1),  GBUF%EINTTH,ITASK,      NEL,
     8   JTUR,       JCVT,       IGRE,       SENSORS,
     9   GBUF%G_WPLA,GBUF%WPLA)
        ENDIF
        RETURN
      ENDIF
C----------------------------------
C     PETROV-GALERKIN PSEUDO MASSES & ALE MASSES
C----------------------------------
      IF (IPARIT == 0)THEN
       CALL QMASS2(
     1   GBUF%OFF,GBUF%RHO,MS,      AIRE,
     2   NC1,     NC2,     NC3,     NC4,
     3   NEL)
      ELSE
       CALL QMASS2P(
     1   GBUF%OFF,GBUF%RHO,AIRE,    FSKY,
     2   FSKY,    IADQ,    NEL,     NFT)
      ENDIF
C--------------------------
C     UPDATE OF MASSES : ALE physical masses
C----------------------------
      IF (JALE+JEUL > 0 )THEN
         IF (IPARIT == 0)THEN
          CALL QMASSREAL2(
     1   GBUF%OFF,GBUF%RHO,MS_2D,   VOLN,
     2   NC1,     NC2,     NC3,     NC4,
     3   NEL)
         ELSE
          CALL QMASSREAL2P(
     1   GBUF%OFF,GBUF%RHO,VOLN,    FSKYM,
     2   IADQ,    NEL,     NFT)
         ENDIF
      ENDIF

C---------------------------------------------
C /ALE/GRID/FLOW-TRACKING - BUFFER UPDATED
C---------------------------------------------
      ! this grid formulation needs an averaged tensor (E=GRAD U)
      IF(ALE%GRID%NWALE == 7 .AND. JALE == 1)THEN
        SUM_EPS(1:9) = ZERO
        SUM_M = ZERO
        SUM_VOL = ZERO
        NUM_ELEM_ALE = NEL
        DO I=1,NEL
          ELEM_MASS = GBUF%RHO(I)*GBUF%VOL(I)
          SUM_EPS(2) = SUM_EPS(2) + ELEM_MASS*EYY(I)
          SUM_EPS(3) = SUM_EPS(3) + ELEM_MASS*EZZ(I)
          SUM_EPS(6) = SUM_EPS(6) + ELEM_MASS*DYZ(I)
          SUM_EPS(9) = SUM_EPS(9) + ELEM_MASS*DZY(I)
          SUM_M = SUM_M + ELEM_MASS
          SUM_VOL = SUM_VOL + GBUF%VOL(I)
        ENDDO
#include "lockon.inc"
        ALE%GRID%flow_tracking_data%EP(2) = ALE%GRID%flow_tracking_data%EP(2) + SUM_EPS(2)
        ALE%GRID%flow_tracking_data%EP(3) = ALE%GRID%flow_tracking_data%EP(3) + SUM_EPS(3)
        ALE%GRID%flow_tracking_data%EP(4) = ALE%GRID%flow_tracking_data%EP(4) + SUM_EPS(4)
        ALE%GRID%flow_tracking_data%EP(6) = ALE%GRID%flow_tracking_data%EP(6) + SUM_EPS(6)
        ALE%GRID%flow_tracking_data%EP(9) = ALE%GRID%flow_tracking_data%EP(9) + SUM_EPS(9)
        ALE%GRID%flow_tracking_data%SUM_M = ALE%GRID%flow_tracking_data%SUM_M + SUM_M
        ALE%GRID%flow_tracking_data%SUM_VOL = ALE%GRID%flow_tracking_data%SUM_VOL + SUM_VOL
        ALE%GRID%flow_tracking_data%NUM_ELEM_ALE = ALE%GRID%flow_tracking_data%NUM_ELEM_ALE + NUM_ELEM_ALE
#include "lockoff.inc"
      ENDIF!(ALE%GRID%NWALE == 7)


C------------------------
C     FORCES ANTI SABLIER (HOURGLASS CONTROL)
C------------------------
      CALL QHVIS2(PM,GBUF%OFF,GBUF%RHO,
     .                  Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .                  VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,
     .                  PY1,PY2,PZ1,PZ2,
     .                  T11,T12,T13,T14,T21,T22,T23,T24, 
     .                  AIRE,SSP,MAT,VD2,VIS,EANI,NGEO,GEO,
     .                  PARTSAV,IPARTQ,EHOU,IPARG(63,NG))
C--------------------------
C     BILANS PAR MATERIAU (output Result summary)
C--------------------------
      IFLAG=MOD(NCYCLE,NCPRI)
      IF(IOUTPRT>0)THEN
c
        IF (MTN == 11) THEN                                      
          EINT => ELBUF_TAB(NG)%GBUF%EINS(1:NEL)                     
        ELSE                                                     
          EINT => ELBUF_TAB(NG)%GBUF%EINT(1:NEL)                     
        ENDIF                                                    
        CALL QBILAN(
     1   PARTSAV,    GBUF%OFF,   EINT,       GBUF%RHO,
     2   GBUF%RK,    GBUF%VOL,   VY1,        VY2,
     3   VY3,        VY4,        VZ1,        VZ2,
     4   VZ3,        VZ4,        VOLN,       IPARTQ,
     5   EHOU,       R22,        R23,        R32,
     6   R33,        GRESAV,     GRTH,       IGRTH,
     7   IBIDON(1),  GBUF%EINTTH,ITASK,      NEL,
     8   JTUR,       JCVT,       IGRE,       SENSORS,
     9   GBUF%G_WPLA,GBUF%WPLA)
      ENDIF
C
C------------------------
C     FORCES DE TRANSPORT
C------------------------
       IF(JALE > 0 .AND. MTN /= 11)THEN
         CALL AMOMT2(
     1   PM,      W,       GBUF%RHO,
     2   Y1,      Y2,      Y3,      Y4,
     3   Z1,      Z2,      Z3,      Z4,
     4   T11,     T12,     T13,     T14,
     5   T21,     T22,     T23,     T24,
     6   PY1,     PY2,     PZ1,     PZ2,
     7   AIREM,   VY1,     VY2,     VY3,
     8   VY4,     VZ1,     VZ2,     VZ3,
     9   VZ4,     EYY,     EZZ,     DYZ,
     A   DZY,     NC1,     NC2,     NC3,
     B   NC4,     MAT,     GBUF%OFF,QMV,
     C   BUFMAT,  DELTAX,  VIS, IPM)
       ENDIF

       IF(JEUL > 0)THEN
        CALL EMOMT2(
     1   PM,      GBUF%RHO,Y1,      Y2,
     2   Y3,      Y4,      Z1,      Z2,
     3   Z3,      Z4,      VY1,     VY2,
     4   VY3,     VY4,     VZ1,     VZ2,
     5   VZ3,     VZ4,     T11,     T12,
     6   T13,     T14,     T21,     T22,
     7   T23,     T24,     PY1,     PY2,
     8   PZ1,     PZ2,     AIRE,    EYY,
     9   EZZ,     DYZ,     DZY,     VDY,
     A   VDZ,     DELTAX,  VIS,
     C   MAT,     QMV,     BUFMAT,  IPM)
       ENDIF
C--------------------
C     FORCES INTERNES
C--------------------
      CALL QFINT2(
     1   GBUF%SIG,PY1,     PY2,     PZ1,
     2   PZ2,     AIRE,    VOLN,    QVIS,
     3   F11,     F12,     F21,     F22,
     4   AX1,     AX2,     R22,     R23,
     5   R32,     R33,     NEL,     JCVT,
     6   SVIS)
C--------------
      IF(JCVT/=0)THEN
        CALL QRROTA2(
     1   R22,     R32,     R23,     R33,
     2   F11,     F21,     F12,     F22,
     3   T11,     T21,     T12,     T22,
     4   T13,     T23,     T14,     T24,
     5   NEL)
      END IF
        IF(JEUL+JALE/=0) CALL CHECK_OFF_ALE(T11 ,T21 ,T12 ,T22 ,T13 ,
     1                                      T23 ,T14 ,T24 ,BIDM,BIDM,
     2                                      BIDM,BIDM,BIDM,BIDM,BIDM,
     3                                      BIDM,BIDM,BIDM,BIDM,BIDM,
     4                                      BIDM,BIDM,BIDM,BIDM,GBUF%OFF,
     5                                      1,NEL,NEL)
C--------------
C     ASSEMBLE
C--------------
      IF(IPARIT == 0)THEN
        CALL QCUMU2(
     1   A,       F11,     F12,     F21,
     2   F22,     AX1,     AX2,     T11,
     3   T12,     T13,     T14,     T21,
     4   T22,     T23,     T24,     NC1,
     5   NC2,     NC3,     NC4,     STI,
     6   STIFN,   NEL)
      ELSE
        CALL QCUMU2P(
     1   F11,     F12,     F21,     F22,
     2   AX1,     AX2,     T11,     T12,
     3   T13,     T14,     T21,     T22,
     4   T23,     T24,     FSKY,    FSKY,
     5   IADQ,    STI,     NEL,     NFT)
      ENDIF
c-----------
      RETURN
      END
