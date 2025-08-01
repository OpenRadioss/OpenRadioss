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
!||    bforc2                 ../engine/source/ale/bimat/bforc2.F
!||--- called by ------------------------------------------------------
!||    alemain                ../engine/source/ale/alemain.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../engine/source/output/message/message.F
!||    arret                  ../engine/source/system/arret.F
!||    bafil2                 ../engine/source/ale/bimat/bafil2.F
!||    balph2                 ../engine/source/ale/bimat/balph2.F
!||    bamom2                 ../engine/source/ale/bimat/bamom2.F
!||    bcumu2                 ../engine/source/ale/bimat/bcumu2.F
!||    bcumu2p                ../engine/source/ale/bimat/bcumu2.F
!||    bcumu2pa               ../engine/source/ale/bimat/bcumu2.F
!||    befil2                 ../engine/source/ale/bimat/befil2.F
!||    bemom2                 ../engine/source/ale/bimat/bemom2.F
!||    blero2                 ../engine/source/ale/bimat/blero2.F
!||    brest2                 ../engine/source/ale/bimat/brest2.F
!||    check_off_ale          ../engine/source/elements/solid/solide/check_off_ale.F
!||    edefo2                 ../engine/source/ale/euler2d/edefo2.F
!||    mmain                  ../engine/source/materials/mat_share/mmain.F90
!||    qbilan                 ../engine/source/elements/solid_2d/quad/qbilan.F
!||    qcoor2                 ../engine/source/elements/solid_2d/quad/qcoor2.F
!||    qdefo2                 ../engine/source/elements/solid_2d/quad/qdefo2.F
!||    qdlen2                 ../engine/source/elements/solid_2d/quad/qdlen2.F
!||    qfint2                 ../engine/source/elements/solid_2d/quad/qfint2.F
!||    qhvis2                 ../engine/source/elements/solid_2d/quad/qhvis2.F
!||    qmass2                 ../engine/source/elements/solid_2d/quad/qmass2.F
!||    qmass2ap               ../engine/source/elements/solid_2d/quad/qmass2ap.F
!||    qmass2p                ../engine/source/elements/solid_2d/quad/qmass2p.F
!||    qmassreal2             ../engine/source/elements/solid_2d/quad/qmassreal2.F
!||    qmassreal2ap           ../engine/source/elements/solid_2d/quad/qmassreal2ap.F
!||    qmassreal2p            ../engine/source/elements/solid_2d/quad/qmassreal2p.F
!||    qrota2                 ../engine/source/elements/solid_2d/quad/qrota2.F
!||    qvolu2                 ../engine/source/elements/solid_2d/quad/qvolu2.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod   ../common_source/modules/ale/ale_connectivity_mod.F
!||    dt_mod                 ../engine/source/modules/dt_mod.F
!||    elbufdef_mod           ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    glob_therm_mod         ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    mat_elem_mod           ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    message_mod            ../engine/share/message_module/message_mod.F
!||    mmain_mod              ../engine/source/materials/mat_share/mmain.F90
!||    nlocal_reg_mod         ../common_source/modules/nlocal_reg_mod.F
!||    output_mod             ../common_source/modules/output/output_mod.F90
!||    sensor_mod             ../common_source/modules/sensor_mod.F90
!||    table_mod              ../engine/share/modules/table_mod.F
!||    timer_mod              ../engine/source/system/timer_mod.F90
!||====================================================================
      SUBROUTINE BFORC2(TIMERS, ELBUF_TAB   ,NG        ,
     1                   PM         ,GEO       ,IC         ,X       ,
     2                   A          ,V         ,MS         ,W       ,FLUX    ,
     3                   FLU1       ,VEUL      ,ALE_CONNECT,IPARG   ,
     4                   JPARG      ,FILL      ,DFILL      ,IMS     ,NLOC_DMG,
     5                   TF         ,NPF       ,BUFMAT     ,PARTSAV ,
     6                   DT2T       ,NELTST    ,ITYPTST    ,STIFN   ,OFFSET  ,
     7                   EANI       ,IPARTQ    ,NEL        ,IADQ    ,FSKY    ,
     8                   IPM        ,BUFVOIS   ,
     9                   GRESAV     ,GRTH      ,IGRTH      ,TABLE  ,IGEO     ,
     A                   VOLN       ,ITASK     ,MS_2D      ,FSKYM  ,MAT_ELEM ,
     B                   H3D_STRAIN ,OUTPUT    ,SZ_BUFVOIS ,SNPC   ,STF      ,SBUFMAT, SVIS,
     C                   NSVOIS     , IRESP    ,IDEL7NOK   ,
     D                   IDTMIN     , MAXFUNC  ,IMON_MAT   ,
     E                   USERL_AVAIL, impl_s     ,idyna  ,DT       ,
     F                   GLOB_THERM,SENSORS)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE TIMER_MOD
      USE MMAIN_MOD
      USE TABLE_MOD
      USE MESSAGE_MOD
      USE ELBUFDEF_MOD            
      USE NLOCAL_REG_MOD
      USE ALE_CONNECTIVITY_MOD
      USE OUTPUT_MOD
      USE MAT_ELEM_MOD
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
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr07_c.inc"
#include      "vect01_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TIMER_), INTENT(INOUT) :: TIMERS
      INTEGER OFFSET,NEL,NG,NSVOIS
      INTEGER,INTENT(IN) :: SZ_BUFVOIS
      INTEGER,INTENT(IN) :: SNPC
      INTEGER,INTENT(IN) :: STF
      INTEGER, INTENT(IN) :: SBUFMAT
      INTEGER, INTENT(IN) :: IRESP
      INTEGER ,INTENT(INOUT) :: IDEL7NOK
      integer,dimension(102) :: IDTMIN
      INTEGER ,INTENT(IN) :: MAXFUNC
      INTEGER, INTENT(IN) :: IMPL_S
      INTEGER, INTENT(IN) :: IDYNA
      INTEGER, INTENT(IN) :: USERL_AVAIL
      INTEGER, INTENT(IN) :: IMON_MAT
      INTEGER IC(*), IPARG(NPARG,NGROUP), JPARG(*), IMS(*), NPF(*),IPARTQ(NUMELQ),IPM(*),IGEO(*),ITASK,H3D_STRAIN
      INTEGER NELTST,ITYPTST, IADQ(4,*),GRTH(*),IGRTH(*)
      my_real DT2T
      my_real PM(*), GEO(*), X(*), A(*), V(*), MS(*), W(*), FLUX(4,*),
     .        FLU1(*), VEUL(*),FILL(NUMNOD,*),EANI(*),FSKY(*),
     .        DFILL(NUMNOD,*), TF(*), BUFMAT(*), PARTSAV(*), STIFN(*),
     .        BUFVOIS(6,*),GRESAV(*),VOLN(MVSIZ),MS_2D(*),FSKYM(*)
      my_real, DIMENSION(MVSIZ,6), INTENT(INOUT) :: SVIS
      TYPE (TTABLE) TABLE(*)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_)  , TARGET :: NLOC_DMG 
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE(OUTPUT_), INTENT(INOUT) :: OUTPUT !< output structure
      TYPE (MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM
      TYPE (DT_), INTENT(IN) :: DT
      type (glob_therm_) ,intent(inout)   :: glob_therm
      type (sensors_),INTENT(INOUT) :: SENSORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NF1,NF2, MTN1, MTN2, LCO, IMULT, IFLAG,IBID
      INTEGER MAT(MVSIZ),NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ),NC4(MVSIZ),
     .        NGL(MVSIZ),NGEO(MVSIZ), IBIDV(1) ,L_TEMP, L_PLA, L_BFRAC,L_BULK,SZ_IX
      INTEGER :: ILAY
      my_real Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .        Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .        AIRE(MVSIZ),AIRES(MVSIZ),AIREM(MVSIZ),VD2(MVSIZ),DVOL(MVSIZ),
     .        DELTAX(MVSIZ),VIS(MVSIZ),
     .        VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), VY4(MVSIZ), VZ1(MVSIZ),
     .        VZ2(MVSIZ), VZ3(MVSIZ), VZ4(MVSIZ),
     .        PY1(MVSIZ), PY2(MVSIZ), PZ1(MVSIZ), PZ2(MVSIZ),
     .        WYZ(MVSIZ),DYZ(MVSIZ),DZY(MVSIZ),QVIS(MVSIZ),SSP(MVSIZ),
     .        S1(MVSIZ),S2(MVSIZ),S3(MVSIZ),S4(MVSIZ),S5(MVSIZ),S6(MVSIZ),
     .        EYY(MVSIZ),EZZ(MVSIZ),ETT(MVSIZ),EYZ(MVSIZ),EYT(MVSIZ),
     .        EZT(MVSIZ),RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),SX(MVSIZ),SY(MVSIZ),
     .        SZ(MVSIZ), TX(MVSIZ), TY(MVSIZ), TZ(MVSIZ)
      my_real F11(MVSIZ), F12(MVSIZ), F21(MVSIZ), F22(MVSIZ),SSP_EQ(MVSIZ),
     .        T11(MVSIZ), T12(MVSIZ), T13(MVSIZ), T14(MVSIZ), T21(MVSIZ),
     .        T22(MVSIZ), T23(MVSIZ),T24(MVSIZ), VDY(MVSIZ), VDZ(MVSIZ),
     .        AX1(MVSIZ),AX2(MVSIZ), EHOU(MVSIZ)

      my_real B11(MVSIZ), B12(MVSIZ), B13(MVSIZ), B14(MVSIZ),B21(MVSIZ), B22(MVSIZ), B23(MVSIZ), B24(MVSIZ) ! Advection term ('force')
      my_real WYY(MVSIZ),WZZ(MVSIZ),VDX(MVSIZ)
      my_real MUVOID(MVSIZ), STI(MVSIZ),BID(MVSIZ), MBID(1)
      my_real SIGY(MVSIZ),ET(MVSIZ),GAMA(MVSIZ,6), R3_FREE(MVSIZ),R4_FREE(MVSIZ)
      my_real DALPH1(MVSIZ), DALPH2(MVSIZ)
      my_real EYYS(MVSIZ), EZZS(MVSIZ), ETTS(MVSIZ), EYZS(MVSIZ), EYTS(MVSIZ), EZTS(MVSIZ),BIDM(MVSIZ)
      my_real VARNL(NEL)
      INTEGER NPARG_BID(NPARG)
C-------------------
      TYPE(L_BUFEL_)  ,POINTER :: LBUF, LBUF1,LBUF2     
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      SZ_IX=NUMELQ+NUMELS+NSVOIS                   ! Size of IX array (either IXS+NSVOIS or IXQ)
      GBUF  => ELBUF_TAB(NG)%GBUF
      LBUF1 => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)  ! buffer mat 1
      LBUF2 => ELBUF_TAB(NG)%BUFLY(2)%LBUF(1,1,1)  ! buffer mat 2
      MTN1  = JPARG(25)
      MTN2  = JPARG(26)
      NF1   = NFT + 1
      NF2   = NF1+NUMELQ
      LCO   = NFT*7 + 1

      IBIDV=0
      IBID = 0
      BID = ZERO
      BIDM(1:MVSIZ) = ZERO

      IF (NSPMD > 1) THEN
        CALL ANCMSG(MSGID=14,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF

      DO I=1,NEL
        WYY(I)=ZERO
        WZZ(I)=ZERO
        VDX(I)=ZERO
      ENDDO
C------------
      CALL QCOOR2(
     1   X,       IC(LCO), Y1,      Y2,
     2   Y3,      Y4,      Z1,      Z2,
     3   Z3,      Z4,      NC1,     NC2,
     4   NC3,     NC4,     NGL,     MAT,
     5   NGEO,    VD2,     VIS,     NEL)
C------------
C      A.L.E.
C------------
      IF (JALE /= 0) THEN
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
       CALL BALPH2(PM,LBUF1%FRAC,LBUF2%FRAC,GBUF%VOL,FILL,
     .             LBUF1%SIG,LBUF1%EINT,LBUF1%VOL,LBUF1%RHO,
     .             FLUX(1,NF1),FLU1(NF1),LBUF1%OFF,
     .             LBUF2%SIG,LBUF2%EINT,LBUF2%VOL,LBUF2%RHO,
     .             FLUX(1,NF2),FLU1(NF2),LBUF2%OFF,
     .             GBUF%SIG,GBUF%EINT,GBUF%RHO,GBUF%TEMP,
     .             GBUF%G_TEMP,GBUF%BFRAC,GBUF%G_BFRAC,
     .             GBUF%PLA,GBUF%G_PLA,VOLN,GBUF%QVIS,GBUF%G_QVIS,NEL, 
     .             AIRE, AIRES, 
     .             EYY, EZZ, ETT, EYZ, EYT, EZT,
     .             EYYS, EZZS, ETTS, EYZS, EYTS, EZTS, 
     .             MAT, NC1, NC2, NC3, NC4, 
     .             DALPH1, DALPH2)
       CALL BAFIL2(V,W,FILL,DFILL,IMS,X, DALPH1, DALPH2, NC1, NC2, NC3, NC4)
C-----------
C      EULER
C-----------
      ELSEIF(JEUL /= 0)THEN
       CALL EDEFO2(GBUF%VOL,V,VEUL,
     .                  Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .                  VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,
     .                  PY1,PY2,PZ1,PZ2,
     .                  WYZ,DYZ,DZY,EYY,EZZ,ETT,EYZ,EYT,EZT,
     .                  VOLN,AIRE,DELTAX,VDY,VDZ,VD2,
     .                  NC1,NC2,NC3,NC4)
       CALL BALPH2(PM,LBUF1%FRAC,LBUF2%FRAC,GBUF%VOL,FILL,
     .             LBUF1%SIG,LBUF1%EINT,LBUF1%VOL,LBUF1%RHO,
     .             FLUX(1,NF1),FLU1(NF1),LBUF1%OFF,
     .             LBUF2%SIG,LBUF2%EINT,LBUF2%VOL,LBUF2%RHO,
     .             FLUX(1,NF2),FLU1(NF2),LBUF2%OFF,
     .             GBUF%SIG,GBUF%EINT,GBUF%RHO,GBUF%TEMP,
     .             GBUF%G_TEMP,GBUF%BFRAC,GBUF%G_BFRAC,
     .             GBUF%PLA,GBUF%G_PLA,VOLN,GBUF%QVIS,GBUF%G_QVIS,NEL, 
     .             AIRE, AIRES, 
     .             EYY, EZZ, ETT, EYZ, EYT, EZT,
     .             EYYS, EZZS, ETTS, EYZS, EYTS, EZTS, 
     .             MAT, NC1, NC2, NC3, NC4, 
     .             DALPH1, DALPH2)
       CALL BEFIL2(V,FILL,DFILL,IMS,X,
     .      DALPH1, DALPH2,
     .      NC1, NC2, NC3, NC4)
      ENDIF
C-----------------------------------------------------
C     SUBMATERIAL 1 (MAT LAW20)
C-----------------------------------------------------
      IMULT=JMULT
      JMULT=1
      ILAY = -1
      LBUF => ELBUF_TAB(NG)%BUFLY(JMULT)%LBUF(1,1,1)  ! buffer mat 1
      MTN=JPARG(25)
      L_TEMP = ELBUF_TAB(NG)%BUFLY(JMULT)%L_TEMP
      L_PLA  = ELBUF_TAB(NG)%BUFLY(JMULT)%L_PLA
      L_BFRAC= ELBUF_TAB(NG)%BUFLY(JMULT)%L_BFRAC 
      L_BULK = ELBUF_TAB(NG)%BUFLY(JMULT)%L_QVIS

      CALL BREST2(GBUF%VOL,IC(LCO),VOLN,
     .            AIRE, AIRES, 
     .            EYY, EZZ, ETT, EYZ, EYT, EZT,
     .            EYYS, EZZS, ETTS, EYZS, EYTS, EZTS,
     .            MAT,MAT_ELEM%MAT_PARAM, NUMMAT)
      CALL BLERO2(LBUF1%FRAC,LBUF1%VOL,LBUF1%RHO,
     .            FLUX(1,NF1),FLU1(NF1),LBUF1%OFF,PM,NGL,VOLN,
     .            AIRE, EYY, EZZ, ETT, EYZ, EYT, EZT, VD2, DVOL,
     .            VY1, VY2, VY3, VY4, 
     .            VZ1, VZ2, VZ3, VZ4,
     .            MAT)
      CALL QROTA2( LBUF1%SIG,S1,       S2,       S3,
     2             S4,       S5,       S6,       WYZ,
     3             NEL,      JCVT)

      IF (ISORTH == 0) THEN 
        !isotropic
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
          GAMA(I,1) = ONE                                
          GAMA(I,2) = ZERO                                  
          GAMA(I,3) = ZERO
          GAMA(I,4) = ZERO                                  
          GAMA(I,5) = ONE                                  
          GAMA(I,6) = ZERO 
        ENDDO
      ENDIF                                  
C-----------------------------------------------------
C     STRESS TENSOR
C-----------------------------------------------------
      CALL MMAIN(TIMERS,
     1   ELBUF_TAB,   NG,          PM,          GEO,
     2                ALE_CONNECT, IC,          IPARG,
     3   V,           TF,          NPF,         BUFMAT,
     4   STI,         X,           DT2T,        NELTST,
     5   ITYPTST,     OFFSET,      NEL,         W,
     6   LBUF%OFF,    NGEO,        MAT,         NGL,
     7   VOLN,        VD2,         DVOL,        DELTAX,
     8   VIS,         QVIS,        SSP,         S1,
     9   S2,          S3,          S4,          S5,
     A   S6,          EYY,         EZZ,         ETT,
     B   EYZ,         EYT,         EZT,         WYY,
     C   WZZ,         WYZ,         RX,          RY,
     D   RZ,          SX,          SY,          SZ,
     E   VDX,         VDY,         VDZ,         MUVOID,
     F   SSP_EQ,      AIRE,        SIGY,        ET,
     G   BUFVOIS,     LBUF%PLA,    R3_FREE,     R4_FREE,
     H   EYY,         EZZ,         ETT,         EYZ,
     I   EYT,         EZT,         WYY,         WZZ,
     J   WYZ,         IPM,         GAMA,        BID,
     K   BID,         BID,         BID,         BID,
     L   BID,         BID,         IBID,        BID,
     M   BID,         IBID,        ILAY,        MBID,
     N   MBID,        1,           1,           1,
     O   TABLE,       BID,         BID,         BID,
     P   BID,         IPARG(1,NG), IGEO,        BID,
     Q   ITASK,       NLOC_DMG,    VARNL,       MAT_ELEM,
     R   H3D_STRAIN,  JPLASOL,     JSPH,        SZ_BUFVOIS,
     T   SNPC,        STF,         SBUFMAT,     GLOB_THERM,
     .   SVIS,        SZ_IX,       IRESP,
     .   N2D,         th_strain,   NGROUP       ,TT, 
     .   DT1,         NTABLE,      NUMELQ,      nummat,
     .   numgeo,      numnod,      numels,            
     .   IDEL7NOK,    IDTMIN,      MAXFUNC,
     .   IMON_MAT,    USERL_AVAIL, impl_s,
     .   idyna,       DT         ,BID     ,sensors)
C----------------------------------
C     PETROV-GALERKIN PSEUDO MASSES & ALE MASSES
C----------------------------------
      IF (IPARIT == 0) THEN
        CALL QMASS2(
     1   LBUF%OFF,LBUF%RHO,MS,      AIRE,
     2   NC1,     NC2,     NC3,     NC4,
     3   NEL)
      ELSE
        CALL QMASS2P(
     1   LBUF%OFF,LBUF%RHO,AIRE,    FSKY,
     2   FSKY,    IADQ,    NEL,     NFT)
      ENDIF
C--------------------------
C     UPDATE OF MASSES : ALE physical masses
C----------------------------  
      IF (JALE+JEUL > 0 )THEN
         IF (IPARIT == 0)THEN
          CALL QMASSREAL2(
     1   LBUF%OFF,LBUF%RHO,MS_2D,   VOLN,
     2   NC1,     NC2,     NC3,     NC4,
     3   NEL)
         ELSE
          CALL QMASSREAL2P(
     1   LBUF%OFF,LBUF%RHO,VOLN,    FSKYM,
     2   IADQ,    NEL,     NFT)
         ENDIF
      ENDIF
C------------------------
C     ANTI-HOURGLASS FORCES
C------------------------
      CALL QHVIS2(PM,LBUF%OFF,LBUF%RHO,
     .                  Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .                  VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,
     .                  PY1,PY2,PZ1,PZ2,
     .                  T11,T12,T13,T14,T21,T22,T23,T24, 
     .                  AIRE,SSP,MAT,VD2,VIS,EANI,NGEO,GEO,
     .                  PARTSAV,IPARTQ,EHOU,IPARG(63,NG))
C--------------------------
C     SYNTHESIS PER SUBMATERIAL
C--------------------------
      IFLAG=MOD(NCYCLE,NCPRI)
      IF(IFLAG == 0 .OR. TT >= OUTPUT%TH%THIS .OR. MDESS /= 0.
     .   OR.TT>=OUTPUT%TH%THIS1(1).OR.TT>=OUTPUT%TH%THIS1(2).
     .   OR.TT>=OUTPUT%TH%THIS1(3).OR.TT>=OUTPUT%TH%THIS1(4).OR.TT>=OUTPUT%TH%THIS1(5).
     .   OR.TT>=OUTPUT%TH%THIS1(6).OR.TT>=OUTPUT%TH%THIS1(7).OR.TT>=OUTPUT%TH%THIS1(8).
     .   OR.TT>=OUTPUT%TH%THIS1(9).OR.NTH/=0.OR.NANIM/=0          .
     .   OR.TT>=TABFIS(1).OR.TT>=TABFIS(2).
     .   OR.TT>=TABFIS(3).OR.TT>=TABFIS(4).OR.TT>=TABFIS(5).
     .   OR.TT>=TABFIS(6).OR.TT>=TABFIS(7).OR.TT>=TABFIS(8).
     .   OR.TT>=TABFIS(10))
     . CALL QBILAN(
     1   PARTSAV,  LBUF%OFF, LBUF%EINT,LBUF%RHO,
     2   LBUF%RK,  LBUF%VOL, VY1,      VY2,
     3   VY3,      VY4,      VZ1,      VZ2,
     4   VZ3,      VZ4,      VOLN,     IPARTQ,
     5   EHOU,     BID,      BID,      BID,
     6   BID,      GRESAV,   GRTH,     IGRTH,
     7   IBID,     BID,      ITASK,    NEL,
     8   JTUR,     JCVT,     IGRE,     SENSORS,
     9   GBUF%G_WPLA,GBUF%WPLA)
C
C--------------------
C     INTERNAL FORCES
C--------------------
      CALL QFINT2(
     1   LBUF%SIG,PY1,     PY2,     PZ1,
     2   PZ2,     AIRE,    VOLN,    QVIS,
     3   F11,     F12,     F21,     F22,
     4   AX1,     AX2,     BID,     BID,
     5   BID,     BID,     NEL,     JCVT,
     6   SVIS)
C------------------------
C     ADVECTION TERM (TRANSPORTATION 'FORCE')
C------------------------
      IF (JALE > 0 .AND. MTN /= 11)
     .  CALL BAMOM2(
     1   PM,        V,         W,         LBUF%RHO,
     2   LBUF1%FRAC,LBUF2%FRAC,FILL(1,1), B11,
     3   B12,       B13,       B14,       B21,
     4   B22,       B23,       B24,       PY1,
     5   PY2,       PZ1,       PZ2,       AIRE,
     6   MAT,       NC1,       NC2,       NC3,
     7   NC4,       NEL)
      IF (JEUL > 0 .AND. MTN /= 11)
     .  CALL BEMOM2(
     1   PM,        V,         LBUF%RHO,  LBUF1%FRAC,
     2   LBUF2%FRAC,FILL(1,1), B11,       B12,
     3   B13,       B14,       B21,       B22,
     4   B23,       B24,       PY1,       PY2,
     5   PZ1,       PZ2,       AIRE,      MAT,
     6   NC1,       NC2,       NC3,       NC4,
     7   NEL)
C--------------
C     ASSEMBLY
C--------------
        IF(JEUL+JALE /= 0) CALL CHECK_OFF_ALE(B11 ,B12 ,B13 ,B14 ,B21 ,
     1                                      B22 ,B23 ,B24 ,BIDM,BIDM,
     2                                      BIDM,BIDM,BIDM,BIDM,BIDM,
     3                                      BIDM,BIDM,BIDM,BIDM,BIDM,
     4                                      BIDM,BIDM,BIDM,BIDM,GBUF%OFF,
     5                                      1,NEL,NEL)

      IF (IPARIT == 0) THEN
      CALL BCUMU2 (LBUF1%FRAC,A,
     .             LBUF%SIG,LBUF%EINT,LBUF%RHO,LBUF%QVIS,GBUF%QVIS,
     .             GBUF%SIG,GBUF%EINT,GBUF%RHO,GBUF%TEMP,LBUF%TEMP,
     .             GBUF%PLA,LBUF%PLA,GBUF%BFRAC,LBUF%BFRAC,    
     .             F11, F12, F21, F22, AX1,AX2,
     .             T11,T12,T13,T14,T21,T22,T23,T24, 
     .             B11,B12,B13,B14,B21,B22,B23,B24,
     .             NC1,NC2,NC3,NC4,STI,STIFN,
     .             L_TEMP, L_PLA, L_BFRAC, L_BULK, NEL)
      ELSE
       CALL BCUMU2P(LBUF1%FRAC,
     .              LBUF%SIG,LBUF%EINT,LBUF%RHO,LBUF%QVIS,GBUF%QVIS,
     .              GBUF%SIG,GBUF%EINT,GBUF%RHO,GBUF%TEMP,LBUF%TEMP,
     .              GBUF%PLA,LBUF%PLA,GBUF%BFRAC,LBUF%BFRAC,    
     .              F11, F12, F21, F22, AX1,AX2,
     .              T11,T12,T13,T14,T21,T22,T23,T24, 
     .              B11,B12,B13,B14,B21,B22,B23,B24,
     .              FSKY,FSKY,IADQ,STI,
     .              L_TEMP,L_PLA,L_BFRAC,L_BULK,NEL)
      ENDIF
      
      
      
      
      
      
C-----------------------------------------------------
C     SUBMATERIAL 2 (MAT LAW20)
C-----------------------------------------------------
      IF(IMULT > 1)THEN
        JMULT = 2
        ILAY = -2
        LBUF => ELBUF_TAB(NG)%BUFLY(JMULT)%LBUF(1,1,1)  ! buffer mat 2
        MTN=JPARG(26)
        L_TEMP = ELBUF_TAB(NG)%BUFLY(JMULT)%L_TEMP
        L_PLA  = ELBUF_TAB(NG)%BUFLY(JMULT)%L_PLA
        L_BFRAC= ELBUF_TAB(NG)%BUFLY(JMULT)%L_BFRAC
        L_BULK = ELBUF_TAB(NG)%BUFLY(JMULT)%L_QVIS

      CALL BREST2(GBUF%VOL,IC(LCO),VOLN,
     .            AIRE, AIRES, 
     .            EYY, EZZ, ETT, EYZ, EYT, EZT,
     .            EYYS, EZZS, ETTS, EYZS, EYTS, EZTS,
     .            MAT, MAT_ELEM%MAT_PARAM, NUMMAT)
      CALL BLERO2(LBUF2%FRAC,LBUF%VOL,LBUF%RHO,
     .            FLUX(1,NF2),FLU1(NF2),LBUF%OFF,PM,NGL,VOLN, 
     .            AIRE, EYY, EZZ, ETT, EYZ, EYT, EZT, VD2, DVOL, 
     .            VY1, VY2, VY3, VY4, 
     .            VZ1, VZ2, VZ3, VZ4,
     .            MAT)
      CALL QROTA2(
     1   LBUF%SIG,S1,      S2,      S3,
     2   S4,      S5,      S6,      WYZ,
     3   NEL,     JCVT)

      IF (ISORTH == 0) THEN 
        !isotropic
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
          GAMA(I,1) = ONE                                
          GAMA(I,2) = ZERO                                  
          GAMA(I,3) = ZERO
          GAMA(I,4) = ZERO                                  
          GAMA(I,5) = ONE                                  
          GAMA(I,6) = ZERO 
        ENDDO
      ENDIF                                  
C-----------------------------------------------------
C     STREE TENSOR
C-----------------------------------------------------
      CALL MMAIN(TIMERS,
     1   ELBUF_TAB,   NG,          PM,          GEO,
     2                ALE_CONNECT, IC,          IPARG,
     3   V,           TF,          NPF,         BUFMAT,
     4   STI,         X,           DT2T,        NELTST,
     5   ITYPTST,     OFFSET,      NEL,         W,
     6   LBUF%OFF,    NGEO,        MAT,         NGL,
     7   VOLN,        VD2,         DVOL,        DELTAX,
     8   VIS,         QVIS,        SSP,         S1,
     9   S2,          S3,          S4,          S5,
     A   S6,          EYY,         EZZ,         ETT,
     B   EYZ,         EYT,         EZT,         WYY,
     C   WZZ,         WYZ,         RX,          RY,
     D   RZ,          SX,          SY,          SZ,
     E   VDX,         VDY,         VDZ,         MUVOID,
     F   SSP_EQ,      AIRE,        SIGY,        ET,
     G   BUFVOIS,     LBUF%PLA,    R3_FREE,     R4_FREE,
     H   EYY,         EZZ,         ETT,         EYZ,
     I   EYT,         EZT,         WYY,         WZZ,
     J   WYZ,         IPM,         GAMA,        BID,
     K   BID,         BID,         BID,         BID,
     L   BID,         BID,         IBID,        BID,
     M   BID,         IBID,        ILAY,        MBID,
     N   MBID,        1,           1,           1,
     O   TABLE,       BID,         BID,         BID,
     P   BID,         NPARG_BID,       IGEO,        BID,
     Q   ITASK,       NLOC_DMG,    VARNL,       MAT_ELEM,
     R   H3D_STRAIN,  JPLASOL,     JSPH,        SZ_BUFVOIS,
     S   SNPC,        STF,         SBUFMAT,     GLOB_THERM,
     *   SVIS,        SZ_IX,       IRESP,
     T   N2D,         th_strain,   NGROUP,      TT, 
     .   DT1,         NTABLE,      NUMELQ,      nummat,
     .   numgeo,      numnod,      numels,
     .   IDEL7NOK,    IDTMIN,      MAXFUNC,
     .   IMON_MAT,    USERL_AVAIL, impl_s,
     .   idyna,       DT         ,BID ,sensors)
C----------------------------------
C     PETROV-GALERKIN PSEUDO MASSES & MASSES ALE
C----------------------------------
      IF (IPARIT == 0) THEN
      CALL QMASS2(
     1   LBUF%OFF,LBUF%RHO,MS,      AIRE,
     2   NC1,     NC2,     NC3,     NC4,
     3   NEL)
      ELSE
       CALL QMASS2AP(
     1   LBUF%OFF,LBUF%RHO,AIRE,    FSKY,
     2   FSKY,    IADQ,    NEL,     NFT)
      ENDIF
C--------------------------
C     UPDATE OF MASSES : ALE physical masses
C----------------------------  
      IF (JALE+JEUL > 0 )THEN
         IF (IPARIT == 0)THEN
          CALL QMASSREAL2(
     1   LBUF%OFF,LBUF%RHO,MS_2D,   VOLN,
     2   NC1,     NC2,     NC3,     NC4,
     3   NEL)
         ELSE
          CALL QMASSREAL2AP(
     1   LBUF%OFF,LBUF%RHO,VOLN,    FSKYM,
     2   IADQ,    NEL,     NFT)
         ENDIF
      ENDIF
C------------------------
C     ANTI-HOURGLASS FORCES
C------------------------
      CALL QHVIS2(PM,LBUF%OFF,LBUF%RHO,
     .            Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,
     .            VY1,VY2,VY3,VY4,VZ1,VZ2,VZ3,VZ4,
     .            PY1,PY2,PZ1,PZ2,
     .            T11,T12,T13,T14,T21,T22,T23,T24, 
     .            AIRE,SSP,MAT,VD2,VIS,EANI,NGEO,GEO,
     .            PARTSAV,IPARTQ,EHOU,IPARG(63,NG))
C--------------------------
C     SYNTHESIS PER SUBMATERIAL
C--------------------------
      IFLAG=MOD(NCYCLE,NCPRI)
      IF(IFLAG == 0 .OR. TT >= OUTPUT%TH%THIS .OR. MDESS /= 0.
     .   OR.TT>=OUTPUT%TH%THIS1(1).OR.TT>=OUTPUT%TH%THIS1(2).
     .   OR.TT>=OUTPUT%TH%THIS1(3).OR.TT>=OUTPUT%TH%THIS1(4).OR.TT>=OUTPUT%TH%THIS1(5).
     .   OR.TT>=OUTPUT%TH%THIS1(6).OR.TT>=OUTPUT%TH%THIS1(7).OR.TT>=OUTPUT%TH%THIS1(8).
     .   OR.TT>=OUTPUT%TH%THIS1(9).OR.NTH/=0.OR.NANIM/=0          .
     .   OR.TT>=TABFIS(1).OR.TT>=TABFIS(2).
     .   OR.TT>=TABFIS(3).OR.TT>=TABFIS(4).OR.TT>=TABFIS(5).
     .   OR.TT>=TABFIS(6).OR.TT>=TABFIS(7).OR.TT>=TABFIS(8).
     .   OR.TT>=TABFIS(10))
c
     . CALL QBILAN(
     1   PARTSAV,  LBUF%OFF, LBUF%EINT,LBUF%RHO,
     2   LBUF%RK,  LBUF%VOL, VY1,      VY2,
     3   VY3,      VY4,      VZ1,      VZ2,
     4   VZ3,      VZ4,      VOLN,     IPARTQ,
     5   EHOU,     BID,      BID,      BID,
     6   BID,      GRESAV,   GRTH,     IGRTH,
     7   IBID,     BID,      ITASK,    NEL,
     8   JTUR,     JCVT,     IGRE,     SENSORS,
     9   GBUF%G_WPLA,GBUF%WPLA)
C--------------------
C       INTERNAL FORCES
C--------------------
        CALL QFINT2(
     1   LBUF%SIG,PY1,     PY2,     PZ1,
     2   PZ2,     AIRE,    VOLN,    QVIS,
     3   F11,     F12,     F21,     F22,
     4   AX1,     AX2,     BID,     BID,
     5   BID,     BID,     NEL,     JCVT,
     6   SVIS)
C------------------------
C       ADVECTION TERM (TRANSPORTATION 'FORCE')
C------------------------
        IF(JALE > 0 .AND. MTN /= 11)
     .    CALL BAMOM2(
     1   PM,        V,         W,         LBUF%RHO,
     2   LBUF2%FRAC,LBUF1%FRAC,FILL(1,2), B11,
     3   B12,       B13,       B14,       B21,
     4   B22,       B23,       B24,       PY1,
     5   PY2,       PZ1,       PZ2,       AIRE,
     6   MAT,       NC1,       NC2,       NC3,
     7   NC4,       NEL)
        IF(JEUL > 0 .AND. MTN /= 11)
     .    CALL BEMOM2(
     1   PM,        V,         LBUF%RHO,  LBUF2%FRAC,
     2   LBUF1%FRAC,FILL(1,2), B11,       B12,
     3   B13,       B14,       B21,       B22,
     4   B23,       B24,       PY1,       PY2,
     5   PZ1,       PZ2,       AIRE,      MAT,
     6   NC1,       NC2,       NC3,       NC4,
     7   NEL)
C--------------
C       ASSEMBLY
C--------------
        IF(JEUL+JALE /= 0) CALL CHECK_OFF_ALE(B11 ,B12 ,B13 ,B14 ,B21 ,
     1                                      B22 ,B23 ,B24 ,BIDM,BIDM,
     2                                      BIDM,BIDM,BIDM,BIDM,BIDM,
     3                                      BIDM,BIDM,BIDM,BIDM,BIDM,
     4                                      BIDM,BIDM,BIDM,BIDM,GBUF%OFF,
     5                                      1,NEL,NEL)
        IF(IPARIT == 0)THEN
          CALL BCUMU2(LBUF2%FRAC,A,
     .             LBUF%SIG,LBUF%EINT,LBUF%RHO,LBUF%QVIS,GBUF%QVIS,
     .             GBUF%SIG,GBUF%EINT,GBUF%RHO,GBUF%TEMP,LBUF%TEMP,
     .             GBUF%PLA,LBUF%PLA,GBUF%BFRAC,LBUF%BFRAC,
     .             F11, F12, F21, F22, AX1,AX2,
     .             T11,T12,T13,T14,T21,T22,T23,T24, 
     .             B11,B12,B13,B14,B21,B22,B23,B24,
     .             NC1,NC2,NC3,NC4,STI,STIFN,
     .             L_TEMP,L_PLA,L_BFRAC,L_BULK,NEL)
        ELSE
          CALL BCUMU2PA(LBUF2%FRAC,
     .                 LBUF%SIG,LBUF%EINT,LBUF%RHO,LBUF%QVIS,GBUF%QVIS,
     .                 GBUF%SIG,GBUF%EINT,GBUF%RHO,GBUF%TEMP,LBUF%TEMP,
     .                 GBUF%PLA,LBUF%PLA,GBUF%BFRAC,LBUF%BFRAC,     
     .                 F11, F12, F21, F22, AX1,AX2,
     .                 T11,T12,T13,T14,T21,T22,T23,T24, 
     .                 B11,B12,B13,B14,B21,B22,B23,B24,
     .                 FSKY,FSKY,IADQ,STI,L_TEMP,L_PLA,
     .                 L_BFRAC,L_BULK,NEL)
        ENDIF
      ENDIF
C-----------
      RETURN
      END
