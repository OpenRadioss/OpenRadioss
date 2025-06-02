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
      !||    cmain3           ../engine/source/materials/mat_share/cmain3.F
      !||--- called by ------------------------------------------------------
      !||    c3forc3          ../engine/source/elements/sh3n/coque3n/c3forc3.F
      !||    c3forc3_crk      ../engine/source/elements/xfem/c3forc3_crk.F
      !||    cbaforc3         ../engine/source/elements/shell/coqueba/cbaforc3.F
      !||    cdk6forc3        ../engine/source/elements/sh3n/coquedk6/cdk6forc3.F
      !||    cdkforc3         ../engine/source/elements/sh3n/coquedk/cdkforc3.F
      !||    cforc3           ../engine/source/elements/shell/coque/cforc3.F
      !||    cforc3_crk       ../engine/source/elements/xfem/cforc3_crk.F
      !||    czforc3          ../engine/source/elements/shell/coquez/czforc3.F
      !||    czforc3_crk      ../engine/source/elements/xfem/czforc3_crk.F
      !||--- calls      -----------------------------------------------------
      !||    delamination     ../engine/source/properties/composite_options/stack/delamination.F
      !||    finter           ../engine/source/tools/curve/finter.F
      !||    layini           ../engine/source/elements/shell/coque/layini.F
      !||    mulawc           ../engine/source/materials/mat_share/mulawc.F90
      !||    mulawglc         ../engine/source/materials/mat_share/mulawglc.F
      !||    put_etfac        ../engine/source/elements/solid/solide8z/put_etfac.F
      !||    thermexpc        ../engine/source/materials/mat_share/thermexpc.F
      !||    usermat_shell    ../engine/source/materials/mat_share/usermat_shell.F
      !||--- uses       -----------------------------------------------------
      !||    drape_mod        ../engine/share/modules/drape_mod.F
      !||    dt_mod           ../engine/source/modules/dt_mod.F
      !||    elbufdef_mod     ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    failwave_mod     ../common_source/modules/failwave_mod.F
      !||    mat_elem_mod     ../common_source/modules/mat_elem/mat_elem_mod.F90
      !||    mulawc_mod       ../engine/source/materials/mat_share/mulawc.F90
      !||    nlocal_reg_mod   ../common_source/modules/nlocal_reg_mod.F
      !||    sensor_mod       ../common_source/modules/sensor_mod.F90
      !||    stack_mod        ../engine/share/modules/stack_mod.F
      !||    table_mod        ../engine/share/modules/table_mod.F
      !||    timer_mod        ../engine/source/system/timer_mod.F90
      !||====================================================================
      SUBROUTINE CMAIN3 (TIMERS,
     1           ELBUF_STR ,JFT       ,JLT       ,NFT       ,IPARG      ,
     2           NEL       ,MTN       ,IPLA      ,ITHK      ,GROUP_PARAM,
     3           PM        ,GEO       ,NPF       ,TF        ,BUFMAT     ,
     4           SSP       ,RHO       ,VISCMX    ,DT1C      ,SIGY       ,
     5           AREA      ,EXX       ,EYY       ,EXY       ,EXZ        ,
     6           EYZ       ,KXX       ,KYY       ,KXY       ,NU         ,
     7           OFF       ,THK0      ,MAT       ,PID       ,MAT_ELEM   ,
     8           FOR       ,MOM       ,GSTR      ,FAILWAVE  ,FWAVE_EL   ,
     9           THK       ,EINT      ,IOFC      ,
     A           G         ,A11       ,A12       ,VOL0      ,INDXDEL    ,
     B           NGL       ,ZCFAC     ,SHF       ,GS        ,EPSD_PG  ,
     C           KFTS      ,JHBE      ,ALPE      ,
     D           DIR_A     ,DIR_B     ,IGEO      ,  
     E           IPM       ,IFAILURE  ,NPG       ,FHEAT     ,
     F           TEMPEL    ,DIE       ,JTHE      ,IEXPAN    ,TEMPEL0    ,
     G           ISHPLYXFEM,PLY_EXX   ,
     H           PLY_EYY   ,PLY_EXY   ,PLY_EXZ   ,PLY_EYZ   ,PLY_F      ,
     I           DEL_PLY   ,TH_IPLY   ,SIG_IPLY  ,E1X       ,E1Y        ,
     J           E1Z       ,E2X       ,E2Y       ,E2Z       ,E3X        ,
     K           E3Y       ,E3Z       ,NG        ,TABLE     ,IXFEM      ,
     L           OFFI      ,SENSORS   ,A11_IPLY  ,ELCRKINI  ,
     M           DIR1_CRK  ,DIR2_CRK  ,ALDT      ,IDT_THERM ,THEACCFACT ,
     N           ISMSTR    ,IR        ,IS        ,NLAY      ,NPT        ,
     O           IXLAY     ,IXEL      ,ISUBSTACK ,STACK     ,
     P           F_DEF     ,ITASK     ,DRAPE     ,VARNL     ,NLOC_DMG   ,
     R           INDX_DRAPE,THKE      ,SEDRAPE   ,NUMEL_DRAPE,DT        ,
     Q           NCYCLE    , SNPC     ,STF       ,NXLAYMAX  ,IDEL7NOK   ,
     S           USERL_AVAIL, MAXFUNC ,VARNL_NPTTOT, SBUFMAT   ,SDIR_A    ,
     T          SDIR_B    ,FOR_G)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE TIMER_MOD
      USE MULAWC_MOD
      USE TABLE_MOD
      USE MAT_ELEM_MOD
      USE STACK_MOD
      USE FAILWAVE_MOD
      USE DRAPE_MOD 
      USE NLOCAL_REG_MOD
      USE SENSOR_MOD
      USE ELBUFDEF_MOD
      USE DT_MOD
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
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr18_c.inc"
#include      "impl1_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TIMER_), INTENT(INOUT) :: TIMERS
      INTEGER,INTENT(IN)    :: USERL_AVAIL ! Flag for User libraries availability
      INTEGER,INTENT(IN)    :: MAXFUNC     ! Maximum number of functions
      INTEGER,INTENT(INOUT) :: IDEL7NOK    ! Element deletion flag for IDEL flag in contact interfaces
      INTEGER,INTENT(IN) :: NXLAYMAX       ! XFEM : maximum number of layers
      INTEGER,INTENT(IN) :: VARNL_NPTTOT   ! Total number of integration points
      INTEGER,INTENT(IN) :: SBUFMAT        ! Size of BUFMAT
      INTEGER,INTENT(IN) :: SDIR_A         ! Size of DIR_A
      INTEGER,INTENT(IN) :: SDIR_B         ! Size of DIR_B
      INTEGER,INTENT(IN) :: STF            ! Size of TF
      INTEGER,INTENT(IN) :: SNPC           ! Size of NPF
      INTEGER JFT,JLT,NFT,IR,IS,NPT,NG,NPG,NLAY,NEL,MTN,IPLA,IOFC,ITHK,
     .   KFTS,JHBE,IFAILURE,JTHE,IEXPAN,ISHPLYXFEM,ISMSTR,
     .   IXLAY,IXEL,IXFEM,ISUBSTACK,ITASK , NCYCLE
      INTEGER ,    INTENT(IN)     ::        SEDRAPE,NUMEL_DRAPE
      INTEGER MAT(MVSIZ), PID(MVSIZ), INDXDEL(MVSIZ), NGL(MVSIZ),NPF(*), 
     .   IGEO(NPROPGI,*),IPM(NPROPMI,*),IPARG(*),
     .   A11_IPLY(MVSIZ,*),ELCRKINI(*),FWAVE_EL(NEL)
      INTEGER , DIMENSION(SEDRAPE) :: INDX_DRAPE
      INTEGER, INTENT(IN) :: IDT_THERM
      my_real ,intent(in) :: THEACCFACT
      my_real ,dimension(mvsiz) ,intent(in) :: EPSD_PG
      my_real PM(NPROPM,*), GEO(NPROPG,*),TF(*),THK(*),EINT(JLT,*),FOR(NEL,5), 
     .   MOM(NEL,3),GSTR(NEL,8),BUFMAT(*),G(*),A11(*),A12(*),VOL0(*),
     .   SHF(*),GS(*),SIGY(MVSIZ),RHO(MVSIZ),SSP(MVSIZ),VISCMX(MVSIZ),
     .   OFF(MVSIZ) ,THK0(MVSIZ) ,NU(MVSIZ),DT1C(MVSIZ),
     .   EXX(MVSIZ), EYY(MVSIZ), EXY(MVSIZ), EXZ(MVSIZ), EYZ(MVSIZ),
     .   KXX(MVSIZ), KYY(MVSIZ), KXY(MVSIZ), AREA(MVSIZ),
     .   ZCFAC(MVSIZ,2),ALPE(MVSIZ),DIR_A(SDIR_A),DIR_B(SDIR_B),TEMPEL(*),DIE(*),
     .   TEMPEL0(MVSIZ), PLY_F(MVSIZ,5,*),
     .   PLY_EXX(MVSIZ,*),PLY_EYY(MVSIZ,*),PLY_EXY(MVSIZ,*), 
     .   PLY_EXZ(MVSIZ,*),PLY_EYZ(MVSIZ,*),DEL_PLY(MVSIZ,3,*) ,
     .   TH_IPLY(MVSIZ,*), SIG_IPLY(MVSIZ,3,*),
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .   E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .   E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),OFFI(MVSIZ,*),
     .   DIR1_CRK(*),DIR2_CRK(*),DMG_SCALE(MVSIZ),
     .   F_DEF(MVSIZ,*),ALDT(MVSIZ),VARNL(NEL,*)
      my_real, DIMENSION(NEL), INTENT(IN) :: THKE
      my_real, dimension(mvsiz), intent(inout) :: fheat
      TYPE (TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (STACK_PLY) :: STACK
      TYPE (FAILWAVE_STR_) ,TARGET :: FAILWAVE 
      TYPE (GROUP_PARAM_) :: GROUP_PARAM
      TYPE (DRAPE_) :: DRAPE(NUMEL_DRAPE)
      TYPE (MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM
      TYPE (NLOCAL_STR_) :: NLOC_DMG 
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
      TYPE (DT_), INTENT(IN) :: DT
      my_real, dimension(nel,5), intent(inout) :: FOR_G
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IT,MX,IGTYP,IFUNC_ALPHA,IPMAT_IPLY,ILAY,LAYNPT_MAX,NLAY_MAX
      INTEGER MAT_IPLY(MVSIZ,NPT),IDAMP_FREQ_RANGE
      INTEGER, DIMENSION(:) , ALLOCATABLE :: MATLY   !!  MATLY(MVSIZ*LAY_MAX)
      my_real, DIMENSION(:) , ALLOCATABLE :: THKLY   !!  THKLY(MVSIZ*LAY_MAX*LAYNPT_MAX)
      my_real, DIMENSION(:,:), ALLOCATABLE :: POSLY,THK_LY
        !!         POSLY(MVSIZ,LAY_MAX*LAYNPT_MAX) THK_LY(NEL,LAY_MAX*LAYNPT_MAX)
      my_real
     .   DTEMP(MVSIZ),
     .   SXX(MVSIZ),SYY(MVSIZ), SXY(MVSIZ),SYZ(MVSIZ),SZX(MVSIZ),
     .   EPSXX(MVSIZ),EPSYY(MVSIZ),EPSXY(MVSIZ),EPSYZ(MVSIZ),EPSZX(MVSIZ),
     .   DEPSXX(MVSIZ),DEPSYY(MVSIZ),DEPSXY(MVSIZ),DEPSYZ(MVSIZ),
     .   DEPSZX(MVSIZ), EPSPXX(MVSIZ),EPSPYY(MVSIZ),EPSPXY(MVSIZ),
     .   EPSPYZ(MVSIZ), EPSPZX(MVSIZ),ETIMP(MVSIZ),TENS(NEL,5),ETH(MVSIZ)
      my_real ALPHA,DF,DEINTTH,T0,TM,FSCAL_ALPHA,DM,ZSHIFT
      INTEGER, DIMENSION(:) ,POINTER :: FLD_IDX
C-------------------------------------
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(BUF_FAIL_),POINTER :: FBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(BUF_DAMP_RANGE_)  ,POINTER :: DAMP_BUF
C-----------------------------------------------
       my_real FINTER 
      EXTERNAL FINTER
C=======================================================================
      GBUF => ELBUF_STR%GBUF
      IGTYP=IGEO(11,PID(1))
      MX = MAT(JFT) 
      DM = GROUP_PARAM%VISC_DM   ! membrane damping
      IF (IMPL_S>0) DM = ZERO
      IDAMP_FREQ_RANGE = IPARG(93)
      DAMP_BUF => ELBUF_STR%DAMP_RANGE
      ZCFAC(1:MVSIZ,1:2) = ZERO
C--------------------------------------------------
C     add source terme for law not user
      IF (JTHE > 0 .AND. (MTN < 28 .OR. MTN == 32)) THEN
        DIE(JFT:JLT) = EINT(JFT:JLT,1) + EINT(JFT:JLT,2)
      ELSE
        DIE(JFT:JLT)   = ZERO
      ENDIF
c-----------------
      IF (IEXPAN > 0 .AND. JTHE > 0) THEN
c
        IF (TT == ZERO) TEMPEL0(JFT:JLT) = TEMPEL(JFT:JLT)
        DTEMP(JFT:JLT) = TEMPEL(JFT:JLT) - TEMPEL0(JFT:JLT) 
        TEMPEL0(JFT:JLT) = TEMPEL(JFT:JLT)
c
        IF (IGTYP /= 11 .AND. IGTYP /= 17 .AND. IGTYP /=51 .AND. IGTYP /= 52) THEN
          IFUNC_ALPHA = IPM(219, MX) 
          FSCAL_ALPHA = PM(191, MX)
          DO I=JFT,JLT 
            ALPHA = FINTER(IFUNC_ALPHA,TEMPEL(I),NPF,TF,DF)
            ALPHA = ALPHA * FSCAL_ALPHA
            ETH(I) = ALPHA*DTEMP(I)
            DEINTTH = -HALF*(FOR(I,1) + FOR(I,2))*ETH(I)*THK0(I)*AREA(I)
            GBUF%EINTTH(I) = GBUF%EINTTH(I)  + DEINTTH
            EINT(I,1) = EINT(I,1) + DEINTTH
          ENDDO 
        ENDIF 
      ENDIF    
! Npt_max
      LAYNPT_MAX = 1
      IF(IGTYP == 51 .OR. IGTYP == 52) THEN
        DO ILAY=1,NLAY
           LAYNPT_MAX = MAX(LAYNPT_MAX , ELBUF_STR%BUFLY(ILAY)%NPTT)
        ENDDO  
      ENDIF
      NLAY_MAX   = MAX(NLAY,NPT,IXLAY, ELBUF_STR%NLAY)
      ALLOCATE(MATLY(MVSIZ*NLAY_MAX), THKLY(MVSIZ*NLAY_MAX*LAYNPT_MAX),
     .         POSLY(MVSIZ,NLAY_MAX*LAYNPT_MAX),THK_LY(NEL,NLAY_MAX*LAYNPT_MAX))
c
c----------------------------
      IF (NPT == 0) THEN
          ! Radioss laws 1,2,22,36,43,56,60,86 with global integration
c----------------------------
        ILAY = 1
        IT   = 1
        LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IR,IS,IT)
        ZSHIFT = GEO(199, PID(1))
c
        CALL MULAWGLC(ELBUF_STR,
     1       JFT     ,JLT     ,PM      ,FOR     ,MOM     ,THK     ,
     2       EINT    ,OFF     ,GSTR    ,DIR_A   ,SHF     ,
     3       MAT     ,AREA    ,EXX     ,EYY     ,EXY     ,NEL     ,
     4       EXZ     ,EYZ     ,KXX     ,KYY     ,KXY     ,DM      ,
     5       PID     ,TF      ,NPF     ,MTN     ,DT1C    ,A11     ,
     6       BUFMAT  ,SSP     ,RHO     ,VISCMX  ,IOFC    ,A12     ,
     7       INDXDEL ,NGL     ,ZCFAC   ,GS      ,SIGY    ,G       ,
     8       THK0    ,EPSD_PG,IPLA    ,IGEO    ,IPM     ,TABLE   ,
     9       IR      ,IS      ,F_DEF   ,ISMSTR  ,NU      ,VOL0    ,
     A       KFTS    ,ZSHIFT  ,IDAMP_FREQ_RANGE,MAT_ELEM,DAMP_BUF ,
     B       FOR_G )
c----------------------------
      ELSE IF (MTN > 28 .AND. MTN < 32 .or. MTN == 99 .or. MTN == 200) THEN
c---    user material law libraries here
c
        CALL LAYINI(ELBUF_STR,JFT      ,JLT      ,GEO      ,IGEO     ,
     .              MAT      ,PID      ,THKLY    ,MATLY    ,POSLY    , 
     .              IGTYP    ,IXFEM    ,IXLAY    ,NLAY     ,NPT      ,   
     .              ISUBSTACK,STACK    ,DRAPE    ,NFT      ,THKE      ,
     .              NEL      ,THK_LY   ,INDX_DRAPE,SEDRAPE ,NUMEL_DRAPE)
c
        CALL USERMAT_SHELL (TIMERS, ELBUF_STR  ,MAT_ELEM,
     1       JFT     ,JLT     ,NEL     ,PM      ,FOR     ,MOM     ,
     2       GSTR    ,THK     ,EINT    ,OFF     ,DIR_A   ,DIR_B   ,
     3       MAT     ,AREA    ,EXX     ,EYY     ,EXY     ,EXZ     ,
     4       EYZ     ,KXX     ,KYY     ,KXY     ,GEO     ,THK_LY  ,
     5       PID     ,TF      ,NPF     ,MTN     ,DT1C    ,DM      ,
     6       BUFMAT  ,SSP     ,RHO     ,VISCMX  ,IPLA    ,IOFC    ,
     7       INDXDEL ,NGL     ,THKLY   ,MATLY   ,ZCFAC   ,NG      ,
     8       SHF     ,GS      ,SIGY    ,THK0    ,EPSD_PG,
     9       POSLY   ,IGEO    ,IPM     ,FAILWAVE,FWAVE_EL,
     A       IFAILURE,ALDT    ,TEMPEL  ,DIE     ,
     B       E1X     ,E1Y     ,E1Z     ,E2X     ,E2Y     ,E2Z     ,
     C       E3X     ,E3Y     ,E3Z     ,TABLE   ,IXFEM   ,ELCRKINI,
     D       DIR1_CRK,DIR2_CRK,IPARG   ,JHBE    ,ISMSTR  ,JTHE    ,
     E       TENS    ,IR      ,IS      ,NLAY    ,NPT     ,IXLAY   ,
     F       IXEL    ,ITHK    ,F_DEF   ,ISHPLYXFEM,
     G       ITASK   ,STACK%PM ,ISUBSTACK,STACK ,ALPE    ,
     H       PLY_EXX ,PLY_EYY ,PLY_EXY ,PLY_EXZ ,PLY_EYZ ,PLY_F   ,
     I       VARNL   ,NLOC_DMG,NLAY_MAX,LAYNPT_MAX,DT    )
cc----------------------------
      ELSE   ! User-type Radioss laws , NPT > 0
c----------------------------
!
!       position, thickness and material in integration points
!
        CALL LAYINI(ELBUF_STR,JFT      ,JLT      ,GEO      ,IGEO     ,
     .              MAT      ,PID      ,THKLY    ,MATLY    ,POSLY    , 
     .              IGTYP    ,IXFEM    ,IXLAY    ,NLAY     ,NPT      ,   
     .              ISUBSTACK,STACK    ,DRAPE    ,NFT     ,THKE       ,
     .              NEL      ,THK_LY   ,INDX_DRAPE, SEDRAPE,NUMEL_DRAPE)
c 
        CALL MULAWC(TIMERS,ELBUF_STR ,
     1       JFT     ,JLT     ,NEL     ,PM      ,FOR     ,MOM     ,
     2       GSTR    ,THK     ,EINT    ,OFF     ,DIR_A   ,DIR_B   ,
     3       MAT     ,AREA    ,EXX     ,EYY     ,EXY     ,EXZ     ,
     4       EYZ     ,KXX     ,KYY     ,KXY     ,GEO     ,THK_LY  ,
     5       PID     ,TF      ,NPF     ,MTN     ,DT1C    ,DM      ,
     6       BUFMAT  ,SSP     ,RHO     ,VISCMX  ,IPLA    ,IOFC    ,
     7       INDXDEL ,NGL     ,THKLY   ,MATLY   ,ZCFAC   ,MAT_ELEM,
     8       SHF     ,GS      ,SIGY    ,THK0    ,EPSD_PG,
     9       POSLY   ,IGEO    ,IPM     ,FAILWAVE,FWAVE_EL,
     A       IFAILURE,ALDT    ,TEMPEL  ,DIE     ,FHEAT   ,
     B       TABLE   ,IXFEM   ,ELCRKINI,
     D       SENSORS ,NG      ,IDT_THERM,THEACCFACT,  
     E       DIR1_CRK,DIR2_CRK,IPARG   ,JHBE    ,ISMSTR  ,JTHE    ,
     F       TENS    ,IR      ,IS      ,NLAY    ,NPT     ,IXLAY   ,
     G       IXEL    ,ITHK    ,F_DEF   ,ISHPLYXFEM,
     H       ITASK   ,ISUBSTACK,STACK  ,ALPE    ,
     I       PLY_EXX ,PLY_EYY ,PLY_EXY ,PLY_EXZ ,PLY_EYZ ,PLY_F   ,
     J       VARNL   ,ETIMP   ,NLOC_DMG,NLAY_MAX,LAYNPT_MAX ,DT,
     K       NCYCLE  ,SNPC    ,STF,IMPL_S  ,IMCONV  ,NPROPGI,
     *       NPROPMI ,NPROPM  ,NPROPG,IMON_MAT,NUMGEO  ,
     *       NUMSTACK, DT1    ,TT  ,NXLAYMAX ,IDEL7NOK,USERL_AVAIL,
     *       MAXFUNC,NUMMAT,VARNL_NPTTOT,    SBUFMAT ,SDIR_A  ,SDIR_B  ,NPARG,
     *       IDAMP_FREQ_RANGE,DAMP_BUF)
      ENDIF ! IF (NPT == 0) 
C----------------------------------------------------------------------
      IF (IEXPAN > 0 .AND. JTHE > 0. AND. IDT_THERM==0) THEN
         CALL THERMEXPC(ELBUF_STR,
     1                  JFT  ,JLT   ,GBUF%FORTH  ,FOR   ,EINT  , 
     2                  OFF  ,ETH   ,THK0        ,EXX   ,EYY   ,    
     3                  PM   ,NPT   ,AREA        ,A11   ,A12   ,
     4                  MAT  ,MTN   ,GBUF%EINTTH ,DIR_A ,IR    ,
     5                  IS   ,NLAY  ,THK         ,NEL   ,IGTYP ,
     6                  NPF  , TF   , IPM        ,TEMPEL ,DTEMP,
     7                  THKLY ,POSLY,MOM, MATLY )    
      ENDIF

       IF (JTHE > 0 .AND. (MTN < 28 .OR. MTN == 32)) THEN
         DO I=JFT,JLT          
           DIE(I) = (EINT(I,1) + EINT(I,2) - DIE(I)) * PM(90,MAT(1))          
         ENDDO  
       ENDIF
       IF (JTHE /= 0) THEN
         GBUF%TEMP(1:NEL) = TEMPEL(1:NEL)
       ENDIF
C-------------------------------------------------------------------
C     delamination  for new formulation of shells 
C-----------------------------------------------
      IF (ISHPLYXFEM > 0) THEN
        IPMAT_IPLY = 2  + 2*NPT  
        DO J=1,NPT-1  
          DO I=JFT,JLT 
            MAT_IPLY(I,J) = STACK%IGEO(IPMAT_IPLY + J ,ISUBSTACK)
          ENDDO                                 
        ENDDO  
        DO I=JFT,JLT
          EPSYZ(I)= GSTR(I,4)
          EPSZX(I)= GSTR(I,5)
        ENDDO
c----------
       CALL DELAMINATION(ELBUF_STR,MAT_ELEM%MAT_PARAM,
     .                   JFT      ,JLT      ,IR      ,IS      ,NPT     ,
     .                   MAT_IPLY ,IPM      ,PM      ,BUFMAT  ,NPF     ,
     .                   TF       ,DT1C     ,NGL     ,OFF     ,TH_IPLY ,
     .                   DEL_PLY  ,SIG_IPLY ,OFFI    ,A11_IPLY,FOR     ,
     .                   MOM      ,PLY_F    ,THK0    ,SHF     ,EPSZX   ,
     .                   EPSYZ    ,AREA     ,PID     ,GEO     ,SSP     ,
     .                   POSLY    ,THKLY    ,KXX     ,KYY     ,KXY     ,
     .                   EXZ      ,EYZ      ,EINT    ,GSTR    ,NEL     ,
     .                   NUMMAT   ) 
     
      ENDIF ! PLYXFEM 
C---------------------------------------------------------             
      IF (IMPL_S > 0) THEN
        IF (MTN /=78)  ETIMP(JFT:JLT) = ZCFAC(JFT:JLT,1)
        CALL PUT_ETFAC(NEL ,ETIMP ,MTN)
      END IF
      DEALLOCATE(MATLY, THKLY, POSLY, THK_LY)
c-----------
      RETURN
      END SUBROUTINE CMAIN3
C
