Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  MULAWC                        source/materials/mat_share/mulawc.F
Chd|-- called by -----------
Chd|        CMAIN3                        source/materials/mat_share/cmain3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ENG_USERLIB_FLAWC             source/user_interface/dyn_userlib.c
Chd|        FAIL_BIQUAD_C                 source/materials/fail/biquad/fail_biquad_c.F
Chd|        FAIL_CHANGCHANG_C             source/materials/fail/changchang/fail_changchang_c.F
Chd|        FAIL_COCKROFT_C               source/materials/fail/cockroft_latham/fail_cockroft_c.F
Chd|        FAIL_ENERGY_C                 source/materials/fail/energy/fail_energy_c.F
Chd|        FAIL_FABRIC_C                 source/materials/fail/fabric/fail_fabric_c.F
Chd|        FAIL_FLD_C                    source/materials/fail/fld/fail_fld_c.F
Chd|        FAIL_FLD_XFEM                 source/materials/fail/fld/fail_fld_xfem.F
Chd|        FAIL_GENE1_C                  source/materials/fail/gene1/fail_gene1_c.F
Chd|        FAIL_HASHIN_C                 source/materials/fail/hashin/fail_hashin_c.F
Chd|        FAIL_HC_DSSE_C                source/materials/fail/hc_dsse/fail_hc_dsse_c.F
Chd|        FAIL_INIEVO_C                 source/materials/fail/inievo/fail_inievo_c.F
Chd|        FAIL_JOHNSON_C                source/materials/fail/johnson_cook/fail_johnson_c.F
Chd|        FAIL_JOHNSON_XFEM             source/materials/fail/johnson_cook/fail_johnson_xfem.F
Chd|        FAIL_NXT_C                    source/materials/fail/nxt/fail_nxt_c.F
Chd|        FAIL_ORTHBIQUAD_C             source/materials/fail/orthbiquad/fail_orthbiquad_c.F
Chd|        FAIL_ORTHSTRAIN_C             source/materials/fail/orthstrain/fail_orthstrain_c.F
Chd|        FAIL_PUCK_C                   source/materials/fail/puck/fail_puck_c.F
Chd|        FAIL_RTCL_C                   source/materials/fail/rtcl/fail_rtcl_c.F
Chd|        FAIL_SETOFF_C                 source/materials/fail/fail_setoff_c.F
Chd|        FAIL_SETOFF_NPG_C             source/materials/fail/fail_setoff_npg_c.F
Chd|        FAIL_SETOFF_WIND_FRWAVE       source/materials/fail/fail_setoff_wind_frwave.F
Chd|        FAIL_SYAZWAN                  source/materials/fail/syazwan/fail_syazwan_c.F
Chd|        FAIL_TAB2_C                   source/materials/fail/tabulated/fail_tab2_c.F
Chd|        FAIL_TAB_C                    source/materials/fail/tabulated/fail_tab_c.F
Chd|        FAIL_TAB_OLD_C                source/materials/fail/tabulated/fail_tab_old_c.F
Chd|        FAIL_TAB_OLD_XFEM             source/materials/fail/tabulated/fail_tab_old_xfem.F
Chd|        FAIL_TAB_XFEM                 source/materials/fail/tabulated/fail_tab_xfem.F
Chd|        FAIL_TBUTCHER_C               source/materials/fail/tuler_butcher/fail_tbutcher_c.F
Chd|        FAIL_TBUTCHER_XFEM            source/materials/fail/tuler_butcher/fail_tbutcher_xfem.F
Chd|        FAIL_TENSSTRAIN_C             source/materials/fail/tensstrain/fail_tensstrain_c.F
Chd|        FAIL_VISUAL_C                 source/materials/fail/visual/fail_visual_c.F
Chd|        FAIL_WIERZBICKI_C             source/materials/fail/wierzbicki/fail_wierzbicki_c.F
Chd|        FAIL_WILKINS_C                source/materials/fail/wilkins/fail_wilkins_c.F
Chd|        FAIL_WIND_FRWAVE              source/materials/fail/alter/fail_wind_frwave.F
Chd|        FAIL_WIND_XFEM                source/materials/fail/alter/fail_wind_xfem.F
Chd|        M25DELAM                      source/materials/mat/mat025/m25delam.F
Chd|        PUTSIGNORC3                   source/elements/shell/coqueba/cmatc3.F
Chd|        ROTOV                         source/airbag/roto.F          
Chd|        SIGEPS01C                     source/materials/mat/mat001/sigeps01c.F
Chd|        SIGEPS02C                     source/materials/mat/mat002/sigeps02c.F
Chd|        SIGEPS104C                    source/materials/mat/mat104/sigeps104c.F
Chd|        SIGEPS107C                    source/materials/mat/mat107/sigeps107c.F
Chd|        SIGEPS109C                    source/materials/mat/mat109/sigeps109c.F
Chd|        SIGEPS110C                    source/materials/mat/mat110/sigeps110c.F
Chd|        SIGEPS112C                    source/materials/mat/mat112/sigeps112c.F
Chd|        SIGEPS119C                    source/materials/mat/mat119/sigeps119c.F
Chd|        SIGEPS121C                    source/materials/mat/mat121/sigeps121c.F
Chd|        SIGEPS15C                     source/materials/mat/mat015/sigeps15c.F
Chd|        SIGEPS19C                     source/materials/mat/mat019/sigeps19c.F
Chd|        SIGEPS22C                     source/materials/mat/mat022/sigeps22c.F
Chd|        SIGEPS25C                     source/materials/mat/mat025/sigeps25c.F
Chd|        SIGEPS25CP                    source/materials/mat/mat025/sigeps25cp.F
Chd|        SIGEPS27C                     source/materials/mat/mat027/sigeps27c.F
Chd|        SIGEPS32C                     source/materials/mat/mat032/sigeps32c.F
Chd|        SIGEPS34C                     source/materials/mat/mat034/sigeps34c.F
Chd|        SIGEPS35C                     source/materials/mat/mat035/sigeps35c.F
Chd|        SIGEPS36C                     source/materials/mat/mat036/sigeps36c.F
Chd|        SIGEPS42C                     source/materials/mat/mat042/sigeps42c.F
Chd|        SIGEPS43C                     source/materials/mat/mat043/sigeps43c.F
Chd|        SIGEPS44C                     source/materials/mat/mat044/sigeps44c.F
Chd|        SIGEPS45C                     source/materials/mat/mat045/sigeps45c.F
Chd|        SIGEPS48C                     source/materials/mat/mat048/sigeps48c.F
Chd|        SIGEPS52C                     source/materials/mat/mat052/sigeps52c.F
Chd|        SIGEPS55C                     source/materials/mat/mat055/sigeps55c.F
Chd|        SIGEPS56C                     source/materials/mat/mat056/sigeps56c.F
Chd|        SIGEPS57C                     source/materials/mat/mat057/sigeps57c.F
Chd|        SIGEPS58C                     source/materials/mat/mat058/sigeps58c.F
Chd|        SIGEPS60C                     source/materials/mat/mat060/sigeps60c.F
Chd|        SIGEPS62C                     source/materials/mat/mat062/sigeps62c.F
Chd|        SIGEPS63C                     source/materials/mat/mat063/sigeps63c.F
Chd|        SIGEPS64C                     source/materials/mat/mat064/sigeps64c.F
Chd|        SIGEPS65C                     source/materials/mat/mat065/sigeps65c.F
Chd|        SIGEPS66C                     source/materials/mat/mat066/sigeps66c.F
Chd|        SIGEPS69C                     source/materials/mat/mat069/sigeps69c.F
Chd|        SIGEPS71C                     source/materials/mat/mat071/sigeps71c.F
Chd|        SIGEPS72C                     source/materials/mat/mat072/sigeps72c.F
Chd|        SIGEPS73C                     source/materials/mat/mat073/sigeps73c.F
Chd|        SIGEPS76C                     source/materials/mat/mat076/sigeps76c.F
Chd|        SIGEPS78C                     source/materials/mat/mat078/sigeps78c.F
Chd|        SIGEPS80C                     source/materials/mat/mat080/sigeps80c.F
Chd|        SIGEPS82C                     source/materials/mat/mat082/sigeps82c.F
Chd|        SIGEPS85C_VOID                source/materials/mat/mat085/sigeps85c_void.F
Chd|        SIGEPS86C                     source/materials/mat/mat086/sigeps86c.F
Chd|        SIGEPS87C                     source/materials/mat/mat087/sigeps87c.F
Chd|        SIGEPS88C                     source/materials/mat/mat088/sigeps88c.F
Chd|        SIGEPS93C                     source/materials/mat/mat093/sigeps93c.F
Chd|        SIGEPS96C                     source/materials/mat/mat096/sigeps96c.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        UROTOV                        source/airbag/uroto.F         
Chd|        XFEM_CRK_DIR                  source/elements/xfem/xfem_crk_dir.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|        LAW_USERSH                    source/user_interface/law_usersh.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE MULAWC(ELBUF_STR ,
     1       JFT      ,JLT      ,NEL      ,PM        ,FOR      ,MOM      ,    
     2       GSTR     ,THK      ,EINT     ,OFF       ,DIR_A    ,DIR_B    ,    
     3       MAT      ,AREA     ,EXX      ,EYY       ,EXY      ,EXZ      ,    
     4       EYZ      ,KXX      ,KYY      ,KXY       ,GEO      ,THK_LY   ,             
     5       PID      ,TF       ,NPF      ,MTN       ,DT1C     ,DM       ,           
     6       BUFMAT   ,SSP      ,RHO      ,VISCMX    ,IPLA     ,IOFC     ,    
     7       INDX     ,NGL      ,THKLY    ,MATLY     ,ZCFAC    ,MATPARAM_TAB,             
     8       SHF      ,GS       ,SIGY     ,THK0      ,EPSP     ,              
     9       POSLY    ,IGEO     ,IPM      ,FAILWAVE  ,FWAVE_EL ,                                   
     A       IFAILURE ,ALDT     ,TEMPEL   ,DIE       ,                        
     B       R11      ,R12      ,R13      ,R21       ,R22      ,R23      ,    
     C       R31      ,R32      ,R33      ,TABLE     ,IXFEM    ,ELCRKINI ,    
     D       LSENSOR  ,SENSOR   ,NG       ,                         
     E       DIR1_CRK ,DIR2_CRK ,IPARG    ,JHBE      ,ISMSTR   ,JTHE     ,             
     F       TENSX    ,IR       ,IS       ,NLAY      ,NPT      ,IXLAY    ,    
     G       IXEL     ,ITHK     ,F_DEF    ,ISHPLYXFEM,
     H       ITASK    ,PM_STACK , ISUBSTACK,STACK    ,TSTAR    ,ALPE     ,
     I       PLY_EXX  ,PLY_EYY  ,PLY_EXY  ,PLY_EXZ   ,PLY_EYZ  ,PLY_F    ,
     J       VARNL    ,ETIMP    ,NLOC_DMG )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE LAW_USERSH
      USE TABLE_MOD
      USE ELBUFDEF_MOD
      USE STACK_MOD
      USE FAILWAVE_MOD
      USE MESSAGE_MOD
      USE MATPARAM_DEF_MOD
      USE NLOCAL_REG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "usrplas_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "scr19_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "com20_c.inc"
#include      "impl1_c.inc"
#include      "userlib.inc"
#include      "com_xfem1.inc"
#include      "timeri_c.inc"
#include      "nchara_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IR,IS,IPT,JFT,JLT,NEL,NPT,MTN,IPLA,IOFC,NG,NLAY,
     .  ISMSTR,IXFEM,NUVARR,IFAILURE,JHBE,IXLAY,IXEL,ITHK,JTHE,
     .  IREP,ISUBSTACK,ITASK,ISHPLYXFEM,NUMTABL
      INTEGER MAT(*),MATLY(*),PID(*),NPF(*),NGL(MVSIZ),INDX(MVSIZ),
     .   IGEO(NPROPGI,*),IPM(NPROPMI,*),ELCRKINI(NXLAYMAX,*),
     .   LSENSOR(NSENSI,*),IPARG(*),FWAVE_EL(NEL)
      my_real DM
      my_real FOR(NEL,5),MOM(NEL,3),GSTR(NEL,8),THK(*),EINT(JLT,2),OFF(*),
     .   DIR_A(*),DIR_B(*),VISCMX(*),PM(NPROPM,*),THK_LY(NEL,*),
     .   AREA(*),GEO(NPROPG,*),TF(*),DT1C(*),THKLY(*),ALDT(*),
     .   EXX(*), EYY(*), EXY(*), EXZ(*), EYZ(*), EPSP(*),
     .   KXX(*), KYY(*), KXY(*),BUFMAT(*),SSP(*),RHO(*),
     .   ZCFAC(MVSIZ,2),SHF(*),GS(*),SIGY(*),THK0(*),
     .   POSLY(MVSIZ,*),TEMPEL(MVSIZ),DIE(*),VARNL(NEL,*),
     .   R11(MVSIZ), R12(MVSIZ), R13(MVSIZ),
     .   R21(MVSIZ), R22(MVSIZ), R23(MVSIZ),
     .   R31(MVSIZ), R32(MVSIZ), R33(MVSIZ),
     .   SENSOR(NSENSR,*),DIR1_CRK(*),DIR2_CRK(*),TENSX(NEL,5),
     .   F_DEF(MVSIZ,8),PM_STACK(20,*),TSTAR(MVSIZ),ALPE(MVSIZ),
     .   PLY_EXX(MVSIZ,*),PLY_EYY(MVSIZ,*),PLY_EXY(MVSIZ,*), 
     .   PLY_EXZ(MVSIZ,*),PLY_EYZ(MVSIZ,*),PLY_F(MVSIZ,5,*),ETIMP(MVSIZ)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TARGET :: ALDT, IPM, VARNL
      TYPE (STACK_PLY) :: STACK
      TYPE (FAILWAVE_STR_) ,TARGET :: FAILWAVE 
      TYPE (MATPARAM_STRUCT_) , DIMENSION(NUMMAT) ,TARGET :: MATPARAM_TAB
      TYPE (NLOCAL_STR_) :: NLOC_DMG 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ::I,J,K,II,JJ,KK,IC,JMLY,IPG,IP,IT,IFL,ILAY,NPG,MPT,MX,IMAT,
     .   IRUPT,NFAIL,IADBUF,IADBUFR,MAXNULVAR,IGTYP,NUVAR,NVARF,JDIR,
     .   NFUNC, JPOS, NINDX,NUPAR,NUPARAM,NFUNC1,IFAILWV,NLOCAL,
     .   IUN,IBIDON1,IBIDON2,IBIDON3,
     .   IPTX,ILAYER,IROT,DMG_FLAG,
     .   IGMAT,IPGMAT,NPTT,IPT_ALL,NPTTOT,NUVARV,ILAW,
     .   FAILNPT,JOFF,SIZNUL,PLY_ID,ISEQ,PROGRESSIVE_CRACK,
     .   ORTH_DAMAGE,L_DMG,IPRONY,ISRATE,NVARTMP,INLOC
      INTEGER :: IJ1,IJ2,IJ3,IJ4,IJ5
      INTEGER :: IJ(5),IFLAG(1)
      INTEGER ,DIMENSION(MAXFUNC) :: IFUNC,IFUNC1
      INTEGER ,DIMENSION(MVSIZ)   :: IFAIL,MATF,IOFF_DUCT,NFIS1,NFIS2,NFIS3
c
      my_real 
     .   SIGDMG(MVSIZ,5),DAMCR(MVSIZ,2),SIGKSI(MVSIZ,5),TENS(MVSIZ,5),
     .   EINT_LOC(2,MVSIZ)
      my_real ,DIMENSION(MVSIZ) :: 
     .   FPSXX,FPSYY,FPSZZ,FPSXY,FPSYX,EPCHK,EPSPDT,DT_INV,COPY_PLA,PLA0,
     .   DEGMB ,DEGFX ,SIGOFF,THKLYL,THKN  ,ETSE,OFF_OLD, 
     .   DEPSXX,DEPSYY,DEPSXY,DEPSYZ,DEPSZX,EPSXX ,EPSYY ,EPSXY,
     .   EPSYZ ,EPSZX ,EPSPXX,EPSPYY,EPSPXY,EPSPYZ,EPSPZX,SIGOXX,
     .   SIGOYY,SIGOXY,SIGOYZ,SIGOZX,SIGNXX,SIGNYY,SIGNXY,SIGNYZ,
     .   SIGNZX,SIGVXX,SIGVYY,SIGVXY,SIGVYZ,SIGVZX,
     .   WMC, EPSPL, YLD,DPLA,VOL0, COEF,HARDM,LA0,G_IMP,VISC,WPLAR,DMAXT,
     .   EPSP_LOC,SIGNXX_FAIL,SIGNYY_FAIL,SIGNXY_FAIL,SIGNYZ_FAIL,
     .   SIGNZX_FAIL,AREAMIN,DAREAMIN,DMG_GLOB_SCALE,DMG_LOC_SCALE,ET_IMP, EPSTHTOT
c
      my_real :: ZT,DTINV, VOL2,FCUT,ASRATE,EPS_M2,EPS_K2,
     .   R1,R2,R3,S1,S2,S3,R12A,R22A,S12B,S22B,RS1,RS2,RS3,
     .   T1,T2,T3,PHI,SUM1,SUM2,FACT,R3R3,S3S3, 
     .   BIDON1,BIDON2,BIDON3,BIDON4,BIDON5,RATIO,VV,AA,TRELAX
      my_real  SCALE1(NEL)
      my_real ,DIMENSION(NEL), TARGET :: LE_MAX
      my_real TT_LOCAL
      my_real, DIMENSION(:) ,POINTER  :: EL_TEMP,YLDFAC,CRKLEN,CRKDIR,DADV,TFAIL,EL_LEN,
     .                                   EL_PLA
      my_real, DIMENSION(NLAY,10)  :: PTHKF
      TARGET :: TEMPEL,BUFMAT,EPSXX,EPSYY,DEPSXX,DEPSYY,SCALE1,POSLY,THKLY
c----
      TYPE(TTABLE) TABLE(*)
      TYPE(ULAWCINTBUF) :: USERBUF     
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(BUF_MAT_) ,POINTER :: MBUF    
      TYPE(BUF_FAIL_),POINTER :: FBUF
      TYPE(BUF_VISC_),POINTER :: VBUF
c----
      INTEGER, DIMENSION(:) ,POINTER  :: FLD_IDX,FOFF,OFFLY,ITABLE,VARTMP
      my_real, DIMENSION(:) ,POINTER  :: UVAR,UVARF,UELR,UELR1,DAM,
     .   DFMAX,TDEL ,OFFL,UVARV,UPARAM,UPARAMF,EPS1,EPS2,DEPS1,DEPS2,
     .   DIRDMG,DIR_ORTH,ZZ,THLY
      TYPE(MATPARAM_STRUCT_) , POINTER :: MATPARAM
c----
      LOGICAL :: LOGICAL_USERL_AVAIL
      LOGICAL :: FLAG_EPS,FLAG_EPSP,FLAG_ZCFAC,FLAG_ETIMP
      LOGICAL :: FLAG_LAW1,FLAG_LAW2,FLAG_LAW25,FLAG_LAW22
!
      CHARACTER OPTION*256
      INTEGER SIZE
      CHARACTER IUSER_KEY*ncharline
c
!!      my_real, DIMENSION(:) ,POINTER :: SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
C======================================================================|
      GBUF => ELBUF_STR%GBUF
      IPG  = (IS-1)*ELBUF_STR%NPTR + IR     ! current Gauss point
      NPG  = ELBUF_STR%NPTR * ELBUF_STR%NPTS
      IGTYP = IGEO(11,PID(1))
      IGMAT = IGEO(98,PID(1))
      INLOC = IPARG(78)                     ! Flag for non-local regularization
!
      LOGICAL_USERL_AVAIL=.FALSE.
      IF (USERL_AVAIL > 0) LOGICAL_USERL_AVAIL=.TRUE.
      DO K=1,5
        IJ(K) = NEL*(K-1)
      ENDDO
      IJ1 = IJ(1) + 1
      IJ2 = IJ(2) + 1
      IJ3 = IJ(3) + 1
      IJ4 = IJ(4) + 1
      IJ5 = IJ(5) + 1
C      
      IPT_ALL = 0
      NPTTOT  = 0
!       need to check the value of ILAW for the array initializations because MTN/=ILAW
      FLAG_LAW1  = .FALSE.
      FLAG_LAW2  = .FALSE.
      FLAG_LAW22 = .FALSE.
      FLAG_LAW25 = .FALSE.
!
!  Check visco elastic model 
!             
      IPRONY = 0
      DO ILAY=1,NLAY
        NPTTOT = NPTTOT + ELBUF_STR%BUFLY(ILAY)%NPTT
        ILAYER = ILAY
        IF (IXFEM == 1 .AND. IXLAY > 0) ILAYER = IXLAY  ! xfem  multilayer
        ILAW = ELBUF_STR%BUFLY(ILAYER)%ILAW
        IMAT = ELBUF_STR%BUFLY(ILAYER)%IMAT
        IF(ILAW==1) FLAG_LAW1 = .TRUE.
        IF(ILAW==2) FLAG_LAW2 = .TRUE.
        IF(ILAW==22) FLAG_LAW22 = .TRUE.
        IF(ILAW==25) FLAG_LAW25 = .TRUE.
        IF(IPM(222,IMAT) > 0) IPRONY=1
      ENDDO
C
!     ZCFAC computation is only usefull for CZFORC3 and CZFORC3_CRK routines
      FLAG_ZCFAC=.FALSE.
      IF( (JHBE>=21.AND.JHBE<=29).OR.(IMPL_S>0)) FLAG_ZCFAC=.TRUE.
      FLAG_ETIMP=.FALSE.
      IF(IMPL_S>0 .AND. MTN==78) FLAG_ETIMP=.TRUE.
      SCALE1(1:NEL) = ONE
      IFLAG(1) = IPLA
      BIDON1 = ZERO
      BIDON2 = ZERO
      BIDON3 = ZERO
      BIDON4 = ZERO
      BIDON5 = ZERO
      IBIDON1 = 0
      IBIDON2 = 0
      IBIDON3 = 0
c      IMAT = IPM(1, MAT(1))
      IUN=1
c
      DMG_FLAG = 0
      TRELAX   = ZERO
c                                 
      DEGMB(JFT:JLT) = FOR(JFT:JLT,1)*EXX(JFT:JLT)+FOR(JFT:JLT,2)*EYY(JFT:JLT)  
     .           + FOR(JFT:JLT,3)*EXY(JFT:JLT)+FOR(JFT:JLT,4)*EYZ(JFT:JLT)  
     .           + FOR(JFT:JLT,5)*EXZ(JFT:JLT)                  
      DEGFX(JFT:JLT) = MOM(JFT:JLT,1)*KXX(JFT:JLT)+MOM(JFT:JLT,2)*KYY(JFT:JLT)  
     .           + MOM(JFT:JLT,3)*KXY(JFT:JLT)                  
!
      IF (FLAG_LAW1 .OR. FLAG_LAW25) THEN
        IF (NPT == 1) THEN
          DEGMB(JFT:JLT) = FOR(JFT:JLT,1)*EXX(JFT:JLT)+FOR(JFT:JLT,2)*EYY(JFT:JLT)+
     .                     FOR(JFT:JLT,3)*EXY(JFT:JLT)+FOR(JFT:JLT,4)*EYZ(JFT:JLT)+
     .                     FOR(JFT:JLT,5)*EXZ(JFT:JLT)
          DEGFX(JFT:JLT) = ZERO
        ENDIF
      ENDIF
!
      IF (FLAG_LAW25 .and. IGTYP == 9 . and. NPT == 1) THEN
        DEGMB(JFT:JLT) = ZERO
        DEGFX(JFT:JLT) = ZERO
      ENDIF
!
      VOL0(JFT:JLT)   = AREA(JFT:JLT)*THK0(JFT:JLT)            
      THKN(JFT:JLT)   = THK(JFT:JLT)
      FOR(JFT:JLT,1)  = ZERO
      FOR(JFT:JLT,2)  = ZERO
      FOR(JFT:JLT,3)  = ZERO
      FOR(JFT:JLT,4)  = ZERO
      FOR(JFT:JLT,5)  = ZERO
      MOM(JFT:JLT,1)  = ZERO
      MOM(JFT:JLT,2)  = ZERO
      MOM(JFT:JLT,3)  = ZERO
      YLD(JFT:JLT)    = ZERO
      IF(FLAG_ZCFAC .AND. MTN /= 22) ZCFAC(JFT:JLT,1)= ZERO
      IF(FLAG_ZCFAC) ZCFAC(JFT:JLT,2)= ONE
      ETSE(JFT:JLT)   = ONE
      IF(FLAG_ETIMP) ETIMP(JFT:JLT)= ZERO
      COEF(JFT:JLT)   = ONE 
      IF(FLAG_LAW25)THEN
          WPLAR(JFT:JLT)=ZERO
          NFIS1(JFT:JLT)=0
          NFIS2(JFT:JLT)=0
          NFIS3(JFT:JLT)=0
          DAMCR(JFT:JLT,1)= ZERO
          DAMCR(JFT:JLT,2)= ZERO
          DMAXT(JFT:JLT)  = ZERO
      ENDIF
      OFF_OLD(JFT:JLT) = OFF(JFT:JLT)
      IOFF_DUCT(JFT:JLT) = 0
      DMG_GLOB_SCALE(JFT:JLT) = ONE
      SIGOFF(1:NEL) = ONE
      EPCHK(1:MVSIZ)  = ZERO
      VISCMX(1:MVSIZ) = ZERO
!
      IF ( FLAG_LAW22 ) THEN
#include "vectorize.inc"
        DO I=JFT,JLT
          ALPE(I) = EM30
        ENDDO
      ENDIF ! IF ( FLAG_LAW22 )
!       compute the inverse of dt and save the result 
      DO I=JFT,JLT
        DT_INV(I) = DT1C(I)/MAX(DT1C(I)**2,EM20)
      ENDDO
      IF (FLAG_LAW25 .OR. FLAG_LAW2) SIGY(JFT:JLT) = ZERO
c-----------------------------------------------------------
c     EQUIVALENT STRAIN RATE (au sens energetique)
c-----------------------------------------------------------
C     e = 1/t integ[1/2 E (eps_m + k z)^2 dz ]
C     e = 1/2 E eps_eq^2
C     eps_eq = sqrt[ eps_m^2 + 1/12 k^2t^2 ]
c
      IF (IXLAY > 0) THEN
        MX = ELBUF_STR%BUFLY(IXLAY)%IMAT
      ELSE
        MX = MAT(JFT)
      ENDIF
      ISRATE = IPM(3,MX)
c
      IF (ISRATE == 1) THEN
#include "vectorize.inc" 
        DO I=JFT,JLT
          EPS_K2 = (KXX(I)*KXX(I)+KYY(I)*KYY(I)+KXX(I)*KYY(I)
     .           + FOURTH*KXY(I)*KXY(I)) *THK(I)*THK(I)*ONE_OVER_9
          EPS_M2 = FOUR_OVER_3*(EXX(I)*EXX(I)+EYY(I)*EYY(I)+EXX(I)*EYY(I)
     .           + FOURTH*EXY(I)*EXY(I))
          EPSP_LOC(I) = SQRT(EPS_K2 + EPS_M2)*DT_INV(I)
        END DO
      ELSE IF (ISRATE > 1) THEN
#include "vectorize.inc" 
        DO I=JFT,JLT
          EPS_M2  = FOUR_OVER_3*(EXX(I)*EXX(I)+EYY(I)*EYY(I)+EXX(I)*EYY(I)
     .            + FOURTH*EXY(I)*EXY(I))
          EPSP_LOC(I) = SQRT(EPS_M2)*DT_INV(I)
        END DO
      ENDIF
c
      IF (ISRATE > 0) THEN    ! strain rate filtering with exponential average
        ASRATE = MIN(ONE, PM(9,MX)*DT1)
        DO I=JFT,JLT
          EPSP(I)   = ASRATE*EPSP_LOC(I) + (ONE-ASRATE)*EPSP(I)
          EPSPL(I)  = EPSP(I)
          EPSPDT(I) = EPSP(I)*DT1
        END DO
      ENDIF
c-----------------------------------------------------------
C     LOOP OVER THICKNESS INTEGRATION POINTS (LAYERS)
C-----------------------------------------------------------
      DO ILAY =1,NLAY
        IF (IXFEM == 1 .AND. IXLAY > 0) THEN  !  multilayer xfem
          ILAYER = IXLAY
        ELSE
          ILAYER = ILAY
        ENDIF
        PROGRESSIVE_CRACK = 0
        ORTH_DAMAGE = 0
        PLY_ID = 0
        IF (IGTYP == 52) THEN
          PLY_ID = PLY_INFO(1,STACK%IGEO(2+ILAY,ISUBSTACK)-NUMSTACK)
        ELSEIF(IGTYP == 17 .OR. IGTYP == 51)THEN
          PLY_ID = IGEO(1,STACK%IGEO(2+ILAY,ISUBSTACK))
        ENDIF
        BUFLY => ELBUF_STR%BUFLY(ILAYER)
        NPTT   = BUFLY%NPTT
        JMLY = 1 + (ILAYER-1)*JLT
        JDIR = 1 + (ILAYER-1)*JLT*2
c
        NFAIL  = BUFLY%NFAIL
        IMAT   = BUFLY%IMAT
        ILAW   = BUFLY%ILAW
        NUVAR  = BUFLY%NVAR_MAT
        NVARTMP= BUFLY%NVARTMP
        NUVARV = BUFLY%NVAR_VISC
        ISEQ   = BUFLY%L_SEQ
        IADBUF = MAX(1,IPM(7,IMAT))
        NUPARAM= IPM(9,IMAT)       
        NFUNC  = IPM(10,IMAT) 
        UPARAM => BUFMAT(IADBUF:IADBUF+NUPARAM)
        NUMTABL= IPM(226,IMAT) 
        ITABLE => IPM(226+1:226+NUMTABL,IMAT)    
        MATPARAM => MATPARAM_TAB(IMAT)

        IF (IGTYP == 11 .OR. IGTYP == 16 .OR. IGTYP == 17 .OR. 
     .      IGTYP == 51 .OR. IGTYP == 52) THEN
          IF (ILAW == 58 .OR. ILAW == 88) NFUNC = IPM(10,IMAT)+IPM(6,IMAT) 
        ENDIF
        DO I=1,NFUNC
          IFUNC(I)=IPM(10+I,IMAT)
        ENDDO  
c
!       check if EPSP arraies are used or not
!       EPSP is not used for law : 2/25/32/42/58/63/63/64/69/71/72/76/78/80/82/85/96 --> FLAG_EPSP=.FALSE.
!       EPSP is used for the other laws   
!       EPSP is also used for the rupture : 4/5/6/14/24
        FLAG_EPSP=.TRUE.
        IF(ILAW==1.OR.ILAW==2.OR.ILAW==25.OR.ILAW==32.OR.ILAW==42.OR.ILAW==58.OR.ILAW==62.OR.
     .       ILAW==63.OR.ILAW==64.OR.ILAW==69.OR.ILAW==71.OR.ILAW==72.OR.
     .       ILAW==76.OR.ILAW==78.OR.ILAW==80.OR.ILAW==82.OR.ILAW==85.OR.ILAW == 88 .OR. ILAW==96) FLAG_EPSP=.FALSE.
!       check if EPS arraies are used or not
!       EPS is not used for law : 2/25/63/64/65/66/72/76/78/93 --> FLAG_EPS=.FALSE.     
!       EPS is used for the other laws 
!       EPS is also used for the rupture : 4/5/6/7/10/24   
        FLAG_EPS=.TRUE.
        IF(ILAW==1.OR.ILAW== 2.OR.ILAW==25.OR.ILAW==32.OR.ILAW==63.OR.ILAW==64.OR.ILAW==65.OR.
     .       ILAW==66.OR.ILAW==72.OR.ILAW==76.OR.ILAW==78.OR.ILAW==85.OR.ILAW==93) FLAG_EPS=.FALSE.


        IF (IFAILURE == 1) THEN   
          DO IT=1,NPTT                                                         
            FBUF => BUFLY%FAIL(IR,IS,IT)                   
            DO IFL = 1, NFAIL      ! loop over fail models in current layer
              IRUPT  =  FBUF%FLOC(IFL)%ILAWF
              IF(IRUPT== 4.OR.IRUPT== 5.OR.IRUPT== 6.OR.IRUPT==7.OR.
     .           IRUPT==10.OR.IRUPT==24.OR.IRUPT==34) FLAG_EPS=.TRUE.

              IF(IRUPT== 4.OR.IRUPT== 5.OR.IRUPT== 6.OR.
     .           IRUPT==14.OR.IRUPT==24) FLAG_EPSP=.TRUE.
            ENDDO
          ENDDO
        ENDIF

!       DEPSYZ/ZX and EPSYZ/ZX are IT independant
!       EPSXY is IT independant for IGTYP/=1
        DEPSYZ(JFT:JLT)=EYZ(JFT:JLT)
        DEPSZX(JFT:JLT)=EXZ(JFT:JLT)
        IF (FLAG_EPS) THEN
          IF (IGTYP == 1) THEN
            EPSYZ(JFT:JLT)= GSTR(JFT:JLT,4)
            EPSZX(JFT:JLT)= GSTR(JFT:JLT,5)
          ELSE
            EPSXY(JFT:JLT)  = ZERO
            EPSYZ(JFT:JLT)  = ZERO
            EPSZX(JFT:JLT)  = ZERO
          ENDIF 
        ENDIF
c----------------------------------------------------------------------
!---
        L_DMG = BUFLY%L_DMG
!---
        DO IT=1,NPTT
          IPT = IPT_ALL + IT        ! count all NPTT through all layers
          JPOS = 1 + (IPT-1)*JLT
c
          LBUF  => BUFLY%LBUF(IR,IS,IT)
          UVAR  => BUFLY%MAT(IR,IS,IT)%VAR
          UVARV => BUFLY%VISC(IR,IS,IT)%VAR
          VARTMP=> BUFLY%MAT(IR,IS,IT)%VARTMP
          DIRDMG => LBUF%DMG(1:L_DMG*NEL)
c
          ! -> Make sure the non-local increment is positive
          IF (INLOC > 0) THEN 
            DO I = JFT,JLT
              IF (OFF(I) == ONE) THEN 
                VARNL(I,IT)    = MAX(VARNL(I,IT),ZERO)
              ELSE 
                VARNL(I,IT)    = ZERO
              ENDIF
              LBUF%PLANL(I)  = LBUF%PLANL(I) + VARNL(I,IT)
              LBUF%EPSDNL(I) = VARNL(I,IT)/MAX(DT1,EM20)
            ENDDO 
          ENDIF
c
          IF (JTHE == 0 .and. BUFLY%L_TEMP > 0) THEN        
             EL_TEMP => LBUF%TEMP(1:NEL)
          ELSE
             EL_TEMP => TEMPEL(1:NEL)
          ENDIF
c         initial scale factor for yield stress defined per IPG,NPI
          IF (ILAW==36 .OR. ILAW==87) THEN
            YLDFAC => SCALE1(1:NEL)
            IF (BUFLY%L_FAC_YLD > 0) THEN        
              YLDFAC => LBUF%FAC_YLD(1:NEL)
            ENDIF
          ENDIF
!---
!!          SIGNXX(JFT:JLT) => LBUF%SIG(IJ1+JFT:IJ1+JLT) 
!!          SIGNYY(JFT:JLT) => LBUF%SIG(IJ2+JFT:IJ2+JLT)
!!          SIGNXY(JFT:JLT) => LBUF%SIG(IJ3+JFT:IJ3+JLT)
!!          SIGNYZ(JFT:JLT) => LBUF%SIG(IJ4+JFT:IJ4+JLT)
!!          SIGNZX(JFT:JLT) => LBUF%SIG(IJ(5)+JFT:IJ(5)+JLT)
!---
C-----------------------------------------
c         COORDONNEES DU POINT D'INTEGRATION IPT
C-----------------------------------------
          ZZ   => POSLY(1:NEL,IPT)
          THLY => THKLY(JPOS:JPOS+NEL-1)
          THKLYL(1:NEL) = THLY(1:NEL)*THK0(1:NEL)
C
          IF (IGTYP == 1 .or. IGTYP == 9) THEN
            WMC(1:NEL) = WM(IPT,NPT)
          ELSE
            WMC(1:NEL) = ZZ(1:NEL)*THLY(1:NEL)
          ENDIF
C-------------------------------
C         INCREMENT DE DEFORMATIONS 
C-------------------------------
          IF (ILAW==58) THEN
             SIGNXX(1:MVSIZ) = ZERO
             SIGNYY(1:MVSIZ) = ZERO
             SIGNXY(1:MVSIZ) = ZERO
             SIGNYZ(1:MVSIZ) = ZERO
             SIGNZX(1:MVSIZ) = ZERO
             SIGVXX(1:MVSIZ) = ZERO
             SIGVYY(1:MVSIZ) = ZERO
             SIGVXY(1:MVSIZ) = ZERO
             SIGVYZ(1:MVSIZ) = ZERO
             SIGVZX(1:MVSIZ) = ZERO
          ENDIF
!       -------------------------------
!       IGTYP = 1
!       -------------------------------
          IF (IGTYP == 1) THEN
        !       -----------
        !       ismstr=10 + foam laws
        !       -----------
           IF (ISMSTR==10 .AND. (MTN == 1 .OR. MTN == 42 .OR.
     .                           MTN == 69.OR. MTN == 71 .OR. MTN == 88)) THEN
            DO I=JFT,JLT 
              ZT=ZZ(I)*THK0(I)
              DEPSXX(I)=EXX(I)+ZT*KXX(I)
              DEPSYY(I)=EYY(I)+ZT*KYY(I)
              DEPSXY(I)=EXY(I)+ZT*KXY(I)
!       not IT dependant
!              DEPSYZ(I)=EYZ(I)
!              DEPSZX(I)=EXZ(I)
C
              TENS(I,1)= F_DEF(I,1)+ZT*F_DEF(I,6)
              TENS(I,2)= F_DEF(I,2)+ZT*F_DEF(I,7)
              TENS(I,3)= F_DEF(I,3)+ZT*F_DEF(I,8)
              TENS(I,4)= F_DEF(I,4)+ZT*F_DEF(I,5)
!       not IT dependant
!              EPSYZ(I)= GSTR(I,4)
!              EPSZX(I)= GSTR(I,5)
            ENDDO
C---------  [F]=[F_DEF]+[1]; [B]=[F][F]^t strain-----
            DO I=JFT,JLT
              EPSXX(I)=TENS(I,1)*(TWO+TENS(I,1))+
     .                 TENS(I,3)*TENS(I,3)
              EPSYY(I)=TENS(I,2)*(TWO+TENS(I,2))+
     .                 TENS(I,4)*TENS(I,4)
              EPSXY(I)=TWO*(TENS(I,3)+TENS(I,4)+TENS(I,1)*TENS(I,4)+
     .                 TENS(I,3)*TENS(I,2))
            ENDDO
           ELSE IF (ILAW == 27) THEN
            DO I=JFT,JLT
              ZT       = ZZ(I)*THK0(I)
              TENS(I,1)=EXX(I)+ZT*KXX(I)
              TENS(I,2)=EYY(I)+ZT*KYY(I)
              TENS(I,3)=HALF*(EXY(I)+ZT*KXY(I))
              TENS(I,4)=HALF*EYZ(I)
              TENS(I,5)=HALF*EXZ(I)
            ENDDO
C
            CALL ROTOV(JFT,JLT,TENS,DIRDMG,NEL)
C
            DO I=JFT,JLT
              DEPSXX(I)=TENS(I,1)
              DEPSYY(I)=TENS(I,2)
              DEPSXY(I)=TWO*TENS(I,3)
              DEPSYZ(I)=TWO*TENS(I,4)
              DEPSZX(I)=TWO*TENS(I,5)         
            ENDDO
           ELSE
        !       -----------
        !       /= ismstr=10 + foam laws
        !       -----------
            DO I=JFT,JLT
              ZT=ZZ(I)*THK0(I)
              DEPSXX(I)=EXX(I)+ZT*KXX(I)
              DEPSYY(I)=EYY(I)+ZT*KYY(I)
              DEPSXY(I)=EXY(I)+ZT*KXY(I)
            ENDDO
            IF (FLAG_EPS) THEN
              DO I=JFT,JLT
                ZT=ZZ(I)*THK0(I)
                EPSXX(I)= GSTR(I,1)+ZT*GSTR(I,6)
                EPSYY(I)= GSTR(I,2)+ZT*GSTR(I,7)
                EPSXY(I)= GSTR(I,3)+ZT*GSTR(I,8)
             ENDDO
            ENDIF
           END IF!(ISMSTR==10)
!       -------------------------------
!       IGTYP = 16
!       -------------------------------
          ELSEIF (IGTYP == 16) THEN
        !       ------------
        !       ISMSTR=11
        !       ------------
            IF (ISMSTR == 11) THEN
c             total strain in fiber coord sys
              DO I=JFT,JLT
                II = JDIR + I-1
                R1 = DIR_A(II)
                S1 = DIR_A(II+NEL)
                R2 = DIR_B(II)
                S2 = DIR_B(II+NEL)
c               total strain in element coord sys   
                ZT = ZZ(I)*THK0(I)
                T1 = GSTR(I,1) + ZT*GSTR(I,6)
                T2 = GSTR(I,2) + ZT*GSTR(I,7)
                T3 = HALF*(GSTR(I,3) + ZT*GSTR(I,8))
                DEPSXY(I) = (R1*R2 + S1*S2) / (R1*S2 - R2*S1)    ! Tan(alpha_totale)
                DEPSXX(I) = R1*R1*T1 + S1*S1*T2 + TWO*R1*S1*T3  ! eps_x dir1
                DEPSYY(I) = R2*R2*T1 + S2*S2*T2 + TWO*R2*S2*T3  ! eps_y dir2           
c
!                DEPSYZ(I) = EYZ(I)  ! incremental
!                DEPSZX(I) = EXZ(I)   
c
                EPSXX(I) = T1     
                EPSYY(I) = T2   
!       not IT dependant
!                EPSXY(I) = ZERO
!                EPSYZ(I) = ZERO                       
!                EPSZX(I) = ZERO  
              ENDDO
            ELSE  
        !       ------------
        !       ISMSTR/=11
        !       ------------
c             strain rate in fiber coord sys
              DO I=JFT,JLT
                II = JDIR + I-1
                R1 = DIR_A(II)
                S1 = DIR_A(II+NEL)
                R2 = DIR_B(II)
                S2 = DIR_B(II+NEL)
C---
!                EPSXX(I) = GSTR(I,1)      
!                EPSYY(I) = GSTR(I,2)   
!       not IT dependant 
!                EPSXY(I) = ZERO
!                EPSYZ(I) = ZERO                     
!                EPSZX(I) = ZERO  
C---
                ZT = ZZ(I)*THK0(I)
                T1 = EXX(I) + ZT*KXX(I)
                T2 = EYY(I) + ZT*KYY(I)
                T3 = HALF*(EXY(I) + ZT*KXY(I))                
                DEPSXY(I) = (R1*R2 + S1*S2) / (R1*S2 - R2*S1)   ! Tan(alpha_totale)
                DEPSXX(I) = R1*R1*T1 + S1*S1*T2 + TWO*R1*S1*T3 ! delta_eps_x dir1
                DEPSYY(I) = R2*R2*T1 + S2*S2*T2 + TWO*R2*S2*T3 ! delta_eps_y dir2
c
!       not IT dependant
!                DEPSYZ(I) = EYZ(I)
!                DEPSZX(I) = EXZ(I)   
c---
c               total true strain in global coord sys
!                EPSXX(I)  = GSTR(I,1) + ZT*GSTR(I,6)
!                EPSYY(I)  = GSTR(I,2) + ZT*GSTR(I,7)
!       not IT dependant
!                EPSXY(I)  = ZERO
!                EPSYZ(I)  = ZERO
!                EPSZX(I)  = ZERO    
              ENDDO
            ENDIF
            IF (FLAG_EPS) THEN
              DO I=JFT,JLT
c             total true strain in global coord sys
                ZT = ZZ(I)*THK0(I)
                EPSXX(I)  = GSTR(I,1) + ZT*GSTR(I,6)
                EPSYY(I)  = GSTR(I,2) + ZT*GSTR(I,7)    
              ENDDO
            ENDIF
c------
!       -------------------------------
!       IGTYP = 9 / 10 / 11 / 17 / 51 and 52
!       -------------------------------
          ELSE
C--         Igtyp 9/10/11/17/51/52
!               -------------------------------
!               IGTYP = 51 or 52 + ilaw=58
!               -------------------------------
           IF ((IGTYP == 51 .OR. IGTYP == 52) .AND. ILAW == 58) THEN
        !       ------------
        !       ISMSTR=11
        !       ------------
            IF (ISMSTR == 11) THEN
c             total strain in fiber coord sys
              DO I=JFT,JLT
                II = JDIR + I-1
                R1 = DIR_A(II)
                S1 = DIR_A(II+NEL)
                R2 = DIR_B(II)
                S2 = DIR_B(II+NEL)
c               total strain in element coord sys   
                ZT = ZZ(I)*THK0(I)
                T1 = GSTR(I,1) + ZT*GSTR(I,6)
                T2 = GSTR(I,2) + ZT*GSTR(I,7)
                T3 = HALF*(GSTR(I,3) + ZT*GSTR(I,8))
                DEPSXY(I) = (R1*R2 + S1*S2) / (R1*S2 - R2*S1)    ! Tan(alpha_totale)
                DEPSXX(I) = R1*R1*T1 + S1*S1*T2 + TWO*R1*S1*T3  ! eps_x dir1
                DEPSYY(I) = R2*R2*T1 + S2*S2*T2 + TWO*R2*S2*T3  ! eps_y dir2           
c
!                DEPSYZ(I) = EYZ(I)  ! incremental
!                DEPSZX(I) = EXZ(I)   
c
!                EPSXX(I) = T1     
!                EPSYY(I) = T2      
!       not IT dependant
!                EPSXY(I) = ZERO
!                EPSYZ(I) = ZERO                       
!                EPSZX(I) = ZERO  
              ENDDO
            ELSE  
        !       ------------
        !       ISMSTR/=11
        !       ------------
c             strain rate in fiber coord sys
              DO I=JFT,JLT
                II = JDIR + I-1
                R1 = DIR_A(II)
                S1 = DIR_A(II+NEL)
                R2 = DIR_B(II)
                S2 = DIR_B(II+NEL)
C
!                EPSXX(I) = GSTR(I,1)      
!                EPSYY(I) = GSTR(I,2)
!       not IT dependant    
!                EPSXY(I) = ZERO
!                EPSYZ(I) = ZERO                     
!                EPSZX(I) = ZERO  
C---
                ZT = ZZ(I)*THK0(I)
                T1 = EXX(I) + ZT*KXX(I)
                T2 = EYY(I) + ZT*KYY(I)
                T3 = HALF*(EXY(I) + ZT*KXY(I))
                DEPSXY(I) = (R1*R2 + S1*S2) / (R1*S2 - R2*S1)   ! Tan(alpha_totale)
                DEPSXX(I) = R1*R1*T1 + S1*S1*T2 + TWO*R1*S1*T3 ! delta_eps_x dir1
                DEPSYY(I) = R2*R2*T1 + S2*S2*T2 + TWO*R2*S2*T3 ! delta_eps_y dir2
c
!                DEPSYZ(I) = EYZ(I)
!                DEPSZX(I) = EXZ(I)   
c---
c               total true strain in global coord sys
!                EPSXX(I)  = GSTR(I,1) + ZT*GSTR(I,6)
!                EPSYY(I)  = GSTR(I,2) + ZT*GSTR(I,7)
!       not IT dependant
!                EPSXY(I)  = ZERO
!                EPSYZ(I)  = ZERO
!                EPSZX(I)  = ZERO    
              ENDDO
            ENDIF ! IF (ISMSTR == 11) THEN
            IF (FLAG_EPS) THEN
                DO I=JFT,JLT
                        ZT = ZZ(I)*THK0(I)
                        T1 = GSTR(I,1) + ZT*GSTR(I,6)
                        T2 = GSTR(I,2) + ZT*GSTR(I,7)
                        EPSXX(I) = T1     
                        EPSYY(I) = T2       
                ENDDO
            ENDIF
           ELSE !        Igtyp 9/10/11/17/51   - NO law58
!               -------------------------------
!               IGTYP /= 51 and 52 or ilaw/=58
!               -------------------------------
           IF (ILAW /= 1 .AND. ILAW /= 2 .AND. ILAW /= 32) THEN
            DO I=JFT,JLT
              ZT       = ZZ(I)*THK0(I)
              TENS(I,1)=EXX(I)+ZT*KXX(I)
              TENS(I,2)=EYY(I)+ZT*KYY(I)
              TENS(I,3)=HALF*(EXY(I)+ZT*KXY(I))
              TENS(I,4)=HALF*EYZ(I)
              TENS(I,5)=HALF*EXZ(I)
            ENDDO
C
            IF (ILAW /= 27) THEN
              CALL ROTOV(JFT,JLT,TENS,DIR_A(JDIR),NEL)
            ELSE
              CALL ROTOV(JFT,JLT,TENS,DIRDMG,NEL)
            ENDIF
C
            DO I=JFT,JLT
              DEPSXX(I)=TENS(I,1)
              DEPSYY(I)=TENS(I,2)
              DEPSXY(I)=TWO*TENS(I,3)
              DEPSYZ(I)=TWO*TENS(I,4)
              DEPSZX(I)=TWO*TENS(I,5)         
            ENDDO
           ELSEIF (ILAW == 1 .OR. ILAW == 2 .OR. ILAW == 32) THEN
            DO I=JFT,JLT
              ZT       = ZZ(I)*THK0(I)
              DEPSXX(I)=EXX(I)+ZT*KXX(I)
              DEPSYY(I)=EYY(I)+ZT*KYY(I)
              DEPSXY(I)=EXY(I)+ZT*KXY(I)
!       depsyz and zx are IT independant !
!              DEPSYZ(I)=EYZ(I)
!              DEPSZX(I)=EXZ(I)
            ENDDO
           ENDIF
!
            IF (ISMSTR==10 .AND. (MTN == 1 .OR. MTN == 42 .OR.
     .                            MTN == 69.OR. MTN == 71 .OR. MTN == 88)) THEN
              DO I=JFT,JLT 
                ZT=ZZ(I)*GBUF%THK_I(I)
C
                TENS(I,1)= F_DEF(I,1)+ZT*F_DEF(I,6)
                TENS(I,2)= F_DEF(I,2)+ZT*F_DEF(I,7)
                TENS(I,3)= F_DEF(I,3)+ZT*F_DEF(I,8)
                TENS(I,4)= F_DEF(I,4)+ZT*F_DEF(I,5)
              ENDDO
C--------   - [F]=[F_DEF]+[1]; [B]=[F][F]^t strain-----
              EPSXX(JFT:JLT)=TENS(JFT:JLT,1)*(TWO+TENS(JFT:JLT,1))+TENS(JFT:JLT,3)*TENS(JFT:JLT,3)
              EPSYY(JFT:JLT)=TENS(JFT:JLT,2)*(TWO+TENS(JFT:JLT,2))+TENS(JFT:JLT,4)*TENS(JFT:JLT,4)
              EPSXY(JFT:JLT)=TENS(JFT:JLT,3)+TENS(JFT:JLT,4)+TENS(JFT:JLT,1)*TENS(JFT:JLT,4)+TENS(JFT:JLT,3)*TENS(JFT:JLT,2)
              DO I=JFT,JLT
                TENS(I,1)= EPSXX(I)
                TENS(I,2)= EPSYY(I)
                TENS(I,3)= EPSXY(I)
                TENS(I,4)= HALF*GSTR(I,4)
                TENS(I,5)= HALF*GSTR(I,5)
              ENDDO
            ELSEIF (ILAW == 32) THEN
              DO I=JFT,JLT
                ZT=ZZ(I)*THK0(I)
                EPSXX(I)= GSTR(I,1)+ZT*GSTR(I,6)
                EPSYY(I)= GSTR(I,2)+ZT*GSTR(I,7)
                EPSXY(I)= GSTR(I,3)+ZT*GSTR(I,8)
              ENDDO
            ELSE
              DO I=JFT,JLT
                ZT=ZZ(I)*THK0(I)
                TENS(I,1)= GSTR(I,1)+ZT*GSTR(I,6)
                TENS(I,2)= GSTR(I,2)+ZT*GSTR(I,7)
                TENS(I,3)= HALF*(GSTR(I,3)+ZT*GSTR(I,8))
                TENS(I,4)= HALF*GSTR(I,4)
                TENS(I,5)= HALF*GSTR(I,5)          
              ENDDO
            END IF!(ISMSTR==10)
C
             IF (ILAW /= 27 . and. ILAW /= 32) THEN
               CALL ROTOV(JFT,JLT,TENS,DIR_A(JDIR),NEL)
             ELSE IF (ILAW == 27) THEN
               CALL ROTOV(JFT,JLT,TENS,DIRDMG,NEL)
             ENDIF
C
            IF (FLAG_EPS) THEN
              DO I=JFT,JLT
                EPSXX(I) = TENS(I,1)
                EPSYY(I) = TENS(I,2)
                EPSXY(I) = TWO*TENS(I,3)
                EPSYZ(I) = TWO*TENS(I,4)
                EPSZX(I) = TWO*TENS(I,5)         
              ENDDO
            ENDIF
           ENDIF  ! IF (IGTYP ==51 .AND. ILAW == 58)
          ENDIF  ! IF (IGTYP == 1)
!       -------------------------------
!       end of IGTYP condition
!       -------------------------------
C---
          IF (FLAG_EPSP) THEN
            DO I=JFT,JLT
              DTINV   =DT_INV(I)!DT1C(I)/MAX(DT1C(I)**2,EM20)
              EPSPXX(I)=DEPSXX(I)*DTINV
              EPSPYY(I)=DEPSYY(I)*DTINV
              EPSPXY(I)=DEPSXY(I)*DTINV
              EPSPYZ(I)=DEPSYZ(I)*DTINV
              EPSPZX(I)=DEPSZX(I)*DTINV
            ENDDO
          ENDIF  
          DPLA(1:MVSIZ) = ZERO
          IF (IFAILURE == 1) THEN  
           IF (ELBUF_STR%BUFLY(ILAYER)%L_PLA > 0) THEN
             PLA0(JFT:JLT) = LBUF%PLA(JFT:JLT)
           ELSE
             PLA0(JFT:JLT) = ZERO
           ENDIF
          ENDIF
C------------------------------------------
C         CONTRAINTES ELASTIQUES + 
C         CONTRAINTES PLASTIQUEMENT ADMISSIBLES
C------------------------------------------
          IF (ILAW == 1) THEN
            CALL SIGEPS01C(JFT       ,JLT      ,NEL      ,IMAT     ,GS       ,
     1    LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     2                     SIGNXX    ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     3                     DEPSXX    ,DEPSYY   ,DEPSXY   ,DEPSYZ   ,DEPSZX   ,
     4                     THKN      ,THKLYL   ,OFF      ,PM       ,ISMSTR   ,
     5                     EPSXX     ,EPSYY    ,EPSXY    )
          ELSEIF (ILAW == 2) THEN
            CALL SIGEPS02C(
     1                  JFT        ,JLT       ,PM       ,EINT     ,THKN     ,
     2                  OFF        ,SIGY      ,DT1C     ,IPLA     ,NEL      ,
     3                  VOL0       ,GS        ,ISRATE   ,THKLYL   ,ETSE     ,
     4                  NGL        ,EPSPDT    ,G_IMP    ,SIGKSI   ,IOFF_DUCT,
     5                  DPLA       ,TSTAR     ,JTHE     ,HARDM    ,EPCHK    ,
     6                  IMAT       ,IPT       ,NPTTOT   ,LBUF%PLA ,OFF_OLD  ,
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8                  SIGNXX     ,SIGNYY    ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     9                  DEPSXX     ,DEPSYY    ,DEPSXY   ,DEPSYZ   ,DEPSZX   ,
     A                  LBUF%SIGB(IJ1),LBUF%SIGB(IJ2),LBUF%SIGB(IJ3),INLOC  ,
     B                  VARNL(1,IT))
!
          ELSEIF (ILAW == 15) THEN
            CALL SIGEPS15C(
     1                 JFT      ,JLT     ,PM       ,LBUF%DAM   ,DIR_A(JDIR),
     2                 IMAT     ,SHF     ,NGL      ,EPSPL      ,DMG_FLAG   ,
     3                 ILAYER   ,NEL     ,ISRATE   ,LBUF%PLA   ,SIGDMG     ,
     4                 DEPSXX   ,DEPSYY  ,DEPSXY   ,DEPSYZ     ,DEPSZX     ,
     5        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     6                 SIGNXX   ,SIGNYY  ,SIGNXY   ,SIGNYZ     ,SIGNZX     ,
     7                 LBUF%DSUM)
          ELSEIF (ILAW == 19) THEN

            CALL SIGEPS19C(
     .            NEL       ,NUPARAM   ,FLAG_ZCFAC,ZCFAC     ,SHF       ,
     .            UPARAM    ,NPTTOT    ,SSP       ,
     .            EPSXX     ,EPSYY     ,EPSXY     ,EPSYZ     ,EPSZX     ,
     .            LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     .            SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ    ,SIGNZX    ,
     .            GBUF%SIGI ,LSENSOR   ,SENSOR    )

          ELSEIF (ILAW == 22) THEN
            CALL SIGEPS22C(
     1                   JFT      ,JLT    ,PM       ,THKN   ,OFF     ,
     2                   SIGY     ,DT1C   ,IPLA     ,NEL    ,OFF_OLD ,
     3                   GS       ,DPLA   ,IOFF_DUCT,NPTT   ,IPT     , 
     4                   EPCHK    ,ALPE   ,THKLYL   ,IMAT   ,LBUF%PLA,
     5                   DEPSXX   ,DEPSYY ,DEPSXY   ,DEPSYZ ,DEPSZX  ,
     6         LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     7                   SIGNXX   ,SIGNYY ,SIGNXY   ,SIGNYZ ,SIGNZX  ,
     8                   INLOC    ,VARNL(1,IT))
          ELSEIF (ILAW == 27) THEN
            CALL SIGEPS27C(
     1                   JFT      ,JLT     ,PM       ,THKN    ,OFF   ,
     2                   GSTR     ,IMAT    ,DT1C     ,IPLA    ,SHF   ,
     3                   NGL      ,THK0    ,THKLYL   ,LBUF%CRAK,LBUF%DAM,
     4                   SIGY     ,ZCFAC   ,DPLA     ,ILAYER  ,IPT   ,
     5                   ISRATE   ,NEL     ,ZZ       ,NPTTOT  ,EPSPDT,
     6                   DEPSXX   ,DEPSYY  ,DEPSXY   ,DEPSYZ  ,DEPSZX,
     7         LBUF%SIG(IJ1),LBUF%SIG(IJ2) ,LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),             
     9                   SIGNXX   ,SIGNYY  ,SIGNXY   ,SIGNYZ  ,SIGNZX,
     4                   DIRDMG   ,LBUF%PLA,INLOC    ,VARNL(1,IT))
!
            EPSPL(JFT:JLT) = EPSPDT(JFT:JLT)
          ELSEIF (ILAW == 32) THEN
            CALL SIGEPS32C(
     1                     JFT        ,JLT    ,PM      ,THKN   ,OFF    ,
     2                     DIR_A(JDIR),IPT    ,IMAT    ,NEL    ,DT1C   ,
     3                     GS         ,EPSP   ,THKLYL  ,IPLA   ,DPLA   ,
     4                     DEPSXX     ,DEPSYY ,DEPSXY  ,DEPSYZ ,DEPSZX ,
     5        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     6                     SIGNXX     ,SIGNYY ,SIGNXY  ,SIGNYZ ,SIGNZX ,
     7                     LBUF%PLA   ,NGL    ,HARDM   ,INLOC  ,VARNL(1,IT),
     8                     LBUF%SEQ   )
               EPSPL(JFT:JLT) = EPSP(JFT:JLT)
          ELSEIF (ILAW == 25) THEN
           IF (IGTYP /= 1 .and. IGTYP /= 9) THEN
            IF (IXFEM == 1 .AND. IXLAY > 0) NPTTOT = ELBUF_STR%BUFLY(IXLAY)%NPTT
!         INTEGRATION BY LAYERS
            CALL SIGEPS25C(
     1        JFT         ,JLT      ,PM          ,OFF      ,GSTR      ,
     2        DIR_A(JDIR) ,THLY     ,IMAT        ,DT1C     ,SHF       ,
     3        IPRONY      ,NGL      ,THK0        ,EXX      ,OFF_OLD   ,
     4        EYY         ,EXY      ,EXZ         ,EYZ      ,KXX       ,
     5        KYY         ,KXY      ,ZZ          ,EPSPL    ,RHO       ,
     6        SSP         ,IPM      ,BUFMAT      ,DT_INV   ,LBUF%OFF  ,
     7        SIGY        ,ZCFAC    ,NPTT        ,ILAYER   ,DAMCR     ,
     8        NFIS1       ,NFIS2    ,NFIS3       ,DMAXT    ,WPLAR     ,
     9        NPTTOT      ,IGTYP    ,NEL         ,LBUF%VISC,LBUF%SIGPLY,
     A        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     B        SIGNXX      ,SIGNYY   ,SIGNXY      ,SIGNYZ   ,SIGNZX    ,
     C        SIGVXX      ,SIGVYY   ,SIGVXY      ,SIGVYZ   ,SIGVZX    ,
     D        ISRATE      ,UVARV    ,ISHPLYXFEM  ,IPT      ,LBUF%SEQ  ,
     E        PLY_EXX     ,PLY_EYY  ,PLY_EXY     ,PLY_EXZ  ,PLY_EYZ   ,
     F        PLY_F       ,LBUF%PLA ,LBUF%DAM    ,LBUF%CRAK,GBUF%IERR ,
     G        IOFF_DUCT   ,IFAILURE ,PLY_ID      ,IPG      )
          ELSEIF (IGTYP == 9) THEN
!           INTEGRATION BY POINTS (through thickness)
            CALL SIGEPS25CP(
     1                      JFT     ,JLT    ,PM       ,OFF     ,DIR_A(JDIR) ,
     2                      SHF     ,NPT    ,NGL      ,IPT     ,OFF_OLD     ,
     3                      THK0    ,EPSPL  ,SIGY     ,ZCFAC   ,NEL    ,
     4                      DEPSXX  ,DEPSYY ,DEPSXY   ,DEPSYZ  ,DEPSZX ,
     5        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     6                      SIGNXX  ,SIGNYY ,SIGNXY   ,SIGNYZ  ,SIGNZX ,
     7                      IMAT    ,WPLAR  ,IOFF_DUCT,LBUF%PLA,ISRATE )
           ENDIF ! IF (IGTYP)
C        
       ELSEIF (ILAW == 34) THEN
            CALL SIGEPS34C(
     1        JLT    ,NUPARAM,NUVAR   ,NFUNC    ,IFUNC   ,
     2        NPF    ,TF     ,TT     ,DT1C    ,UPARAM    ,
     3        RHO    ,THKLYL  ,GS     ,
     4        EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5        DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6        EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8        SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     A        SSP    ,THKN    ,UVAR     ,OFF    )    
          ELSEIF (ILAW == 35) THEN
            CALL SIGEPS35C(
     1        JLT    ,NUPARAM,NUVAR   ,NFUNC    ,IFUNC   ,
     2        NPF    ,NPT    ,IPT     ,IFLAG    ,
     2        TF     ,TT     ,DT1C    ,UPARAM   ,RHO    ,
     3        AREA   ,EINT   ,THKLYL  ,
     4        EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5        DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6        EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8        SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9        SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A        SSP    ,VISCMX ,THKN    ,UVAR     ,OFF    ,
     B        NGL    ,SHF    ,ETSE    ,ISRATE   ,ASRATE ,
     C        EPSP   )
          ELSEIF (ILAW == 36) THEN
              CALL SIGEPS36C(
     1          JLT    ,NUPARAM,NUVAR   ,NVARTMP  ,NFUNC   ,
     2          IFUNC  ,NPF    ,NPT     ,IPT      ,IFLAG   ,
     2          TF     ,TT     ,DT1C    ,UPARAM   ,RHO     ,
     3          AREA   ,EINT   ,THKLYL  ,ASRATE   ,
     4          EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5          DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6          EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7          LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8          SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9          SSP    ,VISCMX ,THKN    ,LBUF%PLA ,UVAR   ,
     A          VARTMP ,OFF    ,NGL    ,IPM     ,IMAT     ,
     B          ETSE   ,GS     ,SIGY   ,EPSPL   ,ISRATE   ,
     C          DPLA   ,G_IMP  ,SIGKSI  ,SHF      ,HARDM  ,
     D          YLDFAC ,LBUF%SIGB,INLOC ,VARNL(1,IT)) 
          ELSEIF (ILAW == 42) THEN
           CALL SIGEPS42C(
     1         JLT    , NUPARAM, NUVAR   , NFUNC , IFUNC , NPF   ,
     2         NPT    , IPT    , 
     2         TF     , TT     , DT1C    , UPARAM, RHO  , 
     3         AREA   , EINT   , THKLYL  ,
     4         EPSPXX , EPSPYY , EPSPXY  , EPSPYZ, EPSPZX, 
     5         DEPSXX , DEPSYY , DEPSXY  , DEPSYZ, DEPSZX,
     6         EPSXX  , EPSYY  , EPSXY   , EPSYZ , EPSZX ,
     7         LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8         SIGNXX , SIGNYY , SIGNXY  , SIGNYZ, SIGNZX,
     9         SIGVXX , SIGVYY , SIGVXY  , SIGVYZ, SIGVZX,
     A         SSP    , VISCMX , THKN    , UVAR   ,
     B         OFF    , NGL    , ISMSTR  , IPM   , GS)
          ELSEIF (ILAW == 43) THEN
           CALL SIGEPS43C(
     1        JLT    ,NUPARAM,NUVAR   ,NPF      ,TF      ,
     2        NFUNC  ,IFUNC  ,ISRATE  ,TT       ,DT1C    ,
     3        UPARAM ,RHO    ,AREA    ,EINT     ,THKLYL  ,
     4        EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX  ,
     5        DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX  ,
     6        EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX   ,
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8        SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9        SSP    ,THKN   ,LBUF%PLA ,UVAR    ,OFF    ,
     A        NGL    ,ETSE   ,HARDM    ,SIGY    ,GS     ,
     B        DPLA   ,EPSP   ,SHF      ,INLOC   ,VARNL(1,IT),
     C        LBUF%SEQ)
           EPSPL(JFT:JLT) = EPSP(JFT:JLT)
          ELSEIF (ILAW == 44) THEN
           CALL SIGEPS44C(
     1        JLT    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,
     2        NPF    ,NPT    ,IPT  ,IFLAG   ,
     2        TF     ,TT     ,DT1C    ,UPARAM  ,RHO,
     3        AREA   ,EINT   ,THKLYL  ,
     4        EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5        DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6        EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8        SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9        SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A        SSP    ,VISCMX ,THKN    ,LBUF%PLA ,UVAR   ,
     B        OFF    ,NGL    ,IPM     ,MATLY(JMLY),ETSE ,
     C        GS     ,SIGY   ,EPSP    ,DPLA     ,ASRATE ,
     D        NVARTMP,VARTMP ,LBUF%SIGB,INLOC   ,VARNL(1,IT))
          ELSEIF (ILAW == 45) THEN
           CALL SIGEPS45C(
     1        JLT    ,NUPARAM,NUVAR   ,NFUNC    ,IFUNC  ,
     2        NPF    ,NPT    ,IPT     ,IFLAG    ,
     2        TF     ,TT     ,DT1C    ,UPARAM   ,RHO    ,
     3        AREA   ,EINT   ,THKLYL  ,
     4        EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5        DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6        EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8        SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9        SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A        SSP    ,VISCMX ,THKN    ,LBUF%PLA ,UVAR   ,
     B        OFF    ,NGL    ,SHF     )
          ELSEIF (ILAW == 48) THEN
           CALL SIGEPS48C(
     1        JLT    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,
     2        NPF    ,NPT    ,IPT  ,IFLAG   ,
     2        TF     ,TT     ,DT1C    ,BUFMAT  ,RHO,
     3        AREA   ,EINT   ,THKLYL  ,
     4        EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5        DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6        EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8        SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9        SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A        SSP    ,VISCMX ,THKN    ,LBUF%PLA ,UVAR   ,
     B        OFF    ,NGL    ,IPM     ,MATLY(JMLY),ETSE ,
     C        GS     ,SIGY   ,EPSPL   ,DPLA     ,ISRATE ,
     D        INLOC  ,VARNL(1,IT))
          ELSEIF (ILAW == 52) THEN
           CALL SIGEPS52C(
     1        JLT    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,
     2        NPF    ,NPT    ,IPT  ,IFLAG   ,
     2        TF     ,TT     ,DT1C    ,BUFMAT  ,RHO,
     3        AREA   ,EINT   ,THKLYL  ,
     4        EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5        DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6        EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8        SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9        SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A        SSP    ,VISCMX ,THKN    ,LBUF%PLA ,UVAR   ,
     B        OFF    ,NGL    ,IPM     ,MATLY(JMLY),ETSE ,
     C        GS     , SIGY , EPSPL   ,TABLE)
          ELSEIF (ILAW == 55) THEN
           CALL SIGEPS55C(
     1        JLT    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,
     2        NPF    ,NPT    ,IPT  ,IFLAG   ,
     2        TF     ,TT     ,DT1C    ,BUFMAT  ,RHO     ,
     3        AREA   ,EINT   ,THKLYL  ,
     4        EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5        DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6        EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8        SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9        SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A        SSP    ,VISCMX ,THKN    ,LBUF%PLA ,UVAR   ,
     B        OFF    ,NGL    ,IPM     ,MATLY(JMLY),ETSE ,
     C        GS     ,SIGY   ,EPSPL   ,ISRATE    )
          ELSEIF (ILAW == 56) THEN
           CALL SIGEPS56C(
     1        JLT    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,
     2        NPF    ,NPT    ,IPT  ,IFLAG   ,
     2        TF     ,TT     ,DT1C    ,BUFMAT  ,RHO,
     3        AREA   ,EINT   ,THKLYL  ,
     4        EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5        DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6        EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8        SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9        SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A        SSP    ,VISCMX ,THKN    ,LBUF%PLA ,UVAR   ,
     B        OFF    ,NGL    ,IPM     ,MATLY(JMLY),ETSE ,
     C        GS     ,SIGY   ,EPSPL   ,DPLA     ,ISRATE )
          ELSEIF (ILAW == 57) THEN
           CALL SIGEPS57C(
     1        JLT    ,NUPARAM,NUVAR   ,NFUNC    ,IFUNC  ,
     2        NPF    ,NPT    ,IPT     ,IFLAG    ,
     2        TF     ,TT     ,DT1C    ,UPARAM   ,RHO    ,
     3        AREA   ,EINT   ,THKLYL  ,
     4        EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5        DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6        EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8        SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9        SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A        SSP    ,VISCMX ,THKN    ,LBUF%PLA ,UVAR   ,
     B        OFF    ,NGL    ,SHF     ,ETSE     ,SIGY   ,
     C        HARDM  ,LBUF%SEQ,ISRATE ,EPSP     ,INLOC  ,
     D        VARNL(1,IT))
          ELSEIF (ILAW == 58) THEN

           CALL SIGEPS58C(
     1        JLT     ,NUPARAM ,NUVAR   ,NFUNC    ,IFUNC  , 
     2        NPF     ,NPT     ,IPT     ,IFLAG    ,          
     2        TF      ,TT      ,DT1C    ,UPARAM   ,RHO    ,  
     3        AREA    ,EINT    ,THKLYL  ,                    
     4        EPSPXX  ,EPSPYY  ,EPSPXY  ,EPSPYZ   ,EPSPZX  ,  
     5        DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ   ,DEPSZX  ,  
     6        EPSXX   ,EPSYY   ,EPSXY   ,EPSYZ    ,EPSZX   ,  
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),  
     8        SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ   ,SIGNZX  ,  
     9        SIGVXX  ,SIGVYY  ,SIGVXY  ,SIGVYZ   ,SIGVZX  ,  
     A        SSP     ,VISCMX  ,THKN    ,LBUF%PLA ,UVAR    ,
     B        OFF     ,NGL     ,PM      ,MATLY(JMLY),ETSE  ,
     C        SHF     ,SIGY    ,EPSPL   ,LBUF%ANG ,
     D        ALDT    ,LSENSOR ,SENSOR  ,ISMSTR   ,TABLE  ,
     E        GBUF%OFF)                          
c
          ELSEIF (ILAW == 60) THEN
           IF (IGTYP /= 10.AND.IGTYP /= 11 .AND.IGTYP /= 17) THEN
            CALL SIGEPS60C(
     1        JLT    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,
     2        NPF    ,NPT    ,IPT  ,IFLAG   ,
     2        TF     ,TT     ,DT1C    ,BUFMAT  ,RHO,
     3        AREA   ,EINT   ,THKLYL   ,
     4        EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5        DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6        EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8        SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9        SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A        SSP    ,VISCMX ,THKN    ,LBUF%PLA ,UVAR   ,
     B        OFF    ,NGL    ,IPM     ,MATLY(JMLY),ETSE ,
     C        GS     ,SIGY   ,EPSPL   ,ISRATE   ,DPLA,SHF,
     D        INLOC  ,VARNL(1,IT))
           ELSE
            CALL SIGEPS60C(
     1        JLT    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,
     2        NPF    ,NPT    ,IPT  ,IFLAG   ,
     2        TF     ,TT     ,DT1C    ,BUFMAT  ,RHO,
     3        AREA   ,EINT   ,THKLYL   ,
     4        EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5        DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6        EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8        SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9        SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A        SSP    ,VISCMX ,THKN    ,LBUF%PLA ,UVAR   ,
     B        OFF    ,NGL    ,IPM     ,MATLY(JMLY),ETSE ,
     C        GS     ,SIGY   ,EPSPL   ,ISRATE   ,DPLA,SHF,
     D        INLOC  ,VARNL(1,IT))
           END IF
           ELSEIF (ILAW == 62) THEN  
            CALL SIGEPS62C(
     1         JLT    , NUPARAM, NUVAR   , NFUNC , IFUNC , NPF   ,
     2         NPT    , IPT    , IFLAG   ,
     2         TF     , TT     , DT1C    , UPARAM, RHO  , 
     3         AREA   , EINT   , THKLYL  ,
     4         EPSPXX , EPSPYY , EPSPXY  , EPSPYZ, EPSPZX, 
     5         DEPSXX , DEPSYY , DEPSXY  , DEPSYZ, DEPSZX,
     6         EPSXX  , EPSYY  , EPSXY   , EPSYZ , EPSZX ,
     7         LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8         SIGNXX , SIGNYY , SIGNXY  , SIGNYZ, SIGNZX,
     9         SIGVXX , SIGVYY , SIGVXY  , SIGVYZ, SIGVZX,
     A         SSP    , VISCMX , THKN    ,UVAR   , OFF   ,
     B         NGL    , ISMSTR , IPM     , GS      ) 
           ELSEIF (ILAW == 63) THEN       
           CALL SIGEPS63C(
     1        JLT    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,
     2        NPF    ,NPT    ,IPT,IFLAG   ,
     2        TF     ,TT     ,DT1C    ,UPARAM  ,RHO,
     3        AREA   ,EINT   ,THKLYL   ,
     4        EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5        DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6        EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8        SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9        SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A        SSP    ,VISCMX ,THKN    ,LBUF%PLA ,UVAR   ,
     B        OFF    ,NGL    ,
     C        ETSE   ,GS     ,VOL0   ,SIGY      , TEMPEL,
     D        DIE    ,COEF   ,INLOC  ,VARNL(1,IT))
          ELSEIF (ILAW == 64) THEN       
           CALL SIGEPS64C(
     1        JLT    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,
     2        NPF    ,NPT    ,IPT,IFLAG   ,
     2        TF     ,TT     ,DT1C    ,UPARAM  ,RHO,
     3        AREA   ,EINT   ,THKLYL   ,
     4        EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5        DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6        EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8        SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9        SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A        SSP    ,VISCMX ,THKN    ,LBUF%PLA ,UVAR   ,
     B        OFF    ,NGL    ,IPM     ,MATLY(JMLY),
     C        ETSE   ,GS     ,VOL0   ,SIGY      ,TEMPEL ,
     D        DIE    ,COEF   ,INLOC  ,VARNL(1,IT))
          ELSEIF (ILAW == 65) THEN
           CALL SIGEPS65C(
     1        JLT    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,
     2        NPF    ,NPT    ,IPT,IFLAG   ,
     2        TF     ,TT     ,DT1C    ,BUFMAT  ,RHO,
     3        AREA   ,EINT   ,THKLYL   ,
     4        EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5        DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6        EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8        SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9        SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A        SSP    ,VISCMX ,THKN    ,LBUF%PLA ,UVAR   ,
     B        OFF    ,NGL    ,IPM     ,MATLY(JMLY),ETSE ,
     C        GS     ,SIGY   ,EPSPL   ,ISRATE   ,DPLA   )
          ELSEIF (ILAW == 66) THEN
            CALL SIGEPS66C(
     1        JLT    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,
     2        NPF    ,NPT    ,IPT     ,IFLAG   ,
     2        TF     ,TT     ,DT1C    ,BUFMAT  ,RHO     ,
     3        AREA   ,EINT   ,THKLYL  ,
     4        EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5        DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6        EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8        SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9        SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A        SSP    ,VISCMX ,THKN    ,LBUF%PLA ,UVAR   ,
     B        OFF    ,NGL    ,IPM     ,MATLY(JMLY),ETSE ,
     C        GS     ,SIGY   ,EPSPL   ,ISRATE   ,INLOC  ,
     D        VARNL(1,IT))
          ELSEIF (ILAW == 69) THEN
           CALL SIGEPS69C(
     1         JLT    , NUPARAM, NUVAR   , NFUNC , IFUNC , NPF   ,
     2         NPT    , IPT ,
     2         TF     , TT     , DT1C    , BUFMAT, RHO   ,  
     3         AREA   , EINT   , THKLYL  ,
     4         EPSPXX , EPSPYY , EPSPXY  , EPSPYZ, EPSPZX, 
     5         DEPSXX , DEPSYY , DEPSXY  , DEPSYZ, DEPSZX,
     6         EPSXX  , EPSYY  , EPSXY   , EPSYZ , EPSZX ,
     7         LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8         SIGNXX , SIGNYY , SIGNXY  , SIGNYZ, SIGNZX,
     9         SIGVXX , SIGVYY , SIGVXY  , SIGVYZ, SIGVZX,
     A         SSP    , VISCMX , THKN    , UVAR  ,
     B         NGL    , OFF    , ISMSTR  , IPM   , GS    ,
     C         MAT    , NUVARV , UVARV )
          ELSEIF (ILAW == 71) THEN !ISMSTR = 10 always (set in cgrtails)
           CALL SIGEPS71C(
     1        JLT    ,NUPARAM,NUVAR   ,NFUNC    ,IFUNC  ,
     2        NPF    ,NPT    ,IPT     ,IFLAG    ,
     2        TF     ,TT     ,DT1C    ,UPARAM   ,RHO    ,
     3        AREA   ,EINT   ,THKLYL  ,
     4        EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5        DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6        EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8        SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9        SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A        SSP    ,VISCMX ,THKN    ,LBUF%PLA ,UVAR   ,
     B        OFF    ,NGL    ,IPM     ,MATLY(JMLY),ETSE ,
     C        GS     ,SIGY   ,VOL0    ,TEMPEL) 
          ELSEIF (ILAW == 72) THEN 
            CALL SIGEPS72C(
     1         JLT      ,NUPARAM  ,NUVAR    ,
     2         TT       ,DT1C     ,UPARAM   ,RHO      ,THKLYL   ,
     3         DEPSXX   ,DEPSYY   ,DEPSXY   ,DEPSYZ   ,DEPSZX   ,
     4         LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     5         SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     6         SSP      ,THKN     ,LBUF%PLA ,UVAR     ,OFF      ,
     7         ETSE     ,GS       ,SIGY     ,HARDM    ,LBUF%SEQ ,
     8         DPLA     ,LBUF%DMG ,INLOC    ,VARNL(1,IT))
          ELSEIF (ILAW == 73) THEN
            CALL SIGEPS73C(
     1         JLT    ,NUPARAM,NUVAR   ,TT      ,DT1C    ,
     2         UPARAM ,RHO    ,AREA    ,EINT   ,THKLYL   ,
     4         EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5         DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6         EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7         LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8         SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9         SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A         SSP    ,VISCMX ,THKN    ,LBUF%PLA ,UVAR   ,
     B         OFF    ,NGL    ,ITABLE  ,ETSE     ,
     C         GS     ,SIGY   ,DPLA    ,EPSPL    ,TABLE  ,
     D         VOL0   ,TEMPEL ,DIE     ,COEF     ,NPF     ,
     E         NFUNC  ,IFUNC  ,TF      ,SHF      ,HARDM  ,
     F         LBUF%SEQ,INLOC ,VARNL(1,IT))
c
          ELSEIF (ILAW == 76) THEN

            CALL SIGEPS76C(
     1         JLT      ,NUPARAM  ,NUVAR    ,NFUNC    ,IFUNC    ,
     2         NPF      ,TF       ,MATPARAM ,TT       ,DT1      ,
     3         UPARAM   ,UVAR     ,RHO      ,OFF      ,NGL      ,
     4         DEPSXX   ,DEPSYY   ,DEPSXY   ,DEPSYZ   ,DEPSZX   ,
     5         LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     6         SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     7         SSP      ,THKN     ,THKLYL   ,LBUF%PLA ,EPSP     ,
     8         ETSE     ,GS       ,SIGY     ,INLOC    ,VARNL(1,IT))
c
               EPSPL(1:NEL) = EPSP(1:NEL)
     
c
          ELSEIF (ILAW == 78) THEN
              CALL SIGEPS78C(
     1         JLT      ,NUPARAM  ,NUVAR    ,NVARTMP  ,TT       ,
     2         NFUNC    ,IFUNC    ,NPF      ,TF       ,UPARAM   ,
     3         THKLYL   ,THKN     ,GS       ,ETSE     ,SIGY     , 
     4         DEPSXX   ,DEPSYY   ,DEPSXY   ,DEPSYZ   ,DEPSZX   ,
     5         LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     6         SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     7         SSP      ,UVAR     ,LBUF%SIGA,LBUF%SIGB,LBUF%SIGC,
     8         RHO      ,OFF      ,LBUF%PLA ,DPLA     ,VARTMP   ,
     9         INLOC    ,VARNL(1,IT),ET_IMP ,LBUF%SEQ )   
          ELSEIF (ILAW == 80) THEN  
            IF (ISRATE == 0) THEN
              DO I=JFT,JLT
                EPS_K2 = (KXX(I)*KXX(I)+KYY(I)*KYY(I)+KXX(I)*KYY(I)
     .                 + FOURTH*KXY(I)*KXY(I)) *THK(I)*THK(I)*ONE_OVER_9
                EPS_M2 = FOUR_OVER_3*(EXX(I)*EXX(I)+EYY(I)*EYY(I)+EXX(I)*EYY(I)
     .                 + FOURTH*EXY(I)*EXY(I))
                DTINV  = DT_INV(I)!DT1C(I)/MAX(DT1C(I)**2,EM20)
                EPSP(I) = SQRT(EPS_M2+EPS_K2)*DTINV 
                EPSPL(I) = EPSP(I)
              ENDDO 
            ENDIF
            CALL SIGEPS80C(
     1         JLT    ,NUPARAM,NUVAR   ,NFUNC    ,IFUNC   ,
     2         NPF    ,NPT    ,IPT     ,IFLAG    ,
     2         TF     ,TT     ,DT1C    ,UPARAM   ,RHO     ,
     3         AREA   ,EINT   ,THKLYL  ,
     4         EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX  ,
     5         DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX  ,
     6         EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX   ,
     7         LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8         SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX  ,
     9         SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX  ,
     A         SSP    ,VISCMX ,THKN    ,LBUF%PLA ,UVAR    ,
     B         OFF    ,NGL    ,PM      ,IPM      ,MATLY(JMLY),    
     C         ETSE   ,GS     ,VOL0    ,SIGY     ,TEMPEL  ,
     D         DIE    ,COEF   ,SHF     ,EPSPL    ,TABLE   ,
     E         ITHK   ,NVARTMP,VARTMP  ,EPSTHTOT)
          ELSEIF (ILAW == 82) THEN
              CALL SIGEPS82C(
     1        JLT    ,NUPARAM,NUVAR   ,NFUNC  ,IFUNC  , 
     2        NPF    ,NPT    ,IPT     ,IFLAG  ,
     2        TF     ,TT     ,DT1C    ,
     3        UPARAM ,RHO    ,AREA    ,EINT   ,THKLYL,
     4        EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ ,EPSPZX , 
     5        DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ ,DEPSZX ,
     6        EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ  ,EPSZX  ,
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8        SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ ,SIGNZX ,
     9        SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ ,SIGVZX ,
     A        SSP    ,VISCMX ,THKN    ,UVAR   ,
     B        NGL    ,OFF    ,ISMSTR  ,IPM    , GS    )
          ELSEIF(ILAW == 85)THEN  
           CALL SIGEPS85C_VOID(
     1        JLT    ,NUPARAM,NUVAR   ,NFUNC    ,IFUNC  ,
     2        NPF    ,NPT    ,IPT     ,IFLAG    ,
     2        TF     ,TT     ,DT1C    ,UPARAM   ,RHO    ,
     3        AREA   ,EINT   ,THKLYL  ,
     4        EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5        DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6        EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8        SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9        SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A        SSP    ,VISCMX ,THKN    ,LBUF%PLA ,UVAR   ,
     B        OFF    ,NGL    ,SHF     )
          ELSEIF (ILAW == 86) THEN
           CALL SIGEPS86C(
     1        JLT    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,
     2        NPF    ,NPT    ,IPT     ,IFLAG   ,
     2        TF     ,TT     ,DT1C    ,UPARAM  ,RHO,
     3        AREA   ,EINT   ,THKLYL  ,
     4        EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5        DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6        EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8        SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9        SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A        SSP    ,VISCMX ,THKN    ,LBUF%PLA ,UVAR   ,
     B        OFF    ,NGL    ,IPM     ,MATLY(JMLY),ETSE ,
     C        GS     ,SIGY   ,EPSPL   ,ISRATE   ,DPLA)
          ELSEIF (ILAW == 87) THEN
            ASRATE = PM(9,MX)
            CALL SIGEPS87C(
     1        JLT    ,NUPARAM,NUVAR   ,NFUNC    ,IFUNC  ,
     2        NPF    ,TF     ,TT     ,DT1C    ,UPARAM   ,
     4        UVAR   ,RHO    ,THKLYL ,THKN    ,OFF    ,
     5        EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     6        DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     7        LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8        SIGNXX ,SIGNYY   ,SIGNXY  ,SIGNYZ     ,SIGNZX ,
     9        SSP    ,LBUF%PLA ,DPLA    ,EPSPL      ,SIGY   ,
     A        ETSE   ,GS       ,ISRATE  ,ASRATE     ,YLDFAC ,
     B        TEMPEL,LBUF%SIGB ,INLOC   ,VARNL(1,IT),LBUF%SEQ)
         ELSEIF (ILAW == 88) THEN
           CALL SIGEPS88C(
     1         JLT    , NUPARAM, NUVAR   , NFUNC , IFUNC , NPF   ,
     2         NPT    , IPT     ,NGL     , OFF    , ISMSTR   , GS ,   
     2         TF     , TT     , DT1C    , BUFMAT(IADBUF), RHO   ,  
     3         AREA   , EINT   , THKLYL  ,
     4         EPSPXX , EPSPYY , EPSPXY  , EPSPYZ, EPSPZX, 
     5         DEPSXX , DEPSYY , DEPSXY  , DEPSYZ, DEPSZX,
     6         EPSXX  , EPSYY  , EPSXY   , EPSYZ , EPSZX ,
     7         LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8         SIGNXX , SIGNYY , SIGNXY  , SIGNYZ, SIGNZX,
     A         SSP    , VISCMX , THKN    , UVAR )          
          ELSEIF (ILAW == 93) THEN     
            CALL SIGEPS93C(
     1         JLT      ,NUPARAM  ,NUVAR    ,NFUNC    ,IFUNC    ,
     2         NPF      ,TF       ,TT       ,DT1C     ,UPARAM   ,
     3         EPSPXX   ,EPSPYY   ,EPSPXY   ,EPSPYZ   ,EPSPZX   ,
     4         DEPSXX   ,DEPSYY   ,DEPSXY   ,DEPSYZ   ,DEPSZX   ,
     5         LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     6         SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     7         SSP      ,THKN     ,LBUF%PLA ,UVAR     ,RHO      ,
     8         OFF      ,ETSE     ,THKLYL   ,SHF      ,SIGY     ,
     9         HARDM    ,LBUF%SEQ ,EPSP     ,ASRATE   ,NVARTMP  ,
     A         VARTMP   ,DPLA     ,INLOC    ,VARNL(1,IT))
c
          ELSEIF (ILAW == 96)THEN 
            CALL SIGEPS96C(
     1         JLT     ,NGL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,
     2         NPF     ,TF      ,UPARAM  ,UVAR    ,JTHE    ,GS      ,
     3         RHO     ,TEMPEL  ,LBUF%PLA,SSP     ,OFF     ,EPSPL   ,
     4         EPSPXX  ,EPSPYY  ,EPSPXY  ,EPSPYZ  ,EPSPZX  ,THKLYL  ,
     5         DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,THKN    ,
     6         LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     7         SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  )
c  
          ELSEIF (ILAW == 104 )THEN
            CALL SIGEPS104C(
     1         JLT     ,NGL     ,IPG     ,ILAYER  ,IT      ,NUPARAM ,NUVAR   ,NFUNC   ,
     2         IFUNC   ,NPF     ,TF      ,DT1C    ,TT      ,UPARAM  ,UVAR    ,JTHE    ,
     3         RHO     ,TEMPEL  ,LBUF%PLA,DPLA    ,SSP     ,LBUF%OFF,LBUF%EPSD,GS      ,
     5         DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,THKLYL  ,OFF     ,
     6         LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     7         THKN    ,SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,VISCMX  ,SIGY    ,
     8         ETSE    ,VARNL(1,IT)      ,LBUF%DMG,LBUF%TEMP        ,LBUF%SEQ,ELBUF_STR%NPTR,
     9         ELBUF_STR%NPTS,ELBUF_STR%NPTT,BUFLY,INLOC   ,EPSPL   )
!
          ELSEIF (ILAW == 107 )THEN
            CALL SIGEPS107C(
     1         JLT     ,NGL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,NPF     ,
     2         TF      ,DT1C    ,TT      ,UPARAM  ,UVAR    ,JTHE    ,RHO     ,
     3         LBUF%PLA,DPLA    ,SSP     ,LBUF%EPSD,OFF     ,
     4         DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,SHF     ,
     5         LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     6         SIGY    ,SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7         ETSE    ,NUMTABL  ,ITABLE  ,TABLE   ,NVARTMP ,VARTMP   )
!
          ELSEIF (ILAW == 109 )THEN
            CALL SIGEPS109C(
     1         JLT     ,NGL     ,NUPARAM ,NUVAR   ,NVARTMP  ,NUMTABL  ,
     2         UPARAM  ,UVAR    ,VARTMP  ,ITABLE  ,TABLE    ,JTHE     ,
     3         TT      ,DT1C    ,OFF     ,RHO     ,LBUF%PLA ,DPLA     ,
     4         SSP     ,SIGY    ,ETSE    ,EL_TEMP ,EPSP     ,GS       ,
     5         DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX   ,
     6         LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     7         SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX   ,
     8         THKN    ,THKLYL  ,INLOC   ,VARNL(1,IT))     
!
          ELSEIF (ILAW == 110 )THEN
            CALL SIGEPS110C(
     1         JLT     ,NGL     ,NUPARAM ,NUVAR   ,NPF      ,
     2         TT      ,DT1C    ,UPARAM  ,UVAR    ,JTHE     ,OFF     ,
     3         GS      ,RHO     ,LBUF%PLA,DPLA    ,EPSP     ,SSP     ,
     4         DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX   ,ASRATE  ,
     5         EPSPXX  ,EPSPYY  ,EPSPXY  ,EPSPYZ  ,EPSPZX   ,
     6         LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     7         SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX   ,THKLYL  ,
     8         THKN    ,SIGY    ,ETSE    ,TEMPEL  ,LBUF%TEMP,LBUF%SEQ,
     9         TF      ,NUMTABL ,ITABLE  ,TABLE   ,NVARTMP  ,VARTMP  ,
     A         LBUF%SIGA,INLOC  ,VARNL(1,IT))   
!
          ELSEIF (ILAW == 112 )THEN
            CALL SIGEPS112C(
     1         JLT     ,NGL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,NPF     ,
     2         TF      ,DT1C    ,TT      ,UPARAM  ,UVAR    ,JTHE    ,RHO     ,
     3         LBUF%PLA,DPLA    ,SSP     ,LBUF%EPSD,OFF     ,
     4         DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,SHF     ,
     5         LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     6         SIGY    ,SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     7         ETSE    ,NUMTABL  ,ITABLE  ,TABLE   ,NVARTMP ,VARTMP   )
!          
          ELSEIF (ILAW == 119 )THEN

            CALL SIGEPS119C(
     .           NEL      ,IPT      ,NPT      ,NUPARAM  ,NUVAR    ,
     .           NUMTABL  ,ITABLE   ,TABLE    ,UVAR     ,UPARAM   ,
     .           THKN     ,THKLYL   ,SHF      ,SSP      ,OFF      ,
     .           FLAG_ZCFAC,ZCFAC   ,DEPSXX   ,DEPSYY   ,DEPSXY   ,
     .           EPSXX    ,EPSYY    ,EPSXY    ,EPSYZ    ,EPSZX    ,
     .           LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     .           SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   , 
     .           WMC      ,THLY     ,THK0) 
!          
          ELSEIF (ILAW == 121 )THEN
            CALL SIGEPS121C(
     1         JLT     ,NGL     ,NUPARAM ,NUVAR   ,NFUNC   ,IFUNC   ,NPF     ,
     2         TF      ,DT1C    ,TT      ,UPARAM  ,UVAR    ,RHO     ,LBUF%PLA,
     3         DPLA    ,SSP     ,LBUF%EPSD,GS     ,THKN    ,THKLYL  ,OFF     ,
     5         DEPSXX  ,DEPSYY  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     6         EPSPXX  ,EPSPYY  ,EPSPXY  ,EPSPYZ  ,EPSPZX  ,EPSP    ,EPSPL   ,
     7         LBUF%SIG(IJ1),LBUF%SIG(IJ2),LBUF%SIG(IJ3),LBUF%SIG(IJ4),LBUF%SIG(IJ5),
     8         SIGNXX  ,SIGNYY  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     9         SIGY    ,ETSE    ,VARNL(1,IT),INLOC,GBUF%DT ,
     A         IPG     ,IT      ,ELBUF_STR%NPTR,ELBUF_STR%NPTS,ELBUF_STR%NPTT,
     B         BUFLY   ,LBUF%OFF)
c
          ENDIF !ILAW
c-------------------------------------------
          DO I=JFT,JLT
            VISCMX(I) = MAX(DM,VISCMX(I))
          ENDDO
c-------------------------------------------
c         End of material laws
C-----------------------------------------------
C         Failure models
C-----------------------------------------------
          IF (IFAILURE == 1) THEN   
            IF ((ITASK==0).AND.(IMON_MAT==1))CALL STARTIME(121,1)
c
            IF (IXFEM > 0) THEN                              
              DO I=JFT,JLT                                                    
                TENSX(I,1) = ZERO                                             
                TENSX(I,2) = ZERO                                             
                TENSX(I,3) = ZERO                                             
                TENSX(I,4) = ZERO                                             
                TENSX(I,5) = ZERO                                             
              ENDDO                                                           
            ENDIF                                                             
C
            SIGOFF(1:NEL) = ONE
            DMG_LOC_SCALE(1:NEL) = ONE  ! local dmg scale by integration point and failure model
c                                        
            MPT  = NPTTOT * MAX(1,NPG)
            FBUF => BUFLY%FAIL(IR,IS,IT)  
c
            ! Length used for regularization (failure criterion parameters scaling)
            !  -> If non-local, criterion parameters are scaled with LE_MAX parameter
            IF (INLOC > 0) THEN 
              LE_MAX(1:NEL) = NLOC_DMG%LE_MAX(MAT(1))
              EL_LEN => LE_MAX(1:NEL)
            !  -> If not, criterion parameters are scaled with the element characteristic length
            ELSE
              EL_LEN => ALDT(1:NEL)
            ENDIF
c
C           plastic strain increment for all plastic laws                   
            IF (BUFLY%L_PLA > 0) THEN
              ! Non-local material
              IF (INLOC > 0) THEN
                DO I=JFT,JLT
                  DPLA(I)  = MAX(VARNL(I,IT),ZERO)
                  EPSPL(I) = LBUF%EPSDNL(I)
                ENDDO
                EL_PLA => LBUF%PLANL(1:NEL)
              ! Classical local material
              ELSE
                DO I=JFT,JLT
                  DPLA(I) = LBUF%PLA(I) - PLA0(I)       
                ENDDO
                EL_PLA => LBUF%PLA(1:NEL)
              ENDIF
            ENDIF
c
            IF (IXFEM == 1) THEN
              UELR1 => ELBUF_STR%BUFLY(ILAYER)%UELR1
            ELSE
              UELR1 => GBUF%UELR1
            ENDIF
            IF (IXLAY == 0) THEN  ! standard element
              UELR  => GBUF%UELR
            ELSEIF (IXLAY > 0) THEN ! xfem phantom element
              UELR  => ELBUF_STR%BUFLY(IXLAY)%UELR
            ENDIF
            DADV => GBUF%DMG(1:NEL)  ! used outside this routine for frontwave propagation
c---
            OFFL => LBUF%OFF
            OFFLY=> BUFLY%OFF
            ZT   = ZZ(1)
c---
            DO IFL = 1, NFAIL      ! loop over fail models in current layer                                      
              IP   = (IFL - 1)*15                                             
              UVARF  => FBUF%FLOC(IFL)%VAR
              NVARF  =  FBUF%FLOC(IFL)%NVAR                                 
              IRUPT  =  FBUF%FLOC(IFL)%ILAWF                                
              DAM    => FBUF%FLOC(IFL)%DAM
              DFMAX  => FBUF%FLOC(IFL)%DAMMX 
              TDEL   => FBUF%FLOC(IFL)%TDEL   
              FLD_IDX=> FBUF%FLOC(IFL)%INDX
              FOFF   => FBUF%FLOC(IFL)%OFF
              NUPAR   = IPM(112 + IP, IMAT)
              NFUNC1  = IPM(115 + IP, IMAT)
              IADBUFR = IPM(114 + IP ,IMAT)
              UPARAMF => BUFMAT(IADBUFR:IADBUFR+NUPAR)
              IF (IXFEM == 0) PTHKF(ILAYER,IFL) = ZERO
              DO I=1,NFUNC1                
                IFUNC1(I) = IPM(115+ IP + I,IMAT)
              ENDDO 
c------------------------------------            
              SELECT CASE (IRUPT)                                            
c------------------------------------            
              CASE (1)     !    Johnson-Cook                                                   
                IF (IXFEM == 0) THEN
                  CALL FAIL_JOHNSON_C(
     1               NEL       ,NUPAR     ,NVARF     ,UPARAMF   ,UVARF     ,
     2               TT        ,NGL       ,IPG       ,ILAYER    ,IT        ,
     3               SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ    ,SIGNZX    ,
     4               DPLA      ,EPSPL     ,TSTAR     ,OFF       ,FOFF      ,
     5               PTHKF(ILAYER,IFL),DFMAX,TDEL    )
                ELSE
                  CALL FAIL_JOHNSON_XFEM(
     1                NEL      ,NUPAR    ,UPARAMF  ,NVARF    ,UVARF    ,
     2                TT       ,TENSX    ,DPLA     ,EPSPL    ,TSTAR    ,
     3                NGL      ,IPT      ,MPT      ,NPTT     ,UELR1    ,
     4                SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     5                OFF      ,OFFL     ,GBUF%NOFF,DFMAX    ,TDEL     ,
     6                ELCRKINI ,IXFEM    ,IXEL     ,ILAYER   ,IT       )
                ENDIF
c 
              CASE (2)     !    Tuler Butcher                                               
                IF (IXFEM == 0) THEN
                  CALL FAIL_TBUTCHER_C(
     1               NEL       ,NUPARAM   ,NVARF     ,UPARAM    ,UVARF     , 
     2               TT        ,DT1C      ,IPG       ,ILAYER    ,IT        , 
     3               SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ    ,SIGNZX    , 
     4               NGL       ,OFF       ,FOFF ,PTHKF(ILAYER,IFL),DFMAX   , 
     5               TDEL      )
                ELSE
                  CALL FAIL_TBUTCHER_XFEM(
     1                NEL      ,NUPAR    ,UPARAMF  ,NVARF    ,UVARF    ,
     2                TT       ,DT1C     ,TENSX    ,DFMAX    ,TDEL     ,
     3                SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     4                NGL      ,IPT      ,MPT      ,
     5                GBUF%NOFF,OFF      ,OFFL     ,ELCRKINI ,IXFEM    ,
     6                IXEL     ,ILAYER   ,IT       ,NPTT     ,UELR1    )
                ENDIF
c
              CASE (3)     !    Wilkins
                CALL FAIL_WILKINS_C(
     1               NEL       ,NUPAR     ,NVARF     ,UPARAMF   ,UVARF     ,
     2               TT        ,NGL       ,IPG       ,ILAYER    ,IT        ,
     3               SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ    ,SIGNZX    ,
     4               DPLA      ,FOFF ,PTHKF(ILAYER,IFL),DFMAX   ,TDEL      )      
c
              CASE (4)     !    User1         
                  DO I=JFT,JLT
                    COPY_PLA(I) = LBUF%PLA(I)
                  ENDDO
                IF (LOGICAL_USERL_AVAIL)THEN
                  TT_LOCAL = TT
                  CALL ENG_USERLIB_FLAWC(IRUPT,NEL ,NUPAR,NVARF,NFUNC1,IFUNC1,NPF,
     2                     TF  ,TT_LOCAL    ,DT1C  ,UPARAMF,NGL ,IPT, 
     3                     MPT ,IPG ,IBIDON2,IBIDON3  , 
     4                     SIGNXX   ,SIGNYY ,SIGNXY ,SIGNYZ ,SIGNZX  ,
     5                     EPSPXX   ,EPSPYY ,EPSPXY ,EPSPYZ ,EPSPZX  ,
     6                     EPSXX    ,EPSYY  ,EPSXY  ,EPSYZ  ,EPSZX   ,
     7                     COPY_PLA ,DPLA   ,EPSPL  ,UVARF  ,UELR    ,
     8                     OFF      ,ALDT   ,AREA   ,BIDON3,BIDON4,BIDON5 )
                ELSE
!                  CALL F04LAWC(NEL ,NUPAR,NVARF,NFUNC1,IFUNC1,NPF,
!     2                     TF  ,TT  ,DT1C   ,UPARAMF,NGL ,IPT, 
!     3                     MPT ,IPG ,IBIDON2,IBIDON3  , 
!     4                     SIGNXX   ,SIGNYY ,SIGNXY ,SIGNYZ ,SIGNZX  ,
!     5                     EPSPXX   ,EPSPYY ,EPSPXY ,EPSPYZ ,EPSPZX  ,
!     6                     EPSXX    ,EPSYY  ,EPSXY  ,EPSYZ  ,EPSZX   ,
!     7                     COPY_PLA ,DPLA   ,EPSPL  ,UVARF  ,UELR    ,
!     8                     OFF      ,ALDT   ,AREA   ,BIDON3,BIDON4,BIDON5 )       
!!!
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/FAIL/USER1 - SHELL '
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
!!!
                ENDIF
                DO I=JFT,JLT
                  LBUF%PLA(I) = COPY_PLA(I)
                ENDDO
                
c
              CASE (5)     !    User2         
                DO I=JFT,JLT
                  COPY_PLA(I) = LBUF%PLA(I)
                ENDDO
                IF (LOGICAL_USERL_AVAIL)THEN
                  TT_LOCAL = TT
                  CALL ENG_USERLIB_FLAWC(IRUPT,NEL ,NUPAR,NVARF,NFUNC1,IFUNC1,NPF,
     2                 TF  ,TT_LOCAL,DT1C  ,UPARAMF,NGL ,IPT, 
     3                 MPT ,IPG ,IBIDON2,IBIDON3  , 
     4                 SIGNXX   ,SIGNYY ,SIGNXY ,SIGNYZ ,SIGNZX  ,
     5                 EPSPXX   ,EPSPYY ,EPSPXY ,EPSPYZ ,EPSPZX  ,
     6                 EPSXX    ,EPSYY  ,EPSXY  ,EPSYZ  ,EPSZX   ,
     7                 COPY_PLA ,DPLA   ,EPSPL  ,UVARF  ,UELR    ,
     8                 OFF      ,ALDT   ,AREA   ,BIDON3,BIDON4,BIDON5 )
                ELSE
!                  CALL F05LAWC (NEL ,NUPAR,NVARF,NFUNC1,IFUNC1,NPF,
!     2                     TF  ,TT   ,DT1C  ,UPARAMF,NGL,IPT, 
!     3                     MPT ,IPG ,IBIDON2,IBIDON3   ,  
!     4                     SIGNXX   ,SIGNYY ,SIGNXY ,SIGNYZ ,SIGNZX  ,
!     5                     EPSPXX   ,EPSPYY ,EPSPXY ,EPSPYZ ,EPSPZX  ,
!     6                     EPSXX    ,EPSYY  ,EPSXY  ,EPSYZ  ,EPSZX   ,
!     7                     COPY_PLA ,DPLA   ,EPSPL  ,UVARF  ,UELR    ,
!     8                     OFF      ,ALDT   ,AREA   ,BIDON3,BIDON4,BIDON5 ) 
!!!
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/FAIL/USER2 - SHELL '
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
!!!
                ENDIF
                DO I=JFT,JLT
                  LBUF%PLA(I) = COPY_PLA(I)
                ENDDO

c
              CASE (6)     !    User3         
                DO I=JFT,JLT
                  COPY_PLA(I) = LBUF%PLA(I)
                ENDDO
                IF (LOGICAL_USERL_AVAIL)THEN
                  TT_LOCAL = TT
                  CALL ENG_USERLIB_FLAWC(IRUPT,NEL ,NUPAR,NVARF,NFUNC1,IFUNC1,NPF,
     2                 TF  ,TT_LOCAL      ,DT1C  ,UPARAMF,NGL ,IPT, 
     3                 MPT ,IPG ,IBIDON2,IBIDON3  , 
     4                 SIGNXX   ,SIGNYY ,SIGNXY ,SIGNYZ ,SIGNZX  ,
     5                 EPSPXX   ,EPSPYY ,EPSPXY ,EPSPYZ ,EPSPZX  ,
     6                 EPSXX    ,EPSYY  ,EPSXY  ,EPSYZ  ,EPSZX   ,
     7                 COPY_PLA ,DPLA   ,EPSPL  ,UVARF  ,UELR    ,
     8                 OFF      ,ALDT   ,AREA   ,BIDON3,BIDON4,BIDON5 )
                ELSE
!                  CALL F06LAWC(NEL   ,NUPAR,NVARF,NFUNC1,IFUNC1,NPF,
!     2              TF       ,TT     ,DT1C   ,UPARAMF,NGL   ,       
!     3              IPT      ,MPT    ,IPG,IBIDON2,IBIDON3,              
!     4              SIGNXX   ,SIGNYY ,SIGNXY ,SIGNYZ ,SIGNZX  ,             
!     5              EPSPXX   ,EPSPYY ,EPSPXY ,EPSPYZ ,EPSPZX  ,             
!     6              EPSXX    ,EPSYY  ,EPSXY  ,EPSYZ  ,EPSZX   ,             
!     7              COPY_PLA ,DPLA   ,EPSPL  ,UVARF  ,UELR    ,             
!     8              OFF      ,ALDT   ,AREA   ,BIDON3,BIDON4,BIDON5  )              
!!!
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/FAIL/USER3 - SHELL '
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
!!!
                ENDIF
                DO I=JFT,JLT
                  LBUF%PLA(I) = COPY_PLA(I)
                ENDDO
c
              CASE (7)     !    FLD
                DO I=JFT,JLT
                  ZT=ZZ(I)*THK0(I)
                  EPSXX(I)= GSTR(I,1)+ZT*GSTR(I,6)
                  EPSYY(I)= GSTR(I,2)+ZT*GSTR(I,7)
                  EPSXY(I)= GSTR(I,3)+ZT*GSTR(I,8)
                  EPSYZ(I)= GSTR(I,4)
                  EPSZX(I)= GSTR(I,5)
                ENDDO
                IF (IXFEM == 0) THEN
                  CALL FAIL_FLD_C(
     1            NEL       ,NUPAR     ,NVARF     ,NFUNC1   ,IFUNC1     ,
     2            NPF       ,TF        ,TT        ,DT1C     ,UPARAMF    ,   
     3            UVAR      ,NGL       ,IPG       ,ILAYER   ,IT         ,
     4            EPSXX     ,EPSYY     ,EPSXY     ,EPSYZ    ,EPSZX      ,
     5            ZT        ,OFF       ,FOFF    ,PTHKF(ILAYER,IFL),TDEL ,
     6            FLD_IDX   ,DAM       ,DFMAX     )
                ELSE
                  CALL FAIL_FLD_XFEM(                                      
     1            NEL       ,NUPAR    ,NVARF    ,NFUNC1   ,IFUNC1    ,
     2            NPF       ,TF       ,TT       ,UPARAMF  ,
     3            NGL       ,IPT      ,MPT      ,SSP      ,TENSX     ,
     4            SIGNXX    ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX    ,  
     5            EPSXX     ,EPSYY    ,EPSXY    ,EPSYZ    ,EPSZX     ,  
     6            UVARF     ,GBUF%NOFF,OFF      ,IP       ,
     7            ELCRKINI  ,IXFEM    ,IXEL     ,ILAYER   ,IT        ,  
     8            OFFL      ,NPTT     ,UELR1    ,DFMAX    ,TDEL      ,
     9            DAM       ,FLD_IDX  )
                ENDIF
c
              CASE (9)     !    Xue_wierzbicki
                CALL FAIL_WIERZBICKI_C(                                               
     1               NEL       ,NUPAR     ,UPARAMF   ,NVARF     ,UVARF     , 
     2               TT        ,NGL       ,IPG       ,ILAYER    ,IT        , 
     3               SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ    ,SIGNZX    ,               
     4               DPLA      ,OFF       ,FOFF ,PTHKF(ILAYER,IFL),DFMAX   ,
     5               TDEL      )           
c
              CASE (10)     !    Tension Strain failure model
                CALL FAIL_TENSSTRAIN_C(                                         
     1               NEL       ,NFUNC1    ,NUPAR     ,NVARF     ,IFUNC1    ,
     2               UPARAMF   ,UVARF     ,NPF       ,TF        ,TT        ,
     3               NGL       ,IPG       ,ILAYER    ,IT        ,EPSPL     ,
     4               EPSXX     ,EPSYY     ,EPSXY     ,EPSYZ     ,EPSZX     ,
     5               OFF       ,FOFF ,PTHKF(ILAYER,IFL),DFMAX   ,TDEL      ,
     6               DMG_FLAG  ,DMG_LOC_SCALE ,ALDT      ,TSTAR ,ISMSTR    )                                
c
              CASE (11)     !    Energy failure model
                CALL FAIL_ENERGY_C(                                            
     1               NEL       ,NUPAR     ,NVARF     ,NFUNC1   ,IFUNC1     , 
     2               UPARAMF   ,UVARF     ,NPF       ,TF       ,TT         , 
     3               NGL       ,IPG       ,ILAYER    ,IT       ,EPSPL      ,  
     4               EINT      ,AREA      ,THKN ,PTHKF(ILAYER,IFL),DMG_FLAG, 
     5               DMG_LOC_SCALE ,OFF       ,FOFF      ,DFMAX    ,TDEL       )
c
              CASE (13)     !    Chang-Chang failure model
                CALL FAIL_CHANGCHANG_C(
     1               NEL       ,NUPAR     ,NVARF     ,UPARAMF   ,UVARF     ,
     2               TT        ,DT1C      ,IPG       ,ILAYER    ,IT        ,
     3               NGL       ,DMG_FLAG  ,DMG_LOC_SCALE ,DFMAX     ,TDEL      ,
     4          SIGNXX   ,SIGNYY  ,SIGNXY  ,SIGNYZ   ,SIGNZX    ,        
     5               OFF       ,FOFF      ,PTHKF(ILAYER,IFL))
c
              CASE (14)     !    Hashin failure model  
                DO I=JFT,JLT        
                  EPSP(I) = MAX(ABS(EPSPXX(I)),ABS(EPSPYY(I)),
     .                          ABS(EPSPXY(I)),EM20)
                ENDDO            
                CALL FAIL_HASHIN_C(
     1               NEL       ,NUPAR     ,NVARF     ,UPARAMF   ,UVARF      ,
     2               TT        ,DT1C      ,IPG       ,ILAYER    ,IT         ,
     3               NGL       ,DMG_FLAG  ,DMG_LOC_SCALE ,DFMAX     ,TDEL       ,
     4               SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ    ,SIGNZX     ,        
     5               OFF       ,FOFF ,PTHKF(ILAYER,IFL),IGTYP   ,PLY_ID     ,
     6               EPSP      ,FWAVE_EL  ,GBUF%DMG  ) 
c
              CASE (16)     !    Modified Puck failure model
                CALL FAIL_PUCK_C(
     1               NEL       ,NUPAR     ,NVARF     ,UPARAMF   ,UVARF     ,
     2               TT        ,NGL       ,IPG       ,ILAYER    ,IT        ,
     3               SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ    ,SIGNZX    ,
     4               OFF       ,FOFF ,PTHKF(ILAYER,IFL),DMG_FLAG,DMG_LOC_SCALE ,
     5               DFMAX     ,TDEL      )
c
              CASE (23)     !    Tabulated failure model
                IF (IXFEM == 0) THEN
                  CALL FAIL_TAB_C(
     1                NEL       ,NUPAR     ,NVARF     ,UPARAMF   ,UVARF     ,
     2                NFUNC1    ,IFUNC1    ,TABLE     ,NPF       ,TF        ,
     3                TT        ,NGL       ,IPG       ,ILAYER    ,IT        , 
     4                SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ    ,SIGNZX    ,      
     5                DPLA      ,EPSPL     ,THKN      ,EL_LEN    ,TSTAR     ,
     6                PTHKF(ILAYER,IFL),DMG_FLAG,DMG_LOC_SCALE ,OFF  ,FOFF      ,
     7                DFMAX     ,TDEL      ,IGTYP     ,INLOC   )
                ELSE
                  CALL FAIL_TAB_XFEM(
     1                NEL      ,NUPAR    ,NVARF    ,NPF      ,TF       ,
     2                TT       ,DT1C     ,UPARAMF  ,NGL      ,IPT      , 
     3                MPT      ,IP       ,NFUNC1   ,IFUNC1   ,TABLE    ,
     4                SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     5                DPLA     ,EPSPL    ,TSTAR    ,TENSX    ,UVARF    ,
     6                GBUF%NOFF,ALDT     ,OFF      ,OFFL     ,ELCRKINI ,
     7                IXFEM    ,IXEL     ,ILAYER   ,DFMAX    ,TDEL     ,
     8                DMG_FLAG )
                ENDIF
c
              CASE (24)     !    Orthotropic strain failure model
                CALL FAIL_ORTHSTRAIN_C(
     1              NEL       ,NUPAR     ,NVARF     ,UPARAMF   ,UVARF     ,
     2              NFUNC1    ,IFUNC1    ,NPF       ,TF        ,NGL       ,
     3              TT        ,DT1       ,IPG       ,ILAYER    ,IT        , 
     4              EPSXX     ,EPSYY     ,EPSXY     ,DMG_FLAG  ,DMG_LOC_SCALE ,
     5              EPSPXX    ,EPSPYY    ,EPSPXY    ,ALDT      ,ISMSTR    ,
     6              SIGNXX    ,SIGNYY    ,SIGNXY    ,PTHKF(ILAYER,IFL)    ,       
     7              OFF       ,OFFLY     ,FOFF      ,DFMAX     ,TDEL      )
c
              CASE (25)     !    NXT Failure
                CALL FAIL_NXT_C(
     1               NEL       ,NUPAR     ,NVARF     ,UPARAMF   ,UVARF     ,
     2               TT        ,NPF       ,TF        ,NFUNC1    ,IFUNC1    ,   
     3               NGL       ,IPG       ,ILAYER    ,IT        ,HARDM     ,   
     4               SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ    ,SIGNZX    ,
     5               OFF       ,FOFF ,PTHKF(ILAYER,IFL),DFMAX   ,TDEL      ,   
     6               DAM       )
c
              CASE (28)     !    windshield failure (Christian Alter model)
                IROT   =  ELBUF_STR%BUFLY(ILAYER)%LY_DIRA
                CRKDIR => ELBUF_STR%BUFLY(ILAYER)%CRKDIR
                IFAILWV = FAILWAVE%WAVE_MOD
                PROGRESSIVE_CRACK = 1
                ORTH_DAMAGE = 1
c
                IF (IXFEM > 0) THEN
                  IF (IXEL == 0) THEN
                    CRKLEN => ELBUF_STR%BUFLY(ILAYER)%DMG(1:NEL)
                  ELSE
                    CRKLEN => ALDT(1:NEL)
                  ENDIF
                  CALL FAIL_WIND_XFEM(
     1            NEL        ,NUPAR      ,NVARF      ,UPARAMF    ,UVARF      ,
     2            TT         ,DT1        ,SSP        ,ALDT       ,CRKLEN     ,
     3            ELCRKINI   ,UELR1      ,OFF        ,OFFLY      ,
     4            SIGNXX     ,SIGNYY     ,SIGNXY     ,SIGNYZ     ,SIGNZX     ,
     5            NGL        ,IXEL       ,ILAYER     ,IPT        ,NPTT       ,
     6            IXFEM      ,IROT       ,DIR_A(JDIR),DIR1_CRK   ,DIR2_CRK   ,
     7            CRKDIR     )
c                  
                ELSEIF (IFAILWV > 0) THEN                  
                  CALL FAIL_WIND_FRWAVE(
     1            NEL        ,NUPAR      ,NVARF      ,UPARAMF    ,UVARF      ,
     2            TT         ,DT1        ,SSP        ,ALDT       ,FWAVE_EL   ,
     3            UELR       ,UELR1      ,OFF        ,OFFLY      ,FOFF       ,
     4            SIGNXX     ,SIGNYY     ,SIGNXY     ,DFMAX      ,NGL        ,
     5            ILAYER     ,IPT        ,NPTT       ,CRKDIR     ,DADV       ,
     6            DMG_FLAG   ,TRELAX     ,THK0       )
                ENDIF
c
              CASE (30)     !    Biquadratic failure model
                CALL FAIL_BIQUAD_C(
     1          JLT      ,NVARF   ,
     2          TT       ,UPARAMF ,NGL      ,IPT      ,MPT      ,
     3          SIGNXX   ,SIGNYY  ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     4          DPLA     ,EPSPL   ,UVARF    ,PTHKF(ILAYER,IFL)  ,UELR1    ,
     5          OFF      ,OFFL    ,DFMAX    ,TDEL     ,NFUNC1   ,
     6          IFUNC1   ,NPF     ,TF       ,EL_LEN   ,FOFF     ,IPG      )
c
              CASE (31)     !    Anisotropic fabric failure model
c
                IF (ISMSTR == 11) THEN
                  DEPS1 => DEPSXX(1:NEL)
                  DEPS2 => DEPSYY(1:NEL)
                  EPS1  => DEPSXX(1:NEL)
                  EPS2  => DEPSYY(1:NEL)
                ELSE
                  II = NEL*3
                  JJ = NEL + II
                  DEPS1 => DEPSXX(1:NEL)
                  DEPS2 => DEPSYY(1:NEL)
                  EPS1  => UVAR(II:II+NEL)
                  EPS2  => UVAR(JJ:JJ+NEL)
                ENDIF
                CALL FAIL_FABRIC_C(
     1               NEL      ,NGL       ,NUPAR     ,NVARF     ,NFUNC1    ,
     2               UPARAMF  ,UVARF     ,IFUNC1    ,TT        ,DT1       ,    
     3               NPF      ,TF        ,DEPS1     ,DEPS2     ,EPS1      ,
     4               EPS2     ,SIGNXX    ,SIGNYY    ,DFMAX     ,TDEL      ,
     5               IPG      ,ILAYER    ,IT        ,OFF       ,FOFF      )                     
c
              CASE (32)     !    HC_DSSE failure model
                CALL FAIL_HC_DSSE_C(
     1           NEL      ,NUPAR    ,NVARF    ,UPARAMF   ,UVARF     ,
     2           TT       ,NGL      ,IPT      ,ILAYER    ,IT        ,
     3           SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX     ,
     4           DPLA     ,OFF      ,FOFF     ,PTHKF(ILAYER,IFL)    ,
     5           DFMAX    ,TDEL     ,UELR1    ,MPT      ,IGTYP      )
c
              CASE (34)     !    Cockcroft-Latham failure model
                CALL FAIL_COCKROFT_C(
     1           JLT      ,NVARF    ,
     2           TT       ,UPARAMF  ,NGL      ,IPT      ,ILAYER    ,
     3           MPT      ,IT       ,IPG       ,
     4           SIGNXX   ,SIGNYY   ,SIGNXY   ,
     5           EPSXX    ,EPSYY    ,EPSXY    ,EPSYZ    ,EPSZX     ,
     6           DPLA     ,UVARF    ,UELR1    ,FOFF     ,
     7           OFF      ,DFMAX    ,TDEL   )
c
              CASE (36)     !    Visual failure model
                IF (ILAW == 2) THEN
                  DO I=JFT,JLT
                    ZT=ZZ(I)*THK0(I)
                    EPSXX(I)= GSTR(I,1)+ZT*GSTR(I,6)
                    EPSYY(I)= GSTR(I,2)+ZT*GSTR(I,7)
                    EPSXY(I)= GSTR(I,3)+ZT*GSTR(I,8)
                  ENDDO
                ENDIF
                CALL FAIL_VISUAL_C(                                           
     1             JLT      ,NVARF   ,TT       ,DT1     ,UPARAMF  ,NGL      ,
     2             SIGNXX   ,SIGNYY  ,SIGNXY   ,EPSXX   ,EPSYY    ,EPSXY    ,
     3             UVARF    ,OFF     ,DFMAX    ,ISMSTR  )
c
              CASE (37)     !    old (obsolete) tabulated failure model
                IF (IXFEM == 0) THEN
                  CALL FAIL_TAB_OLD_C(
     1                NEL       ,NUPAR     ,NVARF     ,UPARAMF   ,UVARF     ,
     2                NFUNC1    ,IFUNC1    ,NPF       ,TF        ,
     3                TT        ,NGL       ,IPG       ,ILAYER    ,IT        , 
     4                SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ    ,SIGNZX    ,      
     5                DPLA      ,EPSPL     ,THKN      ,EL_LEN    ,TSTAR     ,
     6                PTHKF(ILAYER,IFL)    ,OFF       ,FOFF      ,
     7                DFMAX     ,TDEL      ,IGTYP     )
                ELSE
                  CALL FAIL_TAB_OLD_XFEM(
     1                NEL      ,NUPAR    ,NVARF    ,NPF      ,TF       ,
     2                TT       ,DT1C     ,UPARAMF  ,NGL      ,IPT      , 
     3                MPT      ,IP       ,NFUNC1   ,IFUNC1   ,DMG_FLAG ,
     4                SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     5                DPLA     ,EPSPL    ,TSTAR    ,TENSX    ,UVARF    ,
     6                GBUF%NOFF,ALDT     ,OFF      ,OFFL     ,ELCRKINI ,
     7                IXFEM    ,IXEL     ,ILAYER   ,DFMAX    ,TDEL     )
                ENDIF
c
              CASE (38)     !    Orthotropic biquad
c
                CALL FAIL_ORTHBIQUAD_C(
     1            JLT      ,NVARF   ,
     2            TT       ,UPARAMF ,NGL      ,IPT      ,MPT      ,
     3            SIGNXX   ,SIGNYY  ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     4            DPLA     ,EPSPL   ,UVARF    ,PTHKF(ILAYER,IFL)  ,UELR1  ,
     5            OFF      ,OFFL    ,DFMAX    ,TDEL     ,NFUNC1   ,
     6            IFUNC1   ,NPF     ,TF       ,EL_LEN   ,FOFF     ,IPG    )
c
              CASE (39)     !    GENE1
c
                CALL FAIL_GENE1_C(
     1            JLT      ,NUPAR    ,NVARF    ,NFUNC1   ,IFUNC1   ,LBUF%OFF ,
     2            NPF      ,TF       ,TT       ,DT1C     ,UPARAMF  ,IPG      ,
     3            NGL      ,GBUF%DT  ,EPSPL    ,UVARF    ,OFF      ,NPG      ,     
     4            EPSXX    ,EPSYY    ,EPSXY    ,EPSYZ    ,EPSZX    ,AREA     ,            
     5            SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,THKN     , 
     6            TEMPEL   ,DFMAX    ,TDEL     ,ALDT     ,TABLE    ,IFL      ,
     7            THK0     ,ELBUF_STR,IPT      ,MPT      ,PTHKF(ILAYER,IFL),FOFF,
     8            IGTYP    ,THKLYL   ,ILAY     )
c
              CASE (40)     !    RTCL
c
                CALL FAIL_RTCL_C(
     1            JLT      ,NUPAR    ,NVARF    ,TT       ,DT1C     ,UPARAMF  ,
     2            SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,MPT      ,
     3            NGL      ,DPLA     ,UVARF    ,OFF      ,DFMAX    ,TDEL     ,
     4            AREA     ,FOFF     ,IGTYP    ,OFFL     ,IPT      ,THK0     )
c
              CASE (41)     !    TAB2
c
                CALL FAIL_TAB2_C(
     1            JLT      ,NUPAR    ,NVARF    ,NFUNC1   ,IFUNC1   ,
     2            NPF      ,TABLE    ,TF       ,TT       ,UPARAMF  , 
     3            NGL      ,EL_LEN   ,DPLA     ,EPSPL    ,UVARF    ,     
     4            SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,              
     5            EL_TEMP  ,FOFF     ,DFMAX    ,TDEL     ,IPT      ,
     6            IPG      ,DMG_FLAG ,DMG_LOC_SCALE,PTHKF(ILAYER,IFL)) 
c
              CASE (42)     !    INIEVO
c
                CALL FAIL_INIEVO_C(
     1            JLT      ,NUPAR    ,NVARF    ,NFUNC1   ,IFUNC1   ,
     2            NPF      ,TABLE    ,TF       ,TT       ,UPARAMF  , 
     3            NGL      ,EL_LEN   ,DPLA     ,EPSPL    ,UVARF    ,     
     4            SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,              
     5            EL_PLA   ,EL_TEMP  ,SIGY     ,FOFF     ,DFMAX    ,
     6            TDEL     ,IPT      ,IPG      ,DMG_FLAG ,DMG_LOC_SCALE,
     7            PTHKF(ILAYER,IFL))
c
              CASE (99)     !    Syazwan failure model
                DO I=JFT,JLT
                  COPY_PLA(I) = LBUF%PLA(I)
                ENDDO
                CALL FAIL_SYAZWAN_C(
     1          JLT      ,NVARF   ,
     2          TT       ,UPARAMF ,NGL      ,IPT      ,MPT      ,
     3          SIGNXX   ,SIGNYY  ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     4          EPSXX    ,EPSYY   ,EPSXY    ,EPSYZ    ,EPSZX    ,
     5          DPLA     ,EPSPL   ,UVARF    ,PTHKF(ILAYER,IFL)  ,UELR1    ,
     6          OFF      ,OFFL    ,DFMAX    ,TDEL     ,NFUNC1   ,
     7          IFUNC1   ,EL_LEN  ,FOFF     ,IPG      ,COPY_PLA ,
     8          DMG_FLAG ,DMG_LOC_SCALE,NPF ,TF)
c  
c-------------
              END SELECT
c-------------
              IF (ORTH_DAMAGE == 0) THEN
                DO I=1,NEL
                  IF (FOFF(I) == 0)  THEN                        
                    OFFL(I) = ZERO                       
c                    SIGOFF(I) = OFFL(I)                          
                    SIGOFF(I) = ZERO                       
                  ENDIF                                            
                ENDDO
              ELSE
                DO I=1,NEL
                  IF (OFF(I) == ZERO)  THEN                        
                    SIGOFF(I) = ZERO                       
                  ENDIF                                            
                ENDDO
              ENDIF                                              
c-------------
            ENDDO     !  NFAIL
            IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STOPTIME(121,1)
          ENDIF  ! IF (IFAILURE == 1)
c-----------------------------------
c         END of FAILURE MODELS
c-----------------------------------
#include "vectorize.inc"
          DO I=JFT,JLT                                               
            LBUF%SIG(IJ(1)+I) = SIGNXX(I) * SIGOFF(I)      
            LBUF%SIG(IJ(2)+I) = SIGNYY(I) * SIGOFF(I)           
            LBUF%SIG(IJ(3)+I) = SIGNXY(I) * SIGOFF(I)           
            LBUF%SIG(IJ(4)+I) = SIGNYZ(I) * SIGOFF(I)           
            LBUF%SIG(IJ(5)+I) = SIGNZX(I) * SIGOFF(I)           
          ENDDO                                                      
C
c------------------------------------------------
          IF (IGTYP == 1) THEN                                            
            SELECT CASE (DMG_FLAG)                                               
            CASE (0)     
              IF (ILAW==58 .OR. IPRONY == 1) THEN
                FOR(1:NEL,1) = FOR(1:NEL,1) + THLY(1:NEL)*(SIGNXX(1:NEL)+SIGVXX(1:NEL))            
                FOR(1:NEL,2) = FOR(1:NEL,2) + THLY(1:NEL)*(SIGNYY(1:NEL)+SIGVYY(1:NEL))            
                FOR(1:NEL,3) = FOR(1:NEL,3) + THLY(1:NEL)*(SIGNXY(1:NEL)+SIGVXY(1:NEL))            
                FOR(1:NEL,4) = FOR(1:NEL,4) + THLY(1:NEL)*(SIGNYZ(1:NEL)+SIGVYZ(1:NEL))            
                FOR(1:NEL,5) = FOR(1:NEL,5) + THLY(1:NEL)*(SIGNZX(1:NEL)+SIGVZX(1:NEL))            
                MOM(1:NEL,1) = MOM(1:NEL,1) + WMC(1:NEL)*(SIGNXX(1:NEL)+SIGVXX(1:NEL))             
                MOM(1:NEL,2) = MOM(1:NEL,2) + WMC(1:NEL)*(SIGNYY(1:NEL)+SIGVYY(1:NEL))             
                MOM(1:NEL,3) = MOM(1:NEL,3) + WMC(1:NEL)*(SIGNXY(1:NEL)+SIGVXY(1:NEL))                
              ELSE
                FOR(1:NEL,1) = FOR(1:NEL,1) + THLY(1:NEL)*SIGNXX(1:NEL)            
                FOR(1:NEL,2) = FOR(1:NEL,2) + THLY(1:NEL)*SIGNYY(1:NEL)            
                FOR(1:NEL,3) = FOR(1:NEL,3) + THLY(1:NEL)*SIGNXY(1:NEL)            
                FOR(1:NEL,4) = FOR(1:NEL,4) + THLY(1:NEL)*SIGNYZ(1:NEL)            
                FOR(1:NEL,5) = FOR(1:NEL,5) + THLY(1:NEL)*SIGNZX(1:NEL)            
                MOM(1:NEL,1) = MOM(1:NEL,1) + WMC(1:NEL) *SIGNXX(1:NEL)            
                MOM(1:NEL,2) = MOM(1:NEL,2) + WMC(1:NEL) *SIGNYY(1:NEL)            
                MOM(1:NEL,3) = MOM(1:NEL,3) + WMC(1:NEL) *SIGNXY(1:NEL)            
              ENDIF                                                               
            CASE (1)  ! softening with DMG_LOC_SCALE from failure model          
              IF (ILAW==58 .OR. IPRONY == 1) THEN
                FOR(1:NEL,1) = FOR(1:NEL,1) + THLY(1:NEL)*(SIGNXX(1:NEL)+SIGVXX(1:NEL))*DMG_LOC_SCALE(1:NEL)   
                FOR(1:NEL,2) = FOR(1:NEL,2) + THLY(1:NEL)*(SIGNYY(1:NEL)+SIGVYY(1:NEL))*DMG_LOC_SCALE(1:NEL)   
                FOR(1:NEL,3) = FOR(1:NEL,3) + THLY(1:NEL)*(SIGNXY(1:NEL)+SIGVXY(1:NEL))*DMG_LOC_SCALE(1:NEL)   
                FOR(1:NEL,4) = FOR(1:NEL,4) + THLY(1:NEL)*(SIGNYZ(1:NEL)+SIGVYZ(1:NEL))*DMG_LOC_SCALE(1:NEL)   
                FOR(1:NEL,5) = FOR(1:NEL,5) + THLY(1:NEL)*(SIGNZX(1:NEL)+SIGVZX(1:NEL))*DMG_LOC_SCALE(1:NEL)   
                MOM(1:NEL,1) = MOM(1:NEL,1) + WMC(1:NEL) *(SIGNXX(1:NEL)+SIGVXX(1:NEL))*DMG_LOC_SCALE(1:NEL)   
                MOM(1:NEL,2) = MOM(1:NEL,2) + WMC(1:NEL) *(SIGNYY(1:NEL)+SIGVYY(1:NEL))*DMG_LOC_SCALE(1:NEL)   
                MOM(1:NEL,3) = MOM(1:NEL,3) + WMC(1:NEL) *(SIGNXY(1:NEL)+SIGVXY(1:NEL))*DMG_LOC_SCALE(1:NEL)   
              ELSE                                                    
                FOR(1:NEL,1) = FOR(1:NEL,1) + THLY(1:NEL)*SIGNXX(1:NEL)*DMG_LOC_SCALE(1:NEL)   
                FOR(1:NEL,2) = FOR(1:NEL,2) + THLY(1:NEL)*SIGNYY(1:NEL)*DMG_LOC_SCALE(1:NEL)   
                FOR(1:NEL,3) = FOR(1:NEL,3) + THLY(1:NEL)*SIGNXY(1:NEL)*DMG_LOC_SCALE(1:NEL)   
                FOR(1:NEL,4) = FOR(1:NEL,4) + THLY(1:NEL)*SIGNYZ(1:NEL)*DMG_LOC_SCALE(1:NEL)   
                FOR(1:NEL,5) = FOR(1:NEL,5) + THLY(1:NEL)*SIGNZX(1:NEL)*DMG_LOC_SCALE(1:NEL)   
                MOM(1:NEL,1) = MOM(1:NEL,1) + WMC(1:NEL) *SIGNXX(1:NEL)*DMG_LOC_SCALE(1:NEL)   
                MOM(1:NEL,2) = MOM(1:NEL,2) + WMC(1:NEL) *SIGNYY(1:NEL)*DMG_LOC_SCALE(1:NEL)   
                MOM(1:NEL,3) = MOM(1:NEL,3) + WMC(1:NEL) *SIGNXY(1:NEL)*DMG_LOC_SCALE(1:NEL)  
              ENDIF
            CASE (2)  ! internal dammage model within law15         
              FOR(1:NEL,1) = FOR(1:NEL,1) + THLY(1:NEL)*SIGDMG(1:NEL,1)
              FOR(1:NEL,2) = FOR(1:NEL,2) + THLY(1:NEL)*SIGDMG(1:NEL,2)   
              FOR(1:NEL,3) = FOR(1:NEL,3) + THLY(1:NEL)*SIGDMG(1:NEL,3)  
              FOR(1:NEL,4) = FOR(1:NEL,4) + THLY(1:NEL)*SIGDMG(1:NEL,4)  
              FOR(1:NEL,5) = FOR(1:NEL,5) + THLY(1:NEL)*SIGDMG(1:NEL,5)  
              MOM(1:NEL,1) = MOM(1:NEL,1) + WMC(1:NEL) *SIGDMG(1:NEL,1) 
              MOM(1:NEL,2) = MOM(1:NEL,2) + WMC(1:NEL) *SIGDMG(1:NEL,2) 
              MOM(1:NEL,3) = MOM(1:NEL,3) + WMC(1:NEL) *SIGDMG(1:NEL,3) 
            END SELECT                                                             
c
          ELSE   ! IGTYP /= 1)                                              
c
            SELECT CASE (DMG_FLAG)                                            
            CASE (0)                                                           
              IF (ILAW == 58 .OR. IPRONY == 1) THEN
                        TENS(JFT:JLT,1) = SIGNXX(JFT:JLT)+SIGVXX(JFT:JLT)
                        TENS(JFT:JLT,2) = SIGNYY(JFT:JLT)+SIGVYY(JFT:JLT)
                        TENS(JFT:JLT,3) = SIGNXY(JFT:JLT)+SIGVXY(JFT:JLT)
                        TENS(JFT:JLT,4) = SIGNYZ(JFT:JLT)+SIGVYZ(JFT:JLT)
                        TENS(JFT:JLT,5) = SIGNZX(JFT:JLT)+SIGVZX(JFT:JLT)
              ELSE
                        TENS(JFT:JLT,1) = SIGNXX(JFT:JLT)                     
                        TENS(JFT:JLT,2) = SIGNYY(JFT:JLT)
                        TENS(JFT:JLT,3) = SIGNXY(JFT:JLT)
                        TENS(JFT:JLT,4) = SIGNYZ(JFT:JLT)
                        TENS(JFT:JLT,5) = SIGNZX(JFT:JLT)
              ENDIF                                                                
            CASE (1)                                                           
              IF (ILAW == 58 .OR. IPRONY == 1) THEN
                        TENS(JFT:JLT,1) = (SIGNXX(JFT:JLT)+SIGVXX(JFT:JLT))*DMG_LOC_SCALE(JFT:JLT)                               
                        TENS(JFT:JLT,2) = (SIGNYY(JFT:JLT)+SIGVYY(JFT:JLT))*DMG_LOC_SCALE(JFT:JLT)                               
                        TENS(JFT:JLT,3) = (SIGNXY(JFT:JLT)+SIGVXY(JFT:JLT))*DMG_LOC_SCALE(JFT:JLT)                               
                        TENS(JFT:JLT,4) = (SIGNYZ(JFT:JLT)+SIGVYZ(JFT:JLT))*DMG_LOC_SCALE(JFT:JLT)                               
                        TENS(JFT:JLT,5) = (SIGNZX(JFT:JLT)+SIGVZX(JFT:JLT))*DMG_LOC_SCALE(JFT:JLT)                               
              ELSE
                        TENS(JFT:JLT,1) = SIGNXX(JFT:JLT)*DMG_LOC_SCALE(JFT:JLT)                               
                        TENS(JFT:JLT,2) = SIGNYY(JFT:JLT)*DMG_LOC_SCALE(JFT:JLT)                               
                        TENS(JFT:JLT,3) = SIGNXY(JFT:JLT)*DMG_LOC_SCALE(JFT:JLT)                               
                        TENS(JFT:JLT,4) = SIGNYZ(JFT:JLT)*DMG_LOC_SCALE(JFT:JLT)                               
                        TENS(JFT:JLT,5) = SIGNZX(JFT:JLT)*DMG_LOC_SCALE(JFT:JLT)                 
              ENDIF       
            CASE (2)  ! internal dammage model within law15         
              TENS(JFT:JLT,1) = SIGDMG(JFT:JLT,1)
              TENS(JFT:JLT,2) = SIGDMG(JFT:JLT,2)   
              TENS(JFT:JLT,3) = SIGDMG(JFT:JLT,3)  
              TENS(JFT:JLT,4) = SIGDMG(JFT:JLT,4)  
              TENS(JFT:JLT,5) = SIGDMG(JFT:JLT,5)  
            END SELECT
c
            IF (ILAW == 58) THEN                                                  
              DO I=JFT,JLT                                                         
                II = JDIR + I-1
                R1 = DIR_A(II)
                S1 = DIR_A(II+NEL)
                R2 = DIR_B(II)
                S2 = DIR_B(II+NEL)
                RS1= R1*S1                                                         
                RS2= R2*S2                                                         
                R12A = R1*R1                                                       
                R22A = R2*R2                                                       
                S12B = S1*S1                                                       
                S22B = S2*S2                                                       
                RS3 = S1*S2-R1*R2                                                  
                R3R3= ONE+S1*R2+R1*S2                                               
                R3R3= HALF*R3R3                                                  
                S3S3= ONE-S1*R2-R1*S2                                               
                S3S3= HALF*S3S3                                                  
                T1 = TENS(I,1)                                                     
                T2 = TENS(I,2)                                                     
                T3 = TENS(I,3)                                                     
                TENS(I,1) = R12A*T1 + R22A*T2 - RS3*T3                             
                TENS(I,2) = S12B*T1 + S22B*T2 + RS3*T3                             
                TENS(I,3) = RS1*T1  + RS2*T2 + (R3R3 - S3S3)*T3                    
                TENS(I,4) = SIGNYZ(I)                                              
                TENS(I,5) = SIGNZX(I)                                              
              ENDDO                                                                
c
            ELSE     ! IGTYP = 9,10,11,51,52                                                        
              IF (ILAW /= 1 .and. ILAW /= 2 .and. ILAW /= 27 .and. ILAW /= 32)
     .          CALL UROTOV(JFT,JLT,TENS,DIR_A(JDIR),NEL)
            ENDIF                                                                  
C----------------------
C           FORCES AND MOMENTS WHEN IGTYP /= 1                                            
C----------------------
#include "vectorize.inc"
            DO I=JFT,JLT                                                       
              FOR(I,1) = FOR(I,1) + THLY(I)*TENS(I,1)                          
              FOR(I,2) = FOR(I,2) + THLY(I)*TENS(I,2)                          
              FOR(I,3) = FOR(I,3) + THLY(I)*TENS(I,3)                          
              FOR(I,4) = FOR(I,4) + THLY(I)*TENS(I,4)                          
              FOR(I,5) = FOR(I,5) + THLY(I)*TENS(I,5)                          
              MOM(I,1) = MOM(I,1) + WMC(I) *TENS(I,1)                          
              MOM(I,2) = MOM(I,2) + WMC(I) *TENS(I,2)                          
              MOM(I,3) = MOM(I,3) + WMC(I) *TENS(I,3)                          
            ENDDO                                                              
c
          ENDIF   ! IGTYP                                                          
C-----------------------------------------------
C         FACTEURS POUR COQUES B.L. (Zeng&Combescure)
C-----------------------------------------------
          IF (ILAW /= 2  .AND. ILAW /= 15 .AND. ILAW /= 22 .AND.
     .        ILAW /= 25 .AND. ILAW /= 27 .AND. ILAW /= 32) THEN  ! for law25 it is done inside sigeps25c.F
            IF(FLAG_ZCFAC) THEN
                ZCFAC(JFT:JLT,1) = ZCFAC(JFT:JLT,1) + ETSE(JFT:JLT) * THLY(JFT:JLT)
                ZCFAC(JFT:JLT,2) = MIN(ETSE(JFT:JLT),ZCFAC(JFT:JLT,2))  
            ENDIF
            YLD(JFT:JLT) = YLD(JFT:JLT) + SIGY(JFT:JLT)*THLY(JFT:JLT)
            IF(FLAG_ETIMP) ETIMP(JFT:JLT) = ETIMP(JFT:JLT) + ET_IMP(JFT:JLT)*THLY(JFT:JLT)
          ELSEIF ( ILAW == 2 ) THEN
c------------------------------------            
            SELECT CASE (IGTYP)
c------------------------------------            
              CASE (1,9) 
                ZCFAC(JFT:JLT,1) = ZCFAC(JFT:JLT,1) + ETSE(JFT:JLT) / NPT
                ZCFAC(JFT:JLT,2) = MIN(ETSE(JFT:JLT),ZCFAC(JFT:JLT,2))
                YLD(JFT:JLT)     = YLD(JFT:JLT) + SIGY(JFT:JLT) / NPT
              CASE DEFAULT
                ZCFAC(JFT:JLT,1) = ZCFAC(JFT:JLT,1) + ETSE(JFT:JLT) * THLY(JFT:JLT)
                ZCFAC(JFT:JLT,2) = MIN(ETSE(JFT:JLT),ZCFAC(JFT:JLT,2))
                YLD(JFT:JLT)     = YLD(JFT:JLT) + SIGY(JFT:JLT) * THLY(JFT:JLT)
            END SELECT
          ENDIF
c-----------------------------------------------
          IF (IMPL_S > 0) THEN
            CALL PUTSIGNORC3(JFT ,JLT ,IUN,NG,IPT,G_IMP ,SIGKSI)
          END IF
c-----------------------------------------------
          IF (IXEL == 0 .and. PROGRESSIVE_CRACK == 0) THEN   ! original element
            IF (ILAW == 27) THEN
              DIR_ORTH => LBUF%DMG
              IROT = ELBUF_STR%BUFLY(ILAYER)%L_DMG
            ELSE
              DIR_ORTH => BUFLY%DIRA
              IROT = ELBUF_STR%BUFLY(ILAYER)%LY_DIRA
            ENDIF
            IF (IXFEM == 1) THEN        ! multilayer
              CALL XFEM_CRK_DIR(
     .             JLT        ,ILAYER ,IXFEM    ,ELCRKINI,
     .             DIR_ORTH,TENSX  ,DIR1_CRK ,DIR2_CRK ,IROT    )
            ELSEIF (IXFEM == 2) THEN    ! monolayer
              IPTX = 1
              CALL XFEM_CRK_DIR(
     .             JLT    ,IPTX    ,IXFEM   ,ELCRKINI,
     .             DIR_ORTH  ,TENSX  ,DIR1_CRK,DIR2_CRK,IROT    )
            ENDIF
          ENDIF
C------------------------------------------------------------
C     Variable to regularize with Non-local
C-----------------------     
          IF (BUFLY%L_PLA > 0) THEN
            ! Non-local material
            IF (INLOC > 0) THEN
              DO I=JFT,JLT
                VARNL(I,IT) = LBUF%PLA(I)
              ENDDO
            ENDIF
          ENDIF
C-------------------------------------
        ENDDO  !  IT=1,NPTT
        IPT_ALL = IPT_ALL + NPTT
      ENDDO  !  DO ILAY =1,NLAY
c--------------------------------------------------
c     FIN DE BOUCLE SUR POINTS INTEGRATION (LAYERS)
c--------------------------------------------------
c     TEST OF ELEMENT FAILURE 
c--------------------------------------------------
      IF (IFAILURE == 1 .and. IXFEM == 0) THEN
        IF (ORTH_DAMAGE == 1) THEN   ! /fail/alter + front_wave only
          TFAIL => GBUF%DMG(1+NEL:NEL*2)  ! only for /FAIL/ALTER          
          CALL FAIL_SETOFF_WIND_FRWAVE(
     .                       ELBUF_STR,GEO      ,PID(1)   ,NGL      ,
     .                       NEL      ,IR       ,IS       ,NLAY     ,
     .                       NPTTOT   ,PTHKF    ,THK_LY   ,THKLY    ,
     .                       OFF      ,NPG      ,STACK    ,ISUBSTACK, 
     .                       IGTYP    ,FAILWAVE ,FWAVE_EL ,DMG_FLAG ,
     .                       TT       ,TRELAX   ,TFAIL    ,DMG_GLOB_SCALE)
        ELSEIF (NPG == 1) THEN
          CALL FAIL_SETOFF_C(ELBUF_STR,GEO      ,PID(1)   ,NGL      ,
     .                       NEL      ,NLAY     ,NPTTOT   ,PTHKF    ,
     .                       THK_LY   ,THKLY    ,OFF      ,STACK    ,
     .                       ISUBSTACK,IGTYP    ,FAILWAVE ,FWAVE_EL )
        ELSE
          CALL FAIL_SETOFF_NPG_C(
     .                       ELBUF_STR,GEO      ,PID(1)   ,NGL      ,
     .                       NEL      ,IR       ,IS       ,NLAY     ,
     .                       NPTTOT   ,PTHKF    ,THK_LY   ,THKLY    ,
     .                       OFF      ,NPG      ,STACK    ,ISUBSTACK, 
     .                       IGTYP    ,FAILWAVE ,FWAVE_EL )
        ENDIF
      ENDIF  
c---
      NINDX = 0
      IF (IXFEM == 0) THEN
        DO I=JFT,JLT
          IF (OFF(I) == FOUR_OVER_5 .and. IOFF_DUCT(I) == 0 .or.   ! rupture /fail
     .        OFF(I) > ZERO   .and. OFF_OLD(I) < EM01 .or.   ! rupture progressive law 2,22,25
     .        OFF(I) > ZERO   .and. OFF(I) < ONE .and. DMG_FLAG == 1 .and. 
     .        DMG_GLOB_SCALE(I) < EM02) THEN  
            OFF(I) = ZERO
            NINDX  = NINDX + 1
            INDX(NINDX) = I
          ENDIF
        ENDDO
      ENDIF
c------------------------------------------------------------------
C     VISCOSITE DE MEMBRANE
C---------------------------                                              
      IF (IGTYP == 11 .AND. IGMAT > 0) THEN
        IPGMAT = 700
        DO I=JFT,JLT
          RHO(I) = GEO(IPGMAT+1,PID(1))
          SSP(I) = GEO(IPGMAT+9,PID(1))
        ENDDO
      ELSEIF (IGTYP == 52 .OR.
     .      ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0)) THEN
        DO I=JFT,JLT
          SSP(I) = PM_STACK(9,ISUBSTACK)
          RHO(I) = PM_STACK(1,ISUBSTACK) 
        ENDDO
      ENDIF   
!---
! Special treatment for law 19
!---
!  --- start treatment law 19---
C-----------------------------------------------------------
C     DESACTIVATION SI SURFACE < SURFACE MIN
C-----------------------------------------------------------
      IF (MTN == 19) THEN
        MX = MAT(JFT)
        DO I=JFT,JLT
          AREAMIN(I)  = PM(53,MX)
          DAREAMIN(I) = PM(54,MX)
        ENDDO
        IF (ISMSTR == 3) THEN
          DO I=JFT,JLT
            IF (AREAMIN(I) > ZERO) THEN
              AA = (ONE+GSTR(I,1)+GSTR(I,2) - AREAMIN(I)) * DAREAMIN(I)
              AA = MIN(MAX(AA,ZERO),ONE)
              FOR(I,1)=FOR(I,1)*AA
              FOR(I,2)=FOR(I,2)*AA
              FOR(I,3)=FOR(I,3)*AA
            ENDIF
          ENDDO
        ELSE
          DO I=JFT,JLT
           IF(AREAMIN(I) > ZERO)THEN
             VV = GSTR(I,1)+GSTR(I,2)
             VV = ONE + (ONE + (HALF + ONE_OVER_6*VV)*VV)*VV
             AA = (VV - AREAMIN(I)) * DAREAMIN(I)
             AA = MIN(MAX(AA,ZERO),ONE)
             FOR(I,1)=FOR(I,1)*AA
             FOR(I,2)=FOR(I,2)*AA
             FOR(I,3)=FOR(I,3)*AA
           ENDIF
          ENDDO
        ENDIF ! IF (ISMSTR == 3)
      ENDIF  ! IF (MTN == 19)
!  --- end treatment law 19---
c-----------------------------------------------------------------------
      THK(JFT:JLT) = MAX(THKN(JFT:JLT),EM30)
c
      FACT = ONEP414*DM
      VISC(JFT:JLT) = FACT*SSP(JFT:JLT)*SQRT(AREA(JFT:JLT))*DT_INV(JFT:JLT)*RHO(JFT:JLT)
c
      FOR(JFT:JLT,1)=FOR(JFT:JLT,1)+VISC(JFT:JLT)*(EXX(JFT:JLT)+HALF*EYY(JFT:JLT))
      FOR(JFT:JLT,2)=FOR(JFT:JLT,2)+VISC(JFT:JLT)*(EYY(JFT:JLT)+HALF*EXX(JFT:JLT))
      FOR(JFT:JLT,3)=FOR(JFT:JLT,3)+VISC(JFT:JLT)* EXY(JFT:JLT) *THIRD   
c
      FOR(JFT:JLT,1)=FOR(JFT:JLT,1)*OFF(JFT:JLT)*DMG_GLOB_SCALE(JFT:JLT)
      FOR(JFT:JLT,2)=FOR(JFT:JLT,2)*OFF(JFT:JLT)*DMG_GLOB_SCALE(JFT:JLT)
      FOR(JFT:JLT,3)=FOR(JFT:JLT,3)*OFF(JFT:JLT)*DMG_GLOB_SCALE(JFT:JLT)
      FOR(JFT:JLT,4)=FOR(JFT:JLT,4)*OFF(JFT:JLT)*DMG_GLOB_SCALE(JFT:JLT)
      FOR(JFT:JLT,5)=FOR(JFT:JLT,5)*OFF(JFT:JLT)*DMG_GLOB_SCALE(JFT:JLT)
      MOM(JFT:JLT,1)=MOM(JFT:JLT,1)*OFF(JFT:JLT)*DMG_GLOB_SCALE(JFT:JLT)
      MOM(JFT:JLT,2)=MOM(JFT:JLT,2)*OFF(JFT:JLT)*DMG_GLOB_SCALE(JFT:JLT)
      MOM(JFT:JLT,3)=MOM(JFT:JLT,3)*OFF(JFT:JLT)*DMG_GLOB_SCALE(JFT:JLT)
C
C!  --- specific treatment for law 119 - scale factor applied on internal forces---
      IF (ILAW == 119) THEN
        FOR(JFT:JLT,1)=FOR(JFT:JLT,1)*GBUF%INTVAR(JFT:JLT)
        FOR(JFT:JLT,2)=FOR(JFT:JLT,2)*GBUF%INTVAR(JFT:JLT)
        FOR(JFT:JLT,3)=FOR(JFT:JLT,3)*GBUF%INTVAR(JFT:JLT)
        FOR(JFT:JLT,4)=FOR(JFT:JLT,4)*GBUF%INTVAR(JFT:JLT)
        FOR(JFT:JLT,5)=FOR(JFT:JLT,5)*GBUF%INTVAR(JFT:JLT)
        MOM(JFT:JLT,1)=MOM(JFT:JLT,1)*GBUF%INTVAR(JFT:JLT)
        MOM(JFT:JLT,2)=MOM(JFT:JLT,2)*GBUF%INTVAR(JFT:JLT)
        MOM(JFT:JLT,3)=MOM(JFT:JLT,3)*GBUF%INTVAR(JFT:JLT)
      ENDIF
C
      DEGMB(JFT:JLT) = DEGMB(JFT:JLT)+ FOR(JFT:JLT,1)*EXX(JFT:JLT)+FOR(JFT:JLT,2)*EYY(JFT:JLT)
     .                   + FOR(JFT:JLT,3)*EXY(JFT:JLT)+FOR(JFT:JLT,4)*EYZ(JFT:JLT)
     .                   + FOR(JFT:JLT,5)*EXZ(JFT:JLT)
      DEGFX(JFT:JLT) = DEGFX(JFT:JLT)+ MOM(JFT:JLT,1)*KXX(JFT:JLT)+MOM(JFT:JLT,2)*KYY(JFT:JLT)
     .                   + MOM(JFT:JLT,3)*KXY(JFT:JLT)
c-----------------------------------------------------------------------
!---
      IF (MTN == 25) THEN
C----------------------------
C  SHEAR DELAMINATION
C----------------------------
C is an obsolete option (just for the past)
        IF (IGMAT == 0) 
     .    CALL M25DELAM(JFT,JLT,PM,GSTR,GBUF%DAMDL,MAT,NGL,NEL)
!
        IF (ISHPLYXFEM > 0) THEN
          MX = MAT(JFT)
          IPT_ALL = 0
          DO ILAY=1,NLAY
            ILAYER = ILAY
C  cas xfem multilayer
            IF (IXFEM == 1 .AND. IXLAY > 0) ILAYER = IXLAY
            NPTT = ELBUF_STR%BUFLY(ILAYER)%NPTT
            DO IT=1,NPTT
              IPT = IPT_ALL + IT        ! count all NPTT through all layers
              DO I=JFT,JLT
!
                PLY_F(I,1,IPT) = PLY_F(I,1,IPT) + VISC(I)*(PLY_EXX(I,IPT)+HALF*PLY_EYY(I,IPT))
                PLY_F(I,2,IPT) = PLY_F(I,2,IPT) + VISC(I)*(PLY_EYY(I,IPT)+HALF*PLY_EXX(I,IPT))
                PLY_F(I,3,IPT) = PLY_F(I,3,IPT) + VISC(I)* PLY_EXY(I,IPT)*THIRD
              ENDDO
!
              DO I=JFT,JLT
                PLY_F(I,1,IPT) = PLY_F(I,1,IPT)*OFF(I)
                PLY_F(I,2,IPT) = PLY_F(I,2,IPT)*OFF(I)
                PLY_F(I,3,IPT) = PLY_F(I,3,IPT)*OFF(I)
                PLY_F(I,4,IPT) = PLY_F(I,4,IPT)*OFF(I)
                PLY_F(I,5,IPT) = PLY_F(I,5,IPT)*OFF(I)
              ENDDO     
            ENDDO ! DO IT=1,NPTT
            IPT_ALL = IPT_ALL + NPTT
          ENDDO ! DO ILAY=1,NLAY
        ENDIF ! IF (ISHPLYXFEM /= 0)
      ENDIF ! IF (MTN == 25)
!---
      DO I=JFT,JLT
        VOL2 = HALF*VOL0(I)
        IF (MTN == 22) VOL2 = VOL2*OFF(I)
        EINT(I,1) = EINT(I,1) + DEGMB(I)*VOL2
        EINT(I,2) = EINT(I,2) + DEGFX(I)*THK0(I)*VOL2
      ENDDO
!
      IF (JTHE > 0 .AND. MTN /= 2) DIE(JFT:JLT) = DIE(JFT:JLT) + 
     .             COEF(JFT:JLT)*( DEGMB(JFT:JLT)*HALF*VOL0(JFT:JLT) + 
     .                             DEGFX(JFT:JLT)*THK0(JFT:JLT)*HALF*VOL0(JFT:JLT) )
c---------------------------------------------------
c      check element failure with xfem
       IF (IXFEM > 0) THEN
        DO I=JFT,JLT
          IF (OFF(I) == FOUR_OVER_5) THEN
            OFF(I) = ZERO
            NINDX  = NINDX + 1
            INDX(NINDX) = I
          ENDIF
        ENDDO
      ENDIF

c---  Shell rupture print out
c
      IOFC = NINDX  !  INDX used before in IELOF - check if still useful
      IF (NINDX > 0 .and. IXEL == 0) THEN
        IDEL7NOK = 1
        IF (IMCONV == 1) THEN
          DO II = 1,NINDX
            I = INDX(II)
#include    "lockon.inc"
            WRITE(IOUT, 1000) NGL(I)
            WRITE(ISTDO,1100) NGL(I),TT
#include    "lockoff.inc"
          ENDDO
        ENDIF
      ENDIF
c---------------------------------------------------
 1000 FORMAT(1X,'-- RUPTURE OF SHELL ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'-- RUPTURE OF SHELL ELEMENT :',I10,' AT TIME :',G11.4)
c---------------------------------------------------
      RETURN
      END

Chd|====================================================================
Chd|  ROTOS4                        source/materials/mat_share/mulawc.F
Chd|-- called by -----------
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ROTOS4(JFT,JLT,SIGXX,SIGYY,SIGXY,SIGYX,DIR,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,NEL
C     REAL
      my_real
     .   SIGXX(*),SIGYY(*),SIGXY(*),SIGYX(*), DIR(NEL,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   S1, S2, S3, S4,S5
C-----------------------------------------------
      DO I=JFT,JLT
         S1 =    DIR(I,1)*DIR(I,1)*SIGXX(I)
     .          +DIR(I,2)*DIR(I,2)*SIGYY(I)
     .          +DIR(I,1)*DIR(I,2)*(SIGXY(I)+SIGYX(I))
         S2 =    DIR(I,2)*DIR(I,2)*SIGXX(I)
     .          +DIR(I,1)*DIR(I,1)*SIGYY(I)
     .          -DIR(I,2)*DIR(I,1)*(SIGXY(I)+SIGYX(I))
         S5 =   -DIR(I,1)*DIR(I,2)*SIGXX(I)+DIR(I,2)*DIR(I,1)*SIGYY(I)
         S3 = S5+DIR(I,1)*DIR(I,1)*SIGXY(I)-DIR(I,2)*DIR(I,2)*SIGYX(I)
         S4 = S5+DIR(I,1)*DIR(I,1)*SIGYX(I)-DIR(I,2)*DIR(I,2)*SIGXY(I)
         SIGXX(I)=S1
         SIGYY(I)=S2
         SIGXY(I)=S3
         SIGYX(I)=S4
      ENDDO
c-----------
      RETURN
      END


