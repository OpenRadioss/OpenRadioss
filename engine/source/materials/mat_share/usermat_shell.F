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
Chd|  USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- called by -----------
Chd|        CMAIN3                        source/materials/mat_share/cmain3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ENG_USERLIB_FLAWC             source/user_interface/dyn_userlib.c
Chd|        ENG_USERLIB_GET_LAWC_VAR      source/user_interface/dyn_userlib.c
Chd|        ENG_USERLIB_GET_LAWC_VAR_2    source/user_interface/dyn_userlib.c
Chd|        ENG_USERLIB_SET_LAWC          source/user_interface/dyn_userlib.c
Chd|        ENG_USERLIB_SET_LAWC_VAR_2    source/user_interface/dyn_userlib.c
Chd|        ENG_USERLIB_SIGEPS99C         source/user_interface/dyn_userlib.c
Chd|        ENG_USERLIB_SIGEPSC           source/user_interface/dyn_userlib.c
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
Chd|        FAIL_HOFFMAN_C                source/materials/fail/hoffman/fail_hoffman_c.F
Chd|        FAIL_INIEVO_C                 source/materials/fail/inievo/fail_inievo_c.F
Chd|        FAIL_JOHNSON_C                source/materials/fail/johnson_cook/fail_johnson_c.F
Chd|        FAIL_JOHNSON_XFEM             source/materials/fail/johnson_cook/fail_johnson_xfem.F
Chd|        FAIL_MAXSTRAIN_C              source/materials/fail/max_strain/fail_maxstrain_c.F
Chd|        FAIL_NXT_C                    source/materials/fail/nxt/fail_nxt_c.F
Chd|        FAIL_ORTHBIQUAD_C             source/materials/fail/orthbiquad/fail_orthbiquad_c.F
Chd|        FAIL_ORTHENERG_C              source/materials/fail/orthenerg/fail_orthenerg_c.F
Chd|        FAIL_ORTHSTRAIN_C             source/materials/fail/orthstrain/fail_orthstrain_c.F
Chd|        FAIL_PUCK_C                   source/materials/fail/puck/fail_puck_c.F
Chd|        FAIL_RTCL_C                   source/materials/fail/rtcl/fail_rtcl_c.F
Chd|        FAIL_SETOFF_C                 source/materials/fail/fail_setoff_c.F
Chd|        FAIL_SETOFF_NPG_C             source/materials/fail/fail_setoff_npg_c.F
Chd|        FAIL_SETOFF_WIND_FRWAVE       source/materials/fail/fail_setoff_wind_frwave.F
Chd|        FAIL_SYAZWAN_C                source/materials/fail/syazwan/fail_syazwan_c.F
Chd|        FAIL_TAB2_C                   source/materials/fail/tabulated/fail_tab2_c.F
Chd|        FAIL_TAB_C                    source/materials/fail/tabulated/fail_tab_c.F
Chd|        FAIL_TAB_OLD_C                source/materials/fail/tabulated/fail_tab_old_c.F
Chd|        FAIL_TAB_OLD_XFEM             source/materials/fail/tabulated/fail_tab_old_xfem.F
Chd|        FAIL_TAB_XFEM                 source/materials/fail/tabulated/fail_tab_xfem.F
Chd|        FAIL_TBUTCHER_C               source/materials/fail/tuler_butcher/fail_tbutcher_c.F
Chd|        FAIL_TBUTCHER_XFEM            source/materials/fail/tuler_butcher/fail_tbutcher_xfem.F
Chd|        FAIL_TENSSTRAIN_C             source/materials/fail/tensstrain/fail_tensstrain_c.F
Chd|        FAIL_TSAIHILL_C               source/materials/fail/tsaihill/fail_tsaihill_c.F
Chd|        FAIL_TSAIWU_C                 source/materials/fail/tsaiwu/fail_tsaiwu_c.F
Chd|        FAIL_VISUAL_C                 source/materials/fail/visual/fail_visual_c.F
Chd|        FAIL_WIERZBICKI_C             source/materials/fail/wierzbicki/fail_wierzbicki_c.F
Chd|        FAIL_WILKINS_C                source/materials/fail/wilkins/fail_wilkins_c.F
Chd|        FAIL_WIND_FRWAVE              source/materials/fail/alter/fail_wind_frwave.F
Chd|        FAIL_WIND_XFEM                source/materials/fail/alter/fail_wind_xfem.F
Chd|        M25DELAM                      source/materials/mat/mat025/m25delam.F
Chd|        NOLIB_USERMAT99               source/user_interface/nolib_usermat99.F
Chd|        PUTSIGNORC3                   source/elements/shell/coqueba/cmatc3.F
Chd|        ROTOS4                        source/materials/mat_share/rotos4.F
Chd|        ROTOV                         source/airbag/roto.F          
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        UROTOV                        source/airbag/uroto.F         
Chd|        XFEM_CRK_DIR                  source/elements/xfem/xfem_crk_dir.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|        LAW_USERSH                    source/user_interface/law_usersh.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE USERMAT_SHELL (ELBUF_STR ,
     1       JFT      ,JLT      ,NEL      ,PM        ,FOR      ,MOM      ,    
     2       GSTR     ,THK      ,EINT     ,OFF       ,DIR_A    ,DIR_B    ,    
     3       MAT      ,AREA     ,EXX      ,EYY       ,EXY      ,EXZ      ,    
     4       EYZ      ,KXX      ,KYY      ,KXY       ,GEO      ,THK_LY   ,             
     5       PID      ,TF       ,NPF      ,MTN       ,DT1C     ,DM       ,           
     6       BUFMAT   ,SSP      ,RHO      ,VISCMX    ,IPLA     ,IOFC     ,    
     7       INDX     ,NGL      ,THKLY    ,MATLY     ,ZCFAC    ,NG       ,               
     8       SHF      ,GS       ,SIGY     ,THK0      ,EPSP     ,              
     9       POSLY    ,IGEO     ,IPM      ,FAILWAVE  ,FWAVE_EL ,                                   
     A       IFAILURE ,ALDT     ,TEMPEL   ,DIE       ,                        
     B       R11      ,R12      ,R13      ,R21       ,R22      ,R23      ,    
     C       R31      ,R32      ,R33      ,TABLE     ,IXFEM    ,ELCRKINI ,    
     D       DIR1_CRK ,DIR2_CRK ,IPARG    ,JHBE      ,ISMSTR   ,JTHE     ,                        
     E       TENSX    ,IR       ,IS       ,NLAY      ,NPT      ,IXLAY    ,             
     F       IXEL     ,ITHK     ,F_DEF    ,ISHPLYXFEM,                        
     G       ITASK    ,PM_STACK , ISUBSTACK,STACK    ,TSTAR    ,ALPE     ,
     H       PLY_EXX  ,PLY_EYY  ,PLY_EXY  ,PLY_EXZ   ,PLY_EYZ  ,PLY_F    ,
     I       VARNL    ,NLOC_DMG ,NLAY_MAX ,LAYNPT_MAX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE LAW_USERSH
      USE TABLE_MOD
      USE ELBUFDEF_MOD
      USE STACK_MOD
      USE FAILWAVE_MOD
      USE MESSAGE_MOD
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
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IR,IS,IPT,JFT,JLT,NEL,NPT,MTN,IPLA,IOFC,NG,NLAY,
     .  ISMSTR,IXFEM,NUVARR,IFAILURE,JHBE,IXLAY,IXEL,ITHK,JTHE,
     .  IREP,ISUBSTACK,ITASK,ISHPLYXFEM,NUMTABL
      INTEGER MAT(*),MATLY(*),PID(*),NPF(*),NGL(MVSIZ),INDX(MVSIZ),
     .   IGEO(NPROPGI,*),IPM(NPROPMI,*),ELCRKINI(NXLAYMAX,*),
     .   IPARG(*),FWAVE_EL(NEL)
      INTEGER, INTENT(IN) :: NLAY_MAX, LAYNPT_MAX
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
     .   DIR1_CRK(*),DIR2_CRK(*),TENSX(NEL,5),
     .   F_DEF(MVSIZ,8),PM_STACK(20,*),TSTAR(MVSIZ),ALPE(MVSIZ),
     .   PLY_EXX(MVSIZ,*),PLY_EYY(MVSIZ,*),PLY_EXY(MVSIZ,*), 
     .   PLY_EXZ(MVSIZ,*),PLY_EYZ(MVSIZ,*),PLY_F(MVSIZ,5,*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TARGET :: ALDT, IPM, VARNL
      TYPE (STACK_PLY) :: STACK
      TYPE (FAILWAVE_STR_) ,TARGET :: FAILWAVE 
      TYPE (NLOCAL_STR_) :: NLOC_DMG 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,JJ,IC,J,JMLY,IPG,IP,IT,IFL,ILAY,NPG,MPT,IMAT,
     .   IRUPT,NFAIL,IADBUF,IADBUFR,MAXNULVAR,IGTYP,NUVAR,NVARF,JDIR,
     .   NFUNC, JPOS, NINDX,NUPAR,NUPARAM,NFUNC1,IFAILWV,NLOCAL,
     .   IUN,IBID,IBIDON1,IBIDON2,IBIDON3,IBIDON4,IBIDON5,
     .   FAC,ICUT,ILAW_USER,IPTX,FAILEND,ILAYER,IROT,DMG_FLAG,
     .   IGMAT,IPGMAT,KK,MX,NPTT,IPT_ALL,NPTTOT,NUVARV,ILAW,
     .   FAILNPT,JOFF,SIZNUL,PLY_ID,IJ(5),K,ISEQ,PROGRESSIVE_CRACK,
     .   ORTH_DAMAGE,L_DMG,IPRONY,ISRATE,NVARTMP,INLOC,IDRAPE
      INTEGER :: IJ1,IJ2,IJ3,IJ4,IJ5
      INTEGER IFAIL(MVSIZ),IFUNC1(MAXFUNC),
     .   IFUNC(MAXFUNC),MATF(MVSIZ),IFLAG(1),IOFF_DUCT(MVSIZ),
     .   NFIS1(MVSIZ),NFIS2(MVSIZ),NFIS3(MVSIZ)
c
      my_real 
     .   DEGMB(MVSIZ) ,DEGFX(MVSIZ) ,SIGOFF(MVSIZ),
     .   THKLYL(MVSIZ),THKN(MVSIZ)  ,ETSE(MVSIZ),
     .   DEPSXX(MVSIZ),DEPSYY(MVSIZ),DEPSXY(MVSIZ),DEPSYZ(MVSIZ),
     .   DEPSZX(MVSIZ),EPSXX(MVSIZ) ,EPSYY(MVSIZ) ,EPSXY(MVSIZ),
     .   EPSYZ(MVSIZ) ,EPSZX(MVSIZ) ,EPSPXX(MVSIZ),EPSPYY(MVSIZ),
     .   EPSPXY(MVSIZ),EPSPYZ(MVSIZ),EPSPZX(MVSIZ),SIGOXX(MVSIZ),
     .   SIGOYY(MVSIZ),SIGOXY(MVSIZ),SIGOYZ(MVSIZ),SIGOZX(MVSIZ),
     .   SIGNXX(MVSIZ),SIGNYY(MVSIZ),SIGNXY(MVSIZ),SIGNYZ(MVSIZ),
     .   SIGNZX(MVSIZ),SIGVXX(MVSIZ),SIGVYY(MVSIZ),SIGVXY(MVSIZ),
     .   SIGVYZ(MVSIZ),SIGVZX(MVSIZ),TENS(MVSIZ,5),
     .   WMC(MVSIZ), EPSPL(MVSIZ), YLD(MVSIZ),DPLA(MVSIZ),
     .   VOL0(MVSIZ), COEF(MVSIZ),HARDM(MVSIZ),
     .   PLA0(MVSIZ),G_IMP(MVSIZ), SIGKSI(MVSIZ,5),JUMP_MAX(MVSIZ),
     .   WPLAR(MVSIZ),DAMCR(MVSIZ,2),DMAXT(MVSIZ),
     .   VISC(MVSIZ),OFF_OLD(MVSIZ),EPSP_LOC(MVSIZ),
     .   SIGNXX_FAIL(MVSIZ),SIGNYY_FAIL(MVSIZ),SIGNXY_FAIL(MVSIZ),
     .   SIGNYZ_FAIL(MVSIZ),SIGNZX_FAIL(MVSIZ),EINT_LOC(2,MVSIZ),
     .   SIGDMG(MVSIZ,5),AREAMIN(MVSIZ),DAREAMIN(MVSIZ),DMG_GLOB_SCALE(MVSIZ),
     .   DMG_LOC_SCALE(MVSIZ),BID_ARR(MVSIZ)
      my_real 
     .   FPSXX(MVSIZ),FPSYY(MVSIZ),FPSZZ(MVSIZ),FPSXY(MVSIZ),FPSYX(MVSIZ),
     .   EPCHK(MVSIZ),EPSPDT(MVSIZ)
      my_real 
     .   ZT,DTINV, VOL2,FCUT,ASRATE,EPS_M2,EPS_K2,
     .   R1,R2,R3,S1,S2,S3,R12A,R22A,S12B,S22B,RS1,RS2,RS3,
     .   T1,T2,T3,PHI,SUM1,SUM2,FACT,R3R3,S3S3,TRELAX, 
     .   BIDON1,BIDON2,BIDON3,BIDON4,BIDON5,RATIO,VV,AA
      my_real COPY_PLA(MVSIZ)
      my_real, DIMENSION(MVSIZ) :: DT_INV
      my_real, DIMENSION(NEL,5) :: DMG_ORTH_SCALE
      my_real  SCALE1(NEL)
      my_real ,DIMENSION(NEL), TARGET :: LE_MAX
      my_real TT_LOCAL
      my_real, DIMENSION(:) ,POINTER  :: EL_TEMP,YLDFAC,CRKLEN,CRKDIR,DADV,TFAIL,EL_LEN,
     .                                   EL_PLA
      my_real, DIMENSION(NLAY,100)  :: PTHKF
      TARGET :: TEMPEL,BUFMAT,EPSXX,EPSYY,DEPSXX,DEPSYY,SCALE1,POSLY,THKLY
C
      INTEGER , DIMENSION(MVSIZ) :: ADDITIONAL_INT_PARAMETERS
      my_real , DIMENSION(MVSIZ) :: ADDITIONAL_FLT_PARAMETERS
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
     .   DIRDMG,DIR_ORTH,ZZ,THLY,DAMINI
c----
      LOGICAL :: LOGICAL_USERL_AVAIL
      LOGICAL :: FLAG_EPS,FLAG_EPSP,FLAG_ZCFAC
      CHARACTER*256 MDS_LIBNAME
      INTEGER LENGTH
!
      CHARACTER OPTION*256
      INTEGER SIZE
      CHARACTER IUSER_KEY*ncharline
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
      GBUF => ELBUF_STR%GBUF
      IPG  = (IS-1)*ELBUF_STR%NPTR + IR     ! current Gauss point
      NPG  = ELBUF_STR%NPTR * ELBUF_STR%NPTS
      IGTYP = IGEO(11,PID(1))
      IGMAT = IGEO(98,PID(1))
      INLOC = IPARG(78)                     ! Flag for non-local regularization
      IDRAPE = ELBUF_STR%IDRAPE
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
        IF(IPM(222,IMAT) > 0) IPRONY=1
      ENDDO
C
!     ZCFAC computation is only useful for CZFORC3 and CZFORC3_CRK routines
      FLAG_ZCFAC=.FALSE.
      IF( (JHBE>=21.AND.JHBE<=29).OR.(IMPL_S>0)) FLAG_ZCFAC=.TRUE.
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
      IBIDON4 = 0
      IBIDON5 = 0
      IBID = 0
      ILAW_USER = IPM(217, MAT(1))
c      IMAT = IPM(1, MAT(1))
      IUN=1
c
      DMG_FLAG = 0
      TRELAX   = ZERO
c                                 
      DEGMB(JFT:JLT) = FOR(JFT:JLT,1)*EXX(JFT:JLT)+FOR(JFT:JLT,2)*EYY(JFT:JLT)  
     .               + FOR(JFT:JLT,3)*EXY(JFT:JLT)+FOR(JFT:JLT,4)*EYZ(JFT:JLT)  
     .               + FOR(JFT:JLT,5)*EXZ(JFT:JLT)                  
      DEGFX(JFT:JLT) = MOM(JFT:JLT,1)*KXX(JFT:JLT)+MOM(JFT:JLT,2)*KYY(JFT:JLT) +MOM(JFT:JLT,3)*KXY(JFT:JLT)                  
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
      COEF(JFT:JLT)   = ONE 
      OFF_OLD(JFT:JLT) = OFF(JFT:JLT)
      IOFF_DUCT(JFT:JLT) = 0
      DMG_GLOB_SCALE(JFT:JLT) = ONE
      SIGOFF(1:NEL) = ONE
      EPCHK(1:MVSIZ)  = ZERO
      VISCMX(1:MVSIZ) = ZERO
!
!       compute the inverse of dt and save the result 
      DO I=JFT,JLT
        DT_INV(I) = DT1C(I)/MAX(DT1C(I)**2,EM20)
      ENDDO
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
        DO I=JFT,JLT
          ASRATE = MIN(ONE, PM(9,MX)*DT1C(I))
          EPSP(I) = ASRATE*EPSP_LOC(I) + (ONE-ASRATE)*EPSP(I)
          EPSPL(I) = EPSP(I)
          EPSPDT(I) = EPSP(I)*DT1C(I)
        END DO
      ENDIF
c-----------------------------------------------------------
c---- intermediate buffer for GET_U_VAR function
      IF (TT == ZERO) THEN
        DO I=1,MVSIZ
          DO J=1,5000
            UUVAR(I,J) = ZERO
          END DO
        END DO
      ENDIF
c      MAXNULVAR = MIN(5000,NPT*NUVAR)
      DO ILAY=1,NLAY
        ILAYER = ILAY
        IF (IXFEM == 1 .AND. IXLAY > 0) ILAYER = IXLAY  ! xfem  multilayer
        IMAT = ELBUF_STR%BUFLY(ILAYER)%IMAT
        ILAW = ELBUF_STR%BUFLY(ILAYER)%ILAW
C
        DO IT = 1,ELBUF_STR%BUFLY(ILAYER)%NPTT
          UVAR => ELBUF_STR%BUFLY(ILAYER)%MAT(IR,IS,IT)%VAR
          NUVAR = ELBUF_STR%BUFLY(ILAYER)%NVAR_MAT
          DO J=1,NUVAR
            KK = ILAYER
            IF (NLAY == 1) KK = IT
            JJ = KK*J
            II = NEL*(J-1)
            IF (JJ > 5000) EXIT
            DO I=JFT,JLT
              UUVAR(I,JJ) = UVAR(II + I)
            ENDDO
          ENDDO
        ENDDO  !  IT = 1,NPTT
      ENDDO  !  DO ILAY =1,NLAY
C-----------------------------------------------------------
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
c
        DO I=1,NFUNC
          IFUNC(I)=IPM(10+I,IMAT)
        ENDDO  

        FLAG_EPSP = .TRUE.
        FLAG_EPS  = .TRUE.

        IF (IFAILURE == 1) THEN   
          DO IT=1,NPTT                                                         
            FBUF => BUFLY%FAIL(IR,IS,IT)                   
            DO IFL = 1, NFAIL      ! loop over fail models in current layer
              IRUPT  =  FBUF%FLOC(IFL)%ILAWF
              IF(IRUPT== 4.OR.IRUPT== 5.OR.IRUPT== 6.OR.IRUPT==7.OR.
     .           IRUPT==10.OR.IRUPT==24.OR.IRUPT==34.OR.IRUPT==39.OR.
     .           IRUPT==43.OR.IRUPT==47) FLAG_EPS=.TRUE.

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
          IF(IDRAPE > 0 ) JDIR = 1 + (IPT - 1)*JLT*2
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
          SIGOXX(JFT:JLT) = LBUF%SIG(IJ(1)+JFT:IJ(1)+JLT) 
          SIGOYY(JFT:JLT) = LBUF%SIG(IJ(2)+JFT:IJ(2)+JLT)
          SIGOXY(JFT:JLT) = LBUF%SIG(IJ(3)+JFT:IJ(3)+JLT)
          SIGOYZ(JFT:JLT) = LBUF%SIG(IJ(4)+JFT:IJ(4)+JLT)
          SIGOZX(JFT:JLT) = LBUF%SIG(IJ(5)+JFT:IJ(5)+JLT)
c------
          IF (IGTYP == 1) THEN
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
!       -------------------------------
!       IGTYP = 16
!       -------------------------------
          ELSEIF (IGTYP == 16) THEN
            ! 
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
                EPSXX(I) = T1     
                EPSYY(I) = T2   
              ENDDO
            ELSE  
              DO I=JFT,JLT
                II = JDIR + I-1
                R1 = DIR_A(II)
                S1 = DIR_A(II+NEL)
                R2 = DIR_B(II)
                S2 = DIR_B(II+NEL)
                ZT = ZZ(I)*THK0(I)
                T1 = EXX(I) + ZT*KXX(I)
                T2 = EYY(I) + ZT*KYY(I)
                T3 = HALF*(EXY(I) + ZT*KXY(I))                
                DEPSXY(I) = (R1*R2 + S1*S2) / (R1*S2 - R2*S1)   ! Tan(alpha_totale)
                DEPSXX(I) = R1*R1*T1 + S1*S1*T2 + TWO*R1*S1*T3 ! delta_eps_x dir1
                DEPSYY(I) = R2*R2*T1 + S2*S2*T2 + TWO*R2*S2*T3 ! delta_eps_y dir2
c
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
          ELSE   !  Igtyp 9/10/11/17/51/52
!               -------------------------------
            DO I=JFT,JLT                                 
              ZT       = ZZ(I)*THK0(I)                   
              TENS(I,1)=EXX(I)+ZT*KXX(I)                 
              TENS(I,2)=EYY(I)+ZT*KYY(I)                 
              TENS(I,3)=HALF*(EXY(I)+ZT*KXY(I))        
              TENS(I,4)=HALF*EYZ(I)                    
              TENS(I,5)=HALF*EXZ(I)                    
            ENDDO                                        
C
            CALL ROTOV(JFT,JLT,TENS,DIR_A(JDIR),NEL)   
C
            DO I=JFT,JLT                                 
              DEPSXX(I)=TENS(I,1)                        
              DEPSYY(I)=TENS(I,2)                        
              DEPSXY(I)=TWO*TENS(I,3)                   
              DEPSYZ(I)=TWO*TENS(I,4)                   
              DEPSZX(I)=TWO*TENS(I,5)                   
            ENDDO                                        
!
            DO I=JFT,JLT
              ZT=ZZ(I)*THK0(I)
              TENS(I,1)= GSTR(I,1)+ZT*GSTR(I,6)
              TENS(I,2)= GSTR(I,2)+ZT*GSTR(I,7)
              TENS(I,3)= HALF*(GSTR(I,3)+ZT*GSTR(I,8))
              TENS(I,4)= HALF*GSTR(I,4)
              TENS(I,5)= HALF*GSTR(I,5)          
            ENDDO
C
            CALL ROTOV(JFT,JLT,TENS,DIR_A(JDIR),NEL)
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

          ENDIF  ! IF (IGTYP == 1)
!       -------------------------------
!       end of IGTYP
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
          IF (ILAW == 29) THEN
            DO I=JFT,JLT
              U_TAGPLAS(I)=0
            END DO
!           copie EINT (du buffer) en local (EINT_LOC)
            DO I=JFT,JLT
              EINT_LOC(1,I) = EINT(I,1)
              EINT_LOC(2,I) = EINT(I,2)
              COPY_PLA(I)   = LBUF%PLA(I)
            ENDDO
!
            IF (LOGICAL_USERL_AVAIL) THEN
              TT_LOCAL = TT
              CALL ENG_USERLIB_SIGEPSC(29,
     1          JLT    ,NUPARAM,NUVAR   ,NFUNC    ,IFUNC   ,
     2          NPF    ,NPT    ,IPT     ,IFLAG    ,
     2          TF     ,TT_LOCAL,DT1C   ,UPARAM   ,RHO,
     3          AREA   ,EINT_LOC,THKLYL ,
     4          EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5          DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6          EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7          SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ   ,SIGOZX ,
     8          SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9          SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A          SSP    ,VISCMX ,THKN    ,COPY_PLA ,UVAR   ,
     B          OFF    ,NGL    ,SHF     )
            ELSE
!              CALL SIGEPS29C(
!     1          JLT    ,NUPARAM,NUVAR   ,NFUNC    ,IFUNC   ,
!     2          NPF    ,NPT    ,IPT     ,IFLAG    ,
!     2          TF     ,TT     ,DT1C    ,UPARAM   ,RHO,
!     3          AREA   ,EINT_LOC,THKLYL ,
!     4          EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
!     5          DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
!     6          EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
!     7          SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ   ,SIGOZX ,
!     8          SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
!     9          SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
!     A          SSP    ,VISCMX ,THKN    ,COPY_PLA ,UVAR   ,
!     B          OFF    ,NGL    ,SHF     )
!
!!!
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/MAT/LAW29 - SHELL '
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
!!!
            ENDIF
! recopie EINT_LOC en EINT (du buffer)
            DO I=JFT,JLT
              EINT(I,1) = EINT_LOC(1,I)
              EINT(I,2) = EINT_LOC(2,I)
              LBUF%PLA(I) = COPY_PLA(I)
            ENDDO
!
            DO I=JFT,JLT
              IF (U_TAGPLAS(I) /= 0) THEN
                SIGY(I)=U_YELD(I)
                ETSE(I)=U_ETSE(I)
              ELSE
                SIGY(I)=EP30
                ETSE(I)=ONE
              END IF
            END DO
          ELSEIF (ILAW == 30) THEN
            DO I=JFT,JLT
              U_TAGPLAS(I)=0
            END DO
! copie EINT (du buffer) en local (EINT_LOC)
            DO I=JFT,JLT
              EINT_LOC(1,I) = EINT(I,1)
              EINT_LOC(2,I) = EINT(I,2)
              COPY_PLA(I) = LBUF%PLA(I)
            ENDDO
!
            IF (LOGICAL_USERL_AVAIL) THEN
              TT_LOCAL = TT
              CALL ENG_USERLIB_SIGEPSC(30,
     1          JLT    ,NUPARAM,NUVAR   ,NFUNC    ,IFUNC   ,
     2          NPF    ,NPT    ,IPT     ,IFLAG    ,
     2          TF     ,TT_LOCAL,DT1C   ,UPARAM   ,RHO,
     3          AREA   ,EINT_LOC,THKLYL ,
     4          EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5          DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6          EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7          SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ   ,SIGOZX ,
     8          SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9          SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A          SSP    ,VISCMX ,THKN    ,COPY_PLA ,UVAR   ,
     B          OFF    ,NGL    ,SHF     )
            ELSE
!              CALL SIGEPS30C(
!     1          JLT    ,NUPARAM,NUVAR   ,NFUNC    ,IFUNC  ,
!     2          NPF    ,NPT    ,IPT     ,IFLAG    ,
!     2          TF     ,TT     ,DT1C    ,UPARAM   ,RHO    ,
!     3          AREA   ,EINT_LOC,THKLYL ,
!     4          EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
!     5          DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
!     6          EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
!     7          SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ   ,SIGOZX ,
!     8          SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
!     9          SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
!     A          SSP    ,VISCMX ,THKN    ,COPY_PLA ,UVAR   ,
!     B          OFF    ,NGL    ,SHF     )
!!!
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/MAT/LAW30 - SHELL '
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
!!!
            ENDIF
! recopie EINT_LOC en EINT (du buffer)
            DO I=JFT,JLT
              EINT(I,1) = EINT_LOC(1,I)
              EINT(I,2) = EINT_LOC(2,I)
              LBUF%PLA(I) = COPY_PLA(I)
            ENDDO
!
            DO I=JFT,JLT
              IF (U_TAGPLAS(I) /= 0) THEN
                SIGY(I)=U_YELD(I)
                ETSE(I)=U_ETSE(I)
              ELSE
                SIGY(I)=EP30
                ETSE(I)=ONE
              END IF
            END DO
          ELSEIF (ILAW == 31) THEN
            DO I=JFT,JLT
              U_TAGPLAS(I)=0
            END DO
! copie EINT (du buffer) en local (EINT_LOC)
            DO I=JFT,JLT
              EINT_LOC(1,I) = EINT(I,1)
              EINT_LOC(2,I) = EINT(I,2)
              COPY_PLA(I) = LBUF%PLA(I)
            ENDDO
!
            IF (LOGICAL_USERL_AVAIL) THEN
              TT_LOCAL = TT
              CALL ENG_USERLIB_SIGEPSC(31,
     1          JLT    ,NUPARAM,NUVAR   ,NFUNC    ,IFUNC   ,
     2          NPF    ,NPT    ,IPT     ,IFLAG    ,
     2          TF     ,TT_LOCAL,DT1C   ,UPARAM   ,RHO    ,
     3          AREA   ,EINT_LOC,THKLYL ,
     4          EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
     5          DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
     6          EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
     7          SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ   ,SIGOZX ,
     8          SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
     9          SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
     A          SSP    ,VISCMX ,THKN    ,COPY_PLA ,UVAR   ,
     B          OFF    ,NGL    ,SHF     )
            ELSE
!              CALL SIGEPS31C(
!     1          JLT    ,NUPARAM,NUVAR   ,NFUNC    ,IFUNC   ,
!     2          NPF    ,NPT    ,IPT     ,IFLAG    ,
!     2          TF     ,TT     ,DT1C    ,UPARAM   ,RHO    ,
!     3          AREA   ,EINT_LOC,THKLYL ,
!     4          EPSPXX ,EPSPYY ,EPSPXY  ,EPSPYZ   ,EPSPZX ,
!     5          DEPSXX ,DEPSYY ,DEPSXY  ,DEPSYZ   ,DEPSZX ,
!     6          EPSXX  ,EPSYY  ,EPSXY   ,EPSYZ    ,EPSZX  ,
!     7          SIGOXX ,SIGOYY ,SIGOXY  ,SIGOYZ   ,SIGOZX ,
!     8          SIGNXX ,SIGNYY ,SIGNXY  ,SIGNYZ   ,SIGNZX ,
!     9          SIGVXX ,SIGVYY ,SIGVXY  ,SIGVYZ   ,SIGVZX ,
!     A          SSP    ,VISCMX ,THKN    ,COPY_PLA ,UVAR   ,
!     B          OFF    ,NGL    ,SHF     )
!!!
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/MAT/LAW31 - SHELL '
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
!!!
            ENDIF
! recopie EINT_LOC en EINT (du buffer)
            DO I=JFT,JLT
              EINT(I,1) = EINT_LOC(1,I)
              EINT(I,2) = EINT_LOC(2,I)
              LBUF%PLA(I) = COPY_PLA(I)
            ENDDO
!
            DO I=JFT,JLT
              IF (U_TAGPLAS(I) /= 0) THEN
                SIGY(I)=U_YELD(I)
                ETSE(I)=U_ETSE(I)
              ELSE
                SIGY(I)=EP30
                ETSE(I)=ONE
              END IF
            END DO
c---------------------------------
          ELSEIF (ILAW == 99) THEN
c---------------------------------         
C          chrgt de la structure
          
C
! copie EINT (du buffer) en local (EINT_LOC) + COPY_PLA
            DO I=JFT,JLT
              EINT_LOC(1,I) = EINT(I,1)
              EINT_LOC(2,I) = EINT(I,2)
              COPY_PLA(I) = LBUF%PLA(I)
            ENDDO
!
            IF (LOGICAL_USERL_AVAIL) THEN
          
             IF (ISMSTR == 10) THEN
C         
               DO I=JFT,JLT 
                ZT=ZZ(I)*THK0(I)
                FPSXX(I) = F_DEF(I,1)+ZT*F_DEF(I,6) + ONE
                FPSYY(I) = F_DEF(I,2)+ZT*F_DEF(I,7) + ONE
                FPSXY(I) = F_DEF(I,3)+ZT*F_DEF(I,8) 
                FPSYX(I) = F_DEF(I,4)+ZT*F_DEF(I,5) 
                FPSZZ(I) = THKN(I)/GBUF%THK_I(I)
               END DO
               IF (IGTYP /= 1 .AND.IGTYP /= 16) THEN
                CALL ROTOS4(JFT,JLT,FPSXX,FPSYY,FPSXY,FPSYX,DIR_A(JDIR),NEL)
               END IF
             ELSE            
               DO I=JFT,JLT 
                FPSXX(I) = ZERO
                FPSYY(I) = ZERO
                FPSXY(I) = ZERO
                FPSYX(I) = ZERO
                FPSZZ(I) = ZERO
               END DO
             END IF
c            FILL STRUCTURE IN DYNAMICAL LIBRARY
             CALL ENG_USERLIB_GET_LAWC_VAR( 
     *            NCYCLE ,IMAT   ,IPT    ,NPT    ,IFLAG  ,
     *                               R11    ,R12    ,R13    ,R21    ,R22    ,
     *                               R23    ,R31    ,R32    ,R33    ,SIGOXX ,
     *                               SIGOYY ,SIGOXY ,SIGOYZ ,SIGOZX ,EPSPXX ,
     *                               EPSPYY ,EPSPXY ,EPSPYZ ,EPSPZX ,EPSXX  ,
     *                               EPSYY  ,EPSXY  ,EPSYZ  ,EPSZX  ,DEPSXX ,
     *                               DEPSYY ,DEPSXY ,DEPSYZ ,DEPSZX ,THKLYL     ,
     *                               THKN   ,SIGNXX ,SIGNYY ,SIGNXY ,SIGNYZ ,
     *                               SIGNZX ,SIGVXX ,SIGVYY ,SIGVXY ,SIGVYZ ,
     *                               SIGVZX ,DPLA )
          
             IF (DLIBTKVERS >= 1301501260) THEN
                SIZNUL=0           
                CALL ENG_USERLIB_GET_LAWC_VAR_2(
     *             FPSXX  ,MVSIZ ,FPSYY  ,MVSIZ,
     *             FPSZZ  ,MVSIZ ,FPSXY  ,MVSIZ ,FPSYX  ,MVSIZ ,EL_TEMP ,MVSIZ ,
     *             BID_ARR,IPG   ,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *             BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *             BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *             BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *             BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *             BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *             BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *             BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *             BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *             BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *             BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *             BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *             BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *             BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *             BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *             BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *             BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *             BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *             BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *             BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *             BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *             BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *             BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL)
             ENDIF
c            CALL SIGEPS99C in Library
             TT_LOCAL = TT
             CALL ENG_USERLIB_SIGEPS99C(
     *                  JLT      ,NUPARAM  ,NUVAR  ,ILAW_USER ,NFUNC   ,
     *                  IFUNC    ,NPF      ,NGL    ,TF        ,TT_LOCAL,
     *                  DT1C     ,UPARAM   ,RHO    ,AREA      ,EINT_LOC,
     *                  SHF      ,SSP      ,VISCMX ,COPY_PLA  ,UVAR    ,
     *                  OFF      ,SIGY     )
c            Retrieve Results
             CALL  ENG_USERLIB_SET_LAWC( 
     *               SIGNXX  ,SIGNYY ,SIGNXY, SIGNYZ,  SIGNZX,
     *               SIGVXX  ,SIGVYY ,SIGVXY, SIGVYZ,  SIGVZX,
     *               DPLA    ,ETSE   ,THKN  )
             IF (DLIBTKVERS >= 1301501260) THEN
               SIZNUL=0           
               CALL ENG_USERLIB_SET_LAWC_VAR_2(EL_TEMP,MVSIZ,BID_ARR,SIZNUL,
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *              BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL )
             ENDIF

            ELSE
              USERBUF%NCYCLE = NCYCLE
              USERBUF%ID     = IMAT
              USERBUF%ILAYER = IPT
              USERBUF%NPTA   = NPT 
              USERBUF%IFLAG  = IFLAG(1) 
C
              USERBUF%R11    = R11
              USERBUF%R12    = R12
              USERBUF%R13    = R13 
C
              USERBUF%R21    = R21
              USERBUF%R22    = R22
              USERBUF%R23    = R23 
C
              USERBUF%R31    = R31
              USERBUF%R32    = R32
              USERBUF%R33    = R33 
C
!               USERBUF%DIR_A  = DIR_A
!               USERBUF%DIR_B  = DIR_B
C
              USERBUF%SIGOXX = SIGOXX
              USERBUF%SIGOYY = SIGOYY
              USERBUF%SIGOXY = SIGOXY
              USERBUF%SIGOYZ = SIGOYZ
              USERBUF%SIGOZX = SIGOZX
C
              USERBUF%EPSPXX = EPSPXX
              USERBUF%EPSPYY = EPSPYY
              USERBUF%EPSPXY = EPSPXY
              USERBUF%EPSPYZ = EPSPYZ
              USERBUF%EPSPZX = EPSPZX
C                   
              USERBUF%EPSXX = EPSXX
              USERBUF%EPSYY = EPSYY
              USERBUF%EPSXY = EPSXY
              USERBUF%EPSYZ = EPSYZ
              USERBUF%EPSZX = EPSZX          
              IF (ISMSTR == 10) THEN
C
                DO I=JFT,JLT 
                 ZT=ZZ(I)*THK0(I)
                 USERBUF%FPSXX(I) = F_DEF(I,1)+ZT*F_DEF(I,6) + ONE
                 USERBUF%FPSYY(I) = F_DEF(I,2)+ZT*F_DEF(I,7) + ONE
                 USERBUF%FPSXY(I) = F_DEF(I,3)+ZT*F_DEF(I,8) 
                 USERBUF%FPSYX(I) = F_DEF(I,4)+ZT*F_DEF(I,5) 
                 USERBUF%FPSZZ(I) = THKN(I)/GBUF%THK_I(I)
                END DO
                IF (IGTYP /= 1 .AND.IGTYP /= 16) THEN
                 CALL ROTOS4(JFT,JLT,USERBUF%FPSXX,USERBUF%FPSYY,
     +                       USERBUF%FPSXY,USERBUF%FPSYX,DIR_A(JDIR),NEL)
                END IF
              ELSE            
                 USERBUF%FPSXX = ZERO
                 USERBUF%FPSYY = ZERO
                 USERBUF%FPSXY = ZERO
                 USERBUF%FPSYX = ZERO
                 USERBUF%FPSZZ = ZERO
              END IF
C                
              USERBUF%DEPSXX = DEPSXX
              USERBUF%DEPSYY = DEPSYY
              USERBUF%DEPSXY = DEPSXY
              USERBUF%DEPSYZ = DEPSYZ           
              USERBUF%DEPSZX = DEPSZX                     
C
              USERBUF%THKLYL = THKLYL          
              USERBUF%THKN   = THKN
              USERBUF%TEMP   = EL_TEMP
c
              USERBUF%SIGNXX = SIGNXX
              USERBUF%SIGNYY = SIGNYY
              USERBUF%SIGNXY = SIGNXY
              USERBUF%SIGNYZ = SIGNYZ
              USERBUF%SIGNZX = SIGNZX
c
              USERBUF%SIGVXX = SIGVXX
              USERBUF%SIGVYY = SIGVYY
              USERBUF%SIGVXY = SIGVXY
              USERBUF%SIGVYZ = SIGVYZ
              USERBUF%SIGVZX = SIGVZX
           
              USERBUF%DPLA = DPLA                  
C                   
!              CALL SIGEPS99C(
!     1        JLT       ,NUPARAM       ,NUVAR   ,ILAW_USER ,NFUNC   ,
!     2        IFUNC     ,NPF           ,NGL     ,TF        ,TT      ,
!     3        DT1C      ,UPARAM        ,RHO     ,AREA      ,EINT_LOC,
!     4        SHF       ,SSP           ,VISCMX  ,COPY_PLA  ,UVAR    ,
!     5        OFF       ,SIGY          ,USERBUF ,IUSER_KEY)
!!!
              ! ----------------
              ! ERROR to be printed & exit
                CALL NOLIB_USERMAT99(ILAW_USER, IUSER_KEY)
                OPTION='/MAT/'//IUSER_KEY//' - SHELL'
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
!!!
C
              SIGNXX = USERBUF%SIGNXX
              SIGNYY = USERBUF%SIGNYY
              SIGNXY = USERBUF%SIGNXY
              SIGNYZ = USERBUF%SIGNYZ 
              SIGNZX = USERBUF%SIGNZX
C               
              SIGVXX = USERBUF%SIGVXX
              SIGVYY = USERBUF%SIGVYY
              SIGVXY = USERBUF%SIGVXY
              SIGVYZ = USERBUF%SIGVYZ 
              SIGVZX = USERBUF%SIGVZX 
              DPLA   = USERBUF%DPLA
              ETSE   = USERBUF%ETSE
              THKN   = USERBUF%THKN
              IF (JTHE == 0) EL_TEMP = USERBUF%TEMP ! otherwise it's read only in user law
            ENDIF
c
! recopie EINT_LOC en EINT (du buffer)
            DO I=JFT,JLT
              EINT(I,1) = EINT_LOC(1,I)
              EINT(I,2) = EINT_LOC(2,I)
              LBUF%PLA(I) = COPY_PLA(I)
            ENDDO 
c--------------------------------
#ifdef DNC
        ELSEIF (MTN == 200 ) THEN  ! MDS
  
c--------------------------------
         IF(MDS_AVAIL == 1) THEN
            DO I=JFT,JLT
              EINT_LOC(1,I) = EINT(I,1)
              EINT_LOC(2,I) = EINT(I,2)
              COPY_PLA(I) = LBUF%PLA(I)
            ENDDO

            IF (ISMSTR == 10) THEN
C         
               DO I=JFT,JLT 
                ZT=ZZ(I)*THK0(I)
                FPSXX(I) = F_DEF(I,1)+ZT*F_DEF(I,6) + ONE
                FPSYY(I) = F_DEF(I,2)+ZT*F_DEF(I,7) + ONE
                FPSXY(I) = F_DEF(I,3)+ZT*F_DEF(I,8) 
                FPSYX(I) = F_DEF(I,4)+ZT*F_DEF(I,5) 
                FPSZZ(I) = THKN(I)/GBUF%THK_I(I)
               END DO
               IF (IGTYP /= 1 .AND.IGTYP /= 16) THEN
                CALL ROTOS4(JFT,JLT,FPSXX,FPSYY,FPSXY,FPSYX,DIR_A(JDIR),NEL)
               END IF
             ELSE            
               DO I=JFT,JLT 
                FPSXX(I) = ZERO
                FPSYY(I) = ZERO
                FPSXY(I) = ZERO
                FPSYX(I) = ZERO
                FPSZZ(I) = ZERO
               END DO
             END IF

           CALL ENG_MDS_SIGEPS_C( NCYCLE , BUFLY%IMAT, NGL ,
     *                            NEL    , NPT      , IT ,ILAY, IPG     , IFLAG,
     *                            UPARAM  , NUPARAM  , UVAR    , NUVAR,
     *                            NFUNC   , IFUNC    , TF      , NPF ,
     *                            TT_LOCAL, DT1C     , RHO     , AREA, EINT_LOC,
     *                            THKLY   , THK      , SHF     , ETSE    ,
     *                            EPSPXX  , EPSPYY  , EPSPXY  , EPSPYZ  , EPSPZX  ,
     *                            DEPSXX  , DEPSYY  , DEPSXY  , DEPSYZ  , DEPSZX  ,
     *                            EPSXX   , EPSYY   , EPSXY   , EPSYZ   , EPSZX   , 
     *                            SSP     , VISCMX  , COPY_PLA, OFF     ,
     *                            EL_TEMP , R11     , R12     , R13     , R21     ,
     *                            R22     , R23     , R31     , R32     , R33     ,
     *                            SIGY    , SIGOXX  , SIGOYY  , SIGOXY  , SIGOYZ  ,
     *                            SIGOZX  , SIGNXX  , SIGNYY  , SIGNXY  ,
     *                            SIGNYZ  , SIGNZX  , SIGVXX  , SIGVYY  , SIGVXY  ,
     *                            SIGVYZ  , SIGVZX  ,DPLA,
     *                            ADDITIONAL_FLT_PARAMETERS,ADDITIONAL_INT_PARAMETERS)
!
 ! recopie EINT_LOC en EINT (du buffer)
            DO I=JFT,JLT
              EINT(I,1) = EINT_LOC(1,I)
              EINT(I,2) = EINT_LOC(2,I)
              LBUF%PLA(I) = COPY_PLA(I)
            ENDDO 
         ELSE
             CALL MDS_USERLIB_NAME_GET(MDS_LIBNAME,LENGTH)
             CALL ANCMSG(MSGID=287,ANMODE=ANINFO,
     .            C1=MDS_LIBNAME(1:LENGTH))
              CALL ARRET(2)
         ENDIF                         ! MDS_AVAIL == 1 
#endif
       ENDIF !ILAW
c-------------------------------------------
        DO I=JFT,JLT
          VISCMX(I) = MAX(DM,VISCMX(I))
        ENDDO
c-------------------------------------------
c       End of material laws
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
            DMG_LOC_SCALE(1:NEL) = ONE
            DMG_ORTH_SCALE(1:NEL,1:5) = ONE
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
            DADV  => GBUF%DMG
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
              DAMINI => FBUF%FLOC(IFL)%DAMINI
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
     1            NEL       ,NUPAR     ,NFUNC1   ,IFUNC1     ,
     2            NPF       ,TF        ,TT        ,UPARAMF    ,   
     3            NGL       ,IPG       ,ILAYER   ,IT         ,
     4            EPSXX     ,EPSYY     ,EPSXY     ,
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
     6               DMG_FLAG  ,DMG_LOC_SCALE ,ALDT  ,TSTAR     ,ISMSTR    )                                
c
              CASE (11)     !    Energy failure model
                CALL FAIL_ENERGY_C(
     1               NEL       ,NUPAR     ,NVARF     ,NFUNC1   ,IFUNC1     , 
     2               UPARAMF   ,UVARF     ,NPF       ,TF       ,TT         , 
     3               NGL       ,IPG       ,ILAYER    ,IT       ,EPSPL      ,  
     4               AREA      ,THKN      ,PTHKF(ILAYER,IFL)   ,DMG_FLAG   , 
     5               DMG_LOC_SCALE ,OFF   ,FOFF      ,DFMAX    ,TDEL       ,
     6               SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ   ,SIGNZX     ,
     7               DEPSXX    ,DEPSYY    ,DEPSXY    ,DEPSYZ   ,DEPSZX     )     
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
     6               EPSP      ,FWAVE_EL  ,DADV      ) 
c
              CASE (16)     !    Modified Puck failure model
                CALL FAIL_PUCK_C(
     1               NEL       ,NUPAR     ,NVARF     ,UPARAMF   ,UVARF     ,
     2               TT        ,NGL       ,IPG       ,ILAYER    ,IT        ,
     3               SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ    ,SIGNZX    ,
     4               OFF       ,FOFF ,PTHKF(ILAYER,IFL),DMG_FLAG,DMG_LOC_SCALE ,
     5               DFMAX     ,TDEL      ,DT1C      )
c
              CASE (23)     !    Tabulated failure model
                IF (IXFEM == 0) THEN
                  CALL FAIL_TAB_C(
     1                NEL       ,NUPAR     ,NVARF     ,UPARAMF   ,UVARF     ,
     2                NFUNC1    ,IFUNC1    ,TABLE     ,NPF       ,TF        ,
     3                TT        ,NGL       ,IPG       ,ILAYER    ,IT        , 
     4                SIGNXX    ,SIGNYY    ,SIGNXY    ,   
     5                DPLA      ,EPSPL     ,THKN      ,EL_LEN    ,TSTAR     ,
     6                PTHKF(ILAYER,IFL),DMG_FLAG,DMG_LOC_SCALE ,OFF  ,FOFF      ,
     7                DFMAX     ,TDEL      ,IGTYP     ,INLOC     )
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
     6          IFUNC1   ,NPF     ,TF       ,EL_LEN   ,FOFF     ,IPG      ,
     7          DMG_FLAG ,DMG_LOC_SCALE)
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
     5           DFMAX    ,TDEL     ,UELR1    ,MPT      ,IGTYP      ,
     6           FLD_IDX  ,DAM      ,EL_PLA   )
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
                CALL FAIL_VISUAL_C(                                           
     1             JLT      ,NVARF   ,TT       ,DT1     ,UPARAMF ,NGL      ,
     2             SIGNXX   ,SIGNYY  ,SIGNXY   ,EPSXX   ,EPSYY   ,EPSXY    ,
     3             UVARF    ,OFF     ,DFMAX    ,ISMSTR  )
c
              CASE (37)     !    old (obsolete) abulated failure model
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
     4            DPLA     ,EPSPL   ,UVARF    ,GBUF%NOFF,UELR1    ,
     5            OFF      ,OFFL    ,DFMAX    ,TDEL     ,NFUNC1   ,
     6            IFUNC1   ,NPF     ,TF       ,EL_LEN   ,FOFF     , 
     7            IGTYP    )
c
              CASE (39)     !    GENE1
c
                CALL FAIL_GENE1_C(
     1            JLT      ,NUPAR    ,NVARF    ,NFUNC1   ,IFUNC1   ,
     2            NPF      ,TF       ,TT       ,DT1C     ,UPARAMF  ,IPG      ,
     3            NGL      ,GBUF%DT  ,EPSPL    ,UVARF    ,OFF      ,    
     4            EPSXX    ,EPSYY    ,EPSXY    ,AREA     ,THKN     ,            
     5            SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   , 
     6            EL_TEMP  ,DFMAX    ,ALDT     ,TABLE    ,TDEL     ,
     7            THK0     ,IPT      ,PTHKF(ILAYER,IFL)  ,FOFF     ,THKLYL   )
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
     7            PTHKF(ILAYER,IFL)  ,DAMINI   ,AREA     ,INLOC    ,
     8            NPG      )
c
              CASE (43)     !    Syazwan failure model
c
                CALL FAIL_SYAZWAN_C(
     1            JLT      ,UPARAMF  ,NUPAR    ,UVARF    ,NVARF    ,
     2            TT       ,NGL      ,IPT      ,DPLA     ,EL_PLA   ,
     3            SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     4            EPSXX    ,EPSYY    ,EPSXY    ,EPSYZ    ,EPSZX    ,
     5            DFMAX    ,NFUNC1   ,IFUNC1   ,EL_LEN   ,FOFF     ,
     6            IPG      ,DMG_FLAG ,DMG_LOC_SCALE,NPF  ,TF       )
c
              CASE (44)     !    Tsai-Wu failure model
c
                CALL FAIL_TSAIWU_C(
     1            NEL       ,NUPAR     ,NVARF     ,UPARAMF   ,UVARF     ,
     2            TT        ,NGL       ,IPG       ,ILAYER    ,IT        ,
     3            SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ    ,SIGNZX    ,
     4            OFF       ,FOFF ,PTHKF(ILAYER,IFL),DMG_FLAG,DMG_LOC_SCALE ,
     5            DFMAX     ,TDEL      ,DT1C      )
c
              CASE (45)     !    Tsai-Hill failure model
c
                CALL FAIL_TSAIHILL_C(
     1            NEL       ,NUPAR     ,NVARF     ,UPARAMF   ,UVARF     ,
     2            TT        ,NGL       ,IPG       ,ILAYER    ,IT        ,
     3            SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ    ,SIGNZX    ,
     4            OFF       ,FOFF ,PTHKF(ILAYER,IFL),DMG_FLAG,DMG_LOC_SCALE ,
     5            DFMAX     ,TDEL      ,DT1C      )
c 
              CASE (46)     !    Hoffman failure model
c
                CALL FAIL_HOFFMAN_C(
     1            NEL       ,NUPAR     ,NVARF     ,UPARAMF   ,UVARF     ,
     2            TT        ,NGL       ,IPG       ,ILAYER    ,IT        ,
     3            SIGNXX    ,SIGNYY    ,SIGNXY    ,SIGNYZ    ,SIGNZX    ,
     4            OFF       ,FOFF ,PTHKF(ILAYER,IFL),DMG_FLAG,DMG_LOC_SCALE ,
     5            DFMAX     ,TDEL      ,DT1C      )
c
              CASE (47)     !    Maximum Strain failure model
c
                CALL FAIL_MAXSTRAIN_C(
     1            NEL       ,NUPAR     ,NVARF     ,UPARAMF   ,UVARF     ,
     2            TT        ,NGL       ,IPG       ,ILAYER    ,IT        ,
     3            EPSXX     ,EPSYY     ,EPSXY     ,EPSYZ     ,EPSZX     ,
     4            OFF       ,FOFF ,PTHKF(ILAYER,IFL),DMG_FLAG,DMG_LOC_SCALE ,
     5            DFMAX     ,TDEL      ,DT1C      )
c
              CASE (48)     !    Orthotropic energy failure model
c               
                CALL FAIL_ORTHENERG_C(
     1            NEL       ,NUPAR     ,NVARF     ,UPARAMF   ,UVARF     ,
     2            NGL       ,TT        ,IPG       ,ILAYER    ,IT        , 
     3            DEPSXX    ,DEPSYY    ,DEPSXY    ,DMG_FLAG  ,DMG_ORTH_SCALE,
     4            ALDT      ,PTHKF(ILAYER,IFL),FOFF,DFMAX    ,TDEL      ,
     5            SIGNXX    ,SIGNYY    ,SIGNXY    ,IGTYP     ,PLY_ID    )
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
          IF (IGTYP /= 1) THEN                                                     
            SELECT CASE (DMG_FLAG)                                            
            CASE (0)                                                           
               TENS(JFT:JLT,1) = SIGNXX(JFT:JLT)+SIGVXX(JFT:JLT)
               TENS(JFT:JLT,2) = SIGNYY(JFT:JLT)+SIGVYY(JFT:JLT)
               TENS(JFT:JLT,3) = SIGNXY(JFT:JLT)+SIGVXY(JFT:JLT)
               TENS(JFT:JLT,4) = SIGNYZ(JFT:JLT)+SIGVYZ(JFT:JLT)
               TENS(JFT:JLT,5) = SIGNZX(JFT:JLT)+SIGVZX(JFT:JLT)
            CASE (1)                                                           
               TENS(JFT:JLT,1) = (SIGNXX(JFT:JLT)+SIGVXX(JFT:JLT))*DMG_LOC_SCALE(JFT:JLT)                               
               TENS(JFT:JLT,2) = (SIGNYY(JFT:JLT)+SIGVYY(JFT:JLT))*DMG_LOC_SCALE(JFT:JLT)                               
               TENS(JFT:JLT,3) = (SIGNXY(JFT:JLT)+SIGVXY(JFT:JLT))*DMG_LOC_SCALE(JFT:JLT)                               
               TENS(JFT:JLT,4) = (SIGNYZ(JFT:JLT)+SIGVYZ(JFT:JLT))*DMG_LOC_SCALE(JFT:JLT)                               
               TENS(JFT:JLT,5) = (SIGNZX(JFT:JLT)+SIGVZX(JFT:JLT))*DMG_LOC_SCALE(JFT:JLT)  
            CASE (2)                                                           
               TENS(JFT:JLT,1) = (SIGNXX(JFT:JLT)+SIGVXX(JFT:JLT))*DMG_ORTH_SCALE(JFT:JLT,1)                               
               TENS(JFT:JLT,2) = (SIGNYY(JFT:JLT)+SIGVYY(JFT:JLT))*DMG_ORTH_SCALE(JFT:JLT,2)                               
               TENS(JFT:JLT,3) = (SIGNXY(JFT:JLT)+SIGVXY(JFT:JLT))*DMG_ORTH_SCALE(JFT:JLT,3)                               
               TENS(JFT:JLT,4) = (SIGNYZ(JFT:JLT)+SIGVYZ(JFT:JLT))*DMG_ORTH_SCALE(JFT:JLT,4)                               
               TENS(JFT:JLT,5) = (SIGNZX(JFT:JLT)+SIGVZX(JFT:JLT))*DMG_ORTH_SCALE(JFT:JLT,5)                 
            END SELECT
c
            IF (IGTYP == 16) THEN                                                  
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
            ELSE                                                             
             CALL UROTOV(JFT,JLT,TENS,DIR_A(JDIR),NEL)
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
              MOM(I,1) = MOM(I,1) + WMC(I)*TENS(I,1)                           
              MOM(I,2) = MOM(I,2) + WMC(I)*TENS(I,2)                           
              MOM(I,3) = MOM(I,3) + WMC(I)*TENS(I,3)                           
            ENDDO                                                              
c
          ELSE    ! IGTYP = 1                                                  
            SELECT CASE (DMG_FLAG)                                               
            CASE (0)     
                FOR(1:NEL,1) = FOR(1:NEL,1) + THLY(1:NEL)*(SIGNXX(1:NEL)+SIGVXX(1:NEL))            
                FOR(1:NEL,2) = FOR(1:NEL,2) + THLY(1:NEL)*(SIGNYY(1:NEL)+SIGVYY(1:NEL))            
                FOR(1:NEL,3) = FOR(1:NEL,3) + THLY(1:NEL)*(SIGNXY(1:NEL)+SIGVXY(1:NEL))            
                FOR(1:NEL,4) = FOR(1:NEL,4) + THLY(1:NEL)*(SIGNYZ(1:NEL)+SIGVYZ(1:NEL))            
                FOR(1:NEL,5) = FOR(1:NEL,5) + THLY(1:NEL)*(SIGNZX(1:NEL)+SIGVZX(1:NEL))            
                MOM(1:NEL,1) = MOM(1:NEL,1) + WMC(1:NEL)*(SIGNXX(1:NEL)+SIGVXX(1:NEL))             
                MOM(1:NEL,2) = MOM(1:NEL,2) + WMC(1:NEL)*(SIGNYY(1:NEL)+SIGVYY(1:NEL))             
                MOM(1:NEL,3) = MOM(1:NEL,3) + WMC(1:NEL)*(SIGNXY(1:NEL)+SIGVXY(1:NEL))                
            CASE (1)  ! softening with DMG_LOC_SCALE from failure model          
                FOR(1:NEL,1) = FOR(1:NEL,1) + THLY(1:NEL)*(SIGNXX(1:NEL)+SIGVXX(1:NEL))*DMG_LOC_SCALE(1:NEL)   
                FOR(1:NEL,2) = FOR(1:NEL,2) + THLY(1:NEL)*(SIGNYY(1:NEL)+SIGVYY(1:NEL))*DMG_LOC_SCALE(1:NEL)   
                FOR(1:NEL,3) = FOR(1:NEL,3) + THLY(1:NEL)*(SIGNXY(1:NEL)+SIGVXY(1:NEL))*DMG_LOC_SCALE(1:NEL)   
                FOR(1:NEL,4) = FOR(1:NEL,4) + THLY(1:NEL)*(SIGNYZ(1:NEL)+SIGVYZ(1:NEL))*DMG_LOC_SCALE(1:NEL)   
                FOR(1:NEL,5) = FOR(1:NEL,5) + THLY(1:NEL)*(SIGNZX(1:NEL)+SIGVZX(1:NEL))*DMG_LOC_SCALE(1:NEL)   
                MOM(1:NEL,1) = MOM(1:NEL,1) + WMC(1:NEL) *(SIGNXX(1:NEL)+SIGVXX(1:NEL))*DMG_LOC_SCALE(1:NEL)   
                MOM(1:NEL,2) = MOM(1:NEL,2) + WMC(1:NEL) *(SIGNYY(1:NEL)+SIGVYY(1:NEL))*DMG_LOC_SCALE(1:NEL)   
                MOM(1:NEL,3) = MOM(1:NEL,3) + WMC(1:NEL) *(SIGNXY(1:NEL)+SIGVXY(1:NEL))*DMG_LOC_SCALE(1:NEL) 
            CASE (3)  ! orthotropic softening with DMG_ORTH_SCALE from failure model          
                FOR(1:NEL,1) = FOR(1:NEL,1) + THLY(1:NEL)*(SIGNXX(1:NEL)+SIGVXX(1:NEL))*DMG_ORTH_SCALE(1:NEL,1)   
                FOR(1:NEL,2) = FOR(1:NEL,2) + THLY(1:NEL)*(SIGNYY(1:NEL)+SIGVYY(1:NEL))*DMG_ORTH_SCALE(1:NEL,2)   
                FOR(1:NEL,3) = FOR(1:NEL,3) + THLY(1:NEL)*(SIGNXY(1:NEL)+SIGVXY(1:NEL))*DMG_ORTH_SCALE(1:NEL,3)   
                FOR(1:NEL,4) = FOR(1:NEL,4) + THLY(1:NEL)*(SIGNYZ(1:NEL)+SIGVYZ(1:NEL))*DMG_ORTH_SCALE(1:NEL,4)   
                FOR(1:NEL,5) = FOR(1:NEL,5) + THLY(1:NEL)*(SIGNZX(1:NEL)+SIGVZX(1:NEL))*DMG_ORTH_SCALE(1:NEL,5)   
                MOM(1:NEL,1) = MOM(1:NEL,1) + WMC(1:NEL) *(SIGNXX(1:NEL)+SIGVXX(1:NEL))*DMG_ORTH_SCALE(1:NEL,1)   
                MOM(1:NEL,2) = MOM(1:NEL,2) + WMC(1:NEL) *(SIGNYY(1:NEL)+SIGVYY(1:NEL))*DMG_ORTH_SCALE(1:NEL,2)   
                MOM(1:NEL,3) = MOM(1:NEL,3) + WMC(1:NEL) *(SIGNXY(1:NEL)+SIGVXY(1:NEL))*DMG_ORTH_SCALE(1:NEL,3) 
            END SELECT                                                             
c
          ENDIF   ! IGTYP                                                          
C-----------------------------------------------
C         FACTEURS POUR COQUES B.L. (Zeng&Combescure)
C-----------------------------------------------
          IF(FLAG_ZCFAC) THEN
              ZCFAC(JFT:JLT,1) = ZCFAC(JFT:JLT,1) + ETSE(JFT:JLT) * THLY(JFT:JLT)
              ZCFAC(JFT:JLT,2) = MIN(ETSE(JFT:JLT),ZCFAC(JFT:JLT,2))  
          ENDIF
          YLD(JFT:JLT) = YLD(JFT:JLT) + SIGY(JFT:JLT)*THLY(JFT:JLT)
c-----------------------------------------------
          IF (IMPL_S > 0) THEN
            CALL PUTSIGNORC3(JFT ,JLT ,IUN,NG,IPT,G_IMP ,SIGKSI)
          END IF
c-----------------------------------------------
          IF (IXEL == 0 .and. PROGRESSIVE_CRACK == 0) THEN   ! original element
            DIR_ORTH => BUFLY%DIRA
            IROT = ELBUF_STR%BUFLY(ILAYER)%LY_DIRA
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
     .                       ISUBSTACK,IGTYP    ,FAILWAVE ,FWAVE_EL ,
     .                       NLAY_MAX ,LAYNPT_MAX,NUMGEO  ,NUMSTACK ,
     .                       IGEO     )
        ELSE
          CALL FAIL_SETOFF_NPG_C(
     .                       ELBUF_STR,GEO      ,PID(1)   ,NGL      ,
     .                       NEL      ,IR       ,IS       ,NLAY     ,
     .                       NPTTOT   ,PTHKF    ,THK_LY   ,THKLY    ,
     .                       OFF      ,NPG      ,STACK    ,ISUBSTACK, 
     .                       IGTYP    ,FAILWAVE ,FWAVE_EL ,NLAY_MAX ,
     .                       LAYNPT_MAX,NUMGEO  ,IPG      ,NUMSTACK ,
     .                       IGEO     )
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
c
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
c
        DEGMB(JFT:JLT) = DEGMB(JFT:JLT)+ FOR(JFT:JLT,1)*EXX(JFT:JLT)+FOR(JFT:JLT,2)*EYY(JFT:JLT)
     .                     + FOR(JFT:JLT,3)*EXY(JFT:JLT)+FOR(JFT:JLT,4)*EYZ(JFT:JLT)
     .                     + FOR(JFT:JLT,5)*EXZ(JFT:JLT)
        DEGFX(JFT:JLT) = DEGFX(JFT:JLT)+ MOM(JFT:JLT,1)*KXX(JFT:JLT)+MOM(JFT:JLT,2)*KYY(JFT:JLT)
     .                     + MOM(JFT:JLT,3)*KXY(JFT:JLT)
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
