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
Chd|  MMAIN_MOD                     source/materials/mat_share/mmain.F
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|        IG3DUFORC3                    source/elements/ige3d/ig3duforc3.F
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S16FORC3                      source/elements/thickshell/solide16/s16forc3.F
Chd|        S20FORC3                      source/elements/solid/solide20/s20forc3.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SPSTRES                       source/elements/sph/spstres.F 
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE MMAIN_MOD
      CONTAINS
Chd|====================================================================
Chd|  MMAIN                         source/materials/mat_share/mmain.F
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|        IG3DUFORC3                    source/elements/ige3d/ig3duforc3.F
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S16FORC3                      source/elements/thickshell/solide16/s16forc3.F
Chd|        S20FORC3                      source/elements/solid/solide20/s20forc3.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SPSTRES                       source/elements/sph/spstres.F 
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ATUR17                        source/ale/turbulence/atur17.F
Chd|        ATURBN                        source/ale/turbulence/aturbn.F
Chd|        ENG_USERLIB_FLAW              source/user_interface/dyn_userlib.c
Chd|        EOSMAIN                       ../common_source/eos/eosmain.F
Chd|        EOSUPDA                       ../common_source/eos/eosupda.F
Chd|        FAIL_BIQUAD_S                 source/materials/fail/biquad/fail_biquad_s.F
Chd|        FAIL_CHANGCHANG_S             source/materials/fail/changchang/fail_changchang_s.F
Chd|        FAIL_COCKROFT_S               source/materials/fail/cockroft_latham/fail_cockroft_s.F
Chd|        FAIL_EMC                      source/materials/fail/emc/fail_emc.F
Chd|        FAIL_ENERGY_S                 source/materials/fail/energy/fail_energy_s.F
Chd|        FAIL_GENE1_S                  source/materials/fail/gene1/fail_gene1_s.F
Chd|        FAIL_HASHIN_S                 source/materials/fail/hashin/fail_hashin_s.F
Chd|        FAIL_HOFFMAN_S                source/materials/fail/hoffman/fail_hoffman_s.F
Chd|        FAIL_INIEVO_S                 source/materials/fail/inievo/fail_inievo_s.F
Chd|        FAIL_JOHNSON                  source/materials/fail/johnson_cook/fail_johnson.F
Chd|        FAIL_LADEVEZE                 source/materials/fail/ladeveze/fail_ladeveze.F
Chd|        FAIL_MAXSTRAIN_S              source/materials/fail/max_strain/fail_maxstrain_s.F
Chd|        FAIL_ORTHBIQUAD_S             source/materials/fail/orthbiquad/fail_orthbiquad_s.F
Chd|        FAIL_ORTHENERG_S              source/materials/fail/orthenerg/fail_orthenerg_s.F
Chd|        FAIL_ORTHSTRAIN               source/materials/fail/orthstrain/fail_orthstrain_s.F
Chd|        FAIL_PUCK_S                   source/materials/fail/puck/fail_puck_s.F
Chd|        FAIL_RTCL_S                   source/materials/fail/rtcl/fail_rtcl_s.F
Chd|        FAIL_SAHRAEI_S                source/materials/fail/sahraei/fail_sahraei_s.F
Chd|        FAIL_SPALLING_S               source/materials/fail/spalling/fail_spalling_s.F
Chd|        FAIL_SYAZWAN_S                source/materials/fail/syazwan/fail_syazwan_s.F
Chd|        FAIL_TAB2_S                   source/materials/fail/tabulated/fail_tab2_s.F
Chd|        FAIL_TAB_S                    source/materials/fail/tabulated/fail_tab_s.F
Chd|        FAIL_TBUTCHER_S               source/materials/fail/tuler_butcher/fail_tbutcher_s.F
Chd|        FAIL_TENSSTRAIN_S             source/materials/fail/tensstrain/fail_tensstrain_s.F
Chd|        FAIL_TSAIHILL_S               source/materials/fail/tsaihill/fail_tsaihill_s.F
Chd|        FAIL_TSAIWU_S                 source/materials/fail/tsaiwu/fail_tsaiwu_s.F
Chd|        FAIL_VISUAL_S                 source/materials/fail/visual/fail_visual_s.F
Chd|        FAIL_WIERZBICKI_S             source/materials/fail/wierzbicki/fail_wierzbicki_s.F
Chd|        FAIL_WILKINS_S                source/materials/fail/wilkins/fail_wilkins_s.F
Chd|        GRAY10                        source/materials/mat/mat016/gray10.F
Chd|        GRAY20                        source/materials/mat/mat016/gray20.F
Chd|        GRAY30                        source/materials/mat/mat016/gray30.F
Chd|        M10LAW                        source/materials/mat/mat010/m10law.F
Chd|        M11LAW                        source/materials/mat/mat011/m11law.F
Chd|        M12LAW                        source/materials/mat/mat012/m12law.F
Chd|        M13LAW                        source/materials/mat/mat013/m13law.F
Chd|        M14LAW                        source/materials/mat/mat014/m14law.F
Chd|        M16LAW                        source/materials/mat/mat016/m16law.F
Chd|        M17LAW                        source/materials/mat/mat017/m17law.F
Chd|        M18LAW                        source/materials/mat/mat018/m18law.F
Chd|        M1LAW                         source/materials/mat/mat001/m1law.F
Chd|        M1LAWI                        source/materials/mat/mat001/m1lawi.F
Chd|        M1LAWTOT                      source/materials/mat/mat001/m1lawtot.F
Chd|        M21LAW                        source/materials/mat/mat021/m21law.F
Chd|        M22LAW                        source/materials/mat/mat022/m22law.F
Chd|        M24LAW                        source/materials/mat/mat024/m24law.F
Chd|        M25LAW                        source/materials/mat/mat025/m25law.F
Chd|        M26LAW                        source/materials/mat/mat026/m26law.F
Chd|        M2LAW                         source/materials/mat/mat002/m2law.F
Chd|        M3LAW                         source/materials/mat/mat003/m3law.F
Chd|        M46LAW                        source/materials/mat/mat046/m46law.F
Chd|        M49LAW                        source/materials/mat/mat049/m49law.F
Chd|        M4LAW                         source/materials/mat/mat004/m4law.F
Chd|        M5LAW                         source/materials/mat/mat005/m5law.F
Chd|        M6LAW                         source/materials/mat/mat006/m6law.F
Chd|        MDTSPH                        source/materials/mat_share/mdtsph.F
Chd|        MEINT                         source/materials/mat_share/meint.F
Chd|        MJWL                          source/materials/mat/mat005/mjwl.F
Chd|        MNSVIS                        source/materials/mat_share/mnsvis.F
Chd|        MQVISC26                      source/materials/mat_share/mqvisc26.F
Chd|        MQVISCB                       source/materials/mat_share/mqviscb.F
Chd|        MREPLOC                       source/materials/mat_share/mreploc.F
Chd|        MROTENS                       source/materials/mat_share/mrotens.F
Chd|        MTHETA                        source/materials/mat_share/mtheta.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        PUTSIGNOR3                    source/elements/solid/solide8z/putsignor3.F
Chd|        PUT_ETFAC                     source/elements/solid/solide8z/put_etfac.F
Chd|        SBOLTLAW                      source/elements/solid/solide/sboltlaw.F
Chd|        SESA10                        source/materials/mat/mat026/sesa10.F
Chd|        SESA20                        source/materials/mat/mat026/sesa20.F
Chd|        SESA30                        source/materials/mat/mat026/sesa30.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|        VISCMAIN                      source/materials/visc/viscmain.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        MAT_ELEM_MOD                  ../common_source/modules/mat_elem/mat_elem_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULAW_MOD                     source/materials/mat_share/mulaw.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE MMAIN(
     1   ELBUF_TAB,   NG,          PM,          GEO,
     2   FBID,        ALE_CONNECT, IX,          IPARG,
     3   V,           TF,          NPF,         BUFMAT,
     4   STIFN,       X,           DT2T,        NELTST,
     5   ITYPTST,     OFFSET,      NEL,         W,
     6   OFF,         PID,         MAT,         NGL,
     7   VOLN,        VD2,         DVOL,        DELTAX,
     8   VIS,         QVIS,        CXX,         S1,
     9   S2,          S3,          S4,          S5,
     A   S6,          DXX,         DYY,         DZZ,
     B   D4,          D5,          D6,          WXX,
     C   WYY,         WZZ,         RX,          RY,
     D   RZ,          SX,          SY,          SZ,
     E   VDX,         VDY,         VDZ,         MUMAX,
     F   SSP_EQ,      AIRE,        SIGY,        ET,
     G   BUFVOIS,     DEFP,        R3_DAM,      R4_AMU,
     H   MFXX,        MFXY,        MFXZ,        MFYX,
     I   MFYY,        MFYZ,        MFZX,        MFZY,
     J   MFZZ,        IPM,         GAMA,        FR_WAV,
     K   DXY,         DYX,         DYZ,         DZY,
     L   DZX,         DXZ,         ISTRAIN,     TEMPEL,
     M   DIE,         IEXPAN,      ILAY,        MSSA,
     N   DMELS,       IPTR,        IPTS,        IPTT,
     O   TABLE,       FVD2,        FDELTAX,     FSSP,
     P   FQVIS,       IPARG1,      IGEO,        CONDE,
     Q   ITASK,       NLOC_DMG,    VARNL,       MAT_ELEM,
     R   H3D_STRAIN,  JPLASOL,     JSPH,        OPT_MTN,
     S   OPT_JCVT,    OPT_ISORTH,  OPT_ISORTHG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE MAT_ELEM_MOD            
      USE NLOCAL_REG_MOD
      USE ALE_CONNECTIVITY_MOD
      USE MESSAGE_MOD
      USE ANIM_MOD
      USE ALE_MOD
      USE MULAW_MOD
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
#include      "com08_c.inc"
#include      "scr17_c.inc"
#include      "scr19_c.inc"
#include      "nsvis_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
#include      "sms_c.inc"
#include      "userlib.inc"
#include      "timeri_c.inc"
#include      "thermal_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, OPTIONAL, INTENT(IN) :: OPT_MTN, OPT_JCVT, OPT_ISORTH,  OPT_ISORTHG
      INTEGER, INTENT(INOUT) :: JSPH
      INTEGER, INTENT(INOUT) :: JPLASOL
      INTEGER, INTENT(IN) :: NEL
      INTEGER IX(*), IPARG(NPARG,NGROUP), NPF(*),PID(*),
     .        MAT(*)  ,NGL(*), IPM(NPROPMI,*),IPARG1(*),IGEO(NPROPGI,*)
      INTEGER NELTST, ITYPTST, OFFSET, ISTRAIN, 
     .        IEXPAN,NG,ILAY,IPTR,IPTS,IPTT,IBIDON1,IBIDON2,IBIDON3 ,
     .        IBIDON4, IADBUF, ITASK,H3D_STRAIN
      my_real DT2T
      my_real
     .   PM(NPROPM,*), GEO(*), FBID(*), V(*), TF(*), VARNL(NEL),
     .   BUFMAT(*), STIFN(*), X(3,*),OFF(*),W(*),SIGY(*) ,ET(*) ,
C-----------  R4_FREE is replaced by R3_AMU (no more free)  
     .   BUFVOIS(*), DEFP(*) ,R3_DAM(*) ,R4_AMU(*),GAMA(MVSIZ,6), 
     .   VD2(*)   ,DVOL(*)  ,DELTAX(*),VIS(*)   ,
     .   QVIS(*)  ,CXX(*)   ,S1(*)    ,S2(*)    ,S3(*)    ,
     .   S4(*)    ,S5(*)    ,S6(*)    ,DXX(*)   ,DYY(*)   ,
     .   DZZ(*)   ,D4(*)    ,D5(*)    ,D6(*)    ,WXX(*)   ,
     .   WYY(*)   ,WZZ(*)   ,RX(*)    ,RY(*)    ,RZ(*)    ,
     .   SX(*)    ,SY(*)    ,SZ(*)    ,VDX(*)   ,VDY(*)   ,
     .   VDZ(*)   ,MUMAX(*) ,SSP_EQ(MVSIZ)      ,AIRE(*)  ,
     .   MFXX(*)  ,MFXY(*)  ,MFXZ(*)  ,MFYX(*)  ,MFYY(*)  ,
     .   MFYZ(*)  ,MFZX(*)  ,MFZY(*)  ,MFZZ(*) ,TEMPEL(*), DIE(*),
     .   MSSA(*), DMELS(*)  ,VOLN(MVSIZ)
      TARGET :: VARNL,DEFP
      my_real
     .   DXY(*) ,DYX(*) ,DYZ(*) ,DZY(*) ,DZX(*) ,DXZ(*),FR_WAV(*),
     .   FQVIS(*),FVD2(*),FSSP(*),FDELTAX(*),CONDE(*)
      TYPE(TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_) :: NLOC_DMG 
      TYPE (t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE (MAT_ELEM_)  ,INTENT(INOUT) :: MAT_ELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IPG,NUVAR,NPARAM,NFUNC,IFUNC_ALPHA,ILAW,
     .   NVARF,NFAIL,IR,IP,IRUPT,IVISC,EOSTYP,NPTS,NPTT,NPTR,NPG,ISVIS,
     .   MX,NVARTMP,NLAY,INLOC
      INTEGER IFUNC(MAXFUNC),ISELECT,IBPRELD,NVAREOS,NVARVIS
      my_real
     .   FACQ0,E1,E2,E3,E4,E5,E6,ALPHA,T0,TM
      my_real
     .   PNEW(MVSIZ),PSH(MVSIZ), P0(MVSIZ), OFFOLD(MVSIZ),
     .   AMU(MVSIZ), AMU2(MVSIZ), C1(MVSIZ), C2(MVSIZ), 
     .   C3(MVSIZ), C4(MVSIZ), C5(MVSIZ),C6(MVSIZ),
     .   EINC(MVSIZ), RHO0(MVSIZ),VOL_AVG(MVSIZ),
     .   DF(MVSIZ), PC(MVSIZ),ESPE(MVSIZ),TMU(MVSIZ),
     .   TX(MVSIZ)    ,TY(MVSIZ)    ,TZ(MVSIZ),
     .   SS1(MVSIZ),SS2(MVSIZ), SS3(MVSIZ),SS4(MVSIZ),SS5(MVSIZ),
     .   SS6(MVSIZ),DPLA(MVSIZ),TSTAR(MVSIZ),EPSP(MVSIZ),XK(MVSIZ),
     .   TEMPEL0(MVSIZ), FSCAL_ALPHA , SIGL(MVSIZ,6),   
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),R21(MVSIZ),
     .   R22(MVSIZ),R23(MVSIZ),R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   ES1(MVSIZ), ES2(MVSIZ),  ES3(MVSIZ),  ES4(MVSIZ),  ES5(MVSIZ),
     .   ES6(MVSIZ), EINT(MVSIZ), DPDM(MVSIZ), DPDE(MVSIZ),ECOLD(MVSIZ),
     .   VOL(MVSIZ),AL_IMP(MVSIZ),SIGNOR(MVSIZ,6),
     .   EP1(MVSIZ),EP2(MVSIZ),EP3(MVSIZ),
     .   EP4(MVSIZ),EP5(MVSIZ),EP6(MVSIZ),
     .   SV1(MVSIZ),SV2(MVSIZ),SV3(MVSIZ),
     .   SV4(MVSIZ),SV5(MVSIZ),SV6(MVSIZ),
     .   BIDON1,BIDON4,BIDON5,
     .   BID1(MVSIZ), BID2(MVSIZ),ETH(MVSIZ),
     .   STOR1, STOR2, STOR3, STOR4, STOR5, STOR6,
     .   SIGKK(MVSIZ), EPSTH(MVSIZ), RHOREF(MVSIZ), RHOSP(MVSIZ),USER_UELR(MVSIZ)
      my_real
     .   ALOGEY(MVSIZ), POLD(MVSIZ),
     .   P01(MVSIZ), P02(MVSIZ), E01(MVSIZ), E02(MVSIZ),
     .   ER1V(MVSIZ), ER2V(MVSIZ), WDR1V(MVSIZ), WDR2V(MVSIZ),W1(MVSIZ),
     .   HEAT_MECA_L,VOLG(MVSIZ),DE1(NEL),DE2(NEL),DE3(NEL),DE4(NEL),
     .   DE5(NEL),DE6(NEL)
      my_real
     .   Q1,Q2,Q3,STR1,STR2,STR3,STR4,STR5,STR6,WXXF,WYYF,WZZF
      my_real, DIMENSION(NEL), TARGET :: LE_MAX
      my_real, DIMENSION(:) ,POINTER :: STRD1,STRD2,EL_LEN,EL_PLA
      LOGICAL :: LOGICAL_USERL_AVAIL, l_MULAW_CALLED
!
      CHARACTER OPTION*256
      INTEGER LENGTH
      TARGET :: DELTAX
C--------------------------------------------    
       my_real, EXTERNAL :: FINTER 
C--------------------------------------------    
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(BUF_MAT_)  ,POINTER :: MBUF  
      TYPE(BUF_FAIL_) ,POINTER :: FBUF
      TYPE(BUF_VISC_) ,POINTER :: VBUF
      TYPE(BUF_EOS_)  ,POINTER :: EBUF
      my_real,
     .  DIMENSION(:), POINTER  :: UVARF,DFMAX,TDEL,DAMINI
c     Bolt Preloading
      INTEGER IBOLTP, NBPRELD
      my_real, 
     .  DIMENSION(:), POINTER  :: BPRELD
      INTEGER                  :: DMG_FLAG
      INTEGER :: LFT,MTN,LLT,NFT,IAD,ITY,NPT,JALE,ISMSTR,
     .   JEUL,JTUR,JTHE,JLAG,MID,JMULT,JHBE,JIVF,JPOR,JPLA,JCLOSE,
     .   IREP,IINT,IGTYP,JCVT,ISROT,ISRAT,ISORTH,ISORTHG,ICSEN,IFAILURE,
     .   JSMS, IHET, IPARTSPH 

C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      LOGICAL_USERL_AVAIL = .FALSE.
      l_MULAW_CALLED=.FALSE.
      IF(USERL_AVAIL/=0) LOGICAL_USERL_AVAIL = .TRUE.



      LFT = 1
      IF(PRESENT(OPT_MTN)) THEN
        MTN     = OPT_MTN 
      ELSE
        MTN     = IPARG(1,NG)    
      ENDIF
      LLT     = IPARG(2,NG)    
      NFT     = IPARG(3,NG)    
      IAD     = IPARG(4,NG)    
      ITY     = IPARG(5,NG)    
      NPT     = IPARG(6,NG)     
      JALE    = IPARG(7,NG)     
      ISMSTR  = IPARG(9,NG)     
      IF(ITY==1.OR.ITY==2)THEN
         JEUL    = IPARG(11,NG) 
      ELSEIF (MTN == 151 .AND. ITY == 7) THEN
         JEUL    = IPARG(11,NG) 
      ELSE             ! attention dkt9_s3 utilise ce flag
         JEUL    = 0
      END IF 
      JTUR    = IPARG(12,NG)    
      JTHE    = IPARG(13,NG)    
      JLAG    = IPARG(14,NG)
      MID     = IPARG(18,NG)    
      JMULT   = IPARG(20,NG)    
      JHBE    = IPARG(23,NG)    
      JIVF    = IPARG(24,NG)          
      JPOR    = IPARG(27,NG)          
      JPLA    = IPARG(29,NG)      
      JCLOSE  = IPARG(33,NG)      
      IREP    = IPARG(35,NG)      
      IINT    = IPARG(36,NG)    
      IHET    = 0
      IF(JHBE == 24) IHET = IINT

      IF(PRESENT(OPT_JCVT)) THEN
        JCVT  = OPT_JCVT
      ELSE
        JCVT  = IPARG(37,NG)
      ENDIF
      IGTYP   = IPARG(38,NG)
      ICSEN   = IPARG(39,NG)
      ISRAT   = IPARG(40,NG)      
      ISROT   = IPARG(41,NG)       
      IF(IPARG(28,NG) == 10) ISROT = IPARG(74,NG)     
      ISORTH  = IPARG(42,NG)      
      ISORTHG = ISORTH
      IF(PRESENT(OPT_ISORTH)) ISORTH = OPT_ISORTH
      IF(PRESENT(OPT_ISORTHG)) ISORTHG = OPT_ISORTHG

      IFAILURE = IPARG(43,NG)                  
      JSMS = IPARG(52,NG)
      IPARTSPH = IPARG(69,NG)



C compatibility with deprecated law 20
      IF(ILAY < 0) THEN
        JMULT = - ILAY
        ILAY =  - ILAY
        IF(JMULT == 1) THEN
          MTN = IPARG(25,NG)
        ELSE
          MTN = IPARG(26,NG)
        ENDIF
      ENDIF




      GBUF => ELBUF_TAB(NG)%GBUF
      LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IPTR,IPTS,IPTT)
      MBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%MAT(IPTR,IPTS,IPTT)
      FBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%FAIL(IPTR,IPTS,IPTT)
      VBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%VISC(IPTR,IPTS,IPTT)
      EBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%EOS(IPTR,IPTS,IPTT)
      ILAW  = ELBUF_TAB(NG)%BUFLY(ILAY)%ILAW
      NFAIL = ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL
      IVISC = ELBUF_TAB(NG)%BUFLY(ILAY)%IVISC
      NPTR  = ELBUF_TAB(NG)%NPTR
      NPTS  = ELBUF_TAB(NG)%NPTS
      NPTT  = ELBUF_TAB(NG)%NPTT
      NLAY  = ELBUF_TAB(NG)%NLAY
      NPG   = NPTS*NPTT*NPTR
      NVAREOS = ELBUF_TAB(NG)%BUFLY(ILAY)%NVAR_EOS 
      NVARVIS = ELBUF_TAB(NG)%BUFLY(ILAY)%NVAR_VISC
      IPG   = IPTR + ((IPTS-1) + (IPTT-1)*NPTS)*NPTR

      DMG_FLAG = ELBUF_TAB(NG)%BUFLY(ILAY)%L_DMGSCL
      IF ((IGTYP == 20).OR.(IGTYP == 21).OR.(IGTYP == 22)) THEN
        IPG  = IPTR + ((IPTS-1) + (ILAY-1)*NPTS)*NPTR
        NPG  = NPTR*NPTS*NLAY
      ENDIF
C
      IBOLTP = IPARG(72,NG)
      NBPRELD = GBUF%G_BPRELD
      BPRELD => GBUF%BPRELD(1:NBPRELD*NEL)
c--------------------------------------------------------
c     COMPUTE UNDAMAGED EFFECTIVE STRESSES
c---------------------------------------------------------
      ! -> Isotropic stress softening
      IF ((DMG_FLAG == 1).AND.(MTN < 28 .OR. MTN == 49)) THEN 
        DO I = 1,NEL
          LBUF%SIG(NEL*(1-1) + I) = 
     .       LBUF%SIG(NEL*(1-1) + I)/MAX(LBUF%DMGSCL(I),EM20)
          LBUF%SIG(NEL*(2-1) + I) = 
     .       LBUF%SIG(NEL*(2-1) + I)/MAX(LBUF%DMGSCL(I),EM20)
          LBUF%SIG(NEL*(3-1) + I) = 
     .       LBUF%SIG(NEL*(3-1) + I)/MAX(LBUF%DMGSCL(I),EM20)
          LBUF%SIG(NEL*(4-1) + I) = 
     .       LBUF%SIG(NEL*(4-1) + I)/MAX(LBUF%DMGSCL(I),EM20)
          LBUF%SIG(NEL*(5-1) + I) = 
     .       LBUF%SIG(NEL*(5-1) + I)/MAX(LBUF%DMGSCL(I),EM20)
          LBUF%SIG(NEL*(6-1) + I) = 
     .       LBUF%SIG(NEL*(6-1) + I)/MAX(LBUF%DMGSCL(I),EM20)
        ENDDO
      ! -> Orthotropic stress softening
      ELSEIF ((DMG_FLAG == 6).AND.(MTN < 28 .OR. MTN == 49)) THEN 
        DO I = 1,NEL
          LBUF%SIG(NEL*(1-1) + I) = 
     .       LBUF%SIG(NEL*(1-1) + I)/MAX(LBUF%DMGSCL(NEL*(1-1) + I),EM20)
          LBUF%SIG(NEL*(2-1) + I) = 
     .       LBUF%SIG(NEL*(2-1) + I)/MAX(LBUF%DMGSCL(NEL*(2-1) + I),EM20)
          LBUF%SIG(NEL*(3-1) + I) = 
     .       LBUF%SIG(NEL*(3-1) + I)/MAX(LBUF%DMGSCL(NEL*(3-1) + I),EM20)
          LBUF%SIG(NEL*(4-1) + I) = 
     .       LBUF%SIG(NEL*(4-1) + I)/MAX(LBUF%DMGSCL(NEL*(4-1) + I),EM20)
          LBUF%SIG(NEL*(5-1) + I) = 
     .       LBUF%SIG(NEL*(5-1) + I)/MAX(LBUF%DMGSCL(NEL*(5-1) + I),EM20)
          LBUF%SIG(NEL*(6-1) + I) = 
     .       LBUF%SIG(NEL*(6-1) + I)/MAX(LBUF%DMGSCL(NEL*(6-1) + I),EM20)
        ENDDO
      ENDIF
C-----

C Flag Idel
      IF(IDEL7NG>=1.OR.IDTMINS>=1) THEN
        OFFOLD(1:NEL)=OFF(1:NEL)
      END IF
C
      IF (JTHE < 0) THEN
           DIE(1:NEL) = LBUF%EINT(1:NEL)
           DPLA(1:NEL) = ZERO
      ELSE
          TSTAR(1:NEL) = ZERO
          DPLA(1:NEL) = ZERO
      ENDIF        
C tangent module ratio (often w.r.t. G )
      ET(1:NEL) = ONE
C----   ISELECT>0 : modified LEFT CAUCHY-GREEN  STRAIN  saved in ETOTSH 
      ISELECT=0
      IF(ISMSTR >= 10.AND.ISMSTR <= 12) THEN
C------ IPARG(10,NG): ICPRE      
        IF (JHBE==17.AND.IINT==2.AND.JCVT/=0.AND.IPARG(10,NG)/=1) ISELECT=1
      END IF
C------For hyper-elastic laws, due to ET=MAX(ET, initialise to zero      
      IF (IMPL_S > 0 .OR. IHET > 1) THEN
        IF (MTN == 42 .OR. MTN == 62 .OR. MTN == 69 .OR. 
     .      MTN == 82 .OR. MTN == 88 .OR. MTN == 92 .OR.
     .      MTN == 94 .OR. MTN == 111 ) THEN
              ET(1:NEL)=ZERO
         END IF
      END IF
C needed in mqviscb
      FACQ0 =  ONE
      ISVIS = IGEO(35,PID(1))
      IF((IMPL_S > 0 .AND. IDYNA == 0).OR. MTN == 24) ISVIS = 0
C                  
      RHO0(1:NEL)  = PM(1,MAT(1))
      SVIS(1:NEL,1)= ZERO
      SVIS(1:NEL,2)= ZERO
      SVIS(1:NEL,3)= ZERO
      SVIS(1:NEL,4)= ZERO
      SVIS(1:NEL,5)= ZERO
      SVIS(1:NEL,6)= ZERO 
C
      IF(IPARG(64,NG)/=0)THEN
        DF(1:NEL)      = ONE
        VOL_AVG(1:NEL) = VOLN(1:NEL)
      ELSE
          DF(1:NEL)  =  RHO0(1:NEL)/LBUF%RHO(1:NEL)
          VOL_AVG(1:NEL) = VOLN(1:NEL) - HALF*DVOL(1:NEL)
      ENDIF
C-----due to too much returns of QA, temporarily add IRESP here
      IF((ALE%GLOBAL%INCOMP/=1.OR.(JEUL+JALE)/=1).AND.JLAG/=0.AND.N2D==0.AND.JSPH==0.AND.IRESP==1
     .     .AND.IMPL_S==0.AND.ISMSTR/=1.AND.ISMSTR/=3.AND.ISMSTR/=11)THEN
C-----due to issue w/ SP ----in srho3.F R4_AMU is calculated for SP :      
C--    R4_AMU =VOL0DP/VOLDP -1   because RHO*VOLDP=RHO0*VOL0DP   
          AMU(1:NEL) =  R4_AMU(1:NEL)
      ELSE
       IF(MTN == 1 .OR. MTN == 2 .OR. MTN == 22.OR.(MTN >=28.AND.MTN /=45)) THEN     ! for compatibility with QA tests
          AMU(1:NEL) =  LBUF%RHO(1:NEL)/RHO0(1:NEL)-ONE
       ELSE
          AMU(1:NEL) =  ONE/DF(1:NEL)-ONE
       END IF 
       IF(ISMSTR==12)THEN
        DO I=1,NEL
          IF(ABS(LBUF%OFF(I))>ONE) AMU(I) =  R4_AMU(I)
        END DO
       END IF
      ENDIF
          
C --------------------------------------------------------
C
C     RHOREF is corresponding to VOL = Volume for Integration
C        - RHOREF=RHO  for large strain
C        - RHOREF=RHO0*VOL0/VOL for small strain 
C        In any case: d(RHO)/dt=-RHOREF Trace(Epsilon_dot)
C
C     RHOSP must be returned by the material law, as well as SSP
C     It is used ONLY for time step computation wrt viscosity(*) in mqviscb.F
C        (*) wrt Artificial viscosity and Navier Stokes viscosity
C
C     Case of hydrodynamic pressure :
C        !-----------------------
C        !     P = C1 mu, mu = rho/rho0-1, d(rho)/d(mu)=rho0
C        !     ssp^2 = dP/drho = C1/rho0, rhosp = rho0
C        !-----------------------
C     Case of small strain  :
C        !-----------------------
C        !     P = C1 mu, mu = Trace(Epsilon), d(rho)/d(mu)rhoref
C        !     ssp^2 = dP/drho = C1/rhoref, rhosp = rhoref
C        !-----------------------
C     Case of Large strain :
C        !-----------------------
C         !     P = C1 mu, mu = Trace(Epsilon) = ln(rho/rho0), rho= rho0 exp(mu), d(rho)/d(mu)=rho
C         !     ssp^2 = dP/drho = C1/rho, rhosp = rho
C         !-----------------------
C     If is not returned by the material law, RHOSP = RHO insures backward compatibility.
C --------------------------------------------------------
      IF(ISMSTR==1 .OR. ISMSTR==11)THEN
        RHOREF(1:NEL)=RHO0(1:NEL)
      ELSEIF(ISMSTR==2 .OR. ISMSTR==12)THEN
        DO I=1,NEL
          IF(ABS(LBUF%OFF(I))<=ONE)THEN
            RHOREF(I)=LBUF%RHO(I)
          ELSE
            RHOREF(I)=RHO0(I)*LBUF%VOL(I)/MAX(EM20,VOLN(I))
            IF (ISMSTR==12) AMU(I) =  R4_AMU(I)
          END IF
        END DO
      ELSE ! Large Strain, ISMSTR==4 .OR. ISMSTR==10
        RHOREF(1:NEL)=LBUF%RHO(1:NEL)
      END IF
      RHOSP(1:NEL) =LBUF%RHO(1:NEL)
C --------------------------------------------------------
C    THERMAL MATERIAL ISTROPE EXPANSION 
C --------------------------------------------------------

      IF((IEXPAN > 0).AND.(JTHE < 0).AND.(TT/=ZERO)) THEN
         IF(ISMSTR==4) THEN
           AMU(1:NEL)  = AMU(1:NEL) /(AMU(1:NEL) + ONE) ! to get amu = 1- rho0/rho
        ENDIF
        IF(ISMSTR==2) THEN
          DO I=1,NEL
            IF(ABS(LBUF%OFF(I))<=ONE)THEN
              AMU(I)  = AMU(I) /(AMU(I) + ONE)
            END IF
          ENDDO 
        END IF

        DO I=1,NEL
           IFUNC_ALPHA = IPM(219,MAT(1))
           FSCAL_ALPHA = PM(191,MAT(1))
           TEMPEL0(I)  = LBUF%TEMP(I)
           ALPHA = FINTER(IFUNC_ALPHA,TEMPEL(I),NPF,TF,BIDON1)
           ALPHA = ALPHA * FSCAL_ALPHA
           ETH(I)= ALPHA *(TEMPEL(I)-TEMPEL0(I))*OFF(I) 
           LBUF%FORTH(I) = LBUF%FORTH(I) + ETH(I)  ! LBUF%FORTH the total thermal strain over time
           EPSTH(I)= THREE*LBUF%FORTH(I)
           DXX(I)  = DXX(I)-ETH(I)/DT1
           DYY(I)  = DYY(I)-ETH(I)/DT1
           DZZ(I)  = DZZ(I)-ETH(I)/DT1
           DVOL(I) = DVOL(I)-THREE*ETH(I)*VOL_AVG(I)
           AMU(I)  = AMU(I) + EPSTH(I)
           SIGKK(I)= LBUF%SIG(NEL*(1-1)+I)+LBUF%SIG(NEL*(2-1)+I)+LBUF%SIG(NEL*(3-1)+I)
           LBUF%EINTTH(I) = LBUF%EINTTH(I)-HALF*SIGKK(I)*ETH(I)
        ENDDO
      ELSE
        EPSTH(1:NEL)= ZERO
      ENDIF     
c-----Tstar computation for Jonhson Cook failure : T* = (T-T0)/(TM-T0)----
      IF (JTHE < 0) THEN
         MX = MAT(1)
         T0 = PM(79, MX) 
         TM = PM(80, MX) 
         DO I=1,NEL
           TSTAR(I)=MAX(ZERO,(TEMPEL(I)-T0)/(TM-T0))
         ENDDO
      ENDIF 
C-----------------------------------
C     EOS PART1
C-----------------------------------
      IF (MTN == 3.OR.MTN == 4.OR.MTN == 6.OR.MTN==10.OR.MTN==17.OR.MTN == 49.OR.MTN == 102.OR.MTN == 103) THEN
        EOSTYP= IPM(4,MAT(1))
        CALL EOSMAIN(0       ,NEL      ,EOSTYP  ,PM        ,OFF      ,LBUF%EINT,
     2               LBUF%RHO,RHO0     ,AMU     ,AMU2      ,ESPE     ,
     3               DVOL    ,DF       ,VOLN    ,MAT       ,PSH      ,
     4               PNEW    ,DPDM     ,DPDE    ,LBUF%TEMP ,ECOLD    ,
     5               BUFMAT  ,LBUF%SIG ,LBUF%MU ,MTN       ,POLD     ,
     6               NPF     ,TF       ,EBUF%VAR,NVAREOS)
      ENDIF
C-----------------------------------
C     STRESSES DEVIATORIC/TOTAL
C-----------------------------------
      IBPRELD = 0
      IF(IBOLTP /= 0)THEN
        !two conditions checks, otherwise BPRELD has size 0 if IBOLTP==0 and debugger stops as it detects an index out of bounds.
        IF(BPRELD(2*NEL+1) <= ONE .AND.BPRELD(2*NEL+1) > ZERO)IBPRELD = 1
      ENDIF

      IF(IBOLTP /= 0 .AND. IBPRELD == 1)THEN       
       CALL SBOLTLAW(
     1   PM,         LBUF%SIG,   MAT,        DXX,
     2   DYY,        DZZ,        D4,         D5,
     3   D6,         NEL,        LBUF%RHO,   BPRELD,
     4   LBUF%EINT,  LBUF%QVIS,  LBUF%VOL,   STIFN,
     5   DT2T,       NELTST,     ITYPTST,    LBUF%OFF,
     6   GEO,        PID,        MUMAX,      NGL,
     7   CXX,        DVOL,       AIRE,       VOLN,
     8   VD2,        DELTAX,     VIS,        PNEW,
     9   PSH,        QVIS,       SSP_EQ,     S1,
     A   S2,         S3,         S4,         S5,
     B   S6,         MSSA,       DMELS,      CONDE,
     C   AMU,        VOL_AVG,    GBUF%DT,    GBUF%G_DT,
     D   OFF,        IPM,        RHOREF,     RHOSP,
     E   LBUF%VOL0DP,ISMSTR,     JSPH,       JTUR,
     F   ITY,        JTHE,       JSMS,       NPG )
      ELSE IF (MTN == 1) THEN
C
        IF (JHBE==17.AND.IINT==3.AND.ISMSTR == 1) THEN
          CALL M1LAWI(
     1   PM,       OFF,      LBUF%SIG, LBUF%EINT,
     2   LBUF%RHO, LBUF%QVIS,LBUF%VOL, STIFN,
     3   DT2T,     NELTST,   ITYPTST,  LBUF%OFF,
     4   GEO,      PID,      MUMAX,    MAT,
     5   NGL,      CXX,      DVOL,     AIRE,
     6   VOLN,     VD2,      DELTAX,   VIS,
     7   DXX,      DYY,      DZZ,      D4,
     8   D5,       D6,       PNEW,     PSH,
     9   QVIS,     SSP_EQ,   S1,       S2,
     A   S3,       S4,       S5,       S6,
     B   MSSA,     DMELS,    CONDE,    AMU,
     C   VOL_AVG,  GBUF%DT,  GBUF%G_DT,NEL,
     D   IPM,      RHOREF,   RHOSP,    ITY,
     E   JTUR,     JTHE,     JSPH,     ISMSTR,
     F   JSMS,     NPG )
        ELSE IF(ISMSTR >= 10.AND.ISMSTR <= 12)THEN
          CALL M1LAWTOT(
     1   PM,         OFF,        LBUF%SIG,   LBUF%EINT,
     2   LBUF%RHO,   LBUF%QVIS,  LBUF%VOL,   STIFN,
     3   DT2T,       NELTST,     ITYPTST,    LBUF%OFF,
     4   GEO,        PID,        AMU,        MUMAX,
     5   MAT,        NGL,        CXX,        DVOL,
     6   AIRE,       VOLN,       VD2,        DELTAX,
     7   VIS,        DXX,        DYY,        DZZ,
     8   D4,         D5,         D6,         PNEW,
     9   PSH,        QVIS,       SSP_EQ,     S1,
     A   S2,         S3,         S4,         S5,
     B   S6,         MSSA,       DMELS,      CONDE,
     C   MFXX,       MFXY,       MFXZ,       MFYX,
     D   MFYY,       MFYZ,       MFZX,       MFZY,
     E   MFZZ,       GBUF%OFF,   VOL_AVG,    EPSTH,
     F   GBUF%DT,    GBUF%G_DT,  NEL,        GBUF%ETOTSH,
     G   ISELECT,    IPM,        RHOREF,     RHOSP,
     H   LBUF%SIGL,  ITY,        ISMSTR,     JTUR,
     I   JTHE,       JCVT,       JSPH,       JSMS,
     J   NPG )
        ELSE 
          CALL M1LAW(
     1   PM,       OFF,      LBUF%SIG, LBUF%EINT,
     2   LBUF%RHO, LBUF%QVIS,LBUF%VOL, STIFN,
     3   DT2T,     NELTST,   ITYPTST,  LBUF%OFF,
     4   GEO,      PID,      MUMAX,    MAT,
     5   NGL,      CXX,      DVOL,     AIRE,
     6   VOLN,     VD2,      DELTAX,   VIS,
     7   DXX,      DYY,      DZZ,      D4,
     8   D5,       D6,       PNEW,     PSH,
     9   QVIS,     SSP_EQ,   S1,       S2,
     A   S3,       S4,       S5,       S6,
     B   MSSA,     DMELS,    CONDE,    AMU,
     C   VOL_AVG,  GBUF%DT,  GBUF%G_DT,NEL,
     D   IPM,      RHOREF,   RHOSP,    ITY,
     E   JTUR,     JTHE,     JSPH,     ISMSTR,
     F   JSMS,     NPG )
        END IF
C
      ELSEIF (MTN == 2) THEN
C
        CALL M2LAW(
     1   PM,       OFF,      LBUF%SIG, LBUF%EINT,
     2   LBUF%RHO, LBUF%QVIS,LBUF%PLA, LBUF%EPSD,
     3   LBUF%VOL, STIFN,    DT2T,     NELTST,
     4   ITYPTST,  LBUF%OFF, GEO,      PID,
     5   AMU,      VOL_AVG,  MUMAX,    MAT,
     6   NGL,      CXX,      DVOL,     AIRE,
     7   VOLN,     VD2,      DELTAX,   VIS,
     8   DXX,      DYY,      DZZ,      D4,
     9   D5,       D6,       PNEW,     PSH,
     A   QVIS,     SSP_EQ,   S1,       S2,
     B   S3,       S4,       S5,       S6,
     C   JPLASOL,  SIGY,     DEFP,     DPLA,
     D   EPSP,     TSTAR,    ET,       MSSA,
     E   DMELS,    TEMPEL,   LBUF%SIGB,AL_IMP,
     F   SIGNOR,   CONDE,    GBUF%DT,  GBUF%G_DT,
     G   NEL,      IPM,      RHOREF,   RHOSP,
     H   IPG,      LBUF%DMG, ITY,      JTUR,
     I   JTHE,     JSPH,     ISMSTR,   JSMS,
     J   LBUF%EPSQ,NPG )
!----------------
        IF (ISTRAIN > 0 .AND.
     .    (H3D_STRAIN == 1 .OR. TH_STRAIN == 1 )) THEN
!
        ! strain rate
        DO I=1,NEL
          EP1(I) = DXX(I)*OFF(I)
          EP2(I) = DYY(I)*OFF(I)
          EP3(I) = DZZ(I)*OFF(I)
          EP4(I) = D4(I) *OFF(I)
          EP5(I) = D5(I) *OFF(I)
          EP6(I) = D6(I) *OFF(I)
        ENDDO
        ! strain
        IF (JCVT > 0) THEN
          DO I=1,NEL
            ES1(I) = EP1(I)*DT1
            ES2(I) = EP2(I)*DT1
            ES3(I) = EP3(I)*DT1
            ES4(I) = EP4(I)*DT1
            ES5(I) = EP5(I)*DT1
            ES6(I) = EP6(I)*DT1
          ENDDO
        ELSEIF (ISORTH /= 0) THEN
C---------------------------
C       ORTHOTROPE GLOBAL
C---------------------------
          CALL MREPLOC(
     1   GAMA,    R11,     R12,     R13,
     2   R21,     R22,     R23,     R31,
     3   R32,     R33,     RX,      RY,
     4   RZ,      SX,      SY,      SZ,
     5   TX,      TY,      TZ,      NEL,
     6   JSPH)
          DO I=1,NEL
            EP4(I) = HALF*EP4(I)
            EP5(I) = HALF*EP5(I)
            EP6(I) = HALF*EP6(I)
          ENDDO
          CALL MROTENS(1,NEL,EP1,EP2,EP3,EP4,EP5,EP6,
     .         R11,R21,R31,R12,R22,R32,R13,R23,R33)
          DO I=1,NEL
            EP4(I) = TWO*EP4(I)
            EP5(I) = TWO*EP5(I)
            EP6(I) = TWO*EP6(I)
          ENDDO
          DO I=1,NEL
            ES1(I) = EP1(I)*DT1
            ES2(I) = EP2(I)*DT1
            ES3(I) = EP3(I)*DT1
            ES4(I) = EP4(I)*DT1
            ES5(I) = EP5(I)*DT1
            ES6(I) = EP6(I)*DT1
          ENDDO
        ELSE
C---------------------------
C       ISOTROPE GLOBAL
C---------------------------
#include "vectorize.inc"
          DO I=1,NEL
            ES1(I) = EP1(I)*DT1
            ES2(I) = EP2(I)*DT1
            ES3(I) = EP3(I)*DT1
            ES4(I) = EP4(I)*DT1
            ES5(I) = EP5(I)*DT1
            ES6(I) = EP6(I)*DT1
          ENDDO
#include "vectorize.inc"
          DO I=1,NEL
            WXXF = WXX(I)*OFF(I)
            WYYF = WYY(I)*OFF(I)
            WZZF = WZZ(I)*OFF(I)
            Q1 = LBUF%STRA(NEL*(4-1) + I)*WZZF
            Q2 = LBUF%STRA(NEL*(6-1) + I)*WYYF
            Q3 = LBUF%STRA(NEL*(5-1) + I)*WXXF
            STR1 = LBUF%STRA(NEL*(1-1) + I)-Q1+Q2
            STR2 = LBUF%STRA(NEL*(2-1) + I)+Q1-Q3
            STR3 = LBUF%STRA(NEL*(3-1) + I)-Q2+Q3
            STR4 = LBUF%STRA(NEL*(4-1) + I)+
     .             TWO*WZZF*(LBUF%STRA(NEL*(1-1) + I)-LBUF%STRA(NEL*(2-1) + I))+
     .             WYYF*LBUF%STRA(NEL*(5-1) + I)-WXXF*LBUF%STRA(NEL*(6-1) + I)
            STR5 = LBUF%STRA(NEL*(5-1) + I)+
     .             TWO*WXXF*(LBUF%STRA(NEL*(2-1) + I)-LBUF%STRA(NEL*(3-1) + I))+
     .             WZZF*LBUF%STRA(NEL*(6-1) + I)-WYYF*LBUF%STRA(NEL*(4-1) + I)
            STR6 = LBUF%STRA(NEL*(6-1) + I)+
     .             TWO*WYYF*(LBUF%STRA(NEL*(3-1) + I)-LBUF%STRA(NEL*(1-1) + I))+
     .             WXXF*LBUF%STRA(NEL*(4-1) + I)-WZZF*LBUF%STRA(NEL*(5-1) + I)
            LBUF%STRA(NEL*(1-1) + I) = STR1
            LBUF%STRA(NEL*(2-1) + I) = STR2
            LBUF%STRA(NEL*(3-1) + I) = STR3
            LBUF%STRA(NEL*(4-1) + I) = STR4
            LBUF%STRA(NEL*(5-1) + I) = STR5
            LBUF%STRA(NEL*(6-1) + I) = STR6
          ENDDO
        ENDIF ! IF (JCVT > 0)
        ! strain update
        DO I=1,NEL
          LBUF%STRA(NEL*(1-1) + I) = LBUF%STRA(NEL*(1-1) + I) + ES1(I)
          LBUF%STRA(NEL*(2-1) + I) = LBUF%STRA(NEL*(2-1) + I) + ES2(I)
          LBUF%STRA(NEL*(3-1) + I) = LBUF%STRA(NEL*(3-1) + I) + ES3(I)
          LBUF%STRA(NEL*(4-1) + I) = LBUF%STRA(NEL*(4-1) + I) + ES4(I)
          LBUF%STRA(NEL*(5-1) + I) = LBUF%STRA(NEL*(5-1) + I) + ES5(I)
          LBUF%STRA(NEL*(6-1) + I) = LBUF%STRA(NEL*(6-1) + I) + ES6(I)
        ENDDO
       ENDIF ! IF (ISTRAIN > 0
!----------------
      ELSEIF (MTN == 3) THEN
        CALL M3LAW(
     1   PM,      OFF,     LBUF%SIG,LBUF%PLA,
     2   MAT,     NGL,     CXX,     DXX,
     3   DYY,     DZZ,     D4,      D5,
     4   D6,      RHO0,    DPDM,    JPLASOL,
     5   SIGY,    DEFP,    DPLA,    NEL,
     6   NFT)
        IF (JSPH == 0) THEN
          CALL MQVISCB(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,CXX,      LBUF%RE,  STIFN,
     3   DT2T,     NELTST,   ITYPTST,  AIRE,
     4   LBUF%OFF, GEO,      PID,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      PNEW,     PSH,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   LBUF%VOL, MSSA,     DMELS,    IGEO,
     9   FACQ0,    CONDE,    GBUF%DT,  GBUF%G_DT,
     A   IPM,      RHOREF,   RHOSP,    NEL,
     B   ITY,      ISMSTR,   JTUR,     JTHE,
     C   JSMS,     NPG )
        ELSE
          CALL MDTSPH(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,LBUF%RE,  STIFN,    DT2T,
     3   NELTST,   ITYPTST,  LBUF%OFF, GEO,
     4   PID,      MUMAX,    CXX,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      PNEW,     PSH,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   GBUF%G_DT,GBUF%DT,  NEL,      ITY,
     9   JTUR,     JTHE)
        ENDIF
        CALL MEINT(
     1   OFF,      LBUF%SIG, LBUF%QVIS,LBUF%EINT,
     2   VOLN,     ESPE,     S1,       S2,
     3   S3,       S4,       S5,       S6,
     4   DXX,      DYY,      DZZ,      D4,
     5   D5,       D6,       PSH,      DVOL,
     6   DF,       QVIS,     PNEW,     VIS,
     7   TMU,      EINC,     MTN,      VOL_AVG,
     8   NEL,      JTUR,     JLAG,     JPOR)
C
      ELSEIF (MTN == 4) THEN
        CALL M4LAW(
     1   PM,       OFF,      LBUF%SIG, LBUF%PLA,
     2   MAT,      LBUF%TEMP,CXX,      DXX,
     3   DYY,      DZZ,      D4,       D5,
     4   D6,       RHO0,     DPDM,     LBUF%EPSD,
     5   JPLASOL,  SIGY,     DEFP,     DPLA,
     6   EPSP,     TSTAR,    TEMPEL,   NEL,
     7   JTHE)
        IF (JSPH == 0) THEN
          CALL MQVISCB(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,CXX,      LBUF%RE,  STIFN,
     3   DT2T,     NELTST,   ITYPTST,  AIRE,
     4   LBUF%OFF, GEO,      PID,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      PNEW,     PSH,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   LBUF%VOL, MSSA,     DMELS,    IGEO,
     9   FACQ0,    CONDE,    GBUF%DT,  GBUF%G_DT,
     A   IPM,      RHOREF,   RHOSP,    NEL,
     B   ITY,      ISMSTR,   JTUR,     JTHE,
     C   JSMS,     NPG )
        ELSE
          CALL MDTSPH(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,LBUF%RE,  STIFN,    DT2T,
     3   NELTST,   ITYPTST,  LBUF%OFF, GEO,
     4   PID,      MUMAX,    CXX,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      PNEW,     PSH,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   GBUF%G_DT,GBUF%DT,  NEL,      ITY,
     9   JTUR,     JTHE)
        ENDIF
        CALL MEINT(
     1   OFF,      LBUF%SIG, LBUF%QVIS,LBUF%EINT,
     2   VOLN,     ESPE,     S1,       S2,
     3   S3,       S4,       S5,       S6,
     4   DXX,      DYY,      DZZ,      D4,
     5   D5,       D6,       PSH,      DVOL,
     6   DF,       QVIS,     PNEW,     VIS,
     7   TMU,      EINC,     MTN,      VOL_AVG,
     8   NEL,      JTUR,     JLAG,     JPOR)
!----------------
        IF (ISTRAIN > 0 .AND.
     .    (H3D_STRAIN == 1 .OR. TH_STRAIN == 1 )) THEN
!
        ! strain rate
        DO I=1,NEL
          EP1(I) = DXX(I)*OFF(I)
          EP2(I) = DYY(I)*OFF(I)
          EP3(I) = DZZ(I)*OFF(I)
          EP4(I) = D4(I) *OFF(I)
          EP5(I) = D5(I) *OFF(I)
          EP6(I) = D6(I) *OFF(I)
        ENDDO
        ! strain
        IF (JCVT > 0) THEN
          DO I=1,NEL
            ES1(I) = EP1(I)*DT1
            ES2(I) = EP2(I)*DT1
            ES3(I) = EP3(I)*DT1
            ES4(I) = EP4(I)*DT1
            ES5(I) = EP5(I)*DT1
            ES6(I) = EP6(I)*DT1
          ENDDO
        ELSEIF (ISORTH /= 0) THEN
C---------------------------
C       ORTHOTROPE GLOBAL
C---------------------------
          CALL MREPLOC(
     1   GAMA,    R11,     R12,     R13,
     2   R21,     R22,     R23,     R31,
     3   R32,     R33,     RX,      RY,
     4   RZ,      SX,      SY,      SZ,
     5   TX,      TY,      TZ,      NEL,
     6   JSPH)
          DO I=1,NEL
            EP4(I) = HALF*EP4(I)
            EP5(I) = HALF*EP5(I)
            EP6(I) = HALF*EP6(I)
          ENDDO
          CALL MROTENS(1,NEL,EP1,EP2,EP3,EP4,EP5,EP6,
     .         R11,R21,R31,R12,R22,R32,R13,R23,R33)
          DO I=1,NEL
            EP4(I) = TWO*EP4(I)
            EP5(I) = TWO*EP5(I)
            EP6(I) = TWO*EP6(I)
          ENDDO
          DO I=1,NEL
            ES1(I) = EP1(I)*DT1
            ES2(I) = EP2(I)*DT1
            ES3(I) = EP3(I)*DT1
            ES4(I) = EP4(I)*DT1
            ES5(I) = EP5(I)*DT1
            ES6(I) = EP6(I)*DT1
          ENDDO
        ELSE
C---------------------------
C       ISOTROPE GLOBAL
C---------------------------
#include "vectorize.inc"
          DO I=1,NEL
            ES1(I) = EP1(I)*DT1
            ES2(I) = EP2(I)*DT1
            ES3(I) = EP3(I)*DT1
            ES4(I) = EP4(I)*DT1
            ES5(I) = EP5(I)*DT1
            ES6(I) = EP6(I)*DT1
          ENDDO
#include "vectorize.inc"
          DO I=1,NEL
            WXXF = WXX(I)*OFF(I)
            WYYF = WYY(I)*OFF(I)
            WZZF = WZZ(I)*OFF(I)
            Q1 = LBUF%STRA(NEL*(4-1) + I)*WZZF
            Q2 = LBUF%STRA(NEL*(6-1) + I)*WYYF
            Q3 = LBUF%STRA(NEL*(5-1) + I)*WXXF
            STR1 = LBUF%STRA(NEL*(1-1) + I)-Q1+Q2
            STR2 = LBUF%STRA(NEL*(2-1) + I)+Q1-Q3
            STR3 = LBUF%STRA(NEL*(3-1) + I)-Q2+Q3
            STR4 = LBUF%STRA(NEL*(4-1) + I)+
     .             TWO*WZZF*(LBUF%STRA(NEL*(1-1) + I)-LBUF%STRA(NEL*(2-1) + I))+
     .             WYYF*LBUF%STRA(NEL*(5-1) + I)-WXXF*LBUF%STRA(NEL*(6-1) + I)
            STR5 = LBUF%STRA(NEL*(5-1) + I)+
     .             TWO*WXXF*(LBUF%STRA(NEL*(2-1) + I)-LBUF%STRA(NEL*(3-1) + I))+
     .             WZZF*LBUF%STRA(NEL*(6-1) + I)-WYYF*LBUF%STRA(NEL*(4-1) + I)
            STR6 = LBUF%STRA(NEL*(6-1) + I)+
     .             TWO*WYYF*(LBUF%STRA(NEL*(3-1) + I)-LBUF%STRA(NEL*(1-1) + I))+
     .             WXXF*LBUF%STRA(NEL*(4-1) + I)-WZZF*LBUF%STRA(NEL*(5-1) + I)
            LBUF%STRA(NEL*(1-1) + I) = STR1
            LBUF%STRA(NEL*(2-1) + I) = STR2
            LBUF%STRA(NEL*(3-1) + I) = STR3
            LBUF%STRA(NEL*(4-1) + I) = STR4
            LBUF%STRA(NEL*(5-1) + I) = STR5
            LBUF%STRA(NEL*(6-1) + I) = STR6
          ENDDO
        ENDIF ! IF (JCVT > 0)
        ! strain update
        DO I=1,NEL
          LBUF%STRA(NEL*(1-1) + I) = LBUF%STRA(NEL*(1-1) + I) + ES1(I)
          LBUF%STRA(NEL*(2-1) + I) = LBUF%STRA(NEL*(2-1) + I) + ES2(I)
          LBUF%STRA(NEL*(3-1) + I) = LBUF%STRA(NEL*(3-1) + I) + ES3(I)
          LBUF%STRA(NEL*(4-1) + I) = LBUF%STRA(NEL*(4-1) + I) + ES4(I)
          LBUF%STRA(NEL*(5-1) + I) = LBUF%STRA(NEL*(5-1) + I) + ES5(I)
          LBUF%STRA(NEL*(6-1) + I) = LBUF%STRA(NEL*(6-1) + I) + ES6(I)
        ENDDO
       ENDIF ! IF (ISTRAIN > 0
!----------------
C
      ELSEIF (MTN == 5) THEN
        CALL M5LAW(PM   ,LBUF%SIG ,LBUF%EINT  ,LBUF%RHO ,PSH       ,       
     1             P0   ,LBUF%TB  ,LBUF%BFRAC ,VOLN     ,DELTAX    ,       
     2             MAT  ,NEL      ,CXX        ,DF       ,                  
     3             ER1V ,ER2V     ,WDR1V      ,WDR2V    ,W1        ,       
     4             RHO0, AMU)
        IF (JSPH == 0) THEN
          CALL MQVISCB(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,CXX,      LBUF%RE,  STIFN,
     3   DT2T,     NELTST,   ITYPTST,  AIRE,
     4   LBUF%OFF, GEO,      PID,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      PNEW,     PSH,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   LBUF%VOL, MSSA,     DMELS,    IGEO,
     9   FACQ0,    CONDE,    GBUF%DT,  GBUF%G_DT,
     A   IPM,      RHOREF,   RHOSP,    NEL,
     B   ITY,      ISMSTR,   JTUR,     JTHE,
     C   JSMS,     NPG )
        ELSE
          CALL MDTSPH(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,LBUF%RE,  STIFN,    DT2T,
     3   NELTST,   ITYPTST,  LBUF%OFF, GEO,
     4   PID,      MUMAX,    CXX,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      PNEW,     PSH,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   GBUF%G_DT,GBUF%DT,  NEL,      ITY,
     9   JTUR,     JTHE)
        ENDIF
        CALL MJWL(PM     ,MAT     ,OFF      ,LBUF%SIG   ,LBUF%EINT ,
     1            PSH    ,P0      ,LBUF%QVIS,LBUF%VOL   ,LBUF%BFRAC,
     2            VOLN   ,QVIS    ,S1       ,S2         ,S3        ,
     3            DVOL   ,NEL     ,DF       ,LBUF%ABURN ,
     4            ER1V   ,ER2V    ,WDR1V    ,WDR2V      ,W1, AMU)

      ELSEIF (MTN == 6) THEN
        CALL M6LAW(
     1   PM,       OFF,      LBUF%SIG, LBUF%EINT,
     2   LBUF%RHO, LBUF%RK,  LBUF%RE,  LBUF%VK,
     3   VOLN,     RHO0,     VIS,      PSH,
     4   DXX,      DYY,      DZZ,      D4,
     5   D5,       D6,       C1,       C2,
     6   C3,       C4,       C5,       C6,
     7   PC,       AMU,      AMU2,     ESPE,
     8   CXX,      DF,       TMU,      MAT,
     9   WXX,      WYY,      WZZ,      NEL,
     A   DPDM,     JTUR,     JPOR)
        IF (JSPH == 0) THEN
          CALL MQVISCB(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,CXX,      LBUF%RE,  STIFN,
     3   DT2T,     NELTST,   ITYPTST,  AIRE,
     4   LBUF%OFF, GEO,      PID,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      PNEW,     PSH,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   LBUF%VOL, MSSA,     DMELS,    IGEO,
     9   FACQ0,    CONDE,    GBUF%DT,  GBUF%G_DT,
     A   IPM,      RHOREF,   RHOSP,    NEL,
     B   ITY,      ISMSTR,   JTUR,     JTHE,
     C   JSMS,     NPG )
        ELSE
          CALL MDTSPH(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,LBUF%RE,  STIFN,    DT2T,
     3   NELTST,   ITYPTST,  LBUF%OFF, GEO,
     4   PID,      MUMAX,    CXX,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      PNEW,     PSH,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   GBUF%G_DT,GBUF%DT,  NEL,      ITY,
     9   JTUR,     JTHE)
        ENDIF
        CALL MEINT(
     1   OFF,      LBUF%SIG, LBUF%QVIS,LBUF%EINT,
     2   VOLN,     ESPE,     S1,       S2,
     3   S3,       S4,       S5,       S6,
     4   DXX,      DYY,      DZZ,      D4,
     5   D5,       D6,       PSH,      DVOL,
     6   DF,       QVIS,     PNEW,     VIS,
     7   TMU,      EINC,     MTN,      VOL_AVG,
     8   NEL,      JTUR,     JLAG,     JPOR)
        IF(JTUR/=0)CALL ATURBN(
     1   PM,                 OFF,                LBUF%RHO,
     2   LBUF%RK,            LBUF%RE,            GEO,                EINC,
     3   DVOL,               VOLN,               PNEW,               TMU,
     4   VIS,                VD2,                MAT,                PID,
     5   LFT,                LLT,                JPOR,               JCLOSE)
        IF(JTHE == 1)CALL MTHETA(
     1   PM,       LBUF%EINT,LBUF%TEMP,AMU,
     2   C1,       C2,       DF,       PSH,
     3   PC,       MAT,      NEL)
      ELSEIF (MTN == 7) THEN
c        CALL M7LAW(PM     ,OFF     ,LBUF%SIG,LBUF%EINT,LBUF%RHO,
c     .             LBUF%QVIS,LBUF%VOL,MBUF%VAR,STIFN   )
      ELSEIF (MTN == 8) THEN
c        CALL M8LAW(PM     ,OFF     ,LBUF%SIG,LBUF%EINT,LBUF%RHO,
c     .             LBUF%QVIS,LBUF%VOL,MBUF%VAR,STIFN   )
      ELSEIF (MTN == 9) THEN
c        CALL M9LAW(PM     ,OFF     ,LBUF%SIG,LBUF%EINT,LBUF%RHO,
c     .             LBUF%QVIS,LBUF%VOL,MBUF%VAR,STIFN   )
      ELSEIF (MTN == 10) THEN
        CALL M10LAW(PM      ,OFF      ,LBUF%SIG  ,LBUF%EINT,LBUF%RHO,
     2           LBUF%EPSQ  ,LBUF%PLA ,LBUF%VOL  ,MAT      ,CXX     ,
     3           DVOL       ,VOLN     , DXX      ,DYY      ,DZZ     ,
     4           D4         ,D5       ,D6        ,S1       ,S2      ,
     5           S3         ,S4       ,S5        ,S6       ,SIGY    ,
     6           DEFP       ,PNEW     ,PSH       ,AMU      ,LBUF%SEQ,
     7           NEL        ,DPDM)
        IF (JSPH == 0) THEN
          CALL MQVISCB(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,CXX,      LBUF%RE,  STIFN,
     3   DT2T,     NELTST,   ITYPTST,  AIRE,
     4   LBUF%OFF, GEO,      PID,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      BID1,     BID2,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   LBUF%VOL, MSSA,     DMELS,    IGEO,
     9   FACQ0,    CONDE,    GBUF%DT,  GBUF%G_DT,
     A   IPM,      RHOREF,   RHOSP,    NEL,
     B   ITY,      ISMSTR,   JTUR,     JTHE,
     C   JSMS,     NPG )
        ELSE
          CALL MDTSPH(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,LBUF%RE,  STIFN,    DT2T,
     3   NELTST,   ITYPTST,  LBUF%OFF, GEO,
     4   PID,      MUMAX,    CXX,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      BID1,     BID2,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   GBUF%G_DT,GBUF%DT,  NEL,      ITY,
     9   JTUR,     JTHE)
        ENDIF
        CALL MEINT(
     1   OFF,      LBUF%SIG, LBUF%QVIS,LBUF%EINT,
     2   VOLN,     ESPE,     S1,       S2,
     3   S3,       S4,       S5,       S6,
     4   DXX,      DYY,      DZZ,      D4,
     5   D5,       D6,       PSH,      DVOL,
     6   DF,       QVIS,     PNEW,     VIS,
     7   TMU,      EINC,     MTN,      VOL_AVG,
     8   NEL,      JTUR,     JLAG,     JPOR)
      ELSEIF (MTN == 11) THEN      
        CALL M11LAW(PM      ,OFF     ,LBUF%SIG,LBUF%EINT,LBUF%RHO,
     2              LBUF%TEMP,LBUF%RK,LBUF%RE ,LBUF%VK  ,
     3              ALE_CONNECT   ,IX      ,IPARG   ,ELBUF_TAB,V       ,
     4              X       ,LBUF%EINS,STIFN  ,W        ,VD2     ,
     5              VDX     ,VDY     ,VDZ     ,VIS      ,MAT     ,
     6              VOLN    ,QVIS    ,RHO0    ,BUFVOIS  ,IPM     ,
     7              NPF     ,TF      ,SSP_EQ  ,GBUF%MOM ,NEL     )
        IF(IPARG(7,NG)+IPARG(11,NG) > 0 ) THEN
          CXX(1:NEL) = SSP_EQ(1:NEL)
        ELSE
          LBUF%SSP(1:NEL) = SSP_EQ(1:NEL)
        ENDIF
      ELSEIF (MTN == 12) THEN
C       TSAI 3D Complet
        EOSTYP= IPM(4,MAT(1))
        STRD1(1:NEL*3) => LBUF%EPE(1:NEL*3)
        STRD2(1:NEL*3) => LBUF%EPE(1+NEL*3:NEL*6)
        CALL M12LAW(
     1   PM,         OFF,        LBUF%SIG,   LBUF%EINT,
     2   LBUF%PLA,   LBUF%SIGF,  LBUF%EPSF,  LBUF%DAM,
     3   STRD1,      STRD2,      GAMA,       LBUF%VOL,
     4   RX,         RY,         RZ,         SX,
     5   SY,         SZ,         MAT,        VOLN,
     6   DVOL,       CXX,        DXX,        DYY,
     7   DZZ,        D4,         D5,         D6,
     8   S1,         S2,         S3,         S4,
     9   S5,         S6,         SIGY,       DEFP,
     A   NGL,        LBUF%SEQ,   NEL,        EOSTYP,
     B   RHO0,       AMU,        AMU2,       ESPE,
     C   DF,         PSH,        PNEW,       DPDM,
     D   DPDE,       LBUF%RHO,   LBUF%TEMP,  ECOLD,
     E   BUFMAT,     NPF,        TF,         LBUF%TSAIWU,
     F   EBUF%VAR,   NVAREOS,    JCVT,       JSPH)
        IF (JSPH == 0) THEN
          CALL MQVISCB(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,CXX,      LBUF%RE,  STIFN,
     3   DT2T,     NELTST,   ITYPTST,  AIRE,
     4   LBUF%OFF, GEO,      PID,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      PNEW,     PSH,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   LBUF%VOL, MSSA,     DMELS,    IGEO,
     9   FACQ0,    CONDE,    GBUF%DT,  GBUF%G_DT,
     A   IPM,      RHOREF,   RHOSP,    NEL,
     B   ITY,      ISMSTR,   JTUR,     JTHE,
     C   JSMS,     NPG )
        ELSE
          CALL MDTSPH(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,LBUF%RE,  STIFN,    DT2T,
     3   NELTST,   ITYPTST,  LBUF%OFF, GEO,
     4   PID,      MUMAX,    CXX,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      PNEW,     PSH,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   GBUF%G_DT,GBUF%DT,  NEL,      ITY,
     9   JTUR,     JTHE)
        ENDIF     
      ELSEIF (MTN == 13) THEN
        CALL M13LAW(PM      ,OFF     ,LBUF%SIG,LBUF%EINT,LBUF%RHO,
     .              LBUF%QVIS,LBUF%VOL,LBUF%PLA,STIFN   )
      ELSEIF (MTN == 14) THEN
        STRD1(1:NEL*3) => LBUF%EPE(1:NEL*3)
        STRD2(1:NEL*3) => LBUF%EPE(1+NEL*3:NEL*6)
        CALL M14LAW(
     1   PM,         OFF,        LBUF%SIG,   LBUF%EINT,
     2   LBUF%PLA,   LBUF%SIGF,  LBUF%EPSF,  LBUF%DAM,
     3   STRD1,      STRD2,      GAMA,       LBUF%VOL,
     4   RX,         RY,         RZ,         SX,
     5   SY,         SZ,         MAT,        VOLN,
     6   DVOL,       CXX,        DXX,        DYY,
     7   DZZ,        D4,         D5,         D6,
     8   S1,         S2,         S3,         S4,
     9   S5,         S6,         SIGY,       DEFP,
     A   NGL,        LBUF%SEQ,   NEL,        LBUF%TSAIWU,
     B   JCVT,       JSPH)
        IF (JSPH == 0) THEN
          CALL MQVISCB(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,CXX,      LBUF%RE,  STIFN,
     3   DT2T,     NELTST,   ITYPTST,  AIRE,
     4   LBUF%OFF, GEO,      PID,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      PNEW,     PSH,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   LBUF%VOL, MSSA,     DMELS,    IGEO,
     9   FACQ0,    CONDE,    GBUF%DT,  GBUF%G_DT,
     A   IPM,      RHOREF,   RHOSP,    NEL,
     B   ITY,      ISMSTR,   JTUR,     JTHE,
     C   JSMS,     NPG )
        ELSE
          CALL MDTSPH(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,LBUF%RE,  STIFN,    DT2T,
     3   NELTST,   ITYPTST,  LBUF%OFF, GEO,
     4   PID,      MUMAX,    CXX,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      PNEW,     PSH,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   GBUF%G_DT,GBUF%DT,  NEL,      ITY,
     9   JTUR,     JTHE)
        ENDIF
      ELSEIF (MTN == 16) THEN
        CALL M16LAW(PM        ,OFF       ,LBUF%SIG  ,NEL      ,
     2              LBUF%PLA  ,LBUF%TEMP ,LBUF%EPSD ,LBUF%XST ,
     3              MAT       ,DXX       ,DYY       ,DZZ      ,           
     4              D4        ,D5        ,D6        ,POLD     )
        CALL GRAY10(OFF  ,LBUF%SIG ,LBUF%QVIS ,LBUF%EINT ,VOLN,
     2              DVOL ,S1       ,S2        ,S3        ,S4  ,
     3              S5   ,S6       ,DXX       ,DYY       ,DZZ ,
     4              D4   ,D5       ,D6        ,NEL       ,POLD, 
     5              EINC)
        CALL GRAY20(
     1   PM,       LBUF%EINT,LBUF%RHO, LBUF%TEMP,
     2   LBUF%XST, LBUF%QVIS,VOLN,     MAT,
     3   DVOL,     POLD,     DF,       RHO0,
     4   PNEW,     P01,      P02,      E01,
     5   E02,      CXX,      DPDM,     NEL)
        IF (JSPH == 0) THEN
          CALL MQVISCB(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,CXX,      LBUF%RE,  STIFN,
     3   DT2T,     NELTST,   ITYPTST,  AIRE,
     4   LBUF%OFF, GEO,      PID,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      PNEW,     PSH,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   LBUF%VOL, MSSA,     DMELS,    IGEO,
     9   FACQ0,    CONDE,    GBUF%DT,  GBUF%G_DT,
     A   IPM,      RHOREF,   RHOSP,    NEL,
     B   ITY,      ISMSTR,   JTUR,     JTHE,
     C   JSMS,     NPG )
        ELSE
          CALL MDTSPH(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,LBUF%RE,  STIFN,    DT2T,
     3   NELTST,   ITYPTST,  LBUF%OFF, GEO,
     4   PID,      MUMAX,    CXX,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      PNEW,     PSH,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   GBUF%G_DT,GBUF%DT,  NEL,      ITY,
     9   JTUR,     JTHE)
        ENDIF
        CALL GRAY30(LBUF%SIG ,LBUF%EINT ,LBUF%QVIS ,LBUF%VOL ,QVIS,
     2              VOLN     ,DVOL      ,NEL       , P01     ,P02, 
     3              E01      ,E02       ,PNEW)
c
      ELSEIF (MTN == 17) THEN
        CALL M17LAW(
     1   PM,       OFF,      LBUF%SIG, LBUF%EINT,
     2   LBUF%RHO, LBUF%RK,  LBUF%RE,  LBUF%VK,
     3   WXX,      WYY,      WZZ,      VOLN,
     4   MAT,      VIS,      DXX,      DYY,
     5   DZZ,      D4,       D5,       D6,
     6   NEL,      ALOGEY,   CXX,      RHO0,
     7   TMU,      AMU,      AMU2,     PSH,
     8   PC,       ESPE,     C1,       C2,
     9   C3,       C4,       C5,       C6,
     A   DF,       DPDM,     JPOR)
        IF (JSPH == 0) THEN
          CALL MQVISCB(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,CXX,      LBUF%RE,  STIFN,
     3   DT2T,     NELTST,   ITYPTST,  AIRE,
     4   LBUF%OFF, GEO,      PID,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      PNEW,     PSH,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   LBUF%VOL, MSSA,     DMELS,    IGEO,
     9   FACQ0,    CONDE,    GBUF%DT,  GBUF%G_DT,
     A   IPM,      RHOREF,   RHOSP,    NEL,
     B   ITY,      ISMSTR,   JTUR,     JTHE,
     C   JSMS,     NPG )
        ELSE
          CALL MDTSPH(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,LBUF%RE,  STIFN,    DT2T,
     3   NELTST,   ITYPTST,  LBUF%OFF, GEO,
     4   PID,      MUMAX,    CXX,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      PNEW,     PSH,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   GBUF%G_DT,GBUF%DT,  NEL,      ITY,
     9   JTUR,     JTHE)
        ENDIF
        CALL MEINT(
     1   OFF,      LBUF%SIG, LBUF%QVIS,LBUF%EINT,
     2   VOLN,     ESPE,     S1,       S2,
     3   S3,       S4,       S5,       S6,
     4   DXX,      DYY,      DZZ,      D4,
     5   D5,       D6,       PSH,      DVOL,
     6   DF,       QVIS,     PNEW,     VIS,
     7   TMU,      EINC,     MTN,      VOL_AVG,
     8   NEL,      JTUR,     JLAG,     JPOR)
        CALL ATUR17(
     1   PM,      OFF,     LBUF%RHO,LBUF%RK,
     2   LBUF%RE, GEO,     VOLN,    MAT,
     3   DELTAX,  PID,     VIS,     VOLN,
     4   VD2,     DVOL,    AIRE,    EINC,
     5   PNEW,    TMU,     ALOGEY,  NEL,
     6   LFT,     LLT,     JPOR)
        IF(JTHE == 1)CALL MTHETA(
     1   PM,       LBUF%EINT,LBUF%TEMP,AMU,
     2   C1,       C2,       DF,       PSH,
     3   PC,       MAT,      NEL)
      ELSEIF (MTN == 18) THEN
        CALL M18LAW(
     1   PM,         LBUF%VOL,   LBUF%EINT,  LBUF%TEMP,
     2   LBUF%DELTAX,TF,         NPF,        DT2T,
     3   NELTST,     ITYPTST,    IPM,        STIFN,
     4   VOLN,       MAT,        NGL,        CONDE,
     5   NEL,        ITY)
      ELSEIF (MTN == 21) THEN
        CALL M21LAW(PM    ,OFF     ,LBUF%SIG,LBUF%EINT,LBUF%RHO,
     2           LBUF%EPSQ,LBUF%PLA,LBUF%VOL,MAT      ,CXX     ,
     3           DVOL     ,VOLN    ,DXX     ,DYY      ,DZZ     ,
     4           D4       ,D5      ,D6      ,S1       ,S2      ,
     5           S3       ,S4      ,S5      ,S6       ,TF      ,
     6           NPF      ,SIGY    ,DEFP    ,IPM      ,PNEW    ,
     7           PSH      ,AMU     ,LBUF%SEQ,NEL      )
        IF (JSPH == 0) THEN
          CALL MQVISCB(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,CXX,      LBUF%RE,  STIFN,
     3   DT2T,     NELTST,   ITYPTST,  AIRE,
     4   LBUF%OFF, GEO,      PID,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      BID1,     BID2,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   LBUF%VOL, MSSA,     DMELS,    IGEO,
     9   FACQ0,    CONDE,    GBUF%DT,  GBUF%G_DT,
     A   IPM,      RHOREF,   RHOSP,    NEL,
     B   ITY,      ISMSTR,   JTUR,     JTHE,
     C   JSMS,     NPG )
        ELSE
          CALL MDTSPH(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,LBUF%RE,  STIFN,    DT2T,
     3   NELTST,   ITYPTST,  LBUF%OFF, GEO,
     4   PID,      MUMAX,    CXX,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      BID1,     BID2,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   GBUF%G_DT,GBUF%DT,  NEL,      ITY,
     9   JTUR,     JTHE)
        ENDIF
        !no need to call eosmain since it is no energy dependant. P and Psh are already calculated  
        CALL MEINT(
     1   OFF,      LBUF%SIG, LBUF%QVIS,LBUF%EINT,
     2   VOLN,     ESPE,     S1,       S2,
     3   S3,       S4,       S5,       S6,
     4   DXX,      DYY,      DZZ,      D4,
     5   D5,       D6,       PSH,      DVOL,
     6   DF,       QVIS,     PNEW,     VIS,
     7   TMU,      EINC,     MTN,      VOL_AVG,
     8   NEL,      JTUR,     JLAG,     JPOR)
        CALL EOSUPDA(OFF  ,LBUF%SIG ,LBUF%EINT, LBUF%VOL ,PNEW ,NEL)        
      ELSEIF (MTN == 22) THEN
        CALL M22LAW(
     1   PM,       OFF,      LBUF%SIG, LBUF%EINT,
     2   LBUF%RHO, LBUF%QVIS,LBUF%PLA, LBUF%EPSD,
     3   LBUF%VOL, ZERO,     STIFN,    DT2T,
     4   NELTST,   ITYPTST,  LBUF%OFF, GEO,
     5   PID,      AMU,      VOL_AVG,  MUMAX,
     6   MAT,      NGL,      CXX,      DVOL,
     7   AIRE,     VOLN,     VD2,      DELTAX,
     8   VIS,      DXX,      DYY,      DZZ,
     9   D4,       D5,       D6,       PNEW,
     A   PSH,      QVIS,     SSP_EQ,   S1,
     B   S2,       S3,       S4,       S5,
     C   S6,       JPLASOL,  SIGY,     DEFP,
     D   DPLA,     MSSA,     DMELS,    CONDE,
     E   GBUF%DT,  GBUF%G_DT,NEL,      IPM,
     F   RHOREF,   RHOSP,    NFT,      JSPH,
     G   ITY,      JTUR,     JTHE,     ISMSTR,
     H   JSMS,     NPG )
      ELSEIF (MTN == 23) THEN
        CALL M22LAW(
     1   PM,       OFF,      LBUF%SIG, LBUF%EINT,
     2   LBUF%RHO, LBUF%QVIS,LBUF%PLA, LBUF%EPSD,
     3   LBUF%VOL, ONE,      STIFN,    DT2T,
     4   NELTST,   ITYPTST,  LBUF%OFF, GEO,
     5   PID,      AMU,      VOL_AVG,  MUMAX,
     6   MAT,      NGL,      CXX,      DVOL,
     7   AIRE,     VOLN,     VD2,      DELTAX,
     8   VIS,      DXX,      DYY,      DZZ,
     9   D4,       D5,       D6,       PNEW,
     A   PSH,      QVIS,     SSP_EQ,   S1,
     B   S2,       S3,       S4,       S5,
     C   S6,       JPLASOL,  SIGY,     DEFP,
     D   DPLA,     MSSA,     DMELS,    CONDE,
     E   GBUF%DT,  GBUF%G_DT,NEL,      IPM,
     F   RHOREF,   RHOSP,    NFT,      JSPH,
     G   ITY,      JTUR,     JTHE,     ISMSTR,
     H   JSMS,     NPG )
      ELSEIF (MTN == 24) THEN
        CALL M24LAW(
     1   LBUF,     PM,       OFF,      LBUF%SIG,
     2   LBUF%EINT,LBUF%RHO, LBUF%QVIS,LBUF%VOL,
     3   STIFN,    DT2T,     NELTST,   ITYPTST,
     4   OFFSET,   NEL,      LBUF%OFF, GEO,
     5   PID,      MAT,      NGL,      CXX,
     6   AIRE,     VOLN,     VD2,      DELTAX,
     7   VIS,      DXX,      DYY,      DZZ,
     8   PNEW,     PSH,      QVIS,     SSP_EQ,
     9   DVOL,     D4,       D5,       D6,
     A   MUMAX,    GAMA,     MSSA,     DMELS,
     B   R3_DAM,   S1,       S2,       S3,
     C   S4,       S5,       S6,       RX,
     D   RY,       RZ,       SX,       SY,
     E   SZ,       CONDE,    VOL_AVG,  GBUF%DT,
     F   GBUF%G_DT,IPM,      RHOREF,   RHOSP,
     G   LBUF%EPSD,ITY,      JTUR,     JTHE,
     H   JHBE,     JCVT,     JSPH,     ISMSTR,
     I   JSMS,     NPG )
C     like law25 for shell + S33 = EPS33*E33
      ELSEIF (MTN == 25) THEN
        CALL M25LAW(
     1   PM,         MAT,        OFF,        LBUF%SIG,
     2   LBUF%EINT,  S1,         S2,         S3,
     3   S4,         S5,         S6,         DXX,
     4   DYY,        DZZ,        D4,         D5,
     5   D6,         RX,         RY,         RZ,
     6   SX,         SY,         SZ,         TX,
     7   TY,         TZ,         GAMA,       VOLN,
     8   CXX,        LBUF%VOL,   LBUF%EPSD,  LBUF%PLA,
     9   LBUF%DAM,   LBUF%CRAK,  LBUF%STRA,  DEFP,
     A   SIGY,       SIGL,       NGL,        ILAY,
     B   LBUF%OFF,   LBUF%SEQ,   NEL,        IPM,
     C   IPG,        LBUF%TSAIWU,NFT,        NPT,
     D   JCVT,       JSPH,       ISORTH)
        IF (JSPH == 0) THEN
          CALL MQVISCB(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,CXX,      LBUF%RE,  STIFN,
     3   DT2T,     NELTST,   ITYPTST,  AIRE,
     4   LBUF%OFF, GEO,      PID,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      PNEW,     PSH,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   LBUF%VOL, MSSA,     DMELS,    IGEO,
     9   FACQ0,    CONDE,    GBUF%DT,  GBUF%G_DT,
     A   IPM,      RHOREF,   RHOSP,    NEL,
     B   ITY,      ISMSTR,   JTUR,     JTHE,
     C   JSMS,     NPG )
        ELSE
          CALL MDTSPH(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,LBUF%RE,  STIFN,    DT2T,
     3   NELTST,   ITYPTST,  LBUF%OFF, GEO,
     4   PID,      MUMAX,    CXX,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      PNEW,     PSH,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   GBUF%G_DT,GBUF%DT,  NEL,      ITY,
     9   JTUR,     JTHE)
        ENDIF
      ELSEIF (MTN == 26) THEN
        CALL M26LAW(PM      ,OFF      ,LBUF%SIG ,LBUF%RHO ,
     2              LBUF%PLA,LBUF%TEMP,LBUF%EPSD,LBUF%Z   ,
     3              MAT     ,VOLN     ,DVOL     ,DXX      ,
     4              DYY     ,DZZ      ,D4       ,D5       ,
     5              D6      ,NEL, POLD, RHO0, DF)
        CALL SESA10(OFF    ,LBUF%SIG,LBUF%QVIS,LBUF%EINT,VOLN,
     2              DVOL   ,S1      ,S2       ,S3       ,S4  ,
     3              S5     ,S6      ,DXX      ,DYY      ,DZZ ,
     4              D4     ,D5      ,D6       ,NEL, POLD, DF, EINC)
        CALL SESA20(
     1   PM,       LBUF%EINT,LBUF%RHO, LBUF%TEMP,
     2   LBUF%Z,   LBUF%QVIS,BUFMAT,   LBUF%SSP,
     3   XK,       VOLN,     MAT,      QVIS,
     4   DVOL,     POLD,     CXX,      RHO0,
     5   P01,      NEL)
        IF (JSPH == 0) THEN
         CALL MQVISC26(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,CXX,      LBUF%RE,  STIFN,
     3   DT2T,     NELTST,   ITYPTST,  AIRE,
     4   LBUF%OFF, GEO,      PID,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      PNEW,     PSH,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   XK,       NEL,      ITY,      ISMSTR,
     9   JTUR,     JTHE)
        ELSE
          CALL MDTSPH(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,LBUF%RE,  STIFN,    DT2T,
     3   NELTST,   ITYPTST,  LBUF%OFF, GEO,
     4   PID,      MUMAX,    CXX,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      PNEW,     PSH,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   GBUF%G_DT,GBUF%DT,  NEL,      ITY,
     9   JTUR,     JTHE)
        ENDIF
        CALL SESA30(LBUF%SIG ,LBUF%EINT ,LBUF%QVIS ,LBUF%VOL ,QVIS,
     2              VOLN     ,DVOL      ,NEL, P01)
      ELSEIF (MTN == 46 .OR. MTN == 47) THEN
        NUVAR  = IPM(8,MAT(1))
        CALL M46LAW(
     1   LFT,      LLT,      NFT,      MTN,
     2   PM,       OFF,      LBUF%SIG, LBUF%EINT,
     3   LBUF%RHO, LBUF%QVIS,LBUF%VOL, MBUF%VAR,
     4   BUFMAT,   STIFN,    MAT,      NGL,
     5   NUVAR,    DT2T,     NELTST,   RHO0,
     6   ITYPTST,  LBUF%OFF, JLAG,     GEO,
     7   PID,      CXX,      AIRE,     VOLN,
     8   VD2,      DELTAX,   VIS,      DXX,
     9   DYY,      DZZ,      PNEW,     PSH,
     A   QVIS,     SSP_EQ,   WXX,      WYY,
     B   WZZ,      IPM,      MSSA,     DMELS,
     C   DVOL,     S1,       S2,       S3,
     D   S4,       S5,       S6,       CONDE,
     E   VOL_AVG,  GBUF%DT,  GBUF%G_DT,NEL,
     F   D4,       D5,       D6,       RHOREF,
     G   RHOSP,    ISMSTR,   ITY,      JSMS,
     H   JTUR,     JTHE,     NPG )
C
      ELSEIF (MTN == 49) THEN
        CALL M49LAW (MAT      ,PM       ,OFF     ,LBUF%SIG,LBUF%PLA,
     2               LBUF%TEMP,LBUF%EPSD,CXX     ,DF      ,DXX     ,
     3               DYY      ,DZZ      ,D4      ,D5      ,D6      ,
     4               RHO0     ,DPDM     ,SIGY    ,DEFP    ,DPLA    ,
     5               ESPE     ,ECOLD    ,NEL     )
        IF (JSPH == 0) THEN
          CALL MQVISCB(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,CXX,      LBUF%RE,  STIFN,
     3   DT2T,     NELTST,   ITYPTST,  AIRE,
     4   LBUF%OFF, GEO,      PID,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      PNEW,     PSH,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   LBUF%VOL, MSSA,     DMELS,    IGEO,
     9   FACQ0,    CONDE,    GBUF%DT,  GBUF%G_DT,
     A   IPM,      RHOREF,   RHOSP,    NEL,
     B   ITY,      ISMSTR,   JTUR,     JTHE,
     C   JSMS,     NPG )
        ELSE
          CALL MDTSPH(
     1   PM,       OFF,      LBUF%RHO, LBUF%RK,
     2   LBUF%TEMP,LBUF%RE,  STIFN,    DT2T,
     3   NELTST,   ITYPTST,  LBUF%OFF, GEO,
     4   PID,      MUMAX,    CXX,      VOLN,
     5   VD2,      DELTAX,   VIS,      DXX,
     6   DYY,      DZZ,      PNEW,     PSH,
     7   MAT,      NGL,      QVIS,     SSP_EQ,
     8   GBUF%G_DT,GBUF%DT,  NEL,      ITY,
     9   JTUR,     JTHE)
        ENDIF
        CALL MEINT(
     1   OFF,      LBUF%SIG, LBUF%QVIS,LBUF%EINT,
     2   VOLN,     ESPE,     S1,       S2,
     3   S3,       S4,       S5,       S6,
     4   DXX,      DYY,      DZZ,      D4,
     5   D5,       D6,       PSH,      DVOL,
     6   DF,       QVIS,     PNEW,     VIS,
     7   TMU,      EINC,     MTN,      VOL_AVG,
     8   NEL,      JTUR,     JLAG,     JPOR)
C
      ELSEIF (MTN > 28 .AND. MTN < 32 .or. MTN == 99 .or. MTN == 200) THEN
c---    user material law libraries here
c
        NUVAR   = ELBUF_TAB(NG)%BUFLY(ILAY)%NVAR_MAT
        l_MULAW_CALLED=.TRUE.
c
        CALL USERMAT_SOLID(
     1   LFT,        LLT,        NFT,        MTN,
     2   JCVT,       PM,         OFF,        LBUF%SIG,
     3   LBUF%EINT,  LBUF%RHO,   LBUF%QVIS,  LBUF%VOL,
     4   LBUF%STRA,  LBUF%SIGL,  GAMA,       MBUF%VAR,
     5   BUFMAT,     TF,         NPF,        STIFN,
     6   MAT,        NGL,        NUVAR,      DT2T,
     7   NELTST,     ITYPTST,    LBUF%OFF,   GEO,
     8   PID,        LBUF%EPSD,  LBUF%TEMP,  WXX,
     9   WYY,        WZZ,        JSPH,       MUMAX,
     A   CXX,        AIRE,       VOLN,       VD2,
     B   DELTAX,     VIS,        DXX,        DYY,
     C   DZZ,        D4,         D5,         D6,
     D   PNEW,       PSH,        QVIS,       SSP_EQ,
     E   DVOL,       S1,         S2,         S3,
     F   S4,         S5,         S6,         RX,
     G   RY,         RZ,         SX,         SY,
     H   SZ,         TX,         TY,         TZ,
     I   JPLASOL,    SIGY,       DEFP,       ISMSTR,
     J   MFXX,       MFXY,       MFXZ,       MFYX,
     K   MFYY,       MFYZ,       MFZX,       MFZY,
     L   MFZZ,       IPM,        ISORTH,     FBUF,
     M   NFAIL,      NPG,        LBUF%SIGD,  DXY,
     N   DYX,        DYZ,        DZY,        DZX,
     O   DXZ,        FR_WAV,     ISROT,      V,
     P   VARNL,      W,          IX,         X,
     Q   JTHE,       ET,         MSSA,       DMELS,
     R   IPTR,       IPTS,       IPTT,       TABLE,
     S   FVD2,       FDELTAX,    FSSP,       FQVIS,
     T   TEMPEL,     IPARG1,     IGEO,       LBUF%SIGV,
     U   AL_IMP,     SIGNOR,     ISTRAIN,    NG,
     V   ELBUF_TAB,  VBUF,       ILAY,       LBUF%VK,
     W   IPARG,      BUFVOIS,    VDX,        VDY,
     X   VDZ,        IHET,       CONDE,      ITASK,
     Y   IEXPAN,     VOL_AVG,    AMU,        EPSTH,
     Z   LBUF%FORTH, LBUF%VISC,  NEL,        GBUF%ETOTSH,
     1   ISELECT,    TSTAR,      LBUF%MU,    AMU2,
     2   DPDM,       RHOREF,     RHOSP,      NLOC_DMG,
     3   ITY,        JTUR,       MAT_ELEM,   IDEL7NOK)
c
      ELSE   ! 'user type' Radioss material laws 
C---
        NUVAR   = ELBUF_TAB(NG)%BUFLY(ILAY)%NVAR_MAT
        NVARTMP = ELBUF_TAB(NG)%BUFLY(ILAY)%NVARTMP
        l_MULAW_CALLED =.TRUE.
c
        CALL MULAW(
     1   NFT,         MTN,         JCVT,        PM,
     2   OFF,         LBUF%SIG,    LBUF%EINT,   LBUF%RHO,
     3   LBUF%QVIS,   LBUF%VOL,    LBUF%STRA,   LBUF%SIGL,
     4   GAMA,        MBUF%VAR,    BUFMAT,      TF,
     5   NPF,         STIFN,       MAT,         NGL,
     6   NUVAR,       DT2T,        NELTST,      ITYPTST,
     7   LBUF%OFF,    GEO,         PID,         LBUF%EPSD,
     8   LBUF%TEMP,   WXX,         WYY,         WZZ,
     9   JSPH,        MUMAX,       CXX,         AIRE,
     A   VOLN,        VD2,         DELTAX,      VIS,
     B   DXX,         DYY,         DZZ,         D4,
     C   D5,          D6,          PNEW,        PSH,
     D   QVIS,        SSP_EQ,      DVOL,        S1,
     E   S2,          S3,          S4,          S5,
     F   S6,          RX,          RY,          RZ,
     G   SX,          SY,          SZ,          TX,
     H   TY,          TZ,          JPLASOL,     SIGY,
     I   DEFP,        ISMSTR,      MFXX,        MFXY,
     J   MFXZ,        MFYX,        MFYY,        MFYZ,
     K   MFZX,        MFZY,        MFZZ,        IPM,
     L   ISORTH,      FBUF,        NFAIL,       NPG,
     M   LBUF%SIGD,   DXY,         DYX,         DYZ,
     N   DZY,         DZX,         DXZ,         FR_WAV,
     O   V,           VARNL,       W,           IX,
     P   X,           JTHE,        ET,          MSSA,
     Q   DMELS,       IPTR,        IPTS,        IPTT,
     R   TABLE,       FVD2,        FDELTAX,     FSSP,
     S   FQVIS,       TEMPEL,      IGEO,        LBUF%SIGV,
     T   AL_IMP,      SIGNOR,      ISTRAIN,     NG,
     U   ELBUF_TAB,   VBUF,        ILAY,        LBUF%VK,
     V   ALE_CONNECT, IPARG,       BUFVOIS,     VDX,
     W   VDY,         VDZ,         IHET,        CONDE,
     X   ITASK,       IEXPAN,      VOL_AVG,     AMU,
     Y   EPSTH,       LBUF%FORTH,  LBUF%VISC,   NEL,
     Z   GBUF%ETOTSH, ISELECT,     TSTAR,       LBUF%MU,
     1   DPDM,        RHOREF,      RHOSP,       NVARTMP,
     2   MBUF%VARTMP, LBUF%EINTTH, MAT_ELEM,    NLOC_DMG,
     3   ITY,         JTUR,        JSMS,        IDEL7NOK)

      ENDIF 
C-----------------------------------
C     EOS PART2
C-----------------------------------
      IF (MTN == 3.OR.MTN == 4.OR.MTN == 6.OR.MTN==10.OR.MTN==17.OR.MTN == 49.OR.MTN == 102.OR.MTN == 103) THEN
         IF (MTN /= 6 .AND. MTN /= 17) THEN
            PNEW(:) = ZERO
         ENDIF
        CALL EOSMAIN(1       ,NEL      ,EOSTYP  ,PM       ,OFF      ,LBUF%EINT,
     2               LBUF%RHO,RHO0     ,AMU     ,AMU2     ,ESPE     ,
     3               DVOL    ,DF       ,VOLN    ,MAT      ,PSH      ,
     4               PNEW    ,DPDM     ,DPDE    ,LBUF%TEMP,ECOLD    ,
     5               BUFMAT  ,LBUF%SIG ,LBUF%MU ,MTN      ,POLD     ,
     6               NPF     ,TF       ,EBUF%VAR,NVAREOS)
        CALL EOSUPDA(OFF  ,LBUF%SIG ,LBUF%EINT, LBUF%VOL ,PNEW,NEL)
      ELSEIF(l_MULAW_CALLED)THEN
        IF(l_MULAW_CALLED)THEN
         DO I=1,NEL
           IF(LBUF%VOL(I) > ZERO)THEN
             LBUF%EINT(I)=LBUF%EINT(I)/MAX(LBUF%VOL(I),EM20)
           ELSE
             LBUF%EINT(I)=ZERO
           ENDIF
         ENDDO
        ENDIF
      ENDIF

C---------------------------------------------------------------------
C --- Needed for finite element transfert.
C
      IF (JTHE < 0) THEN
        HEAT_MECA_L = ZERO
        DO I=1,NEL
          DIE(I) = (LBUF%EINT(I)*LBUF%VOL(I) - DIE(I)) * PM(90,MAT(1))
          HEAT_MECA_L = HEAT_MECA_L + DIE(I)
        ENDDO
!$OMP CRITICAL
      HEAT_MECA = HEAT_MECA_L + HEAT_MECA
!$OMP END CRITICAL
      ENDIF
      IF((IEXPAN > 0).AND.(JTHE < 0).AND.(TT/=0)) THEN
        DO I=1,NEL
          SIGKK(I) = LBUF%SIG(NEL*(1-1)+I)+LBUF%SIG(NEL*(2-1)+I)+LBUF%SIG(NEL*(3-1)+I)
          LBUF%EINTTH(I) = LBUF%EINTTH(I)-HALF*SIGKK(I)*ETH(I)
        ENDDO
      ENDIF

C---------------------------------------------------------------------
C   calcul of viscosity ( Navier-Stokes) 
C---------------------------------------------------------------------
      IF(ISVIS > 0.AND. ((MTN < 28 .AND. MTN /= 24) .OR. MTN == 49)) THEN
        VOLG(1:NEL)=NPG*VOLN(1:NEL)
        CALL MNSVIS(
     1   PM,      OFF,     LBUF%RHO,GEO,
     2   PID,     CXX,     AIRE,    VOLG,
     3   DXX,     DYY,     DZZ,     D4,
     4   D5,      D6,      MAT,     ISVIS,
     5   RHOREF,  NEL)
          DO  I=1,NEL                                                 
            VOL(I)  = LBUF%VOL(I)                                       
            EINT(I) = LBUF%EINT(I)                                      
            E1=DXX(I)*SVIS(I,1)                                         
            E2=DYY(I)*SVIS(I,2)                                         
            E3=DZZ(I)*SVIS(I,3)                                         
            E4=D4(I)*SVIS(I,4)                                          
            E5=D5(I)*SVIS(I,5)                                          
            E6=D6(I)*SVIS(I,6)                                          
            EINT(I)=EINT(I)*VOL(I)                                      
     .       +(E1+E2+E3+E4+E5+E6)*(VOLN(I)-HALF*DVOL(I))*DT1*HALF  
            EINT(I)=EINT(I)/MAX(EM20,VOL(I))                            
            LBUF%EINT(I) = EINT(I)     
          ENDDO     
      ENDIF    
C-----------------------------------------------------------------------
C     viscous stress (/VISC models) 
C-----------------------------------------------------------------------
      IF (IVISC > 0 .AND. (MTN  < 28 .AND. MTN /= 24)) THEN
        DO I=1,NEL
          EP1(I) = DXX(I)*OFF(I)
          EP2(I) = DYY(I)*OFF(I)
          EP3(I) = DZZ(I)*OFF(I)
          EP4(I) = D4(I) *OFF(I)
          EP5(I) = D5(I) *OFF(I)
          EP6(I) = D6(I) *OFF(I)
        ENDDO
        IF (ISORTH /= 0 .AND. JCVT == 0) THEN
          CALL MREPLOC(
     1   GAMA,    R11,     R12,     R13,
     2   R21,     R22,     R23,     R31,
     3   R32,     R33,     RX,      RY,
     4   RZ,      SX,      SY,      SZ,
     5   TX,      TY,      TZ,      NEL,
     6   JSPH)
          DO I=1,NEL
            EP4(I) = HALF*EP4(I)
            EP5(I) = HALF*EP5(I)
            EP6(I) = HALF*EP6(I)
          ENDDO
          CALL MROTENS(1,NEL,EP1,EP2,EP3,EP4,EP5,EP6,
     .         R11,R21,R31,R12,R22,R32,R13,R23,R33)        
          DO I=1,NEL
            EP4(I) = TWO*EP4(I)
            EP5(I) = TWO*EP5(I)
            EP6(I) = TWO*EP6(I)
          ENDDO
        ENDIF
c---        
         CALL VISCMAIN(MAT_ELEM%MAT_PARAM(MAT(1))%VISC    ,NEL     ,
     .        NVARVIS ,VBUF%VAR,RHO0    ,VIS     ,CXX     ,DT1     , 
     .        EP1     ,EP2     ,EP3     ,EP4     ,EP5     ,EP6     ,
     .        SV1     ,SV2     ,SV3     ,SV4     ,SV5     ,SV6     ,
     .        MFXX    ,MFXY    ,MFXZ    ,MFYX    ,MFYY    ,MFYZ    ,
     .        MFZX    ,MFZY    ,MFZZ    ,
     .        S1      ,S2      ,S3      ,S4      ,S5      ,S6      )
c---
        IF (ISORTH /= 0.AND. JCVT == 0) THEN
          CALL MROTENS(1,NEL,
     .         SV1     ,SV2      ,SV3      ,SV4     ,SV5     ,SV6     ,
     .         R11,R21,R31,R12,R22,R32,R13,R23,R33)
        ENDIF
c
        DO I=1,NEL    
          LBUF%VISC(NEL*(1-1) + I) = SV1(I)*OFF(I)
          LBUF%VISC(NEL*(2-1) + I) = SV2(I)*OFF(I)
          LBUF%VISC(NEL*(3-1) + I) = SV3(I)*OFF(I)
          LBUF%VISC(NEL*(4-1) + I) = SV4(I)*OFF(I)
          LBUF%VISC(NEL*(5-1) + I) = SV5(I)*OFF(I)
          LBUF%VISC(NEL*(6-1) + I) = SV6(I)*OFF(I)
        ENDDO           
        DO I=1,NEL                      
          SVIS(I,1) = LBUF%VISC(NEL*(1-1) + I)         
          SVIS(I,2) = LBUF%VISC(NEL*(2-1) + I)         
          SVIS(I,3) = LBUF%VISC(NEL*(3-1) + I)         
          SVIS(I,4) = LBUF%VISC(NEL*(4-1) + I)         
          SVIS(I,5) = LBUF%VISC(NEL*(5-1) + I)         
          SVIS(I,6) = LBUF%VISC(NEL*(6-1) + I)         
        ENDDO                             
        IF (ISVIS > 0) THEN
c         viscous stress output
          DO I=1,NEL
            LBUF%SIGV(NEL*(1-1) + I) = SVIS(I,1)
            LBUF%SIGV(NEL*(2-1) + I) = SVIS(I,2)
            LBUF%SIGV(NEL*(3-1) + I) = SVIS(I,3)
            LBUF%SIGV(NEL*(4-1) + I) = SVIS(I,4)
            LBUF%SIGV(NEL*(5-1) + I) = SVIS(I,5)
            LBUF%SIGV(NEL*(6-1) + I) = SVIS(I,6)
           ENDDO                          
        ENDIF
      ELSEIF (IVISC > 0) THEN
        DO I=1,NEL    
          LBUF%VISC(NEL*(1-1) + I) = SVIS(I,1)
          LBUF%VISC(NEL*(2-1) + I) = SVIS(I,2)
          LBUF%VISC(NEL*(3-1) + I) = SVIS(I,3)
          LBUF%VISC(NEL*(4-1) + I) = SVIS(I,4)
          LBUF%VISC(NEL*(5-1) + I) = SVIS(I,5)
          LBUF%VISC(NEL*(6-1) + I) = SVIS(I,6)
        ENDDO           
        IF (ISVIS > 0) THEN                      
c         viscous stress output                  
          DO I=1,NEL                           
            LBUF%SIGV(NEL*(1-1) + I) = SVIS(I,1)        
            LBUF%SIGV(NEL*(2-1) + I) = SVIS(I,2)        
            LBUF%SIGV(NEL*(3-1) + I) = SVIS(I,3)        
            LBUF%SIGV(NEL*(4-1) + I) = SVIS(I,4)        
            LBUF%SIGV(NEL*(5-1) + I) = SVIS(I,5)        
            LBUF%SIGV(NEL*(6-1) + I) = SVIS(I,6)        
           ENDDO                                 
        ENDIF                                    
      ENDIF
C-----------------------------------------------------------------------
c     failure for law no user ---
C-----------------------------------------------------------------------
      IF ((ITASK==0).AND.(IMON_MAT==1))CALL STARTIME(121,1)
      IF (NFAIL > 0 .AND. (MTN < 28 .OR. MTN == 49)) THEN
        IF (ISTRAIN > 0 .OR. MTN==24 .OR. MTN==25 .OR. MTN==15) THEN
          DO I=1,NEL
            ES1(I) = LBUF%STRA(NEL*(1-1) + I)     
            ES2(I) = LBUF%STRA(NEL*(2-1) + I) 
            ES3(I) = LBUF%STRA(NEL*(3-1) + I) 
            ES4(I) = LBUF%STRA(NEL*(4-1) + I) 
            ES5(I) = LBUF%STRA(NEL*(5-1) + I) 
            ES6(I) = LBUF%STRA(NEL*(6-1) + I) 
          ENDDO
        ELSE
          DO I=1,NEL
            ES1(I) = EP20
            ES2(I) = EP20
            ES3(I) = EP20
            ES4(I) = EP20
            ES5(I) = EP20
            ES6(I) = EP20  
          ENDDO
        ENDIF
C------------------------------------------------------------
C     Recovering non-local variable
C------------------------------------------------------------
        INLOC = IPARG(78,NG)
        IF (INLOC > 0) THEN
          ! -> Length used for failure criterion parameters scaling is LE_MAX
          LE_MAX(1:NEL) = NLOC_DMG%LE_MAX(MAT(1))
          EL_LEN => LE_MAX(1:NEL) 
          ! -> Copying the non-local plastic strain increment
          IF (ELBUF_TAB(NG)%BUFLY(ILAY)%L_PLA > 0) THEN 
            DO I = 1,NEL
              IF (OFF(I) == ONE) THEN
                VARNL(I) = MAX(VARNL(I),ZERO)
              ELSE
                VARNL(I) = ZERO
              ENDIF
              LBUF%PLANL(I)  = LBUF%PLANL(I) + VARNL(I)
              LBUF%EPSDNL(I) = VARNL(I)/MAX(DT1,EM20)
              DPLA(I) = MAX(VARNL(I),ZERO)
            ENDDO
            EL_PLA => LBUF%PLANL(1:NEL)
          ENDIF
        ELSE
          ! -> Length used for failure criterion parameters scaling is the element length
          EL_LEN => DELTAX(1:NEL)
          EL_PLA => DEFP(1:NEL)
        ENDIF
c---
        DO IR = 1,NFAIL 
          IP = (IR -1)*15         
c
          DO I=1,NEL
            SS1(I) = LBUF%SIG(NEL*(1-1) + I)    
            SS2(I) = LBUF%SIG(NEL*(2-1) + I)
            SS3(I) = LBUF%SIG(NEL*(3-1) + I)
            SS4(I) = LBUF%SIG(NEL*(4-1) + I)
            SS5(I) = LBUF%SIG(NEL*(5-1) + I)
            SS6(I) = LBUF%SIG(NEL*(6-1) + I)
            EINT(I) = LBUF%EINT(I)*LBUF%VOL(I)
            IF (MTN/=4.AND.MTN/=2) THEN
              EPSP(I)=OFF(I)*                                        
     .        MAX( ABS(DXX(I)),ABS(DYY(I)),ABS(DZZ(I)),              
     .       HALF*ABS(D4(I)),HALF*ABS(D5(I)),HALF*ABS(D6(I)))
            ENDIF         
          ENDDO
C------------------------------------------------------------
C     Recovering non-local plastic strain-rate
C------------------------------------------------------------
          IF (INLOC > 0) THEN 
            IF (ELBUF_TAB(NG)%BUFLY(ILAY)%L_PLA > 0) THEN 
              DO I = 1,NEL
                EPSP(I) = LBUF%EPSDNL(I)
              ENDDO
            ENDIF
          ENDIF
C           
          IF (ISORTH  > 0 .AND. JCVT == 0) THEN 
            CALL MREPLOC(
     1   GAMA,    R11,     R12,     R13,
     2   R21,     R22,     R23,     R31,
     3   R32,     R33,     RX,      RY,
     4   RZ,      SX,      SY,      SZ,
     5   TX,      TY,      TZ,      NEL,
     6   JSPH)
            CALL MROTENS(1,NEL,SS1,SS2,SS3,SS4,SS5,SS6,
     .              R11,R12,R13,
     .              R21,R22,R23,
     .              R31,R32,R33)
C strain        
            DO I=1,NEL
              ES4(I) = HALF*ES4(I)
              ES5(I) = HALF*ES5(I)
              ES6(I) = HALF*ES6(I)
            ENDDO              
            CALL MROTENS(1,NEL,ES1,ES2,ES3,ES4,ES5,ES6,
     .              R11,R12,R13,
     .              R21,R22,R23,
     .              R31,R32,R33)
            DO I=1,NEL
              ES4(I) = TWO*ES4(I)
              ES5(I) = TWO*ES5(I)
              ES6(I) = TWO*ES6(I)
            ENDDO 
C strain rate
            DO I=1,NEL
              EP1(I) = DXX(I)
              EP2(I) = DYY(I)
              EP3(I) = DZZ(I)
              EP4(I) = HALF*D4(I)
              EP5(I) = HALF*D5(I)
              EP6(I) = HALF*D6(I)
            ENDDO              
            CALL MROTENS(1,NEL,EP1,EP2,EP3,EP4,EP5,EP6,
     .              R11,R12,R13,
     .              R21,R22,R23,
     .              R31,R32,R33)
            DO I=1,NEL
              EP4(I) = TWO*EP4(I)
              EP5(I) = TWO*EP5(I)
              EP6(I) = TWO*EP6(I)
            ENDDO
          ELSE          !  strain rate
            DO I=1,NEL
              EP1(I) = DXX(I)
              EP2(I) = DYY(I)
              EP3(I) = DZZ(I)
              EP4(I) = HALF*D4(I)
              EP5(I) = HALF*D5(I)
              EP6(I) = HALF*D6(I)
            ENDDO              
          ENDIF  
c
c----  
          UVARF => FBUF%FLOC(IR)%VAR  
          IRUPT  = FBUF%FLOC(IR)%ILAWF 
          NVARF  = FBUF%FLOC(IR)%NVAR 
          DFMAX => FBUF%FLOC(IR)%DAMMX 
          DAMINI => FBUF%FLOC(IR)%DAMINI 
          TDEL  => FBUF%FLOC(IR)%TDEL  
          NPARAM = IPM(112 + IP, MAT(1)) 
          IADBUF = IPM(114 + IP ,MAT(1))  
          NFUNC  = IPM(115 + IP, MAT(1))             
          DO I=1,NFUNC
            IFUNC(I) = IPM(115+IP+I, MAT(1))         
          ENDDO  
          DO I = 1,NEL
            DE1(I) = DXX(I)*DT1
            DE2(I) = DYY(I)*DT1
            DE3(I) = DZZ(I)*DT1
            DE4(I) = DXY(I)*DT1 
            DE5(I) = DYZ(I)*DT1
            DE6(I) = DZX(I)*DT1
          ENDDO 
c
          IF (IRUPT == 1)THEN                                            
C  --- Johnson cook              
               CALL FAIL_JOHNSON(LLT ,NPARAM,NVARF,NFUNC,IFUNC,
     2                     NPF ,TF  ,TT  ,DT1  ,BUFMAT(IADBUF),
     3                     NGL ,IPM ,MAT ,
     4                     SS1  ,SS2  ,SS3  ,SS4   ,SS5   ,SS6,
     5                     DPLA ,EPSP ,TSTAR,UVARF ,OFF   ,IP ,
     6                     DFMAX,TDEL )                     
          ELSEIF(IRUPT == 2)THEN                                         
C  --- Tuler butcher   
              CALL FAIL_TBUTCHER_S(LLT ,NPARAM,NVARF,NFUNC,IFUNC,
     2                    NPF ,TF  ,TT  ,DT1  ,BUFMAT(IADBUF),
     3                    NGL ,IPM ,MAT ,
     4                    SS1  ,SS2  ,SS3  ,SS4   ,SS5   ,SS6,
     5                     UVARF,OFF ,IP   ,DFMAX ,TDEL  )                                     
          ELSEIF(IRUPT == 3)THEN                                         
C  --- wilkins
              CALL FAIL_WILKINS_S(LLT ,NPARAM,NVARF,NFUNC,IFUNC,
     2                    NPF ,TF  ,TT  ,DT1  ,BUFMAT(IADBUF),
     3                    NGL ,IPM ,MAT ,
     4                    SS1  ,SS2  ,SS3  ,SS4   ,SS5   ,SS6,
     5                    DPLA ,UVARF,OFF  ,IP    ,DFMAX ,TDEL ) 
C--------------------------------------------------------------
C  user1
        ELSEIF(IRUPT==4)THEN 
            IF (LOGICAL_USERL_AVAIL)THEN
               USER_UELR(1:LLT)=GBUF%UELR(1:LLT)
               CALL ENG_USERLIB_FLAW(IRUPT,
     1                    LLT ,NPARAM,NVARF,NFUNC,IFUNC,
     2                    NPF ,TF  ,TT  ,DT1  ,BUFMAT(IADBUF),
     3                    NGL ,IBIDON1,IBIDON2,IBIDON3 ,IBIDON4,
     4                    EP1  ,EP2 ,EP3  ,EP4  ,EP5  ,EP6 ,
     5                    ES1  ,ES2 ,ES3  ,ES4  ,ES5  ,ES6 ,
     6                    SS1  ,SS2 ,SS3  ,SS4  ,SS5  ,SS6 ,    
     7                    DEFP ,DPLA,EPSP,UVARF,OFF  ,
     8                    DELTAX,VOLN,USER_UELR,BIDON4,BIDON5) 
            GBUF%UELR(1:LLT) = USER_UELR(1:LLT)
            ELSE
!!!
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/FAIL/USER1 - SOLID '
                LENGTH=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:LENGTH),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
!!!
            ENDIF
C   user2
        ELSEIF(IRUPT==5)THEN        
            IF (LOGICAL_USERL_AVAIL)THEN
               USER_UELR(1:LLT)=GBUF%UELR(1:LLT)
               CALL ENG_USERLIB_FLAW(IRUPT,
     1                    LLT ,NPARAM,NVARF,NFUNC,IFUNC,
     2                    NPF ,TF  ,TT  ,DT1  ,BUFMAT(IADBUF),
     3                    NGL ,IBIDON1,IBIDON2,IBIDON3 ,IBIDON4,
     4                    EP1  ,EP2 ,EP3  ,EP4  ,EP5  ,EP6 ,
     5                    ES1  ,ES2 ,ES3  ,ES4  ,ES5  ,ES6 ,
     6                    SS1  ,SS2 ,SS3  ,SS4  ,SS5  ,SS6 ,    
     7                    DEFP ,DPLA,EPSP,UVARF,OFF  ,
     8                    DELTAX,VOLN,USER_UELR,BIDON4,BIDON5) 
            GBUF%UELR(1:LLT) = USER_UELR(1:LLT)
            ELSE
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/FAIL/USER2 - SOLID '
                LENGTH=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:LENGTH),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
            ENDIF
C   user3         
        ELSEIF(IRUPT==6)THEN
            IF (LOGICAL_USERL_AVAIL)THEN
               USER_UELR(1:LLT)=GBUF%UELR(1:LLT)
               CALL ENG_USERLIB_FLAW(IRUPT,
     1                    LLT ,NPARAM,NVARF,NFUNC,IFUNC,
     2                    NPF ,TF  ,TT  ,DT1  ,BUFMAT(IADBUF),
     3                    NGL ,IBIDON1,IBIDON2,IBIDON3 ,IBIDON4,
     4                    EP1  ,EP2 ,EP3  ,EP4  ,EP5  ,EP6 ,
     5                    ES1  ,ES2 ,ES3  ,ES4  ,ES5  ,ES6 ,
     6                    SS1  ,SS2 ,SS3  ,SS4  ,SS5  ,SS6 ,    
     7                    DEFP ,DPLA,EPSP,UVARF,OFF  ,
     8                    DELTAX,VOLN,USER_UELR,BIDON4,BIDON5) 
            GBUF%UELR(1:LLT) = USER_UELR(1:LLT)
            ELSE
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/FAIL/USER3 - SOLID '
                LENGTH=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:LENGTH),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
!!!
            ENDIF
C-------------------------------------------------------------------                                
          ELSEIF(IRUPT == 8)THEN                                         
C---- JC + spalling            
              CALL FAIL_SPALLING_S(LLT ,NPARAM,NVARF,NFUNC,IFUNC,
     2                     NPF ,TF  ,TT  ,DT1  ,BUFMAT(IADBUF),
     3                     NGL ,IPM ,MAT ,
     4                     SS1  ,SS2  ,SS3  ,SS4   ,SS5   ,SS6,
     5                     DPLA ,EPSP ,TSTAR,UVARF ,OFF   ,
     6                     DFMAX,TDEL ,LBUF%OFF)                     
C
          ELSEIF(IRUPT == 9)THEN                                         
               CALL FAIL_WIERZBICKI_S(LLT ,NPARAM,NVARF,NFUNC,IFUNC,
     2                     NPF ,TF  ,TT  ,DT1  ,BUFMAT(IADBUF),
     3                     NGL ,IPM ,MAT ,
     4                     SS1  ,SS2  ,SS3  ,SS4   ,SS5   ,SS6,
     5                     DPLA ,EL_PLA ,UVARF,OFF   ,DFMAX ,
     6                     TDEL )                          
          ELSEIF(IRUPT == 10)THEN                                        
c---- strain tension    
              CALL FAIL_TENSSTRAIN_S(LLT ,NPARAM,NVARF,NFUNC,IFUNC      ,
     2             NPF ,TF  ,TT  ,DT1  ,BUFMAT(IADBUF),
     3             NGL ,IPM ,MAT ,DELTAX   ,TSTAR ,ISMSTR,
     4             ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6     ,
     5             SS1 ,SS2 ,SS3 ,SS4  ,SS5  ,SS6     ,
     6             EPSP,UVARF,OFF,IP   ,DFMAX,TDEL    ,
     7             MFXX   ,MFXY   ,MFXZ   ,MFYX    ,MFYY   ,MFYZ   ,                 
     8             MFZX   ,MFZY   ,MFZZ   ,LBUF%DMGSCL)
c
          ELSEIF(IRUPT == 11)THEN                                                          
c---- energy failure  
            CALL FAIL_ENERGY_S(
     1        LLT      ,NPARAM   ,NVARF    ,NFUNC    ,IFUNC    ,NPF      ,
     2        TF       ,TT       ,DT1      ,BUFMAT(IADBUF),NGL ,EPSP     ,
     3        UVARF    ,OFF      ,IP       ,DFMAX    ,TDEL     ,LBUF%DMGSCL,            
     4        SS1      ,SS2      ,SS3      ,SS4      ,SS5      ,SS6      ,
     5        DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6      )
          ELSEIF (IRUPT == 13) THEN
C---- chang - chang   
            CALL FAIL_CHANGCHANG_S(
     1         LLT      ,NPARAM   ,NVARF    ,BUFMAT(IADBUF),UVARF,
     2         TT       ,IPG      ,ILAY     ,NPG      ,NGL      ,
     3         LBUF%DMGSCL,DFMAX    ,OFF      ,LBUF%OFF ,GBUF%NOFF,
     4         SS1      ,SS2      ,SS3      ,SS4      ,SS6      ,
     5         TDEL     )     
          ELSEIF(IRUPT == 14)THEN                                        
c --- Hashin    failure model
             DO I=1,NEL
              EPSP(I) = MAX(ABS(EP1(I)),ABS(EP2(I)),ABS(EP3(I)),EM20)
            ENDDO   
            CALL FAIL_HASHIN_S(
     1                  LLT    ,NVARF  ,IP    ,ILAY     ,NPG           ,       
     2                  NPF    ,TF     ,TT    ,DT1      ,BUFMAT(IADBUF), 
     3                  MAT    ,NGL    ,OFF   ,GBUF%NOFF,SS1           ,
     4                  SS2    ,SS3    ,SS4   ,SS5      ,SS6           ,
     5                  UVARF  ,IPM    ,NPARAM,DFMAX    ,TDEL          ,
     6                  EPSP ) 
          ELSEIF(IRUPT == 16)THEN                                        
c --- modified Puck failure model
            CALL FAIL_PUCK_S(LLT    ,NVARF  ,IP    ,ILAY     ,NPG           ,         
     2                  NPF    ,TF     ,TT    ,DT1      ,BUFMAT(IADBUF),
     3                  MAT    ,NGL    ,OFF   ,GBUF%NOFF,SS1           ,
     4                  SS2    ,SS3    ,SS4   ,SS5      ,SS6           ,
     5                  UVARF  ,IPM    ,NPARAM,DFMAX    ,TDEL     )                           
          ELSEIF(IRUPT == 18)THEN                                        
c --- ladeveze delamination damage model
            CALL FAIL_LADEVEZE(
     1                  LLT    ,NVARF  ,IP     ,ILAY     ,NPG           ,       
     2                  NPF    ,TF     ,TT     ,DT1      ,BUFMAT(IADBUF),    
     3                  MAT    ,NGL    ,OFF    ,GBUF%NOFF,SS1           ,      
     4                  SS2    ,SS3    ,SS4    ,SS5      ,SS6           , 
     5                  UVARF  ,IPM    ,NPARAM ,DFMAX    ,TDEL    )                           
          ELSEIF (IRUPT == 23) THEN                                      
c ---   tabulated failure model
              CALL FAIL_TAB_S(
     1        LLT      ,NVARF    ,NPF      ,TF       ,TT       ,         
     2        BUFMAT(IADBUF)     ,NGL      ,EL_LEN  ,         
     3        SS1      ,SS2      ,SS3      ,SS4      ,SS5      ,SS6     ,    
     4        DPLA     ,EPSP     ,TSTAR    ,UVARF    ,           
     5        OFF      ,TABLE    ,DFMAX    ,TDEL     ,
     6        NFUNC    ,IFUNC )                                     
          ELSEIF (IRUPT == 24) THEN                                      
c   --- Orthotropic strain failure
              CALL FAIL_ORTHSTRAIN(
     1        LLT      ,NPARAM   ,NVARF    ,NFUNC    ,IFUNC    ,
     2        NPF      ,TF       ,TT       ,DT1      ,BUFMAT(IADBUF),ISMSTR,   
     3        EP1      ,EP2      ,EP3      ,EP4      ,EP5      ,EP6     ,
     4        ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6     ,
     5        SS1      ,SS2      ,SS3      ,SS4      ,SS5      ,SS6     ,
     6        UVARF    ,OFF      ,IPG      ,NGL      ,DFMAX    ,TDEL    ,                   
     7        GBUF%UELR,NPG      ,DELTAX   )
          ELSEIF (IRUPT == 27) THEN                                      
c ---   extended mohr coulomb failure model                  
              CALL FAIL_EMC(
     1        LLT      ,NVARF    ,NPF      ,TF       ,TT       ,         
     2        DT1      ,BUFMAT(IADBUF)     ,NGL      ,IPM      ,MAT      ,         
     3        SS1      ,SS2      ,SS3      ,SS4      ,SS5      ,SS6  ,    
     4        EL_PLA   ,DPLA     ,EPSP     ,UVARF    ,           
     5        OFF      ,IP       ,DFMAX    ,TDEL     )                                       
          ELSEIF (IRUPT == 29) THEN                                      
c ---   MIT Wierzbicki Sahraei electric battery failure                                               
              CALL FAIL_SAHRAEI_S(
     1        LLT      ,NFUNC    ,IFUNC    ,NPF      ,TF       ,
     2        TT       ,NGL      ,IPM      ,MAT      ,BUFMAT(IADBUF)   ,
     3        ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6 ,  
     4        OFF      ,IP       ,DFMAX    ,TDEL     ,DELTAX   ,
     5        NVARF    ,UVARF    ) 
          ELSEIF (IRUPT == 30) THEN                                      
C  --- Biquadratic failure model             
              CALL FAIL_BIQUAD_S(
     1        LLT      ,NPARAM   ,NVARF    ,NFUNC    ,IFUNC    ,EL_LEN   ,
     2        NPF      ,TF       ,TT       ,BUFMAT(IADBUF),TDEL,      
     3        NGL      ,DPLA     ,UVARF    ,OFF      ,DFMAX    ,              
     4        SS1      ,SS2      ,SS3      ,SS4      ,SS5      ,SS6      )                            
          ELSEIF (IRUPT == 34) THEN                                      
C  --- Cockroft-Latham failure model             
              CALL FAIL_COCKROFT_S(LLT ,NPARAM,NVARF,
     2        TT      ,DT1       ,BUFMAT(IADBUF),
     3        NGL     ,
     4        EP1     ,EP2       ,EP3      ,EP4      ,EP5           ,EP6 ,
     5        ES1     ,ES2       ,ES3      ,ES4      ,ES5           ,ES6 ,
     6        SS1     ,SS2       ,SS3      ,SS4      ,SS5           ,SS6 ,    
     7        EL_PLA  ,DPLA      ,EPSP     ,UVARF    ,OFF           ,
     8        DFMAX   ,TDEL) 
          ELSEIF (IRUPT == 36) THEN                                      
C  --- VISUAL failure model             
              CALL FAIL_VISUAL_S(
     .        LLT     ,NPARAM    ,NVARF    ,TT       ,DT1       ,BUFMAT(IADBUF),
     .        ES1     ,ES2       ,ES3      ,ES4      ,ES5       ,ES6 ,
     .        SS1     ,SS2       ,SS3      ,SS4      ,SS5       ,SS6 ,
     .        UVARF   ,OFF       ,NGL      ,DFMAX    ,ISMSTR    ) 
c
          ELSEIF (IRUPT == 37) THEN                                      
c ---       tabulated failure model (old, obsolete version)
              CALL FAIL_TAB_OLD_S(
     1        LLT      ,NVARF    ,NPF      ,TF       ,TT       ,         
     2        BUFMAT(IADBUF)     ,NGL      ,IPM      ,MAT      ,EL_LEN   ,         
     3        SS1      ,SS2      ,SS3      ,SS4      ,SS5      ,SS6      ,    
     4        EL_PLA   ,DPLA     ,EPSP     ,TSTAR    ,UVARF    ,           
     5        OFF      ,IP       ,DFMAX    ,TDEL     ,
     6        NFUNC    ,IFUNC )   
c
          ELSEIF (IRUPT == 38) THEN                                      
C  --- Orthotropic biquadratic failure model             
              CALL FAIL_ORTHBIQUAD_S(
     1          LLT      ,NPARAM   ,NVARF    ,NFUNC    ,IFUNC    ,
     2          NPF      ,TF       ,TT       ,DT1      ,BUFMAT(IADBUF),
     3          NGL      ,DPLA     ,EPSP     ,UVARF    ,OFF      ,                   
     4          SS1      ,SS2      ,SS3      ,SS4      ,SS5      ,SS6      ,
     5          DFMAX    ,TDEL     ,EL_LEN   ) 
c
          ELSEIF (IRUPT == 39) THEN                                      
c  --- GENE1 failure model                                  
              CALL FAIL_GENE1_S(
     1          LLT      ,NPARAM   ,NVARF    ,NFUNC    ,IFUNC    ,LBUF%OFF ,
     2          NPF      ,TF       ,TT       ,DT1      ,BUFMAT(IADBUF),IPG ,
     3          NGL      ,GBUF%DT  ,EPSP     ,UVARF    ,OFF      ,NPG      ,     
     4          ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6      ,           
     5          SS1      ,SS2      ,SS3      ,SS4      ,SS5      ,SS6      ,
     6          TEMPEL   ,VOLN     ,DFMAX    ,TDEL     ,DELTAX   ,TABLE    ,
     7          IR       ,ELBUF_TAB(NG),ILAY )
c
          ELSEIF (IRUPT == 40) THEN                                      
c  --- RTCL failure model                                  
              CALL FAIL_RTCL_S(
     1          LLT      ,NPARAM   ,NVARF    ,TT       ,DT1      ,BUFMAT(IADBUF),
     2          SS1      ,SS2      ,SS3      ,SS4      ,SS5      ,SS6      ,
     3          NGL      ,DPLA     ,UVARF    ,OFF      ,DFMAX    ,TDEL     )
c
          ELSEIF (IRUPT == 41) THEN                                      
C---- Tabulated failure model version 2
              CALL FAIL_TAB2_S(
     1          LLT      ,NPARAM   ,NVARF    ,NFUNC    ,IFUNC    ,
     2          NPF      ,TABLE    ,TF       ,TT       ,BUFMAT(IADBUF), 
     3          NGL      ,EL_LEN   ,DPLA     ,EPSP     ,UVARF    ,     
     5          SS1      ,SS2      ,SS3      ,SS4      ,SS5      ,SS6      ,              
     6          TEMPEL   ,OFF      ,DFMAX    ,TDEL     ,LBUF%DMGSCL,
     7          GBUF%UELR,IPG      ,NPG      ,LBUF%OFF )   
c
          ELSEIF (IRUPT == 42) THEN                                      
C---- Inievo failure model
              CALL FAIL_INIEVO_S(
     1          LLT      ,NPARAM   ,NVARF    ,NFUNC    ,IFUNC    ,
     2          NPF      ,TABLE    ,TF       ,TT       ,BUFMAT(IADBUF), 
     3          NGL      ,EL_LEN   ,DPLA     ,EPSP     ,UVARF    ,     
     5          SS1      ,SS2      ,SS3      ,SS4      ,SS5      ,SS6      ,               
     6          EL_PLA   ,TEMPEL   ,SIGY     ,OFF      ,DFMAX    ,
     7          TDEL     ,LBUF%DMGSCL,GBUF%UELR,IPG      ,NPG      ,
     8          LBUF%OFF ,DAMINI   ,GBUF%VOL ,INLOC    )
c
          ELSEIF (IRUPT == 43) THEN                                      
C  --- Syazwan failure model             
            CALL FAIL_SYAZWAN_S(
     1        LLT     ,BUFMAT(IADBUF),NPARAM,UVARF    ,NVARF     ,
     2        TT      ,NGL         ,IPG      ,DPLA    ,TDEL      ,
     3        SS1     ,SS2         ,SS3      ,SS4     ,SS5       ,SS6      ,
     4        DFMAX   ,NFUNC       ,IFUNC    ,EL_LEN  ,OFF       ,
     5        NPF     ,TF          ,GBUF%UELR,NPG     ,LBUF%OFF  )
c
          ELSEIF (IRUPT == 44) THEN                                        
c --- Tsai-Wu failure model
            CALL FAIL_TSAIWU_S(
     1          LLT      ,NVARF    ,IPG      ,ILAY     ,NPG      ,TT       ,         
     2          DT1      ,BUFMAT(IADBUF),NGL ,OFF      ,LBUF%OFF ,GBUF%NOFF,
     3          SS1      ,SS2      ,SS3      ,SS4      ,SS5      ,SS6      ,
     5          UVARF    ,NPARAM   ,DFMAX    ,TDEL     ,LBUF%DMGSCL)
c
          ELSEIF (IRUPT == 45) THEN                                        
c --- Tsai-Hill failure model
            CALL FAIL_TSAIHILL_S(
     1          LLT      ,NVARF    ,IPG      ,ILAY     ,NPG      ,TT       ,         
     2          DT1      ,BUFMAT(IADBUF),NGL ,OFF      ,LBUF%OFF ,GBUF%NOFF,
     3          SS1      ,SS2      ,SS3      ,SS4      ,SS5      ,SS6      ,
     5          UVARF    ,NPARAM   ,DFMAX    ,TDEL     ,LBUF%DMGSCL)
c
          ELSEIF (IRUPT == 46) THEN                                        
c --- Hoffman failure model
            CALL FAIL_HOFFMAN_S(
     1          LLT      ,NVARF    ,IPG      ,ILAY     ,NPG      ,TT       ,         
     2          DT1      ,BUFMAT(IADBUF),NGL ,OFF      ,LBUF%OFF ,GBUF%NOFF,
     3          SS1      ,SS2      ,SS3      ,SS4      ,SS5      ,SS6      ,
     5          UVARF    ,NPARAM   ,DFMAX    ,TDEL     ,LBUF%DMGSCL)
c
          ELSEIF (IRUPT == 47) THEN                                        
c---- Maximum strain failure model
            CALL FAIL_MAXSTRAIN_S(
     1          LLT      ,NVARF    ,IPG      ,ILAY     ,NPG      ,TT       ,         
     2          DT1      ,BUFMAT(IADBUF),NGL ,OFF      ,LBUF%OFF ,GBUF%NOFF,
     3          ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6      , 
     4          UVARF    ,NPARAM   ,DFMAX    ,TDEL     ,LBUF%DMGSCL)
c
          ELSEIF (IRUPT == 48) THEN    
c---- Orthotropic energy failure
            CALL FAIL_ORTHENERG_S(
     1          LLT      ,NPARAM   ,NVARF    ,BUFMAT(IADBUF),UVARF,NGL     ,
     2          NPG      ,IPG      ,ILAY     ,OFF      ,LBUF%OFF ,GBUF%NOFF,
     3          DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6      ,
     4          SS1      ,SS2      ,SS3      ,SS4      ,SS5      ,SS6      ,
     5          TT       ,TDEL     ,DFMAX    ,DELTAX   ,LBUF%DMGSCL,IDEL7NOK)
c--------- 
          ENDIF ! IRUPT 
c
c--------------------------------------------------------
c         DAMAGED STRESSES
c---------------------------------------------------------
          ! -> Isotropic stress softening
          IF (DMG_FLAG == 1) THEN 
            DO I = 1,NEL
              SS1(I) = SS1(I)*LBUF%DMGSCL(I)
              SS2(I) = SS2(I)*LBUF%DMGSCL(I)
              SS3(I) = SS3(I)*LBUF%DMGSCL(I)
              SS4(I) = SS4(I)*LBUF%DMGSCL(I)
              SS5(I) = SS5(I)*LBUF%DMGSCL(I)
              SS6(I) = SS6(I)*LBUF%DMGSCL(I)
            ENDDO
          ! -> Orthotropic stress softening
          ELSEIF (DMG_FLAG == 6) THEN 
            DO I = 1,NEL
              SS1(I) = SS1(I)*LBUF%DMGSCL(NEL*(1-1) + I)
              SS2(I) = SS2(I)*LBUF%DMGSCL(NEL*(2-1) + I)
              SS3(I) = SS3(I)*LBUF%DMGSCL(NEL*(3-1) + I)
              SS4(I) = SS4(I)*LBUF%DMGSCL(NEL*(4-1) + I)
              SS5(I) = SS5(I)*LBUF%DMGSCL(NEL*(5-1) + I)
              SS6(I) = SS6(I)*LBUF%DMGSCL(NEL*(6-1) + I)
            ENDDO            
          ENDIF
C                                       
c---------
          IF(ISORTH  > 0 .AND. JCVT == 0) THEN  
             CALL MROTENS(1,NEL,
     .                 SS1 ,SS2 ,SS3 ,
     .                 SS4 ,SS5 ,SS6 ,
     .                 R11,R21,R31,
     .                 R12,R22,R32,
     .                 R13,R23,R33 )
          ENDIF
C
          DO I=1,NEL
            STOR1 = LBUF%SIG(NEL*(1-1) + I)    
            STOR2 = LBUF%SIG(NEL*(2-1) + I)
            STOR3 = LBUF%SIG(NEL*(3-1) + I)
            STOR4 = LBUF%SIG(NEL*(4-1) + I)
            STOR5 = LBUF%SIG(NEL*(5-1) + I)
            STOR6 = LBUF%SIG(NEL*(6-1) + I)
            E1=DXX(I)*(-STOR1+SS1(I))
            E2=DYY(I)*(-STOR2+SS2(I))
            E3=DZZ(I)*(-STOR3+SS3(I))
            E4=D4(I) *(-STOR4+SS4(I))
            E5=D5(I) *(-STOR5+SS5(I))  
            E6=D6(I) *(-STOR6+SS6(I))  
            EINT(I)=LBUF%EINT(I)*LBUF%VOL(I)
     .      +(E1+E2+E3+E4+E5+E6)*(VOLN(I)- HALF*DVOL(I))*DT1*HALF
            LBUF%EINT(I)=EINT(I)/MAX(LBUF%VOL(I),EM30)
          END DO
C
          DO I=1,NEL
            LBUF%SIG(NEL*(1-1) + I) = SS1(I)
            LBUF%SIG(NEL*(2-1) + I) = SS2(I)
            LBUF%SIG(NEL*(3-1) + I) = SS3(I)
            LBUF%SIG(NEL*(4-1) + I) = SS4(I)
            LBUF%SIG(NEL*(5-1) + I) = SS5(I)
            LBUF%SIG(NEL*(6-1) + I) = SS6(I) 
          ENDDO 
C
        ENDDO   !  IR = 1,NFAIL
C
C------------------------------------------------------------
C     Variable to regularize with Non-local
C------------------------------------------------------------
        IF (INLOC > 0) THEN 
          IF (ELBUF_TAB(NG)%BUFLY(ILAY)%L_PLA > 0) THEN 
            DO I = 1,NEL
              VARNL(I) = DEFP(I)
            ENDDO
          ENDIF
        ENDIF 
      ENDIF     !  NFAIL > 0  & USER LAWS
      IF ((ITASK==0).AND.(IMON_MAT==1))CALL STOPTIME(121,1)
C-----------------------------------------------------------------  
      IF(IPARTSPH/=0)THEN
       DO I=1,NEL
        IF(OFF(I) > ZERO .AND. OFF(I) < ONE)THEN
C replace Solid w/SPH within the same cycle.
          OFF(I)=ZERO
          LBUF%SIG(NEL*(1-1) + I)  = ZERO
          LBUF%SIG(NEL*(2-1) + I)  = ZERO
          LBUF%SIG(NEL*(3-1) + I)  = ZERO
          LBUF%SIG(NEL*(4-1) + I)  = ZERO
          LBUF%SIG(NEL*(5-1) + I)  = ZERO
          LBUF%SIG(NEL*(6-1) + I)  = ZERO
          IF (IVISC > 0) THEN
            LBUF%VISC(NEL*(1-1) + I) = ZERO
            LBUF%VISC(NEL*(2-1) + I) = ZERO
            LBUF%VISC(NEL*(3-1) + I) = ZERO
            LBUF%VISC(NEL*(4-1) + I) = ZERO
            LBUF%VISC(NEL*(5-1) + I) = ZERO
            LBUF%VISC(NEL*(6-1) + I) = ZERO
            IF (ISVIS > 0) THEN                      
c           viscous stress output                  
              LBUF%SIGV(NEL*(1-1) + I) = ZERO    
              LBUF%SIGV(NEL*(2-1) + I) = ZERO    
              LBUF%SIGV(NEL*(3-1) + I) = ZERO    
              LBUF%SIGV(NEL*(4-1) + I) = ZERO    
              LBUF%SIGV(NEL*(5-1) + I) = ZERO    
              LBUF%SIGV(NEL*(6-1) + I) = ZERO
            END IF        
          END IF
          STIFN(I)          = ZERO
          QVIS(I)           = ZERO
          SVIS(I,1)         = ZERO
          SVIS(I,2)         = ZERO
          SVIS(I,3)         = ZERO
          SVIS(I,4)         = ZERO
          SVIS(I,5)         = ZERO
          SVIS(I,6)         = ZERO
        END IF
       END DO
      END IF
C-----------------------------------------------------------------  
      IF(IEXPAN > 0 .OR. JTHE <0) THEN
         DO I=1,NEL
            IF(OFF(I) == ZERO) CYCLE
            LBUF%TEMP(I) = TEMPEL(I)
         ENDDO
       ENDIF
c     Flag Idel, test global sur changement de valeur de OFF
      IF(IDEL7NG>=1.OR.IDTMINS>=1) THEN
        DO I=1,NEL
c          IF(OFFOLD(I)>=1.AND.OFF(I)/=OFFOLD(I).AND.OFF(I)>=0.AND.OFF(I)<1)THEN
          IF(OFFOLD(I)>=1.AND.OFF(I)>=0.AND.OFF(I)<1)THEN
            IDEL7NOK = 1
          END IF
        END DO
      END IF
      IF (IMPL_S > 0) THEN
       CALL PUT_ETFAC(NEL ,ET  ,MTN)
       CALL PUTSIGNOR3(1,NEL ,MTN,IPTR,IPTS,IPTT,AL_IMP ,SIGNOR)
      END IF
C-----------  
C-----------------------------------------------------------------  
C  SOUND SPEED (SSP)  POST-TREATMENT
C-----------------------------------------------------------------
        IF(ELBUF_TAB(NG)%BUFLY(ILAY)%L_SSP /=0 )THEN
          LBUF%SSP(1:NEL) = CXX(1:NEL)
        ENDIF

C-----------------------------------------------------------------         
      RETURN
      END
C-----
      END MODULE MMAIN_MOD

