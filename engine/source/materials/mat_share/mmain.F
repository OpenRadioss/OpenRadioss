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
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
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
Chd|        ATUR17                        source/ale/atur17.F           
Chd|        ATURBN                        source/ale/aturbn.F           
Chd|        ENG_USERLIB_FLAW              source/user_interface/dyn_userlib.c
Chd|        EOSMAIN                       ../common_source/eos/eosmain.F
Chd|        EOSUPDA                       ../common_source/eos/eosupda.F
Chd|        FAIL_BIQUAD_S                 source/materials/fail/biquad/fail_biquad_s.F
Chd|        FAIL_COCKROFT_S               source/materials/fail/cockroft_latham/fail_cockroft_s.F
Chd|        FAIL_EMC                      source/materials/fail/emc/fail_emc.F
Chd|        FAIL_ENERGY_S                 source/materials/fail/energy/fail_energy_s.F
Chd|        FAIL_GENE1_S                  source/materials/fail/gene1/fail_gene1_s.F
Chd|        FAIL_HASHIN_S                 source/materials/fail/hashin/fail_hashin_s.F
Chd|        FAIL_JOHNSON                  source/materials/fail/johnson_cook/fail_johnson.F
Chd|        FAIL_LADEVEZE                 source/materials/fail/ladeveze/fail_ladeveze.F
Chd|        FAIL_ORTHBIQUAD_S             source/materials/fail/orthbiquad/fail_orthbiquad_s.F
Chd|        FAIL_ORTHSTRAIN               source/materials/fail/orthstrain/fail_orthstrain_s.F
Chd|        FAIL_PUCK_S                   source/materials/fail/puck/fail_puck_s.F
Chd|        FAIL_RTCL_S                   source/materials/fail/rtcl/fail_rtcl_s.F
Chd|        FAIL_SAHRAEI_S                source/materials/fail/sahraei/fail_sahraei_s.F
Chd|        FAIL_SPALLING_S               source/materials/fail/spalling/fail_spalling_s.F
Chd|        FAIL_TAB_S                    source/materials/fail/tabulated/fail_tab_s.F
Chd|        FAIL_TBUTCHER_S               source/materials/fail/tuler_butcher/fail_tbutcher_s.F
Chd|        FAIL_TENSSTRAIN_S             source/materials/fail/tensstrain/fail_tensstrain_s.F
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
Chd|        M1LAWI                        source/materials/mat/mat001/m1law.F
Chd|        M1LAWTOT                      source/materials/mat/mat001/m1law.F
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
Chd|        PUTSIGNOR3                    source/elements/solid/solide8z/mmats.F
Chd|        PUT_ETFAC                     source/elements/solid/solide8z/mmats.F
Chd|        SBOLTLAW                      source/elements/solid/solide/sboltlaw.F
Chd|        SESA10                        source/materials/mat/mat026/sesa10.F
Chd|        SESA20                        source/materials/mat/mat026/sesa20.F
Chd|        SESA30                        source/materials/mat/mat026/sesa30.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|        VISCMAIN                      source/materials/visc/viscmain.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE MMAIN(
     1           ELBUF_TAB,NG      ,PM      ,GEO     ,FBID    ,ALE_CONNECT   ,
     2           IX       ,IPARG   ,V       ,TF      ,NPF     ,BUFMAT  ,
     3           STIFN    ,X       ,DT2T    ,NELTST  ,ITYPTST ,OFFSET  ,
     4           NEL      ,W       ,OFF     ,PID     ,
     6           MAT      ,NGL     ,VOLN    ,VD2     ,DVOL    ,DELTAX  ,
     7           VIS      ,QVIS    ,CXX     ,S1      ,S2      ,S3      ,
     8           S4       ,S5      ,S6      ,DXX     ,DYY     ,DZZ     ,
     9           D4       ,D5      ,D6      ,WXX     ,WYY     ,WZZ     ,
     A           RX       ,RY      ,RZ      ,SX      ,SY      ,SZ      ,
     B           VDX      ,VDY     ,VDZ     ,MUMAX   ,SSP_EQ  ,AIRE    ,
     C           SIGY     ,ET      ,BUFVOIS ,DEFP    ,R3_DAM  ,R4_AMU ,
     D           MFXX     ,MFXY    ,MFXZ    ,MFYX    ,MFYY    ,MFYZ    , 
     E           MFZX     ,MFZY    ,MFZZ    ,IPM     ,GAMA    ,FR_WAV  ,
     F           DXY      ,DYX     ,DYZ     ,DZY     ,DZX     ,DXZ     ,
     G           ISTRAIN  ,TEMPEL  ,DIE     ,IEXPAN  ,ILAY    ,MSSA    ,
     H           DMELS    ,IPTR    ,IPTS    ,IPTT    ,TABLE   ,FVD2    ,
     I           FDELTAX  ,FSSP    ,FQVIS   ,IPARG1  ,IGEO   , CONDE   ,
     J           ITASK    ,NLOC_DMG,VARNL   ,MATPARAM_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE ELBUFDEF_MOD            
      USE NLOCAL_REG_MOD
      USE ALE_CONNECTIVITY_MOD
      USE MATPARAM_DEF_MOD
      USE MESSAGE_MOD
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
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr01_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "scr19_c.inc"
#include      "units_c.inc"
#include      "nsvis_c.inc"
#include      "secur_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
#include      "sms_c.inc"
#include      "userlib.inc"
#include      "timeri_c.inc"
#include      "thermal_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "nchara_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IX(*), IPARG(NPARG,NGROUP), NPF(*),PID(*),
     .        MAT(*)  ,NGL(*), IPM(NPROPMI,*),IPARG1(*),IGEO(NPROPGI,*)
      INTEGER NELTST, ITYPTST, OFFSET, NEL, ISTRAIN, 
     .        IEXPAN,NG,ILAY,IPTR,IPTS,IPTT,IBIDON1,IBIDON2,IBIDON3 ,
     .        IBIDON4, IADBUF, ITASK     
C
      my_real
     .   DT2T
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
      my_real
     .   DXY(*) ,DYX(*) ,DYZ(*) ,DZY(*) ,DZX(*) ,DXZ(*),FR_WAV(*),
     .   FQVIS(*),FVD2(*),FSSP(*),FDELTAX(*),CONDE(*)
      TYPE(TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_)  , TARGET :: NLOC_DMG 
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE(MATPARAM_STRUCT_)  , DIMENSION(NUMMAT) :: MATPARAM_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IC,IPG,NUVAR,NUVARR,NPARAM,NFUNC,IFLAG,IFUNC_ALPHA,ILAW,
     .   NVARF,NFAIL,IR,IP,IRUPT,IVISC,EOSTYP,NPTS,NPTT,NPTR,NPG,ISVIS,
     .   MX,NVARTMP,NLAY,INLOC,IDEV
      INTEGER IFUNC(MAXFUNC),II(6),ISELECT,IBPRELD
      my_real
     .   FACQ0,E1,E2,E3,E4,E5,E6,ALPHA, CT, REDUC,T0,TM
      my_real
     .   EPSD(MVSIZ),PNEW(MVSIZ),PSH(MVSIZ), P0(MVSIZ), OFFOLD(MVSIZ),
     .   AMU(MVSIZ), AMU2(MVSIZ), C1(MVSIZ), C2(MVSIZ), 
     .   C3(MVSIZ), C4(MVSIZ), C5(MVSIZ),C6(MVSIZ),
     .   EINC(MVSIZ), RHO0(MVSIZ),VOL_AVG(MVSIZ),
     .   SPH(MVSIZ), DF(MVSIZ), PC(MVSIZ),ESPE(MVSIZ),TMU(MVSIZ),
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
     .   BIDON1,BIDON2,BIDON3,BIDON4,BIDON5,
     .   BID1(MVSIZ), BID2(MVSIZ),ETH(MVSIZ),C11(MVSIZ),
     .   STOR1, STOR2, STOR3, STOR4, STOR5, STOR6,
     .   SIGKK(MVSIZ), EPSTH(MVSIZ), RHOREF(MVSIZ), RHOSP(MVSIZ),USER_UELR(MVSIZ)
      my_real
     .   ALOGEY(MVSIZ), POLD(MVSIZ),
     .   P01(MVSIZ), P02(MVSIZ), E01(MVSIZ), E02(MVSIZ),
     .   ER1V(MVSIZ), ER2V(MVSIZ), WDR1V(MVSIZ), WDR2V(MVSIZ),W1(MVSIZ),
     .   HEAT_MECA_L
      my_real, DIMENSION(:) ,POINTER :: STRD1,STRD2
     .  
       LOGICAL :: LOGICAL_USERL_AVAIL, l_MULAW_CALLED
!
      CHARACTER OPTION*256
      INTEGER SIZE
      CHARACTER IUSER_KEY*ncharline
C--------------------------------------------    
       my_real
     .  FINTER 
      EXTERNAL FINTER
C--------------------------------------------    
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(BUF_MAT_)  ,POINTER :: MBUF  
      TYPE(BUF_FAIL_) ,POINTER :: FBUF
      TYPE(BUF_VISC_) ,POINTER :: VBUF
      my_real,
     .  DIMENSION(:), POINTER  :: UVARF,DFMAX,TDEL 
c     Bolt Preloading
      INTEGER IBOLTP, NBPRELD
      my_real, 
     .  DIMENSION(:), POINTER :: BPRELD
!      my_real T1, T2, REDUC1
C=======================================================================
      LOGICAL_USERL_AVAIL = .FALSE.
      l_MULAW_CALLED=.FALSE.
      IF(USERL_AVAIL/=0) LOGICAL_USERL_AVAIL = .TRUE.
      GBUF => ELBUF_TAB(NG)%GBUF
      LBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IPTR,IPTS,IPTT)
      MBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%MAT(IPTR,IPTS,IPTT)
      FBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%FAIL(IPTR,IPTS,IPTT)
      VBUF => ELBUF_TAB(NG)%BUFLY(ILAY)%VISC(IPTR,IPTS,IPTT)
      ILAW  = ELBUF_TAB(NG)%BUFLY(ILAY)%ILAW
      NFAIL = ELBUF_TAB(NG)%BUFLY(ILAY)%NFAIL
      IVISC = ELBUF_TAB(NG)%BUFLY(ILAY)%IVISC
      NPTR  = ELBUF_TAB(NG)%NPTR
      NPTS  = ELBUF_TAB(NG)%NPTS
      NPTT  = ELBUF_TAB(NG)%NPTT
      NLAY  = ELBUF_TAB(NG)%NLAY
      NPG   = NPTS*NPTT*NPTR
      IPG   = IPTR + ((IPTS-1) + (IPTT-1)*NPTS)*NPTR
      IGTYP = IPARG(38,NG)
      IF ((IGTYP == 20).OR.(IGTYP == 21).OR.(IGTYP == 22)) THEN
        IPG  = IPTR + ((IPTS-1) + (ILAY-1)*NPTS)*NPTR
        NPG  = NPTR*NPTS*NLAY
      ENDIF
C
      IBOLTP = IPARG(72,NG)
      NBPRELD = GBUF%G_BPRELD
      BPRELD =>GBUF%BPRELD(1:NBPRELD*NEL)
C-----

      DO I=1,6
        II(I) = NEL*(I-1)
      ENDDO

C Flag Idel
      IF(IDEL7NG.GE.1.OR.IDTMINS.GE.1) THEN
        DO I=LFT,LLT
          OFFOLD(I)=OFF(I)
        END DO
      END IF
C
      IF (JTHE < 0) THEN
           DIE(LFT:LLT) = LBUF%EINT(LFT:LLT)
           DPLA(LFT:LLT) = ZERO
      ELSE
          TSTAR(LFT:LLT) = ZERO
          DPLA(LFT:LLT) = ZERO
      ENDIF        
C tangent module ratio (often w.r.t. G )
      DO I=LFT,LLT
        ET(I)=UN
      ENDDO
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
              ET(LFT:LLT)=ZERO
         END IF
      END IF
C needed in mqviscb
      FACQ0 =  UN
      ISVIS = IGEO(35,PID(1))
      IF((IMPL_S > 0 .AND. IDYNA == 0).OR. MTN == 24) ISVIS = 0
C                  
      RHO0(LFT:LLT)  = PM(1,MAT(1))
      SVIS(LFT:LLT,1)= ZERO
      SVIS(LFT:LLT,2)= ZERO
      SVIS(LFT:LLT,3)= ZERO
      SVIS(LFT:LLT,4)= ZERO
      SVIS(LFT:LLT,5)= ZERO
      SVIS(LFT:LLT,6)= ZERO 
C
      IF(IPARG(64,NG).NE.0)THEN
        DF(LFT:LLT)      = UN
        VOL_AVG(LFT:LLT) = VOLN(LFT:LLT)
      ELSE
          DF(LFT:LLT)  =  RHO0(LFT:LLT)/LBUF%RHO(LFT:LLT)
          VOL_AVG(LFT:LLT) = VOLN(LFT:LLT) - UNDEMI*DVOL(LFT:LLT)
      ENDIF
C-----due to too much returns of QA, temporaly add IRESP here   
      IF((INCOMP/=1.OR.(JEUL+JALE)/=1).AND.JLAG/=0.AND.N2D==0.AND.JSPH==0.AND.IRESP==1
     .     .AND.IMPL_S==0.AND.ISMSTR/=1.AND.ISMSTR/=3.AND.ISMSTR/=11)THEN
C-----due to issue w/ SP ----in srho3.F R4_AMU is calculated for SP :      
C--    R4_AMU =VOL0DP/VOLDP -1   because RHO*VOLDP=RHO0*VOL0DP   
          AMU(LFT:LLT) =  R4_AMU(LFT:LLT)
      ELSE
       IF(MTN == 1 .OR. MTN == 2 .OR. MTN == 22.OR.(MTN >=28.AND.MTN /=45)) THEN     ! for compatibility with QA tests
          AMU(LFT:LLT) =  LBUF%RHO(LFT:LLT)/RHO0(LFT:LLT)-UN
       ELSE
          AMU(LFT:LLT) =  UN/DF(LFT:LLT)-UN
       END IF 
       IF(ISMSTR==12)THEN
        DO I=LFT,LLT
          IF(ABS(LBUF%OFF(I))>UN) AMU(I) =  R4_AMU(I)
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
        RHOREF(LFT:LLT)=RHO0(LFT:LLT)
      ELSEIF(ISMSTR==2 .OR. ISMSTR==12)THEN
        DO I=LFT,LLT
          IF(ABS(LBUF%OFF(I))<=UN)THEN
            RHOREF(I)=LBUF%RHO(I)
          ELSE
            RHOREF(I)=RHO0(I)*LBUF%VOL(I)/MAX(EM20,VOLN(I))
            IF (ISMSTR==12) AMU(I) =  R4_AMU(I)
          END IF
        END DO
      ELSE ! Large Strain, ISMSTR==4 .OR. ISMSTR==10
        RHOREF(LFT:LLT)=LBUF%RHO(LFT:LLT)
      END IF
      RHOSP(LFT:LLT) =LBUF%RHO(LFT:LLT)
C --------------------------------------------------------
C    THERMAL MATERIAL ISTROPE EXPANSION 
C --------------------------------------------------------

      IF((IEXPAN > 0).AND.(JTHE < 0).AND.(TT.NE.ZERO)) THEN
        DO I=LFT,LLT
           IFUNC_ALPHA = IPM(219,MAT(1))
           FSCAL_ALPHA = PM(191,MAT(1))
           TEMPEL0(I)  = LBUF%TEMP(I)
           ALPHA = FINTER(IFUNC_ALPHA,TEMPEL(I),NPF,TF,BIDON1)
           ALPHA = ALPHA * FSCAL_ALPHA
           ETH(I)= ALPHA *(TEMPEL(I)-TEMPEL0(I))*OFF(I) 
           LBUF%FORTH(I) = LBUF%FORTH(I) + ETH(I)  ! LBUF%FORTH contient la deformation thermique totale
           EPSTH(I)= TROIS*LBUF%FORTH(I)
           DXX(I)  = DXX(I)-ETH(I)/DT1
           DYY(I)  = DYY(I)-ETH(I)/DT1
           DZZ(I)  = DZZ(I)-ETH(I)/DT1
           DVOL(I) = DVOL(I)-TROIS*ETH(I)*VOL_AVG(I)
           AMU(I)  = AMU(I) + EPSTH(I)
           SIGKK(I)= LBUF%SIG(II(1)+I)+LBUF%SIG(II(2)+I)+LBUF%SIG(II(3)+I)
           LBUF%EINTTH(I) = LBUF%EINTTH(I)-UNDEMI*SIGKK(I)*ETH(I)
        ENDDO
      ELSE
        DO I=LFT,LLT
           EPSTH(I)= ZERO
        ENDDO
      ENDIF 	
c-----Tstar computation for Jonhson Cook failure : T* = (T-T0)/(TM-T0)----
      IF (JTHE < 0) THEN
         MX = MAT(1)
         T0 = PM(79, MX) 
         TM = PM(80, MX) 
         DO I=LFT,LLT
            TSTAR(I)=MAX(ZERO,(TEMPEL(I)-T0)/(TM-T0))
         ENDDO
      ENDIF 
C-----------------------------------
C     EOS PART1
C-----------------------------------
      IF (MTN == 3.OR.MTN == 4.OR.MTN == 6.OR.MTN.EQ.10.OR.MTN.EQ.17.OR.MTN == 49.OR.MTN == 102.OR.MTN == 103) THEN
        EOSTYP= IPM(4,MAT(1))
        CALL EOSMAIN(0       ,NEL      ,EOSTYP  ,PM        ,OFF      ,LBUF%EINT,
     2               LBUF%RHO,RHO0     ,AMU     ,AMU2      ,ESPE     ,
     3               DVOL    ,DF       ,VOLN    ,MAT       ,PSH      ,
     4               PNEW    ,DPDM     ,DPDE    ,LBUF%TEMP ,ECOLD,
     5               BUFMAT  ,LBUF%SIG ,LBUF%MU ,MTN       ,POLD)
      ENDIF
C-----------------------------------
C     STRESSES DEVIATORIC/TOTAL
C-----------------------------------
      IBPRELD = 0
      IF(IBOLTP /= 0)THEN
        !two conditions checks, otherwise BPRELD has size 0 if IBOLTP==0 and debuger stops as it detects an index out of bounds.
        IF(BPRELD(2*NEL+1) <= UN .AND.BPRELD(2*NEL+1) > ZERO)IBPRELD = 1
      ENDIF

      IF(IBOLTP /= 0 .AND. IBPRELD == 1)THEN       
       CALL SBOLTLAW(PM       ,LBUF%SIG ,MAT      ,DXX   ,
     1               DYY      ,DZZ      ,D4       ,D5    ,
     2               D6       ,NEL      ,LBUF%RHO ,BPRELD,
     3               LBUF%EINT,LBUF%QVIS,LBUF%VOL ,STIFN    ,
     4               DT2T     ,NELTST   ,ITYPTST  ,LBUF%OFF ,
     5               GEO      ,PID      ,
     7               MUMAX    ,NGL      ,CXX      ,DVOL    ,
     8               AIRE     ,VOLN     ,VD2      ,DELTAX  ,
     9               VIS      ,PNEW     ,PSH      ,QVIS    , 
     A               SSP_EQ   ,S1       ,S2       ,S3      ,
     B               S4       ,S5       ,S6       ,MSSA    ,
     C               DMELS    ,CONDE    ,AMU      ,VOL_AVG ,
     D               GBUF%DT  ,GBUF%G_DT,OFF      ,IPM     ,
     E               RHOREF   ,RHOSP    ,LBUF%VOL0DP)
      ELSE IF (MTN == 1) THEN
C
        IF (JHBE==17.AND.IINT==3.AND.ISMSTR == 1) THEN
          CALL M1LAWI(PM       ,OFF      ,LBUF%SIG ,LBUF%EINT,LBUF%RHO,
     2               LBUF%QVIS,LBUF%VOL ,STIFN    ,DT2T     ,NELTST  ,
     3               ITYPTST  ,LBUF%OFF ,GEO      ,PID      ,
     5               MUMAX  ,MAT     ,NGL         ,CXX     ,DVOL    ,
     6               AIRE   ,VOLN    ,VD2         ,DELTAX  ,VIS     ,  
     7               DXX    ,DYY     ,DZZ         ,D4      ,D5      ,
     8               D6     ,PNEW    ,PSH         ,QVIS    ,SSP_EQ  , 
     9               S1     ,S2      ,S3          ,S4      ,S5      ,
     A               S6     ,MSSA    ,DMELS       ,CONDE   ,AMU     ,
     B               VOL_AVG,GBUF%DT ,GBUF%G_DT   ,NEL     ,IPM     ,
     C               RHOREF ,RHOSP  )
        ELSE IF(ISMSTR >= 10.AND.ISMSTR <= 12)THEN
          CALL M1LAWTOT(PM       ,OFF      ,LBUF%SIG ,LBUF%EINT,LBUF%RHO,
     2               LBUF%QVIS,LBUF%VOL ,STIFN    ,DT2T     ,NELTST  ,
     3               ITYPTST  ,LBUF%OFF ,GEO      ,PID      ,AMU     ,
     5               MUMAX  ,MAT     ,NGL     ,CXX     ,DVOL    ,
     6               AIRE   ,VOLN    ,VD2     ,DELTAX  ,VIS     ,  
     7               DXX    ,DYY     ,DZZ     ,D4      ,D5      ,
     8               D6     ,PNEW    ,PSH     ,QVIS    ,SSP_EQ  , 
     9               S1     ,S2      ,S3      ,S4      ,S5      ,
     A               S6     ,MSSA    ,DMELS   ,CONDE   ,MFXX    ,
     B               MFXY   ,MFXZ    ,MFYX    ,MFYY    ,MFYZ    , 
     C               MFZX   ,MFZY    ,MFZZ    ,GBUF%OFF,VOL_AVG ,
     D               EPSTH  ,GBUF%DT ,GBUF%G_DT,NEL ,GBUF%ETOTSH,
     E               ISELECT,IPM     ,RHOREF ,RHOSP  ,LBUF%SIGL )
        ELSE 
          CALL M1LAW(PM       ,OFF      ,LBUF%SIG ,LBUF%EINT,LBUF%RHO,
     2               LBUF%QVIS,LBUF%VOL ,STIFN    ,DT2T     ,NELTST  ,
     3               ITYPTST  ,LBUF%OFF ,GEO      ,PID      ,
     5               MUMAX  ,MAT     ,NGL         ,CXX     ,DVOL    ,
     6               AIRE   ,VOLN    ,VD2         ,DELTAX  ,VIS     ,  
     7               DXX    ,DYY     ,DZZ         ,D4      ,D5      ,
     8               D6     ,PNEW    ,PSH         ,QVIS    ,SSP_EQ  , 
     9               S1     ,S2      ,S3          ,S4      ,S5      ,
     A               S6     ,MSSA    ,DMELS       ,CONDE   ,AMU     ,
     B               VOL_AVG,GBUF%DT ,GBUF%G_DT   ,NEL     ,IPM     ,
     C               RHOREF ,RHOSP  )
        END IF
C
      ELSEIF (MTN == 2) THEN
C
        CALL M2LAW(PM       ,OFF      ,LBUF%SIG ,LBUF%EINT,LBUF%RHO,
     2             LBUF%QVIS,LBUF%PLA ,LBUF%EPSD,LBUF%VOL ,STIFN   ,
     3             DT2T     ,NELTST   ,ITYPTST  ,LBUF%OFF ,GEO     ,
     4             PID      ,AMU      ,VOL_AVG  ,
     6             MUMAX  ,MAT     ,NGL     ,CXX        ,DVOL    ,
     7             AIRE   ,VOLN    ,VD2     ,DELTAX     ,VIS     ,  
     8             DXX    ,DYY     ,DZZ     ,D4         ,D5      ,
     9             D6     ,PNEW    ,PSH     ,QVIS       ,SSP_EQ  ,
     A             S1     ,S2      ,S3      ,S4         ,S5      ,
     B             S6     ,JPLASOL ,SIGY    ,DEFP       ,DPLA    ,
     C             EPSP   ,TSTAR   ,ET      ,MSSA       ,DMELS   ,
     D             TEMPEL ,LBUF%SIGB,AL_IMP ,SIGNOR     ,CONDE   ,
     E             GBUF%DT,GBUF%G_DT ,NEL   ,IPM        ,RHOREF  ,
     F             RHOSP   ) 
      ELSEIF (MTN == 3) THEN
        CALL M3LAW(PM       ,OFF      ,LBUF%SIG ,LBUF%PLA,MAT     ,
     2             NGL      ,CXX      ,DXX      ,DYY     ,DZZ     ,
     3             D4       ,D5       ,D6       ,RHO0    ,DPDM    , 
     4             JPLASOL  ,SIGY     ,DEFP     ,DPLA    ,NEL     )
        IF (JSPH == 0) THEN
          CALL MQVISCB(PM,OFF    ,LBUF%RHO ,LBUF%RK  ,LBUF%TEMP ,CXX    ,
     2         LBUF%RE  ,STIFN   ,DT2T     ,NELTST   ,ITYPTST   ,AIRE   ,
     3         LBUF%OFF ,GEO     ,PID      ,VOLN     ,VD2       ,DELTAX ,  
     4         VIS      ,DXX     ,DYY      ,DZZ      ,PNEW      ,PSH    ,
     5         MAT      ,NGL     ,QVIS     ,SSP_EQ   ,LBUF%VOL  ,MSSA    ,
     6         DMELS    ,IGEO    ,FACQ0    ,CONDE    ,GBUF%DT   ,GBUF%G_DT,
     7         IPM      ,RHOREF  ,RHOSP   )
        ELSE
          CALL MDTSPH(PM ,OFF     ,LBUF%RHO,LBUF%RK   ,LBUF%TEMP,
     2         LBUF%RE   ,STIFN   ,DT2T    ,NELTST    ,ITYPTST  ,
     3         LBUF%OFF  ,GEO     ,PID     ,MUMAX     ,CXX      ,
     4         VOLN      ,VD2     ,DELTAX  ,VIS       ,DXX      ,  
     5         DYY       ,DZZ     ,PNEW    ,PSH       ,MAT      ,
     6         NGL       ,QVIS    ,SSP_EQ  ,GBUF%G_DT ,GBUF%DT )
        ENDIF
        CALL MEINT(OFF    ,LBUF%SIG,LBUF%QVIS,LBUF%EINT,VOLN   ,ESPE   ,
     2             S1     ,S2     ,S3     ,S4     ,S5   ,S6   ,
     3             DXX    ,DYY    ,DZZ    ,D4     ,D5   ,D6   , 
     4             PSH    ,DVOL   ,DF     ,QVIS   ,PNEW ,VIS  ,
     5             TMU    ,EINC   ,MTN    ,VOL_AVG,NEL  ) 
C
      ELSEIF (MTN == 4) THEN
        CALL M4LAW(PM       ,OFF      ,LBUF%SIG ,LBUF%PLA,MAT     ,
     2             LBUF%TEMP,CXX      ,DXX      ,DYY     ,DZZ     ,
     3             D4       ,D5       ,D6       ,RHO0    ,DPDM    ,
     4             LBUF%EPSD,JPLASOL  ,SIGY     ,DEFP    ,DPLA    ,
     5             EPSP     ,TSTAR    ,TEMPEL   ,NEL     )
        IF (JSPH == 0) THEN
          CALL MQVISCB(PM,OFF    ,LBUF%RHO ,LBUF%RK  ,LBUF%TEMP ,CXX      ,
     2         LBUF%RE  ,STIFN   ,DT2T     ,NELTST   ,ITYPTST   ,AIRE     ,
     3         LBUF%OFF ,GEO     ,PID      ,VOLN     ,VD2       ,DELTAX   ,
     4         VIS      ,DXX     ,DYY      ,DZZ      ,PNEW      ,PSH      ,
     5         MAT      ,NGL     ,QVIS     ,SSP_EQ   ,LBUF%VOL ,MSSA      ,
     6         DMELS    ,IGEO    ,FACQ0    ,CONDE    ,GBUF%DT  ,GBUF%G_DT ,
     7         IPM      ,RHOREF  ,RHOSP   )
        ELSE
          CALL MDTSPH(PM ,OFF     ,LBUF%RHO,LBUF%RK   ,LBUF%TEMP,
     2         LBUF%RE   ,STIFN   ,DT2T    ,NELTST    ,ITYPTST  ,
     3         LBUF%OFF  ,GEO     ,PID     ,MUMAX     ,CXX      ,
     4         VOLN    ,VD2     ,DELTAX    ,VIS       ,DXX     ,  
     5         DYY     ,DZZ     ,PNEW      ,PSH       ,MAT     ,
     6         NGL     ,QVIS    ,SSP_EQ    ,GBUF%G_DT ,GBUF%DT )
        ENDIF
        CALL MEINT(OFF   ,LBUF%SIG ,LBUF%QVIS,LBUF%EINT ,VOLN   ,ESPE   ,
     2            S1     ,S2       ,S3       ,S4        ,S5     ,S6     ,
     3            DXX    ,DYY      ,DZZ      ,D4        ,D5     ,D6     , 
     4            PSH    ,DVOL     ,DF       ,QVIS      ,PNEW   ,VIS    ,
     5            TMU    ,EINC     ,MTN      ,VOL_AVG   ,NEL  )
C
      ELSEIF (MTN == 5) THEN
        CALL M5LAW(PM   ,LBUF%SIG ,LBUF%EINT  ,LBUF%RHO ,PSH       ,       
     1             P0   ,LBUF%TB  ,LBUF%BFRAC ,VOLN     ,DELTAX    ,       
     2             MAT  ,NEL      ,CXX        ,DF       ,                  
     3             ER1V ,ER2V     ,WDR1V      ,WDR2V    ,W1        ,       
     4             RHO0, AMU)
        IF (JSPH == 0) THEN
          CALL MQVISCB(PM ,OFF    ,LBUF%RHO ,LBUF%RK ,LBUF%TEMP ,CXX       ,
     2         LBUF%RE    ,STIFN  ,DT2T     ,NELTST  ,ITYPTST   ,AIRE      ,
     3         LBUF%OFF   ,GEO    ,PID      ,VOLN    ,VD2       ,DELTAX    , 
     5         VIS        ,DXX    ,DYY      ,DZZ     ,PNEW      ,PSH       ,
     6         MAT        ,NGL    ,QVIS     ,SSP_EQ  ,LBUF%VOL  ,MSSA      ,
     7         DMELS      ,IGEO   ,FACQ0    ,CONDE   ,GBUF%DT   ,GBUF%G_DT ,
     7         IPM        ,RHOREF ,RHOSP   )
        ELSE
          CALL MDTSPH(PM ,OFF     ,LBUF%RHO  ,LBUF%RK   ,LBUF%TEMP,
     2         LBUF%RE   ,STIFN   ,DT2T      ,NELTST    ,ITYPTST  ,
     3         LBUF%OFF  ,GEO     ,PID       ,MUMAX     ,CXX      ,
     4         VOLN    ,VD2       ,DELTAX    ,VIS       ,DXX      ,  
     5         DYY     ,DZZ       ,PNEW      ,PSH       ,MAT      ,
     6         NGL     ,QVIS      ,SSP_EQ    ,GBUF%G_DT ,GBUF%DT )
        ENDIF
        CALL MJWL(PM     ,MAT     ,OFF      ,LBUF%SIG   ,LBUF%EINT ,
     1            PSH    ,P0      ,LBUF%QVIS,LBUF%VOL   ,LBUF%BFRAC,
     2            VOLN   ,QVIS    ,S1       ,S2         ,S3        ,
     3            DVOL   ,NEL     ,DF       ,LBUF%ABURN ,
     4            ER1V   ,ER2V    ,WDR1V    ,WDR2V      ,W1, AMU)

      ELSEIF (MTN == 6) THEN
        CALL M6LAW(PM  ,OFF     ,LBUF%SIG,LBUF%EINT,LBUF%RHO,LBUF%RK ,
     2         LBUF%RE ,LBUF%VK ,VOLN    ,RHO0     ,VIS     ,PSH     ,
     3         DXX     ,DYY     ,DZZ     ,D4       ,D5      ,D6      ,
     4         C1      ,C2      ,C3      ,C4       ,C5      ,C6      , 
     5         PC      ,AMU     ,AMU2    ,ESPE     ,CXX     ,DF      ,
     6         TMU     ,MAT     ,WXX     ,WYY      ,WZZ     ,NEL     ,
     7         DPDM)
        IF (JSPH == 0) THEN
          CALL MQVISCB(PM ,OFF     ,LBUF%RHO ,LBUF%RK  ,LBUF%TEMP ,CXX       ,
     2         LBUF%RE    ,STIFN   ,DT2T     ,NELTST   ,ITYPTST   ,AIRE      ,
     3         LBUF%OFF   ,GEO     ,PID      ,VOLN     ,VD2       ,DELTAX    ,
     4         VIS        ,DXX     ,DYY      ,DZZ      ,PNEW      ,PSH       ,
     5         MAT        ,NGL     ,QVIS     ,SSP_EQ   ,LBUF%VOL  ,MSSA      ,
     6         DMELS      ,IGEO    ,FACQ0    ,CONDE    ,GBUF%DT   ,GBUF%G_DT ,
     7         IPM        ,RHOREF  ,RHOSP   )
        ELSE
          CALL MDTSPH(PM ,OFF     ,LBUF%RHO,LBUF%RK   ,LBUF%TEMP,
     2         LBUF%RE   ,STIFN   ,DT2T    ,NELTST    ,ITYPTST  ,
     3         LBUF%OFF  ,GEO     ,PID     ,MUMAX     ,CXX      ,
     4        VOLN    ,VD2     ,DELTAX     ,VIS       ,DXX     ,  
     5        DYY     ,DZZ     ,PNEW       ,PSH       ,MAT     ,
     6        NGL     ,QVIS    ,SSP_EQ     ,GBUF%G_DT ,GBUF%DT  )
        ENDIF
        CALL MEINT(OFF    ,LBUF%SIG,LBUF%QVIS,LBUF%EINT,VOLN   ,ESPE   ,
     2            S1     ,S2     ,S3     ,S4     ,S5   ,S6   ,
     3            DXX    ,DYY    ,DZZ    ,D4     ,D5   ,D6   , 
     4            PSH    ,DVOL   ,DF     ,QVIS   ,PNEW ,VIS  ,
     5            TMU    ,EINC   ,MTN    ,VOL_AVG,NEL  )
        IF(JTUR.NE.0)CALL ATURBN (
     1            PM     ,OFF    ,LBUF%RHO,LBUF%RK,LBUF%RE ,GEO   , 
     2            EINC   ,DVOL   ,VOLN   ,PNEW    ,TMU     ,VIS   ,
     3            VD2    ,MAT    ,PID    )
        IF(JTHE == 1)CALL MTHETA(PM,LBUF%EINT,LBUF%TEMP,AMU, C1, C2,
     2                         DF,PSH    ,PC      ,MAT)
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
          CALL MQVISCB(PM,OFF    ,LBUF%RHO ,LBUF%RK  ,LBUF%TEMP ,CXX      ,
     2         LBUF%RE  ,STIFN   ,DT2T     ,NELTST   ,ITYPTST   ,AIRE     ,
     3         LBUF%OFF ,GEO     ,PID      ,VOLN     ,VD2       ,DELTAX   ,  
     5         VIS      ,DXX     ,DYY      ,DZZ      ,BID1      ,BID2     ,
     6         MAT      ,NGL     ,QVIS     ,SSP_EQ   ,LBUF%VOL  ,MSSA     ,
     7         DMELS    ,IGEO    ,FACQ0    ,CONDE    ,GBUF%DT  ,GBUF%G_DT ,
     7         IPM      ,RHOREF  ,RHOSP   )
        ELSE
          CALL MDTSPH(PM ,OFF     ,LBUF%RHO ,LBUF%RK ,LBUF%TEMP,
     2         LBUF%RE   ,STIFN   ,DT2T     ,NELTST  ,ITYPTST  ,
     3         LBUF%OFF  ,GEO     ,PID      ,MUMAX   ,CXX      ,
     4        VOLN    ,VD2     ,DELTAX      ,VIS     ,DXX     ,  
     5        DYY     ,DZZ     ,BID1        ,BID2    ,MAT     ,
     6        NGL     ,QVIS    ,SSP_EQ      ,GBUF%G_DT ,GBUF%DT )
        ENDIF
        CALL MEINT(OFF    ,LBUF%SIG,LBUF%QVIS,LBUF%EINT,VOLN   ,ESPE   ,
     2              S1     ,S2     ,S3     ,S4     ,S5   ,S6   ,
     3              DXX    ,DYY    ,DZZ    ,D4     ,D5   ,D6   , 
     4              PSH    ,DVOL   ,DF     ,QVIS   ,PNEW ,VIS  ,
     5              TMU    ,EINC   ,MTN    ,VOL_AVG,NEL  ) 
      ELSEIF (MTN == 11) THEN      
        CALL M11LAW(PM      ,OFF     ,LBUF%SIG,LBUF%EINT,LBUF%RHO,
     2              LBUF%TEMP,LBUF%RK,LBUF%RE ,LBUF%VK  ,
     3              ALE_CONNECT   ,IX      ,IPARG   ,ELBUF_TAB,V       ,
     4              X       ,LBUF%EINS,STIFN  ,W        ,VD2     ,
     5              VDX     ,VDY     ,VDZ     ,VIS      ,MAT     ,
     6              VOLN    ,QVIS    ,RHO0    ,BUFVOIS  ,IPM     ,
     7              NPF     ,TF      ,SSP_EQ  ,GBUF%MOM ,NEL     )
        IF(IPARG(7,NG)+IPARG(11,NG) > 0 ) THEN
          CXX(LFT:LLT) = SSP_EQ(LFT:LLT)
        ELSE
          LBUF%SSP(LFT:LLT) = SSP_EQ(LFT:LLT)
        ENDIF
      ELSEIF (MTN == 12) THEN
C       TSAI 3D Complet
        EOSTYP= IPM(4,MAT(1))
        STRD1(1:NEL*3) => LBUF%EPE(1:NEL*3)
        STRD2(1:NEL*3) => LBUF%EPE(1+NEL*3:NEL*6)
        
        CALL M12LAW(PM     ,OFF      ,LBUF%SIG,LBUF%EINT,LBUF%PLA,
     2            LBUF%SIGF,LBUF%EPSF,LBUF%DAM,STRD1    ,STRD2   ,
     3            GAMA     ,LBUF%VOL ,RX      ,RY       ,RZ      ,
     4            SX       ,SY       ,SZ      ,MAT      ,VOLN    ,
     5            DVOL     ,CXX      ,DXX     ,DYY      ,DZZ     , 
     6            D4       ,D5       ,D6      ,S1       ,S2      ,
     7            S3       ,S4       ,S5      ,S6       ,SIGY    ,
     8            DEFP     ,NGL      ,LBUF%SEQ,NEL      ,EOSTYP  ,	 
     9            RHO0     ,AMU      ,AMU2    ,ESPE     ,DF      ,
     A            PSH      ,PNEW     ,DPDM    ,DPDE     ,LBUF%RHO,
     B            LBUF%TEMP,ECOLD    ,BUFMAT  )
        IF (JSPH == 0) THEN
          CALL MQVISCB(PM,OFF    ,LBUF%RHO ,LBUF%RK ,LBUF%TEMP ,CXX       ,
     2         LBUF%RE  ,STIFN   ,DT2T     ,NELTST  ,ITYPTST   ,AIRE      ,
     3         LBUF%OFF ,GEO     ,PID      ,VOLN    ,VD2       ,DELTAX    , 
     5         VIS      , DXX    ,DYY      ,DZZ     ,PNEW      ,PSH       ,
     6         MAT      ,NGL     ,QVIS     ,SSP_EQ  ,LBUF%VOL  ,MSSA      ,
     7         DMELS    ,IGEO    ,FACQ0    ,CONDE   ,GBUF%DT   ,GBUF%G_DT ,
     7         IPM      ,RHOREF  ,RHOSP   )
        ELSE
          CALL MDTSPH(PM ,OFF     ,LBUF%RHO,LBUF%RK   ,LBUF%TEMP,
     2         LBUF%RE   ,STIFN   ,DT2T    ,NELTST    ,ITYPTST  ,
     3         LBUF%OFF  ,GEO     ,PID     ,MUMAX     ,CXX      ,
     4        VOLN    ,VD2     ,DELTAX     ,VIS       ,DXX     ,  
     5        DYY     ,DZZ     ,PNEW       ,PSH       ,MAT     ,
     6        NGL     ,QVIS    ,SSP_EQ     ,GBUF%G_DT ,GBUF%DT )
        ENDIF     
      ELSEIF (MTN == 13) THEN
        CALL M13LAW(PM      ,OFF     ,LBUF%SIG,LBUF%EINT,LBUF%RHO,
     .              LBUF%QVIS,LBUF%VOL,LBUF%PLA,STIFN   )
      ELSEIF (MTN == 14) THEN
        STRD1(1:NEL*3) => LBUF%EPE(1:NEL*3)
        STRD2(1:NEL*3) => LBUF%EPE(1+NEL*3:NEL*6)
        CALL M14LAW(PM       ,OFF      ,LBUF%SIG,LBUF%EINT,LBUF%PLA,
     2              LBUF%SIGF,LBUF%EPSF,LBUF%DAM,STRD1    ,STRD2   ,
     3              GAMA     ,LBUF%VOL ,RX      ,RY       ,RZ      ,
     4              SX       ,SY       ,SZ      ,MAT      ,VOLN    ,
     5              DVOL     ,CXX      ,DXX     ,DYY      ,DZZ     , 
     6              D4       ,D5       ,D6      ,S1       ,S2      ,
     7              S3       ,S4       ,S5      ,S6       ,SIGY    ,
     8              DEFP     ,NGL      ,LBUF%SEQ,NEL      )
        IF (JSPH == 0) THEN
          CALL MQVISCB(PM,OFF   ,LBUF%RHO,LBUF%RK ,LBUF%TEMP,CXX      ,
     2         LBUF%RE  ,STIFN   ,DT2T    ,NELTST  ,ITYPTST  ,AIRE    ,
     3         LBUF%OFF ,GEO     ,PID     ,VOLN    ,VD2      ,DELTAX  ,
     4         VIS      ,DXX     ,DYY     ,DZZ     ,PNEW     ,PSH     ,
     5         MAT      ,NGL     ,QVIS    ,SSP_EQ  ,LBUF%VOL ,MSSA    ,
     6         DMELS    ,IGEO    ,FACQ0   ,CONDE   ,GBUF%DT  ,GBUF%G_DT,
     7         IPM      ,RHOREF  ,RHOSP   )
        ELSE
          CALL MDTSPH(PM ,OFF     ,LBUF%RHO,LBUF%RK   ,LBUF%TEMP,
     2         LBUF%RE   ,STIFN   ,DT2T    ,NELTST    ,ITYPTST  ,
     3         LBUF%OFF  ,GEO     ,PID     ,MUMAX     ,CXX      ,
     4        VOLN    ,VD2     ,DELTAX     ,VIS       ,DXX     ,  
     5        DYY     ,DZZ     ,PNEW       ,PSH       ,MAT     ,
     6        NGL     ,QVIS    ,SSP_EQ     ,GBUF%G_DT ,GBUF%DT )
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
        CALL GRAY20(PM,LBUF%EINT,LBUF%RHO,LBUF%TEMP,LBUF%XST,LBUF%QVIS,VOLN,
     .              MAT, DVOL, POLD, DF,
     .              RHO0, PNEW, P01, P02, E01, E02, CXX, DPDM)
        IF (JSPH == 0) THEN
          CALL MQVISCB(PM,OFF   ,LBUF%RHO,LBUF%RK ,LBUF%TEMP,CXX      ,
     2         LBUF%RE  ,STIFN   ,DT2T    ,NELTST  ,ITYPTST  ,AIRE    ,
     3         LBUF%OFF ,GEO     ,PID     ,VOLN    ,VD2      ,DELTAX  , 
     5         VIS      ,DXX     ,DYY     ,DZZ     ,PNEW     ,PSH     ,
     6         MAT      ,NGL     ,QVIS    ,SSP_EQ  ,LBUF%VOL ,MSSA    ,
     7         DMELS    ,IGEO    ,FACQ0   ,CONDE   ,GBUF%DT  ,GBUF%G_DT,
     7         IPM      ,RHOREF  ,RHOSP   )
        ELSE
          CALL MDTSPH(PM ,OFF     ,LBUF%RHO,LBUF%RK   ,LBUF%TEMP,
     2         LBUF%RE   ,STIFN   ,DT2T    ,NELTST    ,ITYPTST  ,
     3         LBUF%OFF  ,GEO     ,PID     ,MUMAX     ,CXX      ,
     4        VOLN    ,VD2     ,DELTAX     ,VIS       ,DXX     ,  
     5        DYY     ,DZZ     ,PNEW       ,PSH       ,MAT     ,
     6        NGL     ,QVIS    ,SSP_EQ     ,GBUF%G_DT ,GBUF%DT )
        ENDIF
        CALL GRAY30(LBUF%SIG ,LBUF%EINT ,LBUF%QVIS ,LBUF%VOL ,QVIS,
     2              VOLN     ,DVOL      ,NEL       , P01     ,P02, 
     3              E01      ,E02       ,PNEW)
c
      ELSEIF (MTN == 17) THEN
        CALL M17LAW(PM      ,OFF     ,LBUF%SIG ,LBUF%EINT ,LBUF%RHO  ,
     2              LBUF%RK ,LBUF%RE ,LBUF%VK  ,WXX       ,WYY       ,
     3              WZZ     ,VOLN    ,MAT      ,VIS       ,DXX       ,
     4              DYY     ,DZZ     ,D4       ,D5        ,D6        ,
     5              NEL     ,ALOGEY  ,CXX, RHO0, TMU,
     6              AMU, AMU2, PSH, PC, ESPE,
     7              C1, C2, C3, C4, C5, C6, DF,DPDM)
        IF (JSPH == 0) THEN
          CALL MQVISCB(PM,OFF   ,LBUF%RHO,LBUF%RK ,LBUF%TEMP,CXX       ,
     2         LBUF%RE ,STIFN   ,DT2T    ,NELTST  ,ITYPTST  ,AIRE      ,
     3         LBUF%OFF,GEO     ,PID     ,VOLN    ,VD2     ,DELTAX   ,
     4         VIS     ,DXX     ,DYY     ,DZZ     ,PNEW    ,PSH      ,
     5         MAT     ,NGL     ,QVIS    ,SSP_EQ  ,LBUF%VOL,MSSA     ,
     6         DMELS   ,IGEO    ,FACQ0   ,CONDE   ,GBUF%DT ,GBUF%G_DT,
     7         IPM     ,RHOREF  ,RHOSP   )
        ELSE
          CALL MDTSPH(PM ,OFF     ,LBUF%RHO,LBUF%RK   ,LBUF%TEMP,
     2         LBUF%RE   ,STIFN   ,DT2T    ,NELTST    ,ITYPTST  ,
     3         LBUF%OFF  ,GEO     ,PID     ,MUMAX     ,CXX      ,
     4          VOLN    ,VD2     ,DELTAX   ,VIS       ,DXX     ,  
     5          DYY     ,DZZ     ,PNEW     ,PSH       ,MAT     ,
     6          NGL     ,QVIS    ,SSP_EQ   ,GBUF%G_DT ,GBUF%DT )
        ENDIF
        CALL MEINT(OFF    ,LBUF%SIG,LBUF%QVIS,LBUF%EINT,VOLN   ,ESPE   ,
     2              S1     ,S2     ,S3     ,S4     ,S5   ,S6   ,
     3              DXX    ,DYY    ,DZZ    ,D4     ,D5   ,D6   , 
     4              PSH    ,DVOL   ,DF     ,QVIS   ,PNEW ,VIS  ,
     5              TMU    ,EINC   ,MTN    ,VOL_AVG,NEL  )
        CALL ATUR17(PM,OFF ,LBUF%RHO,LBUF%RK  ,LBUF%RE ,GEO  ,VOLN,
     .              MAT, DELTAX ,PID    ,VIS  ,VOLN    ,VD2  ,DVOL, 
     .              AIRE, EINC, PNEW, TMU, ALOGEY)
        IF(JTHE == 1)CALL MTHETA(PM,LBUF%EINT,LBUF%TEMP,AMU  ,C1 ,C2  ,
     2                         DF,PSH    ,PC      ,MAT)
      ELSEIF (MTN == 18) THEN
        CALL M18LAW(PM      ,LBUF%VOL,LBUF%EINT,LBUF%TEMP,LBUF%DELTAX,
     2              TF      ,NPF     ,DT2T    ,NELTST  ,ITYPTST ,
     3              IPM     ,STIFN   ,VOLN    ,MAT     ,NGL     ,
     4              CONDE  )
      ELSEIF (MTN == 21) THEN
        CALL M21LAW(PM    ,OFF     ,LBUF%SIG,LBUF%EINT,LBUF%RHO,
     2           LBUF%EPSQ,LBUF%PLA,LBUF%VOL,MAT      ,CXX     ,
     3           DVOL     ,VOLN    ,DXX     ,DYY      ,DZZ     ,
     4           D4       ,D5      ,D6      ,S1       ,S2      ,
     5           S3       ,S4      ,S5      ,S6       ,TF      ,
     6           NPF      ,SIGY    ,DEFP    ,IPM      ,PNEW    ,
     7           PSH      ,AMU     ,LBUF%SEQ,NEL      )
        IF (JSPH == 0) THEN
          CALL MQVISCB(PM       ,OFF      ,LBUF%RHO,LBUF%RK  ,LBUF%TEMP,CXX       ,
     2                 LBUF%RE  ,STIFN    ,DT2T    ,NELTST   ,ITYPTST  ,AIRE      ,
     3                 LBUF%OFF ,GEO      ,PID     ,VOLN    ,VD2       ,DELTAX    ,  
     4                 VIS      ,DXX      ,DYY     ,DZZ     ,BID1      ,BID2      ,
     5                 MAT      ,NGL      ,QVIS    ,SSP_EQ  ,LBUF%VOL  ,MSSA      ,
     6                 DMELS    ,IGEO     ,FACQ0   ,CONDE   ,GBUF%DT   ,GBUF%G_DT , 
     7                 IPM      ,RHOREF   ,RHOSP   )
        ELSE
          CALL MDTSPH(PM        ,OFF     ,LBUF%RHO,LBUF%RK   ,LBUF%TEMP,
     2                LBUF%RE   ,STIFN   ,DT2T    ,NELTST    ,ITYPTST  ,
     3                LBUF%OFF  ,GEO     ,PID     ,MUMAX     ,CXX      ,
     4                VOLN      ,VD2     ,DELTAX  ,VIS       ,DXX      ,  
     5                DYY       ,DZZ     ,BID1    ,BID2      ,MAT      ,
     6                NGL       ,QVIS    ,SSP_EQ  ,GBUF%G_DT ,GBUF%DT )
        ENDIF
        !no need to call eosmain since it is no energy dependant. P and Psh are already calculated  
        CALL MEINT(OFF    ,LBUF%SIG,LBUF%QVIS,LBUF%EINT,VOLN   ,ESPE   ,
     2              S1     ,S2     ,S3     ,S4     ,S5   ,S6   ,
     3              DXX    ,DYY    ,DZZ    ,D4     ,D5   ,D6   , 
     4              PSH    ,DVOL   ,DF     ,QVIS   ,PNEW ,VIS  ,
     5              TMU    ,EINC   ,MTN    ,VOL_AVG,NEL  )         
        CALL EOSUPDA(OFF  ,LBUF%SIG ,LBUF%EINT, LBUF%VOL ,PNEW ,NEL)        
      ELSEIF (MTN == 22) THEN
        CALL M22LAW(PM      ,OFF     ,LBUF%SIG ,LBUF%EINT,LBUF%RHO ,
     2             LBUF%QVIS,LBUF%PLA,LBUF%EPSD,LBUF%VOL ,ZERO     ,
     3             STIFN    ,DT2T    ,NELTST   ,ITYPTST  ,LBUF%OFF ,
     4             GEO      ,PID     ,AMU      ,VOL_AVG  ,
     6             MUMAX  ,MAT     ,NGL     ,CXX     ,DVOL    ,
     7             AIRE   ,VOLN    ,VD2     ,DELTAX  ,VIS     ,  
     8             DXX    ,DYY     ,DZZ     ,D4      ,D5      ,
     9             D6     ,PNEW    ,PSH     ,QVIS    ,SSP_EQ  ,
     A             S1     ,S2      ,S3      ,S4      ,S5      ,
     B             S6     ,JPLASOL ,SIGY    ,DEFP    ,DPLA    ,
     C             MSSA   ,DMELS   ,CONDE   ,GBUF%DT ,GBUF%G_DT,
     D             NEL    ,IPM     ,RHOREF  ,RHOSP   )
      ELSEIF (MTN == 23) THEN
        CALL M22LAW(PM      ,OFF      ,LBUF%SIG ,LBUF%EINT,LBUF%RHO  ,
     2             LBUF%QVIS,LBUF%PLA ,LBUF%EPSD,LBUF%VOL ,UN        , 
     3             STIFN    ,DT2T     ,NELTST   ,ITYPTST  ,LBUF%OFF  ,
     4             GEO      ,PID      ,AMU      ,VOL_AVG   ,
     6             MUMAX  ,MAT      ,NGL     ,CXX        ,DVOL    ,
     7             AIRE   ,VOLN     ,VD2     ,DELTAX     ,VIS     ,  
     8             DXX    ,DYY      ,DZZ     ,D4         ,D5      ,
     9             D6     ,PNEW     ,PSH     ,QVIS       ,SSP_EQ  ,
     A             S1     ,S2       ,S3      ,S4         ,S5      ,
     B             S6     ,JPLASOL  ,SIGY    ,DEFP       ,DPLA    ,
     C             MSSA   ,DMELS    ,CONDE   ,GBUF%DT    ,GBUF%G_DT,
     D             NEL    ,IPM      ,RHOREF  ,RHOSP   )
      ELSEIF (MTN == 24) THEN
        CALL M24LAW(LBUF     ,PM        ,OFF      ,LBUF%SIG ,LBUF%EINT,
     2              LBUF%RHO ,LBUF%QVIS ,LBUF%VOL ,STIFN    ,DT2T     ,
     3              NELTST   ,ITYPTST   ,OFFSET   ,NEL      ,LBUF%OFF ,
     4              GEO      ,PID       ,MAT      ,NGL      ,CXX      ,
     5              AIRE     ,VOLN      ,VD2      ,DELTAX   ,VIS      ,  
     6              DXX      ,DYY       ,DZZ      ,PNEW     ,PSH      ,
     7              QVIS     ,SSP_EQ    ,DVOL     ,D4       ,D5       ,   
     8              D6       ,MUMAX     ,GAMA     ,MSSA     ,DMELS    ,
     9              R3_DAM   ,S1        ,S2       ,S3       ,S4       ,
     A              S5       ,S6        ,RX       ,RY       ,RZ       ,
     B              SX       ,SY        ,SZ       ,CONDE    ,VOL_AVG  ,
     C              GBUF%DT  ,GBUF%G_DT ,IPM      ,RHOREF  ,RHOSP     ,
     D              LBUF%EPSD)
C     like law25 for shell + S33 = EPS33*E33
      ELSEIF (MTN == 25) THEN
        CALL M25LAW(
     1       PM      ,MAT      ,OFF     ,LBUF%SIG,LBUF%EINT,
     2       S1      ,S2       ,S3      ,S4      ,S5       ,S6       ,          
     3       DXX     ,DYY      ,DZZ     ,D4      ,D5       ,D6       ,          
     4       RX      ,RY       ,RZ      ,SX      ,SY       ,SZ       ,            
     5       TX      ,TY       ,TZ      ,GAMA    ,VOLN     ,CXX      ,          
     6       LBUF%VOL,LBUF%EPSD,LBUF%PLA,LBUF%DAM,LBUF%CRAK,LBUF%STRA,     
     7       DEFP    ,SIGY     ,SIGL    ,NGL     ,ILAY     ,LBUF%OFF ,
     8       LBUF%SEQ,NEL      ,IPM     ,IPG     )                         
        IF (JSPH == 0) THEN
          CALL MQVISCB(PM,OFF   ,LBUF%RHO,LBUF%RK ,LBUF%TEMP,CXX       ,
     2         LBUF%RE  ,STIFN   ,DT2T    ,NELTST  ,ITYPTST  ,AIRE      ,
     3         LBUF%OFF ,GEO     ,PID     ,VOLN     ,VD2     ,DELTAX  ,
     4         VIS      ,DXX     ,DYY     ,DZZ      ,PNEW    ,PSH     ,
     5         MAT      ,NGL     ,QVIS    ,SSP_EQ  ,LBUF%VOL,MSSA     ,
     6         DMELS    ,IGEO    ,FACQ0   ,CONDE   ,GBUF%DT ,GBUF%G_DT,
     7         IPM      ,RHOREF  ,RHOSP   )
        ELSE
          CALL MDTSPH(PM ,OFF     ,LBUF%RHO,LBUF%RK   ,LBUF%TEMP,
     2         LBUF%RE   ,STIFN   ,DT2T    ,NELTST    ,ITYPTST  ,
     3         LBUF%OFF  ,GEO     ,PID     ,MUMAX     ,CXX      ,
     4         VOLN      ,VD2     ,DELTAX  ,VIS       ,DXX     ,  
     5         DYY       ,DZZ     ,PNEW    ,PSH       ,MAT     ,
     6         NGL       ,QVIS    ,SSP_EQ  ,GBUF%G_DT ,GBUF%DT )
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
        CALL SESA20(PM     ,LBUF%EINT,LBUF%RHO,LBUF%TEMP,LBUF%Z,
     .              LBUF%QVIS,BUFMAT ,LBUF%SSP,XK       ,VOLN  ,
     .              MAT      ,QVIS   ,DVOL, POLD, CXX, RHO0, P01)
        IF (JSPH == 0) THEN
         CALL MQVISC26(PM,OFF     ,LBUF%RHO,LBUF%RK ,LBUF%TEMP,CXX     ,
     2           LBUF%RE ,STIFN   ,DT2T    ,NELTST  ,ITYPTST  ,AIRE    ,
     3           LBUF%OFF,GEO     ,PID     ,
     4           VOLN    ,VD2     ,DELTAX  ,VIS        ,  
     5           DXX     ,DYY     ,DZZ    ,PNEW    ,PSH     ,MAT        ,
     6           NGL     ,QVIS    ,SSP_EQ ,XK      )
        ELSE
          CALL MDTSPH(PM ,OFF     ,LBUF%RHO,LBUF%RK   ,LBUF%TEMP,
     2         LBUF%RE   ,STIFN   ,DT2T    ,NELTST    ,ITYPTST  ,
     3         LBUF%OFF  ,GEO     ,PID     ,MUMAX     ,CXX      ,
     4          VOLN    ,VD2     ,DELTAX   ,VIS       ,DXX     ,  
     5          DYY     ,DZZ     ,PNEW     ,PSH       ,MAT     ,
     6          NGL     ,QVIS    ,SSP_EQ   ,GBUF%G_DT ,GBUF%DT )
        ENDIF
        CALL SESA30(LBUF%SIG ,LBUF%EINT ,LBUF%QVIS ,LBUF%VOL ,QVIS,
     2              VOLN     ,DVOL      ,NEL, P01)
      ELSEIF (MTN == 46 .OR. MTN == 47) THEN
        NUVAR  = IPM(8,MAT(1))
        CALL M46LAW(
     1       LFT      ,LLT     ,NFT     ,MTN      ,
     2       PM       ,OFF     ,LBUF%SIG,LBUF%EINT,LBUF%RHO,
     3       LBUF%QVIS,LBUF%VOL,MBUF%VAR,BUFMAT   ,STIFN   ,
     4       MAT      ,NGL     ,NUVAR   ,DT2T     ,NELTST  ,RHO0    ,
     5       ITYPTST  ,LBUF%OFF,JLAG    ,GEO      ,PID     ,CXX     ,
     6       AIRE     ,VOLN    ,VD2     ,DELTAX   ,VIS     ,DXX     , 
     7       DYY      ,DZZ     ,PNEW    ,PSH      ,QVIS    ,SSP_EQ  ,
     8       WXX     ,
     9       WYY      ,WZZ     ,IPM     ,MSSA     ,DMELS   ,DVOL    ,
     A       S1       ,S2      ,S3      ,S4       ,S5      ,S6      ,
     B       CONDE    ,VOL_AVG ,GBUF%DT ,GBUF%G_DT,NEL     ,D4      ,
     C       D5       ,D6      ,RHOREF  ,RHOSP   )
C
      ELSEIF (MTN == 49) THEN
        CALL M49LAW (MAT      ,PM       ,OFF     ,LBUF%SIG,LBUF%PLA,
     2               LBUF%TEMP,LBUF%EPSD,CXX     ,DF      ,DXX     ,
     3               DYY      ,DZZ      ,D4      ,D5      ,D6      ,
     4               RHO0     ,DPDM     ,SIGY    ,DEFP    ,DPLA    ,
     5               ESPE     ,ECOLD    ,NEL     )
        IF (JSPH == 0) THEN
          CALL MQVISCB(PM,OFF   ,LBUF%RHO,LBUF%RK ,LBUF%TEMP,CXX       ,
     2         LBUF%RE  ,STIFN   ,DT2T    ,NELTST  ,ITYPTST  ,AIRE     ,
     3         LBUF%OFF ,GEO     ,PID     ,VOLN    ,VD2     ,DELTAX    ,
     4         VIS      ,DXX     ,DYY     ,DZZ     ,PNEW    ,PSH       ,
     5         MAT      ,NGL     ,QVIS    ,SSP_EQ  ,LBUF%VOL,MSSA      ,
     6         DMELS    ,IGEO     ,FACQ0   ,CONDE   ,GBUF%DT ,GBUF%G_DT,
     7         IPM      ,RHOREF   ,RHOSP   )
        ELSE
          CALL MDTSPH(PM ,OFF     ,LBUF%RHO,LBUF%RK   ,LBUF%TEMP,
     2         LBUF%RE   ,STIFN   ,DT2T    ,NELTST    ,ITYPTST  ,
     3         LBUF%OFF  ,GEO     ,PID     ,MUMAX     ,CXX      ,
     4           VOLN    ,VD2     ,DELTAX  ,VIS       ,DXX     ,  
     5           DYY     ,DZZ     ,PNEW    ,PSH       ,MAT     ,
     6           NGL     ,QVIS    ,SSP_EQ  ,GBUF%G_DT ,GBUF%DT )
        ENDIF
        CALL MEINT(OFF    ,LBUF%SIG,LBUF%QVIS,LBUF%EINT,VOLN   ,ESPE   ,
     2              S1     ,S2     ,S3     ,S4     ,S5   ,S6   ,
     3              DXX    ,DYY    ,DZZ    ,D4     ,D5   ,D6   , 
     4              PSH    ,DVOL   ,DF     ,QVIS   ,PNEW ,VIS  ,
     5              TMU    ,EINC   ,MTN    ,VOL_AVG,NEL  ) 
C
      ELSEIF (MTN > 28 .AND. MTN < 32 .or. MTN == 99 .or. MTN == 200) THEN
c---    user material law libraries here
c
        NUVAR   = ELBUF_TAB(NG)%BUFLY(ILAY)%NVAR_MAT
        l_MULAW_CALLED=.TRUE.
c
        CALL USERMAT_SOLID(
     1      LFT         ,LLT        ,NFT         ,MTN       ,JCVT     ,
     2      PM          ,OFF        ,LBUF%SIG    ,LBUF%EINT ,LBUF%RHO ,
     3      LBUF%QVIS   ,LBUF%VOL   ,LBUF%STRA   ,LBUF%SIGL ,GAMA     ,
     4      MBUF%VAR    ,BUFMAT     ,TF          ,NPF       ,STIFN    ,
     5      MAT         ,NGL        ,NUVAR       ,DT2T      ,NELTST   ,
     6      ITYPTST     ,LBUF%OFF   ,GEO         ,PID       ,LBUF%EPSD,
     7      LBUF%TEMP   ,WXX        ,
     9      WYY         ,WZZ        ,JSPH        ,MUMAX     ,CXX      ,
     A      AIRE        ,VOLN       ,VD2         ,DELTAX    ,VIS      ,  
     B      DXX         ,DYY        ,DZZ         ,D4        ,D5       ,
     C      D6          ,PNEW       ,PSH         ,QVIS      ,SSP_EQ   ,
     D      DVOL        ,S1         ,S2          ,S3        ,S4       ,
     E      S5          ,S6         ,RX          ,RY        ,RZ       ,
     F      SX          ,SY         ,SZ          ,TX        ,TY       ,
     G      TZ          ,JPLASOL    ,SIGY        ,DEFP      ,ISMSTR   ,
     H      MFXX        ,MFXY       ,MFXZ        ,MFYX      ,MFYY     , 
     I      MFYZ        ,MFZX       ,MFZY        ,MFZZ      ,IPM      ,
     J      ISORTH      ,FBUF       ,NFAIL       ,NPG       ,LBUF%SIGD,
     K      DXY         ,DYX        ,DYZ         ,DZY       ,DZX      ,
     L      DXZ         ,FR_WAV     ,ISROT       ,V         ,VARNL    ,
     M      W           ,IX         ,X           ,JTHE      ,ET       ,
     N      MSSA        ,DMELS      ,IPTR        ,IPTS      ,IPTT     ,
     O      TABLE       ,FVD2       ,FDELTAX     ,FSSP      ,FQVIS    ,
     P      TEMPEL      ,IPARG1      ,IGEO       ,LBUF%SIGV ,
     Q      AL_IMP      ,SIGNOR     ,ISTRAIN     ,NG        ,ELBUF_TAB,
     R      VBUF        ,ILAY       ,LBUF%VK     ,IPARG    ,
     S      BUFVOIS     ,VDX        ,VDY         ,VDZ       ,IHET     ,
     T      CONDE       ,ITASK      ,IEXPAN      ,VOL_AVG   ,AMU      ,
     U      EPSTH       ,LBUF%FORTH ,LBUF%VISC   ,NEL       ,GBUF%ETOTSH,
     V      ISELECT     ,TSTAR      ,LBUF%MU     ,AMU2      ,DPDM     ,
     W      RHOREF      ,RHOSP      )
c
      ELSE   ! 'user type' Radioss material laws 
C---
        NUVAR   = ELBUF_TAB(NG)%BUFLY(ILAY)%NVAR_MAT
        NVARTMP = ELBUF_TAB(NG)%BUFLY(ILAY)%NVARTMP
        l_MULAW_CALLED =.TRUE.
c
        CALL MULAW(LFT  ,LLT        ,NFT         ,MTN       ,JCVT     ,
     2      PM          ,OFF        ,LBUF%SIG    ,LBUF%EINT ,LBUF%RHO ,
     3      LBUF%QVIS   ,LBUF%VOL   ,LBUF%STRA   ,LBUF%SIGL ,GAMA     ,
     4      MBUF%VAR    ,BUFMAT     ,TF          ,NPF       ,STIFN    ,
     5      MAT         ,NGL        ,NUVAR       ,DT2T      ,NELTST   ,
     6      ITYPTST     ,LBUF%OFF   ,GEO         ,PID       ,LBUF%EPSD,
     7      LBUF%TEMP   ,WXX        ,
     9      WYY         ,WZZ        ,JSPH        ,MUMAX     ,CXX      ,
     A      AIRE        ,VOLN       ,VD2         ,DELTAX    ,VIS      ,  
     B      DXX         ,DYY        ,DZZ         ,D4        ,D5       ,
     C      D6          ,PNEW       ,PSH         ,QVIS      ,SSP_EQ   ,
     D      DVOL        ,S1         ,S2          ,S3        ,S4       ,
     E      S5          ,S6         ,RX          ,RY        ,RZ       ,
     F      SX          ,SY         ,SZ          ,TX        ,TY       ,
     G      TZ          ,JPLASOL    ,SIGY        ,DEFP      ,ISMSTR   ,
     H      MFXX        ,MFXY       ,MFXZ        ,MFYX      ,MFYY     , 
     I      MFYZ        ,MFZX       ,MFZY        ,MFZZ      ,IPM      ,
     J      ISORTH      ,FBUF       ,NFAIL       ,NPG       ,LBUF%SIGD,
     K      DXY         ,DYX        ,DYZ         ,DZY       ,DZX      ,
     L      DXZ         ,FR_WAV     ,ISROT       ,V         ,VARNL    ,
     M      W           ,IX         ,X           ,JTHE      ,ET       ,
     N      MSSA        ,DMELS      ,IPTR        ,IPTS      ,IPTT     ,
     O      TABLE       ,FVD2       ,FDELTAX     ,FSSP      ,FQVIS    ,
     P      TEMPEL      ,IPARG1      ,IGEO       ,LBUF%SIGV ,
     Q      AL_IMP      ,SIGNOR     ,ISTRAIN     ,NG        ,ELBUF_TAB,
     R      VBUF        ,ILAY       ,LBUF%VK     ,ALE_CONNECT     ,IPARG    ,
     S      BUFVOIS     ,VDX        ,VDY         ,VDZ       ,IHET     ,
     T      CONDE       ,ITASK      ,IEXPAN      ,VOL_AVG   ,AMU      ,
     U      EPSTH       ,LBUF%FORTH ,LBUF%VISC   ,NEL       ,GBUF%ETOTSH,
     V      ISELECT     ,TSTAR      ,LBUF%MU     ,AMU2      ,DPDM     ,
     W      RHOREF      ,RHOSP      ,NVARTMP     ,MBUF%VARTMP,LBUF%EINTTH,
     X      MATPARAM_TAB)

      ENDIF 
C-----------------------------------
C     EOS PART2
C-----------------------------------
      IF (MTN == 3.OR.MTN == 4.OR.MTN == 6.OR.MTN.EQ.10.OR.MTN.EQ.17.OR.MTN == 49.OR.MTN == 102.OR.MTN == 103) THEN
         IF (MTN .NE. 6 .AND. MTN .NE. 17) THEN
            PNEW(:) = ZERO
         ENDIF
        CALL EOSMAIN(1       ,NEL      ,EOSTYP  ,PM       ,OFF      ,LBUF%EINT,
     2               LBUF%RHO,RHO0     ,AMU     ,AMU2     ,ESPE     ,
     3               DVOL    ,DF       ,VOLN    ,MAT      ,PSH      ,
     4               PNEW    ,DPDM     ,DPDE    ,LBUF%TEMP,ECOLD    ,
     5               BUFMAT  ,LBUF%SIG ,LBUF%MU ,MTN      ,POLD)
        CALL EOSUPDA(OFF  ,LBUF%SIG ,LBUF%EINT, LBUF%VOL ,PNEW,NEL)
      ELSEIF(l_MULAW_CALLED)THEN
        IF(l_MULAW_CALLED)THEN
         DO I=1,NEL
           IF(LBUF%VOL(I) .GT. ZERO)THEN
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
        DO I=LFT,LLT
          DIE(I) = (LBUF%EINT(I)*LBUF%VOL(I) - DIE(I)) * PM(90,MAT(1))
          HEAT_MECA_L = HEAT_MECA_L + DIE(I)
        ENDDO
!$OMP CRITICAL
      HEAT_MECA = HEAT_MECA_L + HEAT_MECA
!$OMP END CRITICAL
      ENDIF
      IF((IEXPAN > 0).AND.(JTHE < 0).AND.(TT.NE.0)) THEN
        DO I=LFT,LLT
          SIGKK(I) = LBUF%SIG(II(1)+I)+LBUF%SIG(II(2)+I)+LBUF%SIG(II(3)+I)
          LBUF%EINTTH(I) = LBUF%EINTTH(I)-UNDEMI*SIGKK(I)*ETH(I)
        ENDDO
      ENDIF

C---------------------------------------------------------------------
C   calcul of viscosity ( Navier-Stokes) 
C---------------------------------------------------------------------
      IF( (MTN < 28 .AND. MTN /= 24) .OR. MTN == 49) THEN
        CALL MNSVIS(PM ,OFF   ,LBUF%RHO,GEO     ,PID     ,
     2       CXX     ,AIRE    ,VOLN    ,DXX      ,DYY       ,
     3       DZZ     ,D4      ,D5      ,D6      ,MAT       ,
     4       ISVIS   ,RHOREF  )
        IF (ISVIS > 0) THEN
          DO  I=LFT,LLT                                                 
            VOL(I)  = LBUF%VOL(I)                                       
            EINT(I) = LBUF%EINT(I)                                      
            E1=DXX(I)*SVIS(I,1)                                         
            E2=DYY(I)*SVIS(I,2)                                         
            E3=DZZ(I)*SVIS(I,3)                                         
            E4=D4(I)*SVIS(I,4)                                          
            E5=D5(I)*SVIS(I,5)                                          
            E6=D6(I)*SVIS(I,6)                                          
            EINT(I)=EINT(I)*VOL(I)                                      
     .       +(E1+E2+E3+E4+E5+E6)*(VOLN(I)-UNDEMI*DVOL(I))*DT1*UNDEMI  
            EINT(I)=EINT(I)/MAX(EM20,VOL(I))                            
            LBUF%EINT(I) = EINT(I)     
          ENDDO     
        ENDIF  
      ENDIF    
C-----------------------------------------------------------------------
C     viscous stress (/VISC models) 
C-----------------------------------------------------------------------
      IF (IVISC > 0 .AND. (MTN  < 28 .AND. MTN /= 24)) THEN
        DO I=LFT,LLT
          EP1(I) = DXX(I)*OFF(I)
          EP2(I) = DYY(I)*OFF(I)
          EP3(I) = DZZ(I)*OFF(I)
          EP4(I) = D4(I) *OFF(I)
          EP5(I) = D5(I) *OFF(I)
          EP6(I) = D6(I) *OFF(I)
        ENDDO
        IF (ISORTH /= 0 .AND. JCVT == 0) THEN
          CALL MREPLOC(GAMA,R11,R12,R13,R21,R22    ,
     .                 R23 ,R31,R32,R33,
     .                 RX  ,RY ,RZ ,SX ,SY ,SZ     ,
     .                 TX  ,TY ,TZ )
          DO I=LFT,LLT
            EP4(I) = UNDEMI*EP4(I)
            EP5(I) = UNDEMI*EP5(I)
            EP6(I) = UNDEMI*EP6(I)
          ENDDO
          CALL MROTENS(LFT,LLT,EP1,EP2,EP3,EP4,EP5,EP6,
     .         R11,R21,R31,R12,R22,R32,R13,R23,R33)        
          DO I=LFT,LLT
            EP4(I) = DEUX*EP4(I)
            EP5(I) = DEUX*EP5(I)
            EP6(I) = DEUX*EP6(I)
          ENDDO
        ENDIF
c---        
         CALL VISCMAIN(
     .        NEL     ,MAT(1)  ,IVISC   ,IPM     ,BUFMAT  ,
     .        VBUF%VAR,RHO0    ,VIS     ,CXX     ,DT1     , 
     .        EP1     ,EP2     ,EP3     ,EP4     ,EP5     ,EP6     ,
     .        SV1     ,SV2     ,SV3     ,SV4     ,SV5     ,SV6     )
c---
        IF (ISORTH /= 0.AND. JCVT == 0) THEN
          CALL MROTENS(LFT,LLT,
     .         SV1     ,SV2      ,SV3      ,SV4     ,SV5     ,SV6     ,
     .         R11,R21,R31,R12,R22,R32,R13,R23,R33)
        ENDIF
c
        DO I=LFT,LLT    
          LBUF%VISC(II(1) + I) = SV1(I)*OFF(I)
          LBUF%VISC(II(2) + I) = SV2(I)*OFF(I)
          LBUF%VISC(II(3) + I) = SV3(I)*OFF(I)
          LBUF%VISC(II(4) + I) = SV4(I)*OFF(I)
          LBUF%VISC(II(5) + I) = SV5(I)*OFF(I)
          LBUF%VISC(II(6) + I) = SV6(I)*OFF(I)
        ENDDO           
c              Temporaire, pour la compatibilite avec la mainline actuelle        
               DO I=LFT,LLT                      
                 SVIS(I,1) = LBUF%VISC(II(1) + I)		  
                 SVIS(I,2) = LBUF%VISC(II(2) + I)		  
                 SVIS(I,3) = LBUF%VISC(II(3) + I)		  
                 SVIS(I,4) = LBUF%VISC(II(4) + I)		  
                 SVIS(I,5) = LBUF%VISC(II(5) + I)		  
                 SVIS(I,6) = LBUF%VISC(II(6) + I)		  
               ENDDO                             
               IF (ISVIS > 0) THEN
c                viscous stress output
                 DO I=LFT,LLT
                   LBUF%SIGV(II(1) + I) = SVIS(I,1)
                   LBUF%SIGV(II(2) + I) = SVIS(I,2)
                   LBUF%SIGV(II(3) + I) = SVIS(I,3)
                   LBUF%SIGV(II(4) + I) = SVIS(I,4)
                   LBUF%SIGV(II(5) + I) = SVIS(I,5)
                   LBUF%SIGV(II(6) + I) = SVIS(I,6)
                  ENDDO                          
               ENDIF
      ELSEIF (IVISC > 0) THEN
        DO I=LFT,LLT    
          LBUF%VISC(II(1) + I) = SVIS(I,1)
          LBUF%VISC(II(2) + I) = SVIS(I,2)
          LBUF%VISC(II(3) + I) = SVIS(I,3)
          LBUF%VISC(II(4) + I) = SVIS(I,4)
          LBUF%VISC(II(5) + I) = SVIS(I,5)
          LBUF%VISC(II(6) + I) = SVIS(I,6)
        ENDDO           
        IF (ISVIS > 0) THEN                      
c         viscous stress output                  
          DO I=LFT,LLT                           
            LBUF%SIGV(II(1) + I) = SVIS(I,1)	    
            LBUF%SIGV(II(2) + I) = SVIS(I,2)	    
            LBUF%SIGV(II(3) + I) = SVIS(I,3)	    
            LBUF%SIGV(II(4) + I) = SVIS(I,4)	    
            LBUF%SIGV(II(5) + I) = SVIS(I,5)	    
            LBUF%SIGV(II(6) + I) = SVIS(I,6)	    
           ENDDO                                 
        ENDIF                                    
      ENDIF
C-----------------------------------------------------------------------
c     failure for law no user ---
C-----------------------------------------------------------------------
      IF ((ITASK==0).AND.(IMON_MAT==1))CALL STARTIME(121,1)
      IF (NFAIL > 0 .AND. (MTN < 28 .OR. MTN == 49)) THEN
        IF (ISTRAIN > 0 .OR. MTN==24 .OR. MTN==25 .OR. MTN==15) THEN
          DO I=LFT,LLT
            ES1(I) = LBUF%STRA(II(1) + I)     
            ES2(I) = LBUF%STRA(II(2) + I) 
            ES3(I) = LBUF%STRA(II(3) + I) 
            ES4(I) = LBUF%STRA(II(4) + I) 
            ES5(I) = LBUF%STRA(II(5) + I) 
            ES6(I) = LBUF%STRA(II(6) + I) 
          ENDDO
        ELSE
          DO I=LFT,LLT
            ES1(I) = EP20
            ES2(I) = EP20
            ES3(I) = EP20
            ES4(I) = EP20
            ES5(I) = EP20
            ES6(I) = EP20  
          ENDDO
        ENDIF
c---
C------------------------------------------------------------
C     Recovering non-local variable
C------------------------------------------------------------
        INLOC = IPARG(78,NG)
        IF (INLOC > 0) THEN 
          IF (ELBUF_TAB(NG)%BUFLY(ILAY)%L_PLA > 0) THEN 
            DO I = LFT,LLT
              DPLA(I) = MAX(VARNL(I),ZERO)
            ENDDO
          ENDIF
        ENDIF
c---
        DO IR = 1,NFAIL 
          IP = (IR -1)*15         
c
          DO I=LFT,LLT
            SS1(I) = LBUF%SIG(II(1) + I)    
            SS2(I) = LBUF%SIG(II(2) + I)
            SS3(I) = LBUF%SIG(II(3) + I)
            SS4(I) = LBUF%SIG(II(4) + I)
            SS5(I) = LBUF%SIG(II(5) + I)
            SS6(I) = LBUF%SIG(II(6) + I)
            EINT(I) = LBUF%EINT(I)*LBUF%VOL(I)
            IF (MTN.NE.4.AND.MTN.NE.2) THEN
              EPSP(I)=OFF(I)*                                        
     .        MAX( ABS(DXX(I)),ABS(DYY(I)),ABS(DZZ(I)),              
     .       UNDEMI*ABS(D4(I)),UNDEMI*ABS(D5(I)),UNDEMI*ABS(D6(I)))
            ENDIF         
          ENDDO
C
C------------------------------------------------------------
C     Recovering non-local plastic strain-rate
C------------------------------------------------------------
          IF (INLOC > 0) THEN 
            IF (ELBUF_TAB(NG)%BUFLY(ILAY)%L_PLA > 0) THEN 
              DO I = LFT,LLT
                EPSP(I) = MAX(VARNL(I),ZERO)/MAX(DT1,EM20)
              ENDDO
            ENDIF
          ENDIF
C           
          IF (ISORTH  > 0 .AND. JCVT == 0) THEN 
            CALL MREPLOC(GAMA ,R11,R12,R13,R21,R22    ,
     .                  R23  ,R31,R32,R33,                
     .                  RX   ,RY ,RZ ,SX ,SY ,SZ     ,    
     .                  TX   ,TY ,TZ )                    
            CALL MROTENS(LFT,LLT,SS1,SS2,SS3,SS4,SS5,SS6,
     .              R11,R12,R13,
     .              R21,R22,R23,
     .              R31,R32,R33)
C strain        
            DO I=LFT,LLT
              ES4(I) = UNDEMI*ES4(I)
              ES5(I) = UNDEMI*ES5(I)
              ES6(I) = UNDEMI*ES6(I)
            ENDDO              
            CALL MROTENS(LFT,LLT,ES1,ES2,ES3,ES4,ES5,ES6,
     .              R11,R12,R13,
     .              R21,R22,R23,
     .              R31,R32,R33)
            DO I=LFT,LLT
              ES4(I) = DEUX*ES4(I)
              ES5(I) = DEUX*ES5(I)
              ES6(I) = DEUX*ES6(I)
            ENDDO 
C strain rate
            DO I=LFT,LLT
              EP1(I) = DXX(I)
              EP2(I) = DYY(I)
              EP3(I) = DZZ(I)
              EP4(I) = UNDEMI*D4(I)
              EP5(I) = UNDEMI*D5(I)
              EP6(I) = UNDEMI*D6(I)
            ENDDO              
            CALL MROTENS(LFT,LLT,EP1,EP2,EP3,EP4,EP5,EP6,
     .              R11,R12,R13,
     .              R21,R22,R23,
     .              R31,R32,R33)
            DO I=LFT,LLT
              EP4(I) = DEUX*EP4(I)
              EP5(I) = DEUX*EP5(I)
              EP6(I) = DEUX*EP6(I)
            ENDDO
          ELSE          !  strain rate
            DO I=LFT,LLT
              EP1(I) = DXX(I)
              EP2(I) = DYY(I)
              EP3(I) = DZZ(I)
              EP4(I) = UNDEMI*D4(I)
              EP5(I) = UNDEMI*D5(I)
              EP6(I) = UNDEMI*D6(I)
            ENDDO              
          ENDIF  
c
c----  
          UVARF => FBUF%FLOC(IR)%VAR  
          IRUPT  = FBUF%FLOC(IR)%ILAWF 
          NVARF  = FBUF%FLOC(IR)%NVAR 
          DFMAX => FBUF%FLOC(IR)%DAMMX  
          TDEL  => FBUF%FLOC(IR)%TDEL  
          NPARAM = IPM(112 + IP, MAT(1)) 
          IADBUF = IPM(114 + IP ,MAT(1))  
          NFUNC  = IPM(115 + IP, MAT(1))             
          DO I=1,NFUNC
            IFUNC(I) = IPM(115+IP+I, MAT(1))         
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
        ELSEIF(IRUPT.EQ.4)THEN 
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
!               CALL F04LAW(LLT ,NPARAM,NVARF,NFUNC,IFUNC,
!     2                    NPF ,TF  ,TT  ,DT1  ,BUFMAT(IADBUF),
!     3                    NGL ,IBIDON1,IBIDON2,IBIDON3 ,IBIDON4,
!     4                    EP1  ,EP2 ,EP3  ,EP4  ,EP5  ,EP6 ,
!     5                    ES1  ,ES2 ,ES3  ,ES4  ,ES5  ,ES6 ,
!     6                    SS1  ,SS2 ,SS3  ,SS4  ,SS5  ,SS6 ,    
!     7                    DEFP ,DPLA,EPSP,UVARF,OFF  ,
!     8                    DELTAX,VOLN,GBUF%UELR,BIDON4,BIDON5) 
!!!
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/FAIL/USER1 - SOLID '
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
!!!
            ENDIF
C   user2
        ELSEIF(IRUPT.EQ.5)THEN        
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
!               CALL F05LAW(LLT ,NPARAM,NVARF,NFUNC,IFUNC,
!     2                    NPF ,TF  ,TT  ,DT1  ,BUFMAT(IADBUF),
!     3                    NGL ,IBIDON1,IBIDON2,IBIDON3 ,IBIDON4,
!     4                    EP1  ,EP2 ,EP3  ,EP4  ,EP5  ,EP6 ,
!     5                    ES1  ,ES2 ,ES3  ,ES4  ,ES5  ,ES6 ,
!     6                    SS1  ,SS2 ,SS3  ,SS4  ,SS5  ,SS6 ,    
!     7                    DEFP ,DPLA,EPSP,UVARF,OFF  ,
!     8                    DELTAX,VOLN,GBUF%UELR,BIDON4,BIDON5)
!!!
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/FAIL/USER2 - SOLID '
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
!!!
            ENDIF
C   user3         
        ELSEIF(IRUPT.EQ.6)THEN
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
!                CALL F06LAW(LLT ,NPARAM,NVARF,NFUNC,IFUNC,
!     2                    NPF ,TF  ,TT  ,DT1  ,BUFMAT(IADBUF),
!     3                    NGL ,IBIDON1,IBIDON2,IBIDON3 ,IBIDON4,
!     4                    EP1  ,EP2 ,EP3  ,EP4  ,EP5  ,EP6 ,
!     5                    ES1  ,ES2 ,ES3  ,ES4  ,ES5  ,ES6 ,
!     6                    SS1  ,SS2 ,SS3  ,SS4  ,SS5  ,SS6 ,    
!     7                    DEFP ,DPLA,EPSP,UVARF,OFF  ,
!     8                    DELTAX,VOLN,GBUF%UELR,BIDON4,BIDON5)
!!!
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/FAIL/USER3 - SOLID '
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
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
     5                     DPLA ,DEFP ,UVARF,OFF   ,DFMAX ,
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
     8             MFZX   ,MFZY   ,MFZZ   )
c
          ELSEIF(IRUPT == 11)THEN                                                          
c---- energy failure  
              CALL FAIL_ENERGY_S(LLT ,NPARAM,NVARF,NFUNC,IFUNC      ,
     2                    NPF ,TF  ,TT  ,DT1  ,BUFMAT(IADBUF),
     3                    NGL ,IPM ,MAT ,
     5                    SS1 ,SS2 ,SS3 ,SS4  ,SS5  ,SS6     ,
     6                    VOLN,EINT,EPSP,UVARF,OFF  ,IP      ,
     7                    DFMAX,TDEL )                      
          ELSEIF(IRUPT == 14)THEN                                        
c --- Hashin    failure model
             DO I=LFT,LLT
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
     2        BUFMAT(IADBUF)     ,NGL      ,IPM      ,MAT      ,DELTAX   ,         
     3        SS1      ,SS2      ,SS3      ,SS4      ,SS5      ,SS6  ,    
     4        DEFP     ,DPLA     ,EPSP     ,TSTAR    ,UVARF    ,           
     5        OFF      ,IP       ,TABLE    ,DFMAX    ,TDEL     ,
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
     7        GBUF%UELR,NPG      ,DELTAX   ,MFXX     ,MFXY     ,MFXZ     ,                 
     8        MFYX     ,MFYY     , MFYZ    ,MFZX     ,MFZY     ,MFZZ     )
          ELSEIF (IRUPT == 27) THEN                                      
c ---   extended mohr coulomb failure model                  
              CALL FAIL_EMC(
     1        LLT      ,NVARF    ,NPF      ,TF       ,TT       ,         
     2        DT1      ,BUFMAT(IADBUF)     ,NGL      ,IPM      ,MAT      ,         
     3        SS1      ,SS2      ,SS3      ,SS4      ,SS5      ,SS6  ,    
     4        DEFP     ,DPLA     ,EPSP     ,UVARF    ,           
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
     1        LLT     ,NPARAM    ,NVARF    ,NFUNC    ,IFUNC    ,
     2        NPF     ,TF        ,TT       ,DT1      ,BUFMAT(IADBUF)   ,
     3        NGL     ,IPM       ,MAT      ,                   
     4        SS1     ,SS2       ,SS3      ,SS4      ,SS5      ,SS6 ,
     5        DPLA    ,EPSP      ,TSTAR    ,UVARF    ,OFF      ,IP  ,
     6        DFMAX   ,TDEL      ,DELTAX)                     
          ELSEIF (IRUPT == 34) THEN                                      
C  --- Cockroft-Latham failure model             
              CALL FAIL_COCKROFT_S(LLT ,NPARAM,NVARF,
     2        TT      ,DT1       ,BUFMAT(IADBUF),
     3        NGL     ,
     4        EP1     ,EP2       ,EP3      ,EP4      ,EP5           ,EP6 ,
     5        ES1     ,ES2       ,ES3      ,ES4      ,ES5           ,ES6 ,
     6        SS1     ,SS2       ,SS3      ,SS4      ,SS5           ,SS6 ,    
     7        DEFP    ,DPLA      ,EPSP     ,UVARF    ,OFF           ,
     8        DELTAX  ,VOLN      ,DFMAX    ,TDEL) 
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
     2        BUFMAT(IADBUF)     ,NGL      ,IPM      ,MAT      ,DELTAX   ,         
     3        SS1      ,SS2      ,SS3      ,SS4      ,SS5      ,SS6  ,    
     4        DEFP     ,DPLA     ,EPSP     ,TSTAR    ,UVARF    ,           
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
     5          DFMAX    ,TDEL     ,DELTAX) 
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
c---------
          ENDIF ! IRUPT                                                  
c---------
          IF(ISORTH  > 0 .AND. JCVT == 0) THEN  
             CALL MROTENS(LFT,LLT,
     .                 SS1 ,SS2 ,SS3 ,
     .                 SS4 ,SS5 ,SS6 ,
     .                 R11,R21,R31,
     .                 R12,R22,R32,
     .                 R13,R23,R33 )
          ENDIF
C
          DO I=LFT,LLT
            STOR1 = LBUF%SIG(II(1) + I)    
            STOR2 = LBUF%SIG(II(2) + I)
            STOR3 = LBUF%SIG(II(3) + I)
            STOR4 = LBUF%SIG(II(4) + I)
            STOR5 = LBUF%SIG(II(5) + I)
            STOR6 = LBUF%SIG(II(6) + I)
            E1=DXX(I)*(-STOR1+SS1(I))
            E2=DYY(I)*(-STOR2+SS2(I))
            E3=DZZ(I)*(-STOR3+SS3(I))
            E4=D4(I) *(-STOR4+SS4(I))
            E5=D5(I) *(-STOR5+SS5(I))  
            E6=D6(I) *(-STOR6+SS6(I))  
            EINT(I)=LBUF%EINT(I)*LBUF%VOL(I)
     .      +(E1+E2+E3+E4+E5+E6)*(VOLN(I)- UNDEMI*DVOL(I))*DT1*UNDEMI
            LBUF%EINT(I)=EINT(I)/MAX(LBUF%VOL(I),EM30)
          END DO
C
          DO I=LFT,LLT
            LBUF%SIG(II(1) + I) = SS1(I)
            LBUF%SIG(II(2) + I) = SS2(I)
            LBUF%SIG(II(3) + I) = SS3(I)
            LBUF%SIG(II(4) + I) = SS4(I)
            LBUF%SIG(II(5) + I) = SS5(I)
            LBUF%SIG(II(6) + I) = SS6(I) 
          ENDDO 
C
        ENDDO   !  IR = 1,NFAIL
C
C------------------------------------------------------------
C     Variable to regularize with Non-local
C------------------------------------------------------------
        IF (INLOC > 0) THEN 
          IF (ELBUF_TAB(NG)%BUFLY(ILAY)%L_PLA > 0) THEN 
            DO I = LFT,LLT
              VARNL(I) = DEFP(I)
            ENDDO
          ENDIF
        ENDIF 
      ENDIF     !  NFAIL > 0  & USER LAWS
      IF ((ITASK==0).AND.(IMON_MAT==1))CALL STOPTIME(121,1)
C-----------------------------------------------------------------  
      IF(IPARTSPH/=0)THEN
       DO I=LFT,LLT
        IF(OFF(I) > ZERO .AND. OFF(I) < UN)THEN
C replace Solid w/SPH within the same cycle.
          OFF(I)=ZERO
          LBUF%SIG(II(1) + I)  = ZERO
          LBUF%SIG(II(2) + I)  = ZERO
          LBUF%SIG(II(3) + I)  = ZERO
          LBUF%SIG(II(4) + I)  = ZERO
          LBUF%SIG(II(5) + I)  = ZERO
          LBUF%SIG(II(6) + I)  = ZERO
          IF (IVISC > 0) THEN
            LBUF%VISC(II(1) + I) = ZERO
            LBUF%VISC(II(2) + I) = ZERO
            LBUF%VISC(II(3) + I) = ZERO
            LBUF%VISC(II(4) + I) = ZERO
            LBUF%VISC(II(5) + I) = ZERO
            LBUF%VISC(II(6) + I) = ZERO
            IF (ISVIS > 0) THEN                      
c           viscous stress output                  
              LBUF%SIGV(II(1) + I) = ZERO	 
              LBUF%SIGV(II(2) + I) = ZERO	 
              LBUF%SIGV(II(3) + I) = ZERO	 
              LBUF%SIGV(II(4) + I) = ZERO	 
              LBUF%SIGV(II(5) + I) = ZERO	 
              LBUF%SIGV(II(6) + I) = ZERO
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
         DO I=LFT,LLT
            IF(OFF(I) == ZERO) CYCLE
            LBUF%TEMP(I) = TEMPEL(I)
         ENDDO
       ENDIF
c     Flag Idel, test global sur changement de valeur de OFF
      IF(IDEL7NG.GE.1.OR.IDTMINS.GE.1) THEN
        DO I=LFT,LLT
c          IF(OFFOLD(I).GE.1.AND.OFF(I).NE.OFFOLD(I).AND.OFF(I).GE.0.AND.OFF(I).LT.1)THEN
          IF(OFFOLD(I).GE.1.AND.OFF(I).GE.0.AND.OFF(I).LT.1)THEN
            IDEL7NOK = 1
          END IF
        END DO
      END IF
      IF (IMPL_S > 0) THEN
       CALL PUT_ETFAC(NEL ,ET  ,MTN)
       CALL PUTSIGNOR3(LFT,LLT ,MTN,IPTR,IPTS,IPTT,AL_IMP ,SIGNOR)
      END IF
C-----------  
C-----------------------------------------------------------------  
C  ALE/EULER  POST-TREATMENT
C-----------------------------------------------------------------  
      !IF(TT.GE.TANIM)THEN     -> becomes unconditionnal since ssp needed for flux calculation : storage recquired .
        IF(IPARG(7,NG)+IPARG(11,NG) > 0 ) THEN
          LBUF%SSP(LFT:LLT) = CXX(LFT:LLT)
        ENDIF
      !ENDIF      
C-----------------------------------------------------------------         
      RETURN
      END SUBROUTINE MMAIN
C-----
      END MODULE MMAIN_MOD

