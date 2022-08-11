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
Chd|  MULAW                         source/materials/mat_share/mulaw.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ENG_USERLIB_FLAW              source/user_interface/dyn_userlib.c
Chd|        EPSF2U                        source/materials/mat/mat033/sigeps33.F
Chd|        FAIL_BIQUAD_S                 source/materials/fail/biquad/fail_biquad_s.F
Chd|        FAIL_COCKROFT_S               source/materials/fail/cockroft_latham/fail_cockroft_s.F
Chd|        FAIL_EMC                      source/materials/fail/emc/fail_emc.F
Chd|        FAIL_ENERGY_S                 source/materials/fail/energy/fail_energy_s.F
Chd|        FAIL_FLD_TSH                  source/materials/fail/fld/fail_fld_tsh.F
Chd|        FAIL_GENE1_S                  source/materials/fail/gene1/fail_gene1_s.F
Chd|        FAIL_JOHNSON                  source/materials/fail/johnson_cook/fail_johnson.F
Chd|        FAIL_MULLINS_OR_S             source/materials/fail/mullins_or/fail_mullins_OR_s.F
Chd|        FAIL_ORTHBIQUAD_S             source/materials/fail/orthbiquad/fail_orthbiquad_s.F
Chd|        FAIL_ORTHSTRAIN               source/materials/fail/orthstrain/fail_orthstrain_s.F
Chd|        FAIL_RTCL_S                   source/materials/fail/rtcl/fail_rtcl_s.F
Chd|        FAIL_SAHRAEI_S                source/materials/fail/sahraei/fail_sahraei_s.F
Chd|        FAIL_SPALLING_S               source/materials/fail/spalling/fail_spalling_s.F
Chd|        FAIL_TAB_S                    source/materials/fail/tabulated/fail_tab_s.F
Chd|        FAIL_TBUTCHER_S               source/materials/fail/tuler_butcher/fail_tbutcher_s.F
Chd|        FAIL_TENSSTRAIN_S             source/materials/fail/tensstrain/fail_tensstrain_s.F
Chd|        FAIL_VISUAL_S                 source/materials/fail/visual/fail_visual_s.F
Chd|        FAIL_WIERZBICKI_S             source/materials/fail/wierzbicki/fail_wierzbicki_s.F
Chd|        FAIL_WILKINS_S                source/materials/fail/wilkins/fail_wilkins_s.F
Chd|        FMQVISCB                      source/materials/mat_share/mqviscb.F
Chd|        MDTSPH                        source/materials/mat_share/mdtsph.F
Chd|        MQVISCB                       source/materials/mat_share/mqviscb.F
Chd|        MREPLOC                       source/materials/mat_share/mreploc.F
Chd|        MROTENS                       source/materials/mat_share/mrotens.F
Chd|        MROTENSNS                     source/materials/mat_share/mrotens.F
Chd|        MSTRAIN_RATE                  source/materials/mat_share/mstrain_rate.F
Chd|        NSVISUL                       source/materials/mat_share/mnsvis.F
Chd|        SIGEPS100                     source/materials/mat/mat100/sigeps100.F
Chd|        SIGEPS101                     source/materials/mat/mat101/sigeps101.F
Chd|        SIGEPS102                     source/materials/mat/mat102/sigeps102.F
Chd|        SIGEPS103                     source/materials/mat/mat103/sigeps103.F
Chd|        SIGEPS104                     source/materials/mat/mat104/sigeps104.F
Chd|        SIGEPS105                     source/materials/mat/mat105/sigeps105.F
Chd|        SIGEPS106                     source/materials/mat/mat106/sigeps106.F
Chd|        SIGEPS107                     source/materials/mat/mat107/sigeps107.F
Chd|        SIGEPS109                     source/materials/mat/mat109/sigeps109.F
Chd|        SIGEPS111                     source/materials/mat/mat111/sigeps111.F
Chd|        SIGEPS112                     source/materials/mat/mat112/sigeps112.F
Chd|        SIGEPS115                     source/materials/mat/mat115/sigeps115.F
Chd|        SIGEPS120                     source/materials/mat/mat120/sigeps120.F
Chd|        SIGEPS121                     source/materials/mat/mat121/sigeps121.F
Chd|        SIGEPS187                     source/materials/mat/mat187/sigeps187.F
Chd|        SIGEPS28                      source/materials/mat/mat028/sigeps28.F
Chd|        SIGEPS33                      source/materials/mat/mat033/sigeps33.F
Chd|        SIGEPS34                      source/materials/mat/mat034/sigeps34.F
Chd|        SIGEPS35                      source/materials/mat/mat035/sigeps35.F
Chd|        SIGEPS36                      source/materials/mat/mat036/sigeps36.F
Chd|        SIGEPS37                      source/materials/mat/mat037/sigeps37.F
Chd|        SIGEPS38                      source/materials/mat/mat038/sigeps38.F
Chd|        SIGEPS40                      source/materials/mat/mat040/sigeps40.F
Chd|        SIGEPS41                      source/materials/mat/mat041/sigeps41.F
Chd|        SIGEPS42                      source/materials/mat/mat042/sigeps42.F
Chd|        SIGEPS44                      source/materials/mat/mat044/sigeps44.F
Chd|        SIGEPS45                      source/materials/mat/mat045/sigeps45.F
Chd|        SIGEPS48                      source/materials/mat/mat048/sigeps48.F
Chd|        SIGEPS50                      source/materials/mat/mat050/sigeps50.F
Chd|        SIGEPS51                      source/materials/mat/mat051/sigeps51.F
Chd|        SIGEPS52                      source/materials/mat/mat052/sigeps52.F
Chd|        SIGEPS53                      source/materials/mat/mat053/sigeps53.F
Chd|        SIGEPS56                      source/materials/mat/mat056/sigeps56.F
Chd|        SIGEPS60                      source/materials/mat/mat060/sigeps60.F
Chd|        SIGEPS62                      source/materials/mat/mat062/sigeps62.F
Chd|        SIGEPS65                      source/materials/mat/mat065/sigeps65.F
Chd|        SIGEPS66                      source/materials/mat/mat066/sigeps66.F
Chd|        SIGEPS67                      source/materials/mat/mat067/sigeps67.F
Chd|        SIGEPS68                      source/materials/mat/mat068/sigeps68.F
Chd|        SIGEPS69                      source/materials/mat/mat069/sigeps69.F
Chd|        SIGEPS70                      source/materials/mat/mat070/sigeps70.F
Chd|        SIGEPS71                      source/materials/mat/mat071/sigeps71.F
Chd|        SIGEPS72                      source/materials/mat/mat072/sigeps72.F
Chd|        SIGEPS74                      source/materials/mat/mat074/sigeps74.F
Chd|        SIGEPS75                      source/materials/mat/mat075/sigeps75.F
Chd|        SIGEPS76                      source/materials/mat/mat076/sigeps76.F
Chd|        SIGEPS77                      source/materials/mat/mat077/sigeps77.F
Chd|        SIGEPS78                      source/materials/mat/mat078/sigeps78.F
Chd|        SIGEPS79                      source/materials/mat/mat079/sigeps79.F
Chd|        SIGEPS80                      source/materials/mat/mat080/sigeps80.F
Chd|        SIGEPS81                      source/materials/mat/mat081/sigeps81.F
Chd|        SIGEPS82                      source/materials/mat/mat082/sigeps82.F
Chd|        SIGEPS84                      source/materials/mat/mat084/sigeps84.F
Chd|        SIGEPS88                      source/materials/mat/mat088/sigeps88.F
Chd|        SIGEPS90                      source/materials/mat/mat090/sigeps90.F
Chd|        SIGEPS92                      source/materials/mat/mat092/sigeps92.F
Chd|        SIGEPS93                      source/materials/mat/mat093/sigeps93.F
Chd|        SIGEPS94                      source/materials/mat/mat094/sigeps94.F
Chd|        SIGEPS95                      source/materials/mat/mat095/sigeps95.F
Chd|        SIGEPS96                      source/materials/mat/mat096/sigeps96.F
Chd|        SIGEPS97                      source/materials/mat/mat097/sigeps97.F
Chd|        SREPLOC3                      source/elements/solid/solide/sreploc3.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        VISCMAIN                      source/materials/visc/viscmain.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE MULAW (NFT      ,MTN     ,JCVT   ,
     2                  PM      ,OFF     ,SIG      ,EINT    ,RHO    ,
     3                  QOLD    ,VOL     ,STRAIN   ,SIGL    ,GAMA   ,
     4                  UVAR    ,BUFMAT  ,TF       ,NPF     ,STIFN  ,
     5                  MAT     ,NGL     ,NUVAR    ,DT2T    ,NELTST ,
     6                  ITYPTST ,OFFG    ,GEO      ,PID     ,EPSD   ,
     7                  LTEMP   ,WXX     ,
     9                  WYY     ,WZZ     ,JSPH     ,MUMAX   ,SSP    ,
     A                  AIRE    ,VOLN    ,VD2      ,DELTAX  ,VIS    ,  
     B                  D1      ,D2      ,D3       ,D4      ,D5     ,
     C                  D6      ,PNEW    ,PSH      ,Q       ,SSP_EQ ,
     D                  DVOL    ,SOLD1   ,SOLD2    ,SOLD3   ,SOLD4  ,
     E                  SOLD5   ,SOLD6   ,RX       ,RY      ,RZ     ,
     F                  SX      ,SY      ,SZ       ,TX      ,TY     ,
     G                  TZ      ,IPLA    ,SIGY     ,DEFP    ,ISMSTR ,
     H                  MFXX    ,MFXY    ,MFXZ     ,MFYX    ,MFYY   , 
     I                  MFYZ    ,MFZX    ,MFZY     ,MFZZ    ,IPM    ,
     J                  ISORTH  ,FBUF    ,NFAIL    ,NPG     ,SIGDD  ,
     K                  DXY     ,DYX     ,DYZ      ,DZY     ,DZX    ,
     L                  DXZ     ,FR_WAV  ,V       ,VARNL  ,
     M                  W       ,IX      ,X        ,JTHE    ,ET     ,
     N                  MSSA    ,DMELS   ,IPTR     ,IPTS    ,IPTT   ,
     O                  TABLE   ,FVD2    ,FDELTAX  ,FSSP    ,FQVIS  ,
     P                  TEMPEL  ,IGEO     ,SIGV    ,
     Q                  AL_IMP  ,SIGNOR  ,ISTRAIN  ,NG      ,ELBUF_TAB,
     R                  VBUF    ,ILAY    ,VK       ,ALE_CONNECT   ,IPARG  ,
     S                  BUFVOIS ,VDX     ,VDY      ,VDZ     ,IHET   ,
     T                  CONDE   ,ITASK   ,IEXPAN   ,VOL_AVG ,AMU    ,
     U                  EPSTH3  ,EPSTH   ,SVISC    ,NEL     ,ETOTSH ,
     V                  ISELECT ,TSTAR   ,MUOLD    ,DPDM   ,
     W                  RHOREF  ,RHOSP   ,NVARTMP  ,VARTMP  ,EINTTH ,
     X                  MATPARAM_TAB     ,NLOC_DMG )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE ELBUFDEF_MOD            
      USE ALE_CONNECTIVITY_MOD
      USE MATPARAM_DEF_MOD
      USE MESSAGE_MOD
      USE NLOCAL_REG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "nsvis_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "scr19_c.inc"
#include      "usrplas_c.inc"
#include      "userlib.inc"
#include      "impl1_c.inc"
#include      "timeri_c.inc"
#include      "nchara_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFT ,MTN, NUVAR,JCVT,ISORTH,NFAIL,NPG,NVARTMP,
     .        J, JSPH, IPLA,ISMSTR,NELTST, ITYPTST, NUVARR,NG,ILAY,
     .        IX, JTHE, ISROT,ISTRAIN,IPTR, IPTS, IPTT,IHET,IEXPAN,NEL
      INTEGER NPF(*),MAT(MVSIZ),NGL(MVSIZ),PID(MVSIZ),IPM(NPROPMI,*),
     .        IPARG(NPARG,*),IGEO(NPROPGI,*),ITASK,ISELECT
      TARGET  :: IPM
C 
      my_real :: DT2T, BUFVOIS(*)
      my_real :: PM(NPROPM,*), GAMA(*),PNEW(NEL),VARNL(NEL),
     .   OFF(*), SIG(NEL,6), SIGL(NEL,6), STRAIN(NEL,6), EINT(*), 
     .   RHO(*),QOLD(*), VOL(*), BUFMAT(*), TF(*),VK(*),
     .   OFFG(*),STIFN(*),GEO(NPROPG,*),EPSD(*),
     .   WXX(*), WYY(*), WZZ(*), MUMAX(*), SSP(*),
     .   AIRE(*) ,VOLN(*) ,VD2(*) ,DELTAX(*),VIS(*) ,  
     .   SOLD1(MVSIZ) ,SOLD2(MVSIZ) ,SOLD3(MVSIZ) ,
     .   SOLD4(MVSIZ) ,SOLD5(MVSIZ) ,SOLD6(MVSIZ),
     .   D1(MVSIZ) ,D2(MVSIZ),D3(MVSIZ),D4(MVSIZ),D5(MVSIZ),D6(MVSIZ) ,PSH(NEL),
     .   Q(MVSIZ),SSP_EQ(*),DVOL(MVSIZ),
     .   RX(MVSIZ)    ,RY(MVSIZ)    ,RZ(MVSIZ),
     .   SX(MVSIZ)    ,SY(MVSIZ)    ,SZ(MVSIZ)    ,
     .   TX(MVSIZ)    ,TY(MVSIZ)    ,TZ(MVSIZ)   , V(*), W(*), 
     .   MFXX(MVSIZ)  ,MFXY(MVSIZ)  ,MFXZ(MVSIZ)  ,MFYX(MVSIZ) ,MFYY(MVSIZ)   ,MFYZ(MVSIZ)   ,
     .   MFZX(MVSIZ)  ,MFZY(MVSIZ)  ,MFZZ(MVSIZ), FQVIS(*),FSSP(*),
     .   SIGV(NEL,6), SIGY(*),DEFP(NEL), 
     .   DXY(MVSIZ) ,DYX(MVSIZ) ,DYZ(MVSIZ) ,DZY(MVSIZ) ,DZX(MVSIZ) ,DXZ(MVSIZ), SIGDD(NEL,6),
     .   FR_WAV(*),X(*),ET(*), MSSA(*), DMELS(*),FVD2(*),FDELTAX(*),
     .   AL_IMP(MVSIZ),SIGNOR(MVSIZ,6),VDX(*),VDY(*),VDZ(*),EINTTH(MVSIZ),
     .   SVISC(NEL,6),AMU(MVSIZ),DPDM(MVSIZ),VOL_AVG(MVSIZ),EPSTH(NEL),EPSTH3(MVSIZ),
     .   ETOTSH(NEL,6),TSTAR(MVSIZ),MUOLD(NEL),CONDE(*), RHOREF(*), RHOSP(*)
      TARGET :: BUFMAT,DELTAX
      my_real, TARGET, DIMENSION(NUVAR*NEL)   :: UVAR
      my_real, TARGET, DIMENSION(NVARTMP*NEL) :: VARTMP
      my_real, TARGET, DIMENSION(NEL)       :: UVARF1,LTEMP,TEMPEL,SCALE1
      TYPE (BUF_FAIL_), TARGET  :: FBUF
      TYPE (BUF_VISC_)  :: VBUF
      TYPE (TTABLE), DIMENSION(NTABLE) ::  TABLE
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE(MATPARAM_STRUCT_), DIMENSION(NUMMAT),TARGET :: MATPARAM_TAB
      TYPE (NLOCAL_STR_) :: NLOC_DMG 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NV46, NUMEL, INLOC
      INTEGER I,NPAR,IADBUF,NFUNC,NUMTABL,ISRATE,MX,IPG,NPTR,NPTS,
     .  IBID,IBIDON1,IBIDON2,IBIDON3,IBIDON4 ,N48,NIX,ILAW_USER,IGTYP,
     .  NVARF,IR,IP,IRUPT,IMAT,ISVIS,IVISC,NUVARV,ISEQ,IADRUPBUF,IDEV
      my_real 
     .E1,E2,E3,E4,E5,E6,BID1,BID3,Q1,Q2,Q3,SS1,SS2,SS3,SS4,SS5,
     . SS6,WXXF,WYYF,WZZF,P2,EPSP,DAV,ASRATE,C1(MVSIZ),
     . EP1(MVSIZ),EP2(MVSIZ),EP3(MVSIZ),E7(MVSIZ),
     . EP4(2*MVSIZ),EP5(2*MVSIZ),EP6(2*MVSIZ),EINC(MVSIZ),
     . S1(MVSIZ) ,S2(MVSIZ) ,S3(MVSIZ) ,
     . S4(2*MVSIZ) ,S5(2*MVSIZ) ,S6(2*MVSIZ),
     . DE1(MVSIZ),DE2(MVSIZ),DE3(MVSIZ),
     . DE4(2*MVSIZ),DE5(2*MVSIZ),DE6(2*MVSIZ),
     . SO1(MVSIZ),SO2(MVSIZ),SO3(MVSIZ),
     . SO4(2*MVSIZ),SO5(2*MVSIZ),SO6(2*MVSIZ),
     .SXY(MVSIZ),SYX(MVSIZ),SYZ(MVSIZ),SZY(MVSIZ),SZX(MVSIZ),SXZ(MVSIZ),
     .ES1(MVSIZ),ES2(MVSIZ),ES3(MVSIZ),ES4(MVSIZ),ES5(MVSIZ),ES6(MVSIZ),
     .SV1(MVSIZ),SV2(MVSIZ),SV3(MVSIZ),SV4(MVSIZ),SV5(MVSIZ),SV6(MVSIZ),
     .R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),    
     .R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),EPSP1(MVSIZ),DPLA(MVSIZ),
     .PAIR(MVSIZ),DEFP0(MVSIZ)
      my_real  FACQ0
      my_real FPSXX(MVSIZ),FPSYY(MVSIZ),FPSZZ(MVSIZ),FPSXY(MVSIZ),
     *        FPSYZ(MVSIZ),FPSZX(MVSIZ),FPSYX(MVSIZ),FPSZY(MVSIZ),
     *        FPSXZ(MVSIZ),
     *        UPSXX(MVSIZ),UPSYY(MVSIZ),UPSZZ(MVSIZ),UPSXY(MVSIZ),
     *        UPSYZ(MVSIZ),UPSXZ(MVSIZ),TREPSTH(MVSIZ)
      my_real 
     .   RHO0(MVSIZ),BIDON1,BIDON2,BIDON3,BIDON4,BIDON5,POLD
      my_real TT_LOCAL
      my_real, DIMENSION(NEL), TARGET  :: LE_MAX
C----
      my_real, DIMENSION(:), POINTER   :: UPARAM,UPARAMF,UVARF,DFMAX,TDEL,EL_TEMP, 
     .                                    YLDFAC,DAM,EL_LEN
      TYPE(L_BUFEL_)  ,POINTER         :: LBUF
      TYPE(G_BUFEL_)  ,POINTER         :: GBUF      
      TYPE(MATPARAM_STRUCT_) , POINTER :: MATPARAM
      LOGICAL :: LOGICAL_USERL_AVAIL
      my_real :: USER_UELR(MVSIZ)
      INTEGER, DIMENSION(:) ,POINTER   :: FLD_IDX,FOFF,IFUNC,ITABLE
      INTEGER                          :: MAT_COMP,MAT_SMSTR,MAT_FORMU
!
      CHARACTER OPTION*256
      INTEGER SIZE
C=======================================================================
      GBUF   => ELBUF_TAB(NG)%GBUF           
      LBUF   => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IPTR,IPTS,IPTT)
      LOGICAL_USERL_AVAIL=.FALSE.
      IF (USERL_AVAIL>0) LOGICAL_USERL_AVAIL=.TRUE.
      NPTR   = ELBUF_TAB(NG)%NPTR
      NPTS   = ELBUF_TAB(NG)%NPTS
      IPG    = IPTR + ((IPTS-1) + (IPTT-1)*NPTS)*NPTR
      IGTYP  = IPARG(38,NG)
      IF ((IGTYP == 20).OR.(IGTYP == 21).OR.(IGTYP == 22)) THEN
        IPG  = IPTR + ((IPTS-1) + (ILAY-1)*NPTS)*NPTR
      ENDIF
      ISEQ   = ELBUF_TAB(NG)%BUFLY(ILAY)%L_SEQ
      INLOC  = IPARG(78,NG)
      ! Make sure that the non-local variable increment is positive
      IF (INLOC > 0) THEN 
        DO I = 1,NEL
          VARNL(I) = MAX(VARNL(I),ZERO)
        ENDDO
      ENDIF
      ! needed in mqviscb
      FACQ0  = ONE                          
      MX     = MAT(1)                      
      NPAR   = IPM(9,MX)                   
      IADBUF = IPM(7,MX)                   
      NFUNC  = IPM(10,MX)                  
      NUMTABL= IPM(226,MX)                 
      UPARAM => BUFMAT(IADBUF:IADBUF+NPAR) 
      IFUNC  => IPM(10+1:10+NFUNC,MX)      
      ITABLE => IPM(226+1:226+NUMTABL,MX)
      MATPARAM => MATPARAM_TAB(MX)
      !==================================================  
      ! Recovering data from MATPARAM data structure
      ! Material compressibility 
      !   -> 1 : COMPRESSIBLE
      !   -> 2 : INCOMPRESSIBLE
      !   -> 3 : ELASTO-PLASTIC (Default)
      MAT_COMP  = MATPARAM%COMPRESSIBILITY
      ! Strain computation
      !   -> 1 : SMALL STRAIN
      !   -> 2 : LARGE STRAIN (Default)
      MAT_SMSTR = MATPARAM%SMSTR
      ! Reference for strain computation 
      !   -> 1 : INCREMENTAL (Default)
      !   -> 2 : TOTAL 
      MAT_FORMU = MATPARAM%STRAIN_FORMULATION
      !================================================== 
c
      BIDON1 = ZERO
      BIDON2 = ZERO
      BIDON3 = ZERO
      BIDON4 = ZERO
      BIDON5 = ZERO
      BID1 = ZERO
      BID3 = ZERO
      IBID    = 0
      IBIDON1 = 0
      IBIDON2 = 0
      IBIDON3 = 0
      IBIDON4 = 0 
      ILAW_USER = IPM(217, MX)
      IMAT = IPM(1, MX)
      ISVIS = IGEO(35,PID(1))
      IF (IMPL_S >0.AND.IDYNA == 0) ISVIS = 0
c
      DPLA(1:NEL)  = ZERO

      IF (ELBUF_TAB(NG)%BUFLY(ILAY)%L_PLA > 0) THEN
          DEFP0(1:NEL) = LBUF%PLA(1:NEL) 
      ELSE
          DEFP0(1:NEL) = ZERO
      ENDIF


      IF (MTN == 67) THEN
        EL_TEMP => UVAR(NFT+1:NFT+NEL)
      ELSEIF (JTHE == 0 .and. ELBUF_TAB(NG)%BUFLY(ILAY)%L_TEMP > 0) THEN            
        EL_TEMP => LTEMP     ! LBUF%TEMP
      ELSE                                                  
        EL_TEMP => TEMPEL(1:NEL)                           
      ENDIF 

      !initial scale factor for yield stress defined per IPG,NPI  
      SCALE1(1:NEL) = ONE                                        
      YLDFAC => SCALE1(1:NEL)                                   
      IF ( ELBUF_TAB(NG)%BUFLY(ILAY)%L_FAC_YLD > 0) THEN          
         YLDFAC => LBUF%FAC_YLD(1:NEL)                            
      ENDIF                                                       

      DO I=1,NEL
        C1(I)  = PM(32,MX)
        RHO0(I)= PM( 1,MX)
        VIS(I) = ZERO
        EP1(I) = D1(I)*OFF(I)
        EP2(I) = D2(I)*OFF(I)
        EP3(I) = D3(I)*OFF(I)
        EP4(I) = D4(I)*OFF(I)
        EP5(I) = D5(I)*OFF(I)
        EP6(I) = D6(I)*OFF(I)
        SV1(I) = ZERO
        SV2(I) = ZERO
        SV3(I) = ZERO
        SV4(I) = ZERO
        SV5(I) = ZERO
        SV6(I) = ZERO
        E7(I)  = ZERO
      ENDDO 
      IF (MTN==29.OR.MTN==30.OR.MTN==31.OR.MTN==37.OR.MTN==51.OR.MTN==65.OR.MTN==75.OR.MTN==97.OR.MTN==99.OR.MTN==105) THEN
        S1(1:MVSIZ) = ZERO
        S2(1:MVSIZ) = ZERO
        S3(1:MVSIZ) = ZERO
        S4(1:MVSIZ) = ZERO
        S5(1:MVSIZ) = ZERO
        S6(1:MVSIZ) = ZERO
      ENDIF

      IF (JCVT > 0) THEN
C---------------------------
C       ISOTROPIC AND ORTHOTROPIC CONVECTED
C---------------------------
        DO I=1,NEL
         DE1(I) = EP1(I)*DT1
         DE2(I) = EP2(I)*DT1
         DE3(I) = EP3(I)*DT1
         DE4(I) = EP4(I)*DT1
         DE5(I) = EP5(I)*DT1
         DE6(I) = EP6(I)*DT1
         SO1(I) = SIG(I,1)
         SO2(I) = SIG(I,2)
         SO3(I) = SIG(I,3)
         SO4(I) = SIG(I,4)
         SO5(I) = SIG(I,5)
         SO6(I) = SIG(I,6)
         WXX(I)=ZERO
         WYY(I)=ZERO
         WZZ(I)=ZERO 
        ENDDO
C 
      ELSEIF ( MTN==68 ) THEN
C---------------------------
C GLOBAL NONSYMETRIC ORTHOTROPIC TENSOR       
C---------------------------
        CALL MREPLOC(GAMA,R11,R12,R13,R21,R22    ,
     2               R23 ,R31,R32,R33,
     3               RX  ,RY ,RZ ,SX ,SY ,SZ     ,
     4               TX  ,TY ,TZ )
        CALL MROTENSNS(1,NEL,
     .               EP1,DXY,DXZ,
     .               DYX,EP2,DYZ,
     .               DZX,DZY,EP3,
     .               R11,R12,R13,
     .               R21,R22,R23,
     .               R31,R32,R33)
#include "vectorize.inc"
        DO I=1,NEL
         EP4(I) = (DXY(I)+DYX(I))*OFF(I)
         EP5(I) = (DYZ(I)+DZY(I))*OFF(I)
         EP6(I) = (DZX(I)+DXZ(I))*OFF(I)
         EP4(I+NEL) = (DXY(I)-DYX(I))*OFF(I)
         EP5(I+NEL) = (DYZ(I)-DZY(I))*OFF(I)
         EP6(I+NEL) = (DZX(I)-DXZ(I))*OFF(I)

         DE1(I)= EP1(I)*DT1
         DE2(I)= EP2(I)*DT1
         DE3(I)= EP3(I)*DT1
         DE4(I)= EP4(I)*DT1
         DE5(I)= EP5(I)*DT1
         DE6(I)= EP6(I)*DT1
         DE4(I+NEL) = EP4(I+NEL)*DT1
         DE5(I+NEL) = EP5(I+NEL)*DT1
         DE6(I+NEL) = EP6(I+NEL)*DT1

         SO1(I) = SIGL(I,1)
         SO2(I) = SIGL(I,2)
         SO3(I) = SIGL(I,3)
         SO4(I) = SIGL(I,4)
         SO5(I) = SIGL(I,5)
         SO6(I) = SIGL(I,6)

         SO4(I+NEL) = SIGDD(I,4)
         SO5(I+NEL) = SIGDD(I,5)
         SO6(I+NEL) = SIGDD(I,6)
         S4(I+NEL) = ZERO
         S5(I+NEL) = ZERO
         S6(I+NEL) = ZERO

         WXX(I)=ZERO
         WYY(I)=ZERO
         WZZ(I)=ZERO
        ENDDO
      ELSEIF (ISORTH /= 0) THEN
C---------------------------
C       ORTHOTROPIC GLOBAL
C---------------------------
        CALL MREPLOC(GAMA,R11,R12,R13,R21,R22    ,
     2               R23 ,R31,R32,R33,
     3               RX  ,RY ,RZ ,SX ,SY ,SZ     ,
     4               TX  ,TY ,TZ )
        DO I=1,NEL
         EP4(I) = HALF*EP4(I)
         EP5(I) = HALF*EP5(I)
         EP6(I) = HALF*EP6(I)
        ENDDO
        CALL MROTENS(1,NEL,EP1,EP2,EP3,EP4,EP5,EP6,
     .               R11,R12,R13,
     .               R21,R22,R23,
     .               R31,R32,R33)
#include "vectorize.inc"
        DO I=1,NEL
         EP4(I) = TWO*EP4(I)
         EP5(I) = TWO*EP5(I)
         EP6(I) = TWO*EP6(I)
         DE1(I)= EP1(I)*DT1
         DE2(I)= EP2(I)*DT1
         DE3(I)= EP3(I)*DT1
         DE4(I)= EP4(I)*DT1
         DE5(I)= EP5(I)*DT1
         DE6(I)= EP6(I)*DT1
         SO1(I) = SIGL(I,1)
         SO2(I) = SIGL(I,2)
         SO3(I) = SIGL(I,3)
         SO4(I) = SIGL(I,4)
         SO5(I) = SIGL(I,5)
         SO6(I) = SIGL(I,6)
         WXX(I)=ZERO
         WYY(I)=ZERO
         WZZ(I)=ZERO 
        ENDDO
      ELSE
C---------------------------
C       ISOTROPIC GLOBAL
C---------------------------
#include "vectorize.inc"
        DO I=1,NEL
         DE1(I) = EP1(I)*DT1
         DE2(I) = EP2(I)*DT1
         DE3(I) = EP3(I)*DT1
         DE4(I) = EP4(I)*DT1
         DE5(I) = EP5(I)*DT1
         DE6(I) = EP6(I)*DT1
         SO1(I) = SIG(I,1)
         SO2(I) = SIG(I,2)
         SO3(I) = SIG(I,3)
         SO4(I) = SIG(I,4)
         SO5(I) = SIG(I,5)
         SO6(I) = SIG(I,6)
        ENDDO
C
#include "vectorize.inc"
        DO I=1,NEL
             WXXF=WXX(I)*OFF(I)
             WYYF=WYY(I)*OFF(I)
             WZZF=WZZ(I)*OFF(I)
             Q1=STRAIN(I,4)*WZZF
               Q2=STRAIN(I,6)*WYYF
             Q3=STRAIN(I,5)*WXXF
             SS1=STRAIN(I,1)-Q1+Q2
             SS2=STRAIN(I,2)+Q1-Q3
             SS3=STRAIN(I,3)-Q2+Q3
             SS4=STRAIN(I,4)+2.*WZZF*(STRAIN(I,1)-STRAIN(I,2))+
     .           WYYF*STRAIN(I,5)-WXXF*STRAIN(I,6)
             SS5=STRAIN(I,5)+2.*WXXF*(STRAIN(I,2)-STRAIN(I,3))+
     .           WZZF*STRAIN(I,6)-WYYF*STRAIN(I,4)
             SS6=STRAIN(I,6)+2.*WYYF*(STRAIN(I,3)-STRAIN(I,1))+
     .           WXXF*STRAIN(I,4)-WZZF*STRAIN(I,5)
             STRAIN(I,1)= SS1
             STRAIN(I,2)= SS2
             STRAIN(I,3)= SS3
             STRAIN(I,4)= SS4
             STRAIN(I,5)= SS5
             STRAIN(I,6)= SS6
        ENDDO
      ENDIF
c--------------------------
      IF (ISTRAIN > 0) THEN
        DO I=1,NEL
          STRAIN(I,1)= STRAIN(I,1) + DE1(I)
          STRAIN(I,2)= STRAIN(I,2) + DE2(I)
          STRAIN(I,3)= STRAIN(I,3) + DE3(I)
          STRAIN(I,4)= STRAIN(I,4) + DE4(I)
          STRAIN(I,5)= STRAIN(I,5) + DE5(I)
          STRAIN(I,6)= STRAIN(I,6) + DE6(I)
          ES1(I) = STRAIN(I,1)
          ES2(I) = STRAIN(I,2)
          ES3(I) = STRAIN(I,3)
          ES4(I) = STRAIN(I,4)
          ES5(I) = STRAIN(I,5)
          ES6(I) = STRAIN(I,6)
        ENDDO
      ELSE
        DO I=1,NEL
          ES1(I) = ZERO
          ES2(I) = ZERO
          ES3(I) = ZERO
          ES4(I) = ZERO
          ES5(I) = ZERO
          ES6(I) = ZERO
        ENDDO
      ENDIF
c-------------------------------------------------------------------------
c     TOTAL STRAIN
c-------------------------------------------------------------------------
      IF (ISMSTR == 10.OR.ISMSTR == 12) THEN
c       [F] = [M_F]+[1] 
c       only [B] is used for ISMSTR=10,12 now
c------ Use uniform [ES] (instead of [MF]) as other laws
         IF (ISELECT > 0) THEN
          DO I=1,NEL
           ES1(I)=ETOTSH(I,1)*OFF(I)
           ES2(I)=ETOTSH(I,2)*OFF(I)
           ES3(I)=ETOTSH(I,3)*OFF(I)
           ES4(I)=ETOTSH(I,4)*OFF(I)
           ES6(I)=ETOTSH(I,6)*OFF(I)
           ES5(I)=ETOTSH(I,5)*OFF(I)
          END DO
         ELSE
          DO I=1,NEL
           MFXX(I) = OFF(I)*MFXX(I)
           MFYY(I) = OFF(I)*MFYY(I)
           MFZZ(I) = OFF(I)*MFZZ(I)
           MFXY(I) = OFF(I)*MFXY(I)
           MFYX(I) = OFF(I)*MFYX(I)
           MFZX(I) = OFF(I)*MFZX(I)
           MFXZ(I) = OFF(I)*MFXZ(I)
           MFYZ(I) = OFF(I)*MFYZ(I)
           MFZY(I) = OFF(I)*MFZY(I)
          ENDDO
c           Is like 42,62,92,88,90
c           [ES] = [B] - I = [F][F]^t - I
          DO I=1,NEL
            ES1(I)=MFXX(I)*(TWO+MFXX(I))+
     .          MFXY(I)*MFXY(I)+MFXZ(I)*MFXZ(I) 
            ES2(I)=MFYY(I)*(TWO+MFYY(I))+
     .          MFYX(I)*MFYX(I)+MFYZ(I)*MFYZ(I)
            ES3(I)=MFZZ(I)*(TWO+MFZZ(I))+
     .          MFZX(I)*MFZX(I)+MFZY(I)*MFZY(I)
            ES4(I)=MFXY(I)+MFYX(I)+MFXX(I)*MFYX(I)+
     .               MFXY(I)*MFYY(I)+MFXZ(I)*MFYZ(I)
            ES6(I)=MFXZ(I)+MFZX(I)+MFXX(I)*MFZX(I)+
     .               MFXY(I)*MFZY(I)+MFXZ(I)*MFZZ(I)
            ES5(I)=MFZY(I)+MFYZ(I)+MFZX(I)*MFYX(I)+
     .               MFZY(I)*MFYY(I)+MFZZ(I)*MFYZ(I)  
          ENDDO
         END IF !(ISELECT>0) THEN
c
          IF (IDTMIN(1)==3.AND.ISMSTR == 12) THEN
          DO I=1,NEL
           IF (OFFG(I) <=ONE) CYCLE
           ES1(I)=MFXX(I)
           ES2(I)=MFYY(I)
           ES3(I)=MFZZ(I)
           ES4(I)=HALF*(MFXY(I)+MFYX(I))
           ES5(I)=HALF*(MFZY(I)+MFYZ(I))
           ES6(I)=HALF*(MFXZ(I)+MFZX(I))
          ENDDO
          END IF
C---------------------------
C       ORTHOTROPIC GLOBAL
C---------------------------
          IF (JCVT <= 0 .AND. ISORTH /= 0) THEN
c
            CALL MREPLOC(GAMA,R11,R12,R13,R21,R22    ,
     2                   R23 ,R31,R32,R33,
     3                   RX  ,RY ,RZ ,SX ,SY ,SZ     ,
     4                   TX  ,TY ,TZ )
            CALL MROTENS(1,NEL,ES1,ES2,ES3,ES4,ES5,ES6,
     .                   R11,R12,R13,
     .                   R21,R22,R23,
     .                   R31,R32,R33)
          ENDIF
          DO I=1,NEL
            ES4(I) = TWO*ES4(I)
            ES5(I) = TWO*ES5(I)
            ES6(I) = TWO*ES6(I)
          ENDDO
C       ENDIF  ! MTN
c-------------------------------       
      ELSEIF (ISMSTR == 11) THEN
c-------------------------------       
         DO I=1,NEL
           ES1(I)=MFXX(I)
           ES2(I)=MFYY(I)
           ES3(I)=MFZZ(I)
           ES4(I)=MFXY(I)+MFYX(I)
           ES6(I)=MFXZ(I)+MFZX(I)
           ES5(I)=MFZY(I)+MFYZ(I)
         ENDDO
         IF (JCVT == 0 .AND. ISORTH /= 0) THEN
C---------------------------
C       ORTHOTROPIC GLOBAL
C---------------------------
           DO I=1,NEL
            ES4(I) = HALF*ES4(I)
            ES5(I) = HALF*ES5(I)
            ES6(I) = HALF*ES6(I)
           ENDDO
            CALL MREPLOC(GAMA,R11,R12,R13,R21,R22    ,
     2                   R23 ,R31,R32,R33,
     3                   RX  ,RY ,RZ ,SX ,SY ,SZ     ,
     4                   TX  ,TY ,TZ )
            CALL MROTENS(1,NEL,ES1,ES2,ES3,ES4,ES5,ES6,
     .                   R11,R12,R13,
     .                   R21,R22,R23,
     .                   R31,R32,R33)
           DO I=1,NEL
            ES4(I) = TWO*ES4(I)
            ES5(I) = TWO*ES5(I)
            ES6(I) = TWO*ES6(I)
           ENDDO
         ENDIF
C---------------------------
         DO I=1,NEL
          STRAIN(I,1)= ES1(I)
          STRAIN(I,2)= ES2(I)
          STRAIN(I,3)= ES3(I)
          STRAIN(I,4)= ES4(I)
          STRAIN(I,5)= ES5(I)
          STRAIN(I,6)= ES6(I)
         ENDDO
c       THERM STRESS COMPUTATION-----
         IF (IEXPAN > 0) THEN
           DO I=1,NEL
             STRAIN(I,1)= STRAIN(I,1)-EPSTH(I)
             STRAIN(I,2)= STRAIN(I,2)-EPSTH(I)
             STRAIN(I,3)= STRAIN(I,3)-EPSTH(I)
          ENDDO
         ENDIF
      ENDIF  ! ismstr = 10,11,12
      
c---------------------------------------------------------
c     Strain rate filtering coefficient
c---------------------------------------------------------
      ISRATE = IPM(3,MX)
      IF (ISRATE > 0) THEN
        ASRATE = MIN(ONE, PM(9,MX)*DT1)
      ENDIF
c--------------------------------------------------------
c     END TOTAL STRAIN
c---------------------------------------------------------
      IF (MTN == 28) THEN
          CALL SIGEPS28(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     .                  NPF ,TF  ,TT,DT1,BUFMAT,
     .                  RHO0,RHO ,VOLN,EINT,
     .                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     .                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     .                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     .                  SSP ,VIS ,UVAR,OFF  ,NGL  ,0   ,
     .                  IPM ,MAT ,AMU )
      ELSEIF (MTN == 33) THEN
          CALL SIGEPS33(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     .                  NPF ,TF  ,TT,DT1,UPARAM,
     .                  RHO0,RHO ,VOLN,EINT,
     .                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     .                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     .                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     .                  SSP ,VIS ,UVAR,OFF  )
      ELSEIF (MTN == 34) THEN
          CALL SIGEPS34(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     .                  NPF ,TF  ,TT,DT1,UPARAM,
     .                  RHO0,RHO ,VOLN,EINT,
     .                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     .                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     .                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     .                  SSP ,VIS ,UVAR,OFF  )
      ELSEIF (MTN == 35) THEN
          CALL SIGEPS35(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     .                  NPF ,TF  ,TT,DT1,UPARAM,
     .                  RHO0,RHO ,VOLN,EINT,
     .                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     .                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     .                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     .                  SSP ,VIS ,UVAR,OFF  ,ISRATE,ASRATE,
     .                  EPSD)
      ELSEIF (MTN == 36) THEN
        IDEV = 1
        CALL MSTRAIN_RATE(NEL    ,ISRATE ,ASRATE ,EPSD   ,IDEV   ,
     .                    EP1    ,EP2    ,EP3    ,EP4    ,EP5    ,EP6)
c
        CALL SIGEPS36(NEL    ,NPAR   ,NUVAR  ,NFUNC  ,IFUNC  ,
     2                NPF    ,TF     ,TT     ,DT1    ,UPARAM ,
     3                RHO0   ,RHO    ,VOLN   ,EINT   ,
     4                DE1    ,DE2    ,DE3    ,DE4    ,DE5    ,DE6   ,
     5                ES1    ,ES2    ,ES3    ,ES4    ,ES5    ,ES6   ,
     6                SO1    ,SO2    ,SO3    ,SO4    ,SO5    ,SO6   ,
     7                S1     ,S2     ,S3     ,S4     ,S5     ,S6    ,
     8                SSP    ,VIS    ,UVAR   ,OFF    ,NGL    ,0     ,
     9                IPM    ,MAT    ,EPSD   ,IPLA   ,SIGY   ,DEFP  ,
     A                DPLA   ,ET     ,AL_IMP ,SIGNOR ,AMU    ,YLDFAC,
     B                NVARTMP,VARTMP ,LBUF%SIGB)

      ELSEIF (MTN == 37) THEN
          IF (N2D == 0) THEN
            N48 = 8
            NIX = NIXS
          ELSE
            N48 = 4
            NIX = NIXQ
          ENDIF      
          CALL SIGEPS37(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     .                  NPF ,TF  ,TT,DT1,UPARAM,
     .                  RHO0,RHO ,VOLN,EINT,
     .                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     .                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     .                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     .                  SSP ,VIS ,UVAR,OFF  ,IX   ,NIX ,
     .                  NFT)
      ELSEIF (MTN == 38) THEN
          CALL SIGEPS38(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     .                  NPF ,TF  ,TT,DT1,UPARAM,
     .                  RHO0,RHO ,VOLN,EINT,
     .                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     .                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     .                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     .                  SSP ,VIS ,UVAR,OFF  ,
     .                  ISMSTR,MFXX,MFXY,MFXZ,MFYX, 
     .                  MFYY ,MFYZ ,MFZX ,MFZY,MFZZ,ET ,
     .                  IHET ,GBUF%OFF,EPSD )
      ELSEIF (MTN == 40) THEN
          CALL SIGEPS40(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     .                  NPF ,TF  ,TT,DT1,UPARAM,
     .                  RHO0,RHO ,VOLN,EINT,
     .                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     .                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     .                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     .                  SSP ,VIS ,UVAR,OFF  )
      ELSEIF (MTN == 41) THEN
          CALL SIGEPS41(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     .                  NPF ,TF  ,TT,DT1,UPARAM,
     .                  RHO0,RHO ,VOLN,EINT,
     .                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     .                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     .                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     .                  SSP ,VIS ,UVAR,OFF  ,DVOL )
      ELSEIF (MTN == 42) THEN
          CALL SIGEPS42(
     1         NEL     ,NPAR    ,NUVAR   ,NFUNC   ,IFUNC   ,NPF     ,
     2         TF      ,TT      ,DT1     ,UPARAM,RHO0,RHO   ,
     3         VOLN    ,EINT    ,UVAR    ,OFF     ,GBUF%OFF,SSP     ,
     4         EP1     ,EP2     ,EP3     ,EP4     ,EP5     ,EP6     ,
     5         ES1     ,ES2     ,ES3     ,ES4     ,ES5     ,ES6     ,
     6         S1      ,S2      ,S3      ,S4      ,S5      ,S6      ,
     7         MFXX    ,MFXY    ,MFXZ    ,MFYX    ,MFYY    ,MFYZ    ,
     8         MFZX    ,MFZY    ,MFZZ    ,VIS     ,ISMSTR  ,ET      ,
     9         IHET    ,EPSTH3  ,IEXPAN  )
c
      ELSEIF (MTN == 44) THEN
        CALL SIGEPS44(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,UPARAM,
     3                  RHO0,RHO ,VOLN,EINT,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  SSP ,VIS ,UVAR,OFF  ,NGL  ,0   ,
     B                  IPM ,MAT ,EPSD,IPLA ,SIGY ,DEFP,
     C                  DPLA,AMU ,ISRATE ,ASRATE,NVARTMP,
     D                  VARTMP )
      ELSEIF (MTN == 45) THEN
          CALL SIGEPS45(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     .                  NPF ,TF  ,TT,DT1,UPARAM,
     .                  RHO0,RHO ,VOLN,EINT,
     .                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     .                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     .                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     .                  SSP ,VIS ,UVAR,OFF  ,SIGY ,DEFP,
     .                  AMU )
      ELSEIF (MTN == 48) THEN
        IDEV = 1
        CALL MSTRAIN_RATE(NEL    ,ISRATE ,ASRATE ,EPSD   ,IDEV   ,
     .                    EP1    ,EP2    ,EP3    ,EP4    ,EP5    ,EP6)
c
        CALL SIGEPS48(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,BUFMAT,
     3                  RHO0,RHO ,VOLN,EINT,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  SSP ,VIS ,UVAR,OFF  ,NGL  ,0   ,
     B                  IPM ,MAT ,EPSD,SIGY ,DEFP ,DPLA,
     C                  AMU )
      ELSEIF (MTN == 50) THEN
          CALL SIGEPS50(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     .                  NPF ,TF  ,TT,DT1,UPARAM,
     .                  RHO0,RHO ,VOLN,EINT,
     .                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     .                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     .                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     .                  SSP ,VIS ,UVAR,OFF  ,AMU )
      ELSEIF (MTN == 51) THEN
          IF (N2D == 0) THEN
            N48 = 8
            NIX = NIXS
            NUMEL = NUMELS
            NV46=6
          ELSE
            N48 = 4
            NIX = NIXQ
            NUMEL = NUMELQ
            NV46=4
          ENDIF
          !Numerical viscosity is managed inside sigeps51.f        
          !FACQ0 = ZERO
          CALL SIGEPS51(NEL       ,NPAR   ,NUVAR    ,NFUNC  ,IFUNC           ,
     .                  NPF       ,TF     ,TT       ,DT1    ,UPARAM  ,NUMEL        ,
     .                  RHO0      ,RHO    ,VOL      ,EINT   ,SIGY            ,VK           ,  
     .                  EP1       ,EP2    ,EP3      ,EP4    ,EP5             ,EP6          ,
     .                  DE1       ,DE2    ,DE3      ,DE4    ,DE5             ,DE6          ,
     .                  ES1       ,ES2    ,ES3      ,ES4    ,ES5             ,ES6          ,
     .                  SO1       ,SO2    ,SO3      ,SO4    ,SO5             ,SO6          ,
     .                  S1        ,S2     ,S3       ,S4     ,S5              ,S6           ,
     .                  SV1       ,SV2    ,SV3      ,SV4    ,SV5             ,SV6          ,
     .                  SSP       ,VIS    ,UVAR     ,OFF    ,NFT             ,V            ,
     .                  W         ,X      ,IX       ,N48    ,NIX             ,JTHE         ,
     .                  GEO       ,PID    ,ILAY     ,NG     ,ELBUF_TAB       , PM          , 
     .                  IPARG     ,ALE_CONNECT  ,BUFVOIS  ,IPM    ,BUFMAT          ,STIFN        ,
     .                  VD2       ,VDX    ,VDY      ,VDZ    ,MAT             ,GBUF%TAG22   ,
     .                  GBUF%QVIS, DVOL, QOLD       ,NV46)
      ELSEIF (MTN == 52) THEN
        IDEV = 1
        CALL MSTRAIN_RATE(NEL    ,ISRATE ,ASRATE ,EPSD   ,IDEV   ,
     .                    EP1    ,EP2    ,EP3    ,EP4    ,EP5    ,EP6)
c
        CALL SIGEPS52(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,BUFMAT,
     3                  RHO0,RHO ,VOLN,EINT,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  SSP ,VIS ,UVAR,OFF  ,NGL  ,0   ,
     B                  IPM ,MAT ,EPSD,IPLA ,SIGY ,DEFP,
     C                  TABLE )
     
      ELSEIF (MTN == 53) THEN
        CALL SIGEPS53(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,BUFMAT,
     3                  RHO0,RHO ,VOLN,EINT,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  SSP ,VIS ,UVAR,OFF  ,NGL  ,0   ,
     B                  IPM ,MAT ,EPSP,IPLA ,LBUF%SEQ)
      ELSEIF (MTN == 56) THEN
        IDEV = 1
        CALL MSTRAIN_RATE(NEL    ,ISRATE ,ASRATE ,EPSD   ,IDEV   ,
     .                    EP1    ,EP2    ,EP3    ,EP4    ,EP5    ,EP6)
c
        CALL SIGEPS56(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,BUFMAT,
     3                  RHO0,RHO ,VOLN,EINT,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  SSP ,VIS ,UVAR,OFF  ,NGL  ,0   ,
     B                  IPM ,MAT ,EPSD,IPLA ,SIGY ,DEFP,
     C                  DPLA,AMU )
      ELSEIF (MTN == 60) THEN
        IDEV = 1
        CALL MSTRAIN_RATE(NEL    ,ISRATE ,ASRATE ,EPSD   ,IDEV   ,
     .                    EP1    ,EP2    ,EP3    ,EP4    ,EP5    ,EP6)
c
        CALL SIGEPS60(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,BUFMAT,
     3                  RHO0,RHO ,VOLN,EINT,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  SSP ,VIS ,UVAR,OFF  ,NGL  ,0   ,
     B                  IPM  ,MAT ,EPSD,IPLA ,SIGY ,DEFP,
     C                  DPLA ,AMU )
      ELSEIF (MTN == 62) THEN
          CALL SIGEPS62(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,UPARAM,
     3                  RHO0,RHO ,VOLN,EINT,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  SSP ,VIS ,UVAR,OFF  ,ISMSTR,ET ,
     B                  IHET,GBUF%OFF,EPSTH3,IEXPAN)

      ELSEIF (MTN == 65) THEN
C-------------------
C     STRAIN RATE
C-------------------
        DO I=1,NEL
          DAV = (EP1(I)+EP2(I)+EP3(I))*THIRD
          E1 = EP1(I) - DAV
          E2 = EP2(I) - DAV
          E3 = EP3(I) - DAV
          E4 = HALF*EP4(I)
          E5 = HALF*EP5(I)
          E6 = HALF*EP6(I)
          EPSP =HALF*(E1**2+E2**2+E3**2) +E4**2+E5**2+E6**2
          EPSP = SQRT(THREE*EPSP)*THIRD
          EPSD(I)=EPSP
        ENDDO
        CALL SIGEPS65(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,BUFMAT,
     3                  RHO0,RHO ,VOLN,EINT,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  SSP ,VIS ,UVAR,OFF  ,NGL  ,IPM ,
     B                  MAT ,EPSD,IPLA ,SIGY ,DEFP,ET  ,
     C                  DPLA,AMU )

      ELSEIF (MTN == 66) THEN
        IDEV = 1
        CALL MSTRAIN_RATE(NEL    ,ISRATE ,ASRATE ,EPSD   ,IDEV   ,
     .                    EP1    ,EP2    ,EP3    ,EP4    ,EP5    ,EP6)
c
        CALL SIGEPS66(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,BUFMAT,
     3                  RHO0,RHO ,VOLN,EINT,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SSP ,VIS ,UVAR ,OFF  ,NGL  ,IPM ,
     A                  MAT ,EPSD,IPLA ,SIGY ,DEFP ,ET  ,
     B                  AMU )
      ELSEIF (MTN == 67) THEN
          CALL SIGEPS67(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,UPARAM,
     3                  RHO0,RHO ,VOLN,EINT,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  SSP ,VIS ,UVAR,OFF  ,DVOL ,PM(1,MX))
      ELSEIF (MTN == 68) THEN
          CALL SIGEPS68(NEL ,NPAR,NUVAR ,NFUNC ,IFUNC,NPF ,
     .                  TF  ,TT  ,DT1   ,BUFMAT,RHO0 ,RHO ,
     .                  VOLN,EINT,FR_WAV,
     .                  EP1 ,EP2 ,EP3   ,EP4   ,EP5  ,EP6 ,
     .                  DE1 ,DE2 ,DE3   ,DE4   ,DE5  ,DE6 ,
     .                  ES1 ,ES2 ,ES3   ,ES4   ,ES5  ,ES6 ,
     .                  SO1 ,SO2 ,SO3   ,SO4   ,SO5  ,SO6 ,
     .                  S1  ,S2  ,S3    ,S4    ,S5   ,S6  ,
     .                  SV1 ,SV2 ,SV3   ,SV4   ,SV5  ,SV6 ,
     .                  SSP ,VIS ,UVAR  ,OFF   ,NGL  ,0   ,
     .                  IPM ,MAT ,AMU   )
      ELSEIF (MTN == 69) THEN
          NUVARV = IPM(225,MX)
          CALL SIGEPS69(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,BUFMAT,
     3                  RHO0,RHO ,VOLN,EINT,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  SSP ,VIS ,UVAR,OFF  ,WXX  ,WYY ,
     .                  WZZ ,ISMSTR,MFXX,MFXY,MFXZ,MFYX, 
     .                  MFYY ,MFYZ ,MFZX ,MFZY,MFZZ,ET ,
     .                  IHET ,IPM  ,MAT,NUVARV,VBUF%VAR,
     .                  OFFG ,EPSTH3,IEXPAN)
      ELSEIF (MTN == 70) THEN
C-------------------
C     STRAIN RATE
C-------------------
        IDEV = 0
        CALL MSTRAIN_RATE(NEL    ,ISRATE ,ASRATE ,EPSD   ,IDEV   ,
     .                    EP1    ,EP2    ,EP3    ,EP4    ,EP5    ,EP6)
c
C qa=qb=zero by default for law70        
        IF (IGEO(31,PID(1)) == 1) FACQ0 = ZERO
C        
        CALL SIGEPS70(NEL ,NPAR,NUVAR,NFUNC,IFUNC,NPF ,
     2                  TF  ,TT,DT1  ,BUFMAT,RHO0,RHO ,
     3                  VOLN,EINT,OFFG,RHOREF,RHOSP,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  SSP ,VIS ,UVAR,OFF  ,NGL  ,
     B                  PM  ,IPM ,MAT ,EPSD ,ET   )
C
      ELSEIF (MTN == 71) THEN
        CALL SIGEPS71(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,UPARAM,
     3                  RHO0,RHO ,VOLN,EINT,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  SSP ,VIS ,UVAR,OFF  ,NGL ,IPM,
     B                  MAT ,JTHE,TEMPEL,ISMSTR,ET)!,ET
      ELSEIF (MTN == 72) THEN
         CALL SIGEPS72(NEL      ,NPAR     ,NUVAR    ,
     2                 TT       ,DT1      ,UPARAM   ,RHO0     ,RHO      ,
     3                 DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6      ,
     4                 SO1      ,SO2      ,SO3      ,SO4      ,SO5      ,SO6      ,
     5                 S1       ,S2       ,S3       ,S4       ,S5       ,S6       ,
     6                 SSP      ,UVAR     ,OFF      ,NGL      ,SIGY     ,DEFP     ,
     7                 DPLA     ,ET       ,LBUF%SEQ ,LBUF%DMG ,INLOC    ,VARNL    )
      ELSEIF (MTN == 74) THEN
        IDEV = 1
        CALL MSTRAIN_RATE(NEL    ,ISRATE ,ASRATE ,EPSD   ,IDEV   ,
     .                    EP1    ,EP2    ,EP3    ,EP4    ,EP5    ,EP6)
c
        CALL SIGEPS74(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,BUFMAT,
     3                  RHO0,RHO ,VOLN,EINT,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  SSP ,VIS ,UVAR,OFF  ,NGL  ,0   ,
     B                  IPM ,MAT ,EPSD,IPLA ,SIGY ,DEFP,
     C                  DPLA,ET  ,JTHE,TEMPEL,TABLE,LBUF%SEQ,
     D                  AMU ,ISEQ)
C 
      ELSEIF (MTN == 75) THEN
          CALL SIGEPS75(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT  ,DT1  ,UPARAM,
     3                  RHO0,RHO ,VOLN,EINT ,MUOLD,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  SSP ,VIS ,UVAR,OFF  ,DVOL ,VOL ,
     B                  PM  ,IPM ,MAT ,PSH  ,BUFMAT)
C
      ELSEIF (MTN == 76) THEN
        CALL SIGEPS76(NEL      ,NPAR     ,NUVAR    ,NFUNC    ,IFUNC    ,NGL      ,
     .                NPF      ,TF       ,TT       ,DT1      ,UPARAM   ,MATPARAM ,
     .                RHO0     ,RHO      ,DPLA     ,ET       ,SSP      ,SIGY     ,
     .                DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6      ,
     .                SO1      ,SO2      ,SO3      ,SO4      ,SO5      ,SO6      ,
     .                S1       ,S2       ,S3       ,S4       ,S5       ,S6       ,
     .                UVAR     ,OFF      ,EPSD     ,DEFP     ,INLOC    ,VARNL    )
c
      ELSEIF (MTN == 78) THEN
          CALL SIGEPS78(NEL ,NPAR,NUVAR,NFUNC,IFUNC,NPF ,
     .                  TF  ,TT,DT1,UPARAM,RHO0,RHO ,
     .                  LBUF%SIGA,LBUF%SIGB,LBUF%SIGC,UVAR,DEFP,DPLA,
     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     .                  SSP  ,OFF  ,SIGY ,ET ) 
      ELSEIF (MTN == 77) THEN
        IDEV = 0
        CALL MSTRAIN_RATE(NEL    ,ISRATE ,ASRATE ,EPSD   ,IDEV   ,
     .                    EP1    ,EP2    ,EP3    ,EP4    ,EP5    ,EP6)
c
        CALL SIGEPS77(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,UPARAM,
     3                  RHO0,RHO ,VOLN,EINT,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  SSP ,FSSP,VIS ,UVAR,OFF  ,NGL ,
     B                  PM  ,IPM,MAT ,EPSD  ,PAIR )
    
      ELSEIF (MTN == 79) THEN
        IDEV = 1
        CALL MSTRAIN_RATE(NEL    ,ISRATE ,ASRATE ,EPSD   ,IDEV   ,
     .                    EP1    ,EP2    ,EP3    ,EP4    ,EP5    ,EP6)
c
          CALL SIGEPS79(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT  ,DT1  ,UPARAM,
     3                  RHO0,RHO ,VOLN,EINT ,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  SSP ,VIS ,UVAR,OFF  ,DVOL ,VOL ,
     B                  IPM ,MAT ,EPSD,SIGY ,DPLA ,DEFP,
     C                  AMU )
C 
      ELSEIF (MTN == 80) THEN
        CALL SIGEPS80(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,UPARAM,
     3                  RHO0,RHO ,VOLN,EINT,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     A                  SSP ,VIS ,UVAR,OFF  ,NGL  ,IPM ,
     B                  MAT ,EPSD,SIGY,DEFP ,TABLE,TEMPEL,
     C                  NVARTMP   ,VARTMP   ,TREPSTH,EINTTH)
        DO I=1,NEL
             D1(I) = EP1(I)
             D2(I) = EP2(I)
             D3(I) = EP3(I)             
             DVOL(I) = DVOL(I)-TREPSTH(I)*VOL_AVG(I)
        ENDDO

      ELSEIF (MTN == 81) THEN 
          CALL SIGEPS81(NEL   ,NPAR ,NUVAR,NFUNC ,IFUNC ,NGL   ,
     2                  NPF   ,TF   ,TT   ,UPARAM,RHO0  ,RHO   ,
     3                  VOLN  ,AMU  ,DEFP ,SSP   ,VIS   ,UVAR  ,
     4                  EP1   ,EP2  ,EP3  ,EP4   ,EP5   ,EP6   ,
     5                  DE1   ,DE2  ,DE3  ,DE4   ,DE5   ,DE6   ,
     6                  ES1   ,ES2  ,ES3  ,ES4   ,ES5   ,ES6   ,
     7                  SO1   ,SO2  ,SO3  ,SO4   ,SO5   ,SO6   ,
     8                  S1    ,S2   ,S3   ,S4    ,S5    ,S6    ,
     9                  SV1   ,SV2  ,SV3  )


      ELSEIF (MTN == 82) THEN 
          CALL SIGEPS82(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,UPARAM,
     3                  RHO0,RHO ,VOLN,EINT,NGL,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  SSP ,VIS ,UVAR,OFF  ,ISMSTR,ET ,
     B                  MFXX,MFXY,MFXZ,MFYX, MFYY ,MFYZ ,
     C                  MFZX ,MFZY,MFZZ,IHET,GBUF%OFF ,
     D                  EPSTH3,IEXPAN)     
      ELSEIF (MTN == 84) THEN
          CALL SIGEPS84(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     .                  NPF ,TF  ,TT,DT1,UPARAM,
     .                  RHO0,RHO ,VOLN,EINT,TEMPEL,NGL ,
     .                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     .                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     .                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     .                  SSP ,VIS ,UVAR,OFF  ,SIGY ,DEFP,
     .                  DPLA, ET ,IPM ,MAT  ) 
      ELSEIF (MTN == 88) THEN 
          CALL SIGEPS88(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,UPARAM,
     3                  RHO0,RHO ,VOLN,EINT,NGL,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  SSP ,VIS ,UVAR,OFF  ,ISMSTR,ISRATE,
     B                  ASRATE,ET ,IHET,GBUF%OFF,EPSTH3,IEXPAN,
     C                  EPSD )  
      ELSEIF (MTN == 90) THEN
C-------------------
C     visco-hypereslatic law defined by stress strain curve
C-------------------       
        CALL SIGEPS90(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,UPARAM,
     3                  RHO0,RHO ,VOLN,EINT,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     A                  SSP ,VIS ,UVAR,OFF  ,NGL  ,ISMSTR,
     B                  ISRATE,ASRATE, GBUF%OFF ,IHET ,ET ,EPSD  ) 
C      
      ELSEIF (MTN == 92) THEN 
          CALL SIGEPS92(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,UPARAM,
     3                  RHO0,RHO ,VOLN,EINT,NGL,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  SSP ,VIS ,UVAR,OFF  ,ISMSTR,ET ,
     B                  IHET,GBUF%OFF ,EPSTH3,IEXPAN, LBUF%EPSA)   
       ELSEIF (MTN == 94) THEN 
          CALL SIGEPS94(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,UPARAM,
     3                  RHO0,RHO ,VOLN,EINT,NGL,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  SSP ,VIS ,UVAR,OFF  ,ISMSTR,ET ,
     B                  IHET,GBUF%OFF,EPSTH3,IEXPAN )       
       ELSEIF (MTN == 93) THEN 
          CALL SIGEPS93(NEL    ,NPAR   ,NUVAR  ,NFUNC  ,IFUNC  ,
     2                  NPF    ,TF     ,TT     ,DT1    ,UPARAM ,
     3                  EP1    ,EP2    ,EP3    ,EP4    ,EP5    ,EP6    ,
     4                  DE1    ,DE2    ,DE3    ,DE4    ,DE5    ,DE6    ,
     5                  SO1    ,SO2    ,SO3    ,SO4    ,SO5    ,SO6    ,  
     6                  S1     ,S2     ,S3     ,S4     ,S5     ,S6     ,
     7                  SSP    ,DEFP   ,UVAR   ,RHO0   ,OFF    ,
     8                  ET     ,SIGY   ,LBUF%SEQ,EPSD  ,ASRATE ,
     9                  NVARTMP,VARTMP ,DPLA   )     
      ELSEIF (MTN == 95) THEN 
        IADRUPBUF =1
        IR = INT (BUFMAT(IADBUF+20))
        IF (IR>ZERO) THEN
           UVARF => FBUF%FLOC(IR)%VAR  
           NVARF  = FBUF%FLOC(IR)%NVAR 
           IP=(IR -1)*15  
           IADRUPBUF = IPM(114+IP ,MX)
        ELSE
           NVARF = 1                               
           UVARF => UVARF1                            
        ENDIF
          CALL SIGEPS95(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,UPARAM,
     3                  RHO0,RHO ,VOLN,EINT,NGL,
     4                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  MFXX,MFXY,MFXZ,MFYX ,MFYY ,MFYZ,
     B                  MFZX ,MFZY,MFZZ,
     c                  SSP ,VIS ,UVAR,OFF  ,ISMSTR,ET ,
     d                  IHET,GBUF%OFF ,EPSTH3,IEXPAN , 
     E                  BUFMAT(IADRUPBUF) ,UVARF, NVARF,
     F                  JCVT , GBUF%GAMA_R) !gama_r is for the corotational formulation
c
       ELSEIF (MTN == 96) THEN 
          CALL SIGEPS96(
     1         NEL     ,NGL     ,NPAR    ,NUVAR    ,NFUNC   ,IFUNC   ,
     2         NPF     ,TF      ,UPARAM,UVAR,JTHE   ,
     3         RHO     ,TEMPEL  ,DEFP    ,SSP      ,GBUF%OFF,EPSD    ,
     4         EP1     ,EP2     ,EP3     ,EP4      ,EP5     ,EP6     ,
     5         DE1     ,DE2     ,DE3     ,DE4      ,DE5     ,DE6     , 
     6         SO1     ,SO2     ,SO3     ,SO4      ,SO5     ,SO6     , 
     7         S1      ,S2      ,S3      ,S4       ,S5      ,S6      ) 
c
      ELSEIF (MTN == 97) THEN
          IF (N2D == 0) THEN
            N48 = 8
            NIX = NIXS
          ELSE
            N48 = 4
            NIX = NIXQ
          ENDIF
          CALL SIGEPS97(NEL       ,NPAR   ,NUVAR    ,NFUNC  ,IFUNC           ,LBUF%TB   ,
     .                  NPF       ,TF     ,TT       ,DT1    ,UPARAM  ,LBUF%BFRAC,
     .                  RHO0      ,RHO    ,VOL      ,EINT   ,SIGY            ,DELTAX    , 
     .                  EP1       ,EP2    ,EP3      ,EP4    ,EP5             ,EP6       ,
     .                  DE1       ,DE2    ,DE3      ,DE4    ,DE5             ,DE6       ,
     .                  ES1       ,ES2    ,ES3      ,ES4    ,ES5             ,ES6       ,
     .                  SO1       ,SO2    ,SO3      ,SO4    ,SO5             ,SO6       ,
     .                  S1        ,S2     ,S3       ,S4     ,S5              ,S6        ,
     .                  SV1       ,SV2    ,SV3      ,SV4    ,SV5             ,SV6       ,
     .                  SSP       ,VIS    ,UVAR     ,OFF    ,NFT             ,V         ,
     .                  W         ,X      ,IX       ,N48    ,NIX             ,JTHE      ,
     .                  GEO       ,PID    ,ILAY     ,NG     ,ELBUF_TAB       ,PM        , 
     .                  IPARG     ,BUFVOIS  ,IPM    ,BUFMAT          ,STIFN     ,
     .                  VD2       ,VDX    ,VDY      ,VDZ    ,MAT             ,VOLN      ,
     .                  GBUF%QVIS ,DVOL   ,QOLD     )   
      ELSEIF (MTN == 100) THEN 
        IADRUPBUF =1
        IR = INT (BUFMAT(IADBUF+2))         
        IF (IR>ZERO) THEN
           UVARF => FBUF%FLOC(IR)%VAR  
           NVARF  = FBUF%FLOC(IR)%NVAR 
           IP=(IR -1)*15  
           IADRUPBUF = IPM(114+IP ,MX)
        ELSE
           NVARF = 1                               
           UVARF => UVARF1                            
        ENDIF
        CALL SIGEPS100(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,UPARAM,
     3                  RHO0,RHO ,VOLN,EINT,NGL,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                  MFXX,MFXY,MFXZ,MFYX ,MFYY ,MFYZ,
     B                  MFZX ,MFZY,MFZZ,TEMPEL,
     C                  SSP ,VIS ,UVAR,OFF  ,ISMSTR,ET ,
     D                  IHET,GBUF%OFF ,EPSTH,IEXPAN , 
     E                  BUFMAT(IADRUPBUF) ,UVARF, NVARF,
     F                  JCVT , GBUF%GAMA_R)   !gama_r is for the corotational formulation

      ELSEIF (MTN == 101) THEN 
C structure loading
           CALL SREPLOC3(
     .       RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .       R11  ,R21  ,R31  ,R12  ,R22  ,R32  ,R13  ,R23  ,R33  )
            DO I=1,NEL
             FPSXX(I) = ONE+MFXX(I)
             FPSYY(I) = ONE+MFYY(I)
             FPSZZ(I) = ONE+MFZZ(I)
            END DO
            FPSXY(1:MVSIZ) = MFXY(1:MVSIZ)
            FPSYZ(1:MVSIZ) = MFYZ(1:MVSIZ)
            FPSXZ(1:MVSIZ) = MFXZ(1:MVSIZ)          
            FPSYX(1:MVSIZ) = MFYX(1:MVSIZ)
            FPSZY(1:MVSIZ) = MFZY(1:MVSIZ)
            FPSZX(1:MVSIZ) = MFZX(1:MVSIZ)    

            IF (ISORTH /= 0) THEN
             CALL MREPLOC(GAMA,R11,R12,R13,R21,R22    ,
     2                    R23 ,R31,R32,R33,
     3                    RX  ,RY ,RZ ,SX ,SY ,SZ     ,
     4                    TX  ,TY ,TZ )
             CALL MROTENSNS(1,NEL,
     .               FPSXX,FPSXY,FPSXZ,
     .               FPSYX,FPSYY,FPSYZ,
     .               FPSZX,FPSZY,FPSZZ,
     .               R11,R12,R13,
     .               R21,R22,R23,
     .               R31,R32,R33)
            END IF !(ISORTH /= 0) THEN
           CALL EPSF2U(
     1       NEL    ,FPSXX    , FPSXY  , FPSXZ  , FPSYX  ,    
     2       FPSYY  ,FPSYZ    , FPSZX  , FPSZY  , FPSZZ  , 
     3       UPSXX  ,UPSYY    , UPSZZ  , UPSXY  , UPSYZ  ,
     4       UPSXZ  )
        CALL SIGEPS101(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                  NPF ,TF  ,TT,DT1,UPARAM,
     3                  RHO0,RHO ,VOLN,EINT,NGL,
     5                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     C                  SSP ,VIS ,UVAR,OFF  ,ISMSTR,ET ,
     D                  IHET,GBUF%OFF ,EPSTH,IEXPAN,TEMPEL,
     1                  FPSXX    , FPSXY  , FPSXZ  , FPSYX  ,    
     2                  FPSYY  ,FPSYZ    , FPSZX  , FPSZY  , FPSZZ  , 
     3                  UPSXX  ,UPSYY    , UPSZZ  , UPSXY  , UPSYZ  ,
     4                  UPSXZ  )   

      ELSEIF (MTN == 102) THEN 

        CALL SIGEPS102(NEL    ,NPAR    ,NUVAR  ,UPARAM , RHO0     ,RHO    ,
     2                 DE1    ,DE2     ,DE3    ,DE4    , DE5      ,DE6    ,
     3                 SO1    ,SO2     ,SO3    ,SO4    , SO5      ,SO6    ,
     4                 S1     ,S2      ,S3     ,S4     , S5       ,S6     ,
     5                 SSP    ,UVAR    ,OFF    ,ET     ,
     6                 PSH    ,PNEW    ,DPDM   ,SSP    , LBUF%PLA ) 
c
C
      ELSEIF (MTN == 103) THEN 
        IDEV = 1
        CALL MSTRAIN_RATE(NEL    ,ISRATE ,ASRATE ,EPSD   ,IDEV   ,
     .                    EP1    ,EP2    ,EP3    ,EP4    ,EP5    ,EP6)
c
        CALL SIGEPS103(NEL    ,NPAR     ,NUVAR  ,NFUNC  , IFUNC         ,
     B                 NPF    ,TF       ,TT     ,DT1    , UPARAM,
     C                 RHO0   ,RHO      ,VOLN   ,EINT   ,
     D                 DE1    ,DE2      ,DE3    ,DE4    , DE5           ,DE6  ,
     E                 ES1    ,ES2      ,ES3    ,ES4    , ES5           ,ES6  ,
     F                 SO1    ,SO2      ,SO3    ,SO4    , SO5           ,SO6  ,
     G                 S1     ,S2       ,S3     ,S4     , S5            ,S6   ,
     H                 SV1    ,SV2      ,SV3    ,SV4    , SV5           ,SV6  ,
     I                 SSP    ,VIS      ,UVAR   ,OFF    , DPDM          ,
     J                 EPSD   ,LTEMP    ,TEMPEL ,JTHE   , LBUF%PLA      )
C
c
      ELSEIF (MTN == 104) THEN 
c 
        CALL SIGEPS104(NEL    ,NGL    ,NPAR   ,NUVAR  ,
     .                 TT     ,DT1    ,UPARAM ,UVAR   ,JTHE   ,LBUF%OFF,
     .                 RHO0   ,RHO    ,DEFP   ,DPLA   ,EPSD   ,SSP    ,
     .                 DE1    ,DE2    ,DE3    ,DE4    ,DE5    ,DE6    ,
     .                 SO1    ,SO2    ,SO3    ,SO4    ,SO5    ,SO6    ,
     .                 S1     ,S2     ,S3     ,S4     ,S5     ,S6     ,
     .                 SIGY   ,ET     ,TEMPEL ,VARNL  ,OFF    ,IPG    ,
     .                 LBUF%DMG,LBUF%TEMP,LBUF%SEQ,NPG,ELBUF_TAB(NG))
c
      ELSEIF (MTN == 105) THEN
          IF (N2D == 0) THEN
            N48 = 8
            NIX = NIXS
          ELSE
            N48 = 4
            NIX = NIXQ
          ENDIF
          CALL SIGEPS105(NEL       ,NPAR   ,NUVAR    ,NFUNC      ,IFUNC           ,LBUF%TB   ,
     .                   NPF       ,TF     ,TT       ,DT1        ,UPARAM  ,LBUF%BFRAC,
     .                   RHO0      ,RHO    ,VOL      ,EINT       ,SIGY            ,DELTAX    , 
     .                   EP1       ,EP2    ,EP3      ,EP4        ,EP5             ,EP6       ,
     .                   DE1       ,DE2    ,DE3      ,DE4        ,DE5             ,DE6       ,
     .                   ES1       ,ES2    ,ES3      ,ES4        ,ES5             ,ES6       ,
     .                   SO1       ,SO2    ,SO3      ,SO4        ,SO5             ,SO6       ,
     .                   S1        ,S2     ,S3       ,S4         ,S5              ,S6        ,
     .                   SV1       ,SV2    ,SV3      ,SV4        ,SV5             ,SV6       ,
     .                   SSP       ,VIS    ,UVAR     ,OFF        ,NFT             ,V         ,
     .                   W         ,X      ,IX       ,N48        ,NIX             ,JTHE      ,
     .                   GEO       ,PID    ,ILAY     ,NG         ,ELBUF_TAB       ,PM        , 
     .                   IPARG     ,BUFVOIS  ,IPM        ,BUFMAT          ,STIFN     ,
     .                   VD2       ,VDX    ,VDY      ,VDZ        ,MAT             ,VOLN      ,
     .                   GBUF%QVIS ,DVOL   ,QOLD     )   
C
      ELSEIF (MTN == 106) THEN 
        IDEV = 1
        CALL MSTRAIN_RATE(NEL    ,ISRATE ,ASRATE ,EPSD   ,IDEV   ,
     .                    EP1    ,EP2    ,EP3    ,EP4    ,EP5    ,EP6)
c
         CALL SIGEPS106(NEL    ,NPAR     ,NUVAR  ,NFUNC  , IFUNC         ,
     B                  NPF    ,TF       ,TT     ,DT1    , UPARAM,
     C                  RHO0   ,RHO      ,VOLN   ,EINT   ,
     D                  DE1    ,DE2      ,DE3    ,DE4    , DE5           ,DE6  ,
     E                  ES1    ,ES2      ,ES3    ,ES4    , ES5           ,ES6  ,
     F                  SO1    ,SO2      ,SO3    ,SO4    , SO5           ,SO6  ,
     G                  S1     ,S2       ,S3     ,S4     , S5            ,S6   ,
     H                  SV1    ,SV2      ,SV3    ,SV4    , SV5           ,SV6  ,
     I                  SSP    ,VIS      ,UVAR   ,OFF    , LBUF%PLA      ,DPLA ,
     J                  EPSD   ,EL_TEMP  ,JTHE   ,NFT    )
C
      ELSEIF (MTN == 107) THEN 
c 
         CALL SIGEPS107(NEL    ,NGL    ,NPAR   ,NUVAR  ,TT     ,DT1    ,
     .                  UPARAM ,UVAR   ,JTHE   ,OFF    ,RHO0   ,RHO    ,
     .                  LBUF%PLA,DPLA  ,LBUF%EPSD,SSP    ,
     .                  DE1    ,DE2    ,DE3    ,DE4    ,DE5    ,DE6    ,
     .                  SO1    ,SO2    ,SO3    ,SO4    ,SO5    ,SO6    ,
     .                  S1     ,S2     ,S3     ,S4     ,S5     ,S6     ,
     .                  SIGY   ,ET     ,
     .                  NVARTMP,NUMTABL,VARTMP ,ITABLE ,TABLE  )
C
      ELSEIF (MTN == 109) THEN 
         CALL SIGEPS109(
     1        NEL      ,NGL     ,NPAR     ,NUVAR    ,NVARTMP  ,NUMTABL  ,
     2        UPARAM   ,UVAR    ,VARTMP   ,ITABLE   ,TABLE    ,JTHE     ,
     3        TT       ,DT1     ,OFF      ,RHO      ,LBUF%PLA ,DPLA     , 
     4        SSP      ,SIGY    ,ET       ,EL_TEMP  ,EPSD     ,
     5        DE1      ,DE2     ,DE3      ,DE4      ,DE5      ,DE6      , 
     6        SO1      ,SO2     ,SO3      ,SO4      ,SO5      ,SO6      , 
     7        S1       ,S2      ,S3       ,S4       ,S5       ,S6       ,
     8        INLOC    ,VARNL   )
C
      ELSEIF (MTN == 111) THEN 
          CALL SIGEPS111(NEL ,NPAR,NUVAR,NFUNC,IFUNC,
     2                   NPF ,TF  ,TT,DT1,UPARAM,
     3                   RHO0,RHO ,VOLN,EINT,NGL,
     4                   EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     5                   DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     6                   ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     7                   SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     8                   S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     9                   SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     A                   SSP ,VIS ,UVAR,OFF  ,ISMSTR,ET ,
     B                   IHET,GBUF%OFF ,EPSTH3,IEXPAN,   
     A                   MFXX,MFXY,MFXZ,MFYX ,MFYY ,MFYZ,
     B                   MFZX ,MFZY,MFZZ)
C
      ELSEIF (MTN == 112) THEN 
c 
        CALL SIGEPS112(NEL    ,NGL    ,NPAR   ,NUVAR  ,TT     ,DT1    ,
     .                 UPARAM ,UVAR   ,JTHE   ,OFF    ,RHO0   ,RHO    ,
     .                 LBUF%PLA,DPLA  ,LBUF%EPSD      ,SSP    ,ES3    ,
     .                 DE1    ,DE2    ,DE3    ,DE4    ,DE5    ,DE6    ,
     .                 SO1    ,SO2    ,SO3    ,SO4    ,SO5    ,SO6    ,
     .                 S1     ,S2     ,S3     ,S4     ,S5     ,S6     ,
     .                 SIGY   ,ET     ,
     .                 NVARTMP,NUMTABL,VARTMP ,ITABLE   ,TABLE   )
c
      ELSEIF (MTN == 115) THEN 
c 
        CALL SIGEPS115(NEL    ,NGL    ,NPAR   ,NUVAR  ,GBUF%RHO,
     .                 TT     ,DT1    ,UPARAM ,UVAR   ,OFF    ,SIGY   ,
     .                 RHO0   ,DEFP   ,DPLA   ,SSP    ,ET     ,LBUF%SEQ,
     .                 DE1    ,DE2    ,DE3    ,DE4    ,DE5    ,DE6    ,
     .                 SO1    ,SO2    ,SO3    ,SO4    ,SO5    ,SO6    ,
     .                 S1     ,S2     ,S3     ,S4     ,S5     ,S6     )
c
      ELSEIF (MTN == 120) THEN 
c       TAPO (toughened adhesive polymer)

        CALL SIGEPS120(NEL    ,NPAR   ,NUVAR  ,NVARTMP,NUMTABL,ITABLE ,
     .                 TABLE  ,TT     ,DT1    ,SSP    ,UVAR   ,VARTMP ,
     .                 UPARAM ,NGL    ,OFF    ,DEFP   ,EPSD   ,TEMPEL ,
     .                 EP1    ,EP2    ,EP3    ,EP4    ,EP5    ,EP6    ,
     .                 DE1    ,DE2    ,DE3    ,DE4    ,DE5    ,DE6    ,
     .                 SO1    ,SO2    ,SO3    ,SO4    ,SO5    ,SO6    ,
     .                 S1     ,S2     ,S3     ,S4     ,S5     ,S6     ,
     .                 JTHE   ,INLOC  ,VARNL  )
c
      ELSEIF (MTN == 121) THEN 
c 
        CALL SIGEPS121(NEL    ,NGL    ,NPAR   ,NUVAR  ,NFUNC  ,IFUNC  ,
     .                 NPF    ,TF     ,DT1    ,TT     ,UPARAM ,UVAR   ,
     .                 RHO    ,DEFP   ,DPLA   ,SSP    ,EPSD   ,OFF    ,
     .                 LBUF%OFF,
     .                 DE1    ,DE2    ,DE3    ,DE4    ,DE5    ,DE6    ,
     .                 EP1    ,EP2    ,EP3    ,EP4    ,EP5    ,EP6    ,
     .                 SO1    ,SO2    ,SO3    ,SO4    ,SO5    ,SO6    ,
     .                 S1     ,S2     ,S3     ,S4     ,S5     ,S6     ,
     .                 SIGY   ,ET     ,VARNL  ,INLOC  ,GBUF%DT,
     .                 IPG    ,NPG    ,ELBUF_TAB(NG))
c
      ELSEIF (MTN == 187) THEN !BARLAT 2000
          CALL SIGEPS187(NEL   ,NPAR  ,NUVAR ,NFUNC ,IFUNC ,
     .                   NPF   ,TF    ,TT    ,DT1   ,UPARAM,
     .                   RHO0  ,RHO   ,VOLN  ,EINT  ,TEMPEL,NGL   ,
     .                   EP1   ,EP2   ,EP3   ,EP4   ,EP5   ,EP6   ,
     .                   DE1   ,DE2   ,DE3   ,DE4   ,DE5   ,DE6   ,
     .                   ES1   ,ES2   ,ES3   ,ES4   ,ES5   ,ES6   ,
     .                   SO1   ,SO2   ,SO3   ,SO4   ,SO5   ,SO6   ,
     .                   S1    ,S2    ,S3    ,S4    ,S5    ,S6    ,
     .                   SV1   ,SV2   ,SV3   ,SV4   ,SV5   ,SV6   ,
     .                   SSP   ,VIS   ,UVAR  ,OFF   ,SIGY  ,DEFP  ,
     .                   DPLA  ,ET    ,IPM   ,MAT   ,ISRATE,
     .                   YLDFAC,EPSP  ) 

c----------------------------------------
      ENDIF  ! MTN
C=======================================================================
C                  FAILURE MODEL 
C=======================================================================
      IF ((ITASK==0).AND.(IMON_MAT==1))CALL STARTIME(121,1)
      IF (NFAIL > 0) THEN
c
        ! Failure criterion parameters scaling
        IF (INLOC > 0) THEN
          ! -> Length used for failure criterion parameters scaling is LE_MAX
          LE_MAX(1:NEL) = NLOC_DMG%LE_MAX(MAT(1))
          EL_LEN => LE_MAX(1:NEL) 
        ELSE
          ! -> Length used for failure criterion parameters scaling is LE_MAX
          EL_LEN => DELTAX(1:NEL) 
        ENDIF
c
        IF (ELBUF_TAB(NG)%BUFLY(ILAY)%L_PLA > 0) THEN
C for non-local plastic law
          IF (INLOC > 0) THEN 
            DO I=1,NEL
              DPLA(I)  = MAX(VARNL(I),ZERO)
            ENDDO
          ELSE
C for all plastic law
            DO I=1,NEL 
              DPLA(I)  = DEFP(I) - DEFP0(I)
            ENDDO
          ENDIF
        ELSE
          DO I=1,NEL 
            DPLA(I)  = ZERO
          ENDDO
        ENDIF
c        
c        NUMBER_FAILURE = IPM(220, MX)
c        NUVARR = IPM(221, MX)
c
        DO IR = 1,NFAIL
          IP=(IR -1)*15         
C------
          IF (MTN == 36.OR.MTN == 44.OR.MTN == 48.OR.MTN == 56.OR.
     .        MTN == 60.OR.MTN == 76.OR.MTN == 104.OR.MTN == 112.OR.
     .        MTN == 121) THEN
             DO I=1,NEL
              EPSP1(I) = EPSD(I)
             ENDDO
           ELSE
             DO I=1,NEL 
              DAV = (EP1(I)+EP2(I)+EP3(I))*THIRD
              E1 = EP1(I) - DAV
              E2 = EP2(I) - DAV
              E3 = EP3(I) - DAV
              E4 = HALF*EP4(I)
              E5 = HALF*EP5(I)
              E6 = HALF*EP6(I)
              EPSP1(I) =HALF*(E1**2+E2**2+E3**2) +E4**2+E5**2+E6**2
              EPSP1(I) = SQRT(THREE*EPSP1(I))*TWO_THIRD
             ENDDO
           ENDIF
C------------------------------------------------------------
C     Recovering non-local plastic strain-rate
C------------------------------------------------------------
          IF (ELBUF_TAB(NG)%BUFLY(ILAY)%L_PLA > 0) THEN
            IF (INLOC > 0) THEN 
              DO I = 1,NEL
                EPSP1(I) = MAX(VARNL(I),ZERO)/MAX(DT1,EM20)
              ENDDO
            ENDIF
          ENDIF
c----  
          UVARF => FBUF%FLOC(IR)%VAR  
          IRUPT  = FBUF%FLOC(IR)%ILAWF 
          NVARF  = FBUF%FLOC(IR)%NVAR 
          DFMAX => FBUF%FLOC(IR)%DAMMX 
          TDEL  => FBUF%FLOC(IR)%TDEL   
          NPAR   = IPM(112+IP, MX)   
          IADBUF = IPM(114+IP ,MX)
          UPARAMF => BUFMAT(IADBUF:IADBUF+NPAR) 
          NFUNC  = IPM(115+IP, MX)             
          FLD_IDX=> FBUF%FLOC(IR)%INDX
          FOFF   => FBUF%FLOC(IR)%OFF
          DAM    => FBUF%FLOC(IR)%DAM
          IFUNC  => IPM(115+IP+1:115+IP+NFUNC, MX)         
          IF (IRUPT == 1) THEN                                            
C---- Johnson cook            
              CALL FAIL_JOHNSON(NEL ,NPAR,NVARF,NFUNC,IFUNC,
     2                    NPF ,TF  ,TT  ,DT1  ,UPARAMF,
     3                    NGL ,IPM ,MAT ,
     4                    S1  ,S2  ,S3  ,S4   ,S5   ,S6,
     5                    DPLA,EPSP1,TSTAR,UVARF,OFF,IP,
     6                    DFMAX,TDEL )                   
          ELSEIF (IRUPT == 2) THEN                                        
C           Tuler Butcher                                                
               CALL FAIL_TBUTCHER_S(NEL ,NPAR,NVARF,NFUNC,IFUNC,
     2                    NPF ,TF  ,TT  ,DT1  ,UPARAMF,
     3                    NGL ,IPM ,MAT ,
     4                    S1  ,S2  ,S3  ,S4   ,S5   ,S6,
     5                   UVARF,OFF ,IP  ,DFMAX,TDEL )                                    
          ELSEIF (IRUPT == 3) THEN                                        
C   wilkins          
               CALL FAIL_WILKINS_S(NEL ,NPAR,NUVARR,NFUNC,IFUNC,
     2                    NPF ,TF  ,TT  ,DT1  ,UPARAMF,
     3                    NGL ,IPM ,MAT ,
     4                    S1  ,S2  ,S3  ,S4   ,S5   ,S6,
     5                    DPLA,UVARF,OFF,IP   ,DFMAX,TDEL )                               
          ELSEIF (IRUPT == 4) THEN                                         
C   user1         
            IF (LOGICAL_USERL_AVAIL) THEN
               USER_UELR(1:NEL)=GBUF%UELR(1:NEL)

               TT_LOCAL = TT
               CALL ENG_USERLIB_FLAW(IRUPT,NEL ,NPAR,NUVARR,NFUNC,IFUNC,
     2                    NPF ,TF  ,TT_LOCAL  ,DT1  ,UPARAMF,
     3                    NGL ,IBIDON1,IBIDON2,IBIDON3 ,IBIDON4,
     4                    EP1  ,EP2 ,EP3  ,EP4  ,EP5  ,EP6 ,
     5                    ES1  ,ES2 ,ES3  ,ES4  ,ES5  ,ES6 ,
     6                    S1   ,S2  ,S3   ,S4   ,S5   ,S6  ,    
     7                    DEFP ,DPLA,EPSP1,UVARF,OFF  ,                    
     8                    DELTAX,VOLN,USER_UELR,BIDON4,BIDON5)
               GBUF%UELR(1:NEL) = USER_UELR(1:NEL)
            ELSE
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/FAIL/USER1 - SOLID '
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
            ENDIF
          ELSEIF (IRUPT == 5) THEN                                         
C   user2         
            IF (LOGICAL_USERL_AVAIL) THEN
               TT_LOCAL = TT
               USER_UELR(1:NEL)=GBUF%UELR(1:NEL)
               CALL ENG_USERLIB_FLAW(IRUPT,NEL ,NPAR,NUVARR,NFUNC,IFUNC,
     2                    NPF ,TF  ,TT_LOCAL  ,DT1  ,UPARAMF,
     3                    NGL ,IBIDON1,IBIDON2,IBIDON3 ,IBIDON4,
     4                    EP1  ,EP2 ,EP3  ,EP4  ,EP5  ,EP6 ,
     5                    ES1  ,ES2 ,ES3  ,ES4  ,ES5  ,ES6 ,
     6                    S1   ,S2  ,S3   ,S4   ,S5   ,S6  ,    
     7                    DEFP ,DPLA,EPSP1,UVARF,OFF  ,                    
     8                    DELTAX,VOLN,USER_UELR,BIDON4,BIDON5)
               GBUF%UELR(1:NEL) = USER_UELR(1:NEL)
            ELSE
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/FAIL/USER2 - SOLID '
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
            ENDIF
          ELSEIF (IRUPT == 6) THEN                                         
C   user3         
           IF (LOGICAL_USERL_AVAIL) THEN
               TT_LOCAL = TT
               USER_UELR(1:NEL)=GBUF%UELR(1:NEL)
               CALL ENG_USERLIB_FLAW(IRUPT,NEL ,NPAR,NUVARR,NFUNC,IFUNC,
     2                    NPF  ,TF  ,TT_LOCAL  ,DT1  ,UPARAMF,
     3                    NGL  ,IBIDON1,IBIDON2,IBIDON3 ,IBIDON4,
     4                    EP1  ,EP2 ,EP3  ,EP4  ,EP5  ,EP6 ,
     5                    ES1  ,ES2 ,ES3  ,ES4  ,ES5  ,ES6 ,
     6                    S1   ,S2  ,S3   ,S4   ,S5   ,S6  ,    
     7                    DEFP ,DPLA,EPSP1,UVARF,OFF  ,                    
     8                    DELTAX,VOLN,USER_UELR,BIDON4,BIDON5)
               GBUF%UELR(1:NEL) = USER_UELR(1:NEL)
            ELSE
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/FAIL/USER3 - SOLID '
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
            ENDIF
          ELSEIF (IRUPT == 7) THEN                                        
                  CALL FAIL_FLD_TSH(
     1            NEL       ,NPAR      ,NVARF     ,NFUNC    ,IFUNC      ,
     2            NPF       ,TF        ,TT        ,DT1      ,UPARAMF,   
     3            UVARF     ,NGL       ,IPG       ,ILAY     ,IPTT         ,
     5            ES1       ,ES2       ,ES4       ,ES5      ,ES6          ,
     6            OFF       ,FOFF      ,FLD_IDX   ,DAM      ,DFMAX     )
          ELSEIF (IRUPT == 8) THEN                                         
C---- Johnson cook + spalling          
              CALL FAIL_SPALLING_S(NEL ,NPAR,NVARF,NFUNC,IFUNC,
     2                    NPF ,TF  ,TT  ,DT1  ,UPARAMF,
     3                    NGL ,IPM ,MAT ,
     4                    S1  ,S2  ,S3  ,S4   ,S5   ,S6      ,
     5                    DPLA,EPSP1,TSTAR,UVARF,OFF         ,
     6                    DFMAX,TDEL ,OFFG)                   
          ELSEIF (IRUPT == 9) THEN                                         
C---- wierzbicki
              CALL FAIL_WIERZBICKI_S(NEL ,NPAR,NUVARR,NFUNC,IFUNC,
     2                    NPF ,TF  ,TT  ,DT1  ,UPARAMF,
     3                    NGL ,IPM ,MAT ,
     4                    S1  ,S2  ,S3  ,S4   ,S5   ,S6,
     5                    DPLA,DEFP,UVARF,OFF ,DFMAX,
     6                    TDEL )                          
          ELSEIF (IRUPT == 10) THEN                                        
C---- strain tension            
              CALL FAIL_TENSSTRAIN_S(NEL ,NPAR,NVARF,NFUNC,IFUNC       ,
     2             NPF ,TF  ,TT  ,DT1  ,UPARAMF,
     3             NGL ,IPM ,MAT ,DELTAX ,TSTAR, ISMSTR,
     4             ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6     ,
     5             S1  ,S2  ,S3  ,S4   ,S5   ,S6      ,
     6             EPSP1,UVARF,OFF ,IP   ,DFMAX,TDEL  ,
     7             MFXX     ,MFXY     ,MFXZ     ,MFYX     ,MFYY     , MFYZ    ,                 
     8             MFZX     ,MFZY     ,MFZZ     )
          ELSEIF (IRUPT == 11) THEN                                        
C---- energy failure  
              CALL FAIL_ENERGY_S(NEL ,NPAR,NUVARR,NFUNC,IFUNC       ,
     2                    NPF ,TF  ,TT  ,DT1  ,UPARAMF,
     3                    NGL ,IPM ,MAT ,
     5                    S1  ,S2  ,S3  ,S4   ,S5   ,S6      ,
     6                  VOLN,EINT,EPSP1,UVARF,OFF,IP,DFMAX   ,
     7                   TDEL )                    
          ELSEIF (IRUPT == 23) THEN                                      
C---- tabulated failure model
               CALL FAIL_TAB_S(
     1         NEL      ,NVARF    ,NPF      ,TF       ,TT       ,        
     2         UPARAMF  ,NGL      ,IPM      ,MAT      ,EL_LEN   ,         
     3         S1       ,S2       ,S3       ,S4       ,S5       ,S6      ,    
     4         DEFP     ,DPLA     ,EPSP1    ,TSTAR    ,UVARF    ,             
     5         OFF      ,IP       ,TABLE    ,DFMAX    ,TDEL     ,
     6         NFUNC    ,IFUNC    )                                      
          ELSEIF (IRUPT == 24) THEN                                      
c   --- Orthotropic strain failure
              CALL FAIL_ORTHSTRAIN(
     1        NEL      ,NPAR     ,NVARF    ,NFUNC    ,IFUNC    ,
     2        NPF      ,TF       ,TT       ,DT1      ,UPARAMF  ,ISMSTR   ,
     3        EP1      ,EP2      ,EP3      ,EP4      ,EP5      ,EP6      ,
     4        ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6      ,
     5        S1       ,S2       ,S3       ,S4       ,S5       ,S6       ,
     6        UVARF    ,OFF      ,IPG      ,NGL      ,DFMAX    ,TDEL     ,
     7        GBUF%UELR,NPG      ,DELTAX   ,MFXX     ,MFXY     ,MFXZ     ,                 
     8        MFYX     ,MFYY     , MFYZ    ,MFZX     ,MFZY     ,MFZZ     )
          ELSEIF (IRUPT == 27) THEN                                      
c ---   extended mohr coulomb failure model                                             
              CALL FAIL_EMC(
     1         NEL      ,NVARF    ,NPF      ,TF       ,TT       ,        
     2         DT1      ,UPARAMF     ,NGL      ,IPM      ,MAT      ,        
     3         S1       ,S2       ,S3       ,S4       ,S5       ,S6,    
     4         DEFP     ,DPLA     ,EPSP1    ,UVARF ,             
     5         OFF      ,IP       ,DFMAX    ,TDEL    )                                      
          ELSEIF (IRUPT == 29) THEN                             
c ---   MIT Wierzbicki Sahraei electric battery failure                                               
              CALL FAIL_SAHRAEI_S(
     1        NEL      ,NFUNC    ,IFUNC    ,NPF      ,TF       ,
     2        TT       ,NGL      ,IPM      ,MAT      ,UPARAMF  ,
     3        ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6      , 
     4        OFF      ,IP       ,DFMAX    ,TDEL     ,DELTAX   ,NVARF    ,
     5        UVARF    ) 
          ELSEIF (IRUPT == 30) THEN                                      
c  --- Biquadratic failure model                                  
              CALL FAIL_BIQUAD_S(
     1        NEL      ,NPAR     ,NVARF    ,NFUNC    ,IFUNC    ,
     2        NPF      ,TF       ,TT       ,DT1      ,UPARAMF   ,
     3        NGL      ,IPM      ,MAT      ,                   
     4        S1       ,S2       ,S3       ,S4       ,S5       ,S6 ,
     5        DPLA     ,EPSP1    ,TSTAR    ,UVARF    ,OFF,IP   ,
     6        DFMAX    ,TDEL     ,EL_LEN   )    
          ELSEIF (IRUPT == 33. AND . MTN /= 100 .AND. MTN /= 95 ) THEN                                
c  --- Mullins Ogden-Roxburgh dammage model                                  
              CALL FAIL_MULLINS_OR_S(
     1        NEL      ,NPAR     ,NVARF    ,NPF      ,TF       ,
     2        TT       ,DT1      ,UPARAMF,NGL , 
     3        MAT      ,S1       ,S2       ,S3       ,S4       ,
     4        S5       ,S6       ,UVARF    ,OFF      ,IP       ,
     5        DFMAX    ,LBUF%EPSA) 
          ELSEIF (IRUPT == 34) THEN                                           
C  --- Cockroft-Latham failure model             
              CALL FAIL_COCKROFT_S(NEL    ,NPAR     ,NVARF,
     1        TT      ,DT1       ,UPARAMF,
     2        NGL     ,
     3        EP1     ,EP2       ,EP3      ,EP4      ,EP5           ,EP6 ,
     4        ES1     ,ES2       ,ES3      ,ES4      ,ES5           ,ES6 ,
     5        S1      ,S2        ,S3       ,S4       ,S5            ,S6  ,
     6        DEFP    ,DPLA      ,EPSP     ,UVARF    ,OFF           ,
     7        DFMAX   ,TDEL      ) 
          ELSEIF (IRUPT == 36) THEN                                           
C  --- VISUAL failure model             
              CALL FAIL_VISUAL_S(
     .        NEL     ,NPAR     ,NVARF     ,TT      ,DT1      ,UPARAMF ,
     .        ES1     ,ES2       ,ES3      ,ES4     ,ES5      ,ES6     ,
     .        S1      ,S2        ,S3       ,S4      ,S5       ,S6      ,
     .        UVARF   ,OFF       ,NGL      ,DFMAX   ,ISMSTR   ) 
          ELSEIF (IRUPT == 37) THEN                                      
c ---       tabulated failure model (old, obsolete version)
              CALL FAIL_TAB_OLD_S(
     1         NEL      ,NVARF    ,NPF      ,TF       ,TT       ,        
     2         UPARAMF  ,NGL      ,IPM      ,MAT      ,EL_LEN   ,
     3         S1       ,S2       ,S3       ,S4       ,S5       ,S6    ,    
     4         DEFP     ,DPLA     ,EPSP1    ,TSTAR    ,UVARF    ,             
     5         OFF      ,IP       ,DFMAX    ,TDEL     ,
     6         NFUNC    ,IFUNC    )     
c
          ELSEIF (IRUPT == 38) THEN                                      
c  --- Orthotropic biquadratic failure model                                  
              CALL FAIL_ORTHBIQUAD_S(
     1          NEL      ,NPAR     ,NVARF    ,NFUNC    ,IFUNC    ,
     2          NPF      ,TF       ,TT       ,DT1      ,UPARAMF  ,
     3          NGL      ,DPLA     ,EPSP1    ,UVARF    ,OFF      ,                
     4          S1       ,S2       ,S3       ,S4       ,S5       ,S6       ,
     5          DFMAX    ,TDEL     ,EL_LEN   )    
c
          ELSEIF (IRUPT == 39) THEN                                      
c  --- GENE1 failure model                                  
              CALL FAIL_GENE1_S(
     1          NEL      ,NPAR     ,NVARF    ,NFUNC    ,IFUNC    ,LBUF%OFF ,
     2          NPF      ,TF       ,TT       ,DT1      ,UPARAMF  ,IPG      ,
     3          NGL      ,GBUF%DT  ,EPSP1    ,UVARF    ,OFF      ,NPG      ,     
     4          ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6      ,           
     5          S1       ,S2       ,S3       ,S4       ,S5       ,S6       ,
     6          TEMPEL   ,VOLN     ,DFMAX    ,TDEL     ,DELTAX   ,TABLE    ,
     7          IR       ,ELBUF_TAB(NG),ILAY )
c
          ELSEIF (IRUPT == 40) THEN                                      
c  --- RTCL failure model                                  
              CALL FAIL_RTCL_S(
     1          NEL      ,NPAR     ,NVARF    ,TT       ,DT1      ,UPARAMF  ,
     2          S1       ,S2       ,S3       ,S4       ,S5       ,S6       ,
     3          NGL      ,DPLA     ,UVARF    ,OFF      ,DFMAX    ,TDEL     )
          ENDIF
        ENDDO ! IR = 1,NFAIL
      ENDIF   ! NFAIL
      IF ((ITASK==0).AND.(IMON_MAT==1))CALL STOPTIME(121,1)
C     viscous stress (/VISC models) 
      IVISC = IPM(222,MX)
      IF (IVISC > 0 .AND. MTN /= 24) THEN
         CALL VISCMAIN(
     .        NEL     ,MX  ,IVISC   ,IPM     ,BUFMAT  ,
     .        VBUF%VAR,RHO0    ,VIS     ,SSP     ,DT1     , 
     .        EP1     ,EP2     ,EP3     ,EP4     ,EP5     ,EP6     ,
     .        SV1     ,SV2     ,SV3     ,SV4     ,SV5     ,SV6     )
       ENDIF 
C  viscosity ( Navier-Stokes)
      IF (ISVIS > 0) THEN
      CALL NSVISUL(NEL     ,PM      ,OFF     ,RHO     ,GEO    ,
     2             PID     ,SSP     ,AIRE    ,VOLN    ,EP1    ,
     3             EP2     ,EP3     ,EP4     ,EP5     ,EP6    ,
     4             SV1     ,SV2     ,SV3     ,SV4     ,SV5    ,
     5             SV6     ,S3      ,ES3     ,RHO0    ,RHOREF )
      ENDIF 
C=======================================================================
      IF ( MTN==68 ) THEN
c       orthotropic global nonsymetric tensor
        IF (JCVT == 0) THEN
#include "vectorize.inc"
         DO I=1,NEL
           S1(I) = S1(I)*OFF(I)
           S2(I) = S2(I)*OFF(I)
           S3(I) = S3(I)*OFF(I)
           S4(I) = S4(I)*OFF(I)
           S5(I) = S5(I)*OFF(I)
           S6(I) = S6(I)*OFF(I)
           S4(I+NEL) = S4(I+NEL)*OFF(I)
           S5(I+NEL) = S5(I+NEL)*OFF(I)
           S6(I+NEL) = S6(I+NEL)*OFF(I)

           SIGL(I,1) = S1(I)
           SIGL(I,2) = S2(I)
           SIGL(I,3) = S3(I)
           SIGL(I,4) = S4(I)
           SIGL(I,5) = S5(I)
           SIGL(I,6) = S6(I)
           SIGDD(I,4) = S4(I+NEL)
           SIGDD(I,5) = S5(I+NEL)
           SIGDD(I,6) = S6(I+NEL)
c E7 rotation energy
           E7(I) = EP4(I+NEL)*(SO4(I+NEL)+SIGDD(I,4))
     .           + EP5(I+NEL)*(SO5(I+NEL)+SIGDD(I,5))
     .           + EP6(I+NEL)*(SO6(I+NEL)+SIGDD(I,6))

           SXY(I) = S4(I)+S4(I+NEL)
           SYZ(I) = S5(I)+S5(I+NEL)
           SZX(I) = S6(I)+S6(I+NEL)
           SYX(I) = S4(I)-S4(I+NEL)
           SZY(I) = S5(I)-S5(I+NEL)
           SXZ(I) = S6(I)-S6(I+NEL)

           SV1(I)= SV1(I)*OFF(I)
           SV2(I)= SV2(I)*OFF(I)
           SV3(I)= SV3(I)*OFF(I)
           SV4(I)= SV4(I)*OFF(I)
           SV5(I)= SV5(I)*OFF(I)
           SV6(I)= SV6(I)*OFF(I)
         ENDDO
         CALL MROTENSNS(1,NEL,
     .              S1 ,SXY,SXZ,
     .              SYX,S2 ,SYZ,
     .              SZX,SZY,S3 ,
     .              R11,R21,R31,
     .              R12,R22,R32,
     .              R13,R23,R33)
         DO I=1,NEL
           SIGDD(I,1) = (SXY(I)-SYX(I))*HALF
           SIGDD(I,2) = (SYZ(I)-SZY(I))*HALF
           SIGDD(I,3) = (SZX(I)-SXZ(I))*HALF
           S4(I) = (SXY(I)+SYX(I))*HALF
           S5(I) = (SYZ(I)+SZY(I))*HALF
           S6(I) = (SZX(I)+SXZ(I))*HALF
         ENDDO
         CALL MROTENS(1,NEL,
     .              SV1 ,SV2 ,SV3 ,
     .              SV4 ,SV5 ,SV6 ,
     .              R11,R21,R31,
     .              R12,R22,R32,
     .              R13,R23,R33)
        ENDIF
      ELSEIF (ISORTH /= 0) THEN
        IF (JCVT == 0) THEN
         DO 30 I=1,NEL
         SIGL(I,1) = S1(I)*OFF(I)
         SIGL(I,2) = S2(I)*OFF(I)
         SIGL(I,3) = S3(I)*OFF(I)
         SIGL(I,4) = S4(I)*OFF(I)
         SIGL(I,5) = S5(I)*OFF(I)
         SIGL(I,6) = S6(I)*OFF(I)
         SV1(I)= SV1(I)*OFF(I)
         SV2(I)= SV2(I)*OFF(I)
         SV3(I)= SV3(I)*OFF(I)
         SV4(I)= SV4(I)*OFF(I)
         SV5(I)= SV5(I)*OFF(I)
         SV6(I)= SV6(I)*OFF(I)
   30    CONTINUE
         CALL MROTENS(1,NEL,
     .              S1 ,S2 ,S3 ,
     .              S4 ,S5 ,S6 ,
     .              R11,R21,R31,
     .              R12,R22,R32,
     .              R13,R23,R33)
         CALL MROTENS(1,NEL,
     .              SV1 ,SV2 ,SV3 ,
     .              SV4 ,SV5 ,SV6 ,
     .              R11,R21,R31,
     .              R12,R22,R32,
     .              R13,R23,R33)
        ELSE
C        JCVT > 0
         DO I=1,NEL
           SV1(I)= SV1(I)*OFF(I)
           SV2(I)= SV2(I)*OFF(I)
           SV3(I)= SV3(I)*OFF(I)
           SV4(I)= SV4(I)*OFF(I)
           SV5(I)= SV5(I)*OFF(I)
           SV6(I)= SV6(I)*OFF(I)
         ENDDO
        ENDIF
      ENDIF
C------------------------------------------------------------
      DO I=1,NEL
        SIG(I,1) = S1(I)*OFF(I)
        SIG(I,2) = S2(I)*OFF(I)
        SIG(I,3) = S3(I)*OFF(I)
        SIG(I,4) = S4(I)*OFF(I)
        SIG(I,5) = S5(I)*OFF(I)
        SIG(I,6) = S6(I)*OFF(I)
        SVIS(I,1)= SV1(I)*OFF(I)
        SVIS(I,2)= SV2(I)*OFF(I)
        SVIS(I,3)= SV3(I)*OFF(I)
        SVIS(I,4)= SV4(I)*OFF(I)
        SVIS(I,5)= SV5(I)*OFF(I)
        SVIS(I,6)= SV6(I)*OFF(I)
      ENDDO
C
C  isvis for outp
C      
      IF (ISVIS > 0) THEN
        DO I=1,NEL
            SIGV(I,1) = SVIS(I,1)
            SIGV(I,2) = SVIS(I,2)
            SIGV(I,3) = SVIS(I,3)
            SIGV(I,4) = SVIS(I,4)
            SIGV(I,5) = SVIS(I,5)
            SIGV(I,6) = SVIS(I,6)
          ENDDO
      ENDIF 
       IF (IVISC > 0) THEN
         DO I=1,NEL  
           SVISC(I,1) = SVIS(I,1)
           SVISC(I,2) = SVIS(I,2)
           SVISC(I,3) = SVIS(I,3)
           SVISC(I,4) = SVIS(I,4)
           SVISC(I,5) = SVIS(I,5)
           SVISC(I,6) = SVIS(I,6)
        ENDDO    
      ENDIF  
C------------------------------------------------------------
C     Variable to regularize with Non-local
C------------------------------------------------------------
      IF (ELBUF_TAB(NG)%BUFLY(ILAY)%L_PLA > 0) THEN
        IF (INLOC > 0) THEN 
          DO I=1,NEL
            VARNL(I) = DEFP(I)
          ENDDO
        ENDIF
      ENDIF
c------------------------------------------------------------
c     define sound speed  (in all case)
c     define dynamic viscosity (for viscous law)
c-----------------------
      DO I=1,NEL
        IF (SSP(I) == ZERO) SSP(I)=SQRT(C1(I)/RHO0(I))
      ENDDO
c-------------------------------------------
c   bulk viscosity and time step computation
c   this subroutine return the new bulk viscosity q
c-----------------------
      IF (JSPH == 0) THEN  
        CALL MQVISCB(                                            
     1           PM      ,OFF     ,RHO    ,BID1    ,EL_TEMP     ,SSP   ,   
     2           BID3    ,STIFN   ,DT2T   ,NELTST  ,ITYPTST     ,AIRE  ,
     3           OFFG    ,GEO     ,PID    ,VOLN    ,VD2         ,DELTAX,
     4           VIS     ,D1      ,D2      ,D3     ,PNEW        ,PSH   ,
     5           MAT     ,NGL     ,Q       ,SSP_EQ ,VOL     ,MSSA      ,
     6           DMELS   ,IGEO    ,FACQ0   ,CONDE  ,GBUF%DT ,GBUF%G_DT ,
     7           IPM     ,RHOREF  ,RHOSP   )
!------------------------------------------------
      ELSE
        CALL MDTSPH(                                             
     1           PM      ,OFF     ,RHO     ,BID1      ,EL_TEMP ,       
     2           BID3    ,STIFN   ,DT2T    ,NELTST    ,ITYPTST ,
     3           OFFG    ,GEO     ,PID     ,MUMAX     ,SSP     ,
     4           VOLN    ,VD2     ,DELTAX  ,VIS       ,D1      ,
     5           D2      ,D3      ,PNEW    ,PSH       ,MAT     ,
     6           NGL     ,Q       ,SSP_EQ  ,GBUF%G_DT ,GBUF%DT)
      ENDIF
c-----------
      IF (MTN == 77 ) CALL FMQVISCB (                         
     .                NEL ,PM   ,GEO , PID  ,MAT   ,NGL   ,   
     .                  NELTST,ITYPTST,DT2T, UVAR ,
     .                  FSSP ,OFF ,OFFG, AIRE, FDELTAX ,VIS  ,
     .                  FVD2,FQVIS) 
C-----------
      IF (MTN == 67) THEN
        DO I=1,NEL
         EINT(I)=EINT(I)-(Q(I)+QOLD(I))*DVOL(I)*HALF
        END DO  
      ELSEIF (MTN == 24) THEN
        DO I=1,NEL
          E1 = D1(I)*(SOLD1(I)+SIG(I,1) + SVIS(I,1))
          E2 = D2(I)*(SOLD2(I)+SIG(I,2) + SVIS(I,2))
          E3 = D3(I)*(SOLD3(I)+SIG(I,3) + SVIS(I,3))
          E4 = D4(I)*(SOLD4(I)+SIG(I,4) + SVIS(I,4))
          E5 = D5(I)*(SOLD5(I)+SIG(I,5) + SVIS(I,5))
          E6 = D6(I)*(SOLD6(I)+SIG(I,6) + SVIS(I,6))
          EINT(I) = EINT(I)
     .            - (Q(I)+QOLD(I))*DVOL(I)*HALF
     .            + (E1+E2+E3+E4+E5+E6)*VOL_AVG(I)*DT1*HALF
        END DO
      ELSEIF (MTN == 77 ) THEN
       DO I=1,NEL
        P2 = -(SIG(I,1) + SIG(I,2)+SIG(I,3)) * THIRD - PAIR(I) 
        E1 = D1(I)*(SIG(I,1) + PAIR(I) +P2+SVIS(I,1))
        E2 = D2(I)*(SIG(I,2) + PAIR(I) +P2+SVIS(I,2))
        E3 = D3(I)*(SIG(I,3) + PAIR(I) +P2+SVIS(I,3))
        E4 = D4(I)*(SIG(I,4) ) + SVIS(I,4)
        E5 = D5(I)*(SIG(I,5) ) + SVIS(I,5)
        E6 = D6(I)*(SIG(I,6) ) + SVIS(I,6)
        EINT(I)=EINT(I)-(Q(I)+QOLD(I)+P2)*DVOL(I)+(E1+E2+E3+E4+E5+E6+E7(I))*VOL_AVG(I)*DT1
       END DO
      ELSEIF (MTN == 51 ) THEN   
       DO I=1,NEL
         P2 = -(SOLD1(I)+SIG(I,1)+SOLD2(I)+SIG(I,2)+SOLD3(I)+SIG(I,3))* THIRD
         E1 = D1(I)*(SOLD1(I)+SIG(I,1)+P2 + TWO*SVIS(I,1))
         E2 = D2(I)*(SOLD2(I)+SIG(I,2)+P2 + TWO*SVIS(I,2))
         E3 = D3(I)*(SOLD3(I)+SIG(I,3)+P2 + TWO*SVIS(I,3))
         E4 = D4(I)*(SOLD4(I)+SIG(I,4)    + TWO*SVIS(I,4))
         E5 = D5(I)*(SOLD5(I)+SIG(I,5)    + TWO*SVIS(I,5))
         E6 = D6(I)*(SOLD6(I)+SIG(I,6)    + TWO*SVIS(I,6))
         EINT(I)=EINT(I)+(E1+E2+E3+E4+E5+E6)*VOL_AVG(I)*DT1*HALF
       END DO 
      ELSEIF (MTN == 102) THEN 
       !case of material law using /eos
       DO I=1,NEL
         P2  = -(SOLD1(I)+SIG(I,1)+SOLD2(I)+SIG(I,2)+SOLD3(I)+SIG(I,3))* THIRD
         POLD= -(SOLD1(I)+SOLD2(I)+SOLD3(I))* THIRD
         E1  =  D1(I)*(SOLD1(I)+SIG(I,1) + P2+TWO*SVIS(I,1))
         E2  =  D2(I)*(SOLD2(I)+SIG(I,2) + P2+TWO*SVIS(I,2))
         E3  =  D3(I)*(SOLD3(I)+SIG(I,3) + P2+TWO*SVIS(I,3))
         E4  =  D4(I)*(SOLD4(I)+SIG(I,4) + TWO*SVIS(I,4))
         E5  =  D5(I)*(SOLD5(I)+SIG(I,5) + TWO*SVIS(I,5))
         E6  =  D6(I)*(SOLD6(I)+SIG(I,6) + TWO*SVIS(I,6))
         EINT(I)=EINT(I)-
     .      (Q(I)+QOLD(I)+POLD)*DVOL(I)*HALF + 
     .      (E1+E2+E3+E4+E5+E6+E7(I))*VOL_AVG(I)*DT1*HALF
       END DO
      ELSE 
        !other material laws without /eos
#include "vectorize.inc"
        DO I=1,NEL                                                           
          P2 = -(SOLD1(I)+SIG(I,1)+SOLD2(I)+SIG(I,2)+SOLD3(I)+SIG(I,3))* THIRD 
          E1 = D1(I)*(SOLD1(I)+SIG(I,1) + P2+TWO*SVIS(I,1))                   
          E2 = D2(I)*(SOLD2(I)+SIG(I,2) + P2+TWO*SVIS(I,2))                   
          E3 = D3(I)*(SOLD3(I)+SIG(I,3) + P2+TWO*SVIS(I,3))                   
          E4 = D4(I)*(SOLD4(I)+SIG(I,4) + TWO*SVIS(I,4))                      
          E5 = D5(I)*(SOLD5(I)+SIG(I,5) + TWO*SVIS(I,5))                      
          E6 = D6(I)*(SOLD6(I)+SIG(I,6) + TWO*SVIS(I,6))         
          EINC(I) = OFF(I) * (VOL_AVG(I)*DT1 * (E1+E2+E3+E4+E5+E6+E7(I))
     .         - DVOL(I)*(Q(I) + QOLD(I) + P2)) * HALF
        END DO                                                                 
        DO I = 1, NEL
         EINT(I) = EINT(I) + EINC(I)
        END DO                                                                 

      ENDIF
      QOLD(1:NEL) = Q(1:NEL)
C------------------------------------------
      RETURN
      END
