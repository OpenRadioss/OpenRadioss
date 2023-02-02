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
Chd|  USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ENG_USERLIB_FLAW              source/user_interface/dyn_userlib.c
Chd|        ENG_USERLIB_GET_LAW_VAR       source/user_interface/dyn_userlib.c
Chd|        ENG_USERLIB_GET_LAW_VAR_2     source/user_interface/dyn_userlib.c
Chd|        ENG_USERLIB_SET_LAW_VAR       source/user_interface/dyn_userlib.c
Chd|        ENG_USERLIB_SET_LAW_VAR_2     source/user_interface/dyn_userlib.c
Chd|        ENG_USERLIB_SIGEPS            source/user_interface/dyn_userlib.c
Chd|        ENG_USERLIB_SIGEPS99          source/user_interface/dyn_userlib.c
Chd|        EPSF2U                        source/materials/mat/mat033/sigeps33.F
Chd|        FAIL_BIQUAD_S                 source/materials/fail/biquad/fail_biquad_s.F
Chd|        FAIL_COCKROFT_S               source/materials/fail/cockroft_latham/fail_cockroft_s.F
Chd|        FAIL_EMC                      source/materials/fail/emc/fail_emc.F
Chd|        FAIL_ENERGY_S                 source/materials/fail/energy/fail_energy_s.F
Chd|        FAIL_FLD_TSH                  source/materials/fail/fld/fail_fld_tsh.F
Chd|        FAIL_GENE1_S                  source/materials/fail/gene1/fail_gene1_s.F
Chd|        FAIL_INIEVO_S                 source/materials/fail/inievo/fail_inievo_s.F
Chd|        FAIL_JOHNSON                  source/materials/fail/johnson_cook/fail_johnson.F
Chd|        FAIL_MULLINS_OR_S             source/materials/fail/mullins_or/fail_mullins_OR_s.F
Chd|        FAIL_ORTHBIQUAD_S             source/materials/fail/orthbiquad/fail_orthbiquad_s.F
Chd|        FAIL_ORTHSTRAIN               source/materials/fail/orthstrain/fail_orthstrain_s.F
Chd|        FAIL_RTCL_S                   source/materials/fail/rtcl/fail_rtcl_s.F
Chd|        FAIL_SAHRAEI_S                source/materials/fail/sahraei/fail_sahraei_s.F
Chd|        FAIL_SPALLING_S               source/materials/fail/spalling/fail_spalling_s.F
Chd|        FAIL_TAB2_S                   source/materials/fail/tabulated/fail_tab2_s.F
Chd|        FAIL_TAB_S                    source/materials/fail/tabulated/fail_tab_s.F
Chd|        FAIL_TBUTCHER_S               source/materials/fail/tuler_butcher/fail_tbutcher_s.F
Chd|        FAIL_TENSSTRAIN_S             source/materials/fail/tensstrain/fail_tensstrain_s.F
Chd|        FAIL_TSAIWU_S                 source/materials/fail/tsaiwu/fail_tsaiwu_s.F
Chd|        FAIL_VISUAL_S                 source/materials/fail/visual/fail_visual_s.F
Chd|        FAIL_WIERZBICKI_S             source/materials/fail/wierzbicki/fail_wierzbicki_s.F
Chd|        FAIL_WILKINS_S                source/materials/fail/wilkins/fail_wilkins_s.F
Chd|        MDTSPH                        source/materials/mat_share/mdtsph.F
Chd|        MQVISCB                       source/materials/mat_share/mqviscb.F
Chd|        MREPLOC                       source/materials/mat_share/mreploc.F
Chd|        MROTENS                       source/materials/mat_share/mrotens.F
Chd|        MROTENSNS                     source/materials/mat_share/mrotens.F
Chd|        NOLIB_USERMAT99               source/user_interface/nolib_usermat99.F
Chd|        NSVISUL                       source/materials/mat_share/mnsvis.F
Chd|        SREPLOC3                      source/elements/solid/solide/sreploc3.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        VISCMAIN                      source/materials/visc/viscmain.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        LAW_USERSO                    source/user_interface/law_userso.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE USERMAT_SOLID (
     1                  LFT     ,LLT     ,NFT      ,MTN     ,JCVT     ,
     2                  PM      ,OFF     ,SIG      ,EINT    ,RHO      ,
     3                  QOLD    ,VOL     ,STRAIN   ,SIGL    ,GAMA     ,
     4                  UVAR    ,BUFMAT  ,TF       ,NPF     ,STIFN    ,
     5                  MAT     ,NGL     ,NUVAR    ,DT2T    ,NELTST   ,
     6                  ITYPTST ,OFFG    ,GEO      ,PID     ,EPSD     ,
     7                  LTEMP   ,WXX     ,
     9                  WYY     ,WZZ     ,JSPH     ,MUMAX   ,SSP      ,
     A                  AIRE    ,VOLN    ,VD2      ,DELTAX  ,VIS      ,
     B                  D1      ,D2      ,D3       ,D4      ,D5       ,
     C                  D6      ,PNEW    ,PSH      ,Q       ,SSP_EQ   ,
     D                  DVOL    ,SOLD1   ,SOLD2    ,SOLD3   ,SOLD4    ,
     E                  SOLD5   ,SOLD6   ,RX       ,RY      ,RZ       ,
     F                  SX      ,SY      ,SZ       ,TX      ,TY       ,
     G                  TZ      ,IPLA    ,SIGY     ,DEFP    ,ISMSTR   ,
     H                  MFXX    ,MFXY    ,MFXZ     ,MFYX    ,MFYY     ,
     I                  MFYZ    ,MFZX    ,MFZY     ,MFZZ    ,IPM      ,
     J                  ISORTH  ,FBUF    ,NFAIL    ,NPG     ,SIGDD    ,
     K                  DXY     ,DYX     ,DYZ      ,DZY     ,DZX      ,
     L                  DXZ     ,FR_WAV  ,ISROT    ,V       ,VARNL    ,
     M                  W       ,IX      ,X        ,JTHE    ,ET       ,
     N                  MSSA    ,DMELS   ,IPTR     ,IPTS    ,IPTT     ,
     O                  TABLE   ,FVD2    ,FDELTAX  ,FSSP    ,FQVIS    ,
     P                  TEMPEL  ,IPARG1  ,IGEO     ,SIGV    ,
     Q                  AL_IMP  ,SIGNOR  ,ISTRAIN  ,NG      ,ELBUF_TAB,
     R                  VBUF    ,ILAY    ,VK       ,IPARG    ,
     S                  BUFVOIS ,VDX     ,VDY      ,VDZ     ,IHET     ,
     T                  CONDE   ,ITASK   ,IEXPAN   ,VOL_AVG ,AMU      ,
     U                  EPSTH3  ,EPSTH   ,SVISC    ,NEL     ,ETOTSH   ,
     V                  ISELECT ,TSTAR   ,MUOLD    ,AMU2    ,DPDM     ,
     W                  RHOREF  ,RHOSP   ,NLOC_DMG )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE LAW_USERSO
      USE TABLE_MOD
      USE ELBUFDEF_MOD            
      USE MESSAGE_MOD
      USE NLOCAL_REG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "usrplas_c.inc"
#include      "scr06_c.inc"
#include      "units_c.inc"
#include      "nsvis_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr17_c.inc"
#include      "scr19_c.inc"
#include      "com01_c.inc"
#include      "inter22.inc"
#include      "userlib.inc"
#include      "impl1_c.inc"
#include      "timeri_c.inc"
#include      "scr18_c.inc"
#include      "nchara_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT ,LLT ,NFT ,MTN, NUVAR,JCVT,ISORTH,NFAIL,NPG,
     .        J, JSPH, IPLA,ISMSTR,NELTST, ITYPTST, NUVARR,NG,ILAY,
     .        IX, JTHE, ISROT,ISTRAIN,IPTR, IPTS, IPTT,IHET,IEXPAN,NEL
      INTEGER NPF(*),MAT(MVSIZ),NGL(MVSIZ),PID(MVSIZ),IPM(NPROPMI,*),
     .        IPARG1(*),IPARG(NPARG,*),IGEO(NPROPGI,*),ITASK,ISELECT
      TARGET :: IPM
C 
      my_real 
     .   DT2T, BUFVOIS(*)
      my_real 
     .   PM(NPROPM,*), GAMA(*),PNEW(NEL),VARNL(NEL),
     .   OFF(*), SIG(NEL,6), SIGL(NEL,6), STRAIN(NEL,6), EINT(*), 
     .   RHO(*),QOLD(*), VOL(*), BUFMAT(*), TF(*),VK(*),
     .   OFFG(*),STIFN(*),GEO(NPROPG,*),EPSD(*),
     .   WXX(*), WYY(*), WZZ(*), MUMAX(*), SSP(*),
     .   AIRE(*) ,VOLN(*) ,VD2(*) ,DELTAX(*),VIS(*) ,  
     .   SOLD1(MVSIZ) ,SOLD2(MVSIZ) ,SOLD3(MVSIZ) ,
     .   SOLD4(MVSIZ) ,SOLD5(MVSIZ) ,SOLD6(MVSIZ),
     .   D1(*) ,D2(*),D3(*),D4(*),D5(*),D6(*) ,PSH(NEL),
     .   Q(*),SSP_EQ(*),DVOL(*),
     .   RX(MVSIZ)    ,RY(MVSIZ)    ,RZ(MVSIZ),
     .   SX(MVSIZ)    ,SY(MVSIZ)    ,SZ(MVSIZ)    ,
     .   TX(MVSIZ)    ,TY(MVSIZ)    ,TZ(MVSIZ)   , V(*), W(*), 
     .   MFXX(*)  ,MFXY(*)  ,MFXZ(*)  ,MFYX(*) ,MFYY(*)   ,MFYZ(*)   ,
     .   MFZX(*)  ,MFZY(*)  ,MFZZ(*), FQVIS(*),FSSP(*),
     .   SIGV(NEL,6), SIGY(*),DEFP(NEL), 
     .   DXY(*) ,DYX(*) ,DYZ(*) ,DZY(*) ,DZX(*) ,DXZ(*), SIGDD(NEL,6),
     .   FR_WAV(*),X(*),ET(*), MSSA(*), DMELS(*),FVD2(*),FDELTAX(*),
     .   AL_IMP(MVSIZ),SIGNOR(MVSIZ,6),VDX(*),VDY(*),VDZ(*),
     .   SVISC(NEL,6),AMU(MVSIZ),AMU2(MVSIZ),DPDM(MVSIZ),VOL_AVG(MVSIZ),EPSTH(NEL),EPSTH3(MVSIZ),
     .   ETOTSH(NEL,6),TSTAR(MVSIZ),MUOLD(NEL),CONDE(*), RHOREF(*), RHOSP(*)
      TARGET :: BUFMAT,DELTAX,VARNL,DEFP
      my_real, TARGET, DIMENSION(NUVAR*LLT):: UVAR
      my_real, TARGET, DIMENSION(LLT):: UVARF1,LTEMP,TEMPEL,SCALE1,INITVARF
      TYPE (BUF_FAIL_), TARGET  :: FBUF
      TYPE (BUF_VISC_)  :: VBUF
      TYPE (TTABLE), DIMENSION(NTABLE) ::  TABLE
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (NLOCAL_STR_) :: NLOC_DMG 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NV46, NUMEL, INLOC
      INTEGER I,NPAR,IADBUF,NFUNC,NUMTABL,ISRATE,SIZNUL,MX,IPG,NPTR,NPTS,
     .  IBID,IBIDON1,IBIDON2,IBIDON3,IBIDON4 ,N48,NIX,ILAW_USER,IGTYP,
     .  NVARF,IR,IP,IRUPT,IMAT,ISVIS,IVISC,NUVARV,ISEQ,IADRUPBUF,IDEV
      my_real 
     .E1,E2,E3,E4,E5,E6,BID1,BID2,BID3,Q1,Q2,Q3,SS1,SS2,SS3,SS4,SS5,
     .SS6,WXXF,WYYF,WZZF,P2,EPSP,DAV,ASRATE,C1(MVSIZ),
     . EP1(MVSIZ),EP2(MVSIZ),EP3(MVSIZ),E7(MVSIZ),
     . EP4(2*MVSIZ),EP5(2*MVSIZ),EP6(2*MVSIZ),AAA,EINC,
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
     .PAIR(MVSIZ),ES7(MVSIZ),ES8(MVSIZ),ES9(MVSIZ),DEFP0(MVSIZ)
      my_real SS1D,SS2D,SS3D,SS4D,SS5D,SS6D,Q1D,Q2D,Q3D,
     .   WXXFD,WYYFD,WZZFD,FACQ0,ETH
     
      my_real FPSXX(MVSIZ),FPSYY(MVSIZ),FPSZZ(MVSIZ),FPSXY(MVSIZ),
     *        FPSYZ(MVSIZ),FPSZX(MVSIZ),FPSYX(MVSIZ),FPSZY(MVSIZ),
     *        FPSXZ(MVSIZ),BID_ARR(MVSIZ),
     *        UPSXX(MVSIZ),UPSYY(MVSIZ),UPSZZ(MVSIZ),UPSXY(MVSIZ),
     *        UPSYZ(MVSIZ),UPSXZ(MVSIZ)
      my_real 
     .   RHO0(MVSIZ),BIDON1,BIDON2,BIDON3,BIDON4,BIDON5,POLD
      my_real TT_LOCAL
      my_real, DIMENSION(NEL), TARGET  :: LE_MAX
C----
      my_real, DIMENSION(:), POINTER  :: UPARAM,UPARAMF,UVARF,DFMAX,TDEL,EL_TEMP, 
     .                                   YLDFAC,DAM,EL_LEN,DMG_SCALE,EL_PLA,DAMINI
      TYPE(ULAWINTBUF)                :: USERBUF
      TYPE(L_BUFEL_)  ,POINTER        :: LBUF, LBUF1, LBUF2    
      TYPE(G_BUFEL_)  ,POINTER        :: GBUF      
      LOGICAL :: LOGICAL_USERL_AVAIL
      my_real  USER_UELR(MVSIZ)
      INTEGER, DIMENSION(:) ,POINTER  :: FLD_IDX,FOFF,IFUNC,ITABLE
      INTEGER                         :: DMG_FLAG
      my_real, DIMENSION(:,:) , ALLOCATABLE :: ISMSTR10_FP
      my_real  ADDITIONAL_FLT_PARAMETERS(MVSIZ)
      INTEGER  ADDITIONAL_INT_PARAMETERS(MVSIZ),LENGTH
      CHARACTER*256 MDS_LIBNAME
!
      CHARACTER OPTION*256
      INTEGER SIZE
      CHARACTER IUSER_KEY*ncharline
C=======================================================================
      GBUF   => ELBUF_TAB(NG)%GBUF           
      LBUF   => ELBUF_TAB(NG)%BUFLY(ILAY)%LBUF(IPTR,IPTS,IPTT)
      LOGICAL_USERL_AVAIL=.FALSE.
      IF(USERL_AVAIL>0) LOGICAL_USERL_AVAIL=.TRUE.
      NPTR   = ELBUF_TAB(NG)%NPTR
      NPTS   = ELBUF_TAB(NG)%NPTS
      IPG    = IPTR + ((IPTS-1) + (IPTT-1)*NPTS)*NPTR
      IGTYP  = IPARG(38,NG)
      IF ((IGTYP == 20).OR.(IGTYP == 21).OR.(IGTYP == 22)) THEN
        IPG  = IPTR + ((IPTS-1) + (ILAY-1)*NPTS)*NPTR
      ENDIF
      ISEQ   = ELBUF_TAB(NG)%BUFLY(ILAY)%L_SEQ
      INLOC  = IPARG(78,NG)
      DMG_FLAG = ELBUF_TAB(NG)%BUFLY(ILAY)%L_DMGSCL
      ! Make sure that the non-local variable increment is positive
      IF (INLOC > 0) THEN 
        DO I = LFT,LLT
          IF (OFF(I) == ONE) THEN
            VARNL(I) = MAX(VARNL(I),ZERO)
          ELSE
            VARNL(I) = ZERO
          ENDIF
          LBUF%PLANL(I)  = LBUF%PLANL(I) + VARNL(I)
          LBUF%EPSDNL(I) = VARNL(I)/MAX(DT1,EM20)
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
c
      !C
      
      !c               (utilisateurs pour la rupture
       BIDON1 = ZERO
       BIDON2 = ZERO
       BIDON3 = ZERO
       BIDON4 = ZERO
       BIDON5 = ZERO
       BID1 = ZERO
       BID3 = ZERO
      
      !               (utilisateurs pour la rupture
      IBID    = 0
      IBIDON1 = 0
      IBIDON2 = 0
      IBIDON3 = 0
      IBIDON4 = 0 
      ILAW_USER = IPM(217, MX)
      IMAT = IPM(1, MX)
      ISVIS = IGEO(35,PID(1))
      IF(IMPL_S >0.AND.IDYNA == 0) ISVIS = 0
c
      DPLA(LFT:LLT)  = ZERO

      IF (ELBUF_TAB(NG)%BUFLY(ILAY)%L_PLA > 0) THEN
          DEFP0(LFT:LLT) = LBUF%PLA(LFT:LLT) 
      ELSE
          DEFP0(LFT:LLT) = ZERO
      ENDIF


      IF (JTHE == 0 .and. ELBUF_TAB(NG)%BUFLY(ILAY)%L_TEMP > 0) THEN            
        EL_TEMP => LTEMP     ! LBUF%TEMP
      ELSE                                                  
        EL_TEMP => TEMPEL(LFT:LLT)                           
      ENDIF 

      !initial scale factor for yield stress defined per IPG,NPI  
      SCALE1(LFT:LLT) = ONE                                        
      YLDFAC => SCALE1(LFT:LLT)                                   
      IF ( ELBUF_TAB(NG)%BUFLY(ILAY)%L_FAC_YLD > 0) THEN          
         YLDFAC => LBUF%FAC_YLD(1:NEL)                            
      ENDIF                                                       
c
      C1(1:MVSIZ)  = PM(32,MX)
      RHO0(1:MVSIZ)= PM( 1,MX)
      VIS(1:MVSIZ) = ZERO
      EP1(1:MVSIZ) = D1(1:MVSIZ)*OFF(1:MVSIZ)
      EP2(1:MVSIZ) = D2(1:MVSIZ)*OFF(1:MVSIZ)
      EP3(1:MVSIZ) = D3(1:MVSIZ)*OFF(1:MVSIZ)
      EP4(1:MVSIZ) = D4(1:MVSIZ)*OFF(1:MVSIZ)
      EP5(1:MVSIZ) = D5(1:MVSIZ)*OFF(1:MVSIZ)
      EP6(1:MVSIZ) = D6(1:MVSIZ)*OFF(1:MVSIZ)
      SV1(1:MVSIZ) = ZERO
      SV2(1:MVSIZ) = ZERO
      SV3(1:MVSIZ) = ZERO
      SV4(1:MVSIZ) = ZERO
      SV5(1:MVSIZ) = ZERO
      SV6(1:MVSIZ) = ZERO
      E7(1:MVSIZ)  = ZERO
      S1(1:MVSIZ)  = ZERO
      S2(1:MVSIZ)  = ZERO
      S3(1:MVSIZ)  = ZERO
      S4(1:MVSIZ)  = ZERO
      S5(1:MVSIZ)  = ZERO
      S6(1:MVSIZ)  = ZERO

      SSP(1:MVSIZ)=ZERO

      IF (JCVT > 0) THEN
C---------------------------
C       ISOTROPE ET ORTHOTROPE CONVECTE
C---------------------------
        DO I=LFT,LLT
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
      ELSEIF (ISORTH /= 0) THEN
C---------------------------
C       ORTHOTROPE GLOBAL
C---------------------------
        CALL MREPLOC(GAMA,R11,R12,R13,R21,R22    ,
     2               R23 ,R31,R32,R33,
     3               RX  ,RY ,RZ ,SX ,SY ,SZ     ,
     4               TX  ,TY ,TZ )
        DO I=LFT,LLT
         EP4(I) = HALF*EP4(I)
         EP5(I) = HALF*EP5(I)
         EP6(I) = HALF*EP6(I)
        ENDDO
        CALL MROTENS(LFT,LLT,EP1,EP2,EP3,EP4,EP5,EP6,
     .               R11,R12,R13,
     .               R21,R22,R23,
     .               R31,R32,R33)
#include "vectorize.inc"
        DO I=LFT,LLT
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
C       ISOTROPE GLOBAL
C---------------------------
#include "vectorize.inc"
        DO I=LFT,LLT
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
        DO I=LFT,LLT
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
        DO I=LFT,LLT
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
        DO I=LFT,LLT
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
          DO I=LFT,LLT
           ES1(I)=ETOTSH(I,1)*OFF(I)
           ES2(I)=ETOTSH(I,2)*OFF(I)
           ES3(I)=ETOTSH(I,3)*OFF(I)
           ES4(I)=ETOTSH(I,4)*OFF(I)
           ES6(I)=ETOTSH(I,6)*OFF(I)
           ES5(I)=ETOTSH(I,5)*OFF(I)
          END DO
        ELSE
          DO I=LFT,LLT
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
c           Is like 42 ...etc we compute [B]=[F][F]^t - I strain----- e.g. 62,92,88,90 
          DO I=LFT,LLT
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
        IF(IDTMIN(1)==3.AND.ISMSTR == 12)THEN
          DO I=LFT,LLT
            IF (OFFG(I) <=ONE) CYCLE
            ES1(I)=MFXX(I)
            ES2(I)=MFYY(I)
            ES3(I)=MFZZ(I)
            ES4(I)=HALF*(MFXY(I)+MFYX(I))
            ES6(I)=HALF*(MFXZ(I)+MFZX(I))
            ES5(I)=HALF*(MFZY(I)+MFYZ(I))
          ENDDO
        END IF
C---------------------------
C       ORTHOTROPE GLOBAL
C---------------------------
        IF (JCVT <= 0 .AND. ISORTH /= 0) THEN
c
          CALL MREPLOC(GAMA,R11,R12,R13,R21,R22    ,
     2                 R23 ,R31,R32,R33,
     3                 RX  ,RY ,RZ ,SX ,SY ,SZ     ,
     4                 TX  ,TY ,TZ )
          CALL MROTENS(LFT,LLT,ES1,ES2,ES3,ES4,ES5,ES6,
     .                 R11,R12,R13,
     .                 R21,R22,R23,
     .                 R31,R32,R33)
        ENDIF
        DO I=LFT,LLT
          ES4(I) = TWO*ES4(I)
          ES5(I) = TWO*ES5(I)
          ES6(I) = TWO*ES6(I)
        ENDDO
c-------------------------------       
      ELSEIF (ISMSTR==11) THEN
c-------------------------------       
        DO I=LFT,LLT
          ES1(I)=MFXX(I)
          ES2(I)=MFYY(I)
          ES3(I)=MFZZ(I)
          ES4(I)=MFXY(I)+MFYX(I)
          ES6(I)=MFXZ(I)+MFZX(I)
          ES5(I)=MFZY(I)+MFYZ(I)
        ENDDO
        IF (JCVT == 0 .AND. ISORTH /= 0) THEN
C---------------------------
C         ORTHOTROPE GLOBAL
C---------------------------
           DO I=LFT,LLT
            ES4(I) = HALF*ES4(I)
            ES5(I) = HALF*ES5(I)
            ES6(I) = HALF*ES6(I)
           ENDDO
            CALL MREPLOC(GAMA,R11,R12,R13,R21,R22    ,
     2                   R23 ,R31,R32,R33,
     3                   RX  ,RY ,RZ ,SX ,SY ,SZ     ,
     4                   TX  ,TY ,TZ )
            CALL MROTENS(LFT,LLT,ES1,ES2,ES3,ES4,ES5,ES6,
     .                   R11,R12,R13,
     .                   R21,R22,R23,
     .                   R31,R32,R33)
           DO I=LFT,LLT
            ES4(I) = TWO*ES4(I)
            ES5(I) = TWO*ES5(I)
            ES6(I) = TWO*ES6(I)
           ENDDO
        ENDIF
C---------------------------
        DO I=LFT,LLT
         STRAIN(I,1)= ES1(I)
         STRAIN(I,2)= ES2(I)
         STRAIN(I,3)= ES3(I)
         STRAIN(I,4)= ES4(I)
         STRAIN(I,5)= ES5(I)
         STRAIN(I,6)= ES6(I)
        ENDDO
c       THERM STRESS COMPUTATION-----
        IF (IEXPAN > 0) THEN
          DO I=LFT,LLT
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
c
c--------------------------------------------------------
c     COMPUTE UNDAMAGED EFFECTIVE STRESSES
c---------------------------------------------------------
      DMG_SCALE => LBUF%DMGSCL
      IF (DMG_FLAG > 0) THEN 
        DO I = 1,NEL
          SO1(I) = SO1(I)/MAX(DMG_SCALE(I),EM20)
          SO2(I) = SO2(I)/MAX(DMG_SCALE(I),EM20)
          SO3(I) = SO3(I)/MAX(DMG_SCALE(I),EM20)
          SO4(I) = SO4(I)/MAX(DMG_SCALE(I),EM20)
          SO5(I) = SO5(I)/MAX(DMG_SCALE(I),EM20)
          SO6(I) = SO6(I)/MAX(DMG_SCALE(I),EM20)
        ENDDO
      ENDIF
c
c---------------------------------------------------------
c     SELECT MATERIAL LAW
c---------------------------------------------------------
      IF (MTN == 29) THEN
          DO I=LFT,LLT
            U_TAGPLAS(I)=0
          END DO
          IF (LOGICAL_USERL_AVAIL)THEN
            TT_LOCAL = TT
            CALL ENG_USERLIB_SIGEPS( MTN,
     .                  LLT ,NPAR,NUVAR,NFUNC,IFUNC,
     .                  NPF ,TF  ,TT_LOCAL,DT1,UPARAM,
     .                  RHO0,RHO ,VOLN,EINT,
     .                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     .                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     .                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     .                  SSP ,VIS ,UVAR,OFF  )
            
          ELSE
!            CALL SIGEPS29(LLT ,NPAR,NUVAR,NFUNC,IFUNC,
!     .                  NPF ,TF  ,TT,DT1,UPARAM,
!     .                  RHO0,RHO ,VOLN,EINT,
!     .                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
!     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
!     .                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
!     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
!     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
!     .                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
!     .                  SSP ,VIS ,UVAR,OFF  )
!!!
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/MAT/LAW29 - SOLID '
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
!!!
          ENDIF
          DO I=LFT,LLT
            IF(U_TAGPLAS(I)/=0)THEN
              SIGY(I)=U_YELD(I)
              DEFP(I)=U_DEFP(I)
cc              DPLA(I) = DEFP(I) - DEFP0(I)              
            ELSE
              SIGY(I)=EP30
              DEFP(I)=ZERO
            END IF
          END DO
      ELSEIF(MTN == 30)THEN
          DO I=LFT,LLT
            U_TAGPLAS(I)=0
          END DO
          IF (LOGICAL_USERL_AVAIL)THEN 
            TT_LOCAL = TT
            CALL ENG_USERLIB_SIGEPS( MTN,
     .                  LLT ,NPAR,NUVAR,NFUNC,IFUNC,
     .                  NPF ,TF  ,TT_LOCAL,DT1,UPARAM,
     .                  RHO0,RHO ,VOLN,EINT,
     .                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     .                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     .                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     .                  SSP ,VIS ,UVAR,OFF  )
            
          ELSE
!            CALL SIGEPS30(LLT ,NPAR,NUVAR,NFUNC,IFUNC,
!     .                  NPF ,TF  ,TT,DT1,UPARAM,
!     .                  RHO0,RHO ,VOLN,EINT,
!     .                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
!     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
!     .                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
!     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
!     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
!     .                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
!     .                  SSP ,VIS ,UVAR,OFF  )
!!!
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/MAT/LAW30 - SOLID '
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
!!!
          ENDIF
          DO I=LFT,LLT
            IF(U_TAGPLAS(I)/=0)THEN
              SIGY(I)=U_YELD(I)
              DEFP(I)=U_DEFP(I)
cc              DPLA(I) = DEFP(I) - DEFP0(I)              
            ELSE
              SIGY(I)=EP30
              DEFP(I)=ZERO
            END IF
          END DO
      ELSEIF(MTN == 31)THEN
          DO I=LFT,LLT
            U_TAGPLAS(I)=0
          END DO
          IF (LOGICAL_USERL_AVAIL)THEN
            TT_LOCAL = TT
            CALL ENG_USERLIB_SIGEPS( MTN,
     .                  LLT ,NPAR,NUVAR,NFUNC,IFUNC,
     .                  NPF ,TF  ,TT_LOCAL,DT1,UPARAM,
     .                  RHO0,RHO ,VOLN,EINT,
     .                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     .                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     .                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     .                  SSP ,VIS ,UVAR,OFF  )
            
          ELSE
!            CALL SIGEPS31(LLT ,NPAR,NUVAR,NFUNC,IFUNC,
!     .                  NPF ,TF  ,TT,DT1,UPARAM,
!     .                  RHO0,RHO ,VOLN,EINT,
!     .                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
!     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
!     .                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
!     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
!     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
!     .                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
!     .                  SSP ,VIS ,UVAR,OFF  )
!!!
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/MAT/LAW31 - SOLID '
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
!!!
          ENDIF
          DO I=LFT,LLT
            IF(U_TAGPLAS(I)/=0)THEN
              SIGY(I)=U_YELD(I)
              DEFP(I)=U_DEFP(I)
cc              DPLA(I) = DEFP(I) - DEFP0(I)              
            ELSE
              SIGY(I)=EP30
              DEFP(I)=ZERO
            END IF
          END DO
c----------------------------------------
      ELSEIF(MTN  ==  99) THEN
C chgrt de la structure 
C
           CALL SREPLOC3(
     .       RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .       R11  ,R21  ,R31  ,R12  ,R22  ,R32  ,R13  ,R23  ,R33  )

         IF(LOGICAL_USERL_AVAIL)THEN

          IF (ISMSTR == 10) THEN
            DO I=LFT,LLT
             FPSXX(I) = ONE+MFXX(I)
             FPSYY(I) = ONE+MFYY(I)
             FPSZZ(I) = ONE+MFZZ(I)
            END DO
            FPSXY = MFXY(1:MVSIZ)
            FPSYZ = MFYZ(1:MVSIZ)
            FPSXZ = MFXZ(1:MVSIZ)          
            FPSYX = MFYX(1:MVSIZ)
            FPSZY = MFZY(1:MVSIZ)
            FPSZX = MFZX(1:MVSIZ)    
C---------------------------
C       ORTHOTROPE 
C---------------------------
            IF (ISORTH /= 0) THEN
             CALL MREPLOC(GAMA,R11,R12,R13,R21,R22    ,
     2                    R23 ,R31,R32,R33,
     3                    RX  ,RY ,RZ ,SX ,SY ,SZ     ,
     4                    TX  ,TY ,TZ )
             CALL MROTENSNS(LFT,LLT,
     .               FPSXX,FPSXY,FPSXZ,
     .               FPSYX,FPSYY,FPSYZ,
     .               FPSZX,FPSZY,FPSZZ,
     .               R11,R12,R13,
     .               R21,R22,R23,
     .               R31,R32,R33)
            END IF !(ISORTH /= 0) THEN
	    CALL EPSF2U(
     1      LLT    ,FPSXX    , FPSXY  , FPSXZ  , FPSYX  ,    
     2      FPSYY  ,FPSYZ    , FPSZX  , FPSZY  , FPSZZ  , 
     3      UPSXX  ,UPSYY    , UPSZZ  , UPSXY  , UPSYZ  ,
     4      UPSXZ  )

          ELSE  ! (ISMSTR == 10)          
            FPSXX(1:MVSIZ) = ZERO
            FPSYY(1:MVSIZ) = ZERO
            FPSZZ(1:MVSIZ) = ZERO
            FPSXY(1:MVSIZ) = ZERO
            FPSYZ(1:MVSIZ) = ZERO
            FPSXZ(1:MVSIZ) = ZERO          
            FPSYX(1:MVSIZ) = ZERO
            FPSZY(1:MVSIZ) = ZERO
            FPSZX(1:MVSIZ) = ZERO    
            UPSXX(1:MVSIZ) = ZERO
            UPSYY(1:MVSIZ) = ZERO
            UPSZZ(1:MVSIZ) = ZERO
            UPSXY(1:MVSIZ) = ZERO
            UPSYZ(1:MVSIZ) = ZERO
            UPSXZ(1:MVSIZ) = ZERO          
          END IF! (ISMSTR == 10) 

         
C Fill Structure in dynamical library
           CALL ENG_USERLIB_GET_LAW_VAR(
     *             NCYCLE, IMAT,IPTR, IPTS,IPTT,
     *             R11,    R12, R13, R21, R22, R23,  R31,
     *             R32,    R33, SO1, SO2, SO3, SO4,  SO5,
     *             SO6,    EP1, EP2, EP3, EP4, EP5,  EP6,
     *             ES1,    ES2, ES3, ES4, ES5, ES6,  DE1,
     *             DE2,    DE3, DE4, DE5, DE6, RHO0, S1,
     *             S2,     S3,  S4,  S5,  S6,  SV1,  SV2,
     *             SV3,    SV4, SV5, SV6 )
     
           IF(DLIBTKVERS >= 1301501260)THEN
             SIZNUL=0           
             CALL ENG_USERLIB_GET_LAW_VAR_2(
     *         FPSXX  ,MVSIZ ,FPSYY  ,MVSIZ ,
     *         FPSZZ  ,MVSIZ ,FPSXY  ,MVSIZ ,FPSYZ  ,MVSIZ ,FPSZX  ,MVSIZ,
     *         FPSYX  ,MVSIZ ,FPSZY  ,MVSIZ ,FPSXZ  ,MVSIZ ,UPSXX  ,MVSIZ, 
     *         UPSYY  ,MVSIZ ,UPSZZ  ,MVSIZ ,UPSXY  ,MVSIZ ,UPSYZ  ,MVSIZ,
     *         UPSXZ  ,MVSIZ ,EL_TEMP,MVSIZ ,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *         BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *         BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *         BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *         BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *         BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *         BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *         BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *         BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *         BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *         BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *         BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *         BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *         BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *         BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *         BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *         BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *         BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *         BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *         BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *         BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL)
           ENDIF

C Call SIGEPS99 in dynamical user library
           TT_LOCAL = TT
           CALL  ENG_USERLIB_SIGEPS99(
     *                LLT     ,NPAR  ,NUVAR  ,ILAW_USER  ,NFUNC,
     *                IFUNC   ,NPF   ,TF     ,TT_LOCAL   ,DT1,
     *                UPARAM ,RHO    ,VOLN ,EINT ,NGL  ,
     *                SSP            ,VIS    ,UVAR ,OFF  ,SIGY ,
     *                DEFP  )

C Get back results from structure in user library
            CALL ENG_USERLIB_SET_LAW_VAR(
     *                 S1,  S2,   S3, S4,  S5,  S6,
     *                 SV1, SV2,  SV3,SV4, SV5, SV6,
     *                 DPLA )
     
           IF(DLIBTKVERS >= 1301501260)THEN
             SIZNUL=0           
             CALL ENG_USERLIB_SET_LAW_VAR_2(BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,    
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,
     *                                      BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL,BID_ARR,SIZNUL )
           ENDIF
         ELSE !(USERL_AVAIL)
           USERBUF%NCYCLE = NCYCLE
           USERBUF%ID     = IMAT
           USERBUF%IPTR   = IPTR
           USERBUF%IPTS   = IPTS
           USERBUF%IPTT   = IPTT
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
           USERBUF%SIGOXX = SO1
           USERBUF%SIGOYY = SO2
           USERBUF%SIGOZZ = SO3 
           USERBUF%SIGOXY = SO4(1:MVSIZ)
           USERBUF%SIGOYZ = SO5(1:MVSIZ)
           USERBUF%SIGOZX = SO6(1:MVSIZ)
C
           USERBUF%EPSPXX = EP1
           USERBUF%EPSPYY = EP2
           USERBUF%EPSPZZ = EP3
           USERBUF%EPSPXY = EP4(1:MVSIZ)
           USERBUF%EPSPYZ = EP5(1:MVSIZ)
           USERBUF%EPSPZX = EP6(1:MVSIZ)
C                
           USERBUF%EPSXX = ES1
           USERBUF%EPSYY = ES2
           USERBUF%EPSZZ = ES3
           USERBUF%EPSXY = ES4(1:MVSIZ)
           USERBUF%EPSYZ = ES5(1:MVSIZ)
           USERBUF%EPSZX = ES6(1:MVSIZ)          
C---------------------------
           IF (ISMSTR == 10) THEN
            IF (ISORTH /= 0) THEN
C            ORTHOTROPE 
             CALL MREPLOC(GAMA,R11,R12,R13,R21,R22    ,
     2                    R23 ,R31,R32,R33,
     3                    RX  ,RY ,RZ ,SX ,SY ,SZ     ,
     4                    TX  ,TY ,TZ )
             DO I=LFT,LLT
              MFXX(I) = ONE+MFXX(I)
              MFYY(I) = ONE+MFYY(I)
              MFZZ(I) = ONE+MFZZ(I)
             END DO
             CALL MROTENSNS(LFT,LLT,
     .               MFXX,MFXY,MFXZ,
     .               MFYX,MFYY,MFYZ,
     .               MFZX,MFZY,MFZZ,
     .               R11,R12,R13,
     .               R21,R22,R23,
     .               R31,R32,R33)
            END IF !(ISORTH /= 0) THEN
            USERBUF%FPSXX = MFXX(1:MVSIZ)
            USERBUF%FPSYY = MFYY(1:MVSIZ)
            USERBUF%FPSZZ = MFZZ(1:MVSIZ)
            USERBUF%FPSXY = MFXY(1:MVSIZ)
            USERBUF%FPSYZ = MFYZ(1:MVSIZ)
            USERBUF%FPSXZ = MFXZ(1:MVSIZ)          
            USERBUF%FPSYX = MFYX(1:MVSIZ)
            USERBUF%FPSZY = MFZY(1:MVSIZ)
            USERBUF%FPSZX = MFZX(1:MVSIZ)    
	           CALL EPSF2U(
     1           LLT    ,USERBUF%FPSXX,USERBUF%FPSXY,USERBUF%FPSXZ,USERBUF%FPSYX,    
     2           USERBUF%FPSYY,USERBUF%FPSYZ,USERBUF%FPSZX,USERBUF%FPSZY,USERBUF%FPSZZ, 
     3           USERBUF%UPSXX,USERBUF%UPSYY,USERBUF%UPSZZ,USERBUF%UPSXY,USERBUF%UPSYZ,
     4           USERBUF%UPSXZ)
           ELSE  ! ISMSTR /= 10          
            USERBUF%FPSXX = ZERO
            USERBUF%FPSYY = ZERO
            USERBUF%FPSZZ = ZERO
            USERBUF%FPSXY = ZERO
            USERBUF%FPSYZ = ZERO
            USERBUF%FPSXZ = ZERO          
            USERBUF%FPSYX = ZERO
            USERBUF%FPSZY = ZERO
            USERBUF%FPSZX = ZERO
C	    
            USERBUF%UPSXX = ZERO
            USERBUF%UPSYY = ZERO
            USERBUF%UPSZZ = ZERO
            USERBUF%UPSXY = ZERO
            USERBUF%UPSYZ = ZERO
            USERBUF%UPSXZ = ZERO          
           END IF  ! ISMSTR 
C                
           USERBUF%DEPSXX = DE1
           USERBUF%DEPSYY = DE2
           USERBUF%DEPSZZ = DE3
           USERBUF%DEPSXY = DE4(1:MVSIZ)
           USERBUF%DEPSYZ = DE5(1:MVSIZ)          
           USERBUF%DEPSZX = DE6(1:MVSIZ)
c
           USERBUF%RHO0   = RHO0
           USERBUF%TEMP   = EL_TEMP
c          
           USERBUF%SIGNXX = S1
           USERBUF%SIGNYY = S2
           USERBUF%SIGNZZ = S3
           USERBUF%SIGNXY = S4(1:MVSIZ)
           USERBUF%SIGNYZ = S5(1:MVSIZ)
           USERBUF%SIGNZX = S6(1:MVSIZ)
c
           USERBUF%SIGVXX = SV1
           USERBUF%SIGVYY = SV2
           USERBUF%SIGVYY = SV3
           USERBUF%SIGVXY = SV4(1:MVSIZ)
           USERBUF%SIGVYZ = SV5(1:MVSIZ)
           USERBUF%SIGVZX = SV6(1:MVSIZ)
C           
!          CALL SIGEPS99 (LLT          ,NPAR   ,NUVAR  ,ILAW_USER , NFUNC ,
!     .                  IFUNC         ,NPF    ,TF     ,TT        ,DT1    ,
!     .                  UPARAM,RHO    ,VOLN   ,EINT      ,NGL    ,
!     .                  SSP           ,VIS    ,UVAR   ,OFF       ,SIGY   ,
!     .                  DEFP          ,USERBUF)
!!!
              ! ----------------
              ! ERROR to be printed & exit
                CALL NOLIB_USERMAT99(ILAW_USER, IUSER_KEY)
                OPTION='/MAT/'//IUSER_KEY//' - SOLID'
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
!!!     
C
          S1 = USERBUF%SIGNXX
          S2 = USERBUF%SIGNYY
          S3 = USERBUF%SIGNZZ
          S4(1:MVSIZ) = USERBUF%SIGNXY
          S5(1:MVSIZ) = USERBUF%SIGNYZ 
          S6(1:MVSIZ) = USERBUF%SIGNZX
C           
          SV1 = USERBUF%SIGVXX
          SV2 = USERBUF%SIGVYY
          SV3 = USERBUF%SIGVZZ
          SV4(1:MVSIZ) = USERBUF%SIGVXY
          SV5(1:MVSIZ) = USERBUF%SIGVYZ 
          SV6(1:MVSIZ) = USERBUF%SIGVZX 
          DPLA   = USERBUF%DPLA  
          IF (JTHE == 0) EL_TEMP = USERBUF%TEMP ! otherwise it's read only in user law
         ENDIF
c
#ifdef DNC
      ELSE IF (MTN == 200) THEN  ! MDS MATERIAL LAW INTERFACE (in construction)

        IF(MDS_AVAIL == 1 )THEN
          ALLOCATE(ISMSTR10_FP(15,NEL))

          CALL SREPLOC3(
     .       RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .       R11  ,R21  ,R31  ,R12  ,R22  ,R32  ,R13  ,R23  ,R33  )


          IF (ISMSTR == 10) THEN
             DO I=LFT,LLT
               FPSXX(I) = ONE+MFXX(I)
               FPSYY(I) = ONE+MFYY(I)
               FPSZZ(I) = ONE+MFZZ(I)
             END DO
              FPSXY = MFXY(1:MVSIZ)
              FPSYZ = MFYZ(1:MVSIZ)
              FPSXZ = MFXZ(1:MVSIZ)          
              FPSYX = MFYX(1:MVSIZ)
              FPSZY = MFZY(1:MVSIZ)
              FPSZX = MFZX(1:MVSIZ)    
C---------------------------
C       ORTHOTROPE 
C---------------------------

!     ISMSTR10_FP( 1,1) =   FPSXX(1:MVSIZ)
!     ISMSTR10_FP( 2,1) =   FPSYY(1:MVSIZ) 
!     ISMSTR10_FP( 3,1) =   FPSZZ(1:MVSIZ) 
!     ISMSTR10_FP( 4,1) =   FPSXY(1:MVSIZ) 
!     ISMSTR10_FP( 5,1) =   FPSYZ(1:MVSIZ)
!     ISMSTR10_FP( 6,1) =   FPSXZ(1:MVSIZ) 
!     ISMSTR10_FP( 7,1) =   FPSYX(1:MVSIZ) 
!     ISMSTR10_FP( 8,1) =   FPSZY(1:MVSIZ) 
!     ISMSTR10_FP( 9,1) =   FPSZX(1:MVSIZ)
!     ISMSTR10_FP(10,1) =   UPSXX(1:MVSIZ)
!     ISMSTR10_FP(11,1) =   UPSYY(1:MVSIZ)
!     ISMSTR10_FP(12,1) =   UPSZZ(1:MVSIZ)
!     ISMSTR10_FP(13,1) =   UPSXY(1:MVSIZ) 
!     ISMSTR10_FP(14,1) =   UPSYZ(1:MVSIZ)
!     ISMSTR10_FP(15,1) =   UPSXZ(1:MVSIZ) 

             IF (ISORTH /= 0) THEN
             CALL MREPLOC(GAMA,R11,R12,R13,R21,R22    ,
     2                    R23 ,R31,R32,R33,
     3                    RX  ,RY ,RZ ,SX ,SY ,SZ     ,
     4                    TX  ,TY ,TZ )
             CALL MROTENSNS(LFT,LLT,
     .               ISMSTR10_FP(1,1),ISMSTR10_FP(2,1),ISMSTR10_FP(3,1),
     .               ISMSTR10_FP(4,1),ISMSTR10_FP(5,1),ISMSTR10_FP(6,1),
     .               ISMSTR10_FP(7,1),ISMSTR10_FP(8,1),ISMSTR10_FP(9,1),
     .               R11,R12,R13,
     .               R21,R22,R23,
     .               R31,R32,R33)
            END IF !(ISORTH /= 0) THEN
            CALL EPSF2U(
     1      LLT    ,
     *      ISMSTR10_FP( 1,1), ISMSTR10_FP( 4,1), ISMSTR10_FP( 6,1), ISMSTR10_FP( 7,1),    
     *      ISMSTR10_FP( 2,1), ISMSTR10_FP( 5,1), ISMSTR10_FP( 9,1), ISMSTR10_FP( 8,1),ISMSTR10_FP( 3,1), 
     *      ISMSTR10_FP(10,1), ISMSTR10_FP(11,1), ISMSTR10_FP(12,1), ISMSTR10_FP(13,1),ISMSTR10_FP(14,1),
     *      ISMSTR10_FP(15,1)  )

          ELSE  ! (ISMSTR == 10)          
            ISMSTR10_FP( 1,1) =   ZERO
            ISMSTR10_FP( 2,1) =   ZERO
            ISMSTR10_FP( 3,1) =   ZERO
            ISMSTR10_FP( 4,1) =   ZERO
            ISMSTR10_FP( 5,1) =   ZERO
            ISMSTR10_FP( 6,1) =   ZERO
            ISMSTR10_FP( 7,1) =   ZERO
            ISMSTR10_FP( 8,1) =   ZERO
            ISMSTR10_FP( 9,1) =   ZERO
            ISMSTR10_FP(10,1) =   ZERO
            ISMSTR10_FP(11,1) =   ZERO
            ISMSTR10_FP(12,1) =   ZERO
            ISMSTR10_FP(13,1) =   ZERO
            ISMSTR10_FP(14,1) =   ZERO
            ISMSTR10_FP(15,1) =   ZERO
          END IF! (ISMSTR == 10) 

           
          CALL ENG_MDS_SIGEPS_S( NCYCLE ,IMAT, NGL,NEL    ,
     *                          IPTR     , IPTS     , IPTT     , NPAR     ,NUVAR     ,NFUNC     ,IFUNC  ,
     *                          NPF      , TF       , TT       , DT1      , UPARAM   , UVAR     ,
     *                          RHO0     , RHO      , VOLN     , EINT     , EL_TEMP  , AMU      ,
     *                          EP1      , EP2      , EP3      , EP4      , EP5      , EP6      ,
     *                          DE1      , DE2      , DE3      , DE4      , DE5      , DE6      ,
     *                          ES1      , ES2      , ES3      , ES4      , ES5      , ES6      ,
     *                          SO1      , SO2      , SO3      , SO4      , SO5      , SO6   ,
     *                          S1       , S2       , S3       , S4       , S5       , S6       ,
     *                          SV1      , SV2      , SV3      , SV4      , SV5      , SV6      ,
     *                          SSP      , VIS      , OFF      , R11      , R12      , R13      ,
     *                          R21      , R22      , R23      , R31      , R32      , R33      ,
     *                          ISMSTR   , ISMSTR10_FP         ,DEFP      , DPLA     , SIGY     ,
     *                          ADDITIONAL_FLT_PARAMETERS,  ADDITIONAL_INT_PARAMETERS )
C                        
        ELSE
             CALL MDS_USERLIB_NAME_GET(MDS_LIBNAME,LENGTH)
             CALL ANCMSG(MSGID=287,ANMODE=ANINFO,
     .            C1=MDS_LIBNAME(1:LENGTH))
             CALL ARRET(2)
        ENDIF
#endif
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
            DO I=LFT,LLT
              DPLA(I)  = MAX(VARNL(I),ZERO)
            ENDDO
            EL_PLA => LBUF%PLANL(1:NEL)
          ELSE
C for all plastic law
            DO I=LFT,LLT 
              DPLA(I)  = DEFP(I) - DEFP0(I)
            ENDDO
            EL_PLA => DEFP(1:NEL)
          ENDIF
        ELSE
          DO I=LFT,LLT 
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
          DO I=LFT,LLT 
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
C------------------------------------------------------------
C     Recovering non-local plastic strain-rate
C------------------------------------------------------------
          IF (ELBUF_TAB(NG)%BUFLY(ILAY)%L_PLA > 0) THEN
            IF (INLOC > 0) THEN 
              DO I = LFT,LLT
                EPSP1(I) = LBUF%EPSDNL(I)
              ENDDO
            ENDIF
          ENDIF
c----             
          UVARF   => FBUF%FLOC(IR)%VAR     
          IRUPT    = FBUF%FLOC(IR)%ILAWF   
          NVARF    = FBUF%FLOC(IR)%NVAR    
          DFMAX   => FBUF%FLOC(IR)%DAMMX  
          DAMINI  => FBUF%FLOC(IR)%DAMINI    
          TDEL    => FBUF%FLOC(IR)%TDEL   
          NPAR    =  IPM(112+IP, MX)   
          IADBUF  =  IPM(114+IP ,MX)
          UPARAMF => BUFMAT(IADBUF:IADBUF+NPAR) 
          NFUNC   =  IPM(115+IP, MX)             
          FLD_IDX => FBUF%FLOC(IR)%INDX
          FOFF    => FBUF%FLOC(IR)%OFF
          DAM     => FBUF%FLOC(IR)%DAM
          IFUNC   => IPM(115+IP+1:115+IP+NFUNC, MX)         
c          DO I=1,NFUNC
c            IFUNC(I) = IPM(115+IP+I, MX)         
c          ENDDO
c----  
          IF (IRUPT == 1)THEN                                            
C---- Johnson cook            
              CALL FAIL_JOHNSON(LLT ,NPAR,NVARF,NFUNC,IFUNC,
     2                    NPF ,TF  ,TT  ,DT1  ,UPARAMF,
     3                    NGL ,IPM ,MAT ,
     4                    S1  ,S2  ,S3  ,S4   ,S5   ,S6,
     5                    DPLA,EPSP1,TSTAR,UVARF,OFF,IP,
     6                    DFMAX,TDEL )                   
          ELSEIF (IRUPT == 2) THEN                                        
C           Tuler Butcher                                                
               CALL FAIL_TBUTCHER_S(LLT ,NPAR,NVARF,NFUNC,IFUNC,
     2                    NPF ,TF  ,TT  ,DT1  ,UPARAMF,
     3                    NGL ,IPM ,MAT ,
     4                    S1  ,S2  ,S3  ,S4   ,S5   ,S6,
     5                   UVARF,OFF ,IP  ,DFMAX,TDEL )                                    
          ELSEIF (IRUPT == 3) THEN                                        
C   wilkins          
               CALL FAIL_WILKINS_S(LLT ,NPAR,NUVARR,NFUNC,IFUNC,
     2                    NPF ,TF  ,TT  ,DT1  ,UPARAMF,
     3                    NGL ,IPM ,MAT ,
     4                    S1  ,S2  ,S3  ,S4   ,S5   ,S6,
     5                    DPLA,UVARF,OFF,IP   ,DFMAX,TDEL )                               
          ELSEIF (IRUPT == 4)THEN                                         
C   user1         
            IF (LOGICAL_USERL_AVAIL)THEN
               USER_UELR(1:LLT)=GBUF%UELR(1:LLT)

               TT_LOCAL = TT
               CALL ENG_USERLIB_FLAW(IRUPT,LLT ,NPAR,NUVARR,NFUNC,IFUNC,
     2                    NPF ,TF  ,TT_LOCAL  ,DT1  ,UPARAMF,
     3                    NGL ,IBIDON1,IBIDON2,IBIDON3 ,IBIDON4,
     4                    EP1  ,EP2 ,EP3  ,EP4  ,EP5  ,EP6 ,
     5                    ES1  ,ES2 ,ES3  ,ES4  ,ES5  ,ES6 ,
     6                    S1   ,S2  ,S3   ,S4   ,S5   ,S6  ,    
     7                    DEFP ,DPLA,EPSP1,UVARF,OFF  ,                    
     8                    DELTAX,VOLN,USER_UELR,BIDON4,BIDON5)
               GBUF%UELR(1:LLT) = USER_UELR(1:LLT)
            ELSE
!               CALL F04LAW(LLT ,NPAR,NUVARR,NFUNC,IFUNC,
!     2                    NPF   ,TF  ,TT  ,DT1  ,UPARAMF,
!     3                    NGL   ,IBIDON1,IBIDON2,IBIDON3 ,IBIDON4,
!     4                    EP1   ,EP2 ,EP3  ,EP4  ,EP5  ,EP6 ,
!     5                    ES1   ,ES2 ,ES3  ,ES4  ,ES5  ,ES6 ,
!     6                    S1    ,S2  ,S3   ,S4   ,S5   ,S6  ,    
!     7                    DEFP  ,DPLA,EPSP1,UVARF,OFF  ,                    
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
          ELSEIF (IRUPT == 5)THEN                                         
C   user2         
            IF (LOGICAL_USERL_AVAIL)THEN
               TT_LOCAL = TT
               USER_UELR(1:LLT)=GBUF%UELR(1:LLT)
               CALL ENG_USERLIB_FLAW(IRUPT,LLT ,NPAR,NUVARR,NFUNC,IFUNC,
     2                    NPF ,TF  ,TT_LOCAL  ,DT1  ,UPARAMF,
     3                    NGL ,IBIDON1,IBIDON2,IBIDON3 ,IBIDON4,
     4                    EP1  ,EP2 ,EP3  ,EP4  ,EP5  ,EP6 ,
     5                    ES1  ,ES2 ,ES3  ,ES4  ,ES5  ,ES6 ,
     6                    S1   ,S2  ,S3   ,S4   ,S5   ,S6  ,    
     7                    DEFP ,DPLA,EPSP1,UVARF,OFF  ,                    
     8                    DELTAX,VOLN,USER_UELR,BIDON4,BIDON5)
               GBUF%UELR(1:LLT) = USER_UELR(1:LLT)
            ELSE
!               CALL F05LAW(LLT ,NPAR,NUVARR,NFUNC,IFUNC,
!     2                    NPF ,TF  ,TT  ,DT1  ,UPARAMF,
!     3                    NGL ,IBIDON1,IBIDON2,IBIDON3 ,IBIDON4,
!     4                    EP1  ,EP2 ,EP3  ,EP4  ,EP5  ,EP6 ,
!     5                    ES1  ,ES2 ,ES3  ,ES4  ,ES5  ,ES6 ,
!     6                    S1   ,S2  ,S3   ,S4   ,S5   ,S6  ,    
!     7                    DEFP ,DPLA,EPSP1,UVARF,OFF  ,                    
!     8                    DELTAX ,VOLN,GBUF%UELR,BIDON4,BIDON5)
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
          ELSEIF (IRUPT == 6)THEN                                         
C   user3         
           IF (LOGICAL_USERL_AVAIL)THEN
               TT_LOCAL = TT
               USER_UELR(1:LLT)=GBUF%UELR(1:LLT)
               CALL ENG_USERLIB_FLAW(IRUPT,LLT ,NPAR,NUVARR,NFUNC,IFUNC,
     2                    NPF  ,TF  ,TT_LOCAL  ,DT1  ,UPARAMF,
     3                    NGL  ,IBIDON1,IBIDON2,IBIDON3 ,IBIDON4,
     4                    EP1  ,EP2 ,EP3  ,EP4  ,EP5  ,EP6 ,
     5                    ES1  ,ES2 ,ES3  ,ES4  ,ES5  ,ES6 ,
     6                    S1   ,S2  ,S3   ,S4   ,S5   ,S6  ,    
     7                    DEFP ,DPLA,EPSP1,UVARF,OFF  ,                    
     8                    DELTAX,VOLN,USER_UELR,BIDON4,BIDON5)
               GBUF%UELR(1:LLT) = USER_UELR(1:LLT)
            ELSE
!               CALL F06LAW(LLT ,NPAR,NUVARR,NFUNC,IFUNC,
!     2                    NPF  ,TF  ,TT  ,DT1  ,UPARAMF,
!     3                    NGL  ,IBIDON1,IBIDON2,IBIDON3 ,IBIDON4,
!     4                    EP1  ,EP2 ,EP3  ,EP4  ,EP5  ,EP6 ,
!     5                    ES1  ,ES2 ,ES3  ,ES4  ,ES5  ,ES6 ,
!     6                    S1   ,S2  ,S3   ,S4   ,S5   ,S6  ,    
!     7                    DEFP ,DPLA,EPSP1,UVARF,OFF  ,                    
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
          ELSEIF (IRUPT == 7)THEN                                        
                  CALL FAIL_FLD_TSH(
     1            LLT       ,NPAR      ,NVARF     ,NFUNC    ,IFUNC      ,
     2            NPF       ,TF        ,TT        ,DT1      ,UPARAMF,   
     3            UVARF     ,NGL       ,IPG       ,ILAY     ,IPTT         ,
     5            ES1       ,ES2       ,ES4       ,ES5      ,ES6          ,
     6            OFF       ,FOFF      ,FLD_IDX   ,DAM      ,DFMAX     )
          ELSEIF (IRUPT == 8)THEN                                         
C---- Johnson cook + spalling          
              CALL FAIL_SPALLING_S(LLT ,NPAR,NVARF,NFUNC,IFUNC,
     2                    NPF ,TF  ,TT  ,DT1  ,UPARAMF,
     3                    NGL ,IPM ,MAT ,
     4                    S1  ,S2  ,S3  ,S4   ,S5   ,S6      ,
     5                    DPLA,EPSP1,TSTAR,UVARF,OFF         ,
     6                    DFMAX,TDEL ,OFFG)                   
          ELSEIF (IRUPT == 9)THEN                                         
C---- wierzbicki
              CALL FAIL_WIERZBICKI_S(LLT ,NPAR,NUVARR,NFUNC,IFUNC,
     2                    NPF ,TF  ,TT  ,DT1  ,UPARAMF,
     3                    NGL ,IPM ,MAT ,
     4                    S1  ,S2  ,S3  ,S4   ,S5   ,S6,
     5                    DPLA,DEFP,UVARF,OFF ,DFMAX,
     6                    TDEL )                          
          ELSEIF (IRUPT == 10)THEN                                        
C---- strain tension            
              CALL FAIL_TENSSTRAIN_S(LLT ,NPAR,NVARF,NFUNC,IFUNC       ,
     2             NPF ,TF  ,TT  ,DT1  ,UPARAMF,
     3             NGL ,IPM ,MAT ,DELTAX   ,TSTAR ,ISMSTR, 
     4             ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6     ,
     5             S1  ,S2  ,S3  ,S4   ,S5   ,S6      ,
     6             EPSP1,UVARF,OFF ,IP   ,DFMAX,TDEL  ,
     7             MFXX   ,MFXY   ,MFXZ   ,MFYX    ,MFYY   ,MFYZ   ,                 
     8             MFZX   ,MFZY   ,MFZZ   )
          ELSEIF (IRUPT == 11)THEN                                        
C---- energy failure  
              CALL FAIL_ENERGY_S(LLT ,NPAR,NUVARR,NFUNC,IFUNC       ,
     2                    NPF ,TF  ,TT  ,DT1  ,UPARAMF,
     3                    NGL ,IPM ,MAT ,
     5                    S1  ,S2  ,S3  ,S4   ,S5   ,S6      ,
     6                  VOLN,EINT,EPSP1,UVARF,OFF,IP,DFMAX   ,
     7                   TDEL )                    
          ELSEIF (IRUPT == 23) THEN                                      
C---- tabulated failure model
               CALL FAIL_TAB_S(
     1         LLT      ,NVARF    ,NPF      ,TF       ,TT       ,        
     2         UPARAMF  ,NGL      ,IPM      ,MAT      ,EL_LEN   ,        
     3         S1       ,S2       ,S3       ,S4       ,S5       ,S6,    
     4         DEFP     ,DPLA     ,EPSP1    ,TSTAR    ,UVARF ,             
     5         OFF      ,IP       ,TABLE    ,DFMAX    ,TDEL  ,
     6         NFUNC    ,IFUNC )                                      
          ELSEIF (IRUPT == 24) THEN                                      
c   --- Orthotropic strain failure
              CALL FAIL_ORTHSTRAIN(
     1        LLT      ,NPAR     ,NVARF    ,NFUNC    ,IFUNC    ,
     2        NPF      ,TF       ,TT       ,DT1      ,UPARAMF  ,ISMSTR  ,
     3        EP1      ,EP2      ,EP3      ,EP4      ,EP5      ,EP6     ,
     4        ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6     ,
     5        S1       ,S2       ,S3       ,S4       ,S5       ,S6      ,
     6        UVARF    ,OFF      ,IPG      ,NGL      ,DFMAX    ,TDEL    ,
     7        GBUF%UELR,NPG      ,DELTAX   )
          ELSEIF (IRUPT == 27) THEN                                      
c ---   extended mohr coulomb failure model                                             
              CALL FAIL_EMC(
     1         LLT      ,NVARF    ,NPF      ,TF       ,TT       ,        
     2         DT1      ,UPARAMF     ,NGL      ,IPM      ,MAT      ,        
     3         S1       ,S2       ,S3       ,S4       ,S5       ,S6,    
     4         DEFP     ,DPLA     ,EPSP1    ,UVARF ,             
     5         OFF      ,IP       ,DFMAX    ,TDEL    )                                      
          ELSEIF (IRUPT == 29) THEN                                      
c ---   MIT Wierzbicki Sahraei electric battery failure                                               
              CALL FAIL_SAHRAEI_S(
     1        LLT      ,NFUNC    ,IFUNC    ,NPF      ,TF       ,
     2        TT       ,NGL      ,IPM      ,MAT      ,UPARAMF  ,
     3        ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6      , 
     4        OFF      ,IP       ,DFMAX    ,TDEL     ,DELTAX   ,
     5        NVARF    ,UVARF    ) 
          ELSEIF (IRUPT == 30) THEN                                      
c  --- Biquadratic failure model                                  
              CALL FAIL_BIQUAD_S(
     1        LLT      ,NPAR     ,NVARF    ,NFUNC    ,IFUNC    ,
     2        NPF      ,TF       ,TT       ,DT1      ,UPARAMF   ,
     3        NGL      ,IPM      ,MAT      ,                   
     4        S1       ,S2       ,S3       ,S4       ,S5       ,S6 ,
     5        DPLA     ,EPSP1    ,TSTAR    ,UVARF    ,OFF,IP   ,
     6        DFMAX    ,TDEL     ,EL_LEN   )    
          ELSEIF (IRUPT == 33) THEN                                
c  --- Mullins Ogden-Roxburgh damage model                                  
              CALL FAIL_MULLINS_OR_S(
     1        LLT      ,NPAR     ,NVARF    ,NPF      ,TF       ,
     2        TT       ,DT1      ,UPARAMF,NGL , 
     3        MAT      ,S1       ,S2       ,S3       ,S4       ,
     4        S5       ,S6       ,UVARF    ,OFF      ,IP       ,
     5        DFMAX    ,LBUF%EPSA) 
          ELSEIF (IRUPT == 34) THEN                                           
C  --- Cockroft-Latham failure model             
              CALL FAIL_COCKROFT_S(LLT    ,NPAR     ,NVARF,
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
     .        LLT     ,NPAR    ,NVARF    ,TT       ,DT1       ,UPARAMF,
     .        ES1     ,ES2     ,ES3      ,ES4      ,ES5       ,ES6    ,
     .        S1      ,S2      ,S3       ,S4       ,S5        ,S6     ,
     .        UVARF   ,OFF     ,NGL      ,DFMAX    ,ISMSTR    ) 
c
          ELSEIF (IRUPT == 37) THEN                                      
c ---       tabulated failure model (old, obsolete version)
              CALL FAIL_TAB_OLD_S(
     1         LLT      ,NVARF    ,NPF      ,TF       ,TT       ,        
     2         UPARAMF  ,NGL      ,IPM      ,MAT      ,EL_LEN   ,        
     3         S1       ,S2       ,S3       ,S4       ,S5       ,S6,    
     4         DEFP     ,DPLA     ,EPSP1    ,TSTAR    ,UVARF ,             
     5         OFF      ,IP       ,DFMAX    ,TDEL     ,
     6         NFUNC    ,IFUNC ) 
c
          ELSEIF (IRUPT == 38) THEN                                      
c  --- Orthotropic biquadratic failure model                                  
              CALL FAIL_ORTHBIQUAD_S(
     1          LLT      ,NPAR     ,NVARF    ,NFUNC    ,IFUNC    ,
     2          NPF      ,TF       ,TT       ,DT1      ,UPARAMF  ,
     3          NGL      ,DPLA     ,EPSP1    ,UVARF    ,OFF      ,              
     4          S1       ,S2       ,S3       ,S4       ,S5       ,S6       ,
     5          DFMAX    ,TDEL     ,EL_LEN   )  
c
          ELSEIF (IRUPT == 39) THEN                                      
c  --- GENE1 failure model                                  
              CALL FAIL_GENE1_S(
     1          LLT      ,NPAR     ,NVARF    ,NFUNC    ,IFUNC    ,LBUF%OFF ,
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
     1          LLT      ,NPAR     ,NVARF    ,TT       ,DT1      ,UPARAMF  ,
     2          S1       ,S2       ,S3       ,S4       ,S5       ,S6       ,
     3          NGL      ,DPLA     ,UVARF    ,OFF      ,DFMAX    ,TDEL     )
c
          ELSEIF (IRUPT == 41) THEN                                      
C---- Tabulated failure model version 2
              CALL FAIL_TAB2_S(
     1          LLT      ,NPAR     ,NVARF    ,NFUNC    ,IFUNC    ,
     2          NPF      ,TABLE    ,TF       ,TT       ,UPARAMF  , 
     3          NGL      ,EL_LEN   ,DPLA     ,EPSP1    ,UVARF    ,     
     5          S1       ,S2       ,S3       ,S4       ,S5       ,S6       ,               
     6          EL_TEMP  ,OFF      ,DFMAX    ,TDEL     ,DMG_SCALE,
     7          GBUF%UELR,IPG      ,NPG      ,LBUF%OFF ) 
c
          ELSEIF (IRUPT == 42) THEN                                      
C---- Inievo failure model
              CALL FAIL_INIEVO_S(
     1          NEL      ,NPAR     ,NVARF    ,NFUNC    ,IFUNC    ,
     2          NPF      ,TABLE    ,TF       ,TT       ,UPARAMF  , 
     3          NGL      ,EL_LEN   ,DPLA     ,EPSP1    ,UVARF    ,     
     5          S1       ,S2       ,S3       ,S4       ,S5       ,S6       ,               
     6          EL_PLA   ,EL_TEMP  ,SIGY     ,OFF      ,DFMAX    ,
     7          TDEL     ,DMG_SCALE,GBUF%UELR,IPG      ,NPG      ,
     8          LBUF%OFF ,DAMINI   ,GBUF%VOL ,INLOC    )  
c
          ELSEIF (IRUPT == 44) THEN                                      
C---- Tsai-Wu failure model
              CALL FAIL_TSAIWU_S(
     1          NEL      ,NVARF    ,IPG      ,ILAY     ,NPG      ,TT       ,         
     2          DT1      ,UPARAMF  ,NGL      ,OFF      ,LBUF%OFF ,GBUF%NOFF,
     3          S1       ,S2       ,S3       ,S4       ,S5       ,S6       ,  
     4          UVARF    ,NPAR     ,DFMAX    ,TDEL     ,DMG_SCALE) 
c             
          ELSEIF (IRUPT == 45) THEN                                      
C---- Tsai-Hill failure model
              CALL FAIL_TSAIHILL_S(
     1          NEL      ,NVARF    ,IPG      ,ILAY     ,NPG      ,TT       ,         
     2          DT1      ,UPARAMF  ,NGL      ,OFF      ,LBUF%OFF ,GBUF%NOFF,
     3          S1       ,S2       ,S3       ,S4       ,S5       ,S6       ,  
     4          UVARF    ,NPAR     ,DFMAX    ,TDEL     ,DMG_SCALE) 
c             
          ELSEIF (IRUPT == 46) THEN                                      
c---- Hoffman failure model
              CALL FAIL_HOFFMAN_S(
     1          NEL      ,NVARF    ,IPG      ,ILAY     ,NPG      ,TT       ,         
     2          DT1      ,UPARAMF  ,NGL      ,OFF      ,LBUF%OFF ,GBUF%NOFF,
     3          S1       ,S2       ,S3       ,S4       ,S5       ,S6       ,  
     4          UVARF    ,NPAR     ,DFMAX    ,TDEL     ,DMG_SCALE) 
c             
          ENDIF
        ENDDO ! IR = 1,NFAIL
      ENDIF   ! NFAIL
      IF ((ITASK==0).AND.(IMON_MAT==1))CALL STOPTIME(121,1)
C-----------------------------------------------------------------------
C     viscous stress (/VISC models) 
C-----------------------------------------------------------------------
      IVISC = IPM(222,MX)
      IF (IVISC > 0) THEN
         CALL VISCMAIN(
     .        LLT     ,MX  ,IVISC   ,IPM     ,BUFMAT  ,
     .        VBUF%VAR,RHO0    ,VIS     ,SSP     ,DT1     , 
     .        EP1     ,EP2     ,EP3     ,EP4     ,EP5     ,EP6     ,
     .        SV1     ,SV2     ,SV3     ,SV4     ,SV5     ,SV6     )
       ENDIF 
C---------------------------
C
C  calcul of viscosity ( Navier-Stokes)
C
      IF (ISVIS > 0) THEN
      CALL NSVISUL(LLT     ,PM      ,OFF     ,RHO     ,GEO    ,
     2             PID     ,SSP     ,AIRE    ,VOLN    ,EP1    ,
     3             EP2     ,EP3     ,EP4     ,EP5     ,EP6    ,
     4             SV1     ,SV2     ,SV3     ,SV4     ,SV5    ,
     5             SV6     ,S3      ,ES3     ,RHO0    ,RHOREF )
      ENDIF 
c--------------------------------------------------------
c     DAMAGED STRESSES
c---------------------------------------------------------
      IF (DMG_FLAG > 0) THEN 
        DO I = 1,NEL
          S1(I) = S1(I)*DMG_SCALE(I)
          S2(I) = S2(I)*DMG_SCALE(I)
          S3(I) = S3(I)*DMG_SCALE(I)
          S4(I) = S4(I)*DMG_SCALE(I)
          S5(I) = S5(I)*DMG_SCALE(I)
          S6(I) = S6(I)*DMG_SCALE(I)
        ENDDO
      ENDIF
c---------------------------------------------------------
      DO I=LFT,LLT
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
      IF(ISVIS > 0) THEN
        DO I=LFT,LLT
            SIGV(I,1) = SVIS(I,1)
            SIGV(I,2) = SVIS(I,2)
            SIGV(I,3) = SVIS(I,3)
            SIGV(I,4) = SVIS(I,4)
            SIGV(I,5) = SVIS(I,5)
            SIGV(I,6) = SVIS(I,6)
          ENDDO
      ENDIF 
       IF (IVISC > 0) THEN
         DO I=LFT,LLT  
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
          DO I=LFT,LLT
            VARNL(I) = DEFP(I)
          ENDDO
        ENDIF
      ENDIF
C------------------------------------------------------------
C     DEFINE SOUND SPEED  (IN ALL CASE)
C     DEFINE DYNAMIC VISCOSITY (FOR VISCOUS LAW)
C-----------------------
      DO I=LFT,LLT
        IF (SSP(I) == ZERO) SSP(I)=SQRT(C1(I)/RHO0(I))
      ENDDO
C-------------------------------------------
C   BULK VISCOSITY AND TIME STEP COMPUTATION
C   THIS SUBROUTINE RETURN THE NEW BULK VISCOSITY Q
C-----------------------
      IF (JSPH == 0)THEN  
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
C-----------
      DO I=LFT,LLT                                                           
        P2 = -(SOLD1(I)+SIG(I,1)+SOLD2(I)+SIG(I,2)+SOLD3(I)+SIG(I,3))* THIRD 
        E1 = D1(I)*(SOLD1(I)+SIG(I,1) + P2+TWO*SVIS(I,1))                   
        E2 = D2(I)*(SOLD2(I)+SIG(I,2) + P2+TWO*SVIS(I,2))                   
        E3 = D3(I)*(SOLD3(I)+SIG(I,3) + P2+TWO*SVIS(I,3))                   
        E4 = D4(I)*(SOLD4(I)+SIG(I,4) + TWO*SVIS(I,4))                      
        E5 = D5(I)*(SOLD5(I)+SIG(I,5) + TWO*SVIS(I,5))                      
        E6 = D6(I)*(SOLD6(I)+SIG(I,6) + TWO*SVIS(I,6))         
        EINC = VOL_AVG(I)*DT1 * (E1+E2+E3+E4+E5+E6+E7(I))
     .       - DVOL(I)*(Q(I) + QOLD(I) + P2)
        EINT(I) = EINT(I) + OFF(I)*EINC*HALF
      END DO                                                                 
      QOLD(LFT:LLT) = Q(LFT:LLT)
C------------------------------------------
      RETURN
      END
