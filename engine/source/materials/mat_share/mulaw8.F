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
Chd|  MULAW8                        source/materials/mat_share/mulaw8.F
Chd|-- called by -----------
Chd|        MMAIN8                        source/materials/mat_share/mmain8.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FAIL_BIQUAD_S                 source/materials/fail/biquad/fail_biquad_s.F
Chd|        FAIL_EMC                      source/materials/fail/emc/fail_emc.F
Chd|        FAIL_ENERGY_S                 source/materials/fail/energy/fail_energy_s.F
Chd|        FAIL_JOHNSON                  source/materials/fail/johnson_cook/fail_johnson.F
Chd|        FAIL_ORTHBIQUAD_S             source/materials/fail/orthbiquad/fail_orthbiquad_s.F
Chd|        FAIL_RTCL_S                   source/materials/fail/rtcl/fail_rtcl_s.F
Chd|        FAIL_SAHRAEI_S                source/materials/fail/sahraei/fail_sahraei_s.F
Chd|        FAIL_SPALLING_S               source/materials/fail/spalling/fail_spalling_s.F
Chd|        FAIL_TAB_S                    source/materials/fail/tabulated/fail_tab_s.F
Chd|        FAIL_TBUTCHER_S               source/materials/fail/tuler_butcher/fail_tbutcher_s.F
Chd|        FAIL_TENSSTRAIN_S             source/materials/fail/tensstrain/fail_tensstrain_s.F
Chd|        FAIL_VISUAL_S                 source/materials/fail/visual/fail_visual_s.F
Chd|        FAIL_WIERZBICKI_S             source/materials/fail/wierzbicki/fail_wierzbicki_s.F
Chd|        FAIL_WILKINS_S                source/materials/fail/wilkins/fail_wilkins_s.F
Chd|        MQVISC8                       source/materials/mat_share/mqvisc8.F
Chd|        MREPLOC                       source/materials/mat_share/mreploc.F
Chd|        MROTENS                       source/materials/mat_share/mrotens.F
Chd|        SIGEPS28                      source/materials/mat/mat028/sigeps28.F
Chd|        SIGEPS33                      source/materials/mat/mat033/sigeps33.F
Chd|        SIGEPS34                      source/materials/mat/mat034/sigeps34.F
Chd|        SIGEPS35                      source/materials/mat/mat035/sigeps35.F
Chd|        SIGEPS36                      source/materials/mat/mat036/sigeps36.F
Chd|        SIGEPS38                      source/materials/mat/mat038/sigeps38.F
Chd|        SIGEPS40                      source/materials/mat/mat040/sigeps40.F
Chd|        SIGEPS41                      source/materials/mat/mat041/sigeps41.F
Chd|        SIGEPS42                      source/materials/mat/mat042/sigeps42.F
Chd|        SIGEPS44                      source/materials/mat/mat044/sigeps44.F
Chd|        SIGEPS45                      source/materials/mat/mat045/sigeps45.F
Chd|        SIGEPS48                      source/materials/mat/mat048/sigeps48.F
Chd|        SIGEPS50                      source/materials/mat/mat050/sigeps50.F
Chd|        SIGEPS52                      source/materials/mat/mat052/sigeps52.F
Chd|        SIGEPS53                      source/materials/mat/mat053/sigeps53.F
Chd|        SIGEPS56                      source/materials/mat/mat056/sigeps56.F
Chd|        SIGEPS60                      source/materials/mat/mat060/sigeps60.F
Chd|        SIGEPS62                      source/materials/mat/mat062/sigeps62.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE MULAW8(
     1   LFT,     LLT,     NFT,     MTN,
     2   NPT,     D1,      D2,      D3,
     3   D4,      D5,      D6,      PM,
     4   OFF,     SIG,     EINT,    RHO,
     5   QOLD,    VOL,     GAMA,    BUFLY,
     6   BUFMAT,  TF,      NPF,     STIFN,
     7   VOLN,    VOLGP,   DELTAX,  RHO0,
     8   DVOL,    VD2,     VIS,     EPSD,
     9   MAT,     NC,      NGL,     WXX,
     A   WYY,     WZZ,     GEO,     PID,
     B   DT2T,    NELTST,  ITYPTST, IPLA,
     C   RX,      RY,      RZ,      SX,
     D   SY,      SZ,      TX,      TY,
     E   TZ,      ISMSTR,  IPM,     OFFG,
     F   ISORTH,  ET,      MSSA,    DMELS,
     G   TABLE,   IHET,    SSP,     ITASK,
     H   NEL,     ITY,     JSMS,    JSPH,
     I   JTHE,    JTUR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE ELBUFDEF_MOD         
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
#include      "nsvis_c.inc"
#include      "param_c.inc"
#include      "com08_c.inc"
#include      "scr19_c.inc"
#include      "timeri_c.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ITY
      INTEGER, INTENT(IN) :: JSMS
      INTEGER, INTENT(IN) :: JSPH
      INTEGER, INTENT(IN) :: JTHE
      INTEGER, INTENT(IN) :: JTUR
      INTEGER LFT,LLT,NFT,NPT,MTN,ISORTH,IPLA,ISMSTR,NUVARR,NUVAR,NVARTMP,
     .  NELTST,ITYPTST,IHET,ITASK,NEL
      INTEGER NPF(*),MAT(MVSIZ),NC(8,MVSIZ),NGL(MVSIZ),
     .   PID(MVSIZ),IPM(NPROPMI,*)
      my_real
     .  DT2T
      my_real 
     .  PM(NPROPM,*),GAMA(*),RHO(LLT),QOLD(LLT),VOL(LLT),EPSD(LLT), 
     .  OFF(LLT), SIG(NEL,6),EINT(LLT),OFFG(LLT),CST1(LLT), 
     .  BUFMAT(*), TF(*),STIFN(*),ET(*), MSSA(*) ,DMELS(*),
     .  DVOL(MVSIZ) ,VOLGP(MVSIZ,NPT),GEO(NPROPG,*),
     .  VD2(MVSIZ), DELTAX(MVSIZ),VOLN(MVSIZ),
     .  D1(MVSIZ,NPT),D2(MVSIZ,NPT),D3(MVSIZ,NPT),D4(MVSIZ,NPT) , 
     .  D5(MVSIZ,NPT),D6(MVSIZ,NPT),VIS(MVSIZ),RHO0(MVSIZ),
     .  WXX(MVSIZ,NPT) ,WYY(MVSIZ,NPT) ,WZZ(MVSIZ,NPT),
     .  RX(MVSIZ)    ,RY(MVSIZ)    ,RZ(MVSIZ),
     .  SX(MVSIZ)    ,SY(MVSIZ)    ,SZ(MVSIZ),
     .  TX(MVSIZ)    ,TY(MVSIZ)    ,TZ(MVSIZ),SSP(MVSIZ)          
      TARGET :: BUFMAT
      TYPE(TTABLE) TABLE(*)
      TYPE (BUF_LAY_), TARGET :: BUFLY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NPAR,IADBUF,NFUNC,I,J,II,IPT,JPT,IR,IP,ISRATE,
     .  IFUNC(MAXFUNC),IDUM1,IFAIL(MVSIZ),MATF(MVSIZ),IC,IFLAG,
     .  NVARF,NVARF_MAX,MX, IEXPAN8,JJ(6),INLOC,DMG_FLAG
      my_real 
     .C1(MVSIZ),PNEW(MVSIZ),PP(MVSIZ),DEFP(MVSIZ),
     .EP1(MVSIZ),EP2(MVSIZ),EP3(MVSIZ),EP4(MVSIZ),EP5(MVSIZ),EP6(MVSIZ),
     .S1(MVSIZ) ,S2(MVSIZ) ,S3(MVSIZ) ,S4(MVSIZ) ,S5(MVSIZ) ,S6(MVSIZ),
     .ES1(MVSIZ),ES2(MVSIZ),ES3(MVSIZ),ES4(MVSIZ),ES5(MVSIZ),ES6(MVSIZ),
     .DE1(MVSIZ),DE2(MVSIZ),DE3(MVSIZ),DE4(MVSIZ),DE5(MVSIZ),DE6(MVSIZ),
     .SV1(MVSIZ),SV2(MVSIZ),SV3(MVSIZ),SV4(MVSIZ),SV5(MVSIZ),SV6(MVSIZ),
     .SO1(MVSIZ),SO2(MVSIZ),SO3(MVSIZ),SO4(MVSIZ),SO5(MVSIZ),SO6(MVSIZ),
     .R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .SOLD1(MVSIZ) ,SOLD2(MVSIZ) ,SOLD3(MVSIZ) ,
     .SOLD4(MVSIZ) ,SOLD5(MVSIZ) ,SOLD6(MVSIZ),
     .Q(MVSIZ),SSPP(MVSIZ),SIGY(MVSIZ),
     .DPLA(MVSIZ),EPSP1(MVSIZ),TSTAR(MVSIZ),
     .DF(MVSIZ),AMU(MVSIZ)    
      my_real 
     .   E1,E2,E3,E4,E5,E6,BID1,BID2,BID3,DAV,DTA,SS1,SS2,SS3,SS4,
     .   SS5,SS6,Q1,Q2,Q3,ASRATE,EPSP,
     .   BID,DUM1,DUM2,DUM3,BIDON,BIDON1,BIDON2,BIDON3,BIDON4,BIDON5,BIDON6
      my_real TT_LOCAL     
      my_real,
     .  DIMENSION(:)  ,POINTER :: SIGP,SIGLP,STRAIN,UVAR,UVARF,DFMAX,TDELE,UPARAM
      INTEGER, DIMENSION(:), POINTER :: VARTMP
      TYPE(L_BUFEL_)  ,POINTER :: LBUF
      TYPE (FAIL_LOC_),POINTER :: FLOC
!
      CHARACTER OPTION*256
      INTEGER SIZE
      CHARACTER IUSER_KEY*ncharline
C=======================================================================
      MX = MAT(LFT)
      INLOC  = IPM(253,MX)
      NPAR   = IPM(9,MX)
      IADBUF = IPM(7,MX)
      NFUNC  = IPM(10,MX)
      NUVAR  = BUFLY%NVAR_MAT
      NVARTMP= BUFLY%NVARTMP
      UPARAM => BUFMAT(IADBUF:IADBUF+NPAR)
      DMG_FLAG = BUFLY%L_DMGSCL
C
      DO J=1,6
        JJ(J) = NEL*(J-1)
      ENDDO
C
      DO I=1,NFUNC
        IFUNC(I)=IPM(10+I,MX)
      ENDDO
      IF (ISMSTR>=10) ISMSTR=0
      BIDON = ZERO       
      BIDON1 = ZERO      
      BIDON2 = ZERO      
      BIDON3 = ZERO      
      BIDON4 = ZERO      
      BIDON5 = ZERO 
      CST1(LFT:LLT) = ONE
     
      DO I=LFT,LLT
        C1(I)  = PM(32,MX)
        RHO0(I)= PM( 1,MX)
        VIS(I) = ZERO
        SSP(I) = ZERO
        SV1(I) = ZERO
        SV2(I) = ZERO
        SV3(I) = ZERO
        SV4(I) = ZERO
        SV5(I) = ZERO
        SV6(I) = ZERO
        S1(I) = ZERO
        S2(I) = ZERO
        S3(I) = ZERO
        S4(I) = ZERO
        S5(I) = ZERO
        S6(I) = ZERO
        SIG(I,1)=ZERO
        SIG(I,2)=ZERO
        SIG(I,3)=ZERO
        SIG(I,4)=ZERO
        SIG(I,5)=ZERO
        SIG(I,6)=ZERO
        SSPP(I)=ZERO
      ENDDO
c
      IF (ISORTH /= 0) THEN
        CALL MREPLOC(
     1   GAMA,    R11,     R12,     R13,
     2   R21,     R22,     R23,     R31,
     3   R32,     R33,     RX,      RY,
     4   RZ,      SX,      SY,      SZ,
     5   TX,      TY,      TZ,      NEL,
     6   JSPH)
      ENDIF
C-------------------
C     MEAN STRAIN RATE
C-------------------
      IF (MTN == 35.OR.MTN == 36.OR.MTN == 44.OR.MTN == 48.OR.MTN == 60.OR.
     .    MTN == 52) THEN
        DO I=1,LLT
          EP1(I) = ZERO
          EP2(I) = ZERO
          EP3(I) = ZERO
          EP4(I) = ZERO
          EP5(I) = ZERO
          EP6(I) = ZERO  
        ENDDO
c
        DO IPT=1,NPT
          DO I=1,LLT
           EP1(I) = EP1(I) + D1(I,IPT)
           EP2(I) = EP2(I) + D2(I,IPT)
           EP3(I) = EP3(I) + D3(I,IPT)
           EP4(I) = EP4(I) + D4(I,IPT)
           EP5(I) = EP5(I) + D5(I,IPT)
           EP6(I) = EP6(I) + D6(I,IPT)
          ENDDO
        ENDDO
c
        ISRATE = IPM(3,MAT(LFT))
        IF(ISRATE>=1)THEN
           DO I=1,LLT
            DAV = (EP1(I)+EP2(I)+EP3(I))/TWENTY4
            E1 = EP1(I)*ONE_OVER_8 - DAV
            E2 = EP2(I)*ONE_OVER_8 - DAV
            E3 = EP3(I)*ONE_OVER_8 - DAV
            E4 = EP4(I)*ONE_OVER_16
            E5 = EP5(I)*ONE_OVER_16
            E6 = EP6(I)*ONE_OVER_16
            EPSP =HALF*(E1**2+E2**2+E3**2) +E4**2+E5**2+E6**2
            EPSP = SQRT(THREE*EPSP)*TWO_THIRD
            ASRATE = MIN(ONE, PM(9,MX)*DT1)
            EPSP = ASRATE*EPSP + (ONE -ASRATE)*EPSD(I)
            EPSD(I)=EPSP
          ENDDO
        ENDIF
      ENDIF
C--------------------------------------------------
C     BOUCLE SUR LES POINTS DE GAUSS
C--------------------------------------------------
      DO IPT=1,NPT
        LBUF   => BUFLY%LBUF(1,1,IPT)
        SIGP   => BUFLY%LBUF(1,1,IPT)%SIG(1:LLT*6)
        IF(ISORTH /= 0) SIGLP  => BUFLY%LBUF(1,1,IPT)%SIGL(1:LLT*6)
        STRAIN => BUFLY%LBUF(1,1,IPT)%STRA(1:LLT*6)
        UVAR   => BUFLY%MAT(1,1,IPT)%VAR(1:LLT*NUVAR)
        VARTMP => BUFLY%MAT(1,1,IPT)%VARTMP(1:LLT*NVARTMP)
        JPT=(IPT-1)*LLT
C
        DO I=LFT,LLT
          EP1(I) = D1(I,IPT)
          EP2(I) = D2(I,IPT)
          EP3(I) = D3(I,IPT)
          EP4(I) = D4(I,IPT)
          EP5(I) = D5(I,IPT)
          EP6(I) = D6(I,IPT)
          DAV=1.-DVOL(I)/VOLN(I)
          SOLD1(I)=SIGP(JJ(1)+I)*DAV
          SOLD2(I)=SIGP(JJ(2)+I)*DAV
          SOLD3(I)=SIGP(JJ(3)+I)*DAV
          SOLD4(I)=SIGP(JJ(4)+I)*DAV
          SOLD5(I)=SIGP(JJ(5)+I)*DAV
          SOLD6(I)=SIGP(JJ(6)+I)*DAV
        ENDDO
C
      IF (ISORTH /= 0) THEN
        DO I=LFT,LLT
          EP4(I) = HALF*EP4(I)
          EP5(I) = HALF*EP5(I)
          EP6(I) = HALF*EP6(I)
        ENDDO
        CALL MROTENS(LFT,LLT,EP1,EP2,EP3,EP4,EP5,EP6,
     .               R11,R12,R13,
     .               R21,R22,R23,
     .               R31,R32,R33)
        DO I=LFT,LLT
          J = (I-1)*6
          EP4(I) = TWO*EP4(I)
          EP5(I) = TWO*EP5(I)
          EP6(I) = TWO*EP6(I)
          SO1(I) = SIGLP(JJ(1)+I)
          SO2(I) = SIGLP(JJ(2)+I)
          SO3(I) = SIGLP(JJ(3)+I)
          SO4(I) = SIGLP(JJ(4)+I)
          SO5(I) = SIGLP(JJ(5)+I)
          SO6(I) = SIGLP(JJ(6)+I)
        ENDDO
      ELSE
        DO I=LFT,LLT
          SO1(I) = SIGP(JJ(1)+I)
          SO2(I) = SIGP(JJ(2)+I)
          SO3(I) = SIGP(JJ(3)+I)
          SO4(I) = SIGP(JJ(4)+I)
          SO5(I) = SIGP(JJ(5)+I)
          SO6(I) = SIGP(JJ(6)+I)
C
          Q1 =STRAIN(JJ(4)+I)*WZZ(I,IPT)
          Q2 =STRAIN(JJ(6)+I)*WYY(I,IPT)
          Q3 =STRAIN(JJ(5)+I)*WXX(I,IPT)
          SS1=STRAIN(JJ(1)+I)-Q1+Q2
          SS2=STRAIN(JJ(2)+I)+Q1-Q3
          SS3=STRAIN(JJ(3)+I)-Q2+Q3
          SS4=STRAIN(JJ(4)+I)+
     .      TWO*WZZ(I,IPT)*(STRAIN(JJ(1)+I)-STRAIN(JJ(2)+I))+
     .      WYY(I,IPT)*STRAIN(JJ(5)+I)-WXX(I,IPT)*STRAIN(JJ(6)+I)
          SS5=STRAIN(JJ(5)+I)+
     .      TWO*WXX(I,IPT)*(STRAIN(JJ(2)+I)-STRAIN(JJ(3)+I))+
     .      WZZ(I,IPT)*STRAIN(JJ(6)+I)-WYY(I,IPT)*STRAIN(JJ(4)+I)
          SS6=STRAIN(JJ(6)+I)+
     .     TWO*WYY(I,IPT)*(STRAIN(JJ(3)+I)-STRAIN(JJ(1)+I))+
     .      WXX(I,IPT)*STRAIN(JJ(4)+I)-WZZ(I,IPT)*STRAIN(JJ(5)+I)
          STRAIN(JJ(1)+I)= SS1
          STRAIN(JJ(2)+I)= SS2
          STRAIN(JJ(3)+I)= SS3
          STRAIN(JJ(4)+I)= SS4
          STRAIN(JJ(5)+I)= SS5
          STRAIN(JJ(6)+I)= SS6
C
        ENDDO
      ENDIF
C
      DO I=LFT,LLT
        DE1(I) = EP1(I)*DT1
        DE2(I) = EP2(I)*DT1
        DE3(I) = EP3(I)*DT1
        DE4(I) = EP4(I)*DT1
        DE5(I) = EP5(I)*DT1
        DE6(I) = EP6(I)*DT1
        STRAIN(JJ(1)+I)= STRAIN(JJ(1)+I) + DE1(I)
        STRAIN(JJ(2)+I)= STRAIN(JJ(2)+I) + DE2(I)
        STRAIN(JJ(3)+I)= STRAIN(JJ(3)+I) + DE3(I)
        STRAIN(JJ(4)+I)= STRAIN(JJ(4)+I) + DE4(I)
        STRAIN(JJ(5)+I)= STRAIN(JJ(5)+I) + DE5(I)
        STRAIN(JJ(6)+I)= STRAIN(JJ(6)+I) + DE6(I)
        ES1(I) = STRAIN(JJ(1)+I)
        ES2(I) = STRAIN(JJ(2)+I)
        ES3(I) = STRAIN(JJ(3)+I)
        ES4(I) = STRAIN(JJ(4)+I)
        ES5(I) = STRAIN(JJ(5)+I)
        ES6(I) = STRAIN(JJ(6)+I)	  
      ENDDO
C------Compute of AMU as in mmain after thermal expansion computation ---------------
      DO I=LFT,LLT
        DF(I)  =  RHO0(I)/RHO(I)
      ENDDO
      IF(MTN == 45) THEN     ! for compatibility with QA tests
        DO I=LFT,LLT
          AMU(I) =  ONE/DF(I)-ONE
        ENDDO 
      ELSE
        DO I=LFT,LLT
          AMU(I) =  RHO(I)/RHO0(I)-ONE
        ENDDO 
      ENDIF
      IEXPAN8 = 0
C
C--------------------------------------------------------
C     COMPUTE UNDAMAGED EFFECTIVE STRESSES
C---------------------------------------------------------
      IF (DMG_FLAG > 0) THEN 
        DO I = LFT,LLT
          SO1(I) = SO1(I)/MAX(LBUF%DMGSCL(I),EM20)
          SO2(I) = SO2(I)/MAX(LBUF%DMGSCL(I),EM20)
          SO3(I) = SO3(I)/MAX(LBUF%DMGSCL(I),EM20)
          SO4(I) = SO4(I)/MAX(LBUF%DMGSCL(I),EM20)
          SO5(I) = SO5(I)/MAX(LBUF%DMGSCL(I),EM20)
          SO6(I) = SO6(I)/MAX(LBUF%DMGSCL(I),EM20)
        ENDDO
      ENDIF
C---------------------------------------------------------------------------------
      IF(MTN == 28)THEN
        IDUM1 = 0
        CALL SIGEPS28(
     2    LLT      ,NPAR     ,NUVAR    ,NFUNC    ,IFUNC    ,
     3    NPF      ,TF       ,TT       ,DT1      ,BUFMAT,
     4    RHO0     ,RHO      ,VOLN     ,EINT     ,
     5    EP1      ,EP2      ,EP3      ,EP4      ,EP5      ,EP6   ,
     6    DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6   ,
     7    ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6   ,
     8    SO1      ,SO2      ,SO3      ,SO4      ,SO5      ,SO6   ,
     9    S1       ,S2       ,S3       ,S4       ,S5       ,S6    ,
     A    SV1      ,SV2      ,SV3      ,SV4      ,SV5      ,SV6   ,
     B    SSPP     ,VIS      ,UVAR     ,OFF      ,NGL      ,IPT   ,
     C    IPM      ,MAT      ,AMU      )
      ELSEIF(MTN == 29)THEN
!        CALL SIGEPS29(
!     2    LLT      ,NPAR     ,NUVAR    ,NFUNC    ,IFUNC    ,
!     3    NPF      ,TF       ,TT       ,DT1      ,BUFMAT(IADBUF),
!     4    RHO0     ,RHO      ,VOLN     ,EINT     ,
!     5    EP1      ,EP2      ,EP3      ,EP4      ,EP5      ,EP6   ,
!     6    DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6   ,
!     7    ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6   ,
!     8    SO1      ,SO2      ,SO3      ,SO4      ,SO5      ,SO6   ,
!     9    S1       ,S2       ,S3       ,S4       ,S5       ,S6    ,
!     A    SV1      ,SV2      ,SV3      ,SV4      ,SV5      ,SV6   ,
!     B    SSPP     ,VIS      ,UVAR     ,OFF      )
!!!
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/MAT/LAW29 - SOLID '
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
!!!
      ELSEIF(MTN == 30)THEN
!        CALL SIGEPS30(
!     2    LLT      ,NPAR     ,NUVAR    ,NFUNC    ,IFUNC    ,
!     3    NPF      ,TF       ,TT       ,DT1      ,BUFMAT(IADBUF),
!     4    RHO0     ,RHO      ,VOLN     ,EINT     ,
!     5    EP1      ,EP2      ,EP3      ,EP4      ,EP5      ,EP6   ,
!     6    DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6   ,
!     7    ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6   ,
!     8    SO1      ,SO2      ,SO3      ,SO4      ,SO5      ,SO6   ,
!     9    S1       ,S2       ,S3       ,S4       ,S5       ,S6    ,
!     A    SV1      ,SV2      ,SV3      ,SV4      ,SV5      ,SV6   ,
!     B    SSPP     ,VIS      ,UVAR     ,OFF      )
!!!
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/MAT/LAW30 - SOLID '
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
!!!
      ELSEIF(MTN == 31)THEN
!        CALL SIGEPS31(
!     2    LLT      ,NPAR     ,NUVAR    ,NFUNC    ,IFUNC    ,
!     3    NPF      ,TF       ,TT       ,DT1      ,BUFMAT(IADBUF),
!     4    RHO0     ,RHO      ,VOLN     ,EINT     ,
!     5    EP1      ,EP2      ,EP3      ,EP4      ,EP5      ,EP6   ,
!     6    DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6   ,
!     7    ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6   ,
!     8    SO1      ,SO2      ,SO3      ,SO4      ,SO5      ,SO6   ,
!     9    S1       ,S2       ,S3       ,S4       ,S5       ,S6    ,
!     A    SV1      ,SV2      ,SV3      ,SV4      ,SV5      ,SV6   ,
!     B    SSPP     ,VIS      ,UVAR     ,OFF      )
!!!
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/MAT/LAW31 - SOLID '
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
!!!
      ELSEIF(MTN == 33)THEN
        CALL SIGEPS33(
     2    LLT      ,NPAR     ,NUVAR    ,NFUNC    ,IFUNC    ,
     3    NPF      ,TF       ,TT       ,DT1      ,BUFMAT(IADBUF),
     4    RHO0     ,RHO      ,VOLN     ,EINT     ,
     5    EP1      ,EP2      ,EP3      ,EP4      ,EP5      ,EP6   ,
     6    DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6   ,
     7    ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6   ,
     8    SO1      ,SO2      ,SO3      ,SO4      ,SO5      ,SO6   ,
     9    S1       ,S2       ,S3       ,S4       ,S5       ,S6    ,
     A    SV1      ,SV2      ,SV3      ,SV4      ,SV5      ,SV6   ,
     B    SSPP     ,VIS      ,UVAR     ,OFF      )
      ELSEIF(MTN == 34)THEN
        CALL SIGEPS34(
     2    LLT      ,NPAR     ,NUVAR    ,NFUNC    ,IFUNC    ,
     3    NPF      ,TF       ,TT       ,DT1      ,BUFMAT(IADBUF),
     4    RHO0     ,RHO      ,VOLN     ,EINT     ,
     5    EP1      ,EP2      ,EP3      ,EP4      ,EP5      ,EP6   ,
     6    DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6   ,
     7    ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6   ,
     8    SO1      ,SO2      ,SO3      ,SO4      ,SO5      ,SO6   ,
     9    S1       ,S2       ,S3       ,S4       ,S5       ,S6    ,
     A    SV1      ,SV2      ,SV3      ,SV4      ,SV5      ,SV6   ,
     B    SSPP     ,VIS      ,UVAR     ,OFF      )
      ELSEIF(MTN == 35)THEN
        CALL SIGEPS35(
     2    LLT      ,NPAR     ,NUVAR    ,NFUNC    ,IFUNC    ,
     3    NPF      ,TF       ,TT       ,DT1      ,BUFMAT(IADBUF),
     4    RHO0     ,RHO      ,VOLN     ,EINT     ,
     5    EP1      ,EP2      ,EP3      ,EP4      ,EP5      ,EP6   ,
     6    DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6   ,
     7    ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6   ,
     8    SO1      ,SO2      ,SO3      ,SO4      ,SO5      ,SO6   ,
     9    S1       ,S2       ,S3       ,S4       ,S5       ,S6    ,
     A    SV1      ,SV2      ,SV3      ,SV4      ,SV5      ,SV6   ,
     B    SSPP     ,VIS      ,UVAR     ,OFF      ,ISRATE   ,ASRATE,
     C    EPSD     )
      ELSEIF(MTN == 36)THEN
C-------------------
C     STRAIN RATE
C-------------------
        DO I=1,LLT
          ISRATE = IPM(3,MAT(LFT))
          IF(ISRATE == 0)THEN
            DAV = (EP1(I)+EP2(I)+EP3(I))*THIRD
            E1 = EP1(I) - DAV
            E2 = EP2(I) - DAV
            E3 = EP3(I) - DAV
            E4 = HALF*EP4(I)
            E5 = HALF*EP5(I)
            E6 = HALF*EP6(I)
            EPSP =HALF*(E1**2+E2**2+E3**2) +E4**2+E5**2+E6**2
            EPSP = SQRT(THREE*EPSP)*TWO_THIRD
            EPSD(I)=EPSP
          ENDIF
        ENDDO
        CALL SIGEPS36(
     2    LLT      ,NPAR     ,NUVAR    ,NFUNC    ,IFUNC    ,
     3    NPF      ,TF       ,TT       ,DT1      ,UPARAM   ,
     4    RHO0     ,RHO      ,VOLN     ,EINT     ,
     5    DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6   ,
     6    ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6   ,
     7    SO1      ,SO2      ,SO3      ,SO4      ,SO5      ,SO6   ,
     8    S1       ,S2       ,S3       ,S4       ,S5       ,S6    ,
     9    SSPP     ,VIS      ,UVAR     ,OFF      ,NGL      ,IPT   ,
     A    IPM      ,MAT      ,EPSD     ,IPLA     ,SIGY     ,LBUF%PLA,
     B    DPLA     ,ET       ,BIDON    ,BIDON    ,AMU      ,CST1  ,
     C    NVARTMP  ,VARTMP   ,LBUF%SIGB,LBUF%DMG ,INLOC    ,LBUF%PLANL)
c
          DEFP(1:LLT)   =  LBUF%PLA(1:LLT)
c
      ELSEIF(MTN == 38)THEN
        CALL SIGEPS38(
     2    LLT      ,NPAR     ,NUVAR    ,NFUNC    ,IFUNC    ,
     3    NPF      ,TF       ,TT       ,DT1      ,BUFMAT(IADBUF),
     4    RHO0     ,RHO      ,VOLN     ,EINT     ,
     5    EP1      ,EP2      ,EP3      ,EP4      ,EP5      ,EP6   ,
     6    DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6   ,
     7    ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6   ,
     8    SO1      ,SO2      ,SO3      ,SO4      ,SO5      ,SO6   ,
     9    S1       ,S2       ,S3       ,S4       ,S5       ,S6    ,
     A    SV1      ,SV2      ,SV3      ,SV4      ,SV5      ,SV6   ,
     B    SSPP     ,VIS      ,UVAR     ,OFF      ,
     C    ISMSTR   ,BIDON    ,BIDON    ,BIDON    ,BIDON    ,
     D    BIDON    ,BIDON    ,BIDON    ,BIDON    ,BIDON    ,BIDON ,
     E    IHET     ,OFFG     ,EPSD     )   
      ELSEIF(MTN == 40)THEN
        CALL SIGEPS40(
     2    LLT      ,NPAR     ,NUVAR    ,NFUNC    ,IFUNC    ,
     3    NPF      ,TF       ,TT       ,DT1      ,BUFMAT(IADBUF),
     4    RHO0     ,RHO      ,VOLN     ,EINT     ,
     5    EP1      ,EP2      ,EP3      ,EP4      ,EP5      ,EP6   ,
     6    DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6   ,
     7    ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6   ,
     8    SO1      ,SO2      ,SO3      ,SO4      ,SO5      ,SO6   ,
     9    S1       ,S2       ,S3       ,S4       ,S5       ,S6    ,
     A    SV1      ,SV2      ,SV3      ,SV4      ,SV5      ,SV6   ,
     B    SSPP     ,VIS      ,UVAR     ,OFF      )
      ELSEIF(MTN == 41)THEN
        CALL SIGEPS41(
     2    LLT      ,NPAR     ,NUVAR    ,NFUNC    ,IFUNC    ,
     3    NPF      ,TF       ,TT       ,DT1      ,BUFMAT(IADBUF),
     4    RHO0     ,RHO      ,VOLN     ,EINT     ,
     5    EP1      ,EP2      ,EP3      ,EP4      ,EP5      ,EP6   ,
     6    DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6   ,
     7    ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6   ,
     8    SO1      ,SO2      ,SO3      ,SO4      ,SO5      ,SO6   ,
     9    S1       ,S2       ,S3       ,S4       ,S5       ,S6    ,
     A    SV1      ,SV2      ,SV3      ,SV4      ,SV5      ,SV6   ,
     B    SSPP     ,VIS      ,UVAR     ,OFF      ,DVOL     ,LBUF%BFRAC,
     .    LBUF%TEMP)
      ELSEIF(MTN == 42)THEN
        CALL SIGEPS42(
     1       LLT     ,NPAR    ,NUVAR   ,NFUNC   ,IFUNC   ,NPF     ,
     2       TF      ,TT      ,DT1     ,BUFMAT(IADBUF),RHO0,RHO   ,
     3       VOLN    ,EINT    ,UVAR    ,OFF     ,OFFG    ,SSPP    ,
     4       EP1     ,EP2     ,EP3     ,EP4     ,EP5     ,EP6     ,
     5       ES1     ,ES2     ,ES3     ,ES4     ,ES5     ,ES6     ,
     6       S1      ,S2      ,S3      ,S4      ,S5      ,S6      ,
     7       BIDON   ,BIDON   ,BIDON   ,BIDON   ,BIDON   ,BIDON   ,
     8       BIDON   ,BIDON   ,BIDON   ,VIS     ,ISMSTR  ,ET      ,
     9       IHET    ,BIDON   ,IEXPAN8 )
c
      ELSEIF(MTN == 44)THEN
C
C---  strain rate
C
!         DO I=1,LLT
!           ISRATE = IPM(3,MAT(LFT))
!           IF(ISRATE == 0)THEN
!             DAV = (EP1(I)+EP2(I)+EP3(I)) * THIRD
!             E1 = EP1(I) - DAV
!             E2 = EP2(I) - DAV
!             E3 = EP3(I) - DAV
!             E4 = HALF*EP4(I)
!             E5 = HALF*EP5(I)
!             E6 = HALF*EP6(I)
!             EPSP =HALF*(E1**2+E2**2+E3**2) +E4**2+E5**2+E6**2
!             EPSP = SQRT(THREE*EPSP)*TWO_THIRD
!             EPSD(I)=EPSP
!           ENDIF
!         ENDDO
        CALL SIGEPS44(
     2    LLT      ,NPAR     ,NUVAR    ,NFUNC    ,IFUNC    ,
     3    NPF      ,TF       ,TT       ,DT1      ,UPARAM   ,
     4    RHO0     ,RHO      ,VOLN     ,EINT     ,
     5    EP1      ,EP2      ,EP3      ,EP4      ,EP5      ,EP6   ,
     6    DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6   ,
     7    ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6   ,
     8    SO1      ,SO2      ,SO3      ,SO4      ,SO5      ,SO6   ,
     9    S1       ,S2       ,S3       ,S4       ,S5       ,S6    ,
     A    SV1      ,SV2      ,SV3      ,SV4      ,SV5      ,SV6   ,
     B    SSPP     ,VIS      ,UVAR     ,OFF      ,NGL      ,IPT   ,
     C    IPM      ,MAT      ,EPSD     ,IPLA     ,SIGY     ,LBUF%PLA  ,
     D    DPLA     ,AMU      ,ISRATE   ,ASRATE   ,NVARTMP  ,VARTMP)
        DEFP(1:LLT)   =  LBUF%PLA(1:LLT)
      ELSEIF(MTN == 45)THEN
        CALL SIGEPS45(
     2    LLT      ,NPAR     ,NUVAR    ,NFUNC    ,IFUNC    ,
     3    NPF      ,TF       ,TT       ,DT1      ,BUFMAT(IADBUF),
     4    RHO0     ,RHO      ,VOLN     ,EINT     ,
     5    EP1      ,EP2      ,EP3      ,EP4      ,EP5      ,EP6   ,
     6    DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6   ,
     7    ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6   ,
     8    SO1      ,SO2      ,SO3      ,SO4      ,SO5      ,SO6   ,
     9    S1       ,S2       ,S3       ,S4       ,S5       ,S6    ,
     A    SV1      ,SV2      ,SV3      ,SV4      ,SV5      ,SV6   ,
     B    SSPP     ,VIS      ,UVAR     ,OFF      ,SIGY     ,DEFP  ,
     C    AMU      )
      ELSEIF(MTN == 48)THEN
C---    strain rate
        DO I=1,LLT
          ISRATE = IPM(3,MAT(LFT))
          IF(ISRATE == 0)THEN
            DAV = (EP1(I)+EP2(I)+EP3(I)) * THIRD
            E1 = EP1(I) - DAV
            E2 = EP2(I) - DAV
            E3 = EP3(I) - DAV
            E4 = HALF*EP4(I)
            E5 = HALF*EP5(I)
            E6 = HALF*EP6(I)
            EPSP =HALF*(E1**2+E2**2+E3**2) +E4**2+E5**2+E6**2
            EPSP = SQRT(THREE*EPSP)*TWO_THIRD
            EPSD(I)=EPSP
          ENDIF
        ENDDO
        CALL SIGEPS48(
     2    LLT      ,NPAR     ,NUVAR    ,NFUNC    ,IFUNC    ,
     3    NPF      ,TF       ,TT       ,DT1      ,BUFMAT,
     4    RHO0     ,RHO      ,VOLN     ,EINT     ,
     5    EP1      ,EP2      ,EP3      ,EP4      ,EP5      ,EP6   ,
     6    DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6   ,
     7    ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6   ,
     8    SO1      ,SO2      ,SO3      ,SO4      ,SO5      ,SO6   ,
     9    S1       ,S2       ,S3       ,S4       ,S5       ,S6    ,
     A    SV1      ,SV2      ,SV3      ,SV4      ,SV5      ,SV6   ,
     B    SSPP     ,VIS      ,UVAR     ,OFF      ,NGL      ,IPT   ,
     C    IPM      ,MAT      ,EPSD     ,SIGY     ,DEFP     ,DPLA  ,
     D    AMU      )
      ELSEIF(MTN == 50)THEN
          CALL SIGEPS50(LLT ,NPAR,NUVAR,NFUNC,IFUNC,
     .                  NPF ,TF  ,TT,DT1,BUFMAT(IADBUF),
     .                  RHO0,RHO ,VOLN,EINT,
     .                  EP1 ,EP2 ,EP3 ,EP4  ,EP5  ,EP6 ,
     .                  DE1 ,DE2 ,DE3 ,DE4  ,DE5  ,DE6 ,
     .                  ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6 ,
     .                  SO1 ,SO2 ,SO3 ,SO4  ,SO5  ,SO6 ,
     .                  S1  ,S2  ,S3  ,S4   ,S5   ,S6  ,
     .                  SV1 ,SV2 ,SV3 ,SV4  ,SV5  ,SV6 ,
     .                  SSPP ,VIS ,UVAR,OFF ,AMU)
      ELSEIF(MTN == 52)THEN
C---    strain rate
        DO I=1,LLT
          ISRATE = IPM(3,MAT(LFT))
          IF(ISRATE == 0)THEN
            DAV = (EP1(I)+EP2(I)+EP3(I)) * THIRD
            E1 = EP1(I) - DAV
            E2 = EP2(I) - DAV
            E3 = EP3(I) - DAV
            E4 = HALF*EP4(I)
            E5 = HALF*EP5(I)
            E6 = HALF*EP6(I)
            EPSP =HALF*(E1**2+E2**2+E3**2) +E4**2+E5**2+E6**2
            EPSP = SQRT(THREE*EPSP)*TWO_THIRD
            EPSD(I)=EPSP
          ENDIF
        ENDDO
        CALL SIGEPS52(
     2    LLT      ,NPAR     ,NUVAR    ,NFUNC    ,IFUNC    ,
     3    NPF      ,TF       ,TT       ,DT1      ,BUFMAT,
     4    RHO0     ,RHO      ,VOLN     ,EINT     ,
     5    EP1      ,EP2      ,EP3      ,EP4      ,EP5      ,EP6   ,
     6    DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6   ,
     7    ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6   ,
     8    SO1      ,SO2      ,SO3      ,SO4      ,SO5      ,SO6   ,
     9    S1       ,S2       ,S3       ,S4       ,S5       ,S6    ,
     A    SV1      ,SV2      ,SV3      ,SV4      ,SV5      ,SV6   ,
     B    SSPP     ,VIS      ,UVAR     ,OFF      ,NGL      ,IPT   ,
     C    IPM      ,MAT      ,EPSD     ,IPLA     ,SIGY     ,DEFP  ,
     D    TABLE )     
      ELSEIF(MTN == 53)THEN
        CALL SIGEPS53(
     2    LLT      ,NPAR     ,NUVAR    ,NFUNC    ,IFUNC    ,
     3    NPF      ,TF       ,TT       ,DT1      ,BUFMAT,
     4    RHO0     ,RHO      ,VOLN     ,EINT     ,
     5    EP1      ,EP2      ,EP3      ,EP4      ,EP5      ,EP6   ,
     6    DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6   ,
     7    ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6   ,
     8    SO1      ,SO2      ,SO3      ,SO4      ,SO5      ,SO6   ,
     9    S1       ,S2       ,S3       ,S4       ,S5       ,S6    ,
     A    SV1      ,SV2      ,SV3      ,SV4      ,SV5      ,SV6   ,
     B    SSPP     ,VIS      ,UVAR     ,OFF      ,NGL      ,IPT   ,
     C    IPM      ,MAT      ,EPSP     ,IPLA     ,LBUF%SEQ )
      ELSEIF(MTN == 56)THEN
C-------------------
C     STRAIN RATE
C-------------------
        DO I=1,LLT
          ISRATE = IPM(3,MAT(LFT))
          IF(ISRATE == 0)THEN
            DAV = (EP1(I)+EP2(I)+EP3(I))*THIRD
            E1 = EP1(I) - DAV
            E2 = EP2(I) - DAV
            E3 = EP3(I) - DAV
            E4 = HALF*EP4(I)
            E5 = HALF*EP5(I)
            E6 = HALF*EP6(I)
            EPSP =HALF*(E1**2+E2**2+E3**2) +E4**2+E5**2+E6**2
            EPSP = SQRT(THREE*EPSP)*TWO_THIRD
            EPSD(I)=EPSP
          ENDIF
        ENDDO

        CALL SIGEPS56(
     2    LLT      ,NPAR     ,NUVAR    ,NFUNC    ,IFUNC    ,
     3    NPF      ,TF       ,TT       ,DT1      ,BUFMAT,
     4    RHO0     ,RHO      ,VOLN     ,EINT     ,
     5    EP1      ,EP2      ,EP3      ,EP4      ,EP5      ,EP6   ,
     6    DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6   ,
     7    ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6   ,
     8    SO1      ,SO2      ,SO3      ,SO4      ,SO5      ,SO6   ,
     9    S1       ,S2       ,S3       ,S4       ,S5       ,S6    ,
     A    SV1      ,SV2      ,SV3      ,SV4      ,SV5      ,SV6   ,
     B    SSPP     ,VIS      ,UVAR     ,OFF      ,NGL      ,IPT   ,
     C    IPM      ,MAT      ,EPSD     ,IPLA     ,SIGY     ,DEFP  ,
     D    DPLA     ,AMU      )     
      ELSEIF(MTN == 60)THEN
C-------------------
C     STRAIN RATE
C-------------------
        DO I=1,LLT
          ISRATE = IPM(3,MAT(LFT))
          IF(ISRATE == 0)THEN
            DAV = (EP1(I)+EP2(I)+EP3(I))*THIRD
            E1 = EP1(I) - DAV
            E2 = EP2(I) - DAV
            E3 = EP3(I) - DAV
            E4 = HALF*EP4(I)
            E5 = HALF*EP5(I)
            E6 = HALF*EP6(I)
            EPSP =HALF*(E1**2+E2**2+E3**2) +E4**2+E5**2+E6**2
            EPSP = SQRT(THREE*EPSP)*TWO_THIRD
            EPSD(I)=EPSP
          ENDIF
        ENDDO
        CALL SIGEPS60(
     2    LLT      ,NPAR     ,NUVAR    ,NFUNC    ,IFUNC    ,
     3    NPF      ,TF       ,TT       ,DT1      ,BUFMAT,
     4    RHO0     ,RHO      ,VOLN     ,EINT     ,
     5    EP1      ,EP2      ,EP3      ,EP4      ,EP5      ,EP6   ,
     6    DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6   ,
     7    ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6   ,
     8    SO1      ,SO2      ,SO3      ,SO4      ,SO5      ,SO6   ,
     9    S1       ,S2       ,S3       ,S4       ,S5       ,S6    ,
     A    SV1      ,SV2      ,SV3      ,SV4      ,SV5      ,SV6   ,
     B    SSPP     ,VIS      ,UVAR     ,OFF      ,NGL      ,IPT   ,
     C    IPM      ,MAT      ,EPSD     ,IPLA     ,SIGY     ,DEFP ,
     D    DPLA     ,AMU )
      ELSEIF(MTN == 62)THEN
        CALL SIGEPS62(
     2    LLT      ,NPAR     ,NUVAR    ,NFUNC    ,IFUNC    ,
     3    NPF      ,TF       ,TT       ,DT1      ,BUFMAT(IADBUF),
     4    RHO0     ,RHO      ,VOLN     ,EINT     ,
     5    EP1      ,EP2      ,EP3      ,EP4      ,EP5      ,EP6   ,
     6    DE1      ,DE2      ,DE3      ,DE4      ,DE5      ,DE6   ,
     7    ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6   ,
     8    SO1      ,SO2      ,SO3      ,SO4      ,SO5      ,SO6   ,
     9    S1       ,S2       ,S3       ,S4       ,S5       ,S6    ,
     A    SV1      ,SV2      ,SV3      ,SV4      ,SV5      ,SV6   ,
     B    SSPP     ,VIS      ,UVAR     ,OFF, ISMSTR,ET     ,IHET  ,
     C    OFFG     ,BIDON    ,IEXPAN8  )
      ENDIF
C-----------------------------------------------------------------
C                FAilure Model for Use law
C  ---------------------------------------------------------------
      IF ((ITASK==0).AND.(IMON_MAT==1))CALL STARTIME(121,1)
        DO IR = 1,BUFLY%NFAIL
          NUVARR = BUFLY%FAIL(1,1,IPT)%FLOC(IR)%NVAR
          UVARF => BUFLY%FAIL(1,1,IPT)%FLOC(IR)%VAR
          DFMAX => BUFLY%FAIL(1,1,IPT)%FLOC(IR)%DAMMX
          TDELE => BUFLY%FAIL(1,1,IPT)%FLOC(IR)%TDEL   
c          
          IP=(IR -1)*15
          IC = 1
          IFAIL(IC) = IPM(111 +IP ,MAT(1))
          MATF(IC) = MAT(1)
          IFLAG = IPM(111 +IP ,MAT(1))
          DO I=LFT +1 ,LLT
           IFAIL(IC) = IPM(111 +IP,MAT(I-1))
           MATF(IC) = MAT(I-1)
           IFLAG = IPM(111 +IP ,MAT(I-1))
           IF(IPM(111 +IP ,MAT(I))/=IPM(111 +IP,MAT(I-1)))THEN
             IC = IC+1
             MATF(IC)  = MAT(I)
             IFAIL(IC) = IPM(111 +IP,MAT(I))
             IFLAG = 1
           ENDIF
          ENDDO
C       
         IF(MTN == 36.OR.MTN == 44.OR.MTN == 48.OR.MTN == 56.
     .       OR.MTN == 60)THEN
           DO I=LFT,LLT
            TSTAR(I) = ZERO
            EPSP1(I) = EPSD(I)
           ENDDO
         ELSE
           DO I=LFT,LLT 
            TSTAR(I) = ZERO      
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
          NVARF_MAX = 0
          DO II = 1,IC
              NPAR   = IPM(112 +IP ,MATF(II))
              NFUNC  = IPM(115 +IP ,MATF(II))
              NVARF  = IPM(113 +IP ,MATF(II))
              NVARF_MAX = MAX(NVARF_MAX, NVARF)
              IADBUF = IPM(114 + IP ,MATF(II))
              DO I=1,NFUNC
               IFUNC(I) = IPM(115 + IP + I,MATF(II))
              ENDDO
cc             
             IF (IFAIL(IC) == 1)THEN        
C----  Johnson cook           
              CALL FAIL_JOHNSON(LLT ,NPAR,NVARF,NFUNC,IFUNC,
     2                     NPF ,TF  ,TT  ,DT1  ,BUFMAT(IADBUF),
     3                     NGL ,IPM ,MAT ,
     4                     S1  ,S2  ,S3  ,S4   ,S5   ,S6,
     5                     DPLA,EPSP1,TSTAR,UVARF     ,OFF,IP,
     6                     DFMAX,TDELE) 
             ELSEIF(IFAIL(II) == 2) THEN
C -----      Tuler Butcher        
              CALL FAIL_TBUTCHER_S(LLT ,NPAR,NVARF,NFUNC,IFUNC,
     2                     NPF ,TF  ,TT  ,DT1  ,BUFMAT(IADBUF),
     3                     NGL ,IPM ,MAT ,
     4                     S1  ,S2  ,S3  ,S4   ,S5   ,S6,
     5                     UVARF     ,OFF,IP   ,DFMAX,TDELE)       
             ELSEIF(IFAIL(II) == 3) THEN
C --- wilkins
               CALL FAIL_WILKINS_S(LLT ,NPAR,NVARF,NFUNC,IFUNC,
     2                     NPF ,TF  ,TT  ,DT1  ,BUFMAT(IADBUF),
     3                     NGL ,IPM ,MAT ,
     4                     S1  ,S2  ,S3  ,S4   ,S5   ,S6,
     5                     DPLA,UVARF,OFF,IP   ,DFMAX,TDELE)
             ELSEIF(IFAIL(II) == 4)THEN
C  ----user1    
               TT_LOCAL = TT    
!               CALL F04LAW(LLT ,NPAR,NUVARR,NFUNC,IFUNC,
!     2                    NPF ,TF  ,TT_LOCAL,DT1  ,BUFMAT,
!     3                    NGL ,IPM ,NPROPMI,MAT ,IDEL7NOK,
!     4                    EP1 ,EP2 ,EP3 ,EP4 ,EP5  ,EP6   ,
!     5                    ES1 ,ES2 ,ES3 ,ES4 ,ES5  ,ES6   ,
!     6                    S1  ,S2  ,S3  ,S4  ,S5   ,S6    ,
!     7                    DEFP,DPLA,EPSP1,UVARF     ,OFF,
!     8                    DELTAX,VOLN,BIDON3,BIDON4,BIDON5)
!!!
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/FAIL/USER1 - SOLID '
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
!!!
             ELSEIF(IFAIl(II) == 5)THEN
C  ----user2 
               TT_LOCAL = TT
!               CALL F05LAW(LLT ,NPAR,NUVARR,NFUNC,IFUNC,
!     2                    NPF ,TF  ,TT_LOCAL  ,DT1  ,BUFMAT,
!     3                    NGL ,IPM ,NPROPMI,MAT ,IDEL7NOK,
!     4                    EP1 ,EP2 ,EP3 ,EP4 ,EP5  ,EP6   ,
!     5                    ES1 ,ES2 ,ES3 ,ES4 ,ES5  ,ES6   ,
!     6                    S1  ,S2  ,S3  ,S4  ,S5   ,S6    ,
!     7                    DEFP,DPLA,EPSP1,UVARF     ,OFF,
!     8                    DELTAX,VOLN,BIDON3,BIDON4,BIDON5)
!!!
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/FAIL/USER2 - SOLID '
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
!!!
             ELSEIF(IFAIL(II) == 6)THEN
C  ----user3        
              TT_LOCAL = TT
!              CALL F06LAW(LLT ,NPAR,NUVARR,NFUNC,IFUNC,
!     2                    NPF ,TF  ,TT_LOCAL  ,DT1  ,BUFMAT,
!     3                    NGL ,IPM ,NPROPMI,MAT ,IDEL7NOK,
!     4                    EP1 ,EP2 ,EP3 ,EP4 ,EP5  ,EP6   ,
!     5                    ES1 ,ES2 ,ES3 ,ES4 ,ES5  ,ES6   ,
!     6                    S1  ,S2  ,S3  ,S4  ,S5   ,S6    ,
!     7                    DEFP,DPLA,EPSP1,UVARF     ,OFF,
!     8                    DELTAX,VOLN,BIDON3,BIDON4,BIDON5)
!!!
              ! ----------------
              ! ERROR to be printed & exit
                OPTION='/FAIL/USER3 - SOLID '
                SIZE=LEN_TRIM(OPTION)
                CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
                CALL ARRET(2)
              ! ----------------
!!!
             ELSEIF(IFAIL(IC) == 8)THEN        
C----  J     ohnson cook + spalling           
              CALL FAIL_SPALLING_S(LLT ,NPAR,NVARF,NFUNC,IFUNC,
     2                     NPF ,TF  ,TT  ,DT1  ,BUFMAT(IADBUF),
     3                     NGL ,IPM ,MAT ,
     4                     S1  ,S2  ,S3  ,S4   ,S5   ,S6      ,
     5                     DPLA,EPSP1,TSTAR,UVARF    ,OFF     ,
     6                     DFMAX,TDELE,OFFG) 
             ELSEIF(IFAIL(IC) == 9)THEN        
C----  wierzbicki           
              CALL FAIL_WIERZBICKI_S(LLT ,NPAR,NVARF,NFUNC,IFUNC,
     2                     NPF ,TF  ,TT  ,DT1  ,BUFMAT(IADBUF),
     3                     NGL ,IPM ,MAT ,
     4                     S1  ,S2  ,S3  ,S4   ,S5   ,S6      ,
     5                     DPLA,DEFP,UVARF,OFF ,DFMAX,
     6                     TDELE)          
              ELSEIF(IFAIL(II) == 10)THEN
C----strain tension             
               CALL FAIL_TENSSTRAIN_S(LLT ,NPAR,NVARF,NFUNC,IFUNC        ,
     2              NPF ,TF  ,TT  ,DT1  ,BUFMAT(IADBUF),
     3              NGL ,IPM ,MAT ,DELTAX  ,TSTAR,ISMSTR,
     4              ES1 ,ES2 ,ES3 ,ES4  ,ES5  ,ES6     ,
     5              S1  ,S2  ,S3  ,S4   ,S5   ,S6      ,
     6              EPSP1,UVARF   ,OFF  ,IP   ,DFMAX   ,TDELE,
     7              BIDON   ,BIDON   ,BIDON   ,BIDON    ,BIDON   ,BIDON   ,                 
     8              BIDON   ,BIDON   ,BIDON   ,LBUF%DMGSCL)
c
C----        energy failure          
              ELSEIF(IFAIL(II) == 11)THEN
               CALL FAIL_ENERGY_S(LLT ,NPAR,NVARF,NFUNC,IFUNC        ,
     2                     NPF ,TF  ,TT  ,DT1  ,BUFMAT(IADBUF),
     3                     NGL ,IPM ,MAT ,
     5                     S1  ,S2  ,S3  ,S4   ,S5   ,S6      ,
     6                     VOLN,EINT,EPSP1,UVARF     ,OFF,IP  ,
     7                     DFMAX,TDELE)
              ELSEIF (IFAIL(II) == 23) THEN   
C---- tabulated failure model
                CALL FAIL_TAB_S(
     1          LLT      ,NVARF    ,NPF      ,TF       ,TT       ,       
     2          BUFMAT(IADBUF)     ,NGL      ,IPM      ,MAT      ,DELTAX   ,       
     3          S1       ,S2       ,S3       ,S4       ,S5       ,S6,   
     4          DEFP     ,DPLA     ,EPSP1    ,TSTAR    ,UVARF    ,       
     5          OFF      ,IP       ,TABLE    ,DFMAX    ,TDELE    ,
     6          NFUNC    ,IFUNC )                                     
C---
              ELSEIF (IFAIL(II) == 27) THEN   
C----  extended mohr coulomb failure model
                CALL FAIL_EMC(
     1          LLT      ,NVARF    ,NPF      ,TF       ,TT       ,       
     2          DT1      ,BUFMAT(IADBUF)     ,NGL      ,IPM      ,MAT      ,       
     3          S1       ,S2       ,S3       ,S4       ,S5       ,S6,   
     4          DEFP     ,DPLA     ,EPSP1    ,UVARF    ,       
     5          OFF      ,IP       ,DFMAX    ,TDELE    )                                     
              ELSEIF (IFAIL(II) == 29) THEN   
c ---   MIT Wierzbicki Sahraei electric battery failure                                               
              CALL FAIL_SAHRAEI_S(
     1         LLT      ,NFUNC    ,IFUNC    ,NPF      ,TF       ,
     2         TT       ,NGL      ,IPM      ,MAT      ,BUFMAT(IADBUF),
     3         ES1      ,ES2      ,ES3      ,ES4      ,ES5      ,ES6      ,
     4         OFF      ,IP       ,DFMAX    ,TDELE    ,DELTAX   ,
     5         NVARF    ,UVARF    ) 
             ELSEIF (IFAIL(II) == 30) THEN                                      
C  --- Biquadratic failure model                                  
              CALL FAIL_BIQUAD_S(
     1         LLT      ,NPAR     ,NVARF    ,NFUNC    ,IFUNC    ,
     2         NPF      ,TF       ,TT       ,DT1      ,BUFMAT   ,
     3         NGL      ,IPM      ,MAT      ,                   
     4         S1       ,S2       ,S3       ,S4       ,S5       ,S6 ,
     5         DPLA     ,EPSP1    ,TSTAR    ,UVARF    ,OFF,IP   ,
     6         DFMAX    ,TDELE    ,DELTAX)                   
          ELSEIF (IFAIL(II) == 36) THEN                                           
C  --- VISUAL failure model             
              CALL FAIL_VISUAL_S(
     .        LLT     ,NPAR     ,NVARF    ,TT       ,DT1       ,BUFMAT(IADBUF),
     .        ES1     ,ES2      ,ES3      ,ES4      ,ES5       ,ES6 ,
     .        S1      ,S2       ,S3       ,S4       ,S5        ,S6  ,
     .        UVARF   ,OFF      ,NGL      ,DFMAX    ,ISMSTR    )
c
          ELSEIF (IFAIL(II) == 37) THEN                                      
c ---       tabulated failure model (old, obsolete version)
              CALL FAIL_TAB_OLD_S(
     1          LLT      ,NVARF    ,NPF      ,TF       ,TT       ,       
     2          BUFMAT(IADBUF)     ,NGL      ,IPM      ,MAT      ,DELTAX   ,       
     3          S1       ,S2       ,S3       ,S4       ,S5       ,S6,   
     4          DEFP     ,DPLA     ,EPSP1    ,TSTAR    ,UVARF    ,       
     5          OFF      ,IP       ,DFMAX    ,TDELE    ,
     6          NFUNC    ,IFUNC )    
c
          ELSEIF (IFAIL(II) == 38) THEN                                      
C  --- Orthotropic biquadratic failure model                                  
              CALL FAIL_ORTHBIQUAD_S(
     1          LLT      ,NPAR     ,NVARF    ,NFUNC    ,IFUNC    ,
     2          NPF      ,TF       ,TT       ,DT1      ,BUFMAT(IADBUF),
     3          NGL      ,DPLA     ,EPSP1    ,UVARF    ,OFF      ,                   
     4          S1       ,S2       ,S3       ,S4       ,S5       ,S6       ,
     5          DFMAX    ,TDELE    ,DELTAX   )  
c
          ELSEIF (IFAIL(II) == 40) THEN                                      
c  --- RTCL failure model                                  
              CALL FAIL_RTCL_S(
     1          LLT      ,NPAR     ,NVARF    ,TT       ,DT1      ,BUFMAT(IADBUF),
     2          S1       ,S2       ,S3       ,S4       ,S5       ,S6       ,
     3          NGL      ,DPLA     ,UVARF    ,OFF      ,DFMAX    ,TDELE    )
c---------
          ENDIF ! IRUPT
c---------
         ENDDO
        ENDDO ! several failur model boucle ir  
c---------
c--------------------------------------------------------
c     DAMAGED STRESSES
c---------------------------------------------------------
        IF (DMG_FLAG > 0) THEN 
          DO I = LFT,LLT
            S1(I) = S1(I)*LBUF%DMGSCL(I)
            S2(I) = S2(I)*LBUF%DMGSCL(I)
            S3(I) = S3(I)*LBUF%DMGSCL(I)
            S4(I) = S4(I)*LBUF%DMGSCL(I)
            S5(I) = S5(I)*LBUF%DMGSCL(I)
            S6(I) = S6(I)*LBUF%DMGSCL(I)
          ENDDO
        ENDIF
c---------------------------------------------------------
      IF ((ITASK==0).AND.(IMON_MAT==1))CALL STOPTIME(121,1)
c----------
        IF (ISORTH /= 0) THEN
          DO I=LFT,LLT
            SIGLP(JJ(1)+I) = S1(I)
            SIGLP(JJ(2)+I) = S2(I)
            SIGLP(JJ(3)+I) = S3(I)
            SIGLP(JJ(4)+I) = S4(I)
            SIGLP(JJ(5)+I) = S5(I)
            SIGLP(JJ(6)+I) = S6(I)
          ENDDO
          CALL MROTENS(LFT,LLT,
     .                S1 ,S2 ,S3 ,
     .                S4 ,S5 ,S6 ,
     .                R11,R21,R31,
     .                R12,R22,R32,
     .                R13,R23,R33)
          CALL MROTENS(LFT,LLT,
     .                SV1 ,SV2 ,SV3 ,
     .                SV4 ,SV5 ,SV6 ,
     .                R11,R21,R31,
     .                R12,R22,R32,
     .                R13,R23,R33)
        ENDIF
        DO I=LFT,LLT
          SIGP(JJ(1)+I) = S1(I)
          SIGP(JJ(2)+I) = S2(I)
          SIGP(JJ(3)+I) = S3(I)
          SIGP(JJ(4)+I) = S4(I)
          SIGP(JJ(5)+I) = S5(I)
          SIGP(JJ(6)+I) = S6(I)
          SIG(I,1)= SIG(I,1) + ONE_OVER_8*S1(I)
          SIG(I,2)= SIG(I,2) + ONE_OVER_8*S2(I)
          SIG(I,3)= SIG(I,3) + ONE_OVER_8*S3(I)
          SIG(I,4)= SIG(I,4) + ONE_OVER_8*S4(I)
          SIG(I,5)= SIG(I,5) + ONE_OVER_8*S5(I)
          SIG(I,6)= SIG(I,6) + ONE_OVER_8*S6(I)
          SVIS(I,1)= SVIS(I,1) + ONE_OVER_8*SV1(I)
          SVIS(I,2)= SVIS(I,2) + ONE_OVER_8*SV2(I)
          SVIS(I,3)= SVIS(I,3) + ONE_OVER_8*SV3(I)
          SVIS(I,4)= SVIS(I,4) + ONE_OVER_8*SV4(I)
          SVIS(I,5)= SVIS(I,5) + ONE_OVER_8*SV5(I)
          SVIS(I,6)= SVIS(I,6) + ONE_OVER_8*SV6(I)
          SSP(I) = MAX(SSP(I),SSPP(I))
        ENDDO

c        DO J=1,NUVAR
c          DO I=LFT,LLT
c            UVARG(I,J)= UVARG(I,J) + ONE_OVER_8*UVAR(I,J,IPT)
c          ENDDO
c        ENDDO
c
        DTA =HALF*DT1
        DO I=LFT,LLT
          DAV=VOLGP(I,IPT)*OFF(I)*DTA
          EINT(I)=EINT(I)+DAV*(D1(I,IPT)*(SOLD1(I)+SIGP(JJ(1)+I))+
     +                         D2(I,IPT)*(SOLD2(I)+SIGP(JJ(2)+I))+
     +                         D3(I,IPT)*(SOLD3(I)+SIGP(JJ(3)+I))+
     +                         D4(I,IPT)*(SOLD4(I)+SIGP(JJ(4)+I))+
     +                         D5(I,IPT)*(SOLD5(I)+SIGP(JJ(5)+I))+
     +                         D6(I,IPT)*(SOLD6(I)+SIGP(JJ(6)+I)))
        ENDDO
C
      ENDDO  !  IPT=1,NPT
C--------------------------------------------------
C     EGALISATION DE LA PRESSION
C--------------------------------------------------
      DO I=LFT,LLT
        PNEW(I) = -(SIG(I,1) + SIG(I,2) + SIG(I,3)) * THIRD
      ENDDO
c----
      DO IPT=1,NPT
        SIGP => BUFLY%LBUF(1,1,IPT)%SIG(1:LLT*6)
        DO I=LFT,LLT
          PP(I)=PNEW(I) + (SIGP(JJ(1)+I) + SIGP(JJ(2)+I) + SIGP(JJ(3)+I)) * THIRD
          SIGP(JJ(1)+I) =(SIGP(JJ(1)+I)-PP(I))*OFF(I)
          SIGP(JJ(2)+I) =(SIGP(JJ(2)+I)-PP(I))*OFF(I)
          SIGP(JJ(3)+I) =(SIGP(JJ(3)+I)-PP(I))*OFF(I)
          SIGP(JJ(4)+I) = SIGP(JJ(4)+I)       *OFF(I)
          SIGP(JJ(5)+I) = SIGP(JJ(5)+I)       *OFF(I)
          SIGP(JJ(6)+I) = SIGP(JJ(6)+I)       *OFF(I)
          DAV = VOLGP(I,IPT)*OFF(I)*DTA
          EINT(I)=EINT(I)-DAV*PP(I)*(D1(I,IPT)+D2(I,IPT)+D3(I,IPT))
        ENDDO
      ENDDO
C------------------------------------------------------------
C     DEFINE SOUND SPEED  (IN ALL CASE)
C     DEFINE DYNAMIC VISCOSITY (FOR VISCOUS LAW)
C-----------------------
      DO I=LFT,LLT
        IF(SSP(I) == ZERO) SSP(I)=SQRT(C1(I)/RHO0(I))
      ENDDO
C-------------------------------------------
C   BULK VISCOSITY AND TIME STEP COMPUTATION
C   THIS SUBROUTINE RETURN THE NEW BULK VISCOSITY Q
C-----------------------
      CALL MQVISC8(
     1   PM,      OFF,     RHO,     VIS,
     2   VIS,     VIS,     STIFN,   EINT,
     3   D1,      D2,      D3,      VOLN,
     4   DVOL,    VD2,     DELTAX,  VIS,
     5   QOLD,    SSP,     MAT,     NC,
     6   NGL,     GEO,     PID,     DT2T,
     7   NELTST,  ITYPTST, OFFG,    MSSA,
     8   DMELS,   NEL,     ITY,     JTUR,
     9   JTHE,    JSMS)
C
      DO I=LFT,LLT
        EINT(I)=EINT(I)/MAX(EM15,VOL(I))
      ENDDO
C------------------------------------------
      RETURN
      END
