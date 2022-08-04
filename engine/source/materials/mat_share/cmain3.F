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
Chd|  CMAIN3                        source/materials/mat_share/cmain3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|        CDK6FORC3                     source/elements/sh3n/coquedk6/cdk6forc3.F
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|-- calls ---------------
Chd|        DELAMINATION                  source/properties/composite_options/stack/delamination.F
Chd|        LAYINI                        source/elements/shell/coque/layini.F
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        MULAWGLC                      source/materials/mat_share/mulawglc.F
Chd|        PUT_ETFAC                     source/elements/solid/solide8z/mmats.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        THERMEXPC                     source/materials/mat_share/thermexpc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE CMAIN3 (
     1           ELBUF_STR ,JFT       ,JLT       ,NFT       ,IPARG      ,
     2           NEL       ,MTN       ,IPLA      ,ITHK      ,GROUP_PARAM,
     3           PM        ,GEO       ,NPF       ,TF        ,BUFMAT     ,
     4           SSP       ,RHO       ,VISCMX    ,DT1C      ,SIGY       ,
     5           AREA      ,EXX       ,EYY       ,EXY       ,EXZ        ,
     6           EYZ       ,KXX       ,KYY       ,KXY       ,NU         ,
     7           OFF       ,THK0      ,MAT       ,PID       ,MATPARAM_TAB,
     8           FOR       ,MOM       ,GSTR      ,FAILWAVE  ,FWAVE_EL   ,
     9           THK       ,EINT      ,IOFC      ,
     A           G         ,A11       ,A12       ,VOL0      ,INDXDEL    ,
     B           NGL       ,ZCFAC     ,SHF       ,GS        ,EPSP       ,
     C           KFTS      ,JHBE      ,ALPE      ,
     D           DIR_A     ,DIR_B     ,IGEO      ,
     E           IPM       ,IFAILURE  ,NPG       ,
     F           TEMPEL    ,DIE       ,JTHE      ,IEXPAN    ,TEMPEL0    ,
     G           ISHPLYXFEM,PLY_EXX   ,
     H           PLY_EYY   ,PLY_EXY   ,PLY_EXZ   ,PLY_EYZ   ,PLY_F      ,
     I           DEL_PLY   ,TH_IPLY   ,SIG_IPLY  ,E1X       ,E1Y        ,
     J           E1Z       ,E2X       ,E2Y       ,E2Z       ,E3X        ,
     K           E3Y       ,E3Z       ,NG        ,TABLE     ,IXFEM      ,
     L           OFFI      ,LSENSOR   ,SENSOR    ,A11_IPLY  ,ELCRKINI   ,
     M           DIR1_CRK  ,DIR2_CRK  ,ALDT      ,
     N           ISMSTR    ,IR        ,IS        ,NLAY      ,NPT        ,
     O           IXLAY     ,IXEL      ,ISUBSTACK ,STACK     ,
     P           F_DEF     ,ITASK     ,DRAPE     ,VARNL     ,NLOC_DMG   )
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE TABLE_MOD
      USE ELBUFDEF_MOD
      USE STACK_MOD
      USE FAILWAVE_MOD
      USE MATPARAM_DEF_MOD
      USE DRAPE_MOD 
      USE NLOCAL_REG_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "impl1_c.inc"
#include      "sms_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NFT,IR,IS,NPT,NG,NPG,NLAY,NEL,MTN,IPLA,IOFC,ITHK,
     .   KFTS,JHBE,IFAILURE,JTHE,IEXPAN,ISHPLYXFEM,ISMSTR,
     .   IXLAY,IXEL,IXFEM,ISUBSTACK,ITASK
      INTEGER MAT(MVSIZ), PID(MVSIZ), INDXDEL(MVSIZ), NGL(MVSIZ),NPF(*), 
     .   IGEO(NPROPGI,*),IPM(NPROPMI,*),IPARG(*),LSENSOR(NSENSI,*),
     .   A11_IPLY(MVSIZ,*),ELCRKINI(*),FWAVE_EL(NEL)
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*),TF(*),THK(*),EINT(JLT,*),FOR(NEL,5), 
     .   MOM(NEL,3),EPSP(*),GSTR(NEL,8),BUFMAT(*),G(*),A11(*),A12(*),VOL0(*),
     .   SHF(*),GS(*),SIGY(MVSIZ),RHO(MVSIZ),SSP(MVSIZ),VISCMX(MVSIZ),
     .   OFF(MVSIZ) ,THK0(MVSIZ) ,NU(MVSIZ),DT1C(MVSIZ),
     .   EXX(MVSIZ), EYY(MVSIZ), EXY(MVSIZ), EXZ(MVSIZ), EYZ(MVSIZ),
     .   KXX(MVSIZ), KYY(MVSIZ), KXY(MVSIZ), AREA(MVSIZ),
     .   ZCFAC(MVSIZ,2),ALPE(MVSIZ),DIR_A(*),DIR_B(*),TEMPEL(*),DIE(*),
     .   TEMPEL0(MVSIZ), PLY_F(MVSIZ,5,*),
     .   PLY_EXX(MVSIZ,*),PLY_EYY(MVSIZ,*),PLY_EXY(MVSIZ,*), 
     .   PLY_EXZ(MVSIZ,*),PLY_EYZ(MVSIZ,*),DEL_PLY(MVSIZ,3,*) ,
     .   TH_IPLY(MVSIZ,*), SIG_IPLY(MVSIZ,3,*),SENSOR(NSENSR,*),
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .   E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .   E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),OFFI(MVSIZ,*),
     .   DIR1_CRK(*),DIR2_CRK(*),DMG_SCALE(MVSIZ),
     .   F_DEF(MVSIZ,*),ALDT(MVSIZ),VARNL(NEL,*)
      TYPE (TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (STACK_PLY) :: STACK
      TYPE (FAILWAVE_STR_) ,TARGET :: FAILWAVE 
      TYPE (GRP_PARAM_STRUCT_) :: GROUP_PARAM
      TYPE (DRAPE_) :: DRAPE
      TYPE (MATPARAM_STRUCT_) , DIMENSION(NUMMAT) :: MATPARAM_TAB
      TYPE (NLOCAL_STR_) :: NLOC_DMG 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IT,MX,IGTYP,IFUNC_ALPHA,IPMAT_IPLY,ILAY,LAYNPT_MAX,LAY_MAX
      PARAMETER (LAYNPT_MAX = 10)
      PARAMETER (LAY_MAX = 100)
      INTEGER MATLY(MVSIZ*LAY_MAX),MAT_IPLY(MVSIZ,NPT)
      my_real
     .   THKLY(MVSIZ*LAY_MAX*LAYNPT_MAX),POSLY(MVSIZ,LAY_MAX*LAYNPT_MAX),
     .   THK_LY(NEL,LAY_MAX*LAYNPT_MAX),TSTAR(MVSIZ),DTEMP(MVSIZ),
     .   SXX(MVSIZ),SYY(MVSIZ), SXY(MVSIZ),SYZ(MVSIZ),SZX(MVSIZ),
     .   EPSXX(MVSIZ),EPSYY(MVSIZ),EPSXY(MVSIZ),EPSYZ(MVSIZ),EPSZX(MVSIZ),
     .   DEPSXX(MVSIZ),DEPSYY(MVSIZ),DEPSXY(MVSIZ),DEPSYZ(MVSIZ),
     .   DEPSZX(MVSIZ), EPSPXX(MVSIZ),EPSPYY(MVSIZ),EPSPXY(MVSIZ),
     .   EPSPYZ(MVSIZ), EPSPZX(MVSIZ),ETIMP(MVSIZ),TENS(NEL,5),ETH(MVSIZ)
      my_real ALPHA,DF,DEINTTH,T0,TM,FSCAL_ALPHA,DM
      INTEGER, DIMENSION(:) ,POINTER :: FLD_IDX
C-------------------------------------
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(BUF_FAIL_),POINTER :: FBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
C-----------------------------------------------
       my_real FINTER 
      EXTERNAL FINTER
C=======================================================================
      GBUF => ELBUF_STR%GBUF
      IGTYP=IGEO(11,PID(1))
      MX = MAT(JFT) 
      DM = GROUP_PARAM%VISC_DM   ! membrane damping
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
c-----Tstar computation for Jhonson cook failure : T* = (T-T0)/(TM-T0)----
      IF (JTHE > 0) THEN
        T0 = PM(79, MX) 
        TM = PM(80, MX) 
        DO I=JFT,JLT   
           TSTAR(I)=MAX(ZERO,(TEMPEL(I)-T0)/(TM-T0))
        ENDDO
      ELSE
        TSTAR(JFT:JLT) = ZERO
      ENDIF     
c
c----------------------------
      IF (NPT == 0) THEN
          ! Radioss laws 1,2,22,36,43,56,60,86 with global integration
c----------------------------
        ILAY = 1
        IT   = 1
        LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IR,IS,IT)
c
        CALL MULAWGLC(ELBUF_STR,
     1       JFT     ,JLT     ,PM      ,FOR     ,MOM     ,THK     ,
     2       EINT    ,OFF     ,GSTR    ,DIR_A   ,SHF     ,
     3       MAT     ,AREA    ,EXX     ,EYY     ,EXY     ,NEL     ,
     4       EXZ     ,EYZ     ,KXX     ,KYY     ,KXY     ,DM      ,
     5       PID     ,TF      ,NPF     ,MTN     ,DT1C    ,A11     ,
     6       BUFMAT  ,SSP     ,RHO     ,VISCMX  ,IOFC    ,A12     ,
     7       INDXDEL ,NGL     ,ZCFAC   ,GS      ,SIGY    ,G       ,
     8       THK0    ,EPSP    ,IPLA    ,IGEO    ,IPM     ,TABLE   ,
     9       IR      ,IS      ,F_DEF   ,ISMSTR  ,NU      ,VOL0    ,
     A       KFTS    ) 
c----------------------------
      ELSE IF (MTN > 28 .AND. MTN < 32 .or. MTN == 99 .or. MTN == 200) THEN
c---    user material law libraries here
c
        CALL LAYINI(ELBUF_STR,JFT      ,JLT      ,GEO      ,IGEO     ,
     .              MAT      ,PID      ,THKLY    ,MATLY    ,POSLY    , 
     .              IGTYP    ,IXFEM    ,IXLAY    ,NLAY     ,NPT      ,   
     .              ISUBSTACK,STACK    ,DRAPE    ,NFT      ,THK      ,
     .              NEL      ,THK_LY   )
c
        CALL USERMAT_SHELL (ELBUF_STR ,
     1       JFT     ,JLT     ,NEL     ,PM      ,FOR     ,MOM     ,
     2       GSTR    ,THK     ,EINT    ,OFF     ,DIR_A   ,DIR_B   ,
     3       MAT     ,AREA    ,EXX     ,EYY     ,EXY     ,EXZ     ,
     4       EYZ     ,KXX     ,KYY     ,KXY     ,GEO     ,THK_LY  ,
     5       PID     ,TF      ,NPF     ,MTN     ,DT1C    ,DM      ,
     6       BUFMAT  ,SSP     ,RHO     ,VISCMX  ,IPLA    ,IOFC    ,
     7       INDXDEL ,NGL     ,THKLY   ,MATLY   ,ZCFAC   ,
     8       SHF     ,GS      ,SIGY    ,THK0    ,EPSP    ,
     9       POSLY   ,IGEO    ,IPM     ,FAILWAVE,FWAVE_EL,
     A       IFAILURE,ALDT    ,TEMPEL  ,DIE     ,
     B       E1X     ,E1Y     ,E1Z     ,E2X     ,E2Y     ,E2Z     ,
     C       E3X     ,E3Y     ,E3Z     ,TABLE   ,IXFEM   ,ELCRKINI,
     D       LSENSOR ,SENSOR  ,NG      ,
     E       DIR1_CRK,DIR2_CRK,IPARG   ,JHBE    ,ISMSTR  ,JTHE    ,
     F       TENS    ,IR      ,IS      ,NLAY    ,NPT     ,IXLAY   ,
     G       IXEL    ,ITHK    ,F_DEF   ,ISHPLYXFEM,
     H       ITASK   ,STACK%PM ,ISUBSTACK,STACK ,TSTAR   ,ALPE    ,
     I       PLY_EXX ,PLY_EYY ,PLY_EXY ,PLY_EXZ ,PLY_EYZ ,PLY_F   ,
     J       VARNL   ,NLOC_DMG)
cc----------------------------
      ELSE   ! User-type Radioss laws , NPT > 0
c----------------------------
!
!       position, thickness and material in integration points
!
        CALL LAYINI(ELBUF_STR,JFT      ,JLT      ,GEO      ,IGEO     ,
     .              MAT      ,PID      ,THKLY    ,MATLY    ,POSLY    , 
     .              IGTYP    ,IXFEM    ,IXLAY    ,NLAY     ,NPT      ,   
     .              ISUBSTACK,STACK    ,DRAPE    ,NFT     ,THK       ,
     .              NEL      ,THK_LY   )
c 
        CALL MULAWC(ELBUF_STR ,
     1       JFT     ,JLT     ,NEL     ,PM      ,FOR     ,MOM     ,
     2       GSTR    ,THK     ,EINT    ,OFF     ,DIR_A   ,DIR_B   ,
     3       MAT     ,AREA    ,EXX     ,EYY     ,EXY     ,EXZ     ,
     4       EYZ     ,KXX     ,KYY     ,KXY     ,GEO     ,THK_LY  ,
     5       PID     ,TF      ,NPF     ,MTN     ,DT1C    ,DM      ,
     6       BUFMAT  ,SSP     ,RHO     ,VISCMX  ,IPLA    ,IOFC    ,
     7       INDXDEL ,NGL     ,THKLY   ,MATLY   ,ZCFAC   ,MATPARAM_TAB,
     8       SHF     ,GS      ,SIGY    ,THK0    ,EPSP    ,
     9       POSLY   ,IGEO    ,IPM     ,FAILWAVE,FWAVE_EL,
     A       IFAILURE,ALDT    ,TEMPEL  ,DIE     ,
     B       E1X     ,E1Y     ,E1Z     ,E2X     ,E2Y     ,E2Z     ,
     C       E3X     ,E3Y     ,E3Z     ,TABLE   ,IXFEM   ,ELCRKINI,
     D       LSENSOR ,SENSOR  ,NG      ,
     E       DIR1_CRK,DIR2_CRK,IPARG   ,JHBE    ,ISMSTR  ,JTHE    ,
     F       TENS    ,IR      ,IS      ,NLAY    ,NPT     ,IXLAY   ,
     G       IXEL    ,ITHK    ,F_DEF   ,ISHPLYXFEM,
     H       ITASK   ,STACK%PM ,ISUBSTACK,STACK ,TSTAR   ,ALPE    ,
     I       PLY_EXX ,PLY_EYY ,PLY_EXY ,PLY_EXZ ,PLY_EYZ ,PLY_F   ,
     J       VARNL   ,ETIMP   ,NLOC_DMG)
      ENDIF ! IF (NPT == 0) 
C----------------------------------------------------------------------
      IF (IEXPAN > 0 .AND. JTHE > 0. AND.IDT_THERM==0) THEN
         CALL THERMEXPC(ELBUF_STR,
     1                  JFT  ,JLT   ,GBUF%FORTH  ,FOR   ,EINT  , 
     2                  OFF  ,ETH   ,THK0        ,EXX   ,EYY   ,    
     3                  PM   ,NPT   ,AREA        ,A11   ,A12   ,
     4                  MAT  ,MTN   ,GBUF%EINTTH ,DIR_A ,IR    ,
     5                  IS   ,NLAY  ,THK         ,NEL   ,IGTYP ,
     6                  NPF  , TF   , IPM        , TEMPEL ,DTEMP,
     7                  THKLY ,POSLY,MOM, MATLY )    
      ENDIF

       IF (JTHE > 0 .AND. (MTN < 28 .OR. MTN == 32)) THEN
         DO I=JFT,JLT          
           DIE(I) = (EINT(I,1) + EINT(I,2) - DIE(I)) * PM(90,MAT(1))          
         ENDDO  
       ENDIF  
C-------------------------------------------------------------------
      IF ((ITASK==0).AND.(IMON_MAT==1))CALL STARTIME(121,1)
C----------------------------------------------------------------------
c     old failure calls cleaned - now obsolete
C-----------------------------------------------
      IF ((ITASK==0).AND.(IMON_MAT==1))CALL STOPTIME(121,1)
C-----------------------------------------------
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
       CALL DELAMINATION(ELBUF_STR,
     .                   JFT      ,JLT      ,IR      ,IS      ,NPT     ,
     .                   MAT_IPLY ,IPM      ,PM      ,BUFMAT  ,NPF     ,
     .                   TF       ,DT1C     ,NGL     ,OFF     ,TH_IPLY ,
     .                   DEL_PLY  ,SIG_IPLY ,OFFI    ,A11_IPLY,FOR     ,
     .                   MOM      ,PLY_F    ,THK0    ,SHF     ,EPSZX   ,
     .                   EPSYZ    ,AREA     ,PID     ,GEO     ,SSP     ,
     .                   POSLY    ,THKLY    ,KXX     ,KYY     ,KXY     ,
     .                   EXZ      ,EYZ      ,EINT    ,GSTR    ,NEL     ) 
     
      ENDIF ! PLYXFEM 
C---------------------------------------------------------             
      IF (IMPL_S > 0) THEN
        IF (MTN /=78)  ETIMP(JFT:JLT) = ZCFAC(JFT:JLT,1)
        CALL PUT_ETFAC(NEL ,ETIMP ,MTN)
      END IF
c-----------
      RETURN
      END SUBROUTINE CMAIN3
C
