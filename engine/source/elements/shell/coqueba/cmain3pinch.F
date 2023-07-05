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
Chd|  CMAIN3PINCH                   source/elements/shell/coqueba/cmain3pinch.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|        MULAWGLCPINCH                 source/elements/shell/coqueba/mulawglcpinch.F
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|        GROUP_PARAM_MOD               ../common_source/modules/mat_elem/group_param_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        PINCH_LOCAL_MOD               share/modules/pinch_local_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE CMAIN3PINCH (
     1           ELBUF_STR ,JFT       ,JLT       ,NFT       ,IPARG      ,
     2           NEL       ,MTN       ,IPLA      ,ITHK      ,GROUP_PARAM,
     3           PM        ,GEO       ,NPF       ,TF        ,BUFMAT     ,
     4           SSP       ,RHO       ,VISCMX    ,DT1C      ,SIGY       ,
     5           AREA      ,EXX       ,EYY       ,EXY       ,EXZ        ,
     6           EYZ       ,KXX       ,KYY       ,KXY       ,NU         ,
     7           OFF       ,THK0      ,MAT       ,PID       ,
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
     L           OFFI      ,A11_IPLY  ,ELCRKINI   ,
     M           DIR1_CRK  ,DIR2_CRK  ,ALDT      ,
     N           ISMSTR    ,IR        ,IS        ,NLAY      ,NPT       ,
     O           IXLAY     ,IXEL      ,ISUBSTACK ,STACK     ,
     P           F_DEF     ,ITASK     ,DRAPE     ,VARNL     ,
     Q           PINCH_LOCAL,FORP     ,MOMP      ,EZZAVG    ,
     R           AREAPINCH)  
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE TABLE_MOD
      USE ELBUFDEF_MOD
      USE STACK_MOD
      USE FAILWAVE_MOD
      USE GROUP_PARAM_MOD 
      USE PINCHTYPE_MOD
      USE PINCH_LOCAL_MOD
      USE DRAPE_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NFT,IR,IS,NPT,NG,NPG,NLAY,NEL,MTN,IPLA,IOFC,ITHK,
     .   KFTS,JHBE,IFAILURE,JTHE,IEXPAN,ISHPLYXFEM,ISMSTR,
     .   IXLAY,IXEL,IXFEM,ISUBSTACK,ITASK
      INTEGER MAT(MVSIZ), PID(MVSIZ), INDXDEL(MVSIZ), NGL(MVSIZ),NPF(*), 
     .   IGEO(NPROPGI,*),IPM(NPROPMI,*),IPARG(*),
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
     .   TH_IPLY(MVSIZ,*), SIG_IPLY(MVSIZ,3,*),
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .   E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .   E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),OFFI(MVSIZ,*),
     .   DIR1_CRK(*),DIR2_CRK(*),DMG_SCALE(MVSIZ),
     .   F_DEF(MVSIZ,*),ALDT(MVSIZ),VARNL(NEL,*),
     .   FORP(MVSIZ),MOMP(MVSIZ,2),EZZAVG(MVSIZ),AREAPINCH(MVSIZ)
      TYPE(TTABLE) TABLE(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (STACK_PLY) :: STACK
      TYPE (FAILWAVE_STR_) ,TARGET :: FAILWAVE 
      TYPE (GROUP_PARAM_) :: GROUP_PARAM
      TYPE (PINCH_LOCAL_STRUCT_) :: PINCH_LOCAL
      TYPE (DRAPE_) :: DRAPE
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
      DIE(JFT:JLT)   = ZERO

      ILAY = 1
      IT = 1
      LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IR,IS,IT)
C
      IF(MTN == 1 .OR. MTN == 91) THEN
        CALL MULAWGLCPINCH(ELBUF_STR,
     1         JFT     ,JLT     ,PM      ,FOR     ,MOM     ,THK     ,
     2         EINT    ,OFF     ,GSTR    ,LBUF%PLA,DIR_A   ,SHF     ,
     3         MAT     ,AREA    ,EXX     ,EYY     ,EXY     ,NEL     ,
     4         EXZ     ,EYZ     ,KXX     ,KYY     ,KXY     ,DM      ,
     5         PID     ,TF      ,NPF     ,MTN     ,DT1C    ,A11     ,
     6         BUFMAT  ,SSP     ,RHO     ,VISCMX  ,IOFC    ,A12     ,
     7         INDXDEL ,NGL     ,ZCFAC   ,GS      ,SIGY    ,G       ,
     8         THK0    ,EPSP    ,IPLA    ,IGEO    ,IPM     ,TABLE   ,
     9         IR      ,IS      ,F_DEF   ,ISMSTR  ,NU      ,VOL0    ,
     A         KFTS    ,PINCH_LOCAL%EPINCHZZ,PINCH_LOCAL%EPINCHXZ   ,
     B         PINCH_LOCAL%EPINCHYZ      ,FORP    ,MOMP    ,ALDT    ,
     C         EZZAVG  ,AREAPINCH)
      ENDIF 
 
      RETURN
      END SUBROUTINE CMAIN3PINCH
C
