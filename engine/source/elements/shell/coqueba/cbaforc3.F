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
Chd|  CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- called by -----------
Chd|        FORINTC                       source/elements/forintc.F     
Chd|-- calls ---------------
Chd|        CBACOOR                       source/elements/shell/coqueba/cbacoor.F
Chd|        CBACOORPINCH                  source/elements/shell/coqueba/cbacoorpinch.F
Chd|        CBACOORT                      source/elements/shell/coqueba/cbacoor.F
Chd|        CBADEF                        source/elements/shell/coqueba/cbadef.F
Chd|        CBADEF1                       source/elements/shell/coqueba/cbadef.F
Chd|        CBADEFPINCH                   source/elements/shell/coqueba/cbadefpinch.F
Chd|        CBADEFRZ                      source/elements/shell/coqueba/cbadef.F
Chd|        CBADEFSH                      source/elements/shell/coqueba/cbadef.F
Chd|        CBADEFSH_PLY                  source/properties/composite_options/stack/cbadef_ply.F
Chd|        CBADEFT                       source/elements/shell/coqueba/cbadef.F
Chd|        CBADEFT1                      source/elements/shell/coqueba/cbadef.F
Chd|        CBADEFTW                      source/elements/shell/coqueba/cbadef.F
Chd|        CBADEF_PLY                    source/properties/composite_options/stack/cbadef_ply.F
Chd|        CBADERIRZ                     source/elements/shell/coqueba/cbadef.F
Chd|        CBADERIRZT                    source/elements/shell/coqueba/cbadef.F
Chd|        CBADERIT1                     source/elements/shell/coqueba/cbadef.F
Chd|        CBAENER                       source/elements/shell/coqueba/cbaener.F
Chd|        CBAENERS                      source/elements/shell/coqueba/cbaener.F
Chd|        CBAFINT_PLY                   source/properties/composite_options/stack/cbafint_ply.F
Chd|        CBAFINT_REG                   source/elements/shell/coqueba/cbafint_reg.F
Chd|        CBAFORCT                      source/elements/shell/coqueba/cbafori.F
Chd|        CBAFORI                       source/elements/shell/coqueba/cbafori.F
Chd|        CBAFORI1                      source/elements/shell/coqueba/cbafori.F
Chd|        CBAFORIPINCH                  source/elements/shell/coqueba/cbaforipinch.F
Chd|        CBAFORRZ                      source/elements/shell/coqueba/cbafori.F
Chd|        CBAL58WARP                    source/elements/shell/coqueba/cbawarpoff.F
Chd|        CBAPINCHPROJ                  source/elements/shell/coqueba/cbapinchproj.F
Chd|        CBAPINCHTHK                   source/elements/shell/coqueba/cbapinchthk.F
Chd|        CBAPROJ                       source/elements/shell/coqueba/cbaproj.F
Chd|        CBAPROJ_PLY                   source/properties/composite_options/stack/cbaproj_ply.F
Chd|        CBASTRA3                      source/elements/shell/coqueba/cbastra3.F
Chd|        CBASTRA3PINCH                 source/elements/shell/coqueba/cbastra3pinch.F
Chd|        CBATEMPEL                     source/elements/shell/coqueba/cbatempel.F
Chd|        CBATHERM                      source/elements/shell/coqueba/cbatherm.F
Chd|        CBAVARNL                      source/elements/shell/coqueba/cbavarnl.F
Chd|        CBAVISC                       source/elements/shell/coqueba/cbavisc.F
Chd|        CBAVISNP1                     source/elements/shell/coqueba/cbavisc.F
Chd|        CBAVIT_PLY                    source/properties/composite_options/stack/cbavit_ply.F
Chd|        CBILAN                        source/elements/shell/coque/cbilan.F
Chd|        CMAIN3                        source/materials/mat_share/cmain3.F
Chd|        CMAIN3PINCH                   source/elements/shell/coqueba/cmain3pinch.F
Chd|        CNCOEF3                       source/elements/sh3n/coquedk/cncoef3.F
Chd|        CNDT3                         source/elements/sh3n/coquedk/cndt3.F
Chd|        CNDT3PINCH                    source/elements/shell/coqueba/cndt3pinch.F
Chd|        CNDT_PLY                      source/properties/composite_options/stack/cndt_ply.F
Chd|        CUPDT3F                       source/elements/shell/coque/cupdt3.F
Chd|        CUPDTN3                       source/elements/shell/coque/cupdtn3.F
Chd|        CUPDTN3P                      source/elements/shell/coque/cupdtn3.F
Chd|        CUPDTN3PINCH                  source/elements/shell/coqueba/cupdtn3pinch.F
Chd|        CUPDT_PLY                     source/properties/composite_options/stack/cupdt_ply.F
Chd|        DTCBA_REG                     source/elements/shell/coqueba/dtcba_reg.F
Chd|        DTTHERM                       source/elements/sh3n/coquedk/dttherm.F
Chd|        SET_FAILWAVE_SH4N             source/materials/fail/failwave/upd_failwave_sh4n.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|        MAT_ELEM_MOD                  ../common_source/modules/mat_elem/mat_elem_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        PINCH_LOCAL_MOD               share/modules/pinch_local_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE CBAFORC3(
     1   ELBUF_STR,   JFT,         JLT,         NFT,
     2   NPT,         IPARI,       MTN,         IPRI,
     3   ITHK,        NELTST,      ITYPTST,     ITAB,
     4   MAT_ELEM,    ISTRAIN,     IPLA,        TT,
     5   DT1,         DT2T,        PM,          GEO,
     6   PARTSAV,     IXC,         FAILWAVE,    BUFMAT,
     7   TF,          NPF,         IADC,        X,
     8   D,           DR,          V,           VR,
     9   F,           M,           STIFN,       STIFR,
     A   FSKY,        TANI,        OFFSET,      EANI,
     B   INDXOF,      IPARTC,      THKE,        NVC,
     C   IOFC,        IHBE,        F11,         F12,
     D   F13,         F14,         F21,         F22,
     E   F23,         F24,         F31,         F32,
     F   F33,         F34,         M11,         M12,
     G   M13,         M14,         M21,         M22,
     H   M23,         M24,         M31,         M32,
     I   M33,         M34,         KFTS,        ISMSTR,
     J   IGEO,        GROUP_PARAM, IPM,         IFAILURE,
     K   ITASK,       JTHE,        TEMP,        FTHE,
     L   FTHESKY,     IEXPAN,      ISHPLYXFEM,  MS,
     M   IN,          MS_PLY,      ZI_PLY,      INOD_PXFEM,
     N   IEL_PXFEM,   IADC_PXFEM,  GRESAV,      GRTH,
     O   IGRTH,       MSC,         DMELC,       JSMS,
     P   TABLE,       IPARG,       SENSORS,     MSZ2,
     Q   CONDN,       CONDNSKY,    ISUBSTACK,   STACK,
     R   DRAPE_SH4N,  NEL,         NLOC_DMG,    VPINCH,
     S   FPINCH,      STIFPINCH,   INDX_DRAPE,  IGRE,
     T   JTUR)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE TABLE_MOD
      USE STACK_MOD
      USE FAILWAVE_MOD
      USE MAT_ELEM_MOD        
      USE NLOCAL_REG_MOD    
      USE PINCH_LOCAL_MOD
      USE DRAPE_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C O M M O N   B L O C K S
C-----------------------------------------------
#include      "scr14_c.inc"
#include      "scr18_c.inc"
#include      "parit_c.inc"
#include      "param_c.inc"
#include      "timeri_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IGRE,JTUR
      INTEGER JFT,JLT,NFT,NPT,MTN,IPRI,ITHK,NELTST,
     .        ITYPTST ,ISTRAIN,IPLA ,OFFSET,NVC,
     .        IOFC ,IHBE ,KFTS,ISMSTR,IFAILURE,
     .        IEXPAN, ISHPLYXFEM,ITASK,JTHE,IBID,JSMS,ISUBSTACK,NEL
      INTEGER IXC(NIXC,*), IADC(4,*), IPARTC(*), NPF(*),IGEO(NPROPGI,*),
     .        IPM(*),INDXOF(MVSIZ),INOD_PXFEM(*),IEL_PXFEM(*),ITAB(*),
     .        IADC_PXFEM(4,*),GRTH(*),IGRTH(*),IPARG(*),IPARI(NPARI,*),
     .        INDX_DRAPE(SCDRAPE)
C     REAL OU REAL*8
      my_real
     .   F11(MVSIZ), F12(MVSIZ), F13(MVSIZ), F14(MVSIZ),
     .   F21(MVSIZ), F22(MVSIZ), F23(MVSIZ), F24(MVSIZ),
     .   F31(MVSIZ), F32(MVSIZ), F33(MVSIZ), F34(MVSIZ),
     .   M11(MVSIZ), M12(MVSIZ), M13(MVSIZ), M14(MVSIZ),
     .   M21(MVSIZ), M22(MVSIZ), M23(MVSIZ), M24(MVSIZ),
     .   M31(MVSIZ), M32(MVSIZ), M33(MVSIZ), M34(MVSIZ),
     .   TF(*),   PM(NPROPM,*),GEO(NPROPG,*),PARTSAV(*),
     .   BUFMAT(*),   X(3,*),       D(*),     DR(*),  
     .   V(3,*),VR(3,*),F(3,*),M(3,*),STIFN(*),
     .   STIFR(*),FSKY(*),TANI(6,*),EANI(*),THKE(*),TEMP(*),
     .   FTHE(*),FTHESKY(*),IN(*),MS(*),MS_PLY(*), ZI_PLY(*),
     .   GRESAV(*), MSC(*), DMELC(*),MSZ2(*),
     .   CONDN(*),CONDNSKY(*),
     .   FPINCH(3,*),STIFPINCH(*),VPINCH(3,*)
      my_real    
     .   TT, DT1, DT2T 
      TYPE(TTABLE) TABLE(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (STACK_PLY) :: STACK
      TYPE (FAILWAVE_STR_) :: FAILWAVE 
      TYPE (GROUP_PARAM_)  :: GROUP_PARAM
      TYPE (NLOCAL_STR_), TARGET :: NLOC_DMG 
      TYPE (DRAPE_)  :: DRAPE_SH4N(NUMELC_DRAPE)
      TYPE (MAT_ELEM_),INTENT(INOUT) :: MAT_ELEM
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER 
     .   I,II,J,JJ,JG,IR,IS,IT,IPT,NPTR,NPTS,NPTT,NLAY,MX, 
     .   NPLAT,IDRIL,LENE,LENF,LENM,LENS,NNOD,N1,N2,N3,N4,
     .   NG,NPG,PT1,PT2,PT3,PT4,PTF,PTM,PTE,PTS,L_DIRA,L_DIRB,
     .   IPPID,JPID,IPTHK,IPPOS,IPMAT,IPMAT_IPLY,MATLY,IFAILWAVE,
     .   J1,J2,IIGEO,IADI ,IADR,IPANG,IGTYP,IGMAT,ILAY,NPTTOT,IREP,KK(5),K,
     .   LENFPINCH,LENMPINCH,LENEPINCHXZ,LENEPINCHYZ,LENEPINCHZZ,
     .   PTFP,PTMP,PTEPXZ,PTEPYZ,PTEPZZ,MT,NPINCH,IDRAPE,ACTIFXFEM,
     .   SEDRAPE,NUMEL_DRAPE
      INTEGER  MAT(MVSIZ), PID(MVSIZ), NGL(MVSIZ),MAT_IPLY(MVSIZ,NPT),
     .   IPLAT(MVSIZ),ISTACK(MVSIZ,NPT),FWAVE_EL(NEL)
      PARAMETER (NPG = 4)
      PARAMETER (NNOD = 4)
      my_real 
     .   RXYZ(MVSIZ,2*NNOD),
     .   VCORE(MVSIZ,3*NNOD),VXYZ(MVSIZ,3*NNOD),OFF(MVSIZ),
     .   VQN(MVSIZ,9*NNOD),VQG(MVSIZ,9*NNOD),VNRM(MVSIZ,3*NNOD),
     .   BM(MVSIZ,9*NNOD),BMF(MVSIZ,9*NNOD),BF(MVSIZ,6*NNOD),
     .   BC(MVSIZ,10*NNOD),VQ(MVSIZ,9),VJFI(MVSIZ,6,4),
     .   TC(MVSIZ,4),JAC(MVSIZ,NPG),HX(MVSIZ,NPG),HY(MVSIZ,NPG),
     .   VETA(4,NPG),VKSI(4,NPG),VF(MVSIZ,12),VM(MVSIZ,8),
     .   VASTN(MVSIZ,4*NNOD),AREA(MVSIZ),                   
     .   LC(MVSIZ),VDEF(MVSIZ,8),CDET(MVSIZ),THK2(MVSIZ),   
     .   EXX(MVSIZ) ,EYY(MVSIZ) ,EXY(MVSIZ)   ,EXZ(MVSIZ) ,EYZ(MVSIZ),
     .   KXX(MVSIZ) ,KYY(MVSIZ) ,KXY(MVSIZ)   ,SIGY(MVSIZ), 
     .   DT1C(MVSIZ),SSP(MVSIZ) ,VISCMX(MVSIZ),RHO(MVSIZ) ,
     .   NU(MVSIZ)  ,G(MVSIZ)   ,A11(MVSIZ)   ,A12(MVSIZ) ,VOL0(MVSIZ),
     .   THK0(MVSIZ),STI(MVSIZ) ,STIR(MVSIZ)  ,SHF(MVSIZ) ,
     .   GS(MVSIZ)  ,ALPE(MVSIZ),YM(MVSIZ) ,BID,ZCFAC(MVSIZ,2),
     .   X13(MVSIZ) ,Y13(MVSIZ), X24(MVSIZ) ,AMU(MVSIZ),
     .   DD(MVSIZ,6),VOLG(MVSIZ),Y24(MVSIZ),FACN(MVSIZ,2),DIE(MVSIZ),
     .   TEMPEL(MVSIZ),THEM(MVSIZ,4),
     .   ZL(MVSIZ),PLY_F(MVSIZ,5, NPT), PLY_VXYZ(MVSIZ,3*NNOD,NPT),
     .   FLY11(MVSIZ, NPT), FLY21(MVSIZ, NPT), FLY31(MVSIZ, NPT),
     .   FLY12(MVSIZ, NPT), FLY22(MVSIZ, NPT), FLY32(MVSIZ, NPT),
     .   FLY13(MVSIZ, NPT), FLY23(MVSIZ, NPT), FLY33(MVSIZ, NPT),
     .   FLY14(MVSIZ, NPT), FLY24(MVSIZ, NPT), FLY34(MVSIZ, NPT),
     .   PLY_EXX(MVSIZ,NPT), PLY_EYY(MVSIZ,NPT), PLY_EXY(MVSIZ,NPT), 
     .   PLY_EZX(MVSIZ,NPT), PLY_EYZ(MVSIZ,NPT), PLY_FN(MVSIZ,12,NPT),
     .   THKLY(MVSIZ,NPT),VOL0_LY(MVSIZ,NPT),POSLY(MVSIZ,NPT),
     .   DEL_PLY(MVSIZ,12,NPT),TH_IPLY(MVSIZ,NPT),
     .   SIG_IPLY(MVSIZ,3,NPT),VNI(4,4),
     .   VFI(MVSIZ,12,NPT),DELG_PLY(MVSIZ,3,NPT),AMOM(MVSIZ,3,4),
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   A11_PLY(MVSIZ,NPT),A11_IPLY(MVSIZ,NPT),STI_PLY(MVSIZ,NPT),
     .   OFFI(MVSIZ,NPT),RLZ(MVSIZ,NNOD),VRLZ(MVSIZ),
     .   BM0RZ(MVSIZ,4,NNOD),BMKRZ(MVSIZ,4,NNOD),BMERZ(MVSIZ,4,NNOD),
     .   BMRZ(MVSIZ,3,NNOD),BRZ(MVSIZ,4,NNOD),KRZ(MVSIZ),
     .   VMZ(MVSIZ,NNOD),UX1(MVSIZ),UX2(MVSIZ),UX3(MVSIZ),UX4(MVSIZ),
     .   UY1(MVSIZ),UY2(MVSIZ),UY3(MVSIZ),UY4(MVSIZ),
     .   CONDE(MVSIZ),A11R(MVSIZ),
     .   VL1(MVSIZ,3),VL2(MVSIZ,3),VL3(MVSIZ,3),VL4(MVSIZ,3),
     .   XL2(MVSIZ),XL3(MVSIZ),XL4(MVSIZ),YL2(MVSIZ),YL3(MVSIZ),YL4(MVSIZ),
     .   VDEFPINCH(MVSIZ,3), VPINCHXYZ(MVSIZ,NNOD), BCP(MVSIZ,2*NNOD), 
     .   BP(MVSIZ,NNOD), TNPG(MVSIZ,NNOD,NPG), VFPINCH(MVSIZ,4), FACP(MVSIZ),
     .   E, ANU, A11PINCH, FP(MVSIZ,3,4),
     .   VPINCHT1(MVSIZ,NNOD),VPINCHT2(MVSIZ,NNOD),DBETADXY(MVSIZ,3),
     .   BPINCHDAMP(MVSIZ,8),VFPINCHDAMPX(MVSIZ,4),VFPINCHDAMPY(MVSIZ,4),
     .   EZZAVG(MVSIZ),AREAPINCH(MVSIZ),ZLA(MVSIZ)
      INTEGER 
     .   NPLATT,PTW ,LENW,PTT,IPOUT
      INTEGER  IPLATT(MVSIZ)
      my_real 
     .   VCORET(MVSIZ,3*NNOD),BMT(MVSIZ,9*NNOD),VQGT(MVSIZ,9*NNOD),
     .   VJFIT(MVSIZ,6,4),JACT(MVSIZ,NPG),HXT(MVSIZ,NPG),HYT(MVSIZ,NPG),
     .   AREAT(MVSIZ),X13T(MVSIZ) ,Y13T(MVSIZ), X24T(MVSIZ),Y24T(MVSIZ),
     .   BM0RZT(MVSIZ,4,NNOD),BMKRZT(MVSIZ,4,NNOD),BMERZT(MVSIZ,4,NNOD),
     .   BMRZT(MVSIZ,4,NNOD),F_DEF(MVSIZ,8,NPG),
     .  X1G(MVSIZ), X2G(MVSIZ), X3G(MVSIZ), X4G(MVSIZ),
     .  Y1G(MVSIZ), Y2G(MVSIZ), Y3G(MVSIZ), Y4G(MVSIZ),
     .  Z1G(MVSIZ), Z2G(MVSIZ), Z3G(MVSIZ), Z4G(MVSIZ),
     .  VRL1(MVSIZ,3),VRL2(MVSIZ,3),VRL3(MVSIZ,3),VRL4(MVSIZ,3),
     .   UXYZ(MVSIZ,12),AXYZ(MVSIZ,4),WXY(MVSIZ),XLCORE(MVSIZ,2*(NNOD-1))
C-----------------------------------------------
      INTEGER, DIMENSION(NEL) :: OFFLY
      my_real, DIMENSION(:) ,POINTER     :: DIR_A, DIR_B,CRKDIR,DADV
      my_real, ALLOCATABLE, DIMENSION(:) :: DIR1_CRK,DIR2_CRK,DIRA,DIRB
      my_real
     .   EZZPG(MVSIZ,4)
      TARGET :: DIRA,DIRB
      INTEGER :: NDDL, NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ),
     .           INLOC
      my_real, 
     .   DIMENSION(:,:), ALLOCATABLE :: VAR_REG
C-------------------------------------
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(L_BUFEL_) ,POINTER :: LBUF1,LBUF2,LBUF3,LBUF4     
      TYPE(G_BUFEL_) ,POINTER :: GBUF 
      TYPE(L_BUFEL_) ,POINTER :: LBUF
      TYPE(L_BUFEL_DIR_) ,POINTER :: LBUF_DIR
      TYPE(PINCH_LOCAL_STRUCT_) :: PINCH_LOCAL    
C=======================================================================
C     INITIALISATION
C--------------------------
      ISTACK = 0
      GBUF => ELBUF_STR%GBUF
      IDRAPE = ELBUF_STR%IDRAPE
      IBID = 0
      BID = ZERO
      IDRIL = IPARG(41)
      IREP  = IPARG(35)
      INLOC = IPARG(78)
      ACTIFXFEM = IPARG(70)
      NPINCH= IPARG(90)
      SEDRAPE = SCDRAPE
      NUMEL_DRAPE = NUMELC_DRAPE
C
      NLAY = ELBUF_STR%NLAY
      NPTR = ELBUF_STR%NPTR
      NPTS = ELBUF_STR%NPTS
cc      NPT  = MAX(NLAY,NPTT) --> set to = IPARG(6) , keeping it original
!
      DO J=1,5
        KK(J) = NEL*(J-1)
      ENDDO
!
C                                 to allow for NPT = 0 (global LAW_3
      DO I=JFT,JLT
        MAT(I) = IXC(1,I)
        PID(I) = IXC(6,I)
        NGL(I) = IXC(7,I)
      ENDDO
C
      NPTTOT  = 0
      DO ILAY=1,NLAY
        NPTTOT = NPTTOT + ELBUF_STR%BUFLY(ILAY)%NPTT
      ENDDO
      IF (NPT == 0) NPTTOT = NPT  !  compatibility with global integration
C
!-------------------------------------------      
! Tableau pour la variable non-locale
      NDDL = NPTTOT
      ALLOCATE(VAR_REG(NEL,NDDL)) 
!-------------------------------------------
C
c--------------------------------------------
c     Front wave
c--------------------------------------------
      IFAILWAVE = IPARG(79)
      IF (IFAILWAVE > 0) THEN
        FWAVE_EL(:) = ZERO
        OFFLY(:) = ELBUF_STR%BUFLY(1)%OFF(:)
        DO I=2,NLAY
          DO J=1,NEL
            OFFLY(J) = MAX(OFFLY(J), ELBUF_STR%BUFLY(I)%OFF(J))
          ENDDO
        ENDDO        
        DADV => GBUF%DMG
        CALL SET_FAILWAVE_SH4N(FAILWAVE    ,FWAVE_EL ,DADV     ,
     .       NEL      ,IXC      ,ITAB      ,NGL      ,OFFLY    )
      ENDIF
c-------------------------------------
      L_DIRA = ELBUF_STR%BUFLY(1)%LY_DIRA
      L_DIRB = ELBUF_STR%BUFLY(1)%LY_DIRB
      IGTYP = IGEO(11,PID(1))
      IF(IDRAPE > 0 .AND. (IGTYP == 51 .OR. IGTYP == 52)) THEN
        ALLOCATE(DIRA(NPTTOT*NEL*L_DIRA))
        ALLOCATE(DIRB(NPTTOT*NEL*L_DIRB))
        DIRA = ZERO
        DIRB = ZERO
        IF (L_DIRA == 0) THEN
            CONTINUE
        ELSEIF (IREP == 0) THEN
           NPTTOT = 0
           DO ILAY=1,NLAY
              NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
              DO IT=1,NPTT
                 J = NPTTOT + IT
                 LBUF_DIR =>  ELBUF_STR%BUFLY(ILAY)%LBUF_DIR(IT)
                 J1 = 1+(J-1)*L_DIRA*NEL
                 J2 = J*L_DIRA*NEL
                 DIRA(J1:J2) = LBUF_DIR%DIRA(1:NEL*L_DIRA)
              ENDDO
              NPTTOT = NPTTOT + NPTT
            ENDDO 
        ENDIF
        DIR_A => DIRA(1:NPTTOT*NEL*L_DIRA)
        DIR_B => DIRB(1:NPTTOT*NEL*L_DIRB)
      ELSE ! idrape
        ALLOCATE(DIRA(NLAY*NEL*L_DIRA))
        ALLOCATE(DIRB(NLAY*NEL*L_DIRB))
        DIRA=ZERO
        DIRB=ZERO
        IF (L_DIRA == 0) THEN
          CONTINUE
        ELSEIF (IREP == 0) THEN
           DO J=1,NLAY
              J1 = 1+(J-1)*L_DIRA*NEL
              J2 = J*L_DIRA*NEL
              DIRA(J1:J2) = ELBUF_STR%BUFLY(J)%DIRA(1:NEL*L_DIRA)
           ENDDO
         ENDIF
         DIR_A => DIRA(1:NLAY*NEL*L_DIRA)
         DIR_B => DIRB(1:NLAY*NEL*L_DIRB)
      ENDIF ! IDRAPE    
c-------------------------------------
      ALLOCATE(DIR1_CRK(0))
      ALLOCATE(DIR2_CRK(0))
c-------------------------------------
      DO I=JFT,JLT
        DO J=1,8
         VM(I,J) = ZERO
        ENDDO
        DO J=1,12
          VF(I,J) = ZERO
        ENDDO
        DO J=1,4
          VFPINCH(I,J) = ZERO
          EZZPG(I,J)   = ZERO
          VFPINCHDAMPX(I,J) = ZERO
          VFPINCHDAMPY(I,J) = ZERO
        ENDDO
       ALPE(I) = ONE
       A11R(I) = ZERO
      ENDDO
C      
c      CALL CMATBUF3(IGTYP,MTN,NPT,NEL,NBM_S,IPM,MAT,
c     .              IGEO, PID,ISUBSTACK)
      IGTYP = IGEO(11,IXC(6,1))
      IGMAT  = IGEO(98 ,IXC(6,1))
C      
C initiallisation pour la thermique ---
       DO I=JFT,JLT
         THEM(I,1) = ZERO
         THEM(I,2) = ZERO
         THEM(I,3) = ZERO
         THEM(I,4) = ZERO
         TEMPEL(I) = ZERO
       ENDDO
C
       IF(NPINCH > 0) THEN
         ALLOCATE(PINCH_LOCAL%EPINCHXZ(MVSIZ))
         ALLOCATE(PINCH_LOCAL%EPINCHYZ(MVSIZ))
         ALLOCATE(PINCH_LOCAL%EPINCHZZ(MVSIZ))
       ENDIF
C 
C--------------------------
C     CALCULS PRELIMINAIRES 
C--------------------------
       CALL CBACOOR(ELBUF_STR ,JFT,JLT,X,V,
     .              VR,IXC,PM,GBUF%OFF,LC,
     1              AREA,VXYZ, RXYZ,VCORE,JAC,HX,HY,VKSI,VETA,
     2              VQN,VQG,VQ,VJFI,VNRM,VASTN,NPLAT,IPLAT,
     3              X13  ,X24  ,Y13,Y24,OFF, DD,NLAY, 
     4              IREP,NPTTOT,ISMSTR,NEL     ,IDRIL ,
     5              GBUF%SMSTR,DIR_A,DIR_B,FACN,ZL,
     6              R11   ,R12   ,R13   ,R21   ,R22   ,R23   ,
     7              R31   ,R32   ,R33   ,INOD_PXFEM   ,RLZ   ,
     8              THKE  ,ISHPLYXFEM   ,UX1   ,UX2   ,UX3   ,
     9              UX4   ,UY1   ,UY2   ,UY3   ,UY4   ,
     A              VL1   ,VL2   ,VL3   ,VL4   ,XL2   ,
     B              XL3   ,XL4   ,YL2   ,YL3   ,YL4   ,XLCORE,NPINCH)
C
       CALL CNCOEF3(JFT     ,JLT     ,PM      ,MAT     ,GEO     ,
     2              PID     ,OFF     ,AREA    ,SHF     ,THK0    ,
     3              THK2    ,NU      ,G       ,YM      ,
     4              A11     ,A12     ,GBUF%THK,THKE    ,SSP     ,
     5              RHO     ,VOLG    ,GS      ,MTN     ,ITHK    ,
     6              NPTTOT  ,DT1C    ,DT1     ,IHBE    ,AMU     ,
     7              KRZ     ,IGEO    ,A11R    ,ISUBSTACK, STACK%PM)
C
       IF(NPINCH > 0) THEN
         CALL CBACOORPINCH(
     1                     TNPG    ,VPINCHXYZ ,VPINCH , 
     2                     VQ      ,VQN       ,IXC  ,JFT  ,JLT   ,
     3                     NPLAT   ,IPLAT     ,GBUF%THK   ,DT1C  ,
     4                     FACP    ,LC        ,
     5                     VPINCHT1,VPINCHT2)  

         DO I=JFT,JLT
           EZZAVG(I) = FOURTH*(VPINCHXYZ(I,1)+VPINCHXYZ(I,2)+VPINCHXYZ(I,3)+VPINCHXYZ(I,4))*DT1C(I)
           AREAPINCH(I) = AREA(I)
         ENDDO     
       ENDIF 
C
        IF(ISHPLYXFEM > 0)  THEN
           DO J=1,NPT
             DO I=JFT,JLT
               PLY_FN(I,1:12,J) = ZERO 
               VFI(I,1:12,J) = ZERO
               OFFI(I,J) = ONE
             ENDDO
           ENDDO 
           IPPID   = 2                      
           IPMAT   = IPPID + NPT
           IPMAT_IPLY = IPMAT  + NPT           
           IPANG  =  1                      
           IPTHK  =  IPANG + NPT               
           IPPOS  =  IPTHK + NPT               
           DO J=1,NPT                       
              DO I=JFT,JLT  
                THKLY(I,J) = STACK%GEO(IPTHK + J ,ISUBSTACK)*THK0(I)
                MATLY = STACK%IGEO(IPMAT + J ,ISUBSTACK)
                JPID =  STACK%IGEO(IPPID + J, ISUBSTACK)
                ISTACK(I,J) =  IGEO(102  ,JPID)
                POSLY(I,J) = STACK%GEO(IPPOS + J ,ISUBSTACK)*THK0(I) 
                A11_PLY(I,J) = PM(24,MATLY)
              ENDDO  
           ENDDO  
           DO J=1,NPT -1  
               DO I=JFT,JLT  
                 TH_IPLY(I,J) = HALF*(THKLY(I,J) + THKLY(I,J +1 ))
                 MAT_IPLY(I,J) = STACK%IGEO(IPMAT_IPLY + J ,ISUBSTACK)
               ENDDO                                 
           ENDDO 
C    
          CALL CBAVIT_PLY(JFT,JLT,IXC,GBUF%OFF,OFF,NPLAT,IPLAT,NPT,
     1                 VCORE,DD,ZL,VQ , PLY_VXYZ,X13  ,X24  ,
     2                 Y13,Y24,AREA  ,INOD_PXFEM ,DEL_PLY,VNI,ISTACK,VR)
     
        ENDIF
C  
       IF (IDRIL > 0) THEN
        CALL CBADERIRZ(JFT  ,JLT  ,AREA ,X13 ,X24   ,
     2                 Y13  ,Y24  ,BM0RZ,BMKRZ,BMERZ,
     3                 VCORE,NPLAT,IPLAT,ISMSTR)
         DO I=JFT,JLT
           DO J=1,4
             VMZ(I,J) = ZERO
           ENDDO
         END DO
       ELSE
C-------------assumed membrane shear strain-----------------
        CALL CBADEFSH(JFT,JLT,X13,X24,Y13,Y24,BM,VDEF,VXYZ,NPLAT,IPLAT)
        CALL CBAENERS(JFT   ,JLT      ,OFF       ,AREA  ,THK0,
     .                VDEF  ,GBUF%FOR ,GBUF%EINT ,DT1   ,NEL )
       END IF !(IDRIL > 0) THEN
C            
       IF(ISHPLYXFEM > 0) 
     .   CALL CBADEFSH_PLY(JFT,JLT,NPT,NPLAT,IPLAT,X13,X24,Y13,Y24,
     .                   PLY_VXYZ,DT1C ,PLY_EXY)        
C-----------------------------------------------
C     BOUCLE SUR POINTS D'INTEGRATION DE GAUSS
C-----------------------------------------------
      LENF = NEL*GBUF%G_FORPG/NPG
      LENM = NEL*GBUF%G_MOMPG/NPG
C
      IF (NPINCH > 0) THEN      
        LENFPINCH = NEL*GBUF%G_FORPGPINCH/NPG
        LENMPINCH = NEL*GBUF%G_MOMPGPINCH/NPG
        LENEPINCHXZ = NEL*GBUF%G_EPGPINCHXZ/NPG
        LENEPINCHYZ = NEL*GBUF%G_EPGPINCHYZ/NPG
        LENEPINCHZZ = NEL*GBUF%G_EPGPINCHZZ/NPG
      ENDIF
C
      LENS = NEL*GBUF%G_STRPG/NPG
      LENW = NEL*GBUF%G_STRWPG/NPG
      IF (ISMSTR == 10 ) THEN
        CALL CBACOORT(ELBUF_STR,JFT,JLT,X,V,
     .            VR,DR,IXC,PM,GBUF%OFF,AREAT,
     1            UXYZ, AXYZ,VCORET,JACT,HXT,
     2            HYT,VQ,VQGT,VJFIT,NPLATT,IPLATT,
     3            X13T  ,X24T  ,Y13T,Y24T,NPTTOT ,
     4            GBUF%SMSTR ,  IDRIL ,XLCORE,ZL,VQN,NEL)
C  
       IF (IDRIL > 0) THEN
        CALL CBADERIRZ(JFT  ,JLT  ,AREAT,X13T,X24T  ,
     2                 Y13T ,Y24T ,BM0RZT,BMKRZT,BMERZT,
     3                 VCORET,NPLATT,IPLATT,ISMSTR)
C------assumed membrane shear strain---(no assumed shear for Ismstr=10-
       END IF !(IDRIL > 0) THEN
       DO IS = 1,NPTS
       DO IR = 1,NPTR
        NG = NPTR*(IS-1) + IR
        PTF = (NG-1)*LENF+1
        PTM = (NG-1)*LENM+1
        PTS = (NG-1)*LENS+1
c
         DO I=JFT,JLT
          CDET(I) = JACT(I,NG)
          VOL0(I) = THK0(I)*CDET(I)
         ENDDO
C-----------------------------------------------
C       DEFORMATIONS, MATRICE [B]
C-----------------------------------------------
        IF (IDRIL > 0) THEN
         CALL CBADERIRZT(JFT,JLT,NG,BM0RZT,BMKRZT,BMERZT,BMRZT)
        END IF !(IDRIL > 0) THEN
C-------no warped element w/ NPT=1              
        IF (NPTTOT == 1) THEN
         CALL CBADEFT1(JFT,JLT,NG,VCORET,UXYZ,F_DEF(1,1,NG),
     1                 HXT,HYT,BMT,NPLATT,IPLATT,IDRIL,
     2                 BMRZT,AXYZ,WXY )
        ELSE
         CALL CBADERIT1(JFT,JLT,NG,VCORET,VQGT,VJFIT,
     2                  HXT,HYT,VETA,VKSI,BMT,NPLATT,IPLATT,
     3                  IDRIL)
         CALL CBADEFT(JFT,JLT,UXYZ,AXYZ,F_DEF(1,1,NG),
     2                BMT,NPLATT,IPLATT,IDRIL,BMRZT )
        END IF ! NPT == 1
C  
       ENDDO  ! NPTR
      ENDDO  ! NPTS
      END IF
C      
      IF (NPTTOT == 1.AND.MTN==58) THEN
        ZLA(JFT:JLT)= ZL(JFT:JLT)*ZL(JFT:JLT)/AREA(JFT:JLT)
        CALL CBAL58WARP(ELBUF_STR,NEL,
     1                  X,IXC,R13 ,R23 ,R33  ,GBUF%OFF ,ZLA )
      END IF
c      IT  = 1
      DO IS = 1,NPTS
       DO IR = 1,NPTR
        NG = NPTR*(IS-1) + IR
        PTF = (NG-1)*LENF+1
        PTM = (NG-1)*LENM+1
        PTS = (NG-1)*LENS+1
        PTW = (NG-1)*LENW+1
        PTT = (NG-1)*NEL + 1
c-------- can extent the off later
        DO I=JFT,JLT
          CDET(I) = JAC(I,NG)
          VOL0(I) = THK0(I)*CDET(I)
        ENDDO
         IF(ISHPLYXFEM > 0)  THEN        
           DO J=1,NPT
             DO I=JFT,JLT
               OFFI(I,J) = ONE
             ENDDO
           ENDDO 
         ENDIF
C-----------------------------------------------
C       DEFORMATIONS, MATRICE [B]
C-----------------------------------------------
        IF (NPTTOT == 1) THEN
         CALL CBADEF1(JFT,JLT,NG,VCORE,VXYZ,VDEF,
     1                    HX,HY,BM,NPLAT,IPLAT,IDRIL)
C----   
        ELSE
         CALL CBADEF(JFT,JLT,NG,VCORE,AREA,CDET,VQN,VQG,VJFI,
     1                    VXYZ,RXYZ,VDEF,VNRM,VASTN,
     2                    HX,HY,VETA,VKSI,BM,BMF,BF,BC,TC,NPLAT,IPLAT,
     3                    IDRIL,BRZ )
         IF (ISMSTR == 10 ) 
     1    CALL CBADEFTW(JFT,JLT,VXYZ,RXYZ,
     2                   BM,BMF,BF,NPLAT,IPLAT,
     3                   WXY )
        END IF ! NPT == 1
        IF (IDRIL > 0) THEN
          CALL CBADEFRZ(JFT ,JLT  ,AREA ,RLZ   ,VDEF ,
     1                       VXYZ ,BM0RZ,BMKRZ,BMERZ ,VRLZ ,
     2                       BMRZ ,BRZ  ,BM   ,NPLAT ,IPLAT,
     3                       NG   )
        END IF
C
        IF (NPINCH > 0) THEN
          CALL CBADEFPINCH(
     1                     JFT  ,JLT  ,NG   ,VQG   ,VDEF  ,
     2                     VETA ,VKSI ,TC   ,NPLAT ,IPLAT ,
     3                     BCP  ,BP   ,VPINCHXYZ   ,VDEFPINCH ,TNPG,
     4                     DBETADXY   ,VPINCHT1    ,VPINCHT2  ,BPINCHDAMP)
        ENDIF
C
C----------------------------------
C       CALCUL DES DEFORMATIONS 
C----------------------------------
        CALL CBASTRA3(GBUF%STRA,GBUF%STRPG(PTS),
     1             JFT, JLT, NFT, NPG,VDEF, 
     2             EXX, EYY, EXY, EXZ,  EYZ,  
     3             KXX, KYY, KXY, DT1C, TANI,
     4             IEPSDOT, ISTRAIN,UX1 ,UX2 ,UX3 ,
     6             UX4    ,UY1    ,UY2 ,UY3 ,UY4 ,
     7             X13, X24, Y13, Y24, BM  ,
     8             ISMSTR ,MTN ,NPLAT,IPLAT,IDRIL,
     9             WXY    ,F_DEF(1,1,NG),GBUF%STRWPG(PTW),NEL)
C  
        IF (IDRIL == 0) THEN
          CALL CBAENER(GBUF%FORPG(PTF),GBUF%EINT,JFT  ,JLT    ,OFF   ,
     .                 VOL0           ,EXY      ,NEL  )
        ENDIF 
        IF (ISHPLYXFEM > 0 ) THEN 
          DO J=1,NPT                                          
            JG = (NG - 1)*3                                  
            DO I=JFT,JLT                                      
               DELG_PLY(I,1,J) = DEL_PLY(I,1 + JG ,J)         
               DELG_PLY(I,2,J) = DEL_PLY(I,2 + JG ,J)         
               DELG_PLY(I,3,J) = DEL_PLY(I,3 + JG ,J)         
            ENDDO                                             
          ENDDO                                               
C        
          CALL CBADEF_PLY(JFT,JLT,NG,NPT,NPLAT,IPLAT, VQG,    
     .                   PLY_VXYZ,VETA,VKSI,BM,BC,TC,DT1C,    
     .                   PLY_EXX, PLY_EYY, PLY_EYZ, PLY_EZX ) 
        ENDIF   
C
        IF(NPINCH > 0) THEN
C
          NG = NPTR*(IS-1) + IR
          PTFP = (NG-1)*LENFPINCH + 1 
          PTMP = (NG-1)*LENMPINCH + 1
          PTEPXZ = (NG-1)*LENEPINCHXZ + 1
          PTEPYZ = (NG-1)*LENEPINCHYZ + 1
          PTEPZZ = (NG-1)*LENEPINCHZZ + 1
C
          CALL CBASTRA3PINCH(
     1                       JFT   ,JLT   ,NPLAT    ,IPLAT    ,
     2                       VDEFPINCH    ,PINCH_LOCAL%EPINCHXZ ,
     3                       PINCH_LOCAL%EPINCHYZ ,PINCH_LOCAL%EPINCHZZ,
     4                       DT1C  ,NG       ,EZZPG    ,
     5                       GBUF%EPGPINCHXZ(PTEPXZ),
     6                       GBUF%EPGPINCHYZ(PTEPYZ),
     7                       GBUF%EPGPINCHZZ(PTEPZZ)  )
C
        ENDIF 
C
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
        IF (JTHE > 0 ) THEN
           CALL CBATEMPEL(JFT ,JLT   ,NG  ,IXC   ,TEMP ,
     .                  TEMPEL)
        ENDIF
C-----------------------------------------------------------------------
        IF (INLOC>0) THEN
          CALL CBAVARNL(JFT      ,JLT      ,NG       ,IXC      ,NLOC_DMG ,
     .                  VAR_REG  ,NDDL     ,NC1      ,NC2      ,NC3      ,
     .                  NC4      ,NEL      )
        ENDIF
C-----------------
C       CONTRAINTES
C-----------------
        IF ((ITASK==0).AND.(IMON_MAT == 1)) CALL STARTIME(35,1)
C-----------------
        IF (NPINCH > 0) THEN
          CALL CMAIN3PINCH(
     1    ELBUF_STR ,JFT       ,JLT       ,NFT       ,IPARG      ,
     2    NEL       ,MTN       ,IPLA      ,ITHK      ,GROUP_PARAM,
     3    PM        ,GEO       ,NPF       ,TF        ,BUFMAT     ,
     4    SSP       ,RHO       ,VISCMX    ,DT1C      ,SIGY       ,
     5    CDET      ,EXX       ,EYY       ,EXY       ,EXZ        ,
     6    EYZ       ,KXX       ,KYY       ,KXY       ,NU         ,
     7    OFF       ,THK0      ,MAT       ,PID       ,
     8    GBUF%FORPG(PTF),GBUF%MOMPG(PTM) ,GBUF%STRPG(PTS),FAILWAVE,FWAVE_EL,
     9    GBUF%THK  ,GBUF%EINT ,IOFC      ,
     A    G         ,A11       ,A12       ,VOL0      ,INDXOF    ,
     B    NGL       ,ZCFAC     ,SHF       ,GS        ,GBUF%EPSD ,
     C    KFTS      ,IHBE      ,ALPE      ,
     D    DIR_A     ,DIR_B     ,IGEO      ,
     E    IPM       ,IFAILURE  ,NPG       ,
     F    TEMPEL    ,DIE       ,JTHE      ,IEXPAN    ,GBUF%TEMPG(PTT) ,
     G    ISHPLYXFEM,PLY_EXX   ,
     H    PLY_EYY   ,PLY_EXY   ,PLY_EZX   ,PLY_EYZ   ,PLY_F     ,
     I    DELG_PLY  ,TH_IPLY   ,SIG_IPLY  ,R11       ,R12       ,
     J    R13       ,R21       ,R22       ,R23       ,R31       ,
     K    R32       ,R33       ,NG        ,TABLE     ,IBID      ,
     L    OFFI      ,A11_IPLY  ,IBID      ,
     M    DIR1_CRK  ,DIR2_CRK  ,LC        ,
     N    ISMSTR    ,IR        ,IS        ,NLAY      ,NPT       ,
     O    IBID      ,IBID      ,ISUBSTACK ,STACK     ,
     P    F_DEF(1,1,NG),ITASK  ,DRAPE_SH4N     ,VAR_REG(1,1),
     Q    PINCH_LOCAL , GBUF%FORPGPINCH(PTFP), GBUF%MOMPGPINCH(PTMP),EZZAVG  ,
     R    AREAPINCH   )
        ELSE
          CALL CMAIN3(
     1    ELBUF_STR ,JFT       ,JLT       ,NFT       ,IPARG      ,
     2    NEL       ,MTN       ,IPLA      ,ITHK      ,GROUP_PARAM,
     3    PM        ,GEO       ,NPF       ,TF        ,BUFMAT     ,
     4    SSP       ,RHO       ,VISCMX    ,DT1C      ,SIGY       ,
     5    CDET      ,EXX       ,EYY       ,EXY       ,EXZ        ,
     6    EYZ       ,KXX       ,KYY       ,KXY       ,NU         ,
     7    OFF       ,THK0      ,MAT       ,PID       ,MAT_ELEM   ,
     8    GBUF%FORPG(PTF),GBUF%MOMPG(PTM) ,GBUF%STRPG(PTS),FAILWAVE,FWAVE_EL,
     9    GBUF%THK  ,GBUF%EINT ,IOFC      ,
     A    G         ,A11       ,A12       ,VOL0      ,INDXOF    ,
     B    NGL       ,ZCFAC     ,SHF       ,GS        ,GBUF%EPSD ,
     C    KFTS      ,IHBE      ,ALPE      ,
     D    DIR_A     ,DIR_B     ,IGEO      ,
     E    IPM       ,IFAILURE  ,NPG       ,
     F    TEMPEL    ,DIE       ,JTHE      ,IEXPAN    ,GBUF%TEMPG(PTT) ,
     G    ISHPLYXFEM,PLY_EXX   ,
     H    PLY_EYY   ,PLY_EXY   ,PLY_EZX   ,PLY_EYZ   ,PLY_F     ,
     I    DELG_PLY  ,TH_IPLY   ,SIG_IPLY  ,R11       ,R12       ,
     J    R13       ,R21       ,R22       ,R23       ,R31       ,
     K    R32       ,R33       ,NG        ,TABLE     ,IBID      ,
     L    OFFI      ,SENSORS   ,A11_IPLY  ,IBID      ,
     M    DIR1_CRK  ,DIR2_CRK  ,LC        ,
     N    ISMSTR    ,IR        ,IS        ,NLAY      ,NPT       ,
     O    IBID      ,IBID      ,ISUBSTACK ,STACK     ,
     P    F_DEF(1,1,NG),ITASK  ,DRAPE_SH4N     ,VAR_REG(1,1),NLOC_DMG ,
     R    INDX_DRAPE ,THKE        ,SEDRAPE     ,NUMEL_DRAPE)        
        ENDIF
C-----------------
        IF ((ITASK==0).AND.(IMON_MAT == 1)) CALL STOPTIME(35,1)
C      
        IF (IDRIL == 0) THEN
          CALL CBAENER(GBUF%FORPG(PTF),GBUF%EINT,JFT  ,JLT    ,OFF    ,
     .                 VOL0           ,EXY      ,NEL  )
        ENDIF
C----------------------------------------------------------------------------
C       THICKNESS CORRECTION 
C----------------------------
        IF(NPINCH == 0) THEN
          IF (ITHK > 0) THEN
            DO I=JFT,JLT
              GBUF%THK(I) = GBUF%THK(I) - THREE_OVER_4*(GBUF%THK(I)-THK0(I))
              THK0(I) = GBUF%THK(I)
            ENDDO
          ENDIF
        ENDIF
C----------------------------------------------------------------------------
C     FORCES VISCOCITE 
C----------------------------
        CALL CBAVISC(JFT    ,JLT            ,VDEF           ,AMU   ,OFF ,
     2               SHF    ,NU             ,RHO            ,SSP   ,CDET,
     3               THK0   ,GBUF%FORPG(PTF),GBUF%MOMPG(PTM),NPTTOT,MTN ,
     4               IPARTC ,PARTSAV        ,DT1            ,NEL   )
C----------------------------------------------------------------------------
C       FORCES INTERNES 
C----------------------------
        IF (NPTTOT == 1) THEN
          CALL CBAFORI1(JFT   ,JLT   ,GBUF%FORPG(PTF),BM  ,VF  ,
     .                  NPLAT ,IPLAT ,VOL0           ,NEL ) 
        ELSE
          CALL CBAFORI(JFT  ,JLT            ,NG             ,CDET ,THK0,
     2                 THK2 ,GBUF%FORPG(PTF),GBUF%MOMPG(PTM),NEL  ,BM  ,
     3                 BMF  ,BF             ,BC             ,TC   ,VF  ,
     4                 VM   ,NPLAT          ,IPLAT          ,VOL0 )
        END IF !(NPT == 1) THEN
C
        IF (IDRIL > 0) THEN                                           
          CALL CBAFORRZ(JFT  ,JLT  ,VOL0     ,GBUF%FORPG(PTF),GBUF%HOURG,
     2                  VF   ,VMZ  ,BM       ,BMRZ           ,BRZ       ,                
     3                  KRZ  ,VRLZ ,GBUF%EINT,OFF            ,DT1C      ,              
     4                  NPLAT,IPLAT,NG       ,NEL)
        END IF
C
        IF (ISHPLYXFEM > 0)
     .       CALL CBAFINT_PLY(JFT,JLT,NPT,NG,NPLAT,IPLAT,CDET,THKLY,THK2,
     1                        VOL0, PLY_F,BM,BC,TC,SIG_IPLY,VNI,AREA,
     2                        PLY_FN ,VFI,IXC)

        IF (NPINCH > 0) THEN
          CALL CBAFORIPINCH( 
     1                      JFT   ,JLT   ,NG   ,NEL   ,NPLAT ,IPLAT ,  
     2                      CDET  ,THK0  ,THK2 ,VOL0  ,
     3                      GBUF%FORPGPINCH(PTFP) , GBUF%MOMPGPINCH(PTMP), 
     4                      BCP   ,BP    ,VFPINCH ,DBETADXY,
     5                      RHO   ,LC    ,SSP     ,BPINCHDAMP,
     6                      VFPINCHDAMPX ,VFPINCHDAMPY)
        ENDIF
C-------------------------
c     Thermal Analysis
C--------------------------
C
         IF (JTHE > 0) THEN
          CALL CBATHERM(JFT   ,JLT   ,PM     ,MAT     ,THK0  ,IXC     ,
     .                  BM    ,AREA  ,DT1C   ,TEMP    ,TEMPEL,DIE   , 
     .                  NPLAT  ,IPLAT,THEM    ) 
         ENDIF
c-------------------------
c     Virtual internal forces of regularized non local ddl 
c--------------------------
         IF (INLOC > 0) THEN
           CALL CBAFINT_REG(
     1   NLOC_DMG,         VAR_REG(1,1),     THK0,             NEL,
     2   GBUF%OFF,         AREA,             NC1,              NC2,
     3   NC3,              NC4,              ELBUF_STR%NLOC(IR,IS), IXC(1,JFT),
     4   NDDL,             ITASK,            NG,               JFT,
     5   JLT,              X13,              Y13,              X24,
     6   Y24,              DT2T,             GBUF%THK_I, GBUF%AREA,
     7   NFT)
         ENDIF 
       ENDDO  ! NPTR
      ENDDO  ! NPTS
    
C---------------------------------------
C-----FIN DE BOUCLE DE 4 POINTS DE GAUSS
C---------------------------------------
C
      IF (NPINCH > 0) THEN
        CALL CBAPINCHTHK(
     1                   JFT   ,JLT   ,NPLAT  ,IPLAT  ,
     2                   DT1C  ,GBUF%THK      ,THK0   ,EZZPG)          
      ENDIF
C
C----
C---------------------------------------
C     POST-TRAITEMENT - valeurs moyennes
C---------------------------------------
C---
C    = FOR, MOM =
C---
      PT1 = 0
      PT2 = PT1 + LENF
      PT3 = PT2 + LENF
      PT4 = PT3 + LENF
      DO I=JFT,JLT
        DO J=1,5
          GBUF%FOR(KK(J)+I) = FOURTH*(GBUF%FORPG(PT1+KK(J)+I)
     .                             + GBUF%FORPG(PT2+KK(J)+I)
     .                             + GBUF%FORPG(PT3+KK(J)+I)
     .                             + GBUF%FORPG(PT4+KK(J)+I))
        ENDDO
      ENDDO
!
      PT2 = PT1 + LENM
      PT3 = PT2 + LENM
      PT4 = PT3 + LENM
      DO I=JFT,JLT
        DO J=1,3
          GBUF%MOM(KK(J)+I) = FOURTH*(GBUF%MOMPG(PT1+KK(J)+I)
     .                             + GBUF%MOMPG(PT2+KK(J)+I)
     .                             + GBUF%MOMPG(PT3+KK(J)+I)
     .                             + GBUF%MOMPG(PT4+KK(J)+I))
        ENDDO
      ENDDO
c------------------------------      
C     membrane shear traitement
      IF (IDRIL == 0) THEN
        CALL CBAFORCT(JFT     ,JLT  ,VOLG    ,X13  ,X24  ,
     2                Y13     ,Y24  ,GBUF%FOR,VF   ,NPLAT,
     3                IPLAT   ,OFF  ,NEL     )
C
        CALL CBAENERS(JFT   ,JLT      ,OFF       ,AREA ,THK0,
     .                VDEF  ,GBUF%FOR ,GBUF%EINT ,DT1  ,NEL )
      END IF
C
      IF (NPTTOT == 1) THEN
        CALL CBAVISNP1(JFT, JLT,VXYZ,RXYZ,VCORE,
     2                 AMU, OFF,RHO ,SSP ,AREA,THK0 ,
     3                 G  ,DT1 ,VF  ,
     4                 IPARTC,PARTSAV,KFTS)
      ENDIF
C----------------------------
C     TRANSFORME FORCES LOCALES AUX GLOBALES
C----------------------------
      CALL CBAPROJ(
     1            JFT    ,JLT    ,VQN    ,VQ     ,VF    ,
     2            VM     ,NPLAT   ,IPLAT   ,
     3            F11    ,F12    ,F13    ,F14    ,F21   ,
     4            F22    ,F23    ,F24    ,F31    ,F32   ,
     5            F33    ,F34    ,M11    ,M12    ,M13   ,
     6            M14    ,M21    ,M22    ,M23    ,M24   ,
     7            M31    ,M32    ,M33    ,M34    ,VCORE ,
     8            DD     ,VMZ    ,IDRIL  ,OFF    )
      IF( ISHPLYXFEM > 0 ) CALL CBAPROJ_PLY(  
     1            JFT   ,JLT   ,NPT     ,NPLAT  ,IPLAT ,VQN,
     2            VQ    ,PLY_FN ,VFI     ,VCORE  ,DD    ,
     6            FLY11  ,FLY12  ,FLY13 ,FLY14  ,FLY21  ,
     7            FLY22  ,FLY23  ,FLY24 ,FLY31  ,FLY32  ,
     8            FLY33  ,FLY34  ,OFF)
      IF (NPINCH > 0) THEN
        CALL CBAPINCHPROJ(
     1            JFT    ,JLT    ,VQN    ,VQ     ,VFPINCH,
     2            NPLAT  ,IPLAT  ,FP     ,VCORE  ,DD     ,THK0,
     3            VFPINCHDAMPX,VFPINCHDAMPY)
      ENDIF
C--------------------------
C     BILANS PAR MATERIAU
C--------------------------
       IPOUT=2
      IF(IPRI == 1)
     1   CALL CBILAN(
     1   JFT,        JLT,        PM,         V,
     2   IXC,        GBUF%THK,   GBUF%EINT,  PARTSAV,
     3   AREA,       MAT,        IPARTC,     X,
     4   VR,         BID,        BID,        BID,
     5   THK2,       IPOUT,      OFF,        NFT,
     6   GRESAV,     GRTH,       IGRTH,      VL1,
     7   VL2,        VL3,        VL4,        VRL1,
     8   VRL2,       VRL3,       VRL4,       X1G,
     9   X2G,        X3G,        X4G,        Y1G,
     A   Y2G,        Y3G,        Y4G,        Z1G,
     B   Z2G,        Z3G,        Z4G,        IBID,
     C   IEXPAN,     GBUF%EINTTH,ITASK,      GBUF%VOL,
     D   ACTIFXFEM,  IGRE)
C----------------------------
C     CALCUL  DT
C----------------------------
      IF(NPINCH > 0) THEN
C
        IF(MTN == 1) THEN
          MX = MAT(JFT)
          E = PM(20,MX)
          ANU = PM(21,MX)
          A11PINCH = E / (ONE-TWO*ANU)
        ELSEIF(MTN == 91) THEN
          MX = MAT(JFT)
          E = PM(20,MX)
          ANU = PM(21,MX)
          A11PINCH = E / (ONE-TWO*ANU)
        ENDIF                
C          
        CALL CNDT3PINCH(
     1        JFT    ,JLT    ,OFF      , DT2T   ,AMU     ,            
     2        NELTST ,ITYPTST,STI      , STIR   ,GBUF%OFF,   
     3        SSP    ,VISCMX  ,RHO     , VOLG   ,THK0,THK2,  
     4        A11    ,LC   ,ALPE       , NGL    ,ISMSTR,     
     5        IOFC   ,NNOD   ,AREA     , G      ,SHF   ,     
     6        MSC    ,DMELC  ,JSMS     , BID    ,IGTYP ,     
     7        IGMAT  ,A11R   ,GBUF%G_DT, GBUF%DT, A11PINCH)
C
      ELSE
C
        CALL CNDT3(
     1        JFT    ,JLT    ,OFF      , DT2T   ,AMU     ,            
     2        NELTST ,ITYPTST,STI      , STIR   ,GBUF%OFF,   
     3        SSP    ,VISCMX  ,RHO     , VOLG   ,THK0,THK2,  
     4        A11    ,LC   ,ALPE       , NGL    ,ISMSTR,     
     5        IOFC   ,NNOD   ,AREA     , G      ,SHF   ,     
     6        MSC    ,DMELC  ,JSMS     , BID    ,IGTYP ,     
     7        IGMAT  ,A11R   ,GBUF%G_DT, GBUF%DT,MTN   ,
     8        PM     ,MAT(JFT))
C
      ENDIF
C--------------------------
C     THERMAL TIME STEP
C--------------------------
        IF(JTHE > 0.AND.IDT_THERM == 1)THEN
           CALL DTTHERM(
     1   JFT,     JLT,     PM,      TEMPEL,
     2   GBUF%RE, RHO,     GBUF%RK, VOL0,
     3   LC,      MAT,     DT_THERM,OFF,
     4   CONDE,   JTUR)
         ENDIF 
C 
      IF(ISHPLYXFEM > 0) THEN                                     
        CALL CNDT_PLY(                                            
     .     JFT  ,JLT   ,NPT,OFF  , LC  ,AREA ,THKLY,              
     .     TH_IPLY    ,A11_PLY ,A11_IPLY,STI_PLY , OFFI,VISCMX)   
      ENDIF   
C--------------------------
C     NON-LOCAL TIME STEP
C--------------------------
      IF (INLOC > 0) THEN
        CALL DTCBA_REG(NLOC_DMG,THK0       ,NEL     ,GBUF%OFF,
     .                 LC      ,IXC(1,JFT) ,NDDL    ,DT2T    )
      ENDIF 
C--------------------------
C     ASSEMBLE
C--------------------------
      IF(IPARIT == 3)THEN
        CALL CUPDT3F(JFT    ,JLT  ,F   ,M   ,NVC  , 
     2            GBUF%OFF,OFF  ,STI ,STIR,STIFN,
     3            STIFR  ,IXC  ,PM  ,AREA ,GBUF%THK,
     4            F11    ,F12  ,F13 ,F14 ,F21  ,
     5            F22    ,F23  ,F24 ,F31 ,F32  ,
     6            F33    ,F34  ,M11 ,M12 ,M13  ,
     7            M14    ,M21  ,M22 ,M23 ,M24  ,
     8            M31    ,M32  ,M33 ,M34 ,GBUF%EINT,
     9            PARTSAV,MAT ,IPARTC)
      ELSEIF(IPARIT == 0)THEN
        CALL CUPDTN3(JFT   ,JLT  ,F   ,M   ,NVC  , 
     2            GBUF%OFF,OFF  ,STI ,STIR,STIFN,
     3            STIFR  ,IXC   ,PM  ,AREA ,GBUF%THK,
     4            F11    ,F12   ,F13 ,F14 ,F21  ,
     5            F22    ,F23   ,F24 ,F31 ,F32  ,
     6            F33    ,F34   ,M11 ,M12 ,M13  ,
     7            M14    ,M21   ,M22 ,M23 ,M24  ,
     8            M31    ,M32   ,M33 ,M34 ,GBUF%EINT,
     A            PARTSAV,MAT   ,IPARTC ,FACN ,JTHE,
     B            THEM , FTHE  ,CONDN  ,CONDE)
C       
        IF(NPINCH > 0) THEN
          CALL CUPDTN3PINCH(
     1                      JFT   ,JLT    ,NVC   ,IXC       ,
     2                      FP    ,FPINCH ,STI   ,STIFPINCH ,FACP )
        ENDIF
C
      ELSE
        CALL CUPDTN3P(JFT ,JLT  ,GBUF%OFF,OFF  ,STI,
     2            STIR   ,FSKY ,FSKY   ,IADC ,
     4            F11    ,F12  ,F13    ,F14  ,F21,
     5            F22    ,F23  ,F24    ,F31  ,F32,
     6            F33    ,F34  ,M11    ,M12  ,M13,
     7            M14    ,M21  ,M22    ,M23  ,M24,
     8            M31    ,M32  ,M33    ,M34  ,IXC,
     A            GBUF%EINT,PARTSAV,MAT,IPARTC,PM  ,
     B            AREA   ,GBUF%THK,FACN ,JTHE,THEM ,
     C            FTHESKY,CONDNSKY,CONDE )
      ENDIF
C     
      IF(ISHPLYXFEM > 0) THEN
        CALL CUPDT_PLY(
     1   JFT,       JLT,       NVC,       GBUF%OFF,
     2   OFF,       IADC_PXFEM,IEL_PXFEM, INOD_PXFEM,
     3   IXC,       MS,        IN,        MS_PLY,
     4   ZI_PLY,    ISTACK,    POSLY,     FLY11,
     5   FLY12,     FLY13,     FLY14,     FLY21,
     6   FLY22,     FLY23,     FLY24,     FLY31,
     7   FLY32,     FLY33,     FLY34,     FACN,
     8   STI_PLY,   MSZ2,      NFT,       NPT)
      ENDIF  
C------------
      IF (ALLOCATED(DIRB)) DEALLOCATE(DIRB) 
      IF (ALLOCATED(DIRA)) DEALLOCATE(DIRA)
      IF (ALLOCATED(VAR_REG))   DEALLOCATE(VAR_REG)
C
       IF(NPINCH > 0) THEN
         DEALLOCATE(PINCH_LOCAL%EPINCHXZ)
         DEALLOCATE(PINCH_LOCAL%EPINCHYZ)
         DEALLOCATE(PINCH_LOCAL%EPINCHZZ)
       ENDIF
C
C------------
      RETURN
      END
