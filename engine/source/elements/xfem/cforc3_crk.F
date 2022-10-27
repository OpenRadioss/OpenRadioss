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
Chd|  CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|-- called by -----------
Chd|        FORINTC                       source/elements/forintc.F     
Chd|-- calls ---------------
Chd|        CBILAN                        source/elements/shell/coque/cbilan.F
Chd|        CCOEF3                        source/elements/shell/coque/ccoef3.F
Chd|        CCOOR3_CRK                    source/elements/xfem/ccoor3_crk.F
Chd|        CCURV3                        source/elements/shell/coque/ccurv3.F
Chd|        CDEFO3                        source/elements/shell/coque/cdefo3.F
Chd|        CDERI3                        source/elements/shell/coque/cderi3.F
Chd|        CDLEN3                        source/elements/shell/coque/cdlen3.F
Chd|        CDT3                          source/elements/shell/coque/cdt3.F
Chd|        CFINT3                        source/elements/shell/coque/cfint3.F
Chd|        CHSTI3                        source/elements/shell/coque/chsti3.F
Chd|        CHVIS3                        source/elements/shell/coque/chvis3.F
Chd|        CMAIN3                        source/materials/mat_share/cmain3.F
Chd|        CNVEC3                        source/elements/shell/coque/cnvec3.F
Chd|        CPXPY3                        source/elements/shell/coque/cpxpy3.F
Chd|        CSENS3                        source/elements/shell/coque/csens3.F
Chd|        CSSP2A11                      source/elements/sh3n/coque3n/cssp2a11.F
Chd|        CSTRA3                        source/elements/shell/coque/cstra3.F
Chd|        CUPDT3_CRK                    source/elements/xfem/xfemfsky.F
Chd|        MHVIS3                        source/airbag/mhvis3.F        
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        TEMPCG                        source/materials/mat_share/tempcg.F
Chd|        THERMC                        source/materials/mat_share/thermc.F
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
       SUBROUTINE CFORC3_CRK(XFEM_STR,
     1            JFT    ,JLT    ,PM    ,IXC   ,X      ,
     2            F      ,M      ,V     ,VR    ,FAILWAVE,
     3            NVC    ,MTN    ,GEO   ,TF    ,NPF    ,
     4            BUFMAT ,PARTSAV,DT2T  ,NELTST,ITYPTST,
     5            STIFN  ,STIFR  ,FSKY  ,CRKSKY,IADC   ,
     6            D      ,DR     ,TANI  ,OFFSET,EANI   ,
     7            F11    ,F12    ,F13   ,F14   ,F21    ,
     8            F22    ,F23    ,F24   ,F31   ,F32    ,
     9            F33    ,F34    ,M11   ,M12   ,M13    ,
     A            M14    ,M21    ,M22   ,M23   ,M24    ,
     B            M31    ,M32    ,M33   ,M34   ,INDXOF ,
     C            IPARTC ,THKE   ,GROUP_PARAM,MATPARAM_TAB,
     F            NEL    ,ISTRAIN,IHBE  ,KFTS  ,
     G            ITHK   ,IOFC   ,IPLA  ,NFT   ,ISMSTR ,
     H            FZERO  ,IGEO   ,IPM   ,IFAILURE,ITASK ,
     I            JTHE   ,TEMP   , FTHE ,FTHESKY,IEXPAN,
     J            GRESAV ,GRTH   ,IGRTH ,MSC    ,DMELC ,
     K            JSMS  ,TABLE   ,IPARG ,IXFEM,INOD_CRK,
     L            IEL_CRK,IADC_CRK,ELCUTC,
     M            SENSOR_TAB,IXEL ,STACK   ,
     N            ISUBSTACK,UXINT_MEAN,UYINT_MEAN,UZINT_MEAN,NLEVXF,
     O            NODEDGE,CRKEDGE,DRAPE_Q4,IPRI  ,NLOC_DMG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE CRACKXFEM_MOD
      USE ELBUFDEF_MOD
      USE STACK_MOD
      USE FAILWAVE_MOD
      USE MATPARAM_DEF_MOD           
      USE DRAPE_MOD  
      USE NLOCAL_REG_MOD  
      USE SENSOR_MOD
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
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "com_xfem1.inc"
#include      "scr01_c.inc"
#include      "scr02_c.inc"
#include      "scr03_c.inc"
#include      "scr07_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
#include      "warn_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NVC,MTN,NELTST,ITYPTST,OFFSET,
     .   NEL,ISTRAIN,IHBE,ITHK,IOFC,IPLA,NFT,
     .   ISMSTR,KFTS,IFAILURE,IEXPAN,IXEL,NLEVXF,IPRI
      INTEGER NPF(*),IXC(NIXC,*),IADC(4,*),INDXOF(MVSIZ),IPARTC(*),
     .   IGEO(NPROPGI,*),IPM(NPROPMI,*),ITASK,JTHE, JSMS,
     .   GRTH(*),IGRTH(*),IPARG(*),IXFEM,INOD_CRK(*),IEL_CRK(*),
     .   IADC_CRK(4,*),ELCUTC(2,*),ISUBSTACK,NODEDGE(2,*)
C     REAL
      my_real
     .   F11(MVSIZ), F12(MVSIZ), F13(MVSIZ), F14(MVSIZ),
     .   F21(MVSIZ), F22(MVSIZ), F23(MVSIZ), F24(MVSIZ),
     .   F31(MVSIZ), F32(MVSIZ), F33(MVSIZ), F34(MVSIZ),
     .   M11(MVSIZ), M12(MVSIZ), M13(MVSIZ), M14(MVSIZ),
     .   M21(MVSIZ), M22(MVSIZ), M23(MVSIZ), M24(MVSIZ),
     .   M31(MVSIZ), M32(MVSIZ), M33(MVSIZ), M34(MVSIZ),
     .   PM(*), X(3,*), F(*), M(*), V(*), VR(*),
     .   GEO(NPROPG,*),TF(*), BUFMAT(*),PARTSAV(*),STIFN(*),STIFR(*),
     .   FSKY(8,*),D(*),DR(*),TANI(6,*),EANI(*), THKE(*),DT2T,
     .   FZERO(3,4,*),TEMP(*), FTHE(*),FTHESKY(*),GRESAV(*),
     .   MSC(*), DMELC(*),UXINT_MEAN(NLEVXF,MVSIZ),
     .   UYINT_MEAN(NLEVXF,MVSIZ),UZINT_MEAN(NLEVXF,MVSIZ)
      TYPE(TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET :: XFEM_STR
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
      TYPE (XFEM_SKY_)    , DIMENSION(*) :: CRKSKY
      TYPE (STACK_PLY) :: STACK
      TYPE (FAILWAVE_STR_) :: FAILWAVE 
      TYPE (GRP_PARAM_STRUCT_) :: GROUP_PARAM
      TYPE (DRAPE_) :: DRAPE_Q4
      TYPE (MATPARAM_STRUCT_) , DIMENSION(NUMMAT) :: MATPARAM_TAB
      TYPE (NLOCAL_STR_) :: NLOC_DMG 
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IGTYP,ICSEN,IXLAY,NXLAY,NLAYER,IFLAG,IUN,NPG,NPTT,IBID,
     .   IR,IS,IT,IPT,NG,L_DIRA,L_DIRB,J1,J2,ILEV,IGMAT,IPTHK,IREP
      INTEGER MAT(MVSIZ),PID(MVSIZ),NDT(MVSIZ),NGL(MVSIZ),FWAVE(MVSIZ)
C
      my_real 
     .   STI(MVSIZ),STIR(MVSIZ),SIGY(MVSIZ),RHO(MVSIZ),
     .   X2(MVSIZ),X3(MVSIZ),X4(MVSIZ),Y2(MVSIZ),Y3(MVSIZ),
     .   Y4(MVSIZ),Z2(MVSIZ),SSP(MVSIZ),VISCMX(MVSIZ),
     .   VX1(MVSIZ),VX2(MVSIZ),VX3(MVSIZ),VX4(MVSIZ),
     .   VY1(MVSIZ),VY2(MVSIZ),VY3(MVSIZ),VY4(MVSIZ),
     .   VZ1(MVSIZ),VZ2(MVSIZ),VZ3(MVSIZ),VZ4(MVSIZ),
     .   VX13(MVSIZ),VX24(MVSIZ),VY13(MVSIZ),VY24(MVSIZ),
     .   VZ13(MVSIZ),VZ24(MVSIZ),THK02(MVSIZ), 
     .   X1G(MVSIZ),X2G(MVSIZ),X3G(MVSIZ),X4G(MVSIZ),
     .   Y1G(MVSIZ),Y2G(MVSIZ),Y3G(MVSIZ),Y4G(MVSIZ),
     .   Z1G(MVSIZ),Z2G(MVSIZ),Z3G(MVSIZ),Z4G(MVSIZ),
     .   E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),E2X(MVSIZ),
     .   E2Y(MVSIZ),E2Z(MVSIZ),E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ)
      my_real 
     .   EXX(MVSIZ),EYY(MVSIZ),EXY(MVSIZ),EXZ(MVSIZ),EYZ(MVSIZ),
     .   KXX(MVSIZ),KYY(MVSIZ),KXY(MVSIZ),PX1(MVSIZ),
     .   PX2(MVSIZ),PY1(MVSIZ),PY2(MVSIZ),THK0(MVSIZ),
     .   OFF(MVSIZ),NU(MVSIZ),SHF(MVSIZ),AREA(MVSIZ),
     .   G(MVSIZ),YM(MVSIZ),A11(MVSIZ),A12(MVSIZ),
     .   VL1(MVSIZ,3),VL2(MVSIZ,3),VL3(MVSIZ,3),VL4(MVSIZ,3),
     .   VRL1(MVSIZ,3),VRL2(MVSIZ,3),VRL3(MVSIZ,3),VRL4(MVSIZ,3), 
     .   DT1C(MVSIZ),DT2C(MVSIZ),ALDT(MVSIZ),ALPE(MVSIZ),VHX(MVSIZ),
     .   VHY(MVSIZ),DSUB(MVSIZ,3,4),DRSUB(MVSIZ,3,4),TSUB(MVSIZ),
     .   DTCSUB(MVSIZ),AREAS(MVSIZ),A_I(MVSIZ)
      my_real 
     .   H1(MVSIZ),H2(MVSIZ),H3(MVSIZ),VOL0(MVSIZ),VOL00(MVSIZ),
     .   H11(MVSIZ),H12(MVSIZ),H13(MVSIZ),H14(MVSIZ),  
     .   H21(MVSIZ),H22(MVSIZ),H23(MVSIZ),H24(MVSIZ),  
     .   H31(MVSIZ),H32(MVSIZ),H33(MVSIZ),H34(MVSIZ),  
     .   B11(MVSIZ),B12(MVSIZ),B13(MVSIZ),B14(MVSIZ),  
     .   B21(MVSIZ),B22(MVSIZ),B23(MVSIZ),B24(MVSIZ),  
     .   RX1(MVSIZ),RX2(MVSIZ),RX3(MVSIZ),RX4(MVSIZ), 
     .   RY1(MVSIZ),RY2(MVSIZ),RY3(MVSIZ),RY4(MVSIZ),
     .   ZCFAC(MVSIZ,2),GS(MVSIZ),SRH1(MVSIZ),SRH2(MVSIZ),SRH3(MVSIZ),
     .   DIE(MVSIZ),TEMPEL(MVSIZ),THEM(MVSIZ,4),
     .   UX1(MVSIZ),UX2(MVSIZ),UX3(MVSIZ),UX4(MVSIZ),
     .   UY1(MVSIZ),UY2(MVSIZ),UY3(MVSIZ),UY4(MVSIZ),A11R(MVSIZ),
     .   THKE0(MVSIZ)
      my_real 
     .   BID,THKR
C---
      INTEGER, ALLOCATABLE, DIMENSION(:) :: ELCRKINI
      my_real, 
     .    ALLOCATABLE, DIMENSION(:) :: DIRA,DIRB,DIR1_CRK,DIR2_CRK
      my_real,
     .  DIMENSION(:) ,POINTER  ::  DIR_A,DIR_B
      TARGET :: DIRA,DIRB
C---
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF
      my_real,
     .  DIMENSION(:) ,POINTER  ::  OFFG,THKG,STRAG,FORG,MOMG,
     .  EINTG,EPSDG,TEMPG,EINTTH,HOURGG
      DOUBLE PRECISION,
     .  DIMENSION(:) ,POINTER  ::  SMSTRG
      my_real,
     .  DIMENSION(:,:), ALLOCATABLE :: VARNL
C=======================================================================
      IBID = 0
      IUN  = 1
      BID  = ZERO
      NPG  = 0
      NG   = 1
      IR   = 1
      IS   = 1
      IT   = 1
      ALLOCATE(VARNL(NEL,1))
      VARNL = ZERO
C -----will put it in starter     
      IF (ISMSTR>=10) ISMSTR=4
C
      DO I=JFT,JLT
        MAT(I) = IXC(1,I)
        PID(I) = IXC(6,I)
        NGL(I) = IXC(7,I)
      ENDDO
C
      ICSEN = IGEO(3,PID(1))
      IGTYP = IGEO(11,PID(1))
      IGMAT = IGEO(98,PID(1))
      IREP  = IPARG(35)
C-----------------------------------------
      GBUF => XFEM_STR%GBUF
      NXLAY = XFEM_STR%NLAY
      NLAYER = 1    ! only one current xfem layer passes to the cmain3 & mulawc
C-----------------------------------------
      ALLOCATE(ELCRKINI(NXLAY*NEL))
      ALLOCATE(DIR1_CRK(NXLAY*NEL))
      ALLOCATE(DIR2_CRK(NXLAY*NEL))
      ELCRKINI = 0
      DIR1_CRK = ZERO
      DIR2_CRK = ZERO
C
      L_DIRA = XFEM_STR%BUFLY(1)%LY_DIRA
      L_DIRB = XFEM_STR%BUFLY(1)%LY_DIRB
C
      ALLOCATE (DIRA(NXLAY*NEL*L_DIRA))
      ALLOCATE (DIRB(NXLAY*NEL*L_DIRB))
      DIRA=ZERO
      DIRB=ZERO
      DIR_A => DIRA(1:NXLAY*NEL*L_DIRA)
      DIR_B => DIRB(1:NXLAY*NEL*L_DIRB)
C
      DO IXLAY=1,NXLAY
        NPTT = XFEM_STR%BUFLY(IXLAY)%NPTT
        IF (L_DIRA == 0) THEN
          DIRA = ZERO
        ELSEIF (IREP == 0) THEN
          J1 = 1+(IXLAY-1)*L_DIRA*NEL
          J2 = IXLAY*L_DIRA*NEL
          DIRA(J1:J2) = XFEM_STR%BUFLY(IXLAY)%DIRA(1:NEL*L_DIRA)
        ENDIF
C
        DO I=JFT,JLT
          SIGY(I)    = EP30
          ZCFAC(I,1) = ONE
          ZCFAC(I,2) = ONE
          ALPE(I)    = ONE
          TEMPEL(I)  = ZERO
        ENDDO
C---
        ILEV = NXEL*(IXLAY-1) + IXEL
C---
        IF (IGTYP == 1 .or. IGTYP == 9) THEN
          DO I=JFT,JLT
            THKE0(I) = THKE(I)
          ENDDO
        ELSEIF (IGTYP == 51 .OR. IGTYP == 52) THEN
          IPTHK = 1 + NXLAY
          THKR  = STACK%GEO(IPTHK+IXLAY,ISUBSTACK)
          DO I=JFT,JLT
            THKE0(I) = THKE(I) * THKR     ! layer thickness (real)
          ENDDO
        ELSE    !  IGTYP == 11...
          IPTHK = 300
          DO I=JFT,JLT
            THKE0(I) = THKE(I) * GEO(IPTHK+IXLAY,PID(I))
          ENDDO
        ENDIF
c
        IF (NXLAY > 1) THEN
          LBUF => XFEM_STR%BUFLY(IXLAY)%LBUF(IR,IS,1)
          BUFLY  => XFEM_STR%BUFLY(IXLAY)
          OFFG   => LBUF%OFF
          SMSTRG => LBUF%SMSTR
          THKG   => LBUF%THK
          STRAG  => LBUF%STRA
          FORG   => LBUF%FOR
          MOMG   => LBUF%MOM
          EINTG  => LBUF%EINT
          EPSDG  => LBUF%EPSD
          TEMPG  => LBUF%TEMP
          EINTTH => LBUF%EINTTH
          HOURGG => BUFLY%HOURG
        ELSEIF (NXLAY == 1) THEN
          OFFG   => GBUF%OFF
          SMSTRG => GBUF%SMSTR
          THKG   => GBUF%THK
          STRAG  => GBUF%STRA
          FORG   => GBUF%FOR
          MOMG   => GBUF%MOM
          EINTG  => GBUF%EINT
          EPSDG  => GBUF%EPSD
          TEMPG  => GBUF%TEMP
          EINTTH => GBUF%EINTTH
          HOURGG => GBUF%HOURG
        ENDIF
C
        IF (ISUB == 0) THEN
          CALL CCOOR3_CRK(JFT     ,JLT  ,NFT   ,IXC    ,NGL     ,
     2                    PID     ,MAT  ,ILEV  ,IEL_CRK,IADC_CRK,
     3                    OFFG    ,OFF  ,SIGY  ,THKE0  ,THK0    ,
     4                    DT1C    ,VL1  ,VL2   ,VL3    ,VL4     ,
     5                    VRL1    ,VRL2 ,VRL3  ,VRL4   ,X1G     ,
     6                    X2G     ,X3G  ,X4G   ,Y1G    ,Y2G     ,
     7                    Y3G     ,Y4G  ,Z1G   ,Z2G    ,Z3G     ,
     8                    Z4G     )
        ELSE
C  a faire:
cc        CALL CSUBC3(JFT     ,JLT    ,X    ,IXC     ,GEO ,GBUF%OFF ,
cc     2              OFF     ,SIGY   ,PID  ,D       ,DR   ,DSUB,
cc     3            DRSUB,VL1,VL2,VL3,VL4,VRL1,VRL2,VRL3,VRL4,NDT ,PM ,V,
cc     4            GBUF%THK,GBUF%EINT,PARTSAV,TSUB,DTCSUB,AREAS,
cc     5            VR     ,MAT     ,NGL   ,DT1C   ,DT2C ,THKE,THK0,
cc     6            X1G    ,X2G     ,X3G   ,X4G    ,Y1G  ,Y2G    ,
cc     7            Y3G    ,Y4G     ,Z1G   ,Z2G    ,Z3G  ,Z4G    ,
cc     8            IPARTC ,NSG     ,IADC  ,FSKY   ,FSKY ,GRESAV   ,
cc     9            GRTH   ,IGRTH   )
cc        IF(JLT<JFT)RETURN
        ENDIF
c
        CALL CNVEC3(XFEM_STR,DIR_A   ,DIR_B   ,                     
     1     JFT     ,JLT      ,IREP    ,IGTYP   ,NXLAY   ,           
     2     X1G     ,X2G      ,X3G     ,X4G     ,Y1G     ,Y2G     ,  
     3     Y3G     ,Y4G      ,Z1G     ,Z2G     ,Z3G     ,Z4G     ,  
     4     E1X     ,E1Y      ,E1Z     ,E2X     ,E2Y     ,E2Z     ,  
     5     E3X     ,E3Y      ,E3Z     ,NEL     )                             
C
        IF (ISMSTR /= 3) THEN
          CALL CDERI3(
     1      JFT ,JLT ,SMSTRG,OFFG,STI ,STIR,
     2      AREA,PX1 ,PX2  ,PY1 ,PY2 ,X2  ,X3  ,X4  ,Y2  ,Y3  ,  
     3      Y4  ,Z2  ,X1G  ,X2G ,X3G ,X4G ,Y1G ,Y2G ,Y3G ,Y4G ,
     4      Z1G ,Z2G ,Z3G  ,Z4G ,E1X ,E1Y ,E1Z ,E2X ,E2Y ,E2Z ,
     5      E3X ,E3Y ,E3Z  ,VHX ,VHY ,A_I ,UX1 ,UX2 ,UX3 ,UX4 ,
     7      UY1 ,UY2 ,UY3  ,UY4 ,NEL )
        ELSE
          CALL CPXPY3(JFT       ,JLT    ,PM   ,STI  ,STIR,
     2                SMSTRG    ,PX1    ,PX2  ,PY1  ,PY2 ,
     3                IXC       ,AREA   ,X2   ,X3   ,X4  ,
     4                Y2        ,Y3     ,Y4   ,Z2   ,THK0,
     5                MAT       ,NEL    )
        ENDIF
C
        CALL CCOEF3(JFT     ,JLT    ,PM     ,MAT      ,GEO     ,
     2              PID     ,OFF    ,AREA   ,STI      ,STIR    ,
     3              SHF     ,THK0   ,THK02  ,NU       ,
     4              G       ,YM     ,A11    ,A12      ,THKG    ,
     5              SSP     ,RHO    ,H1     ,H2       ,H3      ,
     6              VOL0    ,VOL00  ,ALPE   ,GS       ,MTN     ,
     7              ITHK    ,ISMSTR  ,NPTT   ,KFTS   ,
     8              SRH1   ,SRH2   ,SRH3     ,IGEO    ,
     9              A11R    ,ISUBSTACK      ,STACK%PM)
C
        IF ((NODADT == 0 .AND. ISMSTR /= 3 .AND. IDT1SH == 0) .OR.
     .       IDTMIN(3) /= 0 .OR. IGTYP == 16)
c         CDLEN3 must be always called for tissue property/law for time step correction
     .    CALL CDLEN3(JFT     ,JLT     ,PM      ,OFF     ,AREA,
     .                X2      ,X3      ,X4      ,Y2      ,Y3  ,
     .                Y4      ,ALDT    ,MAT     ,GEO     ,PID ,
     .                IHBE    )
        CALL CDEFO3(JFT,JLT,VL1,VL2,VL3,VL4,DT1C,PX1,PX2,PY1,PY2,AREA,
     2              EXX,EYY,EXY,EXZ ,EYZ ,X2  ,X3 ,X4 ,Y2  ,Y3,
     3              Y4 ,Z2 ,VX1,VX2 ,VX3 ,VX4 ,VY1,VY2,VY3 ,VY4 ,
     4              VZ1,VZ2,VZ3,VZ4 ,E1X ,E1Y ,E1Z,E2X,E2Y ,E2Z ,
     5              E3X,E3Y,E3Z,IHBE)
        CALL CCURV3(JFT ,JLT ,VRL1,VRL2,VRL3,VRL4 ,PX1,
     1              PX2 ,PY1 ,PY2 ,AREA,
     2              RX1 ,RX2 ,RX3 ,RX4 ,RY1 ,RY2 ,RY3 ,RY4 ,
     3              E1X ,E1Y ,E1Z ,E2X ,E2Y ,E2Z ,E3X ,E3Y ,
     4              E3Z ,KXX ,KYY ,KXY ,EXZ ,EYZ ,
     5              BID ,ISMSTR   )
        CALL CSTRA3(JFT    ,JLT      ,STRAG ,SHF    ,AREA, 
     2              EXX    ,EYY      ,EXY      ,EXZ    ,EYZ ,   
     3              KXX    ,KYY      ,KXY      ,DT1C   ,TANI, 
     4              FORG   ,MOMG     ,ISMSTR   ,MTN ,
     6              IHBE   ,NFT      ,ISTRAIN  ,UX1    ,UX2 ,
     7              UX3    ,UX4      ,UY1      ,UY2    ,UY3 ,
     8              UY4    ,PX1      ,PX2      ,PY1    ,PY2 ,
     9              BID    ,BID      ,BID      ,NEL    )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
        DO I = JLT,JFT
          TEMPEL(I) = ZERO
        ENDDO
C
        IF (JTHE > 0) CALL TEMPCG(JFT  ,JLT   ,PM  ,MAT ,IXC,
     .                            TEMP ,TEMPEL)
C-----------------------------
        IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STARTIME(35,1)
C-----------------------------
        CALL CMAIN3(
     1    XFEM_STR  ,JFT       ,JLT       ,NFT       ,IPARG     ,
     2    NEL       ,MTN       ,IPLA      ,ITHK      ,GROUP_PARAM,
     3    PM        ,GEO       ,NPF       ,TF        ,BUFMAT    ,
     4    SSP       ,RHO       ,VISCMX    ,DT1C      ,SIGY      ,
     5    AREA      ,EXX       ,EYY       ,EXY       ,EXZ       ,
     6    EYZ       ,KXX       ,KYY       ,KXY       ,NU        ,
     7    OFF       ,THK0      ,MAT       ,PID       ,MATPARAM_TAB,
     8    FORG      ,MOMG      ,STRAG     ,FAILWAVE  ,FWAVE     ,
     9    THKG      ,EINTG     ,IOFC      ,
     A    G         ,A11       ,A12       ,VOL0      ,INDXOF    ,
     B    NGL       ,ZCFAC     ,SHF       ,GS        ,EPSDG     ,
     C    KFTS      ,IHBE      ,ALPE      ,
     D    DIR_A     ,DIR_B     ,IGEO      ,
     E    IPM       ,IFAILURE  ,NPG       ,
     F    TEMPEL    ,DIE       ,JTHE      ,IEXPAN    ,TEMPG     ,
     G    IBID      ,BID       ,
     H    BID       ,BID       ,BID       ,BID       ,BID       ,
     I    BID       ,BID       ,BID       ,E1X       ,E1Y       ,
     J    E1Z       ,E2X       ,E2Y       ,E2Z       ,E3X       ,
     K    E3Y       ,E3Z       ,IBID      ,TABLE     ,IXFEM     ,
     L    BID       ,SENSOR_TAB,BID       ,ELCRKINI  ,
     M    DIR1_CRK  ,DIR2_CRK  ,ALDT      ,
     N    ISMSTR    ,IR        ,IS        ,NLAYER    ,NPTT      ,
     O    IXLAY     ,IXEL      ,ISUBSTACK ,STACK     ,
     R    BID       ,ITASK     ,DRAPE_Q4  ,VARNL     ,NLOC_DMG  )
C-----------------------------
        IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STOPTIME(35,1)
C-----------------------------
C       FORCES ANTI-SABLIER
C-----------------------------
      DO I=JFT,JLT
        VISCMX(I) = SQRT(ONE + VISCMX(I)*VISCMX(I)) - VISCMX(I)
      ENDDO
      IF (NODADT /= 0 .AND. MTN ==58 ) 
     .            CALL CSSP2A11(PM,MAT(JFT),SSP ,A11  ,JLT   )
c---------------------------------------------------
        IF (NPTT == 1) THEN
          CALL MHVIS3(JFT    ,JLT    ,PM     ,THKG    ,HOURGG    ,
     2                OFF    ,PX1    ,PX2    ,PY1     ,PY2       ,
     3                IXC    ,DT1C   ,SSP    ,RHO     ,STI       ,
     4                EANI   ,GEO    ,PID    ,STIR    ,MAT       ,
     5                THK0   ,VISCMX ,ALPE   ,IPARTC  ,PARTSAV   ,
     6                IHBE   ,NFT    ,ISMSTR  ,RX1       ,
     7                RX2    ,RX3    ,RX4    ,RY1     ,RY2       ,
     8                RY3    ,RY4    ,VX1    ,VX2     ,VX3       ,
     9                VX4    ,VY1    ,VY2    ,VY3     ,VY4       ,
     A                VZ1    ,VZ2    ,VZ3    ,VZ4     ,B11       ,
     B                B12    ,B13    ,B14    ,B21     ,B22       ,
     C                B23    ,B24    ,AREA   ,YM      ,NU        ,
     D                VHX    ,VHY    ,H11    ,H12     ,H13       ,
     E                H14    ,H21    ,H22    ,H23     ,H24       ,
     F                H31    ,H32    ,H33    ,H34     ,H1        ,
     G                H2     ,IGEO   ,NEL    ,MTN     ,A11      )
        ELSEIF (IHBE == 2) THEN
          CALL CHSTI3(
     1            JFT    ,JLT    ,THKG   ,HOURGG ,OFF   ,PX1    ,
     2            PX2    ,PY1    ,PY2    ,SIGY   ,IXC   ,DT1C   ,
     3            SSP    ,RHO    ,STI    ,Z2     ,EANI  ,STIR   ,
     4            SHF    ,THK0   ,THK02  ,VISCMX ,G     ,A11    ,
     5            H1     ,H2     ,H3     ,YM     ,NU    ,ALPE   , 
     6            VHX    ,VHY    ,VX1    ,VX2    ,VX3   ,VX4    ,VY1,
     7            VY2 ,VY3 ,VY4,VZ1  ,VZ2  ,VZ3 ,VZ4 ,AREA   ,
     8            H11 ,H12 ,H13 ,H21 ,H22 ,H23  ,H31  ,H32  ,H33  ,
     9            B11 ,B12 ,B13 ,B14 ,B21 ,B22  ,B23  ,B24  ,
     A            RX1 ,RX2 ,RX3 ,RX4 ,RY1 ,RY2  ,RY3  ,RY4, 
     B            IPARTC,PARTSAV,
     C            IHBE   ,NFT     ,ISMSTR    ,SRH3, IGTYP ,
     D            IGMAT  ,A11R,  NEL)
        ELSE
          CALL CHVIS3(
     2      JFT ,JLT ,THKG,HOURGG,OFF,PX1 ,PX2 ,PY1 ,PY2 ,
     3      IXC ,DT1C,SSP,RHO  ,STI  ,VX1 ,VX2 ,VX3 ,VX4 ,VY1 ,
     4      VY2 ,VY3 ,VY4,VZ1  ,VZ2  ,VZ3 ,VZ4 ,AREA,THK0,VHX ,
     5      VHY ,SHF ,Z2 ,EANI ,STIR,VISCMX,G  ,A11 ,
     6      H1  ,H2  ,H3 ,YM   ,NU  ,THK02,ALPE,H11 ,
     7      H12 ,H13 ,H21 ,H22 ,H23 ,H31  ,H32 ,H33 ,
     8      B11 ,B12 ,B13 ,B14 ,B21 ,B22  ,B23 ,B24 ,
     9      RX1 ,RX2 ,RX3 ,RX4 ,RY1 ,RY2  ,RY3 ,RY4 , 
     A      IPARTC,PARTSAV,
     B      IHBE   ,NFT     ,ISMSTR,KFTS   ,
     C      SRH1, SRH2, SRH3   , IGTYP ,
     D      IGMAT  ,A11R ,NEL)
        ENDIF
C--------------------------
C       BILANS PAR MATERIAU
C--------------------------
c        IFLAG = MOD(NCYCLE,NCPRI)
        IF (IPRI>0) THEN
          CALL CBILAN(JFT    ,JLT       ,PM     ,V     ,IXC   ,
     2                THKG   ,EINTG     ,PARTSAV,AREA  ,MAT   ,
     3                IPARTC ,X         ,VR     ,VOL0  ,VOL00 ,
     4                THK0   ,THK02     ,IUN    ,OFF   ,
     5                NFT    ,GRESAV    ,GRTH   ,IGRTH ,
     6                VL1    ,VL2       ,VL3    ,VL4   ,VRL1  ,
     7                VRL2   ,VRL3      ,VRL4   ,X1G   ,X2G   ,
     8                X3G    ,X4G       ,Y1G    ,Y2G   ,Y3G   ,
     9                Y4G    ,Z1G       ,Z2G    ,Z3G   ,Z4G   ,
     A                IXFEM  ,IEXPAN    ,EINTTH    ,ITASK)
       ENDIF
C--------------------------
C     PAS DE TEMPS
C--------------------------
       IF (ISMSTR /= 3 .AND.(NODADT == 0 .OR. IDTMIN(3) /= 0)) THEN 
         CALL CDT3(JFT     ,JLT    ,YM        ,OFF    ,DT2T    ,        
     2             NELTST  ,ITYPTST,STI       ,STIR   ,OFFG    ,        
     3             DTCSUB  ,NDT    ,DT2C      ,IXC    ,SSP     ,        
     4             VISCMX  ,PX1    ,PX2       ,PY1    ,PY2     ,        
     5             VOL0    ,VOL00  ,RHO       ,ALDT   ,ALPE    ,        
     6             INDXOF  ,NGL    ,ISMSTR    ,IOFC   ,MSC     ,        
     7             DMELC   ,JSMS   ,GBUF%G_DT ,GBUF%DT)                                
       ENDIF                                                        
C----------------------------
C     FORCES INTERNES + ASSEMBLE
C----------------------------
        CALL CFINT3(JFT    ,JLT    ,FORG    ,MOMG    ,THK0   ,THK02  ,
     2              PX1    ,PX2    ,PY1     ,PY2     ,AREA   ,Z2     ,
     3              F11    ,F12    ,F13     ,F14     ,F21    ,F22    ,
     4              F23    ,F24    ,F31     ,F32     ,F33    ,F34    ,
     5              H11    ,H12    ,H13     ,H21     ,H22    ,H23    ,
     6              H31    ,H32    ,H33     ,B11     ,B12    ,B13    ,
     7              B14    ,B21    ,B22     ,B23     ,B24    ,NEL    ,
     8              M11    ,M12    ,M13     ,M14     ,M21    ,M22    , 
     9              M23    ,M24    ,M31     ,M32     ,M33    ,M34    ,
     A              E1X    ,E1Y    ,E1Z     ,E2X     ,E2Y    ,E2Z    ,
     B              E3X    ,E3Y    ,E3Z     ,IHBE    ,NPTT    ,FZERO  )
C-------------------------
c     Thermique des coques 
C--------------------------
        IF (JTHE > 0) THEN
          CALL THERMC(JFT   ,JLT   ,PM     ,MAT     ,THK0  ,IXC     ,
     .                PX1   ,PX2   ,PY1    ,PY2     ,AREA  ,DT1C    ,
     .                TEMP  ,TEMPEL,DIE   , THEM) 
        ENDIF
C--------------------------
C     THERMAL TIME STEP       --- to be added --- (see cforc3.F)
C--------------------------
C
C a faire:
cc      IF(ISUB /= 0)THEN
cc        CALL CSUBF3(JFT  ,JLT  ,NDT ,AREAS,AREA,
cc     4            F11    ,F12  ,F13 ,F14  ,F21 ,
cc     5            F22    ,F23  ,F24 ,F31  ,F32 ,
cc     6            F33    ,F34  ,M11 ,M12  ,M13 ,
cc     7            M14    ,M21  ,M22 ,M23  ,M24 ,
cc     8            M31    ,M32  ,M33 ,M34  )
cc      ENDIF
C
        IF (ICSEN > 0)
     .    CALL CSENS3(JFT    ,JLT    ,PID    ,IGEO   ,EPSDG)
C--------------------------
C     ASSEMBLE
C--------------------------
        IF (IPARIT == 1)
     .    CALL CUPDT3_CRK(
     .         JFT  ,JLT  ,NFT  ,IXC   ,OFF     ,IADC   ,
     .         F11  ,F21  ,F31  ,F12   ,F22     ,F32    ,
     .         F13  ,F23  ,F33  ,F14   ,F24     ,F34    ,
     .         M11  ,M21  ,M31  ,M12   ,M22     ,M32    ,
     .         M13  ,M23  ,M33  ,M14   ,M24     ,M34    ,
     .         STI  ,STIR ,FSKY ,ELCUTC,IADC_CRK,IEL_CRK,
     .         ILEV ,INOD_CRK,OFFG,EINTG,PARTSAV,IPARTC,
     .         IXLAY,CRKSKY)
C-------------------------
      ENDDO  !  DO IXLAY=1,NXLAY
C-------------------------
      IF (ALLOCATED(DIRA))     DEALLOCATE(DIRA)
      IF (ALLOCATED(DIRB))     DEALLOCATE(DIRB)
      IF (ALLOCATED(ELCRKINI)) DEALLOCATE(ELCRKINI)
      IF (ALLOCATED(DIR1_CRK)) DEALLOCATE(DIR1_CRK)
      IF (ALLOCATED(DIR2_CRK)) DEALLOCATE(DIR2_CRK)
      IF (ALLOCATED(VARNL))    DEALLOCATE(VARNL)
C------------------------- 
      RETURN
      END
