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
Chd|  CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|-- called by -----------
Chd|        FORINTC                       source/elements/forintc.F     
Chd|-- calls ---------------
Chd|        CBILAN                        source/elements/shell/coque/cbilan.F
Chd|        CCOOR3Z_CRK                   source/elements/xfem/ccoor3z_crk.F
Chd|        CMAIN3                        source/materials/mat_share/cmain3.F
Chd|        CNCOEF3B                      source/elements/sh3n/coquedk/cncoef3.F
Chd|        CNCOEFORT                     source/elements/sh3n/coquedk/cncoef3.F
Chd|        CNDT3                         source/elements/sh3n/coquedk/cndt3.F
Chd|        CUPDTN3_CRK                   source/elements/xfem/xfemfsky.F
Chd|        CZCORC1                       source/elements/shell/coquez/czcorc.F
Chd|        CZDEF                         source/elements/shell/coquez/czdef.F
Chd|        CZDEFRZ                       source/elements/shell/coquez/czdef.F
Chd|        CZFINTCE                      source/elements/shell/coquez/czfintce.F
Chd|        CZFINTCRZ                     source/elements/shell/coquez/czfintce.F
Chd|        CZFINTN1                      source/elements/shell/coquez/czfintn.F
Chd|        CZFINTNM                      source/elements/shell/coquez/czfintn.F
Chd|        CZFINTNRZ_OR                  source/elements/shell/coquez/czfintn.F
Chd|        CZFINTN_OR                    source/elements/shell/coquez/czfintn.F
Chd|        CZPROJ1                       source/elements/shell/coquez/czproj.F
Chd|        CZSTRA3                       source/elements/shell/coquez/czstra3.F
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
       SUBROUTINE CZFORC3_CRK(XFEM_STR   ,
     1            JFT    ,JLT    ,NFT    ,ITYPTST,
     2            IPARI  ,MTN    ,IPRI   ,ITHK   ,NELTST ,
     3            ISTRAIN,IPLA   ,TT     ,DT1    ,DT2T   ,
     4            PM     ,GEO    ,PARTSAV,IXC    ,GROUP_PARAM,
     5            BUFMAT ,TF     ,NPF    ,IADC   ,FAILWAVE,
     6            X      ,D      ,DR     ,V      ,VR     ,
     7            F      ,M      ,STIFN  ,STIFR  ,FSKY   ,
     8            TANI   ,OFFSET ,EANI   ,INDXOF ,
     9            IPARTC ,THKE   ,NVC    ,IOFC   ,IHBE   ,
     A            F11    ,F12    ,F13    ,F14    ,F21    ,
     B            F22    ,F23    ,F24    ,F31    ,F32    ,
     C            F33    ,F34    ,M11    ,M12    ,M13    ,
     D            M14    ,M21    ,M22    ,M23    ,M24    ,
     E            M31    ,M32    ,M33    ,M34    ,
     F            KFTS   ,FZERO  ,ISMSTR ,MATPARAM_TAB,
     I            IGEO   ,IPM    ,IFAILURE,ITASK , JTHE  ,
     J            TEMP   ,FTHE   ,FTHESKY ,IEXPAN,GRESAV ,
     K            GRTH   ,IGRTH  ,MSC    ,DMELC  ,JSMS   ,
     L            TABLE  ,IPARG  ,IXFEM ,INOD_CRK,IEL_CRK,
     M            IADC_CRK,ELCUTC,CRKSKY,
     N            LSENSOR,SENSOR ,IXEL    ,
     K            ISUBSTACK,UXINT_MEAN,UYINT_MEAN,UZINT_MEAN,NLEVXF,
     L            NODEDGE,CRKEDGE, STACK ,DRAPE_Q4,NLOC_DMG)
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
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G L O B A L   P A R A M E T E R S
C-----------------------------------------------
#include      "param_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C O M M O N   B L O C K S
C-----------------------------------------------
#include      "com_xfem1.inc"
#include      "scr01_c.inc"
#include      "scr14_c.inc"
#include      "parit_c.inc"
#include      "scr17_c.inc"
#include      "timeri_c.inc"
#include      "warn_c.inc"
#include      "comlock.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT ,JLT,NFT,IPARI,MTN,IPRI,ITHK,NELTST,
     .        ITYPTST,ISTRAIN,IPLA,OFFSET,NVC,JSMS,IOFC,IHBE,
     .        KFTS,ISMSTR,IFAILURE,JTHE,IXEL,ISUBSTACK,NLEVXF
      INTEGER IXC(NIXC,*),IADC(4,*),IPARTC(*),NPF(*),IGEO(NPROPGI,*),
     .        IPM(NPROPMI,*),INDXOF(MVSIZ),ITASK,IEXPAN,GRTH(*),IGRTH(*),
     .        IPARG(*),IXFEM,INOD_CRK(*),IEL_CRK(*),IADC_CRK(4,*),
     .        ELCUTC(2,*),LSENSOR(NSENSI,*),NODEDGE(2,*)
C     REAL OU REAL*8
      my_real
     .   F11(MVSIZ),F12(MVSIZ),F13(MVSIZ),F14(MVSIZ),
     .   F21(MVSIZ),F22(MVSIZ),F23(MVSIZ),F24(MVSIZ),
     .   F31(MVSIZ),F32(MVSIZ),F33(MVSIZ),F34(MVSIZ),
     .   M11(MVSIZ),M12(MVSIZ),M13(MVSIZ),M14(MVSIZ),
     .   M21(MVSIZ),M22(MVSIZ),M23(MVSIZ),M24(MVSIZ),
     .   M31(MVSIZ),M32(MVSIZ),M33(MVSIZ),M34(MVSIZ),
     .   TF(*),PM(NPROPM,*),GEO(NPROPG,*),PARTSAV(*),
     .   BUFMAT(*),X(3,*),D(*),DR(*),V(3,*),VR(3,*),
     .   F(3,*),M(3,*),STIFN(*), STIFR(*),FSKY(8,*),TANI(6,*),
     .   EANI(*),THKE(*),FZERO(3,4,*),TEMP(*),FTHE(*),
     .   FTHESKY(*),GRESAV(*),MSC(*), DMELC(*),SENSOR(NSENSR,*),
     .   UXINT_MEAN(NLEVXF,MVSIZ),
     .   UYINT_MEAN(NLEVXF,MVSIZ),UZINT_MEAN(NLEVXF,MVSIZ)
      my_real
     .   TT,DT1,DT2T
      TARGET :: BUFMAT
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
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C   L O C A L   V A R I A B L E S
C--------------------------------
      LOGICAL PLAT(MVSIZ)
      INTEGER
     .   I,J,NEL,MX,NNOD,NPG,IBID,ISROT,IXLAY,NXLAY,NLAYER,NPTT,
     .   IR,IS,IT,IPT,NG,L_DIRA,L_DIRB,J1,J2,IXFEM2,IPTHK,
     .   IORTH,ILEV,IGMAT,IGTYP,IREP,NUPARAM,IADBUF,IMAT
      INTEGER  MAT(MVSIZ),PID(MVSIZ),NGL(MVSIZ),FWAVE(MVSIZ)
      PARAMETER (NNOD = 4)
      my_real
     .  RH0(MVSIZ),LL(MVSIZ),VL13(MVSIZ,3),VL24(MVSIZ,3),VLHI(MVSIZ,3),
     .  RLXYZ(MVSIZ,8),X13(MVSIZ),X24(MVSIZ),Y13(MVSIZ),Y24(MVSIZ),
     .  MX13(MVSIZ),MX23(MVSIZ),MX34(MVSIZ),MY13(MVSIZ),MY23(MVSIZ),
     .  MY34(MVSIZ),Z1(MVSIZ),Z2(MVSIZ)
      my_real
     .   VQ(MVSIZ,9),VDEF(MVSIZ,8),OFF(MVSIZ),DHG(MVSIZ,6),
     .   AREA(MVSIZ),VQN(MVSIZ,12),L13(MVSIZ),L24(MVSIZ),
     .   ZCFAC(MVSIZ,2),A_I(MVSIZ),THK02(MVSIZ)
      my_real
     .   EXX(MVSIZ),EYY(MVSIZ),EXY(MVSIZ),EXZ(MVSIZ),EYZ(MVSIZ),
     .   KXX(MVSIZ),KYY(MVSIZ),KXY(MVSIZ),SIGY(MVSIZ),
     .   DT1C(MVSIZ),SSP(MVSIZ),VISCMX(MVSIZ),RHO(MVSIZ) ,
     .   NU(MVSIZ),G(MVSIZ),A11(MVSIZ),A12(MVSIZ),VOL0(MVSIZ),
     .   THK0(MVSIZ),STI(MVSIZ),STIR(MVSIZ),SHF(MVSIZ) ,
     .   VF(MVSIZ,12),VM(MVSIZ,8),GS(MVSIZ),FAC1(MVSIZ),
     .   ALPE(MVSIZ),YM(MVSIZ),FACN(MVSIZ,2),LXYZ(MVSIZ,8),
     .   DD(MVSIZ,6),DB(MVSIZ,12),AMU(MVSIZ),GSR(MVSIZ),
     .   A11SR(MVSIZ),A12SR(MVSIZ),NUSR(MVSIZ),SHFSR(MVSIZ),
     .   PX2(MVSIZ),PY1(MVSIZ),PY2(MVSIZ),DIE(MVSIZ),TEMPEL(MVSIZ),
     .   THEM(MVSIZ,4),R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),R31(MVSIZ),
     .   R32(MVSIZ),R33(MVSIZ),RLZ(MVSIZ,4),VHGZK(MVSIZ,5),
     .   VHGZE(MVSIZ,5),VRLZ(MVSIZ),BM0RZ(MVSIZ,4,4),BMKRZ(MVSIZ,4,4),
     .   BMERZ(MVSIZ,4,4),VMZ(MVSIZ,4),KRZ(MVSIZ),DIZ(MVSIZ,3)
      my_real
     .   X1G(MVSIZ),X2G(MVSIZ),X3G(MVSIZ),X4G(MVSIZ),
     .   Y1G(MVSIZ),Y2G(MVSIZ),Y3G(MVSIZ),Y4G(MVSIZ),
     .   Z1G(MVSIZ),Z2G(MVSIZ),Z3G(MVSIZ),Z4G(MVSIZ),
     .   VL1(MVSIZ,3),VL2(MVSIZ,3),VL3(MVSIZ,3),VL4(MVSIZ,3),
     .   VRL1(MVSIZ,3),VRL2(MVSIZ,3),VRL3(MVSIZ,3),VRL4(MVSIZ,3),
     .   VX1(MVSIZ),VX2(MVSIZ),VX3(MVSIZ),VX4(MVSIZ),
     .   VY1(MVSIZ),VY2(MVSIZ),VY3(MVSIZ),VY4(MVSIZ),
     .   VZ1(MVSIZ),VZ2(MVSIZ),VZ3(MVSIZ),VZ4(MVSIZ),
     .   VRX1(MVSIZ),VRX2(MVSIZ),VRX3(MVSIZ),VRX4(MVSIZ),
     .   VRY1(MVSIZ),VRY2(MVSIZ),VRY3(MVSIZ),VRY4(MVSIZ),
     .   VRZ1(MVSIZ),VRZ2(MVSIZ),VRZ3(MVSIZ),VRZ4(MVSIZ),
     .   UX1(MVSIZ),UX2(MVSIZ),UX3(MVSIZ),UX4(MVSIZ),
     .   UY1(MVSIZ),UY2(MVSIZ),UY3(MVSIZ),UY4(MVSIZ),A11R(MVSIZ),
     .   THKE0(MVSIZ),XL2(MVSIZ),XL3(MVSIZ),XL4(MVSIZ),
     .   YL2(MVSIZ),YL3(MVSIZ),YL4(MVSIZ),FAC58(MVSIZ,2)
      my_real  BID,THKR
C---
      INTEGER, ALLOCATABLE, DIMENSION(:) :: ELCRKINI
      my_real,
     .    ALLOCATABLE, DIMENSION(:) :: DIRA,DIRB,DIR1_CRK,DIR2_CRK
      my_real,
     .  DIMENSION(:) ,POINTER  ::  DIR_A,DIR_B,UPARAM,UVAR
      TARGET :: DIRA,DIRB
C---
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF
      my_real
     .   HM(MVSIZ,6), HF(MVSIZ,6), HC(MVSIZ,2),HMFOR(MVSIZ,6)
      my_real,
     .  DIMENSION(:) ,POINTER  ::  OFFG,THKG,STRAG,FORG,MOMG,
     .  EINTG,EPSDG,TEMPG,EINTTH,HOURGG
      DOUBLE PRECISION,
     .  DIMENSION(:) ,POINTER  ::  SMSTRG
      my_real,
     .  DIMENSION(:,:), ALLOCATABLE :: VARNL
C=======================================================================
      NEL  = JLT-JFT+1
      BID  = ZERO
      IBID = 0
      NPG  = 0
      NG   = 1
      IR   = 1
      IS   = 1
      IT   = 1
      ALLOCATE(VARNL(NEL,1))
      VARNL = ZERO
C -----will put it in starter     
      IF (ISMST R>= 10) ISMSTR=4
C
      DO I=JFT,JLT
        MAT(I) = IXC(1,I)
        PID(I) = IXC(6,I)
        NGL(I) = IXC(7,I)
      ENDDO
C
      DO I=JFT,JLT
        FAC1(I) = GEO(17,PID(I))
      ENDDO
C      
      ISROT = IPARG(41)
      IGTYP = IGEO(11,PID(1))
      IGMAT = IGEO(98,PID(1))
      IREP  = IPARG(35)
      IMAT  = IXC(1,JFT)
      IADBUF = MAX(1,IPM(7,IMAT))
      NUPARAM= IPM(9,IMAT)
      UPARAM => BUFMAT(IADBUF:IADBUF+NUPARAM)
C-----------------------------------------
      GBUF => XFEM_STR%GBUF
C
      NXLAY  = XFEM_STR%NLAY
      NLAYER = 1    ! only one current xfem layer passes to the cmain3 & mulawc
C-----------------------------------------
      IXFEM2 = 1               ! flag pour CZCORC1 et BILAN
C
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
      ALLOCATE(DIRA(NXLAY*NEL*L_DIRA))
      ALLOCATE(DIRB(NXLAY*NEL*L_DIRB))
      DIRA = ZERO
      DIRB = ZERO
      DIR_A => DIRA(1:NXLAY*NEL*L_DIRA)
      DIR_B => DIRB(1:NXLAY*NEL*L_DIRB)
      UVAR  => XFEM_STR%BUFLY(1)%MAT(IR,IS,IT)%VAR
c-------------------------------------------------
c     Loop over all (Xfem) layers
c-------------------------------------------------
      DO IXLAY = 1,NXLAY
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
          TEMPEL(I) = ZERO
        ENDDO
C-----------------------------------------
C       CALCULS PRELIMINAIRES GLOBAL-->LOCAL
C-----------------------------------------
        ILEV = NXEL*(IXLAY-1) + IXEL
C---
C
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
        ELSE       !  IGTYP == 11...
          IPTHK = 300
          THKR  = GEO(IPTHK+IXLAY,PID(1))
          DO I=JFT,JLT
            THKE0(I) = THKE(I) * THKR
          ENDDO
        ENDIF
c
        IF (NXLAY > 1) THEN
          LBUF   => XFEM_STR%BUFLY(IXLAY)%LBUF(IR,IS,IT)
          BUFLY  => XFEM_STR%BUFLY(IXLAY)
          OFFG   => LBUF%OFF
          SMSTRG => LBUF%SMSTR
          THKG   => LBUF%THK     ! layer thickness, not integration pt !
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
c
C       ! calcul coords + vit des elements fantoms au lieu de std
        CALL CCOOR3Z_CRK(JFT  ,JLT  ,NFT  ,IEL_CRK,IADC_CRK,
     2                   VL1  ,VL2  ,VL3  ,VL4    ,VRL1    ,
     3                   VRL2 ,VRL3 ,VRL4 ,X1G    ,X2G     ,
     4                   X3G  ,X4G  ,Y1G  ,Y2G    ,Y3G     ,
     5                   Y4G  ,Z1G  ,Z2G  ,Z3G    ,Z4G     ,
     6                   VX1  ,VX2  ,VX3  ,VX4    ,VY1     ,
     7                   VY2  ,VY3  ,VY4  ,VZ1    ,VZ2     ,
     8                   VZ3  ,VZ4  ,VRX1 ,VRX2   ,VRX3    ,
     9                   VRX4 ,VRY1 ,VRY2 ,VRY3   ,VRY4    ,
     A                   VRZ1 ,VRZ2 ,VRZ3 ,VRZ4   ,ILEV    ,
     B                   OFFG )
C
        CALL CZCORC1(XFEM_STR,
     1               JFT    ,JLT    ,X       ,V       ,VR     ,
     2               IXC    ,PM     ,PLAT    ,AREA    ,
     3               A_I    ,VL13   ,VL24    ,VLHI    ,RLXYZ  ,
     4               VQN    ,VQ     ,LL      ,L13     ,L24    ,
     5               X13    ,X24    ,Y13     ,Y24     ,MX13   ,
     6               MX23   ,MX34   ,MY13    ,MY23    ,MY34   ,
     7               Z1     ,LXYZ   ,DD      ,DB      ,SMSTRG  ,
     9               IREP   ,NPTT   ,NXLAY   ,ISMSTR  ,
     A               DIR_A  ,DIR_B  ,OFFG    ,RLXYZ   ,LXYZ   ,
     B               FACN   ,PY1    ,PX2     ,PY2     ,R11    ,
     C               R12    ,R13    ,R21     ,R22     ,R23    ,
     D               R31    ,R32    ,R33     ,RLZ     ,ISROT  ,
     E               IXFEM2 ,VX1    ,VX2     ,VX3     ,VX4    ,
     F               VY1    ,VY2    ,VY3     ,VY4     ,VZ1    ,
     G               VZ2    ,VZ3    ,VZ4     ,VRX1    ,VRX2   ,
     H               VRX3   ,VRX4   ,VRY1    ,VRY2    ,VRY3   ,
     I               VRY4   ,VRZ1   ,VRZ2    ,VRZ3    ,VRZ4   ,
     J               X1G    ,X2G    ,X3G     ,X4G     ,Y1G    ,
     K               Y2G    ,Y3G    ,Y4G     ,Z1G     ,Z2G    ,
     L               Z3G    ,Z4G    ,THKE0   ,DIZ     ,UX1    ,
     M               UX2    ,UX3    ,UX4     ,UY1     ,UY2    ,
     N               UY3    ,UY4    ,XL2     ,XL3     ,XL4    ,
     O               YL2    ,YL3    ,YL4     ,VL1     ,VL2    ,
     P               VL3    ,VL4    ,NEL     ,Z2      )
C
        CALL CNCOEF3B(JFT    ,JLT     ,PM      ,MAT    ,GEO    ,
     2                PID    ,AREA    ,SHF     ,THK0   ,
     3                THK02  ,NU      ,G       ,YM     ,
     4                A11    ,A12     ,THKG    ,THKE0  ,SSP    ,
     5                RHO    ,VOL0    ,GS      ,MTN    ,ITHK   ,
     6                NPTT   ,DT1C    ,DT1     ,IHBE   ,AMU    ,
     7                GSR    ,A11SR   ,A12SR   ,NUSR   ,SHFSR  ,
     8                KRZ    ,IGEO    ,A11R  ,ISUBSTACK, STACK%PM,
     9                UPARAM ,DIRA    ,DIRB    ,UVAR   ,FAC58  )
C
        CALL CNCOEFORT(JFT    ,JLT    ,PM       ,MAT     ,GEO    ,
     1                 PID    ,MTN    ,NPTT     ,HM      ,HF     ,
     2                 HC     ,HMFOR  ,IORTH    ,DIR_A   ,IGEO   ,
     3                 ISUBSTACK,STACK,XFEM_STR ,NXLAY   ,THKG   ,
     4                 DRAPE_Q4 ,NFT  ,NEL      )
C----------------------------------
C     CALCUL VITESSE DE DEFORMATION 
C----------------------------------
        CALL CZDEF(JFT     ,JLT  ,AREA ,A_I  ,VL13   ,VL24  ,
     2             VLHI    ,RLXYZ,VDEF ,DHG  ,X13    ,
     3             X24     ,Y13  ,Y24  ,MX13 ,MX23   ,MX34  ,
     4             MY13    ,MY23 ,MY34 ,Z1   ,DT1    ,OFF   ,
     5             OFFG    ,RLXYZ)
        IF (ISROT > 0) THEN
          CALL CZDEFRZ(JFT  ,JLT  ,AREA ,A_I ,RLZ   ,
     1                 VDEF ,VHGZK,VHGZE,X13 ,X24   ,
     2                 Y13  ,Y24  ,MX13 ,MX23,MX34  ,
     3                 MY13 ,MY23 ,MY34 ,Z1  ,DHG   ,
     4                 BM0RZ,BMKRZ,BMERZ,VL13,VL24  ,
     5                 VRLZ )
        ENDIF
C----------------------------------
C     CALCUL DES DEFORMATIONS 
C----------------------------------
        CALL CZSTRA3(JFT    ,JLT    ,NFT, VDEF,STRAG    ,
     2               EXX    ,EYY    ,EXY, EXZ ,EYZ      ,
     3               KXX    ,KYY    ,KXY, DT1C,TANI     ,
     4               IEPSDOT,ISTRAIN,UX1 ,UX2 ,UX3      ,
     5               UX4    ,UY1    ,UY2 ,UY3 ,UY4      ,
     6               Y24    ,PX2    ,PY1 ,PY2 ,AREA     ,
     7               ISMSTR ,MTN    ,BID ,BID ,BID      ,
     8               NEL    )
C-----------------
C     CONTRAINTES
C-----------------
        DO I = JFT,JLT
          TEMPEL(I) = ZERO
        ENDDO
        IF (JTHE > 0 ) CALL TEMPCG(JFT  ,JLT   ,PM  ,MAT ,IXC,
     .                             TEMP ,TEMPEL)
C-----------------------------
        IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STARTIME(35,1)
C-----------------------------
        CALL CMAIN3(
     1       XFEM_STR  ,JFT       ,JLT       ,NFT       ,IPARG   ,
     2       NEL       ,MTN       ,IPLA      ,ITHK      ,GROUP_PARAM,
     3       PM        ,GEO       ,NPF       ,TF        ,BUFMAT  ,
     4       SSP       ,RHO       ,VISCMX    ,DT1C      ,SIGY    ,
     5       AREA      ,EXX       ,EYY       ,EXY       ,EXZ     ,
     6       EYZ       ,KXX       ,KYY       ,KXY       ,NU      ,
     7       OFF       ,THK0      ,MAT       ,PID       ,MATPARAM_TAB,
     8       FORG      ,MOMG      ,STRAG     ,FAILWAVE  ,FWAVE   ,
     9       THKG      ,EINTG     ,IOFC      ,
     A       G         ,A11       ,A12       ,VOL0      ,INDXOF  ,
     B       NGL       ,ZCFAC     ,SHF       ,GS        ,EPSDG   ,
     C       KFTS      ,IHBE      ,ALPE      ,
     D       DIR_A     ,DIR_B     ,IGEO      ,
     E       IPM       ,IFAILURE  ,NPG       ,
     F       TEMPEL    ,DIE       ,JTHE      ,IEXPAN    ,TEMPG   ,
     G       IBID      ,BID       ,
     H       BID       ,BID       ,BID       ,BID       ,BID     ,
     I       BID       ,BID       ,BID       ,R11       ,R12     ,
     J       R13       ,R21       ,R22       ,R23       ,R31     ,
     K       R32       ,R33       ,IBID      ,TABLE     ,IXFEM   ,
     L       BID       ,LSENSOR   ,SENSOR    ,BID       ,ELCRKINI,
     M       DIR1_CRK  ,DIR2_CRK  ,LL        ,
     N       ISMSTR    ,IR        ,IS        ,NLAYER    ,NPTT    ,
     O       IXLAY     ,IXEL      ,ISUBSTACK ,STACK     ,
     R       BID       ,ITASK     ,DRAPE_Q4  ,VARNL     ,NLOC_DMG)
C-----------------------------
        IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STOPTIME(35,1)
C--------------------------
C     BILANS PAR MATERIAU
C--------------------------
        IF (IPRI == 1)
     1    CALL CBILAN(JFT     ,JLT      ,PM     ,V    ,IXC ,
     2                THKG    ,EINTG    ,PARTSAV,AREA ,MAT ,
     3                IPARTC  ,BID      ,BID    ,BID  ,BID ,
     4                BID     ,BID      ,IBID   ,OFF  ,
     5                NFT     ,GRESAV   ,GRTH   ,IGRTH,
     6                VL1     ,VL2      ,VL3    ,VL4  ,VRL1,
     7                VRL2    ,VRL3     ,VRL4   ,X1G  ,X2G ,
     8                X3G     ,X4G      ,Y1G    ,Y2G  ,Y3G ,
     9                Y4G     ,Z1G      ,Z2G    ,Z3G  ,Z4G ,
     A                IXFEM2  ,IEXPAN   ,EINTTH   ,ITASK)
C
        CALL CNDT3(
     1             JFT    ,JLT    ,OFF    , DT2T   ,AMU   ,
     2             NELTST ,ITYPTST,STI    , STIR   ,OFFG  ,
     3             SSP    ,VISCMX ,RHO    , VOL0   ,THK0  ,THK02,
     4             A11    ,LL     ,ALPE   , NGL    ,ISMSTR,
     5             IOFC   ,NNOD   ,AREA   , G      ,SHF   ,
     6             MSC    ,DMELC  ,JSMS   , BID    ,IGTYP ,
     7             IGMAT  ,A11R   ,GBUF%DT, GBUF%DT)
        CALL CZFINTCE(JFT  ,JLT  ,THK0 ,THK02,A_I    ,X13    ,
     2                X24  ,Y13  ,Y24  ,Z1   ,MX23   ,MX13   ,
     3                MX34 ,MY13 ,MY23 ,MY34 ,FORG   ,MOMG   ,
     4                VF   ,VM   ,NEL  )
        IF (ISROT > 0) THEN
          CALL CZFINTCRZ(JFT      ,JLT  ,THK0 ,VOL0 ,AREA  ,X13   ,
     2                   X24      ,Y13  ,Y24  ,Z1   ,MX23  ,MX13  ,
     3                   MX34     ,MY13 ,MY23 ,MY34 ,FORG  ,HOURGG,
     4                   VF       ,VMZ  ,BM0RZ,KRZ  ,VRLZ  ,DT1C  ,
     5                   EINTG    ,OFF  ,NEL  )
          CALL CZFINTNRZ_OR(
     1         JFT     ,JLT     ,THK0   ,THK02    ,A_I  ,DHG       ,
     2         X13     ,X24     ,Y13    ,Y24      ,Z1   ,MX23      ,
     3         MX13    ,MX34    ,MY13   ,MY23     ,MY34 ,HOURGG    ,
     4         FORG    ,MOMG    ,VF     ,VM       ,ZCFAC,A11       ,
     5         A12     ,G       ,GS     ,SIGY     ,OFF  ,FAC1      ,
     6         RHO     ,AREA    ,DT1    ,EINTG    ,AMU  ,VLHI      ,
     7         NPTT    ,IPARTC  ,PARTSAV,KFTS     ,GSR       ,
     8         A11SR   ,A12SR   ,NUSR   ,SHFSR    ,BMKRZ,BMERZ     ,
     9         VHGZK   ,VHGZE   ,KRZ    ,VMZ      ,IORTH,HM        ,
     A         HF      ,HC      ,HMFOR  ,MTN      ,NEL  )
        ELSE
         IF (IORTH == 0) THEN
          CALL CZFINTN1(JFT    ,JLT    ,THK0   ,THK02,A_I  ,DHG   ,
     2                  X13    ,X24    ,Y13    ,Y24  ,Z1   ,MX23  ,
     3                  MX13   ,MX34   ,MY13   ,MY23 ,MY34 ,HOURGG,
     4                  FORG   ,MOMG   ,VF     ,VM   ,ZCFAC,A11   ,
     5                  A12    ,G      ,SHF    ,SIGY ,OFF  ,FAC1  ,
     6                  RHO    ,AREA   ,DT1    ,EINTG,AMU  ,VLHI  ,
     7                  NPTT   ,IPARTC ,PARTSAV,KFTS ,GSR  ,NEL   ,
     8                  A11SR  ,A12SR  ,NUSR   ,SHFSR,MTN  ,FAC58 )
         ELSE
          CALL CZFINTN_OR(JFT  ,JLT  ,THK0 ,THK02,A_I  ,DHG     ,
     2                    X13  ,X24  ,Y13  ,Y24  ,Z1   ,MX23    ,
     3                    MX13 ,MX34 ,MY13 ,MY23 ,MY34 ,HOURGG  ,
     4                    FORG ,MOMG ,VF   ,VM  ,ZCFAC  ,A11    ,
     5                    A12  ,G    ,GS   ,SIGY ,OFF  ,FAC1    ,
     6                    RHO  ,AREA ,  DT1,EINTG,AMU  ,VLHI    ,
     7                    NPTT ,IPARTC,PARTSAV,KFTS  ,GSR  ,
     8                    A11SR,A12SR ,NUSR,SHFSR ,IORTH  ,HM   ,
     9                    HF   ,HC    ,HMFOR,MTN  ,NEL)
         ENDIF  !  IF (IORTH == 0)
        ENDIF !  (ISROT > 0) THEN
C
        IF (NPTT == 1)
     1    CALL CZFINTNM(JFT  ,JLT   ,THK0   ,A_I  ,DHG  ,
     2                  X13  ,X24   ,Y13    ,Y24  ,VF   ,
     3                  G    ,RHO   ,AREA   ,AMU  ,DT1  ,
     4                  OFF  ,IPARTC,PARTSAV,KFTS )
        CALL CZPROJ1(
     1               JFT    ,JLT    ,VQN    ,VQ     ,VF   ,
     2               VM     ,PLAT   ,
     3               F11    ,F12    ,F13    ,F14    ,F21  ,
     4               F22    ,F23    ,F24    ,F31    ,F32  ,
     5               F33    ,F34    ,M11    ,M12    ,M13  ,
     6               M14    ,M21    ,M22    ,M23    ,M24  ,
     7               M31    ,M32    ,M33    ,M34    ,FZERO,
     8               Z1     ,LXYZ   ,DD     ,DB     ,LXYZ ,
     9               ISROT  ,DIZ    ,VMZ    )
C-------------------------
c     Thermique des coques
C--------------------------
C
        IF (JTHE > 0) THEN
          CALL THERMC(JFT    ,JLT     ,PM      ,MAT  ,THK0 ,IXC  ,
     .                Y24    ,PX2     ,PY1     ,PY2  ,AREA ,DT1C ,
     .                TEMP   ,TEMPEL  ,DIE     ,THEM )
        ENDIF
C
C--------------------------
C     ASSEMBLAGE des forces dans les adresses sky des noeuds phantomes
C--------------------------
        IF (IPARIT == 1)
     .    CALL CUPDTN3_CRK(
     .         JFT   ,JLT     ,NFT  ,IXC   ,OFF     ,IADC   ,
     .         F11   ,F21     ,F31  ,F12   ,F22     ,F32    ,
     .         F13   ,F23     ,F33  ,F14   ,F24     ,F34    ,
     .         M11   ,M21     ,M31  ,M12   ,M22     ,M32    ,
     .         M13   ,M23     ,M33  ,M14   ,M24     ,M34    ,
     .         STI   ,STIR    ,FSKY ,ELCUTC,IADC_CRK,IEL_CRK,
     .         ILEV  ,INOD_CRK,FACN ,OFFG  ,EINTG,PARTSAV,
     .         IPARTC,IXLAY   ,CRKSKY)
C-------------------------
      ENDDO  !  DO IXLAY=1,NXLAY
C-------------------------
      IF (ALLOCATED(DIRA)) DEALLOCATE(DIRA)
      IF (ALLOCATED(DIRB)) DEALLOCATE(DIRB)
      IF (ALLOCATED(ELCRKINI)) DEALLOCATE(ELCRKINI)
      IF (ALLOCATED(DIR1_CRK)) DEALLOCATE(DIR1_CRK)
      IF (ALLOCATED(DIR2_CRK)) DEALLOCATE(DIR2_CRK)
      IF (ALLOCATED(VARNL))    DEALLOCATE(VARNL)
C-------------------------
      RETURN
      END
