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
Chd|  CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|-- called by -----------
Chd|        FORINTC                       source/elements/forintc.F     
Chd|-- calls ---------------
Chd|        CBILAN                        source/elements/shell/coque/cbilan.F
Chd|        CFINT_REG                     source/elements/shell/coque/cfint_reg.F
Chd|        CMAIN3                        source/materials/mat_share/cmain3.F
Chd|        CNCOEF3B                      source/elements/sh3n/coquedk/cncoef3.F
Chd|        CNCOEFORT                     source/elements/sh3n/coquedk/cncoef3.F
Chd|        CNDT3                         source/elements/sh3n/coquedk/cndt3.F
Chd|        CRKLAYER4N_ADV                source/elements/xfem/crklayer4n_adv.F
Chd|        CRKLAYER4N_INI                source/elements/xfem/crklayer4n_ini.F
Chd|        CRKLEN4N_ADV                  source/elements/xfem/crklen4n_adv.F
Chd|        CRKOFFC                       source/elements/xfem/precrklay.F
Chd|        CUPDT3F                       source/elements/shell/coque/cupdt3.F
Chd|        CUPDTN3                       source/elements/shell/coque/cupdtn3.F
Chd|        CUPDTN3P                      source/elements/shell/coque/cupdtn3.F
Chd|        CZCORC1                       source/elements/shell/coquez/czcorc.F
Chd|        CZCORCHT                      source/elements/shell/coquez/czcorc.F
Chd|        CZCORCT                       source/elements/shell/coquez/czcorc.F
Chd|        CZDEF                         source/elements/shell/coquez/czdef.F
Chd|        CZDEFRZ                       source/elements/shell/coquez/czdef.F
Chd|        CZDEFRZT                      source/elements/shell/coquez/czdef.F
Chd|        CZDEFT                        source/elements/shell/coquez/czdef.F
Chd|        CZDEFTW                       source/elements/shell/coquez/czdef.F
Chd|        CZFINTCE                      source/elements/shell/coquez/czfintce.F
Chd|        CZFINTCRZ                     source/elements/shell/coquez/czfintce.F
Chd|        CZFINTN1                      source/elements/shell/coquez/czfintn.F
Chd|        CZFINTNM                      source/elements/shell/coquez/czfintn.F
Chd|        CZFINTNM1                     source/elements/shell/coquez/czfintn.F
Chd|        CZFINTNRZ                     source/elements/shell/coquez/czfintn.F
Chd|        CZFINTNRZ_OR                  source/elements/shell/coquez/czfintn.F
Chd|        CZFINTN_OR                    source/elements/shell/coquez/czfintn.F
Chd|        CZPROJ1                       source/elements/shell/coquez/czproj.F
Chd|        CZSTRA3                       source/elements/shell/coquez/czstra3.F
Chd|        CZSTRAH3                      source/elements/shell/coquez/czstrah3.F
Chd|        DTTHERM                       source/elements/sh3n/coquedk/dttherm.F
Chd|        PRECRKLAY                     source/elements/xfem/precrklay.F
Chd|        SET_FAILWAVE_NOD4             source/materials/fail/failwave/set_failwave_nod4.F
Chd|        SET_FAILWAVE_SH4N             source/materials/fail/failwave/upd_failwave_sh4n.F
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
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE CZFORC3(
     1   ELBUF_STR,   JFT,         JLT,         NFT,
     2   NPT,         ITAB,        IPARI,       MTN,
     3   IPRI,        ITHK,        NELTST,      ISTRAIN,
     4   IPLA,        TT,          DT1,         DT2T,
     5   PM,          GEO,         PARTSAV,     IXC,
     6   ITYPTST,     BUFMAT,      TF,          NPF,
     7   IADC,        FAILWAVE,    X,           D,
     8   DR,          V,           VR,          F,
     9   M,           STIFN,       STIFR,       FSKY,
     A   TANI,        EANI,        INDXOF,      ISMSTR,
     B   GROUP_PARAM, IPARTC,      THKE,        NVC,
     C   IOFC,        IHBE,        F11,         F12,
     D   F13,         F14,         F21,         F22,
     E   F23,         F24,         F31,         F32,
     F   F33,         F34,         M11,         M12,
     G   M13,         M14,         M21,         M22,
     H   M23,         M24,         M31,         M32,
     I   M33,         M34,         KFTS,        FZERO,
     J   IGEO,        IPM,         IFAILURE,    ITASK,
     K   JTHE,        TEMP,        FTHE,        FTHESKY,
     L   IEXPAN,      GRESAV,      GRTH,        IGRTH,
     M   XEDGE4N,     MSC,         DMELC,       JSMS,
     N   TABLE,       IPARG,       MATPARAM_TAB,IXFEM,
     O   KNOD2ELC,    SENSORS,     ELCUTC,      INOD_CRK,
     P   IEL_CRK,     IBORDNODE,   NODENR,      IADC_CRK,
     Q   NODEDGE,     CRKNODIAD,   CONDN,       CONDNSKY,
     R   STACK,       ISUBSTACK,   XFEM_STR,    CRKEDGE,
     S   DRAPE_SH4N,  NEL,         NLOC_DMG,    INDX_DRAPE,
     T   IGRE,        JTUR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE ELBUFDEF_MOD
      USE CRACKXFEM_MOD
      USE STACK_MOD
      USE FAILWAVE_MOD
      USE MATPARAM_DEF_MOD     
      USE NLOCAL_REG_MOD        
      USE DRAPE_MOD       
      USE SENSOR_MOD
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
#include      "scr14_c.inc"
#include      "scr18_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
#include      "impl1_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,NFT,NPT,IPARI,MTN,IPRI,ITHK,NELTST,
     .   ITYPTST ,ISTRAIN ,IPLA,NVC,JSMS,ITASK,IEXPAN,IXFEM,
     .   IOFC,IHBE,KFTS,ISMSTR,IFAILURE,JTHE,ISUBSTACK,NEL,N1,N2,N3,N4
      INTEGER IXC(NIXC,*),IADC(4,*),IPARTC(*),NPF(*),IGEO(NPROPGI,*),
     .   IPM(NPROPMI,*),INDXOF(MVSIZ),GRTH(*),IGRTH(*),IPARG(*),ITAB(*),
     .   KNOD2ELC(*),XEDGE4N(4,*),
     .   ELCUTC(2,*),INOD_CRK(*),IEL_CRK(*),IBORDNODE(*),
     .   NODENR(*),IADC_CRK(4,*),NODEDGE(2,*),CRKNODIAD(*),INDX_DRAPE(SCDRAPE)
      INTEGER :: IGRE,JTUR
C     REAL OU REAL*8
      my_real
     .   F11(MVSIZ), F12(MVSIZ), F13(MVSIZ), F14(MVSIZ),
     .   F21(MVSIZ), F22(MVSIZ), F23(MVSIZ), F24(MVSIZ),
     .   F31(MVSIZ), F32(MVSIZ), F33(MVSIZ), F34(MVSIZ),
     .   M11(MVSIZ), M12(MVSIZ), M13(MVSIZ), M14(MVSIZ),
     .   M21(MVSIZ), M22(MVSIZ), M23(MVSIZ), M24(MVSIZ),
     .   M31(MVSIZ), M32(MVSIZ), M33(MVSIZ), M34(MVSIZ),
     .   TF(*),   PM(NPROPM,*),GEO(NPROPG,*),PARTSAV(*),
     .   BUFMAT(*), X(3,*),D(*),DR(*),V(3,*),VR(3,*),F(3,*),M(3,*),
     .   STIFN(*), STIFR(*),FSKY(*),TANI(6,*),EANI(*),THKE(*),
     .   FZERO(3,4,*),TEMP(*),FTHE(*),FTHESKY(*),GRESAV(*),
     .   MSC(*), DMELC(*),CONDN(*),CONDNSKY(*)
      my_real    
     .   TT, DT1, DT2T
      TARGET :: BUFMAT
      TYPE(TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (NLOCAL_STR_)  , TARGET :: NLOC_DMG 
      TYPE (ELBUF_STRUCT_), DIMENSION(NXEL) :: XFEM_STR
      TYPE (XFEM_EDGE_)   , DIMENSION(NXLAYMAX) :: CRKEDGE
      TYPE (STACK_PLY)     :: STACK
      TYPE (FAILWAVE_STR_) :: FAILWAVE 
      TYPE (GRP_PARAM_STRUCT_) :: GROUP_PARAM
      TYPE (DRAPE_) , DIMENSION(NUMELC_DRAPE) :: DRAPE_SH4N
      TYPE (MATPARAM_STRUCT_) , DIMENSION(NUMMAT) :: MATPARAM_TAB
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C   L O C A L   V A R I A B L E S
C--------------------------------
      LOGICAL PLAT(MVSIZ)
      INTEGER 
     .   I,J,IR,IS,MX,NNOD,NUPAR,NLAY,NXLAY,ILAY,NPG,IXEL,IFAILWAVE,
     .   IBID,ISROT,NG,IXFEM2,L_DIRA,L_DIRB,J1,J2,IORTH,IGTYP,IGMAT,IXLAY,
     .   NPTTOT,IREP,IPOUT,IDRAPE,NPTT,IT,ACTIFXFEM,SEDRAPE,NUMEL_DRAPE
      INTEGER  MAT(MVSIZ),PID(MVSIZ),NGL(MVSIZ),FWAVE_EL(NEL)
      PARAMETER (NNOD = 4)
      my_real 
     .  RH0(MVSIZ),LL(MVSIZ),
     .  VL13(MVSIZ,3), VL24(MVSIZ,3), VLHI(MVSIZ,3), RLXYZ(MVSIZ,8),
     .  X13(MVSIZ),  X24(MVSIZ),  Y13(MVSIZ),  Y24(MVSIZ),
     .  MX13(MVSIZ), MX23(MVSIZ), MX34(MVSIZ),
     .  MY13(MVSIZ), MY23(MVSIZ), MY34(MVSIZ), Z1(MVSIZ),Z2(MVSIZ),
     .  VQ(MVSIZ,9), VDEF(MVSIZ,8), OFF(MVSIZ), DHG(MVSIZ,6),
     .  AREA(MVSIZ), VQN(MVSIZ,12), L13(MVSIZ), L24(MVSIZ),
     .  ZCFAC(MVSIZ,2), A_I(MVSIZ), THK02(MVSIZ),
     .  EXX(MVSIZ) ,EYY(MVSIZ) ,EXY(MVSIZ)   ,EXZ(MVSIZ) ,EYZ(MVSIZ),
     .  KXX(MVSIZ) ,KYY(MVSIZ) ,KXY(MVSIZ)   ,SIGY(MVSIZ), 
     .  DT1C(MVSIZ),SSP(MVSIZ) ,VISCMX(MVSIZ),RHO(MVSIZ) ,
     .  NU(MVSIZ)  ,G(MVSIZ)   ,A11(MVSIZ)   ,A12(MVSIZ) ,VOL0(MVSIZ),
     .  THK0(MVSIZ),STI(MVSIZ) ,STIR(MVSIZ)  ,SHF(MVSIZ) ,
     .  VF(MVSIZ,12),VM(MVSIZ,8),GS(MVSIZ)   ,FAC1(MVSIZ),
     .  ALPE(MVSIZ),YM(MVSIZ) ,BID,FACN(MVSIZ,2),
     .  LXYZ(MVSIZ,8),DD(MVSIZ,6),DB(MVSIZ,12),AMU(MVSIZ),
     .  GSR(MVSIZ), A11SR(MVSIZ), A12SR(MVSIZ), NUSR(MVSIZ),
     .  SHFSR(MVSIZ),PX2(MVSIZ),PY1(MVSIZ),PY2(MVSIZ),PX1(MVSIZ),
     .  DIE(MVSIZ), TEMPEL(MVSIZ),THEM(MVSIZ,4),
     .  R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .  R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .  R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .  RLZ(MVSIZ,4),VHGZK(MVSIZ,5),VHGZE(MVSIZ,5),VRLZ(MVSIZ),
     .  BM0RZ(MVSIZ,4,4),BMKRZ(MVSIZ,4,4),BMERZ(MVSIZ,4,4),
     .  VMZ(MVSIZ,4),KRZ(MVSIZ),DIZ(MVSIZ,3),
     .  X1G(MVSIZ), X2G(MVSIZ), X3G(MVSIZ), X4G(MVSIZ),
     .  Y1G(MVSIZ), Y2G(MVSIZ), Y3G(MVSIZ), Y4G(MVSIZ),
     .  Z1G(MVSIZ), Z2G(MVSIZ), Z3G(MVSIZ), Z4G(MVSIZ),
     .  VL1(MVSIZ,3),VL2(MVSIZ,3),VL3(MVSIZ,3),VL4(MVSIZ,3),
     .  VRL1(MVSIZ,3),VRL2(MVSIZ,3),VRL3(MVSIZ,3),VRL4(MVSIZ,3),
     .  VX1(MVSIZ),VX2(MVSIZ),VX3(MVSIZ),VX4(MVSIZ),
     .  VY1(MVSIZ),VY2(MVSIZ),VY3(MVSIZ),VY4(MVSIZ),
     .  VZ1(MVSIZ),VZ2(MVSIZ),VZ3(MVSIZ),VZ4(MVSIZ),
     .  VRX1(MVSIZ),VRX2(MVSIZ),VRX3(MVSIZ),VRX4(MVSIZ),
     .  VRY1(MVSIZ),VRY2(MVSIZ),VRY3(MVSIZ),VRY4(MVSIZ),
     .  VRZ1(MVSIZ),VRZ2(MVSIZ),VRZ3(MVSIZ),VRZ4(MVSIZ),
     .  UX1(MVSIZ),UX2(MVSIZ),UX3(MVSIZ),UX4(MVSIZ),
     .  UY1(MVSIZ),UY2(MVSIZ),UY3(MVSIZ),UY4(MVSIZ),
     .  HM(MVSIZ,6), HF(MVSIZ,6), HC(MVSIZ,2),HMFOR(MVSIZ,6),
     .  CONDE(MVSIZ),A11R(MVSIZ),XL2(MVSIZ),XL3(MVSIZ),XL4(MVSIZ),
     .  YL2(MVSIZ),YL3(MVSIZ),YL4(MVSIZ)
      my_real 
     .   AREAT(MVSIZ),X13T(MVSIZ) ,Y13T(MVSIZ), X24T(MVSIZ),Y24T(MVSIZ),
     .  MX13T(MVSIZ), MX23T(MVSIZ), MX34T(MVSIZ),
     .  MY13T(MVSIZ), MY23T(MVSIZ), MY34T(MVSIZ), Z1T(MVSIZ),
     .  A_IT(MVSIZ),F_DEF(MVSIZ,8), U13(MVSIZ,2),U24(MVSIZ,2),
     .  AXYZ(MVSIZ,4),WXY(MVSIZ),FAC58(MVSIZ,2)  
C--- Variables pour le non-local
      INTEGER :: NDDL, K, INOD(4),NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ),
     .           IPOS(4), L_NLOC, IMAT, INLOC,NUPARAM,IADBUF,IINT
      my_real, DIMENSION(:,:), ALLOCATABLE :: VAR_REG
      my_real, DIMENSION(:), POINTER :: BPRELD,DNL,UVAR
C---
      INTEGER, DIMENSION(NEL) :: OFFLY
      INTEGER, ALLOCATABLE, DIMENSION(:) :: ELCRKINI
      my_real, ALLOCATABLE, DIMENSION(:) :: DIRA,DIRB,DIR1_CRK,DIR2_CRK
      my_real, DIMENSION(:) ,POINTER     :: DIR_A,DIR_B,CRKDIR,CRKLEN,DADV,UPARAM
      TARGET :: DIRA,DIRB
C-----
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF
      TYPE(L_BUFEL_DIR_) ,POINTER :: LBUF_DIR
C------------|---------|------------------------------------------------------
C     ZCFAC(1) AVERAGE ETAN/E
C     ZCFAC(2) MIN(ETAN/E)
C     DEFAULT VALUES SET TO 1.
C=======================================================================
      GBUF => ELBUF_STR%GBUF
      IDRAPE = ELBUF_STR%IDRAPE
C-----
      IBID = 0
      BID  = ZERO
      ISROT = IPARG(41)
      IREP  = IPARG(35)
      INLOC = IPARG(78)
      IINT  = IPARG(36)
      ACTIFXFEM = IPARG(70)
      NLAY  = ELBUF_STR%NLAY
      SEDRAPE = SCDRAPE
      NUMEL_DRAPE = NUMELC_DRAPE
c
c      NPT  = MAX(NLAY,NPTT) --> set to = IPARG(6) , keeping it original
c                                to allow for NPT = 0 (global LAW_3
      IXEL  = 0
      IXLAY = 0
      NPG   = 0
      IR    = 1
      IS    = 1
      NG    = 1
C
      NPTTOT  = 0
      DO ILAY=1,NLAY
        NPTTOT = NPTTOT + ELBUF_STR%BUFLY(ILAY)%NPTT
      ENDDO
      UVAR  => ELBUF_STR%BUFLY(1)%MAT(IR,IS,1)%VAR
!-------------------------------------------      
! Tableau pour la variable non-locale
      NDDL = NPTTOT
      ALLOCATE(VAR_REG(NEL,NDDL)) 
!-------------------------------------------
      IF (NPT == 0) NPTTOT = NPT  !  compatibility with global integration
c-------------------------------------
      L_DIRA = ELBUF_STR%BUFLY(1)%LY_DIRA
      L_DIRB = ELBUF_STR%BUFLY(1)%LY_DIRB
      IGTYP = IGEO(11,IXC(6,1))
      IF(IDRAPE > 0 .AND. (IGTYP == 51 .OR. IGTYP == 52)) THEN
        ALLOCATE(DIRA(NPTTOT*NEL*L_DIRA))
        ALLOCATE(DIRB(NPTTOT*NEL*L_DIRB))
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
      DO I=JFT,JLT
        MAT(I) = IXC(1,I)
        PID(I) = IXC(6,I)
        NGL(I) = IXC(7,I)
      ENDDO
      DO I=JFT,JLT
        SIGY(I)    = EP30
        ZCFAC(I,1) = ONE
        ZCFAC(I,2) = ONE
        ALPE(I)    = ONE
      ENDDO
C
      DO I=JFT,JLT                
        FAC1(I) = GEO(17,PID(I))
        VF(I,1:12) =ZERO		
        VM(I,1:8) =ZERO		
      ENDDO
C      
      IMAT  = IXC(1,JFT)
      IGTYP = IGEO(11,PID(1))
      IGMAT = IGEO(98,PID(1))
      IADBUF = MAX(1,IPM(7,IMAT))
      NUPARAM= IPM(9,IMAT)       
      UPARAM => BUFMAT(IADBUF:IADBUF+NUPARAM)
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
        DADV   => GBUF%DMG
        CALL SET_FAILWAVE_SH4N(FAILWAVE ,FWAVE_EL ,DADV     ,
     .     NEL      ,IXC      ,ITAB     ,NGL      ,OFFLY    )
      ENDIF
c-------------------------------------
C     CALCULS PRELIMINAIRES GLOBAL-->LOCAL
C-----------------------------------------
C     crack xfem
c-------------------------------------
      IXFEM2 = 0
      NXLAY  = NLAY
      IF (IXFEM > 0) THEN
        ALLOCATE(ELCRKINI(NXLAYMAX*MVSIZ))
        ALLOCATE(DIR1_CRK(NXLAYMAX*MVSIZ))
        ALLOCATE(DIR2_CRK(NXLAYMAX*MVSIZ))
        DIR1_CRK = ZERO
        DIR2_CRK = ZERO
        ELCRKINI = 0
        IF (NLEVSET > 0) THEN   
          CALL PRECRKLAY(JFT     ,JLT    ,NFT     ,NXLAY  ,ELCRKINI,
     .                   IEL_CRK,INOD_CRK,NODENR  ,CRKEDGE,XEDGE4N )
        ENDIF
      ELSE
        ALLOCATE(ELCRKINI(0))
        ALLOCATE(DIR1_CRK(0))
        ALLOCATE(DIR2_CRK(0))
      ENDIF  ! IXFEM
C--------------------
      CALL CZCORC1(ELBUF_STR,
     1             JFT     ,JLT     ,X       ,V       ,VR      ,
     2             IXC     ,PM      ,PLAT    ,AREA    ,
     3             A_I     ,VL13    ,VL24    ,VLHI    ,RLXYZ   ,
     4             VQN     ,VQ      ,LL      ,L13     ,L24     ,
     5             X13     ,X24     ,Y13     ,Y24     ,MX13    ,
     6             MX23    ,MX34    ,MY13    ,MY23    ,MY34    ,
     7             Z1      ,LXYZ    ,DD      ,DB    ,GBUF%SMSTR,
     9             IREP    ,NPT     ,NLAY    ,ISMSTR  ,
     A             DIR_A   ,DIR_B   ,GBUF%OFF,RLXYZ   ,LXYZ    ,
     B             FACN    ,PY1     ,PX2     ,PY2     ,R11     ,
     C             R12     ,R13     ,R21     ,R22     ,R23     ,
     D             R31     ,R32     ,R33     ,RLZ     ,ISROT   ,
     E             IXFEM2  ,VX1     ,VX2     ,VX3     ,VX4     ,
     F             VY1     ,VY2     ,VY3     ,VY4     ,VZ1     ,
     G             VZ2     ,VZ3     ,VZ4     ,VRX1    ,VRX2    ,
     H             VRX3    ,VRX4    ,VRY1    ,VRY2    ,VRY3    ,
     I             VRY4    ,VRZ1    ,VRZ2    ,VRZ3    ,VRZ4    ,
     J             X1G     ,X2G     ,X3G     ,X4G     ,Y1G     ,
     K             Y2G     ,Y3G     ,Y4G     ,Z1G     ,Z2G     ,
     L             Z3G     ,Z4G     ,THKE    ,DIZ     ,UX1     ,
     M             UX2     ,UX3     ,UX4     ,UY1     ,UY2     ,
     N             UY3     ,UY4     ,XL2     ,XL3     ,XL4     ,
     O             YL2     ,YL3     ,YL4     ,VL1     ,VL2     ,
     P             VL3     ,VL4     ,NEL     ,Z2      )
C
      CALL CNCOEF3B(JFT    ,JLT     ,PM      ,MAT     ,GEO     ,
     2              PID    ,AREA    ,SHF     ,THK0    ,
     3              THK02  ,NU      ,G       ,YM      ,
     4              A11    ,A12     ,GBUF%THK,THKE    ,SSP     ,
     5              RHO    ,VOL0    ,GS      ,MTN     ,ITHK    ,
     6              NPTTOT ,DT1C    ,DT1     ,IHBE    ,AMU     ,
     7              GSR    ,A11SR   ,A12SR   ,NUSR    ,SHFSR   ,
     8              KRZ    ,IGEO    ,A11R , ISUBSTACK , STACK%PM,
     9              UPARAM ,DIRA    ,DIRB    ,UVAR    ,FAC58   )
C       
      CALL CNCOEFORT(JFT    ,JLT    ,PM        ,MAT     ,GEO     ,
     1               PID    ,MTN    ,NPTTOT    ,HM      ,HF      ,
     2               HC     ,HMFOR  ,IORTH     ,DIR_A   ,IGEO    ,
     3               ISUBSTACK,STACK,ELBUF_STR ,NLAY    ,GBUF%THK,
     4               DRAPE_SH4N  ,NFT  ,NEL    ,INDX_DRAPE , THKE,
     5               SEDRAPE,NUMEL_DRAPE )
C----------------------------------
C     CALCUL VITESSE DE DEFORMATION 
C----------------------------------
        CALL CZDEF(JFT     ,JLT   ,AREA ,A_I  ,VL13   ,VL24  ,
     2             VLHI    ,RLXYZ ,VDEF ,DHG  ,X13    ,
     3             X24     ,Y13   ,Y24  ,MX13 ,MX23   ,MX34  ,
     4             MY13    ,MY23  ,MY34 ,Z1   ,DT1    ,OFF   ,
     5             GBUF%OFF,RLXYZ )
        IF (ISROT > 0) THEN
          CALL CZDEFRZ(JFT   ,JLT   ,AREA  ,A_I  ,RLZ   ,
     1                 VDEF  ,VHGZK ,VHGZE ,X13  ,X24   ,
     2                 Y13   ,Y24   ,MX13  ,MX23 ,MX34  ,
     3                 MY13  ,MY23  ,MY34  ,Z1   ,DHG   ,
     4                 BM0RZ ,BMKRZ ,BMERZ ,VL13 ,VL24  ,
     5                 VRLZ  )
        ENDIF
      IF (ISMSTR == 10 ) THEN
        CALL CZCORCT(ELBUF_STR,
     1                     JFT    ,JLT    ,X      ,V      ,VR     ,
     2                     IXC    ,PM     ,GBUF%OFF,AREAT ,A_IT   ,
     3                     U13    ,U24    ,DR     ,AXYZ   ,VQ     ,
     4                     X13T   ,X24T   ,Y13T   ,Y24T   ,MX13T  ,
     5                     MX23T  ,MX34T  ,MY13T  ,MY23T  ,MY34T  ,
     6                     Z1T    ,GBUF%SMSTR,THKE,NPTTOT ,ISMSTR ,
     7                     ISROT  ,LXYZ   ,Z1     ,VQN    ,NEL    )
C  
        CALL CZDEFT(JFT ,JLT  ,U13   ,U24 ,AXYZ ,
     2              F_DEF,X13T  ,X24T,Y13T ,Y24T ,
     3              Z1T  ,A_IT  )
       IF (ISROT > 0) THEN
        CALL CZDEFRZT(JFT ,JLT  ,AREAT,A_IT ,AXYZ ,
     1                F_DEF,X13T,X24T  ,Y13T ,Y24T ,
     2                MX13T,MX23T,MX34T ,MY13T,MY23T,
     3                MY34T,Z1T  )
       END IF
        CALL CZDEFTW(JFT ,JLT  ,A_I ,VL13  ,VL24,
     2               RLXYZ,X13  ,X24 ,Y13  ,Y24  ,
     3               Z1  ,WXY)
      END IF !(ISMSTR == 10 ) THEN
C----------------------------------
C     CALCUL DES DEFORMATIONS 
C----------------------------------
      CALL CZSTRA3(JFT    ,JLT    ,NFT   ,VDEF ,GBUF%STRA,
     2             EXX    ,EYY    ,EXY   ,EXZ  ,EYZ      ,  
     3             KXX    ,KYY    ,KXY   ,DT1C ,TANI     ,
     4             IEPSDOT,ISTRAIN,UX1   ,UX2  ,UX3      ,
     5             UX4    ,UY1    ,UY2   ,UY3  ,UY4      ,
     6             Y24    ,PX2    ,PY1   ,PY2  ,AREA     ,
     7             ISMSTR ,MTN    ,WXY   ,F_DEF,GBUF%STRW,
     8             NEL    )
       IF (ISTRAIN /= 0) THEN
        CALL CZSTRAH3(JFT   ,JLT    ,GBUF%STRPG,DHG    ,A_I   ,
     3               MX23   ,MX34   ,MY23  ,MY34   ,DT1C  ,
     4               EXX    ,EYY    ,EXY   ,EXZ    ,EYZ      ,  
     3               KXX    ,KYY    ,KXY   ,NEL    )
       END IF 
       IF (NPTTOT == 1 .AND. MTN==58) THEN
          CALL CZCORCHT(ELBUF_STR,
     1               JFT    ,JLT    ,X      ,V      ,VR     ,
     2               IXC    ,PM     ,GBUF%OFF,
     3               VQ     ,GBUF%HOURG,THKE   ,NPTTOT ,ISMSTR ,
     7               LXYZ   ,Z2     ,IINT   ,NEL    )
       END IF 
c-------------------------------------------
c    COMPUTE Regularized non local variable in Gauss point
c-------------------------------------------     
      IF (INLOC > 0) THEN
        L_NLOC = NLOC_DMG%L_NLOC
        DNL  => NLOC_DMG%DNL(1:L_NLOC) ! DNL = non local variable increment
        DO I=JFT,JLT
          NC1(I) = IXC(2,I)
          NC2(I) = IXC(3,I)
          NC3(I) = IXC(4,I)
          NC4(I) = IXC(5,I)
        ENDDO
        DO K = 1,NDDL
#include "vectorize.inc" 
          DO I=JFT,JLT
            INOD(1) = NLOC_DMG%IDXI(NC1(I))
            INOD(2) = NLOC_DMG%IDXI(NC2(I))
            INOD(3) = NLOC_DMG%IDXI(NC3(I))
            INOD(4) = NLOC_DMG%IDXI(NC4(I)) 
            IPOS(1) = NLOC_DMG%POSI(INOD(1)) 
            IPOS(2) = NLOC_DMG%POSI(INOD(2)) 
            IPOS(3) = NLOC_DMG%POSI(INOD(3)) 
            IPOS(4) = NLOC_DMG%POSI(INOD(4))
            VAR_REG(I,K) = FOURTH*(DNL(IPOS(1)+K-1) + DNL(IPOS(2)+K-1) 
     .                          + DNL(IPOS(3)+K-1) + DNL(IPOS(4)+K-1))
          ENDDO
        ENDDO
      ENDIF     
C-----------------
C     CONTRAINTES
C-----------------
      IF (JTHE > 0) THEN
        CALL TEMPCG(JFT  ,JLT    ,PM     ,MAT    ,IXC     ,
     .              TEMP ,TEMPEL )
      ENDIF
C-----------------------------------------------     
      IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STARTIME(35,1)
C-----------------------------------------------
       CALL CMAIN3(
     1      ELBUF_STR ,JFT       ,JLT       ,NFT       ,IPARG      ,
     2      NEL       ,MTN       ,IPLA      ,ITHK      ,GROUP_PARAM,
     3      PM        ,GEO       ,NPF       ,TF        ,BUFMAT     ,
     4      SSP       ,RHO       ,VISCMX    ,DT1C      ,SIGY       ,
     5      AREA      ,EXX       ,EYY       ,EXY       ,EXZ        ,
     6      EYZ       ,KXX       ,KYY       ,KXY       ,NU         ,
     7      OFF       ,THK0      ,MAT       ,PID       ,MATPARAM_TAB,
     8      GBUF%FOR  ,GBUF%MOM  ,GBUF%STRA ,FAILWAVE  ,FWAVE_EL   ,
     9      GBUF%THK  ,GBUF%EINT ,IOFC      ,
     A      G         ,A11       ,A12       ,VOL0      ,INDXOF     ,
     B      NGL       ,ZCFAC     ,SHF       ,GS        ,GBUF%EPSD  ,
     C      KFTS      ,IHBE      ,ALPE      ,
     D      DIR_A     ,DIR_B     ,IGEO      ,
     E      IPM       ,IFAILURE  ,NPG       ,
     F      TEMPEL    ,DIE       ,JTHE      ,IEXPAN    ,GBUF%TEMP  ,
     G      IBID      ,BID       ,
     H      BID       ,BID       ,BID       ,BID       ,BID        ,
     I      BID       ,BID       ,BID       ,R11       ,R12        ,
     J      R13       ,R21       ,R22       ,R23       ,R31        ,
     K      R32       ,R33       ,NG        ,TABLE     ,IXFEM      ,
     L      BID       ,SENSORS   ,BID       ,ELCRKINI   ,
     M      DIR1_CRK  ,DIR2_CRK  ,LL        ,
     N      ISMSTR    ,IR        ,IS        ,NLAY      ,NPT        ,
     O      IXLAY     ,IXEL      ,ISUBSTACK ,STACK     ,
     P      F_DEF     ,ITASK     ,DRAPE_SH4N  ,VAR_REG   ,NLOC_DMG   ,
     R      INDX_DRAPE,THKE        ,SEDRAPE     ,NUMEL_DRAPE)
C-----------------------------------------------
c        
      IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STOPTIME(35,1)
C--------------------------
C     BILANS PAR MATERIAU
C--------------------------
       IPOUT=2
      IF (IPRI == 1)
     1   CALL CBILAN(
     1   JFT,        JLT,        PM,         V,
     2   IXC,        GBUF%THK,   GBUF%EINT,  PARTSAV,
     3   AREA,       MAT,        IPARTC,     X,
     4   VR,         BID,        BID,        BID,
     5   THK02,      IPOUT,      OFF,        NFT,
     6   GRESAV,     GRTH,       IGRTH,      VL1,
     7   VL2,        VL3,        VL4,        VRL1,
     8   VRL2,       VRL3,       VRL4,       X1G,
     9   X2G,        X3G,        X4G,        Y1G,
     A   Y2G,        Y3G,        Y4G,        Z1G,
     B   Z2G,        Z3G,        Z4G,        IBID,
     C   IEXPAN,     GBUF%EINTTH,ITASK,      GBUF%VOL,
     D   ACTIFXFEM,  IGRE)
C
         CALL CNDT3(
     1        JFT    ,JLT    ,OFF    ,DT2T   ,AMU     ,
     2        NELTST ,ITYPTST,STI    ,STIR   ,GBUF%OFF,
     3        SSP    ,VISCMX  ,RHO   ,VOL0   ,THK0,THK02,
     4        A11   ,LL   ,ALPE,NGL,ISMSTR,
     5        IOFC  ,NNOD ,AREA,G  ,SHF   ,
     6        MSC   ,DMELC,JSMS,BID , IGTYP ,
     7        IGMAT  ,A11R   ,GBUF%G_DT, GBUF%DT,MTN   ,
     8        PM     ,MAT(JFT))
c-------------------------------
         CALL CZFINTCE(JFT  ,JLT  ,THK0 ,THK02,A_I     ,X13     ,
     2                 X24  ,Y13  ,Y24  ,Z1   ,MX23    ,MX13    ,
     3                 MX34 ,MY13 ,MY23 ,MY34 ,GBUF%FOR,GBUF%MOM,
     4                 VF   ,VM   ,NEL  )
c
        IF (ISROT > 0) THEN
          CALL CZFINTCRZ(
     1         JFT      ,JLT  ,THK0 ,VOL0 ,AREA    ,X13       ,
     2         X24      ,Y13  ,Y24  ,Z1   ,MX23    ,MX13      ,
     3         MX34     ,MY13 ,MY23 ,MY34 ,GBUF%FOR,GBUF%HOURG,
     4         VF       ,VMZ  ,BM0RZ,KRZ  ,VRLZ    ,DT1C      ,
     5         GBUF%EINT,OFF  ,NEL  )
          IF (IMPL_S > 0 .OR. IORTH == 0) THEN
            CALL CZFINTNRZ(
     1         JFT     ,JLT     ,THK0   ,THK02    ,A_I   ,DHG       ,
     2         X13     ,X24     ,Y13    ,Y24      ,Z1    ,MX23      ,
     3         MX13    ,MX34    ,MY13   ,MY23     ,MY34  ,GBUF%HOURG,
     4         GBUF%FOR,GBUF%MOM,VF     ,VM       ,ZCFAC ,A11       ,
     5         A12     ,G       ,SHF    ,SIGY     ,OFF   ,FAC1      ,
     6         RHO     ,AREA    ,DT1    ,GBUF%EINT,AMU   ,VLHI      ,
     7         NPTTOT  ,IPARTC  ,PARTSAV,KFTS     ,GSR   ,
     8         A11SR   ,A12SR   ,NUSR   ,SHFSR    ,BMKRZ ,BMERZ     ,
     9         VHGZK   ,VHGZE   ,KRZ    ,VMZ      ,NEL   )
          ELSE
            CALL CZFINTNRZ_OR(JFT  ,JLT  ,THK0 ,THK02,A_I  ,DHG    ,
     2                      X13  ,X24  ,Y13  ,Y24  ,Z1   ,MX23 ,
     3                      MX13 ,MX34 ,MY13 ,MY23 ,MY34 ,GBUF%HOURG,
     4                      GBUF%FOR,GBUF%MOM,VF  ,VM  ,ZCFAC  ,A11  ,
     5                      A12  ,G    ,GS   ,SIGY ,OFF  ,FAC1 , 
     6                      RHO  ,AREA ,  DT1,GBUF%EINT,AMU  ,VLHI  ,
     7                      NPTTOT,IPARTC,PARTSAV,KFTS  ,GSR  ,
     8                      A11SR,A12SR ,NUSR,SHFSR,BMKRZ,BMERZ ,
     9                      VHGZK,VHGZE ,KRZ ,VMZ  ,IORTH  ,HM    ,
     A                      HF   ,HC    ,HMFOR, MTN  ,NEL)
          ENDIF ! IF (IMPL_S > 0 .OR. IORTH == 0)
c
        ELSE    ! ISROT == 0
c
          IF (IMPL_S > 0 .OR. IORTH == 0) THEN
            CALL CZFINTN1(
     1         JFT     ,JLT     ,THK0   ,THK02    ,A_I      ,DHG       ,
     2         X13     ,X24     ,Y13    ,Y24      ,Z1       ,MX23      ,
     3         MX13    ,MX34    ,MY13   ,MY23     ,MY34     ,GBUF%HOURG,
     4         GBUF%FOR,GBUF%MOM,VF     ,VM       ,ZCFAC    ,A11       ,
     5         A12     ,G       ,SHF    ,SIGY     ,OFF      ,FAC1      ,
     6         RHO     ,AREA    ,DT1    ,GBUF%EINT,AMU      ,VLHI      ,
     7         NPTTOT  ,IPARTC  ,PARTSAV,KFTS     ,GSR      ,NEL       ,
     8         A11SR   ,A12SR   ,NUSR   ,SHFSR    ,MTN      ,FAC58     )
          ELSE
            CALL CZFINTN_OR(JFT  ,JLT  ,THK0 ,THK02,A_I  ,DHG    ,
     2                      X13  ,X24  ,Y13  ,Y24  ,Z1   ,MX23 ,
     3                      MX13 ,MX34 ,MY13 ,MY23 ,MY34 ,GBUF%HOURG,
     4                      GBUF%FOR,GBUF%MOM,VF  ,VM  ,ZCFAC  ,A11  ,
     5                      A12  ,G    ,GS   ,SIGY ,OFF  ,FAC1 , 
     6                      RHO  ,AREA ,  DT1,GBUF%EINT,AMU  ,VLHI  ,
     7                      NPTTOT,IPARTC,PARTSAV,KFTS  ,GSR  ,
     8                      A11SR,A12SR ,NUSR,SHFSR ,IORTH  ,HM    ,
     9                      HF   ,HC    ,HMFOR,MTN  ,NEL) 
          END IF ! (IMPL_S > 0 .OR. IORTH == 0) THEN
        ENDIF    !  ISROT 
c-------------------------
c     Virtual internal forces of regularized non local ddl 
c--------------------------
      IF (INLOC > 0) THEN
        ! Warning : Y24 = PX1
        CALL CFINT_REG(
     1   NLOC_DMG,        VAR_REG,         GBUF%THK,        NEL,
     2   OFF,             AREA,            NC1,             NC2,
     3   NC3,             NC4,             Y24,             PY1,
     4   PX2,             PY2,             ELBUF_STR%NLOC(1,1), IMAT,
     5   NDDL,            ITASK,           DT2T,            LL,
     7   GBUF%THK_I,      GBUF%AREA,       NFT)
      ENDIF 
c-------------------------------
        IF (NPTTOT == 1) THEN
          IF (MTN==58.OR.MTN==19.OR.MTN==119) THEN
            CALL CZFINTNM1(JFT  ,JLT  ,THK0  ,A_I   ,DHG  ,
     2                     X13  ,X24  ,Y13  ,Y24  ,VF   ,
     4                     MX13 ,MX23 ,MY13 ,MY23 ,
     3                     G    ,RHO  ,AREA ,AMU  ,DT1  ,
     4                     VL13 ,VL24 ,VLHI ,GBUF%HOURG,
     4                     OFF  ,IPARTC,PARTSAV,KFTS  ,NEL )
	  ELSE
            CALL CZFINTNM(JFT  ,JLT  ,THK0  ,A_I   ,DHG  ,
     2                    X13  ,X24  ,Y13  ,Y24  ,VF   ,
     3                    G    ,RHO  ,AREA ,AMU  ,DT1  ,
     4                    OFF  ,IPARTC,PARTSAV,KFTS  )
	  END IF
	END IF
        CALL CZPROJ1(
     1            JFT    ,JLT    ,VQN    ,VQ     ,VF ,
     2            VM     ,PLAT   ,
     3            F11    ,F12    ,F13    ,F14    ,F21,
     4            F22    ,F23    ,F24    ,F31    ,F32,
     5            F33    ,F34    ,M11    ,M12    ,M13,
     6            M14    ,M21    ,M22    ,M23    ,M24,
     7            M31    ,M32    ,M33    ,M34    ,FZERO,
     8            Z1     ,LXYZ   ,DD     ,DB     ,LXYZ,
     9            ISROT  ,DIZ    ,VMZ    )
C-------------------------
c     Thermique des coques 
C--------------------------
        IF (JTHE > 0) THEN  
          CALL THERMC(JFT    ,JLT     ,PM      ,MAT  ,THK0 ,IXC  ,
     .                Y24    ,PX2     ,PY1     ,PY2  ,AREA ,DT1C ,
     .                TEMP   ,TEMPEL  ,DIE     ,THEM )
        ENDIF 
c--------------------------
c     THERMAL TIME STEP
c--------------------------
        IF (JTHE > 0 .AND. IDT_THERM == 1) THEN
           CALL DTTHERM(
     1   JFT,     JLT,     PM,      TEMPEL,
     2   GBUF%RE, RHO,     GBUF%RK, VOL0,
     3   LL,      MAT,     DT_THERM,OFF,
     4   CONDE,   JTUR)
        ENDIF
C
C--------------------------
C     ASSEMBLE
C--------------------------
      IF (IPARIT == 3) THEN
        CALL CUPDT3F(JFT     ,JLT  ,F     ,M    ,NVC      ,
     2               GBUF%OFF,OFF  ,STI   ,STIR ,STIFN    ,
     3               STIFR   ,IXC  ,PM    ,AREA ,GBUF%THK ,
     4               F11     ,F12  ,F13   ,F14  ,F21      ,
     5               F22     ,F23  ,F24   ,F31  ,F32      ,
     6               F33     ,F34  ,M11   ,M12  ,M13      ,
     7               M14     ,M21  ,M22   ,M23  ,M24      ,
     8               M31     ,M32  ,M33   ,M34  ,GBUF%EINT,
     9               PARTSAV ,MAT  ,IPARTC)
      ELSEIF (IPARIT == 0) THEN
        CALL CUPDTN3(JFT     ,JLT   ,F     ,M    ,NVC      ,
     2               GBUF%OFF,OFF   ,STI   ,STIR ,STIFN    ,
     3               STIFR   ,IXC   ,PM    ,AREA ,GBUF%THK ,
     4               F11     ,F12   ,F13   ,F14  ,F21      ,
     5               F22     ,F23   ,F24   ,F31  ,F32      ,
     6               F33     ,F34   ,M11   ,M12  ,M13      ,
     7               M14     ,M21   ,M22   ,M23  ,M24      ,
     8               M31     ,M32   ,M33   ,M34  ,GBUF%EINT,
     A               PARTSAV ,MAT   ,IPARTC,FACN ,JTHE     ,
     B               THEM    ,FTHE  ,CONDN ,CONDE)
      ELSE 
        CALL CUPDTN3P(JFT      ,JLT     ,GBUF%OFF,OFF   ,STI   ,
     2                STIR     ,FSKY    ,FSKY    ,IADC  ,
     4                F11      ,F12     ,F13     ,F14   ,F21   ,
     5                F22      ,F23     ,F24     ,F31   ,F32   ,
     6                F33      ,F34     ,M11     ,M12   ,M13   ,
     7                M14      ,M21     ,M22     ,M23   ,M24   ,
     8                M31      ,M32     ,M33     ,M34   ,IXC   ,
     A                GBUF%EINT,PARTSAV ,MAT     ,IPARTC,PM    ,
     B                AREA     ,GBUF%THK,FACN    ,JTHE  ,THEM  ,
     C                FTHESKY  ,CONDNSKY,CONDE   )
      ENDIF
C-------------------------
c     Fissure des coques 
C--------------------------
      IF (IXFEM > 0) THEN
        DO ILAY=1,NXLAY
          ! crack length calculation for advancing crack                                            
          CRKLEN => ELBUF_STR%BUFLY(ILAY)%DMG(1:NEL)
          CALL  CRKLEN4N_ADV(
     .          NEL       ,NFT       ,ILAY      ,NLAY      ,IXC       ,
     .          CRKLEN    ,ELCRKINI  ,IEL_CRK   ,DIR1_CRK  ,DIR2_CRK  ,     
     .          NODEDGE   ,CRKEDGE   ,XEDGE4N   ,NGL       ,XL2       ,
     .          XL3       ,XL4       ,YL2       ,YL3       ,YL4       ,
     .          LL        )
c
          ! crack advancement                                             
          CALL CRKLAYER4N_ADV(
     .         XFEM_STR ,NEL      ,NFT      ,IXC      ,ELCUTC   ,             
     .         ILAY     ,NXLAY    ,IEL_CRK  ,INOD_CRK ,             
     .         IADC_CRK ,NODENR   ,ELCRKINI ,DIR1_CRK ,DIR2_CRK ,             
     .         NODEDGE  ,CRKNODIAD,KNOD2ELC ,CRKEDGE  ,A_I      ,        
     .         XL2      ,XL3      ,XL4      ,YL2      ,YL3      ,
     .         YL4      ,XEDGE4N  ,NGL      )                                                         
c
          ! crack initialization                                               
          CALL CRKLAYER4N_INI(
     .         XFEM_STR ,NEL      ,NFT      ,IXC      ,ELCUTC   ,
     .         ILAY     ,NXLAY    ,IEL_CRK  ,INOD_CRK ,
     .         IADC_CRK ,NODENR   ,ELCRKINI ,DIR1_CRK ,DIR2_CRK ,
     .         NODEDGE  ,CRKNODIAD,KNOD2ELC ,CRKEDGE  ,A_I      ,
     .         XL2      ,XL3      ,XL4      ,YL2      ,YL3      ,
     .         YL4      ,XEDGE4N  ,NGL      )
        ENDDO                                                              
C
        CALL CRKOFFC(ELBUF_STR,XFEM_STR  ,
     .               JFT      ,JLT       ,NFT    ,IR      ,IS        ,
     .               NXLAY    ,IEL_CRK   ,CRKEDGE,XEDGE4N )
      ENDIF 

c--------------------------------------------
c     Front wave
c--------------------------------------------
      IF (IFAILWAVE > 0) THEN
        CRKDIR => ELBUF_STR%BUFLY(1)%CRKDIR
c         
        CALL SET_FAILWAVE_NOD4(FAILWAVE   ,FWAVE_EL ,NGL      ,
     .       NEL      ,IXC      ,ITAB     ,CRKDIR   ,DIR_A    ,
     .       L_DIRA   ,XL2      ,XL3      ,XL4      ,YL2      ,
     .       YL3      ,YL4      )
      ENDIF
C-----------
      IF (ALLOCATED(DIR2_CRK)) DEALLOCATE(DIR2_CRK)
      IF (ALLOCATED(DIR1_CRK)) DEALLOCATE(DIR1_CRK)
      IF (ALLOCATED(ELCRKINI)) DEALLOCATE(ELCRKINI)
      IF (ALLOCATED(DIRB))     DEALLOCATE(DIRB)
      IF (ALLOCATED(DIRA))     DEALLOCATE(DIRA)
      IF (ALLOCATED(VAR_REG))  DEALLOCATE(VAR_REG)
C-----------
      RETURN
      END
