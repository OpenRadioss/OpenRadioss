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
Chd|  CFORC3                        source/elements/shell/coque/cforc3.F
Chd|-- called by -----------
Chd|        FORINTC                       source/elements/forintc.F     
Chd|-- calls ---------------
Chd|        CBILAN                        source/elements/shell/coque/cbilan.F
Chd|        CCOEF3                        source/elements/shell/coque/ccoef3.F
Chd|        CCOOR3                        source/elements/shell/coque/ccoor3.F
Chd|        CCOORT3                       source/elements/shell/coque/ccoor3.F
Chd|        CCURV3                        source/elements/shell/coque/ccurv3.F
Chd|        CDEFO3                        source/elements/shell/coque/cdefo3.F
Chd|        CDEFOT3                       source/elements/shell/coque/cdefo3.F
Chd|        CDERI3                        source/elements/shell/coque/cderi3.F
Chd|        CDLEN3                        source/elements/shell/coque/cdlen3.F
Chd|        CDT3                          source/elements/shell/coque/cdt3.F
Chd|        CEVEC3                        source/elements/shell/coque/cevec3.F
Chd|        CFINT3                        source/elements/shell/coque/cfint3.F
Chd|        CFINT_REG                     source/elements/shell/coque/cfint_reg.F
Chd|        CHSTI3                        source/elements/shell/coque/chsti3.F
Chd|        CHVIS3                        source/elements/shell/coque/chvis3.F
Chd|        CMAIN3                        source/materials/mat_share/cmain3.F
Chd|        CNVEC3                        source/elements/shell/coque/cnvec3.F
Chd|        CPXPY3                        source/elements/shell/coque/cpxpy3.F
Chd|        CRKLAYER4N_ADV                source/elements/xfem/crklayer4n_adv.F
Chd|        CRKLAYER4N_INI                source/elements/xfem/crklayer4n_ini.F
Chd|        CRKLEN4N_ADV                  source/elements/xfem/crklen4n_adv.F
Chd|        CRKOFFC                       source/elements/xfem/precrklay.F
Chd|        CSENS3                        source/elements/shell/coque/csens3.F
Chd|        CSTRA3                        source/elements/shell/coque/cstra3.F
Chd|        CSUBC3                        source/elements/shell/coque/csubc3.F
Chd|        CSUBF3                        source/elements/shell/coque/csubf3.F
Chd|        CUPDT3                        source/elements/shell/coque/cupdt3.F
Chd|        CUPDT3F                       source/elements/shell/coque/cupdt3.F
Chd|        CUPDT3P                       source/elements/shell/coque/cupdt3.F
Chd|        DTTHERM                       source/elements/sh3n/coquedk/cndt3.F
Chd|        MHVIS3                        source/airbag/mhvis3.F        
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
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
       SUBROUTINE CFORC3 (ELBUF_STR,
     1            JFT     ,JLT     ,PM       ,IXC      ,X       ,
     2            F       ,M       ,V        ,VR       ,FAILWAVE,
     3            NVC     ,MTN     ,GEO      ,TF       ,NPF     ,
     4            BUFMAT  ,PARTSAV ,DT2T     ,NELTST   ,ITYPTST ,
     5            STIFN   ,STIFR   ,FSKY     ,IADC     ,ITAB    ,
     6            D       ,DR      ,TANI     ,OFFSET   ,EANI    ,
     7            F11     ,F12     ,F13      ,F14      ,F21     ,
     8            F22     ,F23     ,F24      ,F31      ,F32     ,
     9            F33     ,F34     ,M11      ,M12      ,M13     ,
     A            M14     ,M21     ,M22      ,M23      ,M24     ,
     B            M31     ,M32     ,M33      ,M34      ,INDXOF  ,
     C            IPARTC  ,THKE    ,GROUP_PARAM,MATPARAM_TAB,
     F            NEL     ,ISTRAIN ,IHBE     ,
     G            ITHK    ,IOFC    ,IPLA     ,NFT      ,ISMSTR  ,
     H            NPT     ,KFTS    ,
     I            FZERO   ,IGEO    ,IPM      ,IFAILURE ,ITASK   ,
     J            JTHE    ,TEMP    , FTHE    ,FTHESKY  ,IEXPAN  ,
     K            GRESAV  ,GRTH    ,XEDGE4N  ,
     L            IGRTH   ,MSC     ,DMELC    ,JSMS     ,TABLE   ,
     M            IPARG   ,IXFEM   ,KNOD2ELC ,LSENSOR  ,SENSOR  ,
     N            ELCUTC  ,INOD_CRK,IEL_CRK  ,IBORDNODE,NODENR  ,
     O            IADC_CRK,NODEDGE ,CRKNODIAD,CONDN   ,
     P            CONDNSKY,STACK   ,ISUBSTACK,XFEM_STR ,CRKEDGE ,
     R            DRAPE_Q4  ,IPRI     ,NLOC_DMG )
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
#include      "scr01_c.inc"
#include      "scr02_c.inc"
#include      "scr03_c.inc"
#include      "scr07_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "com_xfem1.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
#include      "warn_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NVC,MTN,NELTST,ITYPTST,OFFSET,
     .   NEL,ISTRAIN,IHBE,ITASK,JTHE,JSMS,
     .   ITHK,IOFC,IPLA,NFT,ISMSTR,NPT,KFTS,IFAILURE,IEXPAN,
     .   ISUBSTACK,IPRI
      INTEGER NPF(*),IXC(NIXC,*),IADC(4,*),INDXOF(MVSIZ),IPARTC(*),
     .   IGEO(NPROPGI,*),IPM(NPROPMI,*),XEDGE4N(4,*),ITAB(*),
     .   GRTH(*),IGRTH(*),IPARG(*),IXFEM,KNOD2ELC(*),LSENSOR(NSENSI,*),
     .   ELCUTC(2,*),INOD_CRK(*),IEL_CRK(*),IBORDNODE(*),
     .   NODENR(*),IADC_CRK(4,*),NODEDGE(2,*),CRKNODIAD(*)
      my_real
     .   F11(MVSIZ),F12(MVSIZ),F13(MVSIZ),F14(MVSIZ),
     .   F21(MVSIZ),F22(MVSIZ),F23(MVSIZ),F24(MVSIZ),
     .   F31(MVSIZ),F32(MVSIZ),F33(MVSIZ),F34(MVSIZ),
     .   M11(MVSIZ),M12(MVSIZ),M13(MVSIZ),M14(MVSIZ),
     .   M21(MVSIZ),M22(MVSIZ),M23(MVSIZ),M24(MVSIZ),
     .   M31(MVSIZ),M32(MVSIZ),M33(MVSIZ),M34(MVSIZ),
     .   PM(*),X(*),F(*),M(*),V(*),VR(*),
     .   GEO(NPROPG,*),TF(*), BUFMAT(*),PARTSAV(*),STIFN(*),STIFR(*),
     .   FSKY(*),D(*),DR(*),TANI(6,*),EANI(*), THKE(*),DT2T,
     .   FZERO(3,4,*),TEMP(*), FTHE(*),FTHESKY(*),GRESAV(*),
     .   MSC(*), DMELC(*),SENSOR(NSENSR,*),CONDN(*),
     .   CONDNSKY(*)
C
      TYPE (TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (ELBUF_STRUCT_), DIMENSION(NXEL), TARGET :: XFEM_STR  ! take xfem_str
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
      TYPE (FAILWAVE_STR_) :: FAILWAVE 
      TYPE (GRP_PARAM_STRUCT_) :: GROUP_PARAM
      TYPE (NLOCAL_STR_), TARGET :: NLOC_DMG
      TYPE (DRAPE_) :: DRAPE_Q4
      TYPE (MATPARAM_STRUCT_)  , DIMENSION(NUMMAT) :: MATPARAM_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MAT(MVSIZ),PID(MVSIZ),NDT(MVSIZ),NGL(MVSIZ),FWAVE_EL(NEL),
     .   I,J,IGTYP,ICSEN,IFLAG,IUN,NPG,ILAY,NLAY,IXEL,IXLAY,NXLAY,N1,N2,N3,N4,
     .   IBID,NG,IR,IS,L_DIRA,L_DIRB,J1,J2,IGMAT,NPTTOT,IREP,IFAILWAVE
      my_real 
     .   STI(MVSIZ),STIR(MVSIZ),SIGY(MVSIZ),RHO(MVSIZ),
     .   X2(MVSIZ),X3(MVSIZ),X4(MVSIZ),Y2(MVSIZ),Y3(MVSIZ),
     .   Y4(MVSIZ),Z2(MVSIZ),SSP(MVSIZ),VISCMX(MVSIZ),
     .   VX1(MVSIZ), VX2(MVSIZ), VX3(MVSIZ), VX4(MVSIZ),
     .   VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), VY4(MVSIZ),
     .   VZ1(MVSIZ), VZ2(MVSIZ), VZ3(MVSIZ), VZ4(MVSIZ),
     .   VX13(MVSIZ),VX24(MVSIZ),VY13(MVSIZ),VY24(MVSIZ),
     .   VZ13(MVSIZ),VZ24(MVSIZ),THK02(MVSIZ), 
     .   X1G(MVSIZ), X2G(MVSIZ), X3G(MVSIZ), X4G(MVSIZ),
     .   Y1G(MVSIZ), Y2G(MVSIZ), Y3G(MVSIZ), Y4G(MVSIZ),
     .   Z1G(MVSIZ), Z2G(MVSIZ), Z3G(MVSIZ), Z4G(MVSIZ),
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ), E2X(MVSIZ),
     .   E2Y(MVSIZ), E2Z(MVSIZ), E3X(MVSIZ), E3Y(MVSIZ),E3Z(MVSIZ)
      my_real 
     .   EXX(MVSIZ), EYY(MVSIZ), EXY(MVSIZ), EXZ(MVSIZ),EYZ(MVSIZ),
     .   KXX(MVSIZ), KYY(MVSIZ), KXY(MVSIZ), PX1(MVSIZ),
     .   PX2(MVSIZ), PY1(MVSIZ), PY2(MVSIZ), THK0(MVSIZ),
     .   OFF(MVSIZ), NU(MVSIZ) , SHF(MVSIZ), AREA(MVSIZ),
     .   G(MVSIZ)  , YM(MVSIZ) , A11(MVSIZ), A12(MVSIZ),
     .   VL1(MVSIZ,3),VL2(MVSIZ,3),VL3(MVSIZ,3),VL4(MVSIZ,3),
     .   VRL1(MVSIZ,3),VRL2(MVSIZ,3),VRL3(MVSIZ,3),VRL4(MVSIZ,3), 
     .   DT1C(MVSIZ), DT2C(MVSIZ),
     .   ALDT(MVSIZ),ALPE(MVSIZ),VHX(MVSIZ),VHY(MVSIZ)
      my_real 
     .   H1(MVSIZ),  H2(MVSIZ),  H3(MVSIZ),  VOL0(MVSIZ),VOL00(MVSIZ),
     .   H11(MVSIZ), H12(MVSIZ), H13(MVSIZ), H14(MVSIZ),  
     .   H21(MVSIZ), H22(MVSIZ), H23(MVSIZ), H24(MVSIZ),  
     .   H31(MVSIZ), H32(MVSIZ), H33(MVSIZ), H34(MVSIZ),  
     .   B11(MVSIZ), B12(MVSIZ), B13(MVSIZ), B14(MVSIZ),  
     .   B21(MVSIZ), B22(MVSIZ), B23(MVSIZ), B24(MVSIZ),  
     .   RX1(MVSIZ), RX2(MVSIZ), RX3(MVSIZ), RX4(MVSIZ), 
     .   RY1(MVSIZ), RY2(MVSIZ), RY3(MVSIZ), RY4(MVSIZ),
     .   ZCFAC(MVSIZ,2),GS(MVSIZ),
     .   SRH1(MVSIZ),SRH2(MVSIZ),SRH3(MVSIZ),A_I(MVSIZ),
     .   DIE(MVSIZ),TEMPEL(MVSIZ),THEM(MVSIZ,4),BID,
     .   UX1(MVSIZ),UX2(MVSIZ),UX3(MVSIZ),UX4(MVSIZ),
     .   UY1(MVSIZ),UY2(MVSIZ),UY3(MVSIZ),UY4(MVSIZ),
     .   DSUB(MVSIZ,3,4),DRSUB(MVSIZ,3,4),TSUB(MVSIZ),
     .   DTCSUB(MVSIZ),AREAS(MVSIZ),CONDE(MVSIZ),A11R(MVSIZ)

      my_real 
     .  AREAT(MVSIZ),PX1T(MVSIZ),PX2T(MVSIZ),PY1T(MVSIZ),PY2T(MVSIZ),
     .  F_DEF(MVSIZ,8), U13X(MVSIZ),U24X(MVSIZ),U13Y(MVSIZ),U24Y(MVSIZ),
     .  WXY(MVSIZ)
C
      INTEGER, DIMENSION(NEL) :: OFFLY
      INTEGER, ALLOCATABLE, DIMENSION(:) :: ELCRKINI
      my_real, ALLOCATABLE, DIMENSION(:) :: DIRA,DIRB,DIR1_CRK,DIR2_CRK
      my_real, DIMENSION(:) ,POINTER     :: DIR_A, DIR_B,CRKDIR,CRKLEN,DADV
      TARGET :: DIRA,DIRB
C--- Variables pour le non-local
      INTEGER :: NDDL, K, INOD(4),NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ),
     .           IPOS(4), L_NLOC, INLOC
      my_real, DIMENSION(:,:), ALLOCATABLE :: VAR_REG
      my_real, DIMENSION(:), POINTER :: DNL
C-----
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF
      TYPE (STACK_PLY) :: STACK
C=======================================================================
      GBUF => ELBUF_STR%GBUF
C-----
      IUN = 1
      IBID = 0
      BID = ZERO
      NLAY = ELBUF_STR%NLAY
cc      NPT  = MAX(NLAY,NPTT) --> set to = IPARG(6) , keeping it original
C                                 to allow for NPT = 0 (global LAW_3)
      NPG  = 1
      IR   = 1
      IS   = 1
      NG   = 1
      IXEL  = 0
      IXLAY = 0
      IREP = IPARG(35)
      INLOC = IPARG(78)
C
      NPTTOT  = 0
      DO ILAY=1,NLAY
        NPTTOT = NPTTOT + ELBUF_STR%BUFLY(ILAY)%NPTT
      ENDDO
!-------------------------------------------      
! Tableau pour la variable non-locale 
      NDDL = NPTTOT
      ALLOCATE(VAR_REG(NEL,NDDL)) 
!-------------------------------------------
      IF (NPT == 0) NPTTOT = NPT  !  compatibility with global integration
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
c-------------------------------------
      NXLAY = NLAY
C---
      IF (IXFEM > 0) THEN
        ALLOCATE(ELCRKINI(NXLAYMAX*NEL))
        ALLOCATE(DIR1_CRK(NXLAYMAX*NEL))
        ALLOCATE(DIR2_CRK(NXLAYMAX*NEL))
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
      ENDIF
C----------------------
      IF(ISUB == 0)THEN
C       X1G-> X1,X2G-> X2,X3G-> X3,X4G-> X4,
C Y1G-> Y1,Y2G-> Y2,Y3G-> Y3,Y4G-> Y4,Z1G-> Z1,Z2G-> Z2,Z3G-> Z3,Z4G-> Z4
        CALL CCOOR3(JFT  ,JLT  ,X    ,IXC  ,GEO  ,GBUF%OFF,
     2              OFF  ,SIGY ,PID ,V ,VR ,VL1,VL2,VL3,VL4,
     3              VRL1,VRL2,VRL3,VRL4,MAT,DT1C,THKE ,THK0 ,NGL,
     4              X1G  ,X2G  ,X3G  ,X4G  ,Y1G  ,Y2G    ,
     5              Y3G  ,Y4G  ,Z1G  ,Z2G  ,Z3G  ,Z4G    )
      ELSE
        CALL CSUBC3(JFT     ,JLT    ,X    ,IXC     ,GEO ,GBUF%OFF ,
     2              OFF     ,SIGY   ,PID  ,D       ,DR   ,DSUB,
     3            DRSUB,VL1,VL2,VL3,VL4,VRL1,VRL2,VRL3,VRL4,NDT ,PM ,V,
     4            GBUF%THK,GBUF%EINT,PARTSAV,TSUB,DTCSUB,AREAS,
     5            VR     ,MAT     ,NGL   ,DT1C   ,DT2C ,THKE,THK0,
     6            X1G    ,X2G     ,X3G   ,X4G    ,Y1G  ,Y2G    ,
     7            Y3G    ,Y4G     ,Z1G   ,Z2G    ,Z3G  ,Z4G    ,
     8            IPARTC ,IADC  ,FSKY   ,FSKY ,GRESAV   ,
     9            GRTH   ,IGRTH   ,ITASK)
        IF(JLT < JFT)RETURN
      ENDIF
C
      ICSEN= IGEO(3,PID(1))
      IGTYP= IGEO(11,PID(1))
!!      IF(IGTYP == 11 .AND. IDTMINS > 0)IGEO(98,PID(1)) = 1
       IGMAT= IGEO(98,PID(1))
C
      IF (ISHFRAM == 1) THEN
C       repere non convecte (version 3) 
        CALL CEVEC3(ELBUF_STR ,DIR_A,DIR_B,
     1    JFT ,JLT ,X1G ,X2G ,X3G ,X4G ,Y1G ,Y2G ,Y3G ,Y4G ,
     2    Z1G ,Z2G ,Z3G ,Z4G ,E1X ,E1Y ,E1Z ,E2X ,E2Y ,E2Z , 
     3    E3X ,E3Y ,E3Z ,IREP,NLAY,NEL)
      ELSE
C       repere orthogonalise convecte
        CALL CNVEC3(ELBUF_STR ,DIR_A   ,DIR_B   ,
     1       JFT     ,JLT     ,IREP    ,IGTYP   ,NLAY    ,
     2       X1G     ,X2G     ,X3G     ,X4G     ,Y1G     ,Y2G     ,       
     3       Y3G     ,Y4G     ,Z1G     ,Z2G     ,Z3G     ,Z4G     ,       
     4       E1X     ,E1Y     ,E1Z     ,E2X     ,E2Y     ,E2Z     ,       
     5       E3X     ,E3Y     ,E3Z     ,NEL     )
      ENDIF
C
      IF (ISMSTR /= 3)THEN
       CALL CDERI3(JFT ,JLT ,GBUF%SMSTR,GBUF%OFF,
     1             STI ,STIR ,
     2             AREA,PX1 ,PX2 ,PY1 ,PY2 ,X2  ,X3  ,X4  ,Y2  ,Y3  ,  
     3             Y4  ,Z2  ,X1G ,X2G ,X3G ,X4G ,Y1G ,Y2G ,Y3G ,Y4G ,
     4             Z1G ,Z2G ,Z3G ,Z4G ,E1X ,E1Y ,E1Z ,E2X ,E2Y ,E2Z ,
     5             E3X ,E3Y ,E3Z ,VHX ,VHY ,A_I ,UX1 ,UX2 ,UX3 ,UX4 ,
     6             UY1 ,UY2 ,UY3 ,UY4 ,NEL )
      ELSE
       CALL CPXPY3(JFT       ,JLT     ,PM      ,STI     ,STIR    ,
     2             GBUF%SMSTR,PX1     ,PX2     ,PY1     ,PY2     ,
     3             IXC       ,AREA    ,X2      ,X3      ,X4      , 
     4             Y2        ,Y3      ,Y4      ,Z2      ,THK0    ,  
     5             MAT       ,NEL     )
      ENDIF
C
      CALL CCOEF3(JFT     ,JLT    ,PM     ,MAT      ,GEO     ,
     2            PID     ,OFF    ,AREA   ,STI      ,STIR    ,
     3            SHF     ,THK0   ,THK02  ,NU       ,
     4            G       ,YM     ,A11    ,A12      ,GBUF%THK,
     5            SSP     ,RHO    ,H1     ,H2       ,H3      ,
     6            VOL0    ,VOL00  ,ALPE   ,GS       ,MTN     ,
     7            ITHK    ,ISMSTR  ,NPTTOT ,KFTS   ,
     8            SRH1   ,SRH2   ,SRH3     ,IGEO    ,
     9            A11R    ,ISUBSTACK      ,STACK%PM )
C
      IF ((NODADT == 0 .AND. ISMSTR /= 3 .AND. IDT1SH == 0) .OR.
     .     IDTMIN(3) /= 0 .OR. IGTYP == 16.OR.IDT_THERM == 1) THEN
C      CDTLEN must be always called for tissue property/law for time step correction
       CALL CDLEN3(JFT     ,JLT     ,PM      ,OFF     ,AREA,
     2             X2      ,X3      ,X4      ,Y2      ,Y3  ,
     3             Y4      ,ALDT    ,MAT     ,GEO     ,PID ,
     4             IHBE    )
      ENDIF
      CALL CDEFO3(JFT ,JLT,VL1,VL2,VL3,VL4,DT1C,PX1 ,PX2 ,PY1,PY2,AREA,
     2            EXX ,EYY,EXY,EXZ ,EYZ ,X2  ,X3 ,X4 ,Y2  ,Y3,
     3            Y4  ,Z2 ,VX1,VX2 ,VX3 ,VX4 ,VY1,VY2,VY3 ,VY4 ,
     4            VZ1 ,VZ2,VZ3,VZ4 ,E1X ,E1Y ,E1Z,E2X,E2Y ,E2Z ,
     5            E3X ,E3Y,E3Z,IHBE)
      CALL CCURV3(JFT ,JLT ,VRL1,VRL2,VRL3,VRL4 ,PX1 
     1            ,PX2 ,PY1 ,PY2 ,AREA,
     2            RX1 ,RX2 ,RX3 ,RX4 ,RY1 ,RY2 ,RY3 ,RY4 ,
     3            E1X ,E1Y ,E1Z ,E2X ,E2Y ,E2Z ,E3X ,E3Y ,
     4            E3Z ,KXX ,KYY ,KXY ,EXZ ,EYZ ,
     5            WXY ,ISMSTR)
      IF (ISMSTR == 10 ) THEN
        CALL CCOORT3(JFT    ,JLT    ,X     ,IXC   ,GBUF%OFF   ,
     1               DR     ,PX1T  ,PX2T  ,PY1T   ,PY2T       ,
     2               E1X    ,E1Y   ,E1Z   ,E2X    ,E2Y        ,
     3               E2Z    ,E3X   ,E3Y   ,E3Z    ,AREAT      , 
     4               U13X   ,U24X  ,U13Y  ,U24Y   ,GBUF%SMSTR ,
     5               NEL    )
C  
        CALL CDEFOT3(JFT ,JLT  ,PX1T  ,PX2T  ,PY1T   ,
     2              PY2T ,U13X ,U24X  ,U13Y  ,U24Y   ,
     3              F_DEF )
      END IF !(ISMSTR == 10 ) THEN
      CALL CSTRA3(JFT    ,JLT       ,GBUF%STRA,SHF    ,AREA , 
     2            EXX    ,EYY       ,EXY      ,EXZ    ,EYZ  ,   
     3            KXX    ,KYY       ,KXY      ,DT1C   ,TANI , 
     4            GBUF%FOR,GBUF%MOM ,ISMSTR ,MTN  ,
     6            IHBE   ,NFT       ,ISTRAIN  ,UX1    ,UX2  ,
     7            UX3    ,UX4       ,UY1      ,UY2    ,UY3  ,
     8            UY4    ,PX1       ,PX2      ,PY1    ,PY2  ,
     9            WXY    ,GBUF%STRW ,F_DEF    ,NEL    )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      IF (JTHE > 0 ) THEN
        CALL TEMPCG(JFT   ,JLT   ,PM     ,MAT   ,IXC     ,
     .              TEMP ,TEMPEL )
      ENDIF
c-------------------------------------------
c    COMPUTE Regularized non local variable in Gauss point
c-------------------------------------------     
      IF (INLOC > 0) THEN
        L_NLOC = NLOC_DMG%L_NLOC
        DNL  => NLOC_DMG%DNL(1:L_NLOC) ! DNL = non local variable increment
        DO I=JFT,JLT
          NC1(I)  = IXC(2,I)
          NC2(I)  = IXC(3,I)
          NC3(I)  = IXC(4,I)
          NC4(I)  = IXC(5,I)
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
C
      IF ((IMON_MAT==1).AND. ITASK == 0)CALL STARTIME(35,1)
C-----------------------------
        CALL CMAIN3(
     1  ELBUF_STR   ,JFT         ,JLT         ,NFT        ,IPARG      ,
     2  NEL         ,MTN         ,IPLA        ,ITHK       ,GROUP_PARAM,
     3  PM          ,GEO         ,NPF         ,TF         ,BUFMAT     ,
     4  SSP         ,RHO         ,VISCMX      ,DT1C       ,SIGY       ,
     5  AREA        ,EXX         ,EYY         ,EXY        ,EXZ        ,
     6  EYZ         ,KXX         ,KYY         ,KXY        ,NU         ,
     7  OFF         ,THK0        ,MAT         ,PID        ,MATPARAM_TAB,
     8  GBUF%FOR    ,GBUF%MOM    ,GBUF%STRA   ,FAILWAVE   ,FWAVE_EL   ,
     9  GBUF%THK    ,GBUF%EINT   ,IOFC        ,
     A  G           ,A11         ,A12         ,VOL0       ,INDXOF     ,
     B  NGL         ,ZCFAC       ,SHF         ,GS         ,GBUF%EPSD  ,
     C  KFTS        ,IHBE        ,ALPE        ,
     D  DIR_A       ,DIR_B       ,IGEO        ,
     E  IPM         ,IFAILURE    ,NPG         ,
     F  TEMPEL      ,DIE         ,JTHE        ,IEXPAN     ,GBUF%TEMP  ,
     G  IBID        ,BID         ,
     H  BID         ,BID         ,BID         ,BID        ,BID        ,
     I  BID         ,BID         ,BID         ,E1X        ,E1Y        ,
     J  E1Z         ,E2X         ,E2Y         ,E2Z        ,E3X        ,
     K  E3Y         ,E3Z         ,NG          ,TABLE      ,IXFEM      ,
     L  BID         ,LSENSOR     ,SENSOR      ,BID        ,ELCRKINI   ,
     M  DIR1_CRK    ,DIR2_CRK    ,ALDT        ,
     N  ISMSTR      ,IR          ,IS          ,NLAY       ,NPT        ,
     O  IXLAY       ,IXEL        ,ISUBSTACK   ,STACK      ,
     P  F_DEF       ,ITASK       ,DRAPE_Q4    ,VAR_REG    ,NLOC_DMG   )
C-----------------------------
      IF ((IMON_MAT==1).AND. ITASK == 0)CALL STOPTIME(35,1)
C-----------------------------
C     FORCES ANTI-SABLIER
C-----------------------------
      DO I=JFT,JLT
        VISCMX(I) = SQRT(ONE + VISCMX(I)*VISCMX(I)) - VISCMX(I)
      ENDDO
c---------------------------------------------------
      IF (IABS(NPTTOT) == 1) THEN
        CALL MHVIS3(JFT    ,JLT    ,PM     ,GBUF%THK,GBUF%HOURG,
     2              OFF    ,PX1    ,PX2    ,PY1     ,PY2       ,
     3              IXC    ,DT1C   ,SSP    ,RHO     ,STI       ,
     4              EANI   ,GEO    ,PID    ,STIR    ,MAT       ,
     5              THK0   ,VISCMX ,ALPE   ,IPARTC  ,PARTSAV   ,
     6              IHBE   ,NFT    ,ISMSTR  ,RX1       ,
     7              RX2    ,RX3    ,RX4    ,RY1     ,RY2       ,
     8              RY3    ,RY4    ,VX1    ,VX2     ,VX3       ,
     9              VX4    ,VY1    ,VY2    ,VY3     ,VY4       ,
     A              VZ1    ,VZ2    ,VZ3    ,VZ4     ,B11       ,
     B              B12    ,B13    ,B14    ,B21     ,B22       ,
     C              B23    ,B24    ,AREA   ,YM      ,NU        ,
     D              VHX    ,VHY    ,H11    ,H12     ,H13       ,
     E              H14    ,H21    ,H22    ,H23     ,H24       ,
     F              H31    ,H32    ,H33    ,H34     ,H1        ,
     G              H2     ,IGEO   ,NEL    )
      ELSEIF(IHBE == 2)THEN
        CALL CHSTI3(JFT    ,JLT    ,GBUF%THK,GBUF%HOURG,OFF    ,PX1 ,
     2              PX2    ,PY1    ,PY2     ,SIGY      ,IXC    ,DT1C,
     3              SSP    ,RHO    ,STI     ,Z2        ,EANI   ,STIR,
     4              SHF    ,THK0   ,THK02   ,VISCMX    ,G      ,A11 ,
     5   H1  ,H2  ,H3 ,YM   ,NU  , ALPE   , 
     6   VHX ,VHY ,VX1 ,VX2 ,VX3 ,VX4 ,VY1 ,
     7   VY2 ,VY3 ,VY4,VZ1  ,VZ2  ,VZ3 ,VZ4 ,AREA   ,
     8   H11 ,H12 ,H13 ,H21 ,H22 ,H23  ,H31  ,H32  ,H33  ,
     9   B11 ,B12 ,B13 ,B14 ,B21 ,B22  ,B23  ,B24  ,
     A   RX1 ,RX2 ,RX3 ,RX4 ,RY1 ,RY2  ,RY3  ,RY4, 
     B   IPARTC,PARTSAV,
     C   IHBE   ,NFT     ,ISMSTR,SRH3,IGTYP  ,
     D   IGMAT  ,A11R  ,NEL)
      ELSE
        CALL CHVIS3(
     2   JFT ,JLT ,GBUF%THK,GBUF%HOURG,OFF ,PX1 ,PX2 ,PY1 ,PY2 ,
     3   IXC ,DT1C,SSP,RHO  ,STI  ,VX1 ,VX2 ,VX3 ,VX4 ,VY1 ,
     4   VY2 ,VY3 ,VY4,VZ1  ,VZ2  ,VZ3 ,VZ4 ,AREA,THK0,VHX ,
     5   VHY ,SHF ,Z2 ,EANI ,STIR,VISCMX,G  ,A11 ,
     6   H1  ,H2  ,H3 ,YM   ,NU  ,THK02,ALPE,H11 ,
     7   H12 ,H13 ,H21 ,H22 ,H23 ,H31  ,H32 ,H33 ,
     8   B11 ,B12 ,B13 ,B14 ,B21 ,B22  ,B23 ,B24 ,
     9   RX1 ,RX2 ,RX3 ,RX4 ,RY1 ,RY2  ,RY3 ,RY4 , 
     A   IPARTC,PARTSAV,
     B   IHBE   ,NFT     ,ISMSTR,KFTS   ,
     C    SRH1, SRH2, SRH3   ,IGTYP ,
     D   IGMAT   ,A11R  ,NEL)
      ENDIF
C--------------------------
C     BILANS PAR MATERIAU
C--------------------------
c      IFLAG=MOD(NCYCLE,NCPRI)
      IF (IPRI>0) THEN
         CALL CBILAN(JFT    ,JLT      ,PM     ,V     ,IXC   , 
     2              GBUF%THK,GBUF%EINT,PARTSAV,AREA  ,MAT   , 
     3              IPARTC  ,X        ,VR     ,VOL0  ,VOL00 , 
     4              THK0    ,THK02    ,1      ,OFF   , 
     5              NFT     ,GRESAV   ,GRTH   ,IGRTH ,        
     6              VL1     ,VL2      ,VL3    ,VL4   ,VRL1  , 
     7              VRL2    ,VRL3     ,VRL4   ,X1G   ,X2G   , 
     8              X3G     ,X4G      ,Y1G    ,Y2G   ,Y3G   , 
     9              Y4G     ,Z1G      ,Z2G    ,Z3G   ,Z4G   , 
     A              IBID    ,IEXPAN   ,GBUF%EINTTH,ITASK)                                   
       ENDIF
C--------------------------
C     PAS DE TEMPS
C--------------------------
      IF (ISMSTR /= 3.AND.(NODADT == 0.OR.IDTMIN(3) /= 0))THEN
        CALL CDT3(JFT     ,JLT    ,YM        ,OFF      ,DT2T    ,
     2            NELTST  ,ITYPTST,STI       ,STIR     ,GBUF%OFF,
     3            DTCSUB  ,NDT    ,DT2C      ,IXC      ,SSP     ,
     4            VISCMX  ,PX1    ,PX2       ,PY1      ,PY2     ,
     5            VOL0    ,VOL00  ,RHO       ,ALDT     ,ALPE    ,
     6            INDXOF  ,NGL    ,ISMSTR    ,IOFC     ,MSC     ,
     7            DMELC   ,JSMS   ,GBUF%G_DT ,GBUF%DT)
      ENDIF
C----------------------------
C     FORCES INTERNES + ASSEMBLE
C----------------------------
      CALL CFINT3(JFT    ,JLT    ,GBUF%FOR,GBUF%MOM,THK0   ,THK02  ,
     2            PX1    ,PX2    ,PY1     ,PY2     ,AREA   ,Z2     ,
     3            F11    ,F12    ,F13     ,F14     ,F21    ,F22    ,
     4            F23    ,F24    ,F31     ,F32     ,F33    ,F34    ,
     5            H11    ,H12    ,H13     ,H21     ,H22    ,H23    ,
     6            H31    ,H32    ,H33     ,B11     ,B12    ,B13    ,
     7            B14    ,B21    ,B22     ,B23     ,B24    ,NEL    ,
     8            M11    ,M12    ,M13     ,M14     ,M21    ,M22    , 
     9            M23    ,M24    ,M31     ,M32     ,M33    ,M34    ,
     A            E1X    ,E1Y    ,E1Z     ,E2X     ,E2Y    ,E2Z    ,
     B            E3X    ,E3Y    ,E3Z     ,IHBE    ,NPTTOT ,FZERO  )
C-------------------------
c     Thermique des coques 
C--------------------------
C
       IF (JTHE > 0) THEN
        CALL THERMC(JFT   ,JLT   ,PM     ,MAT     ,THK0  ,IXC     ,
     .              PX1   ,PX2   ,PY1    ,PY2     ,AREA  ,DT1C    ,
     .              TEMP  ,TEMPEL,DIE   , THEM    ) 
       ENDIF
C--------------------------
C     THERMAL TIME STEP
C--------------------------
      IF (JTHE > 0 .AND. IDT_THERM == 1) THEN
        CALL DTTHERM(JFT  ,JLT  ,PM  ,TEMPEL,GBUF%RE,   
     2               RHO  ,GBUF%RK,VOL0 ,ALDT,MAT ,     
     3               DT_THERM,OFF,CONDE)                
      ENDIF 
c-------------------------
c     Virtual internal forces of regularized non local ddl 
c--------------------------
      IF (INLOC > 0) THEN
        ! Warning : Y24 = PX1
        CALL CFINT_REG(NLOC_DMG,VAR_REG ,GBUF%THK,NEL     ,
     .                 OFF     ,AREA    ,NC1     ,NC2     ,
     .                 NC3     ,NC4     ,PX1     ,PY1     ,
     .                 PX2     ,PY2     ,ELBUF_STR%NLOC(1,1), 
     .                 IXC(1,JFT),NDDL  ,ITASK   ,DT2T    ,
     .                 ALDT    )
      ENDIF 
C--------------------------
      IF (ISUB /= 0) THEN
        CALL CSUBF3(JFT  ,JLT  ,NDT ,AREAS,AREA,
     4            F11    ,F12  ,F13 ,F14  ,F21 ,
     5            F22    ,F23  ,F24 ,F31  ,F32 ,
     6            F33    ,F34  ,M11 ,M12  ,M13 ,
     7            M14    ,M21  ,M22 ,M23  ,M24 ,
     8            M31    ,M32  ,M33 ,M34  )
      ENDIF
C--------------------------
      IF (IPARIT == 3) THEN
       CALL CUPDT3F(JFT     ,JLT  ,F      ,M    ,NVC      , 
     2              GBUF%OFF,OFF  ,STI    ,STIR ,STIFN    ,
     3              STIFR   ,IXC  ,PM     ,AREA ,GBUF%THK ,
     4              F11     ,F12  ,F13    ,F14  ,F21      ,
     5              F22     ,F23  ,F24    ,F31  ,F32      ,
     6              F33     ,F34  ,M11    ,M12  ,M13      ,
     7              M14     ,M21  ,M22    ,M23  ,M24      ,
     8              M31     ,M32  ,M33    ,M34  ,GBUF%EINT,
     9              PARTSAV ,MAT  ,IPARTC )
      ELSEIF (IPARIT == 0) THEN
       CALL CUPDT3(JFT     ,JLT   ,F     ,M    ,NVC      , 
     2             GBUF%OFF,OFF   ,STI   ,STIR ,STIFN    ,
     3             STIFR   ,IXC   ,PM    ,AREA ,GBUF%THK ,
     4             F11     ,F12   ,F13   ,F14  ,F21      ,
     5             F22     ,F23   ,F24   ,F31  ,F32      ,
     6             F33     ,F34   ,M11   ,M12  ,M13      ,
     7             M14     ,M21   ,M22   ,M23  ,M24      ,
     8             M31     ,M32   ,M33   ,M34  ,GBUF%EINT,
     9             PARTSAV ,MAT   ,IPARTC,JTHE ,THEM     ,
     A             FTHE    ,CONDN ,CONDE )
      ELSE
        CALL CUPDT3P(JFT      ,JLT     ,GBUF%OFF,OFF   ,STI    ,
     2               STIR     ,FSKY    ,FSKY    ,IADC  ,IXC    ,
     3               F11      ,F12     ,F13     ,F14   ,F21    ,
     4               F22      ,F23     ,F24     ,F31   ,F32    ,
     5               F33      ,F34     ,M11     ,M12   ,M13    ,
     6               M14      ,M21     ,M22     ,M23   ,M24    ,
     7               M31      ,M32     ,M33     ,M34   ,
     8               GBUF%EINT,PARTSAV ,MAT     ,IPARTC,PM     ,
     9               AREA     ,GBUF%THK,JTHE    ,THEM  ,FTHESKY,
     A               CONDNSKY ,CONDE   )
      ENDIF
c
      IF (ICSEN > 0)
     .  CALL CSENS3(JFT    ,JLT    ,PID    ,IGEO   ,GBUF%EPSD)
C-------------------------
c     Fissure des coques 
C--------------------------
      IF (IXFEM > 0) THEN
        DO ILAY=1,NXLAY                                                         
c
          ! crack length calculation for advancing crack                                            
          CRKLEN => ELBUF_STR%BUFLY(ILAY)%DMG(1:NEL)
          CALL  CRKLEN4N_ADV(
     .           NEL       ,NFT       ,ILAY      ,NLAY      ,IXC       ,
     .           CRKLEN    ,ELCRKINI  ,IEL_CRK   ,DIR1_CRK  ,DIR2_CRK  ,     
     .           NODEDGE   ,CRKEDGE   ,XEDGE4N   ,NGL       ,X2        ,
     .           X3        ,X4        ,Y2        ,Y3        ,Y4        ,
     .           ALDT      )
c
          CALL CRKLAYER4N_ADV(
     .         XFEM_STR ,NEL      ,NFT      ,IXC      ,ELCUTC   ,    
     .         ILAY     ,NXLAY    ,IEL_CRK  ,INOD_CRK ,                
     .         IADC_CRK ,NODENR   ,ELCRKINI ,DIR1_CRK ,DIR2_CRK ,                 
     .         NODEDGE  ,CRKNODIAD,KNOD2ELC ,CRKEDGE  ,A_I      ,
     .         X2       ,X3       ,X4       ,Y2       ,Y3       ,
     .         Y4       ,XEDGE4N  ,NGL      )                                                         
c
          CALL CRKLAYER4N_INI(
     .         XFEM_STR ,NEL      ,NFT      ,IXC      ,ELCUTC   ,
     .         ILAY     ,NXLAY    ,IEL_CRK  ,INOD_CRK ,
     .         IADC_CRK ,NODENR   ,ELCRKINI ,DIR1_CRK ,DIR2_CRK ,
     .         NODEDGE  ,CRKNODIAD,KNOD2ELC ,CRKEDGE  ,A_I      ,
     .         X2       ,X3       ,X4       ,Y2       ,Y3       ,
     .         Y4       ,XEDGE4N  ,NGL      )                                                         
        ENDDO                                                                   
C
        CALL CRKOFFC(ELBUF_STR,XFEM_STR  ,
     .               JFT      ,JLT       ,NFT    ,IR      ,IS        ,
     .               NXLAY    ,IEL_CRK   ,CRKEDGE,XEDGE4N )
      END IF                                                             
c--------------------------------------------
c     Front wave
c--------------------------------------------
      IF (IFAILWAVE > 0) THEN
        CRKDIR => ELBUF_STR%BUFLY(1)%CRKDIR
c        
        CALL SET_FAILWAVE_NOD4(FAILWAVE   ,FWAVE_EL ,NGL      ,
     .       NEL      ,IXC      ,ITAB     ,CRKDIR   ,DIR_A    ,
     .       L_DIRA   ,X2       ,X3       ,X4       ,Y2       ,
     .       Y3       ,Y4       )
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
      END SUBROUTINE CFORC3
c
