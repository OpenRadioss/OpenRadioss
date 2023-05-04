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
Chd|  C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|-- called by -----------
Chd|        FORINTC                       source/elements/forintc.F     
Chd|-- calls ---------------
Chd|        C3BILAN                       source/elements/sh3n/coque3n/c3bilan.F
Chd|        C3BRZ3                        source/elements/sh3n/coque3n/c3defo3.F
Chd|        C3COEF3                       source/elements/sh3n/coque3n/c3coef3.F
Chd|        C3COEFRZ3                     source/elements/sh3n/coquedk/cncoef3.F
Chd|        C3COOR3_CRK                   source/elements/xfem/c3coor3_crk.F
Chd|        C3CURV3                       source/elements/sh3n/coque3n/c3curv3.F
Chd|        C3DEFO3                       source/elements/sh3n/coque3n/c3defo3.F
Chd|        C3DEFRZ                       source/elements/sh3n/coque3n/c3defo3.F
Chd|        C3DERI3                       source/elements/sh3n/coque3n/c3deri3.F
Chd|        C3DT3                         source/elements/sh3n/coque3n/c3dt3.F
Chd|        C3EVEC3                       source/elements/sh3n/coque3n/c3evec3.F
Chd|        C3FCUM3                       source/elements/sh3n/coque3n/c3fcum3.F
Chd|        C3FINT3                       source/elements/sh3n/coque3n/c3fint3.F
Chd|        C3FINTRZ                      source/elements/sh3n/coque3n/c3fint3.F
Chd|        C3MCUM3                       source/elements/sh3n/coque3n/c3mcum3.F
Chd|        C3MZCUM3                      source/elements/sh3n/coque3n/c3mcum3.F
Chd|        C3PXPY3                       source/elements/sh3n/coque3n/c3pxpy3.F
Chd|        C3STRA3                       source/elements/sh3n/coque3n/c3stra3.F
Chd|        C3UPDT3_CRK                   source/elements/xfem/xfemfsky.F
Chd|        CMAIN3                        source/materials/mat_share/cmain3.F
Chd|        CSENS3                        source/elements/shell/coque/csens3.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        TEMP3CG                       source/elements/sh3n/coque3n/temp3cg.F
Chd|        THERM3C                       source/elements/sh3n/coque3n/therm3c.F
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
      SUBROUTINE C3FORC3_CRK(
     1   XFEM_STR,    JFT,         JLT,         PM,
     2   IXTG,        X,           F,           M,
     3   V,           R,           FAILWAVE,    NVC,
     4   MTN,         GEO,         TF,          NPF,
     5   BUFMAT,      PMSAV,       DT2T,        NELTST,
     6   ITYPTST,     STIFN,       STIFR,       FSKY,
     7   CRKSKY,      IADTG,       EPSDOT,      OFFSET,
     8   IPARTTG,     THKE,        F11,         F12,
     9   F13,         F21,         F22,         F23,
     A   F31,         F32,         F33,         M11,
     B   M12,         M13,         M21,         M22,
     C   M23,         M31,         M32,         M33,
     D   KFTS,        GROUP_PARAM, MATPARAM_TAB,NEL,
     E   ISTRAIN,     ISH3N,       ITHK,        IOFC,
     F   IPLA,        NFT,         ISMSTR,      FZERO,
     G   IGEO,        IPM,         IFAILURE,    ITASK,
     H   JTHE,        TEMP,        FTHE,        FTHESKY,
     I   IEXPAN,      GRESAV,      GRTH,        IGRTH,
     J   MSTG,        DMELTG,      JSMS,        TABLE,
     K   IPARG,       SENSORS,     PTG,         IXFEM,
     L   INOD_CRK,    IEL_CRK,     IADTG_CRK,   ELCUTC,
     M   IXEL,        STACK,       ISUBSTACK,   UXINT_MEAN,
     N   UYINT_MEAN,  UZINT_MEAN,  NLEVXF,      NODEDGE,
     O   CRKEDGE,     DRAPE_SH3N,  IPRI,        NLOC_DMG,
     P   INDX_DRAPE,  IGRE)
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
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IGRE
      INTEGER JFT,JLT,NVC,MTN,NELTST,ITYPTST,OFFSET,
     .        NEL,ISTRAIN,ISH3N,ICSEN,
     .        ITHK,IOFC,IPLA,NFT,ISMSTR,KFTS,IFAILURE,
     .        JSMS,IXEL,ISUBSTACK,NLEVXF,IPRI
      INTEGER NPF(*),IXTG(NIXTG,*),IADTG(3,*),IGEO(NPROPGI,*),IPM(*),
     .   IPARTTG(*),ITASK,JTHE,IEXPAN,GRTH(*),IGRTH(*),IPARG(*),
     .   IXFEM,INOD_CRK(*),IEL_CRK(*),IADTG_CRK(3,*),
     .   ELCUTC(2,*),NODEDGE(2,*),INDX_DRAPE(STDRAPE)
      my_real 
     .   PM(*),F(*),M(*),V(*),R(*),
     .   GEO(NPROPG,*),TF(*),BUFMAT(*),PMSAV(*),STIFN(*),
     .   STIFR(*),FSKY(*),EPSDOT(6,*),THKE(*),DT2T,
     .   F11(MVSIZ),F12(MVSIZ),F13(MVSIZ),
     .   F21(MVSIZ),F22(MVSIZ),F23(MVSIZ),
     .   F31(MVSIZ),F32(MVSIZ),F33(MVSIZ),
     .   M11(MVSIZ),M12(MVSIZ),M13(MVSIZ),
     .   M21(MVSIZ),M22(MVSIZ),M23(MVSIZ),
     .   M31(MVSIZ),M32(MVSIZ),M33(MVSIZ),
     .   FZERO(3,3,*),TEMP(*),FTHE(*),FTHESKY(*),GRESAV(*),
     .   MSTG(*), DMELTG(*),PTG(3,*),
     .   UXINT_MEAN(NLEVXF,MVSIZ),UYINT_MEAN(NLEVXF,MVSIZ),
     .   UZINT_MEAN(NLEVXF,MVSIZ)
!       SP issue :
      REAL(kind=8), DIMENSION(*), INTENT(in) :: X
      TYPE(TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET :: XFEM_STR
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
      TYPE (XFEM_SKY_)    , DIMENSION(*) :: CRKSKY
      TYPE (STACK_PLY) :: STACK
      TYPE (FAILWAVE_STR_) :: FAILWAVE 
      TYPE (GRP_PARAM_STRUCT_) :: GROUP_PARAM 
      TYPE(DRAPE_) :: DRAPE_SH3N(NUMELTG_DRAPE)
      TYPE (MATPARAM_STRUCT_) , DIMENSION(NUMMAT) :: MATPARAM_TAB
      TYPE (NLOCAL_STR_) :: NLOC_DMG 
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
c     indx utilise localement contrairement aux coques 4n 
      INTEGER 
     .   I,J,J1,J2,IR,IS,IT,IPT,IFLAG,IGTYP,IXFEM2,IREP,IMAT,
     .   IUN,NPG,IBID,IDRIL,NG,IXLAY,NXLAY,NLAYER,NPTT,STEP,
     .   L_DIRA,L_DIRB,ILEV,IGMAT,IPTHK,ISH3NFR,IDRAPE,ACTIFXFEM,
     .   SEDRAPE, NUMEL_DRAPE
      INTEGER  MAT(MVSIZ),PID(MVSIZ),NGL(MVSIZ),INDX(MVSIZ),FWAVE(MVSIZ)
      my_real STI(MVSIZ),STIR(MVSIZ),RHO(MVSIZ),
     .   SSP(MVSIZ),VISCMX(MVSIZ),AREA(MVSIZ),
     .   X21(MVSIZ), Y21(MVSIZ), Z21(MVSIZ),
     .   X31(MVSIZ), Y31(MVSIZ), Z31(MVSIZ),
     .   EXX(MVSIZ),EYY(MVSIZ),EXY(MVSIZ),EYZ(MVSIZ),EZX(MVSIZ),
     .   KXX(MVSIZ),KYY(MVSIZ),KXY(MVSIZ),
     .   PX1(MVSIZ),PY1(MVSIZ),PY2(MVSIZ),
     .   X2(MVSIZ), X3(MVSIZ), Y2(MVSIZ), Y3(MVSIZ),
     .   X21G(MVSIZ), Y21G(MVSIZ), Z21G(MVSIZ),
     .   X31G(MVSIZ), Y31G(MVSIZ), Z31G(MVSIZ),
     .   OFF(MVSIZ),SIGY(MVSIZ),THK0(MVSIZ),
     .   NU(MVSIZ),SHF(MVSIZ),DT1C(MVSIZ),
     .   G(MVSIZ),YM(MVSIZ),A11(MVSIZ),A12(MVSIZ),
     .   VOL0(MVSIZ),THK02(MVSIZ),ZCFAC(MVSIZ,2),GS(MVSIZ),
     .   VOL00(MVSIZ),ALPE(MVSIZ),DIE(MVSIZ),TEMPEL(MVSIZ),
     .   E1X0(MVSIZ), E1Y0(MVSIZ), E1Z0(MVSIZ), E2X0(MVSIZ),
     .   E2Y0(MVSIZ), E2Z0(MVSIZ), E3X0(MVSIZ), E3Y0(MVSIZ), E3Z0(MVSIZ),
     .   E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),E2X(MVSIZ),
     .   E2Y(MVSIZ),E2Z(MVSIZ),E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ),
     .   VL1(MVSIZ,3),VL2(MVSIZ,3),VL3(MVSIZ,3),
     .   VRL1(MVSIZ,3),VRL2(MVSIZ,3),VRL3(MVSIZ,3),THEM(MVSIZ,3),
     .   UX1(MVSIZ),UX2(MVSIZ),UX3(MVSIZ),
     .   UY1(MVSIZ),UY2(MVSIZ),UY3(MVSIZ),
     .   VX13(MVSIZ), VX23(MVSIZ),VY12(MVSIZ),
     .   RLZ(MVSIZ,3),WXY(MVSIZ),MLZ(MVSIZ,3),KRZ(MVSIZ),
     .   B0RZ(MVSIZ,3),BKRZ(MVSIZ,2),BERZ(MVSIZ,2),BM0RZ(MVSIZ,3,2),
     .   ECOS(MVSIZ),ESIN(MVSIZ),A11R(MVSIZ),THKE0(MVSIZ),ALDT(MVSIZ)
      my_real 
     .   BID,THKR,F_DEF(MVSIZ,8),WKXY(MVSIZ)
!       SP issue :
        REAL(kind=8), DIMENSION(MVSIZ) ::X1G,X2G,X3G
        REAL(kind=8), DIMENSION(MVSIZ) ::Y1G,Y2G,Y3G
        REAL(kind=8), DIMENSION(MVSIZ) ::Z1G,Z2G,Z3G
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
C
      my_real,
     .  DIMENSION(:) ,POINTER  ::  OFFG,THKG,STRAG,FORG,MOMG,
     .  EINTG,EPSDG,TEMPG,EINTTH,HOURGG
      DOUBLE PRECISION,
     .  DIMENSION(:) ,POINTER  ::  SMSTRG
      my_real,
     .  DIMENSION(:,:), ALLOCATABLE :: VARNL 
C---------------------------
c     Attention : Coque3N + ISMSTR=10 pas compatible avec XFEM !!!
C=======================================================================
      BID    = ZERO
      IUN  = 1  
      IBID = 0  
      NPG  = 0
      IR   = 1
      IS   = 1
      IT   = 1
      NG   = 1
      SEDRAPE = STDRAPE
      NUMEL_DRAPE = NUMELTG_DRAPE
      ALLOCATE(VARNL(NEL,1))
      VARNL = ZERO
C -----will put it in starter     
      IF (ISMSTR>=10) ISMSTR=4
c
      DO I=JFT,JLT
        MAT(I) = IXTG(1,I)
        PID(I) = IXTG(5,I)
        NGL(I) = IXTG(6,I)
        THK0(I) = THKE(I)
      ENDDO
      IMAT   = IXTG(1,JFT)
      ICSEN  = IGEO(3,PID(1))
      IGTYP  = IGEO(11,PID(1))
      IREP   = IPARG(35)
      IDRIL  = IPARG(41)
      ACTIFXFEM = IPARG(70)
C-----------------------------------------
      GBUF  => XFEM_STR%GBUF
      NXLAY  = XFEM_STR%NLAY
      NLAYER = 1
C-----------------------------------------
      IF (IXFEM == 2) THEN
        IXFEM2 = 1      !  monolayer xfem
      ELSE
        IXFEM2 = IXFEM  !  multinayer xfem
      ENDIF
c
      ALLOCATE(ELCRKINI(NXLAYMAX*NEL))
      ALLOCATE(DIR1_CRK(NXLAYMAX*NEL))
      ALLOCATE(DIR2_CRK(NXLAYMAX*NEL))
      ELCRKINI = 0
      DIR1_CRK = ZERO
      DIR2_CRK = ZERO
C
      L_DIRA = XFEM_STR%BUFLY(1)%LY_DIRA
      L_DIRB = XFEM_STR%BUFLY(1)%LY_DIRB
C
      ALLOCATE(DIRA(NXLAY*NEL*L_DIRA))
      ALLOCATE(DIRB(NXLAY*NEL*L_DIRB))
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
          TEMPEL(I) = ZERO
        ENDDO
C---
        ILEV = NXEL*(IXLAY-1) + IXEL
C---
        IF (IGTYP == 1 .or. IGTYP == 9) THEN
          DO I=JFT,JLT
            THKE0(I) = THKE(I)
          END DO
        ELSEIF (IGTYP == 51 .OR. IGTYP == 52) THEN
          IPTHK = 1 + NXLAY
          THKR  = STACK%GEO(IPTHK+IXLAY,ISUBSTACK)
          DO I=JFT,JLT
            THKE0(I) = THKE(I) * THKR     ! layer thickness (real)
          ENDDO
        ELSE       !  IGTYP == 11...
          IPTHK = 300
          DO I=JFT,JLT
            THKE0(I) = THKE(I) * GEO(IPTHK+IXLAY,IXTG(5,1))
          END DO
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
        CALL C3COOR3_CRK(JFT     ,JLT      ,OFFG     ,OFF      ,SIGY     ,
     .                   DT1C    ,THKE0    ,VL1      ,VL2      ,VL3      ,
     .                   VRL1    ,VRL2     ,VRL3     ,X1G      ,X2G      ,
     .                   X3G     ,Y1G      ,Y2G      ,Y3G      ,Z1G      ,
     .                   Z2G     ,Z3G      ,ILEV     ,IEL_CRK  ,IADTG_CRK,
     .                   NFT     )
C----------use always old local sys
        ISH3NFR = 1
        CALL C3EVEC3(XFEM_STR ,DIR_A   ,DIR_B   ,JFT     ,JLT      ,
     .               IREP     ,E1X0    ,E1Y0    ,E1Z0    ,E2X0     ,
     .               E2Y0     ,E2Z0    ,E3X0    ,E3Y0    ,E3Z0     ,
     .               E1X      ,E1Y     ,E1Z     ,E2X     ,
     .               E2Y      ,E2Z     ,E3X     ,E3Y     ,E3Z      ,
     .               NXLAY    ,GBUF%OFF,ECOS    ,ESIN    ,ISH3NFR  ,
     .               NEL      ,AREA    ,X21G    ,Y21G    ,Z21G     ,
     .               X31G     ,Y31G    ,Z31G    , 
     .               X1G      ,X2G     ,X3G     ,Y1G     ,Y2G      ,
     .               Y3G      ,Z1G     ,Z2G     ,Z3G     ) 
        IF (ISMSTR /= 3) THEN
          CALL C3DERI3(JFT      ,JLT     ,PX1     ,PY1     ,PY2     ,
     .                 SMSTRG   ,OFFG    ,ISMSTR  ,ALPE    ,ALDT    ,  
     .                 UX1      ,UX2     ,UX3     ,UY1     ,UY2     ,
     .                 UY3      ,NEL     ,AREA    ,X21G    ,Y21G     ,
     .                 Z21G     ,X31G    ,Y31G    ,Z31G    ,X2       ,
     .                 Y2       ,X3      ,Y3      , 
     .                 E1X      ,E1Y     ,E1Z     ,E2X      ,               
     .                 E2Y      ,E2Z     ,E3X     ,E3Y      ,E3Z     )
        ELSE
C         bug : ismstr=3 non supporte pour coques 3N
          CALL C3PXPY3(JFT       ,JLT     ,PM      ,STI     ,STIR,
     2                 SMSTRG    ,PX1     ,PY1     ,PY2     ,MAT ,
     3                 SSP       ,NEL     )
        ENDIF
C
        IF (IDRIL > 0) CALL C3BRZ3(JFT  ,JLT  ,AREA ,X2   ,X3   ,
     .                             Y3   ,BM0RZ,B0RZ ,BKRZ ,BERZ )
c
        CALL C3COEF3(JFT    ,JLT     ,PM     ,MAT      ,GEO     ,
     2               PID    ,OFF     ,AREA   ,STI      ,STIR    ,
     3               SHF    ,THK0    ,THK02  ,NU      ,
     4               G      ,YM      ,A11    ,A12      ,THKG    ,
     5               SSP    ,RHO     ,VOL0   ,GS       ,MTN     ,
     6               ITHK   ,NPTT    ,ISMSTR ,VOL00    ,IGEO    ,
     7               A11R   , ISUBSTACK, STACK%PM)
c
        CALL C3DEFO3(JFT  ,JLT  ,VL1  ,VL2  ,VL3  ,
     .               IXTG ,ISH3N,PX1  ,PY1  ,PY2  ,
     .               EXX  ,EYY  ,EXY  ,EYZ  ,EZX  ,
     .               VX13 ,VX23 ,VY12 ,
     .               E1X  ,E1Y  ,E1Z  ,E2X  ,
     .               E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  )

        IF (IDRIL > 0) THEN
          CALL C3DEFRZ(JFT    ,JLT   ,RLZ    ,BM0RZ ,B0RZ,
     1                 BKRZ   ,BERZ  ,E3X0   ,E3Y0  ,E3Z0  ,
     2                 VRL1   ,VRL2  ,VRL3   ,EXX   ,EYY  ,
     3                 EXY    ,PX1   ,PY1    ,PY2   ,WXY  ,
     4                 AREA   ,VX13  ,VX23   ,VY12  )
          CALL C3COEFRZ3(JFT    ,JLT     ,G,  KRZ   ,AREA  ,THKE0)
        ENDIF

        CALL C3CURV3(JFT,JLT,VRL1,VRL2,VRL3,
     .               IXTG,WKXY  ,ISMSTR,KXX,KYY,
     .               KXY   ,PX1 ,PY1 ,PY2 ,EYZ  ,EZX  ,
     .               E1X   ,E1Y   ,E1Z   ,E2X   ,E2Y   ,
     .               E2Z   ,E3X   ,E3Y   ,E3Z   )

        CALL C3STRA3(JFT    ,JLT     ,PM     ,
     2               MAT    ,AREA    ,EXX    ,EYY     ,EXY   ,
     3               EZX    ,EYZ     ,KXX    ,KYY     ,KXY   ,
     4               GEO    ,PID     ,NU     ,SHF     ,STRAG ,
     5               SSP    ,RHO     ,EPSDOT ,
     6               NFT    ,ISTRAIN ,ISMSTR ,
     7               UX1    ,UX2     ,UX3    ,UY1     ,UY2   ,
     8               UY3    ,PX1     ,PY1    ,PY2     ,MTN   ,
     9               F_DEF  ,WKXY    ,GBUF%STRW,NEL     )
C
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
        IF (JTHE > 0 ) CALL TEMP3CG(JFT  ,JLT   ,PM     ,MAT   ,IXTG,   
     .                              TEMP ,TEMPEL)
C--------------------------
        IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STARTIME(35,1)
C--------------------------
        CALL CMAIN3(
     1   XFEM_STR  ,JFT       ,JLT       ,NFT       ,IPARG     ,
     2   NEL       ,MTN       ,IPLA      ,ITHK      ,GROUP_PARAM,
     3   PM        ,GEO       ,NPF       ,TF        ,BUFMAT    ,
     4   SSP       ,RHO       ,VISCMX    ,DT1C      ,SIGY      ,
     5   AREA      ,EXX       ,EYY       ,EXY       ,EZX       ,
     6   EYZ       ,KXX       ,KYY       ,KXY       ,NU        ,
     7   OFF       ,THK0      ,MAT       ,PID       ,MATPARAM_TAB,
     8   FORG      ,MOMG      ,STRAG     ,FAILWAVE  ,FWAVE     ,
     9   THKG      ,EINTG     ,IOFC      ,
     A   G         ,A11       ,A12       ,VOL0      ,INDX      ,
     B   NGL       ,ZCFAC     ,SHF       ,GS        ,EPSDG     ,
     C   KFTS      ,ISH3N     ,ALPE      ,
     D   DIR_A     ,DIR_B     ,IGEO      ,
     E   IPM       ,IFAILURE  ,NPG       ,
     F   TEMPEL    ,DIE       ,JTHE     ,IEXPAN     ,TEMPG     ,
     G   IBID      ,BID       ,
     H   BID       ,BID       ,BID       ,BID       ,BID       ,
     I   BID       ,BID       ,BID       ,E1X       ,E1Y       ,
     J   E1Z       ,E2X       ,E2Y       ,E2Z       ,E3X       ,
     K   E3Y       ,E3Z       ,NG        ,TABLE     ,IXFEM     ,
     L   BID       ,SENSORS   ,BID       ,ELCRKINI  ,
     M   DIR1_CRK  ,DIR2_CRK  ,ALDT      ,
     P   ISMSTR    ,IR        ,IS        ,NLAYER    ,NPTT      ,
     Q   IXLAY     ,IXEL      ,ISUBSTACK ,STACK     ,
     P   BID    ,ITASK        ,DRAPE_SH3N  ,VARNL     ,NLOC_DMG ,
     R   INDX_DRAPE , THKE    ,SEDRAPE ,NUMEL_DRAPE)

C--------------------------
        IF ((ITASK==0).AND.(IMON_MAT==1)) CALL STOPTIME(35,1)
C--------------------------
C       PAS DE TEMPS
C--------------------------
        IF (ISMSTR /= 3) CALL C3DT3(
     1          JFT      ,JLT       ,PM      ,OFF    ,DT2T    ,
     2          NELTST   ,ITYPTST   ,STI     ,STIR   ,OFFG    ,
     3          SSP      ,VISCMX    ,ISMSTR  ,NFT    ,IOFC    ,
     4          ALPE     ,MSTG      ,DMELTG  ,JSMS   ,PTG     ,
     5          SHF      ,IGTYP     ,IGMAT   ,G     ,A11      ,
     6          A11R     ,GBUF%G_DT ,GBUF%DT ,ALDT  ,THK0     ,
     7          AREA     ,NGL       ,IMAT    ,MTN   )

C--------------------------
C     BILANS PAR MATERIAU
C--------------------------
c        IFLAG = MOD(NCYCLE,NCPRI)
        IF (IPRI>0)
     +      CALL C3BILAN(
     1   JFT,      JLT,      PM,       V,
     2   THKG,     EINTG,    PMSAV,    IPARTTG,
     3   RHO,      VOL00,    IXTG,     X,
     4   R,        THK02,    AREA,     GRESAV,
     5   GRTH,     IGRTH,    OFF,      IXFEM2,
     6   ILEV,     IEL_CRK,  IADTG_CRK,NFT,
     7   IEXPAN,   EINTTH,   ITASK,    MAT,
     8   GBUF%VOL, ACTIFXFEM,IGRE)
C----------------------------
C     FORCES INTERNES
C----------------------------
        CALL C3FINT3(JFT    ,JLT     ,FORG    ,MOMG  ,THK0,
     2               PX1    ,PY1     ,PY2     ,F11   ,F12 ,
     3               F13    ,F21     ,F22     ,F23   ,F31 ,
     4               F32    ,F33     ,M11     ,M12   ,M13 ,
     5               M21    ,M22     ,M23     ,NEL   )

        IF (IDRIL > 0) THEN
          CALL C3FINTRZ(JFT       ,JLT  ,THK0 ,AREA ,PX1      ,
     2                  PY1       ,PY2  ,F11  ,F12  ,F13      ,
     3                  F21       ,F22  ,F23  ,WXY  ,FORG     ,
     4                  HOURGG    ,MLZ  ,BM0RZ,B0RZ ,BKRZ     ,
     5                  BERZ      ,KRZ  ,RLZ  ,DT1C ,EINTG    ,
     6                  OFF       ,VOL0 ,NEL  )
        ENDIF
C-------------------------
c     Thermique des coques 
C--------------------------
        IF (JTHE > 0) THEN
          CALL THERM3C(JFT   ,JLT   ,PM   ,MAT  ,THK0 ,IXTG,
     .                 PX1   ,PY1   ,PY2  ,AREA ,DT1C ,
     .                 TEMP  ,TEMPEL,DIE  ,THEM )
        ENDIF
C--------------------------
C     THERMAL TIME STEP       --- to be added --- (see c3forc3.F)
C--------------------------
C-------------------------
C     ASSEMBLE
C-------------------------
        CALL C3FCUM3(JFT,JLT,F,
     .               F11,F12,F13,F21,F22,F23,
     .               F31,F32,F33,FZERO,
     .               E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,
     .               E2Z  ,E3X  ,E3Y  ,E3Z  )

        CALL C3MCUM3(JFT,JLT,M,M11,M12,
     .               M13,M21,M22,M23,M31,M32,M33,
     .               E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,
     .               E2Z  ,E3X  ,E3Y  ,E3Z  )
        IF (IDRIL > 0) THEN
          CALL C3MZCUM3(JFT    ,JLT    ,MLZ    ,E3X    ,E3Y,
     .                  E3Z    ,M11    ,M12    ,M13    ,M21,
     .                  M22    ,M23    ,M31    ,M32    ,M33)
        ENDIF

C 
        IF (IPARIT == 1)
     .    CALL C3UPDT3_CRK(
     .           JFT  ,JLT     ,NFT  ,IXTG  ,OFF     ,IADTG  ,
     .           F11  ,F21     ,F31  ,F12   ,F22     ,F32    ,
     .           F13  ,F23     ,F33  ,
     .           M11  ,M21     ,M31  ,M12   ,M22     ,M32    ,
     .           M13  ,M23     ,M33  ,
     .           STI  ,STIR    ,FSKY ,ELCUTC,IADTG_CRK,IEL_CRK,
     .           ILEV ,IXLAY   ,OFFG ,CRKSKY)
C
        IF (ICSEN > 0) CALL CSENS3(JFT ,JLT ,PID ,IGEO ,EPSDG)
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
