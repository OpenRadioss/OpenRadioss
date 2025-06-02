Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    c3forc3             ../engine/source/elements/sh3n/coque3n/c3forc3.F
      !||--- called by ------------------------------------------------------
      !||    forintc             ../engine/source/elements/forintc.F
      !||--- calls      -----------------------------------------------------
      !||    c3bilan             ../engine/source/elements/sh3n/coque3n/c3bilan.F
      !||    c3brz3              ../engine/source/elements/sh3n/coque3n/c3defo3.F
      !||    c3coef3             ../engine/source/elements/sh3n/coque3n/c3coef3.F
      !||    c3coefrz3           ../engine/source/elements/sh3n/coquedk/cncoef3.F
      !||    c3coor3             ../engine/source/elements/sh3n/coque3n/c3coor3.F
      !||    c3coort3            ../engine/source/elements/sh3n/coque3n/c3coor3.F
      !||    c3curv3             ../engine/source/elements/sh3n/coque3n/c3curv3.F
      !||    c3defo3             ../engine/source/elements/sh3n/coque3n/c3defo3.F
      !||    c3defrz             ../engine/source/elements/sh3n/coque3n/c3defo3.F
      !||    c3deft3             ../engine/source/elements/sh3n/coque3n/c3defo3.F
      !||    c3deri3             ../engine/source/elements/sh3n/coque3n/c3deri3.F
      !||    c3dt3               ../engine/source/elements/sh3n/coque3n/c3dt3.F
      !||    c3evec3             ../engine/source/elements/sh3n/coque3n/c3evec3.F
      !||    c3fcum3             ../engine/source/elements/sh3n/coque3n/c3fcum3.F
      !||    c3fint3             ../engine/source/elements/sh3n/coque3n/c3fint3.F
      !||    c3fint_reg          ../engine/source/elements/sh3n/coque3n/c3fint_reg.F
      !||    c3fintrz            ../engine/source/elements/sh3n/coque3n/c3fint3.F
      !||    c3mcum3             ../engine/source/elements/sh3n/coque3n/c3mcum3.F
      !||    c3mzcum3            ../engine/source/elements/sh3n/coque3n/c3mcum3.F
      !||    c3pxpy3             ../engine/source/elements/sh3n/coque3n/c3pxpy3.F
      !||    c3sroto3            ../engine/source/elements/sh3n/coque3n/c3evec3.F
      !||    c3stra3             ../engine/source/elements/sh3n/coque3n/c3stra3.F
      !||    c3updt3             ../engine/source/elements/sh3n/coque3n/c3updt3.F
      !||    c3updt3p            ../engine/source/elements/sh3n/coque3n/c3updt3.F
      !||    cmain3              ../engine/source/materials/mat_share/cmain3.F
      !||    crklayer3n_adv      ../engine/source/elements/xfem/crklayer3n_adv.F
      !||    crklayer3n_ini      ../engine/source/elements/xfem/crklayer3n_ini.F
      !||    crklen3n_adv        ../engine/source/elements/xfem/crklen3n_adv.F
      !||    crkofftg            ../engine/source/elements/xfem/precrklay.F
      !||    csens3              ../engine/source/elements/shell/coque/csens3.F
      !||    dttherm             ../engine/source/time_step/dttherm.F90
      !||    precrklaytg         ../engine/source/elements/xfem/precrklay.F
      !||    set_failwave_nod3   ../engine/source/materials/fail/failwave/set_failwave_nod3.F
      !||    set_failwave_sh3n   ../engine/source/materials/fail/failwave/upd_failwave_sh3n.F
      !||    shroto3             ../engine/source/elements/sh3n/coque3n/c3evec3.F
      !||    shtroto3            ../engine/source/elements/sh3n/coque3n/c3evec3.F
      !||    startime            ../engine/source/system/timer_mod.F90
      !||    stoptime            ../engine/source/system/timer_mod.F90
      !||    temp3cg             ../engine/source/elements/sh3n/coque3n/temp3cg.F
      !||    therm3c             ../engine/source/elements/sh3n/coque3n/therm3c.F
      !||--- uses       -----------------------------------------------------
      !||    crackxfem_mod       ../engine/share/modules/crackxfem_mod.F
      !||    drape_mod           ../engine/share/modules/drape_mod.F
      !||    dt_mod              ../engine/source/modules/dt_mod.F
      !||    dttherm_mod         ../engine/source/time_step/dttherm.F90
      !||    elbufdef_mod        ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    failwave_mod        ../common_source/modules/failwave_mod.F
      !||    glob_therm_mod      ../common_source/modules/mat_elem/glob_therm_mod.F90
      !||    mat_elem_mod        ../common_source/modules/mat_elem/mat_elem_mod.F90
      !||    nlocal_reg_mod      ../common_source/modules/nlocal_reg_mod.F
      !||    sensor_mod          ../common_source/modules/sensor_mod.F90
      !||    stack_mod           ../engine/share/modules/stack_mod.F
      !||    table_mod           ../engine/share/modules/table_mod.F
      !||    timer_mod           ../engine/source/system/timer_mod.F90
      !||====================================================================
      SUBROUTINE C3FORC3(TIMERS,
     1   ELBUF_STR,   JFT,         JLT,         PM,
     2   IXTG,        X,           F,           M,
     3   V,           R,           FAILWAVE,    NVC,
     4   MTN,         GEO,         TF,          NPF,
     5   BUFMAT,      PMSAV,       DT2T,        NELTST,
     6   ITYPTST,     STIFN,       STIFR,       FSKY,
     7   IADTG,       ITAB,        EPSDOT,      OFFSET,
     8   IPARTTG,     THKE,        F11,         F12,
     9   F13,         F21,         F22,         F23,
     A   F31,         F32,         F33,         M11,
     B   M12,         M13,         M21,         M22,
     C   M23,         M31,         M32,         M33,
     D   GROUP_PARAM, MAT_ELEM,    NEL,         ISTRAIN,
     E   ISH3N,       XEDGE3N,     ITHK,        IOFC,
     F   IPLA,        NFT,         ISMSTR,      NPT,
     G   KFTS,        FZERO,       IGEO,        IPM,
     H   IFAILURE,    ITASK,       JTHE,        TEMP,
     I   FTHE,        FTHESKY,     IEXPAN,      GRESAV,
     J   GRTH,        IGRTH,       MSTG,        DMELTG,
     K   JSMS,        TABLE,       IPARG,       IXFEM,
     L   SENSORS,     PTG,         IBORDNODE,   ELCUTC,
     M   INOD_CRK,    IEL_CRK,     NODENR,      IADTG_CRK,
     N   NODEDGE,     CRKNODIAD,   KNOD2ELC,    CONDN,
     O   CONDNSKY,    STACK,       ISUBSTACK,   XFEM_STR,
     P   CRKEDGE,     DRAPE_SH3N,  IPRI,        NLOC_DMG,
     Q   xdp,         INDX_DRAPE,  IGRE,        JTUR,
     R   DT,          SNPC,        STF,         GLOB_THERM,
     S   IDEL7NOK,    USERL_AVAIL, MAXFUNC,     SBUFMAT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TIMER_MOD
      USE TABLE_MOD
      USE MAT_ELEM_MOD
      USE CRACKXFEM_MOD
      USE STACK_MOD
      USE FAILWAVE_MOD
      USE NLOCAL_REG_MOD          
      USE DRAPE_MOD      
      USE SENSOR_MOD
      USE ELBUFDEF_MOD
      USE DT_MOD
      use glob_therm_mod
      use dttherm_mod
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
#include      "scr18_c.inc"
#include      "com_xfem1.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TIMER_), INTENT(INOUT) :: TIMERS ! Timer structure
      INTEGER, INTENT(IN)    :: USERL_AVAIL ! Flag for User libraries availability
      INTEGER, INTENT(IN)    :: MAXFUNC     ! Maximum number of functions
      INTEGER, INTENT(INOUT) :: IDEL7NOK    ! Element deletion flag for IDEL flag in contact interfaces
      INTEGER, INTENT(IN)    :: SBUFMAT     ! size of Bufmat
      INTEGER, INTENT(IN)    :: STF         ! size of TF
      INTEGER, INTENT(IN)    :: SNPC        ! size of NPC
      INTEGER, INTENT(IN) :: JTUR
      INTEGER, INTENT(IN) :: IGRE
      INTEGER JFT, JLT, NVC, MTN,NELTST,ITYPTST, OFFSET,
     .        NEL    ,ISTRAIN,ISH3N , ICSEN,
     .        ITHK  ,IOFC   ,IPLA  ,NFT   ,ISMSTR ,NPT,KFTS,IFAILURE,
     .        JSMS,ISUBSTACK
      INTEGER NPF(*),IXTG(NIXTG,*),IADTG(3,*),IGEO(NPROPGI,*),IPM(*),
     .   IPARTTG(*),ITASK,JTHE,IEXPAN,GRTH(*),IGRTH(*),IPARG(*),ITAB(*),
     .   IXFEM,IBORDNODE(*),
     .   ELCUTC(2,*),INOD_CRK(*),NODENR(*),IEL_CRK(*),IADTG_CRK(3,*),
     .   NODEDGE(2,*),CRKNODIAD(*),KNOD2ELC(*),XEDGE3N(3,*),INDX_DRAPE(STDRAPE)
      my_real 
     .   PM(NPROPM,*), F(*), M(*), V(*), R(*),X(*),
     .   GEO(NPROPG,*), TF(*), BUFMAT(*), PMSAV(*),STIFN(*),
     .   STIFR(*),FSKY(*),EPSDOT(6,*),THKE(*),DT2T,
     .   F11(MVSIZ), F12(MVSIZ), F13(MVSIZ),
     .   F21(MVSIZ), F22(MVSIZ), F23(MVSIZ),
     .   F31(MVSIZ), F32(MVSIZ), F33(MVSIZ),
     .   M11(MVSIZ), M12(MVSIZ), M13(MVSIZ),
     .   M21(MVSIZ), M22(MVSIZ), M23(MVSIZ),
     .   M31(MVSIZ), M32(MVSIZ), M33(MVSIZ),
     .   FZERO(3,3,*),TEMP(*),FTHE(*),FTHESKY(*),GRESAV(*),MSTG(*),
     .   DMELTG(*),PTG(3,*),CONDN(*),CONDNSKY(*)
!       SP issue :
      REAL(kind=8), DIMENSION(*), INTENT(in), TARGET :: XDP

      TYPE(TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (ELBUF_STRUCT_), DIMENSION(NXEL), TARGET :: XFEM_STR  ! take xfem_str
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
      TYPE (STACK_PLY) :: STACK
      TYPE (NLOCAL_STR_)   ,TARGET :: NLOC_DMG 
      TYPE (FAILWAVE_STR_) ,TARGET :: FAILWAVE 
      TYPE (DRAPE_), DIMENSION(NUMELTG_DRAPE)   :: DRAPE_SH3N
      TYPE (SENSORS_) ,INTENT(INOUT) :: SENSORS
      TYPE (MAT_ELEM_),INTENT(INOUT) :: MAT_ELEM
      TYPE (GROUP_PARAM_) :: GROUP_PARAM
      TYPE (DT_), INTENT(IN) :: DT
      type (glob_therm_) ,intent(inout) :: glob_therm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER INDX(MVSIZ)  ! local definition contrary to 4-node-shell elems
      INTEGER MAT(MVSIZ),PID(MVSIZ),NGL(MVSIZ),FWAVE_EL(NEL)
      INTEGER I,J,IFLAG,NPG,IBID,IDRIL,NG,IR,IS,N1,N2,N3,
     .   IXEL,IXLAY,ILAY,NXLAY,NLAY,L_DIRA,L_DIRB,J1,J2,IGMAT,IGTYP,
     .   IMAT,NPTTOT,IREP,ipr,IFRAM_OLD,IPRI,IFAILWAVE, IDRAPE,NPTT,IT,
     .   ACTIFXFEM ,SEDRAPE,NUMEL_DRAPE
      my_real :: DTIME
      INTEGER SDIR_A    ! Size of DIR_A
      INTEGER SDIR_B    ! Size of DIR_B
      my_real :: dt1,dtinv,asrate,eps_m2,eps_k2
      my_real 
     .   STI(MVSIZ),STIR(MVSIZ),RHO(MVSIZ),BID,
     .   VISCMX(MVSIZ),AREA(MVSIZ),
     .   X2L(MVSIZ), X3L(MVSIZ), Y2L(MVSIZ),Y3L(MVSIZ),
     .   EXX(MVSIZ), EYY(MVSIZ), EXY(MVSIZ), EZX(MVSIZ), EYZ(MVSIZ),
     .   KXX(MVSIZ), KYY(MVSIZ), KXY(MVSIZ),
     .   PX1(MVSIZ), PY1(MVSIZ), PY2(MVSIZ),
     .   OFF(MVSIZ), SIGY(MVSIZ),THK0(MVSIZ),
     .   NU(MVSIZ) , SHF(MVSIZ), DT1C(MVSIZ),
     .   G(MVSIZ)   , YM(MVSIZ)  , A11(MVSIZ)   , A12(MVSIZ),
     .   VOL0(MVSIZ),THK02(MVSIZ),ZCFAC(MVSIZ,2), GS(MVSIZ),
     .   VOL00(MVSIZ),ALPE(MVSIZ),DIE(MVSIZ), TEMPEL(MVSIZ),
     .   E1X0(MVSIZ), E1Y0(MVSIZ), E1Z0(MVSIZ), E2X0(MVSIZ),
     .   E2Y0(MVSIZ), E2Z0(MVSIZ), E3X0(MVSIZ), E3Y0(MVSIZ), E3Z0(MVSIZ),
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ), E2X(MVSIZ),
     .   E2Y(MVSIZ), E2Z(MVSIZ), E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),
     .   VL1(MVSIZ,3),VL2(MVSIZ,3),VL3(MVSIZ,3),
     .   VRL1(MVSIZ,3),VRL2(MVSIZ,3),VRL3(MVSIZ,3) ,THEM(MVSIZ,3),
     .   X21G(MVSIZ), Y21G(MVSIZ), Z21G(MVSIZ),
     .   X31G(MVSIZ), Y31G(MVSIZ), Z31G(MVSIZ),
     .   UX1(MVSIZ),UX2(MVSIZ),UX3(MVSIZ),
     .   UY1(MVSIZ),UY2(MVSIZ),UY3(MVSIZ),
     .   VX13(MVSIZ), VX23(MVSIZ),VY12(MVSIZ),
     .   RLZ(MVSIZ,3),WXY(MVSIZ),MLZ(MVSIZ,3),KRZ(MVSIZ),
     .   B0RZ(MVSIZ,3),BKRZ(MVSIZ,2),BERZ(MVSIZ,2),BM0RZ(MVSIZ,3,2),
     .   CONDE(MVSIZ),A11R(MVSIZ),FAC1,ALDT(MVSIZ),SSP(MVSIZ)
      my_real 
     .   AREAT(MVSIZ),X2T(MVSIZ) ,Y2T(MVSIZ), X3T(MVSIZ),Y3T(MVSIZ),
     .   F_DEF(MVSIZ,8), U21X(MVSIZ),U31X(MVSIZ),U21Y(MVSIZ),U31Y(MVSIZ),
     .   RZ13(MVSIZ),RZ23(MVSIZ),BMRZT(MVSIZ,8),WKXY(MVSIZ),
     .   ECOS(MVSIZ),ESIN(MVSIZ),NFOR(NEL,5),NMOM(NEL,3)
      !
       my_real , DIMENSION(NEL) :: ZOFFSET
!       SP issue :
        REAL(kind=8), DIMENSION(MVSIZ) ::X1G,X2G,X3G
        REAL(kind=8), DIMENSION(MVSIZ) ::Y1G,Y2G,Y3G
        REAL(kind=8), DIMENSION(MVSIZ) ::Z1G,Z2G,Z3G
      my_real, dimension(nel)   :: epsd_pg
!     variables for heat transfer
      my_real, dimension(mvsiz) :: fheat
!
C--- Variables for non-local formulation
      INTEGER :: NDDL, K, INOD(3),NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), L_NLOC, IPOS(3),INLOC
      my_real, DIMENSION(:,:), ALLOCATABLE :: VAR_REG
      my_real, DIMENSION(:), POINTER :: BPRELD,DNL
C-----------------------------------------------
      INTEGER, DIMENSION(NEL) :: OFFLY
      INTEGER, ALLOCATABLE, DIMENSION(:) :: ELCRKINI
      my_real ,ALLOCATABLE, DIMENSION(:) :: DIRA,DIRB,DIR1_CRK,DIR2_CRK
      my_real ,DIMENSION(:) ,POINTER     :: DIR_A,DIR_B,CRKDIR,CRKLEN,DADV
      TARGET :: DIRA,DIRB
C-----
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF
      TYPE(L_BUFEL_DIR_) ,POINTER :: LBUF_DIR
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      GBUF   => ELBUF_STR%GBUF
      IDRAPE = ELBUF_STR%IDRAPE
      IBID   = 0
      BID    = ZERO
      IDRIL  = IPARG(41)
      ACTIFXFEM = IPARG(70)
      INLOC  = IPARG(78)
      NLAY  = ELBUF_STR%NLAY
      SEDRAPE = STDRAPE
      NUMEL_DRAPE = NUMELTG_DRAPE
      TEMPEL(:) = ZERO
      FHEAT(: ) = ZERO
cc      NPT  = MAX(NLAY,NPTT) --> set to = IPARG(6) , keeping it original
C                                 to allow for NPT = 0 (global LAW_3
      NPG  = 1
      IR   = 1
      IS   = 1
      NG   = 1
      IXEL  = 0
      IXLAY = 0
      IREP = IPARG(35)
c
      ZCFAC(1:MVSIZ,1:2) = ZERO      
c
      NPTTOT  = 0
      DO ILAY=1,NLAY
        NPTTOT = NPTTOT + ELBUF_STR%BUFLY(ILAY)%NPTT
      ENDDO
      NDDL = NPTTOT
      ALLOCATE(VAR_REG(NEL,NDDL))
      IF (NPT == 0) NPTTOT = NPT  !  compatibility with global integration
      IF (ISH3N==3.AND.ISH3NFRAM==0) THEN
       IFRAM_OLD =0
      ELSE
       IFRAM_OLD =1
      END IF
c
      DO I=JFT,JLT
        MAT(I) = IXTG(1,I)
        PID(I) = IXTG(5,I)
        NGL(I) = IXTG(6,I)
        THK0(I) = THKE(I)   
      ENDDO
      IMAT  = IXTG(1,JFT)
      ICSEN = IGEO(3,PID(1))
      IGTYP = IGEO(11,PID(1))
      IGMAT = IGEO(98,PID(1))  
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
        CALL SET_FAILWAVE_SH3N(FAILWAVE ,FWAVE_EL ,DADV     ,
     .      NEL      ,IXTG    ,ITAB     ,NGL      ,OFFLY    )
c
      ENDIF
c-------------------------------------
      L_DIRA = ELBUF_STR%BUFLY(1)%LY_DIRA
      L_DIRB = ELBUF_STR%BUFLY(1)%LY_DIRB
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
        SDIR_A = NPTTOT*NEL*L_DIRA
        SDIR_B = NPTTOT*NEL*L_DIRB
        DIR_A => DIRA(1:NPTTOT*NEL*L_DIRA)
        DIR_B => DIRB(1:NPTTOT*NEL*L_DIRB)

      ELSE ! idrape
        SDIR_A = NLAY*NEL*L_DIRA
        SDIR_B = NLAY*NEL*L_DIRB
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
         SDIR_A=NLAY*NEL*L_DIRA
         SDIR_B=NLAY*NEL*L_DIRB
         DIR_A => DIRA(1:NLAY*NEL*L_DIRA)
         DIR_B => DIRB(1:NLAY*NEL*L_DIRB)
      ENDIF ! IDRAPE
c-------------------------------------
      NXLAY = NLAY
c
      IF (IXFEM > 0) THEN
        ALLOCATE(ELCRKINI(NXLAYMAX*MVSIZ))
        ALLOCATE(DIR1_CRK(NXLAYMAX*MVSIZ))
        ALLOCATE(DIR2_CRK(NXLAYMAX*MVSIZ))
        DIR1_CRK = ZERO
        DIR2_CRK = ZERO
        ELCRKINI = 0
        IF (NLEVSET > 0) THEN
          CALL PRECRKLAYTG(JFT     ,JLT    ,NFT     ,NXLAY  ,ELCRKINI,
     .                     IEL_CRK,INOD_CRK,NODENR  ,CRKEDGE,XEDGE3N )
        ENDIF
      ELSE
        ALLOCATE(ELCRKINI(0))
        ALLOCATE(DIR1_CRK(0))
        ALLOCATE(DIR2_CRK(0))
      ENDIF  ! IXFEM
C--------------------
C
      CALL C3COOR3(JFT      ,JLT      ,X        ,IXTG     ,
     .             GBUF%OFF ,OFF      ,DT1C     ,
     .             V        ,R        ,VL1      ,VL2      ,VL3      , 
     .             VRL1     ,VRL2     ,VRL3     ,SIGY     ,
     .             X1G      ,X2G      ,X3G      ,Y1G      ,Y2G      ,
     .             Y3G      ,Z1G      ,Z2G      ,Z3G      ,XDP      )
C
C------Change local system (objective) but only for strain,stress computes
C------to avoid many modifs especially complicated treatments like free shear locking
c
      CALL C3EVEC3(ELBUF_STR,DIR_A    ,DIR_B   ,JFT      ,JLT      , 
     .             IREP     ,E1X0     ,E1Y0    ,E1Z0     ,E2X0     , 
     .             E2Y0     ,E2Z0     ,E3X0    ,E3Y0     ,E3Z0     , 
     .             E1X      ,E1Y      ,E1Z     ,E2X      ,               
     .             E2Y      ,E2Z      ,E3X     ,E3Y      ,E3Z      ,      
     .             NLAY     ,GBUF%OFF ,ECOS    ,ESIN     ,IFRAM_OLD,
     .             NEL      ,AREA     ,X21G    ,Y21G     ,Z21G     ,
     .             X31G     ,Y31G     ,Z31G    ,  
     .             X1G      ,X2G      ,X3G     ,Y1G      ,Y2G      ,
     .             Y3G      ,Z1G      ,Z2G     ,Z3G      ) 
c
      IF (ISMSTR /= 3)THEN
        CALL C3DERI3(JFT       ,JLT     ,PX1     ,PY1     ,PY2      ,
     .               GBUF%SMSTR,GBUF%OFF,ISMSTR  ,ALPE    ,ALDT     ,   
     .               UX1       ,UX2     ,UX3     ,UY1     ,UY2      ,
     .               UY3       ,NEL     ,AREA    ,X21G    ,Y21G     ,
     .               Z21G      ,X31G    ,Y31G    ,Z31G    ,X2L      ,
     .               Y2L       ,X3L     ,Y3L     , 
     .               E1X       ,E1Y     ,E1Z     ,E2X     ,                 
     .               E2Y       ,E2Z     ,E3X     ,E3Y     ,E3Z      )       
      ELSE
C       bug ismstr=3 not supported ofr 3-node-shell elements
        CALL C3PXPY3(JFT       ,JLT   ,PM    ,STI    ,STIR,
     2               GBUF%SMSTR,PX1   ,PY1   ,PY2    ,MAT ,
     3               SSP       ,NEL   )
      ENDIF
C
      IF (IDRIL > 0) CALL C3BRZ3(JFT  ,JLT ,AREA ,X2L  ,X3L   ,
     .                           Y3L  ,BM0RZ,B0RZ,BKRZ,BERZ)
c
      CALL C3COEF3(JFT    ,JLT     ,PM     ,MAT      ,GEO     ,
     2             PID    ,OFF     ,AREA   ,STI      ,STIR    ,
     3             SHF    ,THK0    ,THK02  ,NU       ,
     4             G      ,YM      ,A11    ,A12      ,GBUF%THK,
     5             SSP    ,RHO     ,VOL0   ,GS       ,MTN     ,
     6             ITHK   ,NPTTOT  ,ISMSTR ,VOL00    ,IGEO    ,
     7             A11R   ,ISUBSTACK , STACK%PM,NEL, ZOFFSET )
c
      CALL C3DEFO3(JFT   ,JLT   ,VL1   ,VL2   ,VL3   ,
     .             IXTG  ,ISH3N ,PX1   ,PY1   ,PY2   ,
     .             EXX   ,EYY   ,EXY   ,EYZ   ,EZX   ,
     .             VX13  ,VX23  ,VY12  ,
     .             E1X      ,E1Y      ,E1Z     ,E2X      ,                 
     .             E2Y      ,E2Z      ,E3X     ,E3Y      ,E3Z      )       
c
      IF (IDRIL > 0) THEN
         CALL C3DEFRZ(JFT   ,JLT  ,RLZ    ,BM0RZ ,B0RZ,
     1                BKRZ  ,BERZ ,E3X0    ,E3Y0   ,E3Z0  ,
     2                VRL1  ,VRL2 ,VRL3   ,EXX   ,EYY  ,
     3                EXY   ,PX1  ,PY1    ,PY2   ,WXY  ,
     4                AREA  ,VX13 ,VX23  ,VY12  )
        CALL C3COEFRZ3(JFT    ,JLT     ,G,  KRZ   ,AREA  ,THKE)
      END IF
c
      CALL C3CURV3(JFT,JLT,VRL1,VRL2,VRL3,
     .             IXTG,WKXY,ISMSTR,KXX,KYY,KXY,
     .             PX1 ,PY1 ,PY2 ,EYZ  ,EZX  ,
     .             E1X      ,E1Y      ,E1Z     ,E2X      ,                 
     .             E2Y      ,E2Z      ,E3X     ,E3Y      ,E3Z      )       
            
      IF (ISMSTR == 10) THEN
        CALL C3COORT3(JFT    ,JLT    ,X          ,IXTG   ,GBUF%OFF,
     1                R      ,X2L    ,X3L        ,Y2L    ,Y3L     ,
     2                E1X0    ,E1Y0    ,E1Z0        ,E2X0    ,E2Y0     ,
     3                E2Z0    ,E3X0    ,E3Y0        ,E3Z0    ,NEL     ,
     4                U21X   ,U31X   ,U21Y       ,U31Y   ,RZ13    ,
     5                RZ23   ,X2T    ,X3T        ,Y2T    ,Y3T     ,
     6                AREAT  ,GBUF%SMSTR ,IDRIL  )
        CALL C3DEFT3(JFT,JLT,X2T,Y2T,X3T,Y3T,U21X,U21Y,U31X,U31Y,
     .                   BMRZT,RZ13,RZ23,AREAT,F_DEF,IDRIL )
      END IF !(ISMSTR ==10)THEN
C-----------Eij to new system
      IF (IFRAM_OLD==0) THEN
        CALL SHROTO3(JFT,JLT,ECOS,ESIN,EXX,
     .               EYY,EXY,EZX,EYZ,KXX,
     .               KYY,KXY)
      END IF
      CALL C3STRA3(JFT   ,JLT    ,PM      ,
     2             MAT   ,AREA   ,EXX     ,EYY     ,EXY      ,
     3             EZX   ,EYZ    ,KXX     ,KYY     ,KXY      ,
     4             GEO    ,PID   ,NU      ,SHF     ,GBUF%STRA,
     5             SSP   ,RHO    ,EPSDOT  ,
     6             NFT   ,ISTRAIN,ISMSTR  ,
     7             UX1   ,UX2    ,UX3     ,UY1     ,UY2      ,
     8             UY3   ,PX1    ,PY1     ,PY2     ,MTN      ,
     9             F_DEF ,WKXY  ,GBUF%STRW,NEL     )
C-----------F_DEF to new system
      IF (IFRAM_OLD==0.AND.ISMSTR>=10) THEN
       CALL SHTROTO3(JFT,JLT,ECOS,ESIN,GBUF%STRA,
     .               F_DEF,ISMSTR,NEL)
      END IF
c-------------------------------------------
c    COMPUTE Regularized non local variable in Gauss point
c-------------------------------------------     
      IF (INLOC > 0) THEN
        L_NLOC = NLOC_DMG%L_NLOC
        DNL  => NLOC_DMG%DNL(1:L_NLOC) ! DNL = non local variable increment
        DO I=JFT,JLT
          NC1(I) = IXTG(2,I)
          NC2(I) = IXTG(3,I)
          NC3(I) = IXTG(4,I)
        ENDDO
        DO K = 1,NDDL
#include "vectorize.inc"        
          DO I=JFT,JLT
            INOD(1) = NLOC_DMG%IDXI(NC1(I))
            INOD(2) = NLOC_DMG%IDXI(NC2(I))
            INOD(3) = NLOC_DMG%IDXI(NC3(I))
            IPOS(1) = NLOC_DMG%POSI(INOD(1))
            IPOS(2) = NLOC_DMG%POSI(INOD(2))
            IPOS(3) = NLOC_DMG%POSI(INOD(3))
            VAR_REG(I,K) = THIRD*(DNL(IPOS(1)+K-1) 
     .                          + DNL(IPOS(2)+K-1) 
     .                          + DNL(IPOS(3)+K-1))
          ENDDO
        ENDDO
      ENDIF           
!-------------------------------------------------------------------------------
!       global element strain rate (shell energy equivalent)
!-----------------------------------------------------------
!       e = 1/t integ[1/2 e (eps_m + k z)^2 dz ]
!       e = 1/2 e eps_eq^2
!       eps_eq = sqrt[ eps_m^2 + 1/12 k^2t^2 ]
!-------------------------------------------------------------------------------
      dt1   = dt1c(1) 
      dtinv = dt1 / max(dt1**2,em20)  ! inverse of dt
      asrate = one   ! to be changed for default value
#include "vectorize.inc"
      do i = 1,nel
        eps_k2 = (kxx(i)*kxx(i)+kyy(i)*kyy(i)+kxx(i)*kyy(i)              
     &         + fourth*one_over_9*kxy(i)*kxy(i))*gbuf%thk(i)**2
        eps_m2 = four_over_3*(exx(i)*exx(i)+eyy(i)*eyy(i)+exx(i)*eyy(i)  
     &         + fourth*exy(i)*exy(i))
        epsd_pg(i) = sqrt(eps_k2 + eps_m2)*dtinv
      end do
!-------------------------------------------------------------------------------
      IF (JTHE /= 0 ) CALL TEMP3CG(JFT   ,JLT   ,PM     ,MAT   ,IXTG,   
     .                             TEMP ,TEMPEL )
C--------------------------
      IF ((IMON_MAT==1).AND.ITASK == 0)CALL STARTIME(TIMERS,35)
C--------------------------
        CALL CMAIN3(TIMERS,
     1   ELBUF_STR ,JFT       ,JLT       ,NFT       ,IPARG     ,
     2   NEL       ,MTN       ,IPLA      ,ITHK      ,GROUP_PARAM,
     3   PM        ,GEO       ,NPF       ,TF        ,BUFMAT    ,
     4   SSP       ,RHO       ,VISCMX    ,DT1C      ,SIGY      ,
     5   AREA      ,EXX       ,EYY       ,EXY       ,EZX       ,
     6   EYZ       ,KXX       ,KYY       ,KXY       ,NU        ,
     7   OFF       ,THK0      ,MAT       ,PID       ,MAT_ELEM  ,
     8   GBUF%FOR  ,GBUF%MOM  ,GBUF%STRA ,FAILWAVE  ,FWAVE_EL  ,
     9   GBUF%THK  ,GBUF%EINT ,IOFC      ,
     A   G         ,A11       ,A12       ,VOL0      ,INDX      ,
     B   NGL       ,ZCFAC     ,SHF       ,GS        ,EPSD_PG   ,
     C   KFTS      ,ISH3N     ,ALPE      ,
     D   DIR_A     ,DIR_B     ,IGEO      ,
     E   IPM       ,IFAILURE  ,NPG       ,FHEAT     ,
     F   TEMPEL    ,DIE       ,JTHE      ,IEXPAN    ,GBUF%TEMP ,
     G   IBID      ,BID       ,
     H   BID       ,BID       ,BID       ,BID       ,BID       ,
     I   BID       ,BID       ,BID       ,E1X0      ,E1Y0       ,
     J   E1Z0      ,E2X0      ,E2Y0      ,E2Z0      ,E3X0       ,
     K   E3Y0      ,E3Z0      ,NG        ,TABLE     ,IXFEM     ,
     L   BID       ,SENSORS   ,BID       ,ELCRKINI  ,
     M   DIR1_CRK  ,DIR2_CRK  ,ALDT      ,GLOB_THERM%IDT_THERM ,GLOB_THERM%THEACCFACT,
     N   ISMSTR    ,IR        ,IS        ,NLAY      ,NPT       ,
     O   IXLAY     ,IXEL      ,ISUBSTACK ,STACK     ,
     P   F_DEF     ,ITASK     ,DRAPE_SH3N  ,VAR_REG   ,NLOC_DMG ,
     Q   INDX_DRAPE,THKE      ,SEDRAPE     ,NUMEL_DRAPE,DT     ,
     R   NCYCLE    ,SNPC     ,STF          ,NXLAYMAX           ,
     S   IDEL7NOK  ,USERL_AVAIL ,MAXFUNC    ,NPTTOT,
     T   SBUFMAT   ,SDIR_A    , SDIR_B   ,GBUF%FOR_G)
C--------------------------
      IF ((IMON_MAT==1).AND.ITASK == 0)CALL STOPTIME(TIMERS,35)
C--------------------------
C     PAS DE TEMPS
C--------------------------
      IF (ISMSTR /= 3) CALL C3DT3(
     1        JFT    ,JLT       ,PM      ,OFF    ,DT2T    ,
     2        NELTST ,ITYPTST   ,STI     ,STIR   ,GBUF%OFF,
     3        SSP    ,VISCMX    ,ISMSTR  ,NFT    ,IOFC    ,
     4        ALPE   ,MSTG      ,DMELTG  ,JSMS   ,PTG     ,
     5        SHF    ,IGTYP     ,IGMAT   ,G      ,A11     ,
     6        A11R   ,GBUF%G_DT ,GBUF%DT ,ALDT   ,THK0    ,
     7        AREA   ,NGL       ,IMAT    ,MTN    ,NEL      , 
     8        ZOFFSET )
c
C--------keep  GBUF%FOR,GBUF%MOM in new local sys as LBUF%SIG    
      CALL C3SROTO3(JFT    ,JLT    ,ECOS    ,ESIN      ,GBUF%FOR,
     +             GBUF%MOM,NFOR  ,NMOM     ,IFRAM_OLD ,NEL     )
C--------------------------
C     BALANCE FOR EACH MATERIAL
C--------------------------
c      IFLAG=MOD(NCYCLE,NCPRI)
      IF(IPRI>0)
     +   CALL C3BILAN(
     1   JFT,        JLT,        PM,         V,
     2   GBUF%THK,   GBUF%EINT,  PMSAV,      IPARTTG,
     3   RHO,        VOL00,      IXTG,       X,
     4   R,          THK02,      AREA,       GRESAV,
     5   GRTH,       IGRTH,      OFF,        IBID,
     6   IBID,       IBID,       IBID,       IBID,
     7   IEXPAN,     GBUF%EINTTH,ITASK,      MAT,
     8   GBUF%VOL,   ACTIFXFEM,  IGRE,       SENSORS,
     9   NEL,        GBUF%G_WPLA,GBUF%WPLA)
C----------------------------
C     INTERNAL FORCES
C----------------------------
      CALL C3FINT3(JFT    ,JLT     ,NFOR   ,NMOM  ,THK0,
     2             PX1    ,PY1     ,PY2    ,F11   ,F12 ,
     3             F13    ,F21     ,F22    ,F23   ,F31 ,
     4             F32    ,F33     ,M11    ,M12   ,M13 ,
     5             M21    ,M22     ,M23    ,NEL   )
      IF (IDRIL > 0) THEN
        CALL C3FINTRZ(JFT  ,JLT  ,THK0 ,AREA ,PX1  ,
     2                PY1  ,PY2  ,F11  ,F12  ,F13  ,
     3                F21  ,F22  ,F23  ,WXY  ,NFOR ,
     4                GBUF%HOURG,MLZ  ,BM0RZ,B0RZ,BKRZ,
     5                BERZ ,KRZ  ,RLZ  ,DT1C ,GBUF%EINT,
     6                OFF  ,VOL0 ,NEL)
      END IF
c-------------------------
c     Virtual internal forces of regularized non local ddl 
c--------------------------
      IF (INLOC > 0) THEN
        CALL C3FINT_REG(
     1   NLOC_DMG,        VAR_REG,         GBUF%THK,        NEL,
     2   OFF,             AREA,            NC1,             NC2,
     3   NC3,             PX1,             PY1,             PY2,
     4   ELBUF_STR%NLOC(1,1),               IMAT,            NDDL,
     5   ITASK,           DT2T,            ALDT,            GBUF%THK_I,
     6   GBUF%AREA,       NFT)
      ENDIF 
C-------------------------
c     SHELL THERMICS 
C--------------------------
      IF (JTHE /= 0) THEN
        DTIME = DT1C(1)
        IF (MAT_ELEM%MAT_PARAM(IMAT)%HEAT_FLAG == 1) THEN
          CALL THERM3C(NEL   ,PM(1,IMAT)      ,THK0  ,IXTG    ,
     .                 PX1   ,PY1   ,PY2    ,AREA    ,DTIME ,
     .                 TEMP  ,TEMPEL,FHEAT  ,THEM    ,GLOB_THERM%THEACCFACT) 
        ELSE
          CALL THERM3C(NEL   ,PM(1,IMAT)      ,THK0  ,IXTG    ,
     .                 PX1   ,PY1   ,PY2    ,AREA    ,DTIME   ,
     .                 TEMP  ,TEMPEL,DIE    ,THEM    ,GLOB_THERM%THEACCFACT) 
        END IF
      ENDIF
C--------------------------
C     THERMAL TIME STEP
C--------------------------
      IF (JTHE > 0 .AND. GLOB_THERM%IDT_THERM == 1)THEN
           call dttherm(nel     ,pm(1,mat(1))  ,npropm  ,glob_therm   ,
     .                  jtur    ,tempel  ,vol0    ,rho       ,
     .                  aldt    ,off     ,conde   ,gbuf%re   ,gbuf%rk )
      ENDIF 
C-------------------------
C     ASSEMBLY
C-------------------------
      CALL C3FCUM3(JFT,JLT,F,
     .             F11,F12,F13,F21,F22,F23,
     .             F31,F32,F33,FZERO,
     .             E1X      ,E1Y      ,E1Z     ,E2X      ,                 
     .             E2Y      ,E2Z      ,E3X     ,E3Y      ,E3Z      )       
      CALL C3MCUM3(JFT,JLT,M,
     .             M11,M12,M13,M21,M22,M23,M31,M32,M33,
     .             E1X      ,E1Y      ,E1Z     ,E2X      ,                 
     .             E2Y      ,E2Z      ,E3X     ,E3Y      ,E3Z      )       
c
      IF (IDRIL > 0) THEN
       CALL C3MZCUM3(JFT    ,JLT    ,MLZ    ,E3X0    ,E3Y0    ,
     .               E3Z0   ,M11    ,M12    ,M13    ,M21    ,
     .               M22    ,M23    ,M31    ,M32    ,M33)
      END IF
C 
      IF (IPARIT == 0) THEN
        CALL C3UPDT3(JFT  ,JLT  ,F   ,M   ,NVC  ,
     2             GBUF%OFF,OFF  ,STI ,STIR,STIFN,
     3             STIFR  ,IXTG ,GLOB_THERM%NODADT_THERM,
     4             F11    ,F12    ,F13  ,F21 ,F22 ,F23  ,
     5             F31    ,F32  ,F33 ,M11 ,M12  ,
     7             M13    ,M21  ,M22 ,M23 ,M31  ,
     8             M32    ,M33  ,JTHE,THEM,FTHE ,
     9             GBUF%EINT,PM   ,AREA,GBUF%THK,
     A             PMSAV,MAT,IPARTTG,CONDN,CONDE)
      ELSE
        CALL C3UPDT3P(JFT ,JLT  ,GBUF%OFF,OFF,STI,
     2             STIR   ,FSKY ,FSKY,IADTG ,F11,
     4             F12    ,F13  ,F21 ,F22 ,F23  ,
     5             F31    ,F32  ,F33 ,M11 ,M12  ,
     7             M13    ,M21  ,M22 ,M23 ,M31  ,
     8             M32    ,M33  ,JTHE,THEM,FTHESKY,
     9             GBUF%EINT,PM   ,AREA,GBUF%THK ,
     B             PMSAV  ,MAT  ,IPARTTG,CONDNSKY,
     C             CONDE  ,GLOB_THERM%NODADT_THERM)
      ENDIF
C
      IF (ICSEN > 0) CALL CSENS3(JFT  ,JLT  ,PID  ,IGEO  ,EPSD_PG)
C-------------------------
c     SHELL CRACKS 
C--------------------------
      IF (ICRACK3D > 0 .AND. IXFEM > 0) THEN
        DO ILAY=1,NXLAY                                        
          ! crack length calculation for advancing crack                                            
          CRKLEN => ELBUF_STR%BUFLY(ILAY)%DMG(1:NEL)
          CALL  CRKLEN3N_ADV(
     .           NEL       ,NFT       ,ILAY      ,NLAY      ,IXTG      ,
     .           CRKLEN    ,ELCRKINI  ,IEL_CRK   ,DIR1_CRK  ,DIR2_CRK  ,     
     .           NODEDGE   ,CRKEDGE   ,XEDGE3N   ,NGL       ,X2L       ,
     .           X3L       ,Y2L       ,Y3L       ,ALDT      )
c
          CALL CRKLAYER3N_ADV(                                          
     .         NEL       ,NFT       ,ILAY      ,NXLAY     ,IXTG      ,   
     .         ELCUTC    ,ELCRKINI  ,IEL_CRK   ,INOD_CRK  ,IADTG_CRK ,                  
     .         NODENR    ,DIR1_CRK  ,DIR2_CRK  ,NODEDGE   ,CRKNODIAD ,         
     .         KNOD2ELC  ,CRKEDGE   ,XEDGE3N   ,NGL       ,AREA      ,
     .         X2L       ,X3L       ,Y2L       ,Y3L       )
c
          CALL CRKLAYER3N_INI(                                            
     .         NEL       ,NFT       ,ILAY      ,NXLAY     ,IXTG      ,  
     .         ELCUTC    ,ELCRKINI  ,IEL_CRK   ,INOD_CRK  ,IADTG_CRK ,            
     .         NODENR    ,DIR1_CRK  ,DIR2_CRK  ,NODEDGE   ,CRKNODIAD ,           
     .         KNOD2ELC  ,CRKEDGE   ,XEDGE3N   ,NGL       ,AREA      ,
     .         X2L       ,X3L       ,Y2L       ,Y3L       )
        ENDDO                                    
C
        CALL CRKOFFTG(ELBUF_STR,XFEM_STR  ,
     .                JFT      ,JLT       ,NFT    ,IR      ,IS        ,
     .                NXLAY    ,IEL_CRK   ,CRKEDGE,XEDGE3N )
      END IF                                                           

c--------------------------------------------
c     Front wave
c--------------------------------------------
      IF (IFAILWAVE > 0) THEN
        CRKDIR => ELBUF_STR%BUFLY(1)%CRKDIR
c        
        CALL SET_FAILWAVE_NOD3(FAILWAVE   ,FWAVE_EL ,NGL      ,
     .       NEL      ,IXTG     ,ITAB     ,CRKDIR   ,DIR_A    ,
     .       L_DIRA   ,X2L      ,X3L      ,Y2L      ,Y3L      )
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
