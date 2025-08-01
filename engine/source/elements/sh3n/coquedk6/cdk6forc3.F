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
cc
!||====================================================================
!||    cdk6forc3        ../engine/source/elements/sh3n/coquedk6/cdk6forc3.F
!||--- called by ------------------------------------------------------
!||    forintc          ../engine/source/elements/forintc.F
!||--- calls      -----------------------------------------------------
!||    c3bilan          ../engine/source/elements/sh3n/coque3n/c3bilan.F
!||    cbavisc          ../engine/source/elements/shell/coqueba/cbavisc.F
!||    cdk6coor3        ../engine/source/elements/sh3n/coquedk6/cdk6coor3.F
!||    cdk6defo3        ../engine/source/elements/sh3n/coquedk6/cdk6defo3.F
!||    cdk6deri3        ../engine/source/elements/sh3n/coquedk6/cdk6deri3.F
!||    cdk6fcum3        ../engine/source/elements/sh3n/coquedk6/cdk6fcum3.F
!||    cdk6fint3        ../engine/source/elements/sh3n/coquedk6/cdk6fint3.F
!||    cdk6fint_reg     ../engine/source/elements/sh3n/coquedk6/cdk6fint_reg.F
!||    cdk6stra3        ../engine/source/elements/sh3n/coquedk6/cdk6stra3.F
!||    cdk6updt3        ../engine/source/elements/sh3n/coquedk6/cdk6updt3.F
!||    cdk6updt3p       ../engine/source/elements/sh3n/coquedk6/cdk6updt3.F
!||    cmain3           ../engine/source/materials/mat_share/cmain3.F
!||    cncoef3          ../engine/source/elements/sh3n/coquedk/cncoef3.F
!||    cndt3            ../engine/source/elements/sh3n/coquedk/cndt3.F
!||    dttherm          ../engine/source/time_step/dttherm.F90
!||--- uses       -----------------------------------------------------
!||    drape_mod        ../engine/share/modules/drape_mod.F
!||    dt_mod           ../engine/source/modules/dt_mod.F
!||    dttherm_mod      ../engine/source/time_step/dttherm.F90
!||    elbufdef_mod     ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    failwave_mod     ../common_source/modules/failwave_mod.F
!||    glob_therm_mod   ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    mat_elem_mod     ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    nlocal_reg_mod   ../common_source/modules/nlocal_reg_mod.F
!||    sensor_mod       ../common_source/modules/sensor_mod.F90
!||    stack_mod        ../engine/share/modules/stack_mod.F
!||    table_mod        ../engine/share/modules/table_mod.F
!||    timer_mod        ../engine/source/system/timer_mod.F90
!||====================================================================
      SUBROUTINE CDK6FORC3(TIMERS,
     1   ELBUF_STR,   JFT,         JLT,         PM,
     2   IXTG,        X,           F,           M,
     3   V,           R,           FAILWAVE,    NVC,
     4   MTN,         GEO,         TF,          NPF,
     5   BUFMAT,      PMSAV,       DT2T,        NELTST,
     6   ITYPTST,     STIFN,       STIFR,       FSKY,
     7   IADTG,       GROUP_PARAM, EPSDOT,      OFFSET,
     8   IPARTTG,     THKE,        F11,         F12,
     9   F13,         F21,         F22,         F23,
     A   F31,         F32,         F33,         F14,
     B   F15,         F16,         F24,         F25,
     C   F26,         F34,         F35,         F36,
     D   MAT_ELEM,    NEL,         ISTRAIN,     IHBE,
     E   ITHK,        IOFC,        IPLA,        NFT,
     F   ISMSTR,      NPT,         KFTS,        IXTG1,
     G   IADTG1,      IGEO,        IPM,         IFAILURE,
     H   IEXPAN,      GRESAV,      GRTH,        IGRTH,
     I   MSTG,        DMELTG,      JSMS,        TABLE,
     J   IPARG,       SENSORS,     PTG,         JTHE,
     K   CONDN,       CONDNSKY,    ISUBSTACK,   STACK,
     L   ITASK,       DRAPE_SH3N,  IPRI,        NLOC_DMG,
     M   INDX_DRAPE,  IGRE,        JTUR,        DT,
     N   NCYCLE,      SNPC,        STF ,        GLOB_THERM,
     N   NXLAYMAX,    IDEL7NOK,    USERL_AVAIL, MAXFUNC,
     O   SBUFMAT  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TIMER_MOD
      USE MAT_ELEM_MOD
      USE TABLE_MOD
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
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr18_c.inc"
#include      "parit_c.inc"
#include      "scr14_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TIMER_), INTENT(INOUT) :: TIMERS
      INTEGER,INTENT(IN)    :: USERL_AVAIL ! Flag for User libraries availability
      INTEGER,INTENT(IN)    :: MAXFUNC     ! Maximum number of functions
      INTEGER,INTENT(INOUT) :: IDEL7NOK    ! Element deletion flag for IDEL flag in contact interfaces
      INTEGER,INTENT(IN)    :: SBUFMAT     ! Buffer material flag
      INTEGER,INTENT(IN)    :: STF         ! Size of TF
      INTEGER,INTENT(IN)    :: SNPC        ! Size of NPC
      INTEGER,INTENT(IN)    :: NCYCLE      ! Current cycle number
      INTEGER, INTENT(IN) :: NXLAYMAX
      INTEGER, INTENT(IN) :: JTUR
      INTEGER, INTENT(IN) :: IGRE
      INTEGER JFT, JLT, NVC, MTN,NELTST,ITYPTST,OFFSET,
     .   NEL,ISTRAIN,IHBE ,NPT,KFTS,IUN,
     .   ITHK,IOFC,IPLA,NFT,ISMSTR,IFAILURE,IEXPAN , JSMS , JTHE,
     .   ISUBSTACK,ITASK,IPRI
      INTEGER NPF(*),IXTG(NIXTG,*),IADTG(3,*),IXTG1(4,*),IADTG1(3,*),
     .   IPARTTG(*),IGEO(NPROPGI,*),IPM(NPROPMI,*),GRTH(*),IGRTH(*),
     .   IPARG(*),INDX_DRAPE(STDRAPE)
C     REAL
      my_real 
     .   PM(NPROPM,*), X(*), F(*), M(*), V(*), R(*),
     .   GEO(NPROPG,*), TF(*), BUFMAT(*), PMSAV(*),STIFN(*),
     .   STIFR(*),FSKY(*),EPSDOT(6,*),THKE(*),DT2T,
     .   F11(MVSIZ), F12(MVSIZ), F13(MVSIZ),
     .   F21(MVSIZ), F22(MVSIZ), F23(MVSIZ),
     .   F31(MVSIZ), F32(MVSIZ), F33(MVSIZ),
     .   F14(MVSIZ), F15(MVSIZ), F16(MVSIZ),
     .   F24(MVSIZ), F25(MVSIZ), F26(MVSIZ),
     .   F34(MVSIZ), F35(MVSIZ), F36(MVSIZ),
     .   GRESAV(*),MSTG(*), DMELTG(*), PTG(3,*),CONDN(*),CONDNSKY(*)
      TYPE (TTABLE) TABLE(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (STACK_PLY) :: STACK
      TYPE (FAILWAVE_STR_) ,TARGET :: FAILWAVE 
      TYPE (GROUP_PARAM_) :: GROUP_PARAM
      TYPE (NLOCAL_STR_), TARGET :: NLOC_DMG
      TYPE (DRAPE_),DIMENSION(NUMELTG_DRAPE) :: DRAPE_SH3N
      TYPE (MAT_ELEM_)   ,INTENT(INOUT) :: MAT_ELEM
      TYPE (SENSORS_) ,INTENT(INOUT) :: SENSORS
      TYPE (DT_) ,INTENT(IN) :: DT
      type (glob_therm_) ,intent(inout)   :: glob_therm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
c  indx utilise localement contrairement aux coques 4n 
      INTEGER MAT(MVSIZ),PID(MVSIZ),NGL(MVSIZ),INDX(MVSIZ),IVS(MVSIZ),
     .   FWAVE_EL(NEL)
      INTEGER 
     .   I, J, NG,NPG,NNOD,PT0,PT1,PT2,NVS,IFLAG,IBID,NFAIL,
     .   IR,IS,ILAY,NLAY,L_DIRA,L_DIRB,J1,J2,N1,N2,N3,
     .   IGTYP,IGMAT,NPTTOT,IREP,IFAILWAVE,IDRAPE,IT,NPTT,
     .   ACTIFXFEM,SEDRAPE,NUMEL_DRAPE
      INTEGER ,DIMENSION(:) ,POINTER     :: FWAVE_NOD
      PARAMETER (NNOD = 3)
      my_real 
     .   STI(MVSIZ),STIR(MVSIZ),RHO(MVSIZ),
     .   SSP(MVSIZ),VISCMX(MVSIZ),AREA(MVSIZ),AREA2(MVSIZ),
     .   AREA4(MVSIZ),AREA5(MVSIZ),AREA6(MVSIZ)
      my_real 
     .   EXX(MVSIZ), EYY(MVSIZ), EXY(MVSIZ), EXZ(MVSIZ), EYZ(MVSIZ),
     .   KXX(MVSIZ), KYY(MVSIZ), KXY(MVSIZ),
     .   PX2(MVSIZ),PY2(MVSIZ),  PX3(MVSIZ), PY3(MVSIZ),
     .   PB1(MVSIZ,9),PB2(MVSIZ,9),PB3(MVSIZ,18),
     .   OFF(MVSIZ), SIGY(MVSIZ),THK0(MVSIZ),
     .   NU(MVSIZ) , SHF(MVSIZ), DT1C(MVSIZ)
      my_real 
     .   G(MVSIZ)   , YM(MVSIZ)  , A11(MVSIZ)   , A12(MVSIZ),
     .   VOL0(MVSIZ),THK02(MVSIZ),ZCFAC(MVSIZ,2), GS(MVSIZ),
     .   ALPE(MVSIZ)
      my_real 
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),R21(MVSIZ),R22(MVSIZ),
     .   R23(MVSIZ),R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   N4X(MVSIZ),N4Y(MVSIZ),N4Z(MVSIZ),N5X(MVSIZ),N5Y(MVSIZ),
     .   N5Z(MVSIZ),N6X(MVSIZ),N6Y(MVSIZ),N6Z(MVSIZ),
     .   X2(MVSIZ),Y2(MVSIZ),X3(MVSIZ),Y3(MVSIZ),
     .   X4(MVSIZ),Y4(MVSIZ),Z4(MVSIZ),
     .   X5(MVSIZ),Y5(MVSIZ),Z5(MVSIZ),
     .   X6(MVSIZ),Y6(MVSIZ),Z6(MVSIZ),
     .   VLX(MVSIZ,2),VLY(MVSIZ,2),VLZ(MVSIZ,6),
     .   VZ4(MVSIZ,3),VZ5(MVSIZ,3),VZ6(MVSIZ,3),
     .   ALDT(MVSIZ),AMU(MVSIZ),VDEF(MVSIZ,8),DIE(MVSIZ),
     .   TEMPEL(MVSIZ),BID,KRZ(MVSIZ),CONDE(MVSIZ),A11R(MVSIZ)
      my_real , DIMENSIOn(NEL)  ::  ZOFFSET
      my_real, 
     .    ALLOCATABLE, DIMENSION(:), TARGET :: DIRA,DIRB
      my_real,
     .  DIMENSION(:) ,POINTER  :: DIR_A,DIR_B
!     variables for heat transfer
      my_real :: dtinv,asrate,eps_m2,eps_k2
      my_real, dimension(nel)   :: epsd_pg
      my_real, dimension(mvsiz) :: fheat
!
C--- Variables pour le non-local
      INTEGER :: NDDL, K, INOD(3),NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), L_NLOC, IPOS(3),INLOC
      my_real, DIMENSION(:,:), ALLOCATABLE :: VAR_REG
      my_real, DIMENSION(:), POINTER :: DNL,UNL
C-----
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF
      TYPE(L_BUFEL_DIR_) ,POINTER :: LBUF_DIR
      INTEGER SDIR_A    ! Size of DIR_A
      INTEGER SDIR_B    ! Size of DIR_B
C=======================================================================
C GENERALIZED STRAINS ARE SAVED AT NB8 (GENERALIZED FOR ALL MATERIALS)
C     NB1    |5*JLT    | FORCES  
C     NB2    |3*JLT    | MOMENTS
C     NB3    |JLT      | THK0SEUR
C     NB4    |5*JLT    | UNUSED : ONLY FOR FINDING OFF
C     NB5    |2*JLT    | ENERGIES
C     NB6    |JLT      | OFF
C     NB8    |8*JLT    | DEFORMATIONS
C============================== =========================================
      GBUF   => ELBUF_STR%GBUF
      IDRAPE = ELBUF_STR%IDRAPE
      IUN  = 1
      IBID = 0
      BID  = ZERO
      IREP = IPARG(35)
      ACTIFXFEM = IPARG(70)
      INLOC= IPARG(78)
      NLAY = ELBUF_STR%NLAY      
      SEDRAPE = STDRAPE
      NUMEL_DRAPE = NUMELTG_DRAPE
      ! thermal transfert for 3n shells type cdkforc is not available
      TEMPEL(:) = ZERO
      FHEAT(:)  = ZERO
cc      NPT  = MAX(NLAY,NPTT) --> set to = IPARG(6) , keeping it original
C                                 to allow for NPT = 0 (global LAW_3
      NPG  = 0
      IR   = 1
      IS   = 1
      NG   = 1
C
      NPTTOT  = 0
      DO ILAY=1,NLAY
        NPTTOT = NPTTOT + ELBUF_STR%BUFLY(ILAY)%NPTT
      ENDDO
      IF (NPT == 0) NPTTOT = NPT  !  compatibility with global integration
      NDDL = NPTTOT
      ALLOCATE(VAR_REG(NEL,NDDL))
c--------------------------------------------
c     Front wave
c--------------------------------------------
      IFAILWAVE = IPARG(79)
      IF (IFAILWAVE > 0 .and. FAILWAVE%WAVE_MOD == 1) THEN
        DO I=JFT,JLT
          N1 = FAILWAVE%IDXI(IXTG(2,I))
          N2 = FAILWAVE%IDXI(IXTG(3,I))
          N3 = FAILWAVE%IDXI(IXTG(4,I))
          NFAIL = FAILWAVE%FWAVE_NOD(1,N1,1) 
     .          + FAILWAVE%FWAVE_NOD(1,N2,1)
     .          + FAILWAVE%FWAVE_NOD(1,N3,1)
          IF (NFAIL > 0) FWAVE_EL(I) = 1
        ENDDO
      ENDIF
c-------------------------------------
      L_DIRA = ELBUF_STR%BUFLY(1)%LY_DIRA
      L_DIRB = ELBUF_STR%BUFLY(1)%LY_DIRB
      IGTYP = IGEO(11,IXTG(5,1))
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
        SDIR_A=NPTTOT*NEL*L_DIRA
        SDIR_B=NPTTOT*NEL*L_DIRB
        DIR_A => DIRA(1:NPTTOT*NEL*L_DIRA)
        DIR_B => DIRB(1:NPTTOT*NEL*L_DIRB)
      ELSE ! idrape
        SDIR_A=NLAY*NEL*L_DIRA
        SDIR_B=NLAY*NEL*L_DIRB
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
      DO I=JFT,JLT
        A11R(I)   = ZERO
      ENDDO 
c---
      CALL CDK6COOR3(ELBUF_STR,JFT,JLT,MAT,PID,
     .              NGL,X,V,R,IXTG,GBUF%OFF,
     .              OFF,R11,R12,R13,R21,R22,R23,R31,R32,R33,
     .              N4X,N4Y,N4Z,N5X,N5Y,N5Z,N6X,N6Y,N6Z,
     .              X2,Y2,X3,Y3,X4,Y4,Z4,X5,Y5,Z5,X6,Y6,Z6,
     .              GBUF%SMSTR,AREA,AREA2,
     .              VLX,VLY,VLZ,VZ4,VZ5,VZ6,ISMSTR,NLAY,IREP,
     .              DIR_A   ,DIR_B ,IGEO  ,
     .              IXTG1   ,NVS     ,IVS   ,AREA4 ,AREA5 ,
     .              AREA6   ,NEL)
      IGTYP = IGEO(11,PID(1))
      IGMAT = IGEO(98,PID(1))
C
      CALL CNCOEF3(JFT    ,JLT     ,PM      ,MAT     ,GEO     ,
     2             PID    ,OFF     ,AREA    ,SHF     ,THK0    ,
     3             THK02  ,NU      ,G       ,YM      ,
     4             A11    ,A12     ,GBUF%THK,THKE    ,SSP     ,
     5             RHO    ,VOL0    ,GS      ,MTN     ,ITHK    ,
     6             NPTTOT ,DT1C    , DT1    ,IHBE    ,AMU     ,
     7             KRZ    ,IGEO    ,A11R    ,ISUBSTACK , STACK%PM,
     8             NEL     ,ZOFFSET)
      CALL CDK6DERI3(JFT ,JLT, X2,Y2,X3,Y3,AREA2,ALPE,ALDT,NU,THK02,
     1               PX2,PY2,PX3,PY3,X4,Y4,Z4,X5,Y5,Z5,X6,Y6,Z6,
     2               N4X,N4Y,N4Z,N5X,N5Y,N5Z,N6X,N6Y,N6Z,
     3               AREA4,AREA5,AREA6,PB1,PB2,PB3,NVS,IVS,IXTG1)
C
      CALL CDK6DEFO3(JFT,JLT,VLX,VLY,VLZ,VZ4,VZ5,VZ6,
     1               PX2,PY2,PX3,PY3,PB1,PB2,PB3,VDEF)  
      CALL CDK6STRA3(JFT, JLT, NFT, VDEF,GBUF%STRA,
     1             EXX, EYY, EXY, EXZ,  EYZ,  
     2             KXX, KYY, KXY, DT1C, EPSDOT,
     3             IEPSDOT, ISTRAIN,NEL)
c-------------------------------------------
c    COMPUTE Regularized non local variable in Gauss point
c-------------------------------------------     
      IF (INLOC > 0) THEN
        L_NLOC = NLOC_DMG%L_NLOC
        DNL  => NLOC_DMG%DNL(1:L_NLOC) ! DNL = non local variable increment
        UNL  => NLOC_DMG%UNL(1:L_NLOC)
        DO I = JFT,JLT
          NC1(I)  = IXTG(2,I)
          NC2(I)  = IXTG(3,I)
          NC3(I)  = IXTG(4,I)
        ENDDO
        DO K = 1,NDDL
#include "vectorize.inc"        
          DO I = JFT,JLT
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
      dtinv = dt1 / max(dt1**2,em20)  ! inverse of dt
      asrate = one   ! to be changed for default value
#include "vectorize.inc"
      do i = 1,nel
        eps_k2 = (kxx(i)**2+kyy(i)**2+kxx(i)*kyy(i)+fourth*kxy(i)**2)
     .         * one_over_9*gbuf%thk(i)**2
        eps_m2 = four_over_3*(exx(i)**2+eyy(i)**2+exx(i)*eyy(i) + fourth*exy(i)**2)
        epsd_pg(i) = sqrt(eps_k2 + eps_m2)*dtinv
      end do
!-------------------------------------------------------------------------------
       CALL CMAIN3(TIMERS,
     1   ELBUF_STR ,JFT       ,JLT       ,NFT       ,IPARG      ,
     2   NEL       ,MTN       ,IPLA      ,ITHK      ,GROUP_PARAM,
     3   PM        ,GEO       ,NPF       ,TF        ,BUFMAT     ,
     4   SSP       ,RHO       ,VISCMX    ,DT1C      ,SIGY       ,
     5   AREA      ,EXX       ,EYY       ,EXY       ,EXZ        ,
     6   EYZ       ,KXX       ,KYY       ,KXY       ,NU         ,
     7   OFF       ,THK0      ,MAT       ,PID       ,MAT_ELEM   ,
     8   GBUF%FOR  ,GBUF%MOM  ,GBUF%STRA ,FAILWAVE  ,FWAVE_EL   ,
     9   GBUF%THK  ,GBUF%EINT ,IOFC      ,
     A   G         ,A11       ,A12       ,VOL0      ,INDX       ,
     B   NGL       ,ZCFAC     ,SHF       ,GS        ,EPSD_PG    ,
     C   KFTS      ,IHBE      ,ALPE      ,
     D   DIR_A     ,DIR_B     ,IGEO      ,
     E   IPM       ,IFAILURE  ,NPG       ,FHEAT     ,
     F   TEMPEL    ,DIE       ,IBID      ,IBID      ,BID        ,
     G   IBID      ,BID       ,
     H   BID       ,BID       ,BID       ,BID       ,BID        ,
     I   BID       ,BID       ,BID       ,R11       ,R12        ,
     J   R13       ,R21       ,R22       ,R23       ,R31        ,
     K   R32       ,R33       ,NG        ,TABLE     ,IBID       ,
     L   BID       ,SENSORS   ,BID       ,IBID       ,
     M   BID       ,BID       ,ALDT      ,GLOB_THERM%IDT_THERM ,GLOB_THERM%THEACCFACT,
     N   ISMSTR    ,IR        ,IS        ,NLAY      ,NPT        ,
     O   IBID      ,IBID      ,ISUBSTACK ,STACK     ,
     P   BID       ,ITASK     ,DRAPE_SH3N  ,VAR_REG   ,NLOC_DMG  ,
     R   INDX_DRAPE,THKE      ,SEDRAPE     ,NUMEL_DRAPE ,DT      ,
     Q   NCYCLE    ,SNPC      ,STF         ,NXLAYMAX    ,IDEL7NOK  ,
     R   USERL_AVAIL ,MAXFUNC ,NPTTOT      ,SBUFMAT     ,SDIR_A    ,SDIR_B    ,
     S   GBUF%FOR_G)
C----------------------------------------------------------------------------
C     FORCES VISCOCITE 
C----------------------------
C       uniquement membranaire pour l'instant --------
      CALL CBAVISC(JFT     ,JLT     ,VDEF    ,AMU ,OFF,
     2             SHF     ,NU      ,RHO     ,SSP ,AREA,
     3             THK0    ,GBUF%FOR,GBUF%MOM,IUN ,MTN,
     4             IPARTTG ,PMSAV   ,DT1     ,NEL )
C----------------------------
C     FORCES INTERNES
C----------------------------       
      CALL CDK6FINT3(JFT,JLT,VOL0,THK0,GBUF%FOR,GBUF%MOM,
     1                      PX2,PY2,PX3,PY3,PB1,PB2,PB3,
     2                      F11,F12,F13,F21,F22,F23,F31,F32,F33,
     3                      F14,F15,F16,F24,F25,F26,F34,F35,F36,
     4                      N4X,N4Y,N4Z,N5X,N5Y,N5Z,N6X,N6Y,N6Z,
     5                      NEL)
C-------------------------
C     ASSEMBLE
C-------------------------
      CALL CDK6FCUM3(JFT,JLT,
     1              R11,R12,R13,R21,R22,R23,R31,R32,R33,
     2              F11,F12,F13,F21,F22,F23,F31,F32,F33,
     3              F14,F15,F16,F24,F25,F26,F34,F35,F36)
C--------------------------
C     PAS DE TEMPS
C--------------------------
      CALL CNDT3(
     1        JFT    ,JLT    ,OFF      ,DT2T   ,AMU     ,
     2        NELTST ,ITYPTST,STI      ,STIR   ,GBUF%OFF,
     3        SSP    ,VISCMX ,  RHO    ,   VOL0,THK0,THK02,
     4        A11    ,ALDT   , ALPE    ,    NGL,ISMSTR,
     5        IOFC   ,NNOD   ,AREA     ,G      ,SHF   ,
     6        MSTG   ,DMELTG ,JSMS     ,PTG    ,IGTYP ,
     7        IGMAT  ,A11R   ,GBUF%G_DT, GBUF%DT,MTN   ,
     8        PM     ,MAT(JFT), NEL     ,ZOFFSET)
C--------------------------
C     THERMAL TIME STEP
C--------------------------
         IF (JTHE /= 0 .AND. GLOB_THERM%IDT_THERM == 1)THEN
           call dttherm(nel     ,pm(1,mat(1))  ,npropm  ,glob_therm   ,
     .                  jtur    ,tempel  ,vol0    ,rho       ,
     .                  aldt    ,off     ,conde   ,gbuf%re   ,gbuf%rk )
         ENDIF
c-------------------------
c     Virtual internal forces of regularized non local ddl 
c--------------------------
      IF (INLOC > 0) THEN
        CALL CDK6FINT_REG(
     1   NLOC_DMG,        VAR_REG,         THK0,            NEL,
     2   OFF,             AREA,            NC1,             NC2,
     3   NC3,             PX2,             PY2,             PX3,
     4   PY3,             ELBUF_STR%NLOC(1,1),               IXTG(1,JFT),
     5   NDDL,            ITASK,           DT2T,            ALDT,
     6   GBUF%THK_I,      GBUF%AREA,       NFT)
      ENDIF
c-------------------------------
C--------------------------
C     BILANS PAR MATERIAU
C--------------------------
      IF(IPRI>0)
     +   CALL C3BILAN(
     1   JFT,        JLT,        PM,         V,
     2   GBUF%THK,   GBUF%EINT,  PMSAV,      IPARTTG,
     3   RHO,        VOL0,       IXTG,       X,
     4   R,          THK02,      AREA,       GRESAV,
     5   GRTH,       IGRTH,      OFF,        IBID,
     6   IBID,       IBID,       IBID,       IBID,
     7   IEXPAN,     GBUF%EINTTH,ITASK,      MAT,
     8   GBUF%VOL,   ACTIFXFEM,  IGRE,       SENSORS,
     9   NEL,        GBUF%G_WPLA,GBUF%WPLA)
      IF(IPARIT==0)THEN
        CALL CDK6UPDT3(JFT  ,JLT  ,F   ,M   ,NVC  ,
     2             GBUF%OFF,OFF  ,STI ,STIR,STIFN,
     3             STIFR  ,IXTG ,IXTG1, F11  ,
     4             F12    ,F13  ,F21 ,F22 ,F23  ,
     5             F31    ,F32  ,F33 ,F14 ,F15  ,
     7             F16    ,F24  ,F25 ,F26 ,F34 ,
     8             F35    ,F36  ,NVS ,IVS )
      ELSE
        CALL CDK6UPDT3P(JFT   ,JLT  ,GBUF%OFF,OFF   ,STI   ,
     2                  STIR  ,FSKY ,FSKY   ,IADTG ,IADTG1,
     4                  F11   ,F12  ,F13    ,F21   ,F22   ,
     5                  F23   ,F31  ,F32    ,F33   ,F14   ,
     7                  F15   ,F16  ,F24    ,F25   ,F26   ,
     8                  F34   ,F35  ,F36    )
      ENDIF
c--------------------------------------------
c     Front wave
c--------------------------------------------
      IF (IFAILWAVE > 0 .and.  FAILWAVE%WAVE_MOD == 1) THEN
        DO I=JFT,JLT
          N1 = FAILWAVE%IDXI(IXTG(2,I))
          N2 = FAILWAVE%IDXI(IXTG(3,I))
          N3 = FAILWAVE%IDXI(IXTG(4,I))
          IF (FWAVE_EL(I) == -1) THEN
            FAILWAVE%FWAVE_NOD(1,N1,1) = 1
            FAILWAVE%FWAVE_NOD(1,N2,1) = 1
            FAILWAVE%FWAVE_NOD(1,N3,1) = 1
          ENDIF
        ENDDO
      ENDIF
C-----------
      IF (ALLOCATED(VAR_REG))  DEALLOCATE(VAR_REG)     
      RETURN
      END
