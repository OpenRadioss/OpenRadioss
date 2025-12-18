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
!||    alemain                      ../engine/source/ale/alemain.F
!||--- called by ------------------------------------------------------
!||    resol                        ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    agauge                       ../engine/source/ale/agauge.F
!||    alefvm_grav_init             ../engine/source/ale/alefvm/alefvm_grav_init.F
!||    alesub1                      ../engine/source/ale/subcycling/alesub1.F
!||    alethe                       ../engine/source/ale/alethe.F
!||    bcs_wall_trigger             ../engine/source/boundary_conditions/bcs_wall_trigger.F90
!||    bforc2                       ../engine/source/ale/bimat/bforc2.F
!||    build_connectivity           ../engine/source/multifluid/connectivity.F
!||    ebcs_main                    ../engine/source/boundary_conditions/ebcs/ebcs_main.F
!||    initbuf                      ../engine/share/resol/initbuf.F
!||    intti0                       ../engine/source/interfaces/interf/intti0.F
!||    multi_buf2var                ../engine/source/multifluid/multi_buf2var.F
!||    multi_compute_dt             ../engine/source/multifluid/multi_compute_dt.F
!||    multi_computevolume          ../engine/source/multifluid/multi_computevolume.F
!||    multi_face_elem_data         ../engine/source/multifluid/multi_face_data_elem.F
!||    multi_fvm2fem                ../engine/source/multifluid/multi_fvm2fem.F
!||    multi_globalize              ../engine/source/multifluid/multi_globalize.F
!||    multi_pressure_equilibrium   ../engine/source/multifluid/multi_pressure_equilibrium.F
!||    multi_timeevolution          ../engine/source/multifluid/multi_timeevolution.F
!||    multi_var2buf                ../engine/source/multifluid/multi_var2buf.F
!||    my_barrier                   ../engine/source/system/machine.F
!||    ns_fvm_diffusion             ../engine/source/multifluid/ns_fvm_diffusion.F
!||    qforc2                       ../engine/source/elements/solid_2d/quad/qforc2.F
!||    s4alesfem                    ../engine/source/elements/solid/solide4_sfem/s4alesfem.F
!||    s4forc3                      ../engine/source/elements/solid/solide4/s4forc3.F
!||    seggetv                      ../engine/source/interfaces/interf/seggetv.F
!||    sforc3                       ../engine/source/elements/solid/solide/sforc3.F
!||    sinit22_fvm                  ../engine/source/interfaces/int22/sinit22_fvm.F
!||    spmd_envois                  ../engine/source/mpi/fluid/spmd_cfd.F
!||    spmd_init_ebcs               ../engine/source/mpi/fluid/spmd_cfd.F
!||    spmd_l11vois                 ../engine/source/mpi/fluid/spmd_cfd.F
!||    spmd_l51vois                 ../engine/source/mpi/fluid/spmd_cfd.F
!||    spmd_segcom                  ../engine/source/mpi/fluid/spmd_cfd.F
!||    spmd_xvois                   ../engine/source/mpi/fluid/spmd_cfd.F
!||    startime                     ../engine/source/system/timer_mod.F90
!||    startimeg                    ../engine/source/system/timer.F
!||    stoptime                     ../engine/source/system/timer_mod.F90
!||    stoptimeg                    ../engine/source/system/timer.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod         ../common_source/modules/ale/ale_connectivity_mod.F
!||    ale_mod                      ../common_source/modules/ale/ale_mod.F
!||    alefvm_mod                   ../common_source/modules/ale/alefvm_mod.F
!||    bcs_mod                      ../common_source/modules/boundary_conditions/bcs_mod.F90
!||    bcs_wall_trigger_mod         ../engine/source/boundary_conditions/bcs_wall_trigger.F90
!||    diffusion_mod                ../engine/share/modules/diffusion_mod.F
!||    dt_mod                       ../engine/source/modules/dt_mod.F
!||    ebcs_mod                     ../common_source/modules/boundary_conditions/ebcs_mod.F90
!||    elbufdef_mod                 ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    element_mod                  ../common_source/modules/elements/element_mod.F90
!||    glob_therm_mod               ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    groupdef_mod                 ../common_source/modules/groupdef_mod.F
!||    h3d_mod                      ../engine/share/modules/h3d_mod.F
!||    initbuf_mod                  ../engine/share/resol/initbuf.F
!||    intbufdef_mod                ../common_source/modules/interfaces/intbufdef_mod.F90
!||    mat_elem_mod                 ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    matparam_def_mod             ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    multi_fvm_mod                ../common_source/modules/ale/multi_fvm_mod.F90
!||    multimat_param_mod           ../common_source/modules/multimat_param_mod.F90
!||    nlocal_reg_mod               ../common_source/modules/nlocal_reg_mod.F
!||    output_mod                   ../common_source/modules/output/output_mod.F90
!||    python_funct_mod             ../common_source/modules/python_mod.F90
!||    segvar_mod                   ../engine/share/modules/segvar_mod.F
!||    sensor_mod                   ../common_source/modules/sensor_mod.F90
!||    table_mod                    ../engine/share/modules/table_mod.F
!||    th_surf_mod                  ../common_source/modules/interfaces/th_surf_mod.F
!||    timer_mod                    ../engine/source/system/timer_mod.F90
!||====================================================================
      SUBROUTINE ALEMAIN( TIMERS,
     1         PM          ,GEO         ,X               ,A          ,V                 ,
     2         MS          ,WA          ,ELBUF_TAB       ,BUFMAT     ,PARTSAV           ,TF              ,
     3         VAL2        ,VEUL        ,FV              ,STIFN      ,FSKY              ,EANI            ,
     4         PHI         ,FILL        ,DFILL           ,ALPH       ,SKEW              ,W               ,
     5         D           ,DSAVE       ,ASAVE           ,DT2T       ,DT2SAVE           ,XCELL           ,
     6         IPARG       ,NPC         ,IXS             ,IXQ        ,IXTG              ,IADS            ,
     7         IFILL       ,ICODT       ,ISKEW           ,IMS        ,IADQ              ,
     8         NELTST      ,ITYPTST     ,IPARTS          ,IPARTQ     ,ITASK             ,
     A         NODFT       ,NODLT       ,NBRCVOIS        ,TEMP       ,FSAVSURF          ,
     B         NBSDVOIS    ,LNRCVOIS    ,LNSDVOIS        ,NERCVOIS   ,NESDVOIS          ,LERCVOIS        ,
     C         LESDVOIS    ,ISIZXV      ,IAD_ELEM        ,FR_ELEM    ,FSKYM             ,MSNF            ,
     D         IPARI       ,SEGVAR      ,ITAB            ,ISKWN      ,DIFFUSION         ,IRESP           ,
     E         VOLMON      ,FSAV        ,IGRSURF         ,NELTSA     ,
     F         ITYPTSA     ,WEIGHT      ,NPSEGCOM        ,LSEGCOM    ,IPM               ,IGEO            ,
     G         ITABM1      ,AGLOB      ,GRESAV            ,
     H         GRTH        ,IGRTH       ,LGAUGE          ,GAUGE      ,MSSA              ,
     I         DMELS       ,IGAUP       ,NGAUP           ,TABLE      ,MS0               ,
     J         XDP         ,IGRNOD      ,SFEM_NODVAR     ,FSKYI      ,ISKY              ,S_SFEM_NODVAR,
     K         INTBUF_TAB  ,IXT         ,IGRV            ,AGRAV      ,SENSORS           ,
     L         LGRAV       ,CONDNSKY    ,CONDN           ,MS_2D      ,MULTI_FVM         ,IGRTRUSS        ,
     M         IGRBRIC     ,NLOC_DMG    ,ID_GLOBAL_VOIS  ,FACE_VOIS  ,EBCS_TAB          ,ALE_CONNECTIVITY,
     N         MAT_ELEM    ,H3D_DATA    ,DT              ,OUTPUT     ,NEED_COMM_INTER18 ,IDTMINS         ,
     O         IDTMIN      ,MAXFUNC     ,IMON_MAT        ,USERL_AVAIL,
     P         impl_s      ,idyna       ,PYTHON          ,MATPARAM   ,GLOB_THERM  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TIMER_MOD
      USE PYTHON_FUNCT_MOD
      USE INITBUF_MOD
      USE TABLE_MOD
      USE MAT_ELEM_MOD
      USE INTBUFDEF_MOD
      USE ALEFVM_MOD
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
      USE NLOCAL_REG_MOD
      USE SENSOR_MOD
      USE EBCS_MOD
      USE BCS_MOD , only : BCS
      USE ALE_CONNECTIVITY_MOD
      USE DIFFUSION_MOD
      USE SEGVAR_MOD
      USE H3D_MOD
      USE ALE_MOD
      USE DT_MOD
      USE OUTPUT_MOD
      USE TH_SURF_MOD , only : TH_SURF_NUM_CHANNEL
      USE ELBUFDEF_MOD
      USE MULTIMAT_PARAM_MOD , ONLY : M51_IFLG6_SIZE
      USE MATPARAM_DEF_MOD, ONLY : MATPARAM_STRUCT_
      use bcs_wall_trigger_mod
      use glob_therm_mod
      use element_mod , only : nixs,nixq,nixtg,nixt
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
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
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "inter18.inc"
#include      "inter22.inc"
#include      "scr07_c.inc"
#include      "stati_c.inc"
#include      "macro.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TIMER_) :: TIMERS
      INTEGER,INTENT(IN) :: S_SFEM_NODVAR
      TYPE(MATPARAM_STRUCT_),DIMENSION(NUMMAT),INTENT(IN) :: MATPARAM !< material buffer
      INTEGER, INTENT(IN):: IRESP
      integer,dimension(102) :: IDTMIN
      INTEGER ,INTENT(IN) :: MAXFUNC
      INTEGER, INTENT(IN) :: IMON_MAT
      INTEGER, INTENT(IN) :: USERL_AVAIL
      INTEGER, INTENT(IN) :: IMPL_S
      INTEGER, INTENT(IN) :: IDYNA
      my_real,INTENT(INOUT) :: FSAVSURF(TH_SURF_NUM_CHANNEL,NSURF)
      INTEGER IXS(NIXS,NUMELS),IXQ(NIXQ,NUMELQ),IXTG(NIXTG,NUMELTG), ISKY(*),
     .   IFILL(NUMNOD,*), NPC(*), IPARG(NPARG,NGROUP),
     .   IADS(8,*),IADQ(4,*),ICODT(*),ISKEW(*), IMS(*),
     .   IGEO(NPROPGI,NUMGEO),
     .   IPARTS(*) ,IPARTQ(*),IPM(NPROPMI,*),NODFT,
     .   NELTST ,ITYPTST, ITASK, 
     .   NBRCVOIS(*),NBSDVOIS(*), LNRCVOIS(*), LNSDVOIS(*),
     .   NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*),
     .   NELTSA, ITYPTSA,NPSEGCOM(*),LSEGCOM(*),
     .   IAD_ELEM(*),FR_ELEM(*), IPARI(SIPARI),ITAB(NUMNOD),ISKWN(*),
     .   WEIGHT(*), ITABM1(*),
     .   ISIZXV,GRTH(*),IGRTH(*),LGAUGE(3,NBGAUGE),
     .   IGAUP(*),NGAUP(*),NODLT, IXT(NIXT,*),
     .   IGRV(*),LGRAV(*)
      INTEGER, INTENT(IN) :: IDTMINS
      ! for parith/on : ID_GLOBAL_VOIS --> user id ; FACE_VOIS --> face of the remote element
      INTEGER, DIMENSION(*), INTENT(in) :: ID_GLOBAL_VOIS,FACE_VOIS
      LOGICAL, INTENT(inout) :: NEED_COMM_INTER18 !< true if the mpi comm "exchange of remote XCELL data" is mandatory
      DOUBLE PRECISION XDP(3,*)
      
      my_real X(3,NUMNOD),V(3,NUMNOD),MS(*),PM(NPROPM,NUMMAT),SKEW(LSKEW,*),
     .        GEO(NPROPG,NGROUP),BUFMAT(*) ,W(3,NUMNOD),VEUL(*),FILL(NUMNOD,*),
     .        DFILL(NUMNOD,*),ALPH(*),TF(*),
     .        FV(*),A(3,NUMNOD),VAL2(*),PHI(*),
     .        PARTSAV(*) ,STIFN(*) ,D(3,NUMNOD),DSAVE(3,*),ASAVE(3,*),WA(*),
     .        FSKY(*),EANI(*), FSKYM(*),
     .        DT2T, DT2SAVE,
     .        AGLOB(3,*),GAUGE(LLGAUGE,*),MS0(*),
     .        MSNF(*),VOLMON(*),FSAV(NTHVKI,*),GRESAV(*),
     .        MSSA(*), DMELS(*),SFEM_NODVAR(S_SFEM_NODVAR),FSKYI(LSKYI,NFSKYI),
     .        AGRAV(*),CONDN(*),CONDNSKY(*),MS_2D(*),TEMP(*)
     
      TYPE(t_segvar) :: SEGVAR
      TYPE(TTABLE) TABLE(*)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      TYPE (NLOCAL_STR_)     :: NLOC_DMG 
!
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE(t_ebcs_tab), INTENT(INOUT) :: EBCS_TAB
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
      TYPE(T_DIFFUSION),INTENT(INOUT) :: DIFFUSION
      TYPE (MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM
      TYPE (H3D_DATABASE) :: H3D_DATA
      TYPE (SENSORS_) ,INTENT(INOUT)  :: SENSORS
      my_real, INTENT(INOUT) :: XCELL(3,SXCELL)
      TYPE (DT_) ,INTENT(INOUT)  :: DT
      TYPE(OUTPUT_), INTENT(INOUT) :: OUTPUT !< output structure
      TYPE(PYTHON_) :: PYTHON
      type (glob_therm_) ,intent(inout)   :: glob_therm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real, DIMENSION(MVSIZ,6) :: SVIS
      INTEGER N, M, NG, NVC, NF1,OFFSET,ISOLNOD,NSG,NEL,I,LENCOM,ISTRA,IBID,IOUTPRT
      INTEGER IPLA
      INTEGER IADBH, IAD22, NIN, NBRIC_L
      INTEGER SBUFVOIS,SZ_BUFVOIS
      integer :: j,ijk
      my_real FX(MVSIZ,10),FY(MVSIZ,10),FZ(MVSIZ,10),VOLN(MVSIZ) 
      my_real, TARGET  :: BID
      my_real, DIMENSION(:,:),ALLOCATABLE,TARGET, save  :: QMV
      my_real, POINTER :: pQMV
      ! SPMD array for adjacent data ALE/CFD
      my_real, DIMENSION(:,:),ALLOCATABLE ::BUFVOIS

      LOGICAL :: PRED
      
      SAVE BUFVOIS

      DATA OFFSET/0/
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------  

      IBID          = 0
      BID           = ZERO
      SBUFVOIS      = 6

C  IOUTPRT (assembly report)   
         IOUTPRT = 0
         IF(MOD(NCYCLE,IABS(NCPRI)) == 0 .OR. TT >= OUTPUT%TH%THIS .OR. MDESS /= 0
     .    .OR. (ALE%SUB%IALESUB /= 0 .AND. T1S+DT2S >= OUTPUT%TH%THIS)
     .    .OR. TT >= OUTPUT%TH%THIS1(1) .OR. TT >= OUTPUT%TH%THIS1(2)
     .    .OR. TT >= OUTPUT%TH%THIS1(3) .OR. TT >= OUTPUT%TH%THIS1(4) .OR. TT >= OUTPUT%TH%THIS1(5)
     .    .OR. TT >= OUTPUT%TH%THIS1(6) .OR. TT >= OUTPUT%TH%THIS1(7) .OR. TT >= OUTPUT%TH%THIS1(8)
     .    .OR. TT >= OUTPUT%TH%THIS1(9) .OR. NTH /= 0 .OR. NANIM /= 0
     .    .OR. TT >= TABFIS(1) .OR. TT >= TABFIS(2)
     .    .OR. TT >= TABFIS(3) .OR. TT >= TABFIS(4) .OR. TT >= TABFIS(5)
     .    .OR. TT >= TABFIS(6) .OR. TT >= TABFIS(7) .OR. TT >= TABFIS(8)
     .    .OR. TT >= TABFIS(9) .OR. TT >= TABFIS(10)
     .    .OR. (ALE%SUB%IALESUB /= 0 .AND. T1S+DT2S >= OUTPUT%TH%THIS1(1))
     .    .OR. (ALE%SUB%IALESUB /= 0 .AND. T1S+DT2S >= OUTPUT%TH%THIS1(2))
     .    .OR. (ALE%SUB%IALESUB /= 0 .AND. T1S+DT2S >= OUTPUT%TH%THIS1(3))
     .    .OR. (ALE%SUB%IALESUB /= 0 .AND. T1S+DT2S >= OUTPUT%TH%THIS1(4))
     .    .OR. (ALE%SUB%IALESUB /= 0 .AND. T1S+DT2S >= OUTPUT%TH%THIS1(5))
     .    .OR. (ALE%SUB%IALESUB /= 0 .AND. T1S+DT2S >= OUTPUT%TH%THIS1(6))
     .    .OR. (ALE%SUB%IALESUB /= 0 .AND. T1S+DT2S >= OUTPUT%TH%THIS1(7))
     .    .OR. (ALE%SUB%IALESUB /= 0 .AND. T1S+DT2S >= OUTPUT%TH%THIS1(8))
     .    .OR. (ALE%SUB%IALESUB /= 0 .AND. T1S+DT2S >= OUTPUT%TH%THIS1(9)) .OR. ISTAT==3) IOUTPRT=1
      
      if(itask==0) ALLOCATE(QMV(ale%global%s_qmv,2*ale%global%nv46)) !MM-ALE 51 only                       !OPTIM : SHOULD BE ALLOCATED IN ARRALOC
      IF(M51_IFLG6 == 1)SBUFVOIS = M51_IFLG6_SIZE
      
      IF(ITASK==0)ALLOCATE(BUFVOIS(SBUFVOIS ,NSVOIS+NQVOIS)) !SPMD array of adjacent ALE/CFD elems           !OPTIM : SHOULD BE ALLOCATED IN ARRALOC,  same for FLUX_VOIS & N4_VOIS
      IF(ITASK==0)SZ_BUFVOIS=SBUFVOIS*(NSVOIS+NQVOIS)

      IF(N2D /= 0 .AND. NMULT /= 0)THEN
        !WA(1:IADBH-1)                         = FLUX
        !WA(IADBH:IADBH+MAX(1,NMULT)*NUMELQ-1) = BHOLE
        IADBH=MAX(1,NMULT)*4*NUMELQ+1
      ELSE
        IADBH=1
      ENDIF
      !IFSUBM=1 : masse changed
      ALE%SUB%IFSUBM=1

      IF(IALE+IEULER /= 0)THEN
       DO N=NODFT,NODLT
        IF(ALE_CONNECTIVITY%NALE(N) /= 0) MS0(N) = MS(N)
       ENDDO
       IF(ALEFVM_Param%IEnabled > 0)THEN
         DO N=NODFT,NODLT
           IF(ALE_CONNECTIVITY%NALE(N) /= 0) V(1:3,N) = ZERO !reset velocities
         ENDDO       
       ENDIF
      ENDIF
            
      !---------------------------------------------
      !     INTERFACE 18 - VARIABLE GAP
      !---------------------------------------------
      IF(INTER18_IS_VARIABLE_GAP_DEFINED)THEN
        NEED_COMM_INTER18 = .TRUE.
        DO NG=ITASK+1,NGROUP,NTHREAD
           IF (TT > ZERO .AND. IPARG(76, NG) == 1) CYCLE ! --> OFF
           IF(IPARG(8,NG) == 1) CYCLE
           ISOLNOD=IPARG(28,NG)
           CALL INITBUF(
     1        IPARG   ,NG      ,
     2        MTN     ,NEL     ,NFT     ,IAD      ,ITY     ,
     3        NPT     ,JALE    ,ISMSTR  ,JEUL     ,JTUR    ,
     4        JTHE    ,JLAG    ,JMULT   ,JHBE     ,JIVF    ,
     5        NVAUX   ,JPOR    ,JCVT    ,JCLOSE   ,IPLA    ,
     6        IREP    ,IINT    ,IGTYP   ,ISRAT    ,ISROT   ,
     7        ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE ,JSMS    )
           IF (ITY == 1 .AND. ISOLNOD /= 4) THEN
             DO I=1,NEL
               XCELL(1,I+NFT)=EXP(LOG(ELBUF_TAB(NG)%GBUF%VOL(I))/THREE)
               XCELL(2,I+NFT)=ZERO
               XCELL(3,I+NFT)=ZERO
             ENDDO
           ENDIF
        ENDDO
        CALL MY_BARRIER
        IF(NSPMD > 1)THEN
!$OMP SINGLE
          LENCOM = NERCVOIS(NSPMD + 1) + NESDVOIS(NSPMD + 1)
          CALL STARTIME(TIMERS, TIMER_SPMDCFD)
          CALL SPMD_ENVOIS(3, XCELL, NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
          CALL STOPTIME(TIMERS, TIMER_SPMDCFD)
!$OMP END SINGLE
        ENDIF
      ENDIF

      !--------------------------------------------------------
      ! F L U I D E
      !       ALE SUB-CYCLING PREPARATION
      !       ALE%SUB%IALESUB=1 LEGACY IMPLEMENTATION, NOT PARALLEL, DELETED IN V11
      !       ALE%SUB%IALESUB=2 PARALLEL IMPLMEMENTATION IN 2002
      !--------------------------------------------------------
      IF(ALE%SUB%IALESUB /= 0) CALL MY_BARRIER    
      !-----
      IF(ALE%SUB%IALESUB == 2)THEN
        IF(ITASK == 0)THEN
          IF(TT == ZERO .OR. TT+DT1 > T1S+DT2S)THEN
            ALE%SUB%IFSUB  = 0
            ALE%SUB%IFSUBM = 1
            ALE%SUB%DT1SAVE= DT1
            DT1 = TT-T1S
            T1S = TT
          ELSE
            ALE%SUB%IFSUB=1
            ALE%SUB%IFSUBM=0
          ENDIF
        ENDIF
        CALL MY_BARRIER
        IF(ALE%SUB%IFSUB == 0)THEN
          NELTSA  = NELTST
          ITYPTSA = ITYPTST
          DT2SAVE = DT2T
          DT2T    = EP30
        ENDIF
        CALL ALESUB1(ALE_CONNECTIVITY%NALE,V,W,DSAVE,ICODT,ISKEW,SKEW,D,ITASK,NODFT,NODLT,FSKY,FSKY)
      ENDIF
      IF(ALE%SUB%IALESUB == 0  .AND. ITASK == 0)THEN
        ALE%SUB%IFSUB = 0
        T1S = TT
      ENDIF
      !-----      
      
      IF (GLOB_THERM%ITHERM == 1 .AND. IALE+IEULER == 0) ALE%SUB%IFSUBM=0
C----------------------------------
C       ALE - EULER - THERMAL
C       WA  = flux for sforc3 and qforc3
C----------------------------------

      CALL MY_BARRIER
C
      IF (IPARIT == 0 .AND. NSPMD > 1 .AND. ALE%SUB%IFSUBM == 1 .AND. N2D == 0 .AND. ALE%GLOBAL%INCOMP == 0) THEN
        DO N = NODFT, NODLT
          MS(N) = MS(N)*WEIGHT(N)
          MSNF(N) = MSNF(N)*WEIGHT(N)
        ENDDO
        CALL MY_BARRIER
      ENDIF
C---------------------------------------------------------
C       ALE - EULER  + 2D : update masses 
C---------------------------------------------------------
      IF (N2D > 0 .AND. ALE%SUB%IFSUBM == 1) THEN
        DO N = NODFT, NODLT
          MS_2D(N) = ZERO
        ENDDO
        CALL MY_BARRIER
      ENDIF
C
      IF(ALE%SUB%IFSUB == 0)THEN
        IF(ITASK==0) CALL STARTIME(TIMERS,MACRO_TIMER_IFSUB0)
        IF(IALE+IEULER+GLOB_THERM%ITHERM /= 0)THEN
          IF(ALE%GLOBAL%INCOMP == 0) THEN
            IF(NSEGFLU > 0 .AND. N2D == 0) THEN
              CALL SEGGETV(IPARG,ELBUF_TAB,ALE_CONNECTIVITY,ITASK,SEGVAR)
              ! Comm SPMD SEGVAR Pi -> P0
              IF(NSPMD > 1) THEN
!$OMP SINGLE
                CALL STARTIME(TIMERS, TIMER_SPMDCFD)
                CALL SPMD_SEGCOM(SEGVAR,NPSEGCOM,LSEGCOM,NPSEGCOM(NSPMD+1),0)
                CALL STOPTIME(TIMERS, TIMER_SPMDCFD)
!$OMP END SINGLE
              ENDIF
            ENDIF
            !------------------------------------------------!
            ! INTERFACE CONVECTIONS                          !
            !------------------------------------------------!  
            IF(NINTER /= 0)THEN
               !element from Interface type 12 calculated on proc_0 + sending X proc_i -> proc_0
               !Sending X for interface type 12 ?
               IF (ITASK == 0 .AND. ISPMD == 0)
     .           CALL INTTI0(IPARI ,X ,V ,WA ,ITAB ,IPARG ,IXS ,SEGVAR ,SKEW ,ISKWN ,INTBUF_TAB)
               CALL MY_BARRIER
            ENDIF
            !------------------------------------------------!
            ! ELEMENTARY BCS (/EBCS)                         !
            !------------------------------------------------!
            IF(NEBCS > 0)THEN
               IF(ITASK == 0)THEN
                 I=1+NINTER+NRWALL+NRBODY+NSECT+NJOINT+NRBAG
                 CALL EBCS_MAIN(IGRSURF,SEGVAR,VOLMON,
     .                          A,V,W,X,FSAV(1,I),FV,
     .                          MS,STIFN,IPARG,
     .                          ELBUF_TAB,EBCS_TAB,MULTI_FVM,IXQ,IXS,IXTG,
     .                          FSKY,FSAVSURF,TT,DT1,
     .                          SENSORS%NSENSOR,SENSORS%SENSOR_TAB,PYTHON,
     .                          NPC, TF ,SNPC, STF, OUTPUT, MATPARAM,  PM,
     .                          BUFMAT,MCHECK)
               ENDIF
               CALL MY_BARRIER
            ENDIF
            !------------------------------------------------!              
            IF(NSEGFLU > 0) THEN
C Comm SPMD SEGVAR P0 -> Pi
               IF(NSPMD > 1) THEN
!$OMP SINGLE
                 CALL STARTIME(TIMERS, TIMER_SPMDCFD)
                 CALL SPMD_SEGCOM(SEGVAR,NPSEGCOM,LSEGCOM,NPSEGCOM(NSPMD+1),1)
                  CALL STOPTIME(TIMERS, TIMER_SPMDCFD)
C Comm SPMD P0 -> Pi time T=0 depending on EBCS type
                 IF(TT == ZERO)THEN
                    CALL SPMD_INIT_EBCS(V,ISIZXV,IAD_ELEM,FR_ELEM, EBCS_TAB)
                 ENDIF
!$OMP END SINGLE
               ENDIF
            ENDIF
c
         !==========inter22=================================================================
         !------------------------------------------------!
         ! SUPERCELLS MANAGEMENT + VELOCITY INTERPOLATION !
         !------------------------------------------------!         
         IF(INT22>0)THEN 
           NIN     = 1 !temporary (max 1 interface) !!!interface number                     
           IAD22   = IPARI(NPARI*(NIN-1)+39)                                   
           NBRIC_L = IGRBRIC(IPARI(NPARI*(NIN-1)+45))%NENTITY

           CALL MY_BARRIER

           CALL SINIT22_FVM(
     1                       IXS           , ELBUF_TAB, IPARG, ITAB  , ITASK   ,       
     2                       IBID          , NBRIC_L  , X    , ALE_CONNECTIVITY , V       ,
     3                       ale%global%nv46, VEUL     , IGRNOD, IPARI, IGRTRUSS ,
     4                       IXT           , BUFMAT   , IPM
     5                     )                
         ENDIF
         !==================================================================================   
         
         IF (MULTI_FVM%IS_USED) THEN

          IF(ITASK==0) CALL STARTIME(TIMERS,MACRO_TIMER_MULTIFVM)
C     -------------------------
C     Multifluid law variable globalization
C     LBUF -> GBUF
C     -------------------------
            IF (TT  ==  ZERO) THEN
               CALL MULTI_GLOBALIZE(ELBUF_TAB, IPARG, ITASK, MULTI_FVM,
     .              PARTSAV, IPARTS, GRESAV, IGRTH, GRTH)
               IF (MULTI_FVM%NS_DIFF) THEN
                  DO NG = ITASK + 1, NGROUP, NTHREAD
                     IF (IPARG(1, NG) == 151) THEN
                        NEL = IPARG(2, NG)
                        NFT = IPARG(3, NG)
                        ITY = IPARG(5, NG)
                        CALL MULTI_COMPUTEVOLUME(NEL, NG, IPARG, MULTI_FVM%SYM, 
     .                       ELBUF_TAB, IXS, IXQ, IXTG, MULTI_FVM%VOL(1 + NFT : NEL + NFT),X)
                     ENDIF
                  ENDDO
               ENDIF
               CALL MY_BARRIER
C     -------------------------
C     Multifluid pressure equilibration
C     -------------------------
               CALL MULTI_PRESSURE_EQUILIBRIUM(DT1, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG,
     .              PM, IPM, MULTI_FVM, TT, BUFMAT,NPC,TF,NUMMAT,MATPARAM)
C     ------------------------
C     Build FVM Connectivities
C     ------------------------
               CALL BUILD_CONNECTIVITY(ITASK, MULTI_FVM, ALE_CONNECTIVITY, IPARG)
               CALL MY_BARRIER
C     -------------------------
C     Wall boundary condition
C     -------------------------
!$OMP SINGLE
               IF(BCS%NUM_WALL > 0)THEN
                 CALL BCS_WALL_TRIGGER(TT,ALE_CONNECTIVITY,SENSORS%NSENSOR,SENSORS%SENSOR_TAB)
               ENDIF
!$OMP END SINGLE
C     ---------------------
C     Face data computation
C     ---------------------
               CALL MULTI_FACE_ELEM_DATA(ITASK, IPARG, IXS, IXQ, IXTG, X, W, MULTI_FVM)
               CALL MY_BARRIER
               IF (NSPMD  >  1 .AND. ((MULTI_FVM%MUSCL  >  0) .OR. MULTI_FVM%NS_DIFF)) THEN
!$OMP SINGLE
                  LENCOM = NERCVOIS(NSPMD + 1) + NESDVOIS(NSPMD + 1)
                  CALL STARTIME(TIMERS, TIMER_SPMDCFD)
                  CALL SPMD_ENVOIS(3, MULTI_FVM%ELEM_DATA%CENTROID,NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
                  IF (ALLOCATED(MULTI_FVM%VOL)) THEN
                     CALL SPMD_ENVOIS(1, MULTI_FVM%VOL, NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM) 
                  ENDIF
                  CALL STOPTIME(TIMERS, TIMER_SPMDCFD)
!$OMP END SINGLE
                  CALL MY_BARRIER
               ENDIF
C     -------------------------
C     Multifluid law time step computation
C     -------------------------
               CALL MULTI_COMPUTE_DT(DT2T, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG,   
     .              PM, IPM, MULTI_FVM, W, X,
     .              NELTST, ITYPTST)  
C     -------------------------
C     Internal forces and nodal mass computation
C     -------------------------
               IF(N2D == 0)THEN
                  DO I=NODFT,NODLT       
                     MS(I)=MS(I)-MSNF(I)  
                     MSNF(I)=ZERO         
                  ENDDO                  
               ENDIF
               CALL MY_BARRIER
               IF (IALE  >  0) THEN
                  CALL MULTI_FVM2FEM(DT1, ELBUF_TAB, ITASK, 
     .                 IXS, IXQ, IPARG, X, A, V, W, MS, MSNF, VEUL, 
     .                 STIFN, FSKY, IADS, FSKYM, 
     .                 CONDN, CONDNSKY, MULTI_FVM,GLOB_THERM%NODADT_THERM)
               ENDIF
               CALL MY_BARRIER
!$OMP SINGLE
                  MULTI_FVM%IS_RESTART = .FALSE.
!$OMP END SINGLE     
            ELSE
               IF (MULTI_FVM%IS_RESTART) THEN
C     -------------------------
C     Multifluid law variable globalization
C     LBUF -> GBUF
C     -------------------------
                  CALL MULTI_BUF2VAR(ITASK, IPARG, MULTI_FVM, ELBUF_TAB)
                  CALL MY_BARRIER
C     -------------------------
C     Multifluid pressure equilibration
C     -------------------------
               CALL MULTI_PRESSURE_EQUILIBRIUM(DT1, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG,
     .              PM, IPM, MULTI_FVM, TT, BUFMAT,NPC,TF,NUMMAT,MATPARAM)
C     ------------------------
C     Build FVM Connectivities
C     ------------------------
               CALL BUILD_CONNECTIVITY(ITASK, MULTI_FVM, ALE_CONNECTIVITY, IPARG)
               CALL MY_BARRIER

               CALL MULTI_FACE_ELEM_DATA(ITASK, IPARG, IXS, IXQ, IXTG, X, W, MULTI_FVM)
               CALL MY_BARRIER
               IF (NSPMD  >  1 .AND. MULTI_FVM%MUSCL  >  0) THEN
!$OMP SINGLE
                  LENCOM = NERCVOIS(NSPMD + 1) + NESDVOIS(NSPMD + 1)
                  CALL STARTIME(TIMERS,TIMER_SPMDCFD)
                  CALL SPMD_ENVOIS(3, MULTI_FVM%ELEM_DATA%CENTROID, 
     .                 NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
                  CALL STOPTIME(TIMERS,TIMER_SPMDCFD)
!$OMP END SINGLE
                     CALL MY_BARRIER
                  ENDIF
!$OMP SINGLE
                  MULTI_FVM%IS_RESTART = .FALSE.
!$OMP END SINGLE                  
               ENDIF
C     -------------------------
C     Gravity terms
C     -------------------------
               CALL ALEFVM_GRAV_INIT(PYTHON, 
     1              AGRAV, IGRV    , LGRAV, SENSORS%NSENSOR,SENSORS%SENSOR_TAB,
     2              ITASK, NPC     , TF    , SKEW  )

C     -------------------------
C     Wall boundary condition
C     -------------------------
!$OMP SINGLE
               IF(BCS%NUM_WALL > 0)THEN
                 CALL BCS_WALL_TRIGGER(TT,ALE_CONNECTIVITY,SENSORS%NSENSOR,SENSORS%SENSOR_TAB)
               ENDIF
!$OMP END SINGLE

               CALL MY_BARRIER

               IF (MULTI_FVM%MUSCL  ==  1) THEN
C     --------------------------------------------------
C     Multifluid law time evolution --> Prediction phase
C     Half time step evolution
C     --------------------------------------------------     
                  PRED = .TRUE.

                  CALL MULTI_TIMEEVOLUTION(TIMERS,HALF * DT1, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG, 
     .                 PM, IPM, MULTI_FVM, ALE_CONNECTIVITY, V, A, W, X, D, ALE_CONNECTIVITY%NALE,
     .                 PARTSAV, IPARTS, GRESAV, IGRTH, GRTH, 
     .                 NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, 
     .                 ITAB, ITABM1, TT - DT1, 
     .                 STIFN, FSKY, IADS, FSKYM, 
     .                 CONDN, CONDNSKY, BUFMAT, FV, PRED,ID_GLOBAL_VOIS,FACE_VOIS,EBCS_TAB,NPC,TF,FSAVSURF,MATPARAM,
     .                 OUTPUT%TH%WFEXT, OUTPUT)

                  CALL MY_BARRIER
C     -------------------------
C     Multifluid pressure computation
C     -------------------------
                  CALL MULTI_PRESSURE_EQUILIBRIUM(DT1, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG,
     .                 PM, IPM, MULTI_FVM, TT - DT1, BUFMAT,NPC,TF,NUMMAT,MATPARAM)
                  CALL MY_BARRIER  
C     --------------------------------------------------
C     Multifluid law time evolution --> Correction phase
C     Full time step evolution, with predicted fluxes
C     --------------------------------------------------     
                  PRED = .FALSE.

                  CALL MULTI_TIMEEVOLUTION(TIMERS,DT1, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG, 
     .                 PM, IPM, MULTI_FVM, ALE_CONNECTIVITY, V, A, W, X, D, ALE_CONNECTIVITY%NALE,
     .                 PARTSAV, IPARTS, GRESAV, IGRTH, GRTH, 
     .                 NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, 
     .                 ITAB, ITABM1, TT - DT1, 
     .                 STIFN, FSKY, IADS, FSKYM, 
     .                 CONDN, CONDNSKY, BUFMAT, FV, PRED,ID_GLOBAL_VOIS,FACE_VOIS,EBCS_TAB,NPC,TF,FSAVSURF,MATPARAM,
     .                 OUTPUT%TH%WFEXT, OUTPUT)

                  CALL MY_BARRIER
C     -------------------------
C     Multifluid pressure equilibration
C     -------------------------
                  CALL MULTI_PRESSURE_EQUILIBRIUM(DT1, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG,
     .                 PM, IPM, MULTI_FVM, TT, BUFMAT,NPC,TF,NUMMAT,MATPARAM)
                  CALL MY_BARRIER                    
               ELSE
C     -------------------------
C     Multifluid law time evolution
C     ------------------------- 
                  CALL MULTI_TIMEEVOLUTION(TIMERS,DT1, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG, 
     .                 PM, IPM, MULTI_FVM, ALE_CONNECTIVITY, V, A, W, X, D, ALE_CONNECTIVITY%NALE,
     .                 PARTSAV, IPARTS, GRESAV, IGRTH, GRTH, 
     .                 NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, 
     .                 ITAB, ITABM1, TT - DT1, 
     .                 STIFN, FSKY, IADS, FSKYM, 
     .                 CONDN, CONDNSKY, BUFMAT, FV, .FALSE.,ID_GLOBAL_VOIS,FACE_VOIS,EBCS_TAB,NPC,TF,FSAVSURF,MATPARAM,
     .                 OUTPUT%TH%WFEXT, OUTPUT)
                  CALL MY_BARRIER

                  IF (MULTI_FVM%NS_DIFF) THEN
!$OMP SINGLE
                     CALL NS_FVM_DIFFUSION(ALE_CONNECTIVITY, MULTI_FVM, DT1, EBCS_TAB, DIFFUSION, 
     .                    IPM, PM, IPARG, ELBUF_TAB, NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, 
     .                    IXS, FV)
!$OMP END SINGLE
                  ENDIF
C     -------------------------
C     Multifluid pressure equilibration
C     -------------------------
                  CALL MULTI_PRESSURE_EQUILIBRIUM(DT1, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG,
     .                 PM, IPM, MULTI_FVM, TT, BUFMAT,NPC,TF,NUMMAT,MATPARAM)
                  CALL MY_BARRIER   
               ENDIF

C     -------------------------
C     Save old values into ELBUF_TAB struct
C     -------------------------
               CALL MULTI_VAR2BUF(ITASK, IPARG, MULTI_FVM, ELBUF_TAB)
               CALL MY_BARRIER
C     -------------------------
C     Internal forces and nodal mass computation
C     -------------------------
               IF(N2D == 0)THEN
                  DO I=NODFT,NODLT       
                     MS(I)=MS(I)-MSNF(I)  
                     MSNF(I)=ZERO         
                  ENDDO                  
               ENDIF
               CALL MY_BARRIER
               IF (IALE  >  0) THEN
                  CALL MULTI_FVM2FEM(DT1, ELBUF_TAB, ITASK, 
     .                  IXS, IXQ, IPARG, X, A, V, W, MS, MSNF, VEUL, 
     .                  STIFN, FSKY, IADS, FSKYM, 
     .                  CONDN, CONDNSKY, MULTI_FVM,GLOB_THERM%NODADT_THERM)
                  CALL MY_BARRIER
               ENDIF
C     -------------------------
C     Multifluid law time step computation
C     -------------------------
               CALL MULTI_COMPUTE_DT(DT2T, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG, PM, IPM, MULTI_FVM, W, X, NELTST, ITYPTST)  
               CALL MY_BARRIER
            ENDIF
            IF(ITASK==0) CALL STOPTIME(TIMERS,MACRO_TIMER_MULTIFVM)

         ELSE

!------------------------------------------------!
! CONVECTION, CONDUCTION, REZONING               !
!------------------------------------------------!
            call my_barrier()
!$omp do
            do j=1,2*ale%global%nv46
              do ijk=1,ale%global%s_qmv
                qmv(ijk,j) = zero
              enddo
            enddo
!$omp end do                       
            call my_barrier()

            CALL ALETHE(
     1   IPARG,           ELBUF_TAB,       WA,              VAL2,
     2   PHI,             ALE_CONNECTIVITY,IXS,             IXQ,
     3   V,               W,               X,               PM,
     4   MS,              VEUL,            FILL,            DFILL,
     5   ALPH,            FV,              BUFMAT,          TF,
     6   NPC,             ITASK,           NBRCVOIS,        NBSDVOIS,
     7   LNRCVOIS,        LNSDVOIS,        NERCVOIS,        NESDVOIS,
     8   LERCVOIS,        LESDVOIS,        SEGVAR,
     9   MSNF,            NODFT,           NODLT,           WA(IADBH),
     A   IPM,             QMV,
     B   ale%global%s_flux,ale%global%s_qmv,ale%global%nv46,
     C   IAD_ELEM,        GLOB_THERM,
     D   FR_ELEM,         MATPARAM  ,timers,nthread,int22)
         ENDIF
      ELSE
        ALE%SUB%IFSUBM=0
      ENDIF
C
      CALL MY_BARRIER
C
         !---------------------------------------------
         !     MULTIMATERIAL - LAW20(BIMAT)
         !---------------------------------------------
          IF(NMULT > 0)THEN
            DO N=NODFT,NODLT
              IMS(N)=0
              DO M=1,NMULT
                IFILL(N,M)=-2
                DFILL(N,M)=ZERO
              ENDDO
            ENDDO
          ENDIF

         !---------------------------------------------
         !     ALEFVM : FINITE VOLUME FOR MOMENTUM
         !---------------------------------------------          
          IF(ALEFVM_Param%IEnabled > 0) THEN
            !GRAVITY INIT
            ! VERTEX(4,1:NUMNOD) = ZERO  !-> not needed, nodes remain the same ones and VERTEX is initialized on allocation
            CALL ALEFVM_GRAV_INIT(PYTHON, 
     1                           AGRAV, IGRV    , LGRAV, SENSORS%NSENSOR,SENSORS%SENSOR_TAB,
     2                            ITASK   , NPC  , TF    , SKEW   )
          ENDIF       
          

          CALL MY_BARRIER  !MM-ALE 20 + ALEFVM
          
         !---------------------------------------------
         ! TETRA4 : SMOOTH FINITE ELEMENT FORMULATIONS
         !---------------------------------------------
!$OMP SINGLE
          IF(ISFEM >= 1) THEN
            CALL S4ALESFEM(IPARG,IXS,X,ELBUF_TAB,SFEM_NODVAR,S_SFEM_NODVAR,PM,IAD_ELEM,FR_ELEM)
          ENDIF
!$OMP END SINGLE

         !---------------------------------------------
         ! /ALE/GRID/FLOW-TRACKING - INIT BUFFER
         !---------------------------------------------
          IF(ALE%GRID%NWALE == 7) THEN
!$OMP SINGLE
             ALE%GRID%flow_tracking_data%EP(1:9)=ZERO
             ALE%GRID%flow_tracking_data%SUM_M = ZERO
             ALE%GRID%flow_tracking_data%SUM_VOL = ZERO
             ALE%GRID%flow_tracking_data%NUM_ELEM_ALE = 0
!$OMP END SINGLE
          ENDIF

         !---------------------------------------------
         ! ALL ELEMENTS EXPECT LAW/=11
         !---------------------------------------------
          if(itask==0) call startime(timers,timer_ale_elm)
          ! ----------------------------          
          DO NG=ITASK+1,NGROUP,NTHREAD
C     ALE ON / OFF
            SENSORS%NGR_SENSOR(ITASK+1) = NG
            IF (TT  >  ZERO .AND. IPARG(76, NG)  ==  1) CYCLE ! --> OFF
            IF(IPARG(8,NG) /= 1) THEN
              if (iddw>0) call startimeg(ng) ! activation of internal timer for this element group
C
              CALL INITBUF(IPARG    ,NG      ,
     2          MTN     ,NEL     ,NFT     ,IAD     ,ITY     ,
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,IPLA    ,
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )

C
              IF (MTN  ==  151) CYCLE
              IF(JLAG /= 1 .AND. ITY <= 2) THEN
                IF(IPARG(64,NG)==1) ILAW11=1                      !place later in starter>priv>spmd>ddsplit>c_vois() for law51
                IF (MTN /= 0  .AND.  IPARG(64,NG)==0) THEN     !IPARG(64) : law11 & law51-outlet
                  NSG    =IPARG(10,NG)
                  NVC    =IPARG(19,NG)
                  ISOLNOD=IPARG(28,NG)
                  ISTRA  =IPARG(44,NG)
                  JSPH   =0
                  JPLASOL=IPLA
                  ISPH2SOL = 0
                  IPARTSPH = IPARG(69,NG)
                  LFT=1
                  LLT=NEL
                  NF1=NFT+1
C
                  IF (ITY == 1 .AND. ISOLNOD == 4) THEN
                    CALL S4FORC3(TIMERS,  OUTPUT,
     1                  ELBUF_TAB,       NG,              PM,              GEO,
     2                  IXS,             X,               A,               V,
     3                  MS,              W,               WA,              VAL2,
     4                  VEUL,            FV,              ALE_CONNECTIVITY,IPARG,
     5                  TF,              NPC,             BUFMAT,          PARTSAV,
     6                  NLOC_DMG,        DT2T,            NELTST,          ITYPTST,
     7                  STIFN,           FSKY,            IADS,            OFFSET,
     8                  EANI,            IPARTS(NF1),     FX(1,1),         FY(1,1),
     9                  FZ(1,1),         FX(1,2),         FY(1,2),         FZ(1,2),
     A                  FX(1,3),         FY(1,3),         FZ(1,3),         FX(1,4),
     B                  FY(1,4),         FZ(1,4),         NEL,             FSKYM,
     C                  MSNF,            IPM,             IGEO,            BUFVOIS,
     D                  ISTRA,           ITASK,           BID,             BID,
     E                  BID,             IBID,            GRESAV,          GRTH,
     F                  IGRTH,           MSSA,            DMELS,           TABLE,
     G                  XDP,             SFEM_NODVAR,     VOLN,            BID,
     H                  BID,             D,               SENSORS,         IOUTPRT,
     I                  MAT_ELEM,        IBID,            DT,              IDEL7NOK,
     J                  NSVOIS,          SZ_BUFVOIS,      SNPC,            STF,
     K                  SBUFMAT,         SVIS,            IDTMINS,         IRESP, 
     *                  IDEL7NG,         MAXFUNC,         USERL_AVAIL,     GLOB_THERM, 
     V                  impl_s,          idyna,           S_SFEM_NODVAR,ale%global%s_flux,ale%global%nv46)
C
                  ELSEIF (ITY == 1 .AND. ISOLNOD /= 4) THEN
                    NULLIFY(pQMV); 
                    IF(TRIMAT>0)THEN
                      pQMV => QMV(1,1)
                    ELSE
                      pQMV => BID
                    ENDIF
                    CALL SFORC3(TIMERS,OUTPUT,
     1                  ELBUF_TAB,             NG,      PM,            GEO,
     2                  IXS,                   X,       ale%global%nv46,          A,
     3                  V,                     MS,      W,             WA,
     4                  VAL2,                  VEUL,    FV,            ALE_CONNECTIVITY,
     5                  IPARG,                 TF,      NPC,           BUFMAT,
     6                  PARTSAV,               ITAB,    DT2T,          NELTST,
     7                  ITYPTST,               STIFN,   FSKY,          IADS,
     8                  OFFSET,                EANI,    IPARTS(1+NFT), FX(1,1),
     9                  FY(1,1),               FZ(1,1), FX(1,2),       FY(1,2),
     A                  FZ(1,2),               FX(1,3), FY(1,3),       FZ(1,3),
     B                  FX(1,4),               FY(1,4), FZ(1,4),       FX(1,5),
     C                  FY(1,5),               FZ(1,5), FX(1,6),       FY(1,6),
     D                  FZ(1,6),               FX(1,7), FY(1,7),       FZ(1,7),
     E                  FX(1,8),               FY(1,8), FZ(1,8),       NEL,
     F                  FSKYM,                 MSNF,    ISKY,          FSKYI,
     G                  NVC,                   IPM,     IGEO,          BID,
     H                  BID,                   BID,     BID,           BID,
     I                  BUFVOIS,               ITASK,   pQMV,          ISTRA,
     J                  TEMP,                  BID,     BID,           IBID,
     K                  GRESAV,                GRTH,    IGRTH,         MSSA,
     L                  DMELS,                 TABLE,   BID,           BID,
     M                  BID,                   BID,     BID,           BID,
     N                  BID,                   BID,     BID,           IPARG(1,NG),
     O                  XDP,                   BID,     IBID,          IBID,
     P                  VOLN,                  CONDN,   CONDNSKY,      AGRAV,
     Q                  IGRV,                  LGRAV,   SENSORS,       SKEW,
     R                  ALE_CONNECTIVITY%NALE, D,       IOUTPRT,       NLOC_DMG,
     S                  MAT_ELEM,              IBID,    DT,            IDEL7NOK,NSVOIS,
     T                  SZ_BUFVOIS,            SNPC,    STF,           SBUFMAT,SVIS,IDTMINS,IRESP,
     U                  IDEL7NG,               MAXFUNC, USERL_AVAIL,   GLOB_THERM,
     V                  impl_s,                idyna,   OUTPUT%TH%WFEXT,ale%global%s_flux,ale%global%s_qmv)

          !write(*,*) "VALEUR ? ",ncycle_debug,wa(1:6),qmv(1,1:12),ale%global%s_flux,ale%global%s_qmv,ale%global%nv46
C
                  ELSEIF (ITY == 2 .AND. JMULT == 0) THEN
                    NULLIFY(pQMV); 
                    IF(TRIMAT>0)THEN
                      pQMV => QMV(1,1)
                    ELSE
                      pQMV => BID
                    ENDIF   
                    CALL QFORC2(TIMERS, OUTPUT,
     .                  ELBUF_TAB       ,NG               ,
     1                  PM              ,GEO              ,IXQ         ,X         ,A        ,  
     2                  V               ,MS               ,W           ,WA        ,VAL2     ,  
     3                  VEUL            ,ALE_CONNECTIVITY ,IPARG       ,NLOC_DMG  ,
     4                  TF              ,NPC              ,BUFMAT      ,PARTSAV   ,
     5                  DT2T            ,NELTST           ,ITYPTST     ,STIFN     ,OFFSET   ,  
     6                  EANI            ,IPARTQ(1+NFT)    ,NEL         ,IADQ      ,FSKY     ,  
     9                  IPM             ,BUFVOIS          ,pQMV        ,
     A                  GRESAV          ,GRTH             ,IGRTH       ,TABLE     ,IGEO     ,
     B                  VOLN            ,ITASK            ,MS_2D       ,FSKYM     ,IOUTPRT  ,
     C                  MAT_ELEM        ,H3D_DATA%STRAIN  ,SZ_BUFVOIS  ,SNPC      ,STF      ,SBUFMAT  ,
     D                  SVIS            ,NSVOIS           ,IRESP       ,TT        ,DT1      ,      
     .                  IDEL7NOK        ,IDTMIN           ,MAXFUNC     ,  
     .                  IMON_MAT        ,USERL_AVAIL      ,impl_s    ,idyna    ,
     .                  DT              ,GLOB_THERM       ,SENSORS,ale%global%s_flux,ale%global%s_qmv,ale%global%nv46)
c
                  ELSEIF (ITY == 2 .AND. JMULT /= 0) THEN
                    CALL BFORC2(TIMERS,
     .                  ELBUF_TAB   , NG            ,
     1                  PM          , GEO           ,IXQ             , X         ,
     2                  A           , V             ,MS              , W         , WA        ,
     3                  VAL2        , VEUL          ,ALE_CONNECTIVITY, IPARG     ,
     4                  IPARG(1,NG) , FILL          ,DFILL           , IMS       , NLOC_DMG  ,
     5                  TF          , NPC           ,BUFMAT          , PARTSAV   ,
     6                  DT2T        , NELTST        ,ITYPTST         , STIFN     , OFFSET    ,
     7                  EANI        , IPARTQ(1+NFT) ,NEL             , IADQ      , FSKY      ,
     8                  IPM         , BUFVOIS       ,
     9                  GRESAV      , GRTH          ,IGRTH           , TABLE     , IGEO      ,
     O                  VOLN        , ITASK         ,MS_2D           , FSKYM     , MAT_ELEM  ,
     B                  IBID        , OUTPUT        ,SZ_BUFVOIS      , SNPC      , STF       ,SBUFMAT, SVIS,
     C                  NSVOIS      , IRESP         ,IDEL7NOK        , 
     D                  IDTMIN      , MAXFUNC       ,IMON_MAT        , 
     E                  USERL_AVAIL , impl_s         , idyna     ,DT         ,
     F                  GLOB_THERM  , SENSORS,ale%global%s_flux,ale%global%nv46)
                  ENDIF
                ENDIF
              ENDIF
              if (iddw>0) call stoptimeg(ng) ! deactivation of internal timer for this element group
            ENDIF
          ENDDO
          ! ----------------------------
          if(itask==0) call stoptime(timers,timer_ale_elm)          
C
         !---------------------------------------------
         ! ELEMENTS : LAW==11
         !---------------------------------------------
          CALL MY_BARRIER !wait for adjacent elems
          IF (ILAW11 /= 0) THEN            !Law11 or Law51 in/Out
            IF(NSPMD > 1) THEN
             ! SPMD COMMUNICATION VOISINS LOI11
             !
             ! WA[1..IADC-1] : FLUX
             ! WA[IADC..IADD-1] : BUFVOIS (buffer elements voisins en SPMD)
             ! WA[IADD..LENWA] : BUFCOM (wa pour comm spmd)
!$OMP SINGLE
              CALL STARTIME(TIMERS,TIMER_SPMDCFD)
              LENCOM=NBRCVOIS(NSPMD+1)+NBSDVOIS(NSPMD+1)
              IF(M51_IFLG6 == 0) THEN
              !======LAW11-NRF/SPMD=====================================!
                 ! comm adjacent domains
                 CALL SPMD_XVOIS(
     1              V       ,NBRCVOIS,NBSDVOIS,LNRCVOIS,LNSDVOIS,
     2              LENCOM  )
                 ! to be optimized depending on combination of options
                 IF(ALE%GLOBAL%INCOMP == 1 .OR. ITURB + GLOB_THERM%ITHERM == 0)
     .             CALL SPMD_XVOIS(X,NBRCVOIS,NBSDVOIS,LNRCVOIS,LNSDVOIS,LENCOM  )
                   ! Comm adjacent elems
                 CALL SPMD_L11VOIS(
     1                 BUFVOIS ,IPARG   ,ELBUF_TAB,PM      ,IXS     ,
     2                 IXQ     ,NERCVOIS,NESDVOIS,LERCVOIS,LESDVOIS,
     3                 LENCOM  )
              ELSE
              !======LAW51-NRF/SPMD=====================================!
                 CALL SPMD_L51VOIS(
     1                 BUFVOIS ,IPARG   ,ELBUF_TAB,PM      ,IXS     ,
     2                 IXQ     ,NERCVOIS,NESDVOIS,LERCVOIS,LESDVOIS,
     3                 LENCOM  ,IPM     ,BUFMAT)
              ENDIF
             CALL STOPTIME(TIMERS,TIMER_SPMDCFD)
!$OMP END SINGLE
            ENDIF

            if(itask==0) call startime(timers,timer_ale_elm)
            ! ----------------------------
            DO NG=ITASK+1,NGROUP,NTHREAD
C     ALE ON / OFF
              SENSORS%NGR_SENSOR(ITASK+1) = NG
              IF (TT  >  ZERO .AND. IPARG(76, NG)  ==  1) CYCLE ! --> OFF
              CALL INITBUF(IPARG    ,NG      ,
     2              MTN     ,NEL     ,NFT     ,IAD     ,ITY     ,   
     3              NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,   
     4              JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,   
     5              NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,IPLA    ,   
     6              IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,   
     7              ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )   
              IF (MTN  ==  151) CYCLE
              ! ------------------------
              IF (IPARG(8,NG) /= 1) THEN
                if (iddw>0) call startimeg(ng) ! activation of internal timer for this element group
                IF (JLAG /= 1 .AND. ITY <= 2) THEN
                  IF (MTN == 11 .OR. IPARG(64,NG) == 1) THEN
                    NSG    =IPARG(10,NG)
                    NVC    =IPARG(19,NG)
                    ISOLNOD=IPARG(28,NG)
                    ISTRA   = IPARG(44,NG)
                    JSPH   =0
                    ISPH2SOL = 0
                    IPARTSPH = IPARG(69,NG)

                    LFT=1
                    LLT=NEL
                    NF1=NFT+1

                    IF(ITY == 1 .AND. ISOLNOD == 4)THEN
                      CALL S4FORC3(TIMERS,  OUTPUT,
     1                  ELBUF_TAB,       NG,              PM,              GEO,
     2                  IXS,             X,               A,               V,
     3                  MS,              W,               WA,              VAL2,
     4                  VEUL,            FV,              ALE_CONNECTIVITY,IPARG,
     5                  TF,              NPC,             BUFMAT,          PARTSAV,
     6                  NLOC_DMG,        DT2T,            NELTST,          ITYPTST,
     7                  STIFN,           FSKY,            IADS,            OFFSET,
     8                  EANI,            IPARTS(NF1),     FX(1,1),         FY(1,1),
     9                  FZ(1,1),         FX(1,2),         FY(1,2),         FZ(1,2),
     A                  FX(1,3),         FY(1,3),         FZ(1,3),         FX(1,4),
     B                  FY(1,4),         FZ(1,4),         NEL,             FSKYM,
     C                  MSNF,            IPM,             IGEO,            BUFVOIS,
     D                  ISTRA,           ITASK,           BID,             BID,
     E                  BID,             IBID,            GRESAV,          GRTH,
     F                  IGRTH,           MSSA,            DMELS,           TABLE,
     G                  XDP,             SFEM_NODVAR,     VOLN,            BID,
     H                  BID,             D,               SENSORS,         IOUTPRT,
     I                  MAT_ELEM,        IBID,            DT,              IDEL7NOK,
     J                  NSVOIS,          SZ_BUFVOIS,      SNPC,            STF,
     K                  SBUFMAT,         SVIS,            IDTMINS,         IRESP, 
     *                  IDEL7NG,         MAXFUNC,         USERL_AVAIL,     GLOB_THERM, 
     V                  impl_s,          idyna,           S_SFEM_NODVAR,ale%global%s_flux,ale%global%nv46)

                    ELSEIF(ITY == 1 .AND. ISOLNOD /= 4)THEN
                      NULLIFY(pQMV); 
                      IF(TRIMAT>0)THEN
                        pQMV => QMV(1,1)
                      ELSE
                        pQMV => BID
                      ENDIF
                      CALL SFORC3(TIMERS,     OUTPUT,
     1                  ELBUF_TAB,            NG,       PM,            GEO,
     2                  IXS,                  X,        ale%global%nv46,          A,
     3                  V,                    MS,       W,             WA,
     4                  VAL2,                 VEUL,     FV,            ALE_CONNECTIVITY,
     5                  IPARG,                TF,       NPC,           BUFMAT,
     6                  PARTSAV,              ITAB,     DT2T,          NELTST,
     7                  ITYPTST,              STIFN,    FSKY,          IADS,
     8                  OFFSET,               EANI,     IPARTS(1+NFT), FX(1,1),
     9                  FY(1,1),              FZ(1,1),  FX(1,2),       FY(1,2),
     A                  FZ(1,2),              FX(1,3),  FY(1,3),       FZ(1,3),
     B                  FX(1,4),              FY(1,4),  FZ(1,4),       FX(1,5),
     C                  FY(1,5),              FZ(1,5),  FX(1,6),       FY(1,6),
     D                  FZ(1,6),              FX(1,7),  FY(1,7),       FZ(1,7),
     E                  FX(1,8),              FY(1,8),  FZ(1,8),       NEL,
     F                  FSKYM,                MSNF,     ISKY,          FSKYI,
     G                  NVC,                  IPM,      IGEO,          BID,
     H                  BID,                  BID,      BID,           BID,
     I                  BUFVOIS,              ITASK,    pQMV,          ISTRA,
     J                  TEMP,                 BID,      BID,           IBID,
     K                  GRESAV,               GRTH,     IGRTH,         MSSA,
     L                  DMELS,                TABLE,    BID,           BID,
     M                  BID,                  BID,      BID,           BID,
     N                  BID,                  BID,      BID,           IPARG(1,NG),
     O                  XDP,                  BID,      IBID,          IBID,
     P                  VOLN,                 CONDN,    CONDNSKY,      AGRAV,
     Q                  IGRV,                 LGRAV,    SENSORS,       SKEW,
     R                  ALE_CONNECTIVITY%NALE,D,        IOUTPRT,       NLOC_DMG,
     S                  MAT_ELEM,             IBID,     DT,            IDEL7NOK,NSVOIS,
     T                  SZ_BUFVOIS,           SNPC,     STF,SBUFMAT,SVIS,IDTMINS,IRESP,
     U                  IDEL7NG,              MAXFUNC,  USERL_AVAIL  ,GLOB_THERM,
     V                  impl_s,               idyna,    OUTPUT%TH%WFEXT,ale%global%s_flux,ale%global%s_qmv)
               
                    ELSEIF (ITY == 2 .AND. JMULT == 0) THEN
                      NULLIFY(pQMV); 
                      IF(TRIMAT>0)THEN
                        pQMV => QMV(1,1)
                      ELSE
                        pQMV => BID
                      ENDIF  
                      CALL QFORC2(TIMERS, OUTPUT,
     .                            ELBUF_TAB ,NG    ,
     1                            PM        ,GEO              ,IXQ         ,X        ,A         ,  
     2                            V         ,MS               ,W           ,WA       ,VAL2      ,      
     3                            VEUL      ,ALE_CONNECTIVITY ,IPARG       ,NLOC_DMG ,              
     4                            TF        ,NPC              ,BUFMAT      ,PARTSAV  ,              
     5                            DT2T      ,NELTST           ,ITYPTST     ,STIFN    ,OFFSET    ,
     6                            EANI      ,IPARTQ(1+NFT)    ,NEL         ,IADQ     ,FSKY      ,
     9                            IPM       ,BUFVOIS          ,pQMV        ,
     A                            GRESAV    ,GRTH             ,IGRTH       ,TABLE    ,IGEO      ,
     B                            VOLN      ,ITASK            ,MS_2D       ,FSKYM    ,IOUTPRT   ,
     C                            MAT_ELEM  ,H3D_DATA%STRAIN  ,SZ_BUFVOIS  ,SNPC     ,STF       ,SBUFMAT,
     D                            SVIS      ,NSVOIS           ,IRESP       ,TT       ,DT1       ,      
     .                            IDEL7NOK  ,IDTMIN           ,MAXFUNC     ,  
     .                            IMON_MAT  ,USERL_AVAIL      ,impl_s   ,idyna     ,
     .                            DT        ,GLOB_THERM       ,SENSORS,ale%global%s_flux,ale%global%s_qmv,ale%global%nv46)
c
                    ELSEIF(ITY == 2 .AND. JMULT /= 0)THEN
                      CALL BFORC2(TIMERS, ELBUF_TAB  ,NG           ,
     1                            PM         ,GEO          ,IXQ              ,X          ,    
     2                            A          ,V            ,MS               ,W          ,WA        ,  
     3                            VAL2       ,VEUL         ,ALE_CONNECTIVITY ,IPARG      ,
     4                            IPARG(1,NG),FILL         ,DFILL            ,IMS        ,NLOC_DMG  ,    
     5                            TF         ,NPC          ,BUFMAT           ,PARTSAV    ,       
     5                            DT2T       ,NELTST       ,ITYPTST          ,STIFN      ,OFFSET    ,  
     6                            EANI       ,IPARTQ(1+NFT),NEL              ,IADQ       ,FSKY      ,  
     7                            IPM        ,BUFVOIS      ,
     8                            GRESAV     ,GRTH         ,IGRTH            ,TABLE      ,IGEO      ,
     9                            VOLN       ,ITASK        ,MS_2D            ,FSKYM      ,MAT_ELEM,
     A                            IBID       ,OUTPUT       ,SZ_BUFVOIS       ,SNPC       ,STF       ,SBUFMAT ,SVIS,
     B                            NSVOIS     ,IRESP        ,IDEL7NOK         ,
     C                            IDTMIN     ,MAXFUNC      ,IMON_MAT         ,
     E                            USERL_AVAIL,impl_s           ,idyna      ,DT        ,
     F                            GLOB_THERM ,SENSORS,ale%global%s_flux,ale%global%nv46)
                    ENDIF
                  ENDIF
                ENDIF
                if (iddw>0) call stoptimeg(ng) ! deactivation of internal timer for this element group
              ENDIF
              ! ------------------------
            ENDDO!next NG
            ! ----------------------------            
            if(itask==0) call stoptime(timers,timer_ale_elm)
          ENDIF!IF (ILAW11 /= 0) THEN

        ENDIF !IF(IALE+IEULER+ITHERM /= 0)THEN
        IF(ITASK==0) CALL STOPTIME(TIMERS,MACRO_TIMER_IFSUB0)
      ENDIF!IF(IFSUB == 0)THEN

c      !Fluid mass do not contribute in FEM : add external force for FSI
c      IF(IALE+IEULER /= 0)THEN
c        DO N=NODFT,NODLT
c          MSNF(N) = ZERO
c        ENDDO
c      ENDIF

      !-----------------------------!
      !     PRESSURE GAUGE          !
      !-----------------------------!
      IF (NBGAUGE > 0) THEN
      IF (NSPMD  > 1) THEN
        LENCOM =NERCVOIS(NSPMD+1)+NESDVOIS(NSPMD+1)
      END IF
      CALL AGAUGE(
     1          IPARG   ,ELBUF_TAB  ,PHI    ,IXS     ,IXQ     ,
     2          X       ,ALE_CONNECTIVITY,ITASK  ,NERCVOIS,NESDVOIS,
     3          LERCVOIS,LESDVOIS   ,LENCOM ,LGAUGE  ,
     4          GAUGE   ,V          ,IGAUP  ,NGAUP   ,IXTG)
      ENDIF ! IF (NBGAUGE > 0)

      !--------------------------------------------------------
      !       ALE SUB-CYCLING SUITE
      !--------------------------------------------------------
      IF(ALE%SUB%IALESUB == 2)THEN
        !ifsub=0 fluid calculation
        !ifsub=2 fluid calculation + assembly
        !ifsub=1 no fluid calculation
        IF(ITASK == 0) THEN
          IF(ALE%SUB%IFSUB == 0) THEN
            ALE%SUB%IFSUB=2
          ENDIF
        ENDIF
        CALL MY_BARRIER
        IF(ALE%SUB%IFSUB == 1)THEN
          ! getting A from ASAVE
          ! AGLOB is pointing on A (no SMP shift))
          DO N=NODFT,NODLT
            AGLOB(1,N)=ASAVE(1,N)
            AGLOB(2,N)=ASAVE(2,N)
            AGLOB(3,N)=ASAVE(3,N)
          ENDDO
        ENDIF
      ENDIF




      CALL MY_BARRIER
      if(itask==0) deallocate(qmv)
      IF(ITASK==0)DEALLOCATE(BUFVOIS)


      RETURN
      END
