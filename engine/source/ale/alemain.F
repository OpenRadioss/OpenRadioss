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
Chd|  ALEMAIN                       source/ale/alemain.F          
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ALEFVM_GRAV_INIT              source/ale/alefvm/alefvm_grav_init.F
Chd|        ALESUB1                       source/ale/subcycling/alesub1.F
Chd|        ALETHE                        source/ale/alethe.F           
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|        BUILD_CONNECTIVITY            source/multifluid/connectivity.F
Chd|        EBCS_MAIN                     source/boundary_conditions/ebcs/ebcs_main.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        INTTI0                        source/interfaces/interf/intti0.F
Chd|        MULTI_BUF2VAR                 source/multifluid/multi_buf2var.F
Chd|        MULTI_COMPUTEVOLUME           source/multifluid/multi_computevolume.F
Chd|        MULTI_COMPUTE_DT              source/multifluid/multi_compute_dt.F
Chd|        MULTI_FACE_ELEM_DATA          source/multifluid/multi_face_data_elem.F
Chd|        MULTI_FVM2FEM                 source/multifluid/multi_fvm2fem.F
Chd|        MULTI_GLOBALIZE               source/multifluid/multi_globalize.F
Chd|        MULTI_PRESSURE_EQUILIBRIUM    source/multifluid/multi_pressure_equilibrium.F
Chd|        MULTI_TIMEEVOLUTION           source/multifluid/multi_timeevolution.F
Chd|        MULTI_VAR2BUF                 source/multifluid/multi_var2buf.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|        S4ALESFEM                     source/elements/solid/solide4_sfem/s4alesfem.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        SEGGETV                       source/interfaces/interf/seggetv.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SINIT22_FVM                   source/interfaces/int22/sinit22_fvm.F
Chd|        SPMD_ENVOIS                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_INIT_EBCS                source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_L11VOIS                  source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_L51VOIS                  source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_SEGCOM                   source/mpi/fluid/spmd_cfd.F   
Chd|        SPMD_XVOIS                    source/mpi/fluid/spmd_cfd.F   
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        ALEFVM_MOD                    ../common_source/modules/alefvm_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        DIFFUSION_MOD                 share/modules/diffusion_mod.F 
Chd|        EBCS_MOD                      ../common_source/modules/ebcs_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE ALEMAIN(
     1         PM          , GEO         ,X             ,A         ,AR        ,V               ,
     2         MS          , WA          ,ELBUF_TAB     ,BUFMAT    ,PARTSAV   ,TF              ,
     3         VAL2        , VEUL        ,FV            ,STIFN     ,FSKY      ,EANI            ,
     4         PHI         , FILL        ,DFILL         ,ALPH      ,SKEW      ,W               ,
     5         D           , DSAVE       ,ASAVE         ,DT2T      ,DT2SAVE   ,XCELL           ,
     6         IPARG       , NPC         ,IXS           ,IXQ       ,IXTG      ,IADS            ,IADQ    ,
     7         IFILL       , NODPOR      ,ICODT         ,ISKEW     ,IMS       ,
     8         NELTST      , ITYPTST     ,IPART         ,IPARTS    ,IPARTQ    ,ITASK           ,
     A         NODFT       , NODLT       ,NBRCVOIS      ,TEMP      ,
     B         NBSDVOIS    , LNRCVOIS    ,LNSDVOIS      ,NERCVOIS  ,NESDVOIS  ,LERCVOIS        ,
     C         LESDVOIS    , ISIZXV      ,IAD_ELEM      ,FR_ELEM   ,FSKYM     ,MSNF            ,
     D         IPARI       , SEGVAR      ,ITAB          ,ISKWN     ,
     E         VOLMON      , FSAV        ,IGRSURF       ,NELTSA    ,
     F         ITYPTSA     , WEIGHT      ,NPSEGCOM      ,LSEGCOM   ,IPM       ,IGEO            ,
     G         ITABM1      , LENQMV      ,NV46          ,AGLOB     ,GRESAV    ,
     H         GRTH        , IGRTH       ,LGAUGE        ,GAUGE     ,MSSA      ,
     I         DMELS       , IGAUP       ,NGAUP         ,TABLE     ,MS0       ,
     J         XDP         , IGRNOD      ,VARNOD                   ,FSKYI     ,ISKY            ,
     K         INTBUF_TAB  , IXT         ,IGRV          ,AGRAV     ,SENSOR_TAB,
     L         LGRAV       , CONDNSKY    ,CONDN         ,MS_2D     ,MULTI_FVM ,IGRTRUSS        ,
     M         IGRBRIC     , NLOC_DMG    ,ID_GLOBAL_VOIS,FACE_VOIS ,EBCS_TAB  ,ALE_CONNECTIVITY, 
     N         DIFFUSION   , MATPARAM_TAB,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE TABLE_MOD
      USE ELBUFDEF_MOD
      USE INTBUFDEF_MOD
      USE ALEFVM_MOD
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
      USE NLOCAL_REG_MOD
      USE SENSOR_MOD
      USE EBCS_MOD
      USE ALE_CONNECTIVITY_MOD
      USE DIFFUSION_MOD
      USE SEGVAR_MOD
      USE MATPARAM_DEF_MOD
      USE H3D_MOD
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
#include      "com02_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "vale_c.inc"
#include      "inter18.inc"
#include      "inter22.inc"
#include      "alefvm.inc"
#include      "mmale51_c.inc" 
#include      "scr07_c.inc"
#include      "stati_c.inc"
#include      "macro.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,NUMELS),IXQ(NIXQ,NUMELQ),IXTG(NIXTG,NUMELTG), ISKY(*),
     .   IFILL(NUMNOD,*), NPC(*), IPARG(NPARG,NGROUP),NODPOR(*),
     .   IADS(8,*),IADQ(4,*),ICODT(*),ISKEW(*), IMS(*),
     .   IGEO(NPROPGI,NUMGEO),
     .   IPART(LIPART1,*) ,IPARTS(*) ,IPARTQ(*),IPM(NPROPMI,*),NODFT,
     .   NELTST ,ITYPTST, ITASK, 
     .   NBRCVOIS(*),NBSDVOIS(*), LNRCVOIS(*), LNSDVOIS(*),
     .   NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*),
     .   NELTSA, ITYPTSA,NPSEGCOM(*),LSEGCOM(*),
     .   IAD_ELEM(*),FR_ELEM(*), IPARI(SIPARI),ITAB(NUMNOD),ISKWN(*),
     .   WEIGHT(*), ITABM1(*),
     .   ISIZXV, LENQMV,NV46,GRTH(*),IGRTH(*),LGAUGE(3,NBGAUGE),
     .   IGAUP(*),NGAUP(*),NODLT, IXT(NIXT,*),
     .   IGRV(*),LGRAV(*)
      ! for parith/on : ID_GLOBAL_VOIS --> user id ; FACE_VOIS --> face of the remote element
      INTEGER, DIMENSION(*), INTENT(in) :: ID_GLOBAL_VOIS,FACE_VOIS
     
      DOUBLE PRECISION XDP(3,*)
      
      my_real X(3,NUMNOD),V(3,NUMNOD),MS(*),PM(NPROPM,NUMMAT),SKEW(LSKEW,*),
     .        GEO(NPROPG,NGROUP),BUFMAT(*) ,W(3,NUMNOD),VEUL(*),FILL(NUMNOD,*),
     .        DFILL(NUMNOD,*),ALPH(*),TF(*),
     .        FV(*),A(3,NUMNOD),AR(3,NUMNOD),VAL2(*),PHI(*),
     .        PARTSAV(*) ,STIFN(*) ,D(3,NUMNOD),DSAVE(3,*),ASAVE(3,*),WA(*),
     .        FSKY(*),EANI(*), FSKYM(*),
     .        DT2T, DT2SAVE,
     .        AGLOB(3,*),GAUGE(LLGAUGE,*),MS0(*),
     .        MSNF(*),VOLMON(*),FSAV(NTHVKI,*),GRESAV(*),
     .        MSSA(*), DMELS(*),VARNOD(*),FSKYI(LSKYI,NFSKYI),
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
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
      TYPE(T_DIFFUSION), INTENT(INOUT) :: DIFFUSION
      TYPE(MATPARAM_STRUCT_)  , DIMENSION(NUMMAT) :: MATPARAM_TAB
      TYPE (H3D_DATABASE) :: H3D_DATA
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
      my_real, INTENT(INOUT) :: XCELL(3,SXCELL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, M, NG, NVC, NF1,OFFSET,ISOLNOD,  NSG, NEL,I, LENCOM,ISTRA, IBID,IOUTPRT
      INTEGER IPLA
      INTEGER IADBH, IAD22, NIN, NBRIC_L
      INTEGER SBUFVOIS
      
      my_real FX(MVSIZ,10),FY(MVSIZ,10),FZ(MVSIZ,10),VOLN(MVSIZ) 
      my_real, TARGET  :: BID
      my_real, DIMENSION(:,:),ALLOCATABLE,TARGET  :: QMV
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
         IF(MOD(NCYCLE,IABS(NCPRI)) == 0.OR.TT >= THIS.OR.TT >= TDES.
     .   OR.TT >= TPAT.OR.MDESS /= 0.
     .   OR.(IALESUB /= 0.AND.T1S+DT2S >= THIS).
     .   OR.TT >= THIS1(1).OR.TT >= THIS1(2).
     .   OR.TT >= THIS1(3).OR.TT >= THIS1(4).OR.TT >= THIS1(5).
     .   OR.TT >= THIS1(6).OR.TT >= THIS1(7).OR.TT >= THIS1(8).
     .   OR.TT >= THIS1(9).OR.NTH /= 0.OR.NANIM /= 0          .
     .   OR.TT >= TABFIS(1).OR.TT >= TABFIS(2).
     .   OR.TT >= TABFIS(3).OR.TT >= TABFIS(4).OR.TT >= TABFIS(5).
     .   OR.TT >= TABFIS(6).OR.TT >= TABFIS(7).OR.TT >= TABFIS(8).
     .   OR.TT >= TABFIS(9).OR.TT >= TABFIS(10).
     .   OR.(IALESUB /= 0.AND.T1S+DT2S >= THIS1(1)).
     .   OR.(IALESUB /= 0.AND.T1S+DT2S >= THIS1(2)).
     .   OR.(IALESUB /= 0.AND.T1S+DT2S >= THIS1(3)).
     .   OR.(IALESUB /= 0.AND.T1S+DT2S >= THIS1(4)).
     .   OR.(IALESUB /= 0.AND.T1S+DT2S >= THIS1(5)).
     .   OR.(IALESUB /= 0.AND.T1S+DT2S >= THIS1(6)).
     .   OR.(IALESUB /= 0.AND.T1S+DT2S >= THIS1(7)).
     .   OR.(IALESUB /= 0.AND.T1S+DT2S >= THIS1(8)).
     .   OR.(IALESUB /= 0.AND.T1S+DT2S >= THIS1(9)).OR.ISTAT==3) IOUTPRT=1
      
      ALLOCATE(QMV(2*NV46,LENQMV)) !MM-ALE 51 only                       !OPTIM : SHOULD BE ALLOCATED IN ARRALOC
      IF(M51_IFLG6 == 1)SBUFVOIS = M51_IFLG6_SIZE
      
      IF(ITASK==0)ALLOCATE(BUFVOIS(SBUFVOIS ,NSVOIS+NQVOIS)) !SPMD array of adjacent ALE/CFD elems           !OPTIM : SHOULD BE ALLOCATED IN ARRALOC,  same for FLUX_VOIS & N4_VOIS

      IF(N2D /= 0 .AND. NMULT /= 0)THEN
        !WA(1:IADBH-1)                         = FLUX
        !WA(IADBH:IADBH+MAX(1,NMULT)*NUMELQ-1) = BHOLE
        IADBH=MAX(1,NMULT)*4*NUMELQ+1
      ELSE
        IADBH=1
      ENDIF
      !IFSUBM=1 : masse changed
      IFSUBM=1

      IF(IALE+IEULER /= 0)THEN
       DO N=NODFT,NODLT
        IF(ALE_CONNECTIVITY%NALE(N) /= 0) MS0(N) = MS(N)
       ENDDO
       IF(IALEFVM > 0)THEN
         DO N=NODFT,NODLT
           IF(ALE_CONNECTIVITY%NALE(N) /= 0) V(1:3,N) = ZERO !reset velocities
         ENDDO       
       ENDIF
      ENDIF
            
      !---------------------------------------------
      !     INTERFACE 18 - VARIABLE GAP
      !---------------------------------------------
       IF(INTER18_IS_VARIABLE_GAP_DEFINED)THEN
          DO NG=ITASK+1,NGROUP,NTHREAD
             IF (TT > ZERO .AND. IPARG(76, NG) == 1) CYCLE ! --> OFF
             IF(IPARG(8,NG) == 1) CYCLE
             CALL INITBUF (
     1          IPARG   ,NG      ,
     2          MTN     ,NEL     ,NFT     ,IAD      ,ITY     ,
     3          NPT     ,JALE    ,ISMSTR  ,JEUL     ,JTUR    ,
     4          JTHE    ,JLAG    ,JMULT   ,JHBE     ,JIVF    ,
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE   ,IPLA    ,
     6          IREP    ,IINT    ,IGTYP   ,ISRAT    ,ISROT   ,
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE ,JSMS    )
             IF (ITY == 1 .AND. ISOLNOD /= 4) THEN    
               DO I=1,NEL                             
                 XCELL(1,I+NFT)=EXP(LOG(ELBUF_TAB(NG)%GBUF%VOL(I))/THREE)
                 XCELL(2,I+NFT)=ZERO
                 XCELL(3,I+NFT)=ZERO                   
               ENDDO      
             ENDIF                                    
          ENDDO
      CALL MY_BARRIER
        IF(NSPMD>1)THEN
           LENCOM = NERCVOIS(NSPMD + 1) + NESDVOIS(NSPMD + 1)
           CALL SPMD_ENVOIS(1, XCELL, 
     .          NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)    
        ENDIF
      ENDIF
      
      !--------------------------------------------------------
      ! F L U I D E
      !       ALE SUB-CYCLING PREPARATION
      !       IALESUB=1 LEGACY IMPLEMENTATION, NOT PARALLEL, DELETED IN V11
      !       IALESUB=2 PARALLEL IMPLMEMENTATION IN 2002
      !--------------------------------------------------------
      IF(IALESUB /= 0) CALL MY_BARRIER    
      !-----
      IF(IALESUB == 2)THEN
        IF(ITASK == 0)THEN
          IF(TT == ZERO .OR. TT+DT1 > T1S+DT2S)THEN
            IFSUB  = 0
            IFSUBM = 1
            DT1SAVE= DT1
            DT1    = TT-T1S
            T1S    = TT
          ELSE
            IFSUB=1
            IFSUBM=0
          ENDIF
        ENDIF
        CALL MY_BARRIER
        IF(IFSUB == 0)THEN
          NELTSA  = NELTST
          ITYPTSA = ITYPTST
          DT2SAVE = DT2T
          DT2T    = EP30
        ENDIF
        CALL ALESUB1(
     1               ALE_CONNECTIVITY%NALE,V,W,DSAVE,ICODT,ISKEW,SKEW,D,
     2               ITASK,NODFT,NODLT,FSKY,FSKY      )
      ENDIF
      IF(IALESUB == 0  .AND. ITASK == 0)THEN
        IFSUB = 0
        T1S   = TT
      ENDIF
      !-----      
      
      IF(ITHERM == 1 .AND. IALE+IEULER == 0) IFSUBM=0
C----------------------------------
C       ALE - EULER - THERMAL
C       WA  = flux for sforc3 and qforc3
C----------------------------------

      CALL MY_BARRIER
C
      IF (IPARIT == 0 .AND. NSPMD > 1 .AND. 
     +    IFSUBM == 1 .AND. N2D == 0 .AND. INCOMP == 0) THEN
        DO N = NODFT, NODLT
          MS(N) = MS(N)*WEIGHT(N)
          MSNF(N) = MSNF(N)*WEIGHT(N)
        ENDDO
C
        CALL MY_BARRIER
C
      ENDIF
C---------------------------------------------------------
C       ALE - EULER  + 2D : update masses 
C---------------------------------------------------------
      IF (N2D > 0 .AND. IFSUBM == 1) THEN
        DO N = NODFT, NODLT
          MS_2D(N) = ZERO
        ENDDO
C
        CALL MY_BARRIER
C
      ENDIF
C
      IF(IFSUB == 0)THEN
        IF(ITASK==0) CALL STARTIME(MACRO_TIMER_IFSUB0,1)
        IF(IALE+IEULER+ITHERM /= 0)THEN
          IF(INCOMP == 0) THEN
            IF(NSEGFLU > 0 .AND. N2D == 0) THEN
              CALL SEGGETV(IPARG,ELBUF_TAB,ALE_CONNECTIVITY,ITASK,SEGVAR)
C             Comm SPMD SEGVAR Pi -> P0
              IF(NSPMD > 1) THEN

C Part not parallel smt
!$OMP SINGLE
                CALL SPMD_SEGCOM(
     .            SEGVAR,NPSEGCOM,LSEGCOM,NPSEGCOM(NSPMD+1),0)
!$OMP END SINGLE
              ENDIF
            ENDIF
            !------------------------------------------------!
            ! INTERFACE CONVECTIONS                          !
            !------------------------------------------------!  
            IF(NINTER /= 0)THEN
C              element from Interface type 12 calculated on proc_0 + sending X proc_i -> proc_0
C              Envoi X pour interface type 12 ?
               IF (ITASK == 0 .AND. ISPMD == 0)
     .           CALL INTTI0(IPARI ,X      ,V    ,WA    ,
     .                       ITAB  ,IPARG,IXS,SEGVAR,SKEW,ISKWN,INTBUF_TAB)
               CALL MY_BARRIER
            ENDIF
            IF(NEBCS > 0)THEN
               IF(ITASK == 0)THEN
                 I=1+NINTER+NRWALL+NRBODY+NSECT+NJOINT+NRBAG
                 CALL EBCS_MAIN(IGRSURF,SEGVAR,VOLMON,
     .                          A,V,W,X,FSAV(1,I),FV,
     .                          WA,MS,STIFN,IPARG,
     .                          ELBUF_TAB,EBCS_TAB,MULTI_FVM,IXQ,IXS,IXTG,
     .                          ITAB,ALE_CONNECTIVITY%NALE,FSKY)
               ENDIF
               CALL MY_BARRIER
            ENDIF
            !------------------------------------------------!              
            IF(NSEGFLU > 0) THEN
C Comm SPMD SEGVAR P0 -> Pi
               IF(NSPMD > 1) THEN
C Part not parallel smt
!$OMP SINGLE
                 CALL SPMD_SEGCOM(
     .             SEGVAR,NPSEGCOM,LSEGCOM,NPSEGCOM(NSPMD+1),1)

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
     3                       NV46          , VEUL     , IGRNOD, IPARI, IGRTRUSS ,
     4                       IXT           , BUFMAT   , IPM
     5                     )                
         ENDIF
         !==================================================================================   
         
         IF (MULTI_FVM%IS_USED) THEN
          IF(ITASK==0) CALL STARTIME(MACRO_TIMER_MULTIFVM,1)
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
     .              PM, IPM, MULTI_FVM, TT, BUFMAT,NPC,TF)
C     ------------------------
C     Build FVM connectivities
C     ------------------------
               CALL BUILD_CONNECTIVITY(ITASK, MULTI_FVM, ALE_CONNECTIVITY, IPARG)
               CALL MY_BARRIER
C     ---------------------
C     Face data computation
C     ---------------------
               CALL MULTI_FACE_ELEM_DATA(ITASK, IPARG, IXS, IXQ, IXTG, X, W, MULTI_FVM)
               CALL MY_BARRIER
               IF (NSPMD  >  1 .AND. ((MULTI_FVM%MUSCL  >  0) .OR. MULTI_FVM%NS_DIFF)) THEN
!$OMP SINGLE
                  LENCOM = NERCVOIS(NSPMD + 1) + NESDVOIS(NSPMD + 1)
                  CALL SPMD_ENVOIS(3, MULTI_FVM%ELEM_DATA%CENTROID, 
     .                 NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
                  IF (ALLOCATED(MULTI_FVM%VOL)) THEN
                     CALL SPMD_ENVOIS(1, MULTI_FVM%VOL, 
     .                    NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM) 
                  ENDIF
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
     .                 CONDN, CONDNSKY, MULTI_FVM)  
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
     .              PM, IPM, MULTI_FVM, TT, BUFMAT,NPC,TF)
C     ------------------------
C     Build FVM connectivities
C     ------------------------
               CALL BUILD_CONNECTIVITY(ITASK, MULTI_FVM, ALE_CONNECTIVITY, IPARG)
               CALL MY_BARRIER

               CALL MULTI_FACE_ELEM_DATA(ITASK, IPARG, IXS, IXQ, IXTG, X, W, MULTI_FVM)
               CALL MY_BARRIER
               IF (NSPMD  >  1 .AND. MULTI_FVM%MUSCL  >  0) THEN
!$OMP SINGLE
                  LENCOM = NERCVOIS(NSPMD + 1) + NESDVOIS(NSPMD + 1)
                  CALL SPMD_ENVOIS(3, MULTI_FVM%ELEM_DATA%CENTROID, 
     .                 NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
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
               CALL ALEFVM_GRAV_INIT(
     1              AGRAV, IGRV    , LGRAV, SENSOR_TAB,
     2              IXS  , ITASK   , NPC  , TF    , SKEW   ,
     3              ITAB 
     .              )
               CALL MY_BARRIER
               IF (MULTI_FVM%MUSCL  ==  1) THEN
C     --------------------------------------------------
C     Multifluid law time evolution --> Prediction phase
C     Half time step evolution
C     --------------------------------------------------     
                  PRED = .TRUE.

                  CALL MULTI_TIMEEVOLUTION(HALF * DT1, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG, 
     .                 PM, IPM, MULTI_FVM, ALE_CONNECTIVITY, V, A, W, X, D, ALE_CONNECTIVITY%NALE,
     .                 PARTSAV, IPARTS, GRESAV, IGRTH, GRTH, 
     .                 NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, 
     .                 ITAB, ITABM1, TT - DT1, 
     .                 STIFN, FSKY, IADS, FSKYM, 
     .                 CONDN, CONDNSKY, BUFMAT, FV, PRED,ID_GLOBAL_VOIS,FACE_VOIS,EBCS_TAB,NPC,TF)

                  CALL MY_BARRIER
C     -------------------------
C     Multifluid pressure computation
C     -------------------------
                  CALL MULTI_PRESSURE_EQUILIBRIUM(DT1, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG,
     .                 PM, IPM, MULTI_FVM, TT - DT1, BUFMAT,NPC,TF) 
                  CALL MY_BARRIER  
C     --------------------------------------------------
C     Multifluid law time evolution --> Correction phase
C     Full time step evolution, with predicted fluxes
C     --------------------------------------------------     
                  PRED = .FALSE.

                  CALL MULTI_TIMEEVOLUTION(DT1, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG, 
     .                 PM, IPM, MULTI_FVM, ALE_CONNECTIVITY, V, A, W, X, D, ALE_CONNECTIVITY%NALE,
     .                 PARTSAV, IPARTS, GRESAV, IGRTH, GRTH, 
     .                 NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, 
     .                 ITAB, ITABM1, TT - DT1, 
     .                 STIFN, FSKY, IADS, FSKYM, 
     .                 CONDN, CONDNSKY, BUFMAT, FV, PRED,ID_GLOBAL_VOIS,FACE_VOIS,EBCS_TAB,NPC,TF)

                  CALL MY_BARRIER
C     -------------------------
C     Multifluid pressure equilibration
C     -------------------------
                  CALL MULTI_PRESSURE_EQUILIBRIUM(DT1, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG,
     .                 PM, IPM, MULTI_FVM, TT, BUFMAT,NPC,TF) 
                  CALL MY_BARRIER                    
               ELSE
C     -------------------------
C     Multifluid law time evolution
C     ------------------------- 
                  CALL MULTI_TIMEEVOLUTION(DT1, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG, 
     .                 PM, IPM, MULTI_FVM, ALE_CONNECTIVITY, V, A, W, X, D, ALE_CONNECTIVITY%NALE,
     .                 PARTSAV, IPARTS, GRESAV, IGRTH, GRTH, 
     .                 NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, 
     .                 ITAB, ITABM1, TT - DT1, 
     .                 STIFN, FSKY, IADS, FSKYM, 
     .                 CONDN, CONDNSKY, BUFMAT, FV, .FALSE.,ID_GLOBAL_VOIS,FACE_VOIS,EBCS_TAB,NPC,TF)
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
     .                 PM, IPM, MULTI_FVM, TT, BUFMAT,NPC,TF) 
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
     .                 IXS, IXQ, IPARG, X, A, V, W, MS, MSNF, VEUL, 
     .                 STIFN, FSKY, IADS, FSKYM, 
     .                 CONDN, CONDNSKY, MULTI_FVM)  
                  CALL MY_BARRIER
               ENDIF
C     -------------------------
C     Multifluid law time step computation
C     -------------------------
               CALL MULTI_COMPUTE_DT(DT2T, ELBUF_TAB, IPARG, ITASK, IXS, IXQ, IXTG, 
     .              PM, IPM, MULTI_FVM, W, X,
     .              NELTST, ITYPTST)  
               CALL MY_BARRIER
            ENDIF
            IF(ITASK==0) CALL STOPTIME(MACRO_TIMER_MULTIFVM,1)
         ELSE
!------------------------------------------------!
! CONVECTION, CONDUCTION, REZONING               !
!------------------------------------------------! 
            CALL ALETHE(
     1           IPARG    ,ELBUF_TAB ,WA      ,VAL2    ,PHI     ,
     2           ALE_CONNECTIVITY    ,IXS       ,IXQ     ,V       ,W       ,
     3           X        ,PM        ,MS      ,VEUL    ,FILL    ,
     4           DFILL    ,ALPH      ,FV      ,BUFMAT  ,TF      ,
     5           NPC      ,ITASK     ,NBRCVOIS,NBSDVOIS,LNRCVOIS,
     6           LNSDVOIS ,NERCVOIS  ,NESDVOIS,LERCVOIS,LESDVOIS,
     7           IADS     ,SEGVAR    ,MSNF    ,NODFT   ,NODLT   ,
     8           WA(IADBH),IPM       ,QMV     ,ITAB    ,ITABM1  ,
     9           LENQMV   ,NV46      ,LGAUGE  ,GAUGE   ,
     A           IGAUP    ,NGAUP     ,VOLN,
     B           IAD_ELEM, FR_ELEM)
         ENDIF
      ELSE
         IFSUBM=0
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
                DFILL(N,M)=0.
              ENDDO
            ENDDO
          ENDIF

         !---------------------------------------------
         !     ALEFVM : FINITE VOLUME FOR MOMENTUM
         !---------------------------------------------          
          IF(IALEFVM > 0) THEN
            !GRAVITY INIT
            ! VERTEX(4,1:NUMNOD) = ZERO  !-> not needed, nodes remain the same ones and VERTEX is initialized on allocation
            CALL ALEFVM_GRAV_INIT(
     1                           AGRAV, IGRV    , LGRAV, SENSOR_TAB,
     2                           IXS  , ITASK   , NPC  , TF    , SKEW   ,
     3                           ITAB 
     .                           )
          ENDIF       
          

          CALL MY_BARRIER  !MM-ALE 20 + ALEFVM
          
         !---------------------------------------------
         ! TETRA4 : SMOOTH FINITE ELEMENT FORMULATIONS
         !---------------------------------------------
c non parallel SMP section (SPMD communications in S4ALESFEM)
!$OMP SINGLE
          IF(ISFEM >= 1) THEN
            CALL S4ALESFEM(IPARG,IXS,X,ELBUF_TAB,VARNOD,PM,
     .                     IAD_ELEM,FR_ELEM)
          ENDIF
c end non parallel SMP section
!$OMP END SINGLE

         !---------------------------------------------
         ! ALL ELEMENTS EXPECT LAW/=11
         !---------------------------------------------
          DO NG=ITASK+1,NGROUP,NTHREAD
C     ALE ON / OFF
             NGR_SENSOR = NG
             IF (TT  >  ZERO .AND. IPARG(76, NG)  ==  1) CYCLE ! --> OFF
             IF(IPARG(8,NG) /= 1) THEN
C
             CALL INITBUF (IPARG    ,NG      ,
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
                  CALL S4FORC3(ELBUF_TAB,NG     ,
     1            PM        ,GEO       ,IXS     ,X          ,
     2            A         ,V         ,MS      ,W          ,WA       ,
     3            VAL2      ,VEUL      ,FV      ,ALE_CONNECTIVITY      ,IPARG    ,
     4            TF        ,NPC       ,BUFMAT  ,PARTSAV    ,NLOC_DMG ,
     5            DT2T      ,NELTST    ,ITYPTST ,STIFN      ,FSKY     ,
     6            IADS      ,OFFSET    ,EANI    ,IPARTS(NF1),
     7            FX(1,1)   ,FY(1,1)   ,FZ(1,1) ,FX(1,2)    ,FY(1,2)  ,
     8            FZ(1,2)   ,FX(1,3)   ,FY(1,3) ,FZ(1,3)    ,FX(1,4)  ,
     9            FY(1,4)   ,FZ(1,4)   ,NEL     ,FSKYM      ,MSNF     ,
     A            IPM       ,IGEO      ,BUFVOIS ,ISTRA      ,ITASK    ,
     B            BID       ,BID       ,BID     ,IBID       ,GRESAV   ,
     C            GRTH      ,IGRTH     ,MSSA    ,DMELS      ,TABLE    ,
     D            XDP       ,VARNOD    ,VOLN    ,BID        ,BID      ,
     E            D         ,SENSOR_TAB,IOUTPRT ,MATPARAM_TAB,IBID)
C
                 ELSEIF (ITY == 1 .AND. ISOLNOD /= 4) THEN
                  NULLIFY(pQMV); 
                  IF(TRIMAT>0)THEN
                    pQMV => QMV(1,1+NFT)
                  ELSE
                    pQMV => BID
                  ENDIF
                  CALL SFORC3(ELBUF_TAB,NG      ,
     2            PM           ,GEO          ,IXS          ,X            ,NV46    , 
     3            A            ,V            ,MS           ,W            ,WA      , 
     4            VAL2         ,VEUL         ,FV           ,ALE_CONNECTIVITY     ,IPARG   , 
     5            TF           ,NPC          ,BUFMAT       ,PARTSAV      ,ITAB    ,          
     6            DT2T         ,NELTST       ,ITYPTST      ,STIFN        ,FSKY    , 
     7            IADS         ,OFFSET       ,EANI         ,IPARTS(1+NFT),   
     8            FX(1,1)      ,FY(1,1)      ,FZ(1,1)      ,FX(1,2)      ,FY(1,2) ,     
     9            FZ(1,2)      ,FX(1,3)      ,FY(1,3)      ,FZ(1,3)      ,FX(1,4) ,     
     A            FY(1,4)      ,FZ(1,4)      ,FX(1,5)      ,FY(1,5)      ,FZ(1,5) ,     
     B            FX(1,6)      ,FY(1,6)      ,FZ(1,6)      ,FX(1,7)      ,FY(1,7) ,     
     C            FZ(1,7)      ,FX(1,8)      ,FY(1,8)      ,FZ(1,8)      ,NEL     ,     
     D            FSKYM        ,MSNF         ,ISKY         ,FSKYI        ,
     E            NVC          ,IPM          ,IGEO         ,BID          ,BID     ,     
     F            BID          ,BID          ,BID          ,BUFVOIS ,       
     G            ITASK        ,pQMV         ,ISTRA        ,TEMP         ,BID     ,        
     H            BID          ,IBID         ,GRESAV       ,GRTH         ,IGRTH   ,       
     I            MSSA         ,DMELS        ,TABLE        ,BID          ,BID     ,       
     J            BID          ,BID          ,BID          ,BID          ,BID     ,
     K            BID          ,BID          ,IPARG(1,NG)  ,XDP     ,
     M            BID          ,IBID         ,IBID         ,VOLN         ,CONDN   ,
     N            CONDNSKY     ,AGRAV        ,IGRV         ,LGRAV        ,SENSOR_TAB,
     O            SKEW          ,
     P            ALE_CONNECTIVITY%NALE      ,D            ,IOUTPRT      ,NLOC_DMG,
     Q            MATPARAM_TAB ,IBID)
C
                 ELSEIF (ITY == 2 .AND. JMULT == 0) THEN
                  NULLIFY(pQMV); 
                  IF(TRIMAT>0)THEN
                    pQMV => QMV(1,1+NFT)
                  ELSE
                    pQMV => BID
                  ENDIF   
                  CALL QFORC2(
     .              ELBUF_TAB,NG ,
     1              PM           ,GEO          ,IXQ         ,X         ,A        ,  
     2              V            ,MS           ,W           ,WA        ,VAL2     ,  
     3              VEUL         ,FV           ,ALE_CONNECTIVITY       ,IPARG     ,NLOC_DMG ,
     4              TF           ,NPC          ,BUFMAT      ,PARTSAV   ,
     5              DT2T         ,NELTST       ,ITYPTST     ,STIFN     ,OFFSET   ,  
     6              EANI         ,IPARTQ(1+NFT),NEL         ,IADQ      ,FSKY     ,  
     9              IPM          ,BUFVOIS      ,pQMV        ,
     A              GRESAV       ,GRTH         ,IGRTH       ,TABLE     ,IGEO     ,
     B              VOLN         ,ITASK        ,MS_2D       ,FSKYM     ,IOUTPRT  ,MATPARAM_TAB,
     C              H3D_DATA%STRAIN) 
c
                 ELSEIF (ITY == 2 .AND. JMULT /= 0) THEN
                  CALL BFORC2(ELBUF_TAB,NG      ,
     1              PM          ,GEO   ,IXQ    ,X         ,
     2              A           ,V     ,MS     ,W         ,WA          ,
     3              VAL2        ,VEUL  ,FV     ,ALE_CONNECTIVITY     ,IPARG       ,
     4              IPARG(1,NG) ,FILL  ,DFILL  ,IMS       ,NLOC_DMG    ,
     5              IFILL       ,TF    ,NPC    ,BUFMAT    ,PARTSAV     ,
     6              DT2T        ,NELTST,ITYPTST,STIFN     ,OFFSET      ,
     7              EANI        ,IPARTQ(1+NFT),NEL,IADQ   ,FSKY        ,
     8              IPM ,BUFVOIS,
     9              GRESAV,GRTH ,IGRTH ,TABLE  ,IGEO,
     O              VOLN  ,ITASK,MS_2D ,FSKYM  ,MATPARAM_TAB,
     B              IBID)
                 ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDDO
C

         !---------------------------------------------
         ! ELEMENTS : LAW==11
         !---------------------------------------------
          CALL MY_BARRIER !wait for adjacent elems
          IF (ILAW11 /= 0) THEN            !LAW11 ou LAW51 IN/OUT
            IF(NSPMD > 1) THEN
             ! SPMD COMMUNICATION VOISINS LOI11
             !
             ! WA[1..IADC-1] : FLUX
             ! WA[IADC..IADD-1] : BUFVOIS (buffer elements voisins en SPMD)
             ! WA[IADD..LENWA] : BUFCOM (wa pour comm spmd)
C Partie non parallele smt
!$OMP SINGLE
              LENCOM=NBRCVOIS(NSPMD+1)+NBSDVOIS(NSPMD+1)
              IF(M51_IFLG6 == 0) THEN
              !======LAW11-NRF/SPMD=====================================!
                 ! comm noeuds voisins
                 CALL SPMD_XVOIS(
     1              V       ,NBRCVOIS,NBSDVOIS,LNRCVOIS,LNSDVOIS,
     2              LENCOM  )
                 ! a optimiser en fct des combinaisons d'options possibles
                 IF(INCOMP == 1.OR.ITURB+ITHERM == 0)
     .             CALL SPMD_XVOIS(
     1               X       ,NBRCVOIS,NBSDVOIS,LNRCVOIS,LNSDVOIS,
     2               LENCOM  )
                   ! comm elements voisins
                 CALL SPMD_L11VOIS(
     1             BUFVOIS ,IPARG   ,ELBUF_TAB,PM      ,IXS     ,
     2             IXQ     ,NERCVOIS,NESDVOIS,LERCVOIS,LESDVOIS,
     3             LENCOM  )
              ELSE
              !======LAW51-NRF/SPMD=====================================!
                 CALL SPMD_L51VOIS(
     1               BUFVOIS ,IPARG   ,ELBUF_TAB,PM      ,IXS     ,
     2               IXQ     ,NERCVOIS,NESDVOIS,LERCVOIS,LESDVOIS,
     3               LENCOM  ,IPM     ,BUFMAT)
              ENDIF
!$OMP END SINGLE
            ENDIF
            
            DO NG=ITASK+1,NGROUP,NTHREAD
C     ALE ON / OFF
               NGR_SENSOR = NG
               IF (TT  >  ZERO .AND. IPARG(76, NG)  ==  1) CYCLE ! --> OFF
               CALL INITBUF (IPARG    ,NG      ,
     2           MTN     ,NEL     ,NFT     ,IAD     ,ITY     ,
     3           NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4           JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5           NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,IPLA    ,
     6           IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7           ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
               IF (MTN  ==  151) CYCLE
              IF (IPARG(8,NG) /= 1) THEN
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
                  CALL S4FORC3(ELBUF_TAB,NG     ,
     1            PM        ,GEO       ,IXS     ,X          ,
     2            A         ,V         ,MS      ,W          ,WA       ,
     3            VAL2      ,VEUL      ,FV      ,ALE_CONNECTIVITY      ,IPARG    ,
     4            TF        ,NPC       ,BUFMAT  ,PARTSAV    ,NLOC_DMG ,
     5            DT2T      ,NELTST    ,ITYPTST ,STIFN      ,FSKY     ,
     6            IADS      ,OFFSET    ,EANI    ,IPARTS(NF1),
     7            FX(1,1)   ,FY(1,1)   ,FZ(1,1) ,FX(1,2)    ,FY(1,2)  ,
     8            FZ(1,2)   ,FX(1,3)   ,FY(1,3) ,FZ(1,3)    ,FX(1,4)  ,
     9            FY(1,4)   ,FZ(1,4)   ,NEL     ,FSKYM      ,MSNF     ,
     A            IPM       ,IGEO      ,BUFVOIS ,ISTRA      ,ITASK    ,
     B            BID       ,BID       ,BID     ,IBID       ,GRESAV   ,
     C            GRTH      ,IGRTH     ,MSSA    ,DMELS      ,TABLE    ,
     D            XDP       ,BID       ,VOLN    ,BID        ,BID      ,
     E            D         ,SENSOR_TAB,IOUTPRT ,MATPARAM_TAB ,IBID)

                 ELSEIF(ITY == 1 .AND. ISOLNOD /= 4)THEN
                  NULLIFY(pQMV); 
                  IF(TRIMAT>0)THEN
                    pQMV => QMV(1,1+NFT)
                  ELSE
                    pQMV => BID
                  ENDIF
                  CALL SFORC3(ELBUF_TAB,NG      ,
     2            PM           ,GEO         ,IXS       ,X            ,NV46    ,
     3            A            ,V           ,MS        ,W            ,WA      ,   
     4            VAL2         ,VEUL        ,FV        ,ALE_CONNECTIVITY        ,IPARG   ,   
     5            TF           ,NPC         ,BUFMAT    ,PARTSAV      ,ITAB    ,           
     6            DT2T         ,NELTST      ,ITYPTST   ,STIFN        ,FSKY    ,   
     7            IADS         ,OFFSET      ,EANI      ,IPARTS(1+NFT),   
     8            FX(1,1)      ,FY(1,1)     ,FZ(1,1)   ,FX(1,2)      ,FY(1,2) ,      
     9            FZ(1,2)      ,FX(1,3)     ,FY(1,3)   ,FZ(1,3)      ,FX(1,4) ,      
     A            FY(1,4)      ,FZ(1,4)     ,FX(1,5)   ,FY(1,5)      ,FZ(1,5) ,      
     B            FX(1,6)      ,FY(1,6)     ,FZ(1,6)   ,FX(1,7)      ,FY(1,7) ,      
     C            FZ(1,7)      ,FX(1,8)     ,FY(1,8)   ,FZ(1,8)      ,NEL     ,      
     D            FSKYM        ,MSNF        ,ISKY      ,FSKYI        ,
     E            NVC          ,IPM         ,IGEO      ,BID          ,BID     ,
     F            BID          ,BID         ,BID       ,BUFVOIS ,
     G            ITASK        ,pQMV        ,ISTRA     ,TEMP         ,BID     ,
     H            BID          ,IBID        ,GRESAV    ,GRTH         ,IGRTH   ,      
     I            MSSA         ,DMELS       ,TABLE     ,BID          ,BID     ,      
     J            BID          ,BID         ,BID       ,BID          ,BID     ,      
     K            BID          ,BID         ,IPARG(1,NG)  ,XDP     ,      
     M            BID          ,IBID        ,IBID      ,VOLN         ,CONDN   ,
     N            CONDNSKY     ,AGRAV       ,IGRV      ,LGRAV        ,SENSOR_TAB,
     O            SKEW        ,
     P            ALE_CONNECTIVITY%NALE         ,D           ,IOUTPRT   ,NLOC_DMG     ,
     Q            MATPARAM_TAB ,IBID)
               
                 ELSEIF (ITY == 2 .AND. JMULT == 0) THEN
                  NULLIFY(pQMV); 
                  IF(TRIMAT>0)THEN
                    pQMV => QMV(1,1+NFT)
                  ELSE
                    pQMV => BID
                  ENDIF  
                  CALL QFORC2(
     .              ELBUF_TAB,NG ,
     1              PM           ,GEO          ,IXQ         ,X        ,A         ,  
     2              V            ,MS           ,W           ,WA       ,VAL2      ,      
     3              VEUL         ,FV           ,ALE_CONNECTIVITY       ,IPARG    ,NLOC_DMG  ,              
     4              TF           ,NPC          ,BUFMAT      ,PARTSAV  ,              
     5              DT2T         ,NELTST       ,ITYPTST     ,STIFN    ,OFFSET    ,
     6              EANI         ,IPARTQ(1+NFT),NEL         ,IADQ     ,FSKY      ,
     9              IPM          ,BUFVOIS      ,pQMV        ,
     A              GRESAV       ,GRTH         ,IGRTH       ,TABLE    ,IGEO      ,
     B              VOLN         ,ITASK        ,MS_2D       ,FSKYM    ,IOUTPRT   ,MATPARAM_TAB ,
     C              H3D_DATA%STRAIN)
c
                      ELSEIF(ITY == 2 .AND. JMULT /= 0)THEN
                  CALL BFORC2(ELBUF_TAB,NG      ,
     1            PM         ,GEO     ,IXQ       ,X         ,    
     2            A          ,V         ,MS      ,W         ,WA        ,  
     3            VAL2       ,VEUL      ,FV      ,ALE_CONNECTIVITY     ,IPARG     ,  
     4            IPARG(1,NG),FILL      ,DFILL   ,IMS       ,NLOC_DMG  ,             
     5            IFILL      ,TF        ,NPC     ,BUFMAT    ,PARTSAV   ,       
     5            DT2T       ,NELTST    ,ITYPTST ,STIFN     ,OFFSET    ,       
     6            EANI       ,IPARTQ(1+NFT),NEL  ,IADQ      ,FSKY      ,       
     7            IPM        ,BUFVOIS   ,
     8            GRESAV     ,GRTH      ,IGRTH   ,TABLE     ,IGEO      ,
     9            VOLN       ,ITASK     ,MS_2D   ,FSKYM     ,MATPARAM_TAB,
     B            IBID)

                 ENDIF
                ENDIF
               ENDIF
              ENDIF
            ENDDO!next NG
            
          ENDIF!IF (ILAW11 /= 0) THEN
           
        ENDIF!IF(IALE+IEULER+ITHERM /= 0)THEN
        IF(ITASK==0) CALL STOPTIME(MACRO_TIMER_IFSUB0,1)
      ENDIF!IF(IFSUB == 0)THEN

c      !Fluid mass do not contribute in FEM : add external force for FSI
c      IF(IALE+IEULER /= 0)THEN
c        DO N=NODFT,NODLT
c          MSNF(N) = ZERO
c        ENDDO
c      ENDIF

      !--------------------------------------------------------
      !       ALE SUB-CYCLING SUITE
      !--------------------------------------------------------
      IF(IALESUB == 2)THEN
        !ifsub=0 fluid calculation
        !ifsub=2 fluid calculation + assembly
        !ifsub=1 no fluid calculation
        IF(ITASK == 0) THEN
          IF(IFSUB == 0) THEN
            IFSUB=2
          ENDIF
        ENDIF
        CALL MY_BARRIER
        IF(IFSUB == 1)THEN
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
      DEALLOCATE(QMV)
      IF(ITASK==0)DEALLOCATE(BUFVOIS)


      RETURN
      END
