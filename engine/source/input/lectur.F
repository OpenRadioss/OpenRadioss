Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  LECTUR                        source/input/lectur.F         
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ANIM_BUILD_INDEX_ALL          source/output/anim/reader/anim_build_index_all.F
Chd|        ARRET                         source/system/arret.F         
Chd|        CHKIPARI                      source/interfaces/interf/chkstfn3.F
Chd|        FR_RLALE                      source/mpi/kinematic_conditions/fr_rlink1.F
Chd|        FR_RLINK1                     source/mpi/kinematic_conditions/fr_rlink1.F
Chd|        LCBCSF                        source/constraints/general/bcs/lcbcsf.F
Chd|        LECCUT                        source/tools/sect/leccut.F    
Chd|        LECDAMP                       source/input/lecdamp.F        
Chd|        LECEIG                        source/input/leceig.F         
Chd|        LECFLSW                       source/fluid/lecflsw.F        
Chd|        LECFUN                        source/tools/curve/lecfun.F   
Chd|        LECFVBAG                      source/input/lecfvbag.F       
Chd|        LECFVBAG1                     source/input/lecfvbag1.F      
Chd|        LECFXINP                      source/input/lecfxinp.F       
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|        LECIMPL                       source/input/lectur.F         
Chd|        LECINV                        source/input/lecinv.F         
Chd|        LECNOISE                      source/general_controls/computation/lecnoise.F
Chd|        LECSTAT                       source/input/lecstat.F        
Chd|        RBYONF                        source/constraints/general/rbody/rbyonf.F
Chd|        READ_DYNAIN                   source/output/dynain/read_dynain.F
Chd|        RLINK0                        source/constraints/general/rlink/rlink0.F
Chd|        SPMD_CHKW                     source/mpi/generic/spmd_chkw.F
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        SPMD_WIOUT                    source/mpi/generic/spmd_wiout.F
Chd|        NGR2USR                       source/input/freform.F        
Chd|        ALEMUSCL_MOD                  ../common_source/modules/ale/alemuscl_mod.F
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        CHECK_MOD                     ../common_source/modules/check_mod.F
Chd|        DT_MOD                        share/modules/dt_mod.F        
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|        FXB_MOD                       share/modules/fxb_mod.F       
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTSTAMP_GLOB_MOD             share/modules/intstamp_glob_mod.F
Chd|        LOADS_MOD                     ../common_source/modules/loads/loads_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        OUTPUTS_MOD                   ../common_source/modules/outputs_mod.F
Chd|        OUTPUT_MOD                    ../common_source/modules/output/output_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|        STATE_MOD                     ../common_source/modules/state_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE LECTUR(ICODE   ,ISKEW ,ISKWN  ,IXTG   ,IXS   ,
     2                  IXQ     ,IXC   ,IXT    ,IXP    ,IXR   ,
     3                  ITAB    ,ITABM1,NPC    ,IPARG  ,IGRV  ,
     4                  IBGR    ,IPARI ,
     5                  NPBY    ,LPBY  ,NNLINK ,LLINK  ,LINALE,
     6                  NEFLSW  ,NNFLSW,ICUT   ,INOISE ,
     7                  X       ,V     ,VR     ,MS     ,IN    ,
     8                  SKEW    ,PLD           ,RBY    ,
     9                  WA      ,CRFLSW,XCUT   ,ANIN   ,DAMPR ,
     A                  IGRNOD  ,KXSP  ,WEIGHT ,FR_RBY2,FR_RL ,
     B                  PARTSAV,IPART  ,PM    ,
     C                  MONVOL  ,VOLMON,IPART_STATE,GEO,TABLE ,
     D                  IFRAME  ,XFRAME,ELBUF_STR,IGEO ,INTBUF_TAB,
     E                  IPM     ,H3D_DATA, MULTI_FVM,
     F                  IGRPART ,TAG_SKINS6,ICFIELD,LCFIELD,TAGSLV_RBY,
     G                  MDS_LABEL,MDS_OUTPUT_TABLE,MDS_NMAT,MAX_DEPVAR,
     H                  MDS_NDEPSVAR,STACK,IBCL,ILOADP,LLOADP,SENSORS,
     I                  DYNAIN_DATA,DT ,LOADS ,OUTPUT,NAMES_AND_TITLES)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE MESSAGE_MOD
      USE ELBUFDEF_MOD
      USE INTBUFDEF_MOD
      USE STACK_MOD
      USE H3D_MOD
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
      USE INTSTAMP_GLOB_MOD
      USE FVBAG_MOD
      USE CHECK_MOD
      USE SENSOR_MOD
      USE ANIM_MOD
      USE OUTPUTS_MOD
      USE STATE_MOD
      USE ALEMUSCL_MOD , only:ALEMUSCL_Param
      USE ALE_MOD
      USE DT_MOD
      USE LOADS_MOD
      USE OUTPUT_MOD
      USE FXB_MOD
      USE MY_ALLOC_MOD
      USE NAMES_AND_TITLES_MOD, only:NAMES_AND_TITLES_
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "com09_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
#include      "scr02_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "cong1_c.inc"
#include      "cong2_c.inc"
#include      "scrfs_c.inc"
#include      "stati_c.inc"
#include      "statr_c.inc"
#include      "units_c.inc"
#include      "scrcut_c.inc"
#include      "scrnoi_c.inc"
#include      "parit_c.inc"
#include      "chara_c.inc"
#include      "task_c.inc"
#include      "sphcom.inc"
#include      "impl1_c.inc"
#include      "tabsiz_c.inc"
#include      "remesh_c.inc"
#include      "sms_c.inc"
#include      "rad2r_c.inc"
#include      "inter22.inc"
#include      "userlib.inc"
#include      "scr_thermal_c.inc"
#include      "spmd_c.inc"
#include      "intstamp_c.inc"
#include      "couple_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,NGROUP), IPARI(NPARI,*), IXS(NIXS,*),
     .   IXQ(NIXQ,*), IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*),
     .   IXR(NIXR,*), IXTG(NIXTG,*),ITAB(*), ITABM1(*),
     .   ISKWN(LISKN,*), NPBY(NNPBY,*),NNLINK(*) ,LLINK(*) ,LINALE(*),
     .   ICODE(*) ,ISKEW(*),NPC(*),NEFLSW(*),NNFLSW(*),ICUT(*),
     .   INOISE(*),IGRV(NIGRV,*),IBGR(*),
     .   LPBY(*),KXSP(NISP,*),WEIGHT(*),FR_RBY2(*),
     .   FR_RL(NSPMD+2,*),
     .   IPART(*), MONVOL(*), IPART_STATE(*),IFRAME(LISKN,*),
     .   IGEO(NPROPGI,*),IPM(NPROPMI,*),TAG_SKINS6(*),
     .   ICFIELD(SIZFIELD,*), LCFIELD(*), TAGSLV_RBY(*),
     .   MDS_OUTPUT_TABLE(MDS_NMAT*MAX_DEPVAR),MDS_NMAT,MAX_DEPVAR,
     .   MDS_NDEPSVAR(*),IBCL(*),ILOADP(*),LLOADP(*)
      my_real
     .    SKEW(LSKEW,*),RBY(*),MS(*),IN(*),ANIN(*),
     .    X(*), V(*), VR(*),WA(*),PLD(*),CRFLSW(*),XCUT(*),
     .    DAMPR(NRDAMP,*), PARTSAV(*),PM(NPROPM,*),
     .    VOLMON(*),GEO(NPROPG,*),XFRAME(NXFRAME,*)
      TYPE(TTABLE) TABLE(*)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_STR
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (H3D_DATABASE) :: H3D_DATA
      TYPe (MULTI_FVM_STRUCT) :: MULTI_FVM
      TYPE(GROUP_)  ,DIMENSION(NGRNOD)  :: IGRNOD
      TYPE(GROUP_)  ,DIMENSION(NGRPART) :: IGRPART
      CHARACTER MDS_LABEL(1024,MDS_NMAT)
      TYPE (STACK_PLY) :: STACK
      TYPE (SENSORS_) ,INTENT(INOUT) ,TARGET :: SENSORS
      TYPE (DYNAIN_DATABASE), INTENT(INOUT)  :: DYNAIN_DATA
      TYPE (DT_), INTENT(INOUT)              :: DT
      TYPE (LOADS_), INTENT(IN)              :: LOADS
      TYPE(OUTPUT_), INTENT(INOUT) :: OUTPUT !< output structure
      TYPE(NAMES_AND_TITLES_),INTENT(INOUT) :: NAMES_AND_TITLES  !< NAMES_AND_TITLES host the input deck names and titles for outputs
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NLEC(16), I, NSLIOF, NELSOF, NELQOF, NDAMPN,
     .   NELCOF, NELTOF, NELPOF, NELROF, NINTCH, NUBCSN, ITFOR0, IRFE0,
     .   IRFL0, K, NELOF, NN, NBC, IL, II, J, KLG, KUG, NBLK, NG,
     .   ITY, NEL, NFT, IAD, IGOF, K1, K2, N, I1, I2, I3, IR1, IR2, IR3,
     .   ISK, IC, ICR, M1, M2, IM, NOINT, NSEARCH, JPRI, NPTS,
     .   NRBYON, NRBYOF, NELTGOF,NINIV,NSLIOFN,NSLIOFS,KK,NTY,
     .   NSN,NRTS,NRTM,NMN,L,NCPRI1,NRLINK0,NALELK0,
     .   NSPHOF,IWIOUT,NLECSPH(10),NFXINP,NCRST1,NEIGOFF,NEOFF,
     .   NALEOF,NEULEROF,NTHERMOF,NFVMESH,MLW,LL,ISK1,ISK2,
     .   NFVMODI,IOK, ALEStrL(0:7), NALELINK0, GR_ID,
     .   NALELK_starter, uID, NALELK_removed, JALE,USERL_COUNT,
     .   IDEL2,IS,
     .   STEXT1,MULTIREST1,NLPRI1,LOWMACH_OPT,IERROR,
     .   NINTERSKID,IVOLU,IFV,IFV_TYPE,ISTATG_P,
     .   K3,K4,K5,K6,K7,K8,K9,NINEFRICG,NS,NI

      INTEGER :: NBPARTALEON, NBPARTALEOFF, PARTID, ISON, ION, IOFF, NBALEON_PART, NBALEOFF_PART
      INTEGER, DIMENSION(:), ALLOCATABLE :: ALEOFF_PARTIDS_TMP, ALEOFF_PARTIDS,
     .     ALEON_PARTIDS_TMP, ALEON_PARTIDS
      INTEGER, DIMENSION(:), ALLOCATABLE :: ALELIN_ON_OFF
      my_real
     .   TITLE(20), TFI,  DTF, DTM, ALP, GAM,
     .   CV1, CV2, CV3, VOLM, TSTART, TFIN,DT_INPUT, DT_CRIT
       CHARACTER*9 Cale1,Cale2,ALEform(0:7),CHAR1,CHAR2
       CHARACTER*9 Label1,Label2,Label3,Label4,Label5,Label6,Label7,Label8,Label9
      my_real eta1,eta2,MIN_ASPECT,MIN_DEFV
      my_real, DIMENSION(:), POINTER :: OFFG
      TYPE (SENSOR_STR_) ,DIMENSION(:) ,POINTER :: SENSOR_TAB
      LOGICAL IS_ALREADY_PRINTED
      REAL(KIND=8) :: DTH
      REAL(KIND=8), DIMENSION(9) :: DTH1
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER,EXTERNAL :: NGR2USR
C-----------------------------------------------
      ! setting NOINT in IPARI(13)
      IERR = 0
      DT_INPUT= ZERO
      DO I=1,NINTER
         IPARI(13,I)=10
         IF(IPARI(15,I) == 0) IPARI(15,I)=I
      ENDDO
      SENSOR_TAB => SENSORS%SENSOR_TAB(1:SENSORS%NSENSOR)

      CALL CHKIPARI(IPARI)

      READ (IIN,'(20A4)')  TITLE
      READ (IIN,'(4F16.0,I8,I10,2I8)')TFI,DTH,DTF,DTM,NCPRI1,NCRST1,MULTIREST1,NLPRI1
      READ (IIN,'(5F16.0)')(DTH1(I),I=1,5)
      READ (IIN,'(4F16.0)')(DTH1(I+5),I=1,4)
      READ (IIN,'(6F10.0)')ALP,GAM,CV1,CV2,CV3,VOLM
      READ (IIN,'(I8)') LOWMACH_OPT
      READ (IIN,'(10I8)') NSLIOF,NELSOF,NELQOF,NELCOF,NELTOF,NELPOF,NELROF,NELTGOF,NSLIOFN,NSLIOFS
      READ (IIN,'(I8)') NSPHOF
      READ (IIN,'(3I8)') NALEOF,NEULEROF,NTHERMOF
      READ (IIN,'(2I8)') NRLINK0,NALELK0
      READ (IIN,'(2I8)') NALELINK0
C--------------
C     ALE ON / OFF
C--------------
      READ (IIN, '(I8, I8)') NBPARTALEON, NBPARTALEOFF
      IF(MCHECK == 0)THEN
        NRLINK=NRLINK0
        NALELK=NALELK0
        NALELINK=NALELINK0
        IRPREV = 0
      ENDIF
      NRBYON = 0
      NRBYOF = 0
      NUBCSN = 0
      NINIV  = 0
      READ (IIN,'(2I8)') NUBCSN,NINIV
      READ (IIN,'(3I8)') NINTCH,NRBYON,NRBYOF
      READ (IIN,'(I8)') NDAMPN
      READ(IIN,'(I8)') NFXINP
C-----
      READ(IIN,'(2I8)') NEIGOFF, NEOFF
      READ(IIN,'(2I8)') NFVMESH, NFVMODI
C-----
      IF(NCPRI1 == 0) NCPRI1=1
      IF(MCHECK == 0)NCPRI = NCPRI1
C-----
      IF(MCHECK == 0)NLPRI = NLPRI1
C
      IF(NCRST1 == 0.AND.MCHECK == 0) NCRST=10000000
      IF(TFI /= 0.0) TSTOP=TFI
C
      IF(TOUTP0 /= ZERO)  TOUTP = TOUTP0
      IF(DTOUTP0 > ZERO) DTOUTP= DTOUTP0
      IF(DTOUTP<=ZERO)  TOUTP = EP30
      IF (TOUTP < TT-DT2.AND.DTOUTP > ZERO)TOUTP = TOUTP
     .    + INT((TT-DT2-TOUTP)/DTOUTP)*DTOUTP
      IF (TOUTP < TT-DT2)TOUTP = TOUTP+DTOUTP
C
C .sta files
      IF(TSTAT0 /= ZERO)  TSTAT = TSTAT0
      IF(DTSTAT0 > ZERO) DTSTAT= DTSTAT0
      IF(DTSTAT<=ZERO)  TSTAT = EP30
      IF (TSTAT < TT-DT2.AND.DTSTAT > ZERO)TSTAT = TSTAT
     .    + INT((TT-DT2-TSTAT)/DTSTAT)*DTSTAT
      IF (TSTAT < TT-DT2)TSTAT = TSTAT+DTSTAT
C .dynain files
      IF(DYNAIN_DATA%TDYNAIN0 /= ZERO)  DYNAIN_DATA%TDYNAIN = DYNAIN_DATA%TDYNAIN0
      IF(DYNAIN_DATA%DTDYNAIN0 > ZERO)  DYNAIN_DATA%DTDYNAIN= DYNAIN_DATA%DTDYNAIN0
      IF(DYNAIN_DATA%DTDYNAIN<=ZERO)  DYNAIN_DATA%TDYNAIN = EP30
      IF (DYNAIN_DATA%TDYNAIN < TT-DT2.AND.DYNAIN_DATA%DTDYNAIN > ZERO)DYNAIN_DATA%TDYNAIN = DYNAIN_DATA%TDYNAIN
     .    + INT((TT-DT2-DYNAIN_DATA%TDYNAIN)/DYNAIN_DATA%DTDYNAIN)*DYNAIN_DATA%DTDYNAIN
      IF (DYNAIN_DATA%TDYNAIN< TT-DT2)DYNAIN_DATA%TDYNAIN = DYNAIN_DATA%TDYNAIN+DYNAIN_DATA%DTDYNAIN
C abf files
      DO I=1,10
        IF(DTABF0(I) > ZERO) DTABF(I)= DTABF0(I)
        IF(DTABFWR0(I) > ZERO) DTABFWR(I)= DTABFWR0(I)
      ENDDO
      IF (ABFILE(1) /= 0) TABFIS(1) = TT
C
      IF(DTH /= ZERO.AND.MCHECK == 0) OUTPUT%TH%DTHIS=DTH
      DO I= 1, 9
       IF(DTH1(I) /= ZERO.AND.MCHECK == 0) OUTPUT%TH%DTHIS1(I)=DTH1(I)
      ENDDO
c h3d files
      H3D_DATA%DTH3D = ZERO
      H3D_DATA%TH3D_STOP = EP30
      IF(H3D_DATA%TH3D0 /= ZERO)  H3D_DATA%TH3D = H3D_DATA%TH3D0
      IF(H3D_DATA%DTH3D0 > ZERO) H3D_DATA%DTH3D= H3D_DATA%DTH3D0
      IF(H3D_DATA%TH3D_STOP0 > ZERO .AND. H3D_DATA%TH3D_STOP0 /= EP20)THEN
        H3D_DATA%TH3D_STOP = H3D_DATA%TH3D_STOP0
      ENDIF
      IF(H3D_DATA%DTH3D<=ZERO)  THEN
        H3D_DATA%TH3D = EP30
        H3D_DATA%TH3D_STOP = EP30
      ENDIF
      IF (H3D_DATA%TH3D < TT-DT2.AND. H3D_DATA%DTH3D > ZERO)
     .     H3D_DATA%TH3D = H3D_DATA%TH3D + INT((TT-DT2-H3D_DATA%TH3D)/H3D_DATA%DTH3D)*H3D_DATA%DTH3D
      IF (H3D_DATA%TH3D < TT-DT2)H3D_DATA%TH3D = H3D_DATA%TH3D+H3D_DATA%DTH3D
C
      IF(MCHECK == 0)THEN
        ALE%GRID%VGY0=ZERO
        ALE%GRID%VGZ0=ZERO
        IF(DTF /= ZERO) DTFAC=DTF
        IF(DTM /= ZERO) DTMIN=DTM
        IF(ALP /= ZERO) ALE%GRID%ALPHA=ALP
        IF(GAM /= ZERO) ALE%GRID%GAMMA=GAM
        IF(CV1 /= ZERO) ALE%GRID%VGX  =CV1
        IF(CV2 /= ZERO) ALE%GRID%VGY  =CV2
        IF(CV3 /= ZERO) ALE%GRID%VGZ  =CV3
        IF(ALE%GRID%NWALE_ENGINE == 2)THEN
          IF(ALP /= ZERO) THEN
            ALE%GRID%ALPHA = ALP/(-ALE%GRID%VGX+SQRT(ALE%GRID%VGX**2+ ONE))
            DT_INPUT = ALP
          ELSE
            DT_INPUT = ALE%GRID%ALPHA*(-ALE%GRID%VGX+SQRT(ALE%GRID%VGX**2+ ONE))
          ENDIF
        ENDIF
        IF(ALE%GRID%NWALE_ENGINE == 1.AND.ALP == ZERO) ALE%GRID%ALPHA=EP30
        IF(VOLM /= ZERO)VOLMIN=VOLM
        IF(INT22>0)THEN
          DTFAC22 = ONE
          IF(DTFAC>HALF)DTFAC22 = HALF / DTFAC
        ENDIF
      ENDIF
C
      IF(MCHECK==0)THEN
       IF(IDTMINS/=0)THEN
        IF(DTMINS == ZERO)DTMINS = DTMIN
        IF(DTFACS == ZERO)DTFACS = DTFAC
        IF(TOL_SMS == ZERO) TOL_SMS = EM03
        IF(NSMSPCG==0)NSMSPCG=1000
        IDTGRS =IDTGRS_OLD
        IF(ISPMD==0)THEN
C
         IF(IDTMINS==2.AND.IREST_MSELT==0)THEN
           CALL ANCMSG(MSGID=120,ANMODE=ANINFO_BLIND)
           CALL ARRET(2)
         END IF
C
C         IF((IDTMINS==2.OR.IDTMINS_INT/=0).AND.NODADT/=0)THEN
C           CALL ANCMSG(MSGID=209,ANMODE=ANINFO_BLIND)
C           CALL ARRET(2)
C         END IF
        END IF
       ENDIF
       IF(IDTMINS_INT/=0)THEN
        IF(DTMINS_INT == ZERO)DTMINS_INT = DTMIN
        IF(DTFACS_INT == ZERO)DTFACS_INT = DTFAC
        IF(TOL_SMS == ZERO) TOL_SMS = EM03
        IF(NSMSPCG==0)NSMSPCG=1000
       ENDIF
      ELSE
        IDTMINS=IDTMINS_OLD
        DTMINS =DTMINS_OLD
        DTFACS =DTFACS_OLD
        IDTGRS =IDTGRS_OLD
        IDTMINS_INT=IDTMINS_INT_OLD
        DTMINS_INT =DTMINS_INT_OLD
        DTFACS_INT =DTFACS_INT_OLD
      END IF
C
      DTFACX = ONE
      DO I=1,51
        IF(DTMIN1(I) == ZERO)DTMIN1(I) = DTMIN
        IF(DTFAC1(I) == ZERO)DTFAC1(I) = DTFAC
        DTFACX = MIN(DTFAC1(I), DTFACX)
      ENDDO

      IF(IDTMIN(52) == 0)IDTMIN(52) = 1
      IF(DTFAC1(52) == ZERO)DTFAC1(52) = ZEP9
      IF(DTMIN1(52) == ZERO)DTMIN1(52) = EM20

      I=102
        IF(ALE%GLOBAL%IDT_ALE==-1)THEN
          IF(DTMIN1(I) == ZERO)DTMIN1(I) = DTMIN
          IF(DTFAC1(I) == ZERO)DTFAC1(I) = DTFAC
        ELSE
          IF(DTMIN1(I) == ZERO)DTMIN1(I) = ZERO
          IF(DTFAC1(I) == ZERO)DTFAC1(I) = HALF
        ENDIF
C
      IF(NODADT == 0)THEN
        IF(IDTMIN(1) == 0)IDTMIN(1) = 1
        IF(IDTMIN(2) == 0)IDTMIN(2) = 1
        IF(IDTMIN(3) == 0)IDTMIN(3) = 2
        IF(IDTMIN(7) == 0)IDTMIN(7) = 2
      ENDIF
C
      IF(ISPMD == 0) WRITE(IOUT,'(//1X,20A4//)') TITLE
      ITFOR0=ITFORM + 1
      IRFE0 =(IRFORM/5)
      IRFL0 =(IRFORM-5*IRFE0) + 1
      IRFE0 =IRFE0 + 1

C --------------------------------------------------------------------
c      CALL RCHECKMASS(IXR      ,GEO      ,PM       ,MSR      ,INR    ,
c     .                MS       ,IN       ,ITAB     ,IGEO     ,IPM    ,
c     .                UPARAM   ,IPART    ,IPARTR   )
C --------------------------------------------------------------------
C User Libraries output & checks
C --------------------------------------------------------------------
      IF (NSPMD > 1)THEN
        USERL_COUNT = USERL_AVAIL
        CALL SPMD_GLOB_ISUM9(USERL_COUNT,1)
        IF (ISPMD==0)THEN
          IF (USERL_COUNT /= 0 .AND. USERL_COUNT /= NSPMD)THEN
            CALL ANCMSG(MSGID=254,ANMODE=ANINFO,
     .            C1=DLIBFILE(1:DLIBFILE_SIZE))
            CALL ARRET(2)
          ENDIF
         ENDIF
      ENDIF
C When Dynamical user libraries are used, add a print in 0001.out file
      IF (USERL_AVAIL==1)THEN
       IF(ISPMD==0)THEN
        WRITE(IOUT,4500)
        WRITE(IOUT,4600) DLIBFILE(1:DLIBFILE_SIZE),DLIBTKVERS
       ENDIF
      ENDIF
C --------------------------------------------------------------------

      IF (IRAD2R==1) THEN
        IF(IMACH/=3.OR.ISPMD==0) WRITE(IOUT,1099)
        IF (ITFOR0==1 ) THEN
           CALL ANCMSG(MSGID=242,ANMODE=ANINFO_BLIND)
           CALL ARRET(2)
        ENDIF
      ELSE
        IF ((R2R_SIU==1).AND.(ISPMD==0)) THEN
           CALL ANCMSG(MSGID=239,ANMODE=ANINFO_BLIND)
           CALL ARRET(2)
        ENDIF
      ENDIF
C
      MIN_ASPECT = DT%BRICK_CST_COL_MIN
      MIN_DEFV = DT%BRICK_CST_DEFV_MIN
      IF(ISPMD == 0.AND.MCHECK == 0)WRITE(IOUT,1100)TSTOP,OUTPUT%TH%DTHIS,DTFAC,DTMIN
      IF(NODADT == 0)THEN
       IF ((MIN_ASPECT+MIN_DEFV)>ZERO) THEN
         IF(ISPMD == 0.AND.MCHECK == 0)THEN
           WRITE(IOUT,4700) DTFAC1(1),DTMIN1(1),IDTMIN(1),
     +                 MIN_ASPECT,MIN_DEFV,
     +                 DTFAC1(2),DTMIN1(2),IDTMIN(2),
     +                 DTFAC1(3),DTMIN1(3),IDTMIN(3),
     +                 DTFAC1(4),DTMIN1(4),IDTMIN(4),
     +                 DTFAC1(5),DTMIN1(5),IDTMIN(5),
     +                 DTFAC1(6),DTMIN1(6),IDTMIN(6),
     +                 DTFAC1(9),DTMIN1(9),IDTMIN(9),
     +                 DTFAC1(10),DTMIN1(10),IDTMIN(10)
          !WRITE(IOUT,4710) DTFAC1(102),DTMIN1(102)
          IF(IDT1SH /=0) WRITE(IOUT,4720)
          IF(IDT1SOL/=0) WRITE(IOUT,4730)
          IF(IDT1TET10/=0) WRITE(IOUT,4740)
         ENDIF!(ISPMD == 0.AND.MCHECK == 0)
       ELSE
         IF(ISPMD == 0.AND.MCHECK == 0)THEN
          WRITE(IOUT,1105) DTFAC1(1),DTMIN1(1),IDTMIN(1),
     +                 DTFAC1(2),DTMIN1(2),IDTMIN(2),
     +                 DTFAC1(3),DTMIN1(3),IDTMIN(3),
     +                 DTFAC1(4),DTMIN1(4),IDTMIN(4),
     +                 DTFAC1(5),DTMIN1(5),IDTMIN(5),
     +                 DTFAC1(6),DTMIN1(6),IDTMIN(6),
     +                 DTFAC1(9),DTMIN1(9),IDTMIN(9),
     +                 DTFAC1(10),DTMIN1(10),IDTMIN(10)
          !WRITE(IOUT,4710) DTFAC1(102),DTMIN1(102)
          IF(IDT1SH /=0) WRITE(IOUT,4720)
          IF(IDT1SOL/=0) WRITE(IOUT,4730)
          IF(IDT1TET10/=0) WRITE(IOUT,4740)
         ENDIF!(ISPMD == 0.AND.MCHECK == 0)
       END IF !(MIN_ASPECT+MIN_DEFV)>ZERO)
       IF (DT%IDEL_BRICK>ZERO) THEN
           IF(ISPMD == 0.AND.MCHECK == 0)THEN
             WRITE(IOUT,5020)DT%BRICK_DEL_COL_MIN,DT%BRICK_DEL_DEFV_MIN,DT%BRICK_DEL_ASP_MAX,DT%BRICK_DEL_DEFV_MAX
           ENDIF
       ENDIF
        IF(IDTMIN(11) == 3 .OR. IDTMIN(11) == 8) THEN
           IF(ISPMD == 0.AND.MCHECK == 0)WRITE(IOUT,1107)DTFAC1(11),DTMIN1(11),IDTMIN(11)
        ENDIF
        IF(IDTMINS_INT/=0)THEN
         IF((IMACH /= 3.OR.ISPMD == 0).AND.MCHECK == 0)THEN
          WRITE(IOUT,1209) DTFACS_INT,DTMINS_INT
         END IF
        END IF
        IF(IDTMINS/=0)THEN
         IF((IMACH /= 3.OR.ISPMD == 0).AND.MCHECK == 0.AND.IDTGRS<=0)THEN
          WRITE(IOUT,1109) DTFACS,DTMINS,TOL_SMS,NSMSPCG,M_VS_SMS,NCPRISMS,-IDTGRS
         ELSEIF((IMACH /= 3.OR.ISPMD == 0).AND.MCHECK == 0)THEN
          WRITE(IOUT,1109) DTFACS,DTMINS,TOL_SMS,NSMSPCG,M_VS_SMS,NCPRISMS,IGRPART(IDTGRS)%ID
         END IF
        END IF
      ELSE
        IF(ISPMD == 0.AND.MCHECK == 0)THEN
          IF(IDTMIN(11)==0)THEN
            WRITE(IOUT,1116) DTFAC1(11),ZERO
          ELSEIF(IDTMIN(11)==1.OR.IDTMIN(11)==3.OR.IDTMIN(11) == 8)THEN
            IF ( PERCENT_ADDMASS > ZERO) THEN
              WRITE(IOUT,1206) DTFAC1(11),DTMIN1(11),PERCENT_ADDMASS,IDTMIN(11)
            ELSE
              WRITE(IOUT,1106) DTFAC1(11),DTMIN1(11),IDTMIN(11)
            ENDIF
          END IF
        END IF
        IF ((MIN_ASPECT+MIN_DEFV)>ZERO) THEN
         IF(ISPMD == 0.AND.MCHECK == 0)THEN
           WRITE(IOUT,4700) DTFAC1(1),DTMIN1(1),IDTMIN(1),
     +                 MIN_ASPECT,MIN_DEFV,
     +                 DTFAC1(2),DTMIN1(2),IDTMIN(2),
     +                 DTFAC1(3),DTMIN1(3),IDTMIN(3),
     +                 DTFAC1(4),DTMIN1(4),IDTMIN(4),
     +                 DTFAC1(5),DTMIN1(5),IDTMIN(5),
     +                 DTFAC1(6),DTMIN1(6),IDTMIN(6),
     +                 DTFAC1(9),DTMIN1(9),IDTMIN(9),
     +                 DTFAC1(10),DTMIN1(10),IDTMIN(10)
           !WRITE(IOUT,4710) DTFAC1(102),DTMIN1(102)
           IF(IDT1SH /=0) WRITE(IOUT,4720)
           IF(IDT1SOL/=0) WRITE(IOUT,4730)
           IF(IDT1TET10/=0) WRITE(IOUT,4740)
         ENDIF
        ELSE
         IF(ISPMD == 0.AND.MCHECK == 0)THEN
          WRITE(IOUT,1105) DTFAC1(1),DTMIN1(1),IDTMIN(1),
     +                 DTFAC1(2),DTMIN1(2),IDTMIN(2),
     +                 DTFAC1(3),DTMIN1(3),IDTMIN(3),
     +                 DTFAC1(4),DTMIN1(4),IDTMIN(4),
     +                 DTFAC1(5),DTMIN1(5),IDTMIN(5),
     +                 DTFAC1(6),DTMIN1(6),IDTMIN(6),
     +                 DTFAC1(9),DTMIN1(9),IDTMIN(9),
     +                 DTFAC1(10),DTMIN1(10),IDTMIN(10)
          !WRITE(IOUT,4710) DTFAC1(102),DTMIN1(102)
          IF(IDT1SH /=0) WRITE(IOUT,4720)
          IF(IDT1SOL/=0) WRITE(IOUT,4730)
          IF(IDT1TET10/=0) WRITE(IOUT,4740)
         ENDIF
        END IF !((MIN_ASPECT+MIN_DEFV)>ZERO) THEN
        IF (DT%IDEL_BRICK>ZERO) THEN
           IF(ISPMD == 0.AND.MCHECK == 0)THEN
             WRITE(IOUT,5020)DT%BRICK_DEL_COL_MIN,DT%BRICK_DEL_DEFV_MIN,DT%BRICK_DEL_ASP_MAX,DT%BRICK_DEL_DEFV_MAX
           ENDIF
        END IF
        IF(IDTMINS_INT/=0)THEN
         IF((IMACH /= 3.OR.ISPMD == 0).AND.MCHECK == 0)THEN
          WRITE(IOUT,1209) DTFACS_INT,DTMINS_INT
         END IF
        END IF
C
        IF(IDTMINS/=0)THEN
C
          IF (ISMS_SELEC < 3) THEN
          IF((IMACH /= 3.OR.ISPMD == 0).AND.MCHECK == 0.AND.IDTGRS<=0) THEN
           WRITE(IOUT,1108) DTFACS,DTMINS,TOL_SMS,NSMSPCG,NCPRISMS,-IDTGRS
          ELSEIF((IMACH /= 3.OR.ISPMD == 0).AND.MCHECK == 0)THEN
           WRITE(IOUT,1108) DTFACS,DTMINS,TOL_SMS,NSMSPCG,NCPRISMS,IGRPART(IDTGRS)%ID
            END IF
C
          ELSE
C--        /DT/CST_AMS >- automatic element selection
            DT_CRIT = DTMINS / MAX(EM20,DTFAC1(11))
            IF((IMACH /= 3.OR.ISPMD == 0).AND.MCHECK == 0.AND.IDTGRS<=0)THEN
              WRITE(IOUT,2109) DTFACS,DTMINS,TOL_SMS,NSMSPCG,M_VS_SMS,NCPRISMS,DT_CRIT,-IDTGRS
            ELSEIF((IMACH /= 3.OR.ISPMD == 0).AND.MCHECK == 0)THEN
              WRITE(IOUT,2109) DTFACS,DTMINS,TOL_SMS, NSMSPCG,M_VS_SMS,NCPRISMS,DT_CRIT,IGRPART(IDTGRS)%ID
         END IF
C
        END IF
C
      ENDIF
C
      ENDIF
C

      IF(NODADT==0.AND.(ISTATCND/=0.AND.IMPL_S==0))THEN
        CALL ANCMSG(MSGID=121,ANMODE=ANINFO_BLIND)
        CALL ARRET(2)
      END IF
      IF(ISPMD == 0.AND.MCHECK == 0.AND.KDTINT /= 0)THEN
        WRITE(IOUT,'(A)')' NEW (HIDDEN) TIME STEP COMPUTATION',' ON INTERFACE TYPE 7,11 AND 19 IS ON'
      ENDIF
      IF(ISPMD == 0.AND.MCHECK == 0.AND.KDTSMSTR == 0)THEN
        WRITE(IOUT,'(A)')' BACK TO VERSION 4 COMPUTATION OF NODAL TIME STEP',' CASE OF SMALL STRAIN FOR SOLIDS.'
      ENDIF
      IF(CODVERS>=44.AND.NUMSPH /= 0.AND.MCHECK == 0)THEN
        IF(IDTMIN(51) == 3)THEN
         IF(ISPMD == 0)THEN
          WRITE(ISTDO,*)' ** WARNING SMALL STRAIN FORMULATION FOR SPH'
          WRITE(IOUT,*)' ** WARNING SMALL STRAIN FORMULATION IS NOT AVAILABLE FOR SPH,',' OPTION /DT/SPHCEL/CST WILL BE OMITTED.'
         END IF
         IDTMIN(51)=0
        ENDIF
        IF(NODADT == 1)THEN
         IF(ISPMD == 0) WRITE(IOUT,*)'IMPROVED TIME STEP (NODAL) COMPUTATION ON SPH PARTICLES :'
         DTFAC1(51)=DTFAC1(11)
        ENDIF
        IF(ISPMD == 0) WRITE(IOUT,1151)DTFAC1(51),DTMIN1(51),IDTMIN(51)
      ENDIF
C
      IF(ISPMD == 0.AND.MCHECK == 0) THEN
        IF(NUM_OPT_DT_FVMBAG_0+NUM_OPT_DT_FVMBAG_1>0)THEN
         !if at least on option /DT/FVMBAG/0 or /DT/FVMBAG/1 is defined then output parameter for the retained one (last read one)
         ! (multiple definition is not expected)
         IF(NUM_OPT_DT_FVMBAG_1>0)THEN
            WRITE(IOUT,1156)
         ELSE
            WRITE(IOUT,1155)
         ENDIF
         WRITE(IOUT,1152)DTFAC1(52),DTMIN1(52),IDTMIN(52)
        ENDIF
      ENDIF
      IF(ISPMD == 0.AND.MCHECK == 0) THEN
         K1 = 1
         IS_ALREADY_PRINTED = .FALSE.
         DO IVOLU = 1, NVOLU
            IFV = MONVOL(K1 - 1 + 45)
            IFV_TYPE = MONVOL(K1 - 1 + 2)
            IF (IFV_TYPE == 6 .OR. IFV_TYPE == 8 .OR. IFV_TYPE == 11) THEN
               IF(.NOT.IS_ALREADY_PRINTED)THEN
                 WRITE(IOUT,1157)
                 IS_ALREADY_PRINTED=.TRUE.
               ENDIF
            ENDIF
            IF(IFV_TYPE == 6) WRITE(IOUT,1147)MONVOL(K1)
            IF(IFV_TYPE == 8) WRITE(IOUT,1148)MONVOL(K1)
            IF(IFV_TYPE == 11)WRITE(IOUT,1149)MONVOL(K1)
            IF (IFV_TYPE == 6 .OR. IFV_TYPE == 8 .OR. IFV_TYPE == 11) THEN
               WRITE(IOUT,1153)FVDATA_OLD(IFV)%CFL_COEF,FVDATA_OLD(IFV)%DTMIN,FVDATA_OLD(IFV)%ID_DT_OPTION
               IF(FVDATA_OLD(IFV)%ID_DT_OPTION ==2)THEN
                 WRITE(IOUT,1154)FVDATA_OLD(IFV)%L_TYPE,FVDATA_OLD(IFV)%LAMBDA,FVDATA_OLD(IFV)%DTOLD
               ENDIF
            ENDIF
            K1=K1+NIMV
         ENDDO
      ENDIF
C
      IF(ISPMD == 0.AND.MCHECK == 0.AND.IDT1SOL /= 0) WRITE(IOUT,'(A)')' OPTIMIZED TIME STEP COMPUTATION FOR HEPH.'
      IF(ISPMD == 0.AND.MCHECK == 0.AND.IDTTSH /= 0) WRITE(IOUT,'(A)')' OPTIMIZED (SHELL) TIME STEP FOR THICK SHELLS.'
      IF(ISPMD == 0.AND.MCHECK == 0)WRITE(IOUT,1110)NCPRI,NCRST,MULTIREST,INVERS,ITFOR0
      IF(ISPMD == 0.AND.RESTART_FILE == 0) WRITE(IOUT,*) CHECK_MESSAGE(2)(1:len_trim(CHECK_MESSAGE(2)) )
      IF (IMPL_S == 1.AND.IKG>=5) THEN
         IKG=IKG-5
       IF(ISPMD == 0) THEN
         WRITE(IOUT,*) ' ** WARNING ** : PARITH/ON IS NOT ','COMPATIBLE WITH IMPLICIT OPTION '
         WRITE(IOUT,*) ' ** RESETTING **  : PARITH/OFF  '
       ENDIF
      ENDIF
C-------------------------------------------
C     .h3d files
C-------------------------------------------
      CALL LECH3D(GEO,IGEO,IPM,IPART,H3D_DATA,MULTI_FVM,IPARI,IPARG,TAG_SKINS6,
     .            MDS_LABEL,MDS_OUTPUT_TABLE,MDS_NMAT,MAX_DEPVAR,MDS_NDEPSVAR,
     .            ELBUF_STR,STACK,IBCL,ILOADP,LLOADP,LOADS)
c
      IF(H3D_DATA%N_OUTP_H3D /= 0 .AND. ISPMD == 0)THEN
        WRITE(IOUT,5000)H3D_DATA%TH3D,H3D_DATA%DTH3D
        WRITE(IOUT,*)'       |'
        DO I=1,H3D_DATA%N_OUTP_H3D
          IF( H3D_DATA%OUTPUT_LIST(I)%ETYPE == 1 .OR.
     .        H3D_DATA%OUTPUT_LIST(I)%ETYPE == 2 .AND. NUMELCG+NUMELTGG > 0 .OR.
     .        H3D_DATA%OUTPUT_LIST(I)%ETYPE == 3 .AND. NUMELSG > 0 .OR.
     .        H3D_DATA%OUTPUT_LIST(I)%ETYPE == 4 .AND. NUMELTRG+NUMELPG+NUMELRG > 0 .OR.
     .        H3D_DATA%OUTPUT_LIST(I)%ETYPE == 5 .AND. NUMSPHG > 0 .OR.
     .        H3D_DATA%OUTPUT_LIST(I)%ETYPE == 6 .AND. NUMELQG > 0 .OR.
     .        H3D_DATA%OUTPUT_LIST(I)%ETYPE == 7 .AND. NUMSKING > 0) THEN
            CHAR1=' '
            IF(H3D_DATA%OUTPUT_LIST(I)%ETYPE == 1)THEN
              CHAR1='NODAL'
            ELSEIF(H3D_DATA%OUTPUT_LIST(I)%ETYPE == 2)THEN
              CHAR1='SHELL'
            ELSEIF(H3D_DATA%OUTPUT_LIST(I)%ETYPE == 3)THEN
              CHAR1='SOLID'
            ELSEIF(H3D_DATA%OUTPUT_LIST(I)%ETYPE == 4)THEN
              CHAR1='ONED'
            ELSEIF(H3D_DATA%OUTPUT_LIST(I)%ETYPE == 5)THEN
              CHAR1='SPH'
            ELSEIF(H3D_DATA%OUTPUT_LIST(I)%ETYPE == 6)THEN
              CHAR1='QUAD'
            ELSEIF(H3D_DATA%OUTPUT_LIST(I)%ETYPE == 7)THEN
              CHAR1='SKIN'
            ENDIF
c
            CHAR2=' '
            IF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 1)THEN
              CHAR2='SCALAR'
            ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 2)THEN
              CHAR2='VECTOR'
            ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 3)THEN
              CHAR2='TENSOR'
            ELSEIF(H3D_DATA%OUTPUT_LIST(I)%OUTP_TYPE == 4)THEN
              CHAR2='TORSOR'
            ENDIF
c
            STEXT1 = H3D_DATA%OUTPUT_LIST(I)%S_STRING1
            WRITE(IOUT,*)  '       |----'//CHAR1//' '//CHAR2//' : '
c            WRITE(IOUT,*)  '       |----'//H3D_DATA%OUTPUT_LIST(I)%KEYWORD
            WRITE(IOUT,*)  '       |    '//H3D_DATA%OUTPUT_LIST(I)%STRING1(1:STEXT1)

            IF (H3D_DATA%OUTPUT_LIST(I)%IUVAR > 0)
     .        WRITE(IOUT,*)  '       |        UVAR=', H3D_DATA%OUTPUT_LIST(I)%IUVAR

            IF (H3D_DATA%OUTPUT_LIST(I)%PLY > 0)
     .        WRITE(IOUT,*)  '       |        PLY=',H3D_DATA%OUTPUT_LIST(I)%PLY

            IF (H3D_DATA%OUTPUT_LIST(I)%LAYER > 0)
     .        WRITE(IOUT,*)  '       |        LAYER=',H3D_DATA%OUTPUT_LIST(I)%LAYER

            IF (H3D_DATA%OUTPUT_LIST(I)%IPT > 0)
     .        WRITE(IOUT,*)  '       |        IPT=',H3D_DATA%OUTPUT_LIST(I)%IPT

            IF (H3D_DATA%OUTPUT_LIST(I)%IR > 0)
     .        WRITE(IOUT,*)   '       |        IR=',H3D_DATA%OUTPUT_LIST(I)%IR

            IF (H3D_DATA%OUTPUT_LIST(I)%IR > 0)
     .        WRITE(IOUT,*)   '       |        IS=',H3D_DATA%OUTPUT_LIST(I)%IS

            IF (H3D_DATA%OUTPUT_LIST(I)%IT > 0)
     .        WRITE(IOUT,*)   '       |        IT=',H3D_DATA%OUTPUT_LIST(I)%IT

            WRITE(IOUT,*)'       |'
          ENDIF
        ENDDO
        WRITE(IOUT,*)' '
        WRITE(IOUT,*)' '
      ENDIF
C-------------------------------------------
C---------------------
C FE THERMAL ANALYSIS
C---------------------
       IF(ISPMD == 0 .AND. IMASSI > 0 ) THEN
         WRITE(IOUT,'(A)')' RESET INITIAL MASS FOR THIS RUN '
       ENDIF
C---------------------
C INITIAL MASS
C---------------------
       IF(ISPMD == 0 .AND. ITHERM_FE > 0 ) THEN
         WRITE(IOUT,'(A)')' FINITE ELEMENT THERMAL ANALYSIS '
         IF(IDT_THERM == 1) THEN
            WRITE(IOUT,'(A)')' THERMAL ANALYSIS ONLY'
            WRITE(IOUT,'(A,1X,G14.7)')' THERMAL TIME STEP SCALE FACTOR . . . . . . . . .', DTFACTHERM
         ENDIF
         IF(THEACCFACT > ONE ) THEN
           WRITE(IOUT,'(A,1X,G14.7/)')' FACTOR TO SPEED-UP THERMAL ANALYSIS. . . . . . .', THEACCFACT
         ENDIF
       ENDIF
C-------------------------------------------
C     Animations
C-------------------------------------------
      IF(TANIM0 /= ZERO)  TANIM = TANIM0
      IF(TANIM_STOP0/=ZERO .AND. TANIM_STOP0/=EP20)  TANIM_STOP = TANIM_STOP0
      IF(DTANIM0 > ZERO)  DTANIM= DTANIM0
      IF(DTANIM<=ZERO)  TANIM = EP30
      IF (TANIM < TT-DT2.AND.DTANIM > ZERO)TANIM = TANIM
     .    + INT((TT-DT2-TANIM)/DTANIM)*DTANIM
      IF (TANIM < TT-DT2)TANIM = TANIM+DTANIM
C-----------------------
      IF(ANIM_V(14)+H3D_DATA%N_VECT_DROT > 0 .AND.
     .      ((ISECUT == 0 .AND. IISROT == 0 .AND. IMPOSE_DR == 0 .AND. IDROT == 0) .OR. IRODDL == 0)) THEN
       IF(ISPMD == 0) THEN
         WRITE(IOUT,*) ' ** WARNING ** : /ANIM/DROT OPTION USED',
     .                 '    WHILE ROTATIONAL DOF ARE NOT COMPUTED',
     .                 '    (IDROT = 0 IN /IOFLAG OPTION)'
         WRITE(ISTDO,*) ' ** WARNING ** : /ANIM/DROT OPTION USED',
     .                 '    WHILE ROTATIONAL DOF ARE NOT COMPUTED',
     .                 '    (IDROT = 0 IN /IOFLAG OPTION)'
       ENDIF
       IF(ANIM_V(14) == 1) THEN
         ANIM_V(14) = 0
         NV_ANI = NV_ANI - 1
       ENDIF
      ENDIF
C-----------------------
      IF(ISPMD == 0.AND.MCHECK == 0) THEN
C-----------------------
C       Tracing Animations cards
C-----------------------
       WRITE(IOUT,1120)TANIM,DTANIM,TANIM_STOP,SENSORS%ANIM_ID,SENSORS%ANIM_DT,
     +       ANIM_E(1),ANIM_E(2),ANIM_E(3),ANIM_E(25),
     +       ANIM_E(4),ANIM_E(5),ANIM_E(6),
     +       ANIM_E(7),ANIM_E(8),ANIM_E(9),
     +       ANIM_E(10)+ANIM_E(4960)+ANIM_E(4961)+ANIM_E(4962)
        WRITE(IOUT,1129) ANIM_N(3),ANIM_N(4),ANIM_N(6)
       WRITE(IOUT,1130)
     +       ANIM_V(1),ANIM_V(2),ANIM_V(3),ANIM_V(4),ANIM_V(5),
     +       ANIM_V(6),ANIM_V(7),ANIM_V(9),ANIM_V(12),
     +       ANIM_T(1),ANIM_T(2),ANIM_T(3),ANIM_T(4)
       WRITE(IOUT,1140)
     +       ANIM_T(5),ANIM_T(6),ANIM_T(7),ANIM_T(8),
     +       ANIM_M,ANIM_K
       ENDIF
C-----------------------
C     The correct indexes in ANIM data structures wrt integration points or layers
C     of all actual elements will be rebuilt ::
C-----------------------
      CALL ANIM_BUILD_INDEX_ALL(ISPMD   ,MCHECK ,SENSORS ,IGEO ,GEO )
C-------------------------------------------
C     STOP/SENSOR
C-------------------------------------------
      IF (MCHECK == 0) THEN
        ALLOCATE(SENSORS%STOP(SENSORS%NSTOP))
        SENSORS%STOP(:) = 0
      END IF
      IF (ISPMD == 0 .AND. MCHECK == 0) THEN
        IF (SENSORS%NSTOP > 0) THEN
          DO K=1,SENSORS%NSTOP
            IERR = 1
            IF (SENSORS%STOP_TMP(K) > 0) THEN
              DO I=1,SENSORS%NSENSOR
                IF (SENSOR_TAB(I)%SENS_ID == SENSORS%STOP_TMP(K)) THEN
                  SENSORS%STOP(K) = I
                  IERR = 0
                  EXIT
                ENDIF
              ENDDO
            ENDIF
            IF (IERR == 1) THEN
             CALL ANCMSG(MSGID=233, ANMODE=ANINFO,I1=SENSORS%STOP_TMP(K))
            ENDIF
          ENDDO
        ENDIF
      END IF                    !(ISPMD==0.AND.MCHECK==0)
C-------------------------------------------
C     STATE/SENSOR
C-------------------------------------------
      IF (MCHECK == 0) THEN
        ALLOCATE(SENSORS%STAT(SENSORS%NSTAT))
        SENSORS%STAT(:) = 0
      END IF
      IF (ISPMD == 0 .AND. MCHECK == 0) THEN
        MSTATT = 0
        IF (SENSORS%NSTAT > 0) THEN
          DO K=1,SENSORS%NSTAT
            IERR = 1
            IF(SENSORS%STAT(K) /= 0) THEN
              DO I=1,SENSORS%NSENSOR
               IF (SENSORS%SENSOR_TAB(I)%SENS_ID == SENSORS%STAT(K)) THEN
                 SENSORS%STAT(K) = I
                 IERR = 0
                 EXIT
                ENDIF
              ENDDO
            ENDIF
            IF (IERR == 1) THEN
             CALL ANCMSG(MSGID=235, ANMODE=ANINFO,I1=SENSORS%STAT(K))
            ELSE
               MSTAT(K) = 0
            ENDIF
          ENDDO
        ENDIF
      END IF                    !(ISPMD==0.AND.MCHECK==0)
C-------------------------------------------
C     OUTP/SENSOR
C-------------------------------------------
      IF (MCHECK == 0) THEN
        ALLOCATE(SENSORS%OUTP(SENSORS%NOUTP))
        SENSORS%OUTP(:) = 0
      END IF
      IF (ISPMD == 0 .AND. MCHECK == 0) THEN
        IF (SENSORS%NOUTP > 0) THEN
          DO K=1,SENSORS%NOUTP
            IERR = 1
            IF (SENSORS%OUTP_TMP(K) > 0) THEN
              DO I=1,SENSORS%NSENSOR
               IF (SENSORS%SENSOR_TAB(I)%SENS_ID == SENSORS%OUTP_TMP(K)) THEN
                 SENSORS%OUTP(K) = I
                 IERR = 0
                 EXIT
                ENDIF
              ENDDO
            ENDIF
            IF (IERR == 1) THEN
             CALL ANCMSG(MSGID=236, ANMODE=ANINFO,I1 = SENSORS%OUTP_TMP(K))
            ENDIF
          ENDDO
        ENDIF
      END IF                    !(ISPMD==0.AND.MCHECK==0)
C-------------------------------------------
C     H3D/SENSOR
C-------------------------------------------
      IF (ISPMD == 0.AND.MCHECK == 0) THEN
        DO K=1,H3D_DATA%N_SENS_H3D
         IOK = 0
          IF(H3D_DATA%LSENS_H3D(K) /= 0)THEN
           DO I=1,SENSORS%NSENSOR
              IF(H3D_DATA%LSENS_H3D(K) == SENSOR_TAB(I)%SENS_ID)THEN
               H3D_DATA%LSENS_H3D(K)=I
               IOK = 1
               EXIT
             ENDIF
           ENDDO
         ENDIF
         IF(IOK == 0) THEN
           CALL ANCMSG(MSGID=283,ANMODE=ANINFO,I1=H3D_DATA%LSENS_H3D(K))
           CALL ARRET(2)
         ENDIF
       ENDDO
      ENDIF                     !(ISPMD==0.AND.MCHECK==0)
C-------------------------------------------
C     STATE/SENSOR
C-------------------------------------------
      IF(ISPMD == 0.AND.MCHECK == 0) THEN
       WRITE(IOUT,1150)DTIN,DTMX
       IF(IMPL /= 0)
     .   WRITE(IOUT,1160)EPS,EPS2,NITMX
       ISTATG_P = IABS(ISTATG)
       IF(ISTAT == 1)THEN
         WRITE(IOUT,1171)ISTATG_P,BETA,PERIOD
       ELSEIF(ISTAT == 2)THEN
        IF (TST_START>ZERO.OR.TST_STOP>ZERO) THEN
          IF (TST_STOP==ZERO) TST_STOP = TSTOP
          WRITE(IOUT,5001)ISTATG_P,TST_START,TST_STOP
        ELSE
          WRITE(IOUT,1172)ISTATG_P
        END IF
       ELSEIF(ISTAT == 3)THEN
        IF (TST_START>ZERO.OR.TST_STOP>ZERO) THEN
          IF (TST_STOP==ZERO) TST_STOP = TSTOP
          WRITE(IOUT,5011)ISTATG_P,TST_START,TST_STOP
        ELSE
          WRITE(IOUT,5010)ISTATG_P
        END IF
       ENDIF
C-----------------------
       IF(IPARIT == 0)THEN
          WRITE(IOUT,1180)
       ELSEIF(IPARIT == 1)THEN
         WRITE(IOUT,1181)
       ELSE
         WRITE(IOUT,1182)IPARIT-1
       ENDIF
C-----------------------
       WRITE(IOUT,1300) NSLIOF,NELSOF,NELQOF,NELCOF,NELTOF,NELPOF,NELROF,NELTGOF,NSPHOF
       WRITE(IOUT,1400) NRLINK
       WRITE(IOUT,1500) NUBCSN
       IF(IALE /= 0.OR.IEULER /= 0)WRITE(IOUT,1450) NALELINK+NALELK !format v12 + format v5
c
      ELSEIF(ISPMD == 0 .AND. MCHECK /= 0) THEN
      ENDIF                     !(ISPMD==0.AND.MCHECK==0)
C-----------------------------------
C     ALE/EULER : UPWIND
C----------------------------------------
         ! ALE%UPWIND%UPW_UPDATE == 1 : ENGINE /UPWIND CARD DETECTED
         !ALE%UPWIND%UPW_UPDATE == 2 : /UPWIND CARD IS CHANGING AT LEAST ONE PARAMETER
         !if /UPWIND is defined then set new parameters
      IF(ISPMD == 0 .AND. MCHECK == 0)THEN
        IF(IALE /= 0 .OR. IEULER /= 0)THEN
         IF(ALE%UPWIND%UPW_UPDATE /= 0 )THEN
           DO K=1,NUMMAT-1
             !ILAW=PM(19,K)
             JALE=INT(PM(72,K))  ! JALE from PROP (IGEO(62,IPID)) is not checked since this option /UPWIND is obsolete
             IF(JALE /= 0)THEN
               IF(PM(15,K) /= ALE%UPWIND%UPWMG2 .OR. PM(16,K) /= ALE%UPWIND%UPWOG2)THEN
                 ALE%UPWIND%UPW_UPDATE = 2
                 PM(15,K) = ALE%UPWIND%UPWMG2
                 PM(16,K) = ALE%UPWIND%UPWOG2
               ENDIF
            ENDIF
           ENDDO
           IF(ALE%UPWIND%UPWSM /= ALE%UPWIND%UPWSM2)THEN
             ALE%UPWIND%UPW_UPDATE = 2
             ALE%UPWIND%UPWSM = ALE%UPWIND%UPWSM2
           ENDIF
         !if /UPWIND is not defined then catch parameter from previous run
         ELSEIF(ALE%UPWIND%UPW_UPDATE == 0)THEN
           ALE%UPWIND%UPWMG2=ONE
           ALE%UPWIND%UPWOG2=ONE
           ALE%UPWIND%UPWSM2=ONE
           DO K=1,NUMMAT-1
             !ILAW=PM(19,K)
             JALE=INT(PM(72,K))
             IF(JALE /= 0)THEN
               IF(PM(15,K) /= ALE%UPWIND%UPWMG2 .OR. PM(16,K) /= ALE%UPWIND%UPWOG2 .OR. ALE%UPWIND%UPWSM /= ALE%UPWIND%UPWSM2)THEN
                 IF(PM(15,K) /= ZERO)ALE%UPWIND%UPWMG2 = PM(15,K)
                 IF(PM(16,K) /= ZERO)ALE%UPWIND%UPWOG2 = PM(16,K)
                 IF(ALE%UPWIND%UPWSM /= ZERO)ALE%UPWIND%UPWSM2 = ALE%UPWIND%UPWSM
                 EXIT
               ENDIF
            ENDIF
           ENDDO
         ENDIF
           ENDIF
         ENDIF
C-----------------------------------
C     ALE/EULER : OUTPUT
C----------------------------------------
      IF(IALE /= 0 .OR. IEULER /= 0)THEN
         !-----------------------
         !     QUASI-INCOMPRESSIBLE /INCMP
         !-----------------------
          IF(ALE%GLOBAL%INCOMP == 1.AND.MCHECK == 0) WRITE(IOUT,1196)
         !-----------------------
         !     GRID FORMULATION
         !-----------------------
         !---ALE GRID FORMULATION CHECK---!
         IF(ALE%GRID%NWALE_ENGINE /= ALE%GRID%NWALE_RST .AND. ALE%GRID%NWALE_ENGINE /= -1) THEN !Forbid ALE grid formulation switch
           IF(ALE%GRID%NWALE_ENGINE /= 3)THEN  !expect if new one is /ALE/ZERO
             ALEform=(/'DONEA    ','DISP     ','SPRING   ', 'ZERO     ','STANDARD ', 'LAPLACIAN' ,'VOLUME   ', 'MASSFLOW '/)
             ALEStrL=(/5,4,6,4,8,9,6,8/)
             Cale1=ALEform(ALE%GRID%NWALE_ENGINE)
             Cale2=ALEform(ALE%GRID%NWALE_RST)
              CALL ANCMSG(MSGID=229,ANMODE=ANINFO,
     .                    C1=Cale1(1:ALEStrL(ALE%GRID%NWALE_ENGINE)),C2=Cale2(1:ALEStrL(ALE%GRID%NWALE_RST)))
              CALL ARRET(2)
            ELSE
             ALE%GRID%NWALE_ENGINE = 3
            END IF
          END IF

         !--Labels for Staggered Scheme
         IF(ALE%UPWIND%UPWM == 2)THEN
            Label1='TG       '
         ELSEIF(ALE%UPWIND%UPWM == 3)THEN
            Label1='SUPG     '
         ELSE
            Label1='UPWIND   '
         ENDIF
         Label2='UPWIND   '
         Label3='UPWIND   '
         IF(ALEMUSCL_Param%IALEMUSCL == 0)THEN
           Label4='UPWIND   '
         ELSE
           Label4='MUSCL    '
         ENDIF
         !--Labels for Colocated Scheme
         IF(ALEMUSCL_Param%IALEMUSCL == 0)THEN
           Label5='1ST-ORDER'
           Label6='1ST-ORDER'
           Label7='1ST-ORDER'
           Label8='1ST-ORDER'
         ELSEIF(ALEMUSCL_Param%IALEMUSCL == 1)THEN
           Label5='2ND-ORDER'
           Label6='2ND-ORDER'
           Label7='2ND-ORDER'
           Label8='2ND-ORDER'
         ELSEIF(ALEMUSCL_Param%IALEMUSCL == 2)THEN
           Label5='1ST-ORDER'
           Label6='1ST-ORDER'
           Label7='1ST-ORDER'
           Label8='2ND-ORDER'
         ENDIF
         MULTI_FVM%LOWMACH_OPT = .FALSE.
         IF (LOWMACH_OPT == 1 .AND. MULTI_FVM%IS_USED) THEN
            MULTI_FVM%LOWMACH_OPT = .TRUE.
         ENDIF
         ALEform=(/'DONEA    ','DISP     ','SPRING   ','ZERO     ','STANDARD ','LAPLACIAN','VOLUME   ', 'MASSFLOW ' /)
         !effective value which will be retained during numerical solving
         IF(ALE%GRID%NWALE_ENGINE /= -1)THEN
           ALE%GRID%NWALE = ALE%GRID%NWALE_ENGINE
         ELSE
           ALE%GRID%NWALE = ALE%GRID%NWALE_RST
         ENDIF
         Label9(:)='         '
         Label9(1:9)=ALEform(ALE%GRID%NWALE)
         eta1=ALE%UPWIND%UPWMG2
         eta2=ALE%UPWIND%UPWOG2
         IF(ALE%UPWIND%UPWM>1)eta1=ALE%UPWIND%CUPWM
         !OUTPUT NUMERICAL SCHEME AND ITS PARAMETERS
         IF(ISPMD == 0 .AND. MCHECK == 0)THEN
           WRITE(IOUT,1001)
           WRITE(IOUT,1002)Label1,eta1,Label2,eta2,Label3,eta2,Label4
           IF(MULTI_FVM%IS_USED)THEN
             WRITE(IOUT,1003)Label5,Label6,Label7,Label8
             IF(MULTI_FVM%LOWMACH_OPT)WRITE(IOUT,1004)
             IF(ALEMUSCL_Param%IALEMUSCL/=0)WRITE(IOUT,1005)ALEMUSCL_Param%BETA
           ENDIF
           !OUTPUT COURANT NUMBER
           WRITE(IOUT,1006) DTFAC1(102),DTMIN1(102)
           !OUTPUT GRID SMOOTHING FORMULATION AND ITS PARAMETERS
           WRITE(IOUT,1007)Label9(1:9)
           SELECT CASE (ALE%GRID%NWALE)
             CASE(0) !DONEA
               WRITE(IOUT,1008)
               WRITE(IOUT,1200) ALE%GRID%ALPHA,ALE%GRID%GAMMA,ALE%GRID%VGX,ALE%GRID%VGY,ALE%GRID%VGZ,VOLMIN
             CASE(1) !DISP
               WRITE(IOUT,1008)
               WRITE(IOUT,1220) ALE%GRID%ALPHA,ALE%GRID%VGX,ALE%GRID%VGY,ALE%GRID%VGZ,VOLMIN
             CASE(2) !SPRING
               WRITE(IOUT,1008)
               WRITE(IOUT,1250) DT_INPUT,ALE%GRID%ALPHA,ALE%GRID%GAMMA,ALE%GRID%VGX,ALE%GRID%VGY,VOLMIN
             CASE(3) !ZERO
               !no parameters
             CASE(4) !STANDARD
               WRITE(IOUT,1008)
                WRITE(IOUT,1254) ALE%GRID%ALPHA,ALE%GRID%GAMMA,ALE%GRID%VGX,ALE%GRID%VGY
             CASE(5)! LAPLACIAN
               WRITE(IOUT,1008)
               WRITE(IOUT,1254) ALE%GRID%ALPHA,ALE%GRID%GAMMA,ALE%GRID%VGX,ALE%GRID%VGY
             CASE(6) !VOLUME
               !no parameters
             CASE(7) !MASSFLOW
               WRITE(IOUT,1008)
               WRITE(IOUT,1257) ALE%GRID%ALPHA,ALE%GRID%GAMMA
           END SELECT
         ENDIF
       ENDIF !(IALE/=0.OR.IEULER/=0)
          !---CHECK END-------------------!
!-----------------------
!     FEM MOMENTUM INTEGRATION
!-----------------------
      IF(ISPMD == 0.AND.MCHECK == 0)THEN
        IF(IALE+IEULER /= 0 .AND. ALE%GLOBAL%ISFINT /= 3)THEN
          SELECT CASE(ALE%GLOBAL%ISFINT)
            !CASE(3)
            ! WRITE(IOUT,1197)   !default
            CASE(2)
             WRITE(IOUT,1198)
            CASE(1)
             WRITE(IOUT,1199)
          END SELECT
         ENDIF
      ENDIF

C-------------------------------
C ELIMINATION PARTIE ALE EULER THERMIQUE
C-------------------------------
      IF(IALE /= 0.AND.NALEOF == 1)THEN
        IF(ISPMD == 0.AND.MCHECK == 0)THEN
          WRITE(IOUT,*)'ALE FORMULATION SWITCHED OFF '
        END IF
        IALE = 0
      END IF
      IF(IEULER /= 0.AND.NEULEROF == 1)THEN
        IF(ISPMD == 0.AND.MCHECK == 0)THEN
          WRITE(IOUT,*)'EULER FORMULATION SWITCHED OFF '
        END IF
        IEULER = 0
      END IF
      IF(ITHERM /= 0.AND.NTHERMOF == 1)THEN
        IF(ISPMD == 0.AND.MCHECK == 0)THEN
          WRITE(IOUT,*)'THERMIC FORMULATION SWITCHED OFF '
        END IF
        ITHERM = 0
      END IF
C-------------------------------
      IF(IDEL7 /= 0.AND.MCHECK == 0.AND.N2D == 0)THEN
        IDELI7 = IDEL7 - 1
        IF(ISPMD == 0)THEN
          WRITE(IOUT,1550) IDELI7
          CALL ANCMSG(MSGID=122,ANMODE=ANINFO_BLIND)
          CALL ARRET(2)
        ENDIF
      ELSEIF(N2D /= 0.AND.MCHECK == 0)THEN
        IF(ISPMD == 0)WRITE(IOUT,1550) IDEL7-1
      END IF
C-------------------------------
C     INTERFACE CLEANING
C-------------------------------
      IF(NSLIOF /= 0) THEN
C
       NN=(NSLIOF+9)/10
       DO IL=1,NN
         READ (IIN,'(10I10)') (NLEC(I),I=1,10)
         DO 80 I=1,10
          IF(NLEC(I) == 0)GOTO 80
          IF(ISPMD == 0.AND.MCHECK == 0)
     .           WRITE(IOUT,2000) NLEC(I)
               DO K=1,NINTER
                 IF(IPARI(15,K) == NLEC(I))THEN
                   IF(IPARI(7,K)==2)THEN
                     IDEL2= IPARI(17,K)
                     IF(IDEL2 /= 0)THEN
C
C                      SMASS & SINER have been saved only if IDEL2 /= 0
                       NSN  = IPARI(5,K)
                       DO N=1,NSN
                         IS =INTBUF_TAB(K)%NSV(N)
                         IF(IS > 0)THEN
                           MS(IS)=INTBUF_TAB(K)%SMAS(N)
                           IN(IS)=INTBUF_TAB(K)%SINER(N)
                           INTBUF_TAB(K)%NSV(N)=-IS
                         END IF
                       END DO
                     END IF
                   END IF
C----- change T_stop for int25 otherwise issue w/ int25
                   IF(IPARI(7,K)==25)THEN
                     INTBUF_TAB(K)%VARIABLES(11) = ZERO
                   ELSE
                   IPARI(7,K)=0
                 ENDIF
                 END IF
               ENDDO
   80    CONTINUE
       ENDDO
      ENDIF
C-----------------------------------
C     CLEANING INTERFACE NODES
C----------------------------------------
      IF(NSLIOFN /= 0) THEN
       READ (IIN,'(2I8)') I,NN
       KK=0
       DO K=1,NINTER
         IF(IPARI(15,K) == I)KK=K
       ENDDO
       IF(KK == 0)THEN
        IF(ISPMD == 0) THEN
          CALL ANCMSG(MSGID=123,ANMODE=ANINFO_BLIND)
        ENDIF
        CALL ARRET(2)
       ENDIF
       NRTS  =IPARI(3,KK)
       NRTM  =IPARI(3,KK)
       NSN   =IPARI(5,KK)
       NMN   =IPARI(6,KK)
       NTY   =IPARI(7,KK)
       IF (IPARI(71,KK)/=0) NTY = 19   ! Interface type 19
       IF(NTY == 3)THEN
         NSN   =NSN + NMN
       ELSEIF(NTY == 4 .OR. NTY == 5)THEN
       ELSEIF(NTY == 7 .OR. NTY == 10 .OR. NTY == 22)THEN
       ELSE
        IF(ISPMD == 0) THEN
          CALL ANCMSG(MSGID=124,ANMODE=ANINFO_BLIND)
        ENDIF
        CALL ARRET(2)
       ENDIF
       DO IL=1,NN
         READ (IIN,'(10I10)') (NLEC(I),I=1,10)
         DO I=1,10
          IF(NLEC(I) /= 0)THEN
            DO K=1,NSN
              IF(ITAB(INTBUF_TAB(KK)%NSV(K)) == NLEC(I))THEN !NSV(K)
                INTBUF_TAB(KK)%STFNS(K)=ZERO !STFNS(K)
              ENDIF
            ENDDO
          ENDIF
         ENDDO
       ENDDO
      ENDIF
C----------------------------------------------
C     CLEANING ELEMS
C----------------------------------------------
      NELOF=NELSOF+NELQOF+NELCOF+NELTOF+NELPOF+NELROF+NELTGOF+NSPHOF
C-----------------------
C     1. 3D ELEMS
C-----------------------
      IF(NELSOF > 0)THEN
        IF(ISPMD == 0.AND.MCHECK == 0)WRITE (IOUT,1810)
        NN=(NELSOF+4)/5
        NBC=5
        DO IL=1,NN
          READ (IIN,'(10I10)') (NLEC(II),II=1,10)
          DO J=1,NBC
            KLG=NLEC(2*J-1)
            KUG=NLEC(2*J  )
            IF(KLG<=0) GO TO 120
            NBLK=(IL-1)*NBC+J
            DO L=1,NUMELS
              IF(IXS(NIXS,L)>=KLG.AND.IXS(NIXS,L)<=KUG) THEN
                DO K=KLG,KUG
                  IF(IXS(NIXS,L) == K) THEN
                    IXS(1,L)=-IABS(IXS(1,L))
                    GOTO 111
                  ENDIF
                ENDDO
              ENDIF
111           CONTINUE
            ENDDO
          ENDDO
        ENDDO
  120   CONTINUE
        K=0
        DO J=1,NUMELS
          IF(IXS(1,J) < 0)THEN
            K=K+1
            NLEC(K) = IXS(NIXS,J)
            IF(K == 10)THEN
              IF(MCHECK == 0)WRITE (IOUT,'(5I10)') (NLEC(II),II=1,K)
              K=0
            ENDIF
          ENDIF
        ENDDO
        IF(K > 0) THEN
          IF(MCHECK == 0)WRITE (IOUT,'(5I10)') (NLEC(II),II=1,K)
        ENDIF
        IF(NSPMD > 1) THEN
          ! required treatments to retrieve delted elems in correct order
          IWIOUT = 0
          IF (ISPMD /= 0) CALL SPMD_CHKW(IWIOUT,IOUT)
          CALL SPMD_GLOB_ISUM9(IWIOUT,1)
          CALL SPMD_IBCAST(IWIOUT,IWIOUT,1,1,0,2)
          IF (IWIOUT > 0) THEN
            CALL SPMD_WIOUT(IOUT,IWIOUT)
            IWIOUT = 0
          ENDIF
        ENDIF
      ENDIF!(NELSOF > 0)
C-----------------------
C     2. 2D-ELEMS
C-----------------------
      IF(NELQOF > 0)THEN
        IF(ISPMD == 0.AND.MCHECK == 0)WRITE (IOUT,1820)
       IF (INVERS < 18) THEN
         NN=(NELQOF+7)/8
         NBC=8
       ELSE
         NN=(NELQOF+4)/5
         NBC=5
       ENDIF
       DO I=1,NN
            READ (IIN,'(10I10)') (NLEC(II),II=1,10)
            DO J=1,NBC
                KLG=NLEC(2*J-1)
                KUG=NLEC(2*J  )
                IF(KLG<=0) GO TO 220
                NBLK=(I-1)*NBC+J
                DO L=1,NUMELQ
                    IF(IXQ(NIXQ,L)>=KLG.AND.IXQ(NIXQ,L)<=KUG) THEN
                        DO K=KLG,KUG
                            IF(IXQ(NIXQ,L) == K) THEN
                                IXQ(1,L)=-IABS(IXQ(1,L))
                                GOTO 211
                            ENDIF
                        ENDDO
                    ENDIF
  211               CONTINUE
                ENDDO
            ENDDO
       ENDDO
  220  CONTINUE
       K=0
       DO J=1,NUMELQ
         IF(IXQ(1,J) < 0)THEN
           K=K+1
           NLEC(K) = IXQ(NIXQ,J)
           IF(K == 10)THEN
           IF(MCHECK == 0)WRITE (IOUT,'(5I10)') (NLEC(II),II=1,K)
             K=0
           ENDIF
         ENDIF
       ENDDO
       IF(K > 0) THEN
         IF(MCHECK == 0)WRITE (IOUT,'(5I10)') (NLEC(II),II=1,K)
       ENDIF
       IF(NSPMD > 1) THEN
          ! required treatments to retrieve delted elems in correct order
         IWIOUT = 0
         IF (ISPMD /= 0) CALL SPMD_CHKW(IWIOUT,IOUT)
         CALL SPMD_GLOB_ISUM9(IWIOUT,1)
         CALL SPMD_IBCAST(IWIOUT,IWIOUT,1,1,0,2)
         IF (IWIOUT > 0) THEN
           CALL SPMD_WIOUT(IOUT,IWIOUT)
           IWIOUT = 0
         ENDIF
       ENDIF
      END IF
C-----------------------
C     3. SHELL ELEMS
C-----------------------
      IF(NELCOF > 0)THEN
        IF(ISPMD == 0.AND.MCHECK == 0)WRITE (IOUT,1830)
         NN=(NELCOF+4)/5
         NBC=5
       DO I=1,NN
            READ (IIN,'(10I10)') (NLEC(II),II=1,10)
            DO J=1,NBC
                KLG=NLEC(2*J-1)
                KUG=NLEC(2*J  )
                IF(KLG<=0) GO TO 320
                NBLK=(I-1)*NBC+J
                DO L=1,NUMELC
                    IF(IXC(NIXC,L)>=KLG.AND.IXC(NIXC,L)<=KUG) THEN
                        DO K=KLG,KUG
                            IF(IXC(NIXC,L) == K) THEN
                                IXC(1,L)=-IABS(IXC(1,L))
                                GOTO 311
                            ENDIF
                        ENDDO
                    ENDIF
  311               CONTINUE
                ENDDO
            ENDDO
       ENDDO
  320  CONTINUE
       K=0
       DO J=1,NUMELC
         IF(IXC(1,J) < 0)THEN
           K=K+1
           NLEC(K) = IXC(NIXC,J)
           IF(K == 10)THEN
             IF(MCHECK == 0)WRITE (IOUT,'(5I10)') (NLEC(II),II=1,K)
             K=0
           ENDIF
         ENDIF
       ENDDO
       IF(K > 0) THEN
         IF(MCHECK == 0)WRITE (IOUT,'(5I10)') (NLEC(II),II=1,K)
       ENDIF
       IF(NSPMD > 1) THEN
          ! required treatments to retrieve delted elems in correct order
         IWIOUT = 0
         IF (ISPMD /= 0) CALL SPMD_CHKW(IWIOUT,IOUT)
         CALL SPMD_GLOB_ISUM9(IWIOUT,1)
         CALL SPMD_IBCAST(IWIOUT,IWIOUT,1,1,0,2)
         IF (IWIOUT > 0) THEN
           CALL SPMD_WIOUT(IOUT,IWIOUT)
           IWIOUT = 0
         ENDIF
       ENDIF
      END IF
C-----------------------
C     4. ROD ELEMS
C-----------------------
      IF(NELTOF > 0)THEN
        IF(ISPMD == 0.AND.MCHECK == 0)WRITE (IOUT,1840)
         NN=(NELTOF+4)/5
         NBC=5
       DO I=1,NN
            READ (IIN,'(10I10)') (NLEC(II),II=1,10)
            DO J=1,NBC
                KLG=NLEC(2*J-1)
                KUG=NLEC(2*J  )
                IF(KLG<=0) GO TO 420
                NBLK=(I-1)*NBC+J
                DO L=1,NUMELT
                    IF(IXT(NIXT,L)>=KLG.AND.IXT(NIXT,L)<=KUG) THEN
                        DO K=KLG,KUG
                            IF(IXT(NIXT,L) == K) THEN
                                IXT(1,L)=-IABS(IXT(1,L))
                                GOTO 411
                            ENDIF
                        ENDDO
                    ENDIF
  411               CONTINUE
                ENDDO
            ENDDO
       ENDDO
  420  CONTINUE
       K=0
       DO J=1,NUMELT
         IF(IXT(1,J) < 0)THEN
           K=K+1
           NLEC(K) = IXT(NIXT,J)
           IF(K == 10)THEN
             IF(MCHECK == 0)WRITE (IOUT,'(5I10)') (NLEC(II),II=1,K)
             K=0
           ENDIF
         ENDIF
       ENDDO
       IF(K > 0) THEN
         IF(MCHECK == 0) WRITE (IOUT,'(5I10)') (NLEC(II),II=1,K)
       ENDIF
       IF(NSPMD > 1) THEN
          ! required treatments to retrieve delted elems in correct order
         IWIOUT = 0
         IF (ISPMD /= 0) CALL SPMD_CHKW(IWIOUT,IOUT)
         CALL SPMD_GLOB_ISUM9(IWIOUT,1)
         CALL SPMD_IBCAST(IWIOUT,IWIOUT,1,1,0,2)
         IF (IWIOUT > 0) THEN
           CALL SPMD_WIOUT(IOUT,IWIOUT)
           IWIOUT = 0
         ENDIF
       ENDIF
      END IF
C-----------------------
C     5. BEAM ELEMS
C-----------------------
      IF(NELPOF > 0)THEN
        IF(ISPMD == 0.AND.MCHECK == 0)WRITE (IOUT,1850)
       NN=(NELPOF+4)/5
       NBC=5
       DO I=1,NN
            READ (IIN,'(10I10)') (NLEC(II),II=1,10)
            DO J=1,NBC
                KLG=NLEC(2*J-1)
                KUG=NLEC(2*J  )
                IF(KLG<=0) GO TO 520
                NBLK=(I-1)*NBC+J
                DO L=1,NUMELP
                    IF(IXP(NIXP,L)>=KLG.AND.IXP(NIXP,L)<=KUG) THEN
                        DO K=KLG,KUG
                            IF(IXP(NIXP,L) == K) THEN
                                IXP(1,L)=-IABS(IXP(1,L))
                                GOTO 511
                            ENDIF
                        ENDDO
                    ENDIF
  511               CONTINUE
                ENDDO
            ENDDO
       ENDDO
  520  CONTINUE
       K=0
       DO J=1,NUMELP
         IF(IXP(1,J) < 0)THEN
           K=K+1
           NLEC(K) = IXP(NIXP,J)
           IF(K == 10)THEN
             IF(MCHECK == 0)WRITE (IOUT,'(5I10)') (NLEC(II),II=1,K)
             K=0
           ENDIF
         ENDIF
       ENDDO
       IF(K > 0) THEN
         IF(MCHECK == 0)WRITE (IOUT,'(5I10)') (NLEC(II),II=1,K)
       ENDIF
       IF(NSPMD > 1) THEN
          ! required treatments to retrieve delted elems in correct order
         IWIOUT = 0
         IF (ISPMD /= 0) CALL SPMD_CHKW(IWIOUT,IOUT)
         CALL SPMD_GLOB_ISUM9(IWIOUT,1)
         CALL SPMD_IBCAST(IWIOUT,IWIOUT,1,1,0,2)
         IF (IWIOUT > 0) THEN
           CALL SPMD_WIOUT(IOUT,IWIOUT)
           IWIOUT = 0
         ENDIF
       ENDIF
      END IF
C-----------------------
C     6. SPRING ELEMS
C-----------------------
      IF(NELROF > 0)THEN
        IF(ISPMD == 0.AND.MCHECK == 0)WRITE (IOUT,1860)
       NN=(NELROF+4)/5
       NBC=5
       DO I=1,NN
            READ (IIN,'(10I10)') (NLEC(II),II=1,10)
            DO J=1,NBC
                KLG=NLEC(2*J-1)
                KUG=NLEC(2*J  )
                IF(KLG<=0) GO TO 620
                NBLK=(I-1)*NBC+J
                DO L=1,NUMELR
                    IF(IXR(NIXR,L)>=KLG.AND.IXR(NIXR,L)<=KUG) THEN
                        DO K=KLG,KUG
                            IF(IXR(NIXR,L) == K) THEN
                                IXR(1,L)=-IABS(IXR(1,L))
                                GOTO 611
                            ENDIF
                        ENDDO
                    ENDIF
  611               CONTINUE
                ENDDO
            ENDDO
       ENDDO
  620  CONTINUE
       K=0
       DO J=1,NUMELR
         IF(IXR(1,J) < 0)THEN
           K=K+1
           NLEC(K) = IXR(NIXR,J)
           IF(K == 10)THEN
             IF(MCHECK == 0)WRITE (IOUT,'(5I10)') (NLEC(II),II=1,K)
             K=0
           ENDIF
         ENDIF
       ENDDO
       IF(K > 0) THEN
         IF(MCHECK == 0) WRITE (IOUT,'(5I10)') (NLEC(II),II=1,K)
       ENDIF
       IF(NSPMD > 1) THEN
          ! required treatments to retrieve delted elems in correct order
         IWIOUT = 0
         IF (ISPMD /= 0) CALL SPMD_CHKW(IWIOUT,IOUT)
         CALL SPMD_GLOB_ISUM9(IWIOUT,1)
         CALL SPMD_IBCAST(IWIOUT,IWIOUT,1,1,0,2)
         IF (IWIOUT > 0) THEN
           CALL SPMD_WIOUT(IOUT,IWIOUT)
           IWIOUT = 0
         ENDIF
       ENDIF
      ENDIF
C-----------------------
C     7. 3N-SHELL-ELEMS
C-----------------------
      IF(NELTGOF > 0)THEN
        IF(ISPMD == 0.AND.MCHECK == 0)WRITE (IOUT,1870)
         NN=(NELTGOF+4)/5
         NBC=5
       DO I=1,NN
            READ (IIN,'(10I10)') (NLEC(II),II=1,10)
            DO J=1,NBC
                KLG=NLEC(2*J-1)
                KUG=NLEC(2*J  )
                IF(KLG<=0) GO TO 640
                NBLK=(I-1)*NBC+J
                DO L=1,NUMELTG
                    IF(IXTG(NIXTG,L)>=KLG.AND.IXTG(NIXTG,L)<=KUG) THEN
                        DO K=KLG,KUG
                            IF(IXTG(NIXTG,L) == K) THEN
                                IXTG(1,L)=-IABS(IXTG(1,L))
                                GOTO 631
                            ENDIF
                        ENDDO
                    ENDIF
  631               CONTINUE
                ENDDO
            ENDDO
       ENDDO
  640  CONTINUE
       K=0
       DO J=1,NUMELTG
         IF(IXTG(1,J) < 0)THEN
           K=K+1
           NLEC(K) = IXTG(NIXTG,J)
           IF(K == 10)THEN
             IF(MCHECK == 0)WRITE (IOUT,'(5I10)') (NLEC(II),II=1,K)
             K=0
           ENDIF
         ENDIF
       ENDDO
       IF(K > 0) THEN
         IF(MCHECK == 0) WRITE (IOUT,'(5I10)') (NLEC(II),II=1,K)
       ENDIF
       IF(NSPMD > 1) THEN
          ! required treatments to retrieve delted elems in correct order
         IWIOUT = 0
         IF (ISPMD /= 0) CALL SPMD_CHKW(IWIOUT,IOUT)
         CALL SPMD_GLOB_ISUM9(IWIOUT,1)
         CALL SPMD_IBCAST(IWIOUT,IWIOUT,1,1,0,2)
         IF (IWIOUT > 0) THEN
           CALL SPMD_WIOUT(IOUT,IWIOUT)
           IWIOUT = 0
         ENDIF
       ENDIF
C
      END IF
C-----------------------
C     8. SPH PARTICLES
C-----------------------
      IF(NSPHOF > 0)THEN
        IF(ISPMD == 0.AND.MCHECK == 0)WRITE (IOUT,1880)
         NN=(NSPHOF+4)/5
         NBC=5
       K=0
       DO I=1,NN
            READ (IIN,'(10I10)') (NLEC(II),II=1,10)
            DO J=1,NBC
                KLG=NLEC(2*J-1)
                KUG=NLEC(2*J  )
                IF(KLG<=0) GO TO 732
                NBLK=(I-1)*NBC+J
                DO L=1,NUMSPH
                    IF(KXSP(NISP,L)>=KLG.AND.KXSP(NISP,L)<=KUG) THEN
C           number of the related group :
                        NG =MOD(ABS(KXSP(2,L)),NGROUP+1)
                        IF(NG /= 0)THEN
                            NFT=IPARG(3,NG)
                            IAD=IPARG(4,NG)
                            II=L-NFT
                            ELBUF_STR(NG)%GBUF%OFF(II) = ZERO
                            KXSP(2,L) = 0
                            K=K+1
                            NLECSPH(K) = KXSP(NISP,L)
                            IF(K == 10)THEN
                                IF(MCHECK == 0) WRITE (IOUT,'(5I10)') (NLECSPH(II),II=1,K)
                                K=0
                            END IF
                        END IF
                    ENDIF
                ENDDO
            ENDDO
       ENDDO
  732  CONTINUE
       IF(K > 0) THEN
         IF(MCHECK == 0)WRITE (IOUT,'(5I10)') (NLECSPH(II),II=1,K)
       END IF
       IF(NSPMD > 1) THEN
          ! required treatments to retrieve delted elems in correct order
         IWIOUT = 0
         IF (ISPMD /= 0) CALL SPMD_CHKW(IWIOUT,IOUT)
         CALL SPMD_GLOB_ISUM9(IWIOUT,1)
         CALL SPMD_IBCAST(IWIOUT,IWIOUT,1,1,0,2)
         IF (IWIOUT > 0) THEN
           CALL SPMD_WIOUT(IOUT,IWIOUT)
           IWIOUT = 0
         ENDIF
       ENDIF
C
      END IF

C-------------------------------
C     Output by interface :
C        1 - Skid lines
C-------------------------------
      IF(H3D_DATA%N_SCAL_SKID  > 0.AND.NINTSKIDOLD==0) THEN
         NINTERSKID = H3D_DATA%N_SCAL_SKID
         IF(NINTSTAMP/=0) THEN
            ALLOCATE (PSKIDS(NINTERSKID,NUMNODG))
            PSKIDS(1:NINTERSKID,1:NUMNODG) = ZERO
         ELSE
            ALLOCATE (PSKIDS(NINTERSKID,NUMNOD))
            PSKIDS(1:NINTERSKID,1:NUMNOD) = ZERO
         ENDIF
      ELSEIF(H3D_DATA%N_SCAL_SKID  == 0.AND.NINTSKIDOLD==0)THEN
         ALLOCATE (PSKIDS(0,0))
      ENDIF
C-------------------------------------------
C        2 - Frictionnal energy: allocs tabs
C--------------------------------------------
      IF(H3D_DATA%N_SCAL_CSE_FRICINT  > 0.AND.S_EFRICINT==0) THEN
C
         NINEFRICG = H3D_DATA%N_SCAL_CSE_FRICINT
         NINEFRIC_STAMP = 0
         NINEFRIC = 0
         IF(NINTSTAMP/=0) THEN
           DO N=1,NINTER
               NI = H3D_DATA%N_CSE_FRIC_INTER (N)
               IF(NI/= 0.AND.IPARI(7,N)==21) NINEFRIC_STAMP = NINEFRIC_STAMP + 1
           ENDDO
         ENDIF
         IF(NINEFRIC_STAMP==NINEFRICG) THEN
            CALL MY_ALLOC(EFRIC_STAMP,NINEFRICG,NUMNODG)
            EFRIC_STAMP(1:NINEFRICG,1:NUMNODG) = ZERO
            CALL MY_ALLOC(EFRIC,0,0)
         ELSEIF(NINEFRIC_STAMP==0) THEN
            NINEFRIC = NINEFRICG
            CALL MY_ALLOC(EFRIC,NINEFRIC,NUMNOD)
            EFRIC(1:NINEFRICG,1:NUMNOD) = ZERO
            CALL MY_ALLOC(EFRIC_STAMP,0,0)
         ELSE
            NINEFRIC = NINEFRICG-NINEFRIC_STAMP
            CALL MY_ALLOC(EFRIC_STAMP,NINEFRIC_STAMP,NUMNODG)
            EFRIC_STAMP(1:NINEFRIC_STAMP,1:NUMNODG) = ZERO
            CALL MY_ALLOC(EFRIC,NINEFRIC,NUMNOD)
            EFRIC(1:NINEFRIC,1:NUMNOD) = ZERO
            NS = 0
            NN= 0
            DO N=1,NINTER
               NI = H3D_DATA%N_CSE_FRIC_INTER (N)
               IF(NI/= 0.AND.IPARI(7,N)==21) THEN
                  NS = NS+1
                  H3D_DATA%N_CSE_FRIC_INTER (N) = NINEFRIC + NS
               ELSEIF (NI/=0) THEN
                  NN = NN+1
                  H3D_DATA%N_CSE_FRIC_INTER (N) = NN
               ENDIF
            ENDDO
         ENDIF
      ELSEIF(S_EFRICINT==0)THEN
            CALL MY_ALLOC(EFRIC,0,0)
            CALL MY_ALLOC(EFRIC_STAMP,0,0)
      ENDIF
C-----------------------------------

C-----------------------
C     RESET OFF ARRAY
C-----------------------
      IF (NELOF > 0)THEN
       DO 790 NG=1,NGROUP
       OFFG => ELBUF_STR(NG)%GBUF%OFF
       MLW=IPARG(1,NG)
       ITY=IPARG(5,NG)
       NEL=IPARG(2,NG)
       NFT=IPARG(3,NG)
       IAD=IPARG(4,NG)
       IGOF=0
C-----------------------
C     1. SOLID ELEMS
C-----------------------
       IF(ITY == 1.AND.NELSOF /= 0)THEN
        IGOF=1
        DO 710 I=1,NEL
        II=I+NFT
        IF(IXS(1,II) < 0)THEN
         IXS(1,II)=-IXS(1,II)
         IF (MLW /= 0) THEN
           OFFG(I)=ZERO
         ELSE     ! loi0, pas de off
           CALL ANCMSG(MSGID=238,ANMODE=ANINFO_BLIND,I1=IXS(NIXS,II),C1='BRICK',C2='BRICK')
         ENDIF
        ELSE
         IGOF=0
        ENDIF
  710   CONTINUE
C-----------------------
C     2. 2D ELEMS
C-----------------------
       ELSEIF(ITY == 2.AND.NELQOF /= 0)THEN
        IGOF=1
        DO 720 I=1,NEL
        II=I+NFT
        IF(IXQ(1,II) < 0)THEN
         IXQ(1,II)=-IXQ(1,II)
         OFFG(I) = ZERO
        ELSE
         IGOF=0
        ENDIF
  720   CONTINUE
C-----------------------
C     3. SHELL ELEMS
C-----------------------
       ELSEIF(ITY == 3.AND.NELCOF /= 0)THEN
        IGOF=1
        DO I=1,NEL
        II=I+NFT
        IF(IXC(1,II) < 0)THEN
          IXC(1,II)=-IXC(1,II)
          IF (MLW /= 0) THEN
            OFFG(I) = ZERO
          ELSE     ! loi0, pas de off
           CALL ANCMSG(MSGID=238,ANMODE=ANINFO_BLIND,I1=IXC(NIXC,II), C1='SHELL',C2='SHELL')
          ENDIF
        ELSE
          IGOF=0
        ENDIF
        ENDDO
C-----------------------
C     4. ROD ELEMS
C-----------------------
       ELSEIF(ITY == 4.AND.NELTOF /= 0)THEN
C        IGOF=1
C suppression du groupe incompatible avec option gap truss
        IGOF=0
        DO 740 I=1,NEL
        II=I+NFT
        IF(IXT(1,II) < 0)THEN
         IXT(1,II)=-IXT(1,II)
         OFFG(I) = ZERO
C        ELSE
C         IGOF=0
        ENDIF
  740   CONTINUE
C-----------------------
C     5. BEAM ELEMS
C-----------------------
       ELSEIF(ITY == 5.AND.NELPOF /= 0)THEN
        IGOF=1
        DO 750 I=1,NEL
        II=I+NFT
        IF(IXP(1,II) < 0)THEN
         IXP(1,II)=-IXP(1,II)
         OFFG(I) = ZERO
        ELSE
         IGOF=0
        ENDIF
  750   CONTINUE
C-----------------------
C     6. ELEMENTS RESSORTS
C-----------------------
       ELSEIF(ITY == 6.AND.NELROF /= 0)THEN
        IGOF=0
        DO 760 I=1,NEL
        II=I+NFT
        IF(IXR(1,II) < 0)THEN
         IXR(1,II)=-IXR(1,II)
         OFFG(I) = ZERO
        ENDIF
  760   CONTINUE
C-----------------------
C     7. 3N-SHELL-ELEMS
C-----------------------
       ELSEIF(ITY == 7.AND.NELTGOF /= 0)THEN
        IGOF=1
        DO 770 I=1,NEL
        II=I+NFT
        IF(IXTG(1,II) < 0)THEN
         IXTG(1,II)=-IXTG(1,II)
         IF(MLW /= 0) THEN
           OFFG(I) = ZERO
         ELSE     ! loi0, pas de off
          CALL ANCMSG(MSGID=238,ANMODE=ANINFO_BLIND,I1=IXTG(NIXTG,II), C1='SH_3N',C2='SH_3N')
         ENDIF
        ELSE
         IGOF=0
        ENDIF
  770   CONTINUE
C-----------------------
C     8. SPH PARTICLES
C-----------------------
       ELSEIF(ITY == 51.AND.NSPHOF /= 0)THEN
        IGOF=1
        DO 780 I=1,NEL
        II=I+NFT
        IF(KXSP(2,II) /= 0)THEN
         IGOF=0
        ENDIF
  780   CONTINUE
       ENDIF
C----------------------------------------
C     CHECK FOR GROUP CLEANING
C----------------------------------------
       IPARG(8,NG)=MAX0(IPARG(8,NG),IGOF)
  790  CONTINUE
      END IF
C-------------------------------------------
C     ADDING RIGID LINK BETWEEN NODES
C-------------------------------------------
      IF(NRLINK > 0.AND.MCHECK == 0) THEN
       K1 =1
       K2 =1
       DO 800 K=1,NRLINK
       READ (IIN,'(I8,1X,3I1,1X,3I1,I10)')N,I1,I2,I3,IR1,IR2,IR3,ISK
       READ (IIN,'(10I10)')   (LLINK(K2+I-1),I=1,N)
       IC=I3+2*I2+4*I1
       ICR=IR3+2*IR2+4*IR1
       IF(IRODDL == 0)ICR=0
       IF(ISPMD == 0.AND.MCHECK == 0)WRITE(IOUT,2100)  K,I1,I2,I3,IR1,IR2,IR3,ISK,N
       IF(ISPMD == 0.AND.MCHECK == 0)WRITE(IOUT,'(10I10)')  (LLINK(K2+I-1),I=1,N)
       IF ( ISK  /=  0) THEN
         ISK1 = 0
         ISK2 = 0
         DO LL=0,NUMSKW
           IF(ISK == ISKWN(4,LL+1)) THEN
             ISK1 = LL
             ISK2 = 1
           ENDIF
         ENDDO
         IF ( ISK2  ==  0) THEN
           CALL ANCMSG(MSGID=125,ANMODE=ANINFO)
           CALL ARRET(2)
         ENDIF
         ISK = ISK1
       ENDIF
       ! shared verification & defining specific data structure 
       CALL FR_RLINK1(LLINK(K2),ITABM1,FR_RL(1,K),N)
       CALL RLINK0(
     1    V           ,VR          ,MS       ,IN           ,NNLINK(K1),
     2    NNLINK(K1+1),NNLINK(K1+2),LLINK(K2),N            ,IC        ,
     3    ICR         ,NNLINK(K1+3),ISK      ,SKEW(1,ISK+1),ISKWN     ,
     4    FR_RL(1,K)  ,WEIGHT      )
       K1=K1+4
       K2=K2+N
       IF(IC+ICR == 0)THEN
        IF(ISPMD == 0)CALL ANCMSG(MSGID=126,ANMODE=ANINFO)
        CALL ARRET(2)
       ENDIF
  800  CONTINUE
      ENDIF
C-------------------------------------------
C     ALE - LINK ON GRID VELOCITY
C     format v5 ('/VEL/*')
C-------------------------------------------
C
C                  |ALE LINK 1                                       |2              |NALELK
C             +----+----+----+----+----+----+----+----+----+...-+----+----+----+--...+----+----+--...
C   LINALE    |uID | M1 | M2 | N  | IC | IM |id1 |id2 |id3 |... |idn | ...           |
C             +----+----+----+----+----+----+----+----+----+...-+----+----+----+--...+----+----+--...
C (1:SLINALE)   1    2    3    4    5    6   6+1                 6+N
C    +LLINAL               |
C                         (N=1 if grnod_id is used)
C
C    First subarrays are used to define ale links from starter (1:SLINALE). Engine links are in (SLINALE+1:SLINALE+LLINAL)
C    Only Starter part is written in restart files.
C
C             M1 : MAIN node 1
C             M2 : MAIN node 2
C             N  : number of nodes
C             IC : dir XYZ
C             IM : formulation option -1,0,1
C             id*: node_id list or grnod_id
C
C      N<0 means that ALE LINK is taged in FRALNK() as defined from a grnod_id.

C
C   +--RADIOSS2
C      +--LECINP
C      |  +--FREFORM                           (direct access writing IUSC1, counting number of /VEL/ALE cards)
C      |     +--FRALNK(src/freef/fralnk.F)     (LINALE size calculation:SLINALE, reading/checking GRNOD option)
C      +--LECTUR                               (storing data in LINALE)
C      |  +--FR_RLALE(priv/spmd/fr_rlink1.F)   (check and tag available nodes on current domain)
C      +--RESOL
C         +--ALEWDX
C            +--ALELIN(priv/ale/alelin.F)      (ale link subroutine, updated to be used with grnod_id)
C
C
C

C     SLINALE : starter size
C     LLINAL  : engine size
C     NALELK  : engine /VEL/ALE card (obsolete)
C     NALELINK: engine /ALELINK/VEL
C     after reading : SLINALE <- SLINALE+LLINAL
C                      NALELK <- NALELK+NALELINK

      NALELK_starter = 0
      IF(MCHECK==0)NALELK_starter = SLINALE/7 !SLINALE : starter linale size
      NALELK_removed=0
      K=6

      !setting MAIN node user id to internal id
      DO J=1,NALELK_starter
        IF(IRUN == 1 .AND. MCHECK == 0)
     .   CALL FR_RLALE(LINALE(K-4),LINALE(K-3),LINALE(K+1),ITABM1,-1)
        K=K+1+6
      ENDDO

      IF(NALELK /= 0.AND.MCHECK == 0)THEN
       K=SLINALE+6                                                  !LINALE(1:SLINALE):starter cards ;LINALE(SLINALE+1:SLINALE+LLINE):engine cards
       !K=5 !en attendant de copier LIALE starter dans (1:SLINALE)
       DO J=1,NALELK
         READ (IIN,'(3I10,5X,3I1,I10)')M1,M2,N,I1,I2,I3,IM
         !---------------------------------!
         !  ALE LINK DEFINED FROM NODES    !
         !---------------------------------!
         IF(N>0)THEN
           READ (IIN,'(10I10)')(LINALE(K+I),I=1,N)
           IC=I3+2*I2+4*I1
           IF(IC == 0)THEN
            IC=7
            I1=1
            I2=1
            I3=1
           ENDIF
           IF(ISPMD == 0.AND.MCHECK == 0)WRITE(IOUT,2200)M1,M2,I1,I2,I3,IM
            LINALE(K-4)=M1
            LINALE(K-3)=M2
            LINALE(K-2)=N
            LINALE(K-1)=IC
            LINALE(K)=IM
            IF(ISPMD == 0.AND.MCHECK == 0)THEN
              WRITE(IOUT,'(10I10)')(LINALE(K+I),I=1,N)
              WRITE(IOUT,2201)
            ENDIF
c           Verification partagee et tag en - noeuds non present
            CALL FR_RLALE(LINALE(K-4),LINALE(K-3),LINALE(K+1),ITABM1,N)
            K=K+N+6
         !---------------------------------!
         !  ALE LINK DEFINED FROM GRNOD    !
         !---------------------------------!
         ELSE !N<=0
         !M1    < 0
         !GR_ID < 0
           READ (IIN,'(I10)', ERR=998,IOSTAT=IERROR)GR_ID

 0998     IF(IERROR/=0)THEN
            WRITE(ISTDO,*)  ' ** ERROR IN ALE LINK: INVALID GRNOD_ID'
            GR_ID = 0
          ENDIF
           LINALE(K+1)=NGR2USR(GR_ID,IGRNOD,NGRNOD)

           IF(LINALE(K+1)==0)CALL ARRET(2) !node group not found

           IF(IM==0.AND.IGRNOD(LINALE(K+1))%SORTED /= 1)THEN
           !option 0 needs to be defined with a /GRNOD/NODENS
             WRITE(ISTDO,*)' ** ERROR IN ALE LINK:'
             WRITE(ISTDO,*) '   UNSORTABLE NODE GROUP REQUIRED WITH OPTION 0 (/GRNOD/NODENS)'
             WRITE(IOUT ,*)' ** ERROR IN ALE LINK:'
             WRITE(IOUT,*) '   UNSORTABLE NODE GROUP REQUIRED WITH OPTION 0 (/GRNOD/NODENS)'
             CALL ARRET(2)
           ENDIF

           IC=I3+2*I2+4*I1
           IF(IC == 0)THEN
            IC=7
            I1=1
            I2=1
            I3=1
           ENDIF
           IF(ISPMD == 0.AND.MCHECK == 0)THEN
             WRITE(IOUT,2200)M1,M2,I1,I2,I3,IM !N=1 with GRNOD option
             WRITE(IOUT,2202)GR_ID
           ENDIF
            LINALE(K-4)=M1
            LINALE(K-3)=M2
            LINALE(K-2)=N
            LINALE(K-1)=IC
            LINALE(K)=IM
            !shared verification & untag non present nodes
            CALL FR_RLALE(LINALE(K-4),LINALE(K-3),LINALE(K+1),ITABM1,N)
            K=K+1+6
         ENDIF
       ENDDO !next J
       SLINALE = SLINALE + LLINAL
      ENDIF !IF(NALELK /= 0.AND.MCHECK == 0)THEN

C-------------------------------------------
C     ALE - LINK ON GRID VELOCITY
C     format >= v12  ('/ALE/LINK/*')
C-------------------------------------------
      IF(NALELINK /= 0.AND.MCHECK == 0)THEN
       IF(NALELK==0)K=SLINALE+6
       DO J=1,NALELINK
         READ (IIN,'(3I10,5X,3I1,I10)')M1,M2,N,I1,I2,I3,IM
         IF(M1>0.AND.M2>0)THEN
           !---------------------------------!
           !  ALE LINK DEFINED FROM NODES    !
           !---------------------------------!
           IF(N>0)THEN
             READ (IIN,'(10I10)')(LINALE(K+I),I=1,N)
             IC=I3+2*I2+4*I1
             IF(IC == 0)THEN
              IC=7
              I1=1
              I2=1
              I3=1
             ENDIF
             IF(ISPMD == 0.AND.MCHECK == 0)WRITE(IOUT,2200)M1,M2,I1,I2,I3,IM
             LINALE(K-4)=M1
             LINALE(K-3)=M2
             LINALE(K-2)=N
             LINALE(K-1)=IC
             LINALE(K)=IM
             IF(ISPMD == 0.AND.MCHECK == 0)THEN
               WRITE(IOUT,'(10I10)')(LINALE(K+I),I=1,N)
               WRITE(IOUT,2201)
             ENDIF
              ! Verification partagee et tag en - noeuds non present
              CALL FR_RLALE(LINALE(K-4),LINALE(K-3),LINALE(K+1),ITABM1,N)
              K=K+N+6
            !---------------------------------!
            !  ALE LINK DEFINED FROM GRNOD    !
            !---------------------------------!
           ELSE !(N<=0)
              READ (IIN,'(I10)', ERR=999,IOSTAT=IERROR)GR_ID
 0999         IF(IERROR/=0)THEN
                WRITE(ISTDO,*)  ' ** ERROR IN ALE LINK: CANNOT READ GRNOD_ID VALUE'
                GR_ID = 0
                CALL ARRET(2) !node group not read
              ENDIF

              LINALE(K+1)=NGR2USR(GR_ID,IGRNOD,NGRNOD)
              IF(LINALE(K+1)==0)THEN
                WRITE(ISTDO,*)  ' ** ERROR IN ALE LINK: INVALID GRNOD_ID'
                CALL ARRET(2) !node group not found
              ENDIF

              IF(IM==0.AND.IGRNOD(LINALE(K+1))%SORTED /= 1)THEN
              !option 0 needs to be defined with a /GRNOD/NODENS
                WRITE(ISTDO,*)' ** ERROR IN ALE LINK:'
                WRITE(ISTDO,*)'    UNSORTABLE NODE GROUP REQUIRED WITH OPTION 0 (/GRNOD/NODENS)'
                WRITE(IOUT ,*)' ** ERROR IN ALE LINK:'
                WRITE(IOUT,*) '    UNSORTABLE NODE GROUP REQUIRED WITH OPTION 0 (/GRNOD/NODENS)'
                CALL ARRET(2)
              ENDIF

              IC=I3+2*I2+4*I1
              IF(IC == 0)THEN
               IC=7
               I1=1
               I2=1
               I3=1
              ENDIF
              IF(ISPMD == 0.AND.MCHECK == 0)THEN
                WRITE(IOUT,2200)M1,M2,I1,I2,I3,IM !N=1 with GRNOD option
                WRITE(IOUT,2202)GR_ID
              ENDIF
              LINALE(K-4)=M1
              LINALE(K-3)=M2
              LINALE(K-2)=N
              LINALE(K-1)=IC
              LINALE(K)=IM
              !shared verification & untag non present nodes
              CALL FR_RLALE(LINALE(K-4),LINALE(K-3),LINALE(K+1),ITABM1,N)
              K=K+1+6
           ENDIF !(N>0)

         !---------------------------------!
         !  ALE LINK SET ON                !
         !---------------------------------!
         ELSEIF(M1==-1)THEN ! ON
           ALLOCATE(ALELIN_ON_OFF(IABS(N)))
           READ (IIN,'(10I10)')(ALELIN_ON_OFF(I),I=1,N)
           !L=1  !pos
           KK=0 ! number of ale link deactivated
           DO I1=1,N
             L=1 !pos
             M1=ALELIN_ON_OFF(I1)
           DO I=1,NALELK_starter
             !---------------------------------!
             !  CHECK AND DEACTIVATE           !
             !---------------------------------!
             IF(L>=SLINALE)THEN
               !warning : uID does not exist
                 WRITE(ISTDO,*)' ** WARNING ALE LINK DOES NOT EXIST :',M1
                 WRITE(IOUT,*) ' ** WARNING ALE LINK DOES NOT EXIST :',M1
               EXIT
             ELSE
               uID = LINALE(L+0)
               N   = LINALE(L+3)
               IF(uID==-M1)THEN
                 NALELK_removed = NALELK_removed+1 !counting deactivated links
                 LINALE(L+0)=-LINALE(L+0)          !setting negative uID to skip ALE LINK treatment (alelin.F)
                 !printout :
                 IF(ISPMD == 0.AND.MCHECK == 0)WRITE(IOUT,2211)M1 !N=1 with GRNOD option
                 EXIT !next ALELIN_ON_OFF
               ELSEIF(uID==M1)THEN
                 NALELK_removed = NALELK_removed+1 !counting deactivated links
                 !warning uID already set OFF :
                 WRITE(ISTDO,*)' ** WARNING ALE LINK ALREADY ACTIVATED :  ',M1
                 EXIT !next ALELIN_ON_OFF
               ENDIF

             ENDIF
             L = L+6+IABS(N) !next uID position in LINALE()
           ENDDO
           ENDDO
           DEALLOCATE(ALELIN_ON_OFF)
           !IF(NALELK_removed>0)NALELK_removed=1 !/ALE/LINK/ON or OFF card is no longer taken into account

         !---------------------------------!
         !  ALE LINK SET OFF               !
         !---------------------------------!
         ELSEIF(M1==-2)THEN ! OFF
           ALLOCATE(ALELIN_ON_OFF(IABS(N)))
           READ (IIN,'(10I10)')(ALELIN_ON_OFF(I),I=1,N)
           !L=1  !pos
           KK=0 ! number of ale link deactivated
           DO I1=1,N
             L=1 !pos
             M1=ALELIN_ON_OFF(I1)
           DO I=1,NALELK_starter
             !---------------------------------!
             !  CHECK AND DEACTIVATE           !
             !---------------------------------!
             IF(L>=SLINALE)THEN
               !warning : uID does not exist
                 WRITE(ISTDO,*) ' ** WARNING ALE LINK DOES NOT EXIST :',M1
                 WRITE(IOUT,*)  ' ** WARNING ALE LINK DOES NOT EXIST :',M1
               EXIT
             ELSE
               uID = LINALE(L+0)
               N   = LINALE(L+3)
               IF(uID==M1)THEN
                 NALELK_removed = NALELK_removed+1 !counting deactivated links
                 LINALE(L+0)=-LINALE(L+0)          !setting negative uID to skip ALE LINK treatment (alelin.F)
                 !printout :
                 IF(ISPMD == 0.AND.MCHECK == 0)WRITE(IOUT,2210)M1 !N=1 with GRNOD option
                 EXIT !next ALELIN_ON_OFF
               ELSEIF(uID==-M1)THEN
                 NALELK_removed = NALELK_removed+1 !counting deactivated links
                 !warning uID already set OFF :
                 WRITE(ISTDO,*)' ** WARNING ALE LINK ALREADY DEACTIVATED :',M1
                 EXIT !next ALELIN_ON_OFF
               ENDIF
             ENDIF
             L = L+6+IABS(N) !next uID position in LINALE()
           ENDDO
           ENDDO
           DEALLOCATE(ALELIN_ON_OFF)
           !IF(NALELK_removed>0)NALELK_removed=1 !/ALE/LINK/ON or OFF card is no longer taken into account
         ENDIF

       ENDDO
       SLINALE = SLINALE + LLINAL
       NALELK  = NALELK+NALELINK-NALELK_removed
      ENDIF !(NALELINK /= 0.AND.MCHECK == 0)

      IF(MCHECK==0)NALELK=NALELK+NALELK_starter

C-------------------------------------------
C     ALE ON/ OFF
C-------------------------------------------
      IF(NBPARTALEON > 0) ALLOCATE(ALEON_PARTIDS_TMP(NBPARTALEON))
      IF(NBPARTALEOFF > 0) ALLOCATE(ALEOFF_PARTIDS_TMP(NBPARTALEOFF))
      ION = 0
      IOFF = 0
      DO I = 1, NBPARTALEON + NBPARTALEOFF
         READ(IIN, '(I10, I10)') PARTID, ISON
         IF (ISON == 1) THEN
            ION = ION + 1
            ALEON_PARTIDS_TMP(ION) = PARTID
         ELSE IF(ISON == 0) THEN
            IOFF = IOFF + 1
            ALEOFF_PARTIDS_TMP(IOFF) = PARTID
         ENDIF
      ENDDO
C     Checking for doublon and suppression
      DO I = 1, NBPARTALEON
         PARTID = ALEON_PARTIDS_TMP(I)
         IF (PARTID /= -1) THEN
            DO J = 1, NBPARTALEON
               IF (J == I) CYCLE
               IF (ALEON_PARTIDS_TMP(J) == PARTID) THEN
                  NBPARTALEON = NBPARTALEON - 1
                  ALEON_PARTIDS_TMP(J) = -1
               ENDIF
            ENDDO
         ENDIF
      ENDDO
      DO I = 1, NBPARTALEOFF
         PARTID = ALEOFF_PARTIDS_TMP(I)
         IF (PARTID /= -1) THEN
            DO J = 1, NBPARTALEOFF
               IF (J == I) CYCLE
               IF (ALEOFF_PARTIDS_TMP(J) == PARTID) THEN
                  NBPARTALEOFF = NBPARTALEOFF - 1
                  ALEOFF_PARTIDS_TMP(J) = -1
               ENDIF
            ENDDO
C     Check if part is also defined in ON
C     By default, all parts are ON, if a part is set to OFF
C     and ON at the same time, we choose to OFF it
            DO J = 1, NBPARTALEON
               IF (ALEON_PARTIDS_TMP(J) == PARTID) THEN
                  !!! WARNING TO OUTPUT HERE
                  ALEON_PARTIDS_TMP(J) = -1
                  NBPARTALEON = NBPARTALEON - 1
                  CALL ANCMSG(MSGID = 272, ANMODE = ANINFO, I1 = PARTID)
               ENDIF
            ENDDO
         ENDIF
      ENDDO
C     Checking that part ids exist in IPART(4, *)
      NBALEOFF_PART = NBPARTALEOFF
      NBALEON_PART = NBPARTALEON
      DO I = 1, NBPARTALEON
         PARTID = ALEON_PARTIDS_TMP(I)
         ISON = 0
         IF (PARTID /= -1) THEN
            DO J = 1, NPART
               IF (IPART(4 + (J - 1) * LIPART1) == PARTID) THEN
                  ISON = 1
                  ALEON_PARTIDS_TMP(I) = J
                  EXIT
               ENDIF
            ENDDO
         ENDIF
         IF (ISON == 0) THEN
            ALEON_PARTIDS_TMP(I) = -1
            NBALEON_PART = NBALEON_PART - 1
            CALL ANCMSG(MSGID = 271, ANMODE = ANINFO, I1 = PARTID)
            CALL ARRET(2)
         ENDIF
      ENDDO
      DO I = 1, NBPARTALEOFF
         PARTID = ALEOFF_PARTIDS_TMP(I)
         ISON = 0
         IF (PARTID /= -1) THEN
            DO J = 1, NPART
               IF (IPART(4 + (J - 1) * LIPART1) == PARTID) THEN
                  ISON = 1
                  ALEOFF_PARTIDS_TMP(I) = J
                  EXIT
               ENDIF
            ENDDO
         ENDIF
         IF (ISON == 0) THEN
            ALEOFF_PARTIDS_TMP(I) = -1
            NBALEOFF_PART = NBALEOFF_PART - 1
            CALL ANCMSG(MSGID = 271, ANMODE = ANINFO, I1 = PARTID)
            CALL ARRET(2)
         ENDIF
      ENDDO
C     Filling module values
      IF (NBALEOFF_PART > 0) ALLOCATE(ALEOFF_PARTIDS(NBALEOFF_PART))
      IF (NBALEON_PART > 0) ALLOCATE(ALEON_PARTIDS(NBALEON_PART))
      DO I = 1, NBPARTALEON
         PARTID = ALEON_PARTIDS_TMP(I)
         ISON = 0
         IF (PARTID /= -1) THEN
            ISON = ISON + 1
            ALEON_PARTIDS(ISON) = PARTID
         ENDIF
      ENDDO
      ISON = 0
      DO I = 1, NBPARTALEOFF
         PARTID = ALEOFF_PARTIDS_TMP(I)
         IF (PARTID /= -1) THEN
            ISON = ISON + 1
            ALEOFF_PARTIDS(ISON) = PARTID
         ENDIF
      ENDDO
C     OUTPUT
      DO I = 1, NBALEOFF_PART
         WRITE(IOUT, 4800) ALEOFF_PARTIDS(I)
      ENDDO
C-------------------------------------------
C     Fill IPARG
C-------------------------------------------
      K1 = 1 + LIPART1 * (NPART + NTHPART) + 2 * 9 * (NPART + NTHPART)
      K2 = K1 + NUMELS
      DO NG = 1, NGROUP
         ITY = IPARG(5, NG)
         NEL = IPARG(2, NG)
         NFT = IPARG(3, NG)
C     Initially, IPARG(76, *) = 0 --> ON
C     When restarting a computation, IPARG(76, NG) = Whatever was put here during previous run
C     Find part associated to the current group
C     by checking the first element of the group
C     NB : all elements in a same group belong necessarily to the same part

         IF (ITY == 1) THEN
C     Solid groups
            PARTID = IPART(K1 + NFT)
         ELSEIF (ITY == 2) THEN
C     Shells and coques
            PARTID = IPART(K2 + NFT)
         ENDIF
C     Check if this part is found in ALEOFF_PARTIDS
         DO I = 1, NBALEOFF_PART
            IF (ALEOFF_PARTIDS(I) == PARTID) THEN
               IPARG(76, NG) = 1 ! --> OFF
               EXIT
            ENDIF
         ENDDO
C     Check if this part is found in ALEON_PARTIDS
         DO I = 1, NBALEON_PART
            IF (ALEON_PARTIDS(I) == PARTID) THEN
               IPARG(76, NG) = 0 ! --> ON
               EXIT
            ENDIF
         ENDDO
      ENDDO
      IF (ALLOCATED(ALEON_PARTIDS_TMP)) DEALLOCATE(ALEON_PARTIDS_TMP)
      IF (ALLOCATED(ALEOFF_PARTIDS_TMP)) DEALLOCATE(ALEOFF_PARTIDS_TMP)
      IF (ALLOCATED(ALEON_PARTIDS)) DEALLOCATE(ALEON_PARTIDS)
      IF (ALLOCATED(ALEOFF_PARTIDS)) DEALLOCATE(ALEOFF_PARTIDS)
C-------------------------------------------
C     MODIFICATION DES CONDITIONS AUX LIMITES
C-------------------------------------------
      IF(NUBCSN /= 0)THEN
        CALL LCBCSF(ICODE,ISKEW,NUBCSN,ITAB,ITABM1,NPBY ,ISKWN,WEIGHT)
        IF(NSPMD > 1) THEN
C traitement necessaire pour recuperer les noeuds cond limites
          IWIOUT = 0
          IF (ISPMD /= 0) CALL SPMD_CHKW(IWIOUT,IOUT)
          CALL SPMD_GLOB_ISUM9(IWIOUT,1)
          CALL SPMD_IBCAST(IWIOUT,IWIOUT,1,1,0,2)
          IF (IWIOUT > 0) THEN
            CALL SPMD_WIOUT(IOUT,IWIOUT)
            IWIOUT = 0
          END IF
        END IF
      END IF
C-------------------------------------------
C     MODIFICATION DES INTERFACES
C-------------------------------------------
      IF(NINTCH /= 0)THEN
        DO 950 I=1,NINTCH
            READ(IIN,'(2I8,2F16.0)')NOINT,NSEARCH,TSTART,TFIN
            IF(TFIN == 0.0)TFIN=1.E30
            JPRI = -1
            DO 949 J=1,NINTER
              IF(NOINT == IPARI(15,J)) THEN
                IF(NSEARCH /= 0) IPARI(13,J)=NSEARCH
                INTBUF_TAB(J)%VARIABLES(3)  = TSTART
                INTBUF_TAB(J)%VARIABLES(11) = TFIN
                JPRI=J
              ENDIF
 949        CONTINUE
            IF(JPRI/=-1) THEN
                IF(ISPMD == 0.AND.MCHECK == 0) WRITE(IOUT,2300)NOINT,IPARI(13,JPRI),TSTART,TFIN
            ELSE
                IF(ISPMD == 0.AND.MCHECK == 0) WRITE(IOUT,2301)NOINT
            ENDIF
 950    CONTINUE
      ENDIF
C
      IF(MCHECK /= 0)THEN
       DO I = 1,MX_OUTP
        NV_OUTP = NV_OUTP + OUTP_V(I)
        NSS_OUTP = NSS_OUTP + OUTP_SS(I)
        NST_OUTP = NST_OUTP + OUTP_ST(I)
        NCS_OUTP = NCS_OUTP + OUTP_CS(I)
        NCT_OUTP = NCT_OUTP + OUTP_CT(I)
        NTS_OUTP = NTS_OUTP + OUTP_TS(I)
        NPS_OUTP = NPS_OUTP + OUTP_PS(I)
        NPT_OUTP = NPT_OUTP + OUTP_PT(I)
        NRS_OUTP = NRS_OUTP + OUTP_RS(I)
        NRT_OUTP = NRT_OUTP + OUTP_RT(I)
       ENDDO
       IF(OUTP_V(12) == 1)NV_OUTP=NV_OUTP+1
      ENDIF
C-------------------------------------------
C     MODIFICATION OF RIGID BODY
C-------------------------------------------
      IF(NRBYOF /= 0)
     1    CALL   RBYONF(IPARG,IPARI       ,MS     ,IN     ,
     2                  IXS  ,IXQ  ,IXC   ,IXT    ,IXP    ,
     3                  IXR  ,SKEW ,ITAB  ,ITABM1 ,ISKWN  ,
     4                  NPBY ,0    ,NRBYOF,WA     ,LPBY   ,
     5                  RBY  ,X    ,V     ,VR     ,IXTG   ,
     6                  IGRV ,IBGR ,WEIGHT,FR_RBY2,PARTSAV,
     7                  IPART,ELBUF_STR,ICFIELD,LCFIELD,TAGSLV_RBY)
C
      IF(NRBYON /= 0)
     1    CALL   RBYONF(IPARG,IPARI       ,MS     ,IN     ,
     2                  IXS  ,IXQ  ,IXC   ,IXT    ,IXP    ,
     3                  IXR  ,SKEW ,ITAB  ,ITABM1 ,ISKWN  ,
     4                  NPBY ,1    ,NRBYON,WA     ,LPBY   ,
     5                  RBY  ,X    ,V     ,VR     ,IXTG   ,
     6                  IGRV ,IBGR ,WEIGHT,FR_RBY2,PARTSAV,
     7                  IPART,ELBUF_STR,ICFIELD,LCFIELD,TAGSLV_RBY)
C---------------------------------------------------------
C     READING DATA FOR FLUX & ROTATION
C---------------------------------------------------------
      IF (NSFLSW  /= 0.AND.MCHECK == 0) THEN
        CALL LECFLSW (NSFLSW,NTFLSW,NEFLSW,NNFLSW,CRFLSW,X,IXS,IPARG,WA)
      ENDIF
C-------------------------------------------------
C     MODIFICATION OF TIME FUNCTIONS
C-------------------------------------------------
      IF (NFCT  /=  0) THEN
        CALL LECFUN (NPC, PLD, NFCT, NPTS, TABLE)
      END IF
C-------------------------------------------------
C     RESET VELOCITIES
C-------------------------------------------------
      IF (NINIV  /=  0) THEN
        CALL LECINV (NINIV,X,V,VR,ITAB,IFRAME,XFRAME,IGRNOD,FXBIPM,FXBVIT,FXBRPM)
      END IF
C---------------------------------------------------------
C     CUTS
C---------------------------------------------------------
      IF(NCUTS > 0.AND.MCHECK == 0)CALL LECCUT(ICUT,XCUT,ITABM1)
C---------------------------------------------------------
C     ANIMATION (DT ,DMAS)
C---------------------------------------------------------
      DO I=1,SANIN
        ANIN(I)=ZERO
      ENDDO
C---------------------------------------------------------
C     FILTERED SAMPLED OUTPUT
C---------------------------------------------------------
      IF(NNOISE > 0)CALL LECNOISE(INOISE,ITABM1,NAMES_AND_TITLES)
      NNOISER=NNOISE
C---------------------------------------------------------
C     MADYMO COUPLING GENERAL DATA (CONVERSION FACTORS).
C---------------------------------------------------------
#ifdef DNC
      IF( IMADCPL > 0.AND.INVERS>=40.AND.MCHECK == 0)CALL LECCPL()
#endif
C---------------------------------------------------------
C     OPTIONS SPMD SPECIFIQUES
       IF(IPREAD > 0.AND.ISPMD == 0.AND.MCHECK == 0) WRITE(IOUT,4000)
       IF(IDDW > 0.AND.ISPMD == 0.AND.MCHECK == 0) WRITE(IOUT,4100)
C---------------------------------------------------------
C     RAYLEIGH DAMPING
C---------------------------------------------------------
      CALL LECDAMP(NDAMPN, DAMPR, IGRNOD)
C---------------------------------------------------------
      ISTATCND_SAV= ISTATCND
      IF(IMPL_S/=0) THEN
        IF(ISPRB==0.AND.NADMESH/=0)THEN
          IF (IMACH /= 3.OR.ISPMD == 0) THEN
            CALL ANCMSG(MSGID=131,ANMODE=ANINFO)
          ENDIF
          CALL ARRET(2)
        ENDIF
        ISTATCND    = 0
      END IF
C---------------------------------------------------------
      IF (IMPL_S == 1.OR.NEIG > 0) CALL LECIMPL
      IF (NFXINP > 0) CALL LECFXINP(NFXINP)
      IF(ALE%SUB%NODSUBDT /= 0)THEN
        IF(ISPMD == 0) THEN
          CALL ANCMSG(MSGID=129,ANMODE=ANINFO_BLIND)
        END IF
        CALL ARRET(2)
      END IF
      IF (NEIGOFF > 0) CALL LECEIG(NEIGOFF, NEOFF)
      IF (NFVMESH > 0) CALL LECFVBAG(NFVMESH, MONVOL, VOLMON, X)
      IF (NFVMODI > 0) CALL LECFVBAG1(NFVMODI, MONVOL, VOLMON)
C-------------------------------------------
C     .sta files
C-------------------------------------------
      IF(MCHECK /= 0)THEN
        NC_STAT = 0
        DO I = 1,MX_STAT
          NC_STAT = NC_STAT + STAT_C(I)
        ENDDO
      ENDIF
      IF(NSTATPRT /= 0 .OR. NSTATALL /= 0)THEN
        K1=1+LIPART1*(NPART+NTHPART)+2*9*(NPART+NTHPART)
        K2=K1+NUMELS
        K3=K2+NUMELQ
        K4=K3+NUMELC
        K5=K4+NUMELT
        K6=K5+NUMELP
        K7=K6+NUMELR
        K8=K7
        K9=K8+NUMELTG
        CALL LECSTAT(IPART,IPART_STATE,ELBUF_STR,IPM,IPARG,IPART(K1),IPART(K3),IPART(K8))
      END IF
C-------------------------------------------
C     .dynain files
C-------------------------------------------
      IF(DYNAIN_DATA%NDYNAINPRT /= 0 .OR. DYNAIN_DATA%NDYNAINALL /= 0)THEN
        K1=1+LIPART1*(NPART+NTHPART)+2*9*(NPART+NTHPART)
        K2=K1+NUMELS
        K3=K2+NUMELQ
        K4=K3+NUMELC
        K5=K4+NUMELT
        K6=K5+NUMELP
        K7=K6+NUMELR
        K8=K7
        K9=K8+NUMELTG
        CALL READ_DYNAIN(IPART,DYNAIN_DATA,IPART(K3),IPART(K8),IXC,IXTG)
      END IF
c-----------------------------------------
c     deallocate temporary SENSOR arrays
c-----------------------------------------
      IF (ALLOCATED(SENSORS%ANIM_TMP)) DEALLOCATE (SENSORS%ANIM_TMP)
      IF (ALLOCATED(SENSORS%STAT_TMP)) DEALLOCATE (SENSORS%STAT_TMP)
      IF (ALLOCATED(SENSORS%OUTP_TMP)) DEALLOCATE (SENSORS%OUTP_TMP)
      IF (ALLOCATED(SENSORS%STOP_TMP)) DEALLOCATE (SENSORS%STOP_TMP)
C---------------------------------------------------------
      IF(IERR == 0) RETURN
      CALL ARRET(0)
 1001 FORMAT(/
     & 1X,'ALE EULER SOLVERS'/
     & 1X,'-----------------'/)
 1002 FORMAT(
     & 1X,'  +--STAGGERRED SCHEME'/
     & 1X,'  |  +--MOMENTUM          : ',A16,'  eta=',G14.7/
     & 1X,'  |  +--MASS              : ',A16,'  eta=',G14.7/
     & 1X,'  |  +--ENERGY            : ',A16,'  eta=',G14.7/
     & 1X,'  |  +--VOLUME FRACTION   : ',A16)
 1003 FORMAT(
     & 1X,'  |'/
     & 1X,'  +--COLOCATED SCHEME (LAW151)'/
     & 1X,'  |  +--MOMENTUM          : ',A16/
     & 1X,'  |  +--MASS              : ',A16/
     & 1X,'  |  +--ENERGY            : ',A16/
     & 1X,'  |  +--VOLUME FRACTION   : ',A16   )
 1004 FORMAT(
     & 1X,'  |  +--LOW MACH OPTION   : ENABLED' )
 1005 FORMAT(
     & 1X,'  |  +--MUSCL OPTION      : ENABLED          beta=',G14.7)
 1006 FORMAT(
     & 1X,'  |'/
     & 1X,'  +--TIME STEP'/
     & 1X,'  |  +--COURANT NUMBER    : ',G14.7/
     & 1X,'  |  +--MINIMUM TIME STEP : ',G14.7)
 1007 FORMAT(
     & 1X,'  |'/
     & 1X,'  +--GRID SMOOTHING'/
     & 1X,'     +--FORMULATION       : ',A9)
 1008 FORMAT(
     & 1X,'     +--PARAMETERS        :')

 1099 FORMAT(
     .  ' MULTIDOMAINS COUPLING . . . . . . . . . . . . . .',G14.7//)
 1100 FORMAT(
     .  ' FINAL TIME . . . . . . . . . . . . . . . . . . . ',G14.7//
     .  ' TIME INTERVAL FOR TIME HISTORY PLOTS . . . . . . ',G14.7//
     .  ' TIME STEP SCALE FACTOR . . . . . . . . . . . . . ',G14.7//
     .  ' MINIMUM TIME STEP  . . . . . . . . . . . . . . . ',G14.7//)
 1105 FORMAT(
     .  ' BRICK TIME STEP SCALE FACTOR . . . . . . . . . . ',G14.7/
     .  ' BRICK MINIMUM TIME STEP. . . . . . . . . . . . . ',G14.7/
     .  ' MIN. TIME STEP FLAG (1:STOP RUN, 2:DELETE BRICK) ',I5//
     .  ' QUAD TIME STEP SCALE FACTOR. . . . . . . . . . . ',G14.7/
     .  ' QUAD MINIMUM TIME STEP . . . . . . . . . . . . . ',G14.7/
     .  ' MIN. TIME STEP FLAG (1:STOP RUN, 2:DELETE QUAD). ',I5//
     .  ' SHELL TIME STEP SCALE FACTOR . . . . . . . . . . ',G14.7/
     .  ' SHELL MINIMUM TIME STEP. . . . . . . . . . . . . ',G14.7/
     .  ' MIN. TIME FLAG (1:STOP, 2:DELETE, 3:SMALL STRAIN)',I5//
     .  ' TRUSS TIME STEP SCALE FACTOR . . . . . . . . . . ',G14.7/
     .  ' TRUSS MINIMUM TIME STEP. . . . . . . . . . . . . ',G14.7/
     .  ' MIN. TIME FLAG (1:STOP, 2:DELETE). . . . . . . . ',I5//
     .  ' BEAM TIME STEP SCALE FACTOR. . . . . . . . . . . ',G14.7/
     .  ' BEAM MINIMUM TIME STEP . . . . . . . . . . . . . ',G14.7/
     .  ' MIN. TIME FLAG (1:STOP, 2:DELETE). . . . . . . . ',I5//
     .  ' SPRING TIME STEP SCALE FACTOR. . . . . . . . . . ',G14.7/
     .  ' SPRING MINIMUM TIME STEP . . . . . . . . . . . . ',G14.7/
     .  ' MIN. TIME FLAG (1:STOP, 2:DELETE). . . . . . . . ',I5//
     .  ' AIRBAG TIME STEP SCALE FACTOR. . . . . . . . . . ',G14.7/
     .  ' AIRBAG MINIMUM TIME STEP . . . . . . . . . . . . ',G14.7/
     .  ' MIN. TIME FLAG (1:STOP). . . . . . . . . . . . . ',I5//
     .  ' CONTACT TIME STEP SCALE FACTOR . . . . . . . . . ',G14.7/
     .  ' CONTACT MINIMUM TIME STEP. . . . . . . . . . . . ',G14.7/
     .  ' MIN. TIME FLAG(1:STOP, 2:REMOVE NODE FROM INTERF)',I5/ )
 1155 FORMAT('/DT/FVMBAG/0 OPTION')
 1156 FORMAT('/DT/FVMBAG/1 OPTION')
 1157 FORMAT('FVMBAG TIME STEP SYNTHESIS')
 1147 FORMAT(
     .  ' FVMBAG  ID. . . . . . . . . . . . . . . . . . . . ',I10)
 1148 FORMAT(
     .  ' FVMBAG1 ID. . . . . . . . . . . . . . . . . . . . ',I10)
 1149 FORMAT(
     .  ' FVMBAG2 ID. . . . . . . . . . . . . . . . . . . . ',I10)
 1151 FORMAT(
     .  ' SMOOTH PARTICLES TIME STEP SCALE FACTOR. . . . . ',G14.7/
     .  ' SMOOTH PARTICLES MINIMUM TIME STEP . . . . . . . ',G14.7/
     .  ' MIN. TIME FLAG (1:STOP, 2:DELETE, 5:KILL). . . . ',I5  )
 1152 FORMAT(
     .  ' FVMBAG TIME STEP SCALE FACTOR. . . . . . . . . . ',G14.7/
     .  ' FVMBAG MINIMUM TIME STEP . . . . . . . . . . . . ',G14.7/
     .  ' FVMBAG TIME STEP FLAG. . . . . . . . . . . . . . ',I5,/)
 1153 FORMAT(
     .  ' . . TIME STEP SCALE FACTOR. . . . . . . . . . . . . . ',G14.7/
     .  ' . . MINIMUM TIME STEP . . . . . . . . . . . . . . . . ',G14.7/
     .  ' . . TIME STEP FLAG. . . . . . . . . . . . . . . . . . ',I5)
 1154 FORMAT(
     .  ' . . CHARACTERISTIC LENGTH OPTION. . . . . . . . . . . ',I5/
     .  ' . . TIME STEP SMOOTHING FACTOR. . . . . . . . . . . . ',G14.7/
     .  ' . . PREVIOUS TIME STEP. . . . . . . . . . . . . . . . ',G14.7)
 1107 FORMAT(
     .  ' CONTACT NODAL TIME STEP SCALE FACTOR . . . . . . ',G14.7/
     .  ' CONTACT NODAL MINIMUM TIME STEP. . . . . . . . . ',G14.7/
     .  ' CONTACT NODAL MIN. TIME FLAG . . . . . . . . . . ',I5/
     .  '    3:INCREASE MASS, ORIGINAL FORMULATION . . . . ',/
     .  '    8:INCREASE MASS, IMPROVED FORMULATION . . . . ',//)
 1209 FORMAT(
     .  ' AMS CONTACT TIME STEP IS ON . . . . . . . . . . . . .',/
     .  ' . . . . . .(ALL CONTACTS WILL BE CONCERNED).) . . . .',/
     .  ' AMS CONTACT TIME STEP SCALE FACTOR. . . . . . . . . .',G14.7/
     .  ' AMS CONTACT MINIMUM TIME STEP . . . . . . . . . . . .',G14.7//)
 1106 FORMAT(
     .  ' NODAL TIME STEP SCALE FACTOR,  . . . . . . . . . ',G14.7/
     .  ' NODAL MINIMUM TIME STEP. . . . . . . . . . . . . ',G14.7/
     .  ' MIN. TIME FLAG . . . . . . . . . . . . . . . . . ',I5/
     .  '    1:STOP. . . . . . . . . . . . . . . . . . . . ',/
     .  '    3:INCREASE MASS, ORIGINAL FORMULATION . . . . ',/
     .  '    8:INCREASE MASS, IMPROVED FORMULATION . . . . ',//)
 1116 FORMAT(
     .  ' NODAL TIME STEP SCALE FACTOR,  . . . . . . . . . ',G14.7/
     .  ' NODAL MINIMUM TIME STEP. . . . . . . . . . . . . ',G14.7/)
 1206 FORMAT(
     .  ' NODAL TIME STEP SCALE FACTOR,  . . . . . . . . . ',G14.7/
     .  ' NODAL MINIMUM TIME STEP. . . . . . . . . . . . . ',G14.7/
     .  ' ADDED MASS RATIO (DM/M0) . . . . . . . . . . . . ',G14.7/
     .  ' MIN. TIME FLAG . . . . . . . . . . . . . . . . . ',I5/
     .  '    1:STOP. . . . . . . . . . . . . . . . . . . . ',/
     .  '    3:INCREASE MASS, ORIGINAL FORMULATION . . . . ',/
     .  '    8:INCREASE MASS, IMPROVED FORMULATION . . . . ',//)
 1108 FORMAT(
     .  ' ADVANCED MASS SCALING IS ON . . . . . . . . . . .',/
     .  ' AMS NODAL TIME STEP SCALE FACTOR. . . . . . . . .',G14.7/
     .  ' AMS NODAL MINIMUM TIME STEP . . . . . . . . . . .',G14.7/
     .  ' AMS TOLERANCE ON CONVERGENCE. . . . . . . . . . .',G14.7/
     .  ' AMS MAXIMUM NUMBER OF ITERATIONS FOR CONVERGENCE.',I10/
     .  ' OUTPUT FREQUENCY OF INFORMATION / CONVERGENCE . .',I10/
     .  ' PART GROUP ID (=0 ALL PARTS) . . . . . . . . . . ',I10//)
 1109 FORMAT(
     .  ' ADVANCED MASS SCALING IS ON . . . . . . . . . . .',/
     .  ' AMS TIME STEP SCALE FACTOR. . . . . . . . . . . .',G14.7/
     .  ' AMS MINIMUM TIME STEP . . . . . . . . . . . . . .',G14.7/
     .  ' AMS TOLERANCE ON CONVERGENCE. . . . . . . . . . .',G14.7/
     .  ' AMS MAXIMUM NUMBER OF ITERATIONS FOR CONVERGENCE.',I10/
     .  ' NUMBER OF PRECONDITIONING VECTORS FOR PCG . . . .',I10/
     .  ' OUTPUT FREQUENCY OF INFORMATION / CONVERGENCE . .',I10/
     .  ' PART GROUP ID (=0 ALL PARTS) . . . . . . . . . . ',I10//)
 2109 FORMAT(
     .  ' ADVANCED MASS SCALING IS ON . . . . . . . . . . .',/
     .  ' AMS TIME STEP SCALE FACTOR. . . . . . . . . . . .',G14.7/
     .  ' AMS MINIMUM TIME STEP . . . . . . . . . . . . . .',G14.7/
     .  ' AMS TOLERANCE ON CONVERGENCE. . . . . . . . . . .',G14.7/
     .  ' AMS MAXIMUM NUMBER OF ITERATIONS FOR CONVERGENCE.',I10/
     .  ' NUMBER OF PRECONDITIONING VECTORS FOR PCG . . . .',I10/
     .  ' OUTPUT FREQUENCY OF INFORMATION / CONVERGENCE . .',I10/
     .  ' AUTO ELEMENT SELECTION - TIME STEP CRITERIA . . .',G14.7/
     .  ' PART GROUP ID (=0 ALL PARTS) . . . . . . . . . . ',I10//)
 1110 FORMAT(
     .  ' PRINTOUT CYCLE FREQUENCY . . . . . . . . . . . . ',I5   //
     .  ' RESTART CYCLE FREQUENCY. . . . . . . . . . . . . ',I10  //
     .  ' MAXIMUM RESTART WRITE BEFORE OVERWRITE . . . . . ',I5   //
     .  ' INVERS: INPUT DECK VERSION . . . . . . . . . . . ',I5/
     .  ' ITTYP : FLAG FOR TYPE OF T-FILE. . . . . . . . . ' ,I5/)
 1112 FORMAT(
     .  ' NODAL TIME STEP FOR SHELLS SCALE FACTOR. . . . . ',G14.7/
     .  ' NODAL MINIMUM TIME STEP FOR SHELLS . . . . . . . ',G14.7/
     .  ' MIN. TIME FLAG (1:INCREASE MASS) . . . . . . . . ',I5//)
 1120 FORMAT(/
     .  ' TIME FOR FIRST ANIMATION-FILE PLOT . . . . . . . ',G14.7/
     .  ' TIME INTERVAL FOR ANIMATION-FILE PLOTS . . . . . ',G14.7/
     .  ' TIME TO STOP ANIMATION-FILE PLOTS. . . . . . . . ',G14.7/
     .  ' SENSOR FOR ANIMATION-FILE PLOTS. . . . . . . . . ',I10/
     .  ' TIME INTERVAL FOR SENSOR ANIMATION-FILE PLOTS. . ',G14.7//
     .  ' VARIABLE SAVED ON ANIMATION FILES (1: YES) :',/
     .  '      PLASTIC STRAIN. . . . . . ',I5/
     .  '      DENSITY     (SOLID ONLY). ',I5/
     .  '      SPECIFIC ENERGY . . . . . ',I5/
     .  '      HOURGLASS ENERGY. . . . . ',I5/
     .  '      TEMPERATURE (SOLID ONLY). ',I5/
     .  '      THICKNESS   (SHELL ONLY). ',I5/
     .  '      PRESSURE    (SOLID ONLY). ',I5/
     .  '      VON MISES . . . . . . . . ',I5/
     .  '      TURBULENT ENERGY (FLUID). ',I5/
     .  '      TURBULENT VISCOSITY(FLUID)',I5/
     .  '      VORTICITY (FLUID) . . . . ',I5)
 1129 FORMAT(
     .  ' NODAL VALUES SAVED ON ANIMATION FILES (1: YES) :',/
     .  '      PRESSURE. . . . . . . . . ',I5/
     .  '      DENSITY . . . . . . . . . ',I5/
     .  '      TEMPERATURE . . . . . . . ',I5)
 1130 FORMAT(
     .  ' VECTOR SAVED ON ANIMATION FILES (1: YES) :',/
     .  '      VELOCITY VECTOR . . . . . ',I5/
     .  '      DISPLACEMENT VECTOR . . . ',I5/
     .  '      ACCELERATION VECTOR . . . ',I5/
     .  '      CONTACT FORCES. . . . . . ',I5/
     .  '      INTERNAL FORCES . . . . . ',I5/
     .  '      EXTERNAL FORCES . . . . . ',I5/
     .  '      SECTION RBY RWALL FORCES. ',I5/
     .  '      ROTATIONAL VELOCITY VECTOR',I5/
     .  '      CONTACT PRESSURE (VECTORS)',I5/
     .  ' SHELL TENSOR SAVED ON ANIMATION FILES (1: YES) :',/
     .  '      MEMBRANE STRESS . . . . . ',I5/
     .  '      BENDING STRESS(MOMENT/T^2)',I5/
     .  '      UPPER LAYER STRESS. . . . ',I5/
     .  '      LOWER LAYER STRESS. . . . ',I5)
 1140 FORMAT(
     .  '      MEMBRANE STRAIN . . . . . ',I5/
     .  '      CURVATURE . . . . . . . . ',I5/
     .  '      UPPER LAYER STRAIN. . . . ',I5/
     .  '      LOWER LAYER STRAIN. . . . ',I5/
     .  ' NODAL MASS SAVED ON ANIMATION FILES (1: YES) :',I5/
     .  ' KEEP DELETED ELEMENT (1: YES) :',I5//)
 1150 FORMAT(
     .  ' INITIAL TIME STEP. . . . . . . . . . . . . . . . ',G14.7/
     .  ' MAXIMUM TIME STEP. . . . . . . . . . . . . . . . ',G14.7)
 1160 FORMAT(/
     .  ' IMPLICIT : CONJUGATED GRADIENT '   /
     .  ' GLOBAL CONVERGENCE PRECISION . . . . . . . . . . ',G14.7/
     .  ' INCREMENTAL CONVERGENCE PRECISION. . . . . . . . ',G14.7/
     .  ' MAXIMUM NUMBER OF ITERATIONS . . . . . . . . . . ',I5)
 1171 FORMAT(/
     .  ' DYNAMIC RELAXATION  '   /
     .  ' NODE GROUP ID (=0 ALL NODES) . . . . . . . . . . ',I10/
     .  ' BETA . . . . . . . . . . . . . . . . . . . . . . ',G14.7/
     .  ' PERIOD . . . . . . . . . . . . . . . . . . . . . ',G14.7)
 1172 FORMAT(/
     .  ' KINEMATIC RELAXATION  '   /
     .  ' NODE GROUP ID (=0 ALL NODES) . . . . . . . . . . ',I10)
 1180 FORMAT(/
     .  ' PARALLEL ARITHMETIC OFF')
 1181 FORMAT(/
     .  ' PARALLEL ARITHMETIC ON')
 1182 FORMAT(/
     .  ' PARALLEL ARITHMETIC FLAG . . . . . . . . . . . . ',I5)
 1196 FORMAT(/
     .  ' QUASI-COMPRESSIBLE FORMULATION ON (/INCMP)')
 1198 FORMAT(
     .  '          MOMENTUM : MIXED INTEGRATION')
 1199 FORMAT(
     .  '          MOMENTUM : VOLUME INTEGRATION')
 1200 FORMAT(
     . 28X,' ALPHA   : DONEA COEFFICIENT. . . . . . . . . . . ',G14.7/
     . 28X,' GAMMA   : GRID VELOCITY LIMITATION FACTOR. . . . ',G14.7/
     . 28X,' FscaleX : X-GRID VELOCITY SCALE FACTOR . . . . . ',G14.7/
     . 28X,' FscaleY : Y-GRID VELOCITY SCALE FACTOR . . . . . ',G14.7/
     . 28X,' FscaleZ : Z-GRID VELOCITY SCALE FACTOR . . . . . ',G14.7/
     . 28X,' VOLMIN  : MINIMUM VOLUME FOR ELEMENT DELETION. . ',G14.7//)
 1220 FORMAT(
     . 28X,' UMAX  : MAXIMUM ABSOLUTE GRID VELOCITY . . . . . ',G14.7/
     . 28X,' CWX   : X GRID VELOCITY SCALE FACTOR . . . . . . ',G14.7/
     . 28X,' CWY   : Y GRID VELOCITY SCALE FACTOR . . . . . . ',G14.7/
     . 28X,' CWZ   : Z GRID VELOCITY SCALE FACTOR . . . . . . ',G14.7/
     . 28X,' VMIN  : MINIMUM VOLUME FOR ELEMENT DELETION. . . ',G14.7//)
 1250 FORMAT(
     . 28X,' DT0   : TYPICAL TIME STEP. . . . . . . . . . . . ',G14.7/
     . 28X,' DT0*  : EFFECTIVE TIME STEP. . . . . . . . . . . ',G14.7/
     . 28X,' GAMMA : NON LINEARITY FACTOR . . . . . . . . . . ',G14.7/
     . 28X,' ETA   : DAMPING COEFFICIENT  . . . . . . . . . . ',G14.7/
     . 28X,' NU    : SHEAR FACTOR . . . . . . . . . . . . . . ',G14.7/
     . 28X,' VOLMIN: MINIMUM VOLUME FOR ELEMENT DELETION. . . ',G14.7//)
 1254 FORMAT(
     . 28X,' ALPHA : STABILITY FACTOR . . . . . . . . . . . . ',G14.7/
     . 28X,' GAMMA : NON LINEARITY FACTOR . . . . . . . . . . ',G14.7/
     . 28X,' BETA  : DAMPING COEFFICIENT. . . . . . . . . . . ',G14.7/
     . 28X,' LC    : CHARACTERISTIC LENGTH. . . . . . . . . . ',G14.7//)
 1257 FORMAT(
     . 28X,' SCALE_DEF : SCALE FACTOR FOR DEFORMATION . . . . ',G14.7/
     . 28X,' SCALE_ROT : SCALE FACTOR FOR ROTATION. . . . . . ',G14.7//)
 1300 FORMAT(
     .  ' NUMBER OF INTERFACES TO BE ELIMINATED. . . . . . ',I8//
     .  ' NUMBER OF SOLID  ELEMENT BLOCKS TO BE ELIMINATED ',I8//
     .  ' NUMBER OF QUAD   ELEMENT BLOCKS TO BE ELIMINATED ',I8//
     .  ' NUMBER OF SHELL  ELEMENT BLOCKS TO BE ELIMINATED ',I8//
     .  ' NUMBER OF TRUSS  ELEMENT BLOCKS TO BE ELIMINATED ',I8//
     .  ' NUMBER OF BEAM   ELEMENT BLOCKS TO BE ELIMINATED ',I8//
     .  ' NUMBER OF SPRING ELEMENT BLOCKS TO BE ELIMINATED ',I8//
     .  ' NUMBER OF SH_3N  ELEMENT BLOCKS TO BE ELIMINATED ',I8//
     .  ' NUMBER OF  SPH PARTICLES BLOCKS TO BE ELIMINATED ',I8/)
 1400 FORMAT(
     .  ' NUMBER OF RIGID LINKS. . . . . . . . . . . . . . ',I8/)
 1450 FORMAT(
     .  ' NUMBER OF ALE LINKS. . . . . . . . . . . . . . . ',I8/)
 1500 FORMAT(
     .  ' NUMBER OF NEW BOUNDARY CONDITIONS. . . . . . . . ',I8/)
 1550 FORMAT(
     .  ' REMOVE INTER.7 SEGMENT AFTER SHELL FAILURE(1 YES)',I5/)
 1810 FORMAT(///' LIST OF ELIMINATED SOLID  ELEMENTS ')
 1820 FORMAT(///' LIST OF ELIMINATED QUAD   ELEMENTS ')
 1830 FORMAT(///' LIST OF ELIMINATED SHELL  ELEMENTS ')
 1840 FORMAT(///' LIST OF ELIMINATED TRUSS  ELEMENTS ')
 1850 FORMAT(///' LIST OF ELIMINATED BEAM   ELEMENTS ')
 1860 FORMAT(///' LIST OF ELIMINATED SPRING ELEMENTS ')
 1870 FORMAT(///' LIST OF ELIMINATED SH_3N  ELEMENTS ')
 1880 FORMAT(///' LIST OF ELIMINATED   SPH PARTICLES ')
 2000 FORMAT(/' INTERFACE NUMBER',I10,'    IS ELIMINATED')
 2100 FORMAT(/' RIGID LINK:',I5,
     .        ' TRANSLATION X,Y,Z',3(1X,I1),
     .        ' ROTATION    X,Y,Z',3(1X,I1),
     .        ' SKEW    ',I10,/
     .        ' ----------  ',I10,' NODES :')
 2200 FORMAT(/' ALE LINK:      ',/,
     .        ' --------',/,
     .          '     MAIN NODES :  ',2I8,/,
     .          '     (X,Y,Z)      : (',I1,',',I1,',',I1,')',/,
     .          '     TYPE         :  ',I2)
 2201 FORMAT(   '     NODES        :')
 2202 FORMAT(   '     NODE GROUP   :  ',I2)
 2210 FORMAT(' DEACTIVATING ALE LINK ID:',I5)
 2211 FORMAT('   ACTIVATING ALE LINK ID:',I5)
 2300 FORMAT(/' INTERFACE CHANGES'/
     .  ' INTERFACE NB . . . . . . . . . . . . . . . . . . ',I10/
     .  ' SEARCH OF CLOSEST NODES EACH NSEARCH TIME STEPS. ',I5/
     .  ' START TIME . . . . . . . . . . . . . . . . . . . ',G14.7/
     .  ' STOP  TIME . . . . . . . . . . . . . . . . . . . ',G14.7)

 2301 FORMAT(/' ERROR IN INTERFACE CHANGES'/
     .  ' INTERFACE NB IS NOT EXISTING . . . . . . . . . . ',I10)

 4000 FORMAT(/
     .  ' SPMD PARALLEL RESTART READING (PREAD) ACTIVATED')
 4100 FORMAT(/
     .  ' SPMD ELEMENT WEIGHT ESTIMATION (DDW) ACTIVATED')
C
 4500 FORMAT(/
     & 1X,'EXTERNAL LIBRARY FOR USERS CODE INTERFACE  ')
 4600 FORMAT(
     & 1X,'LIBRARY NAME  . . . . . . . . . . . . . . . . . . . . ',A/
     & 1X,'RADIOSS USERS CODE INTERFACE VERSION  . . . . . . . .',I10//)
 4700 FORMAT(
     .  ' BRICK TIME STEP SCALE FACTOR . . . . . . . . . . ',G14.7/
     .  ' BRICK MINIMUM TIME STEP. . . . . . . . . . . . . ',G14.7/
     .  ' MIN. TIME STEP FLAG (1:STOP RUN, 2:DELETE 3:CST )',I5//
     .  ' BRICK_CST MINIMUM ASPECT RATIO (Tet collapse). . ',G14.7/
     .  ' BRICK_CST MINIMUM VOLUME CHANGE . . . . . . . .  ',G14.7//
     .  ' QUAD TIME STEP SCALE FACTOR. . . . . . . . . . . ',G14.7/
     .  ' QUAD MINIMUM TIME STEP . . . . . . . . . . . . . ',G14.7/
     .  ' MIN. TIME STEP FLAG (1:STOP RUN, 2:DELETE QUAD). ',I5//
     .  ' SHELL TIME STEP SCALE FACTOR . . . . . . . . . . ',G14.7/
     .  ' SHELL MINIMUM TIME STEP. . . . . . . . . . . . . ',G14.7/
     .  ' MIN. TIME FLAG (1:STOP, 2:DELETE, 3:SMALL STRAIN)',I5//
     .  ' TRUSS TIME STEP SCALE FACTOR . . . . . . . . . . ',G14.7/
     .  ' TRUSS MINIMUM TIME STEP. . . . . . . . . . . . . ',G14.7/
     .  ' MIN. TIME FLAG (1:STOP, 2:DELETE). . . . . . . . ',I5//
     .  ' BEAM TIME STEP SCALE FACTOR. . . . . . . . . . . ',G14.7/
     .  ' BEAM MINIMUM TIME STEP . . . . . . . . . . . . . ',G14.7/
     .  ' MIN. TIME FLAG (1:STOP, 2:DELETE). . . . . . . . ',I5//
     .  ' SPRING TIME STEP SCALE FACTOR. . . . . . . . . . ',G14.7/
     .  ' SPRING MINIMUM TIME STEP . . . . . . . . . . . . ',G14.7/
     .  ' MIN. TIME FLAG (1:STOP, 2:DELETE). . . . . . . . ',I5//
     .  ' AIRBAG TIME STEP SCALE FACTOR. . . . . . . . . . ',G14.7/
     .  ' AIRBAG MINIMUM TIME STEP . . . . . . . . . . . . ',G14.7/
     .  ' MIN. TIME FLAG (1:STOP). . . . . . . . . . . . . ',I5//
     .  ' INTERFACE TYPE 7 TIME STEP SCALE FACTOR. . . . . ',G14.7/
     .  ' INTERFACE TYPE 7 MINIMUM TIME STEP . . . . . . . ',G14.7/
     .  ' MIN. TIME FLAG(1:STOP, 2:REMOVE NODE FROM INTERF)',I5/ )
 4720 FORMAT(
     .  ' ACCURATE TIME STEP FOR SHELLS IS USED. . . . . . ',/)
 4730 FORMAT(
     .  ' ACCURATE TIME STEP FOR 4-NODE TETRAHEDRA IS USED   ',/)
 4740 FORMAT(
     .  ' ACCURATE TIME STEP FOR 10-NODE TETRAHEDRA IS USED  ',/)
 4800 FORMAT(
     .  ' PART DEACTIVATED FOR ALE / EULER COMPUTATION',2X,I10)
 5000 FORMAT(/' H3D FILES :                  '/
     .  ' TIME FOR FIRST H3D-FILE PLOT . . . . . . . . . . ',G14.7/
     .  ' TIME INTERVAL FOR H3D-FILE PLOTS . . . . . . . . ',G14.7/
     .  ' VARIABLE SAVED ON H3D FILES : ')
 5001 FORMAT(/
     .  ' KINEMATIC RELAXATION  '   /
     .  ' NODE GROUP ID (=0 ALL NODES) . . . . . . . . . . ',I10/
     .  '  START TIME. . . . . . . . . . . . . . . . . . . ',G14.7/
     .  '  STOP  TIME. . . . . . . . . . . . . . . . . . . ',G14.7)
 5010 FORMAT(/
     .  ' ADAPTIVE DYNAMIC RELAXATION  '  ,/
     .  ' NODE GROUP ID (=0 ALL NODES) . . . . . . . . . . ',I10/)
 5011 FORMAT(/
     .  ' ADAPTIVE DYNAMIC RELAXATION  '   /
     .  ' NODE GROUP ID (=0 ALL NODES) . . . . . . . . . . ',I10/
     .  '  START TIME. . . . . . . . . . . . . . . . . . . ',G14.7/
     .  '  STOP  TIME. . . . . . . . . . . . . . . . . . . ',G14.7)
 5020 FORMAT(
     .  ' BRICK_DEL MINIMUM COLLAPSE RATIO. . . . . . . . . ',G14.7/
     .  ' BRICK_DEL MINIMUM VOLUME CHANGE . . . . . . . . . ',G14.7/
     .  ' BRICK_DEL MAXIMUM ASPECT RATIO  . . . . . . . . . ',G14.7/
     .  ' BRICK_DEL MAXIMUM VOLUME CHANGE . . . . . . . . . ',G14.7//)

      END
Chd|====================================================================
Chd|  PROUT_BUCK                    source/input/lectur.F         
Chd|-- called by -----------
Chd|        LECIMPL                       source/input/lectur.F         
Chd|-- calls ---------------
Chd|        IMP_KBCS                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE PROUT_BUCK(IP,NBUCK,IBUCK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_KBCS
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IP,NBUCK,IBUCK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER*25  MSG_BSOL(2)
      DATA
     .            MSG_BSOL
     . / 'BCS',
     .   '-' /
C-----------------------------------------------
       IF (IBUCK > 0) THEN
        WRITE(IP,1000)
       ELSE
        WRITE(IP,2000)
       END IF
       WRITE(IP,3000)NBUCK,SHIFT_B,EMIN_B,EMAX_B,MSGL_B,MAXSET_B, MSG_BSOL(1)

      RETURN
 1000 FORMAT(
     .  ' EULER BUCKLING ANALYSIS (RESTART):'/)
 2000 FORMAT(
     .  ' EULER BUCKLING ANALYSIS :'/)
 3000 FORMAT(
     .  ' NUMBER OF MODES TO BE COMPUTED: . . . . . ',2X,I5/
     .  ' SHIFT IN BUCKLING MODES PENCIL: . . . . . ',2X,G14.7/
     .  ' MINIMUM EIGENVALUE: . . . . . . . . . . . ',2X,G14.7/
     .  ' MAXIMUM EIGENVALUE: . . . . . . . . . . . ',2X,G14.7/
     .  ' OUTPUT MESSAGE LEVEL: . . . . . . . . . . ',2X,I5/
     .  ' NUMBER OF VECTORS IN BLOCK OR SET:  . . . ',2X,I5/
     .  ' LINEAR SOLVER:  . . . . . . . . . . . . . ',2X,A/)
      END
Chd|====================================================================
Chd|  LECIMPL                       source/input/lectur.F         
Chd|-- called by -----------
Chd|        LECTUR                        source/input/lectur.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FLOATMIN                      ../common_source/tools/math/precision.c
Chd|        PROUT_BUCK                    source/input/lectur.F         
Chd|        SPMD_MAX_S                    source/mpi/implicit/imp_spmd.F
Chd|        IMP_KBCS                      share/modules/impbufdef_mod.F 
Chd|        IMP_PCG_PROJ                  share/modules/impbufdef_mod.F 
Chd|        IMP_SPBRM                     share/modules/impbufdef_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LECIMPL
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_KBCS
      USE IMP_PCG_PROJ
      USE IMP_SPBRM
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
#include      "impl2_c.inc"
#include      "buckcom.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,NTY,IPRINT,IBID,ISOL
      my_real CS1(2),S
      REAL FLMIN
      CHARACTER*25  MSG_TYPE(9),MSG_ISOL(9),MSG_INSOL(4),MSG_PRE(5),MSG_BSOL(3)
      DATA
     .            MSG_TYPE
     . / 'STATIC LINEAR',
     .   'STATIC NON-LINEAR',
     .   'DYNAMIC NON-LINEAR',
     .   'MODAL ANALYSIS',
     .   'CHECK',
     .   'QUASI-STATIC LINEAR',
     .   'QUASI-STATIC NON-LINEAR',
     .   'BUCKLING',
     .   'RADIOSS-AcuSolve DC-FSI'/,
     .            MSG_ISOL
     . / 'PREC. CONJUGATE GRADIENT',
     .   'DIRECT(MUMPS) ',
c     .   'PREC. LANZOS ',
     .   'DIRECT',
     .   'MULTI-LEVEL CONDENSATION',
     .   'MIXE SOLVERS: 1 + 3',
     .   'MIXE SOLVERS: 1 + 4',
     .   'AUTO SELECT SOLVER ',
     .   'PCG(GPU) ',
     .   'PCGP(PROJECTION) '/,
     .            MSG_INSOL
     . / 'MODIFIED NEWTON',
     .   'QUASI-NEWTON',
     .   'SLOAN ELASTOPLAS',
     .   'FULL NEWTON'/,
     .            MSG_PRE
     . / 'NONE',
     .   'DIAGONAL JACOBIEN',
     .   'IMCOMP. CHOLESKY(0)',
     .   'STABLILIZED IC(0)',
     .   'FACTORED APPRO. INVERSES'/,
     .            MSG_BSOL
     . / 'SUPERLU',
     .   'MUMPS',
     .   'MULTI-LEVEL CONDENSATION'/
C-----------------------------------------------
       IPRINT=0
C       IMPMV>0 isolv/=1 --> IMPMV=0
       IF (ISPMD == 0) IPRINT=1
       IF(ISOLV >= 3 ) THEN
          CALL ANCMSG(MSGID=296,ANMODE=ANINFO,I1=ISOLV)
          ISOLV = 2
       END IF

#ifndef MUMPS5
       IF(ISOLV == 2 .AND. NSPMD > 1) THEN
         WRITE(6,*) "Fatal error: MUMPS is required, but not available"
         CALL ARRET(5)
       ENDIF
#endif
       IF (NEIG > 0) THEN
        ILINE=1
        ISPRB=0
        IQSTAT=0
        IF (NSPMD == 1) THEN
           IPREC=1
        ELSE
           IPREC=5
        ENDIF
       ENDIF
       IF (NBUCK > 0) THEN
        ILINE=1
C----fix mono w/ BCS solver
        IF (NSPMD == 1) ISOLV =2
       ENDIF
C--------solvers----
C machine  precision minimum -simple
       CALL FLOATMIN(CS1(1),CS1(2),FLMIN)
       P_MACH = TWO*SQRT(FLMIN)
       IF (NSPMD > 1)CALL SPMD_MAX_S(P_MACH)
C  AUTO SELECT SOLVER
       IF (ISOLV == 7 ) THEN

        IF (ILINE == 1) THEN

           WRITE(IOUT,*) ' ** WARNING ** : SOLVER AUTO SELECT IS NOT ','COMPATIBLE WITH LINEAR RUN '
           WRITE(IOUT,*) ' ** RESETTING TO **  : DEFAULT ONE  '

           WRITE(ISTDO,*) ' ** WARNING ** : SOLVER AUTO SELECT IS NOT ','COMPATIBLE WITH LINEAR RUN '
           WRITE(ISTDO,*) ' ** RESETTING TO **  : DEFAULT ONE  '

           ISOLV = 0

        END IF

       ENDIF
C----- default solver-> MUMPS
       IF (ISOLV == 0) ISOLV = 2
C-------ISOLV=2 --> use MUMPS anyway
       IMUMPSV = 0
       IF (ISOLV > 1 .AND. ISOLV <9 ) IMUMPSV = 1
       IF (ISOLV == 2 .OR. ISOLV ==3 ) THEN
        IMUMPSV = 1
        ISOLV = 3
       END IF
       IF ((ISOLV == 3.OR.ISOLV == 4).AND.INTP_C < 0) THEN
        IF(IPRINT==1) THEN
         WRITE(IOUT,*) ' ** WARNING ** : DIRECT SOLVER IS NOT ','COMPATIBLE WITH IMPL/INTER/KNONL OPTION '
         WRITE(IOUT,*) ' ** RESETTING TO **  : MIXE ONE  '
        ENDIF
        ISOLV = ISOLV + 2
       ENDIF
C-------ISOLV=9 -> PCG w/ Projection
        IF (ISOLV == 9) THEN
         IF (M_VS ==0) M_VS=20
         IF (IPRO_S0 ==0) IPRO_S0=4
        END IF
       IF (M_VS > 0) THEN
C------case /IMPL/PROJV/n w/o /SOLV/9
        IF (ISOLV /= 9) THEN
         WRITE(IOUT,*) ' ** WARNING ** : /IMPL/PROJV IS ','ONLY COMPATIBLE WITH PCGP SOLVER  '
         WRITE(IOUT,*) ' ** CHANGE TO **  : ISOLV=9  '
         ISOLV = 9
        END IF
       END IF

       IF (NBUCK > 0.AND.NSPMD == 1.AND.ISOLV /= 3) THEN
         WRITE(IOUT,*) ' ** WARNING ** : /IMPL/BUCKL IS ','ONLY COMPATIBLE WITH BCS SOLVER '
         WRITE(IOUT,*) ' ** CHANGE TO **  : ISOLV=3  '
         ISOLV = 3
       ENDIF

       IF (IPREC == 0.OR.IPREC > 6)IPREC=5
       IF (IMP_CHK > 0) THEN
          NTY = 5
          IF (IPREC /= 5) IPREC=5
          IF (ILINE /= 1) ILINE=1
          IF (ISOLV /= 1) ISOLV=1
          IF (D_TOL /= ZERO) D_TOL = ZERO
       ENDIF

       IF (ISOLV == 2) THEN
        IF (ITOL == 0.OR.ITOL > 1) ITOL=2
        IF (L_TOL == ZERO) L_TOL=P_MACH
       ELSEIF (ISOLV == 1.OR.ISOLV>=7) THEN
        IF (ITOL == 0) ITOL=3
        IF (ITOL > 4) THEN
         IF(IPRINT==1) THEN
         WRITE(IOUT,*) ' ** WARNING ** : WRONG LINEAR STOP ','CRITERION NUMBER, RESET TO DEFAULT ONE ** '
         ENDIF
         ITOL=3
        ENDIF
        IF (L_TOL == ZERO) THEN
         IF (ITOL == 3) THEN
          L_TOL=P_MACH
          IF (N_PAT > 1.OR.ISOLV == 9) L_TOL=P_MACH*EM01
         ELSE
          L_TOL=EM5
         ENDIF
        ENDIF
C--------direct-----
       ELSEIF (ISOLV == 3.OR.ISOLV == 4) THEN
        IPREC=1
        IF (NSPMD>1) IMUMPSV = 1
C--------MIX-----
       ELSEIF (ISOLV == 5.OR.ISOLV == 6) THEN
        IPREC=1
        IMUMPSV = 1
        IF (ITOL == 0) ITOL=1
        IF (ITOL > 4) THEN
         IF(IPRINT==1) THEN
         WRITE(IOUT,*) ' ** WARNING ** : WRONG LINEAR STOP ','CRITERION NUMBER, RESET TO DEFAULT ONE ** '
         ENDIF
         ITOL=3
        ENDIF
        IF (L_TOL == ZERO) THEN
         IF (ITOL == 3) THEN
          L_TOL=P_MACH*EM01
         ELSE
          L_TOL=EM5
         ENDIF
        ENDIF
       ELSE
         IF (ISPMD == 0)THEN
          WRITE(IOUT,*) ' ** WARNING ** : SOLVER NON AVAILABLE '
          WRITE(IOUT,*) ' ** RESETTING TO **  : DEFAULT ONE  '
         ENDIF
         ISOLV = 1
       ENDIF

        IF (IPREC > 2.AND.IPREC /= 5) THEN
         IF (ISPMD == 0)THEN
         WRITE(IOUT,*) ' ** WARNING ** : ',
     .                'THIS PRECONDITION METHOD IS NO MORE SUPPORTED '
         WRITE(IOUT,*) ' ** RESETTING TO **  : DEFAULT ONE  '
         ENDIF
         IPREC = 5
        ENDIF

       IF (N_PAT > 1) THEN
         IF (N_PAT > 4) THEN
          IF(IPRINT==1) THEN
          WRITE(IOUT,*) ' ** WARNING ** : UNAVAILABLE PRECONDITION',' MATRIX PATTERN, RESET TO 4 ** '
          ENDIF
          N_PAT = 4
         ENDIF
         IF (IPREC /= 5) THEN
          IF(IPRINT==1) THEN
           WRITE(IOUT,*) ' ** WARNING ** : INPUT PRECONDITION MATRIX',' PATTERN ONLY AVAILABLE WITH IPREC=5 : IGNORED **'
          ENDIF
          N_PAT = 1
         ENDIF
       ENDIF

       P_MACH = TWO*FLMIN
       IF (NVOLU>0 .AND. IMPMV > 0 .AND. ISOLV/=1) THEN
        IF(IPRINT==1) THEN
         WRITE(IOUT,*) ' ** WARNING ** : DIRECT SOLVER IS NOT ','COMPATIBLE WITH MONITORED VOLUME TYPE3 '
         WRITE(IOUT,*) ' ** STIFFNESS WILL BE IGNORED **  '
        ENDIF
        IMPMV = 0
       END IF

       IF (ILINE == 1) THEN
        NTY=1
        IF (IQSTAT > 0) NTY=6
        INSOLV = 0
        IF (ILINTF > 0) IMP_INT7 = 2
        IF (INTP_C < 0) ITTOFF = 1
        IF (IKT > 0) IKT=0
        NDTFIX = 0
        IF (NBUCK == 0) IKPRES = 0
        IDTC = 0
        IF (ISCAU > 0) THEN
         IF (ISMDISP > 0) THEN
          IF(IPRINT==1) THEN
         WRITE(IOUT,*) ' ** WARNING ** : SMALL DISPLACEMENT IS IGNORED',' UNCOMPATIBLE WITH CAUCHY STRESS OUTPUT OPTION;'
          ENDIF
          ISMDISP = 0
         ENDIF
        ELSE
         ISMDISP = 1
        END IF
       ELSE
C--------nonlinear parametres-----
        IF (ISPRB == 1.AND.IDYNA > 0) THEN
         IF(IPRINT==1) THEN
         WRITE(IOUT,*) ' ** WARNING ** : DYNAMIC IMPLICIT IS NOT ', 'COMPATIBLE WITH IMPL/SPRBACK OPTION '
         WRITE(IOUT,*) ' ** RESETTING **  : STATIC ONE  '
         ENDIF
         IDYNA=0
        ENDIF
        IF (ISPRB == 1.AND.ISIGINI > 0) THEN
         IF(IPRINT==1) THEN
         WRITE(IOUT,*) ' ** WARNING ** : PRE-STRESSES OPTION IS NOT ', 'COMPATIBLE WITH IMPL/SPRBACK OPTION '
         WRITE(IOUT,*) ' ** DEACTIVATING IT  '
         ENDIF
         ISIGINI=0
        ENDIF

        IF (IQSTAT > 1) IQSTAT=1

        IF (IDYNA > 0) THEN
         NTY=3
         IF (IQSTAT > 0) THEN
          IF(IPRINT==1) THEN
          WRITE(IOUT,*) ' ** WARNING ** : QUASI-SATIC IMPLICIT IS NOT ','COMPATIBLE WITH DYNAMIC OPTION '
          WRITE(IOUT,*) ' ** RESETTING **  : DYNAMIC ONE  '
          ENDIF
          IQSTAT=0
         END IF
         IF (IDYNA == 2) THEN
          IF (NEWM_A < HALF) NEWM_A = HALF
          IF (NEWM_B < HALF*NEWM_A) NEWM_B = HALF*NEWM_A
         ELSE
          IF (HHT_A == ZERO) THEN
           HHT_A=-ZEP05
          ELSE
           HHT_A=MIN(ZERO,HHT_A)
           HHT_A=MAX(-THIRD,HHT_A)
          ENDIF
         ENDIF
         NEXP = 1
        ELSE
         NTY=2
         IF (IQSTAT > 0) NTY=7
        END IF !(IDYNA > 0)
C--------------Non linear solvers
        IF (INSOLV > 4)THEN
         IF(IPRINT==1) THEN
          WRITE(IOUT,*) ' ** WARNING ** : WRONG NONLINEAR SOLVER '
          WRITE(IOUT,*) ' ** RESETTING TO**  : DEFAULT ONE '
         ENDIF
         INSOLV = 0
        ENDIF
        IF (INSOLV==4)THEN
         IKT=4
         N_LIM=1
         IF (ISOLV==5 .OR. ISOLV==6) ISOLV=3
        ENDIF

        IF (INSOLV == 0)INSOLV = 1
C--------stop criteria-------
        IF (NITOL < 12) THEN
         IF (NITOL > 3) THEN
          IF(IPRINT==1) THEN
         WRITE(IOUT,*) ' ** WARNING ** : WRONG NONLINEAR STOP ',
     .                'CRITERION NUMBER, RESET TO DEFAULT ONE ** '
          ENDIF
          NITOL=0
         ENDIF
         IF (NITOL == 0) NITOL=2
         IF (NITOL == 1) THEN
           IF (N_TOL == ZERO) N_TOL=EM3
         ELSEIF (NITOL == 2) THEN
           IF (N_TOL == ZERO) N_TOL=FIVE*EM3
         ELSEIF (NITOL == 3) THEN
           IF (N_TOL == ZERO) N_TOL=EM3
         ENDIF
C        IF (N_TOL == ZERO) N_TOL=EM3
        ELSEIF (NITOL == 12) THEN
         IF (N_TOLE == ZERO) N_TOLE=EM3
         IF (N_TOLF == ZERO) N_TOLF=EM02
        ELSEIF (NITOL == 23) THEN
         IF (N_TOLU == ZERO) N_TOLU=EM02
         IF (N_TOLF == ZERO) N_TOLF=EM02
        ELSEIF (NITOL == 13) THEN
         IF (N_TOLU == ZERO) N_TOLU=EM02
         IF (N_TOLE == ZERO) N_TOLE=EM3
        ELSEIF (NITOL == 123) THEN
         IF (N_TOLU == ZERO) N_TOLU=EM02
         IF (N_TOLE == ZERO) N_TOLE=EM3
         IF (N_TOLF == ZERO) N_TOLF=EM02
        ELSE
            CALL ANCMSG(MSGID=132,ANMODE=ANINFO,
     .            I1=NITOL)
            CALL ARRET(2)
        END IF !(NITOL < 12) THEN
C
        IF (IDTC == 1) THEN
         IF (NL_DTP == 0) NL_DTP=4
         IF (NL_DTN == 0) NL_DTN=15
         IF (SCAL_DTN == ZERO) SCAL_DTN=TWO_THIRD
         IF (SCAL_DTP == ZERO) SCAL_DTP=ONEP01
        ELSEIF (IDTC == 2) THEN
         IF (NL_DTP == 0) NL_DTP=6
         IF (NL_DTN == 0) NL_DTN=20
         IF (IDYNA == 0) THEN
          IF (SCAL_DTN == ZERO) SCAL_DTN=TWO_THIRD
          IF (SCAL_DTP == ZERO) SCAL_DTP=ONEP1
         ELSE
          IF (SCAL_DTN == ZERO) SCAL_DTN=TWO_THIRD
          IF (SCAL_DTP == ZERO) SCAL_DTP=ONEP1
         ENDIF
C-------ALEN=ALEN0 IF ALEN0=0 sinon ALEN est calcule auto
C-------RIKS ARC LENGTH METHOD ------
        ELSEIF (IDTC == 3) THEN
         IF (NL_DTP == 0) NL_DTP=12
         IF (NL_DTN == 0) NL_DTN=25
         IF (SCAL_DTN == ZERO) SCAL_DTN=TWO_THIRD
         IF (SCAL_DTP == ZERO) SCAL_DTP=ONEP2
         IF (IAL_M == 0) IAL_M = 2
         IF (NDTFIX > 0) THEN
          IF(IPRINT==1) THEN
          WRITE(ISTDO,*)
     .        ' ** WARNING :RIKS METHOD IS NOT ','COMPATIBLE WITH FIXED TIME POINT '
          WRITE(ISTDO,*) ' ** FIXED TIME POINT : DEACTIVATED  '
          WRITE(IOUT,*)' ** WARNING :RIKS METHOD IS NOT ','COMPATIBLE WITH FIXED TIME POINT '
          WRITE(IOUT,*) ' ** FIXED TIME POINT : DEACTIVATED  '
          NDTFIX = 0
          IDTFIX = 0
         ENDIF
        ENDIF
        ELSE
         IF(.NOT.(ISMDISP > 0.AND.ISOLV < 4)) THEN
         IF(IPRINT==1) THEN
         WRITE(ISTDO,*) ' ** WARNING: NO TIMESTEP CONTROL METHOD DEFINED **'
         WRITE(ISTDO,*)' ** POTENTIAL INFINITE LOOP IF NO CONVERGENCE IS ACHIEVED **'
         WRITE(IOUT,*)' ** WARNING: NO TIMESTEP CONTROL METHOD DEFINED **'
         WRITE(IOUT,*) ' ** POTENTIAL INFINITE LOOP IF NO CONVERGENCE IS ACHIEVED **'
         ENDIF
         END IF !(.NOT.(ISMDISP > 0.AND.ISOLV < 4)) THEN
         IF (SCAL_DTN == ZERO) SCAL_DTN=HALF
        END IF !(IDTC == 1)
C
        IF (DT_MAX == ZERO) DT_MAX=EP10
        IF (DT_MIN == ZERO) DT_MIN=EM10
C
        IF (DT_IMP == ZERO) THEN
         IF(IPRINT==1) THEN
         WRITE(ISTDO,*)' ** WARNING: NO INITIAL TIMESTEP DEFINED **'
         WRITE(IOUT,*)' ** WARNING: NO INITIAL TIMESTEP DEFINED **'
         ENDIF
        ENDIF
C
        IF (RF_MAX == ZERO) RF_MAX=EP30
C
        IF (ISMDISP == 1 ) THEN
         IKG =0
         IKPRES = 0
         IF (IKPROJ == 0 ) IKPROJ =-1
C
         IF (IDYNA > 0 .AND. IDTC > 0.AND.ISOLV < 4 .AND. SCAL_DTP /= ONE) THEN
          IF(IPRINT==1) THEN
           WRITE(ISTDO,*)' ** WARNING: CONST. TIME-STEP WILL BE USED WITH SMALL DISP. **'
           WRITE(IOUT,*)' ** WARNING: CONST. TIME-STEP WILL BE USED WITH SMALL DISP. **'
          ENDIF
          SCAL_DTP = ONE
         ENDIF
        END IF
C
        IF (N_LIM == 0) THEN
         IF (ISOLV < 3.OR.IDTC == 3) THEN
           N_LIM=3
         ELSEIF (ISOLV == 5.OR.ISOLV == 6) THEN
           N_LIM=3
         ELSE
           N_LIM=6
         END IF
        END IF
C
       END IF !IF (ILINE == 1) THEN

       IF (IMP_CHK > 0) THEN
         NTY = 5
         IF (IQSTAT > 0) IQSTAT=1
       ENDIF

       IF (NBUCK > 0) NTY = 8
       IF (ISOLV == 3.AND.IMUMPSD == 0) IMUMPSD=1
C
       IF (ISOLV == 5) IMUMPSD=1
C
C-----------attention initialization should not be inside below--
       IF(IPRINT==1) THEN
        IF (NSPMD == 1.AND.ISOLV == 3) THEN
           MSG_ISOL(ISOLV)='DIRECT (BCS)'
        ELSEIF (IMACH == 3.AND.ISOLV == 3) THEN
           MSG_ISOL(ISOLV)='DIRECT (MUMPS)'
        ENDIF
        IF (NEIG == 0) THEN
         IF(ISOLV == 3.OR.ISOLV == 4) THEN
          ISOL = ISOLV
          IF (IMUMPSV==1) ISOL = 2
          WRITE(IOUT,5010)MSG_TYPE(NTY),MSG_ISOL(ISOL),LPRINT
         ELSE
          ISOL=MIN(9,ISOLV)
          WRITE(IOUT,5000)MSG_TYPE(NTY),MSG_ISOL(ISOL),MSG_PRE(IPREC),ITOL,L_LIM,L_TOL,LPRINT
         ENDIF
        IBID =0
        IF (INTP_C < 0) IBID =1
         WRITE(IOUT,5020)IKG,IKPRES,IAUTSPC,ISPRB,IBID
        ELSE
          WRITE(IOUT,5050)MSG_TYPE(4)
        ENDIF
        IF (M_VS > 0) WRITE(IOUT,8400)M_VS,IPRO_S0
        IF (IKPROJ /= 0) WRITE(IOUT,7600)IKPROJ
C
        IF (ILINE /= 1) THEN
         IBID = INSOLV
         IF (INSOLV==4) INSOLV=1
         IF (IBID == 5) IBID = 3
         IF (NITOL < 12) THEN
         WRITE(IOUT,5100)MSG_INSOL(IBID),DT_IMP,NITOL,N_TOL
         ELSEIF (NITOL < 123) THEN
          IF (NITOL == 12) THEN
          WRITE(IOUT,5112)MSG_INSOL(IBID),DT_IMP,NITOL,N_TOLE,N_TOLF
          ELSEIF (NITOL == 23) THEN
           WRITE(IOUT,5123)MSG_INSOL(IBID),DT_IMP,NITOL,N_TOLF,N_TOLU
          ELSEIF (NITOL == 13) THEN
           WRITE(IOUT,5113)MSG_INSOL(IBID),DT_IMP,NITOL,N_TOLE,N_TOLU
          ENDIF
         ELSEIF (NITOL == 123) THEN
          WRITE(IOUT,5132)MSG_INSOL(IBID),DT_IMP,NITOL,N_TOLE,N_TOLF,
     .                    N_TOLU
         ENDIF
C
         IF(ISOLV == 5.OR.ISOLV == 6) THEN
          WRITE(IOUT,5150)IPUPD,N_LIM,NPRINT,ISIGINI,IRREF,IDTC,DT_MIN,DT_MAX
         ELSE
          WRITE(IOUT,5180)N_LIM, NPRINT,ISIGINI,IRREF,IDTC,DT_MIN,DT_MAX
         ENDIF
C
         IF(IRIG_M == 1) THEN
          WRITE(IOUT,8600)
         ELSEIF(IRIG_M > 1) THEN
          WRITE(IOUT,8700) E_REF(1:3)
         ENDIF
C
         WRITE(IOUT,8010) ISMDISP
         WRITE(IOUT,8200) RF_MIN,RF_MAX
         IF (NCY_MAX > 0) WRITE(IOUT,7900) NCY_MAX
         IF (IDTC == 1) THEN
          WRITE(IOUT,7700)
          WRITE(IOUT,5200)NL_DTP,SCAL_DTP,NL_DTN,SCAL_DTN
         ELSEIF (IDTC == 2) THEN
          WRITE(IOUT,7700)
          WRITE(IOUT,5300)NL_DTP,NL_DTN,SCAL_DTN,SCAL_DTP,ALEN0
         ELSEIF (IDTC == 3) THEN
          S = SCAL_RIKS
          WRITE(IOUT,7800)
          WRITE(IOUT,7650)NL_DTP,NL_DTN,SCAL_DTN,SCAL_DTP,ALEN0,IAL_M,S
          SCAL_RIKS = S*S
         ENDIF
        END IF !(ILINE /= 1)


C
        IF (IDYNA == 2) THEN
          WRITE(IOUT,6300)NEWM_A,NEWM_B
        ELSEIF (IDYNA > 0) THEN
          WRITE(IOUT,5400)HHT_A
        ELSEIF (IQSTAT > 0.AND.SCAL_DTQ /= ONE) THEN
          WRITE(IOUT,6400)SCAL_DTQ
        ENDIF
        IF (IDY_DAMP > 0) WRITE(IOUT,6200)DAMPA_IMP,DAMPB_IMP
        IF (N_PAT > 1) WRITE(IOUT,6100)N_PAT
        IF (ITTOFF > 0) WRITE(IOUT,6500)
        IF (ILINTF > 0) WRITE(IOUT,6600)
        IF (ISCAU > 0) WRITE(IOUT,6700)
        IF (IMP_LR > 0) WRITE(IOUT,6800)
C
       ENDIF ! IF(IPRINT==1) THEN
C
       IF (ILINE /= 1) THEN
         IF (ILINE_S == 0) ILINE_S=3
         IF (ILINE_S == 100) THEN
           ILINE_S=0
           IF(IPRINT==1)WRITE(IOUT,7000)
         ENDIF
       ELSE
         ILINE_S=0
       ENDIF

       IF (ILINE /= 1.AND.ILINE_S > 0) THEN
         IF (NLS_LIM == 0) NLS_LIM=20
         IF (LS_TOL == ZERO) LS_TOL=EM03
         IF(IPRINT==1)WRITE(IOUT,6900)ILINE_S,NLS_LIM,LS_TOL
          IF (ILINE_S == 3) THEN
           IF (NITOL /= 2.AND.NITOL /= 4) THEN
             LS_TOL=FIVE*LS_TOL
           ELSE
             LS_TOL=HALF*LS_TOL
           ENDIF
          ELSEIF (ILINE_S == 2) THEN
            LS_TOL=HALF*LS_TOL
          ENDIF
       ENDIF

       IF (ILINE /= 1.AND.IREFI > 0) THEN
         IF(IPRINT==1)WRITE(IOUT,7100)IREFI
       END IF

       IF (ILINE /= 1.AND.NDIVER == 0) THEN
        IF (IREFI > 2.AND.ILINE_S /= 1) THEN
         NDIVER=2
        ELSEIF(IKT > 0.AND.ILINE_S /= 1) THEN
         NDIVER=0
        ELSE
         NDIVER=1
        END IF
       ELSEIF(NDIVER == -1) THEN
         NDIVER=0
       END IF

       IF (ILINE /= 1.AND.NDIVER > 0) THEN
         IF(IPRINT==1)WRITE(IOUT,7200)NDIVER
         NDIVER = NDIVER + 1
       END IF

       IF (ILINE /= 1) THEN
        IF (TOL_DIV == ZERO ) THEN
         TOL_DIV = EP04
         IF (ILINE_S == 1) TOL_DIV=EP03
         IF (ISMDISP == 1) TOL_DIV=EP10
        ELSE
         IF(IPRINT==1)WRITE(IOUT,8500)TOL_DIV
        END IF
       END IF

       IF (IKT == 1) THEN
        IF(IPRINT==1)WRITE(IOUT,7300)
       ELSEIF (IKT == 2) THEN
        IF(IPRINT==1)WRITE(IOUT,7400)
       ELSEIF (IKT == 3) THEN
        IF(IPRINT==1)WRITE(IOUT,8000)
       ELSEIF (IKT == 4) THEN
        IF(IPRINT==1)WRITE(IOUT,8100)
       END IF

       IF (NDTFIX > 0) THEN
        IF(IPRINT==1) THEN
         WRITE(IOUT,7500) NDTFIX
         WRITE(IOUT,7510)(DTIMPF(J),J=1,NDTFIX)
        END IF
       END IF

       IF (NBUCK > 0) THEN
        IF (IMUMPSV> 0) THEN
          IF (BISOLV /= 1.AND.BISOLV /= 2) THEN
             WRITE(ISTDO,*) ' ** ERROR ** UNAVAILABLE SOLVER FOR BUCKLING ANALYSIS'
             WRITE(IOUT,*)  ' ** ERROR ** UNAVAILABLE SOLVER FOR BUCKLING ANALYSIS'
             CALL ARRET(2)
          ENDIF
          IF (ISPMD == 0) THEN
             WRITE(IOUT,6000) NBUCK, SHFTBUCK, BNITER, BINCV, BMAXNCV, MSG_BSOL(2)
          ENDIF
        ELSE
          CALL PROUT_BUCK(IOUT,NBUCK,IBUCKL)
        END IF
       ENDIF
C
      RETURN
 5000 FORMAT(/
     .  ' IMPLICIT OPTIONS USED :'//
     .  ' IMPLICIT TYPE : . . . . . . . . . . . . . ',2X,A/
     .  ' LINEAR SOLVER : . . . . . . . . . . . . . ',2X,A/
     .  ' PRECONDITION METHOD : . . . . . . . . . . ',2X,A/
     .  ' STOP CRITERION FOR LINEAR SOLVER . . . . .',2X,I5/
     .  ' ITERATION NUM. LIMIT FOR LINEAR SOLVER . .',2X,I5/
     .  ' TOLERANCE FOR LINEAR SOLVER . . . . . . . ',2X,G14.7/
     .  ' PRINTOUT FREQUENCY FOR LINEAR SOLVER . . .',2X,I5/)
 5010 FORMAT(/
     .  ' IMPLICIT OPTIONS USED :'//
     .  ' IMPLICIT TYPE : . . . . . . . . . . . . . ',2X,A/
     .  ' LINEAR SOLVER : . . . . . . . . . . . . . ',2X,A/
     .  ' PRINTOUT FREQUENCY FOR LINEAR SOLVER . . .',2X,I5/)
 5020 FORMAT(/
     .  ' GEOMETRICAL STIFFNESS FLAG  . . .. . . . .',2X,I5/
     .  ' LOAD (PRESSURE) STIFFNESS FLAG  . . . . . ',2X,I5/
     .  ' AUTOSPC FLAG (0:OFF,1:ON,2:ALL) . . .. . .',2X,I5/
     .  ' SPRING-BACK OPTION : . . . . . . . . . . .',2X,I5/
     .  ' SPECIAL PCG SOLVER FOR CONTACT . . . .. . ',2X,I5/)
 5050 FORMAT(/
     .  ' IMPLICIT OPTIONS USED :'//
     .  ' IMPLICIT TYPE : . . . . . . . . . . . . . ',2X,A//)
 5100 FORMAT(
     .  ' NON-LINEAR SOLVER : . . . . . . . . . . . ',2X,A/
     .  ' INITIAL TIME STEP . . . . . . . . . . . . ',2X,G14.7/
     .  ' STOP CRITERION FOR NON-LINEAR SOLVER . . .',2X,I5/
     .  ' TOLERANCE FOR NON-LINEAR SOLVER . . . . . ',2X,G14.7)
 5112 FORMAT(
     .  ' NON-LINEAR SOLVER : . . . . . . . . . . . ',2X,A/
     .  ' INITIAL TIME STEP . . . . . . . . . . . . ',2X,G14.7/
     .  ' STOP CRITERION FOR NON-LINEAR SOLVER . . .',2X,I5/
     .  ' ENERGY TOLERANCE FOR NON-LINEAR SOLVER . .',2X,G14.7/
     .  ' FORCE TOLERANCE FOR NON-LINEAR SOLVER  . .',2X,G14.7)
 5113 FORMAT(
     .  ' NON-LINEAR SOLVER : . . . . . . . . . . . ',2X,A/
     .  ' INITIAL TIME STEP . . . . . . . . . . . . ',2X,G14.7/
     .  ' STOP CRITERION FOR NON-LINEAR SOLVER . . .',2X,I5/
     .  ' ENERGY TOLERANCE FOR NON-LINEAR SOLVER . .',2X,G14.7/
     .  ' DISP. TOLERANCE FOR NON-LINEAR SOLVER  . .',2X,G14.7)
 5123 FORMAT(
     .  ' NON-LINEAR SOLVER : . . . . . . . . . . . ',2X,A/
     .  ' INITIAL TIME STEP . . . . . . . . . . . . ',2X,G14.7/
     .  ' STOP CRITERION FOR NON-LINEAR SOLVER . . .',2X,I5/
     .  ' FORCE TOLERANCE FOR NON-LINEAR SOLVER  . .',2X,G14.7/
     .  ' DISP. TOLERANCE FOR NON-LINEAR SOLVER  . .',2X,G14.7)
 5132 FORMAT(
     .  ' NON-LINEAR SOLVER : . . . . . . . . . . . ',2X,A/
     .  ' INITIAL TIME STEP . . . . . . . . . . . . ',2X,G14.7/
     .  ' STOP CRITERION FOR NON-LINEAR SOLVER . . .',2X,I5/
     .  ' ENERGY TOLERANCE FOR NON-LINEAR SOLVER . .',2X,G14.7/
     .  ' FORCE TOLERANCE FOR NON-LINEAR SOLVER  . .',2X,G14.7/
     .  ' DISP. TOLERANCE FOR NON-LINEAR SOLVER  . .',2X,G14.7)
 5150 FORMAT(
     .  ' REFORMING FLAG IN MIX SOLVER(0:AUTO,>0:ITER)',2X,I5/
     .  ' ITERATION NUM.LIMIT FOR REFORMING MATRIX .',2X,I5/
     .  ' PRINTOUT FREQUENCY FOR NON-LINEAR SOLVER .',2X,I5/
     .  ' PRE-STRESSES CONTROL FLAG . . . . . . . . ',2X,I5/
     .  ' REFERENCE RESIDUAL FLAG . . . . . . .. . .',2X,I5/
     .  ' TIME STEP CONTROL METHOD . . . . . . . . .',2X,I5/
     .  ' MINIMUM TIME STEP . . . . . . . . . . . . ',2X,G14.7/
     .  ' MAXIMUM TIME STEP . . . . . . . . . . . . ',2X,G14.7//)
 5180 FORMAT(
     .  ' ITERATION NUM.LIMIT FOR REFORMING MATRIX .',2X,I5/
     .  ' PRINTOUT FREQUENCY FOR NON-LINEAR SOLVER .',2X,I5/
     .  ' PRE-STRESSES CONTROL FLAG . . . . . . . . ',2X,I5/
     .  ' REFERENCE RESIDUAL FLAG . . . . . . .. . .',2X,I5/
     .  ' TIME STEP CONTROL METHOD . . . . . . . . .',2X,I5/
     .  ' MINIMUM TIME STEP . . . . . . . . . . . . ',2X,G14.7/
     .  ' MAXIMUM TIME STEP . . . . . . . . . . . . ',2X,G14.7//)
 5200 FORMAT(
     .  ' CONVERGE ITERATION NUM. FOR INCREASING DT. ',2X,I5/
     .  ' INCREASING TIME STEP SCALE FACTOR.  . . . .',2X,G14.7/
     .  ' CONVERGE ITERATION NUM. FOR DECREASING DT .',2X,I5/
     .  ' DECREASING TIME STEP SCALE FACTOR.  . . . .',2X,G14.7/)
 5300 FORMAT(
     .  ' DESIRED CONVERGE ITERATION NUM. . . . . . .',2X,I5/
     .  ' MAXIMUM CONVERGE ITERATION NUM. . . . . . .',2X,I5/
     .  ' DECREASING TIME STEP SCALE FACTOR.  . . . .',2X,G14.7/
     .  ' MAXIMUM INCREASING TIME STEP SCALE FACTOR ',2X,G14.7/
     .  ' CONSTANT ARC-LENGTH. . . . . . .. . . . . ',2X,G14.7//)
 5400 FORMAT(
     .  ' TIME INTEGRATION WITH HHT-ALPHA CONSTANT  ',2X,G14.7//)
 6000 FORMAT(
     .  ' EULER BUCKLING ANALYSIS :'/
     .  ' NUMBER OF MODES TO BE COMPUTED :. . . . . ',2X,I5/
     .  ' SHIFT IN BUCKLING MODES PENCIL :. . . . . ',2X,G14.7/
     .  ' MAXIMUM NUMBER OF ARNOLDI ITERATIONS :. . ',2X,I5/
     .  ' INITIAL FACTOR FOR SUBSPACE DIMENSION : . ',2X,I5/
     .  ' MAXIMUM FACTOR FOR SUBSPACE DIMENSION : . ',2X,I5/
     .  ' LINEAR SOLVER : . . . . . . . . . . . . . ',2X,A/)
 6100 FORMAT(
     .  ' PRECONDITION MATRIX SPARSE PATTERN(A^N) . ',2X,I5/)
 6200 FORMAT(
     .  ' IMPLICIT RAYLEIGH DAMPING COEFFICIENTS : ',2X,2G14.7/)
 6300 FORMAT(
     .  ' TIME INTEGRATION WITH NEWMARK CONSTANTS  ',2X,2G14.7//)
 6400 FORMAT(
     .  ' QUASI-STATIC INERTIA SCALE FACTOR :      ',2X,G14.7//)
 6500 FORMAT(
     .  ' CROSSING CONTACT NODE DETECTION DEACTIVATED  .',/)
 6600 FORMAT(
     .  ' LINEAR ANALYSE TAKING INTO ACCOUNT CONTACT . .',/)
 6700 FORMAT(
     .  ' CAUCHY STRESS SELECTED FOR LINEAR ANALYSE . .',/)
 6800 FORMAT(
     .  ' TAKING INTO ACCOUNT LARGE RIGID ROTATION  . .',/)
 6900 FORMAT(
     .  ' LINE-SEARCH METHOD :. . . . . . . . . . . .',2X,I5/
     .  ' MAXIMUM LINE-SEARCH ITERATION NUM. . . . . ',2X,I5/
     .  ' TOLERANCE FOR LINE-SEARCH ITERATION.  . . .',2X,G14.7/)
 7000 FORMAT(
     .  ' LINE-SEARCH DEACTIVATED . . . . . . . . . .  .',/)
 7100 FORMAT(
     .  ' REFERENCE RESIDUAL OPTIONS FOR CONTACT . . ',2X,I5/)
 7200 FORMAT(
     .  ' DIVERGENCE CRITERION NUMBERS :  . . . . . .',2X,I5/)
 7300 FORMAT(
     .  ' SIMPLIFIED TANGENT STIFFNESS ACTIVATED .  .',/)
 7400 FORMAT(
     .  ' AVERAGE CONTINUUM TANGENT STIFFNESS ACTIVATED',/)
 7500 FORMAT(
     .  ' FIXED TIME POINT NUMBER: . . . . . . . .',2X,I5)
 7510 FORMAT( /,3X,6G20.13,//)
 7600 FORMAT(
     .  ' FULL PROJECTION FOR QEPH(-1:OFF,1:ON) . . .. . .',2X,I5/)
 7650 FORMAT(
     .  ' DESIRED CONVERGE ITERATION NUM. . . . . . .',2X,I5/
     .  ' MAXIMUM CONVERGE ITERATION NUM. . . . . . .',2X,I5/
     .  ' DECREASING TIME STEP SCALE FACTOR.  . . . .',2X,G14.7/
     .  ' MAXIMUM INCREASING TIME STEP SCALE FACTOR ',2X,G14.7/
     .  ' CONSTANT ARC-LENGTH. . . . . . .. . . . . ',2X,G14.7/
     .  ' CONSTRAINT TYPE . . . .. . . . . . . . . . ',2X,I5/
     .  ' LOADING CONTRIBUTION SCALE FACTOR . . . . ',2X,G14.7//)
 7700 FORMAT('AUTOMATIC TIME STEP CONTROL:'/)
 7800 FORMAT('AUTOMATIC TIME STEP WITH RIKS METHOD:'/)
 7900 FORMAT(
     .  ' MAXIMUM INCREMENT(NCYCLE) NUM . . .. . .',2X,I5/)
 8000 FORMAT(
     .  ' CONTINUUM TANGENT STIFFNESS ACTIVATED. .',/)
 8100 FORMAT(
     .  ' CONSISTENT TANGENT STIFFNESS ACTIVATED. .',/)
 8200 FORMAT(
     .  ' MINIMUM REFERENCE FORCE RESIDUAL . . . .',2X,G14.7/
     .  ' MAXIMUM REFERENCE FORCE RESIDUAL . . . . ',2X,G14.7//)
 8010 FORMAT(
     .  ' SMALL DISPLACEMENT OPTION(0:OFF,1:ON) . . .. . .',2X,I5/)
 8400 FORMAT(
     .  ' NUMBER OF PROJECTION VECTORS OF PCG . . . ',2X,I5/
     .  ' PROJECTION VECTOR INITIALIZATION METHOD . ',2X,I5/)
 8500 FORMAT(
     .  ' DIVERGING TOL. OF RELATIVE FORCE RESIDUAL :',2X,G14.7/)
 8600 FORMAT(
     .  ' FREE RIGID MOTION /MRIGM USED . . . . . . .'/)
 8700 FORMAT(
     .  ' FREE RIGID MOTION /MRIGM W/ REF_NODE_ID:',2X,3I10/)
      END

