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
Chd|  INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        I10MAIN_OPT_TRI               source/interfaces/intsort/i10opt_opt_tri.F
Chd|        I10MAIN_TRI                   source/interfaces/intsort/i10main_tri.F
Chd|        I11MAIN_CRIT_TRI              source/interfaces/intsort/i11main_crit_tri.F
Chd|        I11MAIN_OPT_TRI               source/interfaces/intsort/i11main_opt_tri.F
Chd|        I11MAIN_TRI                   source/interfaces/intsort/i11main_tri.F
Chd|        I17MAIN_CRIT_TRI              source/interfaces/int17/i17main_pena.F
Chd|        I17MAIN_TRI                   source/interfaces/int17/i17main_pena.F
Chd|        I20MAIN_CRIT_TRI              source/interfaces/intsort/i20main_crit_tri.F
Chd|        I20MAIN_OPT_TRI               source/interfaces/intsort/i20main_opt_tri.F
Chd|        I20MAIN_TRI                   source/interfaces/intsort/i20main_tri.F
Chd|        I21MAIN_CRIT_TRI              source/interfaces/intsort/i21main_crit_tri.F
Chd|        I21MAIN_GAP                   source/interfaces/int21/i21main_gap.F
Chd|        I21MAIN_OPT_TRI               source/interfaces/intsort/i21main_opt_tri.F
Chd|        I21MAIN_TRI                   source/interfaces/intsort/i21main_tri.F
Chd|        I21RESET                      source/interfaces/int21/i21reset.F
Chd|        I21_ICRIT                     source/interfaces/intsort/i21_icrit.F
Chd|        I22MAIN_TRI                   source/interfaces/intsort/i22main_tri.F
Chd|        I22SUBVOL                     source/interfaces/int22/i22subvol.F
Chd|        I23MAIN_OPT_TRI               source/interfaces/intsort/i23main_opt_tri.F
Chd|        I23MAIN_TRI                   source/interfaces/intsort/i23main_tri.F
Chd|        I24MAIN_CRIT_TRI              source/interfaces/intsort/i24main_crit_tri.F
Chd|        I24MAIN_OPT_TRI               source/interfaces/intsort/i24main_opt_tri.F
Chd|        I24MAIN_TRI                   source/interfaces/intsort/i24main_tri.F
Chd|        I25MAIND_2                    source/interfaces/int25/i25maind_2.F
Chd|        I25MAIN_CRIT_TRI              source/interfaces/intsort/i25main_crit_tri.F
Chd|        I25MAIN_FREE                  source/interfaces/intsort/i25main_free.F
Chd|        I25MAIN_GAP                   source/interfaces/int25/i25main_gap.F
Chd|        I25MAIN_NORM                  source/interfaces/int25/i25main_norm.F
Chd|        I25MAIN_OPT_TRI               source/interfaces/intsort/i25main_opt_tri.F
Chd|        I25MAIN_SLID                  source/interfaces/int25/i25main_slid.F
Chd|        I25MAIN_TRI                   source/interfaces/intsort/i25main_tri.F
Chd|        I7MAIN_CRIT_TRI               source/interfaces/intsort/i7main_crit_tri.F
Chd|        I7MAIN_OPT_TRI                source/interfaces/intsort/i7main_opt_tri.F
Chd|        I7MAIN_TRI                    source/interfaces/intsort/i7main_tri.F
Chd|        INT18_LAW151_NSV_SHIFT        source/interfaces/int18/int18_law151_nsv_shift.F
Chd|        INTCRIT                       source/interfaces/intsort/intcrit.F
Chd|        INTER_CHECK_SORT              source/interfaces/generic/inter_check_sort.F
Chd|        INTER_DEALLOCATE_WAIT         source/interfaces/generic/inter_deallocate_wait.F
Chd|        INTER_PREPARE_SORT            source/interfaces/generic/inter_prepare_sort.F
Chd|        INTER_SORT                    source/interfaces/generic/inter_sort.F
Chd|        INTER_TRC_7                   source/interfaces/int07/inter_trc_7.F
Chd|        INTMASS_UPDATE                source/interfaces/interf/intmass_update.F
Chd|        INT_STARTIME                  source/system/timer_interf.F  
Chd|        INT_STOPTIME                  source/system/timer_interf.F  
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_ALLGLOB_ISUM9            source/mpi/generic/spmd_allglob_isum9.F
Chd|        SPMD_BARRIER                  source/mpi/generic/spmd_barrier.F
Chd|        SPMD_EXCH_SORTING_EFRIC       source/mpi/interfaces/spmd_exch_sorting_efric.F
Chd|        SPMD_GET_INACTI_GLOBAL        source/mpi/interfaces/spmd_get_inacti_global.F
Chd|        SPMD_GET_STIF25_EDG           source/mpi/interfaces/spmd_getstif25_edg.F
Chd|        SPMD_I25FRONT_NOR             source/mpi/interfaces/spmd_i25front.F
Chd|        SPMD_I7ITIED_CAND             source/mpi/interfaces/spmd_i7itied_cand.F
Chd|        SPMD_IFRONT                   source/mpi/interfaces/spmd_ifront.F
Chd|        SPMD_IFRONT_STAMP             source/mpi/interfaces/send_cand.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        I22BUFBRIC_MOD                ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUFMOD                     share/modules/restart_mod.F   
Chd|        INTERFACES_MOD                ../common_source/modules/interfaces/interfaces_mod.F
Chd|        INTER_SORTING_MOD             share/modules/inter_sorting_mod.F
Chd|        INTER_STRUCT_MOD              share/modules/inter_struct_mod.F
Chd|        INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|        METRIC_MOD                    ../common_source/modules/metric_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        OUTPUTS_MOD                   ../common_source/modules/outputs_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE INTTRI(
     1       IPARI    ,X       ,W       , ERRORS, 
     2       V        ,MS      ,IN      ,IAD_ELEM ,
     3       FR_ELEM  ,VR      ,ISENDTO ,IRECVFROM,
     4       NEWFRONT ,ITASK   ,WAG     ,DT2T     ,
     5       ITAB     ,NELTST  ,ITYPTST ,WEIGHT   ,
     6       INTLIST  ,NBINTC  ,KINET   ,DRETRI   ,
     7       ISLEN7   ,IRLEN7  ,ISLEN11 ,IRLEN11  ,
     8       TEMP     ,IGRBRIC ,IGRSH3N ,EMINX    ,
     9       IXS      ,IXS16   ,IXS20   ,ISLEN17  ,
     A       IRLEN17  ,IRLEN7T ,ISLEN7T ,NUM_IMP  ,
     B       IND_IMP  ,INTSTAMP,THKNOD  ,IRLEN20  ,
     C       ISLEN20  ,IRLEN20T,ISLEN20T,IRLEN20E ,
     D       ISLEN20E ,RENUM   ,NSNFIOLD,XSLV     ,
     E       XMSR     ,VSLV    ,VMSR    ,SIZE_T   ,
     F       NODNX_SMS,DXANCG  ,IKINE   ,DIAG_SMS ,
     G       COUNT_REMSLV, COUNT_REMSLVE,ALE_CONNECTIVITY,
     H       IXTG     ,SENSORS  ,DELTA_PMAX_GAP    ,
     I       INTBUF_TAB ,DELTA_PMAX_GAP_NODE,
     .                                IAD_FRNOR,FR_NOR,
     J       NB25_CANDT,NB25_IMPCT,NB25_DST1,NB25_DST2,INTLIST25,
     K       IAD_FREDG,FR_EDG,MAIN_PROC,NATIV_SMS,I_OPT_STOK ,
     L       MULTI_FVM,IPARG  ,ELBUF_TAB, H3D_DATA ,T2MAIN_SMS,
     M       LSKYI_SMS_NEW,FORNEQS,INT7ITIED,IDEL7NOK_SAV,MAXDGAP,
     N       T2FAC_SMS,ICODT,ISKEW,FSKYN25,ADDCSRECT,PROCNOR,
     O       INTER_STRUCT,SORT_COMM,RENUM_SIZ,NODNX_SMS_SIZ,TEMP_SIZ,
     P       INTERFACES)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD  
      USE INTBUFMOD
      USE INTSTAMP_MOD
      USE TRI7BOX
      USE INTBUFDEF_MOD  
      USE I22TRI_MOD
      USE I22BUFBRIC_MOD      
      USE MOD_I25MAIN_NORM
      USE MULTI_FVM_MOD
      USE H3D_MOD
      USE METRIC_MOD
      USE GROUPDEF_MOD
      USE ALE_CONNECTIVITY_MOD
      USE INTER_STRUCT_MOD
      USE INTER_SORTING_MOD
      USE SENSOR_MOD
      USE OUTPUTS_MOD
      USE INTERFACES_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "macro.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "intstamp_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
#include      "warn_c.inc" 
#include      "units_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: ERRORS !< number of interfaces that could not be sorted
      INTEGER, INTENT(in) :: NODNX_SMS_SIZ
      INTEGER IPARI(NPARI,*), IXS(*), IXS16(*), IXS20(*),
     .        ITAB(*),
     .        NEWFRONT(*),NBINTC,INTLIST(*),
     .        ISENDTO(NSPMD+1,*),IRECVFROM(NSPMD+1,*),
     .        ITASK,NELTST ,ITYPTST,WEIGHT(*),
     .        IAD_ELEM(2,*) ,FR_ELEM(*),
     .        ISLEN7, IRLEN7, ISLEN11, IRLEN11, ISLEN17 ,IRLEN17,
     .        IRLEN7T ,ISLEN7T,IRLEN20,ISLEN20,IRLEN20T,ISLEN20T,
     .        IRLEN20E, ISLEN20E,
     .        IND_IMP(*),NUM_IMP(*),RENUM(*), NSNFIOLD(NSPMD),
     .        NODNX_SMS(NODNX_SMS_SIZ),IKINE(NUMNOD),I_MEM,COUNT_REMSLV(*),
     .        COUNT_REMSLVE(*), IXTG(NIXTG,*),DELTA_PMAX_GAP_NODE(*),
     .        IAD_FRNOR(*), FR_NOR(*), IAD_FREDG(*), FR_EDG(*), 
     .        NB25_CANDT(PARASIZ), NB25_IMPCT(PARASIZ), 
     .        NB25_DST1(PARASIZ), NB25_DST2(PARASIZ), IPARG(NPARG,*),
     .        INTLIST25(*), MAIN_PROC(*), NATIV_SMS(*), I_OPT_STOK(NINTER),
     .        T2MAIN_SMS(6,*), LSKYI_SMS_NEW, IDEL7NOK_SAV,
     .        ADDCSRECT(*), PROCNOR(*)
      INTEGER, INTENT(IN) ::  ICODT(*), ISKEW(*)
!     INT7ITIED : check if an interface type 7 with ITIED /= 0 is used
!                 in order to force the communication of a list of candidate nodes 
!                 INT7ITIED = 0 type 7 + ITIED/=0 not used
!                 INT7ITIED = 1 type 7 + ITIED/=0 used
      INTEGER, INTENT(IN) :: INT7ITIED
      INTEGER, DIMENSION(*), TARGET :: KINET
      INTEGER, INTENT(in) :: TEMP_SIZ
      TYPE(INTSTAMP_DATA) INTSTAMP(*)
      my_real 
     .   WAG(*),
     .   VR(3,*),IN(*),DT2T,DIST, DRETRI(*), TEMP(TEMP_SIZ), EMINX(*),
     .        THKNOD(*),DELTA_PMAX_GAP(NINTER),
     .        XSLV(18,NINTER),XMSR(12,NINTER),X21MSR(3,NINTSTAMP),
     .        VSLV(6,NINTER),VMSR(6,NINTER),V21MSR(3,NINTSTAMP),
     .        SIZE_T(NINTER),DXANCG(3,*), DIAG_SMS(*),
     .        FORNEQS(*), MAXDGAP(NINTER), T2FAC_SMS(*)
      my_real, TARGET ::  X(3*NUMNOD),V(3*NUMNOD),W(3,NUMNOD)
      my_real, DIMENSION(*), TARGET :: MS
      REAL*4 FSKYN25(3,*)

      TYPE(INTBUF_STRUCT_),DIMENSION(NINTER) :: INTBUF_TAB
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT), TARGET     :: MULTI_FVM
      TYPE(ELBUF_STRUCT_) ,DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
      INTEGER, INTENT(in) :: RENUM_SIZ
      TYPE(inter_struct_type), DIMENSION(NINTER), INTENT(inout) :: INTER_STRUCT   !   structure for interface
      TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
      TYPE (INTERFACES_) ,INTENT(IN) :: INTERFACES
      TYPE (SENSORS_)    ,INTENT(IN) :: SENSORS
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, KK,LL, RETRI, NBLIST,NSENSOR,
     .        IAD17, IGN, IGE, NME, NMES,I,J,K,
     .        IDUM, IADI, ISTAMP, NRTM_T, NME_T, NEDGE_T, ESHIFT, SSHIFT, MULTIMP,
     .        ISENS,NBF,NBL,IB, NIN,NSNE_MAX,NFIC,L_FIC,NNOD3,NSNE3,
     .        NBINTC21, SIZE, NRTM_FE_T, NRTM_IGE_T, ITHK
      my_real PCT1, TS,DELTA_PMAX_DGAP(NINTER),LEN
      INTEGER NB_STOK_N(PARASIZ),NB_JLT(PARASIZ),RETRI21(NINTER),NBCUT,
     .        INTLIST21(NINTSTAMP)
      SAVE NB_STOK_N,NB_JLT,NSNE_MAX,NNOD3
      my_real, DIMENSION(:),ALLOCATABLE, TARGET :: XE,VE
      my_real, DIMENSION(:),ALLOCATABLE, TARGET :: X_IGE,V_IGE
      my_real, DIMENSION(:),POINTER :: PTR_X,PTR_V,PTR_MS
      INTEGER, DIMENSION(:),POINTER :: PTR_KINET
      my_real :: BID
      INTEGER :: IBRIC, NBRIC, II, INOD, NODEID, ISU1, IAD, INACTI
      LOGICAL :: M151_ALLOC, TYPE18
      SAVE XE,VE,M151_ALLOC
      SAVE X_IGE,V_IGE,MAX_IGE,SIZE_X_IGE
      INTEGER :: MAX_IGE,SIZE_X_IGE
      INTEGER :: NB_INTER_SORTED        !   number of interfaces that need to be sorted
      INTEGER, DIMENSION(NBINTC) :: LIST_INTER_SORTED   !   list of interfaces that need to be sorted
      INTEGER :: NTY
C-----------------------------------------------
      NSENSOR = SENSORS%NSENSOR
C Single region
      I_MEM = 0
!$OMP SINGLE
      DELTA_PMAX_GAP_NODE(1:NINTER)=0

      IF (IMONM > 0) THEN
        IF(IMONM == 2 .AND. NSPMD > 1)THEN
          CALL STARTIME(56,1)
          CALL SPMD_BARRIER()
          CALL STOPTIME(56,1)
        END IF 
        CALL STARTIME(15,1)
      ENDIF
     
      !Init variable globale interface
      DO KK=1,NBINTC
        N = INTLIST(KK)
        DELTA_PMAX_GAP(N)=ZERO
        MAXDGAP(N)=-EP30
        XSLV( 1,N)= -EP30
        XSLV( 2,N)= -EP30
        XSLV( 3,N)= -EP30
        XSLV( 4,N)= EP30
        XSLV( 5,N)= EP30
        XSLV( 6,N)= EP30
        XSLV( 7,N)= -EP30
        XSLV( 8,N)= -EP30
        XSLV( 9,N)= -EP30
        XSLV(10,N)= EP30
        XSLV(11,N)= EP30
        XSLV(12,N)= EP30
        XSLV(13,N)= -EP30
        XSLV(14,N)= -EP30
        XSLV(15,N)= -EP30
        XSLV(16,N)= EP30
        XSLV(17,N)= EP30
        XSLV(18,N)= EP30

        XMSR( 1,N)= -EP30
        XMSR( 2,N)= -EP30
        XMSR( 3,N)= -EP30
        XMSR( 4,N)= EP30
        XMSR( 5,N)= EP30
        XMSR( 6,N)= EP30
        XMSR( 7,N)= -EP30
        XMSR( 8,N)= -EP30
        XMSR( 9,N)= -EP30
        XMSR(10,N)= EP30
        XMSR(11,N)= EP30
        XMSR(12,N)= EP30

        VSLV(1,N)= -EP30
        VSLV(2,N)= -EP30
        VSLV(3,N)= -EP30
        VSLV(4,N)= EP30
        VSLV(5,N)= EP30
        VSLV(6,N)= EP30
        VMSR(1,N)= -EP30
        VMSR(2,N)= -EP30
        VMSR(3,N)= -EP30
        VMSR(4,N)= EP30
        VMSR(5,N)= EP30
        VMSR(6,N)= EP30
        SIZE_T(N)=ZERO  
        DELTA_PMAX_DGAP(N)=ZERO    
      END DO
      ! idem pour interface 21
      DO KK=1,NINTSTAMP
        N = INTSTAMP(KK)%NOINTER
        XSLV(1,N)= -EP30
        XSLV(2,N)= -EP30
        XSLV(3,N)= -EP30
        XSLV(4,N)= EP30
        XSLV(5,N)= EP30
        XSLV(6,N)= EP30
        XMSR(1,N)= -EP30
        XMSR(2,N)= -EP30
        XMSR(3,N)= -EP30
        XMSR(4,N)= EP30
        XMSR(5,N)= EP30
        XMSR(6,N)= EP30
        VSLV(1,N)= -EP30
        VSLV(2,N)= -EP30
        VSLV(3,N)= -EP30
        VSLV(4,N)= EP30
        VSLV(5,N)= EP30
        VSLV(6,N)= EP30
        VMSR(1,N)= -EP30
        VMSR(2,N)= -EP30
        VMSR(3,N)= -EP30
        VMSR(4,N)= EP30
        VMSR(5,N)= EP30
        VMSR(6,N)= EP30
      END DO
C-----int24 edge pourait etre optimis  e apres
      NSNE_MAX=0    
      MAX_IGE = 0  
      DO KK=1,NBINTC
C
        N = INTLIST(KK)
        NTY   =IPARI(7,N)
C        Look if interface is activated 
        ISENS = 0
        IF(NTY == 7.OR.NTY == 11.OR.NTY == 24.OR.NTY == 25) 
     .    ISENS = IPARI(64,N)  
        IF (ISENS > 0)  THEN         ! Interface activated by sensor
           TS = SENSORS%SENSOR_TAB(ISENS)%TSTART
        ELSE
           TS = TT
        ENDIF
        IF(NTY == 24.AND.TT>=TS)THEN
         NSNE_MAX = MAX(NSNE_MAX,IPARI(55,N))
C-- 
ccc         NFIC = 3   
ccc         CALL I24XVFIC_UPD(IPARI(1,N),INTBUF_TAB(N),X   ,V ,NFIC  ,ITAB)
        END IF

        IF(INTBUF_TAB(N)%S_NIGE/=0) THEN
            MAX_IGE = MAX(MAX_IGE,INTBUF_TAB(N)%S_NIGE)
        ENDIF
      END DO
      IF (NSNE_MAX>0 ) THEN
       L_FIC=3*(NSNE_MAX+NUMNOD)
       NNOD3 =3*NUMNOD
       ALLOCATE(XE(L_FIC),VE(L_FIC))
       XE(1:NNOD3) = X(1:NNOD3)
       VE(1:NNOD3) = V(1:NNOD3)
      END IF

      IF(MAX_IGE>0) THEN
        ALLOCATE( X_IGE(3*(NUMNOD+MAX_IGE)) )
        ALLOCATE( V_IGE(3*(NUMNOD+MAX_IGE)) )
        X_IGE(1:3*NUMNOD) = X(1:3*NUMNOD)
        V_IGE(1:3*NUMNOD) = V(1:3*NUMNOD)
        SIZE_X_IGE = 3*(NUMNOD+MAX_IGE)
      ELSE
        ALLOCATE( X_IGE(0) )
        ALLOCATE( V_IGE(0) )
        SIZE_X_IGE = 0
      ENDIF

C end of single region
!$OMP END SINGLE

      ! If law151+int18 : shift NSV array
      IF( MULTI_FVM%IS_INT18_LAW151 ) THEN
        CALL INT18_LAW151_NSV_SHIFT('+',ITASK,MULTI_FVM,IPARI,INTBUF_TAB)
        CALL MY_BARRIER()
      ENDIF


C   Inter Type 21 ithe=2 : prepare to communicate tri criteria
      NBINTC21 = 0
      DO KK=1,NINTSTAMP
        N = INTSTAMP(KK)%NOINTER
       IF (IPARI(47,N)==2) THEN
          NBINTC21 = NBINTC21 + 1
          INTLIST21(NBINTC21) = KK
       ENDIF
      END DO

C   Stiffness based on mass and time step for Int 24/25
C       Cyle 1 : update secondary and main nodal masses
      IF(NCYCLE == 1 ) THEN
         DO KK=1,NBINTC
            N = INTLIST(KK)
            NTY = IPARI(7,N)
            IF (NTY == 24 .OR. NTY == 25 ) THEN
               IF(IPARI(97,N) > 0.AND.IPARI(98,N)==2) THEN
                  CALL INTMASS_UPDATE( N   ,IPARI(1,N), INTBUF_TAB(N), MS  )
              ENDIF
            ENDIF
          ENDDO   
       ENDIF  
C
C Critere de retri interface
C
      IF(ITASK==0)CALL STARTIME(120,1)
      DO KK=1,NBINTC
        N      = INTLIST(KK)
        NTY    = IPARI(7,N)
        INACTI = IPARI(22,N)        
        TYPE18 = .FALSE.
        IF(NTY == 7 .AND. INACTI ==7)TYPE18=.TRUE.
        IF(IMONM > 0 ) THEN
         IF(ITASK == 0) CALL INT_STARTIME(INTBUF_TAB(N)%METRIC,I_MAIN_CRIT_TRI)  
        ENDIF
!$OMP ATOMIC WRITE
        IPARI(29,N) = 0
!$OMP END ATOMIC
        NTY   =IPARI(7,N)
        ! Look if interface is activated 
        ISENS = 0
        IF(NTY == 7.OR.NTY == 11.OR.NTY == 24.OR.NTY == 25) ISENS = IPARI(64,N)  
        IF (ISENS > 0)  THEN         ! Interface activated by sensor
           TS = SENSORS%SENSOR_TAB(ISENS)%TSTART
        ELSE
           TS = TT
        ENDIF
C-----------------------------------------------
        IF((NTY == 7.AND.TT>=TS).OR.NTY == 10.OR.NTY == 18)THEN
C-----------------------------------------------
          I7KGLO = 1
          !IF(INTER18_AUTOPARAM == 1)I7KGLO = 0
          IF(INTBUF_TAB(N)%S_NIGE/=0) THEN
            X_IGE(3*NUMNOD+1:3*(NUMNOD+INTBUF_TAB(N)%S_NIGE)) = INTBUF_TAB(N)%XIGE(1:3*INTBUF_TAB(N)%S_NIGE)
            PTR_X => X_IGE
            V_IGE(3*NUMNOD+1:3*(NUMNOD+INTBUF_TAB(N)%S_NIGE)) = INTBUF_TAB(N)%VIGE(1:3*INTBUF_TAB(N)%S_NIGE)
            PTR_V => V_IGE
         ELSEIF (MULTI_FVM%IS_USED .AND. TYPE18) THEN
            PTR_X => MULTI_FVM%X_APPEND
            PTR_V => MULTI_FVM%V_APPEND
         ELSE
            PTR_X => X
            PTR_V => V
          ENDIF

       CALL I7MAIN_CRIT_TRI(
     1      IPARI ,PTR_X        ,N        ,
     2      ITASK ,PTR_V        ,XSLV(1,N)    ,XMSR(1,N),VSLV(1,N),
     3      VMSR(1,N),INTBUF_TAB(N))

C-----------------------------------------------
        ELSEIF(NTY == 24.AND.TT>=TS)THEN
C-----------------------------------------------
          I7KGLO = 1
C         IPARI(4,N) = NRTM ; IPARI(5,N)=NSN  
C-------not necessary but should modify I24BUCE_CRIT
C         NSNE = IPARI(55,N)
C        IF (NSNE >0 ) THEN
C         CALL MY_BARRIER
C!$OMP SINGLE
C         XE(NNOD3+1:(NNOD3+NSNE3)) = INTBUF_TAB(N)%XFIC(1:NSNE3)
C         VE(NNOD3+1:(NNOD3+NSNE3)) = INTBUF_TAB(N)%VFIC(1:NSNE3)
C!$OMP END SINGLE
C        CALL I24MAIN_CRIT_TRI(
C     1      IPARI ,INTBUF_TAB(N),XE           ,N        ,
C     2      ITASK ,VE           ,XSLV(1,N)    ,XMSR(1,N),VSLV(1,N),
C     3      VMSR(1,N),DELTA_PMAX_GAP(N),DELTA_PMAX_DGAP(N))
C        ELSE
        CALL I24MAIN_CRIT_TRI(
     1      IPARI ,INTBUF_TAB(N),X            ,N        ,
     2      ITASK ,V            ,XSLV(1,N)    ,XMSR(1,N),VSLV(1,N),
     3      VMSR(1,N),DELTA_PMAX_GAP(N),DELTA_PMAX_DGAP(N),
     4      DELTA_PMAX_GAP_NODE(N),ITAB)
C        END IF !(NSNE_MAX>0 ) THEN

C-----------------------------------------------
        ELSEIF(NTY == 25.AND.TT>=TS)THEN
C-----------------------------------------------
!$OMP ATOMIC WRITE
          I7KGLO = 1
!$OMP END ATOMIC
C         IPARI(4,N) = NRTM ; IPARI(5,N)=NSN  
          CALL I25MAIN_CRIT_TRI(
     1      IPARI ,INTBUF_TAB(N),X            ,N        ,
     2      ITASK ,V            ,XSLV(1,N)    ,XMSR(1,N),VSLV(1,N),
     3      VMSR(1,N),DELTA_PMAX_GAP(N),DELTA_PMAX_DGAP(N),
     4      DELTA_PMAX_GAP_NODE(N),ITAB)
C
C ITHK = 1 : main gap should be modified as per change in thickness
          ITHK =  IPARI(91,N)
          IF(ITHK == 1) THEN          
            CALL I25MAIN_GAP(
     1        IPARI  ,INTBUF_TAB(N)  ,N    ,ITASK  ,
     2        THKNOD, MAXDGAP(N))
          ELSE
!$OMP ATOMIC WRITE
               MAXDGAP(N) = ZERO
!$OMP END ATOMIC
          ENDIF
C
C-----------------------------------------------
        ELSEIF(NTY == 11.AND.TT>=TS)THEN
C-----------------------------------------------
          I7KGLO = 1
          CALL I11MAIN_CRIT_TRI(
     1      IPARI     ,X        ,N        ,
     2      ITASK     ,V            ,XSLV(1,N)    ,XMSR(1,N),VSLV(1,N),
     3      VMSR(1,N) ,INTBUF_TAB(N))
C-----------------------------------------------
        ELSEIF(NTY == 17)THEN
C-----------------------------------------------
          IF(IPARI(33,N) == 0)THEN
C
            IAD17=1
            DO K=1,N-1
              NTY   =IPARI(7,K)
              IF(IPARI(7,K) == 17.AND.IPARI(33,K) == 0)THEN
                IGN   =IPARI(36,K)
                IGE   =IPARI(34,K)
                NMES  =IGRBRIC(IGN)%NENTITY
                NME   =IGRBRIC(IGE)%NENTITY
                IAD17 = IAD17+6*(NME+NMES)
              END IF
            END DO
C
            I7KGLO = 1
            IGN   =IPARI(36,N)
            IGE   =IPARI(34,N)
            NMES  =IGRBRIC(IGN)%NENTITY
            NME   =IGRBRIC(IGE)%NENTITY
            CALL I17MAIN_CRIT_TRI(
     1        IPARI,INTBUF_TAB(N),X           ,N  ,
     2        ITASK,IGRBRIC      ,EMINX(IAD17),NME,
     3        NMES ,XSLV(1,N)    ,XMSR(1,N)    , SIZE_T     ,IXS,
     4        IXS16,IXS20        )
          END IF
C-----------------------------------------------
        ELSEIF(NTY == 20)THEN
C-----------------------------------------------
          I7KGLO = 1
C         IPARI(4,N) = NRTM ; IPARI(5,N)=NSN
          CALL I20MAIN_CRIT_TRI(
     1      IPARI ,X        ,N        ,
     2      ITASK ,V            ,XSLV(1,N)    ,XMSR(1,N),VSLV(1,N),
     3      VMSR(1,N),MS        ,DXANCG       ,IKINE    ,DIAG_SMS ,
     4      INTBUF_TAB(N)   ,H3D_DATA)
C-----------------------------------------------
        ELSEIF(NTY == 22)THEN
C-----------------------------------------------
          !
C-----------------------------------------------
        ELSEIF(NTY == 23)THEN
C-----------------------------------------------
          I7KGLO = 1
C         IPARI(4,N) = NRTM ; IPARI(5,N)=NSN 
       CALL I7MAIN_CRIT_TRI(
     1      IPARI ,X            ,N        ,
     2      ITASK ,V            ,XSLV(1,N)    ,XMSR(1,N),VSLV(1,N),
     3      VMSR(1,N),INTBUF_TAB(N))
C-----------------------------------------------
        ENDIF
C-----------------------------------------------
        IF(IMONM > 0 ) THEN
         IF(ITASK == 0) CALL INT_STOPTIME(INTBUF_TAB(N)%METRIC,I_MAIN_CRIT_TRI)  
        ENDIF
      ENDDO
C
      DO KK=1,NINTSTAMP
        N = INTSTAMP(KK)%NOINTER
        ISENS = IPARI(64,N)       ! INTERFACE SENSOR NUMBER
        IF (ISENS > 0) THEN    ! IF INTERFACE IS ACTIVATED BY SENSOR 
           TS = SENSORS%SENSOR_TAB(ISENS)%TSTART
        ELSE
           TS = TT
        ENDIF
        X21MSR(1:3,KK) = ZERO
        V21MSR(1:3,KK) = ZERO
        IF(TT>=TS)THEN          ! INTERFACE SENSOR IS ACTIVATED
          IPARI(29,N) = 0
          NTY   =IPARI(7,N)
          I7KGLO = 1
          CALL I21MAIN_GAP(
     1          IPARI ,INTBUF_TAB(N),N     ,ITASK ,
     2          THKNOD)
C
C       remet a 0 le segment main le plus proche (IRTLM): avt barrier
          CALL I21RESET(
     1          IPARI ,INTBUF_TAB(N),N     ,ITASK )
C
          CALL I21MAIN_CRIT_TRI(
     1      IPARI    ,INTBUF_TAB(N),X        ,N       ,
     2      ITASK    ,V            ,XSLV(1,N)    ,XMSR(1,N),VSLV(1,N),
     3      VMSR(1,N),INTSTAMP(KK) ,X21MSR(1,KK) ,V21MSR(1,KK))
        ENDIF
      ENDDO
C
       CALL MY_BARRIER
C
C Partie non parallele smt
C
!$OMP SINGLE

      IF (IMONM > 0) THEN
        CALL STOPTIME(15,1)
        IF(IMONM == 2 .AND. NSPMD > 1)THEN
          CALL STARTIME(57,1)
          CALL SPMD_BARRIER()
          CALL STOPTIME(57,1)
        END IF 
        CALL STARTIME(16,1)
       ENDIF 
C
C Communication critere de tri
C
      ERRORS = 0 
      CALL INTCRIT(
     1    ERRORS, IPARI   ,NEWFRONT ,ISENDTO  ,NSENSOR  , 
     2    IRECVFROM ,DT2T    ,NELTST  ,ITYPTST  ,ITAB      ,
     3    XSLV      ,XMSR    ,VSLV    ,VMSR     ,INTLIST   ,
     4    NBINTC    ,SIZE_T  ,SENSORS%SENSOR_TAB,DELTA_PMAX_GAP,
     5    INTBUF_TAB,DELTA_PMAX_GAP_NODE,IDEL7NOK_SAV,MAXDGAP)


C
C (barriere par interface)
      IF(NINTSTAMP/=0)THEN
        CALL I21_ICRIT(
     1    INTBUF_TAB       ,IPARI   ,DT2T ,NELTST ,NSENSOR ,
     2    ITYPTST  ,XSLV   ,XMSR  ,VSLV   ,VMSR    ,
     3    INTSTAMP ,X21MSR ,V21MSR,SENSORS%SENSOR_TAB,NBINTC21 ,
     4    INTLIST21)
      END IF
C
      IF (IMONM > 0) THEN
        CALL STOPTIME(16,1)
        CALL STARTIME(17,1)
      ENDIF

      IF(TT>ZERO.AND.INT7ITIED/=0) THEN
        CALL SPMD_I7ITIED_CAND(1,NBINTC,IPARI,INTLIST,INTBUF_TAB)
        CALL SPMD_I7ITIED_CAND(2,NBINTC,IPARI,INTLIST,INTBUF_TAB)
      ENDIF

C Fin Partie non parallele smt
!$OMP END SINGLE
      IF(ERRORS > 0) RETURN

      IF(IMPL_S/=1)THEN

       IF((NSPMD>1.AND.ITASK==0).AND.(H3D_DATA%N_SCAL_CSE_FRIC > 0.OR.NINEFRIC > 0).AND.TT > ZERO) THEN
         CALL SPMD_EXCH_SORTING_EFRIC(
     1        IPARI   ,INTLIST ,NBINTC  ,ISLEN7 ,IRLEN7  ,
     2        IRLEN7T ,ISLEN7T ,IRLEN20 ,ISLEN20,IRLEN20T,
     3        ISLEN20T,INTBUF_TAB,H3D_DATA  )
       ENDIF
      ENDIF

      IF(ITASK==0)CALL STOPTIME(120,1) 


      RETRI = 0
      ! ------------------------
      ! new sorting algorithm     
      IF(ITASK==0) NEED_TO_SORT = 0
      ! find the list of interface (for the moment : type7) that needs to be sorted
      CALL INTER_CHECK_SORT( ITASK,NEED_TO_SORT,NBINTC,INTLIST,IPARI,NSENSOR,
     .     INTBUF_TAB,SENSORS%SENSOR_TAB,NB_INTER_SORTED,LIST_INTER_SORTED,INTER_STRUCT)
      ! globalize NEED_TO_SORT : 1= one or several interface(s) must be sorted 
      IF(NSPMD>1.AND.ITASK==0) CALL SPMD_ALLGLOB_ISUM9(NEED_TO_SORT,1)
      CALL MY_BARRIER()
      ! ------------------------

      ! ------------------------
      ! explicit part
      IF(IMPL_S/=1)THEN
            ! -----------
!$OMP SINGLE
            CALL INTER_TRC_7( ITASK,NIN,IPARI,IND_IMP,
     1                          INTBUF_TAB,NB_INTER_SORTED,LIST_INTER_SORTED,INTER_STRUCT)
!$OMP END SINGLE
            ! -----------
            ! inacti exchange for all interfaces
            IF(NEED_TO_SORT>0.AND.NEED_COMM_INACTI.AND.NB_INTER_7_INACTI>0) THEN
                IF(ITASK==0) THEN
                    IF(NSPMD>1) THEN
                        CALL SPMD_GET_INACTI_GLOBAL(IPARI,NB_INTER_SORTED,LIST_INTER_SORTED,INTER_STRUCT)
                    ELSE
                        DO KK=1,NB_INTER_SORTED
                            N = LIST_INTER_SORTED(KK)
                            IPARI(22,N) = INTER_STRUCT(N)%INACTI
                        ENDDO
                    ENDIF   
                ENDIF
                CALL MY_BARRIER()
            ENDIF
            ! -----------
            ! prepare the sort 
            CALL INTER_PREPARE_SORT( ITASK,NB_INTER_SORTED,LIST_INTER_SORTED,ISENDTO,IRECVFROM,
     .                               IPARI,IAD_ELEM,FR_ELEM,X,V,
     .                               MS,TEMP,KINET,NODNX_SMS,ITAB,
     .                               WEIGHT,INTBUF_TAB,INTER_STRUCT,SORT_COMM,NODNX_SMS_SIZ,
     .                               TEMP_SIZ )
            ! -----------
            ! sorting computation
            CALL INTER_SORT( ITASK,NB_INTER_SORTED,LIST_INTER_SORTED,RETRI,IPARI,
     1                       NSENSOR,ISENDTO,IRECVFROM,INTBUF_TAB,X,ITAB,
     2                       RENUM,NSNFIOLD,MULTI_FVM,H3D_DATA,SENSORS%SENSOR_TAB,
     3                       INTER_STRUCT,SORT_COMM ,RENUM_SIZ)
      ENDIF
      ! end : new sorting algorithm
      ! ------------------------  


      ! ------------------------
      ! old sorting algorithm   
C=======================================================================
C     non implicit options-------
C=======================================================================
C
      IF(IMPL_S/=1)THEN


       IDUM = 0
       DO KK=1,NBINTC
        N   = INTLIST(KK)

        NTY = IPARI(7,N)
        INACTI = IPARI(22,N)
        TYPE18=.FALSE.
        IF(NTY==7 .AND. INACTI==7)TYPE18=.TRUE.

        IF( IMONM > 0 .AND. ITASK ==0 ) THEN
          INTBUF_TAB(N)%METRIC%NOINT   = IPARI(15,N)
          INTBUF_TAB(N)%METRIC%NCONT   = IPARI(18,N)
          INTBUF_TAB(N)%METRIC%MULTIMP = IPARI(23,N)
          INTBUF_TAB(N)%METRIC%NSNR   = MAX(INTBUF_TAB(N)%METRIC%NSNR , IPARI(24,N))
          INTBUF_TAB(N)%METRIC%NSN    = IPARI(5,N) 
          CALL INT_STARTIME(INTBUF_TAB(N)%METRIC,I_MAIN_TRI)   
        ENDIF

        ISENS = 0
        IF(NTY == 7.OR.NTY == 11.OR.NTY == 24.OR.NTY == 25) ISENS = IPARI(64,N)  
        IF (ISENS > 0) THEN    ! IF INTERFACE IS ACTIVATED BY SENSOR 
           TS = SENSORS%SENSOR_TAB(ISENS)%TSTART
        ELSE
           TS = TT
        ENDIF
c-----------------------------------------------------------------------
        IF(TYPE18.OR.(NTY==18)) THEN
c-----------------------------------------------------------------------
          NRTM_T = IPARI(4,N)/NTHREAD
          ESHIFT = ITASK*NRTM_T
          IF(ITASK==NTHREAD-1)NRTM_T=IPARI(4,N)-(NTHREAD-1)*NRTM_T
          IF(INTBUF_TAB(N)%S_NIGE/=0) THEN
            X_IGE(3*NUMNOD+1:3*(NUMNOD+INTBUF_TAB(N)%S_NIGE)) = INTBUF_TAB(N)%XIGE(1:3*INTBUF_TAB(N)%S_NIGE)
            PTR_X => X_IGE
            V_IGE(3*NUMNOD+1:3*(NUMNOD+INTBUF_TAB(N)%S_NIGE)) = INTBUF_TAB(N)%VIGE(1:3*INTBUF_TAB(N)%S_NIGE)
            PTR_V => V_IGE
            PTR_MS => MS(1:NUMNOD)  
            PTR_KINET => KINET(1:NUMNOD)      
          ELSEIF (MULTI_FVM%IS_USED .AND. TYPE18) THEN
             PTR_X => MULTI_FVM%X_APPEND
             PTR_V => MULTI_FVM%V_APPEND
             PTR_MS => MULTI_FVM%MASS_APPEND
             PTR_KINET => MULTI_FVM%KINET_APPEND(1:NUMNOD+NUMELS)
          ELSE
            PTR_X => X
            PTR_V => V
            PTR_MS => MS(1:NUMNOD)
            PTR_KINET => KINET(1:NUMNOD)
          ENDIF
       CALL I7MAIN_TRI(
     1 IPARI   ,PTR_X   ,PTR_V   ,
     2 PTR_MS  ,N            ,ITASK        ,WAG     ,WEIGHT  ,
     3 ISENDTO ,IRECVFROM    ,RETRI        ,IAD_ELEM,FR_ELEM ,
     4 ITAB    ,PTR_KINET    ,TEMP         ,NRTM_T ,RENUM   ,
     5 NSNFIOLD,ESHIFT       ,IDUM         ,IDUM    ,NODNX_SMS ,
     6 INTBUF_TAB(N),H3D_DATA,IXS,MULTI_FVM)
c-----------------------------------------------------------------------
        ELSEIF(NTY == 10)THEN
c-----------------------------------------------------------------------
          NRTM_T = IPARI(4,N)/NTHREAD
          ESHIFT = ITASK*NRTM_T
          IF(ITASK==NTHREAD-1)NRTM_T=IPARI(4,N)-(NTHREAD-1)*NRTM_T
          CALL I10MAIN_TRI(
     1 IPARI(1,N),X       ,V         ,
     2 MS        ,N            ,ITASK        ,WAG     ,WEIGHT    ,
     3 ISENDTO   ,IRECVFROM    ,RETRI        ,IAD_ELEM,FR_ELEM   ,
     4 NRTM_T    ,RENUM        ,NSNFIOLD     ,ESHIFT  ,IDUM      ,
     5 IDUM      ,NODNX_SMS    ,ITAB         ,INTBUF_TAB(N)      ,
     6 H3D_DATA   )
c-----------------------------------------------------------------------
        ELSEIF(NTY == 11.AND.TT>=TS)THEN
c-----------------------------------------------------------------------
          NRTM_T = IPARI(4,N)/NTHREAD
          ESHIFT = ITASK*NRTM_T
          IF(ITASK==NTHREAD-1)NRTM_T=IPARI(4,N)-(NTHREAD-1)*NRTM_T
          CALL I11MAIN_TRI(
     1 IPARI      ,X       ,V        ,
     2 MS         ,N            ,ITASK        ,WEIGHT  ,ISENDTO  ,
     3 IRECVFROM  ,RETRI        ,IAD_ELEM     ,FR_ELEM ,ITAB     ,
     4 NRTM_T     ,ESHIFT       ,NODNX_SMS    ,RENUM   ,NSNFIOLD ,
     5 INTBUF_TAB(N),TEMP       )
c-----------------------------------------------------------------------
        ELSEIF(NTY == 17)THEN
c-----------------------------------------------------------------------
          IF(IPARI(33,N) == 0)THEN
C
            IAD17=1
            DO K=1,N-1
              NTY   =IPARI(7,K)
              IF(IPARI(7,K) == 17.AND.IPARI(33,K) == 0)THEN
                IGN   =IPARI(36,K)
                IGE   =IPARI(34,K)
                NMES  =IGRBRIC(IGN)%NENTITY
                NME   =IGRBRIC(IGE)%NENTITY
                IAD17 = IAD17+6*(NME+NMES)
              END IF
            END DO
C
            IGN   =IPARI(36,N)
            IGE   =IPARI(34,N)
            NMES  =IGRBRIC(IGN)%NENTITY
            NME   =IGRBRIC(IGE)%NENTITY
            NME_T = NME/NTHREAD
            ESHIFT = ITASK*NME_T
            IF(ITASK==NTHREAD-1)NME_T=NME-(NTHREAD-1)*(NME/NTHREAD)
            CALL I17MAIN_TRI(
     1 IPARI       ,INTBUF_TAB(N),X       ,N       ,
     2 ITASK       ,IGRBRIC      ,NME     ,NMES    ,
     3 EMINX(IAD17),IXS          ,IXS16        ,IXS20   ,WEIGHT  ,
     3 ISENDTO     ,IRECVFROM    ,RETRI        ,IAD_ELEM,FR_ELEM ,
     4 ITAB        ,V            ,NME_T        ,ESHIFT           )
          END IF
c-----------------------------------------------------------------------
        ELSEIF(NTY == 20)THEN
c-----------------------------------------------------------------------
          NRTM_T = IPARI(4,N)/NTHREAD
          ESHIFT = ITASK*NRTM_T
          IF(ITASK==NTHREAD-1)NRTM_T=IPARI(4,N)-(NTHREAD-1)*NRTM_T
          CALL I20MAIN_TRI(
     1 IPARI   ,X       ,V       ,
     2 MS      ,N            ,ITASK        ,WAG     ,WEIGHT  ,
     3 ISENDTO ,IRECVFROM    ,RETRI        ,IAD_ELEM,FR_ELEM ,
     4 ITAB    ,KINET        ,TEMP         ,NRTM_T  ,RENUM   ,
     5 NSNFIOLD,ESHIFT       ,IDUM         ,IDUM    ,DIAG_SMS,
     6 NODNX_SMS,INTBUF_TAB(N),H3D_DATA)
c-----------------------------------------------------------------------
        ELSEIF(NTY == 22)THEN
c-----------------------------------------------------------------------         
          NRTM_T = IPARI(4,N)/NTHREAD
          ESHIFT = ITASK*NRTM_T
          IF(ITASK==NTHREAD-1)NRTM_T=IPARI(4,N)-(NTHREAD-1)*NRTM_T
          CALL I22MAIN_TRI(
     1                     IPARI       ,X            ,V            ,
     2                     MS          ,N            ,ITASK        ,WAG            ,WEIGHT       ,
     3                     ISENDTO     ,IRECVFROM    ,RETRI        ,IAD_ELEM       ,FR_ELEM      ,
     4                     ITAB        ,KINET        ,TEMP         ,NRTM_T         ,RENUM        ,
     5                     NSNFIOLD    ,ESHIFT       ,IDUM         ,IDUM           ,NODNX_SMS    ,
     6                     IXS         ,IGRBRIC      ,ALE_CONNECTIVITY  ,INTBUF_TAB(N),
     7                     COUNT_REMSLV,H3D_DATA     ,MULTI_FVM)

          CALL I22SUBVOL(
     1                   X       ,N            ,ITASK        ,IPARI(48:50,N) ,ITAB         ,
     2                   IXS     ,IXTG         ,V            ,IPARG          ,ELBUF_TAB    ,
     3                   W       ,IGRSH3N      )            !OPTIM : mettre avant ecriture animation

          CALL MY_BARRIER
          IF(ITASK==0)THEN            
            DEALLOCATE(IRECT_L)
          END IF
          !--------------------------------------------------------------
          !    CINEMATIC TIME STEP (MINIMUM LENGTH)
          !-------------------------------------------------------------- 
           NBF              = 1+ITASK*NB/NTHREAD
           NBL              = (ITASK+1)*NB/NTHREAD
           DX22MIN_L(ITASK) = EP30
           DX22_MIN         = EP30
           NIN              = 1

           DO IB = NBF,NBL  !1,NBRIC
              NBCUT            = BRICK_LIST(NIN,IB)%NBCUT
              IF(NBCUT==0)CYCLE
              DO J=1,12              
                NBCUT            = BRICK_LIST(NIN,IB)%Edge(J)%NBCUT
                IF(NBCUT == 0) CYCLE
                LEN              = BRICK_LIST(NIN,IB)%Edge(J)%LEN
                DX22MIN_L(ITASK) = MIN(DX22MIN_L(ITASK), LEN)
              ENDDO
           ENDDO !IB=1,NBRIC
           
           CALL MY_BARRIER
           
#include "lockon.inc"
          DX22_MIN = MIN(DX22_MIN,DX22MIN_L(ITASK)) 
#include "lockoff.inc"           
        
c-----------------------------------------------------------------------
        ELSEIF(NTY == 23)THEN
c-----------------------------------------------------------------------
          NRTM_T = IPARI(4,N)/NTHREAD
          ESHIFT = ITASK*NRTM_T
          IF(ITASK==NTHREAD-1)NRTM_T=IPARI(4,N)-(NTHREAD-1)*NRTM_T
          CALL I23MAIN_TRI(
     1 IPARI   ,X            ,INTBUF_TAB(N),V       ,
     2 MS      ,N            ,ITASK        ,WAG     ,WEIGHT  ,
     3 ISENDTO ,IRECVFROM    ,RETRI        ,IAD_ELEM,FR_ELEM ,
     4 ITAB    ,KINET        ,NRTM_T       ,RENUM   ,
     5 NSNFIOLD,ESHIFT       ,IDUM         ,IDUM    ,NODNX_SMS ,
     6 H3D_DATA,MULTI_FVM)
c-----------------------------------------------------------------------
        ELSEIF(NTY == 24.AND.TT>=TS)THEN
c-----------------------------------------------------------------------
c          NRTM_T = IPARI(4,N)/NTHREAD
c          ESHIFT = ITASK*NRTM_T
c          IF(ITASK==NTHREAD-1)NRTM_T=IPARI(4,N)-(NTHREAD-1)*NRTM_T
          NRTM_T = (IPARI(4,N)-IPARI(42,N))/NTHREAD
          ESHIFT = ITASK*NRTM_T
          IF(ITASK==NTHREAD-1)NRTM_T=(IPARI(4,N)-IPARI(42,N))
     +                                -(NTHREAD-1)*NRTM_T
         NSNE3 = 3*IPARI(55,N)
        IF (NSNE3 >0 ) THEN
         CALL MY_BARRIER
!$OMP SINGLE
         XE(NNOD3+1:(NNOD3+NSNE3)) = INTBUF_TAB(N)%XFIC(1:NSNE3)
         VE(NNOD3+1:(NNOD3+NSNE3)) = INTBUF_TAB(N)%VFIC(1:NSNE3)
!$OMP END SINGLE
         CALL I24MAIN_TRI(
     1 IPARI   ,XE      ,VE  ,INTBUF_TAB(N),
     2 MS      ,N            ,ITASK        ,WAG     ,WEIGHT  ,
     3 ISENDTO ,IRECVFROM    ,RETRI        ,IAD_ELEM,FR_ELEM ,
     4 ITAB    ,KINET        ,TEMP         ,NRTM_T  ,RENUM     ,
     5 NSNFIOLD,ESHIFT       ,IDUM         ,IDUM    ,NODNX_SMS ,
     6 H3D_DATA,T2MAIN_SMS ,FORNEQS      ,T2FAC_SMS,INTERFACES%PARAMETERS)
        ELSE
       CALL I24MAIN_TRI(
     1 IPARI   ,X       ,V   ,INTBUF_TAB(N),
     2 MS      ,N            ,ITASK        ,WAG     ,WEIGHT  ,
     3 ISENDTO ,IRECVFROM    ,RETRI        ,IAD_ELEM,FR_ELEM ,
     4 ITAB    ,KINET        ,TEMP         ,NRTM_T  ,RENUM     ,
     5 NSNFIOLD,ESHIFT       ,IDUM         ,IDUM    ,NODNX_SMS ,
     6 H3D_DATA,T2MAIN_SMS ,FORNEQS      ,T2FAC_SMS ,INTERFACES%PARAMETERS)
        END IF !(NSNE >0 ) THEN
c-----------------------------------------------------------------------
        ELSEIF(NTY == 25.AND.TT>=TS)THEN
c-----------------------------------------------------------------------
          NEDGE_T = IPARI(68,N)/NTHREAD
          ESHIFT = ITASK*NEDGE_T
          IF(ITASK==NTHREAD-1)NEDGE_T=IPARI(68,N)
     +                                -(NTHREAD-1)*NEDGE_T
          NRTM_T = (IPARI(4,N)-IPARI(42,N))/NTHREAD
          SSHIFT = ITASK*NRTM_T
          IF(ITASK==NTHREAD-1)NRTM_T=(IPARI(4,N)-IPARI(42,N))
     +                                -(NTHREAD-1)*NRTM_T
          CALL I25MAIN_TRI(
     1      IPARI   ,X       ,V   ,INTBUF_TAB(N),
     2      MS      ,N            ,ITASK        ,WEIGHT  ,
     3      ISENDTO ,IRECVFROM    ,RETRI        ,IAD_ELEM,FR_ELEM ,
     4      ITAB    ,KINET        ,TEMP         ,RENUM     ,
     5      NSNFIOLD,IDUM         ,IDUM         ,NODNX_SMS ,
     6      H3D_DATA,ESHIFT       ,NEDGE_T      ,SSHIFT    ,NRTM_T ,
     7      ICODT   ,ISKEW        ,INTERFACES%PARAMETERS)
C-----------------------------------------------------------------------
        ENDIF
        IF(IMONM > 0) THEN
        IF(ITASK==0) CALL INT_STOPTIME(INTBUF_TAB(N)%METRIC,I_MAIN_TRI)   
        ENDIF

C-----------------------------------------------------------------------
       ENDDO
C
       DO KK=1,NINTSTAMP
          N = INTSTAMP(KK)%NOINTER
          ISENS = IPARI(64,N)       ! INTERFACE SENSOR NUMBER
          IF (ISENS > 0) THEN       ! IF INTERFACE IS ACTIVATED BY SENSOR 
            TS = SENSORS%SENSOR_TAB(ISENS)%TSTART
          ELSE
            TS = TT
          ENDIF
          IF(TT>=TS)THEN
            RETRI21(N) = 0
            CALL I21MAIN_TRI(
     1          IPARI   ,X       ,N            ,
     2          ITASK  ,WEIGHT   ,RETRI21(N)   ,IDUM    ,IDUM    ,
     3          INTSTAMP(KK) ,WAG,INTBUF_TAB(N) )
           IF(RETRI21(N)==1) RETRI = 1 
          ENDIF
       ENDDO
      ELSE
C=======================================================================
C      implicit options-------
C=======================================================================
      IADI = 1
      DO KK=1,NBINTC
        N   = INTLIST(KK)
C
        ISENS = 0
        IF(NTY == 7.OR.NTY == 11.OR.NTY == 24.OR.NTY == 25) ISENS = IPARI(64,N)  
        IF (ISENS > 0) THEN    ! IF INTERFACE IS ACTIVATED BY SENSOR 
           TS = SENSORS%SENSOR_TAB(ISENS)%TSTART
        ELSE
           TS = TT
        ENDIF
C
        NTY = IPARI(7,N)
        TYPE18 = .FALSE.
        INACTI = IPARI(22,N)
        IF(NTY == 7 .AND. INACTI ==7)TYPE18=.TRUE.
C-----------------------------------------------------------------------
        IF((NTY == 7.AND.TT>=TS).OR.NTY == 18)THEN
C-----------------------------------------------------------------------
          NRTM_T = IPARI(4,N)/NTHREAD
          ESHIFT = ITASK*NRTM_T
          IF(ITASK==NTHREAD-1)NRTM_T=IPARI(4,N)-(NTHREAD-1)*NRTM_T
          IF(INTBUF_TAB(N)%S_NIGE/=0) THEN
            X_IGE(3*NUMNOD+1:3*(NUMNOD+INTBUF_TAB(N)%S_NIGE)) = INTBUF_TAB(N)%XIGE(1:3*INTBUF_TAB(N)%S_NIGE)
            PTR_X => X_IGE
            V_IGE(3*NUMNOD+1:3*(NUMNOD+INTBUF_TAB(N)%S_NIGE)) = INTBUF_TAB(N)%VIGE(1:3*INTBUF_TAB(N)%S_NIGE)
            PTR_V => V_IGE
            PTR_MS => MS(1:NUMNOD)
            PTR_KINET => KINET(1:NUMNOD)
          ELSEIF (MULTI_FVM%IS_USED .AND. TYPE18) THEN
            PTR_X => MULTI_FVM%X_APPEND
            PTR_V => MULTI_FVM%V_APPEND
            PTR_MS => MULTI_FVM%MASS_APPEND
            PTR_KINET => MULTI_FVM%KINET_APPEND(1:NUMNOD+NUMELS)
          ELSE          
            PTR_X => X
            PTR_V => V
            PTR_MS => MS(1:NUMNOD)
            PTR_KINET => KINET(1:NUMNOD)
          ENDIF
      CALL I7MAIN_TRI(
     1 IPARI   ,PTR_X   ,PTR_V   ,
     2 PTR_MS  ,N            ,ITASK        ,WAG     ,WEIGHT  ,
     3 ISENDTO ,IRECVFROM    ,RETRI        ,IAD_ELEM,FR_ELEM ,
     4 ITAB    ,PTR_KINET    ,TEMP         ,NRTM_T        ,RENUM     ,
     5 NSNFIOLD,ESHIFT       ,NUM_IMP(N)   ,IND_IMP(IADI) ,NODNX_SMS ,
     6 INTBUF_TAB(N),H3D_DATA,IXS,MULTI_FVM)
            IADI = IADI+NUM_IMP(N)
c-----------------------------------------------------------------------
        ELSEIF(NTY == 24.AND.TT>=TS)THEN
c-----------------------------------------------------------------------
c          NRTM_T = IPARI(4,N)/NTHREAD
c          ESHIFT = ITASK*NRTM_T
c          IF(ITASK==NTHREAD-1)NRTM_T=IPARI(4,N)-(NTHREAD-1)*NRTM_T
          NRTM_T = (IPARI(4,N)-IPARI(42,N))/NTHREAD
          ESHIFT = ITASK*NRTM_T
          IF(ITASK==NTHREAD-1)NRTM_T=(IPARI(4,N)-IPARI(42,N))
     +                                -(NTHREAD-1)*NRTM_T
C-------  KINET,MS used for commu remot secnd, for the moment no need.   
         NSNE3 = 3*IPARI(55,N)
        IF (NSNE3 >0 ) THEN
         CALL MY_BARRIER
!$OMP SINGLE
         XE(NNOD3+1:(NNOD3+NSNE3)) = INTBUF_TAB(N)%XFIC(1:NSNE3)
         VE(NNOD3+1:(NNOD3+NSNE3)) = INTBUF_TAB(N)%VFIC(1:NSNE3)
!$OMP END SINGLE
       CALL I24MAIN_TRI(
     1 IPARI   ,XE      ,VE      ,INTBUF_TAB(N),
     2 MS      ,N            ,ITASK        ,WAG     ,WEIGHT  ,
     3 ISENDTO ,IRECVFROM    ,RETRI        ,IAD_ELEM,FR_ELEM ,
     4 ITAB    ,KINET        ,TEMP         ,NRTM_T  ,RENUM     ,
     5 NSNFIOLD,ESHIFT       ,NUM_IMP(N)   ,IND_IMP(IADI),NODNX_SMS ,
     6 H3D_DATA,T2MAIN_SMS ,FORNEQS        ,T2FAC_SMS,INTERFACES%PARAMETERS)
            IADI = IADI+NUM_IMP(N)
        ELSE
       CALL I24MAIN_TRI(
     1 IPARI   ,X       ,V       ,INTBUF_TAB(N),
     2 MS      ,N            ,ITASK        ,WAG     ,WEIGHT  ,
     3 ISENDTO ,IRECVFROM    ,RETRI        ,IAD_ELEM,FR_ELEM ,
     4 ITAB    ,KINET        ,TEMP         ,NRTM_T  ,RENUM     ,
     5 NSNFIOLD,ESHIFT       ,NUM_IMP(N)   ,IND_IMP(IADI),NODNX_SMS ,
     6 H3D_DATA,T2MAIN_SMS ,FORNEQS        ,T2FAC_SMS,INTERFACES%PARAMETERS)
            IADI = IADI+NUM_IMP(N)
        END IF !(NSNE >0 ) THEN
c-----------------------------------------------------------------------
        ELSEIF(NTY == 25.AND.TT>=TS)THEN
c-----------------------------------------------------------------------
          NEDGE_T = IPARI(68,N)/NTHREAD
          ESHIFT  = ITASK*NEDGE_T
          IF(ITASK==NTHREAD-1)NEDGE_T=IPARI(68,N)
     +                                -(NTHREAD-1)*NEDGE_T
          NRTM_T = (IPARI(4,N)-IPARI(42,N))/NTHREAD
          SSHIFT = ITASK*NRTM_T
          IF(ITASK==NTHREAD-1)NRTM_T=(IPARI(4,N)-IPARI(42,N))
     +                                -(NTHREAD-1)*NRTM_T
C-------  KINET,MS used for commu remot secnd, for the moment no need.   
          CALL I25MAIN_TRI(
     1      IPARI   ,X       ,V       ,INTBUF_TAB(N),
     2      MS      ,N            ,ITASK        ,WEIGHT  ,
     3      ISENDTO ,IRECVFROM    ,RETRI        ,IAD_ELEM,FR_ELEM ,
     4      ITAB    ,KINET        ,TEMP         ,RENUM     ,
     5      NSNFIOLD,NUM_IMP(N)   ,IND_IMP(IADI),NODNX_SMS ,
     6      H3D_DATA,ESHIFT,NEDGE_T ,SSHIFT     ,NRTM_T    ,
     7      ICODT   ,ISKEW        ,INTERFACES%PARAMETERS)
            IADI = IADI+NUM_IMP(N)
C-----------------------------------------------------------------------
        ELSEIF(NTY == 10)THEN
C-----------------------------------------------------------------------
          NRTM_T = IPARI(4,N)/NTHREAD
          ESHIFT = ITASK*NRTM_T
          IF(ITASK==NTHREAD-1)NRTM_T=IPARI(4,N)-(NTHREAD-1)*NRTM_T
          CALL I10MAIN_TRI(
     1 IPARI(1,N),X       ,V         ,
     2 MS        ,N            ,ITASK        ,WAG     ,WEIGHT    ,
     3 ISENDTO   ,IRECVFROM    ,RETRI        ,IAD_ELEM,FR_ELEM   ,
     4 NRTM_T    ,RENUM        ,NSNFIOLD     ,ESHIFT  ,NUM_IMP(N),
     5 IND_IMP(IADI) ,NODNX_SMS,ITAB         ,INTBUF_TAB(N)      ,
     6 H3D_DATA)
            IADI = IADI+NUM_IMP(N)
C-----------------------------------------------------------------------
        ELSEIF(NTY == 11.AND.TT>=TS)THEN
C-----------------------------------------------------------------------
          NRTM_T = IPARI(4,N)/NTHREAD
          ESHIFT = ITASK*NRTM_T
          IF(ITASK==NTHREAD-1)NRTM_T=IPARI(4,N)-(NTHREAD-1)*NRTM_T
          CALL I11MAIN_TRI(
     1 IPARI      ,X       ,V        ,
     2 MS         ,N            ,ITASK        ,WEIGHT  ,ISENDTO  ,
     3 IRECVFROM  ,RETRI        ,IAD_ELEM     ,FR_ELEM ,ITAB     ,
     4 NRTM_T     ,ESHIFT       ,NODNX_SMS    ,RENUM   ,NSNFIOLD ,
     5 INTBUF_TAB(N),TEMP )
C-----------------------------------------------------------------------
        ELSEIF(NTY == 17)THEN
C-----------------------------------------------------------------------
          IF(IPARI(33,N) == 0)THEN
            IGN   =IPARI(36,N)
            IGE   =IPARI(34,N)
            NMES  =IGRBRIC(IGN)%NENTITY
            NME   =IGRBRIC(IGE)%NENTITY
            NME_T = NME/NTHREAD
            ESHIFT = ITASK*NME_T
            IF(ITASK==NTHREAD-1)NME_T=NME-(NTHREAD-1)*(NME/NTHREAD)
            CALL I17MAIN_TRI(
     1 IPARI       ,INTBUF_TAB(N),X       ,N       ,
     2 ITASK       ,IGRBRIC      ,NME     ,NMES    ,
     3 EMINX(IAD17),IXS          ,IXS16        ,IXS20   ,WEIGHT  ,
     3 ISENDTO     ,IRECVFROM    ,RETRI        ,IAD_ELEM,FR_ELEM ,
     4 ITAB        ,V            ,NME_T        ,ESHIFT           )
            IAD17 = IAD17+6*(NME+NMES)
          END IF
C-----------------------------------------------------------------------
        ELSEIF(NTY == 20)THEN
C-----------------------------------------------------------------------
          NRTM_T = IPARI(4,N)/NTHREAD
          ESHIFT = ITASK*NRTM_T
          IF(ITASK==NTHREAD-1)NRTM_T=IPARI(4,N)-(NTHREAD-1)*NRTM_T
          CALL I20MAIN_TRI(
     1 IPARI   ,X       ,V       ,
     2 MS      ,N            ,ITASK        ,WAG     ,WEIGHT  ,
     3 ISENDTO ,IRECVFROM    ,RETRI        ,IAD_ELEM,FR_ELEM ,
     4 ITAB    ,KINET        ,TEMP         ,NRTM_T    ,RENUM        ,
     5 NSNFIOLD,ESHIFT       ,NUM_IMP(N)   ,IND_IMP(IADI),DIAG_SMS,
     6 NODNX_SMS,INTBUF_TAB(N),H3D_DATA)
            IADI = IADI+NUM_IMP(N)
C-----------------------------------------------------------------------
        ENDIF
C-----------------------------------------------------------------------
      ENDDO
      ENDIF
C=======================================================================
C
      CALL MY_BARRIER()
C
C=======================================================================
C     OPTIMISATION DU TRI A CHAQUE CYCLE, T25
C=======================================================================
      IF(NINTER25 /= 0)THEN
        IF(ITASK == 0) CALL STOPTIME(17,1)

        IF(IDEL7NOK_SAV/=0)THEN
          IF(ITASK == 0) CALL STOPTIME(2,1)  
          IF(ITASK == 0) CALL STARTIME(8,1)  
          CALL I25MAIN_FREE(ITASK, IPARI  ,INTBUF_TAB ,INTLIST25, ISENDTO,
     2                      IRECVFROM)
C
          CALL MY_BARRIER()
C
          IF(ITASK == 0) CALL STARTIME(2,1)  
          IF(ITASK == 0) CALL STOPTIME(8,1)  
        END IF

        DO KK=1,NINTER25
         N     = INTLIST25(KK)
         NTY   =IPARI(7,N)
C--------TEST IF INTERFACE IS ACTIVE WHEN USING SENSOR-----------
         ISENS = 0

         IF(IMONM > 0) THEN
         IF(ITASK ==0)  CALL INT_STARTIME(INTBUF_TAB(N)%METRIC,I_MAIN_OPT_TRI)  
         ENDIF

         ISENS = IPARI(64,N)      
         IF   (ISENS > 0) THEN    ! IF INTERFACE IS ACTIVATED BY SENSOR 
            TS = SENSORS%SENSOR_TAB(ISENS)%TSTART
         ELSE
            TS = TT
         ENDIF
C
         IF(TT>=TS)THEN
C-----------------------------------------------------------------------
          CALL I25MAIN_OPT_TRI(
     1       N       ,IPARI  ,INTBUF_TAB(N),X            ,V      ,
     2       ITASK   ,ITAB   ,KINET        ,COUNT_REMSLV, 
     3       COUNT_REMSLVE, NB25_CANDT(ITASK+1), I_OPT_STOK(N))

         ENDIF
C-----------------------------------------------------------------------

         IF(IMONM > 0) THEN
           IF(ITASK==0)  CALL INT_STOPTIME(INTBUF_TAB(N)%METRIC,I_MAIN_OPT_TRI)  
         ENDIF

        ENDDO     
C=======================================================================
C
C       Calcul // des normales
C
C       il FAUT garder une barriere avant i25main_norm !!!
        CALL MY_BARRIER
C
        IF (IMON>0 .AND. ITASK==0) THEN
          CALL STOPTIME(2,1)  
          CALL STARTIME(8,1)  
          CALL STARTIME(MACRO_TIMER_T25NORM,1)  
        ENDIF

        CALL I25MAIN_NORM(
     1      INTLIST25,IPARI   ,INTBUF_TAB ,ITASK+1  ,X      ,
     2      ITAB    ,NSENSOR,SENSORS%SENSOR_TAB,IAD_FRNOR,FR_NOR   ,
     3      IAD_FREDG,FR_EDG,IAD_ELEM ,FR_ELEM  ,FSKYN25    ,
     4      ADDCSRECT,PROCNOR)

       CALL MY_BARRIER
       IF (IMON>0 .AND. ITASK==0) THEN
         CALL STOPTIME(MACRO_TIMER_T25NORM,1)
         CALL STARTIME(MACRO_TIMER_T25STFE,1)
       ENDIF
       
!$OMP SINGLE
       IF(IDEL7NOK_SAV > 0) THEN
         DO N = 1,NINTER25
          NIN = INTLIST25(N)
          IF(IPARI(MACRO_IEDGE,NIN) > 0) THEN
C
C           Also needed in SMP for resetting STFE !
            CALL SPMD_GET_STIF25_EDG(
     .        INTBUF_TAB(NIN)%STFE,    IPARI(MACRO_NEDGE,NIN), INTBUF_TAB(NIN)%LEDGE,
     .        NIN ,  ISENDTO, IRECVFROM, INTBUF_TAB(NIN)%MPI_COMM, INTBUF_TAB(NIN)%RANK, 
     .        INTBUF_TAB(NIN)%NSPMD)
          ENDIF
         ENDDO
       ENDIF
!$OMP END SINGLE

C
        IF (IMON>0 .AND. ITASK==0) THEN
          CALL STOPTIME(MACRO_TIMER_T25STFE,1)
          CALL STOPTIME(8,1)  
          CALL STARTIME(2,1)  
        END IF

C=======================================================================
        CALL MY_BARRIER
C
        IF (IMON>0 .AND. ITASK==0) THEN
          CALL STOPTIME(2,1) 
          CALL STARTIME(8,1)  
          CALL STARTIME(MACRO_TIMER_T25SLIDING,1)
        END IF 
C
C statistique interface
        IF (DEBUG(3)>=1.AND.NCYCLE==0) THEN
          NB25_CANDT(ITASK+1) = 0
          NB25_IMPCT(ITASK+1) = 0
          NB25_DST1(ITASK+1)  = 0
          NB25_DST2(ITASK+1)  = 0
        ENDIF
      
        CALL I25MAIN_SLID(
     1      IPARI    ,IAD_ELEM ,FR_ELEM   ,ITAB     ,SENSORS%SENSOR_TAB,
     2      NSENSOR  ,INTLIST25,INTBUF_TAB ,IAD_FRNOR,FR_NOR ,
     3      X        ,V       ,MS      ,TEMP   ,KINET      ,
     4      NATIV_SMS,ITASK+1 ,NB25_DST2, MAIN_PROC,
     5      NEWFRONT ,ISENDTO ,IRECVFROM ,NBINTC,
     6      INTLIST  ,ISLEN7   ,IRLEN7  ,IRLEN7T  ,ISLEN7T,
     7      NB25_DST1,H3D_DATA,  ICODT,ISKEW,INTERFACES%PARAMETERS)
C
        CALL MY_BARRIER
C
        IF (IMON>0 .AND. ITASK==0) THEN
          CALL STOPTIME(MACRO_TIMER_T25SLIDING,1)
          CALL STOPTIME(8,1)  
          CALL STARTIME(2,1)  
        END IF

        IF(ITASK == 0) CALL STARTIME(17,1)
C
      END IF
      ! end : old sorting algorithm 
      ! ------------------------

      ! ------------------------------
      ! new sorting algorithm 
      ! deallocation & wait
      CALL MY_BARRIER()
      ! ------------------------
      ! explicit part
      IF(IMPL_S/=1)THEN
        CALL INTER_DEALLOCATE_WAIT( ITASK,NB_INTER_SORTED,LIST_INTER_SORTED,IPARI,
     1       NSENSOR,IRECVFROM,SENSORS%SENSOR_TAB,INTER_STRUCT,SORT_COMM )
      ENDIF
      ! end : new sorting algorithm 
      ! ------------------------------
C=======================================================================
C
C Partie non parallele
C
!$OMP SINGLE
      IF (IMONM > 0) THEN
        CALL STOPTIME(17,1)
        IF(IMONM == 2 .AND. NSPMD > 1)THEN
          CALL STARTIME(58,1)
          CALL SPMD_BARRIER()
          CALL STOPTIME(58,1)
        END IF
      END IF 
      IF (NSPMD > 1 .AND. (RETRI == 1 .OR. NINTER25 > 0 ) ) THEN        
C
C Communication after sorting
C
        IF (IMONM > 0) CALL STARTIME(18,1)
        CALL SPMD_IFRONT(
     1    IPARI   ,NEWFRONT,ISENDTO ,IRECVFROM,
     2    NSENSOR ,NBINTC  ,INTLIST ,ISLEN7  ,IRLEN7   ,
     3    ISLEN11 ,IRLEN11 ,ISLEN17 ,IRLEN17 ,IRLEN7T  ,
     4    ISLEN7T ,IRLEN20 ,ISLEN20 ,IRLEN20T,ISLEN20T ,
     5    IRLEN20E,ISLEN20E,SENSORS%SENSOR_TAB,INTBUF_TAB, 1 )

        IF(NINTSTAMP /= 0.AND.FTEMPVAR21==1) THEN
          CALL SPMD_IFRONT_STAMP(
     1      IPARI   ,NSENSOR ,INTBUF_TAB, RETRI21,TEMP    ,SENSORS%SENSOR_TAB,  ITAB,
     2      NBINTC21,INTLIST21)
        ENDIF
         
C Fin Partie non parallele
        IF (IMONM > 0) CALL STOPTIME(18,1)
      ENDIF
C
      IF (IMONM > 0)  CALL STARTIME(19,1)
!$OMP END SINGLE
C
C=======================================================================
C     OPTIMISATION DU TRI A CHAQUE CYCLE, ALL BUT T25
C=======================================================================
      DO KK=1,NBINTC
        N = INTLIST(KK)
        NTY   =IPARI(7,N)
C--------TEST IF INTERFACE IS ACTIVE WHEN USING SENSOR-----------
        ISENS = 0


      IF(IMONM > 0) THEN
      IF(ITASK ==0)  CALL INT_STARTIME(INTBUF_TAB(N)%METRIC,I_MAIN_OPT_TRI)  
      ENDIF



        IF(NTY == 7.OR.NTY == 11.OR.NTY == 24.OR.NTY == 25) ISENS = IPARI(64,N)      
        IF (ISENS > 0) THEN    ! IF INTERFACE IS ACTIVATED BY SENSOR 
           TS = SENSORS%SENSOR_TAB(ISENS)%TSTART
        ELSE
           TS = TT
        ENDIF

        TYPE18 = .FALSE.
        INACTI = IPARI(22,N)
        IF(NTY == 7 .AND. INACTI ==7)TYPE18=.TRUE.
C
C-----------------------------------------------------------------------
        IF(NTY == 7.AND.TT>=TS)THEN
C-----------------------------------------------------------------------

          IF(INTBUF_TAB(N)%S_NIGE/=0) THEN
            X_IGE(3*NUMNOD+1:3*(NUMNOD+INTBUF_TAB(N)%S_NIGE)) = INTBUF_TAB(N)%XIGE(1:3*INTBUF_TAB(N)%S_NIGE)
            PTR_X => X_IGE
            V_IGE(3*NUMNOD+1:3*(NUMNOD+INTBUF_TAB(N)%S_NIGE)) = INTBUF_TAB(N)%VIGE(1:3*INTBUF_TAB(N)%S_NIGE)
            PTR_V => V_IGE
          ELSEIF (MULTI_FVM%IS_USED .AND. TYPE18) THEN
            PTR_X => MULTI_FVM%X_APPEND
            PTR_V => MULTI_FVM%V_APPEND
          ELSE
            PTR_X => X
            PTR_V => V
          ENDIF

          CALL I7MAIN_OPT_TRI(
     1       IPARI     ,PTR_X        ,PTR_V,
     2       N         ,ITASK        ,COUNT_REMSLV ,INTBUF_TAB(N))
          
C-----------------------------------------------------------------------
        ELSEIF(NTY == 10)THEN
C-----------------------------------------------------------------------
          CALL I10MAIN_OPT_TRI(
     1       IPARI(1,N),X  ,V  ,
     2       N         ,ITASK        ,COUNT_REMSLV ,INTBUF_TAB(N))
C-----------------------------------------------------------------------
        ELSEIF(NTY == 11.AND.TT>=TS)THEN
C-----------------------------------------------------------------------
          CALL I11MAIN_OPT_TRI(
     1       IPARI     ,INTBUF_TAB(N),X  ,V  ,
     2       N         ,ITASK        ,COUNT_REMSLV )
C-----------------------------------------------------------------------
        ELSEIF(NTY == 20)THEN
C-----------------------------------------------------------------------
          CALL I20MAIN_OPT_TRI(
     1       IPARI     ,X   ,V  ,
     2       N         ,ITASK        ,COUNT_REMSLV,COUNT_REMSLVE,
     3       INTBUF_TAB(N) )
C-----------------------------------------------------------------------
        ELSEIF(NTY == 22)THEN
C-----------------------------------------------------------------------
          !
C-----------------------------------------------------------------------
        ELSEIF(NTY == 23)THEN
C-----------------------------------------------------------------------
          CALL I23MAIN_OPT_TRI(
     1      IPARI     ,INTBUF_TAB(N),N   ,ITASK  ,
     2      COUNT_REMSLV,X          )
C-----------------------------------------------------------------------
        ELSEIF(NTY == 24.AND.TT>=TS)THEN
C-----------------------------------------------------------------------
         NSNE3 = 3*IPARI(55,N)
         IF (NSNE3 >0 ) THEN
         CALL MY_BARRIER
!$OMP SINGLE
         XE(NNOD3+1:(NNOD3+NSNE3)) = INTBUF_TAB(N)%XFIC(1:NSNE3)
         VE(NNOD3+1:(NNOD3+NSNE3)) = INTBUF_TAB(N)%VFIC(1:NSNE3)
!$OMP END SINGLE
          CALL I24MAIN_OPT_TRI(
     1       IPARI     ,INTBUF_TAB(N),XE  ,VE ,
     2       N         ,ITASK        ,COUNT_REMSLV, T2MAIN_SMS,LSKYI_SMS_NEW)
         ELSE
          CALL I24MAIN_OPT_TRI(
     1       IPARI     ,INTBUF_TAB(N),X   ,V  ,
     2       N         ,ITASK        ,COUNT_REMSLV, T2MAIN_SMS,LSKYI_SMS_NEW)
         ENd IF !(NSNE >0 ) THEN
C-----------------------------------------------------------------------
        ELSEIF(NTY == 25.AND.TT>=TS)THEN
C-----------------------------------------------------------------------
C          CALL I25MAIN_OPT_TRI(
C     1       N       ,IPARI  ,INTBUF_TAB(N),X            ,V      ,
C     2       ITASK   ,ITAB   ,KINET        ,COUNT_REMSLV, 
C     3       COUNT_REMSLVE, NB25_CANDT(ITASK+1), I_OPT_STOK(N))

C-----------------------------------------------------------------------
        ENDIF
C-----------------------------------------------------------------------

      IF(IMONM > 0) THEN
        IF(ITASK==0)  CALL INT_STOPTIME(INTBUF_TAB(N)%METRIC,I_MAIN_OPT_TRI)  
      ENDIF

      ENDDO     
C
      IF (NINTSTAMP/=0) THEN
         IF (DEBUG(3)>=1.AND.NCYCLE==0) THEN
            NB_STOK_N(ITASK+1)=0
            NB_JLT(ITASK+1)=0
         ENDIF
      END IF
C
       DO KK=1,NINTSTAMP
          N = INTSTAMP(KK)%NOINTER
C
          ISENS = IPARI(64,N)       ! INTERFACE SENSOR NUMBER
          IF (ISENS > 0) THEN    ! IF INTERFACE IS ACTIVATED BY SENSOR 
           TS = SENSORS%SENSOR_TAB(ISENS)%TSTART
         ELSE
           TS = TT
         ENDIF
        IF(TT>=TS)THEN          ! INTERFACE SENSOR IS ACTIVATED
C
          CALL I21MAIN_OPT_TRI(
     1      IPARI     ,INTBUF_TAB(N),N   ,ITASK  ,
     2      INTSTAMP(KK),NB_STOK_N,NB_JLT)
C
       ENDIF
      ENDDO

      IF (NINTSTAMP/=0) THEN
       IF (DEBUG(3)>=1) THEN
          IF(MOD(NCYCLE+1,DEBUG(3))==0)THEN
           IF (NB_JLT(ITASK+1)==0) THEN
            PCT1= ZERO
           ELSE
            PCT1 = HUNDRED - HUNDRED*NB_STOK_N(ITASK+1)/NB_JLT(ITASK+1)
           ENDIF
#include "lockon.inc"
           WRITE(ISTDO,'(A,I6,A,I4,A,I4,A,I10,A,I10,2X,F5.2,A)')
     .      ' NCYCLE = ',NCYCLE,
     .      ' NSPMD = ',ISPMD+1,
     .      ' ITASK = ',ITASK+1,
     .      '   CANDIDATS = ',NB_JLT(ITASK+1),
     .      '   OPT CAND = ',NB_STOK_N(ITASK+1),PCT1,'%'
#include "lockoff.inc"
          NB_STOK_N(ITASK+1)=0
          NB_JLT(ITASK+1)=0
        END IF
       END IF
      ENDIF
C
C Partie non parallele
C
      CALL MY_BARRIER()
C
!$OMP SINGLE
      IF (IMONM > 0) CALL STOPTIME(19,1)
      IF (NSNE_MAX>0 ) DEALLOCATE(XE,VE)
!$OMP END SINGLE  

      ! If law151+int18 : shift NSV array
      IF( MULTI_FVM%IS_INT18_LAW151 ) THEN
        CALL MY_BARRIER()
        CALL INT18_LAW151_NSV_SHIFT('-',ITASK,MULTI_FVM,IPARI,INTBUF_TAB)
      ENDIF   
C
C=======================================================================
C Partie parallele
      IF(NINTER25 /= 0)THEN
C
C       il FAUT garder une barriere apres i25main_opt_tri !!!
        CALL MY_BARRIER()
C
        IF (IMON>0 .AND. ITASK==0) THEN
          CALL STOPTIME(2,1) 
          CALL STARTIME(8,1)  
        END IF 
      
        CALL I25MAIND_2(
     1      IPARI    ,ITAB    ,SENSORS%SENSOR_TAB,INTLIST25,INTBUF_TAB ,
     2      X        ,V       ,KINET      ,ITASK+1 ,NB25_DST2,
     3      ICODT    ,ISKEW   ,NSENSOR    )
C 
C       CALL MY_BARRIER() ! Barrier vs timer only     
C
        IF (IMON>0 .AND. ITASK==0) THEN
          CALL STOPTIME(8,1)  
          CALL STARTIME(2,1)
        END IF
C
      END IF
      
        
!$OMP SINGLE
     
      IF ((NSPMD > 1 .AND. (RETRI == 1 .OR. NINTER25 > 0 ))) THEN

! If ifront is done at this cycle (retri or ninter25>0
! and not already terminated because of type25 edge2edge 
C
C Communication apres retri
C
        IF (IMONM > 0) CALL STARTIME(18,1)

        CALL SPMD_IFRONT(
     1    IPARI   ,NEWFRONT,ISENDTO ,IRECVFROM,
     2    NSENSOR ,NBINTC  ,INTLIST ,ISLEN7  ,IRLEN7   ,
     3    ISLEN11 ,IRLEN11 ,ISLEN17 ,IRLEN17 ,IRLEN7T  ,
     4    ISLEN7T ,IRLEN20 ,ISLEN20 ,IRLEN20T,ISLEN20T ,
     5    IRLEN20E,ISLEN20E,SENSORS%SENSOR_TAB,INTBUF_TAB, 2)

       IF(NINTER25E > 0) THEN
         CALL SPMD_I25FRONT_NOR(IPARI,
     .                          INTBUF_TAB,
     .                          INTLIST25,
     .                          X)
        ENDIF


C Fin Partie non parallele
        IF (IMONM > 0) CALL STOPTIME(18,1)

      ENDIF

      DEALLOCATE(X_IGE,V_IGE)
!$OMP END SINGLE

      RETURN
      END
C
