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
Chd|  IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        BFGS_INI                      source/implicit/imp_bfgs.F    
Chd|        CFIELD_IMP                    source/loads/general/load_centri/cfield_imp.F
Chd|        CONDENS_B                     source/implicit/upd_glob_k.F  
Chd|        CP_IMPBUF                     source/implicit/produt_v.F    
Chd|        CP_INT_HP                     source/implicit/produt_v.F    
Chd|        CP_REAL_HP                    source/implicit/produt_v.F    
Chd|        CRIT_LLIM                     source/implicit/imp_solv.F    
Chd|        DEALLOCM                      source/implicit/imp_solv.F    
Chd|        DIM_INT_K                     source/implicit/ind_glob_k.F  
Chd|        DIS_CP                        source/implicit/imp_solv.F    
Chd|        DU_INI_HP                     source/implicit/imp_solv.F    
Chd|        DYNA_CPK0                     source/implicit/imp_dyna.F    
Chd|        DYNA_CPR0                     source/implicit/imp_dyna.F    
Chd|        DYNA_INA                      source/implicit/imp_dyna.F    
Chd|        DYNA_WEX                      source/implicit/imp_dyna.F    
Chd|        ETFAC_INI                     source/implicit/imp_init.F    
Chd|        FIL_SPAN1                     source/implicit/ind_glob_k.F  
Chd|        FORCE_IMP                     source/loads/general/force_imp.F
Chd|        FVBC_IMPL1                    source/constraints/general/impvel/fv_imp0.F
Chd|        FV_DD0                        source/constraints/general/impvel/fv_imp0.F
Chd|        FV_FINT0                      source/constraints/general/impvel/fv_imp0.F
Chd|        FV_IMP                        source/constraints/general/impvel/fv_imp0.F
Chd|        FV_IMP1                       source/constraints/general/impvel/fv_imp0.F
Chd|        FV_RW                         source/constraints/general/impvel/fv_imp0.F
Chd|        GETNDDLI_G                    source/mpi/implicit/imp_fri.F 
Chd|        GET_FEXT                      source/implicit/imp_solv.F    
Chd|        GET_NSPC                      source/constraints/general/bcs/bc_imp0.F
Chd|        GRAVIT_IMP                    source/loads/general/grav/gravit_imp.F
Chd|        IDDL2NOD                      source/implicit/recudis.F     
Chd|        IDEL_INT                      source/implicit/ind_glob_k.F  
Chd|        IMP_B2A                       source/implicit/imp_solv.F    
Chd|        IMP_CHECK                     source/implicit/imp_solv.F    
Chd|        IMP_CHECM                     source/implicit/imp_solv.F    
Chd|        IMP_DTKIN                     source/implicit/imp_int_k.F   
Chd|        IMP_DTN                       source/implicit/imp_dt.F      
Chd|        IMP_DYKV                      source/implicit/imp_dyna.F    
Chd|        IMP_DYKV0                     source/implicit/imp_dyna.F    
Chd|        IMP_DYNAM                     source/implicit/imp_dyna.F    
Chd|        IMP_FR7I                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_GLOB_KHP                  source/implicit/imp_glob_k.F  
Chd|        IMP_INTFR                     source/implicit/imp_solv.F    
Chd|        IMP_INTTD0                    source/implicit/imp_int_k.F   
Chd|        IMP_INT_K                     source/implicit/imp_int_k.F   
Chd|        IMP_KPRES                     source/implicit/imp_glob_k.F  
Chd|        IMP_MUMPS1                    source/implicit/imp_mumps.F   
Chd|        IMP_QIFAM                     source/implicit/imp_dyna.F    
Chd|        IMP_SETB                      source/implicit/imp_setb.F    
Chd|        IMP_STOP                      source/implicit/imp_solv.F    
Chd|        IND_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|        IND_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|        IND_INT_K                     source/implicit/ind_glob_k.F  
Chd|        INI_BMINMA_IMP                source/implicit/imp_solv.F    
Chd|        INI_K0H                       source/implicit/imp_solv.F    
Chd|        INI_KIC                       source/implicit/imp_solv.F    
Chd|        INI_KIF                       source/implicit/imp_solv.F    
Chd|        INT5_DIVERG                   source/implicit/imp_solv.F    
Chd|        INTEGRATOR1_HP                source/implicit/integrator.F  
Chd|        INTEGRATORL_HP                source/implicit/integrator.F  
Chd|        INTEGRATOR_HP                 source/implicit/integrator.F  
Chd|        KIN_KNL                       source/implicit/imp_int_k.F   
Chd|        K_BAND                        source/implicit/imp_solv.F    
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|        MONV_IMP                      source/airbag/monv_imp0.F     
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        M_LNZ                         source/implicit/imp_solv.F    
Chd|        NL_SOLV                       source/implicit/nl_solv.F     
Chd|        PRODUT_HP                     source/implicit/produt_v.F    
Chd|        PRODUT_UHP0                   source/implicit/produt_v.F    
Chd|        PR_INFOK                      source/implicit/imp_solv.F    
Chd|        PR_SOLNFO                     source/implicit/imp_solv.F    
Chd|        PUT_NSPC                      source/constraints/general/bcs/bc_imp0.F
Chd|        QSTAT_INI                     source/implicit/imp_dyna.F    
Chd|        RECUKIN                       source/implicit/recudis.F     
Chd|        RER02                         source/implicit/upd_glob_k.F  
Chd|        RGWAL0_IMP                    source/constraints/general/rwall/rgwal0.F
Chd|        SAVE_KIF                      source/implicit/imp_solv.F    
Chd|        SAV_INTTD                     source/implicit/imp_int_k.F   
Chd|        SPBRM_PRE                     source/implicit/imp_solv.F    
Chd|        SPB_RM_RIG                    source/implicit/imp_solv.F    
Chd|        SPMD_MAX_I                    source/mpi/implicit/imp_spmd.F
Chd|        SPMD_MIN_I                    source/mpi/implicit/imp_spmd.F
Chd|        SPMD_MIN_S                    source/mpi/implicit/imp_spmd.F
Chd|        SPMD_SUMF_A                   source/mpi/implicit/imp_spmd.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        UPD_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|        UPD_RHS                       source/implicit/upd_glob_k.F  
Chd|        VAXPY_HP                      source/implicit/produt_v.F    
Chd|        VSCALY_HP                     source/implicit/produt_v.F    
Chd|        WEIGHTDDL                     source/implicit/recudis.F     
Chd|        ZERO1                         source/system/zero.F          
Chd|        ZEROR_HP                      source/implicit/produt_v.F    
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        IMPBUFDEF_MOD                 share/modules/impbufdef_mod.F 
Chd|        IMP_LINTF                     share/modules/impbufdef_mod.F 
Chd|        IMP_PCG_PROJ                  share/modules/impbufdef_mod.F 
Chd|        IMP_WORKI                     share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTERFACES_MOD                ../common_source/modules/interfaces/interfaces_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|        TH_SURF_MOD                   ../common_source/modules/interfaces/th_surf_mod.F
Chd|====================================================================
      SUBROUTINE IMP_SOLV(
     1  ICODE  ,ISKEW  ,ISKWN  ,IPART  ,IXTG   ,IXS    ,IXQ    ,
     2  IXC    ,IXT    ,IXP    ,IXR    ,IXTG1          ,ITAB   ,ITABM1 ,
     3  NPC    ,IBCL   ,IBFV   ,SENSOR_TAB,NNLINK ,LNLINK ,IPARG  ,IGRV   ,
     4  IPARI  ,INTBUF_TAB,NPRW   ,ICONX  ,NPBY   ,LPBY   ,LRIVET ,
     5  NSTRF  ,LJOINT ,ICODT  ,ICODR  ,ISKY   ,ADSKY  ,IADS_F ,
     6  ILINK  ,LLINK  ,WEIGHT         ,ITASK  ,IBVEL  ,LBVEL  ,FBVEL  ,
     7  X      ,D      ,V      ,VR     ,DR     ,THKE   ,DAMP   ,MS     ,
     8  IN     ,PM     ,SKEW   ,GEO    ,EANI   ,BUFMAT ,BUFGEO ,BUFSF  ,
     9  TF     ,FORC   ,VEL    ,FSAV   ,AGRV   ,FR_WAVE,PARTS0 ,
     A  ELBUF  ,RBY    ,RIVET  ,FR_ELEM,IAD_ELEM,
     B  WA             ,A      ,AR     ,STIFN  ,STIFR  ,PARTSAV,FSKY   ,
     C  FSKYI  ,IFRAME ,XFRAME ,W16    ,IACTIV ,FSKYM  ,IGEO   ,IPM    ,
     D  TFEXT  ,NODFT  ,NODLT  ,NINT7  ,NUM_IMP,NS_IMP ,NE_IMP ,IND_IMP,
     L  IT     ,RWBUF  ,LPRW   ,FR_WALL,NBINTC ,INTLIST,FOPT   ,RWSAV  ,
     M          FSAVD  ,GRAPHE ,FAC_K  ,IPIV_K ,NKCOND ,NSENSOR,
     N  MONVOL ,IGRSURF,FR_MV  ,VOLMON ,DIRUL  ,
     O  NODGLOB,MUMPS_PAR,CDDLP,ISENDTO,IRECVFROM,NEWFRONT,IMSCH  ,
     P  I2MSCH ,ISIZXV,ILENXV ,ISLEN7  ,IRLEN7 ,ISLEN11,IRLEN11,ISLEN17,
     Q  IRLEN17,IRLEN7T,ISLEN7T,KINET  ,NUM_IMP1,TEMP   ,DT2PREV,WAINT ,
     R  LGRAV   ,SH4TREE ,SH3TREE,IRLEN20,ISLEN20,IRLEN20T,ISLEN20T    ,
     S  IRLEN20E,ISLEN20E,IRBE3,LRBE3  ,FRBE3  ,FR_I2M,IAD_I2M,FR_RBE3M,
     T  IAD_RBE3M,FRWL6,IRBE2 ,LRBE2   ,INTBUF_TAB_C,IKINE  ,DIAG_SMS,
     V  ICFIELD,LCFIELD,CFIELD,COUNT_REMSLV,COUNT_REMSLVE,
     X  ELBUF_TAB,ELBUF_IMP,XDP,WEIGHT_MD , STACK ,
     Y  DIMFB  ,FBSAV6 ,STABSEN,TABSENSOR,DRAPE_SH4N, DRAPE_SH3N,
     Z  H3D_DATA,MULTI_FVM,IGRBRIC,IGRSH4N,IGRSH3N,IGRBEAM,FORNEQS,MAXDGAP,
     A  NDDL0   ,NNZK0 ,IT_T    ,IMPBUF_TAB,CPTREAC,FTHREAC,NODREAC, DRAPEG,
     B  INTERFACES,TH_SURF ,FSAVSURF,NSEG_LOADP,DPL0CLD,VEL0CLD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE DSGRAPH_MOD
        USE IMP_LINTF
        USE IMP_WORKI
        USE IMP_PCG_PROJ
        USE MESSAGE_MOD
        USE ELBUFDEF_MOD
        USE INTBUFDEF_MOD
        USE STACK_MOD
        USE H3D_MOD
        USE MULTI_FVM_MOD
        USE GROUPDEF_MOD
        USE DRAPE_MOD
        USE IMPBUFDEF_MOD
        USE SENSOR_MOD
        USE INTERFACES_MOD
        USE TH_SURF_MOD , ONLY : TH_SURF_
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "comlock.inc"
#if defined(MUMPS5)
#include      "dmumps_struc.h"
#endif
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "impl2_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr16_c.inc"
#include      "timeri_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ,INTENT(IN) :: NSENSOR
        INTEGER ITASK,ICODE(*), ISKEW(*), ISKWN(LISKN,*),ITABM1(*),
     .     IPART(*),IXS(*),IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*),
     .     IXP(NIXP,*),IXR(NIXR,*), IXTG(NIXTG,*), IXTG1(4,*),
     .     ITAB(*),NPC(*), IBCL(*), IBFV(*),IPARG(NPARG,*),IPARI(NPARI,*),
     .     NPRW(*), NPBY(NNPBY,*), LPBY(*),IADS_F(*),
     .     LRIVET(*), NSTRF(*), LJOINT(*), ICODT(*), ICODR(*), ILINK(*),
     .     LLINK(*),ISKY(*),ADSKY(*),
     .     NNLINK(10,*),LNLINK(*),IGRV(*),IKINE(*),
     .     WEIGHT(*),IFRAME(LISKN,*),IBVEL(NBVELP,*),LBVEL(*),
     .     IACTIV(*),IGEO(*),IPM(*),ICONX(*),NODFT  ,NODLT,IT,
     .     WEIGHT_MD(*),DIMFB,STABSEN,TABSENSOR(*),CPTREAC,NODREAC(*)
        INTEGER LPRW(*), FR_WALL(NSPMD+2,*),FR_ELEM(*),
     .     IAD_ELEM(2,*),NBINTC ,INTLIST(*), IPIV_K(*), NKCOND,
     .     NODGLOB(*), CDDLP(*),LGRAV(*)
        INTEGER NDDL0,NNZK0,IT_T,MONVOL(*),FR_MV(*),
     .          DIRUL(*),SH4TREE(*), SH3TREE(*),
     .          FR_I2M(*),IAD_I2M(*),FR_RBE3M(*),IAD_RBE3M(*),
     .          ICFIELD(*),LCFIELD(*),COUNT_REMSLV(*),COUNT_REMSLVE(*)
        my_real
     .     X(3,*)    ,D(3,*)      ,V(3,*)   ,VR(3,*),DAMP(*),
     .     MS(*)   ,IN(*)   ,PM(NPROPM,*),SKEW(LSKEW,*),GEO(NPROPG,*),
     .     BUFMAT(*) ,TF(*) ,FORC(*)  ,VEL(*),FSAV(NTHVKI,*) ,ELBUF(*) ,
     .     RWBUF(NRWLP,*),RWSAV(*),RBY(NRBY,*),
     .     RIVET(*),WA(*), A(3,*) ,AR(3,*),PARTSAV(*) ,TFEXT,
     .     STIFN(*) ,STIFR(*),FSKY(*),FSKYI(*),DR(3,*),
     .     EANI(*),AGRV(*), THKE(*),FR_WAVE(*),PARTS0(*),BUFGEO(*),
     .     XFRAME(NXFRAME,*),W16(*),FBVEL(*),FSKYM(*),BUFSF(*),
     .     FOPT(6,*),FSAVD(NTHVKI,*), FAC_K(*), DIAG_SMS(*),
     .     CFIELD(*),FORNEQS(*),MAXDGAP(NINTER),FTHREAC(6,*)
        INTEGER  NUM_IMP(*),NS_IMP(*),NE_IMP(*),IND_IMP(*),NINT7
        INTEGER NEWFRONT(*),ISENDTO(*),IRECVFROM(*),IMSCH  ,
     .          I2MSCH ,ISIZXV,ILENXV ,ISLEN7  ,IRLEN7 ,ISLEN11,IRLEN11,
     .          ISLEN17,IRLEN17,IRLEN7T,ISLEN7T,
     .          IRLEN20,ISLEN20,IRLEN20T,ISLEN20T,IRLEN20E,ISLEN20E,
     .          KINET(*),NUM_IMP1(*),IRBE3(*),LRBE3(*),IRBE2(*),LRBE2(*)
      my_real, INTENT(IN) :: 
     .        DPL0CLD(6,NCONLD),VEL0CLD(6,NCONLD)
C     REAL
        my_real
     .    DT2PREV,VOLMON(*) ,TEMP(*),
     .    WAINT(*),FRBE3(*)
        TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB, ELBUF_IMP
        DOUBLE PRECISION
     .          FRWL6(*), XDP(3,*)
        DOUBLE PRECISION
     .          FBSAV6(12,6,DIMFB)
C
        TYPE(PRGRAPH) :: GRAPHE(*)
C
#ifdef MUMPS5
      TYPE(DMUMPS_STRUC) MUMPS_PAR
#else
      ! Fake declaration as DMUMPS_STRUC is shipped with MUMPS
      INTEGER MUMPS_PAR 
#endif
        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*), INTBUF_TAB_C
        TYPE (STACK_PLY) :: STACK
        TYPE(H3D_DATABASE) :: H3D_DATA
        TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
!
        TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
        TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
        TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
        TYPE (GROUP_)  , DIMENSION(NGRBEAM) :: IGRBEAM
        TYPE (GROUP_)  , DIMENSION(NSURF)   :: IGRSURF
        TYPE (DRAPE_)   :: DRAPE_SH4N(NUMELC_DRAPE), DRAPE_SH3N(NUMELTG_DRAPE)
        TYPE (IMPBUF_STRUCT_) ,TARGET :: IMPBUF_TAB
        TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
        TYPE (DRAPEG_)   :: DRAPEG
        TYPE (INTERFACES_)    ,INTENT(IN)     :: INTERFACES
        TYPE (TH_SURF_) , INTENT(IN) :: TH_SURF
        my_real, INTENT(INOUT) :: FSAVSURF(5,NSURF)
        INTEGER, INTENT(INOUT) :: NSEG_LOADP(NSURF)
C
C---D_IMP: dUn+1,i,DD : ddU,i+1
C---R_IMP[1:R02,2:RRR(R_OLD),3:RU0,4:E02,5:DE_OLD,6:EIMP,7,8,9:pour line-search]
C---R_IMP[10: BFAC,DTFAC,11:U2,12:RFAC,13:|FEXT|;14,16:new line-search;17:actual R02;18:GAP;19:TSTART;
C---R_IMP[21:rel res disp.,22:rel res force,23:rel res energy,24:cumul arc length]
C---------20-25:libre
C---I_IMP[1:IT(TOTAL),2:ITC,3:IT0(IT_OLD),4:IWAIT,5:IDIV,6:NDDLI0,7:1ercontact(used only w/ IREFI=4),
C---8:num. of diverging, 9:ICONT_OLD 10:Isign (Riks);11: Ichang(solver);12: IDIV_OLD;13: NDDLI(SMP) or NDDLI_G(SPMD)
#if defined(MUMPS5)
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER  NNMAX,NKMAX,N_IMPN,N_IMPM,LNSS,LNSS2,NDT,NDS,NT_RW
        INTEGER I,J,NTMP,L1,L2,L3,NNDL,INPRINT,ISTOP,LI10,LI11,
     .          LI1,LI2,LI3,LI4,LI5,LI6,LI7,LI8,LI9,LIF,IC,ISETP,
     .          LI12,NDDL_INI0,LI13,LI14,LI15,LNSS3,LNSB2,LNSRB2
        INTEGER, DIMENSION(:),ALLOCATABLE :: IADI0,JDII0
C
        INTEGER, DIMENSION(:),ALLOCATABLE :: NSS,ISS,NSS2,ISS2,NSS3,ISS3
        INTEGER, DIMENSION(:),ALLOCATABLE :: NSB2,ISB2,IAINT2
C      ---INEGA is defined now in impl1_c.inc---
        INTEGER  NNOD,IFDIS,NODFTSK ,NODLTSK,N1,N2,N3
C
        INTEGER LBAND,NCL_MAX,IRFLAG,IPRINT0,IPRJ_S
C
        INTEGER IBID,IFIF,F_DDL,L_DDL,NSPC_OLD,NSPC,NFXV_G
C
        my_real
     .          RBID,EFAC,LBB(NDDL0),DUMMY_FEXT(3,1)
        my_real
     .    TFEXC,TMP,TMP1,TMP2,R2,BFAC,FACI,R02,GAP,BID,WE_IMP
        my_real,
     .           DIMENSION(:),ALLOCATABLE :: DIAG_I0,LT_I0
C
        INTEGER, POINTER     :: NDDL,NNZK,NRBYAC,NINT2,NMC,NMC2,NMONV
        INTEGER, DIMENSION(:) ,POINTER     :: IADK,JDIK,IADM,JDIM
        INTEGER, DIMENSION(:) ,POINTER     :: IDDL,NDOF,INLOC,LSIZE,I_IMP,IKC,
     .                                        IRBYAC,NSC,IINT2,NKUD,IMONV,
     .                                        IKINW,W_DDL,IKUDN,NDOFI,IDDLI,IKUD
        my_real, DIMENSION(:) ,POINTER     :: DIAG_K,LT_K,DIAG_M,LT_M,LB,
     .                                        LB0,BKUD,D_IMP,ELBUF_C,BUFMAT_C,
     .                                        DR_IMP,X_C,DD,DDR,X_A,R_IMP
        my_real, DIMENSION(:) ,POINTER     :: FEXT,DG,DGR,DG0,DGR0,BUFIN_C,AC,ACR
c sb
        character*1 anew_stif

C-----------------------------------------------
        anew_stif = ' '
        DUMMY_FEXT = ZERO
        RBID = ZERO
c
C-----------------------------------------------
C---IMCONV : 0 iteration; 1 converge; -1 line-search, -2 change dt during iteration-----
C---         -3 only reset iteration with Dn-1=0-----
C-----------------------------------------------
        NDDL => IMPBUF_TAB%NDDL
        NNZK => IMPBUF_TAB%NNZK
        NRBYAC => IMPBUF_TAB%NRBYAC
        NINT2 => IMPBUF_TAB%NINT2
        NMC => IMPBUF_TAB%NMC
        NMC2 => IMPBUF_TAB%NMC2
        NMONV => IMPBUF_TAB%NMONV
        IADK => IMPBUF_TAB%IADK
        JDIK => IMPBUF_TAB%JDIK
        IADM => IMPBUF_TAB%IADM
        JDIM => IMPBUF_TAB%JDIM
        IDDL => IMPBUF_TAB%IDDL
        NDOF => IMPBUF_TAB%NDOF
        INLOC => IMPBUF_TAB%INLOC
        LSIZE => IMPBUF_TAB%LSIZE
        I_IMP => IMPBUF_TAB%I_IMP
        IRBYAC => IMPBUF_TAB%IRBYAC
        NSC => IMPBUF_TAB%NSC
        IINT2 => IMPBUF_TAB%IINT2
        NKUD => IMPBUF_TAB%NKUD
        IMONV => IMPBUF_TAB%IMONV
        IKINW => IMPBUF_TAB%IKINW
        IKC => IMPBUF_TAB%IKC
        W_DDL => IMPBUF_TAB%W_DDL
        IKUD => IMPBUF_TAB%IKUD
        NDOFI=> IMPBUF_TAB%NDOFI
        IDDLI=> IMPBUF_TAB%IDDLI
C
        DIAG_K  =>IMPBUF_TAB%DIAG_K
        LT_K    =>IMPBUF_TAB%LT_K
        DIAG_M  =>IMPBUF_TAB%DIAG_M
        LT_M    =>IMPBUF_TAB%LT_M
        LB      =>IMPBUF_TAB%LB
        LB0     =>IMPBUF_TAB%LB0
        BKUD    =>IMPBUF_TAB%BKUD
        D_IMP   =>IMPBUF_TAB%D_IMP
        DR_IMP  =>IMPBUF_TAB%DR_IMP
        ELBUF_C =>IMPBUF_TAB%ELBUF_C
        BUFMAT_C=>IMPBUF_TAB%BUFMAT_C
        X_C     =>IMPBUF_TAB%X_C
        X_A     =>IMPBUF_TAB%X_A
        DD      =>IMPBUF_TAB%DD
        DDR     =>IMPBUF_TAB%DDR
        FEXT  =>IMPBUF_TAB%FEXT
        DG    =>IMPBUF_TAB%DG
        DGR   =>IMPBUF_TAB%DGR
        DG0   =>IMPBUF_TAB%DG0
        DGR0  =>IMPBUF_TAB%DGR0
c                 BUFIN_C=>IMPBUF_TAB%BUFIN_C
        AC=>IMPBUF_TAB%AC
        ACR=>IMPBUF_TAB%ACR
        R_IMP => IMPBUF_TAB%R_IMP
        ALLOCATE(IAINT2(NINT2))
C--------explicite iteration only-------------
C-------smp // first for IMP_GLOB_K, PCG solver, Nonlinear drivers,
        NDT=NEXP
        IF (I_IMP(4)>0) THEN
          CALL INTEGRATOR_HP(NDT  ,D_IMP ,DR_IMP,
     1                       X     ,V     ,VR    ,A     ,AR    )
C    /---------------/
          I_IMP(4)=I_IMP(4)-1
          IF (IMCONV==1) IMCONV=2
          RETURN
        ENDIF
C------------------------------
C        Initialisation
C------------------------------
        IPRINT0=0
        IF (ISPMD==0) THEN
          IF(NCYCLE==1.AND.IMCONV==1.AND.I_IMP(5)==0) IPRINT0=1
          IF (ILINE/=1) INPRINT=NPRINT
        ELSE
          INPRINT=0
        ENDIF
C
        IF (IRREF>0.AND.IMCONV==1.AND.ILINE/=1) THEN
          IRFLAG=IRREF
        ELSE
          IRFLAG=0
        ENDIF
C
        ISETP=ISETK
        NDDLI=0
        NDDLI_G=0
        IF (NINT7==0) THEN
          DO I=1,NUMNOD
            NDOFI(I)=0
          ENDDO
        ENDIF
        ISTOP=0
        IF (IMCONV==2) IMCONV=1
        NNDL = 3*NUMNOD
C
        NSREM=0
        NSL=0
        ICONTA = 0
C
        WE_IMP = TFEXT
        IF (IMCONV==1) THEN
          ITER_NL=0
        ELSE
          ITER_NL=IT+1
        END IF
        IF(NCYCLE==1.AND.IMCONV==1.AND.I_IMP(5)==0) THEN
          R_IMP(19)=TT-DT2
C---------for interface sorting
          IF(NINTER>0) CALL INI_BMINMA_IMP
        END IF
c
C-----------------------------
Citask0        IF (ITASK == 0) THEN
C-----------------------------
        IF (IMCONV==3) CALL CP_REAL_HP(NNDL,X_C,X)
        NFXV_G = NFXVEL
        IF (NSPMD>1) CALL SPMD_MAX_I(NFXV_G)
C
        IF (ILINTF>0) THEN
          ALLOCATE(XI_C(NNDL))
          IF (NCYCLE==1) THEN
            CALL CP_IMPBUF(
     .           1      ,ELBUF,ELBUF_C,BUFMAT ,BUFMAT_C,
     .                     FSAV  ,VOLMON  ,PARTSAV ,INTBUF_TAB       ,
     .                     INTBUF_TAB_C,IPARI   ,ISLEN7 ,IRLEN7   ,
     .                     ISLEN11,IRLEN11,ISLEN17 ,IRLEN17,IRLEN7T  ,
     .                     ISLEN7T,IRLEN20,ISLEN20 ,IRLEN20T,ISLEN20T,
     .           IRLEN20E,ISLEN20E,NEWFRONT,ELBUF_TAB,ELBUF_IMP,
     .           IPARG   )
            CALL CP_REAL_HP(NNDL,X,X_C)
            CALL IMP_SETB(A ,AR ,IDDL ,NDOF ,LB0  )
            CALL INI_KIF
            I_IMP(2)=LPRINT
            LPRINT = 0
            CALL CP_REAL_HP(NNDL,X,XI_C)
          ELSE
            CALL CP_IMPBUF(
     .           2     ,ELBUF,ELBUF_C,BUFMAT ,BUFMAT_C,
     .                     FSAV  ,VOLMON  ,PARTSAV ,INTBUF_TAB       ,
     .                     INTBUF_TAB_C,IPARI   ,ISLEN7 ,IRLEN7   ,
     .                     ISLEN11,IRLEN11,ISLEN17 ,IRLEN17,IRLEN7T  ,
     .                     ISLEN7T,IRLEN20,ISLEN20 ,IRLEN20T,ISLEN20T,
     .           IRLEN20E,ISLEN20E,NEWFRONT,ELBUF_TAB,ELBUF_IMP,
     .           IPARG   )
            CALL CP_REAL_HP(NNDL,X_C,X)
            CALL CP_REAL_HP(NNDL,X_C,XI_C)
            CALL INTEGRATOR1_HP(D_IMP ,XI_C )
            CALL IMP_B2A(A ,AR ,IDDL ,NDOF ,LB0  )
            IF (NCYCLE==ILINTF) THEN
              LPRINT = I_IMP(2)
            ELSE
              LPRINT = 0
            ENDIF
C----------otherwise, X_A is accumulated---
            CALL CP_REAL_HP(NNDL,X,X_A)
          ENDIF
        ENDIF
C
        IF (IMCONV==1 ) THEN
          R_IMP(16)=ZERO
C          R_IMP(6)=ZERO
C---------initialise D,DD-----
          IF (NCYCLE>1.AND.ILINE/=1) THEN
C-----------------Dn,0=Dn-1--------
c              CALL CP_REAL(NNDL,D_IMP,DD)
c              IF (IRODDL/=0) CALL CP_REAL(NNDL,DR_IMP,DDR)
C----------for the case where the run diverges due to the first contact(Gravity)->
C----------use reduced Dn_1 instead of resolution:
            CALL DU_INI_HP(D_IMP  ,DR_IMP,DD    ,
     1                     DDR   ,I_IMP(5),I_IMP(7))
          ENDIF
          CALL ZEROR_HP(D_IMP,NUMNOD)
          IF (IRODDL/=0) CALL ZEROR_HP(DR_IMP,NUMNOD)
C         permet aussi de faire linear avec 'initial state'
          CALL ZEROR_HP(AC,NUMNOD)
          IF (IRODDL/=0) CALL ZEROR_HP(ACR,NUMNOD)
C
          IF (ISIGINI==1.AND.NCYCLE==1) THEN
            CALL IMP_SETB(A ,AR ,IDDL ,NDOF ,LB0  )
          ENDIF
C
          IF (NCYCLE==1.AND.IDYNA>0)
     .       CALL DYNA_INA(IBCL  ,FORC   ,NPC   ,TF    ,A     ,
     2                     V     ,X      ,SKEW  ,AR    ,VR    ,
     3                     SENSOR_TAB,WEIGHT,TFEXC ,IADS_F ,
     4                     FSKY  ,IGRV   ,AGRV  ,MS    ,IN    ,
     5                     LGRAV ,ITASK ,NRBYAC,IRBYAC ,NPBY  ,
     6                     RBY   ,FR_ELEM,IAD_ELEM,NDDL0,NNZK0,
     7                     I_IMP(5),H3D_DATA,CPTREAC,FTHREAC,NODREAC,
     8                     NSENSOR,TH_SURF ,FSAVSURF, NSEG_LOADP,DPL0CLD,
     9                     VEL0CLD,   D,  DR,  NUMNOD,NSURF,
     A                     NFUNCT,NCONLD,NGRAV,NINVEL)
C
C----------------------------------
C       FORCES EXTERNES A=Fext-Fint
C----------------------------------
C
          NCL_MAX=0
          IF(NCONLD/=0) THEN
            IF (IMON>0) CALL STARTIME(4,1)
C         --en spmd force est traite comme elements---
            CALL FORCE_IMP(IBCL   ,FORC    ,NPC    ,TF    ,AC     ,
     2                 V      ,X       ,SKEW   ,ACR   ,VR     ,
     3                 NSENSOR,SENSOR_TAB,TFEXC,
     4                 IADS_F ,FSKY    ,FSKY   ,DUMMY_FEXT,H3D_DATA,
     5                 CPTREAC ,FTHREAC,NODREAC ,TH_SURF ,FSAVSURF,
     6                 NSEG_LOADP,DPL0CLD ,VEL0CLD ,D       ,DR      ,
     7                 NCONLD  ,NUMNOD    ,NSURF,NFUNCT)
C
            IF (IMACH==3.AND.NSPMD>1) THEN
              DO I=IAD_ELEM(1,1),IAD_ELEM(1,NSPMD+1)-1
                J = FR_ELEM(I)
                N1 = 3*(J-1)+1
                N2 = 3*(J-1)+2
                N3 = 3*(J-1)+3
                TMP = ABS(AC(N1))+ABS(AC(N2))+ABS(AC(N3))
                IF (IRODDL/=0) TMP = TMP +
     .                ABS(ACR(N1))+ABS(ACR(N2))+ABS(ACR(N3))
                IF (TMP>ZERO) NCL_MAX = NCL_MAX + 1
              ENDDO
            ENDIF
C
            IF (IMON>0) CALL STOPTIME(4,1)
          ENDIF
C
          IF (IMACH==3.AND.NSPMD>1) THEN
            CALL SPMD_MAX_I(NCL_MAX)
            IF (NCL_MAX>0) THEN
              LBAND = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
              IF (IRODDL/=0) THEN
                NTMP = 6
              ELSE
                NTMP = 3
              ENDIF
              CALL SPMD_SUMF_A(AC,ACR,IAD_ELEM,FR_ELEM,NTMP,LBAND)
            ENDIF
          ENDIF
C---------no //SMP for the moment, add it after----
          IF(NGRAV/=0) THEN
            IF (IMON>0) CALL STARTIME(4,1)
            CALL GRAVIT_IMP(IGRV  ,AGRV  ,NPC   ,TF    ,AC,
     2                      V     ,X     ,SKEW  ,MS,TFEXC,
     3                      NSENSOR,SENSOR_TAB,WEIGHT,
     4                      LGRAV ,ITASK,
     5                      NRBYAC,IRBYAC,NPBY  ,RBY    )
            IF (IMON>0) CALL STOPTIME(4,1)
          ENDIF
C---------no //SMP for the moment, add it after----
          IF(NLOADC/=0) THEN
            IF (IMON>0) CALL STARTIME(4,1)
            CALL CFIELD_IMP(ICFIELD  ,CFIELD,NPC   ,TF    ,AC,
     2                      V     ,X     ,XFRAME  ,MS,TFEXC,
     3                      NSENSOR,SENSOR_TAB,WEIGHT,IFRAME,
     4                      LCFIELD ,ITASK,
     5                      NRBYAC,IRBYAC,NPBY  ,RBY,ISKEW    )
            IF (IMON>0) CALL STOPTIME(4,1)
          ENDIF


          TFEXT = WE_IMP
C fin IF (IMCONV==1)
        ENDIF
C-------------dU_d---------------------------------
        IF(NFXVEL/=0.AND.(IMCONV==1.OR.IMCONV==3)) THEN
          IF (IMON>0) CALL STARTIME(4,1)
          CALL FV_IMP(IBFV  ,NPC    ,TF     ,VEL   ,SENSOR_TAB,
     1                D_IMP  ,DR_IMP ,IKC   ,IDDL  ,NSENSOR   ,
     2                SKEW  ,IFRAME ,XFRAME ,V     ,VR    ,
     3                X     ,DIRUL  ,NDOF   ,A     ,AR    )
          IF (IMON>0) CALL STOPTIME(4,1)
        ENDIF
C-------------RGWAL0_IMP peut suivre-ISETK=1, mais il faut changer-FV_IMP1
C-------------(IKC(I)=4->IMCONV=1 pour eviter le rebond-------
        NT_RW=0
        NRWDONE = 0
        IF (NRWALL>0) THEN
          IF (IMON>0) CALL STARTIME(4,1)
          DO I=1,NDDL0
            IF (IKC(I)==3.OR.IKC(I)==10) IKC(I)=0
          ENDDO
          IF (IMCONV==1) THEN
            DO I=1,NDDL0
              IF (IKC(I)==4.OR.IKC(I)==11) IKC(I)=0
            ENDDO
          ENDIF

          IF (ISMDISP>0.AND.ILINE==0) THEN
            CALL RGWAL0_IMP(
     1       X_A         ,D_IMP    ,V      ,RWBUF   ,LPRW    ,
     2       NPRW        ,MS       ,FSAV(1,NINTER+1),FR_WALL ,
     3       FOPT        ,RWSAV    ,WEIGHT ,FSAVD(1,NINTER+1),
     4       NT_RW       ,IDDL     ,IKC    ,IMCONV,NDOF,FRWL6,
     5       WEIGHT_MD   ,DIMFB    , FBSAV6,STABSEN,TABSENSOR)
          ELSE
            CALL RGWAL0_IMP(
     1       X           ,D_IMP    ,V      ,RWBUF   ,LPRW    ,
     2       NPRW        ,MS       ,FSAV(1,NINTER+1),FR_WALL ,
     3       FOPT        ,RWSAV    ,WEIGHT ,FSAVD(1,NINTER+1),
     4       NT_RW       ,IDDL     ,IKC    ,IMCONV,NDOF,FRWL6,
     5       WEIGHT_MD   ,DIMFB    , FBSAV6,STABSEN,TABSENSOR)
          ENDIF
C
          IF(NT_RW>0) THEN
            CALL FV_RW(IDDL   ,IKC   ,NDOF  ,D_IMP  ,V )
          ENDIF
          IF (IMON>0) CALL STOPTIME(4,1)
        ENDIF
C
        IFDIS=NT_RW+NFXV_G
        IF(IFDIS>0.AND.IMCONV==1) THEN
          IF (NCYCLE>1.AND.ILINE/=1)
C--------------Dn,0=Dn-1--------
     .    CALL FV_DD0(IDDL   ,IKC   ,NDOF  ,DD  ,DDR ,D_IMP)
          IF(NT_RW>0) THEN
            DO I=1,NDDL0
              IF (IKC(I)==3) IKC(I)=4
C
              IF (IKC(I)==10) IKC(I)=11
            ENDDO
          ENDIF
        ENDIF
C
        IRWALL = NT_RW
        IF (NSPMD>1) CALL SPMD_MAX_I(IRWALL)
C-------!!!should stop line-search if Rwall activates!!!
        IF(IRWALL>0.AND.IMCONV>=0) THEN
          IF(IMACH/=3.OR.ISPMD==0) THEN
            WRITE(IOUT,*)'  *--------- RIGID WALL IMPACT---------*'
            WRITE(ISTDO,*)'  *--------- RIGID WALL IMPACT---------*'
          ENDIF
          ISETK = 1
        ENDIF
C
C----------------------------------
C       LB=Fext ;
C----------------------------------
C---------
        CALL IMP_SETB(AC    ,ACR     ,IDDL   ,NDOF  ,LB    )
C-----------------------
citask0        END IF !(ITASK == 0) THEN
C----------------------
c      CALL MY_BARRIER
C---------------------
C-----------------------
        IF (ISOLV==5.OR.ISOLV==6.AND.IMCONV>=0) THEN
          IF (IDSC==0) THEN
C-----------one update per increment
            IF (IPUPD==0.AND.I_IMP(2)==0.AND.IT==0) THEN
              IDSC=MAX(IDSC,ISETK)
            ENDIF
C-----------NDDL could be changed by RWALL impact
            IF(IRWALL > 0 ) IDSC = 1
          ENDIF
        ELSE
          IDSC=MAX(IDSC,ISETK)
        END IF
C    /---------------/
c      CALL MY_BARRIER
C    /---------------/
C----------------------------------
C       MATRICE DE RIGIDITE
C----------------------------------
        IF (ISETK ==1 ) THEN
          IF (IMON>0 .AND. ITASK ==0) CALL STARTIME(31,1)
          L1 = 1+NIXS*NUMELS
          L2 = L1+6*NUMELS10
          L3 = L2+12*NUMELS20
C----------------------------------
Citask0         IF (ITASK == 0) THEN
C----------------------------------
          NDDL = NDDL0
          NNZK = NNZK0
          NNMAX=LSIZE(9)
          NKMAX=LSIZE(10)
          NMC2=LSIZE(11)
          CALL ZERO1(DIAG_K,NDDL)
          CALL ZERO1(LT_K,NNZK)
          LI1 =1
          LI2 = LI1+LSIZE(4)
          LI3 = LI2+LSIZE(5)
          LI4 = LI3+LSIZE(1)
          LI5 = LI4+LSIZE(3)
          LI6 = LI5+LSIZE(7)
          LI7 = LI6+LSIZE(2)
          LI8 = LI7+LSIZE(6)
          LI9 = LI8+NINT2
          LI10 = LI9+LSIZE(8)
C
          LI11 = LI10+(LSIZE(8)-LCOKM)*LSIZE(9)
          LI12 = LI11+LCOKM*LSIZE(10)
          LI13 = LI12+4*LSIZE(11)
          LI14 = LI13+LSIZE(14)
          LI15 = LI14+LSIZE(15)
          LIF = LI15+LSIZE(16)
C---------attention si rigid body reactive(deactive) pendant un restart il faut relancer aussi dim
          IF (ILINE/=1) THEN
            NTMP=0
            IF (I_IMP(11)==1) THEN
              NTMP=1
              I_IMP(11)=-1
            ENDIF
            CALL IND_GLOB_K(NPBY  ,LPBY      ,
     1      ITAB      ,NRBYAC    ,IRBYAC    ,NSC       ,IKINW(LI1),
     2      NMC       ,IKINW(LI2),IKINW(LI3),IKINW(LI4),NINT2     ,
     3      IINT2     ,IPARI     ,INTBUF_TAB,IKINW(LI8),IKINW(LI5),
     4      IKINW(LI6),IKINW(LI7),IPARG     ,ELBUF     ,ELBUF_TAB ,
     5      IXS       ,IXQ       ,IXC       ,IXT       ,IXP       ,
     6      IXR       ,IXTG      ,IXTG1     ,IXS(L1)   ,IXS(L2)   ,
     7      IXS(L3)   ,IDDL      ,NDOF      ,IADK      ,
     8      JDIK      ,NDDL      ,NNZK      ,NNMAX     ,LSIZE(8)  ,
     9      INLOC     ,NKMAX     ,IKINW(LI9),IKINW(LI10),IKINW(LI11),
     A      NMC2      ,IKINW(LI12),NTMP     ,LSIZE(12) ,LSIZE(13) ,
     B      FR_ELEM   ,IAD_ELEM  ,IPM       ,IGEO      ,IRBE3     ,
     C      LRBE3     ,IKINW(LI13),FR_I2M   ,IAD_I2M   ,FR_RBE3M  ,
     D      IAD_RBE3M ,IRBE2     ,LRBE2     ,IKINW(LI14),IKINW(LI15))
C-------------important is no buffer overflow
C           NDDL0 = NDDL
C           NNZK0 = NNZK
          ENDIF
C----------------------------------
citask0         END IF !IF (ITASK == 0) THEN
C----------------------------------
C    /---------------/
c      CALL MY_BARRIER
C    /---------------/
c          NGDONE = 1
          CALL IMP_GLOB_KHP(
     1     PM        ,GEO       ,IPM       ,IGEO      ,ELBUF     ,
     2     IXS       ,IXQ       ,IXC       ,IXT       ,IXP       ,
     3     IXR       ,IXTG      ,IXTG1     ,IXS(L1)   ,
     4     IXS(L2)   ,IXS(L3)   ,IPARG     ,TF        ,NPC       ,
     5     FR_WAVE   ,W16       ,BUFMAT    ,THKE      ,BUFGEO    ,
     6     RBY       ,SKEW      ,X         ,
     7     WA        ,IDDL      ,NDOF      ,DIAG_K    ,LT_K      ,
     8     IADK      ,JDIK      ,IKG       ,IBID      ,ITASK     ,
     9     ELBUF_TAB ,STACK     ,DRAPE_SH4N, DRAPE_SH3N   ,DRAPEG )
C
C    /---------------/
c      CALL MY_BARRIER
C    /---------------/
          NDDL_L = NDDL
C-----------------------------
citask0       IF (ITASK == 0) THEN
C-----------------------------
          IF (IDYNA>0.AND.IDY_DAMP>0) THEN
            CALL DYNA_CPK0(NDDL  ,NNZK  ,IADK  ,JDIK   ,DIAG_K ,
     .                     LT_K  )
          END IF
C-------estimation of A(t+dt) w/ initial velocity----
          IF (NCYCLE==1.AND.IMCONV==1.AND.I_IMP(5)==0
     .        .AND.IDYNA>0.AND.NINVEL>0) THEN
            CALL IMP_DYKV0(NODFT  ,NODLT   ,IDDL   ,NDOF   ,IKC    ,
     .                     DIAG_K ,IADK    ,JDIK   ,LT_K   ,WEIGHT ,
     1                     RBY    ,X       ,SKEW   ,LPBY   ,NPBY   ,
     2                     NRBYAC ,IRBYAC  ,NINT2  ,IINT2  ,IPARI  ,
     3                     INTBUF_TAB      ,IRBE3  ,LRBE3  ,FRBE3  ,
     4                     IRBE2  ,LRBE2   ,V      ,VR     ,NDDL0  ,
     5                     FR_ELEM,IAD_ELEM,MS     ,IN     )
          END IF
          IF (IDYNA>0.OR.IQSTAT>0)
     .     CALL IMP_DYNAM(NODFT  ,NODLT   ,IDDL   ,NDOF   ,DIAG_K ,
     .                    MS     ,IN      ,HHT_A  ,WEIGHT ,IADK   ,
     .                    LT_K   )
C
          IF (IKPRES>0.AND.NBUCK==0)
     1      CALL IMP_KPRES(IBCL  ,FORC   ,NPC   ,TF    ,X     ,
     2                     SKEW  ,NSENSOR,SENSOR_TAB,WEIGHT,IADS_F,
     3                     IDDL  ,NDOF   ,IADK  ,JDIK  ,DIAG_K,
     4                     LT_K  )
          IF(IAUTSPC>0) THEN
            IF(NCYCLE==1.AND.IMCONV==1.AND.I_IMP(5)==0) THEN
            ELSE
              CALL GET_NSPC(NSPC_OLD)
              IF (NSPMD > 1) CALL SPMD_MAX_I(NSPC_OLD)
            END IF
          ENDIF
          CALL UPD_GLOB_K(
     1    ICODT     ,ICODR     ,ISKEW     ,IBFV      ,NPC       ,
     2    TF        ,VEL       ,XFRAME    ,
     3    RBY       ,X         ,SKEW      ,LPBY      ,NPBY      ,
     4    ITAB      ,WEIGHT    ,MS        ,IN        ,NRBYAC    ,
     5    IRBYAC    ,NSC       ,IKINW(LI1),NMC       ,IKINW(LI2),
     6    IKINW(LI3),IKINW(LI4),NINT2     ,IINT2     ,IKINW(LI8),
     7    IKINW(LI5),IKINW(LI6),IKINW(LI7),IPARI     ,INTBUF_TAB,
     8    NDDL      ,NNZK      ,IADK      ,JDIK      ,
     9    DIAG_K    ,LT_K      ,NDOF      ,IDDL      ,IKC       ,
     A    D_IMP     ,LB        ,NKUD      ,IKUD      ,BKUD      ,
     B    NMC2      ,IKINW(LI12),NT_RW    ,DR_IMP    ,DIRUL     ,
     C    IRBE3     ,LRBE3     ,FRBE3     ,IKINW(LI13),IRBE2    ,
     D    LRBE2     ,IKINW(LI14),IKINW(LI15))
C
          anew_stif = 'Y'
c
          IF (IMACH==3.AND.NSPMD>1) THEN
            CALL UPD_FR_K(
     1      IADK     ,JDIK     ,NDOF      ,IKC      ,IDDL     ,
     2      INLOC    ,FR_ELEM  ,IAD_ELEM  ,NDDL     )
C
            CALL WEIGHTDDL(IDDL  ,NDOF  ,IKC  ,WEIGHT ,W_DDL  ,INLOC )
          ENDIF
C--------case autospc---------
          IF(IAUTSPC>0) THEN
            IF(NCYCLE==1.AND.IMCONV==1.AND.I_IMP(5)==0) THEN
            ELSE
              CALL GET_NSPC(NSPC)
              IF (NSPMD > 1) CALL SPMD_MAX_I(NSPC)
              IF (NSPC/=NSPC_OLD) THEN
                IMCONV=-2
                IF (ISPMD==0) THEN
                  WRITE(IOUT,1012)NSPC_OLD,NSPC
                  WRITE(ISTDO,1012)NSPC_OLD,NSPC
                ENDIF
                CALL PUT_NSPC(NSPC_OLD)
              ENDIF
            END IF
          ENDIF
C
          IF (N_PAT>1) THEN
            CALL FIL_SPAN1(NRBYAC,IRBYAC,NPBY,IDDL,NDDL,IKC,NDOF,INLOC)
          ENDIF
C
          IF(NCYCLE==1.AND.IMCONV==1.AND.I_IMP(5)==0) THEN
C--------case mono-domain, Multi-domain NDDL_G is set inside PR_INFOK
            NDDL_G = NDDL
            CALL PR_INFOK(NDDL0,NNZK0,NDDL,NNZK,MAX(NNMAX,NKMAX))
C
            IF (IPREC>4) THEN
              CALL K_BAND(NDDL,IADK,JDIK,IBID)
              MAXB = MIN(MAXB,IBID)
              IF (MAXB>10000) THEN
                CALL M_LNZ(NDDL,IADK,JDIK,MAXB,MAX_L)
              ENDIF
            ENDIF
C
            NTMP = (TSTOP-TT)/DT2
            IF (NTMP>=2) THEN
              IDSGAP = 1
            ELSE
              IDSGAP = 0
            ENDIF
C
            IF (ISOLV==7) THEN
              CALL CRIT_LLIM(NDDL,NNZK)
            END IF
C
            IF (NSPMD == 1) THEN
              DO I=1,NDDL
                W_DDL(I)=1
              ENDDO
            ENDIF
            IF (IMCONV/=-2)CALL INI_K0H(NDDL,NNZK,NNZK,IADK,JDIK)
C
          ENDIF
C
          IF (NINT7<=0.AND.IMCONV==1.AND.NSPMD==1)
     .     CALL IMP_CHECK(ITAB  ,NDDL  ,IDDL  ,DIAG_K  ,NDOF  ,
     .                    IKC   ,INLOC ,NDDL0 )
C
          IF (IMON>0) CALL STOPTIME(31,1)
!         IF(IMACH/=3.OR.ISPMD==0) THEN
c          IF (NCYCLE==1.AND.IMCONV==1)THEN
c           IF (ILINE/=1)THEN
c            WRITE(IOUT,1005)N_LIM
c            WRITE(ISTDO,1005)N_LIM
c            WRITE(IOUT,*)
c            WRITE(ISTDO,*)
c           ENDIF
c          ELSEIF(INPRINT/=0) THEN
c           WRITE(IOUT,1003)
c           IF (INPRINT<0)WRITE(ISTDO,1003)
c          ENDIF
!         END IF
C
          IF (ISOLV==4.OR.ISOLV==6) THEN
            CALL ARRET(5)
          ENDIF
C-----------------------------
citask0         END IF !(ITASK == 0) THEN
C-----------------------------
c      CALL MY_BARRIER
C     ---------------
          IF (IMCONV==-2.AND.ILINE==0) THEN
            IF (NINT7 > 0) NINT7=0
            GOTO 100
          END IF
        ENDIF !IF (ISETK ==1 )
C-----------------------------
citask0       IF (ITASK == 0) THEN
C-----------------------------
        IF (IQSTAT>0) THEN
          CALL QSTAT_INI(NDDL   ,INLOC  ,IDDL    ,NDOF   ,IKC    ,
     .                   MS     ,IN     )
        ENDIF
C----------------------------------
C       MATRICE DE RIGIDITE D'INTERFACE
C----------------------------------
        GAP=EP20
        IF (NINT7>0) THEN
          L1=LSIZE(1)
          L2=LSIZE(2)
          LNSS2=0
          LNSS=0
          IF (IMON>0) CALL STARTIME(31,1)
          CALL SAV_INTTD(NINT7,NUM_IMP,NS_IMP(1+NT_IMP5),
     1                   NE_IMP(1+NT_IMP5),IND_IMP,NUM_IMP1)
C
          IF (IMP_INT==1) CALL IDEL_INT(
     1    IPARI     ,INTBUF_TAB,NUM_IMP   ,NS_IMP    ,NE_IMP    ,
     2    IND_IMP   ,NDOF      ,NINT7     )
C
          CALL DIM_INT_K(
     1    IPARI     ,INTBUF_TAB,NUM_IMP   ,NS_IMP    ,NE_IMP    ,
     2    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     3    LNSS      ,NINT2     ,IINT2     ,IAINT2    ,LNSS2     ,
     4    NDDLI     ,NNZI      ,IDDLI     ,NDOFI     ,N_IMPN    ,
     5    N_IMPM    ,NNMAX     ,NKMAX     ,NDOF      ,NSREM     ,
     6    IRBE3     ,LRBE3     ,LNSS3     ,IRBE2     ,LRBE2     ,
     7    LNSB2     ,LNSRB2    ,IND_IMP  )
          ALLOCATE(IADI0(NDDLI+1))
          ALLOCATE(ITOK(NDDLI))
          ALLOCATE(JDII0(NNZI))
          ALLOCATE(NSS2(L2),NSS3(NRBE3),NSB2(LNSRB2))
          NSB2=0
          ALLOCATE(ISS2(LNSS2),ISS3(LNSS3),ISB2(LNSB2))
          ALLOCATE(NSS(L1))
          ALLOCATE(ISS(LNSS))
C
          DO I=1,L1
            NSS(I)=0
          ENDDO
C
          CALL IND_INT_K(
     1    IPARI     ,INTBUF_TAB,NUM_IMP   ,NS_IMP    ,NE_IMP      ,
     2    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC      ,
     3    NSS       ,ISS       ,NINT2     ,IINT2     ,NSS2        ,
     4    ISS2      ,NDDLI     ,NNZI      ,IADI0     ,JDII0       ,
     5    IDDLI     ,NDOFI     ,N_IMPN    ,ITOK      ,IDDL        ,
     6    NNMAX     ,NKMAX     ,N_IMPM    ,NDOF      ,IAINT2      ,
     7    IRBE3     ,LRBE3     ,NSS3      ,ISS3      ,IRBE2       ,
     8    LRBE2     ,NSB2      ,ISB2      ,IND_IMP  )
          ALLOCATE(DIAG_I0(NDDLI))
          ALLOCATE(LT_I0(NNZI))
          CALL ZERO1(DIAG_I0,NDDLI)
          CALL ZERO1(LT_I0,NNZI)
C
          IF (NSREM>0) THEN
            CALL IMP_FR7I(IPARI ,INTBUF_TAB,NUM_IMP ,NS_IMP ,NSREM ,
     1                     NBINTC,INTLIST)
            IF (INTP_C>0)
     1      CALL IND_FRKD(
     2      IPARI     ,INTBUF_TAB,NUM_IMP   ,NS_IMP    ,NE_IMP    ,
     3      IDDLI     ,IKC       ,NDOF      ,NSREM     ,IND_IMP  )
          ENDIF
C
          NDDL_L = NDDLI
Ctmp-------A n'est pas modifie ici -------------------
          IF (ILINTF>0) THEN
            CALL IMP_INT_K(A     ,V         ,
     1      ICODT     ,ICODR     ,ISKEW     ,IBFV      ,NPC       ,
     2      TF        ,VEL       ,NSENSOR   ,SENSOR_TAB,XFRAME    ,
     3      RBY       ,XI_C      ,SKEW      ,LPBY      ,NPBY      ,
     4      ITAB      ,WEIGHT    ,MS        ,IN        ,NRBYAC    ,
     5      IRBYAC    ,NSS       ,ISS       ,IPARI     ,INTBUF_TAB,
     6      NINT2     ,IINT2     ,IAINT2    ,NSS2      ,
     7      ISS2      ,NDDLI     ,NNZI      ,IADI0     ,JDII0     ,
     8      DIAG_I0   ,LT_I0     ,IDDLI     ,NDDL0     ,IADK      ,
     9      JDIK      ,IKC       ,DIAG_K    ,LT_K      ,IDDL      ,
     A      NUM_IMP   ,NS_IMP    ,NE_IMP    ,IND_IMP   ,NDOFI     ,
     B      ITOK      ,D_IMP     ,LB        ,GAP       ,DIRUL     ,
     C      NT_RW     ,NUM_IMP1  ,IRBE3     ,LRBE3     ,FRBE3     ,
     D      NSS3      ,ISS3      ,IRBE2     ,LRBE2     ,NSB2      ,
     E      ISB2      )
          ELSEIF (ISMDISP>0.AND.ILINE==0) THEN
            CALL IMP_INT_K(A     ,V         ,
     1      ICODT     ,ICODR     ,ISKEW     ,IBFV      ,NPC       ,
     2      TF        ,VEL       ,NSENSOR   ,SENSOR_TAB,XFRAME    ,
     3      RBY       ,X_A       ,SKEW      ,LPBY      ,NPBY      ,
     4      ITAB      ,WEIGHT    ,MS        ,IN        ,NRBYAC    ,
     5      IRBYAC    ,NSS       ,ISS       ,IPARI     ,INTBUF_TAB,
     6      NINT2     ,IINT2     ,IAINT2    ,NSS2      ,
     7      ISS2      ,NDDLI     ,NNZI      ,IADI0     ,JDII0     ,
     8      DIAG_I0   ,LT_I0     ,IDDLI     ,NDDL0     ,IADK      ,
     9      JDIK      ,IKC       ,DIAG_K    ,LT_K      ,IDDL      ,
     A      NUM_IMP   ,NS_IMP    ,NE_IMP    ,IND_IMP   ,NDOFI     ,
     B      ITOK      ,D_IMP     ,LB        ,GAP       ,DIRUL     ,
     C      NT_RW     ,NUM_IMP1  ,IRBE3     ,LRBE3     ,FRBE3     ,
     D      NSS3      ,ISS3      ,IRBE2     ,LRBE2     ,NSB2      ,
     E      ISB2      )
          ELSE
            CALL IMP_INT_K(A     ,V         ,
     1      ICODT     ,ICODR     ,ISKEW     ,IBFV      ,NPC       ,
     2      TF        ,VEL       ,NSENSOR   ,SENSOR_TAB,XFRAME    ,
     3      RBY       ,X         ,SKEW      ,LPBY      ,NPBY      ,
     4      ITAB      ,WEIGHT    ,MS        ,IN        ,NRBYAC    ,
     5      IRBYAC    ,NSS       ,ISS       ,IPARI     ,INTBUF_TAB,
     6      NINT2     ,IINT2     ,IAINT2    ,NSS2      ,
     7      ISS2      ,NDDLI     ,NNZI      ,IADI0     ,JDII0     ,
     8      DIAG_I0    ,LT_I0    ,IDDLI     ,NDDL0     ,IADK      ,
     9      JDIK      ,IKC       ,DIAG_K    ,LT_K      ,IDDL      ,
     A      NUM_IMP   ,NS_IMP    ,NE_IMP    ,IND_IMP   ,NDOFI     ,
     B      ITOK      ,D_IMP     ,LB        ,GAP       ,DIRUL     ,
     C      NT_RW     ,NUM_IMP1  ,IRBE3     ,LRBE3     ,FRBE3     ,
     D      NSS3      ,ISS3      ,IRBE2     ,LRBE2     ,NSB2      ,
     E      ISB2      )
          ENDIF
          IF (IMON>0) CALL STOPTIME(31,1)
C
          DEALLOCATE(NSS2,NSS3,NSB2)
          DEALLOCATE(ISS2,ISS3,ISB2)
          DEALLOCATE(NSS)
          DEALLOCATE(ISS)
C
          IF (NDDLI>0) THEN
C
            IFIF = 0
            IF (ILINTF>0) THEN
              IFIF = NDDLIF
              CALL SAVE_KIF(NDDLI ,IADI0 ,JDII0 ,DIAG_I0,LT_I0   ,
     1                      ITOK  ,NDDL)
            ENDIF
            IF (IFIF>0) THEN
              NDDLI = NDDLIF
              ALLOCATE(IADI(NDDLI+1))
              NNZI = IADIF(NDDLI+1)-IADIF(1)
              ALLOCATE(JDII(NNZI))
              DEALLOCATE(ITOK)
              ALLOCATE(ITOK(NDDLI))
              CALL CP_INT_HP(NDDLI+1,IADIF,IADI)
              CALL CP_INT_HP(NDDLI,IFTOK,ITOK)
              CALL CP_INT_HP(NNZI,JDIIF,JDII)
              ALLOCATE(DIAG_I(NDDLI))
              ALLOCATE(LT_I(NNZI))
              CALL CP_REAL_HP(NDDLI,DIAG_IF,DIAG_I)
              CALL CP_REAL_HP(NNZI,LT_IF,LT_I)
            ELSE
C
              ALLOCATE(IADI(NDDLI+1))
              ALLOCATE(JDII(NNZI))
              CALL CP_INT_HP(NDDLI+1,IADI0,IADI)
              CALL CP_INT_HP(NNZI,JDII0,JDII)
              ALLOCATE(DIAG_I(NDDLI))
              ALLOCATE(LT_I(NNZI))
              CALL CP_REAL_HP(NDDLI,DIAG_I0,DIAG_I)
              CALL CP_REAL_HP(NNZI,LT_I0,LT_I)
C
            ENDIF
            DEALLOCATE(IADI0)
            DEALLOCATE(JDII0)
            DEALLOCATE(DIAG_I0)
            DEALLOCATE(LT_I0)
C
            IF (ISOLV==4.OR.ISOLV==6) THEN
              CALL ARRET(5)
            ENDIF
C
          ELSE
            ALLOCATE(IADI(1))
            ALLOCATE(JDII(1))
            DEALLOCATE(IADI0)
            DEALLOCATE(JDII0)
            ALLOCATE(DIAG_I(1))
            ALLOCATE(LT_I(1))
            DEALLOCATE(DIAG_I0)
            DEALLOCATE(LT_I0)
          ENDIF
C
C         Store the size of the contact stifness matrix for nonlinear solver outputs
          IF ((NSPMD==1.OR.NBINTC==0).AND.IMCONV>=0) I_IMP(13) = NDDLI
          IF ((NSPMD==1.OR.NBINTC==0).AND.IMCONV>=0.AND.
     .         (LPRINT/=0.OR.NPRINT/=0)) THEN
            WRITE(IOUT,1006)
            WRITE(ISTDO,1006)
            WRITE(IOUT,1007)NDDLI,NNZI !,NNMAX
            WRITE(ISTDO,1007)NDDLI,NNZI !,NNMAX
c            WRITE(IOUT,*)
c            WRITE(ISTDO,*)
          ENDIF
        ENDIF
C----------------------------------
        IF (NFXVEL/=0.AND.IMCONV==1) THEN
          CALL FV_IMP1(NKUD   ,IKUD    ,BKUD    ,LB    )
          CALL FVBC_IMPL1(IBFV   ,SKEW  ,XFRAME ,DIRUL ,IDDL   ,
     1                    IKC    ,NDOF  ,D_IMP  ,DR_IMP,ICODT  ,
     3                    ICODR  ,ISKEW )
        ENDIF
C-------------initialization of Fext---for Riks-->approximation for follower load
C         IF (NCYCLE==1.AND.IDTC==3.AND.IMCONV==1.AND.
        IF (IDTC==3.AND.IMCONV==1.AND.
     .      I_IMP(5)==0) THEN
          CALL GET_FEXT(NDDL0 ,NDDL   ,IDDL   ,NDOF   ,IKC   ,
     1                  INLOC ,LB     ,FEXT   ,AC     ,ACR   )
          R_IMP(13) = TSTOP-TT+DT2
C             R_IMP(13) = SQRT(R2)
        END IF
        IF (IDYNA>0.AND.IDY_DAMP>0) THEN
          CALL IMP_DYKV(NODFT  ,NODLT   ,IDDL   ,NDOF   ,IKC    ,
     .                  DIAG_K ,IADK    ,JDIK   ,LT_K   ,WEIGHT ,
     1                  RBY    ,X       ,SKEW   ,LPBY   ,NPBY   ,
     2                  NRBYAC ,IRBYAC  ,NINT2  ,IINT2  ,IPARI  ,
     3                  INTBUF_TAB      ,IRBE3  ,LRBE3  ,FRBE3  ,
     4                  IRBE2  ,LRBE2   ,V      ,VR     ,NDDL0  ,
     5                  FR_ELEM,IAD_ELEM,MS     ,IN     )
        END IF
C-------------LB,A,AR devient Fext-Fint---------------------

        CALL UPD_RHS(ICODT ,ICODR ,ISKEW ,IBFV    ,XFRAME ,
     1              RBY   ,X     ,SKEW   ,LPBY   ,NPBY   ,
     2              NRBYAC,IRBYAC,NINT2  ,IINT2  ,IPARI  ,
     3              INTBUF_TAB   ,NDOF   ,IDDL   ,IKC    ,
     4              NDDL0 ,LB    ,ISETK  ,INLOC  ,DIRUL  ,
     5              A     ,AR    ,AC     ,ACR    ,NT_RW  ,
     6              IRFLAG,W_DDL ,NDDL   ,R_IMP(1),IDYNA ,
     7              V     ,VR    ,MS     ,IN     ,IRBE3  ,
     8              LRBE3 ,FRBE3 ,WEIGHT ,IRBE2  ,LRBE2  )
C
        IF (IMACH==3.AND.NSPMD>1) THEN
          ICONTA = NDDLI + NSREM
          CALL SPMD_MAX_I(ICONTA)
          IF (NBINTC>0.) THEN
            CALL SPMD_MIN_S(GAP)
            IF (ICONTA> 0.AND.GAP>ZERO) THEN
C
              CALL SPMD_MAX_I(IFDIS)
              IF (ILINTF>0) THEN
                CALL IMP_INTFR(
     1          NUM_IMP   ,NS_IMP    ,NE_IMP    ,IPARI     ,INTBUF_TAB,
     2          NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,
     3          IRBYAC    ,NINT2     ,IINT2     ,IDDL      ,IKC       ,
     4          NDOF      ,INLOC     ,NSREM     ,NSL       ,NBINTC    ,
     5          INTLIST   ,XI_C      ,IBFV      ,DIRUL     ,SKEW      ,
     6          XFRAME    ,ISKEW     ,ICODT     ,R_IMP(16) ,D_IMP     ,
     7          LB        ,IFDIS     ,NDDL      ,DR_IMP    ,IDDLI     ,
     8          IRBE3     ,LRBE3     ,FRBE3     ,IRBE2     ,LRBE2     ,
     9          DD        ,DDR       ,A         ,AR        ,AC        ,
     A          ACR       ,MS        ,V         ,NDDL0     ,R_IMP(1)  ,
     B          RBY       ,ICODR     ,NT_RW     ,W_DDL     ,WEIGHT    ,
     C          IRFLAG    )
              ELSEIF (ISMDISP>0.AND.ILINE==0) THEN
                CALL IMP_INTFR(
     1          NUM_IMP   ,NS_IMP    ,NE_IMP    ,IPARI     ,INTBUF_TAB,
     2          NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,
     3          IRBYAC    ,NINT2     ,IINT2     ,IDDL      ,IKC       ,
     4          NDOF      ,INLOC     ,NSREM     ,NSL       ,NBINTC    ,
     5          INTLIST   ,X_A       ,IBFV      ,DIRUL     ,SKEW      ,
     6          XFRAME    ,ISKEW     ,ICODT     ,R_IMP(16) ,D_IMP     ,
     7          LB        ,IFDIS     ,NDDL      ,DR_IMP    ,IDDLI     ,
     8          IRBE3     ,LRBE3     ,FRBE3     ,IRBE2     ,LRBE2     ,
     9          DD        ,DDR       ,A         ,AR        ,AC        ,
     A          ACR       ,MS        ,V         ,NDDL0     ,R_IMP(1)  ,
     B          RBY       ,ICODR     ,NT_RW     ,W_DDL     ,WEIGHT    ,
     C          IRFLAG    )
              ELSE
                CALL IMP_INTFR(
     1          NUM_IMP   ,NS_IMP    ,NE_IMP    ,IPARI     ,INTBUF_TAB,
     2          NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,
     3          IRBYAC    ,NINT2     ,IINT2     ,IDDL      ,IKC       ,
     4          NDOF      ,INLOC     ,NSREM     ,NSL       ,NBINTC    ,
     5          INTLIST   ,X         ,IBFV      ,DIRUL     ,SKEW      ,
     6          XFRAME    ,ISKEW     ,ICODT     ,R_IMP(16) ,D_IMP     ,
     7          LB        ,IFDIS     ,NDDL      ,DR_IMP    ,IDDLI     ,
     8          IRBE3     ,LRBE3     ,FRBE3     ,IRBE2     ,LRBE2     ,
     9          DD        ,DDR       ,A         ,AR        ,AC        ,
     A          ACR       ,MS        ,V         ,NDDL0     ,R_IMP(1)  ,
     B          RBY       ,ICODR     ,NT_RW     ,W_DDL     ,WEIGHT    ,
     C          IRFLAG    )
              END IF !(ILINTF>0) THEN
C
              CALL GETNDDLI_G(
     1        NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     2        NINT2     ,IINT2     ,IPARI     ,INTBUF_TAB,NDOFI     ,
     3        NDOF      ,IKC       ,IDDL      ,FR_ELEM   ,IAD_ELEM  ,
     4        NDDLI     ,NSL       ,NDDLI_G   ,IRBE3     ,LRBE3     ,
     5        IRBE2     ,LRBE2     )
C         Store the size of the contact stifness matrix for nonlinear solver outputs
              IF (ISPMD==0.AND.IMCONV>=0) I_IMP(13) = NDDLI_G
              IF (ISPMD==0.AND.IMCONV>=0.AND.
     .           (LPRINT/=0.OR.NPRINT/=0)) THEN
                WRITE(IOUT,1006)
                WRITE(ISTDO,1006)
                WRITE(IOUT,1011)NDDLI_G
                WRITE(ISTDO,1011)NDDLI_G
                WRITE(IOUT,*)
                WRITE(ISTDO,*)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
C
        IF (INTP_C<0) THEN
          CALL KIN_KNL(
     1    IPARI     ,INTBUF_TAB,NUM_IMP   ,NS_IMP    ,NE_IMP    ,
     2    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     3    NINT2     ,IINT2     ,IBFV      ,DIRUL     ,ISKEW     ,
     6    ICODT     ,NDOFI     ,IDDL      ,IKC       ,NDOF      ,
     5    INLOC     ,IRBE3     ,LRBE3     ,FRBE3     ,X         ,
     6    SKEW      ,IRBE2     ,LRBE2)
        ENDIF
C
        IF (NMONV>0.AND.ISETK==1) CALL MONV_IMP(
     .   MONVOL ,VOLMON  ,X      ,IGRSURF  ,
     1   NMONV  ,IMONV   ,IPARI  ,INTBUF_TAB      ,
     2   A      ,AR      ,NDOF   ,IDDL   ,IKC     ,
     3   INLOC  ,ILINE   ,IBFV   ,SKEW   ,XFRAME  ,
     4   DIRUL  ,ISKEW   ,ICODT  ,IRBE3   ,LRBE3  ,
     5   FRBE3  ,IRBE2   ,LRBE2  ,NSURF)
C
        IF (GAP<ZERO) THEN
          IMCONV = -2
          IF (IMACH/=3.OR.ISPMD==0) THEN
            WRITE(IOUT,1009)INT(-GAP)
            WRITE(ISTDO,1009)INT(-GAP)
          ENDIF
        ENDIF
C------------
        IF (ISPRB==1.AND.IMCONV==1) THEN
          DO I=1,NDDL
            LB0(I) = LB(I)
          ENDDO
        ENDIF
C
        IF (ISIGINI==1.AND.NCYCLE==1.AND.IMCONV==1) THEN
          CALL CONDENS_B(NDDL0  ,IKC  ,LB0  )
        ENDIF
C---------for mono
        NDDLI_G=MAX(NDDLI_G,NDDLI)
        ICONTA = MAX(ICONTA,NDDLI_G)
        IF (ICONTA>0) THEN
          IF (ISOLV<5) IDSC = 1
        ENDIF
C
        IF (ILINTF>2.AND.NCYCLE<ILINTF) THEN
          NSREM = 0
          NSL = 0
        ENDIF
C
        IF (ILINTF>0.AND.NDDLI==0) THEN
          IF (NDDLIF>0) THEN
            NDDLI = NDDLIF
            IF (ALLOCATED(IADI)) DEALLOCATE(IADI)
            ALLOCATE(IADI(NDDLI+1))
            NNZI = IADIF(NDDLI+1)-IADIF(1)
            IF (ALLOCATED(JDII)) DEALLOCATE(JDII)
            ALLOCATE(JDII(NNZI))
            IF (ALLOCATED(ITOK)) DEALLOCATE(ITOK)
            ALLOCATE(ITOK(NDDLI))
            CALL CP_INT_HP(NDDLI,IFTOK,ITOK)
            CALL CP_INT_HP(NDDLI+1,IADIF,IADI)
            CALL CP_INT_HP(NNZI,JDIIF,JDII)
            IF (ALLOCATED(DIAG_I)) DEALLOCATE(DIAG_I)
            ALLOCATE(DIAG_I(NDDLI))
            IF (ALLOCATED(LT_I)) DEALLOCATE(LT_I)
            ALLOCATE(LT_I(NNZI))
            CALL CP_REAL_HP(NDDLI,DIAG_IF,DIAG_I)
            CALL CP_REAL_HP(NNZI,LT_IF,LT_I)
          ENDIF
        ENDIF
C
        R_IMP(18)=GAP
C
        IF (IQSTAT>0.AND.ILINTF>0.AND.ILINTF==NCYCLE)
     .     CALL IMP_QIFAM(NODFT  ,NODLT   ,IDDL   ,NDOF   ,INLOC ,
     .                    IKC    ,DIAG_K  ,MS     ,IN     ,WEIGHT)
C
C        WRITE(6,*) IMUMPSV,IDSC,IMCONV
#if defined(MUMPS5)
        IF (IMUMPSV >0 .AND.IDSC==1.AND.IMCONV>=0)
     .    CALL IMP_MUMPS1(NDDL0,   NNZK0,     NDDL,   NNZK,  NNMAX,
     .                    NODGLOB, IDDL,      NDOF,   INLOC, IKC,
     .                    IADK,    JDIK,      DIAG_K, LT_K,  IAD_ELEM,
     .                    FR_ELEM, MUMPS_PAR, CDDLP,  IADI,  JDII,
     .                    ITOK,    DIAG_I,    LT_I,   NDDLI, NNZI    ,
     .                    IPRINT0, IT )
#else
        WRITE(6,*) "Fatal error: MUMPS is required"
        CALL FLUSH(6)
        CALL ARRET(5)
#endif
        CALL CP_REAL_HP(NDDL,LB,LBB)
C--------PCG w/ Projection----
        IF(NCYCLE==1.AND.IMCONV==1.AND.I_IMP(5)==0) THEN
          IF (M_VS> 0) THEN
C-------------Case smll model---
            NPCGPV=NDDL
            IF (NSPMD>1)CALL SPMD_MIN_I(NPCGPV)
            M_VS=MIN(M_VS,NPCGPV)
            IF (M_VS> 0) NPCGPV=-1
          END IF
C---------for free rigi motion for springback(generalization later)
          IF(IRIG_M>0) THEN
            CALL SPBRM_PRE(ITAB ,
     1           X         ,IPARG     ,IXC       ,IXTG      ,PARTSAV   ,
     2           ELBUF_TAB ,PM        ,NDOF      ,IDDL      ,IKC       )
          END IF
        END IF
C-----------------------------
        IF (ICONTA>0) ISETP = 1
C----------------------------------
C       IMPLICIT RESOLVE
C----------------------------------
 100    CONTINUE
C
        IF (ILINE==1) THEN
          IF (NCYCLE==1.AND.ISPMD==0.AND.ITASK==0) THEN
            IF (IQSTAT>0) THEN
              WRITE(IOUT,*)
              WRITE(IOUT,*)' ** BEGIN LINEAR QUASI-STATIC IMPLICIT COMPUTATION **'
              WRITE(ISTDO,*)
              WRITE(ISTDO,*)' ** BEGIN LINEAR QUASI-STATIC IMPLICIT COMPUTATION **'
              WRITE(IOUT,*)
              WRITE(ISTDO,*)
            ELSE
              WRITE(IOUT,*)
              WRITE(IOUT,*)' ** BEGIN LINEAR STATIC IMPLICIT COMPUTATION **'
              WRITE(ISTDO,*)
              WRITE(ISTDO,*)' ** BEGIN LINEAR STATIC IMPLICIT COMPUTATION **'
              WRITE(IOUT,*)
              WRITE(ISTDO,*)
            END IF !(IQSTAT>0) THEN
          ENDIF
C
          NTMP=0
C
c          R2=ZERO
          CALL PRODUT_HP(NDDL,LB,LB,W_DDL,R2)
C
          IF (R2>ZERO.AND.R2<EP30) THEN
          ELSEIF(IQSTAT==0.AND.ITASK==0.AND.NDDL>0) THEN
            CALL IMP_STOP(0)
          ENDIF
C
          CALL LIN_SOLV(NDDL  ,IDDL  ,NDOF   ,IKC   ,D_IMP ,
     1                 DR_IMP,L_TOL ,NNZK   ,IADK  ,JDIK  ,
     2                 DIAG_K,LT_K   ,NDDLI  ,IADI  ,JDII  ,
     3                 DIAG_I,LT_I   ,ITOK   ,IADM  ,JDIM  ,
     4                 DIAG_M,LT_M   ,LB    ,R_IMP(6),INLOC ,
     5                 FR_ELEM,IAD_ELEM,W_DDL,ITASK ,ISETP  ,
     6                 ISTOP  ,A     ,AR     ,V    ,
     7                 MS    ,X     ,IPARI ,INTBUF_TAB   ,
     8                 NUM_IMP,NS_IMP,NE_IMP,NSREM ,NSL  ,
     9                 NTMP  ,GRAPHE, ITAB  ,RBID  ,IBID ,
     A                 IBID  ,NMONV ,IMONV  ,MONVOL,IGRSURF,
     B                 FR_MV ,VOLMON,IBFV  ,SKEW  ,
     C                 XFRAME,MUMPS_PAR,CDDLP,IND_IMP,XI_C,
     D                 IRBE3 ,LRBE3 ,IRBE2  ,LRBE2 )

C-----------------------------
citask0        IF (ITASK == 0) THEN
C-----------------------------
          IF (INEGA>0) THEN
            CALL IDDL2NOD(NDDL  ,IDDL  ,NDOF  ,IKC   ,INLOC ,
     .                    INEGA ,NNOD  )
            IF (NNOD>0) THEN
              WRITE(IOUT,1008)ITAB(NNOD)
              WRITE(ISTDO,1008)ITAB(NNOD)
            ENDIF
C
          ELSEIF(IPREC>1.AND.ISOLV<=2) THEN
            CALL IMP_CHECM(ITAB  ,NDDL  ,IDDL  ,DIAG_M  ,NDOF  ,
     .                     IKC   ,INLOC ,NDDL0 )
C
          ENDIF
          IF(NFXV_G/=0.AND.(NSREM+NSL-INTP_C)>0) THEN
            CALL FV_IMP(IBFV  ,NPC    ,TF     ,VEL   ,SENSOR_TAB,
     1                  D_IMP  ,DR_IMP ,IKC   ,IDDL  ,NSENSOR   ,
     2                  SKEW  ,IFRAME ,XFRAME ,V     ,VR    ,
     3                  X     ,DIRUL  ,NDOF   ,A     ,AR    )
          ENDIF
          CALL RECUKIN(RBY   ,LPBY  ,NPBY  ,SKEW  ,ISKEW ,
     1                 ITAB  ,WEIGHT,MS    ,IN    ,
     2                 IBFV  ,VEL   ,ICODT,ICODR ,
     3                 NRBYAC,IRBYAC,NINT2 ,IINT2 ,IPARI ,
     4                 INTBUF_TAB   ,NDOF  ,D_IMP ,DR_IMP,
     5                 X     ,XFRAME,DIRUL ,IXR   ,IXC   ,
     6                 IXTG  ,SH4TREE,SH3TREE,IRBE3 ,LRBE3,
     7                 FRBE3 ,IRBE2 ,LRBE2 )
          IF (ISTOP>0) CALL IMP_STOP(-1)
          CALL INTEGRATORL_HP(D_IMP ,DR_IMP,
     1                        X    ,V    ,VR    ,A     ,AR    )
C
c         IF (IMPDEB>0) THEN
c            CALL PR_DEB(NDDL  ,IDDL  ,NDOF   ,IKC   ,ITAB   ,
c     1                  DIAG_K,DIAG_M,INLOC  ,FR_ELEM,IAD_ELEM,
c     2                  IADK  ,JDIK  ,LT_K   ,LT_M   ,NDDLI  ,
c     3                  IADI  ,JDII  ,ITOK   ,DIAG_I ,LT_I   ,
c     4                  LBB   ,LBB   ,0      ,NSREM  ,NSL    ,
c     5                  D_IMP ,DR_IMP,1      ,W_DDL  ,AC     ,
c     6                  ACR   ,A     ,AR     ,R2     ,0  ,NODGLOB)
c         END IF
          IF (ILINTF>0.AND.NCYCLE<ILINTF) THEN
            CALL IMP_INTTD0(
     1         IPARI ,INTBUF_TAB    ,X_A     ,D       ,
     2         MS    ,ITAB   ,IN    ,D_IMP   ,DR_IMP  ,
     3         IMSCH ,I2MSCH ,ISIZXV,ILENXV  ,IGRBRIC ,
     4         ISLEN7,IRLEN7 ,ISLEN11,IRLEN11,ISLEN17 ,
     5         IRLEN17,IRLEN7T,ISLEN7T,IAD_ELEM,FR_ELEM ,
     6         NBINTC,INTLIST,ITASK  ,KINET   ,NEWFRONT,
     7         NUM_IMP,NS_IMP,NE_IMP,IND_IMP ,ISENDTO ,
     8         IRECVFROM,WEIGHT ,IXS   ,TEMP  ,
     9         DT2PREV,WAINT ,NUM_IMP1,IRLEN20,ISLEN20,
     A         IRLEN20T,ISLEN20T,IRLEN20E,ISLEN20E,
     B         IKINE,DIAG_SMS,COUNT_REMSLV,COUNT_REMSLVE,
     C         NSENSOR,SENSOR_TAB,XDP,H3D_DATA,MULTI_FVM,
     D         FORNEQS,MAXDGAP,INTERFACES)

            TT=MAX(ZERO,TT-DT2)
            ISETK =0
C-------save Fint par AC,ACR---------
          ELSE
            IF (ILINTF>0) THEN
              NT_IMP1 = 0
              DO I = 1,NINTER
                NUM_IMP1(I) = 0
              END DO
            ENDIF
C
            CALL INTEGRATOR1_HP(D_IMP ,D    )
            IF ((ISECUT>0 .OR. IISROT>0 .OR. IMPOSE_DR/=0 .OR. IDROT==1) .AND. IRODDL/=0) THEN
              CALL INTEGRATOR1_HP(DR_IMP,DR)
            ENDIF
C
            IF (ISCAU>0)CALL INTEGRATOR1_HP(D_IMP ,X    )
          ENDIF
          CALL INTEGRATOR1_HP(D_IMP ,X_A   )
C-----------------------------
citask0        END IF !(ITASK == 0) THEN
C-----------------------------
        ELSE  !  nonlinear
C-------------------------
C        IF (GAP<ZERO) GOTO 300
          IF (R_IMP(18)<ZERO.OR.IMCONV==-2) GOTO 300
          IF (IMCONV==1) THEN
C--------valeur au debut d'increament--sauf diverge----
citask0          IF (ITASK == 0) THEN
            IF(NCY_MAX>0.AND.NCYCLE>NCY_MAX) CALL IMP_STOP(-3)
            IF (INCONV==1) THEN
              CALL CP_IMPBUF(1     ,ELBUF   ,ELBUF_C ,BUFMAT ,BUFMAT_C ,
     .                       FSAV  ,VOLMON  ,PARTSAV ,INTBUF_TAB       ,
     .                       INTBUF_TAB_C,IPARI   ,ISLEN7 ,IRLEN7   ,
     .                       ISLEN11,IRLEN11,ISLEN17 ,IRLEN17,IRLEN7T  ,
     .                       ISLEN7T,IRLEN20,ISLEN20 ,IRLEN20T,ISLEN20T,
     .             IRLEN20E,ISLEN20E,NEWFRONT,ELBUF_TAB,ELBUF_IMP,
     .             IPARG   )
            END IF
            IF (NCYCLE==1) THEN
              IF (ISPRB==1.AND.I_IMP(5)==0) R_IMP(1) = ZERO
              R_IMP(1) = MAX(R_IMP(1),RF_MIN*RF_MIN)
              R_IMP(1) = MIN(R_IMP(1),RF_MAX*RF_MAX)
              IF (INCONV==1) I_IMP(12)=1
            END IF
C--------case of converge with 0 iteration-------
            IF (ISMDISP>0) THEN
              CALL CP_REAL_HP(NNDL,X_A,X_C)
            ELSE
              CALL CP_REAL_HP(NNDL,X,X_C)
            END IF
            I_IMP(2)=0
            I_IMP(6)=ICONTA
            IT=0
citask0          END IF !(ITASK == 0) THEN
C
            IF (ISIGINI==1) THEN
C           TMP1 = DT2*NCYCLE
C           TMP2 = TSTOP-TT+TMP1-DT2
C           BFAC =TMP1/MAX(DT2,TMP2)
C------strickly proportional
              BFAC= (TT-R_IMP(19))/(TSTOP-R_IMP(19))
              R_IMP(10)=BFAC-ONE
              IF (R_IMP(10)<ZERO)CALL VAXPY_HP(NDDL ,LB  ,LB0  ,R_IMP(10))
            ENDIF
C
C----------------------
c      CALL MY_BARRIER
C---------------------
c          R2=ZERO
            CALL PRODUT_HP(NDDL,LB,LB,W_DDL,R2)
C----------------------
c      CALL MY_BARRIER
C---------------------
            IF (R2>=ZERO.AND.R2<EP30) THEN
            ELSEIF(IDYNA==0.AND.IQSTAT==0) THEN
              CALL IMP_STOP(0)
            ENDIF
C
            IF (INCONV == 1) R_IMP(1)=MAX(R_IMP(1),R2)
!sb
            IF(N_LIM == 1 .AND. ISPRB == 0) R_IMP(1)=R2
!fin sb
            IF (ISPRB==1) THEN
              IF (SQRT(R2/R_IMP(1))<=N_TOL) THEN
c            IF (ITASK == 0) THEN
                DT_IMP=TSTOP-TT+DT2
                CALL ZEROR_HP(D_IMP,NUMNOD)
                IF (IRODDL/=0) CALL ZEROR_HP(DR_IMP,NUMNOD)
c            END IF !(ITASK == 0) THEN
                GOTO 200
              ENDIF
            END IF !(ISPRB==1) THEN
C
citask0          IF (ITASK == 0) THEN
C
            IF (ISPRB==1) THEN
              TMP1 = DT2*NCYCLE
              TMP2 = TSTOP-TT+TMP1-DT2
              BFAC =TMP1/MAX(DT2,TMP2)
              R_IMP(10)=BFAC-ONE
              R_IMP(2)=R2*BFAC*BFAC
              IF (NCYCLE==1) THEN
                R_IMP(12)=EM01
C
                IF (ICONTA>0) R_IMP(12)=ZEP9
              ELSE
                TMP = DT12/MAX(DT12,TSTOP-TT)+N_TOL/SQRT(R2/R_IMP(1))
                TMP = MIN(HALF*TMP,ONE)
                R_IMP(12)=R_IMP(12)*(ONE-TMP)+TMP
              ENDIF
            ELSE
              R_IMP(2)=R2
            ENDIF
            R_IMP(3)=ONE
            R_IMP(4)=R_IMP(6)
C
citask0          END IF !(ITASK == 0) THEN
C----------------------
c      CALL MY_BARRIER
C---------------------
            IF (ISPRB==1) THEN
              TMP = R_IMP(10)+ONE
              CALL VSCALY_HP(NDDL ,LB  ,LB0  ,TMP )
            END IF
          ELSEIF (IMCONV==-1) THEN
C--------line-search------
            IF (ISPRB==1.OR.ISIGINI==1) THEN
              IF (R_IMP(10)<ZERO) THEN
                CALL VAXPY_HP(NDDL ,LB  ,LB0  ,R_IMP(10))
              ENDIF
            ENDIF
          ELSE
citask0          IF (ITASK == 0) THEN
            IT=IT+1
            I_IMP(2)=I_IMP(2)+1
citask0          END IF !(ITASK == 0) THEN
            IF (ISPRB==1.OR.ISIGINI==1) THEN
              IF (R_IMP(10)<ZERO) THEN
                CALL VAXPY_HP(NDDL ,LB  ,LB0  ,R_IMP(10))
              ENDIF
            ENDIF
          ENDIF  !         IF (IMCONV==1) THEN
C----------------------
c      CALL MY_BARRIER
C---------------------
citask0        IF (ITASK == 0) THEN
C-----------------------------
          IF (ISPRB==1) THEN
            FACI=MIN(ONE,R_IMP(12))
            R02=FACI*FACI*R_IMP(1)
          ELSE
            R02=R_IMP(1)
          ENDIF
          IF (IT==1.AND.IREFI==5) THEN
            R02 = MAX(R02,EM20)
            R_IMP(6) = MAX(EM20,R_IMP(6))
          ENDIF
          IF (IT==1.AND.ICONTA>I_IMP(6)) THEN
C       re-evoluer Rref-------
            IF (IREFI==5.AND.NFXV_G>0.AND.IMCONV>=0) THEN
              CALL RER02(RBY   ,LPBY  ,NPBY  ,SKEW  ,ISKEW ,
     1                   ITAB  ,WEIGHT,MS    ,IN    ,
     2                   IBFV  ,VEL   ,ICODT,ICODR ,
     3                   NRBYAC,IRBYAC,NINT2 ,IINT2 ,IPARI ,
     4                   INTBUF_TAB   ,NDOF  ,D_IMP ,DR_IMP,
     5                   X     ,XFRAME,DIRUL ,IXR   ,IXC   ,
     6                   IXTG  ,SH4TREE,SH3TREE,IRBE3 ,LRBE3,
     7                   FRBE3 ,IADK   ,JDIK  ,DIAG_K,LT_K ,
     8                   IDDL  ,IKC    ,INLOC ,NUM_IMP,NS_IMP,
     9                   NE_IMP,IND_IMP,NDDL  ,W_DDL  ,A   ,
     A                   AR    ,R02    ,IRBE2 ,LRBE2  ,X_C  )
              R_IMP(1) = MAX(R02,R_IMP(1))
            ENDIF
            IF (I_IMP(7)==0.AND.IREFI==4) IREFI= -4
          ENDIF
          IF (IMCONV>0.AND.ISPRB/=1) THEN
            R02 = MAX(R02,RF_MIN*RF_MIN)
            R02 = MIN(R02,RF_MAX*RF_MAX)
          END IF
C
          IF (NCYCLE==1.AND.INSOLV>=2.AND.IT==0.AND.IMCONV>=0)
     .     CALL BFGS_INI(NDDL,N_LIM)
          R_IMP(17) = R02
C-----------------------------
citask0        END IF !(ITASK == 0) THEN
C-----------------------------
C----------------------
c      CALL MY_BARRIER
C---------------------
c-------particular case .AND.NFXVEL==0
          IF (NDDL_G==0.AND.NFXVEL > 0) THEN
            IF (IT==0) THEN
C----add 3 to enforce at least one iteration in case of IMPDISP dependent (moving skew or frame)
              IMCONV=3
              ISETK=0
            ELSE
              IMCONV=1
            END IF
C----------------------
c      CALL MY_BARRIER
C---------------------
          ELSE
            CALL NL_SOLV(NDDL  ,IDDL  ,NDOF   ,IKC   ,D_IMP ,
     1                   DR_IMP,NNZK  ,IADK   ,JDIK  ,DIAG_K,
     2                   LT_K  ,LB    ,NDDLI  ,IADI  ,JDII  ,
     3                   DIAG_I,LT_I  ,ITOK   ,IADM  ,JDIM  ,
     4                   DIAG_M,LT_M  ,R_IMP(17),DD    ,DDR   ,
     5                   ITASK ,IT  ,I_IMP(2),R_IMP(3),R_IMP(2),
     6                   I_IMP(5) ,INPRINT,ISETP ,ISTOP ,R_IMP(4),
     7                   R_IMP(5),R_IMP(6),INLOC ,NDDL0 ,R_IMP(7),
     8                   R_IMP(11),R_IMP(18),ITAB  ,FR_ELEM,IAD_ELEM,
     9                   W_DDL    ,A      ,AR    ,V     ,MS    ,
     A                   X        ,IPARI ,INTBUF_TAB    ,NUM_IMP,
     B                   NS_IMP   ,NE_IMP,NSREM ,NSL   ,ICONTA ,
     C                   GRAPHE   ,FAC_K ,IPIV_K, NKCOND,NMONV  ,
     D                   IMONV ,MONVOL ,IGRSURF,FR_MV  ,
     E                   VOLMON,IBFV   ,SKEW  ,XFRAME,MUMPS_PAR,
     F                   CDDLP ,IND_IMP,NBINTC,INTLIST,NEWFRONT,
     G                   ISENDTO,IRECVFROM,IRBE3,LRBE3,I_IMP(8),
     H                   I_IMP(9),I_IMP(10),FEXT  ,DG    ,DGR  ,
     I                   DG0   ,DGR0 ,R_IMP(13),R_IMP(14),
     J                   NODFTSK,NODLTSK,IRBE2,LRBE2,I_IMP(12),
     K                   R_IMP(20),anew_stif)
          END IF !(NDDL==0.AND.NFXVEL > 0) THEN

C---------------------------------
citask0        IF (ITASK == 0) THEN
C---------------------------------
          IF(NFXVEL/=0) THEN
C-----for FV_local
            NTMP=0
            DO I=1,NFXVEL
              NTMP=NTMP+IABS(DIRUL(I))
            END DO
            IF (NTMP>0)
     .      CALL FV_IMP(IBFV  ,NPC    ,TF     ,VEL   ,SENSOR_TAB,
     1                  D_IMP  ,DR_IMP ,IKC   ,IDDL  ,NSENSOR   ,
     2                  SKEW  ,IFRAME ,XFRAME ,V     ,VR    ,
     3                  X     ,DIRUL  ,NDOF   ,A     ,AR    )
          END IF
C--------Rigid motions elimination----
          IF(IRIG_M>0.AND.IMCONV==1) THEN
            CALL SPB_RM_RIG(
     1      X         ,IXC       ,IXTG      ,NDOF      ,IDDL      ,
     2      IKC       ,D_IMP     ,DR_IMP    ,ICODT     ,ICODR ,
     3      SKEW      ,ISKEW     ,itab     )
          END IF
          IF(IMP_LR > 0)THEN
            CALL RECUKIN(RBY   ,LPBY  ,NPBY  ,SKEW  ,ISKEW ,
     1                   ITAB  ,WEIGHT,MS    ,IN    ,
     2                   IBFV  ,VEL   ,ICODT,ICODR ,
     3                   NRBYAC,IRBYAC,NINT2 ,IINT2 ,IPARI ,
     4                   INTBUF_TAB,NDOF  ,D_IMP ,DR_IMP,
     5                   X_C   ,XFRAME,DIRUL ,IXR   ,IXC   ,
     6                   IXTG  ,SH4TREE,SH3TREE,IRBE3 ,LRBE3,
     7                   FRBE3 ,IRBE2 ,LRBE2 )
          ELSE
            CALL RECUKIN(RBY   ,LPBY  ,NPBY  ,SKEW  ,ISKEW ,
     1                   ITAB  ,WEIGHT,MS    ,IN    ,
     2                   IBFV  ,VEL   ,ICODT,ICODR ,
     3                   NRBYAC,IRBYAC,NINT2 ,IINT2 ,IPARI ,
     4                   INTBUF_TAB   ,NDOF  ,D_IMP ,DR_IMP,
     5                   X     ,XFRAME,DIRUL ,IXR   ,IXC   ,
     6                   IXTG  ,SH4TREE,SH3TREE,IRBE3 ,LRBE3,
     7                   FRBE3 ,IRBE2 ,LRBE2 )
          END IF
c       Print information of non-linear solver
          IF (SOLVNFO > ZERO) THEN
            IF (IMCONV /= -1) THEN
              CALL PR_SOLNFO(NDDL  ,IDDL  ,NDOF   ,IKC   ,ITAB   ,
     1                      DIAG_K,DIAG_M,INLOC  ,FR_ELEM,IAD_ELEM,
     2                      IADK  ,JDIK  ,LT_K   ,LT_M   ,NDDLI  ,
     3                      IADI  ,JDII  ,ITOK   ,DIAG_I ,LT_I   ,
     4                      LBB   ,LBB   ,IT     ,NSREM  ,NSL    ,
     5                      D_IMP ,DR_IMP,1      ,W_DDL  ,AC     ,
     6                      ACR   ,A     ,AR     ,R2    ,NDEB0   ,
     7                      R_IMP ,I_IMP ,DD     ,DDR)
            ENDIF
          ENDIF
c
c        IF (IMPDEB>0) THEN
c          IF (NCYCLE>=NDEB0.AND.NCYCLE<=NDEB1) THEN
c            CALL PRODUT_HP(NDDL,LB,LB,W_DDL,R2)
c            CALL PR_DEB(NDDL  ,IDDL  ,NDOF   ,IKC   ,ITAB   ,
c     1                  DIAG_K,DIAG_M,INLOC  ,FR_ELEM,IAD_ELEM,
c     2                  IADK  ,JDIK  ,LT_K   ,LT_M   ,NDDLI  ,
c     3                  IADI  ,JDII  ,ITOK   ,DIAG_I ,LT_I   ,
c     4                  LBB   ,LBB   ,IT     ,NSREM  ,NSL    ,
c     5                  D_IMP ,DR_IMP,1      ,W_DDL  ,AC     ,
c     6                  ACR   ,A     ,AR     ,R2    ,NDEB0   )
c          END IF
c         END IF
c
          IF (NBINTC>0) THEN
            IF (ISMDISP>0) THEN
              CALL IMP_DTKIN(
     1           IPARI ,INTBUF_TAB       ,X_A     ,V       ,
     2           VR    ,ITAB   ,D_IMP   ,DR_IMP  ,NBINTC  ,
     3           INTLIST,ITASK  ,NEWFRONT,ISENDTO ,IRECVFROM,
     4           IDDL  ,NDOF   ,IKC     ,TMP     ,MS      ,
     5           NSENSOR,SENSOR_TAB,MAXDGAP)
            ELSE
              CALL IMP_DTKIN(
     1           IPARI ,INTBUF_TAB      ,X       ,V       ,
     2           VR    ,ITAB   ,D_IMP   ,DR_IMP  ,NBINTC  ,
     3           INTLIST,ITASK  ,NEWFRONT,ISENDTO ,IRECVFROM,
     4           IDDL  ,NDOF   ,IKC     ,TMP     ,MS      ,
     5           NSENSOR,SENSOR_TAB,MAXDGAP)
              IF(NFXV_G/=0.AND.TMP<ONE)
     .        CALL FV_IMP(IBFV  ,NPC    ,TF     ,VEL   ,SENSOR_TAB,
     1                    D_IMP  ,DR_IMP ,IKC   ,IDDL  ,NSENSOR   ,
     2                    SKEW  ,IFRAME ,XFRAME ,V     ,VR    ,
     3                    X     ,DIRUL  ,NDOF   ,A     ,AR    )
            END IF !(ISMDISP>0) THEN
          END IF
C
C---------------------------------
citask0        END IF !(ITASK == 0) THEN
C---------------------------------
 300      CONTINUE
C---------------------------------
citask0        IF (ITASK == 0) THEN
C---------------------------------
          IF (ISMDISP>0) THEN
            CALL CP_REAL_HP(NNDL,X_C,X_A)
          ELSE
            CALL CP_REAL_HP(NNDL,X_C,X)
          END IF
          CALL CP_IMPBUF(2      ,ELBUF   ,ELBUF_C ,BUFMAT  ,BUFMAT_C  ,
     .                   FSAV   ,VOLMON  ,PARTSAV ,INTBUF_TAB       ,
     .                   INTBUF_TAB_C ,IPARI   ,ISLEN7 ,IRLEN7   ,
     .                   ISLEN11,IRLEN11,ISLEN17 ,IRLEN17,IRLEN7T  ,
     .                   ISLEN7T,IRLEN20,ISLEN20 ,IRLEN20T,ISLEN20T,
     .         IRLEN20E,ISLEN20E,NEWFRONT,ELBUF_TAB,ELBUF_IMP,
     .         IPARG   )

          IF (NCYCLE == 1 .AND. ISTOP == 0 .AND.ISOLV == 7) THEN
            IF (IT == 1 .AND. I_IMP(5) == 0 ) THEN
              WRITE (IOUT, *)
     .        "    **PCG SOLVER HAS BEEN SELECTED FIRSTLY FOR THIS RUN**"
              WRITE (ISTDO, *)
     .        "    **PCG SOLVER HAS BEEN SELECTED FIRSTLY FOR THIS RUN**"
            END IF
          END IF

          IF (ISTOP>0) THEN
            IF (ISTOP == 3 .AND.ISOLV == 7) THEN

              ISOLV = 3
              ISETK = 1
              IKPAT = 0
              I_IMP(11)=1
              ISTOP = 0
              IPREC = 1
              IF (NSPMD > 1 ) THEN
                IF (IMUMPSD == 0) IMUMPSD = 1
                IF (IMUMPSV == 0) IMUMPSV = 1
              END IF
              IF (NCYCLE == 1 ) THEN
                IF (ISPMD  == 0) THEN
                  WRITE (IOUT, *)
     .            "    **DIRECT SOLVER HAS BEEN SELECTED FOR THIS RUN**"
                  WRITE (ISTDO, *)
     .            "    **DIRECT SOLVER HAS BEEN SELECTED FOR THIS RUN**"
                END IF !(NSPMD  == 0) THEN
              ELSE
                IF (ISPMD  == 0) THEN
                  WRITE (IOUT, *)
     .            "    **PCG SOLVER HAS BEEN CHANGED TO DIRECT SOLVER **"
                  WRITE (ISTDO, *)
     .            "    **PCG SOLVER HAS BEEN CHANGED TO DIRECT SOLVER **"
                END IF !(NSPMD  == 0) THEN
              END IF

            ENDIF
            IMCONV=-2
            CALL IDDL2NOD(NDDL  ,IDDL  ,NDOF  ,IKC   ,INLOC ,
     .                    ISTOP ,NNOD  )
            IF (NNOD>0) THEN
              WRITE(IOUT,1008)ITAB(NNOD)
              WRITE(ISTDO,1008)ITAB(NNOD)
            ENDIF
          ENDIF
          INCONV = MIN(1,IMCONV)
          IF (IMCONV<=-2) THEN
            CALL ZEROR_HP(D_IMP,NUMNOD)
            IF (IRODDL/=0) CALL ZEROR_HP(DR_IMP,NUMNOD)
            R_IMP(6)=R_IMP(4)
            I_IMP(5)=-2
            IF (ISPRB==1.AND.IMCONV==-3.AND.ICONTA==0) THEN
              DO I=1,NDDL
                LB(I) = LB0(I)
              ENDDO
              IMCONV=1
              GOTO 100
            ENDIF
            TT=MAX(ZERO,TT-DT2)
            NCYCLE=NCYCLE-1
            IF (NCYCLE==0) DT1=ZERO
            CALL INT5_DIVERG(IPARI )
            IF (IMCONV==-2.AND.I_IMP(11)/=1) THEN
C-----------change dt------
              CALL IMP_DTN(IT,R_IMP(11),R_IMP(10),R_IMP(24))
C           CALL NUL_ETFAC_A
              CALL ETFAC_INI(IPARG )
              IF (DT_IMP==DT_MIN) THEN
                CALL IMP_STOP(IMCONV)
              ENDIF
            ENDIF
          ENDIF
C
          IF (IMCONV<=-2.OR.IMCONV==0) THEN
            IF (IT==1.AND.ICONTA>I_IMP(6)) THEN
              R02 =R_IMP(17)
              IF (IREFI==1) THEN
                R02 = MIN(R02,TEN*R_IMP(1))
              ELSEIF (IREFI==2) THEN
                R02 = MIN(R02,ONEP2*R_IMP(1))
              ELSEIF (IREFI==3.OR.IREFI==4.OR.IREFI==5) THEN
                R02 = MIN(R02,R_IMP(1))
              ELSEIF (IREFI==-4) THEN
                I_IMP(7) = 1
                IREFI = 4
              END IF
C----Used for gravity case
              IF (NCYCLE > 1) I_IMP(7) = 1
              R_IMP(1)=MAX(R_IMP(1),R02)
            ENDIF
          ENDIF !IF (IMCONV<=-2.OR.IMCONV==0)
C------------for restart---
          IF (IMCONV>0) THEN
            R_IMP(1) = MAX(R_IMP(1),RF_MIN*RF_MIN)
            R_IMP(1) = MIN(R_IMP(1),RF_MAX*RF_MAX)
          ENDIF
C
          IF (IMCONV==2) DT2=DT2/I_IMP(2)
C---------------------------------
 200      CONTINUE
C    /---------------/
c      CALL MY_BARRIER
C    /---------------/
C---------------------------------
citask0        IF (ITASK == 0) THEN
C---------------------------------
          IF (IMCONV==1.OR.IMCONV==2.OR.IMCONV==3) THEN
            IF(IDYNA>0.AND.NFXVEL/=0) THEN
              CALL FV_FINT0(IBFV  ,NPC    ,TF   ,VEL   ,SENSOR_TAB,
     1                     D_IMP ,DR_IMP,IKC   ,IDDL  ,NSENSOR   ,
     2                     SKEW   ,IFRAME ,XFRAME,A    ,AR    ,
     3                     X      ,NDOF  ,MS   ,IN    ,WEIGHT ,
     4                     RBY    )
            END IF
            IF (IMCONV/=3) CALL INTEGRATOR1_HP(D_IMP ,D     )
            IF (R_IMP(11)<EM10)
     .      CALL PRODUT_UHP0(D_IMP ,DR_IMP,R_IMP(11),WEIGHT)
            CALL IMP_DTN(IT,R_IMP(11),R_IMP(10),R_IMP(24))
            IF ( IQSTAT>0) CALL DIS_CP(NNDL,D_IMP,DR_IMP,0    )
          ENDIF
          IF (INCONV==1 .AND. (ISECUT>0.OR.IISROT>0
     .       .OR. IMPOSE_DR/=0 .OR. IDROT==1)
     .       .AND. IRODDL/=0) THEN
            IF (IMCONV/=3) CALL INTEGRATOR1_HP(DR_IMP,DR)
          ENDIF
          IF (ISMDISP>0) THEN
            CALL INTEGRATOR_HP(NDT   ,D_IMP ,DR_IMP,
     1                         X_A   ,V     ,VR    ,A     ,AR    )
          ELSE
C
            CALL INTEGRATOR_HP(NDT   ,D_IMP ,DR_IMP,
     1                         X     ,V     ,VR    ,A     ,AR    )
          ENDIF
C
          IF(IDYNA>0.AND.IMCONV==1)  THEN
            CALL DYNA_WEX(IBCL  ,FORC   ,NPC   ,TF    ,AC    ,
     2                    V     ,X      ,SKEW  ,ACR   ,VR    ,
     3                    SENSOR_TAB,WEIGHT,TFEXT ,IADS_F,
     4                    FSKY  ,IGRV   ,AGRV  ,MS    ,IN    ,
     5                    LGRAV ,ITASK ,NRBYAC,IRBYAC ,
     6                    NPBY  ,RBY   ,IBFV    ,VEL   ,D_IMP  ,
     7                    DR_IMP,IKC   ,IDDL  ,IFRAME,XFRAME  ,
     8                    NDOF  ,H3D_DATA,CPTREAC,FTHREAC,NODREAC,NSENSOR,
     9                    TH_SURF ,FSAVSURF,NSEG_LOADP,DPL0CLD,
     A                    VEL0CLD, NUMNOD,NSURF,NFUNCT,NCONLD,
     B                    NGRAV,NFXVEL)
            CALL DYNA_CPR0(NDDL0  )
          END IF
C----------D_imp taking D_n-1 :exception for case with Gravity-----
          IF (IMCONV<=-2 .AND.IQSTAT>0 .AND. I_IMP(7) >0) THEN
            CALL DIS_CP(NNDL,D_IMP,DR_IMP,1    )
          END IF
C
          IF (IMCONV == 3 ) INCONV = 0
          IF (IMCONV<=-2) IMCONV=1
          IF (IMCONV==1) I_IMP(1)=I_IMP(1)+IT+1
          IF (IMCONV==1) I_IMP(12)=INCONV
          I_IMP(4)=NDT-1
          IT_T = I_IMP(1)
C--------
citask0        END IF !(ITASK == 0) THEN
C
        ENDIF !IF (ILINE==1) THEN
C----------------------
        CALL MY_BARRIER
C---------------------
citask0       IF (ITASK == 0) THEN
        IF (NINT7>0) THEN
          DEALLOCATE(IADI)
          DEALLOCATE(ITOK)
          DEALLOCATE(JDII)
          DEALLOCATE(DIAG_I)
          DEALLOCATE(LT_I)
        ENDIF
C
        IF ((NSREM+NSL)>0) CALL INI_KIC
        IF (ILINTF>0) DEALLOCATE(XI_C)
        IF (INTP_C<0) CALL DEALLOCM
        IF (NINT2>0) DEALLOCATE(IAINT2)

citask0       END IF !(ITASK == 0) THEN
c
 1001   FORMAT(' SYMBOLIC DIM : NDDL =',I8,1X,'NNZ =',I8,1X,'NB_MAX =',I8)
 1002   FORMAT(' FINAL    DIM : NDDL =',I8,1X,'NNZ =',I8,1X,'NB_MAX =',I8)
 1003   FORMAT(/,5X,'--STIFFNESS MATRIX IS REFORMED --')
 1004   FORMAT(3X,'LINE. SOLVER : ISOLV =',I4,2X,'PREC. Meth. =',I4,2X,
     .         'TOL =',E11.4)
 1005   FORMAT(5X,'--STIFFNESS MATRIX WILL BE REFORMED AFTER EACH ',I4,
     .         2X,'ITERATIONS--')
 1006   FORMAT(5X,'--SUPPLEMENTARY CONTACT STIFFNESS MATRIX',
     .            1X, 'IS CREATED--')
 1007   FORMAT(5X,' WITH DIM. : ND   =',I8,1X,'NZ  =',I8) !,1X,'NB_MAX =',I8)
 1008   FORMAT(3X,'**WARNING: STIFFNESS MATRIX IS NOT DEFINITE**'/,
     .         3X,'**LOOK AT NODE: ',I8)
 1009   FORMAT(3X,'**TIMESTEP WILL BE REDUCED TO AVOID DE-ACTIVATION ',
     .            'IN INTERFACE:**',I8)
 1010   FORMAT(/,5X,'--STIFFNESS MATRIX IS REFORMED',1X,
     .         'DUE TO RIGID WALL IMPACT--'/,5X,'WITH IMPACT NUM. =',I8)
 1011   FORMAT(5X,' WITH DIM. : ND   =',I8)
 1012   FORMAT(3X,'**TIMESTEP WILL BE REDUCED DUE TO ',
     .            'DIM.(ND) CHANGE W/AUTOSPC::**',2I8)
        RETURN
C endif MUMPS defined
#endif
      END
Chd|====================================================================
Chd|  IMP_STOP                      source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_CHECK                     source/implicit/imp_solv.F    
Chd|        IMP_ERRMUMPS                  source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        M36ITER_IMP                   source/materials/mat/mat036/m36iter_imp.F
Chd|        NL_SOLV                       source/implicit/nl_solv.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MY_FLUSH                      source/system/machine.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE IMP_STOP(ISTOP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "units_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
        INTEGER
     .    ISTOP,IMG
        CHARACTER*60       MSG(-4:2)
        DATA               MSG
     .    / 'STOPPED DUE TO SOLVER ERROR **',
     .      'STOPPED DUE TO NCYCLE LIMIT **',
     .      'STOPPED DUE TO TIMESTEP LIMIT **',
     .      'STOPPED DUE TO MODELLING DATA **',
     .      'STOPPED DUE TO LOADING DATA **' ,
     .      'STOPPED DUE TO DIVERGENCE **'  ,
     .      'STOP WITH CHECKING **'  /
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        IF (ISPMD==0) THEN
          IMG=ISTOP
          IF (ISTOP>2) IMG=1
          CALL ANCMSG(MSGID=79,ANMODE=ANINFO,
     .                C1=MSG(IMG),I1=ISTOP)
          CALL MY_FLUSH(IOUT)
        ENDIF
        CALL ARRET(2)
C------------------------------------------
        RETURN
      END
C------------------------------------------
Chd|====================================================================
Chd|  IMP_CHECK                     source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_STOP                      source/implicit/imp_solv.F    
Chd|====================================================================
      SUBROUTINE IMP_CHECK(ITAB  ,NDDL  ,IDDL  ,DIAG_K  ,NDOF  ,
     .                     IKC   ,INLOC ,NDDL0 )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
        INTEGER NDDL,NDDL0,NDOF(*),IDDL(*),IKC(*),INLOC(*),ITAB(*)
        my_real
     .    DIAG_K(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NLIM,NID,NN,NKC,NFT,II,IDI,ND,ID,ISTOP
        PARAMETER (NLIM=6)
        INTEGER I,J,K,N,INOD(NLIM),IDL(6*NLIM),NFIX(NDDL0)
C------------------------------------------
        NID=0
        DO I=1,NDDL
          IF (DIAG_K(I)<EM10) THEN
            NID=NID+1
            IDL(NID)=I
            IF (NID==6*NLIM) GOTO 100
          ENDIF
        ENDDO
 100    CONTINUE
        IF (NID>0) THEN
c            write(*,*)'nid=',nid
          NKC=0
          DO N = 1,NUMNOD
            I=INLOC(N)
            DO J=1,NDOF(I)
              ND = IDDL(I)+J
              IF (IKC(ND)>0) NKC=NKC+1
              NFIX(ND)=NKC
            ENDDO
          ENDDO
          NN=0
          NFT=1
          DO 400 K = 1,NID
            DO  N = NFT,NUMNOD
              I=INLOC(N)
              IDI=IDDL(I)
              ID=IDI-NFIX(IDI)
C          IF (ID>IDL(K)) GOTO 400
              DO J=1,NDOF(I)
                ND = IDI+J
                ID = ND-NFIX(ND)
                IF (IDL(K)==ID) THEN
                  NN=NN+1
                  INOD(NN)=ITAB(I)
c            write(*,*)'id,i,n,j=',id,i,n,j
                  IF (NN==NLIM) GOTO 200
                  NFT=N+1
                  GOTO 400
                ENDIF
              ENDDO
            ENDDO
 400      CONTINUE
 200      CONTINUE
          IF (NN>0) THEN
            ISTOP=1
            WRITE(IOUT,*)
     .      ' **ERROR: STIFFNESS MATRIX IS NOT DEFINITE** '
            WRITE(IOUT,*)'--- LOOK AT NODES:---'
            WRITE(IOUT,*)(INOD(I),I=1,NN)
            WRITE(ISTDO,*)
     .      ' **ERROR: STIFFNESS MATRIX IS NOT DEFINITE** '
            WRITE(ISTDO,*)'--- LOOK AT NODES:---'
            WRITE(ISTDO,*)(INOD(I),I=1,NN)
            IF (NRBE2>0.AND.ILINE==0) ISTOP = 0
            IF (ISTOP>0) CALL IMP_STOP(-1)
          ENDIF
        ENDIF
C
C------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  PR_INFOK                      source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_K_EIG                     stub/imp_k_eig.F              
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        SPMD_INF_G                    source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE PR_INFOK(NDDL0,NNZK0,NDDL,NNZK,NNMAX)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "scr05_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
        INTEGER NDDL0,NNZK0,NDDL,NNZK,NNMAX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NDDLG0,NNZKG0,NDDLG,NNZKG,NNMAXG,NNMAX0
        INTEGER NDDL0P(NSPMD),NNZK0P(NSPMD),NDDLP(NSPMD) ,
     .          NNZKP(NSPMD),NNMAXP(NSPMD),I
C------------------------------------------
        IF (IMACH==3.AND.NSPMD>1) THEN
          NDDLG0 = NDDL0
          NNZKG0 = NNZK0
          NDDLG = NDDL
          NNZKG = NNZK
          NNMAXG = NNMAX
          CALL SPMD_INF_G(
     1     NDDLG0   ,NNZKG0  ,NDDLG    ,NNZKG    ,NNMAXG    ,
     2     NDDL0P   ,NNZK0P  ,NDDLP    ,NNZKP    ,NNMAXP    )
          IF (ISPMD==0) THEN
            IF (IMP_CHK>0) THEN
              WRITE(IOUT,*)
              WRITE(IOUT,*)'  *--------- STIFFNESS MATRIX INFO. ---------*'
              WRITE(IOUT,1001)NDDLG0,NNZKG0,NNMAXG
              WRITE(IOUT,*)
              DO I=1,NSPMD
                WRITE(IOUT,1003)I,NDDL0P(I),NNZK0P(I),NNMAXP(I)
              ENDDO
              WRITE(IOUT,*)
              WRITE(IOUT,1002)NDDLG,NNZKG,NNMAXG
              WRITE(IOUT,*)
              DO I=1,NSPMD
                WRITE(IOUT,1003)I,NDDLP(I),NNZKP(I),NNMAXP(I)
              ENDDO
            ELSE
              WRITE(IOUT,*)
              WRITE(ISTDO,*)
              WRITE(IOUT,*)'  *--------- STIFFNESS MATRIX SETUP ---------*'
              WRITE(ISTDO,*)'  *--------- STIFFNESS MATRIX SETUP ---------*'
              WRITE(IOUT,1001)NDDLG0,NNZKG0,NNMAXG
              WRITE(ISTDO,1001)NDDLG0,NNZKG0,NNMAXG
              WRITE(IOUT,*)
              WRITE(ISTDO,*)
              DO I=1,NSPMD
                WRITE(IOUT,1003)I,NDDL0P(I),NNZK0P(I),NNMAXP(I)
                WRITE(ISTDO,1003)I,NDDL0P(I),NNZK0P(I),NNMAXP(I)
              ENDDO
              WRITE(IOUT,*)
              WRITE(ISTDO,*)
              WRITE(IOUT,1002)NDDLG,NNZKG,NNMAXG
              WRITE(ISTDO,1002)NDDLG,NNZKG,NNMAXG
              WRITE(IOUT,*)
              WRITE(ISTDO,*)
              DO I=1,NSPMD
                WRITE(IOUT,1003)I,NDDLP(I),NNZKP(I),NNMAXP(I)
                WRITE(ISTDO,1003)I,NDDLP(I),NNZKP(I),NNMAXP(I)
              ENDDO
C          IF (L_LIM==0) L_LIM=NDDLG
            ENDIF
          ENDIF
        ELSE
          IF (IMP_CHK>0) THEN
            WRITE(IOUT,*)
            WRITE(IOUT,*)'  *--------- STIFFNESS MATRIX INFO. ---------*'
            WRITE(IOUT,1001)NDDL0,NNZK0,NNMAX
            WRITE(IOUT,1002)NDDL,NNZK,NNMAX
            WRITE(IOUT,*)
          ELSE
            WRITE(IOUT,*)
            WRITE(ISTDO,*)
            WRITE(IOUT,*)'  *--------- STIFFNESS MATRIX SETUP ---------*'
            WRITE(ISTDO,*)'  *--------- STIFFNESS MATRIX SETUP ---------*'
            WRITE(IOUT,1001)NDDL0,NNZK0,NNMAX
            WRITE(ISTDO,1001)NDDL0,NNZK0,NNMAX
            WRITE(IOUT,1002)NDDL,NNZK,NNMAX
            WRITE(ISTDO,1002)NDDL,NNZK,NNMAX
            WRITE(IOUT,*)
            WRITE(ISTDO,*)
          ENDIF
          IF (L_LIM==0) L_LIM=NDDL
        ENDIF
 1001   FORMAT(' SYMBOLIC DIM : ND =',I8,1X,'NZ =',I10,1X,'NB_MAX =',I8)
 1002   FORMAT(' FINAL    DIM : ND =',I8,1X,'NZ =',I10,1X,'NB_MAX =',I8)
 1003   FORMAT(' PROC=',I5,5X,'ND =',I8,1X,'NZ =',I10,1X,'NB_MAX =',I8)
C------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  K_BAND                        source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        IND_SPA2                      source/implicit/ind_glob_k.F  
Chd|        IND_SPAN                      source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE K_BAND(NDDL,IADK,JDIK,NDMAX)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
        INTEGER NDDL,IADK(*),JDIK(*),NDMAX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,JD,ND(NDDL)
C------------------------------------------
        DO I = 1, NDDL
          ND(I) = 1 + IADK(I+1) - IADK(I)
          DO J = IADK(I),IADK(I+1)-1
            JD = JDIK(J)
            ND(JD) = ND(JD) + 1
          ENDDO
        ENDDO
C
        NDMAX = 0
        DO I = 1, NDDL
          NDMAX = MAX(NDMAX,ND(I))
        ENDDO
C------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  M_LNZ                         source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        DIM_SUBNZ                     source/implicit/imp_solv.F    
Chd|        SP_STAT0                      source/implicit/imp_fsa_inv.F 
Chd|====================================================================
      SUBROUTINE M_LNZ(NDDL,IADK,JDIK,NDMAX,NLMAX)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
        INTEGER NDDL,IADK(*),JDIK(*),NDMAX,NLMAX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,JD,JM(NDMAX+1),NC,NNZ
C------------------------------------------
        DO I=1,NDDL
          CALL SP_STAT0(I  ,IADK  ,JDIK  ,NC    ,JM    )
          CALL DIM_SUBNZ(IADK  ,JDIK  ,NC    ,JM    ,NNZ  )
          NLMAX = MAX(NLMAX,NNZ)
        ENDDO
C------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  DIM_SUBNZ                     source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        M_LNZ                         source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        INTAB0                        source/implicit/imp_fsa_inv.F 
Chd|====================================================================
      SUBROUTINE DIM_SUBNZ(IADK  ,JDIK  ,NC    ,JM    , NNZA  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER  IADK(*)  ,JDIK(*),NC    ,JM(*),NNZA
C     REAL
C-----------------------------------------------
C   External function
C-----------------------------------------------
        INTEGER INTAB0
        EXTERNAL INTAB0
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,K,JJ,N
C--------------------------------------------
        NNZA=0
        DO I=1,NC
          J=JM(I)
          DO K=IADK(J),IADK(J+1)-1
            JJ=JDIK(K)
            N=INTAB0(NC,JM,JJ)
            IF (N>0) NNZA=NNZA+1
          ENDDO
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  IMP_CHECM                     source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IMP_CHECM(ITAB  ,NDDL  ,IDDL  ,DIAG_M  ,NDOF  ,
     .                     IKC   ,INLOC ,NDDL0 )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
        INTEGER NDDL,NDDL0,NDOF(*),IDDL(*),IKC(*),INLOC(*),ITAB(*)
        my_real
     .    DIAG_M(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NLIM,NID,NN,NKC,NFT,II,IDI,ND,ID
        PARAMETER (NLIM=6)
        INTEGER I,J,K,N,INOD(NLIM),IDL(6*NLIM),NFIX(NDDL0)
C------------------------------------------
        NID=0
        DO I=1,NDDL
          IF (DIAG_M(I)>EP10) THEN
            NID=NID+1
            IDL(NID)=I
            IF (NID==6*NLIM) GOTO 100
          ENDIF
        ENDDO
 100    CONTINUE
        IF (NID>0) THEN
          NKC=0
          DO N = 1,NUMNOD
            I=INLOC(N)
            DO J=1,NDOF(I)
              ND = IDDL(I)+J
              IF (IKC(ND)>0) NKC=NKC+1
              NFIX(ND)=NKC
            ENDDO
          ENDDO
          NN=0
          NFT=1
          DO 400 K = 1,NID
            DO  N = NFT,NUMNOD
              I=INLOC(N)
              IDI=IDDL(I)
              ID=IDI-NFIX(IDI)
              DO J=1,NDOF(I)
                ND = IDI+J
                ID = ND-NFIX(ND)
                IF (IDL(K)==ID) THEN
                  NN=NN+1
                  INOD(NN)=ITAB(I)
                  IF (NN==NLIM) GOTO 200
                  NFT=N+1
                  GOTO 400
                ENDIF
              ENDDO
            ENDDO
 400      CONTINUE
 200      CONTINUE
          IF (NN>0) THEN
            WRITE(IOUT,*)
     .      ' **WARNING : POSSIBLE NOT DEFINITE STIFFNESS MATRIX ** '
            WRITE(IOUT,*)'--- LOOK AT NODES:---'
            WRITE(IOUT,*)(INOD(I),I=1,NN)
            WRITE(ISTDO,*)
     .      ' **WARNING : POSSIBLE NOT DEFINITE STIFFNESS MATRIX ** '
            WRITE(ISTDO,*)'--- LOOK AT NODES:---'
            WRITE(ISTDO,*)(INOD(I),I=1,NN)
          ENDIF
        ENDIF
C
C------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  IMP_B2A                       source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IMP_B2A(F     ,M      ,IDDL   ,NDOF  ,B  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER  IDDL(*),NDOF(*)
C     REAL
        my_real
     .   F(3,*),M(3,*),B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,ID
C------------------------------------------
        DO I = 1,NUMNOD
          DO J =1,NDOF(I)
            ID = IDDL(I) + J
            IF (J>3) THEN
              M(J-3,I) = B(ID)
            ELSE
              F(J,I) = B(ID)
            ENDIF
          ENDDO
        ENDDO
C------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  INI_KIF                       source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_LINTF                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE INI_KIF
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_LINTF
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        NDDLIF = 0
        NZIF = 0
C------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  SAVE_KIF                      source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        CP_INT                        source/implicit/produt_v.F    
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|        IMP_LINTF                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE SAVE_KIF(NDDL  ,IADK  ,JDIK  ,DIAG_K,LT_K   ,
     1                    ITOK  ,NDDLG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_LINTF
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NDDL,IADK(*),JDIK(*),ITOK(*),NDDLG
C     REAL
        my_real
     .    DIAG_K(*),LT_K(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER IADCP(NDDLIF+1),JDICP(NZIF),ITOCP(NDDLIF)
        INTEGER NDDLI0,NZI0,NZK,IERR1,IERR2,ITAG(NDDLG),
     .          I,J,II,NL,NR,ITON(NDDL)
        my_real
     .    DIAG_CP(NDDLIF),LT_CP(NZIF)
C------------------------------------------
        IF (NDDL==0) RETURN
        IF (NDDLIF==0) THEN
          NDDLIF = NDDL
          NZIF =IADK(NDDL+1)-IADK(1)
          IF(ALLOCATED(IADIF)) DEALLOCATE(IADIF)
          IF(ALLOCATED(JDIIF)) DEALLOCATE(JDIIF)
          IF(ALLOCATED(IFTOK)) DEALLOCATE(IFTOK)
          ALLOCATE(IADIF(NDDLIF+1),IFTOK(NDDLIF),JDIIF(NZIF),STAT=IERR1)
          CALL CP_INT((NDDLIF+1),IADK,IADIF)
          CALL CP_INT(NDDLIF,ITOK,IFTOK)
          CALL CP_INT(NZIF,JDIK,JDIIF)
          IF(ALLOCATED(DIAG_IF)) DEALLOCATE(DIAG_IF)
          IF(ALLOCATED(LT_IF)) DEALLOCATE(LT_IF)
          ALLOCATE(DIAG_IF(NDDLIF),LT_IF(NZIF),STAT=IERR2)
          CALL CP_REAL(NDDLIF,DIAG_K,DIAG_IF)
          CALL CP_REAL(NZIF,LT_K,LT_IF)
        ELSE
          CALL CP_INT((NDDLIF+1),IADIF,IADCP)
          CALL CP_INT(NDDLIF,IFTOK,ITOCP)
          CALL CP_REAL(NDDLIF,DIAG_IF,DIAG_CP)
          CALL CP_INT(NZIF,JDIIF,JDICP)
          CALL CP_REAL(NZIF,LT_IF,LT_CP)
          NDDLI0 = NDDLIF
          NZI0 = NZIF
          DO I = 1,NDDLG
            ITAG(I) = 0
          ENDDO
          DO I = 1,NDDLI0
            ITAG(IFTOK(I)) = I
          ENDDO
          DO I = 1,NDDL
            J = ITOK(I)
            IF (ITAG(J)==0) THEN
              NDDLIF = NDDLIF+1
              NZIF = NZIF+IADK(I+1)-IADK(I)
              ITON(I) = NDDLIF
            ELSE
              ITON(I) = ITAG(J)
            ENDIF
          ENDDO
          IF(ALLOCATED(IADIF)) DEALLOCATE(IADIF)
          IF(ALLOCATED(JDIIF)) DEALLOCATE(JDIIF)
          IF(ALLOCATED(IFTOK)) DEALLOCATE(IFTOK)
          ALLOCATE(IADIF(NDDLIF+1),IFTOK(NDDLIF),JDIIF(NZIF),STAT=IERR1)
          IF(ALLOCATED(DIAG_IF)) DEALLOCATE(DIAG_IF)
          IF(ALLOCATED(LT_IF)) DEALLOCATE(LT_IF)
          ALLOCATE(DIAG_IF(NDDLIF),LT_IF(NZIF),STAT=IERR2)
C---------copy old-----
          CALL CP_INT((NDDLI0+1),IADCP,IADIF)
          CALL CP_INT(NDDLI0,ITOCP,IFTOK)
          CALL CP_REAL(NDDLI0,DIAG_CP,DIAG_IF)
          CALL CP_INT(NZI0,JDICP,JDIIF)
          CALL CP_REAL(NZI0,LT_CP,LT_IF)
C---------add [k]-----
          NL = NDDLI0
          NZIF = NZI0
          DO I = 1,NDDL
            J = ITOK(I)
            IF (ITAG(J)==0) THEN
              NL = NL + 1
              NR = IADK(I+1)-IADK(I)
              IFTOK(NL) = J
              DIAG_IF(NL)=DIAG_K(I)
              DO II = IADK(I),IADK(I+1)-1
                NZIF = NZIF + 1
                JDIIF(NZIF) = ITON(JDIK(II))
                LT_IF(NZIF) = LT_K(II)
              ENDDO
              IADIF(NL+1) = NZIF + 1
            ENDIF
          ENDDO
C
          IF (NL/=NDDLIF)
     .      print *,'--MEMERY PROBLEM [K]if--:',NL,NDDLIF
        ENDIF
C------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  DIAG_KIF                      source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        IMP_LINTF                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE DIAG_KIF(DIAG_K)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_LINTF
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
        my_real
     .    DIAG_K(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,II
C-----------------------------
        RETURN
        DO I=1,NDDLIF
          II = IFTOK(I)
          DIAG_K(II) = DIAG_K(II) +DIAG_IF(I)
        ENDDO
C-----------------------------
        RETURN
      END
Chd|====================================================================
Chd|  MATV_KIF                      source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        MAV_LT2                       source/implicit/produt_v.F    
Chd|        MAV_LTH                       source/implicit/produt_v.F    
Chd|        MAV_LTH0                      source/implicit/produt_v.F    
Chd|        MAV_LTP                       source/implicit/produt_v.F    
Chd|-- calls ---------------
Chd|        IMP_LINTF                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE MATV_KIF(V,W)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_LINTF
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
        my_real
     .    W(*), V(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,K,II,KK
        my_real
     .     L_K
C-----------------------------
        RETURN
        DO I=1,NDDLIF
          II = IFTOK(I)
c        W(II) = W(II) +DIAG_IF(I)*V(II)
          DO J =IADIF(I),IADIF(I+1)-1
            K =JDIIF(J)
            KK = IFTOK(K)
            L_K = LT_IF(J)
            W(II) = W(II) + L_K*V(KK)
            W(KK) = W(KK) + L_K*V(II)
          ENDDO
        ENDDO
C-----------------------------
        RETURN
      END
Chd|====================================================================
Chd|  IMP_CPRE                      source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        BUF_DIM                       source/implicit/produt_v.F    
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE IMP_CPRE(IFLAG,NNDL   ,
     1  ELBUF  ,ELBUF_C,BUFMAT ,FSAV   ,VOLMON  ,BUFMAT_C,X     ,X_C   ,
     2  PARTSAV,R_IMP  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr11_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IFLAG,NNDL
C     REAL
        my_real
     .    ELBUF(*) ,ELBUF_C(*),BUFMAT(*) ,FSAV(*),VOLMON(*) ,BUFMAT_C(*),
     .    X(*)    ,X_C(*) ,PARTSAV(*),R_IMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER LI1,LI2,LI3,LI4,LI5
C--------------Iflag= 1->copy; 2 ->restore---------------
        CALL BUF_DIM(LI1,LI2,LI3,LI4)
        IF (IFLAG==1) THEN
          CALL CP_REAL(LI1,ELBUF,ELBUF_C)
          CALL CP_REAL(LI2,BUFMAT,BUFMAT_C)
          CALL CP_REAL(LI3,FSAV,BUFMAT_C(LI2+1))
          CALL CP_REAL(LI4,VOLMON,BUFMAT_C(LI2+LI3+1))
          CALL CP_REAL(NNDL,X,X_C)
          CALL CP_REAL(NPSAV*NPART,PARTSAV,R_IMP(16))
          R_IMP(14) = ENCIN
          R_IMP(15) = ENROT
        ELSEIF (IFLAG==2) THEN
          CALL CP_REAL(LI1,ELBUF_C,ELBUF)
          CALL CP_REAL(LI2,BUFMAT_C,BUFMAT)
          CALL CP_REAL(LI3,BUFMAT_C(LI2+1),FSAV)
          CALL CP_REAL(LI4,BUFMAT_C(LI2+LI3+1),VOLMON)
          CALL CP_REAL(NNDL,X_C,X)
          CALL CP_REAL(NPSAV*NPART,R_IMP(16),PARTSAV)
          ENCIN = R_IMP(14)
          ENROT = R_IMP(15)
        ENDIF
C-----------------------------
        RETURN
      END
Chd|====================================================================
Chd|  IMP_CHECK0                    source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        SPMD_MAX_I                    source/mpi/implicit/imp_spmd.F
Chd|        SPMD_SEND_VR                  source/mpi/implicit/imp_spmd.F
Chd|        SPMD_SUMF_V                   source/mpi/implicit/imp_spmd.F
Chd|        SPMD_SUM_S                    source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE IMP_CHECK0(ITAB  ,NDDL  ,IDDL  ,DIAG_K  ,DIAG_M  ,
     .                      NDOF  ,IKC   ,INLOC ,NDDL0   ,NIR     ,
     .                      NDDLI ,ITOK  ,DIAG_I,IWAR    ,IERR    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
        INTEGER NDDL,NDDL0,NDOF(*),IDDL(*),IKC(*),INLOC(*),ITAB(*),
     .          NIR,IWAR  ,IERR, NDDLI ,ITOK(*)
        my_real
     .    DIAG_K(*),DIAG_M(*),DIAG_I(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NLIM,NN,NKC,NFT,II,IDI,ND,ID,NID,NDMAX
        INTEGER I,J,K,N,IDDLM(NUMNOD)
        CHARACTER  DIR(3)
        DATA DIR/'X','Y','Z'/
        my_real
     .    S
        my_real,
     .           DIMENSION(:,:),ALLOCATABLE :: SR
C------------------------------------------
        IF (IMACH/=3.OR.ISPMD==0) THEN
          WRITE(IOUT,*)
          WRITE(IOUT,*)' ** ZERO STIFFNESS CHECKING **'
          WRITE(IOUT,*)
          WRITE(ISTDO,*)' * ZERO STIFFNESS CHECKING '
        ENDIF
        NID=0
        NIR=0
        DO I=1,NDDL
          DIAG_M(I) = DIAG_K(I)
        ENDDO
        DO I=1,NDDLI
          J=ITOK(I)
          DIAG_M(J)=DIAG_M(J)+DIAG_I(I)
        ENDDO
        IF (IMACH==3.AND.NSPMD>1)CALL SPMD_SUMF_V(DIAG_M)
        DO I=1,NDDL
          IF (DIAG_M(I)<EM10) THEN
            IF (DIAG_M(I)<=EM20) NIR =NIR +1
            NID = NID + 1
          ENDIF
        ENDDO
C
        IF (IMACH==3.AND.NSPMD>1) THEN
          S = NID
          CALL SPMD_SUM_S(S)
          NN = INT(S)
          IF (NN>0) THEN
            S = NIR
            CALL SPMD_SUM_S(S)
            NIR = INT(S)
          ENDIF
        ELSE
          NN = NID
        ENDIF
C
        IERR = IERR + NIR
        IWAR = IWAR + NN-NIR
        IF (IMACH/=3.OR.ISPMD==0) WRITE(IOUT,1000)NN
        IF (NID>0) THEN
          NKC=0
          DO N = 1,NUMNOD
            I=INLOC(N)
            IDDLM(I)=IDDL(I)-NKC
            DO J=1,NDOF(I)
              ND = IDDL(I)+J
              IF (IKC(ND)>0) NKC=NKC+1
            ENDDO
          ENDDO
        ENDIF
        IF (IMACH==3.AND.NSPMD>1) THEN
          NDMAX = NID
          CALL SPMD_MAX_I(NDMAX)
          IF (NID>0) THEN
            II = 0
            ALLOCATE(SR(3,NID))
            DO  N = 1,NUMNOD
              I=INLOC(N)
              IDI=IDDLM(I)
              NKC = 0
              DO J=1,NDOF(I)
                ND = IDDL(I)+J
                IF (IKC(ND)==0) THEN
                  NKC=NKC+1
                  ID = IDI+NKC
                  IF (DIAG_M(ID)<EM10) THEN
                    II = II + 1
                    SR(1,II)=ITAB(I)
                    SR(2,II)=J
                    SR(3,II)=DIAG_M(ID)
                  ENDIF
                ENDIF
              ENDDO
            ENDDO
            CALL SPMD_SEND_VR(
     1         NID       ,3    ,SR       ,NDMAX     ,IOUT     )
            DEALLOCATE(SR)
          ENDIF
        ELSE
C
          IF (NID>0) THEN
            DO  N = 1,NUMNOD
              I=INLOC(N)
              IDI=IDDLM(I)
              NKC = 0
              DO J=1,NDOF(I)
                ND = IDDL(I)+J
                IF (IKC(ND)==0) THEN
                  NKC=NKC+1
                  ID = IDI+NKC
                  IF (DIAG_M(ID)<EM10) THEN
                    IF (J<=3) THEN
                      WRITE(IOUT,1001)ITAB(I),DIR(J),DIAG_M(ID)
                    ELSE
                      WRITE(IOUT,1002)ITAB(I),DIR(J-3),DIAG_M(ID)
                    ENDIF
                  ENDIF
                ENDIF
              ENDDO
            ENDDO
          ENDIF
        ENDIF
C------------------------------------------
        RETURN
 1000   FORMAT(' ND. =',I8,5X,'WITH POSSIBLE FREE STIFFNESS CHECKED',/)
 1001   FORMAT(' NODE NUM. =',I10,5X,'TRA_DIR = ',1A,5X,'VAL.= ',G14.7)
 1002   FORMAT(' NODE NUM. =',I10,5X,'ROT_DIR = ',1A,5X,'VAL.= ',G14.7)
      END
Chd|====================================================================
Chd|  IMP_CHECM0                    source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        FR_DLFT                       source/mpi/implicit/imp_fri.F 
Chd|        SPMD_MAX_I                    source/mpi/implicit/imp_spmd.F
Chd|        SPMD_SEND_VR                  source/mpi/implicit/imp_spmd.F
Chd|        SPMD_SUM_S                    source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE IMP_CHECM0(ITAB  ,NDDL  ,IDDL  ,DIAG_M  ,NDOF  ,
     .                      IKC   ,INLOC ,NDDL0 ,IWAR    ,IERR  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
        INTEGER NDDL,NDDL0,NDOF(*),IDDL(*),IKC(*),INLOC(*),ITAB(*),
     .          IDDIV,IWAR,IERR
        my_real
     .    DIAG_M(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NLIM,NID,NN,NKC,NFT,II,IDI,ND,ID,IW,IR,NDMAX
        INTEGER I,J,K,N,IDDLM(NUMNOD),IDLFT0,IDLFT1
        CHARACTER  DIR(3)
        DATA DIR/'X','Y','Z'/
        my_real
     .    S
        my_real,
     .           DIMENSION(:,:),ALLOCATABLE :: SR
C------------------------------------------
        IF (IMACH/=3.OR.ISPMD==0) THEN
          WRITE(IOUT,*)
          WRITE(IOUT,*)' ** POSITIVE DEFINITE MATRIX CHECKING **'
          WRITE(IOUT,*)
          WRITE(ISTDO,*)' * POSITIVE DEFINITE MATRIX CHECKING '
        ENDIF
        NID=0
        IDLFT0=0
        IR=0
        IF (IMACH==3.AND.NSPMD>1)
     .    CALL FR_DLFT(NDDL,IDLFT0,IDLFT1)
        DO I=1+IDLFT0,NDDL
          IF (DIAG_M(I)<EM12.OR.DIAG_M(I)>EP10) THEN
            IF (DIAG_M(I)<=EM20) IR =IR +1
            NID = NID+ 1
          ENDIF
        ENDDO
C
        IF (IMACH==3.AND.NSPMD>1) THEN
          S = NID
          CALL SPMD_SUM_S(S)
          NN = INT(S)
          IF (NN>0) THEN
            S = IR
            CALL SPMD_SUM_S(S)
            IR = INT(S)
          ENDIF
        ELSE
          NN = NID
        ENDIF
        IERR = IERR + IR
        IWAR = IWAR + NN-IR
        IF (IMACH/=3.OR.ISPMD==0) WRITE(IOUT,1000)NN
        IF (NID>0) THEN
          NKC=0
          DO N = 1,NUMNOD
            I=INLOC(N)
            IDDLM(I)=IDDL(I)-NKC
            DO J=1,NDOF(I)
              ND = IDDL(I)+J
              IF (IKC(ND)>0) NKC=NKC+1
            ENDDO
          ENDDO
        ENDIF
C
        IF (IMACH==3.AND.NSPMD>1) THEN
          NDMAX = NID
          CALL SPMD_MAX_I(NDMAX)
          ALLOCATE(SR(3,NID))
          IF (NID>0) THEN
            II = 0
            DO  N = 1,NUMNOD
              I=INLOC(N)
              IDI=IDDLM(I)
              IF (NDOF(I)>0.AND.IDI>=IDLFT0) THEN
                NKC = 0
                DO J=1,NDOF(I)
                  ND = IDDL(I)+J
                  IF (IKC(ND)==0) THEN
                    NKC=NKC+1
                    ID = IDI+NKC
                    IF (DIAG_M(ID)<EM12.OR.DIAG_M(ID)>EP10) THEN
                      II = II + 1
                      SR(1,II)=ITAB(I)
                      SR(2,II)=J
                      SR(3,II)=DIAG_M(ID)
                    ENDIF
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
          ENDIF
          CALL SPMD_SEND_VR(
     1          NID       ,3    ,SR       ,NDMAX     ,IOUT     )
          DEALLOCATE(SR)
        ELSE
C
          IF (NID>0) THEN
            DO  N = 1,NUMNOD
              I=INLOC(N)
              IDI=IDDLM(I)
              IF (NDOF(I)>0.AND.IDI>=IDLFT0) THEN
                NKC = 0
                DO J=1,NDOF(I)
                  ND = IDDL(I)+J
                  IF (IKC(ND)==0) THEN
                    NKC=NKC+1
                    ID = IDI+NKC
                    IF (DIAG_M(ID)<EM12.OR.DIAG_M(ID)>EP10) THEN
                      IF (J<=3) THEN
                        WRITE(IOUT,1001)ITAB(I),DIR(J),DIAG_M(ID)
                      ELSE
                        WRITE(IOUT,1002)ITAB(I),DIR(J-3),DIAG_M(ID)
                      ENDIF
                    ENDIF
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
          ENDIF
        ENDIF
C------------------------------------------
        RETURN
 1000   FORMAT(' ND. =',I8,5X,'WITH POSSIBLE FREE CONNECTION CHECKED',/)
 1001   FORMAT(' NODE NUM. =',I10,5X,'TRA_DIR = ',1A,5X,'VAL.= ',G14.7)
 1002   FORMAT(' NODE NUM. =',I10,5X,'ROT_DIR = ',1A,5X,'VAL.= ',G14.7)
 1003   FORMAT(/,' LOOK AT CONNECTIVITY FOR NODE NUM. =',I10)
      END
Chd|====================================================================
Chd|  IMP_CHKM                      source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        CFIELD_IMP                    source/loads/general/load_centri/cfield_imp.F
Chd|        DIM_INT_K                     source/implicit/ind_glob_k.F  
Chd|        FORCE_IMP                     source/loads/general/force_imp.F
Chd|        FV_IMP                        source/constraints/general/impvel/fv_imp0.F
Chd|        FV_IMP1                       source/constraints/general/impvel/fv_imp0.F
Chd|        FV_RW                         source/constraints/general/impvel/fv_imp0.F
Chd|        GRAVIT_IMP                    source/loads/general/grav/gravit_imp.F
Chd|        IDEL_INT                      source/implicit/ind_glob_k.F  
Chd|        IMP_CHECK0                    source/implicit/imp_solv.F    
Chd|        IMP_CHECM0                    source/implicit/imp_solv.F    
Chd|        IMP_COMPAB                    source/implicit/imp_solv.F    
Chd|        IMP_COMPABP                   source/implicit/imp_solv.F    
Chd|        IMP_DYNAM                     source/implicit/imp_dyna.F    
Chd|        IMP_FR7I                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FRFV                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FRI                       source/mpi/implicit/imp_fri.F 
Chd|        IMP_GLOB_KHP                  source/implicit/imp_glob_k.F  
Chd|        IMP_INT_K                     source/implicit/imp_int_k.F   
Chd|        IMP_SETB                      source/implicit/imp_setb.F    
Chd|        IND_INT_K                     source/implicit/ind_glob_k.F  
Chd|        INI_K0H                       source/implicit/imp_solv.F    
Chd|        K_BAND                        source/implicit/imp_solv.F    
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        M_LNZ                         source/implicit/imp_solv.F    
Chd|        PRODUT_W                      source/implicit/produt_v.F    
Chd|        PR_INFOK                      source/implicit/imp_solv.F    
Chd|        PVP_K                         source/implicit/imp_solv.F    
Chd|        RGWAL0_IMP                    source/constraints/general/rwall/rgwal0.F
Chd|        SPMD_MAX_I                    source/mpi/implicit/imp_spmd.F
Chd|        SPMD_MIN_S                    source/mpi/implicit/imp_spmd.F
Chd|        SPMD_SUMF_A                   source/mpi/implicit/imp_spmd.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        UPD_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|        UPD_RHS                       source/implicit/upd_glob_k.F  
Chd|        WEIGHTDDL                     source/implicit/recudis.F     
Chd|        ZERO1                         source/system/zero.F          
Chd|        ZEROR                         source/system/zero.F          
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        IMPBUFDEF_MOD                 share/modules/impbufdef_mod.F 
Chd|        IMP_WORKI                     share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|        TH_SURF_MOD                   ../common_source/modules/interfaces/th_surf_mod.F
Chd|====================================================================
      SUBROUTINE IMP_CHKM(
     1  ICODE  ,ISKEW  ,ISKWN  ,IPART  ,IXTG   ,IXS    ,IXQ    ,
     2  IXC    ,IXT    ,IXP    ,IXR    ,IXTG1          ,ITAB   ,ITABM1 ,
     3  NPC    ,IBCL   ,IBFV   ,SENSOR_TAB,NNLINK ,LNLINK ,IPARG  ,IGRV   ,
     4  IPARI  ,INTBUF_TAB,NPRW   ,ICONX  ,NPBY   ,LPBY   ,LRIVET ,
     5  NSTRF  ,LJOINT ,ICODT  ,ICODR  ,ISKY   ,ADSKY  ,IADS_F ,
     6  ILINK  ,LLINK  ,WEIGHT         ,ITASK  ,IBVEL  ,LBVEL  ,FBVEL  ,
     7  X      ,D      ,V      ,VR     ,DR     ,THKE   ,DAMP   ,MS     ,
     8  IN     ,PM     ,SKEW   ,GEO    ,EANI   ,BUFMAT ,BUFGEO ,BUFSF  ,
     9  TF     ,FORC   ,VEL    ,FSAV   ,AGRV   ,FR_WAVE,PARTS0 ,
     A  ELBUF  ,RBY    ,RIVET  ,FR_ELEM,IAD_ELEM,NSENSOR,
     B  WA            ,A      ,AR     ,STIFN  ,STIFR  ,PARTSAV,FSKY   ,
     C  FSKYI  ,IFRAME ,XFRAME ,W16    ,IACTIV ,FSKYM  ,IGEO   ,IPM    ,
     D  TFEXT  ,NODFT  ,NODLT  ,NINT7  ,NUM_IMP,NS_IMP ,NE_IMP ,IND_IMP,
     L  IT     ,RWBUF  ,LPRW   ,FR_WALL,NBINTC ,INTLIST,FOPT   ,RWSAV  ,
     M  FSAVD  ,DIRUL  ,LGRAV  ,IRBE3  ,LRBE3  ,FRBE3  ,
     N  FRWL6  ,IRBE2  ,LRBE2  ,ICFIELD,LCFIELD,CFIELD,ELBUF_TAB,WEIGHT_MD,
     O  STACK  ,DIMFB  ,FBSAV6 ,STABSEN,TABSENSOR,DRAPE_SH4N,DRAPE_SH3N,H3D_DATA,
     P  NDDL0  ,NNZK0  ,IMPBUF_TAB,CPTREAC,FTHREAC,NODREAC,DRAPEG,TH_SURF ,
     Q  FSAVSURF,NSEG_LOADP,DPL0CLD,VEL0CLD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE DSGRAPH_MOD
        USE IMP_WORKI
        USE ELBUFDEF_MOD
        USE INTBUFDEF_MOD
        USE STACK_MOD
        USE H3D_MOD
        USE IMPBUFDEF_MOD
        USE SENSOR_MOD
        USE DRAPE_MOD
        USE TH_SURF_MOD , ONLY : TH_SURF_
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#if defined(MUMPS5)
#include "dmumps_struc.h"
#endif
#include "timeri_c.inc"
#include "impl1_c.inc"
#include "impl2_c.inc"
#include "param_c.inc"
#include "com01_c.inc"
#include "com04_c.inc"
#include "scr05_c.inc"
#include "units_c.inc"
#include "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ,INTENT(IN) :: NSENSOR
        INTEGER ITASK,ICODE(*), ISKEW(*), ISKWN(LISKN,*),ITABM1(*),
     .     IPART(*),IXS(*),IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*),
     .     IXP(NIXP,*),IXR(NIXR,*), IXTG(NIXTG,*), IXTG1(4,*),
     .     ITAB(*),NPC(*), IBCL(*), IBFV(*),IPARG(NPARG,*),IPARI(NPARI,*),
     .     NPRW(*), NPBY(NNPBY,*), LPBY(*),IADS_F(*),
     .     LRIVET(*), NSTRF(*), LJOINT(*), ICODT(*), ICODR(*), ILINK(*),
     .     LLINK(*),ISKY(*),ADSKY(*),
     .     NNLINK(10,*),LNLINK(*),IGRV(*),LGRAV(*),
     .     WEIGHT(*),IFRAME(LISKN,*),IBVEL(NBVELP,*),LBVEL(*),
     .     IACTIV(*),IGEO(*),IPM(*),ICONX(*),NODFT  ,NODLT,IT,
     .     ICFIELD(*),LCFIELD(*),WEIGHT_MD(*),
     .     DIMFB,STABSEN,TABSENSOR(*),CPTREAC,NODREAC(*)
        INTEGER LPRW(*), FR_WALL(NSPMD+2,*), FR_ELEM(*),
     .     IAD_ELEM(2,*),NBINTC ,INTLIST(*),DIRUL(*)
C     REAL
        my_real
     .     X(3,*)    ,D(3,*)      ,V(3,*)   ,VR(3,*),DAMP(*),
     .     MS(*)   ,IN(*)   ,PM(NPROPM,*),SKEW(LSKEW,*),GEO(NPROPG,*),
     .     BUFMAT(*) ,TF(*) ,FORC(*)  ,VEL(*),FSAV(NTHVKI,*) ,ELBUF(*) ,
     .     RWBUF(NRWLP,*),RWSAV(*),RBY(NRBY,*),
     .     RIVET(*),WA(*), A(3,*) ,AR(3,*),PARTSAV(*) ,TFEXT,
     .     STIFN(*) ,STIFR(*),FSKY(*),FSKYI(*),DR(3,*),
     .     EANI(*),AGRV(*), THKE(*),FR_WAVE(*),PARTS0(*),BUFGEO(*),
     .     XFRAME(NXFRAME,*),W16(*),FBVEL(*),FSKYM(*),BUFSF(*),
     .     FOPT(6,*),FSAVD(NTHVKI,*),CFIELD(*),FRBE3(*),
     .     FTHREAC(6,*)
        INTEGER  NDDL0,NNZK0,NINT7
        INTEGER  NUM_IMP(*),NS_IMP(*),NE_IMP(*),IND_IMP(*),
     .           IRBE3(*),LRBE3(*),IRBE2(*),LRBE2(*)
        TYPE (DRAPE_) :: DRAPE_SH4N(NUMELC_DRAPE), DRAPE_SH3N(NUMELTG_DRAPE)
        TYPE (DRAPEG_) :: DRAPEG
        my_real, INTENT(IN) :: 
     .   DPL0CLD(6,NCONLD),VEL0CLD(6,NCONLD)
C     REAL
c      my_real
c     .  DIAG_K(*),LT_K(*),DD(*) ,DDR(*) ,DIAG_M(*),LT_M(*),LB(*),LB0(*),
c     .  D_IMP(3,*),DR_IMP(3,*),X_C(*) ,BUFMAT_C(*) ,
c     .  BKUD(*),R_IMP(*),AC(3,*),ACR(3,*),FRBE3(*)
        DOUBLE PRECISION
     .          FRWL6(*)
        DOUBLE PRECISION
     .          FBSAV6(12,6,DIMFB)
        TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
        TYPE (STACK_PLY) :: STACK
        TYPE(H3D_DATABASE) :: H3D_DATA
        TYPE (IMPBUF_STRUCT_) ,TARGET :: IMPBUF_TAB
        TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
        TYPE (TH_SURF_) , INTENT(IN) :: TH_SURF
        my_real, INTENT(INOUT) :: FSAVSURF(5,NSURF)
        INTEGER, INTENT(INOUT) :: NSEG_LOADP(NSURF)
#if defined(MUMPS5)
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER  NNMAX,NKMAX,N_IMPN,N_IMPM,LNSS,LNSS2,NDT,NDS,NT_RW
        INTEGER I,J,NTMP,L1,L2,L3,NNDL,INPRINT,ISTOP,LI10,LI11,
     .          LI1,LI2,LI3,LI4,LI5,LI6,LI7,LI8,LI9,LIF,IC,ISETP,
     .          LI12,LNSS3,LI13,LI14,LI15,LNSB2,LNSRB2
C      INTEGER, DIMENSION(:),ALLOCATABLE :: IADI,JDII,ITOK
        INTEGER, DIMENSION(:),ALLOCATABLE :: NSS,ISS,NSS2,ISS2,NSS3,ISS3
        INTEGER, DIMENSION(:),ALLOCATABLE :: NSB2,ISB2,IAINT2
        INTEGER  NNOD,IFDIS,N1,N2,N3
        INTEGER LBAND,NCL_MAX,IRFLAG,IBID
        my_real
     .  TFEXC,TMP,TMP1,TMP2,R2,BFAC,FACI,R02,GAP,RBID,WE_IMP,LAMDA,DUMMY_FEXT(3,1)
C      my_real,
C     .         DIMENSION(:),ALLOCATABLE :: DIAG_I,LT_I
        INTEGER, POINTER     :: NDDL,NNZK,NRBYAC,NINT2,NMC,NMC2,NMONV
        INTEGER, DIMENSION(:) ,POINTER     :: IADK,JDIK,IADM,JDIM
        INTEGER, DIMENSION(:) ,POINTER     :: IDDL,NDOF,INLOC,LSIZE,I_IMP,IKC,
     .                                        IRBYAC,NSC,IINT2,NKUD,IMONV,
     .                                        IKINW,W_DDL,IKUD,NDOFI,IDDLI
        my_real, DIMENSION(:) ,POINTER     :: DIAG_K,LT_K,DIAG_M,LT_M,LB,
     .                                        LB0,BKUD,D_IMP,ELBUF_C,BUFMAT_C,
     .                                        DR_IMP,X_C,DD,DDR,X_A,R_IMP
        my_real, DIMENSION(:) ,POINTER     :: FEXT,DG,DGR,DG0,DGR0,BUFIN_C,AC,ACR
        TYPE(PRGRAPH) :: GRAPHE(1)
#ifdef MUMPS5
        TYPE(DMUMPS_STRUC) MUMPS_PAR
#else
        ! Fake declaration as DMUMPS_STRUC is shipped with MUMPS
        INTEGER MUMPS_PAR 
#endif
  
C------------------------------
C        Initialisation
C-----------------------------------------------
        DUMMY_FEXT = ZERO
        NDDL => IMPBUF_TAB%NDDL
        NNZK => IMPBUF_TAB%NNZK
        NRBYAC => IMPBUF_TAB%NRBYAC
        NINT2 => IMPBUF_TAB%NINT2
        NMC => IMPBUF_TAB%NMC
        NMC2 => IMPBUF_TAB%NMC2
        NMONV => IMPBUF_TAB%NMONV
        IADK => IMPBUF_TAB%IADK
        JDIK => IMPBUF_TAB%JDIK
        IDDL => IMPBUF_TAB%IDDL
        NDOF => IMPBUF_TAB%NDOF
        INLOC => IMPBUF_TAB%INLOC
        LSIZE => IMPBUF_TAB%LSIZE
        I_IMP => IMPBUF_TAB%I_IMP
        IRBYAC => IMPBUF_TAB%IRBYAC
        NSC => IMPBUF_TAB%NSC
        IINT2 => IMPBUF_TAB%IINT2
        NKUD => IMPBUF_TAB%NKUD
        IMONV => IMPBUF_TAB%IMONV
        IKINW => IMPBUF_TAB%IKINW
        IKC => IMPBUF_TAB%IKC
        W_DDL => IMPBUF_TAB%W_DDL
        IKUD => IMPBUF_TAB%IKUD
        IADM => IMPBUF_TAB%IADM
        JDIM => IMPBUF_TAB%JDIM
        IDDLI => IMPBUF_TAB%IDDLI
        NDOFI => IMPBUF_TAB%NDOFI
C
        DIAG_K  =>IMPBUF_TAB%DIAG_K
        LT_K    =>IMPBUF_TAB%LT_K
        DIAG_M  =>IMPBUF_TAB%DIAG_M
        LT_M    =>IMPBUF_TAB%LT_M
        LB      =>IMPBUF_TAB%LB
        LB0     =>IMPBUF_TAB%LB0
        BKUD    =>IMPBUF_TAB%BKUD
        D_IMP   =>IMPBUF_TAB%D_IMP
        ELBUF_C =>IMPBUF_TAB%ELBUF_C
        BUFMAT_C=>IMPBUF_TAB%BUFMAT_C
        X_C     =>IMPBUF_TAB%X_C
        DD      =>IMPBUF_TAB%DD
        DDR     =>IMPBUF_TAB%DDR
        FEXT  =>IMPBUF_TAB%FEXT
        DG    =>IMPBUF_TAB%DG
        DGR   =>IMPBUF_TAB%DGR
        DG0   =>IMPBUF_TAB%DG0
        DGR0  =>IMPBUF_TAB%DGR0
        DR_IMP=>IMPBUF_TAB%DR_IMP
c                 BUFIN_C=>IMPBUF_TAB%BUFIN_C
        AC=>IMPBUF_TAB%AC
        ACR=>IMPBUF_TAB%ACR
        R_IMP => IMPBUF_TAB%R_IMP
        ALLOCATE(IAINT2(NINT2))
        NDDLI=0
        ISTOP=0
        INEGA=0
        NNDL = 3*NUMNOD
        NSREM=0
        NSL=0
        ISETP = 1
        IMP_IW = 0
        IMP_IR = 0
        RBID=ZERO
C
        IF (IRREF>0.AND.IMCONV==1.AND.ILINE/=1) THEN
          IRFLAG=IRREF
        ELSE
          IRFLAG=0
        ENDIF
citask0       IF (ITASK == 0) THEN
C
        CALL ZEROR(D_IMP,NUMNOD)
        IF (IRODDL/=0) CALL ZEROR(DR_IMP,NUMNOD)
        CALL ZEROR(AC,NUMNOD)
        IF (IRODDL/=0) CALL ZEROR(ACR,NUMNOD)
C----------------------------------
C       FORCES EXTERNES A=Fext-Fint
C----------------------------------
        NCL_MAX=0
        R_IMP(16)=ZERO
        IF(NCONLD/=0) THEN
          CALL FORCE_IMP(IBCL  ,FORC  ,NPC   ,TF  ,AC    ,
     2               V     ,X     ,SKEW  ,ACR ,VR    ,
     3               NSENSOR,SENSOR_TAB,TFEXC,
     4               IADS_F,FSKY  ,FSKY, DUMMY_FEXT,H3D_DATA,
     5               CPTREAC,FTHREAC,NODREAC,TH_SURF ,FSAVSURF,
     6               NSEG_LOADP,DPL0CLD ,VEL0CLD ,D       ,DR      ,
     7               NCONLD  ,NUMNOD    ,NSURF,NFUNCT)
          IF (IMACH==3.AND.NSPMD>1) THEN
            DO I=IAD_ELEM(1,1),IAD_ELEM(1,NSPMD+1)-1
              J = FR_ELEM(I)
              N1 = 3*(J-1)+1
              N2 = 3*(J-1)+2
              N3 = 3*(J-1)+3
              TMP = ABS(AC(N1))+ABS(AC(N2))+ABS(AC(N3))
              IF (IRODDL/=0) TMP = TMP +
     .              ABS(ACR(N1))+ABS(ACR(N2))+ABS(ACR(N3))
              IF (TMP>ZERO) NCL_MAX = NCL_MAX + 1
            ENDDO
          ENDIF
        ENDIF
        IF (IMACH==3.AND.NSPMD>1) THEN
          CALL SPMD_MAX_I(NCL_MAX)
          IF (NCL_MAX>0) THEN
            LBAND = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
            IF (IRODDL/=0) THEN
              NTMP = 6
            ELSE
              NTMP = 3
            ENDIF
            CALL SPMD_SUMF_A(AC,ACR,IAD_ELEM,FR_ELEM,NTMP,LBAND)
          ENDIF
        ENDIF
        IF(NGRAV/=0) THEN
c          CALL MY_BARRIER
          CALL GRAVIT_IMP(IGRV  ,AGRV  ,NPC   ,TF    ,AC,
     2                    V     ,X     ,SKEW  ,MS,TFEXC,
     3                    NSENSOR,SENSOR_TAB,WEIGHT,
     4                    LGRAV ,ITASK,
     5                    NRBYAC,IRBYAC,NPBY  ,RBY    )
c          CALL MY_BARRIER
        ENDIF
        IF(NLOADC/=0) THEN
          CALL CFIELD_IMP(ICFIELD  ,CFIELD,NPC   ,TF    ,AC,
     2                    V     ,X     ,XFRAME  ,MS,TFEXC,
     3                    NSENSOR,SENSOR_TAB,WEIGHT,IFRAME,
     4                    LCFIELD ,ITASK,
     5                    NRBYAC,IRBYAC,NPBY  ,RBY,ISKWN    )
        ENDIF
C-------------dU_d---------------------------------
        IF(NFXVEL/=0.AND.IMCONV==1) THEN
          CALL FV_IMP(IBFV  ,NPC    ,TF     ,VEL   ,SENSOR_TAB,
     1                D_IMP ,DR_IMP ,IKC    ,IDDL  ,NSENSOR   ,
     2                SKEW  ,IFRAME ,XFRAME ,V     ,VR    ,
     3                X     ,DIRUL  ,NDOF   ,A     ,VR    )
        ENDIF
C-------------U_d--> rigid wall-------------------------------
        NT_RW=0
        NRWDONE = 0
        IF (NRWALL>0) THEN
          CALL RGWAL0_IMP(
     1      X           ,D_IMP    ,V      ,RWBUF   ,LPRW    ,
     2      NPRW        ,MS       ,FSAV(1,NINTER+1),FR_WALL ,
     3      FOPT        ,RWSAV    ,WEIGHT ,FSAVD(1,NINTER+1),
     4      NT_RW       ,IDDL     ,IKC    ,IMCONV,NDOF,FRWL6,
     5      WEIGHT_MD   ,DIMFB    , FBSAV6,STABSEN,TABSENSOR)
          IF(NT_RW>0) CALL FV_RW(IDDL   ,IKC   ,NDOF  ,D_IMP  ,V )
        ENDIF
        IFDIS=NT_RW+NFXVEL
        IF(IFDIS>0.AND.IMCONV==1) THEN
          IF(NT_RW>0) THEN
            DO I=1,NDDL0
              IF (IKC(I)==3) IKC(I)=4
              IF (IKC(I)==10) IKC(I)=11
            ENDDO
          ENDIF
        ENDIF
        NTMP = NT_RW
        IF (IMACH==3.AND.NSPMD>1) CALL SPMD_MAX_I(NTMP)
        IF(NTMP>0) THEN
          IF(IMACH/=3.OR.ISPMD==0) THEN
            WRITE(IOUT,*)'  *--------- RIGID WALL IMPACT---------*'
          ENDIF
        ENDIF
C----------------------------------
C----------------------------------
        CALL IMP_SETB(AC    ,ACR     ,IDDL   ,NDOF  ,LB    )
C----------------------------------
C  CHECKING
C----------------------------------
        IF (ISPMD==0) THEN
          WRITE(ISTDO,*)
          WRITE(ISTDO,*)' ** BEGIN IMPLICIT MODEL CHECKING **'
          WRITE(IOUT,*)
          WRITE(IOUT,*)' ** BEGIN IMPLICIT MODEL CHECKING **'
          WRITE(IOUT,*)
        ENDIF
        IF (NSPMD>1) THEN
          CALL IMP_COMPABP(
     1     ICODT     ,ICODR     ,ISKEW     ,IBFV      ,NPC       ,
     2     TF        ,VEL       ,NSENSOR   ,SENSOR_TAB,XFRAME    ,
     3     RBY       ,X         ,SKEW      ,LPBY      ,NPBY      ,
     4     ITAB      ,NRBYAC    ,IRBYAC    ,NINT2     ,IINT2     ,
     5     IPARI     ,INTBUF_TAB,NT_RW     ,NDDL      ,
     6     NDOF      ,IKC       ,INLOC     ,IDDL      ,NDDL0     ,
     7     IMP_IW    ,IMP_IR    )
        ELSE
          CALL IMP_COMPAB(
     1     ICODT     ,ICODR     ,ISKEW     ,IBFV      ,NPC       ,
     2     TF        ,VEL       ,NSENSOR   ,SENSOR_TAB,XFRAME    ,
     3     RBY       ,X         ,SKEW      ,LPBY      ,NPBY      ,
     4     ITAB      ,NRBYAC    ,IRBYAC    ,NINT2     ,IINT2     ,
     5     IPARI     ,INTBUF_TAB,NT_RW     ,NDDL      ,
     6     NDOF      ,IKC       ,INLOC     ,IDDL      ,NDDL0     ,
     7     IMP_IW    ,IMP_IR    )
        ENDIF

citask0       END IF !(ITASK == 0) THEN
C----------------------------------
C       MATRICE DE RIGIDITE
C----------------------------------
        IF (ISETK==1) THEN
          IF (IMACH/=3.OR.ISPMD==0) THEN
            WRITE(ISTDO,*)' * FINIT ELEMENT CHECKING '
            WRITE(IOUT,*)' ** FINIT ELEMENT CHECKING **'
            WRITE(IOUT,*)
          ENDIF
          IF (IMON>0 .AND. ITASK ==0) CALL STARTIME(31,1)
          NDDL = NDDL0
          NNZK = NNZK0
          NNMAX=LSIZE(9)
          NKMAX=LSIZE(10)
          NMC2=LSIZE(11)
c        IF (ITASK == 0) THEN
          CALL ZERO1(DIAG_K,NDDL)
          CALL ZERO1(LT_K,NNZK)
c        END IF
          L1 = 1+NIXS*NUMELS
          L2 = L1+6*NUMELS10
          L3 = L2+12*NUMELS20
          LI1 =1
          LI2 = LI1+LSIZE(4)
          LI3 = LI2+LSIZE(5)
          LI4 = LI3+LSIZE(1)
          LI5 = LI4+LSIZE(3)
          LI6 = LI5+LSIZE(7)
          LI7 = LI6+LSIZE(2)
          LI8 = LI7+LSIZE(6)
          LI9 = LI8+NINT2
          LI10 = LI9+LSIZE(8)
          LI11 = LI10+(LSIZE(8)-LCOKM)*LSIZE(9)
          LI12 = LI11+LCOKM*LSIZE(10)
          LI13 = LI12+4*LSIZE(11)
          LI14 = LI13+LSIZE(14)
          LI15 = LI14+LSIZE(15)
          LIF = LI15+LSIZE(16)
C    /---------------/
c      CALL MY_BARRIER
C    /---------------/
          CALL IMP_GLOB_KHP(
     1     PM        ,GEO       ,IPM       ,IGEO      ,ELBUF     ,
     2     IXS       ,IXQ       ,IXC       ,IXT       ,IXP       ,
     3     IXR       ,IXTG      ,IXTG1     ,IXS(L1)   ,
     4     IXS(L2)   ,IXS(L3)   ,IPARG     ,TF        ,NPC       ,
     5     FR_WAVE   ,W16       ,BUFMAT    ,THKE      ,BUFGEO    ,
     6     RBY       ,SKEW      ,X         ,
     7     WA        ,IDDL      ,NDOF      ,DIAG_K    ,LT_K      ,
     8     IADK      ,JDIK      ,IKG       ,IBID      ,ITASK     ,
     9     ELBUF_TAB ,STACK     ,DRAPE_SH4N, DRAPE_SH3N   ,DRAPEG   )
          NDDL_L = NDDL
C    /---------------/
c      CALL MY_BARRIER
C    /---------------/
citask0         IF (ITASK == 0) THEN
C------ 1er VP: for dt check
          IF (IMPDEB>0.AND.NDDL<1000) THEN
            CALL PVP_K(NDDL,IADK,JDIK,IDDL ,INLOC,
     .                 NDOF,ITAB,DIAG_K,LT_K ,LAMDA, J , MS  )
            tmp = TWO*sqrt(ONE/LAMDA)
            write(iout,*) 'critical DT =',tmp
          END IF
          IF (IDYNA>0.OR.IQSTAT>0)
     .    CALL IMP_DYNAM(NODFT  ,NODLT   ,IDDL   ,NDOF   ,DIAG_K ,
     .                   MS     ,IN      ,HHT_A  ,WEIGHT ,IADK   ,
     .                   LT_K   )
          CALL UPD_GLOB_K(
     1    ICODT     ,ICODR     ,ISKEW     ,IBFV      ,NPC       ,
     2    TF        ,VEL       ,XFRAME    ,
     3    RBY       ,X         ,SKEW      ,LPBY      ,NPBY      ,
     4    ITAB      ,WEIGHT    ,MS        ,IN        ,NRBYAC    ,
     5    IRBYAC    ,NSC       ,IKINW(LI1),NMC       ,IKINW(LI2),
     6    IKINW(LI3),IKINW(LI4),NINT2     ,IINT2     ,IKINW(LI8),
     7    IKINW(LI5),IKINW(LI6),IKINW(LI7),IPARI     ,INTBUF_TAB,
     8    NDDL      ,NNZK      ,IADK      ,JDIK      ,
     9    DIAG_K    ,LT_K      ,NDOF      ,IDDL      ,IKC       ,
     A    D_IMP     ,LB        ,NKUD      ,IKUD      ,BKUD      ,
     B    NMC2      ,IKINW(LI12),NT_RW    ,DR_IMP    ,DIRUL     ,
     C    IRBE3     ,LRBE3     ,FRBE3     ,IKINW(LI13),IRBE2    ,
     D    LRBE2     ,IKINW(LI14),IKINW(LI15))
C
          IF (IMACH==3.AND.NSPMD>1) THEN
            CALL UPD_FR_K(
     1      IADK     ,JDIK     ,NDOF      ,IKC      ,IDDL     ,
     2      INLOC    ,FR_ELEM  ,IAD_ELEM  ,NDDL     )
C
            CALL WEIGHTDDL(IDDL  ,NDOF  ,IKC  ,WEIGHT ,W_DDL  ,INLOC )
          ENDIF
C
          CALL PR_INFOK(NDDL0,NNZK0,NDDL,NNZK,MAX(NNMAX,NKMAX))
C
          IF (IPREC>4.AND.NKMAX>200) THEN
            CALL K_BAND(NDDL,IADK,JDIK,IBID)
            MAXB = MIN(MAXB,IBID)
C
            IF (MAXB>10000) THEN
              CALL M_LNZ(NDDL,IADK,JDIK,MAXB,MAX_L)
            ENDIF
C
          ENDIF

          CALL INI_K0H(NDDL,NNZK,NNZK,IADK,JDIK)
          IF (IMON>0) CALL STOPTIME(31,1)
citask0        END IF !(ITASK == 0) THEN
        ENDIF

citask0        IF (ITASK == 0) THEN
C----------------------------------
C       MATRICE DE RIGIDITE D'INTERFACE
C----------------------------------
        GAP=EP20
        IF (NINT7>0) THEN
          L1=LSIZE(1)
          L2=LSIZE(2)
          LNSS2=0
          LNSS=0
          IF (IMON>0) CALL STARTIME(31,1)
          IF (IMP_INT==1) CALL IDEL_INT(
     1    IPARI     ,INTBUF_TAB,NUM_IMP   ,NS_IMP    ,NE_IMP    ,
     2    IND_IMP   ,NDOF      ,NINT7     )
          CALL DIM_INT_K(
     1    IPARI     ,INTBUF_TAB,NUM_IMP   ,NS_IMP    ,NE_IMP    ,
     2    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     3    LNSS      ,NINT2     ,IINT2     ,IAINT2    ,LNSS2     ,
     4    NDDLI     ,NNZI      ,IDDLI     ,NDOFI     ,N_IMPN    ,
     5    N_IMPM    ,NNMAX     ,NKMAX     ,NDOF      ,NSREM     ,
     6    IRBE3     ,LRBE3     ,LNSS3     ,IRBE2     ,LRBE2     ,
     7    LNSB2     ,LNSRB2    ,IND_IMP   )
          ALLOCATE(IADI(NDDLI+1))
          ALLOCATE(ITOK(NDDLI))
          ALLOCATE(JDII(NNZI))
          ALLOCATE(NSS2(L2),NSS3(NRBE3),NSB2(LNSRB2))
          NSB2=0
          ALLOCATE(ISS2(LNSS2),ISS3(LNSS3),ISB2(LNSB2))
          ALLOCATE(NSS(L1))
          ALLOCATE(ISS(LNSS))
          DO I=1,L1
            NSS(I)=0
          ENDDO
          CALL IND_INT_K(
     1    IPARI     ,INTBUF_TAB,NUM_IMP   ,NS_IMP    ,NE_IMP      ,
     2    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC      ,
     3    NSS       ,ISS       ,NINT2     ,IINT2     ,NSS2        ,
     4    ISS2      ,NDDLI     ,NNZI      ,IADI      ,JDII        ,
     5    IDDLI     ,NDOFI     ,N_IMPN    ,ITOK      ,IDDL        ,
     6    NNMAX     ,NKMAX     ,N_IMPM    ,NDOF      ,IAINT2      ,
     7    IRBE3     ,LRBE3     ,NSS3      ,ISS3      ,IRBE2       ,
     8    LRBE2     ,NSB2      ,ISB2      ,IND_IMP   )
          ALLOCATE(DIAG_I(NDDLI))
          ALLOCATE(LT_I(NNZI))
          CALL ZERO1(DIAG_I,NDDLI)
          CALL ZERO1(LT_I,NNZI)
          IF (NSREM>0)
     1      CALL IMP_FR7I(IPARI ,INTBUF_TAB  ,NUM_IMP ,NS_IMP ,NSREM ,
     2                    NBINTC,INTLIST)
Ctmp-------A n'est pas modifie ici -------------------
          CALL IMP_INT_K(A     ,V         ,
     1    ICODT     ,ICODR     ,ISKEW     ,IBFV      ,NPC       ,
     2    TF        ,VEL       ,NSENSOR   ,SENSOR_TAB,XFRAME    ,
     3    RBY       ,X         ,SKEW      ,LPBY      ,NPBY      ,
     4    ITAB      ,WEIGHT    ,MS        ,IN        ,NRBYAC    ,
     5    IRBYAC    ,NSS       ,ISS       ,IPARI     ,INTBUF_TAB,
     6    NINT2     ,IINT2     ,IAINT2    ,NSS2      ,
     7    ISS2      ,NDDLI     ,NNZI      ,IADI      ,JDII      ,
     8    DIAG_I    ,LT_I      ,IDDLI     ,NDDL0     ,IADK      ,
     9    JDIK      ,IKC       ,DIAG_K    ,LT_K      ,IDDL      ,
     A    NUM_IMP   ,NS_IMP    ,NE_IMP    ,IND_IMP   ,NDOFI     ,
     B    ITOK      ,D_IMP     ,LB        ,GAP       ,DIRUL     ,
     C    NT_RW     ,RBID      ,IRBE3     ,LRBE3     ,FRBE3     ,
     D    NSS3      ,ISS3      ,IRBE2     ,LRBE2     ,NSB2      ,
     E    ISB2      )
C
          DEALLOCATE(NSS2,NSS3,NSB2)
          DEALLOCATE(ISS2,ISS3,ISB2)
          DEALLOCATE(NSS)
          DEALLOCATE(ISS)
          IF (NSPMD==1.AND.IMCONV>=0.AND.
     .         (LPRINT/=0.OR.NPRINT/=0)) THEN
            WRITE(IOUT,1006)
c            WRITE(ISTDO,1006)
            WRITE(IOUT,1007)NDDLI,NNZI,NNMAX
c            WRITE(ISTDO,1007)NDDLI,NNZI,NNMAX
            WRITE(IOUT,*)
c            WRITE(ISTDO,*)
          ENDIF
          IF (IMON>0) CALL STOPTIME(31,1)
        ENDIF
C----------------------------------
        IF (NFXVEL/=0.AND.IMCONV==1) THEN
          CALL FV_IMP1(NKUD   ,IKUD    ,BKUD    ,LB    )
        ENDIF
C-------------LB,A,AR devient Fext-Fint---------------------
        CALL UPD_RHS(ICODT ,ICODR ,ISKEW ,IBFV    ,XFRAME ,
     1               RBY   ,X     ,SKEW   ,LPBY   ,NPBY   ,
     2               NRBYAC,IRBYAC,NINT2  ,IINT2  ,IPARI  ,
     3               INTBUF_TAB   ,NDOF   ,IDDL   ,IKC    ,
     4               NDDL0 ,LB    ,ISETK  ,INLOC  ,DIRUL  ,
     5               A     ,AR    ,AC     ,ACR    ,NT_RW  ,
     6               IRFLAG,W_DDL ,NDDL   ,R_IMP(1),IDYNA ,
     7               V     ,VR    ,MS     ,IN     ,IRBE3  ,
     8               LRBE3 ,FRBE3 ,WEIGHT ,IRBE2  ,LRBE2  )

        IF (IMACH==3.AND.NSPMD>1) THEN
          IF (NBINTC>0.) THEN
            ICONTA = NDDLI + NSREM
            CALL SPMD_MAX_I(ICONTA)
            IF (ICONTA> 0) THEN
              CALL IMP_FRI(
     1        NUM_IMP   ,NS_IMP    ,NE_IMP    ,IPARI     ,INTBUF_TAB,
     2        NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,
     3        IRBYAC    ,NINT2     ,IINT2     ,IDDL      ,IKC       ,
     4        NDOF      ,INLOC     ,NSREM     ,NSL       ,NBINTC    ,
     5        INTLIST   ,X         ,IBFV      ,DIRUL     ,SKEW      ,
     6        XFRAME    ,ISKEW     ,ICODT     ,A         ,D_IMP     ,
     7        LB        ,IFDIS     ,NDDL      ,DR_IMP    ,IDDLI     ,
     8        IRBE3     ,LRBE3     ,FRBE3     ,IRBE2  ,LRBE2 )
              CALL SPMD_MIN_S(GAP)
              CALL SPMD_MAX_I(IFDIS)
              IF ((NSREM+NSL)>0.AND.IFDIS>0)
     .          CALL IMP_FRFV(
     1        NUM_IMP   ,NS_IMP    ,NE_IMP    ,IPARI     ,INTBUF_TAB,
     2        IDDL      ,IKC       ,NDOF      ,NSREM     ,
     3        NSL       ,D_IMP     ,DD        ,DR_IMP    ,DDR       ,
     4        A         ,AR        ,MS        ,V         ,X         ,
     5        LB        ,NDDL      ,IBFV      ,SKEW      ,XFRAME    ,
     6        IRBE3     ,LRBE3     ,IRBE2     ,LRBE2     ,R_IMP(16) ,
     7        NDDL0     ,W_DDL     )
            ENDIF
          ENDIF
        ENDIF
        IF (GAP<ZERO) THEN
          IMCONV = -2
          IF (IMACH/=3.OR.ISPMD==0) THEN
            WRITE(IOUT,1009)INT(-GAP)
            WRITE(ISTDO,1009)INT(-GAP)
          ENDIF
        ENDIF
C----------------------------------
C       IMPLICIT [K] CHECK
C----------------------------------
        CALL IMP_CHECK0(ITAB  ,NDDL  ,IDDL  ,DIAG_K  ,DIAG_M  ,
     .                  NDOF  ,IKC   ,INLOC ,NDDL0   ,INEGA   ,
     .                  NDDLI ,ITOK  ,DIAG_I,IMP_IW  ,IMP_IR )
        IF (INEGA>0) GOTO 100
        NTMP=0
        CALL PRODUT_W(NDDL,LB,LB,W_DDL,R2)
        IF (R2>ZERO.AND.R2<EP30) THEN
        ELSEIF(IQSTAT>0) THEN
          WRITE(IOUT,*)
          WRITE(IOUT,*)' ** WARNING :IMPLICIT LOADING DATA **'
          IMP_IW = IMP_IW + 1
        ELSE
          WRITE(IOUT,*)
          WRITE(IOUT,*)' ** ERROR :IMPLICIT LOADING DATA **'
          IMP_IR = IMP_IR + 1
        ENDIF

citask0         END IF !(ITASK == 0) THEN
C
C    /---------------/
c      CALL MY_BARRIER
C    /---------------/
        CALL LIN_SOLV(NDDL  ,IDDL  ,NDOF   ,IKC   ,D_IMP ,
     1                DR_IMP,L_TOL ,NNZK   ,IADK  ,JDIK  ,
     2                DIAG_K,LT_K   ,NDDLI  ,IADI  ,JDII  ,
     3                DIAG_I,LT_I   ,ITOK   ,IADM  ,JDIM  ,
     4                DIAG_M,LT_M   ,LB    ,R_IMP(6),INLOC ,
     5                FR_ELEM,IAD_ELEM,W_DDL,ITASK ,ISETP  ,
     6                ISTOP ,A     ,AR     ,V    ,
     7                MS    ,X     ,IPARI ,INTBUF_TAB   ,
     8                NUM_IMP,NS_IMP,NE_IMP,NSREM ,NSL  ,
     9                NTMP  ,GRAPHE, ITAB  ,RBID  ,IBID ,
     A                IBID  ,NTMP  ,IBID   ,IBID  ,IBID ,
     B                IBID  ,RBID   ,IBFV  ,SKEW  ,
     C                XFRAME,MUMPS_PAR,IBID,IBID  ,RBID ,
     D                IRBE3 ,LRBE3 ,IRBE2  ,LRBE2 )
c       IF (ITASK == 0) THEN
        CALL IMP_CHECM0(ITAB  ,NDDL  ,IDDL  ,DIAG_M  ,NDOF  ,
     .                  IKC   ,INLOC ,NDDL0 ,IMP_IW  ,IMP_IR)
        IF (NINT7>0) THEN
          DEALLOCATE(IADI)
          DEALLOCATE(ITOK)
          DEALLOCATE(JDII)
          DEALLOCATE(DIAG_I)
          DEALLOCATE(LT_I)
        ENDIF
c       END IF !(ITASK == 0) THEN

 100    CONTINUE
C    /---------------/
        CALL MY_BARRIER
C    /---------------/
        IF (ISPMD == 0 .AND. ITASK == 0) THEN
          WRITE(ISTDO,1011)IMP_IR,IMP_IW
        ENDIF

 1001   FORMAT(' SYMBOLIC DIM : NDDL =',I8,1X,'NNZ =',I8,1X,'NB_MAX =',I8)
 1002   FORMAT(' FINAL    DIM : NDDL =',I8,1X,'NNZ =',I8,1X,'NB_MAX =',I8)
 1006   FORMAT(5X,'--SUPPLEMENTARY STIFFNESS MATRIX',
     .         1X, 'DUE TO INTERFACE IS CREATED --')
 1007   FORMAT(5X,' WITH DIM. : ND   =',I8,1X,'NZ  =',I8,1X,'NB_MAX =',I8)
 1009   FORMAT(3X,'**TIMESTEP WILL BE REDUCED TO AVOID DE-ACTIVATION ',
     .            'IN INTERFACE :**',I8)
 1011   FORMAT(/,2X,'** END IMPLICIT MODEL CHECKING **'/,
     .         5X,'TERMINATION WITH '/,I8,' ERRORS '/,I8,' WARNINGS'/
     .         5X,'** DETAILS REPORTED IN LISTING FILE **'/)
        RETURN
#endif
      END
Chd|====================================================================
Chd|  IMP_COMPAB                    source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE IMP_COMPAB(
     1    ICODT     ,ICODR     ,ISKEW     ,IBFV      ,NPC       ,
     2    TF        ,VEL       ,NSENSOR   ,SENSOR_TAB,XFRAME    ,
     3    RBY       ,X         ,SKEW      ,LPBY      ,NPBY      ,
     4    ITAB      ,NRBYAC    ,IRBYAC    ,NINT2     ,IINT2     ,
     5    IPARI     ,INTBUF_TAB,NT_RW     ,NDDL      ,
     6    NDOF      ,IKC       ,INLOC     ,IDDL      ,NDDL0     ,
     7    IWAR      ,IERR      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD
        USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "tabsiz_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#if defined(MUMPS5)
#include      "dmumps_struc.h"
#endif
#include      "sphcom.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ,INTENT(IN) :: NSENSOR
        INTEGER NPC(*),IBFV(NIFV,*),
     .          ICODT(*),ICODR(*),ISKEW(*),NINT2 ,IINT2(*),NT_RW
        INTEGER LPBY(*),NPBY(NNPBY,*),ITAB(*),IPARI(NPARI,*),
     .          NRBYAC,IRBYAC(*),NDDL,NDOF(*),
     .          IDDL(*),IKC(*),INLOC(*) ,NDDL0,IERR,IWAR
        my_real RBY(NRBY,*) ,X(3,*) ,SKEW(*)
        my_real TF(*),VEL(LFXVELR,*),XFRAME(NXFRAME,*)
        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
        TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
#if defined(MUMPS5)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NSN,I,J,K,N,M,JI,NS,NN,IRW,ID
        INTEGER NKINE,IKIN(NUMNOD),IOF(NUMNOD)
        INTEGER ISENS,II,IDEB,IT,ICOMP,NTY,IR,IW
        my_real STARTT, STOPT, TS,SS
        CHARACTER*25  MSG_TYPE(2)
        CHARACTER*25  CSP
        DATA MSG_TYPE/ '** WARNING **', '!! ERROR !!'/
C--------implicit imcompability--
        ICOMP = 0
        IT = 1
        WRITE(IOUT,*)
        WRITE(IOUT,*)' ** INCOMBABILITY CHECKING **'
        WRITE(IOUT,*)
        WRITE(ISTDO,*)
        WRITE(ISTDO,*)' * INCOMBABILITY CHECKING '
C-----------
        IF (NLASER>0) THEN
          ICOMP = ICOMP + NLASER
          IWAR = IWAR +1
          WRITE(IOUT,1100)MSG_TYPE(IT),'IMPACT LASER'
        ENDIF
        DO N =1,NINTER
          NTY   =IPARI(7,N)
          IF (NTY==2.OR.NTY==7.OR.NTY==10.OR.NTY==11
     .        .OR.NTY==5.OR.NTY==24) THEN
            IF(NTY==7.AND.IPARI(33,N)/=0)THEN
              ICOMP = ICOMP + IPARI(33,N)
              IWAR = IWAR +1
              WRITE(IOUT,1100)MSG_TYPE(IT),'LAGRANGE MULTIPLIER INTERFACE'
            ENDIF
          ELSEIF (NTY>0) THEN
            IWAR = IWAR +1
            ICOMP = ICOMP + 1
            WRITE(CSP,'(A,I2.2)')'INTERFACE TYPE ',NTY
            WRITE(IOUT,1100)MSG_TYPE(IT),CSP
          ENDIF
        ENDDO
        IF (NRIVET>0) THEN
          ICOMP = ICOMP + NRIVET
          IWAR = IWAR +1
          WRITE(IOUT,1100)MSG_TYPE(IT),'RIVET'
        ENDIF
        IF (NGJOINT>0) THEN
          ICOMP = ICOMP + NGJOINT
          IWAR = IWAR +1
          WRITE(IOUT,1100)MSG_TYPE(IT),'JOINT TYPE SPRINGS'
        ENDIF
        IF (NJOINT>0) THEN
          ICOMP = ICOMP + NJOINT
          IWAR = IWAR +1
          WRITE(IOUT,1100)MSG_TYPE(IT),'CYLINDRIC JOINT'
        ENDIF
        IF (NUMMPC>0) THEN
          ICOMP = ICOMP + NUMMPC
          IWAR = IWAR +1
          WRITE(IOUT,1100)MSG_TYPE(IT),'MULTI-POINT CONSTRAINTS'
        ENDIF
        IF (NLINK>0) THEN
          ICOMP = ICOMP + NLINK
          IWAR = IWAR +1
          WRITE(IOUT,1100)MSG_TYPE(IT),'RIGID LINK'
        ENDIF
        IF (NUMELX>0) THEN
          ICOMP = ICOMP + NUMELX
          IWAR = IWAR +1
          WRITE(IOUT,1100)MSG_TYPE(IT),'MULTI-PURPOSE ELEMENTS'
        ENDIF
        IF (NUMELS16>0) THEN
          ICOMP = ICOMP + NUMELS16
          IWAR = IWAR +1
          WRITE(IOUT,1100)MSG_TYPE(IT),'SOLID 16n. ELEMENTS'
        ENDIF
        IF (NUMELS20>0) THEN
          ICOMP = ICOMP + NUMELS20
          IWAR = IWAR +1
          WRITE(IOUT,1100)MSG_TYPE(IT),'SOLID 20n. ELEMENTS'
        ENDIF
        IF (NUMELTG6>0) THEN
          ICOMP = ICOMP + NUMELTG6
          IWAR = IWAR +1
          WRITE(IOUT,1100)MSG_TYPE(IT),'SHELL S3N6 ELEMENTS'
        ENDIF
        IF (NUMSPH>0) THEN
          ICOMP = ICOMP + NUMSPH
          IWAR = IWAR +1
          WRITE(IOUT,1100)MSG_TYPE(IT),'SPH ELEMENTS'
        ENDIF
C
        IF (ICOMP>0) WRITE(IOUT,1101) ICOMP
C--------IKIN: 1 MAIN node of rb, 2 s.n. of int2--,3 s.n. of rb,
C------------: 4 bcs, 5 imposed Dis. ,6 sn of rwall, 7 ns of joint,8 ns os rlink
        DO N =1,NUMNOD
          IKIN(N)=0
          IF (NDOF(N)>0) THEN
            IOF(N)=2
          ELSE
            IOF(N)=1
          ENDIF
        ENDDO
C----- MAIN of rigid body first------
        IR =0
        DO I=1,NRBYAC
          N=IRBYAC(I)
          M=NPBY(1,N)
          IF (IKIN(M)==0) THEN
            IKIN(M)=1
          ELSE
            WRITE(IOUT,1001)MSG_TYPE(2),ITAB(M)
            IR = IR + 1
          ENDIF
        ENDDO
        IERR = IERR +IR
C------interface 2--------------
        IW =0
        IR =0
        DO I=1,NINT2
          N=IINT2(I)
          NSN = IPARI(5,N)
          DO J=1,NSN
            NS=INTBUF_TAB(N)%NSV(J)
            IF (IKIN(NS)==0) THEN
              IKIN(NS)=2
            ELSEIF(IKIN(NS)==1) THEN
              WRITE(IOUT,1002)MSG_TYPE(1),ITAB(NS)
              IW =IW + 1
            ELSEIF(IKIN(NS)==2) THEN
              WRITE(IOUT,1003)MSG_TYPE(2),ITAB(NS)
              IR =IR + 1
            ENDIF
          ENDDO
        ENDDO
C----- rigid body ------
        DO I=1,NRBYAC
          N=IRBYAC(I)
          K=IRBYAC(I+NRBYKIN)
          M=NPBY(1,N)
          NSN  =NPBY(2,N)
          DO J=1,NSN
            NS=LPBY(K+J)
            IF (IKIN(NS)==0) THEN
              IKIN(NS)=3
            ELSEIF(IKIN(NS)==1) THEN
              WRITE(IOUT,1004)MSG_TYPE(1),ITAB(NS)
              IW =IW + 1
            ELSEIF(IKIN(NS)==2) THEN
              IT = MIN(IOF(NS),2)
              WRITE(IOUT,1005)MSG_TYPE(IT),ITAB(NS)
              IF (IT==1) THEN
                IW =IW + 1
              ELSEIF (IT==2) THEN
                IR =IR + 1
              ENDIF
            ELSEIF(IKIN(NS)==3) THEN
              IT = MIN(IOF(NS),2)
              WRITE(IOUT,1006)MSG_TYPE(IT),ITAB(NS)
              IF (IT==1) THEN
                IW =IW + 1
              ELSEIF (IT==2) THEN
                IR =IR + 1
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C--------bcs---------
        IF (IRODDL==0) THEN
          DO N = 1,NUMNOD
            IF (ICODT(N) > 0) THEN
              IF (IKIN(N)==0) THEN
                IKIN(N)=4
              ELSEIF(IKIN(N)==2) THEN
                IT = MIN(IOF(N),2)
                WRITE(IOUT,1007)MSG_TYPE(IT),ITAB(N)
                IF (IT==1) THEN
                  IW =IW + 1
                ELSEIF (IT==2) THEN
                  IR =IR + 1
                ENDIF
              ELSEIF(IKIN(N)==3) THEN
                IT = MIN(IOF(N),2)
                WRITE(IOUT,1008)MSG_TYPE(IT),ITAB(N)
                IF (IT==1) THEN
                  IW =IW + 1
                ELSEIF (IT==2) THEN
                  IR =IR + 1
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ELSE
          DO N = 1,NUMNOD
            IF ((ICODT(N)+ICODR(N))>0 ) THEN
              IF (IKIN(N)==0) THEN
                IKIN(N)=4
              ELSEIF(IKIN(N)==2) THEN
                IT = MIN(IOF(N),2)
                WRITE(IOUT,1007)MSG_TYPE(IT),ITAB(N)
                IF (IT==1) THEN
                  IW =IW + 1
                ELSEIF (IT==2) THEN
                  IR =IR + 1
                ENDIF
              ELSEIF(IKIN(N)==3) THEN
                IT = MIN(IOF(N),2)
                WRITE(IOUT,1008)MSG_TYPE(IT),ITAB(N)
                IF (IT==1) THEN
                  IW =IW + 1
                ELSEIF (IT==2) THEN
                  IR =IR + 1
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDIF
C--------fxv---------
        DO NN=1,NFXVEL,NVSIZ
          IF (IBFV(8,NN)==1) GOTO 100
          IF (NSENSOR>0) THEN
            DO 10 II = 1, MIN(NFXVEL-IDEB,NVSIZ)
              N = II+IDEB
              STARTT = VEL(2,N)
              STOPT  = VEL(3,N)
              IF(TT<STARTT)GOTO 10
              IF(TT>STOPT) GOTO 10
              I=IABS(IBFV(1,N))
              ISENS=0
              DO K=1,NSENSOR
                IF(IBFV(4,N)==SENSOR_TAB(K)%SENS_ID) ISENS=K
              ENDDO
              IF(ISENS==0)THEN
                TS=TT
              ELSE
                TS = TT-SENSOR_TAB(ISENS)%TSTART
                IF(TS<ZERO)GOTO 10
              ENDIF
              IF (IKIN(I)==0) THEN
                IKIN(I)=5
              ELSEIF(IKIN(I)==2) THEN
                WRITE(IOUT,1009)MSG_TYPE(2),ITAB(I)
                IR =IR + 1
              ELSEIF(IKIN(I)==3) THEN
                WRITE(IOUT,1010)MSG_TYPE(2),ITAB(I)
                IR =IR + 1
              ELSEIF(IKIN(I)==4) THEN
                WRITE(IOUT,1011)MSG_TYPE(2),ITAB(I)
                IR =IR + 1
              ENDIF
 10         CONTINUE
          ELSE
            DO 20 II = 1, MIN(NFXVEL-IDEB,NVSIZ)
              N = II+IDEB
              STARTT = VEL(2,N)
              STOPT  = VEL(3,N)
              IF(TT<STARTT)GOTO 20
              IF(TT>STOPT) GOTO 20
              I=IABS(IBFV(1,N))
              IF (IKIN(I)==0) THEN
                IKIN(I)=5
              ELSEIF(IKIN(I)==2) THEN
                WRITE(IOUT,1009)MSG_TYPE(2),ITAB(I)
                IR =IR + 1
              ELSEIF(IKIN(I)==3) THEN
                WRITE(IOUT,1010)MSG_TYPE(2),ITAB(I)
                IR =IR + 1
              ELSEIF(IKIN(I)==4) THEN
                WRITE(IOUT,1011)MSG_TYPE(2),ITAB(I)
                IR =IR + 1
              ENDIF
 20         CONTINUE
          ENDIF
C
          IDEB = IDEB + MIN(NFXVEL-IDEB,NVSIZ)
 100      CONTINUE
        ENDDO
C--------rwall---------
        IF (NT_RW>0) THEN
          DO I = 1,NUMNOD
            K = MIN(3,NDOF(I))
            IRW = 0
            DO J =1,K
              ID = IDDL(I) + J
              IF (IKC(ID)==4.OR.IKC(ID)==11) IRW = 1
            ENDDO
            IF (IRW>0) THEN
              IF (IKIN(I)==0) THEN
                IKIN(I)=6
              ELSEIF(IKIN(I)==2) THEN
                WRITE(IOUT,1012)MSG_TYPE(2),ITAB(I)
                IR =IR + 1
              ELSEIF(IKIN(I)==3) THEN
                WRITE(IOUT,1013)MSG_TYPE(2),ITAB(I)
                IR =IR + 1
              ELSEIF(IKIN(I)==4) THEN
                WRITE(IOUT,1014)MSG_TYPE(2),ITAB(I)
                IR =IR + 1
              ELSEIF(IKIN(I)==5) THEN
                WRITE(IOUT,1015)MSG_TYPE(1),ITAB(I)
                IW =IW + 1
              ENDIF
            ENDIF
          ENDDO
        ENDIF
        IERR = IERR +IR
        IWAR = IWAR +IW
        RETURN
 1001   FORMAT(A,' NODE USED FOR DIFF. RBODY MAIN=',I8)
 1002   FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .         'RBODY MAIN AND INTERF. TYPE2 SECONDARY =',I8)
 1003   FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .         'INTERF. TYPE2 SECONDARY AND INTERF. TYPE2 SECONDARY=',I8)
 1004   FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .         ' RBODY MAIN AND RBODY SECONDARY=',I8)
 1005   FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .         ' INTERF. TYPE2 SECONDARY AND RBODY SECONDARY=',I8)
 1006   FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .         ' RBODY SECONDARY AND RBODY SECONDARY=',I8)
 1007   FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .         ' BOUNDARY CONDITIONS AND INTERF. TYPE2 SECONDARY=',I8)
 1008   FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .         ' BOUNDARY CONDITIONS AND RBODY SECONDARY=',I8)
 1009   FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .         ' IMPOSED DISP. AND INTERF. TYPE2 SECONDARY=',I8)
 1010   FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .         ' IMPOSED DISP. AND RBODY SECONDARY=',I8)
 1011   FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .         ' IMPOSED DISP. AND BOUNDARY CONDITIONS=',I8)
 1012   FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .         ' RWALL CONTACT AND INTERF. TYPE2 SECONDARY=',I8)
 1013   FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .         ' RWALL CONTACT AND RBODY SECONDARY=',I8)
 1014   FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .         ' RWALL CONTACT AND BOUNDARY CONDITIONS=',I8)
 1015   FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .         ' RWALL CONTACT AND IMPOSED DISP.=',I8)
 1100   FORMAT(A,' IMPLICIT IS INCOMPABLE WITH :',A/)
 1101   FORMAT(/'**STIFFNESS WILL BE IGNORED WITH',1X,I8,
     .          ' INCOMPABLE OPTIONS**'/)
#endif
      END
Chd|====================================================================
Chd|  IMP_COMPABP                   source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        SPMD_MAX_I                    source/mpi/implicit/imp_spmd.F
Chd|        SPMD_SEND_VI                  source/mpi/implicit/imp_spmd.F
Chd|        SPMD_SUM_S                    source/mpi/implicit/imp_spmd.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE IMP_COMPABP(
     1    ICODT     ,ICODR     ,ISKEW     ,IBFV      ,NPC       ,
     2    TF        ,VEL       ,NSENSOR   ,SENSOR_TAB,XFRAME    ,
     3    RBY       ,X         ,SKEW      ,LPBY      ,NPBY      ,
     4    ITAB      ,NRBYAC    ,IRBYAC    ,NINT2     ,IINT2     ,
     5    IPARI     ,INTBUF_TAB,NT_RW     ,NDDL      ,
     6    NDOF      ,IKC       ,INLOC     ,IDDL      ,NDDL0     ,
     7    IWAR      ,IERR      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD
        USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "sphcom.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ,INTENT(IN) :: NSENSOR
        INTEGER NPC(*),IBFV(NIFV,*),
     .          ICODT(*),ICODR(*),ISKEW(*),NINT2 ,IINT2(*),NT_RW
        INTEGER LPBY(*),NPBY(NNPBY,*),ITAB(*),IPARI(NPARI,*),
     .          NRBYAC,IRBYAC(*),NDDL,NDOF(*),
     .          IDDL(*),IKC(*),INLOC(*) ,NDDL0,IERR,IWAR
        my_real RBY(NRBY,*) ,X(3,*) ,SKEW(*)
        my_real TF(*),VEL(LFXVELR,*),XFRAME(NXFRAME,*)
        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
        TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
#if defined(MUMPS5)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NSN,I,J,K,N,M,JI,K10,K11,K12,NS,NN,IRW,ID,IV
        INTEGER NKINE,IKIN(NUMNOD),IOF(NUMNOD),VI(3,NUMNOD)
        INTEGER ISENS,II,IDEB,IT,ICOMP,NTY,IR,IW,NVMAX,NSIZ
        my_real STARTT, STOPT, TS,SS
        CHARACTER*25  MSG_TYPE(2)
        DATA MSG_TYPE / '** WARNING **', '!! ERROR !!'/
C--------implicit imcompability--
        ICOMP = 0
        IT = 1
        IF (ISPMD==0) THEN
          WRITE(IOUT,*)
          WRITE(IOUT,*)' ** INCOMBABILITY CHECKING **'
          WRITE(IOUT,*)
          WRITE(ISTDO,*)
          WRITE(ISTDO,*)' * INCOMBABILITY CHECKING '
        ENDIF
C
        NN = NLASER
        CALL SPMD_MAX_I(NN)
        IF (NN>0.AND.ISPMD==0) THEN
          ICOMP = ICOMP + NN
          IWAR = IWAR +1
          WRITE(IOUT,1100)MSG_TYPE(IT),'IMPACT LASER'
        ENDIF
        IV = 0
        DO N =1,NINTER
          NTY   =IPARI(7,N)
          IF (NTY==2.OR.NTY==7.OR.NTY==10.OR.NTY==11
     .        .OR.NTY==5.OR.NTY==24) THEN
            IF(NTY==7.AND.IPARI(33,N)/=0)THEN
              ICOMP = ICOMP + IPARI(33,N)
              IWAR = IWAR +1
              WRITE(IOUT,1100)MSG_TYPE(IT),'LAGRANGE MULTIPLIER INTERFACE'
            ENDIF
          ELSEIF (NTY>0) THEN
            IWAR = IWAR +1
            ICOMP = ICOMP + 1
C---------------- espere que ca ne arrive pas le contraire -----
            IF (IV<=NUMNOD) THEN
              IV = IV + 1
              VI(1,IV) = NTY
            ENDIF
          ENDIF
        ENDDO
        NVMAX = IV
        CALL SPMD_MAX_I(NVMAX)
        IF (NVMAX>0) THEN
          CALL SPMD_SEND_VI(
     1    IV       ,1     ,VI       ,NVMAX     ,IOUT     )
        ENDIF
        NN = NRIVET
        CALL SPMD_MAX_I(NN)
        IF (NN>0.AND.ISPMD==0) THEN
          ICOMP = ICOMP + NN
          IWAR = IWAR +1
          WRITE(IOUT,1100)MSG_TYPE(IT),'RIVET'
        ENDIF
        NN = NGJOINT
        CALL SPMD_MAX_I(NN)
        IF (NN>0.AND.ISPMD==0) THEN
          ICOMP = ICOMP + NN
          IWAR = IWAR +1
          WRITE(IOUT,1100)MSG_TYPE(IT),'JOINT TYPE SPRINGS'
        ENDIF
        NN = NJOINT
        CALL SPMD_MAX_I(NN)
        IF (NN>0.AND.ISPMD==0) THEN
          ICOMP = ICOMP + NN
          IWAR = IWAR +1
          WRITE(IOUT,1100)MSG_TYPE(IT),'CYLINDRIC JOINT'
        ENDIF
        NN = NUMMPC
        CALL SPMD_MAX_I(NN)
        IF (NN>0.AND.ISPMD==0) THEN
          ICOMP = ICOMP + NN
          IWAR = IWAR +1
          WRITE(IOUT,1100)MSG_TYPE(IT),'MULTI-POINT CONSTRAINTS'
        ENDIF
        NN = NLINK
        CALL SPMD_MAX_I(NN)
        IF (NN>0.AND.ISPMD==0) THEN
          ICOMP = ICOMP + NN
          IWAR = IWAR +1
          WRITE(IOUT,1100)MSG_TYPE(IT),'RIGID LINK'
        ENDIF
        NN = NUMELX
        CALL SPMD_MAX_I(NN)
        IF (NN>0.AND.ISPMD==0) THEN
          ICOMP = ICOMP + NN
          IWAR = IWAR +1
          WRITE(IOUT,1100)MSG_TYPE(IT),'MULTI-PURPOSE ELEMENTS'
        ENDIF
        NN = NUMELS16
        CALL SPMD_MAX_I(NN)
        IF (NN>0.AND.ISPMD==0) THEN
          ICOMP = ICOMP + NN
          IWAR = IWAR +1
          WRITE(IOUT,1100)MSG_TYPE(IT),'SOLID 16n. ELEMENTS'
        ENDIF
        NN = NUMELS20
        CALL SPMD_MAX_I(NN)
        IF (NN>0.AND.ISPMD==0) THEN
          ICOMP = ICOMP + NN
          IWAR = IWAR +1
          WRITE(IOUT,1100)MSG_TYPE(IT),'SOLID 20n. ELEMENTS'
        ENDIF
        NN = NUMELTG6
        CALL SPMD_MAX_I(NN)
        IF (NN>0.AND.ISPMD==0) THEN
          ICOMP = ICOMP + NN
          IWAR = IWAR +1
          WRITE(IOUT,1100)MSG_TYPE(IT),'SHELL S3N6 ELEMENTS'
        ENDIF
        NN = NUMSPH
        CALL SPMD_MAX_I(NN)
        IF (NN>0.AND.ISPMD==0) THEN
          ICOMP = ICOMP + NN
          IWAR = IWAR +1
          WRITE(IOUT,1100)MSG_TYPE(IT),'SPH ELEMENTS'
        ENDIF
C-----------
        IF (ICOMP>0.AND.ISPMD==0) THEN
          WRITE(IOUT,1101) ICOMP
        ENDIF
C--------IKIN: 1 MAIN node of rb, 2 s.n. of int2--,3 s.n. of rb,
C------------: 4 bcs, 5 imposed Dis. ,6 sn of rwall, 7 ns of joint,8 ns os rlink
        NSIZ = 3
        DO N =1,NUMNOD
          IKIN(N)=0
          IF (NDOF(N)>0) THEN
            IOF(N)=2
          ELSE
            IOF(N)=1
          ENDIF
        ENDDO
C----- MAIN of rigid body first------
        IR =0
        DO I=1,NRBYAC
          N=IRBYAC(I)
          M=NPBY(1,N)
          IF (IKIN(M)==0) THEN
            IKIN(M)=1
          ELSE
c        WRITE(IOUT,1001)MSG_TYPE(2),ITAB(M)
            IR = IR + 1
            VI(1,IR) = 1
            VI(2,IR) = 2
            VI(3,IR) = ITAB(M)
          ENDIF
        ENDDO
        SS = IR
        CALL SPMD_SUM_S(SS)
        NVMAX = IR
        CALL SPMD_MAX_I(NVMAX)
        IERR = IERR +INT(SS)
        IF (NVMAX>0) THEN
          CALL SPMD_SEND_VI(
     1    IR       ,NSIZ   ,VI       ,NVMAX     ,IOUT     )
        ENDIF
C------interface 2--------------
        IW =0
        IR =0
        IV =0
        DO I=1,NINT2
          N=IINT2(I)
          NSN = IPARI(5,N)
          JI=IPARI(1,N)
          K10=JI-1
          K11=K10+4*IPARI(3,N)
C------IRECT(4,NSN)-----
          K12=K11+4*IPARI(4,N)
C------NSV(NSN)--node number---
          DO J=1,NSN
            NS=INTBUF_TAB(N)%NSV(J)
            IF (IKIN(NS)==0) THEN
              IKIN(NS)=2
            ELSEIF(IKIN(NS)==1) THEN
c         WRITE(IOUT,1002)MSG_TYPE(1),ITAB(NS)
              IW =IW + 1
              IV =IV + 1
              VI(1,IV) = 2
              VI(2,IV) = 1
              VI(3,IV) = ITAB(NS)
            ELSEIF(IKIN(NS)==2) THEN
c         WRITE(IOUT,1003)MSG_TYPE(2),ITAB(NS)
              IR =IR + 1
              IV =IV + 1
              VI(1,IV) = 3
              VI(2,IV) = 2
              VI(3,IV) = ITAB(NS)
            ENDIF
          ENDDO
        ENDDO
        NVMAX = IV
        CALL SPMD_MAX_I(NVMAX)
        IF (NVMAX>0) THEN
          CALL SPMD_SEND_VI(
     1    IV       ,NSIZ   ,VI       ,NVMAX     ,IOUT     )
        ENDIF
C----- rigid body ------
        IV =0
        DO I=1,NRBYAC
          N=IRBYAC(I)
          K=IRBYAC(I+NRBYKIN)
          M=NPBY(1,N)
          NSN  =NPBY(2,N)
          DO J=1,NSN
            NS=LPBY(K+J)
            IF (IKIN(NS)==0) THEN
              IKIN(NS)=3
            ELSEIF(IKIN(NS)==1) THEN
c          WRITE(IOUT,1004)MSG_TYPE(1),ITAB(NS)
              IW =IW + 1
              IV =IV + 1
              VI(1,IV) = 4
              VI(2,IV) = 1
              VI(3,IV) = ITAB(NS)
            ELSEIF(IKIN(NS)==2) THEN
              IT = MIN(IOF(NS),2)
c          WRITE(IOUT,1005)MSG_TYPE(IT),ITAB(NS)
              IV =IV + 1
              VI(1,IV) = 5
              VI(2,IV) = IT
              VI(3,IV) = ITAB(NS)
              IF (IT==1) THEN
                IW =IW + 1
              ELSEIF (IT==2) THEN
                IR =IR + 1
              ENDIF
            ELSEIF(IKIN(NS)==3) THEN
              IT = MIN(IOF(NS),2)
c          WRITE(IOUT,1006)MSG_TYPE(IT),ITAB(NS)
              IV =IV + 1
              VI(1,IV) = 6
              VI(2,IV) = IT
              VI(3,IV) = ITAB(NS)
              IF (IT==1) THEN
                IW =IW + 1
              ELSEIF (IT==2) THEN
                IR =IR + 1
              ENDIF
            ENDIF
          ENDDO
        ENDDO
        NVMAX = IV
        CALL SPMD_MAX_I(NVMAX)
        IF (NVMAX>0) THEN
          CALL SPMD_SEND_VI(
     1    IV       ,NSIZ   ,VI       ,NVMAX     ,IOUT     )
        ENDIF
C--------bcs---------
        IV =0
        IF (IRODDL==0) THEN
          DO N = 1,NUMNOD
            IF (ICODT(N) > 0) THEN
              IF (IKIN(N)==0) THEN
                IKIN(N)=4
              ELSEIF(IKIN(N)==2) THEN
                IT = MIN(IOF(N),2)
c          WRITE(IOUT,1007)MSG_TYPE(IT),ITAB(N)
                IV =IV + 1
                VI(1,IV) = 7
                VI(2,IV) = IT
                VI(3,IV) = ITAB(N)
                IF (IT==1) THEN
                  IW =IW + 1
                ELSEIF (IT==2) THEN
                  IR =IR + 1
                ENDIF
              ELSEIF(IKIN(N)==3) THEN
                IT = 1
C          IT = MIN(IOF(N),2)
c          WRITE(IOUT,1008)MSG_TYPE(IT),ITAB(N)
                IV =IV + 1
                VI(1,IV) = 8
                VI(2,IV) = IT
                VI(3,IV) = ITAB(N)
                IF (IT==1) THEN
                  IW =IW + 1
                ELSEIF (IT==2) THEN
                  IR =IR + 1
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ELSE
          DO N = 1,NUMNOD
            IF ((ICODT(N)+ICODR(N))>0 ) THEN
              IF (IKIN(N)==0) THEN
                IKIN(N)=4
              ELSEIF(IKIN(N)==2) THEN
                IT = MIN(IOF(N),2)
c          WRITE(IOUT,1007)MSG_TYPE(IT),ITAB(N)
                IV =IV + 1
                VI(1,IV) = 7
                VI(2,IV) = IT
                VI(3,IV) = ITAB(N)
                IF (IT==1) THEN
                  IW =IW + 1
                ELSEIF (IT==2) THEN
                  IR =IR + 1
                ENDIF
              ELSEIF(IKIN(N)==3) THEN
                IT = 1
C          IT = MIN(IOF(N),2)
c          WRITE(IOUT,1008)MSG_TYPE(IT),ITAB(N)
                IV =IV + 1
                VI(1,IV) = 8
                VI(2,IV) = IT
                VI(3,IV) = ITAB(N)
                IF (IT==1) THEN
                  IW =IW + 1
                ELSEIF (IT==2) THEN
                  IR =IR + 1
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDIF
        NVMAX = IV
        CALL SPMD_MAX_I(NVMAX)
        IF (NVMAX>0) THEN
          CALL SPMD_SEND_VI(
     1    IV       ,NSIZ   ,VI       ,NVMAX     ,IOUT     )
        ENDIF
C--------fxv---------
        IV =0
        DO NN=1,NFXVEL,NVSIZ
          IF (IBFV(8,NN)==1) GOTO 100
          IF (NSENSOR>0) THEN
            DO 10 II = 1, MIN(NFXVEL-IDEB,NVSIZ)
              N = II+IDEB
              STARTT = VEL(2,N)
              STOPT  = VEL(3,N)
              IF(TT<STARTT)GOTO 10
              IF(TT>STOPT) GOTO 10
              I=IABS(IBFV(1,N))
              ISENS=0
              DO K=1,NSENSOR
                IF(IBFV(4,N)==SENSOR_TAB(K)%SENS_ID) ISENS=K
              ENDDO
              IF(ISENS==0)THEN
                TS=TT
              ELSE
                TS = TT-SENSOR_TAB(ISENS)%TSTART
                IF(TS<ZERO)GOTO 10
              ENDIF
              IF (IKIN(I)==0) THEN
                IKIN(I)=5
              ELSEIF(IKIN(I)==2) THEN
c             WRITE(IOUT,1009)MSG_TYPE(2),ITAB(I)
                IV =IV + 1
                VI(1,IV) = 9
                VI(2,IV) = 2
                VI(3,IV) = ITAB(I)
                IR =IR + 1
              ELSEIF(IKIN(I)==3) THEN
c             WRITE(IOUT,1010)MSG_TYPE(2),ITAB(I)
                IV =IV + 1
                VI(1,IV) = 10
                VI(2,IV) = 1
                VI(3,IV) = ITAB(I)
                IW =IW + 1
              ELSEIF(IKIN(I)==4) THEN
c             WRITE(IOUT,1011)MSG_TYPE(2),ITAB(I)
                IV =IV + 1
                VI(1,IV) = 11
                VI(2,IV) = 2
                VI(3,IV) = ITAB(I)
                IR =IR + 1
              ENDIF
 10         CONTINUE
          ELSE
            DO 20 II = 1, MIN(NFXVEL-IDEB,NVSIZ)
              N = II+IDEB
              STARTT = VEL(2,N)
              STOPT  = VEL(3,N)
              IF(TT<STARTT)GOTO 20
              IF(TT>STOPT) GOTO 20
              I=IABS(IBFV(1,N))
              IF (IKIN(I)==0) THEN
                IKIN(I)=5
              ELSEIF(IKIN(I)==2) THEN
c             WRITE(IOUT,1009)MSG_TYPE(2),ITAB(I)
                IV =IV + 1
                VI(1,IV) = 9
                VI(2,IV) = 2
                VI(3,IV) = ITAB(I)
                IR =IR + 1
              ELSEIF(IKIN(I)==3) THEN
c             WRITE(IOUT,1010)MSG_TYPE(2),ITAB(I)
                IV =IV + 1
                VI(1,IV) = 10
                VI(2,IV) = 1
                VI(3,IV) = ITAB(I)
                IW =IW + 1
              ELSEIF(IKIN(I)==4) THEN
c             WRITE(IOUT,1011)MSG_TYPE(2),ITAB(I)
                IV =IV + 1
                VI(1,IV) = 11
                VI(2,IV) = 2
                VI(3,IV) = ITAB(I)
                IR =IR + 1
              ENDIF
 20         CONTINUE
          ENDIF
C
          IDEB = IDEB + MIN(NFXVEL-IDEB,NVSIZ)
 100      CONTINUE
        ENDDO
        NVMAX = IV
        CALL SPMD_MAX_I(NVMAX)
        IF (NVMAX>0) THEN
          CALL SPMD_SEND_VI(
     1    IV       ,NSIZ   ,VI       ,NVMAX     ,IOUT     )
        ENDIF
C--------rwall---------
        IV =0
        IF (NT_RW>0) THEN
          DO I = 1,NUMNOD
            K = MIN(3,NDOF(I))
            IRW = 0
            DO J =1,K
              ID = IDDL(I) + J
              IF (IKC(ID)==4.OR.IKC(ID)==11) IRW = 1
            ENDDO
            IF (IRW>0) THEN
              IF (IKIN(I)==0) THEN
                IKIN(I)=6
              ELSEIF(IKIN(I)==2) THEN
c          WRITE(IOUT,1012)MSG_TYPE(2),ITAB(I)
                IV =IV + 1
                VI(1,IV) = 12
                VI(2,IV) = 2
                VI(3,IV) = ITAB(I)
                IR =IR + 1
              ELSEIF(IKIN(I)==3) THEN
c          WRITE(IOUT,1013)MSG_TYPE(2),ITAB(I)
                IV =IV + 1
                VI(1,IV) = 13
                VI(2,IV) = 1
                VI(3,IV) = ITAB(I)
                IW =IW + 1
              ELSEIF(IKIN(I)==4) THEN
c          WRITE(IOUT,1014)MSG_TYPE(2),ITAB(I)
                IV =IV + 1
                VI(1,IV) = 14
                VI(2,IV) = 2
                VI(3,IV) = ITAB(I)
                IR =IR + 1
              ELSEIF(IKIN(I)==5) THEN
c          WRITE(IOUT,1015)MSG_TYPE(1),ITAB(I)
                IV =IV + 1
                VI(1,IV) = 15
                VI(2,IV) = 1
                VI(3,IV) = ITAB(I)
                IW =IW + 1
              ENDIF
            ENDIF
          ENDDO
        ENDIF
C
        NVMAX = IV
        CALL SPMD_MAX_I(NVMAX)
        IF (NVMAX>0) THEN
          CALL SPMD_SEND_VI(
     1    IV       ,NSIZ   ,VI       ,NVMAX     ,IOUT     )
        ENDIF
        SS = IR
        CALL SPMD_SUM_S(SS)
        IR = INT(SS)
        SS = IW
        CALL SPMD_SUM_S(SS)
        IW = INT(SS)
        IERR = IERR +IR
        IWAR = IWAR +IW
        RETURN
 1100   FORMAT(A,' IMPLICIT IS INCOMPABLE WITH :',A/)
 1101   FORMAT(/'**STIFFNESS WILL BE IGNORED WITH',1X,I8,
     .          ' INCOMPABLE OPTIONS**'/)
C endif MUMPS defined
#endif
      END

Chd|====================================================================
Chd|  IMP_FOUT                      source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        IMPBUFDEF_MOD                 share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE IMP_FOUT(
     1          FANI     ,A        ,AR       ,NFIA      ,NFEA       ,
     2          NODFT    ,NODLT    ,H3D_DATA ,IMPBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE H3D_MOD
        USE IMPBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr14_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NFIA,NFEA,NODFT,NODLT
C     REAL
        my_real
     .     A(3,*)    ,AR(3,*)    ,FANI(3,*)
        TYPE(H3D_DATABASE) :: H3D_DATA
        TYPE (IMPBUF_STRUCT_) ,TARGET :: IMPBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER N,I,J,K,ND
        INTEGER, DIMENSION(:) ,POINTER     :: IDDL,NDOF,IKC
C-------------mis  zero Fint des noeuds dependants------------
        IDDL => IMPBUF_TAB%IDDL
        NDOF => IMPBUF_TAB%NDOF
        IKC => IMPBUF_TAB%IKC
        IF(ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT>0) THEN
          DO I = NODFT,NODLT
            DO J=1,MIN(3,NDOF(I))
              ND = IDDL(I)+J
              IF (IKC(ND)/=0) FANI(J,I+NFIA)= ZERO
            ENDDO
            IF (NDOF(I)==0) THEN
              FANI(1,I+NFIA)= ZERO
              FANI(2,I+NFIA)= ZERO
              FANI(3,I+NFIA)= ZERO
            ENDIF
          ENDDO
        ENDIF
C
        RETURN
      END
Chd|====================================================================
Chd|  IMP_FANII                     source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE IMP_FANII(
     1          FANI     ,FINT     ,NFIA      ,NODFT    ,NODLT    ,
     2          H3D_DATA )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr14_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NFIA,NODFT,NODLT
C     REAL
        my_real
     .     FINT(3,*)    ,FANI(3,*)
        TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER N,I,J,K,ND
C---
        IF(ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT>0) THEN
#include      "vectorize.inc"
          DO N=NODFT,NODLT
            FANI(1,N+NFIA)= FINT(1,N)
            FANI(2,N+NFIA)= FINT(2,N)
            FANI(3,N+NFIA)= FINT(3,N)
          ENDDO
        ENDIF
C
        RETURN
      END
Chd|====================================================================
Chd|  IMP_FANIE                     source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE IMP_FANIE(
     1          FANI   ,FEXT   ,NFIA   ,NFEA     ,NODFT    ,NODLT    ,
     2          H3D_DATA )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr14_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NFIA,NFEA,NODFT,NODLT
C     REAL
        my_real
     .     FEXT(3,*)    ,FANI(3,*)
        TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER N,I,J,K,ND
C---
        IF(ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT>0) THEN
#include      "vectorize.inc"
          DO N=NODFT,NODLT
            FANI(1,N+NFIA)= -FEXT(1,N)
            FANI(2,N+NFIA)= -FEXT(2,N)
            FANI(3,N+NFIA)= -FEXT(3,N)
          ENDDO
        ENDIF
        IF(ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT>0) THEN
#include      "vectorize.inc"
          DO N=NODFT,NODLT
            FANI(1,N+NFEA)= FEXT(1,N)
            FANI(2,N+NFEA)= FEXT(2,N)
            FANI(3,N+NFEA)= FEXT(3,N)
          ENDDO
        ENDIF
        RETURN
      END
Chd|====================================================================
Chd|  INI_KIC                       source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE INI_KIC
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        NDDL_SI = 0
        NDDL_SL = 0
        NZ_SI = 0
        NZ_SL = 0
C------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  DEALLOCM                      source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_KNON                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE DEALLOCM
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_KNON
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        IF(ALLOCATED(IN_KN)) DEALLOCATE(IN_KN)
        IF(ALLOCATED(ID_KN)) DEALLOCATE(ID_KN)
        IF (NUMN_KN>0) THEN
          IF(ALLOCATED(ID_KNM)) DEALLOCATE(ID_KNM)
          IF(ALLOCATED(ID_KNM2)) DEALLOCATE(ID_KNM2)
          IF(ALLOCATED(ID_KNM3)) DEALLOCATE(ID_KNM3)
          IF(ALLOCATED(II2_KN)) DEALLOCATE(II2_KN)
          IF(ALLOCATED(IRB_KN)) DEALLOCATE(IRB_KN)
          IF(ALLOCATED(IBC_KN)) DEALLOCATE(IBC_KN)
          IF(ALLOCATED(IFX_KN)) DEALLOCATE(IFX_KN)
          IF(ALLOCATED(IRW_KN)) DEALLOCATE(IRW_KN)
          IF(ALLOCATED(IRBE3_KN)) DEALLOCATE(IRBE3_KN)
          IF(ALLOCATED(FCDI_KN)) DEALLOCATE(FCDI_KN)
          IF(ALLOCATED(MCDI_KN)) DEALLOCATE(MCDI_KN)
        ENDIF
C------------------------------------------
        RETURN
      END

Chd|====================================================================
Chd|  DEALLOCM_IMP                  source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        FVBC_DEALLO                   source/constraints/general/impvel/fv_imp0.F
Chd|        SPMD_MUMPS_DEAL               source/mpi/implicit/imp_spmd.F
Chd|        IMP_BFGS                      share/modules/impbufdef_mod.F 
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|        IMP_INTBUF                    share/modules/imp_mod_def.F   
Chd|        IMP_KBCS                      share/modules/impbufdef_mod.F 
Chd|        IMP_PCG_PROJ                  share/modules/impbufdef_mod.F 
Chd|        IMP_QSTAT                     share/modules/impbufdef_mod.F 
Chd|        IMP_SPBRM                     share/modules/impbufdef_mod.F 
Chd|        IMP_WORKH                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE DEALLOCM_IMP(MUMPS_PAR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_KBCS
        USE IMP_BFGS
        USE IMP_DYNA
        USE IMP_WORKH
        USE IMP_PCG_PROJ
        USE IMP_QSTAT
        USE IMP_SPBRM
        USE IMP_INTBUF
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "impl1_c.inc"
#include "impl2_c.inc"
#if defined(MUMPS5)
#include "dmumps_struc.h"
#endif
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
#ifdef MUMPS5
      TYPE(DMUMPS_STRUC) MUMPS_PAR
#else
      ! Fake declaration as DMUMPS_STRUC is shipped with MUMPS
      INTEGER MUMPS_PAR 
#endif
#if defined(MUMPS5)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        IF(IMUMPSV > 0 ) CALL SPMD_MUMPS_DEAL(MUMPS_PAR)
        IF (ISOLV>2) THEN
          IF(ALLOCATED(HOLD)) DEALLOCATE(HOLD)
        ENDIF
        IF (INSOLV>1) THEN
          IF(ALLOCATED(BFGS_V)) DEALLOCATE(BFGS_V)
          IF(ALLOCATED(BFGS_W)) DEALLOCATE(BFGS_W)
        ENDIF
        IF (IDYNA>0) THEN
          IF(ALLOCATED(DY_D)) DEALLOCATE(DY_D)
          IF(ALLOCATED(DY_DR)) DEALLOCATE(DY_DR)
          IF(ALLOCATED(DY_V)) DEALLOCATE(DY_V)
          IF(ALLOCATED(DY_VR)) DEALLOCATE(DY_VR)
c        IF(ALLOCATED(DY_A)) DEALLOCATE(DY_A)
c        IF(ALLOCATED(DY_AR)) DEALLOCATE(DY_AR)
          IF (IDY_DAMP>0) THEN
            DEALLOCATE(DY_DIAK0,DY_LTK0)
            DEALLOCATE(DY_IADK0,DY_JDIK0)
          ENDIF
          IF (HHT_A/=ZERO)DEALLOCATE(DY_R0,DY_R1)
        ENDIF
C---------------lin_solv--------
        IF(ALLOCATED(L_U)) DEALLOCATE(L_U)
        IF(ALLOCATED(DIAG_T)) DEALLOCATE(DIAG_T)
        IF(ALLOCATED(L_F0)) DEALLOCATE(L_F0)
        IF (ISOLV==1.OR.ISOLV>4) THEN
          IF(ALLOCATED(IADK0)) DEALLOCATE(IADK0)
          IF(ALLOCATED(JDIK0)) DEALLOCATE(JDIK0)
          IF(ALLOCATED(LT_K0)) DEALLOCATE(LT_K0)
          IF(ALLOCATED(PCG_W1)) DEALLOCATE(PCG_W1)
          IF(ALLOCATED(PCG_W2)) DEALLOCATE(PCG_W2)
          IF(ALLOCATED(PCG_W3)) DEALLOCATE(PCG_W3)
          IF (IPREC==5) THEN
            IF(ALLOCATED(IADM0)) DEALLOCATE(IADM0)
            IF(ALLOCATED(JDIM0)) DEALLOCATE(JDIM0)
            IF(ALLOCATED(LT_M0)) DEALLOCATE(LT_M0)
          ENDIF
          IF (M_VS>0) THEN
            IF(ALLOCATED(PROJ_S)) DEALLOCATE(PROJ_S)
            IF(ALLOCATED(PROJ_T)) DEALLOCATE(PROJ_T)
            IF(ALLOCATED(PROJ_LA_1)) DEALLOCATE(PROJ_LA_1)
            IF(ALLOCATED(PROJ_V)) DEALLOCATE(PROJ_V)
            IF(ALLOCATED(PROJ_W)) DEALLOCATE(PROJ_W)
            IF(ALLOCATED(PROJ_K)) DEALLOCATE(PROJ_K)
          ENDIF
        ENDIF
        IF(IQSTAT==1 .AND. ILINE==0) THEN
          DEALLOCATE(D_N_1)
          IF (IRODDL/=0) DEALLOCATE(DR_N_1)
        END IF
        CALL FVBC_DEALLO
        IF (IRIG_M>0) THEN
          IF(ALLOCATED(IBC_B)) DEALLOCATE(IBC_B)
          IF(ALLOCATED(IE_BC4)) DEALLOCATE(IE_BC4)
          IF(ALLOCATED(IE_BC3)) DEALLOCATE(IE_BC3)
        END IF
C------    faire---
c      IF (NINTER>0) THEN
c       IF(ALLOCATED(INTBUF_TAB_CP)) DEALLOCATE(INTBUF_TAB_CP)
c       IF(ALLOCATED(INTBUF_TAB_IMP)) DEALLOCATE(INTBUF_TAB_IMP)
c       IF(ALLOCATED(BMINMA_IMP)) DEALLOCATE(BMINMA_IMP)
c      END IF
C------------------------------------------
        RETURN
#endif
      END
Chd|====================================================================
Chd|  CRIT_LLIM                     source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CRIT_LLIM(NDDL,NNZK)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
        INTEGER NDDL,NNZK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,JD,NP
        my_real
     .    PFAC,CRITL,S1,S2
C------compute auto seclect solver by L_LIM--------------
C--------take into account to parallel capacities of PCG---
        IF (NSPMD == 1) THEN
          NDDL_G = NDDL
          NNZK_G = NNZK
        END IF
        NP=NSPMD/2
        PFAC= TWO_THIRD*NTHREAD*MAX(1,NP)
        PFAC=MAX(ONE,PFAC)
        S1=NDDL_G*FIVE*EM03
        S2=NNZK_G*TWOP8*EM04
        CRITL=HALF*(S1+S2)
        L_LIM=CRITL*PFAC
C------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  INI_K0H                       source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        DIM_SPAN                      source/implicit/ind_glob_k.F  
Chd|        IMP_PCG_PROJ                  share/modules/impbufdef_mod.F 
Chd|        IMP_WORKH                     share/modules/impbufdef_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INI_K0H(NDDL,NNZ,NNZM,IADK,JDIK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_WORKH
        USE IMP_PCG_PROJ
        USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NDDL,NNZ,NNZM,IADK(*),JDIK(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER IERR,LNZM
C------------------for lin_solv---------------
        ALLOCATE(L_U(NDDL),DIAG_T(NDDL),L_F0(NDDL),STAT=IERR)
        IF (IERR/=0) THEN
          CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                C1='FOR IMPLICIT SOLVER')
          CALL ARRET(2)
        ENDIF
        IF (ISOLV==1.OR.ISOLV>4) THEN
          ALLOCATE(PCG_W1(NDDL),PCG_W2(NDDL),PCG_W3(NDDL),STAT=IERR)
          ALLOCATE(IADK0(NDDL+1),JDIK0(NNZ),LT_K0(NNZ),STAT=IERR)
          IF (IERR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                  C1='FOR PCG SOLVER')
            CALL ARRET(2)
          ENDIF
          LT_K0=ZERO
          IF (IPREC==5) THEN
            LNZM = NNZM
            IF (N_PAT>1) CALL DIM_SPAN(N_PAT,NDDL,IADK,JDIK,LNZM,IERR)
            ALLOCATE(IADM0(NDDL+1),JDIM0(LNZM),LT_M0(LNZM),STAT=IERR)
            IF (IERR/=0) THEN
              CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                    C1='FOR PCG(IPREC=5) SOLVER')
              CALL ARRET(2)
            ENDIF
            LT_M0=ZERO
          END IF !(IPREC==5) THEN
        END IF !(ISOLV==1.OR.ISOLV>4) THEN
C
        IF (M_VS>0) THEN
          LNZM=M_VS+1
          ALLOCATE(PROJ_S(NDDL,LNZM),PROJ_T(NDDL,LNZM),PROJ_LA_1(LNZM),
     .                    PROJ_V(NDDL),PROJ_W(LNZM),PROJ_K(LNZM,LNZM),
     .                    STAT=IERR)
          IF (IERR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                  C1='FOR PCG SOLVER')
            CALL ARRET(2)
          ENDIF
          NCG_RUN = 0
          PROJ_V = ZERO
        END IF
C------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  SET_KSYM                      source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|        LIN_SOLVH0                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVH1                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVHM                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVIH2                   source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SET_KSYM(NDDL,IADK,JDIK,LT_K,IADK0,JDIK0,LT_K0)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER  NDDL,IADK(*),JDIK(*),IADK0(*),JDIK0(*)
        my_real
     .    LT_K(*),LT_K0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER  I,J,K,JD,ICOL(NDDL),NRI,NR0
C----6--K0:matrice complete(non triang)
        DO I = 1, NDDL
          ICOL(I) = IADK(I+1) - IADK(I)
        ENDDO
        DO I = 1, NDDL
          DO J = IADK(I),IADK(I+1)-1
            JD = JDIK(J)
            ICOL(JD) = ICOL(JD) + 1
          ENDDO
        ENDDO
        IADK0(1) = 1
        DO I = 1,NDDL
          IADK0(I+1) = IADK0(I)+ICOL(I)-IADK(I+1)+IADK(I)
          ICOL(I) = 0
        ENDDO
        DO I = 1,NDDL
          DO J=IADK(I),IADK(I+1)-1
            JD = JDIK(J)
            K = IADK0(JD) + ICOL(JD)
            JDIK0(K) = I
            LT_K0(K) = LT_K(J)
            ICOL(JD) = ICOL(JD) + 1
          ENDDO
        ENDDO
C
        RETURN
      END

Chd|====================================================================
Chd|  GET_FEXT                      source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        CONDENS_B                     source/implicit/upd_glob_k.F  
Chd|        IMP_SETBA                     source/implicit/imp_setb.F    
Chd|        IMP_SETBP                     source/implicit/imp_setb.F    
Chd|        SPMD_SUMF_V                   source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE GET_FEXT(NDDL0 ,NDDL   ,IDDL   ,NDOF   ,IKC   ,
     1                    INLOC ,LB     ,FEXT   ,AC     ,ACR   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NDDL0 ,NDDL   ,IDDL(*),NDOF(*),IKC(*),INLOC(*)
        my_real
     .    LB(*),FEXT(*),AC(*),ACR(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J
        my_real
     .    BFAC,NTMP
C-----------------------------------------------
        IF (ABS(TT)<EM20) RETURN
        BFAC=TSTOP/TT
        IF (NSPMD>1) THEN
C -------------------------------
          NTMP = 0
          DO I=1,NDDL0
            FEXT(I)=LB(I)
          ENDDO
          CALL IMP_SETBA(AC    ,ACR      ,IDDL   ,NDOF  ,FEXT    ,
     1                   NTMP  )
          CALL CONDENS_B(NDDL0  ,IKC  ,FEXT)
          CALL SPMD_SUMF_V(FEXT)
          CALL IMP_SETBP(AC    ,ACR    ,IDDL   ,NDOF   ,IKC   ,
     .                   INLOC ,FEXT   )
          DO I=1,NDDL
            FEXT(I)=BFAC*FEXT(I)
          ENDDO
        ELSE
          DO I=1,NDDL0
            FEXT(I)=BFAC*LB(I)
          ENDDO
          CALL CONDENS_B(NDDL0  ,IKC  ,FEXT  )
        END IF
C------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  RE2INT5                       source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RE2INT5(NT_IMP,NUMIMP,NS_IMP,NE_IMP,NUMIMPL,
     1                   IPARI ,NT_IMP0)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER  NT_IMP,NUMIMP(*),NS_IMP(*),NE_IMP(*),
     .           NUMIMPL(NINTER,*),IPARI(NPARI,*),NT_IMP0
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,K,L,N,IAD,IAD1,IADT,ITY,L_CP,L_CPJ,NIMPJ,
     .          IADN(NTHREAD)
        INTEGER, DIMENSION(:),ALLOCATABLE :: NS_CP,NE_CP
        INTEGER IER1
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C-----------------------------------------
        NT_IMP = 0
        IADT = 0
        L_CPJ = 0
        DO J = 1,NTHREAD
          NIMPJ = IADT
          DO N = 1,NINTER
            ITY   =IPARI(7,N)
            IF (ITY==3.OR.ITY==4.OR.ITY==5) THEN
              NUMIMP(N)=0
              IADT =IADT + NUMIMPL(N,J)
            END IF
          END DO
          NIMPJ = -NIMPJ+IADT
          L_CPJ = MAX(L_CPJ,NIMPJ)
        END DO
        IF (IADT==0) RETURN
C
        IF (NTHREAD==1) THEN
          DO N = 1,NINTER
            NUMIMP(N) =NUMIMPL(N,1)
          END DO
        ELSE
          L_CP = L_CPJ*NTHREAD
          ALLOCATE(NS_CP(L_CP),NE_CP(L_CP),STAT=IER1)
C
          IAD=0
          DO J = 1,NTHREAD
            IAD1=(J-1)*NT_IMP0
            DO I = 1,L_CPJ
              NS_CP(IAD+I) = NS_IMP(IAD1+I)
              NE_CP(IAD+I) = NE_IMP(IAD1+I)
            END DO
            IAD =IAD + L_CPJ
            IADN(J) =0
          END DO
C
          IAD = 0
          DO N = 1,NINTER
            ITY   =IPARI(7,N)
            NUMIMP(N)=0
            IF (ITY==3.OR.ITY==4.OR.ITY==5) THEN
              DO J = 1,NTHREAD
                IAD1=(J-1)*L_CP/ NTHREAD + IADN(J)
                DO I = 1,NUMIMPL(N,J)
                  NS_IMP(IAD+I) = NS_CP(IAD1+I)
                  NE_IMP(IAD+I) = NE_CP(IAD1+I)
                END DO
                IAD =IAD + NUMIMPL(N,J)
                NUMIMP(N) = NUMIMP(N)+NUMIMPL(N,J)
                IADN(J) = IADN(J) + NUMIMPL(N,J)
C-------reput zero
                NUMIMPL(N,J)=0
              END DO
            END IF
          END DO
          DEALLOCATE(NS_CP,NE_CP)
        END IF !(NTHREAD==1) THEN
C
        NT_IMP = NT_IMP + IADT
C
        RETURN
      END
Chd|====================================================================
Chd|  RE2INT7                       source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        IMP_STIF24                    source/implicit/imp_solv.F    
Chd|====================================================================
      SUBROUTINE RE2INT7(NT_IMP,NUMIMP,NS_IMP,NE_IMP,IND_IMP,
     1                   NUMIMPL,IPARI,NT_IMP0)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER  NT_IMP,NUMIMP(*),NS_IMP(*),NE_IMP(*),IND_IMP(*),
     .           NUMIMPL(NINTER,*),IPARI(NPARI,*),NT_IMP0
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,K,L,N,IAD,IAD1,IADT,ITY,IAD0,L_CP,L_CPJ,NIMPJ,
     .          NT_IMP7,IADN(NTHREAD)
        INTEGER, DIMENSION(:),ALLOCATABLE :: NS_CP,NE_CP,IND_CP
        INTEGER IER1
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
        IADT = 0
        L_CPJ = 0
        DO J = 1,NTHREAD
          NIMPJ = IADT
          DO N = 1,NINTER
            ITY   =IPARI(7,N)
            IF (ITY==7.OR.ITY==10.OR.ITY==11.OR.ITY==24) THEN
              NUMIMP(N)=0
              IADT =IADT + NUMIMPL(N,J)
            END IF
          END DO
          NIMPJ = -NIMPJ+IADT
          L_CPJ = MAX(L_CPJ,NIMPJ)
        END DO
        IF (IADT==0) GOTO 100

        IF (NTHREAD==1) THEN
          DO N = 1,NINTER
            NUMIMP(N) =NUMIMPL(N,1)
          END DO
        ELSE
          L_CP = L_CPJ*NTHREAD
          ALLOCATE(NS_CP(L_CP),NE_CP(L_CP),IND_CP(L_CP),STAT=IER1)

          IAD0=NT_IMP
          NT_IMP7=NT_IMP0-NT_IMP
          IAD =0
          DO J = 1,NTHREAD
            IAD1=IAD0+(J-1)*NT_IMP7
            DO I = 1,L_CPJ
              NS_CP(IAD+I) = NS_IMP(IAD1+I)
              NE_CP(IAD+I) = NE_IMP(IAD1+I)
              IND_CP(IAD+I) = IND_IMP(IAD1+I)
            END DO
            IAD =IAD + L_CPJ
            IADN(J) =0
          END DO

          IAD = IAD0
          DO N = 1,NINTER
            ITY   =IPARI(7,N)
            IF (ITY==7.OR.ITY==10.OR.ITY==11.OR.ITY==24) THEN
              DO J = 1,NTHREAD
                IAD1=(J-1)*L_CP/ NTHREAD + IADN(J)
                DO I = 1,NUMIMPL(N,J)
                  NS_IMP(IAD+I) = NS_CP(IAD1+I)
                  NE_IMP(IAD+I) = NE_CP(IAD1+I)
                  IND_IMP(IAD+I) = IND_CP(IAD1+I)
                END DO
                IAD =IAD + NUMIMPL(N,J)
                NUMIMP(N) = NUMIMP(N)+NUMIMPL(N,J)
                IADN(J) = IADN(J) + NUMIMPL(N,J)
C        ----reput zero
                NUMIMPL(N,J)=0
              END DO
            END IF
          END DO
          DEALLOCATE(NS_CP,NE_CP,IND_CP)
        END IF !(NTHREAD==1) THEN
C
        NT_IMP = NT_IMP + IADT
C-------int24, Istif=6
 100    CONTINUE
        CALL IMP_STIF24(NUMIMP  ,IPARI   )
        RETURN
      END
Chd|====================================================================
Chd|  DU_INI                        source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE DU_INI(NODFT ,NODLT  ,DN     ,DNR    ,DD    ,
     1                  DDR   ,IDIV   ,ICONT0 )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_DYNA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
#include      "impl2_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NODFT ,NODLT,IDIV   ,ICONT0
        my_real
     .   DN(3,*),DNR(3,*),DD(3,*),DDR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,IRES
        my_real
     .    BFAC,BDT
C--------------Dn,0=Dn-1--------
C--------special case with /QSTAT diverge by contact, restart w/o resolution
        IRES = 1
        IF (IQSTAT >0 .AND. IDIV==-2 .AND. ICONT0 >0 ) THEN
          IDIV=-1
          IRES=0
        END IF
C
        BFAC=DT1_IMP/DT0_IMP
        IF (IDYNA==0) THEN
          IF (ISMDISP>0.OR.IRES==0) THEN
            DO I = NODFT ,NODLT
              DD(1,I) = BFAC*DN(1,I)
              DD(2,I) = BFAC*DN(2,I)
              DD(3,I) = BFAC*DN(3,I)
            END DO
            IF (IRODDL/=0) THEN
              DO I = NODFT ,NODLT
                DDR(1,I) = BFAC*DNR(1,I)
                DDR(2,I) = BFAC*DNR(2,I)
                DDR(3,I) = BFAC*DNR(3,I)
              END DO
            END IF
          ELSE
            DO I = NODFT ,NODLT
              DD(1,I) = DN(1,I)
              DD(2,I) = DN(2,I)
              DD(3,I) = DN(3,I)
            ENDDO
            IF (IRODDL/=0) THEN
              DO I = NODFT ,NODLT
                DDR(1,I) = DNR(1,I)
                DDR(2,I) = DNR(2,I)
                DDR(3,I) = DNR(3,I)
              ENDDO
            END IF
          END IF
        ELSE
          BDT = HALF*DT0_IMP*DT0_IMP*(ONE-TWO*DY_B)
          BDT = ZERO
          DO I = NODFT ,NODLT
            DD(1,I) = BFAC*DN(1,I)+BDT*DY_A(1,I)
            DD(2,I) = BFAC*DN(2,I)+BDT*DY_A(2,I)
            DD(3,I) = BFAC*DN(3,I)+BDT*DY_A(3,I)
          END DO
          IF (IRODDL/=0) THEN
            DO I = NODFT ,NODLT
              DDR(1,I) = BFAC*DNR(1,I)+BDT*DY_AR(1,I)
              DDR(2,I) = BFAC*DNR(2,I)+BDT*DY_AR(2,I)
              DDR(3,I) = BFAC*DNR(3,I)+BDT*DY_AR(3,I)
            END DO
          END IF
        END IF
C------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  PR_DEB                        source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE PR_DEB(NDDL  ,IDDL  ,NDOF   ,IKC   ,ITAB   ,
     1                  DIAG_K,DIAG_M,INLOC  ,FR_ELEM,IAD_ELEM,
     2                  IADK  ,JDIK  ,LT_K   ,LT_M   ,NDDLI  ,
     3                  IADI  ,JDII  ,ITOK   ,DIAG_I ,LT_I   ,
     4                  U     ,F     ,IT     ,NSREM  ,NSL    ,
     5                  D     ,DR    ,IFLAG  ,W_DDL  ,FEXT   ,
     6                  MEXT  ,FINT  ,MINT   ,R01    ,NDEB   ,NODGLOB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_FRK
        USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
        INTEGER NDDL  ,IDDL(*) ,NDOF(*)   ,IKC(*)   ,ITAB(*),IFLAG,
     .          FR_ELEM(*),IAD_ELEM(2,*),INLOC(*),IADK(*),JDIK(*),NODGLOB(*),
     .          NDDLI,IADI(*),JDII(*),ITOK(*),IT,NSREM  ,NSL,W_DDL(*),NDEB
        my_real
     .    DIAG_K(*),DIAG_M(*),LT_K(*),LT_M(*) ,DIAG_I(*),LT_I(*),
     .    U(*),F(*),D(3,*),DR(3,*),FEXT(3,*),MEXT(3,*),FINT(3,*),
     .    R01 ,MINT(3,*),VQ(3,3)
C-----------------------------------------------
c FUNCTION: print-out selected values in function of Ifalg
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   NDDL,NDOF(N)      - num. of total DOF after condensation; Num of DOF of node N
c  I   IDDL(N)           - ID of DOF (before condensation) num of node N: IDDL(N)+1,NDOF(N)
c  I   IKC(NDDL0)        - different independent dof, IKC()=0-> independent dof
c  I   ITAB(N)           - user's node id
c  I   IADK(NDDL+1),JDIK(NNZ)-indices of assemblaged [K] of compressed format
c  I   DIAG_K(NDDL),LT_K(NNZ)-diag_[K] and compressed non zero strick trianluar [K]
c  I   IADM(NDDL+1),JDIM(NNZ)-same than [K], [M]: preconditioner matrix used for PCG only
c  I   DIAG_M(NDDL),LT_M(NNZ)-
c  I   IADI(NDDLI+1),JDII(NNZI)-indices of local [K_i] of contact spring
c  I   ITOK(NDDLI),DIAG_I(NDDL),LT_I(NNZ)- indice from local to glocal [K]
c  I   NSREM ,NSL        -remote SECONDARY and local communated SECONDARY node number (due to // contact)
c  I   U(NDDL),F(NDDL)   - U not available, F-> force Residual {Fext}-{Fint}
c  I   IT                -nonlinear iteration number
c  I   D,DR(3,NUMNOD)    -nodal displacement and rotation
c  I   IFLAG             -print-out options ,see below
c  I   W_DDL             -flag of physical presence of the boundary dof of diff. domains //
c  I   FEXT,MEXT(3,NUMNOD)- external nodal forces and moments
c  I   FINT,MINT(3,NUMNOD)- internal nodal forces and moments
c  I   R01                -force residual norm
c  I   Ndeb               begin of debugging No of cycle.
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
Ctmp +3
        INTEGER i,j,N,ID,ND,IDDLM(NUMNOD),NKC,IDF,nnod,nk,iad,iad2,id2,
     .          IDDL_FRONT(NDDL),ID2N(NUMNOD),NKFRONT,INOD,K,NN,II,
     .          IDDL_FRONT1(NSPMD,NDDL),ITAG(2,NSPMD),INDEX,ILOC,JJ,KK
        CHARACTER CHIF
        CHARACTER*9 FILNAME
        my_real
     .    s_max,xl,yl,zl
C------iflag=1 ->U,F;iflag=2 ->+ [ki];iflag=3 ->+ [k];--iflag=4 ->+[m];
        IDF = ispmd+11
        WRITE(CHIF,'(I1)')ispmd
        FILNAME='DEB'//CHIF//'.TMP'
        OPEN(UNIT=IDF,FILE=FILNAME,STATUS='UNKNOWN',FORM='FORMATTED')
        write(IDF,*)'NCYCLE,IT,NUMNOD,NDDL=',NCYCLE,IT,NUMNOD,NDDL
        write(IDF,*)'NDDLI,NSREM,NSL,IMCONV,NDEB=', NDDLI,NSREM ,NSL,
     .                IMCONV,NDEB
        if (IMCONV==-1.and.(NSREM +NSL)==0) then
          return
        end if
        write(IDF,*)'R01=',R01
c        write(IDF,*)'nddlfr=',nddlfr
        if (NCYCLE==(NDEB+1).AND.IT==0.AND.IMCONV>=0) then
          NKC=0
          DO N =1,NUMNOD
            I=INLOC(N)
            IDDLM(I)=IDDL(I)-NKC
            DO J=1,NDOF(I)
              ND = IDDL(I)+J
              IF (IKC(ND)/=0) NKC = NKC + 1
            ENDDO
c       if (NDOF(I)/=0)
            write(IDF,*)'I,ITAB,NDOF,IDDLM=',I,ITAB(I),NDOF(I),
     .                    IDDLM(I),NODGLOB(I)
          ENDDO
        endif
c        if (NSPMD==1) then
c         I = 849965
        II=0
        S_MAX=ZERO
        NN =0
        K=0
        NKC=0
        DO N =1,NUMNOD
          I=INLOC(N)
          IDDLM(I)=IDDL(I)-NKC
          JJ=0
          DO J=1,NDOF(I)
            ND = IDDL(I)+J
            IF (IKC(ND)/=0) THEN
              NKC = NKC + 1
            ELSE
              JJ = JJ + 1
              ID =IDDLM(I) + JJ
              ID2N(ID)=I
              write(IDF,*)'DIAG_K,F,N,id=',DIAG_K(ID),F(ID),ITAB(I),id
              if (abs(f(id))>S_MAX) THEN
                S_MAX= abs(f(id))
                II = ID
                NN = I
                K=J
              endif
            END IF
          ENDDO
        ENDDO
        if (nn>0) write(IDF,*)'MAX_F,N,J=',F(II),ITAB(NN),K
c        DO N =1,NDDL
c         I = ITOK(N)
c         I = N
c         write(IDF,*),'NC,DIAG_k,DIAG_M=',
c     .      IADK(I+1)-IADK(I),DIAG_K(I),DIAG_M(I)
c         write(IDF,*)'DIAG_K,F(I)=',DIAG_K(I),F(I),I
c         if (abs(f(i))>S_MAX) THEN
c         S_MAX= abs(f(i))
c         II = I
c         endif
c        ENDDO
c         write(IDF,*)'MAX_F=',F(II),II
        if (iflag>1) then
          DO N =1,NUMNOD
            I=INLOC(N)
            write(IDF,*)'FEXT,MEXT,I,ITAB=',I,ITAB(I)
            write(IDF,*)FEXT(1,I),FEXT(2,I),FEXT(3,I)
            IF (IRODDL/=0)write(IDF,*)MEXT(1,I),MEXT(2,I),MEXT(3,I)
          ENDDO
          DO N =1,NUMNOD
            I=INLOC(N)
            write(IDF,*)'FINT,MINT,I,ITAB=',I,ITAB(I)
            write(IDF,*)FINT(1,I),FINT(2,I),FINT(3,I)
c         IF (IRODDL/=0)write(IDF,*)MINT(1,I),MINT(2,I),MINT(3,I)
c         XL = VQ(1,1)*FINT(1,I)+ VQ(1,2)*FINT(2,I)+VQ(1,3)*FINT(3,I)
c         YL = VQ(2,1)*FINT(1,I)+ VQ(2,2)*FINT(2,I)+VQ(2,3)*FINT(3,I)
c         ZL = VQ(3,1)*FINT(1,I)+ VQ(3,2)*FINT(2,I)+VQ(3,3)*FINT(3,I)
c         write(IDF,*)'FINT,local=',I,ITAB(I)
c         write(IDF,*)XL,YL,ZL
          ENDDO
        endif
        if (iflag>=1) then
          DO N =1,NUMNOD
            I=INLOC(N)
c          if (NDOF(I)/=0) then
            write(IDF,*)'D,DR,I,ITAB=',I,ITAB(I)
            write(IDF,*)D(1,I),D(2,I),D(3,I)
            IF (IRODDL/=0)write(IDF,*)DR(1,I),DR(2,I),DR(3,I)
c          end if !(NDOF(I)/=0) then
          ENDDO
        endif
        if (NSPMD>1) then
C
          IF (INTP_D>0) THEN
            write(IDF,*)'NDDL_SL,NDDL_SI=',NDDL_SL,NDDL_SI
            DO I=1,NSL
              N=ISL(I)
              write (IDF,*)'NS,I=',N,ITAB(N),I
            END DO
            DO I=1,NDDL_SL
              ID=IDDL_SL(I)
              write(IDF,*)'ID,DIAG_SL=',ID,DIAG_SL(I)
            END DO
            DO I=1,NDDL_SL
              DO J = IAD_SS(I), IAD_SS(I+1)-1
                write(IDF,*)'LT_SL,NJ,J=',LT_SL(J),JDI_SL(J),J
              END DO
            END DO
            write (IDF,*)'NDDL_SI=',NDDL_SI
            DO I=1,NDDL_SI
              DO J = IAD_SI(I), IAD_SI(I+1)-1
                write(IDF,*)'LT_SI,NJ,J=',LT_SI(J),JDI_SI(J),J
              END DO
            END DO
          ELSE
            DO I=1,NSL
              N=ISL(I)
              DO J=1,MIN(3,NDOF(N))
                ID = IDDSL(J,I)
                IF (ID>0) write(IDF,*)'ID,DIAG_SL=',ID,DIAG_S(J,I)
              END DO
              write (IDF,*)'NS,I=',N,I
            END DO
          END IF
          IAD = 0
          IAD2 = 0
          write(IDF,*)'LEN_K,LEN_V=',LEN_K,LEN_V
          DO I =1,NSPMD
            NKC=0
            DO NK=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              N=FR_ELEM(NK)
              ND=0
              write(IDF,*)'N,ITAB,IDDLFR,IP=',N,ITAB(N),IDDLFR(NK),I
              DO J=1,NDOF(N)
                IF (IKC(IDDL(N)+J)<1) THEN
c          ND=ND+1
                  ID=IDDLFR(NK)+IAD-NKC+J
                  ID2=IDDLFR(NK)+IAD2-NKC+J
                  write(IDF,*)
     .            'FR_ID,id2,id2k,NC=',ID,id2,IFR2K(id2),IADFR(ID+1)-IADFR(ID)
                  IF (IADFR(ID+1)<IADFR(ID)) write(IDF,*)IADFR(ID+1),IADFR(ID)
                ELSE
                  NKC=NKC+1
                ENDIF
              ENDDO
            ENDDO
            IAD = IAD + ND_FR(I) + 1
            IAD2 = IAD2 + ND_FR(I)
          ENDDO
        endif
        if (iflag>=1) then
          write(IDF,*)'[Ki]=',NDDLI
          DO I =1,NDDLI
            N=ID2N(ITOK(I))
            write(IDF,*)'DIAG_I,itok,N=',DIAG_I(I),ITOK(I),ITAB(N)
          ENDDO
          DO I =1,NDDLI
            N=ID2N(ITOK(I))
            write(IDF,*)'NR,I(itok),N=',IADI(I+1)-IADI(I),ITOK(I),ITAB(N)
            DO J=IADI(I),IADI(I+1)-1
              N=ID2N(ITOK(JDII(J)))
              write(IDF,*)'LT_I,_id_NJ,NJ,J=',LT_I(J),ITOK(JDII(J)),ITAB(N),J
            ENDDO
          ENDDO
        endif
        if (iflag>1) then
          write(IDF,*)'LT_[K]=',NDDL
          DO I =1,NDDL
            write(IDF,*)'NR,I=',IADK(I+1)-IADK(I),I
            DO J=IADK(I),IADK(I+1)-1
              write(IDF,*)'LT_K,NJ,J=',LT_K(J),JDIK(J),J
            ENDDO
          ENDDO
        endif
C------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  PR_MATRIX                     source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE PR_MATRIX(NDDL  ,IDDL  ,NDOF   ,IKC   ,ITAB   ,
     1                     DIAG_K,DIAG_M,INLOC  ,FR_ELEM,IAD_ELEM,
     2                     IADK  ,JDIK  ,LT_K   ,LT_M   ,NDDLI  ,
     3                     IADI  ,JDII  ,ITOK   ,DIAG_I ,LT_I   ,
     4                     IFLAG ,it    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_FRK
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
        INTEGER NDDL  ,IDDL(*) ,NDOF(*)   ,IKC(*)   ,ITAB(*),IFLAG,
     .          FR_ELEM(*),IAD_ELEM(2,*),INLOC(*),IADK(*),JDIK(*),
     .          NDDLI,IADI(*),JDII(*),ITOK(*),it
        my_real
     .    DIAG_K(*),DIAG_M(*),LT_K(*),LT_M(*) ,DIAG_I(*),LT_I(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
Ctmp +3
        INTEGER i,j,N,ID,ND,IDDLM(NUMNOD),NKC,IDF,nnod,nk,iad,iad2,id2,
     .          NKFRONT,INOD,K,NN,II,INDEX,ILOC,JJ,KK
        CHARACTER CHIF
        CHARACTER*9 FILNAME
        my_real
     .    s_max,xl,yl,zl
C------
        IDF = ispmd+15
        WRITE(CHIF,'(I1)')ispmd
        FILNAME='MAT'//CHIF//'.TMP'
        OPEN(UNIT=IDF,FILE=FILNAME,STATUS='UNKNOWN',FORM='FORMATTED')
        write(IDF,*)'NCYCLE,NUMNOD,NDDL,NDI=',NCYCLE,NUMNOD,NDDL,NDDLI
        if (IMCONV<0) return
c        write(IDF,*)'nddlfr=',nddlfr
        if (NCYCLE==1) then
          NKC=0
          DO N =1,NUMNOD
            I=INLOC(N)
            IDDLM(I)=IDDL(I)-NKC
            DO J=1,NDOF(I)
              ND = IDDL(I)+J
              IF (IKC(ND)/=0) NKC = NKC + 1
            ENDDO
            if (NDOF(I)/=0)
     .       write(IDF,*)'I,ITAB,NDOF,IDDL=',I,ITAB(I),NDOF(I),
     .                     IDDL(I)
          ENDDO
        endif
        DO N =1,NDDL
          I = N
c         write(IDF,*),'NC,DIAG_k,DIAG_M=',
c     .      IADK(I+1)-IADK(I),DIAG_K(I),DIAG_M(I)
          write(IDF,*)'DIAG_K,DIAG_M=',DIAG_K(I),DIAG_M(I),I
        ENDDO
        if (NSPMD>1) then
C
          IAD = 0
          IAD2 = 0
          write(IDF,*)'LEN_K,LEN_V=',LEN_K,LEN_V
          DO I =1,NSPMD
            NKC=0
            DO NK=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              N=FR_ELEM(NK)
              ND=0
              write(IDF,*)'N,ITAB,IDDLFR,IP=',N,ITAB(N),IDDLFR(NK),I
              DO J=1,NDOF(N)
                IF (IKC(IDDL(N)+J)<1) THEN
c          ND=ND+1
                  ID=IDDLFR(NK)+IAD-NKC+J
                  ID2=IDDLFR(NK)+IAD2-NKC+J
                  write(IDF,*)
     .            'FR_ID,id2,id2k,NC=',ID,id2,IFR2K(id2),IADFR(ID+1)-IADFR(ID)
                  IF (IADFR(ID+1)<IADFR(ID)) write(IDF,*)IADFR(ID+1),IADFR(ID)
                ELSE
                  NKC=NKC+1
                ENDIF
              ENDDO
            ENDDO
            IAD = IAD + ND_FR(I) + 1
            IAD2 = IAD2 + ND_FR(I)
          ENDDO
        endif
        if (NDDLI>0.AND.IFLAG>0) then
          write(IDF,*)'[Ki]=',NDDLI
          DO I =1,NDDLI
            write(IDF,*)'DIAG_I,itok=',DIAG_I(I),ITOK(I)
          ENDDO
          DO I =1,NDDLI
            write(IDF,*)'NR,I=',IADI(I+1)-IADI(I),I
            DO J=IADI(I),IADI(I+1)-1
              write(IDF,*)'LT_I,NJ,J=',LT_I(J),ITOK(JDII(J)),J
            ENDDO
          ENDDO
        endif
        if (NDDL>1.and.IFLAG>1) then
          write(IDF,*)'LT_[K]=',NDDL
          DO I =1,NDDL
            write(IDF,*)'NR,I=',IADK(I+1)-IADK(I),I
            DO J=IADK(I),IADK(I+1)-1
              write(IDF,*)'LT_K,NJ,J=',LT_K(J),JDIK(J),J
            ENDDO
          ENDDO
          if (IFLAG>1) then
            write(IDF,*)'LT_[M]=',NDDL
            DO I =1,NDDL
              write(IDF,*)'NR,I=',IADK(I+1)-IADK(I),I
              DO J=IADK(I),IADK(I+1)-1
                write(IDF,*)'LT_M,NJ,J=',LT_M(J),JDIK(J),J
              ENDDO
            ENDDO
          endif !(IFLAG>1) then
        endif
C------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  PR_SOLNFO                     source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        SPMD_MAX_F                    source/mpi/implicit/imp_spmd.F
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE PR_SOLNFO(NDDL  ,IDDL  ,NDOF   ,IKC   ,ITAB   ,
     1                  DIAG_K,DIAG_M,INLOC  ,FR_ELEM,IAD_ELEM,
     2                  IADK  ,JDIK  ,LT_K   ,LT_M   ,NDDLI  ,
     3                  IADI  ,JDII  ,ITOK   ,DIAG_I ,LT_I   ,
     4                  U     ,F     ,IT     ,NSREM  ,NSL    ,
     5                  D     ,DR    ,IFLAG  ,W_DDL  ,FEXT   ,
     6                  MEXT  ,FINT  ,MINT   ,R01    ,NDEB   ,
     7                  R_IMP ,I_IMP ,DD     ,DDR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_FRK
        USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
#include      "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
        INTEGER NDDL  ,IDDL(*) ,NDOF(*)   ,IKC(*)   ,ITAB(*),IFLAG,
     .          FR_ELEM(*),IAD_ELEM(2,*),INLOC(*),IADK(*),JDIK(*),
     .          NDDLI,IADI(*),JDII(*),ITOK(*),IT,NSREM  ,NSL,W_DDL(*),
     .          NDEB,I_IMP(*)
        my_real
     .    DIAG_K(*),DIAG_M(*),LT_K(*),LT_M(*) ,DIAG_I(*),LT_I(*),
     .    U(*),F(*),D(3,*),DR(3,*),FEXT(3,*),MEXT(3,*),FINT(3,*),
     .    R01 ,MINT(3,*),VQ(3,3),R_IMP(*),DD(3,*),DDR(3,*)
C-----------------------------------------------
c FUNCTION: print-out solver info such as maximum residual, relative residuals, line-search coeff, tolerance
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,N,ID,ND,IDDLM(NUMNOD),NKC,IDF,NNOD,NK,IAD,IAD2,
     .          IDDL_FRONT(NDDL),ID2N(NUMNOD),NKFRONT,INOD,ID2,CUMUL_IT,
     .          IDDL_FRONT1(NSPMD,NDDL),ITAG(2,NSPMD),INDEX,ILOC,JJ,KK,
     .          K,K2,K3,KR,K2R,K3R,NN,NN2,NN3,NNR,NN2R,NN3R,II,IIR,II2,
     .          ITABMAX,IRTABMAX,ITABMAX2,IRTABMAX2,ITABMAX3,IRTABMAX3
        my_real
     .    F_MAX,FR_MAX,D_MAX,DR_MAX,DD_MAX,DDR_MAX,
     .    FMAX,FRMAX,DMAX,DRMAX,DDMAX,DDRMAX,TOLE,TOLF,TOLU
        CHARACTER NODEID*10,FILNAM*100
c       if (IMCONV==-1.and.(NSREM +NSL)==0) then
c         return
c       end if
c       if (NCYCLE==(NDEB+1).AND.IT==0.AND.IMCONV>=0) then
c        NKC=0
c       DO N =1,NUMNOD
c        I=INLOC(N)
c        IDDLM(I)=IDDL(I)-NKC
c        DO J=1,NDOF(I)
c         ND = IDDL(I)+J
c         IF (IKC(ND)/=0) NKC = NKC + 1
c        ENDDO
c       ENDDO
c       endif
        II=0
        IIR=0
        F_MAX=ZERO
        FR_MAX=ZERO
        D_MAX=ZERO
        DR_MAX=ZERO
        DD_MAX=ZERO
        DDR_MAX=ZERO
        FMAX=ZERO
        FRMAX=ZERO
        DMAX=ZERO
        DRMAX=ZERO
        DDMAX=ZERO
        DDRMAX=ZERO
        NN=0
        NN2=0
        NN3=0
        K=0
        K2=0
        K3=0
        NNR=0
        NN2R=0
        NN3R=0
        KR=0
        K2R=0
        K3R=0
        NKC=0
        DO N =1,NUMNOD
          I=INLOC(N)
          IDDLM(I)=IDDL(I)-NKC
          JJ=0
          DO J=1,NDOF(I)
            ND = IDDL(I)+J
            IF (IKC(ND)/=0) THEN
              NKC = NKC + 1
            ELSE
              JJ = JJ + 1
              ID =IDDLM(I) + JJ
              ID2 = IDDLM(I)
c          ID2N(ID)=I
              IF (J < 4) THEN
                IF (abs(F(ID))>F_MAX) THEN
                  F_MAX= abs(F(ID))
                  II = ID
                  NN = I
                  K = J
                ENDIF
                IF (abs(D(J,I))>D_MAX) THEN
                  D_MAX= abs(D(J,I))
                  NN2 = I
                  K2 = J
                ENDIF
                IF (abs(DD(J,I))>DD_MAX) THEN
                  DD_MAX= abs(DD(J,I))
                  NN3 = I
                  K3 = J
                ENDIF
              ELSE
                IF (abs(F(ID))>FR_MAX) THEN
                  FR_MAX= abs(F(ID))
                  IIR = ID
                  NNR = I
                  KR = J
                ENDIF
                IF (abs(DR(J-3,I))>DR_MAX) THEN
                  DR_MAX= abs(DR(J-3,I))
                  NN2R = I
                  K2R = J
                ENDIF
                IF (abs(DDR(J-3,I))>DDR_MAX) THEN
                  DDR_MAX= abs(DDR(J-3,I))
                  NN3R = I
                  K3R = J
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDDO
        IF (II /= 0) FMAX = F(II)
        ITABMAX = ITAB(NN)
        IF (IIR /= 0) FRMAX = F(IIR)
        IRTABMAX = ITAB(NNR)
        IF (NN2 /= 0) DMAX = D(K2,NN2)
        ITABMAX2 = ITAB(NN2)
        IF (NN2R /= 0) DRMAX = DR(K2R-3,NN2R)
        IRTABMAX2 = ITAB(NN2R)
        IF (NN3 /= 0) DDMAX = DD(K3,NN3)
        ITABMAX3 = ITAB(NN3)
        IF (NN3R /= 0) DDRMAX = DDR(K3R-3,NN3R)
        IRTABMAX3 = ITAB(NN3R)
c      Compute maximum value F(II) with several processes in SPMD
        IF (NSPMD>1) THEN
          CALL SPMD_MAX_F(FMAX,ITABMAX,K)
          CALL SPMD_MAX_F(DMAX,ITABMAX2,K2)
          CALL SPMD_MAX_F(DDMAX,ITABMAX3,K3)
          CALL SPMD_MAX_F(FRMAX,IRTABMAX,KR)
          CALL SPMD_MAX_F(DRMAX,IRTABMAX2,K2R)
          CALL SPMD_MAX_F(DDRMAX,IRTABMAX3,K3R)
        END IF
c      Write solver information
        IF (ISPMD==0) THEN
          IF (IT == ZERO) THEN
            WRITE(ISOLINFO,1667) NCYCLE
          ENDIF
          WRITE(ISOLINFO,1060) IT
          WRITE(ISOLINFO,1066) FMAX,ITABMAX,K
          WRITE(ISOLINFO,1064) DMAX,ITABMAX2,K2
          WRITE(ISOLINFO,1062) DDMAX,ITABMAX3,K3
          WRITE(ISOLINFO,1065) FRMAX,IRTABMAX,KR
          WRITE(ISOLINFO,1063) DRMAX,IRTABMAX2,K2R
          WRITE(ISOLINFO,1061) DDRMAX,IRTABMAX3,K3R
          WRITE (NODEID,'(I10)') ITABMAX
          CUMUL_IT = I_IMP(1)+IT
c        Write monitor file
          IF (K==0) THEN
            WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxResidualForce  ',NODEID//'  ',FMAX
          ELSEIF (K==1) THEN
            WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxResidualForce  ',NODEID//'_X',FMAX
          ELSEIF (K==2) THEN
            WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxResidualForce  ',NODEID//'_Y',FMAX
          ELSEIF (K==3) THEN
            WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxResidualForce  ',NODEID//'_Z',FMAX
          ENDIF
c         WRITE (NODEID,'(I10)') ITABMAX2
c         IF (K2==0) THEN
c           WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxIncrementDisp  ',NODEID//'  ',DMAX
c         ELSEIF (K2==1) THEN
c           WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxIncrementDisp  ',NODEID//'_X',DMAX
c         ELSEIF (K2==2) THEN
c           WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxIncrementDisp  ',NODEID//'_Y',DMAX
c         ELSEIF (K2==3) THEN
c           WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxIncrementDisp  ',NODEID//'_Z',DMAX
c         ENDIF
          WRITE (NODEID,'(I10)') ITABMAX3
          IF (K3==0) THEN
            WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxCorrectionDisp ',NODEID//'  ',DDMAX
          ELSEIF (K3==1) THEN
            WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxCorrectionDisp ',NODEID//'_X',DDMAX
          ELSEIF (K3==2) THEN
            WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxCorrectionDisp ',NODEID//'_Y',DDMAX
          ELSEIF (K3==3) THEN
            WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxCorrectionDisp ',NODEID//'_Z',DDMAX
          ENDIF
          WRITE (NODEID,'(I10)') IRTABMAX
          IF (KR==0) THEN
            WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxResidualMoment ',NODEID//'  ',FRMAX
          ELSEIF (KR==4) THEN
            WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxResidualMoment ',NODEID//'_X',FRMAX
          ELSEIF (KR==5) THEN
            WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxResidualMoment ',NODEID//'_Y',FRMAX
          ELSEIF (KR==6) THEN
            WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxResidualMoment ',NODEID//'_Z',FRMAX
          ENDIF
c         WRITE (NODEID,'(I10)') IRTABMAX2
c         IF (K2R==0) THEN
c           WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxIncrementRota  ',NODEID//'  ',DRMAX
c         ELSEIF (K2R==4) THEN
c           WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxIncrementRota  ',NODEID//'_X',DRMAX
c         ELSEIF (K2R==5) THEN
c           WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxIncrementRota  ',NODEID//'_Y',DRMAX
c         ELSEIF (K2R==6) THEN
c           WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxIncrementRota  ',NODEID//'_Z',DRMAX
c         ENDIF
          WRITE (NODEID,'(I10)') IRTABMAX3
          IF (K3R==0) THEN
            WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxCorrectionRota ',NODEID//'  ',DDRMAX
          ELSEIF (K3R==4) THEN
            WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxCorrectionRota ',NODEID//'_X',DDRMAX
          ELSEIF (K3R==5) THEN
            WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxCorrectionRota ',NODEID//'_Y',DDRMAX
          ELSEIF (K3R==6) THEN
            WRITE(ISOLMNTR,1160) CUMUL_IT,0,' MaxCorrectionRota ',NODEID//'_Z',DDRMAX
          ENDIF
          WRITE(NODEID,'(I10)') 0
          WRITE(ISOLMNTR,1160) CUMUL_IT,0,' NormResidualDisp  ',NODEID//'  ',R_IMP(20)
          WRITE(ISOLMNTR,1160) CUMUL_IT,0,' NormResidualForce ',NODEID//'  ',R_IMP(21)
          WRITE(ISOLMNTR,1160) CUMUL_IT,0,' NormResidualEnergy',NODEID//'  ',R_IMP(22)
          WRITE(ISOLMNTR,1160) CUMUL_IT,0,' DimContactMatrix  ',NODEID//'  ',FLOAT(I_IMP(13))
          WRITE(ISOLMNTR,1160) CUMUL_IT,0,' TimeStep          ',NODEID//'  ',DT2
C         CALL MY_FLUSH(ISOLMNTR)
          CALL FLUSH(ISOLMNTR)
          IF (IMCONV == 1) THEN
            IF (NITOL == 1) THEN
              WRITE(ISOLINFO,1067)
            ELSE IF (NITOL == 2) THEN
              WRITE(ISOLINFO,1069)
            ELSE IF (NITOL == 3) THEN
              WRITE(ISOLINFO,1071)
            ELSE IF (NITOL == 12) THEN
              WRITE(ISOLINFO,1073)
            ELSE IF (NITOL == 13) THEN
              WRITE(ISOLINFO,1075)
            ELSE IF (NITOL == 23) THEN
              WRITE(ISOLINFO,1077)
            ELSE IF (NITOL == 123) THEN
              WRITE(ISOLINFO,1079)
            END IF
          ELSE IF (IMCONV == -2) THEN
            IF (NITOL == 1) THEN
              WRITE(ISOLINFO,1068)
            ELSE IF (NITOL == 2) THEN
              WRITE(ISOLINFO,1070)
            ELSE IF (NITOL == 3) THEN
              WRITE(ISOLINFO,1072)
            ELSE IF (NITOL == 12) THEN
              WRITE(ISOLINFO,1074)
            ELSE IF (NITOL == 13) THEN
              WRITE(ISOLINFO,1076)
            ELSE IF (NITOL == 23) THEN
              WRITE(ISOLINFO,1078)
            ELSE IF (NITOL == 123) THEN
              WRITE(ISOLINFO,1080)
            END IF
          ENDIF
          IF (NITOL == 1) THEN
            TOLE = N_TOL
            TOLF = EM02
            TOLU = EM02
          ELSE IF (NITOL == 2) THEN
            TOLE = EM03
            TOLF = N_TOL
            TOLU = EM02
          ELSE IF (NITOL == 3) THEN
            TOLE = EM03
            TOLF = EM02
            TOLU = N_TOL
          ELSE IF (NITOL == 12) THEN
            TOLE = N_TOLE
            TOLF = N_TOLF
            TOLU = EM02
          ELSE IF (NITOL == 13) THEN
            TOLE = N_TOLE
            TOLF = EM02
            TOLU = N_TOLU
          ELSE IF (NITOL == 23) THEN
            TOLE = EM03
            TOLF = N_TOLF
            TOLU = N_TOLU
          ELSE IF (NITOL == 123) THEN
            TOLE = N_TOLE
            TOLF = N_TOLF
            TOLU = N_TOLU
          END IF
          IF (IMCONV == -1) THEN
c         Nothing since line search not ended
          ELSEIF (IMCONV == 0) THEN
            WRITE(ISOLHIST,1666)CUMUL_IT+1,NCYCLE,IT,R_IMP(20),
     .          R_IMP(21),R_IMP(22),TOLE,TOLF,TOLU,
     .          ZERO,ZERO,TT,R_IMP(9)
          ELSEIF (IMCONV == 1) THEN
c         Converged step
            IF (IDTC==3) THEN
              WRITE(ISOLHIST,1668)CUMUL_IT+1,NCYCLE,IT,R_IMP(20),
     .             R_IMP(21),R_IMP(22),TOLE,TOLF,TOLU,
     .             ZERO,ZERO,TT,R_IMP(9),TT,R_IMP(24)
              WRITE(ISOLHIST,1668)CUMUL_IT+1,NCYCLE,IT,R_IMP(20),
     .             R_IMP(21),R_IMP(22),TOLE,TOLF,TOLU,
     .             ONE,ZERO,TT,R_IMP(9),TT,R_IMP(24)
              WRITE(ISOLHIST,1668)CUMUL_IT+1,NCYCLE,IT,R_IMP(20),
     .             R_IMP(21),R_IMP(22),TOLE,TOLF,TOLU,
     .             ZERO,ZERO,TT,R_IMP(9),TT,R_IMP(24)
            ELSE
              WRITE(ISOLHIST,1666)CUMUL_IT+1,NCYCLE,IT,R_IMP(20),
     .             R_IMP(21),R_IMP(22),TOLE,TOLF,TOLU,
     .             ZERO,ZERO,TT,R_IMP(9)
              WRITE(ISOLHIST,1666)CUMUL_IT+1,NCYCLE,IT,R_IMP(20),
     .             R_IMP(21),R_IMP(22),TOLE,TOLF,TOLU,
     .             ONE,ZERO,TT,R_IMP(9)
              WRITE(ISOLHIST,1666)CUMUL_IT+1,NCYCLE,IT,R_IMP(20),
     .             R_IMP(21),R_IMP(22),TOLE,TOLF,TOLU,
     .             ZERO,ZERO,TT,R_IMP(9)
            ENDIF
c         Write .progress file
            REWIND(ISOLPGRS)
            WRITE(ISOLPGRS,'(I4)') CUMUL_IT
            WRITE(ISOLPGRS,'(E11.4)') TT
            WRITE(ISOLPGRS,'(I4)') nint(TT/TSTOP*HUNDRED)
            WRITE(ISOLPGRS,'(I4)') 0
            WRITE(ISOLPGRS,'(I4)') 0
            CALL FLUSH(ISOLPGRS)
          ELSEIF (IMCONV == -2) THEN
c         Diverged step
            WRITE(ISOLHIST,1666)CUMUL_IT+1,NCYCLE,IT,R_IMP(20),
     .           R_IMP(21),R_IMP(22),TOLE,TOLF,TOLU,
     .           ZERO,ZERO,TT,R_IMP(9)
            WRITE(ISOLHIST,1666)CUMUL_IT+1,NCYCLE,IT,R_IMP(20),
     .           R_IMP(21),R_IMP(22),TOLE,TOLF,TOLU,
     .           ZERO,ONE,TT,R_IMP(9)
            WRITE(ISOLHIST,1666)CUMUL_IT+1,NCYCLE,IT,R_IMP(20),
     .           R_IMP(21),R_IMP(22),TOLE,TOLF,TOLU,
     .           ZERO,ZERO,TT,R_IMP(9)
          ENDIF
        ENDIF
1060    FORMAT('       ITERATION',I4)
1061    FORMAT('           LARGEST CORRECTION ROTA.  ',E11.4,
     .         '   AT NODE  ',I10,'   DOF  ',I4)
1062    FORMAT('           LARGEST CORRECTION DISP.  ',E11.4,
     .         '   AT NODE  ',I10,'   DOF  ',I4)
1063    FORMAT('           LARGEST INCREMENT ROTA.   ',E11.4,
     .         '   AT NODE  ',I10,'   DOF  ',I4)
1064    FORMAT('           LARGEST INCREMENT DISP.   ',E11.4,
     .         '   AT NODE  ',I10,'   DOF  ',I4)
1065    FORMAT('           LARGEST RESIDUAL MOMENT   ',E11.4,
     .         '   AT NODE  ',I10,'   DOF  ',I4)
1066    FORMAT('           LARGEST RESIDUAL FORCE    ',E11.4,
     .         '   AT NODE  ',I10,'   DOF  ',I4)
1067    FORMAT('       ACCEPTED EQUILIBRIUM BASED ON SMALL RESIDUAL ENERGY')
1068    FORMAT('       REJECTED EQUILIBRIUM BASED ON SMALL RESIDUAL ENERGY')
1069    FORMAT('       ACCEPTED EQUILIBRIUM BASED ON SMALL RESIDUAL FORCE')
1070    FORMAT('       REJECTED EQUILIBRIUM BASED ON SMALL RESIDUAL FORCE')
1071    FORMAT('       ACCEPTED EQUILIBRIUM BASED ON SMALL RESIDUAL DISP.')
1072    FORMAT('       REJECTED EQUILIBRIUM BASED ON SMALL RESIDUAL DISP.')
1073    FORMAT('       ACCEPTED EQUILIBRIUM BASED ON SMALL RESIDUAL ENERGY AND FORCE')
1074    FORMAT('       REJECTED EQUILIBRIUM BASED ON SMALL RESIDUAL ENERGY AND FORCE')
1075    FORMAT('       ACCEPTED EQUILIBRIUM BASED ON SMALL RESIDUAL ENERGY AND DISP.')
1076    FORMAT('       REJECTED EQUILIBRIUM BASED ON SMALL RESIDUAL ENERGY AND DISP.')
1077    FORMAT('       ACCEPTED EQUILIBRIUM BASED ON SMALL RESIDUAL FORCE AND DISP.')
1078    FORMAT('       REJECTED EQUILIBRIUM BASED ON SMALL RESIDUAL FORCE AND DISP.')
1079    FORMAT('       ACCEPTED EQUILIBRIUM BASED ON SMALL RESIDUAL ENERGY, FORCE AND DISP.')
1080    FORMAT('       REJECTED EQUILIBRIUM BASED ON SMALL RESIDUAL ENERGY, FORCE AND DISP.')
1666    FORMAT(I10,',',I10,',',I10,',',E10.2,',',E10.2,',',E10.2,',',E10.2,',',E10.2,',',E10.2,',',E10.2,
     .         ',',E10.2,',',E10.2,',',E10.2)
1667    FORMAT('   * CYCLE',I6)
1668    FORMAT(I10,',',I10,',',I10,',',E10.2,',',E10.2,',',E10.2,',',E10.2,',',E10.2,',',E10.2,',',E10.2,
     .         ',',E10.2,',',E10.2,',',E10.2,',',E10.2,',',E10.2)
1160    FORMAT(I6,I6,A19,A12,E12.4)
C------------------------------------------
        RETURN
      END

Chd|====================================================================
Chd|  WRITE_TPL_FILE                source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE WRITE_TPL_FILE(FILNAM,IOFF1,IOFF2,IOFF3)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
        CHARACTER(*) FILNAM,IOFF1,IOFF2,IOFF3
        my_real
     .    R01
C-----------------------------------------------
c FUNCTION: print-out tcp file
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J
        my_real
     .    F_MAX
C-----------------------------------------------
        WRITE(ISOLTPL,1000) FILNAM
        WRITE(ISOLTPL,1001)
        WRITE(ISOLTPL,1002) IOFF2,1,4,1,3,4,1,'Residual force',1
     .     ,'Residual force','Residual force','Residual force'
     .     ,'Residual force','Residual force','Residual force'
        WRITE(ISOLTPL,1002) IOFF2,1,4,1,0,4,1,'Tolerance force',1
     .     ,'Tolerance force','Tolerance force','Tolerance force'
     .     ,'Tolerance force','Tolerance force','Tolerance force'
        WRITE(ISOLTPL,1002) IOFF3,1,46,1,3,46,1,'Residual disp.',1
     .     ,'Residual disp.','Residual disp.','Residual disp.'
     .     ,'Residual disp.','Residual disp.','Residual disp.'
        WRITE(ISOLTPL,1002) IOFF3,1,46,1,0,4,1,'Tolerance disp.',1
     .     ,'Tolerance disp.','Tolerance disp.','Tolerance disp.'
     .     ,'Tolerance disp.','Tolerance disp.','Tolerance disp.'
        WRITE(ISOLTPL,1002) IOFF1,1,0,1,3,0,1,'Residual energy',1
     .     ,'Residual energy','Residual energy','Residual energy'
     .     ,'Residual energy','Residual energy','Residual energy'
        WRITE(ISOLTPL,1002) IOFF1,1,0,1,0,4,1,'Tolerance energy',1
     .     ,'Tolerance energy','Tolerance energy','Tolerance energy'
     .     ,'Tolerance energy','Tolerance energy','Tolerance energy'
        WRITE(ISOLTPL,1002) 'On',1,55,1,0,55,1,'Converged step',2
     .     ,'Converged step','Converged step','Converged step'
     .     ,'Converged step','Converged step','Converged step'
        WRITE(ISOLTPL,1002) 'On',1,2,1,0,2,1,'Diverged step',2
     .     ,'Diverged step','Diverged step','Diverged step'
     .     ,'Diverged step','Diverged step','Diverged step'
        WRITE(ISOLTPL,1003)
        WRITE(ISOLTPL,1004) 2,2,2,'Cumulative iterations','Line search coeff'
        WRITE(ISOLTPL,1002) 'On',1,50,1,3,50,1,'Line search coefficient',1
     .     ,'Line search coefficient','Line search coefficient','Line search coefficient'
     .     ,'Line search coefficient','Line search coefficient','Line search coefficient'
        WRITE(ISOLTPL,1003)
        IF (IDTC==3) THEN
          WRITE(ISOLTPL,1004) 3,3,3,'Arc length','Load factor'
          WRITE(ISOLTPL,1006) 'On',1,27,1,3,27,1,'Load factor',1
     .       ,'Load factor','Load factor','Load factor'
     .       ,'Load factor','Load factor','Load factor'
        ELSE
          WRITE(ISOLTPL,1004) 3,3,3,'Cumulative iterations','Time (s)'
          WRITE(ISOLTPL,1002) 'On',1,27,1,3,27,1,'Time',1
     .       ,'Time','Time','Time'
     .       ,'Time','Time','Time'
        ENDIF
        WRITE(ISOLTPL,1005)
1000    FORMAT('    *BeginPage() // Page 1'/
     .         '        *Title("',A,'", On)'/
     .         '        *TitleFont("Arial", 1, 0, 12)'/
     .         '        *Layout(9)'/
c     .       '        *BeginAnimator(Transient)'/
c     .       '            *CurrentTime(Undeformed)'/
c     .       '            *StartTime(0,0000000)'/
c     .       '            *EndTime(1,0000000)'/
c     .       '            *Increment(Forward, Frame, 1, BounceOff)'/
c     .       '        *EndAnimator()'/
     .         '        *WindowIDs(191, 192, 193)'
     .         )
1001    FORMAT('        *ExportFormat("PNG")'/
     .         '        *BeginPlot()'/
     .         '            *PlotType(0)'/
     .         '            *BeginPlotHeader(On)'/
     .         '                *PrimaryFont("Arial", 0, 0, 14)'/
     .         '                *SecondaryFont("Arial", 0, 0, 10)'/
     .         '                *TertiaryFont("Arial", 0, 0, 10)'/
     .         '                *Color(0)'/
     .         '                *Text("Relative residuals")'/
     .         '                *HeaderAlignment(2)'/
     .         '            *EndPlotHeader()'/
     .         '            *BeginPlotFooter(Off)'/
     .         '                *PrimaryFont("Arial", 0, 0, 10)'/
     .         '                *SecondaryFont("Arial", 0, 0, 10)'/
     .         '                *TertiaryFont("Arial", 0, 0, 10)'/
     .         '                *Color(0)'/
     .         '                *Text("{p1w1c1.y.HWRequest} - {p1w1c1.y.HWComponent}")'/
     .         '                *FooterAlignment(2)'/
     .         '            *EndPlotFooter()'/
     .         '            *BeginLegend(On)'/
     .         '                *Font("Arial", 0, 0, 8)'/
     .         '                *BorderWidth(1)'/
     .         '                *Color(0)'/
     .         '                *Leader(Left)'/
     .         '                *Location(BELOW)'/
     .         '                *AutoPosition(False)'/
     .         '                *Reversed(no)'/
     .         '            *EndLegend()'/
     .         '            *UniformAspectRatio(0)'/
     .         '            *FrameColor(66)'/
     .         '            *BackgroundColor(1)'/
     .         '            *GridLineColor(9)'/
     .         '            *ZeroLineColor(0)'/
     .         '            *BeginAxis(X, "Primary", on)'/
     .         '                *Label("Cumulative iterations")'/
     .         '                *Scale(Linear)'/
     .         '                *TicMethod(Increment)'/
     .         '                *Min(0)'/
     .         '                *Max(1)'/
     .         '                *Format(Auto)'/
     .         '                *Precision(5)'/
     .         '                *Increment(10)'/
     .         '                *Grids(1)'/
     .         '                *Color(67)'/
     .         '                *AutoFit(TRUE)'/
     .         '                *LabelFont("Arial", 0, 0, 10)'/
     .         '                *TicsFont("Arial", 0, 0, 8)'/
     .         '                *FitRange(FALSE)'/
     .         '            *EndAxis()'/
     .         '            *BeginAxis(Y, "Primary", on)'/
     .         '                *Label("Relative residual")'/
     .         '                *Scale(Log)'/
     .         '                *TicMethod(PerAxis)'/
     .         '                *Min(0)'/
     .         '                *Max(1)'/
     .         '                *Format(Auto)'/
     .         '                *Precision(4)'/
     .         '                *TicsPerDecade(1)'/
     .         '                *GridsPerDecade(1)'/
     .         '                *Color(67)'/
     .         '                *AutoFit(TRUE)'/
     .         '                *LabelFont("Arial", 0, 0, 10)'/
     .         '                *TicsFont("Arial", 0, 0, 8)'/
     .         '                *FitRange(FALSE)'/
     .         '            *EndAxis()'/
     .         '            *BeginAxis(Y, "Y1", on)'/
     .         '                *Label("")'/
     .         '                *Scale(Linear)'/
     .         '                *TicMethod(PerAxis)'/
     .         '                *Min(0)'/
     .         '                *Max(1)'/
     .         '                *Format(Auto)'/
     .         '                *Precision(0)'/
     .         '                *Tics(2)'/
     .         '                *Grids(2)'/
     .         '                *Color(67)'/
     .         '                *AutoFit(TRUE)'/
     .         '                *LabelFont("Arial", 0, 0, 8)'/
     .         '                *TicsFont("Arial", 0, 0, 8)'/
     .         '                *FitRange(FALSE)'/
     .         '            *EndAxis()'
     .         )
1002    FORMAT('            *BeginCurve(',A,', "{y.HWComponent}")'/
     .         '                *Line(',I2,',',I2,',',I2,')'/
     .         '                *Symbol(',I2,',',I2,',',I2,')'/
     .         '                *Shade(False)'/
     .         '                *Bar(0, 0, 2)'/
     .         '                *ShowInLegend(True)'/
     .         '                *LayerNumber(31)'/
     .         '                *BeginVector(Y, File)'/
     .         '                    *Filename(PLOT_FILE_1)'/
     .         '                    *Datatype("Unknown")'/
     .         '                    *Request("Block 1")'/
     .         '                    *Component("',A,'")'/
     .         '                    *ScaleFactor("1")'/
     .         '                    *Offset("0")'/
     .         '                    *AxisIndex(',I1,')'/
     .         '                    *Attribute("HWReaderHints", "HWReaderHints", "String", "(USE_RXRESULT_READER_FOR_DSY)")'/
     .         '                    *Attribute("HWFile", "File", "String", PLOT_FILE_1)'/
     .         '                    *Attribute("HWSolver", "Solver", "String", "Unknown")'/
     .         '                    *Attribute("HWDatatype", "Datatype", "String", "Unknown")'/
     .         '                    *Attribute("HWRequest", "Request", "String", "Block 1")'/
     .         '                    *Attribute("HWComponent", "Component", "String", "',A,'")'/
     .         '                    *Attribute("HWComplexComponent", "ComplexComponent", "String", "',A,'")'/
     .         '                    *Attribute("HWReader", "Reader", "String", "hgtextcolumn.exe")'/
     .         '                    *Attribute("HWWordSize", "WordSize", "String", "8")'/
     .         '                *EndVector()'/
     .         '                *BeginVector(X, File)'/
     .         '                    *Filename(PLOT_FILE_1)'/
     .         '                    *Datatype("Unknown")'/
     .         '                    *Request("Block 1")'/
     .         '                    *Component("Cumulative iterations")'/
     .         '                    *ScaleFactor("1")'/
     .         '                    *Offset("0")'/
     .         '                    *AxisIndex(1)'/
     .         '                    *Attribute("HWReaderHints", "HWReaderHints", "String", "(USE_RXRESULT_READER_FOR_DSY)")'/
     .         '                    *Attribute("HWFile", "File", "String", PLOT_FILE_1)'/
     .         '                    *Attribute("HWSolver", "Solver", "String", "Unknown")'/
     .         '                    *Attribute("HWDatatype", "Datatype", "String", "Unknown")'/
     .         '                    *Attribute("HWRequest", "Request", "String", "Block 1")'/
     .         '                    *Attribute("HWComponent", "Component", "String", "Cumulative iterations")'/
     .         '                    *Attribute("HWComplexComponent", "ComplexComponent", "String", "Cumulative iterations")'/
     .         '                    *Attribute("HWReader", "Reader", "String", "hgtextcolumn.exe")'/
     .         '                    *Attribute("HWWordSize", "WordSize", "String", "8")'/
     .         '                *EndVector()'/
     .         '                *BeginVector(Time, File)'/
     .         '                    *Filename(PLOT_FILE_1)'/
     .         '                    *Datatype("Time")'/
     .         '                    *ScaleFactor("1")'/
     .         '                    *Offset("0")'/
     .         '                    *Attribute("HWReaderHints", "HWReaderHints", "String", "(USE_RXRESULT_READER_FOR_DSY)")'/
     .         '                    *Attribute("HWFile", "File", "String", PLOT_FILE_1)'/
     .         '                    *Attribute("HWSolver", "Solver", "String", "Unknown")'/
     .         '                    *Attribute("HWDatatype", "Datatype", "String", "Time")'/
     .         '                    *Attribute("HWRequest", "Request", "String", "Time")'/
     .         '                    *Attribute("HWComponent", "Component", "String", "Time")'/
     .         '                    *Attribute("HWComplexComponent", "ComplexComponent", "String", "Time")'/
     .         '                    *Attribute("HWReader", "Reader", "String", "hgtextcolumn.exe")'/
     .         '                    *Attribute("HWWordSize", "WordSize", "String", "8")'/
     .         '                *EndVector()'/
     .         '                *BeginVector(U, File)'/
     .         '                    *Filename(PLOT_FILE_1)'/
     .         '                    *Datatype("Unknown")'/
     .         '                    *Request("Block 1")'/
     .         '                    *Component("Cumulative iterations")'/
     .         '                    *ScaleFactor("1")'/
     .         '                    *Offset("0")'/
     .         '                    *AxisIndex(1)'/
     .         '                    *Attribute("HWReaderHints", "HWReaderHints", "String", "(USE_RXRESULT_READER_FOR_DSY)")'/
     .         '                    *Attribute("HWFile", "File", "String", PLOT_FILE_1)'/
     .         '                    *Attribute("HWSolver", "Solver", "String", "Unknown")'/
     .         '                    *Attribute("HWDatatype", "Datatype", "String", "Unknown")'/
     .         '                    *Attribute("HWRequest", "Request", "String", "Block 1")'/
     .         '                    *Attribute("HWComponent", "Component", "String", "Cumulative iterations")'/
     .         '                    *Attribute("HWComplexComponent", "ComplexComponent", "String", "Cumulative iterations")'/
     .         '                    *Attribute("HWReader", "Reader", "String", "hgtextcolumn.exe")'/
     .         '                    *Attribute("HWWordSize", "WordSize", "String", "8")'/
     .         '                *EndVector()'/
     .         '                *BeginVector(V, File)'/
     .         '                    *Filename(PLOT_FILE_1)'/
     .         '                    *Datatype("Unknown")'/
     .         '                    *Request("Block 1")'/
     .         '                    *Component("',A,'")'/
     .         '                    *ScaleFactor("1")'/
     .         '                    *Offset("0")'/
     .         '                    *AxisIndex(1)'/
     .         '                    *Attribute("HWReaderHints", "HWReaderHints", "String", "(USE_RXRESULT_READER_FOR_DSY)")'/
     .         '                    *Attribute("HWFile", "File", "String", PLOT_FILE_1)'/
     .         '                    *Attribute("HWSolver", "Solver", "String", "Unknown")'/
     .         '                    *Attribute("HWDatatype", "Datatype", "String", "Unknown")'/
     .         '                    *Attribute("HWRequest", "Request", "String", "Block 1")'/
     .         '                    *Attribute("HWComponent", "Component", "String", "',A,'")'/
     .         '                    *Attribute("HWComplexComponent", "ComplexComponent", "String", "',A,'")'/
     .         '                    *Attribute("HWReader", "Reader", "String", "hgtextcolumn.exe")'/
     .         '                    *Attribute("HWWordSize", "WordSize", "String", "8")'/
     .         '                *EndVector()'/
     .         '                *Attribute("HWLastGoodCurveName", "HWLastGoodCurveName", "String", "',A,'")'/
     .         '            *EndCurve()'
     .         )
1003    FORMAT('        *EndPlot()')
1004    FORMAT('        *ExportFormat("PNG")'/
     .         '        *BeginPlot()'/
     .         '            *PlotType(0)'/
     .         '            *BeginPlotHeader(On)'/
     .         '                *PrimaryFont("Arial", 0, 0, 14)'/
     .         '                *SecondaryFont("Arial", 0, 0, 10)'/
     .         '                *TertiaryFont("Arial", 0, 0, 10)'/
     .         '                *Color(0)'/
     .         '                *Text("{p1w',I1,'c1.y.HWComponent}")'/
     .         '                *HeaderAlignment(2)'/
     .         '            *EndPlotHeader()'/
     .         '            *BeginPlotFooter(Off)'/
     .         '                *PrimaryFont("Arial", 0, 0, 10)'/
     .         '                *SecondaryFont("Arial", 0, 0, 10)'/
     .         '                *TertiaryFont("Arial", 0, 0, 10)'/
     .         '                *Color(0)'/
     .         '                *Text("{p1w',I1,'c1.y.HWRequest} - {p1w',I1,'c1.y.HWComponent}")'/
     .         '                *FooterAlignment(2)'/
     .         '            *EndPlotFooter()'/
     .         '            *BeginLegend(On)'/
     .         '                *Font("Arial", 0, 0, 8)'/
     .         '                *BorderWidth(1)'/
     .         '                *Color(0)'/
     .         '                *Leader(Left)'/
     .         '                *Location(BELOW)'/
     .         '                *AutoPosition(False)'/
     .         '                *Reversed(no)'/
     .         '            *EndLegend()'/
     .         '            *UniformAspectRatio(0)'/
     .         '            *FrameColor(66)'/
     .         '            *BackgroundColor(1)'/
     .         '            *GridLineColor(9)'/
     .         '            *ZeroLineColor(0)'/
     .         '            *BeginAxis(X, "Primary", on)'/
     .         '                *Label("',A,'")'/
     .         '                *Scale(Linear)'/
     .         '                *TicMethod(Increment)'/
     .         '                *Min(0)'/
     .         '                *Max(1)'/
     .         '                *Format(Auto)'/
     .         '                *Precision(5)'/
     .         '                *Increment(10)'/
     .         '                *Grids(1)'/
     .         '                *Color(67)'/
     .         '                *AutoFit(TRUE)'/
     .         '                *LabelFont("Arial", 0, 0, 10)'/
     .         '                *TicsFont("Arial", 0, 0, 8)'/
     .         '                *FitRange(FALSE)'/
     .         '            *EndAxis()'/
     .         '            *BeginAxis(Y, "Primary", on)'/
     .         '                *Label("',A,'")'/
     .         '                *Scale(Linear)'/
     .         '                *TicMethod(PerAxis)'/
     .         '                *Min(0)'/
     .         '                *Max(1)'/
     .         '                *Format(Auto)'/
     .         '                *Precision(5)'/
     .         '                *Tics(11)'/
     .         '                *Grids(1)'/
     .         '                *Color(67)'/
     .         '                *AutoFit(TRUE)'/
     .         '                *LabelFont("Arial", 0, 0, 10)'/
     .         '                *TicsFont("Arial", 0, 0, 8)'/
     .         '                *FitRange(FALSE)'/
     .         '            *EndAxis()'
     .         )
1005    FORMAT('        *EndPlot()'/
     .         '        *TimeScales(1, 1, 1)'/
     .         '        *TimeDelays(0, 0, 0)'/
     .         '        *AnimationEnable(1, 1, 1)'/
     .         '        *SyncTolerance(2e-008)'/
     .         '        *SyncTableGenerationPolicy(ALL_BLOCKS)'/
     .         '    *EndPage()'/
     .         '*EndDefine()'
     .         )
1006    FORMAT('            *BeginCurve(',A,', "{y.HWComponent}")'/
     .         '                *Line(',I2,',',I2,',',I2,')'/
     .         '                *Symbol(',I2,',',I2,',',I2,')'/
     .         '                *Shade(False)'/
     .         '                *Bar(0, 0, 2)'/
     .         '                *ShowInLegend(True)'/
     .         '                *LayerNumber(31)'/
     .         '                *BeginVector(Y, File)'/
     .         '                    *Filename(PLOT_FILE_1)'/
     .         '                    *Datatype("Unknown")'/
     .         '                    *Request("Block 1")'/
     .         '                    *Component("',A,'")'/
     .         '                    *ScaleFactor("1")'/
     .         '                    *Offset("0")'/
     .         '                    *AxisIndex(',I1,')'/
     .         '                    *Attribute("HWReaderHints", "HWReaderHints", "String", "(USE_RXRESULT_READER_FOR_DSY)")'/
     .         '                    *Attribute("HWFile", "File", "String", PLOT_FILE_1)'/
     .         '                    *Attribute("HWSolver", "Solver", "String", "Unknown")'/
     .         '                    *Attribute("HWDatatype", "Datatype", "String", "Unknown")'/
     .         '                    *Attribute("HWRequest", "Request", "String", "Block 1")'/
     .         '                    *Attribute("HWComponent", "Component", "String", "',A,'")'/
     .         '                    *Attribute("HWComplexComponent", "ComplexComponent", "String", "',A,'")'/
     .         '                    *Attribute("HWReader", "Reader", "String", "hgtextcolumn.exe")'/
     .         '                    *Attribute("HWWordSize", "WordSize", "String", "8")'/
     .         '                *EndVector()'/
     .         '                *BeginVector(X, File)'/
     .         '                    *Filename(PLOT_FILE_1)'/
     .         '                    *Datatype("Unknown")'/
     .         '                    *Request("Block 1")'/
     .         '                    *Component("Arc length")'/
     .         '                    *ScaleFactor("1")'/
     .         '                    *Offset("0")'/
     .         '                    *AxisIndex(1)'/
     .         '                    *Attribute("HWReaderHints", "HWReaderHints", "String", "(USE_RXRESULT_READER_FOR_DSY)")'/
     .         '                    *Attribute("HWFile", "File", "String", PLOT_FILE_1)'/
     .         '                    *Attribute("HWSolver", "Solver", "String", "Unknown")'/
     .         '                    *Attribute("HWDatatype", "Datatype", "String", "Unknown")'/
     .         '                    *Attribute("HWRequest", "Request", "String", "Block 1")'/
     .         '                    *Attribute("HWComponent", "Component", "String", "Arc length")'/
     .         '                    *Attribute("HWComplexComponent", "ComplexComponent", "String", "Arc length")'/
     .         '                    *Attribute("HWReader", "Reader", "String", "hgtextcolumn.exe")'/
     .         '                    *Attribute("HWWordSize", "WordSize", "String", "8")'/
     .         '                *EndVector()'/
     .         '                *BeginVector(Time, File)'/
     .         '                    *Filename(PLOT_FILE_1)'/
     .         '                    *Datatype("Time")'/
     .         '                    *ScaleFactor("1")'/
     .         '                    *Offset("0")'/
     .         '                    *Attribute("HWReaderHints", "HWReaderHints", "String", "(USE_RXRESULT_READER_FOR_DSY)")'/
     .         '                    *Attribute("HWFile", "File", "String", PLOT_FILE_1)'/
     .         '                    *Attribute("HWSolver", "Solver", "String", "Unknown")'/
     .         '                    *Attribute("HWDatatype", "Datatype", "String", "Time")'/
     .         '                    *Attribute("HWRequest", "Request", "String", "Time")'/
     .         '                    *Attribute("HWComponent", "Component", "String", "Time")'/
     .         '                    *Attribute("HWComplexComponent", "ComplexComponent", "String", "Time")'/
     .         '                    *Attribute("HWReader", "Reader", "String", "hgtextcolumn.exe")'/
     .         '                    *Attribute("HWWordSize", "WordSize", "String", "8")'/
     .         '                *EndVector()'/
     .         '                *BeginVector(U, File)'/
     .         '                    *Filename(PLOT_FILE_1)'/
     .         '                    *Datatype("Unknown")'/
     .         '                    *Request("Block 1")'/
     .         '                    *Component("Arc length")'/
     .         '                    *ScaleFactor("1")'/
     .         '                    *Offset("0")'/
     .         '                    *AxisIndex(1)'/
     .         '                    *Attribute("HWReaderHints", "HWReaderHints", "String", "(USE_RXRESULT_READER_FOR_DSY)")'/
     .         '                    *Attribute("HWFile", "File", "String", PLOT_FILE_1)'/
     .         '                    *Attribute("HWSolver", "Solver", "String", "Unknown")'/
     .         '                    *Attribute("HWDatatype", "Datatype", "String", "Unknown")'/
     .         '                    *Attribute("HWRequest", "Request", "String", "Block 1")'/
     .         '                    *Attribute("HWComponent", "Component", "String", "Arc length")'/
     .         '                    *Attribute("HWComplexComponent", "ComplexComponent", "String", "Arc length")'/
     .         '                    *Attribute("HWReader", "Reader", "String", "hgtextcolumn.exe")'/
     .         '                    *Attribute("HWWordSize", "WordSize", "String", "8")'/
     .         '                *EndVector()'/
     .         '                *BeginVector(V, File)'/
     .         '                    *Filename(PLOT_FILE_1)'/
     .         '                    *Datatype("Unknown")'/
     .         '                    *Request("Block 1")'/
     .         '                    *Component("',A,'")'/
     .         '                    *ScaleFactor("1")'/
     .         '                    *Offset("0")'/
     .         '                    *AxisIndex(1)'/
     .         '                    *Attribute("HWReaderHints", "HWReaderHints", "String", "(USE_RXRESULT_READER_FOR_DSY)")'/
     .         '                    *Attribute("HWFile", "File", "String", PLOT_FILE_1)'/
     .         '                    *Attribute("HWSolver", "Solver", "String", "Unknown")'/
     .         '                    *Attribute("HWDatatype", "Datatype", "String", "Unknown")'/
     .         '                    *Attribute("HWRequest", "Request", "String", "Block 1")'/
     .         '                    *Attribute("HWComponent", "Component", "String", "',A,'")'/
     .         '                    *Attribute("HWComplexComponent", "ComplexComponent", "String", "',A,'")'/
     .         '                    *Attribute("HWReader", "Reader", "String", "hgtextcolumn.exe")'/
     .         '                    *Attribute("HWWordSize", "WordSize", "String", "8")'/
     .         '                *EndVector()'/
     .         '                *Attribute("HWLastGoodCurveName", "HWLastGoodCurveName", "String", "',A,'")'/
     .         '            *EndCurve()'
     .         )
C------------------------------------------
        RETURN
      END
c
Chd|====================================================================
Chd|  INT5_DIVERG                   source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INT5_DIVERG(IPARI )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER  IPARI(NPARI,*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER  N, NTY
C--------------------------------------------
        DO N=1,NINTER
          NTY   =IPARI(7,N)
C-----------------------------------------------------------------------
          IF(NTY == 5 ) THEN
C-----------------------------------------------------------------------
            IPARI(16,N)=IPARI(16,N)-1
C-----------------------------------------------------------------------
          ELSEIF(NTY == 10)THEN
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
          ELSEIF(NTY == 11)THEN
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
          ELSEIF(NTY == 24 ) THEN
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
          ENDIF
        END DO
C-----------------------------------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  DIS_CP                        source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|        IMP_QSTAT                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE DIS_CP(N     ,D      ,DR   ,IFLAG   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_QSTAT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER  N,IFLAG
C     REAL
        my_real
     .   D(*),DR(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,ND
C------------------------------------------
        IF (IFLAG ==0 ) THEN
          CALL CP_REAL(N,D, D_N_1)
          IF (IRODDL/=0) CALL CP_REAL(N,DR, DR_N_1)
        ELSE
          CALL CP_REAL(N,D_N_1 ,D )
          IF (IRODDL/=0) CALL CP_REAL(N,DR_N_1,DR )
        END IF
C------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  IMP_SMPINI                    source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        B_CORRECT_HP                  source/implicit/nl_solv.F     
Chd|        CP_INT_HP                     source/implicit/produt_v.F    
Chd|        CP_REAL_HP                    source/implicit/produt_v.F    
Chd|        DU_INI_HP                     source/implicit/imp_solv.F    
Chd|        FRAC_DD_HP                    source/implicit/integrator.F  
Chd|        FRAC_D_HP                     source/implicit/integrator.F  
Chd|        INTEGRATOR1_HP                source/implicit/integrator.F  
Chd|        INTEGRATOR2_HP                source/implicit/integrator.F  
Chd|        INTEGRATORL_HP                source/implicit/integrator.F  
Chd|        INTEGRATOR_HP                 source/implicit/integrator.F  
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|        MUMPSLB_HP                    source/implicit/lin_solv.F    
Chd|        PRODUT_HP                     source/implicit/produt_v.F    
Chd|        PRODUT_UHP0                   source/implicit/produt_v.F    
Chd|        VAXPY_HP                      source/implicit/produt_v.F    
Chd|        VSCALY_HP                     source/implicit/produt_v.F    
Chd|        VSCAL_HP                      source/implicit/produt_v.F    
Chd|        ZEROR_HP                      source/implicit/produt_v.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IMP_SMPINI(
     1   ITSK   ,N1FTSK ,N1LTSK ,N1   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ITSK   ,N1FTSK ,N1LTSK ,N1
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER OMP_GET_THREAD_NUM
        EXTERNAL OMP_GET_THREAD_NUM
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C     Initialisation des variables pour // SMP
C
        ITSK = OMP_GET_THREAD_NUM()
        N1FTSK = 1+ITSK*N1/ NTHREAD
        N1LTSK = (ITSK+1)*N1/ NTHREAD
C
        RETURN
      END
Chd|====================================================================
Chd|  DU_INI_HP                     source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_SMPINI                    source/implicit/imp_solv.F    
Chd|        IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE DU_INI_HP(DN     ,DNR    ,DD    ,
     1                     DDR   ,IDIV   ,ICONT0 )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_DYNA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
#include      "impl2_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IDIV   ,ICONT0
        my_real
     .   DN(3,*),DNR(3,*),DD(3,*),DDR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NODFT ,NODLT,ITSK
        INTEGER I,J,IRES
        my_real
     .    BFAC,BDT
C--------------Dn,0=Dn-1--------
C--------special case with /QSTAT diverge by contact, restart w/o resolution
        IRES = 1
        IF (IQSTAT >0 .AND. IDIV==-2 .AND. ICONT0 >0 ) THEN
          IDIV=-1
          IRES=0
        END IF
C
        BFAC=DT1_IMP/DT0_IMP
!$OMP PARALLEL PRIVATE(ITSK,NODFT ,NODLT,I,BDT)
        CALL IMP_SMPINI(ITSK   ,NODFT ,NODLT,NUMNOD  )
        IF (IDYNA==0) THEN
          IF (ISMDISP>0.OR.IRES==0) THEN
            DO I = NODFT ,NODLT
              DD(1,I) = BFAC*DN(1,I)
              DD(2,I) = BFAC*DN(2,I)
              DD(3,I) = BFAC*DN(3,I)
            END DO
            IF (IRODDL/=0) THEN
              DO I = NODFT ,NODLT
                DDR(1,I) = BFAC*DNR(1,I)
                DDR(2,I) = BFAC*DNR(2,I)
                DDR(3,I) = BFAC*DNR(3,I)
              END DO
            END IF
          ELSE
            DO I = NODFT ,NODLT
              DD(1,I) = DN(1,I)
              DD(2,I) = DN(2,I)
              DD(3,I) = DN(3,I)
            ENDDO
            IF (IRODDL/=0) THEN
              DO I = NODFT ,NODLT
                DDR(1,I) = DNR(1,I)
                DDR(2,I) = DNR(2,I)
                DDR(3,I) = DNR(3,I)
              ENDDO
            END IF
          END IF
        ELSE
          BDT = HALF*DT0_IMP*DT0_IMP*(ONE-TWO*DY_B)
          BDT = ZERO
          DO I = NODFT ,NODLT
            DD(1,I) = BFAC*DN(1,I)+BDT*DY_A(1,I)
            DD(2,I) = BFAC*DN(2,I)+BDT*DY_A(2,I)
            DD(3,I) = BFAC*DN(3,I)+BDT*DY_A(3,I)
          END DO
          IF (IRODDL/=0) THEN
            DO I = NODFT ,NODLT
              DDR(1,I) = BFAC*DNR(1,I)+BDT*DY_AR(1,I)
              DDR(2,I) = BFAC*DNR(2,I)+BDT*DY_AR(2,I)
              DDR(3,I) = BFAC*DNR(3,I)+BDT*DY_AR(3,I)
            END DO
          END IF
        END IF
!$OMP END PARALLEL
C------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  PRINT_STIF                    source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE PRINT_STIF(IPARI,INTBUF_TAB,IFLAG,NN     ,JG    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE TRI7BOX
        USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARI(NPARI,NINTER)
        INTEGER IFLAG, NN     ,JG
        INTEGER LENS, LENR,P,N
        INTEGER IEDGE,NSN

        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C------------------------------------------
        return
        IF (NSPMD==1) THEN
          NSN   =IPARI(5,NN)
          N = 21
          if (NCYCLE>=81.and.NCYCLE<=81) then
c         write(iout,*)'IFLAG,ISPMD=',IFLAG,ISPMD
c         write(iout,*)'IRTLM(1,)=',INBUF(K1)
c      call my_flush(6)
          end if
        ELSE
          LENS = 0
          LENR = 0
          DO P = 1, NSPMD
            LENS = LENS + NSNSI(NN)%P(P)
            LENR = LENR + NSNFI(NN)%P(P)
          END DO
          if (NCYCLE>=80.and.NCYCLE<=81.AND.LENR >0) then
            write(iout,*)'IFLAG,ISPMD=',IFLAG,ISPMD
            write(iout,*)'STIF_OLDFI()%P(1=',STIF_OLDFI(NN)%P(1,JG)
          end if
c      if (NCYCLE>=78.and.NCYCLE<=79.AND.LENS >0) then
c       N = 55
c       K1 = KD(16)+ 2*(N -1)
c         write(iout,*)'IFLAG,ISPMD=',IFLAG,ISPMD
c         write(iout,*)'IRTLM(1,N)=',INBUF(K1)
c      end if
        END IF !(NSPMD==1) THEN
        RETURN
      END
Chd|====================================================================
Chd|  IMP_STIF24                    source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        RE2INT7                       source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        SPMD_ALLGLOB_ISUM9            source/mpi/generic/spmd_allglob_isum9.F
Chd|        IMP_I7CP                      share/modules/imp_intm.F      
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE IMP_STIF24(NUMIMP  ,IPARI   )
C-----------------------------------------------
        USE TRI7BOX
        USE IMP_I7CP
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER  NUMIMP(*),IPARI(NPARI,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,K,L,N,IAD,N_MAX(NINTER),IFLAG,P,
     .         ITY,NSN,IGSTI,IBIT,RID,INTTH,
     .         IGAP,INACTI,IN1CON,NCONT0,NREBOU
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C-----Calcul NCONT0, Communication is necessary
        IF (TT == ZERO.AND.INCONV==1) THEN
          DO N = 1,NINTER
            ITY   =IPARI(7,N)
            IF (ITY==24) THEN
              IPARI(26,N)= 0
              IPARI(53,N)= 0
            END IF
          END DO
        END IF
        IFLAG=0
        DO N = 1,NINTER
          ITY   =IPARI(7,N)
          IGSTI =IPARI(34,N)
          N_MAX(N)=0
          IF (ITY == 24.AND.IGSTI==6) THEN
            N_MAX(N) =NUMIMP(N)
            IFLAG=1
          END IF
        END DO
        IF (IFLAG==0.OR.IMCONV<0) RETURN
C
        IF (NSPMD>1) CALL SPMD_ALLGLOB_ISUM9(N_MAX,NINTER)

        DO N = 1,NINTER
          ITY   =IPARI(7,N)
          IGSTI =IPARI(34,N)
          IF (ITY /= 24.AND.IGSTI/=6) CYCLE
          IN1CON =IPARI(26,N)
          NCONT0 =IPARI(27,N)
          NREBOU =IPARI(53,N)
          IF (N_MAX(N)>0 .AND. IN1CON==0) THEN
            IF (INCONV==1) THEN
              IN1CON = NCYCLE+1
            ELSE
              IN1CON = NCYCLE
            END IF
          END IF
C---------rebound----
          IF (NCONT0>0 .AND.N_MAX(N)==0) THEN
C------first one will keep negative IN1CON till converging
            IF (NCYCLE == IN1CON) THEN
              NREBOU=1
            ELSE
C--------rebound nb >2
              NREBOU=2
            END IF
c        write(iout,*)'NREBOU,NIN,imconv,ispmd=',
c     +                NREBOU,N,imconv,ispmd
C---------negative :rebound active
            IPARI(53,N)=-NREBOU
          END IF
          IPARI(26,N)=IN1CON
          IPARI(27,N)=N_MAX(N)
        END DO
C
        RETURN
      END
Chd|====================================================================
Chd|  UPD_RHS_FR                    source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_INTFR                     source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        EXT_RHS                       source/implicit/upd_glob_k.F  
Chd|        I2_IMPR1                      source/interfaces/interf/i2_imp1.F
Chd|        I2_IMPR2                      source/interfaces/interf/i2_imp1.F
Chd|        RBE2_IMPR1                    source/constraints/general/rbe2/rbe2_imp0.F
Chd|        RBE3_IMPR1                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBE3_IMPR2                    source/constraints/general/rbe3/rbe3_imp0.F
Chd|        RBY_IMPR1                     source/constraints/general/rbody/rby_imp0.F
Chd|        RBY_IMPR2                     source/constraints/general/rbody/rby_imp0.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE UPD_RHS_FR(ICODT ,ICODR ,ISKEW ,IBFV    ,XFRAME ,
     1                   RBY   ,X     ,SKEW   ,LPBY   ,NPBY   ,
     2                   NRBYAC,IRBYAC,NINT2  ,IINT2  ,IPARI  ,
     3                   INTBUF_TAB   ,NDOF   ,IDDL   ,IKC    ,
     4                   NDDL0 ,B     ,IUPD   ,INLOC  ,LJ     ,
     5                   AC    ,ACR   ,NT_RW  ,W_DDL  ,NDDL   ,
     6                   R02   ,IRBE3 ,LRBE3  ,FRBE3  ,WEIGHT ,
     8                   IRBE2 ,LRBE2 )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IBFV(NIFV,*),ICODT(*),ICODR(*),ISKEW(*),
     .          NINT2 ,IINT2(*),LJ(*),NDDL0,IUPD,
     .          INLOC(*),NT_RW,W_DDL(*) ,NDDL
        INTEGER LPBY(*),NPBY(NNPBY,*),NDOF(*),IDDL(*),IKC(*),
     .          IPARI(NPARI,*), NRBYAC,IRBYAC(*)
        INTEGER WEIGHT(*),IRBE3(*),LRBE3(*),IRBE2(*),LRBE2(*)
        my_real
     .     RBY(NRBY,*) ,X(3,*) ,SKEW(*),R02
        my_real
     .    B(*) ,XFRAME(NXFRAME,*),AC(3,*),ACR(3,*),FRBE3(*)

        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,K,N,JI,JB,K1,IFLAG
C-------R02 correction due the fact that IMP_FRI is done after [K],{LB}condensation----------
C-------only Fext is re-computed, others don't change-------
C-------int2,RBE3,rby speciale (Fext seulement)----------
        IF (IUPD==0) THEN
          DO I=1,NINT2
            N=IINT2(I)
            CALL I2_IMPR1(IPARI(1,N),INTBUF_TAB(N),
     .                    X  ,NDOF ,IDDL    ,B  )
          ENDDO
          IF (NRBE2>0) THEN
            CALL RBE2_IMPR1(
     1                      IRBE2  ,LRBE2 ,X     ,SKEW   ,NDOF   ,
     2                      IDDL   ,B     ,WEIGHT)
          ENDIF
          IF (NRBE3>0) THEN
            CALL RBE3_IMPR1(
     1                      IRBE3  ,LRBE3 ,FRBE3  ,X     ,SKEW   ,
     2                      NDOF   ,IDDL  ,B      ,WEIGHT)
          ENDIF
          DO I=1,NRBYAC
            N=IRBYAC(I)
            K1=IRBYAC(I+NRBYKIN)+1
            CALL RBY_IMPR1(X, RBY(1,N),LPBY(K1),NPBY(1,N),
     1                     NDOF  ,IDDL   ,B    )
          ENDDO
        ENDIF
C-------int2,rby speciale (elems deleted)----------
        DO I=1,NINT2
          N=IINT2(I)
          CALL I2_IMPR2(IPARI(1,N),INTBUF_TAB(N) ,AC    ,ACR  ,
     .                  X  ,NDOF ,IDDL    ,B  )
        ENDDO
        IF (NRBE3>0) THEN
          CALL RBE3_IMPR2(
     1                    IRBE3  ,LRBE3 ,FRBE3  ,X     ,SKEW   ,
     2                    NDOF   ,IDDL  ,B      ,WEIGHT,AC     ,
     3                    ACR    )
        ENDIF
        DO I=1,NRBYAC
          N=IRBYAC(I)
          K1=IRBYAC(I+NRBYKIN)+1
          CALL RBY_IMPR2(X, RBY(1,N),LPBY(K1),NPBY(1,N),
     1                   NDOF  ,IDDL   ,B    ,AC    ,ACR  )
        ENDDO
C-------------
        CALL EXT_RHS(ICODT ,ICODR ,ISKEW ,IBFV    ,XFRAME ,
     1               X     ,SKEW  ,NDOF  ,IDDL    ,IKC    ,
     2               NDDL0 ,B     ,INLOC  ,LJ     ,AC     ,
     3               ACR   ,NT_RW ,W_DDL  ,NDDL   ,R02    )
C
        RETURN
      END
Chd|====================================================================
Chd|  IMP_INTFR                     source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        CP_REAL_HP                    source/implicit/produt_v.F    
Chd|        IMP_FRFV                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FRI                       source/mpi/implicit/imp_fri.F 
Chd|        IMP_SETBA                     source/implicit/imp_setb.F    
Chd|        UPD_RHS_FR                    source/implicit/imp_solv.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IMP_INTFR(
     1    NUM_IMP   ,NS_IMP    ,NE_IMP    ,IPARI     ,INTBUF_TAB,
     2    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,
     3    IRBYAC    ,NINT2     ,IINT2     ,IDDL      ,IKC       ,
     4    NDOF      ,INLOC     ,NSREM     ,NSL       ,NBINTC    ,
     5    INTLIST   ,X         ,IBFV      ,DIRUL     ,SKEW      ,
     6    XFRAME    ,ISKEW     ,ICODT     ,DE        ,D_IMP     ,
     7    LB        ,IFDIS     ,NDDL      ,DR_IMP    ,IDDLI     ,
     8    IRBE3     ,LRBE3     ,FRBE3     ,IRBE2     ,LRBE2     ,
     9    DD        ,DDR       ,A         ,AR        ,AC        ,
     A    ACR       ,MS        ,V         ,NDDL0     ,R02       ,
     B    RBY       ,ICODR     ,NT_RW     ,W_DDL     ,WEIGHT    ,
     C    IRFLAG    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARI(NPARI,*),NUM_IMP(*),NS_IMP(*),
     .          NE_IMP(*),NSREM  ,NSL,NBINTC,INTLIST(*),IRFLAG,
     .          IRBE3(*),LRBE3(*),IRBE2(*),LRBE2(*),NDDL0,W_DDL(*),
     .          WEIGHT(*),ICODR(*),NT_RW
        INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*),
     .          NINT2,IINT2(*),IDDL(*),IKC(*),NDOF(*),INLOC(*),
     .          IBFV(*),DIRUL(*),ISKEW(*),ICODT(*),IFDIS,NDDL,IDDLI(*)
C     REAL
        my_real
     .         X(3,*),SKEW(*) ,XFRAME(*),
     .         A(3,*),D_IMP(3,*),LB(*),DR_IMP(3,*),FRBE3(*),
     .         DD(3,*),DDR(3,*),AR(3,*),MS(*) ,V(3,*),DE,
     .         AC(3,*),ACR(3,*),R02,RBY(NRBY,*)

        TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,N,NKC,ND,NDM
        INTEGER, DIMENSION(NDDL0) :: IDM
        my_real, DIMENSION(NDDL0) :: LB0
C
        CALL CP_REAL_HP(NDDL,LB,LB0)
        CALL IMP_FRI(
     1  NUM_IMP   ,NS_IMP    ,NE_IMP    ,IPARI     ,INTBUF_TAB,
     2  NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,
     3  IRBYAC    ,NINT2     ,IINT2     ,IDDL      ,IKC       ,
     4  NDOF      ,INLOC     ,NSREM     ,NSL       ,NBINTC    ,
     5  INTLIST   ,X         ,IBFV      ,DIRUL     ,SKEW      ,
     6  XFRAME    ,ISKEW     ,ICODT     ,A         ,D_IMP     ,
     7  LB        ,IFDIS     ,NDDL      ,DR_IMP    ,IDDLI     ,
     8  IRBE3     ,LRBE3     ,FRBE3     ,IRBE2     ,LRBE2     )
        IF ( IFDIS>0 .AND. INTP_C <= 0)
     .   CALL IMP_FRFV(
     1  NUM_IMP   ,NS_IMP    ,NE_IMP    ,IPARI     ,INTBUF_TAB,
     2  IDDL      ,IKC       ,NDOF      ,NSREM     ,
     3  NSL       ,D_IMP     ,DD        ,DR_IMP    ,DDR       ,
     4  A         ,AR        ,MS        ,V         ,X         ,
     5  LB        ,NDDL      ,IBFV      ,SKEW      ,XFRAME    ,
     6  IRBE3     ,LRBE3     ,IRBE2     ,LRBE2     ,DE        ,
     7  NDDL0     ,W_DDL     )
C--------Fext change (U_d) w/ remot  to re-evalue R02
        IF (IRFLAG>0) THEN
          DO I =1,NDDL
            LB0(I) =LB(I)-LB0(I)
          END DO
          DO I =NDDL+1,NDDL0
            LB0(I) =ZERO
          END DO
          NKC=0
C------LB0 condensed -> LB0 original
          DO N =1,NUMNOD
            I=INLOC(N)
            NDM=IDDL(I)-NKC
            DO J=1,NDOF(I)
              ND = IDDL(I)+J
              IF (IKC(ND)/=0) THEN
                NKC = NKC + 1
                IDM(ND)=0
              ELSE
                NDM=NDM+1
                IDM(ND)=NDM
              END IF
            ENDDO
          ENDDO
          DO I =NDDL0,1,-1
            ND = IDM(I)
            IF (ND>0) LB0(I) =LB0(ND)
          END DO
          CALL IMP_SETBA(AC    ,ACR     ,IDDL   ,NDOF  ,LB0   ,1 )
          CALL UPD_RHS_FR(ICODT ,ICODR ,ISKEW ,IBFV    ,XFRAME ,
     1                    RBY   ,X     ,SKEW   ,LPBY   ,NPBY   ,
     2                    NRBYAC,IRBYAC,NINT2  ,IINT2  ,IPARI  ,
     3                    INTBUF_TAB   ,NDOF   ,IDDL   ,IKC    ,
     4                    NDDL0 ,LB0   ,ISETK  ,INLOC  ,DIRUL  ,
     5                    AC    ,ACR   ,NT_RW  ,W_DDL  ,NDDL   ,
     6                    R02   ,IRBE3 ,LRBE3  ,FRBE3  ,WEIGHT ,
     8                    IRBE2 ,LRBE2 )
        ENDIF
C----6---------------------------------------------------------------7---------8
        RETURN
      END
Chd|====================================================================
Chd|  INI_BMINMA_IMP                source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_INTBUF                    share/modules/imp_mod_def.F   
Chd|====================================================================
      SUBROUTINE INI_BMINMA_IMP
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_INTBUF
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER  N
C-----------------------------------------------
        ALLOCATE(BMINMA_IMP(6,NINTER))
        DO N = 1,NINTER
          BMINMA_IMP(1,N)=-EP30
          BMINMA_IMP(2,N)=-EP30
          BMINMA_IMP(3,N)=-EP30
          BMINMA_IMP(4,N)=EP30
          BMINMA_IMP(5,N)=EP30
          BMINMA_IMP(6,N)=EP30
        END DO
C------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  SPBRM_PRE                     source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        CGSHELL3                      source/implicit/cgshell.F     
Chd|        CGSHELL4                      source/implicit/cgshell.F     
Chd|        SPB_REFSH3ID                  source/implicit/imp_solv.F    
Chd|        SPB_REFSH4ID                  source/implicit/imp_solv.F    
Chd|        SPMD_MAX_I                    source/mpi/implicit/imp_spmd.F
Chd|        SPMD_MIN_S                    source/mpi/implicit/imp_spmd.F
Chd|        SPMD_SUM_S                    source/mpi/implicit/imp_spmd.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        IMP_SPBRM                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE SPBRM_PRE(ITAB ,
     1    X         ,IPARG     ,IXC       ,IXTG      ,PARTSAV   ,
     2    ELBUF_TAB ,PM        ,NDOF      ,IDDL      ,IKC       )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE ELBUFDEF_MOD
        USE IMP_SPBRM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER
     .     IXC(NIXC,*), IXTG(NIXTG,*), IPARG(NPARG,*),
     .      NDOF(*),IDDL(*),IKC(*),ITAB(*)
C     REAL
        my_real
     .     X(3,*)  ,PARTSAV(NPSAV,*) ,PM(*)
        TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP)      :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER  N4,N3,I,II,J,N, NG,MLW, NF1,NKC,ND,IP4,IP3,IG,
     +           JFT,JLT,NBC,ITY,NEL,NFT,NBCS,E_ID,N1,N2,K
        INTEGER, DIMENSION(:),ALLOCATABLE :: ITAG
C     REAL
        my_real
     .       XC,YC,ZC,MAS,MAS3,DMIN,XMIN,YMIN,ZMIN,MAST,
     .       DMING,XMING,YMING,ZMING
C---limitations : only the blank is deformable(shell only), all tools(part)are rigids---
C------------blank could be composed w/ 4N,3N and in diff parts
C------ IRIG_M>1 -> input E_REF id
C---IF there id BC (symmetry) in the blank----
C----- put N1,N2,N3 (sys_id) in E_REF(1:3),  E_REF(4)=E_REF(3)
        RLSKEW(1:9) = ZERO
        IF (IRIG_M>1 ) THEN
          NBC_B = 0
          ILSKEW = 0
          LSKEW_G = 0
          IKCE(1:6) = 0
          N1=0
          N2=0
          N3=0
          DO I = 1, NUMNOD
            IF(ITAB(I)==E_REF(1)) N1=I
            IF(ITAB(I)==E_REF(2)) N2=I
            IF(ITAB(I)==E_REF(3)) N3=I
          END DO
          E_REF(1)= N1
          E_REF(2)= N2
          E_REF(3)= N3
          DO J = 1, 3
            N = E_REF(J)
            IF (N==0) CYCLE
            DO K= 1,NDOF(N)
              ND = IDDL(N)+K
              IF (IKC(ND)>0.AND.IKCE(K)==0) IKCE(K) = 1
              IF (IKC(ND)==8) ILSKEW = N
            END DO
          END DO
          IF (ILSKEW >0)  LSKEW_G=1
C----  end routine
          RETURN
        END IF
        NBC = 0
        DO I =1,NUMNOD
          NKC=0
          DO J=1,MIN(3,NDOF(I))
            ND = IDDL(I)+J
            IF (IKC(ND)/=0) NKC = NKC + 1
          ENDDO
C--------exclude fixing all translational
          IF (NKC>0.AND.NKC<3) NBC = NBC +1
        ENDDO
        IF (NBC >0) THEN
C----------pense      deallocate
          AllOCATE(IBC_B(NBC))
          NBC = 0
          DO I =1,NUMNOD
            NKC=0
            DO J=1,MIN(3,NDOF(I))
              ND = IDDL(I)+J
              IF (IKC(ND)/=0) NKC = NKC + 1
            ENDDO
            IF (NKC>0.AND.NKC<3) THEN
              NBC = NBC +1
              IBC_B(NBC) = I
            END IF
          ENDDO
        END IF
C------global NBC---
        NBC_B = NBC
        IF (NSPMD>1) CALL SPMD_MAX_I(NBC_B)
C---get reference elem (shell for the moment) for rigid motion compute
C---- E_id : nearest the gravity center if no BC,
C-----E_REF(4) saved 4 nodes which are Xmin, Ymin,Zmin,Dmin, will be updated by v,vr
        IF (NBC >0) THEN
          ALLOCATE(ITAG(NUMNOD))
          ITAG = 0
          DO I =1,NBC
            N = IBC_B(I)
            ITAG(N) =1
          END DO
          NE_BC4 = 0
          NE_BC3 = 0
          DO NG = 1, NGROUP
            IF(IPARG(8,NG)==1) CYCLE
            ITY   =IPARG(5,NG)
            IF (ITY /= 3 .AND. ITY /= 7) CYCLE
            MLW   =IPARG(1,NG)
C
            IF (MLW == 0 .OR. MLW == 13) CYCLE
            NEL   =IPARG(2,NG)
            JFT   =IPARG(3,NG) + 1
            JLT   = IPARG(3,NG) + NEL
            IF (ITY==7) THEN
              DO I = JFT,JLT
                NBCS = 0
                DO J=1,3
                  N= IXTG(J+1,I)
                  NBCS = NBCS + ITAG(N)
                END DO
                IF (NBCS >0) NE_BC3 =NE_BC3 + 1
              END DO
            ELSEIF (ITY==3) THEN
              DO I = JFT,JLT
                NBCS = 0
                DO J=1,4
                  N= IXC(J+1,I)
                  NBCS = NBCS + ITAG(N)
                END DO
                IF (NBCS >0) NE_BC4 =NE_BC4 + 1
              END DO
            END IF
          END DO !NG = 1, NGROUP
          IF (NE_BC3 >0) ALLOCATE(IE_BC3(NE_BC3))
          IF (NE_BC4 >0) ALLOCATE(IE_BC4(NE_BC4))
          NE_BC4 = 0
          NE_BC3 = 0
          DO NG = 1, NGROUP
            IF(IPARG(8,NG)==1) CYCLE
            ITY   =IPARG(5,NG)
            IF (ITY /= 3 .AND. ITY /= 7) CYCLE
            MLW   =IPARG(1,NG)
C
            IF (MLW == 0 .OR. MLW == 13) CYCLE
            NEL   =IPARG(2,NG)
            JFT   =IPARG(3,NG) + 1
            JLT   = IPARG(3,NG) + NEL
            IF (ITY==7) THEN
              DO I = JFT,JLT
                NBCS = 0
                DO J=1,3
                  N= IXTG(J+1,I)
                  NBCS = NBCS + ITAG(N)
                END DO
                IF (NBCS >0) THEN
                  NE_BC3 =NE_BC3 + 1
                  IE_BC3(NE_BC3) = I
                END IF
              END DO
            ELSEIF (ITY==3) THEN
              DO I = JFT,JLT
                NBCS = 0
                DO J=1,4
                  N= IXC(J+1,I)
                  NBCS = NBCS + ITAG(N)
                END DO
                IF (NBCS >0) THEN
                  NE_BC4 =NE_BC4 + 1
                  IE_BC4(NE_BC4) = I
                END IF
              END DO
            END IF
          END DO !NG =
C-------NBC = 0
        ELSEIF (NBC_B == 0) THEN
          N3=0
          N4=0
          MAS =ZERO
          XC = ZERO
          YC = ZERO
          ZC = ZERO
          DO NG = 1, NGROUP
            IF(IPARG(8,NG)==1) CYCLE
            ITY   =IPARG(5,NG)
            IF (ITY /= 3 .AND. ITY /= 7) CYCLE
            MLW   =IPARG(1,NG)
C MLW= 0 ----> void; MLW = 13 ----> rigid material
            IF (MLW == 0 .OR. MLW == 13) CYCLE
            NEL   =IPARG(2,NG)
            JFT=1
            JLT=MIN(NVSIZ,NEL)
            NF1 = IPARG(3,NG)+1
            IF (ITY==7) THEN
              N3 =N3 +1
              CALL CGSHELL3(ELBUF_TAB(NG),JFT,JLT ,PM   ,IXTG(1,NF1),
     +                     X   ,MAS,XC ,YC ,ZC )
            ELSEIF (ITY==3) THEN
              N4 =N4 +1
              CALL CGSHELL4(ELBUF_TAB(NG),JFT,JLT ,PM   ,IXC(1,NF1),
     +                     X   ,MAS,XC ,YC ,ZC )
            END IF
          END DO !IG = 1, NGROUC
C
          IF (N3==0.AND.N4==0) THEN
C-------warning out--
          ELSE
            IF (NSPMD>1) THEN
              MAST = MAS
              CALL SPMD_SUM_S(MAST)
              CALL SPMD_SUM_S(XC)
              CALL SPMD_SUM_S(YC)
              CALL SPMD_SUM_S(ZC)
              MAS = MAST
            END IF
            XC = XC/MAS
            YC = YC/MAS
            ZC = ZC/MAS
C
            XMIN=EP30
            YMIN=EP30
            ZMIN=EP30
            DMIN=EP30
            DO NG = 1, NGROUP
              IF(IPARG(8,NG)==1) CYCLE
              ITY   =IPARG(5,NG)
              IF (ITY /= 3 .AND. ITY /= 7) CYCLE
              MLW   =IPARG(1,NG)
C
              IF (MLW == 0 .OR. MLW == 13) CYCLE
              NEL   =IPARG(2,NG)
              JFT   =IPARG(3,NG) + 1
              JLT   = IPARG(3,NG) + NEL
              IF (ITY==7) THEN
                CALL SPB_REFSH3ID(JFT,JLT,NEL,IXTG,X,XC,YC,ZC,
     +                            E_REF,XMIN,YMIN,ZMIN,DMIN)
              ELSEIF (ITY==3) THEN
                CALL SPB_REFSH4ID(JFT,JLT,NEL,IXC,X,XC,YC,ZC,
     +                            E_REF,XMIN,YMIN,ZMIN,DMIN)
              END IF
            END DO !IG = 1, NGROUC
C-------to get unique ref-element
            IF (NSPMD>1) THEN
              XMING = XMIN
              CALL SPMD_MIN_S(XMING)
              IF (XMING<XMIN) E_REF(1)=0
              YMING = YMIN
              CALL SPMD_MIN_S(YMING)
              IF (YMING<YMIN) E_REF(2)=0
              ZMING = ZMIN
              CALL SPMD_MIN_S(ZMING)
              IF (ZMING<ZMIN) E_REF(3)=0
              DMING = DMIN
              CALL SPMD_MIN_S(DMING)
              IF (DMING<DMIN) E_REF(4)=0
            END IF
          END IF   !((N3==0.AND.N4==0).OR.N3>1 .OR. N4>1)
        END IF !(NBC>0) THEN
        IF (NBC >0) DEALLOCATE(ITAG)
C
        RETURN
      END
Chd|====================================================================
Chd|  SPB_REFSH4ID                  source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        SPBRM_PRE                     source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPB_REFSH4ID(JFT,JLT,NEL,IXC,X,XC,YC,ZC,IE,
     +                        XMIN0,YMIN0,ZMIN0,DMIN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IXC(NIXC,*),JFT, JLT,IE(4),NEL
C     REAL
        my_real
     .         X(3,*),XC,YC,ZC,DMIN,XMIN0,YMIN0,ZMIN0
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER  N,I,II,ND,INDEX(NEL),IEL,J
        my_real
     .         XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,D,XM,YM,ZM,DMINL,
     .         XMINL,YMINL,ZMINL
C---get reference ele (shell for the moment) for rigid motion compute
C---- E_id : nearest the gravity center----
        ND =0
        DO I = JFT,JLT
          XMIN=EP30
          XMAX=-EP30
          YMIN=EP30
          YMAX=-EP30
          ZMIN=EP30
          ZMAX=-EP30
          DO J=1,4
            N= IXC(J+1,I)
            XMIN=MIN(XMIN,X(1,N))
            XMAX=MAX(XMAX,X(1,N))
            YMIN=MIN(YMIN,X(2,N))
            YMAX=MAX(YMAX,X(2,N))
            ZMIN=MIN(ZMIN,X(3,N))
            ZMAX=MAX(ZMAX,X(3,N))
          END DO
          IF((XC < XMIN.OR.XC > XMAX).AND.(YC < YMIN.OR.YC > YMAX)
     +       .AND.(ZC < ZMIN.OR.ZC > ZMAX)) CYCLE
          ND = ND +1
          INDEX(ND) = I
        END DO
        IF (ND ==0) THEN
          IEL =0
        ELSE
          DO II = 1,ND
            I = INDEX(II)
            XM = ZERO
            YM = ZERO
            ZM = ZERO
            DO J=1,4
              N= IXC(J+1,I)
              XM=XM+X(1,N)
              YM=YM+X(2,N)
              ZM=ZM+X(3,N)
            END DO
            XM = FOURTH*XM- XC
            YM = FOURTH*YM- YC
            ZM = FOURTH*ZM- ZC
            XMINL=ABS(XM)
            YMINL=ABS(YM)
            ZMINL=ABS(ZM)
C-------- Z direction is removed
            IF (XMINL<XMIN0) THEN
              IE(1) = I
              XMIN0 = XMINL
            END IF
            IF (YMINL<YMIN0) THEN
              IE(2) = I
              YMIN0 = YMINL
            END IF
c          IF (ZMINL<ZMIN0) THEN
c           IE(3) = I
c           ZMIN0 = ZMINL
c          END IF
            D=XM*XM+YM*YM
c         D=XM*XM+YM*YM+ZM*ZM
            IF (D < DMIN) THEN
              DMIN = D
C---------first node-----
              IE(4) = I
            END IF
          END DO
        ENDIF
c       IF (IEL>0 .AND.DMINL<DMIN) THEN
c        IE(1:4) = IXC(2:5,IEL)
c        DMIN= DMINL
c       END IF
C
        RETURN
      END
Chd|====================================================================
Chd|  SPB_REFSH3ID                  source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        SPBRM_PRE                     source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPB_REFSH3ID(JFT,JLT,NEL,IXTG,X,XC,YC,ZC,IE,
     +                        XMIN0,YMIN0,ZMIN0,DMIN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IXTG(NIXTG,*),JFT, JLT,IE(4),NEL
C     REAL
        my_real
     .         X(3,*),XC,YC,ZC,XMIN0,YMIN0,ZMIN0,DMIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER  N,I,II,ND,INDEX(NEL),IEL,J
        my_real
     .         XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,D,XM,YM,ZM,DMINL,
     .         XMINL,YMINL,ZMINL
C---get reference ele (shell for the moment) for rigid motion compute
C---- E_id : nearest the gravity center----
        ND =0
        DO I = JFT,JLT
          XMIN=EP30
          XMAX=-EP30
          YMIN=EP30
          YMAX=-EP30
          ZMIN=EP30
          ZMAX=-EP30
          DO J=1,3
            N= IXTG(J+1,I)
            XMIN=MIN(XMIN,X(1,N))
            XMAX=MAX(XMAX,X(1,N))
            YMIN=MIN(YMIN,X(2,N))
            YMAX=MAX(YMAX,X(2,N))
            ZMIN=MIN(ZMIN,X(3,N))
            ZMAX=MAX(ZMAX,X(3,N))
          END DO
          IF((XC < XMIN.OR.XC > XMAX).AND.(YC < YMIN.OR.YC > YMAX)
     +       .AND.(ZC < ZMIN.OR.ZC > ZMAX)) CYCLE
          ND = ND +1
          INDEX(ND) = I
        END DO
        IF (ND ==0) THEN
          IEL =0
        ELSE
          DO II = 1,ND
            I = INDEX(II)
            XM = ZERO
            YM = ZERO
            ZM = ZERO
            DO J=1,3
              N= IXTG(J+1,I)
              XM=XM+X(1,N)
              YM=YM+X(2,N)
              ZM=ZM+X(3,N)
            END DO
            XM = THIRD*XM - XC
            YM = THIRD*YM - YC
            ZM = THIRD*ZM - ZC
            XMINL=ABS(XM)
            YMINL=ABS(YM)
            ZMINL=ABS(ZM)
            IF (XMINL<XMIN0) THEN
              IE(1) = I
              XMIN0 = XMINL
            END IF
            IF (YMINL<YMIN0) THEN
              IE(2) = I
              YMIN0 = YMINL
            END IF
            IF (ZMINL<ZMIN0) THEN
              IE(3) = I
              ZMIN0 = ZMINL
            END IF
            D=XM*XM+YM*YM
c         D=XM*XM+YM*YM+ZM*ZM
            IF (D < DMIN) THEN
              DMIN = D
C------------- tag tria w/ negative
              IE(4) = -I
            END IF
          END DO
        ENDIF
c       IF (IEL>0 .AND.DMINL<DMIN) THEN
c        IE(1:3) = IXTG(2:4,IEL)
c        IE(4) = IE(3)
c        DMIN= DMINL
c       END IF
C
        RETURN
      END
Chd|====================================================================
Chd|  SPB_RM_RIG                    source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        GETIKCE                       source/implicit/imp_solv.F    
Chd|        SPB_IEREF3                    source/implicit/imp_solv.F    
Chd|        SPB_IEREF_BC                  source/implicit/imp_solv.F    
Chd|        SPB_REF_NDS                   source/implicit/imp_solv.F    
Chd|        SPB_RGMOD                     source/implicit/imp_solv.F    
Chd|        SPMD_E_REF                    source/mpi/implicit/imp_spmd.F
Chd|        SPMD_N_REF                    source/mpi/implicit/imp_spmd.F
Chd|        TRANSVG2L                     source/implicit/imp_solv.F    
Chd|        TRANSVL2G                     source/implicit/imp_solv.F    
Chd|        VELROT                        source/constraints/general/rbe2/rbe2v.F
Chd|        IMP_SPBRM                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE SPB_RM_RIG(
     1    X         ,IXC       ,IXTG      ,NDOF      ,IDDL      ,
     2    IKC       ,D_IMP     ,DR_IMP    ,ICODT     ,ICODR     ,
     3    SKEW      ,ISKEW     ,itab      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_SPBRM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ICODT(*),ICODR(*),ISKEW(*)
        INTEGER
     .     IXC(NIXC,*), IXTG(NIXTG,*), NDOF(*),IDDL(*),IKC(*),itab(*)
C     REAL
        my_real
     .     X(3,*) ,D_IMP(3,*) ,DR_IMP(3,*)  ,SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER  N,I,J,ND,NN,IE,IEM,NKC,K,ILOC,ISK,IKCL(6)
        my_real
     .         DMIN,D,XC,YC,ZC,DTRA(3),DROT(3),DRS(3),LSM(3),
     .         X0,Y0,Z0,DTRAL(3),DROTL(3),LSML(3),DRS1(3),DROT1(3),DROTL1(3)
C------remove rigid motion of springback (increment)
C----- find reference element: IF BCS the one of smallest Disp w/ BCS (doing ech time)
C------------------------------else nearest the GC of blank (done only once)
C------should update X_REF,D_REF each cycle---
C------case input 3N---
        IF (IRIG_M>1) THEN
          CALL SPB_REF_NDS(
     1     X         ,D_IMP     ,NDOF      ,IDDL      ,IKC       ,
     2     ICODT     ,ICODR     ,ISKEW     ,SKEW      )
          IF (ILSKEW>0) THEN
C----get ISK(from E_REF)set [Q],transfert DTRA,DROT to local reset IKCE(6) put (j)=0 for fixing dir
            NN = ILSKEW
            ISK = ISKEW(NN)
C---------local IKC
            RLSKEW(1:9) = SKEW(1:9,ISK)
            CALL GETIKCE(ICODT(NN),ICODR(NN),NDOF(NN),IKCE)
          END IF
          IF (NSPMD>1) CALL SPMD_N_REF()
        ELSE
          IF (NBC_B>0) THEN
            CALL SPB_IEREF_BC(
     1      X         ,IXC       ,IXTG      ,D_IMP    ,DMIN   ,
     2      NDOF      ,IDDL      ,IKC       )
            IF (ILSKEW>0) THEN
C----get ISK(from E_REF)set [Q],transfert DTRA,DROT to local reset IKCE(6) put (j)=0 for fixing dir
              NN = ILSKEW
              ISK = ISKEW(NN)
C---------local IKC
              RLSKEW(1:9) = SKEW(1:9,ISK)
              CALL GETIKCE(ICODT(NN),ICODR(NN),NDOF(NN),IKCE)
            END IF
          ELSE
            CALL SPB_IEREF3(
     1      X         ,IXC       ,IXTG      ,NDOF      ,IDDL      ,
     2      IKC       ,D_IMP     ,DR_IMP    ,DMIN )
          END IF
C--------communication IKCE,X_REF,D_REF,RLSKEW,N_SEG, LSKEW_G
          IF (NSPMD>1) CALL SPMD_E_REF(DMIN)
        END IF !(IRIG_M>1) THEN
C
        IF (N_SEG==0) RETURN
C-----IKCE : only local BCS nodes are initialized-----
        CALL SPB_RGMOD(N_SEG ,X_REF ,D_REF ,X   ,D_IMP  ,
     +                 X0  ,Y0  ,Z0  ,DTRA,DROT )
        IF (NBC_B>0.OR.LSKEW_G>0) THEN
          DO J=1,3
            IF (IKCE(J)==1) DTRA(J)= ZERO
          ENDDO
c        IF (IRODDL/=0) THEN
          DO J=1,3
            IF (IKCE(J+3)==1) DROT(J)=ZERO
          ENDDO
c        END IF
        END IF
        IF (LSKEW_G>0) THEN
          CALL TRANSVG2L(RLSKEW,DTRA,DTRAL)
          CALL TRANSVG2L(RLSKEW,DROT,DROTL)
          DO J=1,3
            IF (IKCE(J)==1) DTRAL(J)= ZERO
            IF (IKCE(J+3)==1) DROTL(J)=ZERO
          ENDDO
          IF (IRODDL/=0) CALL TRANSVL2G(RLSKEW,DROTL,DROT)
          DO I =1,NUMNOD
            NKC=0
            DO J=1,MIN(3,NDOF(I))
              ND = IDDL(I)+J
              IF (IKC(ND)/=0) NKC = NKC + 1
            ENDDO
            DROT1(1:3) = DROT(1:3)
C--------exclude the node w/all translational fixed
            IF (NKC<3.AND.NDOF(I)>0) THEN
              LSM(1)=X(1,I)-X0
              LSM(2)=X(2,I)-Y0
              LSM(3)=X(3,I)-Z0
C----transfert LSM to local
c         CALL TRANSVG2L(RLSKEW,LSM,LSML)
              CALL VELROT(DROTL,LSML,DRS)
C------otherwise DL(1:3)= DRS+ DTRAL if fixing put DL(j)=0
              DRS(1:3) = DRS(1:3) + DTRAL(1:3)
              IF (NKC>0) THEN
C----------suppose the same ISK w/ reference one
                CALL GETIKCE(ICODT(I),ICODR(I),NDOF(I),IKCL)
                DO J=1,3
                  DROTL1(J) = DROTL(J)
                  IF (IKCL(J)==1) DRS(J)= ZERO
                  IF (IKCL(J+3)==1) DROTL1(J)= ZERO
                ENDDO
                CALL TRANSVL2G(RLSKEW,DROTL1,DROT1)
              END IF !(NKC>0) THEN
              CALL TRANSVL2G(RLSKEW,DRS,DRS1)
C----transfert DL DROT,to global
              DO K=1,3
                D_IMP(K,I)=D_IMP(K,I)- DRS1(K)
              ENDDO
              IF (IRODDL/=0) THEN
                DO K=1,3
                  DR_IMP(K,I)=DR_IMP(K,I)- DROT1(K)
                ENDDO
              END IF
            END IF
          ENDDO
        ELSE
          DO I =1,NUMNOD
            NKC=0
            DO J=1,MIN(3,NDOF(I))
              ND = IDDL(I)+J
              IF (IKC(ND)/=0) NKC = NKC + 1
            ENDDO
C--------exclude the node w/all translational fixed
            IF (NKC<3.AND.NDOF(I)>0) THEN
              LSM(1)=X(1,I)-X0
              LSM(2)=X(2,I)-Y0
              LSM(3)=X(3,I)-Z0
              CALL VELROT(DROT,LSM,DRS)
              DO K=1,3
                ND = IDDL(I)+K
                IF (IKC(ND)==0)D_IMP(K,I)=D_IMP(K,I)- DRS(K)- DTRA(K)
              ENDDO
              IF (IRODDL/=0) THEN
                DO K=1,3
                  ND = IDDL(I)+K+3
                  IF (IKC(ND)==0) DR_IMP(K,I)=DR_IMP(K,I)- DROT(K)
                ENDDO
              END IF
            END IF
          ENDDO
        END IF
        IF (ITTOFF>0 .AND.NCYCLE==1) THEN
          write(iout,*)
          write(iout,*)'Segment served as Reference are the following nodes:'
          if (IRIG_M>1) THEN
            if (E_REF(1)>0) write(iout,*) 'NODE_ref 1 :',itab(E_REF(1))
            if (E_REF(2)>0) write(iout,*) 'NODE_ref 2 :',itab(E_REF(2))
            if (E_REF(3)>0) write(iout,*) 'NODE_ref 3 :',itab(E_REF(3))
          else
            if (E_REF(1)>0.AND.E_REF(2)>0.AND.E_REF(3)>0) then
              write(iout,*) (itab(E_REF(i)),i=1,N_SEG)
            else
              write(iout,*) (E_REF(i),i=1,N_SEG)
            end if
          end if
          write(iout,*)'Reference point at this moment:'
          write(iout,*)X0  ,Y0  ,Z0
          write(iout,*)'DTRA,DROT,N_SEG,ncycle=',N_SEG,ncycle
          write(iout,*)DTRA(1),DTRA(2),DTRA(3)
          write(iout,*)DROT(1),DROT(2),DROT(3)
          write(iout,*)'ILSKEW,LSKEW_G,ISPMD=',ILSKEW,LSKEW_G,ISPMD
          write(iout,*)RLSKEW(1),RLSKEW(2),RLSKEW(3)
          write(iout,*)
        END IF
c       IF (ITTOFF>0.AND.NCYCLE>1) THEN
c        write(iout,*)
c        write(iout,*)'DTRA,DROT,N_SEG,ILOC,ncycle=',N_SEG,ILOC,ncycle
c        write(iout,*)DTRA(1),DTRA(2),DTRA(3)
c        write(iout,*)DROT(1),DROT(2),DROT(3)
c        write(iout,*)'LSKEW_G,ISPMD=',LSKEW_G,ISPMD
c        write(iout,*)RLSKEW(1),RLSKEW(2),RLSKEW(3)
c        write(iout,*)
c       END IF
c      write(iout,*)'E_REF(i)=',(itab(E_REF(i)),i=1,4)
c      IF (ISPMD==0) THEN
c      write(iout,*)'X0,DTRA,DROT,N_SEG,ncycle=',N_SEG,ncycle
c      write(iout,*)X0  ,Y0  ,Z0
c      write(iout,*)DTRA(1),DTRA(2),DTRA(3)
c      write(iout,*)DROT(1),DROT(2),DROT(3)
c      END IF
C------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  SPB_IEREF_BC                  source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        SPB_RM_RIG                    source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        SPMD_MIN_S                    source/mpi/implicit/imp_spmd.F
Chd|        IMP_SPBRM                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE SPB_IEREF_BC(
     1    X         ,IXC       ,IXTG      ,D_IMP     ,DMIN    ,
     2    NDOF      ,IDDL      ,IKC       )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_SPBRM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER
     .     NDOF(*),IDDL(*),IKC(*),IXC(NIXC,*), IXTG(NIXTG,*)
C     REAL
        my_real
     .     DMIN,X(3,*) ,D_IMP(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER  N,I,J,ND,NN,IE,IEM,K
        my_real
     .         D(3),DD,DMINT
C------remove rigid motion of springback (incremental)
C----- find reference element: IF BCS the smallest Disp(at element center)) w/BCS C-------
C------doing  once: NCYCLE=1
        DMIN=EP30
        IF (NCYCLE==1 )THEN
          ILSKEW = 0
          LSKEW_G = 0
          IKCE(1:6) = 0
          E_REF(1:4) = 0
          DO I = 1, NE_BC4
            IE =IE_BC4(I)
            DO K = 1,3
              D(K) = ZERO
              DO J= 1,4
                N = IXC(J+1,IE)
                D(K) = D(K) + D_IMP(K,N)
              END DO
            END DO
            DD = MIN(ABS(D(1)),ABS(D(2)),ABS(D(3)))
            IF (DD < DMIN) THEN
              DMIN = DD
              E_REF(1:4) = IXC(2:5,IE)
            END IF
          END DO
C
          DO I = 1, NE_BC3
            IE =IE_BC3(I)
            DO K = 1,3
              D(K) = ZERO
              DO J= 1,3
                N = IXTG(J+1,IE)
                D(K) = D(K) + D_IMP(K,N)
              END DO
            END DO
            DD = MIN(ABS(D(1)),ABS(D(2)),ABS(D(3)))
            IF (DD < DMIN) THEN
              DMIN = DD
              E_REF(1:3) = IXTG(2:4,IE)
              E_REF(4) = E_REF(3)
            END IF
          END DO
C-----not to use allow BCS (global only IKC=1) dof direction
C----- add BCS w/ SKEW (ILSKEW=1 w/ ref); Ud (IKC=2,9) could also be added--
          DO J = 1, 4
            N = E_REF(J)
            IF (N==0) CYCLE
            DO K= 1,NDOF(N)
              ND = IDDL(N)+K
              IF (IKC(ND)>0.AND.IKCE(K)==0) IKCE(K) = 1
            END DO
          END DO
C--------to limit change during the iteration and dependant of np
          IF (NSPMD>1) THEN
            DMINT = DMIN
            CALL SPMD_MIN_S(DMINT)
C------not in this domain-----
            IF (DMINT<DMIN) THEN
              E_REF(1:4) = 0
            END IF
          END IF
          IF ((E_REF(1)+E_REF(2)+E_REF(3)+E_REF(4))==0) THEN
            N_SEG = 0
          ELSEIF(E_REF(4) == E_REF(3)) THEN
            N_SEG = 3
          ELSE
            N_SEG = 4
          END IF
          IF (N_SEG>0) THEN
            DO J = 1, 4
              N = E_REF(J)
              IF (N==0) CYCLE
              DO K= 1,NDOF(N)
                ND = IDDL(N)+K
                IF (IKC(ND)==8) ILSKEW = N
              END DO
            END DO
            IF (ILSKEW >0) LSKEW_G=1
          END IF
C-------Ncycle>1
        ELSEIF ((E_REF(1)+E_REF(2)+E_REF(3)+E_REF(4)) >0) THEN
          DO K = 1, 3
            D(K) = ZERO
            DO J= 1,3
              N = E_REF(J)
              D(K) = D(K) + D_IMP(K,N)
            END DO
            IF (E_REF(4) /= E_REF(3)) THEN
              N = E_REF(4)
              D(K) = D(K) + D_IMP(K,N)
            END IF
          END DO
          DMIN = MIN(ABS(D(1)),ABS(D(2)),ABS(D(3)))
        END IF !(NCYCLE==1 )THEN
        X_REF(1:3,1:4) = ZERO
        D_REF(1:3,1:4) = ZERO
        IF (N_SEG > 0) THEN
          DO J= 1,4
            N = E_REF(J)
            X_REF(1:3,J)= X(1:3,N)
            D_REF(1:3,J)= D_IMP(1:3,N)
          END DO
        END IF
C------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  SPB_RGMOD                     source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        SPB_RM_RIG                    source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPB_RGMOD(N_SEG,X_REF ,D_REF ,X  ,D  ,
     1                     X0  ,Y0  ,Z0  ,DTRA  ,DROT  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER N_SEG
C     REAL
        my_real
     .     X_REF(3,4),D_REF(3,4),X(3,*),D(3,*),DTRA(3),DROT(3),
     .     X0  ,Y0  ,Z0
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER  I, J, II, L, JJ,NJ,K,NIR
C     REAL
        my_real
     .     XX,YY,ZZ,XXX,YYY,ZZZ,XY,YZ,ZX,XY2,YZ2,ZX2,
     .     B1,B2,B3,C1,C2,C3,FACM,RJ(3,3,4),
     .     X22,Y22,Z22,DET,XM(4),YM(4),ZM(4)
        my_real
     .     XS,YS,ZS
C------------------------------------
C     MATRICE DE JACOBIEN [C]
C------------------------------------
        NIR=N_SEG
        DO J=1,NIR
C        NJ=IRECT(J)
          XM(J)=X_REF(1,J)
          YM(J)=X_REF(2,J)
          ZM(J)=X_REF(3,J)
        ENDDO
        IF(NIR==3) THEN
          XM(4)=ZERO
          YM(4)=ZERO
          ZM(4)=ZERO
        ENDIF
        FACM = ONE / NIR
C----------------------------------------------------
C       VITESSE DE ROTATION MOYENNE DU SEGMENT MAIN
C----------------------------------------------------
        X0=FACM*(XM(1)+XM(2)+XM(3)+XM(4))
        Y0=FACM*(YM(1)+YM(2)+YM(3)+YM(4))
        Z0=FACM*(ZM(1)+ZM(2)+ZM(3)+ZM(4))
        DO J=1,NIR
          XM(J)=XM(J)-X0
          YM(J)=YM(J)-Y0
          ZM(J)=ZM(J)-Z0
        ENDDO
C-------
        XX=0
        YY=0
        ZZ=0
        XY=0
        YZ=0
        ZX=0
        DO J=1,NIR
          XX=XX+ XM(J)*XM(J)
          YY=YY+ YM(J)*YM(J)
          ZZ=ZZ+ ZM(J)*ZM(J)
          XY=XY+ XM(J)*YM(J)
          YZ=YZ+ YM(J)*ZM(J)
          ZX=ZX+ ZM(J)*XM(J)
        ENDDO
        ZZZ=XX+YY
        XXX=YY+ZZ
        YYY=ZZ+XX
        XY2=XY*XY
        YZ2=YZ*YZ
        ZX2=ZX*ZX
        DET= XXX*YYY*ZZZ -XXX*YZ2 -YYY*ZX2 -ZZZ*XY2 -TWO*XY*YZ*ZX
        DET=ONE/DET
        B1=(ZZZ*YYY-YZ2)*DET
        B2=(XXX*ZZZ-ZX2)*DET
        B3=(YYY*XXX-XY2)*DET
        C3=(ZZZ*XY+YZ*ZX)*DET
        C1=(XXX*YZ+ZX*XY)*DET
        C2=(YYY*ZX+XY*YZ)*DET
        DO J=1,NIR
          X22 = C1*XM(J)
          Y22 = C2*YM(J)
          Z22 = C3*ZM(J)
C-------RJ=(R^tR)^-1Rj^t-(j=1,ndir)---
          RJ(1,1,J)=Z22-Y22
          RJ(2,1,J)=B2*ZM(J)-C1*YM(J)
          RJ(3,1,J)=C1*ZM(J)-B3*YM(J)
          RJ(1,2,J)=-B1*ZM(J)+C2*XM(J)
          RJ(2,2,J)=-Z22+X22
          RJ(3,2,J)=-C2*ZM(J)+B3*XM(J)
          RJ(1,3,J)=B1*YM(J)-C3*XM(J)
          RJ(2,3,J)=C3*YM(J)-B2*XM(J)
          RJ(3,3,J)=Y22-X22
        ENDDO
C
        DO I=1,3
          DTRA(I)= ZERO
          DROT(I) = ZERO
          DO J=1,NIR
C         NJ=IRECT(J)
            DROT(I)=DROT(I)+RJ(I,1,J)*D_REF(1,J)+
     .              RJ(I,2,J)*D_REF(2,J)+RJ(I,3,J)*D_REF(3,J)
            DTRA(I)=DTRA(I)+FACM*D_REF(I,J)
          END DO
        END DO
C
        RETURN
      END
Chd|====================================================================
Chd|  SPB_IEREF3                    source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        SPB_RM_RIG                    source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        SPMD_MIN_S                    source/mpi/implicit/imp_spmd.F
Chd|        IMP_SPBRM                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE SPB_IEREF3(
     1    X         ,IXC       ,IXTG      ,NDOF      ,IDDL      ,
     2    IKC       ,D_IMP     ,DR_IMP    ,DMIN )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_SPBRM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER
     .     IXC(NIXC,*), IXTG(NIXTG,*), NDOF(*),IDDL(*),IKC(*)
C     REAL
        my_real
     .     DMIN,X(3,*) ,D_IMP(3,*) ,DR_IMP(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER  N,I,J,ND,NN,IE,IEM,K,NSAVE(4)
        my_real
     .         D(3),DD,DR(3),DDR,DMINT
C----- find reference element: around element center))
C--------NSAVE(1) : XMIN, 2 : YMIN, 3 : ZMIN, 4 : DMIN
        DMIN=EP30
        IF (NCYCLE==1 )THEN
          NSAVE(1:4) = E_REF(1:4)
          E_REF(1:4) = 0
          DO I = 1,4
            IE =IABS(NSAVE(I))
            IF (IE==0) CYCLE
            DO K = 1,3
              D(K) = ZERO
              DR(K) = ZERO
              IF (NSAVE(4)<0) THEN
                DO J= 1,3
                  N = IXTG(J+1,IE)
                  D(K) = D(K) + D_IMP(K,N)
                  DR(K) = DR(K) + DR_IMP(K,N)
                END DO
              ELSE
                DO J= 1,4
                  N = IXC(J+1,IE)
                  D(K) = D(K) + D_IMP(K,N)
                  DR(K) = DR(K) + DR_IMP(K,N)
                END DO
              END IF
            END DO
            DD = MIN(ABS(D(1)),ABS(D(2)),ABS(D(3)))
            DDR = MAX(ABS(DR(1)),ABS(DR(2)),ABS(DR(3)))
C   -------min(max(DR(j))) (1:4)------
            IF (DDR < DMIN) THEN
              DMIN = DDR
              IF (NSAVE(4)<0) THEN
                E_REF(1:3) = IXTG(2:4,IE)
                E_REF(4) = E_REF(3)
              ELSE
                E_REF(1:4) = IXC(2:5,IE)
              END IF
            END IF
          END DO
C
          IF (NSPMD>1) THEN
            DMINT = DMIN
            CALL SPMD_MIN_S(DMINT)
C------not in this domain-----
            IF (DMINT<DMIN) THEN
              E_REF(1:4) = 0
            END IF
          END IF
          IF ((E_REF(1)+E_REF(2)+E_REF(3)+E_REF(4))==0) THEN
            N_SEG = 0
          ELSEIF(E_REF(4) == E_REF(3)) THEN
            N_SEG = 3
          ELSE
            N_SEG = 4
          END IF
C---- Ncycle>1
        ELSE
          IF ((E_REF(1)+E_REF(2)+E_REF(3)+E_REF(4))==0) N_SEG = 0
          IF (N_SEG>0) THEN
            DO K = 1, 3
              DR(K) = ZERO
              DO J= 1,3
                N = E_REF(J)
                DR(K) = DR(K) + DR_IMP(K,N)
              END DO
              IF (E_REF(4) /= E_REF(3)) THEN
                N = E_REF(4)
                DR(K) = DR(K) + DR_IMP(K,N)
              END IF
            END DO
            DMIN = MAX(ABS(DR(1)),ABS(DR(2)),ABS(DR(3)))
          END IF !(N_SEG>0) THEN
        END IF !(NCYCLE==1 )THEN
        X_REF(1:3,1:4) = ZERO
        D_REF(1:3,1:4) = ZERO
        IF (N_SEG > 0) THEN
          DO J= 1,4
            N = E_REF(J)
            X_REF(1:3,J)= X(1:3,N)
            D_REF(1:3,J)= D_IMP(1:3,N)
          END DO
        END IF
C------------------------------------------
        RETURN
      END
Chd|====================================================================
Chd|  IMP_INTBUF_INI                source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        DIM_INT7                      source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|        IMP_INTBUFDEF                 share/modules/imp_mod_def.F   
Chd|====================================================================
      SUBROUTINE IMP_INTBUF_INI(IMP_INTBUF_TAB,NIMP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_INTBUFDEF
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE(IMP_INTBUF_STRUCT_) IMP_INTBUF_TAB(*)
        INTEGER NIMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,N,NI,SIZ,I_CONT
C=======================================================================

        DO NI= 1, NINTER
          I_CONT = NIMP(NI)
          IMP_INTBUF_TAB(NI)%S_I_STOK        = 1
          IMP_INTBUF_TAB(NI)%S_CAND_N       = 0
          IMP_INTBUF_TAB(NI)%S_CAND_E       = 0
          IMP_INTBUF_TAB(NI)%S_INDSUBT      = 0
          IMP_INTBUF_TAB(NI)%S_HJ         = 0
          IMP_INTBUF_TAB(NI)%S_NJ         = 0
          IMP_INTBUF_TAB(NI)%S_STIF         = 0
          ALLOCATE(IMP_INTBUF_TAB(NI)%I_STOK(IMP_INTBUF_TAB(NI)%S_I_STOK))
          IMP_INTBUF_TAB(NI)%I_STOK(1:IMP_INTBUF_TAB(NI)%S_I_STOK) = 0
          IF (I_CONT > 0) THEN
            IMP_INTBUF_TAB(NI)%S_CAND_N       = I_CONT
            IMP_INTBUF_TAB(NI)%S_CAND_E       = I_CONT
            IMP_INTBUF_TAB(NI)%S_INDSUBT       = I_CONT
            IMP_INTBUF_TAB(NI)%S_HJ   = 4*I_CONT
            IMP_INTBUF_TAB(NI)%S_NJ   = 3*I_CONT
            IMP_INTBUF_TAB(NI)%S_STIF = I_CONT
C------Allocate, ini to zero
            ALLOCATE(IMP_INTBUF_TAB(NI)%CAND_N(IMP_INTBUF_TAB(NI)%S_CAND_N))
            IMP_INTBUF_TAB(NI)%CAND_N(1:IMP_INTBUF_TAB(NI)%S_CAND_N) = 0
            ALLOCATE(IMP_INTBUF_TAB(NI)%CAND_E(IMP_INTBUF_TAB(NI)%S_CAND_E))
            IMP_INTBUF_TAB(NI)%CAND_E(1:IMP_INTBUF_TAB(NI)%S_CAND_E) = 0
            ALLOCATE(IMP_INTBUF_TAB(NI)%INDSUBT(IMP_INTBUF_TAB(NI)%S_INDSUBT))
            IMP_INTBUF_TAB(NI)%INDSUBT(1:IMP_INTBUF_TAB(NI)%S_INDSUBT) = 0
C
            ALLOCATE(IMP_INTBUF_TAB(NI)%HJ(IMP_INTBUF_TAB(NI)%S_HJ))
            IMP_INTBUF_TAB(NI)%HJ(1:IMP_INTBUF_TAB(NI)%S_HJ) = ZERO
            ALLOCATE(IMP_INTBUF_TAB(NI)%NJ(IMP_INTBUF_TAB(NI)%S_NJ))
            IMP_INTBUF_TAB(NI)%NJ(1:IMP_INTBUF_TAB(NI)%S_NJ) = ZERO
            ALLOCATE(IMP_INTBUF_TAB(NI)%STIF(IMP_INTBUF_TAB(NI)%S_STIF))
            IMP_INTBUF_TAB(NI)%STIF(1:IMP_INTBUF_TAB(NI)%S_STIF) = ZERO
          END IF

        ENDDO !NI=1,NINTER

C-----
        RETURN

      END SUBROUTINE IMP_INTBUF_INI
Chd|====================================================================
Chd|  IMP_ERRMUMPS                  source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        SPMD_MUMPS_EXEC               source/mpi/implicit/imp_spmd.F
Chd|-- calls ---------------
Chd|        IMP_STOP                      source/implicit/imp_solv.F    
Chd|====================================================================
      SUBROUTINE IMP_ERRMUMPS(IERR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER  IERR,ISTOP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        WRITE(ISTDO,1000)IERR
        WRITE(IOUT,1000)IERR
        SELECT CASE (-IERR)
         CASE(7,8,9,11,13,14,15,17)
          WRITE(ISTDO,1030)
          WRITE(IOUT,1010)
         CASE(6,10)
          WRITE(ISTDO,2000)
          WRITE(IOUT,2010)
        END SELECT
        IF(IERR<0)THEN
          ISTOP=-4
          CALL IMP_STOP(ISTOP)
        END IF
C
        RETURN
 1000   FORMAT(/
     .         '    ** LINEAR SOLVER MUMPS ERROR CODE: ',I6/)
 1030   FORMAT(/
     .         '    ** ERROR MEMORY ISSUE            ' /)
 1010   FORMAT(/
     .         '    ** ERROR MEMORY ISSUE. POSSIBLE SOLUTIONS:' /,
     .         '    *RUN ON A COMPUTER WITH MORE MEMORY ;' /,
     .         '    *TRY LESS THREADS AND LESS PROCS PER COMPUTER NODE ;' /,
     .         '    *CLOSE OTHER APPLICATIONS ;           ' /)
 2000   FORMAT(/
     .         '    ** ERROR OF SINGULAR MATRIX ' /)
 2010   FORMAT(/
     .         '    ** ERROR OF SINGULAR MATRIX. POSSIBLE SOLUTIONS:' /,
     .         '    *CHECK IF THE MODEL IS WELL CONDITIONED ;' /,
     .         '    *TRYING QUASI-STATIC SOLUTION ; ' /)
      END



Chd|====================================================================
Chd|  PVP_K                         source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        JACOBIEN                      source/implicit/imp_glob_k.F  
Chd|        MINV_K                        source/implicit/imp_solv.F    
Chd|        ECND_MOD                      share/modules/ecdn_mod.F      
Chd|====================================================================
      SUBROUTINE PVP_K(ND  ,IADK,JDIK,IDDL ,INLOC,
     .                 NDOF,ITAB,K_DIAG,K_LT ,LAMDA, NODE,MS  )
C----6---------------------------------------------------------------7---------8
        USE ECND_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ND,NODE
        INTEGER JDIK(*)  ,IADK(*),IDDL(*),INLOC(*),NDOF(*),ITAB(*)
C     REAL
        my_real
     .     K_DIAG(*) ,K_LT(*) ,LAMDA,MS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,K,J,IK,ID,JD,L,N,NN,JJ,IDT
        my_real
     .     KE(ND,ND) ,EV(ND,ND),EW(ND),LA,TOL,MSD(ND),MSIJ,TOL1
C----6----------------------------------
        TOL=EM5
        TOL1=EM10
        KE(1:ND,1:ND)=ZERO
        NODE = 0
        LAMDA = ZERO
        IF (NS10E==0) THEN
C-----taking into account to M-1
          DO N =1,NUMNOD
            I=INLOC(N)
            DO J=1,NDOF(I)
              IK = IDDL(I)+J
              MSD(IK)=MS(I)
C----free node
              IF (MSD(IK)<EM20) MSD(IK)=ONE
            ENDDO
          ENDDO
          DO K=1,ND
            KE(K,K) = K_DIAG(K)/MSD(K)
            DO J = IADK(K),IADK(K+1)-1
              JD = JDIK(J)
              MSIJ=ONE/SQRT(MSD(K))/SQRT(MSD(JD))
              KE(K,JD) = K_LT(J)*MSIJ
              KE(JD,K) = KE(K,JD)
            ENDDO
          ENDDO
        ELSE
C-------itet=2 (not dumped [M]), should not mix w/ other elements
          DO K=1,ND
            KE(K,K) = K_DIAG(K)
            DO J = IADK(K),IADK(K+1)-1
              JD = JDIK(J)
              KE(K,JD) = K_LT(J)
              KE(JD,K) = KE(K,JD)
            ENDDO
          ENDDO
          CALL MINV_K(ND  ,ICNDS10,IDDL ,INLOC,NDOF,
     .                MS  ,TOL ,KE   )
        END IF !(NS10E==0) THEN
        CALL JACOBIEN(KE,ND,EW,EV,TOL1,LAMDA)
C---Node:N direction:J
        ID = 0
        DO K=1,ND
          IF (EW(K)>=LAMDA) ID = K
          IF (ID > 0 ) CYCLE
        ENDDO
        DO N =1,NUMNOD
          I=INLOC(N)
          JJ = 0
          DO J=1,NDOF(I)
            IK = IDDL(I)+J
            IF (IK==ID) JJ= J
          ENDDO
          IF (JJ > 0) THEN
            NODE= N
            WRITE(IOUT,*)'1er EIGENVALUE(K/M) OF NODE+DIR:',LAMDA,ITAB(N),JJ
            CYCLE
          END IF
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  MINV_K                        source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        PVP_K                         source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        JACOBIEN                      source/implicit/imp_glob_k.F  
Chd|====================================================================
      SUBROUTINE MINV_K(ND  ,ICNDS10,IDDL ,INLOC,NDOF,
     .                  MS  ,TOL ,KE   )
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ND
        INTEGER ICNDS10(3,*),IDDL(*),INLOC(*),NDOF(*)
C     REAL
        my_real
     .     KE(ND,ND) ,MS(*),TOL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,K,J,IK,ID,JD,L,N,NN,N1,N2,II,JJ,JK
        my_real
     .     EV(ND,ND),EW(ND),
     .     LA,MSD(ND),MSIJ,LAMDA,ME(ND,ND),MSND,MSII
C----6----------------------------------
        LAMDA = ZERO
        ME(1:ND,1:ND) = ZERO
        DO I =1,NS10E
          NN  = IABS(ICNDS10(1,I))
          N1  = ICNDS10(2,I)
          N2  = ICNDS10(3,I)
          ID=INLOC(NN)
          MSND = MS(ID)
          DO J=1,NDOF(ID)
            IK = IDDL(ID)+J
            ME(IK,IK)=ME(IK,IK)+MSND
          END DO
          II=INLOC(N1)
          MSII = THIRD*MS(II) + FOURTH*MSND
          DO J=1,NDOF(II)
            IK = IDDL(II)+J
            ME(IK,IK)=ME(IK,IK)+MSII
          END DO
          JJ=INLOC(N2)
          MSII = THIRD*MS(JJ) + FOURTH*MSND
          DO J=1,NDOF(JJ)
            IK = IDDL(JJ)+J
            ME(IK,IK)=ME(IK,IK)+MSII
          END DO
C--------- m12
          MSII = FOURTH*MSND
C--------suppose NDOF(II)= NDOF(JJ) = NDOF(ID)
          DO J=1,NDOF(II)
            IK = IDDL(II)+J
            JK = IDDL(JJ)+J
            ME(IK,JK)=ME(IK,JK)+MSII
            ME(JK,IK)=ME(JK,IK)+MSII
          END DO
C--------- m13,m23
          MSII = -HALF*MSND
          DO J=1,NDOF(II)
            IK = IDDL(II)+J
            JK = IDDL(ID)+J
            ME(IK,JK)=ME(IK,JK)+MSII
            ME(JK,IK)=ME(JK,IK)+MSII
          END DO
          DO J=1,NDOF(JJ)
            IK = IDDL(JJ)+J
            JK = IDDL(ID)+J
            ME(IK,JK)=ME(IK,JK)+MSII
            ME(JK,IK)=ME(JK,IK)+MSII
          END DO
        END DO !I =1,NS10E
        DO I =1,ND
          DO J =I,ND
            ME(J,I)=ME(I,J)
          END DO
        END DO
        CALL JACOBIEN(ME,ND,EW,EV,TOL,LAMDA)
C-------[EV]'-> [EV]*EW^-1/2
        DO I =1,ND
c        print *,'M, ME(I,I),I=',ME(I,I),I
          EW(I)=ONE/SQRT(EW(I))
        END DO
        DO I =1,ND
          DO J =1,ND
            EV(I,J)=EV(I,J)*EW(J)
          END DO
        END DO
C-------[K]-> [EV]^t*[K]*[EV]
        ME(1:ND,1:ND) = ZERO
        DO I=1,ND
          DO J=1,ND
            DO K = 1,ND
              ME(I,J)=ME(I,J)+KE(I,K)*EV(K,J)
            ENDDO
          ENDDO
        ENDDO
        KE(1:ND,1:ND) = ZERO
        DO I=1,ND
          DO J=1,ND
            DO K = 1,ND
              KE(I,J)=KE(I,J)+EV(K,I)*ME(K,J)
            ENDDO
          ENDDO
        ENDDO
C
        RETURN
      END
Chd|====================================================================
Chd|  GETIKCE                       source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        SPB_REF_NDS                   source/implicit/imp_solv.F    
Chd|        SPB_RM_RIG                    source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GETIKCE(ICT ,ICR,K,IFIX )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ICT,ICR,IFIX(6),K
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER ND
C----------------BC-------------------------
        IFIX(1:6) = 0
        ND = 0
        IF (ICT > 0 .AND. K> 0) THEN
          IF (ICT == 4 .AND. K>2) THEN
            IFIX(ND +1) = 1
          ELSEIF (ICT == 2) THEN
            IFIX(ND +2) = 1
          ELSEIF (ICT == 1) THEN
            IFIX(ND +3) = 1
          ELSEIF (ICT == 3) THEN
            IFIX(ND +2) = 1
            IFIX(ND +3) = 1
          ELSEIF (ICT == 5) THEN
            IF (K>2) IFIX(ND +1) = 1
            IFIX(ND +3) = 1
          ELSEIF (ICT == 6) THEN
            IF (K>2) IFIX(ND +1) = 1
            IFIX(ND +2) = 1
          ELSEIF (ICT == 7) THEN
            IF (K>2) IFIX(ND +1) = 1
            IFIX(ND +2) = 1
            IFIX(ND +3) = 1
          ENDIF
        ENDIF
C
        IF (ICR > 0 .AND. K==6) THEN
          IF (ICR == 1) THEN
            IFIX(ND +6) = 1
          ELSEIF (ICR == 2) THEN
            IFIX(ND +5) = 1
          ELSEIF (ICR == 3) THEN
            IFIX(ND +5) = 1
            IFIX(ND +6) = 1
          ELSEIF (ICR == 4) THEN
            IFIX(ND +4) = 1
          ELSEIF (ICR == 5) THEN
            IFIX(ND +4) = 1
            IFIX(ND +6) = 1
          ELSEIF (ICR == 6) THEN
            IFIX(ND +4) = 1
            IFIX(ND +5) = 1
          ELSEIF (ICR == 7) THEN
            IFIX(ND +4) = 1
            IFIX(ND +5) = 1
            IFIX(ND +6) = 1
          ENDIF
        ENDIF
C
        RETURN
      END
Chd|====================================================================
Chd|  TRANSVG2L                     source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        SPB_RM_RIG                    source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TRANSVG2L(SKEW  ,VG  ,VL  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        my_real
     .     SKEW(*),VG(*),VL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER J
C----------------------------------------
        VL(1)=SKEW(1)*VG(1)+SKEW(2)*VG(2)+SKEW(3)*VG(3)
        VL(2)=SKEW(4)*VG(1)+SKEW(5)*VG(2)+SKEW(6)*VG(3)
        VL(3)=SKEW(7)*VG(1)+SKEW(8)*VG(2)+SKEW(9)*VG(3)
C
        RETURN
      END
Chd|====================================================================
Chd|  TRANSVL2G                     source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        SPB_RM_RIG                    source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TRANSVL2G(SKEW  ,VL  ,VG  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        my_real
     .     SKEW(*),VG(*),VL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER J
C----------------------------------------
        VG(1)=SKEW(1)*VL(1)+SKEW(4)*VL(2)+SKEW(7)*VL(3)
        VG(2)=SKEW(2)*VL(1)+SKEW(5)*VL(2)+SKEW(8)*VL(3)
        VL(3)=SKEW(3)*VL(1)+SKEW(6)*VL(2)+SKEW(9)*VL(3)
C
        RETURN
      END
Chd|====================================================================
Chd|  SPB_REF_NDS                   source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        SPB_RM_RIG                    source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        GETIKCE                       source/implicit/imp_solv.F    
Chd|        IMP_SPBRM                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE SPB_REF_NDS(
     1    X         ,D_IMP     ,NDOF      ,IDDL      ,IKC       ,
     2    ICODT     ,ICODR     ,ISKEW     ,SKEW      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE IMP_SPBRM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER
     .     NDOF(*),IDDL(*),IKC(*),ICODT(*),ICODR(*),ISKEW(*)
C     REAL
        my_real
     .     X(3,*) ,D_IMP(3,*) ,SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER  N,I,J,ND,NN,IE,IEM,K,ISK
        my_real
     .         D(3),DD,DMINT
C------case input 3 ref_nodes
        X_REF(1:3,1:4) = ZERO
        D_REF(1:3,1:4) = ZERO
        N_SEG = 3
        DO J= 1,N_SEG
          N = E_REF(J)
          IF (N==0) CYCLE
          X_REF(1:3,J)= X(1:3,N)
          D_REF(1:3,J)= D_IMP(1:3,N)
        END DO
        IF (ILSKEW>0) THEN
C----get ISK(from E_REF)set [Q],transfert DTRA,DROT to local reset IKCE(6) put (j)=0 for fixing dir
          NN = ILSKEW
          ISK = ISKEW(NN)
C---------local IKC
          RLSKEW(1:9) = SKEW(1:9,ISK)
          CALL GETIKCE(ICODT(NN),ICODR(NN),NDOF(NN),IKCE)
        END IF
C------------------------------------------
        RETURN
      END
C
Chd|====================================================================
Chd|  DEALLOC_IMPBUF                source/implicit/imp_solv.F    
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        IMPBUFDEF_MOD                 share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE DEALLOC_IMPBUF(IMPBUF_TAB)
C-----------------------------------------------
        USE IMPBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE (IMPBUF_STRUCT_) IMPBUF_TAB
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
C
        IF (ALLOCATED(IMPBUF_TAB%IDDL)) DEALLOCATE(IMPBUF_TAB%IDDL)
        IF (ALLOCATED(IMPBUF_TAB%NDOF)) DEALLOCATE(IMPBUF_TAB%NDOF)
        IF (ALLOCATED(IMPBUF_TAB%INLOC)) DEALLOCATE(IMPBUF_TAB%INLOC)
        IF (ALLOCATED(IMPBUF_TAB%IRBYAC))DEALLOCATE(IMPBUF_TAB%IRBYAC)
        IF (ALLOCATED(IMPBUF_TAB%NSC))   DEALLOCATE(IMPBUF_TAB%NSC)
        IF (ALLOCATED(IMPBUF_TAB%IINT2)) DEALLOCATE(IMPBUF_TAB%IINT2)
        IF (ALLOCATED(IMPBUF_TAB%NKUD))  DEALLOCATE(IMPBUF_TAB%NKUD)
        IF (ALLOCATED(IMPBUF_TAB%IMONV)) DEALLOCATE(IMPBUF_TAB%IMONV)
        IF (ALLOCATED(IMPBUF_TAB%IADK)) DEALLOCATE(IMPBUF_TAB%IADK)
        IF (ALLOCATED(IMPBUF_TAB%JDIK)) DEALLOCATE(IMPBUF_TAB%JDIK)
        IF (ALLOCATED(IMPBUF_TAB%IKINW)) DEALLOCATE(IMPBUF_TAB%IKINW)
        IF (ALLOCATED(IMPBUF_TAB%IKC)) DEALLOCATE(IMPBUF_TAB%IKC)
        IF (ALLOCATED(IMPBUF_TAB%IKUD)) DEALLOCATE(IMPBUF_TAB%IKUD)
        IF (ALLOCATED(IMPBUF_TAB%W_DDL)) DEALLOCATE(IMPBUF_TAB%W_DDL)
        IF (ALLOCATED(IMPBUF_TAB%IADM)) DEALLOCATE(IMPBUF_TAB%IADM)
        IF (ALLOCATED(IMPBUF_TAB%JDIM)) DEALLOCATE(IMPBUF_TAB%JDIM)
        IF (ALLOCATED(IMPBUF_TAB%CAND_N)) DEALLOCATE(IMPBUF_TAB%CAND_N)
        IF (ALLOCATED(IMPBUF_TAB%CAND_E)) DEALLOCATE(IMPBUF_TAB%CAND_E)
        IF (ALLOCATED(IMPBUF_TAB%INDSUBT)) DEALLOCATE(IMPBUF_TAB%INDSUBT)
        IF (ALLOCATED(IMPBUF_TAB%NDOFI)) DEALLOCATE(IMPBUF_TAB%NDOFI)
        IF (ALLOCATED(IMPBUF_TAB%IDDLI)) DEALLOCATE(IMPBUF_TAB%IDDLI)
        IF (ALLOCATED(IMPBUF_TAB%INBUF_C)) DEALLOCATE(IMPBUF_TAB%INBUF_C)
        IF (ALLOCATED(IMPBUF_TAB%DIAG_K)) DEALLOCATE(IMPBUF_TAB%DIAG_K)
        IF (ALLOCATED(IMPBUF_TAB%LT_K))   DEALLOCATE(IMPBUF_TAB%LT_K)
        IF (ALLOCATED(IMPBUF_TAB%DIAG_M)) DEALLOCATE(IMPBUF_TAB%DIAG_M)
        IF (ALLOCATED(IMPBUF_TAB%LT_M))   DEALLOCATE(IMPBUF_TAB%LT_M)
        IF (ALLOCATED(IMPBUF_TAB%LB))     DEALLOCATE(IMPBUF_TAB%LB)
        IF (ALLOCATED(IMPBUF_TAB%LB0))    DEALLOCATE(IMPBUF_TAB%LB0)
        IF (ALLOCATED(IMPBUF_TAB%BKUD))   DEALLOCATE(IMPBUF_TAB%BKUD)
        IF (ALLOCATED(IMPBUF_TAB%D_IMP))  DEALLOCATE(IMPBUF_TAB%D_IMP)
        IF (ALLOCATED(IMPBUF_TAB%DR_IMP)) DEALLOCATE(IMPBUF_TAB%DR_IMP)
        IF (ALLOCATED(IMPBUF_TAB%ELBUF_C)) DEALLOCATE(IMPBUF_TAB%ELBUF_C)
        IF (ALLOCATED(IMPBUF_TAB%BUFMAT_C))DEALLOCATE(IMPBUF_TAB%BUFMAT_C)
        IF (ALLOCATED(IMPBUF_TAB%X_C)) DEALLOCATE(IMPBUF_TAB%X_C)
        IF (ALLOCATED(IMPBUF_TAB%DD)) DEALLOCATE(IMPBUF_TAB%DD)
        IF (ALLOCATED(IMPBUF_TAB%DDR)) DEALLOCATE(IMPBUF_TAB%DDR)
        IF (ALLOCATED(IMPBUF_TAB%X_A))    DEALLOCATE(IMPBUF_TAB%X_A)
        IF (ALLOCATED(IMPBUF_TAB%FEXT))   DEALLOCATE(IMPBUF_TAB%FEXT)
        IF (ALLOCATED(IMPBUF_TAB%DG))     DEALLOCATE(IMPBUF_TAB%DG)
        IF (ALLOCATED(IMPBUF_TAB%DGR))    DEALLOCATE(IMPBUF_TAB%DGR)
        IF (ALLOCATED(IMPBUF_TAB%DG0))     DEALLOCATE(IMPBUF_TAB%DG0)
        IF (ALLOCATED(IMPBUF_TAB%DGR0))    DEALLOCATE(IMPBUF_TAB%DGR0)
        IF (ALLOCATED(IMPBUF_TAB%BUFIN_C)) DEALLOCATE(IMPBUF_TAB%BUFIN_C)
        IF (ALLOCATED(IMPBUF_TAB%AC)) DEALLOCATE(IMPBUF_TAB%AC)
        IF (ALLOCATED(IMPBUF_TAB%ACR)) DEALLOCATE(IMPBUF_TAB%ACR)
C
        RETURN
      END

