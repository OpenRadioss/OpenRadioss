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
      !||    impbufdef_mod    ../engine/share/modules/impbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    dealloc_impbuf   ../engine/source/implicit/imp_solv.F
      !||    imp_buck         ../engine/source/implicit/imp_buck.F
      !||    imp_chkm         ../engine/source/implicit/imp_solv.F
      !||    imp_fout         ../engine/source/implicit/imp_solv.F
      !||    imp_restarcp     ../engine/source/implicit/imp_sol_init.F
      !||    imp_sol_init     ../engine/source/implicit/imp_sol_init.F
      !||    imp_solv         ../engine/source/implicit/imp_solv.F
      !||    resol            ../engine/source/engine/resol.F
      !||    wrrestp          ../engine/source/output/restart/wrrestp.F
      !||====================================================================
      MODULE IMPBUFDEF_MOD
C----------------------------------------------------------------------- 
#include "my_real.inc"
C=======================================================================
c DEFINE SIZES (integers arrays)
C=======================================================================
        integer ::   S_IADK               
        integer ::   S_JDIK     
          integer ::   S_ISIJ 
          integer ::   S_IMIJ 
          integer ::   S_NSS  
          integer ::   S_ISS  
          integer ::   S_ISIJ2
          integer ::   S_NSS2 
          integer ::   S_ISS2 
          integer ::   S_NSC2 
          integer ::   S_NROWK
          integer ::   S_ICOK 
          integer ::   S_ICOKM
          integer ::   S_NMIJ2
          integer ::   S_NSS3 
          integer ::   S_ISB2 
          integer ::   S_NSRB2
              integer ::   S_IKC  
              integer ::   S_IKUD 
              integer ::   S_W_DDL
              integer ::   S_IADM 
        integer ::   S_JDIM     
              integer ::   S_NDOFI
              integer ::   S_IDDLI
              integer ::   S_IKINW
          integer ::   S_INBUF_C
C-
        integer ::   S_I_STOK     !  replace NUM_IMP            
        integer ::   S_CAND_N     !  replace NS_IMP  
        integer ::   S_CAND_E     !  replace NE_IMP   
        integer ::   S_INDSUBT    !  replace IND_IMP  
C=======================================================================
c DEFINE SIZES (float arrays)
C=======================================================================
        integer ::   S_DIAG_K           
        integer ::   S_LT_K           
        integer ::   S_DIAG_M           
        integer ::   S_LT_M           
              integer ::   S_LB
              integer ::   S_LB0
              integer ::   S_BKUD
              integer ::   S_D_IMP
              integer ::   S_DR_IMP
        integer ::   S_ELBUF_C
          integer ::   S_BUFMAT_C
          integer ::   S_X_C
          integer ::   S_DD
          integer ::   S_DDR
          integer ::   S_X_A
          integer ::   S_FEXT
          integer ::   S_DG
          integer ::   S_DGR
          integer ::   S_DG0
          integer ::   S_DGR0
          integer ::   S_BUFIN_C
          integer ::   S_AC
          integer ::   S_ACR
c=======================================================================      
c define type IMPBUF_STRUCT_ for implicit structure array
c=======================================================================      
C=======================================================================
      TYPE IMPBUF_STRUCT_
C=======================================================================
c integers & small integer arrays
C=======================================================================
        integer ::   NDDL               
        integer ::   NNZK               
C----NS_IMP... will be changed to POINTER and pt to %I_STOK... in resol
        integer ::   NRBYAC               
        integer ::   NINT2               
        integer ::   NMC               
        integer ::   NMC2               
        integer ::   NMONV               
        integer, DIMENSION(20) ::  LSIZE      
        integer, DIMENSION(20) ::  I_IMP      
C=======================================================================
c DEFINE ARRAYS (integers arrays)
C=======================================================================
        integer, DIMENSION(:) , ALLOCATABLE ::  IRBYAC      
        integer, DIMENSION(:) , ALLOCATABLE ::  NSC      
        integer, DIMENSION(:) , ALLOCATABLE ::  IINT2      
        integer, DIMENSION(:) , ALLOCATABLE ::  NKUD      
        integer, DIMENSION(:) , ALLOCATABLE ::  IMONV      
        integer, DIMENSION(:) , ALLOCATABLE ::  IDDL      
        integer, DIMENSION(:) , ALLOCATABLE ::  NDOF      
        integer, DIMENSION(:) , ALLOCATABLE ::  INLOC    
        integer, DIMENSION(:) , ALLOCATABLE ::  IADK    
        integer, DIMENSION(:) , ALLOCATABLE ::  JDIK    
          integer, DIMENSION(:) , ALLOCATABLE ::  ISIJ 
          integer, DIMENSION(:) , ALLOCATABLE ::  IMIJ 
          integer, DIMENSION(:) , ALLOCATABLE ::  NSS  
          integer, DIMENSION(:) , ALLOCATABLE ::  ISS  
          integer, DIMENSION(:) , ALLOCATABLE ::  ISIJ2
          integer, DIMENSION(:) , ALLOCATABLE ::  NSS2 
          integer, DIMENSION(:) , ALLOCATABLE ::  ISS2 
          integer, DIMENSION(:) , ALLOCATABLE ::  NSC2 
          integer, DIMENSION(:) , ALLOCATABLE ::  NROWK
          integer, DIMENSION(:) , ALLOCATABLE ::  ICOK 
          integer, DIMENSION(:) , ALLOCATABLE ::  ICOKM
          integer, DIMENSION(:) , ALLOCATABLE ::  NMIJ2
          integer, DIMENSION(:) , ALLOCATABLE ::  NSS3 
          integer, DIMENSION(:) , ALLOCATABLE ::  ISB2 
          integer, DIMENSION(:) , ALLOCATABLE ::  NSRB2
              integer, DIMENSION(:) , ALLOCATABLE ::  IKC  
              integer, DIMENSION(:) , ALLOCATABLE ::  IKUD 
              integer, DIMENSION(:) , ALLOCATABLE ::  W_DDL
              integer, DIMENSION(:) , ALLOCATABLE ::  IADM 
              integer, DIMENSION(:) , ALLOCATABLE ::  JDIM 
              integer, DIMENSION(:) , ALLOCATABLE ::  NDOFI
              integer, DIMENSION(:) , ALLOCATABLE ::  IDDLI
              integer, DIMENSION(:) , ALLOCATABLE ::  IKINW
              integer, DIMENSION(:) , ALLOCATABLE ::  INBUF_C
C---for contact
        integer, DIMENSION(:) , ALLOCATABLE ::  I_STOK      
        integer, DIMENSION(:) , ALLOCATABLE ::  CAND_N      
        integer, DIMENSION(:) , ALLOCATABLE ::  CAND_E    
        integer, DIMENSION(:) , ALLOCATABLE ::  INDSUBT   
C=======================================================================
c DEFINE ARRAYS (float arrays)
C=======================================================================
        my_real, DIMENSION(25) ::  R_IMP      
        my_real, DIMENSION(:) , ALLOCATABLE ::   DIAG_K           
        my_real, DIMENSION(:) , ALLOCATABLE ::   LT_K           
        my_real, DIMENSION(:) , ALLOCATABLE ::   DIAG_M           
        my_real, DIMENSION(:) , ALLOCATABLE ::   LT_M           
              my_real, DIMENSION(:) , ALLOCATABLE ::   LB
              my_real, DIMENSION(:) , ALLOCATABLE ::   LB0
              my_real, DIMENSION(:) , ALLOCATABLE ::   BKUD
              my_real, DIMENSION(:) , ALLOCATABLE ::   D_IMP
              my_real, DIMENSION(:) , ALLOCATABLE ::   DR_IMP
        my_real, DIMENSION(:) , ALLOCATABLE ::   ELBUF_C
          my_real, DIMENSION(:) , ALLOCATABLE ::   BUFMAT_C
          my_real, DIMENSION(:) , ALLOCATABLE ::   X_C
          my_real, DIMENSION(:) , ALLOCATABLE ::   DD
          my_real, DIMENSION(:) , ALLOCATABLE ::   DDR
          my_real, DIMENSION(:) , ALLOCATABLE ::   X_A
          my_real, DIMENSION(:) , ALLOCATABLE ::   FEXT
          my_real, DIMENSION(:) , ALLOCATABLE ::   DG
          my_real, DIMENSION(:) , ALLOCATABLE ::   DGR
          my_real, DIMENSION(:) , ALLOCATABLE ::   DG0
          my_real, DIMENSION(:) , ALLOCATABLE ::   DGR0
          my_real, DIMENSION(:) , ALLOCATABLE ::   BUFIN_C
          my_real, DIMENSION(:) , ALLOCATABLE ::   AC
          my_real, DIMENSION(:) , ALLOCATABLE ::   ACR
C=======================================================================
      END TYPE IMPBUF_STRUCT_
C=======================================================================
c---------------
      END MODULE IMPBUFDEF_MOD
      !||====================================================================
      !||    imp_ktan_def   ../engine/share/modules/impbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    cmatch3        ../engine/source/elements/shell/coqueba/cmatc3.F
      !||    cmatip3        ../engine/source/elements/shell/coqueba/cmatc3.F
      !||    etfac_ini      ../engine/source/implicit/imp_init.F
      !||    get_etfac_s    ../engine/source/elements/solid/solide8z/get_etfac_s.F
      !||    gethkt3        ../engine/source/elements/solid/solide8z/gethkt3.F
      !||    iktmat_ini     ../engine/source/implicit/imp_init.F
      !||    imp_ktan       ../engine/share/modules/impbufdef_mod.F
      !||    ktbuf_ini      ../engine/source/implicit/imp_init.F
      !||    mmats          ../engine/source/elements/solid/solide8z/mmats.F
      !||    put_etfac      ../engine/source/elements/solid/solide8z/put_etfac.F
      !||    putsignor3     ../engine/source/elements/solid/solide8z/putsignor3.F
      !||    putsignorc3    ../engine/source/elements/shell/coqueba/cmatc3.F
      !||    sktcons2       ../engine/source/elements/solid/solide8z/sktcons2.F
      !||====================================================================
      MODULE IMP_KTAN_DEF
#include "my_real.inc"
C------
      TYPE MLAW_TAG_    ! element buffer variables depending on material law
        INTEGER ::   L_ETFAC      ! = 1
        INTEGER ::   L_SIGE       ! = 6 ; = 5 for shell
        INTEGER ::   L_A_KT       
        INTEGER ::   L_S UBKT     ! =6
      END TYPE MLAW_TAG_  
      
C      TYPE L_KTBUFG_      ! general law (hyper-elastic, elasto-plastic)
C        my_real, DIMENSION(:) , POINTER ::   ETFAC    !  relative tangent modulus ratio per ele
C      END TYPE L_KTBUFG_
      
      TYPE L_KTBUFEP_      ! elastoplastic material per integration point
        my_real, DIMENSION(:) , POINTER ::   A_KT     !  alpha   dgama for shell
        my_real, DIMENSION(:) , POINTER ::   SIGE     !  trail stress                              
      END TYPE L_KTBUFEP_ 
      
      TYPE L_KTBUFGM_      !  modulus KT general matrix per integration point
C----------------|DD(L_SUBKT) | DG(L_SUBKT) |
C----------------|  sym       | GG(L_SUBKT) |
        my_real, DIMENSION(:) , POINTER ::   KT_DD
        my_real, DIMENSION(:) , POINTER ::   KT_DG
        my_real, DIMENSION(:) , POINTER ::   KT_GG
      END TYPE L_KTBUFGM_

      TYPE KTBUF_STRUCT_
C------the following integer arguments are not necessary for the moment
c        integer     :: MLAW           !  1
c        integer     :: NEL            !  2
c        integer     :: NPTR           !  3
c        integer     :: NPTS           !  4
c        integer     :: NPTT           !  5
C ------------add MLAW_TAG here to avoid pass everywhere the arguments
        my_real  ,          DIMENSION(:)       , ALLOCATABLE :: ETFAC   ! relative tangent modulus ratio - per element(global)
        TYPE(MLAW_TAG_)   , DIMENSION(:)       , ALLOCATABLE :: MLAW_TAG ! variable dimensions - per element(global)
        TYPE (L_KTBUFEP_) , DIMENSION(:,:,:)   , ALLOCATABLE :: KTBUFEP  ! EP mat - per integration point
        TYPE (L_KTBUFGM_) , DIMENSION(:,:,:)   , ALLOCATABLE :: KTBUFMG  ! ET mat - per integration point
      END TYPE KTBUF_STRUCT_
C      
      END MODULE IMP_KTAN_DEF
      !||====================================================================
      !||    imp_bfgs       ../engine/share/modules/impbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    bfgs_0         ../engine/source/implicit/imp_bfgs.F
      !||    bfgs_1         ../engine/source/implicit/imp_bfgs.F
      !||    bfgs_1p        ../engine/source/implicit/imp_bfgs.F
      !||    bfgs_2         ../engine/source/implicit/imp_bfgs.F
      !||    bfgs_2p        ../engine/source/implicit/imp_bfgs.F
      !||    bfgs_h1        ../engine/source/implicit/imp_bfgs.F
      !||    bfgs_h1p       ../engine/source/implicit/imp_bfgs.F
      !||    bfgs_h2        ../engine/source/implicit/imp_bfgs.F
      !||    bfgs_h2p       ../engine/source/implicit/imp_bfgs.F
      !||    bfgs_ini       ../engine/source/implicit/imp_bfgs.F
      !||    bfgs_ls        ../engine/source/implicit/imp_bfgs.F
      !||    deallocm_imp   ../engine/source/implicit/imp_solv.F
      !||    get_slin       ../engine/source/implicit/imp_bfgs.F
      !||    nsloan_0       ../engine/source/implicit/imp_bfgs.F
      !||    nsloan_5       ../engine/source/implicit/imp_bfgs.F
      !||====================================================================
      MODULE IMP_BFGS
C
#include "my_real.inc"
      INTEGER N_BFGS,IACTB
      my_real S_LIN
C
C tableau de int de : V,W pour BFGS algorithm----
      my_real, DIMENSION(:,:), ALLOCATABLE :: BFGS_V,BFGS_W
C 
      END MODULE IMP_BFGS
      !||====================================================================
      !||    imp_dyna       ../engine/share/modules/impbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    deallocm_imp   ../engine/source/implicit/imp_solv.F
      !||    du_ini         ../engine/source/implicit/imp_solv.F
      !||    du_ini_hp      ../engine/source/implicit/imp_solv.F
      !||    dyna_cpk0      ../engine/source/implicit/imp_dyna.F
      !||    dyna_cpr0      ../engine/source/implicit/imp_dyna.F
      !||    dyna_in0       ../engine/source/implicit/imp_dyna.F
      !||    dyna_ina       ../engine/source/implicit/imp_dyna.F
      !||    dyna_ini       ../engine/source/implicit/imp_dyna.F
      !||    dyna_iniv      ../engine/source/implicit/imp_dyna.F
      !||    dyna_ivfac     ../engine/source/implicit/imp_dyna.F
      !||    dyna_wex       ../engine/source/implicit/imp_dyna.F
      !||    ecrit          ../engine/source/output/ecrit.F
      !||    freimpl        ../engine/source/input/freimpl.F
      !||    fv_fint0       ../engine/source/constraints/general/impvel/fv_imp0.F
      !||    getdyna_a      ../engine/source/implicit/imp_dyna.F
      !||    imp_dycrb      ../engine/source/implicit/imp_dyna.F
      !||    imp_dykv       ../engine/source/implicit/imp_dyna.F
      !||    imp_dykv0      ../engine/source/implicit/imp_dyna.F
      !||    imp_dynam      ../engine/source/implicit/imp_dyna.F
      !||    imp_dynar      ../engine/source/implicit/imp_dyna.F
      !||    imp_fhht       ../engine/source/implicit/imp_dyna.F
      !||    imp_fhht1      ../engine/source/implicit/imp_dyna.F
      !||    imprrest       ../engine/source/output/restart/rdresb.F
      !||    impwrest       ../engine/source/output/restart/wrrest.F
      !||    inte_dyna      ../engine/source/implicit/imp_dyna.F
      !||    rbe2cor        ../engine/source/constraints/general/rbody/rgbcor.F
      !||    rbe2cor0       ../engine/source/constraints/general/rbody/rgbcor.F
      !||    rbycor         ../engine/source/constraints/general/rbody/rbycor.F
      !||    rgbcor         ../engine/source/constraints/general/rbody/rgbcor.F
      !||    stop_sensor    ../engine/source/tools/sensor/stop_sensor.F
      !||    wfv_imp        ../engine/source/constraints/general/impvel/fv_imp0.F
      !||====================================================================
      MODULE IMP_DYNA
C
#include "my_real.inc"
C
C tableau de travail :  implicit dynamic ----
      my_real  DY_G,DY_B,DY_EDAMP
      my_real, DIMENSION(:,:), ALLOCATABLE :: DY_D,DY_DR,DY_V
      my_real, DIMENSION(:,:), ALLOCATABLE :: DY_VR,DY_A,DY_AR
      my_real, DIMENSION(:,:), ALLOCATABLE :: DY_IN
      my_real, DIMENSION(:,:), ALLOCATABLE :: DY_DAM,DY_DAMR
      my_real, DIMENSION(:), ALLOCATABLE :: DY_DIAK0,DY_LTK0
      my_real, DIMENSION(:), ALLOCATABLE :: DY_R0,DY_R1
      my_real, DIMENSION(:,:), ALLOCATABLE :: DY_DAM0,DY_DAMR0
      INTEGER, DIMENSION(:), ALLOCATABLE :: DY_IADK0,DY_JDIK0
C 
      END MODULE IMP_DYNA
      !||====================================================================
      !||    imp_monv    ../engine/share/modules/impbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    id_mvini    ../engine/source/airbag/monv_imp0.F
      !||    monv_diag   ../engine/source/airbag/monv_imp0.F
      !||    monv_fvl    ../engine/source/airbag/monv_imp0.F
      !||    monv_m3     ../engine/source/airbag/monv_imp0.F
      !||    monv_prem   ../engine/source/airbag/monv_imp0.F
      !||    mv_matv     ../engine/source/airbag/monv_imp0.F
      !||    recu_kdis   ../engine/source/airbag/monv_imp0.F
      !||====================================================================
      MODULE IMP_MONV
#include "my_real.inc"
      INTEGER NUMN_MV,NI2_MV,NRB_MV,NNMAX_MV,NBC_MV,NFX_MV,NRW_MV,
     .        NRBE3_MV,R3M_MAX,NSPC_MV,NRBE2_MV
C  int de taille NUMN_MV :noeuds locales de mv 
      INTEGER, DIMENSION(:), ALLOCATABLE :: IN_MV
C  int IDDL de taille 3*NUMN_MV,6*(NRB_FR ,4*NI2_FR)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ID_MV,ID_MVM,ID_MVM4
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: ID_MVM2,ID_MVM3
C int de taille NI2_MV,NRB_MV :------
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: II2_MV,IRB_MV,IRBE2_MV
      INTEGER, DIMENSION(:), ALLOCATABLE ::   IRBE3_MV
C  K_DIAG reels de taille (3,NUMN_MV) 
      my_real, DIMENSION(:,:),ALLOCATABLE :: DIAG_MV
C K_DIAG reels de taille (6,NRB_MV) 
      my_real, DIMENSION(:,:),ALLOCATABLE :: DIAG_MVM,DIAG_MVM4
C K_DIAG reels de taille (6,4,NI2_MV) 
      my_real, DIMENSION(:,:,:),ALLOCATABLE :: DIAG_MVM2
      my_real, DIMENSION(:,:,:),ALLOCATABLE :: DIAG_MVM3
C int (3,NBC_MV),(2,NFX_MV),(NRW_MV)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IBC_MV,IFX_MV
      INTEGER, DIMENSION(:), ALLOCATABLE :: IRW_MV,ISPC_MV
      my_real, DIMENSION(:), ALLOCATABLE :: FCDI_MV,MCDI_MV
C      
      END MODULE IMP_MONV
      !||====================================================================
      !||    imp_rwl      ../engine/share/modules/impbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    diag_int     ../engine/source/mpi/implicit/imp_fri.F
      !||    dim_kinkn    ../engine/source/implicit/imp_int_k.F
      !||    fr_u2dd      ../engine/source/mpi/implicit/imp_fri.F
      !||    fv_rwl       ../engine/source/constraints/general/rwall/srw_imp.F
      !||    fv_rwl0      ../engine/source/constraints/general/rwall/srw_imp.F
      !||    fv_rwlr0     ../engine/source/constraints/general/rwall/srw_imp.F
      !||    imp3_a2b     ../engine/source/airbag/monv_imp0.F
      !||    imp3_u2x     ../engine/source/airbag/monv_imp0.F
      !||    ind_kinefr   ../engine/source/mpi/implicit/imp_fri.F
      !||    ind_kinfrk   ../engine/source/mpi/implicit/imp_fri.F
      !||    ini_kinkn    ../engine/source/implicit/imp_int_k.F
      !||    monv_fvl     ../engine/source/airbag/monv_imp0.F
      !||    rwl_impd     ../engine/source/constraints/general/rwall/srw_imp.F
      !||    upd_fr       ../engine/source/mpi/implicit/imp_fri.F
      !||    upd_kml      ../engine/source/mpi/implicit/imp_fri.F
      !||    upd_ksl      ../engine/source/mpi/implicit/imp_fri.F
      !||    updk_mv      ../engine/source/airbag/monv_imp0.F
      !||====================================================================
      MODULE IMP_RWL
#include "my_real.inc"
      INTEGER N_RWL
C tableau de int de taille N_RWL :noeuds secnds de sliding RW 
      INTEGER, DIMENSION(:), ALLOCATABLE :: IN_RWL
C tableau de Direction normale reels de taille (3,N_RWL) 
      my_real, DIMENSION(:,:),ALLOCATABLE :: NOR_RWL
C      
      END MODULE IMP_RWL
      !||====================================================================
      !||    imp_lintf   ../engine/share/modules/impbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    diag_kif    ../engine/source/implicit/imp_solv.F
      !||    imp_solv    ../engine/source/implicit/imp_solv.F
      !||    ini_kif     ../engine/source/implicit/imp_solv.F
      !||    matv_kif    ../engine/source/implicit/imp_solv.F
      !||    save_kif    ../engine/source/implicit/imp_solv.F
      !||====================================================================
      MODULE IMP_LINTF
#include "my_real.inc"
      INTEGER NDDLIF,NZIF
C tableau de int de la [k] interface 
      INTEGER, DIMENSION(:), ALLOCATABLE :: IADIF,JDIIF,IFTOK
C tableau de reels de la [k] interface 
      my_real, DIMENSION(:),ALLOCATABLE :: DIAG_IF,LT_IF
C      
      END MODULE IMP_LINTF
      !||====================================================================
      !||    imp_rest     ../engine/share/modules/impbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    imp_trans    ../engine/source/output/restart/wrrest.F
      !||    imp_trans0   ../engine/source/output/restart/wrrest.F
      !||    imprrest     ../engine/source/output/restart/rdresb.F
      !||    impwrest     ../engine/source/output/restart/wrrest.F
      !||====================================================================
      MODULE IMP_REST
#include "my_real.inc"
C tableau de reels de R-file 
      my_real, DIMENSION(:),ALLOCATABLE :: IMP_RR
C      
      END MODULE IMP_REST
C
      !||====================================================================
      !||    imp_ppat    ../engine/share/modules/impbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    dim_span    ../engine/source/implicit/ind_glob_k.F
      !||    fil_span0   ../engine/source/implicit/ind_glob_k.F
      !||    fil_span1   ../engine/source/implicit/ind_glob_k.F
      !||    ind_span    ../engine/source/implicit/ind_glob_k.F
      !||====================================================================
      MODULE IMP_PPAT
C tableau de int de pre-filtrage 
      INTEGER, DIMENSION(:),ALLOCATABLE :: PRE_FPAT
C      
      END MODULE IMP_PPAT
      !||====================================================================
      !||    imp_qstat      ../engine/share/modules/impbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    deallocm_imp   ../engine/source/implicit/imp_solv.F
      !||    dis_cp         ../engine/source/implicit/imp_solv.F
      !||    qstat_end      ../engine/source/implicit/imp_dyna.F
      !||    qstat_ini      ../engine/source/implicit/imp_dyna.F
      !||    qstat_it       ../engine/source/implicit/imp_dyna.F
      !||====================================================================
      MODULE IMP_QSTAT
C
#include "my_real.inc"
C
C tableau de travail :  linear quasi-static (iterative)----
      my_real, DIMENSION(:), ALLOCATABLE :: QS_U,QS_D
C tableau de travail : D_n-1 used for Gravity case
      my_real, DIMENSION(:), ALLOCATABLE :: D_N_1,DR_N_1
C 
      END MODULE IMP_QSTAT
      !||====================================================================
      !||    imp_kbcs       ../engine/share/modules/impbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    deallocm_imp   ../engine/source/implicit/imp_solv.F
      !||    freimpl        ../engine/source/input/freimpl.F
      !||    imp_init       ../engine/source/implicit/imp_init.F
      !||    imp_mumps1     ../engine/source/implicit/imp_mumps.F
      !||    lecimpl        ../engine/source/input/lectur.F
      !||    prout_buck     ../engine/source/input/lectur.F
      !||====================================================================
      MODULE IMP_KBCS
C tableau de travail de BCS-EXT 
      INTEGER  MEM_BCS
      my_real, DIMENSION(:),ALLOCATABLE :: HOLD
      INTEGER  MSGL_B,MAXSET_B,MSG_LVL,B_ORDER,B_MCORE,LMEMN
      my_real  EMIN_B,EMAX_B,SHIFT_B
C      
      END MODULE IMP_KBCS
      !||====================================================================
      !||    imp_knon     ../engine/share/modules/impbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    deallocm     ../engine/source/implicit/imp_solv.F
      !||    int_matv     ../engine/source/implicit/imp_int_k.F
      !||    int_matvp    ../engine/source/implicit/imp_int_k.F
      !||    kin_knl      ../engine/source/implicit/imp_int_k.F
      !||    recu_kdis0   ../engine/source/implicit/lin_solv.F
      !||====================================================================
      MODULE IMP_KNON
#include "my_real.inc"
      INTEGER NUMN_KN,NI2_KN,NRB_KN,NBC_KN,NFX_KN,NRW_KN,NRBE3_KN,
     .        RKN_MAX,NSPC_KN,NRBE2_KN
      INTEGER, DIMENSION(:), ALLOCATABLE :: IN_KN
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ID_KN,ID_KNM,ID_KNM4
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: ID_KNM2,ID_KNM3
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: II2_KN,IRB_KN,IRBE2_KN
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IBC_KN,IFX_KN
      INTEGER, DIMENSION(:), ALLOCATABLE :: IRW_KN,IRBE3_KN,ISPC_KN
      my_real, DIMENSION(:), ALLOCATABLE :: FCDI_KN,MCDI_KN
C      
      END MODULE IMP_KNON
      !||====================================================================
      !||    imp_aspc     ../engine/share/modules/impbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    bc_imp2      ../engine/source/constraints/general/bcs/bc_imp0.F
      !||    bc_impa      ../engine/source/constraints/general/bcs/bc_imp0.F
      !||    bc_impr1     ../engine/source/constraints/general/bcs/bc_imp0.F
      !||    diag_int     ../engine/source/mpi/implicit/imp_fri.F
      !||    dim_kinkn    ../engine/source/implicit/imp_int_k.F
      !||    fr_u2dd      ../engine/source/mpi/implicit/imp_fri.F
      !||    get_nspc     ../engine/source/constraints/general/bcs/bc_imp0.F
      !||    imp3_a2b     ../engine/source/airbag/monv_imp0.F
      !||    imp3_u2x     ../engine/source/airbag/monv_imp0.F
      !||    imp_init     ../engine/source/implicit/imp_init.F
      !||    ind_kinefr   ../engine/source/mpi/implicit/imp_fri.F
      !||    ind_kinfrk   ../engine/source/mpi/implicit/imp_fri.F
      !||    ini_kinkn    ../engine/source/implicit/imp_int_k.F
      !||    kin_knl      ../engine/source/implicit/imp_int_k.F
      !||    monv_fvl     ../engine/source/airbag/monv_imp0.F
      !||    put_nspc     ../engine/source/constraints/general/bcs/bc_imp0.F
      !||    upd_aspc0    ../engine/source/constraints/general/bcs/bc_imp0.F
      !||    upd_fr       ../engine/source/mpi/implicit/imp_fri.F
      !||    upd_kml      ../engine/source/mpi/implicit/imp_fri.F
      !||    upd_ksl      ../engine/source/mpi/implicit/imp_fri.F
      !||    updk_mv      ../engine/source/airbag/monv_imp0.F
      !||====================================================================
      MODULE IMP_ASPC
      INTEGER NSPCL,NSPCNT
#include "my_real.inc"
      INTEGER, DIMENSION(:),ALLOCATABLE :: IN_SPC,IC_SPC
      my_real, DIMENSION(:), ALLOCATABLE :: SKEW_SPC
C      
      END MODULE IMP_ASPC
C
      !||====================================================================
      !||    imp_worki   ../engine/share/modules/impbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    imp_chkm    ../engine/source/implicit/imp_solv.F
      !||    imp_solv    ../engine/source/implicit/imp_solv.F
      !||====================================================================
      MODULE IMP_WORKI
#include      "my_real.inc"
      INTEGER, DIMENSION(:),ALLOCATABLE :: IADI,JDII,ITOK
      INTEGER  NDDLI ,NNZI,ICONTA,NSREM ,NSL
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: DIAG_I,LT_I,XI_C
 
      END MODULE IMP_WORKI
C
      !||====================================================================
      !||    imp_workh      ../engine/share/modules/impbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    deallocm_imp   ../engine/source/implicit/imp_solv.F
      !||    imp_pcgh       ../engine/source/implicit/imp_pcg.F
      !||    imp_ppcgh      ../engine/source/implicit/imp_pcg.F
      !||    ini_k0h        ../engine/source/implicit/imp_solv.F
      !||    ini_kisc       ../engine/source/implicit/lin_solv.F
      !||    lin_solv       ../engine/source/implicit/lin_solv.F
      !||    lin_solvh0     ../engine/source/implicit/lin_solv.F
      !||    lin_solvh1     ../engine/source/implicit/lin_solv.F
      !||    lin_solvhm     ../engine/source/implicit/lin_solv.F
      !||    lin_solvih2    ../engine/source/implicit/lin_solv.F
      !||    mav_liuh       ../engine/source/implicit/produt_v.F
      !||    mav_lu_h       ../engine/source/implicit/produt_v.F
      !||    mav_lui_h      ../engine/source/implicit/produt_v.F
      !||    mmav_lth       ../engine/source/implicit/produt_v.F
      !||    prec5hc_solv   ../engine/source/implicit/prec_solv.F
      !||    produt_uh      ../engine/source/implicit/produt_v.F
      !||    produt_uh2     ../engine/source/implicit/produt_v.F
      !||    produt_vmh     ../engine/source/implicit/produt_v.F
      !||====================================================================
      MODULE IMP_WORKH
#include      "my_real.inc"
      INTEGER  W_MAXL,ISTOP_H
      my_real K_LAMDA0,K_LAMDA1
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: DIAG_T,L_U,L_F0
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: PCG_W1,PCG_W2,PCG_W3
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: TMP_W1,TMP_W2
#if defined knf
!dec$ attributes offload:mic :: IADK0,JDIK0,IADM0,JDIM0
#endif
      INTEGER, DIMENSION(:),ALLOCATABLE :: IADK0,JDIK0,IADM0,JDIM0
#if defined knf
!dec$ attributes offload:mic :: IADI0,JDII0
#endif
      INTEGER, DIMENSION(:),ALLOCATABLE :: IADI0,JDII0
#if defined knf
!dec$ attributes offload:mic :: LT_K0,LT_M0,LT_I0
#endif
      my_real,
     .        DIMENSION(:),ALLOCATABLE :: LT_K0,LT_M0,LT_I0
 
      END MODULE IMP_WORKH
C
      !||====================================================================
      !||    imp_workg   ../engine/share/modules/impbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    imp_buck    ../engine/source/implicit/imp_buck.F
      !||====================================================================
      MODULE IMP_WORKG
#include      "my_real.inc"
      INTEGER  IPRMES_EL(30)
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: DIAG_KG,LT_KG
 
      END MODULE IMP_WORKG
C
      !||====================================================================
      !||    imp_fvbcl     ../engine/share/modules/impbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    bc_impr1      ../engine/source/constraints/general/bcs/bc_imp0.F
      !||    fv_imp1       ../engine/source/constraints/general/impvel/fv_imp0.F
      !||    fvbc_allo     ../engine/source/constraints/general/impvel/fv_imp0.F
      !||    fvbc_deallo   ../engine/source/constraints/general/impvel/fv_imp0.F
      !||    recu_ul       ../engine/source/constraints/general/impvel/fv_imp0.F
      !||    recukin       ../engine/source/implicit/recudis.F
      !||    upd_glob_k    ../engine/source/implicit/upd_glob_k.F
      !||====================================================================
      MODULE IMP_FVBCL
#include      "my_real.inc"
      INTEGER  NFVBCL,NKUD_L,NKUD_1
      INTEGER, DIMENSION(:),ALLOCATABLE :: ICT_1,ICR_1,IKUD_1
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: BKUD_1,FVBCUDL
 
      END MODULE IMP_FVBCL
C
      !||====================================================================
      !||    imp_pcg_proj   ../engine/share/modules/impbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    deallocm_imp   ../engine/source/implicit/imp_solv.F
      !||    freimpl        ../engine/source/input/freimpl.F
      !||    imp_inisi      ../engine/source/implicit/imp_pcg.F
      !||    imp_inist      ../engine/source/implicit/imp_pcg.F
      !||    imp_inix       ../engine/source/implicit/imp_pcg.F
      !||    imp_ppcgh      ../engine/source/implicit/imp_pcg.F
      !||    imp_pro_p      ../engine/source/implicit/imp_pcg.F
      !||    imp_solv       ../engine/source/implicit/imp_solv.F
      !||    imp_updst      ../engine/source/implicit/imp_pcg.F
      !||    imp_updv2      ../engine/source/implicit/imp_pcg.F
      !||    ini_k0h        ../engine/source/implicit/imp_solv.F
      !||    lecimpl        ../engine/source/input/lectur.F
      !||====================================================================
      MODULE IMP_PCG_PROJ
#include      "my_real.inc"
      INTEGER  M_VS,NCG_RUN
      my_real
     .         K_TMP(2,2),M_TMP(2,2)
      my_real,
     .         DIMENSION(:,:),ALLOCATABLE :: PROJ_S,PROJ_T,PROJ_K
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: PROJ_LA_1,PROJ_V,PROJ_W
 
      END MODULE IMP_PCG_PROJ
C
      !||====================================================================
      !||    imp_ktan       ../engine/share/modules/impbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    cmatch3        ../engine/source/elements/shell/coqueba/cmatc3.F
      !||    cmatip3        ../engine/source/elements/shell/coqueba/cmatc3.F
      !||    etfac_ini      ../engine/source/implicit/imp_init.F
      !||    get_etfac_s    ../engine/source/elements/solid/solide8z/get_etfac_s.F
      !||    gethkt3        ../engine/source/elements/solid/solide8z/gethkt3.F
      !||    iktmat_ini     ../engine/source/implicit/imp_init.F
      !||    ktbuf_ini      ../engine/source/implicit/imp_init.F
      !||    mmats          ../engine/source/elements/solid/solide8z/mmats.F
      !||    put_etfac      ../engine/source/elements/solid/solide8z/put_etfac.F
      !||    putsignor3     ../engine/source/elements/solid/solide8z/putsignor3.F
      !||    putsignorc3    ../engine/source/elements/shell/coqueba/cmatc3.F
      !||    sktcons2       ../engine/source/elements/solid/solide8z/sktcons2.F
      !||--- uses       -----------------------------------------------------
      !||    imp_ktan_def   ../engine/share/modules/impbufdef_mod.F
      !||====================================================================
      MODULE IMP_KTAN

      USE IMP_KTAN_DEF

      TYPE(KTBUF_STRUCT_),TARGET,DIMENSION(:),ALLOCATABLE :: KTBUF_STR
      END MODULE IMP_KTAN
      !||====================================================================
      !||    imp_spbrm      ../engine/share/modules/impbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    deallocm_imp   ../engine/source/implicit/imp_solv.F
      !||    freimpl        ../engine/source/input/freimpl.F
      !||    lecimpl        ../engine/source/input/lectur.F
      !||    spb_ieref3     ../engine/source/implicit/imp_solv.F
      !||    spb_ieref_bc   ../engine/source/implicit/imp_solv.F
      !||    spb_ref_nds    ../engine/source/implicit/imp_solv.F
      !||    spb_rm_rig     ../engine/source/implicit/imp_solv.F
      !||    spbrm_pre      ../engine/source/implicit/imp_solv.F
      !||    spmd_e_ref     ../engine/source/mpi/implicit/imp_spmd.F
      !||    spmd_n_ref     ../engine/source/mpi/implicit/imp_spmd.F
      !||====================================================================
      MODULE IMP_SPBRM

      INTEGER  E_REF(4),NBC_B,NE_BC4,NE_BC3,IKCE(6),N_SEG,ILSKEW,LSKEW_G
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBC_B
      INTEGER, DIMENSION(:),ALLOCATABLE :: IE_BC4,IE_BC3
C------introduced due to SPMD-----      
      my_real
     .        X_REF(3,4),D_REF(3,4),RLSKEW(9)
C-----E_REF(4)4N of elem_ref, NBC_B ,IBC_B(NBC_B): node_id w/ BC;
C------IE_BC(NE_BC) Elem_id which has the BC nodes 
C-------N_SEG=3 3N seg, 0 : no ref;  ----   

      END MODULE IMP_SPBRM
C
      !||====================================================================
      !||    imp_frk        ../engine/share/modules/impbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    dim_fr_k       ../engine/source/mpi/implicit/imp_fri.F
      !||    dim_nrmax      ../engine/source/mpi/implicit/imp_fri.F
      !||    fr_dlft        ../engine/source/mpi/implicit/imp_fri.F
      !||    get_ikin2g     ../engine/source/mpi/implicit/imp_fri.F
      !||    getnddli_g     ../engine/source/mpi/implicit/imp_fri.F
      !||    imp_pcgh       ../engine/source/implicit/imp_pcg.F
      !||    ind_fr_k       ../engine/source/mpi/implicit/imp_fri.F
      !||    ind_fr_k0      ../engine/source/mpi/implicit/imp_fri.F
      !||    ind_kine_kp    ../engine/source/mpi/implicit/imp_fri.F
      !||    ind_nrfr       ../engine/source/mpi/implicit/imp_fri.F
      !||    ini_fr_k       ../engine/source/mpi/implicit/imp_fri.F
      !||    kin_nrmax      ../engine/source/mpi/implicit/imp_fri.F
      !||    kin_nrmax0     ../engine/source/mpi/implicit/imp_fri.F
      !||    nddli_frb      ../engine/source/mpi/implicit/imp_fri.F
      !||    pr_deb         ../engine/source/implicit/imp_solv.F
      !||    pr_matrix      ../engine/source/implicit/imp_solv.F
      !||    pr_solnfo      ../engine/source/implicit/imp_solv.F
      !||    set_ikin2g     ../engine/source/mpi/implicit/imp_fri.F
      !||    spc_fr_k       ../engine/source/mpi/implicit/imp_fri.F
      !||    spmd_inf_g     ../engine/source/mpi/implicit/imp_spmd.F
      !||    spmd_max_iv    ../engine/source/mpi/implicit/imp_spmd.F
      !||    spmd_sumf_k    ../engine/source/mpi/implicit/imp_spmd.F
      !||    spmd_sumf_v    ../engine/source/mpi/implicit/imp_spmd.F
      !||    spmd_sumfc_v   ../engine/source/mpi/implicit/imp_spmd.F
      !||    upd_fr_k       ../engine/source/mpi/implicit/imp_fri.F
      !||    zero_ikin2g    ../engine/source/mpi/implicit/imp_fri.F
      !||====================================================================
      MODULE IMP_FRK
C
      INTEGER NRMAX,N_FRNN,NDDLFR,NZKFR,NDFRMAX,LEN_V,LEN_K
C     NDDLFRB :nb de noeuds fr. partages par au moins 3 proc,----
C  50d1m     DDLP0 :dernier ddl de noeuds fr.avec procs precedents ----
C  50d1m     DDLP1 :premier ddl de noeuds fr.avec procs suivants ----
      INTEGER NDDLFRB,NDDLFRB1,DDLP0,DDLP1
C
C tableau de int de : connectivite des noeuds FR. locaux----
      INTEGER, DIMENSION(:), ALLOCATABLE :: IFRLOC
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAD_RL,FR_ICOL
C tableau de int de taille NKINE indice loc->global: ----
      INTEGER, DIMENSION(:), ALLOCATABLE :: IKIN2G
C tableau de int de : matrice locale des noeuds FR. locaux par proc----
      INTEGER, DIMENSION(:), ALLOCATABLE :: ND_FR,IADFR,JDIFR,IDDLFR
#if defined knf
!dec$ attributes offload:mic :: IFR2K
#endif
      INTEGER, DIMENSION(:), ALLOCATABLE :: IFR2K,JFR2K
C 
      END MODULE IMP_FRK
C
