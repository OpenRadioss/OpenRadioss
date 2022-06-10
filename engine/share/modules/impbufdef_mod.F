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
Chd|  IMPBUFDEF_MOD                 share/modules/impbufdef_mod.F 
Chd|-- called by -----------
Chd|        DEALLOC_IMPBUF                source/implicit/imp_solv.F    
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_EIGSOL                    com/eig/imp_eigsol.F          
Chd|        IMP_FOUT                      source/implicit/imp_solv.F    
Chd|        IMP_RESTARCP                  source/implicit/imp_sol_init.F
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        IMP_SOL_INIT                  source/implicit/imp_sol_init.F
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
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
Chd|====================================================================
Chd|  IMP_KTAN_DEF                  share/modules/impbufdef_mod.F 
Chd|-- called by -----------
Chd|        CMATCH3                       source/elements/shell/coqueba/cmatc3.F
Chd|        CMATIP3                       source/elements/shell/coqueba/cmatc3.F
Chd|        ETFAC_INI                     source/implicit/imp_init.F    
Chd|        GETHKT3                       source/elements/solid/solide8z/mmats.F
Chd|        GET_ETFAC_S                   source/elements/solid/solide8z/mmats.F
Chd|        IKTMAT_INI                    source/implicit/imp_init.F    
Chd|        KTBUF_INI                     source/implicit/imp_init.F    
Chd|        MMATS                         source/elements/solid/solide8z/mmats.F
Chd|        NUL_ETFAC_S                   source/elements/solid/solide8z/mmats.F
Chd|        PUTSIGNOR3                    source/elements/solid/solide8z/mmats.F
Chd|        PUTSIGNORC3                   source/elements/shell/coqueba/cmatc3.F
Chd|        PUT_ETFAC                     source/elements/solid/solide8z/mmats.F
Chd|        SKTCONS2                      source/elements/solid/solide8z/mmats.F
Chd|        IMP_KTAN                      share/modules/impbufdef_mod.F 
Chd|-- calls ---------------
Chd|====================================================================
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
C------the following interger arguments are not necessary for the moment
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
Chd|====================================================================
Chd|  IMP_BFGS                      share/modules/impbufdef_mod.F 
Chd|-- called by -----------
Chd|        BFGS_0                        source/implicit/imp_bfgs.F    
Chd|        BFGS_1                        source/implicit/imp_bfgs.F    
Chd|        BFGS_1P                       source/implicit/imp_bfgs.F    
Chd|        BFGS_2                        source/implicit/imp_bfgs.F    
Chd|        BFGS_2P                       source/implicit/imp_bfgs.F    
Chd|        BFGS_H1                       source/implicit/imp_bfgs.F    
Chd|        BFGS_H1P                      source/implicit/imp_bfgs.F    
Chd|        BFGS_H2                       source/implicit/imp_bfgs.F    
Chd|        BFGS_H2P                      source/implicit/imp_bfgs.F    
Chd|        BFGS_INI                      source/implicit/imp_bfgs.F    
Chd|        BFGS_LS                       source/implicit/imp_bfgs.F    
Chd|        DEALLOCM_IMP                  source/implicit/imp_solv.F    
Chd|        GET_SLIN                      source/implicit/imp_bfgs.F    
Chd|        NSLOAN_0                      source/implicit/imp_bfgs.F    
Chd|        NSLOAN_5                      source/implicit/imp_bfgs.F    
Chd|-- calls ---------------
Chd|====================================================================
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
Chd|====================================================================
Chd|  IMP_DYNA                      share/modules/impbufdef_mod.F 
Chd|-- called by -----------
Chd|        DEALLOCM_IMP                  source/implicit/imp_solv.F    
Chd|        DU_INI                        source/implicit/imp_solv.F    
Chd|        DU_INI_HP                     source/implicit/imp_solv.F    
Chd|        DYNA_CPK0                     source/implicit/imp_dyna.F    
Chd|        DYNA_CPR0                     source/implicit/imp_dyna.F    
Chd|        DYNA_IN0                      source/implicit/imp_dyna.F    
Chd|        DYNA_INA                      source/implicit/imp_dyna.F    
Chd|        DYNA_INI                      source/implicit/imp_dyna.F    
Chd|        DYNA_INIV                     source/implicit/imp_dyna.F    
Chd|        DYNA_IVFAC                    source/implicit/imp_dyna.F    
Chd|        DYNA_WEX                      source/implicit/imp_dyna.F    
Chd|        ECRIT                         source/output/ecrit.F         
Chd|        FREIMPL                       source/input/freimpl.F        
Chd|        FV_FINT0                      source/constraints/general/impvel/fv_imp0.F
Chd|        GETDYNA_A                     source/implicit/imp_dyna.F    
Chd|        IMPRREST                      source/output/restart/rdresb.F
Chd|        IMPWREST                      source/output/restart/wrrest.F
Chd|        IMP_DYCRB                     source/implicit/imp_dyna.F    
Chd|        IMP_DYKV                      source/implicit/imp_dyna.F    
Chd|        IMP_DYKV0                     source/implicit/imp_dyna.F    
Chd|        IMP_DYNAM                     source/implicit/imp_dyna.F    
Chd|        IMP_DYNAR                     source/implicit/imp_dyna.F    
Chd|        IMP_FHHT                      source/implicit/imp_dyna.F    
Chd|        IMP_FHHT1                     source/implicit/imp_dyna.F    
Chd|        INTE_DYNA                     source/implicit/imp_dyna.F    
Chd|        RBE2COR                       source/constraints/general/rbody/rgbcor.F
Chd|        RBE2COR0                      source/constraints/general/rbody/rgbcor.F
Chd|        RBYCOR                        source/constraints/general/rbody/rbycor.F
Chd|        RGBCOR                        source/constraints/general/rbody/rgbcor.F
Chd|        WFV_IMP                       source/constraints/general/impvel/fv_imp0.F
Chd|-- calls ---------------
Chd|====================================================================
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
Chd|====================================================================
Chd|  IMP_MONV                      share/modules/impbufdef_mod.F 
Chd|-- called by -----------
Chd|        ID_MVINI                      source/airbag/monv_imp0.F     
Chd|        MONV_DIAG                     source/airbag/monv_imp0.F     
Chd|        MONV_FVL                      source/airbag/monv_imp0.F     
Chd|        MONV_M3                       source/airbag/monv_imp0.F     
Chd|        MONV_PREM                     source/airbag/monv_imp0.F     
Chd|        MV_MATV                       source/airbag/monv_imp0.F     
Chd|        RECU_KDIS                     source/airbag/monv_imp0.F     
Chd|-- calls ---------------
Chd|====================================================================
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
Chd|====================================================================
Chd|  IMP_RWL                       share/modules/impbufdef_mod.F 
Chd|-- called by -----------
Chd|        DIAG_INT                      source/mpi/implicit/imp_fri.F 
Chd|        DIM_KINKN                     source/implicit/imp_int_k.F   
Chd|        FR_U2DD                       source/mpi/implicit/imp_fri.F 
Chd|        FV_RWL                        source/constraints/general/rwall/srw_imp.F
Chd|        FV_RWL0                       source/constraints/general/rwall/srw_imp.F
Chd|        FV_RWLR0                      source/constraints/general/rwall/srw_imp.F
Chd|        IMP3_A2B                      source/airbag/monv_imp0.F     
Chd|        IMP3_U2X                      source/airbag/monv_imp0.F     
Chd|        IND_KINEFR                    source/mpi/implicit/imp_fri.F 
Chd|        IND_KINFRK                    source/mpi/implicit/imp_fri.F 
Chd|        INI_KINKN                     source/implicit/imp_int_k.F   
Chd|        MONV_FVL                      source/airbag/monv_imp0.F     
Chd|        RWL_IMPD                      source/constraints/general/rwall/srw_imp.F
Chd|        UPDK_MV                       source/airbag/monv_imp0.F     
Chd|        UPD_FR                        source/mpi/implicit/imp_fri.F 
Chd|        UPD_KML                       source/mpi/implicit/imp_fri.F 
Chd|        UPD_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      MODULE IMP_RWL
#include "my_real.inc"
      INTEGER N_RWL
C tableau de int de taille N_RWL :noeuds secnds de sliding RW 
      INTEGER, DIMENSION(:), ALLOCATABLE :: IN_RWL
C tableau de Direction normale reels de taille (3,N_RWL) 
      my_real, DIMENSION(:,:),ALLOCATABLE :: NOR_RWL
C      
      END MODULE IMP_RWL
Chd|====================================================================
Chd|  IMP_LINTF                     share/modules/impbufdef_mod.F 
Chd|-- called by -----------
Chd|        DIAG_KIF                      source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        INI_KIF                       source/implicit/imp_solv.F    
Chd|        MATV_KIF                      source/implicit/imp_solv.F    
Chd|        SAVE_KIF                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      MODULE IMP_LINTF
#include "my_real.inc"
      INTEGER NDDLIF,NZIF
C tableau de int de la [k] interface 
      INTEGER, DIMENSION(:), ALLOCATABLE :: IADIF,JDIIF,IFTOK
C tableau de reels de la [k] interface 
      my_real, DIMENSION(:),ALLOCATABLE :: DIAG_IF,LT_IF
C      
      END MODULE IMP_LINTF
Chd|====================================================================
Chd|  IMP_REST                      share/modules/impbufdef_mod.F 
Chd|-- called by -----------
Chd|        IMPRREST                      source/output/restart/rdresb.F
Chd|        IMPWREST                      source/output/restart/wrrest.F
Chd|        IMP_TRANS                     source/output/restart/wrrest.F
Chd|        IMP_TRANS0                    source/output/restart/wrrest.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE IMP_REST
#include "my_real.inc"
C tableau de reels de R-file 
      my_real, DIMENSION(:),ALLOCATABLE :: IMP_RR
C      
      END MODULE IMP_REST
C
Chd|====================================================================
Chd|  IMP_PPAT                      share/modules/impbufdef_mod.F 
Chd|-- called by -----------
Chd|        DIM_SPAN                      source/implicit/ind_glob_k.F  
Chd|        FIL_SPAN0                     source/implicit/ind_glob_k.F  
Chd|        FIL_SPAN1                     source/implicit/ind_glob_k.F  
Chd|        IND_SPAN                      source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      MODULE IMP_PPAT
C tableau de int de pre-filtrage 
      INTEGER, DIMENSION(:),ALLOCATABLE :: PRE_FPAT
C      
      END MODULE IMP_PPAT
Chd|====================================================================
Chd|  IMP_QSTAT                     share/modules/impbufdef_mod.F 
Chd|-- called by -----------
Chd|        DEALLOCM_IMP                  source/implicit/imp_solv.F    
Chd|        DIS_CP                        source/implicit/imp_solv.F    
Chd|        QSTAT_END                     source/implicit/imp_dyna.F    
Chd|        QSTAT_INI                     source/implicit/imp_dyna.F    
Chd|        QSTAT_IT                      source/implicit/imp_dyna.F    
Chd|-- calls ---------------
Chd|====================================================================
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
Chd|====================================================================
Chd|  IMP_KBCS                      share/modules/impbufdef_mod.F 
Chd|-- called by -----------
Chd|        DEALLOCM_IMP                  source/implicit/imp_solv.F    
Chd|        FREIMPL                       source/input/freimpl.F        
Chd|        IMP_INIT                      source/implicit/imp_init.F    
Chd|        IMP_MUMPS1                    source/implicit/imp_mumps.F   
Chd|        LECIMPL                       source/input/lectur.F         
Chd|        PROUT_BUCK                    source/input/lectur.F         
Chd|-- calls ---------------
Chd|====================================================================
      MODULE IMP_KBCS
C tableau de travail de BCS-EXT 
      INTEGER  MEM_BCS
      my_real, DIMENSION(:),ALLOCATABLE :: HOLD
      INTEGER  MSGL_B,MAXSET_B,MSG_LVL,B_ORDER,B_MCORE,LMEMN
      my_real  EMIN_B,EMAX_B,SHIFT_B
C      
      END MODULE IMP_KBCS
Chd|====================================================================
Chd|  IMP_KNON                      share/modules/impbufdef_mod.F 
Chd|-- called by -----------
Chd|        DEALLOCM                      source/implicit/imp_solv.F    
Chd|        INT_MATV                      source/implicit/imp_int_k.F   
Chd|        INT_MATVP                     source/implicit/imp_int_k.F   
Chd|        KIN_KNL                       source/implicit/imp_int_k.F   
Chd|        RECU_KDIS0                    source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
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
Chd|====================================================================
Chd|  IMP_ASPC                      share/modules/impbufdef_mod.F 
Chd|-- called by -----------
Chd|        BC_IMP2                       source/constraints/general/bcs/bc_imp0.F
Chd|        BC_IMPA                       source/constraints/general/bcs/bc_imp0.F
Chd|        BC_IMPR1                      source/constraints/general/bcs/bc_imp0.F
Chd|        DIAG_INT                      source/mpi/implicit/imp_fri.F 
Chd|        DIM_KINKN                     source/implicit/imp_int_k.F   
Chd|        FR_U2DD                       source/mpi/implicit/imp_fri.F 
Chd|        GET_NSPC                      source/constraints/general/bcs/bc_imp0.F
Chd|        IMP3_A2B                      source/airbag/monv_imp0.F     
Chd|        IMP3_U2X                      source/airbag/monv_imp0.F     
Chd|        IMP_INIT                      source/implicit/imp_init.F    
Chd|        IND_KINEFR                    source/mpi/implicit/imp_fri.F 
Chd|        IND_KINFRK                    source/mpi/implicit/imp_fri.F 
Chd|        INI_KINKN                     source/implicit/imp_int_k.F   
Chd|        KIN_KNL                       source/implicit/imp_int_k.F   
Chd|        MONV_FVL                      source/airbag/monv_imp0.F     
Chd|        PUT_NSPC                      source/constraints/general/bcs/bc_imp0.F
Chd|        UPDK_MV                       source/airbag/monv_imp0.F     
Chd|        UPD_ASPC0                     source/constraints/general/bcs/bc_imp0.F
Chd|        UPD_FR                        source/mpi/implicit/imp_fri.F 
Chd|        UPD_KML                       source/mpi/implicit/imp_fri.F 
Chd|        UPD_KSL                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      MODULE IMP_ASPC
      INTEGER NSPCL,NSPCNT
#include "my_real.inc"
      INTEGER, DIMENSION(:),ALLOCATABLE :: IN_SPC,IC_SPC
      my_real, DIMENSION(:), ALLOCATABLE :: SKEW_SPC
C      
      END MODULE IMP_ASPC
C
Chd|====================================================================
Chd|  IMP_WORKI                     share/modules/impbufdef_mod.F 
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      MODULE IMP_WORKI
#include      "my_real.inc"
      INTEGER, DIMENSION(:),ALLOCATABLE :: IADI,JDII,ITOK
      INTEGER  NDDLI ,NNZI,ICONTA,NSREM ,NSL
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: DIAG_I,LT_I,XI_C
 
      END MODULE IMP_WORKI
C
Chd|====================================================================
Chd|  IMP_WORKH                     share/modules/impbufdef_mod.F 
Chd|-- called by -----------
Chd|        DEALLOCM_IMP                  source/implicit/imp_solv.F    
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|        IMP_PPCGH                     source/implicit/imp_pcg.F     
Chd|        INI_K0H                       source/implicit/imp_solv.F    
Chd|        INI_KISC                      source/implicit/lin_solv.F    
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|        LIN_SOLVH0                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVH1                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVHM                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVIH2                   source/implicit/lin_solv.F    
Chd|        MAV_LIUH                      source/implicit/produt_v.F    
Chd|        MAV_LUI_H                     source/implicit/produt_v.F    
Chd|        MAV_LU_H                      source/implicit/produt_v.F    
Chd|        MMAV_LTH                      source/implicit/produt_v.F    
Chd|        PREC5HC_SOLV                  source/implicit/prec_solv.F   
Chd|        PRODUT_UH                     source/implicit/produt_v.F    
Chd|        PRODUT_UH2                    source/implicit/produt_v.F    
Chd|        PRODUT_VMH                    source/implicit/produt_v.F    
Chd|-- calls ---------------
Chd|====================================================================
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
Chd|====================================================================
Chd|  IMP_WORKG                     share/modules/impbufdef_mod.F 
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|-- calls ---------------
Chd|====================================================================
      MODULE IMP_WORKG
#include      "my_real.inc"
      INTEGER  IPRMES_EL(30)
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: DIAG_KG,LT_KG
 
      END MODULE IMP_WORKG
C
Chd|====================================================================
Chd|  IMP_FVBCL                     share/modules/impbufdef_mod.F 
Chd|-- called by -----------
Chd|        BC_IMPR1                      source/constraints/general/bcs/bc_imp0.F
Chd|        FVBC_ALLO                     source/constraints/general/impvel/fv_imp0.F
Chd|        FVBC_DEALLO                   source/constraints/general/impvel/fv_imp0.F
Chd|        FV_IMP1                       source/constraints/general/impvel/fv_imp0.F
Chd|        RECUKIN                       source/implicit/recudis.F     
Chd|        RECU_UL                       source/constraints/general/impvel/fv_imp0.F
Chd|        UPD_GLOB_K                    source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      MODULE IMP_FVBCL
#include      "my_real.inc"
      INTEGER  NFVBCL,NKUD_L,NKUD_1
      INTEGER, DIMENSION(:),ALLOCATABLE :: ICT_1,ICR_1,IKUD_1
      my_real,
     .         DIMENSION(:),ALLOCATABLE :: BKUD_1,FVBCUDL
 
      END MODULE IMP_FVBCL
C
Chd|====================================================================
Chd|  IMP_PCG_PROJ                  share/modules/impbufdef_mod.F 
Chd|-- called by -----------
Chd|        DEALLOCM_IMP                  source/implicit/imp_solv.F    
Chd|        FREIMPL                       source/input/freimpl.F        
Chd|        IMP_INISI                     source/implicit/imp_pcg.F     
Chd|        IMP_INIST                     source/implicit/imp_pcg.F     
Chd|        IMP_INIX                      source/implicit/imp_pcg.F     
Chd|        IMP_PPCGH                     source/implicit/imp_pcg.F     
Chd|        IMP_PRO_P                     source/implicit/imp_pcg.F     
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        IMP_UPDST                     source/implicit/imp_pcg.F     
Chd|        IMP_UPDV2                     source/implicit/imp_pcg.F     
Chd|        INI_K0H                       source/implicit/imp_solv.F    
Chd|        LECIMPL                       source/input/lectur.F         
Chd|-- calls ---------------
Chd|====================================================================
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
Chd|====================================================================
Chd|  IMP_KTAN                      share/modules/impbufdef_mod.F 
Chd|-- called by -----------
Chd|        CMATCH3                       source/elements/shell/coqueba/cmatc3.F
Chd|        CMATIP3                       source/elements/shell/coqueba/cmatc3.F
Chd|        ETFAC_INI                     source/implicit/imp_init.F    
Chd|        GETHKT3                       source/elements/solid/solide8z/mmats.F
Chd|        GET_ETFAC_S                   source/elements/solid/solide8z/mmats.F
Chd|        IKTMAT_INI                    source/implicit/imp_init.F    
Chd|        KTBUF_INI                     source/implicit/imp_init.F    
Chd|        MMATS                         source/elements/solid/solide8z/mmats.F
Chd|        NUL_ETFAC_S                   source/elements/solid/solide8z/mmats.F
Chd|        PUTSIGNOR3                    source/elements/solid/solide8z/mmats.F
Chd|        PUTSIGNORC3                   source/elements/shell/coqueba/cmatc3.F
Chd|        PUT_ETFAC                     source/elements/solid/solide8z/mmats.F
Chd|        SKTCONS2                      source/elements/solid/solide8z/mmats.F
Chd|-- calls ---------------
Chd|        IMP_KTAN_DEF                  share/modules/impbufdef_mod.F 
Chd|====================================================================
      MODULE IMP_KTAN

      USE IMP_KTAN_DEF

      TYPE(KTBUF_STRUCT_),TARGET,DIMENSION(:),ALLOCATABLE :: KTBUF_STR
      END MODULE IMP_KTAN
Chd|====================================================================
Chd|  IMP_SPBRM                     share/modules/impbufdef_mod.F 
Chd|-- called by -----------
Chd|        DEALLOCM_IMP                  source/implicit/imp_solv.F    
Chd|        FREIMPL                       source/input/freimpl.F        
Chd|        LECIMPL                       source/input/lectur.F         
Chd|        SPBRM_PRE                     source/implicit/imp_solv.F    
Chd|        SPB_IEREF3                    source/implicit/imp_solv.F    
Chd|        SPB_IEREF_BC                  source/implicit/imp_solv.F    
Chd|        SPB_REF_NDS                   source/implicit/imp_solv.F    
Chd|        SPB_RM_RIG                    source/implicit/imp_solv.F    
Chd|        SPMD_E_REF                    source/mpi/implicit/imp_spmd.F
Chd|        SPMD_N_REF                    source/mpi/implicit/imp_spmd.F
Chd|-- calls ---------------
Chd|====================================================================
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
Chd|====================================================================
Chd|  IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|-- called by -----------
Chd|        DIM_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        DIM_NRMAX                     source/mpi/implicit/imp_fri.F 
Chd|        FR_DLFT                       source/mpi/implicit/imp_fri.F 
Chd|        GETNDDLI_G                    source/mpi/implicit/imp_fri.F 
Chd|        GET_IKIN2G                    source/mpi/implicit/imp_fri.F 
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|        IND_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        IND_FR_K0                     source/mpi/implicit/imp_fri.F 
Chd|        IND_KINE_KP                   source/mpi/implicit/imp_fri.F 
Chd|        IND_NRFR                      source/mpi/implicit/imp_fri.F 
Chd|        INI_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        KIN_NRMAX                     source/mpi/implicit/imp_fri.F 
Chd|        KIN_NRMAX0                    source/mpi/implicit/imp_fri.F 
Chd|        NDDLI_FRB                     source/mpi/implicit/imp_fri.F 
Chd|        PR_DEB                        source/implicit/imp_solv.F    
Chd|        PR_MATRIX                     source/implicit/imp_solv.F    
Chd|        PR_SOLNFO                     source/implicit/imp_solv.F    
Chd|        SET_IKIN2G                    source/mpi/implicit/imp_fri.F 
Chd|        SPC_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        SPMD_INF_G                    source/mpi/implicit/imp_spmd.F
Chd|        SPMD_MAX_IV                   source/mpi/implicit/imp_spmd.F
Chd|        SPMD_SUMFC_V                  source/mpi/implicit/imp_spmd.F
Chd|        SPMD_SUMF_K                   source/mpi/implicit/imp_spmd.F
Chd|        SPMD_SUMF_V                   source/mpi/implicit/imp_spmd.F
Chd|        UPD_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        ZERO_IKIN2G                   source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      MODULE IMP_FRK
C
      INTEGER NRMAX,N_FRNN,NDDLFR,NZKFR,NDFRMAX,LEN_V,LEN_K
C     NDDLFRB :nb de noeuds fr. partages par au moins 3 proc,----
Cqz50d1m     DDLP0 :dernier ddl de noeuds fr.avec procs precedents ----
Cqz50d1m     DDLP1 :premier ddl de noeuds fr.avec procs suivants ----
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
