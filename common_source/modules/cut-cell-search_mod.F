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
Chd|  I22BUFBRIC_MOD                modules/cut-cell-search_mod.F 
Chd|-- called by -----------
Chd|        W_BUFBRIC_22                  starter/source/interfaces/inter3d1/w_bufbric_22.F
Chd|        A22CONV3                      engine/source/ale/alefvm/cut_cells/a22conv3.F
Chd|        ACONVE                        engine/source/ale/aconve.F    
Chd|        AFLUX3                        engine/source/ale/ale3d/aflux3.F
Chd|        AFLUX3_INT22_FVM              engine/source/ale/alefvm/cut_cells/aflux3_int22_fvm.F
Chd|        AFLUXT                        engine/source/ale/ale51/afluxt.F
Chd|        ALE51_ANTIDIFF3               engine/source/ale/ale51/ale51_antidiff3.F
Chd|        ALE51_ANTIDIFF3_INT22         engine/source/ale/alefvm/cut_cells/ale51_antidiff3_int22.F
Chd|        ALE51_FINISH                  engine/source/ale/ale51/ale51_finish.F
Chd|        ALE51_INIT                    engine/source/ale/ale51/ale51_init.F
Chd|        ALE51_UPWIND3_INT22           engine/source/ale/alefvm/cut_cells/ale51_upwind3_int22.F
Chd|        ALEFVM_FRESET                 engine/source/ale/alefvm/alefvm_freset.F
Chd|        ALEFVM_GRAVITY_INT22          engine/source/ale/alefvm/alefvm_gravity_int22.F
Chd|        ALEFVM_SCHEME                 engine/source/ale/alefvm/alefvm_scheme.F
Chd|        ALEFVM_SFINT3                 engine/source/ale/alefvm/alefvm_sfint3.F
Chd|        ALEFVM_SFINT3_INT22           engine/source/ale/alefvm/alefvm_sfint3_int22.F
Chd|        ALEFVM_STRESS_INT22           engine/source/ale/alefvm/alefvm_stress_int22.F
Chd|        ALEMUSCL_UPWIND               engine/source/ale/alemuscl/alemuscl_upwind.F
Chd|        ALEMUSCL_UPWIND2              engine/source/ale/alemuscl/alemuscl_upwind2.F
Chd|        AMASS3                        engine/source/elements/solid/solide/amass3.F
Chd|        AMASS3P                       engine/source/elements/solid/solide/amass3.F
Chd|        DELTAX22                      engine/source/interfaces/int22/deltax22.F
Chd|        DESTROY_CELL                  engine/source/interfaces/int22/destroy_cell.F
Chd|        EFLUX3_INT22_FVM              engine/source/ale/alefvm/cut_cells/eflux3_int22_fvm.F
Chd|        H3D_VELVECC22                 engine/source/output/h3d/h3d_results/h3d_velvecc22.F
Chd|        H3D_VELVECZ22                 engine/source/output/h3d/h3d_results/h3d_velvecz22.F
Chd|        I22ASS2                       engine/source/interfaces/int22/i22assembly.F
Chd|        I22BUCE                       engine/source/interfaces/intsort/i22buce.F
Chd|        I22COR3                       engine/source/interfaces/int22/i22cor3.F
Chd|        I22DATAINIT                   engine/source/interfaces/int22/i22datainit.F
Chd|        I22DATAINIT_DB                engine/source/interfaces/int22/i22datainit_db.F
Chd|        I22FOR3                       engine/source/interfaces/int22/i22for3.F
Chd|        I22GBIT                       engine/source/interfaces/int22/i22ident.F
Chd|        I22GET_PREV_DATA              engine/source/interfaces/int22/i22get_prev_data.F
Chd|        I22IDENT                      engine/source/interfaces/int22/i22ident.F
Chd|        I22INTERSECT                  engine/source/interfaces/int22/i22intersect.F
Chd|        I22MAIN_TRI                   engine/source/interfaces/intsort/i22main_tri.F
Chd|        I22SOLID_GETMINMAX            engine/source/interfaces/intsort/i22main_tri.F
Chd|        I22SUBVOL                     engine/source/interfaces/int22/i22subvol.F
Chd|        I22TRIVOX                     engine/source/interfaces/intsort/i22trivox.F
Chd|        I22WETSURF                    engine/source/interfaces/int22/i22wetsurf.F
Chd|        INTTRI                        engine/source/interfaces/intsort/inttri.F
Chd|        MQVISCB                       engine/source/materials/mat_share/mqviscb.F
Chd|        NODALD                        engine/source/output/anim/generate/nodald.F
Chd|        NODALP                        engine/source/output/anim/generate/nodalp.F
Chd|        NODALT                        engine/source/output/anim/generate/nodalt.F
Chd|        NODALVFRAC                    engine/source/output/anim/generate/nodalvfrac.F
Chd|        NODALZVOL                     engine/source/output/anim/generate/nodalzvol.F
Chd|        NODAL_SCHLIEREN               engine/source/output/anim/generate/nodal_schlieren.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        R_BUFBRIC_22                  engine/source/interfaces/int22/r_bufbric_22.F
Chd|        SCOOR3_FVM                    engine/source/ale/alefvm/scoor3_fvm.F
Chd|        SIGEPS105                     engine/source/materials/mat/mat105/sigeps105.F
Chd|        SIGEPS51                      engine/source/materials/mat/mat051/sigeps51.F
Chd|        SIGEPS97                      engine/source/materials/mat/mat097/sigeps97.F
Chd|        SINIT22_FVM                   engine/source/interfaces/int22/sinit22_fvm.F
Chd|        SRHO3                         engine/source/elements/solid/solide/srho3.F
Chd|        VELVECC22                     engine/source/output/anim/generate/velvec.F
Chd|        VELVECZ22                     engine/source/output/anim/generate/velvecz22.F
Chd|        VOLN22                        engine/source/interfaces/int22/voln22.F
Chd|        WRITE_BUF_LAW51               engine/source/materials/mat/mat051/write_buf_law51.F
Chd|        WRITE_CUT_CELL_BUFFER         engine/source/interfaces/int22/write_cut_cell_buffer.F
Chd|        W_BUFBRIC_22                  engine/source/interfaces/int22/w_bufbric_22.F
Chd|        weighting_Cell_Nodes          engine/source/interfaces/int22/weighting_cell_nodes.F
Chd|-- calls ---------------
Chd|        i22EDGE_MOD                   modules/cut-cell-buffer_mod.F 
Chd|====================================================================
      MODULE I22BUFBRIC_MOD
C-----------------------------------------------
C  M o d u l e s
C-----------------------------------------------
      USE i22EDGE_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
      TYPE(BRICK_ENTITY), TARGET,ALLOCATABLE,DIMENSION(:,:) :: 
     .        BRICK_LIST 
      TYPE(EDGE_ENTITY),  TARGET,ALLOCATABLE,DIMENSION(:,:) :: 
     .        EDGE_LIST 
      
      END MODULE I22BUFBRIC_MOD
C
C
C
Chd|====================================================================
Chd|  I22TRI_MOD                    modules/cut-cell-search_mod.F 
Chd|-- called by -----------
Chd|        W_BUFBRIC_22                  starter/source/interfaces/inter3d1/w_bufbric_22.F
Chd|        A22CONV3                      engine/source/ale/alefvm/cut_cells/a22conv3.F
Chd|        ACONV3                        engine/source/ale/ale3d/aconv3.F
Chd|        ACONVE                        engine/source/ale/aconve.F    
Chd|        AFLUX0                        engine/source/ale/aflux0.F    
Chd|        AFLUX3_INT22_FVM              engine/source/ale/alefvm/cut_cells/aflux3_int22_fvm.F
Chd|        AFLUXT                        engine/source/ale/ale51/afluxt.F
Chd|        ALE51_ANTIDIFF3               engine/source/ale/ale51/ale51_antidiff3.F
Chd|        ALE51_ANTIDIFF3_INT22         engine/source/ale/alefvm/cut_cells/ale51_antidiff3_int22.F
Chd|        ALE51_FINISH                  engine/source/ale/ale51/ale51_finish.F
Chd|        ALE51_INIT                    engine/source/ale/ale51/ale51_init.F
Chd|        ALE51_UPWIND3                 engine/source/ale/ale51/ale51_upwind3.F
Chd|        ALE51_UPWIND3_INT22           engine/source/ale/alefvm/cut_cells/ale51_upwind3_int22.F
Chd|        ALEFVM_AFLUX3                 engine/source/ale/alefvm/alefvm_aflux3.F
Chd|        ALEFVM_EFLUX3                 engine/source/ale/alefvm/alefvm_eflux3.F
Chd|        ALEFVM_MAIN                   engine/source/ale/alefvm/alefvm_main.F
Chd|        ALEFVM_SCHEME                 engine/source/ale/alefvm/alefvm_scheme.F
Chd|        ALEFVM_SFINT3                 engine/source/ale/alefvm/alefvm_sfint3.F
Chd|        ALEFVM_SFINT3_INT22           engine/source/ale/alefvm/alefvm_sfint3_int22.F
Chd|        ALEMUSCL_UPWIND               engine/source/ale/alemuscl/alemuscl_upwind.F
Chd|        ALEMUSCL_UPWIND2              engine/source/ale/alemuscl/alemuscl_upwind2.F
Chd|        ALETHE                        engine/source/ale/alethe.F    
Chd|        DELTAX22                      engine/source/interfaces/int22/deltax22.F
Chd|        DESTROY_CELL                  engine/source/interfaces/int22/destroy_cell.F
Chd|        DFUNCS                        engine/source/output/anim/generate/dfunc6.F
Chd|        EFLUX3                        engine/source/ale/euler3d/eflux3.F
Chd|        EFLUX3_INT22_FVM              engine/source/ale/alefvm/cut_cells/eflux3_int22_fvm.F
Chd|        H3D_VELVECC22                 engine/source/output/h3d/h3d_results/h3d_velvecc22.F
Chd|        H3D_VELVECZ22                 engine/source/output/h3d/h3d_results/h3d_velvecz22.F
Chd|        I22ASS2                       engine/source/interfaces/int22/i22assembly.F
Chd|        I22BUCE                       engine/source/interfaces/intsort/i22buce.F
Chd|        I22COR3                       engine/source/interfaces/int22/i22cor3.F
Chd|        I22DATAINIT                   engine/source/interfaces/int22/i22datainit.F
Chd|        I22DATAINIT_DB                engine/source/interfaces/int22/i22datainit_db.F
Chd|        I22FOR3                       engine/source/interfaces/int22/i22for3.F
Chd|        I22GBIT                       engine/source/interfaces/int22/i22ident.F
Chd|        I22GET_PREV_DATA              engine/source/interfaces/int22/i22get_prev_data.F
Chd|        I22IDENT                      engine/source/interfaces/int22/i22ident.F
Chd|        I22INTERSECT                  engine/source/interfaces/int22/i22intersect.F
Chd|        I22MAINF                      engine/source/interfaces/int22/i22mainf.F
Chd|        I22MAIN_TRI                   engine/source/interfaces/intsort/i22main_tri.F
Chd|        I22SHELL_GETMINMAX            engine/source/interfaces/intsort/i22main_tri.F
Chd|        I22SOLID_GETMINMAX            engine/source/interfaces/intsort/i22main_tri.F
Chd|        I22STO                        engine/source/interfaces/intsort/i22sto.F
Chd|        I22SUBVOL                     engine/source/interfaces/int22/i22subvol.F
Chd|        I22TRIVOX                     engine/source/interfaces/intsort/i22trivox.F
Chd|        I22WETSURF                    engine/source/interfaces/int22/i22wetsurf.F
Chd|        INTTRI                        engine/source/interfaces/intsort/inttri.F
Chd|        MQVISCB                       engine/source/materials/mat_share/mqviscb.F
Chd|        NODALD                        engine/source/output/anim/generate/nodald.F
Chd|        NODALP                        engine/source/output/anim/generate/nodalp.F
Chd|        NODALT                        engine/source/output/anim/generate/nodalt.F
Chd|        NODALVFRAC                    engine/source/output/anim/generate/nodalvfrac.F
Chd|        NODALZVOL                     engine/source/output/anim/generate/nodalzvol.F
Chd|        NODAL_SCHLIEREN               engine/source/output/anim/generate/nodal_schlieren.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        R_BUFBRIC_22                  engine/source/interfaces/int22/r_bufbric_22.F
Chd|        SCOOR3_FVM                    engine/source/ale/alefvm/scoor3_fvm.F
Chd|        SIGEPS105                     engine/source/materials/mat/mat105/sigeps105.F
Chd|        SIGEPS51                      engine/source/materials/mat/mat051/sigeps51.F
Chd|        SIGEPS97                      engine/source/materials/mat/mat097/sigeps97.F
Chd|        SINIT22_FVM                   engine/source/interfaces/int22/sinit22_fvm.F
Chd|        SPMD_TRI22VOX                 engine/source/mpi/interfaces/spmd_tri22vox.F
Chd|        VELVECC22                     engine/source/output/anim/generate/velvec.F
Chd|        VELVECZ22                     engine/source/output/anim/generate/velvecz22.F
Chd|        VOLN22                        engine/source/interfaces/int22/voln22.F
Chd|        WRITE_CUT_CELL_BUFFER         engine/source/interfaces/int22/write_cut_cell_buffer.F
Chd|        W_BUFBRIC_22                  engine/source/interfaces/int22/w_bufbric_22.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE I22TRI_MOD
C-----------------------------------------------
C  M o d u l e s
C-----------------------------------------------
      USE I22EDGE_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
#include      "mmale51_c.inc"
C----------------------------------------------- 

      !##########################################!
      !### OUTPUT : GENERAL DEBUG             ###!
      !##########################################! 
      INTEGER :: IBUG22

      !##########################################!
      !### OUTPUT : MAIN SEARCH ALGO          ###!
      !##########################################! 
      INTEGER :: IBUG22_tri
        
      !##########################################!
      !### OUTPUT : VOXEL SEARCH              ###!
      !##########################################! 
      INTEGER :: IBUG22_trivox

      !###########################################!
      !### OUTPUT : INTERSECTIONS CALCULATION  ###!
      !###########################################! 
      INTEGER :: IBUG22_intersect

      !###########################################!
      !### OUTPUT : SUBVOLUME CALCULATION      ###!
      !###########################################! 
      INTEGER :: IBUG22_subvol
      
      !###########################################!
      !### OUTPUT : SUBFACE CALCULATION        ###!
      !###########################################! 
      INTEGER :: IBUG22_aera
 
      !###########################################!
      !### OUTPUT : SUBVOLUMES IDENTIFICATION   ##!
      !###########################################! 
      INTEGER :: IBUG22_ident    
            
      !###########################################!
      !### OUTPUT : CONVECTIVES FLUXES          ##!
      !###########################################! 
      INTEGER :: IBUG22_convec

      !###########################################!
      !### OUTPUT : EOS                         ##!
      !###########################################!
      INTEGER :: IBUG22_eos
      
      !###########################################!
      !### OUTPUT : DVOL                        ##!
      !###########################################!
      INTEGER :: IBUG22_dvol      
      
      !###########################################!
      !### OUTPUT : SIGEPS51                    ##!
      !###########################################!
      INTEGER :: IBUG22_sigeps51     

      !###########################################!
      !### OUTPUT : SIGEPS37                    ##!
      !###########################################!
      INTEGER :: IBUG22_sigeps37    
            
      !###########################################!
      !### OUTPUT : SUBVOLUME SOUND SPEEDS      ##!
      !###########################################!
      INTEGER :: IBUG22_ssp   
      
      !###########################################!
      !### OUTPUT : FLUXES                      ##!
      !###########################################!
      INTEGER :: IBUG22_flux
      
      !###########################################!
      !### OUTPUT : INTER22 ADDITIONAL FLUXES   ##!
      !###########################################!
      INTEGER :: IBUG22_flux22
      
      !###########################################!
      !### OUTPUT : VOLUMETRIC FRACTIONS AFLUXT ##!
      !###########################################!
      INTEGER :: IBUG22_volfrac     
      
      !###########################################!
      !### OUTPUT : CONTINUITY CONDITIONS       ##!
      !###########################################!
      INTEGER :: IBUG22_conti  
      
      !###########################################!
      !### OUTPUT : INTERNAL FORCES             ##!
      !###########################################!
      INTEGER :: IBUG22_fint    
      
      !###########################################!
      !### OUTPUT : SININT                      ##!
      !###########################################!
      INTEGER :: IBUG22_sinit                            

      !###########################################!
      !### OUTPUT : NODALP                      ##!
      !###########################################!
      INTEGER :: IBUG22_nodalp  
      
      !###########################################!
      !### OUTPUT : AMOUIL                      ##!
      !###########################################!
      INTEGER :: IBUG22_amouil                                          

      !###########################################!
      !### OUTPUT : UPWIND/DOWNWIND TRANSPORT   ##!
      !###########################################!
      INTEGER :: IBUG22_spe_convec

      !###########################################!
      !### OUTPUT : CONTACT FORCES              ##!
      !###########################################!
      INTEGER :: IBUG22_fcont    
      INTEGER :: IBUG22_fcontASS      

      !###########################################!
      !### OUTPUT : WET SURFACE                 ##!
      !###########################################!
      INTEGER :: IBUG22_Swet

      !###########################################!
      !### OUTPUT : VELOCITY INTERPOLATION      ##!
      !###########################################!
      INTEGER :: IBUG22_destroy                  
      
      !###########################################!
      !### OUTPUT : MERGE/DEMERGE INFO          ##!
      !###########################################!
      INTEGER :: IBUG22_merge   
      
      !###########################################!
      !### OUTPUT : MERGE/DEMERGE INFO          ##!
      !###########################################!
      INTEGER :: IBUG22_UndirectLink    
      
      !###########################################!
      !### OUTPUT : HM tcl for Cut Cell Buffer  ##!
      !###########################################!
      INTEGER :: IBUG22_OUTP_CCbuffer      

      !###########################################!
      !### OUTPUT : HM tcl for Intersec. Points ##!
      !###########################################!
      INTEGER :: IBUG22_OUTP_IntPoint              
      
      !###########################################!
      !### OUTPUT : HM tcl for Intersec. Points ##!
      !###########################################!
      INTEGER :: IBUG22_TRUSS    
      INTEGER :: IBUG22_OrphanNodes
      
      !###########################################!
      !### OUTPUT : Velocity for CFL            ##!
      !###########################################!
      INTEGER :: IBUG22_VD2

      !###########################################!
      !### OUTPUT : Cinematic Time Step         ##!
      !###########################################!
      INTEGER :: IBUG22_DTMIN

      !###########################################!
      !### OUTPUT : output on NVAR value        ##!
      !###########################################!
      INTEGER :: IBUG22_NVAR      
      
      !###########################################!
      !### OUTPUT : output on NVAR value        ##!
      !###########################################!
      INTEGER :: IBUG22_ITRIMAT      

      !###########################################!
      !### OUTPUT : LAW51 ANTIDIFFUSIVE ALGO    ##!
      !###########################################!
      INTEGER :: IBUG22_ANTIDIFF     

      !###########################################!
      !### OUTPUT : LAW51 ANTIDIFFUSIVE ALGO    ##!
      !###########################################!
      INTEGER :: IBUG22_UPWIND    
      
      !###########################################!
      !### OUTPUT : LINK SWITCHING              ##!
      !###########################################!
      INTEGER :: IBUG22_LINK_SWITCH         
      
      !###########################################!
      !### OUTPUT : TOPOLOGICAL TRACKING        ##!
      !###########################################!
      INTEGER :: IBUG22_TRACKING

      !###########################################!
      !### OUTPUT : PREDICTION/CORRECTION ALGO  ##!
      !###########################################!
      INTEGER :: IBUG22_PREDICTION    
      
      !###########################################!
      !### OUTPUT : MATERIAL BUFFER FILLING     ##!
      !###########################################!
      INTEGER :: IBUG22_FILLMAT    
            
      !###########################################!
      !### OUTPUT : CUT CELL BUFFER LIST        ##!
      !###########################################!
      INTEGER :: IBUG22_CCBUFLIST  

      !###########################################!
      !### OUTPUT : CUT CELL BUFFER LENGTH      ##!
      !###########################################!
      INTEGER :: IBUG22_CC_LENGTH                        
C-----------------------------------------------      
      
      
      INTEGER ::
     .        MIN_IX, MIN_IY, MIN_IZ,            !indice voxel min utilise
     .        MAX_IX, MAX_IY, MAX_IZ             !indice voxel max utilise

      INTEGER ::
     .        CURRENT_ADD,
     .        NRTM_T_MAX ! (nombre maximum des valeurs locales)

      my_real, ALLOCATABLE ::
     .        IRECT_L(:,:)    !IRECT_L <=> IRECT + XREM  (T=thread, L=Localdomain)
      INTEGER ::
     .        NIRECT_L 
      INTEGER :: SIZ_IRECT_L
      PARAMETER (SIZ_IRECT_L = 26)  !longueur buffer (nodes, coordinates, min, max, stif...)

      INTEGER, ALLOCATABLE, DIMENSION(:)  ::
     .        NSNR_G    

      my_real, ALLOCATABLE,DIMENSION(:) ::
     .        XMAXS, XMINS,
     .        YMAXS, YMINS,
     .        ZMAXS, ZMINS

      my_real, ALLOCATABLE,DIMENSION(:,:) ::
     .        BMINMA_LAG_SPMD
      my_real ::
     .        BMINMA_LAG_G(6), ! lagrangian bounds for global domain (for int22)
     .        BMINMA_LAG_R(6)  ! lagrangian remotes domain (for int22)

      my_real, ALLOCATABLE,DIMENSION(:) ::
     .        XMAXE, XMINE,
     .        YMAXE, YMINE,
     .        ZMAXE, ZMINE

      INTEGER :: SIZ_XREM
      PARAMETER (SIZ_XREM = 26)

      INTEGER, POINTER, DIMENSION(:) :: 
     .        LCHAIN_ELEM, LCHAIN_NEXT, LCHAIN_LAST 
 
      INTEGER,  DIMENSION(:,:),ALLOCATABLE :: 
     .        N1_TAB, ADD_EDGE, ADD_FACE  
    
      INTEGER :: 
     .        IMIN, IMAX

      INTEGER,  DIMENSION(:),ALLOCATABLE :: 
     .        UBOUND, LBOUND,
     .        ICANDB                             !ICANDB:(numero de la brique candidate pour l'acces dans BRICK_LIST)   IGROUP->ICANDB         
!     .        MMTAG                
      INTEGER ::
     .        NB                                 !NB: nombre de briques candidates
     
      INTEGER, ALLOCATABLE, DIMENSION(:) ::
     .         EIX1,EIY1,
     .         EIZ1,EIX2,
     .         EIY2,EIZ2

      INTEGER, ALLOCATABLE, DIMENSION(:)   :: LIST_B, LIST_E,    !liste des briques dans CAND_B(1:RI_STOK) et CAND_e(1:RI_STOK) sans doublons
     .                                        IADF, IADL,        ! IADF : add(list_B)->First_add(cand_b)    IADL : add(list_B)->Last_add(cand_b)   (CAND_B est connexe par ID de bric)
     .                                        GET_LIST_E_POS_FROM_CAND_E_POS ,  ! IAD_CAND_TO_LIST : add(CAND_E)->add(List_E) surjectif
     .                                        LIST_B_ADD
      INTEGER :: NCANDB, NCANDE,
     .           NCANDB_ADD

      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAGB, ITAGE,  diag22

      my_real,
     . dimension(:,:,:), allocatable :: vNE, vZA, vZB, ptA

      my_real,
     . dimension(:,:), allocatable :: BasisCONST, pt Z

      INTEGER, DIMENSION(:), ALLOCATABLE :: NbSubTriangles

      INTEGER, ALLOCATABLE, DIMENSION(:) :: ID_,IDLOC_,NG_,NEL_   !sinit_22.F
      
      my_real, ALLOCATABLE, DIMENSION(:) :: PHI22
      my_real, ALLOCATABLE, DIMENSION(:) :: INT22_FCELL_ANIM

      INTEGER :: NBCUT_MAX
      !PARAMETER (NB_CUT_MAX = 26)
      
      INTEGER II_STOK2, ISKIP22, IN22
      
      my_real, ALLOCATABLE, DIMENSION(:,:) :: TMP22ARRAY

      END MODULE I22TRI_MOD
