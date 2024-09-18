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
      !||====================================================================
      !||    i22bufbric_mod               ../common_source/modules/interfaces/cut-cell-search_mod.F
      !||--- called by ------------------------------------------------------
      !||    a22conv3                     ../engine/source/ale/alefvm/cut_cells/a22conv3.F
      !||    aconve                       ../engine/source/ale/aconve.F
      !||    aflux3                       ../engine/source/ale/ale3d/aflux3.F
      !||    aflux3_int22_fvm             ../engine/source/ale/alefvm/cut_cells/aflux3_int22_fvm.F
      !||    afluxt                       ../engine/source/ale/ale51/afluxt.F
      !||    ale51_antidiff3              ../engine/source/ale/ale51/ale51_antidiff3.F
      !||    ale51_antidiff3_int22        ../engine/source/ale/alefvm/cut_cells/ale51_antidiff3_int22.F
      !||    ale51_finish                 ../engine/source/ale/ale51/ale51_finish.F
      !||    ale51_init                   ../engine/source/ale/ale51/ale51_init.F
      !||    ale51_upwind3_int22          ../engine/source/ale/alefvm/cut_cells/ale51_upwind3_int22.F
      !||    alefvm_freset                ../engine/source/ale/alefvm/alefvm_freset.F
      !||    alefvm_gravity_int22         ../engine/source/ale/alefvm/alefvm_gravity_int22.F
      !||    alefvm_scheme                ../engine/source/ale/alefvm/alefvm_scheme.F
      !||    alefvm_sfint3                ../engine/source/ale/alefvm/alefvm_sfint3.F
      !||    alefvm_sfint3_int22          ../engine/source/ale/alefvm/alefvm_sfint3_int22.F
      !||    alefvm_stress_int22          ../engine/source/ale/alefvm/alefvm_stress_int22.F
      !||    alemuscl_upwind              ../engine/source/ale/alemuscl/alemuscl_upwind.F
      !||    alemuscl_upwind2             ../engine/source/ale/alemuscl/alemuscl_upwind2.F
      !||    amass3                       ../engine/source/elements/solid/solide/amass3.F
      !||    amass3p                      ../engine/source/elements/solid/solide/amass3p.F
      !||    anim_nodal_p_elems           ../engine/source/output/anim/generate/anim_nodal_p_elems.F
      !||    deltax22                     ../engine/source/interfaces/int22/deltax22.F
      !||    destroy_cell                 ../engine/source/interfaces/int22/destroy_cell.F
      !||    eflux3_int22_fvm             ../engine/source/ale/alefvm/cut_cells/eflux3_int22_fvm.F
      !||    get_unique_main_cell         ../engine/source/interfaces/int22/get_unique_master_cell.F
      !||    h3d_velvecc22                ../engine/source/output/h3d/h3d_results/h3d_velvecc22.F
      !||    h3d_velvecz22                ../engine/source/output/h3d/h3d_results/h3d_velvecz22.F
      !||    i22ass2                      ../engine/source/interfaces/int22/i22assembly.F
      !||    i22buce                      ../engine/source/interfaces/intsort/i22buce.F
      !||    i22cor3                      ../engine/source/interfaces/int22/i22cor3.F
      !||    i22datainit                  ../engine/source/interfaces/int22/i22datainit.F
      !||    i22datainit_db               ../engine/source/interfaces/int22/i22datainit_db.F
      !||    i22for3                      ../engine/source/interfaces/int22/i22for3.F
      !||    i22gbit                      ../engine/source/interfaces/int22/i22ident.F
      !||    i22get_prev_data             ../engine/source/interfaces/int22/i22get_prev_data.F
      !||    i22ident                     ../engine/source/interfaces/int22/i22ident.F
      !||    i22intersect                 ../engine/source/interfaces/int22/i22intersect.F
      !||    i22main_tri                  ../engine/source/interfaces/intsort/i22main_tri.F
      !||    i22solid_getminmax           ../engine/source/interfaces/intsort/i22main_tri.F
      !||    i22subvol                    ../engine/source/interfaces/int22/i22subvol.F
      !||    i22trivox                    ../engine/source/interfaces/intsort/i22trivox.F
      !||    i22wetsurf                   ../engine/source/interfaces/int22/i22wetsurf.F
      !||    int22listcombi               ../engine/source/interfaces/int22/int22ListCombi.F
      !||    inttri                       ../engine/source/interfaces/intsort/inttri.F
      !||    link_with_unique_main_cell   ../engine/source/interfaces/int22/link_with_unique_master_cell.F
      !||    mqviscb                      ../engine/source/materials/mat_share/mqviscb.F
      !||    nodal_schlieren              ../engine/source/output/anim/generate/nodal_schlieren.F
      !||    nodald                       ../engine/source/output/anim/generate/nodald.F
      !||    nodalt                       ../engine/source/output/anim/generate/nodalt.F
      !||    nodalvfrac                   ../engine/source/output/anim/generate/nodalvfrac.F
      !||    nodalzvol                    ../engine/source/output/anim/generate/nodalzvol.F
      !||    r_bufbric_22                 ../engine/source/interfaces/int22/r_bufbric_22.F
      !||    resol                        ../engine/source/engine/resol.F
      !||    scoor3_fvm                   ../engine/source/ale/alefvm/scoor3_fvm.F
      !||    sigeps105                    ../engine/source/materials/mat/mat105/sigeps105.F
      !||    sigeps51                     ../engine/source/materials/mat/mat051/sigeps51.F
      !||    sigeps97                     ../engine/source/materials/mat/mat097/sigeps97.F
      !||    sinit22_fvm                  ../engine/source/interfaces/int22/sinit22_fvm.F
      !||    srho3                        ../engine/source/elements/solid/solide/srho3.F
      !||    velvecc22                    ../engine/source/output/anim/generate/velvec.F
      !||    velvecz22                    ../engine/source/output/anim/generate/velvecz22.F
      !||    voln22                       ../engine/source/interfaces/int22/voln22.F
      !||    w_bufbric_22                 ../engine/source/interfaces/int22/w_bufbric_22.F
      !||    weighting_cell_nodes         ../engine/source/interfaces/int22/weighting_cell_nodes.F
      !||    write_buf_law51              ../engine/source/materials/mat/mat051/write_buf_law51.F
      !||    write_cut_cell_buffer        ../engine/source/interfaces/int22/write_cut_cell_buffer.F
      !||--- uses       -----------------------------------------------------
      !||    i22edge_mod                  ../common_source/modules/interfaces/cut-cell-buffer_mod.F
      !||====================================================================
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
      TYPE(BRICK_ENTITY), TARGET,ALLOCATABLE,DIMENSION(:,:) :: BRICK_LIST 
      TYPE(EDGE_ENTITY),  TARGET,ALLOCATABLE,DIMENSION(:,:) :: EDGE_LIST 
      
      TYPE INT22_BUF_t
        INTEGER, DIMENSION(2,12)  :: iEDGE                   !12 without diagonals, 24 with diagonals
        INTEGER, DIMENSION(2,2,6) :: iFACE                   ! 2 nodes per face definig a diagonal
        INTEGER, DIMENSION(6,4)   :: nodFACE                 ! quad face
        INTEGER, DIMENSION(6)     :: bNodFACE                ! binary code
        INTEGER, DIMENSION(8,3)   :: i22WhichEdge            !provide 3 edge number (from 1 to 12) related to local node given as index 1.
                                                             !  + : intersection point is the maximum of both if multiple intersection point on edge
                                                             !  - : intersection point is the minimum of both if multiple intersection point on edge
        LOGICAL, DIMENSION(8,6)   :: IsNodeOnFace            !tell if brick node is on a given face
        INTEGER, DIMENSION(8,3)   :: iFacesFromNode          !from a node 1:8 gives 3 connected faces
        INTEGER, DIMENSION(6,8)   :: iGetEdge                !from a node and a face, gives the normal edge
        INTEGER, DIMENSION(6,8)   :: iGetOppositeNode        !idem with opposite note instead of edge number
        INTEGER, DIMENSION(6,8,2) :: IGetTransvNodes         !from face and node, provides 2 other transverse nodes
        INTEGER, DIMENSION(6,4)   :: iGetEdgesFromFace       !from a face_id provides all edge_ids composing the face
        INTEGER, DIMENSION(6,4)   :: iNormalEdgesList        !from a face_id provides all edge_ids normal to the face.    
        INTEGER, DIMENSION(12,2)  :: iGetAdjFace             !from a edge_id get adjfaces
        INTEGER, DIMENSION(6,12)  :: iGetNodeFromEdgeAndFace ! From face_id and edge_id provide node_id which is intersection.
        INTEGER, DIMENSION(6,4)   :: iLeftEdge               ! From node_id 1:4 and face 1:6 gives Left edge (clockwise edge path)      
      END TYPE
      
      TYPE(INT22_BUF_t) :: INT22_BUF
      
      END MODULE I22BUFBRIC_MOD
C
C
C
      !||====================================================================
      !||    i22tri_mod                   ../common_source/modules/interfaces/cut-cell-search_mod.F
      !||--- called by ------------------------------------------------------
      !||    a22conv3                     ../engine/source/ale/alefvm/cut_cells/a22conv3.F
      !||    aconv3                       ../engine/source/ale/ale3d/aconv3.F
      !||    aconve                       ../engine/source/ale/aconve.F
      !||    aflux0                       ../engine/source/ale/aflux0.F
      !||    aflux3_int22_fvm             ../engine/source/ale/alefvm/cut_cells/aflux3_int22_fvm.F
      !||    afluxt                       ../engine/source/ale/ale51/afluxt.F
      !||    ale51_antidiff3              ../engine/source/ale/ale51/ale51_antidiff3.F
      !||    ale51_antidiff3_int22        ../engine/source/ale/alefvm/cut_cells/ale51_antidiff3_int22.F
      !||    ale51_finish                 ../engine/source/ale/ale51/ale51_finish.F
      !||    ale51_init                   ../engine/source/ale/ale51/ale51_init.F
      !||    ale51_upwind3                ../engine/source/ale/ale51/ale51_upwind3.F
      !||    ale51_upwind3_int22          ../engine/source/ale/alefvm/cut_cells/ale51_upwind3_int22.F
      !||    alefvm_aflux3                ../engine/source/ale/alefvm/alefvm_aflux3.F
      !||    alefvm_eflux3                ../engine/source/ale/alefvm/alefvm_eflux3.F
      !||    alefvm_main                  ../engine/source/ale/alefvm/alefvm_main.F
      !||    alefvm_scheme                ../engine/source/ale/alefvm/alefvm_scheme.F
      !||    alefvm_sfint3                ../engine/source/ale/alefvm/alefvm_sfint3.F
      !||    alefvm_sfint3_int22          ../engine/source/ale/alefvm/alefvm_sfint3_int22.F
      !||    alemuscl_upwind              ../engine/source/ale/alemuscl/alemuscl_upwind.F
      !||    alemuscl_upwind2             ../engine/source/ale/alemuscl/alemuscl_upwind2.F
      !||    alethe                       ../engine/source/ale/alethe.F
      !||    anim_nodal_p_elems           ../engine/source/output/anim/generate/anim_nodal_p_elems.F
      !||    deltax22                     ../engine/source/interfaces/int22/deltax22.F
      !||    destroy_cell                 ../engine/source/interfaces/int22/destroy_cell.F
      !||    dfuncs                       ../engine/source/output/anim/generate/dfunc6.F
      !||    eflux3                       ../engine/source/ale/euler3d/eflux3.F
      !||    eflux3_int22_fvm             ../engine/source/ale/alefvm/cut_cells/eflux3_int22_fvm.F
      !||    get_unique_main_cell         ../engine/source/interfaces/int22/get_unique_master_cell.F
      !||    h3d_velvecc22                ../engine/source/output/h3d/h3d_results/h3d_velvecc22.F
      !||    h3d_velvecz22                ../engine/source/output/h3d/h3d_results/h3d_velvecz22.F
      !||    i22ass2                      ../engine/source/interfaces/int22/i22assembly.F
      !||    i22buce                      ../engine/source/interfaces/intsort/i22buce.F
      !||    i22cor3                      ../engine/source/interfaces/int22/i22cor3.F
      !||    i22datainit                  ../engine/source/interfaces/int22/i22datainit.F
      !||    i22datainit_db               ../engine/source/interfaces/int22/i22datainit_db.F
      !||    i22for3                      ../engine/source/interfaces/int22/i22for3.F
      !||    i22gbit                      ../engine/source/interfaces/int22/i22ident.F
      !||    i22get_prev_data             ../engine/source/interfaces/int22/i22get_prev_data.F
      !||    i22ident                     ../engine/source/interfaces/int22/i22ident.F
      !||    i22intersect                 ../engine/source/interfaces/int22/i22intersect.F
      !||    i22main_tri                  ../engine/source/interfaces/intsort/i22main_tri.F
      !||    i22mainf                     ../engine/source/interfaces/int22/i22mainf.F
      !||    i22shell_getminmax           ../engine/source/interfaces/intsort/i22main_tri.F
      !||    i22solid_getminmax           ../engine/source/interfaces/intsort/i22main_tri.F
      !||    i22sto                       ../engine/source/interfaces/intsort/i22sto.F
      !||    i22subvol                    ../engine/source/interfaces/int22/i22subvol.F
      !||    i22trivox                    ../engine/source/interfaces/intsort/i22trivox.F
      !||    i22wetsurf                   ../engine/source/interfaces/int22/i22wetsurf.F
      !||    int22listcombi               ../engine/source/interfaces/int22/int22ListCombi.F
      !||    inttri                       ../engine/source/interfaces/intsort/inttri.F
      !||    link_with_unique_main_cell   ../engine/source/interfaces/int22/link_with_unique_master_cell.F
      !||    mqviscb                      ../engine/source/materials/mat_share/mqviscb.F
      !||    nodal_schlieren              ../engine/source/output/anim/generate/nodal_schlieren.F
      !||    nodald                       ../engine/source/output/anim/generate/nodald.F
      !||    nodalt                       ../engine/source/output/anim/generate/nodalt.F
      !||    nodalvfrac                   ../engine/source/output/anim/generate/nodalvfrac.F
      !||    nodalzvol                    ../engine/source/output/anim/generate/nodalzvol.F
      !||    r_bufbric_22                 ../engine/source/interfaces/int22/r_bufbric_22.F
      !||    resol                        ../engine/source/engine/resol.F
      !||    scoor3_fvm                   ../engine/source/ale/alefvm/scoor3_fvm.F
      !||    sigeps105                    ../engine/source/materials/mat/mat105/sigeps105.F
      !||    sigeps51                     ../engine/source/materials/mat/mat051/sigeps51.F
      !||    sigeps97                     ../engine/source/materials/mat/mat097/sigeps97.F
      !||    sinit22_fvm                  ../engine/source/interfaces/int22/sinit22_fvm.F
      !||    spmd_tri22vox                ../engine/source/mpi/interfaces/spmd_tri22vox.F
      !||    velvecc22                    ../engine/source/output/anim/generate/velvec.F
      !||    velvecz22                    ../engine/source/output/anim/generate/velvecz22.F
      !||    voln22                       ../engine/source/interfaces/int22/voln22.F
      !||    w_bufbric_22                 ../engine/source/interfaces/int22/w_bufbric_22.F
      !||    write_cut_cell_buffer        ../engine/source/interfaces/int22/write_cut_cell_buffer.F
      !||--- uses       -----------------------------------------------------
      !||    i22edge_mod                  ../common_source/modules/interfaces/cut-cell-buffer_mod.F
      !||====================================================================
      MODULE I22TRI_MOD
C-----------------------------------------------
C  M o d u l e s
C-----------------------------------------------
      USE I22EDGE_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
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
     .        ICANDB                             !ICANDB:(id of brick which is candidate for access in BRICK_LIST)   IGROUP->ICANDB         
           
      INTEGER ::
     .        NB                                 !NB: number of candidates (bricks)
     
      INTEGER, ALLOCATABLE, DIMENSION(:) ::
     .         EIX1,EIY1,
     .         EIZ1,EIX2,
     .         EIY2,EIZ2

      INTEGER, ALLOCATABLE, DIMENSION(:)   :: LIST_B, LIST_E,    !list of bricks in CAND_B(1:RI_STOK) & CAND_e(1:RI_STOK), no duplicates
     .                                        IADF, IADL,        ! IADF : add(list_B)->First_add(cand_b)    IADL : add(list_B)->Last_add(cand_b)   (CAND_B is connected regarde brick ids)
     .                                        GET_LIST_E_POS_FROM_CAND_E_POS ,  ! IAD_CAND_TO_LIST : add(CAND_E)->add(List_E) surjective
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
