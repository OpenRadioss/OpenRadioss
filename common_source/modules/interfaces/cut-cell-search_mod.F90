!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!hd|====================================================================
!hd|  I22BUFBRIC_MOD                modules/interfaces/cut-cell-search_mod.F
!hd|-- called by -----------
!hd|        W_BUFBRIC_22                  starter/source/interfaces/inter3d1/w_bufbric_22.F
!hd|        A22CONV3                      engine/source/ale/alefvm/cut_cells/a22conv3.F
!hd|        ACONVE                        engine/source/ale/aconve.F
!hd|        AFLUX3                        engine/source/ale/ale3d/aflux3.F
!hd|        AFLUX3_INT22_FVM              engine/source/ale/alefvm/cut_cells/aflux3_int22_fvm.F
!hd|        AFLUXT                        engine/source/ale/ale51/afluxt.F
!hd|        ALE51_ANTIDIFF3               engine/source/ale/ale51/ale51_antidiff3.F
!hd|        ALE51_ANTIDIFF3_INT22         engine/source/ale/alefvm/cut_cells/ale51_antidiff3_int22.F
!hd|        ALE51_FINISH                  engine/source/ale/ale51/ale51_finish.F
!hd|        ALE51_INIT                    engine/source/ale/ale51/ale51_init.F
!hd|        ALE51_UPWIND3_INT22           engine/source/ale/alefvm/cut_cells/ale51_upwind3_int22.F
!hd|        ALEFVM_FRESET                 engine/source/ale/alefvm/alefvm_freset.F
!hd|        ALEFVM_GRAVITY_INT22          engine/source/ale/alefvm/alefvm_gravity_int22.F
!hd|        ALEFVM_SCHEME                 engine/source/ale/alefvm/alefvm_scheme.F
!hd|        ALEFVM_SFINT3                 engine/source/ale/alefvm/alefvm_sfint3.F
!hd|        ALEFVM_SFINT3_INT22           engine/source/ale/alefvm/alefvm_sfint3_int22.F
!hd|        ALEFVM_STRESS_INT22           engine/source/ale/alefvm/alefvm_stress_int22.F
!hd|        ALEMUSCL_UPWIND               engine/source/ale/alemuscl/alemuscl_upwind.F
!hd|        ALEMUSCL_UPWIND2              engine/source/ale/alemuscl/alemuscl_upwind2.F
!hd|        AMASS3                        engine/source/elements/solid/solide/amass3.F
!hd|        AMASS3P                       engine/source/elements/solid/solide/amass3p.F
!hd|        ANIM_NODAL_P_ELEMS            engine/source/output/anim/generate/anim_nodal_p_elems.F
!hd|        DELTAX22                      engine/source/interfaces/int22/deltax22.F
!hd|        DESTROY_CELL                  engine/source/interfaces/int22/destroy_cell.F
!hd|        EFLUX3_INT22_FVM              engine/source/ale/alefvm/cut_cells/eflux3_int22_fvm.F
!hd|        H3D_VELVECC22                 engine/source/output/h3d/h3d_results/h3d_velvecc22.F
!hd|        H3D_VELVECZ22                 engine/source/output/h3d/h3d_results/h3d_velvecz22.F
!hd|        I22ASS2                       engine/source/interfaces/int22/i22assembly.F
!hd|        I22BUCE                       engine/source/interfaces/intsort/i22buce.F
!hd|        I22COR3                       engine/source/interfaces/int22/i22cor3.F
!hd|        I22DATAINIT                   engine/source/interfaces/int22/i22datainit.F
!hd|        I22DATAINIT_DB                engine/source/interfaces/int22/i22datainit_db.F
!hd|        I22FOR3                       engine/source/interfaces/int22/i22for3.F
!hd|        I22GBIT                       engine/source/interfaces/int22/i22ident.F
!hd|        I22GET_PREV_DATA              engine/source/interfaces/int22/i22get_prev_data.F
!hd|        I22IDENT                      engine/source/interfaces/int22/i22ident.F
!hd|        I22INTERSECT                  engine/source/interfaces/int22/i22intersect.F
!hd|        I22MAIN_TRI                   engine/source/interfaces/intsort/i22main_tri.F
!hd|        I22SOLID_GETMINMAX            engine/source/interfaces/intsort/i22main_tri.F
!hd|        I22SUBVOL                     engine/source/interfaces/int22/i22subvol.F
!hd|        I22TRIVOX                     engine/source/interfaces/intsort/i22trivox.F
!hd|        I22WETSURF                    engine/source/interfaces/int22/i22wetsurf.F
!hd|        INTTRI                        engine/source/interfaces/intsort/inttri.F
!hd|        MQVISCB                       engine/source/materials/mat_share/mqviscb.F
!hd|        NODALD                        engine/source/output/anim/generate/nodald.F
!hd|        NODALT                        engine/source/output/anim/generate/nodalt.F
!hd|        NODALVFRAC                    engine/source/output/anim/generate/nodalvfrac.F
!hd|        NODALZVOL                     engine/source/output/anim/generate/nodalzvol.F
!hd|        NODAL_SCHLIEREN               engine/source/output/anim/generate/nodal_schlieren.F
!hd|        RESOL                         engine/source/engine/resol.F
!hd|        R_BUFBRIC_22                  engine/source/interfaces/int22/r_bufbric_22.F
!hd|        SCOOR3_FVM                    engine/source/ale/alefvm/scoor3_fvm.F
!hd|        SIGEPS105                     engine/source/materials/mat/mat105/sigeps105.F
!hd|        SIGEPS51                      engine/source/materials/mat/mat051/sigeps51.F
!hd|        SIGEPS97                      engine/source/materials/mat/mat097/sigeps97.F
!hd|        SINIT22_FVM                   engine/source/interfaces/int22/sinit22_fvm.F
!hd|        SRHO3                         engine/source/elements/solid/solide/srho3.F
!hd|        VELVECC22                     engine/source/output/anim/generate/velvec.F
!hd|        VELVECZ22                     engine/source/output/anim/generate/velvecz22.F
!hd|        VOLN22                        engine/source/interfaces/int22/voln22.F
!hd|        WRITE_BUF_LAW51               engine/source/materials/mat/mat051/write_buf_law51.F
!hd|        WRITE_CUT_CELL_BUFFER         engine/source/interfaces/int22/write_cut_cell_buffer.F
!hd|        W_BUFBRIC_22                  engine/source/interfaces/int22/w_bufbric_22.F
!hd|        weighting_Cell_Nodes          engine/source/interfaces/int22/weighting_cell_nodes.F
!hd|-- calls ---------------
!hd|        i22EDGE_MOD                   modules/interfaces/cut-cell-buffer_mod.F
!hd|====================================================================
      MODULE I22BUFBRIC_MOD
!-----------------------------------------------
!  M o d u l e s
!-----------------------------------------------
        USE i22EDGE_MOD
!-----------------------------------------------
!   m y _ r e a l
!-----------------------------------------------
#include      "my_real.inc"
!-----------------------------------------------
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
!
!
!
!hd|====================================================================
!hd|  I22TRI_MOD                    modules/interfaces/cut-cell-search_mod.F
!hd|-- called by -----------
!hd|        W_BUFBRIC_22                  starter/source/interfaces/inter3d1/w_bufbric_22.F
!hd|        A22CONV3                      engine/source/ale/alefvm/cut_cells/a22conv3.F
!hd|        ACONV3                        engine/source/ale/ale3d/aconv3.F
!hd|        ACONVE                        engine/source/ale/aconve.F
!hd|        AFLUX0                        engine/source/ale/aflux0.F
!hd|        AFLUX3_INT22_FVM              engine/source/ale/alefvm/cut_cells/aflux3_int22_fvm.F
!hd|        AFLUXT                        engine/source/ale/ale51/afluxt.F
!hd|        ALE51_ANTIDIFF3               engine/source/ale/ale51/ale51_antidiff3.F
!hd|        ALE51_ANTIDIFF3_INT22         engine/source/ale/alefvm/cut_cells/ale51_antidiff3_int22.F
!hd|        ALE51_FINISH                  engine/source/ale/ale51/ale51_finish.F
!hd|        ALE51_INIT                    engine/source/ale/ale51/ale51_init.F
!hd|        ALE51_UPWIND3                 engine/source/ale/ale51/ale51_upwind3.F
!hd|        ALE51_UPWIND3_INT22           engine/source/ale/alefvm/cut_cells/ale51_upwind3_int22.F
!hd|        ALEFVM_AFLUX3                 engine/source/ale/alefvm/alefvm_aflux3.F
!hd|        ALEFVM_EFLUX3                 engine/source/ale/alefvm/alefvm_eflux3.F
!hd|        ALEFVM_MAIN                   engine/source/ale/alefvm/alefvm_main.F
!hd|        ALEFVM_SCHEME                 engine/source/ale/alefvm/alefvm_scheme.F
!hd|        ALEFVM_SFINT3                 engine/source/ale/alefvm/alefvm_sfint3.F
!hd|        ALEFVM_SFINT3_INT22           engine/source/ale/alefvm/alefvm_sfint3_int22.F
!hd|        ALEMUSCL_UPWIND               engine/source/ale/alemuscl/alemuscl_upwind.F
!hd|        ALEMUSCL_UPWIND2              engine/source/ale/alemuscl/alemuscl_upwind2.F
!hd|        ALETHE                        engine/source/ale/alethe.F
!hd|        ANIM_NODAL_P_ELEMS            engine/source/output/anim/generate/anim_nodal_p_elems.F
!hd|        DELTAX22                      engine/source/interfaces/int22/deltax22.F
!hd|        DESTROY_CELL                  engine/source/interfaces/int22/destroy_cell.F
!hd|        DFUNCS                        engine/source/output/anim/generate/dfunc6.F
!hd|        EFLUX3                        engine/source/ale/euler3d/eflux3.F
!hd|        EFLUX3_INT22_FVM              engine/source/ale/alefvm/cut_cells/eflux3_int22_fvm.F
!hd|        H3D_VELVECC22                 engine/source/output/h3d/h3d_results/h3d_velvecc22.F
!hd|        H3D_VELVECZ22                 engine/source/output/h3d/h3d_results/h3d_velvecz22.F
!hd|        I22ASS2                       engine/source/interfaces/int22/i22assembly.F
!hd|        I22BUCE                       engine/source/interfaces/intsort/i22buce.F
!hd|        I22COR3                       engine/source/interfaces/int22/i22cor3.F
!hd|        I22DATAINIT                   engine/source/interfaces/int22/i22datainit.F
!hd|        I22DATAINIT_DB                engine/source/interfaces/int22/i22datainit_db.F
!hd|        I22FOR3                       engine/source/interfaces/int22/i22for3.F
!hd|        I22GBIT                       engine/source/interfaces/int22/i22ident.F
!hd|        I22GET_PREV_DATA              engine/source/interfaces/int22/i22get_prev_data.F
!hd|        I22IDENT                      engine/source/interfaces/int22/i22ident.F
!hd|        I22INTERSECT                  engine/source/interfaces/int22/i22intersect.F
!hd|        I22MAINF                      engine/source/interfaces/int22/i22mainf.F
!hd|        I22MAIN_TRI                   engine/source/interfaces/intsort/i22main_tri.F
!hd|        I22SHELL_GETMINMAX            engine/source/interfaces/intsort/i22main_tri.F
!hd|        I22SOLID_GETMINMAX            engine/source/interfaces/intsort/i22main_tri.F
!hd|        I22STO                        engine/source/interfaces/intsort/i22sto.F
!hd|        I22SUBVOL                     engine/source/interfaces/int22/i22subvol.F
!hd|        I22TRIVOX                     engine/source/interfaces/intsort/i22trivox.F
!hd|        I22WETSURF                    engine/source/interfaces/int22/i22wetsurf.F
!hd|        INTTRI                        engine/source/interfaces/intsort/inttri.F
!hd|        MQVISCB                       engine/source/materials/mat_share/mqviscb.F
!hd|        NODALD                        engine/source/output/anim/generate/nodald.F
!hd|        NODALT                        engine/source/output/anim/generate/nodalt.F
!hd|        NODALVFRAC                    engine/source/output/anim/generate/nodalvfrac.F
!hd|        NODALZVOL                     engine/source/output/anim/generate/nodalzvol.F
!hd|        NODAL_SCHLIEREN               engine/source/output/anim/generate/nodal_schlieren.F
!hd|        RESOL                         engine/source/engine/resol.F
!hd|        R_BUFBRIC_22                  engine/source/interfaces/int22/r_bufbric_22.F
!hd|        SCOOR3_FVM                    engine/source/ale/alefvm/scoor3_fvm.F
!hd|        SIGEPS105                     engine/source/materials/mat/mat105/sigeps105.F
!hd|        SIGEPS51                      engine/source/materials/mat/mat051/sigeps51.F
!hd|        SIGEPS97                      engine/source/materials/mat/mat097/sigeps97.F
!hd|        SINIT22_FVM                   engine/source/interfaces/int22/sinit22_fvm.F
!hd|        SPMD_TRI22VOX                 engine/source/mpi/interfaces/spmd_tri22vox.F
!hd|        VELVECC22                     engine/source/output/anim/generate/velvec.F
!hd|        VELVECZ22                     engine/source/output/anim/generate/velvecz22.F
!hd|        VOLN22                        engine/source/interfaces/int22/voln22.F
!hd|        WRITE_CUT_CELL_BUFFER         engine/source/interfaces/int22/write_cut_cell_buffer.F
!hd|        W_BUFBRIC_22                  engine/source/interfaces/int22/w_bufbric_22.F
!hd|-- calls ---------------
!hd|====================================================================
      MODULE I22TRI_MOD
!-----------------------------------------------
!  M o d u l e s
!-----------------------------------------------
        USE I22EDGE_MOD
!-----------------------------------------------
!   m y _ r e a l
!-----------------------------------------------
#include      "my_real.inc"
#include      "mmale51_c.inc"
!-----------------------------------------------

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
!-----------------------------------------------


        INTEGER ::&
        &MIN_IX, MIN_IY, MIN_IZ,&            !indice voxel min utilise
        &MAX_IX, MAX_IY, MAX_IZ             !indice voxel max utilise

        INTEGER ::&
        &CURRENT_ADD,&
        &NRTM_T_MAX ! (nombre maximum des valeurs locales)

        my_real, ALLOCATABLE ::&
        &IRECT_L(:,:)    !IRECT_L <=> IRECT + XREM  (T=thread, L=Localdomain)
        INTEGER ::&
        &NIRECT_L
        INTEGER :: SIZ_IRECT_L
        PARAMETER (SIZ_IRECT_L = 26)  !longueur buffer (nodes, coordinates, min, max, stif...)

        INTEGER, ALLOCATABLE, DIMENSION(:)  ::&
        &NSNR_G

        my_real, ALLOCATABLE,DIMENSION(:) ::&
        &XMAXS, XMINS,&
        &YMAXS, YMINS,&
        &ZMAXS, ZMINS

        my_real, ALLOCATABLE,DIMENSION(:,:) ::&
        &BMINMA_LAG_SPMD
        my_real ::&
        &BMINMA_LAG_G(6),& ! lagrangian bounds for global domain (for int22)
        &BMINMA_LAG_R(6)  ! lagrangian remotes domain (for int22)

        my_real, ALLOCATABLE,DIMENSION(:) ::&
        &XMAXE, XMINE,&
        &YMAXE, YMINE,&
        &ZMAXE, ZMINE

        INTEGER :: SIZ_XREM
        PARAMETER (SIZ_XREM = 26)

        INTEGER, POINTER, DIMENSION(:) ::&
        &LCHAIN_ELEM, LCHAIN_NEXT, LCHAIN_LAST

        INTEGER,  DIMENSION(:,:),ALLOCATABLE ::&
        &N1_TAB, ADD_EDGE, ADD_FACE

        INTEGER ::&
        &IMIN, IMAX

        INTEGER,  DIMENSION(:),ALLOCATABLE ::&
        &UBOUND, LBOUND,&
        &ICANDB                             !ICANDB:(id of brick which is candidate for access in BRICK_LIST)   IGROUP->ICANDB

        INTEGER ::&
        &NB                                 !NB: number of candidates (bricks)

        INTEGER, ALLOCATABLE, DIMENSION(:) ::&
        &EIX1,EIY1,&
        &EIZ1,EIX2,&
        &EIY2,EIZ2

        INTEGER, ALLOCATABLE, DIMENSION(:)   :: LIST_B, LIST_E,&    !list of bricks in CAND_B(1:RI_STOK) & CAND_e(1:RI_STOK), no duplicates
        &IADF, IADL,&        ! IADF : add(list_B)->First_add(cand_b)    IADL : add(list_B)->Last_add(cand_b)   (CAND_B is connected regarde brick ids)
        &GET_LIST_E_POS_FROM_CAND_E_POS ,&  ! IAD_CAND_TO_LIST : add(CAND_E)->add(List_E) surjective
        &LIST_B_ADD
        INTEGER :: NCANDB, NCANDE,&
        &NCANDB_ADD

        INTEGER, DIMENSION(:), ALLOCATABLE :: ITAGB, ITAGE,  diag22

        my_real,&
        &dimension(:,:,:), allocatable :: vNE, vZA, vZB, ptA

        my_real,&
        &dimension(:,:), allocatable :: BasisCONST, ptZ

        INTEGER, DIMENSION(:), ALLOCATABLE :: NbSubTriangles

        INTEGER, ALLOCATABLE, DIMENSION(:) :: ID_,IDLOC_,NG_,NEL_   !sinit_22.F

        my_real, ALLOCATABLE, DIMENSION(:) :: PHI22
        my_real, ALLOCATABLE, DIMENSION(:) :: INT22_FCELL_ANIM

        INTEGER :: NBCUT_MAX
        !PARAMETER (NB_CUT_MAX = 26)

        INTEGER II_STOK2, ISKIP22, IN22

        my_real, ALLOCATABLE, DIMENSION(:,:) :: TMP22ARRAY

      END MODULE I22TRI_MOD
