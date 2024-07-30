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
      !||    setdef_mod                         ../common_source/modules/setdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    check_eltyp                        ../starter/source/model/sets/check_eltyp.F
      !||    clause_init                        ../starter/source/model/sets/clause_init.F
      !||    contrl                             ../starter/source/starter/contrl.F
      !||    copy_list_in_cause                 ../starter/source/model/sets/create_element_clause.F
      !||    create_box_clause                  ../starter/source/model/sets/create_box_clause.F
      !||    create_elem_all_clause             ../starter/source/model/sets/create_elem_all_clause.F
      !||    create_element_from_part           ../starter/source/model/sets/create_element_from_part.F
      !||    create_ellipse_clause              ../starter/source/model/sets/create_ellipse_clause.F
      !||    create_elt_box                     ../starter/source/model/sets/create_elt_box.F
      !||    create_elt_clause                  ../starter/source/model/sets/create_element_clause.F
      !||    create_elt_list                    ../starter/source/model/sets/create_element_clause.F
      !||    create_elt_list_g                  ../starter/source/model/sets/create_element_clause.F
      !||    create_line_from_element           ../starter/source/model/sets/create_line_from_element.F
      !||    create_line_from_surface           ../starter/source/model/sets/create_line_from_surface.F
      !||    create_line_from_surface_all       ../starter/source/model/sets/create_line_from_surface_all.F
      !||    create_line_from_surface_ext       ../starter/source/model/sets/create_line_from_surface_ext.F
      !||    create_line_from_surface_ext_all   ../starter/source/model/sets/create_line_from_ext_surface_ext_all.F
      !||    create_map_tables                  ../starter/source/model/sets/map_tables.F
      !||    create_node_all_clause             ../starter/source/model/sets/create_node_all_clause.F
      !||    create_node_box                    ../starter/source/model/sets/create_node_box.F
      !||    create_node_clause                 ../starter/source/model/sets/create_node_clause.F
      !||    create_node_from_element           ../starter/source/model/sets/create_node_from_element.F
      !||    create_node_from_rbody             ../starter/source/model/sets/create_node_from_rbody.F
      !||    create_node_from_seg               ../starter/source/model/sets/create_node_from_seg.F
      !||    create_node_list                   ../starter/source/model/sets/create_node_clause.F
      !||    create_node_list_g                 ../starter/source/model/sets/create_node_clause.F
      !||    create_nodens_clause               ../starter/source/model/sets/create_nodens_clause.F90
      !||    create_part_all_clause             ../starter/source/model/sets/create_part_all_clause.F
      !||    create_part_clause                 ../starter/source/model/sets/create_part_clause.F
      !||    create_part_list                   ../starter/source/model/sets/create_part_clause.F
      !||    create_part_list_g                 ../starter/source/model/sets/create_part_clause.F
      !||    create_plane_clause                ../starter/source/model/sets/create_plane_clause.F90
      !||    create_rbody_box                   ../starter/source/model/sets/create_rbody_box.F
      !||    create_rbody_clause                ../starter/source/model/sets/create_rbody_clause.F
      !||    create_rbody_list                  ../starter/source/model/sets/create_rbody_clause.F
      !||    create_rbody_list_g                ../starter/source/model/sets/create_rbody_clause.F
      !||    create_seg_clause                  ../starter/source/model/sets/create_seg_clause.F
      !||    create_set_array                   ../starter/source/model/sets/create_set_clause.F
      !||    create_set_clause                  ../starter/source/model/sets/create_set_clause.F
      !||    create_set_collect                 ../starter/source/model/sets/create_setcol_clause.F
      !||    create_set_list                    ../starter/source/model/sets/create_set_clause.F
      !||    create_set_list_g                  ../starter/source/model/sets/create_set_clause.F
      !||    create_setcol_array                ../starter/source/model/sets/create_setcol_clause.F
      !||    create_setcol_clause               ../starter/source/model/sets/create_setcol_clause.F
      !||    create_setcol_list                 ../starter/source/model/sets/create_setcol_clause.F
      !||    create_setcol_list_g               ../starter/source/model/sets/create_setcol_clause.F
      !||    create_subm_clause                 ../starter/source/model/sets/create_subm_clause.F
      !||    create_subm_list                   ../starter/source/model/sets/create_subm_clause.F
      !||    create_subm_list_g                 ../starter/source/model/sets/create_subm_clause.F
      !||    create_subs_clause                 ../starter/source/model/sets/create_subs_clause.F
      !||    create_subs_list                   ../starter/source/model/sets/create_subs_clause.F
      !||    create_subs_list_g                 ../starter/source/model/sets/create_subs_clause.F
      !||    create_surface_from_element        ../starter/source/model/sets/create_surface_from_element.F
      !||    elem_1d_line_buffer                ../starter/source/model/sets/elem_1D_line_buffer.F
      !||    fill_gr                            ../starter/source/model/sets/fill_gr.F
      !||    fill_igr                           ../starter/source/model/sets/fill_igr.F
      !||    fill_line                          ../starter/source/model/sets/fill_gr.F
      !||    fill_surf                          ../starter/source/model/sets/fill_gr.F
      !||    fill_surf_ellipse                  ../starter/source/model/sets/fill_gr_surf_ellipse.F
      !||    fill_surf_plane                    ../starter/source/model/sets/fill_gr_surf_plane.F90
      !||    fractal_element_neighbor           ../starter/source/materials/fail/fractal/fractal_element_neighbor.F90
      !||    hm_read_inivol                     ../starter/source/initial_conditions/inivol/hm_read_inivol.F
      !||    hm_set                             ../starter/source/model/sets/hm_set.F
      !||    inivol_interf_mod                  ../starter/share/modules1/inivol_mod.F
      !||    insert_clause_in_set               ../starter/source/model/sets/insert_clause_in_set.F
      !||    lectur                             ../engine/source/input/lectur.F
      !||    line_buffer                        ../starter/source/model/sets/line_buffer.F
      !||    quad_surface_buffer                ../starter/source/model/sets/quad_surface_buffer.F
      !||    set_init                           ../starter/source/model/sets/set_init.F
      !||    set_mod                            ../starter/share/modules1/set_mod.F
      !||    shell_surface_buffer               ../starter/source/model/sets/shell_surface_buffer.F
      !||    shell_surface_buffer_remesh        ../starter/source/model/sets/shell_surface_buffer_remesh.F
      !||    solid_surface_buffer               ../starter/source/model/sets/solid_surface_buffer.F
      !||    sort_set                           ../starter/source/model/sets/sort_sets.F
      !||    st_qaprint_driver                  ../starter/source/output/qaprint/st_qaprint_driver.F
      !||    st_qaprint_set                     ../starter/source/output/qaprint/st_qaprint_set.F
      !||    surface_buffer                     ../starter/source/model/sets/surface_buffer.F
      !||    tag_node_from_part_sphcel          ../starter/source/model/sets/tag_node_from_part_sphcel.F90
      !||--- uses       -----------------------------------------------------
      !||    names_and_titles_mod               ../common_source/modules/names_and_titles_mod.F
      !||====================================================================
      MODULE SETDEF_MOD
        USE NAMES_AND_TITLES_MOD, ONLY: NCHARTITLE
C----------------------------------------------------------------------- 
#include "my_real.inc"   
c=======================================================================      
!---------
!   SET DATA STRUCTURE
!---------
c=======================================================================   
c=======================================================================   
!                               SET
c=======================================================================   
C=======================================================================
      INTEGER :: NSETS

C-----------------------------------------------------------------------
      TYPE SET_
        INTEGER   :: SET_ID               ! SET identifier
        CHARACTER(LEN=NCHARTITLE) :: TITLE ! SET title
        INTEGER   :: SET_TYPE         ! SET type      (1-GENERAL, 2-COLLECT)
        INTEGER   :: SET_ACTIV         
        INTEGER   :: KEYTYPE          ! SET KEY type (all keys)
        INTEGER   :: SURF_SET_ID      ! SET surface ID
        INTEGER   :: LINE_SET_ID      ! SET line ID
        INTEGER   :: SURF_SET_FLAG    ! SET surface activation flag
        INTEGER   :: LINE_SET_FLAG    ! SET line activation flag
        INTEGER   :: NSEG             ! SET surface segments
        INTEGER   :: NSEG_1D          ! SET line/edge segments
!==============================================================
!        INTEGER   :: LEVEL ! Hierarchy level
!                             (FLAG 'SUBLEVEL DONE' FOR SET OF SET)
!                            = 0 ---> not yet initialized
!                            = 1 ---> done
        INTEGER   :: SET_GRNOD_ID        ! SET grnod identifier
        INTEGER   :: SET_GRPART_ID       ! SET grpart identifier
        INTEGER   :: SET_GRSOLID_ID      ! SET grsolid identifier
        INTEGER   :: SET_GRSH4N_ID       ! SET grsh4n identifier
        INTEGER   :: SET_GRSH3N_ID       ! SET grsh3n identifier
        INTEGER   :: SET_GRQUAD_ID       ! SET grquad identifier
        INTEGER   :: SET_GRTRIA_ID       ! SET grtria identifier
        INTEGER   :: SET_GRBEAM_ID       ! SET grbeam identifier
        INTEGER   :: SET_GRTRUSS_ID      ! SET grtruss identifier
        INTEGER   :: SET_GRSPRING_ID     ! SET grspring identifier
        INTEGER   :: SET_NSURF_ID        ! SET grsurf identifier
        INTEGER   :: SET_NSLIN_ID        ! SET grslin identifier
!
        INTEGER   :: NB_NODE          ! SET nb of nodes
        INTEGER   :: NB_PART          ! SET nb of parts
        INTEGER   :: NB_SOLID         ! SET nb of solids
        INTEGER   :: NB_SH4N          ! SET nb of sh4n
        INTEGER   :: NB_SH3N          ! SET nb of sh3n
        INTEGER   :: NB_QUAD          ! SET nb of quads
        INTEGER   :: NB_TRIA          ! SET nb of trias
        INTEGER   :: NB_BEAM          ! SET nb of beams
        INTEGER   :: NB_TRUSS         ! SET nb of truss
        INTEGER   :: NB_SPRING        ! SET nb of spring
        INTEGER   :: HAS_SURF_SEG     ! Flag indicating if a surface was created even if NB_SURF_SEG = 0
        INTEGER   :: NB_SURF_SEG      ! SET nb of surfacce's segments
        INTEGER   :: HAS_LINE_SEG     ! Flag indicating if a line was created even if NB_SURF_SEG = 0
        INTEGER   :: NB_LINE_SEG      ! SET nb of line/edge's segments
!
        INTEGER   :: NB_RBODY         ! SET nb of Rbody
        INTEGER   :: NB_SPHCEL        ! SET nb of sphcel

        INTEGER   :: NB_ELLIPSE       ! CLAUSE nb of Ellipses within a SET (max = 1, one per /SET)
        INTEGER   :: ELLIPSE_IAD_BUFR
        INTEGER   :: ELLIPSE_ID_MADYMO
        INTEGER   :: ELLIPSE_N
        MY_REAL   :: ELLIPSE_XC
        MY_REAL   :: ELLIPSE_YC
        MY_REAL   :: ELLIPSE_ZC
        MY_REAL   :: ELLIPSE_A
        MY_REAL   :: ELLIPSE_B
        MY_REAL   :: ELLIPSE_C

        INTEGER   :: NB_PLANE       ! CLAUSE nb of Planes within a SET (max = 1, one per /SET)
        INTEGER   :: PLANE_IAD_BUFR
        MY_REAL   :: PLANE_XM
        MY_REAL   :: PLANE_YM
        MY_REAL   :: PLANE_ZM
        MY_REAL   :: PLANE_XM1
        MY_REAL   :: PLANE_YM1
        MY_REAL   :: PLANE_ZM1

        INTEGER   :: NB_NODENS      ! CLAUSE nb of NODENS -> unsortable node list
!
        INTEGER, DIMENSION(:), ALLOCATABLE  ::   NODE
        INTEGER, DIMENSION(:), ALLOCATABLE  ::   PART
        INTEGER, DIMENSION(:), ALLOCATABLE  ::   SOLID
        INTEGER, DIMENSION(:), ALLOCATABLE  ::   SH4N
        INTEGER, DIMENSION(:), ALLOCATABLE  ::   SH3N
        INTEGER, DIMENSION(:), ALLOCATABLE  ::   QUAD
        INTEGER, DIMENSION(:), ALLOCATABLE  ::   TRIA
        INTEGER, DIMENSION(:), ALLOCATABLE  ::   BEAM
        INTEGER, DIMENSION(:), ALLOCATABLE  ::   TRUSS
        INTEGER, DIMENSION(:), ALLOCATABLE  ::   SPRING
        INTEGER, DIMENSION(:), ALLOCATABLE  ::   NODENS
        INTEGER, DIMENSION(:), ALLOCATABLE  ::   SPHCEL
!
        INTEGER, DIMENSION(:), ALLOCATABLE  ::   RBODY
!
        INTEGER, DIMENSION(:,:), ALLOCATABLE  :: SURF_NODES ! dim = (NB_SURF_SEG,4)
        INTEGER, DIMENSION(:)  , ALLOCATABLE  :: SURF_ELTYP ! dim = (NB_SURF_SEG)
        INTEGER, DIMENSION(:)  , ALLOCATABLE  :: SURF_ELEM  ! dim = (NB_SURF_SEG)
!
        INTEGER, DIMENSION(:,:), ALLOCATABLE  :: LINE_NODES ! dim = (NB_LINE_SEG,2)
        INTEGER, DIMENSION(:)  , ALLOCATABLE  :: LINE_ELTYP ! dim = (NB_LINE_SEG)
        INTEGER, DIMENSION(:)  , ALLOCATABLE  :: LINE_ELEM  ! dim = (NB_LINE_SEG)

        MY_REAL, DIMENSION(:)  , ALLOCATABLE  :: ELLIPSE_SKEW
!==============================================================
!       SET HIERARCHY
        INTEGER   :: FATHER
        INTEGER   :: NCHILD
!
        INTEGER, DIMENSION(:)  , ALLOCATABLE  :: CHILD
        INTEGER, DIMENSION(:)  , ALLOCATABLE  :: CHILD_OP ! =0 add ; =1 remove
!==============================================================
!--------------
      END TYPE SET_
C-----------------------------------------------------------------------
c---------------
      END MODULE SETDEF_MOD


      !||====================================================================
      !||    set_scratch_mod                    ../common_source/modules/setdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    create_box_clause                  ../starter/source/model/sets/create_box_clause.F
      !||    create_line_from_element           ../starter/source/model/sets/create_line_from_element.F
      !||    create_line_from_surface           ../starter/source/model/sets/create_line_from_surface.F
      !||    create_line_from_surface_ext_all   ../starter/source/model/sets/create_line_from_ext_surface_ext_all.F
      !||    create_set_clause                  ../starter/source/model/sets/create_set_clause.F
      !||    create_set_collect                 ../starter/source/model/sets/create_setcol_clause.F
      !||    create_setcol_clause               ../starter/source/model/sets/create_setcol_clause.F
      !||    create_surface_from_element        ../starter/source/model/sets/create_surface_from_element.F
      !||    hm_set                             ../starter/source/model/sets/hm_set.F
      !||    insert_clause_in_set               ../starter/source/model/sets/insert_clause_in_set.F
      !||====================================================================
      MODULE SET_SCRATCH_MOD
C----------------------------------------------------------------------- 
c=======================================================================      
!---------
!   SET DATA STRUCTURE
!---------
c=======================================================================   
c=======================================================================   
!                               SET
c=======================================================================   
C=======================================================================
C-----------------------------------------------------------------------
      TYPE SET_SCRATCH
        INTEGER   :: SZ_SURF,SZ_LINE
        INTEGER, DIMENSION(:,:), ALLOCATABLE  ::  SURF
        INTEGER, DIMENSION(:,:), ALLOCATABLE  ::  LINE
!==============================================================
!--------------
      END TYPE SET_SCRATCH
C-----------------------------------------------------------------------
c---------------
      END MODULE SET_SCRATCH_MOD
