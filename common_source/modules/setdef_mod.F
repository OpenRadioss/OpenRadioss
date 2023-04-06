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
Chd|  SETDEF_MOD                    modules/setdef_mod.F          
Chd|-- called by -----------
Chd|        CHECK_ELTYP                   starter/source/model/sets/check_eltyp.F
Chd|        CLAUSE_INIT                   starter/source/model/sets/clause_init.F
Chd|        CONTRL                        starter/source/starter/contrl.F
Chd|        COPY_LIST_IN_CAUSE            starter/source/model/sets/create_element_clause.F
Chd|        CREATE_BOX_CLAUSE             starter/source/model/sets/create_box_clause.F
Chd|        CREATE_ELEMENT_FROM_PART      starter/source/model/sets/create_element_from_part.F
Chd|        CREATE_ELEM_ALL_CLAUSE        starter/source/model/sets/create_elem_all_clause.F
Chd|        CREATE_ELT_BOX                starter/source/model/sets/create_elt_box.F
Chd|        CREATE_ELT_CLAUSE             starter/source/model/sets/create_element_clause.F
Chd|        CREATE_ELT_LIST               starter/source/model/sets/create_element_clause.F
Chd|        CREATE_ELT_LIST_G             starter/source/model/sets/create_element_clause.F
Chd|        CREATE_LINE_FROM_ELEMENT      starter/source/model/sets/create_line_from_element.F
Chd|        CREATE_LINE_FROM_SURFACE      starter/source/model/sets/create_line_from_surface.F
Chd|        CREATE_LINE_FROM_SURFACE_ALL  starter/source/model/sets/create_line_from_surface_all.F
Chd|        CREATE_LINE_FROM_SURFACE_EXT  starter/source/model/sets/create_line_from_surface_ext.F
Chd|        CREATE_MAP_TABLES             starter/source/model/sets/map_tables.F
Chd|        CREATE_NODE_ALL_CLAUSE        starter/source/model/sets/create_node_all_clause.F
Chd|        CREATE_NODE_BOX               starter/source/model/sets/create_node_box.F
Chd|        CREATE_NODE_CLAUSE            starter/source/model/sets/create_node_clause.F
Chd|        CREATE_NODE_FROM_ELEMENT      starter/source/model/sets/create_node_from_element.F
Chd|        CREATE_NODE_FROM_RBODY        starter/source/model/sets/create_node_from_rbody.F
Chd|        CREATE_NODE_FROM_SEG          starter/source/model/sets/create_node_from_seg.F
Chd|        CREATE_NODE_LIST              starter/source/model/sets/create_node_clause.F
Chd|        CREATE_NODE_LIST_G            starter/source/model/sets/create_node_clause.F
Chd|        CREATE_PART_ALL_CLAUSE        starter/source/model/sets/create_part_all_clause.F
Chd|        CREATE_PART_CLAUSE            starter/source/model/sets/create_part_clause.F
Chd|        CREATE_PART_LIST              starter/source/model/sets/create_part_clause.F
Chd|        CREATE_PART_LIST_G            starter/source/model/sets/create_part_clause.F
Chd|        CREATE_RBODY_BOX              starter/source/model/sets/create_rbody_box.F
Chd|        CREATE_RBODY_CLAUSE           starter/source/model/sets/create_rbody_clause.F
Chd|        CREATE_RBODY_LIST             starter/source/model/sets/create_rbody_clause.F
Chd|        CREATE_RBODY_LIST_G           starter/source/model/sets/create_rbody_clause.F
Chd|        CREATE_SEG_CLAUSE             starter/source/model/sets/create_seg_clause.F
Chd|        CREATE_SETCOL_ARRAY           starter/source/model/sets/create_setcol_clause.F
Chd|        CREATE_SETCOL_CLAUSE          starter/source/model/sets/create_setcol_clause.F
Chd|        CREATE_SETCOL_LIST            starter/source/model/sets/create_setcol_clause.F
Chd|        CREATE_SETCOL_LIST_G          starter/source/model/sets/create_setcol_clause.F
Chd|        CREATE_SET_ARRAY              starter/source/model/sets/create_set_clause.F
Chd|        CREATE_SET_CLAUSE             starter/source/model/sets/create_set_clause.F
Chd|        CREATE_SET_COLLECT            starter/source/model/sets/create_setcol_clause.F
Chd|        CREATE_SET_LIST               starter/source/model/sets/create_set_clause.F
Chd|        CREATE_SET_LIST_G             starter/source/model/sets/create_set_clause.F
Chd|        CREATE_SUBM_CLAUSE            starter/source/model/sets/create_subm_clause.F
Chd|        CREATE_SUBM_LIST              starter/source/model/sets/create_subm_clause.F
Chd|        CREATE_SUBM_LIST_G            starter/source/model/sets/create_subm_clause.F
Chd|        CREATE_SUBS_CLAUSE            starter/source/model/sets/create_subs_clause.F
Chd|        CREATE_SUBS_LIST              starter/source/model/sets/create_subs_clause.F
Chd|        CREATE_SUBS_LIST_G            starter/source/model/sets/create_subs_clause.F
Chd|        CREATE_SURFACE_FROM_ELEMENT   starter/source/model/sets/create_surface_from_element.F
Chd|        ELEM_1D_LINE_BUFFER           starter/source/model/sets/elem_1D_line_buffer.F
Chd|        FILL_GR                       starter/source/model/sets/fill_gr.F
Chd|        FILL_IGR                      starter/source/model/sets/fill_igr.F
Chd|        FILL_LINE                     starter/source/model/sets/fill_gr.F
Chd|        FILL_SURF                     starter/source/model/sets/fill_gr.F
Chd|        HM_READ_INIVOL                starter/source/initial_conditions/inivol/hm_read_inivol.F
Chd|        HM_SET                        starter/source/model/sets/hm_set.F
Chd|        INSERT_CLAUSE_IN_SET          starter/source/model/sets/insert_clause_in_set.F
Chd|        LINE_BUFFER                   starter/source/model/sets/line_buffer.F
Chd|        QUAD_SURFACE_BUFFER           starter/source/model/sets/quad_surface_buffer.F
Chd|        SET_INIT                      starter/source/model/sets/set_init.F
Chd|        SHELL_SURFACE_BUFFER          starter/source/model/sets/shell_surface_buffer.F
Chd|        SHELL_SURFACE_BUFFER_REMESH   starter/source/model/sets/shell_surface_buffer_remesh.F
Chd|        SOLID_SURFACE_BUFFER          starter/source/model/sets/solid_surface_buffer.F
Chd|        SORT_SET                      starter/source/model/sets/sort_sets.F
Chd|        ST_QAPRINT_DRIVER             starter/source/output/qaprint/st_qaprint_driver.F
Chd|        ST_QAPRINT_SET                starter/source/output/qaprint/st_qaprint_set.F
Chd|        SURFACE_BUFFER                starter/source/model/sets/surface_buffer.F
Chd|        SET_MOD                       starter/share/modules1/set_mod.F
Chd|        CREATE_LINE_FROM_SURFACE_EXT_ALLstarter/source/model/sets/create_line_from_ext_surface_ext_all.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE SETDEF_MOD
C----------------------------------------------------------------------- 
#include "nchar_c.inc"    
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
        CHARACTER :: TITLE*nchartitle ! SET title
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


Chd|====================================================================
Chd|  SET_SCRATCH_MOD               modules/setdef_mod.F          
Chd|-- called by -----------
Chd|        CREATE_BOX_CLAUSE             starter/source/model/sets/create_box_clause.F
Chd|        CREATE_LINE_FROM_ELEMENT      starter/source/model/sets/create_line_from_element.F
Chd|        CREATE_LINE_FROM_SURFACE      starter/source/model/sets/create_line_from_surface.F
Chd|        CREATE_SETCOL_CLAUSE          starter/source/model/sets/create_setcol_clause.F
Chd|        CREATE_SET_CLAUSE             starter/source/model/sets/create_set_clause.F
Chd|        CREATE_SET_COLLECT            starter/source/model/sets/create_setcol_clause.F
Chd|        CREATE_SURFACE_FROM_ELEMENT   starter/source/model/sets/create_surface_from_element.F
Chd|        HM_SET                        starter/source/model/sets/hm_set.F
Chd|        INSERT_CLAUSE_IN_SET          starter/source/model/sets/insert_clause_in_set.F
Chd|        CREATE_LINE_FROM_SURFACE_EXT_ALLstarter/source/model/sets/create_line_from_ext_surface_ext_all.F
Chd|-- calls ---------------
Chd|====================================================================
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
