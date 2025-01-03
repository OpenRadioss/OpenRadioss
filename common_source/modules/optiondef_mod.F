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
      !||    optiondef_mod                   ../common_source/modules/optiondef_mod.F
      !||--- called by ------------------------------------------------------
      !||    addmaspart                      ../starter/source/tools/admas/addmaspart.F
      !||    bigbox                          ../starter/source/model/box/bigbox.F
      !||    bigbox2                         ../starter/source/model/box/bigbox.F
      !||    bigsbox                         ../starter/source/model/box/bigbox.F
      !||    box_surf_sh                     ../starter/source/model/box/bigbox.F
      !||    boxassem1                       ../starter/source/model/box/bigbox.F
      !||    boxassem2                       ../starter/source/model/box/bigbox.F
      !||    boxassem3                       ../starter/source/model/box/bigbox.F
      !||    boxassem4                       ../starter/source/model/box/bigbox.F
      !||    boxbox                          ../starter/source/model/box/boxbox.F
      !||    boxbox2                         ../starter/source/model/box/boxbox.F
      !||    boxboxs                         ../starter/source/model/box/boxbox.F
      !||    boxtage                         ../starter/source/model/box/bigbox.F
      !||    boxtagn                         ../starter/source/model/box/bigbox.F
      !||    create_box_clause               ../starter/source/model/sets/create_box_clause.F
      !||    create_elt_box                  ../starter/source/model/sets/create_elt_box.F
      !||    create_elt_clause               ../starter/source/model/sets/create_element_clause.F
      !||    create_node_box                 ../starter/source/model/sets/create_node_box.F
      !||    create_node_clause              ../starter/source/model/sets/create_node_clause.F
      !||    create_nodens_clause            ../starter/source/model/sets/create_nodens_clause.F90
      !||    create_rbody_box                ../starter/source/model/sets/create_rbody_box.F
      !||    create_rbody_clause             ../starter/source/model/sets/create_rbody_clause.F
      !||    elstagbox                       ../starter/source/model/box/bigbox.F
      !||    elstagboxbox                    ../starter/source/model/box/boxbox.F
      !||    elt_box                         ../starter/source/model/sets/fill_clause_elt_box.F
      !||    facebox                         ../starter/source/model/box/bigbox.F
      !||    fill_clause_elt_box             ../starter/source/model/sets/fill_clause_elt_box.F
      !||    fill_clause_node_box            ../starter/source/model/sets/fill_clause_node_box.F
      !||    fill_clause_rbody_box           ../starter/source/model/sets/fill_clause_rbody_box.F
      !||    hm_bigbox                       ../starter/source/model/box/hm_bigbox.F
      !||    hm_bigbox2                      ../starter/source/model/box/hm_bigbox2.F
      !||    hm_bigsbox                      ../starter/source/groups/hm_bigsbox.F
      !||    hm_lecgre                       ../starter/source/groups/hm_lecgre.F
      !||    hm_lecgrn                       ../starter/source/groups/hm_lecgrn.F
      !||    hm_read_admas                   ../starter/source/tools/admas/hm_read_admas.F
      !||    hm_read_box                     ../starter/source/model/box/hm_read_box.F
      !||    hm_read_inicrack                ../starter/source/initial_conditions/inicrack/hm_read_inicrack.F
      !||    hm_read_lines                   ../starter/source/groups/hm_read_lines.F
      !||    hm_read_surf                    ../starter/source/groups/hm_read_surf.F
      !||    hm_set                          ../starter/source/model/sets/hm_set.F
      !||    inicrkfill                      ../starter/source/elements/xfem/inicrkfill.F
      !||    initia                          ../starter/source/elements/initia/initia.F
      !||    lecgroup                        ../starter/source/groups/lecgroup.F
      !||    lectur                          ../engine/source/input/lectur.F
      !||    node_box                        ../starter/source/model/sets/fill_clause_node_box.F
      !||    option_mod                      ../starter/share/modules1/options_mod.F
      !||    printgroup                      ../starter/source/output/outfile/printgroup.F
      !||    rbody_box                       ../starter/source/model/sets/fill_clause_rbody_box.F
      !||    read_box_box                    ../starter/source/model/box/read_box_box.F
      !||    read_box_cyl                    ../starter/source/model/box/read_box_cyl.F
      !||    read_box_rect                   ../starter/source/model/box/read_box_rect.F
      !||    read_box_spher                  ../starter/source/model/box/read_box_spher.F
      !||    read_clause_box                 ../starter/source/model/sets/read_clause_box.F
      !||    sboxboxsurf                     ../starter/source/model/box/bigbox.F
      !||    simple_elt_box                  ../starter/source/model/sets/simpl_elt_box.F
      !||    simple_node_box                 ../starter/source/model/sets/simple_node_box.F
      !||    simple_rbody_box                ../starter/source/model/sets/simple_rbody_box.F
      !||    spmd_msin_addmass               ../starter/source/elements/initia/spmd_msin_addmass.F
      !||    st_qaprint_driver               ../starter/source/output/qaprint/st_qaprint_driver.F
      !||    st_qaprint_initial_conditions   ../starter/source/output/qaprint/st_qaprint_initial_conditions.F
      !||    st_qaprint_model_tools          ../starter/source/output/qaprint/st_qaprint_model_tools.F
      !||--- uses       -----------------------------------------------------
      !||    names_and_titles_mod            ../common_source/modules/names_and_titles_mod.F
      !||====================================================================
      MODULE OPTIONDEF_MOD
!----------------------------------------------------------------------- 
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARTITLE
!----------------------------------------------------------------------- 
#include "my_real.inc"
!-----------------------------------------------------------------------
!--------------
!--------------
!      /INICRACK
!--------------
!--------------
      TYPE SEG_
        INTEGER, DIMENSION(:),ALLOCATABLE :: NODES   ! - 2 nodes of the segment for /SURF
        my_real   :: RATIO ! - INICRACK RATIO OF ONE SEGMENT
      END TYPE SEG_
!--------------
      TYPE INICRACK_
        INTEGER   :: ID ! - INICRACK IDENTIFIER
        CHARACTER(LEN=NCHARTITLE) :: TITLE ! INICRACK title
        INTEGER   :: NSEG  ! - INICRACK NUMBER OF SEGMENTS
!
        TYPE (SEG_) ,DIMENSION(:) ,ALLOCATABLE :: SEG     ! SEGMENTS OF INICRACK
!--------------
      END TYPE INICRACK_
!-----------------------------------------------------------------------
!--------------
!--------------
!      /ADMAS
!--------------
!--------------
      TYPE PART_
        my_real   :: RPMAS ! ADMAS added to PARTS (or NODES)
      END TYPE PART_
!--------------
      TYPE ADMAS_
        INTEGER   :: ID ! - ADMAS IDENTIFIER
        CHARACTER(LEN=NCHARTITLE) :: TITLE ! ADMAS title
        INTEGER   :: TYPE ! ADMAS type
!                    = 0 ! Mass is added to each node of node group
!                    = 1 ! Mass/N is added to each node of node group.
!                          N being the total number of nodes in the node group
!                    = 2 ! Mass/Area - additional surface mass applied on the shell area
!                    = 3 ! Additional mass distributed on the part-group
!                    = 4 ! Final mass distributed on the part-group
!                    = 5 ! Mass is added to each single node
!                    = 6 ! Additional mass distributed on each single part
!                    = 7 ! Final mass distributed on each single part
        INTEGER   :: WEIGHT_FLAG  ! Flag to switch between area weighted distribution and volume 
!                                   weighted distribution of the added mass to parts)
!                    = 0 ! Volume weighted distribution (shell and solid parts) (default)
!                    = 1 ! Area weighted distribution (shell parts only)
        INTEGER   :: NPART ! NUMBER of parts to get ADMAS
        INTEGER, DIMENSION(:),ALLOCATABLE :: PARTID ! PART_ID to get ADMAS
        TYPE (PART_) ,DIMENSION(:) ,ALLOCATABLE :: PART  ! PARTS get ADMAS
      END TYPE ADMAS_
!-----------------------------------------------------------------------
!--------------
!      /BOX
!--------------
!--------------
      TYPE BOX_
        INTEGER   :: ID ! - BOX IDENTIFIER
        CHARACTER(LEN=NCHARTITLE) :: TITLE ! BOX title
        INTEGER   :: ISKBOX ! BOX SKEW_ID (RECTA + CYLIN)
        INTEGER   :: NOD1 ! FIRST NODE for box limit definition  - N1 -
        INTEGER   :: NOD2 ! SECOND NODE for box limit definition - N2 -
        INTEGER   :: TYPE ! BOX SHAPE (1='RECTA',2='CYLIN' ,3='SPHER')
        INTEGER   :: NBLEVELS ! TEMPORARY LEVEL NB OF BOXES
        INTEGER   :: LEVEL ! FLAG "SUBLEVEL DONE" FOR BOX OF BOXES
        INTEGER   :: ACTIBOX ! FLAG FOR ACTIVATED BOX FOR (GRNOD,GRSHEL,LINE,SURF...)
        INTEGER   :: SURFIAD !  temporary address for solid external surface (in box)
        INTEGER   :: BOXIAD !  temporary address
        my_real   :: DIAM ! BOX diameter (CYLIN + SPHER)
        my_real   :: X1 ! coord. for  - N1 -
        my_real   :: Y1 ! coord. for  - N1 -
        my_real   :: Z1 ! coord. for  - N1 -
        my_real   :: X2 ! coord. for  - N2 -
        my_real   :: Y2 ! coord. for  - N2 -
        my_real   :: Z2 ! coord. for  - N2 -
        INTEGER   :: NBOXBOX ! NUMBER OF SUB BOXES (BOXES OF BOXES)
        INTEGER ,DIMENSION(:) ,ALLOCATABLE :: IBOXBOX ! SUB BOXES (BOX OF BOXES)
        INTEGER   :: NENTITY ! NUMBER OF BOX ENTITIES (NODES,ELEMS,LINES,SURF)
!                              WITHIN ACTIVATED BOX
        INTEGER ,DIMENSION(:) ,ALLOCATABLE :: ENTITY ! BOX entity storage
      END TYPE BOX_
!--------------
      END MODULE OPTIONDEF_MOD
