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
Chd|  OPTIONDEF_MOD                 modules/optiondef_mod.F       
Chd|-- called by -----------
Chd|        ADDMASPART                    starter/source/tools/admas/addmaspart.F
Chd|        BIGBOX                        starter/source/model/box/bigbox.F
Chd|        BIGBOX2                       starter/source/model/box/bigbox.F
Chd|        BIGSBOX                       starter/source/model/box/bigbox.F
Chd|        BOXASSEM1                     starter/source/model/box/bigbox.F
Chd|        BOXASSEM2                     starter/source/model/box/bigbox.F
Chd|        BOXASSEM3                     starter/source/model/box/bigbox.F
Chd|        BOXASSEM4                     starter/source/model/box/bigbox.F
Chd|        BOXBOX                        starter/source/model/box/boxbox.F
Chd|        BOXBOX2                       starter/source/model/box/boxbox.F
Chd|        BOXBOXS                       starter/source/model/box/boxbox.F
Chd|        BOXTAGE                       starter/source/model/box/bigbox.F
Chd|        BOXTAGN                       starter/source/model/box/bigbox.F
Chd|        BOX_SURF_SH                   starter/source/model/box/bigbox.F
Chd|        CREATE_BOX_CLAUSE             starter/source/model/sets/create_box_clause.F
Chd|        CREATE_ELT_BOX                starter/source/model/sets/create_elt_box.F
Chd|        CREATE_ELT_CLAUSE             starter/source/model/sets/create_element_clause.F
Chd|        CREATE_NODE_BOX               starter/source/model/sets/create_node_box.F
Chd|        CREATE_NODE_CLAUSE            starter/source/model/sets/create_node_clause.F
Chd|        CREATE_RBODY_BOX              starter/source/model/sets/create_rbody_box.F
Chd|        CREATE_RBODY_CLAUSE           starter/source/model/sets/create_rbody_clause.F
Chd|        ELSTAGBOX                     starter/source/model/box/bigbox.F
Chd|        ELSTAGBOXBOX                  starter/source/model/box/boxbox.F
Chd|        FACEBOX                       starter/source/model/box/bigbox.F
Chd|        FILL_CLAUSE_ELT_BOX           starter/source/model/sets/fill_clause_elt_box.F
Chd|        FILL_CLAUSE_NODE_BOX          starter/source/model/sets/fill_clause_node_box.F
Chd|        FILL_CLAUSE_RBODY_BOX         starter/source/model/sets/fill_clause_rbody_box.F
Chd|        HM_BIGBOX                     starter/source/model/box/hm_bigbox.F
Chd|        HM_BIGBOX2                    starter/source/model/box/hm_bigbox2.F
Chd|        HM_BIGSBOX                    starter/source/groups/hm_bigsbox.F
Chd|        HM_LECGRE                     starter/source/groups/hm_lecgre.F
Chd|        HM_LECGRN                     starter/source/groups/hm_lecgrn.F
Chd|        HM_READ_ADMAS                 starter/source/tools/admas/hm_read_admas.F
Chd|        HM_READ_BOX                   starter/source/model/box/hm_read_box.F
Chd|        HM_READ_INICRACK              starter/source/initial_conditions/inicrack/hm_read_inicrack.F
Chd|        HM_READ_LINES                 starter/source/groups/hm_read_lines.F
Chd|        HM_READ_SURF                  starter/source/groups/hm_read_surf.F
Chd|        HM_SET                        starter/source/model/sets/hm_set.F
Chd|        INICRKFILL                    starter/source/elements/xfem/inicrkfill.F
Chd|        INITIA                        starter/source/elements/initia/initia.F
Chd|        LECGROUP                      starter/source/groups/lecgroup.F
Chd|        LECTUR                        starter/source/starter/lectur.F
Chd|        PRINTGROUP                    starter/source/output/outfile/printgroup.F
Chd|        READ_BOX_BOX                  starter/source/model/box/read_box_box.F
Chd|        READ_BOX_CYL                  starter/source/model/box/read_box_cyl.F
Chd|        READ_BOX_RECT                 starter/source/model/box/read_box_rect.F
Chd|        READ_BOX_SPHER                starter/source/model/box/read_box_spher.F
Chd|        READ_CLAUSE_BOX               starter/source/model/sets/read_clause_box.F
Chd|        SBOXBOXSURF                   starter/source/model/box/bigbox.F
Chd|        SIMPLE_ELT_BOX                starter/source/model/sets/simpl_elt_box.F
Chd|        SIMPLE_NODE_BOX               starter/source/model/sets/simple_node_box.F
Chd|        SIMPLE_RBODY_BOX              starter/source/model/sets/simple_rbody_box.F
Chd|        SPMD_MSIN_ADDMASS             starter/source/elements/initia/spmd_msin_addmass.F
Chd|        ST_QAPRINT_DRIVER             starter/source/output/qaprint/st_qaprint_driver.F
Chd|        ST_QAPRINT_INITIAL_CONDITIONS starter/source/output/qaprint/st_qaprint_initial_conditions.F
Chd|        ST_QAPRINT_MODEL_TOOLS        starter/source/output/qaprint/st_qaprint_model_tools.F
Chd|        OPTION_MOD                    starter/share/modules1/options_mod.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE OPTIONDEF_MOD
!-----------------------------------------------------------------------
#include "nchar_c.inc"
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
        CHARACTER :: TITLE*nchartitle ! INICRACK title
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
        CHARACTER :: TITLE*nchartitle ! ADMAS title
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
        CHARACTER :: TITLE*nchartitle ! BOX title
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
