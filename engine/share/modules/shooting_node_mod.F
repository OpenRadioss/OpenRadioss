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
Chd|  RBODY_MOD                     share/modules/rbody_mod.F     
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RESOL_HEAD                    source/engine/resol_head.F    
Chd|        RESTALLOC                     source/output/restart/arralloc.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE SHOOTING_NODE_MOD
!$COMMENT
!       SHOOTING_NODE_MOD :
!$ENDCOMMENT
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
        TYPE working_array
            INTEGER, DIMENSION(:), ALLOCATABLE :: SURF  ! working array for surface
            INTEGER, DIMENSION(:), ALLOCATABLE :: PROC  ! working array for processor            
        END TYPE working_array

        TYPE remote_elm
            INTEGER, DIMENSION(:), ALLOCATABLE :: REMOTE_ELM_M  ! number of remote element for the main edges
            INTEGER, DIMENSION(:), ALLOCATABLE :: REMOTE_ELM_S  ! number of remote element for the secondary edges
        END TYPE remote_elm

        TYPE shooting_node_type 
            ! ------------------------------
            ! secondary node array
            INTEGER :: SIZE_SEC_NODE ! size of INTER_SEC_NODE & SEC_NODE_ID
            INTEGER, DIMENSION(:), ALLOCATABLE :: SHIFT_S_NODE ! shift to point to INTER_SEC_NODE/SEC_NODE_ID arrays & number of interface per node
            INTEGER, DIMENSION(:), ALLOCATABLE :: INTER_SEC_NODE ! list of interface of the nodes
            INTEGER, DIMENSION(:), ALLOCATABLE :: SEC_NODE_ID ! ID of secondary nodes in each interface
            ! ------------------------------
            ! main node array
            INTEGER :: MAX_PROC_NB      ! mamimun number of processor
            INTEGER :: SIZE_M_NODE_PROC ! size of M_NODE_PROC
            INTEGER, DIMENSION(:), ALLOCATABLE :: SHIFT_M_NODE_PROC ! shift to point to M_NODE_PROC array & number of processor per node
            INTEGER, DIMENSION(:), ALLOCATABLE :: M_NODE_PROC       ! list of processor per node

            INTEGER :: MAX_SURF_NB      ! mamimun number of surface
            INTEGER :: SIZE_M_NODE_SURF ! size of M_NODE_SURF
            INTEGER, DIMENSION(:), ALLOCATABLE :: SHIFT_M_NODE_SURF ! shift to point to M_NODE_SURF array & number of surface per node
            INTEGER, DIMENSION(:), ALLOCATABLE :: M_NODE_SURF       ! list of surface per node

            INTEGER :: MAX_EDGE_NB      ! mamimun number of edge
            INTEGER :: SIZE_M_NODE_EDGE ! size of M_NODE_EDGE
            INTEGER :: SIZE_S_NODE_EDGE ! size of S_NODE_EDGE
            INTEGER, DIMENSION(:), ALLOCATABLE :: SHIFT_M_NODE_EDGE ! shift to point to M_NODE_EDGE array & number of edge per node
            INTEGER, DIMENSION(:), ALLOCATABLE :: SHIFT_S_NODE_EDGE ! shift to point to M_NODE_EDGE array & number of edge per node
            INTEGER, DIMENSION(:), ALLOCATABLE :: M_NODE_EDGE       ! list of edge per node (M node)
            INTEGER, DIMENSION(:), ALLOCATABLE :: S_NODE_EDGE       ! list of edge per node (S node)

            INTEGER, DIMENSION(:,:), ALLOCATABLE :: SHIFT_INTERFACE   ! shift for surface ID
            ! ------------------------------
            ! working array
            INTEGER, DIMENSION(:), ALLOCATABLE :: GLOBAL_NB_ELEM_OFF    ! number of deactivated element for each omp thread
            INTEGER :: S_GLOBAL_ELEM_INDEX  ! size of "GLOBAL_ELEM_IND" array
            INTEGER, DIMENSION(:), ALLOCATABLE :: GLOBAL_ELEM_INDEX     ! list of deactivated element

            INTEGER :: S_SAVE_SURFACE   ! size of SAVE_SURFACE array
            INTEGER, DIMENSION(:), ALLOCATABLE :: SAVE_SURFACE ! list of deativated surface
            INTEGER :: SAVE_SURFACE_NB ! numner of deactivated surface

            INTEGER :: S_SAVE_PROC   ! size of SAVE_PROC array
            INTEGER, DIMENSION(:), ALLOCATABLE :: SAVE_PROC ! list of processor with a deativated surface
            INTEGER :: SAVE_PROC_NB ! number of processor + the 4 nodes associated to the surfaces

            INTEGER :: S_SAVE_M_EDGE,S_SAVE_S_EDGE   ! size of SAVE_M_EDGE & SAVE_S_EDGE array
            INTEGER, DIMENSION(:), ALLOCATABLE :: SAVE_M_EDGE ! list of deativated edge : main node
            INTEGER, DIMENSION(:), ALLOCATABLE :: SAVE_S_EDGE ! list of deativated edge : secondary node
            INTEGER :: SAVE_M_EDGE_NB,SAVE_S_EDGE_NB ! number of deactivated edge

            INTEGER :: S_SAVE_PROC_EDGE   ! size of SAVE_PROC_EDGE array
            INTEGER, DIMENSION(:), ALLOCATABLE :: SAVE_PROC_EDGE ! list of processor with a deativated edge
            INTEGER :: SAVE_PROC_NB_EDGE ! number of processor + the 4 nodes associated to the surfaces


            TYPE(remote_elm), DIMENSION(:), ALLOCATABLE :: INTER
        END TYPE shooting_node_type 


C-----------------------------------------------
      END MODULE SHOOTING_NODE_MOD
