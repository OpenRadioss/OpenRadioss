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
Chd|  INTER_STRUCT_MOD              share/modules/inter_struct_mod.F
Chd|-- called by -----------
Chd|        INTER_CELL_COLOR              source/interfaces/generic/inter_cell_color.F
Chd|        INTER_CHECK_SORT              source/interfaces/generic/inter_check_sort.F
Chd|        INTER_COLOR_VOXEL             source/interfaces/generic/inter_color_voxel.F
Chd|        INTER_COUNT_NODE_CURV         source/interfaces/generic/inter_count_node_curv.F
Chd|        INTER_DEALLOCATE_WAIT         source/interfaces/generic/inter_deallocate_wait.F
Chd|        INTER_MINMAX_NODE             source/interfaces/generic/inter_minmax_node.F
Chd|        INTER_PREPARE_SORT            source/interfaces/generic/inter_prepare_sort.F
Chd|        INTER_SORT                    source/interfaces/generic/inter_sort.F
Chd|        INTER_SORT_07                 source/interfaces/int07/inter_sort_07.F
Chd|        INTER_STRUCT_INIT             source/interfaces/generic/inter_struct_init.F
Chd|        INTER_TRC_7                   source/interfaces/int07/inter_trc_7.F
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        SPMD_BOX_LIMIT_REDUCTION      source/mpi/interfaces/spmd_box_limit_reduction.F
Chd|        SPMD_CELL_EXCHANGE            source/mpi/generic/spmd_cell_exchange.F
Chd|        SPMD_CELL_LIST_EXCHANGE       source/mpi/interfaces/spmd_cell_list_exchange.F
Chd|        SPMD_CELL_SIZE_EXCHANGE_INIT  source/mpi/interfaces/spmd_cell_size_exchange_init.F
Chd|        SPMD_GET_INACTI_GLOBAL        source/mpi/interfaces/spmd_get_inacti_global.F
Chd|        SPMD_SPLIT_COMM_INTER         source/mpi/interfaces/spmd_split_comm_inter.F
Chd|-- calls ---------------
Chd|====================================================================
        MODULE INTER_STRUCT_MOD
#include      "my_real.inc"
!$COMMENT
!       INTER_STRUCT_MOD :
!              create the inter_struct_type
!                NMN_G : integer ; total number of main node of the interface
!                I_SK_OLD : integer ;
!                INACTI,INACTII : integer ; inacti option
!                CURV_MAX_MAX : float ; max value of CURV_MAX
!                CURV_MAX : float array ;
!                SIZE_CAND_A : integer ; size of cand_a array
!                CAND_A : integer array ;
!                NSNFIOLD : integer array ; old value of NSNFI
!                BOX_LIMIT_MAIN : float array ; min&max position of main nodes
!$ENDCOMMENT
            TYPE inter_struct_type
                INTEGER :: NMN_G
                INTEGER :: I_SK_OLD
                INTEGER :: INACTI,INACTII
                my_real :: CURV_MAX_MAX
                my_real, DIMENSION(:), ALLOCATABLE :: CURV_MAX
                INTEGER :: SIZE_CAND_A      !   size of cand_a array
                INTEGER, DIMENSION(:), ALLOCATABLE :: CAND_A
                INTEGER, DIMENSION(:), ALLOCATABLE :: NSNFIOLD
                my_real, DIMENSION(12) :: BOX_LIMIT_MAIN
            END TYPE inter_struct_type
        END MODULE INTER_STRUCT_MOD



