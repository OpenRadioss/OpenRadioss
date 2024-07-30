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
      !||    inter_struct_mod               ../engine/share/modules/inter_struct_mod.F
      !||--- called by ------------------------------------------------------
      !||    inter_cell_color               ../engine/source/interfaces/generic/inter_cell_color.F
      !||    inter_check_sort               ../engine/source/interfaces/generic/inter_check_sort.F
      !||    inter_color_voxel              ../engine/source/interfaces/generic/inter_color_voxel.F
      !||    inter_count_node_curv          ../engine/source/interfaces/generic/inter_count_node_curv.F
      !||    inter_deallocate_wait          ../engine/source/interfaces/generic/inter_deallocate_wait.F
      !||    inter_minmax_node              ../engine/source/interfaces/generic/inter_minmax_node.F
      !||    inter_prepare_sort             ../engine/source/interfaces/generic/inter_prepare_sort.F
      !||    inter_sort                     ../engine/source/interfaces/generic/inter_sort.F
      !||    inter_sort_07                  ../engine/source/interfaces/int07/inter_sort_07.F
      !||    inter_struct_init              ../engine/source/interfaces/generic/inter_struct_init.F
      !||    inter_trc_7                    ../engine/source/interfaces/int07/inter_trc_7.F
      !||    inttri                         ../engine/source/interfaces/intsort/inttri.F
      !||    resol                          ../engine/source/engine/resol.F
      !||    spmd_box_limit_reduction       ../engine/source/mpi/interfaces/spmd_box_limit_reduction.F
      !||    spmd_cell_exchange             ../engine/source/mpi/generic/spmd_cell_exchange.F
      !||    spmd_cell_list_exchange        ../engine/source/mpi/interfaces/spmd_cell_list_exchange.F
      !||    spmd_cell_size_exchange_init   ../engine/source/mpi/interfaces/spmd_cell_size_exchange_init.F
      !||    spmd_get_inacti_global         ../engine/source/mpi/interfaces/spmd_get_inacti_global.F
      !||    spmd_split_comm_inter          ../engine/source/mpi/interfaces/spmd_split_comm_inter.F
      !||====================================================================
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



