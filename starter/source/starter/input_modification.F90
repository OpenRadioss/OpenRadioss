!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||====================================================================
!||    input_modification_mod   ../starter/source/starter/input_modification.F90
!||--- called by ------------------------------------------------------
!||    contrl                   ../starter/source/starter/contrl.F
!||    hm_s_ale                 ../starter/source/devtools/hm_reader/hm_s_ale.F90
!||    lectur                   ../starter/source/starter/lectur.F
!||    s_ale_message            ../starter/source/ale/s_ale_message.F90
!||    starter0                 ../starter/source/starter/starter0.F
!||====================================================================
      module input_modification_mod
        implicit none
        ! --------------------------------
        ! Structured ALE options
        type s_ale_
          character(len=100) :: title !< s-ale title
          integer :: mesh_id !< s-ale mesh id
          integer :: part_id !< part id related to the s-ale option (if any, 0 otherwise)
          integer :: control_point_id(3) !< control point id
          integer :: sub_area_control_point_nb(3) !< number of sub-area of control point
          integer :: kind_of_mesh(3) !< kind of mesh (regular of refined)
          integer :: trimming_nb !< number of trimming
          integer :: element_nb(3) !< number of element in each direction (without trimming)
          integer :: created_elements_nb !< total number of created elements
          integer :: node_nb(3) !< number of node in each direction (without trimming)
          integer :: created_nodes_nb !< total number of created nodes
          integer :: boundary_id(6) !< boundary condition id
          integer :: bcs_id(6) !< /BCS/ boundary condition id
          integer :: boundary_type(6) !< boundary condition type
          integer :: error !< id of the error if the s-ale option is not correct, 0 otherwise
          integer :: material_kind !< material type
          integer :: material_id !< material id
          integer :: element_offset !< element offset for the created elements
          integer :: node_offset !< node offset for the created nodes    
          integer :: reference_node_id !< reference node id
          integer :: link_vel_id !< /ALE/LINK/VEL id if reference node exists
        end type  s_ale_
        ! --------------------------------
        
        type input_modif_
          integer :: s_ale_nb !< number of s-ale options
          type(s_ale_), dimension(:), allocatable :: s_ale !< s-ale options
        end type input_modif_

      end module input_modification_mod
