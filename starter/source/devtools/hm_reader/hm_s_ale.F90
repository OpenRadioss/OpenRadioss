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
!||    hm_s_ale_mod   ../starter/source/devtools/hm_reader/hm_s_ale.F90
!||--- called by ------------------------------------------------------
!||    contrl         ../starter/source/starter/contrl.F
!||====================================================================
      module hm_s_ale_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief S-ALE card reader
!! \details This subroutine reads the S-ALE card from the input file & modifies the input
! ======================================================================================================================
!||====================================================================
!||    hm_s_ale                 ../starter/source/devtools/hm_reader/hm_s_ale.F90
!||--- called by ------------------------------------------------------
!||    contrl                   ../starter/source/starter/contrl.F
!||--- calls      -----------------------------------------------------
!||    hm_option_count          ../starter/source/devtools/hm_reader/hm_option_count.F
!||    hm_option_read_key       ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start          ../starter/source/devtools/hm_reader/hm_option_start.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    input_modification_mod   ../starter/source/starter/input_modification.F90
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_s_ale(nsubmod,lsubmodel,input_modification)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use message_mod
          use hm_option_read_mod
          use names_and_titles_mod, only : nchartitle,ncharline
          use submodel_mod , only : submodel_data
          use input_modification_mod, only : input_modif_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nsubmod !< number of submodel
          type(submodel_data), intent(in) :: lsubmodel(nsubmod) !< submodel data
          type(input_modif_), intent(inout) :: input_modification !< input modification data structure          
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,next
          integer :: s_ale_nb ! number of s ale option
          integer :: s_ale_id   ! S ale option identifier
          logical :: is_available
          character(len=nchartitle) :: titr
          character(len=ncharline) :: key1,key2,key3
          integer, parameter :: message_size = 45
          integer, dimension(message_size) :: message_value ! 1 : s-ale mesh id
                                                  ! 2-->4 : control point id
                                                  ! 5-->7 : number of sub-area of control point
                                                  ! 8-->10: kind of mesh (regular of refined)
                                                  ! 11 : number of trimming
                                                  ! 12-->14 : number of element in each direction (without trimming)
                                                  ! 15 : total number of created elements
                                                  ! 16-->18 : number of node in each direction (without trimming)
                                                  ! 19 : total number of created nodes
                                                  ! 20-->25: boundary condition id
                                                  ! 26-->31: /BCS boundary condition id
                                                  ! 32-->37: boundary condition type
                                                  ! 38 : not ok, error id
                                                  ! 39 : kind of law
                                                  ! 40 : material id
                                                  ! 41 : element offset
                                                  ! 42 : node offset
                                                  ! 43 : s-ale mesh id
                                                  ! 44 : reference node id
                                                  ! 45 : /ALE/LINK/VEL id if reference node exists

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          is_available = .false.
          call hm_option_count('/ALE/STRUCTURED_MESH', s_ale_nb)
          call hm_option_start('/ALE/STRUCTURED_MESH')
          next = 0
          do i=1,s_ale_nb
            titr = ''
            key1  = ''
            key2  = ''
            key3  = ''            
            call hm_option_read_key(lsubmodel,option_titr=titr,keyword2=key2,keyword3=key3)
            if(len_trim(key3)==0) then
              next = next + 1 ! count the number of s-ale option
            endif
          enddo
          input_modification%s_ale_nb = next
          allocate(input_modification%s_ale(next))

          call hm_option_start('/ALE/STRUCTURED_MESH')          
          next = 0
          do i=1,s_ale_nb
            titr = ''
            key1  = ''
            key2  = ''
            key3  = ''            
            call hm_option_read_key(lsubmodel,option_titr=titr,keyword2=key2,keyword3=key3)
            message_value(1:message_size) = 0
            message_value(1) = 0 ! if =1 --> activation of debug messages in the hm reader
            if(len_trim(key3)==0) then
              call cpp_sale_mesh_create(message_value)
              next = next + 1
              input_modification%s_ale(next)%title(1:100) = ''
              input_modification%s_ale(next)%title(1:len_trim(titr)) = titr(1:len_trim(titr))
              input_modification%s_ale(next)%mesh_id = message_value(43)
              input_modification%s_ale(next)%part_id = message_value(1)
              input_modification%s_ale(next)%control_point_id(1:3) = message_value(2:4)
              input_modification%s_ale(next)%sub_area_control_point_nb(1:3) = message_value(5:7)
              input_modification%s_ale(next)%kind_of_mesh(1:3) = message_value(8:10)
              input_modification%s_ale(next)%trimming_nb = message_value(11)
              input_modification%s_ale(next)%element_nb(1:3) = message_value(12:14)
              input_modification%s_ale(next)%created_elements_nb = message_value(15)
              input_modification%s_ale(next)%node_nb(1:3) = message_value(16:18)
              input_modification%s_ale(next)%created_nodes_nb = message_value(19)
              input_modification%s_ale(next)%boundary_id(1:6) = message_value(20:25)
              input_modification%s_ale(next)%bcs_id(1:6) = message_value(26:31)
              input_modification%s_ale(next)%boundary_type(1:6) = message_value(32:37)
              input_modification%s_ale(next)%error = message_value(38)
              input_modification%s_ale(next)%material_kind = message_value(39)
              input_modification%s_ale(next)%material_id = message_value(40)
              input_modification%s_ale(next)%element_offset = message_value(41)
              input_modification%s_ale(next)%node_offset = message_value(42)
              input_modification%s_ale(next)%reference_node_id = message_value(44)
              input_modification%s_ale(next)%link_vel_id = message_value(45)
            endif
          enddo
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine hm_s_ale
      end module hm_s_ale_mod