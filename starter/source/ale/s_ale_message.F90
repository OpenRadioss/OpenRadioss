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
!||    s_ale_message_mod   ../starter/source/ale/s_ale_message.F90
!||--- called by ------------------------------------------------------
!||    lectur              ../starter/source/starter/lectur.F
!||====================================================================
      module s_ale_message_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Here is a small description of the routine, [after the header]
!! \details if needed, more details can be added here
!||====================================================================
!||    s_ale_message            ../starter/source/ale/s_ale_message.F90
!||--- called by ------------------------------------------------------
!||    lectur                   ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||--- uses       -----------------------------------------------------
!||    input_modification_mod   ../starter/source/starter/input_modification.F90
!||    message_mod              ../starter/share/message_module/message_mod.F
!||====================================================================
        subroutine s_ale_message(istdo,iout,input_modification)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use message_mod
          use input_modification_mod, only : input_modif_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: istdo !< output unit
          integer, intent(in) :: iout !< output unit
          type(input_modif_), intent(in) :: input_modification !< input modification data structure
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,my_offset
          real(kind=8) :: percent
          character(len=8) :: bcs_string(6)
          logical :: print_bcs
          character(len=2), parameter :: dir_label(6) = ['X-', 'X+', 'Y-', 'Y+', 'Z-', 'Z+']
          character(len=100) :: titr
          integer :: character_nb,error_id,int1,int2,message_id
          logical, dimension(:), allocatable :: error_check
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          write(istdo,'(a)') ' .. STRUCTURED ALE OPTION'
          write(iout,2009) input_modification%s_ale_nb


          !integer, dimension(42) :: message_value ! 1 : s-ale mesh id
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
          allocate(error_check(input_modification%s_ale_nb))
          error_check(1:input_modification%s_ale_nb) = .false.
          do i=1,input_modification%s_ale_nb
            if(input_modification%s_ale(i)%error<0) error_check(i) = .true.
          enddo
          
          do i=1,input_modification%s_ale_nb
            character_nb = len_trim(input_modification%s_ale(i)%title)
            titr=''
            titr(1:character_nb) = input_modification%s_ale(i)%title(1:character_nb)
            write(iout,*)
            write(iout,2023)
            if(.not.error_check(i)) then
              write(iout,2010) input_modification%s_ale(i)%mesh_id,input_modification%s_ale(i)%title(1:character_nb), &
                               input_modification%s_ale(i)%part_id,input_modification%s_ale(i)%control_point_id(1:3)
              do j=1,3
                if(input_modification%s_ale(i)%kind_of_mesh(j)==0) then                
                  if(j==1) write(iout,2014)
                  if(j==2) write(iout,2015)
                  if(j==3) write(iout,2016)
                else
                  if(j==1) write(iout,2011)
                  if(j==2) write(iout,2012)
                  if(j==3) write(iout,2013)
                endif
              enddo

              write(iout,2018) input_modification%s_ale(i)%created_elements_nb
              my_offset = input_modification%s_ale(i)%element_offset
              if(my_offset>0) then
                write(iout,2024) input_modification%s_ale(i)%element_offset, &
                                input_modification%s_ale(i)%element_offset+input_modification%s_ale(i)%created_elements_nb
              else
                message_id = 3157
                int1 = input_modification%s_ale(i)%mesh_id
                int2 = abs(input_modification%s_ale(i)%element_offset)
                call ancmsg(msgid=message_id,msgtype=msgwarning,anmode=aninfo_blind_2,i1=int1,i2=int2,c1=titr(1:character_nb))
                write(iout,2025) abs(input_modification%s_ale(i)%element_offset), &
                                abs(input_modification%s_ale(i)%element_offset)+input_modification%s_ale(i)%created_elements_nb
              endif
              write(iout,2020) input_modification%s_ale(i)%created_nodes_nb
              my_offset = input_modification%s_ale(i)%node_offset
              if(my_offset>0) then
                write(iout,2026) input_modification%s_ale(i)%node_offset, &
                                 input_modification%s_ale(i)%node_offset+input_modification%s_ale(i)%created_nodes_nb
              else
                message_id = 3158
                int1 = input_modification%s_ale(i)%mesh_id
                int2 = abs(input_modification%s_ale(i)%node_offset)
                call ancmsg(msgid=message_id,msgtype=msgwarning,anmode=aninfo_blind_2,i1=int1,i2=int2,c1=titr(1:character_nb))                                 
                write(iout,2027) abs(input_modification%s_ale(i)%node_offset), &
                                 abs(input_modification%s_ale(i)%node_offset)+input_modification%s_ale(i)%created_nodes_nb
              endif
              if(input_modification%s_ale(i)%trimming_nb>0) then
                write(iout,2017)
                percent = real(input_modification%s_ale(i)%created_elements_nb) / real((input_modification%s_ale(i)%element_nb(1)* &
                                                                            input_modification%s_ale(i)%element_nb(2)* & 
                                                                            input_modification%s_ale(i)%element_nb(3)))
                
                percent = (1. - percent)*100.
                write(iout,2019) percent
                percent = real(input_modification%s_ale(i)%created_nodes_nb) / real((input_modification%s_ale(i)%node_nb(1)* &
                                                                            input_modification%s_ale(i)%node_nb(2)* & 
                                                                            input_modification%s_ale(i)%node_nb(3)))
                percent = (1. - percent)*100.
                write(iout,2021) percent            
              endif

              print_bcs = .false.
              bcs_string(1:6)(1:8) = ''
              do j=1,6
                if(input_modification%s_ale(i)%boundary_type(j)==1) then
                  print_bcs = .true.
                  bcs_string(j)(1:8) = "SLIPWALL"
                elseif(input_modification%s_ale(i)%boundary_type(j)==2) then
                  print_bcs = .true.
                  bcs_string(j)(1:3) = "NRF"
                endif
              enddo
              if(print_bcs) then

                write(iout,2022)
                do j=1,6
                  if(input_modification%s_ale(i)%boundary_type(j)==1) then
                    write(iout,'(5X,A2," DIRECTION: ",A8," , /ALE/BCS ID:",i5," , /BCS ID:",i5)') &
                      dir_label(j), bcs_string(j), input_modification%s_ale(i)%boundary_id(j), &
                      input_modification%s_ale(i)%bcs_id(j)
                    if(j==6) write(iout,*)
                  elseif(input_modification%s_ale(i)%boundary_type(j)==2) then 
                    write(iout,'(5X,A2," DIRECTION: ",A8," , /EBCS/NRF ID:",i5)') &
                              dir_label(j),bcs_string(j),input_modification%s_ale(i)%boundary_id(j)
                  endif
                enddo
              endif
              if(input_modification%s_ale(i)%reference_node_id>0) then
                write(iout,*)
                write(iout,2028) input_modification%s_ale(i)%reference_node_id, &
                                input_modification%s_ale(i)%link_vel_id
              elseif(input_modification%s_ale(i)%reference_node_id<0) then
                write(iout,*)
                message_id = 3159
                int1 = input_modification%s_ale(i)%mesh_id
                int2 = abs(input_modification%s_ale(i)%reference_node_id)
                call ancmsg(msgid=message_id,msgtype=msgwarning,anmode=aninfo_blind_2,i1=int1,i2=int2,c1=titr(1:character_nb))    
              endif
            else
              error_id = input_modification%s_ale(i)%error
              if(error_id==-1) then ! invalid part id
                int1 = input_modification%s_ale(i)%mesh_id
                int2 = input_modification%s_ale(i)%part_id                
                message_id = 3153
              elseif(error_id<=-2.and.error_id>=-4) then ! invalid control point id
                int1 = input_modification%s_ale(i)%mesh_id
                int2 = input_modification%s_ale(i)%control_point_id(abs(error_id)-1)
                message_id = 3154
              elseif(error_id<=-5.and.error_id>=-7) then ! control point error (number of element, sub-area, ...)
                int1 = input_modification%s_ale(i)%mesh_id
                int2 = input_modification%s_ale(i)%control_point_id(abs(error_id)-4)
                message_id = 3155                     
              elseif(error_id==-39) then ! invalid material law
                int1 = input_modification%s_ale(i)%mesh_id
                int2 = input_modification%s_ale(i)%material_id
                message_id = 3156              
              endif
              call ancmsg(msgid=message_id,msgtype=msgerror,anmode=aninfo_blind_2,i1=int1,i2=int2,c1=titr(1:character_nb))
            endif
          enddo
          write(iout,2023) 
          write(iout,*)
          return
! ----------------------------------------------------------------------------------------------------------------------


 2009 FORMAT(5X,'STRUCTURED ALE OPTION  ',/, &
             5X,'---------------------  ',/, &
             5X,'TOTAL NUMBER OF STRUCTURED MESHES  =',I5 )

 2010 FORMAT(5X,'STRUCTURED MESH ID =',I5/, &
             5X,'STRUCTURED MESH TITLE:',A/, &
             5X,'PART ID =',I5/, &
             5X,'CONTROL POINT ID (X DIRECTION) =',I5/, &
             5X,'CONTROL POINT ID (Y DIRECTION) =',I5/, &
             5X,'CONTROL POINT ID (Z DIRECTION) =',I5/ )

 2011 FORMAT(5X,'REFINED MESH IS GENERATED IN X DIRECTION')
 2012 FORMAT(5X,'REFINED MESH IS GENERATED IN Y DIRECTION')
 2013 FORMAT(5X,'REFINED MESH IS GENERATED IN Z DIRECTION'/ )
 2014 FORMAT(5X,'REGULAR MESH IS GENERATED IN X DIRECTION')
 2015 FORMAT(5X,'REGULAR MESH IS GENERATED IN Y DIRECTION')
 2016 FORMAT(5X,'REGULAR MESH IS GENERATED IN Z DIRECTION'/ )

 2017 FORMAT(5X,'TRIMMING OPTION IS USED TO MODIFY THE MESH')
 2018 FORMAT(5X,'TOTAL NUMBER OF CREATED ELEMENTS = ',I10 )
 2024 FORMAT(5X,'THE ELEMENT IDS BELONG TO THE INTERVAL [',I10,',',I10,']')
 2025 FORMAT(5X,'ELEMENT OFFSET ISSUE, THE ELEMENT IDS BELONG TO THE INTERVAL [',I10,',',I10,']')
 2019 FORMAT(5X,'TRIMMING REDUCED THE NUMBER OF ELEMENTS BY ',F6.2,' %' )
 2020 FORMAT(5X,'TOTAL NUMBER OF CREATED NODES = ',I10 )
 2026 FORMAT(5X,'THE NODE IDS BELONG TO THE INTERVAL [',I10,',',I10,']', / )
 2027 FORMAT(5X,'NODE OFFSET ISSUE, THE NODE IDS BELONG TO THE INTERVAL [',I10,',',I10,']', / ) 
 2021 FORMAT(5X,'TRIMMING REDUCED THE NUMBER OF NODES BY ',F6.2,' %', / )
 2022 FORMAT(5X,'BOUNDARY CONDITION TYPE ')
 2023 FORMAT(5X,'--------------  ')

 2028 FORMAT(5X,'A REFERENCE NODE IS USED IN THE STRUCTURED MESH', / , &
             5X,'AN /ALE/LINK/VEL IS AUTOMATICALLY GENERATED' / , &
             5X,'REFERENCE NODE ID =',I5/, &
             5X,'/ALE/LINK/VEL ID =',I5/ )

        end subroutine s_ale_message
      end module s_ale_message_mod
