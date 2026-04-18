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
!||    hm_rbodies_add_main_node_mod   ../starter/source/devtools/hm_reader/hm_rbodies_add_main_node.F90
!||--- called by ------------------------------------------------------
!||    starter0                       ../starter/source/starter/starter0.F
!||====================================================================
      module hm_rbodies_add_main_node_mod
        implicit none
        
        interface
          subroutine add_rbody_main_node(addednodeid) bind(C, name="cpp_add_rbody_main_node")
            use, intrinsic :: iso_c_binding
            integer(C_INT), intent(inout) :: addednodeid
          end subroutine add_rbody_main_node
        end interface
        
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief add new main node to /RBODY when not existing
! ======================================================================================================================
!||====================================================================
!||    hm_rbodies_add_main_node   ../starter/source/devtools/hm_reader/hm_rbodies_add_main_node.F90
!||--- called by ------------------------------------------------------
!||    starter0                   ../starter/source/starter/starter0.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                     ../starter/source/output/message/message.F
!||    hm_get_intv                ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_count            ../starter/source/devtools/hm_reader/hm_option_count.F
!||    hm_option_read_key         ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start            ../starter/source/devtools/hm_reader/hm_option_start.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod         ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod                ../starter/share/message_module/message_mod.F
!||    submodel_mod               ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_rbodies_add_main_node(lsubmodel)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use submodel_mod
          use message_mod
          use hm_option_read_mod
          use names_and_titles_mod, only : nchartitle
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type (submodel_data),INTENT(IN)    :: lsubmodel(nsubmod) !< submodel table
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
          integer :: nbrbodies ! number of rbodies
          integer :: rbodyid   ! rbody identifier
          integer :: m_id      ! initial main node id
          integer :: addednodeid ! main node identifier added
          character(len=nchartitle) :: rbodyidTitle ! rbody title
          logical :: is_available
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          is_available = .false.
          call hm_option_count('/RBODY', nbrbodies)
          call hm_option_start('/RBODY')
          do i=1,nbrbodies
            call hm_option_read_key(lsubmodel,             &
                                    option_id   = rbodyid, &
                                    option_titr = rbodyidtitle)
            call hm_get_intv('node_ID',m_id,is_available,lsubmodel)
            if(m_id == 0 )  then
              call add_rbody_main_node(addednodeid)
              if(addednodeid > 0)  call ancmsg(msgid=3122,      &
                                             msgtype=msgwarning,&
                                             anmode=aninfo,     &
                                             i1=rbodyid,        &
                                             c1=rbodyidtitle,   &
                                             i2=addednodeid)
            end if
          enddo
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine hm_rbodies_add_main_node
      end module hm_rbodies_add_main_node_mod
