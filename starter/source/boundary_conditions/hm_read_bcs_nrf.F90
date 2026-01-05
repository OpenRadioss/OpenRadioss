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
!||    hm_read_bcs_nrf_mod   ../starter/source/boundary_conditions/hm_read_bcs_nrf.F90
!||--- called by ------------------------------------------------------
!||    lectur                ../starter/source/starter/lectur.F
!||====================================================================
      module hm_read_bcs_nrf_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Reader subroutine for option /BCS/NRF
!! \details
!||====================================================================
!||    hm_read_bcs_nrf        ../starter/source/boundary_conditions/hm_read_bcs_nrf.F90
!||--- called by ------------------------------------------------------
!||    lectur                 ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_read_key     ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start        ../starter/source/devtools/hm_reader/hm_option_start.F
!||    nodgrnr5               ../starter/source/starter/freform.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||====================================================================
        subroutine hm_read_bcs_nrf(lsubmodel, igrnod, ngrnod, itabm1, numnod, multi_fvm)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use submodel_mod , only : submodel_data, nsubmod
          use groupdef_mod , only : group_
          use message_mod , only : ancmsg, msgwarning, msgerror, aninfo
          use hm_option_read_mod , only : hm_option_read_key
          use multi_fvm_mod , only : multi_fvm_struct
          use bcs_mod , only : bcs
          use constant_mod , only : zero, em20, ep20
          use names_and_titles_mod , only : nchartitle
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
#include "units_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: ngrnod, numnod                            !< size for array definition
          integer, intent(in) :: itabm1(numnod)                            !< list of user node ids
          type(submodel_data), dimension(nsubmod), intent(in) :: lsubmodel !< submodel data for Reader subroutines
          type (group_), dimension(ngrnod), target :: igrnod               !< data for group of nodes
          type(multi_fvm_struct), intent(inout) :: multi_fvm               !< data for collocated scheme (multifluid law 151)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: user_grnod_id
          integer :: internal_grnod_id
          integer :: ii
          integer :: id,uid,sub_id
          integer :: num_nodes_in_group
          integer :: ibufnodes(numnod)
          logical :: is_available_grnod
          character(len=nchartitle) :: titr
          character :: label*31, mess*40
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External Functions
! ----------------------------------------------------------------------------------------------------------------------
          integer,external :: nodgrnr5
! ----------------------------------------------------------------------------------------------------------------------
          data mess/"BOUNDARY CONDITIONS                     "/
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Preconditions
! ----------------------------------------------------------------------------------------------------------------------
          ! if no option /BCS/NRF in input file then return
          if(bcs%num_nrf == 0)return
          ! compatible only with FEM
          if(multi_fvm%is_used)then
            label = "/BCS/NRF"
            call ancmsg(msgid=139,anmode=aninfo,msgtype=msgwarning,i1=id,c1=label,c2=label,c3=titr)
            return
          end if
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          !call hm_debug_print_option('/BCS/NRG')


          call hm_option_start("/BCS/NRF")
          allocate(bcs%nrf(bcs%num_nrf))
          write(iout, 2010)

          do ii = 1, bcs%num_nrf

            !READER
            call hm_option_read_key(lsubmodel, option_id=id, unit_id=uid, submodel_id=sub_id, option_titr=titr)
            call hm_get_intv("set_ID", user_grnod_id, is_available_grnod, lsubmodel)

            !CHECK USER SET IDENTIFIER
            num_nodes_in_group = nodgrnr5(user_grnod_id,internal_grnod_id,ibufnodes,igrnod,itabm1,mess)

            !FILL BARRIER DATA STRUCTURE
            bcs%nrf(ii)%user_id = id
            bcs%nrf(ii)%set_id = internal_grnod_id

            write(iout, 2001) id, trim(titr)
            write(iout, 2011) user_grnod_id !mandatory
            write(iout, 2015)

          end do !next ii


          return
! ----------------------------------------------------------------------------------------------------------------------
2010      FORMAT(5X, &
            5X,/,"     NON REFLECTING BOUNDARY FRONTIER  ", /,&
            5X, "--------------------------------  ", /)

2001      format(5X, "id:", I0,",",5X,A)
2011      format(5X, "set identifier . . . . . . . . . . . . =", I10)
2015      format(5X)

! ----------------------------------------------------------------------------------------------------------------------
        end subroutine hm_read_bcs_nrf
      end module hm_read_bcs_nrf_mod
