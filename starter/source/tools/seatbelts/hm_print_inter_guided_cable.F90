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
!||    hm_print_inter_guided_cable_mod   ../starter/source/tools/seatbelts/hm_print_inter_guided_cable.F90
!||--- called by ------------------------------------------------------
!||    lectur                            ../starter/source/starter/lectur.F
!||====================================================================
      module hm_print_inter_guided_cable_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Printout  for option /INTER/GUIDED_CABLE
!! \details
!||====================================================================
!||    hm_print_inter_guided_cable   ../starter/source/tools/seatbelts/hm_print_inter_guided_cable.F90
!||--- called by ------------------------------------------------------
!||    lectur                        ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                        ../starter/source/output/message/message.F
!||    fretitl2                      ../starter/source/starter/freform.F
!||--- uses       -----------------------------------------------------
!||    message_mod                   ../starter/share/message_module/message_mod.F
!||====================================================================
        subroutine hm_print_inter_guided_cable(ninter,nintsub,ngrnod,ngrpart,igrnod,igrpart,                &
                                               npari,ipari,ltitr,lnopt1 ,nom_opt)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use message_mod , only : ancmsg, msgwarning, msgerror, aninfo_blind_1
          use groupdef_mod , only : group_
          use names_and_titles_mod , only : nchartitle
          use seatbelt_mod, only : nguided_cable,guide
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
#include "units_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: ninter                                    !< number of interfaces
          integer, intent(in) :: nintsub                                   !< number of sub-interfaces
          integer, intent(in) :: ngrnod                                    !< number of groups of nodes
          integer, intent(in) :: ngrpart                                   !< number of groups of parts
          integer, intent(in) :: npari                                     !< size of array in IPARI
          integer, intent(in) :: ltitr                                     !< title length
          integer, intent(in) :: lnopt1                                    !< option length
          integer, intent(in) :: ipari(npari,ninter)                       !< interface part connectivity
          integer, intent(in) :: nom_opt(lnopt1,*)                         !< option titles for /TH/INTER
          type (group_), dimension(ngrnod), target :: igrnod               !< data for group of nodes
          type (group_), dimension(ngrpart),target :: igrpart              !< data for group of parts
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ig,j,id
          integer :: grnod_id_u, grpart_id_u      
          character(len=nchartitle) :: titr
          character(len=40) :: mess
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External Functions
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------      
          data mess/'INTERFACE INPUT                         '/          
! ----------------------------------------------------------------------------------------------------------------------
!   
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!                            
          do ig = 1, nguided_cable      
!
            id = guide(ig)%id      
            call fretitl2(titr, nom_opt(lnopt1-ltitr+1,nintsub+ig), ltitr)  
!           
!---------  Check common Id with other interfaces----------     
    
            if (ninter > 0) then
              do j=1,ninter
                if (id == ipari(15,j)) then
                  call ancmsg(msgid=117, msgtype=msgerror, anmode=aninfo_blind_1, i1=id, c1=trim(titr), i2=id)
                end if
              end do
            end if
!
            grnod_id_u = 0
            grpart_id_u = 0
            if (guide(ig)%node_set > 0) grnod_id_u = igrnod(guide(ig)%node_set)%id
            if (guide(ig)%part_set > 0) grpart_id_u = igrpart(guide(ig)%part_set)%id
!          
            write(iout, 1000) 
            write(iout, 1001) id,trim(titr)
            write(iout, 1002)
            write(iout, 1100) grnod_id_u,grpart_id_u,guide(ig)%istiff,guide(ig)%stfac,guide(ig)%fric
!            
          end do
!
! ----------------------------------------------------------------------------------------------------------------------
1000      format(/1X,'   INTERFACES               ' /                                  &
                  1X,' --------------             '/)

1001      format(/1X,'  INTERFACE NUMBER :',I10,1X,A)

1002      format(/5X,'TYPE== GUIDED CABLE')

1100      format(/5X,'SET OF NODES  . . . . . . . . . . . . . .',I10                   &
                 /5X,'SET OF PARTS  . . . . . . . . . . . . . .',I10                   &
                 /5X,'CONTACT STIFFNESS TYPE  . . . . . . . . .',I10                   &
                 /5X,'STIFFNESS SCALE FACTOR  . . . . . . . . .',1PG14.4               &
                 /5X,'FRICTION COEFFICIENT  . . . . . . . . . .',1PG14.4)
!                 
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine hm_print_inter_guided_cable
      end module hm_print_inter_guided_cable_mod