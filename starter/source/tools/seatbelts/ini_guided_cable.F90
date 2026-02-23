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
!||    ini_guided_cable_mod   ../starter/source/tools/seatbelts/ini_guided_cable.F90
!||--- called by ------------------------------------------------------
!||    initia                 ../starter/source/elements/initia/initia.F
!||====================================================================
      module ini_guided_cable_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Reader subroutine for option /GUIDE
!! \details
!||====================================================================
!||    ini_guided_cable   ../starter/source/tools/seatbelts/ini_guided_cable.F90
!||--- called by ------------------------------------------------------
!||    initia             ../starter/source/elements/initia/initia.F
!||--- calls      -----------------------------------------------------
!||    ifrontplus         ../starter/source/spmd/node/frontplus.F
!||    nlocal             ../starter/source/spmd/node/ddtools.F
!||--- uses       -----------------------------------------------------
!||    message_mod        ../starter/share/message_module/message_mod.F
!||====================================================================
        subroutine ini_guided_cable(nspmd)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use message_mod , only : ancmsg, msgwarning, msgerror, aninfo_blind_1
          use constant_mod , only : zero, em20, ep20
          use precision_mod, only : WP
          use seatbelt_mod, only : nguided_cable,guide
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
#include "units_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nspmd                                     !< number of spmd domains
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,ig, p
          integer :: node1, anchor_node
          integer :: sum_proc(nspmd)        
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External Functions
! ----------------------------------------------------------------------------------------------------------------------
          integer,external :: nlocal
! ----------------------------------------------------------------------------------------------------------------------
!   
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!          
!-------------------------------------------------------------------------------------------          
!         Stick anchor nodes on the same processor than guide
!-------------------------------------------------------------------------------------------   
!                  
          do ig = 1, nguided_cable
!
            sum_proc(1:nspmd) = 0             
            do i = guide(ig)%ncont+1, guide(ig)%nb_nodes
              node1 = guide(ig)%nodes(i)
              do p = 1, nspmd
                if (nlocal(node1, p) == 1) sum_proc(p) = sum_proc(p) + 1
              end do
            end do
!
            guide(ig)%proc = 0
            do p = 1, nspmd
              if (sum_proc(p)==guide(ig)%nb_nodes - guide(ig)%ncont) guide(ig)%proc = p
            end do 
!
            do i = 1, guide(ig)%ncont
              anchor_node = guide(ig)%cont(i)%anchor_node
              if ((nlocal(anchor_node, guide(ig)%proc) == 0)) then
                ! anchor node must be stick on the proc of the guide
                call ifrontplus(anchor_node, guide(ig)%proc)
              end if
            end do
!            
          end do
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine ini_guided_cable
      end module ini_guided_cable_mod