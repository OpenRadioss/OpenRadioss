!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||    tag_node_from_part_sphcel_mod   ../starter/source/model/sets/tag_node_from_part_sphcel.F90
      !||--- called by ------------------------------------------------------
      !||    create_node_from_element        ../starter/source/model/sets/create_node_from_element.F
      !||====================================================================
      module tag_node_from_part_sphcel_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!\brief This subroutine creates a clause of nodes from sphcels
!=======================================================================================================================
      !||====================================================================
      !||    tag_node_from_part_sphcel   ../starter/source/model/sets/tag_node_from_part_sphcel.F90
      !||--- called by ------------------------------------------------------
      !||    create_node_from_element    ../starter/source/model/sets/create_node_from_element.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod                 ../starter/share/message_module/message_mod.F
      !||====================================================================
      subroutine tag_node_from_part_sphcel(nb_sphcel  ,sphcel  ,tagnod,clause_node,ind,numnod)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use SETDEF_MOD
          use MESSAGE_MOD
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                                   intent(in) :: nb_sphcel                         !< nb of sphcel in clause
          integer,                                   intent(inout) :: ind                            !< total nb of nodes in clause
          integer,                                   intent(in) :: numnod                            !< total nb of nodes in model
          integer,                                   intent(in)    :: sphcel(nb_sphcel)              !< sphcel ID in sphcel PART clause
          integer,                                   intent(inout) :: clause_node(numnod)            !< copy nodes in clause node (from sphcel)
          integer,                                   intent(inout) :: tagnod(numnod)                 !< tag nods of set
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,nod                        !< local entities id's
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
          if (nb_sphcel > 0) then
            ! node from sphcel part
            do i=1,nb_sphcel
              nod = sphcel(i)
              if (tagnod(nod) == 0) then
                tagnod(nod) = 1
                ind = ind+1
                clause_node(ind) = nod
              endif
            enddo
          endif
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine tag_node_from_part_sphcel
      end module tag_node_from_part_sphcel_mod