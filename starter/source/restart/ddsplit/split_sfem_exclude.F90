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
!||    split_sfem_exclude_mod   ../starter/source/restart/ddsplit/split_sfem_exclude.F90
!||--- called by ------------------------------------------------------
!||    ddsplit                  ../starter/source/restart/ddsplit/ddsplit.F
!||====================================================================
      module split_sfem_exclude_mod

      implicit none

      contains
!=======================================================================================================================
!!\brief This subroutine get local number of nodes excluded for nodal pressure
!=======================================================================================================================
!||====================================================================
!||    c_sfem_exclude   ../starter/source/restart/ddsplit/split_sfem_exclude.F90
!||--- called by ------------------------------------------------------
!||    ddsplit          ../starter/source/restart/ddsplit/ddsplit.F
!||====================================================================
        subroutine c_sfem_exclude(numnod,nodlocal,ne_sfem,in_sfem,ne_sfem_l)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer , intent(in   )                          :: numnod           !< number of node
          integer , dimension(numnod), intent(in   )       :: nodlocal         !< global to local node number
          integer, intent (in)                             :: ne_sfem          !< number node of excluded sfem
          integer, intent (in   ) ,dimension(ne_sfem)      :: in_sfem          !< list of excluded nodes
          integer, intent (inout)                          :: ne_sfem_l        !< number local node of excluded sfem
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,node
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          do i=1,ne_sfem
            node=in_sfem(i)
            if (nodlocal(node)>0) ne_sfem_l=ne_sfem_l+1
          end do
!
        end subroutine c_sfem_exclude
!=======================================================================================================================
!!\brief This subroutine write restart list of node sfem_exclude
!=======================================================================================================================
!||====================================================================
!||    w_sfem_exclude   ../starter/source/restart/ddsplit/split_sfem_exclude.F90
!||--- called by ------------------------------------------------------
!||    ddsplit          ../starter/source/restart/ddsplit/ddsplit.F
!||--- calls      -----------------------------------------------------
!||====================================================================
        subroutine w_sfem_exclude(numnod,nodlocal,ne_sfem,in_sfem,ne_sfem_l,len_ia)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer , intent(in   )                          :: numnod           !< number of node
          integer , dimension(numnod), intent(in   )       :: nodlocal         !< global to local node number
          integer, intent (in)                             :: ne_sfem          !< number node of excluded sfem
          integer, intent (in   ) ,dimension(ne_sfem)      :: in_sfem          !< list of excluded nodes
          integer, intent (in   )                          :: ne_sfem_l        !< number local node of excluded sfem
          integer, intent (inout)                          :: len_ia           !< size of integer array
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,node,nl
          integer, dimension(ne_sfem_l) :: itmp        
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          nl = 0
          itmp = 0
          do i=1,ne_sfem
            node=in_sfem(i)
            if (nodlocal(node)>0)  then 
              nl = nl + 1
              itmp(nl) = nodlocal(node)
            end if
          end do
          call write_i_c(itmp,ne_sfem_l)
          len_ia = len_ia + ne_sfem_l
!
        end subroutine w_sfem_exclude
!
      end module split_sfem_exclude_mod
