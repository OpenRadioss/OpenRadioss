!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
module q1np_get_elem_nodes_mod
  implicit none
contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

!! \brief Return the 8 corner nodes of a Q1NP element.
!! \details The corner nodes are rebuilt from the bulk table (bottom face, nodes 1-4) and the
!!          control-point table (top face corners, nodes 5-8) using the standard hex winding.
!!          IQ is the Q1NP element index (KQ1NP_TAB column), obtained from KQ1NP_TAB_INV.
  subroutine q1np_get_elem_nodes(iq, ns8)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   MODULES
! ----------------------------------------------------------------------------------------------------------------------
    use q1np_restart_mod, only : kq1np_tab, iq1np_tab, iq1np_bulk_tab
! ----------------------------------------------------------------------------------------------------------------------
!                                                   IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
    implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
    integer, intent(in)  :: iq       !< Q1NP element index (column of KQ1NP_TAB)
    integer, intent(out) :: ns8(8)   !< the 8 corner nodes (bottom 1-4, top 5-8)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   LOCAL VARIABLES
! ----------------------------------------------------------------------------------------------------------------------
    integer :: offset_bulk, idx_cp, p, q
! ----------------------------------------------------------------------------------------------------------------------
!                                                   BODY
! ----------------------------------------------------------------------------------------------------------------------
    offset_bulk = kq1np_tab(14, iq)
    idx_cp      = kq1np_tab(4, iq) - 1
    p           = kq1np_tab(8, iq)
    q           = kq1np_tab(9, iq)
    ! bottom face: bulk nodes
    ns8(1) = iq1np_bulk_tab(offset_bulk)
    ns8(2) = iq1np_bulk_tab(offset_bulk + 1)
    ns8(3) = iq1np_bulk_tab(offset_bulk + 2)
    ns8(4) = iq1np_bulk_tab(offset_bulk + 3)
    ! top face: control-point corners
    ns8(5) = iq1np_tab(idx_cp + 1)
    ns8(6) = iq1np_tab(idx_cp + (p+1))
    ns8(7) = iq1np_tab(idx_cp + (p+1)*(q+1))
    ns8(8) = iq1np_tab(idx_cp + (p+1)*q + 1)

    return
  end subroutine q1np_get_elem_nodes

end module q1np_get_elem_nodes_mod
