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
module q1np_fix_front_mod
  implicit none
contains

! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

!! \brief Propagate parent solid front ownership to Q1NP promoted control points.
!! \details Q1NP promoted control points are appended after DOMDEC and therefore do
!!          not appear in the standard element connectivities used to build IFRONT.
!!          For each promoted control point, this routine sticks it on every SPMD domain
!!          owning one of its parent HEX8 (or bulk) nodes, so that restart splitting
!!          (DDSPLIT) sees a consistent front. It must be called after DOMDEC2 has built
!!          IFRONT and before GET_SIZE_NUMNOD_LOCAL / DDSPLIT.
  subroutine q1np_fix_front(ixs, nixs, numels, numnod, nspmd, iout)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   MODULES
! ----------------------------------------------------------------------------------------------------------------------
    use q1np_restart_mod , only : numelq1np_g, numnod_cp_added, numnod_old, &
                                  kq1np_tab, iq1np_tab, iq1np_bulk_tab
    use front_mod , only : ifront
! ----------------------------------------------------------------------------------------------------------------------
!                                                   IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
    implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
    integer, intent(in) :: nixs                    !< first dimension of the IXS connectivity array
    integer, intent(in) :: numels                  !< number of solid elements
    integer, intent(in) :: numnod                  !< current number of nodes (including promoted control points)
    integer, intent(in) :: nspmd                   !< number of SPMD domains
    integer, intent(in) :: iout                    !< output listing file unit
    integer, intent(in) :: ixs(nixs*numels)        !< solid element connectivity array
! ----------------------------------------------------------------------------------------------------------------------
!                                                   EXTERNAL FUNCTIONS
! ----------------------------------------------------------------------------------------------------------------------
    integer, external :: nlocal                    !< returns 1 if node is sticked on the given SPMD domain
! ----------------------------------------------------------------------------------------------------------------------
!                                                   LOCAL VARIABLES
! ----------------------------------------------------------------------------------------------------------------------
    integer :: iel, i, j, proc
    integer :: n_ctrl, offset_ctrl, offset_bulk, iel_hex8
    integer :: node_cp, node_src
    integer :: q1np_front_missing
! ----------------------------------------------------------------------------------------------------------------------
!                                                   BODY
! ----------------------------------------------------------------------------------------------------------------------
    do iel = 1, numelq1np_g
      n_ctrl = kq1np_tab(3,iel)
      offset_ctrl = kq1np_tab(4,iel)
      offset_bulk = kq1np_tab(14,iel)
      iel_hex8 = kq1np_tab(10,iel)
      do i = 1, n_ctrl
        node_cp = iq1np_tab(offset_ctrl + i - 1)
        if (node_cp <= numnod_old .or. node_cp > numnod) cycle
        if (iel_hex8 > 0 .and. iel_hex8 <= numels) then
          do j = 1, 8
            node_src = ixs(nixs*(iel_hex8-1) + j + 1)
            if (node_src <= 0) cycle
            do proc = 1, nspmd
              if (nlocal(node_src,proc) == 1) then
                call ifrontplus(node_cp,proc)
              endif
            enddo
          enddo
        elseif (offset_bulk > 0) then
          do j = 1, 4
            node_src = iq1np_bulk_tab(offset_bulk + j - 1)
            if (node_src <= 0) cycle
            do proc = 1, nspmd
              if (nlocal(node_src,proc) == 1) then
                call ifrontplus(node_cp,proc)
              endif
            enddo
          enddo
        endif
      enddo
    enddo
    q1np_front_missing = 0
    do node_cp = numnod_old + 1, numnod
      if (ifront%ientry(node_cp) == -1) then
        q1np_front_missing = q1np_front_missing + 1
      endif
    enddo
    write(iout,'(A,3I10)') ' Q1NP FRONT FIX: promoted, total, missing =', &
      numnod_cp_added, numnod, q1np_front_missing

! ----------------------------------------------------------------------------------------------------------------------
  end subroutine q1np_fix_front

end module q1np_fix_front_mod
