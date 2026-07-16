!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    ists_ass0_mod     ../engine/source/interfaces/ists/ists_ass0.F90
!||--- called by ------------------------------------------------------
!||    ists_mainf        ../engine/source/interfaces/ists/ists_mainf.F
!||====================================================================
module ists_ass0_mod
  implicit none
contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief STS force assembly for option /PARITH/OFF
!! \details Assembles forces and stiffnesses from the STS contact load array
!!          into the global acceleration (A) and nodal stiffness (STIFN) arrays.
!!          Optionally accumulates contact forces in FCONT when inconv == 1
!!          and animation or output of contact forces is requested.
subroutine ists_ass0(a, stifn, load_arr, node_arr, count, max_sts_size_actual, &
                      fcont, numnod, inconv, anim_v4, outp_v4, n_vect_cont)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
  use PRECISION_MOD, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
  implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
  integer,       intent(in)    :: max_sts_size_actual                     !< maximum STS array size
  integer,       intent(in)    :: count                                   !< number of contact pairs + 1
  integer,       intent(in)    :: numnod                                  !< number of physical nodes
  integer,       intent(in)    :: inconv                                  !< implicit convergence flag (/IMPL1/ INCONV)
  integer,       intent(in)    :: anim_v4                                 !< ANIM_V(4) animation flag (/SCR14/)
  integer,       intent(in)    :: outp_v4                                 !< OUTP_V(4) output flag (/SCR16/)
  integer,       intent(in)    :: n_vect_cont                             !< H3D contact vector count
  real(kind=WP), intent(inout) :: a(3, numnod)                            !< nodal accelerations (global array)
  real(kind=WP), intent(inout) :: stifn(numnod)                          !< nodal stiffnesses (global array)
  real(kind=WP), intent(inout) :: fcont(3, numnod)                       !< contact forces, size numnod
  real(kind=WP), intent(in)    :: load_arr(max_sts_size_actual, 8, 4)     !< per-pair load array
  integer,       intent(in)    :: node_arr(max_sts_size_actual * 8)       !< node id array, 8 ids per pair
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
  integer :: i, j, j1
  integer :: n_contacts
  logical :: do_fcont
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
  do_fcont  = inconv == 1 .and. anim_v4 + outp_v4 + n_vect_cont > 0
  n_contacts = max(0, count - 1)

  do i = 1, n_contacts
    if (node_arr((i - 1) * 8 + 1) <= 0) exit

    do j = 1, 8
      j1 = node_arr((i - 1) * 8 + j)
      if (j1 <= 0) cycle

      a(1, j1) = a(1, j1) + load_arr(i, j, 1)
      a(2, j1) = a(2, j1) + load_arr(i, j, 2)
      a(3, j1) = a(3, j1) + load_arr(i, j, 3)
      stifn(j1) = stifn(j1) + load_arr(i, j, 4)

      if (do_fcont .and. j1 <= numnod) then
        fcont(1, j1) = fcont(1, j1) + load_arr(i, j, 1)
        fcont(2, j1) = fcont(2, j1) + load_arr(i, j, 2)
        fcont(3, j1) = fcont(3, j1) + load_arr(i, j, 3)
      end if
    end do
  end do

! ----------------------------------------------------------------------------------------------------------------------
end subroutine ists_ass0

!! \brief STS force assembly for option /PARITH/ON
!! \details Scatters forces and stiffnesses from the STS contact load array into
!!          the parith sky arrays (FSKYI/ISKY) instead of writing A/STIFN directly.
!!          This makes the STS contribution participate in the deterministic,
!!          node-ordered parith reduction, so results are reproducible regardless
!!          of thread count (the direct A write is order-dependent because the STS
!!          candidate list comes from the parallel INT7 bucket sort).
!!          The routine is called from the JTASK==1 section of ISTS_MAINF; the
!!          NISKY slot reservation is still guarded by lockon/lockoff because other
!!          threads may concurrently reserve slots for other interfaces.
subroutine ists_ass_parith(fskyi, isky, nisky, lskyi, nfskyi, load_arr, node_arr, &
                           count, max_sts_size_actual, fcont, numnod, inconv, &
                           anim_v4, outp_v4, n_vect_cont)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
  use PRECISION_MOD, only : WP
  use MESSAGE_MOD, only : ancmsg, aninfo
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
  implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "comlock.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
  integer,       intent(in)    :: lskyi                                   !< first dimension of the sky force array
  integer,       intent(in)    :: nfskyi                                  !< number of columns of the sky force array
  integer,       intent(in)    :: max_sts_size_actual                     !< maximum STS array size
  integer,       intent(in)    :: count                                   !< number of contact pairs + 1
  integer,       intent(in)    :: numnod                                  !< number of physical nodes
  integer,       intent(in)    :: inconv                                  !< implicit convergence flag (/IMPL1/ INCONV)
  integer,       intent(in)    :: anim_v4                                 !< ANIM_V(4) animation flag (/SCR14/)
  integer,       intent(in)    :: outp_v4                                 !< OUTP_V(4) output flag (/SCR16/)
  integer,       intent(in)    :: n_vect_cont                             !< H3D contact vector count
  integer,       intent(inout) :: nisky                                   !< global sky slot counter (/PARIT/ NISKY)
  integer,       intent(inout) :: isky(lskyi)                             !< sky slot to node id map
  real(kind=WP), intent(inout) :: fskyi(lskyi, nfskyi)                    !< sky force array (fx/fy/fz/stiff/...)
  real(kind=WP), intent(inout) :: fcont(3, numnod)                       !< contact forces, size numnod
  real(kind=WP), intent(in)    :: load_arr(max_sts_size_actual, 8, 4)     !< per-pair load array
  integer,       intent(in)    :: node_arr(max_sts_size_actual * 8)       !< node id array, 8 ids per pair
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
  integer :: i, j, j1, k
  integer :: n_contacts, n_slots, base_slot, slot
  logical :: do_fcont
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
  do_fcont  = inconv == 1 .and. anim_v4 + outp_v4 + n_vect_cont > 0
  n_contacts = max(0, count - 1)

  ! Count the number of nodal contributions (one sky slot per non-zero node).
  n_slots = 0
  do i = 1, n_contacts
    if (node_arr((i - 1) * 8 + 1) <= 0) exit
    do j = 1, 8
      if (node_arr((i - 1) * 8 + j) > 0) n_slots = n_slots + 1
    end do
  end do
  if (n_slots == 0) return

  ! Reserve a contiguous block of sky slots. Other threads may reserve slots for
  ! other interfaces concurrently, so the counter update must be locked.
#include "lockon.inc"
  base_slot = nisky
  nisky = nisky + n_slots
#include "lockoff.inc"

  ! Abort cleanly if the sky array is too small rather than corrupting memory.
  if (base_slot + n_slots > lskyi) then
    call ancmsg(msgid=26, anmode=aninfo)
    call arret(2)
  end if

  slot = base_slot
  do i = 1, n_contacts
    if (node_arr((i - 1) * 8 + 1) <= 0) exit

    do j = 1, 8
      j1 = node_arr((i - 1) * 8 + j)
      if (j1 <= 0) cycle

      slot = slot + 1
      do k = 1, nfskyi
        fskyi(slot, k) = 0.0_WP
      end do
      fskyi(slot, 1) = load_arr(i, j, 1)
      fskyi(slot, 2) = load_arr(i, j, 2)
      fskyi(slot, 3) = load_arr(i, j, 3)
      fskyi(slot, 4) = load_arr(i, j, 4)
      isky(slot) = j1

      if (do_fcont .and. j1 <= numnod) then
        fcont(1, j1) = fcont(1, j1) + load_arr(i, j, 1)
        fcont(2, j1) = fcont(2, j1) + load_arr(i, j, 2)
        fcont(3, j1) = fcont(3, j1) + load_arr(i, j, 3)
      end if
    end do
  end do

! ----------------------------------------------------------------------------------------------------------------------
end subroutine ists_ass_parith

end module ists_ass0_mod
