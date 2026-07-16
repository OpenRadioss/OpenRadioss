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
!||    ists_ass2_mod     ../engine/source/interfaces/ists/ists_ass2.F90
!||====================================================================
module ists_ass2_mod
  implicit none
contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief STS force assembly for option /PARITH/ON (skyline storage)
!! \details Assembles forces and stiffnesses from the STS contact load array
!!          into the skyline buffer FSKYI/ISKY under OpenMP lock protection.
!!          Corresponds to the STS counterpart of I7ASS2.
subroutine ists_ass2(fskyi, isky, count, max_sts_size_actual, &
                      nisky, lskyi, nfskyi, load_arr, node_arr)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
  use PRECISION_MOD,  only : WP
  use MESSAGE_MOD
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
  integer,       intent(in)    :: max_sts_size_actual                     !< maximum STS array size
  integer,       intent(in)    :: count                                   !< number of contact pairs + 1
  integer,       intent(inout) :: nisky                                   !< current skyline counter (/PARIT/ NISKY)
  integer,       intent(in)    :: lskyi                                   !< skyline buffer size (/PARIT/ LSKYI)
  integer,       intent(in)    :: nfskyi                                  !< skyline force buffer columns (/PARIT/ NFSKYI)
  integer,       intent(inout) :: isky(*)                                  !< skyline node id array
  real(kind=WP), intent(inout) :: fskyi(lskyi, nfskyi)                  !< skyline force buffer
  real(kind=WP), intent(in)    :: load_arr(max_sts_size_actual, 8, 4)   !< per-pair load: (pairs, 8 nodes, fx/fy/fz/stiff)
  integer,       intent(in)    :: node_arr(max_sts_size_actual * 8)      !< node id array, 8 ids per pair
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
  integer :: i, j
  integer :: niskyl1, niskyl
  integer :: n_contacts
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
  n_contacts = max(0, count - 1)
  if (n_contacts <= 0) return

  if (n_contacts > max_sts_size_actual) then
    write(*, *) 'ists_ass2: ERROR - array bounds exceeded'
    write(*, *) '  n_contacts = ', n_contacts, ' > max_sts_size_actual = ', max_sts_size_actual
    return
  end if

  niskyl1 = n_contacts * 8

#include "lockon.inc"
  if (nisky + niskyl1 > lskyi) then
    call ancmsg(msgid=26, anmode=aninfo)
    call arret(2)
  end if
  niskyl = nisky
  nisky  = nisky + niskyl1
#include "lockoff.inc"

  do i = 1, n_contacts
    do j = 1, 8
      niskyl = niskyl + 1
      fskyi(niskyl, 1) = load_arr(i, j, 1)
      fskyi(niskyl, 2) = load_arr(i, j, 2)
      fskyi(niskyl, 3) = load_arr(i, j, 3)
      fskyi(niskyl, 4) = load_arr(i, j, 4)
      isky(niskyl)     = node_arr((i - 1) * 8 + j)
    end do
  end do

! ----------------------------------------------------------------------------------------------------------------------
end subroutine ists_ass2

end module ists_ass2_mod
