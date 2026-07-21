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
!! \brief Delete stale contact-force CSV output files at engine startup
!! \details Removes leftover STS/NTS/Q1NP contact-force CSV files from a
!!          previous run so that the current run starts appending to clean
!!          output files. The set of files to remove depends on the active
!!          interfaces:
!!          - q1np_contact_forces.csv when Q1NP contact is active
!!          - sts_contact_forces.csv / nts_contact_forces.csv otherwise,
!!            depending on the availability of the STS voxel driver data.
!!          The cleanup is performed only once, on the first call, and the
!!          caller is responsible for restricting the call to the master
!!          process (ISPMD == 0).
module csv_contact_ini_mod
  implicit none
contains

! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================

!! \brief Delete stale contact-force CSV files left over from a previous run
subroutine csv_contact_ini(ipari, npari, ninter, igrsurf, nsurf)

! ----------------------------------------------------------------------------------------------------------------------
!                                                   MODULES
! ----------------------------------------------------------------------------------------------------------------------
  use GROUPDEF_MOD, only : surf_
  use Q1NP_RESTART_MOD, only : numelq1np_g, q1np_nknot_sets_g, kq1np_tab
  use STS_VOXEL_DRIVER_MOD, only : sts_voxel_driver_data_ready

! ----------------------------------------------------------------------------------------------------------------------
!                                                   IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
  implicit none

! ----------------------------------------------------------------------------------------------------------------------
!                                                   ARGUMENTS
! ----------------------------------------------------------------------------------------------------------------------
  integer,     intent(in) :: npari               !< Leading dimension of IPARI
  integer,     intent(in) :: ninter              !< Number of interfaces
  integer,     intent(in) :: nsurf               !< Number of surfaces
  integer,     intent(in) :: ipari(npari,ninter) !< Interface parameter array
  type(surf_), intent(in) :: igrsurf(nsurf)      !< Surface groups

! ----------------------------------------------------------------------------------------------------------------------
!                                                   LOCAL VARIABLES
! ----------------------------------------------------------------------------------------------------------------------
  logical, parameter :: i7sts = .true.  ! STS voxel driver preferred over NTS for INT7
  logical, save      :: first = .true.  ! Ensures the cleanup runs only once
  logical :: sts, nts, q1, q1on, vox
  integer :: ni, nt7, id, si, mi

! ----------------------------------------------------------------------------------------------------------------------
!                                                   BODY
! ----------------------------------------------------------------------------------------------------------------------
  if (.not. first) return
  first = .false.

  sts = .false.
  nts = .false.
  q1on = numelq1np_g > 0 .and. q1np_nknot_sets_g >= 2 .and. allocated(kq1np_tab)
  q1 = q1on

  ! Determine which STS/NTS CSV files may exist when Q1NP is not active
  if (.not. q1on) then
    do ni = 1, ninter
      if (ipari(33, ni) == 1) cycle
      nt7 = ipari(7, ni)
      if (nt7 /= 7) cycle
      id = ipari(15, ni)
      vox = sts_voxel_driver_data_ready(igrsurf, nsurf, id, .false., si, mi)
      if (i7sts .and. vox) sts = .true.
      if (.not. i7sts .or. .not. vox) nts = .true.
    end do
  end if

  ! Remove the stale CSV files for the active contact families
  if (sts) call delete_file_if_exists('sts_contact_forces.csv')
  if (nts) call delete_file_if_exists('nts_contact_forces.csv')
  if (q1)  call delete_file_if_exists('q1np_contact_forces.csv')

contains

  !! \brief Delete a file if it exists, ignoring I/O errors
  subroutine delete_file_if_exists(fname)
    character(len=*), intent(in) :: fname
    logical :: fex
    integer :: lu, ioa, iob
    inquire(file=fname, exist=fex)
    if (fex) then
      open(newunit=lu, file=fname, status='old', iostat=ioa)
      if (ioa == 0) close(unit=lu, status='delete', iostat=iob)
    end if
  end subroutine delete_file_if_exists

! ----------------------------------------------------------------------------------------------------------------------
end subroutine csv_contact_ini

end module csv_contact_ini_mod
