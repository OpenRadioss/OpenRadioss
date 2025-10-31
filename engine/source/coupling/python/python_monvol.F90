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
!||    python_monvol_mod   ../engine/source/coupling/python/python_monvol.F90
!||--- called by ------------------------------------------------------
!||    resol               ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    precision_mod       ../common_source/modules/precision_mod.F90
!||====================================================================
      module python_monvol_mod
        implicit none
        interface
!    void cpp_python_update_reals(char * basename, int * uid, my_real *reals, int num_reals)
          subroutine python_update_reals(basename, uid, reals, num_reals) bind(c, name="cpp_python_update_reals")
            use, intrinsic :: iso_c_binding, only : c_char, c_int
            use precision_mod, only : WP
            implicit none
            character(kind=c_char), intent(in) :: basename(*)
            integer(c_int), intent(in) :: uid(*)
            real(kind=WP), intent(inout) :: reals(*)
            integer(c_int), intent(in), value :: num_reals
          end subroutine python_update_reals

        end interface
      contains
!||====================================================================
!||    python_monvol         ../engine/source/coupling/python/python_monvol.F90
!||--- called by ------------------------------------------------------
!||    resol                 ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    monvol_struct_mod     ../engine/share/modules/monvol_struct_mod.F
!||    precision_mod         ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine python_monvol(t_monvol)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Module
! ----------------------------------------------------------------------------------------------------------------------
          use, intrinsic :: iso_c_binding
          use MONVOL_STRUCT_MOD
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Implicit None
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(monvol_struct_), dimension(:), intent(in) :: t_monvol !< the monitored volume data structure
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer(c_int) :: nvolu
          integer :: i
          real(kind=WP), dimension(:), allocatable :: reals
          integer, dimension(:), allocatable :: uid
          ! C compatible character string for Python
          character(kind=c_char, len=:), allocatable :: basename
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          nvolu = size(t_monvol)
          if (nvolu > 0) then
            allocate(reals(nvolu))
            allocate(uid(nvolu))
            ! Expose the monitored volume data to Python
            do i = 1, nvolu
              reals(i) = t_monvol(i)%volume
              uid(i) = t_monvol(i)%uid
            end do
            allocate(character(len=11) :: basename)
            basename = "MONVOL_VOL" // c_null_char
            call python_update_reals(basename, uid, reals, nvolu)
            deallocate(basename)
            do i = 1, nvolu
              reals(i) = t_monvol(i)%pressure
            end do
            allocate(character(len=9) :: basename)
            basename = "MONVOL_P" // c_null_char
            call python_update_reals(basename, uid, reals, nvolu)
            deallocate(basename)
            do i = 1, nvolu
              reals(i) = t_monvol(i)%area
            end do
            allocate(character(len=9) :: basename)
            basename = "MONVOL_A" // c_null_char
            call python_update_reals(basename, uid, reals, nvolu)
            deallocate(basename)
            do i = 1, nvolu
              reals(i) = t_monvol(i)%temperature
            end do
            allocate(character(len=9) :: basename)
            basename = "MONVOL_T" // c_null_char
            call python_update_reals(basename, uid, reals, nvolu)
            deallocate(basename)
            deallocate(reals)
            deallocate(uid)
          end if
        end subroutine python_monvol

      end module python_monvol_mod
