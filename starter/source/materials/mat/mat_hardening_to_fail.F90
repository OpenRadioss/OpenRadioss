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
!||    mat_hardening_to_fail_mod   ../starter/source/materials/mat/mat_hardening_to_fail.F90
!||--- called by ------------------------------------------------------
!||    law02_upd                   ../starter/source/materials/mat/mat002/law02_upd.F90
!||====================================================================
      module mat_hardening_to_fail_mod
      implicit none
      contains
        ! ==========================================================================================
        ! \brief Updating material parameters of /mat/law02
        ! \details create tabulated hardening function for failure model initialization
        ! ==========================================================================================
!!||--- calls      -----------------------------------------------------
!||====================================================================
!||    mat_hardening_to_fail   ../starter/source/materials/mat/mat_hardening_to_fail.F90
!||--- called by ------------------------------------------------------
!||    law02_upd               ../starter/source/materials/mat/mat002/law02_upd.F90
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine mat_hardening_to_fail(npt, eps, sig, fail)
! --------------------------------------------------------------------------------------------------
!         Modules
! --------------------------------------------------------------------------------------------------
          use fail_param_mod
          use precision_mod ,only : WP
! --------------------------------------------------------------------------------------------------
          implicit none
! --------------------------------------------------------------------------------------------------
!         Dummy arguments
! --------------------------------------------------------------------------------------------------
          integer                           ,intent(in)    :: npt
          type(fail_param_)                 ,intent(inout) :: fail !< failure model data structure
          real(kind=WP)     ,dimension(npt) ,intent(in)    :: eps
          real(kind=WP)     ,dimension(npt) ,intent(in)    :: sig
! --------------------------------------------------------------------------------------------------
!         Local variables
! --------------------------------------------------------------------------------------------------
          integer :: i,j,ndim,ntable
          type(table_4d_) ,dimension(:) ,allocatable :: table_copy
! ==================================================================================================
          ntable = fail%ntable4d
          allocate (table_copy(ntable+1))
          do i = 1,ntable
            ! create local copy of failure model function tables
            call copy_table_to(fail%table4d(i), table_copy(i))
            
            ! deallocate original function tables
            ndim = fail%table4d(i)%ndim
            if (allocated (fail%table4d(i)%y1d)) deallocate(fail%table4d(i)%y1d)
            if (allocated (fail%table4d(i)%y2d)) deallocate(fail%table4d(i)%y2d)
            if (allocated (fail%table4d(i)%y3d)) deallocate(fail%table4d(i)%y3d)
            if (allocated (fail%table4d(i)%y4d)) deallocate(fail%table4d(i)%y4d)
          end do           
          if (allocated (fail%table4d)) deallocate(fail%table4d)
!-------------------------------------------------------------------------------
          ! allocate new function tables and deallocate local copies

          allocate (fail%table4d(ntable+1))
          do i = 1,ntable
            call copy_table_to(table_copy(i) ,fail%table4d(i))
            ndim = table_copy(i)%ndim
            if (allocated (table_copy(i)%y1d)) deallocate(table_copy(i)%y1d)
            if (allocated (table_copy(i)%y2d)) deallocate(table_copy(i)%y2d)
            if (allocated (table_copy(i)%y3d)) deallocate(table_copy(i)%y3d)
            if (allocated (table_copy(i)%y4d)) deallocate(table_copy(i)%y4d)
          end do           
!
          ! add static hardening function to the table list of failure model
!
          ntable = ntable + 1
          fail%ntable4d = ntable
          fail%table4d(ntable)%notable = 1
          fail%table4d(ntable)%ndim    = 1
          allocate (fail%table4d(ntable)%x(1))
          allocate (fail%table4d(ntable)%x(1)%values(npt))
          allocate (fail%table4d(ntable)%y1d(npt))

          fail%table4d(ntable)%x(1)%values(1:npt) = eps(1:npt)
          fail%table4d(ntable)%y1d(1:npt)         = sig(1:npt)
!-------------------------------------------------------------------------------
          return
          end subroutine mat_hardening_to_fail
          end module mat_hardening_to_fail_mod
