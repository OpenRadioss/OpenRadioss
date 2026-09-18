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


      module multi_cutcell_mod
        use precision_mod, only : WP
        use grid2D_struct_multicutcell_mod, only : grid2D_struct_multicutcell
        
        implicit none

        type multi_cutcell_struct
          integer :: nbmat
          integer :: nelem
          !logical :: is_used                  
          !logical :: is_restart
          !integer :: muscl
          real(kind=WP), dimension(:, :), allocatable :: vel ! size 3 x nelem
          real(kind=WP), dimension(:), allocatable :: sound_speed !size nelem
          real(kind=WP), dimension(:), allocatable :: rho, eint, etot, pres !size nelem
          real(kind=WP), dimension(:, :), allocatable :: phase_pres, phase_rho !size nelem x nbmat
          real(kind=WP), dimension(:, :), allocatable :: phase_velx, phase_vely, phase_velz !size nelem x nbmat
          type(grid2D_struct_multicutcell), dimension(:, :), allocatable :: grid !size nelem x nbmat

          ! indicates whether we run in 3d (sym = 0), or 2d (sym = 1 planar case, sym = 2 cylindrical case)
          integer :: sym
          ! low mach options for water / air applications
          !logical :: lowmach_opt
        end type multi_cutcell_struct
        
        contains

        subroutine allocate_multi_cutcell_type(nbmat, nelem, multi_cutcell_var)
            integer, value :: nbmat, nelem
            type(multi_cutcell_struct), intent(out) :: multi_cutcell_var

            multi_cutcell_var%nbmat = nbmat
            multi_cutcell_var%nelem = nelem
            multi_cutcell_var%sym = 1
            allocate(multi_cutcell_var%vel(3, nelem)) ; multi_cutcell_var%vel(1:3, 1:nelem) = 0._WP
            allocate(multi_cutcell_var%sound_speed(nelem)) ; multi_cutcell_var%sound_speed(1:nelem) = 0._WP
            allocate(multi_cutcell_var%rho(nelem)) ; multi_cutcell_var%rho(1:nelem) = 0._WP
            allocate(multi_cutcell_var%eint(nelem)) ; multi_cutcell_var%eint(1:nelem) = 0._WP
            allocate(multi_cutcell_var%etot(nelem)) ; multi_cutcell_var%etot(1:nelem) = 0._WP
            allocate(multi_cutcell_var%pres(nelem)) ; multi_cutcell_var%pres(1:nelem) = 0._WP
            allocate(multi_cutcell_var%phase_pres(nelem, nbmat)) ; multi_cutcell_var%phase_pres(1:nelem, 1:nbmat)= 0._WP
            allocate(multi_cutcell_var%phase_rho(nelem, nbmat) ) ; multi_cutcell_var%phase_rho(1:nelem, 1:nbmat) = 0._WP
            !allocate(multi_cutcell_var%phase_velx(nelem, nbmat)) ! not allocated for now
            allocate(multi_cutcell_var%phase_vely(nelem, nbmat)) ; multi_cutcell_var%phase_vely(1:nelem, 1:nbmat) = 0._WP
            allocate(multi_cutcell_var%phase_velz(nelem, nbmat)) ; multi_cutcell_var%phase_velz(1:nelem, 1:nbmat) = 0._WP
        end subroutine allocate_multi_cutcell_type

        subroutine deallocate_multi_cutcell_type(multi_cutcell_var)
            type(multi_cutcell_struct), intent(inout) :: multi_cutcell_var

            deallocate(multi_cutcell_var%vel)
            deallocate(multi_cutcell_var%sound_speed)
            deallocate(multi_cutcell_var%rho)
            deallocate(multi_cutcell_var%eint)
            deallocate(multi_cutcell_var%etot)
            deallocate(multi_cutcell_var%pres)
            deallocate(multi_cutcell_var%phase_pres)
            deallocate(multi_cutcell_var%phase_rho)
            !deallocate(multi_cutcell_var%phase_velx) ! not allocated for now
            deallocate(multi_cutcell_var%phase_vely)
            deallocate(multi_cutcell_var%phase_velz)
        end subroutine deallocate_multi_cutcell_type
      end module multi_cutcell_mod