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
      module get_metis_arguments_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Get command line arguments related to METIS, and set the corresponding variables                                                              
        subroutine get_metis_arguments(legacy_partitioning, metis_random_seed)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          logical, intent(out) :: legacy_partitioning
          integer, intent(out) :: metis_random_seed
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
          character(len=255) :: arg
          integer :: num_args
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          legacy_partitioning = .true.
          metis_random_seed = 0
          ! look for -METIS_SEED in the command line arguments, and set metis_random_seed accordingly
          num_args = command_argument_count()
          do i = 1, num_args
            call get_command_argument(i, arg)
            if (arg == '-METIS_SEED') then
              if (i < num_args) then
                call get_command_argument(i + 1, arg)
                read(arg, *) metis_random_seed
                write(*,*) 'Using METIS random seed: ', metis_random_seed
                if (metis_random_seed < 0) then
                  legacy_partitioning = .false.
                end if
              else
                print *, 'Error: -METIS_SEED argument provided without a seed value.'
              end if
            endif
          end do
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine get_metis_arguments
      end module get_metis_arguments_mod
