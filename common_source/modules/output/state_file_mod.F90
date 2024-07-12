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

      module state_file_mod

! ======================================================================================================================
!! \brief data structure for state files output
!! \details
! ======================================================================================================================
         implicit none

         type state_
            integer  stat_numelsph,stat_numelsph_g
            logical is_stat_sph

            integer, dimension(:), allocatable :: stat_sph
         end type state_


         contains

         
         subroutine state_init(state,mx_stat)
            

           type(state_),intent(inout)  :: state
           integer, intent(in) :: mx_stat

           state%stat_numelsph = 0
           state%stat_numelsph_g = 0
           state%is_stat_sph = .false.

           allocate(state%stat_sph(mx_stat))
           state%stat_sph(1:mx_stat) = 0
           
         end subroutine state_init


      end module state_file_mod
