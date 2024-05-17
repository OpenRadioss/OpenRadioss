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
      module cast_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Convert a double precision value to a my_real value
        pure function double_to_my_real(x,nan_replacement, max_value) result(res)
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, intent(in) :: x !< Input value
          my_real :: res !< result
          my_real, intent(in), optional :: nan_replacement !< replacement value for NaN
          my_real, intent(in), optional :: max_value !< replacement value for values greater than max_value
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          my_real :: nan_replacement_local
          my_real :: max_value_local
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          if( present(nan_replacement) ) then
            nan_replacement_local = nan_replacement
          else
            nan_replacement_local = 0
          end if
          if ( present(max_value) ) then
            max_value_local = max_value
          else
            max_value_local = huge(res)
          end if

          if( x .ne. x ) then
            res = nan_replacement_local
          else if( x < -max_value_local ) then
            res = -max_value_local
          else if( x > max_value_local ) then
            res = max_value_local
#ifndef MYREAL8
          else if( x < -huge(res) ) then
            res = -huge(res)
          else if( x > huge(res) ) then
            res = huge(res)
          else if( x < -tiny(res) .and. x .ne. 0) then
            res = -tiny(res)
          else if( x > tiny(res) .and. x .ne. 0) then
            res = tiny(res)
#endif
          else
            res = x
          end if

        end function double_to_my_real
      end module cast_mod
