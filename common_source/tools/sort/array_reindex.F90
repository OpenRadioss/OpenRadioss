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
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief   This subroutine is reindexing an INTEGER array using index(1:n) array
!! \details      Example array = (/ 110 220 330 440/)
!! \details              index = (/4 3 2 1/)
!! \details      result will be  (/ 440 330 220 110/)


          subroutine integer_array_reindex(array, index, n)
            implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
            integer, intent(in) :: n
            integer, intent(inout) :: array(n)
            integer, intent(inout) :: index(n)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
            integer :: ii
            integer :: temp_array(n)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
            temp_array(1:n)=array(1:n)
            do ii = 1, n
              array(ii) = temp_array(index(ii))
            end do
          end subroutine integer_array_reindex

! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief   This subroutine is reindexing a REAL array using index(1:n) array
!! \details      Example array = (/ 110.1 220.1 330.1 440.1/)
!! \details              index = (/4 3 2 1/)
!! \details      result will be  (/ 440.1 330.1 220.1 110.1/)
          subroutine real_array_reindex(array, index, n)
            implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
            integer, intent(in) :: n
            my_real, intent(inout) :: array(n)
            integer, intent(inout) :: index(n)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
            integer :: ii
            my_real :: temp_array(n)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
            temp_array(1:n)=array(1:n)
            do ii = 1, n
              array(ii) = temp_array(index(ii))
            end do
          end subroutine real_array_reindex

! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief   This subroutine is reindexing a array of POINTS using index(1:n) array
!! \details      Example array = (/ P1 P2 P3 P4/)
!! \details              index = (/4 3 2 1/)
!! \details      result will be  (/ P4 P3 P2 P1 /)
          subroutine points_array_reindex(array, index, n)
            use polygon_clipping_mod , only : polygon_point_
            implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
            integer, intent(in) :: n
            type(polygon_point_), intent(inout) :: array(n)
            integer, intent(inout) :: index(n)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
            integer :: ii
            type(polygon_point_) :: temp_array(n)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
            do ii=1,n
              temp_array(ii)%y=array(ii)%y
              temp_array(ii)%z=array(ii)%z
            end do
            do ii = 1, n
              array(ii)%y = temp_array(index(ii))%y
              array(ii)%z = temp_array(index(ii))%z
            end do
          end subroutine points_array_reindex