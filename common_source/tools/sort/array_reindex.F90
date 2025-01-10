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
module array_reindex_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief   This subroutine is reindexing an INTEGER array using index(1:n) array
!! \details      Example array = (/ 110 220 330 440/)
!! \details              index = (/4 3 2 1/)
!! \details      result will be  (/ 440 330 220 110/)


      !||====================================================================
      !||    integer_array_reindex      ../common_source/tools/sort/array_reindex.F90
      !||--- called by ------------------------------------------------------
      !||    clipping_weiler_atherton   ../common_source/tools/clipping/polygon_clipping_mod.F90
      !||====================================================================
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
      !||====================================================================
      !||    real_array_reindex         ../common_source/tools/sort/array_reindex.F90
      !||--- called by ------------------------------------------------------
      !||    iniebcs_propergol_get_cv   ../starter/source/boundary_conditions/ebcs/iniebcs_propergol.F90
      !||====================================================================
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

end module array_reindex_mod