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
      module polygon_mod
        implicit none
#include  "my_real.inc"

        type polygon_point_
          my_real :: y
          my_real :: z
        end type polygon_point_

        type polygon_
          type(polygon_point_), allocatable, dimension(:) :: point
          integer :: numpoint ! defined points
          integer :: size ! allocated size numpoint <= size)
          my_real :: area
          my_real :: diag ! maximum dimension along y and z
        end type polygon_

        type polygon_list_
          integer num_polygons
          type(polygon_),allocatable,dimension(:) :: polygon
        end type polygon_list_

      contains

! ======================================================================================================================
!                                                   FUNCTION
! ======================================================================================================================
!! \brief add 'point' in poly data structure.
!! \details  pre-condition, allocation must be correctly sized, otherwise an error message is displayed
        function polygon_addpoint(poly, point) result(ierr)
          use constant_mod , only : zero
          implicit none
#include  "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(polygon_), intent(inout) :: poly
          type(polygon_point_), intent(in) :: point
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: numpt, isize
          integer ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          numpt = poly%numpoint
          isize = poly%size
          ierr=1
          if(numpt >= isize)then
            write(*,*) "** ERROR : unexpected situation with polygon_addpoint"
            return
          end if
          numpt = numpt+1
          poly%numpoint = numpt
          poly%point(numpt)%y = point%y;
          poly%point(numpt)%z = point%z;
          ierr = 0
          !area not recomputed for performance reason. It has to be calculated once the polygon is fully built
        end function polygon_addpoint



! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief allocate poly with size numnode and zeroing
!! \details
        subroutine polygon_create(poly, numnodes)
          use constant_mod , only : zero
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(polygon_), intent(out) :: poly
          integer, intent(in) :: numnodes
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          poly%size = numnodes
          poly%numpoint = 0
          allocate(poly%point(numnodes));
          poly%point(1:numnodes)%y = zero;
          poly%point(1:numnodes)%z = zero;
          poly%area = zero
          poly%diag = zero
        end subroutine polygon_create



! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
        subroutine polygon_zeroing(poly)
          use constant_mod , only : zero
          implicit none
#include  "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(polygon_) :: poly
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          integer ii
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          poly%numpoint = 0
          do ii=1,poly%size
            poly%point(ii)%y = zero;
            poly%point(ii)%z = zero;
            poly%diag = zero
            poly%area = zero
          end do
        end subroutine polygon_zeroing



! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
        subroutine polygon_destroy(poly)
          implicit none
#include  "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(polygon_), intent(out) :: poly
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          if (allocated(poly%point))deallocate(poly%point)
        end subroutine polygon_destroy



! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
        subroutine polygon_list_destroy(list)
          use constant_mod , only : zero
          implicit none
#include  "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(polygon_list_), intent(out) :: list
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ii
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          do ii=1,list%num_polygons
            call polygon_destroy(list%polygon(ii))
          end do
        end subroutine polygon_list_destroy



! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief copy Base_polygon into Target_polygon (allocated inside this subroutine)
!! \details
        subroutine polygon_copy(Base_polygon, Target_polygon)
          implicit none
#include  "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(polygon_), intent(in) :: Base_polygon
          type(polygon_), intent(out) :: Target_polygon
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
          integer Base_size
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          Base_size = Base_polygon%size
          if(Base_size > 0)then
            allocate(Target_polygon%point(Base_polygon%size))
            Target_polygon%point(1:Base_size)%y = Base_polygon%point(1:Base_size)%y
            Target_polygon%point(1:Base_size)%z = Base_polygon%point(1:Base_size)%z
            Target_polygon%numpoint = Base_polygon%numpoint
            Target_polygon%size = Base_polygon%size
            Target_polygon%area = Base_polygon%area
          else
            ! not expected
            stop 220582
          end if
        end subroutine polygon_copy


    end module polygon_mod
