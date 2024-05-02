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
!===================================================================================================

      module random_walk_def_mod

! ======================================================================================================================
!! \brief data structure for random walk algorithm used in fractal damage initialization
!! \details 
! ======================================================================================================================
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------

      type random_walk_  
        integer :: id                                   ! element id
        integer :: elnum                                ! element number
        integer :: nix                                  ! number of edge connexions (3/4)
        my_real :: damage                               ! damage value 
        integer, dimension(:), allocatable :: neighbor  ! neighbor element list (nix)
      end type random_walk_ 
            
      type fractal_  
        integer :: imat
        integer :: nelem
        type (random_walk_), dimension(:), allocatable :: random_walk  ! (nelem)
      end type fractal_ 
      
      type fail_fractal_               ! (nfail_fractal)
        integer :: nfail               ! number of fractal_dmg models (global)
        type (fractal_) ,dimension(:), allocatable :: fractal   ! (nfail_fractal)     
      end type fail_fractal_ 
! ----------------------------------------------------------------------------------------------------------------------
      end module random_walk_def_mod
