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

      !||====================================================================
      !||    brokmann_random_def_mod   ../common_source/modules/brokmann_random_def_mod.F90
      !||--- called by ------------------------------------------------------
      !||    brokmann_crack_init       ../starter/source/materials/fail/windshield_alter/brokmann_crack_init.F90
      !||    brokmann_elem_renum       ../starter/source/materials/fail/windshield_alter/brokmann_elem_spmd_renum.F90
      !||    brokmann_random           ../starter/source/materials/fail/windshield_alter/brokmann_random.F90
      !||    c3init3                   ../starter/source/elements/sh3n/coque3n/c3init3.F
      !||    cbainit3                  ../starter/source/elements/shell/coqueba/cbainit3.F
      !||    cinit3                    ../starter/source/elements/shell/coque/cinit3.F
      !||    fail_windshield_init      ../starter/source/materials/fail/windshield_alter/fail_windshield_init.F
      !||    initia                    ../starter/source/elements/initia/initia.F
      !||    lectur                    ../engine/source/input/lectur.F
      !||    updfail                   ../starter/source/materials/updfail.F90
      !||====================================================================
      module brokmann_random_def_mod

! ======================================================================================================================
!! \brief data structure for random walk algorithm used in fractal damage initialization
!! \details
! ======================================================================================================================
        implicit none
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
      type brokmann_elem_
        integer :: id                    ! element id
        integer :: elnum                 ! element number
        integer :: nix                   ! number of shell nodes (3/4)
        my_real :: random(6)             ! generated random numbers
      end type brokmann_elem_

      type brokmann_
        integer :: imat
        integer :: nelem
        type (brokmann_elem_), dimension(:), allocatable :: brokmann_elem  ! (nelem)
      end type brokmann_

      type fail_brokmann_               ! (nfail_fractal)
        integer :: nfail                ! number of brokmann dmg models (global)
        type (brokmann_) ,dimension(:), allocatable :: brokmann   ! (nfail)
      end type fail_brokmann_
! ----------------------------------------------------------------------------------------------------------------------
      end module brokmann_random_def_mod
