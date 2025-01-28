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
!! \brief  Type for AMS working directory
      !||====================================================================
      !||    file_descriptor_mod   ../engine/source/modules/file_descriptor_mod.F90
      !||--- called by ------------------------------------------------------
      !||    mulawc                ../engine/source/materials/mat_share/mulawc.F90
      !||====================================================================
        module ams_work_mod
            implicit none
#include      "my_real.inc"
            type ams_check_                                  !< replace module name in sms_fsa_inv (sms_check routine)
                  integer, dimension(:), allocatable :: iadm
                  integer, dimension(:), allocatable :: jadm
                  integer, dimension(:), allocatable :: kadm
                  integer, dimension(:), allocatable :: jdim
                  integer, dimension(:), allocatable :: jdim2
                  integer, dimension(:), allocatable :: isortnd
                  integer, dimension(:), allocatable :: invnd

                  my_real, dimension(:), allocatable :: diag_m
                  my_real, dimension(:), allocatable :: lt_m
                  my_real, dimension(:), allocatable :: lt_m2
                  my_real, dimension(:), allocatable :: diag_inv
                  integer nndft0
                  integer nndft1
                  integer nnzm
            end type ams_check_

            type ams_work_
               type (ams_check_) :: check
            end type ams_work_

        end module ams_work_mod

