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
! ==================================================================================================
!                                                   PROCEDURES
! ==================================================================================================
!! \brief read ale rezoning data structure from restart file
!! \details
!||====================================================================
!||    read_ale_rezoning_param_mod   ../engine/source/output/restart/read_ale_rezoning_param.F90
!||--- called by ------------------------------------------------------
!||    read_matparam                 ../engine/source/output/restart/read_matparam.F
!||====================================================================
      module read_ale_rezoning_param_mod
      contains

!||====================================================================
!||    read_ale_rezoning_param   ../engine/source/output/restart/read_ale_rezoning_param.F90
!||--- called by ------------------------------------------------------
!||    read_matparam             ../engine/source/output/restart/read_matparam.F
!||--- calls      -----------------------------------------------------
!||    read_i_c                  ../common_source/tools/input_output/write_routtines.c
!||--- uses       -----------------------------------------------------
!||    ale_mod                   ../common_source/modules/ale/ale_mod.F
!||====================================================================
      subroutine read_ale_rezoning_param(rezon)
! --------------------------------------------------------------------------------------------------
!                                                   Modules
! --------------------------------------------------------------------------------------------------
      use ale_mod , only : ale_rezon_
! --------------------------------------------------------------------------------------------------
!                                                   Implicit none
! --------------------------------------------------------------------------------------------------
      implicit none
! --------------------------------------------------------------------------------------------------
!                                                   Arguments
! --------------------------------------------------------------------------------------------------
      type(ale_rezon_) ,intent(inout)    :: rezon
! --------------------------------------------------------------------------------------------------
!                                                   Local variables
! --------------------------------------------------------------------------------------------------
      integer :: iad,ifix
      integer ,dimension(:) ,allocatable :: ibuf
! --------------------------------------------------------------------------------------------------
!                                                   Body
! --------------------------------------------------------------------------------------------------
      ! read integer parameters
      ifix = 2
      allocate (ibuf(ifix))
      call read_i_c(ibuf,ifix)
!
      iad = 1
        rezon%num_nuvar_mat = ibuf(iad)
      iad = iad+1
        rezon%num_nuvar_eos = ibuf(iad)
!
      deallocate(ibuf)

!-----------
      return
      end subroutine read_ale_rezoning_param

      end module read_ale_rezoning_param_mod
