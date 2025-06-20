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
!! \brief read therm_param data structure from restart file
!! \details
      !||====================================================================
      !||    read_therpmaram_mod   ../engine/source/output/restart/read_thermparam.F90
      !||--- called by ------------------------------------------------------
      !||    read_matparam         ../engine/source/output/restart/read_matparam.F
      !||====================================================================
      module read_therpmaram_mod
      contains

      !||====================================================================
      !||    read_thermparam   ../engine/source/output/restart/read_thermparam.F90
      !||--- called by ------------------------------------------------------
      !||    read_matparam     ../engine/source/output/restart/read_matparam.F
      !||--- calls      -----------------------------------------------------
      !||    read_db           ../common_source/tools/input_output/read_db.F
      !||    read_i_c          ../common_source/tools/input_output/write_routtines.c
      !||--- uses       -----------------------------------------------------
      !||    precision_mod     ../common_source/modules/precision_mod.F90
      !||    therm_param_mod   ../common_source/modules/mat_elem/therm_param_mod.F90
      !||====================================================================
      subroutine read_thermparam(therm)
! --------------------------------------------------------------------------------------------------
!                                                   Modules
! --------------------------------------------------------------------------------------------------
      use therm_param_mod
      use precision_mod, only : WP
! --------------------------------------------------------------------------------------------------
!                                                   Implicit none
! --------------------------------------------------------------------------------------------------
      implicit none
! --------------------------------------------------------------------------------------------------
!                                                   Arguments
! --------------------------------------------------------------------------------------------------
      type(therm_param_) ,intent(inout)    :: therm
! --------------------------------------------------------------------------------------------------
!                                                   Local variables
! --------------------------------------------------------------------------------------------------
      integer :: iad,ifix,rfix
      integer ,dimension(:) ,allocatable :: ibuf
      real(kind=WP) ,dimension(:) ,allocatable :: rbuf
! --------------------------------------------------------------------------------------------------
!                                                   Body
! --------------------------------------------------------------------------------------------------
      ! read integer parameters
      ifix = 2
      allocate (ibuf(ifix))
      call read_i_c(ibuf,ifix)
!
      iad = 1
        therm%iform = ibuf(iad)
      iad = iad+1
        therm%func_thexp = ibuf(iad)
!
      deallocate(ibuf)

      ! read real value parameters
      rfix = 9
      allocate (rbuf(rfix))
      call read_db(rbuf,rfix)
!
      iad = 1
        therm%tref        = rbuf(iad)
      iad = iad+1
        therm%tmelt       = rbuf(iad)
      iad = iad+1
        therm%rhocp       = rbuf(iad)
      iad = iad+1
        therm%as          = rbuf(iad)
      iad = iad+1
        therm%bs          = rbuf(iad)
      iad = iad+1
        therm%al          = rbuf(iad)
      iad = iad+1
        therm%bl          = rbuf(iad)
      iad = iad+1
        therm%efrac       = rbuf(iad)
      iad = iad+1
        therm%scale_thexp = rbuf(iad)
!
      deallocate(rbuf)
!-----------
      return
      end subroutine read_thermparam

      end module read_therpmaram_mod
