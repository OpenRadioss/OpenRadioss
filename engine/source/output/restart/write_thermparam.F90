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
! ==================================================================================================
!                                                   PROCEDURES
! ==================================================================================================
!! \brief write therm_param data structure in restart file
!! \details
      !||====================================================================
      !||    write_thermparam        ../starter/source/materials/mat/write_thermparam.F
      !||--- called by ------------------------------------------------------
      !||    write_matparam         ../starter/source/materials/mat/write_matparam.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      module write_therpmaram_mod
      contains

      subroutine write_thermparam(therm)
! --------------------------------------------------------------------------------------------------
!                                                   Modules
! --------------------------------------------------------------------------------------------------
      use therm_param_mod
! --------------------------------------------------------------------------------------------------
!                                                   Implicit none
! --------------------------------------------------------------------------------------------------
      implicit none
! --------------------------------------------------------------------------------------------------
!                                                   Included files
! --------------------------------------------------------------------------------------------------
#include "my_real.inc"
! --------------------------------------------------------------------------------------------------
!                                                   Arguments
! --------------------------------------------------------------------------------------------------
      type(therm_param_) ,intent(in)    :: therm
! --------------------------------------------------------------------------------------------------
!                                                   Local variables
! --------------------------------------------------------------------------------------------------
      integer :: iad,ifix,rfix
      integer ,dimension(:) ,allocatable :: ibuf
      my_real ,dimension(:) ,allocatable :: rbuf
! --------------------------------------------------------------------------------------------------
!                                                   Body
! --------------------------------------------------------------------------------------------------
      ! write integer parameters
      ifix = 2
      allocate (ibuf(ifix))
!
      iad = 1
        ibuf(iad) = therm%iform
      iad = iad+1
        ibuf(iad) = therm%func_thexp
!
      call write_i_c(ibuf,ifix)
      deallocate(ibuf)

      ! write real value parameters
      rfix = 9
      allocate (rbuf(rfix))
!
      iad = 1
        rbuf(iad) = therm%tref
      iad = iad+1
        rbuf(iad) = therm%tmelt
      iad = iad+1
        rbuf(iad) = therm%rhocp
      iad = iad+1
        rbuf(iad) = therm%as
      iad = iad+1
        rbuf(iad) = therm%bs
      iad = iad+1
        rbuf(iad) = therm%al
      iad = iad+1
        rbuf(iad) = therm%bl
      iad = iad+1
        rbuf(iad) = therm%efrac
      iad = iad+1
        rbuf(iad) = therm%scale_thexp
!
      call write_db(rbuf,rfix)
      deallocate(rbuf)
!-----------
      return
      end

      end module write_therpmaram_mod
