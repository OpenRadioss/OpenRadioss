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
!! \brief write therm_param data structure in restart file
!! \details
      !||====================================================================
      !||    write_therpmaram_mod   ../starter/source/materials/mat/write_thermparam.F90
      !||--- called by ------------------------------------------------------
      !||    write_matparam         ../starter/source/materials/mat/write_matparam.F
      !||====================================================================
      module write_therpmaram_mod
        implicit none
      contains

      !||====================================================================
      !||    write_thermparam   ../starter/source/materials/mat/write_thermparam.F90
      !||--- called by ------------------------------------------------------
      !||    write_matparam     ../starter/source/materials/mat/write_matparam.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        subroutine write_thermparam(therm,len)
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
          type(therm_param_) ,intent(in)    :: therm
          integer            ,intent(inout) :: len
! --------------------------------------------------------------------------------------------------
!                                                   Local variables
! --------------------------------------------------------------------------------------------------
          integer :: ifix,rfix
          integer ,dimension(:) ,allocatable :: ibuf
          real(kind=WP) ,dimension(:) ,allocatable :: rbuf
! --------------------------------------------------------------------------------------------------
!                                                   Body
! --------------------------------------------------------------------------------------------------
          ! write integer parameters
          ifix = 2
          allocate (ibuf(ifix))
!
          ibuf(1) = therm%iform
          ibuf(2) = therm%func_thexp
!
          call write_i_c(ibuf,ifix)
          deallocate(ibuf)

          ! write real value parameters
          rfix = 10
          allocate (rbuf(rfix))
!
          rbuf(1)  = therm%tini
          rbuf(2)  = therm%tref
          rbuf(3)  = therm%tmelt
          rbuf(4)  = therm%rhocp
          rbuf(5)  = therm%as
          rbuf(6)  = therm%bs
          rbuf(7)  = therm%al
          rbuf(8)  = therm%bl
          rbuf(9)  = therm%efrac
          rbuf(10) = therm%scale_thexp
!
          call write_db(rbuf,rfix)
          deallocate(rbuf)
!
          len = len + ifix + rfix
!-----------
          return
        end subroutine write_thermparam

      end module write_therpmaram_mod
