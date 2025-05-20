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
!! \brief initialize temperature in integration points in shells if defined by /initemp
!! \details
! ==================================================================================================
      !||====================================================================
      !||    initemp_shell_mod   ../starter/source/materials/therm/initemp_shell.F90
      !||--- called by ------------------------------------------------------
      !||    read_material_models       ../starter/source/materials/read_material_models.F
      !||====================================================================
      module initemp_shell_mod
      contains

      !||====================================================================
      !||    initemp_shell   ../starter/source/materials/therm/initemp_shell.F90
      !||--- called by ------------------------------------------------------
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      subroutine initemp_shell(                                                     &
                 elbuf_str, temp  ,nel ,numnod ,numsh  ,nshnod ,nix  ,ix   )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use elbufdef_mod
      use constant_mod, only : zero
!============================================================================
      implicit none
! ----------------------------------------------------------------------------------------
!     Included files
! ----------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer ,intent(in) :: numnod
      integer ,intent(in) :: nshnod
      integer ,intent(in) :: nel
      integer ,intent(in) :: nix
      integer ,intent(in) :: numsh
      integer ,dimension(nix,numsh) ,intent(in)    :: ix   
      my_real ,dimension(numnod)    ,intent(in)    :: temp
      type(elbuf_struct_)           ,intent(inout) :: elbuf_str
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i,j,ilay,ir,is,it
      integer ,dimension(nel) :: tempel
!=========================================================================================
      ! calculate mean element temperature from nodal temperature   
      tempel(1:nel) = zero
      do j = 1,nshnod
        do i = 1,nel
          tempel(i) = tempel(i) + temp(ix(j+1,i)) / nshnod
        end do
      end do
!
      !  distribute element temperature to integration points  
!
      do ilay=1,elbuf_str%nlay
        if (elbuf_str%bufly(ilay)%l_temp > 0) then
          do ir=1,elbuf_str%nptr
            do is=1,elbuf_str%npts
              do it=1,elbuf_str%bufly(ilay)%nptt
                elbuf_str%bufly(ilay)%lbuf(ir,is,it)%temp(1:nel) = tempel(1:nel)
              end do
            end do
          end do
        end if
      end do
!-----------
      return
      end  subroutine initemp_shell      
      end  module initemp_shell_mod      

