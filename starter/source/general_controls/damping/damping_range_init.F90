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
      !||====================================================================
      !||    damping_range_init_mod   ../starter/source/general_controls/damping/damping_range_init.F90
      !||--- called by ------------------------------------------------------
      !||    initia                   ../starter/source/elements/initia/initia.F
      !||====================================================================
      module damping_range_init_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!\brief This subroutine transfer parameters of maxwell component in group buffer
!=======================================================================================================================
!
      !||====================================================================
      !||    damping_range_init   ../starter/source/general_controls/damping/damping_range_init.F90
      !||--- called by ------------------------------------------------------
      !||    initia               ../starter/source/elements/initia/initia.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
        subroutine damping_range_init(ndamp,nrdamp,dampr,ngroup,nparg,iparg,elbuf_tab)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod ,only : zero,one,four,eight,two,em02,half,pi
          use elbufdef_mod ,only : elbuf_struct_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                                intent(in   ) :: nrdamp                 !< first dimension of idampr
          integer,                                intent(in   ) :: ndamp                  !< number of damping conditions
          integer,                                intent(in   ) :: ngroup                 !< number of groups
          integer,                                intent(in   ) :: nparg                  !< number of groups          
          integer,                                intent(in   ) :: iparg(nparg,ngroup)    !< structure of integer per group
          my_real,                                intent(in   ) :: dampr(nrdamp,ndamp)    !< structure of damping parameters           
          type (elbuf_struct_), target, dimension(ngroup), intent(inout) :: elbuf_tab     !< structure of group buffer
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ng,idamp_freq_range
! ----------------------------------------------------------------------------------------------------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!          
          do ng=1,ngroup  
            idamp_freq_range = iparg(93,ng)
            if (idamp_freq_range > 0) then
              elbuf_tab(ng)%damp_range%alpha(1:3) = dampr(32:34,idamp_freq_range)
              elbuf_tab(ng)%damp_range%tau(1:3)   = dampr(35:37,idamp_freq_range) 
            endif
          enddo       
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine damping_range_init
      end module damping_range_init_mod
