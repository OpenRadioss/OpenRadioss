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
!! \brief calculate element temperature in integration point from nodal values
!! \details
! ==================================================================================================
      !||====================================================================
      !||    s20temp_mod   ../starter/source/elements/solid/solide20/s20temp.F
      !||--- called by ------------------------------------------------------
      !||====================================================================
      module s20temp_mod
      contains

      !||====================================================================
      !||    s20temp   ../starter/source/elements/solid/solide20/s20temp.F
      !||--- called by ------------------------------------------------------
      !||====================================================================

      subroutine s20temp(nel ,numnod ,mvsiz ,npe, nc, ni, temp ,tempel)

!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use constant_mod, only : zero
!-----------------------------------------------
      implicit none
!----------------------------------------------------------------------------------------
!   Included files
!----------------------------------------------------------------------------------------
#include "my_real.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer ,intent(in) :: nel
      integer ,intent(in) :: numnod
      integer ,intent(in) :: mvsiz
      integer ,intent(in) :: npe
      integer ,dimension(mvsiz,npe) ,intent(in)  :: nc       !< element connectivity
      my_real ,dimension(npe)       ,intent(in)  :: ni       !< form functions
      my_real ,dimension(numnod)    ,intent(in)  :: temp     !< nodal temperature
      my_real ,dimension(nel)       ,intent(out) :: tempel   !< element temperature in Gauss point
!-----------------------------------------------
!   L o c a l   v a r i a b l e s
!-----------------------------------------------
      integer i,j
!=========================================================================================
      tempel(1:nel) = zero 
      do i=1,nel
        do j=1,npe                                                  
          tempel(i) = tempel(i) + ni(j) * temp(nc(i,j)) 
        enddo  
      enddo  
!-----------
      return
      end
      end module s20temp_mod
