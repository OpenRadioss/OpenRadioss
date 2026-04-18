!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    eikonal_lmax_mod               ../starter/source/initial_conditions/detonation/eikonal_Lmax.F90
!||--- called by ------------------------------------------------------
!||    eikonal_fast_marching_method   ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
!||====================================================================
      module eikonal_Lmax_mod
      implicit none
      contains
!||====================================================================
!||    eikonal_lmax                   ../starter/source/initial_conditions/detonation/eikonal_Lmax.F90
!||--- called by ------------------------------------------------------
!||    eikonal_fast_marching_method   ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine eikonal_Lmax(neldet, xel, Lmax, nvois)
!! \brief Initialization of detonation velocity in case of mixture (multimaterial law)
!! \details laws 51 and 151
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use constant_mod , only : ep20,em20
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,intent(in) :: neldet                  !< number of explosive cells
          integer,intent(in) :: nvois                   !< number of ajacent elems, used to deduced 2d/3d case
          real(kind=WP),intent(inout) :: Lmax           !< max reference length
          real(kind=WP),intent(inout) :: xel(3,neldet) !< all centroids
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ii
          real(kind=WP) :: xx(2),yy(2),zz(2),diag(2)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          xx(1:2) = (/ ep20, -ep20 /)
          yy(1:2) = (/ ep20, -ep20 /)
          zz(1:2) = (/ ep20, -ep20 /)

          if(nvois >= 6)then
  
            do ii=1,neldet
              xx(1) = min(xx(1),xel(1,ii))
              yy(1) = min(yy(1),xel(2,ii))
              zz(1) = min(zz(1),xel(3,ii))
              xx(2) = max(xx(2),xel(1,ii))
              yy(2) = max(yy(2),xel(2,ii))
              zz(2) = max(zz(2),xel(3,ii))
            enddo
            diag(1) = min(xx(1),yy(1),zz(1))
            diag(2) = max(xx(2),yy(2),zz(2))

          else
  
            do ii=1,neldet
              yy(1) = min(yy(1),xel(2,ii))
              zz(1) = min(zz(1),xel(3,ii))
              yy(2) = max(yy(2),xel(2,ii))
              zz(2) = max(zz(2),xel(3,ii))
            enddo
            diag(1) = min(yy(1),zz(1))
            diag(2) = max(yy(2),zz(2))
      
          endif
  
          Lmax = max(em20,diag(2)-diag(1))

        end subroutine eikonal_Lmax
! ----------------------------------------------------------------------------------------------------------------------
      end module eikonal_Lmax_mod
