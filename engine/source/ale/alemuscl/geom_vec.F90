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
!||    geom_vec_mod                    ../engine/source/ale/alemuscl/geom_vec.F90
!||--- called by ------------------------------------------------------
!||    ale51_gradient_reconstruction   ../engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
!||====================================================================
      module geom_vec_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief 
!! \details
!||====================================================================
!||    geom_vec                        ../engine/source/ale/alemuscl/geom_vec.F90
!||--- called by ------------------------------------------------------
!||    ale51_gradient_reconstruction   ../engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
!||--- uses       -----------------------------------------------------
!||    constant_mod                    ../common_source/modules/constant_mod.F
!||    debug_mod                       ../engine/share/modules/debug_mod.F
!||    mvsiz_mod                       ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod                   ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine geom_vec(nel,nft,n_entity,a,b,c,elcenter)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : half,zero
          use precision_mod, only : WP
          use mvsiz_mod, only : MVSIZ



          USE DEBUG_MOD
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
          integer, intent(in) :: nel !< number of elements
          integer, intent(in) :: nft !< first element index
          integer, intent(in) :: n_entity !< number of entities
          real(WP), dimension(mvsiz,3,12), intent(in) :: a !< first point of the triangle
          real(WP), dimension(mvsiz,3,12), intent(in) :: b !< second point of the triangle
          real(WP), dimension(mvsiz,3,12), intent(in) :: c !< third point of the triangle
          real(WP), dimension(n_entity,3), intent(out) :: elcenter !< output : element center
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,ijk,ii
          real(kind=WP) :: ab,bc,ca
          real(kind=WP), dimension(3) :: norm          
          real(kind=WP), dimension(nel) :: vol
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to minimum ]
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          vol(1:nel) = zero
          do ijk=1,12
            do i=1,nel
              ii = i + nft
              norm(1) = (b(i,2,ijk) - a(i,2,ijk)) * (c(i,3,ijk) - a(i,3,ijk)) - &
                        (b(i,3,ijk) - a(i,3,ijk)) * (c(i,2,ijk) - a(i,2,ijk))
              norm(2) = (b(i,3,ijk) - a(i,3,ijk)) * (c(i,1,ijk) - a(i,1,ijk)) - &
                        (b(i,1,ijk) - a(i,1,ijk)) * (c(i,3,ijk) - a(i,3,ijk))
              norm(3) = (b(i,1,ijk) - a(i,1,ijk)) * (c(i,2,ijk) - a(i,2,ijk)) - &
                        (b(i,2,ijk) - a(i,2,ijk)) * (c(i,1,ijk) - a(i,1,ijk))
              vol(i) = vol(i) + a(i,1,ijk)*norm(1) + a(i,2,ijk)*norm(2) + a(i,3,ijk)*norm(3)
              ab = (half * (a(i,1,ijk) + b(i,1,ijk)))**2 + (half * (a(i,2,ijk) + b(i,2,ijk)))**2  &
                                                         + (half * (a(i,3,ijk) + b(i,3,ijk)))**2

              bc = (half * (b(i,1,ijk) + c(i,1,ijk)))**2 + (half * (b(i,2,ijk) + c(i,2,ijk)))**2  &
                                                         + (half * (b(i,3,ijk) + c(i,3,ijk)))**2

              ca = (half * (c(i,1,ijk) + a(i,1,ijk)))**2 + (half * (c(i,2,ijk) + a(i,2,ijk)))**2  &
                                                         + (half * (c(i,3,ijk) + a(i,3,ijk)))**2
              elcenter(ii,1) = elcenter(ii,1) + norm(1) * (ab + bc + ca)
              elcenter(ii,2) = elcenter(ii,2) + norm(2) * (ab + bc + ca)
              elcenter(ii,3) = elcenter(ii,3) + norm(3) * (ab + bc + ca)
            enddo
          enddo
          vol(1:nel) = vol(1:nel) / 6.0d0
          do ijk=1,3
            do i=1,nel
              ii = i + nft
              elcenter(ii,ijk) = elcenter(ii,ijk) / (12.d0 *vol(i))
            enddo
          enddo

          !do i=1,nel
            !ii = i + nft
            !write(*,*) ncycle_debug,"elcenter O ", II, ELCENTER(II,1), ELCENTER(II,2), ELCENTER(II,3)     
          !enddo

! ----------------------------------------------------------------------------------------------------------------------
        end subroutine geom_vec
      end module geom_vec_mod
