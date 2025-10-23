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
!||    transform_translate_in_local_skew_mod   ../starter/source/model/transformation/transform_translate_in_local_skew.F90
!||--- called by ------------------------------------------------------
!||    lecsubmod                               ../starter/source/model/submodel/lecsubmod.F
!||    lectrans                                ../starter/source/model/transformation/lectrans.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      module transform_translate_in_local_skew_mod
        use precision_mod, only : WP
        implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Compute minimum distance from a group of nodes to a surface
!=======================================================================================================================
!||====================================================================
!||    transform_translate_in_local_skew   ../starter/source/model/transformation/transform_translate_in_local_skew.F90
!||--- called by ------------------------------------------------------
!||    lecsubmod                           ../starter/source/model/submodel/lecsubmod.F
!||    lectrans                            ../starter/source/model/transformation/lectrans.F
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine transform_translate_in_local_skew( &
                          &   nodes  ,n_nodes ,x    ,numnod ,isk   ,&
                          &   tx     ,ty      ,tz   ,skew   ,lskew ,&
                          &   sskew  )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                                   intent(in) :: nodes(n_nodes)               !< Group of nodes
          integer,                                   intent(in) :: n_nodes                      !< Number of nodes in group
          integer,                                   intent(in) :: numnod                       !< Total number of nodes in model
          integer,                                   intent(in) :: isk                          !< Index of skew (0 if no skew)
          real(kind=WP),                             intent(in) :: tx                           !< Translation distance in X direction
          real(kind=WP),                             intent(in) :: ty                           !< Translation distance in Y direction
          real(kind=WP),                             intent(in) :: tz                           !< Translation distance in Z direction
          integer,                                   intent(in) :: lskew                        !< Length of skew
          integer,                                   intent(in) :: sskew                        !< Sum of skews
          real(kind=WP),                             intent(inout) :: x(3, numnod)              !< Coordinates of all nodes in model
          real(kind=WP),                             intent(in) :: skew(lskew,sskew/lskew)      !< Skew matrices
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i, igrnod                                                      !< local entities id's
          real(kind=WP) :: invertskew_x(3),invertskew_Y(3),invertskew_Z(3)          !< Inverse of skew matrix
          real(kind=WP) :: orig(3)                                                  !< origin of skew system
          real(kind=WP) :: detskew                                                  !< Determinant of skew matrix
          real(kind=WP) :: norm(3)                                                  !< Norms of inverse skew vectors
          real(kind=WP), dimension(:,:), allocatable :: xn                          !< Transformed coordinates of group nodes
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(xn(3, n_nodes))
          xn(:,:) = 0.0_WP
          orig(:) = HUGE(0.0_WP)

          if (isk > 0) then
            orig(1)=skew(10,isk)
            orig(2)=skew(11,isk)
            orig(3)=skew(12,isk)

            ! group nodes coordinates in skew system
            do i=1, n_nodes
              igrnod = nodes(i)
              xn(1,i) = skew(1,isk)*(x(1,igrnod) - orig(1)) + skew(2,isk)*(x(2,igrnod) - orig(2)) + &
                     &  skew(3,isk)*(x(3,igrnod) - orig(3))
              xn(2,i) = skew(4,isk)*(x(1,igrnod) - orig(1)) + skew(5,isk)*(x(2,igrnod) - orig(2)) + &
                     &  skew(6,isk)*(x(3,igrnod) - orig(3))
              xn(3,i) = skew(7,isk)*(x(1,igrnod) - orig(1)) + skew(8,isk)*(x(2,igrnod) - orig(2)) + &
                     &  skew(9,isk)*(x(3,igrnod) - orig(3))
            end do
          else
            do i=1, n_nodes
              igrnod = nodes(i)
              xn(1,i) = x(1,igrnod)
              xn(2,i) = x(2,igrnod)
              xn(3,i) = x(3,igrnod)
            end do
          endif
          
          ! Compute inverse of skew matrix for transforming back to global coordinates
          ! skew is a 3x3 matrix stored in column-major order

          ! error : correct above
          ! skew_X = (skew(1,isk), skew(2,isk), skew(3,isk)) ! - X axis direction
          ! skew_Y = (skew(4,isk), skew(5,isk), skew(6,isk)) ! - Y axis direction
          ! skew_Z = (skew(7,isk), skew(8,isk), skew(9,isk)) ! - Z axis direction

          if (isk > 0) then
            detskew = (skew(1,isk)*skew(5,isk)*skew(9,isk)) + (skew(4,isk)*skew(8,isk)*skew(3,isk))&
                  & + (skew(7,isk)*skew(2,isk)*skew(6,isk)) &
                  & - (skew(7,isk)*skew(5,isk)*skew(3,isk)) - (skew(8,isk)*skew(6,isk)*skew(1,isk))&
                  & - (skew(9,isk)*skew(2,isk)*skew(4,isk))

            detskew = max(detskew, 1e-20)

            ! Compute inverse using formula for 3x3 matrix
            ! inv(A) = 1/det(A) * adj(A)

            invertskew_x(1) = (skew(5,isk)*skew(9,isk)-skew(6,isk)*skew(8,isk))/detskew
            invertskew_x(2) = (skew(8,isk)*skew(3,isk)-skew(2,isk)*skew(9,isk))/detskew
            invertskew_x(3) = (skew(2,isk)*skew(6,isk)-skew(5,isk)*skew(3,isk))/detskew

            invertskew_Y(1) = (skew(7,isk)*skew(6,isk)-skew(4,isk)*skew(9,isk))/detskew
            invertskew_Y(2) = (skew(1,isk)*skew(9,isk)-skew(7,isk)*skew(3,isk))/detskew
            invertskew_Y(3) = (skew(4,isk)*skew(3,isk)-skew(1,isk)*skew(6,isk))/detskew

            invertskew_Z(1) = (skew(4,isk)*skew(8,isk)-skew(7,isk)*skew(5,isk))/detskew
            invertskew_Z(2) = (skew(7,isk)*skew(2,isk)-skew(1,isk)*skew(8,isk))/detskew
            invertskew_Z(3) = (skew(1,isk)*skew(5,isk)-skew(4,isk)*skew(2,isk))/detskew

            ! normalize the inverse vectors to ensure they are unit vectors
            norm(1) = sqrt(invertskew_x(1)**2 + invertskew_x(2)**2 + invertskew_x(3)**2)
            norm(2) = sqrt(invertskew_Y(1)**2 + invertskew_Y(2)**2 + invertskew_Y(3)**2)
            norm(3) = sqrt(invertskew_Z(1)**2 + invertskew_Z(2)**2 + invertskew_Z(3)**2)
          
            invertskew_x(1) = invertskew_x(1)/norm(1)
            invertskew_x(2) = invertskew_x(2)/norm(1)
            invertskew_x(3) = invertskew_x(3)/norm(1)

            invertskew_Y(1) = invertskew_Y(1)/norm(2)
            invertskew_Y(2) = invertskew_Y(2)/norm(2)
            invertskew_Y(3) = invertskew_Y(3)/norm(2)

            invertskew_Z(1) = invertskew_Z(1)/norm(3)
            invertskew_Z(2) = invertskew_Z(2)/norm(3)
            invertskew_Z(3) = invertskew_Z(3)/norm(3)
          else
            ! For global coordinates, set inverse to identity
            detskew = 1.0_WP
            invertskew_x(1) = 1.0_WP
            invertskew_x(2) = 0.0_WP
            invertskew_x(3) = 0.0_WP

            invertskew_Y(1) = 0.0_WP
            invertskew_Y(2) = 1.0_WP
            invertskew_Y(3) = 0.0_WP

            invertskew_Z(1) = 0.0_WP
            invertskew_Z(2) = 0.0_WP
            invertskew_Z(3) = 1.0_WP
          end if

          ! Translate the group nodes in all directions

          if (isk == 0) then
            ! global translation of group nodes in all three directions
            do i=1, n_nodes
              igrnod = nodes(i)
              x(1,igrnod) = xn(1,i) + tx
              x(2,igrnod) = xn(2,i) + ty
              x(3,igrnod) = xn(3,i) + tz
            end do
          else
            ! skewed translation of group nodes in in all three directions
            do i=1, n_nodes
              igrnod = nodes(i)
              xn(1,i) = xn(1,i) + tx
              xn(2,i) = xn(2,i) + ty
              xn(3,i) = xn(3,i) + tz

              ! local to global transformation after translation in local system

              x(1,igrnod) = invertskew_x(1) * (xn(1,i)) + invertskew_x(2) * (xn(2,i)) &
                        & + invertskew_x(3) * (xn(3,i)) + orig(1)
              x(2,igrnod) = invertskew_Y(1) * (xn(1,i)) + invertskew_Y(2) * (xn(2,i)) &
                        & + invertskew_Y(3) * (xn(3,i)) + orig(2)
              x(3,igrnod) = invertskew_Z(1) * (xn(1,i)) + invertskew_Z(2) * (xn(2,i)) &
                        & + invertskew_Z(3) * (xn(3,i)) + orig(3)
            end do
          end if

          if(allocated(xn)) deallocate(xn)
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine transform_translate_in_local_skew
      end module transform_translate_in_local_skew_mod
