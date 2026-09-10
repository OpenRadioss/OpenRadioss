!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
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
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine transform_translate_in_local_skew( &
        &   nodes  ,n_nodes ,x    ,numnod    ,skew_trans   ,&
        &   tx     ,ty      ,tz   )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use MY_ALLOC_MOD, only : my_alloc
          use my_dealloc_mod, only : my_dealloc
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
          real(kind=WP),                             intent(in) :: tx                           !< Translation distance in X direction
          real(kind=WP),                             intent(in) :: ty                           !< Translation distance in Y direction
          real(kind=WP),                             intent(in) :: tz                           !< Translation distance in Z direction
          real(kind=WP),                             intent(inout) :: x(3, numnod)              !< Coordinates of all nodes in model
          real(kind=WP),                             intent(inout) :: skew_trans(12)               !< Skew translation vector
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i, igrnod                                                      !< local entities id's
          real(kind=WP) :: invertskew_x(3),invertskew_Y(3),invertskew_Z(3)          !< Inverse of skew matrix
          real(kind=WP) :: orig(3)                                                  !< origin of skew system
          real(kind=WP) :: detskew                                                  !< Determinant of skew matrix
          real(kind=WP) :: norm(3)                                                  !< Norms of inverse skew vectors
          real(kind=WP), dimension(:,:), allocatable :: xn                          !< Transformed coordinates of group nodes
          real(kind=WP) :: tx_loc(3)                                                !< Translation vector in local skew system
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          call my_alloc(xn, 3, n_nodes, "xn")
          xn(:,:) = 0.0_WP
          orig(:) = HUGE(0.0_WP)
          tx_loc(:) = 0.0_WP

          orig(1)=skew_trans(10)
          orig(2)=skew_trans(11)
          orig(3)=skew_trans(12)

          ! group nodes coordinates in local skew system
          do i=1, n_nodes
            igrnod = nodes(i)
            xn(1,i) = skew_trans(1)*(x(1,igrnod) - orig(1)) + skew_trans(2)*(x(2,igrnod) - orig(2)) + &
            &         skew_trans(3)*(x(3,igrnod) - orig(3))
            xn(2,i) = skew_trans(4)*(x(1,igrnod) - orig(1)) + skew_trans(5)*(x(2,igrnod) - orig(2)) + &
            &         skew_trans(6)*(x(3,igrnod) - orig(3))
            xn(3,i) = skew_trans(7)*(x(1,igrnod) - orig(1)) + skew_trans(8)*(x(2,igrnod) - orig(2)) + &
            &         skew_trans(9)*(x(3,igrnod) - orig(3))
          end do
          ! translation vector in local skew system
          tx_loc(1) = skew_trans(1)*tx+ skew_trans(2)*ty+ skew_trans(3)*tz
          tx_loc(2) = skew_trans(4)*tx+ skew_trans(5)*ty+ skew_trans(6)*tz
          tx_loc(3) = skew_trans(7)*tx+ skew_trans(8)*ty+ skew_trans(9)*tz

          ! Compute inverse of skew matrix for transforming back to global coordinates
          ! skew is a 3x3 matrix stored in column-major order

           detskew = (skew_trans(1)*skew_trans(5)*skew_trans(9)) + (skew_trans(4)*skew_trans(8)*skew_trans(3))&
            &      + (skew_trans(7)*skew_trans(2)*skew_trans(6)) &
            &      - (skew_trans(7)*skew_trans(5)*skew_trans(3)) - (skew_trans(8)*skew_trans(6)*skew_trans(1))&
            &      - (skew_trans(9)*skew_trans(2)*skew_trans(4))

          detskew = max(detskew, 1e-20)

          ! Compute inverse using formula for 3x3 matrix
          ! inv(A) = 1/det(A) * adj(A)

          invertskew_x(1) = (skew_trans(5)*skew_trans(9)-skew_trans(6)*skew_trans(8))/detskew
          invertskew_x(2) = (skew_trans(8)*skew_trans(3)-skew_trans(2)*skew_trans(9))/detskew
          invertskew_x(3) = (skew_trans(2)*skew_trans(6)-skew_trans(5)*skew_trans(3))/detskew

          invertskew_Y(1) = (skew_trans(7)*skew_trans(6)-skew_trans(4)*skew_trans(9))/detskew
          invertskew_Y(2) = (skew_trans(1)*skew_trans(9)-skew_trans(7)*skew_trans(3))/detskew
          invertskew_Y(3) = (skew_trans(4)*skew_trans(3)-skew_trans(1)*skew_trans(6))/detskew

          invertskew_Z(1) = (skew_trans(4)*skew_trans(8)-skew_trans(7)*skew_trans(5))/detskew
          invertskew_Z(2) = (skew_trans(7)*skew_trans(2)-skew_trans(1)*skew_trans(8))/detskew
          invertskew_Z(3) = (skew_trans(1)*skew_trans(5)-skew_trans(4)*skew_trans(2))/detskew

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

          ! Translate the group nodes in all directions

          ! skewed translation of group nodes in in all three directions
          do i=1, n_nodes
            igrnod = nodes(i)
            xn(1,i) = xn(1,i) + tx_loc(1)
            xn(2,i) = xn(2,i) + tx_loc(2)
            xn(3,i) = xn(3,i) + tx_loc(3)

            ! local to global transformation after translation in local system

            x(1,igrnod) = invertskew_x(1) * (xn(1,i)) + invertskew_x(2) * (xn(2,i)) &
            &           + invertskew_x(3) * (xn(3,i)) + orig(1)
            x(2,igrnod) = invertskew_Y(1) * (xn(1,i)) + invertskew_Y(2) * (xn(2,i)) &
            &           + invertskew_Y(3) * (xn(3,i)) + orig(2)
            x(3,igrnod) = invertskew_Z(1) * (xn(1,i)) + invertskew_Z(2) * (xn(2,i)) &
            &           + invertskew_Z(3) * (xn(3,i)) + orig(3)
            end do
            ! update the skew translation vector with the new origin after translation
            skew_trans(10) = orig(1) + tx
            skew_trans(11) = orig(2) + ty
            skew_trans(12) = orig(3) + tz

          if(allocated(xn)) call my_dealloc(xn)
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine transform_translate_in_local_skew
      end module transform_translate_in_local_skew_mod
