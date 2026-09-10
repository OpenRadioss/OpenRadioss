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
!||    local_to_global_skew_mod   ../starter/source/model/transformation/local_to_global_skew.F90
!||--- called by ------------------------------------------------------
!||    lecsubmod                               ../starter/source/model/submodel/lecsubmod.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      module local_to_global_skew_mod
        use precision_mod, only : WP
        implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Compute local to global translation using skew transformation matrix
!=======================================================================================================================
!||====================================================================
!||    local_to_global_skew   ../starter/source/model/transformation/local_to_global_skew.F90
!||--- called by ------------------------------------------------------
!||    lecsubmod                           ../starter/source/model/submodel/lecsubmod.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine local_to_global_skew(tx,ty,tz,skew_trans)
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
          real(kind=WP),                             intent(inout) :: tx                           !< Translation distance in X direction
          real(kind=WP),                             intent(inout) :: ty                           !< Translation distance in Y direction
          real(kind=WP),                             intent(inout) :: tz                           !< Translation distance in Z direction
          real(kind=WP),                             intent(in) :: skew_trans(12)                  !< Skew translation vector
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          real(kind=WP) :: invertskew_x(3),invertskew_Y(3),invertskew_Z(3)          !< Inverse of skew matrix
          real(kind=WP) :: detskew                                                  !< Determinant of skew matrix
          real(kind=WP) :: norm(3)                                                  !< Norms of inverse skew vectors
          real(kind=WP) :: txyz(3)                                                  !< translation vector in local coordinates
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

            txyz(1) = tx
            txyz(2) = ty
            txyz(3) = tz

            ! Compute inverse of skew matrix for transforming back to global coordinates
            ! skew is a 3x3 matrix stored in column-major order

            detskew = (skew_trans(1)*skew_trans(5)*skew_trans(9)) + (skew_trans(4)*skew_trans(8)*skew_trans(3))&
            &       + (skew_trans(7)*skew_trans(2)*skew_trans(6)) &
            &       - (skew_trans(7)*skew_trans(5)*skew_trans(3)) - (skew_trans(8)*skew_trans(6)*skew_trans(1))&
            &       - (skew_trans(9)*skew_trans(2)*skew_trans(4))

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

            ! Translate local to global

            tx = invertskew_x(1) * txyz(1) + invertskew_x(2) * txyz(2) &
            &  + invertskew_x(3) * txyz(3)
            ty = invertskew_Y(1) * txyz(1) + invertskew_Y(2) * txyz(2) &
            &  + invertskew_Y(3) * txyz(3)
            tz = invertskew_Z(1) * txyz(1) + invertskew_Z(2) * txyz(2) &
            &  + invertskew_Z(3) * txyz(3)

! ----------------------------------------------------------------------------------------------------------------------
        end subroutine local_to_global_skew
      end module local_to_global_skew_mod
