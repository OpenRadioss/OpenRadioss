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
!||    conjugate_gradient_vec_mod    ../engine/source/ale/alemuscl/conjugate_gradient_vec.F90
!||--- called by ------------------------------------------------------
!||    gradient_reconstruction_new   ../engine/source/ale/alemuscl/gradient_reconstruction.F90
!||====================================================================
      module conjugate_gradient_vec_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!||====================================================================
!||    conjugate_gradient_vec        ../engine/source/ale/alemuscl/conjugate_gradient_vec.F90
!||--- called by ------------------------------------------------------
!||    gradient_reconstruction_new   ../engine/source/ale/alemuscl/gradient_reconstruction.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                  ../common_source/modules/constant_mod.F
!||    precision_mod                 ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine conjugate_gradient_vec(nel,dim,max_iter,mat,rhs,sol,tol)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod , only : WP
          use constant_mod , only : zero
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
          integer, intent(in) :: dim !< system dimension
          integer, intent(in) :: max_iter !< maximum number of iterations          
          real(kind=WP), intent(in) :: tol !< tolerance for convergence          
          real(kind=WP), dimension(nel,dim,dim), intent(in) :: mat !< matrix
          real(kind=WP), dimension(nel,dim), intent(in) :: rhs !< right hand side
          real(kind=WP), dimension(nel,dim), intent(inout) :: sol !< Vector solution
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,iter
          real(kind=WP), dimension(nel,dim) :: r ! residual
          real(kind=WP), dimension(nel,dim) :: rnew ! new residual
          real(kind=WP), dimension(nel,dim) :: p ! search direction
          real(kind=WP), dimension(nel,dim) :: temp ! temporary vector
          real(kind=WP), dimension(nel) :: alpha ! step length
          real(kind=WP), dimension(nel) :: beta ! scaling factor
          real(kind=WP), dimension(nel) :: norm_init ! initial norm of the residual
          real(kind=WP), dimension(nel) :: error ! normalized error
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
          ! initialize the vector solution to 0.
          sol(1:nel,1:dim) = zero
          ! initialization of the algorithm
          do i=1,nel
            r(i,1) = rhs(i,1) - (mat(i,1,1)*sol(i,1) + mat(i,1,2)*sol(i,2) + mat(i,1,3)*sol(i,3))
            r(i,2) = rhs(i,2) - (mat(i,2,1)*sol(i,1) + mat(i,2,2)*sol(i,2) + mat(i,2,3)*sol(i,3))
            r(i,3) = rhs(i,3) - (mat(i,3,1)*sol(i,1) + mat(i,3,2)*sol(i,2) + mat(i,3,3)*sol(i,3))
          enddo
          
          p(1:nel,1:dim) = r(1:nel,1:dim)
          do i=1,nel
            norm_init(i) = max(ABS(r(i,1)),ABS(r(i,2)),ABS(r(i,3)))
          enddo          
          !norm_init(1:nel) = MAXVAL(ABS(r(1:nel,1:dim)))
          error(1:nel) = norm_init(1:nel)

          do iter=1,max_iter
            do i=1,nel
              if(error(i) > tol) then
                temp(i,1) = mat(i,1,1)*p(i,1) + mat(i,1,2)*p(i,2) + mat(i,1,3)*p(i,3)
                temp(i,2) = mat(i,2,1)*p(i,1) + mat(i,2,2)*p(i,2) + mat(i,2,3)*p(i,3)
                temp(i,3) = mat(i,3,1)*p(i,1) + mat(i,3,2)*p(i,2) + mat(i,3,3)*p(i,3)
                alpha(i) = (r(i,1)*r(i,1) + r(i,2)*r(i,2) + r(i,3)*r(i,3)) / &
                          (temp(i,1)*p(i,1) + temp(i,2)*p(i,2) + temp(i,3)*p(i,3))
                sol(i,1) = sol(i,1) + alpha(i) * p(i,1)
                sol(i,2) = sol(i,2) + alpha(i) * p(i,2)
                sol(i,3) = sol(i,3) + alpha(i) * p(i,3)
                rnew(i,1) = r(i,1) - alpha(i) * temp(i,1)
                rnew(i,2) = r(i,2) - alpha(i) * temp(i,2)
                rnew(i,3) = r(i,3) - alpha(i) * temp(i,3)
                beta(i) = (rnew(i,1)*rnew(i,1) + rnew(i,2)*rnew(i,2) + rnew(i,3)*rnew(i,3)) / &
                          (r(i,1)*r(i,1) + r(i,2)*r(i,2) + r(i,3)*r(i,3))

                p(i,1) = rnew(i,1) + beta(i) * p(i,1)
                p(i,2) = rnew(i,2) + beta(i) * p(i,2)
                p(i,3) = rnew(i,3) + beta(i) * p(i,3)
                r(i,1) = rnew(i,1)
                r(i,2) = rnew(i,2)
                r(i,3) = rnew(i,3)
                error(i) = max(ABS(r(i,1)),ABS(r(i,2)),ABS(r(i,3))) / norm_init(i)
              endif
            enddo
          enddo
        end subroutine conjugate_gradient_vec
      end module conjugate_gradient_vec_mod
