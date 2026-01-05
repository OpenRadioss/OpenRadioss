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
!||    damping_range_compute_param_mod   ../starter/source/general_controls/damping/damping_range_compute_param.F90
!||--- called by ------------------------------------------------------
!||    hm_read_damp                      ../starter/source/general_controls/damping/hm_read_damp.F
!||====================================================================
      module damping_range_compute_param_mod
      implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!\brief This subroutine compute de 3 maxwell components for the damping
!=======================================================================================================================
!
!||====================================================================
!||    damping_range_compute_param   ../starter/source/general_controls/damping/damping_range_compute_param.F90
!||--- called by ------------------------------------------------------
!||    hm_read_damp                  ../starter/source/general_controls/damping/hm_read_damp.F
!||--- calls      -----------------------------------------------------
!||    invert                        ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine damping_range_compute_param(damp_ratio,f_low,f_high,maxwell_alpha,maxwell_tau)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod ,only : zero,one,four,eight,two,em02,half,pi
          use precision_mod ,only : WP
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
          real(kind=WP),                                intent(in   ) :: damp_ratio           !< target damping ration in frequency range
          real(kind=WP),                                intent(in   ) :: f_low                !< lower bound of frequency range
          real(kind=WP),                                intent(in   ) :: f_high               !< upper bound of frequency range
          real(kind=WP),                                intent(inout) :: maxwell_alpha(3)     !< alpha parameters of the 3 maxwell components
          real(kind=WP),                                intent(inout) :: maxwell_tau(3)       !< tau parameters of the 3 maxwell components
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,nerror
          real(kind=WP) :: f_mid,f_max,damp_ratio_sample,e_fac
          real(kind=WP) :: e_max(3),freq_sample(3),factor(3)
          real(kind=WP) :: matrix(3,3),inv_mat(3,3)
! ----------------------------------------------------------------------------------------------------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!         Factor is computed in order that the summation of 3 components give the target damping ratio for 3 different points
!
          f_mid = SQRT(f_low*f_high)
          freq_sample(1) = f_low
          freq_sample(2) = f_mid
          freq_sample(3) = f_high
!
          factor(1) = 0.965
          factor(2) = one
          factor(3) = 0.965
!
          do i=1,3
            do j=1,3
              f_max = freq_sample(j)
              damp_ratio_sample = (four*damp_ratio*(freq_sample(i)/f_max))/(two + two*(freq_sample(i)/f_max)**2)
              matrix(i,j) = damp_ratio_sample
            end do
          end do
!
          call invert(matrix,inv_mat,3,nerror)
!
!         Parameters are found by solving matrix^-1 . freq_sample
!
          do i=1,3
            e_fac = zero
            do j=1,3
              e_fac= e_fac + inv_mat(i,j)*damp_ratio*factor(j)
            end do
            e_max(i) = e_fac*damp_ratio
          end do
!
!         Computation of alpha and tau for each maxwell component
!
          do i=1,3
            maxwell_alpha(i) = eight*e_max(i)**2+four*e_max(i)*SQRT(four*e_max(i)**2+one)
            maxwell_tau(i) = one/(sqrt(maxwell_alpha(i)+one)*two*pi*freq_sample(i))
          end do
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine damping_range_compute_param
      end module damping_range_compute_param_mod
