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
!||    analysis_kinking_failure         ../engine/source/materials/mat/mat123/analysis_failure.F90
!||--- called by ------------------------------------------------------
!||    sigeps123c             ../engine/source/materials/mat/mat123/signeps123c.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod       ../common_source/modules/constant_mod.F
!||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
!||====================================================================
      module analyze_failure_mod
      implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief max finding algo based on Brent's algo
!! \details
!||====================================================================
!||    analyze_failure         ../engine/source/materials/mat/mat123/analyze_failure.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================     
             subroutine  analyze_failure(S_T, S_L, Y_t, mu_T, mu_L, type, &
                                                  sigma_b, sigma_c, tau_bc, tau_ab, tau_ca, &
                                                  critical_phi, max_f)
!----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        use matparam_def_mod 
        use constant_mod 
        use failure_tools_mod
        use precision_mod, only : WP 
!! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      ! Material properties
      real(kind=wp) , intent(in) :: S_T, S_L, Y_t, mu_T, mu_L
       ! Stress components
      real(kind=wp), intent(in) :: sigma_b, sigma_c, tau_bc, tau_ab, tau_ca
       ! Results
       real(kind=wp) , intent(out):: critical_phi, max_f
       integer ,intent(in) :: type
!-----------------------------------------------------------------------------------------------------------------------
!                                              L o c a l   V a r i a b l e s
!-----------------------------------------------------------------------------------------------------------------------
       integer :: status ! status of the minimization
       integer :: maxiter
       real(kind=wp) :: a, b, tol, fmin
! ----------------------------------------------------------------------------------------------------------------------  

     !! use, intrinsic :: iso_fortran_env, only: dp => real64
     !! use optimize, only: minimize_scalar

     ! Use Brent's optimization method (recommended)
      a = zero
      b = pi
      tol = em08
      maxiter = 100
      call brent_minimize(a, b , tol, type, maxiter ,&
                                  S_T, S_L, Y_t, mu_T, mu_L, &
                                  sigma_b, sigma_c, tau_bc, tau_ab, tau_ca ,&
                                  critical_phi, fmin,status)
      max_f = -fmin  ! Convert back from minimization
      !
      end subroutine analyze_failure

      end  module analyze_failure_mod
