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
!||    analyze_failure_trial_mod   ../engine/source/materials/mat/mat123/analyze_failure_trial.F90
!||--- called by ------------------------------------------------------
!||    sigeps123                   ../engine/source/materials/mat/mat123/sigeps123.F90
!||    sigeps123c                  ../engine/source/materials/mat/mat123/sigeps123c.F90
!||====================================================================
      module analyze_failure_trial_mod
      implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief max finding algo based on Brent's algo
!! \details
!||====================================================================
!||    analyze_failure_trial   ../engine/source/materials/mat/mat123/analyze_failure_trial.F90
!||--- called by ------------------------------------------------------
!||    sigeps123               ../engine/source/materials/mat/mat123/sigeps123.F90
!||    sigeps123c              ../engine/source/materials/mat/mat123/sigeps123c.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod            ../common_source/modules/constant_mod.F
!||    failure_tools_mod       ../engine/source/materials/mat/mat123/failure_tools_mod.F90
!||    matparam_def_mod        ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod           ../common_source/modules/precision_mod.F90
!||====================================================================
             subroutine  analyze_failure_trial(S_T, S_L, Y_t, mu_T, mu_L, type, &
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
       real(kind=wp), intent(out) :: critical_phi, max_f
       integer ,intent(in) :: type
!-----------------------------------------------------------------------------------------------------------------------
!                                              L o c a l   V a r i a b l e s
!-----------------------------------------------------------------------------------------------------------------------
       integer :: status ! status of the minimization
       integer :: maxiter,i,n_trials
       real(kind=wp) :: a, b, tol, fmin,f_kink,sigma_n,tau_T,tau_L,phi
       real(kind=wp) :: cos2p,sin2p,cosp,sinp,denom_T,denom_L,f_matrix
! ----------------------------------------------------------------------------------------------------------------------  

     !! use, intrinsic :: iso_fortran_env, only: dp => real64
     !! use optimize, only: minimize_scalar

     n_trials = 180
     max_f = zero 
     if(type ==1 ) then ! for fiber kinking failure analysis
      do i = 1, n_trials
        phi = (i-1) * pi / n_trials  ! 0 to pi
     ! Calculate trigonometric functions
        cos2p = cos(two * phi)
        sin2p = sin(two * phi)
        cosp = cos(phi)
        sinp = sin(phi)
        
        ! Calculate tractions on the fracture plane (Eq. 3.91)
        sigma_n = (sigma_b + sigma_c)*half + &
                  (sigma_b - sigma_c)*half * cos2p +  tau_bc * sin2p
        tau_T = -(sigma_b - sigma_c)*half * sin2p + tau_bc * cos2p
        tau_L = tau_ab * cosp + tau_ca * sinp
    
    ! Apply kinking failure criterion
        if (sigma_n <= 0.0d0) then
           ! Eq. 3.89: Compressive/shear dominated regime
          !!  denom_T = S_T - mu_T * sigma_n
          !!  denom_L = S_L - mu_L * sigma_n
            
            ! Avoid division by zero or negative denominators
          !!  if (denom_T <= zero .or. denom_L <= zero) then
         !!       f_val = huge(one)  ! Certain failure
         !!   else
         !!       f_val = (tau_T / denom_T)**2 + (tau_L / denom_L)**2
         !!   end if
        ! Compression side (friction-based criterion)
            f_kink = (tau_T/(S_T - mu_T*sigma_n))**2 +  &
                (tau_L/(S_L - mu_L*sigma_n))**2
        else
        ! Tension side (quadratic criterion)  
            f_kink = (sigma_n/Y_t)**2 + (tau_T/S_T)**2 + (tau_L/S_L)**2
        endif
    
    ! Find maximum failure index and corresponding angle
        if (f_kink > max_f) then
           max_f = f_kink
           critical_phi = phi
        endif
      end do
     else ! for matrix failure analysis
        do i = 1, n_trials
          phi = (i-1) * pi / n_trials  ! 0 to pi
       ! Calculate trigonometric functions
          cos2p = cos(two * phi)
          sin2p = sin(two * phi)
          cosp = cos(phi)
          sinp = sin(phi)
          
          ! Calculate tractions on the fracture plane (Eq. 3.91)
          sigma_n = (sigma_b + sigma_c)*half + &
                    (sigma_b - sigma_c)*half * cos2p +  tau_bc * sin2p
          tau_T = -(sigma_b - sigma_c)*half * sin2p + tau_bc * cos2p
          tau_L = tau_ab * cosp + tau_ca * sinp
      
      ! Apply matrix failure criterion
          if (sigma_n <= zero) then
            ! Eq. 3.89: Compressive/shear dominated regime
            denom_T = S_T - mu_T * sigma_n
            denom_L = S_L - mu_L * sigma_n
            
            ! Avoid division by zero or negative denominators
            if (denom_T <= zero .or. denom_L <= zero) then
                f_matrix = huge(one)  ! Certain failure
            else
              f_matrix = (tau_T / denom_T)**2 + (tau_L / denom_L)**2
            end if
          else
            ! Eq. 3.90: Tensile dominated regime
            f_matrix = (sigma_n / Y_t)**2 + (tau_T / S_T)**2 + (tau_L / S_L)**2
          end if
      
      ! Find maximum failure index and corresponding angle
          if (f_matrix > max_f) then
             max_f = f_matrix
             critical_phi = phi
          endif
        end do



     endif  

      !
      end subroutine analyze_failure_trial

      end  module analyze_failure_trial_mod
