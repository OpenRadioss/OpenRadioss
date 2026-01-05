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
!||    failure_tools_mod       ../engine/source/materials/mat/mat123/failure_tools_mod.F90
!||--- called by ------------------------------------------------------
!||    analyze_failure         ../engine/source/materials/mat/mat123/analyze_failure.F90
!||    analyze_failure_trial   ../engine/source/materials/mat/mat123/analyze_failure_trial.F90
!||====================================================================
      module failure_tools_mod
      implicit none
!=====================================================================================================================
! Function interface for callbacks
      contains
! ======================================================================================================================
!                                                   FUNCTION
! ======================================================================================================================

  ! Main failure criterion function (Eq. 3.89 and 3.90)
!||====================================================================
!||    negative_f_kink   ../engine/source/materials/mat/mat123/failure_tools_mod.F90
!||--- called by ------------------------------------------------------
!||    brent_minimize    ../engine/source/materials/mat/mat123/failure_tools_mod.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod      ../common_source/modules/constant_mod.F
!||    precision_mod     ../common_source/modules/precision_mod.F90
!||====================================================================
       function negative_f_kink(phi, S_T, S_L, Y_t, mu_T, mu_L, &
                    sigma_bm, sigma_cp, tau_bm_cp, tau_am_bm, tau_cp_am) result(f_val)
        ! 
        use constant_mod , only : zero, one, two,half
        use precision_mod, only : WP
        implicit none
            ! Input parameters

        real(kind=wp) , intent(in) :: phi
        real(kind=wp) , intent(in) :: S_T, S_L, Y_t, mu_T, mu_L
       ! Stress components
        real(kind=wp) , intent(in) :: sigma_bm, sigma_cp, tau_bm_cp, tau_am_bm, tau_cp_am


        real(kind=wp) :: sigma_n, tau_T, tau_L, cos2p, sin2p, cosp, sinp
        real(kind=wp) :: denom_T, denom_L,f_val
        
        ! Calculate trigonometric functions
        cos2p = cos(two * phi)
        sin2p = sin(two * phi)
        cosp = cos(phi)
        sinp = sin(phi)
        
        ! Calculate tractions on the fracture plane (Eq. 3.91)
        sigma_n = (sigma_bm + sigma_cp)*half + &
                  (sigma_bm - sigma_cp)*half * cos2p + &
                  tau_bm_cp * sin2p
        tau_T = -(sigma_bm - sigma_cp)*half * sin2p + &
                tau_bm_cp * cos2p
        tau_L = tau_am_bm * cosp + tau_cp_am * sinp


        
        ! Select appropriate failure criterion based on normal stress
        if (sigma_n <= zero) then
            ! Eq. 3.89: Compressive/shear dominated regime
            denom_T = S_T - mu_T * sigma_n
            denom_L = S_L - mu_L * sigma_n
            
            ! Avoid division by zero or negative denominators
            if (denom_T <= zero .or. denom_L <= zero) then
                f_val = huge(one)  ! Certain failure
            else
                f_val = (tau_T / denom_T)**2 + (tau_L / denom_L)**2
            end if
        else
            ! Eq. 3.90: Tensile dominated regime
            f_val = (sigma_n / Y_t)**2 + (tau_T / S_T)**2 + (tau_L / S_L)**2
        end if
        f_val = -f_val ! We negate to convert max problem to min problem
      end function negative_f_kink
! ======================================================================================================================
!                                                   FUNCTION
! ======================================================================================================================

  ! Main failure criterion function (Eq. 3.89 and 3.90)
!||====================================================================
!||    negative_f_matrix   ../engine/source/materials/mat/mat123/failure_tools_mod.F90
!||--- called by ------------------------------------------------------
!||    brent_minimize      ../engine/source/materials/mat/mat123/failure_tools_mod.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod        ../common_source/modules/constant_mod.F
!||    precision_mod       ../common_source/modules/precision_mod.F90
!||====================================================================
       function negative_f_matrix(phi, S_T, S_L, Y_t, mu_T, mu_L, &
                    sigma_b, sigma_c, tau_bc, tau_ab, tau_ca) result(f_val)
        ! 
        use constant_mod , only : zero, one, two,half
        use precision_mod, only : WP
        implicit none
            ! Input parameters

        real(kind=wp) , intent(in) :: phi
        real(kind=wp) , intent(in) :: S_T, S_L, Y_t, mu_T, mu_L
       ! Stress components
        real(kind=wp) , intent(in) :: sigma_b, sigma_c, tau_bc, tau_ab, tau_ca


        real(kind=wp) :: sigma_n, tau_T, tau_L, cos2p, sin2p, cosp, sinp
        real(kind=wp) :: denom_T, denom_L,f_val,aa,bb
        
        ! Calculate trigonometric functions
        cos2p = cos(two * phi)
        sin2p = sin(two * phi)
        cosp = cos(phi)
        sinp = sin(phi)
        ! Calculate tractions on the fracture plane (Eq. 3.91)
        aa = half*(sigma_b + sigma_c) 
        bb = half*(sigma_b - sigma_c)
        sigma_n = aa + bb*cos2p + tau_bc*sin2p
        tau_T = -bb*sin2p + tau_bc*cos2p
        tau_L = tau_ab*cosp + tau_ca*sinp
        ! Select appropriate failure criterion based on normal stress
        if (sigma_n <= zero) then
            ! Eq. 3.89: Compressive/shear dominated regime
            denom_T = S_T - mu_T * sigma_n
            denom_L = S_L - mu_L * sigma_n
            
            ! Avoid division by zero or negative denominators
            if (denom_T <= zero .or. denom_L <= zero) then
                f_val = huge(one)  ! Certain failure
            else
                f_val = (tau_T / denom_T)**2 + (tau_L / denom_L)**2
            end if
        else
            ! Eq. 3.90: Tensile dominated regime
            f_val = (sigma_n / Y_t)**2 + (tau_T / S_T)**2 + (tau_L / S_L)**2
        end if
        f_val = -f_val ! We negate to convert max problem to min problem
      end function negative_f_matrix

! ======================================================================================================================
!                                                   SUBROUTINE
! ======================================================================================================================
       ! Brent's minimization algorithm
!||====================================================================
!||    brent_minimize      ../engine/source/materials/mat/mat123/failure_tools_mod.F90
!||--- called by ------------------------------------------------------
!||    analyze_failure     ../engine/source/materials/mat/mat123/analyze_failure.F90
!||--- calls      -----------------------------------------------------
!||    negative_f_kink     ../engine/source/materials/mat/mat123/failure_tools_mod.F90
!||    negative_f_matrix   ../engine/source/materials/mat/mat123/failure_tools_mod.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod        ../common_source/modules/constant_mod.F
!||    precision_mod       ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine brent_minimize(a, b, tol, type, maxiter, &
                    S_T, S_L, Y_t, mu_T, mu_L,    &
                    sigma_b, sigma_c, tau_bc, tau_ab, tau_ca, & 
                    xmin, fmin, status)
         !
        use constant_mod , only : zero, one, two,three,five,em08,half
        use precision_mod, only : WP 
        implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ---------------------------------------------------------------------------------------------------------------------- 
        real(kind=wp), intent(in) :: a, b
        real(kind=wp), intent(in) :: S_T, S_L, Y_t, mu_T, mu_L
        real(kind=wp), intent(in)  :: sigma_b, sigma_c, tau_bc, tau_ab, tau_ca
        real(kind=wp), intent(out) :: xmin, fmin
        integer, intent(out) :: status
        integer, intent(in) :: type
        real(kind=wp), intent(in), optional :: tol
        integer, intent(in), optional :: maxiter
! ----------------------------------------------------------------------------------------------------------------------
!                                                   L o c a l   V a r i a b l e s
! ----------------------------------------------------------------------------------------------------------------------   
        real(kind=wp) :: local_tol, a_local, b_local, c, d, e, etemp
        real(kind=wp) :: fu, fv, fw, fx, p, q, r, tol1, tol2, u, v, w, x, xm
        integer :: local_maxiter, iter
        real(kind=wp), parameter :: golden_ratio = (three- sqrt(five)) * half
        real(kind=wp), parameter :: eps = epsilon(one)
        
        ! Set default parameters
        local_tol = em08
        if (present(tol)) local_tol = tol
        local_maxiter = 100
        if (present(maxiter)) local_maxiter = maxiter
        
        a_local = a
        b_local = b
        status = 1  ! Assume failure initially
        
        ! Initial points
        w = a_local + golden_ratio * (b_local - a_local)
        v = w
        x = w
        !type == 1 - fiber
        !type == 2 - matrix
        if(type == 1) then
          fw = negative_f_kink(w, S_T, S_L, Y_t, mu_T, mu_L, &
                      sigma_b, sigma_c, tau_bc, tau_ab, tau_ca) !! sigma_bm, sigma_cpsi, tau_bm_cpsi, tau_am_bm, tau_cpsi_am 
         else
          fw = negative_f_matrix(w, S_T, S_L, Y_t, mu_T, mu_L, &
                     sigma_b, sigma_c,tau_bc, tau_ab, tau_ca)  !! sigma_b, sigma_c, tau_bc, tau_ab, tau_ca ! for shell sigma_c=zero
         endif             
         fv = fw
         fx = fw
         d = zero
         e = zero
        !!
        do iter = 1, local_maxiter
            xm = half * (a_local + b_local)
            tol1 = local_tol * abs(x) + eps
            tol2 = two* tol1
            
            ! Check convergence
            if (abs(x - xm) <= tol2 - half * (b_local - a_local)) then
                xmin = x
                fmin = fx
                status = 0
                return
            end if
           !! 
            if (abs(e) > tol1) then
                ! Attempt parabolic interpolation
                r = (x - w) * (fx - fv)
                q = (x - v) * (fx - fw)
                p = (x - v) * q - (x - w) * r
                q = two * (q - r)
                if (q > zero) p = -p
                q = abs(q)
                etemp = e
                e = d
                
                if (abs(p) < abs(half * q * etemp) .and. &
                    p > q * (a_local - x) .and. p < q * (b_local - x)) then
                    ! Parabolic interpolation
                    d = p / q
                    u = x + d
                    if (u - a_local < tol2 .or. b_local - u < tol2) then
                        d = sign(tol1, xm - x)
                    end if
                else
                    ! Golden section
                    e = merge(a_local - x, b_local - x, x >= xm)
                    d = golden_ratio * e
                end if
            else
                ! Golden section
                e = merge(a_local - x, b_local - x, x >= xm)
                d = golden_ratio * e
            end if
            
            u = x + merge(d, sign(tol1, d), abs(d) >= tol1)
            if(type == 1) then
                 fu = negative_f_kink(u, S_T, S_L, Y_t, mu_T, mu_L, &
                      sigma_b, sigma_c, tau_bc, tau_ab, tau_ca) 
            else
                 fu = negative_f_matrix(u, S_T, S_L, Y_t, mu_T, mu_L, &
                     sigma_b, sigma_c,tau_bc, tau_ab, tau_ca)  
            endif                      
            !!
            ! Update brackets
            if (fu <= fx) then
                if (u >= x) then
                    a_local = x
                else
                    b_local = x
                end if
                v = w; fv = fw
                w = x; fw = fx
                x = u; fx = fu
            else
                if (u < x) then
                    a_local = u
                else
                    b_local = u
                end if
                if (fu <= fw .or. abs(w - x) < eps) then
                    v = w; fv = fw
                    w = u; fw = fu
                else if (fu <= fv .or. abs(v - x) < eps .or. abs(v - w) < eps) then
                    v = u; fv = fu
                end if
            end if
        end do
        
        ! Maximum iterations reached
        xmin = x
        fmin = fx
        status = 2
       end subroutine brent_minimize
  !
       end module failure_tools_mod
