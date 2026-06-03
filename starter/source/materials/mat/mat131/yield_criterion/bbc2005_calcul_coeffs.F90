!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
!||====================================================================
!||    bbc2005_calcul_coeffs_mod
!||    Newton-Raphson Solver with Multi-Start Global Search
!||    (Includes Dynamic LM, Line Search & Step Tolerance)
!||====================================================================
      module bbc2005_calcul_coeffs_mod
        implicit none
      contains

!===============================================================================
!   Dummy function for Newton Raphson calculation
!===============================================================================
!===============================================================================
!   Newton-Raphson Solver for BBC2005 8x8 Non-linear System
!===============================================================================
!||====================================================================
!||    bbc2005_calcul_coeffs             ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_calcul_coeffs.F90
!||--- called by ------------------------------------------------------
!||    hm_read_yield_criterion_bbc2005   ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_BBC2005.F90
!||--- calls      -----------------------------------------------------
!||    bbc2005_evaluate                  ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_evaluate.F90
!||    bbc2005_solve_linear_8x8          ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_solve_linear_8x8.F90
!||--- uses       -----------------------------------------------------
!||    bbc2005_evaluate_mod              ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_evaluate.F90
!||    bbc2005_solve_linear_8x8_mod      ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_solve_linear_8x8.F90
!||====================================================================
        subroutine bbc2005_calcul_coeffs(y0, y45, y90, r0, r45, r90, yb, rb, k_val, &
                                         a, b, l_coeff, m_coeff, n_coeff, p, q, r_coeff, mat_id, titr)
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
            use precision_mod, only : WP
            use bbc2005_evaluate_mod
            use bbc2005_solve_linear_8x8_mod
            use bbc2005_jacobian_mod
            use names_and_titles_mod, only : nchartitle
            use message_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
            implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
            real(kind=8),                intent(in)    :: y0                                      !< yield stress at 0 degrees
            real(kind=8),                intent(in)    :: y45                                     !< yield stress at 45 degrees
            real(kind=8),                intent(in)    :: y90                                     !< yield stress at 90 degrees
            real(kind=8),                intent(in)    :: r0                                      !< strain ratio at 0 degrees
            real(kind=8),                intent(in)    :: r45                                     !< strain ratio at 45 degrees
            real(kind=8),                intent(in)    :: r90                                     !< strain ratio at 90 degrees
            real(kind=8),                intent(in)    :: yb                                      !< yield stress biaxial
            real(kind=8),                intent(in)    :: rb                                      !< strain ratio biaxial
            integer,                     intent(in)    :: k_val                                   !< yield criterion exponent parameter
            real(kind=WP),               intent(out)   :: a                                       !< BBC2005 coefficient a
            real(kind=WP),               intent(out)   :: b                                       !< BBC2005 coefficient b
            real(kind=WP),               intent(out)   :: l_coeff                                 !< BBC2005 coefficient L
            real(kind=WP),               intent(out)   :: m_coeff                                 !< BBC2005 coefficient M
            real(kind=WP),               intent(out)   :: n_coeff                                 !< BBC2005 coefficient N
            real(kind=WP),               intent(out)   :: p                                       !< BBC2005 coefficient P
            real(kind=WP),               intent(out)   :: q                                       !< BBC2005 coefficient Q
            real(kind=WP),               intent(out)   :: r_coeff                                 !< BBC2005 coefficient R
            integer,                     intent(in)    :: mat_id                                  !< material ID
            character(len=nchartitle),   intent(in)    :: titr                                    !< material law user title
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
            real(kind=8) :: x(8), dx(8), F(8), Jac(8,8)
            real(kind=8) :: y_exp(8)
            real(kind=8) :: eps, err_norm
            integer       :: iter, max_iter, i, info
            real(kind=8) :: x_new(8), F_new(8)
            real(kind=8) :: alpha, err_norm_new, damp_factor
            real(kind=8) :: max_dx, scale_factor
            integer       :: ls_iter, max_ls_iter
            logical       :: step_accepted
            real(kind=8) :: guess_pool(3, 8)
            real(kind=8) :: best_x(8), best_err
            integer       :: attempt

            ! 1. Assemble target experimental data
            y_exp(1) = y0 
            y_exp(2) = y45 
            y_exp(3) = y90 
            y_exp(4) = r0
            y_exp(5) = r45 
            y_exp(6) = r90 
            y_exp(7) = yb 
            y_exp(8) = rb

            ! 2. Construct 3 different initial guess points
            ! Configuration 1: Perfect balanced point (handle almost all normal materials and perfectly isotropic bodies)
            guess_pool(1,1)=0.5
            guess_pool(1,2)=0.5 
            guess_pool(1,3)=0.5
            guess_pool(1,4)=0.5
            guess_pool(1,5)=0.5 
            guess_pool(1,6)=0.5
            guess_pool(1,7)=0.5 
            guess_pool(1,8)=0.5
            
            ! Configuration 2: Extreme left bias (prevent stuck in smooth symmetric center, detect left-side minimum valley)
            guess_pool(2,1)=0.7
            guess_pool(2,2)=0.3
            guess_pool(2,3)=0.6
            guess_pool(2,4)=0.4
            guess_pool(2,5)=0.6
            guess_pool(2,6)=0.4 
            guess_pool(2,7)=0.6 
            guess_pool(2,8)=0.4
            
            ! Configuration 3: Extreme right bias (detect right-side minimum valley)
            guess_pool(3,1)=0.3
            guess_pool(3,2)=0.7
            guess_pool(3,3)=0.4 
            guess_pool(3,4)=0.6
            guess_pool(3,5)=0.4 
            guess_pool(3,6)=0.6
            guess_pool(3,7)=0.4 
            guess_pool(3,8)=0.6

            ! Newton's method, converge within 100 steps or reach friction noise
            max_iter = 100       
            max_ls_iter = 50     ! Line search allows bisection 50 times
            
            ! [Unlock 1] Convergence threshold approaches limit
            eps = 1.0E-10
            
            ! Initialize global best record
            best_err = 1.0E30

            best_x(1) = guess_pool(1, 1)
            best_x(2) = guess_pool(1, 2)
            best_x(3) = guess_pool(1, 3)
            best_x(4) = guess_pool(1, 4)
            best_x(5) = guess_pool(1, 5)
            best_x(6) = guess_pool(1, 6)
            best_x(7) = guess_pool(1, 7)
            best_x(8) = guess_pool(1, 8)

            ! =========================================================
            ! Ultimate Command Center: Global Multi-Start Loop
            ! ===========================================================
            do attempt = 1, 3
                x(1) = guess_pool(attempt, 1)
                x(2) = guess_pool(attempt, 2)
                x(3) = guess_pool(attempt, 3)
                x(4) = guess_pool(attempt, 4)
                x(5) = guess_pool(attempt, 5)
                x(6) = guess_pool(attempt, 6)
                x(7) = guess_pool(attempt, 7)
                x(8) = guess_pool(attempt, 8)

                ! ----------------------------------------------------- 
                ! Local Tactical Engine: Newton-Raphson Loop
                ! -----------------------------------------------------
                do iter = 1, max_iter
                    ! Evaluate current residual F(x)
                    call bbc2005_evaluate(x, y0, k_val, y_exp, F) 

                    err_norm = 0.0
                    do i = 1, 8
                        err_norm = err_norm + abs(F(i))
                    end do
                    
                    ! Check convergence (perfectly reach absolute tolerance)
                    if (err_norm < eps) then
                        exit
                    end if

                    ! Call globally smooth analytical Jacobian
                    call bbc2005_jacobian(x, k_val, y_exp, Jac)
                    
                    ! [Unlock 2] Dynamic Tikhonov regularization, allow damping down to 1E-12
                    damp_factor = max(1.0E-12, min(1.0E-3, err_norm * 1.0E-4))
                    do i = 1, 8
                        Jac(i, i) = Jac(i, i) + damp_factor 
                    end do
                    
                    ! Solve linear system Jac * dx = F
                    call bbc2005_solve_linear_8x8(Jac, F, dx, info)
                    if (info /= 0) then
                        call ancmsg(msgid=3161, &
                        msgtype=msgerror,       &
                        anmode=aninfo_blind_2,  &
                        i1=mat_id,              &
                        c1=titr)
                        exit
                    end if

                    ! Calculate maximum parameter update step size
                    max_dx = 0.0
                    do i = 1, 8
                        if (abs(dx(i)) > max_dx) max_dx = abs(dx(i))
                    end do

                    ! [Unlock 3] Step Tolerance (relaxed to 1E-8, allow deeper exploration)
                    if (max_dx < 1.0E-8) then
                        exit
                    end if
                    
                    ! Direction-preserving proportional scaling
                    scale_factor = 1.0
                    if (max_dx > 0.5) then
                        scale_factor = 0.5 / max_dx
                    end if

                    ! Strict backtracking line search
                    alpha = 1.0
                    step_accepted = .false.
                    
                    do ls_iter = 1, max_ls_iter
                        do i = 1, 8
                            x_new(i) = x(i) - alpha * (scale_factor * dx(i))
                        end do
                        
                        x_new(1) = max(x_new(1), 1.0E-4)
                        x_new(2) = max(x_new(2), 1.0E-4)

                        call bbc2005_evaluate(x_new, y0, k_val, y_exp, F_new)
                        
                        err_norm_new = 0.0
                        do i = 1, 8
                            err_norm_new = err_norm_new + abs(F_new(i))
                        end do

                        if (err_norm_new < err_norm) then
                            x = x_new
                            step_accepted = .true.
                            exit  
                        else
                            alpha = alpha * 0.5
                        end if
                    end do
                    
                    ! If line search fails, it means reaching the numerical limit pit bottom, stop
                    if (.not. step_accepted) then
                        exit
                    end if
                end do
                ! -----------------------------------------------------
                ! Local Tactical Engine Ends

                ! Record the valley bottom solution found by current initial guess point
                if (err_norm < best_err) then
                    best_err = err_norm
                    best_x = x
                end if

                ! Early victory judgment: if an initial guess point has directly found a perfect solution, stop remaining attempts
                if (best_err < eps) then
                    exit
                end if
            end do
            ! =========================================================

            ! 4. Output the global best solution
            
            a       = best_x(1)
            b       = best_x(2)
            l_coeff = best_x(3)
            m_coeff = best_x(4)
            n_coeff = best_x(5)
            p       = best_x(6)
            q       = best_x(7)
            r_coeff = best_x(8)
            

        end subroutine bbc2005_calcul_coeffs
      end module bbc2005_calcul_coeffs_mod