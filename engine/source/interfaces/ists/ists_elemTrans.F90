!||====================================================================
!||    sts_handle_element_transition  ../engine/source/interfaces/ists/ists_elemTrans.F90
!||--- called by ------------------------------------------------------
!||    sts_gp_update_xi_history   ../engine/source/interfaces/ists/ists_tangentvel.F90
!||====================================================================
!-----------------------------------------------
!   Handle element transition for global xi tracking
!-----------------------------------------------
      subroutine sts_handle_element_transition(xi1_local_prev, xi1_local_new, &
     &     xi2_local_prev, xi2_local_new, &
     &     gp_xi1_period, gp_xi2_period, tol)

      implicit none

!-- Input arguments
      real*8, intent(in)    :: xi1_local_prev, xi1_local_new
      real*8, intent(in)    :: xi2_local_prev, xi2_local_new
      real*8, intent(in)    :: tol

!-- In/out arguments
      integer, intent(inout) :: gp_xi1_period
      integer, intent(inout) :: gp_xi2_period

      real*8, parameter :: edge_eps = 0.5d0
      logical xi1_crossed, xi2_crossed

!-----------------------------------------------
!  Detect crossings of +/-1 in xi1 and xi2 and
!  update period counters accordingly.
!-----------------------------------------------

      xi1_crossed = .false.
      xi2_crossed = .false.

!-- Check transition in xi1 direction (exact +/-1 boundary)
      if (dabs(xi1_local_prev - 1.0d0) < tol) then
!       Was at +1 boundary, likely crossed to next element
        if (xi1_local_new < 0.0d0) then
!         Crossed from +1 to -1 (forward)
          gp_xi1_period = gp_xi1_period + 1
          xi1_crossed = .true.
        endif
      else if (dabs(xi1_local_prev + 1.0d0) < tol) then
!       Was at -1 boundary, likely crossed to previous element
        if (xi1_local_new > 0.0d0) then
!         Crossed from -1 to +1 (backward)
          gp_xi1_period = gp_xi1_period - 1
          xi1_crossed = .true.
        endif
      endif

!-- Fallback edge crossing for xi1 (sign flip away from boundary)
      if (.not. xi1_crossed) then
        if (xi1_local_prev > edge_eps .and. xi1_local_new < -edge_eps) then
          gp_xi1_period = gp_xi1_period + 1
        else if (xi1_local_prev < -edge_eps .and. &
     &             xi1_local_new > edge_eps) then
          gp_xi1_period = gp_xi1_period - 1
        endif
      endif

!-- Check transition in xi2 direction (exact +/-1 boundary)
      if (dabs(xi2_local_prev - 1.0d0) < tol) then
        if (xi2_local_new < 0.0d0) then
          gp_xi2_period = gp_xi2_period + 1
          xi2_crossed = .true.
        endif
      else if (dabs(xi2_local_prev + 1.0d0) < tol) then
        if (xi2_local_new > 0.0d0) then
          gp_xi2_period = gp_xi2_period - 1
          xi2_crossed = .true.
        endif
      endif

!-- Fallback edge crossing for xi2 (sign flip away from boundary)
      if (.not. xi2_crossed) then
        if (xi2_local_prev > edge_eps .and. xi2_local_new < -edge_eps) then
          gp_xi2_period = gp_xi2_period + 1
        else if (xi2_local_prev < -edge_eps .and. &
     &             xi2_local_new > edge_eps) then
          gp_xi2_period = gp_xi2_period - 1
        endif
      endif

      return
      end
