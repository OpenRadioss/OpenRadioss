!||====================================================================
!||    sts_gp_warm_start_xi   ../engine/source/interfaces/ists/ists_tangentvel.F90
!||--- called by ------------------------------------------------------
!||    STS_CONTACT_EVAL_PAIR   ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||====================================================================
!-----------------------------------------------
!   Warm-start xi for segment projection
!-----------------------------------------------
      subroutine sts_gp_warm_start_xi(gp_index, xi1_guess, xi2_guess, &
     &     have_guess)

      use sts_gp_state_mod
      implicit none

      integer, intent(in)  :: gp_index
      real*8,  intent(out) :: xi1_guess, xi2_guess
      logical, intent(out) :: have_guess

      have_guess = .false.
      xi1_guess  = 0.d0
      xi2_guess  = 0.d0

      if (gp_index .LE. 0 .OR. gp_index .GT. MAX_GLOBAL_GP) return
      if (.NOT. GP_INITIALIZED(gp_index)) return

      xi1_guess = GP_XI1_GLOBAL(gp_index) &
     &          - 2.0d0*GP_XI1_PERIOD(gp_index)
      xi2_guess = GP_XI2_GLOBAL(gp_index) &
     &          - 2.0d0*GP_XI2_PERIOD(gp_index)
      have_guess = .true.

      return
      end

!||====================================================================
!||    sts_gp_update_xi_history ../engine/source/interfaces/ists/ists_tangentvel.F90
!||--- called by ------------------------------------------------------
!||    STS_CONTACT_EVAL_PAIR   ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||--- calls ---------------------------------------------------------
!||    sts_handle_element_transition  ../engine/source/interfaces/ists/ists_elemTrans.F90
!||====================================================================
!-----------------------------------------------
!   Update global xi history (warm-start / border crossing)
!-----------------------------------------------
      subroutine sts_gp_update_xi_history(xi1, xi2, gp_index, dxi1, dxi2)

      use sts_gp_state_mod
      implicit none

      real*8,  intent(in)  :: xi1, xi2
      integer, intent(in)  :: gp_index
      real*8,  intent(out) :: dxi1, dxi2

      real*8  xi1_prev_local, xi2_prev_local
      real*8, parameter :: tol = 1.0d-6
      real*8, parameter :: dxi_reinit = 0.2d0

      dxi1 = 0.d0
      dxi2 = 0.d0

      if (gp_index .LE. 0 .OR. gp_index .GT. MAX_GLOBAL_GP) return

      if (.NOT. GP_INITIALIZED(gp_index)) then
        GP_XI1_GLOBAL(gp_index)      = xi1
        GP_XI2_GLOBAL(gp_index)      = xi2
        GP_XI1_GLOBAL_PREV(gp_index) = xi1
        GP_XI2_GLOBAL_PREV(gp_index) = xi2
        GP_XI1_PERIOD(gp_index)      = 0
        GP_XI2_PERIOD(gp_index)      = 0
        GP_INITIALIZED(gp_index)     = .TRUE.
      else
        xi1_prev_local = GP_XI1_GLOBAL(gp_index) &
     &                  - 2.0d0*GP_XI1_PERIOD(gp_index)
        xi2_prev_local = GP_XI2_GLOBAL(gp_index) &
     &                  - 2.0d0*GP_XI2_PERIOD(gp_index)

        call sts_handle_element_transition(xi1_prev_local, xi1, &
     &       xi2_prev_local, xi2, &
     &       GP_XI1_PERIOD(gp_index), GP_XI2_PERIOD(gp_index), tol)

        GP_XI1_GLOBAL_PREV(gp_index) = GP_XI1_GLOBAL(gp_index)
        GP_XI2_GLOBAL_PREV(gp_index) = GP_XI2_GLOBAL(gp_index)

        GP_XI1_GLOBAL(gp_index) = xi1 + 2.0d0*GP_XI1_PERIOD(gp_index)
        GP_XI2_GLOBAL(gp_index) = xi2 + 2.0d0*GP_XI2_PERIOD(gp_index)

        dxi1 = GP_XI1_GLOBAL(gp_index) - GP_XI1_GLOBAL_PREV(gp_index)
        dxi2 = GP_XI2_GLOBAL(gp_index) - GP_XI2_GLOBAL_PREV(gp_index)

        if (ABS(dxi1) > dxi_reinit .OR. ABS(dxi2) > dxi_reinit) then
          GP_XI1_GLOBAL(gp_index)      = xi1
          GP_XI2_GLOBAL(gp_index)      = xi2
          GP_XI1_GLOBAL_PREV(gp_index) = xi1
          GP_XI2_GLOBAL_PREV(gp_index) = xi2
          GP_XI1_PERIOD(gp_index)      = 0
          GP_XI2_PERIOD(gp_index)      = 0
          GP_TTRIAL1_HIST(gp_index)    = 0.d0
          GP_TTRIAL2_HIST(gp_index)    = 0.d0
          dxi1 = 0.d0
          dxi2 = 0.d0
        endif
      endif

      return
      end

!||====================================================================
!||    sts_gp_covariant_slip  ../engine/source/interfaces/ists/ists_tangentvel.F90
!||--- called by ------------------------------------------------------
!||    STS_CONTACT_EVAL_PAIR   ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||====================================================================
!-----------------------------------------------
!   Covariant slip increment from tangential velocity.
!   (slip1, slip2) = M*dxi for physical slip v_tang*dt.
!-----------------------------------------------
      subroutine sts_gp_covariant_slip(v_tang, rhoxi1, rhoxi2, dt, &
     &     slip1, slip2)

      implicit none

      real*8, intent(in)  :: v_tang(3), rhoxi1(3), rhoxi2(3), dt
      real*8, intent(out) :: slip1, slip2

      real*8 :: s(3)

      s(1) = v_tang(1) * dt
      s(2) = v_tang(2) * dt
      s(3) = v_tang(3) * dt
      slip1 = s(1)*rhoxi1(1) + s(2)*rhoxi1(2) + s(3)*rhoxi1(3)
      slip2 = s(1)*rhoxi2(1) + s(2)*rhoxi2(2) + s(3)*rhoxi2(3)

      return
      end

!||====================================================================
!||    sts_gp_tangential_velocity  ../engine/source/interfaces/ists/ists_tangentvel.F90
!||--- called by ------------------------------------------------------
!||    STS_CONTACT_EVAL_PAIR   ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||====================================================================
!-----------------------------------------------
!   Relative tangential velocity at a contact GP
!   (secondary minus primary, projected onto tangent plane).
!-----------------------------------------------
      subroutine sts_gp_tangential_velocity(N_xi, N_eta, node_ids, V, &
     &     norm_contact, v_tang)

#include      "my_real.inc"
      implicit none

      real*8  N_xi(3,4), N_eta(3,4)
      integer node_ids(8)
      my_real V(3,*)
      real*8  norm_contact(3)
      real*8  v_tang(3)

      integer j
      real*8  v_prim(3), v_sec(3), v_rel(3), v_n

      v_prim = 0.d0
      v_sec  = 0.d0
      DO j = 1, 4
        v_prim(1) = v_prim(1) + N_xi(1,j)  * DBLE(V(1, node_ids(j)))
        v_prim(2) = v_prim(2) + N_xi(1,j)  * DBLE(V(2, node_ids(j)))
        v_prim(3) = v_prim(3) + N_xi(1,j)  * DBLE(V(3, node_ids(j)))
        v_sec(1)  = v_sec(1)  + N_eta(1,j) * DBLE(V(1, node_ids(j+4)))
        v_sec(2)  = v_sec(2)  + N_eta(1,j) * DBLE(V(2, node_ids(j+4)))
        v_sec(3)  = v_sec(3)  + N_eta(1,j) * DBLE(V(3, node_ids(j+4)))
      ENDDO

      v_rel(1) = v_sec(1) - v_prim(1)
      v_rel(2) = v_sec(2) - v_prim(2)
      v_rel(3) = v_sec(3) - v_prim(3)

      v_n = v_rel(1)*norm_contact(1) + v_rel(2)*norm_contact(2) &
     &    + v_rel(3)*norm_contact(3)

      v_tang(1) = v_rel(1) - v_n*norm_contact(1)
      v_tang(2) = v_rel(2) - v_n*norm_contact(2)
      v_tang(3) = v_rel(3) - v_n*norm_contact(3)

      return
      end
