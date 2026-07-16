!||====================================================================
!||    STS_CONTACT_EVAL_PAIR   ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||--- called by ------------------------------------------------------
!||    STS_CONTACTS_ASSEMBLE   ../engine/source/interfaces/ists/ists_contacts_assemble.F90
!||--- calls ---------------------------------------------------------
!||    sts_gausspt             ../engine/source/interfaces/ists/ists_sts_gausspt.F90
!||    sts_lobattopt           ../engine/source/interfaces/ists/ists_sts_lobattopt.F90
!||    sts_project             ../engine/source/interfaces/ists/ists_sts_project.F90
!||    sts_pos                 ../engine/source/interfaces/ists/ists_pos.F90
!||    sts_surfgeom            ../engine/source/interfaces/ists/ists_sts_surfgeom.F90
!||    sts_penetr              ../engine/source/interfaces/ists/ists_sts_penetr.F90
!||    sts_gp_update_xi_history ../engine/source/interfaces/ists/ists_tangentvel.F90
!||    sts_gp_warm_start_xi    ../engine/source/interfaces/ists/ists_tangentvel.F90
!||    sts_gp_tangential_velocity ../engine/source/interfaces/ists/ists_tangentvel.F90
!||    sts_gp_covariant_slip      ../engine/source/interfaces/ists/ists_tangentvel.F90
!||    sts_gp_update_dt2t       ../engine/source/interfaces/ists/ists_contact_dt_mod.F90
!||    sts_gp_ivis2_normal      ../engine/source/interfaces/ists/ists_contact_visc_mod.F90
!||    sts_shape               ../engine/source/interfaces/ists/ists_shape_fct.F90
!||====================================================================
!
!   Evaluate one STS segment pair for Gauss or Lobatto quadrature,
!   including projection, normal penalty, optional friction, and energy.
!
      subroutine STS_CONTACT_EVAL_PAIR(XUPD, STIF, p, IMPACT, EL_NR, node_stiff, OPTION, &
      &                   FRICC, XMU, IFPEN, &
      &                   p_friction, node_ids, V, numnod, &
      &                   CALC_FRICTION, MAX_STS_SIZE, GAP, GP_WEIGHT, &
      &                   PAIR_MAX_PENETRATION, &
      &                   ECONTT_PAIR, ECONVT_PAIR, MS, NOINT, VISC, IVIS2, &
      &                   VISCFFRIC, DT2T, NELTST, ITYPTST, &
      &                   COMMIT_CONTACT, PROBE_SCORE, VALID_GP, MIN_PENE, &
      &                   DT1, DTFAC1_10)
!-----------------------------------------------
!   M o d u l e s   /   I m p l i c i t   T y p e s
!-----------------------------------------------
      use constant_mod
      use sts_gp_state_mod
      use ists_contact_dt_mod
      use ists_contact_visc_mod
      use precision_mod, only : WP
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
#include      "mvsiz_p.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
!     XUPD     : Coordinates of contact element (3,8)
!                Nodes 1-4 are Primary (master) nodes
!                Nodes 5-8 are Secondary (slave) nodes
!     STIF     : Pair contact stiffness parameter (normal)
!     p        : Output contact forces (24 components) - normal + friction combined
!     IMPACT   : Output flag indicating if penetration was detected
!     EL_NR    : Pair index in the STS candidate arrays
!     node_stiff: Output nodal stiffness values (8 components)
!     OPTION   : 0 = Gauss quadrature, 1 = Lobatto quadrature.
!     FRICC, XMU: Friction parameters
!     IFPEN    : Penetration flag array
!     p_friction: Output friction forces (24 components) - separate output
!     node_ids : Node IDs for velocity interpolation (8 components)
!     CALC_FRICTION: Flag to enable/disable friction calculation
!     MAX_STS_SIZE: Maximum size for history arrays
!     GAP   : Gap used in penetration test.
!     PAIR_MAX_PENETRATION : Max |penetr - gap| over activated Gauss points.
!-----------------------------------------------
      INTEGER, INTENT(INOUT) :: IMPACT
      INTEGER, INTENT(IN)    :: OPTION, EL_NR
      INTEGER, INTENT(IN)    :: node_ids(8)
      INTEGER, INTENT(IN)    :: numnod
      real(kind=WP), INTENT(IN)    :: V(3,numnod)
      LOGICAL, INTENT(IN)    :: CALC_FRICTION
      real(kind=WP), INTENT(IN)    :: STIF
      real*8, INTENT(INOUT) :: p(24), p_friction(24)
      real*8, INTENT(IN)    :: XUPD(3,8)
      real*8, INTENT(INOUT) :: node_stiff(8)
      real(kind=WP), INTENT(IN)    :: FRICC(MVSIZ)
      real(kind=WP), INTENT(INOUT) :: XMU(MVSIZ)
      INTEGER, INTENT(IN)    :: MAX_STS_SIZE
      INTEGER, INTENT(INOUT) :: IFPEN(MAX_STS_SIZE)
      real(kind=WP), INTENT(IN)    :: GAP  ! Gap value from user input
      REAL*8, INTENT(IN)  :: GP_WEIGHT(4)
      REAL*8, INTENT(INOUT) :: PAIR_MAX_PENETRATION
      REAL*8, INTENT(INOUT) :: ECONTT_PAIR, ECONVT_PAIR
      real(kind=WP), INTENT(IN)    :: MS(numnod)
      INTEGER, INTENT(IN)    :: NOINT, IVIS2
      real(kind=WP), INTENT(IN)    :: VISC, VISCFFRIC
      real(kind=WP), INTENT(INOUT) :: DT2T
      INTEGER, INTENT(INOUT) :: NELTST, ITYPTST
      LOGICAL, INTENT(IN)    :: COMMIT_CONTACT
      REAL*8, INTENT(INOUT)    :: PROBE_SCORE
      INTEGER, INTENT(INOUT)   :: VALID_GP
      REAL*8, INTENT(INOUT)    :: MIN_PENE
      real(kind=WP), INTENT(IN)    :: DT1
      real(kind=WP), INTENT(IN)    :: DTFAC1_10
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER i, j, z, q, ip
      INTEGER pair_fric_idx
      real*8  xi1, xi2
      real*8  penetr, PENE, GAPV, FAC
      real*8  d1, d1_fric, d1_stif
      real*8  a(3,24), daxi1(3,24), daxi2(3,24)
      real*8  f_normal, f_visc, v_n
      real*8  daeta1(3,24), daeta2(3,24)
      real*8  rhoxi1(3), rhoxi2(3)
      real*8  m_ij(2,2), detm, mij(2,2), detmPrimary
      real*8  norm_contact(3), norm_fric(3)
      real*8  pm(24), pm_friction(24)
      real*8  eta1(10), eta2(10), wi1(10), wi2(10)
      real*8  energy
      real*8  area_weight
      real*8  gap_distance
      real*8  raw_gap_distance
      real*8  clear_ratio
      real*8  N_xi(3,4), N_eta(3,4)
      real*8  FXT, FYT, FZT, PHI, FN
      real*8  v_tang(3)
      INTEGER INDEX_CAND
      INTEGER gp_index
      INTEGER mst_key(4), sec_key(4)
      INTEGER sec_corner_idx
      REAL*8 gp_area_scale

!     Deep-penetration guards (1/value)
      real*8, PARAMETER :: PREC_CLEAR_GAUSS = 5.0d-5
      real*8, PARAMETER :: PREC_CLEAR_LOBATTO = 5.0d-5

      real*8  dxi1, dxi2  ! Projection-history increments
      real*8  slip1, slip2
      real*8  T_trial(2), T_real(2), T_trialabs
      real*8  xi1_guess, xi2_guess
      logical have_guess
      
      ! Gauss quadrature for friction calculation
      real*8  eta1_gauss(10), eta2_gauss(10), wi1_gauss(10), wi2_gauss(10)
      real*8  xi1_gauss, xi2_gauss
      real*8  m_ij_gauss(2,2), detm_gauss
      real*8  rhoxi1_gauss(3), rhoxi2_gauss(3)
      logical gauss_valid
      
      ! Friction calculation variables (selected projection)
      real*8  xi1_fric, xi2_fric
      real*8  m_ij_fric(2,2), detm_fric
      real*8  rhoxi1_fric(3), rhoxi2_fric(3)
      real*8  wi1_fric, wi2_fric, eta1_fric, eta2_fric
      logical use_gauss_for_friction
      logical calc_fric_this_pair
      real*8  fric_weight
      integer valid_gp_count
      real*8 clear_frac
!-----------------------------------------------
!   I n i t i a l i z a t i o n
!-----------------------------------------------
      IMPACT = 0
      PAIR_MAX_PENETRATION = 0.0D0
      ECONTT_PAIR = 0.0D0
      ECONVT_PAIR = 0.0D0
      PROBE_SCORE = 0.0D0
      VALID_GP = 0
      MIN_PENE = HUGE(1.0D0)
      ip = 2 ! 2x2 Gauss or Lobatto quadrature order
      pair_fric_idx = MIN(MAX(EL_NR, 1), MVSIZ)
      calc_fric_this_pair = COMMIT_CONTACT .AND. CALC_FRICTION .AND. &
     &  FRICC(pair_fric_idx) .GT. 0.0d0
      
      ! Get quadrature points and weights
      IF (OPTION == 0) THEN
        ! Gauss: one quadrature rule for normal and friction
        call sts_gausspt(ip, eta1, wi1)
        call sts_gausspt(ip, eta2, wi2)
        eta1_gauss = eta1
        eta2_gauss = eta2
        wi1_gauss = wi1
        wi2_gauss = wi2
      ELSE
        ! Lobatto for normal contact and friction.
        call sts_lobattopt(ip, eta1, wi1)
        call sts_lobattopt(ip, eta2, wi2)
      ENDIF

      ! Initialize force arrays
      DO i=1,24
        pm(i) = 0.d0
        pm_friction(i) = 0.d0
        p(i) = 0.d0
        p_friction(i) = 0.d0
      ENDDO
      GAPV = GAP ! Effective scalar gap (MIN(GAPMAX,MAX(GAPMIN,GAP))).
      energy = 0.0d0
      IF (COMMIT_CONTACT) XMU(1) = FRICC(pair_fric_idx) ! Friction coefficient mu
      node_stiff = 0.0d0
      FAC = 1.0d0
      valid_gp_count = 0
      rhoxi1_gauss = 0.0d0
      rhoxi2_gauss = 0.0d0
      xi1_gauss = 0.0d0
      xi2_gauss = 0.0d0
      m_ij_gauss = 0.0d0
      detm_gauss = 0.0d0
      eta1_gauss = 0.0d0
      eta2_gauss = 0.0d0
      wi1_gauss = 0.0d0
      wi2_gauss = 0.0d0
!-----------------------------------------------
!   M a i n   C o m p u t a t i o n
!-----------------------------------------------
!     Loop over integration points
      DO z=1,ip
        DO q=1,ip
          IF (COMMIT_CONTACT) THEN
            ! Get the unique pair key for the current Gauss/Lobatto point
            call sts_gp_canonical_pair_key(node_ids, mst_key, sec_key)
            call sts_gp_acquire_slot(mst_key, sec_key, z, q, OPTION, &
     &          gp_index)

            ! Get the warm start (border crossing) guess for the current Gauss/Lobatto point
            call sts_gp_warm_start_xi(gp_index, xi1_guess, xi2_guess, &
     &          have_guess)
          ELSE
            gp_index = 0
            xi1_guess = 0.0d0
            xi2_guess = 0.0d0
            have_guess = .FALSE.
          ENDIF
          
          ! Project Secondary surface to Primary surface at current Gauss/Lobatto point
          call sts_project(XUPD, xi1, xi2, eta1(z), eta2(q), &
     &        xi1_guess, xi2_guess, have_guess)
          
          ! Check if projection is valid
          IF ((xi1 .NE. xi1) .OR. (xi2 .NE. xi2) .OR. &
     &        (dabs(xi1) .GT. 1.05d0) .OR. &
     &        (dabs(xi2) .GT. 1.05d0)) THEN
            CYCLE
          END IF
          
          ! Build position and derivative matrices
          call sts_pos(a, daxi1, daxi2, daeta1, daeta2, xi1, xi2, &
     &               eta1(z), eta2(q))
          
          ! Calculate surface geometry and metrics
          call sts_surfgeom(XUPD, daxi1, daxi2, daeta1, daeta2, norm_contact, &
     &                    rhoxi1, rhoxi2, m_ij, detm, mij, detmPrimary)
          area_weight = wi1(z) * wi2(q) * dsqrt(detm)

          ! ==== FRICTION PROJECTION ====
          IF (.NOT. calc_fric_this_pair) THEN
            gauss_valid = .FALSE.
          ELSE IF (OPTION == 0) THEN
            ! Gauss quadrature: reuse current projection and geometry
            xi1_gauss = xi1
            xi2_gauss = xi2
            gauss_valid = .TRUE.
            rhoxi1_gauss = rhoxi1
            rhoxi2_gauss = rhoxi2
            m_ij_gauss(1,1) = m_ij(1,1)
            m_ij_gauss(1,2) = m_ij(1,2)
            m_ij_gauss(2,1) = m_ij(2,1)
            m_ij_gauss(2,2) = m_ij(2,2)
            detm_gauss = detm
          ELSE
            ! Lobatto quadrature: use the already projected Lobatto point
            ! for friction as well, so friction history remains attached to
            ! the same physical integration point.
            gauss_valid = .FALSE.
          ENDIF

          ! ==== Normal impact =====
          call sts_shape(eta1(z), eta2(q), N_eta)
          gp_area_scale = 1.0D0
          IF (OPTION == 1 .AND. ip == 2) THEN
            sec_corner_idx = 1
            DO i = 2, 4
              IF (N_eta(1,i) > N_eta(1,sec_corner_idx)) THEN
                sec_corner_idx = i
              ENDIF
            ENDDO
            gp_area_scale = GP_WEIGHT(sec_corner_idx)
          ENDIF
          area_weight = area_weight * gp_area_scale
          IF (area_weight <= 0.0D0) CYCLE

          ! Compute signed clearance to the primary projection and orient
          ! the primary normal toward the secondary integration point.
          call sts_penetr(XUPD, penetr, norm_contact, a)
          IF (penetr .LT. 0.0d0) THEN
            penetr = -penetr
            DO i=1,3
              norm_contact(i) = -norm_contact(i)
            ENDDO
          ENDIF
          

          ! Check for penetration
          PENE = penetr - GAPV
          valid_gp_count = valid_gp_count + 1
          MIN_PENE = MIN(MIN_PENE, PENE)
          ! No penetration - skip to next Gauss point
          IF (PENE .GT. 0.d0) THEN
            ! Reset the slot for the current Gauss/Lobatto point
            IF (COMMIT_CONTACT .AND. gp_index .GT. 0) &
     &        CALL sts_gp_reset_slot(gp_index)
            ! Skip to next Gauss point
            CYCLE
          ENDIF
          
          penetr = PENE
          ! Calculate penalty parameter.
          ! Clamp the physical clearance so FAC stays bounded for deep
          ! penetration. Without this, FAC=GAP/clearance becomes singular
          ! as a Lobatto point approaches zero clearance.
          clear_frac = PREC_CLEAR_GAUSS
          IF (OPTION == 1) clear_frac = PREC_CLEAR_LOBATTO
          IF (DABS(GAPV) .GT. EM10) THEN
            raw_gap_distance = GAPV + PENE
            gap_distance = MAX(raw_gap_distance, &
     &        DABS(GAPV) * clear_frac, EM10)
            FAC = DABS(GAPV) / gap_distance
          ELSE
            raw_gap_distance = 0.0d0
            FAC = 1.0d0
          ENDIF
          d1_fric = 0.5d0 * STIF ! Tangential stiffness for friction calculation
          d1 = 0.5d0 * STIF * FAC ! Normal stiffness for normal contact calculation
          IF ((d1 .NE. d1) .OR. DABS(d1) .GT. HUGE(1.0d0) .OR. &
     &        d1 .LE. 0.0d0) CYCLE

          ! Penetration detected with a valid integrated force contribution.
          IMPACT = 1
          PAIR_MAX_PENETRATION = MAX(PAIR_MAX_PENETRATION, DABS(PENE))
          PROBE_SCORE = PROBE_SCORE + DABS(d1 * penetr) * area_weight
          IF (.NOT. COMMIT_CONTACT) CYCLE

!         Shape functions for stiffness integration and IVIS2 mass.
          call sts_shape(xi1, xi2, N_xi)

          CALL sts_gp_normal_velocity(N_xi, N_eta, node_ids, V, numnod, &
     &        norm_contact, v_n)

          d1_stif = d1
          f_visc = 0.d0
          CALL sts_gp_ivis2_normal(d1, GAPV, PENE, v_n, N_eta, &
     &        node_ids, MS, numnod, VISC, IVIS2, VISCFFRIC, DT1, &
     &        d1_stif, f_visc)

          f_normal = d1 * penetr

!         Accumulate nodal stiffness with the same shape-function
!         area integration used by NTS/Q1NP (post-IVIS2 stiffness).
          DO j = 1, 4
            node_stiff(j) = node_stiff(j) &
     &        + d1_stif * DABS(N_xi(1,j)) * area_weight
            node_stiff(j+4) = node_stiff(j+4) &
     &        + d1_stif * DABS(N_eta(1,j)) * area_weight
          ENDDO

          CALL sts_gp_update_dt2t(node_ids, MS, numnod, d1_stif, N_xi, N_eta, &
     &        area_weight, GAPV, PENE, V, norm_contact, NOINT, &
     &        DT2T, NELTST, ITYPTST, DTFAC1_10)

          INDEX_CAND = EL_NR
          IF (INDEX_CAND >= 1 .AND. INDEX_CAND <= MAX_STS_SIZE) THEN
            IFPEN(INDEX_CAND) = 1
          ENDIF
          
          ! Accumulate the same penalty potential used by legacy TYPE7
          ! for FAC = gap / current_clearance.
          IF (DABS(GAPV) .GT. EM10) THEN
            clear_ratio = MAX(TINY(clear_ratio), gap_distance / DABS(GAPV))
            energy = energy + 0.5d0 * STIF * DABS(GAPV)**2 * (clear_ratio - 1.0d0 - DLOG(clear_ratio)) * area_weight
            IF (raw_gap_distance .LT. gap_distance) THEN
              energy = energy + 0.25d0 * STIF * DABS(GAPV) / &
     &          gap_distance * (PENE**2 - &
     &          (gap_distance - DABS(GAPV))**2) * area_weight
            ENDIF
          ELSE
            energy = energy + 0.5d0 * d1 * penetr**2 * area_weight
          ENDIF

          ! Compute residual forces (normal + viscous component)
          DO i=1,24
            DO j=1,3
              pm(i) = pm(i) + (f_normal + f_visc) * &
     &                a(j,i) * norm_contact(j) * area_weight
            ENDDO
          ENDDO
          ! ===== FRICTION CALCULATION =====
          IF (calc_fric_this_pair) THEN
            
            if (gp_index .LE. 0) then
              CYCLE
            endif

            ! Determine which projection to use for friction.
            IF (gauss_valid) THEN
              ! ==== USE GAUSS PROJECTION (preferred) ====
              use_gauss_for_friction = .TRUE.
              xi1_fric = xi1_gauss
              xi2_fric = xi2_gauss
              m_ij_fric(1,1) = m_ij_gauss(1,1)
              m_ij_fric(1,2) = m_ij_gauss(1,2)
              m_ij_fric(2,1) = m_ij_gauss(2,1)
              m_ij_fric(2,2) = m_ij_gauss(2,2)
              detm_fric = detm_gauss
              rhoxi1_fric(1) = rhoxi1_gauss(1)
              rhoxi1_fric(2) = rhoxi1_gauss(2)
              rhoxi1_fric(3) = rhoxi1_gauss(3)
              rhoxi2_fric(1) = rhoxi2_gauss(1)
              rhoxi2_fric(2) = rhoxi2_gauss(2)
              rhoxi2_fric(3) = rhoxi2_gauss(3)
              wi1_fric = wi1_gauss(z)
              wi2_fric = wi2_gauss(q)
              eta1_fric = eta1_gauss(z)
              eta2_fric = eta2_gauss(q)
            ELSE IF (dabs(xi1) .LE. 1.05d0 .AND. dabs(xi2) .LE. 1.05d0) THEN
              ! ==== USE CURRENT PROJECTION ====
              use_gauss_for_friction = .FALSE.
              xi1_fric = xi1
              xi2_fric = xi2
              m_ij_fric(1,1) = m_ij(1,1)
              m_ij_fric(1,2) = m_ij(1,2)
              m_ij_fric(2,1) = m_ij(2,1)
              m_ij_fric(2,2) = m_ij(2,2)
              detm_fric = detm
              rhoxi1_fric(1) = rhoxi1(1)
              rhoxi1_fric(2) = rhoxi1(2)
              rhoxi1_fric(3) = rhoxi1(3)
              rhoxi2_fric(1) = rhoxi2(1)
              rhoxi2_fric(2) = rhoxi2(2)
              rhoxi2_fric(3) = rhoxi2(3)
              wi1_fric = wi1(z)
              wi2_fric = wi2(q)
              eta1_fric = eta1(z)
              eta2_fric = eta2(q)
            ELSE
              ! Both projections invalid - skip friction for this GP
              CYCLE
            ENDIF

            ! Shape functions at friction projection point
            IF (use_gauss_for_friction .AND. OPTION == 0) THEN
              ! Reuse shape functions already evaluated for normal forces
            ELSE
              call sts_shape(xi1_fric, xi2_fric, N_xi)
              call sts_shape(eta1_fric, eta2_fric, N_eta)
            ENDIF

            ! Friction uses the normal from the selected integration point.
            norm_fric = norm_contact

            ! Calculate the tangential velocity at the current Gauss/Lobatto point
            call sts_gp_tangential_velocity(N_xi, N_eta, node_ids, V, numnod, &
     &          norm_fric, v_tang)

            ! Calculate the covariant slip at the current Gauss/Lobatto point
            call sts_gp_covariant_slip(v_tang, rhoxi1_fric, rhoxi2_fric, DT1, slip1, slip2)

            ! Tangential trial stiffness: STIF0 = 0.5*STIF (no FAC), as in NTS IFQ>=10
            T_trial(1) = GP_TTRIAL1_HIST(gp_index) - d1_fric * slip1
            T_trial(2) = GP_TTRIAL2_HIST(gp_index) - d1_fric * slip2

            ! Magnitude of trial tangential traction using selected metric
            T_trialabs = 0.0d0
            DO i = 1, 2
              DO j = 1, 2
                T_trialabs = T_trialabs + T_trial(i) * T_trial(j) * &
     &                       m_ij_fric(i, j)
              END DO
            END DO
            T_trialabs = DSQRT(MAX(EM30, T_trialabs))

            ! ===== FRICTION YIELD FUNCTION =====
            FN = d1 * DABS(penetr)  ! Normal force at the Gauss point from penalty method
            PHI = T_trialabs - XMU(1) * FN

            ! ===== Sticking/Sliding logic =====
            IF (PHI .LE. 0.0d0) THEN
              ! ===== STICKING =====
              T_real(1) = T_trial(1)
              T_real(2) = T_trial(2)
              GP_IS_STICKING(gp_index) = .TRUE.
            ELSE
              ! ===== SLIDING =====
              T_real(1) = XMU(1) * FN * T_trial(1) / MAX(1.d-30, T_trialabs)
              T_real(2) = XMU(1) * FN * T_trial(2) / MAX(1.d-30, T_trialabs)
              GP_IS_STICKING(gp_index) = .FALSE.
            ENDIF

            ! Update global history of tangential traction with final value
            GP_TTRIAL1_HIST(gp_index) = T_real(1)
            GP_TTRIAL2_HIST(gp_index) = T_real(2)

            ! Update xi history for warm-start (border crossing) projection next timestep
            call sts_gp_update_xi_history(xi1_fric, xi2_fric, gp_index, &
     &          dxi1, dxi2)

            ! ===== CONVERT FROM 2D TANGENT PLANE TO 3D =====
            ! Convert using selected tangent vectors
            FXT = T_real(1)*rhoxi1_fric(1) + T_real(2)*rhoxi2_fric(1)
            FYT = T_real(1)*rhoxi1_fric(2) + T_real(2)*rhoxi2_fric(2)
            FZT = T_real(1)*rhoxi1_fric(3) + T_real(2)*rhoxi2_fric(3)

            ! ===== ACCUMULATE FRICTION FORCES TO NODES =====
            fric_weight = wi1_fric * wi2_fric * dsqrt(detm_fric) * &
     &        gp_area_scale
            ! Primary nodes (1-4): subtract friction
            DO j=1,4
              pm_friction((j-1)*3+1) = pm_friction((j-1)*3+1) - &
     &                               N_xi(1,j) * FXT * fric_weight
              pm_friction((j-1)*3+2) = pm_friction((j-1)*3+2) - &
     &                               N_xi(1,j) * FYT * fric_weight
              pm_friction((j-1)*3+3) = pm_friction((j-1)*3+3) - &
     &                               N_xi(1,j) * FZT * fric_weight
            ENDDO
            
            ! Secondary nodes (5-8): add friction
            DO j=1,4
              pm_friction(12+(j-1)*3+1) = pm_friction(12+(j-1)*3+1) + &
     &                                  N_eta(1,j) * FXT * fric_weight
              pm_friction(12+(j-1)*3+2) = pm_friction(12+(j-1)*3+2) + &
     &                                  N_eta(1,j) * FYT * fric_weight
              pm_friction(12+(j-1)*3+3) = pm_friction(12+(j-1)*3+3) + &
     &                                  N_eta(1,j) * FZT * fric_weight
            ENDDO

            ! mirrors NTS EFRIC_L, integrated at GP
            ECONVT_PAIR = ECONVT_PAIR - DT1 * (v_tang(1)*FXT + v_tang(2)*FYT + v_tang(3)*FZT) * fric_weight
          ENDIF
          ! ===== END FRICTION CALCULATION =====
        ENDDO
      ENDDO
         
!-----------------------------------------------
!   F i n a l   R e s u l t s
!-----------------------------------------------
      ! Set output forces
      ! Combine normal and friction forces
      DO i=1,24
        p(i) = -pm(i) + pm_friction(i)
        p_friction(i) = pm_friction(i)
      ENDDO

      ECONTT_PAIR = energy

      VALID_GP = valid_gp_count
      IF (valid_gp_count <= 0) MIN_PENE = 0.0D0

      RETURN
      END
