!||====================================================================
!||    q1np_contact_driver               ../engine/source/interfaces/ists_q1np/q1np_contact_driver.F90
!||--- called by ------------------------------------------------------
!||    i7mainf                           ../engine/source/interfaces/int07/i7mainf.F
!||--- calls      -----------------------------------------------------
!||    q1np_contact_broad_phase          ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_narrow_phase         ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||    q1np_contact_force_assembly       ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||--- uses       -----------------------------------------------------
!||    q1np_restart_mod                  ../common_source/modules/q1np_restart_mod.F90
!||    restmod                           ../engine/share/modules/restart_mod.F
!||    q1np_contact_algorithms_mod       ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||====================================================================
      MODULE Q1NP_CONTACT_DRIVER_MOD
        USE PRECISION_MOD, ONLY : WP
        USE CONSTANT_MOD, ONLY : ONE
        USE Q1NP_RESTART_MOD
        USE Q1NP_CONTACT_EXPORT_MOD, ONLY : &
     &    Q1NP_CONTACT_EXPORT_BEGIN_CYCLE
        USE Q1NP_CONTACT_ALGORITHMS_MOD, ONLY : &
     &    Q1NP_CONTACT_BROAD_PHASE, &
     &    Q1NP_CONTACT_NARROW_PHASE, &
     &    Q1NP_CONTACT_FORCE_ASSEMBLY, &
     &    Q1NP_CONTACT_WORKSPACE_FREE, &
     &    Q1NP_CONTACT_INIT_GRID_NODES, &
     &    Q1NP_CONTACT_PAIR, &
     &    Q1NP_CONTACT_WORKSPACE
        IMPLICIT NONE
        PRIVATE

        INTEGER, SAVE :: Q1NP_CONTACT_INT7_LAST_NCYCLE = -1
        LOGICAL, SAVE :: Q1NP_CONTACT_INT7_ALREADY = .FALSE.

!       Adaptive skip settings
        LOGICAL, PARAMETER :: SKIP_ENABLED  = .TRUE.
        REAL(KIND=WP), PARAMETER :: SKIP_SCALE    = 8.0_WP
        REAL(KIND=WP), PARAMETER :: SKIP_EXPONENT = 1.5_WP
        INTEGER, PARAMETER :: SKIP_MAX      = 200
        REAL(KIND=WP), PARAMETER :: GAP_FALLBACK  = 1.0E-6_WP
        INTEGER, SAVE :: SKIP_REMAINING = 0

        PUBLIC :: Q1NP_CONTACT_DRIVER_INT7
        PUBLIC :: Q1NP_CONTACT_INIT_GRID_NODES

      CONTAINS

!=======================================================================
!   Q1NP_CONTACT_DRIVER_INT7
!   INT7 entry: runs broad phase, narrow phase, and force assembly,
!   at most once per NCYCLE.
!=======================================================================
        SUBROUTINE Q1NP_CONTACT_DRIVER_INT7(NCYCLE, NUMNOD, X, A, &
     &      STIFN, GAP, IGSTI, KMIN, KMAX, IRECTM, NSV, STFNS, NSN, &
     &      STFM, NRTM, FCONT, DO_FCONT, IMPACT_glob)
          INTEGER, INTENT(IN) :: NCYCLE, NUMNOD, IGSTI, NSN, NRTM
          INTEGER, INTENT(INOUT) :: IMPACT_glob
          INTEGER, INTENT(IN) :: IRECTM(:)
          INTEGER, INTENT(IN) :: NSV(:)
          REAL(KIND=WP), INTENT(IN) :: X(3,NUMNOD)
          REAL(KIND=WP), INTENT(IN) :: GAP, KMIN, KMAX
          REAL(KIND=WP), INTENT(IN) :: STFNS(:), STFM(:)
          REAL(KIND=WP), INTENT(INOUT) :: A(3,NUMNOD), STIFN(NUMNOD)
          REAL(KIND=WP), INTENT(INOUT) :: FCONT(3,NUMNOD)
          LOGICAL, INTENT(IN) :: DO_FCONT

          TYPE(Q1NP_CONTACT_WORKSPACE) :: WS
          TYPE(Q1NP_CONTACT_PAIR), ALLOCATABLE :: PAIRS(:)
          INTEGER :: N_PAIRS
          REAL(KIND=WP) :: D_MIN

          IF (NCYCLE /= Q1NP_CONTACT_INT7_LAST_NCYCLE) THEN
            Q1NP_CONTACT_INT7_LAST_NCYCLE = NCYCLE
            CALL Q1NP_CONTACT_EXPORT_BEGIN_CYCLE(NCYCLE, NUMELQ1NP_G)
            Q1NP_CONTACT_INT7_ALREADY = .FALSE.
          END IF
          IMPACT_glob = 0
          IF (Q1NP_CONTACT_INT7_ALREADY) RETURN

!         --- Adaptive skip ---
          IF (SKIP_ENABLED .AND. SKIP_REMAINING > 0) THEN
            SKIP_REMAINING = SKIP_REMAINING - 1
            Q1NP_CONTACT_INT7_ALREADY = .TRUE.
            RETURN
          END IF

!         --- Broad phase (point clouds + voxel search) ---
          CALL Q1NP_CONTACT_BROAD_PHASE( &
     &      KQ1NP_TAB, IQ1NP_TAB, Q1NP_KTAB, &
     &      X, NUMNOD, NUMELQ1NP_G, GAP, WS, D_MIN)
          ! WS (WORKSPACE) holds point clouds, parametric coordinates, 
          ! and voxel candidate lists built by the broad phase and 
          ! consumed by the narrow phase.

!         --- Narrow phase (NURBS projection) ---
          N_PAIRS = 0
          IF (WS%NPTS_A > 0 .AND. WS%NPTS_B > 0) THEN
            ! NURBS-to-NURBS projection on the point clouds from the 
            ! broad phase.
            CALL Q1NP_CONTACT_NARROW_PHASE( &
     &        WS, KQ1NP_TAB, IQ1NP_TAB, Q1NP_KTAB, &
     &        X, NUMNOD, GAP, PAIRS, N_PAIRS)
          END IF
          ! PAIRS (CONTACT_PAIRS) holds the penetrating contact pairs.

!         --- Force assembly (penalty forces + FCONT scatter) ---
          IF (N_PAIRS > 0) THEN
            IMPACT_glob = 1
            CALL Q1NP_CONTACT_FORCE_ASSEMBLY( &
     &        PAIRS, N_PAIRS, &
     &        KQ1NP_TAB, IQ1NP_TAB, IQ1NP_BULK_TAB, IRECTM, &
     &        Q1NP_KTAB, X, NUMNOD, GAP, A, STIFN, &
     &        IGSTI, KMIN, KMAX, NSV, STFNS, NSN, STFM, NRTM, &
     &        FCONT, DO_FCONT)
          END IF

!         --- Cleanup and adaptive skip ---
          CALL Q1NP_CONTACT_WORKSPACE_FREE(WS)
          CALL Q1NP_CONTACT_UPDATE_SKIP(D_MIN, GAP, N_PAIRS > 0)

          IF (ALLOCATED(PAIRS)) DEALLOCATE(PAIRS)
          Q1NP_CONTACT_INT7_ALREADY = .TRUE.
        END SUBROUTINE Q1NP_CONTACT_DRIVER_INT7

!=======================================================================
!   Q1NP_CONTACT_UPDATE_SKIP
!=======================================================================
        SUBROUTINE Q1NP_CONTACT_UPDATE_SKIP(D_MIN, GAP, HAS_CONTACT)
          REAL(KIND=WP), INTENT(IN) :: D_MIN, GAP
          LOGICAL, INTENT(IN) :: HAS_CONTACT

          REAL(KIND=WP) :: GAP_CONTACT, TRIGGER_TOL, DIST_RATIO

          IF (.NOT. SKIP_ENABLED) THEN
            SKIP_REMAINING = 0
            RETURN
          END IF

          IF (HAS_CONTACT) THEN
            SKIP_REMAINING = 0
            RETURN
          END IF

          GAP_CONTACT = MAX(GAP_FALLBACK, ABS(GAP))
          TRIGGER_TOL = MAX(1.0E-12_WP, &
     &      GAP_CONTACT)
          DIST_RATIO = D_MIN / TRIGGER_TOL
          IF (DIST_RATIO <= ONE) THEN
            SKIP_REMAINING = 0
          ELSE
            SKIP_REMAINING = MIN(SKIP_MAX, &
     &        INT(SKIP_SCALE * (DIST_RATIO - ONE)**SKIP_EXPONENT))
          END IF
        END SUBROUTINE Q1NP_CONTACT_UPDATE_SKIP

      END MODULE Q1NP_CONTACT_DRIVER_MOD
