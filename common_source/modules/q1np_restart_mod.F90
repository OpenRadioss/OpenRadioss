!====================================================================
!  Q1NP_RESTART_MOD              common_source/modules/q1np_restart_mod.F90
!====================================================================
      MODULE Q1NP_RESTART_MOD
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        USE PRECISION_MOD, ONLY : WP
        USE MY_ALLOC_MOD, ONLY : MY_ALLOC
        USE MY_DEALLOC_MOD, ONLY : MY_DEALLOC
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
        IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Global variables
! ----------------------------------------------------------------------------------------------------------------------
        INTEGER :: NUMELQ1NP_G      = 0
        INTEGER :: SKQ1NP_G         = 0
        INTEGER :: SIQ1NP_G         = 0
        INTEGER :: SQ1NPBULK_G      = 0
        INTEGER :: SQ1NPCTRL_SHARED_G = 0
        INTEGER :: SQ1NPCTRL_L_G    = 0
        INTEGER :: SQ1NPWEIGHT_L_G  = 0
        INTEGER :: SQ1NPKNOT_L_G    = 0
        INTEGER :: TABVINT_LEN_G    = 0
        INTEGER :: NKQ1NP_G         = 0
        INTEGER :: NUMNOD_CP_ADDED  = 0
        INTEGER :: NUMNOD_OLD       = 0
        INTEGER :: NKQ1NP,NBULKQ1NP
        PARAMETER (NKQ1NP=15)
        PARAMETER (NBULKQ1NP=4)
        INTEGER, PARAMETER :: IDEBUG_Q1NP = 2
!  
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: KQ1NP_TAB
        INTEGER, DIMENSION(:),   ALLOCATABLE :: IQ1NP_TAB
        INTEGER, DIMENSION(:),   ALLOCATABLE :: IQ1NP_BULK_TAB
        INTEGER, DIMENSION(:),   ALLOCATABLE :: KQ1NP_TAB_INV
        REAL(KIND=WP), DIMENSION(:),   ALLOCATABLE, TARGET :: Q1NP_KTAB
        REAL(KIND=WP), DIMENSION(:),   ALLOCATABLE :: Q1NP_WTAB
        REAL(KIND=WP), DIMENSION(:,:), ALLOCATABLE :: Q1NP_CPTAB    
! 
!     Q1NP Gauss quadrature scheme variables
! 
        INTEGER :: Q1NP_NP_U_G = 0  ! Number of Gauss points in U direction
        INTEGER :: Q1NP_NP_V_G = 0  ! Number of Gauss points in V direction
        INTEGER :: Q1NP_NP_T_G = 0  ! Number of Gauss points in T direction
        REAL(KIND=WP), DIMENSION(:), ALLOCATABLE :: Q1NP_GP_U_G  ! Gauss point coordinates in U direction
        REAL(KIND=WP), DIMENSION(:), ALLOCATABLE :: Q1NP_GP_V_G  ! Gauss point coordinates in V direction
        REAL(KIND=WP), DIMENSION(:), ALLOCATABLE :: Q1NP_GP_T_G  ! Gauss point coordinates in T direction
        REAL(KIND=WP), DIMENSION(:), ALLOCATABLE :: Q1NP_GW_U_G  ! Gauss weights in U direction
        REAL(KIND=WP), DIMENSION(:), ALLOCATABLE :: Q1NP_GW_V_G  ! Gauss weights in V direction
        REAL(KIND=WP), DIMENSION(:), ALLOCATABLE :: Q1NP_GW_T_G  ! Gauss weights in T direction
! 
!     Q1NP grid dimensions and knot vectors
! 
        INTEGER :: Q1NP_NX_G = 0  ! Number of elements in U direction
        INTEGER :: Q1NP_NY_G = 0  ! Number of elements in V direction
        ! Knot vectors pool (U then V), concatenated across knot sets.
        REAL(KIND=WP), DIMENSION(:), POINTER :: Q1NP_KTAB_G => NULL()

        ! Per-knot-set metadata to support heterogeneous NX/NY.
        INTEGER :: Q1NP_NKNOT_SETS_G = 2
        INTEGER, ALLOCATABLE :: Q1NP_NX_SET_G(:)      ! NX for each knot set
        INTEGER, ALLOCATABLE :: Q1NP_NY_SET_G(:)      ! NY for each knot set
        INTEGER, ALLOCATABLE :: Q1NP_KTAB_OFF_G(:)    ! 1-based start index into Q1NP_KTAB_G
        INTEGER, ALLOCATABLE :: Q1NP_KTAB_LEN_G(:)    ! number of entries for (U then V)

      CONTAINS

        SUBROUTINE RESET_Q1NP_COUNTS()
          NUMELQ1NP_G      = 0
          SKQ1NP_G         = 0
          SIQ1NP_G         = 0
          SQ1NPBULK_G      = 0
          SQ1NPCTRL_SHARED_G = 0
          SQ1NPCTRL_L_G    = 0
          SQ1NPWEIGHT_L_G  = 0
          SQ1NPKNOT_L_G    = 0
          TABVINT_LEN_G    = 0
          NKQ1NP_G         = 0
          Q1NP_NP_U_G      = 0
          Q1NP_NP_V_G      = 0
          Q1NP_NP_T_G      = 0
          Q1NP_NX_G        = 0
          Q1NP_NY_G        = 0
          Q1NP_KTAB_G      => NULL()

          Q1NP_NKNOT_SETS_G = 0
          IF (ALLOCATED(Q1NP_NX_SET_G)) CALL MY_DEALLOC(Q1NP_NX_SET_G)
          IF (ALLOCATED(Q1NP_NY_SET_G)) CALL MY_DEALLOC(Q1NP_NY_SET_G)
          IF (ALLOCATED(Q1NP_KTAB_OFF_G)) CALL MY_DEALLOC(Q1NP_KTAB_OFF_G)
          IF (ALLOCATED(Q1NP_KTAB_LEN_G)) CALL MY_DEALLOC(Q1NP_KTAB_LEN_G)
          IF (ALLOCATED(Q1NP_GP_U_G)) CALL MY_DEALLOC(Q1NP_GP_U_G)
          IF (ALLOCATED(Q1NP_GP_V_G)) CALL MY_DEALLOC(Q1NP_GP_V_G)
          IF (ALLOCATED(Q1NP_GP_T_G)) CALL MY_DEALLOC(Q1NP_GP_T_G)
          IF (ALLOCATED(Q1NP_GW_U_G)) CALL MY_DEALLOC(Q1NP_GW_U_G)
          IF (ALLOCATED(Q1NP_GW_V_G)) CALL MY_DEALLOC(Q1NP_GW_V_G)
          IF (ALLOCATED(Q1NP_GW_T_G)) CALL MY_DEALLOC(Q1NP_GW_T_G)
        END SUBROUTINE RESET_Q1NP_COUNTS

        SUBROUTINE RESET_Q1NP_STATE()
          CALL RESET_Q1NP_COUNTS()
        END SUBROUTINE RESET_Q1NP_STATE

        SUBROUTINE SET_Q1NP_COUNTS(NUMELQ1NP_IN, &
          SKQ1NP,SIQ1NP,SQ1NPBULK,                &
          SQ1NPCTRL_SHARED,SQ1NPCTRL_L,          &
          SQ1NPWEIGHT_L,SQ1NPKNOT_L)
          INTEGER, INTENT(IN) :: NUMELQ1NP_IN
          INTEGER, INTENT(IN) :: SKQ1NP,SIQ1NP,SQ1NPBULK
          INTEGER, INTENT(IN) :: SQ1NPCTRL_SHARED,SQ1NPCTRL_L
          INTEGER, INTENT(IN) :: SQ1NPWEIGHT_L,SQ1NPKNOT_L

          NUMELQ1NP_G        = NUMELQ1NP_IN
          SKQ1NP_G           = SKQ1NP
          SIQ1NP_G           = SIQ1NP
          SQ1NPBULK_G        = SQ1NPBULK
          SQ1NPCTRL_SHARED_G = SQ1NPCTRL_SHARED
          SQ1NPCTRL_L_G      = SQ1NPCTRL_L
          SQ1NPWEIGHT_L_G    = SQ1NPWEIGHT_L
          SQ1NPKNOT_L_G      = SQ1NPKNOT_L
        END SUBROUTINE SET_Q1NP_COUNTS

        SUBROUTINE SET_Q1NP_KNOT_SETS(NSETS_IN, NX_SET_IN, NY_SET_IN, KTAB_OFF_IN, KTAB_LEN_IN)
          INTEGER, INTENT(IN) :: NSETS_IN
          INTEGER, INTENT(IN) :: NX_SET_IN(:)
          INTEGER, INTENT(IN) :: NY_SET_IN(:)
          INTEGER, INTENT(IN) :: KTAB_OFF_IN(:)
          INTEGER, INTENT(IN) :: KTAB_LEN_IN(:)

          INTEGER :: I

          Q1NP_NKNOT_SETS_G = NSETS_IN

          IF (NSETS_IN <= 0) RETURN

          IF (ALLOCATED(Q1NP_NX_SET_G)) CALL MY_DEALLOC(Q1NP_NX_SET_G)
          IF (ALLOCATED(Q1NP_NY_SET_G)) CALL MY_DEALLOC(Q1NP_NY_SET_G)
          IF (ALLOCATED(Q1NP_KTAB_OFF_G)) CALL MY_DEALLOC(Q1NP_KTAB_OFF_G)
          IF (ALLOCATED(Q1NP_KTAB_LEN_G)) CALL MY_DEALLOC(Q1NP_KTAB_LEN_G)

          CALL MY_ALLOC(Q1NP_NX_SET_G, NSETS_IN, "Q1NP_NX_SET_G")
          CALL MY_ALLOC(Q1NP_NY_SET_G, NSETS_IN, "Q1NP_NY_SET_G")
          CALL MY_ALLOC(Q1NP_KTAB_OFF_G, NSETS_IN, "Q1NP_KTAB_OFF_G")
          CALL MY_ALLOC(Q1NP_KTAB_LEN_G, NSETS_IN, "Q1NP_KTAB_LEN_G")

          DO I = 1, NSETS_IN
            Q1NP_NX_SET_G(I)   = NX_SET_IN(I)
            Q1NP_NY_SET_G(I)   = NY_SET_IN(I)
            Q1NP_KTAB_OFF_G(I) = KTAB_OFF_IN(I)
            Q1NP_KTAB_LEN_G(I) = KTAB_LEN_IN(I)
          ENDDO

          ! Keep legacy single-knot-set view as the first set.
          Q1NP_NX_G = Q1NP_NX_SET_G(1)
          Q1NP_NY_G = Q1NP_NY_SET_G(1)
        END SUBROUTINE SET_Q1NP_KNOT_SETS

        SUBROUTINE SET_Q1NP_TABVINT_LEN(LEN)
          INTEGER, INTENT(IN) :: LEN
          TABVINT_LEN_G = LEN
        END SUBROUTINE SET_Q1NP_TABVINT_LEN

        SUBROUTINE Q1NP_INIT_GAUSS_SCHEME_STARTER(NP_U, NP_V, NP_T)
          INTEGER, INTENT(IN) :: NP_U, NP_V, NP_T
!         INTEGER :: I  ! no local variables currently needed
! 
!         Set number of Gauss points
          Q1NP_NP_U_G = NP_U
          Q1NP_NP_V_G = NP_V
          Q1NP_NP_T_G = NP_T
! 
!         Deallocate if already allocated
          IF (ALLOCATED(Q1NP_GP_U_G)) CALL MY_DEALLOC(Q1NP_GP_U_G)
          IF (ALLOCATED(Q1NP_GP_V_G)) CALL MY_DEALLOC(Q1NP_GP_V_G)
          IF (ALLOCATED(Q1NP_GP_T_G)) CALL MY_DEALLOC(Q1NP_GP_T_G)
          IF (ALLOCATED(Q1NP_GW_U_G)) CALL MY_DEALLOC(Q1NP_GW_U_G)
          IF (ALLOCATED(Q1NP_GW_V_G)) CALL MY_DEALLOC(Q1NP_GW_V_G)
          IF (ALLOCATED(Q1NP_GW_T_G)) CALL MY_DEALLOC(Q1NP_GW_T_G)
! 
!         Allocate arrays
          IF (NP_U > 0) THEN
            CALL MY_ALLOC(Q1NP_GP_U_G, NP_U, "Q1NP_GP_U_G")
            CALL MY_ALLOC(Q1NP_GW_U_G, NP_U, "Q1NP_GW_U_G")
            CALL Q1NP_GAUSS_1D(NP_U, Q1NP_GP_U_G, Q1NP_GW_U_G)
          END IF

          IF (NP_V > 0) THEN
            CALL MY_ALLOC(Q1NP_GP_V_G, NP_V, "Q1NP_GP_V_G")
            CALL MY_ALLOC(Q1NP_GW_V_G, NP_V, "Q1NP_GW_V_G")
            CALL Q1NP_GAUSS_1D(NP_V, Q1NP_GP_V_G, Q1NP_GW_V_G)
          END IF

          IF (NP_T > 0) THEN
            CALL MY_ALLOC(Q1NP_GP_T_G, NP_T, "Q1NP_GP_T_G")
            CALL MY_ALLOC(Q1NP_GW_T_G, NP_T, "Q1NP_GW_T_G")
            CALL Q1NP_GAUSS_1D(NP_T, Q1NP_GP_T_G, Q1NP_GW_T_G)
          END IF

        END SUBROUTINE Q1NP_INIT_GAUSS_SCHEME_STARTER

!       Thread-safe variant of Q1NP_INIT_GAUSS_SCHEME_STARTER.
!       It writes ONLY to the caller-supplied allocatable arrays (no
!       module-global state), so it can be called concurrently from an
!       OpenMP parallel region (e.g. the FORINT element-group loop that
!       drives Q1NP_FORC3). Each caller keeps its own thread-local scheme.
!       NOTE: the ALLOCATE statements below are intentionally kept as raw
!       Fortran allocations (NOT MY_ALLOC). A named MY_ALLOC call records
!       the allocation address in a shared global tracker, which is not
!       thread-safe and would race when this routine runs inside an
!       OpenMP parallel region.
        SUBROUTINE Q1NP_BUILD_GAUSS_SCHEME(NP_U, NP_V, NP_T, &
     &      GP_U, GW_U, GP_V, GW_V, GP_T, GW_T)
          INTEGER, INTENT(IN) :: NP_U, NP_V, NP_T
          REAL(KIND=WP), ALLOCATABLE, INTENT(INOUT) :: GP_U(:), GW_U(:)
          REAL(KIND=WP), ALLOCATABLE, INTENT(INOUT) :: GP_V(:), GW_V(:)
          REAL(KIND=WP), ALLOCATABLE, INTENT(INOUT) :: GP_T(:), GW_T(:)

          IF (ALLOCATED(GP_U)) DEALLOCATE(GP_U)
          IF (ALLOCATED(GW_U)) DEALLOCATE(GW_U)
          IF (ALLOCATED(GP_V)) DEALLOCATE(GP_V)
          IF (ALLOCATED(GW_V)) DEALLOCATE(GW_V)
          IF (ALLOCATED(GP_T)) DEALLOCATE(GP_T)
          IF (ALLOCATED(GW_T)) DEALLOCATE(GW_T)

          IF (NP_U > 0) THEN
            ALLOCATE(GP_U(NP_U), GW_U(NP_U))
            CALL Q1NP_GAUSS_1D(NP_U, GP_U, GW_U)
          END IF

          IF (NP_V > 0) THEN
            ALLOCATE(GP_V(NP_V), GW_V(NP_V))
            CALL Q1NP_GAUSS_1D(NP_V, GP_V, GW_V)
          END IF

          IF (NP_T > 0) THEN
            ALLOCATE(GP_T(NP_T), GW_T(NP_T))
            CALL Q1NP_GAUSS_1D(NP_T, GP_T, GW_T)
          END IF

        END SUBROUTINE Q1NP_BUILD_GAUSS_SCHEME

        SUBROUTINE Q1NP_GAUSS_1D(N, GP, GW)
          INTEGER, INTENT(IN) :: N
          REAL(KIND=WP), INTENT(INOUT) :: GP(N), GW(N)
          INTEGER :: I
! 
!         Simple Gauss-Legendre quadrature for [-1,1]
!         For small orders, use hardcoded values
          IF (N == 1) THEN
            GP(1) = 0.0_wp
            GW(1) = 2.0_wp
          ELSEIF (N == 2) THEN
            GP(1) = -0.577350269189626_wp
            GP(2) =  0.577350269189626_wp
            GW(1) = 1.0_wp
            GW(2) = 1.0_wp
          ELSEIF (N == 3) THEN
            GP(1) = -0.774596669241483_wp
            GP(2) =  0.0_wp
            GP(3) =  0.774596669241483_wp
            GW(1) = 0.555555555555556_wp
            GW(2) = 0.888888888888889_wp
            GW(3) = 0.555555555555556_wp
          ELSEIF (N == 4) THEN
            GP(1) = -0.861136311594053_wp
            GP(2) = -0.339981043584856_wp
            GP(3) =  0.339981043584856_wp
            GP(4) =  0.861136311594053_wp
            GW(1) = 0.347854845137454_wp
            GW(2) = 0.652145154862546_wp
            GW(3) = 0.652145154862546_wp
            GW(4) = 0.347854845137454_wp
          ELSEIF (N == 5) THEN
            GP(1) = -0.906179845938664_wp
            GP(2) = -0.538469310105683_wp
            GP(3) =  0.0_wp
            GP(4) =  0.538469310105683_wp
            GP(5) =  0.906179845938664_wp
            GW(1) = 0.236926885056189_wp
            GW(2) = 0.478628670499366_wp
            GW(3) = 0.568888888888889_wp
            GW(4) = 0.478628670499366_wp
            GW(5) = 0.236926885056189_wp
          ELSE
!           For higher orders, use uniform distribution as fallback
            DO I = 1, N
              GP(I) = -1.0_wp + 2.0_wp * REAL(I-1, KIND=WP) / REAL(N-1, KIND=WP)
              GW(I) =  2.0_wp / REAL(N, KIND=WP)
            END DO
          END IF

        END SUBROUTINE Q1NP_GAUSS_1D

      END MODULE Q1NP_RESTART_MOD

