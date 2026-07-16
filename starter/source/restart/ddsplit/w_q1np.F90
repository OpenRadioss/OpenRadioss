!====================================================================
!  W_Q1NP_INT / W_Q1NP_REAL   starter/source/restart/ddsplit/w_q1np.F
!====================================================================
      MODULE W_Q1NP_MOD
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        USE PRECISION_MOD, ONLY : WP
        USE Q1NP_RESTART_MOD
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
        IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Public interface
! ----------------------------------------------------------------------------------------------------------------------
        PUBLIC :: W_Q1NP_INT
        PUBLIC :: W_Q1NP_REAL

      CONTAINS

        SUBROUTINE W_Q1NP_INT(NUMELQ1NP_IN, NKQ1NP, KQ1NP_TAB, &
          IQ1NP_TAB, IQ1NP_BULK_TAB, NODLOCAL, CEL,            &
          CEP, IPROC, NUMELS, NUMELS_L, LEN_IA)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Dummy arguments
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER, INTENT(IN)    :: NUMELQ1NP_IN, NKQ1NP
          INTEGER, INTENT(IN)    :: KQ1NP_TAB(NKQ1NP, *)
          INTEGER, INTENT(IN)    :: IQ1NP_TAB(*) 
          INTEGER, INTENT(IN)    :: IQ1NP_BULK_TAB(*) 
          INTEGER, INTENT(IN)    :: NODLOCAL(*) ! Local node ID
          INTEGER, INTENT(IN)    :: CEL(*) ! HEX8 element ID
          INTEGER, INTENT(IN)    :: CEP(*) ! Domain owner of each solid element (0-based)
          INTEGER, INTENT(IN)    :: IPROC ! Current domain id (0-based)
          INTEGER, INTENT(IN)    :: NUMELS ! Global number of solid elements
          INTEGER, INTENT(IN)    :: NUMELS_L ! Local number of solid elements in this domain
          INTEGER, INTENT(INOUT) :: LEN_IA 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER :: BASE_INT_COUNT, CTRL_INT_COUNT, BULK_INT_COUNT
          INTEGER :: IERR, IEL, I, N_CTRL, OFFSET_CTRL, OFFSET_BULK
          INTEGER :: NIN_SRC, IEL_HEX8_SRC
          INTEGER, ALLOCATABLE :: KQ1NP_TAB_LOC(:,:), IQ1NP_TAB_LOC(:), IQ1NP_BULK_TAB_LOC(:)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          IF (NUMELQ1NP_IN <= 0) RETURN

          BASE_INT_COUNT = NKQ1NP * NUMELQ1NP_IN
          CTRL_INT_COUNT = MAX(0, SIQ1NP_G)
          BULK_INT_COUNT = MAX(0, SQ1NPBULK_G)

          IF (SKQ1NP_G > 0 .AND. SKQ1NP_G /= BASE_INT_COUNT) THEN
            WRITE(*, '(A,I10,A,I10)')                           &
              '    WARNING: Expected base ints=', BASE_INT_COUNT,  &
              ' but SKQ1NP_G=', SKQ1NP_G
          END IF

          ALLOCATE(KQ1NP_TAB_LOC(NKQ1NP,NUMELQ1NP_IN),STAT=IERR)
          IF (IERR /= 0) THEN
            CALL W_Q1NP_INT_WRITE_MAIN_INT_BLOCKS(LEN_IA, .FALSE.)
            CALL W_Q1NP_INT_APPEND_META(LEN_IA)
            RETURN
          END IF

          IF (CTRL_INT_COUNT > 0) THEN
            ALLOCATE(IQ1NP_TAB_LOC(CTRL_INT_COUNT),STAT=IERR)
          ELSE
            ALLOCATE(IQ1NP_TAB_LOC(0),STAT=IERR)
          END IF
          IF (IERR /= 0) THEN
            DEALLOCATE(KQ1NP_TAB_LOC)
            CALL W_Q1NP_INT_WRITE_MAIN_INT_BLOCKS(LEN_IA, .FALSE.)
            CALL W_Q1NP_INT_APPEND_META(LEN_IA)
            RETURN
          END IF

          IF (BULK_INT_COUNT > 0) THEN
            ALLOCATE(IQ1NP_BULK_TAB_LOC(BULK_INT_COUNT),STAT=IERR)
          ELSE
            ALLOCATE(IQ1NP_BULK_TAB_LOC(0),STAT=IERR)
          END IF
          IF (IERR /= 0) THEN
            DEALLOCATE(KQ1NP_TAB_LOC, IQ1NP_TAB_LOC)
            CALL W_Q1NP_INT_WRITE_MAIN_INT_BLOCKS(LEN_IA, .FALSE.)
            CALL W_Q1NP_INT_APPEND_META(LEN_IA)
            RETURN
          END IF

          KQ1NP_TAB_LOC = KQ1NP_TAB(1:NKQ1NP,1:NUMELQ1NP_IN)
          IF (CTRL_INT_COUNT > 0) IQ1NP_TAB_LOC = IQ1NP_TAB(1:CTRL_INT_COUNT)
          IF (BULK_INT_COUNT > 0) IQ1NP_BULK_TAB_LOC = IQ1NP_BULK_TAB(1:BULK_INT_COUNT)

          DO IEL = 1, NUMELQ1NP_IN
            IEL_HEX8_SRC = KQ1NP_TAB(10, IEL)
            IF (IEL_HEX8_SRC > 0) THEN
              IF (CEL(IEL_HEX8_SRC) > 0) THEN
                KQ1NP_TAB_LOC(10, IEL) = CEL(IEL_HEX8_SRC)
              END IF
            END IF

            N_CTRL = KQ1NP_TAB(3, IEL)
            OFFSET_CTRL = KQ1NP_TAB(4, IEL)
            DO I = 1, N_CTRL
              NIN_SRC = IQ1NP_TAB(OFFSET_CTRL + I - 1)
              IF (NIN_SRC > 0) THEN
                IF (NODLOCAL(NIN_SRC) > 0) THEN
                  IQ1NP_TAB_LOC(OFFSET_CTRL + I - 1) = NODLOCAL(NIN_SRC)
                END IF
              END IF
            END DO

            OFFSET_BULK = KQ1NP_TAB(14, IEL)
            DO I = 1, 4
              NIN_SRC = IQ1NP_BULK_TAB(OFFSET_BULK + I - 1)
              IF (NIN_SRC > 0) THEN
                IF (NODLOCAL(NIN_SRC) > 0) THEN
                  IQ1NP_BULK_TAB_LOC(OFFSET_BULK + I - 1) = NODLOCAL(NIN_SRC)
                END IF
              END IF
            END DO
          END DO

          CALL W_Q1NP_INT_WRITE_MAIN_INT_BLOCKS(LEN_IA, .TRUE.)

          DEALLOCATE(KQ1NP_TAB_LOC, IQ1NP_TAB_LOC, IQ1NP_BULK_TAB_LOC)

          CALL W_Q1NP_INT_APPEND_META(LEN_IA)

        CONTAINS

          SUBROUTINE W_Q1NP_INT_WRITE_MAIN_INT_BLOCKS(LEN_IA, USE_LOC)
            INTEGER, INTENT(INOUT) :: LEN_IA
            LOGICAL, INTENT(IN)     :: USE_LOC

!           Main integer block: per-element connectivity/header (KQ1NP)
            IF (USE_LOC) THEN
              CALL WRITE_I_C(KQ1NP_TAB_LOC, BASE_INT_COUNT)
            ELSE
              CALL WRITE_I_C(KQ1NP_TAB, BASE_INT_COUNT)
            END IF
            LEN_IA = LEN_IA + BASE_INT_COUNT
            IF (CTRL_INT_COUNT > 0) THEN

!             Control-point node references for all Q1NP elements (IQ1NP)
              IF (USE_LOC) THEN
                CALL WRITE_I_C(IQ1NP_TAB_LOC, CTRL_INT_COUNT)
              ELSE
                CALL WRITE_I_C(IQ1NP_TAB, CTRL_INT_COUNT)
              END IF
              LEN_IA = LEN_IA + CTRL_INT_COUNT
            END IF
            IF (BULK_INT_COUNT > 0) THEN

!             Embedded HEX8 bulk-node references (IQ1NP_BULK)
              IF (USE_LOC) THEN
                CALL WRITE_I_C(IQ1NP_BULK_TAB_LOC, BULK_INT_COUNT)
              ELSE
                CALL WRITE_I_C(IQ1NP_BULK_TAB, BULK_INT_COUNT)
              END IF
              LEN_IA = LEN_IA + BULK_INT_COUNT
            END IF
          END SUBROUTINE W_Q1NP_INT_WRITE_MAIN_INT_BLOCKS

          SUBROUTINE W_Q1NP_INT_APPEND_META(LEN_IA)
            INTEGER, INTENT(INOUT) :: LEN_IA
            INTEGER :: NSETS
            INTEGER, DIMENSION(1) :: NSETS_ARR
!           Knot-set metadata (heterogeneous NX/NY support)
            NSETS = MAX(0, Q1NP_NKNOT_SETS_G)
            NSETS_ARR(1) = NSETS
            
!           Number of knot sets in this restart block
            CALL WRITE_I_C(NSETS_ARR, 1)
            IF (NSETS > 0) THEN
!             Per-set knot metadata: NX, NY, knot-offset, knot-length
              CALL WRITE_I_C(Q1NP_NX_SET_G, NSETS)
              CALL WRITE_I_C(Q1NP_NY_SET_G, NSETS)
              CALL WRITE_I_C(Q1NP_KTAB_OFF_G, NSETS)
              CALL WRITE_I_C(Q1NP_KTAB_LEN_G, NSETS)
            END IF
            LEN_IA = LEN_IA + 1 + 4 * NSETS

!           Q1NP inverse connectivity (global solid element -> Q1NP element),
!           localized to the current domain (matches rdresb.F/wrrestp.F read order)
            CALL W_IELOC(KQ1NP_TAB_INV, CEP, IPROC, NUMELS, NUMELS_L, LEN_IA)
          END SUBROUTINE W_Q1NP_INT_APPEND_META

        END SUBROUTINE W_Q1NP_INT

        SUBROUTINE W_Q1NP_REAL(Q1NP_WTAB, Q1NP_KTAB, Q1NP_CPTAB, LEN_AM)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Dummy arguments
! ----------------------------------------------------------------------------------------------------------------------
          REAL(KIND=WP), INTENT(IN)    :: Q1NP_WTAB(*)
          REAL(KIND=WP), INTENT(IN)    :: Q1NP_KTAB(*)
          REAL(KIND=WP), INTENT(IN)    :: Q1NP_CPTAB(3, *)
          INTEGER,       INTENT(INOUT) :: LEN_AM
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          INTEGER        :: WEIGHT_COUNT, KNOT_COUNT, CP_COUNT
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          WEIGHT_COUNT = MAX(0, SQ1NPWEIGHT_L_G)
          KNOT_COUNT   = MAX(0, SQ1NPKNOT_L_G)
          CP_COUNT     = MAX(0, SQ1NPCTRL_SHARED_G)

          IF (WEIGHT_COUNT + KNOT_COUNT + CP_COUNT <= 0) RETURN

          IF (WEIGHT_COUNT > 0) THEN
!           NURBS weights
            CALL WRITE_DB(Q1NP_WTAB, WEIGHT_COUNT)
            LEN_AM = LEN_AM + WEIGHT_COUNT
          END IF

          IF (KNOT_COUNT > 0) THEN
!           Concatenated knot vectors
            CALL WRITE_DB(Q1NP_KTAB, KNOT_COUNT)
            LEN_AM = LEN_AM + KNOT_COUNT
          END IF

          IF (CP_COUNT > 0) THEN
!           Shared control points (X/Y/Z packed)
            CALL WRITE_DB(Q1NP_CPTAB, 3 * CP_COUNT)
            LEN_AM = LEN_AM + 3 * CP_COUNT
          END IF

        END SUBROUTINE W_Q1NP_REAL

      END MODULE W_Q1NP_MOD

