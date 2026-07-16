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
!||    q1np_geom_mod                    ../common_source/modules/q1np_geom_mod.F90
!||--- called by ------------------------------------------------------
!||    q1np_forc3                        ../engine/source/elements/solid/solid_q1np/q1np_forc3.F90
!||    q1np_genelements                  ../starter/source/elements/solid/solid_q1np/q1np_genelements.F90
!||    q1np_init_lbuf_vol                ../starter/source/elements/solid/solid_q1np/q1np_init_lbuf_vol.F90
!||    q1np_mass3                        ../starter/source/elements/solid/solid_q1np/q1np_mass3.F90
!||    q1np_volume                       ../starter/source/elements/solid/solid_q1np/q1np_volume.F90
!||====================================================================
      MODULE Q1NP_GEOM_MOD
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        USE PRECISION_MOD, ONLY : WP
        USE CONSTANT_MOD , ONLY : ZERO, ONE, TWO, HALF, FOURTH
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
        IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Public interface
! ----------------------------------------------------------------------------------------------------------------------
        PUBLIC :: Q1NP_GET_KNOT_VECTORS ! Extract U,V knot vectors from Q1NP_KTAB
        PUBLIC :: Q1NP_DERS_BASIS_FUNS ! Compute B-spline basis functions and their derivatives
        PUBLIC :: Q1NP_SHAPE_FUNCTIONS ! Compute Q1NP shape functions and their derivatives (NURBS top + bilinear bottom)
        PUBLIC :: Q1NP_JACOBIAN ! Jacobian matrix and determinant computation
        PUBLIC :: Q1NP_FIND_SPAN ! find knot span index i such that U(i) <= UVAL < U(i+1).
        PUBLIC :: Q1NP_BERNSTEIN_BASIS ! Bernstein basis of degree P at T in [0,1].
        PUBLIC :: Q1NP_KNOT_SINGLE_SPAN ! Check if knot vector has only one span
        PUBLIC :: Q1NP_BASIS_ROW_AT_UV ! fill one row of design matrix for least-squares.

      CONTAINS

!C=======================================================================
!C   Q1NP_GET_KNOT_VECTORS: Extract U,V knot vectors from Q1NP_KTAB
!C=======================================================================
        SUBROUTINE Q1NP_GET_KNOT_VECTORS(NX, NY, P, Q, Q1NP_KTAB, U, V)
  !C-----------------------------------------------
  !C   D u m m y   A r g u m e n t s
  !C-----------------------------------------------
          INTEGER, INTENT(IN) :: NX, NY, P, Q
          REAL(KIND=WP), INTENT(IN)  :: Q1NP_KTAB(:)
          REAL(KIND=WP), INTENT(OUT) :: U(:), V(:)
  !C-----------------------------------------------
  !C   L o c a l   V a r i a b l e s
  !C-----------------------------------------------
          INTEGER :: NKNOT_U, NKNOT_V, IV_OFFSET, I
  !C----------------------------------------------------------------------
  !C   Knot vector length: n_knots = n_control + 2*order + 1 (open B-spline)
  !C----------------------------------------------------------------------
          NKNOT_U = NX + 2*P + 1
          NKNOT_V = NY + 2*Q + 1
          IV_OFFSET = NKNOT_U   ! V starts after U in Q1NP_KTAB

  !C----------------------------------------------------------------------
  !C   Extract U knot vector (first NKNOT_U entries)
  !C----------------------------------------------------------------------
          DO I = 1, NKNOT_U
            U(I) = Q1NP_KTAB(I)
          END DO

  !C----------------------------------------------------------------------
  !C   Extract V knot vector (starts at NKNOT_U + 1)
  !C----------------------------------------------------------------------
          DO I = 1, NKNOT_V
            V(I) = Q1NP_KTAB(IV_OFFSET + I)
          END DO

          RETURN
        END SUBROUTINE Q1NP_GET_KNOT_VECTORS

!C=======================================================================
!C   Q1NP_DERS_BASIS_FUNS: Compute B-spline basis functions and their derivatives
!C=======================================================================
      SUBROUTINE Q1NP_DERS_BASIS_FUNS(SPAN, UVAL, P, U, NDERS, DERS)
  !C----------------------------------------------------------------------
  !C   D u m m y   A r g u m e n t s
  !C----------------------------------------------------------------------
          INTEGER, INTENT(IN) :: SPAN, P, NDERS
          REAL(KIND=WP), INTENT(IN) :: UVAL
          REAL(KIND=WP), INTENT(IN) :: U(:)
          REAL(KIND=WP), INTENT(OUT) :: DERS(0:NDERS,0:P)
  !C----------------------------------------------------------------------
  !C   L o c a l   V a r i a b l e s
  !C----------------------------------------------------------------------
          INTEGER :: J, R, K, RK, PK, J1, J2
          REAL(KIND=WP) :: LEFT(0:P), RIGHT(0:P)
          REAL(KIND=WP) :: NDU(0:P,0:P)
          REAL(KIND=WP) :: A(0:1,0:P)
          REAL(KIND=WP) :: TEMP, DENOM, D, RFACT
          INTEGER :: S1, S2
          REAL(KIND=WP), PARAMETER :: TOL_NDU = 1.0E-15_WP

  !C----------------------------------------------------------------------
  !C   Build NDU: Cox-de Boor recursion. NDU(j,r) = knot diff; NDU(r,j) = basis. (2D array)
  !C----------------------------------------------------------------------
          NDU(0,0) = ONE

          DO J = 1, P
            LEFT(J)  = UVAL - U(SPAN + 1 - J)
            RIGHT(J) = U(SPAN + J) - UVAL
            TEMP = ZERO

            DO R = 0, J-1
              DENOM = RIGHT(R+1) + LEFT(J-R)
              IF (ABS(DENOM) .LE. TOL_NDU) THEN
                NDU(J,R) = ZERO
              ELSE
                NDU(J,R) = DENOM
              END IF

              IF (ABS(NDU(J,R)) .GT. TOL_NDU) THEN
                DENOM = NDU(R,J-1) / NDU(J,R)
              ELSE
                DENOM = ZERO
              END IF

              NDU(R,J) = TEMP + RIGHT(R+1) * DENOM
              TEMP     = LEFT(J-R) * DENOM
            END DO

            NDU(J,J) = TEMP
          END DO

  !C----------------------------------------------------------------------
  !C   Extract basis functions DERS(0,J) = NDU(J,P)
  !C----------------------------------------------------------------------
          DO J = 0, P
            DERS(0,J) = NDU(J,P)
          END DO

          IF (NDERS <= 0) THEN
            RETURN
          END IF

  !C----------------------------------------------------------------------
  !C   Compute derivatives
  !C----------------------------------------------------------------------
          DO R = 0, P
            S1 = 0
            S2 = 1
            A(0,0) = ONE
            DO J = 1, P
              A(0,J) = ZERO
              A(1,J) = ZERO
            END DO

            DO K = 1, NDERS
              D  = ZERO
              RK = R - K
              PK = P - K

              IF (R >= K) THEN
                DENOM = NDU(PK+1,RK)
                IF (ABS(DENOM) .GT. TOL_NDU) THEN
                  A(S2,0) = A(S1,0) / DENOM
                ELSE
                  A(S2,0) = ZERO
                END IF
                D = A(S2,0) * NDU(RK,PK)
              END IF

              IF (RK >= -1) THEN
                J1 = 1
              ELSE
                J1 = -RK
              END IF

              IF (R-1 <= PK) THEN
                J2 = K-1
              ELSE
                J2 = P-R
              END IF

              DO J = J1, J2
                DENOM = NDU(PK+1,RK+J)
                IF (ABS(DENOM) .GT. TOL_NDU) THEN
                  A(S2,J) = (A(S1,J) - A(S1,J-1)) / DENOM
                ELSE
                  A(S2,J) = ZERO
                END IF
                D = D + A(S2,J) * NDU(RK+J,PK)
              END DO

              IF (R <= PK) THEN
                DENOM = NDU(PK+1,R)
                IF (ABS(DENOM) .GT. TOL_NDU) THEN
                  A(S2,K) = -A(S1,K-1) / DENOM
                ELSE
                  A(S2,K) = ZERO
                END IF
                D = D + A(S2,K) * NDU(R,PK)
              END IF

              DERS(K,R) = D

              S1 = S2
              S2 = 1 - S1
            END DO
          END DO

  !C----------------------------------------------------------------------
  !C   Multiply by factorial factors
  !C----------------------------------------------------------------------
          RFACT = REAL(P, KIND=WP)
          DO K = 1, NDERS
            DO J = 0, P
              DERS(K,J) = DERS(K,J) * RFACT
            END DO
            RFACT = RFACT * REAL(P-K, KIND=WP)
          END DO

          RETURN
        END SUBROUTINE Q1NP_DERS_BASIS_FUNS

!C=======================================================================
!C   Q1NP_SHAPE_FUNCTIONS: Compute Q1NP shape functions and their derivatives (NURBS top + bilinear bottom)
!C=======================================================================
        SUBROUTINE Q1NP_SHAPE_FUNCTIONS(XI, ETA, ZETA, P, Q, U, V, &
     &                                  ELEM_U, ELEM_V, N, DN_LOCAL)
  !C----------------------------------------------------------------------
  !C   D u m m y   A r g u m e n t s
  !C----------------------------------------------------------------------
          INTEGER,      INTENT(IN)  :: P, Q, ELEM_U, ELEM_V
          REAL(KIND=WP),INTENT(IN)  :: XI, ETA, ZETA
          REAL(KIND=WP),INTENT(IN)  :: U(:), V(:)
          REAL(KIND=WP),INTENT(OUT) :: N(:), DN_LOCAL(:,:)
  !C----------------------------------------------------------------------
  !C   L o c a l   V a r i a b l e s
  !C----------------------------------------------------------------------
          INTEGER :: N_TOP, N_TOTAL, SU, SV
          INTEGER :: I, J, IDX
          REAL(KIND=WP) :: XI_LOC, ETA_LOC
          REAL(KIND=WP) :: AU, BU, AV, BV
          REAL(KIND=WP) :: UVAL, VVAL
          REAL(KIND=WP) :: KSPAN_U, KSPAN_V
          REAL(KIND=WP) :: DU_DXI, DV_DETA
          REAL(KIND=WP) :: NU_DERS(0:1,0:P), NV_DERS(0:1,0:Q)
          REAL(KIND=WP) :: NU(0:P), DNU_DU(0:P)
          REAL(KIND=WP) :: NV(0:Q), DNV_DV(0:Q)
          REAL(KIND=WP) :: N_TOP_ARY((P+1)*(Q+1))
          REAL(KIND=WP) :: DN_TOP_DU((P+1)*(Q+1))
          REAL(KIND=WP) :: DN_TOP_DV((P+1)*(Q+1))
          REAL(KIND=WP) :: N_BOT(4), DN_BOT_DU(4), DN_BOT_DV(4)
          REAL(KIND=WP) :: NZ_TOP, NZ_BOT, DNZ_TOP, DNZ_BOT
          REAL(KIND=WP) :: INV_KSPAN_UV

  !C----------------------------------------------------------------------
  !C   Initialize N_TOP and N_TOTAL (number of shape functions on top and bottom surfaces)
  !C----------------------------------------------------------------------
          N_TOP   = (P+1)*(Q+1)
          N_TOTAL = N_TOP + 4

  !C----------------------------------------------------------------------
  !C   Map from parent [-1,1] to local [0,1]
  !C----------------------------------------------------------------------
          XI_LOC  = HALF * (XI  + ONE)
          ETA_LOC = HALF * (ETA + ONE)

  !C----------------------------------------------------------------------
  !C   Element span in U,V and current knot span [AU,BU] x [AV,BV]
  !C----------------------------------------------------------------------
          SU = P + ELEM_U + 1
          SV = Q + ELEM_V + 1
          AU = U(SU)
          BU = U(SU+1)
          AV = V(SV)
          BV = V(SV+1)

  !C----------------------------------------------------------------------
  !C   Map parent (XI,ETA) in [-1,1] to (UVAL,VVAL) in current knot span
  !C----------------------------------------------------------------------
          UVAL = AU + (BU - AU) * XI_LOC
          VVAL = AV + (BV - AV) * ETA_LOC

  !C----------------------------------------------------------------------
  !C   Compute scaling factors for derivatives
  !C----------------------------------------------------------------------
          KSPAN_U = BU - AU
          KSPAN_V = BV - AV
          DU_DXI  = HALF * KSPAN_U
          DV_DETA = HALF * KSPAN_V

  !C----------------------------------------------------------------------
  !C   Evaluate B-spline basis functions and derivatives
  !C----------------------------------------------------------------------
          CALL Q1NP_DERS_BASIS_FUNS(SU, UVAL, P, U, 1, NU_DERS)
          CALL Q1NP_DERS_BASIS_FUNS(SV, VVAL, Q, V, 1, NV_DERS)

          DO I = 0, P
            NU(I)     = NU_DERS(0,I)
            DNU_DU(I) = NU_DERS(1,I)
          END DO
          DO J = 0, Q
            NV(J)     = NV_DERS(0,J)
            DNV_DV(J) = NV_DERS(1,J)
          END DO

  !C----------------------------------------------------------------------
  !C   Tensor product for top surface
  !C----------------------------------------------------------------------
          IDX = 1
          DO J = 0, Q
            DO I = 0, P
              N_TOP_ARY(IDX)   = NU(I)     * NV(J)
              DN_TOP_DU(IDX)   = DNU_DU(I) * NV(J)
              DN_TOP_DV(IDX)   = NU(I)     * DNV_DV(J)
              IDX = IDX + 1
            END DO
          END DO

  !C----------------------------------------------------------------------
  !C   Linear blending in through-thickness direction
  !C----------------------------------------------------------------------
          NZ_TOP  = HALF * (ONE + ZETA)
          NZ_BOT  = HALF * (ONE - ZETA)
          DNZ_TOP = HALF
          DNZ_BOT = -HALF

  !C----------------------------------------------------------------------
  !C   Bottom corner shape functions in (u,v) on patch [AU,BU] x [AV,BV]
  !C----------------------------------------------------------------------
          INV_KSPAN_UV = ONE / (KSPAN_U * KSPAN_V)
          N_BOT(1) = (BU - UVAL) * (BV - VVAL) * INV_KSPAN_UV
          N_BOT(2) = (UVAL - AU) * (BV - VVAL) * INV_KSPAN_UV
          N_BOT(3) = (UVAL - AU) * (VVAL - AV) * INV_KSPAN_UV
          N_BOT(4) = (BU - UVAL) * (VVAL - AV) * INV_KSPAN_UV

          DN_BOT_DU(1) = -(BV - VVAL) * INV_KSPAN_UV
          DN_BOT_DU(2) =  (BV - VVAL) * INV_KSPAN_UV
          DN_BOT_DU(3) =  (VVAL - AV) * INV_KSPAN_UV
          DN_BOT_DU(4) = -(VVAL - AV) * INV_KSPAN_UV

          DN_BOT_DV(1) = -(BU - UVAL) * INV_KSPAN_UV
          DN_BOT_DV(2) = -(UVAL - AU) * INV_KSPAN_UV
          DN_BOT_DV(3) =  (UVAL - AU) * INV_KSPAN_UV
          DN_BOT_DV(4) =  (BU - UVAL) * INV_KSPAN_UV

  !C----------------------------------------------------------------------
  !C   Combine top and bottom
  !C----------------------------------------------------------------------
          DO I = 1, N_TOP
            N(I) = N_TOP_ARY(I) * NZ_TOP
            DN_LOCAL(I,1) = DN_TOP_DU(I) * DU_DXI  * NZ_TOP
            DN_LOCAL(I,2) = DN_TOP_DV(I) * DV_DETA * NZ_TOP
            DN_LOCAL(I,3) = N_TOP_ARY(I) * DNZ_TOP
          END DO

          DO I = 1, 4
            N(N_TOP + I) = N_BOT(I) * NZ_BOT
            DN_LOCAL(N_TOP + I,1) = DN_BOT_DU(I) * DU_DXI  * NZ_BOT
            DN_LOCAL(N_TOP + I,2) = DN_BOT_DV(I) * DV_DETA * NZ_BOT
            DN_LOCAL(N_TOP + I,3) = N_BOT(I)       * DNZ_BOT
          END DO

          RETURN
        END SUBROUTINE Q1NP_SHAPE_FUNCTIONS

!C=======================================================================
!C   Jacobian matrix and determinant computation
!C=======================================================================
        SUBROUTINE Q1NP_JACOBIAN(DN_LOCAL, XNODE, NNODE, J, DETJ, JINV, DN_GLOBAL, IERR)
  !C----------------------------------------------------------------------
  !C   D u m m y   A r g u m e n t s
  !C----------------------------------------------------------------------
          INTEGER,      INTENT(IN)  :: NNODE
          REAL(KIND=WP),INTENT(IN)  :: DN_LOCAL(NNODE,3), XNODE(3,NNODE)
          REAL(KIND=WP),INTENT(OUT) :: J(3,3), DETJ
          REAL(KIND=WP),INTENT(OUT), OPTIONAL :: JINV(3,3)
          REAL(KIND=WP),INTENT(OUT), OPTIONAL :: DN_GLOBAL(NNODE,3)
          INTEGER,      INTENT(OUT), OPTIONAL :: IERR
  !C----------------------------------------------------------------------
  !C   L o c a l   V a r i a b l e s
  !C----------------------------------------------------------------------
          INTEGER :: K
          REAL(KIND=WP) :: JINV_LOC(3,3)
          REAL(KIND=WP) :: DETJ_INV

  !C----------------------------------------------------------------------
  !C   Assemble Jacobian matrix: J(i,j) = sum_k XNODE(i,k) * DN_LOCAL(k,j)
  !C----------------------------------------------------------------------
          J = ZERO
          DO K = 1, NNODE
            J(1,1) = J(1,1) + XNODE(1,K) * DN_LOCAL(K,1)
            J(1,2) = J(1,2) + XNODE(1,K) * DN_LOCAL(K,2)
            J(1,3) = J(1,3) + XNODE(1,K) * DN_LOCAL(K,3)

            J(2,1) = J(2,1) + XNODE(2,K) * DN_LOCAL(K,1)
            J(2,2) = J(2,2) + XNODE(2,K) * DN_LOCAL(K,2)
            J(2,3) = J(2,3) + XNODE(2,K) * DN_LOCAL(K,3)

            J(3,1) = J(3,1) + XNODE(3,K) * DN_LOCAL(K,1)
            J(3,2) = J(3,2) + XNODE(3,K) * DN_LOCAL(K,2)
            J(3,3) = J(3,3) + XNODE(3,K) * DN_LOCAL(K,3)
          END DO

  !C----------------------------------------------------------------------
  !C   Compute determinant (3x3 explicit formula)
  !C----------------------------------------------------------------------
          DETJ = J(1,1)*(J(2,2)*J(3,3) - J(2,3)*J(3,2)) - &
     &           J(1,2)*(J(2,1)*J(3,3) - J(2,3)*J(3,1)) + &
     &           J(1,3)*(J(2,1)*J(3,2) - J(2,2)*J(3,1))

          IF (PRESENT(IERR)) IERR = 0

          IF (PRESENT(JINV) .OR. PRESENT(DN_GLOBAL)) THEN
            IF (DETJ <= EPSILON(ONE)) THEN
              JINV_LOC = ZERO
              IF (PRESENT(IERR)) IERR = 1
            ELSE
              DETJ_INV = ONE / DETJ
              JINV_LOC(1,1) =  (J(2,2)*J(3,3) - J(2,3)*J(3,2)) * DETJ_INV
              JINV_LOC(1,2) = -(J(1,2)*J(3,3) - J(1,3)*J(3,2)) * DETJ_INV
              JINV_LOC(1,3) =  (J(1,2)*J(2,3) - J(1,3)*J(2,2)) * DETJ_INV
              JINV_LOC(2,1) = -(J(2,1)*J(3,3) - J(2,3)*J(3,1)) * DETJ_INV
              JINV_LOC(2,2) =  (J(1,1)*J(3,3) - J(1,3)*J(3,1)) * DETJ_INV
              JINV_LOC(2,3) = -(J(1,1)*J(2,3) - J(1,3)*J(2,1)) * DETJ_INV
              JINV_LOC(3,1) =  (J(2,1)*J(3,2) - J(2,2)*J(3,1)) * DETJ_INV
              JINV_LOC(3,2) = -(J(1,1)*J(3,2) - J(1,2)*J(3,1)) * DETJ_INV
              JINV_LOC(3,3) =  (J(1,1)*J(2,2) - J(1,2)*J(2,1)) * DETJ_INV
            END IF
          END IF

          IF (PRESENT(JINV)) THEN
            JINV = JINV_LOC
          END IF

          IF (PRESENT(DN_GLOBAL)) THEN
            DN_GLOBAL = MATMUL(DN_LOCAL, JINV_LOC)
          END IF

          RETURN
        END SUBROUTINE Q1NP_JACOBIAN

!C=======================================================================
!C   Q1NP_FIND_SPAN: find knot span index i such that U(i) <= UVAL < U(i+1).
!C=======================================================================
        SUBROUTINE Q1NP_FIND_SPAN(U, NK, P, UVAL, SPAN)
  !C----------------------------------------------------------------------
  !C   D u m m y   A r g u m e n t s
  !C----------------------------------------------------------------------
          INTEGER,      INTENT(IN)  :: NK, P
          INTEGER,      INTENT(OUT) :: SPAN
          REAL(KIND=WP),INTENT(IN)  :: U(:), UVAL
  !C----------------------------------------------------------------------
  !C   L o c a l   V a r i a b l e s
  !C----------------------------------------------------------------------
          INTEGER :: I

          IF (UVAL >= U(NK)) THEN
            SPAN = NK - P - 1
            RETURN
          END IF

          SPAN = 1
          DO I = 2, NK - 1
            IF (UVAL >= U(I) .AND. UVAL < U(I+1)) THEN
              SPAN = I
              RETURN
            END IF
          END DO

          RETURN
        END SUBROUTINE Q1NP_FIND_SPAN

!C=======================================================================
!C   Q1NP_BERNSTEIN_BASIS: Bernstein basis of degree P at T in [0,1].
!C=======================================================================
        SUBROUTINE Q1NP_BERNSTEIN_BASIS(P, T, B)
          INTEGER,      INTENT(IN)  :: P
          REAL(KIND=WP),INTENT(IN)  :: T
          REAL(KIND=WP),INTENT(OUT) :: B(0:P)
          INTEGER :: I, K
          REAL(KIND=WP) :: COEF, TI, T1I

          IF (P .LE. 0) THEN
            B(0) = ONE
            RETURN
          END IF
          DO I = 0, P
            COEF = ONE
            DO K = 1, I
              COEF = COEF * REAL(P - K + 1, KIND=WP) / REAL(K, KIND=WP)
            END DO
            TI  = MERGE(ONE, T**I, I .EQ. 0)
            T1I = MERGE(ONE, (ONE - T)**(P - I), I .EQ. P)
            B(I) = COEF * TI * T1I
          END DO
          RETURN
        END SUBROUTINE Q1NP_BERNSTEIN_BASIS

!C=======================================================================
!C   Q1NP_KNOT_SINGLE_SPAN: .TRUE. if knot vector has no interior knots
!C=======================================================================
        LOGICAL FUNCTION Q1NP_KNOT_SINGLE_SPAN(V, NK)
          INTEGER,      INTENT(IN) :: NK
          REAL(KIND=WP),INTENT(IN) :: V(NK)
          INTEGER :: I
          REAL(KIND=WP), PARAMETER :: TOL = 1.0E-9_WP

          Q1NP_KNOT_SINGLE_SPAN = .TRUE.
          DO I = 1, NK
            IF (V(I) .GT. TOL .AND. V(I) .LT. (ONE - TOL)) THEN
              Q1NP_KNOT_SINGLE_SPAN = .FALSE.
              RETURN
            END IF
          END DO
          RETURN
        END FUNCTION Q1NP_KNOT_SINGLE_SPAN

!C=======================================================================
!C   Q1NP_BASIS_ROW_AT_UV: fill one row of design matrix for least-squares.
!C=======================================================================
        SUBROUTINE Q1NP_BASIS_ROW_AT_UV(UU, VV, U, V, P, Q, NCP_U, NCP_V, &
     &                                  A_ROW)
          INTEGER,      INTENT(IN) :: P, Q, NCP_U, NCP_V
          REAL(KIND=WP),INTENT(IN) :: UU, VV
          REAL(KIND=WP),INTENT(IN) :: U(:), V(:)
          REAL(KIND=WP),INTENT(OUT):: A_ROW(:)
          INTEGER :: SPAN_U, SPAN_V, COL, II, JJ, I, J, NKU, NKV
          REAL(KIND=WP) :: NU_DERS(0:0,0:10), NV_DERS(0:0,0:10)
          REAL(KIND=WP) :: UU_EVAL, VV_EVAL
          LOGICAL :: U_SINGLE_SPAN, V_SINGLE_SPAN
          REAL(KIND=WP), PARAMETER :: EPS_PARAM = 1.0E-5_WP

          UU_EVAL = UU
          VV_EVAL = VV
          IF (UU_EVAL .LE. ZERO) UU_EVAL = EPS_PARAM
          IF (UU_EVAL .GE. ONE)  UU_EVAL = ONE - EPS_PARAM
          IF (VV_EVAL .LE. ZERO) VV_EVAL = EPS_PARAM
          IF (VV_EVAL .GE. ONE)  VV_EVAL = ONE - EPS_PARAM

          DO COL = 1, NCP_U * NCP_V
            A_ROW(COL) = ZERO
          END DO

          CALL Q1NP_FIND_SPAN(U, NCP_U + P + 1, P, UU_EVAL, SPAN_U)
          CALL Q1NP_FIND_SPAN(V, NCP_V + Q + 1, Q, VV_EVAL, SPAN_V)
          NKU = SIZE(U)
          NKV = SIZE(V)
          U_SINGLE_SPAN = Q1NP_KNOT_SINGLE_SPAN(U, NKU)
          V_SINGLE_SPAN = Q1NP_KNOT_SINGLE_SPAN(V, NKV)

          IF (U_SINGLE_SPAN) THEN
            CALL Q1NP_BERNSTEIN_BASIS(P, UU_EVAL, NU_DERS(0,0:P))
          ELSE
            CALL Q1NP_DERS_BASIS_FUNS(SPAN_U, UU_EVAL, P, U, 0, NU_DERS)
          END IF
          IF (V_SINGLE_SPAN) THEN
            CALL Q1NP_BERNSTEIN_BASIS(Q, VV_EVAL, NV_DERS(0,0:Q))
          ELSE
            CALL Q1NP_DERS_BASIS_FUNS(SPAN_V, VV_EVAL, Q, V, 0, NV_DERS)
          END IF

          DO J = 0, Q
            JJ = SPAN_V - Q + J
            IF (JJ < 1 .OR. JJ > NCP_V) CYCLE
            DO I = 0, P
              II = SPAN_U - P + I
              IF (II < 1 .OR. II > NCP_U) CYCLE
              COL = (JJ - 1) * NCP_U + II
              A_ROW(COL) = NU_DERS(0,I) * NV_DERS(0,J)
            END DO
          END DO

          RETURN
        END SUBROUTINE Q1NP_BASIS_ROW_AT_UV

      END MODULE Q1NP_GEOM_MOD

