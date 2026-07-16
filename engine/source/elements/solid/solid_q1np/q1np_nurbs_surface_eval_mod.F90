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
!Copyright>        Commercial Alternative: Altair Radioss
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!||====================================================================
!||    q1np_nurbs_surface_eval_mod       ../engine/source/elements/solid/solid_q1np/q1np_nurbs_surface_eval_mod.F90
!||--- called by ------------------------------------------------------
!||    q1np_contact_broad_phase          ../engine/source/elements/solid/solid_q1np/q1np_contact_broad_phase.F90
!||--- calls      -----------------------------------------------------
!||    q1np_shape_functions              ../common_source/modules/q1np_geom_mod.F90
!||--- uses       -----------------------------------------------------
!||    precision_mod                     ../common_source/modules/precision_mod.F
!||    constant_mod                      ../common_source/modules/constant_mod.F
!||    q1np_geom_mod                     ../common_source/modules/q1np_geom_mod.F90
!||====================================================================
!
!   Standalone evaluation of a single point on the NURBS top surface of
!   a Q1NP element.
!
!   The top surface corresponds to ZETA = +1 in the parent element
!   coordinate system [-1,+1]^3.  Only the (P+1)*(Q+1) NURBS control
!   points contribute; the four bilinear bulk nodes at ZETA = -1 have
!   zero weight and are ignored.
!
      MODULE Q1NP_NURBS_SURFACE_EVALUATION_MOD
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        USE PRECISION_MOD, ONLY : WP
        USE CONSTANT_MOD , ONLY : ZERO, ONE
        USE Q1NP_GEOM_MOD, ONLY : Q1NP_SHAPE_FUNCTIONS
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
        IMPLICIT NONE
        PRIVATE
        PUBLIC :: Q1NP_EVALUATE_NURBS_TOP_SURFACE_POINT
        PUBLIC :: Q1NP_EVALUATE_NURBS_TOP_SURFACE_POINT_AND_DERIVS
        PUBLIC :: Q1NP_EVALUATE_NURBS_SHAPE_VALUES

      CONTAINS

!=======================================================================
!   Q1NP_EVALUATE_NURBS_TOP_SURFACE_POINT
!
!   Evaluate the physical (x,y,z) position of a point on the NURBS top
!   surface of one Q1NP element, given parametric coordinates (xi,eta)
!   in the parent domain [-1,+1]^2.
!
!=======================================================================
        SUBROUTINE Q1NP_EVALUATE_NURBS_TOP_SURFACE_POINT( &
     &      XI_PARAM, ETA_PARAM,       &
     &      P_DEGREE, Q_DEGREE,        &
     &      ELEM_U_IDX, ELEM_V_IDX,   &
     &      U_KNOT_VEC, V_KNOT_VEC,   &
     &      NCTRL,                     &
     &      CTRL_POINT_IDS,            &
     &      X_COORDS,                  &
     &      NUMNOD,                    &
     &      XYZ_OUT)
!C----------------------------------------------------------------------
!C   D u m m y   A r g u m e n t s
!C----------------------------------------------------------------------
!C     XI_PARAM       - parametric coordinate in u-direction, [-1,+1]   (IN)
!C     ETA_PARAM      - parametric coordinate in v-direction, [-1,+1]   (IN)
!C     P_DEGREE       - NURBS degree in u-direction                     (IN)
!C     Q_DEGREE       - NURBS degree in v-direction                     (IN)
!C     ELEM_U_IDX     - 0-based element index in u knot span            (IN)
!C     ELEM_V_IDX     - 0-based element index in v knot span            (IN)
!C     U_KNOT_VEC(:)  - U knot vector for this knot set                 (IN)
!C     V_KNOT_VEC(:)  - V knot vector for this knot set                 (IN)
!C     NCTRL          - number of NURBS control points for this element (IN)
!C     CTRL_POINT_IDS(NCTRL) - global node IDs of control points        (IN)
!C     X_COORDS(3,NUMNOD) - current global nodal coordinates            (IN)
!C     NUMNOD         - total number of nodes in the model              (IN)
!C     XYZ_OUT(3)     - computed physical position on the top surface   (OUT)
!C----------------------------------------------------------------------
          INTEGER, INTENT(IN) :: P_DEGREE, Q_DEGREE
          INTEGER, INTENT(IN) :: ELEM_U_IDX, ELEM_V_IDX
          INTEGER, INTENT(IN) :: NCTRL, NUMNOD
          INTEGER, INTENT(IN) :: CTRL_POINT_IDS(NCTRL)
          REAL(KIND=WP), INTENT(IN)  :: XI_PARAM, ETA_PARAM
          REAL(KIND=WP), INTENT(IN)  :: U_KNOT_VEC(:), V_KNOT_VEC(:)
          REAL(KIND=WP), INTENT(IN)  :: X_COORDS(3,NUMNOD)
          REAL(KIND=WP), INTENT(OUT) :: XYZ_OUT(3)
!C----------------------------------------------------------------------
!C   L o c a l   V a r i a b l e s
!C----------------------------------------------------------------------
          INTEGER :: NNODE_TOTAL, K, GID
          INTEGER, PARAMETER :: NBULK = 4
          INTEGER, PARAMETER :: MAX_NNODE_LOCAL = 50
          REAL(KIND=WP) :: NVAL(MAX_NNODE_LOCAL)
          REAL(KIND=WP) :: DN_TMP(MAX_NNODE_LOCAL,3)

          NNODE_TOTAL = NCTRL + NBULK

          CALL Q1NP_SHAPE_FUNCTIONS(XI_PARAM, ETA_PARAM, ONE, &
     &        P_DEGREE, Q_DEGREE, U_KNOT_VEC, V_KNOT_VEC,    &
     &        ELEM_U_IDX, ELEM_V_IDX,                         &
     &        NVAL(1:NNODE_TOTAL), DN_TMP(1:NNODE_TOTAL,1:3))

          XYZ_OUT = ZERO
          DO K = 1, NCTRL
            GID = CTRL_POINT_IDS(K)
            IF (GID <= 0 .OR. GID > NUMNOD) CYCLE
            XYZ_OUT(1:3) = XYZ_OUT(1:3) + NVAL(K) * X_COORDS(1:3, GID)
          END DO

        END SUBROUTINE Q1NP_EVALUATE_NURBS_TOP_SURFACE_POINT

!=======================================================================
!   Q1NP_EVALUATE_NURBS_TOP_SURFACE_POINT_AND_DERIVS
!
!   Evaluate position S(xi,eta) AND first-order tangent vectors
!   dS/dXI, dS/dETA on the NURBS top surface (ZETA = +1).
!   Used by Newton projection to form the orthogonality residual
!   [(S - x_src) . Su,  (S - x_src) . Sv] = 0.
!=======================================================================
        SUBROUTINE Q1NP_EVALUATE_NURBS_TOP_SURFACE_POINT_AND_DERIVS( &
     &      XI_PARAM, ETA_PARAM,       &
     &      P_DEGREE, Q_DEGREE,        &
     &      ELEM_U_IDX, ELEM_V_IDX,   &
     &      U_KNOT_VEC, V_KNOT_VEC,   &
     &      NCTRL,                     &
     &      CTRL_POINT_IDS,            &
     &      X_COORDS,                  &
     &      NUMNOD,                    &
     &      XYZ_OUT, DXYZ_DXI, DXYZ_DETA)
          INTEGER, INTENT(IN) :: P_DEGREE, Q_DEGREE
          INTEGER, INTENT(IN) :: ELEM_U_IDX, ELEM_V_IDX
          INTEGER, INTENT(IN) :: NCTRL, NUMNOD
          INTEGER, INTENT(IN) :: CTRL_POINT_IDS(NCTRL)
          REAL(KIND=WP), INTENT(IN)  :: XI_PARAM, ETA_PARAM
          REAL(KIND=WP), INTENT(IN)  :: U_KNOT_VEC(:), V_KNOT_VEC(:)
          REAL(KIND=WP), INTENT(IN)  :: X_COORDS(3,NUMNOD)
          REAL(KIND=WP), INTENT(OUT) :: XYZ_OUT(3)
          REAL(KIND=WP), INTENT(OUT) :: DXYZ_DXI(3), DXYZ_DETA(3)

          INTEGER :: NNODE_TOTAL, K, GID
          INTEGER, PARAMETER :: NBULK = 4
          INTEGER, PARAMETER :: MAX_NNODE_LOCAL = 50
          REAL(KIND=WP) :: NVAL(MAX_NNODE_LOCAL)
          REAL(KIND=WP) :: DN_TMP(MAX_NNODE_LOCAL,3)

          NNODE_TOTAL = NCTRL + NBULK

          CALL Q1NP_SHAPE_FUNCTIONS(XI_PARAM, ETA_PARAM, ONE, &
     &        P_DEGREE, Q_DEGREE, U_KNOT_VEC, V_KNOT_VEC,    &
     &        ELEM_U_IDX, ELEM_V_IDX,                         &
     &        NVAL(1:NNODE_TOTAL), DN_TMP(1:NNODE_TOTAL,1:3))

          XYZ_OUT   = ZERO
          DXYZ_DXI  = ZERO
          DXYZ_DETA = ZERO
          DO K = 1, NCTRL
            GID = CTRL_POINT_IDS(K)
            IF (GID <= 0 .OR. GID > NUMNOD) CYCLE
            XYZ_OUT(1:3)   = XYZ_OUT(1:3)   + NVAL(K) * X_COORDS(1:3, GID)
            DXYZ_DXI(1:3)  = DXYZ_DXI(1:3)  + DN_TMP(K,1) * X_COORDS(1:3, GID)
            DXYZ_DETA(1:3) = DXYZ_DETA(1:3) + DN_TMP(K,2) * X_COORDS(1:3, GID)
          END DO

        END SUBROUTINE Q1NP_EVALUATE_NURBS_TOP_SURFACE_POINT_AND_DERIVS

!=======================================================================
!   Q1NP_EVALUATE_NURBS_SHAPE_VALUES
!
!=======================================================================
        SUBROUTINE Q1NP_EVALUATE_NURBS_SHAPE_VALUES( &
     &      XI_PARAM, ETA_PARAM,       &
     &      P_DEGREE, Q_DEGREE,        &
     &      ELEM_U_IDX, ELEM_V_IDX,   &
     &      U_KNOT_VEC, V_KNOT_VEC,   &
     &      NCTRL,                     &
     &      NVAL_OUT)
          INTEGER, INTENT(IN) :: P_DEGREE, Q_DEGREE
          INTEGER, INTENT(IN) :: ELEM_U_IDX, ELEM_V_IDX
          INTEGER, INTENT(IN) :: NCTRL
          REAL(KIND=WP), INTENT(IN)  :: XI_PARAM, ETA_PARAM
          REAL(KIND=WP), INTENT(IN)  :: U_KNOT_VEC(:), V_KNOT_VEC(:)
          REAL(KIND=WP), INTENT(OUT) :: NVAL_OUT(NCTRL)

          INTEGER :: NNODE_TOTAL
          INTEGER, PARAMETER :: NBULK = 4
          INTEGER, PARAMETER :: MAX_NNODE_LOCAL = 50
          REAL(KIND=WP) :: NVAL(MAX_NNODE_LOCAL)
          REAL(KIND=WP) :: DN_TMP(MAX_NNODE_LOCAL,3)

          NNODE_TOTAL = NCTRL + NBULK

          CALL Q1NP_SHAPE_FUNCTIONS(XI_PARAM, ETA_PARAM, ONE, &
     &        P_DEGREE, Q_DEGREE, U_KNOT_VEC, V_KNOT_VEC,    &
     &        ELEM_U_IDX, ELEM_V_IDX,                         &
     &        NVAL(1:NNODE_TOTAL), DN_TMP(1:NNODE_TOTAL,1:3))

          NVAL_OUT(1:NCTRL) = NVAL(1:NCTRL)

        END SUBROUTINE Q1NP_EVALUATE_NURBS_SHAPE_VALUES

      END MODULE Q1NP_NURBS_SURFACE_EVALUATION_MOD
