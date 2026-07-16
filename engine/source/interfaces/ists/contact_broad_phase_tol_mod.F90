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
!||    contact_broad_phase_tol_mod   ../engine/source/interfaces/ists/contact_broad_phase_tol_mod.F90
!||--- called by ------------------------------------------------------
!||    sts_broad_phase_voxel_mod      ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||    q1np_contact_algorithms_mod     ../engine/source/interfaces/ists_q1np/q1np_contact_algorithms.F90
!||====================================================================
!
!   Shared broad-phase search tolerance for STS and Q1NP voxel contact.
!
      MODULE CONTACT_BROAD_PHASE_TOL_MOD
        USE PRECISION_MOD, ONLY : WP
        USE CONSTANT_MOD,  ONLY : ZERO, ONEP01
        IMPLICIT NONE
        PRIVATE

        REAL(KIND=WP), PARAMETER, PUBLIC :: INTER_BP_TOL_GAP_FALLBACK = 1.0E-6_WP
        REAL(KIND=WP), PARAMETER, PUBLIC :: INTER_BP_TOL_ALPHA_MESH   = 0.5_WP
        REAL(KIND=WP), PARAMETER, PUBLIC :: INTER_BP_TOL_PAD_FACTOR  = 1.0_WP
        REAL(KIND=WP), PARAMETER, PUBLIC :: INTER_BP_TOL_CELL_FACTOR = 1.0_WP
        REAL(KIND=WP), PARAMETER :: INTER_BP_TOL_HUGE = HUGE(1.0_WP)

        PUBLIC :: INTER_BP_TOL_GAP_PHYS
        PUBLIC :: INTER_BP_TOL_MESH_SCALE
        PUBLIC :: INTER_BP_TOL_MESH_SCALE_SURF
        PUBLIC :: INTER_BP_TOL_MESH_MARGIN
        PUBLIC :: INTER_BP_TOL_TZINF
        PUBLIC :: INTER_BP_TOL_VMAXDT_SURF
        PUBLIC :: INTER_BP_TOL_SEARCH
        PUBLIC :: INTER_BP_TOL_PAD_CELL

      CONTAINS

        REAL(KIND=WP) FUNCTION INTER_BP_TOL_GAP_PHYS(GAP_USER)
          REAL(KIND=WP), INTENT(IN) :: GAP_USER

          INTER_BP_TOL_GAP_PHYS = MAX(INTER_BP_TOL_GAP_FALLBACK, ABS(GAP_USER))
        END FUNCTION INTER_BP_TOL_GAP_PHYS

        REAL(KIND=WP) FUNCTION INTER_BP_TOL_MESH_SCALE(PTS, NPTS)
          INTEGER, INTENT(IN) :: NPTS
          REAL(KIND=WP), INTENT(IN) :: PTS(3, *)

          INTEGER :: I, J, NCOUNT
          REAL(KIND=WP) :: D_MIN, D_SUM, DX, DY, DZ, DIST

          INTER_BP_TOL_MESH_SCALE = INTER_BP_TOL_GAP_FALLBACK

          IF (NPTS < 2) RETURN

          D_SUM = ZERO
          NCOUNT = 0
          DO I = 1, NPTS
            D_MIN = INTER_BP_TOL_HUGE
            DO J = 1, NPTS
              IF (J == I) CYCLE
              DX = PTS(1, I) - PTS(1, J)
              DY = PTS(2, I) - PTS(2, J)
              DZ = PTS(3, I) - PTS(3, J)
              DIST = SQRT(DX*DX + DY*DY + DZ*DZ)
              D_MIN = MIN(D_MIN, DIST)
            END DO
            IF (D_MIN < INTER_BP_TOL_HUGE) THEN
              D_SUM = D_SUM + D_MIN
              NCOUNT = NCOUNT + 1
            END IF
          END DO

          IF (NCOUNT > 0) THEN
            INTER_BP_TOL_MESH_SCALE = D_SUM / REAL(NCOUNT, WP)
          END IF
        END FUNCTION INTER_BP_TOL_MESH_SCALE

!----------------------------------------------------------------------
! O(NSEG) mesh scale from segment corner geometry (STS voxel broad phase).
! Characteristic length per segment = longest quad edge; return the mean.
!----------------------------------------------------------------------
        REAL(KIND=WP) FUNCTION INTER_BP_TOL_MESH_SCALE_SURF( &
     &      IGRSURF, NSURF, SURF_IDX, X, NUMNOD)
          USE GROUPDEF_MOD, ONLY : SURF_
          INTEGER, INTENT(IN) :: NSURF, SURF_IDX, NUMNOD
          TYPE(SURF_), DIMENSION(NSURF), INTENT(IN) :: IGRSURF
          REAL(KIND=WP), INTENT(IN) :: X(3, NUMNOD)

          INTEGER :: ISEG, NSEG, K, N1, N2
          REAL(KIND=WP) :: DX, DY, DZ, EDGE_LEN, H_SEG, H_SUM
          INTEGER :: H_COUNT

          INTER_BP_TOL_MESH_SCALE_SURF = INTER_BP_TOL_GAP_FALLBACK
          IF (SURF_IDX <= 0 .OR. SURF_IDX > NSURF) RETURN
          NSEG = IGRSURF(SURF_IDX)%NSEG
          IF (NSEG <= 0) RETURN
          IF (.NOT. ALLOCATED(IGRSURF(SURF_IDX)%NODES)) RETURN

          H_SUM = ZERO
          H_COUNT = 0
          DO ISEG = 1, NSEG
            H_SEG = ZERO
            DO K = 1, 4
              N1 = IGRSURF(SURF_IDX)%NODES(ISEG, K)
              N2 = IGRSURF(SURF_IDX)%NODES(ISEG, MOD(K, 4) + 1)
              IF (N1 <= 0 .OR. N1 > NUMNOD) CYCLE
              IF (N2 <= 0 .OR. N2 > NUMNOD) CYCLE
              DX = X(1, N2) - X(1, N1)
              DY = X(2, N2) - X(2, N1)
              DZ = X(3, N2) - X(3, N1)
              EDGE_LEN = SQRT(DX*DX + DY*DY + DZ*DZ)
              H_SEG = MAX(H_SEG, EDGE_LEN)
            END DO
            IF (H_SEG > ZERO) THEN
              H_SUM = H_SUM + H_SEG
              H_COUNT = H_COUNT + 1
            END IF
          END DO

          IF (H_COUNT > 0) THEN
            INTER_BP_TOL_MESH_SCALE_SURF = H_SUM / REAL(H_COUNT, WP)
          END IF
        END FUNCTION INTER_BP_TOL_MESH_SCALE_SURF

        REAL(KIND=WP) FUNCTION INTER_BP_TOL_MESH_MARGIN(H_MESH)
          REAL(KIND=WP), INTENT(IN) :: H_MESH

          INTER_BP_TOL_MESH_MARGIN = INTER_BP_TOL_ALPHA_MESH * MAX(H_MESH, INTER_BP_TOL_GAP_FALLBACK)
        END FUNCTION INTER_BP_TOL_MESH_MARGIN

!----------------------------------------------------------------------
! INT25-style influence zone: GAP + mesh margin + VMAXDT.
!----------------------------------------------------------------------
        REAL(KIND=WP) FUNCTION INTER_BP_TOL_TZINF(GAP_USER, H_MESH, VMAXDT)
          REAL(KIND=WP), INTENT(IN) :: GAP_USER, H_MESH, VMAXDT

          INTER_BP_TOL_TZINF = INTER_BP_TOL_GAP_PHYS(GAP_USER) + &
     &      INTER_BP_TOL_MESH_MARGIN(H_MESH) + MAX(ZERO, VMAXDT)
        END FUNCTION INTER_BP_TOL_TZINF

!----------------------------------------------------------------------
! Axis-aligned relative-velocity estimate.
!----------------------------------------------------------------------
        SUBROUTINE INTER_BP_TOL_SURF_VEL_EXTREMA( &
     &      IGRSURF, NSURF, SURF_IDX, V, NUMNOD, VMIN, VMAX)
          USE GROUPDEF_MOD, ONLY : SURF_
          INTEGER, INTENT(IN) :: NSURF, SURF_IDX, NUMNOD
          TYPE(SURF_), DIMENSION(NSURF), INTENT(IN) :: IGRSURF
          REAL(KIND=WP), INTENT(IN) :: V(3, *)
          REAL(KIND=WP), INTENT(OUT) :: VMIN(3), VMAX(3)

          INTEGER :: ISEG, NSEG, K, NID
          LOGICAL :: HAS_NODE

          VMIN = ZERO
          VMAX = ZERO
          HAS_NODE = .FALSE.
          IF (SURF_IDX <= 0 .OR. SURF_IDX > NSURF) RETURN
          NSEG = IGRSURF(SURF_IDX)%NSEG
          IF (NSEG <= 0) RETURN
          IF (.NOT. ALLOCATED(IGRSURF(SURF_IDX)%NODES)) RETURN

          DO ISEG = 1, NSEG
            DO K = 1, 4
              NID = IGRSURF(SURF_IDX)%NODES(ISEG, K)
              IF (NID <= 0 .OR. NID > NUMNOD) CYCLE
              IF (.NOT. HAS_NODE) THEN
                VMIN(1) = V(1, NID)
                VMAX(1) = V(1, NID)
                VMIN(2) = V(2, NID)
                VMAX(2) = V(2, NID)
                VMIN(3) = V(3, NID)
                VMAX(3) = V(3, NID)
                HAS_NODE = .TRUE.
              ELSE
                VMIN(1) = MIN(VMIN(1), V(1, NID))
                VMAX(1) = MAX(VMAX(1), V(1, NID))
                VMIN(2) = MIN(VMIN(2), V(2, NID))
                VMAX(2) = MAX(VMAX(2), V(2, NID))
                VMIN(3) = MIN(VMIN(3), V(3, NID))
                VMAX(3) = MAX(VMAX(3), V(3, NID))
              END IF
            END DO
          END DO
        END SUBROUTINE INTER_BP_TOL_SURF_VEL_EXTREMA

        REAL(KIND=WP) FUNCTION INTER_BP_TOL_VMAXDT_SURF( &
     &      V, IGRSURF, NSURF, SEC_SURF_IDX, MST_SURF_IDX, NUMNOD, DT1)
          USE GROUPDEF_MOD, ONLY : SURF_
          INTEGER, INTENT(IN) :: NSURF, SEC_SURF_IDX, MST_SURF_IDX, NUMNOD
          TYPE(SURF_), DIMENSION(NSURF), INTENT(IN) :: IGRSURF
          REAL(KIND=WP), INTENT(IN) :: V(3, *), DT1

          REAL(KIND=WP) :: VMIN_S(3), VMAX_S(3), VMIN_M(3), VMAX_M(3)
          REAL(KIND=WP) :: VX, VY, VZ, VV

          INTER_BP_TOL_VMAXDT_SURF = ZERO
          IF (DT1 <= ZERO) RETURN

          CALL INTER_BP_TOL_SURF_VEL_EXTREMA( &
     &      IGRSURF, NSURF, SEC_SURF_IDX, V, NUMNOD, VMIN_S, VMAX_S)
          CALL INTER_BP_TOL_SURF_VEL_EXTREMA( &
     &      IGRSURF, NSURF, MST_SURF_IDX, V, NUMNOD, VMIN_M, VMAX_M)

          VX = MAX(VMAX_S(1) - VMIN_M(1), VMAX_M(1) - VMIN_S(1), ZERO)
          VY = MAX(VMAX_S(2) - VMIN_M(2), VMAX_M(2) - VMIN_S(2), ZERO)
          VZ = MAX(VMAX_S(3) - VMIN_M(3), VMAX_M(3) - VMIN_S(3), ZERO)
          VV = SQRT(VX*VX + VY*VY + VZ*VZ)
          INTER_BP_TOL_VMAXDT_SURF = REAL(ONEP01, KIND=WP) * VV * DT1
        END FUNCTION INTER_BP_TOL_VMAXDT_SURF

        SUBROUTINE INTER_BP_TOL_SEARCH(GAP_USER, H_MESH_A, H_MESH_B, TOL_SEARCH)
          REAL(KIND=WP), INTENT(IN) :: GAP_USER, H_MESH_A, H_MESH_B
          REAL(KIND=WP), INTENT(OUT) :: TOL_SEARCH
          REAL(KIND=WP) :: H_MESH

          H_MESH = MAX(H_MESH_A, H_MESH_B, INTER_BP_TOL_GAP_FALLBACK)
          TOL_SEARCH = INTER_BP_TOL_TZINF(GAP_USER, H_MESH, ZERO)
        END SUBROUTINE INTER_BP_TOL_SEARCH

        SUBROUTINE INTER_BP_TOL_PAD_CELL(TOL_SEARCH, SEARCH_PADDING, CELL_SIZE)
          REAL(KIND=WP), INTENT(IN) :: TOL_SEARCH
          REAL(KIND=WP), INTENT(OUT) :: SEARCH_PADDING, CELL_SIZE

          SEARCH_PADDING = INTER_BP_TOL_PAD_FACTOR * TOL_SEARCH
          CELL_SIZE = INTER_BP_TOL_CELL_FACTOR * TOL_SEARCH
        END SUBROUTINE INTER_BP_TOL_PAD_CELL

      END MODULE CONTACT_BROAD_PHASE_TOL_MOD
