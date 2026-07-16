!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    ists_contact_dt_mod     ../engine/source/interfaces/ists/ists_contact_dt_mod.F90
!||--- called by ------------------------------------------------------
!||    sts_contact_eval_pair   ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod            ../common_source/modules/constant_mod.F
!||    precision_mod           ../common_source/modules/precision_mod.F90
!||====================================================================
      MODULE ists_contact_dt_mod

      USE constant_mod
      USE PRECISION_MOD, ONLY : WP
      IMPLICIT NONE

      CONTAINS

!-----------------------------------------------
! Compute the normal velocity between the primary and secondary surfaces
!-----------------------------------------------
!||====================================================================
!||    sts_gp_normal_velocity   ../engine/source/interfaces/ists/ists_contact_dt_mod.F90
!||--- called by ------------------------------------------------------
!||    sts_contact_eval_pair    ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||    sts_gp_update_dt2t       ../engine/source/interfaces/ists/ists_contact_dt_mod.F90
!||====================================================================
      SUBROUTINE sts_gp_normal_velocity(N_xi, N_eta, node_ids, V, numnod, &
     &     norm_contact, v_n)
      IMPLICIT NONE

      REAL*8, INTENT(IN)  :: N_xi(3,4), N_eta(3,4)
      INTEGER, INTENT(IN) :: node_ids(8)
      INTEGER, INTENT(IN) :: numnod
      real(kind=WP), INTENT(IN) :: V(3,numnod)
      REAL*8, INTENT(IN)  :: norm_contact(3)
      REAL*8, INTENT(INOUT) :: v_n

      INTEGER :: j
      REAL*8  :: v_prim(3), v_sec(3), v_rel(3)

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
      END SUBROUTINE sts_gp_normal_velocity

!-----------------------------------------------
! Update the critical timestep limit (DT2T) for a contact GP
!-----------------------------------------------
!||====================================================================
!||    sts_gp_update_dt2t       ../engine/source/interfaces/ists/ists_contact_dt_mod.F90
!||--- called by ------------------------------------------------------
!||    sts_contact_eval_pair    ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||--- calls      -----------------------------------------------------
!||    sts_gp_normal_velocity   ../engine/source/interfaces/ists/ists_contact_dt_mod.F90
!||====================================================================
      SUBROUTINE sts_gp_update_dt2t(node_ids, MS, numnod, d1, N_xi, N_eta, &
     &     area_weight, GAPV, PENE, V, norm_contact, NOINT, &
     &     DT2T, NELTST, ITYPTST, DTFAC1_10)
      IMPLICIT NONE

      INTEGER, INTENT(IN)    :: node_ids(8), NOINT
      INTEGER, INTENT(IN)    :: numnod
      real(kind=WP), INTENT(IN)    :: MS(numnod)
      REAL*8, INTENT(IN)     :: d1, GAPV
      REAL*8, INTENT(IN)     :: N_xi(3,4), N_eta(3,4), area_weight
      REAL*8, INTENT(IN)     :: PENE
      real(kind=WP), INTENT(IN)    :: V(3,numnod)
      REAL*8, INTENT(IN)     :: norm_contact(3)
      real(kind=WP), INTENT(INOUT) :: DT2T
      INTEGER, INTENT(INOUT) :: NELTST, ITYPTST
      real(kind=WP), INTENT(IN)    :: DTFAC1_10

      INTEGER :: j, nid
      REAL*8  :: d1d, k_node, mas2, dt_stif, dt_kin, dt_gp, dist, v_n
      REAL*8  :: shape_w
      REAL*8, PARAMETER :: STS_DT_CLEAR_FRAC = 5.0D-4

      d1d = DBLE(d1)
      IF (d1d <= EM20) RETURN

      dt_gp = 1.d20

      DO j = 1, 4
        shape_w = DABS(N_xi(1,j)) * area_weight
        k_node = d1d * shape_w
        nid = node_ids(j)
        IF (nid > 0) THEN
          mas2 = TWO * DBLE(MS(nid))
          IF (mas2 > ZERO .AND. k_node > EM20) THEN
            dt_stif = DTFAC1_10 * DSQRT(mas2 / k_node)
            dt_gp = MIN(dt_gp, dt_stif)
          ENDIF
        ENDIF
      ENDDO

      DO j = 1, 4
        shape_w = DABS(N_eta(1,j)) * area_weight
        k_node = d1d * shape_w
        nid = node_ids(j+4)
        IF (nid > 0) THEN
          mas2 = TWO * DBLE(MS(nid))
          IF (mas2 > ZERO .AND. k_node > EM20) THEN
            dt_stif = DTFAC1_10 * DSQRT(mas2 / k_node)
            dt_gp = MIN(dt_gp, dt_stif)
          ENDIF
        ENDIF
      ENDDO

      CALL sts_gp_normal_velocity(N_xi, N_eta, node_ids, V, numnod, &
     &     norm_contact, v_n)

!     PENE is the signed gap residual used by STS_CONTACT_EVAL_PAIR:
!       PENE = current_clearance - GAPV
!     The kinematic contact timestep must therefore use the remaining
!     physical clearance, not GAPV - PENE. The old expression grew as
!     penetration increased and delayed the timestep cut exactly when a
!     Lobatto point was approaching zero clearance.
      dist = DBLE(GAPV) + PENE
      dist = MAX(EM10, dist)
      IF (DABS(DBLE(GAPV)) > EM10) THEN
        dist = MAX(dist, DABS(DBLE(GAPV)) * STS_DT_CLEAR_FRAC)
      ENDIF
      IF (v_n < ZERO) THEN
        dt_kin = HALF * dist / MAX(EM30, -v_n)
        dt_gp = MIN(dt_gp, dt_kin)
      ENDIF

      IF (dt_gp < DBLE(DT2T)) THEN
        DT2T = dt_gp
        NELTST = NOINT
        ITYPTST = 10
      ENDIF
      END SUBROUTINE sts_gp_update_dt2t

      END MODULE ists_contact_dt_mod
