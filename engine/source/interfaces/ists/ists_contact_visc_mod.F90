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
!||    ists_contact_visc_mod   ../engine/source/interfaces/ists/ists_contact_visc_mod.F90
!||--- called by ------------------------------------------------------
!||    sts_contact_eval_pair   ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod            ../common_source/modules/constant_mod.F
!||    precision_mod           ../common_source/modules/precision_mod.F90
!||====================================================================
      MODULE ists_contact_visc_mod

      USE constant_mod
      USE PRECISION_MOD, ONLY : WP
      IMPLICIT NONE

      CONTAINS

!||====================================================================
!||    sts_gp_secondary_mass   ../engine/source/interfaces/ists/ists_contact_visc_mod.F90
!||--- called by ------------------------------------------------------
!||    sts_gp_ivis2_normal     ../engine/source/interfaces/ists/ists_contact_visc_mod.F90
!||====================================================================
      SUBROUTINE sts_gp_secondary_mass(N_eta, node_ids, MS, numnod, msi_gp)
      IMPLICIT NONE

      REAL*8, INTENT(IN)     :: N_eta(3,4)
      INTEGER, INTENT(IN)    :: node_ids(8)
      INTEGER, INTENT(IN)    :: numnod
      real(kind=WP), INTENT(IN)    :: MS(numnod)
      REAL*8, INTENT(INOUT)    :: msi_gp

      INTEGER :: j, nid

      msi_gp = EM10
      DO j = 1, 4
        nid = node_ids(j+4)
        IF (nid > 0) THEN
          msi_gp = msi_gp + N_eta(1,j)*N_eta(1,j) * DBLE(MS(nid))
        ENDIF
      ENDDO
      END SUBROUTINE sts_gp_secondary_mass

!   IVIS2=0/1 normal viscous damping at a contact Gauss point.
!   d1_in  : penalty stiffness before IVIS2 (0.5*STIF*FAC).
!   d1_out : stiffness for STIFN assembly (post IVIS2 boost).
!   f_visc : scalar normal viscous force (add to d1*penetr).
!||====================================================================
!||    sts_gp_ivis2_normal     ../engine/source/interfaces/ists/ists_contact_visc_mod.F90
!||--- called by ------------------------------------------------------
!||    sts_contact_eval_pair   ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||--- calls      -----------------------------------------------------
!||    sts_gp_secondary_mass   ../engine/source/interfaces/ists/ists_contact_visc_mod.F90
!||====================================================================
      SUBROUTINE sts_gp_ivis2_normal(d1_in, GAPV, PENE, v_n, N_eta, &
     &     node_ids, MS, numnod, VISC, IVIS2, VISCFFRIC, DT1, &
     &     d1_out, f_visc)
      IMPLICIT NONE

      REAL*8, INTENT(IN)    :: d1_in, PENE, v_n
      REAL*8, INTENT(IN)    :: GAPV
      real(kind=WP), INTENT(IN)   :: VISC, VISCFFRIC, DT1
      INTEGER, INTENT(IN)   :: IVIS2
      REAL*8, INTENT(IN)    :: N_eta(3,4)
      INTEGER, INTENT(IN)   :: node_ids(8)
      INTEGER, INTENT(IN)   :: numnod
      real(kind=WP), INTENT(IN)   :: MS(numnod)
      REAL*8, INTENT(INOUT)   :: d1_out, f_visc

      REAL*8 :: gap_dist, vis2, vis, visca, fac_stif, c_damp, dt1inv
      REAL*8 :: msi_gp, gapvd

      d1_out = d1_in
      f_visc = 0.d0

      IF (VISC <= EM20) RETURN
      IF (.NOT.(IVIS2 == 0 .OR. IVIS2 == 1)) RETURN

      CALL sts_gp_secondary_mass(N_eta, node_ids, MS, numnod, msi_gp)

      gap_dist = MAX(EM10, DBLE(GAPV) + PENE)
      gapvd = DABS(DBLE(GAPV))

      vis2 = TWO * d1_in * msi_gp
      IF (v_n < ZERO .AND. gapvd > EM10) THEN
        vis2 = vis2 / MAX(EM10, gap_dist / gapvd)
      ENDIF

      visca = ZEP4
      fac_stif = d1_in / MAX(EM30, d1_in)
      vis = DSQRT(MAX(EM30, vis2))

      c_damp = fac_stif * (DBLE(VISC) * vis &
     &       + visca*visca * TWO * msi_gp * MAX(ZERO, -v_n) / gap_dist)

      IF (DT1 > EM30) THEN
        dt1inv = ONE / DBLE(DT1)
      ELSE
        dt1inv = 0.d0
      ENDIF

      IF (gapvd > EM10) THEN
        d1_out = d1_in * DBLE(GAPV) / gap_dist
      ELSE
        d1_out = d1_in
      ENDIF
      d1_out = d1_out + c_damp * dt1inv
      d1_out = MAX(d1_out, fac_stif*DSQRT(MAX(ZERO, DBLE(VISCFFRIC))) &
     &         * vis * dt1inv)

      f_visc = c_damp * v_n
      END SUBROUTINE sts_gp_ivis2_normal

      END MODULE ists_contact_visc_mod
