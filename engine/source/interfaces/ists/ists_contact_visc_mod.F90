!||====================================================================
!||    ists_contact_visc_mod  ../engine/source/interfaces/ists/ists_contact_visc_mod.F90
!||--------------------------------------------------------------------
!||  STS normal viscous damping (IVIS2 analog of NTS i7for3).
!||====================================================================
      MODULE ists_contact_visc_mod

      USE constant_mod
      USE PRECISION_MOD, ONLY : WP
      IMPLICIT NONE

      CONTAINS

!||====================================================================
!||    sts_gp_secondary_mass  ../engine/source/interfaces/ists/ists_contact_visc_mod.F90
!||--- called by ------------------------------------------------------
!||    sts_gp_ivis2_normal      ../engine/source/interfaces/ists/ists_contact_visc_mod.F90
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

!||====================================================================
!||    sts_gp_ivis2_normal  ../engine/source/interfaces/ists/ists_contact_visc_mod.F90
!||--- called by ------------------------------------------------------
!||    STS_CONTACT_EVAL_PAIR   ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||====================================================================
!   IVIS2=0/1 normal viscous damping at a contact Gauss point.
!   d1_in  : penalty stiffness before IVIS2 (0.5*STIF*FAC).
!   d1_out : stiffness for STIFN assembly (post IVIS2 boost).
!   f_visc : scalar normal viscous force (add to d1*penetr).
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
