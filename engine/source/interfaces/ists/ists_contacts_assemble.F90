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
!||    STS_CONTACTS_ASSEMBLE  ../engine/source/interfaces/ists/ists_contacts_assemble.F90
!||--- called by ------------------------------------------------------
!||    i7mainf              ../engine/source/interfaces/int07/i7mainf.F
!||--- calls ---------------------------------------------------------
!||    STS_CONTACT_EVAL_PAIR    ../engine/source/interfaces/ists/ists_CONTACT_EVAL_PAIR.F90
!||====================================================================
!
!   Evaluate STS candidate pairs for one quadrature mode, accumulate
!   pair loads, and return force/energy totals for /TH/INTER output.
!
!||====================================================================
!||    sts_contacts_assemble           ../engine/source/interfaces/ists/ists_contacts_assemble.F90
!||--- called by ------------------------------------------------------
!||    ists_mainf                      ../engine/source/interfaces/ists/ists_mainf.F90
!||--- calls      -----------------------------------------------------
!||    sts_build_lobatto_gp_weights    ../engine/source/interfaces/ists/ists_contacts_assemble.F90
!||    sts_contact_eval_pair           ../engine/source/interfaces/ists/ists_contact_eval_pair.F90
!||    sts_contact_pair_aabb_skip      ../engine/source/interfaces/ists/ists_contacts_assemble.F90
!||    sts_pair_activity_should_skip   ../engine/source/interfaces/ists/ists_sts_bp_algo_mod.F90
!||    sts_pair_activity_update        ../engine/source/interfaces/ists/ists_sts_bp_algo_mod.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                    ../common_source/modules/constant_mod.F
!||    ists_sts_pair_activity_mod      ../engine/source/interfaces/ists/ists_sts_bp_algo_mod.F90
!||    my_alloc_mod                    ../common_source/tools/memory/my_alloc.F90
!||    my_dealloc_mod                  ../common_source/tools/memory/my_dealloc.F90
!||    precision_mod                   ../common_source/modules/precision_mod.F90
!||    sts_gp_state_mod                ../engine/source/interfaces/ists/ists_gp_state_mod.F90
!||====================================================================
      SUBROUTINE STS_CONTACTS_ASSEMBLE(CONT_ELEMENT, COUNT, OPTION, STS_INTERFACE_ID, NCYCLE_IN, &
     & CAND_MST_SEG_ID, CAND_SEC_SEG_ID, CAND_SEC_GP_MASK, &
     & load_arr, node_id_load, L_out, IMPACT_glob, STIF, &
     & MAX_STS_SIZE_ACTUAL, FRICC, XMU, IFPEN, GAP, V, MS, NUMNOD, &
     & VISC, IVIS2, VISCFFRIC, DT2T, NELTST, ITYPTST, &
     & ECONTT_TOT, ECONVT_TOT, FN_TOT, FT_TOT, DT1, DTFAC1_10)
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use constant_mod
      use sts_gp_state_mod
      use ists_sts_pair_activity_mod, only : &
     &  STS_PAIR_ACTIVITY_SHOULD_SKIP, STS_PAIR_ACTIVITY_UPDATE
      use my_alloc_mod, only : my_alloc
      use my_dealloc_mod, only : my_dealloc
      use precision_mod, only : WP
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
#include      "mvsiz_p.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      REAL*8, INTENT(IN)    :: CONT_ELEMENT(MAX_STS_SIZE_ACTUAL,3,8)
      real(kind=WP), INTENT(IN)    :: STIF(MAX_STS_SIZE_ACTUAL)
      INTEGER, INTENT(IN)    :: COUNT, OPTION, STS_INTERFACE_ID, NCYCLE_IN
      INTEGER, INTENT(IN)    :: CAND_SEC_SEG_ID(MAX_STS_SIZE_ACTUAL,5)
      INTEGER, INTENT(IN)    :: CAND_MST_SEG_ID(MAX_STS_SIZE_ACTUAL,5)
      INTEGER, INTENT(IN)    :: CAND_SEC_GP_MASK(MAX_STS_SIZE_ACTUAL,4)
      REAL*8, INTENT(INOUT) :: load_arr(MAX_STS_SIZE_ACTUAL,8,4)
      INTEGER, INTENT(INOUT) :: node_id_load(MAX_STS_SIZE_ACTUAL*8)
      INTEGER, INTENT(INOUT) :: L_out, IMPACT_glob
      INTEGER, INTENT(IN)    :: MAX_STS_SIZE_ACTUAL
      real(kind=WP), INTENT(IN)    :: FRICC(MVSIZ)
      real(kind=WP), INTENT(INOUT) :: XMU(MVSIZ)
      INTEGER, INTENT(INOUT) :: IFPEN(MAX_STS_SIZE_ACTUAL)
      real(kind=WP), INTENT(IN)    :: GAP  ! Gap value from user input
      INTEGER, INTENT(IN)    :: NUMNOD
      real(kind=WP), INTENT(IN)    :: V(3,numnod), MS(numnod)
      real(kind=WP), INTENT(IN)    :: VISC, VISCFFRIC(MVSIZ)
      real(kind=WP), INTENT(INOUT) :: DT2T
      INTEGER, INTENT(IN)    :: IVIS2
      INTEGER, INTENT(INOUT) :: NELTST, ITYPTST
      REAL*8, INTENT(INOUT) :: ECONTT_TOT, ECONVT_TOT
      REAL*8, INTENT(INOUT) :: FN_TOT(3), FT_TOT(3)
      real(kind=WP), INTENT(IN)    :: DT1, DTFAC1_10
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER I, J, K, L, IMPACT
      INTEGER selected_option, impact_gauss, impact_lobatto
      INTEGER neltst_probe, ityptst_probe
      INTEGER valid_gauss, valid_lobatto
      REAL*8 XUPD(3,8)
      REAL*8 p_load_new(24)
      REAL*8 p_probe(24)
      REAL*8 node_stiff(8)
      REAL*8 node_stiff_probe(8)
      REAL*8 unit_gp_weight(4)
      REAL*8 p_friction(24)  ! Friction forces (separate output)
      REAL*8 p_friction_probe(24)
      REAL*8 pair_max_penetration
      REAL*8 probe_pen_gauss, probe_pen_lobatto
      REAL*8 probe_score_gauss, probe_score_lobatto
      REAL*8 min_pene_gauss, min_pene_lobatto
      REAL*8 econt_pair, econtv_pair
      REAL*8 econt_probe, econtv_probe
      real(kind=WP) DT2T_PROBE
      INTEGER node_ids(8)  ! Node IDs for velocity interpolation
      REAL*8 gap_abs, lobatto_margin
      REAL*8, ALLOCATABLE, SAVE :: lobatto_gp_weight(:,:)
      LOGICAL pair_activity_skip, pair_aabb_skip
      LOGICAL STS_CONTACT_PAIR_AABB_SKIP
      REAL*8, PARAMETER :: STS_MIXED_LOBATTO_GAP_MARGIN = 2.0D-2
      REAL*8, PARAMETER :: STS_MIXED_LOBATTO_REL_MARGIN = 2.0D-1
!-----------------------------------------------
!   I n i t i a l i z a t i o n
!-----------------------------------------------
      IMPACT_glob = 0
      ECONTT_TOT = 0.0D0
      ECONVT_TOT = 0.0D0
      FN_TOT = 0.0D0
      FT_TOT = 0.0D0
      
!     No candidate pairs are available for this pass.
      IF (COUNT <= 0) THEN
        L_out = 1
        RETURN
      END IF
      
      ! Initialize counters
      K = 1
      L = 1
      unit_gp_weight = 1.0D0

      IF (.NOT. ALLOCATED(lobatto_gp_weight) .OR. &
     &    SIZE(lobatto_gp_weight, 2) < COUNT) THEN
        IF (ALLOCATED(lobatto_gp_weight)) CALL MY_DEALLOC(lobatto_gp_weight)
        CALL MY_ALLOC(lobatto_gp_weight, 4, COUNT, "LOBATTO_GP_WEIGHT")
      ENDIF
      lobatto_gp_weight(1:4, 1:COUNT) = 1.0D0
      IF (OPTION == 1 .OR. OPTION == 2) THEN
        lobatto_gp_weight = 0.0D0
        CALL STS_BUILD_LOBATTO_GP_WEIGHTS(COUNT, MAX_STS_SIZE_ACTUAL, &
     &    CAND_SEC_SEG_ID, CAND_MST_SEG_ID, CAND_SEC_GP_MASK, &
     &    lobatto_gp_weight)
      ENDIF

!-----------------------------------------------
!   M a i n   L o o p
!-----------------------------------------------
      DO I = 1, COUNT
        IMPACT = 0
        XUPD = CONT_ELEMENT(I, 1:3, 1:8)

        pair_aabb_skip = STS_CONTACT_PAIR_AABB_SKIP(XUPD, GAP)
        IF (pair_aabb_skip) THEN
          CYCLE
        ENDIF
      
        ! Get node IDs for velocity interpolation
        DO J = 1, 4
          node_ids(J)   = CAND_MST_SEG_ID(I, J+1)   ! Primary nodes
          node_ids(J+4) = CAND_SEC_SEG_ID(I, J+1)   ! Secondary nodes
        ENDDO
        XMU(1) = FRICC(MIN(I,MVSIZ)) ! Friction coefficient mu
      
        selected_option = OPTION
        impact_gauss = 0
        impact_lobatto = 0
        probe_pen_gauss = 0.0D0
        probe_pen_lobatto = 0.0D0
        probe_score_gauss = 0.0D0
        probe_score_lobatto = 0.0D0
        valid_gauss = 0
        valid_lobatto = 0
        min_pene_gauss = 0.0D0
        min_pene_lobatto = 0.0D0

        IF (OPTION == 2 .OR. OPTION == 0) THEN
          DT2T_PROBE = DT2T
          NELTST_PROBE = NELTST
          ITYPTST_PROBE = ITYPTST

          CALL STS_CONTACT_EVAL_PAIR(XUPD, STIF(I), p_probe, &
     &                      impact_gauss, I, node_stiff_probe, 0, &
     &                      FRICC, XMU, IFPEN, &
     &                      p_friction_probe, &
     &                      node_ids, V, numnod, .FALSE., MAX_STS_SIZE_ACTUAL, &
     &                      GAP, unit_gp_weight, probe_pen_gauss, &
     &                      econt_probe, &
     &                      econtv_probe, MS, STS_INTERFACE_ID, VISC, &
     &                      IVIS2, VISCFFRIC(MIN(I,MVSIZ)), DT2T_PROBE, &
     &                      NELTST_PROBE, ITYPTST_PROBE, &
     &                      .FALSE., probe_score_gauss, valid_gauss, &
     &                      min_pene_gauss, DT1, DTFAC1_10)
          DT2T_PROBE = DT2T
          NELTST_PROBE = NELTST
          ITYPTST_PROBE = ITYPTST

          CALL STS_CONTACT_EVAL_PAIR(XUPD, STIF(I), p_probe, &
     &                      impact_lobatto, I, node_stiff_probe, 1, &
     &                      FRICC, XMU, IFPEN, &
     &                      p_friction_probe, &
     &                      node_ids, V, numnod, .FALSE., MAX_STS_SIZE_ACTUAL, &
     &                      GAP, lobatto_gp_weight(1:4,I), &
     &                      probe_pen_lobatto, &
     &                      econt_probe, &
     &                      econtv_probe, MS, STS_INTERFACE_ID, VISC, &
     &                      IVIS2, VISCFFRIC(MIN(I,MVSIZ)), DT2T_PROBE, &
     &                      NELTST_PROBE, ITYPTST_PROBE, &
     &                      .FALSE., probe_score_lobatto, &
     &                      valid_lobatto, min_pene_lobatto, DT1, DTFAC1_10)
        ENDIF

        IF (OPTION == 2) THEN
          IF (impact_gauss == 0 .AND. impact_lobatto == 0) THEN
            selected_option = -1
          ELSE IF (impact_lobatto == 1 .AND. impact_gauss == 0) THEN
            selected_option = 1
          ELSE IF (impact_gauss == 1 .AND. impact_lobatto == 0) THEN
            gap_abs = MAX(DABS(DBLE(GAP)), 1.0D-30)
            lobatto_margin = MAX(STS_MIXED_LOBATTO_GAP_MARGIN*gap_abs, &
     &        STS_MIXED_LOBATTO_REL_MARGIN*probe_pen_gauss)
            IF (valid_lobatto <= 0) THEN
              selected_option = -1
            ELSE IF (min_pene_lobatto > lobatto_margin) THEN
              selected_option = -1
            ELSE
              selected_option = 0
            ENDIF
          ELSE
            gap_abs = MAX(DABS(DBLE(GAP)), 1.0D-30)
            lobatto_margin = MAX(STS_MIXED_LOBATTO_GAP_MARGIN*gap_abs, &
     &        STS_MIXED_LOBATTO_REL_MARGIN*MAX(probe_pen_gauss, &
     &        probe_pen_lobatto))
            IF (probe_pen_lobatto > probe_pen_gauss + &
     &          lobatto_margin) THEN
              selected_option = 1
            ELSE IF (probe_pen_lobatto > probe_pen_gauss .AND. &
     &          probe_score_lobatto > &
     &          (1.0D0 + STS_MIXED_LOBATTO_REL_MARGIN) * &
     &          probe_score_gauss) THEN
              selected_option = 1
            ELSE
              selected_option = 0
            ENDIF
          ENDIF
        ELSE IF (OPTION == 0) THEN
          IF (impact_gauss == 1 .AND. impact_lobatto == 0) THEN
            gap_abs = MAX(DABS(DBLE(GAP)), 1.0D-30)
            lobatto_margin = MAX(STS_MIXED_LOBATTO_GAP_MARGIN*gap_abs, &
     &        STS_MIXED_LOBATTO_REL_MARGIN*probe_pen_gauss)
            IF (valid_lobatto <= 0) THEN
              selected_option = -1
            ELSE IF (min_pene_lobatto > lobatto_margin) THEN
              selected_option = -1
            ENDIF
          ENDIF
        ENDIF

        IF (selected_option < 0) THEN
          CYCLE
        ENDIF

        CALL STS_PAIR_ACTIVITY_SHOULD_SKIP(NCYCLE_IN, &
     &    STS_INTERFACE_ID, CAND_SEC_SEG_ID(I,1), &
     &    CAND_MST_SEG_ID(I,1), selected_option, pair_activity_skip)
        IF (pair_activity_skip) THEN
          CYCLE
        ENDIF

        ! Commit exactly one quadrature. Probe calls above are side-effect free.
        ! Normal penalty: d1 = 0.5*STIF*FAC; friction trial: d1_fric = 0.5*STIF (NTS STIF0).
        CALL STS_CONTACT_EVAL_PAIR(XUPD, STIF(I), p_load_new, IMPACT, I, &
     &                    node_stiff, selected_option, &
     &                    FRICC, XMU, IFPEN, &
     &                    p_friction, node_ids, V, numnod, &
     &                    .TRUE., MAX_STS_SIZE_ACTUAL, GAP, &
     &                    lobatto_gp_weight(1:4,I), &
     &                    pair_max_penetration, econt_pair, econtv_pair, &
     &                    MS, STS_INTERFACE_ID, VISC, IVIS2, &
     &                    VISCFFRIC(MIN(I,MVSIZ)), DT2T, NELTST, ITYPTST, &
     &                    .TRUE., probe_score_gauss, valid_gauss, &
     &                    min_pene_gauss, DT1, DTFAC1_10)
        CALL STS_PAIR_ACTIVITY_UPDATE(NCYCLE_IN, STS_INTERFACE_ID, &
     &    CAND_SEC_SEG_ID(I,1), CAND_MST_SEG_ID(I,1), selected_option, &
     &    IMPACT, valid_gauss, min_pene_gauss, DBLE(GAP))
      
        IF (IMPACT == 1) THEN
          IMPACT_glob = 1
          ECONTT_TOT = ECONTT_TOT + econt_pair
          ECONVT_TOT = ECONVT_TOT + econtv_pair

!         Slave-side force resultants for /TH/INTER (normal = p - p_friction).
          DO J = 5, 8
            FN_TOT(1) = FN_TOT(1) + p_load_new(3*(J-1)+1) - p_friction(3*(J-1)+1)
            FN_TOT(2) = FN_TOT(2) + p_load_new(3*(J-1)+2) - p_friction(3*(J-1)+2)
            FN_TOT(3) = FN_TOT(3) + p_load_new(3*(J-1)+3) - p_friction(3*(J-1)+3)
            FT_TOT(1) = FT_TOT(1) + p_friction(3*(J-1)+1)
            FT_TOT(2) = FT_TOT(2) + p_friction(3*(J-1)+2)
            FT_TOT(3) = FT_TOT(3) + p_friction(3*(J-1)+3)
          ENDDO

          IF (L > MAX_STS_SIZE_ACTUAL .OR. &
     &        K + 7 > MAX_STS_SIZE_ACTUAL*8) THEN
            EXIT
          END IF

          ! Save node IDs: Primary (1-4), Secondary (5-8)
          node_id_load(K:K+3) = CAND_MST_SEG_ID(I, 2:5)
          node_id_load(K+4:K+7) = CAND_SEC_SEG_ID(I, 2:5)
          K = K + 8
      
          ! Store forces: Primary (1-4), Secondary (5-8)
          DO J = 1, 4
            ! Primary forces
            load_arr(L, J, 1:3) = p_load_new(3*(J-1) + 1 : 3*J)
            ! Secondary forces
            load_arr(L, J + 4, 1:3) = p_load_new(12 + 3*(J-1) + 1 : 12 + 3*J)
          ENDDO
      
          ! Store stiffness info for all 8 nodes
          load_arr(L, 1:8, 4) = node_stiff(1:8)
      
          L = L + 1
          
!         Stop after filling the last available pair-load slot.
          IF (L > MAX_STS_SIZE_ACTUAL .OR. K > MAX_STS_SIZE_ACTUAL*8) THEN
            EXIT
          END IF
        ENDIF
      ENDDO

      L_out = L
      END SUBROUTINE STS_CONTACTS_ASSEMBLE

!=======================================================================
!   STS_CONTACT_PAIR_AABB_SKIP
!
!   Skip segment pairs (in the narrow phase) whose primary and secondary axis-aligned boxes are
!   separated by more than a conservative gap-scaled padding.
!=======================================================================
!||====================================================================
!||    sts_contact_pair_aabb_skip   ../engine/source/interfaces/ists/ists_contacts_assemble.F90
!||--- called by ------------------------------------------------------
!||    sts_contacts_assemble        ../engine/source/interfaces/ists/ists_contacts_assemble.F90
!||--- uses       -----------------------------------------------------
!||    precision_mod                ../common_source/modules/precision_mod.F90
!||====================================================================
      LOGICAL FUNCTION STS_CONTACT_PAIR_AABB_SKIP(XPAIR, GAP_IN)
      USE PRECISION_MOD, ONLY : WP
      IMPLICIT NONE
      REAL*8, INTENT(IN) :: XPAIR(3,8)
      real(kind=WP), INTENT(IN) :: GAP_IN
      REAL*8, PARAMETER :: STS_AABB_SKIP_GAP_FACTOR = 1.5D0
      REAL*8, PARAMETER :: STS_AABB_SKIP_ABS = 1.0D-12
      REAL*8 :: MST_MIN(3), MST_MAX(3), SEC_MIN(3), SEC_MAX(3)
      REAL*8 :: DELTA, SEP2, PAD
      INTEGER :: A, J

      STS_CONTACT_PAIR_AABB_SKIP = .FALSE.

      DO A = 1, 3
        MST_MIN(A) = XPAIR(A,1)
        MST_MAX(A) = XPAIR(A,1)
        SEC_MIN(A) = XPAIR(A,5)
        SEC_MAX(A) = XPAIR(A,5)
      ENDDO

      DO J = 2, 4
        DO A = 1, 3
          MST_MIN(A) = MIN(MST_MIN(A), XPAIR(A,J))
          MST_MAX(A) = MAX(MST_MAX(A), XPAIR(A,J))
        ENDDO
      ENDDO

      DO J = 6, 8
        DO A = 1, 3
          SEC_MIN(A) = MIN(SEC_MIN(A), XPAIR(A,J))
          SEC_MAX(A) = MAX(SEC_MAX(A), XPAIR(A,J))
        ENDDO
      ENDDO

      SEP2 = 0.0D0
      DO A = 1, 3
        IF (MST_MAX(A) < SEC_MIN(A)) THEN
          DELTA = SEC_MIN(A) - MST_MAX(A)
        ELSE IF (SEC_MAX(A) < MST_MIN(A)) THEN
          DELTA = MST_MIN(A) - SEC_MAX(A)
        ELSE
          DELTA = 0.0D0
        ENDIF
        SEP2 = SEP2 + DELTA * DELTA
      ENDDO

      PAD = MAX(DABS(DBLE(GAP_IN)) * STS_AABB_SKIP_GAP_FACTOR, &
     &  STS_AABB_SKIP_ABS)
      STS_CONTACT_PAIR_AABB_SKIP = SEP2 > PAD * PAD
      END FUNCTION STS_CONTACT_PAIR_AABB_SKIP

!=======================================================================
!   STS_BUILD_LOBATTO_GP_WEIGHTS
!
!   Split Lobatto corner weight over duplicate secondary-segment/master-
!   patch pairs so expanded patches do not multiply the same corner load.
!=======================================================================
!||====================================================================
!||    sts_build_lobatto_gp_weights   ../engine/source/interfaces/ists/ists_contacts_assemble.F90
!||--- called by ------------------------------------------------------
!||    sts_contacts_assemble          ../engine/source/interfaces/ists/ists_contacts_assemble.F90
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    my_alloc_mod                   ../common_source/tools/memory/my_alloc.F90
!||    my_dealloc_mod                 ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
      SUBROUTINE STS_BUILD_LOBATTO_GP_WEIGHTS(COUNT_IN, CAPACITY, &
     & CAND_SEC_SEG_ID, CAND_MST_SEG_ID, CAND_SEC_GP_MASK, WEIGHT)
      use my_alloc_mod, only : my_alloc
      use my_dealloc_mod, only : my_dealloc
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: COUNT_IN, CAPACITY
      INTEGER, INTENT(IN) :: CAND_SEC_SEG_ID(CAPACITY,5)
      INTEGER, INTENT(IN) :: CAND_MST_SEG_ID(CAPACITY,5)
      INTEGER, INTENT(IN) :: CAND_SEC_GP_MASK(CAPACITY,4)
      REAL*8, INTENT(INOUT) :: WEIGHT(4, COUNT_IN)
      INTEGER :: HASH_SIZE, I, C, SEC_SEG, MST_SEG
      INTEGER :: IDX, PROBE, CNT
      INTEGER, ALLOCATABLE :: HASH_SEC_SEG(:), HASH_CORNER(:)
      INTEGER, ALLOCATABLE :: HASH_COUNT(:)
      INTEGER(KIND=8) :: HKEY

      IF (COUNT_IN <= 0) RETURN
      HASH_SIZE = MAX(17, 8*COUNT_IN + 1)
      CALL MY_ALLOC(HASH_SEC_SEG, HASH_SIZE, "HASH_SEC_SEG")
      CALL MY_ALLOC(HASH_CORNER, HASH_SIZE, "HASH_CORNER")
      CALL MY_ALLOC(HASH_COUNT, HASH_SIZE, "HASH_COUNT")
      HASH_SEC_SEG = 0
      HASH_CORNER = 0
      HASH_COUNT = 0

!     A Lobatto corner belongs to one secondary segment. INT7 remapping
!     can expand that same corner to several master patch segments. Keep
!     full 2x2 secondary coverage, but split the corner weight only over
!     that segment's local master-patch candidates. Do not split globally
!     by node, because adjacent secondary segments legitimately share
!     physical nodes and each segment still owns its own integrated area.
      DO I = 1, COUNT_IN
        SEC_SEG = CAND_SEC_SEG_ID(I,1)
        MST_SEG = CAND_MST_SEG_ID(I,1)
        IF (SEC_SEG <= 0 .OR. MST_SEG <= 0) CYCLE
        DO C = 1, 4
          IF (CAND_SEC_GP_MASK(I,C) == 0) CYCLE
          HKEY = INT(SEC_SEG, KIND=8) * INT(8, KIND=8) + &
     &      INT(C, KIND=8)
          IDX = INT(MOD(HKEY, INT(HASH_SIZE, KIND=8))) + 1
          DO PROBE = 1, HASH_SIZE
            IF (HASH_COUNT(IDX) == 0) THEN
              HASH_SEC_SEG(IDX) = SEC_SEG
              HASH_CORNER(IDX) = C
              EXIT
            ELSE IF (HASH_SEC_SEG(IDX) == SEC_SEG .AND. &
     &               HASH_CORNER(IDX) == C) THEN
              EXIT
            ENDIF
            IDX = IDX + 1
            IF (IDX > HASH_SIZE) IDX = 1
          ENDDO
          HASH_COUNT(IDX) = HASH_COUNT(IDX) + 1
        ENDDO
      ENDDO

      DO I = 1, COUNT_IN
        SEC_SEG = CAND_SEC_SEG_ID(I,1)
        MST_SEG = CAND_MST_SEG_ID(I,1)
        IF (SEC_SEG <= 0 .OR. MST_SEG <= 0) CYCLE
        DO C = 1, 4
          IF (CAND_SEC_GP_MASK(I,C) == 0) CYCLE
          HKEY = INT(SEC_SEG, KIND=8) * INT(8, KIND=8) + &
     &      INT(C, KIND=8)
          IDX = INT(MOD(HKEY, INT(HASH_SIZE, KIND=8))) + 1
          CNT = 1
          DO PROBE = 1, HASH_SIZE
            IF (HASH_COUNT(IDX) == 0) EXIT
            IF (HASH_SEC_SEG(IDX) == SEC_SEG .AND. &
     &          HASH_CORNER(IDX) == C) THEN
              CNT = HASH_COUNT(IDX)
              EXIT
            ENDIF
            IDX = IDX + 1
            IF (IDX > HASH_SIZE) IDX = 1
          ENDDO
          WEIGHT(C,I) = 1.0D0 / DBLE(CNT)
        ENDDO
      ENDDO

      CALL MY_DEALLOC(HASH_SEC_SEG)
      CALL MY_DEALLOC(HASH_CORNER)
      CALL MY_DEALLOC(HASH_COUNT)
      END SUBROUTINE STS_BUILD_LOBATTO_GP_WEIGHTS
