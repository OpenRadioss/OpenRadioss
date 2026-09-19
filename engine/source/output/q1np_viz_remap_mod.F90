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
!||    q1np_viz_remap_mod   ../engine/source/output/q1np_viz_remap_mod.F90
!||--- called by ------------------------------------------------------
!||    genani               ../engine/source/output/anim/generate/genani.F
!||    genh3d               ../engine/source/output/h3d/h3d_results/genh3d.F
!||--- uses       -----------------------------------------------------
!||    my_alloc_mod         ../common_source/tools/memory/my_alloc.F90
!||    my_dealloc_mod       ../common_source/tools/memory/my_dealloc.F90
!||    precision_mod        ../common_source/modules/precision_mod.F
!||    q1np_nurbs_surface_evaluation_mod
!||                          ../engine/source/elements/solid/solid_q1np/q1np_nurbs_surface_eval_mod.F90
!||    q1np_restart_mod     ../common_source/modules/q1np_restart_mod.F90
!||====================================================================
!
!   Shared visualization remap for legacy Q1NP HEX nodes (H3D + ANIM).
!   Legacy face nodes follow the evaluated Q1NP span corners so post
!   processors show the NURBS surface instead of the undeformed HEX face.
!
!   SMP-only for now (NSPMD == 1), matching the previous H3D implementation.
!
      MODULE Q1NP_VIZ_REMAP_MOD
        USE PRECISION_MOD, ONLY : WP
        USE MY_ALLOC_MOD
        USE MY_DEALLOC_MOD, ONLY : MY_DEALLOC
        USE Q1NP_RESTART_MOD, ONLY : NUMELQ1NP_G, Q1NP_NKNOT_SETS_G,       &
     &      Q1NP_NX_G, Q1NP_NY_G, Q1NP_NX_SET_G, Q1NP_NY_SET_G,            &
     &      Q1NP_KTAB_OFF_G, IQ1NP_TAB, KQ1NP_TAB, Q1NP_KTAB
        USE Q1NP_NURBS_SURFACE_EVALUATION_MOD, ONLY :                      &
     &      Q1NP_EVALUATE_NURBS_SHAPE_VALUES
        IMPLICIT NONE
        PRIVATE

        INTEGER, PARAMETER :: Q1NP_MAX_CTRL = 50
        LOGICAL, PARAMETER, PUBLIC :: Q1NP_VIZ_REMAP_ENABLED = .TRUE.

        INTEGER, DIMENSION(:), ALLOCATABLE :: Q1NP_OLDNODE_IDS_SAVE
        INTEGER, DIMENSION(:), ALLOCATABLE :: Q1NP_ELEM_IDS_SAVE
        INTEGER, DIMENSION(:), ALLOCATABLE :: Q1NP_CORNER_IDS_SAVE
        REAL(KIND=WP), DIMENSION(:,:), ALLOCATABLE :: Q1NP_REFXYZ_SAVE
        LOGICAL :: Q1NP_REMAP_MAP_READY = .FALSE.

        INTEGER, PARAMETER :: Q1NP_FACE_IXS(4,6) = RESHAPE( (/              &
     &      2,3,4,5,  6,7,8,9,  2,3,7,6,                                   &
     &      3,4,8,7,  4,5,9,8,  5,2,6,9 /), (/ 4, 6 /) )
        INTEGER, PARAMETER :: Q1NP_CORNER_PERM(4,8) = RESHAPE( (/           &
     &      1,2,3,4,  2,3,4,1,  3,4,1,2,  4,1,2,3,                         &
     &      1,4,3,2,  4,3,2,1,  3,2,1,4,  2,1,4,3 /), (/ 4, 8 /) )
        REAL(KIND=WP), PARAMETER :: Q1NP_CORNER_XI(4) =                    &
     &      (/ -1._WP, 1._WP, 1._WP, -1._WP /)
        REAL(KIND=WP), PARAMETER :: Q1NP_CORNER_ETA(4) =                   &
     &      (/ -1._WP, -1._WP, 1._WP, 1._WP /)

        PUBLIC :: Q1NP_VIZ_REMAP_ENSURE_MAP
        PUBLIC :: Q1NP_VIZ_REMAP_ACTIVE
        PUBLIC :: Q1NP_VIZ_REMAP_FILL_DISPLACEMENT
        PUBLIC :: Q1NP_VIZ_REMAP_FILL_COORDS

      CONTAINS

!=======================================================================
        LOGICAL FUNCTION Q1NP_VIZ_REMAP_ACTIVE(ISPMD, NSPMD)
          INTEGER, INTENT(IN) :: ISPMD, NSPMD
          Q1NP_VIZ_REMAP_ACTIVE =                                          &
     &        Q1NP_VIZ_REMAP_ENABLED .AND.                                 &
     &        Q1NP_REMAP_MAP_READY .AND.                                   &
     &        ISPMD == 0 .AND. NSPMD == 1 .AND.                            &
     &        ALLOCATED(Q1NP_OLDNODE_IDS_SAVE) .AND.                       &
     &        ALLOCATED(Q1NP_ELEM_IDS_SAVE) .AND.                          &
     &        ALLOCATED(Q1NP_CORNER_IDS_SAVE) .AND.                        &
     &        ALLOCATED(Q1NP_REFXYZ_SAVE) .AND.                            &
     &        SIZE(Q1NP_OLDNODE_IDS_SAVE) > 0
        END FUNCTION Q1NP_VIZ_REMAP_ACTIVE

!=======================================================================
!   Build (once) the legacy-node -> (elem, corner, refxyz) map.
!=======================================================================
        SUBROUTINE Q1NP_VIZ_REMAP_ENSURE_MAP(X, IXS, NUMNOD, NUMELS, NIXS, &
     &                                       ISPMD, NSPMD)
          INTEGER, INTENT(IN) :: NUMNOD, NUMELS, NIXS, ISPMD, NSPMD
          INTEGER, INTENT(IN) :: IXS(NIXS, NUMELS)
          REAL(KIND=WP), INTENT(IN) :: X(3*NUMNOD)

          INTEGER :: IEL, HEX_IEL, OFFCP, NCTRL, FACE_NODE(4)
          INTEGER :: CTRL_IDS(Q1NP_MAX_CTRL)
          INTEGER :: KK, JJ, II, NMAP, IFACE, IPERM
          INTEGER :: BEST_IFACE, BEST_IPERM, OLD_NODE_ID, CORNER_ID
          INTEGER :: KNOT_SET_ID, P_CUR, Q_CUR, ELEM_U_IDX, ELEM_V_IDX
          INTEGER :: NX_CUR, NY_CUR, U_LEN, V_LEN
          INTEGER, DIMENSION(:), ALLOCATABLE :: NODE_TO_ELEM, NODE_TO_CORNER
          REAL(KIND=WP), DIMENSION(:), ALLOCATABLE :: NODE_TO_DIST2
          REAL(KIND=WP), DIMENSION(:,:), ALLOCATABLE :: NODE_TO_REFXYZ
          REAL(KIND=WP), DIMENSION(:), ALLOCATABLE :: U_KNOT, V_KNOT
          REAL(KIND=WP) :: DIST2, BEST_DIST2, NODE_MATCH_DIST2
          REAL(KIND=WP) :: CORNER_XYZ(3,4), NVAL(Q1NP_MAX_CTRL)

          IF (.NOT. Q1NP_VIZ_REMAP_ENABLED) RETURN
          IF (.NOT. (ISPMD == 0 .AND. NSPMD == 1)) RETURN
          IF (Q1NP_REMAP_MAP_READY) RETURN

          NMAP = 0
          CALL MY_DEALLOC(Q1NP_OLDNODE_IDS_SAVE)
          CALL MY_DEALLOC(Q1NP_ELEM_IDS_SAVE)
          CALL MY_DEALLOC(Q1NP_CORNER_IDS_SAVE)
          CALL MY_DEALLOC(Q1NP_REFXYZ_SAVE)

          IF (NUMNOD > 0 .AND. NUMELQ1NP_G > 0) THEN
            IF (ALLOCATED(KQ1NP_TAB) .AND. ALLOCATED(IQ1NP_TAB) .AND.      &
     &          ALLOCATED(Q1NP_KTAB)) THEN
              CALL MY_ALLOC(NODE_TO_ELEM, NUMNOD, 'Q1NP_NODE_TO_ELEM')
              CALL MY_ALLOC(NODE_TO_CORNER, NUMNOD, 'Q1NP_NODE_TO_CORNER')
              CALL MY_ALLOC(NODE_TO_DIST2, NUMNOD, 'Q1NP_NODE_TO_DIST2')
              CALL MY_ALLOC(NODE_TO_REFXYZ, 3, NUMNOD, 'Q1NP_NODE_TO_REFXYZ')
              NODE_TO_ELEM(1:NUMNOD) = 0
              NODE_TO_CORNER(1:NUMNOD) = 0
              NODE_TO_DIST2(1:NUMNOD) = HUGE(0._WP)
              NODE_TO_REFXYZ(1:3,1:NUMNOD) = 0._WP

              DO IEL = 1, NUMELQ1NP_G
                HEX_IEL = 0
                IF (SIZE(KQ1NP_TAB,1) >= 10) HEX_IEL = KQ1NP_TAB(10,IEL)
                IF (HEX_IEL >= 1 .AND. HEX_IEL <= NUMELS) THEN
                  IF (IXS(NIXS,HEX_IEL) /= KQ1NP_TAB(5,IEL)) HEX_IEL = 0
                END IF
                IF (HEX_IEL <= 0) THEN
                  DO II = 1, NUMELS
                    IF (IXS(NIXS,II) == KQ1NP_TAB(5,IEL)) THEN
                      HEX_IEL = II
                      EXIT
                    END IF
                  END DO
                END IF
                IF (HEX_IEL < 1 .OR. HEX_IEL > NUMELS) CYCLE

                NCTRL = KQ1NP_TAB(3,IEL)
                OFFCP = KQ1NP_TAB(4,IEL)
                P_CUR = KQ1NP_TAB(8,IEL)
                Q_CUR = KQ1NP_TAB(9,IEL)
                ELEM_U_IDX = KQ1NP_TAB(6,IEL)
                ELEM_V_IDX = KQ1NP_TAB(7,IEL)
                IF (NCTRL <= 0 .OR. OFFCP < 1) CYCLE
                IF (NCTRL > Q1NP_MAX_CTRL) CYCLE
                IF (OFFCP + NCTRL - 1 > SIZE(IQ1NP_TAB)) CYCLE

                KNOT_SET_ID = 1
                IF (SIZE(KQ1NP_TAB,1) >= 15) KNOT_SET_ID = KQ1NP_TAB(15,IEL)
                NX_CUR = KQ1NP_TAB(12,IEL)
                NY_CUR = KQ1NP_TAB(13,IEL)
                IF (NX_CUR <= 0 .OR. NY_CUR <= 0) THEN
                  IF (Q1NP_NKNOT_SETS_G > 0 .AND. KNOT_SET_ID > 0 .AND.    &
     &                KNOT_SET_ID <= Q1NP_NKNOT_SETS_G) THEN
                    NX_CUR = Q1NP_NX_SET_G(KNOT_SET_ID)
                    NY_CUR = Q1NP_NY_SET_G(KNOT_SET_ID)
                  ELSE
                    NX_CUR = Q1NP_NX_G
                    NY_CUR = Q1NP_NY_G
                  END IF
                END IF
                U_LEN = NX_CUR + 2*P_CUR + 1
                V_LEN = NY_CUR + 2*Q_CUR + 1
                IF (U_LEN <= 0 .OR. V_LEN <= 0) CYCLE

                CALL MY_ALLOC(U_KNOT, U_LEN, 'Q1NP_U_KNOT')
                CALL MY_ALLOC(V_KNOT, V_LEN, 'Q1NP_V_KNOT')
                IF (Q1NP_NKNOT_SETS_G > 0 .AND. KNOT_SET_ID > 0 .AND.      &
     &              KNOT_SET_ID <= Q1NP_NKNOT_SETS_G .AND.                 &
     &              ALLOCATED(Q1NP_KTAB_OFF_G)) THEN
                  U_KNOT(1:U_LEN) = Q1NP_KTAB(                              &
     &                Q1NP_KTAB_OFF_G(KNOT_SET_ID):                         &
     &                Q1NP_KTAB_OFF_G(KNOT_SET_ID)+U_LEN-1)
                  V_KNOT(1:V_LEN) = Q1NP_KTAB(                              &
     &                Q1NP_KTAB_OFF_G(KNOT_SET_ID)+U_LEN:                   &
     &                Q1NP_KTAB_OFF_G(KNOT_SET_ID)+U_LEN+V_LEN-1)
                ELSE
                  U_KNOT(1:U_LEN) = Q1NP_KTAB(1:U_LEN)
                  V_KNOT(1:V_LEN) = Q1NP_KTAB(U_LEN+1:U_LEN+V_LEN)
                END IF

                CTRL_IDS(1:Q1NP_MAX_CTRL) = 0
                DO KK = 1, NCTRL
                  CTRL_IDS(KK) = IQ1NP_TAB(OFFCP+KK-1)
                END DO

                DO CORNER_ID = 1, 4
                  CALL Q1NP_EVALUATE_NURBS_SHAPE_VALUES(                    &
     &                Q1NP_CORNER_XI(CORNER_ID),                            &
     &                Q1NP_CORNER_ETA(CORNER_ID),                           &
     &                P_CUR, Q_CUR,                                         &
     &                ELEM_U_IDX, ELEM_V_IDX,                               &
     &                U_KNOT, V_KNOT,                                       &
     &                NCTRL, NVAL(1:NCTRL))
                  CORNER_XYZ(1:3,CORNER_ID) = 0._WP
                  DO KK = 1, NCTRL
                    II = CTRL_IDS(KK)
                    IF (II <= 0 .OR. II > NUMNOD) CYCLE
                    CORNER_XYZ(1,CORNER_ID) = CORNER_XYZ(1,CORNER_ID)      &
     &                  + NVAL(KK) * X(3*(II-1)+1)
                    CORNER_XYZ(2,CORNER_ID) = CORNER_XYZ(2,CORNER_ID)      &
     &                  + NVAL(KK) * X(3*(II-1)+2)
                    CORNER_XYZ(3,CORNER_ID) = CORNER_XYZ(3,CORNER_ID)      &
     &                  + NVAL(KK) * X(3*(II-1)+3)
                  END DO
                END DO
                CALL MY_DEALLOC(U_KNOT)
                CALL MY_DEALLOC(V_KNOT)

                BEST_DIST2 = HUGE(0._WP)
                BEST_IFACE = 0
                BEST_IPERM = 0
                DO IFACE = 1, 6
                  DO KK = 1, 4
                    FACE_NODE(KK) = IXS(Q1NP_FACE_IXS(KK,IFACE), HEX_IEL)
                  END DO
                  DO IPERM = 1, 8
                    DIST2 = 0._WP
                    DO KK = 1, 4
                      JJ = Q1NP_CORNER_PERM(KK,IPERM)
                      OLD_NODE_ID = FACE_NODE(JJ)
                      IF (OLD_NODE_ID <= 0 .OR. OLD_NODE_ID > NUMNOD) THEN
                        DIST2 = HUGE(0._WP)
                        EXIT
                      END IF
                      DIST2 = DIST2                                        &
     &                    + (X(3*(OLD_NODE_ID-1)+1) - CORNER_XYZ(1,KK))**2 &
     &                    + (X(3*(OLD_NODE_ID-1)+2) - CORNER_XYZ(2,KK))**2 &
     &                    + (X(3*(OLD_NODE_ID-1)+3) - CORNER_XYZ(3,KK))**2
                    END DO
                    IF (DIST2 < BEST_DIST2) THEN
                      BEST_DIST2 = DIST2
                      BEST_IFACE = IFACE
                      BEST_IPERM = IPERM
                    END IF
                  END DO
                END DO

                IF (BEST_IFACE <= 0 .OR. BEST_IPERM <= 0) CYCLE
                DO KK = 1, 4
                  JJ = Q1NP_CORNER_PERM(KK,BEST_IPERM)
                  OLD_NODE_ID = IXS(Q1NP_FACE_IXS(JJ,BEST_IFACE), HEX_IEL)
                  CORNER_ID = KK
                  IF (OLD_NODE_ID <= 0 .OR. OLD_NODE_ID > NUMNOD) CYCLE
                  NODE_MATCH_DIST2 =                                       &
     &                (X(3*(OLD_NODE_ID-1)+1) - CORNER_XYZ(1,CORNER_ID))**2 &
     &              + (X(3*(OLD_NODE_ID-1)+2) - CORNER_XYZ(2,CORNER_ID))**2 &
     &              + (X(3*(OLD_NODE_ID-1)+3) - CORNER_XYZ(3,CORNER_ID))**2
                  IF (NODE_MATCH_DIST2 < NODE_TO_DIST2(OLD_NODE_ID)) THEN
                    NODE_TO_ELEM(OLD_NODE_ID) = IEL
                    NODE_TO_CORNER(OLD_NODE_ID) = CORNER_ID
                    NODE_TO_DIST2(OLD_NODE_ID) = NODE_MATCH_DIST2
                    NODE_TO_REFXYZ(1:3,OLD_NODE_ID) =                      &
     &                  CORNER_XYZ(1:3,CORNER_ID)
                  END IF
                END DO
              END DO

              NMAP = COUNT(NODE_TO_ELEM > 0)
              IF (NMAP > 0) THEN
                CALL MY_ALLOC(Q1NP_OLDNODE_IDS_SAVE, NMAP,                  &
     &              'Q1NP_OLDNODE_IDS_SAVE')
                CALL MY_ALLOC(Q1NP_ELEM_IDS_SAVE, NMAP,                     &
     &              'Q1NP_ELEM_IDS_SAVE')
                CALL MY_ALLOC(Q1NP_CORNER_IDS_SAVE, NMAP,                   &
     &              'Q1NP_CORNER_IDS_SAVE')
                CALL MY_ALLOC(Q1NP_REFXYZ_SAVE, 3, NMAP,                    &
     &              'Q1NP_REFXYZ_SAVE')
                II = 0
                DO KK = 1, NUMNOD
                  IF (NODE_TO_ELEM(KK) > 0) THEN
                    II = II + 1
                    Q1NP_OLDNODE_IDS_SAVE(II) = KK
                    Q1NP_ELEM_IDS_SAVE(II) = NODE_TO_ELEM(KK)
                    Q1NP_CORNER_IDS_SAVE(II) = NODE_TO_CORNER(KK)
                    Q1NP_REFXYZ_SAVE(1:3,II) = NODE_TO_REFXYZ(1:3,KK)
                  END IF
                END DO
              END IF

              CALL MY_DEALLOC(NODE_TO_ELEM)
              CALL MY_DEALLOC(NODE_TO_CORNER)
              CALL MY_DEALLOC(NODE_TO_DIST2)
              CALL MY_DEALLOC(NODE_TO_REFXYZ)
            END IF
          END IF

          Q1NP_REMAP_MAP_READY = .TRUE.
        END SUBROUTINE Q1NP_VIZ_REMAP_ENSURE_MAP

!=======================================================================
!   Fill synthetic displacements for H3D.
!   FIRST_FRAME=.TRUE.  -> D = X - REF   (node creation onto reference)
!   FIRST_FRAME=.FALSE. -> D = TARGET - REF (follow current span corners)
!=======================================================================
        SUBROUTINE Q1NP_VIZ_REMAP_FILL_DISPLACEMENT(X, D_OUT, NUMNOD,      &
     &      ISPMD, NSPMD, FIRST_FRAME)
          INTEGER, INTENT(IN) :: NUMNOD, ISPMD, NSPMD
          LOGICAL, INTENT(IN) :: FIRST_FRAME
          REAL(KIND=WP), INTENT(IN) :: X(3*NUMNOD)
          REAL(KIND=WP), INTENT(INOUT) :: D_OUT(3*NUMNOD)

          INTEGER :: NMAP, KK, OLD_NODE_ID, IEL, CORNER_ID
          INTEGER :: CACHE_ELEM, OFFCP, NCTRL, CTRL_IDS(Q1NP_MAX_CTRL)
          INTEGER :: II, JJ, KNOT_SET_ID, P_CUR, Q_CUR
          INTEGER :: ELEM_U_IDX, ELEM_V_IDX, NX_CUR, NY_CUR, U_LEN, V_LEN
          INTEGER :: NODE_ID_TMP
          LOGICAL :: CORNER_READY
          REAL(KIND=WP), DIMENSION(:), ALLOCATABLE :: U_KNOT, V_KNOT
          REAL(KIND=WP) :: CORNER_XYZ(3,4), TARGET_XYZ(3)
          REAL(KIND=WP) :: NVAL(Q1NP_MAX_CTRL)

          IF (.NOT. Q1NP_VIZ_REMAP_ACTIVE(ISPMD, NSPMD)) RETURN
          IF (.NOT. ALLOCATED(KQ1NP_TAB)) RETURN
          IF (.NOT. ALLOCATED(IQ1NP_TAB)) RETURN
          IF (.NOT. ALLOCATED(Q1NP_KTAB)) RETURN

          NMAP = SIZE(Q1NP_OLDNODE_IDS_SAVE)

          IF (FIRST_FRAME) THEN
            DO KK = 1, NMAP
              OLD_NODE_ID = Q1NP_OLDNODE_IDS_SAVE(KK)
              IF (OLD_NODE_ID < 1 .OR. OLD_NODE_ID > NUMNOD) CYCLE
              D_OUT(3*(OLD_NODE_ID-1)+1) =                                 &
     &            X(3*(OLD_NODE_ID-1)+1) - Q1NP_REFXYZ_SAVE(1,KK)
              D_OUT(3*(OLD_NODE_ID-1)+2) =                                 &
     &            X(3*(OLD_NODE_ID-1)+2) - Q1NP_REFXYZ_SAVE(2,KK)
              D_OUT(3*(OLD_NODE_ID-1)+3) =                                 &
     &            X(3*(OLD_NODE_ID-1)+3) - Q1NP_REFXYZ_SAVE(3,KK)
            END DO
            RETURN
          END IF

          CACHE_ELEM = 0
          CORNER_READY = .FALSE.
          DO KK = 1, NMAP
            OLD_NODE_ID = Q1NP_OLDNODE_IDS_SAVE(KK)
            IEL = Q1NP_ELEM_IDS_SAVE(KK)
            CORNER_ID = Q1NP_CORNER_IDS_SAVE(KK)
            IF (OLD_NODE_ID < 1 .OR. OLD_NODE_ID > NUMNOD) CYCLE
            IF (IEL < 1 .OR. IEL > NUMELQ1NP_G) CYCLE
            IF (CORNER_ID < 1 .OR. CORNER_ID > 4) CYCLE

            IF (CACHE_ELEM /= IEL) THEN
              CACHE_ELEM = IEL
              CORNER_READY = .FALSE.
              IF (ALLOCATED(U_KNOT)) CALL MY_DEALLOC(U_KNOT)
              IF (ALLOCATED(V_KNOT)) CALL MY_DEALLOC(V_KNOT)
              NCTRL = KQ1NP_TAB(3,IEL)
              OFFCP = KQ1NP_TAB(4,IEL)
              P_CUR = KQ1NP_TAB(8,IEL)
              Q_CUR = KQ1NP_TAB(9,IEL)
              ELEM_U_IDX = KQ1NP_TAB(6,IEL)
              ELEM_V_IDX = KQ1NP_TAB(7,IEL)
              IF (NCTRL <= 0 .OR. OFFCP < 1) CYCLE
              IF (NCTRL > Q1NP_MAX_CTRL) CYCLE
              IF (OFFCP + NCTRL - 1 > SIZE(IQ1NP_TAB)) CYCLE

              KNOT_SET_ID = 1
              IF (SIZE(KQ1NP_TAB,1) >= 15) KNOT_SET_ID = KQ1NP_TAB(15,IEL)
              NX_CUR = KQ1NP_TAB(12,IEL)
              NY_CUR = KQ1NP_TAB(13,IEL)
              IF (NX_CUR <= 0 .OR. NY_CUR <= 0) THEN
                IF (Q1NP_NKNOT_SETS_G > 0 .AND. KNOT_SET_ID > 0 .AND.      &
     &              KNOT_SET_ID <= Q1NP_NKNOT_SETS_G) THEN
                  NX_CUR = Q1NP_NX_SET_G(KNOT_SET_ID)
                  NY_CUR = Q1NP_NY_SET_G(KNOT_SET_ID)
                ELSE
                  NX_CUR = Q1NP_NX_G
                  NY_CUR = Q1NP_NY_G
                END IF
              END IF
              U_LEN = NX_CUR + 2*P_CUR + 1
              V_LEN = NY_CUR + 2*Q_CUR + 1
              IF (U_LEN <= 0 .OR. V_LEN <= 0) CYCLE

              CALL MY_ALLOC(U_KNOT, U_LEN, 'Q1NP_U_KNOT')
              CALL MY_ALLOC(V_KNOT, V_LEN, 'Q1NP_V_KNOT')
              IF (Q1NP_NKNOT_SETS_G > 0 .AND. KNOT_SET_ID > 0 .AND.        &
     &            KNOT_SET_ID <= Q1NP_NKNOT_SETS_G .AND.                   &
     &            ALLOCATED(Q1NP_KTAB_OFF_G)) THEN
                U_KNOT(1:U_LEN) = Q1NP_KTAB(                                &
     &              Q1NP_KTAB_OFF_G(KNOT_SET_ID):                           &
     &              Q1NP_KTAB_OFF_G(KNOT_SET_ID)+U_LEN-1)
                V_KNOT(1:V_LEN) = Q1NP_KTAB(                                &
     &              Q1NP_KTAB_OFF_G(KNOT_SET_ID)+U_LEN:                     &
     &              Q1NP_KTAB_OFF_G(KNOT_SET_ID)+U_LEN+V_LEN-1)
              ELSE
                U_KNOT(1:U_LEN) = Q1NP_KTAB(1:U_LEN)
                V_KNOT(1:V_LEN) = Q1NP_KTAB(U_LEN+1:U_LEN+V_LEN)
              END IF

              CTRL_IDS(1:Q1NP_MAX_CTRL) = 0
              DO II = 1, NCTRL
                CTRL_IDS(II) = IQ1NP_TAB(OFFCP+II-1)
              END DO
              DO JJ = 1, 4
                CALL Q1NP_EVALUATE_NURBS_SHAPE_VALUES(                      &
     &              Q1NP_CORNER_XI(JJ), Q1NP_CORNER_ETA(JJ),                &
     &              P_CUR, Q_CUR, ELEM_U_IDX, ELEM_V_IDX,                   &
     &              U_KNOT, V_KNOT, NCTRL, NVAL(1:NCTRL))
                CORNER_XYZ(1:3,JJ) = 0._WP
                DO II = 1, NCTRL
                  NODE_ID_TMP = CTRL_IDS(II)
                  IF (NODE_ID_TMP <= 0 .OR. NODE_ID_TMP > NUMNOD) CYCLE
                  CORNER_XYZ(1,JJ) = CORNER_XYZ(1,JJ)                      &
     &                + NVAL(II) * X(3*(NODE_ID_TMP-1)+1)
                  CORNER_XYZ(2,JJ) = CORNER_XYZ(2,JJ)                      &
     &                + NVAL(II) * X(3*(NODE_ID_TMP-1)+2)
                  CORNER_XYZ(3,JJ) = CORNER_XYZ(3,JJ)                      &
     &                + NVAL(II) * X(3*(NODE_ID_TMP-1)+3)
                END DO
              END DO
              CALL MY_DEALLOC(U_KNOT)
              CALL MY_DEALLOC(V_KNOT)
              CORNER_READY = .TRUE.
            END IF

            IF (.NOT. CORNER_READY) CYCLE
            TARGET_XYZ(1:3) = CORNER_XYZ(1:3,CORNER_ID)
            D_OUT(3*(OLD_NODE_ID-1)+1) =                                   &
     &          TARGET_XYZ(1) - Q1NP_REFXYZ_SAVE(1,KK)
            D_OUT(3*(OLD_NODE_ID-1)+2) =                                   &
     &          TARGET_XYZ(2) - Q1NP_REFXYZ_SAVE(2,KK)
            D_OUT(3*(OLD_NODE_ID-1)+3) =                                   &
     &          TARGET_XYZ(3) - Q1NP_REFXYZ_SAVE(3,KK)
          END DO
          IF (ALLOCATED(U_KNOT)) CALL MY_DEALLOC(U_KNOT)
          IF (ALLOCATED(V_KNOT)) CALL MY_DEALLOC(V_KNOT)
        END SUBROUTINE Q1NP_VIZ_REMAP_FILL_DISPLACEMENT

!=======================================================================
!   Overwrite remapped node coordinates with current NURBS span corners
!   (ANIM absolute-coordinate path). Caller must initialize X_OUT = X.
!=======================================================================
        SUBROUTINE Q1NP_VIZ_REMAP_FILL_COORDS(X, X_OUT, NUMNOD,            &
     &      ISPMD, NSPMD)
          INTEGER, INTENT(IN) :: NUMNOD, ISPMD, NSPMD
          REAL(KIND=WP), INTENT(IN) :: X(3*NUMNOD)
          REAL(KIND=WP), INTENT(INOUT) :: X_OUT(3*NUMNOD)

          INTEGER :: NMAP, KK, OLD_NODE_ID, IEL, CORNER_ID
          INTEGER :: CACHE_ELEM, OFFCP, NCTRL, CTRL_IDS(Q1NP_MAX_CTRL)
          INTEGER :: II, JJ, KNOT_SET_ID, P_CUR, Q_CUR
          INTEGER :: ELEM_U_IDX, ELEM_V_IDX, NX_CUR, NY_CUR, U_LEN, V_LEN
          INTEGER :: NODE_ID_TMP
          LOGICAL :: CORNER_READY
          REAL(KIND=WP), DIMENSION(:), ALLOCATABLE :: U_KNOT, V_KNOT
          REAL(KIND=WP) :: CORNER_XYZ(3,4)
          REAL(KIND=WP) :: NVAL(Q1NP_MAX_CTRL)

          IF (.NOT. Q1NP_VIZ_REMAP_ACTIVE(ISPMD, NSPMD)) RETURN
          IF (.NOT. ALLOCATED(KQ1NP_TAB)) RETURN
          IF (.NOT. ALLOCATED(IQ1NP_TAB)) RETURN
          IF (.NOT. ALLOCATED(Q1NP_KTAB)) RETURN

          NMAP = SIZE(Q1NP_OLDNODE_IDS_SAVE)
          CACHE_ELEM = 0
          CORNER_READY = .FALSE.
          DO KK = 1, NMAP
            OLD_NODE_ID = Q1NP_OLDNODE_IDS_SAVE(KK)
            IEL = Q1NP_ELEM_IDS_SAVE(KK)
            CORNER_ID = Q1NP_CORNER_IDS_SAVE(KK)
            IF (OLD_NODE_ID < 1 .OR. OLD_NODE_ID > NUMNOD) CYCLE
            IF (IEL < 1 .OR. IEL > NUMELQ1NP_G) CYCLE
            IF (CORNER_ID < 1 .OR. CORNER_ID > 4) CYCLE

            IF (CACHE_ELEM /= IEL) THEN
              CACHE_ELEM = IEL
              CORNER_READY = .FALSE.
              IF (ALLOCATED(U_KNOT)) CALL MY_DEALLOC(U_KNOT)
              IF (ALLOCATED(V_KNOT)) CALL MY_DEALLOC(V_KNOT)
              NCTRL = KQ1NP_TAB(3,IEL)
              OFFCP = KQ1NP_TAB(4,IEL)
              P_CUR = KQ1NP_TAB(8,IEL)
              Q_CUR = KQ1NP_TAB(9,IEL)
              ELEM_U_IDX = KQ1NP_TAB(6,IEL)
              ELEM_V_IDX = KQ1NP_TAB(7,IEL)
              IF (NCTRL <= 0 .OR. OFFCP < 1) CYCLE
              IF (NCTRL > Q1NP_MAX_CTRL) CYCLE
              IF (OFFCP + NCTRL - 1 > SIZE(IQ1NP_TAB)) CYCLE

              KNOT_SET_ID = 1
              IF (SIZE(KQ1NP_TAB,1) >= 15) KNOT_SET_ID = KQ1NP_TAB(15,IEL)
              NX_CUR = KQ1NP_TAB(12,IEL)
              NY_CUR = KQ1NP_TAB(13,IEL)
              IF (NX_CUR <= 0 .OR. NY_CUR <= 0) THEN
                IF (Q1NP_NKNOT_SETS_G > 0 .AND. KNOT_SET_ID > 0 .AND.      &
     &              KNOT_SET_ID <= Q1NP_NKNOT_SETS_G) THEN
                  NX_CUR = Q1NP_NX_SET_G(KNOT_SET_ID)
                  NY_CUR = Q1NP_NY_SET_G(KNOT_SET_ID)
                ELSE
                  NX_CUR = Q1NP_NX_G
                  NY_CUR = Q1NP_NY_G
                END IF
              END IF
              U_LEN = NX_CUR + 2*P_CUR + 1
              V_LEN = NY_CUR + 2*Q_CUR + 1
              IF (U_LEN <= 0 .OR. V_LEN <= 0) CYCLE

              CALL MY_ALLOC(U_KNOT, U_LEN, 'Q1NP_U_KNOT')
              CALL MY_ALLOC(V_KNOT, V_LEN, 'Q1NP_V_KNOT')
              IF (Q1NP_NKNOT_SETS_G > 0 .AND. KNOT_SET_ID > 0 .AND.        &
     &            KNOT_SET_ID <= Q1NP_NKNOT_SETS_G .AND.                   &
     &            ALLOCATED(Q1NP_KTAB_OFF_G)) THEN
                U_KNOT(1:U_LEN) = Q1NP_KTAB(                                &
     &              Q1NP_KTAB_OFF_G(KNOT_SET_ID):                           &
     &              Q1NP_KTAB_OFF_G(KNOT_SET_ID)+U_LEN-1)
                V_KNOT(1:V_LEN) = Q1NP_KTAB(                                &
     &              Q1NP_KTAB_OFF_G(KNOT_SET_ID)+U_LEN:                     &
     &              Q1NP_KTAB_OFF_G(KNOT_SET_ID)+U_LEN+V_LEN-1)
              ELSE
                U_KNOT(1:U_LEN) = Q1NP_KTAB(1:U_LEN)
                V_KNOT(1:V_LEN) = Q1NP_KTAB(U_LEN+1:U_LEN+V_LEN)
              END IF

              CTRL_IDS(1:Q1NP_MAX_CTRL) = 0
              DO II = 1, NCTRL
                CTRL_IDS(II) = IQ1NP_TAB(OFFCP+II-1)
              END DO
              DO JJ = 1, 4
                CALL Q1NP_EVALUATE_NURBS_SHAPE_VALUES(                      &
     &              Q1NP_CORNER_XI(JJ), Q1NP_CORNER_ETA(JJ),                &
     &              P_CUR, Q_CUR, ELEM_U_IDX, ELEM_V_IDX,                   &
     &              U_KNOT, V_KNOT, NCTRL, NVAL(1:NCTRL))
                CORNER_XYZ(1:3,JJ) = 0._WP
                DO II = 1, NCTRL
                  NODE_ID_TMP = CTRL_IDS(II)
                  IF (NODE_ID_TMP <= 0 .OR. NODE_ID_TMP > NUMNOD) CYCLE
                  CORNER_XYZ(1,JJ) = CORNER_XYZ(1,JJ)                      &
     &                + NVAL(II) * X(3*(NODE_ID_TMP-1)+1)
                  CORNER_XYZ(2,JJ) = CORNER_XYZ(2,JJ)                      &
     &                + NVAL(II) * X(3*(NODE_ID_TMP-1)+2)
                  CORNER_XYZ(3,JJ) = CORNER_XYZ(3,JJ)                      &
     &                + NVAL(II) * X(3*(NODE_ID_TMP-1)+3)
                END DO
              END DO
              CALL MY_DEALLOC(U_KNOT)
              CALL MY_DEALLOC(V_KNOT)
              CORNER_READY = .TRUE.
            END IF

            IF (.NOT. CORNER_READY) CYCLE
            X_OUT(3*(OLD_NODE_ID-1)+1) = CORNER_XYZ(1,CORNER_ID)
            X_OUT(3*(OLD_NODE_ID-1)+2) = CORNER_XYZ(2,CORNER_ID)
            X_OUT(3*(OLD_NODE_ID-1)+3) = CORNER_XYZ(3,CORNER_ID)
          END DO
          IF (ALLOCATED(U_KNOT)) CALL MY_DEALLOC(U_KNOT)
          IF (ALLOCATED(V_KNOT)) CALL MY_DEALLOC(V_KNOT)
        END SUBROUTINE Q1NP_VIZ_REMAP_FILL_COORDS

      END MODULE Q1NP_VIZ_REMAP_MOD
