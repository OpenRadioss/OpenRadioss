Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!||====================================================================
!||    alemuscl_upwind        ../engine/source/ale/alemuscl/alemuscl_upwind.F
!||--- called by ------------------------------------------------------
!||    afluxt                 ../engine/source/ale/ale51/afluxt.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod   ../common_source/modules/ale/ale_connectivity_mod.F
!||    alemuscl_mod           ../common_source/modules/ale/alemuscl_mod.F
!||    i22bufbric_mod         ../common_source/modules/interfaces/cut-cell-search_mod.F
!||    i22tri_mod             ../common_source/modules/interfaces/cut-cell-search_mod.F
!||    segvar_mod             ../engine/share/modules/segvar_mod.F
!||====================================================================
      SUBROUTINE ALEMUSCL_UPWIND(FLUX, ALE_CONNECT, X, IXS, FLUX_VOIS,
     .                           N4_VOIS, ITAB, NV46, ITRIMAT, SEGVAR)
C-----------------------------------------------
C   D e s c r i p t i o n
C   This subroutines performs the following steps:
C     1 - compute a gradient for volume fraction ALPH
C         (calls GRADIENT_RECONSTRUCTION)
C     2 - reconstruct a value for volume fraction on each edge of the mesh
C         based on an affine approximation
C     3 - upwind this value on the edge and store it in the flux
C-----------------------------------------------
      USE I22BUFBRIC_MOD
      USE I22TRI_MOD
      USE ALEMUSCL_MOD
      USE SEGVAR_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "spmd_c.inc"
#include "vect01_c.inc"
#include "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NV46
      my_real, INTENT(OUT) :: FLUX(NV46, *)
      my_real, INTENT(IN) :: X(3,NUMNOD)
      INTEGER, INTENT(IN) :: IXS(NIXS,NUMELS)
      my_real, INTENT(OUT) :: FLUX_VOIS(NUMELS+NSVOIS, NV46)
      INTEGER, INTENT(OUT) :: N4_VOIS(NUMELS+NSVOIS,8)
      INTEGER, INTENT(IN) :: ITAB(NUMNOD)
      INTEGER, INTENT(IN) :: ITRIMAT
      TYPE(t_segvar),INTENT(IN) :: SEGVAR
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, II, KK, JJ, IAD2, IAD3
      INTEGER :: NEIGHBOOR_LIST(NV46), FACE_NEIGHBOOR(NV46)
      my_real :: ALPHAK
      my_real :: XK, YK, ZK
      my_real :: XF, YF, ZF
      INTEGER :: FACE_TO_NODE_LOCAL_ID(6, 4)
      my_real :: NORM(3), A(3), B(3), C(3), SURF, SURF1, SURF2
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
!!! Once for all, associate node local id to a face number
!!! Face 1
      FACE_TO_NODE_LOCAL_ID(1, 1) = 1 ; FACE_TO_NODE_LOCAL_ID(1, 2) = 4
      FACE_TO_NODE_LOCAL_ID(1, 3) = 3 ; FACE_TO_NODE_LOCAL_ID(1, 4) = 2
!!! Face 2
      FACE_TO_NODE_LOCAL_ID(2, 1) = 3 ; FACE_TO_NODE_LOCAL_ID(2, 2) = 4
      FACE_TO_NODE_LOCAL_ID(2, 3) = 8 ; FACE_TO_NODE_LOCAL_ID(2, 4) = 7
!!! Face 3
      FACE_TO_NODE_LOCAL_ID(3, 1) = 5 ; FACE_TO_NODE_LOCAL_ID(3, 2) = 6
      FACE_TO_NODE_LOCAL_ID(3, 3) = 7 ; FACE_TO_NODE_LOCAL_ID(3, 4) = 8
!!! Face 4
      FACE_TO_NODE_LOCAL_ID(4, 1) = 1 ; FACE_TO_NODE_LOCAL_ID(4, 2) = 2
      FACE_TO_NODE_LOCAL_ID(4, 3) = 6 ; FACE_TO_NODE_LOCAL_ID(4, 4) = 5
!!! Face 5
      FACE_TO_NODE_LOCAL_ID(5, 1) = 2 ; FACE_TO_NODE_LOCAL_ID(5, 2) = 3
      FACE_TO_NODE_LOCAL_ID(5, 3) = 7 ; FACE_TO_NODE_LOCAL_ID(5, 4) = 6
!!! Face 6
      FACE_TO_NODE_LOCAL_ID(6, 1) = 1 ; FACE_TO_NODE_LOCAL_ID(6, 2) = 5
      FACE_TO_NODE_LOCAL_ID(6, 3) = 8 ; FACE_TO_NODE_LOCAL_ID(6, 4) = 4

!!! First of all, compute gradient for alpha
      DO I = LFT, LLT
        II = I + NFT
        IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
        !!! Element centroid
        XK = ALEMUSCL_Buffer%ELCENTER(II,1) ; 
        YK = ALEMUSCL_Buffer%ELCENTER(II,2) ; 
        ZK = ALEMUSCL_Buffer%ELCENTER(II,3)
        !!! Neighbors
        DO KK = 1, NV46
          !!! Only for outgoing fluxes
          IF (FLUX(KK, II) > ZERO) THEN
            !!! Storing neighbor indexes
            NEIGHBOOR_LIST(KK) = ALE_CONNECT%ee_connect%connected(IAD2 + KK - 1)
            FACE_NEIGHBOOR(KK) = KK
            IF (NEIGHBOOR_LIST(KK) <= 0) THEN
              IF(NEIGHBOOR_LIST(KK)==0)NEIGHBOOR_LIST(KK) = II
              !case <0 is for eBCS. -NEIGHBOR_LIST is then the segment number
            ELSEIF (NEIGHBOOR_LIST(KK) <= NUMELS) THEN
              IAD3 = ALE_CONNECT%ee_connect%iad_connect(NEIGHBOOR_LIST(KK))
              !!! Store the face number to which II and NEIGHBOR_LIST(KK) are adjacent
              DO JJ = 1, NV46
                IF (ALE_CONNECT%ee_connect%connected(IAD3 + JJ - 1) == II) THEN
                  FACE_NEIGHBOOR(KK) = JJ
                ENDIF
              ENDDO  ! JJ = 1, NV46
            ENDIF

            !!! Face centroid
            XF = ZERO
            YF = ZERO
            ZF = ZERO

            A(1:3) = X(1:3, IXS(FACE_TO_NODE_LOCAL_ID(KK, 1) + 1, II))
            B(1:3) = X(1:3, IXS(FACE_TO_NODE_LOCAL_ID(KK, 2) + 1, II))
            C(1:3) = X(1:3, IXS(FACE_TO_NODE_LOCAL_ID(KK, 3) + 1, II))

            NORM(1) = (B(2) - A(2)) * (C(3) - A(3)) - (B(3) - A(3)) * (C(2) - A(2))
            NORM(2) = (B(3) - A(3)) * (C(1) - A(1)) - (B(1) - A(1)) * (C(3) - A(3))
            NORM(3) = (B(1) - A(1)) * (C(2) - A(2)) - (B(2) - A(2)) * (C(1) - A(1))

            SURF1 = HALF * ABS(SQRT(NORM(1) * NORM(1) + NORM(2) * NORM(2) + NORM(3) * NORM(3)))
            XF = SURF1 * THIRD * (A(1) + B(1) + C(1))
            YF = SURF1 * THIRD * (A(2) + B(2) + C(2))
            ZF = SURF1 * THIRD * (A(3) + B(3) + C(3))

            A(1:3) = X(1:3, IXS(FACE_TO_NODE_LOCAL_ID(KK, 1) + 1, II))
            B(1:3) = X(1:3, IXS(FACE_TO_NODE_LOCAL_ID(KK, 3) + 1, II))
            C(1:3) = X(1:3, IXS(FACE_TO_NODE_LOCAL_ID(KK, 4) + 1, II))

            NORM(1) = (B(2) - A(2)) * (C(3) - A(3)) - (B(3) - A(3)) * (C(2) - A(2))
            NORM(2) = (B(3) - A(3)) * (C(1) - A(1)) - (B(1) - A(1)) * (C(3) - A(3))
            NORM(3) = (B(1) - A(1)) * (C(2) - A(2)) - (B(2) - A(2)) * (C(1) - A(1))

            SURF2 = HALF * ABS(SQRT(NORM(1) * NORM(1) + NORM(2) * NORM(2) + NORM(3) * NORM(3)))
            XF = XF + SURF2 * THIRD * (A(1) + B(1) + C(1))
            YF = YF + SURF2 * THIRD * (A(2) + B(2) + C(2))
            ZF = ZF + SURF2 * THIRD * (A(3) + B(3) + C(3))

            SURF = SURF1 + SURF2
            XF = XF / SURF
            YF = YF / SURF
            ZF = ZF / SURF

            !!! Reconstruct second order value for ALPHA(II) on the face
            ALPHAK = ALEMUSCL_Buffer%VOLUME_FRACTION(II,ITRIMAT)
     .           + ALEMUSCL_Buffer%GRAD(II,1,ITRIMAT) * (XF - XK)
     .           + ALEMUSCL_Buffer%GRAD(II,2,ITRIMAT) * (YF - YK)
     .           + ALEMUSCL_Buffer%GRAD(II,3,ITRIMAT) * (ZF - ZK)
            !!! Partial volume flux is then computed as:
            FLUX(KK, II) = ALPHAK * FLUX(KK, II)
            IF (NEIGHBOOR_LIST(KK) > 0)THEN
              IF (NEIGHBOOR_LIST(KK) <= NUMELS .AND. NEIGHBOOR_LIST(KK) > 0) THEN
                !!! The opposite of the flux goes to the neighboord
                FLUX(FACE_NEIGHBOOR(KK), NEIGHBOOR_LIST(KK)) = -FLUX(KK, II)
              ELSE
                !!! ALE51_ANTIDIFF3
                FLUX_VOIS(II, KK) = FLUX(KK, II)
                N4_VOIS(II, 1) = ITAB(IXS(2, II))
                N4_VOIS(II, 2) = ITAB(IXS(3, II))
                N4_VOIS(II, 3) = ITAB(IXS(4, II))
                N4_VOIS(II, 4) = ITAB(IXS(5, II))
                N4_VOIS(II, 5) = ITAB(IXS(6, II))
                N4_VOIS(II, 6) = ITAB(IXS(7, II))
                N4_VOIS(II, 7) = ITAB(IXS(8, II))
                N4_VOIS(II, 8) = ITAB(IXS(9, II))
              ENDIF
            ENDIF
          ENDIF  ! (FLUX(KK, II) > ZERO)
        ENDDO  ! KK = 1, NV46
      ENDDO  ! I = LFT, LLT

C-----------------------------------------------
C         incoming volume fluxes from EBCS
C-----------------------------------------------
      IF(NSEGFLU > 0)THEN
        DO I = LFT, LLT
          II = I + NFT
          IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
          DO KK=1,NV46
            IF(FLUX(KK,II) < ZERO .AND. ALE_CONNECT%ee_connect%connected(IAD2 + KK - 1) < 0)THEN
              FLUX(KK,II) = SEGVAR%PHASE_ALPHA(ITRIMAT,-ALE_CONNECT%ee_connect%connected(IAD2 + KK - 1))*FLUX(KK,II)
            ENDIF
          ENDDO
        ENDDO
      ENDIF

C-----------------------------------------------
      END SUBROUTINE ALEMUSCL_UPWIND
