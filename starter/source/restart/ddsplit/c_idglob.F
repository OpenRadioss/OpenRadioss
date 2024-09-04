Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||    c_idglob               ../starter/source/restart/ddsplit/c_idglob.F
      !||--- called by ------------------------------------------------------
      !||    ddsplit                ../starter/source/restart/ddsplit/ddsplit.F
      !||--- uses       -----------------------------------------------------
      !||    reorder_mod            ../starter/share/modules1/reorder_mod.F
      !||====================================================================
      SUBROUTINE C_IDGLOB(NUMEL, NUMELS_L, NUMELQ_L, NUMELTG_L, NUMELS_G, NUMELQ_G, NUMELTG_G,
     .     PROC, CEL, CEP, IPARG, ALE_CONNECTIVITY, IXS,IXQ,IXTG, IDGLOB_L, UIDGLOB_L, N2D, NGROUP, NPARG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE REORDER_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: N2D !< 2d / 3d flag
      INTEGER, INTENT(IN) :: NGROUP, NPARG !< sizes of array IPARG
      INTEGER, INTENT(IN) :: PROC, NUMEL
      INTEGER, INTENT(IN) :: NUMELS_L, NUMELQ_L, NUMELTG_L  !< local number of elems (current domain)
      INTEGER, INTENT(IN) :: NUMELS_G, NUMELQ_G, NUMELTG_G  !< global number of elems (all domains)
      INTEGER, INTENT(IN) :: CEL(*), CEP(*)
      INTEGER, INTENT(IN) :: IXS(NIXS, NUMELS_G), IXQ(NIXQ, NUMELQ_G), IXTG(NIXTG, NUMELTG_G)
      INTEGER, INTENT(IN) :: IPARG(NPARG, NGROUP)
      INTEGER, INTENT(INOUT) :: IDGLOB_L(*), UIDGLOB_L(*)
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: PROCI, II, JJ, I, J, I_LOC, NEL, ITY, NFT, ICOUNT,NG, PROCJ, IAD1, LGTH
      INTEGER :: NELEM_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      ALLOCATE(TAG(NUMEL))
      TAG(1:NUMEL) = 0

      ICOUNT = 0
      NELEM_L = 0
      DO NG = 1, NGROUP
         NEL = IPARG(2, NG)
         NFT = IPARG(3, NG)
         ITY = IPARG(5, NG)
         IF (ITY == 1) THEN
            ! bricks
            NELEM_L = NUMELS_L
            DO II = 1, NEL
               I = II + NFT
               IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(I)
               LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(I+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(I)
               PROCI = CEP(I)
               IF (PROCI == PROC) THEN
                  I_LOC = CEL(I)
                  IDGLOB_L(I_LOC) = PERMUTATION%SOLID(I)
                  UIDGLOB_L(I_LOC) = IXS(NIXS, I)
                  DO JJ = 1, LGTH
                     J = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + JJ - 1)
                     IF (J > 0) THEN
                        PROCJ = CEP(J)
                        IF (PROCJ /= PROC .AND. TAG(J) == 0) THEN
                           ICOUNT = ICOUNT + 1
                           IDGLOB_L(NELEM_L + ICOUNT) = PERMUTATION%SOLID(J)
                           UIDGLOB_L(NELEM_L + ICOUNT) = IXS(NIXS, J)
                           TAG(J) = 1
                       ENDIF
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO

         ELSEIF( ITY == 2)THEN
            ! quads
            NELEM_L = NUMELQ_L
            DO II = 1, NEL
               I = II + NFT
               IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(I)
               LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(I+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(I)
               PROCI = CEP(I)
               IF (PROCI == PROC) THEN
                  I_LOC = CEL(I)
                  IDGLOB_L(I_LOC) = PERMUTATION%SOLID(I)
                  UIDGLOB_L(I_LOC) = IXQ(NIXQ, I)
                  DO JJ = 1, LGTH
                     J = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + JJ - 1)
                     IF (J > 0) THEN
                        PROCJ = CEP(J)
                        IF (PROCJ /= PROC .AND. TAG(J) == 0) THEN
                           ICOUNT = ICOUNT + 1
                           IDGLOB_L(NELEM_L + ICOUNT) = PERMUTATION%SOLID(J)
                           UIDGLOB_L(NELEM_L + ICOUNT) = IXQ(NIXQ, J)
                           TAG(J) = 1
                       ENDIF
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO

         ELSEIF(ITY == 7 .AND. N2D >= 0)THEN
           ! trias
           NELEM_L = NUMELTG_L
            DO II = 1, NEL
               I = II + NFT
               IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(I)
               LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(I+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(I)
               PROCI = CEP(I)
               IF (PROCI == PROC) THEN
                  I_LOC = CEL(I)
                  IDGLOB_L(I_LOC) = PERMUTATION%SOLID(I)
                  UIDGLOB_L(I_LOC) = IXTG(NIXTG, I)
                  DO JJ = 1, LGTH
                     J = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + JJ - 1)
                     IF (J > 0) THEN
                        PROCJ = CEP(J)
                        IF (PROCJ /= PROC .AND. TAG(J) == 0) THEN
                           ICOUNT = ICOUNT + 1
                           IDGLOB_L(NELEM_L + ICOUNT) = PERMUTATION%SOLID(J)
                           UIDGLOB_L(NELEM_L + ICOUNT) = IXTG(NIXTG , J)
                           TAG(J) = 1
                       ENDIF
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
         ELSE
           ! not a group of solid elems
           CYCLE
         ENDIF

      ENDDO
      
      DEALLOCATE(TAG)
      END SUBROUTINE C_IDGLOB
C
      !||====================================================================
      !||    c_fasolfr   ../starter/source/restart/ddsplit/c_idglob.F
      !||--- called by ------------------------------------------------------
      !||    ddsplit     ../starter/source/restart/ddsplit/ddsplit.F
      !||====================================================================
      SUBROUTINE C_FASOLFR(FASOLFR, CEP, P, CEL, NFASOLFR_L)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FASOLFR(2,*), CEP(*),CEL(*),P, NFASOLFR_L
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NN
C
      DO N=1,NFASOLFR
        NN=FASOLFR(1,N)
        IF(CEP(NN)==P)NFASOLFR_L=NFASOLFR_L+1
      ENDDO
C
      RETURN
      END
