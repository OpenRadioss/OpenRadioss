Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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

Chd|====================================================================
Chd|  C_IDGLOB                      source/restart/ddsplit/c_idglob.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        REORDER_MOD                   share/modules1/reorder_mod.F  
Chd|====================================================================
      SUBROUTINE C_IDGLOB(NUMEL, NUMELS_L, NUMELQ_L, NUMELTG_L, 
     .     PROC, CEL, CEP, IPARG, ALE_CONNECTIVITY, IXS, IDGLOB_L, UIDGLOB_L)
      USE REORDER_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: PROC, NUMEL, NUMELS_L, NUMELQ_L, NUMELTG_L
      INTEGER, INTENT(IN) :: CEL(*), CEP(*), IXS(NIXS, *)
      INTEGER, INTENT(IN) :: IPARG(NPARG, *)
      INTEGER, INTENT(INOUT) :: IDGLOB_L(*), UIDGLOB_L(*)
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: PROCI, II, JJ, I, J, I_LOC, NEL, ITY, NFT, ICOUNT,
     .     NG, PROCJ, IAD1, LGTH
      INTEGER :: NELEM_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG

      ALLOCATE(TAG(NUMEL))
      TAG(1:NUMEL) = 0

      ICOUNT = 0
      DO NG = 1, NGROUP
         NEL = IPARG(2, NG)
         NFT = IPARG(3, NG)
         ITY = IPARG(5, NG)
         IF (ITY == 1) THEN
            NELEM_L = NUMELS_L
         ENDIF
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
      ENDDO
      
      DEALLOCATE(TAG)
      END SUBROUTINE C_IDGLOB
C
Chd|====================================================================
Chd|  C_FASOLFR                     source/restart/ddsplit/c_idglob.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|====================================================================
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
