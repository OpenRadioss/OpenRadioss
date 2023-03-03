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
C
Chd|====================================================================
Chd|  W_ING2LOC                     source/restart/ddsplit/w_ing2loc.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE W_ING2LOC(
     .  ALE_CONNECTIVITY,ELEMID_L,NUMNOD,NODGLOB,NODLOCAL,NUMNOD_L,LEN_IA,NWALE)
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMNOD, NUMNOD_L,LEN_IA,
     .        NODGLOB(*),  NODLOCAL(*), ELEMID_L(*), NWALE
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NI, LEN, IAD1, IAD2
      INTEGER, DIMENSION(:), ALLOCATABLE :: NB_CONNECT, CONNECTED, IAD_CONNECT, ADSKY

      ALLOCATE(IAD_CONNECT(NUMNOD_L + 1), NB_CONNECT(NUMNOD_L), ADSKY(NUMNOD_L))
      IF (ALE_CONNECTIVITY%has_nn_connect) THEN
         DO I = 1, NUMNOD_L
            IAD1 = ALE_CONNECTIVITY%NN_CONNECT%IAD_CONNECT(NODGLOB(I))
            IAD2 = ALE_CONNECTIVITY%NN_CONNECT%IAD_CONNECT(NODGLOB(I) + 1) - 1
            NB_CONNECT(I) = 0
            DO J = IAD1, IAD2
               IF (NODLOCAL(ALE_CONNECTIVITY%NN_CONNECT%CONNECTED(J)) > 0) THEN
                  NB_CONNECT(I) = NB_CONNECT(I) + 1
               ENDIF
            ENDDO
         ENDDO

         IAD_CONNECT(1) = 1
         DO I = 2, NUMNOD_L + 1
            IAD_CONNECT(I) = IAD_CONNECT(I - 1) + NB_CONNECT(I - 1)
         ENDDO

         LEN = IAD_CONNECT(NUMNOD_L + 1)
         ALLOCATE(CONNECTED(LEN))
         CONNECTED(1:LEN) = 0
         DO I = 1, NUMNOD_L
            ADSKY(I) = IAD_CONNECT(I)
         ENDDO
         

         DO I = 1, NUMNOD_L
            IAD1 = ALE_CONNECTIVITY%NN_CONNECT%IAD_CONNECT(NODGLOB(I))
            IAD2 = ALE_CONNECTIVITY%NN_CONNECT%IAD_CONNECT(NODGLOB(I) + 1) - 1
            DO J = IAD1, IAD2
               IF (NODLOCAL(ALE_CONNECTIVITY%NN_CONNECT%CONNECTED(J)) > 0) THEN
                  CONNECTED(ADSKY(I)) = NODLOCAL(ALE_CONNECTIVITY%NN_CONNECT%CONNECTED(J))
                  ADSKY(I) = ADSKY(I) + 1
               ENDIF
            ENDDO
         ENDDO

         CALL WRITE_I_C(IAD_CONNECT, NUMNOD_L + 1)
         CALL WRITE_I_C(CONNECTED, LEN)
         
         LEN_IA = LEN_IA + NUMNOD_L + 1 + LEN
      ENDIF

      IF (ALE_CONNECTIVITY%has_ne_connect) THEN
C     Node - element connectivity
         IF (ALLOCATED(CONNECTED)) DEALLOCATE(CONNECTED)
         DO I = 1, NUMNOD_L
            IAD1 = ALE_CONNECTIVITY%NE_CONNECT%IAD_CONNECT(NODGLOB(I))
            IAD2 = ALE_CONNECTIVITY%NE_CONNECT%IAD_CONNECT(NODGLOB(I) + 1) - 1
            NB_CONNECT(I) = 0
            DO J = IAD1, IAD2
               IF (ELEMID_L(ALE_CONNECTIVITY%NE_CONNECT%CONNECTED(J)) > 0) THEN
                  NB_CONNECT(I) = NB_CONNECT(I) + 1
               ENDIF
            ENDDO
         ENDDO

         IAD_CONNECT(1) = 1
         DO I = 2, NUMNOD_L + 1
            IAD_CONNECT(I) = IAD_CONNECT(I - 1) + NB_CONNECT(I - 1)
         ENDDO

         LEN = IAD_CONNECT(NUMNOD_L + 1)
         ALLOCATE(CONNECTED(LEN))

         DO I = 1, NUMNOD_L
            ADSKY(I) = IAD_CONNECT(I)
         ENDDO
         

         DO I = 1, NUMNOD_L
            IAD1 = ALE_CONNECTIVITY%NE_CONNECT%IAD_CONNECT(NODGLOB(I))
            IAD2 = ALE_CONNECTIVITY%NE_CONNECT%IAD_CONNECT(NODGLOB(I) + 1) - 1
            DO J = IAD1, IAD2
               IF (ELEMID_L(ALE_CONNECTIVITY%NE_CONNECT%CONNECTED(J)) > 0) THEN
                  CONNECTED(ADSKY(I)) = ELEMID_L(ALE_CONNECTIVITY%NE_CONNECT%CONNECTED(J))
                  ADSKY(I) = ADSKY(I) + 1
               ENDIF
            ENDDO
         ENDDO

         CALL WRITE_I_C(IAD_CONNECT, NUMNOD_L + 1)
         CALL WRITE_I_C(CONNECTED, LEN)
         
         LEN_IA = LEN_IA + NUMNOD_L + 1 + LEN
      ENDIF 

      IF (ALLOCATED(IAD_CONNECT)) DEALLOCATE(IAD_CONNECT)
      IF (ALLOCATED(CONNECTED)) DEALLOCATE(CONNECTED)
      IF (ALLOCATED(ADSKY)) DEALLOCATE(ADSKY)
      IF (ALLOCATED(NB_CONNECT)) DEALLOCATE(NB_CONNECT)

      RETURN
      END
