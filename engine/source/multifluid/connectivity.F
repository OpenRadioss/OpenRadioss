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
Chd|====================================================================
Chd|  BUILD_CONNECTIVITY            source/multifluid/connectivity.F
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE BUILD_CONNECTIVITY(ITASK, MULTI_FVM, ALE_CONNECTIVITY, IPARG)
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
C     Builds array KVOIS that contains the number kface2 of
C     the face by which an element J is a neighbor of an 
C     element I of tab IVOIS.
C     If I is an element and KFACE a face number, if J the neighbor
C     of I by face KFACE, that is to say J = IVOIS (KFACE, I)
C     then, KFACE2 is such that I = IVOIS (KFACE2, J)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE MULTI_FVM_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
! NGROUP
#include      "com01_c.inc"
! NUMELQ, NUMELS, NUMELTG
#include      "com04_c.inc"
! NTHREAD
#include      "task_c.inc"
! NPARG
#include      "param_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      INTEGER, INTENT(IN) :: ITASK
      INTEGER, INTENT(IN) :: IPARG(NPARG, *)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NB_FACE, KFACE, KFACE2, II, I, J, MATLAW
      INTEGER :: ITY, NFT, NEL, NG, NUMEL_SPMD
      LOGICAL :: IDBG 
      INTEGER :: IAD, IAD2, LGTH, LGTH2

      DO NG = ITASK + 1, NGROUP, NTHREAD
         MATLAW = IPARG(1, NG)
         IF (MATLAW == 151) THEN
            NEL = IPARG(2, NG)
            NFT = IPARG(3, NG)
            ITY = IPARG(5, NG)
            SELECT CASE (MULTI_FVM%SYM)
            CASE (0)
               NB_FACE = 6
               NUMEL_SPMD = NUMELS
            CASE (1, 2)
               IF (ITY == 2) THEN
C     QUADS
                  NB_FACE = 4
                  NUMEL_SPMD = NUMELQ
               ELSEIF (ITY == 7) THEN
C     TRIANGLES
                  NB_FACE = 3
                  NUMEL_SPMD = NUMELTG
               ENDIF
            CASE DEFAULT
               CALL ARRET(2)
            END SELECT                  
            DO II = 1, NEL
               I = II + NFT
               IAD = ALE_CONNECTIVITY%ee_connect%iad_connect(I)
               LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(I+1) - 
     .              ALE_CONNECTIVITY%ee_connect%iad_connect(I)
               DO KFACE = 1, LGTH
                  J = ALE_CONNECTIVITY%ee_connect%connected(IAD + KFACE - 1)
                  IF (J > 0 .AND. J <= NUMEL_SPMD) THEN
                     IAD2 = ALE_CONNECTIVITY%ee_connect%iad_connect(J)
                     LGTH2 = ALE_CONNECTIVITY%ee_connect%iad_connect(J+1) - 
     .                    ALE_CONNECTIVITY%ee_connect%iad_connect(J)
                     DO KFACE2 = 1, LGTH2
                        IF (I == ALE_CONNECTIVITY%ee_connect%connected(IAD2 + KFACE2 - 1)) THEN
                           EXIT
                        ENDIF
                     ENDDO
                     MULTI_FVM%FVM_CONNECTIVITY%KVOIS(NB_FACE * (I - 1) + KFACE) = KFACE2
                  ENDIF
               ENDDO
            ENDDO
         ENDIF
      ENDDO

      IDBG = .FALSE.
C     Verification (debug only)
      IF (IDBG) THEN
         DO NG = ITASK + 1, NGROUP, NTHREAD
            MATLAW = IPARG(1, NG)
            IF (MATLAW == 151) THEN
               NEL = IPARG(2, NG)
               NFT = IPARG(3, NG)
               ITY = IPARG(5, NG)
               SELECT CASE (MULTI_FVM%SYM)
               CASE (0)
                  NB_FACE = 6
                  NUMEL_SPMD = NUMELS
               CASE (1, 2)
                  IF (ITY == 2) THEN
C     QUADS
                     NB_FACE = 4
                     NUMEL_SPMD = NUMELQ
                  ELSEIF (ITY == 7) THEN
C     TRIANGLES
                     NB_FACE = 3
                     NUMEL_SPMD = NUMELTG
                  ENDIF
               CASE DEFAULT
                  CALL ARRET(2)
               END SELECT                  
               DO II = 1, NEL
                  I = II + NFT
                  IAD = ALE_CONNECTIVITY%ee_connect%iad_connect(I)
                  LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(I+1) - 
     .                 ALE_CONNECTIVITY%ee_connect%iad_connect(I)
                  DO KFACE = 1, LGTH
                     J = ALE_CONNECTIVITY%ee_connect%connected(IAD + KFACE - 1)
                     IF (J> 0 .AND. J <= NUMEL_SPMD) THEN
                        IAD2 = ALE_CONNECTIVITY%ee_connect%iad_connect(J)
                        KFACE2 = MULTI_FVM%FVM_CONNECTIVITY%KVOIS(NB_FACE * (I - 1) + KFACE)
                        IF (ALE_CONNECTIVITY%ee_connect%connected(IAD2 + KFACE2 - 1) /= I) THEN
                           PRINT*, "OUPS"
                        ENDIF
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
         ENDDO         
      ENDIF
  
      END SUBROUTINE BUILD_CONNECTIVITY
