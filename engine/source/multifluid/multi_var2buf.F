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
Chd|  MULTI_VAR2BUF                 source/multifluid/multi_var2buf.F
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTI_VAR2BUF(ITASK, IPARG, MULTI_FVM, ELBUF_TAB)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE MULTI_FVM_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
C     NPARG
#include      "param_c.inc"
C     NGROUP
#include      "com01_c.inc"
C     NTHREAD
#include      "task_c.inc"
C     MVSIZ
#include      "mvsiz_p.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ITASK, IPARG(NPARG, *)
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP), INTENT(INOUT) :: ELBUF_TAB
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NBMAT, IMAT, II, I, NG, MATLAW, NEL, NFT
      my_real :: VOL, CUMUL_ALPHA(MVSIZ)
      TYPE(G_BUFEL_), POINTER :: GBUF
      TYPE(L_BUFEL_), POINTER :: LBUF

      NBMAT = MULTI_FVM%NBMAT
      DO NG = ITASK + 1, NGROUP, NTHREAD
         MATLAW = IPARG(1, NG)
         IF (MATLAW == 151) THEN
            NEL = IPARG(2, NG)
            NFT = IPARG(3, NG)
            GBUF => ELBUF_TAB(NG)%GBUF
            DO II = 1, NEL
               I = II + NFT
C     Mass density
               GBUF%RHO(II) = MULTI_FVM%RHO(I)
C     Volumic internal energy
               GBUF%EINT(II) = MULTI_FVM%EINT(I)
C     Velocity
               GBUF%MOM(II + 0 * NEL) = MULTI_FVM%VEL(1, I)
               GBUF%MOM(II + 1 * NEL) = MULTI_FVM%VEL(2, I)
               GBUF%MOM(II + 2 * NEL) = MULTI_FVM%VEL(3, I)
               GBUF%SIG(II + 0 * NEL) = -MULTI_FVM%PRES(I)
               GBUF%SIG(II + 1 * NEL) = -MULTI_FVM%PRES(I)
               GBUF%SIG(II + 2 * NEL) = -MULTI_FVM%PRES(I)
            ENDDO
            IF (NBMAT > 1) THEN
               CUMUL_ALPHA(1:NEL) = ZERO
C     Fill partial values
               DO IMAT = 1, NBMAT
                  DO II = 1, NEL
                     I = II + NFT
                     CUMUL_ALPHA(II) = CUMUL_ALPHA(II) + MULTI_FVM%PHASE_ALPHA(IMAT, I)
                  ENDDO
               ENDDO
               DO IMAT = 1, NBMAT
                  LBUF => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1, 1, 1)
                  DO II = 1, NEL
                     I = II + NFT 
                     VOL = GBUF%VOL(II)
C     Normalize volumic fraction
                     MULTI_FVM%PHASE_ALPHA(IMAT, I) = MULTI_FVM%PHASE_ALPHA(IMAT, I) / 
     .                    CUMUL_ALPHA(II)
C     Volume
                     LBUF%VOL(II) = MULTI_FVM%PHASE_ALPHA(IMAT, I) * VOL
C     Mass density
                     LBUF%RHO(II) = MULTI_FVM%PHASE_RHO(IMAT, I)
C     Volumic internal energy
                     LBUF%EINT(II) = MULTI_FVM%PHASE_EINT(IMAT, I)
C     PRessure
                     LBUF%SIG(II + 0 * NEL) = -MULTI_FVM%PHASE_PRES(IMAT, I)
                     LBUF%SIG(II + 1 * NEL) = -MULTI_FVM%PHASE_PRES(IMAT, I)
                     LBUF%SIG(II + 2 * NEL) = -MULTI_FVM%PHASE_PRES(IMAT, I)
                  ENDDO
               ENDDO
CCCCC 
               IF (MULTI_FVM%MUSCL > 0) THEN
                  DO II = 1, NEL
                     I = II + NFT
                     MULTI_FVM%RHO(I) = ZERO
                  ENDDO
                  DO IMAT = 1, NBMAT
                     DO II = 1, NEL
                        I = II + NFT
                        MULTI_FVM%RHO(I) = MULTI_FVM%RHO(I) + 
     .                       MULTI_FVM%PHASE_ALPHA(IMAT, I) * MULTI_FVM%PHASE_RHO(IMAT, I)
                     ENDDO
                  ENDDO
               ENDIF
            ENDIF
         ENDIF !IF (MATLAW == 151) THEN
      ENDDO  ! NG
      END SUBROUTINE MULTI_VAR2BUF
