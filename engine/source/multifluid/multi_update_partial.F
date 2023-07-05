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
Chd|  MULTI_UPDATE_PARTIAL          source/multifluid/multi_update_partial.F
Chd|-- called by -----------
Chd|        MULTI_TIMEEVOLUTION           source/multifluid/multi_timeevolution.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTI_UPDATE_PARTIAL(LOCAL_MATID, PM, IMAT, NG, ELBUF_TAB, 
     .     IPARG, ITASK, MULTI_FVM, VOLNEW)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD      
      USE ELBUFDEF_MOD
      USE MULTI_FVM_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IMAT, NG, LOCAL_MATID
      my_real, INTENT(IN) :: PM(NPROPM, *), VOLNEW(*)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      INTEGER, INTENT(IN) :: IPARG(NPARG, *)
      INTEGER, INTENT(IN) :: ITASK ! SMP TASK
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE(G_BUFEL_), POINTER :: GBUF
      TYPE(L_BUFEL_), POINTER :: LBUF
      INTEGER :: II, I, NEL, NFT
      my_real :: TMP2, TMP3, TMP4, RHO0, E0
C-----------------------------------------------
C     B e g i n n i n g   o f   s u b r o u t i n e
C-----------------------------------------------
      GBUF => ELBUF_TAB(NG)%GBUF
      LBUF => ELBUF_TAB(NG)%BUFLY(IMAT)%LBUF(1, 1, 1)
      RHO0 = PM(1, LOCAL_MATID)
      E0 = PM(23, LOCAL_MATID)
      NEL = IPARG(2, NG)
      NFT = IPARG(3, NG)

      DO II = 1, NEL
         I = II + NFT
C     Volume
         TMP4 = MULTI_FVM%PHASE_ALPHA(IMAT, I)
C     Mass
         TMP2 = MULTI_FVM%PHASE_RHO(IMAT, I)
C     Mass * e specifique
         TMP3 = MULTI_FVM%PHASE_EINT(IMAT, I)
         
C     Filtrage
         IF (TMP4 / GBUF%VOL(II) < EM08) THEN
            MULTI_FVM%PHASE_ALPHA(IMAT, I) = ZERO
            MULTI_FVM%PHASE_RHO(IMAT, I) = RHO0
            MULTI_FVM%PHASE_EINT(IMAT, I) = E0
         ELSE
            IF (TMP2 < ZERO) THEN
               MULTI_FVM%PHASE_RHO(IMAT, I) = LBUF%RHO(II)
               MULTI_FVM%PHASE_EINT(IMAT, I) = LBUF%EINT(II)
               MULTI_FVM%PHASE_ALPHA(IMAT, I) = TMP4 / VOLNEW(II)
            ELSE
C     Mass density
               MULTI_FVM%PHASE_RHO(IMAT, I) = TMP2 / TMP4
C     INternal energy
               MULTI_FVM%PHASE_EINT(IMAT, I) = TMP3 / TMP4
C     Volume fraction
               MULTI_FVM%PHASE_ALPHA(IMAT, I) = TMP4 / VOLNEW(II)
            ENDIF
         ENDIF
      ENDDO
C-----------------------------------------------
C     E n d  o f   s u b r o u t i n e
C-----------------------------------------------
      END SUBROUTINE MULTI_UPDATE_PARTIAL
