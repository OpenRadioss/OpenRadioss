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
Chd|  MULTI_DEALLOCATE              source/multifluid/multi_deallocate.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTI_DEALLOCATE(MULTI_FVM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MULTI_FVM_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C----------------------------------------------
C     D u m m y   a r g u m e n t s
C----------------------------------------------
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IERR, IMAT, NBMAT
C----------------------------------------------
C     B e g i n n i n g   o f   s u b r o u t i n e
C----------------------------------------------
C     Multifluid law
      IF (MULTI_FVM%IS_USED) THEN
         IF (ALLOCATED(MULTI_FVM%FLUXES)) DEALLOCATE(MULTI_FVM%FLUXES)
         IF (ALLOCATED(MULTI_FVM%SUBVOL_FLUXES)) DEALLOCATE(MULTI_FVM%SUBVOL_FLUXES)
         IF (ALLOCATED(MULTI_FVM%SUBMASS_FLUXES)) DEALLOCATE(MULTI_FVM%SUBMASS_FLUXES)
         IF (ALLOCATED(MULTI_FVM%SUBENER_FLUXES)) DEALLOCATE(MULTI_FVM%SUBENER_FLUXES)
         IF (ALLOCATED(MULTI_FVM%VEL)) DEALLOCATE(MULTI_FVM%VEL)
         IF (ALLOCATED(MULTI_FVM%SOUND_SPEED)) DEALLOCATE(MULTI_FVM%SOUND_SPEED)
         IF (ALLOCATED(MULTI_FVM%RHO)) DEALLOCATE(MULTI_FVM%RHO)
         IF (ALLOCATED(MULTI_FVM%EINT)) DEALLOCATE(MULTI_FVM%EINT)
         IF (ALLOCATED(MULTI_FVM%PRES)) DEALLOCATE(MULTI_FVM%PRES)
         IF (ALLOCATED(MULTI_FVM%TBURN)) DEALLOCATE(MULTI_FVM%TBURN)
         IF (ALLOCATED(MULTI_FVM%BFRAC)) DEALLOCATE(MULTI_FVM%BFRAC)
         IF (ALLOCATED(MULTI_FVM%VOL)) DEALLOCATE(MULTI_FVM%VOL)
C     MUSCL Variables
         IF (ALLOCATED(MULTI_FVM%GRAD_RHO)) DEALLOCATE(MULTI_FVM%GRAD_RHO)
         IF (ALLOCATED(MULTI_FVM%GRAD_U)) DEALLOCATE(MULTI_FVM%GRAD_U)
         IF (ALLOCATED(MULTI_FVM%GRAD_V)) DEALLOCATE(MULTI_FVM%GRAD_V)
         IF (ALLOCATED(MULTI_FVM%GRAD_W)) DEALLOCATE(MULTI_FVM%GRAD_W)
         IF (ALLOCATED(MULTI_FVM%GRAD_PRES)) DEALLOCATE(MULTI_FVM%GRAD_PRES)
C     Face data
         IF (ALLOCATED(MULTI_FVM%FACE_DATA%SURF)) DEALLOCATE(MULTI_FVM%FACE_DATA%SURF)
         IF (ALLOCATED(MULTI_FVM%FACE_DATA%NORMAL)) DEALLOCATE(MULTI_FVM%FACE_DATA%NORMAL)
         IF (ALLOCATED(MULTI_FVM%FACE_DATA%WFAC)) DEALLOCATE(MULTI_FVM%FACE_DATA%WFAC)
C     Phase data
         NBMAT = MULTI_FVM%NBMAT
         IF (NBMAT > 1) THEN
            IF (ALLOCATED(MULTI_FVM%PHASE_ALPHA)) 
     .           DEALLOCATE(MULTI_FVM%PHASE_ALPHA)
            IF (ALLOCATED(MULTI_FVM%PHASE_PRES)) 
     .           DEALLOCATE(MULTI_FVM%PHASE_PRES)
            IF (ALLOCATED(MULTI_FVM%PHASE_RHO)) 
     .           DEALLOCATE(MULTI_FVM%PHASE_RHO)
            IF (ALLOCATED(MULTI_FVM%PHASE_EINT)) 
     .           DEALLOCATE(MULTI_FVM%PHASE_EINT)
            
            IF (MULTI_FVM%MUSCL > 0) THEN
               IF (ALLOCATED(MULTI_FVM%PHASE_GRAD_RHO)) 
     .              DEALLOCATE(MULTI_FVM%PHASE_GRAD_RHO)
               IF (ALLOCATED(MULTI_FVM%PHASE_GRAD_ALPHA)) 
     .              DEALLOCATE(MULTI_FVM%PHASE_GRAD_ALPHA)
               IF (ALLOCATED(MULTI_FVM%PHASE_GRAD_PRES)) 
     .              DEALLOCATE(MULTI_FVM%PHASE_GRAD_PRES)
            ENDIF
         ENDIF
         IF (ALLOCATED(MULTI_FVM%FVM_CONNECTIVITY%KVOIS))
     .        DEALLOCATE(MULTI_FVM%FVM_CONNECTIVITY%KVOIS)
        
      ENDIF
C----------------------------------------------
C     E n d   o f   s u b r o u t i n e
C----------------------------------------------
      END SUBROUTINE MULTI_DEALLOCATE
