Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    multi_deallocate   ../engine/source/multifluid/multi_deallocate.F
!||--- called by ------------------------------------------------------
!||    resol              ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    message_mod        ../engine/share/message_module/message_mod.F
!||    multi_fvm_mod      ../common_source/modules/ale/multi_fvm_mod.F90
!||    my_dealloc_mod     ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
      SUBROUTINE MULTI_DEALLOCATE(MULTI_FVM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MULTI_FVM_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      use my_dealloc_mod, only : my_dealloc
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
      INTEGER :: NBMAT
C----------------------------------------------
C     B e g i n n i n g   o f   s u b r o u t i n e
C----------------------------------------------
C     Multifluid law
      IF (MULTI_FVM%IS_USED) THEN
         IF (ALLOCATED(MULTI_FVM%FLUXES)) call my_dealloc(MULTI_FVM%FLUXES)
         IF (ALLOCATED(MULTI_FVM%SUBVOL_FLUXES)) call my_dealloc(MULTI_FVM%SUBVOL_FLUXES)
         IF (ALLOCATED(MULTI_FVM%SUBMASS_FLUXES)) call my_dealloc(MULTI_FVM%SUBMASS_FLUXES)
         IF (ALLOCATED(MULTI_FVM%SUBENER_FLUXES)) call my_dealloc(MULTI_FVM%SUBENER_FLUXES)
         IF (ALLOCATED(MULTI_FVM%VEL)) call my_dealloc(MULTI_FVM%VEL)
         IF (ALLOCATED(MULTI_FVM%ACC)) call my_dealloc(MULTI_FVM%ACC)
         IF (ALLOCATED(MULTI_FVM%SOUND_SPEED)) call my_dealloc(MULTI_FVM%SOUND_SPEED)
         IF (ALLOCATED(MULTI_FVM%RHO)) call my_dealloc(MULTI_FVM%RHO)
         IF (ALLOCATED(MULTI_FVM%EINT)) call my_dealloc(MULTI_FVM%EINT)
         IF (ALLOCATED(MULTI_FVM%PRES)) call my_dealloc(MULTI_FVM%PRES)
         IF (ALLOCATED(MULTI_FVM%TBURN)) call my_dealloc(MULTI_FVM%TBURN)
         IF (ALLOCATED(MULTI_FVM%BFRAC)) call my_dealloc(MULTI_FVM%BFRAC)
         IF (ALLOCATED(MULTI_FVM%VOL)) call my_dealloc(MULTI_FVM%VOL)
C     MUSCL Variables
         IF (ALLOCATED(MULTI_FVM%GRAD_RHO)) call my_dealloc(MULTI_FVM%GRAD_RHO)
         IF (ALLOCATED(MULTI_FVM%GRAD_U)) call my_dealloc(MULTI_FVM%GRAD_U)
         IF (ALLOCATED(MULTI_FVM%GRAD_V)) call my_dealloc(MULTI_FVM%GRAD_V)
         IF (ALLOCATED(MULTI_FVM%GRAD_W)) call my_dealloc(MULTI_FVM%GRAD_W)
         IF (ALLOCATED(MULTI_FVM%GRAD_PRES)) call my_dealloc(MULTI_FVM%GRAD_PRES)
C     Face data
         IF (ALLOCATED(MULTI_FVM%FACE_DATA%SURF)) call my_dealloc(MULTI_FVM%FACE_DATA%SURF)
         IF (ALLOCATED(MULTI_FVM%FACE_DATA%NORMAL)) call my_dealloc(MULTI_FVM%FACE_DATA%NORMAL)
         IF (ALLOCATED(MULTI_FVM%FACE_DATA%WFAC)) call my_dealloc(MULTI_FVM%FACE_DATA%WFAC)
C     Phase data
         NBMAT = MULTI_FVM%NBMAT
         IF (NBMAT > 1) THEN
            IF (ALLOCATED(MULTI_FVM%PHASE_ALPHA)) call my_dealloc(MULTI_FVM%PHASE_ALPHA)
            IF (ALLOCATED(MULTI_FVM%PHASE_PRES)) call my_dealloc(MULTI_FVM%PHASE_PRES)
            IF (ALLOCATED(MULTI_FVM%PHASE_RHO)) call my_dealloc(MULTI_FVM%PHASE_RHO)
            IF (ALLOCATED(MULTI_FVM%PHASE_EINT)) call my_dealloc(MULTI_FVM%PHASE_EINT)
            
            IF (MULTI_FVM%MUSCL > 0) THEN
               IF (ALLOCATED(MULTI_FVM%PHASE_GRAD_RHO)) call my_dealloc(MULTI_FVM%PHASE_GRAD_RHO)
               IF (ALLOCATED(MULTI_FVM%PHASE_GRAD_ALPHA)) call my_dealloc(MULTI_FVM%PHASE_GRAD_ALPHA)
               IF (ALLOCATED(MULTI_FVM%PHASE_GRAD_PRES)) call my_dealloc(MULTI_FVM%PHASE_GRAD_PRES)
            ENDIF
         ENDIF
         IF (ALLOCATED(MULTI_FVM%FVM_CONNECTIVITY%KVOIS)) call my_dealloc(MULTI_FVM%FVM_CONNECTIVITY%KVOIS)
        
      ENDIF
C----------------------------------------------
C     E n d   o f   s u b r o u t i n e
C----------------------------------------------
      END SUBROUTINE MULTI_DEALLOCATE
