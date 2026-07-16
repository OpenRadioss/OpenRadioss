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
!||    sts_voxel_driver_mod   ../engine/source/interfaces/ists/ists_voxel_driver.F90
!||--- called by ------------------------------------------------------
!||    i7mainf                ../engine/source/interfaces/int07/i7mainf.F
!||--- uses       -----------------------------------------------------
!||    sts_broad_phase_voxel_mod   ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||====================================================================
!
!   STS voxel-broad-phase driver.
!
!   This module is the single entry point to query whether the hardcoded 
!   /SURF user IDs 100 (secondary) and 200 (master) exist in the model 
!   and to resolve them to internal IGRSURF indices.
!
!   The caller (i7mainf) uses
!   STS_VOXEL_BROAD_PHASE from STS_BROAD_PHASE_VOXEL_MOD to populate
!   the existing CAND_*_SEG_ID and CONT_ELEMENT working arrays, then
!   calls STS_CONTACTS_ASSEMBLE and I7ASS33 in place. 
!
      MODULE STS_VOXEL_DRIVER_MOD
        USE GROUPDEF_MOD, ONLY : SURF_
        IMPLICIT NONE
        PRIVATE
!-----------------------------------------------------------------------
!       Hardcoded STS surface user IDs (must be present in the input deck)
!-----------------------------------------------------------------------
        INTEGER, PARAMETER, PUBLIC :: STS_VOXEL_SEC_SURF_USER_ID = 100
        INTEGER, PARAMETER, PUBLIC :: STS_VOXEL_MST_SURF_USER_ID = 200
!-----------------------------------------------------------------------
!       One-time error flag (avoid spamming the log every cycle)
!-----------------------------------------------------------------------
        LOGICAL, SAVE :: STS_VOXEL_ERROR_DONE = .FALSE.
!-----------------------------------------------------------------------
        PUBLIC :: STS_VOXEL_DRIVER_RESOLVE_IDS
        PUBLIC :: STS_VOXEL_DRIVER_DATA_READY
!
      CONTAINS
!=======================================================================
!   STS_VOXEL_DRIVER_RESOLVE_IDS
!
!   Walk IGRSURF once and return the internal indices of the surfaces
!   whose user IDs match STS_VOXEL_SEC_SURF_USER_ID (100) and
!   STS_VOXEL_MST_SURF_USER_ID (200). Either output is set to 0 when
!   the matching surface does not exist.
!=======================================================================
        SUBROUTINE STS_VOXEL_DRIVER_RESOLVE_IDS(IGRSURF, NSURF, &
     &      SEC_SURF_IDX, MST_SURF_IDX)
          INTEGER, INTENT(IN) :: NSURF
          TYPE(SURF_), DIMENSION(NSURF), INTENT(IN) :: IGRSURF
          INTEGER, INTENT(OUT) :: SEC_SURF_IDX, MST_SURF_IDX
!
          INTEGER :: I
!
          SEC_SURF_IDX = 0
          MST_SURF_IDX = 0
          DO I = 1, NSURF
            IF (IGRSURF(I)%ID == STS_VOXEL_SEC_SURF_USER_ID) SEC_SURF_IDX = I
            IF (IGRSURF(I)%ID == STS_VOXEL_MST_SURF_USER_ID) MST_SURF_IDX = I
          END DO
!
        END SUBROUTINE STS_VOXEL_DRIVER_RESOLVE_IDS
!=======================================================================
!   STS_VOXEL_DRIVER_DATA_READY
!
!   Return .TRUE. when both /SURF user IDs 100 and 200 exist and have
!   non-empty segment lists. On the first call where the data is not
!   ready (and only when MAY_PRINT is .TRUE.), a clear one-time error
!   is emitted to unit 6 identifying the missing/empty surface.
!
!   Outputs SEC_SURF_IDX/MST_SURF_IDX are always populated with the
!   resolved indices (0 when missing) so the caller can continue to
!   inspect them after a failure.
!=======================================================================
        LOGICAL FUNCTION STS_VOXEL_DRIVER_DATA_READY( &
     &      IGRSURF, NSURF, NOINT, MAY_PRINT, &
     &      SEC_SURF_IDX, MST_SURF_IDX) RESULT(IS_READY)
          INTEGER, INTENT(IN) :: NSURF, NOINT
          TYPE(SURF_), DIMENSION(NSURF), INTENT(IN) :: IGRSURF
          LOGICAL, INTENT(IN) :: MAY_PRINT
          INTEGER, INTENT(OUT) :: SEC_SURF_IDX, MST_SURF_IDX
!
          LOGICAL :: SEC_OK, MST_OK
!
          IS_READY = .FALSE.
          CALL STS_VOXEL_DRIVER_RESOLVE_IDS(IGRSURF, NSURF, &
     &        SEC_SURF_IDX, MST_SURF_IDX)
!
          SEC_OK = (SEC_SURF_IDX > 0)
          IF (SEC_OK) SEC_OK = IGRSURF(SEC_SURF_IDX)%NSEG > 0
          MST_OK = (MST_SURF_IDX > 0)
          IF (MST_OK) MST_OK = IGRSURF(MST_SURF_IDX)%NSEG > 0
!
          IF (SEC_OK .AND. MST_OK) THEN
            IS_READY = .TRUE.
            RETURN
          END IF
!
          IF (MAY_PRINT .AND. .NOT. STS_VOXEL_ERROR_DONE) THEN
            WRITE(6,*) ' '
            WRITE(6,*) ' ** ERROR (STS): hardcoded /SURF user IDs missing'
            WRITE(6,*) '    Required: secondary user ID =', &
     &                 STS_VOXEL_SEC_SURF_USER_ID, &
     &                 ', master user ID =', STS_VOXEL_MST_SURF_USER_ID
            IF (SEC_SURF_IDX <= 0) THEN
              WRITE(6,*) '    Secondary surface (user ID =', &
     &                   STS_VOXEL_SEC_SURF_USER_ID, &
     &                   ') not found in the model.'
            ELSE IF (.NOT. SEC_OK) THEN
              WRITE(6,*) '    Secondary surface user ID =', &
     &                   STS_VOXEL_SEC_SURF_USER_ID, ' has 0 segments.'
            END IF
            IF (MST_SURF_IDX <= 0) THEN
              WRITE(6,*) '    Master surface (user ID =', &
     &                   STS_VOXEL_MST_SURF_USER_ID, &
     &                   ') not found in the model.'
            ELSE IF (.NOT. MST_OK) THEN
              WRITE(6,*) '    Master surface user ID =', &
     &                   STS_VOXEL_MST_SURF_USER_ID, ' has 0 segments.'
            END IF
            WRITE(6,*) '    STS contact disabled for INT7 interface NOINT=', &
     &                 NOINT
            STS_VOXEL_ERROR_DONE = .TRUE.
          END IF
!
        END FUNCTION STS_VOXEL_DRIVER_DATA_READY
!
      END MODULE STS_VOXEL_DRIVER_MOD
