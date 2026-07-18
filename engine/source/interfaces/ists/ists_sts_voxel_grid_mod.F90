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
!
!   Per-interface voxel broad-phase parameters. Static mesh/gap tolerance
!   and CELL_SIZE are frozen on first broad-phase call; SEARCH_PADDING is
!   refreshed each cycle with the kinematic VMAXDT term.
!
!||====================================================================
!||    ists_sts_voxel_grid_mod        ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||--- called by ------------------------------------------------------
!||    ists_mainf                     ../engine/source/interfaces/ists/ists_mainf.F90
!||    sts_broad_phase_voxel_mod      ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||--- calls      -----------------------------------------------------
!||    ists_sts_voxel_grid_is_ready   ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||--- uses       -----------------------------------------------------
!||    precision_mod                  ../common_source/modules/precision_mod.F90
!||====================================================================
      MODULE ISTS_STS_VOXEL_GRID_MOD
        USE PRECISION_MOD, ONLY : WP
        IMPLICIT NONE
        PRIVATE

        TYPE :: STS_VOXEL_GRID_STATE
          LOGICAL :: initialized = .FALSE.
          REAL(KIND=WP) :: tol_static = 0.0_WP
          REAL(KIND=WP) :: cell_size = 0.0_WP
          REAL(KIND=WP) :: search_padding = 0.0_WP
          REAL(KIND=WP) :: pad_sq = 0.0_WP
          INTEGER :: n_cell_radius = 1
        END TYPE STS_VOXEL_GRID_STATE

        TYPE(STS_VOXEL_GRID_STATE), ALLOCATABLE, SAVE :: STS_VOXEL_GRID(:)

        PUBLIC :: ISTS_STS_VOXEL_GRID_IS_READY
        PUBLIC :: ISTS_STS_VOXEL_GRID_GET
        PUBLIC :: ISTS_STS_VOXEL_GRID_INIT
        PUBLIC :: ISTS_STS_VOXEL_GRID_UPDATE_DYNAMIC
        PUBLIC :: ISTS_STS_VOXEL_GRID_GET_TOL_STATIC

      CONTAINS
!=======================================================================
!   ISTS_STS_VOXEL_GRID_ENSURE_SIZE
!=======================================================================
!||====================================================================
!||    ists_sts_voxel_grid_ensure_size   ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||--- called by ------------------------------------------------------
!||    ists_sts_voxel_grid_init          ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||--- calls      -----------------------------------------------------
!||====================================================================
        SUBROUTINE ISTS_STS_VOXEL_GRID_ENSURE_SIZE(NIN)
          INTEGER, INTENT(IN) :: NIN
          TYPE(STS_VOXEL_GRID_STATE), ALLOCATABLE :: TMP(:)
          INTEGER :: OLD_SIZE, I

          IF (NIN <= 0) RETURN
          IF (.NOT. ALLOCATED(STS_VOXEL_GRID)) THEN
            ALLOCATE(STS_VOXEL_GRID(NIN))
            DO I = 1, NIN
              STS_VOXEL_GRID(I)%initialized = .FALSE.
              STS_VOXEL_GRID(I)%tol_static = 0.0_WP
              STS_VOXEL_GRID(I)%cell_size = 0.0_WP
              STS_VOXEL_GRID(I)%search_padding = 0.0_WP
              STS_VOXEL_GRID(I)%pad_sq = 0.0_WP
              STS_VOXEL_GRID(I)%n_cell_radius = 1
            END DO
            RETURN
          END IF
          OLD_SIZE = SIZE(STS_VOXEL_GRID)
          IF (NIN <= OLD_SIZE) RETURN
          ALLOCATE(TMP(NIN))
          IF (OLD_SIZE > 0) TMP(1:OLD_SIZE) = STS_VOXEL_GRID(1:OLD_SIZE)
          DO I = OLD_SIZE + 1, NIN
            TMP(I)%initialized = .FALSE.
            TMP(I)%tol_static = 0.0_WP
            TMP(I)%cell_size = 0.0_WP
            TMP(I)%search_padding = 0.0_WP
            TMP(I)%pad_sq = 0.0_WP
            TMP(I)%n_cell_radius = 1
          END DO
          DEALLOCATE(STS_VOXEL_GRID)
          CALL MOVE_ALLOC(TMP, STS_VOXEL_GRID)
        END SUBROUTINE ISTS_STS_VOXEL_GRID_ENSURE_SIZE

!=======================================================================
!   ISTS_STS_VOXEL_GRID_IS_READY
!=======================================================================
!||====================================================================
!||    ists_sts_voxel_grid_is_ready   ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||--- called by ------------------------------------------------------
!||    ists_mainf                     ../engine/source/interfaces/ists/ists_mainf.F90
!||    ists_sts_voxel_grid_mod        ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||    sts_voxel_broad_phase          ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||    sts_voxel_init_grid_params     ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||====================================================================
        LOGICAL FUNCTION ISTS_STS_VOXEL_GRID_IS_READY(NIN)
          INTEGER, INTENT(IN) :: NIN

          ISTS_STS_VOXEL_GRID_IS_READY = .FALSE.
          IF (NIN <= 0) RETURN
          IF (.NOT. ALLOCATED(STS_VOXEL_GRID)) RETURN
          IF (NIN > SIZE(STS_VOXEL_GRID)) RETURN
          ISTS_STS_VOXEL_GRID_IS_READY = STS_VOXEL_GRID(NIN)%initialized
        END FUNCTION ISTS_STS_VOXEL_GRID_IS_READY

!=======================================================================
!   ISTS_STS_VOXEL_GRID_GET
!=======================================================================
!||====================================================================
!||    ists_sts_voxel_grid_get   ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||--- called by ------------------------------------------------------
!||    ists_mainf                ../engine/source/interfaces/ists/ists_mainf.F90
!||    sts_voxel_pair_search     ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||====================================================================
        SUBROUTINE ISTS_STS_VOXEL_GRID_GET(NIN, CELL_SIZE, SEARCH_PADDING, &
     &    PAD_SQ, N_CELL_RADIUS)
          INTEGER, INTENT(IN) :: NIN
          INTEGER, INTENT(INOUT) :: N_CELL_RADIUS
          REAL(KIND=WP), INTENT(INOUT) :: CELL_SIZE, SEARCH_PADDING, PAD_SQ

          CELL_SIZE = STS_VOXEL_GRID(NIN)%cell_size
          SEARCH_PADDING = STS_VOXEL_GRID(NIN)%search_padding
          PAD_SQ = STS_VOXEL_GRID(NIN)%pad_sq
          N_CELL_RADIUS = STS_VOXEL_GRID(NIN)%n_cell_radius
        END SUBROUTINE ISTS_STS_VOXEL_GRID_GET

!=======================================================================
!   ISTS_STS_VOXEL_GRID_GET_TOL_STATIC
!=======================================================================
!||====================================================================
!||    ists_sts_voxel_grid_get_tol_static   ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||--- called by ------------------------------------------------------
!||    sts_voxel_broad_phase                ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||====================================================================
        SUBROUTINE ISTS_STS_VOXEL_GRID_GET_TOL_STATIC(NIN, TOL_STATIC)
          INTEGER, INTENT(IN) :: NIN
          REAL(KIND=WP), INTENT(INOUT) :: TOL_STATIC

          TOL_STATIC = STS_VOXEL_GRID(NIN)%tol_static
        END SUBROUTINE ISTS_STS_VOXEL_GRID_GET_TOL_STATIC

!=======================================================================
!   ISTS_STS_VOXEL_GRID_INIT
!   Static tolerance and voxel cell size on first broad-phase call.
!=======================================================================
!||====================================================================
!||    ists_sts_voxel_grid_init             ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||--- called by ------------------------------------------------------
!||    sts_voxel_init_grid_params           ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||--- calls      -----------------------------------------------------
!||    ists_sts_voxel_grid_ensure_size      ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||    ists_sts_voxel_grid_update_dynamic   ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||====================================================================
        SUBROUTINE ISTS_STS_VOXEL_GRID_INIT(NIN, TOL_STATIC, CELL_SIZE)
          INTEGER, INTENT(IN) :: NIN
          REAL(KIND=WP), INTENT(IN) :: TOL_STATIC, CELL_SIZE

          CALL ISTS_STS_VOXEL_GRID_ENSURE_SIZE(NIN)
          STS_VOXEL_GRID(NIN)%tol_static = TOL_STATIC
          STS_VOXEL_GRID(NIN)%cell_size = CELL_SIZE
          STS_VOXEL_GRID(NIN)%initialized = .TRUE.
          CALL ISTS_STS_VOXEL_GRID_UPDATE_DYNAMIC(NIN, TOL_STATIC)
        END SUBROUTINE ISTS_STS_VOXEL_GRID_INIT

!=======================================================================
!   ISTS_STS_VOXEL_GRID_UPDATE_DYNAMIC
!   Refresh search padding, pair cutoff, and voxel neighborhood radius.
!=======================================================================
!||====================================================================
!||    ists_sts_voxel_grid_update_dynamic   ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||--- called by ------------------------------------------------------
!||    ists_sts_voxel_grid_init             ../engine/source/interfaces/ists/ists_sts_voxel_grid_mod.F90
!||    sts_voxel_broad_phase                ../engine/source/interfaces/ists/ists_broad_phase_voxel.F90
!||====================================================================
        SUBROUTINE ISTS_STS_VOXEL_GRID_UPDATE_DYNAMIC(NIN, SEARCH_PADDING)
          INTEGER, INTENT(IN) :: NIN
          REAL(KIND=WP), INTENT(IN) :: SEARCH_PADDING

          IF (NIN <= 0) RETURN
          IF (.NOT. ALLOCATED(STS_VOXEL_GRID)) RETURN
          IF (NIN > SIZE(STS_VOXEL_GRID)) RETURN
          IF (.NOT. STS_VOXEL_GRID(NIN)%initialized) RETURN

          STS_VOXEL_GRID(NIN)%search_padding = SEARCH_PADDING
          STS_VOXEL_GRID(NIN)%pad_sq = SEARCH_PADDING * SEARCH_PADDING
          IF (STS_VOXEL_GRID(NIN)%cell_size > 0.0_WP) THEN
            STS_VOXEL_GRID(NIN)%n_cell_radius = MAX(1, &
     &        CEILING(SEARCH_PADDING / STS_VOXEL_GRID(NIN)%cell_size))
          ELSE
            STS_VOXEL_GRID(NIN)%n_cell_radius = 1
          END IF
        END SUBROUTINE ISTS_STS_VOXEL_GRID_UPDATE_DYNAMIC

      END MODULE ISTS_STS_VOXEL_GRID_MOD
