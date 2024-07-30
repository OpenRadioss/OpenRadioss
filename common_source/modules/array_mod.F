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
      !||    array_mod                                ../common_source/modules/array_mod.F
      !||--- called by ------------------------------------------------------
      !||    ale_box_coloration                       ../starter/source/initial_conditions/inivol/ale_box_coloration.F
      !||    check_remote_surface_state               ../engine/source/interfaces/interf/check_remote_surface_state.F
      !||    domain_decomposition_pcyl                ../starter/source/loads/general/load_pcyl/domain_decomposition_pcyl.F
      !||    get_neighbour_surface                    ../engine/source/interfaces/interf/get_neighbour_surface.F90
      !||    get_neighbour_surface_from_remote_proc   ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
      !||    i18main_kine_1                           ../engine/source/interfaces/int18/i18main_kine.F
      !||    igrsurf_split                            ../starter/source/spmd/igrsurf_split.F
      !||    init_inivol                              ../starter/source/initial_conditions/inivol/init_inivol.F90
      !||    init_inivol_2d_polygons                  ../starter/source/initial_conditions/inivol/init_inivol_2D_polygons.F90
      !||    init_nodal_state                         ../engine/source/interfaces/interf/init_nodal_state.F
      !||    initia                                   ../starter/source/elements/initia/initia.F
      !||    int18_alloc                              ../engine/source/interfaces/int18/int18_alloc.F
      !||    intfop2                                  ../engine/source/interfaces/interf/intfop2.F
      !||    resol                                    ../engine/source/engine/resol.F
      !||    spmd_exch_deleted_surf_edge              ../engine/source/mpi/interfaces/spmd_exch_deleted_surf_edge.F
      !||    spmd_exch_inter_18                       ../engine/source/mpi/interfaces/spmd_exch_inter_18.F
      !||    spmd_exch_neighbour_segment              ../engine/source/mpi/interfaces/spmd_exch_neighbour_segment.F90
      !||    update_neighbour_segment                 ../engine/source/interfaces/interf/update_neighbour_segment.F90
      !||====================================================================
        MODULE ARRAY_MOD
#include "my_real.inc"
!$COMMENT
!       ARRAY_MOD description
!           allocation & dealloaction of arrays (1d/2d/3d)
!       ARRAY_MOD organization
!$ENDCOMMENT

        TYPE array_type
            INTEGER :: SIZE_INT_ARRAY_1D
            INTEGER, DIMENSION(2) :: SIZE_INT_ARRAY_2D
            INTEGER, DIMENSION(3) :: SIZE_INT_ARRAY_3D
            INTEGER, DIMENSION(:), ALLOCATABLE :: INT_ARRAY_1D
            INTEGER, DIMENSION(:,:), ALLOCATABLE :: INT_ARRAY_2D
            INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: INT_ARRAY_3D

            INTEGER :: SIZE_MY_REAL_ARRAY_1D
            INTEGER, DIMENSION(2) :: SIZE_MY_REAL_ARRAY_2D
            INTEGER, DIMENSION(3) :: SIZE_MY_REAL_ARRAY_3D
            my_real, DIMENSION(:), ALLOCATABLE :: MY_REAL_ARRAY_1D
            my_real, DIMENSION(:,:), ALLOCATABLE :: MY_REAL_ARRAY_2D
            my_real, DIMENSION(:,:,:), ALLOCATABLE :: MY_REAL_ARRAY_3D

            INTEGER :: SIZE_DB_ARRAY_1D
            INTEGER, DIMENSION(2) :: SIZE_DB_ARRAY_2D
            INTEGER, DIMENSION(3) :: SIZE_DB_ARRAY_3D
            REAL(kind=8), DIMENSION(:), ALLOCATABLE :: DB_ARRAY_1D
            REAL(kind=8), DIMENSION(:,:), ALLOCATABLE :: DB_ARRAY_2D
            REAL(kind=8), DIMENSION(:,:,:), ALLOCATABLE :: DB_ARRAY_3D

            INTEGER :: SIZE_SP_ARRAY_1D
            INTEGER, DIMENSION(2) :: SIZE_SP_ARRAY_2D
            INTEGER, DIMENSION(3) :: SIZE_SP_ARRAY_3D
            REAL(kind=4), DIMENSION(:), ALLOCATABLE :: SP_ARRAY_1D
            REAL(kind=4), DIMENSION(:,:), ALLOCATABLE :: SP_ARRAY_2D
            REAL(kind=4), DIMENSION(:,:,:), ALLOCATABLE :: SP_ARRAY_3D
        END TYPE array_type

        CONTAINS
            ! ----------------------------
      !||====================================================================
      !||    alloc_1d_array                ../common_source/modules/array_mod.F
      !||--- called by ------------------------------------------------------
      !||    domain_decomposition_pcyl     ../starter/source/loads/general/load_pcyl/domain_decomposition_pcyl.F
      !||    igrsurf_split                 ../starter/source/spmd/igrsurf_split.F
      !||    init_inivol                   ../starter/source/initial_conditions/inivol/init_inivol.F90
      !||    init_nodal_state              ../engine/source/interfaces/interf/init_nodal_state.F
      !||    spmd_exch_deleted_surf_edge   ../engine/source/mpi/interfaces/spmd_exch_deleted_surf_edge.F
      !||====================================================================
            SUBROUTINE ALLOC_1D_ARRAY(THIS)
            IMPLICIT NONE

            TYPE(array_type), INTENT(inout) :: THIS

            ALLOCATE( THIS%INT_ARRAY_1D( THIS%SIZE_INT_ARRAY_1D ) )

            RETURN
            END SUBROUTINE 
            ! ----------------------------
      !||====================================================================
      !||    alloc_2d_array               ../common_source/modules/array_mod.F
      !||--- called by ------------------------------------------------------
      !||    check_remote_surface_state   ../engine/source/interfaces/interf/check_remote_surface_state.F
      !||====================================================================
            SUBROUTINE ALLOC_2D_ARRAY(THIS)
            IMPLICIT NONE

            TYPE(array_type), INTENT(inout) :: THIS

            ALLOCATE( THIS%INT_ARRAY_2D( THIS%SIZE_INT_ARRAY_2D(1),THIS%SIZE_INT_ARRAY_2D(2) ) )

            RETURN
            END SUBROUTINE 
            ! ----------------------------
      !||====================================================================
      !||    alloc_3d_array       ../common_source/modules/array_mod.F
      !||--- called by ------------------------------------------------------
      !||    ale_box_coloration   ../starter/source/initial_conditions/inivol/ale_box_coloration.F
      !||====================================================================
            SUBROUTINE ALLOC_3D_ARRAY(THIS)
            IMPLICIT NONE

            TYPE(array_type), INTENT(inout) :: THIS

            ALLOCATE( THIS%INT_ARRAY_3D( THIS%SIZE_INT_ARRAY_3D(1),
     .                                   THIS%SIZE_INT_ARRAY_3D(2),
     .                                   THIS%SIZE_INT_ARRAY_3D(3) ) )

            RETURN
            END SUBROUTINE 
            ! ----------------------------   
      !||====================================================================
      !||    dealloc_1d_array              ../common_source/modules/array_mod.F
      !||--- called by ------------------------------------------------------
      !||    domain_decomposition_pcyl     ../starter/source/loads/general/load_pcyl/domain_decomposition_pcyl.F
      !||    igrsurf_split                 ../starter/source/spmd/igrsurf_split.F
      !||    init_inivol                   ../starter/source/initial_conditions/inivol/init_inivol.F90
      !||    init_nodal_state              ../engine/source/interfaces/interf/init_nodal_state.F
      !||    spmd_exch_deleted_surf_edge   ../engine/source/mpi/interfaces/spmd_exch_deleted_surf_edge.F
      !||====================================================================
            SUBROUTINE DEALLOC_1D_ARRAY(THIS)
            IMPLICIT NONE

            TYPE(array_type), INTENT(inout) :: THIS

            DEALLOCATE( THIS%INT_ARRAY_1D )

            RETURN
            END SUBROUTINE 
            ! ----------------------------
      !||====================================================================
      !||    dealloc_2d_array             ../common_source/modules/array_mod.F
      !||--- called by ------------------------------------------------------
      !||    check_remote_surface_state   ../engine/source/interfaces/interf/check_remote_surface_state.F
      !||====================================================================
            SUBROUTINE DEALLOC_2D_ARRAY(THIS)
            IMPLICIT NONE

            TYPE(array_type), INTENT(inout) :: THIS

            DEALLOCATE( THIS%INT_ARRAY_2D )

            RETURN
            END SUBROUTINE 
            ! ----------------------------
      !||====================================================================
      !||    dealloc_3d_array   ../common_source/modules/array_mod.F
      !||--- called by ------------------------------------------------------
      !||    init_inivol        ../starter/source/initial_conditions/inivol/init_inivol.F90
      !||====================================================================
            SUBROUTINE DEALLOC_3D_ARRAY(THIS)
            IMPLICIT NONE

            TYPE(array_type), INTENT(inout) :: THIS

            DEALLOCATE( THIS%INT_ARRAY_3D )

            RETURN
            END SUBROUTINE 
            ! ----------------------------
      !||====================================================================
      !||    alloc_my_real_1d_array                   ../common_source/modules/array_mod.F
      !||--- called by ------------------------------------------------------
      !||    get_neighbour_surface                    ../engine/source/interfaces/interf/get_neighbour_surface.F90
      !||    get_neighbour_surface_from_remote_proc   ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
      !||    spmd_exch_inter_18                       ../engine/source/mpi/interfaces/spmd_exch_inter_18.F
      !||    spmd_exch_neighbour_segment              ../engine/source/mpi/interfaces/spmd_exch_neighbour_segment.F90
      !||====================================================================
            SUBROUTINE ALLOC_MY_REAL_1D_ARRAY(THIS)
#include "implicit_f.inc"

            TYPE(array_type), INTENT(inout) :: THIS

            ALLOCATE( THIS%MY_REAL_ARRAY_1D( THIS%SIZE_MY_REAL_ARRAY_1D ) )

            RETURN
            END SUBROUTINE ALLOC_MY_REAL_1D_ARRAY
            ! ----------------------------
            ! ----------------------------
      !||====================================================================
      !||    dealloc_my_real_1d_array                 ../common_source/modules/array_mod.F
      !||--- called by ------------------------------------------------------
      !||    get_neighbour_surface                    ../engine/source/interfaces/interf/get_neighbour_surface.F90
      !||    get_neighbour_surface_from_remote_proc   ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
      !||    spmd_exch_inter_18                       ../engine/source/mpi/interfaces/spmd_exch_inter_18.F
      !||====================================================================
            SUBROUTINE DEALLOC_MY_REAL_1D_ARRAY(THIS)
#include "implicit_f.inc"

            TYPE(array_type), INTENT(inout) :: THIS

            DEALLOCATE( THIS%MY_REAL_ARRAY_1D )

            RETURN
            END SUBROUTINE DEALLOC_MY_REAL_1D_ARRAY
            ! ----------------------------
        END MODULE ARRAY_MOD
