Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||    ale51_gradient_reconstruction            ../engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
!||    ale_box_coloration                       ../starter/source/initial_conditions/inivol/ale_box_coloration.F
!||    check_remote_surface_state               ../engine/source/interfaces/interf/check_remote_surface_state.F
!||    domain_decomposition_pcyl                ../starter/source/loads/general/load_pcyl/domain_decomposition_pcyl.F
!||    get_neighbour_surface                    ../engine/source/interfaces/interf/get_neighbour_surface.F90
!||    get_neighbour_surface_from_remote_proc   ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
!||    i18main_kine_1                           ../engine/source/interfaces/int18/i18main_kine.F
!||    i25sto                                   ../engine/source/interfaces/intsort/i25sto.F
!||    i25trivox1                               ../starter/source/interfaces/inter3d1/i25trivox1.F
!||    i7trivox1                                ../starter/source/interfaces/inter3d1/i7trivox1.F
!||    igrsurf_split                            ../starter/source/spmd/igrsurf_split.F
!||    init_inivol                              ../starter/source/initial_conditions/inivol/init_inivol.F90
!||    init_inivol_2d_polygons                  ../starter/source/initial_conditions/inivol/init_inivol_2D_polygons.F90
!||    init_nodal_state                         ../engine/source/interfaces/interf/init_nodal_state.F
!||    initia                                   ../starter/source/elements/initia/initia.F
!||    int18_alloc                              ../engine/source/interfaces/int18/int18_alloc.F
!||    inter_init_node_color                    ../engine/source/interfaces/generic/inter_init_node_color.F90
!||    inter_save_candidate                     ../starter/source/interfaces/inter3d1/inter_save_candidate.F90
!||    intfop2                                  ../engine/source/interfaces/interf/intfop2.F
!||    resol                                    ../engine/source/engine/resol.F
!||    spmd_exch_deleted_surf_edge              ../engine/source/mpi/interfaces/spmd_exch_deleted_surf_edge.F
!||    spmd_exch_inter_18                       ../engine/source/mpi/interfaces/spmd_exch_inter_18.F
!||    spmd_exch_n_neighbor                     ../engine/source/mpi/ale/spmd_exch_n_neighbor.F90
!||    spmd_exch_neighbour_segment              ../engine/source/mpi/interfaces/spmd_exch_neighbour_segment.F90
!||    spmd_xv_inter_type1                      ../engine/source/mpi/nodes/spmd_sd_xv_inter1.F90
!||    update_neighbour_segment                 ../engine/source/interfaces/interf/update_neighbour_segment.F90
!||====================================================================
      MODULE ARRAY_MOD
#include "my_real.inc"
!$COMMENT
!       ARRAY_MOD description
!           allocation & dealloaction of arrays (1d/2d/3d)
!       ARRAY_MOD organization
!       DO NOT ALLOCATE LARGE ARRAYS OF TYPE(array_type) if only a few elements are needed
!        SIZE(array_type) == 1248 bytes (ifx) or 1152 bytes (gfortran)
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
        TYPE array_type_int_1d
          INTEGER :: SIZE_INT_ARRAY_1D
          INTEGER, DIMENSION(:), ALLOCATABLE :: INT_ARRAY_1D
        END TYPE array_type_int_1d

        TYPE array_type_my_real_2d
          INTEGER, DIMENSION(2) :: SIZE_MY_REAL_ARRAY_2D
          my_real, DIMENSION(:,:), ALLOCATABLE :: MY_REAL_ARRAY_2D
        END TYPE array_type_my_real_2d        
        TYPE array_type_my_real_3d
          INTEGER, DIMENSION(3) :: SIZE_MY_REAL_ARRAY_3D
          my_real, DIMENSION(:,:,:), ALLOCATABLE :: MY_REAL_ARRAY_3D
        END TYPE array_type_my_real_3d                

        interface alloc_1d_array
          MODULE PROCEDURE alloc_1d_array_full
          MODULE PROCEDURE alloc_int_1d_array
        end interface
        interface dealloc_1d_array
          MODULE PROCEDURE dealloc_1d_array_full
          MODULE PROCEDURE dealloc_int_1d_array
        end interface

        interface alloc_my_real_2d_array
          MODULE PROCEDURE alloc_my_real_2d_array_full
          MODULE PROCEDURE alloc_my_real_2d_array
        end interface alloc_my_real_2d_array    
        interface dealloc_my_real_2d_array
          MODULE PROCEDURE dealloc_my_real_2d_array_full
          MODULE PROCEDURE dealloc_my_real_2d_array
        end interface dealloc_my_real_2d_array      
        interface alloc_my_real_3d_array
          MODULE PROCEDURE alloc_my_real_3d_array_full
          MODULE PROCEDURE alloc_my_real_3d_array
        end interface alloc_my_real_3d_array    
        interface dealloc_my_real_3d_array
          MODULE PROCEDURE dealloc_my_real_3d_array_full
          MODULE PROCEDURE dealloc_my_real_3d_array
        end interface dealloc_my_real_3d_array                     


      CONTAINS
        ! ----------------------------
!||====================================================================
!||    alloc_1d_array_full   ../common_source/modules/array_mod.F
!||====================================================================
        SUBROUTINE ALLOC_1D_ARRAY_FULL(THIS)
          IMPLICIT NONE

          TYPE(array_type), INTENT(inout) :: THIS

          ALLOCATE( THIS%INT_ARRAY_1D( THIS%SIZE_INT_ARRAY_1D ) )

          RETURN
        END SUBROUTINE

!||====================================================================
!||    alloc_int_1d_array      ../common_source/modules/array_mod.F
!||--- called by ------------------------------------------------------
!||    inter_init_node_color   ../engine/source/interfaces/generic/inter_init_node_color.F90
!||====================================================================
        SUBROUTINE ALLOC_INT_1D_ARRAY(THIS)
          IMPLICIT NONE

          TYPE(array_type_int_1d), INTENT(inout) :: THIS

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
     .                                 THIS%SIZE_INT_ARRAY_3D(2),
     .                                 THIS%SIZE_INT_ARRAY_3D(3) ) )

          RETURN
        END SUBROUTINE
        ! ----------------------------
!||====================================================================
!||    dealloc_1d_array_full   ../common_source/modules/array_mod.F
!||====================================================================
        SUBROUTINE DEALLOC_1D_ARRAY_FULL(THIS)
          IMPLICIT NONE

          TYPE(array_type), INTENT(inout) :: THIS

          DEALLOCATE( THIS%INT_ARRAY_1D )

          RETURN
        END SUBROUTINE
        ! ----------------------------
!||====================================================================
!||    dealloc_int_1d_array   ../common_source/modules/array_mod.F
!||====================================================================
        SUBROUTINE DEALLOC_INT_1D_ARRAY(THIS)
          IMPLICIT NONE

          TYPE(array_type_int_1d), INTENT(inout) :: THIS

          DEALLOCATE( THIS%INT_ARRAY_1D )

          RETURN
        END SUBROUTINE

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



!||====================================================================
!||    alloc_my_real_2d_array   ../common_source/modules/array_mod.F
!||--- called by ------------------------------------------------------
!||    spmd_exch_n_neighbor     ../engine/source/mpi/ale/spmd_exch_n_neighbor.F90
!||    spmd_xv_inter_type1      ../engine/source/mpi/nodes/spmd_sd_xv_inter1.F90
!||====================================================================
        SUBROUTINE ALLOC_MY_REAL_2D_ARRAY_full(THIS)
#include "implicit_f.inc"

          TYPE(array_type), INTENT(inout) :: THIS

          ALLOCATE( THIS%MY_REAL_ARRAY_2D( THIS%SIZE_MY_REAL_ARRAY_2D(1),THIS%SIZE_MY_REAL_ARRAY_2D(2) ) )

          RETURN
        END SUBROUTINE ALLOC_MY_REAL_2D_ARRAY_full
        ! ----------------------------
        SUBROUTINE ALLOC_MY_REAL_2D_ARRAY(THIS)
#include "implicit_f.inc"

          TYPE(array_type_my_real_2d), INTENT(inout) :: THIS

          ALLOCATE( THIS%MY_REAL_ARRAY_2D( THIS%SIZE_MY_REAL_ARRAY_2D(1),THIS%SIZE_MY_REAL_ARRAY_2D(2) ) )

          RETURN
        END SUBROUTINE ALLOC_MY_REAL_2D_ARRAY
        ! ----------------------------        
        ! ----------------------------
!||====================================================================
!||    dealloc_my_real_2d_array   ../common_source/modules/array_mod.F
!||--- called by ------------------------------------------------------
!||    spmd_exch_n_neighbor       ../engine/source/mpi/ale/spmd_exch_n_neighbor.F90
!||    spmd_xv_inter_type1        ../engine/source/mpi/nodes/spmd_sd_xv_inter1.F90
!||====================================================================
        SUBROUTINE DEALLOC_MY_REAL_2D_ARRAY_full(THIS)
#include "implicit_f.inc"

          TYPE(array_type), INTENT(inout) :: THIS

          DEALLOCATE( THIS%MY_REAL_ARRAY_2D )

          RETURN
        END SUBROUTINE DEALLOC_MY_REAL_2D_ARRAY_full
        ! ----------------------------
        SUBROUTINE DEALLOC_MY_REAL_2D_ARRAY(THIS)
#include "implicit_f.inc"

          TYPE(array_type_my_real_2d), INTENT(inout) :: THIS

          DEALLOCATE( THIS%MY_REAL_ARRAY_2D )

          RETURN
        END SUBROUTINE DEALLOC_MY_REAL_2D_ARRAY
        ! ---------------------------- 

        SUBROUTINE ALLOC_MY_REAL_3D_ARRAY_full(THIS)
#include "implicit_f.inc"

          TYPE(array_type), INTENT(inout) :: THIS

          ALLOCATE( THIS%MY_REAL_ARRAY_3D( THIS%SIZE_MY_REAL_ARRAY_3D(1),THIS%SIZE_MY_REAL_ARRAY_3D(2),
     .                                     THIS%SIZE_MY_REAL_ARRAY_3D(3) ) )

          RETURN
        END SUBROUTINE ALLOC_MY_REAL_3D_ARRAY_full
        ! ----------------------------
        SUBROUTINE ALLOC_MY_REAL_3D_ARRAY(THIS)
#include "implicit_f.inc"

          TYPE(array_type_my_real_3d), INTENT(inout) :: THIS
          ALLOCATE( THIS%MY_REAL_ARRAY_3D( THIS%SIZE_MY_REAL_ARRAY_3D(1),THIS%SIZE_MY_REAL_ARRAY_3D(2),
     .                                     THIS%SIZE_MY_REAL_ARRAY_3D(3) ) )

          RETURN
        END SUBROUTINE ALLOC_MY_REAL_3D_ARRAY
        ! ----------------------------        
        ! ----------------------------
        SUBROUTINE DEALLOC_MY_REAL_3D_ARRAY_full(THIS)
#include "implicit_f.inc"

          TYPE(array_type), INTENT(inout) :: THIS

          DEALLOCATE( THIS%MY_REAL_ARRAY_3D )

          RETURN
        END SUBROUTINE DEALLOC_MY_REAL_3D_ARRAY_full
        ! ----------------------------
        SUBROUTINE DEALLOC_MY_REAL_3D_ARRAY(THIS)
#include "implicit_f.inc"

          TYPE(array_type_my_real_3d), INTENT(inout) :: THIS

          DEALLOCATE( THIS%MY_REAL_ARRAY_3D )

          RETURN
        END SUBROUTINE DEALLOC_MY_REAL_3D_ARRAY
        ! ----------------------------             


      END MODULE ARRAY_MOD
