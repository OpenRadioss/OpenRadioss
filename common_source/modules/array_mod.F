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
Chd|  ARRAY_MOD                     modules/array_mod.F           
Chd|-- called by -----------
Chd|        ALE_BOX_COLORATION            starter/source/initial_conditions/inivol/ale_box_coloration.F
Chd|        DOMAIN_DECOMPOSITION_PCYL     starter/source/loads/general/load_pcyl/domain_decomposition_pcyl.F
Chd|        IGRSURF_SPLIT                 starter/source/spmd/igrsurf_split.F
Chd|        INITIA                        starter/source/elements/initia/initia.F
Chd|        CHECK_REMOTE_SURFACE_STATE    engine/source/interfaces/interf/check_remote_surface_state.F
Chd|        INIT_NODAL_STATE              engine/source/interfaces/interf/init_nodal_state.F
Chd|        SPMD_EXCH_DELETED_SURF_EDGE   engine/source/mpi/interfaces/spmd_exch_deleted_surf_edge.F
Chd|-- calls ---------------
Chd|====================================================================
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
Chd|====================================================================
Chd|  ALLOC_1D_ARRAY                modules/array_mod.F           
Chd|-- called by -----------
Chd|        DOMAIN_DECOMPOSITION_PCYL     starter/source/loads/general/load_pcyl/domain_decomposition_pcyl.F
Chd|        IGRSURF_SPLIT                 starter/source/spmd/igrsurf_split.F
Chd|        INITIA                        starter/source/elements/initia/initia.F
Chd|        INIT_NODAL_STATE              engine/source/interfaces/interf/init_nodal_state.F
Chd|        SPMD_EXCH_DELETED_SURF_EDGE   engine/source/mpi/interfaces/spmd_exch_deleted_surf_edge.F
Chd|-- calls ---------------
Chd|====================================================================
            SUBROUTINE ALLOC_1D_ARRAY(THIS)
            IMPLICIT NONE

            TYPE(array_type), INTENT(inout) :: THIS

            ALLOCATE( THIS%INT_ARRAY_1D( THIS%SIZE_INT_ARRAY_1D ) )

            RETURN
            END SUBROUTINE 
            ! ----------------------------
Chd|====================================================================
Chd|  ALLOC_2D_ARRAY                modules/array_mod.F           
Chd|-- called by -----------
Chd|        CHECK_REMOTE_SURFACE_STATE    engine/source/interfaces/interf/check_remote_surface_state.F
Chd|-- calls ---------------
Chd|====================================================================
            SUBROUTINE ALLOC_2D_ARRAY(THIS)
            IMPLICIT NONE

            TYPE(array_type), INTENT(inout) :: THIS

            ALLOCATE( THIS%INT_ARRAY_2D( THIS%SIZE_INT_ARRAY_2D(1),THIS%SIZE_INT_ARRAY_2D(2) ) )

            RETURN
            END SUBROUTINE 
            ! ----------------------------
Chd|====================================================================
Chd|  ALLOC_3D_ARRAY                modules/array_mod.F           
Chd|-- called by -----------
Chd|        ALE_BOX_COLORATION            starter/source/initial_conditions/inivol/ale_box_coloration.F
Chd|-- calls ---------------
Chd|====================================================================
            SUBROUTINE ALLOC_3D_ARRAY(THIS)
            IMPLICIT NONE

            TYPE(array_type), INTENT(inout) :: THIS

            ALLOCATE( THIS%INT_ARRAY_3D( THIS%SIZE_INT_ARRAY_3D(1),
     .                                   THIS%SIZE_INT_ARRAY_3D(2),
     .                                   THIS%SIZE_INT_ARRAY_3D(3) ) )

            RETURN
            END SUBROUTINE 
            ! ----------------------------   
Chd|====================================================================
Chd|  DEALLOC_1D_ARRAY              modules/array_mod.F           
Chd|-- called by -----------
Chd|        DOMAIN_DECOMPOSITION_PCYL     starter/source/loads/general/load_pcyl/domain_decomposition_pcyl.F
Chd|        IGRSURF_SPLIT                 starter/source/spmd/igrsurf_split.F
Chd|        INITIA                        starter/source/elements/initia/initia.F
Chd|        INIT_NODAL_STATE              engine/source/interfaces/interf/init_nodal_state.F
Chd|        SPMD_EXCH_DELETED_SURF_EDGE   engine/source/mpi/interfaces/spmd_exch_deleted_surf_edge.F
Chd|-- calls ---------------
Chd|====================================================================
            SUBROUTINE DEALLOC_1D_ARRAY(THIS)
            IMPLICIT NONE

            TYPE(array_type), INTENT(inout) :: THIS

            DEALLOCATE( THIS%INT_ARRAY_1D )

            RETURN
            END SUBROUTINE 
            ! ----------------------------
Chd|====================================================================
Chd|  DEALLOC_2D_ARRAY              modules/array_mod.F           
Chd|-- called by -----------
Chd|        CHECK_REMOTE_SURFACE_STATE    engine/source/interfaces/interf/check_remote_surface_state.F
Chd|-- calls ---------------
Chd|====================================================================
            SUBROUTINE DEALLOC_2D_ARRAY(THIS)
            IMPLICIT NONE

            TYPE(array_type), INTENT(inout) :: THIS

            DEALLOCATE( THIS%INT_ARRAY_2D )

            RETURN
            END SUBROUTINE 
            ! ----------------------------
Chd|====================================================================
Chd|  DEALLOC_3D_ARRAY              modules/array_mod.F           
Chd|-- called by -----------
Chd|        INITIA                        starter/source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|====================================================================
            SUBROUTINE DEALLOC_3D_ARRAY(THIS)
            IMPLICIT NONE

            TYPE(array_type), INTENT(inout) :: THIS

            DEALLOCATE( THIS%INT_ARRAY_3D )

            RETURN
            END SUBROUTINE 
            ! ----------------------------
Chd|====================================================================
Chd|  ALLOC_1D_ARRAY                modules/array_mod.F           
Chd|-- called by -----------
Chd|        DOMAIN_DECOMPOSITION_PCYL     starter/source/loads/general/load_pcyl/domain_decomposition_pcyl.F
Chd|        IGRSURF_SPLIT                 starter/source/spmd/igrsurf_split.F
Chd|        INITIA                        starter/source/elements/initia/initia.F
Chd|        INIT_NODAL_STATE              engine/source/interfaces/interf/init_nodal_state.F
Chd|        SPMD_EXCH_DELETED_SURF_EDGE   engine/source/mpi/interfaces/spmd_exch_deleted_surf_edge.F
Chd|-- calls ---------------
Chd|====================================================================
            SUBROUTINE ALLOC_MY_REAL_1D_ARRAY(THIS)
#include "implicit_f.inc"

            TYPE(array_type), INTENT(inout) :: THIS

            ALLOCATE( THIS%MY_REAL_ARRAY_1D( THIS%SIZE_MY_REAL_ARRAY_1D ) )

            RETURN
            END SUBROUTINE ALLOC_MY_REAL_1D_ARRAY
            ! ----------------------------
            ! ----------------------------
Chd|====================================================================
Chd|  ALLOC_1D_ARRAY                modules/array_mod.F           
Chd|-- called by -----------
Chd|        DOMAIN_DECOMPOSITION_PCYL     starter/source/loads/general/load_pcyl/domain_decomposition_pcyl.F
Chd|        IGRSURF_SPLIT                 starter/source/spmd/igrsurf_split.F
Chd|        INITIA                        starter/source/elements/initia/initia.F
Chd|        INIT_NODAL_STATE              engine/source/interfaces/interf/init_nodal_state.F
Chd|        SPMD_EXCH_DELETED_SURF_EDGE   engine/source/mpi/interfaces/spmd_exch_deleted_surf_edge.F
Chd|-- calls ---------------
Chd|====================================================================
            SUBROUTINE DEALLOC_MY_REAL_1D_ARRAY(THIS)
#include "implicit_f.inc"

            TYPE(array_type), INTENT(inout) :: THIS

            DEALLOCATE( THIS%MY_REAL_ARRAY_1D )

            RETURN
            END SUBROUTINE DEALLOC_MY_REAL_1D_ARRAY
            ! ----------------------------
        END MODULE ARRAY_MOD
