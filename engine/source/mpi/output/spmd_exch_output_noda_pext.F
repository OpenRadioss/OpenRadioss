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
!||    spmd_exch_output_noda_pext_mod   ../engine/source/mpi/output/spmd_exch_output_noda_pext.F
!||--- called by ------------------------------------------------------
!||    sortie_main                      ../engine/source/output/sortie_main.F
!||====================================================================
          module spmd_exch_output_noda_pext_mod
          contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief spmd exchange between domain which have common nodes
!! \details
!||====================================================================
!||    spmd_exch_output_noda_pext     ../engine/source/mpi/output/spmd_exch_output_noda_pext.F
!||--- called by ------------------------------------------------------
!||    sortie_main                    ../engine/source/output/sortie_main.F
!||--- calls      -----------------------------------------------------
!||    spmd_exch_nodarea              ../engine/source/mpi/anim/spmd_exch_nodarea.F
!||    spmd_gather_nodal_scalar       ../engine/source/mpi/nodes/spmd_gather_nodal_scalar.F
!||--- uses       -----------------------------------------------------
!||    constant_mod                   ../common_source/modules/constant_mod.F
!||    nodal_arrays_mod               ../common_source/modules/nodal_arrays.F90
!||    output_mod                     ../common_source/modules/output/output_mod.F90
!||    spmd_gather_nodal_scalar_mod   ../engine/source/mpi/nodes/spmd_gather_nodal_scalar.F
!||    th_mod                         ../engine/share/modules/th_mod.F
!||====================================================================
          SUBROUTINE spmd_exch_output_noda_pext(OUTPUT, NSPMD, NODES, NUMNOD, NUMNODG,
     .                                           TRIGGER_TH,
     .                                           TRIGGER_ANIM,
     .                                           TRIGGER_H3D)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
            use th_mod , only : th_has_noda_pext
            use nodal_arrays_mod , only : nodal_arrays_
            use constant_mod , only : zero
            use output_mod , only : OUTPUT_
            use spmd_gather_nodal_scalar_mod , only : spmd_gather_nodal_scalar
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
            TYPE(OUTPUT_), INTENT(INOUT) :: OUTPUT
            LOGICAL,INTENT(IN) :: TRIGGER_ANIM
            LOGICAL,INTENT(IN) :: TRIGGER_H3D
            LOGICAL,INTENT(IN) :: TRIGGER_TH
            INTEGER,INTENT(IN) :: NSPMD
            INTEGER,INTENT(IN) :: NUMNOD
            INTEGER,INTENT(IN) :: NUMNODG
            TYPE(NODAL_ARRAYS_), INTENT(IN) :: NODES
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
            INTEGER :: LENR
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
            IF(TH_HAS_NODA_PEXT > 0 .OR. OUTPUT%DATA%ANIM_HAS_NODA_PEXT > 0 
     .          .OR. OUTPUT%DATA%H3D_HAS_NODA_PEXT > 0 )THEN
              IF(TRIGGER_ANIM .OR. TRIGGER_TH .OR. TRIGGER_H3D)THEN
                IF(NSPMD > 1)THEN
                  LENR = NODES%BOUNDARY_ADD(1,NSPMD+1)-NODES%BOUNDARY_ADD(1,1)
                  !exchange between domains which have common nodes
                  CALL SPMD_EXCH_NODAREA(OUTPUT%DATA%NODA_PEXT, NODES%BOUNDARY_ADD, NODES%BOUNDARY, LENR, NODES%WEIGHT)
                  CALL SPMD_EXCH_NODAREA(OUTPUT%DATA%NODA_SURF, NODES%BOUNDARY_ADD, NODES%BOUNDARY, LENR, NODES%WEIGHT)
                  !send everything to node 0 in global array (ANIM)
                  IF(OUTPUT%DATA%ANIM_HAS_NODA_PEXT > 0)THEN
                    CALL SPMD_GATHER_NODAL_SCALAR(OUTPUT%DATA%NODA_PEXT_G,
     .                  NUMNODG, OUTPUT%DATA%NODA_PEXT, NUMNOD, NODES%WEIGHT, NODES%NODGLOB)   !NODGLOB :id_loc -> id_glob
                    CALL SPMD_GATHER_NODAL_SCALAR(OUTPUT%DATA%NODA_SURF_G, NUMNODG,
     .                   OUTPUT%DATA%NODA_SURF, NUMNOD, NODES%WEIGHT, NODES%NODGLOB)
                  END IF
                ELSE
                  IF(OUTPUT%DATA%ANIM_HAS_NODA_PEXT > 0)THEN
                    !in this case numnodg = numnod
                    OUTPUT%DATA%NODA_PEXT_G(1:NUMNOD) = OUTPUT%DATA%NODA_PEXT(1:NUMNOD)
                    OUTPUT%DATA%NODA_SURF_G(1:NUMNOD) = OUTPUT%DATA%NODA_SURF(1:NUMNOD)
                  END IF
                ENDIF
              ENDIF
            ENDIF
          END SUBROUTINE SPMD_EXCH_OUTPUT_NODA_PEXT
! ----------------------------------------------------------------------------------------------------------------------
          end module spmd_exch_output_noda_pext_mod
