!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
          module spmd_exch_output_noda_pext_mod
          contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief spmd exchange between domain which have comon nodes
!! \details
          SUBROUTINE spmd_exch_output_noda_pext(NSPMD, NODES, NUMNOD, NUMNODG,
     .                                           TRIGGER_TH,
     .                                           TRIGGER_ANIM,
     .                                           TRIGGER_H3D)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
            use th_mod , only : th_has_noda_pext
            use nodal_arrays_mod , only : nodal_arrays_
            use constant_mod , only : zero
            use output_mod , only : NODA_PEXT_G, NODA_SURF_G
            use output_mod , only : NODA_PEXT, NODA_SURF
            use output_mod , only : ANIM_HAS_NODA_PEXT, H3D_HAS_NODA_PEXT
            use spmd_gather_nodal_scalar_mod , only : spmd_gather_nodal_scalar
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
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
            IF(TH_HAS_NODA_PEXT > 0 .OR. ANIM_HAS_NODA_PEXT > 0 .OR. H3D_HAS_NODA_PEXT > 0 )THEN
              IF(TRIGGER_ANIM .OR. TRIGGER_TH .OR. TRIGGER_H3D)THEN
                IF(NSPMD > 1)THEN
                  LENR = NODES%BOUNDARY_ADD(1,NSPMD+1)-NODES%BOUNDARY_ADD(1,1)
                  !exchange between domains which have common nodes
                  CALL SPMD_EXCH_NODAREA(NODA_PEXT, NODES%BOUNDARY_ADD, NODES%BOUNDARY, LENR, NODES%WEIGHT)
                  CALL SPMD_EXCH_NODAREA(NODA_SURF, NODES%BOUNDARY_ADD, NODES%BOUNDARY, LENR, NODES%WEIGHT)
                  !send everything to node 0 in global array (ANIM)
                  IF(ANIM_HAS_NODA_PEXT > 0)THEN
                    CALL SPMD_GATHER_NODAL_SCALAR(NODA_PEXT_G, NUMNODG, NODA_PEXT, NUMNOD, NODES%WEIGHT, NODES%NODGLOB)   !NODGLOB :id_loc -> id_glob
                    CALL SPMD_GATHER_NODAL_SCALAR(NODA_SURF_G, NUMNODG, NODA_SURF, NUMNOD, NODES%WEIGHT, NODES%NODGLOB)
                  END IF
                ELSE
                  IF(ANIM_HAS_NODA_PEXT > 0)THEN
                    !in this case numnodg = numnod
                    NODA_PEXT_G(1:NUMNOD) = NODA_PEXT(1:NUMNOD)
                    NODA_SURF_G(1:NUMNOD) = NODA_SURF(1:NUMNOD)
                  END IF
                ENDIF
              ENDIF
            ENDIF
          END SUBROUTINE SPMD_EXCH_OUTPUT_NODA_PEXT
! ----------------------------------------------------------------------------------------------------------------------
          end module spmd_exch_output_noda_pext_mod
