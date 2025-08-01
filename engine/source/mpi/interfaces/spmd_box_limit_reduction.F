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
!||    spmd_box_limit_reduction   ../engine/source/mpi/interfaces/spmd_box_limit_reduction.F
!||--- called by ------------------------------------------------------
!||    inter_init_component       ../engine/source/interfaces/generic/inter_init_component.F90
!||    inter_prepare_sort         ../engine/source/interfaces/generic/inter_prepare_sort.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    inter_struct_mod           ../engine/share/modules/inter_struct_mod.F
!||    spmd_mod                   ../engine/source/mpi/spmd_mod.F90
!||====================================================================
        SUBROUTINE SPMD_BOX_LIMIT_REDUCTION(NB_INTER_SORTED,box_limit)
!$COMMENT
!       SPMD_GRID_LIMIT_REDUCTION description :
!       compute the minimal & maximal positions      
!
!       SPMD_GRID_LIMIT_REDUCTION organization :
!       loop over the nodes & min+max computation
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTER_STRUCT_MOD
        USE SPMD_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: NB_INTER_SORTED        !< number of interfaces that need to be sorted
        my_real, dimension(6), intent(inout) :: box_limit !< upper & lower bound of the box
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I
        REAL(kind=8), DIMENSION(6) :: BUFFER_S,BUFFER_R
#ifdef MPI        
        INTEGER :: ierror
        INTEGER :: STATUS_(MPI_STATUS_SIZE)
        CHARACTER(len=4) :: MY_OPERATION
C-----------------------------------------------
        !   ------------------  
        !   globalization of min/max position      
        DO I=1,3
            BUFFER_S(I) = BOX_LIMIT(I)  ! max values
            BUFFER_S(3+I) = -ONE * BOX_LIMIT(3+I)   ! min values
        ENDDO
        !   ------------------
        MY_OPERATION(1:4) = ''
        MY_OPERATION(1:3) = "MAX"
        
        CALL SPMD_ALLREDUCE(BUFFER_S,BUFFER_R,6,SPMD_MAX)
        !   ------------------
        IF(NB_INTER_SORTED>0) THEN
            DO I=1,3
                BOX_LIMIT(I) = BUFFER_R(I)
                BOX_LIMIT(3+I) = -ONE * BUFFER_R(3+I)
            ENDDO 
        ENDIF
        !   ------------------
#endif
        RETURN
        END SUBROUTINE SPMD_BOX_LIMIT_REDUCTION
