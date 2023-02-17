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
Chd|  CHECK_COARSE_GRID             source/interfaces/generic/check_coarse_grid.F
Chd|-- called by -----------
Chd|        SPMD_COARSE_CELL_EXCHANGE     source/mpi/interfaces/spmd_coarse_cell_exchange.F
Chd|-- calls ---------------
Chd|        INTER_SORTING_MOD             share/modules/inter_sorting_mod.F
Chd|====================================================================
        SUBROUTINE CHECK_COARSE_GRID(NIN,MAIN_COARSE_GRID,SORT_COMM,ITIED)
!$COMMENT
!       CHECK_COARSE_GRID description :
!       for a given interface, check if the correct processor needs to communicate with a remote processor
!
!       CHECK_COARSE_GRID organization :
!           loop over the processor P and :
!               * if P and the current processor ISPMD colored a common cell --> ISPMD and P need to communicate
!               * if P and the current processor ISPMD did not colored a common cell --> ISPMD and P don't need to communicate
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTER_SORTING_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
#include      "impl1_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: NIN
        INTEGER, INTENT(in) :: ITIED
        INTEGER, DIMENSION(NB_BOX_COARSE_GRID**3), INTENT(in) :: MAIN_COARSE_GRID
        TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------       
        LOGICAL :: KEEP_THIS_PROC,BREAK_
        INTEGER :: I,J,IJK
        INTEGER :: IX,IY,IZ
        INTEGER :: IX_R,IY_R,IZ_R
        INTEGER :: ID_PROC,DISPL        
!   ----------------------------------------

        !   allocatio of KEEP_PROC : true if need to send/rcv to proc P
        IF(.NOT.ALLOCATED(SORT_COMM(NIN)%KEEP_PROC)) THEN
            ALLOCATE(SORT_COMM(NIN)%KEEP_PROC(SORT_COMM(NIN)%PROC_NUMBER) )
        ENDIF
        !   loop over the processor of the interface : keep P if at least 1 common cell between P and current proc
        DO I=1,SORT_COMM(NIN)%PROC_NUMBER
            ID_PROC = SORT_COMM(NIN)%PROC_LIST(I)
            DISPL = SORT_COMM(NIN)%RCV_DISPLS_COARSE_CELL(I)
            SORT_COMM(NIN)%KEEP_PROC(I) = .FALSE.
            KEEP_THIS_PROC = .FALSE.
            BREAK_ = .FALSE.
            J = 1
            !   no cell from P --> keep = false
            IF(SORT_COMM(NIN)%RCV_SIZE_COARSE_CELL(I)==0) BREAK_ = .TRUE.

            IF(.NOT.BREAK_) THEN
                IF(SORT_COMM(NIN)%GLOBAL_COARSE_CELL(DISPL+NB_BOX_COARSE_GRID**3+1)==0) BREAK_ = .TRUE.
            ENDIF

            IF(.NOT.BREAK_) THEN
                ! loop voer the cell of P and check if 1 common cell
                DO J=1,SORT_COMM(NIN)%GLOBAL_COARSE_CELL(DISPL+NB_BOX_COARSE_GRID**3+1)
                    IJK = SORT_COMM(NIN)%GLOBAL_COARSE_CELL(DISPL+J)
                    IF(MAIN_COARSE_GRID(IJK)==1) KEEP_THIS_PROC = .TRUE.
                ENDDO
            ENDIF
            IF(ITIED/=0) KEEP_THIS_PROC=.TRUE.
            IF(KEEP_THIS_PROC) SORT_COMM(NIN)%KEEP_PROC(I) = .TRUE.
        ENDDO
        !   ------------------
        RETURN
        END SUBROUTINE CHECK_COARSE_GRID
