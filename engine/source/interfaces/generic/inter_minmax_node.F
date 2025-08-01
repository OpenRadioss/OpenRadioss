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
!||    inter_minmax_node      ../engine/source/interfaces/generic/inter_minmax_node.F
!||--- called by ------------------------------------------------------
!||    inter_init_component   ../engine/source/interfaces/generic/inter_init_component.F90
!||--- calls      -----------------------------------------------------
!||    my_barrier             ../engine/source/system/machine.F
!||--- uses       -----------------------------------------------------
!||    intbufdef_mod          ../common_source/modules/interfaces/intbufdef_mod.F90
!||    inter_struct_mod       ../engine/share/modules/inter_struct_mod.F
!||    message_mod            ../engine/share/message_module/message_mod.F
!||====================================================================
        SUBROUTINE INTER_MINMAX_NODE( ITASK,box_limit,X )
!$COMMENT
!       INTER_MINMAX_NODE description :
!       compute the minimal & maximal positions      
!
!       INTER_MINMAX_NODE organization :
!       loop over the nodes & min+max computation
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE INTBUFDEF_MOD  
        USE INTER_STRUCT_MOD
        USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: ITASK
        my_real, dimension(6), intent(inout) :: box_limit !< upper & lower bound of the box
        my_real, DIMENSION(3,NUMNOD), INTENT(in) :: X            !   position
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: FIRST, LAST
        INTEGER :: I,J
        my_real, DIMENSION(3) :: MAX_LIMIT,MIN_LIMIT
!   ----------------------------------------

        MAX_LIMIT(1:3) = -EP30
        MIN_LIMIT(1:3) = EP30
        FIRST = 1+ITASK*NUMNOD/NTHREAD
        LAST = (ITASK+1)*NUMNOD/NTHREAD
        !   ------------------
        !   find the min & max position 
        DO I=FIRST,LAST
            DO J=1,3
                MAX_LIMIT(J) = MAX(MAX_LIMIT(J),X(J,I))
                MIN_LIMIT(J) = MIN(MIN_LIMIT(J),X(J,I))
            ENDDO            
        ENDDO
        !   ------------------

        !   ------------------
        !   omp reduction 
#include "lockon.inc"
        BOX_LIMIT(1) = MAX(BOX_LIMIT(1),MAX_LIMIT(1))
        BOX_LIMIT(2) = MAX(BOX_LIMIT(2),MAX_LIMIT(2))
        BOX_LIMIT(3) = MAX(BOX_LIMIT(3),MAX_LIMIT(3))

        BOX_LIMIT(4) = MIN(BOX_LIMIT(4),MIN_LIMIT(1))
        BOX_LIMIT(5) = MIN(BOX_LIMIT(5),MIN_LIMIT(2))
        BOX_LIMIT(6) = MIN(BOX_LIMIT(6),MIN_LIMIT(3))
#include "lockoff.inc"
        !   ------------------
        CALL MY_BARRIER()

        RETURN
        END SUBROUTINE INTER_MINMAX_NODE
