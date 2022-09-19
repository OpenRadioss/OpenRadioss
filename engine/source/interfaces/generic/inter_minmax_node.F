Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  INTER_MINMAX_NODE             source/interfaces/generic/inter_minmax_node.F
Chd|-- called by -----------
Chd|        INTER_PREPARE_SORT            source/interfaces/generic/inter_prepare_sort.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTER_SORTING_MOD             share/modules/inter_sorting_mod.F
Chd|        INTER_STRUCT_MOD              share/modules/inter_struct_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
        SUBROUTINE INTER_MINMAX_NODE( ITASK,X )
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
        USE INTER_SORTING_MOD
        USE MESSAGE_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
#include      "impl1_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: ITASK
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
