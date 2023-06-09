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
Chd|  DEALLOC_SHOOT_INTER           source/interfaces/interf/dealloc_shoot_inter.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        SHOOTING_NODE_MOD             share/modules/shooting_node_mod.F
Chd|====================================================================
        SUBROUTINE DEALLOC_SHOOT_INTER( SHOOT_STRUCT )
!$COMMENT
!       DEALLOC_SHOOT_INTER description
!           deallocation of SHOOT_STRUCT%array
!       DEALLOC_SHOOT_INTER organization
!$ENDCOMMENT
        USE SHOOTING_NODE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE(shooting_node_type), INTENT(inout) :: SHOOT_STRUCT ! structure for shooting node algo  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        IF(ALLOCATED(SHOOT_STRUCT%SAVE_SURFACE) )DEALLOCATE( SHOOT_STRUCT%SAVE_SURFACE )
        IF(ALLOCATED(SHOOT_STRUCT%SAVE_PROC) )DEALLOCATE( SHOOT_STRUCT%SAVE_PROC )

        IF(ALLOCATED(SHOOT_STRUCT%SAVE_M_EDGE) )DEALLOCATE( SHOOT_STRUCT%SAVE_M_EDGE )
        IF(ALLOCATED(SHOOT_STRUCT%SAVE_S_EDGE) )DEALLOCATE( SHOOT_STRUCT%SAVE_S_EDGE )
        IF(ALLOCATED(SHOOT_STRUCT%SAVE_PROC_EDGE) )DEALLOCATE( SHOOT_STRUCT%SAVE_PROC_EDGE)

        IF(ALLOCATED(SHOOT_STRUCT%GLOBAL_ELEM_INDEX) ) DEALLOCATE(SHOOT_STRUCT%GLOBAL_ELEM_INDEX)
        SHOOT_STRUCT%NUMBER_REMOTE_SURF = 0
        SHOOT_STRUCT%SIZE_REMOTE_SURF = 0
        IF(ALLOCATED(SHOOT_STRUCT%REMOTE_SURF)) DEALLOCATE(SHOOT_STRUCT%REMOTE_SURF)
        ALLOCATE( SHOOT_STRUCT%REMOTE_SURF(SHOOT_STRUCT%SIZE_REMOTE_SURF) )

        IF(ALLOCATED(SHOOT_STRUCT%SAVE_SURFACE_EXT)) DEALLOCATE(SHOOT_STRUCT%SAVE_SURFACE_EXT)

        RETURN
        END SUBROUTINE DEALLOC_SHOOT_INTER
