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
Chd|  COUNT_NB_ELEM_EDGE            source/interfaces/interf/count_nb_elem_edge.F
Chd|-- called by -----------
Chd|        INIT_NODAL_STATE              source/interfaces/interf/init_nodal_state.F
Chd|-- calls ---------------
Chd|        SHOOTING_NODE_MOD             share/modules/shooting_node_mod.F
Chd|====================================================================
        SUBROUTINE COUNT_NB_ELEM_EDGE( MODE,SIZE_BUFFER,BUFFER,SHOOT_STRUCT,CHUNK)
!$COMMENT
!       COUNT_NB_ELEM_EDGE description
!           count the number of element associated to an edge
!           mode=1 : secondary node / mode=2 : main node
!       COUNT_NB_ELEM_EDGE organization
!$ENDCOMMENT
        USE SHOOTING_NODE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER :: MODE     ! mode = 1 --> main node / mode = 2 --> secondary node
        INTEGER, INTENT(in) :: CHUNK ! chunk size : 1-> interface id, 2-> surface id, 3:6-> node id (for type11, 5&6 values are equal to 0)
        TYPE(shooting_node_type), INTENT(inout) :: SHOOT_STRUCT ! structure for shooting node algo
        INTEGER, INTENT(in) :: SIZE_BUFFER  ! size of buffer array
        INTEGER, DIMENSION(SIZE_BUFFER), INTENT(inout) :: BUFFER ! list of nin/edge id/number of remote element 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I
        INTEGER :: N1,N2,NIN
        INTEGER :: IRECT_ID,NB_REMOTE_ELEM
C-----------------------------------------------

        DO I=1,SIZE_BUFFER,CHUNK
            NIN = BUFFER(I)
            IRECT_ID = BUFFER(I+1)
            NB_REMOTE_ELEM = BUFFER(I+2)
            IF(MODE==1) THEN
                SHOOT_STRUCT%INTER(NIN)%REMOTE_ELM_M(IRECT_ID) = SHOOT_STRUCT%INTER(NIN)%REMOTE_ELM_M(IRECT_ID) + NB_REMOTE_ELEM
            ELSE
                SHOOT_STRUCT%INTER(NIN)%REMOTE_ELM_S(IRECT_ID) = SHOOT_STRUCT%INTER(NIN)%REMOTE_ELM_S(IRECT_ID) + NB_REMOTE_ELEM
            ENDIF
        ENDDO

        RETURN
        END SUBROUTINE COUNT_NB_ELEM_EDGE

