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
Chd|  CHECK_NODAL_STATE             source/interfaces/interf/check_nodal_state.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
        SUBROUTINE CHECK_NODAL_STATE( ITASK,ITAG,NEWFRONT,INTBUF_TAB,SIZE_SEC_NODE,
     .                        SHIFT_S_NODE,INTER_SEC_NODE,SEC_NODE_ID)
!$COMMENT
!       CHECK_NODAL_STATE description
!           deactivation of node from an interface
!       CHECK_NODAL_STATE organization
!$ENDCOMMENT
        USE INTBUFDEF_MOD  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "scr05_c.inc"
#include      "com04_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: ITASK ! omp thread ID
        INTEGER, INTENT(in) :: SIZE_SEC_NODE    ! size of INTER_SEC_NODE & SEC_NODE_ID
        INTEGER, DIMENSION(2*NUMNOD), INTENT(in) :: ITAG    ! tag of node : 0 if secondary node is deactivated, 1 if secondary node is activated
        INTEGER, DIMENSION(NINTER), INTENT(inout) :: NEWFRONT   ! array for sorting : 1 --> need to sort the interface NIN
        TYPE(INTBUF_STRUCT_), DIMENSION(NINTER), INTENT(inout) :: INTBUF_TAB    ! interface data
!   shift to point to INTER_SEC_NODE/SEC_NODE_ID arrays & number of interface per node:
!   SHIFT_S_NODE(i) = index to NTER_SEC_NODE/SEC_NODE_ID for node_id = i
!   SHIFT_S_NODE(i+1) - SHIFT_S_NODE(i) = number of interface per node
        INTEGER, DIMENSION(NUMNOD+1), INTENT(in) :: SHIFT_S_NODE

        INTEGER, DIMENSION(SIZE_SEC_NODE), INTENT(in) :: INTER_SEC_NODE,SEC_NODE_ID ! list of interface of the nodes & ID of secondary nodes in each interface
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,J,K
        INTEGER :: FIRST,LAST
        INTEGER :: LOCAL_COUNTER,SHIFT
        INTEGER :: NB_INTERFACE,NIN,NODE_ID
        INTEGER, DIMENSION(:), ALLOCATABLE :: LOCAL_INDEX_SECONDARY_NODE
C-----------------------------------------------
        LOCAL_COUNTER = 0
        FIRST = 1 + ITASK * (NUMNOD / NTHREAD)
        LAST = (ITASK + 1) * (NUMNOD / NTHREAD)
        IF(ITASK+1==NTHREAD) LAST = NUMNOD
        ALLOCATE( LOCAL_INDEX_SECONDARY_NODE( NUMNOD/NTHREAD ) )
        ! --------------------------
        ! find the deactivated nodes 
        DO I=FIRST,LAST
            IF(ITAG(I)==0) THEN
                LOCAL_COUNTER = LOCAL_COUNTER + 1
                LOCAL_INDEX_SECONDARY_NODE(LOCAL_COUNTER) = I   ! save the node ID
            ENDIF
        ENDDO
        ! --------------------------
        ! loop over the interface of deactivated nodes to deactivate the secondary nodes
        DO I=1,LOCAL_COUNTER
            K = LOCAL_INDEX_SECONDARY_NODE(I)  ! get the node ID
            NB_INTERFACE = SHIFT_S_NODE(K+1) - SHIFT_S_NODE(K)  ! get the number of interface where the node is defined
            SHIFT = SHIFT_S_NODE(K)             ! shift for the array "INTER_SEC_NODE"
            DO J=1,NB_INTERFACE
                NIN = INTER_SEC_NODE(SHIFT+J)   ! interface ID
                NODE_ID = SEC_NODE_ID(SHIFT+J)  ! secondary node ID of the interface NIN

                IF(INTBUF_TAB(NIN)%STFNS(NODE_ID)>ZERO) THEN    ! check the current state of the node
                    INTBUF_TAB(NIN)%STFNS(NODE_ID) = -INTBUF_TAB(NIN)%STFNS(NODE_ID) ! nodal state change
                    NEWFRONT(NIN) = -1  ! force the sorting of the interface NIN for the next cycle
                ENDIF
            ENDDO        
        ENDDO
        ! --------------------------
        DEALLOCATE( LOCAL_INDEX_SECONDARY_NODE )

        RETURN
        END SUBROUTINE CHECK_NODAL_STATE
