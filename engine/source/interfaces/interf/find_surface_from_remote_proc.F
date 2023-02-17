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
Chd|  FIND_SURFACE_FROM_REMOTE_PROC source/interfaces/interf/find_surface_from_remote_proc.F
Chd|-- called by -----------
Chd|        SPMD_EXCH_DELETED_SURF_EDGE   source/mpi/interfaces/spmd_exch_deleted_surf_edge.F
Chd|-- calls ---------------
Chd|        CHECK_SURFACE_STATE           source/interfaces/interf/check_surface_state.F
Chd|        SYSFUS2                       source/system/sysfus.F        
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        SHOOTING_NODE_MOD             share/modules/shooting_node_mod.F
Chd|====================================================================
        SUBROUTINE FIND_SURFACE_FROM_REMOTE_PROC(SHOOT_STRUCT,NB_SURFACE,LIST_NODE,INTBUF_TAB,ITABM1 )
!$COMMENT
!       FIND_SURFACE_FROM_REMOTE_PROC description
!           this routine finds the surface id from a list of remote node
!       FIND_SURFACE_FROM_REMOTE_PROC organization 
!           loop over the node list:
!           - find the local node id from the global node id
!           - intersection of lists of surface for the node to obtain the surface id
!           - deactivation of the surface 
!$ENDCOMMENT
        USE INTBUFDEF_MOD   
        USE SHOOTING_NODE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: NB_SURFACE   ! number of "surface" (ie. 4 nodes)
        INTEGER, DIMENSION(4*NB_SURFACE), INTENT(in) :: LIST_NODE   ! list of 4 nodes
        INTEGER, DIMENSION(NUMNOD), INTENT(in) :: ITABM1    ! array of global id
        TYPE(shooting_node_type), INTENT(inout) :: SHOOT_STRUCT ! structure for shooting node algo  
        TYPE(INTBUF_STRUCT_), DIMENSION(NINTER), INTENT(inout) :: INTBUF_TAB    ! interface data         
 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,J,K,NODE_ID
        INTEGER :: NB_SURFACE_R_PROC
        INTEGER, DIMENSION(4) :: LOCAL_NODE,GLOBAL_NODE
        INTEGER, DIMENSION(:), ALLOCATABLE :: LIST_SURFACE_R_PROC

        INTEGER :: SHIFT
        INTEGER :: NODE_SURF_NB,EVERAL_SURF
        INTEGER :: NB_RESULT_INTERSECT,NB_SURFACE_1,NB_SURFACE_2
        INTEGER, DIMENSION(:), ALLOCATABLE :: RESULT_INTERSECT,INTERSECT_1,INTERSECT_2
        INTEGER, DIMENSION(:), ALLOCATABLE :: TMP_ARRAY
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
        INTEGER SYSFUS2 
C-----------------------------------------------
        ALLOCATE( LIST_SURFACE_R_PROC(4*NB_SURFACE) )
        NB_SURFACE_R_PROC = 0

        ! --------------------------
        ! working array : surface
        ALLOCATE( RESULT_INTERSECT( SHOOT_STRUCT%MAX_SURF_NB ) )
        ALLOCATE( INTERSECT_1( SHOOT_STRUCT%MAX_SURF_NB ) )
        ALLOCATE( INTERSECT_2( SHOOT_STRUCT%MAX_SURF_NB ) )


        DO I=1,NB_SURFACE
            GLOBAL_NODE(1:4) = LIST_NODE( (I-1)*4+1:(I-1)*4+4)
            DO J=1,4
                LOCAL_NODE(J) = SYSFUS2(GLOBAL_NODE(J),ITABM1,NUMNOD)
            ENDDO
            NODE_ID = LOCAL_NODE(1)    ! get the node ID
      
            NB_RESULT_INTERSECT = SHOOT_STRUCT%SHIFT_M_NODE_SURF(NODE_ID+1) - SHOOT_STRUCT%SHIFT_M_NODE_SURF(NODE_ID)   ! get the number of surface of the node
            SHIFT = SHOOT_STRUCT%SHIFT_M_NODE_SURF(NODE_ID)
            RESULT_INTERSECT(1:NB_RESULT_INTERSECT) = SHOOT_STRUCT%M_NODE_SURF( SHIFT+1:SHIFT+NB_RESULT_INTERSECT )

            NODE_SURF_NB = 4
            IF(LOCAL_NODE(3)==LOCAL_NODE(4)) NODE_SURF_NB = 3

            DO J=2,NODE_SURF_NB
                NB_SURFACE_1 = NB_RESULT_INTERSECT
                INTERSECT_1(1:NB_SURFACE_1) = RESULT_INTERSECT(1:NB_RESULT_INTERSECT)
                NODE_ID = LOCAL_NODE(J)    ! get the node ID  
                ! -----------------------         
                ! intersection of surface 
                NB_SURFACE_2 = SHOOT_STRUCT%SHIFT_M_NODE_SURF(NODE_ID+1) - SHOOT_STRUCT%SHIFT_M_NODE_SURF(NODE_ID)   ! get the number of surface of the node
                SHIFT = SHOOT_STRUCT%SHIFT_M_NODE_SURF(NODE_ID)
                INTERSECT_2(1:NB_SURFACE_2) = SHOOT_STRUCT%M_NODE_SURF( SHIFT+1:SHIFT+NB_SURFACE_2 )
                IF(NB_SURFACE_1>0.AND.NB_SURFACE_2>0) THEN
                    CALL INTERSECT_2_SORTED_SETS( INTERSECT_1,NB_SURFACE_1,
     .                                            INTERSECT_2,NB_SURFACE_2,
     .                                            RESULT_INTERSECT,NB_RESULT_INTERSECT )
                ELSE
                    NB_RESULT_INTERSECT = 0
                ENDIF
                ! end : intersection of surface 
                ! -----------------------
            ENDDO
            IF(NB_SURFACE_R_PROC + NB_RESULT_INTERSECT > SIZE(LIST_SURFACE_R_PROC) ) THEN                
                ALLOCATE( TMP_ARRAY(NB_SURFACE_R_PROC) )
                TMP_ARRAY(1:NB_SURFACE_R_PROC) = LIST_SURFACE_R_PROC(1:NB_SURFACE_R_PROC)
                DEALLOCATE( LIST_SURFACE_R_PROC )
                ALLOCATE( LIST_SURFACE_R_PROC( (NB_SURFACE_R_PROC+NB_RESULT_INTERSECT) * 2 ) )
                LIST_SURFACE_R_PROC(1:NB_SURFACE_R_PROC) = TMP_ARRAY(1:NB_SURFACE_R_PROC)
                DEALLOCATE( TMP_ARRAY )
            ENDIF
            
            LIST_SURFACE_R_PROC(1+NB_SURFACE_R_PROC:NB_SURFACE_R_PROC+NB_RESULT_INTERSECT) = 
     .            RESULT_INTERSECT(1:NB_RESULT_INTERSECT)
            NB_SURFACE_R_PROC = NB_SURFACE_R_PROC + NB_RESULT_INTERSECT            
        ENDDO

        CALL CHECK_SURFACE_STATE( -1,NB_SURFACE_R_PROC,LIST_SURFACE_R_PROC,SHOOT_STRUCT%SHIFT_INTERFACE,INTBUF_TAB )

        DEALLOCATE( LIST_SURFACE_R_PROC )
        DEALLOCATE( RESULT_INTERSECT )
        DEALLOCATE( INTERSECT_1 )
        DEALLOCATE( INTERSECT_2 )

        RETURN
        END SUBROUTINE FIND_SURFACE_FROM_REMOTE_PROC
