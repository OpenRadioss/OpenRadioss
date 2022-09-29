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
Chd|  FIND_EDGE_FROM_REMOTE_PROC    source/interfaces/interf/find_edge_from_remote_proc.F
Chd|-- called by -----------
Chd|        SPMD_EXCH_DELETED_SURF_EDGE   source/mpi/interfaces/spmd_exch_deleted_surf_edge.F
Chd|-- calls ---------------
Chd|        CHECK_EDGE_STATE              source/interfaces/interf/check_edge_state.F
Chd|        SYSFUS2                       source/system/sysfus.F        
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        SHOOTING_NODE_MOD             share/modules/shooting_node_mod.F
Chd|====================================================================
        SUBROUTINE FIND_EDGE_FROM_REMOTE_PROC(SHOOT_STRUCT,NB_EDGE,LIST_NODE,INTBUF_TAB,ITABM1,
     .                                        NEWFRONT,IPARI,GEO,
     .                                        IXS,IXC,IXT,IXP,IXR,IXTG,
     .                                        ADDCNEL,CNEL,TAG_NODE,TAG_ELEM )
!$COMMENT
!       FIND_EDGE_FROM_REMOTE_PROC description
!           deactivation of edges coming from remote proc
!       FIND_EDGE_FROM_REMOTE_PROC organization
!           the element associated to the edge is on a remote proc
!           the remote proc send me : "you must deactivate this edge because my element is 
!           deleted"
!           local procs find the local edge/interfaces 
!           local procs deactivate the edge from the interfaces     
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
#include      "scr05_c.inc"
#include      "com04_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "rnur_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: NB_EDGE   ! number of "edge" (ie. 2 nodes)
        INTEGER, DIMENSION(2*NB_EDGE), INTENT(in) :: LIST_NODE   ! list of 2 nodes
        INTEGER, DIMENSION(NUMNOD), INTENT(in) :: ITABM1    ! array of global id
        TYPE(shooting_node_type), INTENT(inout) :: SHOOT_STRUCT ! structure for shooting node algo  
        TYPE(INTBUF_STRUCT_), DIMENSION(NINTER), INTENT(inout) :: INTBUF_TAB    ! interface data         
         INTEGER, DIMENSION(NINTER), INTENT(inout) :: NEWFRONT   ! array for sorting : 1 --> need to sort the interface NIN
        INTEGER, DIMENSION(NIXS,NUMELS),TARGET, INTENT(in) :: IXS   ! solid array
        INTEGER, DIMENSION(NIXC,NUMELC),TARGET, INTENT(in) :: IXC   ! shell array
        INTEGER, DIMENSION(NIXT,NUMELT),TARGET, INTENT(in) :: IXT! truss array
        INTEGER, DIMENSION(NIXP,NUMELP),TARGET, INTENT(in) :: IXP! beam array
        INTEGER, DIMENSION(NIXR,NUMELR),TARGET, INTENT(in) :: IXR! spring array
        INTEGER, DIMENSION(NIXTG,NUMELTG),TARGET, INTENT(in) :: IXTG! triangle array
        INTEGER, DIMENSION(0:NUMNOD+1), INTENT(in) :: ADDCNEL ! address for the CNEL array
        INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) :: IPARI
        my_real, DIMENSION(NPROPG,NUMGEO), INTENT(in) :: GEO
        INTEGER, DIMENSION(0:LCNEL), INTENT(in) :: CNEL ! connectivity node-->element
        INTEGER, DIMENSION(NUMNOD), INTENT(inout) :: TAG_NODE
        INTEGER, DIMENSION(NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR+NUMELTG+NUMELUR), INTENT(inout) :: TAG_ELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,J,K,NODE_ID
        INTEGER :: MY_SIZE
        INTEGER :: NB_EDGE_R_PROC_M,NB_EDGE_R_PROC_S
        INTEGER, DIMENSION(2) :: LOCAL_NODE,GLOBAL_NODE
        INTEGER, DIMENSION(:), ALLOCATABLE :: LIST_EDGE_R_PROC

        INTEGER :: SHIFT
        INTEGER :: NB_RESULT_INTERSECT,NB_EDGE_1,NB_EDGE_2
        INTEGER, DIMENSION(:), ALLOCATABLE :: RESULT_INTERSECT,INTERSECT_1,INTERSECT_2
        INTEGER, DIMENSION(:), ALLOCATABLE :: TMP_ARRAY
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
        INTEGER SYSFUS2 
C-----------------------------------------------
        ALLOCATE( LIST_EDGE_R_PROC(4*NB_EDGE) )
        NB_EDGE_R_PROC_M = 0
        NB_EDGE_R_PROC_S = 0

        ! --------------------------
        ! working array : edge
        ALLOCATE( RESULT_INTERSECT( SHOOT_STRUCT%MAX_EDGE_NB ) )
        ALLOCATE( INTERSECT_1( SHOOT_STRUCT%MAX_EDGE_NB ) )
        ALLOCATE( INTERSECT_2( SHOOT_STRUCT%MAX_EDGE_NB ) )


        DO I=1,NB_EDGE
            GLOBAL_NODE(1:2) = LIST_NODE( (I-1)*2+1:(I-1)*2+2)
            DO J=1,2
                LOCAL_NODE(J) = SYSFUS2(GLOBAL_NODE(J),ITABM1,NUMNOD)
            ENDDO
            ! ------------------
            ! MAIN NODE
            ! Initialization of edge/proc for the first node
            NODE_ID = LOCAL_NODE(1)    ! get the node ID
            NB_EDGE_1 = SHOOT_STRUCT%SHIFT_M_NODE_EDGE(NODE_ID+1) - SHOOT_STRUCT%SHIFT_M_NODE_EDGE(NODE_ID)   ! get the number of edge of the node
            SHIFT = SHOOT_STRUCT%SHIFT_M_NODE_EDGE(NODE_ID)
            INTERSECT_1(1:NB_EDGE_1) = SHOOT_STRUCT%M_NODE_EDGE( SHIFT+1:SHIFT+NB_EDGE_1 )
            ! ------------------
            ! Initialization of edge/proc for the second node
            NODE_ID = LOCAL_NODE(2)    ! get the node ID
            NB_EDGE_2 = SHOOT_STRUCT%SHIFT_M_NODE_EDGE(NODE_ID+1) - SHOOT_STRUCT%SHIFT_M_NODE_EDGE(NODE_ID)   ! get the number of edge of the node
            SHIFT = SHOOT_STRUCT%SHIFT_M_NODE_EDGE(NODE_ID)
            INTERSECT_2(1:NB_EDGE_2) = SHOOT_STRUCT%M_NODE_EDGE( SHIFT+1:SHIFT+NB_EDGE_2 )
            ! ------------------

            ! -----------------------
            NB_RESULT_INTERSECT = 0
            IF(NB_EDGE_1>0.AND.NB_EDGE_2>0) THEN
                CALL INTERSECT_2_SORTED_SETS( INTERSECT_1,NB_EDGE_1,
     .                                        INTERSECT_2,NB_EDGE_2,
     .                                        RESULT_INTERSECT,NB_RESULT_INTERSECT )
            ELSE
                NB_RESULT_INTERSECT = 0
            ENDIF
            ! end : intersection of edge : main nodes
            ! -----------------------

            IF(NB_EDGE_R_PROC_M + NB_RESULT_INTERSECT > SIZE(LIST_EDGE_R_PROC) ) THEN
                ALLOCATE( TMP_ARRAY(NB_EDGE_R_PROC_M) )
                TMP_ARRAY(1:NB_EDGE_R_PROC_M) = LIST_EDGE_R_PROC(1:NB_EDGE_R_PROC_M)
                DEALLOCATE( LIST_EDGE_R_PROC )
                ALLOCATE( LIST_EDGE_R_PROC( (NB_EDGE_R_PROC_M+NB_RESULT_INTERSECT) * 2 ) )
                LIST_EDGE_R_PROC(1:NB_EDGE_R_PROC_M) = TMP_ARRAY(1:NB_EDGE_R_PROC_M)
            ENDIF
            LIST_EDGE_R_PROC(1+NB_EDGE_R_PROC_M:1+NB_EDGE_R_PROC_M+NB_RESULT_INTERSECT) = 
     .            RESULT_INTERSECT(1:NB_RESULT_INTERSECT)
            NB_EDGE_R_PROC_M = NB_EDGE_R_PROC_M + NB_RESULT_INTERSECT    
        ENDDO



        DO I=1,NB_EDGE
            GLOBAL_NODE(1:2) = LIST_NODE( (I-1)*2+1:(I-1)*2+2)
            DO J=1,2
                LOCAL_NODE(J) = SYSFUS2(GLOBAL_NODE(J),ITABM1,NUMNOD)
            ENDDO
            ! ------------------
            ! SECONDARY NODE
            ! Initialization of edge/proc for the first node
            NODE_ID = LOCAL_NODE(1)    ! get the node ID
            NB_EDGE_1 = SHOOT_STRUCT%SHIFT_S_NODE_EDGE(NODE_ID+1) - SHOOT_STRUCT%SHIFT_S_NODE_EDGE(NODE_ID)   ! get the number of edge of the node
            SHIFT = SHOOT_STRUCT%SHIFT_S_NODE_EDGE(NODE_ID)
            INTERSECT_1(1:NB_EDGE_1) = SHOOT_STRUCT%S_NODE_EDGE( SHIFT+1:SHIFT+NB_EDGE_1 )
            ! ------------------
            ! Initialization of edge/proc for the second node
            NODE_ID = LOCAL_NODE(2)    ! get the node ID
            NB_EDGE_2 = SHOOT_STRUCT%SHIFT_S_NODE_EDGE(NODE_ID+1) - SHOOT_STRUCT%SHIFT_S_NODE_EDGE(NODE_ID)   ! get the number of edge of the node
            SHIFT = SHOOT_STRUCT%SHIFT_S_NODE_EDGE(NODE_ID)
            INTERSECT_2(1:NB_EDGE_2) = SHOOT_STRUCT%S_NODE_EDGE( SHIFT+1:SHIFT+NB_EDGE_2 )
            ! ------------------

            ! -----------------------
            NB_RESULT_INTERSECT = 0
            IF(NB_EDGE_1>0.AND.NB_EDGE_2>0) THEN
                CALL INTERSECT_2_SORTED_SETS( INTERSECT_1,NB_EDGE_1,
     .                                        INTERSECT_2,NB_EDGE_2,
     .                                        RESULT_INTERSECT,NB_RESULT_INTERSECT )
            ELSE
                NB_RESULT_INTERSECT = 0
            ENDIF
            ! end : intersection of edge : secondary nodes
            ! -----------------------
            MY_SIZE = NB_EDGE_R_PROC_S + NB_EDGE_R_PROC_M
            IF(MY_SIZE + NB_RESULT_INTERSECT > SIZE(LIST_EDGE_R_PROC) ) THEN
                ALLOCATE( TMP_ARRAY(MY_SIZE) )
                TMP_ARRAY(1:MY_SIZE) = LIST_EDGE_R_PROC(1:MY_SIZE)
                DEALLOCATE( LIST_EDGE_R_PROC )
                ALLOCATE( LIST_EDGE_R_PROC( (MY_SIZE+NB_RESULT_INTERSECT) * 2 ) )
                LIST_EDGE_R_PROC(1:MY_SIZE) = TMP_ARRAY(1:MY_SIZE)
                DEALLOCATE( TMP_ARRAY )
            ENDIF
            LIST_EDGE_R_PROC(1+MY_SIZE:MY_SIZE+NB_RESULT_INTERSECT) = 
     .            RESULT_INTERSECT(1:NB_RESULT_INTERSECT)
            NB_EDGE_R_PROC_S = NB_EDGE_R_PROC_S + NB_RESULT_INTERSECT          
        ENDDO

        CALL CHECK_EDGE_STATE( -1,NB_EDGE_R_PROC_M,NB_EDGE_R_PROC_S,
     .                         LIST_EDGE_R_PROC(1),LIST_EDGE_R_PROC(1+NB_EDGE_R_PROC_M),
     .                         SHOOT_STRUCT%SHIFT_INTERFACE,INTBUF_TAB,NEWFRONT,IPARI,GEO,
     .                         IXS,IXC,IXT,IXP,IXR,IXTG,
     .                         ADDCNEL,ITABM1,CNEL,TAG_NODE,TAG_ELEM,SHOOT_STRUCT )

        DEALLOCATE( LIST_EDGE_R_PROC )
        DEALLOCATE( RESULT_INTERSECT )
        DEALLOCATE( INTERSECT_1 )
        DEALLOCATE( INTERSECT_2 )
        RETURN
        END SUBROUTINE FIND_EDGE_FROM_REMOTE_PROC
