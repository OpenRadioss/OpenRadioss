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
Chd|  FIND_EDGE_INTER               source/interfaces/interf/find_edge_inter.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        SHOOTING_NODE_MOD             share/modules/shooting_node_mod.F
Chd|====================================================================
        SUBROUTINE FIND_EDGE_INTER( ITASK,ITAB,SHOOT_STRUCT,IXS,IXS10,
     1                              IXC,IXTG,IXQ,IXT,IXP,
     2                              IXR,GEO )
!$COMMENT
!       FIND_EDGE_INTER description
!           this routine finds the edge id and the interfaces id of a list of deleted elements
!       FIND_EDGE_INTER organization 
!           loop over the deleted element:
!               intersection of the edge list for the x nodes of the element --> give the edge id where 
!               the nodes are defined
!               intersection of the proc list for the x nodes of the element --> give the proc id where 
!               the nodes are defined
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
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: ITASK ! omp thread ID
        INTEGER, DIMENSION(NIXS,NUMELS),TARGET, INTENT(in) :: IXS   ! solid array
        INTEGER, DIMENSION(6,NUMELS10),TARGET, INTENT(in) :: IXS10  ! tetra10 array
        INTEGER, DIMENSION(NIXC,NUMELC),TARGET, INTENT(in) :: IXC   ! shell array
        INTEGER, DIMENSION(NIXTG,NUMELTG),TARGET, INTENT(in) :: IXTG! triangle array
        INTEGER, DIMENSION(NIXQ,NUMELQ),TARGET, INTENT(in) :: IXQ! quad array
        INTEGER, DIMENSION(NIXT,NUMELT),TARGET, INTENT(in) :: IXT! truss array
        INTEGER, DIMENSION(NIXP,NUMELP),TARGET, INTENT(in) :: IXP! beam array
        INTEGER, DIMENSION(NIXR,NUMELR),TARGET, INTENT(in) :: IXR! spring array
        INTEGER, DIMENSION(NUMNOD), INTENT(in) :: ITAB ! array to convert local id to global id
        my_real, DIMENSION(NPROPG,NUMGEO), INTENT(in) :: GEO
        TYPE(shooting_node_type), INTENT(inout) :: SHOOT_STRUCT ! structure for shooting node algo       

!        INTEGER, DIMENSION(SIZE_SEC_NODE), INTENT(in) :: INTER_SEC_NODE,SEC_NODE_ID ! list of interface of the nodes & ID of secondary nodes in each interface
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,J,K,N,IJK
        INTEGER :: NODE_ID,NODE_ID_1,NODE_ID_2,ELEM_ID
        INTEGER :: OFFSET_SOLID,OFFSET_QUAD,OFFSET_SHELL,OFFSET_TRUSS
        INTEGER :: OFFSET_BEAM,OFFSET_SPRING,OFFSET_TRIANGLE,OFFSET_UR
        INTEGER, DIMENSION(2,12), TARGET :: EDGES_SOL ! definition of edge for solid
        INTEGER, DIMENSION(2,24), TARGET :: EDGES_TETRA10 ! definition of edge for tetra10
        INTEGER, DIMENSION(2,4), TARGET :: EDGES_SHELL ! definition of edge for shell/quad
        INTEGER, DIMENSION(2,3), TARGET :: EDGES_TRI ! definition of edge for triangle & spring type12
        INTEGER, DIMENSION(2,1), TARGET :: EDGES_2DELM ! definition of edge for 2d elment : truss/beam/spring
        INTEGER, DIMENSION(2,2), TARGET :: EDGES_SPRING_TYP12 ! definition of edge spring type 12
        INTEGER,DIMENSION(:,:), POINTER :: POINTER_EDGE,IX

        LOGICAL :: NO_EDGE,DO_COMPUTATION
        INTEGER :: SHIFT,SHIFT_ELM,OLD_SIZE
        INTEGER :: EDGE_NUMBER
        INTEGER :: NB_PROC_1,NB_PROC_2,NODE_EDGE_NB,SEVERAL_PROC,SEVERAL_EDGE
        INTEGER :: NB_RESULT_INTERSECT,NB_RESULT_INTERSECT_2,NB_EDGE_1,NB_EDGE_2
        INTEGER :: NB_RESULT_INTERSECT_3
        INTEGER, DIMENSION(:), ALLOCATABLE :: RESULT_INTERSECT,INTERSECT_1,INTERSECT_2
        INTEGER, DIMENSION(:), ALLOCATABLE :: RESULT_INTERSECT_2,INTERSECT_3,INTERSECT_4
        INTEGER, DIMENSION(:), ALLOCATABLE :: RESULT_INTERSECT_3
        INTEGER, DIMENSION(:), ALLOCATABLE :: TMP_ARRAY
        INTEGER, DIMENSION(4) :: LOCAL_NODE

        DATA EDGES_SHELL/1,2,
     .                   2,3,
     .                   3,4,
     .                   4,1/

        DATA EDGES_TRI/1,2,
     .                 2,3,
     .                 3,1/
        DATA EDGES_SPRING_TYP12/1,2,
     .                          2,3/

        DATA EDGES_2DELM/1,2/

        DATA EDGES_SOL/1,2,
     .                 2,3,
     .                 3,4,
     .                 4,1,
     .                 2,6,
     .                 6,5,
     .                 5,1,
     .                 3,7,
     .                 7,6,
     .                 4,8,
     .                 8,7,
     .                 5,8/

        DATA EDGES_TETRA10/ 1,11,
     .                     11,14,
     .                     14,1 ,
     .                      3,11,
     .                     11,15,
     .                     15,3 ,
     .                      5,14,
     .                     14,15,
     .                     15,5 ,
     .                      1,13,
     .                     13,14,
     .                      6,13,
     .                     13,16,
     .                     16,6 ,
     .                     14,16,
     .                     16,5 ,
     .                     11,12,
     .                     12,3 ,
     .                      6,12,
     .                     12,13,
     .                     11,13,
     .                     12,15,
     .                     12,16,
     .                     15,16/
C-----------------------------------------------

        ! --------------------------
        OFFSET_SOLID = 0
        OFFSET_QUAD=OFFSET_SOLID+NUMELS
        OFFSET_SHELL=OFFSET_QUAD+NUMELQ
        OFFSET_TRUSS=OFFSET_SHELL+NUMELC
        OFFSET_BEAM=OFFSET_TRUSS+NUMELT
        OFFSET_SPRING=OFFSET_BEAM+NUMELP
        OFFSET_TRIANGLE=OFFSET_SPRING+NUMELR
        OFFSET_UR=OFFSET_TRIANGLE+NUMELTG       
        ! --------------------------

        ! --------------------------
        ! allocation of SAVE_EDGE : index of deactivated edge 
        SHOOT_STRUCT%S_SAVE_M_EDGE = 2*SHOOT_STRUCT%S_GLOBAL_ELEM_INDEX    ! size of SAVE_EDGE array
        ALLOCATE( SHOOT_STRUCT%SAVE_M_EDGE( SHOOT_STRUCT%S_SAVE_M_EDGE ) )
        SHOOT_STRUCT%S_SAVE_S_EDGE = 2*SHOOT_STRUCT%S_GLOBAL_ELEM_INDEX    ! size of SAVE_EDGE array
        ALLOCATE( SHOOT_STRUCT%SAVE_S_EDGE( SHOOT_STRUCT%S_SAVE_S_EDGE ) )

        SHOOT_STRUCT%SAVE_M_EDGE_NB = 0    ! number of deactivated edge : main nodes
        SHOOT_STRUCT%SAVE_S_EDGE_NB = 0    ! number of deactivated edge : main nodes
        SHOOT_STRUCT%SAVE_M_EDGE( 1:SHOOT_STRUCT%S_SAVE_M_EDGE ) = 0 
        SHOOT_STRUCT%SAVE_S_EDGE( 1:SHOOT_STRUCT%S_SAVE_S_EDGE ) = 0 
        ! --------------------------
        ! allocation of SAVE_PROC : index of processor with the 4 nodes + 4 node ids
        SHOOT_STRUCT%S_SAVE_PROC_EDGE = 3*SHOOT_STRUCT%S_GLOBAL_ELEM_INDEX    ! size of SAVE_PROC array

        ALLOCATE( SHOOT_STRUCT%SAVE_PROC_EDGE( SHOOT_STRUCT%S_SAVE_PROC_EDGE ) )
        SHOOT_STRUCT%SAVE_PROC_NB_EDGE = 0    ! number of processor + 2 nodes of deactivated edges 
        SHOOT_STRUCT%SAVE_PROC_EDGE( 1:SHOOT_STRUCT%S_SAVE_PROC_EDGE ) = 0 
        ! --------------------------
        ! working array : edge
        ALLOCATE( RESULT_INTERSECT( SHOOT_STRUCT%MAX_EDGE_NB ) )
        ALLOCATE( RESULT_INTERSECT_3( SHOOT_STRUCT%MAX_EDGE_NB ) )
        ALLOCATE( INTERSECT_1( SHOOT_STRUCT%MAX_EDGE_NB ) )
        ALLOCATE( INTERSECT_2( SHOOT_STRUCT%MAX_EDGE_NB ) )
        ! working array : processor
        ALLOCATE( RESULT_INTERSECT_2( SHOOT_STRUCT%MAX_PROC_NB ) )
        ALLOCATE( INTERSECT_3( SHOOT_STRUCT%MAX_PROC_NB ) )
        ALLOCATE( INTERSECT_4( SHOOT_STRUCT%MAX_PROC_NB ) )

        NODE_EDGE_NB = 2   ! number of node per edge
        ! --------------------------
        DO I=1,SHOOT_STRUCT%S_GLOBAL_ELEM_INDEX
            ELEM_ID = SHOOT_STRUCT%GLOBAL_ELEM_INDEX(I) ! get the id of the deleted element
            DO_COMPUTATION = .TRUE.
            ! ----------------------
            IF(ELEM_ID<=NUMELS8) THEN
                ! solid element : 8 nodes --> 12 edges
                !     o----o
                !    /+   /|
                !   o-+--o |
                !   | o++|+o
                !   |+   |/
                !   o----o
                EDGE_NUMBER = 12 ! number of edge
                IX => IXS(1:NIXS,1:NUMELS)
                POINTER_EDGE => EDGES_SOL(1:2,1:12)
                SHIFT_ELM = OFFSET_SOLID
            ELSEIF(ELEM_ID<=NUMELS8+NUMELS10) THEN
                ! solid element : tetra10 --> 10 surfaces
                !     4 internal surfaces per "real surfaces"
                !     tetra4       -->          tetra10
                !      3d view                    2d view (draw a tetra10 with 3d view is really hard :) )
                !       o                          o
                !      /+\                        / \
                !     / + \                      /   \
                !    /  +  \                    o-----o
                !   /   o   \                  / \   / \
                !  /  +    + \                /   \ /   \
                ! o-----------o              o---- o ----o
                EDGE_NUMBER = 24 ! number of edge
                IX => IXS10(1:6,1:NUMELS10)
                POINTER_EDGE => EDGES_TETRA10(1:2,1:24)
                SHIFT_ELM = NUMELS8
            ELSEIF(ELEM_ID<=NUMELS) THEN
                ! other solid element : at least 8 nodes --> 12 edges
                !     o----o
                !    /|   /|
                !   o----o |
                !   | o--|-o
                !   |/   |/
                !   o----o
                EDGE_NUMBER = 12 ! number of edge
                IX => IXS(1:NIXS,1:NUMELS)
                POINTER_EDGE => EDGES_SOL(1:2,1:12)
                SHIFT_ELM = OFFSET_SOLID
            ELSEIF(ELEM_ID<=OFFSET_SHELL) THEN
                ! quad element
                ! 4 nodes / 4 edges
                !   o----o 
                !   |    |
                !   |    |
                !   o----o
                EDGE_NUMBER = 4 ! number of edges
                IX => IXQ(1:NIXQ,1:NUMELQ)
                POINTER_EDGE => EDGES_SHELL(1:2,1:4)
                SHIFT_ELM = OFFSET_QUAD
                DO_COMPUTATION = .FALSE.
            ELSEIF(ELEM_ID<=OFFSET_TRUSS) THEN
                ! shell element
                ! 4 nodes / 4 edges
                !   o----o 
                !   |    |
                !   |    |
                !   o----o
                EDGE_NUMBER = 4 ! number of edges
                IX => IXC(1:NIXC,1:NUMELC)
                POINTER_EDGE => EDGES_SHELL(1:2,1:4)
                SHIFT_ELM = OFFSET_SHELL
            ELSEIF(ELEM_ID<=OFFSET_BEAM) THEN
                ! truss element
                ! 2 nodes / 1 edges
                !   o----o 
                EDGE_NUMBER = 1 ! number of edges
                IX => IXT(1:NIXT,1:NUMELT)
                POINTER_EDGE => EDGES_2DELM(1:2,1:1)
                SHIFT_ELM = OFFSET_TRUSS
            ELSEIF(ELEM_ID<=OFFSET_SPRING) THEN
                ! beam element
                ! 2 nodes / 1 edges
                !   o----o 
                EDGE_NUMBER = 1 ! number of edges
                IX => IXP(1:NIXP,1:NUMELP)
                POINTER_EDGE => EDGES_2DELM(1:2,1:1)
                SHIFT_ELM = OFFSET_BEAM
            ELSEIF(ELEM_ID<=OFFSET_TRIANGLE) THEN
                ! spring element
                ! 2 nodes / 1 edges
                !   o----o 
                EDGE_NUMBER = 1 ! number of edges
                IX => IXR(1:NIXR,1:NUMELR)
                POINTER_EDGE => EDGES_2DELM(1:2,1:1)
                SHIFT_ELM = OFFSET_SPRING
                IF(NINT(GEO(12,IXR(1,ELEM_ID-SHIFT_ELM)))==12) THEN
                    ! spring element type 12 :
                    ! 3 nodes / 2 edges
                    !   o--o--o 
                    EDGE_NUMBER = 2 ! number of edges
                    POINTER_EDGE => EDGES_SPRING_TYP12(1:2,1:2)
                ENDIF
            ELSEIF(ELEM_ID<=OFFSET_UR) THEN
                ! triangle element
                ! 3 nodes / 3 edges
                !       o
                !      / \
                !     /   \
                !    o-----o
                EDGE_NUMBER = 3 ! number of surface
                IX => IXTG(1:NIXTG,1:NUMELTG)
                POINTER_EDGE => EDGES_TRI(1:2,1:3)
                SHIFT_ELM = OFFSET_TRIANGLE
            ELSE
                ! user element   
                DO_COMPUTATION = .FALSE.         
            ENDIF
            ! ----------------------
            IF(DO_COMPUTATION) THEN
                ! ----------------------
                ! loop over the edges of the element
                DO K=1,EDGE_NUMBER
                    SEVERAL_PROC = 0
                    SEVERAL_EDGE = 0
                    NO_EDGE = .FALSE.
                    ! ------------------
                    ! MAIN NODE
                    ! Initialization of edge/proc for the first node
                    N = POINTER_EDGE(1,K)                ! get the node of the edge
                    NODE_ID = IX(N+1,ELEM_ID-SHIFT_ELM)    ! get the node ID  
                    NODE_ID_1 = NODE_ID
                    LOCAL_NODE(1) = NODE_ID
                    NB_EDGE_1 = SHOOT_STRUCT%SHIFT_M_NODE_EDGE(NODE_ID+1) - SHOOT_STRUCT%SHIFT_M_NODE_EDGE(NODE_ID)   ! get the number of edge of the node
                    SHIFT = SHOOT_STRUCT%SHIFT_M_NODE_EDGE(NODE_ID)
                    INTERSECT_1(1:NB_EDGE_1) = SHOOT_STRUCT%M_NODE_EDGE( SHIFT+1:SHIFT+NB_EDGE_1 )
                    ! processor init
                    NB_PROC_1 = SHOOT_STRUCT%SHIFT_M_NODE_PROC(NODE_ID+1) - SHOOT_STRUCT%SHIFT_M_NODE_PROC(NODE_ID) ! get the number of processor of the node     
                    SHIFT = SHOOT_STRUCT%SHIFT_M_NODE_PROC(NODE_ID)
                    INTERSECT_3(1:NB_PROC_1) = SHOOT_STRUCT%M_NODE_PROC( SHIFT+1:SHIFT+NB_PROC_1 )
                    ! ------------------
                    ! Initialization of edge/proc for the second node
                    N = POINTER_EDGE(2,K)                ! get the node of the edge
                    NODE_ID = IX(N+1,ELEM_ID-SHIFT_ELM)    ! get the node ID 
                    NODE_ID_2 = NODE_ID 
                    LOCAL_NODE(2) = NODE_ID
                    NB_EDGE_2 = SHOOT_STRUCT%SHIFT_M_NODE_EDGE(NODE_ID+1) - SHOOT_STRUCT%SHIFT_M_NODE_EDGE(NODE_ID)   ! get the number of edge of the node
                    SHIFT = SHOOT_STRUCT%SHIFT_M_NODE_EDGE(NODE_ID)
                    INTERSECT_2(1:NB_EDGE_2) = SHOOT_STRUCT%M_NODE_EDGE( SHIFT+1:SHIFT+NB_EDGE_2 )
                    ! processor init
                    NB_PROC_2 = SHOOT_STRUCT%SHIFT_M_NODE_PROC(NODE_ID+1) - SHOOT_STRUCT%SHIFT_M_NODE_PROC(NODE_ID) ! get the number of processor of the node     
                    SHIFT = SHOOT_STRUCT%SHIFT_M_NODE_PROC(NODE_ID)
                    INTERSECT_4(1:NB_PROC_2) = SHOOT_STRUCT%M_NODE_PROC( SHIFT+1:SHIFT+NB_PROC_2 )
                    ! ------------------

                    IF( NODE_ID_1 /= NODE_ID_2 ) THEN 
                        ! -----------------------         
                        ! intersection of main edge 
                        NB_RESULT_INTERSECT = 0
                        IF(NB_EDGE_1>0.AND.NB_EDGE_2>0) THEN
                            CALL INTERSECT_2_SORTED_SETS( INTERSECT_1,NB_EDGE_1,
     .                                                  INTERSECT_2,NB_EDGE_2,
     .                                                    RESULT_INTERSECT,NB_RESULT_INTERSECT )
                        ELSE
                            NB_RESULT_INTERSECT = 0
                        ENDIF
                        ! end : intersection of edge 
                        ! -----------------------
    
                        ! -----------------------         
                        ! intersection of processor 
                        IF(NB_PROC_1>1.AND.NB_PROC_2>1) THEN
                            CALL INTERSECT_2_SORTED_SETS( INTERSECT_3,NB_PROC_1,
     .                                                    INTERSECT_4,NB_PROC_2,
     .                                                    RESULT_INTERSECT_2,NB_RESULT_INTERSECT_2 )
                        ELSE
                            NB_RESULT_INTERSECT_2 = 0
                        ENDIF
                        ! end : intersection of processor 
                        ! -----------------------
                    ELSE
                        NB_RESULT_INTERSECT = 0
                        NB_RESULT_INTERSECT_2 = 0
                    ENDIF


                    ! ------------------
                    ! SECONDARY NODE
                    ! Initialization of edge/proc for the first node
                    N = POINTER_EDGE(1,K)                ! get the node of the edge
                    NODE_ID = IX(N+1,ELEM_ID-SHIFT_ELM)    ! get the node ID  
                    LOCAL_NODE(3) = NODE_ID
                    NB_EDGE_1 = SHOOT_STRUCT%SHIFT_S_NODE_EDGE(NODE_ID+1) - SHOOT_STRUCT%SHIFT_S_NODE_EDGE(NODE_ID)   ! get the number of edge of the node
                    SHIFT = SHOOT_STRUCT%SHIFT_S_NODE_EDGE(NODE_ID)
                    INTERSECT_1(1:NB_EDGE_1) = SHOOT_STRUCT%S_NODE_EDGE( SHIFT+1:SHIFT+NB_EDGE_1 )
                    ! ------------------
                    ! Initialization of edge/proc for the second node
                    N = POINTER_EDGE(2,K)                ! get the node of the edge
                    NODE_ID = IX(N+1,ELEM_ID-SHIFT_ELM)    ! get the node ID  
                    LOCAL_NODE(4) = NODE_ID
                    NB_EDGE_2 = SHOOT_STRUCT%SHIFT_S_NODE_EDGE(NODE_ID+1) - SHOOT_STRUCT%SHIFT_S_NODE_EDGE(NODE_ID)   ! get the number of edge of the node
                    SHIFT = SHOOT_STRUCT%SHIFT_S_NODE_EDGE(NODE_ID)
                    INTERSECT_2(1:NB_EDGE_2) = SHOOT_STRUCT%S_NODE_EDGE( SHIFT+1:SHIFT+NB_EDGE_2 )
                    ! ------------------

                    IF( NODE_ID_1 /= NODE_ID_2 ) THEN 
                        ! -----------------------         
                        ! intersection of secondary edge 
                        NB_RESULT_INTERSECT_3 = 0
                        IF(NB_EDGE_1>0.AND.NB_EDGE_2>0) THEN
                          CALL INTERSECT_2_SORTED_SETS( INTERSECT_1,NB_EDGE_1,
     .                                                    INTERSECT_2,NB_EDGE_2,
     .                                                    RESULT_INTERSECT_3,NB_RESULT_INTERSECT_3 )
                        ELSE
                            NB_RESULT_INTERSECT_3 = 0
                        ENDIF
                        ! end : intersection of edge 
                        ! -----------------------
                    ELSE
                        NB_RESULT_INTERSECT_3 = 0
                    ENDIF

                    IF(NB_RESULT_INTERSECT>0) THEN
                    ! one or several edge on the current processor
                    ! save the edge id
                        IF( SHOOT_STRUCT%SAVE_M_EDGE_NB+NB_RESULT_INTERSECT>SHOOT_STRUCT%S_SAVE_M_EDGE) THEN
                            ALLOCATE( TMP_ARRAY(SHOOT_STRUCT%S_SAVE_M_EDGE) )
                            TMP_ARRAY(1:SHOOT_STRUCT%S_SAVE_M_EDGE) = SHOOT_STRUCT%SAVE_M_EDGE(1:SHOOT_STRUCT%S_SAVE_M_EDGE)

                            DEALLOCATE( SHOOT_STRUCT%SAVE_M_EDGE )
                            OLD_SIZE = SHOOT_STRUCT%S_SAVE_M_EDGE
                            SHOOT_STRUCT%S_SAVE_M_EDGE = 1.20*(SHOOT_STRUCT%S_SAVE_M_EDGE+5*NB_RESULT_INTERSECT)
                            ALLOCATE( SHOOT_STRUCT%SAVE_M_EDGE( SHOOT_STRUCT%S_SAVE_M_EDGE ) )
                            SHOOT_STRUCT%SAVE_M_EDGE(1:OLD_SIZE) =  TMP_ARRAY(1:OLD_SIZE)
                            DEALLOCATE( TMP_ARRAY )
                        ENDIF
                        DO J=1,NB_RESULT_INTERSECT
                            SHOOT_STRUCT%SAVE_M_EDGE_NB = SHOOT_STRUCT%SAVE_M_EDGE_NB + 1
                            SHOOT_STRUCT%SAVE_M_EDGE( SHOOT_STRUCT%SAVE_M_EDGE_NB ) =  RESULT_INTERSECT(J)
                        ENDDO
                    ENDIF

                    IF(NB_RESULT_INTERSECT_2>1) THEN        !SEVERAL_PROC==NODE_SURF_NB) THEN
                        ! one or several edge on a remote processor : 
                        ! save the remote proc id & the node id
                        ! |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  ...
                        !   pi    n1    n2     pj    n1     n3   pk    n3   n10
                        !  proc id & the 2 nodes        
                                                 

                        IF( SHOOT_STRUCT%SAVE_PROC_NB_EDGE+3*(NB_RESULT_INTERSECT_2-1)>
     .                      SHOOT_STRUCT%S_SAVE_PROC_EDGE) THEN
                            ALLOCATE( TMP_ARRAY(SHOOT_STRUCT%S_SAVE_PROC_EDGE) )
                            TMP_ARRAY(1:SHOOT_STRUCT%S_SAVE_PROC_EDGE) =
     .                          SHOOT_STRUCT%SAVE_PROC_EDGE(1:SHOOT_STRUCT%S_SAVE_PROC_EDGE)

                            DEALLOCATE( SHOOT_STRUCT%SAVE_PROC_EDGE )
                            OLD_SIZE = SHOOT_STRUCT%S_SAVE_PROC_EDGE
                            SHOOT_STRUCT%S_SAVE_PROC_EDGE =
     .                       1.20*(SHOOT_STRUCT%SAVE_PROC_NB_EDGE+3*(NB_RESULT_INTERSECT_2-1))
                            ALLOCATE( SHOOT_STRUCT%SAVE_PROC_EDGE( SHOOT_STRUCT%S_SAVE_PROC_EDGE ) )
                            SHOOT_STRUCT%SAVE_PROC_EDGE(1:OLD_SIZE) =  TMP_ARRAY(1:OLD_SIZE)
                            DEALLOCATE( TMP_ARRAY )
                        ENDIF

                        DO J=1,NB_RESULT_INTERSECT_2
                            IF(RESULT_INTERSECT_2(J)/=ISPMD+1) THEN
                                SHOOT_STRUCT%SAVE_PROC_NB_EDGE = SHOOT_STRUCT%SAVE_PROC_NB_EDGE + 1
                                SHOOT_STRUCT%SAVE_PROC_EDGE( SHOOT_STRUCT%SAVE_PROC_NB_EDGE ) =  RESULT_INTERSECT_2(J)    ! save the remore proc id

                                DO IJK=1,2
                                    SHOOT_STRUCT%SAVE_PROC_NB_EDGE = 
     .                               SHOOT_STRUCT%SAVE_PROC_NB_EDGE + 1
                                    SHOOT_STRUCT%SAVE_PROC_EDGE( SHOOT_STRUCT%SAVE_PROC_NB_EDGE ) =
     .                               ITAB(LOCAL_NODE(IJK))  ! convert local id to global id
                                ENDDO
                            ENDIF
                        ENDDO
                    ELSE
                        ! no edge on the current processor or on a remote processor
                    ENDIF

                    IF(NB_RESULT_INTERSECT_3>0) THEN
                    ! one or several edge on the current processor
                    ! save the edge id
                       
                        IF( SHOOT_STRUCT%SAVE_S_EDGE_NB+NB_RESULT_INTERSECT_3>
     .                       SHOOT_STRUCT%S_SAVE_S_EDGE) THEN
                            ALLOCATE( TMP_ARRAY(SHOOT_STRUCT%S_SAVE_S_EDGE) )
                            TMP_ARRAY(1:SHOOT_STRUCT%S_SAVE_S_EDGE) = SHOOT_STRUCT%SAVE_S_EDGE(1:SHOOT_STRUCT%S_SAVE_S_EDGE)

                            DEALLOCATE( SHOOT_STRUCT%SAVE_S_EDGE )
                            OLD_SIZE = SHOOT_STRUCT%S_SAVE_S_EDGE
                            SHOOT_STRUCT%S_SAVE_S_EDGE = 1.20*(SHOOT_STRUCT%S_SAVE_S_EDGE+5*NB_RESULT_INTERSECT_3)
                            ALLOCATE( SHOOT_STRUCT%SAVE_S_EDGE( SHOOT_STRUCT%S_SAVE_S_EDGE ) )
                            SHOOT_STRUCT%SAVE_S_EDGE(1:OLD_SIZE) =  TMP_ARRAY(1:OLD_SIZE)
                            DEALLOCATE( TMP_ARRAY )
                        ENDIF
                        DO J=1,NB_RESULT_INTERSECT_3
                            SHOOT_STRUCT%SAVE_S_EDGE_NB = SHOOT_STRUCT%SAVE_S_EDGE_NB + 1
                            SHOOT_STRUCT%SAVE_S_EDGE( SHOOT_STRUCT%SAVE_S_EDGE_NB ) =  RESULT_INTERSECT_3(J)
                        ENDDO
                    ENDIF
                ENDDO
                ! end : loop over the surfaces of the element
                ! ----------------------
            ENDIF
        ENDDO
        ! --------------------------

        ! --------------------------
        ! working array : surface
        DEALLOCATE( RESULT_INTERSECT )
        DEALLOCATE( RESULT_INTERSECT_3 )
        DEALLOCATE( INTERSECT_1 )
        DEALLOCATE( INTERSECT_2 )
        ! working array : processor
        DEALLOCATE( RESULT_INTERSECT_2 )
        DEALLOCATE( INTERSECT_3 )
        DEALLOCATE( INTERSECT_4 )
        ! --------------------------

        RETURN
        END SUBROUTINE FIND_EDGE_INTER
