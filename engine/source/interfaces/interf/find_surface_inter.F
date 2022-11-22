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
Chd|  FIND_SURFACE_INTER            source/interfaces/interf/find_surface_inter.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        SHOOTING_NODE_MOD             share/modules/shooting_node_mod.F
Chd|====================================================================
        SUBROUTINE FIND_SURFACE_INTER(ITASK,ITAB,SHOOT_STRUCT,IXS,IXS10,IXC,IXTG )
!$COMMENT
!       FIND_EDGE_INTER description
!           this routine finds the surface id and the interfaces id of a list of deleted elements
!       FIND_EDGE_INTER organization 
!           loop over the deleted element:
!               intersection of the surface list for the x nodes of the element --> give the surface id where 
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
        INTEGER, DIMENSION(NUMNOD), INTENT(in) :: ITAB ! array to convert local id to global id
        TYPE(shooting_node_type), INTENT(inout) :: SHOOT_STRUCT ! structure for shooting node algo       
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,J,K,N,IJK
        INTEGER :: NODE_ID,ELEM_ID
        INTEGER :: OFFSET_SOLID,OFFSET_QUAD,OFFSET_SHELL,OFFSET_TRUSS
        INTEGER :: OFFSET_BEAM,OFFSET_SPRING,OFFSET_TRIANGLE,OFFSET_UR
        INTEGER, DIMENSION(4,6), TARGET :: FACES ! definition of faces for solid
        INTEGER, DIMENSION(3,16), TARGET :: FACES10 ! definition of faces for tetra10
        INTEGER, DIMENSION(4,1), TARGET :: FACES_SHELL ! definition of face for shell/quad/triangle
        INTEGER,DIMENSION(:,:), POINTER :: POINTER_FACE,IX

        LOGICAL :: NO_SURF,DO_COMPUTATION
        INTEGER :: SHIFT,SHIFT_ELM,OLD_SIZE
        INTEGER :: SURFACE_NUMBER
        INTEGER :: NB_PROC_1,NB_PROC_2,NODE_SURF_NB,SEVERAL_PROC,SEVERAL_SURF
        INTEGER :: NB_RESULT_INTERSECT,NB_RESULT_INTERSECT_2,NB_SURFACE_1,NB_SURFACE_2
        INTEGER, DIMENSION(:), ALLOCATABLE :: RESULT_INTERSECT,INTERSECT_1,INTERSECT_2
        INTEGER, DIMENSION(:), ALLOCATABLE :: RESULT_INTERSECT_2,INTERSECT_3,INTERSECT_4
        INTEGER, DIMENSION(:), ALLOCATABLE :: TMP_ARRAY
        INTEGER, DIMENSION(4) :: LOCAL_NODE
        DATA FACES_SHELL/1,2,3,4/

        DATA FACES/1,2,3,4,
     .             1,2,6,5,
     .             2,3,7,6,
     .             3,4,8,7,
     .             1,5,8,4,
     .             5,6,7,8/
        DATA FACES10/1,11,14,
     .               3,11,15,
     .               5,14,15,
     .               11,14,15,
     .               1,13,14,
     .               6,13,16,
     .               5,14,16,
     .               13,14,16,
     .               3,11,12,
     .               6,12,13,
     .               1,11,13,
     .               11,12,13,
     .               3,12,15,
     .               6,12,16,
     .               5,15,16,
     .               12,15,16/
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
        ! allocation of SAVE_SURFACE : index of deactivated surface 
        SHOOT_STRUCT%S_SAVE_SURFACE = 4*SHOOT_STRUCT%S_GLOBAL_ELEM_INDEX    ! size of SAVE_SURFACE array
        ALLOCATE( SHOOT_STRUCT%SAVE_SURFACE( SHOOT_STRUCT%S_SAVE_SURFACE ) )
        SHOOT_STRUCT%SAVE_SURFACE_NB = 0    ! number of deactivated surface 
        SHOOT_STRUCT%SAVE_SURFACE( 1:SHOOT_STRUCT%S_SAVE_SURFACE ) = 0 
        ! --------------------------
        ! allocation of SAVE_PROC : index of processor with the 4 nodes + 4 node ids
        SHOOT_STRUCT%S_SAVE_PROC = 5*SHOOT_STRUCT%S_GLOBAL_ELEM_INDEX    ! size of SAVE_PROC array
        ALLOCATE( SHOOT_STRUCT%SAVE_PROC( SHOOT_STRUCT%S_SAVE_PROC ) )
        SHOOT_STRUCT%SAVE_PROC_NB = 0    ! number of processor + 4 nodes of deactivated surface 
        SHOOT_STRUCT%SAVE_PROC( 1:SHOOT_STRUCT%S_SAVE_PROC ) = 0 
        ! --------------------------
        ! working array : surface
        ALLOCATE( RESULT_INTERSECT( SHOOT_STRUCT%MAX_SURF_NB ) )
        ALLOCATE( INTERSECT_1( SHOOT_STRUCT%MAX_SURF_NB ) )
        ALLOCATE( INTERSECT_2( SHOOT_STRUCT%MAX_SURF_NB ) )
        ! working array : processor
        ALLOCATE( RESULT_INTERSECT_2( SHOOT_STRUCT%MAX_PROC_NB ) )
        ALLOCATE( INTERSECT_3( SHOOT_STRUCT%MAX_PROC_NB ) )
        ALLOCATE( INTERSECT_4( SHOOT_STRUCT%MAX_PROC_NB ) )
        ! --------------------------
        DO I=1,SHOOT_STRUCT%S_GLOBAL_ELEM_INDEX
            ELEM_ID = SHOOT_STRUCT%GLOBAL_ELEM_INDEX(I) ! get the id of the deleted element
            DO_COMPUTATION = .TRUE.
            ! ----------------------
            IF(ELEM_ID<=NUMELS8) THEN
                ! solid element : 8 nodes --> 6 surfaces
                !     o----o
                !    /+   /|
                !   o-+--o |
                !   | o++|+o
                !   |+   |/
                !   o----o
                SURFACE_NUMBER = 6 ! number of surface
                NODE_SURF_NB = 4   ! number of node per surface
                IX => IXS(1:NIXS,1:NUMELS)
                POINTER_FACE => FACES(1:4,1:6)
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
                SURFACE_NUMBER = 16 ! number of surface
                IX => IXS10(1:6,1:NUMELS10)
                POINTER_FACE => FACES10(1:3,1:16)
                NODE_SURF_NB = 3  ! number of node per surface
                SHIFT_ELM = NUMELS8
            ELSEIF(ELEM_ID<=NUMELS) THEN
                ! other solid element : at least 8 nodes --> 6 surfaces
                !     o----o
                !    /|   /|
                !   o----o |
                !   | o--|-o
                !   |/   |/
                !   o----o
                SURFACE_NUMBER = 6 ! number of surface
                IX => IXS(1:NIXS,1:NUMELS)
                POINTER_FACE => FACES(1:4,1:6)
                NODE_SURF_NB = 4 ! number of node per surface
                SHIFT_ELM = OFFSET_SOLID
            ELSEIF(ELEM_ID<=OFFSET_SHELL) THEN
                !   quad element
                DO_COMPUTATION = .FALSE.
            ELSEIF(ELEM_ID<=OFFSET_TRUSS) THEN
                ! shell element
                ! 4 nodes / 1 surface
                !   o----o 
                !   |    |
                !   |    |
                !   o----o
                SURFACE_NUMBER = 1 ! number of surface
                IX => IXC(1:NIXC,1:NUMELC)
                POINTER_FACE => FACES_SHELL(1:4,1:1)
                NODE_SURF_NB = 4 ! number of node per surface
                SHIFT_ELM = OFFSET_SHELL
            ELSEIF(ELEM_ID<=OFFSET_BEAM) THEN
                !   truss element
                DO_COMPUTATION = .FALSE.
            ELSEIF(ELEM_ID<=OFFSET_SPRING) THEN
                !   beam element
                DO_COMPUTATION = .FALSE.
            ELSEIF(ELEM_ID<=OFFSET_TRIANGLE) THEN
                !   spring element
                DO_COMPUTATION = .FALSE.
            ELSEIF(ELEM_ID<=OFFSET_UR) THEN
                ! triangle element
                ! 3 nodes / 1 surface
                !       o
                !      / \
                !     /   \
                !    o-----o
                SURFACE_NUMBER = 1 ! number of surface
                IX => IXTG(1:NIXTG,1:NUMELTG)
                POINTER_FACE => FACES_SHELL(1:4,1:1)
                NODE_SURF_NB = 3 ! number of node per surface
                SHIFT_ELM = OFFSET_TRIANGLE
            ELSE
                ! user element   
                DO_COMPUTATION = .FALSE.         
            ENDIF
            ! ----------------------
            IF(DO_COMPUTATION) THEN
                ! ----------------------
                ! loop over the surfaces of the element
                DO K=1,SURFACE_NUMBER
                    SEVERAL_PROC = 0
                    SEVERAL_SURF = 0
                    NO_SURF = .FALSE.
                    ! ------------------
                    ! first node of the surface face(1:4,k)
                    J = 1
                    N = POINTER_FACE(J,K)                ! get the node of the surfaces
                    NODE_ID = IX(N+1,ELEM_ID-SHIFT_ELM)    ! get the node ID    
                    LOCAL_NODE(1) = NODE_ID     
       
                    NB_RESULT_INTERSECT = SHOOT_STRUCT%SHIFT_M_NODE_SURF(NODE_ID+1) - SHOOT_STRUCT%SHIFT_M_NODE_SURF(NODE_ID)   ! get the number of surface of the node
                    SHIFT = SHOOT_STRUCT%SHIFT_M_NODE_SURF(NODE_ID)
                    RESULT_INTERSECT(1:NB_RESULT_INTERSECT) = SHOOT_STRUCT%M_NODE_SURF( SHIFT+1:SHIFT+NB_RESULT_INTERSECT )

                    NB_RESULT_INTERSECT_2 = SHOOT_STRUCT%SHIFT_M_NODE_PROC(NODE_ID+1) - SHOOT_STRUCT%SHIFT_M_NODE_PROC(NODE_ID) ! get the number of processor of the node 
                    SHIFT = SHOOT_STRUCT%SHIFT_M_NODE_PROC(NODE_ID)
                    RESULT_INTERSECT_2(1:NB_RESULT_INTERSECT_2) = SHOOT_STRUCT%M_NODE_PROC( SHIFT+1:SHIFT+NB_RESULT_INTERSECT_2 )
 
                    IF(NB_RESULT_INTERSECT_2>1) THEN
                        SEVERAL_PROC = SEVERAL_PROC + 1
                    ELSEIF(NB_RESULT_INTERSECT_2<1) THEN
                        ! this case is not possible, i hope i'm not here :)
                    ENDIF
                    ! ------------------

                    DO J=2,NODE_SURF_NB
                        NB_SURFACE_1 = NB_RESULT_INTERSECT
                        INTERSECT_1(1:NB_SURFACE_1) = RESULT_INTERSECT(1:NB_RESULT_INTERSECT)

                        N = POINTER_FACE(J,K)                ! get the node of the surfaces
                        NODE_ID = IX(N+1,ELEM_ID-SHIFT_ELM)    ! get the node ID  
                        LOCAL_NODE(J) = NODE_ID
                        ! -----------------------         
                        ! intersection of surface 
                        NB_SURFACE_2 = SHOOT_STRUCT%SHIFT_M_NODE_SURF(NODE_ID+1) - SHOOT_STRUCT%SHIFT_M_NODE_SURF(NODE_ID)   ! get the number of surface of the node
                        SHIFT = SHOOT_STRUCT%SHIFT_M_NODE_SURF(NODE_ID)
                        INTERSECT_2(1:NB_SURFACE_2) = SHOOT_STRUCT%M_NODE_SURF( SHIFT+1:SHIFT+NB_SURFACE_2 )
                        IF(NB_SURFACE_1>0.AND.NB_SURFACE_2>0) THEN
                            CALL INTERSECT_2_SORTED_SETS( INTERSECT_1,NB_SURFACE_1,
     .                                                    INTERSECT_2,NB_SURFACE_2,
     .                                                    RESULT_INTERSECT,NB_RESULT_INTERSECT )
                        ELSE
                            NB_RESULT_INTERSECT = 0
                        ENDIF
                        ! end : intersection of surface 
                        ! -----------------------

                        ! -----------------------         
                        ! intersection of processor 
                        NB_PROC_1 = NB_RESULT_INTERSECT_2
                        INTERSECT_3(1:NB_PROC_1) = RESULT_INTERSECT_2(1:NB_PROC_1)

                        NB_PROC_2 = SHOOT_STRUCT%SHIFT_M_NODE_PROC(NODE_ID+1) - SHOOT_STRUCT%SHIFT_M_NODE_PROC(NODE_ID) ! get the number of processor of the node     
                        IF(NB_PROC_1>1.AND.NB_PROC_2>1) THEN
                            SEVERAL_PROC = SEVERAL_PROC + 1
                            ! -----------------------         
                            ! intersection of processor 
                            SHIFT = SHOOT_STRUCT%SHIFT_M_NODE_PROC(NODE_ID)
                            INTERSECT_4(1:NB_PROC_2) = SHOOT_STRUCT%M_NODE_PROC( SHIFT+1:SHIFT+NB_PROC_2 )

                            CALL INTERSECT_2_SORTED_SETS( INTERSECT_3,NB_PROC_1,
     .                                                    INTERSECT_4,NB_PROC_2,
     .                                                    RESULT_INTERSECT_2,NB_RESULT_INTERSECT_2 )
                            ! -----------------------
                        ELSEIF(NB_PROC_2<1) THEN
                            ! this case is not possible, i hope i'm not here :)
                        ELSE
                            NB_RESULT_INTERSECT_2 = 0
                        ENDIF


                        ! end : intersection of processor 
                        ! -----------------------
                    ENDDO

                    IF(NB_RESULT_INTERSECT>0) THEN
                        ! one or several surface on the current processor
                        ! save the surface id

                        IF( SHOOT_STRUCT%SAVE_SURFACE_NB+NB_RESULT_INTERSECT>SHOOT_STRUCT%S_SAVE_SURFACE) THEN
                            ALLOCATE( TMP_ARRAY(SHOOT_STRUCT%S_SAVE_SURFACE) )
                            TMP_ARRAY(1:SHOOT_STRUCT%S_SAVE_SURFACE) =
     .                          SHOOT_STRUCT%SAVE_SURFACE(1:SHOOT_STRUCT%S_SAVE_SURFACE)

                            DEALLOCATE( SHOOT_STRUCT%SAVE_SURFACE )
                            OLD_SIZE = SHOOT_STRUCT%S_SAVE_SURFACE
                            SHOOT_STRUCT%S_SAVE_SURFACE = 1.20*(SHOOT_STRUCT%S_SAVE_SURFACE+5*NB_RESULT_INTERSECT)
                            ALLOCATE( SHOOT_STRUCT%SAVE_SURFACE( SHOOT_STRUCT%S_SAVE_SURFACE ) )
                            SHOOT_STRUCT%SAVE_SURFACE(1:OLD_SIZE) =  TMP_ARRAY(1:OLD_SIZE)
                            DEALLOCATE( TMP_ARRAY )
                        ENDIF
                        DO J=1,NB_RESULT_INTERSECT
                            SHOOT_STRUCT%SAVE_SURFACE_NB = SHOOT_STRUCT%SAVE_SURFACE_NB + 1
                            SHOOT_STRUCT%SAVE_SURFACE( SHOOT_STRUCT%SAVE_SURFACE_NB ) =  RESULT_INTERSECT(J)
                        ENDDO
                    ENDIF
                    IF(NB_RESULT_INTERSECT_2>1) THEN        !SEVERAL_PROC==NODE_SURF_NB) THEN
                        ! one or several surface on a remote processor : 
                        ! save the remote proc id & the node id
                        ! |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  10  |
                        !   pi    n1    n2     n3   n4    pj    n1     n2   n3     n3
                        !  proc id & the 4 nodes        | proc id & the 3 nodes of triangle + n4=n3

                        IF( SHOOT_STRUCT%SAVE_PROC_NB+5*(NB_RESULT_INTERSECT_2-1)>SHOOT_STRUCT%S_SAVE_PROC) THEN
                            ALLOCATE( TMP_ARRAY(SHOOT_STRUCT%S_SAVE_PROC) )
                            TMP_ARRAY(1:SHOOT_STRUCT%S_SAVE_PROC) =
     .                          SHOOT_STRUCT%SAVE_PROC(1:SHOOT_STRUCT%S_SAVE_PROC)

                            DEALLOCATE( SHOOT_STRUCT%SAVE_PROC )
                            OLD_SIZE = SHOOT_STRUCT%S_SAVE_PROC
                            SHOOT_STRUCT%S_SAVE_PROC = 1.20*(SHOOT_STRUCT%SAVE_PROC_NB+5*(NB_RESULT_INTERSECT_2-1))
                            ALLOCATE( SHOOT_STRUCT%SAVE_PROC( SHOOT_STRUCT%S_SAVE_PROC ) )
                            SHOOT_STRUCT%SAVE_PROC(1:OLD_SIZE) =  TMP_ARRAY(1:OLD_SIZE)
                            DEALLOCATE( TMP_ARRAY )
                        ENDIF

                        DO J=1,NB_RESULT_INTERSECT_2
                            IF(RESULT_INTERSECT_2(J)/=ISPMD+1) THEN
                                SHOOT_STRUCT%SAVE_PROC_NB = SHOOT_STRUCT%SAVE_PROC_NB + 1
                                SHOOT_STRUCT%SAVE_PROC( SHOOT_STRUCT%SAVE_PROC_NB ) =  RESULT_INTERSECT_2(J)    ! save the remote proc id

                                IF(NODE_SURF_NB==3) LOCAL_NODE(4) = LOCAL_NODE(3)
                                DO IJK=1,4
                                    SHOOT_STRUCT%SAVE_PROC_NB = SHOOT_STRUCT%SAVE_PROC_NB + 1
                                    SHOOT_STRUCT%SAVE_PROC( SHOOT_STRUCT%SAVE_PROC_NB ) =  ITAB(LOCAL_NODE(IJK))  ! convert local id to global id

                                ENDDO
                            ENDIF
                        ENDDO
                    ELSE
                        ! no surface on the current processor or on a remote processor
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
        DEALLOCATE( INTERSECT_1 )
        DEALLOCATE( INTERSECT_2 )
        ! working array : processor
        DEALLOCATE( RESULT_INTERSECT_2 )
        DEALLOCATE( INTERSECT_3 )
        DEALLOCATE( INTERSECT_4 )
        ! --------------------------

        RETURN
        END SUBROUTINE FIND_SURFACE_INTER
