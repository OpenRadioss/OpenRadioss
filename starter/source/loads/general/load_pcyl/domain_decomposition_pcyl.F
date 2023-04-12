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
Chd|====================================================================
        SUBROUTINE DOMAIN_DECOMPOSITION_PCYL(LOADS,IFRAME)
!$COMMENT
!       DOMAIN_DECOMPOSITION_PCYL description
!       DOMAIN_DECOMPOSITION_PCYL affects a segment to a processor
!       
!       DOMAIN_DECOMPOSITION_PCYL organization :
!       loop over the segments of the surface of the load option to :
!           * find where the nodes are defined (a segment is defined by 3 or 4 nodes)
!           * affect a processor to the segment
!           * save the info per processor for the splitting
!           * force the node of the frame used by the load on the processors 
!             where /load nodes are defined
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE LOADS_MOD
        USE ARRAY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
! nspmd definition
#include "com01_c.inc"
! numnod & numfram definition
#include "com04_c.inc"
! liskn definition
#include "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE(LOADS_),INTENT(INOUT) :: LOADS ! structure of load cyl
        INTEGER ,DIMENSION(LISKN,NUMFRAM+1)   ,INTENT(IN) :: IFRAME ! frame data structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        LOGICAL :: NEXT_OPERATION
        INTEGER :: I,J,K
        INTEGER :: NUMBER_SEGMENT,LOCAL_NUMBER_SEG,NUMBER_NODE
        INTEGER :: NODE_ID,MY_PROC,MY_SIZE,FRAME_ID
        INTEGER :: NUMBER_PROC,NUMBER_PROC_1,NUMBER_PROC_2
        INTEGER, DIMENSION(NSPMD) :: LIST_1,LIST_2
        INTEGER :: NB_RESULT_INTERSECT
        INTEGER, DIMENSION(NSPMD) :: RESULT_INTERSECT
        INTEGER, DIMENSION(NSPMD) :: PROC_ARRAY
        LOGICAL, DIMENSION(:), ALLOCATABLE :: BOOL
        INTEGER, DIMENSION(:), ALLOCATABLE :: TEMP_ARRAY
        TYPE(array_type), DIMENSION(:), ALLOCATABLE :: LOCAL_ARRAY
        INTEGER :: NB_LOCAL_ARRAY
        INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX_LOCAL_ARRAY
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
        ALLOCATE( BOOL(NUMNOD) )
        BOOL(1:NUMNOD) = .true.
        NB_LOCAL_ARRAY = 0
        ALLOCATE( INDEX_LOCAL_ARRAY(NUMNOD) )
        ALLOCATE( LOCAL_ARRAY(NUMNOD) )
        LOCAL_ARRAY(1:NUMNOD)%SIZE_INT_ARRAY_1D = 0

        IF(.NOT.ALLOCATED(LOADS%CYL_RESTART)) ALLOCATE( LOADS%CYL_RESTART(LOADS%NLOAD_CYL) )
        ! -------------------
        ! loop over the /LOAD/PCYL
        DO I=1,LOADS%NLOAD_CYL
            NUMBER_SEGMENT = LOADS%LOAD_CYL(I)%NSEG ! number of segment for the PCYL I
            ! ------------
            ! loop over the segments of the surface to find where the node id are defined 
            DO J=1,NUMBER_SEGMENT ! loop over the segments of the surface
                DO K=1,4
                    NODE_ID = LOADS%LOAD_CYL(I)%SEGNOD(J,K) ! get the node id (if the segment is a triangle, NODE_ID(node 4) = 0))
                    NUMBER_PROC = 0
                    IF(NODE_ID/=0) THEN
                        IF(BOOL(NODE_ID)) THEN
                            CALL PLIST_IFRONT(PROC_ARRAY,NODE_ID,NUMBER_PROC)
                            BOOL(NODE_ID) = .false.
                            LOCAL_ARRAY(NODE_ID)%SIZE_INT_ARRAY_1D = NUMBER_PROC
                            CALL ALLOC_1D_ARRAY(LOCAL_ARRAY(NODE_ID))
                            LOCAL_ARRAY(NODE_ID)%INT_ARRAY_1D(1:NUMBER_PROC) = PROC_ARRAY(1:NUMBER_PROC)
                            NB_LOCAL_ARRAY = NB_LOCAL_ARRAY + 1
                            INDEX_LOCAL_ARRAY(NB_LOCAL_ARRAY) = NODE_ID
                        ENDIF
                    ENDIF
                ENDDO
            ENDDO
            ! ------------
            
            ! ------------
            IF(.NOT.ALLOCATED(LOADS%CYL_RESTART(I)%SEGMENT_TO_PROC)) THEN
                ALLOCATE( LOADS%CYL_RESTART(I)%SEGMENT_TO_PROC(NUMBER_SEGMENT) )
            ENDIF
            IF(.NOT.ALLOCATED(LOADS%CYL_RESTART(I)%PROC)) ALLOCATE( LOADS%CYL_RESTART(I)%PROC(NSPMD) )
            DO J=1,NSPMD
                LOADS%CYL_RESTART(I)%PROC(J)%LOCAL_SEGMENT_NUMBER = 0
                LOADS%CYL_RESTART(I)%PROC(J)%S_LOCAL_SEGMENT = NUMBER_SEGMENT / NSPMD + 4
                MY_SIZE = LOADS%CYL_RESTART(I)%PROC(J)%S_LOCAL_SEGMENT
                IF(ALLOCATED(LOADS%CYL_RESTART(I)%PROC(J)%LOCAL_SEGMENT)) THEN
                    DEALLOCATE( LOADS%CYL_RESTART(I)%PROC(J)%LOCAL_SEGMENT )
                ENDIF
                ALLOCATE( LOADS%CYL_RESTART(I)%PROC(J)%LOCAL_SEGMENT(MY_SIZE) )
            ENDDO

            DO J=1,NUMBER_SEGMENT
                NUMBER_NODE = 4
                NODE_ID = LOADS%LOAD_CYL(I)%SEGNOD(J,4)
                IF(NODE_ID==0) NUMBER_NODE = 3

                NODE_ID = LOADS%LOAD_CYL(I)%SEGNOD(J,1)
                NUMBER_PROC_1 = LOCAL_ARRAY(NODE_ID)%SIZE_INT_ARRAY_1D
                LIST_1(1:NUMBER_PROC_1) = LOCAL_ARRAY(NODE_ID)%INT_ARRAY_1D(1:NUMBER_PROC_1)
                NEXT_OPERATION = .true.
                ! -----------------------
                ! find the processors where the nodes are defined
                ! if the nodes are on 2 or more processors, neet to choose the main processor
                ! (main processor is the processor that will compute the surface in the engine)
                DO K=2,NUMBER_NODE
                    NODE_ID = LOADS%LOAD_CYL(I)%SEGNOD(J,K)
                    NUMBER_PROC_2 = LOCAL_ARRAY(NODE_ID)%SIZE_INT_ARRAY_1D
                    LIST_2(1:NUMBER_PROC_2) = LOCAL_ARRAY(NODE_ID)%INT_ARRAY_1D(1:NUMBER_PROC_2)
                    ! -----------------------         
                    ! intersection of processor 
                    IF(NEXT_OPERATION) THEN
                        CALL INTERSECT_2_SORTED_SETS( LIST_1,NUMBER_PROC_1,
     .                                                LIST_2,NUMBER_PROC_2,
     .                                                RESULT_INTERSECT,NB_RESULT_INTERSECT )
                        IF(NB_RESULT_INTERSECT>0) THEN
                            LIST_1(1:NB_RESULT_INTERSECT) = RESULT_INTERSECT(1:NB_RESULT_INTERSECT)
                            NUMBER_PROC_1 = NB_RESULT_INTERSECT
                        ENDIF
                    ELSE
                        NB_RESULT_INTERSECT = 0
                    ENDIF
                    NEXT_OPERATION = (NB_RESULT_INTERSECT>0)
                    ! end : intersection of surface 
                    ! -----------------------
                ENDDO
                ! -----------------------

                IF(NB_RESULT_INTERSECT>0) THEN
                    MY_PROC = LIST_1(1) ! choose the first processor of the list
                ELSE
                    MY_PROC = LIST_1(1) ! choose the first processor of the list and add the 4 nodes on this processor
                    DO K=1,NUMBER_NODE
                        NODE_ID = LOADS%LOAD_CYL(I)%SEGNOD(J,K)
                        CALL IFRONTPLUS(NODE_ID,MY_PROC) ! add the missig node on the processor MY_PROC
                    ENDDO
                ENDIF


                LOADS%CYL_RESTART(I)%SEGMENT_TO_PROC(J) = MY_PROC
                LOCAL_NUMBER_SEG = LOADS%CYL_RESTART(I)%PROC(MY_PROC)%LOCAL_SEGMENT_NUMBER

        
                MY_SIZE = LOADS%CYL_RESTART(I)%PROC(MY_PROC)%S_LOCAL_SEGMENT
                IF(MY_SIZE< LOCAL_NUMBER_SEG + 1 ) THEN
                    ALLOCATE(TEMP_ARRAY(LOCAL_NUMBER_SEG))
                    TEMP_ARRAY(1:LOCAL_NUMBER_SEG) = 
     .               LOADS%CYL_RESTART(I)%PROC(MY_PROC)%LOCAL_SEGMENT(1:LOCAL_NUMBER_SEG)
                    DEALLOCATE( LOADS%CYL_RESTART(I)%PROC(MY_PROC)%LOCAL_SEGMENT )
                    LOADS%CYL_RESTART(I)%PROC(MY_PROC)%S_LOCAL_SEGMENT = MY_SIZE *1.2 + 4
                    MY_SIZE = LOADS%CYL_RESTART(I)%PROC(MY_PROC)%S_LOCAL_SEGMENT
                    ALLOCATE( LOADS%CYL_RESTART(I)%PROC(MY_PROC)%LOCAL_SEGMENT(MY_SIZE) )
                    LOADS%CYL_RESTART(I)%PROC(MY_PROC)%LOCAL_SEGMENT(1:LOCAL_NUMBER_SEG) = 
     .               TEMP_ARRAY(1:LOCAL_NUMBER_SEG)
                    DEALLOCATE(TEMP_ARRAY)
                ENDIF
                LOCAL_NUMBER_SEG = LOCAL_NUMBER_SEG + 1
                LOADS%CYL_RESTART(I)%PROC(MY_PROC)%LOCAL_SEGMENT_NUMBER = LOCAL_NUMBER_SEG
                LOADS%CYL_RESTART(I)%PROC(MY_PROC)%LOCAL_SEGMENT( LOCAL_NUMBER_SEG ) = J
            ENDDO
            ! ------------

            ! ------------
            ! add the node of the frame on the processors with /load nodes 
            DO J=1,NSPMD
                IF(LOADS%CYL_RESTART(I)%PROC(J)%LOCAL_SEGMENT_NUMBER>0) THEN
                    FRAME_ID = LOADS%LOAD_CYL(I)%IFRAME + 1
                    IF(FRAME_ID>0) THEN
                        DO K=1,2
                            NODE_ID = IFRAME(K,FRAME_ID)
                            CALL IFRONTPLUS(NODE_ID,J)
                        ENDDO
                    ENDIF
                ENDIF
            ENDDO
            ! ------------
        ENDDO
        ! -------------------

        ! loop over the nodes used by the /load to deallocate LOCAL_ARRAY%... array
        DO J=1,NB_LOCAL_ARRAY
            NODE_ID = INDEX_LOCAL_ARRAY(J)
            CALL DEALLOC_1D_ARRAY(LOCAL_ARRAY(NODE_ID))
        ENDDO
        ! ------------

        DEALLOCATE( BOOL )
        DEALLOCATE( LOCAL_ARRAY )
        RETURN
        END SUBROUTINE DOMAIN_DECOMPOSITION_PCYL
