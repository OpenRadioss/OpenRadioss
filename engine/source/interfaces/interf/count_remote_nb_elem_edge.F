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
Chd|  COUNT_REMOTE_NB_ELEM_EDGE     source/interfaces/interf/count_remote_nb_elem_edge.F
Chd|-- called by -----------
Chd|        INIT_NODAL_STATE              source/interfaces/interf/init_nodal_state.F
Chd|-- calls ---------------
Chd|        SYSFUS2                       source/system/sysfus.F        
Chd|====================================================================
        SUBROUTINE COUNT_REMOTE_NB_ELEM_EDGE( SIZE_BUFFER,BUFFER,GEO,IXS,IXC,
     .                                 IXT,IXP,IXR,IXTG,ADDCNEL,ITABM1,CNEL,CHUNK)
!$COMMENT
!       COUNT_REMOTE_NB_ELEM_EDGE description
!           check if a list of node is associated 
!           to a deleted element : 
!           - if it's true : need to deactivate the node from the interface
!       COUNT_REMOTE_NB_ELEM_EDGE organization
!           loop over the 2 or 4 nodes:
!           - convert the global id to local id
!           - check if a element is associated to the list of node (2 or 4 nodes)
!           - if it's true, send to the remote processor "you need to deactivate
!             the nodes from your interface!"
!$ENDCOMMENT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: SIZE_BUFFER
        INTEGER, INTENT(in) :: CHUNK ! chunk size : 1-> interface id, 2-> surface id, 3:6-> node id (for type11, 5&6 values are equal to 0)
        INTEGER, DIMENSION(SIZE_BUFFER), INTENT(inout) :: BUFFER
        INTEGER, DIMENSION(NIXS,NUMELS),TARGET, INTENT(in) :: IXS   ! solid array
        INTEGER, DIMENSION(NIXC,NUMELC),TARGET, INTENT(in) :: IXC   ! shell array
        INTEGER, DIMENSION(NIXT,NUMELT),TARGET, INTENT(in) :: IXT! truss array
        INTEGER, DIMENSION(NIXP,NUMELP),TARGET, INTENT(in) :: IXP! beam array
        INTEGER, DIMENSION(NIXR,NUMELR),TARGET, INTENT(in) :: IXR! spring array
        INTEGER, DIMENSION(NIXTG,NUMELTG),TARGET, INTENT(in) :: IXTG! triangle array
        INTEGER, DIMENSION(0:NUMNOD+1), INTENT(in) :: ADDCNEL ! address for the CNEL array
        INTEGER, DIMENSION(NUMNOD), INTENT(in) :: ITABM1    ! array of global id
        my_real, DIMENSION(NPROPG,NUMGEO), INTENT(in) :: GEO
        INTEGER, DIMENSION(0:LCNEL), INTENT(in) :: CNEL ! connectivity node-->element
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,J,K
        INTEGER :: N1,N2,N3,N4,NIN
        INTEGER :: NUMBER_NODE
        INTEGER, DIMENSION(4) :: LOCAL_NODE,GLOBAL_NODE
        INTEGER :: ELEM_ID,NODE_ID
        INTEGER :: OFFSET_SOLID,OFFSET_QUAD,OFFSET_SHELL
        INTEGER :: OFFSET_TRUSS,OFFSET_BEAM,OFFSET_SPRING
        INTEGER :: OFFSET_TRIANGLE,OFFSET_UR

        INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_NODE
        INTEGER :: NUMBER_ELEMENT, NUMBER_ELEMENT_HERE
        INTEGER, DIMENSION(:), ALLOCATABLE :: LIST_ELEMENT
        LOGICAL, DIMENSION(:), ALLOCATABLE :: ALREADY_HERE

        INTEGER :: SYSFUS2 ! external function : give the local ID of a global node ID
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
        ! allocation of already_here array 
        ! cnel is not bijective (2 differents index can give the same cnel(i) value)
        ! --> need to tag the elements to avoid a duplication
        NUMBER_ELEMENT = NUMELS + NUMELQ + NUMELC + NUMELT + NUMELP + NUMELR + NUMELTG
        ALLOCATE( LIST_ELEMENT(NUMBER_ELEMENT) ) ! <-- list of element to flush only the modified cells
        ALLOCATE( ALREADY_HERE(NUMBER_ELEMENT) ) ! <-- boolean to avoid any element duplications
        ALREADY_HERE(1:NUMBER_ELEMENT) = .FALSE.
        NUMBER_ELEMENT_HERE = 0

        ALLOCATE( TAG_NODE(NUMNOD+1) )
        TAG_NODE(1:NUMNOD+1) = 0
        DO J=1,SIZE_BUFFER,CHUNK
            NIN = BUFFER(J)
            GLOBAL_NODE(1:4) = BUFFER(J+2:J+CHUNK-1)
            LOCAL_NODE(1) = SYSFUS2(GLOBAL_NODE(1),ITABM1,NUMNOD)
            LOCAL_NODE(2) = SYSFUS2(GLOBAL_NODE(2),ITABM1,NUMNOD)
            LOCAL_NODE(3:4) = NUMNOD+1
            NUMBER_NODE = 2 ! for type 11, there are 2 nodes per segments
            IF(GLOBAL_NODE(3)/=0) LOCAL_NODE(3) = SYSFUS2(GLOBAL_NODE(3),ITABM1,NUMNOD)
            IF(GLOBAL_NODE(4)/=0) LOCAL_NODE(4) = SYSFUS2(GLOBAL_NODE(4),ITABM1,NUMNOD)
            IF( (GLOBAL_NODE(3)/=0).AND.(GLOBAL_NODE(4)/=0) ) NUMBER_NODE = 4 ! for type 7, there are 4 nodes per segments
            BUFFER(J+2:J+3) = 0
            N1 = LOCAL_NODE(1)
            N2 = LOCAL_NODE(2)
            N3 = LOCAL_NODE(3)
            N4 = LOCAL_NODE(4)
            NUMBER_ELEMENT_HERE = 0
            ! ---------------
            ! loop over the elements connected to N1 
            DO I=ADDCNEL(N1),ADDCNEL(N1+1)-1
                ELEM_ID = CNEL(I)   ! <-- element id
                TAG_NODE(N1) = 0
                TAG_NODE(N2) = 0
                TAG_NODE(N3) = 0
                TAG_NODE(N4) = 0
                ! -----------------
                ! solid element
                IF(ELEM_ID<=OFFSET_SHELL) THEN
                    DO K=2,9
                        NODE_ID = IXS(K,ELEM_ID)
                        TAG_NODE(NODE_ID) = 1
                    ENDDO
                ELSEIF(ELEM_ID>OFFSET_SHELL.AND.ELEM_ID<=OFFSET_TRUSS) THEN
                ! -----------------
                ! shell element
                    DO K=2,5
                        NODE_ID = IXC(K,ELEM_ID-OFFSET_SHELL)
                        TAG_NODE(NODE_ID) = 1
                    ENDDO
                ELSEIF(ELEM_ID>OFFSET_TRUSS.AND.ELEM_ID<=OFFSET_BEAM) THEN
                ! -----------------
                ! truss element
                    DO K=2,3
                        NODE_ID = IXT(K,ELEM_ID-OFFSET_TRUSS)
                        TAG_NODE(NODE_ID) = 1
                    ENDDO
                ELSEIF(ELEM_ID>OFFSET_BEAM.AND.ELEM_ID<=OFFSET_SPRING) THEN
                ! -----------------
                ! beam element
                    DO K=2,3
                        NODE_ID = IXP(K,ELEM_ID-OFFSET_BEAM)
                        TAG_NODE(NODE_ID) = 1
                    ENDDO
                ELSEIF(ELEM_ID>OFFSET_SPRING.AND.ELEM_ID<=OFFSET_TRIANGLE) THEN
                ! -----------------
                ! spring element
                    DO K=2,3
                        NODE_ID = IXR(K,ELEM_ID-OFFSET_SPRING)
                        TAG_NODE(NODE_ID) = 1
                    ENDDO

                    IF(NINT(GEO(12,IXR(1,ELEM_ID-OFFSET_SPRING))) == 12) THEN
                        NODE_ID = IXR(4,ELEM_ID-OFFSET_SPRING)
                        TAG_NODE(NODE_ID) = 1 
                    ENDIF
                ELSEIF(ELEM_ID>OFFSET_TRIANGLE.AND.ELEM_ID<=OFFSET_UR) THEN
                ! -----------------
                ! triangle element
                    DO K=2,4
                        NODE_ID = IXTG(K,ELEM_ID-OFFSET_TRIANGLE)
                        TAG_NODE(NODE_ID) = 1
                    ENDDO
                ENDIF
                ! -----------------

                ! -----------------
                ! check if the element has N1 & N2 & N3 & N4 (if N3 and N4 are real nodes)
                IF( (TAG_NODE(N1)+TAG_NODE(N2)+TAG_NODE(N3)+TAG_NODE(N4)) == NUMBER_NODE) THEN
                    IF( .NOT.ALREADY_HERE(ELEM_ID) ) THEN ! check if the element is already taken into account
                        BUFFER(J+2) = BUFFER(J+2) + 1
                        ALREADY_HERE(ELEM_ID) = .TRUE. ! <-- the element is now taken into account
                        NUMBER_ELEMENT_HERE = NUMBER_ELEMENT_HERE  + 1
                        LIST_ELEMENT(NUMBER_ELEMENT_HERE) = ELEM_ID
                    ENDIF
                ENDIF

                ! -----------------
            ENDDO
            ! ---------------

            ! ---------------
            ! re-initialization of already_here array for the next node
            DO I=1,NUMBER_ELEMENT_HERE
                ELEM_ID = LIST_ELEMENT(I)
                ALREADY_HERE(ELEM_ID) = .FALSE.
            ENDDO
            ! ---------------
        ENDDO

        DEALLOCATE( TAG_NODE )
        DEALLOCATE( LIST_ELEMENT )
        DEALLOCATE( ALREADY_HERE )

        RETURN
        END SUBROUTINE COUNT_REMOTE_NB_ELEM_EDGE

