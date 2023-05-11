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
Chd|  CHECK_ACTIVE_ELEM_EDGE        source/interfaces/interf/check_active_elem_edge.F
Chd|-- called by -----------
Chd|        CHECK_EDGE_STATE              source/interfaces/interf/check_edge_state.F
Chd|        CHECK_SURFACE_STATE           source/interfaces/interf/check_surface_state.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CHECK_ACTIVE_ELEM_EDGE( NUMBER_NODE, N1,N2,N3,N4,
     .                                     DEACTIVATION,GEO,IXS,IXC,
     .                                     IXT,IXP,IXR,IXTG,ADDCNEL,ITABM1,CNEL,
     .                                     TAG_NODE,TAG_ELEM )
!$COMMENT
!       CHECK_ACTIVE_ELEM_EDGE description
!           check if a element associated to an edge is active
!       CHECK_ACTIVE_ELEM_EDGE organization
!           loop over the element associated to the N1 node
!           if 1 active element contains N1 & N2, the edge must be kept (for interface 11)
!           if 1 active element contains N1 & N2 & N3 & N4, the surface must be kept (for interface 7)
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
        LOGICAL, INTENT(inout) :: DEACTIVATION
        INTEGER, INTENT(in) :: NUMBER_NODE ! number of node of the edge/surface
        INTEGER, INTENT(in) :: N1,N2,N3,N4 ! node id : for interface type11 -> N3&N4 are fake node id (ie. =0)
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
        INTEGER, DIMENSION(NUMNOD), INTENT(inout) :: TAG_NODE
        INTEGER, DIMENSION(NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR+NUMELTG), INTENT(inout) :: TAG_ELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        LOGICAL :: STILL_COMPUTE,STILL_ALIVE
        INTEGER :: I,J,K
        INTEGER :: NEXT
        INTEGER :: ELEM_ID,NODE_ID,OTHER_NODE
        INTEGER :: OFFSET_SOLID,OFFSET_QUAD,OFFSET_SHELL
        INTEGER :: OFFSET_TRUSS,OFFSET_BEAM,OFFSET_SPRING
        INTEGER :: OFFSET_TRIANGLE,OFFSET_UR
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

        STILL_COMPUTE = .TRUE.
        NEXT = 0
        I = ADDCNEL(N1) + NEXT
        DEACTIVATION = .FALSE.
        STILL_ALIVE = .FALSE.

        ! ------------------
        ! check if 1 or more elements is/are associated to 
        ! N1 node
        ! if no element, the edge can be deactivated (only if
        ! the remote elements associated to N1/N2 are all deleted)
        IF((ADDCNEL(N1+1) - ADDCNEL(N1)) ==0) THEN
            DEACTIVATION = .TRUE.
            STILL_COMPUTE = .FALSE.
        ENDIF
        ! ------------------

        DO WHILE( STILL_COMPUTE )

            ELEM_ID = CNEL(I)
            TAG_NODE(N1) = 0
            TAG_NODE(N2) = 0
            IF(NUMBER_NODE>2) THEN
                TAG_NODE(N3) = 0
                TAG_NODE(N4) = 0
            ENDIF
            ! -------------------------
            ! loop over the active element
            ! for interface 11 : if still 1 or more active elements with N1 & N2 : need to keep the edge
            !                    else : need to deactivate the edge
            ! for interface 7 : if still 1 or more active elements with N1 & N2 & N3 & N4 : need to keep the surface
            !                    else : need to deactivate the surface
            IF(TAG_ELEM(ELEM_ID)>0) THEN
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
                ! for interface 7 : need to check if the element has N3 & N4
                OTHER_NODE = 0
                IF(NUMBER_NODE>2) OTHER_NODE = TAG_NODE(N3)
                IF(NUMBER_NODE>3) OTHER_NODE = TAG_NODE(N4)

                ! -----------------
        
                ! -----------------
                ! check if the element has N1 & N2
                IF(TAG_NODE(N1)+TAG_NODE(N2)+OTHER_NODE==NUMBER_NODE) THEN
                    STILL_COMPUTE = .FALSE.
                    STILL_ALIVE = .TRUE.
                ENDIF
                ! -----------------
            ENDIF
            NEXT = NEXT + 1
            I = ADDCNEL(N1) + NEXT
            IF(I>ADDCNEL(N1+1)-1) THEN
                STILL_COMPUTE = .FALSE.
            ENDIF
            ! -------------------------
        ENDDO

        IF(.NOT.STILL_ALIVE) THEN
            DEACTIVATION = .TRUE.
        ENDIF

        RETURN
        END SUBROUTINE CHECK_ACTIVE_ELEM_EDGE

