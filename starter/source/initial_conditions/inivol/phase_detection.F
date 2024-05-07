Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  PHASE_DETECTION               source/initial_conditions/inivol/phase_detection.F
Chd|-- called by -----------
Chd|        GETPHASE                      source/initial_conditions/inivol/getphase.F
Chd|-- calls ---------------
Chd|        FIND_CLOSEST_NODE             source/initial_conditions/inivol/find_closest_node.F
Chd|        IN_OUT_SIDE                   source/initial_conditions/inivol/in_out_side.F
Chd|        MYQSORT                       ../common_source/tools/sort/myqsort.F
Chd|        PHASE_PROPAGATION             source/initial_conditions/inivol/phase_propagation.F
Chd|====================================================================
        SUBROUTINE PHASE_DETECTION(LEADING_DIMENSION,NB_CELL_X,NB_CELL_Y,NB_CELL_Z,NB_BOX_LIMIT,
     .                             IPARG,IXS,IXQ,IXTG,X,ID_SURFACE,
     .                             CELL,CELL_POSITION,NODAL_PHASE,CLOSEST_NODE_ID,
     .                             NNOD2SURF,KNOD2SURF,INOD2SURF,
     .                             NOD_NORMAL,NSEG_USED,SEGTOSURF,NSEG,SURF_ELTYP,SURFACE_NODES,SWIFTSURF)
!$COMMENT
!       PHASE_DETECTION description
!       PHASE_DETECTION find the pseudo distance node - surface
!       pseudo distance = 1 (node in the phase 1), -1 (node in the phase -1), or a real distance 
!       there are 3 cases :
!        * a node is far from the surface and the pseudo distance of neighbour nodes is known
!              --> apply the pseudo distance of neighbour nodes to the node
!        * a node is far from the surface and the pseudo distance of neighbour nodes is unknown
!              --> compute the pseudo distance and apply it to the node
!        * a node is close to a surface --> need to compute the real distance to the surface
!       
!       PHASE_DETECTION organization :
!       - loop over the ALE element group
!           - for each ALE element group, loop over the NEL elements
!               - loop over the nodes of each element
!                   * if all the nodes are far from the surface
!                       * if the pseudo distance of at least 1 node is known --> apply it to the other nodes
!                       * if the pseudo distance of all nodes is unknown --> need to compute the pseudo distance &
!                                                                            apply it to the other nodes   
!                   * if at least one nead is near a surface --> need to compute the real distance
!$ENDCOMMENT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
! com01 : ngroup/n2d
#include "com01_c.inc" 
! com04 : numels/numelq/numeltg
#include "com04_c.inc"
! param : nparg
#include "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: LEADING_DIMENSION
        INTEGER, INTENT(IN) :: NB_BOX_LIMIT ! maximum number of cell 
        INTEGER, INTENT(IN) :: NB_CELL_X,NB_CELL_Y,NB_CELL_Z
        INTEGER, DIMENSION(NPARG,NGROUP), INTENT(IN) ::  IPARG  ! group data
        INTEGER, DIMENSION(NIXS,NUMELS),INTENT(IN), TARGET :: IXS ! solid data
        INTEGER, DIMENSION(NIXQ,NUMELQ),INTENT(IN), TARGET :: IXQ ! quad data
        INTEGER, DIMENSION(NIXTG,NUMELTG),INTENT(IN), TARGET :: IXTG ! triangle data
        INTEGER, DIMENSION(NUMNOD), INTENT(INOUT) :: NODAL_PHASE ! phase of nodes (in / out / near the surface)
        INTEGER, DIMENSION(NUMNOD), INTENT(INOUT) :: CLOSEST_NODE_ID ! list of closest node id
        INTEGER, DIMENSION(3,NUMNOD), INTENT(IN) :: CELL_POSITION ! position of node/cell
        INTEGER, DIMENSION(NB_CELL_X,NB_CELL_Y,NB_CELL_Z), INTENT(INOUT) :: CELL ! phase of the voxcell
        my_real, DIMENSION(3,NUMNOD), INTENT(IN) :: X ! position
        INTEGER, INTENT(IN) :: ID_SURFACE ! id of the surface
        INTEGER, DIMENSION(NSEG,4), INTENT(IN) :: SURFACE_NODES ! list of nodes for each segment of the surface
        INTEGER, INTENT(IN) :: NNOD2SURF,NSEG_USED ! size of SEGTOSURF & INOD2SURF arrays
        INTEGER, DIMENSION(NUMNOD+1), INTENT(IN) :: KNOD2SURF
        INTEGER, DIMENSION(NNOD2SURF,NUMNOD), INTENT(IN) :: INOD2SURF
        my_real, DIMENSION(3,NUMNOD), INTENT(IN) :: NOD_NORMAL
        INTEGER, DIMENSION(NSEG_USED), INTENT(IN) :: SEGTOSURF
        INTEGER, INTENT(IN) :: NSEG ! number of segment of the current surface
        INTEGER, DIMENSION(NSEG), INTENT(IN) :: SURF_ELTYP ! type of surface (shell or triangle)
        INTEGER, DIMENSION(NSURF), INTENT(IN) :: SWIFTSURF      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        LOGICAL :: BOOL,CONDITION
        INTEGER :: I,J,NG
        INTEGER :: MTN,NEL,NFT,ITY,ISOLNOD,INIVOL
        INTEGER :: INOD,NODE_NUMBER,FIRST,SURF_NODE_NUMBER,NODE_ID,CLOSEST_NODE
        INTEGER, DIMENSION(:,:), POINTER :: IXP
        INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_ARRAY,SURF_NODE_LIST

        INTEGER :: IX,IY,IZ,NEXT_NODE
        INTEGER :: MY_PHASE,OLD_PHASE
        INTEGER :: UNKNOWN_CELL_INDEX,LAST_UNKNOWN_CELL,CURRENT_UNKNOWN_CELL
        INTEGER, DIMENSION(8,3) :: POSITION_SAVE
        my_real :: DIST,EPS

        INTEGER :: info

        INTEGER, DIMENSION(:), ALLOCATABLE :: KEY1,KEY2,ID_LIST
        my_real, DIMENSION(:), ALLOCATABLE :: X_TMP
        my_real, DIMENSION(3) :: XN
        my_real, DIMENSION(:,:), ALLOCATABLE :: LOCAL_X
        INTEGER, DIMENSION(:), ALLOCATABLE:: LIST_NODE ! list of node close to the surface
        INTEGER, DIMENSION(32) :: ID_NODE_SAVE
C-----------------------------------------------
        ALLOCATE( TAG_ARRAY(NUMNOD) )   
        ALLOCATE( SURF_NODE_LIST(NUMNOD) )
        ALLOCATE( LIST_NODE(NUMNOD) )
        TAG_ARRAY(1:NUMNOD) = 0
        SURF_NODE_NUMBER = 0

        ! ----------------------      
        ! sort the node of the surface according a direction
        ! this sorting is usefull to reduce the elapsed time of the 
        ! NNS algo
        DO J=1,4
            DO I=1,NSEG
                NODE_ID = SURFACE_NODES(I,J)
                IF(TAG_ARRAY(NODE_ID)==0) THEN
                    TAG_ARRAY(NODE_ID) = 1
                    SURF_NODE_NUMBER = SURF_NODE_NUMBER + 1
                    SURF_NODE_LIST(SURF_NODE_NUMBER) = NODE_ID
                ENDIF
            ENDDO
        ENDDO
        
        ALLOCATE( X_TMP(SURF_NODE_NUMBER) )
        ALLOCATE( KEY2(SURF_NODE_NUMBER) )
        DO I=1,SURF_NODE_NUMBER
            X_TMP(I) = X(LEADING_DIMENSION,SURF_NODE_LIST(I))
            KEY2(I) = I
        ENDDO

        ! Sort according to chosen direction
        CALL MYQSORT(SURF_NODE_NUMBER,X_TMP,KEY2,info)
        DEALLOCATE( X_TMP )
        ! ----------------------

        TAG_ARRAY(1:NUMNOD) = 0
        NEXT_NODE = 0
        ALLOCATE( LOCAL_X(3,8) )

        ! -----------------------
        ! loop over the solid / quad / triangle elements with 51/151 material
        DO NG=1,NGROUP
            MTN = IPARG(1,NG)   ! material law
            NEL = IPARG(2,NG)   ! number of element 
            NFT = IPARG(3,NG)   ! adress of first element
            ITY = IPARG(5,NG)   ! type of element 
            ISOLNOD = IPARG(28,NG)
            INIVOL = IPARG(53,NG)
            IF(INIVOL<=0) CYCLE
            IF(MTN/=51.AND.MTN/=151) CYCLE
            IF(N2D ==0.AND.ITY/= 1)THEN
              CYCLE
            ELSEIF(N2D>0.AND.ITY/=7.AND.ITY/= 2)THEN
              CYCLE
            ENDIF
            ! ---------------
            ! depending on the king of element
            IF(ITY==1) THEN
                FIRST = 1
                NODE_NUMBER = 8
                IXP => IXS(1:NODE_NUMBER+1,NFT+1:NFT+NEL)
            ELSEIF(ITY==2) THEN
                FIRST = 2
                NODE_NUMBER = 4
                IXP => IXQ(1:NODE_NUMBER+1,NFT+1:NFT+NEL)
            ELSEIF(ITY==7) THEN
                FIRST = 2
                NODE_NUMBER = 3
                IXP => IXTG(1:NODE_NUMBER+1,NFT+1:NFT+NEL)
            ENDIF
            ! ---------------

            ! ---------------
            ! loop over the elements of the group
            DO J=1,NEL
                OLD_PHASE = 0
                MY_PHASE = 0
                BOOL = .FALSE.
                CONDITION = .TRUE.
                I = 1
                UNKNOWN_CELL_INDEX = 0
                POSITION_SAVE(1:NODE_NUMBER,1:3) = 0
                LAST_UNKNOWN_CELL = 0

                ! ---------------
                ! loop over the elements of the group
                DO WHILE (CONDITION)
                    NODE_ID = IXP(1+I,J) ! node id
                    IX = CELL_POSITION(1,NODE_ID) ! position in the grid
                    IY = CELL_POSITION(2,NODE_ID) ! position in the grid
                    IZ = CELL_POSITION(3,NODE_ID) ! position in the grid
                    ! ---------------
                    ! the cell (ix,iy,iz) is crossed by a surface --> need to compute the real distance
                    IF(CELL(IX,IY,IZ)==2) THEN
                        BOOL = .TRUE.
                        CONDITION = .FALSE.
                    ! ---------------
                    ! the pseudo distance of the cell (ix,iy,iz) is known --> apply it to the cell
                    ELSEIF(CELL(IX,IY,IZ)==1.OR.CELL(IX,IY,IZ)==-1) THEN
                        OLD_PHASE = MY_PHASE
                        MY_PHASE = CELL(IX,IY,IZ)
                    ! ---------------
                    ! the pseudo distance of the cell (ix,iy,iz) is unknown --> need to find the pseudo distance
                    ELSEIF(CELL(IX,IY,IZ)==0) THEN
                        CURRENT_UNKNOWN_CELL = IX + 1000 * IY + 1000**2 * IZ 
                        IF(LAST_UNKNOWN_CELL/=CURRENT_UNKNOWN_CELL) THEN
                            UNKNOWN_CELL_INDEX = UNKNOWN_CELL_INDEX + 1
                            POSITION_SAVE(UNKNOWN_CELL_INDEX,1) = IX
                            POSITION_SAVE(UNKNOWN_CELL_INDEX,2) = IY
                            POSITION_SAVE(UNKNOWN_CELL_INDEX,3) = IZ
                            LAST_UNKNOWN_CELL = CURRENT_UNKNOWN_CELL
                            ID_NODE_SAVE(UNKNOWN_CELL_INDEX) = NODE_ID
                        ENDIF
                    ENDIF
                    ! ---------------
                    I = I + 1
                    IF( I>NODE_NUMBER ) CONDITION = .FALSE.
                ENDDO
                ! -------------



                IF(BOOL) THEN
                ! -------------
                ! current element is near a surface, need to compute the distance to the surface
                    DO I=1,NODE_NUMBER
                        NODE_ID = IXP(1+I,J)
                        IF(TAG_ARRAY(NODE_ID)==0) THEN
                            TAG_ARRAY(NODE_ID) = 1 
                            NEXT_NODE = NEXT_NODE + 1
                            LIST_NODE(NEXT_NODE) = NODE_ID
                        ENDIF
                    ENDDO
                ELSE
                ! -------------
                ! current element is far from a surface, 2 cases :
                !   * nodes of element are in a non tagged cell --> need to find the phase of the cells
                !   * at least 1 node is in a tagged cell --> apply the phase to the element & the adjacent cells

                    ! -------------
                    ! i found a phase, apply it to the ndoes
                    IF(MY_PHASE/=0) THEN
                        DO I=1,NODE_NUMBER
                            NODE_ID = IXP(1+I,J)
                            NODAL_PHASE(NODE_ID) = MY_PHASE
                        ENDDO
                        DO I=1,UNKNOWN_CELL_INDEX
                            IX = POSITION_SAVE(I,1)
                            IY = POSITION_SAVE(I,2)
                            IZ = POSITION_SAVE(I,3)
                            CELL(IX,IY,IZ) = MY_PHASE
                        ENDDO
                    ! -------------
                    ! i need to find the phase of the current cells and extend it to the empty cells
                    ELSE
                        ! --------------------
                        ! find the nearest node
                        ALLOCATE( ID_LIST(UNKNOWN_CELL_INDEX) )
                        ALLOCATE( KEY1(UNKNOWN_CELL_INDEX) )
                        DO I=1,UNKNOWN_CELL_INDEX
                            IX = POSITION_SAVE(I,1)
                            IY = POSITION_SAVE(I,2)
                            IZ = POSITION_SAVE(I,3)
                            NODE_ID = ID_NODE_SAVE(I)
                            LOCAL_X(1,I) = X(1,NODE_ID)
                            LOCAL_X(2,I) = X(2,NODE_ID)
                            LOCAL_X(3,I) = X(3,NODE_ID)
                            KEY1(I) = I
                        ENDDO
                        EPS = 1D-6
                        CALL FIND_CLOSEST_NODE(LEADING_DIMENSION,UNKNOWN_CELL_INDEX,SURF_NODE_NUMBER,NUMNOD,
     .                                         LOCAL_X,X,EPS, 
     .                                         KEY1,KEY2,SURF_NODE_LIST,ID_LIST)
                        ! --------------------

                        ! --------------------
                        ! compute the pseudo distance
                        DO I=1,UNKNOWN_CELL_INDEX
                            INOD = ID_LIST(I)
                            XN(1:3) = LOCAL_X(1:3,I)
                            DIST = ZERO
                            CALL IN_OUT_SIDE( INOD,INOD2SURF,KNOD2SURF,NNOD2SURF,X,
     .                                        XN,DIST,NSEG,SURF_ELTYP,NOD_NORMAL,
     .                                        SURFACE_NODES,SWIFTSURF,ID_SURFACE,SEGTOSURF )
                            IX = POSITION_SAVE(I,1)
                            IY = POSITION_SAVE(I,2)
                            IZ = POSITION_SAVE(I,3)
                            MY_PHASE = INT(DIST)
                            CELL(IX,IY,IZ) = MY_PHASE
                        ENDDO
                        
                        ! --------------------
                        ! save the pseudo distance
                        DO I=1,NODE_NUMBER
                            NODE_ID = IXP(1+I,J)
                            IX = CELL_POSITION(1,NODE_ID)
                            IY = CELL_POSITION(2,NODE_ID)
                            IZ = CELL_POSITION(3,NODE_ID)
                            MY_PHASE = CELL(IX,IY,IZ) 
                            NODAL_PHASE(NODE_ID) = MY_PHASE
                        ENDDO
                        ! --------------------
                        ! extend the phase to the others cells
                        DO I=1,UNKNOWN_CELL_INDEX
                            IX = POSITION_SAVE(I,1)
                            IY = POSITION_SAVE(I,2)
                            IZ = POSITION_SAVE(I,3)   
                            CALL PHASE_PROPAGATION(IX,IY,IZ,NB_CELL_X,NB_CELL_Y,NB_CELL_Z,NB_BOX_LIMIT,CELL)
                        ENDDO
                        ! --------------------
                        DEALLOCATE( ID_LIST )
                        DEALLOCATE( KEY1 )
                    ENDIF
                ! -------------
                ENDIF
            ENDDO
            ! ---------------
        ENDDO
        ! -----------------------

        DEALLOCATE( LOCAL_X )



        ! -----------------------
        ! find the nearest node
        ALLOCATE( LOCAL_X(3,NEXT_NODE) )
        ALLOCATE( ID_LIST(NEXT_NODE) )
        ALLOCATE( KEY1(NEXT_NODE) )

        DO I=1,NEXT_NODE
            NODE_ID = LIST_NODE(I)
            LOCAL_X(1:3,I) = X(1:3,NODE_ID)
            KEY1(I) = I
        ENDDO

        EPS = 1D-6
                       
        ! --------------------
        CALL FIND_CLOSEST_NODE(LEADING_DIMENSION,NEXT_NODE,SURF_NODE_NUMBER,NUMNOD,
     .                         LOCAL_X,X,EPS, 
     .                         KEY1,KEY2,SURF_NODE_LIST,ID_LIST)
        ! --------------------

        ! --------------------
        ! compute the pseudo distance & save the closest node id        
        DO I=1,NEXT_NODE
            CLOSEST_NODE = ID_LIST(I)
            NODE_ID = LIST_NODE(I)
            XN(1:3) = LOCAL_X(1:3,I)
            DIST = ZERO
            CALL IN_OUT_SIDE( CLOSEST_NODE,INOD2SURF,KNOD2SURF,NNOD2SURF,X,
     .                        XN,DIST,NSEG,SURF_ELTYP,NOD_NORMAL,
     .                        SURFACE_NODES,SWIFTSURF,ID_SURFACE,SEGTOSURF )
            MY_PHASE = INT(DIST)
            NODAL_PHASE(NODE_ID) = MY_PHASE
            CLOSEST_NODE_ID(NODE_ID) = CLOSEST_NODE
        ENDDO
        ! --------------------

        ! -----------------------

        DEALLOCATE( KEY2 )
        DEALLOCATE( TAG_ARRAY )   
        DEALLOCATE( SURF_NODE_LIST )
        DEALLOCATE( LIST_NODE )
        DEALLOCATE( LOCAL_X )

        RETURN
        END SUBROUTINE PHASE_DETECTION
