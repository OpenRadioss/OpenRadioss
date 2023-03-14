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
Chd|  FIND_CLOSEST_NODE             source/initial_conditions/inivol/find_closest_node.F
Chd|-- called by -----------
Chd|        PHASE_DETECTION               source/initial_conditions/inivol/phase_detection.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE FIND_CLOSEST_NODE(LEADING_DIRECTION,N1,N2,N3,
     .                                             X1,X2,EPS, 
     .                                             KEY1,KEY2,ID_X2,ID_LIST)
!---
C  Nearest Neighbor search
C  This routines returns ID_LIST and DIST such as
C  ID_LIST(I=1:N2) is the id of the closest node in X1(ID_X2(1:N2))
C
C This NNS algorithm is not optimal
C - worst case O(N1 x N2) 
C - best case O(N1 x LOG(N1)) + O(N1)
C - Expected in practice: O(N1 log(N1)) + O (N1 x (N2)^(1/3)) 
C      which is good enough for cases where N2 << N1
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
! com04 : numels/numelq/numeltg
#include "com04_c.inc"
       
! --------------------------------------
        INTEGER, INTENT(IN) :: LEADING_DIRECTION
        INTEGER, INTENT(IN)    ::  N1,N2,N3  ! N1: number if node
                                             ! N2: number of root nodes
                                             ! N3 : total number of node
        my_real, DIMENSION(3,N1), INTENT(IN)    ::  X1 ! Coordinates 
        my_real, DIMENSION(3,N3), INTENT(IN)    ::  X2 ! Coordinates 
        my_real, INTENT(IN)    ::  EPS  ! Minimum distance between two distinct points
        INTEGER, DIMENSION(N1), INTENT(IN) :: KEY1
        INTEGER, DIMENSION(N2), INTENT(IN) ::  KEY2
        INTEGER, DIMENSION(N2), INTENT(IN) ::  ID_X2  ! Indexes of 'root' coordinates
        INTEGER, DIMENSION(N1),INTENT(INOUT) ::  ID_LIST! id of the closest root node
! --------------------------------------
C      LOCAL VARIABLES
        INTEGER :: I,J,ID,info,JMIN
        INTEGER :: LAST_POSITION
        my_real :: LEADING_SIZE
        my_real :: DELTA,DX,DY,DZ,DXL,DYL,DZL,DL
        integer :: ijk
! --------------------------------------
        LAST_POSITION = 1 
        JMIN = 1
        DO I = 1,N1
            DX =ABS(X2(1,ID_X2(KEY2(LAST_POSITION))) - X1(1,KEY1(I)))   
            DY =ABS(X2(2,ID_X2(KEY2(LAST_POSITION))) - X1(2,KEY1(I)))   
            DZ =ABS(X2(3,ID_X2(KEY2(LAST_POSITION))) - X1(3,KEY1(I)))   
            LEADING_SIZE = ABS(X2(LEADING_DIRECTION,ID_X2(KEY2(LAST_POSITION))) - X1(LEADING_DIRECTION,KEY1(I)))   
            DELTA = SQRT(DX*DX+DY*DY+DZ*DZ)

C        Avoid nodes closer than EPS
            IF (DELTA < EPS) DELTA = HUGE(DELTA)

C        Upward search
            J = LAST_POSITION + 1
            JMIN = LAST_POSITION
            ID = KEY2(LAST_POSITION)
            DXL = LEADING_SIZE
            DO WHILE (J <= N2 .AND. (DXL <= DELTA .OR. DELTA < EPS))
                DXL =ABS(X2(1,ID_X2(KEY2(J))) - X1(1,KEY1(I)))   
                DYL =ABS(X2(2,ID_X2(KEY2(J))) - X1(2,KEY1(I)))   
                DZL =ABS(X2(3,ID_X2(KEY2(J))) - X1(3,KEY1(I)))   
                DL = SQRT(DXL*DXL+DYL*DYL+DZL*DZL)
                IF (DL  < DELTA .AND. DL  > EPS) THEN
                    DELTA = DL
                    ID = KEY2(J)
                    JMIN = J
                ENDIF
                J = J + 1
                IF (J <= N2) DXL =ABS(X2(LEADING_DIRECTION,ID_X2(KEY2(J)))-X1(LEADING_DIRECTION,KEY1(I)))  
            ENDDO

C        Backward search
            J = LAST_POSITION
            DXL = LEADING_SIZE
            DO WHILE (J > 0 .AND. (DXL <= DELTA .OR. DELTA < EPS))
                DXL =ABS(X2(1,ID_X2(KEY2(J))) - X1(1,KEY1(I)))   
                DYL =ABS(X2(2,ID_X2(KEY2(J))) - X1(2,KEY1(I)))   
                DZL =ABS(X2(3,ID_X2(KEY2(J))) - X1(3,KEY1(I)))   
                DL = SQRT(DXL*DXL+DYL*DYL+DZL*DZL)
                IF (DL  < DELTA .AND. DL > EPS) THEN
                    DELTA = DL
                    ID = KEY2(J)
                    JMIN = J
                ENDIF
                J = J - 1
                IF (J > 0)  DXL =ABS(X2(LEADING_DIRECTION,ID_X2(KEY2(J)))-X1(LEADING_DIRECTION,KEY1(I)))  
            ENDDO
            LAST_POSITION = JMIN 
            ID_LIST(KEY1(I)) = ID
        ENDDO

        DO I = 1,N1
            ID_LIST(I) = ID_X2(ID_LIST(I))
        ENDDO

        RETURN
!---
        END SUBROUTINE FIND_CLOSEST_NODE
