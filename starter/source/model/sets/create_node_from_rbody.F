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
Chd|  CREATE_NODE_FROM_RBODY        source/model/sets/create_node_from_rbody.F
Chd|-- called by -----------
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        TAG_NODE_FROM_RBODY           source/model/sets/tag_node_from_rbody.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|====================================================================
      SUBROUTINE CREATE_NODE_FROM_RBODY(RBY_MSN,CLAUSE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SETDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN), DIMENSION(2,NRBODY) :: RBY_MSN
!
      TYPE (SET_) ::  CLAUSE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IND,LIMIT
      INTEGER, ALLOCATABLE, DIMENSION(:) :: TAGNOD,CLAUSE_NODE
      INTEGER IWORK(70000)
      INTEGER, DIMENSION(:),ALLOCATABLE:: IDX,SORT
C=======================================================================
!
      ALLOCATE(TAGNOD(NUMNOD))
      TAGNOD(:) = 0
      ALLOCATE(CLAUSE_NODE(NUMNOD))

      IND=0
!
      ! RBODY
      IF ( CLAUSE%NB_RBODY > 0 )
     . CALL TAG_NODE_FROM_RBODY(
     .                          CLAUSE%NB_RBODY ,CLAUSE%RBODY ,CLAUSE_NODE,IND,
     .                          TAGNOD          ,RBY_MSN      )
C-----------
      LIMIT = NUMNOD/2
      IF (IND < LIMIT)THEN     ! cheaper to use Order on small node groups
        ALLOCATE(IDX(2*IND))
        ALLOCATE(SORT(IND))
        SORT(1:IND) = CLAUSE_NODE(1:IND)
        CALL MY_ORDERS(0,IWORK,SORT,IDX,IND,1)

        DO I=1,IND
          CLAUSE_NODE(I) = SORT(IDX(I))
        ENDDO
        DEALLOCATE(IDX)
        DEALLOCATE(SORT)
      ELSE
       IND = 0
       DO I=1,NUMNOD
         IF (TAGNOD(I) == 1) THEN
           IND = IND + 1
           CLAUSE_NODE(IND) = I
         ENDIF
       ENDDO
      ENDIF
!
! Decide whether the result is stored in an array or in the clause.
! In certain cases it is useful to store in ARRAY.
! Example : Clause with delete clause. Nodes must be recreated & merged...
! ----------------------------------------------------------------------------
      CLAUSE%NB_NODE = IND
      IF(ALLOCATED( CLAUSE%NODE )) DEALLOCATE( CLAUSE%NODE )
      ALLOCATE( CLAUSE%NODE(IND) )
      CLAUSE%NODE(1:IND) = CLAUSE_NODE(1:IND)
C-----------
      DEALLOCATE(TAGNOD)
      DEALLOCATE(CLAUSE_NODE)
C-----------
      RETURN
      END
