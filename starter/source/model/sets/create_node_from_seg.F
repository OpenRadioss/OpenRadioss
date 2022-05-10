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
Chd|  CREATE_NODE_FROM_SEG          source/model/sets/create_node_from_seg.F
Chd|-- called by -----------
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CREATE_NODE_FROM_SEG( CLAUSE )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Create PART Clause from LIST
C------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     CLAUSE        (SET structure) Clause to be treated
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SETDEF_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (SET_) ::  CLAUSE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IND,NOD(4),NB_SEG_NODE,NB_SEG,LIMIT
      INTEGER, ALLOCATABLE, DIMENSION(:) :: TAGNOD,CLAUSE_NODE
      INTEGER IWORK(70000)
      INTEGER, DIMENSION(:),ALLOCATABLE:: IDX,SORT
C=======================================================================
!
      NB_SEG_NODE = 4 ! by default surf SEG
      NB_SEG = CLAUSE%NB_SURF_SEG  ! only one surf seg par CLAUSE

      IF (CLAUSE%NB_SURF_SEG == 0) THEN
        NB_SEG_NODE = 2 ! line SEG
        NB_SEG = CLAUSE%NB_LINE_SEG  ! only one line seg par CLAUSE
      ENDIF
!
      ALLOCATE(TAGNOD(NUMNOD))
      TAGNOD(:) = 0
      ALLOCATE(CLAUSE_NODE(NUMNOD))

      IND = 0

      IF (CLAUSE%NB_SURF_SEG > 0) THEN
       ! none from surf of SEG
        DO I=1,NB_SEG
          DO J=1,NB_SEG_NODE
            NOD(J) = CLAUSE%SURF_NODES(I,J)
            IF(TAGNOD(NOD(J)) == 0)THEN
              TAGNOD(NOD(J)) = 1
              IND = IND+1
              CLAUSE_NODE(IND) = NOD(J)
            ENDIF
          ENDDO
        ENDDO

      ELSE
       ! none from line of SEG
        DO I=1,NB_SEG
          DO J=1,NB_SEG_NODE
            NOD(J) = CLAUSE%LINE_NODES(I,J)
            IF(TAGNOD(NOD(J)) == 0)THEN
              TAGNOD(NOD(J)) = 1
              IND = IND+1
              CLAUSE_NODE(IND) = NOD(J)
            ENDIF
          ENDDO
        ENDDO

      ENDIF ! IF (CLAUSE%NB_SURF_SEG > 0)

!-------
      LIMIT = NUMNOD/2
      IF (IND < LIMIT)THEN                                  ! cheaper to use Order on small node groups
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
      ! clause node allocation
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
