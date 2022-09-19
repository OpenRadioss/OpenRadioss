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
Chd|  FILL_CLAUSE_RBODY_BOX         source/model/sets/fill_clause_rbody_box.F
Chd|-- called by -----------
Chd|        CREATE_RBODY_BOX              source/model/sets/create_rbody_box.F
Chd|-- calls ---------------
Chd|        SET_MERGE_SIMPLE              source/model/sets/set_merge_simple.F
Chd|        SIMPLE_RBODY_BOX              source/model/sets/simple_rbody_box.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|====================================================================
      SUBROUTINE FILL_CLAUSE_RBODY_BOX( IBOX   ,X  ,SKEW  ,SET_TITLE ,KEYSET,
     *                                 BOXLIST, BOXLIST_SIZE,
     *                                 BOXNDS, SZ_BOXNDS     ,RBY_MSN)

#include      "set_def.inc"
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE OPTIONDEF_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "nchar_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (BOX_)    , DIMENSION(NBBOX)  :: IBOX

      INTEGER  BOXNDS(*),BOXLIST(*)
      INTEGER  SZ_BOXNDS, BOXLIST_SIZE
      INTEGER, INTENT(IN), DIMENSION(2,NRBODY) :: RBY_MSN

      my_real
     .        X(3,*),SKEW(LSKEW,*)

      CHARACTER KEYSET*ncharfield
      CHARACTER SET_TITLE*nchartitle
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,B_NDS_SIZE,NB_RESULT,
     .        ID,IB,NB_BOX_OF_BOX,J,CLAUSE_OPERATOR
      LOGICAL BOOL
      INTEGER, DIMENSION(:) , ALLOCATABLE :: B_NDS,RESULT
C-----------------------------------------------
      ALLOCATE(B_NDS(NRBODY))
      ALLOCATE(RESULT(NRBODY))

!
      ! Tag nodes from boxes
      ! ---------------------
      CLAUSE_OPERATOR = SET_ADD
      SZ_BOXNDS = 0
      DO I=1,BOXLIST_SIZE
        IB = BOXLIST(I)
        B_NDS_SIZE = 0

        CALL RBODY_BOX( IB  ,
     *                  IBOX ,X  ,SKEW  ,SET_TITLE ,KEYSET,
     *                  B_NDS, B_NDS_SIZE,RBY_MSN)


        CALL SET_MERGE_SIMPLE( BOXNDS,  SZ_BOXNDS  ,
     *                          B_NDS,   B_NDS_SIZE ,
     *                          RESULT,  NB_RESULT  ,
     *                          CLAUSE_OPERATOR)

         BOXNDS(1:NB_RESULT) = RESULT(1:NB_RESULT)
         SZ_BOXNDS = NB_RESULT
      ENDDO ! DO I=1,BOXLIST_SIZE

C-------
      RETURN
      END


      RECURSIVE SUBROUTINE RBODY_BOX( IB  ,
     *                     IBOX ,X  ,SKEW  ,SET_TITLE ,KEYSET,
     *                     BOXNDS, SZ_BOXNDS,RBY_MSN)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Recursive routine - Go through tree and fill the Node Box array
C
C------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME           DESCRIPTION                         
C
C     IB             Recursive indice / Current Box to treat
C     IBOX           IBOX Structure
C     SKEW           Skew Structure
C     SET_TITLE      Title for Error message
C     KEYSET         KEYSET for  Error message
C     BOXNDS         merged node array
C     SZ_BOXNDS      number of stacked nodes in BOXNDS
C============================================================================
#include      "set_def.inc"
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE OPTIONDEF_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "nchar_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (BOX_)    , DIMENSION(NBBOX)  :: IBOX
      INTEGER, INTENT(IN), DIMENSION(2,NRBODY) :: RBY_MSN

      INTEGER  BOXNDS(*)
      INTEGER  IB,SZ_BOXNDS

      my_real
     .        X(3,*),SKEW(LSKEW,*)

      CHARACTER KEYSET*ncharfield
      CHARACTER SET_TITLE*nchartitle
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,B_NDS_SIZE,NB_RESULT,
     .        NB_BOX_OF_BOX,J,NEW_BOX,CLAUSE_OPERATOR
      LOGICAL BOOL
      INTEGER, DIMENSION(:) , ALLOCATABLE :: B_NDS,RESULT

C-----------------------------------------------


      NB_BOX_OF_BOX   = IBOX(IB)%NBOXBOX

      IF ( NB_BOX_OF_BOX == 0 ) THEN

          ! Fill SIMPLE Boxes
          ! ---------------------
          SZ_BOXNDS = 0
          CALL SIMPLE_RBODY_BOX(IBOX, X, SKEW, IB,
     *                         BOXNDS, SZ_BOXNDS,RBY_MSN)

      ELSE
          ALLOCATE(B_NDS(NRBODY))
          ALLOCATE(RESULT(NRBODY))


          DO I=1,NB_BOX_OF_BOX

            J    = IBOX(IB)%IBOXBOX(I)  !  could be negative
            NEW_BOX = ABS(J)

            B_NDS_SIZE=0

            CALL RBODY_BOX ( NEW_BOX ,
     *                       IBOX ,X  ,SKEW  ,SET_TITLE ,KEYSET,
     *                       B_NDS, B_NDS_SIZE ,RBY_MSN)

            IF (J < 0)THEN
                CLAUSE_OPERATOR = SET_DELETE
            ELSE
                CLAUSE_OPERATOR = SET_ADD
            ENDIF

            CALL SET_MERGE_SIMPLE( BOXNDS,  SZ_BOXNDS  ,
     *                             B_NDS,   B_NDS_SIZE ,
     *                             RESULT,  NB_RESULT  ,
     *                             CLAUSE_OPERATOR)

            BOXNDS(1:NB_RESULT) = RESULT(1:NB_RESULT)
            SZ_BOXNDS = NB_RESULT
          ENDDO

       ENDIF

C-------
      RETURN
      END







