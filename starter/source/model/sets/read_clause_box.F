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
Chd|  READ_CLAUSE_BOX               source/model/sets/read_clause_box.F
Chd|-- called by -----------
Chd|        CREATE_ELT_BOX                source/model/sets/create_elt_box.F
Chd|        CREATE_NODE_BOX               source/model/sets/create_node_box.F
Chd|        CREATE_RBODY_BOX              source/model/sets/create_rbody_box.F
Chd|-- calls ---------------
Chd|        HM_GET_INT_ARRAY_2INDEXES     source/devtools/hm_reader/hm_get_int_array_2indexes.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE READ_CLAUSE_BOX(IBOX, IS_AVAILABLE, LSUBMODEL, JCLAUSE , IDS_MAX,
     .                           BOXLIST, BOXLIST_SIZE)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Read the Box clause
C   Creates a list of Box
C------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME           DESCRIPTION                         
C
C     IBOX           _BOX Structure - list of boxes
C     IS_AVAILABLE   Bool, needs to read input
C     LSUBMODEL      SUBMODEL Structure
C     JCLAUSE        Current clause number
C     IDS_MAX        MAX Number of clauses
C     BOXLIST        OUTPUT / List of Boxes
C     BOXLIST_SIZE   OUTPUT / SIZE of BOXLIST
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE OPTIONDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
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
      INTEGER JCLAUSE,BOXLIST_SIZE,IDS_MAX
      INTEGER BOXLIST(*)
      LOGICAL :: IS_AVAILABLE
!
      TYPE (BOX_)  , DIMENSION(NBBOX)  :: IBOX
C-----------------------------------------------
      TYPE(SUBMODEL_DATA),INTENT(IN):: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,IB,IDBOX,IDS
C=======================================================================
      ! Read Boxes
      ! ---------------------
      BOXLIST_SIZE = 0
      DO K=1,IDS_MAX
        CALL HM_GET_INT_ARRAY_2INDEXES('ids',IDS,JCLAUSE,K,IS_AVAILABLE,LSUBMODEL)
        IF (IDS /= 0) THEN
          DO IB=1,NBBOX
            IDBOX = IBOX(IB)%ID
            IF (IDS == IDBOX) THEN
              ! tag boxes
              BOXLIST_SIZE = BOXLIST_SIZE + 1
              BOXLIST(BOXLIST_SIZE) = IB
            ENDIF
          ENDDO
        ENDIF ! IF (IDS /= 0)
      ENDDO ! DO K=1,IDS_MAX
C-------------------------
      RETURN
      END
