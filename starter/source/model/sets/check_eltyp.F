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
Chd|  CHECK_ELTYP                   source/model/sets/check_eltyp.F
Chd|-- called by -----------
Chd|        CREATE_ELT_CLAUSE             source/model/sets/create_element_clause.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CHECK_ELTYP(ELTYP,CLAUSE,OK)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   For a SET of elements : 
C   Check if the input deck has elements from this type.
C   If not : Warning Message + Negative answer.
C------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     ELTYP         type of Element from SET as defined in "radioss_maptavle.inc"
C     CLAUSE        (SET structure) Clause to be treated
C     OK            INTEGER : Elt found / Not found
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
#include      "radioss_maptable.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: ELTYP
      TYPE (SET_),INTENT(INOUT) ::  CLAUSE
      INTEGER,INTENT(OUT) :: OK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NBELT
      CHARACTER (LEN=50) :: EL_TITLE
C-----------------------------------------------
      SELECT CASE (ELTYP)
         CASE(ELT_SOLID)
            NBELT = NUMELS
            EL_TITLE='SOLID ELEMENTS'
         CASE(ELT_QUAD)
            NBELT = NUMELQ
            EL_TITLE='QUAD ELEMENTS'
         CASE(ELT_SH4N)
            NBELT = NUMELC
            EL_TITLE='SHELL ELEMENTS'
         CASE(ELT_TRUSS)
            NBELT = NUMELT
            EL_TITLE='TRUSS ELEMENTS'
         CASE(ELT_BEAM)
            NBELT = NUMELP
            EL_TITLE='BEAM ELEMENTS'
         CASE(ELT_SPRING)
            NBELT = NUMELR
            EL_TITLE='SPRING ELEMENTS'
         CASE(ELT_SH3N)
            NBELT = NUMELTG
            EL_TITLE='SH3N ELEMENTS'
         CASE(ELT_TRIA)
            NBELT = NUMELTG
            EL_TITLE='TRIA ELEMENTS'
      END SELECT

      IF(NBELT == 0)THEN
        OK = 0
                 CALL ANCMSG(MSGID=2007,ANMODE=ANINFO,
     .               MSGTYPE=MSGWARNING,
     .               I1 = CLAUSE%SET_ID,
     .               C1=TRIM(CLAUSE%TITLE),
     .               C2=EL_TITLE)
      ELSE
        OK = 1
      ENDIF
      END
