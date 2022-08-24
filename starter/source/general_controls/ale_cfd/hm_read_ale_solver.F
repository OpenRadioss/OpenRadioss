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
Chd|  HM_READ_ALE_SOLVER            source/general_controls/ale_cfd/hm_read_ale_solver.F
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_NEXT                source/devtools/hm_reader/hm_option_next.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_ALE_SOLVER(LSUBMODEL, UNITAB, ICAA, ISFINT)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is reading /ALE/SOLVER/FINT option in user input file
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "submod_c.inc"
#include "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE(UNIT_TYPE_), INTENT(IN) :: UNITAB
      INTEGER, INTENT(IN) :: ICAA
      INTEGER, INTENT(INOUT) :: ISFINT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NALESOLVER
      LOGICAL :: IS_AVAILABLE

      CALL HM_OPTION_COUNT('/ALE/SOLVER/FINT', NALESOLVER)
      
      IF (NALESOLVER > 0) THEN
         CALL HM_OPTION_START('/ALE/SOLVER/FINT')
         CALL HM_OPTION_NEXT()
         CALL HM_GET_INTV('Iform', ISFINT, IS_AVAILABLE, LSUBMODEL)
         IF (ISFINT < 0 .OR. ISFINT > 3)THEN
             CALL ANCMSG(MSGID   = 1138    ,
     .                   MSGTYPE = MSGERROR,
     .                   ANMODE  = ANINFO  ,
     .                   C1      = 'ISFINT',
     .                   I1      = ISFINT )           
           ENDIF  
           IF (ISFINT == 2 .AND. ICAA == 1)THEN
             CALL ANCMSG(MSGID   = 1138    ,
     .                   MSGTYPE = MSGERROR,
     .                   ANMODE  = ANINFO  ,
     .                   C1      = 'ISFINT=2 NOT COMPATIBLE  WITH CAA',
     .                   I1      = ICAA )             
           ENDIF
           IF (ISFINT == 0) ISFINT = 3
      ENDIF

      END 
