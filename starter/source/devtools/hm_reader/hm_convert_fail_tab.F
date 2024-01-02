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
Chd|  HM_CONVERT_FAIL_TAB           source/devtools/hm_reader/hm_convert_fail_tab.F
Chd|-- called by -----------
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_CONVERT_FAIL_TAB(TABLE_MAXID,FAIL_MAXID,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   ROUTINE TO CONVERT /FAIL/TAB -> /FAIL/TAB1 + /TABLE
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C    
C     TABLE_MAXID   MAX ID TABLE
C     FAIL_MAXID    MAX ID FAIL
C     LSUBMODEL     SUBMODEL STRUCTURE    
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD       
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      INTEGER,INTENT(IN)::TABLE_MAXID,FAIL_MAXID
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NBFAILTAB,OFFSET,ISFIRST,IS_CONVERT,CPT
      CHARACTER KEY*ncharline
C-----------------------------------------------
      IS_CONVERT = 0
      CALL HM_OPTION_COUNT('/FAIL/TAB', NBFAILTAB)
      CALL HM_OPTION_START('/FAIL/TAB')
      DO I=1,NBFAILTAB
        ISFIRST = 1
        KEY  = ''
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          KEYWORD2    = KEY )
        OFFSET = 0
        IF ((KEY /= 'TAB1').AND.(KEY /= 'TAB2')) THEN
          CALL CPP_CONVERT_FAIL_TAB(TABLE_MAXID,FAIL_MAXID,OFFSET,ISFIRST)
          IS_CONVERT = 1
        ENDIF
      ENDDO
C--------------------------------------------------
C DELETE /FAIL/TAB ENTITIES
C--------------------------------------------------
      CPT = 0
      IF (IS_CONVERT == 1) THEN
        DO I=1,NBFAILTAB
          CALL HM_OPTION_START('/FAIL/TAB')
          KEY  = '' 
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          KEYWORD2    = KEY )
          CPT = 1
          DO WHILE (((KEY == 'TAB1').OR.(KEY == 'TAB2')) .AND. CPT < NBFAILTAB)
            KEY  = '' 
            CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          KEYWORD2    = KEY )
            CPT = CPT + 1
          ENDDO
          IF ((KEY /= 'TAB1').AND.(KEY /= 'TAB2')) CALL CPP_DELETE_ENTITY()
        ENDDO
      ENDIF
C--------------------------------------------------
      RETURN
C
      END

