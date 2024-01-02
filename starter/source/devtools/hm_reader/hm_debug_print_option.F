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
Chd|  HM_DEBUG_PRINT_OPTION         source/devtools/hm_reader/hm_debug_print_option.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
       SUBROUTINE HM_DEBUG_PRINT_OPTION(OPTION)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE UNITAB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      CHARACTER*(*),INTENT(IN)::OPTION
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       CHARACTER FILNAM*512
       INTEGER I,LEN_FILNAM,CPT
C=======================================================================
C WRITE /ANIM OPTIONS THAT ARE IN MEMORY(HM MODEL) IN 1.RAD FILE 
C=======================================================================
C-----------------------------------------------
C OPEN debug_hm.txt
C-----------------------------------------------
       FILNAM  ='debug_hm.txt'
       LEN_FILNAM=12
       CALL CPP_DEBUG_MODEL_OPEN_FILE(FILNAM,LEN_FILNAM)
C--------------------------------------------------
C START BROWSING OPTIONS
C--------------------------------------------------
      CALL HM_OPTION_COUNT(OPTION(1:LEN_TRIM(OPTION)),CPT)
      CALL HM_OPTION_START(OPTION(1:LEN_TRIM(OPTION)))
C--------------------------------------------------
C WRITE INd ebug_hm.txt
C--------------------------------------------------
      DO I=1,CPT
        CALL CPP_DEBUG_GLOBAL_ENTITY_SDI_WRITE()
      ENDDO
C-----------------------------------------------
C CLOSE debug_hm.txt
C-----------------------------------------------
      CALL CPP_DEBUG_MODEL_CLOSE_FILE()
C-----------------------------------------------
       RETURN
       END
