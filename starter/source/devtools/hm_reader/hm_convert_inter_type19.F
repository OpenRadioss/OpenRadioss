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
Chd|  HM_CONVERT_INTER_TYPE19       source/devtools/hm_reader/hm_convert_inter_type19.F
Chd|-- called by -----------
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_NEXT                source/devtools/hm_reader/hm_option_next.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_CONVERT_INTER_TYPE19(IDMAX_INTER,IDMAX_GRNOD,IDMAX_LINE,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   ROUTINE TO CONVERT /INTER/TYPE19 -> /INTER/TYPE11 + /GRNOD  + LINE
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C    
C     IDMAX_INTER   MAX ID INTERFACES
C     IDMAX_GRNOD   MAX ID GRNODES
C     IDMAX_LINE    MAX ID LINES
C     LSUBMODEL     SUBMODEL STRUCTURE    
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD       
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      INTEGER,INTENT(IN)::IDMAX_INTER,IDMAX_GRNOD,IDMAX_LINE
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NBINTER19,OFFSET,ISFIRST
C-----------------------------------------------
      CALL HM_OPTION_COUNT('/INTER/TYPE19', NBINTER19)
      CALL HM_OPTION_START('/INTER/TYPE19')
      DO I=1,NBINTER19
        ISFIRST = 1
        CALL HM_OPTION_NEXT()
        OFFSET = 0
        CALL CPP_CONVERT_INTER_TYPE19(IDMAX_INTER,IDMAX_GRNOD,IDMAX_LINE,OFFSET,ISFIRST)
      ENDDO
C--------------------------------------------------
C DELETE TYPE19 INTERFACES
C--------------------------------------------------
      DO I=1,NBINTER19
        CALL HM_OPTION_START('/INTER/TYPE19') 
        CALL HM_OPTION_NEXT()
        CALL CPP_DELETE_ENTITY()
      ENDDO
C--------------------------------------------------
      RETURN
C
      END

