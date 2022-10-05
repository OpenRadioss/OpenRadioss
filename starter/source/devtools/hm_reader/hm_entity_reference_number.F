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
Chd|  HM_ENTITY_REFERENCE_NUMBER          source/devtools/hm_reader/hm_entity_reference_number.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_ENTITY_REFERENCE_NUMBER(NAME,ID,REF_NUMBER)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   REQUEST INFO INTO MODEL NEUTRAL OBJECT DATABASE USING HM_READER
C   HOW MANY TIMES OPTION IS REFERENCED IN THE MODEL
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME         DESCRIPTION              
C
C     NAME         OPTION          
C     ID           ID OF OPTION
C     REF_NUMBER   NUMBER OF ENTITIES REFERENCING 'NAME' with ID
C     LSUBMODEL    SUBMODEL STRUCTURE           
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C OUTPUT ARGUMENTS
      CHARACTER*(ncharkey),INTENT(IN)::NAME
      INTEGER,INTENT(IN)::ID
      INTEGER,INTENT(OUT)::REF_NUMBER
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C=======================================================================
      CALL CPP_ENTITY_REFERENCE_NUMBER(NAME(1:LEN_TRIM(NAME)),LEN_TRIM(NAME),ID,REF_NUMBER) 
      RETURN
C
      END
