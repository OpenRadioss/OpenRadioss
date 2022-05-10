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
Chd|  HM_GET_FLOAT_ARRAY_2INDEXES   source/devtools/hm_reader/hm_get_float_array_2indexes.F
Chd|-- called by -----------
Chd|        HM_READ_PROP17                source/properties/shell/hm_read_prop17.F
Chd|        HM_READ_PROP51                source/properties/shell/hm_read_prop51.F
Chd|        HM_READ_STACK                 source/stack/hm_read_stack.F  
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_GET_FLOAT_ARRAY_2INDEXES(NAME,RVAL,INDEX1,INDEX2,IS_AVAILABLE,LSUBMODEL,UNITAB)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   REQUEST DATA INTO MODEL NEUTRAL OBJECT DATABASE USING HM_READER
C   PICK VALUE IN A LIST OF VALUES
C   ASK (INDEX1_TH,INDEX2)_TH (REAL) VALUE OF 'NAME' FIELD DEFINED IN .cfg FILE 
C   APPLY AUTOMATICALLY UNIT SYSTEM USING DIMENSION DEFINED IN .cfg FILE
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME         DESCRIPTION              
C
C     NAME         FIELD NAME         
C     RVAL         REAL VALUE OF THE FIELD 
C     INDEX1       INDEX NUMBER OF THE VALUE (ROW)
C     INDEX2       INDEX NUMBER OF THE VALUE (COLUMN)
C     IS_AVAILABLE VALUE AVAILABLE IN MODEL OR NOT
C     LSUBMODEL    SUBMODEL STRUCTURE       
C     UNITAB       UNIT ARRAY       
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD       
      USE SUBMODEL_MOD
      USE UNITAB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "nchar_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C----------------------------------------------- 
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)::INDEX1,INDEX2
      CHARACTER*(*),INTENT(IN)::NAME
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
      my_real,
     .  INTENT(OUT)::RVAL
      LOGICAL,INTENT(OUT)::IS_AVAILABLE
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,VALUE_TYPE,SUB_ID,IFLAGUNIT,UID
      my_real
     .   FAC_L,FAC_M,FAC_T,FAC
      REAL*8 DVAL,LENGTH_DIM,MASS_DIM,TIME_DIM
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C=======================================================================
C--------------------------------------------------
        IS_AVAILABLE = .FALSE.
        LENGTH_DIM = ZERO
        MASS_DIM = ZERO
        TIME_DIM = ZERO
        FAC = UN
C--------------------------------------------------
        CALL CPP_GET_FLOATV_2INDEX(NAME(1:LEN_TRIM(NAME)),LEN_TRIM(NAME),DVAL,INDEX1,IS_AVAILABLE,
     .                    LENGTH_DIM,MASS_DIM,TIME_DIM,UID,SUB_ID,INDEX2)
C--------------------------------------------------
C      ID OFFSETS FOR //SUBMODEL 
C--------------------------------------------------
        IF(SUB_ID .NE. 0 .AND. UID .NE. 0)THEN
          UID = UID + LSUBMODEL(SUB_ID)%OFF_DEF
        ELSEIF(SUB_ID .NE. 0 )THEN
          IF(LSUBMODEL(SUB_ID)%UID .NE. 0)THEN
            UID = LSUBMODEL(SUB_ID)%UID
          ENDIF
        ENDIF
C--------------------------------------------------
c      APPLY UNIT SYSTEM
C--------------------------------------------------
        IFLAGUNIT = 0 
        FAC_M = ZERO
        FAC_L = ZERO
        FAC_T = ZERO  
        DO J=1,NUNITS                         
          IF (UNITAB%UNIT_ID(J) == UID) THEN    
            FAC_M = UNITAB%FAC_M(J)
            FAC_L = UNITAB%FAC_L(J)
            FAC_T = UNITAB%FAC_T(J)
            IFLAGUNIT = 1                      
            EXIT                              
          ENDIF        
        ENDDO
        IF (FAC_M  .NE. ZERO) FAC = FAC * (FAC_M ** MASS_DIM  )
        IF (FAC_L  .NE. ZERO) FAC = FAC * (FAC_L ** LENGTH_DIM)
        IF (FAC_T  .NE. ZERO) FAC = FAC * (FAC_T ** TIME_DIM  )     
C-------------------------------------------------- 
        RVAL  = DVAL *  FAC
C--------------------------------------------------                             
      RETURN
C
      END
