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
Chd|  HM_GET_STRING_INDEX           source/devtools/hm_reader/hm_get_string_index.F
Chd|-- called by -----------
Chd|        HM_READ_DRAPE                 source/properties/composite_options/drape/hm_read_drape.F
Chd|        HM_READ_FAIL_USER             source/materials/fail/failuser/hm_read_fail_user.F
Chd|        HM_READ_MAT29_31              source/materials/mat/matuser/hm_read_mat_user29_31.F
Chd|        HM_READ_MAT_99                source/materials/mat/matuser/hm_read_mat_user_99.F
Chd|        HM_READ_MAT_MDS               source/materials/mat/matuser/hm_read_mat_mds.F
Chd|        HM_READ_MONVOL_TYPE11         source/airbag/hm_read_monvol_type11.F
Chd|        HM_READ_MONVOL_TYPE7          source/airbag/hm_read_monvol_type7.F
Chd|        HM_READ_MONVOL_TYPE8          source/airbag/hm_read_monvol_type8.F
Chd|        HM_READ_MONVOL_TYPE9          source/airbag/hm_read_monvol_type9.F
Chd|        HM_READ_PROP28                source/properties/xelem/hm_read_prop28.F
Chd|        HM_READ_PROP_USER             source/properties/user_spring_solid/hm_read_prop_user.F
Chd|        HM_READ_THGRNE                source/output/th/hm_read_thgrne.F
Chd|        HM_READ_THGRNS                source/output/th/hm_read_thgrns.F
Chd|        HM_READ_WINDOW_USER           source/tools/userwi/hm_read_window_user.F
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|        HM_THGRKI_VENT                source/output/th/hm_thgrki_vent.F
Chd|        HM_THVARVENT                  source/output/th/hm_thvarent.F
Chd|        READ_SENSOR_USER              source/tools/sensor/read_sensor_user.F
Chd|        SORT_SET                      source/model/sets/sort_sets.F 
Chd|        ST_QAPRINT_SET                source/output/qaprint/st_qaprint_set.F
Chd|        HM_THVARC                     source/output/th/hm_read_thvarc.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE HM_GET_STRING_INDEX(NAME,SVAL,INDEX,SIZE,IS_AVAILABLE)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   REQUEST DATA INTO MODEL NEUTRAL OBJECT DATABASE USING HM_READER
C   PICK VALUE
C   ASK INDEX_TH (STRING) VALUE OF 'NAME' FIELD DEFINED IN .cfg FILE 
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME         DESCRIPTION              
C
C     NAME         FIELD NAME         
C     SVAL         STRING VALUE OF THE FIELD 
C     INDEX        INDEX NUMBER OF THE VALUE 
C     IS_AVAILABLE VALUE AVAILABLE IN MODEL OR NOT
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD    
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_BOOL

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
      INTEGER,INTENT(IN)::INDEX
      CHARACTER*(*),INTENT(IN)::NAME
C OUTPUT ARGUMENTS
      CHARACTER*(*),INTENT(OUT)::SVAL
      INTEGER,INTENT(IN)::SIZE
      LOGICAL,INTENT(OUT)::IS_AVAILABLE
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,VALUE_TYPE,SUB_ID
      LOGICAL(KIND=C_BOOL) :: C_IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C=======================================================================
      SVAL = ''
C--------------------------------------------------
      C_IS_AVAILABLE = .FALSE.
      CALL CPP_GET_STRING_INDEX(NAME(1:LEN_TRIM(NAME)),LEN_TRIM(NAME),SVAL,INDEX,SIZE,C_IS_AVAILABLE)
      IS_AVAILABLE = C_IS_AVAILABLE
C--------------------------------------------------
      RETURN
C
      END
