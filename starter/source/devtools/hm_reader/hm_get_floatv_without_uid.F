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
Chd|  HM_GET_FLOATV_WITHOUT_UID     source/devtools/hm_reader/hm_get_floatv_without_uid.F
Chd|-- called by -----------
Chd|        HM_READ_LAGMUL                source/tools/lagmul/hm_read_lagmul.F
Chd|        HM_READ_SPHGLO                source/general_controls/computation/hm_read_sphglo.F
Chd|        HM_READ_UNIT                  source/general_controls/computation/hm_read_unit.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE HM_GET_FLOATV_WITHOUT_UID(NAME,RVAL,IS_AVAILABLE)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   REQUEST DATA INTO MODEL NEUTRAL OBJECT DATABASE USING HM_READER
C   PICK VALUE
C   ASK (REAL) VALUE OF 'NAME' FIELD DEFINED IN .cfg FILE 
C   ONLY FOR OPTION THAT ARE NOT UNIT DEPENDANT
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME         DESCRIPTION              
C
C     NAME         FIELD NAME         
C     RVAL         REAL VALUE OF THE FIELD 
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
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C----------------------------------------------- 
C INPUT ARGUMENTS
      CHARACTER*(*),INTENT(IN)::NAME
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
      REAL*8 DVAL,LENGTH_DIM,MASS_DIM,TIME_DIM
      LOGICAL(KIND=C_BOOL) :: C_IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C=======================================================================
C--------------------------------------------------
        C_IS_AVAILABLE = .FALSE.
        LENGTH_DIM = ZERO
        MASS_DIM = ZERO
        TIME_DIM = ZERO
C--------------------------------------------------
        CALL CPP_GET_FLOATV_ONLY(NAME(1:LEN_TRIM(NAME)),LEN_TRIM(NAME),DVAL,C_IS_AVAILABLE,
     .                    LENGTH_DIM,MASS_DIM,TIME_DIM)
C--------------------------------------------------         
        RVAL  = DVAL
        IS_AVAILABLE = C_IS_AVAILABLE
C--------------------------------------------------                           
      RETURN
C
      END
