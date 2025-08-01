Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    hm_set_intv    ../starter/source/devtools/hm_reader/hm_set_intv.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    message_mod    ../starter/share/message_module/message_mod.F
!||    submodel_mod   ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_SET_INTV(NAME,IVAL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   SET INTEGER DATA IDENTIFIED BY 'NAME' INTO MODEL NEUTRAL OBJECT DATABASE USING HM_READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME         DESCRIPTION              
C
C     NAME         FIELD NAME         
C     IVAL         INTEGER VALUE OF THE FIELD 
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_BOOL
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
      CHARACTER*50,INTENT(IN)::NAME
      INTEGER,INTENT(IN)::IVAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL(KIND=C_BOOL) C_IS_AVAILABLE      
C-----------------------------------------------
      CALL CPP_SET_INTV(NAME(1:LEN_TRIM(NAME)),LEN_TRIM(NAME),IVAL,C_IS_AVAILABLE)
C-----------------------------------------------
      RETURN
C
      END
