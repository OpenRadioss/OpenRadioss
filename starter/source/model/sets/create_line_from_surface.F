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
Chd|  CREATE_LINE_FROM_SURFACE      source/model/sets/create_line_from_surface.F
Chd|-- called by -----------
Chd|        CREATE_BOX_CLAUSE             source/model/sets/create_box_clause.F
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|        INSERT_CLAUSE_IN_SET          source/model/sets/insert_clause_in_set.F
Chd|-- calls ---------------
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SET_SCRATCH_MOD               ../common_source/modules/setdef_mod.F
Chd|====================================================================
      SUBROUTINE CREATE_LINE_FROM_SURFACE(CLAUSE     ,KEYSET,OPT_A,OPT_E,DELBUF,
     .                                    GO_IN_ARRAY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SETDEF_MOD
      USE SET_SCRATCH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "radioss_maptable.inc"
#include      "implicit_f.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER OPT_A,OPT_E
      CHARACTER KEYSET*ncharfield
      TYPE (SET_) :: CLAUSE
      TYPE (SET_SCRATCH) ::  DELBUF
      LOGICAL GO_IN_ARRAY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IEXT
C=======================================================================


!------------------
      IEXT = 1  ! par default (external surface)
      IF ( OPT_A == 1 .OR. KEYSET == 'ALL' .OR.
     .                     KEYSET == 'BOX' .OR.
     .                     KEYSET == 'BOX2')  IEXT = 2 ! all surfaces (internal + external)
!------------------

      CALL CREATE_LINE_FROM_SURFACE_EXT_ALL(CLAUSE ,IEXT ,OPT_E,DELBUF,GO_IN_ARRAY)

C-----------
      RETURN
      END
