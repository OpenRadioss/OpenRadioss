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
Chd|  INVERTED_GROUP_INIT           source/model/sets/inverted_group_init.F
Chd|-- called by -----------
Chd|        COMPUTE_CONNECT_PARTELM       source/model/sets/compute_connect_partelm.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        INVERTED_GROUP_MOD            share/modules1/inverted_group_mod.F
Chd|====================================================================
        SUBROUTINE INVERTED_GROUP_INIT(MODE,INV_GROUP)
!$COMMENT
!       INVERTED_GROUP_INIT description
!       initialization of INV_GROUP structure (number of elm & part)
!       
!       INVERTED_GROUP_INIT organization :
!           2 different modes :
!           - MODE = 0 --> first initialization
!           - MODE = 1 --> recomputation of the inverted connectivity
!             the size does not change, the structure was already allocated,
!             flush to 0 the array 
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE INVERTED_GROUP_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: MODE
        TYPE(INVERTGROUP_STRUCT_) :: INV_GROUP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: MY_SIZE
!   ----------------------------------------
        !   ---------------
        !   first initialization
        IF(MODE==0) THEN
            INV_GROUP%IS_USED = .FALSE.
            INV_GROUP%ALREADY_ALLOC = .FALSE.
            INV_GROUP%ALREADY_DONE = .FALSE.

            INV_GROUP%SIZE_INDPART = NPART + 1
            INV_GROUP%SIZE_S = NUMELS
            INV_GROUP%SIZE_Q = NUMELQ
            INV_GROUP%SIZE_C = NUMELC
            INV_GROUP%SIZE_T = NUMELT
            INV_GROUP%SIZE_P = NUMELP
            INV_GROUP%SIZE_TG = NUMELTG
            INV_GROUP%SIZE_TRIA= NUMELTRIA
            INV_GROUP%SIZE_R = NUMELR        
        ELSE
        !   ---------------
        !   recompute the inverted connectivity : flush to 0
            INV_GROUP%IS_USED = .TRUE.
            MY_SIZE = INV_GROUP%SIZE_INDPART

            INV_GROUP%INDPARTS( 1:MY_SIZE ) = 0
            INV_GROUP%INDPARTQ( 1:MY_SIZE ) = 0
            INV_GROUP%INDPARTC( 1:MY_SIZE ) = 0
            INV_GROUP%INDPARTT( 1:MY_SIZE ) = 0
            INV_GROUP%INDPARTP( 1:MY_SIZE ) = 0
            INV_GROUP%INDPARTTG( 1:MY_SIZE ) = 0
            INV_GROUP%INDPARTTRIA( 1:MY_SIZE ) = 0
            INV_GROUP%INDPARTR( 1:MY_SIZE ) = 0

            MY_SIZE = INV_GROUP%SIZE_S
            INV_GROUP%PARTS( 1:MY_SIZE ) = 0

            MY_SIZE = INV_GROUP%SIZE_Q
            INV_GROUP%PARTQ( 1:MY_SIZE ) = 0

            MY_SIZE = INV_GROUP%SIZE_T
            INV_GROUP%PARTT( 1:MY_SIZE ) = 0

            MY_SIZE = INV_GROUP%SIZE_P
            INV_GROUP%PARTP( 1:MY_SIZE ) = 0
        
            MY_SIZE = INV_GROUP%SIZE_TG
            INV_GROUP%PARTTG( 1:MY_SIZE ) = 0

            MY_SIZE = INV_GROUP%SIZE_TRIA
            INV_GROUP%PARTTRIA( 1:MY_SIZE ) = 0

            MY_SIZE = INV_GROUP%SIZE_R
            INV_GROUP%PARTR( 1:MY_SIZE ) = 0  
        !   ---------------  
        ENDIF
!   ----------------------------------------
        RETURN
        END SUBROUTINE INVERTED_GROUP_INIT
