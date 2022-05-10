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
Chd|  INVERTED_GROUP_ALLOC          source/model/sets/inverted_group_alloc.F
Chd|-- called by -----------
Chd|        COMPUTE_CONNECT_PARTELM       source/model/sets/compute_connect_partelm.F
Chd|-- calls ---------------
Chd|        INVERTED_GROUP_MOD            share/modules1/inverted_group_mod.F
Chd|====================================================================
        SUBROUTINE INVERTED_GROUP_ALLOC(INV_GROUP)
!$COMMENT
!       INVERTED_GROUP_ALLOC description
!       allocation of the different arrays of INV_GROUP structure
!       
!       INVERTED_GROUP_ALLOC organization :
!           none
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE(INVERTGROUP_STRUCT_) :: INV_GROUP
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: MY_SIZE
!   ----------------------------------------
        INV_GROUP%IS_USED = .TRUE.
        INV_GROUP%ALREADY_ALLOC = .TRUE.

        MY_SIZE = INV_GROUP%SIZE_INDPART
        ALLOCATE( INV_GROUP%INDPARTS( MY_SIZE ) )
        ALLOCATE( INV_GROUP%INDPARTQ( MY_SIZE ) )
        ALLOCATE( INV_GROUP%INDPARTC( MY_SIZE ) )
        ALLOCATE( INV_GROUP%INDPARTT( MY_SIZE ) )
        ALLOCATE( INV_GROUP%INDPARTP( MY_SIZE ) )
        ALLOCATE( INV_GROUP%INDPARTTG( MY_SIZE ) )
        ALLOCATE( INV_GROUP%INDPARTTRIA( MY_SIZE ) )
        ALLOCATE( INV_GROUP%INDPARTR( MY_SIZE ) )

        INV_GROUP%INDPARTS( 1:MY_SIZE ) = 0
        INV_GROUP%INDPARTQ( 1:MY_SIZE ) = 0
        INV_GROUP%INDPARTC( 1:MY_SIZE ) = 0
        INV_GROUP%INDPARTT( 1:MY_SIZE ) = 0
        INV_GROUP%INDPARTP( 1:MY_SIZE ) = 0
        INV_GROUP%INDPARTTG( 1:MY_SIZE ) = 0
        INV_GROUP%INDPARTTRIA( 1:MY_SIZE ) = 0
        INV_GROUP%INDPARTR( 1:MY_SIZE ) = 0

        MY_SIZE = INV_GROUP%SIZE_S
        ALLOCATE( INV_GROUP%PARTS( MY_SIZE ) )
        INV_GROUP%PARTS( 1:MY_SIZE ) = 0

        MY_SIZE = INV_GROUP%SIZE_Q
        ALLOCATE( INV_GROUP%PARTQ( MY_SIZE ) )
        INV_GROUP%PARTQ( 1:MY_SIZE ) = 0

        MY_SIZE = INV_GROUP%SIZE_C
        ALLOCATE( INV_GROUP%PARTC( MY_SIZE ) )
        INV_GROUP%PARTC( 1:MY_SIZE ) = 0

        MY_SIZE = INV_GROUP%SIZE_T
        ALLOCATE( INV_GROUP%PARTT( MY_SIZE ) )
        INV_GROUP%PARTT( 1:MY_SIZE ) = 0

        MY_SIZE = INV_GROUP%SIZE_P
        ALLOCATE( INV_GROUP%PARTP( MY_SIZE ) )
        INV_GROUP%PARTP( 1:MY_SIZE ) = 0

        MY_SIZE = INV_GROUP%SIZE_TG
        ALLOCATE( INV_GROUP%PARTTG( MY_SIZE ) )
        INV_GROUP%PARTTG( 1:MY_SIZE ) = 0

        MY_SIZE = INV_GROUP%SIZE_TRIA
        ALLOCATE( INV_GROUP%PARTTRIA( MY_SIZE ) )
        INV_GROUP%PARTTRIA( 1:MY_SIZE ) = 0

        MY_SIZE = INV_GROUP%SIZE_R
        ALLOCATE( INV_GROUP%PARTR( MY_SIZE ) )
        INV_GROUP%PARTR( 1:MY_SIZE ) = 0      
!   ----------------------------------------
        RETURN
        END SUBROUTINE INVERTED_GROUP_ALLOC
