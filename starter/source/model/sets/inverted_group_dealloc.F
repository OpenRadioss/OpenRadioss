Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  INVERTED_GROUP_DEALLOC        source/model/sets/inverted_group_dealloc.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        INVERTED_GROUP_MOD            share/modules1/inverted_group_mod.F
Chd|====================================================================
        SUBROUTINE INVERTED_GROUP_DEALLOC(INV_GROUP)
!$COMMENT
!       INVERTED_GROUP_DEALLOC description
!       deallocation of the different arrays of INV_GROUP structure
!       
!       INVERTED_GROUP_DEALLOC organization :
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

!   ----------------------------------------
        IF(INV_GROUP%ALREADY_ALLOC) THEN

            DEALLOCATE( INV_GROUP%INDPARTS )
            DEALLOCATE( INV_GROUP%INDPARTQ )
            DEALLOCATE( INV_GROUP%INDPARTC )
            DEALLOCATE( INV_GROUP%INDPARTT )
            DEALLOCATE( INV_GROUP%INDPARTP )
            DEALLOCATE( INV_GROUP%INDPARTTG )
            DEALLOCATE( INV_GROUP%INDPARTTRIA )
            DEALLOCATE( INV_GROUP%INDPARTR )

            DEALLOCATE( INV_GROUP%PARTS )
            DEALLOCATE( INV_GROUP%PARTQ )
            DEALLOCATE( INV_GROUP%PARTT )
            DEALLOCATE( INV_GROUP%PARTP )
            DEALLOCATE( INV_GROUP%PARTTG )
            DEALLOCATE( INV_GROUP%PARTTRIA )
            DEALLOCATE( INV_GROUP%PARTR )
        ENDIF   
!   ---------------------------------------- 

        RETURN
        END SUBROUTINE INVERTED_GROUP_DEALLOC
