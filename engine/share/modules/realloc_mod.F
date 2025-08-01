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
!||    realloc_mod        ../engine/share/modules/realloc_mod.F
!||--- called by ------------------------------------------------------
!||    i11trivox          ../engine/source/interfaces/intsort/i11trivox.F
!||    i22get_prev_data   ../engine/source/interfaces/int22/i22get_prev_data.F
!||    i22intersect       ../engine/source/interfaces/int22/i22intersect.F
!||    i22trivox          ../engine/source/interfaces/intsort/i22trivox.F
!||    i25trivox_edg      ../engine/source/interfaces/intsort/i25trivox_edg.F
!||====================================================================
      MODULE REALLOC_MOD
      CONTAINS
        
!||====================================================================
!||    ireallocate     ../engine/share/modules/realloc_mod.F
!||--- called by ------------------------------------------------------
!||    i11trivox       ../engine/source/interfaces/intsort/i11trivox.F
!||    i22trivox       ../engine/source/interfaces/intsort/i22trivox.F
!||    i25trivox_edg   ../engine/source/interfaces/intsort/i25trivox_edg.F
!||====================================================================
      FUNCTION IREALLOCATE(ptr, new_size)
!reallocate INTEGER ARRAY
C-----------------------------------------------
C   I n c l u d e s
C-----------------------------------------------      
#include      "implicit_f.inc"      
C-----------------------------------------------
C   D u m m y  a r g u m e n t s 
C-----------------------------------------------
      INTEGER, DIMENSION(:),POINTER :: ptr,IREALLOCATE
      INTEGER, intent(in)   :: new_size
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------      
      INTEGER :: old_size, ierr
C-----------------------------------------------
C   P r e c o n d i t i o n s
C-----------------------------------------------          
      !Precondition : le pointeur doit etre affecte
      IF(.NOT. ASSOCIATED(ptr)) RETURN
      
      !il reste suffisamment de memoire
      ALLOCATE(IREALLOCATE(1:new_size), STAT=ierr)
      IF(ierr /= 0) STOP "memory allocation error"
C-----------------------------------------------                                            
       old_size = MIN(SIZE(ptr), new_size)
       IREALLOCATE(1:old_size) = ptr(1:old_size)                       
       DEALLOCATE(ptr) ! a ne surtout pas oublier    
            
      END FUNCTION IREALLOCATE
C-----------------------------------------------




!||====================================================================
!||    jreallocate   ../engine/share/modules/realloc_mod.F
!||====================================================================
      FUNCTION JREALLOCATE(ptr, new_size)
!reallocate MY_REAL ARRAY
C-----------------------------------------------
C   I n c l u d e s
C-----------------------------------------------      
#include      "implicit_f.inc"      
C-----------------------------------------------
C   D u m m y  a r g u m e n t s 
C-----------------------------------------------
      my_real
     .,DIMENSION(:),POINTER :: ptr,JREALLOCATE
       INTEGER, intent(in)  :: new_size
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------      
      INTEGER :: old_size, ierr
C-----------------------------------------------
C   P r e c o n d i t i o n s
C-----------------------------------------------          
      !Precondition : le pointeur doit etre affecte
      IF(.NOT. ASSOCIATED(ptr)) RETURN
      
      !il reste suffisamment de memoire
      ALLOCATE(JREALLOCATE(1:new_size), STAT=ierr)
      IF(ierr /= 0) STOP "memory allocation error"
C-----------------------------------------------                                            
       old_size = MIN(SIZE(ptr), new_size)
       JREALLOCATE(1:old_size) = ptr(1:old_size)                       
       DEALLOCATE(ptr) ! a ne surtout pas oublier    
            
      END FUNCTION JREALLOCATE
C-----------------------------------------------


          
      END MODULE REALLOC_MOD
