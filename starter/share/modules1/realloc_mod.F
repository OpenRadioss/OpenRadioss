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
Chd|  REALLOC_MOD                   share/modules1/realloc_mod.F  
Chd|-- called by -----------
Chd|        I11TRIVOX1                    source/interfaces/inter3d1/i11trivox1.F
Chd|        I25TRIVOX_EDG                 source/interfaces/inter3d1/i25trivox_edg.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE REALLOC_MOD
      CONTAINS

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
        IF(.NOT. ASSOCIATED(ptr)) RETURN
        ALLOCATE(IREALLOCATE(1:new_size), STAT=ierr)
        IF(ierr /= 0) STOP "memory allocation error"
C-----------------------------------------------
        old_size = MIN(SIZE(ptr), new_size)
        IREALLOCATE(1:old_size) = ptr(1:old_size)
        DEALLOCATE(ptr)

        END FUNCTION IREALLOCATE
C-----------------------------------------------

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
     .  ,DIMENSION(:),POINTER :: ptr,JREALLOCATE
        INTEGER, intent(in)  :: new_size
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER :: old_size, ierr
C-----------------------------------------------
        IF(.NOT. ASSOCIATED(ptr)) RETURN

        ALLOCATE(JREALLOCATE(1:new_size), STAT=ierr)
        IF(ierr /= 0) STOP "memory allocation error"
C-----------------------------------------------
        old_size = MIN(SIZE(ptr), new_size)
        JREALLOCATE(1:old_size) = ptr(1:new_size)
        DEALLOCATE(ptr) 

        END FUNCTION JREALLOCATE
C-----------------------------------------------
      END MODULE REALLOC_MOD
