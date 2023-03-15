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
Chd|  USER_WINDOWS_MOD                    share/modules1/sensor_mod.F   
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      MODULE USER_WINDOWS_MOD
!$COMMENT
!       USER_WINDOWS_MOD :
!               Type for UserWindows + SPMD Structures
!$ENDCOMMENT
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include "my_real.inc"
C-----------------------------------------------
        TYPE USER_WINDOWS_
           INTEGER :: HAS_USER_WINDOW
           INTEGER :: USER_WINDOWS_ID
           INTEGER :: NUVAR
           INTEGER :: NUVARI
           INTEGER :: S_IUSER
           INTEGER :: S_USER
           INTEGER :: N_USERNODS
           my_real :: USREINT
           INTEGER, DIMENSION(:),ALLOCATABLE :: IUSER
           my_real, DIMENSION(:),ALLOCATABLE :: USER
           INTEGER, DIMENSION(:),ALLOCATABLE :: USERNODS
           ! SPMD Structures
           INTEGER :: S_FR_USERW
           INTEGER, DIMENSION(:),ALLOCATABLE :: IAD_USERW
           INTEGER, DIMENSION(:),ALLOCATABLE :: FR_USERW
           
           ! UserWindows SAV_ARRAYS
           my_real, DIMENSION(:,:),ALLOCATABLE :: A_SAV
           my_real, DIMENSION(:,:),ALLOCATABLE :: AR_SAV
           
           INTEGER :: S_WA
           INTEGER, DIMENSION(:),ALLOCATABLE :: WA
        END TYPE USER_WINDOWS_

        CONTAINS 
        
        SUBROUTINE USER_WINDOWS_INIT(USER_WINDOW)
            TYPE(USER_WINDOWS_) :: USER_WINDOW
            USER_WINDOW%HAS_USER_WINDOW = 0
            USER_WINDOW%NUVAR           = 0
            USER_WINDOW%NUVARI          = 0
            USER_WINDOW%N_USERNODS      = 0
            USER_WINDOW%S_FR_USERW      = 0
            USER_WINDOW%S_IUSER         = 0
            USER_WINDOW%S_USER          = 0
            USER_WINDOW%USREINT         = 0
        END SUBROUTINE  USER_WINDOWS_INIT


        ! Clean subroutine - deallocates all arrays
        ! -----------------------------------------
        SUBROUTINE USER_WINDOWS_CLEAN(USER_WINDOW)
            TYPE(USER_WINDOWS_) :: USER_WINDOW

            IF(USER_WINDOW%HAS_USER_WINDOW == 1)THEN
              IF(ALLOCATED(USER_WINDOW%IUSER))     DEALLOCATE(USER_WINDOW%IUSER)
              IF(ALLOCATED(USER_WINDOW%USER))      DEALLOCATE(USER_WINDOW%USER)
              IF(ALLOCATED(USER_WINDOW%USERNODS))  DEALLOCATE(USER_WINDOW%USERNODS)
              IF(ALLOCATED(USER_WINDOW%IAD_USERW)) DEALLOCATE(USER_WINDOW%IAD_USERW)
              IF(ALLOCATED(USER_WINDOW%FR_USERW))  DEALLOCATE(USER_WINDOW%FR_USERW)
              IF(ALLOCATED(USER_WINDOW%A_SAV))     DEALLOCATE(USER_WINDOW%A_SAV)
              IF(ALLOCATED(USER_WINDOW%AR_SAV))    DEALLOCATE(USER_WINDOW%AR_SAV)
              IF(ALLOCATED(USER_WINDOW%WA))    DEALLOCATE(USER_WINDOW%WA)
            ENDIF
            USER_WINDOW%HAS_USER_WINDOW = 0
            USER_WINDOW%NUVAR           = 0
            USER_WINDOW%NUVARI          = 0
            USER_WINDOW%N_USERNODS      = 0
            USER_WINDOW%S_FR_USERW      = 0
            USER_WINDOW%S_IUSER         = 0
            USER_WINDOW%S_USER          = 0
            USER_WINDOW%USREINT         = 0
        END SUBROUTINE  USER_WINDOWS_CLEAN

      END MODULE USER_WINDOWS_MOD
C-----------------------------------------------


