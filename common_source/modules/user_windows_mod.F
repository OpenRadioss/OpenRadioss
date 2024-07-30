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
      !||====================================================================
      !||    user_windows_mod        ../common_source/modules/user_windows_mod.F
      !||--- called by ------------------------------------------------------
      !||    c_front                 ../starter/source/restart/ddsplit/c_front.F
      !||    contrl                  ../starter/source/starter/contrl.F
      !||    ddsplit                 ../starter/source/restart/ddsplit/ddsplit.F
      !||    hm_read_window_user     ../starter/source/tools/userwi/hm_read_window_user.F
      !||    lectur                  ../engine/source/input/lectur.F
      !||    rdresb                  ../engine/source/output/restart/rdresb.F
      !||    resol                   ../engine/source/engine/resol.F
      !||    resol_head              ../engine/source/engine/resol_head.F
      !||    set_user_window_nodes   ../starter/source/user_interface/user_windows_tools.F
      !||    sortie_main             ../engine/source/output/sortie_main.F
      !||    spmd_exch_userwi        ../engine/source/mpi/user_interface/spmd_exch_userwi.F
      !||    spmd_userwi_rest        ../starter/source/user_interface/user_windows_tools.F
      !||    st_qaprint_driver       ../starter/source/output/qaprint/st_qaprint_driver.F
      !||    st_qaprint_userwi       ../starter/source/output/qaprint/st_qaprint_userwi.F
      !||    starter0                ../starter/source/starter/starter0.F
      !||    user_interface_mod      ../engine/share/modules/user_interface_mod.F
      !||    user_windows_routine    ../engine/source/user_interface/user_windows.F
      !||    userwi_read             ../engine/source/user_interface/userwindow_interface_routines.F
      !||    userwi_write            ../engine/source/user_interface/userwindow_interface_routines.F
      !||    w_front                 ../starter/source/restart/ddsplit/w_front.F
      !||    wrrestp                 ../engine/source/output/restart/wrrestp.F
      !||====================================================================
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
        
      !||====================================================================
      !||    user_windows_init   ../common_source/modules/user_windows_mod.F
      !||--- called by ------------------------------------------------------
      !||    starter0            ../starter/source/starter/starter0.F
      !||====================================================================
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
      !||====================================================================
      !||    user_windows_clean   ../common_source/modules/user_windows_mod.F
      !||--- called by ------------------------------------------------------
      !||    radioss2             ../engine/source/engine/radioss2.F
      !||    starter0             ../starter/source/starter/starter0.F
      !||====================================================================
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


