!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
! Load buffer (time=0) from restart file.
!  specific to option /ALE/GRID/MASSFLOW 
      SUBROUTINE READ_ALE_GRID()
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      USE ALE_MOD , ONLY : ALE 
! ----------------------------------------------------------------------------------------------------------------------
        implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"               
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      my_real, DIMENSION(16) :: RTMP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------                   
      
      IF(ALE%GRID%NWALE == 7)THEN
        CALL READ_DB(RTMP,16)
        ALE%GRID%MASSFLOW_DATA%EIGENVEC(1:3,1) = RTMP(1:3)
        ALE%GRID%MASSFLOW_DATA%EIGENVEC(1:3,2) = RTMP(4:6)
        ALE%GRID%MASSFLOW_DATA%EIGENVEC(1:3,3) = RTMP(7:9)
        ALE%GRID%MASSFLOW_DATA%BETA0(1:6) = RTMP(10:15)
        ALE%GRID%MASSFLOW_DATA%MS_ELEM_MEAN_0 = RTMP(16)
      ENDIF

!-----------
      RETURN
      END
