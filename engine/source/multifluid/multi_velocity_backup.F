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
Chd|  MULTI_VELOCITY_BACKUP
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE MULTI_VELOCITY_BACKUP(ITASK,MULTI_FVM,N2D,NUMELS,NUMELQ,NUMELTG)
!$COMMENT
!       velocity backup to derivate later into acceleration (post-traitement)
!       in multi_update_global() subroutine : ACC[n] = (VEL[n]-VEL[n-1])/TIMESTEP
!$ENDCOMMENT  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------      
        USE MULTI_FVM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ITASK,NUMELS,NUMELQ,NUMELTG,N2D
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,NUMEL,NODF,NODL
C-----------------------------------------------
        IF(N2D == 0)THEN
          NUMEL = NUMELS
        ELSE
          NUMEL = NUMELQ + NUMELTG
        ENDIF
        IF(ALLOCATED(MULTI_FVM%ACC))THEN
          NODF = 1 + ITASK * NUMEL / NTHREAD
          NODL = (1 + ITASK) * NUMEL / NTHREAD
          DO I = NODF, NODL
            MULTI_FVM%ACC(1, I) = MULTI_FVM%VEL(1, I)
            MULTI_FVM%ACC(2, I) = MULTI_FVM%VEL(2, I)
            MULTI_FVM%ACC(3, I) = MULTI_FVM%VEL(3, I)
          ENDDO
        ENDIF

        RETURN
        END SUBROUTINE MULTI_VELOCITY_BACKUP
C-----------------------------------------------
