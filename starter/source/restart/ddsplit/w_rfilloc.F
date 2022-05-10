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
C
Chd|====================================================================
Chd|  W_RFILLOC                     source/restart/ddsplit/w_rfilloc.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|====================================================================
      SUBROUTINE W_RFILLOC(FILL,NODGLOB,NUMNOD,NUMNOD_L,NMULT,LEN_AM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODGLOB(*),NUMNOD, NUMNOD_L, NMULT, LEN_AM
      my_real
     .        FILL(NUMNOD,*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
      my_real, DIMENSION(:), ALLOCATABLE :: RTMP
C-----------------------------------------------
!     allocate 1d array
      ALLOCATE( RTMP(NUMNOD_L) )
! ----------------------------------
C
      DO J = 1, NMULT
        DO I = 1, NUMNOD_L
          RTMP(I) = FILL(NODGLOB(I),J)
        ENDDO
        CALL WRITE_DB(RTMP,NUMNOD_L)
        LEN_AM = LEN_AM + NUMNOD_L
      END DO
C
! ----------------------------------
!     deallocate 1d array
      DEALLOCATE( RTMP )
! ----------------------------------
C
      RETURN
      END
