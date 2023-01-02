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
C
Chd|====================================================================
Chd|  W_R3NLOC                      source/restart/ddsplit/w_r3nloc.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|====================================================================
      SUBROUTINE W_R3NLOC(RTAB,NODGLOB,NUMNOD_L,LEN_AM)
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
      INTEGER NODGLOB(*), NUMNOD_L, LEN_AM
      my_real
     .        RTAB(3,*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N
      my_real, DIMENSION(:,:), ALLOCATABLE :: RTMP
C-----------------------------------------------
!     allocate 2d array
      ALLOCATE( RTMP(3,NUMNOD_L) )
! ---------------------------------
C
      DO I = 1, NUMNOD_L
        N = NODGLOB(I)
        RTMP(1,I) = RTAB(1,N)
        RTMP(2,I) = RTAB(2,N)
        RTMP(3,I) = RTAB(3,N)
      ENDDO
C
      CALL WRITE_DB(RTMP,3*NUMNOD_L)
      LEN_AM = LEN_AM + 3*NUMNOD_L
C
!     deallocate 2d array
      DEALLOCATE( RTMP )
! ---------------------------------
      RETURN
      END
