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
Chd|  W_RNNLOC                      source/restart/ddsplit/w_rnnloc.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|====================================================================
      SUBROUTINE W_RNNLOC(RTAB,NODGLOB,NUMNOD_L,DIM,LEN_AM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODGLOB(*), NUMNOD_L, LEN_AM,DIM
      my_real
     .        RTAB(DIM,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N,J
      my_real, DIMENSION(:,:), ALLOCATABLE :: RTMP
C-----------------------------------------------
!     allocate 2d array
      ALLOCATE( RTMP(DIM,NUMNOD_L) )
! -----------------------------------
C
      DO I = 1, NUMNOD_L
        N = NODGLOB(I)
        DO J=1,DIM
           RTMP(J,I) = RTAB(J,N)
        ENDDO
      ENDDO
C
      CALL WRITE_DB(RTMP,DIM*NUMNOD_L)
      LEN_AM = LEN_AM + DIM*NUMNOD_L
C
! -----------------------------------
!     deallocate 2d array
      DEALLOCATE( RTMP )
! -----------------------------------
      RETURN
      END
