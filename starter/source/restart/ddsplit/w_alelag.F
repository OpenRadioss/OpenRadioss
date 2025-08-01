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
!||    w_alelag   ../starter/source/restart/ddsplit/w_alelag.F
!||--- called by ------------------------------------------------------
!||    ddsplit    ../starter/source/restart/ddsplit/ddsplit.F
!||--- calls      -----------------------------------------------------
!||====================================================================
      SUBROUTINE W_ALELAG(DFLOW,VFLOW,WFLOW,NODGLOB,NUMNOD_L,LEN_AM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODGLOB(*),NUMNOD_L, LEN_AM
      my_real
     .       DFLOW(3,*),VFLOW(3,*),WFLOW(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N
      my_real, DIMENSION(:,:), ALLOCATABLE :: RTMP1,RTMP2,RTMP3
C-----------------------------------------------
!      2d array
       ALLOCATE( RTMP1(3,NUMNOD_L),RTMP2(3,NUMNOD_L) )
       ALLOCATE( RTMP3(3,NUMNOD_L) )
! ------------------------------------
C
       DO I = 1, NUMNOD_L
        N = NODGLOB(I)
        RTMP1(1,I) = DFLOW(1,N)
        RTMP1(2,I) = DFLOW(2,N)
        RTMP1(3,I) = DFLOW(3,N)
C
        RTMP2(1,I) = VFLOW(1,N)
        RTMP2(2,I) = VFLOW(2,N)
        RTMP2(3,I) = VFLOW(3,N)
C
        RTMP3(1,I) = WFLOW(1,N)
        RTMP3(2,I) = WFLOW(2,N)
        RTMP3(3,I) = WFLOW(3,N)        
      ENDDO
C displacement
      CALL WRITE_DB(RTMP1,3*NUMNOD_L)
      LEN_AM = LEN_AM + 3*NUMNOD_L
C velocity     
      CALL WRITE_DB(RTMP2,3*NUMNOD_L)
      LEN_AM = LEN_AM + 3*NUMNOD_L
C grille velocity     
      CALL WRITE_DB(RTMP3,3*NUMNOD_L)
      LEN_AM = LEN_AM + 3*NUMNOD_L
C
! ------------------------------------
!      2d array
       DEALLOCATE( RTMP1,RTMP2 )
       DEALLOCATE( RTMP3 )
! ------------------------------------
      RETURN
      END      
