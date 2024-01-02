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
C
Chd|====================================================================
Chd|  W_SKWSPH                      source/restart/ddsplit/w_skwsph.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|====================================================================
      SUBROUTINE W_SKWSPH(SKEW,NUMSPH_L,CEPSP,PROC,LEN_AM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMSPH_L, PROC, LEN_AM,
     .        CEPSP(*)
      my_real
     .        SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IE_L
      my_real, DIMENSION(:,:), ALLOCATABLE :: SKEW_L
C-----------------------------------------------
C
! -----------------------------
!     allocate 2d array
      ALLOCATE( SKEW_L(LSKEW,NUMSPH_L) )
! -----------------------------
      IE_L = 0
C
      DO I = 1, NUMSPH
        IF(CEPSP(I)==PROC) THEN
          IE_L = IE_L + 1
          SKEW_L(1,IE_L) = SKEW(1,I)
          SKEW_L(2,IE_L) = SKEW(2,I)
          SKEW_L(3,IE_L) = SKEW(3,I)
          SKEW_L(4,IE_L) = SKEW(4,I)
          SKEW_L(5,IE_L) = SKEW(5,I)
          SKEW_L(6,IE_L) = SKEW(6,I)
          SKEW_L(7,IE_L) = SKEW(7,I)
          SKEW_L(8,IE_L) = SKEW(8,I)
          SKEW_L(9,IE_L) = SKEW(9,I)
          SKEW_L(10,IE_L) = SKEW(10,I)
          SKEW_L(11,IE_L) = SKEW(11,I)
          SKEW_L(12,IE_L) = SKEW(12,I)
        END IF
      END DO
C
      CALL WRITE_DB(SKEW_L,LSKEW*NUMSPH_L)
      LEN_AM = LEN_AM + LSKEW*NUMSPH_L
! -----------------------------
!     deallocate 2d array
      DEALLOCATE( SKEW_L )
! -----------------------------
C
      RETURN
      END
