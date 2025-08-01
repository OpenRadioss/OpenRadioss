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
C
!||====================================================================
!||    w_reloc    ../starter/source/restart/ddsplit/w_reloc.F
!||--- called by ------------------------------------------------------
!||    ddsplit    ../starter/source/restart/ddsplit/ddsplit.F
!||--- calls      -----------------------------------------------------
!||====================================================================
      SUBROUTINE W_RELOC(RTAB,CEP,P,NUMEL,NUMEL_L,LEN_AM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER CEP(*), P, NUMEL, NUMEL_L, LEN_AM
      my_real
     .        RTAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, EL
      my_real, DIMENSION(:), ALLOCATABLE :: RTMP
C-----------------------------------------------
!     allocate 1d array
      ALLOCATE( RTMP(NUMEL_L) )
! --------------------------------
C
      EL = 0
      DO I = 1, NUMEL
        IF(CEP(I)==P) THEN
          EL = EL + 1
          RTMP(EL) = RTAB(I)
        ENDIF
      ENDDO
C
      CALL WRITE_DB(RTMP,NUMEL_L)
      LEN_AM = LEN_AM + NUMEL_L
C
! --------------------------------
!     deallocate 1d array
      DEALLOCATE( RTMP )
! --------------------------------
      RETURN
      END
