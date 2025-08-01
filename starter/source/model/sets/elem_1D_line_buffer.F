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
!||    elem_1d_line_buffer   ../starter/source/model/sets/elem_1D_line_buffer.F
!||--- called by ------------------------------------------------------
!||    line_buffer           ../starter/source/model/sets/line_buffer.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE ELEM_1D_LINE_BUFFER(
     .                   IX     ,NIX      ,NIX1      ,NIX2   ,IELTYP,
     .                   NSEG   ,IAD_LINE ,BUFTMPLINE,CLAUSE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SETDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX,NIX1,NIX2,IELTYP,NSEG,IAD_LINE
      INTEGER IX(NIX,*),BUFTMPLINE(*)
!
      TYPE (SET_) :: CLAUSE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,JJ,K,NODE(2),NELEM
C=======================================================================
!
!     ! LINE segments
      NELEM = 0
      IF (IELTYP == 4) NELEM = CLAUSE%NB_TRUSS
      IF (IELTYP == 5) NELEM = CLAUSE%NB_BEAM
      IF (IELTYP == 6) NELEM = CLAUSE%NB_SPRING
!
      DO J=1,NELEM
        JJ = -HUGE(JJ)
        IF (IELTYP == 4) JJ = CLAUSE%TRUSS(J)
        IF (IELTYP == 5) JJ = CLAUSE%BEAM(J)
        IF (IELTYP == 6) JJ = CLAUSE%SPRING(J)
!
        NSEG=NSEG+1                           
!
        DO K=NIX1,NIX2
          NODE(K-1) = IX(K,JJ)
        ENDDO
!
        DO K=NIX1,NIX2                      
          BUFTMPLINE(IAD_LINE) = NODE(K-1)
          IAD_LINE=IAD_LINE+1
        ENDDO
!
        BUFTMPLINE(IAD_LINE) = IELTYP
        IAD_LINE=IAD_LINE+1
        BUFTMPLINE(IAD_LINE) = JJ ! elem_ID
        IAD_LINE=IAD_LINE+1
      ENDDO ! DO J=1,NELEM
C-----------
      RETURN
      END
