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
!||    local_index   ../starter/source/interfaces/interf1/local_index.F
!||--- called by ------------------------------------------------------
!||    i12chk3       ../starter/source/interfaces/inter3d1/i12chk3.F
!||    i1chk2        ../starter/source/interfaces/inter2d1/i1chk2.F
!||    i1chk3        ../starter/source/interfaces/inter3d1/i1chk3.F
!||    i3sti2        ../starter/source/interfaces/inter2d1/i3sti2.F
!||    i3sti3        ../starter/source/interfaces/inter3d1/i3sti3.F
!||    i6sti3        ../starter/source/interfaces/inter3d1/i6sti3.F
!||    i9sti2        ../starter/source/interfaces/int09/i9sti2.F
!||    i9sti3        ../starter/source/interfaces/int09/i9sti3.F
!||====================================================================
      SUBROUTINE LOCAL_INDEX(IL,IG,NODES,N)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     Search IL such as NODES(IL)=IG
C       PRECONDITION : NODES(:) MUST BE ORDERED
C       SEARCH ALGORITHM : Dichotomic Search
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN)  :: IG, N, NODES(N)
      INTEGER, INTENT(OUT) :: IL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IMIN, IMAX, K, M
C-----------------------------------------------
      IMIN=0
      IMAX=N
C
      M=N/2
      DO WHILE (M > 0)
        K=M+IMIN
        IF(NODES(K) < IG) THEN
          IMIN=K
          M=IMAX-IMIN
        ELSE
          IMAX=K
        ENDIF
        M=M/2
      ENDDO
      IL=IMAX
C
      RETURN
      END
