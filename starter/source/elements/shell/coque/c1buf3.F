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
!||    c1buf3     ../starter/source/elements/shell/coque/c1buf3.F
!||--- called by ------------------------------------------------------
!||    c3init3    ../starter/source/elements/sh3n/coque3n/c3init3.F
!||    cbainit3   ../starter/source/elements/shell/coqueba/cbainit3.F
!||    cdkinit3   ../starter/source/elements/sh3n/coquedk/cdkinit3.F
!||    cinit3     ../starter/source/elements/shell/coque/cinit3.F
!||====================================================================
      SUBROUTINE C1BUF3(GEO,THK,OFF,THKE,KSHTREE,SHTREE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KSHTREE, SHTREE(KSHTREE,*)
      my_real
     .   GEO(NPROPG,*), THK(*), OFF(*),THKE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N
C=======================================================================
      IF(NADMESH==0)THEN
        DO I=LFT,LLT
          THK(I)=THKE(I)
          OFF(I)=ONE
        END DO
      ELSE
        DO I=LFT,LLT
          THK(I)=THKE(I)
          N=NFT+I
          IF(SHTREE(3,N)>=0)THEN
            OFF(I)=ONE
          ELSE
            OFF(I)=-ONE
          END IF
        END DO
      END IF
C-----------
      RETURN
      END
