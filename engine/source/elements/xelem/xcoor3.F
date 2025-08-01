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
!||    xcoor3   ../engine/source/elements/xelem/xcoor3.F
!||--- called by ------------------------------------------------------
!||    animx    ../engine/source/output/anim/generate/animx.F
!||    xforc3   ../engine/source/elements/xelem/xforc3.F
!||====================================================================
      SUBROUTINE XCOOR3(X   ,KXX  ,IXX   ,ITAB  ,NX   ,
     2                  UID ,UIX  ,XUSR  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr23_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXX(NIXX),IXX(*),NX,ITAB(*),UIX(NX),UID
C     REAL
      my_real
     .   X(3,*),XUSR(3,NX)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,I1,IADNOD
C     REAL
C--------------------------------------------
C     COORDONNEES.
C--------------------------------------------
C     NX    =KXX(3)
      IADNOD=KXX(4)
      DO K=1,NX
         I1=IXX(IADNOD+K-1)
         UIX(K)   =ITAB(I1)
         XUSR(1,K)=X(1,I1)
         XUSR(2,K)=X(2,I1)
         XUSR(3,K)=X(3,I1)
      ENDDO
      UID=KXX(5)
      RETURN
      END
