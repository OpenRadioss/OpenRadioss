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
Chd|====================================================================
Chd|  M32ELAS                       source/materials/mat/mat032/m32elas.F
Chd|-- called by -----------
Chd|        SIGEPS32C                     source/materials/mat/mat032/sigeps32c.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M32ELAS(JFT    ,JLT    ,PM     ,IMAT   ,GS     ,
     .                   SIGOXX ,SIGOYY ,SIGOXY ,SIGOYZ ,SIGOZX ,
     .                   SIGNXX ,SIGNYY ,SIGNXY ,SIGNYZ ,SIGNZX ,
     .                   DEPSXX ,DEPSYY ,DEPSXY ,DEPSYZ ,DEPSZX ,
     .                   NEL    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IMAT,JFT,JLT,NEL
C     REAL
      my_real
     . PM(NPROPM,*),GS(*),
     . SIGOXX(NEL),SIGOYY(NEL),SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     . SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     . DEPSXX(MVSIZ),DEPSYY(MVSIZ),DEPSXY(MVSIZ),DEPSYZ(MVSIZ),
     . DEPSZX(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     . A11,A21,G11
C-----------------------------------------------
!
      G11 = PM(22,IMAT)
      A11 = PM(24,IMAT)
      A21 = PM(25,IMAT)
!
      DO I=JFT,JLT
        SIGNXX(I) = SIGOXX(I) + A11*DEPSXX(I)+A21*DEPSYY(I)
        SIGNYY(I) = SIGOYY(I) + A21*DEPSXX(I)+A11*DEPSYY(I)
        SIGNXY(I) = SIGOXY(I) + G11  *DEPSXY(I)
        SIGNYZ(I) = SIGOYZ(I) + GS(I)*DEPSYZ(I)
        SIGNZX(I) = SIGOZX(I) + GS(I)*DEPSZX(I)
      ENDDO
!
      RETURN
      END
