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
Chd|  MMOD_NORM                     source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        SZHOUR3_OR                    source/elements/solid/solidez/szhour3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MMOD_NORM(JFT  ,JLT   ,GG    ,CC     ,CG    ,G33     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT  
C     REAL
      my_real
     .   GG(*),CC(MVSIZ,3,3),CG(MVSIZ,3,3),G33(MVSIZ,3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
C     REAL
      my_real
     .   G_1(MVSIZ)
C-----------------------------------------------
C-------Normalized by GG------
        DO I=JFT,JLT
         G_1(I) =ONE/GG(I)
        END DO
C       
         DO J=1,3
          DO K=J,3
           DO I=JFT,JLT
            CC(I,J,K) = CC(I,J,K)*G_1(I)
            G33(I,J,K) = G33(I,J,K)*G_1(I)
            CC(I,K,J) = CC(I,J,K)
            G33(I,K,J) = G33(I,J,K)
           ENDDO
          END DO
         END DO
c
         DO J=1,3
          DO K=1,3
           DO I=JFT,JLT
            CG(I,J,K) = CG(I,J,K)*G_1(I)
           END DO
          END DO
         END DO
C
      RETURN
      END
