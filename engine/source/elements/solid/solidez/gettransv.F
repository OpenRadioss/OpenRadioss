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
Chd|  GETTRANSV                     source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|        M24C2ARM                      source/elements/solid/solidez/szhour3.F
Chd|        MDAMA24                       source/elements/solid/solidez/mdama24.F
Chd|        MMODUL                        source/elements/solid/solidez/szhour3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GETTRANSV(JFT    ,JLT   ,GAMA ,QC  ,QCG  ,
     .                     QGC    ,QG   ) 
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
     .   GAMA(MVSIZ,6),QC(MVSIZ,3,3),QCG(MVSIZ,3,3),
     .   QGC(MVSIZ,3,3),QG(MVSIZ,3,3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
C     REAL
      my_real
     .   E1(MVSIZ,3),E2(MVSIZ,3),E3(MVSIZ,3)
C-----------------------------------------------
       DO J= 1,3
        DO I=JFT,JLT
          E1(I,J)=GAMA(I,J)
          E2(I,J)=GAMA(I,J+3)
        ENDDO
       ENDDO
       DO I=JFT,JLT
          E3(I,1)=E1(I,2)*E2(I,3)-E1(I,3)*E2(I,2)
          E3(I,2)=E1(I,3)*E2(I,1)-E1(I,1)*E2(I,3)
          E3(I,3)=E1(I,1)*E2(I,2)-E1(I,2)*E2(I,1)
       ENDDO
C       
       DO J= 1,3
        DO I=JFT,JLT
         QC(I,1,J)=E1(I,J)*E1(I,J)
         QC(I,2,J)=E2(I,J)*E2(I,J)
         QC(I,3,J)=E3(I,J)*E3(I,J)
C
         QGC(I,1,J)=E1(I,J)*E2(I,J)
         QGC(I,2,J)=E2(I,J)*E3(I,J)
         QGC(I,3,J)=E1(I,J)*E3(I,J)
        ENDDO
       ENDDO
       DO J= 1,3
        K = J+1
        IF (K>3) K = 1
        DO I=JFT,JLT
         QG(I,1,J)=E1(I,J)*E2(I,K)+E2(I,J)*E1(I,K)
         QG(I,2,J)=E2(I,J)*E3(I,K)+E3(I,J)*E2(I,K)
         QG(I,3,J)=E3(I,J)*E1(I,K)+E1(I,J)*E3(I,K)
C
         QCG(I,1,J)=TWO*E1(I,J)*E1(I,K)
         QCG(I,2,J)=TWO*E2(I,J)*E2(I,K)
         QCG(I,3,J)=TWO*E3(I,J)*E3(I,K)
        ENDDO
       ENDDO
C      
      RETURN
      END
