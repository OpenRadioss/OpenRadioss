Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  S8ROTA3                       source/elements/solid/solide8/s8rota3.F
Chd|-- called by -----------
Chd|        S8FORC3                       source/elements/solid/solide8/s8forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ROTA3(SIG,WXX,WYY,WZZ,NEL)
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
      INTEGER NEL
C     REAL
      my_real
     .   SIG(NEL,6), WXX(MVSIZ), WYY(MVSIZ), WZZ(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     . S1, S2, S3, S4, S5, S6,
     . Q1, Q2, Q3
C-----------------------------------------------
      IF (JCVT == 0) THEN
        DO I=LFT,LLT
         S1=SIG(I,1)
         S2=SIG(I,2)
         S3=SIG(I,3)
         S4=SIG(I,4)
         S5=SIG(I,5)
         S6=SIG(I,6)
C
         Q1=TWO*S4*WZZ(I)
         Q2=TWO*S6*WYY(I)
         Q3=TWO*S5*WXX(I) 
C

         SIG(I,1)=S1-Q1+Q2
         SIG(I,2)=S2+Q1-Q3
         SIG(I,3)=S3-Q2+Q3
         SIG(I,4)=S4+WZZ(I)*(S1-S2)+WYY(I)*S5-WXX(I)*S6
         SIG(I,5)=S5+WXX(I)*(S2-S3)+WZZ(I)*S6-WYY(I)*S4
         SIG(I,6)=S6+WYY(I)*(S3-S1)+WXX(I)*S4-WZZ(I)*S5
        ENDDO
      ENDIF
C
      RETURN
      END
