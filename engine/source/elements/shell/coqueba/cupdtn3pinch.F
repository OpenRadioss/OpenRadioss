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
Chd|====================================================================
Chd|  CUPDTN3PINCH                  source/elements/shell/coqueba/cupdtn3pinch.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CUPDTN3PINCH(
     1                        JFT  ,JLT    ,NVC  ,IXC       , 
     2                        FP   ,FPINCH ,STI  ,STIFPINCH ,FACP )
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
      INTEGER JFT, JLT, NVC
      INTEGER IXC(NIXC,MVSIZ)
      my_real
     .   FP(MVSIZ,3,4), FPINCH(3,*),
     .   STI(MVSIZ), STIFPINCH(*), FACP(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NVC1, NVC2, NVC3, NVC4, I, J
C-----------------------------------------------
C 
      NVC1= NVC/8
      NVC2=(NVC-NVC1*8)/4
      NVC3=(NVC-NVC1*8-NVC2*4)/2
      NVC4=(NVC-NVC1*8-NVC2*4-NVC3*2)
C
      IF(NVC1==0)THEN
#include "vectorize.inc"
        DO I=JFT,JLT
          FPINCH(1,IXC(2,I))=FPINCH(1,IXC(2,I))-FP(I,1,1)
          FPINCH(2,IXC(2,I))=FPINCH(2,IXC(2,I))-FP(I,2,1)
          FPINCH(3,IXC(2,I))=FPINCH(3,IXC(2,I))-FP(I,3,1)
          STIFPINCH(IXC(2,I))=STIFPINCH(IXC(2,I))+STI(I)*FACP(I)
        ENDDO
      ELSE
        DO I=JFT,JLT
          FPINCH(1,IXC(2,I))=FPINCH(1,IXC(2,I))-FP(I,1,1)
          FPINCH(2,IXC(2,I))=FPINCH(2,IXC(2,I))-FP(I,2,1)
          FPINCH(3,IXC(2,I))=FPINCH(3,IXC(2,I))-FP(I,3,1)
          STIFPINCH(IXC(2,I))=STIFPINCH(IXC(2,I))+STI(I)*FACP(I)
        ENDDO
      ENDIF        
C
      IF(NVC2==0)THEN
#include "vectorize.inc"
        DO I=JFT,JLT
          FPINCH(1,IXC(3,I))=FPINCH(1,IXC(3,I))-FP(I,1,2)
          FPINCH(2,IXC(3,I))=FPINCH(2,IXC(3,I))-FP(I,2,2)
          FPINCH(3,IXC(3,I))=FPINCH(3,IXC(3,I))-FP(I,3,2)
          STIFPINCH(IXC(3,I))=STIFPINCH(IXC(3,I))+STI(I)*FACP(I)
        ENDDO
      ELSE
        DO I=JFT,JLT
          FPINCH(1,IXC(3,I))=FPINCH(1,IXC(3,I))-FP(I,1,2)
          FPINCH(2,IXC(3,I))=FPINCH(2,IXC(3,I))-FP(I,2,2)
          FPINCH(3,IXC(3,I))=FPINCH(3,IXC(3,I))-FP(I,3,2)
          STIFPINCH(IXC(3,I))=STIFPINCH(IXC(3,I))+STI(I)*FACP(I)
        ENDDO
      ENDIF        
C
      IF(NVC3==0)THEN
#include "vectorize.inc"
        DO I=JFT,JLT
          FPINCH(1,IXC(4,I))=FPINCH(1,IXC(4,I))-FP(I,1,3)
          FPINCH(2,IXC(4,I))=FPINCH(2,IXC(4,I))-FP(I,2,3)
          FPINCH(3,IXC(4,I))=FPINCH(3,IXC(4,I))-FP(I,3,3)
          STIFPINCH(IXC(4,I))=STIFPINCH(IXC(4,I))+STI(I)*FACP(I)
        ENDDO
      ELSE
        DO I=JFT,JLT
          FPINCH(1,IXC(4,I))=FPINCH(1,IXC(4,I))-FP(I,1,3)
          FPINCH(2,IXC(4,I))=FPINCH(2,IXC(4,I))-FP(I,2,3)
          FPINCH(3,IXC(4,I))=FPINCH(3,IXC(4,I))-FP(I,3,3)
          STIFPINCH(IXC(4,I))=STIFPINCH(IXC(4,I))+STI(I)*FACP(I)
        ENDDO
      ENDIF        
C
      IF(NVC4==0)THEN
#include "vectorize.inc"
        DO I=JFT,JLT
          FPINCH(1,IXC(5,I))=FPINCH(1,IXC(5,I))-FP(I,1,4)
          FPINCH(2,IXC(5,I))=FPINCH(2,IXC(5,I))-FP(I,2,4)
          FPINCH(3,IXC(5,I))=FPINCH(3,IXC(5,I))-FP(I,3,4)
          STIFPINCH(IXC(5,I))=STIFPINCH(IXC(5,I))+STI(I)*FACP(I)
        ENDDO
      ELSE
        DO I=JFT,JLT
          FPINCH(1,IXC(5,I))=FPINCH(1,IXC(5,I))-FP(I,1,4)
          FPINCH(2,IXC(5,I))=FPINCH(2,IXC(5,I))-FP(I,2,4)
          FPINCH(3,IXC(5,I))=FPINCH(3,IXC(5,I))-FP(I,3,4)
          STIFPINCH(IXC(5,I))=STIFPINCH(IXC(5,I))+STI(I)*FACP(I)
        ENDDO
      ENDIF
C
      RETURN
      END
