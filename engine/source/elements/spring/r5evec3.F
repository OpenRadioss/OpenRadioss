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
Chd|  R5EVEC3                       source/elements/spring/r5evec3.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE R5EVEC3(RLOC ,V    ,NGL  ,AL   ,X1 ,
     .                   Y1   ,Z1   ,X2   ,Y2   ,Z2 ,
     .                   EXX  ,EYX  ,EZX  ,EXY  ,EYY,
     .                   EZY  ,EXZ  ,EYZ  ,EZZ  ,RX1,
     .                   RY1  ,RZ1  ,RX2  ,RY2  ,RZ2,
     .                   VX1  ,VX2  ,VY1  ,VY2  ,VZ1,
     .                   VZ2  ,NC1  ,NC2  )
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
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGL(*),NC1(*),NC2(*)
C     REAL
      my_real
     .   RLOC(6,*),V(3,*),X1(*),X2(*),Y1(*),Y2(*),
     .   Z1(*),Z2(*),
     .   EXX(MVSIZ), EYX(MVSIZ), EZX(MVSIZ),
     .   EXY(MVSIZ), EYY(MVSIZ), EZY(MVSIZ),
     .   EXZ(MVSIZ), EYZ(MVSIZ), EZZ(MVSIZ),
     .   RX1(MVSIZ),RX2(MVSIZ),RY1(MVSIZ),
     .   RY2(MVSIZ),RZ1(MVSIZ),RZ2(MVSIZ),AL(MVSIZ),
     .   VX1(MVSIZ),VX2(MVSIZ),VY1(MVSIZ),VY2(MVSIZ),
     .   VZ1(MVSIZ),VZ2(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C     REAL
      my_real
     .     RX(3),RXX1, RXX2, SINT(MVSIZ), 
     .     SUM(MVSIZ) ,SUM2(MVSIZ), SUM3(MVSIZ) ,THETA(MVSIZ),
     .     COST(MVSIZ)
C-----------------------------------------------
      DO I=LFT,LLT
        EXX(I)=(X2(I)-X1(I))
        EYX(I)=(Y2(I)-Y1(I))
        EZX(I)=(Z2(I)-Z1(I))
        AL(I) =SQRT(EXX(I)**2+EYX(I)**2+EZX(I)**2)
      ENDDO
C
      DO I=LFT,LLT
        IF (AL(I) <= EM15) THEN
          EXX(I)= ONE
          EYX(I)= ZERO
          EZX(I)= ZERO
          EXY(I)= ZERO
          EYY(I)= ONE
          EZY(I)= ZERO	  
        ELSE
          EXX(I)=EXX(I)/AL(I) 
          EYX(I)=EYX(I)/AL(I) 
          EZX(I)=EZX(I)/AL(I) 
        ENDIF
      ENDDO
C
      DO I=LFT,LLT
        EXY(I)=RLOC(4,I)
        EYY(I)=RLOC(5,I)
        EZY(I)=RLOC(6,I)
      ENDDO
C
      DO I=LFT,LLT
        EXZ(I)=EYX(I)*EZY(I)-EZX(I)*EYY(I)
        EYZ(I)=EZX(I)*EXY(I)-EXX(I)*EZY(I)
        EZZ(I)=EXX(I)*EYY(I)-EYX(I)*EXY(I)
      ENDDO
C
      DO I=LFT,LLT
        EXY(I)=EYZ(I)*EZX(I)-EZZ(I)*EYX(I)
        EYY(I)=EZZ(I)*EXX(I)-EXZ(I)*EZX(I)
        EZY(I)=EXZ(I)*EYX(I)-EYZ(I)*EXX(I)
      ENDDO
C--------------------------------------------
C     TORSION MOYENNE EN COORDONNEES GLOBALES
C--------------------------------------------
      DO I=LFT,LLT
        RXX1   = EXX(I)*RX1(I)+EYX(I)*RY1(I)+EZX(I)*RZ1(I)
        RXX2   = EXX(I)*RX2(I)+EYX(I)*RY2(I)+EZX(I)*RZ2(I)
        THETA(I) = (RXX1+RXX2)/TWO*DT1
        SUM2(I)  = MAX(EM15,SQRT(EXY(I)**2+EYY(I)**2+EZY(I)**2))
        SUM3(I)  = MAX(EM15,SQRT(EXZ(I)**2+EYZ(I)**2+EZZ(I)**2))
        COST(I)  = COS(THETA(I))/SUM2(I)
        SINT(I)  = SIN(THETA(I))/SUM3(I)
      ENDDO
C ... it is modified.
      DO I=LFT,LLT
        EXY(I)= EXY(I)*COST(I)+EXZ(I)*SINT(I)
        EYY(I)= EYY(I)*COST(I)+EYZ(I)*SINT(I)
        EZY(I)= EZY(I)*COST(I)+EZZ(I)*SINT(I)
      ENDDO
C
      DO I=LFT,LLT
        SUM(I)=MAX(EM15,SQRT(EXY(I)**2+EYY(I)**2+EZY(I)**2))
        EXY(I)=EXY(I)/SUM(I) 
        EYY(I)=EYY(I)/SUM(I) 
        EZY(I)=EZY(I)/SUM(I) 
      ENDDO
C
      DO I=LFT,LLT
        EXZ(I)=EYX(I)*EZY(I)-EZX(I)*EYY(I)
        EYZ(I)=EZX(I)*EXY(I)-EXX(I)*EZY(I)
        EZZ(I)=EXX(I)*EYY(I)-EYX(I)*EXY(I)
      ENDDO
C
      DO I=LFT,LLT
        SUM(I)=MAX(EM15,SQRT(EXZ(I)**2+EYZ(I)**2+EZZ(I)**2))
        EXZ(I)=EXZ(I)/SUM(I) 
        EYZ(I)=EYZ(I)/SUM(I) 
        EZZ(I)=EZZ(I)/SUM(I) 
      ENDDO
C
      DO I=LFT,LLT
        RLOC(1,I) = EXX(I)
        RLOC(2,I) = EYX(I)
        RLOC(3,I) = EZX(I)
        RLOC(4,I) = EXY(I)
        RLOC(5,I) = EYY(I)
        RLOC(6,I) = EZY(I)
      ENDDO
C
      DO I=LFT,LLT
        VX1(I)=V(1,NC1(I))
        VY1(I)=V(2,NC1(I))
        VZ1(I)=V(3,NC1(I))
        VX2(I)=V(1,NC2(I))
        VY2(I)=V(2,NC2(I))
        VZ2(I)=V(3,NC2(I))
      ENDDO
C---
      RETURN
      END
