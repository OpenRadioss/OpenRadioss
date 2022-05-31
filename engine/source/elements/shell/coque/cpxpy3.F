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
Chd|  CPXPY3                        source/elements/shell/coque/cpxpy3.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CPXPY3(JFT ,JLT ,PM  ,STI  ,SMSTR,
     2                  PX1  ,PX2 ,PY1 ,PY2 ,STIR,
     3                  IXC  ,AREA,X2  ,X3  ,X4  , 
     4                  Y2   ,Y3  ,Y4  ,Z2  ,THK0,  
     5                  MAT  ,NEL )
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
      INTEGER MAT(MVSIZ), IXC(NIXC,*), JFT, JLT,NEL
C     REAL ou REAL*8
      my_real
     .   PM(NPROPM,*), STI(*), STIR(*),SMSTR(*),
     .   PX1(*), PX2(*), PY1(*), PY2(*),
     .   X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),AREA(MVSIZ),
     .   Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ), Z2(MVSIZ),THK0(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II(4), MX
C     REAL ou REAL*8
      my_real
     .     YOUNG , 
     .     AX, AN, PXX2, PYY2, PXN, PXX, AAA, AL2
C=======================================================================
      MX = MAT(JFT)  
      YOUNG=PM(20,MX)
C
      DO I=1,4
        II(I) = NEL*(I-1)
      ENDDO
C
      DO I=JFT,JLT
        Z2(I)  = ZERO
        PX1(I) = SMSTR(II(1)+I)
        PX2(I) = SMSTR(II(2)+I)
        PY1(I) = SMSTR(II(3)+I)
        PY2(I) = SMSTR(II(4)+I)
        X2(I)  =-TWO*PY1(I)
        Y2(I)  = ZERO
        X3(I)  =-TWO*PY2(I)
        Y3(I)  = TWO*PX2(I)
        X4(I)  = ZERO
        Y4(I)  =-TWO*PX1(I)
        Z2(I)  = ZERO
        AREA(I)= TWO*(PY2(I)*PX1(I)-PY1(I)*PX2(I))
      ENDDO
C
      DO I=JFT,JLT
        PXX = MAX(PX1(I)**2+PY1(I)**2,PX2(I)**2+PY2(I)**2)
        STI(I) = YOUNG * PXX  * THK0(I) / AREA(I)
        STIR(I)= ZERO
      ENDDO
C
      DO I=JFT,JLT
        IF (IXC(4,I) == IXC(5,I)) THEN
          AL2= (PY1(I)+PY2(I))**2
          AL2= MIN(AL2, PX1(I)**2+PY1(I)**2 )
          AL2= FOUR * MIN(AL2, PX2(I)**2+PY2(I)**2 )
          STI(I) = HALF * AREA(I) * THK0(I) * YOUNG / AL2
        ENDIF
      ENDDO
C-----------
      RETURN
      END
