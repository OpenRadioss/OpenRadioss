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
Chd|  S8DEFO3                       source/elements/solid/solide8/s8defo3.F
Chd|-- called by -----------
Chd|        S8FORC3                       source/elements/solid/solide8/s8forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8DEFO3(
     1   VX,      VY,      VZ,      PX1,
     2   PX2,     PX3,     PX4,     PX5,
     3   PX6,     PX7,     PX8,     PY1,
     4   PY2,     PY3,     PY4,     PY5,
     5   PY6,     PY7,     PY8,     PZ1,
     6   PZ2,     PZ3,     PZ4,     PZ5,
     7   PZ6,     PZ7,     PZ8,     IPT,
     8   D1,      D2,      D3,      D4,
     9   D5,      D6,      WXX,     WYY,
     A   WZZ,     NEL,     JCVT)
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
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: JCVT
C     REAL
      my_real
     .    VX(MVSIZ,8), VY(MVSIZ,8), VZ(MVSIZ,8),
     .   PX1(MVSIZ,8),PX2(MVSIZ,8),PX3(MVSIZ,8),PX4(MVSIZ,8),      
     .   PX5(MVSIZ,8),PX6(MVSIZ,8),PX7(MVSIZ,8),PX8(MVSIZ,8),      
     .   PY1(MVSIZ,8),PY2(MVSIZ,8),PY3(MVSIZ,8),PY4(MVSIZ,8),      
     .   PY5(MVSIZ,8),PY6(MVSIZ,8),PY7(MVSIZ,8),PY8(MVSIZ,8),      
     .   PZ1(MVSIZ,8),PZ2(MVSIZ,8),PZ3(MVSIZ,8),PZ4(MVSIZ,8),      
     .   PZ5(MVSIZ,8),PZ6(MVSIZ,8),PZ7(MVSIZ,8),PZ8(MVSIZ,8),
     .    D1(MVSIZ,8), D2(MVSIZ,8), D3(MVSIZ,8), D4(MVSIZ,8),
     .    D5(MVSIZ,8), D6(MVSIZ,8),
     .   WXX(MVSIZ),WYY(MVSIZ),WZZ(MVSIZ)  
      INTEGER IPT    
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   DXX(MVSIZ), DYY(MVSIZ), DZZ(MVSIZ),
     .   DXY(MVSIZ), DYX(MVSIZ), DYZ(MVSIZ),
     .   DZY(MVSIZ), DZX(MVSIZ), DXZ(MVSIZ), DT1D2
C-----------------------------------------------
C
      DO 30  I=1,NEL
      DXX(I) =PX1(I,IPT)*VX(I,1)+PX2(I,IPT)*VX(I,2)
     +       +PX3(I,IPT)*VX(I,3)+PX4(I,IPT)*VX(I,4)
     +       +PX5(I,IPT)*VX(I,5)+PX6(I,IPT)*VX(I,6)
     +       +PX7(I,IPT)*VX(I,7)+PX8(I,IPT)*VX(I,8)
      DYY(I) =PY1(I,IPT)*VY(I,1)+PY2(I,IPT)*VY(I,2)
     +       +PY3(I,IPT)*VY(I,3)+PY4(I,IPT)*VY(I,4)
     +       +PY5(I,IPT)*VY(I,5)+PY6(I,IPT)*VY(I,6)
     +       +PY7(I,IPT)*VY(I,7)+PY8(I,IPT)*VY(I,8)
      DZZ(I) =PZ1(I,IPT)*VZ(I,1)+PZ2(I,IPT)*VZ(I,2)
     +       +PZ3(I,IPT)*VZ(I,3)+PZ4(I,IPT)*VZ(I,4)
     +       +PZ5(I,IPT)*VZ(I,5)+PZ6(I,IPT)*VZ(I,6)
     +       +PZ7(I,IPT)*VZ(I,7)+PZ8(I,IPT)*VZ(I,8)
      DXY(I) =PY1(I,IPT)*VX(I,1)+PY2(I,IPT)*VX(I,2)
     +       +PY3(I,IPT)*VX(I,3)+PY4(I,IPT)*VX(I,4)
     +       +PY5(I,IPT)*VX(I,5)+PY6(I,IPT)*VX(I,6)
     +       +PY7(I,IPT)*VX(I,7)+PY8(I,IPT)*VX(I,8)
      DXZ(I) =PZ1(I,IPT)*VX(I,1)+PZ2(I,IPT)*VX(I,2)
     +       +PZ3(I,IPT)*VX(I,3)+PZ4(I,IPT)*VX(I,4)
     +       +PZ5(I,IPT)*VX(I,5)+PZ6(I,IPT)*VX(I,6)
     +       +PZ7(I,IPT)*VX(I,7)+PZ8(I,IPT)*VX(I,8)
      DYX(I) =PX1(I,IPT)*VY(I,1)+PX2(I,IPT)*VY(I,2)
     +       +PX3(I,IPT)*VY(I,3)+PX4(I,IPT)*VY(I,4)
     +       +PX5(I,IPT)*VY(I,5)+PX6(I,IPT)*VY(I,6)
     +       +PX7(I,IPT)*VY(I,7)+PX8(I,IPT)*VY(I,8)
      DYZ(I) =PZ1(I,IPT)*VY(I,1)+PZ2(I,IPT)*VY(I,2)
     +       +PZ3(I,IPT)*VY(I,3)+PZ4(I,IPT)*VY(I,4)
     +       +PZ5(I,IPT)*VY(I,5)+PZ6(I,IPT)*VY(I,6)
     +       +PZ7(I,IPT)*VY(I,7)+PZ8(I,IPT)*VY(I,8)
      DZX(I) =PX1(I,IPT)*VZ(I,1)+PX2(I,IPT)*VZ(I,2)
     +       +PX3(I,IPT)*VZ(I,3)+PX4(I,IPT)*VZ(I,4)
     +       +PX5(I,IPT)*VZ(I,5)+PX6(I,IPT)*VZ(I,6)
     +       +PX7(I,IPT)*VZ(I,7)+PX8(I,IPT)*VZ(I,8)
      DZY(I) =PY1(I,IPT)*VZ(I,1)+PY2(I,IPT)*VZ(I,2)
     +       +PY3(I,IPT)*VZ(I,3)+PY4(I,IPT)*VZ(I,4)
     +       +PY5(I,IPT)*VZ(I,5)+PY6(I,IPT)*VZ(I,6)
     +       +PY7(I,IPT)*VZ(I,7)+PY8(I,IPT)*VZ(I,8)
   30 CONTINUE
C
      DT1D2=HALF*DT1
C
      IF (JCVT/=0) THEN
       DO I=1,NEL
         D4(I,IPT)= DXY(I)+DYX(I)
     .           -DT1*(DXX(I)*DXY(I)+DYX(I)*DYY(I)+DZX(I)*DZY(I))
         D5(I,IPT)= DYZ(I)+DZY(I)
     .           -DT1*(DYY(I)*DYZ(I)+DZY(I)*DZZ(I)+DXY(I)*DXZ(I))
         D6(I,IPT)= DXZ(I)+DZX(I)
     .           -DT1*(DZZ(I)*DZX(I)+DXZ(I)*DXX(I)+DYZ(I)*DYX(I))
         D1(I,IPT)= DXX(I)  
     .           -DT1D2*(DXX(I)*DXX(I)+DYX(I)*DYX(I)+DZX(I)*DZX(I))
         D2(I,IPT)= DYY(I)
     .           -DT1D2*(DYY(I)*DYY(I)+DZY(I)*DZY(I)+DXY(I)*DXY(I))
         D3(I,IPT)= DZZ(I)
     .           -DT1D2*(DZZ(I)*DZZ(I)+DXZ(I)*DXZ(I)+DYZ(I)*DYZ(I))
        WXX(I)=ZERO
        WYY(I)=ZERO
        WZZ(I)=ZERO
       ENDDO
      ELSE
       DO I=1,NEL
        D1(I,IPT)=DXX(I)
        D2(I,IPT)=DYY(I)
        D3(I,IPT)=DZZ(I)
        D4(I,IPT)=DXY(I)+DYX(I)
        D5(I,IPT)=DYZ(I)+DZY(I)
        D6(I,IPT)=DXZ(I)+DZX(I)
        WZZ(I)=DT1D2*(DYX(I)-DXY(I))
        WYY(I)=DT1D2*(DXZ(I)-DZX(I))
        WXX(I)=DT1D2*(DZY(I)-DYZ(I))
       ENDDO
      ENDIF
      RETURN
C
      END
