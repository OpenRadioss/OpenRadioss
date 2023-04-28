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
Chd|  T6DERI3                       source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- called by -----------
Chd|        S6_TSTRAIN                    source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE T6DERI3(
     .   X1, X2, X3, X4, X5, X6, 
     .   Y1, Y2, Y3, Y4, Y5, Y6, 
     .   Z1, Z2, Z3, Z4, Z5, Z6, 
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   DET  ,NEL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), 
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*),   
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*) 
     
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),DET(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .   DETT ,  AJ1(NEL), AJ2(NEL) ,AJ3(NEL), 
     .   AJ4(NEL)  ,  AJ5(NEL), AJ6(NEL),
     .   AJ7(NEL)  ,  AJ8(NEL), AJ9(NEL),
     .   AJI1, AJI2, AJI3,
     .   AJI4, AJI5, AJI6,
     .   AJI7, AJI8, AJI9,
     .   JAC_59_68, JAC_67_49, JAC_48_57,
     .   AJ12, AJ45, AJ78,
     .   FAC
C-----------------------------------------------
C
      DO I=1,NEL
        AJ1(I)=X2(I)-X1(I)+X5(I)-X4(I)
        AJ2(I)=Y2(I)-Y1(I)+Y5(I)-Y4(I)
        AJ3(I)=Z2(I)-Z1(I)+Z5(I)-Z4(I)
        AJ4(I)=X3(I)-X1(I)+X6(I)-X4(I)
        AJ5(I)=Y3(I)-Y1(I)+Y6(I)-Y4(I)
        AJ6(I)=Z3(I)-Z1(I)+Z6(I)-Z4(I)
        AJ7(I)=THIRD*(X4(I)-X1(I)+X5(I)-X2(I)+X6(I)-X3(I))
        AJ8(I)=THIRD*(Y4(I)-Y1(I)+Y5(I)-Y2(I)+Y6(I)-Y3(I))
        AJ9(I)=THIRD*(Z4(I)-Z1(I)+Z5(I)-Z2(I)+Z6(I)-Z3(I))
      END DO 
C
      DO I=1,NEL
        JAC_59_68=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
        JAC_67_49=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
        JAC_48_57=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
        DET(I)=AJ1(I)*JAC_59_68+AJ2(I)*JAC_67_49+AJ3(I)*JAC_48_57
      END DO 
C      
      DO I=1,NEL
        DETT=1.0/DET(I)
C
C     INVERSE DE LA MATRICE JACOBIENNE
C
        AJI1=DETT*JAC_59_68
        AJI4=DETT*JAC_67_49
        AJI7=DETT*JAC_48_57
        AJI2=DETT*(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
        AJI5=DETT*(AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
        AJI8=DETT*(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
        AJI3=DETT*( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
        AJI6=DETT*(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
        AJI9=DETT*(AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
C
        AJ12=AJI1+AJI2
        AJ45=AJI4+AJI5
        AJ78=AJI7+AJI8
C
C ----------symtrie(a b c a b c)->P1-P3,anti-symtrie(-1 -1 -1 1 1 1)->P4
        PX1(I)=-AJ12
        PY1(I)=-AJ45
        PZ1(I)=-AJ78
        PX2(I)=AJI1
        PY2(I)=AJI4
        PZ2(I)=AJI7
        PX3(I)=AJI2
        PY3(I)=AJI5
        PZ3(I)=AJI8
        PX4(I)=THIRD*AJI3
        PY4(I)=THIRD*AJI6
        PZ4(I)=THIRD*AJI9
      END DO
C
      RETURN
      END
