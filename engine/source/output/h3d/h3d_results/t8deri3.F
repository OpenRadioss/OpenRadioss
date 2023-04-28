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
Chd|  T8DERI3                       source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- called by -----------
Chd|        S8_TSTRAIN                    source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE T8DERI3(
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
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
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)
     
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
     .   AJ1(NEL),AJ2(NEL),AJ3(NEL),
     .   AJ4(NEL),AJ5(NEL),AJ6(NEL),
     .   DETT ,  AJ7(NEL), AJ8(NEL) , AJ9(NEL),
     .   AJI1, AJI2, AJI3,
     .   AJI4, AJI5, AJI6,
     .   AJI7, AJI8, AJI9,
     .   JAC_59_68, JAC_67_49, JAC_48_57,
     .   AJ12, AJ45, AJ78,
     .   A17 , A28 ,
     .   B17 , B28 ,
     .   C17 , C28 ,
     .   X17 , X28 , X35 , X46,
     .   Y17 , Y28 , Y35 , Y46,
     .   Z17 , Z28 , Z35 , Z46
C-----------------------------------------------
C
      DO I=1,NEL
        X17=X7(I)-X1(I)
        X28=X8(I)-X2(I)
        X35=X5(I)-X3(I)
        X46=X6(I)-X4(I)
        Y17=Y7(I)-Y1(I)
        Y28=Y8(I)-Y2(I)
        Y35=Y5(I)-Y3(I)
        Y46=Y6(I)-Y4(I)
        Z17=Z7(I)-Z1(I)
        Z28=Z8(I)-Z2(I)
        Z35=Z5(I)-Z3(I)
        Z46=Z6(I)-Z4(I)
c             
        AJ1(I)=X17+X28-X35-X46
        AJ2(I)=Y17+Y28-Y35-Y46
        AJ3(I)=Z17+Z28-Z35-Z46
        A17=X17+X46
        A28=X28+X35
        B17=Y17+Y46
        B28=Y28+Y35
        C17=Z17+Z46
        C28=Z28+Z35

        AJ4(I)=A17+A28
        AJ5(I)=B17+B28
        AJ6(I)=C17+C28
        AJ7(I)=A17-A28
        AJ8(I)=B17-B28
        AJ9(I)=C17-C28
      ENDDO
C                  
C     JACOBIAN
C
      DO I=1,NEL      
        JAC_59_68=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
        JAC_67_49=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
        JAC_48_57=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
        DET(I)=AJ1(I)*JAC_59_68+AJ2(I)*JAC_67_49+AJ3(I)*JAC_48_57
C        DET(I)=ONE_OVER_64*(AJ1(I)*JAC_59_68+AJ2(I)*JAC_67_49+AJ3(I)*JAC_48_57)
        DETT=1.0/DET(I)
C       INVERSE DE LA MATRICE JACOBIENNE
        AJI1=DETT*JAC_59_68
        AJI4=DETT*JAC_67_49
        AJI7=DETT*JAC_48_57
        AJI2=DETT*(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
        AJI5=DETT*( AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
        AJI8=DETT*(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
        AJI3=DETT*( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
        AJI6=DETT*(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
        AJI9=DETT*( AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
C
        AJ12=AJI1-AJI2
        AJ45=AJI4-AJI5
        AJ78=AJI7-AJI8
        PX3(I)= AJ12+AJI3
        PY3(I)= AJ45+AJI6
        PZ3(I)= AJ78+AJI9
        PX4(I)= AJ12-AJI3
        PY4(I)= AJ45-AJI6
        PZ4(I)= AJ78-AJI9
!
        AJ12=AJI1+AJI2
        AJ45=AJI4+AJI5
        AJ78=AJI7+AJI8
!
        PX1(I)=-AJ12-AJI3
        PY1(I)=-AJ45-AJI6
        PZ1(I)=-AJ78-AJI9
        PX2(I)=-AJ12+AJI3
        PY2(I)=-AJ45+AJI6
        PZ2(I)=-AJ78+AJI9
      ENDDO
C
      RETURN
      END
