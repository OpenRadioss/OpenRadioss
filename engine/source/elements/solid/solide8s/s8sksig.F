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
Chd|  S8SKSIG                       source/elements/solid/solide8s/s8sksig.F
Chd|-- called by -----------
Chd|        S8SKE3                        source/elements/solid/solide8s/s8ske3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8SKSIG(X,IXS,NEL,QF,KS,V1,V2,V3,V4,V5,V6,V7,V8,
     .                   NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*),
     .        NC5(*), NC6(*), NC7(*), NC8(*),
     .        IXS(NIXS,*)
      
      DOUBLE PRECISION 
     .   QF(NEL,24),KS(24,24,NEL),
     .   V1(MVSIZ,3,3),V2(MVSIZ,3,3),V3(MVSIZ,3,3),V4(MVSIZ,3,3),
     .   V5(MVSIZ,3,3),V6(MVSIZ,3,3),V7(MVSIZ,3,3),V8(MVSIZ,3,3),
     .   X1(MVSIZ), Y1(MVSIZ), Z1(MVSIZ),
     .   X2(MVSIZ), Y2(MVSIZ), Z2(MVSIZ),
     .   X3(MVSIZ), Y3(MVSIZ), Z3(MVSIZ),
     .   X4(MVSIZ), Y4(MVSIZ), Z4(MVSIZ),
     .   X5(MVSIZ), Y5(MVSIZ), Z5(MVSIZ),
     .   X6(MVSIZ), Y6(MVSIZ), Z6(MVSIZ),
     .   X7(MVSIZ), Y7(MVSIZ), Z7(MVSIZ),
     .   X8(MVSIZ), Y8(MVSIZ), Z8(MVSIZ)
      my_real
     .   X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,INOD,J,J2,J3,J4,J5,J6,J7,J8,K 
      DOUBLE PRECISION
     .   Q1,Q2,Q3, A(3,3), B(3,24),
     .   QX1,QY1,QZ1,QX2,QY2,QZ2,QX3,QY3,QZ3,QX4,QY4,QZ4,
     .   QX5,QY5,QZ5,QX6,QY6,QZ6,QX7,QY7,QZ7,QX8,QY8,QZ8,
     .   XI1, YI1, ZI1, XI2, YI2, ZI2, XI3, YI3, ZI3,
     .   XI4, YI4, ZI4, XI5, YI5, ZI5, XI6, YI6, ZI6,
     .   XI7, YI7, ZI7, XI8, YI8, ZI8
C=======================================================================
            
      KS(1:24,1:24,1:NEL) = ZERO
      
      DO INOD=1,8
        II = 3*(INOD-1)
        DO J=1,3
          DO I=1,NEL
            Q1 = QF(I,II+1)
            Q2 = QF(I,II+2)
            Q3 = QF(I,II+3)
            KS(II+1,J,I)= Q3*V1(I,2,J)-Q2*V1(I,3,J)
            KS(II+2,J,I)=-Q3*V1(I,1,J)+Q1*V1(I,3,J)
            KS(II+3,J,I)= Q2*V1(I,1,J)-Q1*V1(I,2,J)
            J2 = J+3
            KS(II+1,J2,I)= Q3*V2(I,2,J)-Q2*V2(I,3,J)
            KS(II+2,J2,I)=-Q3*V2(I,1,J)+Q1*V2(I,3,J)
            KS(II+3,J2,I)= Q2*V2(I,1,J)-Q1*V2(I,2,J)
            J3 = J+6
            KS(II+1,J3,I)= Q3*V3(I,2,J)-Q2*V3(I,3,J)
            KS(II+2,J3,I)=-Q3*V3(I,1,J)+Q1*V3(I,3,J)
            KS(II+3,J3,I)= Q2*V3(I,1,J)-Q1*V3(I,2,J)
            J4 = J+9
            KS(II+1,J4,I)= Q3*V4(I,2,J)-Q2*V4(I,3,J)
            KS(II+2,J4,I)=-Q3*V4(I,1,J)+Q1*V4(I,3,J)
            KS(II+3,J4,I)= Q2*V4(I,1,J)-Q1*V4(I,2,J)
            J5 = J+12
            KS(II+1,J5,I)= Q3*V5(I,2,J)-Q2*V5(I,3,J)
            KS(II+2,J5,I)=-Q3*V5(I,1,J)+Q1*V5(I,3,J)
            KS(II+3,J5,I)= Q2*V5(I,1,J)-Q1*V5(I,2,J)
            J6 = J+15
            KS(II+1,J6,I)= Q3*V6(I,2,J)-Q2*V6(I,3,J)
            KS(II+2,J6,I)=-Q3*V6(I,1,J)+Q1*V6(I,3,J)
            KS(II+3,J6,I)= Q2*V6(I,1,J)-Q1*V6(I,2,J)
            J7 = J+18
            KS(II+1,J7,I)= Q3*V7(I,2,J)-Q2*V7(I,3,J)
            KS(II+2,J7,I)=-Q3*V7(I,1,J)+Q1*V7(I,3,J)
            KS(II+3,J7,I)= Q2*V7(I,1,J)-Q1*V7(I,2,J)
            J8 = J+21
            KS(II+1,J8,I)= Q3*V8(I,2,J)-Q2*V8(I,3,J)
            KS(II+2,J8,I)=-Q3*V8(I,1,J)+Q1*V8(I,3,J)
            KS(II+3,J8,I)= Q2*V8(I,1,J)-Q1*V8(I,2,J)
          ENDDO
        ENDDO
      ENDDO

      DO INOD=1,8
        II = 3*(INOD-1)
        DO J=1,3
          DO I=1,NEL
            Q1 = QF(I,II+1)
            Q2 = QF(I,II+2)
            Q3 = QF(I,II+3)
            KS(J,II+1,I)=KS(J,II+1,I)+Q3*V1(I,2,J)-Q2*V1(I,3,J)
            KS(J,II+2,I)=KS(J,II+2,I)-Q3*V1(I,1,J)+Q1*V1(I,3,J)
            KS(J,II+3,I)=KS(J,II+3,I)+Q2*V1(I,1,J)-Q1*V1(I,2,J)
            J2=J+3
            KS(J2,II+1,I)=KS(J2,II+1,I)+Q3*V2(I,2,J)-Q2*V2(I,3,J)
            KS(J2,II+2,I)=KS(J2,II+2,I)-Q3*V2(I,1,J)+Q1*V2(I,3,J)
            KS(J2,II+3,I)=KS(J2,II+3,I)+Q2*V2(I,1,J)-Q1*V2(I,2,J)
            J3=J+6
            KS(J3,II+1,I)=KS(J3,II+1,I)+Q3*V3(I,2,J)-Q2*V3(I,3,J)
            KS(J3,II+2,I)=KS(J3,II+2,I)-Q3*V3(I,1,J)+Q1*V3(I,3,J)
            KS(J3,II+3,I)=KS(J3,II+3,I)+Q2*V3(I,1,J)-Q1*V3(I,2,J)
            J4=J+9
            KS(J4,II+1,I)=KS(J4,II+1,I)+Q3*V4(I,2,J)-Q2*V4(I,3,J)
            KS(J4,II+2,I)=KS(J4,II+2,I)-Q3*V4(I,1,J)+Q1*V4(I,3,J)
            KS(J4,II+3,I)=KS(J4,II+3,I)+Q2*V4(I,1,J)-Q1*V4(I,2,J)
            J5=J+12
            KS(J5,II+1,I)=KS(J5,II+1,I)+Q3*V5(I,2,J)-Q2*V5(I,3,J)
            KS(J5,II+2,I)=KS(J5,II+2,I)-Q3*V5(I,1,J)+Q1*V5(I,3,J)
            KS(J5,II+3,I)=KS(J5,II+3,I)+Q2*V5(I,1,J)-Q1*V5(I,2,J)
            J6=J+15
            KS(J6,II+1,I)=KS(J6,II+1,I)+Q3*V6(I,2,J)-Q2*V6(I,3,J)
            KS(J6,II+2,I)=KS(J6,II+2,I)-Q3*V6(I,1,J)+Q1*V6(I,3,J)
            KS(J6,II+3,I)=KS(J6,II+3,I)+Q2*V6(I,1,J)-Q1*V6(I,2,J)
            J7=J+18
            KS(J7,II+1,I)=KS(J7,II+1,I)+Q3*V7(I,2,J)-Q2*V7(I,3,J)
            KS(J7,II+2,I)=KS(J7,II+2,I)-Q3*V7(I,1,J)+Q1*V7(I,3,J)
            KS(J7,II+3,I)=KS(J7,II+3,I)+Q2*V7(I,1,J)-Q1*V7(I,2,J)
            J8=J+21
            KS(J8,II+1,I)=KS(J8,II+1,I)+Q3*V8(I,2,J)-Q2*V8(I,3,J)
            KS(J8,II+2,I)=KS(J8,II+2,I)-Q3*V8(I,1,J)+Q1*V8(I,3,J)
            KS(J8,II+3,I)=KS(J8,II+3,I)+Q2*V8(I,1,J)-Q1*V8(I,2,J)
          ENDDO
        ENDDO
      ENDDO
      
      DO I=1,NEL
        NC1(I)=IXS(2,I)
        NC2(I)=IXS(3,I)
        NC3(I)=IXS(4,I)
        NC4(I)=IXS(5,I)
        NC5(I)=IXS(6,I)
        NC6(I)=IXS(7,I)
        NC7(I)=IXS(8,I)
        NC8(I)=IXS(9,I)
      ENDDO
      DO I=1,NEL
        X1(I)=X(1,NC1(I))
        Y1(I)=X(2,NC1(I))
        Z1(I)=X(3,NC1(I))
        X2(I)=X(1,NC2(I))
        Y2(I)=X(2,NC2(I))
        Z2(I)=X(3,NC2(I))
        X3(I)=X(1,NC3(I))
        Y3(I)=X(2,NC3(I))
        Z3(I)=X(3,NC3(I))
        X4(I)=X(1,NC4(I))
        Y4(I)=X(2,NC4(I))
        Z4(I)=X(3,NC4(I))
        X5(I)=X(1,NC5(I))
        Y5(I)=X(2,NC5(I))
        Z5(I)=X(3,NC5(I))
        X6(I)=X(1,NC6(I))
        Y6(I)=X(2,NC6(I))
        Z6(I)=X(3,NC6(I))
        X7(I)=X(1,NC7(I))
        Y7(I)=X(2,NC7(I))
        Z7(I)=X(3,NC7(I))
        X8(I)=X(1,NC8(I))
        Y8(I)=X(2,NC8(I))
        Z8(I)=X(3,NC8(I))
      ENDDO
      DO I=1,NEL
        XI1 = ZERO
        YI1 = ZERO
        ZI1 = ZERO
        XI2 = X2(I)-X1(I)
        YI2 = Y2(I)-Y1(I)
        ZI2 = Z2(I)-Z1(I)
        XI3 = X3(I)-X1(I)
        YI3 = Y3(I)-Y1(I)
        ZI3 = Z3(I)-Z1(I)
        XI4 = X4(I)-X1(I)
        YI4 = Y4(I)-Y1(I)
        ZI4 = Z4(I)-Z1(I)
        XI5 = X5(I)-X1(I)
        YI5 = Y5(I)-Y1(I)
        ZI5 = Z5(I)-Z1(I)
        XI6 = X6(I)-X1(I)
        YI6 = Y6(I)-Y1(I)
        ZI6 = Z6(I)-Z1(I)
        XI7 = X7(I)-X1(I)
        YI7 = Y7(I)-Y1(I)
        ZI7 = Z7(I)-Z1(I)
        XI8 = X8(I)-X1(I)
        YI8 = Y8(I)-Y1(I)
        ZI8 = Z8(I)-Z1(I)
        
        QX1 = QF(I,1)
        QY1 = QF(I,2)
        QZ1 = QF(I,3)
        QX2 = QF(I,4)
        QY2 = QF(I,5)
        QZ2 = QF(I,6)
        QX3 = QF(I,7)
        QY3 = QF(I,8)
        QZ3 = QF(I,9)
        QX4 = QF(I,10)
        QY4 = QF(I,11)
        QZ4 = QF(I,12)
        QX5 = QF(I,13)
        QY5 = QF(I,14)
        QZ5 = QF(I,15)
        QX6 = QF(I,16)
        QY6 = QF(I,17)
        QZ6 = QF(I,18)
        QX7 = QF(I,19)
        QY7 = QF(I,20)
        QZ7 = QF(I,21)
        QX8 = QF(I,22)
        QY8 = QF(I,23)
        QZ8 = QF(I,24)
        
        A(1,1) = -ZI1*QZ1+YI1*QY1-ZI2*QZ2+YI2*QY2
     .           -ZI3*QZ3+YI3*QY3-ZI4*QZ4+YI4*QY4
     .           -ZI5*QZ5+YI5*QY5-ZI6*QZ6+YI6*QY6
     .           -ZI7*QZ7+YI7*QY7-ZI8*QZ8+YI8*QY8
        A(2,1) = -XI1*QY1-XI2*QY2-XI3*QY3-XI4*QY4
     .           -XI5*QY5-XI6*QY6-XI7*QY7-XI8*QY8
        A(3,1) = +XI1*QZ1+XI2*QZ2+XI3*QZ3+XI4*QZ4
     .           +XI5*QZ5+XI6*QZ6+XI7*QZ7+XI8*QZ8
        A(1,2) =  YI1*QX1+YI2*QX2+YI3*QX3+YI4*QX4
     .           +YI5*QX5+YI6*QX6+YI7*QX7+YI8*QX8
        A(2,2) = -ZI1*QZ1-XI1*QX1-ZI2*QZ2-XI2*QX2
     .           -ZI3*QZ3-XI3*QX3-ZI4*QZ4-XI4*QX4
     .           -ZI5*QZ5-XI5*QX5-ZI6*QZ6-XI6*QX6
     .           -ZI7*QZ7-XI7*QX7-ZI8*QZ8-XI8*QX8
        A(3,2) =  YI1*QZ1+YI2*QZ2+YI3*QZ3+YI4*QZ4
     .           +YI5*QZ5+YI6*QZ6+YI7*QZ7+YI8*QZ8
        A(1,3) =  ZI1*QX1+ZI2*QX2+ZI3*QX3+ZI4*QX4
     .           +ZI5*QX5+ZI6*QX6+ZI7*QX7+ZI8*QX8
        A(2,3) =  ZI1*QY1+ZI2*QY2+ZI3*QY3+ZI4*QY4
     .           +ZI5*QY5+ZI6*QY6+ZI7*QY7+ZI8*QY8
        A(3,3) = -YI1*QY1-XI1*QX1-YI2*QY2-XI2*QX2
     .           -YI3*QY3-XI3*QX3-YI4*QY4-XI4*QX4
     .           -YI5*QY5-XI5*QX5-YI6*QY6-XI6*QX6
     .           -YI7*QY7-XI7*QX7-YI8*QY8-XI8*QX8
       
        A(1,2) = HALF*(A(1,2)+A(2,1))
        A(1,3) = HALF*(A(1,3)+A(3,1))
        A(2,3) = HALF*(A(2,3)+A(3,2))
        A(2,1) = A(1,2)
        A(3,1) = A(1,3)
        A(3,2) = A(2,3)
        
        DO J=1,3
          B(1,J) = A(1,1)*V1(I,1,J)+A(1,2)*V1(I,2,J)+A(1,3)*V1(I,3,J)
          B(2,J) = A(2,1)*V1(I,1,J)+A(2,2)*V1(I,2,J)+A(2,3)*V1(I,3,J)
          B(3,J) = A(3,1)*V1(I,1,J)+A(3,2)*V1(I,2,J)+A(3,3)*V1(I,3,J)
          J2 = J+3
          B(1,J2) = A(1,1)*V2(I,1,J)+A(1,2)*V2(I,2,J)+A(1,3)*V2(I,3,J)
          B(2,J2) = A(2,1)*V2(I,1,J)+A(2,2)*V2(I,2,J)+A(2,3)*V2(I,3,J)
          B(3,J2) = A(3,1)*V2(I,1,J)+A(3,2)*V2(I,2,J)+A(3,3)*V2(I,3,J)
          J3 = J+6
          B(1,J3) = A(1,1)*V3(I,1,J)+A(1,2)*V3(I,2,J)+A(1,3)*V3(I,3,J)
          B(2,J3) = A(2,1)*V3(I,1,J)+A(2,2)*V3(I,2,J)+A(2,3)*V3(I,3,J)
          B(3,J3) = A(3,1)*V3(I,1,J)+A(3,2)*V3(I,2,J)+A(3,3)*V3(I,3,J)
          J4 = J+9
          B(1,J4) = A(1,1)*V4(I,1,J)+A(1,2)*V4(I,2,J)+A(1,3)*V4(I,3,J)
          B(2,J4) = A(2,1)*V4(I,1,J)+A(2,2)*V4(I,2,J)+A(2,3)*V4(I,3,J)
          B(3,J4) = A(3,1)*V4(I,1,J)+A(3,2)*V4(I,2,J)+A(3,3)*V4(I,3,J)
          J5 = J+12
          B(1,J5) = A(1,1)*V5(I,1,J)+A(1,2)*V5(I,2,J)+A(1,3)*V5(I,3,J)
          B(2,J5) = A(2,1)*V5(I,1,J)+A(2,2)*V5(I,2,J)+A(2,3)*V5(I,3,J)
          B(3,J5) = A(3,1)*V5(I,1,J)+A(3,2)*V5(I,2,J)+A(3,3)*V5(I,3,J)
          J6 = J+15
          B(1,J6) = A(1,1)*V6(I,1,J)+A(1,2)*V6(I,2,J)+A(1,3)*V6(I,3,J)
          B(2,J6) = A(2,1)*V6(I,1,J)+A(2,2)*V6(I,2,J)+A(2,3)*V6(I,3,J)
          B(3,J6) = A(3,1)*V6(I,1,J)+A(3,2)*V6(I,2,J)+A(3,3)*V6(I,3,J)
          J7 = J+18
          B(1,J7) = A(1,1)*V7(I,1,J)+A(1,2)*V7(I,2,J)+A(1,3)*V7(I,3,J)
          B(2,J7) = A(2,1)*V7(I,1,J)+A(2,2)*V7(I,2,J)+A(2,3)*V7(I,3,J)
          B(3,J7) = A(3,1)*V7(I,1,J)+A(3,2)*V7(I,2,J)+A(3,3)*V7(I,3,J)
          J8 = J+21
          B(1,J8) = A(1,1)*V8(I,1,J)+A(1,2)*V8(I,2,J)+A(1,3)*V8(I,3,J)
          B(2,J8) = A(2,1)*V8(I,1,J)+A(2,2)*V8(I,2,J)+A(2,3)*V8(I,3,J)
          B(3,J8) = A(3,1)*V8(I,1,J)+A(3,2)*V8(I,2,J)+A(3,3)*V8(I,3,J)
        ENDDO
        
        DO K=1,24
          KS(1,K,I) = KS(1,K,I)+V1(I,1,1)*B(1,K)+V1(I,2,1)*B(2,K)+V1(I,3,1)*B(3,K)
          KS(2,K,I) = KS(2,K,I)+V1(I,1,2)*B(1,K)+V1(I,2,2)*B(2,K)+V1(I,3,2)*B(3,K)
          KS(3,K,I) = KS(3,K,I)+V1(I,1,3)*B(1,K)+V1(I,2,3)*B(2,K)+V1(I,3,3)*B(3,K)
          KS(4,K,I) = KS(4,K,I)+V2(I,1,1)*B(1,K)+V2(I,2,1)*B(2,K)+V2(I,3,1)*B(3,K)
          KS(5,K,I) = KS(5,K,I)+V2(I,1,2)*B(1,K)+V2(I,2,2)*B(2,K)+V2(I,3,2)*B(3,K)
          KS(6,K,I) = KS(6,K,I)+V2(I,1,3)*B(1,K)+V2(I,2,3)*B(2,K)+V2(I,3,3)*B(3,K)
          KS(7,K,I) = KS(7,K,I)+V3(I,1,1)*B(1,K)+V3(I,2,1)*B(2,K)+V3(I,3,1)*B(3,K)
          KS(8,K,I) = KS(8,K,I)+V3(I,1,2)*B(1,K)+V3(I,2,2)*B(2,K)+V3(I,3,2)*B(3,K)
          KS(9,K,I) = KS(9,K,I)+V3(I,1,3)*B(1,K)+V3(I,2,3)*B(2,K)+V3(I,3,3)*B(3,K)
          KS(10,K,I) = KS(10,K,I)+V4(I,1,1)*B(1,K)+V4(I,2,1)*B(2,K)+V4(I,3,1)*B(3,K)
          KS(11,K,I) = KS(11,K,I)+V4(I,1,2)*B(1,K)+V4(I,2,2)*B(2,K)+V4(I,3,2)*B(3,K)
          KS(12,K,I) = KS(12,K,I)+V4(I,1,3)*B(1,K)+V4(I,2,3)*B(2,K)+V4(I,3,3)*B(3,K)
          KS(13,K,I) = KS(13,K,I)+V5(I,1,1)*B(1,K)+V5(I,2,1)*B(2,K)+V5(I,3,1)*B(3,K)
          KS(14,K,I) = KS(14,K,I)+V5(I,1,2)*B(1,K)+V5(I,2,2)*B(2,K)+V5(I,3,2)*B(3,K)
          KS(15,K,I) = KS(15,K,I)+V5(I,1,3)*B(1,K)+V5(I,2,3)*B(2,K)+V5(I,3,3)*B(3,K)
          KS(16,K,I) = KS(16,K,I)+V6(I,1,1)*B(1,K)+V6(I,2,1)*B(2,K)+V6(I,3,1)*B(3,K)
          KS(17,K,I) = KS(17,K,I)+V6(I,1,2)*B(1,K)+V6(I,2,2)*B(2,K)+V6(I,3,2)*B(3,K)
          KS(18,K,I) = KS(18,K,I)+V6(I,1,3)*B(1,K)+V6(I,2,3)*B(2,K)+V6(I,3,3)*B(3,K)
          KS(19,K,I) = KS(19,K,I)+V7(I,1,1)*B(1,K)+V7(I,2,1)*B(2,K)+V7(I,3,1)*B(3,K)
          KS(20,K,I) = KS(20,K,I)+V7(I,1,2)*B(1,K)+V7(I,2,2)*B(2,K)+V7(I,3,2)*B(3,K)
          KS(21,K,I) = KS(21,K,I)+V7(I,1,3)*B(1,K)+V7(I,2,3)*B(2,K)+V7(I,3,3)*B(3,K)
          KS(22,K,I) = KS(22,K,I)+V8(I,1,1)*B(1,K)+V8(I,2,1)*B(2,K)+V8(I,3,1)*B(3,K)
          KS(23,K,I) = KS(23,K,I)+V8(I,1,2)*B(1,K)+V8(I,2,2)*B(2,K)+V8(I,3,2)*B(3,K)
          KS(24,K,I) = KS(24,K,I)+V8(I,1,3)*B(1,K)+V8(I,2,3)*B(2,K)+V8(I,3,3)*B(3,K)
        ENDDO
      ENDDO

      RETURN
      END
          
