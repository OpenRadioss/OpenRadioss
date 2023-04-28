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
Chd|  U_FROM_F3                     source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- called by -----------
Chd|        S6_TSTRAIN                    source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|        S8_TSTRAIN                    source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|        T4_TSTRAIN                    source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE U_FROM_F3(F,STRAIN,NEL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   F(3,3,NEL), STRAIN(6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      DOUBLE PRECISION 
     .         FMAT(3,3),UM(3,3),IC,I2C,I3C,IU,I2U,I3U,
     .         C11,C21,C31,C12,C22,C32,C13,C23,C33,DETJ0,DETM1,
     .         CC11,CC21,CC31,CC12,CC22,CC32,CC13,CC23,CC33,
     .         A,B,ZZ,A1,A2,A3,A4,X0(8),Y0(8),Z0(8)
C-----------------------------------------------
      DO I=1,NEL
        FMAT(1,1)= F(1,1,I)+1.0
        FMAT(2,2)= F(2,2,I)+1.0
        FMAT(3,3)= F(3,3,I)+1.0
        FMAT(1,2:3)= F(1,2:3,I)
        FMAT(2:3,1)= F(2:3,1,I)
        FMAT(2,3)= F(2,3,I)
        FMAT(3,2)= F(3,2,I)
        C11 = FMAT(1,1)*FMAT(1,1)+FMAT(2,1)*FMAT(2,1)+FMAT(3,1)*FMAT(3,1)
        C21 = FMAT(1,2)*FMAT(1,1)+FMAT(2,2)*FMAT(2,1)+FMAT(3,2)*FMAT(3,1)
        C31 = FMAT(1,3)*FMAT(1,1)+FMAT(2,3)*FMAT(2,1)+FMAT(3,3)*FMAT(3,1)
        C12 = C21
        C22 = FMAT(1,2)*FMAT(1,2)+FMAT(2,2)*FMAT(2,2)+FMAT(3,2)*FMAT(3,2)
        C32 = FMAT(1,3)*FMAT(1,2)+FMAT(2,3)*FMAT(2,2)+FMAT(3,3)*FMAT(3,2)
        C13 = C31
        C23 = C32
        C33 = FMAT(1,3)*FMAT(1,3)+FMAT(2,3)*FMAT(2,3)+FMAT(3,3)*FMAT(3,3)
        
        CC11 = C11*C11+C12*C21+C13*C31
        CC21 = C21*C11+C22*C21+C23*C31
        CC31 = C31*C11+C32*C21+C33*C31
        CC12 = CC21
        CC22 = C21*C12+C22*C22+C23*C32
        CC32 = C31*C12+C32*C22+C33*C32
        CC13 = CC31
        CC23 = CC32
        CC33 = C31*C13+C32*C23+C33*C33
        
        IC = C11+C22+C33
        I2C = C11*C22+C22*C33+C11*C33-C21*C12-C13*C31-C23*C32
        I3C = C11*C22*C33+C12*C23*C31+C13*C21*C32
     .      -(C13*C22*C31+C12*C21*C33+C11*C23*C32)
     	
        A = (2.*IC*IC*IC-9.*IC*I2C+27*I3C)*32./27.
        B = (4*(I2C*I2C*I2C+IC*IC*IC*I3C)-IC*IC*I2C*I2C-18*IC*I2C*I3C
     .      +27*I3C*I3C)*1024./27.
        B = MAX(B,ZERO)
        A1 = A+SQRT(B)
        A2 = A-SQRT(B)
        ZZ = -2*IC/3.+SIGN(ABS(A1)**(1/3),A1)+SIGN(ABS(A2)**(1/3),A2)
        A = 2*IC+ZZ
        IF (A == ZERO) THEN
          IU = SQRT(IC+2*SQRT(I2C))
        ELSE
          A = SQRT(A)
          IU = 0.5*(A+SQRT(2*IC-ZZ+16*SQRT(I3C)/A))
        ENDIF
        I3U = SQRT(I3C)
        I2U = 0.5*(IU*IU-IC)
        A = IU*I2U-I3U
        B = I3U+IU*IC
        A1 = IU*A
        A2 = A*B
        A3 = I2U*I3U*B+IU*IU*(I2U*I2C+I3C)
        A4 = ONE/(I3U*I3U*B+IU*IU*(IU*I3C+I3U*I2C))
        UM(1,1) = A4*(A1*CC11-A2*C11+A3)
        UM(2,1) = A4*(A1*CC21-A2*C21)
        UM(3,1) = A4*(A1*CC31-A2*C31)
        UM(1,2) = A4*(A1*CC12-A2*C12)
        UM(2,2) = A4*(A1*CC22-A2*C22+A3)
        UM(3,2) = A4*(A1*CC32-A2*C32)
        UM(1,3) = A4*(A1*CC13-A2*C13)
        UM(2,3) = A4*(A1*CC23-A2*C23)
        UM(3,3) = A4*(A1*CC33-A2*C33+A3)
        STRAIN(1,I) = UM(1,1)-1.0
        STRAIN(2,I) = UM(2,2)-1.0
        STRAIN(3,I) = UM(3,3)-1.0
        STRAIN(4,I) = 0.5*(UM(1,2)+UM(2,1))
        STRAIN(5,I) = 0.5*(UM(2,3)+UM(3,2))
        STRAIN(6,I) = 0.5*(UM(1,3)+UM(3,1))
       END DO
C
      RETURN
      END
