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
Chd|  CRFRAME_IMP                   source/elements/solid/solide8s/crframe_imp.F
Chd|-- called by -----------
Chd|        SRCOOR3_IMP                   source/elements/solid/solide8s/srcoor3_imp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CRFRAME_IMP(
     1   SAV,     INVJ,    XD1,     XD2,
     2   XD3,     XD4,     XD5,     XD6,
     3   XD7,     XD8,     YD1,     YD2,
     4   YD3,     YD4,     YD5,     YD6,
     5   YD7,     YD8,     ZD1,     ZD2,
     6   ZD3,     ZD4,     ZD5,     ZD6,
     7   ZD7,     ZD8,     R,       NEL)
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
C     REAL
!      my_real
!     .   OFFG(*)
      
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ),
     .   SAV(NEL,21),INVJ(MVSIZ,3,3), R(3,3,MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J      
C=======================================================================
      DOUBLE PRECISION DX_DR,DX_DS,DX_DT,DY_DR,DY_DS,DY_DT,DZ_DR,DZ_DS,DZ_DT
      DOUBLE PRECISION FMAT(3,3),UM(3,3),IC,I2C,I3C,IU,I2U,I3U
      DOUBLE PRECISION C11,C21,C31,C12,C22,C32,C13,C23,C33,DETJ0,DETM1
      DOUBLE PRECISION CC11,CC21,CC31,CC12,CC22,CC32,CC13,CC23,CC33
      DOUBLE PRECISION A,B,ZZ,A1,A2,A3,A4,X0(8),Y0(8),Z0(8)
      DOUBLE PRECISION MILLE24
      MILLE24 = 1024.0
      
      DO I=1,NEL
        X0(1) = ZERO
        Y0(1) = ZERO
        Z0(1) = ZERO
        X0(2) = SAV(I,1)
        Y0(2) = SAV(I,2)
        Z0(2) = SAV(I,3)
        X0(3) = SAV(I,4)
        Y0(3) = SAV(I,5)
        Z0(3) = SAV(I,6)
        X0(4) = SAV(I,7)
        Y0(4) = SAV(I,8)
        Z0(4) = SAV(I,9)
        X0(5) = SAV(I,10)
        Y0(5) = SAV(I,11)
        Z0(5) = SAV(I,12)
        X0(6) = SAV(I,13)
        Y0(6) = SAV(I,14)
        Z0(6) = SAV(I,15)
        X0(7) = SAV(I,16)
        Y0(7) = SAV(I,17)
        Z0(7) = SAV(I,18)
        X0(8) = SAV(I,19)
        Y0(8) = SAV(I,20)
        Z0(8) = SAV(I,21)

        DX_DR = (X0(3)+X0(4)+X0(7)+X0(8))-(X0(1)+X0(2)+X0(5)+X0(6))
        DY_DR = (Y0(3)+Y0(4)+Y0(7)+Y0(8))-(Y0(1)+Y0(2)+Y0(5)+Y0(6))
        DZ_DR = (Z0(3)+Z0(4)+Z0(7)+Z0(8))-(Z0(1)+Z0(2)+Z0(5)+Z0(6))
        !DX_DR = DX_DR/EIGHT
        !DY_DR = DY_DR/EIGHT
        !DZ_DR = DZ_DR/EIGHT
        DX_DS = (X0(5)+X0(6)+X0(7)+X0(8))-(X0(1)+X0(2)+X0(3)+X0(4))
        DY_DS = (Y0(5)+Y0(6)+Y0(7)+Y0(8))-(Y0(1)+Y0(2)+Y0(3)+Y0(4))
        DZ_DS = (Z0(5)+Z0(6)+Z0(7)+Z0(8))-(Z0(1)+Z0(2)+Z0(3)+Z0(4))
        !DX_DS = DX_DS/EIGHT
        !DY_DS = DY_DS/EIGHT
        !DZ_DS = DZ_DS/EIGHT
        DX_DT = (X0(2)+X0(3)+X0(6)+X0(7))-(X0(1)+X0(4)+X0(5)+X0(8))
        DY_DT = (Y0(2)+Y0(3)+Y0(6)+Y0(7))-(Y0(1)+Y0(4)+Y0(5)+Y0(8))
        DZ_DT = (Z0(2)+Z0(3)+Z0(6)+Z0(7))-(Z0(1)+Z0(4)+Z0(5)+Z0(8))
        !DX_DT = DX_DT/EIGHT
        !DY_DT = DY_DT/EIGHT
        !DZ_DT = DZ_DT/EIGHT
        DETJ0 =(DX_DR*(DY_DS*DZ_DT-DZ_DS*DY_DT)
     .         -DX_DS*(DY_DR*DZ_DT-DY_DT*DZ_DR)
     .         +DX_DT*(DY_DR*DZ_DS-DY_DS*DZ_DR))
        DETM1 = ONE/DETJ0
        DETM1 = EIGHT*DETM1
        INVJ(I,1,1) = (DY_DS*DZ_DT-DZ_DS*DY_DT)*DETM1
        INVJ(I,2,1) = (DZ_DR*DY_DT-DY_DR*DZ_DT)*DETM1
        INVJ(I,3,1) = (DY_DR*DZ_DS-DY_DS*DZ_DR)*DETM1
        INVJ(I,1,2) = (DX_DT*DZ_DS-DX_DS*DZ_DT)*DETM1
        INVJ(I,2,2) = (DX_DR*DZ_DT-DX_DT*DZ_DR)*DETM1
        INVJ(I,3,2) = (DX_DS*DZ_DR-DX_DR*DZ_DS)*DETM1
        INVJ(I,1,3) = (DX_DS*DY_DT-DX_DT*DY_DS)*DETM1
        INVJ(I,2,3) = (DX_DT*DY_DR-DX_DR*DY_DT)*DETM1
        INVJ(I,3,3) = (DX_DR*DY_DS-DX_DS*DY_DR)*DETM1
      
        DX_DR = (XD3(I)+XD4(I)+XD7(I)+XD8(I))-(XD1(I)+XD2(I)+XD5(I)+XD6(I))
        DY_DR = (YD3(I)+YD4(I)+YD7(I)+YD8(I))-(YD1(I)+YD2(I)+YD5(I)+YD6(I))
        DZ_DR = (ZD3(I)+ZD4(I)+ZD7(I)+ZD8(I))-(ZD1(I)+ZD2(I)+ZD5(I)+ZD6(I))
        DX_DR = DX_DR/EIGHT
        DY_DR = DY_DR/EIGHT
        DZ_DR = DZ_DR/EIGHT
        DX_DS = (XD5(I)+XD6(I)+XD7(I)+XD8(I))-(XD1(I)+XD2(I)+XD3(I)+XD4(I))
        DY_DS = (YD5(I)+YD6(I)+YD7(I)+YD8(I))-(YD1(I)+YD2(I)+YD3(I)+YD4(I))
        DZ_DS = (ZD5(I)+ZD6(I)+ZD7(I)+ZD8(I))-(ZD1(I)+ZD2(I)+ZD3(I)+ZD4(I))
        DX_DS = DX_DS/EIGHT
        DY_DS = DY_DS/EIGHT
        DZ_DS = DZ_DS/EIGHT
        DX_DT = (XD2(I)+XD3(I)+XD6(I)+XD7(I))-(XD1(I)+XD4(I)+XD5(I)+XD8(I))
        DY_DT = (YD2(I)+YD3(I)+YD6(I)+YD7(I))-(YD1(I)+YD4(I)+YD5(I)+YD8(I))
        DZ_DT = (ZD2(I)+ZD3(I)+ZD6(I)+ZD7(I))-(ZD1(I)+ZD4(I)+ZD5(I)+ZD8(I))
        DX_DT = DX_DT/EIGHT
        DY_DT = DY_DT/EIGHT
        DZ_DT = DZ_DT/EIGHT
        
        FMAT(1,1) = (DX_DR*INVJ(I,1,1)+DX_DS*INVJ(I,2,1)+DX_DT*INVJ(I,3,1))
        FMAT(2,1) = (DY_DR*INVJ(I,1,1)+DY_DS*INVJ(I,2,1)+DY_DT*INVJ(I,3,1))
        FMAT(3,1) = (DZ_DR*INVJ(I,1,1)+DZ_DS*INVJ(I,2,1)+DZ_DT*INVJ(I,3,1))
        FMAT(1,2) = (DX_DR*INVJ(I,1,2)+DX_DS*INVJ(I,2,2)+DX_DT*INVJ(I,3,2))
        FMAT(2,2) = (DY_DR*INVJ(I,1,2)+DY_DS*INVJ(I,2,2)+DY_DT*INVJ(I,3,2))
        FMAT(3,2) = (DZ_DR*INVJ(I,1,2)+DZ_DS*INVJ(I,2,2)+DZ_DT*INVJ(I,3,2))
        FMAT(1,3) = (DX_DR*INVJ(I,1,3)+DX_DS*INVJ(I,2,3)+DX_DT*INVJ(I,3,3))
        FMAT(2,3) = (DY_DR*INVJ(I,1,3)+DY_DS*INVJ(I,2,3)+DY_DT*INVJ(I,3,3))
        FMAT(3,3) = (DZ_DR*INVJ(I,1,3)+DZ_DS*INVJ(I,2,3)+DZ_DT*INVJ(I,3,3))
        
        C11 = FMAT(1,1)*FMAT(1,1)+FMAT(2,1)*FMAT(2,1)+FMAT(3,1)*FMAT(3,1)
        C21 = FMAT(1,2)*FMAT(1,1)+FMAT(2,2)*FMAT(2,1)+FMAT(3,2)*FMAT(3,1)
        C31 = FMAT(1,3)*FMAT(1,1)+FMAT(2,3)*FMAT(2,1)+FMAT(3,3)*FMAT(3,1)
        C12 = FMAT(1,1)*FMAT(1,2)+FMAT(2,1)*FMAT(2,2)+FMAT(3,1)*FMAT(3,2)
        C22 = FMAT(1,2)*FMAT(1,2)+FMAT(2,2)*FMAT(2,2)+FMAT(3,2)*FMAT(3,2)
        C32 = FMAT(1,3)*FMAT(1,2)+FMAT(2,3)*FMAT(2,2)+FMAT(3,3)*FMAT(3,2)
        C13 = FMAT(1,1)*FMAT(1,3)+FMAT(2,1)*FMAT(2,3)+FMAT(3,1)*FMAT(3,3)
        C23 = FMAT(1,2)*FMAT(1,3)+FMAT(2,2)*FMAT(2,3)+FMAT(3,2)*FMAT(3,3)
        C33 = FMAT(1,3)*FMAT(1,3)+FMAT(2,3)*FMAT(2,3)+FMAT(3,3)*FMAT(3,3)
        
        CC11 = C11*C11+C12*C21+C13*C31
        CC21 = C21*C11+C22*C21+C23*C31
        CC31 = C31*C11+C32*C21+C33*C31
        CC12 = C11*C12+C12*C22+C13*C32
        CC22 = C21*C12+C22*C22+C23*C32
        CC32 = C31*C12+C32*C22+C33*C32
        CC13 = C11*C13+C12*C23+C13*C33
        CC23 = C21*C13+C22*C23+C23*C33
        CC33 = C31*C13+C32*C23+C33*C33
        
        IC = C11+C22+C33
        I2C = C11*C22+C22*C33+C11*C33-C21*C12-C13*C31-C23*C32
        I3C = C11*C22*C33+C12*C23*C31+C13*C21*C32
     .      -(C13*C22*C31+C12*C21*C33+C11*C23*C32)
     
        A = (TWO*IC*IC*IC-NINE*IC*I2C+TWENTY7*I3C)*THIRTY2/TWENTY7
            B = (FOUR*(I2C*I2C*I2C+IC*IC*IC*I3C)-IC*IC*I2C*I2C-EIGHTEEN*IC*I2C*I3C
     .      +TWENTY7*I3C*I3C)*MILLE24/TWENTY7
        B = MAX(B,ZERO)
        A1 = A+SQRT(B)
        A2 = A-SQRT(B)
        !ZZ = -TWO_THIRD*IC+CBRT(A1)+CBRT(A2)
        ZZ = -TWO_THIRD*IC+SIGN(ABS(A1)**THIRD,A1)+SIGN(ABS(A2)**THIRD,A2)
        A = TWO*IC+ZZ
        IF (A == ZERO) THEN
          IU = SQRT(IC+TWO*SQRT(I2C))
        ELSE
          A = SQRT(A)
          IU = HALF*(A+SQRT(TWO*IC-ZZ+SIXTEEN*SQRT(I3C)/A))
        ENDIF
        I3U = SQRT(I3C)
        I2U = HALF*(IU*IU-IC)
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
        
        R(1,1,I) = FMAT(1,1)*UM(1,1)+FMAT(1,2)*UM(2,1)+FMAT(1,3)*UM(3,1)
        R(2,1,I) = FMAT(2,1)*UM(1,1)+FMAT(2,2)*UM(2,1)+FMAT(2,3)*UM(3,1)
        R(3,1,I) = FMAT(3,1)*UM(1,1)+FMAT(3,2)*UM(2,1)+FMAT(3,3)*UM(3,1)
        R(1,2,I) = FMAT(1,1)*UM(1,2)+FMAT(1,2)*UM(2,2)+FMAT(1,3)*UM(3,2)
        R(2,2,I) = FMAT(2,1)*UM(1,2)+FMAT(2,2)*UM(2,2)+FMAT(2,3)*UM(3,2)
        R(3,2,I) = FMAT(3,1)*UM(1,2)+FMAT(3,2)*UM(2,2)+FMAT(3,3)*UM(3,2)
        R(1,3,I) = FMAT(1,1)*UM(1,3)+FMAT(1,2)*UM(2,3)+FMAT(1,3)*UM(3,3)
        R(2,3,I) = FMAT(2,1)*UM(1,3)+FMAT(2,2)*UM(2,3)+FMAT(2,3)*UM(3,3)
        R(3,3,I) = FMAT(3,1)*UM(1,3)+FMAT(3,2)*UM(2,3)+FMAT(3,3)*UM(3,3)
      ENDDO
      RETURN
      END
