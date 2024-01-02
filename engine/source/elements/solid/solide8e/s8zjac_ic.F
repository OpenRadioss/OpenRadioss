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
Chd|  S8ZJAC_IC                     source/elements/solid/solide8e/s8zjac_ic.F
Chd|-- called by -----------
Chd|        S8_IS17JAC_I                  source/elements/solid/solide8e/s8_is17jac_i.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE S8ZJAC_IC(
     .              XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .              YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .              ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   ,
     .              AJ1  ,AJ2  ,AJ3  ,
     .              AJ4  ,AJ5  ,AJ6  ,
     .              AJ7  ,AJ8  ,AJ9  ,
     .              HX, HY, HZ,
     .              PXC1, PXC2, PXC3, PXC4, 
     .              PYC1, PYC2, PYC3, PYC4, 
     .              PZC1, PZC2, PZC3, PZC4,
     .              JAC_I,INDEX,NEL )
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
      INTEGER NEL,INDEX(*)
      my_real
     .   HX(4,*), HY(4,*), HZ(4,*),  
     .   AJ1(*),AJ2(*),AJ3(*),
     .   AJ4(*),AJ5(*),AJ6(*),
     .   AJ7(*),AJ8(*),AJ9(*),JAC_I(10,*),
     .   PXC1(*), PXC2(*), PXC3(*), PXC4(*), 
     .   PYC1(*), PYC2(*), PYC3(*), PYC4(*), 
     .   PZC1(*), PZC2(*), PZC3(*), PZC4(*) 
      DOUBLE PRECISION
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ), 
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ), 
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J
      
      my_real
     .   DET(MVSIZ) ,DETT(MVSIZ)
C                                                                     12
      my_real
     .   X17(MVSIZ) , X28(MVSIZ) , X35(MVSIZ) , X46(MVSIZ),
     .   Y17(MVSIZ) , Y28(MVSIZ) , Y35(MVSIZ) , Y46(MVSIZ),
     .   Z17(MVSIZ) , Z28(MVSIZ) , Z35(MVSIZ) , Z46(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .   AJI1, AJI2, AJI3,AJI4, AJI5, AJI6,AJI7, AJI8, AJI9,
     .   AJ12, AJ45, AJ78,AJ12P, AJ45P, AJ78P,
     .   A17(MVSIZ) , A28(MVSIZ) ,
     .   B17(MVSIZ) , B28(MVSIZ) ,
     .   C17(MVSIZ) , C28(MVSIZ) ,JAC_1(10,MVSIZ)
C-----------------------------------------------
      DO I=1,NEL
      X17(I)=XD7(I)-XD1(I)
      X28(I)=XD8(I)-XD2(I)
      X35(I)=XD5(I)-XD3(I)
      X46(I)=XD6(I)-XD4(I)
      Y17(I)=YD7(I)-YD1(I)
      Y28(I)=YD8(I)-YD2(I)
      Y35(I)=YD5(I)-YD3(I)
      Y46(I)=YD6(I)-YD4(I)
      Z17(I)=ZD7(I)-ZD1(I)
      Z28(I)=ZD8(I)-ZD2(I)
      Z35(I)=ZD5(I)-ZD3(I)
      Z46(I)=ZD6(I)-ZD4(I)
      END DO
C
      DO I=1,NEL
      AJ4(I)=X17(I)+X28(I)-X35(I)-X46(I)
      AJ5(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
      AJ6(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
      A17(I)=X17(I)+X46(I)
      A28(I)=X28(I)+X35(I)
      B17(I)=Y17(I)+Y46(I)
      B28(I)=Y28(I)+Y35(I)
      C17(I)=Z17(I)+Z46(I)
      C28(I)=Z28(I)+Z35(I)
C
      AJ7(I)=A17(I)+A28(I)
      AJ8(I)=B17(I)+B28(I)
      AJ9(I)=C17(I)+C28(I)
      AJ1(I)=A17(I)-A28(I)
      AJ2(I)=B17(I)-B28(I)
      AJ3(I)=C17(I)-C28(I)
      END DO
C
C     JACOBIAN
C
      DO I=1,NEL
      JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
      JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
      JAC_38_29(I)=(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
      JAC_19_37(I)=( AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
      JAC_27_18(I)=(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
      JAC_26_35(I)=( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
      JAC_34_16(I)=(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
      JAC_15_24(I)=( AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
      JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
C
      DET(I)=ONE_OVER_64*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
      DETT(I)=ONE_OVER_64/DET(I)
      ENDDO
C
       DO I=1,NEL
         AJI1=DETT(I)*JAC_59_68(I)
         AJI4=DETT(I)*JAC_67_49(I)
         AJI7=DETT(I)*JAC_48_57(I)
         AJI2=DETT(I)*JAC_38_29(I)
         AJI5=DETT(I)*JAC_19_37(I)
         AJI8=DETT(I)*JAC_27_18(I)
         AJI3=DETT(I)*JAC_26_35(I)
         AJI6=DETT(I)*JAC_34_16(I)
         AJI9=DETT(I)*JAC_15_24(I)
         AJ12=AJI1-AJI2
         AJ45=AJI4-AJI5
         AJ78=AJI7-AJI8
         PXC2(I)= AJ12-AJI3
         PYC2(I)= AJ45-AJI6
         PZC2(I)= AJ78-AJI9
         PXC4(I)=-AJ12-AJI3
         PYC4(I)=-AJ45-AJI6
         PZC4(I)=-AJ78-AJI9
         AJ12P=AJI1+AJI2
         AJ45P=AJI4+AJI5
         AJ78P=AJI7+AJI8
         PXC1(I)=-AJ12P-AJI3
         PYC1(I)=-AJ45P-AJI6
         PZC1(I)=-AJ78P-AJI9
         PXC3(I)=AJ12P-AJI3
         PYC3(I)=AJ45P-AJI6
         PZC3(I)=AJ78P-AJI9
         JAC_1(1,I)=AJI1
         JAC_1(4,I)=AJI4
         JAC_1(7,I)=AJI7
         JAC_1(2,I)=AJI2
         JAC_1(5,I)=AJI5
         JAC_1(8,I)=AJI8
         JAC_1(3,I)=AJI3
         JAC_1(6,I)=AJI6
         JAC_1(9,I)=AJI9
         JAC_1(10,I)=DET(I)
       ENDDO
C mode 1
C 1 1 -1 -1 -1 -1 1 1
       DO I=1,NEL
         HX(1,I)=(XD1(I)+XD2(I)-XD3(I)-XD4(I)-XD5(I)-XD6(I)+XD7(I)+XD8(I))
         HY(1,I)=(YD1(I)+YD2(I)-YD3(I)-YD4(I)-YD5(I)-YD6(I)+YD7(I)+YD8(I))
         HZ(1,I)=(ZD1(I)+ZD2(I)-ZD3(I)-ZD4(I)-ZD5(I)-ZD6(I)+ZD7(I)+ZD8(I))
       ENDDO
C mode 2
C 1 -1 -1 1 -1 1 1 -1
       DO I=1,NEL
         HX(2,I)=(XD1(I)-XD2(I)-XD3(I)+XD4(I)-XD5(I)+XD6(I)+XD7(I)-XD8(I))
         HY(2,I)=(YD1(I)-YD2(I)-YD3(I)+YD4(I)-YD5(I)+YD6(I)+YD7(I)-YD8(I))
         HZ(2,I)=(ZD1(I)-ZD2(I)-ZD3(I)+ZD4(I)-ZD5(I)+ZD6(I)+ZD7(I)-ZD8(I))
       ENDDO
C mode 3
C 1 -1 1 -1 1 -1 1 -1
       DO I=1,NEL
         HX(3,I)=(XD1(I)-XD2(I)+XD3(I)-XD4(I)+XD5(I)-XD6(I)+XD7(I)-XD8(I))
         HY(3,I)=(YD1(I)-YD2(I)+YD3(I)-YD4(I)+YD5(I)-YD6(I)+YD7(I)-YD8(I))
         HZ(3,I)=(ZD1(I)-ZD2(I)+ZD3(I)-ZD4(I)+ZD5(I)-ZD6(I)+ZD7(I)-ZD8(I))
       ENDDO
C mode 4
C -1 1 -1 1 1 -1 1 -1
       DO I=1,NEL
         HX(4,I)=(-XD1(I)+XD2(I)-XD3(I)+XD4(I)+XD5(I)-XD6(I)+XD7(I)-XD8(I))
         HY(4,I)=(-YD1(I)+YD2(I)-YD3(I)+YD4(I)+YD5(I)-YD6(I)+YD7(I)-YD8(I))
         HZ(4,I)=(-ZD1(I)+ZD2(I)-ZD3(I)+ZD4(I)+ZD5(I)-ZD6(I)+ZD7(I)-ZD8(I))
       ENDDO
#include "vectorize.inc"
        DO J=1,NEL
          I = INDEX(J)
          JAC_I(1:10,I) = JAC_1(1:10,J)
        ENDDO      
C       
      RETURN
C
      END
