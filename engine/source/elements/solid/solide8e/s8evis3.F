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
Chd|  S8EVIS3                       source/elements/solid/solide8e/s8eforc3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EVIS3(
     1   VX1,     VX2,     VX3,     VX4,
     2   VX5,     VX6,     VX7,     VX8,
     3   VY1,     VY2,     VY3,     VY4,
     4   VY5,     VY6,     VY7,     VY8,
     5   VZ1,     VZ2,     VZ3,     VZ4,
     6   VZ5,     VZ6,     VZ7,     VZ8,
     7   F11,     F21,     F31,     F12,
     8   F22,     F32,     F13,     F23,
     9   F33,     F14,     F24,     F34,
     A   F15,     F25,     F35,     F16,
     B   F26,     F36,     F17,     F27,
     C   F37,     F18,     F28,     F38,
     D   PX1H1,   PX1H2,   PX1H3,   PX1H4,
     E   PX2H1,   PX2H2,   PX2H3,   PX2H4,
     F   PX3H1,   PX3H2,   PX3H3,   PX3H4,
     G   PX4H1,   PX4H2,   PX4H3,   PX4H4,
     H   JR_1,    JS_1,    JT_1,    NU,
     I   FCL,     NEL)
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
      INTEGER, INTENT(IN) :: NEL
      my_real
     .   NU,
     .   VX1(*),VX2(*),VX3(*),VX4(*),VX5(*),VX6(*),VX7(*),VX8(*),
     .   VY1(*),VY2(*),VY3(*),VY4(*),VY5(*),VY6(*),VY7(*),VY8(*),
     .   VZ1(*),VZ2(*),VZ3(*),VZ4(*),VZ5(*),VZ6(*),VZ7(*),VZ8(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*),
     .   PX1H1(*), PX1H2(*), PX1H3(*), PX1H4(*),  
     .   PX2H1(*), PX2H2(*), PX2H3(*), PX2H4(*),  
     .   PX3H1(*), PX3H2(*), PX3H3(*), PX3H4(*),  
     .   PX4H1(*), PX4H2(*), PX4H3(*), PX4H4(*),
     .   JR_1(MVSIZ),JS_1(MVSIZ),JT_1(MVSIZ),FCL(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, J,K,IET, MT
      my_real
     .   H11(MVSIZ), H22(MVSIZ), H33(MVSIZ),
     .   H12(MVSIZ), H13(MVSIZ), H23(MVSIZ),
     .   HGX1(MVSIZ), HGX2(MVSIZ), HGX3(MVSIZ), HGX4(MVSIZ),
     .   HGY1(MVSIZ), HGY2(MVSIZ), HGY3(MVSIZ), HGY4(MVSIZ),
     .   HGZ1(MVSIZ), HGZ2(MVSIZ), HGZ3(MVSIZ), HGZ4(MVSIZ),
     .   VX3478, VX2358, VX1467, VX1256, 
     .   VY3478, VY2358, VY1467, VY1256,
     .   VZ3478, VZ2358, VZ1467, VZ1256,
     .   VX17, VY17, VZ17,
     .   VX28, VY28, VZ28,
     .   VX35, VY35, VZ35,
     .   VX46, VY46, VZ46, 
     .   JR0(MVSIZ),JS0(MVSIZ),JT0(MVSIZ),NFHOUR(MVSIZ,3,4),
     .   FHOURT(3,4),DT05,RHO0,ETMAX,NU1,NU2,NU3,NU4,DEINT,
     .   E_R,E_S,E_T,FAC,FAC1,FAC2,COEFH,HQ13P,HQ13N,HQ24P,HQ24N,FF
C-----------------------------------------------
C
        NU1 =TWO/(ONE-NU)
        NU2 =NU*NU1
        NU3 =TWO_THIRD*(ONE + NU)
        NU4 =NU
      DO I=1,NEL
       JR0(I) = ONE/MAX(EM20,JR_1(I))
       JS0(I) = ONE/MAX(EM20,JS_1(I))
       JT0(I) = ONE/MAX(EM20,JT_1(I))
       H11(I) = JS0(I)*JT0(I)*JR_1(I)
       H22(I) = JR0(I)*JT0(I)*JS_1(I)
       H33(I) = JR0(I)*JS0(I)*JT_1(I)
       H12(I) = JT0(I)
       H13(I) = JS0(I)
       H23(I) = JR0(I)
      ENDDO
      DO I=1,NEL
       VX3478=VX3(I)-VX4(I)-VX7(I)+VX8(I)
       VX2358=VX2(I)-VX3(I)-VX5(I)+VX8(I)
       VX1467=VX1(I)-VX4(I)-VX6(I)+VX7(I)
       VX1256=VX1(I)-VX2(I)-VX5(I)+VX6(I)
C
       VY3478=VY3(I)-VY4(I)-VY7(I)+VY8(I)
       VY2358=VY2(I)-VY3(I)-VY5(I)+VY8(I)
       VY1467=VY1(I)-VY4(I)-VY6(I)+VY7(I)
       VY1256=VY1(I)-VY2(I)-VY5(I)+VY6(I)
C
       VZ3478=VZ3(I)-VZ4(I)-VZ7(I)+VZ8(I)
       VZ2358=VZ2(I)-VZ3(I)-VZ5(I)+VZ8(I)
       VZ1467=VZ1(I)-VZ4(I)-VZ6(I)+VZ7(I)
       VZ1256=VZ1(I)-VZ2(I)-VZ5(I)+VZ6(I)
!
       HGX3(I)=(VX1467-VX2358)*ONE_OVER_8
       HGX1(I)=(VX1467+VX2358)*ONE_OVER_8
       HGX2(I)=(VX1256-VX3478)*ONE_OVER_8
       HGX4(I)=-(VX1256+VX3478)*ONE_OVER_8       
C
       HGY3(I)=(VY1467-VY2358)*ONE_OVER_8
       HGY1(I)=(VY1467+VY2358)*ONE_OVER_8
       HGY2(I)=(VY1256-VY3478)*ONE_OVER_8
       HGY4(I)=-(VY1256+VY3478)*ONE_OVER_8       
C
       HGZ3(I)=(VZ1467-VZ2358)*ONE_OVER_8
       HGZ1(I)=(VZ1467+VZ2358)*ONE_OVER_8
       HGZ2(I)=(VZ1256-VZ3478)*ONE_OVER_8
       HGZ4(I)=-(VZ1256+VZ3478)*ONE_OVER_8       
      ENDDO
      DO I=1,NEL
        VX17=VX1(I)-VX7(I)
        VX28=VX2(I)-VX8(I)
        VX35=VX3(I)-VX5(I)
        VX46=VX4(I)-VX6(I)
        VY17=VY1(I)-VY7(I)
        VY28=VY2(I)-VY8(I)
        VY35=VY3(I)-VY5(I)
        VY46=VY4(I)-VY6(I)
        VZ17=VZ1(I)-VZ7(I)
        VZ28=VZ2(I)-VZ8(I)
        VZ35=VZ3(I)-VZ5(I)
        VZ46=VZ4(I)-VZ6(I)
C   alpha =1 ->eta zeta   
C 1 1 -1 -1 -1 -1 1 1
        HGX1(I)= HGX1(I)
     &          -(PX1H1(I)*VX17+PX2H1(I)*VX28
     &            +PX3H1(I)*VX35+PX4H1(I)*VX46)
        HGY1(I)= HGY1(I)
     &          -(PX1H1(I)*VY17+PX2H1(I)*VY28
     &            +PX3H1(I)*VY35+PX4H1(I)*VY46)
        HGZ1(I)= HGZ1(I)
     &          -(PX1H1(I)*VZ17+PX2H1(I)*VZ28
     &            +PX3H1(I)*VZ35+PX4H1(I)*VZ46)
C
C   alpha =2 ->zeta ksi   
C 1 -1 -1 1 -1 1 1 -1
        HGX2(I)= HGX2(I)
     &          -(PX1H2(I)*VX17+PX2H2(I)*VX28
     &            +PX3H2(I)*VX35+PX4H2(I)*VX46)
        HGY2(I)= HGY2(I)
     &          -(PX1H2(I)*VY17+PX2H2(I)*VY28
     &            +PX3H2(I)*VY35+PX4H2(I)*VY46)
        HGZ2(I)= HGZ2(I)
     &          -(PX1H2(I)*VZ17+PX2H2(I)*VZ28
     &            +PX3H2(I)*VZ35+PX4H2(I)*VZ46)
C   alpha =3 ->ksi eta    
C 1 -1 1 -1 1 -1 1 -1
        HGX3(I)= HGX3(I)
     &          -(PX1H3(I)*VX17+PX2H3(I)*VX28
     &            +PX3H3(I)*VX35+PX4H3(I)*VX46)
        HGY3(I)= HGY3(I)
     &          -(PX1H3(I)*VY17+PX2H3(I)*VY28
     &            +PX3H3(I)*VY35+PX4H3(I)*VY46)
        HGZ3(I)= HGZ3(I)
     &          -(PX1H3(I)*VZ17+PX2H3(I)*VZ28
     &            +PX3H3(I)*VZ35+PX4H3(I)*VZ46)
C
C   alpha =4 ->ksi eta zeta
C -1 1 -1 1 1 -1 1 -1
        HGX4(I)= HGX4(I)
     &          -(PX1H4(I)*VX17+PX2H4(I)*VX28
     &            +PX3H4(I)*VX35+PX4H4(I)*VX46)
        HGY4(I)= HGY4(I)
     &          -(PX1H4(I)*VY17+PX2H4(I)*VY28
     &            +PX3H4(I)*VY35+PX4H4(I)*VY46)
        HGZ4(I)= HGZ4(I)
     &          -(PX1H4(I)*VZ17+PX2H4(I)*VZ28
     &            +PX3H4(I)*VZ35+PX4H4(I)*VZ46)
      ENDDO
!
C   -------viscous ----
      DO I=1,NEL
        FHOURT(1,1) = FCL(I)*HGX1(I)
        FHOURT(1,2) = FCL(I)*HGX2(I)
        FHOURT(1,3) = FCL(I)*HGX3(I)
        FHOURT(1,4) = FCL(I)*HGX4(I)
        FHOURT(2,1) = FCL(I)*HGY1(I)
        FHOURT(2,2) = FCL(I)*HGY2(I)
        FHOURT(2,3) = FCL(I)*HGY3(I)
        FHOURT(2,4) = FCL(I)*HGY4(I)
        FHOURT(3,1) = FCL(I)*HGZ1(I)
        FHOURT(3,2) = FCL(I)*HGZ2(I)
        FHOURT(3,3) = FCL(I)*HGZ3(I)
        FHOURT(3,4) = FCL(I)*HGZ4(I)
C
        NFHOUR(I,1,1) = (H22(I)+H33(I))*FHOURT(1,1)
     .                +H12(I)*FHOURT(2,2)+H13(I)*FHOURT(3,3)
        NFHOUR(I,2,2) = (H11(I)+H33(I))*FHOURT(2,2)
     .                +H23(I)*FHOURT(3,3)+H12(I)*FHOURT(1,1)
        NFHOUR(I,3,3) = (H11(I)+H22(I))*FHOURT(3,3)
     .                +H13(I)*FHOURT(1,1)+H23(I)*FHOURT(2,2)
        NFHOUR(I,1,2) = NU1*H11(I)*FHOURT(1,2)+NU2*H12(I)*FHOURT(2,1)
        NFHOUR(I,1,3) = NU1*H11(I)*FHOURT(1,3)+NU2*H13(I)*FHOURT(3,1)
        NFHOUR(I,2,1) = NU1*H22(I)*FHOURT(2,1)+NU2*H12(I)*FHOURT(1,2)
        NFHOUR(I,3,1) = NU1*H33(I)*FHOURT(3,1)+NU2*H13(I)*FHOURT(1,3)
        NFHOUR(I,2,3) = NU1*H22(I)*FHOURT(2,3)+NU2*H23(I)*FHOURT(3,2)
        NFHOUR(I,3,2) = NU1*H33(I)*FHOURT(3,2)+NU2*H23(I)*FHOURT(2,3)
        NFHOUR(I,1,4) = NU3*H11(I)*FHOURT(1,4)
        NFHOUR(I,2,4) = NU3*H22(I)*FHOURT(2,4)
        NFHOUR(I,3,4) = NU3*H33(I)*FHOURT(3,4)
      ENDDO
C
      DO I=1,NEL
        HQ13P = (NFHOUR(I,1,1)+NFHOUR(I,1,3))*ONE_OVER_8
        HQ13N = (NFHOUR(I,1,1)-NFHOUR(I,1,3))*ONE_OVER_8
        HQ24P = (NFHOUR(I,1,2)+NFHOUR(I,1,4))*ONE_OVER_8
        HQ24N = (NFHOUR(I,1,2)-NFHOUR(I,1,4))*ONE_OVER_8
        FF =-PX1H1(I)*NFHOUR(I,1,1)-PX1H2(I)*NFHOUR(I,1,2)
     .      -PX1H3(I)*NFHOUR(I,1,3)-PX1H4(I)*NFHOUR(I,1,4)
        F11(I) =F11(I)-(HQ13P+HQ24N+FF)
        F17(I) =F17(I)-(HQ13P+HQ24P-FF)
        FF =-PX2H1(I)*NFHOUR(I,1,1)-PX2H2(I)*NFHOUR(I,1,2)
     .      -PX2H3(I)*NFHOUR(I,1,3)-PX2H4(I)*NFHOUR(I,1,4)
        F12(I) =F12(I)-(HQ13N-HQ24N+FF)
        F18(I) =F18(I)-(HQ13N-HQ24P-FF)
        FF =-PX3H1(I)*NFHOUR(I,1,1)-PX3H2(I)*NFHOUR(I,1,2)
     .      -PX3H3(I)*NFHOUR(I,1,3)-PX3H4(I)*NFHOUR(I,1,4)
        F13(I) =F13(I)-(-HQ13N-HQ24P+FF)
        F15(I) =F15(I)-(-HQ13N-HQ24N-FF)
        FF =-PX4H1(I)*NFHOUR(I,1,1)-PX4H2(I)*NFHOUR(I,1,2)
     .      -PX4H3(I)*NFHOUR(I,1,3)-PX4H4(I)*NFHOUR(I,1,4)
        F14(I) =F14(I)-(-HQ13P+HQ24P+FF)
        F16(I) =F16(I)-(-HQ13P+HQ24N-FF)
      ENDDO
      DO I=1,NEL
        HQ13P = (NFHOUR(I,2,1)+NFHOUR(I,2,3))*ONE_OVER_8
        HQ13N = (NFHOUR(I,2,1)-NFHOUR(I,2,3))*ONE_OVER_8
        HQ24P = (NFHOUR(I,2,2)+NFHOUR(I,2,4))*ONE_OVER_8
        HQ24N = (NFHOUR(I,2,2)-NFHOUR(I,2,4))*ONE_OVER_8
        FF =-PX1H1(I)*NFHOUR(I,2,1)-PX1H2(I)*NFHOUR(I,2,2)
     .      -PX1H3(I)*NFHOUR(I,2,3)-PX1H4(I)*NFHOUR(I,2,4)
        F21(I) =F21(I)-(HQ13P+HQ24N+FF)
        F27(I) =F27(I)-(HQ13P+HQ24P-FF)
        FF =-PX2H1(I)*NFHOUR(I,2,1)-PX2H2(I)*NFHOUR(I,2,2)
     .      -PX2H3(I)*NFHOUR(I,2,3)-PX2H4(I)*NFHOUR(I,2,4)
        F22(I) =F22(I)-(HQ13N-HQ24N+FF)
        F28(I) =F28(I)-(HQ13N-HQ24P-FF)
        FF =-PX3H1(I)*NFHOUR(I,2,1)-PX3H2(I)*NFHOUR(I,2,2)
     .      -PX3H3(I)*NFHOUR(I,2,3)-PX3H4(I)*NFHOUR(I,2,4)
        F23(I) =F23(I)-(-HQ13N-HQ24P+FF)
        F25(I) =F25(I)-(-HQ13N-HQ24N-FF)
        FF =-PX4H1(I)*NFHOUR(I,2,1)-PX4H2(I)*NFHOUR(I,2,2)
     .      -PX4H3(I)*NFHOUR(I,2,3)-PX4H4(I)*NFHOUR(I,2,4)
        F24(I) =F24(I)-(-HQ13P+HQ24P+FF)
        F26(I) =F26(I)-(-HQ13P+HQ24N-FF)
      ENDDO
      DO I=1,NEL
        HQ13P = (NFHOUR(I,3,1)+NFHOUR(I,3,3))*ONE_OVER_8
        HQ13N = (NFHOUR(I,3,1)-NFHOUR(I,3,3))*ONE_OVER_8
        HQ24P = (NFHOUR(I,3,2)+NFHOUR(I,3,4))*ONE_OVER_8
        HQ24N = (NFHOUR(I,3,2)-NFHOUR(I,3,4))*ONE_OVER_8
        FF =-PX1H1(I)*NFHOUR(I,3,1)-PX1H2(I)*NFHOUR(I,3,2)
     .      -PX1H3(I)*NFHOUR(I,3,3)-PX1H4(I)*NFHOUR(I,3,4)
        F31(I) =F31(I)-(HQ13P+HQ24N+FF)
        F37(I) =F37(I)-(HQ13P+HQ24P-FF)
        FF =-PX2H1(I)*NFHOUR(I,3,1)-PX2H2(I)*NFHOUR(I,3,2)
     .      -PX2H3(I)*NFHOUR(I,3,3)-PX2H4(I)*NFHOUR(I,3,4)
        F32(I) =F32(I)-(HQ13N-HQ24N+FF)
        F38(I) =F38(I)-(HQ13N-HQ24P-FF)
        FF =-PX3H1(I)*NFHOUR(I,3,1)-PX3H2(I)*NFHOUR(I,3,2)
     .      -PX3H3(I)*NFHOUR(I,3,3)-PX3H4(I)*NFHOUR(I,3,4)
        F33(I) =F33(I)-(-HQ13N-HQ24P+FF)
        F35(I) =F35(I)-(-HQ13N-HQ24N-FF)
        FF =-PX4H1(I)*NFHOUR(I,3,1)-PX4H2(I)*NFHOUR(I,3,2)
     .      -PX4H3(I)*NFHOUR(I,3,3)-PX4H4(I)*NFHOUR(I,3,4)
        F34(I) =F34(I)-(-HQ13P+HQ24P+FF)
        F36(I) =F36(I)-(-HQ13P+HQ24N-FF)
      ENDDO
C
      RETURN
      END
