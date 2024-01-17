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
      SUBROUTINE SZHOUR_CTL(
     .   PM,      RHO,     OFF,     VX1,
     .   VX2,     VX3,     VX4,     VX5,
     .   VX6,     VX7,     VX8,     VY1,
     .   VY2,     VY3,     VY4,     VY5,
     .   VY6,     VY7,     VY8,     VZ1,
     .   VZ2,     VZ3,     VZ4,     VZ5,
     .   VZ6,     VZ7,     VZ8,     F11,
     .   F21,     F31,     F12,     F22,
     .   F32,     F13,     F23,     F33,
     .   F14,     F24,     F34,     F15,
     .   F25,     F35,     F16,     F26,
     .   F36,     F17,     F27,     F37,
     .   F18,     F28,     F38,     PX1H1,
     .   PX1H2,   PX1H3,   PX2H1,   PX2H2,
     .   PX2H3,   PX3H1,   PX3H2,   PX3H3,
     .   PX4H1,   PX4H2,   PX4H3,   VOL,
     .   FHOUR,   MTN,      DT1 ,    MAT,     
     .   CXX  ,   EINT ,  NPROPM, NUMMAT,
     .   STRHG,    JR  ,      JS,    JT , 
     .   VOL0 ,   NEL )
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
#include      "scr14_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: MTN,NPROPM,NUMMAT
      INTEGER, DIMENSION(MVSIZ) :: MAT
      my_real, DIMENSION(NPROPM,NUMMAT) ,INTENT(IN):: PM
      my_real, DIMENSION(NEL,3,4) ,INTENT(INOUT):: FHOUR
      my_real, DIMENSION(NEL,18)  ,INTENT(INOUT):: STRHG
      my_real, DIMENSION(NEL) ,INTENT(IN):: RHO,VOL0
      my_real, DIMENSION(MVSIZ) ,INTENT(IN):: OFF,VOL,
     .   VX1,VX2,VX3,VX4,VX5,VX6,VX7,VX8,
     .   VY1,VY2,VY3,VY4,VY5,VY6,VY7,VY8,
     .   VZ1,VZ2,VZ3,VZ4,VZ5,VZ6,VZ7,VZ8,
     .   PX1H1, PX1H2, PX1H3,  
     .   PX2H1, PX2H2, PX2H3,  
     .   PX3H1, PX3H2, PX3H3,  
     .   PX4H1, PX4H2, PX4H3,CXX,JR,JS,JT
      my_real, DIMENSION(MVSIZ) ,INTENT(INOUT):: 
     .   F11,F21,F31,F12,F22,F32,
     .   F13,F23,F33,F14,F24,F34,
     .   F15,F25,F35,F16,F26,F36,
     .   F17,F27,F37,F18,F28,F38,EINT
      my_real, INTENT(IN):: DT1
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX, J, II, IC
C     REAL
      my_real
     .   CAQ(MVSIZ), FCL(MVSIZ), EDT(MVSIZ),
     .   HX1(MVSIZ), HX2(MVSIZ), HX3(MVSIZ), HX4(MVSIZ),
     .   HY1(MVSIZ), HY2(MVSIZ), HY3(MVSIZ), HY4(MVSIZ),
     .   HZ1(MVSIZ), HZ2(MVSIZ), HZ3(MVSIZ), HZ4(MVSIZ),
     .   HGX1(MVSIZ), HGX2(MVSIZ), HGX3(MVSIZ), HGX4(MVSIZ),
     .   HGY1(MVSIZ), HGY2(MVSIZ), HGY3(MVSIZ), HGY4(MVSIZ),
     .   HGZ1(MVSIZ), HGZ2(MVSIZ), HGZ3(MVSIZ), HGZ4(MVSIZ),
     .   G11(MVSIZ),G21(MVSIZ),G31(MVSIZ),G41(MVSIZ),
     .   G51(MVSIZ),G61(MVSIZ),G71(MVSIZ),G81(MVSIZ),
     .   G12(MVSIZ),G22(MVSIZ),G32(MVSIZ),G42(MVSIZ),
     .   G52(MVSIZ),G62(MVSIZ),G72(MVSIZ),G82(MVSIZ),
     .   G13(MVSIZ),G23(MVSIZ),G33(MVSIZ),G43(MVSIZ),
     .   G53(MVSIZ),G63(MVSIZ),G73(MVSIZ),G83(MVSIZ),NU2(MVSIZ),
     .   JR_1(MVSIZ),JS_1(MVSIZ),JT_1(MVSIZ),NU4(MVSIZ),
     .   E0,G0,C1,NU,EHOURT ,QH,LAMG ,STIF ,LL ,FVL,NU1
C----------------------------------------------- 
C------recompute modulus for law42,88,90
      MX = MAT(1)
      NU=PM(21,MX)
      G0=PM(22,MX)
      C1=PM(32,MX)
      E0=PM(20,MX)
      QH = TEN
      SELECT CASE (MTN)
        CASE (70)
          E0=PM(24,MX)
          LAMG = THIRD*(E0/(1-TWO*NU)+TWO*E0/(1+NU))
          QH = TWELVE
        CASE (42,69)
          C1 = THIRD*E0/(1-TWO*NU)
          G0 = HALF*G0
          LAMG = C1+FOUR_OVER_3*G0
        CASE (88)
          C1 = THIRD*E0/(1-TWO*NU)
          LAMG = C1+FOUR_OVER_3*G0
        CASE DEFAULT
          LAMG = C1+FOUR_OVER_3*G0
      END SELECT
      STIF = 0.038*QH*LAMG       ! factor=1/8/3; QH=10 isn't taken into account for dt
      FVL = FOURTH*EM01*ZEP00666666667
      DO I=1,NEL
         LL = OFF(I)*VOL(I)**THIRD
         CAQ(I)=STIF*DT1
         EDT(I)=CAQ(I)*LL
         FCL(I)=FVL*RHO(I)*CXX(I)*LL*LL
      ENDDO
       DO I=1,NEL
C 1 -1 1 -1 1 -1 1 -1
        G11(I)= ONE-PX1H1(I)
        G21(I)=-ONE-PX2H1(I)
        G31(I)= ONE-PX3H1(I)
        G41(I)=-ONE-PX4H1(I)
        G51(I)= ONE+PX3H1(I)
        G61(I)=-ONE+PX4H1(I)
        G71(I)= ONE+PX1H1(I)
        G81(I)=-ONE+PX2H1(I)
        HGX1(I)=
     &    G11(I)*VX1(I)+G21(I)*VX2(I)+G31(I)*VX3(I)+G41(I)*VX4(I)
     &   +G51(I)*VX5(I)+G61(I)*VX6(I)+G71(I)*VX7(I)+G81(I)*VX8(I)
        HGY1(I)=
     &    G11(I)*VY1(I)+G21(I)*VY2(I)+G31(I)*VY3(I)+G41(I)*VY4(I)
     &   +G51(I)*VY5(I)+G61(I)*VY6(I)+G71(I)*VY7(I)+G81(I)*VY8(I)
        HGZ1(I)=
     &    G11(I)*VZ1(I)+G21(I)*VZ2(I)+G31(I)*VZ3(I)+G41(I)*VZ4(I)
     &   +G51(I)*VZ5(I)+G61(I)*VZ6(I)+G71(I)*VZ7(I)+G81(I)*VZ8(I)
       ENDDO
C
       DO I=1,NEL
C 1 1 -1 -1 -1 -1 1 1
        G12(I)= ONE-PX1H2(I)
        G22(I)= ONE-PX2H2(I)
        G32(I)=-ONE-PX3H2(I)
        G42(I)=-ONE-PX4H2(I)
        G52(I)=-ONE+PX3H2(I)
        G62(I)=-ONE+PX4H2(I)
        G72(I)= ONE+PX1H2(I)
        G82(I)= ONE+PX2H2(I)
        HGX2(I)=
     &   G12(I)*VX1(I)+G22(I)*VX2(I)+G32(I)*VX3(I)+G42(I)*VX4(I)
     &  +G52(I)*VX5(I)+G62(I)*VX6(I)+G72(I)*VX7(I)+G82(I)*VX8(I)
        HGY2(I)=
     &   G12(I)*VY1(I)+G22(I)*VY2(I)+G32(I)*VY3(I)+G42(I)*VY4(I)
     &  +G52(I)*VY5(I)+G62(I)*VY6(I)+G72(I)*VY7(I)+G82(I)*VY8(I)
        HGZ2(I)=
     &   G12(I)*VZ1(I)+G22(I)*VZ2(I)+G32(I)*VZ3(I)+G42(I)*VZ4(I)
     &  +G52(I)*VZ5(I)+G62(I)*VZ6(I)+G72(I)*VZ7(I)+G82(I)*VZ8(I)
       ENDDO
       DO I=1,NEL
C 1 -1 -1 1 -1 1 1 -1
        G13(I)= ONE-PX1H3(I)
        G23(I)=-ONE-PX2H3(I)
        G33(I)=-ONE-PX3H3(I)
        G43(I)= ONE-PX4H3(I)
        G53(I)=-ONE+PX3H3(I)
        G63(I)= ONE+PX4H3(I)
        G73(I)= ONE+PX1H3(I)
        G83(I)=-ONE+PX2H3(I)
        HGX3(I)=
     &   G13(I)*VX1(I)+G23(I)*VX2(I)+G33(I)*VX3(I)+G43(I)*VX4(I)
     &  +G53(I)*VX5(I)+G63(I)*VX6(I)+G73(I)*VX7(I)+G83(I)*VX8(I)
        HGY3(I)=
     &   G13(I)*VY1(I)+G23(I)*VY2(I)+G33(I)*VY3(I)+G43(I)*VY4(I)
     &  +G53(I)*VY5(I)+G63(I)*VY6(I)+G73(I)*VY7(I)+G83(I)*VY8(I)
        HGZ3(I)=
     &   G13(I)*VZ1(I)+G23(I)*VZ2(I)+G33(I)*VZ3(I)+G43(I)*VZ4(I)
     &  +G53(I)*VZ5(I)+G63(I)*VZ6(I)+G73(I)*VZ7(I)+G83(I)*VZ8(I)
       ENDDO
C       
       DO I=1,NEL
C 1 -1 1 -1 -1 1 -1 1
         HGX4(I)=VX1(I)-VX2(I)+VX3(I)-VX4(I)-VX5(I)+VX6(I)-VX7(I)+VX8(I)
         HGY4(I)=VY1(I)-VY2(I)+VY3(I)-VY4(I)-VY5(I)+VY6(I)-VY7(I)+VY8(I)
         HGZ4(I)=VZ1(I)-VZ2(I)+VZ3(I)-VZ4(I)-VZ5(I)+VZ6(I)-VZ7(I)+VZ8(I)
       ENDDO
C
      DO I=1,NEL
C
       FHOUR(I,1,1) = FHOUR(I,1,1) + EDT(I)*HGX1(I)
       FHOUR(I,1,2) = FHOUR(I,1,2) + EDT(I)*HGX2(I)
       FHOUR(I,1,3) = FHOUR(I,1,3) + EDT(I)*HGX3(I)
       FHOUR(I,1,4) = FHOUR(I,1,4) + EDT(I)*HGX4(I)
       FHOUR(I,2,1) = FHOUR(I,2,1) + EDT(I)*HGY1(I)
       FHOUR(I,2,2) = FHOUR(I,2,2) + EDT(I)*HGY2(I)
       FHOUR(I,2,3) = FHOUR(I,2,3) + EDT(I)*HGY3(I)
       FHOUR(I,2,4) = FHOUR(I,2,4) + EDT(I)*HGY4(I)
       FHOUR(I,3,1) = FHOUR(I,3,1) + EDT(I)*HGZ1(I)
       FHOUR(I,3,2) = FHOUR(I,3,2) + EDT(I)*HGZ2(I)
       FHOUR(I,3,3) = FHOUR(I,3,3) + EDT(I)*HGZ3(I)
       FHOUR(I,3,4) = FHOUR(I,3,4) + EDT(I)*HGZ4(I)
      ENDDO
       DO I=1,NEL
        HX1(I)=FHOUR(I,1,1) +  FCL(I)*HGX1(I)
        HX2(I)=FHOUR(I,1,2) +  FCL(I)*HGX2(I)
        HX3(I)=FHOUR(I,1,3) +  FCL(I)*HGX3(I)
        HX4(I)=FHOUR(I,1,4) +  FCL(I)*HGX4(I)
        HY1(I)=FHOUR(I,2,1) +  FCL(I)*HGY1(I)
        HY2(I)=FHOUR(I,2,2) +  FCL(I)*HGY2(I)
        HY3(I)=FHOUR(I,2,3) +  FCL(I)*HGY3(I)
        HY4(I)=FHOUR(I,2,4) +  FCL(I)*HGY4(I)
        HZ1(I)=FHOUR(I,3,1) +  FCL(I)*HGZ1(I)
        HZ2(I)=FHOUR(I,3,2) +  FCL(I)*HGZ2(I)
        HZ3(I)=FHOUR(I,3,3) +  FCL(I)*HGZ3(I)
        HZ4(I)=FHOUR(I,3,4) +  FCL(I)*HGZ4(I)
       ENDDO
C
       DO I=1,NEL
        F11(I)=-G11(I)*HX1(I)-G12(I)*HX2(I)-G13(I)*HX3(I)-HX4(I)
        F12(I)=-G21(I)*HX1(I)-G22(I)*HX2(I)-G23(I)*HX3(I)+HX4(I)
        F13(I)=-G31(I)*HX1(I)-G32(I)*HX2(I)-G33(I)*HX3(I)-HX4(I)
        F14(I)=-G41(I)*HX1(I)-G42(I)*HX2(I)-G43(I)*HX3(I)+HX4(I)
        F15(I)=-G51(I)*HX1(I)-G52(I)*HX2(I)-G53(I)*HX3(I)+HX4(I)
        F16(I)=-G61(I)*HX1(I)-G62(I)*HX2(I)-G63(I)*HX3(I)-HX4(I)
        F17(I)=-G71(I)*HX1(I)-G72(I)*HX2(I)-G73(I)*HX3(I)+HX4(I)
        F18(I)=-G81(I)*HX1(I)-G82(I)*HX2(I)-G83(I)*HX3(I)-HX4(I)
C
        F21(I)=-G11(I)*HY1(I)-G12(I)*HY2(I)-G13(I)*HY3(I)-HY4(I)
        F22(I)=-G21(I)*HY1(I)-G22(I)*HY2(I)-G23(I)*HY3(I)+HY4(I)
        F23(I)=-G31(I)*HY1(I)-G32(I)*HY2(I)-G33(I)*HY3(I)-HY4(I)
        F24(I)=-G41(I)*HY1(I)-G42(I)*HY2(I)-G43(I)*HY3(I)+HY4(I)
        F25(I)=-G51(I)*HY1(I)-G52(I)*HY2(I)-G53(I)*HY3(I)+HY4(I)
        F26(I)=-G61(I)*HY1(I)-G62(I)*HY2(I)-G63(I)*HY3(I)-HY4(I)
        F27(I)=-G71(I)*HY1(I)-G72(I)*HY2(I)-G73(I)*HY3(I)+HY4(I)
        F28(I)=-G81(I)*HY1(I)-G82(I)*HY2(I)-G83(I)*HY3(I)-HY4(I)
C
        F31(I)=-G11(I)*HZ1(I)-G12(I)*HZ2(I)-G13(I)*HZ3(I)-HZ4(I)
        F32(I)=-G21(I)*HZ1(I)-G22(I)*HZ2(I)-G23(I)*HZ3(I)+HZ4(I)
        F33(I)=-G31(I)*HZ1(I)-G32(I)*HZ2(I)-G33(I)*HZ3(I)-HZ4(I)
        F34(I)=-G41(I)*HZ1(I)-G42(I)*HZ2(I)-G43(I)*HZ3(I)+HZ4(I)
        F35(I)=-G51(I)*HZ1(I)-G52(I)*HZ2(I)-G53(I)*HZ3(I)+HZ4(I)
        F36(I)=-G61(I)*HZ1(I)-G62(I)*HZ2(I)-G63(I)*HZ3(I)-HZ4(I)
        F37(I)=-G71(I)*HZ1(I)-G72(I)*HZ2(I)-G73(I)*HZ3(I)+HZ4(I)
        F38(I)=-G81(I)*HZ1(I)-G82(I)*HZ2(I)-G83(I)*HZ3(I)-HZ4(I)
       ENDDO
c 
      DO I=1,NEL
        EINT(I)= EINT(I)+DT1*(
     &   HZ1(I)*HGZ1(I) + HZ2(I)*HGZ2(I) + 
     &   HZ3(I)*HGZ3(I) + HZ4(I)*HGZ4(I) + 
     &   HX1(I)*HGX1(I) + HX2(I)*HGX2(I) + 
     &   HX3(I)*HGX3(I) + HX4(I)*HGX4(I) + 
     &   HY1(I)*HGY1(I) + HY2(I)*HGY2(I) + 
     &   HY3(I)*HGY3(I) + HY4(I)*HGY4(I) )/VOL0(I) 
      ENDDO
C      
      IF(((ANIM_N(IAD_GPS+400+1) == 1) .OR. (ANIM_N(IAD_GPS+400+2) == 1) .OR. 
     .   (ANIM_N(IAD_GPS+400+3) == 1) .OR. (ANIM_N(IAD_GPS+400+4) == 1) .OR. 
     .   (ANIM_N(IAD_GPS+400+5) == 1) .OR. (ANIM_N(IAD_GPS+400+6) == 1)) )THEN
        NU1 =TWO/(ONE-NU)
       DO I=1,NEL
        JR_1(I) = ONE/MAX(EM20,JR(I))
        JS_1(I) = ONE/MAX(EM20,JS(I))
        JT_1(I) = ONE/MAX(EM20,JT(I))
        NU2(I) =HALF*NU*NU1
        NU4(I) =NU
       ENDDO
       CALL SZSTRAINHG(
     1   JR_1,    JS_1,    JT_1,    STRHG,
     2   NEL,     HGX1,    HGX2,    HGX3,
     3   HGX4,    HGY1,    HGY2,    HGY3,
     4   HGY4,    HGZ1,    HGZ2,    HGZ3,
     5   HGZ4,    NU4,     NU2)
      ENDIF
C
      RETURN
      END
