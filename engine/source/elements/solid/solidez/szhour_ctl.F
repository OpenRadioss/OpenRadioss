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
      !||====================================================================
      !||    szhour_ctl      ../engine/source/elements/solid/solidez/szhour_ctl.F
      !||--- called by ------------------------------------------------------
      !||    szforc3         ../engine/source/elements/solid/solidez/szforc3.F
      !||--- calls      -----------------------------------------------------
      !||    shour_ctl       ../engine/source/elements/solid/solidez/shour_ctl.F90
      !||    szstrainhg      ../engine/source/elements/solid/solidez/szstrainhg.F
      !||--- uses       -----------------------------------------------------
      !||    shour_ctl_mod   ../engine/source/elements/solid/solidez/shour_ctl.F90
      !||====================================================================
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
     .   VOL0 ,    DN  ,   STI  ,   NEL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SHOUR_CTL_MOD,   ONLY : SHOUR_CTL
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
     .   F17,F27,F37,F18,F28,F38,EINT,STI
      my_real, INTENT(IN):: DT1,DN
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
       DO I=1,NEL
        F11(I)=zero
        F12(I)=zero
        F13(I)=zero
        F14(I)=zero
        F15(I)=zero
        F16(I)=zero
        F17(I)=zero
        F18(I)=zero
C
        F21(I)=zero
        F22(I)=zero
        F23(I)=zero
        F24(I)=zero
        F25(I)=zero
        F26(I)=zero
        F27(I)=zero
        F28(I)=zero
C
        F31(I)=zero
        F32(I)=zero
        F33(I)=zero
        F34(I)=zero
        F35(I)=zero
        F36(I)=zero
        F37(I)=zero
        F38(I)=zero
       ENDDO
         CALL SHOUR_CTL(
     .       PM,      RHO,     OFF,     VX1,
     .       VX2,     VX3,     VX4,     VX5,
     .       VX6,     VX7,     VX8,     VY1,
     .       VY2,     VY3,     VY4,     VY5,
     .       VY6,     VY7,     VY8,     VZ1,
     .       VZ2,     VZ3,     VZ4,     VZ5,
     .       VZ6,     VZ7,     VZ8,     F11,
     .       F21,     F31,     F12,     F22,
     .       F32,     F13,     F23,     F33,
     .       F14,     F24,     F34,     F15,
     .       F25,     F35,     F16,     F26,
     .       F36,     F17,     F27,     F37,
     .       F18,     F28,     F38,     PX1H1,
     .       PX1H2,   PX1H3,   PX2H1,   PX2H2,
     .       PX2H3,   PX3H1,   PX3H2,   PX3H3,
     .       PX4H1,   PX4H2,   PX4H3,    VOL,
     .       FHOUR,   MTN,      DT1 ,    MAT,     
     .      CXX,     EINT,   NPROPM, NUMMAT,
     .       VOL0,   DN  ,     STI ,   NEL )
!
      IF(((ANIM_N(IAD_GPS+400+1) == 1) .OR. (ANIM_N(IAD_GPS+400+2) == 1) .OR. 
     .   (ANIM_N(IAD_GPS+400+3) == 1) .OR. (ANIM_N(IAD_GPS+400+4) == 1) .OR. 
     .   (ANIM_N(IAD_GPS+400+5) == 1) .OR. (ANIM_N(IAD_GPS+400+6) == 1)) )THEN
        MX = MAT(1)
        NU=PM(21,MX)
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
