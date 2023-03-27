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
Chd|  AMOMTN3                       source/elements/solid/solide/amomtn3.F
Chd|-- called by -----------
Chd|        AMOMT3                        source/elements/solid/solide/amomt3.F
Chd|-- calls ---------------
Chd|        UPWIND_V                      source/elements/solid/solide/upwind.F
Chd|====================================================================
      SUBROUTINE AMOMTN3(
     1   RHO,     DELTAX,  VIS,     VOL,
     2   A0,      A1,      A2,      A3,
     3   AJV,     R,       S,       T,
     4   VDX0,    VDY0,    VDZ0,    VX0,
     5   VY0,     VZ0,     VX1,     VY1,
     6   VZ1,     VX2,     VY2,     VZ2,
     7   VX3,     VY3,     VZ3,     F11,
     8   F21,     F31,     F12,     F22,
     9   F32,     F13,     F23,     F33,
     A   F14,     F24,     F34,     F15,
     B   F25,     F35,     F16,     F26,
     C   F36,     F17,     F27,     F37,
     D   F18,     F28,     F38,     IAD22,
     E   NC1,     NC2,     NC3,     NC4,
     F   NC5,     NC6,     NC7,     NC8,
     G   NALE,    NEL)
C-----------------------------------------------
C   M o d u l e  s
C-----------------------------------------------
      USE ALE_MOD
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
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      my_real
     .   A0(MVSIZ,3),A1(MVSIZ,3),A2(MVSIZ,3),A3(MVSIZ,3),
     .   AJV(*),R(MVSIZ,3),S(MVSIZ,3),T(MVSIZ,3),
     .   VX1(*),VX2(*),VX3(*),VX0(*),VDX0(*),
     .   VY1(*),VY2(*),VY3(*),VY0(*),VDY0(*),
     .   VZ1(*),VZ2(*),VZ3(*),VZ0(*),VDZ0(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*),
     .   RHO(*),DELTAX(*),VIS(*),VOL(*),IAD22(*)
      INTEGER :: NC1(*), NC2(*), NC3(*), NC4(*), 
     .     NC5(*), NC6(*), NC7(*), NC8(*), NALE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   GAM(MVSIZ),
     .   DXXP(MVSIZ),DXYP(MVSIZ),DXZP(MVSIZ),
     .   DYXP(MVSIZ),DYYP(MVSIZ),DYZP(MVSIZ),
     .   DZXP(MVSIZ),DZYP(MVSIZ),DZZP(MVSIZ),
     .   UP0(MVSIZ),UP1(MVSIZ),UP2(MVSIZ),UP3(MVSIZ),
     .   F1P(MVSIZ),F2P(MVSIZ),F3P(MVSIZ),
     .   F1(MVSIZ),F2(MVSIZ),F3(MVSIZ),FAC,
     .   OFF1(MVSIZ), OFF2(MVSIZ), OFF3(MVSIZ), OFF4(MVSIZ), 
     .   OFF5(MVSIZ), OFF6(MVSIZ), OFF7(MVSIZ), OFF8(MVSIZ)  
      INTEGER I
C-----------------------------------------------
      CALL UPWIND_V(RHO,VIS,VDX0,VDY0,VDZ0,R,S,T,DELTAX,GAM)
      OFF1(1:MVSIZ) = ONE
      OFF2(1:MVSIZ) = ONE
      OFF3(1:MVSIZ) = ONE
      OFF4(1:MVSIZ) = ONE
      OFF5(1:MVSIZ) = ONE
      OFF6(1:MVSIZ) = ONE
      OFF7(1:MVSIZ) = ONE
      OFF8(1:MVSIZ) = ONE
      DO I=1,NEL
         IF (NALE(NC1(I)) == 0) OFF1(I) = ZERO
         IF (NALE(NC2(I)) == 0) OFF2(I) = ZERO
         IF (NALE(NC3(I)) == 0) OFF3(I) = ZERO
         IF (NALE(NC4(I)) == 0) OFF4(I) = ZERO
         IF (NALE(NC5(I)) == 0) OFF5(I) = ZERO
         IF (NALE(NC6(I)) == 0) OFF6(I) = ZERO
         IF (NALE(NC7(I)) == 0) OFF7(I) = ZERO
         IF (NALE(NC8(I)) == 0) OFF8(I) = ZERO
      ENDDO
      IF (ALE%UPWIND%UPWM==2) THEN

      !-------------------------------------------!
      ! MOMENTUM ADVECTION                        !
      ! TG : Taylor Galerkine                     !
      !-------------------------------------------!        
       DO I=1,NEL
        GAM(I)=GAM(I)/AJV(I)

        DXXP(I)=A0(I,1)*VX0(I)+A1(I,1)*VX1(I)+A2(I,1)*VX2(I)+A3(I,1)*VX3(I)  
        DXYP(I)=A0(I,2)*VX0(I)+A1(I,2)*VX1(I)+A2(I,2)*VX2(I)+A3(I,2)*VX3(I)  
        DXZP(I)=A0(I,3)*VX0(I)+A1(I,3)*VX1(I)+A2(I,3)*VX2(I)+A3(I,3)*VX3(I)  
        DYXP(I)=A0(I,1)*VY0(I)+A1(I,1)*VY1(I)+A2(I,1)*VY2(I)+A3(I,1)*VY3(I)  
        DYYP(I)=A0(I,2)*VY0(I)+A1(I,2)*VY1(I)+A2(I,2)*VY2(I)+A3(I,2)*VY3(I)  
        DYZP(I)=A0(I,3)*VY0(I)+A1(I,3)*VY1(I)+A2(I,3)*VY2(I)+A3(I,3)*VY3(I)  
        DZXP(I)=A0(I,1)*VZ0(I)+A1(I,1)*VZ1(I)+A2(I,1)*VZ2(I)+A3(I,1)*VZ3(I)  
        DZYP(I)=A0(I,2)*VZ0(I)+A1(I,2)*VZ1(I)+A2(I,2)*VZ2(I)+A3(I,2)*VZ3(I)  
        DZZP(I)=A0(I,3)*VZ0(I)+A1(I,3)*VZ1(I)+A2(I,3)*VZ2(I)+A3(I,3)*VZ3(I)  

        FAC=EIGHT*GAM(I)
        UP0(I)=ONE +FAC*(A0(I,1)*VDX0(I)+A0(I,2)*VDY0(I)+A0(I,3)*VDZ0(I))
        UP1(I)=ONE +FAC*(A1(I,1)*VDX0(I)+A1(I,2)*VDY0(I)+A1(I,3)*VDZ0(I))
        UP2(I)=ONE +FAC*(A2(I,1)*VDX0(I)+A2(I,2)*VDY0(I)+A2(I,3)*VDZ0(I))
        UP3(I)=ONE +FAC*(A3(I,1)*VDX0(I)+A3(I,2)*VDY0(I)+A3(I,3)*VDZ0(I))

        FAC=RHO(I)*VOL(I)/AJV(I)/SIXTY4
        F1P(I)= FAC*(VDX0(I)*DXXP(I)+VDY0(I)*DXYP(I)+VDZ0(I)*DXZP(I))
        F2P(I)= FAC*(VDX0(I)*DYXP(I)+VDY0(I)*DYYP(I)+VDZ0(I)*DYZP(I))
        F3P(I)= FAC*(VDX0(I)*DZXP(I)+VDY0(I)*DZYP(I)+VDZ0(I)*DZZP(I))

        F11(I) = F11(I) - UP0(I)*F1P(I)*OFF1(I)
        F21(I) = F21(I) - UP0(I)*F2P(I)*OFF1(I)
        F31(I) = F31(I) - UP0(I)*F3P(I)*OFF1(I)
        F12(I) = F12(I) - UP1(I)*F1P(I)*OFF2(I)
        F22(I) = F22(I) - UP1(I)*F2P(I)*OFF2(I)
        F32(I) = F32(I) - UP1(I)*F3P(I)*OFF2(I)
        F13(I) = F13(I) - UP2(I)*F1P(I)*OFF3(I)
        F23(I) = F23(I) - UP2(I)*F2P(I)*OFF3(I)
        F33(I) = F33(I) - UP2(I)*F3P(I)*OFF3(I)
        F14(I) = F14(I) - UP3(I)*F1P(I)*OFF4(I)
        F24(I) = F24(I) - UP3(I)*F2P(I)*OFF4(I)
        F34(I) = F34(I) - UP3(I)*F3P(I)*OFF4(I)
        F15(I) = F15(I) - F1P(I)*OFF5(I)
        F25(I) = F25(I) - F2P(I)*OFF5(I)
        F35(I) = F35(I) - F3P(I)*OFF5(I)
        F16(I) = F16(I) - F1P(I)*OFF6(I)
        F26(I) = F26(I) - F2P(I)*OFF6(I)
        F36(I) = F36(I) - F3P(I)*OFF6(I)
        F17(I) = F17(I) - F1P(I)*OFF7(I)
        F27(I) = F27(I) - F2P(I)*OFF7(I)
        F37(I) = F37(I) - F3P(I)*OFF7(I)
        F18(I) = F18(I) - F1P(I)*OFF8(I)
        F28(I) = F28(I) - F2P(I)*OFF8(I)
        F38(I) = F38(I) - F3P(I)*OFF8(I)
       ENDDO
       
       !inter22
       IF(INT22>0)THEN
         DO I=1,NEL
           IF(NINT(IAD22(I))==0)THEN
             !currently unpluged only for cut cell buffer      
             F1P(I) = ZERO
             F2P(I) = ZERO
             F3P(I) = ZERO
           ENDIF
         ENDDO
       ENDIF

       DO I=1,NEL
         F1(I)=-GAM(I)*(-F1P(I)*(DYYP(I)+DZZP(I))+F2P(I)*DXYP(I)+F3P(I)*DXZP(I))
         F2(I)=-GAM(I)*( F1P(I)*DYXP(I)-F2P(I)*(DXXP(I)+DZZP(I))+F3P(I)*DYZP(I))
         F3(I)=-GAM(I)*( F1P(I)*DZXP(I)+F2P(I)*DZYP(I)-F3P(I)*(DYYP(I)+DXXP(I)))
         F11(I) = F11(I) - F1(I)*OFF1(I)
         F21(I) = F21(I) - F2(I)*OFF1(I)
         F31(I) = F31(I) - F3(I)*OFF1(I)
         F12(I) = F12(I) - F1(I)*OFF2(I)
         F22(I) = F22(I) - F2(I)*OFF2(I)
         F32(I) = F32(I) - F3(I)*OFF2(I)
         F13(I) = F13(I) - F1(I)*OFF3(I)
         F23(I) = F23(I) - F2(I)*OFF3(I)
         F33(I) = F33(I) - F3(I)*OFF3(I)
         F14(I) = F14(I) - F1(I)*OFF4(I)
         F24(I) = F24(I) - F2(I)*OFF4(I)
         F34(I) = F34(I) - F3(I)*OFF4(I)
         F15(I) = F15(I) - F1(I)*OFF5(I)
         F25(I) = F25(I) - F2(I)*OFF5(I)
         F35(I) = F35(I) - F3(I)*OFF5(I)
         F16(I) = F16(I) - F1(I)*OFF6(I)
         F26(I) = F26(I) - F2(I)*OFF6(I)
         F36(I) = F36(I) - F3(I)*OFF6(I)
         F17(I) = F17(I) - F1(I)*OFF7(I)
         F27(I) = F27(I) - F2(I)*OFF7(I)
         F37(I) = F37(I) - F3(I)*OFF7(I)
         F18(I) = F18(I) - F1(I)*OFF8(I)
         F28(I) = F28(I) - F2(I)*OFF8(I)
         F38(I) = F38(I) - F3(I)*OFF8(I)
       ENDDO

      ELSE

      !-------------------------------------------!
      ! MOMENTUM ADVECTION                        !
      ! SUPG : Streamline Upwind Petrov Galerkine !
      !-------------------------------------------! 
       DO I=1,NEL
        GAM(I)=GAM(I)/AJV(I)
        DXXP(I)=A0(I,1)*VX0(I)+A1(I,1)*VX1(I)+A2(I,1)*VX2(I)+A3(I,1)*VX3(I)  
        DXYP(I)=A0(I,2)*VX0(I)+A1(I,2)*VX1(I)+A2(I,2)*VX2(I)+A3(I,2)*VX3(I)  
        DXZP(I)=A0(I,3)*VX0(I)+A1(I,3)*VX1(I)+A2(I,3)*VX2(I)+A3(I,3)*VX3(I)  
        DYXP(I)=A0(I,1)*VY0(I)+A1(I,1)*VY1(I)+A2(I,1)*VY2(I)+A3(I,1)*VY3(I)  
        DYYP(I)=A0(I,2)*VY0(I)+A1(I,2)*VY1(I)+A2(I,2)*VY2(I)+A3(I,2)*VY3(I)  
        DYZP(I)=A0(I,3)*VY0(I)+A1(I,3)*VY1(I)+A2(I,3)*VY2(I)+A3(I,3)*VY3(I)  
        DZXP(I)=A0(I,1)*VZ0(I)+A1(I,1)*VZ1(I)+A2(I,1)*VZ2(I)+A3(I,1)*VZ3(I)  
        DZYP(I)=A0(I,2)*VZ0(I)+A1(I,2)*VZ1(I)+A2(I,2)*VZ2(I)+A3(I,2)*VZ3(I)  
        DZZP(I)=A0(I,3)*VZ0(I)+A1(I,3)*VZ1(I)+A2(I,3)*VZ2(I)+A3(I,3)*VZ3(I)  

        FAC=EIGHT*GAM(I)
        UP0(I)=ONE+FAC*(A0(I,1)*VDX0(I)+A0(I,2)*VDY0(I)+A0(I,3)*VDZ0(I))
        UP1(I)=ONE+FAC*(A1(I,1)*VDX0(I)+A1(I,2)*VDY0(I)+A1(I,3)*VDZ0(I))
        UP2(I)=ONE+FAC*(A2(I,1)*VDX0(I)+A2(I,2)*VDY0(I)+A2(I,3)*VDZ0(I))
        UP3(I)=ONE+FAC*(A3(I,1)*VDX0(I)+A3(I,2)*VDY0(I)+A3(I,3)*VDZ0(I))

        FAC=RHO(I)*VOL(I)/AJV(I)/SIXTY4
        F1P(I)= FAC*(VDX0(I)*DXXP(I)+VDY0(I)*DXYP(I)+VDZ0(I)*DXZP(I))
        F2P(I)= FAC*(VDX0(I)*DYXP(I)+VDY0(I)*DYYP(I)+VDZ0(I)*DYZP(I))
        F3P(I)= FAC*(VDX0(I)*DZXP(I)+VDY0(I)*DZYP(I)+VDZ0(I)*DZZP(I))
       ENDDO

        !inter22
        IF(INT22>0)THEN 
          DO I=1,NEL       
            IF(NINT(IAD22(I))==0)THEN
              !currently unpluged only for cut cell buffer
              F1P(I) = ZERO
              F2P(I) = ZERO
              F3P(I) = ZERO
            ENDIF        
          ENDDO
        ENDIF

       DO I=1,NEL
        F11(I) = F11(I) - UP0(I)*F1P(I)*OFF1(I)
        F21(I) = F21(I) - UP0(I)*F2P(I)*OFF1(I)
        F31(I) = F31(I) - UP0(I)*F3P(I)*OFF1(I)
        F12(I) = F12(I) - UP1(I)*F1P(I)*OFF2(I)
        F22(I) = F22(I) - UP1(I)*F2P(I)*OFF2(I)
        F32(I) = F32(I) - UP1(I)*F3P(I)*OFF2(I)
        F13(I) = F13(I) - UP2(I)*F1P(I)*OFF3(I)
        F23(I) = F23(I) - UP2(I)*F2P(I)*OFF3(I)
        F33(I) = F33(I) - UP2(I)*F3P(I)*OFF3(I)
        F14(I) = F14(I) - UP3(I)*F1P(I)*OFF4(I)
        F24(I) = F24(I) - UP3(I)*F2P(I)*OFF4(I)
        F34(I) = F34(I) - UP3(I)*F3P(I)*OFF4(I)
        F15(I) = F15(I) - F1P(I)*OFF5(I)
        F25(I) = F25(I) - F2P(I)*OFF5(I)
        F35(I) = F35(I) - F3P(I)*OFF5(I)
        F16(I) = F16(I) - F1P(I)*OFF6(I)
        F26(I) = F26(I) - F2P(I)*OFF6(I)
        F36(I) = F36(I) - F3P(I)*OFF6(I)
        F17(I) = F17(I) - F1P(I)*OFF7(I)
        F27(I) = F27(I) - F2P(I)*OFF7(I)
        F37(I) = F37(I) - F3P(I)*OFF7(I)
        F18(I) = F18(I) - F1P(I)*OFF8(I)
        F28(I) = F28(I) - F2P(I)*OFF8(I)
        F38(I) = F38(I) - F3P(I)*OFF8(I)
       ENDDO

      ENDIF

C-----------------------------------------------
      RETURN
      END
