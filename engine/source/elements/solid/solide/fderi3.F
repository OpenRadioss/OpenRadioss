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
Chd|  FDERI3                        source/elements/solid/solide/fderi3.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|        SCHKJAB3                      source/elements/solid/solide/sderi3.F
Chd|====================================================================
      SUBROUTINE FDERI3(
     1   OFF,     DET,     NGL,     X1,
     2   X2,      X3,      X4,      X5,
     3   X6,      X7,      X8,      Y1,
     4   Y2,      Y3,      Y4,      Y5,
     5   Y6,      Y7,      Y8,      Z1,
     6   Z2,      Z3,      Z4,      Z5,
     7   Z6,      Z7,      Z8,      PX1,
     8   PX2,     PX3,     PX4,     PY1,
     9   PY2,     PY3,     PY4,     PZ1,
     A   PZ2,     PZ3,     PZ4,     PX1H1,
     B   PX1H2,   PX1H3,   PX2H1,   PX2H2,
     C   PX2H3,   PX3H1,   PX3H2,   PX3H3,
     D   PX4H1,   PX4H2,   PX4H3,   NEL,
     E   JHBE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
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
      INTEGER, INTENT(IN) :: JHBE
      my_real
     .   OFF(*),DET(*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),  
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   PX1H1(*), PX1H2(*), PX1H3(*),  
     .   PX2H1(*), PX2H2(*), PX2H3(*),  
     .   PX3H1(*), PX3H2(*), PX3H3(*),  
     .   PX4H1(*), PX4H2(*), PX4H3(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J
      my_real
     .   DETT(MVSIZ) ,  AJ7(MVSIZ), AJ8(MVSIZ) , AJ9(MVSIZ),
     .   AJI1(MVSIZ), AJI2(MVSIZ), AJI3(MVSIZ),
     .   AJI4(MVSIZ), AJI5(MVSIZ), AJI6(MVSIZ),
     .   AJI7(MVSIZ), AJI8(MVSIZ), AJI9(MVSIZ),
     .   X17(MVSIZ) , X28(MVSIZ) , X35(MVSIZ) , X46(MVSIZ),
     .   Y17(MVSIZ) , Y28(MVSIZ) , Y35(MVSIZ) , Y46(MVSIZ),
     .   Z17(MVSIZ) , Z28(MVSIZ) , Z35(MVSIZ) , Z46(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   AJ12(MVSIZ), AJ45(MVSIZ), AJ78(MVSIZ),
     .   A17(MVSIZ) , A28(MVSIZ) ,
     .   B17(MVSIZ) , B28(MVSIZ) ,
     .   C17(MVSIZ) , C28(MVSIZ) , AJ4(MVSIZ),
     .   AJ5(MVSIZ) , AJ6(MVSIZ) , AJ1(MVSIZ),
     .   AJ2(MVSIZ) , AJ3(MVSIZ) ,
     .   HX,HY,HZ,    
     .   ICOR
C-----------------------------------------------
C
      DO  I=1,NEL
        X17(I)=X7(I)-X1(I)
        X28(I)=X8(I)-X2(I)
        X35(I)=X5(I)-X3(I)
        X46(I)=X6(I)-X4(I)
        Y17(I)=Y7(I)-Y1(I)
        Y28(I)=Y8(I)-Y2(I)
        Y35(I)=Y5(I)-Y3(I)
        Y46(I)=Y6(I)-Y4(I)
        Z17(I)=Z7(I)-Z1(I)
        Z28(I)=Z8(I)-Z2(I)
        Z35(I)=Z5(I)-Z3(I)
        Z46(I)=Z6(I)-Z4(I)
      ENDDO
C 
      DO  I=1,NEL               
         AJ1(I)=X17(I)+X28(I)-X35(I)-X46(I)
         AJ2(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
         AJ3(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
         A17(I)=X17(I)+X46(I)
         A28(I)=X28(I)+X35(I)
         B17(I)=Y17(I)+Y46(I)
         B28(I)=Y28(I)+Y35(I)
         C17(I)=Z17(I)+Z46(I)
         C28(I)=Z28(I)+Z35(I)
      ENDDO
         
      DO I=1,NEL
        AJ4(I)=A17(I)+A28(I)
        AJ5(I)=B17(I)+B28(I)
        AJ6(I)=C17(I)+C28(I)
        AJ7(I)=A17(I)-A28(I)
        AJ8(I)=B17(I)-B28(I)
        AJ9(I)=C17(I)-C28(I)
      ENDDO
C                  
C     JACOBIAN
C
      DO  I=1,NEL      
        JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
        JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
        JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
      ENDDO
C
      DO  I=1,NEL    
       DET(I)=ONE_OVER_64*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
      ENDDO 
C        
      CALL SCHKJAB3(
     1   OFF,     DET,     NGL,     NEL)
C
      DO  I=1,NEL
        DETT(I)=ONE_OVER_64/DET(I)
      ENDDO  
        

C
C     INVERSE DE LA MATRICE JACOBIENNE
C
      DO  I=1,NEL
        AJI1(I)=DETT(I)*JAC_59_68(I)
        AJI4(I)=DETT(I)*JAC_67_49(I)
        AJI7(I)=DETT(I)*JAC_48_57(I)
        AJI2(I)=DETT(I)*(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
        AJI5(I)=DETT(I)*( AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
        AJI8(I)=DETT(I)*(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
        AJI3(I)=DETT(I)*( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
        AJI6(I)=DETT(I)*(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
        AJI9(I)=DETT(I)*( AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
      ENDDO
C
      DO  I=1,NEL
        AJ12(I)=AJI1(I)-AJI2(I)
        AJ45(I)=AJI4(I)-AJI5(I)
        AJ78(I)=AJI7(I)-AJI8(I)
c
        PX3(I)= AJ12(I)+AJI3(I)
        PY3(I)= AJ45(I)+AJI6(I)
        PZ3(I)= AJ78(I)+AJI9(I)
        PX4(I)= AJ12(I)-AJI3(I)
        PY4(I)= AJ45(I)-AJI6(I)
        PZ4(I)= AJ78(I)-AJI9(I)
C
        AJ12(I)=AJI1(I)+AJI2(I)
        AJ45(I)=AJI4(I)+AJI5(I)
        AJ78(I)=AJI7(I)+AJI8(I)
C        
        PX1(I)=-AJ12(I)-AJI3(I)
        PY1(I)=-AJ45(I)-AJI6(I)
        PZ1(I)=-AJ78(I)-AJI9(I)
        PX2(I)=-AJ12(I)+AJI3(I)
        PY2(I)=-AJ45(I)+AJI6(I)
        PZ2(I)=-AJ78(I)+AJI9(I)
      ENDDO  
C
      IF(JHBE/=0)THEN
        DO I=1,NEL
C 1 -1 1 -1 1 -1 1 -1
         HX=(X1(I)-X2(I)+X3(I)-X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY=(Y1(I)-Y2(I)+Y3(I)-Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ=(Z1(I)-Z2(I)+Z3(I)-Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))
         PX1H1(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H1(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H1(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H1(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
       ENDDO
C 1 1 -1 -1 -1 -1 1 1
        DO I=1,NEL
         HX=(X1(I)+X2(I)-X3(I)-X4(I)-X5(I)-X6(I)+X7(I)+X8(I))
         HY=(Y1(I)+Y2(I)-Y3(I)-Y4(I)-Y5(I)-Y6(I)+Y7(I)+Y8(I))
         HZ=(Z1(I)+Z2(I)-Z3(I)-Z4(I)-Z5(I)-Z6(I)+Z7(I)+Z8(I))
         PX1H2(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H2(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H2(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H2(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
       ENDDO

C 1 -1 -1 1 -1 1 1 -1
        DO I=1,NEL
         HX=(X1(I)-X2(I)-X3(I)+X4(I)-X5(I)+X6(I)+X7(I)-X8(I))
         HY=(Y1(I)-Y2(I)-Y3(I)+Y4(I)-Y5(I)+Y6(I)+Y7(I)-Y8(I))
         HZ=(Z1(I)-Z2(I)-Z3(I)+Z4(I)-Z5(I)+Z6(I)+Z7(I)-Z8(I))
         PX1H3(I)=PX1(I)*HX+ PY1(I)*HY+PZ1(I)*HZ
         PX2H3(I)=PX2(I)*HX+ PY2(I)*HY+PZ2(I)*HZ
         PX3H3(I)=PX3(I)*HX+ PY3(I)*HY+PZ3(I)*HZ
         PX4H3(I)=PX4(I)*HX+ PY4(I)*HY+PZ4(I)*HZ
        ENDDO

      ENDIF
      RETURN
C
 1000 FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB',I10/)
 2000 FORMAT(/' ZERO OR NEGATIVE VOLUME : DELETE 3D-ELEMENT NB',I10/)
      END
