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
Chd|  S8EDERICT3                    source/elements/solid/solide8e/s8ederict3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        SCHKJAB3                      source/elements/solid/solide/schkjab3.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S8EDERICT3(
     1   OFF,     DET,     NGL,     X1,
     2   X2,      X3,      X4,      X5,
     3   X6,      X7,      X8,      Y1,
     4   Y2,      Y3,      Y4,      Y5,
     5   Y6,      Y7,      Y8,      Z1,
     6   Z2,      Z3,      Z4,      Z5,
     7   Z6,      Z7,      Z8,      PX1,
     8   PX2,     PX3,     PX4,     PY1,
     9   PY2,     PY3,     PY4,     PZ1,
     A   PZ2,     PZ3,     PZ4,     HX,
     B   HY,      HZ,      AJ1,     AJ2,
     C   AJ3,     AJ4,     AJ5,     AJ6,
     D   AJ7,     AJ8,     AJ9,     SMAX,
     E   NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
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
      DOUBLE PRECISION
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)
C     REAL   
      my_real
     .   OFF(*),DET(*),  
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   HX(MVSIZ,4),  HY(MVSIZ,4), HZ(MVSIZ,4),   
     .   AJ1(*),AJ2(*),AJ3(*),
     .   AJ4(*),AJ5(*),AJ6(*),
     .   AJ7(*),AJ8(*),AJ9(*),SMAX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   DETT(MVSIZ) ,   
     .   AJI1(MVSIZ), AJI2(MVSIZ), AJI3(MVSIZ),
     .   AJI4(MVSIZ), AJI5(MVSIZ), AJI6(MVSIZ),
     .   AJI7(MVSIZ), AJI8(MVSIZ), AJI9(MVSIZ),
     .   X17(MVSIZ) , X28(MVSIZ) , X35(MVSIZ) , X46(MVSIZ),
     .   Y17(MVSIZ) , Y28(MVSIZ) , Y35(MVSIZ) , Y46(MVSIZ),
     .   Z17(MVSIZ) , Z28(MVSIZ) , Z35(MVSIZ) , Z46(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .   AJ12(MVSIZ), AJ45(MVSIZ), AJ78(MVSIZ),
     .   A17(MVSIZ) , A28(MVSIZ) ,
     .   B17(MVSIZ) , B28(MVSIZ) ,
     .   C17(MVSIZ) , C28(MVSIZ)
C-----------------------------------------------
      DO 10 I=1,NEL
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
   10 Z46(I)=Z6(I)-Z4(I)
C
      DO 20 I=1,NEL
      AJ4(I)=X17(I)+X28(I)-X35(I)-X46(I)
      AJ5(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
      AJ6(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
      A17(I)=X17(I)+X46(I)
      A28(I)=X28(I)+X35(I)
      B17(I)=Y17(I)+Y46(I)
      B28(I)=Y28(I)+Y35(I)
      C17(I)=Z17(I)+Z46(I)
   20 C28(I)=Z28(I)+Z35(I)
      DO 30 I=1,NEL
      AJ7(I)=A17(I)+A28(I)
      AJ8(I)=B17(I)+B28(I)
      AJ9(I)=C17(I)+C28(I)
      AJ1(I)=A17(I)-A28(I)
      AJ2(I)=B17(I)-B28(I)
   30 AJ3(I)=C17(I)-C28(I)
C
C     JACOBIAN
C
      DO 100 I=1,NEL
      JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
      JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
      JAC_38_29(I)=(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
      JAC_19_37(I)=( AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
      JAC_27_18(I)=(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
      JAC_26_35(I)=( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
      JAC_34_16(I)=(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
      JAC_15_24(I)=( AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
  100 JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
C
      DO 110 I=1,NEL
  110 DET(I)=ONE_OVER_64*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
C
      CALL SCHKJAB3(
     1   OFF,     DET,     NGL,     NEL)
C
      DO 140 I=1,NEL
  140 DETT(I)=ONE_OVER_64/DET(I)
C
C     INVERSE DE LA MATRICE JACOBIENNE
C
      DO 150 I=1,NEL
      AJI1(I)=DETT(I)*JAC_59_68(I)
      AJI4(I)=DETT(I)*JAC_67_49(I)
      AJI7(I)=DETT(I)*JAC_48_57(I)
      AJI2(I)=DETT(I)*JAC_38_29(I)
      AJI5(I)=DETT(I)*JAC_19_37(I)
      AJI8(I)=DETT(I)*JAC_27_18(I)
      AJI3(I)=DETT(I)*JAC_26_35(I)
      AJI6(I)=DETT(I)*JAC_34_16(I)
      AJI9(I)=DETT(I)*JAC_15_24(I)
  150 CONTINUE
C
      DO 160 I=1,NEL
      AJ12(I)=AJI1(I)-AJI2(I)
      AJ45(I)=AJI4(I)-AJI5(I)
  160 AJ78(I)=AJI7(I)-AJI8(I)
      DO 170 I=1,NEL
      PX2(I)= AJ12(I)-AJI3(I)
      PY2(I)= AJ45(I)-AJI6(I)
      PZ2(I)= AJ78(I)-AJI9(I)
      PX4(I)=-AJ12(I)-AJI3(I)
      PY4(I)=-AJ45(I)-AJI6(I)
  170 PZ4(I)=-AJ78(I)-AJI9(I)
      DO 180 I=1,NEL
      AJ12(I)=AJI1(I)+AJI2(I)
      AJ45(I)=AJI4(I)+AJI5(I)
  180 AJ78(I)=AJI7(I)+AJI8(I)
      DO 190 I=1,NEL
      PX1(I)=-AJ12(I)-AJI3(I)
      PY1(I)=-AJ45(I)-AJI6(I)
      PZ1(I)=-AJ78(I)-AJI9(I)
      PX3(I)=AJ12(I)-AJI3(I)
      PY3(I)=AJ45(I)-AJI6(I)
  190 PZ3(I)=AJ78(I)-AJI9(I)
C
C mode 1
C 1 1 -1 -1 -1 -1 1 1
       DO I=1,NEL
         HX(I,1)=(X1(I)+X2(I)-X3(I)-X4(I)-X5(I)-X6(I)+X7(I)+X8(I))
         HY(I,1)=(Y1(I)+Y2(I)-Y3(I)-Y4(I)-Y5(I)-Y6(I)+Y7(I)+Y8(I))
         HZ(I,1)=(Z1(I)+Z2(I)-Z3(I)-Z4(I)-Z5(I)-Z6(I)+Z7(I)+Z8(I))
       ENDDO
C mode 2
C 1 -1 -1 1 -1 1 1 -1
       DO I=1,NEL
         HX(I,2)=(X1(I)-X2(I)-X3(I)+X4(I)-X5(I)+X6(I)+X7(I)-X8(I))
         HY(I,2)=(Y1(I)-Y2(I)-Y3(I)+Y4(I)-Y5(I)+Y6(I)+Y7(I)-Y8(I))
         HZ(I,2)=(Z1(I)-Z2(I)-Z3(I)+Z4(I)-Z5(I)+Z6(I)+Z7(I)-Z8(I))
       ENDDO
C mode 3
C 1 -1 1 -1 1 -1 1 -1
       DO I=1,NEL
         HX(I,3)=(X1(I)-X2(I)+X3(I)-X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY(I,3)=(Y1(I)-Y2(I)+Y3(I)-Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ(I,3)=(Z1(I)-Z2(I)+Z3(I)-Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))
       ENDDO
C mode 4
C -1 1 -1 1 1 -1 1 -1
       DO I=1,NEL
         HX(I,4)=(-X1(I)+X2(I)-X3(I)+X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY(I,4)=(-Y1(I)+Y2(I)-Y3(I)+Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ(I,4)=(-Z1(I)+Z2(I)-Z3(I)+Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))
       ENDDO
C       
C----surface max mediane-- *16
       DO I=1,NEL
         SMAX(I)= JAC_59_68(I)*JAC_59_68(I)+JAC_67_49(I)*JAC_67_49(I)
     .            +JAC_48_57(I)*JAC_48_57(I)
         SMAX(I)= MAX(SMAX(I),JAC_38_29(I)*JAC_38_29(I)+JAC_19_37(I)*JAC_19_37(I)
     .            +JAC_27_18(I)*JAC_27_18(I))
         SMAX(I)= MAX(SMAX(I),JAC_26_35(I)*JAC_26_35(I)+JAC_34_16(I)*JAC_34_16(I)
     .            +JAC_15_24(I)*JAC_15_24(I))
       ENDDO
       DO I=1,NEL
         IF(SMAX(I)<=ZERO)THEN
           CALL ANCMSG(MSGID=173,ANMODE=ANINFO,
     .                 I1=NGL(I))
           CALL ARRET(2)
         ENDIF
         SMAX(I)= ONE/SQRT(SMAX(I))
       ENDDO
      RETURN
C
 1000 FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB',I10/)
 2000 FORMAT(/' ZERO OR NEGATIVE VOLUME : DELETE 3D-ELEMENT NB',I10/)
      END 
