Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||    s8ederic3     ../engine/source/elements/solid/solide8e/s8ederic3.F
!||--- called by ------------------------------------------------------
!||    s8eforc3      ../engine/source/elements/solid/solide8e/s8eforc3.F
!||    s8sforc3      ../engine/source/elements/solid/solide8s/s8sforc3.F
!||--- calls      -----------------------------------------------------
!||    ancmsg        ../engine/source/output/message/message.F
!||    arret         ../engine/source/system/arret.F
!||    schkjabt3     ../engine/source/elements/solid/solide4/schkjabt3.F
!||--- uses       -----------------------------------------------------
!||    message_mod   ../engine/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE S8EDERIC3(
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
     B   PX1H2,   PX1H3,   PX1H4,   PX2H1,
     C   PX2H2,   PX2H3,   PX2H4,   PX3H1,
     D   PX3H2,   PX3H3,   PX3H4,   PX4H1,
     E   PX4H2,   PX4H3,   PX4H4,   HX,
     F   HY,      HZ,      JR_1,    JS_1,
     G   JT_1,    AJ1,     AJ2,     AJ3,
     H   AJ4,     AJ5,     AJ6,     AJ7,
     I   AJ8,     AJ9,     SMAX,    SAV,
     J   OFFG,    NEL,     ISMSTR,  JLAG)
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
      INTEGER, INTENT(IN) :: JLAG
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER NEL
      DOUBLE PRECISION X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .                 Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),
     .                 Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .                 SAV(NEL,21)

      my_real OFF(*),DET(*),
     .        PX1(*), PX2(*), PX3(*), PX4(*),
     .        PY1(*), PY2(*), PY3(*), PY4(*),
     .        PZ1(*), PZ2(*), PZ3(*), PZ4(*),
     .        PX1H1(*), PX1H2(*), PX1H3(*),PX1H4(*),
     .        PX2H1(*), PX2H2(*), PX2H3(*),PX2H4(*),
     .        PX3H1(*), PX3H2(*), PX3H3(*),PX3H4(*),
     .        PX4H1(*), PX4H2(*), PX4H3(*),PX4H4(*),
     .        HX(MVSIZ,4),  HY(MVSIZ,4), HZ(MVSIZ,4),
     .        JR_1(*),JS_1(*),JT_1(*),
     .        AJ1(*),AJ2(*),AJ3(*),
     .        AJ4(*),AJ5(*),AJ6(*),
     .        AJ7(*),AJ8(*),AJ9(*),SMAX(*),OFFG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR,NNEGA,INDEX(MVSIZ)
      my_real DETT(MVSIZ) ,
     .        AJI1(MVSIZ), AJI2(MVSIZ), AJI3(MVSIZ),
     .        AJI4(MVSIZ), AJI5(MVSIZ), AJI6(MVSIZ),
     .        AJI7(MVSIZ), AJI8(MVSIZ), AJI9(MVSIZ),
     .        X17(MVSIZ) , X28(MVSIZ) , X35(MVSIZ) , X46(MVSIZ),
     .        Y17(MVSIZ) , Y28(MVSIZ) , Y35(MVSIZ) , Y46(MVSIZ),
     .        Z17(MVSIZ) , Z28(MVSIZ) , Z35(MVSIZ) , Z46(MVSIZ),
     .        JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .        JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .        JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .        AJ12(MVSIZ), AJ45(MVSIZ), AJ78(MVSIZ),
     .        A17(MVSIZ) , A28(MVSIZ) ,
     .        B17(MVSIZ) , B28(MVSIZ) ,
     .        C17(MVSIZ) , C28(MVSIZ)
C-----------------------------------------------
      DO I=1,NEL
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
      ENDDO

      DO I=1,NEL
        AJ7(I)=A17(I)+A28(I)
        AJ8(I)=B17(I)+B28(I)
        AJ9(I)=C17(I)+C28(I)
        AJ1(I)=A17(I)-A28(I)
        AJ2(I)=B17(I)-B28(I)
        AJ3(I)=C17(I)-C28(I)
      ENDDO
C
C     JACOBIAN
C
      DO I=1,NEL
        JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
        JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
        JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
      ENDDO
C
      DO I=1,NEL
        DET(I)=ONE_OVER_64*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
      ENDDO
C
      CALL SCHKJABT3(
     1   OFF,     DET,     NGL,     OFFG,
     2   NNEGA,   INDEX,   NEL,     ISMSTR,
     3   JLAG)
      IF (NNEGA>0) THEN
        IF (ISMSTR==10.OR.ISMSTR==12) THEN
#include "vectorize.inc"
         DO J=1,NNEGA
          I = INDEX(J)
            X1(I)=SAV(I,1)
            Y1(I)=SAV(I,8)
            Z1(I)=SAV(I,15)
            X2(I)=SAV(I,2)
            Y2(I)=SAV(I,9)
            Z2(I)=SAV(I,16)
            X3(I)=SAV(I,3)
            Y3(I)=SAV(I,10)
            Z3(I)=SAV(I,17)
            X4(I)=SAV(I,4)
            Y4(I)=SAV(I,11)
            Z4(I)=SAV(I,18)
            X5(I)=SAV(I,5)
            Y5(I)=SAV(I,12)
            Z5(I)=SAV(I,19)
            X6(I)=SAV(I,6)
            Y6(I)=SAV(I,13)
            Z6(I)=SAV(I,20)
            X7(I)=SAV(I,7)
            Y7(I)=SAV(I,14)
            Z7(I)=SAV(I,21)
            X8(I)=ZERO
            Y8(I)=ZERO
            Z8(I)=ZERO
         ENDDO
        ELSE
#include "vectorize.inc"
         DO J=1,NNEGA
          I = INDEX(J)
            X1(I)=SAV(I,1)
            Y1(I)=SAV(I,2)
            Z1(I)=SAV(I,3)
            X2(I)=SAV(I,4)
            Y2(I)=SAV(I,5)
            Z2(I)=SAV(I,6)
            X3(I)=SAV(I,7)
            Y3(I)=SAV(I,8)
            Z3(I)=SAV(I,9)
            X4(I)=SAV(I,10)
            Y4(I)=SAV(I,11)
            Z4(I)=SAV(I,12)
            X5(I)=SAV(I,13)
            Y5(I)=SAV(I,14)
            Z5(I)=SAV(I,15)
            X6(I)=SAV(I,16)
            Y6(I)=SAV(I,17)
            Z6(I)=SAV(I,18)
            X7(I)=SAV(I,19)
            Y7(I)=SAV(I,20)
            Z7(I)=SAV(I,21)
            X8(I)=ZERO
            Y8(I)=ZERO
            Z8(I)=ZERO
         ENDDO
        END IF
#include "vectorize.inc"
         DO J=1,NNEGA
          I = INDEX(J)
C
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
C
            AJ4(I)=X17(I)+X28(I)-X35(I)-X46(I)
            AJ5(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
            AJ6(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
            A17(I)=X17(I)+X46(I)
            A28(I)=X28(I)+X35(I)
            B17(I)=Y17(I)+Y46(I)
            B28(I)=Y28(I)+Y35(I)
            C17(I)=Z17(I)+Z46(I)
            C28(I)=Z28(I)+Z35(I)
            AJ7(I)=A17(I)+A28(I)
            AJ8(I)=B17(I)+B28(I)
            AJ9(I)=C17(I)+C28(I)
            AJ1(I)=A17(I)-A28(I)
            AJ2(I)=B17(I)-B28(I)
            AJ3(I)=C17(I)-C28(I)
C      
            JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
            JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
            JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
C
           DET(I)=ONE_OVER_64*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
             OFFG(I) = TWO
         ENDDO
      END IF
C      
      DO I=1,NEL
      JAC_38_29(I)=(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
      JAC_19_37(I)=( AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
      JAC_27_18(I)=(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
      JAC_26_35(I)=( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
      JAC_34_16(I)=(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
      JAC_15_24(I)=( AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
      END DO
C
      DO I=1,NEL
        DETT(I)=ONE_OVER_64/DET(I)
      ENDDO
C
C     INVERSE DE LA MATRICE JACOBIENNE
C
      DO I=1,NEL
        AJI1(I)=DETT(I)*JAC_59_68(I)
        AJI4(I)=DETT(I)*JAC_67_49(I)
        AJI7(I)=DETT(I)*JAC_48_57(I)
        AJI2(I)=DETT(I)*JAC_38_29(I)
        AJI5(I)=DETT(I)*JAC_19_37(I)
        AJI8(I)=DETT(I)*JAC_27_18(I)
        AJI3(I)=DETT(I)*JAC_26_35(I)
        AJI6(I)=DETT(I)*JAC_34_16(I)
        AJI9(I)=DETT(I)*JAC_15_24(I)
      ENDDO
C
      DO I=1,NEL
        AJ12(I)=AJI1(I)-AJI2(I)
        AJ45(I)=AJI4(I)-AJI5(I)
        AJ78(I)=AJI7(I)-AJI8(I)
      ENDDO
      DO I=1,NEL
        PX2(I)= AJ12(I)-AJI3(I)
        PY2(I)= AJ45(I)-AJI6(I)
        PZ2(I)= AJ78(I)-AJI9(I)
        PX4(I)=-AJ12(I)-AJI3(I)
        PY4(I)=-AJ45(I)-AJI6(I)
        PZ4(I)=-AJ78(I)-AJI9(I)
      ENDDO
      DO I=1,NEL
        AJ12(I)=AJI1(I)+AJI2(I)
        AJ45(I)=AJI4(I)+AJI5(I)
        AJ78(I)=AJI7(I)+AJI8(I)
      ENDDO
      DO I=1,NEL
        PX1(I)=-AJ12(I)-AJI3(I)
        PY1(I)=-AJ45(I)-AJI6(I)
        PZ1(I)=-AJ78(I)-AJI9(I)
        PX3(I)=AJ12(I)-AJI3(I)
        PY3(I)=AJ45(I)-AJI6(I)
        PZ3(I)=AJ78(I)-AJI9(I)
      ENDDO
C
C---- for f_vis  
      JR_1(1:NEL)=AJI2(1:NEL)
      JS_1(1:NEL)=AJI6(1:NEL)
      JT_1(1:NEL)=AJI7(1:NEL)
C
C mode 1
C 1 1 -1 -1 -1 -1 1 1
       DO I=1,NEL
         HX(I,1)=(X1(I)+X2(I)-X3(I)-X4(I)-X5(I)-X6(I)+X7(I)+X8(I))
         HY(I,1)=(Y1(I)+Y2(I)-Y3(I)-Y4(I)-Y5(I)-Y6(I)+Y7(I)+Y8(I))
         HZ(I,1)=(Z1(I)+Z2(I)-Z3(I)-Z4(I)-Z5(I)-Z6(I)+Z7(I)+Z8(I))
         PX1H1(I)=PX1(I)*HX(I,1)+ PY1(I)*HY(I,1)+PZ1(I)*HZ(I,1)
         PX2H1(I)=PX2(I)*HX(I,1)+ PY2(I)*HY(I,1)+PZ2(I)*HZ(I,1)
         PX3H1(I)=PX3(I)*HX(I,1)+ PY3(I)*HY(I,1)+PZ3(I)*HZ(I,1)
         PX4H1(I)=PX4(I)*HX(I,1)+ PY4(I)*HY(I,1)+PZ4(I)*HZ(I,1)
       ENDDO
C mode 2
C 1 -1 -1 1 -1 1 1 -1
       DO I=1,NEL
         HX(I,2)=(X1(I)-X2(I)-X3(I)+X4(I)-X5(I)+X6(I)+X7(I)-X8(I))
         HY(I,2)=(Y1(I)-Y2(I)-Y3(I)+Y4(I)-Y5(I)+Y6(I)+Y7(I)-Y8(I))
         HZ(I,2)=(Z1(I)-Z2(I)-Z3(I)+Z4(I)-Z5(I)+Z6(I)+Z7(I)-Z8(I))
         PX1H2(I)=PX1(I)*HX(I,2)+ PY1(I)*HY(I,2)+PZ1(I)*HZ(I,2)
         PX2H2(I)=PX2(I)*HX(I,2)+ PY2(I)*HY(I,2)+PZ2(I)*HZ(I,2)
         PX3H2(I)=PX3(I)*HX(I,2)+ PY3(I)*HY(I,2)+PZ3(I)*HZ(I,2)
         PX4H2(I)=PX4(I)*HX(I,2)+ PY4(I)*HY(I,2)+PZ4(I)*HZ(I,2)
       ENDDO
C mode 3
C 1 -1 1 -1 1 -1 1 -1
       DO I=1,NEL
         HX(I,3)=(X1(I)-X2(I)+X3(I)-X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY(I,3)=(Y1(I)-Y2(I)+Y3(I)-Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ(I,3)=(Z1(I)-Z2(I)+Z3(I)-Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))
         PX1H3(I)=PX1(I)*HX(I,3)+ PY1(I)*HY(I,3)+PZ1(I)*HZ(I,3)
         PX2H3(I)=PX2(I)*HX(I,3)+ PY2(I)*HY(I,3)+PZ2(I)*HZ(I,3)
         PX3H3(I)=PX3(I)*HX(I,3)+ PY3(I)*HY(I,3)+PZ3(I)*HZ(I,3)
         PX4H3(I)=PX4(I)*HX(I,3)+ PY4(I)*HY(I,3)+PZ4(I)*HZ(I,3)
       ENDDO
C mode 4
C -1 1 -1 1 1 -1 1 -1
       DO I=1,NEL
         HX(I,4)=(-X1(I)+X2(I)-X3(I)+X4(I)+X5(I)-X6(I)+X7(I)-X8(I))
         HY(I,4)=(-Y1(I)+Y2(I)-Y3(I)+Y4(I)+Y5(I)-Y6(I)+Y7(I)-Y8(I))
         HZ(I,4)=(-Z1(I)+Z2(I)-Z3(I)+Z4(I)+Z5(I)-Z6(I)+Z7(I)-Z8(I))
         PX1H4(I)=PX1(I)*HX(I,4)+ PY1(I)*HY(I,4)+PZ1(I)*HZ(I,4)
         PX2H4(I)=PX2(I)*HX(I,4)+ PY2(I)*HY(I,4)+PZ2(I)*HZ(I,4)
         PX3H4(I)=PX3(I)*HX(I,4)+ PY3(I)*HY(I,4)+PZ3(I)*HZ(I,4)
         PX4H4(I)=PX4(I)*HX(I,4)+ PY4(I)*HY(I,4)+PZ4(I)*HZ(I,4)
       ENDDO
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
           CALL ANCMSG(MSGID=173,ANMODE=ANINFO,I1=NGL(I))
           CALL ARRET(2)
         ENDIF
         SMAX(I)= ONE/SQRT(SMAX(I))
       ENDDO
      RETURN
C
 1000 FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB',I10/)
 2000 FORMAT(/' ZERO OR NEGATIVE VOLUME : DELETE 3D-ELEMENT NB',I10/)
      END
