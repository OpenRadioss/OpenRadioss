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
Chd|  S8ZJAC_I3                     source/elements/solid/solide8e/s8zjac_i3.F
Chd|-- called by -----------
Chd|        S8_IS17JAC_I                  source/elements/solid/solide8e/s8_is17jac_i.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ZJAC_I3(
     .    KSI,ETA,ZETA,WI,
     .    HX, HY, HZ,   
     .    CJ1,CJ2,CJ3,
     .    CJ4,CJ5,CJ6,
     .    CJ7,CJ8,CJ9,
     .    JAC_I,L_PIJ,PIJ,SIG,SIGL,INDEX,NB,NEL)
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
      INTEGER L_PIJ,NEL,INDEX(*),NB
      my_real
     .   KSI,ETA,ZETA,WI,
     .   HX(4,*), HY(4,*), HZ(4,*),  
     .   CJ1(*),CJ2(*),CJ3(*),
     .   CJ4(*),CJ5(*),CJ6(*),
     .   CJ7(*),CJ8(*),CJ9(*),
     .   JAC_I(10,*),PIJ(NEL,24),SIG(NEL,6),SIGL(NEL,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J
      my_real
     .   DET(MVSIZ) ,  DETT(MVSIZ) ,
     .   AJ1(MVSIZ),AJ2(MVSIZ),AJ3(MVSIZ),
     .   AJ4(MVSIZ),AJ5(MVSIZ),AJ6(MVSIZ),
     .   AJ7(MVSIZ),AJ8(MVSIZ),AJ9(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ)
      my_real
     .   AJI1(MVSIZ), AJI2(MVSIZ), AJI3(MVSIZ),
     .   AJI4(MVSIZ), AJI5(MVSIZ), AJI6(MVSIZ),
     .   AJI7(MVSIZ), AJI8(MVSIZ), AJI9(MVSIZ),
     .   A1PR1(MVSIZ),A1PR3(MVSIZ),A1PR5(MVSIZ),A1PR7(MVSIZ),
     .   A4PR1(MVSIZ),A4PR3(MVSIZ),A4PR5(MVSIZ),A4PR7(MVSIZ),
     .   A7PR1(MVSIZ),A7PR3(MVSIZ),A7PR5(MVSIZ),A7PR7(MVSIZ),
     .   A2PS1(MVSIZ),A2PS2(MVSIZ),A2PS5(MVSIZ),A2PS6(MVSIZ),
     .   A5PS1(MVSIZ),A5PS2(MVSIZ),A5PS5(MVSIZ),A5PS6(MVSIZ),
     .   A8PS1(MVSIZ),A8PS2(MVSIZ),A8PS5(MVSIZ),A8PS6(MVSIZ),
     .   A3PT1(MVSIZ),A3PT2(MVSIZ),A3PT3(MVSIZ),A3PT4(MVSIZ),
     .   A6PT1(MVSIZ),A6PT2(MVSIZ),A6PT3(MVSIZ),A6PT4(MVSIZ),
     .   A9PT1(MVSIZ),A9PT2(MVSIZ),A9PT3(MVSIZ),A9PT4(MVSIZ),
     .   PR(8),PS(8),PT(8),RP, SP, TP, RM, SM, TM,
     .   PX1, PX2, PX3, PX4,PX5, PX6, PX7, PX8,
     .   PY1, PY2, PY3, PY4,PY5, PY6, PY7, PY8,
     .   PZ1, PZ2, PZ3, PZ4,PZ5, PZ6, PZ7, PZ8,
     .   JAC_1(10,MVSIZ),PIJ12(24,NEL)
C-----------------------------------------------
      DO I=1,NB
       AJ1(I)=CJ1(I)+HX(3,I)*ETA+(HX(2,I)+HX(4,I)*ETA)*ZETA
       AJ2(I)=CJ2(I)+HY(3,I)*ETA+(HY(2,I)+HY(4,I)*ETA)*ZETA
       AJ3(I)=CJ3(I)+HZ(3,I)*ETA+(HZ(2,I)+HZ(4,I)*ETA)*ZETA
C
       AJ4(I)=CJ4(I)+HX(1,I)*ZETA+(HX(3,I)+HX(4,I)*ZETA)*KSI
       AJ5(I)=CJ5(I)+HY(1,I)*ZETA+(HY(3,I)+HY(4,I)*ZETA)*KSI
       AJ6(I)=CJ6(I)+HZ(1,I)*ZETA+(HZ(3,I)+HZ(4,I)*ZETA)*KSI
C
       AJ7(I)=CJ7(I)+HX(2,I)*KSI+(HX(1,I)+HX(4,I)*KSI)*ETA
       AJ8(I)=CJ8(I)+HY(2,I)*KSI+(HY(1,I)+HY(4,I)*KSI)*ETA
       AJ9(I)=CJ9(I)+HZ(2,I)*KSI+(HZ(1,I)+HZ(4,I)*KSI)*ETA
      ENDDO
C
C     JACOBIAN
C
      DO I=1,NB
      JAC_59_68(I)=AJ5(I)*AJ9(I)-AJ6(I)*AJ8(I)
      JAC_67_49(I)=AJ6(I)*AJ7(I)-AJ4(I)*AJ9(I)
      JAC_38_29(I)=(-AJ2(I)*AJ9(I)+AJ3(I)*AJ8(I))
      JAC_19_37(I)=( AJ1(I)*AJ9(I)-AJ3(I)*AJ7(I))
      JAC_27_18(I)=(-AJ1(I)*AJ8(I)+AJ2(I)*AJ7(I))
      JAC_26_35(I)=( AJ2(I)*AJ6(I)-AJ3(I)*AJ5(I))
      JAC_34_16(I)=(-AJ1(I)*AJ6(I)+AJ3(I)*AJ4(I))
      JAC_15_24(I)=( AJ1(I)*AJ5(I)-AJ2(I)*AJ4(I))
      JAC_48_57(I)=AJ4(I)*AJ8(I)-AJ5(I)*AJ7(I)
      ENDDO
C
      DO I=1,NB
      DET(I)=ONE_OVER_512*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
      DETT(I)=ONE_OVER_512/DET(I)
      ENDDO
C      
      IF (L_PIJ ==0) THEN
      DO I=1,NB
       JAC_1(1,I)=DETT(I)*JAC_59_68(I)
       JAC_1(4,I)=DETT(I)*JAC_67_49(I)
       JAC_1(7,I)=DETT(I)*JAC_48_57(I)
       JAC_1(2,I)=DETT(I)*JAC_38_29(I)
       JAC_1(5,I)=DETT(I)*JAC_19_37(I)
       JAC_1(8,I)=DETT(I)*JAC_27_18(I)
       JAC_1(3,I)=DETT(I)*JAC_26_35(I)
       JAC_1(6,I)=DETT(I)*JAC_34_16(I)
       JAC_1(9,I)=DETT(I)*JAC_15_24(I)
       JAC_1(10,I)=DET(I)
      ENDDO
#include "vectorize.inc"
          DO J=1,NB
            I = INDEX(J)
            JAC_I(1:10,I) = JAC_1(1:10,J)
          ENDDO      
      ELSE
C--------calculate Pij----      
      DO I=1,NB
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
C------NI,ksi-------
      RP = ONE + KSI
      SP = ONE + ETA
      TP = ONE + ZETA
      RM = ONE - KSI
      SM = ONE - ETA
      TM = ONE - ZETA
      PR(1)=-SM*TM
      PR(2)=-PR(1)
      PR(3)= SP*TM
      PR(4)=-PR(3)
      PR(5)=-SM*TP
      PR(6)=-PR(5)
      PR(7)= SP*TP
      PR(8)=-PR(7)
      PS(1)=-RM*TM
      PS(2)=-RP*TM
      PS(3)=-PS(2)
      PS(4)=-PS(1)
      PS(5)=-RM*TP
      PS(6)=-RP*TP
      PS(7)=-PS(6)
      PS(8)=-PS(5)
      PT(1)=-RM*SM
      PT(2)=-RP*SM
      PT(3)=-RP*SP
      PT(4)=-RM*SP
      PT(5)=-PT(1)
      PT(6)=-PT(2)
      PT(7)=-PT(3)
      PT(8)=-PT(4)
cc
      DO I=1,NB
        A1PR1(I)=AJI1(I)*PR(1)
        A1PR3(I)=AJI1(I)*PR(3)
        A1PR5(I)=AJI1(I)*PR(5)
        A1PR7(I)=AJI1(I)*PR(7)
        A4PR1(I)=AJI4(I)*PR(1)
        A4PR3(I)=AJI4(I)*PR(3)
        A4PR5(I)=AJI4(I)*PR(5)
        A4PR7(I)=AJI4(I)*PR(7)
        A7PR1(I)=AJI7(I)*PR(1)
        A7PR3(I)=AJI7(I)*PR(3)
        A7PR5(I)=AJI7(I)*PR(5)
        A7PR7(I)=AJI7(I)*PR(7)
      ENDDO
      DO I=1,NB
        A2PS1(I)=AJI2(I)*PS(1)
        A2PS2(I)=AJI2(I)*PS(2)
        A2PS5(I)=AJI2(I)*PS(5)
        A2PS6(I)=AJI2(I)*PS(6)
        A5PS1(I)=AJI5(I)*PS(1)
        A5PS2(I)=AJI5(I)*PS(2)
        A5PS5(I)=AJI5(I)*PS(5)
        A5PS6(I)=AJI5(I)*PS(6)
        A8PS1(I)=AJI8(I)*PS(1)
        A8PS2(I)=AJI8(I)*PS(2)
        A8PS5(I)=AJI8(I)*PS(5)
        A8PS6(I)=AJI8(I)*PS(6)
      ENDDO
C
      DO I=1,NB
        A3PT1(I)=AJI3(I)*PT(1)
        A3PT2(I)=AJI3(I)*PT(2)
        A3PT3(I)=AJI3(I)*PT(3)
        A3PT4(I)=AJI3(I)*PT(4)
        A6PT1(I)=AJI6(I)*PT(1)
        A6PT2(I)=AJI6(I)*PT(2)
        A6PT3(I)=AJI6(I)*PT(3)
        A6PT4(I)=AJI6(I)*PT(4)
        A9PT1(I)=AJI9(I)*PT(1)
        A9PT2(I)=AJI9(I)*PT(2)
        A9PT3(I)=AJI9(I)*PT(3)
        A9PT4(I)=AJI9(I)*PT(4)
      ENDDO
C
      DO I=1,NB
       PX1= A1PR1(I)+A2PS1(I)+A3PT1(I)       
       PX2=-A1PR1(I)+A2PS2(I)+A3PT2(I)
       PX3= A1PR3(I)-A2PS2(I)+A3PT3(I)
       PX4=-A1PR3(I)-A2PS1(I)+A3PT4(I)
       PX5= A1PR5(I)+A2PS5(I)-A3PT1(I)
       PX6=-A1PR5(I)+A2PS6(I)-A3PT2(I)
       PX7= A1PR7(I)-A2PS6(I)-A3PT3(I)
       PX8=-A1PR7(I)-A2PS5(I)-A3PT4(I)
C
       PY1= A4PR1(I)+A5PS1(I)+A6PT1(I)
       PY2=-A4PR1(I)+A5PS2(I)+A6PT2(I)
       PY3= A4PR3(I)-A5PS2(I)+A6PT3(I)
       PY4=-A4PR3(I)-A5PS1(I)+A6PT4(I)
       PY5= A4PR5(I)+A5PS5(I)-A6PT1(I)
       PY6=-A4PR5(I)+A5PS6(I)-A6PT2(I)
       PY7= A4PR7(I)-A5PS6(I)-A6PT3(I)
       PY8=-A4PR7(I)-A5PS5(I)-A6PT4(I)
C
       PZ1= A7PR1(I)+A8PS1(I)+A9PT1(I)
       PZ2=-A7PR1(I)+A8PS2(I)+A9PT2(I)
       PZ3= A7PR3(I)-A8PS2(I)+A9PT3(I)
       PZ4=-A7PR3(I)-A8PS1(I)+A9PT4(I)
       PZ5= A7PR5(I)+A8PS5(I)-A9PT1(I)
       PZ6=-A7PR5(I)+A8PS6(I)-A9PT2(I)
       PZ7= A7PR7(I)-A8PS6(I)-A9PT3(I)
       PZ8=-A7PR7(I)-A8PS5(I)-A9PT4(I)
C
       PIJ12(1,I) = PX1
       PIJ12(2,I) = PY1
       PIJ12(3,I) = PZ1
       PIJ12(4,I) = PX2
       PIJ12(5,I) = PY2
       PIJ12(6,I) = PZ2
       PIJ12(7,I) = PX3
       PIJ12(8,I) = PY3
       PIJ12(9,I) = PZ3
       PIJ12(10,I) = PX4
       PIJ12(11,I) = PY4
       PIJ12(12,I) = PZ4
       PIJ12(13,I) = PX5
       PIJ12(14,I) = PY5
       PIJ12(15,I) = PZ5
       PIJ12(16,I) = PX6
       PIJ12(17,I) = PY6
       PIJ12(18,I) = PZ6
       PIJ12(19,I) = PX7
       PIJ12(20,I) = PY7
       PIJ12(21,I) = PZ7
       PIJ12(22,I) = PX8
       PIJ12(23,I) = PY8
       PIJ12(24,I) = PZ8
      ENDDO
#include "vectorize.inc"
          DO J=1,NB
            I = INDEX(J)
            PIJ(I,1:24) = PIJ12(1:24,J)
          ENDDO      
      END IF !(L_JAC >0) THEN
#include "vectorize.inc"
          DO J=1,NB
            I = INDEX(J)
            SIGL(I,1:6) = SIG(I,1:6)
          ENDDO      
C
      RETURN
      END
