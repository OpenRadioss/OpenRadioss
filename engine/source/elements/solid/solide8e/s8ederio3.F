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
Chd|  S8EDERIO3                     source/elements/solid/solide8e/s8ederio3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        S8BICP11                      source/elements/solid/solide8e/s8bicp11.F
Chd|====================================================================
      SUBROUTINE S8EDERIO3(
     1   OFFG,    OFF,     VOL,     NGL,
     2   KSI,     ETA,     ZETA,    WI,
     3   HX,      HY,      HZ,      PX1,
     4   PX2,     PX3,     PX4,     PX5,
     5   PX6,     PX7,     PX8,     PY1,
     6   PY2,     PY3,     PY4,     PY5,
     7   PY6,     PY7,     PY8,     PZ1,
     8   PZ2,     PZ3,     PZ4,     PZ5,
     9   PZ6,     PZ7,     PZ8,     PXC1,
     A   PXC2,    PXC3,    PXC4,    PYC1,
     B   PYC2,    PYC3,    PYC4,    PZC1,
     C   PZC2,    PZC3,    PZC4,    BXY1,
     D   BXY2,    BXY3,    BXY4,    BXY5,
     E   BXY6,    BXY7,    BXY8,    BYX1,
     F   BYX2,    BYX3,    BYX4,    BYX5,
     G   BYX6,    BYX7,    BYX8,    BXZ1,
     H   BXZ2,    BXZ3,    BXZ4,    BXZ5,
     I   BXZ6,    BXZ7,    BXZ8,    BZX1,
     J   BZX2,    BZX3,    BZX4,    BZX5,
     K   BZX6,    BZX7,    BZX8,    BYZ1,
     L   BYZ2,    BYZ3,    BYZ4,    BYZ5,
     M   BYZ6,    BYZ7,    BYZ8,    BZY1,
     N   BZY2,    BZY3,    BZY4,    BZY5,
     O   BZY6,    BZY7,    BZY8,    BXX1,
     P   BXX2,    BXX3,    BXX4,    BXX5,
     Q   BXX6,    BXX7,    BXX8,    BYY1,
     R   BYY2,    BYY3,    BYY4,    BYY5,
     S   BYY6,    BYY7,    BYY8,    BZZ1,
     T   BZZ2,    BZZ3,    BZZ4,    BZZ5,
     U   BZZ6,    BZZ7,    BZZ8,    AJP1,
     V   AJP2,    AJP3,    AJP4,    AJP5,
     W   AJP6,    AJP7,    AJP8,    AJP9,
     X   AJ4,     AJ5,     AJ6,     AJ7,
     Y   AJ8,     AJ9,     PR,      PS,
     Z   PT,      SMAX,    DELTAX,  ICP,
     1   IDEG,    NU,      NEL)
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
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER ICP,IDEG(*)
C     REAL
      my_real
     .   OFF(*),OFFG(*),VOL(*),KSI,ETA,ZETA,WI,
     .   HX(MVSIZ,4),  HY(MVSIZ,4), HZ(MVSIZ,4),   
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PY5(*), PY6(*), PY7(*), PY8(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),
     .   AJP1(*),AJP2(*),AJP3(*),
     .   AJP4(*),AJP5(*),AJP6(*),
     .   AJP7(*),AJP8(*),AJP9(*),
     .   AJ4(*),AJ5(*),AJ6(*),
     .   AJ7(*),AJ8(*),AJ9(*),
     .   PR(8),PS(8),PT(8),
     .   SMAX(*),DELTAX(*),
     .   PXC1(*), PXC2(*), PXC3(*), PXC4(*),
     .   PYC1(*), PYC2(*), PYC3(*), PYC4(*),
     .   PZC1(*), PZC2(*), PZC3(*), PZC4(*),
     .   BXY1(*),BXY2(*),BXY3(*),BXY4(*),
     .   BXY5(*),BXY6(*),BXY7(*),BXY8(*),
     .   BYX1(*),BYX2(*),BYX3(*),BYX4(*),
     .   BYX5(*),BYX6(*),BYX7(*),BYX8(*),
     .   BXZ1(*),BXZ2(*),BXZ3(*),BXZ4(*),
     .   BXZ5(*),BXZ6(*),BXZ7(*),BXZ8(*),
     .   BZX1(*),BZX2(*),BZX3(*),BZX4(*),
     .   BZX5(*),BZX6(*),BZX7(*),BZX8(*),
     .   BYZ1(*),BYZ2(*),BYZ3(*),BYZ4(*),
     .   BYZ5(*),BYZ6(*),BYZ7(*),BYZ8(*),
     .   BZY1(*),BZY2(*),BZY3(*),BZY4(*),
     .   BZY5(*),BZY6(*),BZY7(*),BZY8(*),
     .   BXX1(*),BXX2(*),BXX3(*),BXX4(*),
     .   BXX5(*),BXX6(*),BXX7(*),BXX8(*),
     .   BYY1(*),BYY2(*),BYY3(*),BYY4(*),
     .   BYY5(*),BYY6(*),BYY7(*),BYY8(*),
     .   BZZ1(*),BZZ2(*),BZZ3(*),BZZ4(*),
     .   BZZ5(*),BZZ6(*),BZZ7(*),BZZ8(*),NU(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   DET(MVSIZ) ,DETT(MVSIZ) ,  
     .   AJ1(MVSIZ),AJ2(MVSIZ),AJ3(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .   AJI1(MVSIZ), AJI2(MVSIZ), AJI3(MVSIZ),
     .   AJI4(MVSIZ), AJI5(MVSIZ), AJI6(MVSIZ),
     .   AJI7(MVSIZ), AJI8(MVSIZ), AJI9(MVSIZ),
     .   AJ12(MVSIZ), AJ45(MVSIZ), AJ78(MVSIZ),
     .   A1PR1,A1PR3,A1PR5,A1PR7,
     .   A4PR1,A4PR3,A4PR5,A4PR7,
     .   A7PR1,A7PR3,A7PR5,A7PR7,
     .   A2PS1,A2PS2,A2PS5,A2PS6,
     .   A5PS1,A5PS2,A5PS5,A5PS6,
     .   A8PS1,A8PS2,A8PS5,A8PS6,
     .   A3PT1,A3PT2,A3PT3,A3PT4,
     .   A6PT1,A6PT2,A6PT3,A6PT4,
     .   A9PT1,A9PT2,A9PT3,A9PT4,
     .   BXHI,BYHI,BZHI,TMP,DETI
C-----------------------------------------------
       AJ1(1:NEL)=AJP1(1:NEL)
       AJ2(1:NEL)=AJP2(1:NEL)
       AJ3(1:NEL)=AJP3(1:NEL)
       AJ4(1:NEL)=AJP4(1:NEL)
       AJ5(1:NEL)=AJP5(1:NEL)
       AJ6(1:NEL)=AJP6(1:NEL)
       AJ7(1:NEL)=AJP7(1:NEL)
       AJ8(1:NEL)=AJP8(1:NEL)
       AJ9(1:NEL)=AJP9(1:NEL)

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
      ENDDO
C
      DO I=1,NEL
      DET(I)=ONE_OVER_512*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
      VOL(I)= WI*DET(I)
      DETT(I)=ONE_OVER_512/DET(I)
      ENDDO
C
      ICOR = 0
      DO I=1,NEL
        OFF(I)=OFFG(I)
        IF(OFF(I) ==ZERO)THEN
         DET(I)=ONE
         IF (VOL(I)<=ZERO) VOL(I)=ONE
        ELSEIF (VOL(I)<=ZERO) THEN
         VOL(I)= EM20
         OFF(I) =ZERO
          ICOR=1
        ENDIF
      ENDDO
C
      IF (ICOR>0.AND.IMPL_S>0) THEN
        DO I=1,NEL
          IF(VOL(I)<=ZERO.AND.OFF(I)/=ZERO)THEN
           IF (IMP_CHK>0) THEN
#include "lockon.inc"
            WRITE(IOUT ,2001) NGL(I)
#include "lockoff.inc"
            IDEL7NOK = 1
            IMP_IR = IMP_IR + 1
           ELSEIF (IMCONV==1) THEN
#include "lockon.inc"
            WRITE(ISTDO,2000) NGL(I)
            WRITE(IOUT ,2000) NGL(I)
#include "lockoff.inc"
            IDEL7NOK = 1   
           ENDIF 
          ENDIF
        ENDDO
      END IF
C
c      DO I=1,NEL
c        DETT(I)=ONE_OVER_512/DET(I)
c      ENDDO
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
cc-----pxI
      DO I=1,NEL
        A1PR1=AJI1(I)*PR(1)
        A1PR3=AJI1(I)*PR(3)
        A1PR5=AJI1(I)*PR(5)
        A1PR7=AJI1(I)*PR(7)
        A2PS1=AJI2(I)*PS(1)
        A2PS2=AJI2(I)*PS(2)
        A2PS5=AJI2(I)*PS(5)
        A2PS6=AJI2(I)*PS(6)
        A3PT1=AJI3(I)*PT(1)
        A3PT2=AJI3(I)*PT(2)
        A3PT3=AJI3(I)*PT(3)
        A3PT4=AJI3(I)*PT(4)
       PX1(I)= A1PR1+A2PS1+A3PT1
       PX2(I)=-A1PR1+A2PS2+A3PT2
       PX3(I)= A1PR3-A2PS2+A3PT3
       PX4(I)=-A1PR3-A2PS1+A3PT4
       PX5(I)= A1PR5+A2PS5-A3PT1
       PX6(I)=-A1PR5+A2PS6-A3PT2
       PX7(I)= A1PR7-A2PS6-A3PT3
       PX8(I)=-A1PR7-A2PS5-A3PT4
      ENDDO
C
      DO I=1,NEL
C
        A4PR1=AJI4(I)*PR(1)
        A4PR3=AJI4(I)*PR(3)
        A4PR5=AJI4(I)*PR(5)
        A4PR7=AJI4(I)*PR(7)
        A5PS1=AJI5(I)*PS(1)
        A5PS2=AJI5(I)*PS(2)
        A5PS5=AJI5(I)*PS(5)
        A5PS6=AJI5(I)*PS(6)
        A6PT1=AJI6(I)*PT(1)
        A6PT2=AJI6(I)*PT(2)
        A6PT3=AJI6(I)*PT(3)
        A6PT4=AJI6(I)*PT(4)
       PY1(I)= A4PR1+A5PS1+A6PT1
       PY2(I)=-A4PR1+A5PS2+A6PT2
       PY3(I)= A4PR3-A5PS2+A6PT3
       PY4(I)=-A4PR3-A5PS1+A6PT4
       PY5(I)= A4PR5+A5PS5-A6PT1
       PY6(I)=-A4PR5+A5PS6-A6PT2
       PY7(I)= A4PR7-A5PS6-A6PT3
       PY8(I)=-A4PR7-A5PS5-A6PT4
      ENDDO
C
      DO I=1,NEL
C
        A7PR1=AJI7(I)*PR(1)
        A7PR3=AJI7(I)*PR(3)
        A7PR5=AJI7(I)*PR(5)
        A7PR7=AJI7(I)*PR(7)
        A8PS1=AJI8(I)*PS(1)
        A8PS2=AJI8(I)*PS(2)
        A8PS5=AJI8(I)*PS(5)
        A8PS6=AJI8(I)*PS(6)
        A9PT1=AJI9(I)*PT(1)
        A9PT2=AJI9(I)*PT(2)
        A9PT3=AJI9(I)*PT(3)
        A9PT4=AJI9(I)*PT(4)
       PZ1(I)= A7PR1+A8PS1+A9PT1
       PZ2(I)=-A7PR1+A8PS2+A9PT2
       PZ3(I)= A7PR3-A8PS2+A9PT3
       PZ4(I)=-A7PR3-A8PS1+A9PT4
       PZ5(I)= A7PR5+A8PS5-A9PT1
       PZ6(I)=-A7PR5+A8PS6-A9PT2
       PZ7(I)= A7PR7-A8PS6-A9PT3
       PZ8(I)=-A7PR7-A8PS5-A9PT4
      ENDDO
C
      IF (IDTS6==0) THEN
       DO I=1,NEL
        DELTAX(I)=MIN(DELTAX(I),HUNDRED28*DET(I)*SMAX(I))
       ENDDO
      ELSE
      DO I=1,NEL
       IF (IDEG(I)==0) DELTAX(I)=MIN(DELTAX(I),HUNDRED28*DET(I)*SMAX(I))
      ENDDO
      END IF
C-----------for the imcompressible laws
C-------|BXX BYX BZX |
C-------|BXY BYY BZY |  same than H8A
C-------|BXZ BYZ BZZ |
      IF (ICP==11) THEN
       CALL S8BICP11(
     1   PX1,     PX2,     PX3,     PX4,
     2   PX5,     PX6,     PX7,     PX8,
     3   PY1,     PY2,     PY3,     PY4,
     4   PY5,     PY6,     PY7,     PY8,
     5   PZ1,     PZ2,     PZ3,     PZ4,
     6   PZ5,     PZ6,     PZ7,     PZ8,
     7   PXC1,    PXC2,    PXC3,    PXC4,
     8   PYC1,    PYC2,    PYC3,    PYC4,
     9   PZC1,    PZC2,    PZC3,    PZC4,
     A   BXY1,    BXY2,    BXY3,    BXY4,
     B   BXY5,    BXY6,    BXY7,    BXY8,
     C   BYX1,    BYX2,    BYX3,    BYX4,
     D   BYX5,    BYX6,    BYX7,    BYX8,
     E   BXZ1,    BXZ2,    BXZ3,    BXZ4,
     F   BXZ5,    BXZ6,    BXZ7,    BXZ8,
     G   BZX1,    BZX2,    BZX3,    BZX4,
     H   BZX5,    BZX6,    BZX7,    BZX8,
     I   BYZ1,    BYZ2,    BYZ3,    BYZ4,
     J   BYZ5,    BYZ6,    BYZ7,    BYZ8,
     K   BZY1,    BZY2,    BZY3,    BZY4,
     L   BZY5,    BZY6,    BZY7,    BZY8,
     M   BXX1,    BXX2,    BXX3,    BXX4,
     N   BXX5,    BXX6,    BXX7,    BXX8,
     O   BYY1,    BYY2,    BYY3,    BYY4,
     P   BYY5,    BYY6,    BYY7,    BYY8,
     Q   BZZ1,    BZZ2,    BZZ3,    BZZ4,
     R   BZZ5,    BZZ6,    BZZ7,    BZZ8,
     S   NU,      NEL)
      ENDIF
C
      RETURN
 2000 FORMAT(/' ZERO OR NEGATIVE SUB-VOLUME : DELETE 3D-ELEMENT NB',
     .          I10/)
 2001 FORMAT(/' ZERO OR NEGATIVE SOLID SUB-VOLUME : ELEMENT NB:',
     .          I10/)
      END
