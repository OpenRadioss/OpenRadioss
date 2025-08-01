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
!||    s8efint3   ../engine/source/elements/solid/solide8e/s8efint3.F
!||--- called by ------------------------------------------------------
!||    s8eforc3   ../engine/source/elements/solid/solide8e/s8eforc3.F
!||====================================================================
      SUBROUTINE S8EFINT3(
     1   SIG,     PX1,     PX2,     PX3,
     2   PX4,     PY1,     PY2,     PY3,
     3   PY4,     PZ1,     PZ2,     PZ3,
     4   PZ4,     PX5,     PX6,     PX7,
     5   PX8,     PY5,     PY6,     PY7,
     6   PY8,     PZ5,     PZ6,     PZ7,
     7   PZ8,     BXY1,    BXY2,    BXY3,
     8   BXY4,    BXY5,    BXY6,    BXY7,
     9   BXY8,    BYX1,    BYX2,    BYX3,
     A   BYX4,    BYX5,    BYX6,    BYX7,
     B   BYX8,    BXZ1,    BXZ2,    BXZ3,
     C   BXZ4,    BXZ5,    BXZ6,    BXZ7,
     D   BXZ8,    BZX1,    BZX2,    BZX3,
     E   BZX4,    BZX5,    BZX6,    BZX7,
     F   BZX8,    BYZ1,    BYZ2,    BYZ3,
     G   BYZ4,    BYZ5,    BYZ6,    BYZ7,
     H   BYZ8,    BZY1,    BZY2,    BZY3,
     I   BZY4,    BZY5,    BZY6,    BZY7,
     J   BZY8,    BXX1,    BXX2,    BXX3,
     K   BXX4,    BXX5,    BXX6,    BXX7,
     L   BXX8,    BYY1,    BYY2,    BYY3,
     M   BYY4,    BYY5,    BYY6,    BYY7,
     N   BYY8,    BZZ1,    BZZ2,    BZZ3,
     O   BZZ4,    BZZ5,    BZZ6,    BZZ7,
     P   BZZ8,    PXY1,    PXY2,    PXY3,
     Q   PXY4,    PXY5,    PXY6,    PXY7,
     R   PXY8,    PYX1,    PYX2,    PYX3,
     S   PYX4,    PYX5,    PYX6,    PYX7,
     T   PYX8,    PXZ1,    PXZ2,    PXZ3,
     U   PXZ4,    PXZ5,    PXZ6,    PXZ7,
     V   PXZ8,    PZX1,    PZX2,    PZX3,
     W   PZX4,    PZX5,    PZX6,    PZX7,
     X   PZX8,    PYZ1,    PYZ2,    PYZ3,
     Y   PYZ4,    PYZ5,    PYZ6,    PYZ7,
     Z   PYZ8,    PZY1,    PZY2,    PZY3,
     1   PZY4,    PZY5,    PZY6,    PZY7,
     2   PZY8,    F11,     F21,     F31,
     3   F12,     F22,     F32,     F13,
     4   F23,     F33,     F14,     F24,
     5   F34,     F15,     F25,     F35,
     6   F16,     F26,     F36,     F17,
     7   F27,     F37,     F18,     F28,
     8   F38,     VOL,     QVIS,    ICP,
     9   JFAC,    NEL,     I_SH,    IDEGE,
     A   SIG_A,   ISEL_V,  ISMSTR,  SVIS)
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
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER ICP,NEL,I_SH,IDEGE(*),ISEL_V
C     REAL
      my_real
     .   SIG(NEL,6),
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   PX5(*), PX6(*), PX7(*), PX8(*),  
     .   PY5(*), PY6(*), PY7(*), PY8(*),  
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),  
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
     .   BZZ5(*),BZZ6(*),BZZ7(*),BZZ8(*),
     .   PXY1(*),PXY2(*),PXY3(*),PXY4(*),
     .   PXY5(*),PXY6(*),PXY7(*),PXY8(*),
     .   PYX1(*),PYX2(*),PYX3(*),PYX4(*),
     .   PYX5(*),PYX6(*),PYX7(*),PYX8(*),
     .   PXZ1(*),PXZ2(*),PXZ3(*),PXZ4(*),
     .   PXZ5(*),PXZ6(*),PXZ7(*),PXZ8(*),
     .   PZX1(*),PZX2(*),PZX3(*),PZX4(*),
     .   PZX5(*),PZX6(*),PZX7(*),PZX8(*),
     .   PYZ1(*),PYZ2(*),PYZ3(*),PYZ4(*),
     .   PYZ5(*),PYZ6(*),PYZ7(*),PYZ8(*),
     .   PZY1(*),PZY2(*),PZY3(*),PZY4(*),
     .   PZY5(*),PZY6(*),PZY7(*),PZY8(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*),
     .   VOL(*),QVIS(*),JFAC(*),SIG_A(MVSIZ,3)
      my_real, DIMENSION(MVSIZ,6), INTENT(INOUT) :: SVIS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,IB
C     REAL
      my_real
     .   S1(MVSIZ), S2(MVSIZ), S3(MVSIZ),
     .   S4(MVSIZ), S5(MVSIZ), S6(MVSIZ),
     .   P(MVSIZ),COEF,FVOL
C-----------------------------------------------
      IF (ICP==1.AND.(ISMSTR==10.OR.ISMSTR==12)) THEN
       DO I=1,NEL
        FVOL=JFAC(I)*VOL(I)
        S1(I)=(SIG(I,1)+SVIS(I,1)-QVIS(I))*FVOL
        S2(I)=(SIG(I,2)+SVIS(I,2)-QVIS(I))*FVOL
        S3(I)=(SIG(I,3)+SVIS(I,3)-QVIS(I))*FVOL
        S4(I)=(SIG(I,4)+SVIS(I,4))*FVOL
        S5(I)=(SIG(I,5)+SVIS(I,5))*FVOL
        S6(I)=(SIG(I,6)+SVIS(I,6))*FVOL
       ENDDO
      ELSEIF (ICP==1) THEN
       COEF=ZEP3 
       DO I=1,NEL
        P(I) =COEF*(SIG(I,1)+SIG(I,2)+SIG(I,3)
     .            +SVIS(I,1)+SVIS(I,2)+SVIS(I,3))
        IF (IDEGE(I)>10) P(I) =QVIS(I)
        S1(I)=(SIG(I,1)+SVIS(I,1)-P(I))*VOL(I)
        S2(I)=(SIG(I,2)+SVIS(I,2)-P(I))*VOL(I)
        S3(I)=(SIG(I,3)+SVIS(I,3)-P(I))*VOL(I)
        S4(I)=(SIG(I,4)+SVIS(I,4))*VOL(I)
        S5(I)=(SIG(I,5)+SVIS(I,5))*VOL(I)
        S6(I)=(SIG(I,6)+SVIS(I,6))*VOL(I)
       ENDDO
      ELSE
      DO I=1,NEL
        S1(I)=(SIG(I,1)+SVIS(I,1)-QVIS(I))*VOL(I)
        S2(I)=(SIG(I,2)+SVIS(I,2)-QVIS(I))*VOL(I)
        S3(I)=(SIG(I,3)+SVIS(I,3)-QVIS(I))*VOL(I)
        S4(I)=(SIG(I,4)+SVIS(I,4))*VOL(I)
        S5(I)=(SIG(I,5)+SVIS(I,5))*VOL(I)
        S6(I)=(SIG(I,6)+SVIS(I,6))*VOL(I)
c        P(I) =THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3)
c     .            +SVIS(I,1)+SVIS(I,2)+SVIS(I,3))
       ENDDO
       IF (ISEL_V>0) THEN
         DO I=1,NEL
           S1(I)=S1(I)+SIG_A(I,1)*VOL(I)
           S2(I)=S2(I)+SIG_A(I,2)*VOL(I)
           S3(I)=S3(I)+SIG_A(I,3)*VOL(I)
         ENDDO
      ENDIF
      ENDIF
      IF (I_SH==0) THEN
      IF (ICP==11) THEN
       DO I=1,NEL
        F11(I)=F11(I)-(S1(I)*BXX1(I)+S4(I)*PY1(I)+S6(I)*PZ1(I))
     .               -(S2(I)*BXY1(I)+S3(I)*BXZ1(I))
        F21(I)=F21(I)-(S2(I)*BYY1(I)+S4(I)*PX1(I)+S5(I)*PZ1(I))
     .               -(S1(I)*BYX1(I)+S3(I)*BYZ1(I))
        F31(I)=F31(I)-(S3(I)*BZZ1(I)+S6(I)*PX1(I)+S5(I)*PY1(I))
     .               -(S1(I)*BZX1(I)+S2(I)*BZY1(I))
        F12(I)=F12(I)-(S1(I)*BXX2(I)+S4(I)*PY2(I)+S6(I)*PZ2(I))
     .               -(S2(I)*BXY2(I)+S3(I)*BXZ2(I))
        F22(I)=F22(I)-(S2(I)*BYY2(I)+S4(I)*PX2(I)+S5(I)*PZ2(I))
     .               -(S1(I)*BYX2(I)+S3(I)*BYZ2(I))
        F32(I)=F32(I)-(S3(I)*BZZ2(I)+S6(I)*PX2(I)+S5(I)*PY2(I))
     .               -(S1(I)*BZX2(I)+S2(I)*BZY2(I))
        F13(I)=F13(I)-(S1(I)*BXX3(I)+S4(I)*PY3(I)+S6(I)*PZ3(I))
     .               -(S2(I)*BXY3(I)+S3(I)*BXZ3(I))
        F23(I)=F23(I)-(S2(I)*BYY3(I)+S4(I)*PX3(I)+S5(I)*PZ3(I))
     .               -(S1(I)*BYX3(I)+S3(I)*BYZ3(I))
        F33(I)=F33(I)-(S3(I)*BZZ3(I)+S6(I)*PX3(I)+S5(I)*PY3(I))
     .               -(S1(I)*BZX3(I)+S2(I)*BZY3(I))
        F14(I)=F14(I)-(S1(I)*BXX4(I)+S4(I)*PY4(I)+S6(I)*PZ4(I))
     .               -(S2(I)*BXY4(I)+S3(I)*BXZ4(I))
        F24(I)=F24(I)-(S2(I)*BYY4(I)+S4(I)*PX4(I)+S5(I)*PZ4(I))
     .               -(S1(I)*BYX4(I)+S3(I)*BYZ4(I))
        F34(I)=F34(I)-(S3(I)*BZZ4(I)+S6(I)*PX4(I)+S5(I)*PY4(I))
     .               -(S1(I)*BZX4(I)+S2(I)*BZY4(I))
        F15(I)=F15(I)-(S1(I)*BXX5(I)+S4(I)*PY5(I)+S6(I)*PZ5(I))
     .               -(S2(I)*BXY5(I)+S3(I)*BXZ5(I))
        F25(I)=F25(I)-(S2(I)*BYY5(I)+S4(I)*PX5(I)+S5(I)*PZ5(I))
     .               -(S1(I)*BYX5(I)+S3(I)*BYZ5(I))
        F35(I)=F35(I)-(S3(I)*BZZ5(I)+S6(I)*PX5(I)+S5(I)*PY5(I))
     .               -(S1(I)*BZX5(I)+S2(I)*BZY5(I))
        F16(I)=F16(I)-(S1(I)*BXX6(I)+S4(I)*PY6(I)+S6(I)*PZ6(I))
     .               -(S2(I)*BXY6(I)+S3(I)*BXZ6(I))
        F26(I)=F26(I)-(S2(I)*BYY6(I)+S4(I)*PX6(I)+S5(I)*PZ6(I))
     .               -(S1(I)*BYX6(I)+S3(I)*BYZ6(I))
        F36(I)=F36(I)-(S3(I)*BZZ6(I)+S6(I)*PX6(I)+S5(I)*PY6(I))
     .               -(S1(I)*BZX6(I)+S2(I)*BZY6(I))
        F17(I)=F17(I)-(S1(I)*BXX7(I)+S4(I)*PY7(I)+S6(I)*PZ7(I))
     .               -(S2(I)*BXY7(I)+S3(I)*BXZ7(I))
        F27(I)=F27(I)-(S2(I)*BYY7(I)+S4(I)*PX7(I)+S5(I)*PZ7(I))
     .               -(S1(I)*BYX7(I)+S3(I)*BYZ7(I))
        F37(I)=F37(I)-(S3(I)*BZZ7(I)+S6(I)*PX7(I)+S5(I)*PY7(I))
     .               -(S1(I)*BZX7(I)+S2(I)*BZY7(I))
        F18(I)=F18(I)-(S1(I)*BXX8(I)+S4(I)*PY8(I)+S6(I)*PZ8(I))
     .               -(S2(I)*BXY8(I)+S3(I)*BXZ8(I))
        F28(I)=F28(I)-(S2(I)*BYY8(I)+S4(I)*PX8(I)+S5(I)*PZ8(I))
     .               -(S1(I)*BYX8(I)+S3(I)*BYZ8(I))
        F38(I)=F38(I)-(S3(I)*BZZ8(I)+S6(I)*PX8(I)+S5(I)*PY8(I))
     .               -(S1(I)*BZX8(I)+S2(I)*BZY8(I))
       ENDDO
      ELSE
       DO I=1,NEL
        F11(I)=F11(I)-(S1(I)*PX1(I)+S4(I)*PY1(I)+S6(I)*PZ1(I))
        F21(I)=F21(I)-(S2(I)*PY1(I)+S4(I)*PX1(I)+S5(I)*PZ1(I))
        F31(I)=F31(I)-(S3(I)*PZ1(I)+S6(I)*PX1(I)+S5(I)*PY1(I))
        F12(I)=F12(I)-(S1(I)*PX2(I)+S4(I)*PY2(I)+S6(I)*PZ2(I))
        F22(I)=F22(I)-(S2(I)*PY2(I)+S4(I)*PX2(I)+S5(I)*PZ2(I))
        F32(I)=F32(I)-(S3(I)*PZ2(I)+S6(I)*PX2(I)+S5(I)*PY2(I))
        F13(I)=F13(I)-(S1(I)*PX3(I)+S4(I)*PY3(I)+S6(I)*PZ3(I))
        F23(I)=F23(I)-(S2(I)*PY3(I)+S4(I)*PX3(I)+S5(I)*PZ3(I))
        F33(I)=F33(I)-(S3(I)*PZ3(I)+S6(I)*PX3(I)+S5(I)*PY3(I))
        F14(I)=F14(I)-(S1(I)*PX4(I)+S4(I)*PY4(I)+S6(I)*PZ4(I))
        F24(I)=F24(I)-(S2(I)*PY4(I)+S4(I)*PX4(I)+S5(I)*PZ4(I))
        F34(I)=F34(I)-(S3(I)*PZ4(I)+S6(I)*PX4(I)+S5(I)*PY4(I))
        F15(I)=F15(I)-(S1(I)*PX5(I)+S4(I)*PY5(I)+S6(I)*PZ5(I))
        F25(I)=F25(I)-(S2(I)*PY5(I)+S4(I)*PX5(I)+S5(I)*PZ5(I))
        F35(I)=F35(I)-(S3(I)*PZ5(I)+S6(I)*PX5(I)+S5(I)*PY5(I))
        F16(I)=F16(I)-(S1(I)*PX6(I)+S4(I)*PY6(I)+S6(I)*PZ6(I))
        F26(I)=F26(I)-(S2(I)*PY6(I)+S4(I)*PX6(I)+S5(I)*PZ6(I))
        F36(I)=F36(I)-(S3(I)*PZ6(I)+S6(I)*PX6(I)+S5(I)*PY6(I))
        F17(I)=F17(I)-(S1(I)*PX7(I)+S4(I)*PY7(I)+S6(I)*PZ7(I))
        F27(I)=F27(I)-(S2(I)*PY7(I)+S4(I)*PX7(I)+S5(I)*PZ7(I))
        F37(I)=F37(I)-(S3(I)*PZ7(I)+S6(I)*PX7(I)+S5(I)*PY7(I))
        F18(I)=F18(I)-(S1(I)*PX8(I)+S4(I)*PY8(I)+S6(I)*PZ8(I))
        F28(I)=F28(I)-(S2(I)*PY8(I)+S4(I)*PX8(I)+S5(I)*PZ8(I))
        F38(I)=F38(I)-(S3(I)*PZ8(I)+S6(I)*PX8(I)+S5(I)*PY8(I))
       ENDDO
      END IF !(ICP==11) THEN
C--------------free shear locking       
      ELSE
       IF (ICP==11) THEN
        DO I=1,NEL
        F11(I)=F11(I)-(S1(I)*BXX1(I)+S4(I)*PXY1(I)+S6(I)*PXZ1(I))
     .               -(S2(I)*BXY1(I)+S3(I)*BXZ1(I))
        F21(I)=F21(I)-(S2(I)*BYY1(I)+S4(I)*PYX1(I)+S5(I)*PYZ1(I))
     .               -(S1(I)*BYX1(I)+S3(I)*BYZ1(I))
        F31(I)=F31(I)-(S3(I)*BZZ1(I)+S6(I)*PZX1(I)+S5(I)*PZY1(I))
     .               -(S1(I)*BZX1(I)+S2(I)*BZY1(I))
        F12(I)=F12(I)-(S1(I)*BXX2(I)+S4(I)*PXY2(I)+S6(I)*PXZ2(I))
     .               -(S2(I)*BXY2(I)+S3(I)*BXZ2(I))
        F22(I)=F22(I)-(S2(I)*BYY2(I)+S4(I)*PYX2(I)+S5(I)*PYZ2(I))
     .               -(S1(I)*BYX2(I)+S3(I)*BYZ2(I))
        F32(I)=F32(I)-(S3(I)*BZZ2(I)+S6(I)*PZX2(I)+S5(I)*PZY2(I))
     .               -(S1(I)*BZX2(I)+S2(I)*BZY2(I))
        F13(I)=F13(I)-(S1(I)*BXX3(I)+S4(I)*PXY3(I)+S6(I)*PXZ3(I))
     .               -(S2(I)*BXY3(I)+S3(I)*BXZ3(I))
        F23(I)=F23(I)-(S2(I)*BYY3(I)+S4(I)*PYX3(I)+S5(I)*PYZ3(I))
     .               -(S1(I)*BYX3(I)+S3(I)*BYZ3(I))
        F33(I)=F33(I)-(S3(I)*BZZ3(I)+S6(I)*PZX3(I)+S5(I)*PZY3(I))
     .               -(S1(I)*BZX3(I)+S2(I)*BZY3(I))
        F14(I)=F14(I)-(S1(I)*BXX4(I)+S4(I)*PXY4(I)+S6(I)*PXZ4(I))
     .               -(S2(I)*BXY4(I)+S3(I)*BXZ4(I))
        F24(I)=F24(I)-(S2(I)*BYY4(I)+S4(I)*PYX4(I)+S5(I)*PYZ4(I))
     .               -(S1(I)*BYX4(I)+S3(I)*BYZ4(I))
        F34(I)=F34(I)-(S3(I)*BZZ4(I)+S6(I)*PZX4(I)+S5(I)*PZY4(I))
     .               -(S1(I)*BZX4(I)+S2(I)*BZY4(I))
        F15(I)=F15(I)-(S1(I)*BXX5(I)+S4(I)*PXY5(I)+S6(I)*PXZ5(I))
     .               -(S2(I)*BXY5(I)+S3(I)*BXZ5(I))
        F25(I)=F25(I)-(S2(I)*BYY5(I)+S4(I)*PYX5(I)+S5(I)*PYZ5(I))
     .               -(S1(I)*BYX5(I)+S3(I)*BYZ5(I))
        F35(I)=F35(I)-(S3(I)*BZZ5(I)+S6(I)*PZX5(I)+S5(I)*PZY5(I))
     .               -(S1(I)*BZX5(I)+S2(I)*BZY5(I))
        F16(I)=F16(I)-(S1(I)*BXX6(I)+S4(I)*PXY6(I)+S6(I)*PXZ6(I))
     .               -(S2(I)*BXY6(I)+S3(I)*BXZ6(I))
        F26(I)=F26(I)-(S2(I)*BYY6(I)+S4(I)*PYX6(I)+S5(I)*PYZ6(I))
     .               -(S1(I)*BYX6(I)+S3(I)*BYZ6(I))
        F36(I)=F36(I)-(S3(I)*BZZ6(I)+S6(I)*PZX6(I)+S5(I)*PZY6(I))
     .               -(S1(I)*BZX6(I)+S2(I)*BZY6(I))
        F17(I)=F17(I)-(S1(I)*BXX7(I)+S4(I)*PXY7(I)+S6(I)*PXZ7(I))
     .               -(S2(I)*BXY7(I)+S3(I)*BXZ7(I))
        F27(I)=F27(I)-(S2(I)*BYY7(I)+S4(I)*PYX7(I)+S5(I)*PYZ7(I))
     .               -(S1(I)*BYX7(I)+S3(I)*BYZ7(I))
        F37(I)=F37(I)-(S3(I)*BZZ7(I)+S6(I)*PZX7(I)+S5(I)*PZY7(I))
     .               -(S1(I)*BZX7(I)+S2(I)*BZY7(I))
        F18(I)=F18(I)-(S1(I)*BXX8(I)+S4(I)*PXY8(I)+S6(I)*PXZ8(I))
     .               -(S2(I)*BXY8(I)+S3(I)*BXZ8(I))
        F28(I)=F28(I)-(S2(I)*BYY8(I)+S4(I)*PYX8(I)+S5(I)*PYZ8(I))
     .               -(S1(I)*BYX8(I)+S3(I)*BYZ8(I))
        F38(I)=F38(I)-(S3(I)*BZZ8(I)+S6(I)*PZX8(I)+S5(I)*PZY8(I))
     .               -(S1(I)*BZX8(I)+S2(I)*BZY8(I))
        ENDDO
       ELSE
        DO I=1,NEL
        F11(I)=F11(I)-(S1(I)*PX1(I)+S4(I)*PXY1(I)+S6(I)*PXZ1(I))
        F21(I)=F21(I)-(S2(I)*PY1(I)+S4(I)*PYX1(I)+S5(I)*PYZ1(I))
        F31(I)=F31(I)-(S3(I)*PZ1(I)+S6(I)*PZX1(I)+S5(I)*PZY1(I))
        F12(I)=F12(I)-(S1(I)*PX2(I)+S4(I)*PXY2(I)+S6(I)*PXZ2(I))
        F22(I)=F22(I)-(S2(I)*PY2(I)+S4(I)*PYX2(I)+S5(I)*PYZ2(I))
        F32(I)=F32(I)-(S3(I)*PZ2(I)+S6(I)*PZX2(I)+S5(I)*PZY2(I))
        F13(I)=F13(I)-(S1(I)*PX3(I)+S4(I)*PXY3(I)+S6(I)*PXZ3(I))
        F23(I)=F23(I)-(S2(I)*PY3(I)+S4(I)*PYX3(I)+S5(I)*PYZ3(I))
        F33(I)=F33(I)-(S3(I)*PZ3(I)+S6(I)*PZX3(I)+S5(I)*PZY3(I))
        F14(I)=F14(I)-(S1(I)*PX4(I)+S4(I)*PXY4(I)+S6(I)*PXZ4(I))
        F24(I)=F24(I)-(S2(I)*PY4(I)+S4(I)*PYX4(I)+S5(I)*PYZ4(I))
        F34(I)=F34(I)-(S3(I)*PZ4(I)+S6(I)*PZX4(I)+S5(I)*PZY4(I))
        F15(I)=F15(I)-(S1(I)*PX5(I)+S4(I)*PXY5(I)+S6(I)*PXZ5(I))
        F25(I)=F25(I)-(S2(I)*PY5(I)+S4(I)*PYX5(I)+S5(I)*PYZ5(I))
        F35(I)=F35(I)-(S3(I)*PZ5(I)+S6(I)*PZX5(I)+S5(I)*PZY5(I))
        F16(I)=F16(I)-(S1(I)*PX6(I)+S4(I)*PXY6(I)+S6(I)*PXZ6(I))
        F26(I)=F26(I)-(S2(I)*PY6(I)+S4(I)*PYX6(I)+S5(I)*PYZ6(I))
        F36(I)=F36(I)-(S3(I)*PZ6(I)+S6(I)*PZX6(I)+S5(I)*PZY6(I))
        F17(I)=F17(I)-(S1(I)*PX7(I)+S4(I)*PXY7(I)+S6(I)*PXZ7(I))
        F27(I)=F27(I)-(S2(I)*PY7(I)+S4(I)*PYX7(I)+S5(I)*PYZ7(I))
        F37(I)=F37(I)-(S3(I)*PZ7(I)+S6(I)*PZX7(I)+S5(I)*PZY7(I))
        F18(I)=F18(I)-(S1(I)*PX8(I)+S4(I)*PXY8(I)+S6(I)*PXZ8(I))
        F28(I)=F28(I)-(S2(I)*PY8(I)+S4(I)*PYX8(I)+S5(I)*PYZ8(I))
        F38(I)=F38(I)-(S3(I)*PZ8(I)+S6(I)*PZX8(I)+S5(I)*PZY8(I))
        ENDDO
        IF (I_SH>1.OR.ISEL_V>0) THEN
        DO I=1,NEL
        IF(IDEGE(I)>10) CYCLE
        F11(I)=F11(I)-(S2(I)*BXY1(I)+S3(I)*BXZ1(I))
        F21(I)=F21(I)-(S1(I)*BYX1(I)+S3(I)*BYZ1(I))
        F31(I)=F31(I)-(S1(I)*BZX1(I)+S2(I)*BZY1(I))
        F12(I)=F12(I)-(S2(I)*BXY2(I)+S3(I)*BXZ2(I))
        F22(I)=F22(I)-(S1(I)*BYX2(I)+S3(I)*BYZ2(I))
        F32(I)=F32(I)-(S1(I)*BZX2(I)+S2(I)*BZY2(I))
        F13(I)=F13(I)-(S2(I)*BXY3(I)+S3(I)*BXZ3(I))
        F23(I)=F23(I)-(S1(I)*BYX3(I)+S3(I)*BYZ3(I))
        F33(I)=F33(I)-(S1(I)*BZX3(I)+S2(I)*BZY3(I))
        F14(I)=F14(I)-(S2(I)*BXY4(I)+S3(I)*BXZ4(I))
        F24(I)=F24(I)-(S1(I)*BYX4(I)+S3(I)*BYZ4(I))
        F34(I)=F34(I)-(S1(I)*BZX4(I)+S2(I)*BZY4(I))
        F15(I)=F15(I)-(S2(I)*BXY5(I)+S3(I)*BXZ5(I))
        F25(I)=F25(I)-(S1(I)*BYX5(I)+S3(I)*BYZ5(I))
        F35(I)=F35(I)-(S1(I)*BZX5(I)+S2(I)*BZY5(I))
        F16(I)=F16(I)-(S2(I)*BXY6(I)+S3(I)*BXZ6(I))
        F26(I)=F26(I)-(S1(I)*BYX6(I)+S3(I)*BYZ6(I))
        F36(I)=F36(I)-(S1(I)*BZX6(I)+S2(I)*BZY6(I))
        F17(I)=F17(I)-(S2(I)*BXY7(I)+S3(I)*BXZ7(I))
        F27(I)=F27(I)-(S1(I)*BYX7(I)+S3(I)*BYZ7(I))
        F37(I)=F37(I)-(S1(I)*BZX7(I)+S2(I)*BZY7(I))
        F18(I)=F18(I)-(S2(I)*BXY8(I)+S3(I)*BXZ8(I))
        F28(I)=F28(I)-(S1(I)*BYX8(I)+S3(I)*BYZ8(I))
        F38(I)=F38(I)-(S1(I)*BZX8(I)+S2(I)*BZY8(I))
        ENDDO
        END IF !(I_SH>1) THEN
       END IF !(ICP==11) THEN
      END IF! (I_SH==0) THEN
C   ---------------
      RETURN
      END
