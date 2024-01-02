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
Chd|  S8EJACIT3                     source/elements/solid/solide8e/s8ejacit3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EJACIT3(
     1   VOLDP,   KSI,     ETA,     ZETA,
     2   WI,      AJP1,    AJP2,    AJP3,
     3   AJP4,    AJP5,    AJP6,    AJP7,
     4   AJP8,    AJP9,    AJI1,    AJI2,
     5   AJI3,    AJI4,    AJI5,    AJI6,
     6   AJI7,    AJI8,    AJI9,    JAC_I,
     7   NEL,     ISMSTR)
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
      INTEGER, INTENT(IN) :: ISMSTR
C     REAL
      my_real
     .   KSI,ETA,ZETA,WI,
     .   AJP1(*),AJP2(*),AJP3(*),
     .   AJP4(*),AJP5(*),AJP6(*),
     .   AJP7(*),AJP8(*),AJP9(*),
     .   AJI1(MVSIZ), AJI2(MVSIZ), AJI3(MVSIZ),
     .   AJI4(MVSIZ), AJI5(MVSIZ), AJI6(MVSIZ),
     .   AJI7(MVSIZ), AJI8(MVSIZ), AJI9(MVSIZ),
     .   JAC_I(10,MVSIZ)
      DOUBLE PRECISION 
     .   VOLDP(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   DET(MVSIZ) ,DETT(MVSIZ) ,  
     .   AJ1(MVSIZ),AJ2(MVSIZ),AJ3(MVSIZ),
     .   AJ4(MVSIZ),AJ5(MVSIZ),AJ6(MVSIZ),
     .   AJ7(MVSIZ),AJ8(MVSIZ),AJ9(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC_38_29(MVSIZ), JAC_19_37(MVSIZ), JAC_27_18(MVSIZ),
     .   JAC_26_35(MVSIZ), JAC_34_16(MVSIZ), JAC_15_24(MVSIZ),
     .   AJ12(MVSIZ), AJ45(MVSIZ), AJ78(MVSIZ)
      DOUBLE PRECISION 
     .   DETDP
C-----------------------------------------------
      IF (ISMSTR==11) THEN
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
      DETDP=ONE_OVER_512*(AJ1(I)*JAC_59_68(I)+AJ2(I)*JAC_67_49(I)+AJ3(I)*JAC_48_57(I))
      DET(I) = DETDP
      VOLDP(I)= WI*DETDP
      DETT(I)=ONE_OVER_512/DET(I)
      ENDDO
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
C---------don't forget this for Ismstr11 and R,S,T
c      DO I=1,NEL
c       IF (IDEG(I)==0) DELTAX(I)=MIN(DELTAX(I),HUNDRED28*DET(I)*SMAX(I))
c      ENDDO
C------ ISMSTR =10
      ELSE
      DO I=1,NEL
       AJI1(I)=JAC_I(1,I)
       AJI4(I)=JAC_I(4,I)
       AJI7(I)=JAC_I(7,I)
       AJI2(I)=JAC_I(2,I)
       AJI5(I)=JAC_I(5,I)
       AJI8(I)=JAC_I(8,I)
       AJI3(I)=JAC_I(3,I)
       AJI6(I)=JAC_I(6,I)
       AJI9(I)=JAC_I(9,I)
       VOLDP(I) =JAC_I(10,I)
      END DO
      END IF !(ISMSTR==11)
C
      RETURN
      END
