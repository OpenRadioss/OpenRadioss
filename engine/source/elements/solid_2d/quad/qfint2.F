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
!||    qfint2   ../engine/source/elements/solid_2d/quad/qfint2.F
!||--- called by ------------------------------------------------------
!||    bforc2   ../engine/source/ale/bimat/bforc2.F
!||    qforc2   ../engine/source/elements/solid_2d/quad/qforc2.F
!||====================================================================
      SUBROUTINE QFINT2(
     1   SIG,     PY1,     PY2,     PZ1,
     2   PZ2,     AREA,    VOL,     QVIS,
     3   F11,     F12,     F21,     F22,
     4   AX1,     AX2,     R22,     R23,
     5   R32,     R33,     NEL,     JCVT,
     6   SVIS)
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
      INTEGER, INTENT(IN) :: JCVT
      INTEGER NEL
      my_real
     .   SIG(NEL,6),
     .   PY1(*), PY2(*), PZ1(*), PZ2(*), AREA(*), VOL(*), QVIS(*),
     .   F11(*), F12(*), F21(*), F22(*), AX1(*),AX2(*),
     .   R22(*), R23(*), R32(*), R33(*)
      my_real, DIMENSION(MVSIZ,6), INTENT(INOUT) :: SVIS
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   FAC(MVSIZ), S1(MVSIZ), S2(MVSIZ), S4(MVSIZ),
     .   T1,T2,T3,T4
C-----------------------------------------------
      IF (N2D==1) THEN
      DO I=1,NEL
        FAC(I)=AREA(I)*AREA(I)/ MAX(EM15,VOL(I))/FOUR
      ENDDO
      ELSE
      DO I=1,NEL
        FAC(I)=ZERO
      ENDDO
      ENDIF

      IF(JCVT==0)THEN
       DO I=1,NEL
         AX1(I)=(SIG(I,3)+SVIS(I,3)-SIG(I,1)-SVIS(I,1))*FAC(I)
         AX2(I)=(SIG(I,4)+SVIS(I,4))*FAC(I)
       END DO
      ELSE
       DO I=1,NEL
         S1(I)=SIG(I,1)+SVIS(I,1)
         S2(I)=SIG(I,2)+SVIS(I,2)
         S4(I)=SIG(I,4)+SVIS(I,4)
         T1=S1(I)*R22(I)+S4(I)*R23(I)
         T2=S4(I)*R32(I)+S2(I)*R33(I)
         T3=S1(I)*R32(I)+S4(I)*R33(I)
         T4=S4(I)*R22(I)+S2(I)*R23(I)
         S1(I)=R22(I)*T1+R23(I)*T4
         S2(I)=R32(I)*T3+R33(I)*T2
         S4(I)=R22(I)*T3+R23(I)*T2
       END DO
       DO I=1,NEL
         AX1(I)= (SIG(I,3)+SVIS(I,3)-S1(I))*FAC(I)
         AX2(I)= S4(I)*FAC(I)
       END DO
      END IF

      DO I=1,NEL
        S1(I) = SIG(I,1)+SVIS(I,1)-QVIS(I)
        S2(I) = SIG(I,2)+SVIS(I,2)-QVIS(I)
        S4(I) = SIG(I,4)+SVIS(I,4)
        F11(I)=S1(I)*PY1(I)+S4(I)*PZ1(I)
        F21(I)=S2(I)*PZ1(I)+S4(I)*PY1(I)
        F12(I)=S1(I)*PY2(I)+S4(I)*PZ2(I)
        F22(I)=S2(I)*PZ2(I)+S4(I)*PY2(I)
      ENDDO

      RETURN
      END
