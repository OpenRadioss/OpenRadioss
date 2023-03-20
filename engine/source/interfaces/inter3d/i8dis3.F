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
Chd|  I8DIS3                        source/interfaces/inter3d/i8dis3.F
Chd|-- called by -----------
Chd|        INTVO8                        source/interfaces/inter3d/intvo8.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I8DIS3(IGIMP,NTY,DIST,
     .                  X1,Y1,Z1,
     .                  X2,Y2,Z2,
     .                  X3,Y3,Z3,
     .                  X4,Y4,Z4,
     .                  XI,YI,ZI,
     .                  XP,YP,ZP,
     .                  N1,N2,N3,
     .                  ANS,SSC,TTC,
     .                  H1,H2,H3,
     .                  H4,XFACE,
     .                  ALP)

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGIMP,NTY
      my_real DIST(*)
      my_real
     .   X1(*), X2(*), X3(*), X4(*), Y1(*), Y2(*), Y3(*), Y4(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*), XI(*), YI(*), ZI(*), XP(*), YP(*),
     .   ZP(*), ANS(*), ALP(*), N1(*), N2(*), N3(*), SSC(*), TTC(*),
     .   XFACE(*), H1(*), H2(*), H3(*), H4(*)

C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
C-----------------------------------------------
C=======================================================================
      DO I=LFT,LLT
        H1(I) = ZERO
        H2(I) = ZERO
        H3(I) = ZERO
        H4(I) = ZERO
        XP(I) = ZERO
        YP(I) = ZERO
        ZP(I) = ZERO
        ANS(I) = ZERO
        DIST(I) = ZERO


        IF (XFACE(I) == ZERO) CYCLE
        IF (ABS(SSC(I))>ONE+ALP(I) .OR. ABS(TTC(I))>ONE+ALP(I)) THEN
          XFACE(I)=ZERO
        ELSE
          IF(ABS(SSC(I)) > ONE) SSC(I)=SSC(I)/ABS(SSC(I))
          IF(ABS(TTC(I)) > ONE) TTC(I)=TTC(I)/ABS(TTC(I))
        ENDIF
      END DO
C
      DO I=LFT,LLT
        H1(I) = FOURTH*(ONE-TTC(I))*(ONE-SSC(I))
        H2(I) = FOURTH*(ONE-TTC(I))*(ONE+SSC(I))
        H3(I) = FOURTH*(ONE+TTC(I))*(ONE+SSC(I))
        H4(I) = FOURTH*(ONE+TTC(I))*(ONE-SSC(I))
      END DO
C
      DO I=LFT,LLT
        XP(I)=H1(I)*X1(I)+H2(I)*X2(I)+H3(I)*X3(I)+H4(I)*X4(I)
        YP(I)=H1(I)*Y1(I)+H2(I)*Y2(I)+H3(I)*Y3(I)+H4(I)*Y4(I)
        ZP(I)=H1(I)*Z1(I)+H2(I)*Z2(I)+H3(I)*Z3(I)+H4(I)*Z4(I)
      END DO
C
      DO I=LFT,LLT
        ANS(I)= N1(I)*(XI(I)-XP(I))
     .         +N2(I)*(YI(I)-YP(I))
     .         +N3(I)*(ZI(I)-ZP(I))
      END DO
      IF (NTY == 8) THEN
        DO I=LFT,LLT
          DIST(I) = ANS(I)
        ENDDO
      ENDIF
C-----------
      RETURN
      END
