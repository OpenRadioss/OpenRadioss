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
Chd|  R5DEF3                        source/elements/spring/r5def3.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE R5DEF3(
     1   XL,      VX2L,    RY1L,    RZ1L,
     2   RX2L,    RY2L,    RZ2L,    FR_WAVE,
     3   FR_W_E,  EINT,    FX,      XMOM,
     4   YMOM,    ZMOM,    FY,      FZ,
     5   PARTSAV, IPARTR,  EXX,     EYX,
     6   EZX,     EXY,     EYY,     EZY,
     7   EXZ,     EYZ,     EZZ,     RX1,
     8   RY1,     RZ1,     RX2,     RY2,
     9   RZ2,     VX1,     VX2,     VY1,
     A   VY2,     VZ1,     VZ2,     NC1,
     B   NC2,     NEL)
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
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER IPARTR(*),NC1(*),NC2(*)
C     REAL
      my_real
     .   XL(*), VX2L(*),RY1L(*), RZ1L(*), RX2L(*), RY2L(*), RZ2L(*),
     .   FR_WAVE(*) ,FR_W_E(*),EINT(*) ,  
     .   FX(*), FY(*), FZ(*), XMOM(*), YMOM(*),ZMOM(*),PARTSAV(NPSAV,*),
     .   EXX(MVSIZ), EYX(MVSIZ), EZX(MVSIZ),
     .   EXY(MVSIZ), EYY(MVSIZ), EZY(MVSIZ),
     .   EXZ(MVSIZ), EYZ(MVSIZ), EZZ(MVSIZ), RX1(MVSIZ), RX2(MVSIZ),
     .   RY1(MVSIZ), RY2(MVSIZ), RZ1(MVSIZ), RZ2(MVSIZ), VX1(MVSIZ),
     .   VX2(MVSIZ), VY1(MVSIZ), VY2(MVSIZ), VZ1(MVSIZ), VZ2(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX
C     REAL
      my_real
     .     VX1L, VY1L, VY2L, VZ1L, VZ2L,THETA , XLDEMI,XSIGN
C-----------------------------------------------
C--------------------------------------------
C     VITESSES REPERE CONVECTEE
C--------------------------------------------
      DO I=1,NEL
        RX2L(I) = EXX(I)*(RX2(I)-RX1(I))
     .          + EYX(I)*(RY2(I)-RY1(I))
     .          + EZX(I)*(RZ2(I)-RZ1(I))
        RY1L(I) = EXY(I)*RX1(I)+EYY(I)*RY1(I)+EZY(I)*RZ1(I)
        RY2L(I) = EXY(I)*RX2(I)+EYY(I)*RY2(I)+EZY(I)*RZ2(I)
        RZ1L(I) = EXZ(I)*RX1(I)+EYZ(I)*RY1(I)+EZZ(I)*RZ1(I)
        RZ2L(I) = EXZ(I)*RX2(I)+EYZ(I)*RY2(I)+EZZ(I)*RZ2(I)
        VX2L(I) = EXX(I)*(VX2(I)-VX1(I))
     .          + EYX(I)*(VY2(I)-VY1(I))
     .          + EZX(I)*(VZ2(I)-VZ1(I))
        VY2L   = EXY(I)*(VX2(I)-VX1(I))
     .         + EYY(I)*(VY2(I)-VY1(I))
     .         + EZY(I)*(VZ2(I)-VZ1(I))
        XSIGN  = SIGN(ONE, XL(I) - HALF*VX2L(I)*DT1)
        XLDEMI = XSIGN/MAX(EM15,ABS(XL(I) - HALF*VX2L(I)*DT1))
        THETA  = VY2L * XLDEMI
        RZ1L(I) = RZ1L(I) - THETA
        RZ2L(I) = RZ2L(I) - THETA
        VZ2L   = EXZ(I)*(VX2(I)-VX1(I))
     .         + EYZ(I)*(VY2(I)-VY1(I))
     .         + EZZ(I)*(VZ2(I)-VZ1(I))
        THETA  = VZ2L * XLDEMI
        RY1L(I) = RY1L(I) + THETA
        RY2L(I) = RY2L(I) + THETA
        VX2L(I) = VX2L(I)
     .    - HALF*DT1*XLDEMI*(VY2L*VY2L+VZ2L*VZ2L)
      ENDDO
C--------------------------------------------
C     Energy
C--------------------------------------------
      DO I=1,NEL
        EINT(I) = EINT(I)
     .+ HALF*DT1 * (VX2L(I) * FX(I) + RX2L(I) * XMOM(I) 
     .          + (RY2L(I) - RY1L(I)) * YMOM(I) 
     .          + (RZ2L(I) - RZ1L(I)) * ZMOM(I)
     .          + HALF * (RY2L(I) + RY1L(I)) * FZ(I) * XL(I)
     .          - HALF * (RZ2L(I) + RZ1L(I)) * FY(I) * XL(I) )
      ENDDO
C
      IF (NPSAV >= 21) THEN
        DO I=1,NEL
          MX = IPARTR(I)
          PARTSAV(23,MX)=PARTSAV(23,MX)
     .          + HALF*DT1 * (RX2L(I) * XMOM(I)
     .          + (RY2L(I) - RY1L(I)) * YMOM(I) 
     .          + (RZ2L(I) - RZ1L(I)) * ZMOM(I)
     .          + HALF * (RY2L(I) + RY1L(I)) * FZ(I) * XL(I)
     .          - HALF * (RZ2L(I) + RZ1L(I)) * FY(I) * XL(I) )
        ENDDO
      ENDIF
C--------------------------------------------
C     Front wave
C--------------------------------------------
      IF (IFRWV /= 0) THEN
        DO I=1,NEL
          FR_W_E(I)=MAX(FR_WAVE(NC1(I)),FR_WAVE(NC2(I)),ZERO)
        ENDDO
      ENDIF
C---
      RETURN
      END
