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
Chd|  Q4DEFO2                       source/elements/solid_2d/quad4/q4defo2.F
Chd|-- called by -----------
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE Q4DEFO2(
     1   PY1,     PY2,     PY3,     PY4,
     2   PZ1,     PZ2,     PZ3,     PZ4,
     3   BYZ1,    BYZ2,    BYZ3,    BYZ4,
     4   BZY1,    BZY2,    BZY3,    BZY4,
     5   VY1,     VY2,     VY3,     VY4,
     6   VZ1,     VZ2,     VZ3,     VZ4,
     7   EYZ,     EYY,     EZZ,     EXX,
     8   WXX,     R22,     R23,     AY,
     9   OFF,     OFFG,    VOLO,    EINT,
     A   DSV,     ICP,     FAC,     NEL,
     B   JCVT)
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
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: NEL
        INTEGER, INTENT(IN) :: JCVT
        INTEGER ICP
C     REAL
        my_real
     .      PY1(*),PY2(*),PY3(*),PY4(*),
     .      PZ1(*),PZ2(*),PZ3(*),PZ4(*),
     .      BYZ1(*),BYZ2(*),BYZ3(*),BYZ4(*),
     .      BZY1(*),BZY2(*),BZY3(*),BZY4(*),
     .      VY1(*),VY2(*),VY3(*),VY4(*),
     .      VZ1(*),VZ2(*),VZ3(*),VZ4(*),
     .      EYZ(*),EYY(*),EZZ(*),EXX(*),WXX(*),
     .      R22(*),R23(*),
     .      AY(*),OFF(*),OFFG(*),VOLO(*),
     .      EINT(*),DSV(*),FAC(*)
C-----------------------------------------------
c FUNCTION:
c ARGUMENTS:  (I: input, O: output, IO: input & output, W: workspace)
c TYPE NAME                FUNCTION
c  I   PY1(*)~PZ4(*)      - SHAPE DERIVATIVES
c  I   VY1(*)~VZ4(*)      - NODAL VELOCITIES
c  O   EYY(*),EZZ(*),EXX(*) - RATE OF NORMAL STRAIN
c  O   WXX(*)             - COMPONENT OF ROTATION TENSOR
c  I   AY(*)              - Ni/r AT CENTER FOR AXISYMMETRIC CASE
c  IO  OFF(*)             - ELEMENT D/A FLAG, OF POINT
c  I   OFFG(*)            - ELEMENT D/A FLAG, OVERALL
c  IO  VOLO(*)            - EV(NB6)
c  IO  EINT(*)            - EV(NB3)
c  I   DSV(*)             - VOLUME STRAIN RATE AT CENTER
c  I   ICP                - FLAG FOR CONSTANT PRESURE
c  I   FAC(*)             - FRACTION ABOUT PLASTICITY STATE
c  I   R22(*)~R33(*)      - TRANSFORMATION MATRIX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I
C     REAL
        my_real
     .      DYZ(MVSIZ),DZY(MVSIZ),
     .      DVC(MVSIZ),DV1,TOL,
     .      DT1D2,VYG
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C     STRAIN RATE BEFORE CORRECTION
        DO I=1,NEL
          EYY(I)=PY1(I)*VY1(I)+PY2(I)*VY2(I)+PY3(I)*VY3(I)+PY4(I)*VY4(I)
          EZZ(I)=PZ1(I)*VZ1(I)+PZ2(I)*VZ2(I)+PZ3(I)*VZ3(I)+PZ4(I)*VZ4(I)
          DYZ(I)=PZ1(I)*VY1(I)+PZ2(I)*VY2(I)+PZ3(I)*VY3(I)+PZ4(I)*VY4(I)
          DZY(I)=PY1(I)*VZ1(I)+PY2(I)*VZ2(I)+PY3(I)*VZ3(I)+PY4(I)*VZ4(I)
        ENDDO
C
Ccw>>>
C
C     STRAIN RATE AFTER CORRECTION AND INTRINSIC ROTATION
        DT1D2=HALF*DT1
        IF(JCVT==0) THEN
          DO I=1,NEL
            WXX(I)=DT1D2*(DZY(I)-DYZ(I))
          ENDDO
        ELSE
          DO I=1,NEL
            WXX(I)=ZERO
            EYY(I)=EYY(I)-DT1D2*(EYY(I)*EYY(I)+DZY(I)*DZY(I))
            EZZ(I)=EZZ(I)-DT1D2*(EZZ(I)*EZZ(I)+DYZ(I)*DYZ(I))
          ENDDO
        ENDIF
C
Ccw<<<
        DO I=1,NEL
          DVC(I)=EYY(I)+EZZ(I)+EXX(I)
        ENDDO
Ccw>>> ----- only assumed strain
        DO I=1,NEL
          EYY(I)=EYY(I)+
     .      BYZ1(I)*VZ1(I)+BYZ2(I)*VZ2(I)+BYZ3(I)*VZ3(I)+BYZ4(I)*VZ4(I)
          EZZ(I)=EZZ(I)+
     .      BZY1(I)*VY1(I)+BZY2(I)*VY2(I)+BZY3(I)*VY3(I)+BZY4(I)*VY4(I)
        ENDDO
C
C
Ccw<<<
C     MODIFY NORMAL STRAIN RATE FOR CONSTANT PRESSURE CASE
c      IF(ICP>0) THEN
c       IF(N2D==1) THEN
c        IF(ICP==2) THEN
c          DO I=1,NEL
c            DVC(I)=FAC(I)*THIRD*(DSV(I)-DVC(I))
c            EYY(I)=EYY(I)+DVC(I)
c            EZZ(I)=EZZ(I)+DVC(I)
c            EXX(I)=EXX(I)+DVC(I)
c          ENDDO
c        ELSEIF(ICP==1) THEN
c          DO I=1,NEL
c            DVC(I)=THIRD*(DSV(I)-DVC(I))
c            EYY(I)=EYY(I)+DVC(I)
c            EZZ(I)=EZZ(I)+DVC(I)
c            EXX(I)=EXXC(I)+DVC(I)
c          ENDDO
c        ENDIF
c       ELSE
c        IF(ICP==2) THEN
c          DO I=1,NEL
c            DVC(I)=FAC(I)*HALF*(DSV(I)-DVC(I))
c            EYY(I)=EYY(I)+DVC(I)
c            EZZ(I)=EZZ(I)+DVC(I)
c          ENDDO
c        ELSEIF(ICP==1) THEN
c          DO I=1,NEL
c            DVC(I)=HALF*(DSV(I)-DVC(I))
c            EYY(I)=EYY(I)+DVC(I)
c            EZZ(I)=EZZ(I)+DVC(I)
c          ENDDO
c        ENDIF
c       END IF!(N2D==1) THEN
C----For hypoelastic laws-----
c       DO I=1,NEL
c        IF(OFFG(I)==TWO) CYCLE
c   DV1 = ONE- DVC(I)*DT1
c         VOLO(I) = VOLO(I)*DV1
c         EINT(I) = EINT(I)/DV1
c       ENDDO
c      END IF!(ICP>0) THEN
Ccw>>>
C
        RETURN
      END

