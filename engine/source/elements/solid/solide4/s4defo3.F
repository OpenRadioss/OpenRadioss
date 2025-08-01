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
!||    s4defo3   ../engine/source/elements/solid/solide4/s4defo3.F
!||--- called by ------------------------------------------------------
!||    s4forc3   ../engine/source/elements/solid/solide4/s4forc3.F
!||====================================================================
      SUBROUTINE S4DEFO3(
     1   PX1,     PX2,     PX3,     PX4,
     2   PY1,     PY2,     PY3,     PY4,
     3   PZ1,     PZ2,     PZ3,     PZ4,
     4   VX1,     VX2,     VX3,     VX4,
     5   VY1,     VY2,     VY3,     VY4,
     6   VZ1,     VZ2,     VZ3,     VZ4,
     7   DXX,     DXY,     DXZ,     DYX,
     8   DYY,     DYZ,     DZX,     DZY,
     9   DZZ,     D4,      D5,      D6,
     A   WXX,     WYY,     WZZ,     NEL,
     B   ISMSTR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: ISMSTR
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*), D4(*), D5(*), D6(*),
     .   WXX(*), WYY(*), WZZ(*)
CMasParINCLUDE 'sdefo3.intmap.inc'
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
C     REAL
      my_real
     .   DT1D2
      my_real 
     .   PXX2,PYY2,PZZ2,PXX2P,PYY2P,PZZ2P,AAA,BBB,CCC,
     .   EXX,EXY,EXZ,EYX,EYY,EYZ,EZX,EZY,EZZ
C-----------------------------------------------
      DO I=1,NEL
        DXX(I)=PX1(I)*VX1(I)+PX2(I)*VX2(I)+
     .         PX3(I)*VX3(I)+PX4(I)*VX4(I)
        DYY(I)=PY1(I)*VY1(I)+PY2(I)*VY2(I)+
     .         PY3(I)*VY3(I)+PY4(I)*VY4(I)
        DZZ(I)=PZ1(I)*VZ1(I)+PZ2(I)*VZ2(I)+
     .         PZ3(I)*VZ3(I)+PZ4(I)*VZ4(I)
        DXY(I)=PY1(I)*VX1(I)+PY2(I)*VX2(I)+
     .         PY3(I)*VX3(I)+PY4(I)*VX4(I)
        DXZ(I)=PZ1(I)*VX1(I)+PZ2(I)*VX2(I)+
     .         PZ3(I)*VX3(I)+PZ4(I)*VX4(I)
        DYX(I)=PX1(I)*VY1(I)+PX2(I)*VY2(I)+
     .         PX3(I)*VY3(I)+PX4(I)*VY4(I)
        DYZ(I)=PZ1(I)*VY1(I)+PZ2(I)*VY2(I)+
     .         PZ3(I)*VY3(I)+PZ4(I)*VY4(I)
        DZX(I)=PX1(I)*VZ1(I)+PX2(I)*VZ2(I)+
     .         PX3(I)*VZ3(I)+PX4(I)*VZ4(I)
        DZY(I)=PY1(I)*VZ1(I)+PY2(I)*VZ2(I)+
     .         PY3(I)*VZ3(I)+PY4(I)*VZ4(I)
      ENDDO
C
      DT1D2=HALF*DT1
C
      IF(ISMSTR==2.OR.ISMSTR==4)THEN
C
C       2nd order correction is needed only w/large strain 
C       since small strain is not objective anyway
        DO I=1,NEL
C
         EXX=DXX(I)
         EYY=DYY(I)
         EZZ=DZZ(I)
         EXY=DXY(I)
         EYX=DYX(I)
         EXZ=DXZ(I)
         EZX=DZX(I)
         EYZ=DYZ(I)
         EZY=DZY(I)
C
C        Euler-Almansi A=1/2 [I-Invers(B)], with Left Cauchy-Green tensor B = F.Transp(F) 
C                        Invers(B)=Invers(Transp(F)).Invers(F)
C
C        small perturbation  A ~ (I-Transp(gradU)).(I-gradU)
C                            A ~ 1/2 (gradU + Transp(gradU) - Transp(gradU).gradU)
C
         DXX(I)  = DXX(I)-DT1D2*(EXX*EXX+EYX*EYX+EZX*EZX)
         DYY(I)  = DYY(I)-DT1D2*(EYY*EYY+EZY*EZY+EXY*EXY)
         DZZ(I)  = DZZ(I)-DT1D2*(EZZ*EZZ+EXZ*EXZ+EYZ*EYZ)
         AAA = DT1D2*(EXX*EXY+EYX*EYY+EZX*EZY)
         DXY(I)  = DXY(I) -AAA
         DYX(I)  = DYX(I) -AAA
         D4(I)   = DXY(I)+DYX(I)
         BBB = DT1D2*(EYY*EYZ+EZY*EZZ+EXY*EXZ)
         DYZ(I)  = DYZ(I) -BBB
         DZY(I)  = DZY(I) -BBB
         D5(I)   = DYZ(I)+DZY(I)
         CCC = DT1D2*(EZZ*EZX+EXZ*EXX+EYZ*EYX)
         DXZ(I)  = DXZ(I) -CCC
         DZX(I)  = DZX(I) -CCC
         D6(I)   = DXZ(I)+DZX(I)
C
C        WZZ(I)=DT1D2*(EYX-EXY)
C        WYY(I)=DT1D2*(EXZ-EZX)
C        WXX(I)=DT1D2*(EZY-EYZ)
C
C        B = F Transp(F) = V^2, F = V.R 
C        => R = [Invers(Transp(F)).F]^1/2
C        small perturbation  => R ~ I + 1/2 ( gradU - Transp(gradU) - Transp(gradU).gradU )
C
C                [ 0     wzz    -wyy ]
C        and R = [ wzz     0    -wxx ]
C                [ wyy   wxx       0 ]
C
         WZZ(I)=DT1*(HALF*(EYX-EXY)-AAA)
         WYY(I)=DT1*(HALF*(EXZ-EZX)-BBB)
         WXX(I)=DT1*(HALF*(EZY-EYZ)-CCC)
        ENDDO
      ELSE ! ISMSTR==1 or Total Strain Formulation (Approximated Strain Rate)
        DO I=1,NEL
         D4(I)   = DXY(I)+DYX(I)
         D5(I)   = DYZ(I)+DZY(I)
         D6(I)   = DXZ(I)+DZX(I)
         WZZ(I)=DT1D2*(DYX(I)-DXY(I))
         WYY(I)=DT1D2*(DXZ(I)-DZX(I))
         WXX(I)=DT1D2*(DZY(I)-DYZ(I))
        ENDDO
      END IF
C
      RETURN
C
      END
