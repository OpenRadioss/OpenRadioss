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
Chd|  S8EDEFO3                      source/elements/solid/solide8e/s8edefo3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EDEFO3(
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    BXX1, BXX2, BXX3, BXX4, BXX5, BXX6, BXX7, BXX8,
     .    BYY1, BYY2, BYY3, BYY4, BYY5, BYY6, BYY7, BYY8,
     .    BZZ1, BZZ2, BZZ3, BZZ4, BZZ5, BZZ6, BZZ7, BZZ8,
     .    PXY1, PXY2, PXY3, PXY4, PXY5, PXY6, PXY7, PXY8,
     .    PYX1, PYX2, PYX3, PYX4, PYX5, PYX6, PYX7, PYX8,
     .    PXZ1, PXZ2, PXZ3, PXZ4, PXZ5, PXZ6, PXZ7, PXZ8,
     .    PZX1, PZX2, PZX3, PZX4, PZX5, PZX6, PZX7, PZX8,
     .    PYZ1, PYZ2, PYZ3, PYZ4, PYZ5, PYZ6, PYZ7, PYZ8,
     .    PZY1, PZY2, PZY3, PZY4, PZY5, PZY6, PZY7, PZY8,
     .    VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .    VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .    VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .    DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ, D4, D5, D6,
     .    WXX, WYY, WZZ,VOLO, OFF,EINT, OFFS,DSV,ICP ,FAC,SDV,
     .    I_SH,IDEGE,VOL0DP)
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
#include      "vect01_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICP,I_SH,IDEGE(*)
C     REAL
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PY5(*), PY6(*), PY7(*), PY8(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*), D4(*), D5(*), D6(*),
     .   WXX(*), WYY(*), WZZ(*), OFFS(*),DSV(*),
     .   VOLO(*),OFF(*),EINT(*),FAC(*),SDV(*),
     .   PXY1(MVSIZ),PXY2(MVSIZ),PXY3(MVSIZ),PXY4(MVSIZ),
     .   PXY5(MVSIZ),PXY6(MVSIZ),PXY7(MVSIZ),PXY8(MVSIZ),
     .   PYX1(MVSIZ),PYX2(MVSIZ),PYX3(MVSIZ),PYX4(MVSIZ),
     .   PYX5(MVSIZ),PYX6(MVSIZ),PYX7(MVSIZ),PYX8(MVSIZ),
     .   PXZ1(MVSIZ),PXZ2(MVSIZ),PXZ3(MVSIZ),PXZ4(MVSIZ),
     .   PXZ5(MVSIZ),PXZ6(MVSIZ),PXZ7(MVSIZ),PXZ8(MVSIZ),
     .   PZX1(MVSIZ),PZX2(MVSIZ),PZX3(MVSIZ),PZX4(MVSIZ),
     .   PZX5(MVSIZ),PZX6(MVSIZ),PZX7(MVSIZ),PZX8(MVSIZ),
     .   PYZ1(MVSIZ),PYZ2(MVSIZ),PYZ3(MVSIZ),PYZ4(MVSIZ),
     .   PYZ5(MVSIZ),PYZ6(MVSIZ),PYZ7(MVSIZ),PYZ8(MVSIZ),
     .   PZY1(MVSIZ),PZY2(MVSIZ),PZY3(MVSIZ),PZY4(MVSIZ),
     .   PZY5(MVSIZ),PZY6(MVSIZ),PZY7(MVSIZ),PZY8(MVSIZ),
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
     .   BZZ5(*),BZZ6(*),BZZ7(*),BZZ8(*)
      DOUBLE PRECISION 
     .   VOL0DP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   DT1D2,DVC(MVSIZ),TOL,DT1D
      DOUBLE PRECISION 
     .   DVP,DV,DV1
C                                                                     12
C-----------------------------------------------
      TOL = ONE-EM20
      IF (I_SH==0) THEN
      DO I=LFT,LLT
       DXY(I) =PY1(I)*VX1(I)+PY2(I)*VX2(I)+PY3(I)*VX3(I)+PY4(I)*VX4(I)
     +        +PY5(I)*VX5(I)+PY6(I)*VX6(I)+PY7(I)*VX7(I)+PY8(I)*VX8(I)
       DXZ(I) =PZ1(I)*VX1(I)+PZ2(I)*VX2(I)+PZ3(I)*VX3(I)+PZ4(I)*VX4(I)
     +        +PZ5(I)*VX5(I)+PZ6(I)*VX6(I)+PZ7(I)*VX7(I)+PZ8(I)*VX8(I)
       DYX(I) =PX1(I)*VY1(I)+PX2(I)*VY2(I)+PX3(I)*VY3(I)+PX4(I)*VY4(I)
     +        +PX5(I)*VY5(I)+PX6(I)*VY6(I)+PX7(I)*VY7(I)+PX8(I)*VY8(I)
       DYZ(I) =PZ1(I)*VY1(I)+PZ2(I)*VY2(I)+PZ3(I)*VY3(I)+PZ4(I)*VY4(I)
     +        +PZ5(I)*VY5(I)+PZ6(I)*VY6(I)+PZ7(I)*VY7(I)+PZ8(I)*VY8(I)
       DZX(I) =PX1(I)*VZ1(I)+PX2(I)*VZ2(I)+PX3(I)*VZ3(I)+PX4(I)*VZ4(I)
     +        +PX5(I)*VZ5(I)+PX6(I)*VZ6(I)+PX7(I)*VZ7(I)+PX8(I)*VZ8(I)
       DZY(I) =PY1(I)*VZ1(I)+PY2(I)*VZ2(I)+PY3(I)*VZ3(I)+PY4(I)*VZ4(I)
     +        +PY5(I)*VZ5(I)+PY6(I)*VZ6(I)+PY7(I)*VZ7(I)+PY8(I)*VZ8(I)
      ENDDO
      ELSE
       DO I=LFT,LLT
       DXY(I) =PXY1(I)*VX1(I)+PXY2(I)*VX2(I)
     +        +PXY3(I)*VX3(I)+PXY4(I)*VX4(I)
     +        +PXY5(I)*VX5(I)+PXY6(I)*VX6(I)
     +        +PXY7(I)*VX7(I)+PXY8(I)*VX8(I)
       DXZ(I) =PXZ1(I)*VX1(I)+PXZ2(I)*VX2(I)
     +        +PXZ3(I)*VX3(I)+PXZ4(I)*VX4(I)
     +        +PXZ5(I)*VX5(I)+PXZ6(I)*VX6(I)
     +        +PXZ7(I)*VX7(I)+PXZ8(I)*VX8(I)
       DYX(I) =PYX1(I)*VY1(I)+PYX2(I)*VY2(I)
     +        +PYX3(I)*VY3(I)+PYX4(I)*VY4(I)
     +        +PYX5(I)*VY5(I)+PYX6(I)*VY6(I)
     +        +PYX7(I)*VY7(I)+PYX8(I)*VY8(I)
       DYZ(I) =PYZ1(I)*VY1(I)+PYZ2(I)*VY2(I)
     +        +PYZ3(I)*VY3(I)+PYZ4(I)*VY4(I)
     +        +PYZ5(I)*VY5(I)+PYZ6(I)*VY6(I)
     +        +PYZ7(I)*VY7(I)+PYZ8(I)*VY8(I)
       DZX(I) =PZX1(I)*VZ1(I)+PZX2(I)*VZ2(I)
     +        +PZX3(I)*VZ3(I)+PZX4(I)*VZ4(I)
     +        +PZX5(I)*VZ5(I)+PZX6(I)*VZ6(I)
     +        +PZX7(I)*VZ7(I)+PZX8(I)*VZ8(I)
       DZY(I) =PZY1(I)*VZ1(I)+PZY2(I)*VZ2(I)
     +        +PZY3(I)*VZ3(I)+PZY4(I)*VZ4(I)
     +        +PZY5(I)*VZ5(I)+PZY6(I)*VZ6(I)
     +        +PZY7(I)*VZ7(I)+PZY8(I)*VZ8(I)
       ENDDO
      END IF !(I_SH==0) THEN
      IF (ICP==11) THEN
       DO I=LFT,LLT
        DXX(I) =BXX1(I)*VX1(I)+BXX2(I)*VX2(I)+BXX3(I)*VX3(I)+
     .          BXX4(I)*VX4(I)+BXX5(I)*VX5(I)+BXX6(I)*VX6(I)+
     .          BXX7(I)*VX7(I)+BXX8(I)*VX8(I)
        DYY(I) =BYY1(I)*VY1(I)+BYY2(I)*VY2(I)+BYY3(I)*VY3(I)+
     .          BYY4(I)*VY4(I)+BYY5(I)*VY5(I)+BYY6(I)*VY6(I)+
     .          BYY7(I)*VY7(I)+BYY8(I)*VY8(I)
        DZZ(I) =BZZ1(I)*VZ1(I)+BZZ2(I)*VZ2(I)+BZZ3(I)*VZ3(I)+
     .          BZZ4(I)*VZ4(I)+BZZ5(I)*VZ5(I)+BZZ6(I)*VZ6(I)+
     .          BZZ7(I)*VZ7(I)+BZZ8(I)*VZ8(I)
        DYY(I) =DYY(I)+BXY1(I)*VX1(I)+BXY2(I)*VX2(I)
     +        +BXY3(I)*VX3(I)+BXY4(I)*VX4(I)
     +        +BXY5(I)*VX5(I)+BXY6(I)*VX6(I)
     +        +BXY7(I)*VX7(I)+BXY8(I)*VX8(I)
        DZZ(I) =DZZ(I)+BXZ1(I)*VX1(I)+BXZ2(I)*VX2(I)
     +        +BXZ3(I)*VX3(I)+BXZ4(I)*VX4(I)
     +        +BXZ5(I)*VX5(I)+BXZ6(I)*VX6(I)
     +        +BXZ7(I)*VX7(I)+BXZ8(I)*VX8(I)
        DXX(I) =DXX(I)+BYX1(I)*VY1(I)+BYX2(I)*VY2(I)
     +        +BYX3(I)*VY3(I)+BYX4(I)*VY4(I)
     +        +BYX5(I)*VY5(I)+BYX6(I)*VY6(I)
     +        +BYX7(I)*VY7(I)+BYX8(I)*VY8(I)
        DZZ(I) =DZZ(I)+BYZ1(I)*VY1(I)+BYZ2(I)*VY2(I)
     +        +BYZ3(I)*VY3(I)+BYZ4(I)*VY4(I)
     +        +BYZ5(I)*VY5(I)+BYZ6(I)*VY6(I)
     +        +BYZ7(I)*VY7(I)+BYZ8(I)*VY8(I)
        DXX(I) =DXX(I)+BZX1(I)*VZ1(I)+BZX2(I)*VZ2(I)
     +        +BZX3(I)*VZ3(I)+BZX4(I)*VZ4(I)
     +        +BZX5(I)*VZ5(I)+BZX6(I)*VZ6(I)
     +        +BZX7(I)*VZ7(I)+BZX8(I)*VZ8(I)
        DYY(I) =DYY(I)+BZY1(I)*VZ1(I)+BZY2(I)*VZ2(I)
     +        +BZY3(I)*VZ3(I)+BZY4(I)*VZ4(I)
     +        +BZY5(I)*VZ5(I)+BZY6(I)*VZ6(I)
     +        +BZY7(I)*VZ7(I)+BZY8(I)*VZ8(I)
        DVC(I) =ZERO
       ENDDO
      ELSE
       DO I=LFT,LLT
       DXX(I) =PX1(I)*VX1(I)+PX2(I)*VX2(I)+PX3(I)*VX3(I)+PX4(I)*VX4(I)
     .        +PX5(I)*VX5(I)+PX6(I)*VX6(I)+PX7(I)*VX7(I)+PX8(I)*VX8(I)
       DYY(I) =PY1(I)*VY1(I)+PY2(I)*VY2(I)+PY3(I)*VY3(I)+PY4(I)*VY4(I)
     .        +PY5(I)*VY5(I)+PY6(I)*VY6(I)+PY7(I)*VY7(I)+PY8(I)*VY8(I)
       DZZ(I) =PZ1(I)*VZ1(I)+PZ2(I)*VZ2(I)+PZ3(I)*VZ3(I)+PZ4(I)*VZ4(I)
     .        +PZ5(I)*VZ5(I)+PZ6(I)*VZ6(I)+PZ7(I)*VZ7(I)+PZ8(I)*VZ8(I)
       DVC(I) =DXX(I)+DYY(I)+DZZ(I)
       ENDDO
      END IF !(ICP==11) THEN
C------force ICP>0 -->I_sh=0 or check   !!!!   
      IF (I_SH>1) THEN
#include   "nofusion.inc"
       DO I=LFT,LLT
        IF(IDEGE(I)>10) CYCLE
        DYY(I) =DYY(I)+BXY1(I)*VX1(I)+BXY2(I)*VX2(I)
     +        +BXY3(I)*VX3(I)+BXY4(I)*VX4(I)
     +        +BXY5(I)*VX5(I)+BXY6(I)*VX6(I)
     +        +BXY7(I)*VX7(I)+BXY8(I)*VX8(I)
        DZZ(I) =DZZ(I)+BXZ1(I)*VX1(I)+BXZ2(I)*VX2(I)
     +        +BXZ3(I)*VX3(I)+BXZ4(I)*VX4(I)
     +        +BXZ5(I)*VX5(I)+BXZ6(I)*VX6(I)
     +        +BXZ7(I)*VX7(I)+BXZ8(I)*VX8(I)
        DXX(I) =DXX(I)+BYX1(I)*VY1(I)+BYX2(I)*VY2(I)
     +        +BYX3(I)*VY3(I)+BYX4(I)*VY4(I)
     +        +BYX5(I)*VY5(I)+BYX6(I)*VY6(I)
     +        +BYX7(I)*VY7(I)+BYX8(I)*VY8(I)
        DZZ(I) =DZZ(I)+BYZ1(I)*VY1(I)+BYZ2(I)*VY2(I)
     +        +BYZ3(I)*VY3(I)+BYZ4(I)*VY4(I)
     +        +BYZ5(I)*VY5(I)+BYZ6(I)*VY6(I)
     +        +BYZ7(I)*VY7(I)+BYZ8(I)*VY8(I)
        DXX(I) =DXX(I)+BZX1(I)*VZ1(I)+BZX2(I)*VZ2(I)
     +        +BZX3(I)*VZ3(I)+BZX4(I)*VZ4(I)
     +        +BZX5(I)*VZ5(I)+BZX6(I)*VZ6(I)
     +        +BZX7(I)*VZ7(I)+BZX8(I)*VZ8(I)
        DYY(I) =DYY(I)+BZY1(I)*VZ1(I)+BZY2(I)*VZ2(I)
     +        +BZY3(I)*VZ3(I)+BZY4(I)*VZ4(I)
     +        +BZY5(I)*VZ5(I)+BZY6(I)*VZ6(I)
     +        +BZY7(I)*VZ7(I)+BZY8(I)*VZ8(I)
        DVC(I) =(DXX(I)+DYY(I)+DZZ(I)-DVC(I))*DT1
       ENDDO
      END IF !(I_SH>1) THEN
      IF (ISMSTR==12.AND.I_SH>0) THEN
#include   "nofusion.inc"
       DO I=LFT,LLT
        IF(OFFS(I)<=ONE) CYCLE
        DYY(I) =DYY(I)+BXY1(I)*VX1(I)+BXY2(I)*VX2(I)
     +        +BXY3(I)*VX3(I)+BXY4(I)*VX4(I)
     +        +BXY5(I)*VX5(I)+BXY6(I)*VX6(I)
     +        +BXY7(I)*VX7(I)+BXY8(I)*VX8(I)
        DZZ(I) =DZZ(I)+BXZ1(I)*VX1(I)+BXZ2(I)*VX2(I)
     +        +BXZ3(I)*VX3(I)+BXZ4(I)*VX4(I)
     +        +BXZ5(I)*VX5(I)+BXZ6(I)*VX6(I)
     +        +BXZ7(I)*VX7(I)+BXZ8(I)*VX8(I)
        DXX(I) =DXX(I)+BYX1(I)*VY1(I)+BYX2(I)*VY2(I)
     +        +BYX3(I)*VY3(I)+BYX4(I)*VY4(I)
     +        +BYX5(I)*VY5(I)+BYX6(I)*VY6(I)
     +        +BYX7(I)*VY7(I)+BYX8(I)*VY8(I)
        DZZ(I) =DZZ(I)+BYZ1(I)*VY1(I)+BYZ2(I)*VY2(I)
     +        +BYZ3(I)*VY3(I)+BYZ4(I)*VY4(I)
     +        +BYZ5(I)*VY5(I)+BYZ6(I)*VY6(I)
     +        +BYZ7(I)*VY7(I)+BYZ8(I)*VY8(I)
        DXX(I) =DXX(I)+BZX1(I)*VZ1(I)+BZX2(I)*VZ2(I)
     +        +BZX3(I)*VZ3(I)+BZX4(I)*VZ4(I)
     +        +BZX5(I)*VZ5(I)+BZX6(I)*VZ6(I)
     +        +BZX7(I)*VZ7(I)+BZX8(I)*VZ8(I)
        DYY(I) =DYY(I)+BZY1(I)*VZ1(I)+BZY2(I)*VZ2(I)
     +        +BZY3(I)*VZ3(I)+BZY4(I)*VZ4(I)
     +        +BZY5(I)*VZ5(I)+BZY6(I)*VZ6(I)
     +        +BZY7(I)*VZ7(I)+BZY8(I)*VZ8(I)
        DVC(I) =(DXX(I)+DYY(I)+DZZ(I)-DVC(I))*DT1
       ENDDO
      END IF !(ISMSTR==12) THEN
C-------------Volume traitement------
      IF (ICP==2.AND.I_SH<=1) THEN
       DO I=LFT,LLT
        DVC(I)=(DSV(I)-DVC(I))*FAC(I)*DT1
       ENDDO
      ELSEIF (ICP==1) THEN
       DO I=LFT,LLT
        DVC(I)=(DSV(I)-DVC(I))*DT1
       ENDDO
      ENDIF 
      IF ((ICP>0.AND.ISMSTR/=10.AND.ISMSTR/=12).OR.I_SH>1) THEN
       DO I=LFT,LLT
        DV =DVC(I)*OFF(I)
        IF(IDEGE(I)>10) DV = ZERO
        SDV(I) =DV
        IF (DV>TOL) THEN
          DV =ZERO
        ENDIF
        IF(OFFS(I)==TWO.OR.ISMSTR==11) CYCLE
         DV1 = ONE- DV
         VOLO(I) = VOLO(I)*DV1
         IF (IRESP==1) VOL0DP(I) = VOL0DP(I)*DV1
         EINT(I) = EINT(I)/DV1
c        ENDIF
       ENDDO
      ELSE
       DO I=LFT,LLT
        SDV(I) =ZERO
       ENDDO
      ENDIF 
C
      DT1D2=HALF*DT1
      IF (ISCAU>0)DT1D2=DT1
      DT1D=TWO*DT1D2
      IF (JCVT/=0) THEN
C      
       IF (ISMDISP>0.AND.ISCAU==0) THEN
        DO I=LFT,LLT
         D4(I)   = DXY(I)+DYX(I)
         D5(I)   = DYZ(I)+DZY(I)
         D6(I)   = DXZ(I)+DZX(I)
         WXX(I)=ZERO
         WYY(I)=ZERO
         WZZ(I)=ZERO
        ENDDO
       ELSE
       DO I=LFT,LLT
        D4(I)   = DXY(I)+DYX(I)
     .           -DT1D*(DXX(I)*DXY(I)+DYX(I)*DYY(I)+DZX(I)*DZY(I))
        D5(I)   = DYZ(I)+DZY(I)
     .           -DT1D*(DYY(I)*DYZ(I)+DZY(I)*DZZ(I)+DXY(I)*DXZ(I))
        D6(I)   = DXZ(I)+DZX(I)
     .           -DT1D*(DZZ(I)*DZX(I)+DXZ(I)*DXX(I)+DYZ(I)*DYX(I))
        DXX(I)  = DXX(I)
     .           -DT1D2*(DXX(I)*DXX(I)+DYX(I)*DYX(I)+DZX(I)*DZX(I))
        DYY(I)  = DYY(I)
     .           -DT1D2*(DYY(I)*DYY(I)+DZY(I)*DZY(I)+DXY(I)*DXY(I))
        DZZ(I)  = DZZ(I)
     .           -DT1D2*(DZZ(I)*DZZ(I)+DXZ(I)*DXZ(I)+DYZ(I)*DYZ(I))
        WXX(I)=ZERO
        WYY(I)=ZERO
        WZZ(I)=ZERO
       ENDDO
       ENDIF 
      ELSE
       DO I=LFT,LLT
        D4(I)=DXY(I)+DYX(I)
        D5(I)=DYZ(I)+DZY(I)
        D6(I)=DXZ(I)+DZX(I)
        WZZ(I)=DT1D2*(DYX(I)-DXY(I))
        WYY(I)=DT1D2*(DXZ(I)-DZX(I))
        WXX(I)=DT1D2*(DZY(I)-DYZ(I))
       ENDDO
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  S8EDEFOT3                     source/elements/solid/solide8e/s8edefo3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|        JACOB_J33                     source/elements/solid/solide8e/s8edefo3.F
Chd|====================================================================
      SUBROUTINE S8EDEFOT3(
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    BXX1, BXX2, BXX3, BXX4, BXX5, BXX6, BXX7, BXX8,
     .    BYY1, BYY2, BYY3, BYY4, BYY5, BYY6, BYY7, BYY8,
     .    BZZ1, BZZ2, BZZ3, BZZ4, BZZ5, BZZ6, BZZ7, BZZ8,
     .    PXY1,PXY2,PXY3,PXY4,PXY5,PXY6,PXY7,PXY8,
     .    PYX1,PYX2,PYX3,PYX4,PYX5,PYX6,PYX7,PYX8,
     .    PXZ1,PXZ2,PXZ3,PXZ4,PXZ5,PXZ6,PXZ7,PXZ8,
     .    PZX1,PZX2,PZX3,PZX4,PZX5,PZX6,PZX7,PZX8,
     .    PYZ1,PYZ2,PYZ3,PYZ4,PYZ5,PYZ6,PYZ7,PYZ8,
     .    PZY1,PZY2,PZY3,PZY4,PZY5,PZY6,PZY7,PZY8,
     .    VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .    VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .    VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .    DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ,ICP,
     .    DET0,JFAC ,NU  ,I_SH,IDEGE,DXX_A,DYY_A,DZZ_A,OFFG)
C                                                                     12
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICP,I_SH,IDEGE(*)
C     REAL
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PY5(*), PY6(*), PY7(*), PY8(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*),DXX_A(*),DYY_A(*),DZZ_A(*),
     .   PXY1(MVSIZ),PXY2(MVSIZ),PXY3(MVSIZ),PXY4(MVSIZ),
     .   PXY5(MVSIZ),PXY6(MVSIZ),PXY7(MVSIZ),PXY8(MVSIZ),
     .   PYX1(MVSIZ),PYX2(MVSIZ),PYX3(MVSIZ),PYX4(MVSIZ),
     .   PYX5(MVSIZ),PYX6(MVSIZ),PYX7(MVSIZ),PYX8(MVSIZ),
     .   PXZ1(MVSIZ),PXZ2(MVSIZ),PXZ3(MVSIZ),PXZ4(MVSIZ),
     .   PXZ5(MVSIZ),PXZ6(MVSIZ),PXZ7(MVSIZ),PXZ8(MVSIZ),
     .   PZX1(MVSIZ),PZX2(MVSIZ),PZX3(MVSIZ),PZX4(MVSIZ),
     .   PZX5(MVSIZ),PZX6(MVSIZ),PZX7(MVSIZ),PZX8(MVSIZ),
     .   PYZ1(MVSIZ),PYZ2(MVSIZ),PYZ3(MVSIZ),PYZ4(MVSIZ),
     .   PYZ5(MVSIZ),PYZ6(MVSIZ),PYZ7(MVSIZ),PYZ8(MVSIZ),
     .   PZY1(MVSIZ),PZY2(MVSIZ),PZY3(MVSIZ),PZY4(MVSIZ),
     .   PZY5(MVSIZ),PZY6(MVSIZ),PZY7(MVSIZ),PZY8(MVSIZ),
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
     .   BZZ5(*),BZZ6(*),BZZ7(*),BZZ8(*),DET0(*),JFAC(*),NU(*),OFFG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   DET(MVSIZ),FAC,BASE
C-----------------------------------------------
C---- this sub_routine has been called 2 times, one for Ismstr=10,12 another for 11
      IF (ISMSTR==11.AND.I_SH>0) THEN
       DO I=LFT,LLT
       DXY(I) =PXY1(I)*VX1(I)+PXY2(I)*VX2(I)
     +        +PXY3(I)*VX3(I)+PXY4(I)*VX4(I)
     +        +PXY5(I)*VX5(I)+PXY6(I)*VX6(I)
     +        +PXY7(I)*VX7(I)+PXY8(I)*VX8(I)
       DXZ(I) =PXZ1(I)*VX1(I)+PXZ2(I)*VX2(I)
     +        +PXZ3(I)*VX3(I)+PXZ4(I)*VX4(I)
     +        +PXZ5(I)*VX5(I)+PXZ6(I)*VX6(I)
     +        +PXZ7(I)*VX7(I)+PXZ8(I)*VX8(I)
       DYX(I) =PYX1(I)*VY1(I)+PYX2(I)*VY2(I)
     +        +PYX3(I)*VY3(I)+PYX4(I)*VY4(I)
     +        +PYX5(I)*VY5(I)+PYX6(I)*VY6(I)
     +        +PYX7(I)*VY7(I)+PYX8(I)*VY8(I)
       DYZ(I) =PYZ1(I)*VY1(I)+PYZ2(I)*VY2(I)
     +        +PYZ3(I)*VY3(I)+PYZ4(I)*VY4(I)
     +        +PYZ5(I)*VY5(I)+PYZ6(I)*VY6(I)
     +        +PYZ7(I)*VY7(I)+PYZ8(I)*VY8(I)
       DZX(I) =PZX1(I)*VZ1(I)+PZX2(I)*VZ2(I)
     +        +PZX3(I)*VZ3(I)+PZX4(I)*VZ4(I)
     +        +PZX5(I)*VZ5(I)+PZX6(I)*VZ6(I)
     +        +PZX7(I)*VZ7(I)+PZX8(I)*VZ8(I)
       DZY(I) =PZY1(I)*VZ1(I)+PZY2(I)*VZ2(I)
     +        +PZY3(I)*VZ3(I)+PZY4(I)*VZ4(I)
     +        +PZY5(I)*VZ5(I)+PZY6(I)*VZ6(I)
     +        +PZY7(I)*VZ7(I)+PZY8(I)*VZ8(I)
       ENDDO
      ELSE
       DO I=LFT,LLT
	    IF (ISMSTR==12.AND.OFFG(I)>ONE) CYCLE
       DXY(I) =PY1(I)*VX1(I)+PY2(I)*VX2(I)+PY3(I)*VX3(I)+PY4(I)*VX4(I)
     +        +PY5(I)*VX5(I)+PY6(I)*VX6(I)+PY7(I)*VX7(I)+PY8(I)*VX8(I)
       DXZ(I) =PZ1(I)*VX1(I)+PZ2(I)*VX2(I)+PZ3(I)*VX3(I)+PZ4(I)*VX4(I)
     +        +PZ5(I)*VX5(I)+PZ6(I)*VX6(I)+PZ7(I)*VX7(I)+PZ8(I)*VX8(I)
       DYX(I) =PX1(I)*VY1(I)+PX2(I)*VY2(I)+PX3(I)*VY3(I)+PX4(I)*VY4(I)
     +        +PX5(I)*VY5(I)+PX6(I)*VY6(I)+PX7(I)*VY7(I)+PX8(I)*VY8(I)
       DYZ(I) =PZ1(I)*VY1(I)+PZ2(I)*VY2(I)+PZ3(I)*VY3(I)+PZ4(I)*VY4(I)
     +        +PZ5(I)*VY5(I)+PZ6(I)*VY6(I)+PZ7(I)*VY7(I)+PZ8(I)*VY8(I)
       DZX(I) =PX1(I)*VZ1(I)+PX2(I)*VZ2(I)+PX3(I)*VZ3(I)+PX4(I)*VZ4(I)
     +        +PX5(I)*VZ5(I)+PX6(I)*VZ6(I)+PX7(I)*VZ7(I)+PX8(I)*VZ8(I)
       DZY(I) =PY1(I)*VZ1(I)+PY2(I)*VZ2(I)+PY3(I)*VZ3(I)+PY4(I)*VZ4(I)
     +        +PY5(I)*VZ5(I)+PY6(I)*VZ6(I)+PY7(I)*VZ7(I)+PY8(I)*VZ8(I)
       ENDDO
      END IF !(ISMSTR==11) THEN
C
      IF (ICP==11) THEN
#include   "nofusion.inc"
       DO I=LFT,LLT
        DXX(I) =BXX1(I)*VX1(I)+BXX2(I)*VX2(I)+BXX3(I)*VX3(I)+
     .          BXX4(I)*VX4(I)+BXX5(I)*VX5(I)+BXX6(I)*VX6(I)+
     .          BXX7(I)*VX7(I)+BXX8(I)*VX8(I)
        DYY(I) =BYY1(I)*VY1(I)+BYY2(I)*VY2(I)+BYY3(I)*VY3(I)+
     .          BYY4(I)*VY4(I)+BYY5(I)*VY5(I)+BYY6(I)*VY6(I)+
     .          BYY7(I)*VY7(I)+BYY8(I)*VY8(I)
        DZZ(I) =BZZ1(I)*VZ1(I)+BZZ2(I)*VZ2(I)+BZZ3(I)*VZ3(I)+
     .          BZZ4(I)*VZ4(I)+BZZ5(I)*VZ5(I)+BZZ6(I)*VZ6(I)+
     .          BZZ7(I)*VZ7(I)+BZZ8(I)*VZ8(I)
        DYY(I) =DYY(I)+BXY1(I)*VX1(I)+BXY2(I)*VX2(I)
     +        +BXY3(I)*VX3(I)+BXY4(I)*VX4(I)
     +        +BXY5(I)*VX5(I)+BXY6(I)*VX6(I)
     +        +BXY7(I)*VX7(I)+BXY8(I)*VX8(I)
        DZZ(I) =DZZ(I)+BXZ1(I)*VX1(I)+BXZ2(I)*VX2(I)
     +        +BXZ3(I)*VX3(I)+BXZ4(I)*VX4(I)
     +        +BXZ5(I)*VX5(I)+BXZ6(I)*VX6(I)
     +        +BXZ7(I)*VX7(I)+BXZ8(I)*VX8(I)
        DXX(I) =DXX(I)+BYX1(I)*VY1(I)+BYX2(I)*VY2(I)
     +        +BYX3(I)*VY3(I)+BYX4(I)*VY4(I)
     +        +BYX5(I)*VY5(I)+BYX6(I)*VY6(I)
     +        +BYX7(I)*VY7(I)+BYX8(I)*VY8(I)
        DZZ(I) =DZZ(I)+BYZ1(I)*VY1(I)+BYZ2(I)*VY2(I)
     +        +BYZ3(I)*VY3(I)+BYZ4(I)*VY4(I)
     +        +BYZ5(I)*VY5(I)+BYZ6(I)*VY6(I)
     +        +BYZ7(I)*VY7(I)+BYZ8(I)*VY8(I)
        DXX(I) =DXX(I)+BZX1(I)*VZ1(I)+BZX2(I)*VZ2(I)
     +        +BZX3(I)*VZ3(I)+BZX4(I)*VZ4(I)
     +        +BZX5(I)*VZ5(I)+BZX6(I)*VZ6(I)
     +        +BZX7(I)*VZ7(I)+BZX8(I)*VZ8(I)
        DYY(I) =DYY(I)+BZY1(I)*VZ1(I)+BZY2(I)*VZ2(I)
     +        +BZY3(I)*VZ3(I)+BZY4(I)*VZ4(I)
     +        +BZY5(I)*VZ5(I)+BZY6(I)*VZ6(I)
     +        +BZY7(I)*VZ7(I)+BZY8(I)*VZ8(I)
       ENDDO
      ELSE
       DO I=LFT,LLT
	    IF (ISMSTR==12.AND.OFFG(I)>ONE) CYCLE
       DXX(I) =PX1(I)*VX1(I)+PX2(I)*VX2(I)+PX3(I)*VX3(I)+PX4(I)*VX4(I)
     .        +PX5(I)*VX5(I)+PX6(I)*VX6(I)+PX7(I)*VX7(I)+PX8(I)*VX8(I)
       DYY(I) =PY1(I)*VY1(I)+PY2(I)*VY2(I)+PY3(I)*VY3(I)+PY4(I)*VY4(I)
     .        +PY5(I)*VY5(I)+PY6(I)*VY6(I)+PY7(I)*VY7(I)+PY8(I)*VY8(I)
       DZZ(I) =PZ1(I)*VZ1(I)+PZ2(I)*VZ2(I)+PZ3(I)*VZ3(I)+PZ4(I)*VZ4(I)
     .        +PZ5(I)*VZ5(I)+PZ6(I)*VZ6(I)+PZ7(I)*VZ7(I)+PZ8(I)*VZ8(I)
       ENDDO
      END IF !(ICP==11) THEN
C   
      IF (I_SH>1) THEN
       IF (ISMSTR==11) THEN
#include   "nofusion.inc"
       DO I=LFT,LLT
        DYY(I) =DYY(I)+BXY1(I)*VX1(I)+BXY2(I)*VX2(I)
     +        +BXY3(I)*VX3(I)+BXY4(I)*VX4(I)
     +        +BXY5(I)*VX5(I)+BXY6(I)*VX6(I)
     +        +BXY7(I)*VX7(I)+BXY8(I)*VX8(I)
        DZZ(I) =DZZ(I)+BXZ1(I)*VX1(I)+BXZ2(I)*VX2(I)
     +        +BXZ3(I)*VX3(I)+BXZ4(I)*VX4(I)
     +        +BXZ5(I)*VX5(I)+BXZ6(I)*VX6(I)
     +        +BXZ7(I)*VX7(I)+BXZ8(I)*VX8(I)
        DXX(I) =DXX(I)+BYX1(I)*VY1(I)+BYX2(I)*VY2(I)
     +        +BYX3(I)*VY3(I)+BYX4(I)*VY4(I)
     +        +BYX5(I)*VY5(I)+BYX6(I)*VY6(I)
     +        +BYX7(I)*VY7(I)+BYX8(I)*VY8(I)
        DZZ(I) =DZZ(I)+BYZ1(I)*VY1(I)+BYZ2(I)*VY2(I)
     +        +BYZ3(I)*VY3(I)+BYZ4(I)*VY4(I)
     +        +BYZ5(I)*VY5(I)+BYZ6(I)*VY6(I)
     +        +BYZ7(I)*VY7(I)+BYZ8(I)*VY8(I)
        DXX(I) =DXX(I)+BZX1(I)*VZ1(I)+BZX2(I)*VZ2(I)
     +        +BZX3(I)*VZ3(I)+BZX4(I)*VZ4(I)
     +        +BZX5(I)*VZ5(I)+BZX6(I)*VZ6(I)
     +        +BZX7(I)*VZ7(I)+BZX8(I)*VZ8(I)
        DYY(I) =DYY(I)+BZY1(I)*VZ1(I)+BZY2(I)*VZ2(I)
     +        +BZY3(I)*VZ3(I)+BZY4(I)*VZ4(I)
     +        +BZY5(I)*VZ5(I)+BZY6(I)*VZ6(I)
     +        +BZY7(I)*VZ7(I)+BZY8(I)*VZ8(I)
       ENDDO
       ELSE
         DO I=LFT,LLT
           DYY_A(I) =BXY1(I)*VX1(I)+BXY2(I)*VX2(I)
     +              +BXY3(I)*VX3(I)+BXY4(I)*VX4(I)
     +              +BXY5(I)*VX5(I)+BXY6(I)*VX6(I)
     +              +BXY7(I)*VX7(I)+BXY8(I)*VX8(I)
     +              +BZY1(I)*VZ1(I)+BZY2(I)*VZ2(I)
     +              +BZY3(I)*VZ3(I)+BZY4(I)*VZ4(I)
     +              +BZY5(I)*VZ5(I)+BZY6(I)*VZ6(I)
     +              +BZY7(I)*VZ7(I)+BZY8(I)*VZ8(I)
           DZZ_A(I) =BXZ1(I)*VX1(I)+BXZ2(I)*VX2(I)
     +              +BXZ3(I)*VX3(I)+BXZ4(I)*VX4(I)
     +              +BXZ5(I)*VX5(I)+BXZ6(I)*VX6(I)
     +              +BXZ7(I)*VX7(I)+BXZ8(I)*VX8(I)
     +              +BYZ1(I)*VY1(I)+BYZ2(I)*VY2(I)
     +              +BYZ3(I)*VY3(I)+BYZ4(I)*VY4(I)
     +              +BYZ5(I)*VY5(I)+BYZ6(I)*VY6(I)
     +              +BYZ7(I)*VY7(I)+BYZ8(I)*VY8(I)
           DXX_A(I) =BYX1(I)*VY1(I)+BYX2(I)*VY2(I)
     +              +BYX3(I)*VY3(I)+BYX4(I)*VY4(I)
     +              +BYX5(I)*VY5(I)+BYX6(I)*VY6(I)
     +              +BYX7(I)*VY7(I)+BYX8(I)*VY8(I)
     +              +BZX1(I)*VZ1(I)+BZX2(I)*VZ2(I)
     +              +BZX3(I)*VZ3(I)+BZX4(I)*VZ4(I)
     +              +BZX5(I)*VZ5(I)+BZX6(I)*VZ6(I)
     +              +BZX7(I)*VZ7(I)+BZX8(I)*VZ8(I)
         ENDDO
       END IF !(ISMSTR==11) THEN
      END IF !(I_SH>1) THEN
C      
      IF (ICP==1) THEN
       CALL JACOB_J33(DET,
     .                DXX, DXY, DXZ,
     .                DYX, DYY, DYZ,
     .                DZX, DZY, DZZ)
C-----F'=FAC*F=MF'+[1]-->MF'=FAC*MF+(FAC-ONE)*[1]
        DO I=LFT,LLT
         FAC=TWO_THIRD*NU(I)
         BASE = DET0(I)/MAX(EM20,DET(I))
         IF(ISMSTR==12.AND.OFFG(I)>ONE) BASE=ONE
         JFAC(I)=EXP(FAC*LOG(MAX(EM20,BASE)))
c         JFAC(I)=(DET0(I)/MAX(EM20,DET(I)))**FAC
        ENDDO
       DO I=LFT,LLT
        IF(IDEGE(I)>10.OR.(ISMSTR==12.AND.OFFG(I)>ONE)) CYCLE
        DXX(I) = JFAC(I)*DXX(I)+JFAC(I)-ONE
        DYY(I) = JFAC(I)*DYY(I)+JFAC(I)-ONE
        DZZ(I) = JFAC(I)*DZZ(I)+JFAC(I)-ONE
        DXY(I) = JFAC(I)*DXY(I)
        DYX(I) = JFAC(I)*DYX(I)
        DZX(I) = JFAC(I)*DZX(I)
        DXZ(I) = JFAC(I)*DXZ(I)
        DYZ(I) = JFAC(I)*DYZ(I)
        DZY(I) = JFAC(I)*DZY(I)
       ENDDO
      END IF !(ICP==1) THEN
C
      RETURN
      END
Chd|====================================================================
Chd|  S8EDEFOC3                     source/elements/solid/solide8e/s8edefo3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EDEFOC3(
     .    PXC1, PXC2, PXC3, PXC4, 
     .    PYC1, PYC2, PYC3, PYC4, 
     .    PZC1, PZC2, PZC3, PZC4, 
     .    VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .    VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .    VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .    DVCA)
C                                                                     12
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
     .   PXC1(*), PXC2(*), PXC3(*), PXC4(*),
     .   PYC1(*), PYC2(*), PYC3(*), PYC4(*),
     .   PZC1(*), PZC2(*), PZC3(*), PZC4(*), DVCA(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
       DO I=LFT,LLT
        DVCA(I)=PXC1(I)*(VX1(I)-VX7(I))+PXC2(I)*(VX2(I)-VX8(I))
     .         +PXC3(I)*(VX3(I)-VX5(I))+PXC4(I)*(VX4(I)-VX6(I))
     .         +PYC1(I)*(VY1(I)-VY7(I))+PYC2(I)*(VY2(I)-VY8(I))
     .         +PYC3(I)*(VY3(I)-VY5(I))+PYC4(I)*(VY4(I)-VY6(I))
     .         +PZC1(I)*(VZ1(I)-VZ7(I))+PZC2(I)*(VZ2(I)-VZ8(I))
     .         +PZC3(I)*(VZ3(I)-VZ5(I))+PZC4(I)*(VZ4(I)-VZ6(I))
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  JACOB_J33                     source/elements/solid/solide8e/s8edefo3.F
Chd|-- called by -----------
Chd|        JACOB_F0                      source/elements/solid/solide8e/s8edefo3.F
Chd|        S10VOLNODT3                   source/elements/solid/solide4_sfem/s10volnodt3.F
Chd|        S10_ICP                       source/elements/solid/solide4_sfem/s10_icp.F
Chd|        S4VOLN_M                      source/elements/solid/solide4_sfem/s4voln_m.F
Chd|        S8EDEFOT3                     source/elements/solid/solide8e/s8edefo3.F
Chd|        S8ZDEFOT3                     source/elements/solid/solide8z/s8zdefo3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE JACOB_J33(DET,
     .                     AJ1,AJ2,AJ3,
     .                     AJ4,AJ5,AJ6,
     .                     AJ7,AJ8,AJ9)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   AJ1(*),AJ2(*),AJ3(*),
     .   AJ4(*),AJ5(*),AJ6(*),
     .   AJ7(*),AJ8(*),AJ9(*),
     .   DET(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
C                                                                     12
      my_real
     .   JAC_59_68, JAC_67_49, JAC_48_57,
     .   AJ11, AJ22, AJ33
C-----------------------------------------------
C     JACOBIAN of [AJ]+[1]
C
      DO I=LFT,LLT
       AJ11=AJ1(I)+ONE
       AJ22=AJ5(I)+ONE
       AJ33=AJ9(I)+ONE
       JAC_59_68=AJ22*AJ33-AJ6(I)*AJ8(I)
       JAC_67_49=AJ6(I)*AJ7(I)-AJ4(I)*AJ33
       JAC_48_57=AJ4(I)*AJ8(I)-AJ22*AJ7(I)
C
       DET(I)=AJ11*JAC_59_68+AJ2(I)*JAC_67_49+AJ3(I)*JAC_48_57
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  JACOB_F0                      source/elements/solid/solide8e/s8edefo3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|        JACOB_J33                     source/elements/solid/solide8e/s8edefo3.F
Chd|        SDEFOT3                       source/elements/solid/solide/sdefo3.F
Chd|====================================================================
      SUBROUTINE JACOB_F0(DET0,
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8)
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
C     REAL
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), DET0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
C                                                                     12
      my_real
     .   DXX(MVSIZ), DXY(MVSIZ), DXZ(MVSIZ),
     .   DYX(MVSIZ), DYY(MVSIZ), DYZ(MVSIZ),
     .   DZX(MVSIZ), DZY(MVSIZ), DZZ(MVSIZ)
C-----------------------------------------------
       CALL SDEFOT3(
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ)
C-----------
       CALL JACOB_J33(DET0,
     .                DXX, DXY, DXZ,
     .                DYX, DYY, DYZ,
     .                DZX, DZY, DZZ)
      RETURN
      END
Chd|====================================================================
Chd|  S8EDEFW3                      source/elements/solid/solide8e/s8edefo3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EDEFW3(
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4,
     .   VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .   VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .   VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .   WXX, WYY, WZZ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   WXX(*), WYY(*), WZZ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J
C     REAL
      my_real
     .   DXY, DYX, DXZ,
     .   DZY, DYZ, DZX,
     .   VX17, VY17, VZ17,
     .   VX28, VY28, VZ28,
     .   VX35, VY35, VZ35,
     .   VX46, VY46, VZ46, DT1D2, DT1D
C-----------------------------------------------
      DT1D2=HALF*DT1
       DO I=LFT,LLT
         VX17=VX1(I)-VX7(I)
         VX28=VX2(I)-VX8(I)
         VX35=VX3(I)-VX5(I)
         VX46=VX4(I)-VX6(I)
         VY17=VY1(I)-VY7(I)
         VY28=VY2(I)-VY8(I)
         VY35=VY3(I)-VY5(I)
         VY46=VY4(I)-VY6(I)
         VZ17=VZ1(I)-VZ7(I)
         VZ28=VZ2(I)-VZ8(I)
         VZ35=VZ3(I)-VZ5(I)
         VZ46=VZ4(I)-VZ6(I)
         DXY=PY1(I)*VX17+PY2(I)*VX28+
     .  	PY3(I)*VX35+PY4(I)*VX46
         DXZ=PZ1(I)*VX17+PZ2(I)*VX28+
     .  	PZ3(I)*VX35+PZ4(I)*VX46
         DYX=PX1(I)*VY17+PX2(I)*VY28+
     .  	PX3(I)*VY35+PX4(I)*VY46
         DYZ=PZ1(I)*VY17+PZ2(I)*VY28+
     .  	PZ3(I)*VY35+PZ4(I)*VY46
         DZX=PX1(I)*VZ17+PX2(I)*VZ28+
     .  	PX3(I)*VZ35+PX4(I)*VZ46
         DZY=PY1(I)*VZ17+PY2(I)*VZ28+
     .  	PY3(I)*VZ35+PY4(I)*VZ46
C
        WZZ(I)=DT1D2*(DYX-DXY)
        WYY(I)=DT1D2*(DXZ-DZX)
        WXX(I)=DT1D2*(DZY-DYZ)
       ENDDO
C
C
      RETURN
C
      END
Chd|====================================================================
Chd|  S8ESELECSHT                   source/elements/solid/solide8e/s8edefo3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ESELECSHT(NPTR,NPTS,NPTT,NNPT,
     .                       MFXX ,MFXY ,MFXZ ,
     .                       MFYX ,MFYY ,MFYZ ,
     .                       MFZX ,MFZY ,MFZZ ,
     .                       BXX  ,BYY  ,BZZ  ,
     .                       BXY  ,BYZ  ,BXZ  ,I_SH ,OFFG)
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPTR,NPTS,NPTT,NNPT ,I_SH
C     REAL
      my_real
     .  MFXX(MVSIZ,NNPT),MFXY(MVSIZ,NNPT),MFXZ(MVSIZ,NNPT),
     .  MFYX(MVSIZ,NNPT),MFYY(MVSIZ,NNPT),MFYZ(MVSIZ,NNPT),
     .  MFZX(MVSIZ,NNPT),MFZY(MVSIZ,NNPT),MFZZ(MVSIZ,NNPT),
     .  BXX(MVSIZ,NNPT),BYY(MVSIZ,NNPT),BZZ(MVSIZ,NNPT),
     .  BXY(MVSIZ,NNPT),BXZ(MVSIZ,NNPT),BYZ(MVSIZ,NNPT),OFFG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,IR,IS,IT,IP
      my_real
     .  B0XY(MVSIZ,2),B0XZ(MVSIZ,2),B0YZ(MVSIZ,2),
     .  B0YX(MVSIZ,2),B0ZX(MVSIZ,2),B0ZY(MVSIZ,2)
C------------
       IF (ISMSTR/=11) THEN      
         DO IP = 1,NNPT
           DO I=LFT,LLT
C ----[B]=[MF+1][MF+1]^t -1 
            IF (ISMSTR==12.AND.OFFG(I) >ONE) CYCLE 
            BXX(I,IP)=MFXX(I,IP)*(TWO+MFXX(I,IP))+
     .                MFXY(I,IP)*MFXY(I,IP)+MFXZ(I,IP)*MFXZ(I,IP) 
            BYY(I,IP)=MFYY(I,IP)*(TWO+MFYY(I,IP))+
     .                MFYX(I,IP)*MFYX(I,IP)+MFYZ(I,IP)*MFYZ(I,IP)
            BZZ(I,IP)=MFZZ(I,IP)*(TWO+MFZZ(I,IP))+
     .                MFZX(I,IP)*MFZX(I,IP)+MFZY(I,IP)*MFZY(I,IP)
            BXY(I,IP)=MFXY(I,IP)+MFYX(I,IP)+MFXX(I,IP)*MFYX(I,IP)+
     .                MFXY(I,IP)*MFYY(I,IP)+MFXZ(I,IP)*MFYZ(I,IP)
            BXZ(I,IP)=MFXZ(I,IP)+MFZX(I,IP)+MFXX(I,IP)*MFZX(I,IP)+
     .                MFXY(I,IP)*MFZY(I,IP)+MFXZ(I,IP)*MFZZ(I,IP)
            BYZ(I,IP)=MFZY(I,IP)+MFYZ(I,IP)+MFZX(I,IP)*MFYX(I,IP)+
     .                MFZY(I,IP)*MFYY(I,IP)+MFZZ(I,IP)*MFYZ(I,IP)  
           ENDDO
         END DO !IP = 1
       END IF !(ISMSTR/=11) THEN               
C	
        DO IR = 1,2
          B0XY(LFT:LLT,IR) = ZERO
          B0XZ(LFT:LLT,IR) = ZERO
          B0YZ(LFT:LLT,IR) = ZERO
        END DO !IR = 1,2
C	
      IF (ISMSTR==11) THEN                           
        DO IR = 1,2
          B0YX(LFT:LLT,IR) = ZERO
          B0ZX(LFT:LLT,IR) = ZERO
          B0ZY(LFT:LLT,IR) = ZERO
        END DO !IR = 1,2
       DO IR=1,NPTR
        DO IS=1,NPTS
        DO IT=1,NPTT
C-----------
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
C
C------ moyen in 3 directions
         DO I=LFT,LLT
           B0XY(I,IR) = B0XY(I,IR)+MFXY(I,IP)
           B0XZ(I,IT) = B0XZ(I,IT)+MFXZ(I,IP)
           B0YZ(I,IS) = B0YZ(I,IS)+MFYZ(I,IP)
           B0YX(I,IR) = B0YX(I,IR)+MFYX(I,IP)
           B0ZX(I,IT) = B0ZX(I,IT)+MFZX(I,IP)
           B0ZY(I,IS) = B0ZY(I,IS)+MFZY(I,IP)
         END DO
        ENDDO    !  IT=1,NPTT
        ENDDO     !  IS=1,NPTS
       ENDDO      !  IR=1,NPTR
C       
       DO IR = 1,2
         DO I=LFT,LLT
           B0XY(I,IR) = B0XY(I,IR)*FOURTH
           B0XZ(I,IR) = B0XZ(I,IR)*FOURTH
           B0YZ(I,IR) = B0YZ(I,IR)*FOURTH
           B0YX(I,IR) = B0YX(I,IR)*FOURTH
           B0ZX(I,IR) = B0ZX(I,IR)*FOURTH
           B0ZY(I,IR) = B0ZY(I,IR)*FOURTH
         END DO
       END DO !IR = 1,2
C---------takes the selective mean values-----
       DO IR=1,NPTR
        DO IS=1,NPTS
        DO IT=1,NPTT
C-----------
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
C
         DO I=LFT,LLT
           MFXY(I,IP) = B0XY(I,IR)
           MFXZ(I,IP) = B0XZ(I,IT)
           MFYZ(I,IP) = B0YZ(I,IS)
           MFYX(I,IP) = B0YX(I,IR)
           MFZX(I,IP) = B0ZX(I,IT)
           MFZY(I,IP) = B0ZY(I,IS)
         END DO
        ENDDO    !  IT=1,NPTT
        ENDDO     !  IS=1,NPTS
       ENDDO      !  IR=1,NPTR
      ELSE                                     
       DO IR=1,NPTR
        DO IS=1,NPTS
        DO IT=1,NPTT
C-----------
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
C
C------ moyen in 3 directions
         DO I=LFT,LLT
           IF (ISMSTR==12.AND.OFFG(I) >ONE) CYCLE 
           B0XY(I,IR) = B0XY(I,IR)+BXY(I,IP)
           B0XZ(I,IT) = B0XZ(I,IT)+BXZ(I,IP)
           B0YZ(I,IS) = B0YZ(I,IS)+BYZ(I,IP)
         END DO
        ENDDO    !  IT=1,NPTT
        ENDDO     !  IS=1,NPTS
       ENDDO      !  IR=1,NPTR
C       
       DO IR = 1,2
         DO I=LFT,LLT
           IF (ISMSTR==12.AND.OFFG(I) >ONE) CYCLE 
           B0XY(I,IR) = B0XY(I,IR)*FOURTH
           B0XZ(I,IR) = B0XZ(I,IR)*FOURTH
           B0YZ(I,IR) = B0YZ(I,IR)*FOURTH
         END DO
       END DO !IR = 1,2
C---------takes the selective mean values-----
       DO IR=1,NPTR
        DO IS=1,NPTS
        DO IT=1,NPTT
C-----------
         IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
C
         DO I=LFT,LLT
           IF (ISMSTR==12.AND.OFFG(I) >ONE) CYCLE 
           BXY(I,IP) = B0XY(I,IR)
           BXZ(I,IP) = B0XZ(I,IT)
           BYZ(I,IP) = B0YZ(I,IS)
         END DO
        ENDDO    !  IT=1,NPTT
        ENDDO     !  IS=1,NPTS
       ENDDO      !  IR=1,NPTR
      END IF !(ISMSTR==11) THEN  
C       
      RETURN
       END
Chd|====================================================================
Chd|  S8ETOTSH10                    source/elements/solid/solide8e/s8edefo3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ETOTSH10(MB ,
     .                       BXX  ,BYY  ,BZZ  ,
     .                       BXY  ,BYZ  ,BXZ  ,NEL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .  MB(NEL,6),BXX(*),BYY(*),BZZ(*),
     .  BXY(*),BXZ(*),BYZ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C
        DO I=LFT,LLT
	  MB(I,1) = BXX(I)
	  MB(I,2) = BYY(I)
	  MB(I,3) = BZZ(I)
	  MB(I,4) = BXY(I)
	  MB(I,5) = BYZ(I)
	  MB(I,6) = BXZ(I)
        END DO
C       
      RETURN
       END
Chd|====================================================================
Chd|  S8EDEFOT12                    source/elements/solid/solide8e/s8edefo3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EDEFOT12(
     .    PX1, PX2, PX3, PX4, PX5, PX6, PX7, PX8,
     .    PY1, PY2, PY3, PY4, PY5, PY6, PY7, PY8,
     .    PZ1, PZ2, PZ3, PZ4, PZ5, PZ6, PZ7, PZ8,
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    BXX1, BXX2, BXX3, BXX4, BXX5, BXX6, BXX7, BXX8,
     .    BYY1, BYY2, BYY3, BYY4, BYY5, BYY6, BYY7, BYY8,
     .    BZZ1, BZZ2, BZZ3, BZZ4, BZZ5, BZZ6, BZZ7, BZZ8,
     .    PXY1,PXY2,PXY3,PXY4,PXY5,PXY6,PXY7,PXY8,
     .    PYX1,PYX2,PYX3,PYX4,PYX5,PYX6,PYX7,PYX8,
     .    PXZ1,PXZ2,PXZ3,PXZ4,PXZ5,PXZ6,PXZ7,PXZ8,
     .    PZX1,PZX2,PZX3,PZX4,PZX5,PZX6,PZX7,PZX8,
     .    PYZ1,PYZ2,PYZ3,PYZ4,PYZ5,PYZ6,PYZ7,PYZ8,
     .    PZY1,PZY2,PZY3,PZY4,PZY5,PZY6,PZY7,PZY8,
     .    VX1, VX2, VX3, VX4, VX5, VX6, VX7, VX8,
     .    VY1, VY2, VY3, VY4, VY5, VY6, VY7, VY8,
     .    VZ1, VZ2, VZ3, VZ4, VZ5, VZ6, VZ7, VZ8,
     .    R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .    DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY, DZZ,
     .    ICP ,DET0,JFAC ,NU ,IDEGE,OFFG ,ISEL_V ,I_SH )
C                                                                     12
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISEL_V,I_SH,ICP,IDEGE(*)
C     REAL
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PY5(*), PY6(*), PY7(*), PY8(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),
     .   PXY1(MVSIZ),PXY2(MVSIZ),PXY3(MVSIZ),PXY4(MVSIZ),
     .   PXY5(MVSIZ),PXY6(MVSIZ),PXY7(MVSIZ),PXY8(MVSIZ),
     .   PYX1(MVSIZ),PYX2(MVSIZ),PYX3(MVSIZ),PYX4(MVSIZ),
     .   PYX5(MVSIZ),PYX6(MVSIZ),PYX7(MVSIZ),PYX8(MVSIZ),
     .   PXZ1(MVSIZ),PXZ2(MVSIZ),PXZ3(MVSIZ),PXZ4(MVSIZ),
     .   PXZ5(MVSIZ),PXZ6(MVSIZ),PXZ7(MVSIZ),PXZ8(MVSIZ),
     .   PZX1(MVSIZ),PZX2(MVSIZ),PZX3(MVSIZ),PZX4(MVSIZ),
     .   PZX5(MVSIZ),PZX6(MVSIZ),PZX7(MVSIZ),PZX8(MVSIZ),
     .   PYZ1(MVSIZ),PYZ2(MVSIZ),PYZ3(MVSIZ),PYZ4(MVSIZ),
     .   PYZ5(MVSIZ),PYZ6(MVSIZ),PYZ7(MVSIZ),PYZ8(MVSIZ),
     .   PZY1(MVSIZ),PZY2(MVSIZ),PZY3(MVSIZ),PZY4(MVSIZ),
     .   PZY5(MVSIZ),PZY6(MVSIZ),PZY7(MVSIZ),PZY8(MVSIZ),
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
     .   R11(*), R12(*), R13(*),
     .   R21(*), R22(*), R23(*),
     .   R31(*), R32(*), R33(*),
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*),
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
     .   BZZ5(*),BZZ6(*),BZZ7(*),BZZ8(*),OFFG(*),DET0(*),JFAC(*),NU(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   VLX1(MVSIZ), VLX2(MVSIZ), VLX3(MVSIZ), VLX4(MVSIZ),
     .   VLX5(MVSIZ), VLX6(MVSIZ), VLX7(MVSIZ), VLX8(MVSIZ),
     .   VLY1(MVSIZ), VLY2(MVSIZ), VLY3(MVSIZ), VLY4(MVSIZ),
     .   VLY5(MVSIZ), VLY6(MVSIZ), VLY7(MVSIZ), VLY8(MVSIZ),
     .   VLZ1(MVSIZ), VLZ2(MVSIZ), VLZ3(MVSIZ), VLZ4(MVSIZ),
     .   VLZ5(MVSIZ), VLZ6(MVSIZ), VLZ7(MVSIZ), VLZ8(MVSIZ)
      my_real
     .   JAC_59_68, JAC_67_49, JAC_48_57,
     .   AJ11, AJ22, AJ33,DET(MVSIZ),FAC,BASE
C-----------------------------------------------
      IF (JCVT > 0 ) THEN
        DO I=LFT,LLT
	     IF(OFFG(I)<=ONE) CYCLE
         VLX1(I)=R11(I)*VX1(I)+R21(I)*VY1(I)+R31(I)*VZ1(I)
         VLY1(I)=R12(I)*VX1(I)+R22(I)*VY1(I)+R32(I)*VZ1(I)
         VLZ1(I)=R13(I)*VX1(I)+R23(I)*VY1(I)+R33(I)*VZ1(I)
         VLX2(I)=R11(I)*VX2(I)+R21(I)*VY2(I)+R31(I)*VZ2(I)
         VLY2(I)=R12(I)*VX2(I)+R22(I)*VY2(I)+R32(I)*VZ2(I)
         VLZ2(I)=R13(I)*VX2(I)+R23(I)*VY2(I)+R33(I)*VZ2(I)
         VLX3(I)=R11(I)*VX3(I)+R21(I)*VY3(I)+R31(I)*VZ3(I)
         VLY3(I)=R12(I)*VX3(I)+R22(I)*VY3(I)+R32(I)*VZ3(I)
         VLZ3(I)=R13(I)*VX3(I)+R23(I)*VY3(I)+R33(I)*VZ3(I)
         VLX4(I)=R11(I)*VX4(I)+R21(I)*VY4(I)+R31(I)*VZ4(I)
         VLY4(I)=R12(I)*VX4(I)+R22(I)*VY4(I)+R32(I)*VZ4(I)
         VLZ4(I)=R13(I)*VX4(I)+R23(I)*VY4(I)+R33(I)*VZ4(I)
         VLX5(I)=R11(I)*VX5(I)+R21(I)*VY5(I)+R31(I)*VZ5(I)
         VLY5(I)=R12(I)*VX5(I)+R22(I)*VY5(I)+R32(I)*VZ5(I)
         VLZ5(I)=R13(I)*VX5(I)+R23(I)*VY5(I)+R33(I)*VZ5(I)
         VLX6(I)=R11(I)*VX6(I)+R21(I)*VY6(I)+R31(I)*VZ6(I)
         VLY6(I)=R12(I)*VX6(I)+R22(I)*VY6(I)+R32(I)*VZ6(I)
         VLZ6(I)=R13(I)*VX6(I)+R23(I)*VY6(I)+R33(I)*VZ6(I)
         VLX7(I)=R11(I)*VX7(I)+R21(I)*VY7(I)+R31(I)*VZ7(I)
         VLY7(I)=R12(I)*VX7(I)+R22(I)*VY7(I)+R32(I)*VZ7(I)
         VLZ7(I)=R13(I)*VX7(I)+R23(I)*VY7(I)+R33(I)*VZ7(I)
         VLX8(I)=R11(I)*VX8(I)+R21(I)*VY8(I)+R31(I)*VZ8(I)
         VLY8(I)=R12(I)*VX8(I)+R22(I)*VY8(I)+R32(I)*VZ8(I)
         VLZ8(I)=R13(I)*VX8(I)+R23(I)*VY8(I)+R33(I)*VZ8(I)
        ENDDO
      ELSE
         VLX1(LFT:LLT)=VX1(LFT:LLT)
         VLY1(LFT:LLT)=VY1(LFT:LLT)
         VLZ1(LFT:LLT)=VZ1(LFT:LLT)
         VLX2(LFT:LLT)=VX2(LFT:LLT)
         VLY2(LFT:LLT)=VY2(LFT:LLT)
         VLZ2(LFT:LLT)=VZ2(LFT:LLT)
         VLX3(LFT:LLT)=VX3(LFT:LLT)
         VLY3(LFT:LLT)=VY3(LFT:LLT)
         VLZ3(LFT:LLT)=VZ3(LFT:LLT)
         VLX4(LFT:LLT)=VX4(LFT:LLT)
         VLY4(LFT:LLT)=VY4(LFT:LLT)
         VLZ4(LFT:LLT)=VZ4(LFT:LLT)
         VLX5(LFT:LLT)=VX5(LFT:LLT)
         VLY5(LFT:LLT)=VY5(LFT:LLT)
         VLZ5(LFT:LLT)=VZ5(LFT:LLT)
         VLX6(LFT:LLT)=VX6(LFT:LLT)
         VLY6(LFT:LLT)=VY6(LFT:LLT)
         VLZ6(LFT:LLT)=VZ6(LFT:LLT)
         VLX7(LFT:LLT)=VX7(LFT:LLT)
         VLY7(LFT:LLT)=VY7(LFT:LLT)
         VLZ7(LFT:LLT)=VZ7(LFT:LLT)
         VLX8(LFT:LLT)=VX8(LFT:LLT)
         VLY8(LFT:LLT)=VY8(LFT:LLT)
         VLZ8(LFT:LLT)=VZ8(LFT:LLT)
      END IF !(JCVT > 0
C   
      IF (I_SH>0) THEN
       DO I=LFT,LLT
	    IF(OFFG(I)<=ONE) CYCLE
        DXY(I) =PXY1(I)*VLX1(I)+PXY2(I)*VLX2(I)
     +         +PXY3(I)*VLX3(I)+PXY4(I)*VLX4(I)
     +         +PXY5(I)*VLX5(I)+PXY6(I)*VLX6(I)
     +         +PXY7(I)*VLX7(I)+PXY8(I)*VLX8(I)
        DXZ(I) =PXZ1(I)*VLX1(I)+PXZ2(I)*VLX2(I)
     +         +PXZ3(I)*VLX3(I)+PXZ4(I)*VLX4(I)
     +         +PXZ5(I)*VLX5(I)+PXZ6(I)*VLX6(I)
     +         +PXZ7(I)*VLX7(I)+PXZ8(I)*VLX8(I)
        DYX(I) =PYX1(I)*VLY1(I)+PYX2(I)*VLY2(I)
     +         +PYX3(I)*VLY3(I)+PYX4(I)*VLY4(I)
     +         +PYX5(I)*VLY5(I)+PYX6(I)*VLY6(I)
     +         +PYX7(I)*VLY7(I)+PYX8(I)*VLY8(I)
        DYZ(I) =PYZ1(I)*VLY1(I)+PYZ2(I)*VLY2(I)
     +         +PYZ3(I)*VLY3(I)+PYZ4(I)*VLY4(I)
     +         +PYZ5(I)*VLY5(I)+PYZ6(I)*VLY6(I)
     +         +PYZ7(I)*VLY7(I)+PYZ8(I)*VLY8(I)
        DZX(I) =PZX1(I)*VLZ1(I)+PZX2(I)*VLZ2(I)
     +         +PZX3(I)*VLZ3(I)+PZX4(I)*VLZ4(I)
     +         +PZX5(I)*VLZ5(I)+PZX6(I)*VLZ6(I)
     +         +PZX7(I)*VLZ7(I)+PZX8(I)*VLZ8(I)
        DZY(I) =PZY1(I)*VLZ1(I)+PZY2(I)*VLZ2(I)
     +         +PZY3(I)*VLZ3(I)+PZY4(I)*VLZ4(I)
     +         +PZY5(I)*VLZ5(I)+PZY6(I)*VLZ6(I)
     +         +PZY7(I)*VLZ7(I)+PZY8(I)*VLZ8(I)
       ENDDO
      ELSE
       DO I=LFT,LLT
	    IF(OFFG(I)<=ONE) CYCLE
        DXY(I) =PY1(I)*VLX1(I)+PY2(I)*VLX2(I)+PY3(I)*VLX3(I)+PY4(I)*VLX4(I)
     +         +PY5(I)*VLX5(I)+PY6(I)*VLX6(I)+PY7(I)*VLX7(I)+PY8(I)*VLX8(I)
        DXZ(I) =PZ1(I)*VLX1(I)+PZ2(I)*VLX2(I)+PZ3(I)*VLX3(I)+PZ4(I)*VLX4(I)
     +         +PZ5(I)*VLX5(I)+PZ6(I)*VLX6(I)+PZ7(I)*VLX7(I)+PZ8(I)*VLX8(I)
        DYX(I) =PX1(I)*VLY1(I)+PX2(I)*VLY2(I)+PX3(I)*VLY3(I)+PX4(I)*VLY4(I)
     +         +PX5(I)*VLY5(I)+PX6(I)*VLY6(I)+PX7(I)*VLY7(I)+PX8(I)*VLY8(I)
        DYZ(I) =PZ1(I)*VLY1(I)+PZ2(I)*VLY2(I)+PZ3(I)*VLY3(I)+PZ4(I)*VLY4(I)
     +         +PZ5(I)*VLY5(I)+PZ6(I)*VLY6(I)+PZ7(I)*VLY7(I)+PZ8(I)*VLY8(I)
        DZX(I) =PX1(I)*VLZ1(I)+PX2(I)*VLZ2(I)+PX3(I)*VLZ3(I)+PX4(I)*VLZ4(I)
     +         +PX5(I)*VLZ5(I)+PX6(I)*VLZ6(I)+PX7(I)*VLZ7(I)+PX8(I)*VLZ8(I)
        DZY(I) =PY1(I)*VLZ1(I)+PY2(I)*VLZ2(I)+PY3(I)*VLZ3(I)+PY4(I)*VLZ4(I)
     +         +PY5(I)*VLZ5(I)+PY6(I)*VLZ6(I)+PY7(I)*VLZ7(I)+PY8(I)*VLZ8(I)
       ENDDO
      END IF !(ISMSTR==11) THEN
C	   
       DO I=LFT,LLT
	    IF(OFFG(I)<=ONE) CYCLE
        DXX(I) =PX1(I)*VLX1(I)+PX2(I)*VLX2(I)+PX3(I)*VLX3(I)+PX4(I)*VLX4(I)
     .         +PX5(I)*VLX5(I)+PX6(I)*VLX6(I)+PX7(I)*VLX7(I)+PX8(I)*VLX8(I)
        DYY(I) =PY1(I)*VLY1(I)+PY2(I)*VLY2(I)+PY3(I)*VLY3(I)+PY4(I)*VLY4(I)
     .         +PY5(I)*VLY5(I)+PY6(I)*VLY6(I)+PY7(I)*VLY7(I)+PY8(I)*VLY8(I)
        DZZ(I) =PZ1(I)*VLZ1(I)+PZ2(I)*VLZ2(I)+PZ3(I)*VLZ3(I)+PZ4(I)*VLZ4(I)
     .         +PZ5(I)*VLZ5(I)+PZ6(I)*VLZ6(I)+PZ7(I)*VLZ7(I)+PZ8(I)*VLZ8(I)
       ENDDO
	   IF (ISEL_V>0) THEN
#include   "nofusion.inc"
       DO I=LFT,LLT
	    IF(OFFG(I)<=ONE) CYCLE
        DYY(I) =DYY(I)+BXY1(I)*VLX1(I)+BXY2(I)*VLX2(I)
     +                +BXY3(I)*VLX3(I)+BXY4(I)*VLX4(I)
     +                +BXY5(I)*VLX5(I)+BXY6(I)*VLX6(I)
     +                +BXY7(I)*VLX7(I)+BXY8(I)*VLX8(I)
        DZZ(I) =DZZ(I)+BXZ1(I)*VLX1(I)+BXZ2(I)*VLX2(I)
     +                +BXZ3(I)*VLX3(I)+BXZ4(I)*VLX4(I)
     +                +BXZ5(I)*VLX5(I)+BXZ6(I)*VLX6(I)
     +                +BXZ7(I)*VLX7(I)+BXZ8(I)*VLX8(I)
        DXX(I) =DXX(I)+BYX1(I)*VLY1(I)+BYX2(I)*VLY2(I)
     +                +BYX3(I)*VLY3(I)+BYX4(I)*VLY4(I)
     +                +BYX5(I)*VLY5(I)+BYX6(I)*VLY6(I)
     +                +BYX7(I)*VLY7(I)+BYX8(I)*VLY8(I)
        DZZ(I) =DZZ(I)+BYZ1(I)*VLY1(I)+BYZ2(I)*VLY2(I)
     +                +BYZ3(I)*VLY3(I)+BYZ4(I)*VLY4(I)
     +                +BYZ5(I)*VLY5(I)+BYZ6(I)*VLY6(I)
     +                +BYZ7(I)*VLY7(I)+BYZ8(I)*VLY8(I)
        DXX(I) =DXX(I)+BZX1(I)*VLZ1(I)+BZX2(I)*VLZ2(I)
     +                +BZX3(I)*VLZ3(I)+BZX4(I)*VLZ4(I)
     +                +BZX5(I)*VLZ5(I)+BZX6(I)*VLZ6(I)
     +                +BZX7(I)*VLZ7(I)+BZX8(I)*VLZ8(I)
        DYY(I) =DYY(I)+BZY1(I)*VLZ1(I)+BZY2(I)*VLZ2(I)
     +                +BZY3(I)*VLZ3(I)+BZY4(I)*VLZ4(I)
     +                +BZY5(I)*VLZ5(I)+BZY6(I)*VLZ6(I)
     +                +BZY7(I)*VLZ7(I)+BZY8(I)*VLZ8(I)
       ENDDO
	   END IF !(ISEL_V>0) THEN
      IF (ICP==1) THEN
        DO I=LFT,LLT
	     IF(OFFG(I)<=ONE) CYCLE
         AJ11=DXX(I)+ONE
         AJ22=DYY(I)+ONE
         AJ33=DZZ(I)+ONE
         JAC_59_68=AJ22*AJ33-DYZ(I)*DZY(I)
         JAC_67_49=DYZ(I)*DZX(I)-DYX(I)*AJ33
         JAC_48_57=DYX(I)*DZY(I)-AJ22*DZX(I)
C       
         DET(I)=AJ11*JAC_59_68+DXY(I)*JAC_67_49+DXZ(I)*JAC_48_57
C-----F'=FAC*F=MF'+[1]-->MF'=FAC*MF+(FAC-ONE)*[1]
         FAC=TWO_THIRD*NU(I)
         BASE = DET0(I)/MAX(EM20,DET(I))
         JFAC(I)=EXP(FAC*LOG(MAX(EM20,BASE)))
        ENDDO
       DO I=LFT,LLT
        IF(IDEGE(I)>10.OR.OFFG(I)<=ONE) CYCLE
        DXX(I) = JFAC(I)*DXX(I)+JFAC(I)-ONE
        DYY(I) = JFAC(I)*DYY(I)+JFAC(I)-ONE
        DZZ(I) = JFAC(I)*DZZ(I)+JFAC(I)-ONE
        DXY(I) = JFAC(I)*DXY(I)
        DYX(I) = JFAC(I)*DYX(I)
        DZX(I) = JFAC(I)*DZX(I)
        DXZ(I) = JFAC(I)*DXZ(I)
        DYZ(I) = JFAC(I)*DYZ(I)
        DZY(I) = JFAC(I)*DZY(I)
       ENDDO
      END IF !(ICP==1) THEN
C
      RETURN
      END
