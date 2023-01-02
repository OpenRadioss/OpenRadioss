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
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
Chd|====================================================================
Chd|  CDK6DEFO3                     source/elements/sh3n/coquedk6/cdk6defo3.F
Chd|-- called by -----------
Chd|        CDK6FORC3                     source/elements/sh3n/coquedk6/cdk6forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CDK6DEFO3(JFT,JLT,VLX,VLY,VLZ,VZ4,VZ5,VZ6,
     1                     PX2,PY2,PX3,PY3,PB1,PB2,PB3,VDEF)
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
#include      "scr14_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C  ds007  18/2/00   +1
      INTEGER JFT, JLT
C     REAL
      my_real
     .   PX2(*),PY2(*),PX3(*),PY3(*),PB1(MVSIZ,3,3),PB2(MVSIZ,3,3),PB3(MVSIZ,3,6),
     .   VLX(MVSIZ,2),VLY(MVSIZ,2),VLZ(MVSIZ,6),VZ4(MVSIZ,3),VZ5(MVSIZ,3),VZ6(MVSIZ,3),VDEF(MVSIZ,8)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
      my_real
     .   A5,A6,A4
C-----------------------------------------------
C
      DO I=JFT,JLT
       VDEF(I,1)=PX2(I)*VLX(I,1)+PX3(I)*VLX(I,2)
       VDEF(I,2)=PY2(I)*VLY(I,1)+PY3(I)*VLY(I,2)
       VDEF(I,3)=PX2(I)*VLY(I,1)+PX3(I)*VLY(I,2)
     .          +PY2(I)*VLX(I,1)+PY3(I)*VLX(I,2)
       VDEF(I,4)=0.
       VDEF(I,5)=0.
      ENDDO
C
      DO I=JFT,JLT
       A5 = PB3(I,1,4)*VZ5(I,1)+PB3(I,1,5)*VZ5(I,2)
     .     +PB3(I,1,6)*VZ5(I,3)
       A6 = PB3(I,2,4)*VZ6(I,1)+PB3(I,2,5)*VZ6(I,2)
     .     +PB3(I,2,6)*VZ6(I,3)
       A4 = PB3(I,3,4)*VZ4(I,1)+PB3(I,3,5)*VZ4(I,2)
     .     +PB3(I,3,6)*VZ4(I,3)
       VDEF(I,6)=PB1(I,1,1)*VLZ(I,1)+PB1(I,1,2)*VLZ(I,2)
     .          +PB1(I,1,3)*VLZ(I,3)+PB2(I,1,1)*VLZ(I,4)
     .          +PB2(I,1,2)*VLZ(I,5)+PB2(I,1,3)*VLZ(I,6)
     .          +PB3(I,1,1)*A5+PB3(I,1,2)*A6+PB3(I,1,3)*A4
       VDEF(I,7)=PB1(I,2,1)*VLZ(I,1)+PB1(I,2,2)*VLZ(I,2)
     .          +PB1(I,2,3)*VLZ(I,3)+PB2(I,2,1)*VLZ(I,4)
     .          +PB2(I,2,2)*VLZ(I,5)+PB2(I,2,3)*VLZ(I,6)
     .          +PB3(I,2,1)*A5+PB3(I,2,2)*A6+PB3(I,2,3)*A4
       VDEF(I,8)=PB1(I,3,1)*VLZ(I,1)+PB1(I,3,2)*VLZ(I,2)
     .          +PB1(I,3,3)*VLZ(I,3)+PB2(I,3,1)*VLZ(I,4)
     .          +PB2(I,3,2)*VLZ(I,5)+PB2(I,3,3)*VLZ(I,6)
     .          +PB3(I,3,1)*A5+PB3(I,3,2)*A6+PB3(I,3,3)*A4
      ENDDO
C
      RETURN
      END
