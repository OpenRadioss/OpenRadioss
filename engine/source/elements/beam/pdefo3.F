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
Chd|  PDEFO3                        source/elements/beam/pdefo3.F 
Chd|-- called by -----------
Chd|        PFORC3                        source/elements/beam/pforc3.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PDEFO3(V     ,EXX    ,EXY    ,EXZ    ,AL  ,
     .                  NC1   ,NC2    ,NC3    ,E1X    ,E1Y ,
     .                  E1Z   ,E2X    ,E2Y    ,E2Z    ,E3X ,
     .                  E3Y   ,E3Z    ,VX1G   ,VX2G   ,VY1G,
     .                  VY2G  ,VZ1G   ,VZ2G   )
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ)
      my_real
     .   V(3,*),EXX(MVSIZ),EXY(MVSIZ),EXZ(MVSIZ),AL(MVSIZ),
     .   E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),E2X(MVSIZ),E2Y(MVSIZ),
     .   E2Z(MVSIZ),E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ),VX1G(MVSIZ),
     .   VX2G(MVSIZ),VY1G(MVSIZ),VY2G(MVSIZ),VZ1G(MVSIZ),VZ2G(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   VX1(MVSIZ),VX2(MVSIZ),VY1(MVSIZ),VY2(MVSIZ),VZ1(MVSIZ),VZ2(MVSIZ)
C-----------------------------------------------
      DO I=LFT,LLT
        VX1G(I)=V(1,NC1(I))
        VY1G(I)=V(2,NC1(I))
        VZ1G(I)=V(3,NC1(I))
        VX2G(I)=V(1,NC2(I))
        VY2G(I)=V(2,NC2(I))
        VZ2G(I)=V(3,NC2(I))
      ENDDO
C
      DO I=LFT,LLT
        VX1(I)=E1X(I)*VX1G(I)+E1Y(I)*VY1G(I)+E1Z(I)*VZ1G(I)
        VY1(I)=E2X(I)*VX1G(I)+E2Y(I)*VY1G(I)+E2Z(I)*VZ1G(I)
        VZ1(I)=E3X(I)*VX1G(I)+E3Y(I)*VY1G(I)+E3Z(I)*VZ1G(I)
        VX2(I)=E1X(I)*VX2G(I)+E1Y(I)*VY2G(I)+E1Z(I)*VZ2G(I)
        VY2(I)=E2X(I)*VX2G(I)+E2Y(I)*VY2G(I)+E2Z(I)*VZ2G(I)
        VZ2(I)=E3X(I)*VX2G(I)+E3Y(I)*VY2G(I)+E3Z(I)*VZ2G(I)
      ENDDO
C
      DO I=LFT,LLT
        EXX(I)=(VX2(I)-VX1(I))/AL(I)
        EXY(I)=(VY2(I)-VY1(I))/AL(I)
        EXZ(I)=(VZ2(I)-VZ1(I))/AL(I)
      ENDDO
C-----------------------------------------------
      RETURN
      END
