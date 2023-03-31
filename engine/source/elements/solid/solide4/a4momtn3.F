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
Chd|  A4MOMTN3                      source/elements/solid/solide4/a4momtn3.F
Chd|-- called by -----------
Chd|        A4MOMT3                       source/elements/solid/solide4/a4momt3.F
Chd|-- calls ---------------
Chd|        UPWIND4                       source/elements/solid/solide4/upwind4.F
Chd|====================================================================
      SUBROUTINE A4MOMTN3(
     1   RHO,     VOL,     VIS,     R,
     2   S,       T,       DELTAX,  VMX,
     3   VMY,     VMZ,     DXX,     DXY,
     4   DXZ,     DYX,     DYY,     DYZ,
     5   DZX,     DZY,     DZZ,     PX,
     6   PY,      PZ,      VDX,     VDY,
     7   VDZ,     FX,      FY,      FZ,
     8   NEL)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      my_real
     .   RHO(*),VOL(*),VIS(*),VDX(*),VDY(*),VDZ(*),VMX(*),VMY(*),VMZ(*),
     .   R(MVSIZ,3),S(MVSIZ,3),T(MVSIZ,3),DELTAX(*),PX(*),PY(*),PZ(*),
     .   FX(*),FY(*),FZ(*),
     .   DXX(*),DXY(*),DXZ(*),
     .   DYX(*),DYY(*),DYZ(*),
     .   DZX(*),DZY(*),DZZ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I
      my_real :: A(MVSIZ),GAM(MVSIZ) 
C-----------------------------------------------
C   S o u r c e   C o d e
C-----------------------------------------------
      CALL UPWIND4(
     1   RHO,     VIS,     VDX,     VDY,
     2   VDZ,     R,       S,       T,
     3   GAM,     NEL)
      DO I=1,NEL
        A(I)=FOURTH*RHO(I)*VOL(I)*(ONE+FOUR*GAM(I)*( PX(I)*VDX(I)+PY(I)*VDY(I)+PZ(I)*VDZ(I)))
        FX(I)=FX(I)-A(I)*(VDX(I)*DXX(I)+VDY(I)*DXY(I)+VDZ(I)*DXZ(I))
        FY(I)=FY(I)-A(I)*(VDX(I)*DYX(I)+VDY(I)*DYY(I)+VDZ(I)*DYZ(I))
        FZ(I)=FZ(I)-A(I)*(VDX(I)*DZX(I)+VDY(I)*DZY(I)+VDZ(I)*DZZ(I))
      ENDDO
      RETURN
      END
