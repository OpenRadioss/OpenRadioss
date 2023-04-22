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
Chd|  Q4VIS2                        source/elements/solid_2d/quad4/q4vis2.F
Chd|-- called by -----------
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE Q4VIS2(
     1   PM,      OFF,     RHO,     Y1,
     2   Y2,      Y3,      Y4,      Z1,
     3   Z2,      Z3,      Z4,      VY1,
     4   VY2,     VY3,     VY4,     VZ1,
     5   VZ2,     VZ3,     VZ4,     PY1,
     6   PY2,     PZ1,     PZ2,     T11,
     7   T12,     T13,     T14,     T21,
     8   T22,     T23,     T24,     AREA,
     9   CXX,     NEL)
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      my_real
     .   PM(NPROPM,*),OFF(*), RHO(*),
     .   Y1(*),Y2(*),Y3(*),Y4(*),Z1(*),Z2(*),Z3(*),Z4(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VZ1(*), VZ2(*), VZ3(*),
     .   PY1(*), PY2(*), PZ1(*), PZ2(*),
     .   T11(*), T12(*), T13(*), T14(*), T21(*), T22(*), T23(*), T24(*),
     .   VZ4(*), AREA(*), CXX(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
      my_real
     .   CAQ(MVSIZ),FCL(MVSIZ) , 
     .   G11(MVSIZ) , G21(MVSIZ) , G31(MVSIZ) , G41(MVSIZ),
     .   HGY(MVSIZ), HGZ(MVSIZ), 
     .   HY,HZ,FAC,PX1H1,PX2H1, ARE
C-----------------------------------------------
C 
c      will use CAQ if necessary CAQ=0.001 (Quad BELYTSCHKO CAQ=0.1)
C
      DO I=1,NEL
       FAC = EM03*OFF(I)
       ARE = MAX(AREA(I),EM20)
       FCL(I)=HALF*FAC*RHO(I)*SQRT(ARE)*CXX(I)
      END DO 
C
C
C HOURGLASS BELYTSCHKO
C
        DO I=1,NEL
         HY=Y1(I)-Y2(I)+Y3(I)-Y4(I)
         HZ=Z1(I)-Z2(I)+Z3(I)-Z4(I)
         FAC=ONE/MAX(EM20,AREA(I))
         PX1H1=FAC*(PY1(I)*HY+PZ1(I)*HZ)
         PX2H1=FAC*(PY2(I)*HY+PZ2(I)*HZ)
         G11(I)= ONE -PX1H1
         G21(I)=-ONE -PX2H1
         G31(I)= ONE +PX1H1
         G41(I)=-ONE +PX2H1 
        END DO 
        DO I=1,NEL
         HGY(I)=FCL(I)*(
     &    G11(I)*VY1(I)+G21(I)*VY2(I)+G31(I)*VY3(I)+G41(I)*VY4(I))
         HGZ(I)=FCL(I)*(
     &    G11(I)*VZ1(I)+G21(I)*VZ2(I)+G31(I)*VZ3(I)+G41(I)*VZ4(I))
        END DO 
        DO I=1,NEL
         T11(I) =T11(I) - G11(I)*HGY(I)
         T12(I) =T12(I) - G21(I)*HGY(I)
         T13(I) =T13(I) - G31(I)*HGY(I)
         T14(I) =T14(I) - G41(I)*HGY(I)
         T21(I) =T21(I) - G11(I)*HGZ(I)
         T22(I) =T22(I) - G21(I)*HGZ(I)
         T23(I) =T23(I) - G31(I)*HGZ(I)
         T24(I) =T24(I) - G41(I)*HGZ(I)
        END DO 
C
      RETURN
      END
