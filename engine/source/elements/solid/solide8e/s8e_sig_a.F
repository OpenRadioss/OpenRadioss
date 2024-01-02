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
Chd|  S8E_SIG_A                     source/elements/solid/solide8e/s8e_sig_a.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8E_SIG_A(
     1   C1,      G,       OFF,     BXY1,
     2   BXY2,    BXY3,    BXY4,    BXY5,
     3   BXY6,    BXY7,    BXY8,    BYX1,
     4   BYX2,    BYX3,    BYX4,    BYX5,
     5   BYX6,    BYX7,    BYX8,    BXZ1,
     6   BXZ2,    BXZ3,    BXZ4,    BXZ5,
     7   BXZ6,    BXZ7,    BXZ8,    BZX1,
     8   BZX2,    BZX3,    BZX4,    BZX5,
     9   BZX6,    BZX7,    BZX8,    BYZ1,
     A   BYZ2,    BYZ3,    BYZ4,    BYZ5,
     B   BYZ6,    BYZ7,    BYZ8,    BZY1,
     C   BZY2,    BZY3,    BZY4,    BZY5,
     D   BZY6,    BZY7,    BZY8,    VX1,
     E   VX2,     VX3,     VX4,     VX5,
     F   VX6,     VX7,     VX8,     VY1,
     G   VY2,     VY3,     VY4,     VY5,
     H   VY6,     VY7,     VY8,     VZ1,
     I   VZ2,     VZ3,     VZ4,     VZ5,
     J   VZ6,     VZ7,     VZ8,     PIJ,
     K   SIG_A,   EINT,    VOL,     D1,
     L   D2,      D3,      SIG,     S1,
     M   S2,      S3,      NEL,     OFFG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
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
     .   PIJ(NEL,*),C1,G,SIG_A(MVSIZ,3),OFF(*),EINT(*),
     .   VOL(*),D1(*),D2(*),D3(*),SIG(NEL,6),S1(*),S2(*),S3(*),OFFG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      my_real
     .   DC1,DG2,DXX(MVSIZ),DYY(MVSIZ),DZZ(MVSIZ),EKK,DAV,
     .   DT05,E1,E2,E3,DE,P2
C PIJ(NEL,25-27) are used call stress with v-terms
C-----------------------------------------------
       IF (TT==ZERO) PIJ(1:NEL,25:27)=ZERO
C----1,compute Dii with v-terms, compute incremental stress
       DO I=1,NEL
               IF (OFFG(I) >ONE) CYCLE
           DYY(I) =BXY1(I)*VX1(I)+BXY2(I)*VX2(I)
     +              +BXY3(I)*VX3(I)+BXY4(I)*VX4(I)
     +              +BXY5(I)*VX5(I)+BXY6(I)*VX6(I)
     +              +BXY7(I)*VX7(I)+BXY8(I)*VX8(I)
     +              +BZY1(I)*VZ1(I)+BZY2(I)*VZ2(I)
     +              +BZY3(I)*VZ3(I)+BZY4(I)*VZ4(I)
     +              +BZY5(I)*VZ5(I)+BZY6(I)*VZ6(I)
     +              +BZY7(I)*VZ7(I)+BZY8(I)*VZ8(I)
           DZZ(I) =BXZ1(I)*VX1(I)+BXZ2(I)*VX2(I)
     +              +BXZ3(I)*VX3(I)+BXZ4(I)*VX4(I)
     +              +BXZ5(I)*VX5(I)+BXZ6(I)*VX6(I)
     +              +BXZ7(I)*VX7(I)+BXZ8(I)*VX8(I)
     +              +BYZ1(I)*VY1(I)+BYZ2(I)*VY2(I)
     +              +BYZ3(I)*VY3(I)+BYZ4(I)*VY4(I)
     +              +BYZ5(I)*VY5(I)+BYZ6(I)*VY6(I)
     +              +BYZ7(I)*VY7(I)+BYZ8(I)*VY8(I)
           DXX(I) =BYX1(I)*VY1(I)+BYX2(I)*VY2(I)
     +              +BYX3(I)*VY3(I)+BYX4(I)*VY4(I)
     +              +BYX5(I)*VY5(I)+BYX6(I)*VY6(I)
     +              +BYX7(I)*VY7(I)+BYX8(I)*VY8(I)
     +              +BZX1(I)*VZ1(I)+BZX2(I)*VZ2(I)
     +              +BZX3(I)*VZ3(I)+BZX4(I)*VZ4(I)
     +              +BZX5(I)*VZ5(I)+BZX6(I)*VZ6(I)
     +              +BZX7(I)*VZ7(I)+BZX8(I)*VZ8(I)
       ENDDO
       DC1=C1*DT1
       DG2=TWO*G*DT1
       SIG_A(1:NEL,1:3)=PIJ(1:NEL,25:27)
       DO I=1,NEL
               IF (OFFG(I) >ONE) CYCLE
         EKK=(DXX(I)+DYY(I)+DZZ(I))*DC1
c       IF (OFFG(I) >ONE) EKK=ZERO
         DAV=-THIRD*(DXX(I)+DYY(I)+DZZ(I))
         PIJ(I,25)=PIJ(I,25)*OFF(I)+EKK+DG2*(DXX(I)+DAV)
         PIJ(I,26)=PIJ(I,26)*OFF(I)+EKK+DG2*(DYY(I)+DAV)
         PIJ(I,27)=PIJ(I,27)*OFF(I)+EKK+DG2*(DZZ(I)+DAV)
       END DO
       DT05 =HALF*DT1
      DO I=1,NEL
              IF (OFFG(I) >ONE) CYCLE
       E1=(DXX(I)+D1(I))*(SIG_A(I,1)+PIJ(I,25))+DXX(I)*(S1(I)+SIG(I,1))
       E2=(DYY(I)+D2(I))*(SIG_A(I,2)+PIJ(I,26))+DYY(I)*(S2(I)+SIG(I,2))
       E3=(DZZ(I)+D3(I))*(SIG_A(I,3)+PIJ(I,27))+DZZ(I)*(S3(I)+SIG(I,3))
       DE= (E1+E2+E3)*DT05*OFF(I) 
       EINT(I)=EINT(I)+DE 
       SIG_A(I,1:3)=PIJ(I,25:27)
      ENDDO
C-----------
      RETURN
      END
