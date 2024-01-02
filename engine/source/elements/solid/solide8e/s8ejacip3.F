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
Chd|  S8EJACIP3                     source/elements/solid/solide8e/s8ejacip3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EJACIP3(
     1   HX,      HY,      HZ,      CJ1,
     2   CJ2,     CJ3,     CJ4,     CJ5,
     3   CJ6,     CJ7,     CJ8,     CJ9,
     4   AJ1,     AJ2,     AJ3,     AJ4,
     5   AJ5,     AJ6,     AJ7,     AJ8,
     6   AJ9,     NEL)
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
C     REAL
      my_real
     .   HX(MVSIZ,4),  HY(MVSIZ,4), HZ(MVSIZ,4),   
     .   CJ1(*),CJ2(*),CJ3(*),
     .   CJ4(*),CJ5(*),CJ6(*),
     .   CJ7(*),CJ8(*),CJ9(*),
     .   AJ1(MVSIZ,8),AJ2(MVSIZ,8),AJ3(MVSIZ,8),
     .   AJ4(MVSIZ,8),AJ5(MVSIZ,8),AJ6(MVSIZ,8),
     .   AJ7(MVSIZ,8),AJ8(MVSIZ,8),AJ9(MVSIZ,8)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,IP
C     REAL
C                                                                     12
      my_real
     .   HX1PG(MVSIZ), HX2PG(MVSIZ), HX3PG(MVSIZ),HX4PG2(MVSIZ),
     .   HY1PG(MVSIZ), HY2PG(MVSIZ), HY3PG(MVSIZ),HY4PG2(MVSIZ),
     .   HZ1PG(MVSIZ), HZ2PG(MVSIZ), HZ3PG(MVSIZ),HZ4PG2(MVSIZ),
     .   PG2,PG
       PARAMETER (PG=.577350269189625D0)
C-----------------------------------------------
      PG2=PG*PG
C
      DO I=1,NEL
       HX1PG(I)=HX(I,1)*PG
       HX2PG(I)=HX(I,2)*PG
       HX3PG(I)=HX(I,3)*PG
       HX4PG2(I)=HX(I,4)*PG2
       HY1PG(I)=HY(I,1)*PG
       HY2PG(I)=HY(I,2)*PG
       HY3PG(I)=HY(I,3)*PG
       HY4PG2(I)=HY(I,4)*PG2
       HZ1PG(I)=HZ(I,1)*PG
       HZ2PG(I)=HZ(I,2)*PG
       HZ3PG(I)=HZ(I,3)*PG
       HZ4PG2(I)=HZ(I,4)*PG2
      ENDDO
      IP=1
C - - -    
      DO I=1,NEL
       AJ1(I,IP)=CJ1(I)-HX3PG(I)-HX2PG(I)+HX4PG2(I)
       AJ2(I,IP)=CJ2(I)-HY3PG(I)-HY2PG(I)+HY4PG2(I)
       AJ3(I,IP)=CJ3(I)-HZ3PG(I)-HZ2PG(I)+HZ4PG2(I)
C
       AJ4(I,IP)=CJ4(I)-HX1PG(I)-HX3PG(I)+HX4PG2(I)
       AJ5(I,IP)=CJ5(I)-HY1PG(I)-HY3PG(I)+HY4PG2(I)
       AJ6(I,IP)=CJ6(I)-HZ1PG(I)-HZ3PG(I)+HZ4PG2(I)
C
       AJ7(I,IP)=CJ7(I)-HX2PG(I)-HX1PG(I)+HX4PG2(I)
       AJ8(I,IP)=CJ8(I)-HY2PG(I)-HY1PG(I)+HY4PG2(I)
       AJ9(I,IP)=CJ9(I)-HZ2PG(I)-HZ1PG(I)+HZ4PG2(I)
      ENDDO
      IP=2    
C + - -    
      DO I=1,NEL
       AJ1(I,IP)=CJ1(I)-HX3PG(I)-HX2PG(I)+HX4PG2(I)
       AJ2(I,IP)=CJ2(I)-HY3PG(I)-HY2PG(I)+HY4PG2(I)
       AJ3(I,IP)=CJ3(I)-HZ3PG(I)-HZ2PG(I)+HZ4PG2(I)
C
       AJ4(I,IP)=CJ4(I)-HX1PG(I)+HX3PG(I)-HX4PG2(I)
       AJ5(I,IP)=CJ5(I)-HY1PG(I)+HY3PG(I)-HY4PG2(I)
       AJ6(I,IP)=CJ6(I)-HZ1PG(I)+HZ3PG(I)-HZ4PG2(I)
C
       AJ7(I,IP)=CJ7(I)+HX2PG(I)-HX1PG(I)-HX4PG2(I)
       AJ8(I,IP)=CJ8(I)+HY2PG(I)-HY1PG(I)-HY4PG2(I)
       AJ9(I,IP)=CJ9(I)+HZ2PG(I)-HZ1PG(I)-HZ4PG2(I)
      ENDDO
      IP=3    
C - + -    
      DO I=1,NEL
       AJ1(I,IP)=CJ1(I)+HX3PG(I)-HX2PG(I)-HX4PG2(I)
       AJ2(I,IP)=CJ2(I)+HY3PG(I)-HY2PG(I)-HY4PG2(I)
       AJ3(I,IP)=CJ3(I)+HZ3PG(I)-HZ2PG(I)-HZ4PG2(I)
C
       AJ4(I,IP)=CJ4(I)-HX1PG(I)-HX3PG(I)+HX4PG2(I)
       AJ5(I,IP)=CJ5(I)-HY1PG(I)-HY3PG(I)+HY4PG2(I)
       AJ6(I,IP)=CJ6(I)-HZ1PG(I)-HZ3PG(I)+HZ4PG2(I)
C
       AJ7(I,IP)=CJ7(I)-HX2PG(I)+HX1PG(I)-HX4PG2(I)
       AJ8(I,IP)=CJ8(I)-HY2PG(I)+HY1PG(I)-HY4PG2(I)
       AJ9(I,IP)=CJ9(I)-HZ2PG(I)+HZ1PG(I)-HZ4PG2(I)
      ENDDO
      IP=4    
C + + -    
      DO I=1,NEL
       AJ1(I,IP)=CJ1(I)+HX3PG(I)-HX2PG(I)-HX4PG2(I)
       AJ2(I,IP)=CJ2(I)+HY3PG(I)-HY2PG(I)-HY4PG2(I)
       AJ3(I,IP)=CJ3(I)+HZ3PG(I)-HZ2PG(I)-HZ4PG2(I)
C
       AJ4(I,IP)=CJ4(I)-HX1PG(I)+HX3PG(I)-HX4PG2(I)
       AJ5(I,IP)=CJ5(I)-HY1PG(I)+HY3PG(I)-HY4PG2(I)
       AJ6(I,IP)=CJ6(I)-HZ1PG(I)+HZ3PG(I)-HZ4PG2(I)
C
       AJ7(I,IP)=CJ7(I)+HX2PG(I)+HX1PG(I)+HX4PG2(I)
       AJ8(I,IP)=CJ8(I)+HY2PG(I)+HY1PG(I)+HY4PG2(I)
       AJ9(I,IP)=CJ9(I)+HZ2PG(I)+HZ1PG(I)+HZ4PG2(I)
      ENDDO
      IP=5    
C - - +    
      DO I=1,NEL
       AJ1(I,IP)=CJ1(I)-HX3PG(I)+HX2PG(I)-HX4PG2(I)
       AJ2(I,IP)=CJ2(I)-HY3PG(I)+HY2PG(I)-HY4PG2(I)
       AJ3(I,IP)=CJ3(I)-HZ3PG(I)+HZ2PG(I)-HZ4PG2(I)
C
       AJ4(I,IP)=CJ4(I)+HX1PG(I)-HX3PG(I)-HX4PG2(I)
       AJ5(I,IP)=CJ5(I)+HY1PG(I)-HY3PG(I)-HY4PG2(I)
       AJ6(I,IP)=CJ6(I)+HZ1PG(I)-HZ3PG(I)-HZ4PG2(I)
C
       AJ7(I,IP)=CJ7(I)-HX2PG(I)-HX1PG(I)+HX4PG2(I)
       AJ8(I,IP)=CJ8(I)-HY2PG(I)-HY1PG(I)+HY4PG2(I)
       AJ9(I,IP)=CJ9(I)-HZ2PG(I)-HZ1PG(I)+HZ4PG2(I)
      ENDDO
      IP=6    
C + - +    
      DO I=1,NEL
       AJ1(I,IP)=CJ1(I)-HX3PG(I)+HX2PG(I)-HX4PG2(I)
       AJ2(I,IP)=CJ2(I)-HY3PG(I)+HY2PG(I)-HY4PG2(I)
       AJ3(I,IP)=CJ3(I)-HZ3PG(I)+HZ2PG(I)-HZ4PG2(I)
C
       AJ4(I,IP)=CJ4(I)+HX1PG(I)+HX3PG(I)+HX4PG2(I)
       AJ5(I,IP)=CJ5(I)+HY1PG(I)+HY3PG(I)+HY4PG2(I)
       AJ6(I,IP)=CJ6(I)+HZ1PG(I)+HZ3PG(I)+HZ4PG2(I)
C
       AJ7(I,IP)=CJ7(I)+HX2PG(I)-HX1PG(I)-HX4PG2(I)
       AJ8(I,IP)=CJ8(I)+HY2PG(I)-HY1PG(I)-HY4PG2(I)
       AJ9(I,IP)=CJ9(I)+HZ2PG(I)-HZ1PG(I)-HZ4PG2(I)
      ENDDO
      IP=7    
C - + +    
      DO I=1,NEL
       AJ1(I,IP)=CJ1(I)+HX3PG(I)+HX2PG(I)+HX4PG2(I)
       AJ2(I,IP)=CJ2(I)+HY3PG(I)+HY2PG(I)+HY4PG2(I)
       AJ3(I,IP)=CJ3(I)+HZ3PG(I)+HZ2PG(I)+HZ4PG2(I)
C
       AJ4(I,IP)=CJ4(I)+HX1PG(I)-HX3PG(I)-HX4PG2(I)
       AJ5(I,IP)=CJ5(I)+HY1PG(I)-HY3PG(I)-HY4PG2(I)
       AJ6(I,IP)=CJ6(I)+HZ1PG(I)-HZ3PG(I)-HZ4PG2(I)
C
       AJ7(I,IP)=CJ7(I)-HX2PG(I)+HX1PG(I)-HX4PG2(I)
       AJ8(I,IP)=CJ8(I)-HY2PG(I)+HY1PG(I)-HY4PG2(I)
       AJ9(I,IP)=CJ9(I)-HZ2PG(I)+HZ1PG(I)-HZ4PG2(I)
      ENDDO
      IP=8    
C + + +    
      DO I=1,NEL
       AJ1(I,IP)=CJ1(I)+HX3PG(I)+HX2PG(I)+HX4PG2(I)
       AJ2(I,IP)=CJ2(I)+HY3PG(I)+HY2PG(I)+HY4PG2(I)
       AJ3(I,IP)=CJ3(I)+HZ3PG(I)+HZ2PG(I)+HZ4PG2(I)
C
       AJ4(I,IP)=CJ4(I)+HX1PG(I)+HX3PG(I)+HX4PG2(I)
       AJ5(I,IP)=CJ5(I)+HY1PG(I)+HY3PG(I)+HY4PG2(I)
       AJ6(I,IP)=CJ6(I)+HZ1PG(I)+HZ3PG(I)+HZ4PG2(I)
C
       AJ7(I,IP)=CJ7(I)+HX2PG(I)+HX1PG(I)+HX4PG2(I)
       AJ8(I,IP)=CJ8(I)+HY2PG(I)+HY1PG(I)+HY4PG2(I)
       AJ9(I,IP)=CJ9(I)+HZ2PG(I)+HZ1PG(I)+HZ4PG2(I)
      ENDDO
C
      RETURN
      END
