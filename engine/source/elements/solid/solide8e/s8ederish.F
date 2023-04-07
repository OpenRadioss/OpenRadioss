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
Chd|  S8EDERISH                     source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EDERISH(
     1   KSI,     ETA,     ZETA,    PXC1,
     2   PXC2,    PXC3,    PXC4,    PYC1,
     3   PYC2,    PYC3,    PYC4,    PZC1,
     4   PZC2,    PZC3,    PZC4,    PX1H1,
     5   PX1H2,   PX1H3,   PX1H4,   PX2H1,
     6   PX2H2,   PX2H3,   PX2H4,   PX3H1,
     7   PX3H2,   PX3H3,   PX3H4,   PX4H1,
     8   PX4H2,   PX4H3,   PX4H4,   AJI1,
     9   AJI2,    AJI3,    AJI4,    AJI5,
     A   AJI6,    AJI7,    AJI8,    AJI9,
     B   PXY1,    PXY2,    PXY3,    PXY4,
     C   PXY5,    PXY6,    PXY7,    PXY8,
     D   PYX1,    PYX2,    PYX3,    PYX4,
     E   PYX5,    PYX6,    PYX7,    PYX8,
     F   PXZ1,    PXZ2,    PXZ3,    PXZ4,
     G   PXZ5,    PXZ6,    PXZ7,    PXZ8,
     H   PZX1,    PZX2,    PZX3,    PZX4,
     I   PZX5,    PZX6,    PZX7,    PZX8,
     J   PYZ1,    PYZ2,    PYZ3,    PYZ4,
     K   PYZ5,    PYZ6,    PYZ7,    PYZ8,
     L   PZY1,    PZY2,    PZY3,    PZY4,
     M   PZY5,    PZY6,    PZY7,    PZY8,
     N   NEL)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
C     REAL
      my_real
     .   KSI,ETA,ZETA,
     .   PXC1(*), PXC2(*), PXC3(*), PXC4(*),  
     .   PYC1(*), PYC2(*), PYC3(*), PYC4(*),  
     .   PZC1(*), PZC2(*), PZC3(*), PZC4(*),  
     .   PX1H1(*), PX1H2(*), PX1H3(*),PX1H4(*),  
     .   PX2H1(*), PX2H2(*), PX2H3(*),PX2H4(*),  
     .   PX3H1(*), PX3H2(*), PX3H3(*),PX3H4(*),  
     .   PX4H1(*), PX4H2(*), PX4H3(*),PX4H4(*),  
     .   AJI1(MVSIZ), AJI2(MVSIZ), AJI3(MVSIZ),
     .   AJI4(MVSIZ), AJI5(MVSIZ), AJI6(MVSIZ),
     .   AJI7(MVSIZ), AJI8(MVSIZ), AJI9(MVSIZ),
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
     .   PZY5(MVSIZ),PZY6(MVSIZ),PZY7(MVSIZ),PZY8(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,ICOR,ep
C     REAL
C                                                                     12
      my_real
     .   D1X(MVSIZ) , D2X(MVSIZ) , D3X(MVSIZ) , D4X(MVSIZ) ,
     .   D1Y(MVSIZ) , D2Y(MVSIZ) , D3Y(MVSIZ) , D4Y(MVSIZ) ,
     .   D1Z(MVSIZ) , D2Z(MVSIZ) , D3Z(MVSIZ) , D4Z(MVSIZ) ,
     .   XG1(MVSIZ), XG2(MVSIZ), XG3(MVSIZ), XG4(MVSIZ),  
     .   YG1(MVSIZ), YG2(MVSIZ), YG3(MVSIZ), YG4(MVSIZ),   
     .   ZG1(MVSIZ), ZG2(MVSIZ), ZG3(MVSIZ), ZG4(MVSIZ),   
     .   F1,F2,F3,XS,XAS,YS,YAS,ZS,ZAS,CS,CAS ,BXHI,BYHI,BZHI
C-----------------------------------------------
       F1 = ETA*ZETA
       F2 = KSI*ZETA
       F3 = KSI*ETA
      DO I=1,NEL
       D1X(I)=AJI3(I)*ETA +AJI2(I)*ZETA
       D2X(I)=AJI1(I)*ZETA +AJI3(I)*KSI
       D3X(I)=AJI2(I)*KSI +AJI1(I)*ETA
       D4X(I)=AJI1(I)*F1+AJI2(I)*F2+AJI3(I)*F3
C
       D1Y(I)=AJI6(I)*ETA +AJI5(I)*ZETA
       D2Y(I)=AJI4(I)*ZETA +AJI6(I)*KSI
       D3Y(I)=AJI5(I)*KSI +AJI4(I)*ETA
       D4Y(I)=AJI4(I)*F1+AJI5(I)*F2+AJI6(I)*F3
C
       D1Z(I)=AJI9(I)*ETA +AJI8(I)*ZETA
       D2Z(I)=AJI7(I)*ZETA +AJI9(I)*KSI
       D3Z(I)=AJI8(I)*KSI +AJI7(I)*ETA
       D4Z(I)=AJI7(I)*F1+AJI8(I)*F2+AJI9(I)*F3
      ENDDO
C-------Node 1,7-----
      DO I=1,NEL
       XG1(I) = PX1H1(I)*D1X(I)
       XG2(I) = PX1H2(I)*D2X(I)
       XG3(I) = PX1H3(I)*D3X(I)
       XG4(I) = PX1H4(I)*D4X(I)
       YG1(I) = PX1H1(I)*D1Y(I)
       YG2(I) = PX1H2(I)*D2Y(I)
       YG3(I) = PX1H3(I)*D3Y(I)
       YG4(I) = PX1H4(I)*D4Y(I)
       ZG1(I) = PX1H1(I)*D1Z(I)
       ZG2(I) = PX1H2(I)*D2Z(I)
       ZG3(I) = PX1H3(I)*D3Z(I)
       ZG4(I) = PX1H4(I)*D4Z(I)
      ENDDO
C
      DO I=1,NEL
C
       YS =D1Y(I)+D2Y(I)
       YAS =YG1(I)+YG2(I)-PYC1(I)
       PXY1(I)=YS-YAS
       PXY7(I)=YS+YAS
       XS =D1X(I)+D2X(I)
       XAS =XG1(I)+XG2(I)-PXC1(I)
       PYX1(I)=XS-XAS
       PYX7(I)=XS+XAS
       ZS =D1Z(I)+D3Z(I)
       ZAS =ZG1(I)+ZG3(I)-PZC1(I)
       PXZ1(I)=ZS-ZAS
       PXZ7(I)=ZS+ZAS
       XS =D1X(I)+D3X(I)
       XAS =XG1(I)+XG3(I)-PXC1(I)
       PZX1(I)=XS-XAS
       PZX7(I)=XS+XAS
       ZS =D2Z(I)+D3Z(I)
       ZAS =ZG2(I)+ZG3(I)-PZC1(I)
       PYZ1(I)=ZS-ZAS
       PYZ7(I)=ZS+ZAS
       YS =D2Y(I)+D3Y(I)
       YAS =YG2(I)+YG3(I)-PYC1(I)
       PZY1(I)=YS-YAS
       PZY7(I)=YS+YAS
      ENDDO
C-------Node 2,8-----
      DO I=1,NEL
       XG1(I) = PX2H1(I)*D1X(I)
       XG2(I) = PX2H2(I)*D2X(I)
       XG3(I) = PX2H3(I)*D3X(I)
       XG4(I) = PX2H4(I)*D4X(I)
       YG1(I) = PX2H1(I)*D1Y(I)
       YG2(I) = PX2H2(I)*D2Y(I)
       YG3(I) = PX2H3(I)*D3Y(I)
       YG4(I) = PX2H4(I)*D4Y(I)
       ZG1(I) = PX2H1(I)*D1Z(I)
       ZG2(I) = PX2H2(I)*D2Z(I)
       ZG3(I) = PX2H3(I)*D3Z(I)
       ZG4(I) = PX2H4(I)*D4Z(I)
      ENDDO
C
      DO I=1,NEL
C
       YS =D1Y(I)-D2Y(I)
       YAS =YG1(I)+YG2(I)-PYC2(I)
       PXY2(I)=YS-YAS
       PXY8(I)=YS+YAS
       XS =D1X(I)-D2X(I)
       XAS =XG1(I)+XG2(I)-PXC2(I)
       PYX2(I)=XS-XAS
       PYX8(I)=XS+XAS
       ZS =D1Z(I)-D3Z(I)
       ZAS =ZG1(I)+ZG3(I)-PZC2(I)
       PXZ2(I)=ZS-ZAS
       PXZ8(I)=ZS+ZAS
       XS =D1X(I)-D3X(I)
       XAS =XG1(I)+XG3(I)-PXC2(I)
       PZX2(I)=XS-XAS
       PZX8(I)=XS+XAS
       ZS =-D2Z(I)-D3Z(I)
       ZAS =ZG2(I)+ZG3(I)-PZC2(I)
       PYZ2(I)=ZS-ZAS
       PYZ8(I)=ZS+ZAS
       YS =-D2Y(I)-D3Y(I)
       YAS =YG2(I)+YG3(I)-PYC2(I)
       PZY2(I)=YS-YAS
       PZY8(I)=YS+YAS
      ENDDO
C-------Node 3,5-----
      DO I=1,NEL
       XG1(I) = PX3H1(I)*D1X(I)
       XG2(I) = PX3H2(I)*D2X(I)
       XG3(I) = PX3H3(I)*D3X(I)
       XG4(I) = PX3H4(I)*D4X(I)
       YG1(I) = PX3H1(I)*D1Y(I)
       YG2(I) = PX3H2(I)*D2Y(I)
       YG3(I) = PX3H3(I)*D3Y(I)
       YG4(I) = PX3H4(I)*D4Y(I)
       ZG1(I) = PX3H1(I)*D1Z(I)
       ZG2(I) = PX3H2(I)*D2Z(I)
       ZG3(I) = PX3H3(I)*D3Z(I)
       ZG4(I) = PX3H4(I)*D4Z(I)
      ENDDO
C
      DO I=1,NEL
C
       YS =-D1Y(I)-D2Y(I)
       YAS =YG1(I)+YG2(I)-PYC3(I)
       PXY3(I)=YS-YAS
       PXY5(I)=YS+YAS
       XS =-D1X(I)-D2X(I)
       XAS =XG1(I)+XG2(I)-PXC3(I)
       PYX3(I)=XS-XAS
       PYX5(I)=XS+XAS
       ZS =-D1Z(I)+D3Z(I)
       ZAS =ZG1(I)+ZG3(I)-PZC3(I)
       PXZ3(I)=ZS-ZAS
       PXZ5(I)=ZS+ZAS
       XS =-D1X(I)+D3X(I)
       XAS =XG1(I)+XG3(I)-PXC3(I)
       PZX3(I)=XS-XAS
       PZX5(I)=XS+XAS
       ZS =-D2Z(I)+D3Z(I)
       ZAS =ZG2(I)+ZG3(I)-PZC3(I)
       PYZ3(I)=ZS-ZAS
       PYZ5(I)=ZS+ZAS
       YS =-D2Y(I)+D3Y(I)
       YAS =YG2(I)+YG3(I)-PYC3(I)
       PZY3(I)=YS-YAS
       PZY5(I)=YS+YAS
      ENDDO
C-------Node 4,6-----
      DO I=1,NEL
       XG1(I) = PX4H1(I)*D1X(I)
       XG2(I) = PX4H2(I)*D2X(I)
       XG3(I) = PX4H3(I)*D3X(I)
       XG4(I) = PX4H4(I)*D4X(I)
       YG1(I) = PX4H1(I)*D1Y(I)
       YG2(I) = PX4H2(I)*D2Y(I)
       YG3(I) = PX4H3(I)*D3Y(I)
       YG4(I) = PX4H4(I)*D4Y(I)
       ZG1(I) = PX4H1(I)*D1Z(I)
       ZG2(I) = PX4H2(I)*D2Z(I)
       ZG3(I) = PX4H3(I)*D3Z(I)
       ZG4(I) = PX4H4(I)*D4Z(I)
      ENDDO
C
      DO I=1,NEL
C
       YS =-D1Y(I)+D2Y(I)
       YAS =YG1(I)+YG2(I)-PYC4(I)
       PXY4(I)=YS-YAS
       PXY6(I)=YS+YAS
       XS =-D1X(I)+D2X(I)
       XAS =XG1(I)+XG2(I)-PXC4(I)
       PYX4(I)=XS-XAS
       PYX6(I)=XS+XAS
       ZS =-D1Z(I)-D3Z(I)
       ZAS =ZG1(I)+ZG3(I)-PZC4(I)
       PXZ4(I)=ZS-ZAS
       PXZ6(I)=ZS+ZAS
       XS =-D1X(I)-D3X(I)
       XAS =XG1(I)+XG3(I)-PXC4(I)
       PZX4(I)=XS-XAS
       PZX6(I)=XS+XAS
       ZS =D2Z(I)-D3Z(I)
       ZAS =ZG2(I)+ZG3(I)-PZC4(I)
       PYZ4(I)=ZS-ZAS
       PYZ6(I)=ZS+ZAS
       YS =D2Y(I)-D3Y(I)
       YAS =YG2(I)+YG3(I)-PYC4(I)
       PZY4(I)=YS-YAS
       PZY6(I)=YS+YAS
      ENDDO
C      
      RETURN
      END
