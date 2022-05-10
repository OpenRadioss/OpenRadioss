Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  DXWALS                        source/output/anim/dxwals.F   
Chd|-- called by -----------
Chd|        DXYZSECT                      source/output/anim/dxyzsect.F 
Chd|-- calls ---------------
Chd|        WRITE_R_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE DXWALS(X ,RWL ,MSR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com01_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MSR
C     REAL ou REAL*8
      my_real
     .   X(3,*), RWL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,L
C     REAL ou REAL*8
      my_real
     .   XWL, YWL, ZWL,XN,YN,ZN,D,DX,DY,DZ,XX,YY,ZZ,R,XX0,YY0,ZZ0,
     .   X0(6),Y0(6),Z0(6),DX0(6),DY0(6),DZ0(6),DX1(6),DY1(6),DZ1(6)
      REAL R4
      DATA DX0/ 0., 0., 0., 0., 0., 0./
      DATA DY0/ 1.,-1., 0., 0., 0., 0./
      DATA DZ0/ 0., 0.,-1., 1., 1.,-1./
      DATA DX1/ 1., 1., 1., 1., 0., 0./
      DATA DY1/ 0., 0., 0., 0., 1., 1./
      DATA DZ1/ 0., 0., 0., 0., 0., 0./
      DATA X0 /-3.,-3.,-3.,-3.,-3., 3./
      DATA Y0 /-3., 3.,-3., 3.,-3.,-3./
      DATA Z0 /-3., 3., 3.,-3.,-3., 3./
C-----------------------------------------------
C
       XN =RWL(1)
       YN =RWL(2)
       ZN =RWL(3)
       IF(MSR.EQ.0)THEN
        XWL=RWL(4)
        YWL=RWL(5)
        ZWL=RWL(6)
       ELSE
        XWL=X(1,MSR)
        YWL=X(2,MSR)
        ZWL=X(3,MSR)
       ENDIF
C
      R = UNDEMI*RWL(7)
C
      DO I = 1,6
        XX0 = X0(I)
        YY0 = Y0(I)
        ZZ0 = Z0(I)
        DO J = 1,7
          XX = XX0
          YY = YY0
          ZZ = ZZ0
          DO L = 1,7
            D = R/SQRT(XX*XX+YY*YY+ZZ*ZZ)
            R4 = D*XX + XWL
            CALL WRITE_R_C(R4,1)
            R4 = D*YY + YWL
            CALL WRITE_R_C(R4,1)
            R4 = D*ZZ + ZWL
            CALL WRITE_R_C(R4,1)
            XX = XX + DX0(I)
            YY = YY + DY0(I)
            ZZ = ZZ + DZ0(I)
          ENDDO
          XX0 = XX0 + DX1(I)
          YY0 = YY0 + DY1(I)
          ZZ0 = ZZ0 + DZ1(I)
        ENDDO
      ENDDO
C
      RETURN
      END
