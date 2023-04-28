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
Chd|  SH4_TSTRAIN                   source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- called by -----------
Chd|        H3D_SHELL_TENSOR              source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- calls ---------------
Chd|        C4SYSG2L                      source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|====================================================================
        SUBROUTINE SH4_TSTRAIN(XN,YN,ZN,DX,DY,DZ,STRAIN,NEL)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C XN      |  4*NEL  | R | R | X-coordinate ARRAY (4n quad)
C YN      |  4*NEL  | R | R | Y-coordinate ARRAY (4n quad)
C ZN      |  4*NEL  | R | R | Z-coordinate ARRAY (4n quad)
C DX      |  4*NEL  | R | R | X-Displ ARRAY (4n quad)
C DY      |  4*NEL  | R | R | Y-Displ ARRAY (4n quad)
C DZ      |  4*NEL  | R | R | D-Displ ARRAY (4n quad)
C STRAIN  |  3*NEL  | R | W | STRAIN ARRAY
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER NEL
      my_real 
     .   XN(4,NEL) , YN(4,NEL) , ZN(4,NEL), 
     .   DX(4,NEL) , DY(4,NEL) , DZ(4,NEL),STRAIN(3,*) 
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,NNOD
      PARAMETER (NNOD = 4)
      my_real 
     .   X0N(NNOD,NEL) , Y0N(NNOD,NEL) , Z0N(NNOD,NEL), 
     .   LXYZ0(3),DETA1(NEL),COREL(2,NNOD),XX,YY,ZZ,
     .   XL2(NEL),XL3(NEL),XL4(NEL),YL2(NEL),
     .   YL3(NEL),YL4(NEL),ZL1(NEL),ZL(NEL),
     .   X13,X24,Y13,Y24,UX1,UY1,UX2,UX3,UX4,UY2,UY3,UY4,
     .   PX1(NEL),PX2(NEL),PY1(NEL),PY2(NEL),
     .   UX13(NEL),UX24(NEL),UY13(NEL),UY24(NEL),
     .   X0L2(NEL),X0L3(NEL),X0L4(NEL),Y0L2(NEL),
     .   Y0L3(NEL),Y0L4(NEL),AREA(NEL),AREA_I(NEL),FXX,FYY,FXY,FYX
C----------------------------------------------
C------Compute coordinates in elementary local sys: actual configuration first
        CALL C4SYSG2L(XN,YN,ZN,XL2,YL2,XL3,YL3,XL4,YL4,ZL1,AREA,NEL)
C------initial configuration :
        DO I=1,NEL
           X0N(1:NNOD,I) = XN(1:NNOD,I)-DX(1:NNOD,I)
           Y0N(1:NNOD,I) = YN(1:NNOD,I)-DY(1:NNOD,I)
           Z0N(1:NNOD,I) = ZN(1:NNOD,I)-DZ(1:NNOD,I)
        ENDDO
        CALL C4SYSG2L(X0N,Y0N,Z0N,X0L2,Y0L2,X0L3,Y0L3,X0L4,Y0L4,ZL,AREA,NEL)
C-------[B0]---remove the origine to the center--------------
       DO I=1,NEL
C-----------EM20=1.0E-20,FOURTH=0.25,HALF=0.5,ZERO=0.--------------
        AREA_I(I) = ONE/MAX(EM20,AREA(I))
        LXYZ0(1)=FOURTH*(X0L2(I)+X0L3(I)+X0L4(I))
        LXYZ0(2)=FOURTH*(Y0L2(I)+Y0L3(I)+Y0L4(I))
        COREL(1,1)=-LXYZ0(1)
        COREL(1,2)=X0L2(I)-LXYZ0(1)
        COREL(1,3)=X0L3(I)-LXYZ0(1)
        COREL(1,4)=X0L4(I)-LXYZ0(1)
        COREL(2,1)=-LXYZ0(2)
        COREL(2,2)=Y0L2(I)-LXYZ0(2)
        COREL(2,3)=Y0L3(I)-LXYZ0(2)
        COREL(2,4)=Y0L4(I)-LXYZ0(2)
C----
        X13 =(COREL(1,1)-COREL(1,3))*HALF
        X24 =(COREL(1,2)-COREL(1,4))*HALF
        Y13 =(COREL(2,1)-COREL(2,3))*HALF
        Y24 =(COREL(2,2)-COREL(2,4))*HALF
        PY2(I) =X13*AREA_I(I)
        PY1(I) =-X24*AREA_I(I)
        PX2(I) =-Y13*AREA_I(I)
        PX1(I) =Y24*AREA_I(I)
       ENDDO
C------ objective disp (or projected disp to free rigide rotation)
         UX1= ZERO
         UY1= ZERO
        DO I=1,NEL
          UX2 = XL2(I)-X0L2(I)
          UY2 = YL2(I)-Y0L2(I)
          UX3 = XL3(I)-X0L3(I)
          UY3 = YL3(I)-Y0L3(I)
          UX4 = XL4(I)-X0L4(I)
          UY4 = YL4(I)-Y0L4(I)
          UX13(I)=UX1-UX3
          UX24(I)=UX2-UX4
          UY13(I)=UY1-UY3
          UY24(I)=UY2-UY4
        ENDDO 
C---------------
C  MEMBRANE [F]-1 = [B0]{d}, [e] ({d} has been projected to free rigide rotation)
C---------------
      DO I=1,NEL
        FXX = PX1(I)*UX13(I)+PX2(I)*UX24(I)
        FYY = PY1(I)*UY13(I)+PY2(I)*UY24(I)
        FYX = PX1(I)*UY13(I)+PX2(I)*UY24(I)
        FXY = PY1(I)*UX13(I)+PY2(I)*UX24(I)
        STRAIN(1,I) = FXX
        STRAIN(2,I) = FYY
        STRAIN(3,I) = 0.5*(FXY+FYX)
      ENDDO
C
      RETURN
      END
