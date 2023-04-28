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
Chd|  SH3_TSTRAIN                   source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- called by -----------
Chd|        H3D_SHELL_TENSOR              source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- calls ---------------
Chd|        C3SYSG2L                      source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|        U_FROM_F2                     source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|====================================================================
        SUBROUTINE SH3_TSTRAIN(XN,YN,ZN,DX,DY,DZ,STRAIN,NEL,IHBE)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C XN      |  3*NEL  | R | R | X-coordinate ARRAY (3n tria)
C YN      |  3*NEL  | R | R | Y-coordinate ARRAY (3n tria)
C ZN      |  3*NEL  | R | R | Z-coordinate ARRAY (3n tria)
C DX      |  3*NEL  | R | R | X-Displ ARRAY (3n tria)
C DY      |  3*NEL  | R | R | Y-Displ ARRAY (3n tria)
C DZ      |  3*NEL  | R | R | D-Displ ARRAY (3n tria)
C STRAIN  |  3*NEL  | R | W | STRAIN ARRAY
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER NEL,IHBE
      my_real 
     .   XN(3,NEL) , YN(3,NEL) , ZN(3,NEL), 
     .   DX(3,NEL) , DY(3,NEL) , DZ(3,NEL),STRAIN(3,*) 
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,NNOD
      PARAMETER (NNOD = 3)
      my_real 
     .   X0N(NNOD,NEL) , Y0N(NNOD,NEL) , Z0N(NNOD,NEL), 
     .   DETA1(NEL),XX,YY,ZZ,
     .   XL2(NEL),XL3(NEL),YL2(NEL),YL3(NEL),
     .   UX1,UY1,UX2,UX3,UY2,UY3,
     .   PX2(NEL),PX3(NEL),PY2(NEL),PY3(NEL),
     .   UX21(NEL),UX31(NEL),UY21(NEL),UY31(NEL),
     .   X0L2(NEL),X0L3(NEL),Y0L2(NEL),
     .   Y0L3(NEL),AREA(NEL),AREA_I(NEL),FXX,FYY,FXY,FYX,F(2,2,NEL) 
C----------------------------------------------
C------Compute coordinates in elementary local sys: actual configuration first
        CALL C3SYSG2L(XN,YN,ZN,XL2,YL2,XL3,YL3,AREA,NEL)
C------initial configuration :
        DO I=1,NEL
           X0N(1:NNOD,I) = XN(1:NNOD,I)-DX(1:NNOD,I)
           Y0N(1:NNOD,I) = YN(1:NNOD,I)-DY(1:NNOD,I)
           Z0N(1:NNOD,I) = ZN(1:NNOD,I)-DZ(1:NNOD,I)
        ENDDO
        CALL C3SYSG2L(X0N,Y0N,Z0N,X0L2,Y0L2,X0L3,Y0L3,AREA,NEL)
C-----------[B0]-----------------
       DO I=1,NEL
        AREA_I(I)=0.5/AREA(I)
        PX2(I)= Y0L3(I)*AREA_I(I)
        PY2(I)=-X0L3(I)*AREA_I(I)
        PX3(I)=-Y0L2(I)*AREA_I(I)
        PY3(I)= X0L2(I)*AREA_I(I)
       ENDDO
C------ objective disp (free rigide rotation)
         UX1= ZERO
         UY1= ZERO
        DO I=1,NEL
          UX2 = XL2(I)-X0L2(I)
          UY2 = YL2(I)-Y0L2(I)
          UX3 = XL3(I)-X0L3(I)
          UY3 = YL3(I)-Y0L3(I)
          UX21(I)=UX2-UX1
          UX31(I)=UX3-UX1
          UY21(I)=UY2-UY1
          UY31(I)=UY3-UY1
        ENDDO 
C---------------
C  MEMBRANE [F]-1 = [B0]{d}, [e] ({d} has been projected to free rigide rotation)
      DO I=1,NEL
        F(1,1,I) = PX2(I)*UX21(I)+PX3(I)*UX31(I)
        F(2,2,I) = PY2(I)*UY21(I)+PY3(I)*UY31(I)
        F(2,1,I) = PX2(I)*UY21(I)+PX3(I)*UY31(I)
        F(1,2,I) = PY2(I)*UX21(I)+PY3(I)*UX31(I)
        STRAIN(1,I) = F(1,1,I)
        STRAIN(2,I) = F(2,2,I)
        STRAIN(3,I) = 0.5*(F(1,2,I) + F(2,1,I))
      ENDDO
C--- local sys for 3N isn't really material ----       
      IF (IHBE==3) CALL U_FROM_F2(F,STRAIN,NEL )
C
      RETURN
      END
