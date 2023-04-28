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
Chd|  S8_TSTRAIN                    source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- called by -----------
Chd|        H3D_SOLID_TENSOR_1            source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- calls ---------------
Chd|        T8DERI3                       source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|        U_FROM_F3                     source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|====================================================================
        SUBROUTINE S8_TSTRAIN(XN,YN,ZN,DX,DY,DZ,STRAIN,NEL)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C XN      |  8*NEL  | R | R | X-coordinate ARRAY (8n Hexa)
C YN      |  8*NEL  | R | R | Y-coordinate ARRAY (8n Hexa)
C ZN      |  8*NEL  | R | R | Z-coordinate ARRAY (8n Hexa)
C DX      |  8*NEL  | R | R | X-Displ ARRAY (8n Hexa)
C DY      |  8*NEL  | R | R | Y-Displ ARRAY (8n Hexa)
C DZ      |  8*NEL  | R | R | D-Displ ARRAY (8n Hexa)
C STRAIN  |  6*NEL  | R | W | STRAIN ARRAY
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER NEL
      my_real 
     .   XN(NEL,8) , YN(NEL,8) , ZN(NEL,8), 
     .   DX(NEL,8) , DY(NEL,8) , DZ(NEL,8),STRAIN(6,*) 
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,NNOD
      PARAMETER (NNOD = 8)
      my_real 
     .   X0N(NEL,NNOD) , Y0N(NEL,NNOD) , Z0N(NEL,NNOD), 
     .   PX1(NEL), PX2(NEL), PX3(NEL), PX4(NEL),  
     .   PY1(NEL), PY2(NEL), PY3(NEL), PY4(NEL),  
     .   PZ1(NEL), PZ2(NEL), PZ3(NEL), PZ4(NEL),
     .   VOL(NEL),F(3,3,NEL),
     .   VX17, VY17, VZ17,
     .   VX28, VY28, VZ28,
     .   VX35, VY35, VZ35,
     .   VX46, VY46, VZ46
C----------------------------------------------
C------initial configuration :
        DO I=1,NEL
           X0N(I,1:NNOD) = XN(I,1:NNOD)-DX(I,1:NNOD)
           Y0N(I,1:NNOD) = YN(I,1:NNOD)-DY(I,1:NNOD)
           Z0N(I,1:NNOD) = ZN(I,1:NNOD)-DZ(I,1:NNOD)
        END DO
        CALL T8DERI3(
     .   X0N(1,1),X0N(1,2),X0N(1,3),X0N(1,4),
     .   X0N(1,5),X0N(1,6),X0N(1,7),X0N(1,8),
     .   Y0N(1,1),Y0N(1,2),Y0N(1,3),Y0N(1,4), 
     .   Y0N(1,5),Y0N(1,6),Y0N(1,7),Y0N(1,8), 
     .   Z0N(1,1),Z0N(1,2),Z0N(1,3),Z0N(1,4),
     .   Z0N(1,5),Z0N(1,6),Z0N(1,7),Z0N(1,8),
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4, 
     .   VOL  ,NEL )
C---------------
C  MEMBRANE [F]-[1] 
C---------------
       DO I=1,NEL
         VX17=DX(I,1)-DX(I,7)
         VX28=DX(I,2)-DX(I,8)
         VX35=DX(I,3)-DX(I,5)
         VX46=DX(I,4)-DX(I,6)
         VY17=DY(I,1)-DY(I,7)
         VY28=DY(I,2)-DY(I,8)
         VY35=DY(I,3)-DY(I,5)
         VY46=DY(I,4)-DY(I,6)
         VZ17=DZ(I,1)-DZ(I,7)
         VZ28=DZ(I,2)-DZ(I,8)
         VZ35=DZ(I,3)-DZ(I,5)
         VZ46=DZ(I,4)-DZ(I,6)
C
         F(1,1,I)=PX1(I)*VX17+PX2(I)*VX28+PX3(I)*VX35+PX4(I)*VX46
         F(2,2,I)=PY1(I)*VY17+PY2(I)*VY28+PY3(I)*VY35+PY4(I)*VY46
         F(3,3,I)=PZ1(I)*VZ17+PZ2(I)*VZ28+PZ3(I)*VZ35+PZ4(I)*VZ46
         F(1,2,I)=PY1(I)*VX17+PY2(I)*VX28+PY3(I)*VX35+PY4(I)*VX46
         F(1,3,I)=PZ1(I)*VX17+PZ2(I)*VX28+PZ3(I)*VX35+PZ4(I)*VX46
         F(2,1,I)=PX1(I)*VY17+PX2(I)*VY28+PX3(I)*VY35+PX4(I)*VY46
         F(2,3,I)=PZ1(I)*VY17+PZ2(I)*VY28+PZ3(I)*VY35+PZ4(I)*VY46
         F(3,1,I)=PX1(I)*VZ17+PX2(I)*VZ28+PX3(I)*VZ35+PX4(I)*VZ46
         F(3,2,I)=PY1(I)*VZ17+PY2(I)*VZ28+PY3(I)*VZ35+PY4(I)*VZ46
         
       END DO
C---------------
C  MEMBRANE [e]=[U]-1 
C---------------
       CALL U_FROM_F3(F,STRAIN,NEL)
C
      RETURN
      END
