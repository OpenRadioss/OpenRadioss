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
Chd|  S6_TSTRAIN                    source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- called by -----------
Chd|        H3D_SOLID_TENSOR_1            source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- calls ---------------
Chd|        T6DERI3                       source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|        U_FROM_F3                     source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|====================================================================
        SUBROUTINE S6_TSTRAIN(XN,YN,ZN,DX,DY,DZ,STRAIN,NEL)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C XN      |  6*NEL  | R | R | X-coordinate ARRAY (6n Penta)
C YN      |  6*NEL  | R | R | Y-coordinate ARRAY (6n Penta)
C ZN      |  6*NEL  | R | R | Z-coordinate ARRAY (6n Penta)
C DX      |  6*NEL  | R | R | X-Displ ARRAY (6n Penta)
C DY      |  6*NEL  | R | R | Y-Displ ARRAY (6n Penta)
C DZ      |  6*NEL  | R | R | D-Displ ARRAY (6n Penta)
C STRAIN  |  6*NEL  | R | W | STRAIN ARRAY
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER NEL
      my_real 
     .   XN(NEL,6) , YN(NEL,6) , ZN(NEL,6), 
     .   DX(NEL,6) , DY(NEL,6) , DZ(NEL,6),STRAIN(6,*) 
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,NNOD
      PARAMETER (NNOD = 6)
      my_real 
     .   X0N(NEL,NNOD) , Y0N(NEL,NNOD) , Z0N(NEL,NNOD), 
     .   PX1(NEL), PX2(NEL), PX3(NEL), PX4(NEL),  
     .   PY1(NEL), PY2(NEL), PY3(NEL), PY4(NEL),  
     .   PZ1(NEL), PZ2(NEL), PZ3(NEL), PZ4(NEL),
     .   VOL(NEL),F(3,3,NEL)
      my_real
     .   VX14, VY14, VZ14,
     .   VX25, VY25, VZ25,
     .   VX36, VY36, VZ36,
     .   VXHI, VYHI, VZHI
C----------------------------------------------
C------initial configuration :
        DO I=1,NEL
           X0N(I,1:NNOD) = XN(I,1:NNOD)-DX(I,1:NNOD)
           Y0N(I,1:NNOD) = YN(I,1:NNOD)-DY(I,1:NNOD)
           Z0N(I,1:NNOD) = ZN(I,1:NNOD)-DZ(I,1:NNOD)
        END DO
        CALL T6DERI3(
     .   X0N(1,1),X0N(1,2),X0N(1,3),X0N(1,4),X0N(1,5),X0N(1,6),
     .   Y0N(1,1),Y0N(1,2),Y0N(1,3),Y0N(1,4),Y0N(1,5),Y0N(1,6), 
     .   Z0N(1,1),Z0N(1,2),Z0N(1,3),Z0N(1,4),Z0N(1,5),Z0N(1,6),
     .   PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4,
     .   PZ1, PZ2, PZ3, PZ4, 
     .   VOL  ,NEL )
C---------------
C  MEMBRANE [F]-[1] 
C---------------
       DO I=1,NEL
         VX14=DX(I,1)+DX(I,4)
         VX25=DX(I,2)+DX(I,5)
         VX36=DX(I,3)+DX(I,6)
         VXHI=DX(I,4)+DX(I,5)+DX(I,6)-DX(I,1)-DX(I,2)-DX(I,3)
         VY14=DY(I,1)+DY(I,4)
         VY25=DY(I,2)+DY(I,5)
         VY36=DY(I,3)+DY(I,6)
         VYHI=DY(I,4)+DY(I,5)+DY(I,6)-DY(I,1)-DY(I,2)-DY(I,3)
         VZ14=DZ(I,1)+DZ(I,4)
         VZ25=DZ(I,2)+DZ(I,5)
         VZ36=DZ(I,3)+DZ(I,6)
         VZHI=DZ(I,4)+DZ(I,5)+DZ(I,6)-DZ(I,1)-DZ(I,2)-DZ(I,3)
C
         F(1,1,I)=PX1(I)*VX14+PX2(I)*VX25+PX3(I)*VX36+PX4(I)*VXHI
         F(2,2,I)=PY1(I)*VY14+PY2(I)*VY25+PY3(I)*VY36+PY4(I)*VYHI
         F(3,3,I)=PZ1(I)*VZ14+PZ2(I)*VZ25+PZ3(I)*VZ36+PZ4(I)*VZHI
         F(1,2,I)=PY1(I)*VX14+PY2(I)*VX25+PY3(I)*VX36+PY4(I)*VXHI
         F(1,3,I)=PZ1(I)*VX14+PZ2(I)*VX25+PZ3(I)*VX36+PZ4(I)*VXHI
         F(2,1,I)=PX1(I)*VY14+PX2(I)*VY25+PX3(I)*VY36+PX4(I)*VYHI
         F(2,3,I)=PZ1(I)*VY14+PZ2(I)*VY25+PZ3(I)*VY36+PZ4(I)*VYHI
         F(3,1,I)=PX1(I)*VZ14+PX2(I)*VZ25+PX3(I)*VZ36+PX4(I)*VZHI
         F(3,2,I)=PY1(I)*VZ14+PY2(I)*VZ25+PY3(I)*VZ36+PY4(I)*VZHI
       END DO
C---------------
C  MEMBRANE [e]=[U]-1 
C---------------
       CALL U_FROM_F3(F,STRAIN,NEL)
C
      RETURN
      END
