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
Chd|  C3SYSG2L                      source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- called by -----------
Chd|        SH3_TSTRAIN                   source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- calls ---------------
Chd|        CLSYS3                        source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|====================================================================
      SUBROUTINE C3SYSG2L(XN,YN,ZN,XL2,YL2,XL3,YL3,AREA,NEL) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C XN      |  3*NEL  | R | R | X-coordinate ARRAY (3n tria)
C YN      |  3*NEL  | R | R | Y-coordinate ARRAY (3n tria)
C ZN      |  3*NEL  | R | R | Z-coordinate ARRAY (3n tria)
C XL2     |    NEL  | R | W | Local X-coordinate of N2 (relative to N1)
C YL2     |    NEL  | R | W | Local Y-coordinate of N2 (relative to N1)
C XL3     |    NEL  | R | W | Local X-coordinate of N3 (relative to N1)
C YL3     |    NEL  | R | W | Local Y-coordinate of N3 (relative to N1)
C AREA    |    NEL  | R | W | AREA of tria
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   XN(3,*) , YN(3,*) , ZN(3,*), 
     .   XL2(NEL),XL3(NEL),YL2(NEL),YL3(NEL),AREA(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   RX(NEL),RY(NEL),RZ(NEL),SX(NEL),SY(NEL),SZ(NEL),
     .   VQ(3,3,NEL), DETA1(NEL),XX,YY,ZZ
        DO I=1,NEL
          RX(I)=XN(2,I)-XN(1,I)
          RY(I)=YN(2,I)-YN(1,I)
          RZ(I)=ZN(2,I)-ZN(1,I)
          SX(I)=XN(3,I)-XN(1,I)
          SY(I)=YN(3,I)-YN(1,I)
          SZ(I)=ZN(3,I)-ZN(1,I)
        ENDDO 
        CALL CLSYS3(RX, RY, RZ, SX, SY, SZ, 
     .              VQ, DETA1,NEL)
C------ Global -> Local Coordinate  
        DO I=1,NEL
          XX=RX(I)
          YY=RY(I)
          ZZ=RZ(I)
          XL2(I)=VQ(1,1,I)*XX+VQ(2,1,I)*YY+VQ(3,1,I)*ZZ
          YL2(I)=VQ(1,2,I)*XX+VQ(2,2,I)*YY+VQ(3,2,I)*ZZ
C          
          XX=SX(I)
          YY=SY(I)
          ZZ=SZ(I)
          XL3(I)=VQ(1,1,I)*XX+VQ(2,1,I)*YY+VQ(3,1,I)*ZZ
          YL3(I)=VQ(1,2,I)*XX+VQ(2,2,I)*YY+VQ(3,2,I)*ZZ
          AREA(I)=0.5*DETA1(I)
        ENDDO 
c-----------
      RETURN
      END
