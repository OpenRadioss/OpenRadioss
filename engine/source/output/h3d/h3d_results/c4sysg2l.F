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
Chd|  C4SYSG2L                      source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- called by -----------
Chd|        SH4_TSTRAIN                   source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- calls ---------------
Chd|        CLSYS3                        source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|====================================================================
      SUBROUTINE C4SYSG2L(XN,YN,ZN,XL2,YL2,XL3,YL3,XL4,YL4,ZL1,AREA,NEL) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C XN      |  4*NEL  | R | R | X-coordinate ARRAY (4n quad)
C YN      |  4*NEL  | R | R | Y-coordinate ARRAY (4n quad)
C ZN      |  4*NEL  | R | R | Z-coordinate ARRAY (4n quad)
C XL2     |    NEL  | R | W | Local X-coordinate of N2 (relative to N1)
C YL2     |    NEL  | R | W | Local Y-coordinate of N2 (relative to N1)
C ZL1     |    NEL  | R | W | Local Z-coordinate of N1 (Z1 -Z1 Z1 -Z1)
C XL3     |    NEL  | R | W | Local X-coordinate of N3 (relative to N1)
C YL3     |    NEL  | R | W | Local Y-coordinate of N3 (relative to N1)
C XL4     |    NEL  | R | W | Local X-coordinate of N4 (relative to N1)
C YL4     |    NEL  | R | W | Local Y-coordinate of N4 (relative to N1)
C AREA    |    NEL  | R | W | AREA of quad
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   XN(4,*) , YN(4,*) , ZN(4,*), 
     .   XL2(NEL),XL3(NEL),XL4(NEL),YL2(NEL),
     .   YL3(NEL),YL4(NEL),ZL1(NEL),AREA(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   RX(NEL),RY(NEL),RZ(NEL),SX(NEL),SY(NEL),SZ(NEL),
     .   VQ(3,3,NEL), LXYZ0(3),DETA1(NEL),XX,YY,ZZ
C-----------------------------------------------
        DO I=1,NEL
          RX(I)=XN(2,I)+XN(3,I)-XN(1,I)-XN(4,I)
          RY(I)=YN(2,I)+YN(3,I)-YN(1,I)-YN(4,I)
          RZ(I)=ZN(2,I)+ZN(3,I)-ZN(1,I)-ZN(4,I)
          SX(I)=XN(3,I)+XN(4,I)-XN(1,I)-XN(2,I)
          SY(I)=YN(3,I)+YN(4,I)-YN(1,I)-YN(2,I)
          SZ(I)=ZN(3,I)+ZN(4,I)-ZN(1,I)-ZN(2,I)
        ENDDO 
C------Local elem. base:
        CALL CLSYS3(RX, RY, RZ, SX, SY, SZ, 
     .              VQ, DETA1,NEL)
C------ Global -> Local Coordinate  FOURTH=0.25 ;
        DO I=1,NEL
          LXYZ0(1)=FOURTH*(XN(1,I)+XN(2,I)+XN(3,I)+XN(4,I))
          LXYZ0(2)=FOURTH*(YN(1,I)+YN(2,I)+YN(3,I)+YN(4,I))
          LXYZ0(3)=FOURTH*(ZN(1,I)+ZN(2,I)+ZN(3,I)+ZN(4,I))
          XX=XN(2,I)-XN(1,I)
          YY=YN(2,I)-YN(1,I)
          ZZ=ZN(2,I)-ZN(1,I)
          XL2(I)=VQ(1,1,I)*XX+VQ(2,1,I)*YY+VQ(3,1,I)*ZZ
          YL2(I)=VQ(1,2,I)*XX+VQ(2,2,I)*YY+VQ(3,2,I)*ZZ
          XX=XN(2,I)-LXYZ0(1)
          YY=YN(2,I)-LXYZ0(2)
          ZZ=ZN(2,I)-LXYZ0(3)
          ZL1(I)=VQ(1,3,I)*XX+VQ(2,3,I)*YY+VQ(3,3,I)*ZZ
C          
          XX=XN(3,I)-XN(1,I)
          YY=YN(3,I)-YN(1,I)
          ZZ=ZN(3,I)-ZN(1,I)
          XL3(I)=VQ(1,1,I)*XX+VQ(2,1,I)*YY+VQ(3,1,I)*ZZ
          YL3(I)=VQ(1,2,I)*XX+VQ(2,2,I)*YY+VQ(3,2,I)*ZZ
C
          XX=XN(4,I)-XN(1,I)
          YY=YN(4,I)-YN(1,I)
          ZZ=ZN(4,I)-ZN(1,I)
          XL4(I)=VQ(1,1,I)*XX+VQ(2,1,I)*YY+VQ(3,1,I)*ZZ
          YL4(I)=VQ(1,2,I)*XX+VQ(2,2,I)*YY+VQ(3,2,I)*ZZ
          AREA(I)=FOURTH*DETA1(I)
        ENDDO 
c-----------
      RETURN
      END
