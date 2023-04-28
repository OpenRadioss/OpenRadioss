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
Chd|  CLSYS3                        source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- called by -----------
Chd|        C3SYSG2L                      source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|        C4SYSG2L                      source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|        SDLENSH                       source/elements/thickshell/solidec/sdlensh.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CLSYS3(RX, RY, RZ, SX, SY, SZ, VQ, DET,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   RX(*) , RY(*) , RZ(*), 
     .   SX(*) , SY(*) , SZ(*),
     .   DET(*),VQ(3,3,*)
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C RX      |    NEL  | R | R | X-of covariant vecter g1
C RY      |    NEL  | R | R | Y-of covariant vecter g1
C RZ      |    NEL  | R | R | Z-of covariant vecter g1
C SX      |    NEL  | R | R | X-of covariant vecter g2
C SY      |    NEL  | R | R | Y-of covariant vecter g2
C SZ      |    NEL  | R | R | Z-of covariant vecter g2
C VQ      |3*3*NEL  | R | W | Local elem sys bases
C DET     |    NEL  | R | W | det of g1 ^ g2
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   E1X(NEL), E1Y(NEL), E1Z(NEL),
     .   E2X(NEL), E2Y(NEL), E2Z(NEL),
     .   E3X(NEL), E3Y(NEL), E3Z(NEL), 
     .   C1,C2,CC,C1C1,C2C2,C1_1,C2_1
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO I=1,NEL
C---------E3------------
       E3X(I) = RY(I) * SZ(I) - RZ(I) * SY(I) 
       E3Y(I) = RZ(I) * SX(I) - RX(I) * SZ(I) 
       E3Z(I) = RX(I) * SY(I) - RY(I) * SX(I) 
       DET(I) = SQRT(E3X(I)*E3X(I) + E3Y(I)*E3Y(I) + E3Z(I)*E3Z(I))
C ----- EM20=1.0E-20      
       DET(I) = MAX(EM20,DET(I))
       E3X(I) = E3X(I) / DET(I) 
       E3Y(I) = E3Y(I) / DET(I) 
       E3Z(I) = E3Z(I) / DET(I) 
      ENDDO 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       DO I=1,NEL
        C1C1 = RX(I)*RX(I) + RY(I)*RY(I) + RZ(I)*RZ(I)
        C2C2 = SX(I)*SX(I) + SY(I)*SY(I) + SZ(I)*SZ(I)
C ----- ZERO=0., ONE=1.0    
        IF(C1C1 /= ZERO) THEN
          C2_1 = SQRT(C2C2/MAX(EM20,C1C1))
          C1_1 = ONE
        ELSEIF(C2C2 /= ZERO)THEN
          C2_1 = ONE
          C1_1 = SQRT(C1C1/MAX(EM20,C2C2))
        END IF 
        E1X(I) = RX(I)*C2_1+(SY(I)*E3Z(I)-SZ(I)*E3Y(I))*C1_1
        E1Y(I) = RY(I)*C2_1+(SZ(I)*E3X(I)-SX(I)*E3Z(I))*C1_1 
        E1Z(I) = RZ(I)*C2_1+(SX(I)*E3Y(I)-SY(I)*E3X(I))*C1_1
       ENDDO 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       DO I=1,NEL
        C1 = SQRT(E1X(I)*E1X(I) + E1Y(I)*E1Y(I) + E1Z(I)*E1Z(I))
        IF ( C1 /= ZERO) C1 = ONE / MAX(EM20,C1)
        E1X(I) = E1X(I)*C1
        E1Y(I) = E1Y(I)*C1
        E1Z(I) = E1Z(I)*C1
        E2X(I) = E3Y(I) * E1Z(I) - E3Z(I) * E1Y(I)
        E2Y(I) = E3Z(I) * E1X(I) - E3X(I) * E1Z(I)
        E2Z(I) = E3X(I) * E1Y(I) - E3Y(I) * E1X(I)
       ENDDO 
      DO I=1,NEL
        VQ(1,1,I)=E1X(I)
        VQ(2,1,I)=E1Y(I)
        VQ(3,1,I)=E1Z(I)
        VQ(1,2,I)=E2X(I)
        VQ(2,2,I)=E2Y(I)
        VQ(3,2,I)=E2Z(I)
        VQ(1,3,I)=E3X(I)
        VQ(2,3,I)=E3Y(I)
        VQ(3,3,I)=E3Z(I)
      ENDDO 
c-----------
      RETURN
      END
