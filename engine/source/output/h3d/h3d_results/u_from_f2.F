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
Chd|  U_FROM_F2                     source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- called by -----------
Chd|        SH3_TSTRAIN                   source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE U_FROM_F2(F,STRAIN,NEL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   F(2,2,NEL), STRAIN(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      DOUBLE PRECISION 
     .         FMAT(2,2),UM(2,2),IC,I2C,I3C,IU,I2U,I3U,
     .         C11,C21,C31,C12,C22,C32,C13,C23,C33,DETJ0,DETM1,
     .         CC11,CC21,CC31,CC12,CC22,CC32,CC13,CC23,CC33,
     .         A,B,ZZ,A1,A2,A3,A4,ALPHA,C_A,S_A,C_A2,S_A2
C-----------------------------------------------
      DO I=1,NEL
        FMAT(1,1)= F(1,1,I)+1.0
        FMAT(2,2)= F(2,2,I)+1.0
        FMAT(1,2)= F(1,2,I)
        FMAT(2,1)= F(2,1,I)
        C11 = FMAT(1,1)*FMAT(1,1)+FMAT(2,1)*FMAT(2,1)
        C12 = FMAT(1,1)*FMAT(1,2)+FMAT(2,1)*FMAT(2,2)
        C22 = FMAT(1,2)*FMAT(1,2)+FMAT(2,2)*FMAT(2,2)
        CC12 = 0.5*(C11-C22)
        IC  = 0.5*(C11+C22)
        B = SQRT(CC12*CC12+C12*C12)
        CC11 = SQRT(IC + B)
        CC22 = SQRT(IC - B)
        IF (ABS(CC12)<EM20) THEN
         C_A = 0.0
         S_A = 1.0
        ELSE
         ALPHA =0.5*(atan(C12/CC12))
         C_A = COS(ALPHA)
         S_A = SIN(ALPHA)
        END IF
        C_A2 = C_A*C_A
        S_A2 = S_A*S_A
        UM(1,1) = CC11*C_A2+CC22*S_A2
        UM(2,2) = CC11*S_A2+CC22*C_A2
        UM(1,2) = (CC11-CC22)*S_A*C_A
        STRAIN(1,I) = UM(1,1)-ONE
        STRAIN(2,I) = UM(2,2)-ONE
        STRAIN(3,I) = UM(1,2)
       END DO
C
      RETURN
      END
