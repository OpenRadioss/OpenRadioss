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
Chd|  DERSBASISFUNS                 source/elements/ige3d/dersbasisfuns.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DERSBASISFUNS(IDXI, PXI, XI, KXI, DERS1, DERS2)
C--------------------------------------------------------------------------------------------------------
C
C This subroutine calculates B-spline functions and derivates of B-spline functions
C Assembling B spline functions for NURBS is outside this subroutine 
C (Like subroutine Bspline_basis_and_deriv, p.101 ISOGEOMETRIC ANALYSIS)
C
C--------------------------------------------------------------------------------------------------------
C VAR      | SIZE      |  TYP  |  RW   |  DEFINITION
C--------------------------------------------------------------------------------------------------------
C IDXI     |  1         |  I    |   R   |  ELEMENT INDEX IN KNOT VECTOR IN XI DIRECTION
C PXI      |  1         |  I    |   R    |  POLYNOMIAL INTERPOLATION DEGREE IN XI DIRECTION
C XI       |  1         |  F    |  R   |  COUNTER PARAMETER VALUE (WHERE THE FUNCTION AND DERIVATE ARE CALCULATED)
C--------------------------------------------------------------------------------------------------------
C KXI      |  NKXI      |  F    |   R   |  (FULL) KNOT VECTOR IN XI DIRECTION FOR THE CURRENT PATCH (GROUP)
C--------------------------------------------------------------------------------------------------------
C DERS1    |  PXI+1     |  F    |   W   |  INTERPOLATION FUNCTION
C DERS2    |  PXI+1     |  F    |   W   |  DERIVATE OF INTERPOLATION FUNCTION
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include     "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PXI, IDXI
      my_real
     .   XI, KXI(*), DERS1(PXI+1), DERS2(PXI+1)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, L, KR, KP, J1, J2, NDERS, LS1, LS2
      my_real 
     .  SAVED, TEMP, D
      my_real, 
     .  DIMENSION(PXI+1) :: ALEFT, RIGHT
      my_real, 
     .  DIMENSION(2,PXI+1) :: DERS, A
      my_real, 
     .  DIMENSION(PXI+1,PXI+1) :: ANDU
C
C=======================================================================
C   S o u r c e  L i n e s
C=======================================================================
C
      NDERS=1
      ANDU(1,1)=UN

      DO J = 1,PXI   
        ALEFT(J+1) = XI - KXI(IDXI+1-J)
        RIGHT(J+1) = KXI(IDXI+J) - XI
        SAVED = ZERO
        DO L = 0,J-1
      	  ANDU(J+1,L+1) = RIGHT(L+2) + ALEFT(J-L+1)
      	  TEMP = ANDU(L+1,J)/ANDU(J+1,L+1)
      	  ANDU(L+1,J+1) = SAVED + RIGHT(L+2)*TEMP
      	  SAVED = ALEFT(J-L+1)*TEMP
        ENDDO
        ANDU(J+1,J+1) = SAVED
      ENDDO

C LOAD BASIS FUNCTIONS
      DO J = 0,PXI
        DERS(1,J+1) = ANDU(J+1,PXI+1)
      ENDDO

C COMPUTE DERIVATES 
      DO L = 0,PXI
        LS1 = 0
        LS2 = 1
        A(1,1) = UN
	
        DO K = 1,NDERS
          D = ZERO
          KR = L-K
          KP = PXI-K
          IF (L >= K) THEN
            A(LS2+1,1) = A(LS1+1,1)/ANDU(KP+2,KR+1)
            D = A(LS2+1,1)*ANDU(KR+1,KP+1)
          ENDIF
          IF (KR >= -1) THEN
            J1 = 1
          ELSE
            J1 = -KR
          ENDIF
          IF ((L-1) <= KP) THEN
            J2 = K-1
	  ELSE 
	    J2 = PXI-L
          ENDIF
          DO J = J1,J2
            A(LS2+1,J+1) = (A(LS1+1,J+1)-A(LS1+1,J))/ANDU(KP+2,KR+J+1)
            D = D + A(LS2+1,J+1)*ANDU(KR+J+1,KP+1)
          ENDDO
          IF (L <= KP) THEN
            A(LS2+1,K+1) = -A(LS1+1,K)/ANDU(KP+2,L+1)
            D = D + A(LS2+1,K+1)*ANDU(L+1,KP+1)
          ENDIF
          DERS(K+1,L+1) = D
          J = LS1
          LS1 = LS2
          LS2 = J
          ENDDO
      ENDDO

C MULTIPLY THROUGH BY THE CORRECT FACTORS

      L = PXI
      DO K = 1,NDERS
        DO J = 0,PXI
	  DERS(K+1,J+1) = DERS(K+1,J+1)*L
        ENDDO
        L = L*(PXI-K)
      ENDDO

      DO J = 1,PXI+1
        DERS1(J) = DERS(1,J)
        DERS2(J) = DERS(2,J)
      ENDDO
 
      RETURN
      END
