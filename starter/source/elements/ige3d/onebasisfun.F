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
Chd|  ONEBASISFUN                   source/elements/ige3d/onebasisfun.F
Chd|-- called by -----------
Chd|        IG3DONEBASIS                  source/elements/ige3d/ig3donebasis.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ONEBASISFUN(IDXII, IDXI, PXI, XI, KXI, DERS1)
C--------------------------------------------------------------------------------------------------------
C
C This subroutine calculates B-spline function 
C Assembling B spline functions for NURBS is outside this subroutine 
C
C--------------------------------------------------------------------------------------------------------
C VAR      | SIZE      | TYP  |  RW  |  DEFINITION
C--------------------------------------------------------------------------------------------------------
C IDXI     | 1         | I    |  R   | ELEMENT INDEX IN KNOT VECTOR IN XI DIRECTION
C PXI      | 1         | I    |  R   | POLYNOMIAL INTERPOLATION DEGREE IN XI DIRECTION
C XI       | 1         | F    |  R   | COUNTER PARAMETER VALUE (WHERE THE FUNCTION AND DERIVATE ARE CALCULATED)
C--------------------------------------------------------------------------------------------------------
C KXI      | NKXI      | F    |  R   | (FULL) KNOT VECTOR IN XI DIRECTION FOR THE CURRENT PATCH (GROUP)
C--------------------------------------------------------------------------------------------------------
C DERS1    | PXI+1     | F    |  W   | INTERPOLATION FUNCTION
C DERS2    | PXI+1     | F    |  W   | DERIVATE OF INTERPOLATION FUNCTION
C--------------------------------------------------------------------------------------------------------
C Implicite Types
C--------------------------------------------------------------------------------------------------------
#include     "implicit_f.inc"
C--------------------------------------------------------------------------------------------------------
C Dummy Arguments 
C--------------------------------------------------------------------------------------------------------
      INTEGER PXI, IDXI, IDXII
      my_real, 
     .  INTENT(IN) :: XI
      my_real, 
     .  DIMENSION(*), INTENT(IN) :: KXI
      my_real DERS1
C--------------------------------------------------------------------------------------------------------
C Local variables 
C--------------------------------------------------------------------------------------------------------
      INTEGER J, JJ, K, L, NDERS
      my_real 
     .  SAVED, TEMP, ALEFT, RIGHT
      my_real, 
     .  DIMENSION(PXI+1,PXI+1) :: ANDU

      ANDU(:,:)=ZERO
      ANDU(IDXII,1)=ONE

c      DO J=0,PXI
c        IF ((XI>=KXI(IDXI+J)).AND.(XI<KXI(IDXI+J+1))) THEN
c          ANDU(J+1,1) = ONE
c        ELSE
c          ANDU(J+1,1) = ZERO
c        ENDIF
c      ENDDO

      DO K=1,PXI
        IF (ANDU(1,K) == 0) THEN
          SAVED = ZERO
        ELSE 
          SAVED = ((XI-KXI(IDXI))*ANDU(1,K))/(KXI(IDXI+K)-KXI(IDXI))
        ENDIF
        DO J=0,PXI-K
          ALEFT = KXI(IDXI+J+1)
          RIGHT = KXI(IDXI+J+K+1)
          IF (ANDU(J+2,K) == 0) THEN
            ANDU(J+1,K+1) = SAVED
            SAVED = ZERO
          ELSE
            TEMP = ANDU(J+2,K)/(RIGHT-ALEFT)
            ANDU(J+1,K+1) = SAVED+(RIGHT-XI)*TEMP
            SAVED = (XI-ALEFT)*TEMP
          ENDIF  
        ENDDO
      ENDDO
      
      DERS1 = ANDU(1,PXI+1) 

      RETURN
      END
