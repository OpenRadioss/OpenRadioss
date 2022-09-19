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
Chd|  CHOLFACT                      source/tools/lagmul/cholfact.F
Chd|-- called by -----------
Chd|        LAG_MULT_SOLV                 source/tools/lagmul/lag_mult_solv.F
Chd|        LAG_MULT_SOLVP                source/tools/lagmul/lag_mult_solv.F
Chd|-- calls ---------------
Chd|====================================================================
      INTEGER FUNCTION CHOLFACT(N,DIAG,H,IADH,JCIH,WCOL,IROW,JCOL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, IADH(*),JCIH(*),IROW(*),JCOL(*)
      my_real DIAG(*),H(*),WCOL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, ISK, IEK, ISJ, IEJ, IC, IPTR
      my_real LVAL, T
C======================================================================|
      DO J = 1, N
        IROW(J) = 0
        JCOL(J) = 0
      ENDDO
C---
      DO K = 1,N
        ISK = IADH(K)
        IEK = IADH(K+1)-1
        DO J = ISK, IEK
          IC = JCIH(J)
          WCOL(IC) = H(J)
          IROW(IC) = 1
        ENDDO
C---
        IF (DIAG(K)<=0.) GOTO 999
        DIAG(K) = SQRT(DIAG(K))
C---
        J = JCOL(K)
100     CONTINUE
        IF (J==0) GOTO 200
          ISJ = IROW(J)
          IEJ = IADH(J+1)-1
          LVAL = H(ISJ)
          ISJ = ISJ + 1
          IF (ISJ<IEJ) THEN
            IROW(J) = ISJ
            IPTR = J
            J = JCOL(J)
            JCOL(IPTR) = JCOL(JCIH(ISJ))
            JCOL(JCIH(ISJ)) = IPTR
          ELSE
            J = JCOL(J)
          ENDIF
          DO I = ISJ, IEJ
            IC = JCIH(I)
            IF (IROW(IC)/=0) THEN
              WCOL(IC) = WCOL(IC) - LVAL*H(I)
            ENDIF
          ENDDO
        GOTO 100
200     CONTINUE
C---
        IF (ISK<IEK) THEN
          IPTR = JCIH(ISK)
          JCOL(K) = JCOL(IPTR)
          JCOL(IPTR) = K
          IROW(K) = ISK
        ENDIF
C---
        DO J = ISK, IEK
          IC = JCIH(J)
          T = WCOL(IC)/DIAG(K)
          DIAG(IC) = DIAG(IC) - T*T
          H(J) = T
          IROW(IC) = 0
        ENDDO
      ENDDO
C----
      CHOLFACT = 0
      RETURN
999   CONTINUE
      CHOLFACT = K
C-----------------------------------------------
      RETURN
      END

Chd|====================================================================
Chd|  PRECHOL                       source/tools/lagmul/cholfact.F
Chd|-- called by -----------
Chd|        LAG_MULT_SOLV                 source/tools/lagmul/lag_mult_solv.F
Chd|        LAG_MULT_SOLVP                source/tools/lagmul/lag_mult_solv.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PRECHOL(Z,D,L,R,NC,IADH,JCIH)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC,IADH(*),JCIH(*)
      my_real
     .    Z(*),D(*),L(*),R(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IH
      my_real S
C======================================================================|
C--- back subst Ly = r
      DO I=1,NC
        Z(I) = R(I)
      ENDDO
      DO I=1,NC
        Z(I) = Z(I) / D(I)
        DO IH=IADH(I),IADH(I+1)-1
          J = JCIH(IH)
          Z(J) = Z(J) - L(IH)*Z(I)
        ENDDO
      ENDDO
C--- back subst LT z = y
      DO I=NC,1,-1
        S = Z(I)
        DO IH=IADH(I),IADH(I+1)-1
          J = JCIH(IH)
          S = S - L(IH)*Z(J)
        ENDDO
        Z(I) = S / D(I)
      ENDDO
C-----------------------------------------------
      RETURN
      END
