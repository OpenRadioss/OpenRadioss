Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
C
Chd|====================================================================
Chd|  VALPR                         source/constraints/general/rbody/valpr.F
Chd|-- called by -----------
Chd|        INEPRI                        source/constraints/general/rbody/inepri.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VALPR (A,R,N,MV)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, MV
      my_real A(*), R(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IQ, J, I, IJ, IA, IND, L, M, MQ, LQ, LM, LL, MM, ILQ, IMQ, IM, IL, ILR, IMR, JQ, K
      my_real RANGE, ANORM, ANRMX, THR, X, Y, SINX, SINX2, COSX, COSX2, SINCS

      RANGE = 1.0E-7

      IF (MV-1 /= 0)THEN
        IQ = -N
        DO J = 1,N
          IQ = IQ+N
          DO I = 1,N
            IJ = IQ+I
            R(IJ) = ZERO
            IF (I-J /= 0)CYCLE
            R(IJ) = ONE
          ENDDO
        ENDDO
      ENDIF

      ANORM = ZERO
      DO I = 1,N
       DO J = 1,N
        IF (I-J /= 0)THEN   ! 60,70,60
          IA = I+(J*J-J)/2
          ANORM = ANORM+A(IA)*A(IA)
        ENDIF
       ENDDO
      ENDDO

      IF (ANORM >= ZERO)THEN
        ANORM = 1.414*SQRT(ANORM)
        ANRMX = ANORM*RANGE/FLOAT(N)
        IND = 0
        THR = ANORM
        THR = THR/FLOAT(N)
        L = 1
        M = L+1
        MQ = (M*M-M)/2
        LQ = (L*L-L)/2
        LM = L+MQ
        DO
          IF (ABS(A(LM))-THR >=0) THEN
            IND = 1
            LL = L+LQ
            MM = M+MQ
            X = HALF*(A(LL)-A(MM))
            Y = -A(LM)/SQRT(A(LM)*A(LM)+X*X)
            IF (X<ZERO) THEN
             Y = -Y
            ELSEIF(X==ZERO)THEN
             Y = -ONE
            ENDIF
            SINX = Y/SQRT(TWO*(ONE+(SQRT(ONE-Y*Y))))
            SINX2 = SINX*SINX
            COSX = SQRT(ONE - SINX2)
            COSX2 = COSX*COSX
            SINCS = SINX*COSX
            ILQ = N*(L-1)
            IMQ = N*(M-1)
            DO I = 1,N
              IQ = (I*I-I)/2
              IF (I-L == 0) CYCLE
              IF (I-M > 0)THEN
                IM = M+IQ
              ELSEIF (I-M < 0)THEN
                IM = I+MQ
              ELSE
                CYCLE
              ENDIF
              IF (I-L<0) THEN
                IL = I+LQ
              ELSE
                IL = L+IQ
              ENDIF
              X = A(IL)*COSX-A(IM)*SINX
              A(IM) = A(IL)*SINX+A(IM)*COSX
              A(IL) = X
              IF (MV-1 == 0)CYCLE
              ILR = ILQ+I
              IMR = IMQ+I
              X = R(ILR)*COSX-R(IMR)*SINX
              R(IMR) = R(ILR)*SINX+R(IMR)*COSX
              R(ILR) = X
            ENDDO
            X = TWO*A(LM)*SINCS
            Y = A(LL)*COSX2+A(MM)*SINX2-X
            X = A(LL)*SINX2+A(MM)*COSX2+X
            A(LM) = (A(LL)-A(MM))*SINCS+A(LM)*(COSX2-SINX2)
            A(LL) = Y
            A(MM) = X
          ELSE
            IF (M-N /=0) THEN
              M = M+1
              MQ = (M*M-M)/2
              LQ = (L*L-L)/2
              LM = L+MQ
              CYCLE
            ENDIF

            IF (L-(N-1) /=0)THEN
              L = L+1
              M = L+1
              CYCLE
            ENDIF
            IF (IND-1 == 0) THEN
              IND = 0
              L = 1
              CYCLE
            ENDIF
            IF (THR-ANRMX <= 0) EXIT
            THR = THR/FLOAT(N)
          ENDIF
        ENDDO
      ENDIF

      IQ = -N

      DO I = 1,N
        IQ = IQ+N
        LL = I+(I*I-I)/2
        JQ = N*(I-2)
        DO J = I,N
          JQ = JQ+N
          MM = J+(J*J-J)/2
          IF (A(LL)>=A(MM)) CYCLE
           X = A(LL)
           A(LL) = A(MM)
           A(MM) = X
           IF (MV==1) CYCLE
            DO K = 1,N
             ILR = IQ+K
             IMR = JQ+K
             X = R(ILR)
             R(ILR) = R(IMR)
             R(IMR) = X
            ENDDO
        ENDDO
      ENDDO
C
      ANORM=SQRT(R(1)*R(1)+R(2)*R(2)+R(3)*R(3))
      R(1)=R(1)/ANORM
      R(2)=R(2)/ANORM
      R(3)=R(3)/ANORM
      ANORM=SQRT(R(4)*R(4)+R(5)*R(5)+R(6)*R(6))
      R(4)=R(4)/ANORM
      R(5)=R(5)/ANORM
      R(6)=R(6)/ANORM
      R(7)=R(2)*R(6)-R(3)*R(5)
      R(8)=R(3)*R(4)-R(1)*R(6)
      R(9)=R(1)*R(5)-R(2)*R(4)
      ANORM=SQRT(R(7)*R(7)+R(8)*R(8)+R(9)*R(9))
      R(7)=R(7)/ANORM
      R(8)=R(8)/ANORM
      R(9)=R(9)/ANORM

      RETURN
      END
