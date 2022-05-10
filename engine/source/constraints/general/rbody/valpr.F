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
C
Chd|====================================================================
Chd|  VALPR                         source/constraints/general/rbody/valpr.F
Chd|-- called by -----------
Chd|        INEPRI                        source/constraints/general/rbody/inepri.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VALPR (A,R,N,MV)
C-----------------------------------
C     IBM FORTRAM LIBRARY EIGENVALUE
C-----------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, MV
C     REAL ou REAL*8
      my_real
     .   A(*), R(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IQ, J, I, IJ, IA, IND, L, M, MQ, LQ, LM, LL, MM, ILQ, IMQ,
     .   IM, IL, ILR, IMR, JQ, K
C     REAL ou REAL*8
      my_real
     .   RANGE, ANORM, ANRMX, THR, X, Y, SINX, SINX2, COSX, COSX2,
     .   SINCS
      RANGE = 1.0E-7
      IF (MV-1.EQ.0)GOTO 50
   20 IQ = -N
      DO 40 J = 1,N
      IQ = IQ+N
      DO 40 I = 1,N
      IJ = IQ+I
      R(IJ) = ZERO
      IF (I-J.NE.0)GOTO 40
   30 R(IJ) = UN
   40 CONTINUE
   50 ANORM = ZERO
      DO 70 I = 1,N
      DO 70 J = 1,N
      IF (I-J) 60,70,60
   60 IA = I+(J*J-J)/2
      ANORM = ANORM+A(IA)*A(IA)
   70 CONTINUE
      IF (ANORM) 330,330,80
   80 ANORM = 1.414*SQRT(ANORM)
      ANRMX = ANORM*RANGE/FLOAT(N)
      IND = 0
      THR = ANORM
   90 THR = THR/FLOAT(N)
  100 L = 1
  110 M = L+1
  120 MQ = (M*M-M)/2
      LQ = (L*L-L)/2
      LM = L+MQ
      IF (ABS(A(LM))-THR) 260,130,130
  130 IND = 1
      LL = L+LQ
      MM = M+MQ
      X = UNDEMI*(A(LL)-A(MM))
      Y = -A(LM)/SQRT(A(LM)*A(LM)+X*X)
      IF (X.LT.ZERO) THEN
       Y = -Y
      ELSEIF(X.EQ.ZERO)THEN
       Y = -UN
      ENDIF
      SINX = Y/SQRT(DEUX*(UN+(SQRT(UN-Y*Y))))
      SINX2 = SINX*SINX
      COSX = SQRT(UN - SINX2)
      COSX2 = COSX*COSX
      SINCS = SINX*COSX
      ILQ = N*(L-1)
      IMQ = N*(M-1)
      DO 250 I = 1,N
      IQ = (I*I-I)/2
      IF (I-L) 160,230,160
  160 IF (I-M) 170,230,180
  170 IM = I+MQ
      GO TO 190
  180 IM = M+IQ
  190 IF (I-L) 200,210,210
  200 IL = I+LQ
      GO TO 220
  210 IL = L+IQ
  220 X = A(IL)*COSX-A(IM)*SINX
      A(IM) = A(IL)*SINX+A(IM)*COSX
      A(IL) = X
  230 IF (MV-1) 240,250,240
  240 ILR = ILQ+I
      IMR = IMQ+I
      X = R(ILR)*COSX-R(IMR)*SINX
      R(IMR) = R(ILR)*SINX+R(IMR)*COSX
      R(ILR) = X
  250 CONTINUE
      X = DEUX*A(LM)*SINCS
      Y = A(LL)*COSX2+A(MM)*SINX2-X
      X = A(LL)*SINX2+A(MM)*COSX2+X
      A(LM) = (A(LL)-A(MM))*SINCS+A(LM)*(COSX2-SINX2)
      A(LL) = Y
      A(MM) = X
  260 IF (M-N) 270,280,270
  270 M = M+1
      GO TO 120
  280 IF (L-(N-1)) 290,300,290
  290 L = L+1
      GO TO 110
  300 IF (IND-1) 320,310,320
  310 IND = 0
      GO TO 100
  320 IF (THR-ANRMX) 330,330,90
  330 IQ = -N
C
      DO 370 I = 1,N
      IQ = IQ+N
      LL = I+(I*I-I)/2
      JQ = N*(I-2)
      DO 370 J = I,N
      JQ = JQ+N
      MM = J+(J*J-J)/2
      IF (A(LL).GE.A(MM)) GOTO 370
       X = A(LL)
       A(LL) = A(MM)
       A(MM) = X
       IF (MV.EQ.1) GOTO 370
        DO 360 K = 1,N
         ILR = IQ+K
         IMR = JQ+K
         X = R(ILR)
         R(ILR) = R(IMR)
         R(IMR) = X
  360   CONTINUE
  370 CONTINUE
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
C
      RETURN
      END
