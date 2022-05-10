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
C
Chd|====================================================================
Chd|  MINTER1D_RAT                  eos/minter1d_rat.F            
Chd|-- called by -----------
Chd|        MINTP1_RT                     common_source/eos/mintp1_rt.F 
Chd|        MINTP_RE                      common_source/eos/mintp_re.F  
Chd|        MINTP_RT                      common_source/eos/mintp_rt.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MINTER1D_RAT(X0,X1,X2,X3,Y0,Y1,Y2,Y3,X,Y,YP,I,N)
C
C     INTERPOLATION (RATIONAL FUNCTION)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I, N
C     REAL ou REAL*8
      my_real
     .   X0, X1, X2, X3, Y0, Y1, Y2, Y3, X, Y, YP     
Csm45a2     .   X0, X1, X2, X3, Y0, Y1, Y2, Y3, X, Y, YP,EM30
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL ou REAL*8
      my_real
     .   Q, D, R, S, SP, C2, DM, SM, C1, C6, C4, C3, C3D, C5
C-----------------------------------------------
C
Csm45a2      EM30 = 1.E-30
      Q = X-X1
      D = X2-X1
      R = D-Q
      S = (Y2-Y1) / D
      SP = (Y3-Y2) / (X3-X2)
      C2 = (SP-S) / (X3-X1)
      DM = X1-X0
Cgw065 +1
      DM = SIGN(MAX(EM30,ABS(DM)),DM)
      SM = (Y1-Y0) / DM
      C1 = (S-SM) / (D+DM)
Csm45a2      C6 = 0.
      C6 = ZERO
C
      IF(I.EQ.1)THEN
Csm45a2        IF(S*(S-D*C2).LE.0.) C2 = S/D
        IF(S*(S-D*C2).LE.ZERO) C2 = S/D
        C4 = C2
      ELSE IF(I.EQ.N-1)THEN
        C4 = C1
      ELSE
        IF(I.EQ.2.AND.SM*(SM-DM*C1).LE.0.)C1=(S-SM-SM) / D
        C3 = ABS(C2*R)
        C3D = C3+ABS(C1*Q)
Csm45a2        C5 = 0.
Csm45a2        IF(C3D.GT.0.) THEN
        C5 = ZERO
        IF(C3D.GT.ZERO) THEN
          C3 = C3/C3D
          C5 = C3*(C1-C2)
        ENDIF
        C4 = C2+C5
        C6 = D*C5*(1.-C3)
      ENDIF
      Y = Y1 + Q*(S-R*C4)
      YP = S+(Q-R)*C4+C6
      RETURN
      END
