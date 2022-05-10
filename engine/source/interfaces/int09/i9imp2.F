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
Chd|  I9IMP2                        source/interfaces/int09/i9imp2.F
Chd|-- called by -----------
Chd|        I9MAIN2                       source/interfaces/int09/i9main2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I9IMP2(X     ,V     ,A     ,IRECT,MSR  ,
     1                  NSV   ,ILOC  ,IRTL  ,NSN  ,CST  ,
     2                  FRIGAP,NOR   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN
      INTEGER IRECT(4,*), MSR(*), NSV(*), ILOC(*), IRTL(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*), V(3,*), A(3,*), CST(2,*),FRIGAP(*), NOR(3,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IX(2), II, I, J, K, L, M, IMP, I3, I2, JJ, J3, J2, LOLD
C     REAL ou REAL*8
      my_real
     .   H(2), N2, N3, GAP, YM1, ZM1, YM2, ZM2, YS, ZS, T2, T3,
     .   XL, ANS, SS, STIF, FNI, FYI, FZI, SS0, FTI, DS, ANST
C-----------------------------------------------
      GAP =FRIGAP(2)
C
      DO 500 II=1,NSN
      I=NSV(II)
      J=ILOC(II)
      K=MSR(J)
      L=IRTL(II)
      M=MSR(IRECT(1,L))
      IX(1)=M
      YM1=X(2,M) + (V(2,M) + A(2,M)*DT12)*DT2
      ZM1=X(3,M) + (V(3,M) + A(3,M)*DT12)*DT2
      M=MSR(IRECT(2,L))
      IX(2)=M
      YM2=X(2,M) + (V(2,M) + A(2,M)*DT12)*DT2
      ZM2=X(3,M) + (V(3,M) + A(3,M)*DT12)*DT2
      YS =X(2,I) + (V(2,I) + A(2,I)*DT12)*DT2
      ZS =X(3,I) + (V(3,I) + A(3,I)*DT12)*DT2
      T2=YM2-YM1
      T3=ZM2-ZM1
      XL=SQRT(T2**2+T3**2)
      T2=T2/XL
      T3=T3/XL
      N2= T3
      N3=-T2
C
      ANS =N2*(YS-YM1)+N3*(ZS-ZM1)
      ANS =ANS-GAP
      IF(ANS.GT.ZERO)THEN
        ILOC(II)=-ILOC(II)
      ELSE
        H(2)=T2*(YS-YM1)+T3*(ZS-ZM1)
        H(2)=H(2)/XL
        H(1)=UN - H(2)
        SS=H(2)-H(1)
        IF(SS.GT. UNP05)ILOC(II)=-ILOC(II)
        IF(SS.LT.-UNP05)ILOC(II)=-ILOC(II)
        SS= MAX(-UN,SS)
        SS= MIN( UN,SS)
        NOR(2,II)=N2
        NOR(3,II)=N3
        CST(1,II)=SS
      ENDIF
C
 500  CONTINUE
      RETURN
      END
