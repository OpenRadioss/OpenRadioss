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
Chd|  I9FRMS                        source/interfaces/int09/i9frms.F
Chd|-- called by -----------
Chd|        I9MAIN2                       source/interfaces/int09/i9main2.F
Chd|        I9MAIN3                       source/interfaces/int09/i9main3.F
Chd|-- calls ---------------
Chd|        SHAPEH                        source/ale/inter/shapeh.F     
Chd|====================================================================
      SUBROUTINE I9FRMS(X    ,SKEW,A   ,FN ,FT   ,
     2                  IRECT,LMSR,CRST,MSR,NSV  ,
     3                  ILOC ,IRTL,MS  ,NOR,LCODE,
     4                  ISKEW,FRIC,MSMN,MSMT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr04_c.inc"
#include      "scr08_s_c.inc"
#include      "scr08_a_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), LMSR(4,*), MSR(*), NSV(*), ILOC(*), IRTL(*),
     .   LCODE(*), ISKEW(*)
C     REAL
      my_real
     .   X(3,*), SKEW(LSKEW,*), A(*), FN(*), FT(*), MSMN(*), MSMT(*),
     .   CRST(2,*), MS(*), NOR(3,*), FRIC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, I3, J3, I2, J2, I1, J1, II, L, JJ, NN, JJ3,
     .   JJ2, JJ1, ISK, LCOD
C     REAL
      my_real
     .   H(4), N1, N2, N3, AA(3), SS, TT, XMSS, FXI, FYI, FZI, FSN,
     .   FXN, FYN, FZN, FXT, FYT, FZT, FST, FAC, FXMI, FYMI, FZMI,
     .   FSM, FXMN, FYMN, FZMN, FXMT, FYMT, FZMT, FSMT
C-----------------------------------------------
C
      IF(N2D==0)THEN
        NIR=4
      ELSE
        NIR=2
      ENDIF
C
      DO 10 I=1,NMN
      J=MSR(I)
      I3=3*I
      I2=I3-1
      I1=I2-1
      MSMN(I1)=MS(J)
      MSMT(I1)=MS(J)
      FN(I1)=ZERO
      FN(I2)=ZERO
      FN(I3)=ZERO
      FT(I1)=ZERO
      FT(I2)=ZERO
      FT(I3)=ZERO
 10   CONTINUE
C
      DO 60 II=1,NSN
      I=NSV(II)
      J=ILOC(II)
      IF(J<1) GO TO 60
      L=IRTL(II)
      DO 20 JJ=1,NIR
      NN=IRECT(JJ,L)
  20  IY(JJ)=NN
C
      SS=CRST(1,II)
      TT=CRST(2,II)
      N1=NOR(1,II)
      N2=NOR(2,II)
      N3=NOR(3,II)
C
      I3=3*I
      I2=I3-1
      I1=I2-1
C
      AA(1)=A(I1)
      AA(2)=A(I2)
      AA(3)=A(I3)
C
      IF(N2D==0)THEN
        CALL SHAPEH(H,SS,TT)
      ELSE
        H(1) = HALF*(ONE - SS)
        H(2) = HALF*(ONE + SS)
      ENDIF
      DO 50 JJ=1,NIR
      J3=3*IY(JJ)
      J2=J3-1
      J1=J2-1
      JJ3=3*MSR(IY(JJ))
      JJ2=JJ3-1
      JJ1=JJ2-1
C
      XMSS=MS(I)*H(JJ)
C-------------------------------------
C     FORCES TOTALE
C-------------------------------------
      FXI=AA(1)*XMSS
      FYI=AA(2)*XMSS
      FZI=AA(3)*XMSS
      FXMI=A(JJ1)*XMSS
      FYMI=A(JJ1)*XMSS
      FZMI=A(JJ1)*XMSS
C-------------------------------------
C     FORCE NORMALE SECND
C-------------------------------------
      FSN=(FXI*N1+FYI*N2+FZI*N3)
      FXN=FSN*N1
      FYN=FSN*N2
      FZN=FSN*N3
C-------------------------------------
C     FORCE NORMALE main (CORRECTIF)
C-------------------------------------
      FSM = (FXMI*N1+FYMI*N2+FZMI*N3)
      FXMN = FSM*N1
      FYMN = FSM*N2
      FZMN = FSM*N3
C-------------------------------------
C     SOMME DES FORCES NORMALES
C-------------------------------------
      FN(J1) = FN(J1) + FXN - FXMN
      FN(J2) = FN(J2) + FYN - FYMN
      FN(J3) = FN(J3) + FZN - FZMN
      MSMN(J1) = MSMN(J1) + XMSS
C-------------------------------------
C     FORCE FRICTION
C-------------------------------------
      FXT = FXI - FXN
      FYT = FYI - FYN
      FZT = FZI - FZN
      FST = SQRT(FXT*FXT+FYT*FYT+FZT*FZT)
      FAC = MIN(ONE,FRIC*FSN/MAX(EM30,FST))
C-------------------------------------
C     FORCE FRICTION main (CORRECTIF)
C-------------------------------------
      FXMT = FXMI - FXMN
      FYMT = FYMI - FYMN
      FZMT = FZMI - FZMN
C-------------------------------------
C     SOMME DES FORCES DE FRICTION
C-------------------------------------
      FT(J1)=FT(J1) + (FXT - FXMT)*FAC
      FT(J2)=FT(J2) + (FYT - FYMT)*FAC
      FT(J3)=FT(J3) + (FZT - FZMT)*FAC
      MSMT(J1)=MSMT(J1) + XMSS*FAC
C
 50   CONTINUE
 60   CONTINUE
C
      RETURN
      END
