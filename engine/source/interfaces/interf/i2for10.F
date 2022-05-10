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
Chd|  I2FOR10                       source/interfaces/interf/i2for10.F
Chd|-- called by -----------
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2FOR10(A       ,MS      ,STIFN   ,WEIGHT  ,IRECT   ,
     2                   NSV     ,MSR     ,IRTL    ,IRUPT   ,CRST    ,
     3                   FSM     ,NSN     ,CSTS_BIS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NSN
      INTEGER  IRECT(4,*),MSR(*),NSV(*),IRTL(*),IRUPT(*),WEIGHT(*)
C     REAL ou REAL*8
      my_real
     .   A(3,*),MS(*),STIFN(*),FSM(3,*),CRST(2,*),CSTS_BIS(2,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR,I,J,II,JJ,L,W
C     REAL ou REAL*8
      my_real
     .   S,T,SP,SM,TP,TM,FXI,FYI,FZI,XMSI,STFN
      my_real
     .   H(4),H2(4)
C=======================================================================
      NIR=4
C----------------          
      DO II=1,NSN
        I=NSV(II)
        L=IRTL(II)
C
        IF (I > 0) THEN
          W = WEIGHT(I)
C
          S = CRST(1,II)
          T = CRST(2,II)
          SP=UN + S
          SM=UN - S
          TP=QUART*(UN + T)
          TM=QUART*(UN - T)
          H(1)=TM*SM
          H(2)=TM*SP
          H(3)=TP*SP
          H(4)=TP*SM

C        Additional shape functions for distribution of mass / inertia - to avoid negative masses for projection outside of the element
          S = CSTS_BIS(1,II)
          T = CSTS_BIS(2,II)
          SP=UN + S
          SM=UN - S
          TP=QUART*(UN + T)
          TM=QUART*(UN - T)
          H2(1)=TM*SM
          H2(2)=TM*SP
          H2(3)=TP*SP
          H2(4)=TP*SM       
c
          IF (IRUPT(II) == 0) THEN
C---------- pas de rupture                   
c
            XMSI= MS(I)*W
            STFN= STIFN(I)*W
            FXI = A(1,I)*W    
            FYI = A(2,I)*W    
            FZI = A(3,I)*W      
            DO JJ=1,NIR
              J=IRECT(JJ,L)
              A(1,J)  = A(1,J) + H(JJ) * FXI          
              A(2,J)  = A(2,J) + H(JJ) * FYI          
              A(3,J)  = A(3,J) + H(JJ) * FZI          
              MS(J)   = MS(J)  + H2(JJ) * XMSI    
              STIFN(J)= STIFN(J)+H(JJ) * STFN
            ENDDO
            IF(IRODDL == 0)THEN
              STIFN(I)=EM20
              MS(I)   =ZERO
              A(1,I)  =ZERO
              A(2,I)  =ZERO
              A(3,I)  =ZERO
            ENDIF
          ELSEIF (IRUPT(II) == -1) THEN
C---------- rupture partielle  
C
            FXI = FSM(1,II)  
            FYI = FSM(2,II)  
            FZI = FSM(3,II)  
C
            A(1,I) = A(1,I) - FXI
            A(2,I) = A(2,I) - FYI   
            A(3,I) = A(3,I) - FZI   
C
            DO JJ=1,NIR
              J=IRECT(JJ,L)
              A(1,J) = A(1,J) + H(JJ) * FXI *W          
              A(2,J) = A(2,J) + H(JJ) * FYI *W          
              A(3,J) = A(3,J) + H(JJ) * FZI *W         
            ENDDO
          ENDIF
C----------
        ENDIF
      ENDDO
C-----------
      RETURN
      END SUBROUTINE I2FOR10
C
C
Chd|====================================================================
Chd|  I2MOM10                       source/interfaces/interf/i2for10.F
Chd|-- called by -----------
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2MOM10(NSN    ,NMN    ,AR     ,IRECT   ,CRST    , 
     2                   MSR    ,NSV    ,IRTL   ,IN      ,MS      , 
     3                   A      ,X      ,WEIGHT ,STIFR   ,STIFN   ,
     4                   IRUPT  ,ILEV   ,CSTS_BIS) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN, NMN, ILEV,
     .   IRECT(4,*), MSR(*), NSV(*), IRTL(*), WEIGHT(*), IRUPT(*)
C     REAL ou REAL*8
      my_real
     .   A(3,*), AR(3,*),CRST(2,*), MS(*),
     .   X(3,*),IN(*),STIFR(*),STIFN(*),CSTS_BIS(2,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, II, L, JJ, W
C     REAL ou REAL*8
      my_real
     .   H(4),S,T,XMSI,FXI,FYI,FZI,MXI,MYI,MZI,INS,AA,
     .   X0,X1,X2,X3,X4,Y0,Y1,Y2,Y3,Y4,Z0,Z1,Z2,Z3,Z4,
     .   XC0,YC0,ZC0,SP,SM,TP,TM,XC,YC,ZC,
     .   STF,H2(4)
C=======================================================================
      IF (IMACH == 3) THEN
        CONTINUE
      ELSE
#include "vectorize.inc"
        DO II=1,NMN
          I=MSR(II)
          IN(I)=MAX(EM20,IN(I))
        ENDDO
      ENDIF
C
      DO II=1,NSN
        IF (IRUPT(II) == 0) THEN
          I = NSV(II)
          IF (I > 0) THEN
            L = IRTL(II)
C
            S = CRST(1,II)
            T = CRST(2,II)
            SP= UN + S
            SM= UN - S
            TP= QUART*(UN + T)
            TM= QUART*(UN - T)
            H(1)=TM*SM
            H(2)=TM*SP
            H(3)=TP*SP
            H(4)=TP*SM

C        Additional shape functions for distribution of mass / inertia - to avoid negative masses for projection outside of the element
            S = CSTS_BIS(1,II)
            T = CSTS_BIS(2,II)
            SP= UN + S
            SM= UN - S
            TP= QUART*(UN + T)
            TM= QUART*(UN - T)
            H2(1)=TM*SM
            H2(2)=TM*SP
            H2(3)=TP*SP
            H2(4)=TP*SM
C
            X0 = X(1,I)
            Y0 = X(2,I)
            Z0 = X(3,I)
C
            X1 = X(1,IRECT(1,L))
            Y1 = X(2,IRECT(1,L))
            Z1 = X(3,IRECT(1,L))
            X2 = X(1,IRECT(2,L))
            Y2 = X(2,IRECT(2,L))
            Z2 = X(3,IRECT(2,L))
            X3 = X(1,IRECT(3,L))
            Y3 = X(2,IRECT(3,L))
            Z3 = X(3,IRECT(3,L))
            X4 = X(1,IRECT(4,L))
            Y4 = X(2,IRECT(4,L))
            Z4 = X(3,IRECT(4,L))
C
            XC = X1 * H(1) + X2 * H(2) + X3 * H(3) + X4 * H(4)  
            YC = Y1 * H(1) + Y2 * H(2) + Y3 * H(3) + Y4 * H(4)  
            ZC = Z1 * H(1) + Z2 * H(2) + Z3 * H(3) + Z4 * H(4) 
C
            XC0=X0-XC
            YC0=Y0-YC
            ZC0=Z0-ZC
C
            AA = XC0*XC0 + YC0*YC0 + ZC0*ZC0
            INS = IN(I) + AA * MS(I)
            STF = STIFR(I) + AA * STIFN(I)
C
            FXI=A(1,I)
            FYI=A(2,I)
            FZI=A(3,I)
C
            MXI = AR(1,I) + YC0 * FZI - ZC0 * FYI
            MYI = AR(2,I) + ZC0 * FXI - XC0 * FZI
            MZI = AR(3,I) + XC0 * FYI - YC0 * FXI
C
            W = WEIGHT(I)
            DO JJ=1,4                        
              J=IRECT(JJ,L) 
              AR(1,J) =AR(1,J) + MXI*H(JJ)*W    
              AR(2,J) =AR(2,J) + MYI*H(JJ)*W    
              AR(3,J) =AR(3,J) + MZI*H(JJ)*W    
              IN(J)   =IN(J)   + INS*H2(JJ)*W     
              STIFR(J)=STIFR(J)+ STF*H(JJ)*W  
            ENDDO                            
            STIFR(I)=EM20                    
            STIFN(I)=EM20                    
            IN(I)   =ZERO                    
            MS(I)   =ZERO                    
            A(1,I)  =ZERO                    
            A(2,I)  =ZERO                    
            A(3,I)  =ZERO                    
C
          ENDIF
        ENDIF
      ENDDO
C---
      RETURN
      END
