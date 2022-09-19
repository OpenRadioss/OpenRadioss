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
Chd|  I2FOR10P                      source/interfaces/interf/i2for10p.F
Chd|-- called by -----------
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2FOR10P(A     ,MS    ,STIFN ,WEIGHT,IADI2 ,
     2                    FSKYI2,NSV   ,IRUPT ,CRST  ,FSM   ,
     3                    NIR   ,NSN   ,I0    ,I2SIZE, CSTS_BIS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,I0,NIR,I2SIZE
      INTEGER NSV(*),IRUPT(*),WEIGHT(*),IADI2(NIR,*)
C     REAL
      my_real
     .   MS(*),STIFN(*),A(3,*),FSM(3,*),CRST(2,*),FSKYI2(I2SIZE,*),CSTS_BIS(2,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, NN
C     REAL
      my_real
     .   SS,TT,XMSI,STFN,FXI,FYI,FZI,SP,SM,TP,TM,H1, H2, H3, H4,H21, H22, H23, H24
C=======================================================================
#include "vectorize.inc"
      DO II=1,NSN
        I=NSV(II)
        IF (I > 0) THEN
C
          SS=CRST(1,II)
          TT=CRST(2,II)
          SP=ONE + SS
          SM=ONE - SS
          TP=FOURTH*(ONE + TT)
          TM=FOURTH*(ONE - TT)
          H1=TM*SM
          H2=TM*SP
          H3=TP*SP
          H4=TP*SM

C        Additional shape functions for distribution of mass / inertia - to avoid negative masses for projection outside of the element
          SS=CSTS_BIS(1,II)
          TT=CSTS_BIS(2,II)
          SP=ONE + SS
          SM=ONE - SS
          TP=FOURTH*(ONE + TT)
          TM=FOURTH*(ONE - TT)
          H21=TM*SM
          H22=TM*SP
          H23=TP*SP
          H24=TP*SM
C
          FXI=A(1,I)
          FYI=A(2,I)
          FZI=A(3,I)
C
          IF (IRUPT(II) == 0) THEN
C---------  pas de rupture                      - 
C
            IF (WEIGHT(I) == 1) THEN
              XMSI = MS(I)
              STFN = STIFN(I)
C
              I0 = I0 + 1
              NN = IADI2(1,I0)
              FSKYI2(1,NN) = FXI*H1
              FSKYI2(2,NN) = FYI*H1
              FSKYI2(3,NN) = FZI*H1
              FSKYI2(4,NN) = XMSI*H21
              FSKYI2(5,NN) = STFN*H1
C
              NN = IADI2(2,I0)
              FSKYI2(1,NN) = FXI*H2
              FSKYI2(2,NN) = FYI*H2
              FSKYI2(3,NN) = FZI*H2
              FSKYI2(4,NN) = XMSI*H22
              FSKYI2(5,NN) = STFN*H2
C
              NN = IADI2(3,I0)
              FSKYI2(1,NN) = FXI*H3
              FSKYI2(2,NN) = FYI*H3
              FSKYI2(3,NN) = FZI*H3
              FSKYI2(4,NN) = XMSI*H23
              FSKYI2(5,NN) = STFN*H3
C
              NN = IADI2(4,I0)
              FSKYI2(1,NN) = FXI*H4
              FSKYI2(2,NN) = FYI*H4
              FSKYI2(3,NN) = FZI*H4
              FSKYI2(4,NN) = XMSI*H24
              FSKYI2(5,NN) = STFN*H4
            ENDIF
C
            IF (IRODDL == 0)THEN
              STIFN(I)=EM20
              MS(I) =ZERO
              A(1,I)=ZERO
              A(2,I)=ZERO
              A(3,I)=ZERO
            ENDIF
          ELSEIF (IRUPT(II) == -1) THEN
C---------  rupture partielle                     - 
C
            FXI = FSM(1,II)
            FYI = FSM(2,II)
            FZI = FSM(3,II)
C
            A(1,I) = A(1,I) - FXI
            A(2,I) = A(2,I) - FYI
            A(3,I) = A(3,I) - FZI
C
            IF (WEIGHT(I) == 1) THEN
              I0 = I0 + 1
              NN = IADI2(1,I0)
              FSKYI2(1,NN) = FXI*H1
              FSKYI2(2,NN) = FYI*H1
              FSKYI2(3,NN) = FZI*H1
              FSKYI2(4,NN) = ZERO
              FSKYI2(5,NN) = ZERO
C
              NN = IADI2(2,I0)
              FSKYI2(1,NN) = FXI*H2
              FSKYI2(2,NN) = FYI*H2
              FSKYI2(3,NN) = FZI*H2
              FSKYI2(4,NN) = ZERO
              FSKYI2(5,NN) = ZERO
C
              NN = IADI2(3,I0)
              FSKYI2(1,NN) = FXI*H3
              FSKYI2(2,NN) = FYI*H3
              FSKYI2(3,NN) = FZI*H3
              FSKYI2(4,NN) = ZERO
              FSKYI2(5,NN) = ZERO
C
              NN = IADI2(4,I0)
              FSKYI2(1,NN) = FXI*H4
              FSKYI2(2,NN) = FYI*H4
              FSKYI2(3,NN) = FZI*H4
              FSKYI2(4,NN) = ZERO
              FSKYI2(5,NN) = ZERO
            ENDIF
          ELSEIF (IRUPT(II) == 1 .AND. WEIGHT(I) == 1) THEN
C---------  rupture totale                     - 
C
            I0 = I0 + 1
            NN = IADI2(1,I0)
            FSKYI2(1,NN) = ZERO
            FSKYI2(2,NN) = ZERO
            FSKYI2(3,NN) = ZERO
            FSKYI2(4,NN) = ZERO
            FSKYI2(5,NN) = ZERO
C
            NN = IADI2(2,I0)
            FSKYI2(1,NN) = ZERO
            FSKYI2(2,NN) = ZERO
            FSKYI2(3,NN) = ZERO
            FSKYI2(4,NN) = ZERO
            FSKYI2(5,NN) = ZERO
C
            NN = IADI2(3,I0)
            FSKYI2(1,NN) = ZERO
            FSKYI2(2,NN) = ZERO
            FSKYI2(3,NN) = ZERO
            FSKYI2(4,NN) = ZERO
            FSKYI2(5,NN) = ZERO
C
            NN = IADI2(4,I0)
            FSKYI2(1,NN) = ZERO
            FSKYI2(2,NN) = ZERO
            FSKYI2(3,NN) = ZERO
            FSKYI2(4,NN) = ZERO
            FSKYI2(5,NN) = ZERO
          ENDIF
C
        ENDIF
      ENDDO
C-----------
      RETURN
      END SUBROUTINE I2FOR10P
C      
Chd|====================================================================
Chd|  I2MOM10P                      source/interfaces/interf/i2for10p.F
Chd|-- called by -----------
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2MOM10P(
     1           X         ,A         ,AR        ,MS        ,IN        ,
     2           STIFN     ,STIFR     ,WEIGHT    ,IRECT     ,NSV       ,
     3           MSR       ,IRTL      ,IRUPT     ,CRST      ,IADI2     ,
     4           FSKYI2    ,NSN       ,NMN       ,NIR       ,I0        ,
     5           I2SIZE    ,IDEL2     ,CSTS_BIS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NSN, NMN, NIR, I0, I2SIZE, IDEL2
      INTEGER  IRECT(4,*), MSR(*), NSV(*), IRTL(*), WEIGHT(*),
     .         IADI2(NIR,*),IRUPT(*)
C     REAL
      my_real
     .        A(3,*), AR(3,*),CRST(2,*), MS(*),
     .        X(3,*),IN(*),STIFN(*),STIFR(*), FSKYI2(I2SIZE,*), CSTS_BIS(2,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, II, L, NN
C     REAL
      my_real
     .   SS, TT, XMSI, FXI, FYI, FZI, MXI, MYI, MZI,INS,
     .   X0,X1,X2,X3,X4,Y0,Y1,Y2,Y3,Y4,Z0,Z1,Z2,Z3,Z4,AA,
     .   XC0,YC0,ZC0,SP,SM,TP,TM,XC,YC,ZC,H1, H2,H3,H4,
     .   STF,H21,H22,H23,H24
C-----------------------------------------------
#include "vectorize.inc"
      DO II=1,NMN
        J=MSR(II)
        IN(J)=MAX(EM20,IN(J))
      ENDDO
C
#include "vectorize.inc"
      DO II=1,NSN
        I=NSV(II)
        IF (I > 0) THEN
C
          IF (IRUPT(II) == 0) THEN
C---------  pas de rupture                      - 
C
            IF (WEIGHT(I) == 1) THEN
              L=IRTL(II)
C
              SS=CRST(1,II)
              TT=CRST(2,II)
              SP=ONE + SS
              SM=ONE - SS
              TP=FOURTH*(ONE + TT)
              TM=FOURTH*(ONE - TT)
              H1=TM*SM
              H2=TM*SP
              H3=TP*SP
              H4=TP*SM

C        Additional shape functions for distribution of mass / inertia - to avoid negative masses for projection outside of the element
              SS=CSTS_BIS(1,II)
              TT=CSTS_BIS(2,II)
              SP=ONE + SS
              SM=ONE - SS
              TP=FOURTH*(ONE + TT)
              TM=FOURTH*(ONE - TT)
              H21=TM*SM
              H22=TM*SP
              H23=TP*SP
              H24=TP*SM
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
              XC = X1 * H1 + X2 * H2 + X3 * H3 + X4 * H4
              YC = Y1 * H1 + Y2 * H2 + Y3 * H3 + Y4 * H4
              ZC = Z1 * H1 + Z2 * H2 + Z3 * H3 + Z4 * H4
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
              I0 = I0 + 1
              NN = IADI2(1,I0)
              FSKYI2(6,NN) = MXI*H1
              FSKYI2(7,NN) = MYI*H1
              FSKYI2(8,NN) = MZI*H1
              FSKYI2(9,NN) = INS*H21
              FSKYI2(10,NN)= STF*H1
C
              NN = IADI2(2,I0)
              FSKYI2(6,NN) = MXI*H2
              FSKYI2(7,NN) = MYI*H2
              FSKYI2(8,NN) = MZI*H2
              FSKYI2(9,NN) = INS*H22
              FSKYI2(10,NN)= STF*H2
C
              NN = IADI2(3,I0)
              FSKYI2(6,NN) = MXI*H3
              FSKYI2(7,NN) = MYI*H3
              FSKYI2(8,NN) = MZI*H3
              FSKYI2(9,NN) = INS*H23
              FSKYI2(10,NN)= STF*H3
C
              NN = IADI2(4,I0)
              FSKYI2(6,NN) = MXI*H4
              FSKYI2(7,NN) = MYI*H4
              FSKYI2(8,NN) = MZI*H4
              FSKYI2(9,NN) = INS*H24
              FSKYI2(10,NN)= STF*H4
            ENDIF
C
            STIFN(I)=EM20
            STIFR(I)=EM20
            IN(I) =ZERO
            MS(I) =ZERO
            A(1,I)=ZERO
            A(2,I)=ZERO
            A(3,I)=ZERO
C
          ELSEIF (WEIGHT(I) == 1) THEN
C---------  rupture partielle ou totale                      - 
C
            I0 = I0 + 1
            NN = IADI2(1,I0)
            FSKYI2(6,NN) = ZERO
            FSKYI2(7,NN) = ZERO
            FSKYI2(8,NN) = ZERO
            FSKYI2(9,NN) = ZERO
            FSKYI2(10,NN)= ZERO
C
            NN = IADI2(2,I0)
            FSKYI2(6,NN) = ZERO
            FSKYI2(7,NN) = ZERO
            FSKYI2(8,NN) = ZERO
            FSKYI2(9,NN) = ZERO
            FSKYI2(10,NN)= ZERO
C
            NN = IADI2(3,I0)
            FSKYI2(6,NN) = ZERO
            FSKYI2(7,NN) = ZERO
            FSKYI2(8,NN) = ZERO
            FSKYI2(9,NN) = ZERO
            FSKYI2(10,NN)= ZERO
C
            NN = IADI2(4,I0)
            FSKYI2(6,NN) = ZERO
            FSKYI2(7,NN) = ZERO
            FSKYI2(8,NN) = ZERO
            FSKYI2(9,NN) = ZERO
            FSKYI2(10,NN)= ZERO
          ENDIF
C
        ENDIF
      ENDDO
C-----------
      RETURN
      END SUBROUTINE I2MOM10P
