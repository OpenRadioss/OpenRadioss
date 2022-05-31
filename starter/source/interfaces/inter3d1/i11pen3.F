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
Chd|  I11PEN3_VOX1                  source/interfaces/inter3d1/i11pen3.F
Chd|-- called by -----------
Chd|        I11STO_VOX1                   source/interfaces/inter3d1/i11sto.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I11PEN3_VOX1(JLT   ,CAND_S ,CAND_M ,GAPMIN ,DRAD   ,
     .                        MARGE ,GAP_S  ,GAP_M  ,GAP_S_L,GAP_M_L,
     .                        IGAP  ,X      ,IRECTS ,IRECTM ,PENE  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, IGAP
      INTEGER IRECTS(2,*), IRECTM(2,*),CAND_S(*),CAND_M(*)
      my_real
     .     GAPMIN, DRAD, MARGE
      my_real
     .     X(3,*), PENE(MVSIZ), 
     .     GAP_S(*), GAP_M(*), GAP_S_L(*), GAP_M_L(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG,N1,N2,M1,M2
      my_real
     .     XS12,YS12,ZS12,XM12,YM12,ZM12,XA,XB,
     .     XS2,XM2,XSM,XS2M2,YS2,YM2,YSM,YS2M2,ZS2,ZM2,ZSM,ZS2M2,
     .     XX,YY,ZZ,ALS,ALM,DET,
     .     GAP2, GAPV(MVSIZ)
C-----------------------------------------------
C
      IF(IGAP.EQ.0)THEN
        DO I=1,JLT
          GAPV(I)=MAX(DRAD,GAPMIN)+MARGE
        ENDDO
      ELSE
        DO I=1,JLT
          GAPV(I)=GAP_S(CAND_S(I))+GAP_M(CAND_M(I))
          IF(IGAP == 3)  
     .      GAPV(I)=MIN(GAP_S_L(CAND_S(I))+GAP_M_L(CAND_M(I)),GAPV(I))
          GAPV(I)=MAX(DRAD,GAPMIN,GAPV(I))+MARGE
        ENDDO
      ENDIF
C--------------------------------------------------------
C  
C--------------------------------------------------------
C       F = [A*X1+(1-A)*X2-B*X3-(1-B)*X4]^2 + [..Y..]^2 + [..Z..]^2
C       DF/DA = 0 = (X1-X2)(A(X1-X2)+X2-X4 +B(X4-X3))+...
C       DF/DA = 0 = A(X1-X2)^2 +X2-X4 + B(X1-X2)(X4-X3))+...
C       DF/DA = 0 = A[(X1-X2)^2 + (Y1-Y2)^2 + (Z1-Z2)^2] 
C                 + B[(X1-X2)(X4-X3) + (Y1-Y2)(Y4-Y3) + (Z1-Z2)(Z4-Z3)]
C                 +   (X1-X2)(X2-X4) + (Y1-Y2)(Y2-Y4) + (Z1-Z2)(Z2-Z4) 
C       DF/DB = 0 = (X4-X3)(A(X1-X2)+X2-X4 +B(X4-X3))+...
C       DF/DB = 0 = B[(X4-X3)^2 + (Y4-Y3)^2 + (Z4-Z3)^2] 
C                 + A[(X1-X2)(X4-X3) + (Y1-Y2)(Y4-Y3) + (Z1-Z2)(Z4-Z3)]
C                 +   (X4-X3)(X2-X4) + (Y4-Y3)(Y2-Y4) + (Z4-Z3)(Z2-Z4) 
C       XS2 = [(X1-X2)^2 + (Y1-Y2)^2 + (Z1-Z2)^2]
C       XM2 = [(X4-X3)^2 + (Y4-Y3)^2 + (Z4-Z3)^2]
C       XSM = [(X1-X2)(X4-X3) + (Y1-Y2)(Y4-Y3) + (Z1-Z2)(Z4-Z3)]
C       XA = (X1-X2)(X2-X4) + (Y1-Y2)(Y2-Y4) + (Z1-Z2)(Z2-Z4)
C       XB = (X4-X3)(X2-X4) + (Y4-Y3)(Y2-Y4) + (Z4-Z3)(Z2-Z4)
C       A XS2 + B XSM +   XA = 0
C       A XSM + B XM2 +   XB = 0
C
C       A = -(XA + B XSM)/XS2
C       -(XA + B XSM)*XSM + B XM2*XS2 +   XB*XS2 = 0
C       -B XSM*XSM + B XM2*XS2 +   XB*XS2-XA*XSM  = 0
C       B*(XM2*XS2 - XSM*XSM) = -XB*XS2+XA*XSM  
C       B = (XA*XSM-XB*XS2) / (XM2*XS2 - XSM*XSM)
C       A = (XB*XSM-XA*XM2) / (XM2*XS2 - XSM*XSM)
      DO I=1,JLT
       N1=IRECTS(1,CAND_S(I))
       N2=IRECTS(2,CAND_S(I))
       M1=IRECTM(1,CAND_M(I))
       M2=IRECTM(2,CAND_M(I))
       XS12 = X(1,N2)-X(1,N1)
       YS12 = X(2,N2)-X(2,N1)
       ZS12 = X(3,N2)-X(3,N1)
       XS2 = XS12*XS12 + YS12*YS12 + ZS12*ZS12
       XM12 = X(1,M2)-X(1,M1)
       YM12 = X(2,M2)-X(2,M1)
       ZM12 = X(3,M2)-X(3,M1)
       XM2 = XM12*XM12 + YM12*YM12 + ZM12*ZM12
       XSM = - (XS12*XM12 + YS12*YM12 + ZS12*ZM12)
       XS2M2 = X(1,M2)-X(1,N2)
       YS2M2 = X(2,M2)-X(2,N2)
       ZS2M2 = X(3,M2)-X(3,N2)
       XA =  XS12*XS2M2 + YS12*YS2M2 + ZS12*ZS2M2
       XB = -XM12*XS2M2 - YM12*YS2M2 - ZM12*ZS2M2 
       DET = XM2*XS2 - XSM*XSM
       DET = MAX(EM20,DET)
C
       ALM = (XA*XSM-XB*XS2) / DET
       XS2 = MAX(XS2,EM20)
       XM2 = MAX(XM2,EM20)
       ALM=MIN(ONE,MAX(ZERO,ALM))
       ALS = -(XA + ALM*XSM) / XS2
       ALS=MIN(ONE,MAX(ZERO,ALS))
       ALM = -(XB + ALS*XSM) / XM2
       ALM=MIN(ONE,MAX(ZERO,ALM))

C !!!!!!!!!!!!!!!!!!!!!!!
C  PENE = GAP^2 - DIST^2 UTILISE POUR TESTER SI NON NUL
C!!!!!!!!!!!!!!!!!!!!!!!!
       XX =  ALS*X(1,N1) + (1.-ALS)*X(1,N2)
     .      - ALM*X(1,M1) - (1.-ALM)*X(1,M2)
       YY =  ALS*X(2,N1) + (1.-ALS)*X(2,N2)
     .      - ALM*X(2,M1) - (1.-ALM)*X(2,M2)
       ZZ =  ALS*X(3,N1) + (1.-ALS)*X(3,N2)
     .      - ALM*X(3,M1) - (1.-ALM)*X(3,M2)
       GAP2=GAPV(I)*GAPV(I)
       PENE(I) = MAX(ZERO,GAP2- XX*XX - YY*YY - ZZ*ZZ)
C
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  I11PEN3                       source/interfaces/inter3d1/i11pen3.F
Chd|-- called by -----------
Chd|        I11STO                        source/interfaces/inter3d1/i11sto.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I11PEN3(JLT   ,CAND_N,CAND_E,GAP   ,X,
     .                  IRECTS  ,IRECTM ,PENE  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT
      INTEGER IRECTS(2,*), IRECTM(2,*),CAND_N(*),CAND_E(*)
      my_real
     .     GAP
      my_real
     .     X(3,*), PENE(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG,N1,N2,M1,M2
      my_real
     .     XS12,YS12,ZS12,XM12,YM12,ZM12,XA,XB,
     .     XS2,XM2,XSM,XS2M2,YS2,YM2,YSM,YS2M2,ZS2,ZM2,ZSM,ZS2M2,
     .     XX,YY,ZZ,ALS,ALM,DET,
     .     GAP2
C-----------------------------------------------
       GAP2=GAP*GAP
C
C--------------------------------------------------------
C  
C--------------------------------------------------------
C       F = [A*X1+(1-A)*X2-B*X3-(1-B)*X4]^2 + [..Y..]^2 + [..Z..]^2
C       DF/DA = 0 = (X1-X2)(A(X1-X2)+X2-X4 +B(X4-X3))+...
C       DF/DA = 0 = A(X1-X2)^2 +X2-X4 + B(X1-X2)(X4-X3))+...
C       DF/DA = 0 = A[(X1-X2)^2 + (Y1-Y2)^2 + (Z1-Z2)^2] 
C                 + B[(X1-X2)(X4-X3) + (Y1-Y2)(Y4-Y3) + (Z1-Z2)(Z4-Z3)]
C                 +   (X1-X2)(X2-X4) + (Y1-Y2)(Y2-Y4) + (Z1-Z2)(Z2-Z4) 
C       DF/DB = 0 = (X4-X3)(A(X1-X2)+X2-X4 +B(X4-X3))+...
C       DF/DB = 0 = B[(X4-X3)^2 + (Y4-Y3)^2 + (Z4-Z3)^2] 
C                 + A[(X1-X2)(X4-X3) + (Y1-Y2)(Y4-Y3) + (Z1-Z2)(Z4-Z3)]
C                 +   (X4-X3)(X2-X4) + (Y4-Y3)(Y2-Y4) + (Z4-Z3)(Z2-Z4) 
C       XS2 = [(X1-X2)^2 + (Y1-Y2)^2 + (Z1-Z2)^2]
C       XM2 = [(X4-X3)^2 + (Y4-Y3)^2 + (Z4-Z3)^2]
C       XSM = [(X1-X2)(X4-X3) + (Y1-Y2)(Y4-Y3) + (Z1-Z2)(Z4-Z3)]
C       XA = (X1-X2)(X2-X4) + (Y1-Y2)(Y2-Y4) + (Z1-Z2)(Z2-Z4)
C       XB = (X4-X3)(X2-X4) + (Y4-Y3)(Y2-Y4) + (Z4-Z3)(Z2-Z4)
C       A XS2 + B XSM +   XA = 0
C       A XSM + B XM2 +   XB = 0
C
C       A = -(XA + B XSM)/XS2
C       -(XA + B XSM)*XSM + B XM2*XS2 +   XB*XS2 = 0
C       -B XSM*XSM + B XM2*XS2 +   XB*XS2-XA*XSM  = 0
C       B*(XM2*XS2 - XSM*XSM) = -XB*XS2+XA*XSM  
C       B = (XA*XSM-XB*XS2) / (XM2*XS2 - XSM*XSM)
C       A = (XB*XSM-XA*XM2) / (XM2*XS2 - XSM*XSM)
      DO I=1,JLT
       N1=IRECTS(1,CAND_N(I))
       N2=IRECTS(2,CAND_N(I))
       M1=IRECTM(1,CAND_E(I))
       M2=IRECTM(2,CAND_E(I))
       XS12 = X(1,N2)-X(1,N1)
       YS12 = X(2,N2)-X(2,N1)
       ZS12 = X(3,N2)-X(3,N1)
       XS2 = XS12*XS12 + YS12*YS12 + ZS12*ZS12
       XM12 = X(1,M2)-X(1,M1)
       YM12 = X(2,M2)-X(2,M1)
       ZM12 = X(3,M2)-X(3,M1)
       XM2 = XM12*XM12 + YM12*YM12 + ZM12*ZM12
       XSM = - (XS12*XM12 + YS12*YM12 + ZS12*ZM12)
       XS2M2 = X(1,M2)-X(1,N2)
       YS2M2 = X(2,M2)-X(2,N2)
       ZS2M2 = X(3,M2)-X(3,N2)
       XA =  XS12*XS2M2 + YS12*YS2M2 + ZS12*ZS2M2
       XB = -XM12*XS2M2 - YM12*YS2M2 - ZM12*ZS2M2 
       DET = XM2*XS2 - XSM*XSM
       DET = MAX(EM20,DET)
C
       ALM = (XA*XSM-XB*XS2) / DET
       XS2 = MAX(XS2,EM20)
       XM2 = MAX(XM2,EM20)
       ALM=MIN(ONE,MAX(ZERO,ALM))
       ALS = -(XA + ALM*XSM) / XS2
       ALS=MIN(ONE,MAX(ZERO,ALS))
       ALM = -(XB + ALS*XSM) / XM2
       ALM=MIN(ONE,MAX(ZERO,ALM))

C !!!!!!!!!!!!!!!!!!!!!!!
C  PENE = GAP^2 - DIST^2 UTILISE POUR TESTER SI NON NUL
C!!!!!!!!!!!!!!!!!!!!!!!!
       XX =  ALS*X(1,N1) + (1.-ALS)*X(1,N2)
     .      - ALM*X(1,M1) - (1.-ALM)*X(1,M2)
       YY =  ALS*X(2,N1) + (1.-ALS)*X(2,N2)
     .      - ALM*X(2,M1) - (1.-ALM)*X(2,M2)
       ZZ =  ALS*X(3,N1) + (1.-ALS)*X(3,N2)
     .      - ALM*X(3,M1) - (1.-ALM)*X(3,M2)
       PENE(I) = MAX(ZERO,GAP2- XX*XX - YY*YY - ZZ*ZZ)
C
      ENDDO
C
      RETURN
      END
