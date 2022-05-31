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
Chd|  I11DST3                       source/interfaces/inter3d1/i11dst3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I11DST3(JLT   ,GAP   ,CAND_S,CAND_M,IRECTS  ,
     .                  IRECTM ,NX,NY,NZ,
     .                  N1,N2,M1,M2,JLT_NEW,
     .                  X ,IGAP,GAP_S,GAP_M, GAPV2,
     .                  GAP_S_L,GAP_M_L,DRAD)
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
      INTEGER JLT,JLT_NEW,IGAP
      INTEGER IRECTS(2,*), IRECTM(2,*),CAND_S(*),CAND_M(*),
     . N1(*),N2(*),M1(*),M2(*)
      my_real
     .     GAP,DRAD,
     . NX(*),NY(*),NZ(*),X(3,*),GAP_S(*),GAP_M(*), GAPV2(*),
     . GAP_S_L(*),GAP_M_L(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .     XS12,YS12,ZS12,XM12,YM12,ZM12,XA,XB,
     .     XS2,XM2,XSM,XS2M2,YS2,YM2,YSM,YS2M2,ZS2,ZM2,ZSM,ZS2M2,
     .     XX,YY,ZZ,ALS,ALM,DET,H1S,H2S,H1M,H2M,
     .     GAP2,DRAD2,
     .     PENE2(MVSIZ)
C-----------------------------------------------
       GAP2=GAP*GAP
       DRAD2=DRAD*DRAD
C
      IF(IGAP.EQ.0)THEN
        DO I=1,JLT
          GAPV2(I)=GAP2
        ENDDO
      ELSE
        DO I=1,JLT
          GAPV2(I)=GAP_S(CAND_S(I))+GAP_M(CAND_M(I))
         IF(IGAP == 3)  
     .     GAPV2(I)=MIN(GAP_S_L(CAND_S(I))+GAP_M_L(CAND_M(I)),GAPV2(I))
          GAPV2(I)=MAX(GAPV2(I)*GAPV2(I),GAP2)
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
       N1(I)=IRECTS(1,CAND_S(I))
       N2(I)=IRECTS(2,CAND_S(I))
       M1(I)=IRECTM(1,CAND_M(I))
       M2(I)=IRECTM(2,CAND_M(I))
       XS12 = X(1,N2(I))-X(1,N1(I))
       YS12 = X(2,N2(I))-X(2,N1(I))
       ZS12 = X(3,N2(I))-X(3,N1(I))
       XS2 = XS12*XS12 + YS12*YS12 + ZS12*ZS12
       XM12 = X(1,M2(I))-X(1,M1(I))
       YM12 = X(2,M2(I))-X(2,M1(I))
       ZM12 = X(3,M2(I))-X(3,M1(I))
       XM2 = XM12*XM12 + YM12*YM12 + ZM12*ZM12
       XSM = - (XS12*XM12 + YS12*YM12 + ZS12*ZM12)
       XS2M2 = X(1,M2(I))-X(1,N2(I))
       YS2M2 = X(2,M2(I))-X(2,N2(I))
       ZS2M2 = X(3,M2(I))-X(3,N2(I))
       XA =  XS12*XS2M2 + YS12*YS2M2 + ZS12*ZS2M2
       XB = -XM12*XS2M2 - YM12*YS2M2 - ZM12*ZS2M2 
       DET = XM2*XS2 - XSM*XSM
       DET = MAX(EM20,DET)
C
       H1M = (XA*XSM-XB*XS2) / DET
C
       XS2 = MAX(XS2,EM20)
       XM2 = MAX(XM2,EM20)
       H1M=MIN(ONE,MAX(ZERO,H1M))
       H1S = -(XA + H1M*XSM) / XS2
       H1S=MIN(ONE,MAX(ZERO,H1S))
       H1M = -(XB + H1S*XSM) / XM2
       H1M=MIN(ONE,MAX(ZERO,H1M))
C
       H2S = ONE - H1S
       H2M = ONE - H1M
C !!!!!!!!!!!!!!!!!!!!!!!
C  PENE = GAP^2 - DIST^2 UTILISE POUR TESTER SI NON NUL
C!!!!!!!!!!!!!!!!!!!!!!!!
       NX(I) =  H1S*X(1,N1(I)) + H2S*X(1,N2(I))
     .      - H1M*X(1,M1(I)) - H2M*X(1,M2(I))
       NY(I) =  H1S*X(2,N1(I)) + H2S*X(2,N2(I))
     .      - H1M*X(2,M1(I)) - H2M*X(2,M2(I))
       NZ(I) =  H1S*X(3,N1(I)) + H2S*X(3,N2(I))
     .      - H1M*X(3,M1(I)) - H2M*X(3,M2(I))
       PENE2(I) = MAX(GAPV2(I),DRAD2) - NX(I)*NX(I) - NY(I)*NY(I) - NZ(I)*NZ(I)
       PENE2(I) = MAX(ZERO,PENE2(I)) 
C
      ENDDO
      DO I=1,JLT
        IF(PENE2(I).NE.ZERO)THEN
          JLT_NEW = JLT_NEW + 1
          CAND_S(JLT_NEW) = CAND_S(I)
          CAND_M(JLT_NEW) = CAND_M(I)
          N1(JLT_NEW)  = N1(I)
          N2(JLT_NEW)  = N2(I)
          M1(JLT_NEW)  = M1(I)
          M2(JLT_NEW)  = M2(I)
          NX(JLT_NEW)  = NX(I)
          NY(JLT_NEW)  = NY(I)
          NZ(JLT_NEW)  = NZ(I)
          GAPV2(JLT_NEW)  = GAPV2(I)
        ENDIF
      ENDDO
C
      RETURN
      END
