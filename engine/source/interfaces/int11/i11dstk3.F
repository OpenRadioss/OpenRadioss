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
Chd|  I11DSTK3                      source/interfaces/int11/i11dstk3.F
Chd|-- called by -----------
Chd|        I11FKU3                       source/interfaces/int11/i11ke3.F
Chd|        I11KE3                        source/interfaces/int11/i11ke3.F
Chd|        IMP_I11MAINF                  source/interfaces/int11/i11ke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I11DSTK3(
     1   JLT    ,CAND_S,CAND_M,H1S   ,H2S   ,
     2   H1M    ,H2M   ,NX    ,NY    ,NZ    ,
     3   STIF   ,N1    ,N2    ,M1    ,M2    ,
     4   XXS1   ,XXS2  ,XYS1  ,XYS2  ,
     5   XZS1   ,XZS2  ,XXM1  ,XXM2  ,XYM1  ,
     6   XYM2   ,XZM1  ,XZM2  ,VXS1  ,VXS2  ,
     7   VYS1   ,VYS2  ,VZS1  ,VZS2  ,VXM1  ,
     8   VXM2   ,VYM1  ,VYM2  ,VZM1  ,VZM2  ,
     9   MS1    ,MS2   ,MM1   ,MM2   ,GAPV  )
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
      INTEGER JLT,IGAP
      INTEGER CAND_S(*),CAND_M(*),
     .        N1(*),N2(*),M1(*),M2(*)
      my_real
     .     H1S(*),H2S(*),H1M(*),H2M(*),NX(*),NY(*),NZ(*),STIF(*),
     .     XXS1(*) ,XXS2(*) ,XYS1(*) ,XYS2(*) ,
     .     XZS1(*) ,XZS2(*) ,XXM1(*) ,XXM2(*) ,XYM1(*),
     .     XYM2(*) ,XZM1(*) ,XZM2(*) ,VXS1(*) ,VXS2(*),
     .     VYS1(*) ,VYS2(*) ,VZS1(*) ,VZS2(*) ,VXM1(*),
     .     VXM2(*) ,VYM1(*) ,VYM2(*) ,VZM1(*) ,VZM2(*),
     .     MS1(*) ,MS2(*) ,MM1(*) ,MM2(*), GAPV(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .     PENE2(MVSIZ),
     .     XS12,YS12,ZS12,XM12,YM12,ZM12,XA,XB,
     .     XS2,XM2,XSM,XS2M2,YS2,YM2,YSM,YS2M2,ZS2,ZM2,ZSM,ZS2M2,
     .     XX,YY,ZZ,ALS,ALM,DET,
     .     GAP2
C-----------------------------------------------
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
C
C IF B<0 => B=0
C
C       XS2 = [(X1-X2)^2 + (Y1-Y2)^2 + (Z1-Z2)^2]
C       XA = (X1-X2)(X2-X4) + (Y1-Y2)(Y2-Y4) + (Z1-Z2)(Z2-Z4)
C       A = - XA /XS2
C       B = 0
C
C ELSEIF B>1 => B=1
C
C       B = 1
C       XS2 = [(X1-X2)^2 + (Y1-Y2)^2 + (Z1-Z2)^2]
C       XSM = [(X1-X2)(X4-X3) + (Y1-Y2)(Y4-Y3) + (Z1-Z2)(Z4-Z3)]
C       XA = (X1-X2)(X2-X4) + (Y1-Y2)(Y2-Y4) + (Z1-Z2)(Z2-Z4)
C       A = -(XA + XSM)/XS2
C
C IF A<0 => A=0
C
C
C ELSEIF A>1 => A=1
C
C
      DO I=1,JLT
       XS12 = XXS2(I)-XXS1(I)
       YS12 = XYS2(I)-XYS1(I)
       ZS12 = XZS2(I)-XZS1(I)
       XS2  = XS12*XS12 + YS12*YS12 + ZS12*ZS12
       XM12 = XXM2(I)-XXM1(I)
       YM12 = XYM2(I)-XYM1(I)
       ZM12 = XZM2(I)-XZM1(I)
       XM2 = XM12*XM12 + YM12*YM12 + ZM12*ZM12
       XSM = - (XS12*XM12 + YS12*YM12 + ZS12*ZM12)
       XS2M2 = XXM2(I)-XXS2(I)
       YS2M2 = XYM2(I)-XYS2(I)
       ZS2M2 = XZM2(I)-XZS2(I)
C
       XA =  XS12*XS2M2 + YS12*YS2M2 + ZS12*ZS2M2
       XB = -XM12*XS2M2 - YM12*YS2M2 - ZM12*ZS2M2 
       DET = XM2*XS2 - XSM*XSM
       DET = MAX(EM20,DET)
C
       H1S(I) = (XB*XSM-XA*XM2) / DET
       H1M(I) = (XA*XSM-XB*XS2) / DET
C
       XS2 = MAX(XS2,EM20)
       XM2 = MAX(XM2,EM20)
       IF(H1M(I).LT.ZERO)THEN
         H1M(I) = ZERO
         H1S(I) = -XA / XS2
       ELSEIF(H1M(I).GT.ONE)THEN
         H1M(I) = ONE
         H1S(I) = -(XA + XSM) / XS2
       ENDIF
C
       IF(H1S(I).LT.ZERO)THEN
         H1S(I) = ZERO
         H1M(I) = -XB / XM2
       ELSEIF(H1S(I).GT.ONE)THEN
         H1S(I) = ONE
         H1M(I) = -(XB + XSM) / XM2
       ENDIF
C
       H1M(I) = MIN(ONE,H1M(I))
       H1M(I) = MAX(ZERO,H1M(I))
C
       H2S(I) = ONE -H1S(I)
       H2M(I) = ONE -H1M(I)
C !!!!!!!!!!!!!!!!!!!!!!!
C  PENE = GAP^2 - DIST^2 UTILISE POUR TESTER SI NON NUL
C!!!!!!!!!!!!!!!!!!!!!!!!
       NX(I) = H1S(I)*XXS1(I) + H2S(I)*XXS2(I)
     .       - H1M(I)*XXM1(I) - H2M(I)*XXM2(I)
       NY(I) = H1S(I)*XYS1(I) + H2S(I)*XYS2(I)
     .       - H1M(I)*XYM1(I) - H2M(I)*XYM2(I)
       NZ(I) = H1S(I)*XZS1(I) + H2S(I)*XZS2(I)
     .       - H1M(I)*XZM1(I) - H2M(I)*XZM2(I)
C
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  I11DSTR3                      source/interfaces/int11/i11dstk3.F
Chd|-- called by -----------
Chd|        IMP_I11MAINF                  source/interfaces/int11/i11ke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I11DSTR3(
     1                    JLT    ,CAND_S,CAND_M,STIF   ,GAPV   ,
     2                     NX    ,NY    ,NZ    ,JLT_NEW)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "scr05_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,JLT_NEW
      INTEGER CAND_S(*),CAND_M(*)
      my_real
     .     NX(*),NY(*),NZ(*),STIF(*), GAPV(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .     PENE2(MVSIZ),GAP2,FAC
C--------------------------------------------------------
       FAC = ZEP97*ZEP97
Ctmp       FAC = (ONE +HALF)*(ONE +HALF)
      DO I=1,JLT
       GAP2 = FAC*GAPV(I)*GAPV(I)
       PENE2(I) = GAP2 - NX(I)*NX(I) - NY(I)*NY(I) - NZ(I)*NZ(I)
       PENE2(I) = MAX(ZERO,PENE2(I)) 
      ENDDO
#include  "vectorize.inc"
      DO I=1,JLT
        IF(PENE2(I).NE.ZERO.AND.STIF(I).NE.ZERO)THEN
          JLT_NEW = JLT_NEW + 1
          CAND_S(JLT_NEW) = CAND_S(I)
          CAND_M(JLT_NEW) = CAND_M(I)
        ENDIF
      ENDDO
C
      RETURN
      END
