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
Chd|  CDKDERIC3                     source/elements/sh3n/coquedk/cdkderi3.F
Chd|-- called by -----------
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CDKDERIC3(JFT ,JLT, X2,Y2,X3,Y3,AREA2,ALPE,ALDT,
     1                    PX2,PY2,PX3,PY3,PX,PY,PXY,PYY,VOL,VOLG,
     2                    NU,THK2)
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
      INTEGER JFT, JLT
      my_real
     .     X2(*), Y2(*), X3(*), Y3(*),AREA2(*), 
     .     PX2(*), PY2(*), PX3(*), PY3(*), 
     .     PX(MVSIZ,3), PY(MVSIZ,3), PXY(MVSIZ,3), PYY(MVSIZ,3), 
     .     ALDT(*),ALPE(*),VOL(*),VOLG(*),NU(*),THK2(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL ou REAL*8
      my_real
     .     AREAI(MVSIZ),
     .     AL1, AL2, AL3, ALMAX,ALMIN,FAC
      MY_REAL
     .   X32(MVSIZ), Y32(MVSIZ),AL4, AL5, AL6      
C-----------------------------------------------
      DO I=JFT,JLT
       AREAI(I)=ONE/AREA2(I)
       PX2(I)=Y3(I)*AREAI(I)
       PY2(I)=-X3(I)*AREAI(I)
       PX3(I)=-Y2(I)*AREAI(I)
       PY3(I)=X2(I)*AREAI(I)
Cqz       PX1(I)=-PX3(I)-PX2(I)
Cqz       PY1(I)=-PY2(I)-PY3(I)
       X32(I) =X3(I)-X2(I)
       Y32(I) =Y3(I)-Y2(I)
      ENDDO
C
      DO I=JFT,JLT
       AL1 = X32(I)*X32(I) + Y32(I)*Y32(I)       
       AL2 = X3(I) * X3(I) + Y3(I) * Y3(I)
       AL3 = X2(I) * X2(I) + Y2(I) * Y2(I)
       ALMAX = MAX(AL1,AL2,AL3)
       ALMIN = MIN(AL1,AL2,AL3)
       FAC =1.0+0.6*(1+NU(I))*THK2(I)/ALMIN
       ALMAX = ALMAX*FAC
       ALDT(I)= AREA2(I) / SQRT(ALMAX)
       ALPE(I)=ONE
       AL4 = THREE/MAX(AL1,EM20)
       AL5 = THREE/MAX(AL2,EM20)
       AL6 = THREE/MAX(AL3,EM20)
       PXY(I,1)=X32(I)*Y32(I)*AL4
       PXY(I,2)=X3(I)*Y3(I)*AL5
       PXY(I,3)=X2(I)*Y2(I)*AL6
       PYY(I,1)=Y32(I)*Y32(I)*AL4
       PYY(I,2)=Y3(I)*Y3(I)*AL5
       PYY(I,3)=Y2(I)*Y2(I)*AL6
       AL4 = TWO*AL4
       AL5 = TWO*AL5
       AL6 = TWO*AL6       
       PX(I,1)=X32(I)*AL4
       PX(I,2)=-X3(I)*AL5
       PX(I,3)=X2(I)*AL6
       PY(I,1)=Y32(I)*AL4
       PY(I,2)=-Y3(I)*AL5
       PY(I,3)=Y2(I)*AL6
       VOL(I) = THIRD*VOLG(I)
      ENDDO
C---------------------------------------------------------
      RETURN
C
      END
Chd|====================================================================
Chd|  CDKDERI3                      source/elements/sh3n/coquedk/cdkderi3.F
Chd|-- called by -----------
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CDKDERI3(JFT ,JLT,PX2,PY2,PX3,PY3,PX,PY,PXY,PYY,  
     1                    BZ1,BZ2,BZ3,BRX1,BRX2,BRX3,
     2                    BRY1,BRY2,BRY3,KSI,ETA)
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
      INTEGER JFT, JLT
      my_real
     .     PX2(*), PY2(*), PX3(*), PY3(*), 
     .     PX(MVSIZ,3), PY(MVSIZ,3), PXY(MVSIZ,3), PYY(MVSIZ,3),
     .     BZ1(MVSIZ,2),BZ2(MVSIZ,2),BZ3(MVSIZ,2), BRX1(MVSIZ,3),
     .     BRX2(MVSIZ,3),BRX3(MVSIZ,3),BRY1(MVSIZ,3),BRY2(MVSIZ,3),BRY3(MVSIZ,3),
     .     KSI, ETA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL ou REAL*8
      MY_REAL
     .   XR1,XR2,XR3,YR1,YR2,YR3,XS1,XS2,XS3,YS1,YS2,YS3,
     .   X2Y,Y2X,X6,Y6,XY6,RXY1(3),RXY2(3)
C-----------------------------------------------
      X2Y =ONE-TWO*KSI-ETA
      Y2X =ONE-TWO*ETA-KSI
      X6  = SIX*KSI-ONE
      Y6  = SIX*ETA-ONE
      XY6 = X6+Y6-THREE      
      DO I=JFT,JLT
       XR1= PX(I,3)*X2Y
       XR2= PX(I,2)*ETA
       XR3= PX(I,1)*ETA
       XS1= -PX(I,2)*Y2X
       XS2= -PX(I,3)*KSI
       XS3= -PX(I,1)*KSI
       BZ1(I,1)=PX2(I)*(-XR1+XR3)-PX3(I)*(XS3+XS2)
       BZ1(I,2)=-PX2(I)*(XR2+XR3)+PX3(I)*(-XS1+XS3)
C       BZ1(1,I)=-BZ1(2,I)-BZ1(3,I)
C
       YR1= PY(I,3)*X2Y
       YR2= PY(I,2)*ETA
       YR3= PY(I,1)*ETA
       YS1= -PY(I,2)*Y2X
       YS2= -PY(I,3)*KSI
       YS3= -PY(I,1)*KSI
       BZ2(I,1)=PY2(I)*(-YR1+YR3)-PY3(I)*(YS3+YS2)
       BZ2(I,2)=-PY2(I)*(YR2+YR3)+PY3(I)*(-YS1+YS3)
C       BZ2(1,I)=-BZ2(2,I)-BZ2(3,I)
C
       BZ3(I,1)=PX2(I)*(-YR1+YR3)-PX3(I)*(YS3+YS2)+
     .           PY2(I)*(-XR1+XR3)-PY3(I)*(XS3+XS2)
       BZ3(I,2)=-PX2(I)*(YR2+YR3)+PX3(I)*(-YS1+YS3)
     .           -PY2(I)*(XR2+XR3)+PY3(I)*(-XS1+XS3)
C       BZ3(1,I)=-BZ3(2,I)-BZ3(3,I)
      ENDDO
C
      DO I=JFT,JLT
       XR1= PXY(I,3)*X2Y
       XR2= -PXY(I,2)*ETA
       XR3= PXY(I,1)*ETA
       XS1= PXY(I,2)*Y2X
       XS2= -PXY(I,3)*KSI
       XS3= PXY(I,1)*KSI
       BRX1(I,1)=PX2(I)*(XR1+XR2)+PX3(I)*(XS1+XS2)
       BRX1(I,2)=PX2(I)*(XR1+XR3)+PX3(I)*(XS3+XS2)
       BRX1(I,3)=PX2(I)*(XR2+XR3)+PX3(I)*(XS1+XS3)
C
       YR1= PYY(I,3)*X2Y
       YR2= ONE-PYY(I,2)*ETA
       YR3= PYY(I,1)*ETA-ONE
       YS1= PYY(I,2)*Y2X
       YS2= ONE-PYY(I,3)*KSI
       YS3= PYY(I,1)*KSI-ONE
       BRX2(I,1)=PY2(I)*(YR1+YR2)+PY3(I)*(YS1+YS2)
       BRX2(I,2)=PY2(I)*(YR1+YR3)+PY3(I)*(YS3+YS2)
       BRX2(I,3)=PY2(I)*(YR2+YR3)+PY3(I)*(YS1+YS3)
C
       RXY1(1)=PX2(I)*(YR1+YR2)+PX3(I)*(YS1+YS2)
       RXY2(1)=PY2(I)*(XR1+XR2)+PY3(I)*(XS1+XS2)
       RXY1(2)=PX2(I)*(YR1+YR3)+PX3(I)*(YS3+YS2)
       RXY2(2)=PY2(I)*(XR1+XR3)+PY3(I)*(XS3+XS2)
       RXY1(3)=PX2(I)*(YR2+YR3)+PX3(I)*(YS1+YS3)
       RXY2(3)=PY2(I)*(XR2+XR3)+PY3(I)*(XS1+XS3)
       BRX3(I,1)=RXY1(1)+RXY2(1)
       BRX3(I,2)=RXY1(2)+RXY2(2)
       BRX3(I,3)=RXY1(3)+RXY2(3)
C
       BRY1(I,1)=RXY1(1)+(PX2(I)+PX3(I))*XY6
       BRY1(I,2)=RXY1(2)+PX2(I)*X6
       BRY1(I,3)=RXY1(3)+PX3(I)*Y6
C
       BRY2(I,1)=-RXY2(1)
       BRY2(I,2)=-RXY2(2)
       BRY2(I,3)=-RXY2(3)
C
       BRY3(I,1)=(PY2(I)+PY3(I))*XY6+BRX2(I,1)-BRX1(I,1)
       BRY3(I,2)=PY2(I)*X6+BRX2(I,2)-BRX1(I,2)
       BRY3(I,3)=PY3(I)*Y6+BRX2(I,3)-BRX1(I,3)
      ENDDO
C
C---------------------------------------------------------
      RETURN
C
      END
