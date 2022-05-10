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
Chd|  MOD_CLOSE                     source/elements/solid/solide/mod_close.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MOD_CLOSE(GEO,NGEO,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,HH,
     .   XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .   YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .   ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      DOUBLE PRECISION
     .   XD1(*), XD2(*), XD3(*), XD4(*), XD5(*), XD6(*), XD7(*), XD8(*),
     .   YD1(*), YD2(*), YD3(*), YD4(*), YD5(*), YD6(*), YD7(*), YD8(*),
     .   ZD1(*), ZD2(*), ZD3(*), ZD4(*), ZD5(*), ZD6(*), ZD7(*), ZD8(*)     

      my_real
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .   GEO(NPROPG,*),HH(*)
     
      INTEGER NGEO(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,J1,J2,K,IX1,IX2,IX3,IX4,IX5,IX6,IX7,IX8,KMAX
C     REAL ou REAL*8
      my_real
     .  X13,Y13,Z13,X24,Y24,Z24,SX(3),SY(3),SZ(3),SN(3),DH,DHX,DHY,DHZ,
     .  XC1,YC1,ZC1,XC2,YC2,ZC2,X(3,8),H,SNMAX,HTEST(MVSIZ),HCLOS(MVSIZ)
      INTEGER ICF1(4,3),ICF2(4,3)
      DATA ICF1/1,2,3,4, 2,6,7,3, 1,5,6,2/
      DATA ICF2/5,6,7,8, 1,5,8,4, 4,8,7,3/
C
      DO I=LFT,LLT
       HCLOS(I)=GEO(129,NGEO(I))
       HTEST(I)=GEO(130,NGEO(I))
      ENDDO
C      
      DO I=LFT,LLT	
  	X(1,1)=XD1(I)
  	X(2,1)=YD1(I)
  	X(3,1)=ZD1(I)
  	X(1,2)=XD2(I)
  	X(2,2)=YD2(I)
  	X(3,2)=ZD2(I)
  	X(1,3)=XD3(I)
  	X(2,3)=YD3(I)
  	X(3,3)=ZD3(I)
  	X(1,4)=XD4(I)
  	X(2,4)=YD4(I)
  	X(3,4)=ZD4(I)
  	X(1,5)=XD5(I)
  	X(2,5)=YD5(I)
  	X(3,5)=ZD5(I)
  	X(1,6)=XD6(I)
  	X(2,6)=YD6(I)
  	X(3,6)=ZD6(I)
  	X(1,7)=XD7(I)
  	X(2,7)=YD7(I)
  	X(3,7)=ZD7(I)
  	X(1,8)=XD8(I)
  	X(2,8)=YD8(I)
  	X(3,8)=ZD8(I)	
C
        DO K=1,3
           IX1=ICF1(1,K)
           IX2=ICF1(2,K)
           IX3=ICF1(3,K)
           IX4=ICF1(4,K)
           IX5=ICF2(1,K)
           IX6=ICF2(2,K)
           IX7=ICF2(3,K)
           IX8=ICF2(4,K)
           X13=X(1,IX3)-X(1,IX1)+X(1,IX7)-X(1,IX5)
           Y13=X(2,IX3)-X(2,IX1)+X(2,IX7)-X(2,IX5)
           Z13=X(3,IX3)-X(3,IX1)+X(3,IX7)-X(3,IX5)
           X24=X(1,IX4)-X(1,IX2)+X(1,IX8)-X(1,IX6)
           Y24=X(2,IX4)-X(2,IX2)+X(2,IX8)-X(2,IX6)
           Z24=X(3,IX4)-X(3,IX2)+X(3,IX8)-X(3,IX6)
	   SX(K)=Y13*Z24-Z13*Y24
	   SY(K)=Z13*X24-X13*Z24
	   SZ(K)=X13*Y24-Y13*X24
	   SN(K)=SQRT(SX(K)**2+SY(K)**2+SZ(K)**2)
C	   SX(K)=SX(K)/SN(K)
C	   SY(K)=SY(K)/SN(K)
C	   SZ(K)=SZ(K)/SN(K)
        ENDDO
        SNMAX=0
        DO K=1,3
	  IF(SN(K).GT.SNMAX)THEN
	     KMAX=K
	     SNMAX=SN(K)
	   SX(K)=SX(K)/SN(K)
	   SY(K)=SY(K)/SN(K)
	   SZ(K)=SZ(K)/SN(K)
	  ENDIF
	ENDDO
C	
        H=1.E30
        DO J=1,4
	  J2=ICF2(J,KMAX)
	  J1=ICF1(J,KMAX)
          H=MIN(H,
     .      (X(1,J2)-X(1,J1))*SX(KMAX)+
     .      (X(2,J2)-X(2,J1))*SY(KMAX)+
     .      (X(3,J2)-X(3,J1))*SZ(KMAX)   )
        ENDDO
C
        HH(I)=ZERO
C		
	IF(H.LT.HTEST(I))THEN
          IX1=ICF1(1,KMAX)
          IX2=ICF1(2,KMAX)
          IX3=ICF1(3,KMAX)
          IX4=ICF1(4,KMAX)
          IX5=ICF2(1,KMAX)
          IX6=ICF2(2,KMAX)
          IX7=ICF2(3,KMAX)
          IX8=ICF2(4,KMAX)
          DH=UNDEMI*(HTEST(I)-H)
	  DHX=DH*SX(KMAX)
	  DHY=DH*SY(KMAX)
	  DHZ=DH*SZ(KMAX)
	  X(1,IX1)=X(1,IX1)-DHX
	  X(2,IX1)=X(2,IX1)-DHY
	  X(3,IX1)=X(3,IX1)-DHZ
	  X(1,IX2)=X(1,IX2)-DHX
	  X(2,IX2)=X(2,IX2)-DHY
	  X(3,IX2)=X(3,IX2)-DHZ
	  X(1,IX3)=X(1,IX3)-DHX
	  X(2,IX3)=X(2,IX3)-DHY
	  X(3,IX3)=X(3,IX3)-DHZ
	  X(1,IX4)=X(1,IX4)-DHX
	  X(2,IX4)=X(2,IX4)-DHY
	  X(3,IX4)=X(3,IX4)-DHZ
	  X(1,IX5)=X(1,IX5)+DHX
	  X(2,IX5)=X(2,IX5)+DHY
	  X(3,IX5)=X(3,IX5)+DHZ
	  X(1,IX6)=X(1,IX6)+DHX
	  X(2,IX6)=X(2,IX6)+DHY
	  X(3,IX6)=X(3,IX6)+DHZ
	  X(1,IX7)=X(1,IX7)+DHX
	  X(2,IX7)=X(2,IX7)+DHY
	  X(3,IX7)=X(3,IX7)+DHZ
	  X(1,IX8)=X(1,IX8)+DHX
	  X(2,IX8)=X(2,IX8)+DHY
	  X(3,IX8)=X(3,IX8)+DHZ
  	  XD1(I)=X(1,1)
  	  YD1(I)=X(2,1)
  	  ZD1(I)=X(3,1)
  	  XD2(I)=X(1,2)
  	  YD2(I)=X(2,2)
  	  ZD2(I)=X(3,2)
  	  XD3(I)=X(1,3)
  	  YD3(I)=X(2,3)
  	  ZD3(I)=X(3,3)
  	  XD4(I)=X(1,4)
  	  YD4(I)=X(2,4)
  	  ZD4(I)=X(3,4)
  	  XD5(I)=X(1,5)
  	  YD5(I)=X(2,5)
  	  ZD5(I)=X(3,5)
  	  XD6(I)=X(1,6)
  	  YD6(I)=X(2,6)
  	  ZD6(I)=X(3,6)
  	  XD7(I)=X(1,7)
  	  YD7(I)=X(2,7)
  	  ZD7(I)=X(3,7)
  	  XD8(I)=X(1,8)
  	  YD8(I)=X(2,8)
  	  ZD8(I)=X(3,8)	  	  
          HH(I)=Max(UN-H/HCLOS(I),ZERO)
	  HH(I)=MIN(UN,HH(I))
	ENDIF
      ENDDO
      
c copy and cast XD(DP) to X(SP) to assure coherence between XD et X      
      DO I=LFT,LLT
        X1(I)= XD1(I) 
        Y1(I)= YD1(I) 
        Z1(I)= ZD1(I)
        X2(I)= XD2(I)  
        Y2(I)= YD2(I)  
        Z2(I)= ZD2(I) 
        X3(I)= XD3(I) 
        Y3(I)= YD3(I) 
        Z3(I)= ZD3(I) 
        X4(I)= XD4(I) 
        Y4(I)= YD4(I) 
        Z4(I)= ZD4(I) 
        X5(I)= XD5(I) 
        Y5(I)= YD5(I)
        Z5(I)= ZD5(I) 
        X6(I)= XD6(I) 
        Y6(I)= YD6(I) 
        Z6(I)= ZD6(I) 
        X7(I)= XD7(I) 
        Y7(I)= YD7(I) 
        Z7(I)= ZD7(I) 
        X8(I)= XD8(I) 
        Y8(I)= YD8(I) 
        Z8(I)= ZD8(I) 
      ENDDO          
          
      RETURN
      END		
	
