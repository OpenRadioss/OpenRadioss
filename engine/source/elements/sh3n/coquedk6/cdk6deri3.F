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
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
Chd|====================================================================
Chd|  CDK6DERI3                     source/elements/sh3n/coquedk6/cdk6deri3.F
Chd|-- called by -----------
Chd|        CDK6FORC3                     source/elements/sh3n/coquedk6/cdk6forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CDK6DERI3(JFT,JLT,X2,Y2,X3,Y3,AREA2,ALPE,ALDT,NU,THK2,
     1                    PX2,PY2,PX3,PY3,X4,Y4,Z4,X5,Y5,Z5,X6,Y6,Z6,
     2                    N4X,N4Y,N4Z,N5X,N5Y,N5Z,N6X,N6Y,N6Z,
     3                    AREA4,AREA5,AREA6,PB1,PB2,PB3,NVS,IVS,IXTG1)
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
      INTEGER JFT, JLT,NVS,IVS(*),IXTG1(4,*)
      my_real
     .     X2(*), Y2(*), X3(*), Y3(*),AREA2(*), 
     .     X4(*), Y4(*), X5(*), Y5(*),AREA4(*),AREA5(*), 
     .     X6(*), Y6(*), Z4(*), Z5(*),Z6(*),AREA6(*), 
     .     N4X(*),N4Y(*),N4Z(*),N5X(*),N5Y(*),N5Z(*),N6X(*),N6Y(*),
     .     N6Z(*),PX2(*), PY2(*), PX3(*), PY3(*), NU(*),THK2(*),
     .     ALDT(*),ALPE(*),PB1(MVSIZ,3,3),PB2(MVSIZ,3,3),PB3(MVSIZ,3,6)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,EP
C     REAL ou REAL*8
      my_real
     .     AREAI(MVSIZ),
     .     AL1(MVSIZ), AL2(MVSIZ), AL3(MVSIZ), ALMAX, ALMIN
      MY_REAL
     .   X32(MVSIZ), Y32(MVSIZ),AL4, AL5, AL6,AH(MVSIZ),
     .   H1(MVSIZ),H2(MVSIZ),H3(MVSIZ),H4(MVSIZ),H5(MVSIZ),H6(MVSIZ),
     .   C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),S1(MVSIZ),S2(MVSIZ),S3(MVSIZ),
     .   H14(MVSIZ),H24(MVSIZ),H25(MVSIZ),H35(MVSIZ),H4D,H5D,H6D,
     .   BETA1(MVSIZ),BETA2(MVSIZ),BETA3(MVSIZ),X42,Y42,Z2,X53,Y53,
     .   H36(MVSIZ),H16(MVSIZ),AL41,AL42,AL52,AL53,AI1,AI2,AI3,
     .   BETA14(MVSIZ),BETA24(MVSIZ),BETA25(MVSIZ),BETA35(MVSIZ),
     .   BETA36(MVSIZ),BETA16(MVSIZ),AL61,AL63,X61,Y61
      MY_REAL
     .   C11(MVSIZ,3,3), C21(MVSIZ,3,3),C22(MVSIZ,3),B11,B12,B13,
     .   B21,B22,B23,B31,B32,B33,B1,B2,B3,H15,H26,H34,FAC
c     .   EM20,ONE,HALF,TWO,EP20,ZEP6
c      DATA  EM20/1.E-20/,ONE/1.0/,HALF/0.5/,TWO/2.0/,EP20/1.E+20/,
c     .      ZEP6/0.6/
C-----------------------------------------------
      DO I=JFT,JLT
       AREAI(I)=ONE/AREA2(I)
       PX2(I)=Y3(I)*AREAI(I)
       PY2(I)=-X3(I)*AREAI(I)
       PX3(I)=-Y2(I)*AREAI(I)
       PY3(I)=X2(I)*AREAI(I)
       X32(I) =X3(I)-X2(I)
       Y32(I) =Y3(I)-Y2(I)
      ENDDO
C
      DO I=JFT,JLT
       AL1(I) = SQRT(X32(I)*X32(I) + Y32(I)*Y32(I))       
       AL2(I) = SQRT(X3(I) * X3(I) + Y3(I) * Y3(I))
       AL3(I) = SQRT(X2(I) * X2(I) + Y2(I) * Y2(I))
C------Hi inverse--------   
       H1(I) =  AL1(I)*AREAI(I)     
       H2(I) =  AL2(I)*AREAI(I)
       H3(I) =  AL3(I)*AREAI(I)
       AI1 = ONE/AL1(I)   
       AI2 = ONE/AL2(I)    
       AI3 = ONE/AL3(I) 
       C1(I) =  Y32(I)*AI1
       S1(I) = -X32(I)*AI1
       C2(I) = -Y3(I)*AI2
       S2(I) =  X3(I)*AI2
       C3(I) =  Y2(I)*AI3
       S3(I) = -X2(I)*AI3
       BETA1(I) =  S3(I)*S2(I)+C3(I)*C2(I)
       BETA2(I) =  S3(I)*S1(I)+C3(I)*C1(I)
       BETA3(I) =  S1(I)*S2(I)+C1(I)*C2(I)
       Z2 =Z4(I)*Z4(I)
       AL41 = X4(I) * X4(I) + Y4(I) * Y4(I) +Z2
       X42 =X4(I)-X2(I)
       Y42 =Y4(I)-Y2(I)
       AL42 = X42 * X42 + Y42 * Y42+Z2
       H4(I) =  AL3(I)*AREA4(I)     
       H14(I) = AREA4(I)*SQRT(AL42)   
       H24(I) = AREA4(I)*SQRT(AL41)
       H4D= (AL42-AL41)/AL3(I)  
       BETA14(I) = HALF*(H4D-AL3(I))/SQRT(AL41)
       BETA24(I) = -HALF*(H4D+AL3(I))/SQRT(AL42)
       Z2 =Z5(I)*Z5(I)
       AL52 = X5(I) * X5(I) + Y5(I) * Y5(I)+Z2
       X53 =X5(I)-X32(I)
       Y53 =Y5(I)-Y32(I)
       AL53 = X53 * X53 + Y53 * Y53+Z2
       H5 (I)=  AL1(I)*AREA5(I)     
       H25(I) = AREA5(I)*SQRT(AL53)   
       H35(I) = AREA5(I)*SQRT(AL52)   
       H5D= (AL53-AL52)/AL1(I)  
       BETA25(I) = HALF*(H5D-AL1(I))/SQRT(AL52)
       BETA35(I) = -HALF*(H5D+AL1(I))/SQRT(AL53)
       Z2 =Z6(I)*Z6(I)
       AL63 = X6(I) * X6(I) + Y6(I) * Y6(I)+Z2
       X61 =X6(I)+X3(I)
       Y61 =Y6(I)+Y3(I)
       AL61 = X61 * X61 + Y61 * Y61+Z2
       H6(I) =  AL2(I)*AREA6(I)     
       H36(I) = AREA6(I)*SQRT(AL61)   
       H16(I) = AREA6(I)*SQRT(AL63)   
       H6D= (AL61-AL63)/AL2(I) 
       BETA36(I) = HALF*(H6D-AL2(I))/SQRT(AL63)
       BETA16(I) = -HALF*(H6D+AL2(I))/SQRT(AL61)
      ENDDO
      DO I=JFT,JLT
       C11(I,1,1) = H1(I) 	      
       C11(I,2,1) = H1(I)*BETA3(I) 	      
       C11(I,3,1) = H1(I)*BETA2(I) 	      
       C11(I,1,2) = H2(I)*BETA3(I) 	      
       C11(I,2,2) = H2(I) 	      
       C11(I,3,2) = H2(I)*BETA1(I) 	      
       C11(I,1,3) = H3(I)*BETA2(I) 	      
       C11(I,2,3) = H3(I)*BETA1(I)	      
       C11(I,3,3) = H3(I) 	      
      ENDDO
      DO I=JFT,JLT
       C21(I,1,1) = BETA24(I)*H14(I) 	      
       C21(I,2,1) = 0.0 	      
       C21(I,3,1) = BETA36(I)*H16(I) 	      
       C21(I,1,2) = BETA14(I)*H24(I) 	      
       C21(I,2,2) = BETA35(I)*H25(I) 	      
       C21(I,3,2) = 0.0 	      
       C21(I,1,3) = 0.0 	      
       C21(I,2,3) = BETA25(I)*H35(I)	      
       C21(I,3,3) = BETA16(I)*H36(I) 	      
      ENDDO
      DO I=JFT,JLT
       C22(I,1) = H4(I) 	      
       C22(I,2) = H5(I) 	      
       C22(I,3) = H6(I) 	      
      ENDDO
C--------EN FUNCTION DES CONDITIONS LIMITES----------------------------------
      DO EP=NVS+1,JLT
        I =IVS(EP)
C--------cote node 4  ---------------------------------------
C--------LIBRE ---------------------------------------
       IF (IXTG1(1,I).EQ.0) THEN
        C21(I,1,1) = -C11(I,3,1)	      
        C21(I,1,2) = -C11(I,3,2)	      
        C21(I,1,3) = -C11(I,3,3)
        C22(I,1)   = 0.0	      
        H4(I)=EP20
C--------BLOQUE ---------------------------------------
       ELSEIF (IXTG1(1,I).LT.0) THEN
        C21(I,1,1) = C11(I,3,1)	      
        C21(I,1,2) = C11(I,3,2)	      
        C21(I,1,3) = C11(I,3,3)	      
        C22(I,1)   = 0.0	      
        H4(I)=EP20
       ENDIF 
C--------cote node 5  ---------------------------------------
C--------LIBRE ---------------------------------------
       IF (IXTG1(2,I).EQ.0) THEN
        C21(I,2,1) = -C11(I,1,1)	      
        C21(I,2,2) = -C11(I,1,2)	      
        C21(I,2,3) = -C11(I,1,3)	      
        C22(I,2)   = 0.0	      
        H5(I)=EP20
C--------BLOQUE ---------------------------------------
       ELSEIF (IXTG1(2,I).LT.0) THEN
        C21(I,2,1) = C11(I,1,1)	      
        C21(I,2,2) = C11(I,1,2)	      
        C21(I,2,3) = C11(I,1,3)	      
        C22(I,2)   = 0.0	      
        H5(I)=EP20
       ENDIF 
C--------cote node 6  ---------------------------------------
C--------LIBRE ---------------------------------------
       IF (IXTG1(3,I).EQ.0) THEN
        C21(I,3,1) = -C11(I,2,1)	      
        C21(I,3,2) = -C11(I,2,2)	      
        C21(I,3,3) = -C11(I,2,3)	      
        C22(I,3)   = 0.0	      
        H6(I)=EP20
C--------BLOQUE ---------------------------------------
       ELSEIF (IXTG1(3,I).LT.0) THEN
        C21(I,3,1) = C11(I,2,1)	      
        C21(I,3,2) = C11(I,2,2)	      
        C21(I,3,3) = C11(I,2,3)	      
        C22(I,3)   = 0.0	      
        H6(I)=EP20
       ENDIF 
      ENDDO
C
      DO I=JFT,JLT
       H15 =-TWO*H1(I)*H5(I)/(H1(I)+H5(I))
       H26 =-TWO*H2(I)*H6(I)/(H2(I)+H6(I))
       H34 =-TWO*H3(I)*H4(I)/(H3(I)+H4(I))
       ALMIN = -HALF*MIN(H15,H26,H34)
       AH(I) = ALMIN*ALMIN*THK2(I)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
       B11=C1(I)*C1(I)*H15
       B12=C2(I)*C2(I)*H26
       B13=C3(I)*C3(I)*H34
       B21= H15 - B11      
       B22= H26 - B12      
       B23= H34 - B13      
       B31=TWO*C1(I)*S1(I)*H15
       B32=TWO*C2(I)*S2(I)*H26
       B33=TWO*C3(I)*S3(I)*H34
C
       PB1(I,1,1) =B11*C11(I,1,1)+B12*C11(I,2,1)+B13*C11(I,3,1)	      
       PB1(I,1,2) =B11*C11(I,1,2)+B12*C11(I,2,2)+B13*C11(I,3,2)
       PB1(I,1,3) =B11*C11(I,1,3)+B12*C11(I,2,3)+B13*C11(I,3,3)
       PB1(I,2,1) =B21*C11(I,1,1)+B22*C11(I,2,1)+B23*C11(I,3,1)	      
       PB1(I,2,2) =B21*C11(I,1,2)+B22*C11(I,2,2)+B23*C11(I,3,2)
       PB1(I,2,3) =B21*C11(I,1,3)+B22*C11(I,2,3)+B23*C11(I,3,3)
       PB1(I,3,1) =B31*C11(I,1,1)+B32*C11(I,2,1)+B33*C11(I,3,1)	      
       PB1(I,3,2) =B31*C11(I,1,2)+B32*C11(I,2,2)+B33*C11(I,3,2)
       PB1(I,3,3) =B31*C11(I,1,3)+B32*C11(I,2,3)+B33*C11(I,3,3)
C
       PB2(I,1,1) =B13*C22(I,1)	      
       PB2(I,1,2) =B11*C22(I,2)
       PB2(I,1,3) =B12*C22(I,3)
       PB2(I,2,1) =B23*C22(I,1)	      
       PB2(I,2,2) =B21*C22(I,2)
       PB2(I,2,3) =B22*C22(I,3)
       PB2(I,3,1) =B33*C22(I,1)	      
       PB2(I,3,2) =B31*C22(I,2)
       PB2(I,3,3) =B32*C22(I,3)
C
       PB3(I,1,1) =B11	      
       PB3(I,1,2) =B12
       PB3(I,1,3) =B13
       PB3(I,2,1) =B21	      
       PB3(I,2,2) =B22
       PB3(I,2,3) =B23
       PB3(I,3,1) =B31	      
       PB3(I,3,2) =B32
       PB3(I,3,3) =B33
       PB3(I,1,4) =C21(I,2,1)	      
       PB3(I,1,5) =C21(I,2,2)
       PB3(I,1,6) =C21(I,2,3)
       PB3(I,2,4) =C21(I,3,1)	      
       PB3(I,2,5) =C21(I,3,2)
       PB3(I,2,6) =C21(I,3,3)
       PB3(I,3,4) =C21(I,1,1)	      
       PB3(I,3,5) =C21(I,1,2)
       PB3(I,3,6) =C21(I,1,3)
      ENDDO
C
      DO I=JFT,JLT
       FAC =ONE+ZEP6*(1+NU(I))*AH(I)
       ALMAX = SQRT(FAC)*MAX(AL1(I),AL2(I),AL3(I))
       ALDT(I)= AREA2(I) /ALMAX
       ALPE(I)=ONE
      ENDDO
C

C---------------------------------------------------------
      RETURN
C
      END
