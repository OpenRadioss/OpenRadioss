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
Chd|  I2THERM                       source/interfaces/interf/i2therm.F
Chd|-- called by -----------
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2THERM(X       ,NSN     ,NSV     ,IRTL    ,MS      ,
     .                   WEIGHT  ,IRECT   ,CRST    ,IADI2   ,KTHE    ,
     .                   TEMP    ,AREAS   ,FTHE    ,FTHESKYI,CONDN   ,   
     .                   CONDNSKYI,I0     ,ITAB    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr14_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "scr_thermal_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NSN,I0
      INTEGER  IRECT(4,*),NSV(*),IRTL(*),WEIGHT(*), IADI2(4,*),ITAB(*)
      my_real
     .   KTHE,
     .   X(3,*),TEMP(*),MS(*),CRST(2,*),AREAS(*),FTHE(*),FTHESKYI(*),
     .   CONDN(*),CONDNSKYI(*)
C     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II,L,W,IX1,IX2,IX3,IX4,NN
C     REAL ou REAL*8
      my_real
     .   S, T, SP ,SM  , TP, TM ,H1,H2,H3,H4,AX1,AY1,AZ1,AX2,AY2,AZ2,AX,AY,AZ,
     .   PHI1,PHI2,PHI3,PHI4,AREAM,AREAC,TEMPS,TEMPM,PHI,CONDINT

C-----------------------------------------------

      DO II=1,NSN
         I=NSV(II)
C
         IF(I.GT.0)THEN
           L=IRTL(II)
C
           W = WEIGHT(I)
           S = CRST(1,II)  
           T = CRST(2,II)  
           SP=ONE + S          
           SM=ONE - S          
           TP=FOURTH*(ONE + T)  
           TM=FOURTH*(ONE - T)  
C
           IX1 = IRECT(1,L)                                       
           IX2 = IRECT(2,L)                                       
           IX3 = IRECT(3,L)                                       
           IX4 = IRECT(4,L)  
           IF(IX3.EQ.IX4) THEN
              H1=TM*SM                                         
              H2=TM*SP  
              H3=ONE-H1-H2
              H4=ZERO
           ELSE
              H1=TM*SM                                         
              H2=TM*SP                                         
              H3=TP*SP                                         
              H4=TP*SM      
           ENDIF                                              
C            
           AX1 = X(1,IX3) - X(1,IX1)
           AY1 = X(2,IX3) - X(2,IX1)
           AZ1 = X(3,IX3) - X(3,IX1)
           AX2 = X(1,IX4) - X(1,IX2)
           AY2 = X(2,IX4) - X(2,IX2)
           AZ2 = X(3,IX4) - X(3,IX2)
C            
           AX  = AY1*AZ2 - AZ1*AY2
           AY  = AZ1*AX2 - AX1*AZ2
           AZ  = AX1*AY2 - AY1*AX2
C
           AREAM = ONE_OVER_8*SQRT(AX*AX+AY*AY+AZ*AZ)      
           AREAC = MIN(AREAS(II),AREAM)
C            
           TEMPS = TEMP(I)
           TEMPM = H1*TEMP(IX1)+H2*TEMP(IX2)+H3*TEMP(IX3)+H4*TEMP(IX4)
C                    
           PHI  =  AREAC*(TEMPM - TEMPS)*DT1*KTHE*THEACCFACT 

C
           CONDINT = AREAC*KTHE*THEACCFACT

           PHI1 = -PHI *H1
           PHI2 = -PHI *H2
           PHI3 = -PHI *H3
           PHI4 = -PHI *H4
c
           FTHE(I)=FTHE(I)+PHI

           IF(IDT_THERM == 1) CONDN(I) = CONDN(I) + CONDINT*W   
c
           IF (IPARIT == 0.AND.W == 1) THEN
               FTHE(IX1)=FTHE(IX1)+PHI1 
               FTHE(IX2)=FTHE(IX2)+PHI2 
               FTHE(IX3)=FTHE(IX3)+PHI3              
               FTHE(IX4)=FTHE(IX4)+PHI4   
               IF(IDT_THERM == 1) THEN
                  CONDN(IX1)=CONDN(IX1)+ABS(H1)*CONDINT
                  CONDN(IX2)=CONDN(IX2)+ABS(H2)*CONDINT 
                  CONDN(IX3)=CONDN(IX3)+ABS(H3)*CONDINT              
                  CONDN(IX4)=CONDN(IX4)+ABS(H4)*CONDINT
               ENDIF
           ELSEIF (IPARIT > 0.AND.W == 1) THEN
               I0 = I0 + 1                                                                                                         
               NN = IADI2(1,I0)
               FTHESKYI(NN)=PHI1
               NN = IADI2(2,I0) 
               FTHESKYI(NN)=PHI2 
               NN = IADI2(3,I0)
               FTHESKYI(NN)=PHI3
               NN = IADI2(4,I0) 
               FTHESKYI(NN)=PHI4 
               IF(IDT_THERM == 1) THEN
                  NN = IADI2(1,I0)
                  CONDNSKYI(NN)=ABS(H1)*CONDINT
                  NN = IADI2(2,I0) 
                  CONDNSKYI(NN)=ABS(H2)*CONDINT
                  NN = IADI2(3,I0)
                  CONDNSKYI(NN)=ABS(H3)*CONDINT
                  NN = IADI2(4,I0)                            
                  CONDNSKYI(NN)=ABS(H4)*CONDINT
               ENDIF      
           ENDIF
         ELSE
           L = IRTL(II)
C
           IX1 = IRECT(1,L)                                       
           IX2 = IRECT(2,L)                                       
           IX3 = IRECT(3,L)                                          
           IX4 = IRECT(4,L)  
           FTHE(I)= ZERO
           FTHE(IX1)=ZERO 
           FTHE(IX2)=ZERO 
           FTHE(IX3)=ZERO             
           FTHE(IX4)=ZERO  
           CONDN(I)= ZERO
           CONDN(IX1)=ZERO 
           CONDN(IX2)=ZERO 
           CONDN(IX3)=ZERO             
           CONDN(IX4)=ZERO    
         ENDIF
        ENDDO   
C
      RETURN
      END



