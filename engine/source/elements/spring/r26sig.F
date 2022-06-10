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
Chd|  R26SIG                        source/elements/spring/r26sig.F
Chd|-- called by -----------
Chd|        R26DEF3                       source/elements/spring/r26def3.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE R26SIG(FX     ,XK     ,DX     ,DXOLD  ,DMAX   ,
     .                  E      ,OFF    ,XL0    ,TF     ,NPF    , 
     .                  ANIM   ,IANI   ,FR_WAVE,DMN    ,DMX    , 
     .                  IGEO   ,GEO    ,PID    ,DV0    ,ALPHA  )                 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IANI
      INTEGER NPF(*),IGEO(NPROPGI,*),PID(*)
C     REAL
      my_real
     .   GEO(NPROPG,*),FX(*),XK(*),DX(*),DXOLD(*),TF(*),XL0(*),E(*),
     .   OFF(*), DMN(*), DMX(*), ANIM(*),FR_WAVE(*),DMAX(*),DV0(*),
     .   ALPHA(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr05_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, J1, J2
      INTEGER FUNC1(MVSIZ), FUNC2(MVSIZ),NFUNC(MVSIZ),NFUND(MVSIZ) 
C     REAL
      my_real
     .    DDX(MVSIZ),FOLD(MVSIZ),DVX(MVSIZ),DV(MVSIZ),DL(MVSIZ),
     .    RATE1(MVSIZ),RATE2(MVSIZ),YFAC1(MVSIZ), YFAC2(MVSIZ),
     .    FMIN(MVSIZ),FMAX(MVSIZ),XFAC(MVSIZ)
      my_real
     .    DF1,DF2,Y1,Y2,VEL,FAC,DAMP,DAMM,DT11
C-----------------------------------------------
C   E x t e r n a l
C-----------------------------------------------
      my_real
     .    finter
C=======================================================================
      DT11 = DT1
      IF (DT11 == ZERO) DT11 = EP30
      DO I=LFT,LLT
        E(I)    =E(I)/XL0(I)
        DX(I)   =DX(I)/XL0(I)
        DXOLD(I)=DXOLD(I)/XL0(I)
        DMAX(I) =DMAX(I)/XL0(I)
      ENDDO
C
      DO I=LFT,LLT
        FOLD(I)=FX(I)
        DDX(I)= (DX(I)-DXOLD(I))
        DVX(I)= DDX(I)/ DT11
        DV(I) = ABS(DVX(I))
        DL(I) = ABS(DX(I))
      ENDDO
      IF (TT == ZERO) THEN
        DO I=LFT,LLT
          DV0(I) = DV(I)
        ENDDO
      ENDIF
c     filtrage des vitesses
      DO I=LFT,LLT
        DV(I)  = (ONE-ALPHA(I))*DV0(I) + ALPHA(I)*DV(I)
        DV0(I) =  DV(I)
      ENDDO
C
      IF(IANI.NE.0)THEN
        DO I=LFT,LLT
          J =I+NFT
          DAMP=DX(I)/MAX(DMX(I),EM15)
          DAMM=DX(I)/MIN(DMN(I),-EM15)
          ANIM(J)=MAX(ANIM(J),DAMP,DAMM)
          ANIM(J)=MIN(ANIM(J),ONE)
        ENDDO
      ENDIF
C-------------------------------------
      DO I=LFT,LLT
        IF (DX(I) >= ZERO) THEN
          FX(I) = XK(I)*DX(I)
        ELSE
          FX(I) = FOLD(I) + XK(I)*DDX(I)
        ENDIF
        IAD = 100
C calcul du y_max
        NFUNC(I) = IGEO(20,PID(I))  
        XFAC(I) = GEO(5,PID(I))
c        IGEO(IAD+I)     = IFUN                                  
c        GEO(IAD+100+I) = VEL                                    
c        GEO(IAD+200+I) = YSCALE                                 

        J1 = 1                                                   
        DO J=2,NFUNC(I)-1                                        
          VEL = GEO(IAD+100+J,PID(I))                            
          IF (DV(I) >= VEL) J1 = J                               
        ENDDO                                                    
        FUNC1(I) = IGEO(IAD+J1,PID(I))                           
        RATE1(I) = GEO(IAD+100+J1,PID(I))                        
        YFAC1(I) = GEO(IAD+200+J1,PID(I))                        
        IF (NFUNC(I) > 1)THEN                                    
          J2 = J1+1                                              
          FUNC2(I) = IGEO(IAD+J2,PID(I))                         
          RATE2(I) = GEO(IAD+100+J2,PID(I))                      
          YFAC2(I) = GEO(IAD+200+J2,PID(I))                      
C
          Y1 = YFAC1(I)*FINTER(FUNC1(I),DL(I)/XFAC(I),NPF,TF,DF1)        
          Y2 = YFAC2(I)*FINTER(FUNC2(I),DL(I)/XFAC(I),NPF,TF,DF2)         
C                                                                
          FAC     = (DV(I) - RATE1(I))/(RATE2(I) - RATE1(I))     
          FMAX(I) =  -MAX(Y1 + FAC*(Y2 - Y1), EM20)               
        ELSE                                                     
          FMAX(I) =  -YFAC1(I)*FINTER(FUNC1(I),DL(I)/XFAC(I),NPF,TF,DF1)  
        ENDIF                                                    
C calcul du y_min  
        J1 = NFUNC(I) + 1                                        
        NFUND(I) = IGEO(21,PID(I))                               
        DO J=2,NFUND(I)-1                                        
          VEL = GEO(IAD+100+J,PID(I))                            
          IF (DV(I) >= VEL) J1 = NFUNC(I) + J                    
        ENDDO                                                    
        FUNC1(I) = IGEO(IAD+J1,PID(I))                           
        RATE1(I) = GEO(IAD+100+J1,PID(I))                        
        YFAC1(I) = GEO(IAD+200+J1,PID(I))                        
C                                                                
        IF (NFUND(I) > 1) THEN                                   
          J2 = J1+1                                              
          FUNC2(I) = IGEO(IAD+J2,PID(I))                         
          RATE2(I) = GEO(IAD+100+J2,PID(I))                      
          YFAC2(I) = GEO(IAD+200+J2,PID(I))                      
C
          Y1 = YFAC1(I)*FINTER(FUNC1(I),DL(I)/XFAC(I),NPF,TF,DF1)        
          Y2 = YFAC2(I)*FINTER(FUNC2(I),DL(I)/XFAC(I),NPF,TF,DF2)         
          FAC = (DV(I) - RATE1(I))/(RATE2(I) - RATE1(I))         
c          FMIN(I) = -MAX(Y2 + FAC*(Y1-Y2), EM20)                
          FMIN(I) =  -MAX(Y1 + FAC*(Y2 - Y1), EM20)               
        ELSE                                                     
          FMIN(I) =  -YFAC1(I)*FINTER(FUNC1(I),DL(I)/XFAC(I),NPF,TF,DF1)  
        ENDIF                                                    
C--------------
        IF (DX(I) < ZERO) THEN        
          IF (DL(I) < DMAX(I)) THEN      
            IF (FX(I) > FMIN(I)) THEN    
              FX(I) = FMIN(I)            
            ELSEIF (FX(I) < FMAX(I) )THEN
              FX(I) = FMAX(I)            
            ENDIF                        
          ENDIF                          
        ENDIF                            

      ENDDO
C--------------
      DO I=LFT,LLT
        IF (ABS(DDX(I)) > ZERO)  XK(I) = ABS(FX(I)-FOLD(I))/ABS(DDX(I))
        E(I) = E(I) + (DX(I)-DXOLD(I)) * (FX(I)+FOLD(I)) * HALF
        E(I) = E(I)*XL0(I)
        DX(I)= DX(I)*XL0(I)
        DXOLD( I)=DXOLD(I)*XL0(I)
      ENDDO
C---
      RETURN
      END
