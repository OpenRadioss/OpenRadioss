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
Chd|  FRV                           source/materials/mat/mat024/fr.F
Chd|-- called by -----------
Chd|        CRIT24                        source/materials/mat/mat024/crit24.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FRV(S1,S2,S3,S4,S5,S6,
     .               SM,VK0,ROB,FC,RT,RC,
     .               RCT1,RCT2,AA,AC,BC,BT,
     .               ROK,TOL,FA,NINDEX,INDEX,IBUG,
     .               NEL,SEQ,ICRIT_OUTP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINDEX, INDEX(NEL),ICRIT_OUTP
      my_real :: FC,RT,RC,RCT1,RCT2,AA,AC,BC,BT,TOL
      my_real, DIMENSION(NEL) :: S1,S2,S3,S4,S5,S6,SM,VK0,ROB,ROK,FA,SEQ
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, NEL, IBUG
      my_real VK, BB, DF, RF, R2, AJ3, CS3T
C=======================================================================
#include "vectorize.inc"
      DO N = 1,NINDEX
        I = INDEX(N)
        IF (I > 0) THEN
          IF (SM(I) >= RT-TOL ) THEN                                        
            VK = ONE                                                               
          ELSEIF( SM(I) > RC ) THEN                                            
            VK = ONE+(ONE-VK0(I))*(RCT1-TWO*RC*SM(I)+SM(I)**2)/RCT2       
          ELSEIF(SM(I) > ROK(I))THEN                                              
            VK = VK0(I)                                                           
          ELSE                                                                    
            VK = VK0(I)*(ONE - ((SM(I)-ROK(I))/(ROB(I)-ROK(I)))**2)                
          ENDIF                                                                   
C
          IF (SM(I) > AC/AA) THEN                                           
            FA(I) = HALF*(SM(I)-AC/AA) / MIN(BC,BT)/FC           
          ELSEIF (SM(I) <= ROB(I)) THEN                                           
            BB = MAX(BC,BT)                                                  
            DF = SQRT(BB*BB-AA*ROB(I)+AC)                                   
            RF = (-BB+DF)/AA                                                   
            FA(I) = TWO*RF*(SM(I)-ROB(I))/(ROB(I)-ROK(I))/FC                 
          ELSE                                                                    
            R2 = (S1(I)**2+S2(I)**2+S3(I)**2)+ TWO*S4(I)**2+TWO*S5(I)**2       
     .         + TWO*S6(I)**2                                                   
            IF (IBUG == 0) THEN                                                   
              AJ3 = S1(I)*S2(I)*S3(I)-S1(I)*S5(I)*S5(I)-S2(I)*S6(I)*S6(I)         
     .            - S3(I)*S4(I)*S4(I) + TWO*S4(I)*S5(I)*S6(I)                                       
            ELSE   ! old version                               
              AJ3 = S1(I)*S2(I)*S3(I)-S1(I)*S5(I)*S5(I)-S2(I)*S6(I)*S6(I)         
     .            - S3(I)*S4(I)*S4(I)                                             
            ENDIF                                                                 
c
            CS3T = HALF * AJ3*(THREE/(HALF*MAX(R2,EM20)))**THREE_HALF                  
            CS3T = MIN(ONE,CS3T)                                                   
            CS3T = MAX(-ONE,CS3T)                                                  
            BB = HALF*((ONE -CS3T)*BC+(ONE +CS3T)*BT)                       
            DF = SQRT(MAX(ZERO,BB*BB-AA*SM(I)+AC,ZERO))                     
            RF = (-BB+DF)/AA                                                   
            FA(I)=(SQRT(R2)-VK*RF)/FC                                          
c
!!            IF (ICRIT_OUTP == 0) THEN ! (EQUIVALENT STRESS)/CTITERION FOR OUTPUT  
!!              SEQ(I) = FA(I)                                               
!!              ICRIT_OUTP = 1                                                      
!!            ENDIF ! IF (ICRIT_OUTP == 0)                                          
          ENDIF                                                                   
c
        ENDIF                                                                   
      ENDDO
c-----------
      RETURN
      END
