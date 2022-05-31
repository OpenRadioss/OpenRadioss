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
Chd|  I2_FFORM                      source/interfaces/interf/i2_fform.F
Chd|-- called by -----------
Chd|        I2CURVF                       source/interfaces/interf/i2curvf.F
Chd|        I2CURVFP                      source/interfaces/interf/i2curvfp.F
Chd|        I2CURVV                       source/interfaces/interf/i2curvv.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2_FFORM(
     .   NIR,S,T,H,HH,HRS,HRT,HPS,HPT,HPRS,HPRT,
     .   HXS,HXT,LS1,LS2,LT1,LT2,LS,LT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIR,I
      my_real
     .  S,T,LS1,LS2,LT1,LT2,LS,LT
      my_real
     .  H(4),HH(4),HRS(4),HRT(4),HPS(4),HPT(4),HPRS(4),HPRT(4),
     .  HXS(4),HXT(4)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .  SLM,SLP,TLM,TLP,SHM,SHP,THM,THP,SM2,SP2,TM2,TP2,SRM,SRP,TRM,TRP,
     .  SRPM,SRPP,TRPM,TRPP,S2,S3,T2,T3
C-----------------------------------------------
C     Form functions based on Adini Clough Melosh rectangular element
C======================================================================|
      S2 = S*S
      S3 = S*S2
      T2 = T*T
      T3 = T*T2
c
      SLM  = (ONE - S)*HALF                                            
      SLP  = (ONE + S)*HALF                                            
      TLM  = (ONE - T)*HALF                                            
      TLP  = (ONE + T)*HALF                                            
C
      LS   = TLM*LS1 + TLP*LS2                                             
      LT   = SLM*LT1 + SLP*LT2                              
C
      SM2 = SLM*SLM                                                    
      SP2 = SLP*SLP                                                    
      TM2 = TLM*TLM                                                    
      TP2 = TLP*TLP                                                    
C---- fonctions de forme lineaires
      H(1)  = SLM*TLM                                                    
      H(2)  = SLP*TLM                                                    
      H(3)  = SLP*TLP                                                    
      H(4)  = SLM*TLP                                                    
C---- fonctions de forme bicubiques (Adini, Clough)
c     u       = sum( HH(i)*u(i) + HRS(i) *theta_s(i)  + HRT(i) *theta_t(i))     
c     theta_s = sum(HPS(i)*u(i) + HPRS(i)*theta_s(i)) + HXS(i) *theta_t(i))    
c     theta_t = sum(HPT(i)*u(i) + HXT(i) *theta_s(i)) + HPRT(i)*theta_t(i))    
c
c     F(i)    =  HH(i)*Fz +  HPS(i)*Ms +  HPT(i)*Mt
c     Ms(i)   = HRS(i)*Fz + HPRS(i)*Ms +  HXT(i)*Mt
c     Mt(i)   = HRT(i)*Fz +  HXS(i)*Ms + HPRT(i)*Mt
C-------------------------
      HH(1)  = SLM*TLM*(ONE - S*SLP - T*TLP)                                               
      HH(2)  = SLP*TLM*(ONE + S*SLM - T*TLP)                                                 
      HH(3)  = SLP*TLP*(ONE + S*SLM + T*TLM)                                                  
      HH(4)  = SLM*TLP*(ONE - S*SLP + T*TLM)                                                 

      HRS(1) = SLM*TLP*TM2*LT                                             
      HRS(2) = SLP*TLP*TM2*LT                                            
      HRS(3) =-SLP*TLM*TP2*LT                                                 
      HRS(4) =-SLM*TLM*TP2*LT                                                 
c
      HRT(1) =-TLM*SLP*SM2*LS                    
      HRT(2) = TLM*SLM*SP2*LS                    
      HRT(3) = TLP*SLM*SP2*LS                    
      HRT(4) =-TLP*SLP*SM2*LS                    
C-----
      HPS(1) = (-THREE + FOUR*S + SIX*T2*SLM - S3) * FOURTH / LT                                         
      HPS(2) = (-THREE - FOUR*S + SIX*T2*SLP + S3) * FOURTH / LT                                          
      HPS(3) = ( THREE + FOUR*S - SIX*T2*SLP - S3) * FOURTH / LT                                         
      HPS(4) = ( THREE - FOUR*S - SIX*T2*SLM + S3) * FOURTH / LT          

      HPT(1) =-(-THREE + FOUR*T + SIX*S2*TLM - T3) * FOURTH / LS                                          
      HPT(2) =-( THREE - FOUR*T - SIX*S2*TLM + T3) * FOURTH / LS                                           
      HPT(3) =-( THREE + FOUR*T - SIX*S2*TLP - T3) * FOURTH / LS                                                
      HPT(4) =-(-THREE - FOUR*T + SIX*S2*TLP + T3) * FOURTH / LS
C-----
      SRPM = THREE_HALF*S2 - S - HALF                                          
      SRPP = THREE_HALF*S2 + S - HALF                                           
      TRPM = THREE_HALF*T2 - T - HALF                                           
      TRPP = THREE_HALF*T2 + T - HALF                                          

      HPRS(1) = SLM * TRPM                                               
      HPRS(2) = SLP * TRPM                                               
      HPRS(3) = SLP * TRPP                                               
      HPRS(4) = SLM * TRPP    

      HPRT(1) = TLM * SRPM                                              
      HPRT(2) = TLM * SRPP                                              
      HPRT(3) = TLP * SRPP                                              
      HPRT(4) = TLP * SRPM    
C-----
      HXS(1) =-SLP*SM2 * LS/LT                                                
      HXS(2) = SLM*SP2 * LS/LT                                                
      HXS(3) =-HXS(2)                                                
      HXS(4) =-HXS(1)                 

      HXT(1) =-TLP*TM2 * LT/LS                                                
      HXT(2) =-HXT(1)                                                
      HXT(3) =-TLM*TP2 * LT/LS                                               
      HXT(4) =-HXT(3)                   
C-----------
      RETURN
      END
