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
Chd|  JWL                           eos/jwl.F                     
Chd|-- called by -----------
Chd|        EOSMAIN                       common_source/eos/eosmain.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE JWL
     1              (IFLAG,NEL  ,PM   ,OFF  ,EINT ,MU  ,MU2 , 
     2               ESPE ,DVOL ,DF   ,VNEW ,MAT  ,PSH ,
     3               PNEW ,DPDM ,DPDE ,THETA,ECOLD)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine contains numerical solving
C of JWL EOS
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(*), IFLAG, NEL
      my_real
     .   PM(NPROPM,*), 
     .   OFF(*)  ,EINT(*) ,MU(*)   , 
     .   MU2(*)  ,ESPE(*) ,DVOL(*) ,DF(*)  , 
     .   VNEW(*) ,PNEW(*) ,DPDM(*),
     .   DPDE(*) ,THETA(*),ECOLD(*)
      my_real, INTENT(INOUT) :: PSH(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr06_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,IBFRAC
      my_real
     .   VDET ,  
     .   BFRAC(MVSIZ),WDR1V,WDR2V,
     .   BFRAC1, BFRAC2, RHO0 , AA , BB , R1,
     .   R2, W1,  BHE, P0, BULK,UNPMU,
     .   R1DF,R2DF,ER1DF,ER2DF,dPdmu
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      !-----------------------------------------
      !     COMPUTE BULK MODULUS FOR SOUND SPEED
      !     COMPUTE COLD COMPRESSION ENERGY
      !-----------------------------------------
      IF(IFLAG == 0) THEN


      ELSEIF(IFLAG == 1) THEN

         
      ELSEIF(IFLAG == 2) THEN
        !----------------------------------------
        !     FOR USE WITH MULTULMATERIAL LAW 151
        !----------------------------------------
         MX     = MAT(LFT)         
         RHO0   = PM( 1,MX)        
         AA     = PM(33,MX)        
         BB     = PM(34,MX)        
         R1     = PM(35,MX)        
         R2     = PM(36,MX)        
         W1     = PM(45,MX)        
         VDET   = PM(38,MX)        
         BHE    = PM(40,MX)        
         P0     = PM(31,MX)        
         BULK   = PM(44,MX)        
         IBFRAC = NINT(PM(41,MX))  
         PSH(1:NEL) = PM(88,MX)        
             
         DO I=1,NEL
           BFRAC(I)=ONE
         ENDDO    
                                              
         DO I=1, NEL                                                                                        
            IF (VNEW(I) .GT. ZERO) THEN                                                                     
              R1DF = R1*DF(I)                                                                               
              R2DF = R2*DF(I)                                                                               
              ER1DF = EXP(-R1DF)                                                                            
              ER2DF = EXP(-R2DF)                                                                              
              PNEW(I) =  - PSH(I) + AA*(ONE-W1/R1DF)*ER1DF + BB*(ONE-W1/R2DF)*ER2DF + W1*ESPE(I)/DF(I)   
              PNEW(I) = MAX(ZERO - PSH(I), PNEW(I))                                                         
              ! dPdE  : partial derivative                                                                  
              ! dPdmu : partial derivative                                                                  
              ! DPDM  : total derivative                                                                         
              dPdE(I) = W1/DF(I)                                                                            
              dPdmu   =                                                                                     
     .              -AA*W1*ER1DF/R1 + AA*(ONE-W1/(R1DF))*R1DF*R1DF*ER1DF                                     
     .              -BB*W1*ER2DF/R2 + BB*(ONE-W1/(R2DF))*R2DF*R2DF*ER2DF                                     
     .              +W1*ESPE(I)                                                                             
              DPDM(I) = dPdmu + (PNEW(I)+PSH(I))*DF(I)*DF(I)*dPdE(I)                                        
            ENDIF                                                                                           
         ENDDO                                                                                              
      ENDIF
      
C-----------------------------------------------
      RETURN
      END
