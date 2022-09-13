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
Chd|  OSBORNE                       eos/osborne.F                 
Chd|-- called by -----------
Chd|        EOSMAIN                       common_source/eos/eosmain.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE OSBORNE  (IFLAG,NEL, PM   ,OFF  ,EINT ,MU   ,MU2 , 
     2                     ESPE ,DVOL ,DF   ,VNEW ,MAT  ,PSH ,
     3                     PNEW ,DPDM ,DPDE ,THETA,ECOLD)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine contains numerical solving
C of OSBORNE EOS
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
     .   VNEW(*) ,PNEW(*) ,DPDM(*), PSH(*) ,
     .   DPDE(*) ,THETA(*),ECOLD(*),
     .   DENOM       ,PART1       ,PART2       ,PART3
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
      INTEGER I, MX
      my_real :: P0,E0,SPH,AA, BB,TFEXTT, DVV, PP,dPdMU
      my_real :: A1,A2,B0,B1,B2,C0,C1,D0, A2_,RHO0,DE, ESP
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF(IFLAG == 0) THEN
C-----------------------------------------
C     COMPUTE BULK MODULUS FOR SOUND SPEED
C     COMPUTE COLD COMPRESSION ENERGY
C-----------------------------------------

       MX           = MAT(1)
       RHO0         = PM(01,MX)           
       E0           = PM(23,MX)           
       A1           = PM(164,MX)        
       A2           = PM(32,MX)        
       B0           = PM(33,MX)        
       B1           = PM(35,MX)        
       B2           = PM(36,MX)        
       C0           = PM(160,MX)        
       C1           = PM(161,MX)        
       D0           = PM(162,MX)        
       P0           = PM(163,MX)        
       PSH(LFT:LLT) = PM(88,MX) 
       SPH          = PM(69,MX)    
       
       DO I=LFT,LLT
         A2_=A2
         IF(MU(I)<ZERO)A2_=-A2
         DENOM   = (ESPE(I)+D0)
         PP      = (A1*MU(I)+A2_*MU(I)*MU(I)+(B0+B1*MU(I)+B2*MU(I)*MU(I))*ESPE(I)+(C1*MU(I)+C0)*ESPE(I)*ESPE(I))
     .             /DENOM
         dPdMU   = (A1+2*A2_*MU(I)+(TWO*B2*MU(I)+B1)*ESPE(I)+C1*ESPE(I)*ESPE(I))/DENOM
         dPdE(I) = (((B2*MU(I)+B1)*MU(I)+B0)+(TWO*(C1*MU(I)+C0))*ESPE(I) - PP/DENOM)/DENOM                                      
         DPDM(I) = dPdmu + dPdE(I)*DF(I)*DF(I)*(PP)   !total derivative
       ENDDO

       DO I=LFT,LLT
         ECOLD(I)=-THREE100*SPH 
         !IF(MU(I)>ZERO) ECOLD(I)=ECOLD(I)*(ONE+C4 *MU(I)*(ONE-MU(I)))+HALF*C1 *MU2(I)
       ENDDO

      ELSEIF(IFLAG == 1) THEN
C----------------------------------------
C     UPDATE PRESSURE AND INTERNAL ENERGY
C----------------------------------------
       TFEXTT  = ZERO
       
       MX           = MAT(1) 
       RHO0         = PM(01,MX)           
       E0           = PM(23,MX)           
       A1           = PM(164,MX)        
       A2           = PM(32,MX)        
       B0           = PM(33,MX)        
       B1           = PM(35,MX)        
       B2           = PM(36,MX)        
       C0           = PM(160,MX)        
       C1           = PM(161,MX)        
       D0           = PM(162,MX)        
       P0           = PM(163,MX)        
       PSH(LFT:LLT) = PM(88,MX) 
       SPH          = PM(69,MX) 

       ! MEINT
       !  EINT(I)=EINT(I)+ HALF*DVOL(I)*(POLD(I)-PSH(I)-QOLD(I)-QNEW(I))
       !  ESPE(I) = EINT / V0
       
       DO I=LFT,LLT
         A2_=A2
         IF(MU(I)<ZERO)A2_=-A2
         
         DVV      = DVOL(I)*DF(I) / MAX(EM15,VNEW(I))   ! DVOL/V0  car  ESPE =EINT/V0
         DVV      = HALF*DVV ! car 2 iterations

         DENOM   = (ESPE(I)+D0)
         PP      = (A1*MU(I)+A2_*MU(I)*MU(I)+(B0+B1*MU(I)+B2*MU(I)*MU(I))*ESPE(I)+(C1*MU(I)+C0)*ESPE(I)*ESPE(I))
     .             /DENOM     
         ESPE(I) = ESPE(I) - (PP)*DVV
         
         DENOM   = (ESPE(I)+D0)         
         PP      = (A1*MU(I)+A2_*MU(I)*MU(I)+(B0+B1*MU(I)+B2*MU(I)*MU(I))*ESPE(I)+(C1*MU(I)+C0)*ESPE(I)*ESPE(I))
     .             /DENOM
         ESPE(I) = ESPE(I) - (PP)*DVV

         PNEW(I) = PP
              
         EINT(I) = EINT(I) - HALF*DVOL(I)*(PNEW(I))
         PNEW(I) = PNEW(I) + PSH(I)
         TFEXTT  = TFEXTT-DVOL(I)*PSH(I)
       ENDDO
       
#include "atomic.inc"
       TFEXT = TFEXT + TFEXTT
#include "atomend.inc"
C
C------------------------
C     COMPUTE TEMPERATURE
C------------------------

       MX     = MAT(1)
       SPH    = PM(69,MX)

       DO I=LFT,LLT
         IF(OFF(I)<1..OR.SPH<=0.) CYCLE
         THETA(I) = (DF(I)*EINT(I)/VNEW(I)-ECOLD(I))/SPH
         THETA(I) = MAX(ZERO,THETA(I))
       ENDDO
      ELSEIF(IFLAG == 2) THEN
         MX           = MAT(1)
         RHO0         = PM(01,MX)           
         E0           = PM(23,MX)           
         A1           = PM(164,MX)        
         A2           = PM(32,MX)        
         B0           = PM(33,MX)        
         B1           = PM(35,MX)        
         B2           = PM(36,MX)        
         C0           = PM(160,MX)        
         C1           = PM(161,MX)        
         D0           = PM(162,MX)        
         P0           = PM(163,MX)        
         PSH(1:NEL) = PM(88,MX) 
         SPH          = PM(69,MX)    
         
         DO I=1, NEL
            IF (VNEW(I) > ZERO) THEN
               A2_=A2
               IF(MU(I)<ZERO)A2_=-A2
               DENOM   = (ESPE(I)+D0)
               PP      = (A1*MU(I)+A2_*MU(I)*MU(I)+(B0+B1*MU(I)+B2*MU(I)*MU(I))*ESPE(I)+(C1*MU(I)+C0)*ESPE(I)*ESPE(I))
     .              /DENOM
               dPdMU   = (A1+2*A2_*MU(I)+(TWO*B2*MU(I)+B1)*ESPE(I)+C1*ESPE(I)*ESPE(I))/DENOM
               dPdE(I) = (((B2*MU(I)+B1)*MU(I)+B0)+(TWO*(C1*MU(I)+C0))*ESPE(I) - PP/DENOM)/DENOM                                      
               DPDM(I) = dPdmu + dPdE(I)*DF(I)*DF(I)*(PP) !total derivative
               PNEW(I) = PP
            ENDIF
         ENDDO
      ENDIF
C------------------------      
      RETURN
      END
