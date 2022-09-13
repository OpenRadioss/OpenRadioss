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
Chd|  STIFFGAS                      eos/stiffgas.F                
Chd|-- called by -----------
Chd|        EOSMAIN                       common_source/eos/eosmain.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE STIFFGAS
     1                    (IFLAG,NEL, PM   ,OFF  ,EINT ,MU   ,MU2 , 
     2                     ESPE ,DVOL ,DF   ,VNEW ,MAT  ,PSH ,
     3                     PNEW ,DPDM ,DPDE ,THETA,ECOLD)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine contains numerical solving
C of STIFFENED GAS EOS
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
      INTEGER I, MX
      my_real :: P0,GAMMA,T0,E0,SPH,AA, BB,TFEXTT, DVV, PP, PSTAR, PC
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF(IFLAG == 0) THEN
C-----------------------------------------
C     COMPUTE BULK MODULUS FOR SOUND SPEED
C     COMPUTE COLD COMPRESSION ENERGY
C-----------------------------------------
         MX           = MAT(1)
         E0           = PM(23,MX)         
         GAMMA        = PM(34,MX)
         P0           = PM(32,MX)
         PSH(LFT:LLT) = PM(88,MX)
         PSTAR        = PM(35,MX)
         SPH          = PM(69,MX)
         
       DO I=LFT,LLT
         PP      = -GAMMA*PSTAR-PSH(I) + (GAMMA-ONE)*(ONE+MU(I))*ESPE(I)
         DPDM(I) =  (GAMMA-ONE) *ESPE(I)+(GAMMA-ONE)*(ONE+MU(I))*DF(I)*DF(I)*(PP+PSH(I) )
         DPDE(I) = (GAMMA-ONE)*(ONE+MU(I))
       ENDDO

       DO I=LFT,LLT
         ECOLD(I)=-THREE100*SPH
         IF(MU(I)>ZERO) ECOLD(I)=ECOLD(I)*(ONE+(GAMMA-ONE)*MU(I)*(ONE-MU(I)))
       ENDDO

      ELSEIF(IFLAG == 1) THEN
C----------------------------------------
C     UPDATE PRESSURE AND INTERNAL ENERGY
C----------------------------------------
       TFEXTT       = ZERO
       MX           = MAT(1)
       E0           = PM(23,MX)
       GAMMA        = PM(34,MX)
       P0           = PM(32,MX)
       PSH(LFT:LLT) = PM(88,MX)
       PSTAR        = PM(35,MX)
       SPH          = PM(69,MX)
       PC           = PM(37, MX)
       DO I=LFT,LLT
         AA      = -GAMMA*PSTAR-PSH(I)
         BB      = (GAMMA-ONE)*(ONE+MU(I))
         DVV     = HALF*DVOL(I)*DF(I) / MAX(EM15,VNEW(I))
         PNEW(I) = PNEW(I) + (AA+BB*(ESPE(I)-PSH(I) *DVV))/(ONE+BB*DVV)
         PNEW(I) = MAX(PNEW(I),PC-PSH(I) )*OFF(I)
         EINT(I) = EINT(I) - HALF*DVOL(I)*(PNEW(I)+PSH(I) )
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
          E0           = PM(23,MX)         
          GAMMA        = PM(34,MX)
          P0           = PM(32,MX)
          PSH(1:NEL) = PM(88,MX)
          PSTAR        = PM(35,MX)
          SPH          = PM(69,MX)
          PC = PM(37, MX)
          DO I=1, NEL
             IF (VNEW(I) > ZERO) THEN
                PNEW(I) = (GAMMA-ONE)*(ONE+MU(I))*ESPE(I) - GAMMA*PSTAR
                PNEW(I) = MAX(PNEW(I),PC)*OFF(I)
                !EINT(I) = (PNEW(I) + PSH(I) + GAMMA * PSTAR) / (GAMMA - ONE)
                DPDM(I) = (GAMMA-ONE)*(ESPE(I)+PNEW(I)*DF(I)) !total derivative
                DPDE(I) = (GAMMA-ONE)*(ONE+MU(I)) !partial derivative
                PNEW(I) = PNEW(I)-PSH(I)
             ENDIF
          ENDDO
      ENDIF
C------------------------      
      RETURN
      END
