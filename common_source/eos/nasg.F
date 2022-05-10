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
Chd|  NASG                          eos/nasg.F                    
Chd|-- called by -----------
Chd|        EOSMAIN                       common_source/eos/eosmain.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NASG
     1              (IFLAG,NEL  ,PM   ,OFF  ,EINT ,MU  ,MU2 , 
     2               ESPE ,DVOL ,DF   ,VNEW ,MAT  ,PSH ,
     3               PNEW ,DPDM ,DPDE ,THETA,ECOLD)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine contains numerical solving
C of NOBLE ABEL STIFFENED GAS EOS
C 2nd order integration in time
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
      my_real :: P0,GAMMA,T0,E0,SPH,AA, BB,TFEXTT, DVV, PP, PSTAR, PC,V0
      my_real :: Q,Q_,Cv,RHO0,NUM,DENOM,UNPMU,B,RHO
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      !-----------------------------------------
      !     COMPUTE BULK MODULUS FOR SOUND SPEED
      !     COMPUTE COLD COMPRESSION ENERGY
      !-----------------------------------------
      IF(IFLAG == 0) THEN
         MX           = MAT(1)
         E0           = PM(23 ,MX)         
         GAMMA        = PM(34 ,MX)
         P0           = PM(32 ,MX)
         PSH(LFT:LLT) = PM(88 ,MX)
         PSTAR        = PM(160,MX)
         SPH          = PM(69 ,MX)
         Q            = PM(35 ,MX)
         Q_           = PM(36 ,MX)
         B            = PM(161,MX)
         Cv           = PM(162,MX)
         T0           = PM(79, MX)
         RHO0         = PM(1 , MX)
         DO I=LFT,LLT
           UNPMU   =  UN+MU(I)
           DENOM   = (UN-RHO0*b*UNPMU)
           NUM     = (ESPE(I)-RHO0*q)
           PP      = (GAMMA-UN)*UNPMU*NUM/DENOM - GAMMA*PSTAR
           dPdE(I) = (GAMMA-UN)*UNPMU / DENOM                                         !partial derivative    
           DPDM(I) = (GAMMA-UN)*NUM/DENOM/DENOM + DPDE(I)*(PP+PSH(I))/UNPMU/UNPMU     !total derivative
         ENDDO
         DO I=LFT,LLT
           ECOLD(I)=-TROIS100*SPH
           IF(MU(I).GT.ZERO) ECOLD(I)=ECOLD(I)*(UN+(GAMMA-UN)*MU(I)*(UN-MU(I)))
         ENDDO

      !----------------------------------------
      !     UPDATE PRESSURE AND INTERNAL ENERGY
      !----------------------------------------
      ELSEIF(IFLAG == 1) THEN
         TFEXTT       = ZERO      
         MX           = MAT(1)
         E0           = PM(23 ,MX)         
         GAMMA        = PM(34 ,MX)
         P0           = PM(32 ,MX)
         PSH(LFT:LLT) = PM(88 ,MX)
         PSTAR        = PM(160,MX)
         SPH          = PM(69 ,MX)
         Q            = PM(35 ,MX)
         Q_           = PM(36 ,MX)
         B            = PM(161,MX)
         Cv           = PM(162,MX)
         T0           = PM(79, MX)
         RHO0         = PM(1 , MX)
         DO I=LFT,LLT
           EINT(I) = EINT(I) - UNDEMI*DVOL(I)*(PNEW(I)+PSH(I))
           UNPMU   =  UN+MU(I)
           RHO     = UNPMU*RHO0
           V0      = VNEW(I)*RHO/RHO0
           DENOM   = (VNEW(I)/V0-RHO0*b)
           AA      = (GAMMA-UN)*(-RHO0*q/DENOM)-GAMMA*PSTAR
           BB      = (GAMMA-UN)/ DENOM     
           PNEW(I) = (AA+BB*EINT(I)/V0)/(UN+BB*DVOL(I)/DEUX/V0)      
           PNEW(I) = MAX(PNEW(I),-GAMMA*PSTAR)*OFF(I)         
           TFEXTT  = TFEXTT-DVOL(I)*PSH(I)
         ENDDO
#include "atomic.inc"
       TFEXT = TFEXT + TFEXTT
#include "atomend.inc"
         DO I=LFT,LLT
           IF(OFF(I).LT.UN.OR.SPH.LE.ZERO) CYCLE
           UNPMU   =  UN+MU(I)
           RHO     = UNPMU*RHO0
           THETA(I) = (UN/RHO-B)*(PNEW(I)+PSTAR)/Cv/(GAMMA-UN)
           THETA(I) = MAX(ZERO,THETA(I))
         ENDDO
         
       !----------------------------------------
       !     FOR USE WITH MULTULMATERIAL LAW 151
       !----------------------------------------
       ELSEIF(IFLAG == 2) THEN
         MX           = MAT(1)
         E0           = PM(23 ,MX)         
         GAMMA        = PM(34 ,MX)
         P0           = PM(32 ,MX)
         PSH(LFT:LLT) = PM(88 ,MX)
         PSTAR        = PM(160,MX)
         SPH          = PM(69 ,MX)
         Q            = PM(35 ,MX)
         Q_           = PM(36 ,MX)
         B            = PM(161,MX)
         Cv           = PM(162,MX)
         T0           = PM(79, MX)
         RHO0         = PM(1 , MX)
         PC           = PM(37, MX)
          DO I=1, NEL
             IF (VNEW(I) .GT. ZERO) THEN
                UNPMU   =  UN+MU(I)
                DENOM   = (UN-RHO0*b*UNPMU)
                NUM     = (ESPE(I)-RHO0*q)
                PNEW(I) = -PSH(I) +  (GAMMA-UN)*UNPMU*NUM/DENOM - GAMMA*PSTAR
                PNEW(I) = MAX(PNEW(I),MAX(PC, -GAMMA*PSTAR))*OFF(I)
                dPdE(I) = (GAMMA-UN)*UNPMU / DENOM                                              !partial derivative    
                DPDM(I) = (GAMMA-UN)*NUM/DENOM/DENOM + dPdE(I)*(PNEW(I)+PSH(I))/UNPMU/UNPMU     !total derivative
             ENDIF
          ENDDO
      ENDIF
      
C-----------------------------------------------
      RETURN
      END
