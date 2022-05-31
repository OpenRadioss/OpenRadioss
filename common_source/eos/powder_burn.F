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
Chd|  POWDER_BURN                   eos/powder_burn.F             
Chd|-- called by -----------
Chd|        EOSMAIN                       common_source/eos/eosmain.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE POWDER_BURN    (
     1                            IFLAG,NEL  ,PM   ,OFF  ,EINT ,MU   ,MU2 , 
     2                            ESPE ,DVOL ,DF   ,VNEW ,MAT  ,PSH  ,
     3                            PNEW ,DPDM ,DPDE ,THETA,ECOLD)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine contains numerical solving
C of LSZK EOS
C Landau Stanyukovich Zeldovich Kompaneet
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
      my_real :: P0,PSH(MVSIZ),GAMMA,T0,E0,SPH,AA, BB,TFEXTT, DVV, PP, AR0B, B
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF(IFLAG == 0) THEN
C-----------------------------------------
C     COMPUTE BULK MODULUS FOR SOUND SPEED
C     COMPUTE COLD COMPRESSION ENERGY
C-----------------------------------------
       MX           = MAT(LFT)
       E0           = PM(23,MX)         
       GAMMA        = PM(34,MX)
       P0           = PM(32,MX)
       PSH(LFT:LLT) = PM(88,MX)
       AR0B         = PM(35,MX)
       B            = PM(36,MX)
       SPH          = PM(69,MX)

       DO I=LFT,LLT
         PP      = (GAMMA-ONE)*(ONE+MU(I))*ESPE(I) + AR0B*EXP(B*LOG(1+MU(I)))
         DPDM(I) = (GAMMA-ONE)*ESPE(I)+AR0B*B/(ONE+MU(I))*EXP(B*LOG(1+MU(I)))+(GAMMA-1)*DF(I)*(PP+PSH(I))   !total derivative
         DPDE(I) = (GAMMA-ONE)*(ONE+MU(I))                                                                  !partial derivative
       ENDDO

       DO I=LFT,LLT
         ECOLD(I)=-THREE100*SPH
         !IF(MU(I).GT.ZERO) ECOLD(I)=
       ENDDO

      ELSEIF(IFLAG == 1) THEN
C----------------------------------------
C     UPDATE PRESSURE AND INTERNAL ENERGY
C----------------------------------------
       TFEXTT       = ZERO
       MX           = MAT(LFT)
       E0           = PM(23,MX)         
       GAMMA        = PM(34,MX)
       P0           = PM(32,MX)
       PSH(LFT:LLT) = PM(88,MX)
       AR0B         = PM(35,MX)
       B            = PM(36,MX)
       SPH          = PM(69,MX)

       DO I=LFT,LLT
         AA         = (GAMMA-ONE)*(ONE+MU(I))
         BB         = AR0B*EXP(B*LOG(1+MU(I)))
         DVV        = HALF*DVOL(I)*DF(I) / MAX(EM15,VNEW(I))
         PNEW(I)    = (BB + AA*(ESPE(I)-PSH(I)*DVV) ) / (ONE+AA*DVV)         
         PNEW(I)    = MAX(PNEW(I),ZERO)*OFF(I)
         EINT(I)    = EINT(I) - HALF*DVOL(I)*(PNEW(I)+PSH(I) )
         PNEW(I)    = PNEW(I) + PSH(I)
         TFEXTT     = TFEXTT-DVOL(I)*PSH(I)
       ENDDO
       
#include "atomic.inc"
       TFEXT = TFEXT + TFEXTT
#include "atomend.inc"
C
C------------------------
C     COMPUTE TEMPERATURE
C------------------------

         MX         = MAT(LFT)
         SPH        = PM(69,MX)

       DO I=LFT,LLT
         IF(OFF(I).LT.1..OR.SPH.LE.0.) CYCLE
         THETA(I) = (DF(I)*EINT(I)/VNEW(I)-ECOLD(I))/SPH
         THETA(I)   = MAX(ZERO,THETA(I))
       ENDDO
      ELSEIF (IFLAG == 2) THEN
         MX           = MAT(LFT)
         E0           = PM(23,MX)         
         GAMMA        = PM(34,MX)
         P0           = PM(32,MX)
         PSH(1:NEL) = PM(88,MX)
         AR0B         = PM(35,MX)
         B            = PM(36,MX)
         SPH          = PM(69,MX)
         
         DO I=1, NEL
            IF (VNEW(I) .GT. ZERO) THEN
               PNEW(I) = (GAMMA-ONE)*(ONE+MU(I))*ESPE(I) + AR0B*EXP(B*LOG(1+MU(I)))
               DPDM(I) = (GAMMA-ONE)*ESPE(I)+AR0B*B/(ONE+MU(I))*EXP(B*LOG(1+MU(I)))+
     .              (GAMMA-1)*DF(I)*(PNEW(I)+PSH(I)) !total derivative
               DPDE(I) = (GAMMA-ONE)*(ONE+MU(I)) !partial derivative
            ENDIF
         ENDDO
      ENDIF
C------------------------      
      RETURN
      END
