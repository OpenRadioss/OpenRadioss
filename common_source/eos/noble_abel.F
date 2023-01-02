Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  NOBLE_ABEL                    eos/noble_abel.F              
Chd|-- called by -----------
Chd|        EOSMAIN                       common_source/eos/eosmain.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NOBLE_ABEL(IFLAG,NEL, PM   ,OFF  ,EINT ,MU   ,MU2 , 
     2                      ESPE ,DVOL ,DF   ,VNEW ,MAT  ,PSH ,
     3                      PNEW ,DPDM ,DPDE ,THETA,ECOLD)
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
     .   VNEW(*) ,PSH(*)  ,PNEW(*) ,DPDM(*),
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
      my_real
     .   GM1(MVSIZ),TFEXTT, DVV, PP,BB_(MVSIZ),RHO0(MVSIZ)
      my_real
     .   C0(MVSIZ),C2(MVSIZ),C3(MVSIZ),C5(MVSIZ),
     .   SPH(MVSIZ), BR0,DENOM,BB
C--------------------------------------------------------------------
      IF(IFLAG == 0) THEN
C-----------------------------------------
C     COMPUTE BULK MODULUS FOR SOUND SPEED
C     COMPUTE COLD COMPRESSION ENERGY
C-----------------------------------------

      ! PM(23) =  E0
      ! PM(32) =  BB
      ! PM(33) =  GAMMA-ONE     
      ! PM(34) =  RHO0         
      ! PM(88) =  PSH

       DO I=LFT,LLT
        MX      = MAT(I)
        BB_(I)  = PM(32,MX)
        GM1(I)  = PM(33,MX)
        RHO0(I) = PM(34,MX)
        PSH(I)  = PM(88,MX)
        SPH(I)  = PM(69,MX)      
       ENDDO
    
       DO I=LFT,LLT
        DENOM   = ONE-BB_(I)*RHO0(I)*(ONE+MU(I))
        PP      = ESPE(I)*GM1(I)*(MU(I)+ONE)/DENOM 
        DPDE(I) = GM1(I)*(ONE+MU(I))/DENOM
        DPDM(I) = GM1(I)*ESPE(I)/DENOM + PP/DENOM*BB_(I)*RHO0(I) + PP*DF(I)*DF(I)*DPDE(I)      
       ENDDO
C
       DO I=LFT,LLT
        ECOLD(I)=-THREE100*SPH(I)
        IF(MU(I)>ZERO) THEN
          ECOLD(I)=ECOLD(I)*(ONE+GM1(I)*MU(I)*(ONE-MU(I))) 
        ENDIF
       ENDDO
C
      ELSEIF(IFLAG == 1) THEN
C----------------------------------------
C     UPDATE PRESSURE AND INTERNAL ENERGY
C----------------------------------------
       TFEXTT   = ZERO
       DO I=LFT,LLT
        MX      = MAT(I)
        BB_(I)  = PM(32,MX)
        GM1(I)  = PM(33,MX)
        RHO0(I) = PM(34,MX)
        PSH(I)  = PM(88,MX)
        SPH(I)  = PM(69,MX)
       ENDDO
C
       DO I=LFT,LLT

        DENOM   = ONE-BB_(I)*RHO0(I)*(ONE+MU(I))
        BB      = GM1(I)*(ONE+MU(I))/DENOM
        DVV     = HALF*DVOL(I)*DF(I) / MAX(EM15,VNEW(I))
        PNEW(I) = (BB*ESPE(I))/(ONE+BB*DVV)
        PNEW(I) = PNEW(I)*OFF(I)
        EINT(I) = EINT(I) - HALF*DVOL(I)*PNEW(I)
        TFEXTT  = TFEXTT-DVOL(I)*PSH(I)
        PNEW(I) = PNEW(I)-PSH(I)
       ENDDO
#include "atomic.inc"
       TFEXT = TFEXT + TFEXTT
#include "atomend.inc"
C
C------------------------
C     COMPUTE TEMPERATURE
C------------------------
       DO I=LFT,LLT
        MX     =MAT(I)
        SPH(I) =PM(69,MX)
       ENDDO
       DO I=1,NEL
        IF(OFF(I)<1..OR.SPH(I)<=0.) CYCLE
        THETA(I) = (DF(I)*EINT(I)/VNEW(I)-ECOLD(I))/SPH(I)
        THETA(I) = MAX(ZERO,THETA(I))
       ENDDO
      ELSEIF(IFLAG == 2) THEN
         DO I=1, NEL
            MX      = MAT(I)
            BB_(I)  = PM(32,MX)
            GM1(I)  = PM(33,MX)
            RHO0(I) = PM(34,MX)
            PSH(I)  = PM(88,MX)
            SPH(I)  = PM(69,MX)      
         ENDDO
         
         DO I=1, NEL
            IF (VNEW(I) > ZERO) THEN
               DENOM   = ONE-BB_(I)*RHO0(I)*(ONE+MU(I))
               PP      = ESPE(I)*GM1(I)*(MU(I)+ONE)/DENOM 
               DPDE(I) = GM1(I)*(ONE+MU(I))/DENOM
               DPDM(I) = GM1(I)*ESPE(I)/DENOM + PP/DENOM*BB_(I)*RHO0(I) + PP*DF(I)*DF(I)*DPDE(I) 
               PNEW(I) = PP-PSH(I)
            ENDIF
         ENDDO
      ENDIF
      RETURN
      END
