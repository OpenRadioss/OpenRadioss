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
Chd|  SMS_BCSCYC                    source/ams/sms_bcscyc.F       
Chd|-- called by -----------
Chd|        SMS_ENCIN_2                   source/ams/sms_encin_2.F      
Chd|        SMS_MASS_SCALE_2              source/ams/sms_mass_scale_2.F 
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|-- calls ---------------
Chd|        ACC_CYCON                     source/constraints/general/bcs/bcscyc.F
Chd|====================================================================
      SUBROUTINE SMS_BCSCYC(IBCSCYC,LBCSCYC,SKEW,X,A)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBCSCYC(4,*),LBCSCYC(2,*)
      my_real
     .     X(3,*),A(3,*),SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,ISK,IAD,NN,N1,N2,ID
      my_real
     .   AX, AY, AZ
C-----------------------------------------------
!$OMP DO SCHEDULE(DYNAMIC,1)
      DO I=1,NBCSCYC
        IAD = IBCSCYC(1,I)+1
        ISK = IBCSCYC(2,I)
        NN  = IBCSCYC(3,I)
        ID  = IBCSCYC(4,I)
        CALL ACC_CYCON(NN,LBCSCYC(1,IAD),ISK,SKEW,X  ,A)
      END DO
!$OMP END DO
C
      RETURN
      END
