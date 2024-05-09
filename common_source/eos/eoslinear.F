Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  EOSLINEAR                     eos/eoslinear.F               
Chd|-- called by -----------
Chd|        EOSMAIN                       common_source/eos/eosmain.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE EOSLINEAR(IFLAG,NEL, PM   ,OFF  ,EINT ,MU   ,MU2  , 
     2                     DVOL ,DF   ,VNEW ,MAT  ,PNEW ,DPDM ,
     3                     DPDE ,THETA,ECOLD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(NEL), IFLAG, NEL
      my_real PM(NPROPM,NUMMAT), 
     .        OFF(NEL) ,EINT(NEL) ,MU(NEL)   , 
     .        MU2(NEL) ,DVOL(NEL) ,DF(NEL)   ,VNEW(NEL),
     .        PNEW(NEL),DPDM(NEL) ,DPDE(NEL) ,THETA(NEL),
     .        ECOLD(NEL)
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
      my_real BULK(MVSIZ),PC(MVSIZ),SPH(MVSIZ),C0(MVSIZ),PSH(MVSIZ)
C-----------------------------------------
      IF(IFLAG == 0) THEN
C-----------------------------------------
C     COMPUTE BULK MODULUS FOR SOUND SPEED
C-----------------------------------------
       DO I=1,NEL
        MX = MAT(I)
        BULK(I) = PM(32,MX)
        SPH(I) = PM(69,MX)
        C0(I) = PM(104,MX)
        PC(I) = PM(37,MX)
        PSH(I) = PM(88,MX)
       ENDDO
       DO I=1,NEL
        DPDM(I) = BULK(I)
        DPDE(I) = ZERO
        PNEW(I) = C0(I)+BULK(I)*MU(I)
        PNEW(I) = MAX(PC(I)-PSH(I),PNEW(I))        !law10 : yield criteria depends on P
        PNEW(I) = PNEW(I)*OFF(I)
       ENDDO
C----------------------------
C     COLD COMPRESSION ENERGY
C----------------------------
       DO I=1,NEL
        ECOLD(I)=-THREE100*SPH(I)
        IF(MU(I) > ZERO) THEN
          ECOLD(I)=ECOLD(I)+HALF*BULK(I)*MU2(I)
        ENDIF
       ENDDO
C------------------------------------------------
      ELSEIF(IFLAG == 1) THEN
C----------------------------------------
C     UPDATE PRESSURE AND INTERNAL ENERGY
C----------------------------------------
       DO I=1,NEL
        MX     = MAT(I)
        BULK(I)= PM(32,MX)
        PC(I)  = PM(37,MX)
        C0(I)  = PM(104,MX)
        PSH(I) = PM(88,MX)
       ENDDO
       DO I=1,NEL
        PNEW(I) = C0(I)+BULK(I)*MU(I)
        PNEW(I) = MAX(PNEW(I),PC(I)-PSH(I))
        PNEW(I) = PNEW(I)*OFF(I)
        EINT(I) = EINT(I) - HALF*DVOL(I)*(PNEW(I)+PSH(I))
       ENDDO
C------------------------
C     COMPUTE TEMPERATURE
C------------------------
       DO I=1,NEL
        MX     = MAT(I)
        SPH(I) = PM(69,MX)
        PSH(I) = PM(88,MX)
       ENDDO
       DO I=1,NEL
        IF(OFF(I)<1..OR.SPH(I)<=0.) CYCLE
        THETA(I)=(DF(I)*EINT(I)/VNEW(I)-ECOLD(I))/SPH(I)
        THETA(I)=MAX(ZERO,THETA(I))
       ENDDO
C------------------------------------------------
      ELSEIF (IFLAG == 2) THEN
        DO I=1, NEL                                
           MX = MAT(I)  
           C0(I) = PM(104,MX)                                    
           BULK(I) = PM(32,MX)                       
           PC(I) = PM(37,MX)                       
           PSH(I) = PM(88,MX)                       
        ENDDO                                      
        DO I=1, NEL                                
           IF (VNEW(I) > ZERO) THEN             
              PNEW(I) = C0(I)+BULK(I)*MU(I)        
              PNEW(I) = MAX(PNEW(I),PC(I)-PSH(I))  
              PNEW(I) = PNEW(I)*OFF(I)             
              DPDM(I) = BULK(I)                    
              DPDE(I) = ZERO                       
           ENDIF                                   
        ENDDO                                      
      ENDIF
      RETURN
      END
