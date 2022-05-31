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
Chd|  VOLP_LFLUID                   source/airbag/volp_lfluid.F   
Chd|-- called by -----------
Chd|        MONVOL0                       source/airbag/monvol0.F       
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE VOLP_LFLUID(IVOLU, RVOLU, VOL, FSAV, NPC, TF, PMAIN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPC(*),IVOLU(*), PMAIN
C
      my_real
     .   TF(*), RVOLU(*), FSAV(*), VOL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IFUNCT
C
      my_real
     .   P0, VOL0, DYDX, AREA, VEPS, VINC, POLD, PEXT, VOLD, PRES,
     .   Q,  QOLD, DV,   XFUN
      my_real SCALEF, BULK, PMAX, GMASS, DMASS, RHOI
      my_real FINTER 
      EXTERNAL FINTER
C-----------------------------------------------
C
C CALCUL DE LA PRESSION
C
      PEXT   =RVOLU(3)
      VOL0   =RVOLU(4)
      VINC   =RVOLU(5)
      POLD   =RVOLU(12)
      VOLD   =RVOLU(16)
      VEPS   =RVOLU(17)
      AREA   =RVOLU(18)
      QOLD   =RVOLU(23)

      GMASS  =RVOLU(20)
      RHOI   =RVOLU(34)

C   BULK
      IFUNCT = IVOLU(21)
      SCALEF = RVOLU(35)
      IF(IFUNCT == 0) THEN
         BULK = SCALEF	  
      ELSE
         BULK = SCALEF * FINTER(IFUNCT,TT,NPC,TF,DYDX)
      ENDIF

C   FLUID MASS IN
      IFUNCT = IVOLU(22)
      SCALEF = RVOLU(36)
      IF(IFUNCT == 0) THEN
         DMASS = SCALEF * DT1
      ELSE
         DMASS = SCALEF * FINTER(IFUNCT,TT,NPC,TF,DYDX) * DT1
      ENDIF 
      GMASS = GMASS + DMASS
      RVOLU(54)=RVOLU(54)+DMASS

C   FLUID MASS OUT (time)
      IFUNCT = IVOLU(23)
      SCALEF = RVOLU(37)
      IF(IFUNCT == 0) THEN
         DMASS = SCALEF * DT1
      ELSE
         DMASS = SCALEF * FINTER(IFUNCT,TT,NPC,TF,DYDX) * DT1
      ENDIF 
      GMASS = GMASS - DMASS

C   FLUID MASS OUT (pressure)
      IFUNCT = IVOLU(24)
      SCALEF = RVOLU(38)
      IF(IFUNCT == 0) THEN
         DMASS = SCALEF * DT1
      ELSE
         DMASS = SCALEF * FINTER(IFUNCT,POLD,NPC,TF,DYDX) * DT1
      ENDIF 
      GMASS = GMASS - DMASS

C   P0
      IFUNCT = IVOLU(25)
      SCALEF = RVOLU(39)
      IF(IFUNCT == 0) THEN
         P0 = SCALEF	  
      ELSE
         P0 = SCALEF * FINTER(IFUNCT,TT,NPC,TF,DYDX)
      ENDIF

C   PMAX
      IFUNCT = IVOLU(26)
      SCALEF = RVOLU(40)
      IF(IFUNCT == 0) THEN
         PMAX = SCALEF	  
      ELSE
         PMAX = SCALEF * FINTER(IFUNCT,TT,NPC,TF,DYDX)
      ENDIF

      VOL0 = GMASS / RHOI
      VOL = VOL + VEPS
      XFUN = (VOL0-VINC)/(VOL-VINC)
      PRES = BULK * MAX(ZERO,LOG(XFUN)) + P0
      PRES = MIN(PRES,PMAX)
      Q = ZERO

      IF (ISPMD+1.EQ.PMAIN) THEN
        DV=VOL-VOLD
        TFEXT=TFEXT+(HALF*(Q+QOLD+PRES+POLD))*DV
        FSAV(1)=GMASS
        FSAV(2)=VOL
        FSAV(3)=PRES
        FSAV(4)=AREA
        FSAV(5)=ZERO
        FSAV(6)=ZERO
        FSAV(7)=ZERO
        FSAV(8)=ZERO
        FSAV(9)=ZERO	
        FSAV(10)=ZERO
        FSAV(11)=ZERO
        FSAV(12)=ZERO
        FSAV(13)=ZERO
        FSAV(14)=ZERO
        FSAV(15)=RVOLU(54)
        FSAV(16)=ZERO
      ENDIF
C
      RVOLU(16)=VOL
      RVOLU(12)=PRES
      RVOLU(20)=GMASS
      RVOLU(23)=Q
C
      RETURN
      END
