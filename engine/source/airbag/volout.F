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
Chd|  VOLOUT                        source/airbag/volout.F        
Chd|-- called by -----------
Chd|        MONVOL0                       source/airbag/monvol0.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VOLOUT(RVOLU,VOL,FSAV,PMAIN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PMAIN
      my_real
     .   RVOLU(*),FSAV(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   VOL,AREA
C-----------------------------------------------
      IF (ISPMD+1==PMAIN) THEN
        AREA = RVOLU(18)
C
        FSAV(1)=ZERO
        FSAV(2)=VOL
        FSAV(3)=ZERO
        FSAV(4)=AREA
        FSAV(5)=ZERO
        FSAV(6)=ZERO
        FSAV(7)=ZERO
        FSAV(8)=ZERO
        FSAV(9)=ZERO
        FSAV(10)=ZERO
        FSAV(11)=ZERO
        FSAV(12)=ZERO	
      ENDIF
C
      RETURN
      END
