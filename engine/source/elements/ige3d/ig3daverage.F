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
Chd|  IG3DAVERAGE                   source/elements/ige3d/ig3daverage.F
Chd|-- called by -----------
Chd|        IG3DUFORC3                    source/elements/ige3d/ig3duforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IG3DAVERAGE(
     1   SIG,     SIGG,    VOL0,    VOL0G,
     2   RHO,     EINT,    EINTG,   RHOG,
     3   VOL,     VOLG,    EPLAS,   EPLASG,
     4   G_PLA,   EPSD,    EPSDG,   NEL,
     5   ISRAT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ISRAT
      INTEGER G_PLA,NEL
      my_real
     .   SIG(NEL,6), SIGG(NEL,6), VOL(*), VOLG(*) , VOL0(*),
     .   VOL0G(*), EINT(*)  , RHO(*), EINTG(*), RHOG(*),
     .   EPLAS(*), EPLASG(*), EPSD(*),EPSDG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   WIP(MVSIZ)
C-----------------------------------------------
c
      DO I=1,NEL  
       WIP(I) = VOL(I)/VOLG(I)                                       
       SIGG(I,1) = SIGG(I,1) + WIP(I) * SIG(I,1)         
       SIGG(I,2) = SIGG(I,2) + WIP(I) * SIG(I,2)         
       SIGG(I,3) = SIGG(I,3) + WIP(I) * SIG(I,3)         
       SIGG(I,4) = SIGG(I,4) + WIP(I) * SIG(I,4)         
       SIGG(I,5) = SIGG(I,5) + WIP(I) * SIG(I,5)         
       SIGG(I,6) = SIGG(I,6) + WIP(I) * SIG(I,6)         
       RHOG(I)   = RHOG(I)   + WIP(I) * RHO(I)                       
       EINTG(I)  = EINTG(I)  + EINT(I) * VOL0(I)/VOL0G(I)
      ENDDO
      IF (ISRAT > 0) THEN                                                    
        DO I=1,NEL                                                         
          EPSDG(I) = EPSDG(I) + WIP(I) * EPSD(I)                                
        ENDDO                                                                
      ENDIF 
      IF (G_PLA > 0) THEN                                                    
        DO I=1,NEL                                                          
          EPLASG(I)  = EPLASG(I)  + WIP(I) * EPLAS(I)                              
        ENDDO                                                                 
      ENDIF
c
C-----------
      RETURN
      END
