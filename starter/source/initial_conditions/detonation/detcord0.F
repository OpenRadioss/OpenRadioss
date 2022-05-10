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
Chd|  DETCORD0                      source/initial_conditions/detonation/detcord0.F
Chd|-- called by -----------
Chd|        READ_DFS_DETCORD              source/initial_conditions/detonation/read_dfs_detcord.F
Chd|-- calls ---------------
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|====================================================================
      SUBROUTINE DETCORD0(DETONATORS,ITAB,N2,ALT,X,VDET2,IOPT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DETONATORS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "vect01_c.inc"
#include      "units_c.inc"
#include      "scr11_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: ITAB(*),IOPT,N2
      my_real :: ALT,VDET2,X(3,*)
      TYPE(DETONATOR_STRUCT_)::DETONATORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: J,I,II,NDETCORD,NPE,NPE2
      my_real :: XLP1,XLP2,YLP1,YLP2,ZLP1,ZLP2,D,XL0,YL0,ZL0
      
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
      NDETCORD = DETONATORS%NDETCORD
      IF(NDETCORD.EQ.0)RETURN
      IF(IOPT.EQ.0)    RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

        NDETCORD = DETONATORS%NDETCORD
        NPE = DETONATORS%NPE
        NPE2 = DETONATORS%NPE2
        J = 1
        I = N2+J-1
        DETONATORS%DCORD(I)= ALT

       IF(IOPT.EQ.1)THEN
        DO J=1,NPE2-1 
          !first point
          I        = N2+J-1   
          II       = DETONATORS%ICORD(I) 
          XLP1     = X(1,II)  
          YLP1     = X(2,II)  
          ZLP1     = X(3,II)  
          !second point
          I        = I+1
          II       = DETONATORS%ICORD(I)
          XLP2     = X(1,II)       
          YLP2     = X(2,II)
          ZLP2     = X(3,II)
          !distance P1-P2
          XL0      = (XLP1-XLP2)                                       
          YL0      = (YLP1-YLP2)                                       
          ZL0      = (ZLP1-ZLP2)        
          D        = XL0**2 + YL0**2 + ZL0**2
          D        = SQRT(D)
          DETONATORS%DCORD(I) = DETONATORS%DCORD(I-1) + D/VDET2
        ENDDO
       ELSE
          DETONATORS%DCORD(N2+1:N2+NPE2-2) = ALT
       ENDIF


C-----------------------------------------------
      RETURN
      END
