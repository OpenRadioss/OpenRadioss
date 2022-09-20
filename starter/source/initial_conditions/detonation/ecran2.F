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
Chd|  ECRAN2                        source/initial_conditions/detonation/ecran2.F
Chd|-- called by -----------
Chd|        M5IN2                         source/initial_conditions/detonation/m5in2.F
Chd|        M5IN2T                        source/initial_conditions/detonation/m5in2t.F
Chd|-- calls ---------------
Chd|        IOMBR                         source/initial_conditions/detonation/iombr.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|====================================================================
      SUBROUTINE ECRAN2(DETONATORS,N1,NP1,X,VDET)
C-----------------------------------------------
      USE DETONATORS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real  X(3,NUMNOD)
      my_real,INTENT(IN)::VDET
      TYPE(DETONATOR_STRUCT_),TARGET ::DETONATORS
      INTEGER,INTENT(IN) :: N1,NP1
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr11_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER,POINTER,DIMENSION(:) :: IECR, IORDR, IFLG
      my_real,POINTER,DIMENSION(:) ::  DTIME
      INTEGER J, I, II, NPE
      my_real DDMX, DTOS
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER,EXTERNAL :: IOMBR
C-----------------------------------------------

      NPE=DETONATORS%NPE
      IECR => DETONATORS%IECRAN(N1:N1+NPE-1)
      IORDR => DETONATORS%IECRAN(NP1:NP1+NPE-1) 
      DTIME => DETONATORS%DECRAN(1:)   
            
C---------------------------------------
C     COMPUTING DETONATION:TIME FOR ONE ELEMENT (CENTROID)
C---------------------------------------
C     1) DETONATION ORIGIN: USER DETONATION POINT
C         check if the target can be directly lightened from detonation origin.
C            yes if no crossing with the screen (IOMBR returns 0)
C             no otherwise (IOMBR return 1)
C---------------------------------------
      DDMX=EP20
      IF(IOMBR(DETONATORS,X,IECR,DDMX,VDET) == 0)RETURN   !Vdet is optional velocity from origin (default is material velocity VDTO)

C-----------------------------------------
C     2) DETONATION ORIGIN:EACH POINT OF THE SCREEN PATH
C-----------------------------------------
      DTOS=EP20
      
      NPE=DETONATORS%NPE
      DO J=1,NPE
        I=IORDR(J)
        II=IECR(I)
        YD=X(2,II)
        ZD=X(3,II)
        DTO0=DTIME(I)
        IF(IOMBR(DETONATORS,X,IECR,DDMX,VDTO) == 0)THEN
          DTOS= MIN(DTOS,DTO) !keep minimum value (first arrival time)
        ENDIF
      END DO !J=1,NPE
      
      DTO=DTOS
      
      RETURN
      END
