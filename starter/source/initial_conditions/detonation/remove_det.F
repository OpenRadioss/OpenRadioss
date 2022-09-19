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
C
C   <----NDETPS-------><----NDETSG--------><--NDECRAN--><--NDETPL--><---NDETCORD-->
C   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
C   |  |  |I1|  |  |  |I2|  |  |  |  |  |  |I3|  |  |  |I4|  |  |  |I5|  |  |  |  |
C   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
C          ^           ^                    ^           ^
C
Chd|====================================================================
Chd|  REMOVE_DET                    source/initial_conditions/detonation/remove_det.F
Chd|-- called by -----------
Chd|        READ_DFS_DETCORD              source/initial_conditions/detonation/read_dfs_detcord.F
Chd|        READ_DFS_DETLINE              source/initial_conditions/detonation/read_dfs_detline.F
Chd|        READ_DFS_DETPLAN              source/initial_conditions/detonation/read_dfs_detplan.F
Chd|        READ_DFS_DETPOINT             source/initial_conditions/detonation/read_dfs_detpoint.F
Chd|        READ_DFS_WAVE_SHAPER          source/initial_conditions/detonation/read_dfs_wave_shaper.F
Chd|-- calls ---------------
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|====================================================================
      SUBROUTINE REMOVE_DET(CHAIN,I1    ,I2    ,I3    ,I4    , I5  , DETONATORS)
      USE DETONATORS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*7,intent(in)    :: CHAIN                   !gives which cursor to remove
      INTEGER    ,intent(inout) :: I1,I2,I3,I4,I5          !current cursor
      TYPE(DETONATOR_STRUCT_) :: DETONATORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,I
      INTEGER :: NDETPS,NDETSG,NECRAN,NDETPL,NDETCORD
C-----------------------------------------------
      NDETPS = DETONATORS%NDETPS
      NDETSG = DETONATORS%NDETSG
      NECRAN = DETONATORS%NECRAN
      NDETPL = DETONATORS%NDETPL
      NDETCORD = DETONATORS%NDETCORD

       !-------------------------!
       !        DETPOIN          !
       !-------------------------!
      IF (CHAIN == 'DETPOIN') THEN
	NDETPS=NDETPS-1
	I2=I2-1
	I3=I3-1
	I4=I4-1
	DO I=I1+1,NDETPS+NDETSG+NECRAN+NDETPL+NDETCORD
	  DO J=1,8
	    DETONATORS%BURN(J,I)=DETONATORS%BURN(J,I+1)
	  END DO
	END DO

       !-------------------------!
       !        DETLINE          !
       !-------------------------!
      ELSEIF (CHAIN == 'DETLINE') THEN
        NDETSG=NDETSG-1
	I3=I3-1
	I4=I4-1
	DO I=I2+1,NDETPS+NDETSG+NECRAN+NDETPL+NDETCORD
	  DO J=1,8
	    DETONATORS%BURN(J,I)=DETONATORS%BURN(J,I+1)
	  END DO
	END DO

       !-------------------------!
       !        WAV_SHA          !
       !-------------------------!
      ELSEIF (CHAIN == 'WAV_SHA') THEN
        NECRAN=NECRAN-1
	I4=I4-1
	DO I=I3+1,NDETPS+NDETSG+NECRAN+NDETPL+NDETCORD
	  DO J=1,8
	    DETONATORS%BURN(J,I)=DETONATORS%BURN(J,I+1)
	  END DO
	END DO	

       !-------------------------!
       !        DETPLAN          !
       !-------------------------!
      ELSEIF (CHAIN == 'DETPLAN') THEN
        NDETPL=NDETPL-1
	DO I=I4+1,NDETPS+NDETSG+NECRAN+NDETPL+NDETCORD
	  DO J=1,8
	    DETONATORS%BURN(J,I)=DETONATORS%BURN(J,I+1)
	  END DO
	END DO	

       !-------------------------!
       !        DETCORD          !
       !-------------------------!
      ELSEIF (CHAIN == 'DETCORD') THEN
        NDETCORD=NDETCORD-1
	DO I=I5+1,NDETPS+NDETSG+NECRAN+NDETPL+NDETCORD
	  DO J=1,8
	    DETONATORS%BURN(J,I)=DETONATORS%BURN(J,I+1)
	  END DO
	END DO	

      END IF

      END SUBROUTINE
C-----------------------------------------------


