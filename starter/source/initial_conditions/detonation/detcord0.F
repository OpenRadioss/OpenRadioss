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
Chd|  DETCORD0                      source/initial_conditions/detonation/detcord0.F
Chd|-- called by -----------
Chd|        READ_DFS_DETCORD              source/initial_conditions/detonation/read_dfs_detcord.F
Chd|-- calls ---------------
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|====================================================================
      SUBROUTINE DETCORD0(DETONATOR_CORD,ALT,X,VDET2,IOPT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DETONATORS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IOPT
      my_real :: ALT,VDET2,X(3,*)
      TYPE(DETONATOR_CORD_STRUCT_)::DETONATOR_CORD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: J,I,NOD_ID,NNOD
      my_real :: XLP1,XLP2,YLP1,YLP2,ZLP1,ZLP2,D,XL0,YL0,ZL0
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
      IF(IOPT == 0)    RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------


       NNOD = DETONATOR_CORD%NUMNOD

       IF(IOPT == 1)THEN
        DETONATOR_CORD%TDET_PATH(1) = ALT
        DO J=1,NNOD-1
          !first point
          NOD_ID   = DETONATOR_CORD%NODES(J)
          XLP1     = X(1,NOD_ID)
          YLP1     = X(2,NOD_ID)
          ZLP1     = X(3,NOD_ID)
          !second point
          NOD_ID   = DETONATOR_CORD%NODES(J+1)
          XLP2     = X(1,NOD_ID)
          YLP2     = X(2,NOD_ID)
          ZLP2     = X(3,NOD_ID)
          !distance P1-P2
          XL0      = (XLP1-XLP2)                                       
          YL0      = (YLP1-YLP2)                                       
          ZL0      = (ZLP1-ZLP2)        
          D        = XL0**2 + YL0**2 + ZL0**2
          D        = SQRT(D)
          DETONATOR_CORD%TDET_PATH(J+1) = DETONATOR_CORD%TDET_PATH(J) + D/VDET2
        ENDDO
       ELSE
          DETONATOR_CORD%TDET_PATH(1:NNOD) = ALT
       ENDIF


C-----------------------------------------------
      RETURN
      END
