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
Chd|  UPGRADE_IXINT                 source/interfaces/interf1/upgrade_ixint.F
Chd|-- called by -----------
Chd|        I20INI3                       source/interfaces/inter3d1/i20ini3.F
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        IXINTMOD                      share/modules1/restart_mod.F  
Chd|====================================================================
      SUBROUTINE UPGRADE_IXINT(SIXINT,NELEMINT,NEW_SIZE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IXINTMOD
C-------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SIXINT,NELEMINT,NEW_SIZE

      INTEGER, DIMENSION(:), ALLOCATABLE :: TEMP

      ALLOCATE(TEMP(NELEMINT*6))
      TEMP(1:6*NELEMINT)=IXINT(1:6*NELEMINT)
      DEALLOCATE (IXINT)

      SIXINT = (NELEMINT+NEW_SIZE)*6
      ALLOCATE(IXINT(SIXINT))
      IXINT(1:6*NELEMINT)=TEMP(1:6*NELEMINT)

      DEALLOCATE(TEMP)
      END
C
