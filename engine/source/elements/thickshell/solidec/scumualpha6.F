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
Chd|  SCUMUALPHA6                   source/elements/thickshell/solidec/scumualpha6.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|-- calls ---------------
Chd|        DTDC_MOD                      share/modules/dtdc_mod.F      
Chd|====================================================================
      SUBROUTINE SCUMUALPHA6(
     1   OFFG,    ALPHA_E, NC1,     NC2,
     2   NC3,     NC4,     NC5,     NC6,
     3   NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DTDC_MOD  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*), NC5(*), NC6(*)
C     REAL
      my_real
     .   OFFG(*),ALPHA_E(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J,N
C-----------------------------------------------
#include "lockon.inc"
      DO I=1,NEL
        IF (OFFG(I)==ZERO) CYCLE
        N = NC1(I)
        ALPHA_DC(N) = MAX(ALPHA_DC(N),ALPHA_E(I))
        N = NC2(I)
        ALPHA_DC(N) = MAX(ALPHA_DC(N),ALPHA_E(I))
        N = NC3(I)
        ALPHA_DC(N) = MAX(ALPHA_DC(N),ALPHA_E(I))
        N = NC4(I)
        ALPHA_DC(N) = MAX(ALPHA_DC(N),ALPHA_E(I))
        N = NC5(I)
        ALPHA_DC(N) = MAX(ALPHA_DC(N),ALPHA_E(I))
        N = NC6(I)
        ALPHA_DC(N) = MAX(ALPHA_DC(N),ALPHA_E(I))
      END DO
#include "lockoff.inc"
C      
      RETURN
      END
