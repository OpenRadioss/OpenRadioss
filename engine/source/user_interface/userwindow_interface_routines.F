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
Chd|  USERWINDOW_ACCESS_DUM         source/user_interface/userwindow_interface_routines.F
Chd|-- called by -----------
Chd|        RADIOSS0                      source/engine/radioss0.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE USERWINDOW_ACCESS_DUM(IERR)
#include      "implicit_f.inc"
      INTEGER IERR
      IERR=0
      END      
C
Chd|====================================================================
Chd|  USERWINDOW_GET_A              source/user_interface/userwindow_interface_routines.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        USER_INTERFACE_MOD            share/modules/user_interface_mod.F
Chd|====================================================================
      SUBROUTINE USERWINDOW_GET_A(A_BUF)
      USE USER_INTERFACE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      DOUBLE PRECISION A_BUF(3,NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      IF(IUSERWI /= 0)THEN
        DO I=1,NUMNOD
          A_BUF(1,I)=A_SAV(1,I)
          A_BUF(2,I)=A_SAV(2,I)
          A_BUF(3,I)=A_SAV(3,I)
        ENDDO      
      ENDIF
      END
Chd|====================================================================
Chd|  USERWINDOW_GET_AR             source/user_interface/userwindow_interface_routines.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        USER_INTERFACE_MOD            share/modules/user_interface_mod.F
Chd|====================================================================
      SUBROUTINE USERWINDOW_GET_AR(AR_BUF)
      USE USER_INTERFACE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      DOUBLE PRECISION AR_BUF(3,NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      IF(IUSERWI /= 0)THEN
        DO I=1,NUMNOD
          AR_BUF(1,I)=AR_SAV(1,I)
          AR_BUF(2,I)=AR_SAV(2,I)
          AR_BUF(3,I)=AR_SAV(3,I)
        ENDDO      
      ENDIF
      END
