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
Chd|  INPOINT                       source/interfaces/interf1/inpoint.F
Chd|-- called by -----------
Chd|        LECINS                        source/interfaces/interf1/lecins.F
Chd|        LECINT                        source/interfaces/interf1/lecint.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INPOINT(MSN,NOINT,BRICS,ITAB,MSV)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
      INTEGER NOINT,MSN
      INTEGER BRICS(*), ITAB(*),MSV(*)
      INTEGER I, J
C------------------------------------------------------------
C       INPUT PAR NOEUDS
C------------------------------------------------------------
        DO J=1,MSN
          MSV(J)=BRICS(J)
        ENDDO
        IF(IPRI>=1) THEN
          WRITE(IOUT,'(/,A,/)')' NODES USED FOR SURFACE DEFINITION'
          WRITE(IOUT,FMT=FMW_10I)(ITAB(MSV(I)),I=1,MSN)
        ENDIF
C------------------------------------------------------------
      RETURN
      END
