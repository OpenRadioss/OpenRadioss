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
C
Chd|====================================================================
Chd|  W_LSENS                       source/restart/ddsplit/w_lsens.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE W_LSENS(LSENSOR,NSENSOR,NSENSI,NODLOCAL,LEN_IA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSENSOR, LEN_IA, NSENSI,
     .        LSENSOR(NSENSI,NSENSOR), NODLOCAL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N2,LSENSTMP(NSENSI,NSENSOR), TYP
C=======================================================================
      DO I = 1, NSENSOR
        DO J = 1, NSENSI
          LSENSTMP(J,I) = LSENSOR(J,I)
        ENDDO
        TYP = LSENSOR(2,I)
        N2  = LSENSOR(5,I)
        IF (TYP == 2) THEN
          LSENSTMP(4,I) = NODLOCAL(LSENSOR(4,I))
          LSENSTMP(5,I) = NODLOCAL(LSENSOR(5,I))
        ELSE IF (TYP == 13) THEN
          LSENSTMP(4,I) = NODLOCAL(LSENSOR(4,I))
          IF (N2 > 0) LSENSTMP(5,I) = NODLOCAL(LSENSOR(5,I))
        ELSE IF (TYP == 16) THEN   ! /sensor/dist_surf
        ENDIF
      ENDDO
      CALL WRITE_I_C(LSENSTMP,NSENSI*NSENSOR)
      LEN_IA = LEN_IA + NSENSI*NSENSOR
C
      RETURN
      END
