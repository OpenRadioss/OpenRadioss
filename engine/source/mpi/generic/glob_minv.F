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
C==========================================================================
Chd|====================================================================
Chd|  GLOB_MINV                     source/mpi/generic/glob_minv.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GLOB_MINV(RIN,RINOUT,LEN,TYPE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
#include      "com04_c.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN, TYPE
      my_real
     .        RIN(*), RINOUT(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, L
      INTEGER VLEN
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      VLEN = 3+NVOLU*7 
      DO I = 1,LEN
        RINOUT(1+(I-1)*VLEN) = MIN(RINOUT(1+(I-1)*VLEN),RIN(1+(I-1)*VLEN))
        IF(RIN(1+(I-1)*VLEN) == RINOUT(1+(I-1)*VLEN))THEN
          RINOUT(2+(I-1)*VLEN) = RIN(2+(I-1)*VLEN)
          RINOUT(3+(I-1)*VLEN) = RIN(3+(I-1)*VLEN)
        END IF
        DO L=4,VLEN
          RINOUT(L+(I-1)*VLEN) = RINOUT(L+(I-1)*VLEN)+RIN(L+(I-1)*VLEN)
        END DO
      ENDDO
C
#endif
      RETURN
      END
