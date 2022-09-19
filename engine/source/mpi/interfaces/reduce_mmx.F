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
Chd|====================================================================
Chd|  REDUCE_MMX                    source/mpi/interfaces/reduce_mmx.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REDUCE_MMX(RIN,RINOUT,LEN,TYPE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
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
      INTEGER I, L, I_LEN
      PARAMETER (I_LEN = 46)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      L = 1
      DO I=1,LEN
C X MIN/MAX
        RINOUT(L)   = MAX(RINOUT(L),RIN(L))
        RINOUT(L+1) = MAX(RINOUT(L+1),RIN(L+1))
        RINOUT(L+2) = MAX(RINOUT(L+2),RIN(L+2))
        RINOUT(L+3) = MIN(RINOUT(L+3),RIN(L+3))
        RINOUT(L+4) = MIN(RINOUT(L+4),RIN(L+4))
        RINOUT(L+5) = MIN(RINOUT(L+5),RIN(L+5))

        RINOUT(L+6) = MAX(RINOUT(L+6),RIN(L+6))
        RINOUT(L+7) = MAX(RINOUT(L+7),RIN(L+7))
        RINOUT(L+8) = MAX(RINOUT(L+8),RIN(L+8))

        RINOUT(L+ 9) = MIN(RINOUT(L+9),RIN(L+9))
        RINOUT(L+10) = MIN(RINOUT(L+10),RIN(L+10))
        RINOUT(L+11) = MIN(RINOUT(L+11),RIN(L+11))

        RINOUT(L+12) = MAX(RINOUT(L+12),RIN(L+12))
        RINOUT(L+13) = MAX(RINOUT(L+13),RIN(L+13))
        RINOUT(L+14) = MAX(RINOUT(L+14),RIN(L+14))

        RINOUT(L+15) = MIN(RINOUT(L+15),RIN(L+15))
        RINOUT(L+16) = MIN(RINOUT(L+16),RIN(L+16))
        RINOUT(L+17) = MIN(RINOUT(L+17),RIN(L+17))

        L = L + 18
C
        RINOUT(L)   = MAX(RINOUT(L),RIN(L))
        RINOUT(L+1) = MAX(RINOUT(L+1),RIN(L+1))
        RINOUT(L+2) = MAX(RINOUT(L+2),RIN(L+2))
        RINOUT(L+3) = MIN(RINOUT(L+3),RIN(L+3))
        RINOUT(L+4) = MIN(RINOUT(L+4),RIN(L+4))
        RINOUT(L+5) = MIN(RINOUT(L+5),RIN(L+5))
        RINOUT(L+6) = MAX(RINOUT(L+6),RIN(L+6))
        RINOUT(L+7) = MAX(RINOUT(L+7),RIN(L+7))
        RINOUT(L+8) = MAX(RINOUT(L+8),RIN(L+8))
        RINOUT(L+9) = MIN(RINOUT(L+9),RIN(L+9))
        RINOUT(L+10) = MIN(RINOUT(L+10),RIN(L+10))
        RINOUT(L+11) = MIN(RINOUT(L+11),RIN(L+11))

        L = L + 12
C V MIN/MAX
        RINOUT(L)   = MAX(RINOUT(L),RIN(L))
        RINOUT(L+1) = MAX(RINOUT(L+1),RIN(L+1))
        RINOUT(L+2) = MAX(RINOUT(L+2),RIN(L+2))
        RINOUT(L+3) = MIN(RINOUT(L+3),RIN(L+3))
        RINOUT(L+4) = MIN(RINOUT(L+4),RIN(L+4))
        RINOUT(L+5) = MIN(RINOUT(L+5),RIN(L+5))
        L = L + 6
C
        RINOUT(L)   = MAX(RINOUT(L),RIN(L))
        RINOUT(L+1) = MAX(RINOUT(L+1),RIN(L+1))
        RINOUT(L+2) = MAX(RINOUT(L+2),RIN(L+2))
        RINOUT(L+3) = MIN(RINOUT(L+3),RIN(L+3))
        RINOUT(L+4) = MIN(RINOUT(L+4),RIN(L+4))
        RINOUT(L+5) = MIN(RINOUT(L+5),RIN(L+5))
        L = L + 6
C NEWFRONT / SIZE
        RINOUT(L) = RINOUT(L)+RIN(L)
        L = L + 1
C TZINF
        RINOUT(L) = MIN(RINOUT(L),RIN(L))
        L = L + 1
C TZINF
        RINOUT(L) = MAX(RINOUT(L),RIN(L))
        L = L + 1
C T25 main gap changes with thickness change
        RINOUT(L) = MAX(RINOUT(L),RIN(L))
        L = L + 1
      END DO
C
#endif
      RETURN
      END
