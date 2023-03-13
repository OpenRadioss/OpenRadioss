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
C==========================================================================
Chd|====================================================================
Chd|  GLOB_MIN                      source/mpi/generic/glob_min.F 
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GLOB_MIN(RIN,RINOUT,LEN,TYPE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
      INTEGER, PARAMETER ::  VLEN = 10 
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------

      DO I = 1, LEN
         RINOUT(1+(I-1)*VLEN) = MIN(RINOUT(1+(I-1)*VLEN),RIN(1+(I-1)*VLEN))
         IF(RIN(1+(I-1)*VLEN) == RINOUT(1+(I-1)*VLEN))THEN
           RINOUT(2+(I-1)*VLEN) = RIN(2+(I-1)*VLEN)
           RINOUT(3+(I-1)*VLEN) = RIN(3+(I-1)*VLEN)
         END IF
         RINOUT(4+(I-1)*VLEN) = RINOUT(4+(I-1)*VLEN)+RIN(4+(I-1)*VLEN)
         RINOUT(5+(I-1)*VLEN) = RINOUT(5+(I-1)*VLEN)+RIN(5+(I-1)*VLEN)
         RINOUT(6+(I-1)*VLEN) = MIN(RINOUT(6+(I-1)*VLEN),RIN(6+(I-1)*VLEN))
         RINOUT(7+(I-1)*VLEN) = RINOUT(7+(I-1)*VLEN)+RIN(7+(I-1)*VLEN)
         RINOUT(8+(I-1)*VLEN) = MAX(RINOUT(8+(I-1)*VLEN),RIN(8+(I-1)*VLEN))
         RINOUT(9+(I-1)*VLEN) = MAX(RINOUT(9+(I-1)*VLEN),RIN(9+(I-1)*VLEN))
         RINOUT(10+(I-1)*VLEN) = MAX(RINOUT(10+(I-1)*VLEN),RIN(10+(I-1)*VLEN))
C
C        DO L=11,VLEN
C          RINOUT(L+(I-1)*VLEN) = MAX(RINOUT(L+(I-1)*VLEN),RIN(L+(I-1)*VLEN))
C        END DO
      ENDDO
      
C
#endif
      RETURN
      END
