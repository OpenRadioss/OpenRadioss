Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    spmd_barrier    ../engine/source/mpi/generic/spmd_barrier.F
      !||--- called by ------------------------------------------------------
      !||    check_nan_acc   ../engine/source/output/outfile/check_nan_acc.F
      !||    inttri          ../engine/source/interfaces/intsort/inttri.F
      !||    resol           ../engine/source/engine/resol.F
      !||    sphprep         ../engine/source/elements/sph/sphprep.F
      !||    sphtri0         ../engine/source/elements/sph/sphtri0.F
      !||    thermbilan      ../engine/source/constraints/thermic/thermbilan.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE SPMD_BARRIER()
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER ierror
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      call MPI_BARRIER(SPMD_COMM_WORLD,ierror)
C
#endif
      RETURN
      END
