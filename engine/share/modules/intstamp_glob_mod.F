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
      !||====================================================================
      !||    intstamp_glob_mod     ../engine/share/modules/intstamp_glob_mod.F
      !||--- called by ------------------------------------------------------
      !||    i21buce               ../engine/source/interfaces/intsort/i21buce.F
      !||    i21cor3               ../engine/source/interfaces/int21/i21cor3.F
      !||    lectur                ../engine/source/input/lectur.F
      !||    radioss2              ../engine/source/engine/radioss2.F
      !||    rdresb                ../engine/source/output/restart/rdresb.F
      !||    resol                 ../engine/source/engine/resol.F
      !||    restalloc             ../engine/source/output/restart/arralloc.F
      !||    spmd_i21fthecom       ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_i21tempcom       ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_ifront_stamp     ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_initfi           ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_savefi           ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    update_struct_int21   ../engine/source/interfaces/int21/update_struct_int21.F
      !||    wrrestp               ../engine/source/output/restart/wrrestp.F
      !||--- uses       -----------------------------------------------------
      !||    intstamp_mod          ../engine/share/modules/intstamp_mod.F
      !||====================================================================
      MODULE INTSTAMP_GLOB_MOD
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTSTAMP_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
      TYPE int_pointer
        INTEGER, DIMENSION(:), POINTER :: P
      END TYPE int_pointer

      TYPE real_pointer
        my_real, DIMENSION(:), POINTER :: P
      END TYPE real_pointer

      TYPE(INTSTAMP_DATA), DIMENSION(:), ALLOCATABLE :: INTSTAMP
      INTEGER,
     .       DIMENSION(:), ALLOCATABLE :: INDEXCONT, TAGCONT
      my_real,
     .       DIMENSION(:,:), ALLOCATABLE :: FCONTG, FNCONTG, FTCONTG, PSKIDS

C SPMD STRUCTURES :
C NMNFI : Tab of pointers siz NINSTAMP => number of remote main node in NMVFI 
C          by SPMD domain
C NMVFI/NMVSI : Tab of pointers => remote main node 
C TEMPNOD : Tab of pointers => Local nodes corresponding to main node in NMVFI
C NTEMP : Tab of pointers => Temperature of main node in NMVFI
      TYPE(int_pointer), DIMENSION(:), ALLOCATABLE :: 
     .  NMNFI,NMNSI,NMVFI,NMVSI,TEMPNOD

      TYPE(real_pointer), DIMENSION(:), ALLOCATABLE :: 
     . NMTEMP

      END MODULE INTSTAMP_GLOB_MOD
