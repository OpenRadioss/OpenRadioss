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
      !||    parameters_mod         ../common_source/modules/interfaces/parameters_mod.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_inter_type24   ../starter/source/interfaces/int24/hm_read_inter_type24.F
      !||    hm_read_inter_type25   ../starter/source/interfaces/int25/hm_read_inter_type25.F
      !||    i24cor3                ../engine/source/interfaces/int24/i24cor3.F
      !||    i24for3                ../engine/source/interfaces/int24/i24for3.F
      !||    i24main_tri            ../engine/source/interfaces/intsort/i24main_tri.F
      !||    i25cor3_3              ../engine/source/interfaces/int25/i25cor3.F
      !||    i25cor3_e2s            ../engine/source/interfaces/int25/i25cor3_e2s.F
      !||    i25cor3e               ../engine/source/interfaces/int25/i25cor3e.F
      !||    i25for3                ../engine/source/interfaces/int25/i25for3.F
      !||    i25main_slid           ../engine/source/interfaces/int25/i25main_slid.F
      !||    i25main_tri            ../engine/source/interfaces/intsort/i25main_tri.F
      !||    i25surfi               ../starter/source/interfaces/inter3d1/i25surfi.F
      !||    rdcomi                 ../engine/source/output/restart/rdcomm.F
      !||    rdcomr                 ../engine/source/output/restart/rdcomm.F
      !||    spmd_fiadd_poff        ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_fiadd_pon         ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_initfi            ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_savefi            ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    wrcomi                 ../engine/source/output/restart/wrcomm.F
      !||    wrcomip                ../starter/source/restart/ddsplit/wrcommp.F
      !||    wrcomr                 ../engine/source/output/restart/wrcomm.F
      !||====================================================================
        MODULE PARAMETERS_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
       implicit  none
!   -----------------------------------------------
!   D e r i v e d   T y p e   D e f i n i t i o n s
!   -----------------------------------------------

!   -------------------------

C----------------------------------------------
C   Global Parameters activated by Contact
C---------------------------------------------

          TYPE PARAMETERS_
             integer  ISTIF_DT      ! stiffness based on Mass and DT computation flag
             integer  INTCAREA      ! flag for contact area computation for output
             integer  INT25_EROSION_SOLID ! flag for considering erosion of solids for Inter25
             my_real  DT_STIFINT    ! time step for stiffness based on Mass and DT computation 
             my_real, DIMENSION(:) , ALLOCATABLE ::   INTAREAN ! nodal areas for total contact area output
          END TYPE PARAMETERS_

C
        END MODULE PARAMETERS_MOD
