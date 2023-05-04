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
Chd|  PARAMETERS_MOD                modules/interfaces/parameters_mod.F
Chd|-- called by -----------
Chd|        INTERFACES_MOD                common_source/modules/interfaces/interfaces_mod.F
Chd|        HM_READ_INTER_TYPE24          starter/source/interfaces/int24/hm_read_inter_type24.F
Chd|        HM_READ_INTER_TYPE25          starter/source/interfaces/int25/hm_read_inter_type25.F
Chd|        WRCOMIP                       starter/source/restart/ddsplit/wrcommp.F
Chd|        WRCOMR                        starter/source/restart/ddsplit/wrcomm.F
Chd|        I24COR3                       engine/source/interfaces/int24/i24cor3.F
Chd|        I24FOR3                       engine/source/interfaces/int24/i24for3.F
Chd|        I24MAIN_TRI                   engine/source/interfaces/intsort/i24main_tri.F
Chd|        I25COR3E                      engine/source/interfaces/int25/i25cor3e.F
Chd|        I25COR3_3                     engine/source/interfaces/int25/i25cor3.F
Chd|        I25COR3_E2S                   engine/source/interfaces/int25/i25cor3_e2s.F
Chd|        I25FOR3                       engine/source/interfaces/int25/i25for3.F
Chd|        I25MAIN_SLID                  engine/source/interfaces/int25/i25main_slid.F
Chd|        I25MAIN_TRI                   engine/source/interfaces/intsort/i25main_tri.F
Chd|        RDCOMI                        engine/source/output/restart/rdcomm.F
Chd|        RDCOMR                        engine/source/output/restart/rdcomm.F
Chd|        SPMD_FIADD_POFF               engine/source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD_PON                engine/source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_INITFI                   engine/source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_SAVEFI                   engine/source/mpi/interfaces/spmd_i7tool.F
Chd|        WRCOMI                        engine/source/output/restart/wrcomm.F
Chd|        WRCOMR                        engine/source/output/restart/wrcomm.F
Chd|-- calls ---------------
Chd|====================================================================
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
             my_real  DT_STIFINT    ! time step for stiffness based on Mass and DT computation 
             my_real, DIMENSION(:) , ALLOCATABLE ::   INTAREAN ! nodal areas for total contact area output
          END TYPE PARAMETERS_

C
        END MODULE PARAMETERS_MOD
