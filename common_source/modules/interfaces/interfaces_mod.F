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
Chd|  INTERFACES_MOD                modules/interfaces/interfaces_mod.F
Chd|-- called by -----------
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        HM_READ_INTERFACES            starter/source/interfaces/reader/hm_read_interfaces.F
Chd|        HM_READ_INTER_STRUCT          starter/source/interfaces/reader/hm_read_inter_struct.F
Chd|        HM_READ_THGROU                starter/source/output/th/hm_read_thgrou.F
Chd|        INITIA                        starter/source/elements/initia/initia.F
Chd|        LECTUR                        starter/source/starter/lectur.F
Chd|        I24MAINF                      engine/source/interfaces/int24/i24main.F
Chd|        I25MAINF                      engine/source/interfaces/int25/i25mainf.F
Chd|        IMP_INTTD0                    engine/source/implicit/imp_int_k.F
Chd|        IMP_SOLV                      engine/source/implicit/imp_solv.F
Chd|        IMP_TRIPI                     engine/source/implicit/imp_int_k.F
Chd|        INTFOP2                       engine/source/interfaces/interf/intfop2.F
Chd|        INTTRI                        engine/source/interfaces/intsort/inttri.F
Chd|        RADIOSS2                      engine/source/engine/radioss2.F
Chd|        RDRESA                        engine/source/output/restart/rdresa.F
Chd|        RDRESB                        engine/source/output/restart/rdresb.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        RESOL_HEAD                    engine/source/engine/resol_head.F
Chd|        SPMD_I7FCOM_POFF              engine/source/mpi/forces/spmd_i7fcom_poff.F
Chd|        SPMD_I7FCOM_PON               engine/source/mpi/forces/spmd_i7fcom_pon.F
Chd|        SPMD_I7XVCOM2                 engine/source/mpi/interfaces/spmd_i7xvcom2.F
Chd|        WRRESTP                       engine/source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        PARAMETERS_MOD                modules/interfaces/parameters_mod.F
Chd|====================================================================
        MODULE INTERFACES_MOD
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
       USE PARAMETERS_MOD
       USE SPMD_ARRAYS_MOD
       implicit none
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"

!   -----------------------------------------------
!   D e r i v e d   T y p e   D e f i n i t i o n s
!   -----------------------------------------------

!   -------------------------

C----------------------------------------------
C   Global Interfaces stucture
C---------------------------------------------

          TYPE INTERFACES_
               TYPE (PARAMETERS_) PARAMETERS
               TYPE (SPMD_ARRAYS_) SPMD_ARRAYS
          END TYPE INTERFACES_

C
        END MODULE INTERFACES_MOD
