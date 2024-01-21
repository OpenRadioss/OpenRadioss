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
Chd|  SEATBELT_MOD                  modules/seatbelt_mod.F        
Chd|-- called by -----------
Chd|        CREATE_SEATBELT               starter/source/tools/seatbelts/create_seatbelt.F
Chd|        C_SEATBELTS                   starter/source/restart/ddsplit/c_seatbelts.F
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        HM_READ_RETRACTOR             starter/source/tools/seatbelts/hm_read_retractor.F
Chd|        HM_READ_SLIPRING              starter/source/tools/seatbelts/hm_read_slipring.F
Chd|        INI_SEATBELT                  starter/source/tools/seatbelts/ini_seatbelt.F
Chd|        LECTUR                        starter/source/starter/lectur.F
Chd|        NEW_SEATBELT                  starter/source/tools/seatbelts/new_seatbelt.F
Chd|        RGRHEAD                       starter/source/elements/spring/rgrhead.F
Chd|        RGRTAILS                      starter/source/elements/spring/rgrtails.F
Chd|        RINIT3                        starter/source/elements/spring/rinit3.F
Chd|        ST_QAPRINT_SEATBELTS          starter/source/output/qaprint/st_qaprint_seatbelts.F
Chd|        W_SEATBELTS                   starter/source/restart/ddsplit/w_seatbelts.F
Chd|        HIST2                         engine/source/output/th/hist2.F
Chd|        KINE_SEATBELT_FORCE           engine/source/tools/seatbelts/kine_seatbelt_force.F
Chd|        KINE_SEATBELT_VEL             engine/source/tools/seatbelts/kine_seatbelt_vel.F
Chd|        MATERIAL_FLOW                 engine/source/tools/seatbelts/material_flow.F
Chd|        R23L114DEF3                   engine/source/elements/spring/r23l114def3.F
Chd|        RDRESB                        engine/source/output/restart/rdresb.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        RESTALLOC                     engine/source/output/restart/arralloc.F
Chd|        SPMD_COLLECT_SEATBELT         engine/source/mpi/output/spmd_collect_seatbelt.F
Chd|        SPMD_EXCH_A_SEATBELT          engine/source/mpi/seatbelts/spmd_exch_a_seatbelt.F
Chd|        UPDATE_SLIPRING               engine/source/tools/seatbelts/update_slipring.F
Chd|        WRRESTP                       engine/source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE DAMP_MOD
C----------------------------------------------------------------------- 
#include "my_real.inc"
c
c=======================================================================      
c define structure for Damping
c=======================================================================      
C 
      INTEGER , ALLOCATABLE , DIMENSION(:) :: ID_DAMP_VREL ! ID of ∕DAMP/VREL in DAMP list
      INTEGER , ALLOCATABLE , DIMENSION(:) :: FR_DAMP_VREL ! SPMD communications for /DAMP/VREL
C
      INTEGER :: NDAMP_VREL ! Number of /DAMP/VREL      
C
c---------------
      END MODULE  DAMP_MOD
