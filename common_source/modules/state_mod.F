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
Chd|  STATE_MOD                     modules/state_mod.F           
Chd|-- called by -----------
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        LECTUR                        starter/source/starter/lectur.F
Chd|        WRCOMIP                       starter/source/restart/ddsplit/wrcommp.F
Chd|        DYNAIN_C_STRAG                engine/source/output/dynain/dynain_c_strag.F
Chd|        DYNAIN_C_STRSG                engine/source/output/dynain/dynain_c_strsg.F
Chd|        DYNAIN_NODE                   engine/source/output/dynain/dynain_node.F
Chd|        DYNAIN_SHEL_MP                engine/source/output/dynain/dynain_shel_mp.F
Chd|        DYNAIN_SHEL_SPMD              engine/source/output/dynain/dynain_shel_spmd.F
Chd|        DYNAIN_SIZE_C                 engine/source/output/dynain/dynain_size.F
Chd|        ECRIT                         engine/source/output/ecrit.F  
Chd|        FREDYNAIN                     engine/source/input/fredynain.F
Chd|        FREFORM                       engine/source/input/freform.F 
Chd|        GENDYNAIN                     engine/source/output/dynain/gendynain.F
Chd|        LECINP                        engine/source/input/lecinp.F  
Chd|        LECTUR                        engine/source/input/lectur.F  
Chd|        RADIOSS2                      engine/source/engine/radioss2.F
Chd|        RDCOMI                        engine/source/output/restart/rdcomm.F
Chd|        RDCOMR                        engine/source/output/restart/rdcomm.F
Chd|        RDRESA                        engine/source/output/restart/rdresa.F
Chd|        RDRESB                        engine/source/output/restart/rdresb.F
Chd|        READ_DYNAIN                   engine/source/output/dynain/read_dynain.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        RESOL_HEAD                    engine/source/engine/resol_head.F
Chd|        SORTIE_MAIN                   engine/source/output/sortie_main.F
Chd|        STOP_SENSOR                   engine/source/tools/sensor/stop_sensor.F
Chd|        WRCOMI                        engine/source/output/restart/wrcomm.F
Chd|        WRCOMR                        engine/source/output/restart/wrcomm.F
Chd|        WRRESTP                       engine/source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|====================================================================
        MODULE STATE_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
!   -----------------------------------------------
!   D e r i v e d   T y p e   D e f i n i t i o n s
!   -----------------------------------------------

!   -------------------------
        TYPE DYNAIN_DATABASE
            INTEGER :: ZIPDYNAIN
            INTEGER :: MX_DYNAIN  =20
            INTEGER :: NDYNAINPRT,NC_DYNAIN,
     .        DYNAIN_NUMELC,DYNAIN_NUMELTG,
     .        DYNAIN_NUMELC_G,DYNAIN_NUMELTG_G,
     .        NDYNAINALL,IDYNAINF,DYNAIN_CHECK
            INTEGER :: DYNAIN_C(20) ! size MX_DYNAIN
            INTEGER , DIMENSION(:), ALLOCATABLE :: IPART_DYNAIN
            my_real
     .        DTDYNAIN0,TDYNAIN0,DTDYNAIN, TDYNAIN
        END TYPE DYNAIN_DATABASE

C
        END MODULE STATE_MOD
