!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!hd|====================================================================
!hd|  STATE_MOD                     modules/state_mod.F
!hd|-- called by -----------
!hd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
!hd|        LECTUR                        starter/source/starter/lectur.F
!hd|        WRCOMIP                       starter/source/restart/ddsplit/wrcommp.F
!hd|        DYNAIN_C_STRAG                engine/source/output/dynain/dynain_c_strag.F
!hd|        DYNAIN_C_STRSG                engine/source/output/dynain/dynain_c_strsg.F
!hd|        DYNAIN_NODE                   engine/source/output/dynain/dynain_node.F
!hd|        DYNAIN_SHEL_MP                engine/source/output/dynain/dynain_shel_mp.F
!hd|        DYNAIN_SHEL_SPMD              engine/source/output/dynain/dynain_shel_spmd.F
!hd|        DYNAIN_SIZE_C                 engine/source/output/dynain/dynain_size.F
!hd|        ECRIT                         engine/source/output/ecrit.F
!hd|        FREDYNAIN                     engine/source/input/fredynain.F
!hd|        FREFORM                       engine/source/input/freform.F
!hd|        GENDYNAIN                     engine/source/output/dynain/gendynain.F
!hd|        LECINP                        engine/source/input/lecinp.F
!hd|        LECTUR                        engine/source/input/lectur.F
!hd|        RADIOSS2                      engine/source/engine/radioss2.F
!hd|        RDCOMI                        engine/source/output/restart/rdcomm.F
!hd|        RDCOMR                        engine/source/output/restart/rdcomm.F
!hd|        RDRESA                        engine/source/output/restart/rdresa.F
!hd|        RDRESB                        engine/source/output/restart/rdresb.F
!hd|        READ_DYNAIN                   engine/source/output/dynain/read_dynain.F
!hd|        RESOL                         engine/source/engine/resol.F
!hd|        RESOL_HEAD                    engine/source/engine/resol_head.F
!hd|        SORTIE_MAIN                   engine/source/output/sortie_main.F
!hd|        STOP_SENSOR                   engine/source/tools/sensor/stop_sensor.F
!hd|        WRCOMI                        engine/source/output/restart/wrcomm.F
!hd|        WRCOMR                        engine/source/output/restart/wrcomm.F
!hd|        WRRESTP                       engine/source/output/restart/wrrestp.F
!hd|-- calls ---------------
!hd|====================================================================
      MODULE STATE_MOD
!-----------------------------------------------
!   m y _ r e a l
!-----------------------------------------------
#include      "my_real.inc"
!   -----------------------------------------------
!   D e r i v e d   T y p e   D e f i n i t i o n s
!   -----------------------------------------------

!   -------------------------
        TYPE DYNAIN_DATABASE
          INTEGER :: ZIPDYNAIN
          INTEGER :: MX_DYNAIN  =20
          INTEGER :: NDYNAINPRT,NC_DYNAIN,&
          &DYNAIN_NUMELC,DYNAIN_NUMELTG,&
          &DYNAIN_NUMELC_G,DYNAIN_NUMELTG_G,&
          &NDYNAINALL,IDYNAINF,DYNAIN_CHECK
          INTEGER :: DYNAIN_C(20) ! size MX_DYNAIN
          INTEGER , DIMENSION(:), ALLOCATABLE :: IPART_DYNAIN
          my_real &
          &DTDYNAIN0,TDYNAIN0,DTDYNAIN, TDYNAIN
        END TYPE DYNAIN_DATABASE

!
      END MODULE STATE_MOD
