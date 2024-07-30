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
      !||    state_mod          ../common_source/modules/state_mod.F
      !||--- called by ------------------------------------------------------
      !||    ddsplit            ../starter/source/restart/ddsplit/ddsplit.F
      !||    dynain_c_strag     ../engine/source/output/dynain/dynain_c_strag.F
      !||    dynain_c_strsg     ../engine/source/output/dynain/dynain_c_strsg.F
      !||    dynain_node        ../engine/source/output/dynain/dynain_node.F
      !||    dynain_shel_mp     ../engine/source/output/dynain/dynain_shel_mp.F
      !||    dynain_shel_spmd   ../engine/source/output/dynain/dynain_shel_spmd.F
      !||    dynain_size_c      ../engine/source/output/dynain/dynain_size.F
      !||    ecrit              ../engine/source/output/ecrit.F
      !||    fredynain          ../engine/source/input/fredynain.F
      !||    freform            ../engine/source/input/freform.F
      !||    gendynain          ../engine/source/output/dynain/gendynain.F
      !||    lecinp             ../engine/source/input/lecinp.F
      !||    lectur             ../engine/source/input/lectur.F
      !||    radioss2           ../engine/source/engine/radioss2.F
      !||    rdcomi             ../engine/source/output/restart/rdcomm.F
      !||    rdcomr             ../engine/source/output/restart/rdcomm.F
      !||    rdresa             ../engine/source/output/restart/rdresa.F
      !||    rdresb             ../engine/source/output/restart/rdresb.F
      !||    read_dynain        ../engine/source/output/dynain/read_dynain.F
      !||    resol              ../engine/source/engine/resol.F
      !||    resol_head         ../engine/source/engine/resol_head.F
      !||    sortie_main        ../engine/source/output/sortie_main.F
      !||    stop_sensor        ../engine/source/tools/sensor/stop_sensor.F
      !||    wrcomi             ../engine/source/output/restart/wrcomm.F
      !||    wrcomip            ../starter/source/restart/ddsplit/wrcommp.F
      !||    wrcomr             ../engine/source/output/restart/wrcomm.F
      !||    wrrestp            ../engine/source/output/restart/wrrestp.F
      !||====================================================================
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
