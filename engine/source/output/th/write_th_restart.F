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
Chd|  WRITE_TH_RESTART              source/output/th/write_th_restart.F
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TIME_HISTORY_MOD              ../common_source/modules/output/time_history_mod.F
Chd|====================================================================
      SUBROUTINE WRITE_TH_RESTART(TH)
   ! -----------------------------------------------
   !   ROUTINE DESCRIPTION    :
   !   ========================
   !   write Time History buffer
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE TIME_HISTORY_MOD
        USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE(TH_),INTENT(IN) :: TH   ! TH Type 
C-----------------------------------------------
        CALL WRITE_I_C(TH%NITHGR,  1)
        CALL WRITE_I_C(TH%SITHGRP, 1) 
        CALL WRITE_I_C(TH%SITHBUF, 1)

        CALL WRITE_I_C(TH%SITHGRPA,1)
        CALL WRITE_I_C(TH%SITHBUFA,1)

        CALL WRITE_I_C(TH%SITHGRPB,1)
        CALL WRITE_I_C(TH%SITHBUFB,1)

        CALL WRITE_I_C(TH%SITHGRPC,1)
        CALL WRITE_I_C(TH%SITHBUFC,1)

        CALL WRITE_I_C(TH%SITHGRPD,1)
        CALL WRITE_I_C(TH%SITHBUFD,1)

        CALL WRITE_I_C(TH%SITHGRPE,1)
        CALL WRITE_I_C(TH%SITHBUFE,1)

        CALL WRITE_I_C(TH%SITHGRPF,1)
        CALL WRITE_I_C(TH%SITHBUFF,1)

        CALL WRITE_I_C(TH%SITHGRPG,1)
        CALL WRITE_I_C(TH%SITHBUFG,1)

        CALL WRITE_I_C(TH%SITHGRPH,1)
        CALL WRITE_I_C(TH%SITHBUFH,1)

        CALL WRITE_I_C(TH%SITHGRPI,1)
        CALL WRITE_I_C(TH%SITHBUFI,1)

 

        CALL WRITE_I_C(TH%ITHGRP,  TH%SITHGRP )
        CALL WRITE_I_C(TH%ITHBUF,  TH%SITHBUF )

        CALL WRITE_I_C(TH%ITHGRPA, TH%SITHGRPA)
        CALL WRITE_I_C(TH%ITHBUFA, TH%SITHBUFA)

        CALL WRITE_I_C(TH%ITHGRPB, TH%SITHGRPB)
        CALL WRITE_I_C(TH%ITHBUFB, TH%SITHBUFB)

        CALL WRITE_I_C(TH%ITHGRPC, TH%SITHGRPC)
        CALL WRITE_I_C(TH%ITHBUFC, TH%SITHBUFC)

        CALL WRITE_I_C(TH%ITHGRPD, TH%SITHGRPD)
        CALL WRITE_I_C(TH%ITHBUFD, TH%SITHBUFD)

        CALL WRITE_I_C(TH%ITHGRPE, TH%SITHGRPE)
        CALL WRITE_I_C(TH%ITHBUFE, TH%SITHBUFE)

        CALL WRITE_I_C(TH%ITHGRPF, TH%SITHGRPF)
        CALL WRITE_I_C(TH%ITHBUFF, TH%SITHBUFF)

        CALL WRITE_I_C(TH%ITHGRPG, TH%SITHGRPG)
        CALL WRITE_I_C(TH%ITHBUFG, TH%SITHBUFG)

        CALL WRITE_I_C(TH%ITHGRPH, TH%SITHGRPH)
        CALL WRITE_I_C(TH%ITHBUFH, TH%SITHBUFH)

        CALL WRITE_I_C(TH%ITHGRPI, TH%SITHGRPI)
        CALL WRITE_I_C(TH%ITHBUFI, TH%SITHBUFI)

      END SUBROUTINE WRITE_TH_RESTART