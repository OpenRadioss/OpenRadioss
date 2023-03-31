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
Chd|  READ_TH                      source/output/th/read_th.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE READ_TH_RESTART(TH)
   ! -----------------------------------------------
   !   ROUTINE DESCRIPTION    :
   !   ========================
   !   Read Time History buffer
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE TIME_HISTORY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE(TH_),INTENT(INOUT) :: TH   ! TH Type 
C-----------------------------------------------
        CALL READ_I_C(TH%NITHGR,  1)
        CALL READ_I_C(TH%SITHGRP, 1) 
        CALL READ_I_C(TH%SITHBUF, 1)

        CALL READ_I_C(TH%SITHGRPA,1)
        CALL READ_I_C(TH%SITHBUFA,1)

        CALL READ_I_C(TH%SITHGRPB,1)
        CALL READ_I_C(TH%SITHBUFB,1)

        CALL READ_I_C(TH%SITHGRPC,1)
        CALL READ_I_C(TH%SITHBUFC,1)

        CALL READ_I_C(TH%SITHGRPD,1)
        CALL READ_I_C(TH%SITHBUFD,1)

        CALL READ_I_C(TH%SITHGRPE,1)
        CALL READ_I_C(TH%SITHBUFE,1)

        CALL READ_I_C(TH%SITHGRPF,1)
        CALL READ_I_C(TH%SITHBUFF,1)

        CALL READ_I_C(TH%SITHGRPG,1)
        CALL READ_I_C(TH%SITHBUFG,1)

        CALL READ_I_C(TH%SITHGRPH,1)
        CALL READ_I_C(TH%SITHBUFH,1)

        CALL READ_I_C(TH%SITHGRPI,1)
        CALL READ_I_C(TH%SITHBUFI,1)

        CALL TH_ALLOC(TH,
     .                TH%SITHGRP,  TH%SITHBUF ,
     .                TH%SITHGRPA, TH%SITHBUFA,
     .                TH%SITHGRPB, TH%SITHBUFB,
     .                TH%SITHGRPC, TH%SITHBUFC,
     .                TH%SITHGRPD, TH%SITHBUFD,
     .                TH%SITHGRPE, TH%SITHBUFE,
     .                TH%SITHGRPF, TH%SITHBUFF,
     .                TH%SITHGRPG, TH%SITHBUFG,
     .                TH%SITHGRPH, TH%SITHBUFH,
     .                TH%SITHGRPI, TH%SITHBUFI )

        CALL READ_I_C(TH%ITHGRP,  TH%SITHGRP )
        CALL READ_I_C(TH%ITHBUF,  TH%SITHBUF )

        CALL READ_I_C(TH%ITHGRPA, TH%SITHGRPA)
        CALL READ_I_C(TH%ITHBUFA, TH%SITHBUFA)

        CALL READ_I_C(TH%ITHGRPB, TH%SITHGRPB)
        CALL READ_I_C(TH%ITHBUFB, TH%SITHBUFB)

        CALL READ_I_C(TH%ITHGRPC, TH%SITHGRPC)
        CALL READ_I_C(TH%ITHBUFC, TH%SITHBUFC)

        CALL READ_I_C(TH%ITHGRPD, TH%SITHGRPD)
        CALL READ_I_C(TH%ITHBUFD, TH%SITHBUFD)

        CALL READ_I_C(TH%ITHGRPE, TH%SITHGRPE)
        CALL READ_I_C(TH%ITHBUFE, TH%SITHBUFE)

        CALL READ_I_C(TH%ITHGRPF, TH%SITHGRPF)
        CALL READ_I_C(TH%ITHBUFF, TH%SITHBUFF)

        CALL READ_I_C(TH%ITHGRPG, TH%SITHGRPG)
        CALL READ_I_C(TH%ITHBUFG, TH%SITHBUFG)

        CALL READ_I_C(TH%ITHGRPH, TH%SITHGRPH)
        CALL READ_I_C(TH%ITHBUFH, TH%SITHBUFH)

        CALL READ_I_C(TH%ITHGRPI, TH%SITHGRPI)
        CALL READ_I_C(TH%ITHBUFI, TH%SITHBUFI)

      END SUBROUTINE READ_TH_RESTART
