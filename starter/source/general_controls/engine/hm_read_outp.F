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
Chd|  HM_READ_OUTP                  source/general_controls/engine/hm_read_outp.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE HM_READ_OUTP()
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
C     Initializations ../..
C-----------------------------------------------
      TOUTP0 = ZERO
      DTOUTP0 = ZERO
      DO I = 1,MX_OUTP
        OUTP_V(I) = 0
        OUTP_SS(I) = 0
        OUTP_ST(I) = 0
        OUTP_CS(I) = 0
        OUTP_CT(I) = 0
        OUTP_TS(I) = 0
        OUTP_PS(I) = 0
        OUTP_PT(I) = 0
        OUTP_RS(I) = 0
        OUTP_RT(I) = 0
        OUTP_SPS(I)=0
        OUTP_SPT(I)=0
      ENDDO
C-----------------------------------------------
C     Read options ../..
C-----------------------------------------------
C-----------------------------------------------
C     Counters
C-----------------------------------------------
      NV_OUTP = 0
      NSS_OUTP = 0
      NST_OUTP = 0
      NCS_OUTP = 0
      NCT_OUTP = 0
      NTS_OUTP = 0
      NPS_OUTP = 0
      NPT_OUTP = 0
      NRS_OUTP = 0
      NRT_OUTP = 0
      NSPS_OUTP = 0
      NSPT_OUTP = 0
      DO I = 1,MX_OUTP
        NV_OUTP = NV_OUTP + OUTP_V(I)
        NSS_OUTP = NSS_OUTP + OUTP_SS(I)
        NST_OUTP = NST_OUTP + OUTP_ST(I)
        NCS_OUTP = NCS_OUTP + OUTP_CS(I)
        NCT_OUTP = NCT_OUTP + OUTP_CT(I)
        NTS_OUTP = NTS_OUTP + OUTP_TS(I)
        NPS_OUTP = NPS_OUTP + OUTP_PS(I)
        NPT_OUTP = NPT_OUTP + OUTP_PT(I)
        NRS_OUTP = NRS_OUTP + OUTP_RS(I)
        NRT_OUTP = NRT_OUTP + OUTP_RT(I)
        NSPS_OUTP = NSPS_OUTP + OUTP_SPS(I)
        NSPT_OUTP = NSPT_OUTP + OUTP_SPT(I)
      ENDDO
      IF(OUTP_V(12)==1)NV_OUTP=NV_OUTP+1
C
      RETURN
      END
      
