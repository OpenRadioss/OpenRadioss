Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||    int12w          ../engine/source/ale/inter/int12w.F
!||--- called by ------------------------------------------------------
!||    intal3          ../engine/source/ale/inter/intal3.F
!||--- calls      -----------------------------------------------------
!||    in12r           ../engine/source/interfaces/interf/in12r.F
!||    iqela3          ../engine/source/ale/inter/iqela3.F
!||--- uses       -----------------------------------------------------
!||    intbufdef_mod   ../common_source/modules/interfaces/intbufdef_mod.F90
!||====================================================================
      SUBROUTINE INT12W( IPARI, INTBUF_TAB, VG  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
       USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(SIPARI)
      my_real VG(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITIED, NSN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      NSN   =IPARI(5)
      ITIED=INT(INTBUF_TAB%VARIABLES(1))
      CALL IQELA3(VG,VG,INTBUF_TAB%IRECTM,INTBUF_TAB%CSTS,INTBUF_TAB%NSV,INTBUF_TAB%IRTLM,INTBUF_TAB%MSR,NSN)
      IF(ITIED == 2) THEN
        CALL IN12R(VG,INTBUF_TAB%VARIABLES(1),INTBUF_TAB%NSV,NSN,2,INTBUF_TAB%FCOUNT)
      ENDIF
      RETURN
      END
