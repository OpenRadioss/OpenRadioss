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
Chd|  I24MAIN_CRIT_TRI              source/interfaces/intsort/i24main_crit_tri.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        I24BUCE_CRIT                  source/interfaces/intsort/i24buce_crit.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I24MAIN_CRIT_TRI(
     1  IPARI  ,INTBUF_TAB     ,X      ,NIN    ,
     2  ITASK  ,V      ,XSLV_L ,XMSR_L ,VSLV_L ,
     3  VMSR_L ,DELTA_PMAX_GAP,DELTA_PMAX_DGAP,
     4  DELTA_PMAX_GAP_NODE,ITAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN,ITASK,
     .        IPARI(NPARI,NINTER),DELTA_PMAX_GAP_NODE,ITAB(*)
C     REAL
      my_real 
     .   X(3,*), V(3,*),DELTA_PMAX_GAP,PMAX_GAP,
     .   XSLV_L(*), XMSR_L(*), VSLV_L(*), VMSR_L(*),
     .   DELTA_PMAX_DGAP
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IEDGE,INTPLY
C     REAL
      my_real
     .   STARTT, STOPT, BID(1)
      INTEGER :: NMN,NSN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C
      NSN   =IPARI(5,NIN)
      NMN   =IPARI(6,NIN)
      IEDGE =IPARI(58,NIN)
      INTPLY =IPARI(66,NIN)
C
C OW CALCUL DU CRITERE DE TRI : OPTIMISER avec NSN = IPARI(5,NIN) - NSNE

      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT>TT) RETURN
      IF(TT>STOPT)  RETURN
C
C test temporaire pour assurer la compatibilite restart en 51d et 51e
       IF(INTBUF_TAB%VARIABLES(16)==ZERO)INTBUF_TAB%VARIABLES(16)=EP30
       CALL I24BUCE_CRIT(
     1 X      ,INTBUF_TAB%NSV,INTBUF_TAB%MSR,NSN      ,NMN      ,
     2 ITASK      ,INTBUF_TAB%XSAV,NIN      ,INTBUF_TAB%STFNS,V     ,
     3 XSLV_L      ,XMSR_L    ,VSLV_L     ,VMSR_L  ,INTBUF_TAB%PENE_OLD,
     4 INTBUF_TAB%GAP_S,INTBUF_TAB%GAPN_M,DELTA_PMAX_GAP,INTBUF_TAB%VARIABLES(23),
     5 INTBUF_TAB%EDGE8L2,INTBUF_TAB%ISEADD,INTBUF_TAB%ISEDGE,IEDGE,INTBUF_TAB%DGAP_M,
     6 INTBUF_TAB%DELTA_PMAX_DGAP,INTPLY  ,DELTA_PMAX_GAP_NODE,ITAB    )
C
      RETURN
      END
