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
Chd|  I25MAIN_CRIT_TRI              source/interfaces/intsort/i25main_crit_tri.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        I25BUCE_CRIT                  source/interfaces/intsort/i25buce_crit.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I25MAIN_CRIT_TRI(
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
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr04_c.inc"
#include      "task_c.inc"
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
      my_real
     .   STARTT, STOPT
      INTEGER :: ISENS
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C
      NSN   =IPARI(5,NIN)
      NMN   =IPARI(6,NIN)
C
      ISENS = IPARI(64,NIN)  
      IF(ISENS == 0) THEN
         STOPT =INTBUF_TAB%VARIABLES(11)
         IF(TT > STOPT) RETURN
      ENDIF

      CALL I25BUCE_CRIT(
     1 X            ,INTBUF_TAB%NSV,INTBUF_TAB%MSR,NSN      ,NMN      ,
     2 ITASK        ,INTBUF_TAB%XSAV,NIN      ,INTBUF_TAB%STFNS,V     ,
     3 XSLV_L       ,XMSR_L       ,VSLV_L     ,VMSR_L       )
C
      RETURN
      END
