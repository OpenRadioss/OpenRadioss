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
Chd|  I7MAIN_OPT_TRI                source/interfaces/intsort/i7main_opt_tri.F
Chd|-- called by -----------
Chd|        IMP_TRIPI                     source/implicit/imp_int_k.F   
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        I7OPTCD                       source/interfaces/intsort/i7optcd.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I7MAIN_OPT_TRI(
     1                  IPARI   ,X     ,V           ,
     2                  NIN     ,ITASK ,COUNT_REMSLV, INTBUF_TAB)
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C----------------------------------------------- 
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr04_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,NINTER), ITASK, NIN,
     .        COUNT_REMSLV(*)
      my_real 
     .   X(*), V(3,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER KK, I, IFQ, I_STOK
      my_real
     .   STARTT,GAP,GAPMIN,STOPT
C-----------------------------------------------
C   S o u r c e   C o d e
C-----------------------------------------------

      IFQ =IPARI(31,NIN) 
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT>TT.OR.TT>STOPT) RETURN
      GAP =INTBUF_TAB%VARIABLES(2)
      GAPMIN = INTBUF_TAB%VARIABLES(13)

       !-----------------------------------------------------------
       !     Filtrage des candidats a l'impact
       !     (si candidat retenu, cand_n(i) = - cand_n(i))
       !-----------------------------------------------------------
       I_STOK = INTBUF_TAB%I_STOK(1)
       CALL I7OPTCD(
     1        INTBUF_TAB%NSV          ,INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,X     ,I_STOK ,
     2        INTBUF_TAB%IRECTM       ,GAP              ,INTBUF_TAB%GAP_S        ,INTBUF_TAB%GAP_M,IPARI(21,NIN),
     3        INTBUF_TAB%STFNS        ,ITASK            ,INTBUF_TAB%STFM,IFQ     ,INTBUF_TAB%IFPEN,
     4        INTBUF_TAB%FTSAVX       ,INTBUF_TAB%FTSAVY,INTBUF_TAB%FTSAVZ,NIN   ,IPARI(5,NIN) ,
     5        INTBUF_TAB%VARIABLES(16),IPARI(39,NIN)    ,INTBUF_TAB%GAP_SL       ,INTBUF_TAB%GAP_ML,
     6        COUNT_REMSLV            ,GAPMIN           ,INTBUF_TAB%VARIABLES(32),INTBUF_TAB%VARIABLES(46))
C
      RETURN
      END
