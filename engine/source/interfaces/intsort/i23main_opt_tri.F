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
Chd|  I23MAIN_OPT_TRI               source/interfaces/intsort/i23main_opt_tri.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        I23OPTCD                      source/interfaces/int23/i23optcd.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I23MAIN_OPT_TRI(
     1                  IPARI   ,INTBUF_TAB ,NIN     ,ITASK ,
     2                  COUNT_REMSLV,X )
C-----------------------------------------------
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,NINTER), ITASK, NIN,
     .        COUNT_REMSLV(*)
C     REAL
      my_real 
     .   X(3,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER KK, I, INACTI, I_STOK, NSN
C     REAL
      my_real
     .   STARTT,GAP,STOPT, DX, DY, DZ, GAPMIN
C-----------------------------------------------
C
      NSN    =IPARI(5,NIN)
      INACTI =IPARI(22,NIN)
C
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT>TT) goto 999
      IF(TT>STOPT)  goto 999
C
      GAP =INTBUF_TAB%VARIABLES(2)
      GAPMIN =INTBUF_TAB%VARIABLES(13)
C
C-----------------------------------------------------------
C     Filtrage des candidats a l'impact
C     (si candidat retenu, cand_n(i) = - cand_n(i))
C-----------------------------------------------------------
       I_STOK = INTBUF_TAB%I_STOK(1)
       CALL I23OPTCD(
     1 INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,X	   ,I_STOK,INTBUF_TAB%IRECTM,
     2GAP      ,INTBUF_TAB%GAP_S,IPARI(21,NIN),IPARI(5,NIN) ,INTBUF_TAB%STFNS,
     3ITASK	   ,INTBUF_TAB%STFM,INACTI   ,INTBUF_TAB%FTSAVX,
     4INTBUF_TAB%FTSAVY,INTBUF_TAB%FTSAVZ,INTBUF_TAB%CAND_P,NIN   ,INTBUF_TAB%VARIABLES(16),
     5IPARI(39,NIN),COUNT_REMSLV,INTBUF_TAB%GAP_M,
     .  				    INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     .GAPMIN)
C-----------------------------------------------------------
C
  999   continue
      RETURN
      END
