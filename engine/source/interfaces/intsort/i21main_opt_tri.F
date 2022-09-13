Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  I21MAIN_OPT_TRI               source/interfaces/intsort/i21main_opt_tri.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        I21OPTCD                      source/interfaces/int21/i21optcd.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|====================================================================
      SUBROUTINE I21MAIN_OPT_TRI(
     1                  IPARI   ,INTBUF_TAB ,NIN     ,ITASK ,
     2                  INTSTAMP,NB_STOK_N,NB_JLT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTSTAMP_MOD
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
     .     NB_STOK_N(*),NB_JLT(*)
      TYPE(INTSTAMP_DATA) INTSTAMP
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER KK, I, INACTI, I_STOK, NSN, ADXLOC
C     REAL
      my_real
     .   STARTT,GAP,STOPT, DX, DY, DZ
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
C
      IF(NSN > 0) THEN
         ADXLOC = 1+3*(NSN+2)
      ELSE
         ADXLOC = 1
      ENDIF
C
C-----------------------------------------------------------
C     Filtrage des candidats a l'impact
C     (si candidat retenu, cand_n(i) = - cand_n(i))
C-----------------------------------------------------------
       I_STOK = INTBUF_TAB%I_STOK(1)
       CALL I21OPTCD(
     1 INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,INTBUF_TAB%XSAV(ADXLOC),
     .                                    I_STOK     ,INTBUF_TAB%IRECTM,
     2GAP      ,INTBUF_TAB%GAP_S,IPARI(21,NIN),IPARI(5,NIN) ,INTBUF_TAB%STFNS,
     3ITASK	   ,INTBUF_TAB%STF,INACTI   ,INTBUF_TAB%IFPEN,INTBUF_TAB%FTSAVX,
     4INTBUF_TAB%FTSAVY,INTBUF_TAB%FTSAVZ,INTBUF_TAB%PENIS,NIN   ,INTBUF_TAB%VARIABLES(16),
     5IPARI(39,NIN),INTBUF_TAB%IRTLM,INTBUF_TAB%CSTS,INTBUF_TAB%VARIABLES(23),
     .  					  INTBUF_TAB%NOD_NORMAL,
     6INTBUF_TAB%XM0,INTBUF_TAB%VARIABLES(32),NB_STOK_N,NB_JLT,INTBUF_TAB%VARIABLES(46))
C-----------------------------------------------------------
C
  999   continue
      RETURN
      END
