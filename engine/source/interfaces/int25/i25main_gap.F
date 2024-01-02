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
Chd|  I25MAIN_GAP                   source/interfaces/int25/i25main_gap.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        I25GAP3                       source/interfaces/int25/i25gap3.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|====================================================================
      SUBROUTINE I25MAIN_GAP(
     1  IPARI  ,INTBUF_TAB  ,NIN    ,ITASK  ,
     2  THKNOD , MAXDGAP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTSTAMP_MOD
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
     .        IPARI(NPARI,NINTER)
C     REAL
      my_real 
     .   THKNOD(*), MAXDGAP

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NRTM, NMN
C     REAL
      my_real
     .   STARTT, STOPT, GAPMAX_M, GAPSCALE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C
      NRTM  =IPARI(4,NIN)
      NMN   =IPARI(6,NIN)
C
      GAPMAX_M = INTBUF_TAB%VARIABLES(36)
      GAPSCALE = INTBUF_TAB%VARIABLES(45)
C
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
C
      IF(STARTT>TT) RETURN
      IF(TT>STOPT)  RETURN
C
      CALL I25GAP3(
     1  ITASK ,
     2  NRTM  ,INTBUF_TAB%IRECTM   ,INTBUF_TAB%GAP_NM   ,INTBUF_TAB%GAP_M,   
     3  NMN   ,INTBUF_TAB%MSR      ,INTBUF_TAB%GAPN_M   ,
     4  GAPMAX_M   ,GAPSCALE   ,INTBUF_TAB%MSEGTYP24   ,THKNOD,
     5  INTBUF_TAB%GAPMSAV, MAXDGAP)
C
      RETURN
      END
