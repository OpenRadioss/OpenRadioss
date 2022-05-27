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
Chd|  I21MAIN_GAP                   source/interfaces/int21/i21main_gap.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        I21GAP3                       source/interfaces/int21/i21gap3.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|====================================================================
      SUBROUTINE I21MAIN_GAP(
     1  IPARI  ,INTBUF_TAB  ,NIN    ,ITASK  ,
     2  THKNOD)
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
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN,ITASK,
     .        IPARI(NPARI,NINTER)
C     REAL
      my_real 
     .   THKNOD(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER KD(50), JD(50), JFI, KFI, IGAP, IDEL
C     REAL
      my_real
     .   STARTT, STOPT
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C---------------------------------------------------- 
C	Calcul des adresses des buffers d'interfaces. 
C----------------------------------------------------
C
      NSN   =IPARI(5,NIN)
      NRTS  =IPARI(3,NIN)
      IGAP  =IPARI(21,NIN)
      IDEL  =IPARI(17,NIN)
C
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)

      IF(STARTT.GT.TT) RETURN
      IF(TT.GT.STOPT)  RETURN
C
       CALL I21GAP3(
     1  ITASK ,INTBUF_TAB%IRECTS,NRTS ,NSN ,INTBUF_TAB%NSV,
     2  INTBUF_TAB%VARIABLES(2),IGAP  ,INTBUF_TAB%GAP_S,INTBUF_TAB%VARIABLES(13) ,
     .  				     INTBUF_TAB%VARIABLES(6)  ,
     3 INTBUF_TAB%VARIABLES(16),INTBUF_TAB%VARIABLES(8),INTBUF_TAB%VARIABLES(5),THKNOD,
     .  				       INTBUF_TAB%VARIABLES(19),
     4  INTBUF_TAB%VARIABLES(23),INTBUF_TAB%VARIABLES(22),INTBUF_TAB%VARIABLES(32),IDEL,
     .  				       INTBUF_TAB%THKNOD0   ,
     5  INTBUF_TAB%GAP_S(1+NSN) ,INTBUF_TAB%VARIABLES(46))

C
      RETURN
      END
