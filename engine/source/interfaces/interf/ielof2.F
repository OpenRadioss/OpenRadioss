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
Chd|  IELOF2                        source/interfaces/interf/ielof2.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        IELOF3                        source/interfaces/interf/ielof3.F
Chd|        IELOF5                        source/interfaces/interf/ielof5.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IELOF2(INTBUF_TAB,IXC,IPARI,NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      INTEGER IXC(*), IPARI(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com09_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NRTS, NRTM, NSN, NMN, NTY, NST, MST, IER, IGAP
C-----------------------------------------------
C
      NRTS  =IPARI(3)
      NRTM  =IPARI(4)
      NSN   =IPARI(5)
      NMN   =IPARI(6)
      NTY   =IPARI(7)
      NST   =IPARI(8)
      MST   =IPARI(9)
C  091 +1
      IGAP  =IPARI(21)
C
C
      GOTO(500,500,30,30,30,500,70,500,500)NTY
      GOTO 500
C
 30    CALL IELOF3(
     1 INTBUF_TAB%IRECTS,INTBUF_TAB%STFS,NRTS  ,IXC  ,INTBUF_TAB%STFNS,
     2 INTBUF_TAB%NSEGS,INTBUF_TAB%LNSV,NSN, INTBUF_TAB%NSV,NEL    ,
     3 IER       )
       IF(IER==1)CALL IELOF3(
     1 INTBUF_TAB%IRECTM,INTBUF_TAB%STFM,NRTM  ,IXC,INTBUF_TAB%STFNM,
     2 INTBUF_TAB%NSEGM,INTBUF_TAB%LMSR,NMN , INTBUF_TAB%MSR,NEL    ,
     3 IER       )
      GO TO 500
 70   IF(IDELI7==0)GOTO 500
       CALL IELOF5(
     1 INTBUF_TAB%IRECTM,INTBUF_TAB%STFM,NRTM ,IXC ,INTBUF_TAB%MSR,
     2 NEL       )
      GO TO 500
  500 CONTINUE
C
      RETURN
      END
