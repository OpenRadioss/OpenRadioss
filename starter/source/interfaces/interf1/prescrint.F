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
Chd|  PRESCRINT                     source/interfaces/interf1/prescrint.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        PRESEGMT                      source/interfaces/interf1/presegmt.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUFSCRATCH_MOD             source/interfaces/interf1/intbufscratch_mod.F
Chd|====================================================================
      SUBROUTINE PRESCRINT(IPARI, INTBUF_TAB,INSCR)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE INTBUFSCRATCH_MOD
      USE INTBUFDEF_MOD 
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com02_c.inc"
#include      "com03_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(SCRATCH_STRUCT_) INSCR(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  NI, NSN, NMN, NTY, NRTS, NRTM, NST, NMT,LFI
C=======================================================================
      LFI  = 1
      DO NI=1,NINTER
        NRTS   =IPARI(3,NI)
        NRTM   =IPARI(4,NI)
        NSN    =IPARI(5,NI)
        NMN    =IPARI(6,NI)
        NTY    =IPARI(7,NI)
        IF (NTY == 1)THEN
          CALL PRESEGMT(INTBUF_TAB(NI)%IRECTS,INTBUF_TAB(NI)%NSV,NRTS,NSN,NST)
          LFI = 1 + NMN + 1+NSN + NST
        ELSEIF(NTY == 2)THEN
          CALL PRESEGMT(INTBUF_TAB(NI)%IRECTS,INTBUF_TAB(NI)%NSV,NRTS,NSN,NST)
          CALL PRESEGMT(INTBUF_TAB(NI)%IRECTM,INTBUF_TAB(NI)%MSR,NRTM,NMN,NMT)
          LFI = 1 + NMN + NSN + 1+NSN +1+NMN + NST + NMT
        ENDIF
        INSCR(NI)%SINSCR = LFI
      ENDDO
C-----
      RETURN
      END
