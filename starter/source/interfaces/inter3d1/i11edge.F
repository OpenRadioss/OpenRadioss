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
C
Chd|====================================================================
Chd|  I11EDGE                       source/interfaces/inter3d1/i11edge.F
Chd|-- called by -----------
Chd|        LECINS                        source/interfaces/interf1/lecins.F
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I11EDGE(IPARI,INTBUF_TAB)
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
      INTEGER IPARI(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NRTS, NRTM,NSN,NMN,I,NPTR,OLDPTR
C
      NRTS=IPARI(3)
      NRTM=IPARI(4)
      NSN =IPARI(5)
      NMN =IPARI(6)
C
      !compact IRTS on 2*NRTS
      DO I = 1, NRTS !KD(10)
        INTBUF_TAB%IRECTS(2*(I-1)+1) = INTBUF_TAB%IRECTS(4*(I-1)+1)
        INTBUF_TAB%IRECTS(2*(I-1)+2) = INTBUF_TAB%IRECTS(4*(I-1)+2)
      ENDDO

      INTBUF_TAB%S_IRECTS = 2*NRTS
C
      !compact IRTM on 2*NRTM
      DO I = 1, NRTM !(KD(11)
        INTBUF_TAB%IRECTM(2*(I-1)+1) = INTBUF_TAB%IRECTM(4*(I-1)+1)
        INTBUF_TAB%IRECTM(2*(I-1)+2) = INTBUF_TAB%IRECTM(4*(I-1)+2)
      ENDDO

      INTBUF_TAB%S_IRECTM = 2*NRTM
C
      RETURN
      END
C

