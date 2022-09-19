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
Chd|  I2VIT27                       source/interfaces/interf/i2vit27.F
Chd|-- called by -----------
Chd|        INTTI2V                       source/interfaces/interf/intti2v.F
Chd|-- calls ---------------
Chd|        I2ROT3_27                     source/interfaces/interf/i2vit3.F
Chd|        I2VIT3_27                     source/interfaces/interf/i2vit3.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I2VIT27(INTBUF_TAB,IPARI,WEIGHT,NSN ,
     .                   X    ,V   ,A    ,VR   ,AR   ,
     .                   MS  ,IN   )
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
      INTEGER NSN
      INTEGER IPARI(*), WEIGHT(*)
      my_real X(*), V(*), A(*),MS(*),IN(*),AR(*),VR(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NMN,NINDXC
      INTEGER NSVC(NSN)
C=======================================================================
      NMN    = IPARI(6)
      NINDXC = 0
      NSVC(1:NSN) = 0
c
      DO I=1,NSN
        IF (INTBUF_TAB%IRUPT(I) == 0) THEN
          NINDXC = NINDXC + 1
          NSVC(I) = INTBUF_TAB%NSV(I)
        ENDIF
      ENDDO
c
      IF (NINDXC > 0) THEN
        CALL I2VIT3_27(NSN       ,NMN       ,A         ,INTBUF_TAB%IRECTM,
     .              INTBUF_TAB%CSTS,INTBUF_TAB%MSR,NSVC,INTBUF_TAB%IRTLM,
     .               V         ,MS        ,WEIGHT    ,INTBUF_TAB%NMAS)
c
C       IF (IRODDL /= 0) THEN
          CALL I2ROT3_27(NSN       ,NMN     ,AR        ,INTBUF_TAB%IRECTM,
     .                INTBUF_TAB%CSTS,INTBUF_TAB%MSR  ,NSVC,INTBUF_TAB%IRTLM,
     .                VR        ,IN        ,A         ,V         ,X         ,
     .                INTBUF_TAB%SINER,INTBUF_TAB%DPARA,INTBUF_TAB%MSEGTYP2)
         ENDIF
C      ENDIF
C-----------
      RETURN
      END
