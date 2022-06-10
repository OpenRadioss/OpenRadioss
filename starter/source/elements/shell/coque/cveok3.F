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
C
Chd|====================================================================
Chd|  CVEOK3                        source/elements/shell/coque/cveok3.F
Chd|-- called by -----------
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|        INIRIG_MAT                    source/elements/initia/inirig_mat.F
Chd|        INIVOID                       source/elements/initia/inivoid.F
Chd|-- calls ---------------
Chd|        CCOLI3                        source/elements/shell/coque/ccoli3.F
Chd|====================================================================
      SUBROUTINE CVEOK3(NVC ,NOD ,IX1,IX2,IX3,IX4)
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NVC,NOD
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NVC1, NVC2, NVC3, NVC4
C=======================================================================
      CALL CCOLI3(IX1,NVC1,LFT,LLT)
      CALL CCOLI3(IX2,NVC2,LFT,LLT)
      CALL CCOLI3(IX3,NVC3,LFT,LLT)
      IF (NOD >= 4) THEN
        CALL CCOLI3(IX4,NVC4,LFT,LLT)
      ELSE
        NVC4=0
      ENDIF
C
      NVC = NVC1*8+NVC2*4+NVC3*2+NVC4
c-----------
      RETURN
      END
