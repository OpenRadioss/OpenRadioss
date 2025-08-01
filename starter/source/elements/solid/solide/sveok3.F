Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    sveok3             ../starter/source/elements/solid/solide/sveok3.F
!||--- called by ------------------------------------------------------
!||    multifluid_init3   ../starter/source/multifluid/multifluid_init3.F
!||    sinit3             ../starter/source/elements/solid/solide/sinit3.F
!||--- calls      -----------------------------------------------------
!||    ccoli3             ../starter/source/elements/shell/coque/ccoli3.F
!||====================================================================
      SUBROUTINE SVEOK3(NVC,NOD, 
     .     IX1, IX2, IX3, IX4, IX5, IX6, IX7, IX8)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NVC,NOD
      INTEGER :: IX1(*), IX2(*), IX3(*), IX4(*), IX5(*), IX6(*), IX7(*), IX8(*)

C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  NVC1, NVC2, NVC3, NVC4,NVC5, NVC6, NVC7, NVC8
C
      CALL CCOLI3(IX1,NVC1,LFT,LLT)
      CALL CCOLI3(IX2,NVC2,LFT,LLT)
      CALL CCOLI3(IX3,NVC3,LFT,LLT)
      CALL CCOLI3(IX4,NVC4,LFT,LLT)
      IF (NOD>=4) THEN
         CALL CCOLI3(IX5,NVC5,LFT,LLT)
         CALL CCOLI3(IX6,NVC6,LFT,LLT)
         CALL CCOLI3(IX7,NVC7,LFT,LLT)
         CALL CCOLI3(IX8,NVC8,LFT,LLT)
       ELSE
          NVC5=0
          NVC6=0
          NVC7=0
          NVC8=0
       ENDIF
C
      NVC=NVC1*128+NVC2*64+NVC3*32+NVC4*16+NVC5*8+NVC6*4+NVC7*2+NVC8
C
      RETURN
      END
