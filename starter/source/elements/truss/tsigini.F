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
Chd|  TSIGINI                       source/elements/truss/tsigini.F
Chd|-- called by -----------
Chd|        TINIT3                        source/elements/truss/tinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TSIGINI(NSIGTRUSS  ,SIGTRUSS   ,PTTRUSS,  EINT   ,FOR   ,
     .                   G_PLA      ,PLA        ,AREA   )
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSIGTRUSS,G_PLA
      INTEGER PTTRUSS(*)
!     REAL ou REAL*8
      my_real
     .   SIGTRUSS(NSIGTRUSS,*),EINT(*),FOR(*),PLA(*),AREA(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,JJ
C----------------------------------------------------------------------- 
!---
!     CONTRAINTES INITIALES + OTHERS
!---
!
      DO I=LFT,LLT
        II = I+NFT
        JJ = PTTRUSS(II)
        IF (JJ > 0) THEN
!---
          EINT(I) = SIGTRUSS(3,JJ)
          FOR(I)  = SIGTRUSS(4,JJ)
          IF (G_PLA > 0) PLA(I)  = SIGTRUSS(5,JJ)
          AREA(I) = SIGTRUSS(6,JJ)
!---
        ENDIF ! IF (JJ > 0)
      ENDDO ! DO I=LFT,LLT
!---
      RETURN
      END
