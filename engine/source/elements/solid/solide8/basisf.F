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
Chd|  BASISF                        source/elements/solid/solide8/basisf.F
Chd|-- called by -----------
Chd|        S8DERI3                       source/elements/solid/solide8/s8deri3.F
Chd|        SDLEN8                        source/elements/solid/solidez/szforc3.F
Chd|        SDLEN_SMS                     source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        BASIS8                        source/elements/solid/solide8/basis8.F
Chd|====================================================================
      SUBROUTINE BASISF(H,P1,P2,P3,IPT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPT
C     REAL ou REAL*8
      my_real
     .   H(*), P1(*), P2(*), P3(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL ou REAL*8
      my_real
     .   G(3,8)
      DATA   G/-.5773502691896,-.5773502691896,-.5773502691896,
     1          .5773502691896,-.5773502691896,-.5773502691896,
     2          .5773502691896, .5773502691896,-.5773502691896,
     3         -.5773502691896, .5773502691896,-.5773502691896,
     4         -.5773502691896,-.5773502691896, .5773502691896,
     5          .5773502691896,-.5773502691896, .5773502691896,
     6          .5773502691896, .5773502691896, .5773502691896,
     7         -.5773502691896, .5773502691896, .5773502691896/
C
      CALL BASIS8(G(1,IPT),G(2,IPT),G(3,IPT),H,P1,P2,P3)
      RETURN
      END
