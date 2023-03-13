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
Chd|  S10NXT4                       source/elements/solid/solide10/s10nxt4.F
Chd|-- called by -----------
Chd|        ELOFF                         source/elements/eloff.F       
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10NXT4(NX,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real NX(MVSIZ,4,4)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IP,K1,K2,K3,K4
      INTEGER IPERM(4,4)

      DATA IPERM/
     .            2, 4, 3, 1,
     .            4, 1, 3, 2,
     .            1, 4, 2, 3,
     .            1, 2, 3, 4/
      my_real ALPH,BETA
C-----------------------------------------------
      ALPH=ZEP5854102
      BETA=ZEP1381966
C
      DO IP=1,4
        K1 = IPERM(1,IP)
        K2 = IPERM(2,IP)
        K3 = IPERM(3,IP)
        K4 = IPERM(4,IP)
       DO I=1,NEL
        NX(I,K1,IP) = BETA
        NX(I,K2,IP) = BETA
        NX(I,K3,IP) = BETA
        NX(I,K4,IP) = ALPH
       ENDDO
      ENDDO
C
      RETURN
      END

