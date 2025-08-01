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
!||    morthlock3   ../engine/source/elements/solid/solide4/morthlock3.F
!||--- called by ------------------------------------------------------
!||    s10ke3       ../engine/source/elements/solid/solide10/s10ke3.F
!||    s4ke3        ../engine/source/elements/solid/solide4/s4ke3.F
!||====================================================================
      SUBROUTINE MORTHLOCK3(LFT,LLT ,GAMA,E1X,E2X,E3X,
     2                      E1Y,E2Y ,E3Y ,E1Z,E2Z,E3Z)
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
      INTEGER LFT,LLT
      my_real  
     .  E1X(MVSIZ) , E1Y(MVSIZ) , E1Z(MVSIZ) , 
     .  E2X(MVSIZ) , E2Y(MVSIZ) , E2Z(MVSIZ) , 
     .  E3X(MVSIZ) , E3Y(MVSIZ) , E3Z(MVSIZ) ,GAMA(MVSIZ,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real 
     .   S11, S12, S13, S21, S22, S31,S32,S1,S2
C-----gama -> orthotropic system [{a}{b}{c}]---
        DO I=LFT,LLT
          S11 = GAMA(I,1)*E1X(I)+GAMA(I,2)*E2X(I)+GAMA(I,3)*E3X(I)
          S21 = GAMA(I,1)*E1Y(I)+GAMA(I,2)*E2Y(I)+GAMA(I,3)*E3Y(I)
          S31 = GAMA(I,1)*E1Z(I)+GAMA(I,2)*E2Z(I)+GAMA(I,3)*E3Z(I)
C          
          S12 = GAMA(I,4)*E1X(I)+GAMA(I,5)*E2X(I)+GAMA(I,6)*E3X(I)
          S22 = GAMA(I,4)*E1Y(I)+GAMA(I,5)*E2Y(I)+GAMA(I,6)*E3Y(I)
          S32 = GAMA(I,4)*E1Z(I)+GAMA(I,5)*E2Z(I)+GAMA(I,6)*E3Z(I)
          GAMA(I,1) = S11                                  
          GAMA(I,2) = S21                                  
          GAMA(I,3) = S31 
          GAMA(I,4) = S12                                  
          GAMA(I,5) = S22                                  
          GAMA(I,6) = S32
       END DO
C
      RETURN
      END
