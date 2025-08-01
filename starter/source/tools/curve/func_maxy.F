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
!||    func_maxy        ../starter/source/tools/curve/func_maxy.F
!||--- called by ------------------------------------------------------
!||    law100_upd_nht   ../starter/source/materials/mat/mat100/law100_upd.F
!||--- uses       -----------------------------------------------------
!||    message_mod      ../starter/share/message_module/message_mod.F
!||    table_mod        ../starter/share/modules1/table_mod.F
!||====================================================================
      SUBROUTINE FUNC_MAXY(IDN,FAC,NPC,PLD,MAXY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDN,NPC(*) 
      my_real PLD(*),FAC,MAXY
C-----------------------------------------------
      INTENT(IN)    :: NPC,PLD,IDN
      INTENT(OUT)   :: MAXY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ID,NP1,NP2,K1,PN1,PN2
      my_real DYDX,DX,DY,YY
C========================================= 
          ! COMPUTE MAXIMUM y coordinate  
C========================================= 
          PN1 = NPC(IDN)
          PN2 = NPC(IDN+1)
          MAXY = ZERO
          DO J = PN1+1,PN2-1,2
            YY = PLD(J) 
            MAXY = MAX(YY , MAXY)
          ENDDO
          MAXY    = FAC* MAXY
c-----------
      RETURN
      END
