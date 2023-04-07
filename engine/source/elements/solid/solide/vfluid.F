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
C --- fluid structure       
Chd|====================================================================
Chd|  VFLUID                        source/elements/solid/solide/prelecflow.F
Chd|-- called by -----------
Chd|        PRELECFLOW                    source/elements/solid/solide/prelecflow.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VFLUID(
     1   UVAR,    NEL,     MLW,     PHI1,
     2   PHI2,    PHI3,    NFT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NFT
      INTEGER NEL,MLW
      my_real
     .   DT2T
      my_real
     .  PHI1(*),PHI2(*),UVAR(*),PHI3(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER I,IE
       my_real
     .  VOL0
C======================================================================|
      DO I=1,NEL
        IE =NFT+I
        
        VOL0 = UVAR(I + 2*NEL)
C        
C density, energy, alpha  
        PHI1(IE) = UVAR(I)
        PHI2(IE) = UVAR(I + NEL) 
        PHI3(IE) = UVAR(I + 20*NEL)
C       
        UVAR(I) =  PHI1(IE)*VOL0
        UVAR(I + NEL) = PHI2(IE)*VOL0
      ENDDO    
C------------------------------------------------------
C     CALCUL DES CONTRAINTES SUIVANT LOIS CONSTITUTIVES
C------------------------------------------------------
      RETURN
      END
