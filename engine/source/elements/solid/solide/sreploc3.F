Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  SREPLOC3                      source/elements/solid/solide/sreploc3.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S10KE3                        source/elements/solid/solide10/s10ke3.F
Chd|        S20KE3                        source/elements/solid/solide20/s20ke3.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S4KE3                         source/elements/solid/solide4/s4ke3.F
Chd|        SCOOR3                        source/elements/solid/solide/scoor3.F
Chd|        SCOOR3_FVM                    source/ale/alefvm/scoor3_fvm.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SREPLOC3(
     1   RX,      RY,      RZ,      SX,
     2   SY,      SZ,      TX,      TY,
     3   TZ,      E1X,     E2X,     E3X,
     4   E1Y,     E2Y,     E3Y,     E1Z,
     5   E2Z,     E3Z,     NEL)
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
      INTEGER, INTENT(IN) :: NEL
C     REAL
      my_real, INTENT(IN),  DIMENSION(MVSIZ) ::
     .   RX, RY, RZ, SX, SY, SZ, TX, TY, TZ
      my_real, INTENT(OUT),  DIMENSION(MVSIZ) ::
     .   E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .  SUMA
C=======================================================================
C     Orthogonalisation du repere
C       E1=R, E3=R^S, E2=E3^E1                            
C--------------------------------
      DO I=1,NEL                                      
        SUMA = SQRT(RX(I)**2+RY(I)**2+RZ(I)**2)          
        IF (SUMA > ZERO) SUMA=ONE/SUMA                      
        E1X(I) = RX(I)*SUMA                              
        E1Y(I) = RY(I)*SUMA                              
        E1Z(I) = RZ(I)*SUMA                              
        E3X(I) = E1Y(I)*SZ(I) - E1Z(I)*SY(I)              
        E3Y(I) = E1Z(I)*SX(I) - E1X(I)*SZ(I)              
        E3Z(I) = E1X(I)*SY(I) - E1Y(I)*SX(I)              
        SUMA = SQRT(E3X(I)**2+E3Y(I)**2+E3Z(I)**2)       
        IF (SUMA > ZERO) SUMA=ONE/SUMA                      
        E3X(I) = E3X(I)*SUMA                             
        E3Y(I) = E3Y(I)*SUMA                             
        E3Z(I) = E3Z(I)*SUMA                             
        E2X(I) = E3Y(I)*E1Z(I) - E3Z(I)*E1Y(I)                 
        E2Y(I) = E3Z(I)*E1X(I) - E3X(I)*E1Z(I)                 
        E2Z(I) = E3X(I)*E1Y(I) - E3Y(I)*E1X(I)                 
        SUMA = SQRT(E2X(I)**2+E2Y(I)**2+E2Z(I)**2)         
        IF (SUMA > ZERO) SUMA=ONE/SUMA                      
        E2X(I) = E2X(I)*SUMA                             
        E2Y(I) = E2Y(I)*SUMA                             
        E2Z(I) = E2Z(I)*SUMA                             
      ENDDO                                             
C-------------
      RETURN
      END SUBROUTINE SREPLOC3
