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
!||    scortho3   ../engine/source/elements/thickshell/solidec/scortho3.F
!||--- called by ------------------------------------------------------
!||    srcoor3    ../engine/source/elements/solid/solide/srcoor3.F
!||    srcoork    ../engine/source/elements/solid/solide8z/srcoork.F
!||    srep2glo   ../engine/source/elements/sph/srep2glo.F
!||    tsh_dir2   ../engine/source/output/h3d/h3d_results/h3d_skin_tensor.F
!||====================================================================
      SUBROUTINE SCORTHO3(
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
      my_real, INTENT(OUT), DIMENSION(MVSIZ) ::
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
     .   DET,C1,C2 
C-----------------------------------------------
      DO 100 I=1,NEL
C
       E3X(I) = TY(I) * RZ(I) - TZ(I) * RY(I) 
       E3Y(I) = TZ(I) * RX(I) - TX(I) * RZ(I) 
       E3Z(I) = TX(I) * RY(I) - TY(I) * RX(I) 
C
       DET = SQRT(E3X(I)*E3X(I) + E3Y(I)*E3Y(I) + E3Z(I)*E3Z(I))
       IF ( DET/=ZERO) DET = ONE / DET
       E3X(I) = E3X(I) * DET
       E3Y(I) = E3Y(I) * DET
       E3Z(I) = E3Z(I) * DET
C
       C1=SQRT(TX(I)*TX(I)+TY(I)*TY(I)+TZ(I)*TZ(I))
       C2=SQRT(RX(I)*RX(I)+RY(I)*RY(I)+RZ(I)*RZ(I))
       E1X(I)=TX(I)*C2 +(RY(I) * E3Z(I) - RZ(I) * E3Y(I))*C1  
       E1Y(I)=TY(I)*C2 +(RZ(I) * E3X(I) - RX(I) * E3Z(I))*C1  
       E1Z(I)=TZ(I)*C2 +(RX(I) * E3Y(I) - RY(I) * E3X(I))*C1
       DET = SQRT(E1X(I)*E1X(I) + E1Y(I)*E1Y(I) + E1Z(I)*E1Z(I))
       IF ( DET/=ZERO) DET = ONE / DET
       E1X(I) = E1X(I)*DET
       E1Y(I) = E1Y(I)*DET
       E1Z(I) = E1Z(I)*DET
C
       E2X(I) = E3Y(I) * E1Z(I) - E3Z(I) * E1Y(I)
       E2Y(I) = E3Z(I) * E1X(I) - E3X(I) * E1Z(I)
       E2Z(I) = E3X(I) * E1Y(I) - E3Y(I) * E1X(I)
 100  CONTINUE
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      RETURN
      END
