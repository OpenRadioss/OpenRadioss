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
!||    sortho3    ../engine/source/elements/solid/solide/sortho3.F
!||--- called by ------------------------------------------------------
!||    pre_heph   ../engine/source/output/anim/generate/tensor6.F
!||    sr8coor3   ../engine/source/elements/solid/solide8/sr8coor3.F
!||    srcoor3    ../engine/source/elements/solid/solide/srcoor3.F
!||    srcoork    ../engine/source/elements/solid/solide8z/srcoork.F
!||    srep2glo   ../engine/source/elements/sph/srep2glo.F
!||====================================================================
      SUBROUTINE SORTHO3(
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
      my_real, DIMENSION(MVSIZ), INTENT(IN)  ::
     .   RX, RY, RZ, SX, SY, SZ, TX, TY, TZ
      my_real, DIMENSION(MVSIZ), INTENT(OUT) ::
     .   E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,NITER
C     REAL
      my_real
     .   aa,bb
      my_real, DIMENSION(MVSIZ) ::
     .   UX, UY, UZ, VX, VY, VZ, WX, WY, WZ
      DATA NITER/3/
C=======================================================================
c     norme r s t
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO 50 I=1,NEL
      aa = sqrt(rx(I)*rx(I) + ry(I)*ry(I) + rz(I)*rz(I))
      if ( aa/=ZERO) aa = ONE / aa
      Ux(I) = rx(I) * aa
      Uy(I) = ry(I) * aa
      Uz(I) = rz(I) * aa
      aa = sqrt(sx(I)*sx(I) + sy(I)*sy(I) + sz(I)*sz(I))
      if ( aa/=ZERO) aa = ONE / aa
      Vx(I) = sx(I) * aa
      Vy(I) = sy(I) * aa
      Vz(I) = sz(I) * aa
      aa = sqrt(tx(I)*tx(I) + ty(I)*ty(I) + tz(I)*tz(I))
      if ( aa/=ZERO) aa = ONE / aa
      Wx(I) = tx(I) * aa
      Wy(I) = ty(I) * aa
      Wz(I) = tz(I) * aa
 50   CONTINUE
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c     iterations
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      N=0
111   CONTINUE
      N=N+1
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO 100 I=1,NEL
       e1x(I) = Vy(I) * Wz(I) - Vz(I) * Wy(I) + Ux(I)
       e1y(I) = Vz(I) * Wx(I) - Vx(I) * Wz(I) + Uy(I)
       e1z(I) = Vx(I) * Wy(I) - Vy(I) * Wx(I) + Uz(I)
c
       e2x(I) = Wy(I) * Uz(I) - Wz(I) * Uy(I) + Vx(I)
       e2y(I) = Wz(I) * Ux(I) - Wx(I) * Uz(I) + Vy(I)
       e2z(I) = Wx(I) * Uy(I) - Wy(I) * Ux(I) + Vz(I)
c
       e3x(I) = Uy(I) * Vz(I) - Uz(I) * Vy(I) + Wx(I)
       e3y(I) = Uz(I) * Vx(I) - Ux(I) * Vz(I) + Wy(I)
       e3z(I) = Ux(I) * Vy(I) - Uy(I) * Vx(I) + Wz(I)
c
       bb = sqrt(e1x(I)*e1x(I) + e1y(I)*e1y(I) + e1z(I)*e1z(I))
       if ( bb/=ZERO) bb = ONE / bb
       Ux(I) = e1x(I) * bb
       Uy(I) = e1y(I) * bb
       Uz(I) = e1z(I) * bb
c
       bb = sqrt(e2x(I)*e2x(I) + e2y(I)*e2y(I) + e2z(I)*e2z(I))
       if ( bb/=ZERO) bb = ONE / bb
       Vx(I) = e2x(I) * bb
       Vy(I) = e2y(I) * bb
       Vz(I) = e2z(I) * bb
c
       bb = sqrt(e3x(I)*e3x(I) + e3y(I)*e3y(I) + e3z(I)*e3z(I))
       if ( bb/=ZERO) bb = ONE / bb
       Wx(I) = e3x(I) * bb
       Wy(I) = e3y(I) * bb
       Wz(I) = e3z(I) * bb
c
 100  CONTINUE
      IF (N < NITER) GOTO 111
c     norme et orthogonalisation
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO 200 I=1,NEL
      e1x(I) = Ux(I)
      e1y(I) = Uy(I)
      e1z(I) = Uz(I)
c
      e3x(I) = e1y(I) * Vz(I) - e1z(I) * Vy(I)
      e3y(I) = e1z(I) * Vx(I) - e1x(I) * Vz(I)
      e3z(I) = e1x(I) * Vy(I) - e1y(I) * Vx(I)
c
      aa = sqrt(e3x(I)*e3x(I) + e3y(I)*e3y(I) + e3z(I)*e3z(I))
      if ( aa/=ZERO) aa = ONE / aa
      e3x(I) = e3x(I) * aa
      e3y(I) = e3y(I) * aa
      e3z(I) = e3z(I) * aa
c
      e2x(I) = e3y(I) * e1z(I) - e3z(I) * e1y(I)
      e2y(I) = e3z(I) * e1x(I) - e3x(I) * e1z(I)
      e2z(I) = e3x(I) * e1y(I) - e3y(I) * e1x(I)
 200  CONTINUE
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      RETURN
      END 
