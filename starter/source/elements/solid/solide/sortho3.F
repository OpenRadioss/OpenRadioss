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
Chd|====================================================================
Chd|  SORTHO3                       source/elements/solid/solide/sortho3.F
Chd|-- called by -----------
Chd|        S4REFSTA3                     source/elements/solid/solide4/s4refsta3.F
Chd|        SRCOOR3                       source/elements/solid/solide/srcoor3.F
Chd|        SREFSTA3                      source/elements/solid/solide/srefsta3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SORTHO3(
     .    RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,     
     .    E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  )
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
C     REAL ou REAL*8
      my_real
     .   RX(*), RY(*), RZ(*),
     .   SX(*), SY(*), SZ(*), 
     .   TX(*), TY(*), TZ(*),  
     .   E1X(*), E1Y(*), E1Z(*),
     .   E2X(*), E2Y(*), E2Z(*),
     .   E3X(*), E3Y(*), E3Z(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,NITER
C     REAL ou REAL*8
      my_real
     .   ux(MVSIZ),uy(MVSIZ),uz(MVSIZ),
     .   vx(MVSIZ),vy(MVSIZ),vz(MVSIZ),
     .   wx(MVSIZ),wy(MVSIZ),wz(MVSIZ)
      my_real
     .   aa,bb
      DATA NITER/3/
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO I=LFT,LLT
      ux(I)=rx(I)
      uy(I)=ry(I)
      uz(I)=rz(I)
      vx(I)=sx(I)
      vy(I)=sy(I)
      vz(I)=sz(I)
      wx(I)=tx(I)
      wy(I)=ty(I)
      wz(I)=tz(I)
      ENDDO
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c     norme r s t
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO 50 I=LFT,LLT
      aa = sqrt(ux(I)*ux(I) + uy(I)*uy(I) + uz(I)*uz(I))
      if ( aa.ne.ZERO) aa = UN/ aa
      ux(I) = ux(I) * aa
      uy(I) = uy(I) * aa
      uz(I) = uz(I) * aa
      aa = sqrt(vx(I)*vx(I) + vy(I)*vy(I) + vz(I)*vz(I))
      if ( aa.ne.ZERO) aa = UN / aa
      vx(I) = vx(I) * aa
      vy(I) = vy(I) * aa
      vz(I) = vz(I) * aa
      aa = sqrt(wx(I)*wx(I) + wy(I)*wy(I) + wz(I)*wz(I))
      if ( aa.ne.ZERO) aa = UN / aa
      wx(I) = wx(I) * aa
      wy(I) = wy(I) * aa
      wz(I) = wz(I) * aa
 50   CONTINUE
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c     iterations
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      N=0
111   CONTINUE
      N=N+1
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO 100 I=LFT,LLT
       e1x(I) = vy(I) * wz(I) - vz(I) * wy(I) + ux(I)
       e1y(I) = vz(I) * wx(I) - vx(I) * wz(I) + uy(I)
       e1z(I) = vx(I) * wy(I) - vy(I) * wx(I) + uz(I)
c
       e2x(I) = wy(I) * uz(I) - wz(I) * uy(I) + vx(I)
       e2y(I) = wz(I) * ux(I) - wx(I) * uz(I) + vy(I)
       e2z(I) = wx(I) * uy(I) - wy(I) * ux(I) + vz(I)
c
       e3x(I) = uy(I) * vz(I) - uz(I) * vy(I) + wx(I)
       e3y(I) = uz(I) * vx(I) - ux(I) * vz(I) + wy(I)
       e3z(I) = ux(I) * vy(I) - uy(I) * vx(I) + wz(I)
c
       bb = sqrt(e1x(I)*e1x(I) + e1y(I)*e1y(I) + e1z(I)*e1z(I))
       if ( bb.ne.ZERO) bb = UN / bb
       ux(I) = e1x(I) * bb
       uy(I) = e1y(I) * bb
       uz(I) = e1z(I) * bb
c
       bb = sqrt(e2x(I)*e2x(I) + e2y(I)*e2y(I) + e2z(I)*e2z(I))
       if ( bb.ne.ZERO) bb = UN / bb
       vx(I) = e2x(I) * bb
       vy(I) = e2y(I) * bb
       vz(I) = e2z(I) * bb
c
       bb = sqrt(e3x(I)*e3x(I) + e3y(I)*e3y(I) + e3z(I)*e3z(I))
       if ( bb.ne.ZERO) bb = UN / bb
       wx(I) = e3x(I) * bb
       wy(I) = e3y(I) * bb
       wz(I) = e3z(I) * bb
c
 100  CONTINUE
      IF (N.LT.NITER) GOTO 111
c     norme et orthogonalisation
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO 200 I=LFT,LLT
      e1x(I) = ux(I)
      e1y(I) = uy(I)
      e1z(I) = uz(I)
c
      e3x(I) = e1y(I) * vz(I) - e1z(I) * vy(I)
      e3y(I) = e1z(I) * vx(I) - e1x(I) * vz(I)
      e3z(I) = e1x(I) * vy(I) - e1y(I) * vx(I)
c
      aa = sqrt(e3x(I)*e3x(I) + e3y(I)*e3y(I) + e3z(I)*e3z(I))
      if ( aa.ne.ZERO) aa = UN / aa
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
