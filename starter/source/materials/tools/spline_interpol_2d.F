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
Chd|  SPLINE_INTERPOL_2D            source/materials/tools/spline_interpol_2d.F
Chd|-- called by -----------
Chd|        TEST_SPLINE                   source/materials/tools/test_splines.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPLINE_INTERPOL_2D(PTX, PTY ,KNOTS, T,  NX  ,NY  )
C-----------------------------------------------
      ! FOR A GIVEN POSITION T in [0,1] which is position on [P1,P2], 
      ! GET VALUE ON SPLINE P0-P1-P2-P3 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
      ! INPUT  - PTS(0:3,:) are 4 control points
      ! INPUT  - T is in [0,1]  : T=0 => VAL = P(2,:)
      ! OUTPUT - C is interpolated value.
      ! OUTPUT - C_D is 1st derivative
      ! OUTPUT - C_DD is 2nd derivatice
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT(IN)    :: T
      my_real, INTENT(IN)    :: PTX(4)
      my_real, INTENT(IN)    :: PTY(4)
      my_real, INTENT(IN)    :: KNOTS(4)
      my_real, INTENT(OUT)   :: NX,NY
c      my_real, INTENT(INOUT) :: C(4),C_D(4),C_DD(4)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: DK21,DK32,DK43,DK31,DK42,KT1,KT2,KT3,KT4
      my_real :: TT  ! in [T1,T2]=[KNOT(2),KNOT(3)]
      my_real :: A1X,A1Y,A2X,A2Y,A3X,A3Y,B1X,B1Y,B2X,B2Y
      my_real :: CX,CY,C_DX,C_DY,C_DDX,C_DDY
      my_real :: A1PX,A1PY,A2PX,A2PY,A3PX,A3PY,B1PX,B1PY,B2PX,B2PY
      my_real :: B1PPX,B1PPY,B2PPX,B2PPY
C-----------------------------------------------
C   S o u r c e   L i n e s
c=======================================================================
      DK21 = KNOTS(2) - KNOTS(1)
      DK32 = KNOTS(3) - KNOTS(2)
      DK43 = KNOTS(4) - KNOTS(3)
      DK31 = KNOTS(3) - KNOTS(1)
      DK42 = KNOTS(4) - KNOTS(2)
      TT   = KNOTS(2) + T * DK32
      KT1  = KNOTS(1) - TT
      KT2  = KNOTS(2) - TT
      KT3  = KNOTS(3) - TT
      KT4  = KNOTS(4) - TT
c
      A1X  = (KT2*PTX(1) - KT1*PTX(2)) / DK21
      A1Y  = (KT2*PTY(1) - KT1*PTY(2)) / DK21
      A2X  = (KT3*PTX(2) - KT2*PTX(3)) / DK32
      A2Y  = (KT3*PTY(2) - KT2*PTY(3)) / DK32
      A3X  = (KT4*PTX(3) - KT3*PTX(4)) / DK43
      A3Y  = (KT4*PTY(3) - KT3*PTY(4)) / DK43
c      
      B1X  = (KT3*A1X - KT1*A2X) / DK31
      B1Y  = (KT3*A1Y - KT1*A2Y) / DK31
      B2X  = (KT4*A2X - KT2*A3X) / DK42
      B2Y  = (KT4*A2Y - KT2*A3Y) / DK42
c
      NX   = (KT3*B1X - KT2*B2X) / DK32 
      NY   = (KT3*B1Y - KT2*B2Y) / DK32 
c
      A1PX = (PTX(2) - PTX(1)) / DK21
      A1PY = (PTY(2) - PTY(1)) / DK21
      A2PX = (PTX(3) - PTX(2)) / DK32
      A2PY = (PTY(3) - PTY(2)) / DK32
      A3PX = (PTX(4) - PTX(3)) / DK43
      A3PY = (PTY(4) - PTY(3)) / DK43
      B1PX = (A2X-A1X)/ DK31 + KT3 / DK31*A1PX - KT1 / DK31*A2PX  
      B1PY = (A2Y-A1Y)/ DK31 + KT3 / DK31*A1PY - KT1 / DK31*A2PY  
      B2PX = (A3X-A2X)/ DK42 + KT4 / DK42*A2PX - KT2 / DK42*A3PX 
      B2PY = (A3Y-A2Y)/ DK42 + KT4 / DK42*A2PY - KT2 / DK42*A3PY 
!      C_DX = (B2-B1Y)/ DK32 + KT3 / DK32*B1P - KT2 / DK32*B2P
!      C_DY = (B2-B1)/ DK32 + KT3 / DK32*B1P - KT2 / DK32*B2P
      B1PPX= TWO*(A2PX - A1PX) / DK31
      B1PPY= TWO*(A2PY - A1PY) / DK31
      B2PPX= TWO*(A3PX - A2PX) / DK42 
      B2PPY= TWO*(A3PY - TWO*A2PY) / DK42 
!      C_DDX= (TWO*B2P-TWO*B1P + KT3*B1PP - KT2*B2PP) / DK32
!      C_DDY= (TWO*B2P-TWO*B1P + KT3*B1PP - KT2*B2PP) / DK32
c------------
      RETURN
      END SUBROUTINE SPLINE_INTERPOL_2D     
