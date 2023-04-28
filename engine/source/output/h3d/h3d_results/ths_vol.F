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
Chd|  THS_VOL                       source/output/h3d/h3d_results/h3d_solid_scalar_1.F
Chd|-- called by -----------
Chd|        H3D_SOLID_SCALAR_1            source/output/h3d/h3d_results/h3d_solid_scalar_1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE THS_VOL(RX, RY, RZ, SX, SY, SZ, TX, TY, TZ,DET,NEL)
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
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: RX,RY,RZ
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: SX,SY,SZ
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: TX,TY,TZ
      my_real, DIMENSION(MVSIZ), INTENT(OUT) :: DET
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real JAC1, JAC2, JAC3, JAC4, JAC5, JAC6, JAC7, JAC8, JAC9 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO I=1,NEL
       JAC1 = RX(I) 
       JAC2 = RY(I) 
       JAC3 = RZ(I) 
       JAC4 = SX(I) 
       JAC5 = SY(I) 
       JAC6 = SZ(I) 
       JAC7 = TX(I) 
       JAC8 = TY(I) 
       JAC9 = TZ(I) 
       DET(I) = JAC1 * (JAC5 * JAC9 - JAC8 * JAC6) + 
     .          JAC2 * (JAC6 * JAC7 - JAC4 * JAC9) + 
     .          JAC3 * (JAC4 * JAC8 - JAC7 * JAC5)
      ENDDO 
c-----------
      RETURN
      END
