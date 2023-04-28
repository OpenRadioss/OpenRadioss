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
Chd|  ROTO_SIG2D                    source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|-- called by -----------
Chd|        H3D_FLD_STRAIN                source/output/h3d/h3d_results/h3d_fld_strain.F
Chd|        H3D_SKIN_TENSOR               source/output/h3d/h3d_results/h3d_skin_tensor.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ROTO_SIG2D(JFT,JLT,SIG,DIR)
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
      INTEGER JFT, JLT
      my_real
     .   SIG(3,MVSIZ), DIR(MVSIZ,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   X2,Y2,XY,XYS2,SIGN(3)
C-----------------------------------------------
      DO I=JFT,JLT
         X2=DIR(I,1)*DIR(I,1)
         Y2=DIR(I,2)*DIR(I,2)
         XY=DIR(I,1)*DIR(I,2)
         XYS2=TWO*XY*SIG(3,I)
         SIGN(1) = X2*SIG(1,I)+Y2*SIG(2,I)-XYS2
         SIGN(2) = Y2*SIG(1,I)+X2*SIG(2,I)+XYS2
         SIGN(3) =(SIG(1,I)-SIG(2,I))*XY+(X2-Y2)*SIG(3,I)
         SIG(1,I) = SIGN(1)
         SIG(2,I) = SIGN(2)
         SIG(3,I) = SIGN(3)
      ENDDO
C-----------
      RETURN
      END
