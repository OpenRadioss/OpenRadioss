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
Chd|  MATV66                        source/elements/solid/solidez/szhour3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MATV66(
     1   CC,      CG,      G33,     V,
     2   U,       NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      my_real
     .   CC(MVSIZ,3,3),CG(MVSIZ,3,3),G33(MVSIZ,3,3),
     .   V(MVSIZ,6),U(MVSIZ,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J
C-----------------------------------------------
       DO I=1,NEL
        U(I,1) =CC(I,1,1)*V(I,1)+CC(I,1,2)*V(I,2)+CC(I,1,3)*V(I,3)
        U(I,2) =CC(I,2,1)*V(I,1)+CC(I,2,2)*V(I,2)+CC(I,2,3)*V(I,3)
        U(I,3) =CC(I,3,1)*V(I,1)+CC(I,3,2)*V(I,2)+CC(I,3,3)*V(I,3)
       END DO
       DO I=1,NEL
        U(I,1) =U(I,1)+CG(I,1,1)*V(I,4)+CG(I,1,2)*V(I,5)+CG(I,1,3)*V(I,6)
        U(I,2) =U(I,2)+CG(I,2,1)*V(I,4)+CG(I,2,2)*V(I,5)+CG(I,2,3)*V(I,6)
        U(I,3) =U(I,3)+CG(I,3,1)*V(I,4)+CG(I,3,2)*V(I,5)+CG(I,3,3)*V(I,6)
       END DO
       DO I=1,NEL
        U(I,4) =CG(I,1,1)*V(I,1)+CG(I,2,1)*V(I,2)+CG(I,3,1)*V(I,3)
        U(I,5) =CG(I,1,2)*V(I,1)+CG(I,2,2)*V(I,2)+CG(I,3,2)*V(I,3)
        U(I,6) =CG(I,1,3)*V(I,1)+CG(I,2,3)*V(I,2)+CG(I,3,3)*V(I,3)
       END DO
       DO I=1,NEL
        U(I,4) =U(I,4)+G33(I,1,1)*V(I,4)+G33(I,1,2)*V(I,5)+G33(I,1,3)*V(I,6)
        U(I,5) =U(I,5)+G33(I,2,1)*V(I,4)+G33(I,2,2)*V(I,5)+G33(I,2,3)*V(I,6)
        U(I,6) =U(I,6)+G33(I,3,1)*V(I,4)+G33(I,3,2)*V(I,5)+G33(I,3,3)*V(I,6)
       END DO
C      
      RETURN
      END
