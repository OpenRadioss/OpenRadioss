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
!||    mean_node_norm2   ../starter/source/initial_conditions/inivol/mean_node_norm2.F
!||--- called by ------------------------------------------------------
!||    getphase          ../starter/source/initial_conditions/inivol/getphase.F
!||====================================================================
      SUBROUTINE MEAN_NODE_NORM2(IN,NOD_NORMAL,NX,NY,NZ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: IN(4)
      my_real,INTENT(IN) :: NX,NY,NZ
      my_real,INTENT(INOUT) :: NOD_NORMAL(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER r,NBNOD
      my_real AAA, X0,Y0,Z0
      my_real X01,Y01,Z01, X02,Y02,Z02, X03,Y03,Z03, X04,Y04,Z04
      my_real XN1,YN1,ZN1, XN2,YN2,ZN2, XN3,YN3,ZN3, XN4,YN4,ZN4
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(IN(4) /= IN(3))THEN
        NOD_NORMAL(1,IN(1))=NOD_NORMAL(1,IN(1))+NX
        NOD_NORMAL(2,IN(1))=NOD_NORMAL(2,IN(1))+NY
        NOD_NORMAL(3,IN(1))=NOD_NORMAL(3,IN(1))+NZ

        NOD_NORMAL(1,IN(2))=NOD_NORMAL(1,IN(2))+NX
        NOD_NORMAL(2,IN(2))=NOD_NORMAL(2,IN(2))+NY
        NOD_NORMAL(3,IN(2))=NOD_NORMAL(3,IN(2))+NZ

        NOD_NORMAL(1,IN(3))=NOD_NORMAL(1,IN(3))+NX
        NOD_NORMAL(2,IN(3))=NOD_NORMAL(2,IN(3))+NY
        NOD_NORMAL(3,IN(3))=NOD_NORMAL(3,IN(3))+NZ

        NOD_NORMAL(1,IN(4))=NOD_NORMAL(1,IN(4))+NX
        NOD_NORMAL(2,IN(4))=NOD_NORMAL(2,IN(4))+NY
        NOD_NORMAL(3,IN(4))=NOD_NORMAL(3,IN(4))+NZ
      ELSE
        NOD_NORMAL(1,IN(1))=NOD_NORMAL(1,IN(1))+NX
        NOD_NORMAL(2,IN(1))=NOD_NORMAL(2,IN(1))+NY
        NOD_NORMAL(3,IN(1))=NOD_NORMAL(3,IN(1))+NZ

        NOD_NORMAL(1,IN(2))=NOD_NORMAL(1,IN(2))+NX
        NOD_NORMAL(2,IN(2))=NOD_NORMAL(2,IN(2))+NY
        NOD_NORMAL(3,IN(2))=NOD_NORMAL(3,IN(2))+NZ

        NOD_NORMAL(1,IN(3))=NOD_NORMAL(1,IN(3))+NX
        NOD_NORMAL(2,IN(3))=NOD_NORMAL(2,IN(3))+NY
        NOD_NORMAL(3,IN(3))=NOD_NORMAL(3,IN(3))+NZ
      ENDIF
C-----------------------------------------------
      RETURN
      END
