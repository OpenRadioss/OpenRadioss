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
!||    tag_node_from_1d_2d_elem   ../starter/source/model/sets/tag_node_from_1D_2D_elem.F
!||--- called by ------------------------------------------------------
!||    create_node_from_element   ../starter/source/model/sets/create_node_from_element.F
!||--- uses       -----------------------------------------------------
!||    message_mod                ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE TAG_NODE_FROM_1D_2D_ELEM(
     .                         IX     ,NIX    ,NIX1   ,NIX2   ,NUMEL,
     .                         ELEM   ,TAGNOD,CLAUSE_NODE,IND)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMEL,NIX1,NIX2,NIX
      INTEGER IX(NIX,*),TAGNOD(*),ELEM(*)
      INTEGER CLAUSE_NODE(*),IND
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II
C===============================================
!
      DO I=1,NUMEL
        II=ELEM(I)
        DO J=NIX1,NIX2
          IF(TAGNOD(IX(J,II)) == 0)THEN
            TAGNOD(IX(J,II)) = 1
            IND = IND+1
            CLAUSE_NODE(IND) = IX(J,II)
          ENDIF
        ENDDO
      ENDDO
C-----------
      RETURN
      END
