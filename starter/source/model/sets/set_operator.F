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
!||    set_operator   ../starter/source/model/sets/set_operator.F
!||--- called by ------------------------------------------------------
!||    hm_set         ../starter/source/model/sets/hm_set.F
!||--- uses       -----------------------------------------------------
!||    set_mod        ../starter/share/modules1/set_mod.F
!||====================================================================
      SUBROUTINE SET_OPERATOR( OPT_D, OPT_O,OPT_G,
     *                         OPT_B, OPT_A,OPT_E,
     *                         OPT_I, OPT_C,
     *                         CLAUSE_OPERATOR )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Defines the SET Operators : SET_ADD, SET_DELETE, SET_INTERSECT
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME                                                 DESCRIPTION
C
C     OPT_D,OPT_O,OPT_G,OPT_B,OPT_A,OPT_E,OPT_I,OPT_C,OP : INPUT - Option Operator
c     CLAUSE OPERATOR : OUTPUT                             CLAUSE_OPERATOR
C============================================================================
C-----------------------------------------------
C   D e f i n i t i o n s 
C-----------------------------------------------

C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SET_MOD , ONLY : SET_ADD,SET_DELETE,SET_INTERSECT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: OPT_D, OPT_O,OPT_G,
     *                       OPT_B, OPT_A,OPT_E,
     *                       OPT_I, OPT_C

      INTEGER, INTENT(OUT) :: CLAUSE_OPERATOR
C-----------------------------------------------
      IF(OPT_D == 1) THEN
          CLAUSE_OPERATOR = SET_DELETE
      ELSEIF (OPT_I == 1) THEN
          CLAUSE_OPERATOR = SET_INTERSECT
      ELSE
          CLAUSE_OPERATOR = SET_ADD
      ENDIF


      END
