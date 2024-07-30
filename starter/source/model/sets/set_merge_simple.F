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
      !||====================================================================
      !||    set_merge_simple           ../starter/source/model/sets/set_merge_simple.F
      !||--- called by ------------------------------------------------------
      !||    elt_box                    ../starter/source/model/sets/fill_clause_elt_box.F
      !||    fill_clause_elt_box        ../starter/source/model/sets/fill_clause_elt_box.F
      !||    fill_clause_node_box       ../starter/source/model/sets/fill_clause_node_box.F
      !||    fill_clause_rbody_box      ../starter/source/model/sets/fill_clause_rbody_box.F
      !||    insert_clause_in_set       ../starter/source/model/sets/insert_clause_in_set.F
      !||    node_box                   ../starter/source/model/sets/fill_clause_node_box.F
      !||    rbody_box                  ../starter/source/model/sets/fill_clause_rbody_box.F
      !||    sort_set                   ../starter/source/model/sets/sort_sets.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE SET_MERGE_SIMPLE( SET_ENTITY,    NB_SET_ENTITY,
     *                             CLAUSE_ENTITY, NB_CLAUSE_ENTITY,
     *                             RESULT       , NB_RESULT,
     *                             CLAUSE_OPERATOR)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Apply Operators on simple set
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME                                 DESCRIPTION                         
C
C     SET_ENTITY,    NB_SET_ENTITY       : Current SET
C     CLAUSE_ENTITY, NB_CLAUSE_ENTITY    : Clause to merge
C     RESULT       , NB_RESULT           : Stores the result
C     CLAUSE_OPERATOR                      OPERATOR
C============================================================================
C-----------------------------------------------
C   D e f i n i t i o n s 
C-----------------------------------------------
#include      "set_def.inc"
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NB_SET_ENTITY, NB_CLAUSE_ENTITY, CLAUSE_OPERATOR

      INTEGER, DIMENSION(NB_SET_ENTITY), INTENT(IN) :: SET_ENTITY
      INTEGER, DIMENSION(NB_CLAUSE_ENTITY), INTENT(IN) :: CLAUSE_ENTITY

      INTEGER :: NB_RESULT
      INTEGER RESULT(*)
                        
      IF( CLAUSE_OPERATOR == SET_ADD) THEN
           CALL UNION_2_SORTED_SETS( SET_ENTITY, NB_SET_ENTITY  ,
     *                               CLAUSE_ENTITY, NB_CLAUSE_ENTITY ,
     *                               RESULT,        NB_RESULT      )
      ENDIF

      IF( CLAUSE_OPERATOR == SET_DELETE) THEN
           CALL DIFFERENCE_2_SORTED_SETS ( SET_ENTITY, NB_SET_ENTITY  ,
     *                                     CLAUSE_ENTITY, NB_CLAUSE_ENTITY ,
     *                                     RESULT,        NB_RESULT      )
      ENDIF
         
      IF( CLAUSE_OPERATOR == SET_INTERSECT) THEN
           CALL INTERSECT_2_SORTED_SETS ( SET_ENTITY, NB_SET_ENTITY  ,
     *                                    CLAUSE_ENTITY, NB_CLAUSE_ENTITY ,
     *                                    RESULT,        NB_RESULT      )
      ENDIF

      END
