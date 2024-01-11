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
Chd|  INSERT_CLAUSE_IN_SET          source/model/sets/insert_clause_in_set.F
Chd|-- called by -----------
Chd|        CREATE_SETCOL_CLAUSE          source/model/sets/create_setcol_clause.F
Chd|        CREATE_SET_CLAUSE             source/model/sets/create_set_clause.F
Chd|        CREATE_SET_COLLECT            source/model/sets/create_setcol_clause.F
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|-- calls ---------------
Chd|        CREATE_LINE_FROM_ELEMENT      source/model/sets/create_line_from_element.F
Chd|        CREATE_LINE_FROM_SURFACE      source/model/sets/create_line_from_surface.F
Chd|        CREATE_NODE_FROM_ELEMENT      source/model/sets/create_node_from_element.F
Chd|        CREATE_SURFACE_FROM_ELEMENT   source/model/sets/create_surface_from_element.F
Chd|        SET_MERGE_SIMPLE              source/model/sets/set_merge_simple.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SET_SCRATCH_MOD               ../common_source/modules/setdef_mod.F
Chd|====================================================================
      SUBROUTINE INSERT_CLAUSE_IN_SET(SET       ,CLAUSE   ,CLAUSE_OPERATOR,
     .                                IXS       ,IXS10    ,IXS20     ,IXS16     ,IXQ    ,
     .                                IXC       ,IXTG     ,IXT       ,IXP       ,IXR    ,
     .                                IXX       ,KXX      ,KXSP      ,GEO       ,SH4TREE,
     .                                SH3TREE   ,KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC,
     .                                KNOD2ELTG ,NOD2ELTG ,IPARTC    ,IPARTG    ,IPARTS ,
     .                                IPART     ,OPT_A    ,OPT_O     ,KNOD2ELQ  ,NOD2ELQ,
     .                                X         ,KEYSET   ,OPT_E     ,DELBUF    )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Apply the clause to the current set
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     SET           Set Structure - Current SET
C     CLAUSE        Filled CLAUSE
C============================================================================
C-----------------------------------------------
C   D e f i n i t i o n s 
C-----------------------------------------------
#include      "set_def.inc"
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE HM_OPTION_READ_MOD
      USE SETDEF_MOD
      USE MESSAGE_MOD
      USE SET_SCRATCH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (SET_) ::  SET
      TYPE (SET_) ::  CLAUSE
      TYPE (SET_SCRATCH) ::  DELBUF
      INTEGER CLAUSE_OPERATOR

      INTEGER  OPT_A,OPT_O,OPT_E
      INTEGER IXS(NIXS,*),IXS10(6,*),IXS16(8,*),IXS20(12,*),
     .        IXQ(NIXQ,*),IXC(NIXC,*),IXTG(NIXTG,*),IXT(NIXT,*),
     .        IXP(NIXP,*),IXR(NIXR,*),IXX(*),KXX(*),KXSP(NISP,*),
     .        SH4TREE(*),SH3TREE(*),KNOD2ELS(*),KNOD2ELC(*),KNOD2ELTG(*),
     .        KNOD2ELQ(*),NOD2ELS(*),NOD2ELC(*),NOD2ELTG(*),NOD2ELQ(*),
     .        IPARTS(*),IPARTC(*),IPARTG(*),IPART(LIPART1,*)
      my_real
     .    GEO(NPROPG,*),X(3,*)
      CHARACTER KEYSET*ncharfield
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER, DIMENSION(:), ALLOCATABLE :: RESULT
      INTEGER, DIMENSION(:), ALLOCATABLE :: NODES
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: SURF
      INTEGER NEW_SIZE,NS,ND_SIZE,OPERATOR
C-----------------------------------------------
C     NODES 
C-----------------------------------------------
      IF(  CLAUSE%NB_NODE > 0) THEN

         IF( SET%NB_NODE > 0 ) THEN

            ALLOCATE(RESULT(SET%NB_NODE + CLAUSE%NB_NODE ))                  ! Results SET

            CALL SET_MERGE_SIMPLE( SET%NODE,    SET%NB_NODE    ,
     *                             CLAUSE%NODE, CLAUSE%NB_NODE ,
     *                             RESULT,      NEW_SIZE       ,
     *                             CLAUSE_OPERATOR)

            IF (ALLOCATED(SET%NODE)) DEALLOCATE (SET%NODE)
            ALLOCATE(SET%NODE(NEW_SIZE))

            SET%NODE(1:NEW_SIZE) = RESULT(1:NEW_SIZE)
            SET%NB_NODE=NEW_SIZE

            DEALLOCATE(RESULT)

         ELSE   ! SET is empty fill it with clause when ADD

            IF ( CLAUSE_OPERATOR == SET_ADD) THEN 
               IF (ALLOCATED(SET%NODE)) DEALLOCATE (SET%NODE)
               ALLOCATE(SET%NODE(CLAUSE%NB_NODE) )

               SET%NB_NODE=CLAUSE%NB_NODE
               SET%NODE(1:CLAUSE%NB_NODE)=CLAUSE%NODE(1:CLAUSE%NB_NODE)

            ENDIF

         ENDIF
      ELSE
        IF ( CLAUSE_OPERATOR == SET_INTERSECT) THEN              ! SET Intersection with Empty clause gives Empty SET
            IF (SET%NB_NODE > 0)THEN
               IF(ALLOCATED(SET%NODE)) DEALLOCATE(SET%NODE)
               SET%NB_NODE=0
            ENDIF
        ENDIF
      ENDIF
C-----------------------------------------------
C     PARTS
C-----------------------------------------------
      IF(  CLAUSE%NB_PART > 0) THEN

         IF( SET%NB_PART > 0 ) THEN

            ALLOCATE(RESULT(SET%NB_PART + CLAUSE%NB_PART ))                  ! Results SET

            CALL SET_MERGE_SIMPLE( SET%PART,    SET%NB_PART    ,
     *                             CLAUSE%PART, CLAUSE%NB_PART ,
     *                             RESULT,      NEW_SIZE       ,
     *                             CLAUSE_OPERATOR             )
         
            IF (ALLOCATED(SET%PART)) DEALLOCATE (SET%PART)
            ALLOCATE(SET%PART(NEW_SIZE))

            SET%PART(1:NEW_SIZE) = RESULT(1:NEW_SIZE)
            SET%NB_PART=NEW_SIZE

            DEALLOCATE(RESULT)

         ELSE   ! SET is empty fill it with clause when ADD
            IF ( CLAUSE_OPERATOR == SET_ADD) THEN 
               IF (ALLOCATED(SET%PART)) DEALLOCATE (SET%PART)
               ALLOCATE(SET%PART(CLAUSE%NB_PART) )

               SET%NB_PART=CLAUSE%NB_PART
               SET%PART(1:SET%NB_PART)=CLAUSE%PART(1:SET%NB_PART)

            ENDIF

         ENDIF
      ELSE
        IF ( CLAUSE_OPERATOR == SET_INTERSECT) THEN              ! SET Intersection with Empty clause gives Empty SET
            IF (SET%NB_PART > 0)THEN
               IF(ALLOCATED(SET%PART)) DEALLOCATE(SET%PART)
               SET%NB_PART=0
            ENDIF
        ENDIF
      ENDIF
C-----------------------------------------------
C     SOLIDS
C-----------------------------------------------
      IF(  CLAUSE%NB_SOLID > 0) THEN

         IF( SET%NB_SOLID > 0 ) THEN

            ALLOCATE(RESULT(SET%NB_SOLID + CLAUSE%NB_SOLID ))                  ! Results SET

            CALL SET_MERGE_SIMPLE( SET%SOLID,    SET%NB_SOLID    ,
     *                             CLAUSE%SOLID, CLAUSE%NB_SOLID ,
     *                             RESULT,       NEW_SIZE        ,
     *                             CLAUSE_OPERATOR)
         
            IF (ALLOCATED(SET%SOLID)) DEALLOCATE (SET%SOLID)
            ALLOCATE(SET%SOLID(NEW_SIZE))

            SET%SOLID(1:NEW_SIZE) = RESULT(1:NEW_SIZE)
            SET%NB_SOLID=NEW_SIZE

            DEALLOCATE(RESULT)

         ELSE   ! SET is empty fill it with clause when ADD

            IF ( CLAUSE_OPERATOR == SET_ADD) THEN 
               IF (ALLOCATED(SET%SOLID)) DEALLOCATE (SET%SOLID)
               ALLOCATE(SET%SOLID(CLAUSE%NB_SOLID) )

               SET%NB_SOLID=CLAUSE%NB_SOLID
               SET%SOLID(1:CLAUSE%NB_SOLID)=CLAUSE%SOLID(1:CLAUSE%NB_SOLID)

            ENDIF

         ENDIF
      ELSE
        IF ( CLAUSE_OPERATOR == SET_INTERSECT) THEN              ! SET Intersection with Empty clause gives Empty SET
            IF (SET%NB_SOLID > 0)THEN
               IF(ALLOCATED(SET%SOLID)) DEALLOCATE(SET%SOLID)
               SET%NB_SOLID=0
            ENDIF
        ENDIF
      ENDIF
C-----------------------------------------------
C     SH4N
C-----------------------------------------------
      IF(  CLAUSE%NB_SH4N > 0) THEN

         IF( SET%NB_SH4N > 0 ) THEN

            ALLOCATE(RESULT(SET%NB_SH4N + CLAUSE%NB_SH4N ))                  ! Results SET

            CALL SET_MERGE_SIMPLE( SET%SH4N,    SET%NB_SH4N    ,
     *                             CLAUSE%SH4N, CLAUSE%NB_SH4N ,
     *                             RESULT,      NEW_SIZE       ,
     *                             CLAUSE_OPERATOR)

            IF (ALLOCATED(SET%SH4N)) DEALLOCATE (SET%SH4N)
            ALLOCATE(SET%SH4N(NEW_SIZE))

            SET%SH4N(1:NEW_SIZE) = RESULT(1:NEW_SIZE)
            SET%NB_SH4N=NEW_SIZE

            DEALLOCATE(RESULT)

         ELSE   ! SET is empty fill it with clause when ADD

            IF ( CLAUSE_OPERATOR == SET_ADD) THEN 
               IF (ALLOCATED(SET%SH4N)) DEALLOCATE (SET%SH4N)
               ALLOCATE(SET%SH4N(CLAUSE%NB_SH4N) )

               SET%NB_SH4N=CLAUSE%NB_SH4N
               SET%SH4N(1:CLAUSE%NB_SH4N)=CLAUSE%SH4N(1:CLAUSE%NB_SH4N)

            ENDIF

         ENDIF
      ELSE
        IF ( CLAUSE_OPERATOR == SET_INTERSECT) THEN              ! SET Intersection with Empty clause gives Empty SET
            IF (SET%NB_SH4N > 0)THEN
               IF(ALLOCATED(SET%SH4N)) DEALLOCATE(SET%SH4N)
               SET%NB_SH4N=0
            ENDIF
        ENDIF
      ENDIF
C-----------------------------------------------
C     SH3N 
C-----------------------------------------------
      IF(  CLAUSE%NB_SH3N > 0) THEN

         IF( SET%NB_SH3N > 0 ) THEN

            ALLOCATE(RESULT(SET%NB_SH3N + CLAUSE%NB_SH3N ))                  ! Results SET

            CALL SET_MERGE_SIMPLE( SET%SH3N,    SET%NB_SH3N    ,
     *                             CLAUSE%SH3N, CLAUSE%NB_SH3N ,
     *                             RESULT,      NEW_SIZE       ,
     *                             CLAUSE_OPERATOR)

            IF (ALLOCATED(SET%SH3N)) DEALLOCATE (SET%SH3N)
            ALLOCATE(SET%SH3N(NEW_SIZE))

            SET%SH3N(1:NEW_SIZE) = RESULT(1:NEW_SIZE)
            SET%NB_SH3N=NEW_SIZE

            DEALLOCATE(RESULT)

         ELSE   ! SET is empty fill it with clause when ADD

            IF ( CLAUSE_OPERATOR == SET_ADD) THEN 
               IF (ALLOCATED(SET%SH3N)) DEALLOCATE (SET%SH3N)
               ALLOCATE(SET%SH3N(CLAUSE%NB_SH3N) )

               SET%NB_SH3N=CLAUSE%NB_SH3N
               SET%SH3N(1:CLAUSE%NB_SH3N) = CLAUSE%SH3N(1:CLAUSE%NB_SH3N)

            ENDIF

         ENDIF
      ELSE
         IF ( CLAUSE_OPERATOR == SET_INTERSECT) THEN              ! SET Intersection with Empty clause gives Empty SET
            IF (SET%NB_SH3N > 0)THEN
               IF(ALLOCATED(SET%SH3N)) DEALLOCATE(SET%SH3N)
               SET%NB_SH3N=0
            ENDIF
         ENDIF
      ENDIF
C-----------------------------------------------
C     QUAD 
C-----------------------------------------------
      IF(  CLAUSE%NB_QUAD > 0) THEN

         IF( SET%NB_QUAD > 0 ) THEN

            ALLOCATE(RESULT(SET%NB_QUAD + CLAUSE%NB_QUAD ))                  ! Results SET

            CALL SET_MERGE_SIMPLE( SET%QUAD,    SET%NB_QUAD    ,
     *                             CLAUSE%QUAD, CLAUSE%NB_QUAD ,
     *                             RESULT,      NEW_SIZE       ,
     *                             CLAUSE_OPERATOR)

            IF (ALLOCATED(SET%QUAD)) DEALLOCATE (SET%QUAD)
            ALLOCATE(SET%QUAD(NEW_SIZE))

            SET%QUAD(1:NEW_SIZE) = RESULT(1:NEW_SIZE)
            SET%NB_QUAD=NEW_SIZE

            DEALLOCATE(RESULT)

         ELSE   ! SET is empty fill it with clause when ADD

            IF ( CLAUSE_OPERATOR == SET_ADD) THEN 
               IF (ALLOCATED(SET%QUAD)) DEALLOCATE (SET%QUAD)
               ALLOCATE(SET%QUAD(CLAUSE%NB_QUAD) )

               SET%NB_QUAD=CLAUSE%NB_QUAD
               SET%QUAD(1:CLAUSE%NB_QUAD)=CLAUSE%QUAD(1:CLAUSE%NB_QUAD)

            ENDIF

         ENDIF
      ELSE
        IF ( CLAUSE_OPERATOR == SET_INTERSECT) THEN              ! SET Intersection with Empty clause gives Empty SET
            IF (SET%NB_QUAD > 0)THEN
               IF(ALLOCATED(SET%QUAD)) DEALLOCATE(SET%QUAD)
               SET%NB_QUAD=0
            ENDIF
        ENDIF
      ENDIF
C-----------------------------------------------
C     TRIA 
C-----------------------------------------------
      IF(  CLAUSE%NB_TRIA > 0) THEN

         IF( SET%NB_TRIA > 0 ) THEN

            ALLOCATE(RESULT(SET%NB_TRIA + CLAUSE%NB_TRIA ))                  ! Results SET

            CALL SET_MERGE_SIMPLE( SET%TRIA,    SET%NB_TRIA    ,
     *                             CLAUSE%TRIA, CLAUSE%NB_TRIA ,
     *                             RESULT,      NEW_SIZE       ,
     *                             CLAUSE_OPERATOR)

            IF (ALLOCATED(SET%TRIA)) DEALLOCATE (SET%TRIA)
            ALLOCATE(SET%TRIA(NEW_SIZE))

            SET%TRIA(1:NEW_SIZE) = RESULT(1:NEW_SIZE)
            SET%NB_TRIA=NEW_SIZE

            DEALLOCATE(RESULT)

         ELSE   ! SET is empty fill it with clause when ADD

            IF ( CLAUSE_OPERATOR == SET_ADD) THEN 
               IF (ALLOCATED(SET%TRIA)) DEALLOCATE (SET%TRIA)
               ALLOCATE(SET%TRIA(CLAUSE%NB_TRIA) )

               SET%NB_TRIA=CLAUSE%NB_TRIA
               SET%TRIA(1:CLAUSE%NB_TRIA) = CLAUSE%TRIA(1:CLAUSE%NB_TRIA)

            ENDIF

         ENDIF
      ELSE
        IF ( CLAUSE_OPERATOR == SET_INTERSECT) THEN              ! SET Intersection with Empty clause gives Empty SET
            IF (SET%NB_TRIA > 0)THEN
               IF(ALLOCATED(SET%TRIA)) DEALLOCATE(SET%TRIA)
               SET%NB_TRIA=0
            ENDIF
        ENDIF
      ENDIF
C-----------------------------------------------
C     BEAM 
C-----------------------------------------------
      IF(  CLAUSE%NB_BEAM > 0) THEN

         IF( SET%NB_BEAM > 0 ) THEN

            ALLOCATE(RESULT(SET%NB_BEAM + CLAUSE%NB_BEAM ))                  ! Results SET

            CALL SET_MERGE_SIMPLE( SET%BEAM,    SET%NB_BEAM    ,
     *                             CLAUSE%BEAM, CLAUSE%NB_BEAM ,
     *                             RESULT,      NEW_SIZE       ,
     *                             CLAUSE_OPERATOR)

            IF (ALLOCATED(SET%BEAM)) DEALLOCATE (SET%BEAM)
            ALLOCATE(SET%BEAM(NEW_SIZE))

            SET%BEAM(1:NEW_SIZE) = RESULT(1:NEW_SIZE)
            SET%NB_BEAM=NEW_SIZE

            DEALLOCATE(RESULT)

         ELSE   ! SET is empty fill it with clause when ADD

            IF ( CLAUSE_OPERATOR == SET_ADD) THEN 
               IF (ALLOCATED(SET%BEAM)) DEALLOCATE (SET%BEAM)
               ALLOCATE(SET%BEAM(CLAUSE%NB_BEAM) )

               SET%NB_BEAM=CLAUSE%NB_BEAM
               SET%BEAM(1:CLAUSE%NB_BEAM) = CLAUSE%BEAM(1:CLAUSE%NB_BEAM) 
            ENDIF

         ENDIF
      ELSE
        IF ( CLAUSE_OPERATOR == SET_INTERSECT) THEN              ! SET Intersection with Empty clause gives Empty SET
            IF (SET%NB_BEAM > 0)THEN
               IF(ALLOCATED(SET%BEAM)) DEALLOCATE(SET%BEAM)
               SET%NB_BEAM=0
            ENDIF
        ENDIF
      ENDIF
C-----------------------------------------------
C     TRUSS 
C-----------------------------------------------
      IF(  CLAUSE%NB_TRUSS > 0) THEN

         IF( SET%NB_TRUSS > 0 ) THEN

            ALLOCATE(RESULT(SET%NB_TRUSS + CLAUSE%NB_TRUSS ))                  ! Results SET

            CALL SET_MERGE_SIMPLE( SET%TRUSS,    SET%NB_TRUSS    ,
     *                             CLAUSE%TRUSS, CLAUSE%NB_TRUSS ,
     *                             RESULT,      NEW_SIZE       ,
     *                             CLAUSE_OPERATOR)

            IF (ALLOCATED(SET%TRUSS)) DEALLOCATE (SET%TRUSS)
            ALLOCATE(SET%TRUSS(NEW_SIZE))

            SET%TRUSS(1:NEW_SIZE) = RESULT(1:NEW_SIZE)
            SET%NB_TRUSS=NEW_SIZE

            DEALLOCATE(RESULT)

         ELSE   ! SET is empty fill it with clause when ADD

            IF ( CLAUSE_OPERATOR == SET_ADD) THEN 
               IF (ALLOCATED(SET%TRUSS)) DEALLOCATE (SET%TRUSS)
               ALLOCATE(SET%TRUSS(CLAUSE%NB_TRUSS) )

               SET%NB_TRUSS=CLAUSE%NB_TRUSS
               SET%TRUSS(1:CLAUSE%NB_TRUSS) = CLAUSE%TRUSS(1:CLAUSE%NB_TRUSS)

            ENDIF

         ENDIF
      ELSE
        IF ( CLAUSE_OPERATOR == SET_INTERSECT) THEN              ! SET Intersection with Empty clause gives Empty SET
            IF (SET%NB_TRUSS > 0)THEN
               IF(ALLOCATED(SET%TRUSS)) DEALLOCATE(SET%TRUSS)
               SET%NB_TRUSS=0
            ENDIF
        ENDIF
      ENDIF
C-----------------------------------------------
C     SPRING 
C-----------------------------------------------
      IF(  CLAUSE%NB_SPRING > 0) THEN

         IF( SET%NB_SPRING > 0 ) THEN

            ALLOCATE(RESULT(SET%NB_SPRING + CLAUSE%NB_SPRING ))                  ! Results SET

            CALL SET_MERGE_SIMPLE( SET%SPRING,    SET%NB_SPRING    ,
     *                             CLAUSE%SPRING, CLAUSE%NB_SPRING ,
     *                             RESULT,      NEW_SIZE       ,
     *                             CLAUSE_OPERATOR)

            IF (ALLOCATED(SET%SPRING)) DEALLOCATE (SET%SPRING)
            ALLOCATE(SET%SPRING(NEW_SIZE))

            SET%SPRING(1:NEW_SIZE) = RESULT(1:NEW_SIZE)
            SET%NB_SPRING=NEW_SIZE

            DEALLOCATE(RESULT)

         ELSE   ! SET is empty fill it with clause when ADD

            IF ( CLAUSE_OPERATOR == SET_ADD) THEN 
               IF (ALLOCATED(SET%SPRING)) DEALLOCATE (SET%SPRING)
               ALLOCATE(SET%SPRING(CLAUSE%NB_SPRING) )

               SET%NB_SPRING=CLAUSE%NB_SPRING
               SET%SPRING(1:CLAUSE%NB_SPRING)=CLAUSE%SPRING(1:CLAUSE%NB_SPRING)

            ENDIF
         ENDIF
      ELSE
        IF ( CLAUSE_OPERATOR == SET_INTERSECT) THEN              ! SET Intersection with Empty clause gives Empty SET
            IF (SET%NB_SPRING > 0)THEN
               IF(ALLOCATED(SET%SPRING)) DEALLOCATE(SET%SPRING)
               SET%NB_SPRING=0
            ENDIF
        ENDIF
      ENDIF
C-----------------------------------------------
C     SURFACES
C-----------------------------------------------

      !------------------------!
      !    SURFACES  -ELLIPSE- !
      !------------------------!
      IF ( SET%NB_ELLIPSE > 0 ) THEN

         SET%ELLIPSE_IAD_BUFR = CLAUSE%ELLIPSE_IAD_BUFR
         SET%ELLIPSE_ID_MADYMO = CLAUSE%ELLIPSE_ID_MADYMO
         SET%ELLIPSE_N = CLAUSE%ELLIPSE_N
         SET%ELLIPSE_XC = CLAUSE%ELLIPSE_XC
         SET%ELLIPSE_YC = CLAUSE%ELLIPSE_YC
         SET%ELLIPSE_ZC = CLAUSE%ELLIPSE_ZC
         SET%ELLIPSE_A = CLAUSE%ELLIPSE_A
         SET%ELLIPSE_B = CLAUSE%ELLIPSE_B
         SET%ELLIPSE_C = CLAUSE%ELLIPSE_C
         CALL MY_ALLOC(SET%ELLIPSE_SKEW,9)
         SET%ELLIPSE_SKEW(1:9) = CLAUSE%ELLIPSE_SKEW(1:9)
      ELSE
        !------------------------!
        !    classic SURFACES    !
        !------------------------!
        IF( CLAUSE%NB_SURF_SEG > 0) THEN

          IF( SET%NB_SURF_SEG > 0 ) THEN

              !
              ! Low level CPP routines / One per operator
              ! The result is held in CPP Structure 
              ! until SET is reallocated and unstacked
              !
              IF ( CLAUSE_OPERATOR == SET_ADD) THEN                            

                  CALL UNION_SURFACE( SET%SURF_NODES(1,1), SET%SURF_NODES(1,2), 
     .                                SET%SURF_NODES(1,3), SET%SURF_NODES(1,4),
     .                                SET%SURF_ELTYP, SET%SURF_ELEM, SET%NB_SURF_SEG,
     .                                CLAUSE%SURF_NODES(1,1), CLAUSE%SURF_NODES(1,2), 
     .                                CLAUSE%SURF_NODES(1,3), CLAUSE%SURF_NODES(1,4),
     .                                CLAUSE%SURF_ELTYP, CLAUSE%SURF_ELEM, CLAUSE%NB_SURF_SEG,
     .                                NS  )

              ELSEIF ( CLAUSE_OPERATOR == SET_DELETE) THEN

                  CALL DELETE_SURFACE(SET%SURF_NODES(1,1), SET%SURF_NODES(1,2), 
     .                                SET%SURF_NODES(1,3), SET%SURF_NODES(1,4),
     .                                SET%SURF_ELTYP, SET%SURF_ELEM, SET%NB_SURF_SEG,
     .                                CLAUSE%SURF_NODES(1,1), CLAUSE%SURF_NODES(1,2), 
     .                                CLAUSE%SURF_NODES(1,3), CLAUSE%SURF_NODES(1,4),
     .                                CLAUSE%SURF_ELTYP, CLAUSE%SURF_ELEM, CLAUSE%NB_SURF_SEG,
     .                                NS  )

              ELSEIF ( CLAUSE_OPERATOR == SET_INTERSECT)THEN

                  CALL INTERSECT_SURFACE( SET%SURF_NODES(1,1), SET%SURF_NODES(1,2), 
     .                                    SET%SURF_NODES(1,3), SET%SURF_NODES(1,4),
     .                                    SET%SURF_ELTYP, SET%SURF_ELEM, SET%NB_SURF_SEG,
     .                                    CLAUSE%SURF_NODES(1,1), CLAUSE%SURF_NODES(1,2), 
     .                                    CLAUSE%SURF_NODES(1,3), CLAUSE%SURF_NODES(1,4),
     .                                    CLAUSE%SURF_ELTYP, CLAUSE%SURF_ELEM, CLAUSE%NB_SURF_SEG,
     .                                    NS )
              ELSE
                  print*,'Unknown clause operator'
              ENDIF

              DEALLOCATE(SET%SURF_NODES)
              DEALLOCATE(SET%SURF_ELTYP)
              DEALLOCATE(SET%SURF_ELEM)

              CALL MY_ALLOC(SET%SURF_NODES,NS,4)
              CALL MY_ALLOC  (SET%SURF_ELTYP,NS)
              CALL MY_ALLOC  (SET%SURF_ELEM,NS)

              SET%NB_SURF_SEG = NS

              CALL GET_MERGED_SURFACE(SET%SURF_NODES(1,1), 
     .                                SET%SURF_NODES(1,2),
     .                                SET%SURF_NODES(1,3),
     .                                SET%SURF_NODES(1,4),
     .                                SET%SURF_ELTYP,
     .                                SET%SURF_ELEM)

          ELSE      ! SET WAS EMPTY FILL it with Clause if SET_ADD operator

            IF ( CLAUSE_OPERATOR == SET_ADD) THEN
              IF (ALLOCATED (SET%SURF_NODES) ) DEALLOCATE(SET%SURF_NODES)
              IF (ALLOCATED (SET%SURF_ELTYP) ) DEALLOCATE(SET%SURF_ELTYP)
              IF (ALLOCATED (SET%SURF_ELEM) ) DEALLOCATE(SET%SURF_ELEM)

              NEW_SIZE = CLAUSE%NB_SURF_SEG
              CALL MY_ALLOC(SET%SURF_NODES,NEW_SIZE,4)
              CALL MY_ALLOC  (SET%SURF_ELTYP,NEW_SIZE)
              CALL MY_ALLOC  (SET%SURF_ELEM,NEW_SIZE)

              SET%NB_SURF_SEG = NEW_SIZE
              SET%SURF_NODES(1:NEW_SIZE,1:4) = CLAUSE%SURF_NODES(1:NEW_SIZE,1:4)
              SET%SURF_ELTYP(1:NEW_SIZE)     = CLAUSE%SURF_ELTYP(1:NEW_SIZE)
              SET%SURF_ELEM(1:NEW_SIZE)      = CLAUSE%SURF_ELEM(1:NEW_SIZE)

            ENDIF
          ENDIF
        ELSE             
          IF ( CLAUSE_OPERATOR == SET_INTERSECT) THEN              ! SET : Intersection with Empty clause gives Empty SET
            IF (SET%NB_SURF_SEG > 0)THEN
               IF(ALLOCATED(SET%SURF_NODES)) DEALLOCATE(SET%SURF_NODES)
               IF(ALLOCATED(SET%SURF_ELTYP)) DEALLOCATE(SET%SURF_ELTYP)
               IF(ALLOCATED(SET%SURF_ELEM)) DEALLOCATE(SET%SURF_ELEM)
               SET%NB_SURF_SEG=0
            ENDIF
          ENDIF
        ENDIF
      ENDIF ! IF ( SET%NB_ELLIPSE > 0 )
C-----------------------------------------------
C     LINES
C-----------------------------------------------
      IF( CLAUSE%NB_LINE_SEG > 0) THEN

         IF( SET%NB_LINE_SEG > 0 ) THEN

            !
            ! Low level CPP routines / One per operator
            ! The result is held in CPP Structure 
            ! until SET is reallocated and unstacked
            !
            IF ( CLAUSE_OPERATOR == SET_ADD) THEN                            

                CALL UNION_LINE( SET%LINE_NODES(1,1), SET%LINE_NODES(1,2), 
     .                           SET%LINE_ELTYP, SET%LINE_ELEM, SET%NB_LINE_SEG,
     .                           CLAUSE%LINE_NODES(1,1), CLAUSE%LINE_NODES(1,2), 
     .                           CLAUSE%LINE_ELTYP, CLAUSE%LINE_ELEM, CLAUSE%NB_LINE_SEG,
     .                           NEW_SIZE  )

            ELSEIF ( CLAUSE_OPERATOR == SET_DELETE) THEN

                CALL DELETE_LINE ( SET%LINE_NODES(1,1), SET%LINE_NODES(1,2), 
     .                             SET%LINE_ELTYP, SET%LINE_ELEM, SET%NB_LINE_SEG,
     .                             CLAUSE%LINE_NODES(1,1), CLAUSE%LINE_NODES(1,2), 
     .                             CLAUSE%LINE_ELTYP, CLAUSE%LINE_ELEM, CLAUSE%NB_LINE_SEG,
     .                             NEW_SIZE  )

            ELSEIF ( CLAUSE_OPERATOR == SET_INTERSECT)THEN

                CALL INTERSECT_LINE (  SET%LINE_NODES(1,1), SET%LINE_NODES(1,2), 
     .                                 SET%LINE_ELTYP, SET%LINE_ELEM, SET%NB_LINE_SEG,
     .                                 CLAUSE%LINE_NODES(1,1), CLAUSE%LINE_NODES(1,2), 
     .                                 CLAUSE%LINE_ELTYP, CLAUSE%LINE_ELEM, CLAUSE%NB_LINE_SEG,
     .                                 NEW_SIZE    )
            ELSE
                print*,'Unknown clause operator'
            ENDIF

                DEALLOCATE(SET%LINE_NODES)
                DEALLOCATE(SET%LINE_ELTYP)
                DEALLOCATE(SET%LINE_ELEM)

                CALL MY_ALLOC(SET%LINE_NODES,NEW_SIZE,4)
                CALL MY_ALLOC  (SET%LINE_ELTYP,NEW_SIZE)
                CALL MY_ALLOC  (SET%LINE_ELEM,NEW_SIZE)

                SET%NB_LINE_SEG = NEW_SIZE

                CALL GET_MERGED_LINES (SET%LINE_NODES(1,1), 
     .                                 SET%LINE_NODES(1,2),
     .                                 SET%LINE_ELTYP,
     .                                 SET%LINE_ELEM)

         ELSE      ! SET WAS EMPTY FILL it with Clause if SET_ADD operator

                IF ( CLAUSE_OPERATOR == SET_ADD) THEN
                    IF (ALLOCATED (SET%LINE_NODES) ) DEALLOCATE(SET%LINE_NODES)
                    IF (ALLOCATED (SET%LINE_ELTYP) ) DEALLOCATE(SET%LINE_ELTYP)
                    IF (ALLOCATED (SET%LINE_ELEM) ) DEALLOCATE(SET%LINE_ELEM)

                    NEW_SIZE = CLAUSE%NB_LINE_SEG
                    CALL MY_ALLOC(SET%LINE_NODES,NEW_SIZE,2)
                    CALL MY_ALLOC  (SET%LINE_ELTYP,NEW_SIZE)
                    CALL MY_ALLOC  (SET%LINE_ELEM,NEW_SIZE)

                    SET%NB_LINE_SEG = NEW_SIZE
                    SET%LINE_NODES(1:NEW_SIZE,1:2) = CLAUSE%LINE_NODES(1:NEW_SIZE,1:2)
                    SET%LINE_ELTYP(1:NEW_SIZE)     = CLAUSE%LINE_ELTYP(1:NEW_SIZE)
                    SET%LINE_ELEM(1:NEW_SIZE)      = CLAUSE%LINE_ELEM(1:NEW_SIZE)

                ENDIF
         ENDIF
      ELSE             
        IF ( CLAUSE_OPERATOR == SET_INTERSECT) THEN              ! SET : Intersection with Empty clause gives Empty SET
            IF (SET%NB_LINE_SEG > 0)THEN
               IF(ALLOCATED(SET%LINE_NODES)) DEALLOCATE(SET%LINE_NODES)
               IF(ALLOCATED(SET%LINE_ELTYP)) DEALLOCATE(SET%LINE_ELTYP)
               IF(ALLOCATED(SET%LINE_ELEM)) DEALLOCATE(SET%LINE_ELEM)
               SET%NB_LINE_SEG=0
            ENDIF
        ENDIF
      ENDIF

      ! -----------------------------------------------
      !     IN CASE OF DELETE redo NODES_FROM_ELEM 
      !                            SURFACE_FROM_ELEMENT
      !                            LINE_FROM_SURFACE
      ! -----------------------------------------------
      IF(CLAUSE_OPERATOR == SET_DELETE)THEN

        !---
        ! Nodes reconstruction after delete
        !---

        ALLOCATE(NODES(NUMNOD))
        ALLOCATE(RESULT(NUMNOD ))                  ! Results SET

        CALL CREATE_NODE_FROM_ELEMENT(
     .                           IXS   ,IXS10  ,IXS20  ,IXS16  ,IXQ    ,
     .                           IXC   ,IXTG   ,IXT    ,IXP    ,IXR    ,
     .                           IXX   ,KXX    ,KXSP   ,SET ,GEO    ,
     .                           NODES ,ND_SIZE,.TRUE. )
        OPERATOR = SET_ADD
        CALL SET_MERGE_SIMPLE( SET%NODE,    SET%NB_NODE    ,
     .                         NODES, ND_SIZE ,
     .                         RESULT,      NEW_SIZE       ,
     .                         OPERATOR)  


         IF (ALLOCATED(SET%NODE)) DEALLOCATE (SET%NODE)
         ALLOCATE(SET%NODE(NEW_SIZE))

         SET%NODE(1:NEW_SIZE) = RESULT(1:NEW_SIZE)
         SET%NB_NODE=NEW_SIZE

         DEALLOCATE(RESULT)
         DEALLOCATE(NODES)


        !---
        ! Surfs reconstruction after delete
        !---


        CALL CREATE_SURFACE_FROM_ELEMENT(
     .                     IXS       ,IXS10    ,SH4TREE   ,SH3TREE   ,IXC       ,
     .                     IXTG      ,KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC   ,
     .                     KNOD2ELTG ,NOD2ELTG ,IPARTC    ,IPARTG    ,IPARTS    ,
     .                     IPART     ,SET      ,OPT_A     ,OPT_O     ,IXQ       ,
     .                     KNOD2ELQ  ,NOD2ELQ  ,X         ,KEYSET    ,DELBUF    ,
     .                     .TRUE.   )

        !--- void calling UNION_SURFACE with non allocated arrays
        IF(.NOT. ALLOCATED(SET%SURF_NODES))ALLOCATE(SET%SURF_NODES(1,4))
        IF(.NOT. ALLOCATED(SET%SURF_ELTYP))ALLOCATE(SET%SURF_ELTYP(1))
        IF(.NOT. ALLOCATED(SET%SURF_ELEM)) ALLOCATE(SET%SURF_ELEM(1))
        IF(.NOT. ALLOCATED(DELBUF%SURF))   ALLOCATE(DELBUF%SURF(1,6))
      
        CALL UNION_SURFACE(SET%SURF_NODES(1,1), SET%SURF_NODES(1,2), 
     .                     SET%SURF_NODES(1,3), SET%SURF_NODES(1,4),
     .                     SET%SURF_ELTYP, SET%SURF_ELEM, SET%NB_SURF_SEG,
     .                     DELBUF%SURF(1,1), DELBUF%SURF(1,2), 
     .                     DELBUF%SURF(1,3), DELBUF%SURF(1,4),
     .                     DELBUF%SURF(1,5), DELBUF%SURF(1,6), DELBUF%SZ_SURF,
     .                     NS  )

        IF(ALLOCATED(SET%SURF_NODES))DEALLOCATE(SET%SURF_NODES)
        IF(ALLOCATED(SET%SURF_ELTYP))DEALLOCATE(SET%SURF_ELTYP)
        IF(ALLOCATED(SET%SURF_ELEM)) DEALLOCATE(SET%SURF_ELEM)
        IF(ALLOCATED(DELBUF%SURF))   DEALLOCATE(DELBUF%SURF)

        CALL MY_ALLOC(SET%SURF_NODES,NS,4)
        CALL MY_ALLOC(SET%SURF_ELTYP,NS)
        CALL MY_ALLOC(SET%SURF_ELEM,NS)

        SET%NB_SURF_SEG = NS

        CALL GET_MERGED_SURFACE(SET%SURF_NODES(1,1), 
     .                          SET%SURF_NODES(1,2),
     .                          SET%SURF_NODES(1,3),
     .                          SET%SURF_NODES(1,4),
     .                          SET%SURF_ELTYP,
     .                          SET%SURF_ELEM)

        DELBUF%SZ_SURF = 0
        !---
        ! Lines reconstruction after delete
        !---

        ! Line from 1D_ELEMENT
        !-------------------
        CALL CREATE_LINE_FROM_ELEMENT(IXT    ,IXP  ,IXR  ,SET ,DELBUF    ,
     .                                .TRUE. )
        IF(.NOT. ALLOCATED(SET%LINE_NODES))ALLOCATE(SET%LINE_NODES(1,2))
        IF(.NOT. ALLOCATED(SET%LINE_ELTYP))ALLOCATE(SET%LINE_ELTYP(1))
        IF(.NOT. ALLOCATED(SET%LINE_ELEM)) ALLOCATE(SET%LINE_ELEM(1))
        IF(.NOT. ALLOCATED(DELBUF%LINE))   ALLOCATE(DELBUF%LINE(1,4))

        CALL UNION_LINE(SET%LINE_NODES(1,1), SET%LINE_NODES(1,2), 
     .                  SET%LINE_ELTYP, SET%LINE_ELEM, SET%NB_LINE_SEG,
     .                  DELBUF%LINE(1,1), DELBUF%LINE(1,2), 
     .                  DELBUF%LINE(1,3), DELBUF%LINE(1,4), DELBUF%SZ_LINE,
     .                  NEW_SIZE  )

        IF(ALLOCATED(SET%LINE_NODES))DEALLOCATE(SET%LINE_NODES)
        IF(ALLOCATED(SET%LINE_ELTYP))DEALLOCATE(SET%LINE_ELTYP)
        IF(ALLOCATED(SET%LINE_ELEM)) DEALLOCATE(SET%LINE_ELEM)
        IF(ALLOCATED(DELBUF%LINE))   DEALLOCATE(DELBUF%LINE)

        CALL MY_ALLOC(SET%LINE_NODES,NEW_SIZE,4)
        CALL MY_ALLOC  (SET%LINE_ELTYP,NEW_SIZE)
        CALL MY_ALLOC  (SET%LINE_ELEM,NEW_SIZE)

        SET%NB_LINE_SEG = NEW_SIZE

        CALL GET_MERGED_LINES (SET%LINE_NODES(1,1), 
     .                         SET%LINE_NODES(1,2),
     .                         SET%LINE_ELTYP,
     .                         SET%LINE_ELEM)

        DELBUF%SZ_LINE = 0
        

        ! Line from SURFACE
        CALL CREATE_LINE_FROM_SURFACE(SET  ,KEYSET,OPT_A,OPT_E,DELBUF    ,
     .                               .TRUE.)

        IF(.NOT. ALLOCATED(SET%LINE_NODES))ALLOCATE(SET%LINE_NODES(1,2))
        IF(.NOT. ALLOCATED(SET%LINE_ELTYP))ALLOCATE(SET%LINE_ELTYP(1))
        IF(.NOT. ALLOCATED(SET%LINE_ELEM)) ALLOCATE(SET%LINE_ELEM(1))
        IF(.NOT. ALLOCATED(DELBUF%LINE))   ALLOCATE(DELBUF%LINE(1,4))


        CALL UNION_LINE(SET%LINE_NODES(1,1), SET%LINE_NODES(1,2), 
     .                  SET%LINE_ELTYP, SET%LINE_ELEM, SET%NB_LINE_SEG,
     .                  DELBUF%LINE(1,1), DELBUF%LINE(1,2), 
     .                  DELBUF%LINE(1,3), DELBUF%LINE(1,4), DELBUF%SZ_LINE,
     .                  NEW_SIZE  )

        IF(ALLOCATED(SET%LINE_NODES))DEALLOCATE(SET%LINE_NODES)
        IF(ALLOCATED(SET%LINE_ELTYP))DEALLOCATE(SET%LINE_ELTYP)
        IF(ALLOCATED(SET%LINE_ELEM)) DEALLOCATE(SET%LINE_ELEM)
        IF(ALLOCATED(DELBUF%LINE))   DEALLOCATE(DELBUF%LINE)

        CALL MY_ALLOC(SET%LINE_NODES,NEW_SIZE,4)
        CALL MY_ALLOC  (SET%LINE_ELTYP,NEW_SIZE)
        CALL MY_ALLOC  (SET%LINE_ELEM,NEW_SIZE)

        SET%NB_LINE_SEG = NEW_SIZE

        CALL GET_MERGED_LINES (SET%LINE_NODES(1,1), 
     .                         SET%LINE_NODES(1,2),
     .                         SET%LINE_ELTYP,
     .                         SET%LINE_ELEM)

        DELBUF%SZ_LINE = 0

      ENDIF ! IF(CLAUSE_OPERATOR == SET_DELETE)


      END






