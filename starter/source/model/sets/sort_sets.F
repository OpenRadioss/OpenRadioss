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
Chd|  SORT_SET                      source/model/sets/sort_sets.F 
Chd|-- called by -----------
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        CREATE_SETCOL_ARRAY           source/model/sets/create_setcol_clause.F
Chd|        CREATE_SET_ARRAY              source/model/sets/create_set_clause.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_GET_STRING_INDEX           source/devtools/hm_reader/hm_get_string_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        SET_MERGE_SIMPLE              source/model/sets/set_merge_simple.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MAPPING_OPTION_MOD            share/modules1/dichotomy_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE SORT_SET (LSUBMODEL ,MAP_TABLES, SET_LIST ,SET,CLAUSE)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Main Routine to Sort SETs according to their dependencies (/SET of /SET)
C   If a SET has SET clause (child SETs), ensure that those are treated before.
C
C   All Sets are parsed to find Child Sets, fill a Graph with SET & Childs 
C   Go through the Graph to generate the list
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C     LSUBMODEL     SUBMODEL Structure
C     MAP_TABLES    Mapping table structure
C     SET_LIST      List of sorted SETs
C     SET           SET Structure / ACTIV Flag will be defined for /SET/COLLECT
C===========================================================================================
C-----------------------------------------------
C   D e f i n i t i o n s 
C-----------------------------------------------
#include      "set_def.inc"
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE SETDEF_MOD
      USE MAPPING_OPTION_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
      TYPE(MAPPING_STRUCT_) :: MAP_TABLES
      TYPE (SET_), DIMENSION(NSETS),INTENT(INOUT) :: SET
      INTEGER SET_LIST(NSETS)
      TYPE (SET_) ::  CLAUSE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IGS,IGS2,ID1,ID2,IG,I,J,SET_CLAUSE_SIZE,SET_ARRAY_SIZE,NEW_SIZE,IERROR,S,T,ID
      INTEGER SET_ID,ISET_TYPE,CLAUSES_MAX,ITMP,ICODE,NCOLLECT,SETCOL_ARRAY_SIZE
      INTEGER OPT_D,OPT_O,OPT_G,OPT_B,OPT_A,OPT_E,OPT_I,OPT_C
      INTEGER IDEBUG
      LOGICAL IS_AVAILABLE
      INTEGER, DIMENSION(:),ALLOCATABLE  :: SET_ARRAY,SET_CLAUSE_ARRAY,RESULT,SETCOL_ARRAY
      INTEGER, DIMENSION(:),ALLOCATABLE  :: COLLECT_LIST,IS_COLLECT
      INTEGER CLAUSE_OPERATOR
      
C-----------------------------------------------
C     Characters
      CHARACTER MESS*40
      CHARACTER KEYSET*ncharfield,SET_TYPE*ncharfield,KEY*ncharkey,KEY_TYPE*ncharfield
      CHARACTER TITLE*nchartitle,SET_TITLE*nchartitle,KEYPART*ncharkey,TITLE2*nchartitle
C-----------------------------------------------
      IDEBUG=0

      IF (IDEBUG == 1)THEN
         print*,' ' 
         print*,' ' 
         print*,' -----------------------------------------------'
         print*,'  SORTING SETS'
         print*,' -----------------------------------------------'
         print*,' ' 
      ENDIF

      ALLOCATE(SET_ARRAY(NSETS))
      ALLOCATE(SETCOL_ARRAY(NSETS))
      ALLOCATE(SET_CLAUSE_ARRAY(NSETS))
      ALLOCATE(IS_COLLECT(NSETS))
      ALLOCATE(RESULT(NSETS))
      ALLOCATE(COLLECT_LIST(MAP_TABLES%NSET_COLLECT))
      

      !-------------------------------------------------
      ! SET%IS_ACTIVE & /SET/COLLECT
      ! ----------------------------
      ! Loops to define which set is active
      ! In /SET/COLLECT only one SET of the SERIES is active
      ! And assembled by the others.
      ! All secondary SETs needs to be treated before the 
      ! the active SET. 
      ! /SET/GENERAL : all SETs are active
      !-------------------------------------------------

      DO IGS=1,NSETS
        SET(IGS)%SET_ACTIV=-1
        IS_COLLECT(IGS)=0
      ENDDO
      
      IF (MAP_TABLES%NSET_COLLECT > 0)THEN

        IGS = MAP_TABLES%ISETCOLM(1,2)
        IS_COLLECT(IGS)=1
        SET(IGS)%SET_ACTIV=1
        
        DO I=2,MAP_TABLES%NSET_COLLECT
           IGS = MAP_TABLES%ISETCOLM(I,2)
           IGS2 = MAP_TABLES%ISETCOLM(I-1,2)

           ID1 = MAP_TABLES%ISETCOLM(I,1)
           ID2 = MAP_TABLES%ISETCOLM(I-1,1)

           IS_COLLECT(IGS)=1
           IF(ID1 /= ID2) THEN
             SET(IGS)%SET_ACTIV=1
           ELSE
             SET(IGS)%SET_ACTIV=0
           ENDIF
        ENDDO
      ENDIF 
      DO IGS=1,NSETS
         IF(IS_COLLECT(IGS) == 0) SET(IGS)%SET_ACTIV=1
      ENDDO     
      !-------------------------------------------------
    
      CALL HM_OPTION_START('/SET')

      DO IGS=1,NSETS
        
        SET_ARRAY_SIZE=0

        CALL HM_OPTION_READ_KEY (LSUBMODEL,
     .                           OPTION_ID   = SET_ID,
     .                           OPTION_TITR = SET_TITLE,
     .                           KEYWORD2    = KEY)


        CALL HM_GET_STRING('set_Type' , SET_TYPE ,ncharfield, IS_AVAILABLE)
        CALL HM_GET_INTV  ('iset_Type', ISET_TYPE,IS_AVAILABLE,LSUBMODEL)

        CALL HM_GET_INTV('clausesmax',CLAUSES_MAX,IS_AVAILABLE,LSUBMODEL)




          !  Parse all clauses find SET clauses 
          ! -------------------------
          DO J=1,CLAUSES_MAX  ! max KEY's of the current /SET
             CALL HM_GET_STRING_INDEX('KEY_type', KEYSET, J, ncharline, IS_AVAILABLE)
  
             CALL HM_GET_INT_ARRAY_INDEX('opt_D',OPT_D,J,IS_AVAILABLE,LSUBMODEL)
             CALL HM_GET_INT_ARRAY_INDEX('opt_O',OPT_O,J,IS_AVAILABLE,LSUBMODEL)
             CALL HM_GET_INT_ARRAY_INDEX('opt_G',OPT_G,J,IS_AVAILABLE,LSUBMODEL)
             CALL HM_GET_INT_ARRAY_INDEX('opt_B',OPT_B,J,IS_AVAILABLE,LSUBMODEL)
             CALL HM_GET_INT_ARRAY_INDEX('opt_A',OPT_A,J,IS_AVAILABLE,LSUBMODEL) 
             CALL HM_GET_INT_ARRAY_INDEX('opt_E',OPT_E,J,IS_AVAILABLE,LSUBMODEL)
             CALL HM_GET_INT_ARRAY_INDEX('opt_I',OPT_I,J,IS_AVAILABLE,LSUBMODEL)
             CALL HM_GET_INT_ARRAY_INDEX('opt_C',OPT_C,J,IS_AVAILABLE,LSUBMODEL)

           
             IF(TRIM(KEYSET) == 'SET' )THEN
  
              !  get the list of SETs for the current clause
              ! ---------------------------------------------
               SET_CLAUSE_SIZE = 0
               CALL CREATE_SET_ARRAY(SET_CLAUSE_ARRAY  , SET_CLAUSE_SIZE, 
     .                               MAP_TABLES%ISETM  , MAP_TABLES%NSET_GENERAL,
     .                               J  ,OPT_G ,IS_AVAILABLE ,
     .                               LSUBMODEL,CLAUSE,0)              

               IF( SET_CLAUSE_SIZE > 0 ) THEN
  
                 NEW_SIZE = 0
                 CALL SET_MERGE_SIMPLE( SET_ARRAY       , SET_ARRAY_SIZE  ,
     *                                  SET_CLAUSE_ARRAY, SET_CLAUSE_SIZE ,
     *                                  RESULT          , NEW_SIZE        ,
     *                                  SET_ADD )

                 SET_ARRAY(1:NEW_SIZE) = RESULT(1:NEW_SIZE)
                 SET_ARRAY_SIZE = NEW_SIZE
               ENDIF

             ELSEIF (TRIM(KEYSET) == 'SETCOL' )THEN
                CALL CREATE_SETCOL_ARRAY(SET,SETCOL_ARRAY,SETCOL_ARRAY_SIZE  ,
     *                                   MAP_TABLES%ISETCOLM,MAP_TABLES%NSET_COLLECT,
     *                                   J,OPT_G ,IS_AVAILABLE ,
     *                                   LSUBMODEL) 

                IF(SETCOL_ARRAY_SIZE > 0 ) THEN
  
                 NEW_SIZE = 0
                 CALL SET_MERGE_SIMPLE( SET_ARRAY       , SET_ARRAY_SIZE    ,
     *                                  SETCOL_ARRAY    , SETCOL_ARRAY_SIZE ,
     *                                  RESULT          , NEW_SIZE        ,
     *                                  SET_ADD )

                 SET_ARRAY(1:NEW_SIZE) = RESULT(1:NEW_SIZE)
                 SET_ARRAY_SIZE = NEW_SIZE
               ENDIF

                
             ENDIF 


          ENDDO           ! DO J=1,CLAUSES_MAX


          IF(TRIM(KEY) == 'COLLECT')THEN                 ! SET COLLECT - find all other SETs with Same ID. 

            IF (SET(IGS)%SET_ACTIV==1 ) THEN

              DO J=1,MAP_TABLES%NSET_COLLECT             ! Find all SET with same ID but "inactive" / set them as dependant from this SET.

                 ID = MAP_TABLES%ISETCOLM(J,1)
                 IG = MAP_TABLES%ISETCOLM(J,2)

                 IF (ID > SET_ID) EXIT                   ! ISETCOLM is sorted by UID, when ID is greater we have finished.
                 
                
                 IF( ID == SET_ID .AND. SET(IG)%SET_ACTIV==0)THEN      
                    SET_ARRAY_SIZE = SET_ARRAY_SIZE + 1
                    SET_ARRAY(SET_ARRAY_SIZE)=IG
                 ENDIF

              ENDDO
            ENDIF

          ENDIF

        IF (IDEBUG == 1)THEN
             WRITE(6,'(A,I8,A,I8,A,I8)') 'SET ',SET_ID,'-> ',IGS,' Number of Child list : ',SET_ARRAY_SIZE
             WRITE(6,'(A, 100I8)') 'Child List      ',( SET_ARRAY(T), T=1,SET_ARRAY_SIZE)
             WRITE(6,'(A)') ' ' 
         ENDIF

         ! Create an Edge in the Dependency Graph
         CALL SET_GRAPH_ADD_SET ( IGS, SET_ARRAY, SET_ARRAY_SIZE) 
           

      ENDDO
 
      !  ALL Edges are done
      ! --------------------
      CALL SET_GRAPH_SORT ( SET_LIST , IERROR)
      
      IF (IERROR < 0) THEN
         print*,'ERROR CIRCULAR DEPENDENCY ON SET ',-IERROR
         CALL ARRET(2)
      ENDIF

      IF (IDEBUG == 1)THEN
         print*,' ' 
         print*,' -----------------------------------------------'
         WRITE(6,'(A)') 'SORTED SETS'
         print*,SET_LIST(1:NSETS)
         print*,' ' 
         print*,' -----------------------------------------------'
         print*,' ' 
      ENDIF

      CALL SET_GRAPH_CLEAN()

      END
