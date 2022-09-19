Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  CREATE_NODE_CLAUSE            source/model/sets/create_node_clause.F
Chd|-- called by -----------
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|-- calls ---------------
Chd|        CREATE_NODE_BOX               source/model/sets/create_node_box.F
Chd|        CREATE_NODE_LIST              source/model/sets/create_node_clause.F
Chd|        CREATE_NODE_LIST_G            source/model/sets/create_node_clause.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CREATE_NODE_CLAUSE(
     .                        CLAUSE   ,ITABM1  ,JCLAUSE ,OPT_G ,IS_AVAILABLE ,
     .                        LSUBMODEL,OPT_B   ,IBOX    ,X     ,SKEW         ,
     .                        SET_TITLE,KEYSET  )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Treat the NODE Clause, read NODEs from HM_READER & fill clause
C   Calls CREATE_NODE_LIST (simple list)
C   Calls CREATE_NODE_LIST_G (NODE_G : All nodes from a MIN to MAX with increment)
C------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     CLAUSE        (SET structure) Clause to be treated
C     ITABM1        MAP Table UID -> LocalID
C     JCLAUSE       parameter with HM_READER (current clause read)
C     Opt_G         Opt_G operator 1 if PART_G is set, 0 else
C     IS_AVAILABLE  Bool / Result of HM_interface
C     LSUBMODEL     SUBMODEL Structure.
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SETDEF_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE HM_OPTION_READ_MOD
      USE OPTIONDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "nchar_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JCLAUSE,OPT_G,OPT_B
      LOGICAL :: IS_AVAILABLE
      INTEGER, INTENT(IN), DIMENSION(NUMNOD,2) :: ITABM1
      my_real
     .        X(3,*),SKEW(LSKEW,*)
      CHARACTER KEYSET*ncharfield,SET_TITLE*nchartitle
C-----------------------------------------------
      TYPE (SET_) ::  CLAUSE
      TYPE(SUBMODEL_DATA),INTENT(IN):: LSUBMODEL(*)
      TYPE (BOX_)  , DIMENSION(NBBOX)  :: IBOX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
!
        IF ( OPT_G == 1 ) THEN

             CALL CREATE_NODE_LIST_G(CLAUSE,  ITABM1  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL)

        ELSEIF ( OPT_G == 0 .AND. OPT_B == 0 ) THEN

             CALL CREATE_NODE_LIST  (CLAUSE,  ITABM1  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL)

        ELSEIF ( OPT_B == 1 ) THEN

             CALL CREATE_NODE_BOX   (CLAUSE  ,ITABM1  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     .                               IBOX    ,X       ,SKEW    ,SET_TITLE    ,KEYSET   )
        ENDIF
C-----------------------------------------------
      END
Chd|====================================================================
Chd|  CREATE_NODE_LIST              source/model/sets/create_node_clause.F
Chd|-- called by -----------
Chd|        CREATE_NODE_CLAUSE            source/model/sets/create_node_clause.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_INT_ARRAY_2INDEXES     source/devtools/hm_reader/hm_get_int_array_2indexes.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        SET_USRTOS                    source/model/sets/ipartm1.F   
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CREATE_NODE_LIST(
     .               CLAUSE,  ITABM1  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Create NODE Clause from LIST
C------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     CLAUSE        (SET structure) Clause to be treated
C     ITABM1        MAP Table UID -> LocalID
C     JCLAUSE       parameter with HM_READER (current clause read)
C     IS_AVAILABLE  Bool / Result of HM_interface
C     LSUBMODEL     SUBMODEL Structure.
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SETDEF_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JCLAUSE
      LOGICAL :: IS_AVAILABLE
      INTEGER, INTENT(IN), DIMENSION(NUMNOD,2) :: ITABM1
!
      TYPE (SET_) ::  CLAUSE
      TYPE(SUBMODEL_DATA),INTENT(IN):: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IDS,NINDX,LIST_SIZE,IDS_MAX,NODSYS
      INTEGER IWORK(70000)
!
      INTEGER, ALLOCATABLE, DIMENSION(:) :: NODE_READ_TMP,INDEX,SORT
C
      INTEGER SET_USRTOS
      EXTERNAL SET_USRTOS
C=======================================================================

      CALL HM_GET_INT_ARRAY_INDEX('idsmax' ,IDS_MAX ,JCLAUSE,IS_AVAILABLE,LSUBMODEL)

      ALLOCATE(NODE_READ_TMP(IDS_MAX))
      NODE_READ_TMP(1:IDS_MAX) = 0
      ALLOCATE(INDEX(2*IDS_MAX))
      ALLOCATE(SORT(IDS_MAX))

      INDEX = 0

      NINDX = 0
      LIST_SIZE = 0

      ! Read & convert Nodes
      ! ---------------------
      DO I=1,IDS_MAX
         CALL HM_GET_INT_ARRAY_2INDEXES('ids',IDS,JCLAUSE,I,IS_AVAILABLE,LSUBMODEL)

         NODSYS = SET_USRTOS(IDS,ITABM1,NUMNOD)
         IF(NODSYS == 0)THEN
           ! Node was not found. Issue a Warning & Skip.
           CALL ANCMSG(MSGID=1902,ANMODE=ANINFO,
     .                             MSGTYPE=MSGWARNING,
     .                             I1 = CLAUSE%SET_ID,
     .                             I2=IDS,
     .                             C1=TRIM(CLAUSE%TITLE),
     .                             C2='NODE')
         ELSE

           NODSYS=ITABM1(NODSYS,2)

           NINDX=NINDX+1    !   nb of CLAUSE nodes
           NODE_READ_TMP(NINDX) = NODSYS
         ENDIF

      ENDDO ! DO K=1,IDS_MAX





      ! Sort the Readed NODEs and remove eventual duplicates
      ! ----------------------------------------------------

      DO I=1,NINDX
        INDEX(I) = I
        SORT(I) = NODE_READ_TMP(I)
      ENDDO
      CALL MY_ORDERS(0,IWORK,SORT,INDEX,NINDX,1)

      DO I=1,NINDX
         NODE_READ_TMP(I) = SORT(INDEX(I))
      ENDDO

      CALL REMOVE_DUPLICATES(NODE_READ_TMP,NINDX,LIST_SIZE)

      ! Copy in final SET
      ! ------------------
      CLAUSE%NB_NODE = LIST_SIZE
      ALLOCATE( CLAUSE%NODE( LIST_SIZE ) )

      DO I=1,LIST_SIZE
        CLAUSE%NODE(I) = NODE_READ_TMP(I)
      ENDDO
C-------------------------
       DEALLOCATE(NODE_READ_TMP)
       DEALLOCATE(SORT)
       DEALLOCATE(INDEX)
C-------------------------
      RETURN
      END

Chd|====================================================================
Chd|  CREATE_NODE_LIST_G            source/model/sets/create_node_clause.F
Chd|-- called by -----------
Chd|        CREATE_NODE_CLAUSE            source/model/sets/create_node_clause.F
Chd|-- calls ---------------
Chd|        HM_GET_INT_ARRAY_2INDEXES     source/devtools/hm_reader/hm_get_int_array_2indexes.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        SET_USRTOS_NEAREST            source/model/sets/ipartm1.F   
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CREATE_NODE_LIST_G(
     .               CLAUSE,  ITABM1  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Create PART Clause from Generation All parts from Min to Max with Increment (Opt_G)
C--------------------------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     CLAUSE        (SET structure) Clause to be treated
C     ITABM1        MAP Table UID -> LocalID
C     JCLAUSE       parameter with HM_READER (current clause read)
C     IS_AVAILABLE  Bool / Result of HM_interface
C     LSUBMODEL     SUBMODEL Structure.
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SETDEF_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JCLAUSE
      LOGICAL :: IS_AVAILABLE
      INTEGER, INTENT(IN), DIMENSION(NUMNOD,2) :: ITABM1
!
      TYPE (SET_) ::  CLAUSE
      TYPE(SUBMODEL_DATA),INTENT(IN):: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IDS,LIST_SIZE,IDS_MAX,GENE_MAX,K,N,N1
      INTEGER START_GENE,END_GENE,INCR_GENE,NSTART,NSTOP,STACK,STACK_ONE,NB_RESULT
!-
      INTEGER, ALLOCATABLE, DIMENSION(:) :: NODE_READ_TMP,
     .         NODE_READ_ONE,RESULT
C
      INTEGER  SET_USRTOS_NEAREST
      EXTERNAL SET_USRTOS_NEAREST
C=======================================================================
      CALL HM_GET_INT_ARRAY_INDEX('genemax' ,GENE_MAX ,JCLAUSE,IS_AVAILABLE,LSUBMODEL)

      ALLOCATE(NODE_READ_TMP(NUMNOD))
      ALLOCATE(NODE_READ_ONE(NUMNOD))


      IF (GENE_MAX > 1) THEN
          ALLOCATE(RESULT(NUMNOD))
      ENDIF

      STACK=0

      DO K=1,GENE_MAX
          CALL HM_GET_INT_ARRAY_2INDEXES('start'  ,START_GENE,JCLAUSE,K,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_2INDEXES('end'    ,END_GENE  ,JCLAUSE,K,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_2INDEXES('by'     ,INCR_GENE ,JCLAUSE,K,IS_AVAILABLE,LSUBMODEL)

          ! set value by default for increment to 1
          IF (INCR_GENE == 0) INCR_GENE = 1

          NSTART = SET_USRTOS_NEAREST(START_GENE,ITABM1,NUMNOD,1)
          NSTOP  = SET_USRTOS_NEAREST(END_GENE,ITABM1,NUMNOD,2)

          STACK_ONE=0
          DO N=NSTART, NSTOP
             N1 = ITABM1(N,1)
             IF ( MOD( N1-START_GENE , INCR_GENE) == 0)THEN
                STACK_ONE = STACK_ONE+1
                NODE_READ_ONE(STACK_ONE) = ITABM1(N,2)
             ENDIF
          ENDDO

          IF (STACK==0) THEN
             NODE_READ_TMP(1:STACK_ONE) = NODE_READ_ONE(1:STACK_ONE)
             STACK = STACK_ONE
          ELSE
             ! This code will not go if  GENE_MAX == 1 / Result does not need to be allocated
             CALL UNION_2_SORTED_SETS( NODE_READ_TMP, STACK ,
     *                                 NODE_READ_ONE, STACK_ONE ,
     *                                 RESULT,        NB_RESULT )

            NODE_READ_TMP(1:NB_RESULT) = RESULT(1:NB_RESULT)
            STACK = NB_RESULT
          ENDIF
      ENDDO

      CLAUSE%NB_NODE = STACK
      ALLOCATE(CLAUSE%NODE(STACK))
      CLAUSE%NODE(1:STACK) = NODE_READ_TMP(1:STACK)
C-------------------------
      DEALLOCATE (NODE_READ_TMP)
      DEALLOCATE (NODE_READ_ONE)
      IF (ALLOCATED(RESULT)) DEALLOCATE (RESULT)
C-------------------------
      END


