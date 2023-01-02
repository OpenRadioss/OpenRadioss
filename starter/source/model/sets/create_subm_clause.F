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
Chd|  CREATE_SUBM_CLAUSE            source/model/sets/create_subm_clause.F
Chd|-- called by -----------
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|-- calls ---------------
Chd|        CREATE_SUBM_LIST              source/model/sets/create_subm_clause.F
Chd|        CREATE_SUBM_LIST_G            source/model/sets/create_subm_clause.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CREATE_SUBM_CLAUSE(
     .                        CLAUSE   ,ISUBMM  ,JCLAUSE ,OPT_G ,IS_AVAILABLE ,
     .                        LSUBMODEL,IPART   )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Treat the PART Clause, read PARTs from HM_READER & fill clause
C   Calls CREATE_PART_LIST (simple list)
C   Calls CREATE_PART_LIST_G (PART_G : All parts from a MIN to MAX with increment)
C------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     CLAUSE        (SET structure) Clause to be treated
C     ISUBSM        MAP Table UID -> LocalID
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
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "submod_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JCLAUSE,OPT_G
      LOGICAL :: IS_AVAILABLE
      INTEGER, INTENT(IN), DIMENSION(NSUBMOD,2) :: ISUBMM
      INTEGER IPART(LIPART1,NPART)
C-----------------------------------------------
      TYPE (SET_) ::  CLAUSE
      TYPE(SUBMODEL_DATA),INTENT(IN):: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
!
        IF (OPT_G == 1 ) THEN
             CALL CREATE_SUBM_LIST_G(CLAUSE,  ISUBMM  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     .                               IPART )
        ELSE
             CALL CREATE_SUBM_LIST  (CLAUSE,  ISUBMM  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     .                               IPART )
        ENDIF
C-----------------------------------------------
      END
Chd|====================================================================
Chd|  CREATE_SUBM_LIST              source/model/sets/create_subm_clause.F
Chd|-- called by -----------
Chd|        CREATE_SUBM_CLAUSE            source/model/sets/create_subm_clause.F
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
      SUBROUTINE CREATE_SUBM_LIST(
     .               CLAUSE,  ISUBMM  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     .               IPART )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Create PART Clause from LIST
C------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     CLAUSE        (SET structure) Clause to be treated
C     ISUBSM        MAP Table UID -> LocalID
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
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JCLAUSE
      LOGICAL :: IS_AVAILABLE
      INTEGER, INTENT(IN), DIMENSION(NSUBMOD,2) :: ISUBMM
      INTEGER IPART(LIPART1,NPART)
!
      TYPE (SET_) ::  CLAUSE
      TYPE(SUBMODEL_DATA),INTENT(IN):: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IDS,NINDX,LIST_SIZE,IDS_MAX,SUBMM,PARTM,ISUB,IP,SUB_INDEX,
     . LIST_SIZE_S,LIST_SIZE_P,LIST_SIZE_N,NODE
      INTEGER IWORK(70000)
!
      INTEGER, ALLOCATABLE, DIMENSION(:) :: SUBM_READ_TMP,SORTED_SUBM,INDEXS,
     . PART_READ_TMP,SORTED_PARTS,INDEXP,TAGNODSUB,NODE_READ_TMP,SORTED_NODES,INDEXN
C
      INTEGER SET_USRTOS
      EXTERNAL SET_USRTOS
C=======================================================================

      CALL HM_GET_INT_ARRAY_INDEX('idsmax' ,IDS_MAX ,JCLAUSE,IS_AVAILABLE,LSUBMODEL)

      ALLOCATE(SUBM_READ_TMP(IDS_MAX))
      ALLOCATE(SORTED_SUBM(IDS_MAX))

      ALLOCATE(PART_READ_TMP(NPART))
      ALLOCATE(SORTED_PARTS(NPART))

      ALLOCATE(INDEXS(2*IDS_MAX)) ! subsets
      INDEXS = 0

      ALLOCATE(INDEXP(2*NPART)) ! parts of subsets
      INDEXP = 0

      ALLOCATE(TAGNODSUB(NUMNOD))

      ALLOCATE(NODE_READ_TMP(NUMNOD))
!!      ALLOCATE(SORTED_NODES(NUMNOD))
!!      ALLOCATE(INDEXN(2*NUMNOD)) ! nodes of subsets
!!      INDEXN = 0


      NINDX = 0
      LIST_SIZE_S = 0
      LIST_SIZE_P = 0
      LIST_SIZE_N = 0

      ! Read & convert Subsets
      ! ---------------------
      DO I=1,IDS_MAX
         CALL HM_GET_INT_ARRAY_2INDEXES('ids',IDS,JCLAUSE,I,IS_AVAILABLE,LSUBMODEL)

         SUBMM = SET_USRTOS(IDS,ISUBMM,NSUBMOD)
         IF(SUBMM == 0)THEN        
           ! Submodel was not found. Issue a Warning & Skip.
           CALL ANCMSG(MSGID=1902,ANMODE=ANINFO,
     .                             MSGTYPE=MSGWARNING,
     .                             I1 = CLAUSE%SET_ID,
     .                             I2=IDS,
     .                             C1=TRIM(CLAUSE%TITLE),
     .                             C2='SUBMODEL')
         ELSE

           SUBMM=ISUBMM(SUBMM,2)

           NINDX=NINDX+1    !   nb of CLAUSE submodels
           SUBM_READ_TMP(NINDX) = SUBMM
         ENDIF

      ENDDO ! DO K=1,IDS_MAX


      ! Sort the Readed SUBSETs and remove eventual duplicates
      ! ---------------------------------------------------- 

      IWORK(:) = 0
      DO I=1,NINDX
        INDEXS(I) = I
      ENDDO
      CALL MY_ORDERS(0,IWORK,SUBM_READ_TMP,INDEXS,NINDX,1)
                
      DO I=1,NINDX
         SORTED_SUBM(I) = SUBM_READ_TMP(INDEXS(I))
      ENDDO

      CALL REMOVE_DUPLICATES(SORTED_SUBM,NINDX,LIST_SIZE_S)


      ! TAG Parts from Readed SUBSETs
      ! ---------------------------------------------------- 

      NINDX = 0
      DO I=1,LIST_SIZE_S
        ISUB = SORTED_SUBM(I)
        DO IP=1,NPART
          SUB_INDEX = IPART(9,IP)
          IF (ISUB == SUB_INDEX) THEN

             PARTM = IP

             NINDX=NINDX+1    !   nb of Parts of CLAUSE subsets
             PART_READ_TMP(NINDX) = PARTM

          ENDIF
        ENDDO
      ENDDO


      ! Sort of TAG Parts from Readed SUBMODELs and remove eventual duplicates
      ! ---------------------------------------------------- 

      IWORK(:) = 0
      DO I=1,NINDX
        INDEXP(I) = I
      ENDDO
      CALL MY_ORDERS(0,IWORK,PART_READ_TMP,INDEXP,NINDX,1)
                
      DO I=1,NINDX
         SORTED_PARTS(I) = PART_READ_TMP(INDEXP(I))
      ENDDO

      LIST_SIZE_P = 0
      CALL REMOVE_DUPLICATES(SORTED_PARTS,NINDX,LIST_SIZE_P)


      ! Copy in final SET
      ! ------------------
      CLAUSE%NB_PART = LIST_SIZE_P
      ALLOCATE( CLAUSE%PART( LIST_SIZE_P ) )

      DO I=1,LIST_SIZE_P
        CLAUSE%PART(I) = SORTED_PARTS(I)
      ENDDO
!---


      ! Tag & convert Nodes of Submodel
      ! ---------------------
      CALL CPP_NODE_SUB_TAG(TAGNODSUB)


      NINDX = 0
      DO I=1,LIST_SIZE_S
        ISUB = SORTED_SUBM(I)
        DO J=1,NUMNOD
          SUB_INDEX = TAGNODSUB(J)
          IF (ISUB == SUB_INDEX) THEN

             NODE = J

             NINDX=NINDX+1    !   nb of Nodes of CLAUSE subsets
             NODE_READ_TMP(NINDX) = NODE

          ENDIF
        ENDDO
      ENDDO


      ! Sort of TAG Nodes from Readed SUBMODELs and remove eventual duplicates
      ! ---------------------------------------------------- 

      ALLOCATE(SORTED_NODES(NINDX))
      ALLOCATE(INDEXN(2*NINDX))
      INDEXN = 0

      IWORK(:) = 0
      DO I=1,NINDX
        INDEXN(I) = I
      ENDDO
      CALL MY_ORDERS(0,IWORK,NODE_READ_TMP,INDEXN,NINDX,1)
                
      DO I=1,NINDX
         SORTED_NODES(I) = NODE_READ_TMP(INDEXN(I))
      ENDDO

      LIST_SIZE_N = 0
      CALL REMOVE_DUPLICATES(SORTED_NODES,NINDX,LIST_SIZE_N)


      ! Copy in final SET
      ! ------------------
      CLAUSE%NB_NODE = LIST_SIZE_N  
      ALLOCATE( CLAUSE%NODE( LIST_SIZE_N ) )

      DO I=1,LIST_SIZE_N
        CLAUSE%NODE(I) = SORTED_NODES(I)
      ENDDO

C-------------------------
       DEALLOCATE(SUBM_READ_TMP)
       DEALLOCATE(SORTED_SUBM)
       DEALLOCATE(INDEXS)
       DEALLOCATE(PART_READ_TMP)
       DEALLOCATE(SORTED_PARTS)
       DEALLOCATE(INDEXP)
       DEALLOCATE(TAGNODSUB)
       DEALLOCATE(NODE_READ_TMP)
       DEALLOCATE(SORTED_NODES)
       DEALLOCATE(INDEXN)
C-------------------------
      RETURN
      END
Chd|====================================================================
Chd|  CREATE_SUBM_LIST_G            source/model/sets/create_subm_clause.F
Chd|-- called by -----------
Chd|        CREATE_SUBM_CLAUSE            source/model/sets/create_subm_clause.F
Chd|-- calls ---------------
Chd|        HM_GET_INT_ARRAY_2INDEXES     source/devtools/hm_reader/hm_get_int_array_2indexes.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        SET_USRTOS_NEAREST            source/model/sets/ipartm1.F   
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CREATE_SUBM_LIST_G(
     .               CLAUSE,  ISUBMM  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     .               IPART )
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
C     ISUBMM        MAP Table UID -> LocalID
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
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JCLAUSE
      LOGICAL :: IS_AVAILABLE
      INTEGER, INTENT(IN), DIMENSION(NSUBMOD,2) :: ISUBMM
      INTEGER IPART(LIPART1,NPART)
!
      TYPE (SET_) ::  CLAUSE
      TYPE(SUBMODEL_DATA),INTENT(IN):: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IDS,LIST_SIZE,IDS_MAX,PARTM,GENE_MAX,K,S,S1,
     . NINDX,IP,ISUB,SUB_INDEX,NODE
      INTEGER START_GENE,END_GENE,INCR_GENE,SSTART,SSTOP,STACK,STACK_ONE,NB_RESULT
!-
      INTEGER, ALLOCATABLE, DIMENSION(:) :: SUBM_READ_TMP,SUBM_READ_ONE,RESULT,
     .         PART_READ_TMP,SORTED_PARTS,INDEX,TAGNODSUB,NODE_READ_TMP,
     .         SORTED_NODES,INDEXN
C
      INTEGER  SET_USRTOS_NEAREST
      EXTERNAL SET_USRTOS_NEAREST
      INTEGER IWORK(70000)
C=======================================================================
      CALL HM_GET_INT_ARRAY_INDEX('genemax' ,GENE_MAX ,JCLAUSE,IS_AVAILABLE,LSUBMODEL)

      ALLOCATE(SUBM_READ_TMP(NSUBMOD))
      ALLOCATE(SUBM_READ_ONE(NSUBMOD))

      ALLOCATE(PART_READ_TMP(NPART))
      ALLOCATE(SORTED_PARTS(NPART))

      ALLOCATE(INDEX(2*NPART))
      INDEX = 0

      IF (GENE_MAX > 1) THEN
          ALLOCATE(RESULT(NSUBMOD))
      ENDIF

      ALLOCATE(TAGNODSUB(NUMNOD))

      ALLOCATE(NODE_READ_TMP(NUMNOD))
!!      ALLOCATE(SORTED_NODES(NUMNOD))
!!      ALLOCATE(INDEXN(2*NUMNOD)) ! nodes of subsets
!!      INDEXN = 0

      STACK=0

      DO K=1,GENE_MAX
          CALL HM_GET_INT_ARRAY_2INDEXES('start'  ,START_GENE,JCLAUSE,K,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_2INDEXES('end'    ,END_GENE  ,JCLAUSE,K,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_2INDEXES('by'     ,INCR_GENE ,JCLAUSE,K,IS_AVAILABLE,LSUBMODEL)

          SSTART = SET_USRTOS_NEAREST(START_GENE,ISUBMM,NSUBMOD,1)
          SSTOP  = SET_USRTOS_NEAREST(END_GENE,ISUBMM,NSUBMOD,2)
     
          STACK_ONE=0

          DO S=SSTART, SSTOP
             S1 = ISUBMM(S,1)
             IF ( MOD( S1-START_GENE , INCR_GENE) == 0 ) THEN
                STACK_ONE = STACK_ONE+1
                SUBM_READ_ONE(STACK_ONE) = ISUBMM(S,2)
             ENDIF
          ENDDO

          IF (STACK==0) THEN
             SUBM_READ_TMP(1:STACK_ONE) = SUBM_READ_ONE(1:STACK_ONE)
             STACK = STACK_ONE
          ELSE
             ! This code will not go if  GENE_MAX == 1 / Result does not need to be allocated
             CALL UNION_2_SORTED_SETS( SUBM_READ_TMP, STACK ,
     *                                 SUBM_READ_ONE, STACK_ONE ,
     *                                 RESULT,        NB_RESULT )

            SUBM_READ_TMP(1:NB_RESULT) = RESULT(1:NB_RESULT)
            STACK = NB_RESULT
          ENDIF
      ENDDO


      ! TAG Parts from Readed SUBSETs
      ! ---------------------------------------------------- 

      NINDX = 0
      DO I=1,STACK
        ISUB = SUBM_READ_TMP(I)
        DO IP=1,NPART
           SUB_INDEX = IPART(9,IP)
           IF (ISUB == SUB_INDEX) THEN

             PARTM = IP

             NINDX=NINDX+1    !   nb of Parts of CLAUSE submodels
             PART_READ_TMP(NINDX) = PARTM

           ENDIF
        ENDDO ! DO
      ENDDO


      ! Sort of TAG Parts from Readed SUBMODELs and remove eventual duplicates
      ! ---------------------------------------------------- 


      DO I=1,NINDX
        INDEX(I) = I
      ENDDO
      CALL MY_ORDERS(0,IWORK,PART_READ_TMP,INDEX,NINDX,1)
                
      DO I=1,NINDX
         SORTED_PARTS(I) = PART_READ_TMP(INDEX(I))
      ENDDO

      LIST_SIZE = 0
      CALL REMOVE_DUPLICATES(SORTED_PARTS,NINDX,LIST_SIZE)



!
      ! Copy in final SET
      ! ------------------
      CLAUSE%NB_PART = LIST_SIZE
      ALLOCATE(CLAUSE%PART(LIST_SIZE))
      CLAUSE%PART(1:LIST_SIZE) = SORTED_PARTS(1:LIST_SIZE)



      ! Tag & convert Nodes of Submodel
      ! ---------------------
      CALL CPP_NODE_SUB_TAG(TAGNODSUB)


      NINDX = 0
      DO I=1,STACK
        ISUB = SUBM_READ_TMP(I)
        DO J=1,NUMNOD
          SUB_INDEX = TAGNODSUB(J)
          IF (ISUB == SUB_INDEX) THEN

             NODE = J

             NINDX=NINDX+1    !   nb of Nodes of CLAUSE subsets
             NODE_READ_TMP(NINDX) = NODE

          ENDIF
        ENDDO
      ENDDO


      ! Sort of TAG Nodes from Readed SUBMODELs and remove eventual duplicates
      ! ---------------------------------------------------- 

      ALLOCATE(SORTED_NODES(NINDX))
      ALLOCATE(INDEXN(2*NINDX))
      INDEXN = 0

      IWORK(:) = 0
      DO I=1,NINDX
        INDEXN(I) = I
      ENDDO
      CALL MY_ORDERS(0,IWORK,NODE_READ_TMP,INDEXN,NINDX,1)
                
      DO I=1,NINDX
         SORTED_NODES(I) = NODE_READ_TMP(INDEXN(I))
      ENDDO

      LIST_SIZE = 0
      CALL REMOVE_DUPLICATES(SORTED_NODES,NINDX,LIST_SIZE)


      ! Copy in final SET
      ! ------------------
      CLAUSE%NB_NODE = LIST_SIZE  
      ALLOCATE( CLAUSE%NODE( LIST_SIZE ) )

      DO I=1,LIST_SIZE
        CLAUSE%NODE(I) = SORTED_NODES(I)
      ENDDO




!---
      DEALLOCATE (PART_READ_TMP)
      DEALLOCATE (SORTED_PARTS)
      IF (ALLOCATED(RESULT)) DEALLOCATE (RESULT)
      DEALLOCATE (SUBM_READ_TMP)
      DEALLOCATE (SUBM_READ_ONE)
      DEALLOCATE(TAGNODSUB)
      DEALLOCATE(NODE_READ_TMP)
      DEALLOCATE(SORTED_NODES)
      DEALLOCATE(INDEXN)
!---
      END
