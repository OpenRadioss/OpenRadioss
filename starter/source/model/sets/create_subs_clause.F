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
Chd|  CREATE_SUBS_CLAUSE            source/model/sets/create_subs_clause.F
Chd|-- called by -----------
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|-- calls ---------------
Chd|        CREATE_SUBS_LIST              source/model/sets/create_subs_clause.F
Chd|        CREATE_SUBS_LIST_G            source/model/sets/create_subs_clause.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CREATE_SUBS_CLAUSE(
     .                        CLAUSE   ,ISUBSM  ,JCLAUSE ,OPT_G ,IS_AVAILABLE ,
     .                        LSUBMODEL,SUBSET  )
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
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JCLAUSE,OPT_G
      LOGICAL :: IS_AVAILABLE
      INTEGER, INTENT(IN), DIMENSION(NSUBS,2) :: ISUBSM
C-----------------------------------------------
      TYPE (SET_) ::  CLAUSE
      TYPE(SUBMODEL_DATA),INTENT(IN):: LSUBMODEL(*)
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
!
        IF (OPT_G == 1 ) THEN
             CALL CREATE_SUBS_LIST_G(CLAUSE,  ISUBSM  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     .                               SUBSET)
        ELSE
             CALL CREATE_SUBS_LIST  (CLAUSE,  ISUBSM  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     .                               SUBSET)
        ENDIF
C-----------------------------------------------
      END
Chd|====================================================================
Chd|  CREATE_SUBS_LIST              source/model/sets/create_subs_clause.F
Chd|-- called by -----------
Chd|        CREATE_SUBS_CLAUSE            source/model/sets/create_subs_clause.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_INT_ARRAY_2INDEXES     source/devtools/hm_reader/hm_get_int_array_2indexes.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        SET_USRTOS                    source/model/sets/ipartm1.F   
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CREATE_SUBS_LIST(
     .               CLAUSE,  ISUBSM  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     .               SUBSET)
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
      USE GROUPDEF_MOD
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
      INTEGER, INTENT(IN), DIMENSION(NSUBS,2) :: ISUBSM
!
      TYPE (SET_) ::  CLAUSE
      TYPE(SUBMODEL_DATA),INTENT(IN):: LSUBMODEL(*)
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IDS,NINDX,LIST_SIZE,IDS_MAX,SUBSM,PARTM,ISET,IP
      INTEGER IWORK(70000)
!
      INTEGER, ALLOCATABLE, DIMENSION(:) :: SUBS_READ_TMP,SORTED_SUBS,INDEXS,
     . PART_READ_TMP,SORTED_PARTS,INDEXP
C
      INTEGER SET_USRTOS
      EXTERNAL SET_USRTOS
C=======================================================================

      CALL HM_GET_INT_ARRAY_INDEX('idsmax' ,IDS_MAX ,JCLAUSE,IS_AVAILABLE,LSUBMODEL)

      ALLOCATE(SUBS_READ_TMP(IDS_MAX))
      ALLOCATE(SORTED_SUBS(IDS_MAX))

      ALLOCATE(PART_READ_TMP(NPART))
      ALLOCATE(SORTED_PARTS(NPART))

      ALLOCATE(INDEXS(2*IDS_MAX)) ! subsets
      INDEXS = 0

      ALLOCATE(INDEXP(2*NPART)) ! parts of subsets
      INDEXP = 0

      NINDX = 0
      LIST_SIZE = 0

      ! Read & convert Subsets
      ! ---------------------
      DO I=1,IDS_MAX
         CALL HM_GET_INT_ARRAY_2INDEXES('ids',IDS,JCLAUSE,I,IS_AVAILABLE,LSUBMODEL)
!
         SUBSM = SET_USRTOS(IDS,ISUBSM,NSUBS)
         IF(SUBSM == 0)THEN        
           ! Subset was not found. Issue a Warning & Skip.
           CALL ANCMSG(MSGID=1902,ANMODE=ANINFO,
     .                             MSGTYPE=MSGWARNING,
     .                             I1 = CLAUSE%SET_ID,
     .                             I2=IDS,
     .                             C1=TRIM(CLAUSE%TITLE),
     .                             C2='SUBSET')
         ELSE

           SUBSM=ISUBSM(SUBSM,2)

           NINDX=NINDX+1    !   nb of CLAUSE subsets
           SUBS_READ_TMP(NINDX) = SUBSM
         ENDIF

      ENDDO ! DO K=1,IDS_MAX


      ! Sort the Readed SUBSETs and remove eventual duplicates
      ! ---------------------------------------------------- 

      IWORK(:) = 0
      DO I=1,NINDX
        INDEXS(I) = I
      ENDDO
      CALL MY_ORDERS(0,IWORK,SUBS_READ_TMP,INDEXS,NINDX,1)
                
      DO I=1,NINDX
         SORTED_SUBS(I) = SUBS_READ_TMP(INDEXS(I))
      ENDDO

      CALL REMOVE_DUPLICATES(SORTED_SUBS,NINDX,LIST_SIZE)


      ! TAG Parts from Readed SUBSETs
      ! ---------------------------------------------------- 

      NINDX = 0
      DO I=1,LIST_SIZE
        ISET = SORTED_SUBS(I)
        DO IP=1,SUBSET(ISET)%NTPART

           PARTM = SUBSET(ISET)%TPART(IP)

           NINDX=NINDX+1    !   nb of Parts of CLAUSE subsets
           PART_READ_TMP(NINDX) = PARTM
        ENDDO ! DO
      ENDDO


      ! Sort of TAG Parts from Readed SUBSETs and remove eventual duplicates
      ! ---------------------------------------------------- 

      IWORK(:) = 0
      DO I=1,NINDX
        INDEXP(I) = I
      ENDDO
      CALL MY_ORDERS(0,IWORK,PART_READ_TMP,INDEXP,NINDX,1)
                
      DO I=1,NINDX
         SORTED_PARTS(I) = PART_READ_TMP(INDEXP(I))
      ENDDO

      LIST_SIZE = 0
      CALL REMOVE_DUPLICATES(SORTED_PARTS,NINDX,LIST_SIZE)


      ! Copy in final SET
      ! ------------------
      CLAUSE%NB_PART = LIST_SIZE  
      ALLOCATE( CLAUSE%PART( LIST_SIZE ) )

      DO I=1,LIST_SIZE
        CLAUSE%PART(I) = SORTED_PARTS(I)
      ENDDO
!---

C-------------------------
       DEALLOCATE(SUBS_READ_TMP)
       DEALLOCATE(SORTED_SUBS)
       DEALLOCATE(INDEXS)
       DEALLOCATE(PART_READ_TMP)
       DEALLOCATE(SORTED_PARTS)
       DEALLOCATE(INDEXP)
C-------------------------
      RETURN
      END
Chd|====================================================================
Chd|  CREATE_SUBS_LIST_G            source/model/sets/create_subs_clause.F
Chd|-- called by -----------
Chd|        CREATE_SUBS_CLAUSE            source/model/sets/create_subs_clause.F
Chd|-- calls ---------------
Chd|        HM_GET_INT_ARRAY_2INDEXES     source/devtools/hm_reader/hm_get_int_array_2indexes.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        SET_USRTOS_NEAREST            source/model/sets/ipartm1.F   
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CREATE_SUBS_LIST_G(
     .               CLAUSE,  ISUBSM  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     .               SUBSET)
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
      USE GROUPDEF_MOD
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
      INTEGER, INTENT(IN), DIMENSION(NSUBS,2) :: ISUBSM
!
      TYPE (SET_) ::  CLAUSE
      TYPE(SUBMODEL_DATA),INTENT(IN):: LSUBMODEL(*)
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IDS,LIST_SIZE,IDS_MAX,PARTM,GENE_MAX,K,S,S1,
     . NINDX,IP,ISET
      INTEGER START_GENE,END_GENE,INCR_GENE,SSTART,SSTOP,STACK,STACK_ONE,NB_RESULT
!-
      INTEGER, ALLOCATABLE, DIMENSION(:) :: SUBS_READ_TMP,SUBS_READ_ONE,RESULT,
     .         PART_READ_TMP,SORTED_PARTS,INDEX
C
      INTEGER  SET_USRTOS_NEAREST
      EXTERNAL SET_USRTOS_NEAREST
      INTEGER IWORK(70000)
C=======================================================================
      CALL HM_GET_INT_ARRAY_INDEX('genemax' ,GENE_MAX ,JCLAUSE,IS_AVAILABLE,LSUBMODEL)

      ALLOCATE(SUBS_READ_TMP(NSUBS))
      ALLOCATE(SUBS_READ_ONE(NSUBS))

      ALLOCATE(PART_READ_TMP(NPART))
      ALLOCATE(SORTED_PARTS(NPART))

      ALLOCATE(INDEX(2*NPART))
      INDEX = 0

      IF (GENE_MAX > 1) THEN
          ALLOCATE(RESULT(NSUBS))
      ENDIF

      STACK=0

      DO K=1,GENE_MAX
          CALL HM_GET_INT_ARRAY_2INDEXES('start'  ,START_GENE,JCLAUSE,K,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_2INDEXES('end'    ,END_GENE  ,JCLAUSE,K,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_2INDEXES('by'     ,INCR_GENE ,JCLAUSE,K,IS_AVAILABLE,LSUBMODEL)

          ! set value by default for increment to 1
          IF (INCR_GENE == 0) INCR_GENE = 1

          SSTART = SET_USRTOS_NEAREST(START_GENE,ISUBSM,NSUBS,1)
          SSTOP  = SET_USRTOS_NEAREST(END_GENE,ISUBSM,NSUBS,2)
     
          STACK_ONE=0

          DO S=SSTART, SSTOP
             S1 = ISUBSM(S,1)
             IF ( MOD( S1-START_GENE , INCR_GENE) == 0 ) THEN
                STACK_ONE = STACK_ONE+1
                SUBS_READ_ONE(STACK_ONE) = ISUBSM(S,2)
             ENDIF
          ENDDO

          IF (STACK==0) THEN
             SUBS_READ_TMP(1:STACK_ONE) = SUBS_READ_ONE(1:STACK_ONE)
             STACK = STACK_ONE
          ELSE
             ! This code will not go if  GENE_MAX == 1 / Result does not need to be allocated
             CALL UNION_2_SORTED_SETS( SUBS_READ_TMP, STACK ,
     *                                 SUBS_READ_ONE, STACK_ONE ,
     *                                 RESULT,        NB_RESULT )

            SUBS_READ_TMP(1:NB_RESULT) = RESULT(1:NB_RESULT)
            STACK = NB_RESULT
          ENDIF
      ENDDO


      ! TAG Parts from Readed SUBSETs
      ! ---------------------------------------------------- 

      NINDX = 0
      DO I=1,STACK
        ISET = SUBS_READ_TMP(I)
        DO IP=1,SUBSET(ISET)%NTPART

           PARTM = SUBSET(ISET)%TPART(IP)

           NINDX=NINDX+1    !   nb of Parts of CLAUSE subsets
           PART_READ_TMP(NINDX) = PARTM
        ENDDO ! DO
      ENDDO


      ! Sort of TAG Parts from Readed SUBSETs and remove eventual duplicates
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
!---
      DEALLOCATE (PART_READ_TMP)
      DEALLOCATE (SORTED_PARTS)
      IF (ALLOCATED(RESULT)) DEALLOCATE (RESULT)
      DEALLOCATE (SUBS_READ_TMP)
      DEALLOCATE (SUBS_READ_ONE)
!---
      END
