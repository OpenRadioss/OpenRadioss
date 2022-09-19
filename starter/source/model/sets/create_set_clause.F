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
Chd|  CREATE_SET_ARRAY              source/model/sets/create_set_clause.F
Chd|-- called by -----------
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|        SORT_SET                      source/model/sets/sort_sets.F 
Chd|-- calls ---------------
Chd|        CREATE_SET_LIST               source/model/sets/create_set_clause.F
Chd|        CREATE_SET_LIST_G             source/model/sets/create_set_clause.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CREATE_SET_ARRAY(SET_ARRAY ,ARRAY_SIZE,
     .                            ISETM  , NSET_GENERAL,
     .                            JCLAUSE ,OPT_G ,IS_AVAILABLE ,
     .                            LSUBMODEL,CLAUSE,FLAG)
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
C     IPARTM1       MAP Table UID -> LocalID
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
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JCLAUSE,OPT_G,ARRAY_SIZE,NSET_GENERAL,FLAG
      LOGICAL :: IS_AVAILABLE
      INTEGER, INTENT(IN), DIMENSION(NSETS,2) :: ISETM
C-----------------------------------------------
      TYPE (SET_) ::  CLAUSE
      INTEGER SET_ARRAY(*)
      TYPE(SUBMODEL_DATA),INTENT(IN):: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
!
        IF (OPT_G == 1 ) THEN
             CALL CREATE_SET_LIST_G(SET_ARRAY, ARRAY_SIZE ,ISETM ,NSET_GENERAL ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL)
        ELSE
             CALL CREATE_SET_LIST  (SET_ARRAY, ARRAY_SIZE ,ISETM ,NSET_GENERAL ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     .                              CLAUSE   ,FLAG)
        ENDIF
C-----------------------------------------------
      END
Chd|====================================================================
Chd|  CREATE_SET_LIST               source/model/sets/create_set_clause.F
Chd|-- called by -----------
Chd|        CREATE_SET_ARRAY              source/model/sets/create_set_clause.F
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
      SUBROUTINE CREATE_SET_LIST(
     .               ARRAY, ARRAY_SIZE, ISETM ,NSET_GENERAL ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     .               CLAUSE,FLAG)
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
C     IPARTM1       MAP Table UID -> LocalID
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
#include      "param_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JCLAUSE, ARRAY_SIZE,NSET_GENERAL,FLAG
      LOGICAL :: IS_AVAILABLE
      INTEGER, INTENT(IN), DIMENSION(NSETS,2) :: ISETM
      INTEGER ARRAY(*)
      TYPE (SET_) ::  CLAUSE
      TYPE(SUBMODEL_DATA),INTENT(IN):: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IDS,NINDX,LIST_SIZE,IDS_MAX,SETM
      INTEGER IWORK(70000)
!
      INTEGER, ALLOCATABLE, DIMENSION(:) :: SET_READ_TMP,INDEX
C
      INTEGER SET_USRTOS
      EXTERNAL SET_USRTOS
C=======================================================================

      ARRAY_SIZE=0

      CALL HM_GET_INT_ARRAY_INDEX('idsmax' ,IDS_MAX ,JCLAUSE,IS_AVAILABLE,LSUBMODEL)

      ALLOCATE(SET_READ_TMP(IDS_MAX))
      SET_READ_TMP(1:IDS_MAX) = 0

      ALLOCATE(INDEX(2*IDS_MAX))
      INDEX = 0

      NINDX = 0
      LIST_SIZE = 0

      ! Read & convert Part list
      ! -------------------------
      DO I=1,IDS_MAX
         CALL HM_GET_INT_ARRAY_2INDEXES('ids',IDS,JCLAUSE,I,IS_AVAILABLE,LSUBMODEL)
!
         SETM = SET_USRTOS(IDS,ISETM,NSET_GENERAL)

         IF(SETM > 0)THEN                 ! 0 if SET was not found
           SETM=ISETM(SETM,2)
           NINDX=NINDX+1                   !   nb of CLAUSE sets
           SET_READ_TMP(NINDX) = SETM
         ELSEIF (FLAG == 1) THEN
           ! SET was not found. Issue a Warning & Skip.
           CALL ANCMSG(MSGID=1902,ANMODE=ANINFO,
     .                             MSGTYPE=MSGWARNING,
     .                             I1 = CLAUSE%SET_ID,
     .                             I2=IDS,
     .                             C1=TRIM(CLAUSE%TITLE),
     .                             C2='SET')
         ENDIF
      ENDDO ! DO K=1,IDS_MAX

      ! Sort the Readed SETs and remove eventual duplicates
      ! ----------------------------------------------------

      DO I=1,NINDX
        INDEX(I) = I
      ENDDO
      CALL MY_ORDERS(0,IWORK,SET_READ_TMP,INDEX,NINDX,1)

      DO I=1,NINDX
        ARRAY(I)=SET_READ_TMP(INDEX(I))
      ENDDO

      CALL REMOVE_DUPLICATES( ARRAY,NINDX,LIST_SIZE)

      ! Copy in final SET
      ! ------------------
      ARRAY_SIZE = LIST_SIZE

!---

C-------------------------
       DEALLOCATE(SET_READ_TMP)
       DEALLOCATE(INDEX)
C-------------------------
      RETURN
      END

Chd|====================================================================
Chd|  CREATE_SET_LIST_G             source/model/sets/create_set_clause.F
Chd|-- called by -----------
Chd|        CREATE_SET_ARRAY              source/model/sets/create_set_clause.F
Chd|-- calls ---------------
Chd|        HM_GET_INT_ARRAY_2INDEXES     source/devtools/hm_reader/hm_get_int_array_2indexes.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        SET_USRTOS_NEAREST            source/model/sets/ipartm1.F   
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CREATE_SET_LIST_G(
     .               ARRAY, ARRAY_SIZE,  ISETM, NSET_GENERAL, JCLAUSE ,IS_AVAILABLE ,LSUBMODEL)
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
C     IPARTM1       MAP Table UID -> LocalID
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
#include      "param_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JCLAUSE,ARRAY_SIZE, NSET_GENERAL
      INTEGER ARRAY(ARRAY_SIZE)
      LOGICAL :: IS_AVAILABLE
      INTEGER, INTENT(IN), DIMENSION(NSETS,2) :: ISETM
!

      TYPE(SUBMODEL_DATA),INTENT(IN):: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IDS,NINDX,LIST_SIZE,IDS_MAX,PARTM,GENE_MAX,K,P,P1
      INTEGER START_GENE,END_GENE,INCR_GENE,PSTART,PSTOP,STACK,STACK_ONE,NB_RESULT
      INTEGER IWORK(70000)
!-
      INTEGER, ALLOCATABLE, DIMENSION(:) :: SET_READ_TMP,SET_READ_ONE,IDEX,RESULT
C
      INTEGER  SET_USRTOS_NEAREST
      EXTERNAL SET_USRTOS_NEAREST
C=======================================================================
      CALL HM_GET_INT_ARRAY_INDEX('genemax' ,GENE_MAX ,JCLAUSE,IS_AVAILABLE,LSUBMODEL)

      ALLOCATE(SET_READ_TMP(NSETS))
      ALLOCATE(SET_READ_ONE(NSETS))
      ALLOCATE(RESULT(NSETS))

      STACK=0

      DO K=1,GENE_MAX
          CALL HM_GET_INT_ARRAY_2INDEXES('start'  ,START_GENE,JCLAUSE,K,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_2INDEXES('end'    ,END_GENE  ,JCLAUSE,K,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_2INDEXES('by'     ,INCR_GENE ,JCLAUSE,K,IS_AVAILABLE,LSUBMODEL)

          ! set value by default for increment to 1
          IF (INCR_GENE == 0) INCR_GENE = 1

          PSTART = SET_USRTOS_NEAREST(START_GENE, ISETM, NSET_GENERAL,1)
          PSTOP  = SET_USRTOS_NEAREST(END_GENE,   ISETM, NSET_GENERAL,2)

          STACK_ONE=0               ! assemble in SET_READ_ONE Stack_one
          DO P=PSTART, PSTOP
             P1 = ISETM(P,1)
             IF ( MOD( P1-START_GENE , INCR_GENE) == 0)THEN
                  STACK_ONE = STACK_ONE+1
                  SET_READ_ONE(STACK_ONE) = ISETM(P,2)
             ENDIF
          ENDDO

          IF(STACK==0) THEN

             SET_READ_TMP(1:STACK_ONE) = SET_READ_ONE(1:STACK_ONE)
             STACK=STACK_ONE

          ELSE

             ! This code will not go if  GENE_MAX == 1 / Result does not need to be allocated
             CALL UNION_2_SORTED_SETS( SET_READ_TMP, STACK ,
     *                                 SET_READ_ONE, STACK_ONE ,
     *                                 RESULT,        NB_RESULT   )
          SET_READ_TMP(1:NB_RESULT)=RESULT(1:NB_RESULT)
          STACK = NB_RESULT

          ENDIF


      ENDDO

      ARRAY_SIZE = STACK
      ARRAY(1:STACK) = SET_READ_TMP(1:STACK)

      DEALLOCATE (SET_READ_ONE)
      DEALLOCATE (SET_READ_TMP)
      DEALLOCATE (RESULT)
      END
Chd|====================================================================
Chd|  CREATE_SET_CLAUSE             source/model/sets/create_set_clause.F
Chd|-- called by -----------
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|-- calls ---------------
Chd|        INSERT_CLAUSE_IN_SET          source/model/sets/insert_clause_in_set.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SET_SCRATCH_MOD               ../common_source/modules/setdef_mod.F
Chd|====================================================================
      SUBROUTINE CREATE_SET_CLAUSE(SET,
     .                             SETL,SETL_SIZE,
     .                             CLAUSE,
     .                             IXS       ,IXS10    ,IXS20     ,IXS16     ,IXQ    ,
     .                             IXC       ,IXTG     ,IXT       ,IXP       ,IXR    ,
     .                             IXX       ,KXX      ,KXSP      ,GEO       ,SH4TREE,
     .                             SH3TREE   ,KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC,
     .                             KNOD2ELTG ,NOD2ELTG ,IPARTC    ,IPARTG    ,IPARTS ,
     .                             IPART     ,OPT_A    ,OPT_O     ,KNOD2ELQ  ,NOD2ELQ,
     .                             X         ,KEYSET   ,OPT_E     ,DELBUF    )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Create SET of SET Clause
C--------------------------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     SET           (SET structure) SET ARRAY
C     SETL          List of SETs from Clause 
C     SETL_SIZE     Number of SETs from Clause 
C     CLAUSE        (SET structure) clause to fill.
C
C     IXS - KXSP, IGEO : not need in this case, but kept for compatibility.
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SETDEF_MOD
      USE SET_SCRATCH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "set_def.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SETL_SIZE
      INTEGER  OPT_A,OPT_O,OPT_E
      INTEGER, DIMENSION(NSETS),    INTENT(IN) :: SETL
      TYPE (SET_),DIMENSION(NSETS), INTENT(IN) :: SET
      TYPE (SET_) :: CLAUSE
      TYPE (SET_SCRATCH) ::  DELBUF
C
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
       INTEGER I,IGS
C-----------------------------------------------
       DO I=1,SETL_SIZE
          IGS = SETL(I)

          ! Each from list SET is add in the clause with SET_ADD
          ! The goal is to merge all entities from SET in the clause.
          ! Call to insert_clause_in_set can be used for that...
          CALL INSERT_CLAUSE_IN_SET(CLAUSE    ,SET(IGS) ,SET_ADD,
     .                              IXS       ,IXS10    ,IXS20   ,IXS16     ,IXQ    ,
     .                              IXC       ,IXTG     ,IXT     ,IXP       ,IXR    ,
     .                              IXX       ,KXX      ,KXSP    ,GEO       ,SH4TREE,
     .                              SH3TREE   ,KNOD2ELS ,NOD2ELS ,KNOD2ELC  ,NOD2ELC,
     .                              KNOD2ELTG ,NOD2ELTG ,IPARTC  ,IPARTG    ,IPARTS ,
     .                              IPART     ,OPT_A    ,OPT_O   ,KNOD2ELQ  ,NOD2ELQ,
     .                              X         ,KEYSET   ,OPT_E   ,DELBUF    )
C
       ENDDO
C-----------------------------------------------
       END


