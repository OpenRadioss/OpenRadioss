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
Chd|  CREATE_SETCOL_ARRAY           source/model/sets/create_setcol_clause.F
Chd|-- called by -----------
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|        SORT_SET                      source/model/sets/sort_sets.F 
Chd|-- calls ---------------
Chd|        CREATE_SETCOL_LIST            source/model/sets/create_setcol_clause.F
Chd|        CREATE_SETCOL_LIST_G          source/model/sets/create_setcol_clause.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CREATE_SETCOL_ARRAY(SET,SETCOL_ARRAY ,ARRAY_SIZE, 
     .                               ISETCOLM  , NSET_COLLECT,
     .                               JCLAUSE ,OPT_G ,IS_AVAILABLE ,
     .                               LSUBMODEL)
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (SET_), DIMENSION(NSETS),INTENT(INOUT) :: SET
      INTEGER JCLAUSE,OPT_G,ARRAY_SIZE,NSET_COLLECT
      LOGICAL :: IS_AVAILABLE
      INTEGER, INTENT(IN), DIMENSION(NSETS,2) :: ISETCOLM
C-----------------------------------------------
      INTEGER SETCOL_ARRAY(*)
      TYPE(SUBMODEL_DATA),INTENT(IN):: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
!
        IF (OPT_G == 1 ) THEN
             CALL CREATE_SETCOL_LIST_G(SET,SETCOL_ARRAY, ARRAY_SIZE ,ISETCOLM ,NSET_COLLECT ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL)
        ELSE
             CALL CREATE_SETCOL_LIST  (SET,SETCOL_ARRAY, ARRAY_SIZE ,ISETCOLM ,NSET_COLLECT ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL)
        ENDIF
C-----------------------------------------------
      END
Chd|====================================================================
Chd|  CREATE_SETCOL_LIST            source/model/sets/create_setcol_clause.F
Chd|-- called by -----------
Chd|        CREATE_SETCOL_ARRAY           source/model/sets/create_setcol_clause.F
Chd|-- calls ---------------
Chd|        HM_GET_INT_ARRAY_2INDEXES     source/devtools/hm_reader/hm_get_int_array_2indexes.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CREATE_SETCOL_LIST(
     .               SET,ARRAY, ARRAY_SIZE, ISETCOLM ,NSET_COLLECT ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL)
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (SET_), DIMENSION(NSETS),INTENT(INOUT) :: SET
      INTEGER JCLAUSE, ARRAY_SIZE,NSET_COLLECT
      LOGICAL :: IS_AVAILABLE
      INTEGER, INTENT(IN), DIMENSION(NSETS,2) :: ISETCOLM
      INTEGER ARRAY(*)

      TYPE(SUBMODEL_DATA),INTENT(IN):: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IDS,NINDX,LIST_SIZE,IDS_MAX,PARTM,SETCOL
      INTEGER IWORK(70000)
!
      INTEGER, ALLOCATABLE, DIMENSION(:) :: PART_READ_TMP,INDEX
C
      INTEGER SET_USRTOS
      EXTERNAL SET_USRTOS
C=======================================================================

      ARRAY_SIZE=0

      CALL HM_GET_INT_ARRAY_INDEX('idsmax' ,IDS_MAX ,JCLAUSE,IS_AVAILABLE,LSUBMODEL)

      ALLOCATE(PART_READ_TMP(IDS_MAX))
      PART_READ_TMP(1:IDS_MAX) = 0

      ALLOCATE(INDEX(2*IDS_MAX))
      INDEX = 0

      NINDX = 0
      LIST_SIZE = 0

      ! Read & convert Part list
      ! -------------------------
      DO I=1,IDS_MAX
         CALL HM_GET_INT_ARRAY_2INDEXES('ids',IDS,JCLAUSE,I,IS_AVAILABLE,LSUBMODEL)
!
         SETCOL = 0
         DO J=1,NSET_COLLECT
           IF(ISETCOLM(J,1)==IDS .AND.  SET(ISETCOLM(J,2))%SET_ACTIV==1) THEN
               SETCOL = ISETCOLM(J,2)
               EXIT
           ENDIF
           IF(ISETCOLM(J,1)>IDS) EXIT
         ENDDO

         
         IF(SETCOL > 0)THEN                 ! 0 if SET was not found
           NINDX=NINDX+1                   !   nb of CLAUSE parts
           PART_READ_TMP(NINDX) = SETCOL
         ENDIF
      ENDDO ! DO K=1,IDS_MAX

      ! Sort the Readed PARTs and remove eventual duplicates
      ! ---------------------------------------------------- 

      DO I=1,NINDX
        INDEX(I) = I
      ENDDO
      CALL MY_ORDERS(0,IWORK,PART_READ_TMP,INDEX,NINDX,1)

      DO I=1,NINDX
        ARRAY(I)=PART_READ_TMP(INDEX(I))
      ENDDO

      CALL REMOVE_DUPLICATES( ARRAY,NINDX,LIST_SIZE)

      ! Copy in final SET
      ! ------------------
      ARRAY_SIZE = LIST_SIZE  
      
!---

C-------------------------
       DEALLOCATE(PART_READ_TMP)
       DEALLOCATE(INDEX)
C-------------------------
      RETURN
      END

Chd|====================================================================
Chd|  CREATE_SETCOL_LIST_G          source/model/sets/create_setcol_clause.F
Chd|-- called by -----------
Chd|        CREATE_SETCOL_ARRAY           source/model/sets/create_setcol_clause.F
Chd|-- calls ---------------
Chd|        HM_GET_INT_ARRAY_2INDEXES     source/devtools/hm_reader/hm_get_int_array_2indexes.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        SET_USRTOS_NEAREST            source/model/sets/ipartm1.F   
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CREATE_SETCOL_LIST_G(
     .               SET,ARRAY, ARRAY_SIZE,  ISETCOLM, NSET_COLLECT, JCLAUSE ,IS_AVAILABLE ,LSUBMODEL)
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (SET_), DIMENSION(NSETS),INTENT(INOUT) :: SET
      INTEGER JCLAUSE,ARRAY_SIZE, NSET_COLLECT
      INTEGER ARRAY(ARRAY_SIZE)
      LOGICAL :: IS_AVAILABLE
      INTEGER, INTENT(IN), DIMENSION(NSET_COLLECT,2) :: ISETCOLM
!
      
      TYPE(SUBMODEL_DATA),INTENT(IN):: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IGS,IDS,NINDX,LIST_SIZE,IDS_MAX,PARTM,GENE_MAX,K,P,P1
      INTEGER START_GENE,END_GENE,INCR_GENE,PSTART,PSTOP,STACK,STACK_ONE,NB_RESULT
      INTEGER IWORK(70000)
!-
      INTEGER, ALLOCATABLE, DIMENSION(:) :: PART_READ_TMP,PART_READ_ONE,IDEX,RESULT,SZ_DICHO
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: SETCOL_DICHO
C
      INTEGER  SET_USRTOS_NEAREST
      EXTERNAL SET_USRTOS_NEAREST
C=======================================================================
      ALLOCATE(SETCOL_DICHO(NSETS,2))
      SZ_DICHO = 0
      DO I=1,NSET_COLLECT
        IDS = ISETCOLM(I,1)
        IGS = ISETCOLM(I,2)
        IF(SET(IGS)%SET_ACTIV == 1)THEN
           SZ_DICHO=SZ_DICHO+1
           SETCOL_DICHO(SZ_DICHO,1) = IDS
           SETCOL_DICHO(SZ_DICHO,2) = IGS
        ENDIF
      ENDDO

      CALL HM_GET_INT_ARRAY_INDEX('genemax' ,GENE_MAX ,JCLAUSE,IS_AVAILABLE,LSUBMODEL)

      ALLOCATE(PART_READ_TMP(NSETS))
      ALLOCATE(PART_READ_ONE(NSETS))
      ALLOCATE(RESULT(NSETS))

      STACK=0

      DO K=1,GENE_MAX
          CALL HM_GET_INT_ARRAY_2INDEXES('start'  ,START_GENE,JCLAUSE,K,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_2INDEXES('end'    ,END_GENE  ,JCLAUSE,K,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_2INDEXES('by'     ,INCR_GENE ,JCLAUSE,K,IS_AVAILABLE,LSUBMODEL)

          ! set value by default for increment to 1
          IF (INCR_GENE == 0) INCR_GENE = 1

          PSTART = SET_USRTOS_NEAREST(START_GENE, SETCOL_DICHO, SZ_DICHO,1)
          PSTOP  = SET_USRTOS_NEAREST(END_GENE,   SETCOL_DICHO, SZ_DICHO,2)
     
          STACK_ONE=0               ! assemble in PART_READ_ONE Stack_one
          DO P=PSTART, PSTOP
             P1 = SETCOL_DICHO(P,1)
             IF ( MOD( P1-START_GENE , INCR_GENE) == 0)THEN
                  STACK_ONE = STACK_ONE+1
                  PART_READ_ONE(STACK_ONE) = P
             ENDIF
          ENDDO

          IF(STACK==0) THEN

             PART_READ_TMP(1:STACK_ONE) = PART_READ_ONE(1:STACK_ONE)
             STACK=STACK_ONE

          ELSE

             ! This code will not go if  GENE_MAX == 1 / Result does not need to be allocated
             CALL UNION_2_SORTED_SETS( PART_READ_TMP, STACK ,
     *                                 PART_READ_ONE, STACK_ONE ,
     *                                 RESULT,        NB_RESULT   )
             PART_READ_TMP(1:NB_RESULT)=RESULT(1:NB_RESULT)
             STACK = NB_RESULT

          ENDIF


      ENDDO

      ARRAY_SIZE = STACK
      ARRAY(1:STACK) = PART_READ_TMP(1:STACK)

      DEALLOCATE (PART_READ_ONE)
      DEALLOCATE (PART_READ_TMP)
      DEALLOCATE (RESULT)
      END
Chd|====================================================================
Chd|  CREATE_SETCOL_CLAUSE          source/model/sets/create_setcol_clause.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        INSERT_CLAUSE_IN_SET          source/model/sets/insert_clause_in_set.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SET_SCRATCH_MOD               ../common_source/modules/setdef_mod.F
Chd|====================================================================
      SUBROUTINE CREATE_SETCOL_CLAUSE(SET,
     *                                SETL,SETL_SIZE,
     *                                CLAUSE,
     *                                IXS       ,IXS10    ,IXS20     ,IXS16     ,IXQ    ,
     *                                IXC       ,IXTG     ,IXT       ,IXP       ,IXR    ,
     *                                IXX       ,KXX      ,KXSP      ,GEO       ,SH4TREE,
     .                                SH3TREE   ,KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC,
     .                                KNOD2ELTG ,NOD2ELTG ,IPARTC    ,IPARTG    ,IPARTS ,
     .                                IPART     ,OPT_A    ,OPT_O     ,KNOD2ELQ  ,NOD2ELQ,
     .                                X         ,KEYSET   ,OPT_E     ,DELBUF    )
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SETL_SIZE
      INTEGER, DIMENSION(NSETS),    INTENT(IN) :: SETL
      TYPE (SET_),DIMENSION(NSETS), INTENT(IN) :: SET
      TYPE (SET_) :: CLAUSE
      TYPE (SET_SCRATCH) ::  DELBUF
      INTEGER  OPT_A,OPT_O,OPT_E
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
          CALL INSERT_CLAUSE_IN_SET(CLAUSE    ,SET(IGS) ,SET_ADD   ,
     *                              IXS       ,IXS10    ,IXS20     ,IXS16     ,IXQ    ,
     *                              IXC       ,IXTG     ,IXT       ,IXP       ,IXR    ,
     *                              IXX       ,KXX      ,KXSP      ,GEO       ,SH4TREE,
     .                              SH3TREE   ,KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC,
     .                              KNOD2ELTG ,NOD2ELTG ,IPARTC    ,IPARTG    ,IPARTS ,
     .                              IPART     ,OPT_A    ,OPT_O     ,KNOD2ELQ  ,NOD2ELQ,
     .                              X         ,KEYSET   ,OPT_E     ,DELBUF    )
C
       ENDDO
C-----------------------------------------------
       END

Chd|====================================================================
Chd|  CREATE_SET_COLLECT            source/model/sets/create_setcol_clause.F
Chd|-- called by -----------
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|-- calls ---------------
Chd|        INSERT_CLAUSE_IN_SET          source/model/sets/insert_clause_in_set.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SET_SCRATCH_MOD               ../common_source/modules/setdef_mod.F
Chd|====================================================================
      SUBROUTINE CREATE_SET_COLLECT(SET       ,SET_ID   ,IGS   ,SETCOL_ARRAY ,ARRAY_SIZE,
     *                              IXS       ,IXS10    ,IXS20     ,IXS16     ,IXQ    ,
     *                              IXC       ,IXTG     ,IXT       ,IXP       ,IXR    ,
     *                              IXX       ,KXX      ,KXSP      ,GEO       ,SH4TREE,
     *                              SH3TREE   ,KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC,
     *                              KNOD2ELTG ,NOD2ELTG ,IPARTC    ,IPARTG    ,IPARTS ,
     *                              IPART     ,OPT_A    ,OPT_O     ,KNOD2ELQ  ,NOD2ELQ,
     *                              X         ,KEYSET   ,OPT_E     ,DELBUF    )
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
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SET_ID,ARRAY_SIZE ,IGS
      INTEGER  OPT_A,OPT_O,OPT_E
      INTEGER, DIMENSION(ARRAY_SIZE,2),    INTENT(IN) :: SETCOL_ARRAY
      TYPE (SET_),DIMENSION(NSETS), INTENT(IN) :: SET
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
       INTEGER I,CUR
C-----------------------------------------------
       DO I=1,ARRAY_SIZE
          
          IF(SETCOL_ARRAY(I,1) == SET_ID .AND. SETCOL_ARRAY(I,2) /= IGS) THEN    ! The SET IGS is the active one...
             CUR =  SETCOL_ARRAY(I,2)                                             ! Others needs to be merged.


             ! Each from list SET is add in the clause with SET_ADD
             ! The goal is to merge all entities from SET in the clause.
             ! Call to insert_clause_in_set can be used for that...
             CALL INSERT_CLAUSE_IN_SET(SET(IGS)  ,SET(CUR) ,SET_ADD   ,
     .                                 IXS       ,IXS10    ,IXS20     ,IXS16     ,IXQ    ,
     .                                 IXC       ,IXTG     ,IXT       ,IXP       ,IXR    ,
     .                                 IXX       ,KXX      ,KXSP      ,GEO       ,SH4TREE,
     .                                 SH3TREE   ,KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC,
     .                                 KNOD2ELTG ,NOD2ELTG ,IPARTC    ,IPARTG    ,IPARTS ,
     .                                 IPART     ,OPT_A    ,OPT_O     ,KNOD2ELQ  ,NOD2ELQ,
     .                                 X         ,KEYSET   ,OPT_E     ,DELBUF    )
          ENDIF
C
       ENDDO
C-----------------------------------------------
       END


