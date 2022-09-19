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
Chd|  CREATE_ELT_CLAUSE             source/model/sets/create_element_clause.F
Chd|-- called by -----------
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|-- calls ---------------
Chd|        CHECK_ELTYP                   source/model/sets/check_eltyp.F
Chd|        CREATE_ELT_BOX                source/model/sets/create_elt_box.F
Chd|        CREATE_ELT_LIST               source/model/sets/create_element_clause.F
Chd|        CREATE_ELT_LIST_G             source/model/sets/create_element_clause.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CREATE_ELT_CLAUSE( CLAUSE,ELTYP,
     *                              MAP,    MAPSIZE,
     *                              JCLAUSE,Opt_G,IS_AVAILABLE ,LSUBMODEL,OPT_B,
     *                              OPT_C  ,IBOX    ,X        ,SKEW   ,SET_TITLE,
     *                              KEYSET ,NIX     ,IX       ,NIX1   ,IPARTE   ,
     *                              IPART  ,KLEVTREE,ELTREE   ,KELTREE)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Treat the Elmeent Clause, read Elements from HM_READER & fill clause
C   Calls CREATE_ELT_LIST (simple list)
C   Calls CREATE_ELT_LIST_G (PART_G : All parts from a MIN to MAX with increment)
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
#include      "scr17_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IPARTE(*),IPART(LIPART1,*),IX(NIX,*),ELTREE(KELTREE,*)
      INTEGER  ELTYP,JCLAUSE,Opt_G,OPT_B,OPT_C,NIX,NIX1,KLEVTREE,
     .         KELTREE
      INTEGER, INTENT(IN)           ::  MAPSIZE
      INTEGER, DIMENSION(MAPSIZE,2) :: MAP
      LOGICAL :: IS_AVAILABLE
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
      INTEGER BOXTYPE
      INTEGER GOT_ELEMENTS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
!
        CALL CHECK_ELTYP(ELTYP,CLAUSE,GOT_ELEMENTS)
        IF(GOT_ELEMENTS == 0) THEN
           RETURN
        ENDIF

        IF (OPT_G == 1 ) THEN

             CALL CREATE_ELT_LIST_G(CLAUSE,ELTYP,  MAP,    MAPSIZE,  JCLAUSE ,IS_AVAILABLE ,LSUBMODEL)

        ELSEIF( OPT_G == 0 .AND. OPT_B == 0 .AND. OPT_C == 0 ) THEN

             CALL CREATE_ELT_LIST  (CLAUSE,ELTYP,  MAP,    MAPSIZE,  JCLAUSE ,IS_AVAILABLE ,LSUBMODEL)

        ELSEIF ( OPT_B == 1 .OR. OPT_C == 1 ) THEN
             IF (OPT_B == 1) BOXTYPE = 1
             IF (OPT_C == 1) BOXTYPE = 2
             CALL CREATE_ELT_BOX(
     *               CLAUSE  ,IPARTE  ,JCLAUSE ,IS_AVAILABLE ,LSUBMODEL,
     *               IBOX    ,X       ,SKEW    ,SET_TITLE    ,KEYSET   ,
     *               BOXTYPE ,MAPSIZE ,NIX     ,IX           ,NIX1     ,
     *               IPART   ,ELTREE  ,KLEVTREE,KELTREE      ,ELTYP    )

        ENDIF
C-----------------------------------------------
      END
Chd|====================================================================
Chd|  CREATE_ELT_LIST_G             source/model/sets/create_element_clause.F
Chd|-- called by -----------
Chd|        CREATE_ELT_CLAUSE             source/model/sets/create_element_clause.F
Chd|-- calls ---------------
Chd|        COPY_LIST_IN_CAUSE            source/model/sets/create_element_clause.F
Chd|        HM_GET_INT_ARRAY_2INDEXES     source/devtools/hm_reader/hm_get_int_array_2indexes.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        SET_USRTOS_NEAREST            source/model/sets/ipartm1.F   
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CREATE_ELT_LIST_G(
     .               CLAUSE,ELTYP , MAP,    MAPSIZE,  JCLAUSE ,IS_AVAILABLE ,LSUBMODEL)
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
#include      "radioss_maptable.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JCLAUSE,ELTYP
      INTEGER, INTENT(IN) :: MAPSIZE
      INTEGER, DIMENSION(MAPSIZE,2), INTENT(IN) :: MAP

      LOGICAL :: IS_AVAILABLE
!
      TYPE (SET_) ::  CLAUSE
      TYPE(SUBMODEL_DATA),INTENT(IN):: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IDS,IDS_MAX,GENE_MAX,K,E,E1
      INTEGER START_GENE,END_GENE,INCR_GENE,ESTART,ESTOP,STACK,STACK_ONE,NB_RESULT
!-
      INTEGER, ALLOCATABLE, DIMENSION(:) :: ELT_READ_TMP,
     .         ELT_READ_ONE,RESULT
C
      INTEGER  SET_USRTOS_NEAREST
      EXTERNAL SET_USRTOS_NEAREST
C=======================================================================
      CALL HM_GET_INT_ARRAY_INDEX('genemax' ,GENE_MAX ,JCLAUSE,IS_AVAILABLE,LSUBMODEL)

      ALLOCATE(ELT_READ_TMP(MAPSIZE))
      ALLOCATE(ELT_READ_ONE(MAPSIZE))

      IF (GENE_MAX > 1) THEN
          ALLOCATE(RESULT(MAPSIZE))
      ENDIF

      STACK=0

      DO K=1,GENE_MAX
          CALL HM_GET_INT_ARRAY_2INDEXES('start'  ,START_GENE,JCLAUSE,K,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_2INDEXES('end'    ,END_GENE  ,JCLAUSE,K,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_2INDEXES('by'     ,INCR_GENE ,JCLAUSE,K,IS_AVAILABLE,LSUBMODEL)

          ! set value by default for increment to 1
          IF (INCR_GENE == 0) INCR_GENE = 1

          ESTART = SET_USRTOS_NEAREST(START_GENE,MAP,MAPSIZE,1)
          ESTOP  = SET_USRTOS_NEAREST(END_GENE,MAP,MAPSIZE,2)

          STACK_ONE = 0
          DO E=ESTART, ESTOP
             E1 = MAP(E,1)
             IF ( MOD( E1-START_GENE , INCR_GENE) == 0)THEN
                STACK_ONE = STACK_ONE+1
                ELT_READ_ONE(STACK_ONE) = MAP(E,2)
             ENDIF
          ENDDO

          IF (STACK==0) THEN
             ELT_READ_TMP(1:STACK_ONE) = ELT_READ_ONE(1:STACK_ONE)
             STACK = STACK_ONE
          ELSE
             ! This code will not go if  GENE_MAX == 1 / Result does not need to be allocated
             CALL UNION_2_SORTED_SETS( ELT_READ_TMP, STACK ,
     *                                 ELT_READ_ONE, STACK_ONE ,
     *                                 RESULT,       NB_RESULT )

            ELT_READ_TMP(1:NB_RESULT) = RESULT(1:NB_RESULT)
            STACK = NB_RESULT
          ENDIF
      ENDDO

      CALL COPY_LIST_IN_CAUSE(CLAUSE,ELTYP,ELT_READ_TMP,STACK)

C-------------------------
      DEALLOCATE (ELT_READ_TMP)
      DEALLOCATE (ELT_READ_ONE)
      IF (ALLOCATED(RESULT)) DEALLOCATE (RESULT)
C-------------------------
      END
Chd|====================================================================
Chd|  CREATE_ELT_LIST               source/model/sets/create_element_clause.F
Chd|-- called by -----------
Chd|        CREATE_ELT_CLAUSE             source/model/sets/create_element_clause.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        COPY_LIST_IN_CAUSE            source/model/sets/create_element_clause.F
Chd|        GET_ELT_NAME                  source/model/sets/create_element_clause.F
Chd|        HM_GET_INT_ARRAY_2INDEXES     source/devtools/hm_reader/hm_get_int_array_2indexes.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        SET_USRTOS                    source/model/sets/ipartm1.F   
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE CREATE_ELT_LIST(
     .               CLAUSE,ELTYP,MAP,MAPSIZE,JCLAUSE,IS_AVAILABLE ,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Create ELEMENT Clause from LIST
C------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     CLAUSE        (SET structure) Clause to be treated
C     ELTYP         Id of Element as in radioss_hashtable.inc
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
#include      "radioss_maptable.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JCLAUSE,ELTYP
      INTEGER, INTENT(IN) :: MAPSIZE
      INTEGER, DIMENSION(MAPSIZE,2), INTENT(IN) :: MAP

      LOGICAL :: IS_AVAILABLE
!
      TYPE (SET_) ::  CLAUSE
      TYPE(SUBMODEL_DATA),INTENT(IN):: LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER (LEN=20) :: ELEMENT_NAME
      INTEGER I,IDS,SYSID,NINDX,LIST_SIZE,IDS_MAX,ELTP,ELT
      INTEGER IWORK(70000)
!
      INTEGER, ALLOCATABLE, DIMENSION(:) :: ELT_READ_TMP,SORTED_LIST,IDX
C
      INTEGER SET_USRTOS
      EXTERNAL SET_USRTOS
C
C=======================================================================

      CALL HM_GET_INT_ARRAY_INDEX('idsmax' ,IDS_MAX ,JCLAUSE,IS_AVAILABLE,LSUBMODEL)

      ALLOCATE(ELT_READ_TMP(IDS_MAX))
      ALLOCATE(SORTED_LIST(IDS_MAX))

      ELT_READ_TMP(1:IDS_MAX) = 0

      ALLOCATE(IDX(2*IDS_MAX))
      IDX = 0

      NINDX = 0
      LIST_SIZE = 0

      ! Read & convert Parts
      ! ---------------------
      DO I=1,IDS_MAX
         CALL HM_GET_INT_ARRAY_2INDEXES('ids',IDS,JCLAUSE,I,IS_AVAILABLE,LSUBMODEL)

         ELTP = SET_USRTOS(IDS,MAP,MAPSIZE)
         IF(ELTP == 0)THEN
                             ! Element was not found. Issue a Warning & Skip.
           CALL GET_ELT_NAME(ELTYP, ELEMENT_NAME)
           CALL ANCMSG(MSGID=1902,ANMODE=ANINFO,
     .                             MSGTYPE=MSGWARNING,
     .                             I1 = CLAUSE%SET_ID,
     .                             I2=IDS,
     .                             C1=TRIM(CLAUSE%TITLE),
     .                             C2=TRIM(ELEMENT_NAME)  )
         ELSE

           ELT=MAP(ELTP,2)

           NINDX=NINDX+1    !   nb of CLAUSE parts
           ELT_READ_TMP(NINDX) = ELT
         ENDIF
!         print*,'Read IDS',IDS,' SYSID: ',ELT,'verif:',IX(NIX,ELT)

      ENDDO ! DO K=1,IDS_MAX

      ! Sort the Readed PARTs and remove eventual duplicates
      ! ----------------------------------------------------
      DO I=1,NINDX
        IDX(I) = I
      ENDDO
      CALL MY_ORDERS(0,IWORK,ELT_READ_TMP,IDX,NINDX,1)

      DO I=1,NINDX
         SORTED_LIST(I) = ELT_READ_TMP(IDX(I))
      ENDDO


      CALL REMOVE_DUPLICATES(SORTED_LIST,NINDX,LIST_SIZE)

      ! Copy in final SET
      ! ------------------
      CALL COPY_LIST_IN_CAUSE(CLAUSE,ELTYP,SORTED_LIST,LIST_SIZE)
!---

C-------------------------
       DEALLOCATE(ELT_READ_TMP)
       DEALLOCATE(IDX)
       DEALLOCATE(SORTED_LIST)
C-------------------------
      RETURN
      END


Chd|====================================================================
Chd|  COPY_LIST_IN_CAUSE            source/model/sets/create_element_clause.F
Chd|-- called by -----------
Chd|        CREATE_ELT_BOX                source/model/sets/create_elt_box.F
Chd|        CREATE_ELT_LIST               source/model/sets/create_element_clause.F
Chd|        CREATE_ELT_LIST_G             source/model/sets/create_element_clause.F
Chd|-- calls ---------------
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|====================================================================
      SUBROUTINE COPY_LIST_IN_CAUSE(CLAUSE,ELTYP,SORTED_LIST,LIST_SIZE)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Copy Element list in clause, allocates the element set,and copies the list in.
C------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     CLAUSE        (SET structure) Clause to be treated
C     ELTYP         Id of Element as in radioss_maptable.inc
C     SORTED_LIST   sorted list of elements
C     list_size     sizeof list
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SETDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "radioss_maptable.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (SET_) ::  CLAUSE
      INTEGER, INTENT(IN) :: ELTYP,LIST_SIZE
      INTEGER, INTENT(IN), DIMENSION(LIST_SIZE) :: SORTED_LIST
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------


      SELECT CASE (ELTYP)

         CASE ( ELT_SOLID )

            ! SOLID ELEM
            !-----------------------
            CLAUSE%NB_SOLID = LIST_SIZE
            ALLOCATE( CLAUSE%SOLID( LIST_SIZE ) )
c
            DO I=1,LIST_SIZE
               CLAUSE%SOLID(I) = SORTED_LIST(I)
            ENDDO

         CASE ( ELT_QUAD )

            ! QUAD ELEM
            !-----------------------
            CLAUSE%NB_QUAD = LIST_SIZE
            ALLOCATE( CLAUSE%QUAD( LIST_SIZE ) )
c
            DO I=1,LIST_SIZE
               CLAUSE%QUAD(I) = SORTED_LIST(I)
            ENDDO

         CASE ( ELT_SH4N )

            ! SHELL ELEM
            !-----------------------
            CLAUSE%NB_SH4N = LIST_SIZE
            ALLOCATE( CLAUSE%SH4N( LIST_SIZE ) )
c
            DO I=1,LIST_SIZE
               CLAUSE%SH4N(I) = SORTED_LIST(I)
            ENDDO

         CASE ( ELT_SH3N )

            ! SH3N ELEM
            !-----------------------
            CLAUSE%NB_SH3N = LIST_SIZE
            ALLOCATE( CLAUSE%SH3N( LIST_SIZE ) )
c
            DO I=1,LIST_SIZE
               CLAUSE%SH3N(I) = SORTED_LIST(I)
            ENDDO

         CASE ( ELT_TRIA )

            ! TRIA ELEM
            !-----------------------
            CLAUSE%NB_TRIA = LIST_SIZE
            ALLOCATE( CLAUSE%TRIA( LIST_SIZE ) )
c
            DO I=1,LIST_SIZE
               CLAUSE%TRIA(I) = SORTED_LIST(I)
            ENDDO

         CASE ( ELT_TRUSS )

            ! TRUSS ELEM
            !-----------------------
            CLAUSE%NB_TRUSS = LIST_SIZE
            ALLOCATE( CLAUSE%TRUSS( LIST_SIZE ) )
c
            DO I=1,LIST_SIZE
               CLAUSE%TRUSS(I) = SORTED_LIST(I)
            ENDDO

         CASE ( ELT_BEAM )

            ! BEAM ELEM
            !-----------------------
            CLAUSE%NB_BEAM = LIST_SIZE
            ALLOCATE( CLAUSE%BEAM( LIST_SIZE ) )
c
            DO I=1,LIST_SIZE
               CLAUSE%BEAM(I) = SORTED_LIST(I)
            ENDDO

         CASE ( ELT_SPRING )

            ! SPRING ELEM
            !-----------------------
            CLAUSE%NB_SPRING = LIST_SIZE
            ALLOCATE( CLAUSE%SPRING( LIST_SIZE ) )
c
            DO I=1,LIST_SIZE
               CLAUSE%SPRING(I) = SORTED_LIST(I)
            ENDDO

      END SELECT
!---
      END

Chd|====================================================================
Chd|  GET_ELT_NAME                  source/model/sets/create_element_clause.F
Chd|-- called by -----------
Chd|        CREATE_ELT_LIST               source/model/sets/create_element_clause.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GET_ELT_NAME(ELTYP, ELEMENT_NAME)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Get Element type NAME from ELTYP
C------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     ELTYP         Id of Element as in radioss_maptable.inc
C     ELEMENT_NAME  Element name
C============================================================================
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "radioss_maptable.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ELTYP
      CHARACTER (LEN=20) :: ELEMENT_NAME
C-----------------------------------------------
      SELECT CASE (ELTYP)

         CASE ( ELT_SOLID )

            ! SOLID ELEM
            !-----------------------
            ELEMENT_NAME='SOLID ELEMENT'

         CASE ( ELT_QUAD )

            ! QUAD ELEM
            !-----------------------
            ELEMENT_NAME='QUAD ELEMENT'

         CASE ( ELT_SH4N )

            ! SHELL ELEM
            !-----------------------
            ELEMENT_NAME='SHELL ELEMENT'

         CASE ( ELT_SH3N )

            ! SH3N ELEM
            !-----------------------
            ELEMENT_NAME='TRIANGLE ELEMENT'

         CASE ( ELT_TRIA )

            ! TRIA ELEM
            !-----------------------
            ELEMENT_NAME='TRIANGLE ELEMENT'

         CASE ( ELT_TRUSS )

            ! SPRING ELEM
            !-----------------------
            ELEMENT_NAME='TRUSS ELEMENT'

         CASE ( ELT_BEAM )

            ! BEAM ELEM
            !-----------------------
            ELEMENT_NAME='BEAM ELEMENT'

         CASE ( ELT_SPRING )

            ! SPRING ELEM
            !-----------------------
            ELEMENT_NAME='SPRING ELEMENT'


      END SELECT
      END

