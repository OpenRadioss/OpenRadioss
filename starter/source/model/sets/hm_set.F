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
Chd|  HM_SET                        source/model/sets/hm_set.F    
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CLAUSE_INIT                   source/model/sets/clause_init.F
Chd|        CREATE_BOX_CLAUSE             source/model/sets/create_box_clause.F
Chd|        CREATE_ELEMENT_FROM_PART      source/model/sets/create_element_from_part.F
Chd|        CREATE_ELEM_ALL_CLAUSE        source/model/sets/create_elem_all_clause.F
Chd|        CREATE_ELT_CLAUSE             source/model/sets/create_element_clause.F
Chd|        CREATE_LINE_FROM_ELEMENT      source/model/sets/create_line_from_element.F
Chd|        CREATE_LINE_FROM_SURFACE      source/model/sets/create_line_from_surface.F
Chd|        CREATE_NODE_ALL_CLAUSE        source/model/sets/create_node_all_clause.F
Chd|        CREATE_NODE_CLAUSE            source/model/sets/create_node_clause.F
Chd|        CREATE_NODE_FROM_ELEMENT      source/model/sets/create_node_from_element.F
Chd|        CREATE_NODE_FROM_RBODY        source/model/sets/create_node_from_rbody.F
Chd|        CREATE_NODE_FROM_SEG          source/model/sets/create_node_from_seg.F
Chd|        CREATE_PART_ALL_CLAUSE        source/model/sets/create_part_all_clause.F
Chd|        CREATE_PART_CLAUSE            source/model/sets/create_part_clause.F
Chd|        CREATE_RBODY_CLAUSE           source/model/sets/create_rbody_clause.F
Chd|        CREATE_SEG_CLAUSE             source/model/sets/create_seg_clause.F
Chd|        CREATE_SETCOL_ARRAY           source/model/sets/create_setcol_clause.F
Chd|        CREATE_SET_ARRAY              source/model/sets/create_set_clause.F
Chd|        CREATE_SET_CLAUSE             source/model/sets/create_set_clause.F
Chd|        CREATE_SET_COLLECT            source/model/sets/create_setcol_clause.F
Chd|        CREATE_SUBM_CLAUSE            source/model/sets/create_subm_clause.F
Chd|        CREATE_SUBS_CLAUSE            source/model/sets/create_subs_clause.F
Chd|        CREATE_SURFACE_FROM_ELEMENT   source/model/sets/create_surface_from_element.F
Chd|        FILL_IGR                      source/model/sets/fill_igr.F  
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_GET_STRING_INDEX           source/devtools/hm_reader/hm_get_string_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START_LIST          source/devtools/hm_reader/hm_option_start.F
Chd|        INSERT_CLAUSE_IN_SET          source/model/sets/insert_clause_in_set.F
Chd|        SET_INIT                      source/model/sets/set_init.F  
Chd|        SET_OPERATOR                  source/model/sets/set_operator.F
Chd|        SORT_SET                      source/model/sets/sort_sets.F 
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        INVERTED_GROUP_MOD            share/modules1/inverted_group_mod.F
Chd|        MAPPING_OPTION_MOD            share/modules1/dichotomy_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SET_SCRATCH_MOD               ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_SET(SET      ,LSUBMODEL ,INV_GROUP ,MAP_TABLES,IPART     ,
     .                  IGRSURF  ,IGRNOD    ,IGRSLIN   ,IGRPART  ,IGRBRIC   , 
     .                  IGRQUAD  ,IGRSH4N   ,IGRSH3N   ,IGRTRUSS ,IGRBEAM   ,
     .                  IGRSPRING,IXS       ,IXS10     ,IXC      ,IXTG      ,
     .                  KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC  ,KNOD2ELTG ,
     .                  NOD2ELTG ,IPARTC    ,IPARTG    ,IPARTS   ,SH4TREE   ,
     .                  SH3TREE  ,IXQ       ,KNOD2ELQ  ,NOD2ELQ  ,X         ,
     .                  IXT      ,IXP       ,IXR       ,IXX      ,KXX       ,
     .                  KXSP     ,IXS20     ,IXS16     ,GEO      ,ITABM1    ,
     .                  IBOX     ,SKEW      ,IPARTQ    ,IPARTT   ,IPARTP    ,
     .                  IPARTR   ,SUBSET    ,RBY_MSN   )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Main Routine for SET read & treatments
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     SET           Set Structure
C     LSUBMODEL     Submodel Structure
C     IPARTM1       PART Map table UID -> Internal ID
C     IGRxxx        Radioss Structures Groups of Nodes, Parts, Elements, Surfaces and lines
C===========================================================================================
C-----------------------------------------------
C   D e f i n i t i o n s 
C-----------------------------------------------
#include      "set_def.inc"
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE HM_OPTION_READ_MOD
      USE SETDEF_MOD
      USE SUBMODEL_MOD
      USE INVERTED_GROUP_MOD
      USE MAPPING_OPTION_MOD
      USE GROUPDEF_MOD
      USE OPTIONDEF_MOD
      USE MESSAGE_MOD
      USE SET_SCRATCH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "radioss_maptable.inc"
#include      "nchar_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (SET_), DIMENSION(NSETS),INTENT(INOUT) :: SET
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
      TYPE(INVERTGROUP_STRUCT_),INTENT(IN) :: INV_GROUP
      TYPE(MAPPING_STRUCT_) :: MAP_TABLES
      TYPE (BOX_)  , DIMENSION(NBBOX)  :: IBOX

      TYPE (GROUP_)  , INTENT(INOUT):: IGRNOD(*)
      TYPE (SURF_)   , INTENT(INOUT):: IGRSURF(*)
      TYPE (SURF_)   , INTENT(INOUT):: IGRSLIN(*)
      TYPE (GROUP_)  , INTENT(INOUT):: IGRPART(*)
      TYPE (GROUP_)  , INTENT(INOUT):: IGRBRIC(*)
      TYPE (GROUP_)  , INTENT(INOUT):: IGRQUAD(*)
      TYPE (GROUP_)  , INTENT(INOUT):: IGRSH4N(*)
      TYPE (GROUP_)  , INTENT(INOUT):: IGRSH3N(*)
      TYPE (GROUP_)  , INTENT(INOUT):: IGRTRUSS(*)
      TYPE (GROUP_)  , INTENT(INOUT):: IGRBEAM(*)
      TYPE (GROUP_)  , INTENT(INOUT):: IGRSPRING(*)
      TYPE (SUBSET_) , INTENT(INOUT) :: SUBSET(*)

      INTEGER, DIMENSION(2*NUMNOD), INTENT(IN) :: ITABM1
      INTEGER, DIMENSION(2*NRBODY), INTENT(IN) :: RBY_MSN

      INTEGER IPART(LIPART1,NPART)
      INTEGER, INTENT(IN) :: IXS(NIXS,*),IXS10(6,*),IXS16(8,*),IXS20(12,*),
     .  IXC(NIXC,*),IXTG(NIXTG,*),KNOD2ELS(*),KNOD2ELC(*),KNOD2ELTG(*),
     .  NOD2ELS(*),NOD2ELC(*),NOD2ELTG(*),IPARTC(*),IPARTG(*),IPARTS(*),
     .  SH4TREE(*),SH3TREE(*),KNOD2ELQ(*),NOD2ELQ(*),IXQ(NIXQ,*),
     .  IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),IXX(*),KXX(*),KXSP(NISP,*),
     .  IPARTQ(*),IPARTT(*),IPARTP(*),IPARTR(*)
      my_real
     .  X(3,*),GEO(NPROPG,*),SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IGS,I,J,ELTYP_SEG,ELTYP_ALL
      INTEGER SET_ID,ISET_TYPE,CLAUSES_MAX,ITMP,ICODE,SETL_SIZE
      INTEGER OPT_D,OPT_O,OPT_G,OPT_B,OPT_A,OPT_E,OPT_I,OPT_C
      INTEGER , DIMENSION(:), ALLOCATABLE :: SET_LIST,SETL
      INTEGER IDEBUG
      LOGICAL IS_AVAILABLE
      TYPE (SET_) ::  CLAUSE
      INTEGER CLAUSE_OPERATOR
      INTEGER ADMBID
      INTEGER DUMMY_ARRAY(10),DSZ
      DATA ADMBID/0/
      TYPE (SET_SCRATCH) ::  DELBUF
C-----------------CREATE_SET_ARRAY------------------------------
C     Characters
      CHARACTER MESS*40
      CHARACTER KEYSET*ncharfield,SET_TYPE*ncharfield,KEY*ncharkey,KEY_TYPE*ncharfield
      CHARACTER TITLE*nchartitle,SET_TITLE*nchartitle,KEYPART*ncharkey,TITLE2*nchartitle
C-----------------------------------------------
C Initialization / SET Dependency build
C-----------------------------------------------

      IDEBUG =  0
      IF (IDEBUG == 1) THEN
        print*,'------------- SET NG -------------'
      ENDIF


C-------------------------------------------
C Reorder Sets to take care of dependencies
C-------------------------------------------
      ALLOCATE(SET_LIST(NSETS))
      CALL SORT_SET(LSUBMODEL ,MAP_TABLES, SET_LIST,SET,CLAUSE)

C-----------------------------------------------
C SET MAIN LOOP
C-----------------------------------------------
      CALL HM_OPTION_START_LIST('/SET')

      ! ------------------------
      !     Initial CLAUSE init
      ! ------------------------


      DO I = 1,NSETS
        IGS = SET_LIST(I)

        SET_TYPE = ' '
        KEYSET = ' '
        KEY    = ' '

        CALL HM_OPTION_READ_KEY (LSUBMODEL,
     .                           OPTION_ID   = SET_ID,
     .                           OPTION_TITR = SET_TITLE,
     .                           KEYWORD2    = KEY,
     .                           OPT_POS     = IGS )
        CALL HM_GET_STRING('set_Type' , SET_TYPE ,ncharfield, IS_AVAILABLE)
        CALL HM_GET_INTV  ('iset_Type', ISET_TYPE,IS_AVAILABLE,LSUBMODEL)

        ! --------------------
        !   Current SET_INIT  
        ! --------------------
        CALL SET_INIT(SET  ,IGS  ,SET_ID, SET_TITLE, ISET_TYPE)


        CALL HM_GET_INTV('clausesmax',CLAUSES_MAX,IS_AVAILABLE,LSUBMODEL)

        IF (IDEBUG == 1)THEN
             print*,'--------------------------------------------'
             print*,'IGS       =',IGS
             print*,'MY_SET_ID =',SET_ID
             print*,'SET_TITLE =',TRIM(SET_TITLE)
             print*,'      KEY =',KEY(1:LEN_TRIM(KEY))
             print*,'SET_TYPE  =',SET_TYPE(1:LEN_TRIM(SET_TYPE))
             print*,'CLAUSE_MAX=',CLAUSES_MAX
             print*,'SET_ACTIVE=',SET(IGS)%SET_ACTIV
             print*,' '
        ENDIF

        DO J=1,CLAUSES_MAX  ! max KEY's of the current /SET

           CALL CLAUSE_INIT(CLAUSE)

           CALL HM_GET_STRING_INDEX('KEY_type', KEYSET, J, ncharline, IS_AVAILABLE)

           CALL HM_GET_INT_ARRAY_INDEX('opt_D',OPT_D,J,IS_AVAILABLE,LSUBMODEL)
           CALL HM_GET_INT_ARRAY_INDEX('opt_O',OPT_O,J,IS_AVAILABLE,LSUBMODEL)
           CALL HM_GET_INT_ARRAY_INDEX('opt_G',OPT_G,J,IS_AVAILABLE,LSUBMODEL)
           CALL HM_GET_INT_ARRAY_INDEX('opt_B',OPT_B,J,IS_AVAILABLE,LSUBMODEL)
           CALL HM_GET_INT_ARRAY_INDEX('opt_A',OPT_A,J,IS_AVAILABLE,LSUBMODEL) 
           CALL HM_GET_INT_ARRAY_INDEX('opt_E',OPT_E,J,IS_AVAILABLE,LSUBMODEL)
           CALL HM_GET_INT_ARRAY_INDEX('opt_I',OPT_I,J,IS_AVAILABLE,LSUBMODEL)
           CALL HM_GET_INT_ARRAY_INDEX('opt_C',OPT_C,J,IS_AVAILABLE,LSUBMODEL)

           !-----------------------
           ! issue 'KEY'  ---> read one more character than the KEY
           ! ===> workaround

           ITMP = LEN(TRIM(KEYSET))
           ICODE = IACHAR(KEYSET(ITMP:ITMP))
           IF (ICODE == 0) THEN
             KEYSET(ITMP:ITMP)=' '
           ENDIF

           !---------------------
           ! Clause Operator
           !---------------------
           CALL SET_OPERATOR( OPT_D,OPT_O,OPT_G,              !   CLAUSE_OPERATOR = SET_ADD       (1)
     *                       OPT_B,opt_A,opt_E,               !                     SET_DELETE    (2)
     *                       OPT_I,OPT_C,                     !                     SET_INTERSECT (3)
     *                       CLAUSE_OPERATOR )

           !  -----------------------------------------------------
           IF (IDEBUG == 1)THEN
                WRITE(6,'(A,I6,A)' ) '      ',J,'    --------'
                WRITE(6,'(A,A)')  '                ',TRIM(KEYSET)
                WRITE(6,'(A,I2)') '                OP :',CLAUSE_OPERATOR
           ENDIF
           !  -----------------------------------------------------
           ! To issue error & Warnings, initialize Clause ID & Title with SET_ID + TITLE
           CLAUSE%SET_ID = SET(IGS)%SET_ID
           CLAUSE%TITLE  = TRIM(SET(IGS)%TITLE)
           
           ! Clause Treatment 
           !------------------------

           SELECT CASE (TRIM(KEYSET))

              CASE ( 'PART' )

                  ! PART LIST FROM CLAUSE
                  !-----------------------
                  CALL CREATE_PART_CLAUSE(
     .                                    CLAUSE   ,MAP_TABLES%IPARTM ,J     ,OPT_G ,IS_AVAILABLE ,
     .                                    LSUBMODEL)

                  ! Element from PART
                  !-------------------
                  CALL CREATE_ELEMENT_FROM_PART(CLAUSE  ,INV_GROUP) 

                  ! Surface from ELEMENT
                  !-------------------
                  CALL CREATE_SURFACE_FROM_ELEMENT(
     .                       IXS       ,IXS10    ,SH4TREE   ,SH3TREE   ,IXC       ,
     .                       IXTG      ,KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC   ,
     .                       KNOD2ELTG ,NOD2ELTG ,IPARTC    ,IPARTG    ,IPARTS    ,
     .                       IPART     ,CLAUSE   ,OPT_A     ,OPT_O     ,IXQ       ,
     .                       KNOD2ELQ  ,NOD2ELQ  ,X         ,KEYSET    ,DELBUF    ,
     .                       .FALSE.   )


                  ! Line from 1D_ELEMENT
                  !-------------------
                  CALL CREATE_LINE_FROM_ELEMENT(IXT    ,IXP  ,IXR  ,CLAUSE,DELBUF    ,
     .                                          .FALSE.)

                  ! Line from SURFACE
                  CALL CREATE_LINE_FROM_SURFACE(CLAUSE,KEYSET,OPT_A,OPT_E,DELBUF    ,
     .                                          .FALSE.)

                  ! Node from ELEMENT
                  CALL CREATE_NODE_FROM_ELEMENT(
     .                            IXS   ,IXS10  ,IXS20  ,IXS16  ,IXQ    ,
     .                            IXC   ,IXTG   ,IXT    ,IXP    ,IXR    ,
     .                            IXX   ,KXX    ,KXSP   ,CLAUSE ,GEO,
     .                            DUMMY_ARRAY   ,DSZ    ,.FALSE. )


              CASE ( 'NODE' )

                  ! NODE LIST FROM CLAUSE
                  !-----------------------
                  CALL CREATE_NODE_CLAUSE(
     .                                    CLAUSE   ,ITABM1  ,J     ,OPT_G ,IS_AVAILABLE ,
     .                                    LSUBMODEL,OPT_B   ,IBOX  ,X     ,SKEW         ,
     .                                    SET_TITLE,KEYSET  )


              CASE ( 'SOLID' )
                  
                  ! SOLID LIST FROM CLAUSE
                  !-----------------------
                  CALL CREATE_ELT_CLAUSE(
     .                                    CLAUSE, ELT_SOLID,
     .                                    MAP_TABLES%ISOLM, NUMELS, 
     .                                    J      ,OPT_G   ,IS_AVAILABLE,LSUBMODEL ,OPT_B    ,
     .                                    OPT_C  ,IBOX    ,X           ,SKEW      ,SET_TITLE,
     .                                    KEYSET ,NIXS    ,IXS         ,8         ,IPARTS   ,
     .                                    IPART  ,ADMBID  ,ADMBID      ,ADMBID    )

                  ! Surface from ELEMENT
                  !-------------------
                  CALL CREATE_SURFACE_FROM_ELEMENT(
     .                       IXS       ,IXS10    ,SH4TREE   ,SH3TREE   ,IXC       ,
     .                       IXTG      ,KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC   ,
     .                       KNOD2ELTG ,NOD2ELTG ,IPARTC    ,IPARTG    ,IPARTS    ,
     .                       IPART     ,CLAUSE   ,OPT_A     ,OPT_O     ,IXQ       ,
     .                       KNOD2ELQ  ,NOD2ELQ  ,X         ,KEYSET    ,DELBUF    ,
     .                       .FALSE.   )

                  ! Line from SURFACE
                  CALL CREATE_LINE_FROM_SURFACE(CLAUSE,KEYSET,OPT_A,OPT_E,DELBUF    ,
     .                                          .FALSE.)

                  ! Node from ELEMENT
                  CALL CREATE_NODE_FROM_ELEMENT(
     .                            IXS   ,IXS10  ,IXS20  ,IXS16  ,IXQ    ,
     .                            IXC   ,IXTG   ,IXT    ,IXP    ,IXR    ,
     .                            IXX   ,KXX    ,KXSP   ,CLAUSE ,GEO    ,
     .                            DUMMY_ARRAY   ,DSZ    ,.FALSE. )



              CASE ( 'QUAD' )
                  
                  ! QUAD LIST FROM CLAUSE
                  !-----------------------
                  CALL CREATE_ELT_CLAUSE(
     .                                    CLAUSE, ELT_QUAD,
     .                                    MAP_TABLES%IQUADM, NUMELQ, 
     .                                    J      ,OPT_G   ,IS_AVAILABLE,LSUBMODEL ,OPT_B    ,
     .                                    OPT_C  ,IBOX    ,X           ,SKEW      ,SET_TITLE,
     .                                    KEYSET ,NIXQ    ,IXQ         ,4         ,IPARTQ   ,
     .                                    IPART  ,ADMBID  ,ADMBID      ,ADMBID    )

                  ! Surface from ELEMENT
                  !-------------------
                  CALL CREATE_SURFACE_FROM_ELEMENT(
     .                       IXS       ,IXS10    ,SH4TREE   ,SH3TREE   ,IXC       ,
     .                       IXTG      ,KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC   ,
     .                       KNOD2ELTG ,NOD2ELTG ,IPARTC    ,IPARTG    ,IPARTS    ,
     .                       IPART     ,CLAUSE   ,OPT_A     ,OPT_O     ,IXQ       ,
     .                       KNOD2ELQ  ,NOD2ELQ  ,X         ,KEYSET    ,DELBUF    ,
     .                       .FALSE.   )

                  ! Line from SURFACE
                  CALL CREATE_LINE_FROM_SURFACE(CLAUSE,KEYSET,OPT_A,OPT_E,DELBUF    ,
     .                                          .FALSE.)

                  ! Node from ELEMENT
                  CALL CREATE_NODE_FROM_ELEMENT(
     .                            IXS   ,IXS10  ,IXS20  ,IXS16  ,IXQ    ,
     .                            IXC   ,IXTG   ,IXT    ,IXP    ,IXR    ,
     .                            IXX   ,KXX    ,KXSP   ,CLAUSE ,GEO    ,
     .                            DUMMY_ARRAY   ,DSZ    ,.FALSE. )



              CASE ( 'SHELL' )
                  
                  ! SHELL LIST FROM CLAUSE
                  !-----------------------
                  CALL CREATE_ELT_CLAUSE(
     .                                    CLAUSE, ELT_SH4N,
     .                                    MAP_TABLES%ISH4NM, NUMELC, 
     .                                    J      ,OPT_G   ,IS_AVAILABLE,LSUBMODEL  ,OPT_B    ,
     .                                    OPT_C  ,IBOX    ,X           ,SKEW       ,SET_TITLE,
     .                                    KEYSET ,NIXC    ,IXC         ,4          ,IPARTC   ,
     .                                    IPART  ,3       ,SH4TREE     ,KSH4TREE   )


                  ! Surface from ELEMENT
                  !-------------------
                  CALL CREATE_SURFACE_FROM_ELEMENT(
     .                       IXS       ,IXS10    ,SH4TREE   ,SH3TREE   ,IXC       ,
     .                       IXTG      ,KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC   ,
     .                       KNOD2ELTG ,NOD2ELTG ,IPARTC    ,IPARTG    ,IPARTS    ,
     .                       IPART     ,CLAUSE   ,OPT_A     ,OPT_O     ,IXQ       ,
     .                       KNOD2ELQ  ,NOD2ELQ  ,X         ,KEYSET    ,DELBUF    ,
     .                       .FALSE.   )

                  ! Line from SURFACE
                  CALL CREATE_LINE_FROM_SURFACE(CLAUSE,KEYSET,OPT_A,OPT_E,DELBUF    ,
     .                                          .FALSE.)

                  ! Node from ELEMENT
                  CALL CREATE_NODE_FROM_ELEMENT(
     .                            IXS   ,IXS10  ,IXS20  ,IXS16  ,IXQ    ,
     .                            IXC   ,IXTG   ,IXT    ,IXP    ,IXR    ,
     .                            IXX   ,KXX    ,KXSP   ,CLAUSE ,GEO    ,
     .                            DUMMY_ARRAY   ,DSZ    ,.FALSE. )



              CASE ( 'SH3N' )
                  
                  ! SH3N LIST FROM CLAUSE
                  !-----------------------
                  CALL CREATE_ELT_CLAUSE(
     .                                    CLAUSE, ELT_SH3N,
     .                                    MAP_TABLES%ISH3NM, NUMELTG, 
     .                                    J      ,OPT_G   ,IS_AVAILABLE,LSUBMODEL  ,OPT_B    ,
     .                                    OPT_C  ,IBOX    ,X           ,SKEW       ,SET_TITLE,
     .                                    KEYSET ,NIXTG   ,IXTG        ,3          ,IPARTG   ,
     .                                    IPART  ,3       ,SH3TREE     ,KSH3TREE   )

                  ! Surface from ELEMENT
                  !-------------------
                  CALL CREATE_SURFACE_FROM_ELEMENT(
     .                       IXS       ,IXS10    ,SH4TREE   ,SH3TREE   ,IXC       ,
     .                       IXTG      ,KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC   ,
     .                       KNOD2ELTG ,NOD2ELTG ,IPARTC    ,IPARTG    ,IPARTS    ,
     .                       IPART     ,CLAUSE   ,OPT_A     ,OPT_O     ,IXQ       ,
     .                       KNOD2ELQ  ,NOD2ELQ  ,X         ,KEYSET    ,DELBUF    ,
     .                       .FALSE.   )

                  ! Line from SURFACE
                  CALL CREATE_LINE_FROM_SURFACE(CLAUSE,KEYSET,OPT_A,OPT_E,DELBUF    ,
     .                                          .FALSE.)

                  ! Node from ELEMENT
                  CALL CREATE_NODE_FROM_ELEMENT(
     .                            IXS   ,IXS10  ,IXS20  ,IXS16  ,IXQ    ,
     .                            IXC   ,IXTG   ,IXT    ,IXP    ,IXR    ,
     .                            IXX   ,KXX    ,KXSP   ,CLAUSE ,GEO    ,
     .                            DUMMY_ARRAY   ,DSZ    ,.FALSE. )



              CASE ( 'TRIA' )
                  
                  ! SH3N LIST FROM CLAUSE
                  !-----------------------
                  CALL CREATE_ELT_CLAUSE(
     .                                    CLAUSE, ELT_TRIA,
     .                                    MAP_TABLES%ITRIAM, NUMELTRIA, 
     .                                    J      ,OPT_G   ,IS_AVAILABLE,LSUBMODEL  ,OPT_B    ,
     .                                    OPT_C  ,IBOX    ,X           ,SKEW       ,SET_TITLE,
     .                                    KEYSET ,NIXTG   ,IXTG        ,3          ,IPARTG   ,
     .                                    IPART  ,ADMBID  ,ADMBID      ,ADMBID     )

                  ! No surfaces, no lines

                  ! Node from ELEMENT
                  CALL CREATE_NODE_FROM_ELEMENT(
     .                            IXS   ,IXS10  ,IXS20  ,IXS16  ,IXQ    ,
     .                            IXC   ,IXTG   ,IXT    ,IXP    ,IXR    ,
     .                            IXX   ,KXX    ,KXSP   ,CLAUSE ,GEO    ,
     .                            DUMMY_ARRAY   ,DSZ    ,.FALSE. )


              CASE ( 'TRUSS' )
                  
                  ! TRUSS LIST FROM CLAUSE
                  !-----------------------
                  CALL CREATE_ELT_CLAUSE(
     .                                    CLAUSE, ELT_TRUSS,
     .                                    MAP_TABLES%ITRUSSM, NUMELT, 
     .                                    J      ,OPT_G   ,IS_AVAILABLE,LSUBMODEL  ,OPT_B    ,
     .                                    OPT_C  ,IBOX    ,X           ,SKEW       ,SET_TITLE,
     .                                    KEYSET ,NIXT    ,IXT         ,2          ,IPARTT   ,
     .                                    IPART  ,ADMBID  ,ADMBID      ,ADMBID     )

                  ! Line from 1D_ELEMENT
                  !-------------------
                  CALL CREATE_LINE_FROM_ELEMENT(IXT    ,IXP  ,IXR  ,CLAUSE,DELBUF    ,
     .                                          .FALSE.)

                  ! Node from ELEMENT
                  CALL CREATE_NODE_FROM_ELEMENT(
     .                            IXS   ,IXS10  ,IXS20  ,IXS16  ,IXQ    ,
     .                            IXC   ,IXTG   ,IXT    ,IXP    ,IXR    ,
     .                            IXX   ,KXX    ,KXSP   ,CLAUSE ,GEO    ,
     .                            DUMMY_ARRAY   ,DSZ    ,.FALSE. )


              CASE ( 'BEAM' )
                  
                  ! BEAM LIST FROM CLAUSE
                  !-----------------------
                  CALL CREATE_ELT_CLAUSE(
     .                                    CLAUSE, ELT_BEAM,
     .                                    MAP_TABLES%IBEAMM, NUMELP, 
     .                                    J      ,OPT_G   ,IS_AVAILABLE,LSUBMODEL  ,OPT_B    ,
     .                                    OPT_C  ,IBOX    ,X           ,SKEW       ,SET_TITLE,
     .                                    KEYSET ,NIXP    ,IXP         ,2          ,IPARTP   ,
     .                                    IPART  ,ADMBID  ,ADMBID      ,ADMBID     )

                  ! Line from 1D_ELEMENT
                  !-------------------
                  CALL CREATE_LINE_FROM_ELEMENT(IXT    ,IXP  ,IXR  ,CLAUSE,DELBUF    ,
     .                                          .FALSE.)

                  ! Node from ELEMENT
                  CALL CREATE_NODE_FROM_ELEMENT(
     .                            IXS   ,IXS10  ,IXS20  ,IXS16  ,IXQ    ,
     .                            IXC   ,IXTG   ,IXT    ,IXP    ,IXR    ,
     .                            IXX   ,KXX    ,KXSP   ,CLAUSE ,GEO    ,
     .                            DUMMY_ARRAY   ,DSZ    ,.FALSE. )

              CASE ( 'SPRING' )
                  
                  ! SPRING LIST FROM CLAUSE
                  !-----------------------
                  CALL CREATE_ELT_CLAUSE(
     .                                    CLAUSE, ELT_SPRING,
     .                                    MAP_TABLES%ISPRINGM, NUMELR, 
     .                                    J      ,OPT_G   ,IS_AVAILABLE,LSUBMODEL  ,OPT_B    ,
     .                                    OPT_C  ,IBOX    ,X           ,SKEW       ,SET_TITLE,
     .                                    KEYSET ,NIXR    ,IXR         ,2          ,IPARTR   ,
     .                                    IPART  ,ADMBID  ,ADMBID      ,ADMBID     )

                  ! Line from 1D_ELEMENT
                  !-------------------
                  CALL CREATE_LINE_FROM_ELEMENT(IXT    ,IXP  ,IXR  ,CLAUSE,DELBUF    ,
     .                                          .FALSE.)

                  ! Node from ELEMENT
                  CALL CREATE_NODE_FROM_ELEMENT(
     .                            IXS   ,IXS10  ,IXS20  ,IXS16  ,IXQ    ,
     .                            IXC   ,IXTG   ,IXT    ,IXP    ,IXR    ,
     .                            IXX   ,KXX    ,KXSP   ,CLAUSE ,GEO    ,
     .                            DUMMY_ARRAY   ,DSZ    ,.FALSE. )


              CASE ('SET')

                  ALLOCATE(SETL(NSETS))
                  SETL_SIZE=0
            
                  CALL CREATE_SET_ARRAY(SETL ,SETL_SIZE,      ! Read the clause & compute the list of SETs
     .                                  MAP_TABLES%ISETM  , MAP_TABLES%NSET_GENERAL,
     .                                  J ,OPT_G ,IS_AVAILABLE ,
     .                                  LSUBMODEL,CLAUSE,1)
                  
                  CALL CREATE_SET_CLAUSE( SET,  
     *                                    SETL ,SETL_SIZE,
     *                                    CLAUSE,
     *                                    IXS       ,IXS10    ,IXS20     ,IXS16     ,IXQ    ,
     *                                    IXC       ,IXTG     ,IXT       ,IXP       ,IXR    ,
     *                                    IXX       ,KXX      ,KXSP      ,GEO       ,SH4TREE,
     *                                    SH3TREE   ,KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC,
     *                                    KNOD2ELTG ,NOD2ELTG ,IPARTC    ,IPARTG    ,IPARTS ,
     *                                    IPART     ,OPT_A    ,OPT_O     ,KNOD2ELQ  ,NOD2ELQ,
     *                                    X         ,KEYSET   ,OPT_E     ,DELBUF    )

                  DEALLOCATE(SETL)

              CASE ('SETCOL')

                  ALLOCATE(SETL(NSETS))
                  SETL_SIZE=0

                  CALL CREATE_SETCOL_ARRAY(SET,SETL, SETL_SIZE ,
     *                                   MAP_TABLES%ISETCOLM,MAP_TABLES%NSET_COLLECT,
     *                                   J,OPT_G ,IS_AVAILABLE ,
     *                                   LSUBMODEL) 

                  ! At this stage we have list of SETs with internal IDs
                  ! The merge is same for SETs
                  ! -----------------------------------------------------
                  CALL CREATE_SET_CLAUSE( SET,                
     *                                    SETL ,SETL_SIZE,
     *                                    CLAUSE,
     *                                    IXS       ,IXS10    ,IXS20     ,IXS16     ,IXQ    ,
     *                                    IXC       ,IXTG     ,IXT       ,IXP       ,IXR    ,
     *                                    IXX       ,KXX      ,KXSP      ,GEO       ,SH4TREE,
     *                                    SH3TREE   ,KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC,
     *                                    KNOD2ELTG ,NOD2ELTG ,IPARTC    ,IPARTG    ,IPARTS ,
     *                                    IPART     ,OPT_A    ,OPT_O     ,KNOD2ELQ  ,NOD2ELQ,
     *                                    X         ,KEYSET   ,OPT_E     ,DELBUF    )

              CASE ( 'SEG' )
                  
                  ! Surfaces and lignes LIST FROM CLAUSE
                  !-----------------------
                  CALL CREATE_SEG_CLAUSE(
     .               CLAUSE,  ITABM1  ,J     ,IS_AVAILABLE ,LSUBMODEL)


                  ! Line from SURFACE
!!                  ELTYP_SEG = 0
                  IF (CLAUSE%NB_SURF_SEG > 0)
     .            CALL CREATE_LINE_FROM_SURFACE(CLAUSE,KEYSET,OPT_A,OPT_E,DELBUF    ,
     .                                          .FALSE.)

                  ! Node from SEG (SURF or LINE)
                  CALL CREATE_NODE_FROM_SEG( CLAUSE )


              CASE ( 'ALL' )

                  !-----------------------     
                  ! ALL model
                  !-----------------------

                  ! PART LIST FROM CLAUSE
                  !-----------------------
                  CALL CREATE_PART_ALL_CLAUSE( CLAUSE )

                  ! Element from PART
                  !-------------------
                  CALL CREATE_ELEM_ALL_CLAUSE( CLAUSE )

                  ! Surface from ELEMENT
                  !-------------------
                  CALL CREATE_SURFACE_FROM_ELEMENT(
     .                       IXS       ,IXS10    ,SH4TREE   ,SH3TREE   ,IXC       ,
     .                       IXTG      ,KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC   ,
     .                       KNOD2ELTG ,NOD2ELTG ,IPARTC    ,IPARTG    ,IPARTS    ,
     .                       IPART     ,CLAUSE   ,OPT_A     ,OPT_O     ,IXQ       ,
     .                       KNOD2ELQ  ,NOD2ELQ  ,X         ,KEYSET    ,DELBUF    ,
     .                       .FALSE.   )


                  ! Line from 1D_ELEMENT
                  !-------------------
                  CALL CREATE_LINE_FROM_ELEMENT(IXT    ,IXP  ,IXR  ,CLAUSE,DELBUF    ,
     .                                          .FALSE.)

                  ! Line from SURFACE
!!                  ELTYP_ALL = 0
                  CALL CREATE_LINE_FROM_SURFACE(CLAUSE,KEYSET,OPT_A,OPT_E,DELBUF    ,
     .                                          .FALSE.)

                  ! Node   ( 1 ... NUMNOD )
                  CALL CREATE_NODE_ALL_CLAUSE( CLAUSE )


                 !IF (IDEBUG == 1)THEN
           !      print*,'Check_ALL_model'
           !      print*,'---------------'
           !      print*,'NUMNOD = ' ,NUMNOD,    'CLAUSE%NB_NODE = '  ,CLAUSE%NB_NODE
           !      print*,'NPART  = ' ,NPART,     'CLAUSE%NB_PART = '  ,CLAUSE%NB_PART
           !      print*,'SOLID  = ' ,NUMELS,    'CLAUSE%NB_SOLID = ' ,CLAUSE%NB_SOLID
           !      print*,'QUAD  = '  ,NUMELQ,    'CLAUSE%NB_QUAD = '  ,CLAUSE%NB_QUAD
           !      print*,'SHELL  = ' ,NUMELC,    'CLAUSE%NB_SH4N = '  ,CLAUSE%NB_SH4N
           !      print*,'SH3N  = '  ,NUMELTG,   'CLAUSE%NB_SH3N = '  ,CLAUSE%NB_SH3N
           !      print*,'TRIA  = '  ,NUMELTRIA, 'CLAUSE%NB_TRIA = '  ,CLAUSE%NB_TRIA
           !      print*,'TRUSS  = ' ,NUMELT,    'CLAUSE%NB_TRUSS = ' ,CLAUSE%NB_TRUSS
           !      print*,'BEAM  = '  ,NUMELP,    'CLAUSE%NB_BEAM = '  ,CLAUSE%NB_BEAM
           !      print*,'SPRING  = ',NUMELR,    'CLAUSE%NB_SPRING = ',CLAUSE%NB_SPRING
                 !ENDIF

              CASE ( 'BOX', 'BOX2' )

                  CALL CREATE_BOX_CLAUSE( 
     *                  CLAUSE   ,J        ,IS_AVAILABLE,LSUBMODEL ,KEYSET   ,
     *                  ITABM1   ,IBOX     ,X           ,SKEW      ,IXS10    ,
     *                  SET_TITLE,IPART    ,SH4TREE     ,SH3TREE   ,IPARTS   ,
     *                  IPARTQ   ,IPARTC   ,IPARTG      ,IPARTT    ,IPARTP   ,
     *                  IPARTR   ,IXS      ,IXQ         ,IXC       ,IXTG     ,
     *                  IXT      ,IXP      ,IXR         ,KNOD2ELS  ,NOD2ELS  ,
     *                  KNOD2ELC ,NOD2ELC  ,KNOD2ELTG   ,NOD2ELTG  ,KNOD2ELQ ,
     *                  NOD2ELQ  ,OPT_A    ,OPT_O       ,OPT_E     ,DELBUF   ,
     *                  RBY_MSN  ,MAP_TABLES%IRBODYM)


              CASE ( 'SUBS' )

                  ! PART LIST FROM CLAUSE
                  !-----------------------
                  CALL CREATE_SUBS_CLAUSE(
     .                                    CLAUSE   ,MAP_TABLES%ISUBSM ,J     ,OPT_G ,IS_AVAILABLE ,
     .                                    LSUBMODEL,SUBSET            )

                  ! Element from PART
                  !-------------------
                  CALL CREATE_ELEMENT_FROM_PART(CLAUSE  ,INV_GROUP) 

                  ! Surface from ELEMENT
                  !-------------------
                  CALL CREATE_SURFACE_FROM_ELEMENT(
     .                       IXS       ,IXS10    ,SH4TREE   ,SH3TREE   ,IXC       ,
     .                       IXTG      ,KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC   ,
     .                       KNOD2ELTG ,NOD2ELTG ,IPARTC    ,IPARTG    ,IPARTS    ,
     .                       IPART     ,CLAUSE   ,OPT_A     ,OPT_O     ,IXQ       ,
     .                       KNOD2ELQ  ,NOD2ELQ  ,X         ,KEYSET    ,DELBUF    ,
     .                       .FALSE.   )


                  ! Line from 1D_ELEMENT
                  !-------------------
                  CALL CREATE_LINE_FROM_ELEMENT(IXT    ,IXP  ,IXR  ,CLAUSE,DELBUF    ,
     .                                          .FALSE.)

                  ! Line from SURFACE
                  CALL CREATE_LINE_FROM_SURFACE(CLAUSE,KEYSET,OPT_A,OPT_E,DELBUF    ,
     .                                          .FALSE.)

                  ! Node from ELEMENT
                  CALL CREATE_NODE_FROM_ELEMENT(
     .                            IXS   ,IXS10  ,IXS20  ,IXS16  ,IXQ    ,
     .                            IXC   ,IXTG   ,IXT    ,IXP    ,IXR    ,
     .                            IXX   ,KXX    ,KXSP   ,CLAUSE ,GEO    ,
     .                            DUMMY_ARRAY   ,DSZ    ,.FALSE. )


              CASE ( 'SUBM' )


                  ! PART LIST FROM CLAUSE
                  !-----------------------
                  CALL CREATE_SUBM_CLAUSE(
     .                                    CLAUSE   ,MAP_TABLES%ISUBMM ,J     ,OPT_G ,IS_AVAILABLE ,
     .                                    LSUBMODEL,IPART             )

                  ! Element from PART
                  !-------------------
                  CALL CREATE_ELEMENT_FROM_PART(CLAUSE  ,INV_GROUP) 

                  ! Surface from ELEMENT
                  !-------------------
                  CALL CREATE_SURFACE_FROM_ELEMENT(
     .                       IXS       ,IXS10    ,SH4TREE   ,SH3TREE   ,IXC       ,
     .                       IXTG      ,KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC   ,
     .                       KNOD2ELTG ,NOD2ELTG ,IPARTC    ,IPARTG    ,IPARTS    ,
     .                       IPART     ,CLAUSE   ,OPT_A     ,OPT_O     ,IXQ       ,
     .                       KNOD2ELQ  ,NOD2ELQ  ,X         ,KEYSET    ,DELBUF    ,
     .                       .FALSE.   )


                  ! Line from 1D_ELEMENT
                  !-------------------
                  CALL CREATE_LINE_FROM_ELEMENT(IXT    ,IXP  ,IXR  ,CLAUSE,DELBUF    ,
     .                                          .FALSE.)

                  ! Line from SURFACE
                  CALL CREATE_LINE_FROM_SURFACE(CLAUSE,KEYSET,OPT_A,OPT_E,DELBUF    ,
     .                                          .FALSE.)



              CASE ( 'RBODY' )

                  ! RBODY LIST FROM CLAUSE
                  CALL CREATE_RBODY_CLAUSE(
     .                        CLAUSE   ,MAP_TABLES%IRBODYM ,J   ,OPT_G ,IS_AVAILABLE ,
     .                        LSUBMODEL,OPT_B              ,IBOX,X     ,SKEW         ,
     .                        SET_TITLE,KEYSET             ,RBY_MSN    )

                  ! Main node from RBODY
                  CALL CREATE_NODE_FROM_RBODY(RBY_MSN,CLAUSE)


              CASE DEFAULT

                  CALL ANCMSG(MSGID=1906,ANMODE=ANSTOP,
     .                             MSGTYPE=MSGERROR,
     .                             I1 = CLAUSE%SET_ID,
     .                             C1=TRIM(CLAUSE%TITLE),
     .                             C2=TRIM(KEYSET)  )


           END SELECT 

C -----------------------------------------------------
          CALL INSERT_CLAUSE_IN_SET(SET(IGS)  ,CLAUSE   ,CLAUSE_OPERATOR,
     .                              IXS       ,IXS10    ,IXS20     ,IXS16     ,IXQ    ,
     .                              IXC       ,IXTG     ,IXT       ,IXP       ,IXR    ,
     .                              IXX       ,KXX      ,KXSP      ,GEO       ,SH4TREE,
     .                              SH3TREE   ,KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC,
     .                              KNOD2ELTG ,NOD2ELTG ,IPARTC    ,IPARTG    ,IPARTS ,
     .                              IPART     ,OPT_A    ,OPT_O     ,KNOD2ELQ  ,NOD2ELQ,
     .                              X         ,KEYSET   ,OPT_E     ,DELBUF    )
C -----------------------------------------------------
!
        ENDDO ! DO J=1,CLAUSES_MAX

        IF( TRIM(KEY) == 'COLLECT' ) THEN
       
           IF (SET(IGS)%SET_ACTIV == 1)THEN         ! Merge the Collect SETs only if Active

            CALL CREATE_SET_COLLECT(SET       ,SET_ID   ,IGS     ,MAP_TABLES%ISETCOLM ,MAP_TABLES%NSET_COLLECT,
     *                              IXS       ,IXS10    ,IXS20   ,IXS16               ,IXQ    ,
     *                              IXC       ,IXTG     ,IXT     ,IXP                 ,IXR    ,
     *                              IXX       ,KXX      ,KXSP    ,GEO         ,SH4TREE,
     .                              SH3TREE   ,KNOD2ELS ,NOD2ELS ,KNOD2ELC    ,NOD2ELC,
     .                              KNOD2ELTG ,NOD2ELTG ,IPARTC  ,IPARTG      ,IPARTS ,
     .                              IPART     ,OPT_A    ,OPT_O   ,KNOD2ELQ    ,NOD2ELQ,
     .                              X         ,KEYSET   ,OPT_E   ,DELBUF      )

           ENDIF

        ENDIF

      ENDDO ! DO IGS=1,NSETS

      CALL CLAUSE_INIT(CLAUSE)



      !  -----------------------------------------------------
      !IF (IDEBUG == 1)THEN
      !  DO IGS=1,NSETS
      !    print*,'-----------'
      !    print*,IGS
      !    print*,'NB_PART=',SET(IGS)%NB_PART
      !    IF(SET(IGS)%NB_PART > 0)THEN
      !       DO I=1,SET(IGS)%NB_PART
      !          print*,'PARTS:',SET(IGS)%PART(I),IPART(4,SET(IGS)%PART(I))
      !       ENDDO
      !    ENDIF
      !  ENDDO
      !  print*,'-----------'
      !  print*,' '
      !ENDIF
      !  -----------------------------------------------------

      ! Fill Radioss group, lines surfaces
      !  -----------------------------------------------------
      CALL FILL_IGR(SET,
     .              IGRSURF ,IGRNOD  ,IGRSLIN  ,IGRPART ,IGRBRIC, IGRQUAD ,   
     .              IGRSH4N ,IGRSH3N ,IGRTRUSS ,IGRBEAM ,IGRSPRING)


      END 
