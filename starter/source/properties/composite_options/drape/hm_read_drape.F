Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty off
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
Chd|  HM_READ_PRELECDRAPE           source/properties/composite_options/drape/hm_read_drape.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PRELECDRAPE(IDRAPEID,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "drape_c.inc"
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDRAPEID(*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ID
      CHARACTER TITR*nchartitle,MESS*40
      DATA MESS/'DRAPE DEFINITION                        '/
c      REAL ou REAL*8
      my_real
     .    BID
C======================================================================|
      ! PREREAD OF DRAPE
      CALL HM_OPTION_START('/DRAPE')
      ! Loop over DRAPE
      DO I=1,NDRAPE
        TITR = ''   
        CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                          OPTION_ID      = ID,
     .                          OPTION_TITR    = TITR)
        IDRAPEID(I) = ID
      ENDDO ! DO I=1,NDRAPE
      ! Looking for double IDs
      CALL UDOUBLE(IDRAPEID,1,NDRAPE,MESS,0,BID)
C-------------------------------------
      RETURN
      END
C
Chd|====================================================================
Chd|  HM_READ_DRAPE                 source/properties/composite_options/drape/hm_read_drape.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOAT_ARRAY_2INDEXES   source/devtools/hm_reader/hm_get_float_array_2indexes.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_2INDEXES     source/devtools/hm_reader/hm_get_int_array_2indexes.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_GET_STRING_INDEX           source/devtools/hm_reader/hm_get_string_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        UDOUBLE3                      source/system/sysfus.F        
Chd|        DRAPE_MOD                     share/modules1/drape_mod.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_DRAPE(DRAPE_WRK ,IWORK_T ,IWORKSH,IGRSH3N    ,IGRSH4N,
     .                         IXC   ,IXTG  ,IGEO   ,IGEO_STACK ,LSUBMODEL  ,
     .                         UNITAB, INDXSH )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE STACK_MOD
      USE GROUPDEF_MOD
      USE DRAPE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE UNITAB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "drape_c.inc"
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ::  IWORKSH(3,*),IXC(NIXC,*),
     .            IXTG(NIXTG,*),IGEO(NPROPGI,*),
     .            IGEO_STACK(NPROPGI,*),INDXSH(NUMELC+NUMELTG)
c      REAL ou REAL*8
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N  
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (DRAPE_)  , DIMENSION(NUMELC + NUMELTG) ,TARGET :: DRAPE_WRK
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      TYPE(DRAPE_WORK_)     , DIMENSION(NUMELC+NUMELTG), TARGET :: IWORK_T
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  ::I, II,J,JJ,IX,ID,SHELL_ID,SH3N_ID,GRSHEL_ID,GRSH3N_ID,OFFC,
     . IT1,IT2,IT3,IT4,NEL,IAD,ITY,IDSHEL,IDSH3N,PID,
     . JPID,IGTYP,IE,IP,IDRP,JDRP,ISH,IGR,JGR,IS,LISTMAX,SLICELISTMAX,
     . NIS,NO_ISH,NO_ISHEUSED_DRAPE,NUMS,NPT,IPPID,ISL,NPT_DRP,NSLICE,
     . SLICELIST,NPT_SLICE,MAT_ID,NO_USED_DRAPE
      INTEGER , DIMENSION(NDRAPE) :: DRP_SHEL, DRP_SH3N,DRAPE_ID
c      REAL ou REAL*8
      my_real
     .    THINNING,THETA_DRAPE,BID
      CHARACTER TITR*nchartitle,DRAPE_ENTITY*nchartitle,
     .          MESS*40,MESS1*40,MESS2*40,
     .          MESS3*40,MESS4*40,MESS5*40
      DATA MESS/'DRAPE DEFINITION                        '/
      DATA MESS1/'SHELL                                   '/
      DATA MESS2/'GRSHEL                                  '/
      DATA MESS3/'SH3N                                    '/
      DATA MESS4/'GRSH3N                                  '/
      DATA MESS5/'/DRAPE                                  '/
      INTEGER, DIMENSION (:)  ,ALLOCATABLE :: TAGSH,INDX_TMP
      INTEGER, DIMENSION (:,:),ALLOCATABLE ::  ISH3N_DRP,IGRSH4N_DRP,ISH4N_DRP,IGRSH3N_DRP,
     .                                         ITMP_SH4N,ITMP_GRSH4N,ITMP_SH3N,ITMP_GRSH3N
      INTEGER, DIMENSION(:,:,:),ALLOCATABLE :: ISH4N,ISH3N,ISH4N_GR,ISH3N_GR
      my_real, DIMENSION(:,:,:),ALLOCATABLE :: RSH4N,RSH3N,RSH4N_GR,RSH3N_GR
      LOGICAL :: IS_AVAILABLE
C======================================================================|
      IS_AVAILABLE = .FALSE.
      DRAPE_ID = 0
      DRP_SHEL = 0
      DRP_SH3N = 0
      NUMELC_DRAPE = 0
      NUMELTG_DRAPE = 0
C
      ALLOCATE(TAGSH(NUMELC+NUMELTG), INDX_TMP(NUMELC + NUMELTG))
      INDX_TMP = 0
C
      !====================================================================================
      ! Start reading /DRAPE
      !====================================================================================
      CALL HM_OPTION_START('/DRAPE')
      ! Loop over DRAPE
      DO I=1,NDRAPE
        IT1 = 0
        IT2 = 0
        IT3 = 0
        IT4 = 0
        TAGSH(1:NUMELC+NUMELTG) = 0
        !---------------------------------
        ! Read TITLE and ID
        !---------------------------------
        TITR = ''   
        CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                          OPTION_ID      = ID,
     .                          OPTION_TITR    = TITR)
        DRAPE_ID(I) = ID
        IF (IPRI == 5) THEN
          WRITE (IOUT,1001)
        ELSE
          WRITE (IOUT,1002)
        ENDIF
        ! Count number of entities
        CALL HM_GET_INTV('drapelistmax',LISTMAX,IS_AVAILABLE,LSUBMODEL)
        SLICELISTMAX = 0
        DO II = 1,LISTMAX
          ! N of slice entities
           CALL  HM_GET_INT_ARRAY_INDEX('slicelistmax',SLICELIST,II,IS_AVAILABLE,LSUBMODEL)
           SLICELISTMAX= MAX(SLICELISTMAX,SLICELIST)
        ENDDO  
        !!
        ALLOCATE(ISH4N(LISTMAX,SLICELISTMAX,2) ,ISH4N_GR(LISTMAX,SLICELISTMAX,2),
     .           ISH3N(LISTMAX,SLICELISTMAX,2) ,ISH3N_GR(LISTMAX,SLICELISTMAX,2),
     .           ISH4N_DRP(LISTMAX,3),IGRSH4N_DRP(LISTMAX,3),ISH3N_DRP(LISTMAX,3),IGRSH3N_DRP(LISTMAX,3),
     .           ITMP_SH4N(2,LISTMAX),ITMP_SH3N(2,LISTMAX),ITMP_GRSH4N(2,LISTMAX),ITMP_GRSH3N(2,LISTMAX))
        ISH4N = 0
        ISH3N = 0
        ISH4N_GR = 0
        ISH3N_GR = 0
        ISH4N_DRP  = 0
        ISH3N_DRP  = 0
        IGRSH4N_DRP = 0
        IGRSH3N_DRP = 0
        ITMP_SH4N =0
        ITMP_SH3N =0
        ITMP_GRSH4N = 0
        ITMP_GRSH3N = 0
        ALLOCATE(RSH4N(LISTMAX,SLICELISTMAX,2),RSH3N(LISTMAX,SLICELISTMAX,2),RSH4N_GR(LISTMAX,SLICELISTMAX,2),
     .             RSH3N_GR(LISTMAX, SLICELISTMAX,2))
         RSH4N = ZERO
         RSH3N = ZERO
         RSH4N_GR = ZERO
         RSH3N_GR = ZERO
     
        !--------------------------------------------------------------------------
        ! Loop over entities
        !--------------------------------------------------------------------------
        DO II = 1,LISTMAX
          ! N of slice entities
           CALL  HM_GET_INT_ARRAY_INDEX('slicelistmax',SLICELIST,II,IS_AVAILABLE,LSUBMODEL)
 
         !! tag of DRAPE elements
          ! Reading the Drape entity type
          CALL HM_GET_STRING_INDEX('entity_type',DRAPE_ENTITY,II,10,IS_AVAILABLE) 
          DRAPE_ENTITY(LEN_TRIM(DRAPE_ENTITY)+1:10)=' '
          !------------------------------------------------------------------------
          ! 1 - If entity is a SHELL
          !------------------------------------------------------------------------
          IF (DRAPE_ENTITY(1:5) == 'SHELL') THEN
            ! Id of the SHELL
            CALL HM_GET_INT_ARRAY_INDEX('elem_sh_n4',SHELL_ID,II,IS_AVAILABLE,LSUBMODEL)
            IT1 = IT1 + 1      
            ISH4N_DRP(IT1,1) = SHELL_ID                                       
            ISH4N_DRP(IT1,2) = ID                  
            ISH4N_DRP(IT1,3) = SLICELIST            
            ITMP_SH4N(1,IT1) = SHELL_ID
            ITMP_SH4N(2,IT1) = ID
            DO JJ = 1, SLICELIST
            ! drape slice Thinning
               CALL HM_GET_FLOAT_ARRAY_2INDEXES('thinning',THINNING,II,JJ,IS_AVAILABLE,LSUBMODEL,UNITAB)
            ! drape slice Angle 
               CALL HM_GET_FLOAT_ARRAY_2INDEXES('theta_slice',THETA_DRAPE,II,JJ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
            ! Id of the Mat
               CALL HM_GET_INT_ARRAY_2INDEXES('mat_ID',MAT_ID,II,JJ,IS_AVAILABLE,LSUBMODEL)
            ! npt of slice
               CALL HM_GET_INT_ARRAY_2INDEXES('npt_slice',NPT_SLICE,II,JJ,IS_AVAILABLE,LSUBMODEL)     
            ! Checking shell ID
              IF (SHELL_ID == 0) THEN
               CALL ANCMSG(MSGID=1163,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     C1=MESS5,
     .                     I1=ID,
     .                     C2=MESS1,
     .                     I2=SHELL_ID)
              ENDIF
              IF (IPRI == 5)                                                       
     .            WRITE(IOUT,'(10X,I10,14X,A6,7X,I10,7X,I10,2(15X,1PG20.13))')            
     .            ID,DRAPE_ENTITY(1:5),SHELL_ID,JJ,THINNING,THETA_DRAPE               
              ! Converting angle from deg to rad                                   
              THETA_DRAPE=THETA_DRAPE*PI/HUNDRED80                                 
              ! Default thinning value                                             
              IF (THINNING == ZERO) THINNING = ONE              
              ! Tag shell element                    
              ISH4N(IT1,JJ,1) = MAT_ID                                         
              ISH4N(IT1,JJ,2) = NPT_SLICE                                      
              RSH4N(IT1,JJ,1) = THINNING                                       
              RSH4N(IT1,JJ,2) = THETA_DRAPE   
           ENDDO ! SLICELIST                                
          !----------------- -------------------------------------------------------
          ! 2 - If entity is a SH3N
          !------------------------------------------------------------------------
          ELSEIF (DRAPE_ENTITY(1:4) == 'SH3N') THEN
            ! Id of the SH3N
            CALL HM_GET_INT_ARRAY_INDEX('elem_sh_n3',SH3N_ID,II,IS_AVAILABLE,LSUBMODEL)
             !!
            IT2 = IT2 + 1      
            ISH3N_DRP(IT2,1) = SH3N_ID                                       
            ISH3N_DRP(IT2,2) =  ID                  
            ISH3N_DRP(IT2,3) =  SLICELIST
            ITMP_SH3N(1,IT2) = SH3N_ID
            ITMP_SH3N(2,IT2) = ID
            DO JJ = 1, SLICELIST 
                 ! drape slice Thinning    
                  CALL HM_GET_FLOAT_ARRAY_2INDEXES('thinning',THINNING,II,JJ,IS_AVAILABLE,LSUBMODEL,UNITAB)
                  ! drape slice Angle 
                  CALL HM_GET_FLOAT_ARRAY_2INDEXES('theta_slice',THETA_DRAPE,II,JJ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
                  ! Id of the Mat
                  CALL HM_GET_INT_ARRAY_2INDEXES('mat_ID',MAT_ID,II,JJ,IS_AVAILABLE,LSUBMODEL)
                  ! npt of slice
                  CALL HM_GET_INT_ARRAY_2INDEXES('npt_slice',NPT_SLICE,II,JJ,IS_AVAILABLE,LSUBMODEL)      
                  ! Checking sh3n ID
                  IF (SH3N_ID == 0) THEN
                     CALL ANCMSG(MSGID=1163,
     .                           MSGTYPE=MSGERROR,
     .                           ANMODE=ANINFO,
     .                           C1=MESS5,
     .                           I1=ID,
     .                           C2=MESS3,
     .                           I2=SH3N_ID)
                  ENDIF
                  IF (IPRI == 5)
     .            WRITE(IOUT,'(10X,I10,14X,A6,7X,I10,7X,I10,2(15X,1PG20.13))')    
     .                ID,DRAPE_ENTITY(1:4),SH3N_ID,JJ,THINNING,THETA_DRAPE
                  ! Converting angle from deg to rad
                  THETA_DRAPE=THETA_DRAPE*PI/HUNDRED80
                  ! Default thinning value
                  IF (THINNING == ZERO) THINNING = ONE
                  ! Tag sh3n element
                   ISH3N(IT2,JJ,1) = MAT_ID                                         
                   ISH3N(IT2,JJ,2) = NPT_SLICE                                      
                   RSH3N(IT2,JJ,1) = THINNING                                       
                   RSH3N(IT2,JJ,2) = THETA_DRAPE   
             ENDDO ! SLICELIST       
          !------------------------------------------------------------------------
          ! 3 - If entity is a Groupe of SHELL
          !------------------------------------------------------------------------
          ELSEIF (DRAPE_ENTITY(1:6) == 'GRSHEL') THEN
            ! Id of the GRSHEL
            CALL HM_GET_INT_ARRAY_INDEX('grshel_id',GRSHEL_ID,II,IS_AVAILABLE,LSUBMODEL)
                ! drape slice Thinning
               !!
            IT3 = IT3 + 1      
            IGRSH4N_DRP(IT3,1) =  GRSHEL_ID                                     
            IGRSH4N_DRP(IT3,2) =  ID                  
            IGRSH4N_DRP(IT3,3) =  SLICELIST
            ITMP_GRSH4N(1,IT3) = GRSHEL_ID
            ITMP_GRSH4N(2,IT3) = ID
            DO JJ = 1, SLICELIST     
                CALL HM_GET_FLOAT_ARRAY_2INDEXES('thinning',THINNING,II,JJ,IS_AVAILABLE,LSUBMODEL,UNITAB)         
                ! drape slice Angle                                                                              
                CALL HM_GET_FLOAT_ARRAY_2INDEXES('theta_slice',THETA_DRAPE,II,JJ,IS_AVAILABLE,LSUBMODEL,UNITAB)   
                ! Id of the Mat                                                                                  
                CALL HM_GET_INT_ARRAY_2INDEXES('mat_ID',MAT_ID,II,JJ,IS_AVAILABLE,LSUBMODEL)                      
                ! npt of slice                                                                                   
                CALL HM_GET_INT_ARRAY_2INDEXES('npt_slice',NPT_SLICE,II,JJ,IS_AVAILABLE,LSUBMODEL)                
                ! Checking grshell ID                                                                            
                IF (GRSHEL_ID == 0) THEN                                                                         
                   CALL ANCMSG(MSGID=1163,                                                                       
     .                         MSGTYPE=MSGERROR,                                                                 
     .                         ANMODE=ANINFO,                                                                    
     .                         C1=MESS5,                                                                         
     .                         I1=ID,                                                                            
     .                         C2=MESS2,                                                                         
     .                         I2=GRSHEL_ID)                                                                     
                ENDIF                                                                                            
                IF (IPRI == 5)                                                                                   
     .                WRITE(IOUT,'(10X,I10,14X,A6,7X,I10,7X,I10,2(15X,1PG20.13))')                                           
     .              ID,DRAPE_ENTITY(1:6),GRSHEL_ID,JJ,THINNING,THETA_DRAPE                                          
                ! Converting angle from deg to rad                                                               
                THETA_DRAPE=THETA_DRAPE*PI/HUNDRED80                                                             
                ! Default thinning value                                                                         
                IF (THINNING == ZERO) THINNING = ONE                                                             
                ! Tag grshell                                                              
                ISH4N_GR(IT3,JJ,1) = MAT_ID                                                                  
                ISH4N_GR(IT3,JJ,2) = NPT_SLICE                                                               
                RSH4N_GR(IT3,JJ,1)  = THINNING                                                                 
                RSH4N_GR(IT3,JJ,2)  = THETA_DRAPE                                                             
              ENDDO    
          !------------------------------------------------------------------------
          ! 4 - If entity is a Groupe of SH3N
          !------------------------------------------------------------------------
          ELSEIF (DRAPE_ENTITY(1:6) == 'GRSH3N') THEN
            ! Id of the GRSH3N
            CALL HM_GET_INT_ARRAY_INDEX('grtria_id',GRSH3N_ID,II,IS_AVAILABLE,LSUBMODEL)
               !!
            IT4 = IT4 + 1      
            IGRSH3N_DRP(IT4,1) =  GRSH3N_ID                                     
            IGRSH3N_DRP(IT4,2) =  ID                  
            IGRSH3N_DRP(IT4,3) =  SLICELIST
            ITMP_GRSH4N(1,IT4) = GRSH3N_ID
            ITMP_GRSH4N(2,IT4) = ID
            DO JJ = 1, SLICELIST 
                ! drape slice Thinning
                CALL HM_GET_FLOAT_ARRAY_2INDEXES('thinning',THINNING,II,JJ,IS_AVAILABLE,LSUBMODEL,UNITAB)
                ! drape slice Angle 
                CALL HM_GET_FLOAT_ARRAY_2INDEXES('theta_slice',THETA_DRAPE,II,JJ,IS_AVAILABLE,LSUBMODEL,UNITAB)  
                ! Id of the Mat
                CALL HM_GET_INT_ARRAY_2INDEXES('mat_ID',MAT_ID,II,JJ,IS_AVAILABLE,LSUBMODEL)
                ! npt of slice
                CALL HM_GET_INT_ARRAY_2INDEXES('npt_slice',NPT_SLICE,II,JJ,IS_AVAILABLE,LSUBMODEL)                 
                ! Checking grsh3n ID
                IF (GRSH3N_ID == 0) THEN
                   CALL ANCMSG(MSGID=1163,
     .                         MSGTYPE=MSGERROR,
     .                         ANMODE=ANINFO,
     .                         C1=MESS5,
     .                         I1=ID,
     .                         C2=MESS4,
     .                         I2=GRSH3N_ID)
                ENDIF
                IF (IPRI == 5)
     .                WRITE(IOUT,'(10X,I10,14X,A6,7X,I10,7X,I10,2(15X,1PG20.13))')     
     .              ID,DRAPE_ENTITY(1:6),GRSH3N_ID,JJ,THINNING,THETA_DRAPE
                ! Converting angle from deg to rad
                THETA_DRAPE = THETA_DRAPE*PI/HUNDRED80
                ! Default thinning value
                IF (THINNING == ZERO) THINNING = ONE
                ! Tag grsh3n
             
                ISH3N_GR(IT4,JJ,1) = MAT_ID
                ISH3N_GR(IT4,JJ,2) = NPT_SLICE
                RSH3N_GR(IT4,JJ,1)  = THINNING
                RSH3N_GR(IT4,JJ,2)  = THETA_DRAPE
            ENDDO ! SLICELIST     
          ENDIF
        ENDDO ! LISTMAX
        !------------------------------------------------------------
        ! CHECK FOR UNUSED DRAPE
        !------------------------------------------------------------
        NO_USED_DRAPE = 0
        IPPID = 2
        DO IE=1,NUMELC
          IX = IXC(NIXC,IE)
          PID = IXC(6,IE)
          IGTYP = IGEO(11,PID)
          NPT = IWORKSH(1,IE) 
          IF (IGTYP == 17 .OR. IGTYP == 51) THEN
            DO IP=1,NPT
              JPID = IWORK_T(IE)%PLYID(IP)! ply pid number
c                    IGEO(1, JPID)       ! ply pid ID
              IF (JPID > 0) THEN
                JDRP = IGEO(48,JPID)
                IF (ID == JDRP)THEN
                   NO_USED_DRAPE = NO_USED_DRAPE + 1
                ENDIF
              ENDIF
            ENDDO
          ELSEIF (IGTYP == 52) THEN
              DO IP=1,NPT
                JPID = IWORK_T(IE)%PLYID(IP) ! ply pid number
c                      IGEO(1, JPID)       ! ply pid ID
                IF (JPID > 0) THEN
                  JDRP = IGEO_STACK(48,JPID)
                  IF (ID == JDRP)THEN
                    NO_USED_DRAPE = NO_USED_DRAPE + 1
                  ENDIF
                ENDIF 
              ENDDO ! DO IP=1,N1
          ENDIF ! IF (IGTYP == 17 .OR. IGTYP == 51)
        ENDDO ! DO IE=1,NUMELC
C
        DO IE=1,NUMELTG
          IX = IXTG(NIXTG,IE)
          PID = IXTG(5,IE)
          IGTYP = IGEO(11,PID)
          NPT = IWORKSH(1,NUMELC + IE)
          IF (IGTYP == 17 .OR. IGTYP == 51) THEN
            DO IP=1,NPT
              JPID = IWORK_T(NUMELC + IE)%PLYID(IP) ! ply pid number
c                    IGEO(1, JPID)       ! ply pid ID
              IF (JPID > 0) THEN
                JDRP = IGEO(48,JPID)
                IF (ID == JDRP)THEN
                   NO_USED_DRAPE = NO_USED_DRAPE + 1
                ENDIF
              ENDIF
            ENDDO
          ELSEIF (IGTYP == 52) THEN
              DO IP=1,NPT
                JPID = IWORK_T(NUMELC + IE)%PLYID(IP) ! ply pid number
c                      IGEO(1, JPID)       ! ply pid ID
                IF (JPID > 0) THEN
                  JDRP = IGEO_STACK(48,JPID)
                  IF (ID == JDRP)THEN
                    NO_USED_DRAPE = NO_USED_DRAPE + 1
                  ENDIF
                ENDIF 
              ENDDO ! DO IP=1,N1
          ENDIF ! IF (IGTYP == 17 .OR. IGTYP == 51)
        ENDDO ! DO IE=1,NUMELTG
        ! Drape ID non-associated to any ply
        IF (NO_USED_DRAPE == 0) THEN
          CALL ANCMSG(MSGID=1169,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO,
     .                C1=MESS5,
     .                I1=ID)
        ENDIF
        !-------------------------------------------------------------------------
        ! Looking for ID doubles (shell, sh3n, grshel, grsh3n) in the same /DRAPE
        !-------------------------------------------------------------------------
        ! Double shell -
         CALL UDOUBLE3(ITMP_SH4N,2,IT1,MESS5,MESS1,0,BID)
        ! Double grshel -
        CALL UDOUBLE3(ITMP_GRSH4N,2,IT3,MESS5,MESS2,0,BID)
        ! To be checked for sh3n, grsh3n
        !   - double she3n -
        CALL UDOUBLE3(ITMP_SH3N,2,IT2,MESS5,MESS3,0,BID)
        !   - double grshel -
        CALL UDOUBLE3(ITMP_GRSH3N,2,IT4,MESS5,MESS4,0,BID)
        !-------------------------------------------------------------------------
        ! Filling DRAPE data structure
        !-------------------------------------------------------------------------
   
      IF (IT1 > 0) THEN
          DO J=1,IT1
            ISH    = ISH4N_DRP(J,1)
            IDRP   = ISH4N_DRP(J,2)
            NSLICE = ISH4N_DRP(J,3)
            NO_ISH = 0
            DO IE=1,NUMELC
              IX = IXC(NIXC,IE)
              PID = IXC(6,IE)
              IGTYP = IGEO(11,PID)
              NPT = IWORKSH(1,IE)
              NPT_DRP = 0
              IF (ISH == IX) THEN
                 NO_ISH = NO_ISH + 1
c tag of sh4n to check doubles within  the DRAPE
                IF (TAGSH(IE) == 0) THEN
                  TAGSH(IE) = ISH
                  NIS = 0
                  IF (.NOT.ALLOCATED(DRAPE_WRK(IE)%DRAPE_PLY)) THEN
                     ALLOCATE(DRAPE_WRK(IE)%DRAPE_PLY(NPT))
                     NUMELC_DRAPE = NUMELC_DRAPE + 1
                     INDX_TMP(IE) = NUMELC_DRAPE
                     DRAPE_WRK(IE)%NPLY_DRAPE = 0 
                  ENDIF   
                  NO_ISH = NO_ISH + 1
C  count DRAPE entities for printing out
                  DRP_SHEL(I) = DRP_SHEL(I) + 1
C
                  IF (IGTYP == 17 .OR. IGTYP == 51) THEN
                    IPPID = 2
                   IF (.NOT.ALLOCATED(DRAPE_WRK(IE)%INDX_PLY)) THEN
                         ALLOCATE(DRAPE_WRK(IE)%INDX_PLY(NPT)  )
                         DRAPE_WRK(IE)%INDX_PLY = 0 
                    ENDIF
                    NPT_DRP = DRAPE_WRK(IE)%NPLY_DRAPE
                    DO IP=1,NPT
                      JPID = IWORK_T(IE)%PLYID(IP) ! ply pid number
c                            IGEO(1, JPID)       ! ply pid ID
                      IF (JPID > 0) THEN
                        JDRP = IGEO(48,JPID)
                        IF (IDRP == JDRP)THEN
                             ALLOCATE(DRAPE_WRK(IE)%DRAPE_PLY(IP)%RDRAPE(NSLICE,2))
                             ALLOCATE(DRAPE_WRK(IE)%DRAPE_PLY(IP)%IDRAPE(NSLICE,2))
                             DRAPE_WRK(IE)%DRAPE_PLY(IP)%RDRAPE = ZERO
                             DRAPE_WRK(IE)%DRAPE_PLY(IP)%IDRAPE = 0
                             DRAPE_WRK(IE)%DRAPE_PLY(IP)%NSLICE = NSLICE
                             NPT_DRP = NPT_DRP + 1
                             DRAPE_WRK(IE)%NPLY_DRAPE = NPT_DRP
                             DRAPE_WRK(IE)%INDX_PLY(NPT_DRP) = IP
                             DRAPE_WRK(IE)%DRAPE_PLY(IP)%IPID =  IDRP                    
                             DO ISL = 1,NSLICE                                                            
                                DRAPE_WRK(IE)%DRAPE_PLY(IP)%RDRAPE(ISL,1) =  RSH4N(J,ISL,1)
                                DRAPE_WRK(IE)%DRAPE_PLY(IP)%RDRAPE(ISL,2) =  RSH4N(J,ISL,2)               
                                DRAPE_WRK(IE)%DRAPE_PLY(IP)%IDRAPE(ISL,1) =  ISH4N(J,ISL,1)  !! Mat_id    
                                DRAPE_WRK(IE)%DRAPE_PLY(IP)%IDRAPE(ISL,2) =  ISH4N(J,ISL,2)  !! NPT_SLICE 
                             ENDDO ! nbre of slice                     
check if SH4N of the DRAPE is inside any plys
                          NIS = NIS + 1
                        ENDIF
                      ENDIF 
                    ENDDO ! DO IP=1,NPT
                  ELSEIF (IGTYP == 52) THEN
                     IPPID = 2
                     IF (.NOT.ALLOCATED(DRAPE_WRK(IE)%INDX_PLY)) THEN
                         ALLOCATE(DRAPE_WRK(IE)%INDX_PLY(NPT)  )
                         DRAPE_WRK(IE)%INDX_PLY = 0 
                     ENDIF
                     NPT_DRP = DRAPE_WRK(IE)%NPLY_DRAPE
                     DO IP=1,NPT
                        JPID = IWORK_T(IE)%PLYID(IP) ! ply pid number
c                                   IGEO(1, JPID)       ! ply pid ID
                        IF (JPID > 0) THEN
                          JDRP = IGEO_STACK(48,JPID)
                          IF (IDRP==JDRP)THEN
                             ALLOCATE(DRAPE_WRK(IE)%DRAPE_PLY(IP)%RDRAPE(NSLICE,2)       )
                             ALLOCATE(DRAPE_WRK(IE)%DRAPE_PLY(IP)%IDRAPE(NSLICE,2)       )
                              DRAPE_WRK(IE)%DRAPE_PLY(IP)%RDRAPE = ZERO
                             DRAPE_WRK(IE)%DRAPE_PLY(IP)%IDRAPE = 0
                             DRAPE_WRK(IE)%DRAPE_PLY(IP)%NSLICE = NSLICE
                             NPT_DRP = NPT_DRP + 1
                             DRAPE_WRK(IE)%NPLY_DRAPE = NPT_DRP
                             DRAPE_WRK(IE)%INDX_PLY(NPT_DRP) = IP 
                             DRAPE_WRK(IE)%DRAPE_PLY(IP)%IPID =  IDRP                   
                             DO ISL = 1,NSLICE                                                            
                                DRAPE_WRK(IE)%DRAPE_PLY(IP)%RDRAPE(ISL,1) =  RSH4N(J,ISL,1)
                                DRAPE_WRK(IE)%DRAPE_PLY(IP)%RDRAPE(ISL,2) =  RSH4N(J,ISL,2)               
                                DRAPE_WRK(IE)%DRAPE_PLY(IP)%IDRAPE(ISL,1) =  ISH4N(J,ISL,1)  !! Mat_id    
                                DRAPE_WRK(IE)%DRAPE_PLY(IP)%IDRAPE(ISL,2) =  ISH4N(J,ISL,2)  !! NPT_SLICE 
                             ENDDO ! nbre of slice                                                        
C  count DRAPE entities for printing out
c                            DRP_SHEL(I) = DRP_SHEL(I) + 1
check if SH4N of the DRAPE is inside any plys
                            NIS = NIS + 1
                          ENDIF
                        ENDIF 
                      ENDDO ! DO IP=1,NPT
                  ENDIF ! F (IGTYP == 17 .OR. IGTYP == 51)
C---
                  IF (NIS == 0 .AND. 
     .               (IGTYP == 17. OR. IGTYP == 51 .OR. IGTYP == 52)) THEN
C
C  error message to be add
C
C  --- SH4N --- from /DRAPE not associated to a PID = 17, 51, 52 plys
C
                    CALL ANCMSG(MSGID=1172,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO,
     .                          C1=MESS5,
     .                          I1=ID,
     .                          C2=MESS1,
     .                          I2=ISH)
                  ELSEIF (NIS == 0 .AND. 
     .                    IGTYP /= 17. AND. IGTYP /= 51 .AND. IGTYP /= 52) THEN
C  --- SH4N --- from /DRAPE belong to a not allowed PID
                    CALL ANCMSG(MSGID=1171,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO,
     .                          C1=MESS5,
     .                          I1=ID,
     .                          C2=MESS1,
     .                          I2=ISH)
                  ENDIF ! IF (NIS == 0
                ENDIF ! IF (TAGSH(IE) == 0)
              ENDIF ! IF (ISH == IX)
            ENDDO ! DO IE=1,NUMELC
C---
            IF (NO_ISH == 0) THEN
C  --- SH4N --- from /DRAPE is not existing
              CALL ANCMSG(MSGID=1174,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    C1=MESS5,
     .                    I1=ID,
     .                    C2=MESS1,
     .                    I2=ISH)
            ENDIF
          ENDDO ! DO J=1,IT1
        ENDIF ! IF (IT1 > 0)
C---
C---
        IF (IT3 > 0) THEN
          DO J=1,IT3
            IGR    = IGRSH4N_DRP(J,1)
            IDRP   = IGRSH4N_DRP(J,2)
            NSLICE = IGRSH4N_DRP(J,3)
            DO JJ=1,NGRSHEL
              OFFC = NGRNOD + NGRBRIC + NGRQUAD + JJ
              JGR = IGRSH4N(JJ)%ID
              NEL = IGRSH4N(JJ)%NENTITY
C element type Q4
              ITY = IGRSH4N(JJ)%GRTYPE
              IF (IGR == JGR) THEN
                IF (ITY == 3) THEN  
                  DO II = 1,NEL
                    IDSHEL = IGRSH4N(JJ)%ENTITY(II)
                    PID = IXC(6,IDSHEL)
                    IGTYP = IGEO(11,PID)
                    NPT =IWORKSH(1,IDSHEL)
                     IF (TAGSH(IDSHEL) == 0) THEN
                      TAGSH(IDSHEL) = IDSHEL  
                      NIS = 0
                      IF (.NOT.ALLOCATED(DRAPE_WRK(IDSHEL)%DRAPE_PLY)) THEN
                          ALLOCATE(DRAPE_WRK(IDSHEL)%DRAPE_PLY(NPT))
                          NUMELC_DRAPE = NUMELC_DRAPE + 1
                          INDX_TMP(IDSHEL) = NUMELC_DRAPE
                          DRAPE_WRK(IDSHEL)%NPLY_DRAPE = 0 
                      ENDIF   
C
C  count DRAPE entities for printing out
                      DRP_SHEL(I) = DRP_SHEL(I) + 1
                      IPPID = 2
                      IF (IGTYP == 17 .OR. IGTYP == 51) THEN
                       IF (.NOT.ALLOCATED(DRAPE_WRK(IDSHEL)%INDX_PLY)) THEN
                          ALLOCATE(DRAPE_WRK(IDSHEL)%INDX_PLY(NPT)       )
                          DRAPE_WRK(IDSHEL)%INDX_PLY = 0 
                        ENDIF 
                        NPT_DRP = DRAPE_WRK(IDSHEL)%NPLY_DRAPE    
                        DO IP=1,NPT
                          JPID = IWORK_T(IDSHEL)%PLYID(IP) ! ply pid number
c                           IGEO(1, JPID)       ! ply pid ID
                          IF (JPID > 0) THEN
                            JDRP = IGEO(48,JPID)
                            IF (IDRP==JDRP)THEN
                             ALLOCATE(DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%RDRAPE(NSLICE,2)       )
                             ALLOCATE(DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IDRAPE(NSLICE,2)       )
                             DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IDRAPE = 0
                             DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%RDRAPE = ZERO
                             DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%NSLICE = NSLICE
                             NPT_DRP = NPT_DRP + 1
                             DRAPE_WRK(IDSHEL)%NPLY_DRAPE = NPT_DRP
                             DRAPE_WRK(IDSHEL)%INDX_PLY(NPT_DRP) = IP 
                             DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IPID =  IDRP                
                             DO ISL = 1,NSLICE                                                            
                                DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%RDRAPE(ISL,1) =  RSH4N_GR(J,ISL,1)
                                DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%RDRAPE(ISL,2) =  RSH4N_GR(J,ISL,2)               
                                DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IDRAPE(ISL,1) =  ISH4N_GR(J,ISL,1)  !! Mat_id    
                                DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IDRAPE(ISL,2) =  ISH4N_GR(J,ISL,2)  !! NPT_SLICE 
                             ENDDO ! nbre of slice                          
C
C  count DRAPE entities for printing out
c                              DRP_SHEL(I) = DRP_SHEL(I) + 1
C
check if SH4N of grshel of the DRAPE is inside any plys
                              NIS = NIS + 1
                            ENDIF
                          ENDIF
                        ENDDO
                      ELSEIF (IGTYP == 52) THEN
                          IF (.NOT.ALLOCATED(DRAPE_WRK(IDSHEL)%INDX_PLY)) THEN
                                 ALLOCATE(DRAPE_WRK(IDSHEL)%INDX_PLY(NPT)       )
                                 DRAPE_WRK(IDSHEL)%INDX_PLY = 0 
                                 DRAPE_WRK(IDSHEL)%NPLY_DRAPE = 0
                          ENDIF 
                          NPT_DRP  = DRAPE_WRK(IDSHEL)%NPLY_DRAPE
                          DO IP=1,NPT
                            JPID = IWORK_T(IDSHEL)%PLYID(IP)
                            IF (JPID > 0) THEN
                              JDRP = IGEO_STACK(48,JPID)
                              IF (IDRP==JDRP)THEN
                               ALLOCATE(DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%RDRAPE(NSLICE,2)       )
                               ALLOCATE(DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IDRAPE(NSLICE,2)       )
                               DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%RDRAPE = ZERO
                               DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IDRAPE = 0
                               DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%NSLICE = NSLICE
                               NPT_DRP = NPT_DRP + 1
                               DRAPE_WRK(IDSHEL)%NPLY_DRAPE = NPT_DRP
                               DRAPE_WRK(IDSHEL)%INDX_PLY(NPT_DRP) = IP
                               DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IPID =  IDRP 
                               DO ISL = 1,NSLICE                                                            
                                 DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%RDRAPE(ISL,1) =  RSH4N_GR(J,ISL,1)     
                                 DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%RDRAPE(ISL,2) =  RSH4N_GR(J,ISL,2)               
                                 DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IDRAPE(ISL,1) =  ISH4N_GR(J,ISL,1)  !! Mat_id    
                                 DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IDRAPE(ISL,2) =  ISH4N_GR(J,ISL,2)  !! NPT_SLICE
                               ENDDO ! nbre of slice                                                                                  
C  count DRAPE entities for printing out
c                                DRP_SHEL(I) = DRP_SHEL(I) + 1
C
check if SH4N of grshel of the DRAPE is inside any plys
                                NIS = NIS + 1
                              ENDIF
                            ENDIF 
                          ENDDO
                      ENDIF ! IF (IGTYP == 17 .OR. IGTYP == 51)
C---
                      IF (NIS == 0 .AND. 
     .                   (IGTYP == 17. OR. IGTYP == 51 .OR. IGTYP == 52)) THEN
C  --- SH4N --- from /DRAPE not associated to a PID = 17, 51, 52 plys
                        CALL ANCMSG(MSGID=1173,
     .                              MSGTYPE=MSGERROR,
     .                              ANMODE=ANINFO,
     .                              C1=MESS5,
     .                              I1=ID,
     .                              C2=MESS2,
     .                              I2=IGR,
     .                              C3=MESS1,
     .                              I3=IXC(NIXC,IDSHEL))
                      ELSEIF (NIS == 0 .AND. 
     .                        IGTYP /= 17. AND. IGTYP /= 51 .AND. IGTYP /= 52) THEN
C  --- SH4N --- from /DRAPE belong to a not allowed PID
                        CALL ANCMSG(MSGID=1170,
     .                              MSGTYPE=MSGERROR,
     .                              ANMODE=ANINFO,
     .                              C1=MESS5,
     .                              I1=ID,
     .                              C2=MESS2,
     .                              I2=IGR,
     .                              C3=MESS1,
     .                              I3=IXC(NIXC,IDSHEL))
                      ENDIF
                    ELSEIF (TAGSH(IDSHEL) == IXC(NIXC,IDSHEL)) THEN
                      CALL ANCMSG(MSGID=1155,
     .                            MSGTYPE=MSGERROR,
     .                            ANMODE=ANINFO,
     .                            C1=MESS,
     .                            I1=IDRP,
     .                            C2=MESS2,
     .                            I2=IGR,
     .                            C3=MESS1,
     .                            I3=IXC(NIXC,IDSHEL))
                    ENDIF ! IF (TAGSH(IE) == 0)
                  ENDDO ! DO II = 1,NEL
                ENDIF ! IF (ITY == 3)
              ENDIF ! IF (IGR == JGR)
            ENDDO ! DO JJ=1,NGRSHEL
          ENDDO ! DO J=1,IT3
        ENDIF ! IF (IT3 > 0)
        IF (IT2 > 0) THEN
          DO J=1,IT2
            ISH    = ISH3N_DRP(J,1)
            IDRP   = ISH3N_DRP(J,2)
            NSLICE = ISH3N_DRP(J,3)
            NO_ISH = 0
            DO IE=1,NUMELTG
              IX = IXTG(NIXTG,IE)
              PID = IXTG(5,IE)
              IGTYP = IGEO(11,PID)
              NPT_DRP = 0
              IF (ISH == IX) THEN
                NO_ISH = NO_ISH + 1
c   tag of SH3N to check doubles within grshel of the DRAPE
                NPT = IWORKSH(1,NUMELC +IE)  ! nb max de plys belong to the element
                IF (TAGSH(IE+NUMELC) == 0) THEN
                  TAGSH(IE+NUMELC) = ISH
                  NIS = 0
                  IF (.NOT.ALLOCATED(DRAPE_WRK(NUMELC + IE)%DRAPE_PLY)) THEN
                     ALLOCATE(DRAPE_WRK(NUMELC + IE)%DRAPE_PLY(NPT))
                     NUMELTG_DRAPE = NUMELTG_DRAPE + 1
                     INDX_TMP(NUMELC + IE) = NUMELTG_DRAPE
                     DRAPE_WRK(IE + NUMELC)%NPLY_DRAPE = 0 
                  ENDIF  
C  count DRAPE entities for printing out
                  DRP_SH3N(I) = DRP_SH3N(I) + 1
                  IPPID = 2
                  IF (IGTYP == 17 .OR. IGTYP == 51) THEN
                    IF (.NOT.ALLOCATED(DRAPE_WRK(NUMELC + IE)%INDX_PLY)) THEN
                         ALLOCATE(DRAPE_WRK(NUMELC + IE)%INDX_PLY(NPT)       )
                           DRAPE_WRK(NUMELC + IE)%INDX_PLY = 0 
                    ENDIF 
                    NPT_DRP = DRAPE_WRK(NUMELC + IE)%NPLY_DRAPE  
                    DO IP=1,NPT
                      JPID = IWORK_T(NUMELC + IE)%PLYID(IP)  ! ply pid 
c                            IGEO(1, JPID)       ! ply pid ID
                      IF (JPID > 0) THEN
                        JDRP = IGEO(48,JPID)
                        IF (IDRP==JDRP)THEN
                               ALLOCATE(DRAPE_WRK(IE+NUMELC)%DRAPE_PLY(IP)%RDRAPE(NSLICE,2)       )
                               ALLOCATE(DRAPE_WRK(IE+NUMELC)%DRAPE_PLY(IP)%IDRAPE(NSLICE,2)       )
                               DRAPE_WRK(IE+NUMELC)%DRAPE_PLY(IP)%RDRAPE = ZERO
                               DRAPE_WRK(IE+NUMELC)%DRAPE_PLY(IP)%IDRAPE = 0
                               DRAPE_WRK(IE+NUMELC)%DRAPE_PLY(IP)%NSLICE = NSLICE
                               NPT_DRP = NPT_DRP + 1
                               DRAPE_WRK(IE+NUMELC)%NPLY_DRAPE = NPT_DRP
                               DRAPE_WRK(IE+NUMELC)%INDX_PLY(NPT_DRP) = IP 
                               DRAPE_WRK(IE+NUMELC)%DRAPE_PLY(IP)%IPID =  IDRP                                     
                               DO ISL = 1,NSLICE                                                            
                                 DRAPE_WRK(IE+NUMELC)%DRAPE_PLY(IP)%RDRAPE(ISL,1) =  RSH3N(J,ISL,1)
                                 DRAPE_WRK(IE+NUMELC)%DRAPE_PLY(IP)%RDRAPE(ISL,2) =  RSH3N(J,ISL,2)               
                                 DRAPE_WRK(IE+NUMELC)%DRAPE_PLY(IP)%IDRAPE(ISL,1) =  ISH3N(J,ISL,1)  !! Mat_id    
                                 DRAPE_WRK(IE+NUMELC)%DRAPE_PLY(IP)%IDRAPE(ISL,2) =  ISH3N(J,ISL,2)  !! NPT_SLICE 
                               ENDDO ! nbre of slice   
C
C  count DRAPE entities for printing out
c                          DRP_SH3N(I) = DRP_SH3N(I) + 1
C
check if SH3N of grshel of the DRAPE is inside any plys
                          NIS = NIS + 1
                        ENDIF
                      ENDIF 
                    ENDDO ! DO IP=1,NPT
                  ELSEIF (IGTYP == 52) THEN
                    IF (.NOT.ALLOCATED(DRAPE_WRK(NUMELC + IE)%INDX_PLY)) THEN
                         ALLOCATE(DRAPE_WRK(NUMELC + IE)%INDX_PLY(NPT)       )
                          DRAPE_WRK(NUMELC + IE)%INDX_PLY = 0 
                    ENDIF 
                    NPT_DRP = DRAPE_WRK(NUMELC + IE)%NPLY_DRAPE 
                      DO IP=1,NPT
                        JPID = IWORK_T(NUMELC + IE)%PLYID(IP)  ! ply pid 
c                              IGEO(1, JPID)       ! ply pid ID
                        IF (JPID > 0) THEN
                          JDRP = IGEO_STACK(48,JPID)
                          IF (IDRP==JDRP)THEN
                           ALLOCATE(DRAPE_WRK(NUMELC + IE)%DRAPE_PLY(IP)%RDRAPE(NSLICE,2)       )
                           ALLOCATE(DRAPE_WRK(NUMELC + IE)%DRAPE_PLY(IP)%IDRAPE(NSLICE,2)       )
                           DRAPE_WRK(NUMELC + IE)%DRAPE_PLY(IP)%NSLICE = NSLICE                                  
                           NPT_DRP = NPT_DRP + 1
                           DRAPE_WRK(NUMELC + IE)%NPLY_DRAPE = NPT_DRP
                           DRAPE_WRK(NUMELC + IE)%INDX_PLY(NPT_DRP) = IP
                           DRAPE_WRK(NUMELC + IE)%DRAPE_PLY(IP)%IPID =  IDRP                                        
                           DO ISL = 1,NSLICE                                                            
                            DRAPE_WRK(NUMELC + IE)%DRAPE_PLY(IP)%RDRAPE(ISL,1) =  RSH3N(J,ISL,1) 
                            DRAPE_WRK(NUMELC + IE)%DRAPE_PLY(IP)%RDRAPE(ISL,2) =  RSH3N(J,ISL,2)               
                            DRAPE_WRK(NUMELC + IE)%DRAPE_PLY(IP)%IDRAPE(ISL,1) =  ISH3N(J,ISL,1)  !! Mat_id    
                            DRAPE_WRK(NUMELC + IE)%DRAPE_PLY(IP)%IDRAPE(ISL,2) =  ISH3N(J,ISL,2)  !! NPT_SLICE 
                           ENDDO ! nbre of slice                         
C
C  count DRAPE entities for printing out
c                          DRP_SH3N(I) = DRP_SH3N(I) + 1
C
check if SH3N of grshel of the DRAPE is inside any plys
                            NIS = NIS + 1
                          ENDIF
                        ENDIF 
                      ENDDO ! DO IP=1,NPT
                  ENDIF ! IF (IGTYP == 17 .OR. IGTYP == 51)
C---
                  IF (NIS == 0 .AND. 
     .               (IGTYP == 17. OR. IGTYP == 51 .OR. IGTYP == 52)) THEN
C  --- SH3N --- from /DRAPE not associated to a PID = 17, 51, 52 plys
                    CALL ANCMSG(MSGID=1172,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO,
     .                          C1=MESS5,
     .                          I1=ID,
     .                          C2=MESS3,
     .                          I2=ISH)
                  ELSEIF (NIS == 0 .AND. 
     .                    IGTYP /= 17. AND. IGTYP /= 51 .AND. IGTYP /= 52) THEN
C  --- SH3N --- from /DRAPE belong to a not allowed PID
                    CALL ANCMSG(MSGID=1171,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO,
     .                          C1=MESS5,
     .                          I1=ID,
     .                          C2=MESS3,
     .                          I2=ISH)
                  ENDIF ! IF (NIS == 0
                ENDIF ! IF (TAGSH(IE+NUMELC) == 0)
              ENDIF ! IF (ISH == IX)
            ENDDO ! DO IE=1,NUMELTG
C---
            IF (NO_ISH == 0) THEN
C  --- SH3N --- from /DRAPE is not existing
              CALL ANCMSG(MSGID=1174,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    C1=MESS5,
     .                    I1=ID,
     .                    C2=MESS3,
     .                    I2=ISH)
            ENDIF
          ENDDO ! DO J=1,IT2
        ENDIF ! IF (IT2 > 0)
C---
        IF (IT4 > 0) THEN
          DO J=1,IT4
            IGR    = IGRSH3N_DRP(J,1)
            IDRP   = IGRSH3N_DRP(J,2)
            NSLICE = IGRSH3N_DRP(J,3)
            DO JJ=1, NGRSH3N
              OFFC = NGRNOD + NGRBRIC + NGRQUAD + NGRSHEL + NGRTRUS + 
     .               NGRBEAM + NGRSPRI + JJ
              JGR = IGRSH3N(JJ)%ID
              NEL = IGRSH3N(JJ)%NENTITY
C element type T3 
              ITY = IGRSH3N(JJ)%GRTYPE
              IF (IGR == JGR) THEN
                IF (ITY == 7) THEN  !!! obsolete
                  DO II = 1,NEL
                    IDSH3N = IGRSH3N(JJ)%ENTITY(II)
                    IDSHEL = IDSH3N + NUMELC
                    PID = IXTG(5,IDSH3N)
                    IGTYP = IGEO(11,PID)
                    NPT = IWORKSH(1,IDSHEL)
                    NPT_DRP = 0
                    IF (TAGSH(IDSHEL) == 0) THEN
                     TAGSH(IDSHEL) =  IXTG(NIXTG,IDSH3N)
                     NIS = 0
                     IF (.NOT.ALLOCATED(DRAPE_WRK(IDSHEL)%DRAPE_PLY)) THEN
                      ALLOCATE(DRAPE_WRK(IDSHEL)%DRAPE_PLY(NPT))
                      NUMELTG_DRAPE = NUMELTG_DRAPE + 1
                      INDX_TMP(IDSHEL) = NUMELTG_DRAPE
                      DRAPE_WRK(IDSHEL)%NPLY_DRAPE = 0
                     ENDIF 
C
C  count DRAPE entities for printing out
                      DRP_SH3N(I) = DRP_SH3N(I) + 1
                       IPPID = 2
                       IF (IGTYP == 17 .OR. IGTYP == 51) THEN
                        IF (.NOT.ALLOCATED(DRAPE_WRK(IDSHEL)%INDX_PLY)) THEN
                         ALLOCATE(DRAPE_WRK(IDSHEL)%INDX_PLY(NPT)       )
                           DRAPE_WRK(IDSHEL)%INDX_PLY = 0 
                        ENDIF 
                        NPT_DRP = DRAPE_WRK(IDSHEL)%NPLY_DRAPE                          
                        DO IP=1,NPT
                          JPID = IWORK_T(IDSHEL)%PLYID(IP)  ! ply pid number
c                           IGEO(1, JPID)       ! ply pid ID
                          IF (JPID > 0) THEN
                            JDRP = IGEO(48,JPID)
                            IF (IDRP==JDRP)THEN
                              ALLOCATE(DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%RDRAPE(NSLICE,2))
                              ALLOCATE(DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IDRAPE(NSLICE,2))
                              DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%RDRAPE = ZERO
                              DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IDRAPE = 0
                              DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%NSLICE = NSLICE
                              NPT_DRP = NPT_DRP + 1                                  
                              DRAPE_WRK(IDSHEL)%NPLY_DRAPE = NPT_DRP 
                              DRAPE_WRK(IDSHEL)%INDX_PLY(NPT_DRP)= IP
                              DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IPID =  IDRP                  
                              DO ISL = 1,NSLICE                                                            
                                DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%RDRAPE(ISL,1) =  RSH3N_GR(J,ISL,1)           
                                DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%RDRAPE(ISL,2) =  RSH3N_GR(J,ISL,2)              
                                DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IDRAPE(ISL,1) =  ISH3N_GR(J,ISL,1)  !! Mat_id    
                                DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IDRAPE(ISL,2) =  ISH3N_GR(J,ISL,2)  !! NPT_SLICE 
                              ENDDO ! nbre of slice  
C  count DRAPE entities for printing out
c                              DRP_SH3N(I) = DRP_SH3N(I) + 1
check if SH3N of grshel of the DRAPE is inside any plys
                              NIS = NIS + 1
                            ENDIF
                          ENDIF
                        ENDDO ! DO IP=1,N1
                      ELSEIF (IGTYP == 52) THEN
                          IF (.NOT.ALLOCATED(DRAPE_WRK(IDSHEL)%INDX_PLY)) THEN
                           ALLOCATE(DRAPE_WRK(IDSHEL)%INDX_PLY(NPT)       )
                           DRAPE_WRK(IDSHEL)%INDX_PLY = 0 
                          ENDIF 
                          NPT_DRP = DRAPE_WRK(IDSHEL)%NPLY_DRAPE      
                          DO IP=1,NPT
                            JPID = IWORK_T(IDSHEL)%PLYID(IP)
c                             IGEO(1, JPID)       ! ply pid ID
                            IF (JPID > 0) THEN
                              JDRP = IGEO_STACK(48,JPID)
                              IF (IDRP==JDRP)THEN
                              ALLOCATE(DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%RDRAPE(NSLICE,2)       )
                              ALLOCATE(DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IDRAPE(NSLICE,2)       )
                              DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%RDRAPE = ZERO
                              DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IDRAPE = 0
                              DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%NSLICE = NSLICE 
                              NPT_DRP = NPT_DRP + 1                                  
                              DRAPE_WRK(IDSHEL)%NPLY_DRAPE = NPT_DRP 
                              DRAPE_WRK(IDSHEL)%INDX_PLY(NPT_DRP)= IP 
                              DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IPID =  IDRP 
                              DO ISL = 1,NSLICE                                                            
                                DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%RDRAPE(ISL,1) =  RSH3N_GR(J,ISL,1)              
                                DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%RDRAPE(ISL,2) =  RSH3N_GR(J,ISL,2)               
                                DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IDRAPE(ISL,1) =  ISH3N_GR(J,ISL,1)  !! Mat_id    
                                DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IDRAPE(ISL,2) =  ISH3N_GR(J,ISL,2)  !! NPT_SLICE 
                              ENDDO ! nbre of slice                          
C  count DRAPE entities for printing out
c                                DRP_SH3N(I) = DRP_SH3N(I) + 1
C
check if SH3N of grshel of the DRAPE is inside any plys
                                NIS = NIS + 1
                              ENDIF
                            ENDIF
                          ENDDO
                      ENDIF ! IF (IGTYP == 17 .OR. IGTYP == 51)
C---
                      IF (NIS == 0 .AND. 
     .                   (IGTYP == 17. OR. IGTYP == 51 .OR. IGTYP == 52)) THEN
C  --- SH3N --- from /DRAPE not associated to a PID = 17, 51, 52 plys
                        CALL ANCMSG(MSGID=1173,
     .                              MSGTYPE=MSGERROR,
     .                              ANMODE=ANINFO,
     .                              C1=MESS5,
     .                              I1=ID,
     .                              C2=MESS4,
     .                              I2=IGR,
     .                              C3=MESS3,
     .                              I3=IXTG(NIXTG,IDSH3N))
                      ELSEIF (NIS == 0 .AND. 
     .                        IGTYP /= 17. AND. IGTYP /= 51 .AND. IGTYP /= 52) THEN
C  --- SH3N --- from /DRAPE belong to a not allowed PID
                        CALL ANCMSG(MSGID=1170,
     .                              MSGTYPE=MSGERROR,
     .                              ANMODE=ANINFO,
     .                              C1=MESS5,
     .                              I1=ID,
     .                              C2=MESS4,
     .                              I2=IGR,
     .                              C3=MESS3,
     .                              I3=IXTG(NIXTG,IDSH3N))
                      ENDIF
                    ELSEIF (TAGSH(IDSHEL) == IXTG(NIXTG,IDSH3N)) THEN
                      CALL ANCMSG(MSGID=1155,
     .                            MSGTYPE=MSGERROR,
     .                            ANMODE=ANINFO,
     .                            C1=MESS,
     .                            I1=IDRP,
     .                            C2=MESS4,
     .                            I2=IGR,
     .                            C3=MESS3,
     .                            I3=IXTG(NIXTG,IDSH3N))
                    ENDIF ! IF (TAGSH(IDSHEL) == 0)
                  ENDDO ! DO II = 1,NEL
                ENDIF ! IF (ITY == 7)
              ENDIF ! IF (IGR == JGR)
            ENDDO ! DO JJ=1,NGRSHEL
          ENDDO ! DO J=1,IT4
        ENDIF ! IF (IT4 > 0)
C---
        IF (IPRI < 5) WRITE(IOUT,'(10X,I10,2(15X,I10))')
     .                      ID,DRP_SHEL(I),DRP_SH3N(I)
C---
         DEALLOCATE(ISH4N,ISH4N_GR,ISH3N ,ISH3N_GR, 
     .               ISH4N_DRP,IGRSH4N_DRP,ISH3N_DRP,IGRSH3N_DRP,
     .               ITMP_SH4N,ITMP_SH3N, ITMP_GRSH4N, ITMP_GRSH3N)
         DEALLOCATE(RSH4N,RSH3N,RSH4N_GR,   RSH3N_GR)
      ENDDO ! DO I=1,NDRAPE
!
      IF(NUMELC_DRAPE > 0) THEN
         DO I=1,NUMELC
          II = INDX_TMP(I)
          IF(II > 0)INDXSH(II) = I
         ENDDO
      ENDIF
       
      IF(NUMELTG_DRAPE > 0) THEN
         DO I=1,NUMELTG
          II = INDX_TMP(I + NUMELC)
          IF(II > 0) INDXSH(NUMELC_DRAPE + II) = I+ NUMELC
         ENDDO
      ENDIF
      DEALLOCATE(INDX_TMP)
      !====================================================================================
      ! End reading /DRAPE
      !====================================================================================
C-------------------------------------
C Recherche des ID doubles (parmis les options /DRAPE)
C-------------------------------------
      CALL UDOUBLE(DRAPE_ID,1,NDRAPE,MESS,0,BID)
C---
C-----------------------------
 1001 FORMAT(//
     .'     DRAPE OPTION     '/
     .'     -------------    '/
     .'        DRAPE NUMBER         ENTITY TYPE        ENTITY ID     SLICE NUMBER',
     .'            PLY THINNING FACTOR       PLY ORIENTATION ANGLE CHANGE')
 1002 FORMAT(//
     .'     DRAPE OPTION     '/
     .'     -------------    '/
     .'        DRAPE NUMBER    NB. OF SHELL ELEMENTS     NB. OF SH3N ELEMENTS')
C-----------------------------
      RETURN
      END
