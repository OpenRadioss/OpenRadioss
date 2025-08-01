Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    hm_read_fail                 ../starter/source/materials/fail/hm_read_fail.F
!||--- called by ------------------------------------------------------
!||    read_material_models         ../starter/source/materials/read_material_models.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                       ../starter/source/output/message/message.F
!||    fail_init                    ../starter/source/materials/fail/fail_init.F
!||    hm_get_boolv                 ../starter/source/devtools/hm_reader/hm_get_boolv.F
!||    hm_get_intv                  ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_count              ../starter/source/devtools/hm_reader/hm_option_count.F
!||    hm_option_read_key           ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start              ../starter/source/devtools/hm_reader/hm_option_start.F
!||    hm_read_fail_alter           ../starter/source/materials/fail/windshield_alter/hm_read_fail_alter.F
!||    hm_read_fail_biquad          ../starter/source/materials/fail/biquad/hm_read_fail_biquad.F
!||    hm_read_fail_chang           ../starter/source/materials/fail/changchang/hm_read_fail_chang.F
!||    hm_read_fail_cockcroft       ../starter/source/materials/fail/cockroft_latham/hm_read_fail_cockcroft.F
!||    hm_read_fail_composite       ../starter/source/materials/fail/composite/hm_read_fail_composite.F90
!||    hm_read_fail_connect         ../starter/source/materials/fail/connect/hm_read_fail_connect.F
!||    hm_read_fail_emc             ../starter/source/materials/fail/emc/hm_read_fail_emc.F
!||    hm_read_fail_energy          ../starter/source/materials/fail/energy/hm_read_fail_energy.F
!||    hm_read_fail_fabric          ../starter/source/materials/fail/fabric/hm_read_fail_fabric.F
!||    hm_read_fail_fld             ../starter/source/materials/fail/fld/hm_read_fail_fld.F
!||    hm_read_fail_gene1           ../starter/source/materials/fail/gene1/hm_read_fail_gene1.F
!||    hm_read_fail_gurson          ../starter/source/materials/fail/gurson/hm_read_fail_gurson.F
!||    hm_read_fail_hashin          ../starter/source/materials/fail/hashin/hm_read_fail_hashin.F
!||    hm_read_fail_hc_dsse         ../starter/source/materials/fail/hc_dsse/hm_read_fail_hc_dsse.F
!||    hm_read_fail_hoffman         ../starter/source/materials/fail/hoffman/hm_read_fail_hoffman.F
!||    hm_read_fail_inievo          ../starter/source/materials/fail/inievo/hm_read_fail_inievo.F
!||    hm_read_fail_johnson         ../starter/source/materials/fail/johnson_cook/hm_read_fail_johnson.F
!||    hm_read_fail_ladeveze        ../starter/source/materials/fail/ladeveze/hm_read_fail_ladeveze.F
!||    hm_read_fail_lemaitre        ../starter/source/materials/fail/lemaitre/hm_read_fail_lemaitre.F90
!||    hm_read_fail_maxstrain       ../starter/source/materials/fail/max_strain/hm_read_fail_maxstrain.F
!||    hm_read_fail_nxt             ../starter/source/materials/fail/nxt/hm_read_fail_nxt.F
!||    hm_read_fail_orthbiquad      ../starter/source/materials/fail/orthbiquad/hm_read_fail_orthbiquad.F
!||    hm_read_fail_orthenerg       ../starter/source/materials/fail/orthenerg/hm_read_fail_orthenerg.F
!||    hm_read_fail_orthstrain      ../starter/source/materials/fail/orthstrain/hm_read_fail_orthstrain.F
!||    hm_read_fail_puck            ../starter/source/materials/fail/puck/hm_read_fail_puck.F
!||    hm_read_fail_rtcl            ../starter/source/materials/fail/rtcl/hm_read_fail_rtcl.F
!||    hm_read_fail_sahraei         ../starter/source/materials/fail/sahraei/hm_read_fail_sahraei.F
!||    hm_read_fail_snconnect       ../starter/source/materials/fail/snconnect/hm_read_fail_snconnect.F
!||    hm_read_fail_spalling        ../starter/source/materials/fail/spalling/hm_read_fail_spalling.F90
!||    hm_read_fail_syazwan         ../starter/source/materials/fail/syazwan/hm_read_fail_syazwan.F
!||    hm_read_fail_tab1            ../starter/source/materials/fail/tabulated/hm_read_fail_tab1.F
!||    hm_read_fail_tab2            ../starter/source/materials/fail/tabulated/hm_read_fail_tab2.F
!||    hm_read_fail_tab_old         ../starter/source/materials/fail/tabulated/hm_read_fail_tab_old.F
!||    hm_read_fail_tbutcher        ../starter/source/materials/fail/tuler_butcher/hm_read_fail_tbutcher.F
!||    hm_read_fail_tensstrain      ../starter/source/materials/fail/tensstrain/hm_read_fail_tensstrain.F
!||    hm_read_fail_tsaihill        ../starter/source/materials/fail/tsaihill/hm_read_fail_tsaihill.F
!||    hm_read_fail_tsaiwu          ../starter/source/materials/fail/tsaiwu/hm_read_fail_tsaiwu.F
!||    hm_read_fail_user            ../starter/source/materials/fail/failuser/hm_read_fail_user.F
!||    hm_read_fail_visual          ../starter/source/materials/fail/visual/hm_read_fail_visual.F
!||    hm_read_fail_wierzbicki      ../starter/source/materials/fail/wierzbicki/hm_read_fail_wierzbicki.F
!||    hm_read_fail_wilkins         ../starter/source/materials/fail/wilkins/hm_read_fail_wilkins.F
!||    hm_read_fractal_dmg          ../starter/source/materials/fail/fractal/hm_read_fractal_dmg.F90
!||    hm_read_mullins_or           ../starter/source/materials/fail/mullins_or/hm_read_fail_mullins_or.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod                 ../starter/share/modules1/elbuftag_mod.F
!||    hm_option_read_mod           ../starter/share/modules1/hm_option_read_mod.F
!||    hm_read_fail_composite_mod   ../starter/source/materials/fail/composite/hm_read_fail_composite.F90
!||    hm_read_fail_lemaitre_mod    ../starter/source/materials/fail/lemaitre/hm_read_fail_lemaitre.F90
!||    hm_read_fail_spalling_mod    ../starter/source/materials/fail/spalling/hm_read_fail_spalling.F90
!||    hm_read_fractal_dmg_mod      ../starter/source/materials/fail/fractal/hm_read_fractal_dmg.F90
!||    message_mod                  ../starter/share/message_module/message_mod.F
!||    submodel_mod                 ../starter/share/modules1/submodel_mod.F
!||    table_mod                    ../starter/share/modules1/table_mod.F
!||====================================================================
      SUBROUTINE HM_READ_FAIL(MAT_PARAM,NUMMAT ,MAXFAIL ,FAIL_TAG ,
     .                        NTABLE   ,TABLE  ,FAILWAVE,NLOC_DMG ,
     .                        UNITAB  ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD
      USE ELBUFTAG_MOD            
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE FAILWAVE_MOD
      USE NLOCAL_REG_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE TABLE_MOD
      USE HM_READ_FAIL_SPALLING_MOD
      USE HM_READ_FRACTAL_DMG_MOD
      USE HM_READ_FAIL_LEMAITRE_MOD
      USE HM_READ_FAIL_COMPOSITE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARLINE, NCHARKEY
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com_xfem1.inc"
#include      "userlib.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NUMMAT
      INTEGER ,INTENT(IN) :: NTABLE
      INTEGER ,INTENT(IN) :: MAXFAIL
      TYPE (FAIL_TAG_), DIMENSION(0:MAXFAIL ) :: FAIL_TAG
      TYPE (FAILWAVE_STR_) :: FAILWAVE 
      TYPE (NLOCAL_STR_)   :: NLOC_DMG 
      TYPE (TTABLE)  ,INTENT(IN) :: TABLE(NTABLE)    
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(INOUT) :: MAT_PARAM
      TYPE (SUBMODEL_DATA),INTENT(IN) ::LSUBMODEL(*)
      TYPE (UNIT_TYPE_),INTENT(IN) :: UNITAB 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,IMAT,IFAIL,IFL,ILAW,MAT_ID,FAIL_ID,
     .   ILOC,IRUPT,IFAILWAVE,UNIT_ID,IXFEM,HM_NBFAIL,IUNIT,IFLAGUNIT
      INTEGER ,DIMENSION(:) ,ALLOCATABLE :: FAIL_MAT_NUM
      CHARACTER MESS*40
      CHARACTER(LEN = NCHARKEY) :: KEY
      CHARACTER(LEN=NCHARTITLE) :: TITR
C----------------------------------      
      my_real UNITAB_SUB(4)
      CHARACTER(LEN=NCHARLINE) :: IUSER_KEY
C----------------------------------      
      LOGICAL :: IS_AVAILABLE,IS_ID_LINE
C----------------------------------      
      DATA MESS /'FAILURE MODELS DEFINITION '/
C======================================================================- 
      IS_AVAILABLE = .FALSE.
      IS_ID_LINE   = .FALSE.
c
c     default allocation sizes of internal variables in element buffer
c
      DO I = 0,MAXFAIL   !  common to all failure models of the same type (IRUPT) 
        FAIL_TAG(I)%LF_DAM    = 0
        FAIL_TAG(I)%LF_DAMMX  = 1
        FAIL_TAG(I)%LF_DAMINI = 0
        FAIL_TAG(I)%LF_TDEL   = 1
        FAIL_TAG(I)%LF_INDX   = 0
        FAIL_TAG(I)%LF_OFF    = 1
        FAIL_TAG(I)%LF_DIR    = 0
      ENDDO
c      
      ICRACK3D = 0
c--------------------------------------------------
c     COUNT FAILURE MODELS USING CFG FILES
c--------------------------------------------------
c      
      CALL HM_OPTION_COUNT('FAILURE',HM_NBFAIL)
c      
C--------------------------------------------------
C     EXTRACT DATAS OF /FAIL/... LINE
C--------------------------------------------------
      ALLOCATE (FAIL_MAT_NUM(HM_NBFAIL))
      FAIL_MAT_NUM(:) = 0
c
      CALL HM_OPTION_START('FAILURE')      
c
      DO IFAIL = 1,HM_NBFAIL
c
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID = FAIL_ID, ! read in optional line if added
     .                          UNIT_ID   = UNIT_ID    ,
     .                          KEYWORD2  = KEY    )
c
        CALL HM_GET_INTV('mat_id' ,MAT_ID ,IS_AVAILABLE,LSUBMODEL)
c
c       ! number of failure models par material law
        DO IMAT = 1,NUMMAT-1
          IF (MAT_PARAM(IMAT)%MAT_ID == MAT_ID) THEN
            MAT_PARAM(IMAT)%NFAIL = MAT_PARAM(IMAT)%NFAIL + 1
            FAIL_MAT_NUM(IFAIL) = IMAT
            EXIT
          ENDIF
        END DO        
      END DO  ! IFAIL
c-------------------------------------------------
c     Allocate and initialize failure structure for each material
c-------------------------------------------------
c
      CALL FAIL_INIT(MAT_PARAM ,NUMMAT)
c 
      MAT_PARAM(1:NUMMAT)%NFAIL = 0
c-------------------------------------------------
c     Read failure models and add to material data structure
c-------------------------------------------------
      CALL HM_OPTION_START('FAILURE')      
c
      DO IFAIL = 1,HM_NBFAIL
        IMAT = FAIL_MAT_NUM(IFAIL)
c
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID = FAIL_ID, ! read in optional line if added
     .                            UNIT_ID   = UNIT_ID    ,
     .                            KEYWORD2  = KEY    )
        CALL HM_GET_INTV('mat_id' ,MAT_ID ,IS_AVAILABLE,LSUBMODEL)
c
        IF (IMAT == 0) THEN     ! MAT_ID not found for this failure model
          CALL ANCMSG(MSGID=3050,ANMODE=ANINFO,MSGTYPE=MSGWARNING,
     .                 C1= KEY   ,
     .                 I1= FAIL_ID,
     .                 I2= MAT_ID)
c
        ELSE IF (IMAT > 0) THEN      
c
          ! CHECK IF Unit_ID exists
          IUNIT = 1
          IF (UNIT_ID > 0) THEN
            IFLAGUNIT = 0
            DO IUNIT=1,UNITAB%NUNITS
              IF (UNITAB%UNIT_ID(IUNIT) == UNIT_ID) THEN
                IFLAGUNIT = 1
                UNIT_ID = IUNIT
                EXIT
              ENDIF
            ENDDO
            IF (IFLAGUNIT == 0) THEN
              CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                    I1= MAT_ID,
     .                    I2= UNIT_ID,
     .                    C1='MATERIAL',
     .                    C2='FAILURE MODEL',
     .                    C3=' ')
            ENDIF
          ENDIF
c
          CALL HM_GET_INTV('mat_id' ,MAT_ID ,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_BOOLV ('ID_CARD_EXIST',IS_ID_LINE,IS_AVAILABLE)
          IF ( .NOT.  (IS_ID_LINE)) FAIL_ID = 0
c
          IXFEM     = 0
          ILOC      = 0
          IFAILWAVE = 0
c
          ! Printing out material ID, failure criterion type and ID
          WRITE(IOUT,1000) MAT_ID,ADJUSTR(KEY(1:10)),FAIL_ID
c
          IF (MAT_PARAM(IMAT)%MAT_ID == MAT_ID) THEN
            MAT_PARAM(IMAT)%NFAIL = MAT_PARAM(IMAT)%NFAIL + 1
            IFL   = MAT_PARAM(IMAT)%NFAIL
            ILAW  = MAT_PARAM(IMAT)%ILAW
c-------  ----------------------------------------------------------------
c
            IF (KEY(1:5) == 'JOHNS') THEN
              IRUPT = 1
              CALL HM_READ_FAIL_JOHNSON(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,ILAW   ,IRUPT   ,IXFEM    ,
     .             MAT_PARAM(IMAT)%TITLE ,LSUBMODEL ,UNITAB )
c---
c     
            ELSEIF (KEY(1:5) == 'TBUTC') THEN 
              IRUPT = 2    
              CALL HM_READ_FAIL_TBUTCHER(MAT_PARAM(IMAT)%FAIL(IFL), 
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,IXFEM    ,
     .             LSUBMODEL,UNITAB   )
c     
            ELSEIF (KEY(1:5) == 'WILKI') THEN
              IRUPT = 3             
              CALL HM_READ_FAIL_WILKINS(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB   )
c     
            ELSEIF (KEY(1:5) == 'USER1') THEN
              IRUPT = 4    
              UNITAB_SUB(1) = UNITAB%UNIT_ID(IUNIT)
              UNITAB_SUB(2) = UNITAB%FAC_M(IUNIT)
              UNITAB_SUB(3) = UNITAB%FAC_L(IUNIT)
              UNITAB_SUB(4) = UNITAB%FAC_T(IUNIT)
              IUSER_KEY = REPEAT(' ', NCHARLINE)
              IUSER_KEY(1:LEN_TRIM(KEY)) = KEY(1:LEN_TRIM(KEY))
!
              CALL HM_READ_FAIL_USER(MAT_PARAM(IMAT)%FAIL(IFL), 
     .             IRUPT,IUSER_KEY,USERL_AVAIL,
     .             LSUBMODEL,UNITAB_SUB,MAT_ID)
c     
            ELSEIF (KEY(1:5) == 'USER2') THEN
              IRUPT = 5
              UNITAB_SUB(1) = UNITAB%UNIT_ID(IUNIT)
              UNITAB_SUB(2) = UNITAB%FAC_M(IUNIT)
              UNITAB_SUB(3) = UNITAB%FAC_L(IUNIT)
              UNITAB_SUB(4) = UNITAB%FAC_T(IUNIT)
              IUSER_KEY = REPEAT(' ', NCHARLINE)
              IUSER_KEY(1:LEN_TRIM(KEY)) = KEY(1:LEN_TRIM(KEY))
!
              CALL HM_READ_FAIL_USER(MAT_PARAM(IMAT)%FAIL(IFL), 
     .             IRUPT,IUSER_KEY,USERL_AVAIL,
     .             LSUBMODEL,UNITAB_SUB,MAT_ID)
c     
            ELSEIF (KEY(1:5) == 'USER3') THEN
              IRUPT = 6               
              UNITAB_SUB(1) = UNITAB%UNIT_ID(IUNIT)
              UNITAB_SUB(2) = UNITAB%FAC_M(IUNIT)
              UNITAB_SUB(3) = UNITAB%FAC_L(IUNIT)
              UNITAB_SUB(4) = UNITAB%FAC_T(IUNIT)
              IUSER_KEY = REPEAT(' ', NCHARLINE)
              IUSER_KEY(1:LEN_TRIM(KEY)) = KEY(1:LEN_TRIM(KEY))
!
              CALL HM_READ_FAIL_USER(MAT_PARAM(IMAT)%FAIL(IFL), 
     .             IRUPT,IUSER_KEY,USERL_AVAIL,
     .             LSUBMODEL,UNITAB_SUB,MAT_ID)
c     
            ELSEIF (KEY(1:3) == 'FLD') THEN
              IRUPT = 7    
              CALL HM_READ_FAIL_FLD(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,IXFEM    ,
     .             LSUBMODEL,UNITAB   ,FAIL_TAG(IRUPT))
c     
            ELSEIF (KEY(1:5) == 'SPALL') THEN
              IRUPT = 8    
              CALL HM_READ_FAIL_SPALLING(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,
     .             LSUBMODEL,UNITAB   ,FAIL_TAG(IRUPT),IOUT       )
c     
            ELSEIF (KEY(1:6) == 'WIERZB') THEN
              IRUPT = 9 
              CALL HM_READ_FAIL_WIERZBICKI(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB   )
c     
            ELSEIF (KEY(1:8) == 'TENSSTRA') THEN
              IRUPT = 10
              CALL HM_READ_FAIL_TENSSTRAIN(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,
     .             LSUBMODEL,UNITAB   ,UNIT_ID  )
c     
            ELSEIF (KEY(1:6) == 'ENERGY') THEN
              IRUPT = 11
              CALL HM_READ_FAIL_ENERGY(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB)
c
            ELSEIF (KEY(1:11) == 'FRACTAL_DMG') THEN 
              IRUPT = 12                    
              CALL HM_READ_FRACTAL_DMG(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB   ,IOUT    )
              FAIL_TAG(IRUPT)%LF_DAM    = 1
              FAIL_TAG(IRUPT)%LF_DAMMX  = 1
c     
            ELSEIF (KEY(1:5) == 'CHANG') THEN
              IRUPT = 13
              CALL HM_READ_FAIL_CHANG(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,
     .             LSUBMODEL,UNITAB   ,FAIL_TAG(IRUPT))
c     
            ELSEIF (KEY(1:6) == 'HASHIN') THEN
              IRUPT = 14
              CALL HM_READ_FAIL_HASHIN(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,IFAILWAVE,
     .             LSUBMODEL,UNITAB   ,FAIL_TAG(IRUPT))
c     
            !!  ELSEIF (KEY(1:6) == '') THEN   
            !!     IRUPT = 15                      ! not used
c     
            ELSEIF (KEY(1:4) == 'PUCK') THEN
                 IRUPT = 16 
              CALL HM_READ_FAIL_PUCK(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,
     .             LSUBMODEL,UNITAB   ,FAIL_TAG(IRUPT))
c     
            !! ELSEIF (KEY(1:10) == '') THEN
            !!       IRUPT = 17                     ! not used
c     
            ELSEIF (KEY(1:8) == 'LAD_DAMA') THEN
              IRUPT = 18
              CALL HM_READ_FAIL_LADEVEZE(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,
     .             LSUBMODEL,UNITAB)  
c     
            !!   ELSEIF (KEY(1:8) == ' ') THEN   ! not used
            !!     IRUPT = 19
c     
            ELSEIF (KEY(1:7) == 'CONNECT') THEN
              IRUPT = 20
              CALL HM_READ_FAIL_CONNECT(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,UNIT_ID ,
     .             LSUBMODEL,UNITAB   ,FAIL_TAG(IRUPT))
c     
            !! ELSEIF (KEY(1:10) == ' ') THEN 
            !!       IRUPT = 21                     ! not used
c     
            !! ELSEIF (KEY(1:8) == ' ') THEN 
            !!       IRUPT = 22                     ! not used 
c     
            ELSEIF (KEY(1:4) == 'TAB1') THEN
              IRUPT = 23 
              CALL HM_READ_FAIL_TAB1(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,IXFEM    ,
     .             LSUBMODEL,UNITAB   )
c     
            ELSEIF (KEY(1:4) == 'TAB2') THEN
              IRUPT = 41
              CALL HM_READ_FAIL_TAB2(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,MAT_PARAM(IMAT)%TITLE,
     .             LSUBMODEL,UNITAB   )
c     
            ELSEIF (KEY(1:3) == 'TAB') THEN
              IRUPT = 37
              CALL HM_READ_FAIL_TAB_OLD(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,IXFEM    ,
     .             LSUBMODEL,UNITAB   )
c     
            ELSEIF (KEY(1:10) == 'ORTHSTRAIN') THEN
              IRUPT = 24 
              CALL HM_READ_FAIL_ORTHSTRAIN(MAT_PARAM(IMAT)%FAIL(IFL),
     .             FAIL_ID   ,IRUPT    ,LSUBMODEL,UNITAB   ,   
     .             FAIL_TAG(IRUPT))
c     
            ELSEIF (KEY(1:3) == 'NXT') THEN
              IRUPT = 25   
              CALL HM_READ_FAIL_NXT(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,
     .             LSUBMODEL,UNITAB   ,FAIL_TAG(IRUPT))
c     
            ELSEIF (KEY(1:9) == 'SNCONNECT') THEN
              IRUPT = 26
              CALL HM_READ_FAIL_SNCONNECT(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB   )
              FAIL_TAG(IRUPT)%LF_DAM = 3
c     
            ELSEIF (KEY(1:3) == 'EMC') THEN
              IRUPT = 27 
              CALL HM_READ_FAIL_EMC(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB   )
c     
            ELSEIF (KEY(1:5) == 'ALTER') THEN
              IRUPT = 28
              CALL HM_READ_FAIL_ALTER(MAT_PARAM(IMAT)%FAIL(IFL)   ,FAIL_TAG  ,
     .             MAXFAIL  ,MAT_ID   ,FAIL_ID  ,IRUPT  ,IXFEM     ,
     .             IFAILWAVE,LSUBMODEL,UNITAB   )
c     
            ELSEIF (KEY(1:8) == 'SAHRAEI') THEN
              IRUPT = 29
              CALL HM_READ_FAIL_SAHRAEI(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB   )
c     
            ELSEIF (KEY(1:6) == 'BIQUAD') THEN
              IRUPT = 30
              CALL HM_READ_FAIL_BIQUAD(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,
     .             MAT_PARAM(IMAT)%TITLE ,LSUBMODEL,UNITAB   )
c     
            ELSEIF (KEY(1:6) == 'FABRIC') THEN
              IRUPT = 31
              CALL HM_READ_FAIL_FABRIC(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,UNIT_ID ,
     .             LSUBMODEL,UNITAB   )
c     
            ELSEIF (KEY(1:7) == 'HC_DSSE') THEN
              IRUPT = 32
              CALL HM_READ_FAIL_HC_DSSE(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB   )
              FAIL_TAG(IRUPT)%LF_DAM  = 1    ! damag
              FAIL_TAG(IRUPT)%LF_INDX = 1    ! HC_DSc     
            ELSEIF (KEY(1:10) == 'MULLINS_OR') THEN
              IRUPT = 33
              CALL HM_READ_MULLINS_OR(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB   )
c     
            ELSEIF (KEY(1:9) == 'COCKCROFT') THEN
              IRUPT = 34
              CALL HM_READ_FAIL_COCKCROFT(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB   )
c     
            ELSEIF (KEY(1:6) == 'GURSON') THEN
              IRUPT = 35
              CALL HM_READ_FAIL_GURSON(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,ILOC   ,
     .             MAT_PARAM(IMAT)%TITLE ,LSUBMODEL,UNITAB   )
c     
            ELSEIF (KEY(1:6) == 'VISUAL') THEN
              IRUPT = 36
              CALL HM_READ_FAIL_VISUAL(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB   )
c     
            ELSEIF (KEY(1:10) == 'ORTHBIQUAD') THEN
              IRUPT = 38
              CALL HM_READ_FAIL_ORTHBIQUAD(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,
     .             MAT_PARAM(IMAT)%TITLE ,LSUBMODEL,UNITAB   )
c     
            ELSEIF (KEY(1:5) == 'GENE1') THEN
              IRUPT = 39
              CALL HM_READ_FAIL_GENE1(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,
     .             MAT_PARAM(IMAT)%TITLE ,LSUBMODEL,UNITAB,    
     .             FAIL_TAG(IRUPT))
c     
            ELSEIF (KEY(1:4) == 'RTCL') THEN
              IRUPT = 40
              CALL HM_READ_FAIL_RTCL(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB   )
c     
            ELSEIF (KEY(1:6) == 'INIEVO') THEN
              IRUPT = 42
              CALL HM_READ_FAIL_INIEVO(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,
     .             MAT_PARAM(IMAT)%TITLE ,LSUBMODEL,UNITAB   )
              FAIL_TAG(IRUPT)%LF_DAMINI = 1
c
            ELSEIF (KEY(1:7) == 'SYAZWAN') THEN
              IRUPT = 43
              CALL HM_READ_FAIL_SYAZWAN(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,
     .             MAT_PARAM(IMAT)%TITLE ,LSUBMODEL,UNITAB   )
c
            ELSEIF (KEY(1:6) == 'TSAIWU') THEN
              IRUPT = 44
              CALL HM_READ_FAIL_TSAIWU(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB,
     .             FAIL_TAG(IRUPT))
c
            ELSEIF (KEY(1:8) == 'TSAIHILL') THEN
              IRUPT = 45
              CALL HM_READ_FAIL_TSAIHILL(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB ,
     .             FAIL_TAG(IRUPT))
c
            ELSEIF (KEY(1:7) == 'HOFFMAN') THEN
              IRUPT = 46
              CALL HM_READ_FAIL_HOFFMAN(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB,
     .             FAIL_TAG(IRUPT))
c
            ELSEIF (KEY(1:9) == 'MAXSTRAIN') THEN
              IRUPT = 47
              CALL HM_READ_FAIL_MAXSTRAIN(MAT_PARAM(IMAT)%FAIL(IFL),
     .             MAT_ID   ,FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB  ,
     .             FAIL_TAG(IRUPT))
c
            ELSEIF(KEY(1:9) == 'ORTHENERG')THEN
              IRUPT = 48
              CALL HM_READ_FAIL_ORTHENERG(MAT_PARAM(IMAT)%FAIL(IFL),
     .             FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB   )
c
            ELSEIF (KEY(1:8) == 'LEMAITRE') THEN
              IRUPT = 50
              CALL HM_READ_FAIL_LEMAITRE(MAT_PARAM(IMAT)%FAIL(IFL),
     .             FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB ,IOUT     )
c
            ELSEIF (KEY(1:9) == 'COMPOSITE') THEN
              IRUPT = 51
              CALL HM_READ_FAIL_COMPOSITE(MAT_PARAM(IMAT)%FAIL(IFL),
     .             FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB ,FAIL_TAG(IRUPT),
     .             IOUT)
            ENDIF
c-------  --
c                   
            NLOC_DMG%IMOD     = MAX(NLOC_DMG%IMOD, ILOC)
            FAILWAVE%WAVE_MOD = MAX(FAILWAVE%WAVE_MOD,IFAILWAVE)
      
            MAT_PARAM(IMAT)%IFAILWAVE = IFAILWAVE
            MAT_PARAM(IMAT)%NLOC      = ILOC
            MAT_PARAM(IMAT)%IXFEM     = IXFEM
            IF (IXFEM == 1) ICRACK3D  = 1
c
          ENDIF ! MAT_ID
        ENDIF   ! IMAT > 0
c
      END DO  ! IFAIL        
c------------------------------
      DEALLOCATE (FAIL_MAT_NUM)
C-----------       
      RETURN
C-----------       
 1000 FORMAT(//
     & 5X,'MAT_ID . . . . . . . . . . . . . . .=',I10/
     & 5X,'FAILURE MODEL TYPE . . . . . . . . .=',A/
     & 5X,'FAIL_ID. . . . . . . . . . . . . . .=',I10/)
C-----------  
      END
