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
Chd/        HM_READ_FAIL_TBUTCHER         source/materials/fail/tuler_bucher/hm_read_fail_tbutcher.F
Chd|====================================================================
Chd|  HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- called by -----------
Chd|        READ_MATERIAL_MODELS          source/materials/read_material_models.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_GET_BOOLV                  source/devtools/hm_reader/hm_get_boolv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_READ_FAIL_ALTER            source/materials/fail/windshield_alter/hm_read_fail_alter.F
Chd|        HM_READ_FAIL_BIQUAD           source/materials/fail/biquad/hm_read_fail_biquad.F
Chd|        HM_READ_FAIL_CHANG            source/materials/fail/changchang/hm_read_fail_chang.F
Chd|        HM_READ_FAIL_COCKCROFT        source/materials/fail/cockroft_latham/hm_read_fail_cockcroft.F
Chd|        HM_READ_FAIL_CONNECT          source/materials/fail/connect/hm_read_fail_connect.F
Chd|        HM_READ_FAIL_EMC              source/materials/fail/emc/hm_read_fail_emc.F
Chd|        HM_READ_FAIL_ENERGY           source/materials/fail/energy/hm_read_fail_energy.F
Chd|        HM_READ_FAIL_FABRIC           source/materials/fail/fabric/hm_read_fail_fabric.F
Chd|        HM_READ_FAIL_FLD              source/materials/fail/fld/hm_read_fail_fld.F
Chd|        HM_READ_FAIL_GENE1            source/materials/fail/gene1/hm_read_fail_gene1.F
Chd|        HM_READ_FAIL_GURSON           source/materials/fail/gurson/hm_read_fail_gurson.F
Chd|        HM_READ_FAIL_HASHIN           source/materials/fail/hashin/hm_read_fail_hashin.F
Chd|        HM_READ_FAIL_HC_DSSE          source/materials/fail/hc_dsse/hm_read_fail_hc_dsse.F
Chd|        HM_READ_FAIL_JOHNSON          source/materials/fail/johnson_cook/hm_read_fail_johnson.F
Chd|        HM_READ_FAIL_LADEVEZE         source/materials/fail/ladeveze/hm_read_fail_ladeveze.F
Chd|        HM_READ_FAIL_NXT              source/materials/fail/nxt/hm_read_fail_nxt.F
Chd|        HM_READ_FAIL_ORTHBIQUAD       source/materials/fail/orthbiquad/hm_read_fail_orthbiquad.F
Chd|        HM_READ_FAIL_ORTHSTRAIN       source/materials/fail/orthstrain/hm_read_fail_orthstrain.F
Chd|        HM_READ_FAIL_PUCK             source/materials/fail/puck/hm_read_fail_puck.F
Chd|        HM_READ_FAIL_RTCL             source/materials/fail/rtcl/hm_read_fail_rtcl.F
Chd|        HM_READ_FAIL_SAHRAEI          source/materials/fail/sahraei/hm_read_fail_sahraei.F
Chd|        HM_READ_FAIL_SNCONNECT        source/materials/fail/snconnect/hm_read_fail_snconnect.F
Chd|        HM_READ_FAIL_SPALLING         source/materials/fail/spalling/hm_read_fail_spalling.F
Chd|        HM_READ_FAIL_TAB1             source/materials/fail/tabulated/hm_read_fail_tab1.F
Chd|        HM_READ_FAIL_TAB2             source/materials/fail/tabulated/hm_read_fail_tab2.F
Chd|        HM_READ_FAIL_TAB_OLD          source/materials/fail/tabulated/hm_read_fail_tab_old.F
Chd|        HM_READ_FAIL_TBUTCHER         source/materials/fail/tuler_butcher/hm_read_fail_tbutcher.F
Chd|        HM_READ_FAIL_TENSSTRAIN       source/materials/fail/tensstrain/hm_read_fail_tensstrain.F
Chd|        HM_READ_FAIL_USER             source/materials/fail/failuser/hm_read_fail_user.F
Chd|        HM_READ_FAIL_VISUAL           source/materials/fail/visual/hm_read_fail_visual.F
Chd|        HM_READ_FAIL_WIERZBICKI       source/materials/fail/wierzbicki/hm_read_fail_wierzbicki.F
Chd|        HM_READ_FAIL_WILKINS          source/materials/fail/wilkins/hm_read_fail_wilkins.F
Chd|        HM_READ_MULLINS_OR            source/materials/fail/mullins_or/hm_read_fail_mullins_or.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL(FAIL_TAG,FAILWAVE,NLOC_DMG,IPM     ,PM       ,
     .                        BUFMAT  ,BUFLEN  ,IADBUF  ,UNITAB  ,LSUBMODEL,
     .                        NRUPT   ,NVAR_FAIL,TABLE  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD
      USE FAILWAVE_MOD
      USE NLOCAL_REG_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE TABLE_MOD
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "scr19_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
#include      "tablen_c.inc"
#include      "com_xfem1.inc"
#include      "userlib.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER  :: BUFLEN,IADBUF
      INTEGER ,DIMENSION(NPROPMI,NUMMAT), INTENT(INOUT) :: IPM
      my_real ,DIMENSION(NPROPM ,NUMMAT), INTENT(INOUT) :: PM
      my_real ,DIMENSION(*)             , INTENT(INOUT) :: BUFMAT

      TYPE(FAIL_TAG_), DIMENSION(0:MAXFAIL ) :: FAIL_TAG
      TYPE(FAILWAVE_STR_) :: FAILWAVE 
      TYPE(NLOCAL_STR_)   :: NLOC_DMG 
      TYPE(SUBMODEL_DATA),INTENT(IN) ::LSUBMODEL(*)
      INTEGER, DIMENSION(MAXLAW,NRUPT),INTENT(INOUT)    :: NVAR_FAIL 
      INTEGER,INTENT(IN)                                :: NRUPT 
      TYPE(TTABLE) TABLE(NTABLE)    
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,NF,NFUNC,NUVAR,MAXPARAM,IMID,MAT_ID,FAIL_ID,ILAW,
     .   NUVARG,ILOC,IRUP,IFAILWAVE,UNIT_ID,IXFEM,HM_NBFAIL,IUNIT,IFLAGUNIT
      CHARACTER KEY*20,MESS*40,TITR*nchartitle
C----------------------------------      
      PARAMETER (MAXPARAM = 1000) 
      INTEGER ,DIMENSION(MAXFUNC)       :: IFUNC
      my_real ,DIMENSION(MAXPARAM)      :: UPARAM
      my_real UNITAB_SUB(4)
      CHARACTER IUSER_KEY*ncharline
C----------------------------------      
      LOGICAL    :: IS_AVAILABLE,IS_ID_LINE
C----------------------------------      
      DATA MESS /'FAILURE MODELS DEFINITION '/
C======================================================================- 

      IS_AVAILABLE = .FALSE.
      IS_ID_LINE   = .FALSE.
C      
      DO I=0,MAXFAIL
        FAIL_TAG(I)%NUVAR    = 0
        FAIL_TAG(I)%LF_DAM   = 0
        FAIL_TAG(I)%LF_DAMMX = 1
        FAIL_TAG(I)%LF_TDEL  = 1
        FAIL_TAG(I)%LF_INDX  = 0
        FAIL_TAG(I)%LF_OFF   = 1
        FAIL_TAG(I)%LF_DIR   = 0
      ENDDO
c
      IPM(220, 1:NUMMAT-1) = 0
      IPM(236, 1:NUMMAT-1) = 0
      NVAR_FAIL(:,:)= 0
      IFUNC(:)  = 0
C      
      ILOC      = 0
      NFUNC     = 0
      NUVAR     = 0
      ICRACK3D  = 0
      IRUP      = 0
      NUVARG    = 0
c
c--------------------------------------------------
c     COUNT FAILURE MODELS USING CFG FILES
c--------------------------------------------------
c      
      CALL HM_OPTION_COUNT('FAILURE',HM_NBFAIL)
c
c--------------------------------------------------
c START BROWSING MODEL MATERIALS
c--------------------------------------------------
c
      CALL HM_OPTION_START('FAILURE')
      
c
C--------------------------------------------------
C EXTRACT DATAS OF /FAIL/... LINE
C--------------------------------------------------
c
      DO NF = 1,HM_NBFAIL
c
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID = FAIL_ID, ! read in optional line if added
     .                          UNIT_ID   = UNIT_ID    ,
     .                          KEYWORD2  = KEY    )
c------------------
c       GET FAIL_ID 
c------------------ 
c 
        CALL HM_GET_BOOLV ('ID_CARD_EXIST',IS_ID_LINE,IS_AVAILABLE)
        IF ( .NOT.  (IS_ID_LINE)) FAIL_ID = 0
        CALL HM_GET_INTV('mat_id' ,MAT_ID ,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C       CHECK IF Unit_ID exists
C--------------------------------------------------
        IUNIT = 1
        IF (UNIT_ID > 0) THEN
        IFLAGUNIT = 0
        DO IUNIT=1,NUNITS
            IF (UNITAB%UNIT_ID(IUNIT) == UNIT_ID) THEN
            IFLAGUNIT = 1
              UNIT_ID = IUNIT
            EXIT
          ENDIF
        ENDDO
          IF (IFLAGUNIT == 0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1= MAT_ID,
     .                  I2= UNIT_ID,
     .                  C1='MATERIAL',
     .                  C2='FAILURE MODEL',
     .                  C3=' ')
        ENDIF
        ENDIF
c 
C--------------------------------------------------
C          CHECK FAILURE MODEL
C--------------------------------------------------
c--------------------------------------------                 
            !SELECT CASE (KEY(1:LEN_TRIM(KEY)))
c--------------------------------------------      
        IF(KEY(1:5) == 'JOHNS')THEN
              IRUP=1       
c--------------------------------------------      
        ELSEIF(KEY(1:5) == 'TBUTC')THEN 
             IRUP=2    
c--------------------------------------------      
        ELSEIF(KEY(1:5) == 'WILKI')THEN
             IRUP=3             
c--------------------------------------------      
        ELSEIF(KEY(1:5) == 'USER1')THEN
             IRUP=4    
c--------------------------------------------      
        ELSEIF(KEY(1:5) == 'USER2')THEN
             IRUP=5
c--------------------------------------------      
        ELSEIF(KEY(1:5) == 'USER3')THEN
             IRUP=6               
c--------------------------------------------      
        ELSEIF(KEY(1:3) == 'FLD')THEN
             IRUP=7    
c--------------------------------------------      
        ELSEIF(KEY(1:5) == 'SPALL')THEN
             IRUP=8    
c--------------------------------------------      
        ELSEIF(KEY(1:6) == 'WIERZB')THEN
             IRUP=9 
c--------------------------------------------      
        ELSEIF(KEY(1:8) == 'TENSSTRA')THEN
             IRUP= 10
c--------------------------------------------      
        ELSEIF(KEY(1:6) == 'ENERGY')THEN
             IRUP= 11
c--------------------------------------------      
        ELSEIF(KEY(1:5) == 'CHANG')THEN
             IRUP= 13
c--------------------------------------------      
        ELSEIF(KEY(1:6) == 'HASHIN')THEN
             IRUP = 14
c--------------------------------------------      
        ELSEIF(KEY(1:6) == 'YAMADA')THEN
             IRUP = 15 
c--------------------------------------------      
        ELSEIF(KEY(1:4) == 'PUCK') THEN
             IRUP = 16 
c--------------------------------------------      
        ELSEIF(KEY(1:10) == 'XFEM_TBUTC')THEN
              IRUP = 17
              ! with new reader XFEM_TBUTC is changed to TBUTC in object file
              !=>IRUP = 2
c--------------------------------------------      
        ELSEIF(KEY(1:8) == 'LAD_DAMA')THEN
              IRUP = 18
c--------------------------------------------      
        ELSEIF(KEY(1:8) == 'POWER_DA')THEN
             IRUP = 19
c--------------------------------------------      
        ELSEIF(KEY(1:7) == 'CONNECT')THEN
              IRUP = 20
c--------------------------------------------      
        ELSEIF(KEY(1:10) == 'XFEM_JOHNS')THEN
              IRUP = 21
              ! with new reader XFEM_JOHNS is changed to JOHNS in object file
              !=>IRUP = 1
c--------------------------------------------      
        ELSEIF(KEY(1:8) == 'XFEM_FLD')THEN
              IRUP = 22 
              ! with new reader XFEM_FLD is changed to FLD in object file
              !=>IRUP = 7
c--------------------------------------------      
        ELSEIF(KEY(1:4) == 'TAB1')THEN
              IRUP = 23 
c--------------------------------------------      
        ELSEIF(KEY(1:10) == 'ORTHSTRAIN')THEN
             IRUP= 24 
c--------------------------------------------      
        ELSEIF(KEY(1:3) == 'NXT')THEN ! Failure criteria based on NXT 
             IRUP= 25   
c--------------------------------------------      
        ELSEIF(KEY(1:9) == 'SNCONNECT')THEN
              IRUP = 26
c--------------------------------------------      
        ELSEIF(KEY(1:3) == 'EMC')THEN
             IRUP= 27 
c--------------------------------------------      
        ELSEIF(KEY(1:5) == 'ALTER')THEN
             IRUP= 28
c--------------------------------------------      
        ELSEIF(KEY(1:8) == 'SAHRAEI')THEN
             IRUP= 29
c--------------------------------------------      
        ELSEIF(KEY(1:6) == 'BIQUAD')THEN
             IRUP= 30
c--------------------------------------------      
        ELSEIF(KEY(1:6) == 'FABRIC')THEN
             IRUP= 31
c--------------------------------------------      
        ELSEIF(KEY(1:7) == 'HC_DSSE')THEN
             IRUP= 32
c--------------------------------------------      
        ELSEIF(KEY(1:10) == 'MULLINS_OR')THEN
             IRUP= 33
c--------------------------------------------      
        ELSEIF(KEY(1:9) == 'COCKCROFT')THEN
             IRUP= 34
c--------------------------------------------      
        ELSEIF(KEY(1:6) == 'GURSON')THEN
             IRUP= 35
c--------------------------------------------      
        ELSEIF(KEY(1:6) == 'VISUAL')THEN
             IRUP= 36
c--------------------------------------------      
        ELSEIF(KEY(1:4) == 'TAB2')THEN
             IRUP= 41
c--------------------------------------------      
        ELSEIF(KEY(1:3) == 'TAB')THEN
             IRUP= 37
c--------------------------------------------      
        ELSEIF(KEY(1:10) == 'ORTHBIQUAD')THEN
             IRUP= 38
c--------------------------------------------      
        ELSEIF(KEY(1:5) == 'GENE1')THEN
             IRUP= 39
c--------------------------------------------      
        ELSEIF(KEY(1:4) == 'RTCL')THEN
             IRUP= 40
c--------------------------------------------      
        ELSE  
             IRUP= 0
        ENDIF
c--------------------------------------------      
c       CALL FAILURE MODEL READING ROUTINES FOR EACH MATERIAL LAW
c--------------------------------------------      
        UPARAM(:) = ZERO
        IFAILWAVE = 0
c----
        DO I=1,NUMMAT-1
          IMID  = IPM(1,I)
          IF (IMID == MAT_ID) THEN
            IXFEM = 0
            CALL FRETITL2(TITR,IPM(NPROPMI-LTITR+1,I),LTITR)
            WRITE(IOUT,1000) MAT_ID,IRUP,FAIL_ID
c--------------------------------------------      
            SELECT CASE (IRUP)
c--------------------------------------------      
            CASE(1)
              CALL HM_READ_FAIL_JOHNSON(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,UNITAB,MAT_ID    ,FAIL_ID  ,
     .             LSUBMODEL,IXFEM    )
c---
            CASE(2)
              CALL HM_READ_FAIL_TBUTCHER(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,UNITAB,MAT_ID    ,FAIL_ID  ,
     .             LSUBMODEL,IXFEM)
c---
            CASE(3)
              CALL HM_READ_FAIL_WILKINS(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,IXFEM          ,MAT_ID    ,FAIL_ID  ,
     .             LSUBMODEL,UNITAB)
c---
            CASE(4,5,6)
              UNITAB_SUB(1) = UNITAB%UNIT_ID(IUNIT)
              UNITAB_SUB(2) = UNITAB%FAC_M(IUNIT)
              UNITAB_SUB(3) = UNITAB%FAC_L(IUNIT)
              UNITAB_SUB(4) = UNITAB%FAC_T(IUNIT)
!
              IUSER_KEY = KEY(1:LEN_TRIM(KEY))
!
              CALL HM_READ_FAIL_USER(IRUP,IUSER_KEY,
     .                               USERL_AVAIL,
     .                               UPARAM,MAXPARAM,NUPARAM,
     .                               NUVAR,IFUNC,MAXFUNC,NFUNC,
     .                               LSUBMODEL,
     .                               UNITAB_SUB,
     .                               MAT_ID)
c---
            CASE(7)
              CALL HM_READ_FAIL_FLD(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,IXFEM          ,MAT_ID    ,FAIL_ID  ,
     .             LSUBMODEL,UNITAB)
                   FAIL_TAG(IRUP)%LF_DAM  = 1    ! damage factor for ANIM output
                   FAIL_TAG(IRUP)%LF_INDX = 1    ! FLD zone index for ANIM output
c---
            CASE(8)
              CALL HM_READ_FAIL_SPALLING(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,UNITAB         ,MAT_ID    ,FAIL_ID  ,
     .             LSUBMODEL)
c---
            CASE(9)
              CALL HM_READ_FAIL_WIERZBICKI(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,UNITAB         ,MAT_ID    ,FAIL_ID  ,
     .             LSUBMODEL)
c---
            CASE(10)
              CALL HM_READ_FAIL_TENSSTRAIN(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,UNITAB         ,MAT_ID    ,FAIL_ID  ,
     .             LSUBMODEL,UNIT_ID)
c---
            CASE(11)
              CALL HM_READ_FAIL_ENERGY(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,MAT_ID         ,FAIL_ID   ,
     .             LSUBMODEL,UNITAB)
c---
            CASE(13)
              CALL HM_READ_FAIL_CHANG(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,IXFEM          ,MAT_ID    ,FAIL_ID  ,
     .             LSUBMODEL,UNITAB)
c---
            CASE(14)
              CALL HM_READ_FAIL_HASHIN(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,UNITAB         ,MAT_ID    ,FAIL_ID  ,
     .             IFAILWAVE,LSUBMODEL)
c---
            CASE(16)
              CALL HM_READ_FAIL_PUCK(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,IXFEM          ,MAT_ID    ,FAIL_ID  ,
     .             LSUBMODEL,UNITAB)
c---
c---
            CASE(18)
              CALL HM_READ_FAIL_LADEVEZE(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,UNITAB,MAT_ID  ,FAIL_ID   ,
     .             LSUBMODEL)
c---
            CASE(20)
              CALL HM_READ_FAIL_CONNECT(
     .             UPARAM   ,MAXPARAM ,NUPARAM    ,NUVAR     ,IFUNC    ,    
     .             MAXFUNC  ,NFUNC    ,UNITAB     ,MAT_ID    ,FAIL_ID  ,    
     .             LSUBMODEL,UNIT_ID)
c---
            CASE(23)
              CALL HM_READ_FAIL_TAB1(
     .             UPARAM   ,MAXPARAM ,NUPARAM    ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,UNITAB     ,MAT_ID    ,FAIL_ID  ,
     .             LSUBMODEL,IXFEM    )
c---
            CASE(24)
              CALL HM_READ_FAIL_ORTHSTRAIN(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,UNITAB         ,FAIL_ID   ,LSUBMODEL)
c---
            CASE(25)
              CALL HM_READ_FAIL_NXT(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,MAT_ID         ,FAIL_ID   ,
     .             LSUBMODEL,UNITAB )
                   FAIL_TAG(IRUP)%LF_DAM = 1
c---
            CASE(26)
              CALL HM_READ_FAIL_SNCONNECT(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,UNITAB         ,MAT_ID    ,FAIL_ID  ,
     .             LSUBMODEL)
                   FAIL_TAG(IRUP)%LF_DAM = 3
c---
            CASE(27)
              CALL HM_READ_FAIL_EMC(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,MAT_ID         ,FAIL_ID   ,
     .             LSUBMODEL,UNITAB)
c---
            CASE(28)
              CALL HM_READ_FAIL_ALTER(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,NFUNC    ,
     .             FAIL_TAG(IRUP)  ,MAT_ID ,FAIL_ID   ,IXFEM     ,IFAILWAVE,
     .             LSUBMODEL,UNITAB )
C---
            CASE(29)
              CALL HM_READ_FAIL_SAHRAEI(
     .             UPARAM   ,MAXPARAM ,NUPARAM  ,NUVAR    ,NFUNC    ,IFUNC    ,
     .             MAXFUNC  ,FAIL_ID  ,MAT_ID   ,LSUBMODEL,UNITAB   ,TITR     ) 
c---
            CASE(30)
              CALL HM_READ_FAIL_BIQUAD(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,UNITAB         ,MAT_ID    ,FAIL_ID  ,
     .             LSUBMODEL)
c---
            CASE(31)
              CALL HM_READ_FAIL_FABRIC(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,UNITAB         ,MAT_ID    ,FAIL_ID  ,
     .             LSUBMODEL,UNIT_ID)
c---
            CASE(32)
              CALL HM_READ_FAIL_HC_DSSE(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,MAT_ID         ,FAIL_ID   ,
     .             LSUBMODEL,UNITAB )
c---
            CASE(33)
              CALL HM_READ_MULLINS_OR(
     .             UPARAM   ,MAXPARAM ,NUPARAM       ,NUVAR      ,MAT_ID    ,
     .             FAIL_ID  ,LSUBMODEL,UNITAB) 
C---
            CASE(34)
              CALL HM_READ_FAIL_COCKCROFT(
     .             UPARAM, MAXPARAM, NUPARAM, NUVAR, IFUNC, MAXFUNC, NFUNC, FAIL_ID, 
     .             LSUBMODEL, UNITAB)
C---
            CASE(35)
              CALL HM_READ_FAIL_GURSON(
     .             UPARAM   ,MAXPARAM ,NUPARAM  ,NUVAR    ,NFUNC    ,FAIL_ID  ,MAT_ID   ,
     .             LSUBMODEL,UNITAB   ,ILOC     ,TITR     )  
C---
            CASE(36)
              CALL HM_READ_FAIL_VISUAL(
     .             UPARAM   ,MAXPARAM ,NUPARAM, NUVAR, NFUNC , IFUNC, 
     .             MAXFUNC  ,LSUBMODEL, UNITAB  )
c---
           CASE(37)
              CALL HM_READ_FAIL_TAB_OLD(
     .             UPARAM   ,MAXPARAM ,NUPARAM    ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,UNITAB     ,MAT_ID    ,FAIL_ID  ,
     .             LSUBMODEL,IXFEM    )
c---
           CASE(38)
              CALL HM_READ_FAIL_ORTHBIQUAD(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,UNITAB         ,FAIL_ID   ,MAT_ID   ,
     .             TITR     ,LSUBMODEL)
c---
           CASE(39)
              CALL HM_READ_FAIL_GENE1(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,UNITAB         ,FAIL_ID   ,MAT_ID   ,
     .             TABLE    ,TITR     ,LSUBMODEL)
c---
           CASE(40)
              CALL HM_READ_FAIL_RTCL(
     .             UPARAM   ,MAXPARAM ,NUPARAM  ,NUVAR    ,UNITAB   ,
     .             FAIL_ID  ,MAT_ID   ,TITR     ,LSUBMODEL)
c---
           CASE(41)
              CALL HM_READ_FAIL_TAB2(
     .             UPARAM   ,MAXPARAM ,NUPARAM        ,NUVAR     ,IFUNC    ,
     .             MAXFUNC  ,NFUNC    ,UNITAB         ,FAIL_ID   ,MAT_ID   ,
     .             TABLE    ,TITR     ,LSUBMODEL)
c
c--------------------------------------------      
            END SELECT
c--------------------------------------------      
c
c           stockage des informations pour le modele de rupture
c--------------------------------------------      
            FAIL_TAG(IRUP)%NUVAR = NUVAR
            IPM(253,I)    = ILOC 
            IPM(254,I)    = IFAILWAVE 
            NLOC_DMG%IMOD = MAX(NLOC_DMG%IMOD, ILOC)
            FAILWAVE%WAVE_MOD = MAX(FAILWAVE%WAVE_MOD,IFAILWAVE)
            N = IPM(220, I)
            IPM(111 + 15*N  ,I) = IRUP         
            IPM(112 + 15*N  ,I) = NUPARAM
            IPM(113 + 15*N  ,I) = NUVAR
            IPM(114 + 15*N  ,I) = IADBUF
            IPM(115 + 15*N  ,I) = NFUNC 
c 
c           Flag for XFEM (mono + multi-layer)
C 
            
            IF (IXFEM == 1) ICRACK3D = 1
            IPM(236,I) = MAX(IPM(236,I), IXFEM)  
c
            DO J=1,NFUNC
              IPM(115+ J +  N*15,I) = IFUNC(J)
            ENDDO  
C 
            ILAW = NINT(PM(19,I))
            IPM(220, I) = IPM(220,I) + 1 ! ifail number
            IF (ILAW /= 0) NVAR_FAIL(ILAW, N+1) = MAX(NVAR_FAIL(ILAW, N+1),NUVAR)
            IPM(236+IPM(220,I), I) = FAIL_ID
            IPM(241+IPM(220,I), I) = IRUP
c-------------------------
            DO J=1,NUPARAM
              BUFMAT(IADBUF+J-1) = UPARAM(J)
            ENDDO
            IADBUF = IADBUF + NUPARAM
            BUFLEN = BUFLEN + NUPARAM
c-------------------------
          ENDIF   
c
        ENDDO    !  I=1,NUMMAT-1
      ENDDO      !  NF = 1,HM_NBFAIL
c--------------------------------------------------------------------
C     Somme(max(ai), max(bi)) ....)
      DO I=1,NUMMAT-1
        N = IPM(220, I)                           
        IF (N >  0) THEN                           
          ILAW = NINT(PM(19,I))                     
          NUVARG = 0                                
          DO J=1,N                                
             IF(ILAW /= 0) NUVARG = NUVARG + NVAR_FAIL(ILAW, J) 
          ENDDO                                   
          IPM(221,I)  = NUVARG                    
        ENDIF 
      ENDDO
C-----------       
 1000 FORMAT(//
     & 5X,'MAT_ID . . . . . . . . . . . . . . .=',I10/
     & 5X,'FAILURE MODEL. . . . . . . . . . . .=',I10/
     & 5X,'FAIL_ID. . . . . . . . . . . . . . .=',I10/)
C-----------       
      RETURN
      END
