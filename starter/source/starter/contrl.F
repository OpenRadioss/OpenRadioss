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
Chd|  CONTRL                        source/starter/contrl.F       
Chd|-- called by -----------
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        CONTRBE2                      source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        FIND_DT1BRICK_ENGINE          source/starter/contrl.F       
Chd|        FMT_DEF                       source/starter/fmt_def.F      
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_ELEM_COUNT                 source/devtools/hm_reader/hm_elem_count.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_NEXT                source/devtools/hm_reader/hm_option_next.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_PRELCE16S                  source/elements/reader/hm_read_solid.F
Chd|        HM_PREREAD_INIVEL             source/initial_conditions/general/inivel/hm_read_inivel.F
Chd|        HM_PREREAD_NODE               source/elements/reader/hm_preread_node.F
Chd|        HM_PREREAD_PART               source/model/assembling/hm_read_part.F
Chd|        HM_PREREAD_PROPERTIES         source/properties/hm_preread_properties.F
Chd|        HM_READ_ALE_GRID              source/general_controls/ale_grid/hm_read_ale_grid.F
Chd|        HM_READ_ALE_MUSCL             source/general_controls/ale_cfd/hm_read_ale_muscl.F
Chd|        HM_READ_ALE_SOLVER            source/general_controls/ale_cfd/hm_read_ale_solver.F
Chd|        HM_READ_ANALY                 source/general_controls/computation/hm_read_analy.F
Chd|        HM_READ_CAA                   source/general_controls/computation/hm_read_caa.F
Chd|        HM_READ_DEFINTER              source/general_controls/default_values/hm_read_definter.F
Chd|        HM_READ_DEFSHELL              source/general_controls/default_values/hm_read_defshell.F
Chd|        HM_READ_DEFSOLID              source/general_controls/default_values/hm_read_defsolid.F
Chd|        HM_READ_IMPLICIT              source/general_controls/computation/hm_read_implicit.F
Chd|        HM_READ_INISTA                source/initial_conditions/inista/hm_read_inista.F
Chd|        HM_READ_IOFLAG                source/general_controls/inputoutput/hm_read_ioflag.F
Chd|        HM_READ_LAGMUL                source/tools/lagmul/hm_read_lagmul.F
Chd|        HM_READ_REFSTA                source/loads/reference_state/refsta/hm_read_refsta.F
Chd|        HM_READ_SMS                   source/general_controls/computation/hm_read_sms.F
Chd|        HM_READ_SPHGLO                source/general_controls/computation/hm_read_sphglo.F
Chd|        HM_READ_SPMD                  source/general_controls/computation/hm_read_spmd.F
Chd|        HM_READ_UNIT                  source/general_controls/computation/hm_read_unit.F
Chd|        HM_READ_UPWIND                source/general_controls/computation/hm_read_upwind.F
Chd|        NBADIGEMESH                   source/elements/ige3d/nbadigemesh.F
Chd|        NBADMESH                      source/model/remesh/nbadmesh.F
Chd|        NBSPH                         source/elements/sph/nbsph.F   
Chd|        ID                            source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
Chd|        ALE_EBCS_MOD                  ../common_source/modules/ale_ebcs_mod.F
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        CHECK_MOD                     ../common_source/modules/check_mod.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        GRP_SIZE_MOD                  share/modules1/grp_size_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        INIVOL_ARRAY_MOD              share/modules1/inivol_mod.F   
Chd|        INIVOL_DEF_MOD                share/modules1/inivol_mod.F   
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        REFSTA_MOD                    share/modules1/refsta_mod.F   
Chd|        RESTMOD                       share/modules1/restart_mod.F  
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        USER_WINDOWS_MOD              ../common_source/modules/user_windows_mod.F
Chd|====================================================================
      SUBROUTINE CONTRL(MULTI_FVM,LSUBMODEL,IS_DYNA,DETONATORS,USER_WINDOWS)
C----------------------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD 
      USE SUBMODEL_MOD
      USE PINCHTYPE_MOD
      USE HM_OPTION_READ_MOD
      USE CHECK_MOD
      USE INOUTFILE_MOD
      USE SETDEF_MOD
      USE DETONATORS_MOD
      USE INIVOL_DEF_MOD
      USE INIVOL_ARRAY_MOD
      USE REFSTA_MOD
      USE ALE_EBCS_MOD
      USE RESTMOD
      USE GRP_SIZE_MOD
      USE ANIM_MOD
      USE USER_WINDOWS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com05_c.inc"
#include      "com06_c.inc"
#include      "com07_c.inc"
#include      "com08_c.inc"
#include      "com09_c.inc"
#include      "com10_c.inc"
#include      "com_xfem1.inc"
#include      "intstamp_c.inc"
#include      "random_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr10_c.inc"
#include      "scr12_c.inc"
#include      "scr15_c.inc"
#include      "scr16_c.inc"
#include      "scr21_c.inc"
#include      "scr22_c.inc"
#include      "scr23_c.inc"
#include      "titr_c.inc"
#include      "param_c.inc"
#include      "ale_param.inc"
#include      "sphcom.inc"
#include      "lagmult.inc"
#include      "fxbcom.inc"
#include      "fluid_scr_c.inc"
#include      "scr14_c.inc"
#include      "remesh_c.inc"
#include      "sysunit.inc"
#include      "commandline.inc"
#include      "r2r_c.inc"
#include      "userlib.inc"
#include      "spmd_c.inc"
#include      "alefvm.inc"
#include      "drape_c.inc"
#include      "thermal_c.inc"
#include      "inter22.inc"
#include      "alesolver.inc"
#include      "alemuscl.inc"
#include      "boltpr_c.inc"
#include      "ige3d_c.inc"
#include      "submod_c.inc"
#include      "com_engcards_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      INTEGER,INTENT(IN)::IS_DYNA
      TYPE(DETONATOR_STRUCT_) :: DETONATORS
      TYPE(USER_WINDOWS_), INTENT(INOUT) :: USER_WINDOWS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, ISFIL, IPCT, IBID,INSH,
     .        IHBE_DD,IPARITH,JALE,JEUL,
     .        IUN,NSHFRAM,JUPW,IMAT,IG,ISH3N_DD,IFRAME_DDS,
     .   IPLA_DDS,NPTS_DD,UID,IHBE,ISMSTR,IPLAS,ICPRE,ICSTR,NPT,
     .   ISROT,L1,NPTS, NIMPDISP,NIMPVEL,NIMPACC,ICR,
     .   STAT,IERROR,ID,IDX,IDY,IDZ,NCTRL,BID,NSH3NFRAM,NS17_OLD,
     .        IGNORE_SPMD,IGNORE_THREADS,GOT_VARIABLE,KROT,OLD_RSB,
     .        NRAFX,NRAFY,NRAFZ,NFUNCT0,NTABLE0,NTABLE1,
     .        IRFE,IRFL, HM_NINTER_DEF,NB_AMS,NUMNUSR,NPERTURB_HM
      INTEGER IARCHS(8)
      INTEGER IS_BEGIN,SCHAR
      my_real
     .   DTINI, DTX ,RBID
      CHARACTER CART*ncharline, XRFILE*ncharline,
     .   KEY*ncharline,KEY2*ncharline,TMPLINE*ncharline
      CHARACTER TITR*nchartitle,MESS*40, ERRMSG*40
      CHARACTER (LEN=255) :: STR_NBTHREADS

C OpenMP specific
#if defined(_OPENMP)
       INTEGER OMP_GET_THREAD_NUM, OMP_GET_NUM_THREADS,
     .NTHREAD1
       EXTERNAL OMP_GET_THREAD_NUM, OMP_GET_NUM_THREADS
       CHARACTER (LEN=255) :: STR
#endif
       INTEGER NTHREAD_S
       my_real :: DT_INPUT
C
      INTEGER , DIMENSION(:), ALLOCATABLE :: NPLY,NSUB,NISUB
      
      INTEGER :: NITER,IFORM

      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=4096) :: TMP_NAME
      LOGICAL :: IS_AVAILABLE
      INTEGER :: NUMTITLE, NGRTRIA, IDUMMY, NANIM_VERS
      INTEGER :: NALEMUSCL
      INTEGER :: NB_INISHE,NB_INISH3,NB_INIBRI,NB_INIQUAD,
     .           NB_INIBEAM,NB_INITRUSS,NB_INISPRIG
      CHARACTER*20 UNIT_NAME
      INTEGER IS_U_STRING
C----------------------------------------------------------
C   E x t e r n a l  F u n c t i o n
C----------------------------------------------------------
      INTEGER  NBCS_LAGM
      EXTERNAL NBCS_LAGM
C-----------------------------------------------
C   data
C-----------------------------------------------
      DATA S0FILE/'                '/
      DATA XRFILE/'                '/
      DATA IUN/1/
C=======================================================================
C     Read title
C=======================================================================
      CALL HM_OPTION_COUNT('/TITLE', NUMTITLE)
      IF (NUMTITLE > 0) THEN
         CALL HM_OPTION_START('/TITLE')
         CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_TITR = LINE)
         CALL HM_GET_STRING('MY_TITLE', LINE, ncharline, IS_AVAILABLE)
      ELSE 
         LINE = ' '
      ENDIF
!     Standard output
      WRITE(ISTDO,'(A)') LINE(1:LEN_TRIM(LINE))
!     For 0.out title writing
      READ(LINE(1:LEN_TRIM(LINE)),'(20A4)') TEXT
      IMOT=0
      REEL=ZEP66  
C=======================================================================
C     LECTURE DES SYSTEMES D'UNITES
C     Cette option doit rester la premiere lue car elle sert a convertir
C     les valeurs par defaut de toutes les options dans le systeme
C     d'unites defini par l'utilisateur
C=======================================================================
      WRITE(ISTDO,'(A)')TITRE(12)
      CALL HM_OPTION_COUNT('/UNIT',NUNIT0)
c
      CALL HM_OPTION_COUNT('/BEGIN',IS_BEGIN)

      SCHAR = 20
      IF (IS_BEGIN /= 0) THEN
        CALL HM_OPTION_START('/BEGIN')
        CALL HM_OPTION_NEXT()

        CALL HM_GET_INTV('INVERS', INVERS, IS_AVAILABLE, LSUBMODEL)
        INVERS_SRC = INVERS
        INVERS_INIT = INVERS
        CALL FMT_DEF(INVERS,0)

        CALL HM_GET_STRING('length_inputunit_code',KEYLI,SCHAR,IS_AVAILABLE)
        CALL HM_GET_STRING('mass_inputunit_code',KEYMI,SCHAR,IS_AVAILABLE)
        CALL HM_GET_STRING('time_inputunit_code',KEYTI,SCHAR,IS_AVAILABLE)
        CALL HM_GET_STRING('length_workunit_code',KEYL,SCHAR,IS_AVAILABLE)
        CALL HM_GET_STRING('mass_workunit_code',KEYM,SCHAR,IS_AVAILABLE)
        CALL HM_GET_STRING('time_workunit_code',KEYT,SCHAR,IS_AVAILABLE)
C
        IF(INVERS <= 90)THEN
          CALL HM_OPTION_START('/UNIT')
C     
          DO N=1,NUNIT0
            CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                         OPTION_ID = ID)
            UNIT_NAME = ''
            CALL HM_GET_STRING('UNIT_NAME',UNIT_NAME,2*ncharfield,IS_AVAILABLE)
c
            IF(UNIT_NAME == 'LENGTH') THEN
              CALL HM_GET_STRING('TYPE_UNIT',KEYL,2*ncharfield,IS_AVAILABLE)
            ENDIF
            IF(UNIT_NAME == 'MASS') THEN
              CALL HM_GET_STRING('TYPE_UNIT',KEYM,2*ncharfield,IS_AVAILABLE)
            ENDIF
            IF(UNIT_NAME == 'TIME') THEN
              CALL HM_GET_STRING('TYPE_UNIT',KEYT,2*ncharfield,IS_AVAILABLE)
            ENDIF
c
          ENDDO
        ENDIF
      ENDIF
      CALL HM_READ_UNIT(UNITAB,LSUBMODEL)
C=======================================================================
C     READING CONTROL CARDS
C=======================================================================
      WRITE(ISTDO,'(A)')TITRE(10)
C-------------------------------------------------------------------
C     READING OF /SPMD
C-------------------------------------------------------------------
      CALL HM_READ_SPMD(LSUBMODEL)
C
      IGNORE_SPMD=0
      IF (GOT_NCPU ==1) THEN
        IF (NSPMD/=0) THEN
          IGNORE_SPMD=1
        END IF
        NSPMD = NCPU
      ENDIF
C
C Calcul du nombre de threads
C
      IGNORE_THREADS=0
      GOT_VARIABLE=0
#if defined(_OPENMP)
      STR = ' '
      CALL  GETENV('OMP_NUM_THREADS',STR)
      NTHREAD1=0
C nthread1 : nombre de threads fixe par la variable environnememt
      READ(STR,'(I10)',ERR=999)NTHREAD1
      IF(NTHREAD1>0)THEN
        NTHREAD = NTHREAD1
      ENDIF
      IF (GOT_NTH ==1) THEN
        IF (NTHREAD/=0) THEN
          IGNORE_THREADS=1
        END IF
        NTHREAD = NTH
C       CALL OMP_SET_NUM_THREADS(NTHREAD)
      ELSE
        IF(NTHREAD1>0)THEN
          GOT_VARIABLE=1
        END IF
      ENDIF
C cas ou ni -nt, ni OMP_NUM_THREADS, ni /SPMD sont renseignes
      NTHREAD = MAX(NTHREAD,1)
      NTHREAD_S = NTHREAD
      NTHREAD_R2R = NTHREAD
      CALL OMP_SET_NUM_THREADS(NTHREAD_S)

c else (open mp non defined)
#elif 1
      IF (GOT_NTH ==1) THEN
        IF (NTHREAD/=0) THEN
          IGNORE_THREADS=1
        END IF
        NTHREAD = NTH
      ENDIF
      NTHREAD_S = 1
#endif
C-------------------------------------------------------------------
C Option /ARCH non presente, IARCH= IBUILTIN et Grpsiz = ARCHINFO(IBUILTIN)
        IARCH = IBUILTIN
        NVSIZ = ARCHINFO(IBUILTIN,1)
        IVECTOR = ARCHINFO(IBUILTIN,2)
    
      ! ---------------------------
      ! -grp_size hidden option
      IF(GRP_SIZE_BOOL) THEN
        NVSIZ = GRP_SIZE
      ENDIF
      ! ---------------------------
C-------------------------------------------------------------------
C     READING OF /IOFLAG
C-------------------------------------------------------------------
      CALL HM_READ_IOFLAG(LSUBMODEL)
C-------------------------------------------------------------------
C     READING OF /ANALY
C-------------------------------------------------------------------
      ISUB  = 0
      IPARI0= 1
      INTEG8= 0
      CALL HM_READ_ANALY(NANALY,IPARITH,ISUB,IPARI0,MSHSUB,LSUBMODEL)
      N2D   = NANALY
C-------------------------------------------------------------------
C     READING OF /IMPLICIT
C-------------------------------------------------------------------
      CALL HM_READ_IMPLICIT(LSUBMODEL)
C-------------------------------------------------------------------
C     READING OF /AMS
C-------------------------------------------------------------------
      CALL HM_OPTION_COUNT('/AMS', NB_AMS)
      CALL HM_READ_SMS(LSUBMODEL)
C-------------------------------------------------------------------
C     READING OF /CAA
C-------------------------------------------------------------------
      CALL HM_READ_CAA(LSUBMODEL)
C-------------------------------------------------------------------
C     READING OF /RANDOM
C-------------------------------------------------------------------
      CALL HM_OPTION_COUNT('/RANDOM',NRAND)
C-------------------------------------------------------------------
C     READING OF /LAGMUL
C-------------------------------------------------------------------
      CALL HM_READ_LAGMUL(LSUBMODEL)
C-------------------------------------------------------------------
      CALL HM_OPTION_COUNT('/PRIVATE/METADATA/FATXML',IPRIVATE)
      IF(IPRIVATE > 0 ) IPRIVATE = 1
C-------------------------------------------------------------------
C     READING OF /STAMPING
C-------------------------------------------------------------------
      CALL HM_OPTION_COUNT('/STAMPING',ISTAMPING)
      IF(ISTAMPING > 0 ) ISTAMPING = 1
C-------------------------------------------------------------------
      ICRASH = 0
C-------------------------------------------------------------------
C     READING DEFAULT VALUES FOR SHELLS, SOLIDS, INTERFACES ...
C-------------------------------------------------------------------
C
      IHBE_D=0
      IHBE_DS=0
      ISST_D=0
      ISST_DS=0
      ITHK_D=0
      IPLA_D=0
      IPLA_DDS=2 
      IPLA_DS =2 
      ISTRA_D =1
      ISTR_D=0
      ISHEA_D=0 ! Old hidden flag in /DEF_SHELL
      INER_9_12 = ZERO
      INSH = 0 ! Old hidden flag in /DEF_SHELL
      IHBE_DD=1
      ISH3N_D =0
      ISH3N_DD=2
      IFRAME_DS  = 0
      IFRAME_DDS = 1
      NPTS_D    = -1 ! Old hidden flag in /DEF_SHELL
      NPTS_DD   = 0
      IDRIL_D=0
      IMAS_DS = 0
      ICPRE_D =0 
C-------------------------------------------------------------------
C     READING OF SHELL PROPERTIES DEFAULT VALUES
C-------------------------------------------------------------------
      CALL HM_READ_DEFSHELL(LSUBMODEL)
C-------------------------------------------------------------------
C     READING OF SOLID PROPERTIES DEFAULT VALUES
C-------------------------------------------------------------------
      CALL HM_READ_DEFSOLID(IPLA_DDS,LSUBMODEL)
      IF(IIMPLICIT == 1) THEN
        IF (IPLA_D == 0) IPLA_D = 1
        IF (IHBE_DS == 0) IHBE_DS = 14
        IF (IHBE_D == 0)  IHBE_D  = 24
        IF (IDRIL_D == 0) IDRIL_D = 1
      ENDIF
C----------------------------------------------
      IF(IFRAME_DS == 0) IFRAME_DS = IFRAME_DDS  
      IS17OLD = 1 ! To be cleaned
C-----
      IF(IHBE_D==0)IHBE_D = IHBE_DD
      IF(ISH3N_D==0)ISH3N_D = ISH3N_DD
      IF(INSH==1)INER_9_12 = TWELVE      
      IF(INSH==2)INER_9_12 = SIXTEEN      
      IF(INSH==3)INER_9_12 = FOURTY8  
      IF(INSH==4)INER_9_12 = THIRTY     
      IF(INSH==5)INER_9_12 = NINE       
C-------------------------------------------------------------------
C     READING OF INTERFACES DEFAULT VALUES
C-------------------------------------------------------------------
      HM_NINTER_DEF    = 0
      DEF_INTER(1:100) = 0
      CALL HM_OPTION_COUNT('/DEFAULT/INTER',HM_NINTER_DEF)
      CALL HM_READ_DEFINTER(HM_NINTER_DEF,DEF_INTER,LSUBMODEL)
C-------------------------------------------------------------------
      CALL HM_OPTION_COUNT('/INTTHICK/V5',IINTTHICK)
      IF (IINTTHICK > 1) THEN
        CALL ANCMSG(MSGID=725,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=IINTTHICK)
      ENDIF
C-------------------------------------------------------------------
C     READGING OF /SHFRAM
C-------------------------------------------------------------------
      ISHFRAM = 0
      CALL HM_OPTION_COUNT('/SHFRA/V4',NSHFRAM)
      IF (NSHFRAM == 1) THEN
        ISHFRAM = 2
      ELSEIF (NSHFRAM > 1) THEN
        CALL ANCMSG(MSGID=546,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=NSHFRAM)
      ENDIF      
C-------------------------------------------------------------------
C     READGING OF /SH_3NFR
C-------------------------------------------------------------------
      ISH3NFRAM = 0
C=======================================================================
C     Initialisations vs DOMDEC (Must stay after reading the control cards, because of output to STDO)
C=======================================================================
      IF(DECNEQ==0) DECNEQ = 100
      IF(DECTYP < 0) THEN
        DECTYP = - DECTYP
        EDGE_FILTERING = 0
      ELSE
        EDGE_FILTERING = 1
      ENDIF
      OLD_RSB=0
      IF(DECTYP == 2)THEN
         DECTYP=0
         OLD_RSB=1
      ENDIF
      IF(DECTYP==0)THEN
        IF(NB_AMS==0.AND.IIMPLICIT==0) THEN
          DECTYP = 3
        ELSE
          DECTYP = 5
        END IF
      END IF

      DDNOD_SMS=0
      IF(DECTYP==7)THEN
        IF(NB_AMS/=0)THEN
          DECTYP = 5
          DDNOD_SMS=1
        ELSEIF(IIMPLICIT/=0) THEN
          DECTYP = 5
        ELSE
          DECTYP = 3
        END IF
      END IF
      IF(NSPMD < 1)       NSPMD=1
      IF(NSPMD > PARASIZ) NSPMD=PARASIZ
      IF(NTHREAD < 1)       NTHREAD=1
C maximum number of SMP threads equal to NTHMAX
      IF(NTHREAD > NTHMAX) NTHREAD=NTHMAX
C
      IF (GOT_INSPIRE_ALM == 1)THEN
        IF (NTHREAD_S==1)THEN
          WRITE(ISTDO,'(A,I4,A)')' .. SOLVER RUNNING ON ',NTHREAD_S,
     .    ' THREAD'
        ELSE
          WRITE(ISTDO,'(A,I4,A)')' .. SOLVER RUNNING ON ',NTHREAD_S,
     .    ' THREADS'
        ENDIF
      ELSE
        IF (NTHREAD_S==1)THEN
          WRITE(ISTDO,'(A,I4,A)')' .. STARTER RUNNING ON ',NTHREAD_S,
     .    ' THREAD'
        ELSE
          WRITE(ISTDO,'(A,I4,A)')' .. STARTER RUNNING ON ',NTHREAD_S,
     .    ' THREADS'
        ENDIF
      ENDIF
    
c warning if stacksize value lower than 512 M
      IF (STACKSIZ>0 .AND. STACKSIZ<=512)THEN
          CALL ANCMSG(MSGID=1032,
     .        MSGTYPE=MSGWARNING,
     .        ANMODE=ANINFO_BLIND_1)
      ENDIF

c warning if KMP_STACKSIZE value not set
      IF (NTHREAD_S > 1 .AND. KMP_SET == 1)THEN
          CALL ANCMSG(MSGID=1004,
     .        MSGTYPE=MSGWARNING,
     .        ANMODE=ANINFO_BLIND_1)

      ENDIF

c warning if KMP_STACKSIZE value lower than default value
      IF (NTHREAD_S > 1 .AND. KMP_SET == 2)THEN
          CALL ANCMSG(MSGID=1005,
     .        MSGTYPE=MSGWARNING,
     .        ANMODE=ANINFO_BLIND_1)

      ENDIF
C=======================================================================
C     DENOMBREMENT DES OPTIONS
C=======================================================================
C     WRITE(ISTDO,'(/,A)')' .. OPTIONS COUNTING'
C-------------------------------------------------------------------
C     User-Defined Nodes & Cnodes numbering
C     Check nodes within some tolerance and possibly merge nodes
C-------------------------------------------------------------------
      CALL CPP_NODES_COUNT(NUMNUSR,NUMCNOD)
C------
C     Pre-read Nodes & Cnodes and compute NUMNOD taking into account that some nodes may be merged.
      CALL HM_PREREAD_NODE(UNITAB,LSUBMODEL,NUMNUSR,IS_DYNA) ! NUMNOD is computed here
C------
C     numnod=0 : ask user to provide relevant input file.
      IF(NUMNOD==0)THEN
          CALL ANCMSG(MSGID=3,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO)
          CALL ARRET(2)
      ENDIF
C-------------------------------------------------------------------
      ISUMNX      = 0
      NANIM1D     = 0
      MAXNX       = 0
      NANIM2D     = 0
      NANIM3D     = 0
      NTHREAD_R2R = 1  
      IBID        = 0
      RBID        = ZERO
      NPINCH      = 0
      NSUBDOM     = 0
C---------------------------------------------------
      CALL HM_OPTION_COUNT('/SUBDOMAIN',NSUBDOM)
      CALL HM_OPTION_COUNT('/EXTERN/LINK',NR2RLNK)
C---------------------------------------------------
      LENMOD=0
      LENGLM=0
      LENCP=0
      LENLM=0
      LENFLS=0
      LENDLS=0
      LENVAR=0
      LENRPM=0
      LENMCD=0
      LENELM=0
      LENSIG=0
      LENGRVI=0
      LENGRVR=0
C---  Xfem -------------------------------------------------------
C---
C   ATTENTION:    NUMNOD, NUMELC, NUMELTG ==> will be changed (by xfem)
C---
      NLEVMAX     = 0                    
      CALL HM_OPTION_COUNT('/INICRACK',NINICRACK)
C---------------------------------------------------
C     COUNTING ELEMENTS
C---------------------------------------------------
      NUMELX  = 0
      NUMBRICK  = 0
      NUMTETRA4 = 0
      NUMPENTA6 = 0
      NUMELS10  = 0
      NUMELS20  = 0
      CALL HM_ELEM_COUNT('XELEM',NUMELX,IS_DYNA)
      CALL HM_ELEM_COUNT('BRICK',NUMBRICK,IS_DYNA)
      CALL HM_ELEM_COUNT('TETRA4',NUMTETRA4,IS_DYNA)
      CALL HM_ELEM_COUNT('PENTA6',NUMPENTA6,IS_DYNA)
      NUMELS8 = NUMBRICK+NUMTETRA4+NUMPENTA6
C
      CALL HM_ELEM_COUNT('TETRA10',NUMELS10,IS_DYNA)
      IF (IS_DYNA == 0) CALL HM_ELEM_COUNT('BRICK20',NUMELS20,IS_DYNA)
      CALL HM_ELEM_COUNT('SHEL16',NUMELS16,IS_DYNA)
C
C
      NUMELS  = NUMELS8+NUMELS10+NUMELS20+NUMELS16
C
C     Look for /DT1/BRICK & /DT1TET10 /DTTSH in RADIOSS Engine input deck
C
      IF(NUMELS>0)THEN
        CALL FIND_DT1BRICK_ENGINE()
      END IF
C-----
      CALL HM_ELEM_COUNT('SHELL',NUMELC,IS_DYNA)
      IF(NUMELC > 0 .AND. NANALY /= 0)THEN
          CALL ANCMSG(MSGID=285,MSGTYPE=MSGERROR,ANMODE=ANINFO)
          NUMELC = 0
      ENDIF      
C-----
      CALL HM_ELEM_COUNT('SH3N',NUMELTG,IS_DYNA)
      IF(NUMELTG > 0 .AND. NANALY /= 0)THEN
          CALL ANCMSG(MSGID=287,MSGTYPE=MSGERROR,ANMODE=ANINFO)
          NUMELTG = 0
      ENDIF
C-----
      CALL HM_ELEM_COUNT('TRUSS' ,NUMELT,IS_DYNA)
      CALL HM_ELEM_COUNT('BEAM'  ,NUMELP,IS_DYNA)
      CALL HM_ELEM_COUNT('SPRING',NUMELR,IS_DYNA)
C-----
      CALL HM_ELEM_COUNT('RIVET',NRIVET,IS_DYNA)
C-----
      NUMELQ = 0
      CALL HM_ELEM_COUNT('QUAD',NUMELQ,IS_DYNA)
C-----
      CALL HM_ELEM_COUNT('TRIA',NUMELTRIA,IS_DYNA)
      NUMELTG = NUMELTG + NUMELTRIA  !2d shell + 3d tria (only one type depending on N2D flag)
C-----
C---------------------------------------------------
      CALL HM_OPTION_COUNT('PART',NPART)
      IF(NPART==0)THEN
          CALL ANCMSG(MSGID=1114,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO)
      ENDIF
      CALL HM_OPTION_COUNT('SUBSET',NSUBS)
C     add 1 : for global subset
      NSUBS    = NSUBS+1
C---------------------------------------------------
      CALL HM_OPTION_COUNT('/THPART',NTHPART)
C---------------------------------------------------
C     LOOKING FOR /ADMESH and COUNTING ADDITIONAL NODES & ELEMENTS
C---------------------------------------------------
      CALL HM_OPTION_COUNT('/ADMESH/GLOBAL',NADMESHG)
      CALL HM_OPTION_COUNT('/ADMESH/SET',NADMESHSET)
      CALL HM_OPTION_COUNT('/ADMESH/STATE',NADMESHSTAT)
      NADMESH = NADMESHG + NADMESHSET + NADMESHSTAT
      NUMNOD0  = NUMNOD
      NUMELC0  = NUMELC
      NUMELTG0 = NUMELTG
      ISTATCND = 0
      IADMERRT = 0
      IF(NADMESH/=0)THEN
C
C       NUMNOD, NUMELC, NUMELTG will be changed
        CALL NBADMESH(LSUBMODEL,NUMNUSR,UNITAB)
      END IF
C-----
      IF(NSPMD > 1)THEN
C       Tests option non disponible en SPMD (ie NSPMD > 1)
        IF(NADMESH/=0)THEN
          CALL ANCMSG(MSGID=704,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO)
        END IF
      END IF
C---------------------------------------------------
C     LOOKING FOR IGE REFINEMENT and COUNTING ADDITIONAL NODES & ELEMENTS
C---------------------------------------------------
      NUMNODIGE0 = NUMNOD
      NUMELIG3D0 = NUMELIG3D
      IF(NRAFMAX/=0)THEN
C
C       NUMNOD, NUMELIG3D will be changed, SIXIG3D and KXIG3D too
        CALL NBADIGEMESH(LSUBMODEL,NUMNUSR)
      END IF
C---------------------------------------------------
C     MATERIALS (+1 materiau fictif ressort)
C---------------------------------------------------
      HM_NUMMAT = 0
      CALL HM_OPTION_COUNT('MATERIAL',HM_NUMMAT)
      NUMMAT      = HM_NUMMAT + 1
C---------------------------------------------------
C     PROPERTIES
C---------------------------------------------------
      HM_NUMGEO = 0
      CALL HM_OPTION_COUNT('PROPERTY',HM_NUMGEO)
      NUMGEO      = HM_NUMGEO
C-----
      CALL HM_OPTION_COUNT('/PLY', NUMPLY)
      CALL HM_OPTION_COUNT('/DRAPE',NDRAPE)
      CALL HM_OPTION_COUNT('/STACK',NUMSTACK)         
C---------------------------------------------------
C     TOOLS
C---------------------------------------------------
      CALL HM_OPTION_COUNT('/ACCEL', NACCELM)
C-----
      CALL HM_OPTION_COUNT('/GAUGE', NBGAUGE)
C-----
      CALL HM_OPTION_COUNT('/ACTIV', NACTIV)
C-----
      CALL HM_OPTION_COUNT('/ADMAS',NODMAS)
C-----
      CALL HM_OPTION_COUNT('/CLUSTER',NCLUSTER)
C-----
      CALL HM_OPTION_COUNT('/FUNCT'  , NFUNCT0)
      CALL HM_OPTION_COUNT('/TABLE/0', NTABLE0)
      CALL HM_OPTION_COUNT('/TABLE/1', NTABLE1)
      NFUNCT = NFUNCT0 + NTABLE0 + NTABLE1 
      NTABLE = NFUNCT
C-----
      CALL HM_OPTION_COUNT('/FUNC_2D', NFUNC2D)
C-----
      IPERTURB = 0
      CALL HM_OPTION_COUNT('/PERTURB',NPERTURB)
      IF (NPERTURB > 0) IPERTURB = 1
C-----
      CALL HM_OPTION_COUNT('/SENSOR', NSENSOR)
C-----
      CALL HM_OPTION_COUNT('/SKEW',NUMSKW)
      CALL HM_OPTION_COUNT('/FRAME',NUMFRAM)
C-----
      CALL HM_OPTION_COUNT('TRANSFORM',NTRANSF)
C---------------------------------------------------
C     LOADS
C---------------------------------------------------
      CALL HM_OPTION_COUNT('/CLOAD',NCONLD)
      CALL HM_OPTION_COUNT('/PLOAD',NPRELD)
      CALL HM_OPTION_COUNT('/LOAD/CENTRI',NLOADC)
      CALL HM_OPTION_COUNT('/LOAD/PFLUID',NLOADP_F)
      CALL HM_OPTION_COUNT('/LOAD/PBLAST',NLOADP_B)
      CALL HM_OPTION_COUNT('/LOAD/PRESSURE',NLOADP_HYD)
C-----
      CALL HM_OPTION_COUNT('/GRAV',NGRAV)
C---------------------------------------------------
C     KINEMATIC CONDITIONS
C---------------------------------------------------
      CALL HM_OPTION_COUNT('/BCS/LAGMUL',NBCSLAG)
      CALL HM_OPTION_COUNT('/BCS/CYCLIC',NBCSCYC)
      CALL HM_OPTION_COUNT('/BCS',NUMBCS)
      CALL HM_OPTION_COUNT('/NBCS',NUMBCSN)
      NBCSKIN = NUMBCS - NBCSLAG
C-----
      CALL HM_OPTION_COUNT('/IMPDISP',NIMPDISP)
      CALL HM_OPTION_COUNT('/IMPVEL' ,NIMPVEL)
      CALL HM_OPTION_COUNT('/IMPACC' ,NIMPACC)
      NFXVEL  = NIMPDISP + NIMPVEL + NIMPACC
C-----
      CALL HM_OPTION_COUNT('/RBODY',NRBODY)
      NRBODY0 = NRBODY
      CALL HM_OPTION_COUNT('/RBODY/LAGMUL',NRBYLAG)
      NRBYKIN = NRBODY - NRBYLAG
      CALL HM_OPTION_COUNT('/FXBODY',NFXBODY)
C-----
      CALL HM_OPTION_COUNT('/MERGE/RBODY',NRBMERGE)
C-----
      CALL HM_OPTION_COUNT('/MERGE/NODE',NB_MERGE_NODE)
C-----
      CALL HM_OPTION_COUNT('/RBE2', NRBE2)
      CALL HM_OPTION_COUNT('/RBE3', NRBE3)
C-----
      CALL HM_OPTION_COUNT('/RLINK', NLINK)
C-----
      CALL HM_OPTION_COUNT('/RWALL',NRWALL)
C-----
      CALL HM_OPTION_COUNT('/MPC',NUMMPC)
      CALL HM_OPTION_COUNT('/CYL_JOINT', NJOINT)
      CALL HM_OPTION_COUNT('/GJOINT', NGJOINT)
C---------------------------------------------------
C     SEATBELT TOOLS
C---------------------------------------------------
      CALL HM_OPTION_COUNT('/SLIPRING', NSLIPRING)
      CALL HM_OPTION_COUNT('/RETRACTOR', NRETRACTOR)
C---------------------------------------------------
C     MONITORED VOLUMES
C---------------------------------------------------
      CALL HM_OPTION_COUNT('/MONVOL', NMONVOL)
C---------------------------------------------------
C     INTERFACES
C---------------------------------------------------
      NINTER = 0 
      HM_NINTER= 0
      CALL HM_OPTION_COUNT('/INTER',HM_NINTER)
C-----      
      CALL HM_OPTION_COUNT('/INTER/TYPE22',INT22)
      IF(INT22>0)NSUB22=2
      IF(INT22>0)IALEFVM=1 !AUTOMATICALLY ENABLING ALE FVM SCHEME FOR FSI INTER22
C-----      
      CALL HM_OPTION_COUNT('/INTER/SUB',NINTSUB)
      NINTER = HM_NINTER - NINTSUB
C-----      
      CALL HM_OPTION_COUNT('/FRICTION',NINTERFRIC)
      CALL HM_OPTION_COUNT('/FRIC_ORIENT',NFRIC_ORIENT)
C---------------------------------------------------
      CALL HM_OPTION_COUNT('/DAMP',NDAMP)
C---------------------------------------------------
      CALL HM_OPTION_COUNT('/PRELOAD',NPRELOAD)   !Bolt preloading
C---------------------------------------------------
      CALL HM_OPTION_COUNT('/SECT',NSECT)
C---------------------------------------------------
C---------------------------------------------------
C     Box, Groups, Lines, Surfaces, Sets
C---------------------------------------------------
      CALL HM_OPTION_COUNT('/BOX' ,NBBOX)
      CALL HM_OPTION_COUNT('/SURF',NSURF)
      CALL HM_OPTION_COUNT('/LINE' ,NSLIN)
      CALL HM_OPTION_COUNT('/GRNOD' ,NGRNOD )
      CALL HM_OPTION_COUNT('/GRBRIC',NGRBRIC)
      CALL HM_OPTION_COUNT('/GRQUAD',NGRQUAD)
      CALL HM_OPTION_COUNT('/GRPART',NGRPART)
      CALL HM_OPTION_COUNT('/GRSHEL',NGRSHEL)
      CALL HM_OPTION_COUNT('/GRSH3N',NGRSH3N)
      CALL HM_OPTION_COUNT('/GRTRIA',NGRTRIA)
      NGRSH3N = NGRSH3N + NGRTRIA ! 3D or 2D (same buffer)
      CALL HM_OPTION_COUNT('/GRTRUS',NGRTRUS)
      CALL HM_OPTION_COUNT('/GRBEAM',NGRBEAM)
      CALL HM_OPTION_COUNT('/GRSPRI',NGRSPRI)
      NSETS = 0
      CALL HM_OPTION_COUNT('/SET',NSETS)
      NGPE    = NGRNOD + NGRBRIC + NGRQUAD + NGRSHEL + NGRSH3N +
     .          NGRTRUS + NGRBEAM + NGRSPRI + NGRPART
C---------------------------------------------------
C     Initial conditions
C---------------------------------------------------
      CALL HM_OPTION_COUNT('/INIVEL',HM_NINVEL)
      NINVEL      = HM_NINVEL
C-----
      CALL HM_OPTION_COUNT('/REFSTA',IREFSTA)
      IS_REFSTA = .FALSE.
      IF(IREFSTA > 0)IS_REFSTA = .TRUE.
C-----            
      CALL HM_OPTION_COUNT('/XREF',NXREF)
C-----            
      CALL HM_OPTION_COUNT('/EREF',NEREF)
C
C---- READING OF /INISTA
      CALL HM_READ_INISTA(S0FILE   ,ISIGI    ,IOUTP_FMT,
     .                    IROOTYY_R,LSUBMODEL)
C
      IRFE=IRFORM/5
      IRFL=IRFORM-5*IRFE
      IRFORM=5*IRFE+IRFL
      IF (IRFORM /= 12) THEN     
        CALL ANCMSG(MSGID=636,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1)
        IRFORM = 12
      ENDIF
C
C---- READING OF /REFSTA
      CALL HM_READ_REFSTA(LSUBMODEL, XRFILE)
C-----
      CALL HM_OPTION_COUNT('/INIGRAV',NINIGRAV)
C----- 
      CALL HM_OPTION_COUNT('/INIMAP1D', NINIMAP1D)
      CALL HM_OPTION_COUNT('/INIMAP2D', NINIMAP2D)
C-----
      CALL HM_OPTION_COUNT('/INIVOL' ,NINIVOL)
C---------------------------------------------------
C     Thermal FE options 
C---------------------------------------------------
      CALL HM_OPTION_COUNT('/INITEMP',NINTEMP)
      CALL HM_OPTION_COUNT('/IMPTEMP',NIMTEMP)
      CALL HM_OPTION_COUNT('/IMPFLUX',NIMPFLUX)
      CALL HM_OPTION_COUNT('/CONVEC',NCONVEC)
      CALL HM_OPTION_COUNT('/RADIATION',NRADIA)
C---------------------------------------------------
C     SPH
C---------------------------------------------------
      CALL HM_OPTION_COUNT('/SPHCEL',NUMSPH)

      NSELSP = 0
      CALL HM_OPTION_COUNT('/SPHBCS',NSPCOND)
      NSPHSYM= 0
      MAXPJET= 0
C
c call NBSPH only once NSPMD is known in order to compute NSPHRES=NSPHRES*NSPMD to prepare NSPHRES by proc 
c for inlets treatment in SPMD
c NSPHRES is the global number of SPH reserve
C     NBSPH computes NSPHIO & NSPHRES :
      CALL NBSPH(LSUBMODEL)
C
C     add NSPHRES number of SPH reserve for inlet
      NUMNOD=NUMNOD+NSPHRES
      NUMSPH=NUMSPH+NSPHRES
C---------------------------------------------------
C     MADYMO
C---------------------------------------------------
      NEXMAD =0
      NCONX =0
      CALL HM_OPTION_COUNT('/MADYMO/EXFEM', NEXMAD)
      CALL HM_OPTION_COUNT('/MADYMO/LINK', NCONX)
      IF (NCONX > 0)  CALL ANCMSG(MSGID   = 2023    ,
     .                            MSGTYPE = MSGERROR,
     .                             ANMODE  = ANINFO  )       
C---------------------------------------------------
      NEIG = 0
      CALL HM_OPTION_COUNT('/EIG',NEIG)
      IF (NEIG>0) IPARI0 = 0
C---------------------------------------------------
      NFLOW = 0
      CALL HM_OPTION_COUNT('/BEM/FLOW', NFLOW)
      IDUMMY = 0
      CALL HM_OPTION_COUNT('/BEM/DAA', IDUMMY)
      NFLOW = NFLOW + IDUMMY
C---------------------------------------------------
C     DFS, ALE, EULER
C---------------------------------------------------
      FLG_FSI = 0    
      IS_BOUNDARY_MATERIAL = .FALSE.  
C-----
      CALL HM_OPTION_COUNT('/EBCS',NEBCS)
C---------------------------------------------------
! Counting /DFS
C---------------------------------------------------
      DETONATORS%NDETPS   = 0
      DETONATORS%NDETSG   = 0
      DETONATORS%NECRAN   = 0
      DETONATORS%NDETPL   = 0
      DETONATORS%NDETCORD = 0
      CALL HM_OPTION_COUNT('/DFS/DETPOIN',DETONATORS%NDETPS) 
      CALL HM_OPTION_COUNT('/DFS/DETLINE',DETONATORS%NDETSG)   
      CALL HM_OPTION_COUNT('/DFS/WAV_SHA',DETONATORS%NECRAN)
      CALL HM_OPTION_COUNT('/DFS/DETPLAN',DETONATORS%NDETPL)   
      CALL HM_OPTION_COUNT('/DFS/DETCORD',DETONATORS%NDETCORD)
      DETONATORS%NDET=DETONATORS%NDETPS+DETONATORS%NDETSG+DETONATORS%NECRAN+DETONATORS%NDETPL+DETONATORS%NDETCORD
C---------------------------------------------------
! /DFS/LASER
C---------------------------------------------------
      NLASER = 0
      CALL HM_OPTION_COUNT('/DFS/LASER',NLASER)
C---------------------------------------------------
      JEUL=0
      CALL HM_OPTION_COUNT('/EULER/MAT', JEUL)
      JEUL=MIN(JEUL,1)
      LVEUL=32
      IF(INTEG8==1)LVEUL=52
C---------------------------------------------------
      JALE=0
      CALL HM_OPTION_COUNT('/ALE/MAT', JALE)
      JALE=MIN(JALE,1)
C---------------------------------------------------
      NALEBCS = 0
      CALL HM_OPTION_COUNT('/ALE/BCS', NALEBCS)
      IF(ICAA == 1) NWALE = -1
      DT_INPUT = ZERO
!     *******************************     !
!     /ALE/GRID: ALE grid formulation     !
!     *******************************     !
      CALL HM_READ_ALE_GRID(DT_INPUT, ALPHA, GAMMA, VGX, VGY, VGZ, VOLMIN, LSUBMODEL, UNITAB)

!     ******************************************************************     !
!     /ALE/MUSCL: activation of second order methods for LAW51 or LAW151     !
!     ******************************************************************     !
      CALL HM_READ_ALE_MUSCL(LSUBMODEL, UNITAB)

!     ***********************     !
!     /ALE/LINK/VEL: counting     !
!     ***********************     !
      CALL HM_OPTION_COUNT('/ALE/LINK/VEL', NALELK)

!     ****************     !
!     /ALE/SOLVER/FINT     !
!     ****************     !
      CALL HM_READ_ALE_SOLVER(LSUBMODEL, UNITAB, ICAA, ISFINT)

!     -------     !
!     /UPWIND     !
!     -------     !
      CALL HM_READ_UPWIND(JUPW, UPWMG, UPWOG, UPWSM, LSUBMODEL, UNITAB)
C---------------------------------------------------
C   OPTIONS DE L'ENGINE LUES DANS LE STARTER
C---------------------------------------------------
      CALL HM_OPTION_COUNT('/RUN',NRUN_ENG)
      CALL HM_OPTION_COUNT('/ANIM',NANIM_ENG)
      CALL HM_OPTION_COUNT('/TFILE',NTFILE_ENG)
      CALL HM_OPTION_COUNT('/RFILE',NRFILE_ENG)
      CALL HM_OPTION_COUNT('/DT',NDT_ENG)
      CALL HM_OPTION_COUNT('/STOP',NSTOP_ENG)
      CALL HM_OPTION_COUNT('/VERS',NVERS_ENG)
      NGINE=NRUN_ENG+NANIM_ENG+NTFILE_ENG+NRFILE_ENG+
     +      NDT_ENG+NSTOP_ENG+NVERS_ENG
C---------------------------------------------------
      NDSOLV=0
C----------------------------------------------------------
C     Anim version 5 non encore supporte
      ANIM_VERS  = 44
      NANIM_VERS = 0
      CALL HM_OPTION_COUNT('/ANIM/VERS',NANIM_VERS)
      IF (NANIM_VERS > 0) THEN 
        CALL HM_OPTION_START('/ANIM/VERS')
        CALL HM_OPTION_NEXT()
        CALL HM_GET_INTV('Anim_vers', ANIM_VERS, IS_AVAILABLE, LSUBMODEL)
      ENDIF
C=======================================================================
C     Some initializations ... (some flags / values need a 1st pre-reading of some options)
C=======================================================================
      IFORM8 = 2
      DTINI = ZERO
      DTFAC = ZERO
      DTHIS = ZERO
      DO I = 1,10
        DTABF(I) = EP30
        DTABFWR(I) = EP30
      ENDDO
      DTMIN = ZERO
      IHSH  = 0
      DO I = 1,9
        DTHIS1(I)= 0
      ENDDO
C----------------------------------------------------------
C     solide avec rotation
C----------------------------------------------------------
      KROT = 0
      CALL HM_PREREAD_INIVEL(KROT,LSUBMODEL)
      IRODDL=MIN(1,NUMELC+NUMELP+NRBODY+NUMELR+NUMELTG+NGJOINT+NUMMPC+NFXBODY+NUMELX+KROT)
      IRODDL0 = 0
      IISROT = 0
      !for SIN initialization and IN allocation in lectur.F
      CALL HM_OPTION_START('MATERIAL')
      DO I = 1, HM_NUMMAT
         MAT_NUMBER    = I 
         CALL HM_OPTION_READ_KEY(LSUBMODEL,KEYWORD2    = KEY   )
         IF (KEY(1:5) == 'LAW13' .OR. KEY(1:5) == 'RIGID') IRODDL0 = 1
         IF (KEY(1:5) == 'LAW68' .OR. KEY(1:5) == 'COSSE') IISROT = 1
      ENDDO
C--------------------------------------------
C     PRELECTURE DES PIDS pour triangles SH3N6 et rotation solides 
C     PRELECTURE DES PIDS, PARTS & SOLIDS pour Solids TO SPH
C--------------------------------------------
      ALLOCATE(IGEO(NPROPGI*NUMGEO),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                           C1='IGEO')
      IGEO=0
      NUMELTG6 = 0
      NSPHSOL  = 0
      ALLOCATE(NSUB (NUMGEO + NUMSTACK) ,STAT=stat)
      ALLOCATE(NISUB(NUMGEO + NUMSTACK) ,STAT=stat)
      ALLOCATE(NPLY (NUMGEO + NUMSTACK) ,STAT=stat)
c      
      CALL HM_PREREAD_PROPERTIES(IGEO,NSPHSOL,NPLY,NSUB,NISUB,LSUBMODEL)

      IF(NSPHSOL/=0.AND.NUMELS8/=0)THEN
        ALLOCATE(IPART(LIPART1*NPART),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='IPART')
        IPART=0
        CALL HM_PREREAD_PART(IPART,IGEO,LSUBMODEL)
C
        ALLOCATE(IXS(NIXS*NUMELS8),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='IPART')
        CALL HM_PRELCE16S(IPART,IGEO,IXS,NSPHSOL,LSUBMODEL,
     .                    IS_DYNA   )
C
        IREST_MSELT=1
C
      ENDIF
      NUMNOD=NUMNOD+NSPHSOL
      NUMSPH=NUMSPH+NSPHSOL
C-----
      IF(NUMSPH/=0)THEN
        CALL HM_READ_SPHGLO(LSUBMODEL)
      ENDIF
C----------------------------------------------------------
      CALL CONTRBE2(ICR,LSUBMODEL)
      IF(IISROT==1.OR.ICR>0)IRODDL = 1
C----------------------------------------------------------
      CALL HM_OPTION_COUNT('/USERWI',USER_WINDOWS%HAS_USER_WINDOW)
      NCPRI=1
      IF(DTINI==ZERO)DTINI=EP06
      IF(VOLMIN==ZERO)VOLMIN=-EP30
      DT2OLD=DTINI/ONEP1
      TT=ZERO
      DT1=ZERO
      DT2=ZERO
C
      T1S= ZERO
      DT2S=ZERO
C
      DTX=ZERO
      TSTOP=ZERO
      DTANIM = ZERO
      TANIM=ZERO
      TANIM_STOP = EP20
      DTOUTP = ZERO
      TOUTP=ZERO
      THIS=ZERO
      TABFIS=ZERO
      TABFWR=ZERO
      ECONTV = ZERO
      TFEXT = ZERO
      REINT = ZERO
      UREINT = ZERO
      HEAT_STORED = ZERO
      HEAT_MECA   = ZERO
      HEAT_CONV   = ZERO
      HEAT_RADIA  = ZERO
      HEAT_FFLUX  = ZERO
      ECONTD = ZERO
      ECONT_CUMU = ZERO

      IRUN=0
      IGER=0
      IPAT=0
      IANIM=0
      IH3D=0
C
      DO I = 1,9
        THIS1(I)= 0
      ENDDO
C      
      N2D = NANALY
      IF(DTFAC==ZERO.AND.N2D/=0)DTFAC=0.67 !0.67 => 0.670000016689301     !ZEP67 => 0.670000000000000     
      IF(DTFAC==ZERO.AND.N2D==0)DTFAC=0.90 !0.90 => 0.899999976158142     !ZEP9  => 0.900000000000000 
C=======================================================================
C     Opening of some input files (.sty, etc...)
C=======================================================================
      IF(ISIGI==1.OR.ISIGI==2) THEN
C--     Lecture fichier S00 supprimee
      ELSEIF(ISIGI==3.OR.ISIGI==4.OR.ISIGI==5) THEN
C--     Lecture fichier Y00/Ynn
        IF(S0FILE==' ') THEN
          ISIGI=-ISIGI
        ELSE
          J = 0
          IF(IROOTYY_R==2)THEN
           DO  I=1,ncharline
            IF(S0FILE(I:I)/=' ')J = J + 1
           ENDDO
           N = J - 3
           TMP_NAME=INFILE_NAME(1:INFILE_NAME_LEN)//S0FILE(1:len_trim(S0FILE))    
           LEN_TMP_NAME = INFILE_NAME_LEN+len_trim(S0FILE)
           IF(S0FILE(N:N)=='Y')THEN
            OPEN(UNIT=IIN4,FILE=TMP_NAME(1:LEN_TMP_NAME),ACCESS='SEQUENTIAL',
     .       FORM='FORMATTED',STATUS='OLD')

            TMP_NAME=INFILE_NAME(1:INFILE_NAME_LEN)//S0FILE(1:N)//'000'    
            LEN_TMP_NAME = INFILE_NAME_LEN+N+3
            OPEN(UNIT=IIN5,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .       ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='OLD')
           ELSE
            OPEN(UNIT=IIN4,FILE=TMP_NAME(1:LEN_TMP_NAME),ERR=100,ACCESS='SEQUENTIAL',
     .        FORM='FORMATTED',STATUS='OLD')
            CALL ANCMSG(MSGID=169,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  C1=S0FILE)
100         CALL ANCMSG(MSGID=2062,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  C1=S0FILE)
           ENDIF 
          ELSE
            J = 0
            I = 1
            DO WHILE(S0FILE(I:I)/=' ')
             J = J + 1
             I = I + 1
            ENDDO
            N = J-3
            TMP_NAME=INFILE_NAME(1:INFILE_NAME_LEN)//S0FILE(1:len_trim(S0FILE))    
            LEN_TMP_NAME = INFILE_NAME_LEN+len_trim(S0FILE)
            IF(S0FILE(N:N+4)=='.sty')THEN
              OPEN(UNIT=IIN4,FILE=TMP_NAME(1:LEN_TMP_NAME),ACCESS='SEQUENTIAL',
     .        FORM='FORMATTED',STATUS='OLD')

              TMP_NAME=INFILE_NAME(1:INFILE_NAME_LEN)//S0FILE(1:N-6)//'_0000.sty'
              LEN_TMP_NAME = INFILE_NAME_LEN+N-6+9
              OPEN(UNIT=IIN5,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .        ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='OLD')    
            ELSE
              OPEN(UNIT=IIN4,FILE=TMP_NAME(1:LEN_TMP_NAME),ERR=200,ACCESS='SEQUENTIAL',
     .          FORM='FORMATTED',STATUS='OLD')
              CALL ANCMSG(MSGID=169,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    C1=S0FILE)
200           CALL ANCMSG(MSGID=2062,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    C1=S0FILE)
            ENDIF  
          ENDIF
        ENDIF
      ENDIF
C---------------------------------------------------
C     COUNT /INI CARDS READ BY HM_READER
C---------------------------------------------------
      CALL HM_OPTION_COUNT('/INISHE',   NB_INISHE)
      CALL HM_OPTION_COUNT('/INISH3',   NB_INISH3)
      CALL HM_OPTION_COUNT('/INIBRI',   NB_INIBRI)
      CALL HM_OPTION_COUNT('/INIQUA',   NB_INIQUAD)
      CALL HM_OPTION_COUNT('/INIBEAM',  NB_INIBEAM)
      CALL HM_OPTION_COUNT('/INITRUSS', NB_INITRUSS)
      CALL HM_OPTION_COUNT('/INISPRI',  NB_INISPRIG)
      IF(ISIGI==0.AND.
     .   (NB_INISHE+NB_INISH3
     .   +NB_INIBRI+NB_INIQUAD
     .   +NB_INIBEAM+NB_INITRUSS
     .   +NB_INISPRIG)/=0) ISIGI=-3
C=======================================================================
C     Writing to 0000.out FILE
C=======================================================================
      WRITE (IOUT,'(//,1X,20A4)')TEXT
      WRITE (IOUT,'(//A/A/,(A,1PG20.13))')TITRE(110),TITRE(111)
      WRITE (IOUT,'(A,I10)')  TITRE(117)(1:57),IPRI,
     .      TITRE(120)(1:57),INVERS_SRC
      WRITE (IOUT,'(A,I10)')
     .  '    NSUBS:   NUMBER OF SUBSETS. . . . . . . . . . . . . .',
     .  NSUBS,
     .  '    NPART:   NUMBER OF PARTS. . . . . . . . . . . . . . .',
     .  NPART
      WRITE (IOUT,'(A,I10)')  TITRE(125)(1:57),NUMMAT-1,
     .      TITRE(135)(1:57),NUMGEO,
     .      TITRE(126)(1:57),NUMNOD,TITRE(127)(1:57),NUMSKW,
     .      TITRE(128)(1:57),NUMBCS
      IF (NALEBCS /= 0)
     .     WRITE (IOUT,'(A,I10)') TITRE(189)(1:57), NALEBCS
      WRITE (IOUT,'(A,I10)')
     .  '    NUMFRAM: NUMBER OF REFERENCE FRAMES . . . . . . . . .',
     .                                                      NUMFRAM
      WRITE (IOUT,'(A,I10)')  
     .      TITRE(131)(1:57),NUMELQ ,
     .      TITRE(191)(1:57),NUMELTRIA,  
     .      TITRE(132)(1:57),NUMELS ,
     .      TITRE(133)(1:57),NUMELC ,
     .      TITRE(134)(1:57),NUMELT ,
     .      TITRE(136)(1:57),NUMELP ,
     .      TITRE(137)(1:57),NUMELR ,
     .      TITRE(180)(1:57),NUMELTG-NUMELTRIA,
     .      TITRE(182)(1:57),NUMELX ,
     .      TITRE(186)(1:57),NUMELIG3D
      WRITE (IOUT,'(A,I10)')
     .'    NUMSPH : NUMBER OF SMOOTH PARTICLES (SPH CELLS) . . .',
     .                                                      NUMSPH
      WRITE (IOUT,'(A,I10)')
     .'    NSPHBCS: NUMBER OF SPH SYMMETRY CONDITIONS. . . . . .',
     .                                                     NSPCOND
      WRITE (IOUT,'(A,I10)')
     .'    NSPHIO : NUMBER OF SPH INLET/OUTLET CONDITIONS. . . .',
     .                                                      NSPHIO
      IF(NSPHRES/=0)THEN
       WRITE (IOUT,'(A,/,A,I10)')
     .'    NSPHRES:NUMBER OF PARTICLES FROM SPH RESERVES AMONG NUMSPH,',
     .'            NUMBER OF NODES FROM SPH RESERVES AMONG NUMNOD. . .',
     .  NSPHRES
      ENDIF
      IF(NSPHSOL/=0)THEN
       WRITE (IOUT,'(A,/,A,I10)')
     .'    NSPHSOL:NUMBER OF PARTICLES & NODES CREATED FROM SOLIDS .',
     .'            (AMONG NUMSPH & NUMNOD) . . . . . . . . . . . . .',
     .  NSPHSOL
      ENDIF
      WRITE (IOUT,'(/(A,I10))')  TITRE(138)(1:57),NFUNCT,
     .'    NGRAV:  NUMBER OF GRAVITY LOADS . . . . . . . . . . .',
     .NGRAV,
     .'    NFUNC2D:  NUMBER OF USER 2D FUNCTIONS . . . . . . . .',
     .NFUNC2D,
     .'    NINIGRV:NUMBER OF INITIAL GRAVITY LOADS . . . . . . .',
     .NINIGRAV,
     .'    NCONLD: NUMBER OF CONCENTRATED LOADS. . . . . . . . .',
     .NCONLD,
     .'    NINVEL: NUMBER OF INITIAL VELOCITIES. . . . . . . . .',
     .NINVEL,
     .'    NPRELD: NUMBER OF PRESSURE LOADS. . . . . . . . . . .',
     .NPRELD,
     .'    NINIMAP1D: NUMBER OF INITIAL 1D MAPPING.. . . . . . .',
     .NINIMAP1D,
     .'    NINIMAP2D: NUMBER OF INITIAL 2D MAPPING.. . . . . . .',
     .NINIMAP2D     
      IF(NPRELOAD>0) THEN
        WRITE (IOUT,'(A,I10)')
     .  '    NPRELOAD: NUMBER OF BOLT PRELOADINGS. . . . . . . . .',
     .  NPRELOAD
      ENDIF
      IF(DETONATORS%NDET > 0)THEN
        WRITE (IOUT,'(/(A,I10))') TITRE(141)(1:57),DETONATORS%NDETPS,
     .                            TITRE(171)(1:57),DETONATORS%NDETSG,
     .                            TITRE(172)(1:57),DETONATORS%NECRAN,
     .                            TITRE(187)(1:57),DETONATORS%NDETCORD,
     .                            TITRE(188)(1:57),DETONATORS%NDETPL
      ENDIF
      IF(NLASER > 0) WRITE (IOUT,'((A,I10))') TITRE(178)(1:57),NLASER
      WRITE (IOUT,'(4X,A,I10)') 
     .  'NUMBER OF ACCELEROMETERS. . . . . . . . . . . . . . .',
     .   NACCELM,
     .  'NUMBER OF SENSORS . . . . . . . . . . . . . . . . . .',
     .   NSENSOR,
     .  'NUMBER OF GAUGES. . . . . . . . . . . . . . . . . . .',
     .   NBGAUGE
      WRITE (IOUT,'(A,I10)') TITRE(146)(1:57),NINTER,
     .      TITRE(147)(1:57),NRWALL,TITRE(148)(1:57),NRBODY,
     .'    NFXBODY: NUMBER OF FLEXIBLE BODIES. . . . . . . . . .',
     .NFXBODY,
     .      TITRE(149)(1:57),NCONX,
     .      TITRE(150)(1:57),NODMAS,TITRE(183)(1:57),NIMPDISP,
     .      TITRE(184)(1:57),NIMPVEL,TITRE(185)(1:57),NIMPACC,
     .      TITRE(152)(1:57),NRIVET,TITRE(153)(1:57),NSECT,
     .      TITRE(155)(1:57),NJOINT
C
      IF(NINTERFRIC > 0) WRITE (IOUT,'(A,I10)') TITRE(190)(1:57),NINTERFRIC
      IF(NALELK>0)WRITE(IOUT,'(A,I10)')
     .'    NALELK: NUMBER OF ALE LINKS. . . . . . . . . . . . . ',
     .NALELK
      WRITE (IOUT, 5051) NACTIV
      WRITE (IOUT, 5052) NDAMP
      WRITE (IOUT, 5053) NGJOINT
      WRITE (IOUT, 5054) NUMMPC
      WRITE (IOUT, 5050) NR2RLNK
      WRITE (IOUT, 5055) NSUBDOM     
      WRITE (IOUT,5000)NVOLU+NMONVOL
      WRITE(IOUT,'(A,I10)')
     .'    NEIG:   NUMBER OF EIGEN AND STATIC MODES PROBLEMS . .',
     .NEIG
      WRITE(IOUT,'(A,I10)')
     .'    NBEM:   NUMBER OF BEM SOLVED PROBLEMS . . . . . . . .',
     .NFLOW
      WRITE(IOUT,'(A,I10)')
     .'    NRBE2:  NUMBER OF RBE2 RIGID ELEMENTS . . . . . . . .',
     .NRBE2
      WRITE(IOUT,'(A,I10)')
     .'    NRBE3:  NUMBER OF RBE3 CONSTRAINT ELEMENTS  . . . . .',
     .NRBE3
      WRITE (IOUT,'(2A/A,I10)')'    INITIAL STRESS FILE =',TRIM(S0FILE),
     . '                                          FLAG ISIGI. . .',
     . ISIGI
C
      IF (IREFSTA/=0) THEN
       WRITE (IOUT,'(2A/A,I10/A,I10)')
     . '    REFERENCE METRIC FILE =',XRFILE,
     . '                                          FLAG IREFSTA. .',
     . IREFSTA,
     . '                               NUMBER OF STEPS , NITRS. .',
     . NITRS
      ENDIF
      
      IF(JALE+JEUL/=0)THEN         
        WRITE(IOUT,'(A,I10)')
     . '    ISFINT: ALE/EULER MOMENTUM INTEGRATION FORMULATION. .',
     . ISFINT
      ENDIF 
      IF(ICAA==1)THEN
        WRITE(IOUT,5380)
      END IF            
      IF(JALE/=0)THEN
        WRITE (IOUT,5100)NWALE
        SELECT CASE (NWALE)
          CASE(0);WRITE (IOUT,5199) ALPHA,GAMMA, VGX,VGY,VGZ,VOLMIN
          CASE(1);WRITE (IOUT,5200) ALPHA,VGX,VGY,VGZ,VOLMIN
          CASE(2);WRITE (IOUT,5300) DT_INPUT, ALPHA,GAMMA,VGX,VGY,VOLMIN
          CASE(3);WRITE (IOUT,5350) 
          CASE(4);WRITE (IOUT,5351) ALPHA,GAMMA,VGX,VGY 
          CASE(5);WRITE (IOUT,5353) ALPHA,NINT(VGX)               
        END SELECT
      ENDIF

      IF(I_ALE_SOLVER /= 0)IALEMUSCL=0 !muscl not compatible with FVM solver for int22
      CALL HM_OPTION_COUNT('/ALE/MUSCL', NALEMUSCL)     
      IF( UPWSM/=ONE)THEN
       IF(NALEMUSCL>0 .OR. IALEMUSCL>0)THEN
         NALEMUSCL=0 
         IALEMUSCL=0     
         !ignore muscl et utiliser upwsm3  (backward compatibility)
         !double check engine file with /ALE/MUSCL/OFF
        ENDIF
      ENDIF
            
      IF(JALE+JEUL/=0)THEN
        IF(IALEMUSCL == 1 .AND. JUPW/=0 ) THEN   !eta3 + muscl(law51) : conflict
           CALL ANCMSG(MSGID=1564, MSGTYPE=MSGWARNING, ANMODE=ANINFO)
        ELSEIF(IALEMUSCL == 0 .OR. JUPW/=0)THEN
          WRITE (IOUT,5360)UPWMG,UPWOG !,UPWSM
        END IF
        IF (IALEMUSCL == 1 .OR. IALEMUSCL==2) THEN
           WRITE (IOUT, 5504) I_BETA,IALEMUSCL-1
        ENDIF
      ENDIF
      
      WRITE (IOUT,'(//,A,I10)')  TITRE(130)(1:57),NANALY
      
      IF(IHBE_DS==0) IHBE_DS = IHBE_DD
      IF(N2D/=0) IHBE_DS = 2
      IF(N2D==0)THEN
        IF(NUMELQ/=0. OR. (NUMELTG/=0.AND.N2D/=0) )THEN
          CALL ANCMSG(MSGID   = 286,
     .                MSGTYPE = MSGERROR,
     .                ANMODE  = ANINFO)
        ENDIF
      ENDIF
C                                                                    012
      WRITE(IOUT,5500)INTEG8,IPARITH,ISUB,MSHSUB,USER_WINDOWS%HAS_USER_WINDOW,IHBE_DS,
     .      ITET4_D,ITET10_D,ISST_DS,ICPRE_D,IHBE_D,ISST_D,ITHK_D,
     .      IPLA_D,ISTR_D,ISHEA_D,INSH,ISH3N_D, NPTS_D, IFRAME_DS
      WRITE(IOUT,5501)ISHFRAM
      IF(ISH3NFRAM>0)WRITE(IOUT,5901)ISH3NFRAM
      IF(JALE/=0 .AND. IALEFVM/=0)WRITE(IOUT,5502)I_ALE_SOLVER
      IF(JALE/=0 .AND. ISFINT/=0) WRITE(IOUT,5503)ISFINT      
      WRITE(IOUT,5700)LAGM_TOL
      WRITE(IOUT,5800)IMAS_DS
      IF ((NUMSPH>0).AND.(NSPHSOL==0)) WRITE (IOUT,5600) SPASORT,LVOISPH,KVOISPH
      IF ((NUMSPH>0).AND.(NSPHSOL>0)) THEN
        IF (ITSOL2SPH==1) WRITE (IOUT,5610) SPASORT,LVOISPH,KVOISPH,ITSOL2SPH
        IF (ITSOL2SPH==2) WRITE (IOUT,5620) SPASORT,LVOISPH,KVOISPH,ITSOL2SPH
      ENDIF
C-------------------------------------------------------------------
      WRITE(IOUT,1000)
      IF(IEXPM==1) THEN
      ELSE IF(IMOT/=0) THEN
        WRITE(IOUT,*)'MEMORY REQUEST NOT EFFICIENT ON THIS COMPUTER'
      ENDIF
      IF(ICRAY<2) THEN
C 
        IPCT=NINT(REEL*100)
c        WRITE(IOUT,3000) LMA,LAM,IPCT
      ELSE IF(ICRAY==2) THEN
C       MasPar Special
c        WRITE(IOUT,3002) LMA,LAM
      ENDIF

      IF(NTHREAD_S>1)THEN
        STR_NBTHREADS = 'THREADS'
      ELSE
        STR_NBTHREADS = 'THREAD'
      ENDIF
      IF(GOT_INSPIRE_ALM == 1)THEN
        WRITE(IOUT,4001) NTHREAD_S,STR_NBTHREADS,NSPMD,NTHREAD,IARCH,
     .                 ARCHN(IARCH),NVSIZ 
      ELSE
        WRITE(IOUT,4000) NTHREAD_S,STR_NBTHREADS,NSPMD,NTHREAD,IARCH,
     .                 ARCHN(IARCH),NVSIZ 
      ENDIF
C
      IF(IGNORE_SPMD==1.OR.IGNORE_THREADS==1)THEN
            WRITE(IOUT,'(A)') ' '
            WRITE(IOUT,'(A)') '    INFO : NUMBER OF SPMD DOMAIN AND THREADS DEFINED IN COMMAND LINE'
            WRITE(IOUT,'(A)') '           /SPMD CARD PARAMETER ARE IGNORED'
      ENDIF
      IF(GOT_VARIABLE==1)THEN
            WRITE(IOUT,'(A)') ' '
            WRITE(IOUT,*)     '    INFO : OMP_NUM_THREADS SET, DEFAULT SETTING IGNORED'
            WRITE(IOUT,*)' '
      ENDIF

      IF(OLD_RSB==1)THEN
            WRITE(IOUT,'(A)') ' '
            WRITE(IOUT,'(A)') '    INFO : /SPMD OPTION, DECTYP=2 '
            WRITE(IOUT,'(A)') '           RSB DOMAIN DECOMPOSITION DEPRECATED, CHANGING TO DEFAULT VALUE'
      ENDIF


      IF (USERL_AVAIL==1)THEN
        WRITE(IOUT,4500)
        IF(GOT_INSPIRE_ALM == 1)THEN
	  WRITE(IOUT,4601) DLIBFILE(1:DLIBFILE_SIZE),DLIBTKVERS
        ELSE
	  WRITE(IOUT,4600) DLIBFILE(1:DLIBFILE_SIZE),DLIBTKVERS
        ENDIF
      ENDIF
C
C--------------------------------------------------------------------
C     DEALLOCATE
C--------------------------------------------------------------------
      IF(ALLOCATED(IXS))  DEALLOCATE(IXS)
      IF(ALLOCATED(IGEO)) DEALLOCATE(IGEO)
      IF(ALLOCATED(IPART))DEALLOCATE(IPART)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--            
 1000 FORMAT(//
     & 4X,'SPEED PARAMETERS  '/
     & 4X,'----------------  '/)
c 2000 FORMAT(
c     & 4X,'MEMORY REQUESTED BY USER  (KWORDS). . . . . . . . . .',I10)
c 3000 FORMAT(
c     & 4X,'MEMORY AVAILABLE FOR INTEGERS . . . . . . . . . . . .',I10/
c     & 4X,'MEMORY AVAILABLE FOR REALS. . . . . . . . . . . . . .',I10/
c     & 4X,'PERCENTAGE OF MEMORY FOR REALS  . . . . . . . . . . .',I10)
c 3002 FORMAT(
c     & 4X,'MEMORY AVAILABLE FOR INTEGERS . . . . . . . . . . . .',I10/
c     & 4X,'MEMORY AVAILABLE FOR REALS. . . . . . . . . . . . . .',I10)
 4000 FORMAT(
     & 4X,'STARTER RUNNING ON. . . . . . . . . . . . . . . . . .',I10,
     .                    ' ',A20/
     & 4X,'NUMBER OF SPMD DOMAINS. . . . . . . . . . . . . . . .',I10/
     & 4X,'NUMBER OF THREADS PER DOMAIN. . . . . . . . . . . . .',I10/
     & 4X,'ARCHITECTURE OPTIMIZATION . . . . . . . . . . . . . .',I10,
     .                    ', ',A20/
     & 4X,'SIZE OF ELEMENT BUFFER. . . . . . . . . . . . . . . .',I10)
 4001 FORMAT(
     & 4X,'SOLVER RUNNING ON . . . . . . . . . . . . . . . . . .',I10,
     .                    ' ',A20/
     & 4X,'NUMBER OF SPMD DOMAINS. . . . . . . . . . . . . . . .',I10/
     & 4X,'NUMBER OF THREADS PER DOMAIN. . . . . . . . . . . . .',I10/
     & 4X,'ARCHITECTURE OPTIMIZATION . . . . . . . . . . . . . .',I10,
     .                    ', ',A20/
     & 4X,'SIZE OF ELEMENT BUFFER. . . . . . . . . . . . . . . .',I10)

 4500 FORMAT(//
     & 4X,'EXTERNAL LIBRARY FOR USERS CODE INTERFACE  '/
     & 4X,'-----------------------------------------  '/)
 4600 FORMAT(
     & 4X,'LIBRARY NAME  . . . . . . . . . . . . . . . . . . . . ',A/
     & 4X,'RADIOSS USERS CODE INTERFACE VERSION  . . . . . . . .',I10)
 4601 FORMAT(
     & 4X,'LIBRARY NAME  . . . . . . . . . . . . . . . . . . . . ',A/
     & 4X,'SOLVER USERS CODE INTERFACE VERSION . . . . . . . . .',I10)
      

 5000 FORMAT(
     & '    NVOLU:  NUMBER OF MONITORED VOLUMES . . . . . . . . .',I10)
 5050 FORMAT(
     & '    NR2RLNK: NUMBER OF EXTERNAL COUPLING LINKS . . . . . ',I10)
 5051 FORMAT(
     & '    NACTIV: NUMBER OF ELEMENT DEACTIVATION GROUPS . . . .',I10)
 5052 FORMAT(
     & '    NDAMP:  NUMBER OF RAYLEIGH DAMPING GROUPS . . . . . .',I10)
 5053 FORMAT(
     & '    NGJOINT: NUMBER OF GEAR TYPE JOINTS . . . . . . . . .',I10)
 5054 FORMAT(
     & '    NUMMPC: NUMBER OF MULTI-POINT CONSTRAINTS . . . . . .',I10)
 5055 FORMAT(
     & '    NSUBDOM: NUMBER OF SUBDOMAINS . . . . . .. . . . . . ',I10)     
 5100 FORMAT(/
     & 4X,'NWALE : CHOICE OF ALE GRID VELOCITY FORMULATION . . .',I10)
 5199 FORMAT(//
     & 4X,'ALE GRID FORMULATION                           '/
     & 4X,'--------------------                           '/
     & 5X,'DONEA GRID VELOCITY COMPUTATION METHOD         '/
     & 5X,'ALPHA   : DONEA COEFFICIENT. . . . . . . . . . ',1PG20.13/
     & 5X,'GAMMA   : GRID VELOCITY LIMITATION FACTOR. . . ',1PG20.13/
     & 5X,'FscaleX : X-GRID VELOCITY SCALE FACTOR . . . . ',1PG20.13/
     & 5X,'FscaleY : Y-GRID VELOCITY SCALE FACTOR . . . . ',1PG20.13/
     & 5X,'FscaleZ : Z-GRID VELOCITY SCALE FACTOR . . . . ',1PG20.13/
     & 5X,'VOLMIN  : MINIMUM VOLUME FOR ELEMENT DELETION. ',1PG20.13)
 5200 FORMAT(//
     & 4X,'ALE GRID FORMULATION                           '/
     & 4X,'--------------------                           '/
     & 5X,'ALTAIR AVERAGE DISPLACEMENT GRID FORMULATION   '/
     & 5X,'UMAX  : MAXIMUM ABSOLUTE GRID VELOCITY . . . . ',1PG20.13/
     & 5X,'CWX   : X GRID VELOCITY SCALE FACTOR . . . . . ',1PG20.13/
     & 5X,'CWY   : Y GRID VELOCITY SCALE FACTOR . . . . . ',1PG20.13/
     & 5X,'CWZ   : Z GRID VELOCITY SCALE FACTOR . . . . . ',1PG20.13/
     & 5X,'VMIN  : MINIMUM VOLUME FOR ELEMENT DELETION. . ',1PG20.13)
 5300 FORMAT(//
     & 4X,'ALE GRID FORMULATION                           '/
     & 4X,'--------------------                           '/
     &5X,'ALTAIR SPRING METHOD FOR GRID VELOCITY COMPUTATION    '/
     &5X,'DT0   : TYPICAL TIME STEP  . . . . . . . . . . . . ',1PG20.13/
     &5X,'DT0*  : EFFECTIVE TIME STEP. . . . . . . . . . . . ',1PG20.13/
     &5X,'GAMMA : NON LINEARITY FACTOR . . . . . . . . . . . ',1PG20.13/
     &5X,'ETA   : DAMPING COEFFICIENT  . . . . . . . . . . . ',1PG20.13/
     &5X,'NU    : SHEAR FACTOR . . . . . . . . . . . . . . . ',1PG20.13/
     &5X,'VOLMIN: MINIMUM VOLUME FOR ELEMENT DELETION. . . . ',1PG20.13)
 5350 FORMAT(//
     & 4X,'ALE GRID FORMULATION                           '/
     & 4X,'--------------------                           '/
     & 5X,'GRID VELOCITY IS NOT COMPUTED (QUASI EULER) ')
 5351 FORMAT(//
     & 4X,'ALE GRID FORMULATION                           '/
     & 4X,'--------------------                           '/
     & 5X,'ALTAIR STANDARD METHOD FOR GRID VELOCITY COMPUTATION '/
     & 5X,'ALPHA : STABILITY FACTOR . . . . . . . . . . . . ',1PG20.13/
     & 5X,'GAMMA : NON LINEARITY FACTOR . . . . . . . . . . ',1PG20.13/
     & 5X,'BETA  : DAMPING COEFFICIENT. . . . . . . . . . . ',1PG20.13/
     & 5X,'LC    : CHARACTERISTIC LENGTH. . . . . . . . . . ',1PG20.13)
 5353 FORMAT(//
     & 4X,'ALE GRID FORMULATION                           '/
     & 4X,'--------------------                           '/
     & 5X,'LAPLACIAN SMOOTHING '/
     & 5X,'LAMBDA:. . . . . . . . . . . . . . . . . . . . . ',1PG20.13/
     & 5X,'NITER :. . . . . . . . . . . . . . . . . . . . . ',I10)          
 5360 FORMAT(//
     & 4X,'ALE UPWIND PARAMETERS                   '/
     & 4X,'---------------------                   '/
     & 5X,'UPWIND FOR MOMENTUM TRANSPORT . . . . .=',1PG20.13/,
     & 5X,'UPWIND FOR OTHER TRANSPORT. . . . . . .=',1PG20.13/)
 5380 FORMAT(/
     & 4X,'CAA FLUID FORMULATION ACTIVATED         ')
 5400 FORMAT(
     & 4X,'alpha M + beta K DAMPING                    '/,
     & 4X,'------------------------                    '/,
     . 4X,'NODE GROUP ID (=0 ALL NODES). . . . . . ',I5/,
     & 5X,'ALPHA . . . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'BETA. . . . . . . . . . . . . . . . . .=',1PG20.13)
 5500 FORMAT(//4X,'ANALYSIS OPTIONS'/
     &         4X,'----------------'//
     & 4X,'INTEG8 : 8 GAUSS POINT CONDENSED INTEGRATION . . . . .',I10/
     & 4X,'IPARITH: PARALLEL ARITHMETIC FLAG (2 OFF, 1 0N). . . .',I10/
     & 4X,'ISUB   : (OLD) SUBCYCLING (1:YES 0:NO) . . . . . . . .',I10/
     & 4X,'MSHSUB : MEMORY ALLOCATION FOR (NEW) SHELLS SUBCYCLING',/
     & 4X,'                                     (1:YES 0:NO). . .',I10/
     & 4X,'IUSERW : GENERAL USER WINDOW FLAG. . . . . . . . . . .',I10/
     & 4X,'ISOLID : DEFAULT BRICK FORMULATION FLAG. . . . . . . .',I10/
     & 4X,'ITET4  : DEFAULT TETRA4 FORMULATION FLAG . . . . . . .',I10/
     & 4X,'ITET10 : DEFAULT TETRA10 FORMULATION FLAG. . . . . . .',I10/
     & 4X,'ISMSTR : DEFAULT BRICK SMALL STRAIN FLAG . . . . . . .',I10/
     & 4X,'ICPRE  : DEFAULT SOLID CONSTANT PRESSURE FLAG. . . . .',I10/
     & 4X,'ISHELL : DEFAULT SHELL FORMULATION FLAG. . . . . . . .',I10/
     & 4X,'ISST   : DEFAULT SHELL SMALL STRAIN FLAG . . . . . . .',I10/
     & 4X,'ITHK   : DEFAULT SHELL THICKNESS VARIATION FLAG. . . .',I10/
     & 4X,'IPLA   : DEFAULT PLANE STRESS PLASTICITY FLAG. . . . .',I10/
     & 4X,'ISTR   : DEFAULT SHELL STRAIN COMPUTATION FLAG . . . .',I10/
     & 4X,'ISHEA  : DEFAULT SHELL SHEAR COMPUTATION FLAG. . . . .',I10/
     & 4X,'INSH   : SHELL INERTIA FLAG. . . . . . . . . . . . . .',I10/
     & 4X,'ISH3N  : DEFAULT 3 NODE SHELL FORMULATION FLAG . . . .',I10/
     & 4X,'NPTS   : SHELL PROPERTIES DEFAULT NUMBER OF           ',/  
     & 4X,'         INTEGRATION POINTS OR LAYERS. . . . . . . . .',I10/
     & 4X,'IFRAME : DEFAULT SOLID FRAME FORMULATION FLAG . . . . ',I10)
 5501 FORMAT(
     & 4X,'ISHFRAM: LOCAL SHELL FRAME DEFINITION. . . . . . . . .',I10)
 5502 FORMAT(
     & 4X,'IALEFVM: FVM FOR ALE MOMENTUM EQUATION.  . . . . . . .',I10) 
 5503 FORMAT(
     & 4X,'ISFINT : INTERNAL FORCES FORMULATION . . . . . . . . .',I10)  
 5504 FORMAT(//
     & 4X,'ALEMUSCL                   '/
     & 4X,'---------------------                   '/
     & 5X,'COMPRESSION COEFFICIENT (BETA). . . . . . . . . . . : ',1PG20.13/  
     & 5X,'FORMULATION FLAG (IFLAG). . . . . . . . . . . . . . : ',I10)
 5700 FORMAT(//
     & 4X,'LAGRANGE MULTIPLIER OPTIONS'/
     & 4X,'---------------------------'/
     & 4X,'LAGM_TOL:CONVERGENCE CRITERION. . . . . . . . . . . .',
     &                                                        1PG20.13)
      RETURN
 5600 FORMAT(//4X,'SPH GLOBAL PARAMETERS'/
     &         4X,'---------------------'//
     & 4X,'ALPHA SORT :SORTING SECURITY COEFFICIENT . . . . . .',
     &                                                      1PG20.13/,
     & 4X,'LNEIGH     :MAXIMUM NUMBER OF COMPUTED NEIGHBOURS. .',I10/,
     & 4X,'NNEIGH     :MAXIMUM NUMBER OF STORED NEIGHBOURS. . .',I10)
 5610 FORMAT(//4X,'SPH GLOBAL PARAMETERS'/
     &         4X,'---------------------'//
     & 4X,'ALPHA SORT :SORTING SECURITY COEFFICIENT . . . . . .',
     &                                                      1PG20.13/,
     & 4X,'LNEIGH     :MAXIMUM NUMBER OF COMPUTED NEIGHBOURS. .',I10/,
     & 4X,'NNEIGH     :MAXIMUM NUMBER OF STORED NEIGHBOURS. . .',I10/,
     & 4X,'ITSOL2SPH  :SOL2SPH PARTICLES ACTIVATION TYPE. . . .',I10/,
     & 4X,'(SOL2SPH PARTICLES ACTIVATION BASED ON PARTS)')
 5620 FORMAT(//4X,'SPH GLOBAL PARAMETERS'/
     &         4X,'---------------------'//
     & 4X,'ALPHA SORT :SORTING SECURITY COEFFICIENT . . . . . .',
     &                                                      1PG20.13/,
     & 4X,'LNEIGH     :MAXIMUM NUMBER OF COMPUTED NEIGHBOURS. .',I10/,
     & 4X,'NNEIGH     :MAXIMUM NUMBER OF STORED NEIGHBOURS. . .',I10/,
     & 4X,'ITSOL2SPH  :SOL2SPH PARTICLES ACTIVATION TYPE. . . .',I10/,
     & 4X,'(SOL2SPH PARTICLES ACTIVATION BASED ON SUBSETS)')
 5800 FORMAT(
     & //4X,'NODAL MASS DISTRIBUTION FLAG . . . . . . . . .',I10)
 5901 FORMAT(
     & 4X,'ISH3NFRAM: OLD LOCAL SH3N FRAME ACTIVATION . . . . . .',I10)
 999  CALL FREERR(1)
      RETURN
      END
Chd|====================================================================
Chd|  FIND_DT1BRICK_ENGINE          source/starter/contrl.F       
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FIND_DT1BRICK_ENGINE()
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr15_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IO_ERR1, NITER
      CHARACTER FILNAM*109, KEYA*80, KEYA2*80

      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=4096) :: TMP_NAME
C-----------------------------------------------
      IO_ERR1=0
      IDTTSH=0
C-----------------------------------------------
C     Lecture des donnees dans le fichier engine
C-----------------------------------------------   
      FILNAM=ROOTNAM(1:ROOTLEN)//'_0001.rad'

      TMP_NAME=INFILE_NAME(1:INFILE_NAME_LEN)//FILNAM(1:ROOTLEN+9)
      LEN_TMP_NAME = INFILE_NAME_LEN+ROOTLEN+9
      OPEN(UNIT=71,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .        ACCESS='SEQUENTIAL',STATUS='OLD',IOSTAT=IO_ERR1)
C
      IF (IO_ERR1/=0) THEN
         FILNAM=ROOTNAM(1:ROOTLEN)//'D01'
         TMP_NAME=INFILE_NAME(1:INFILE_NAME_LEN)//FILNAM(1:ROOTLEN+3)
         LEN_TMP_NAME = INFILE_NAME_LEN+ROOTLEN+3
         OPEN(UNIT=71,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .        ACCESS='SEQUENTIAL',STATUS='OLD',IOSTAT=IO_ERR1)
      ENDIF      

      IF (IO_ERR1==0) THEN
C
 10     READ(71,'(A)',END=20) KEYA
C
        IF(KEYA(1:10)=='/DT1/BRICK') THEN
          IDT1SOL=1
        ELSEIF(KEYA(1:9)=='/DT1TET10') THEN
          NITER=0
          READ(KEYA(11:20),'(I10)',ERR=30) NITER
 30       CONTINUE
          IF(NITER == 0)NITER=1
          IDT1TET10=NITER+1
        ELSEIF(KEYA(1:6)=='/DTTSH') THEN
          IDTTSH=1
        ENDIF
C
        GOTO 10
C
 20     CONTINUE

        CLOSE(71)
C
      ELSE
C
        IF(NUMTETRA4/=0) CALL ANCMSG(MSGID=1589,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_2,
     .              C1=ROOTNAM(1:ROOTLEN)//'_0001.rad',
     .              C2=ROOTNAM(1:ROOTLEN)//'D01')   
C
        IF(NUMELS10/=0) CALL ANCMSG(MSGID=1606,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_2,
     .              C1=ROOTNAM(1:ROOTLEN)//'_0001.rad',
     .              C2=ROOTNAM(1:ROOTLEN)//'D01')   
C
      ENDIF
C------------------------------------------- 
      RETURN
      END
Chd|====================================================================
Chd|  INI_H3DTMAX_ENGINE            source/starter/contrl.F       
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        READ_H3DTMAX_KEY              source/starter/contrl.F       
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        OUTMAX_MOD                    ../common_source/modules/outmax_mod.F
Chd|====================================================================
      SUBROUTINE INI_H3DTMAX_ENGINE(IPARG,IPART,IPARTS,IPARTC,IPARTG,IDDLEVEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INOUTFILE_MOD
      USE OUTMAX_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr15_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: IDDLEVEL
      INTEGER, DIMENSION(NPARG,NGROUP) ,INTENT(IN):: IPARG
      INTEGER ,DIMENSION(LIPART1,NPART),INTENT(IN):: IPART
      INTEGER ,DIMENSION(NUMELS),INTENT(IN):: IPARTS
      INTEGER ,DIMENSION(NUMELC),INTENT(IN):: IPARTC
      INTEGER ,DIMENSION(NUMELTG),INTENT(IN):: IPARTG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, N ,NELC , NELTG , IP , NPRT , IPRT
      INTEGER IH3D,NG,ITY,NFT,IKEY,K_LEN

      INTEGER :: TMAX_IPART(NPART),NKPART(NPART+1,NKEYMAX+1)
      CHARACTER CARTE*ncharline
C-----------------------------------------------
      IF (ALLOCATED(IKEYMAX)) DEALLOCATE(IKEYMAX)
      IF (ALLOCATED(IPART_OK)) DEALLOCATE(IPART_OK)
      ALLOCATE(IKEYMAX(NKEYMAX),IPART_OK(NGROUP,NKEYMAX))
      IKEYMAX =0
      IPART_OK = 0
      LMAX_DIS = 0
      LMAX_VEL = 0
      LMAX_NSIG = 0
      LMAX_NSTRA = 0
      NKPART(1:NPART+1,1:NKEYMAX+1) = 0
C-----------------------------------------------
C     Lecture des donnees dans le fichier engine
C-----------------------------------------------
      IH3D=1
      K_LEN = 19 
      CARTE(1:K_LEN) = '/H3D/ELEM/VONM/TMAX'    
      CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,NKPART(1,IH3D),IPART)
      IF (IKEY >0) THEN
        IKEYMAX(IH3D) = 1
      ELSE
        K_LEN = 20 
        CARTE(1:K_LEN) = '/H3D/SOLID/VONM/TMAX'    
        CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,NKPART(1,IH3D),IPART)
        IF (IKEY >0) IKEYMAX(IH3D) = 1
        CARTE(1:K_LEN) = '/H3D/SHELL/VONM/TMAX'    
        CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,NKPART(1,IH3D),IPART)
        IF (IKEY >0) IKEYMAX(IH3D) = 1
      END IF
      IH3D=2
      K_LEN = 20 
      CARTE(1:K_LEN) = '/H3D/ELEM/SIGEQ/TMAX'    
      CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,NKPART(1,IH3D),IPART)
      IF (IKEY >0) THEN
        IKEYMAX(IH3D) = 1
      ELSE
        K_LEN = 21 
        CARTE(1:K_LEN) = '/H3D/SOLID/SIGEQ/TMAX'    
        CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,NKPART(1,IH3D),IPART)
        IF (IKEY >0) IKEYMAX(IH3D) = 1
        CARTE(1:K_LEN) = '/H3D/SHELL/SIGEQ/TMAX'    
        CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,NKPART(1,IH3D),IPART)
        IF (IKEY >0) IKEYMAX(IH3D) = 1
      END IF
      IH3D=3
      K_LEN = 19 
      CARTE(1:K_LEN) = '/H3D/ELEM/ENER/TMAX'    
      CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,NKPART(1,IH3D),IPART)
      IF (IKEY >0) THEN
        IKEYMAX(IH3D) = 1
      ELSE
        K_LEN = 20 
        CARTE(1:K_LEN) = '/H3D/SOLID/ENER/TMAX'    
        CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,NKPART(1,IH3D),IPART)
        IF (IKEY >0) IKEYMAX(IH3D) = 1
        CARTE(1:K_LEN) = '/H3D/SHELL/ENER/TMAX'    
        CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,NKPART(1,IH3D),IPART)
        IF (IKEY >0) IKEYMAX(IH3D) = 1
      END IF
      IH3D=4
      K_LEN = 19 
      CARTE(1:K_LEN) = '/H3D/ELEM/DAMA/TMAX'    
      CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,NKPART(1,IH3D),IPART)
      IF (IKEY >0) THEN
        IKEYMAX(IH3D) = 1
      ELSE
        K_LEN = 20 
        CARTE(1:K_LEN) = '/H3D/SOLID/DAMA/TMAX'    
        CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,NKPART(1,IH3D),IPART)
        IF (IKEY >0) IKEYMAX(IH3D) = 1
        CARTE(1:K_LEN) = '/H3D/SHELL/DAMA/TMAX'    
        CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,NKPART(1,IH3D),IPART)
        IF (IKEY >0) IKEYMAX(IH3D) = 1
      END IF
      IH3D=5
      K_LEN = 26 
      CARTE(1:K_LEN) = '/H3D/ELEM/TENS/STRESS/TMAX'    
      CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,NKPART(1,IH3D),IPART)
      IF (IKEY >0) THEN
        IKEYMAX(IH3D) = 1
      ELSE
        K_LEN = 27 
        CARTE(1:K_LEN) = '/H3D/SOLID/TENS/STRESS/TMAX'    
        CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,NKPART(1,IH3D),IPART)
        IF (IKEY >0) IKEYMAX(IH3D) = 1
        CARTE(1:K_LEN) = '/H3D/SHELL/TENS/STRESS/TMAX'    
        CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,NKPART(1,IH3D),IPART)
        IF (IKEY >0) IKEYMAX(IH3D) = 1
      END IF
      IH3D=6
      K_LEN = 26 
      CARTE(1:K_LEN) = '/H3D/ELEM/TENS/STRAIN/TMAX'    
      CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,NKPART(1,IH3D),IPART)
      IF (IKEY >0) THEN
        IKEYMAX(IH3D) = 1
      ELSE
        K_LEN = 27 
        CARTE(1:K_LEN) = '/H3D/SOLID/TENS/STRAIN/TMAX'    
        CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,NKPART(1,IH3D),IPART)
        IF (IKEY >0) IKEYMAX(IH3D) = 1
        CARTE(1:K_LEN) = '/H3D/SHELL/TENS/STRAIN/TMAX'    
        CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,NKPART(1,IH3D),IPART)
        IF (IKEY >0) IKEYMAX(IH3D) = 1
      END IF
      IH3D=7 ! NKEYMAX+1
      K_LEN = 9 
      CARTE(1:K_LEN) = '/H3D/PART'    
      CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,NKPART(1,IH3D),IPART)
      IF (IKEY == 0 ) NKPART(1:NPART,IH3D)=1
      IH3D=8 
      K_LEN =18 
      CARTE(1:K_LEN) = '/H3D/NODA/DIS/TMAX'    
      CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,TMAX_IPART,IPART)
      IF (IKEY > 0 ) LMAX_DIS = 3
      IH3D=9 
      K_LEN =18 
      CARTE(1:K_LEN) = '/H3D/NODA/VEL/TMAX'    
      CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,TMAX_IPART,IPART)
      IF (IKEY > 0 ) LMAX_VEL = 3
      IH3D=10 
      K_LEN =18 
      CARTE(1:K_LEN) = '/H3D/NODA/GPS/TMAX'    
      CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,TMAX_IPART,IPART)
      IF (IKEY > 0 ) LMAX_NSIG = 6
      IH3D=11 
      K_LEN =23 
      CARTE(1:K_LEN) = '/H3D/NODA/GPSTRAIN/TMAX'    
      CALL READ_H3DTMAX_KEY(CARTE,K_LEN,IKEY,TMAX_IPART,IPART)
      IF (IKEY > 0 ) LMAX_NSTRA = 6
C
        IH3D = 0
        DO I=1,NKEYMAX
          IH3D = IH3D + IKEYMAX(I)
        END DO
        IF (IH3D>0) THEN
          DO I=1,NKEYMAX
           NKPART(1:NPART,I)=NKPART(1:NPART,NKEYMAX+1)*NKPART(1:NPART,I)
          END DO
C
          DO NG=1,NGROUP
            NFT=IPARG(3,NG)+1
            ITY=IPARG(5,NG)
C          
            IPRT = 0
            SELECT CASE (ITY)
              CASE(1)
                IPRT = IPARTS(NFT)
              CASE(3)
                IPRT = IPARTC(NFT)
              CASE(7)
                IPRT = IPARTG(NFT)
            END SELECT
            IF(IPRT>0) IPART_OK(NG,1:NKEYMAX) = NKPART(IPRT,1:NKEYMAX)
          END DO
        END IF
C------------------------------------------- 
      RETURN
      END
Chd|====================================================================
Chd|  READ_H3DTMAX_KEY              source/starter/contrl.F       
Chd|-- called by -----------
Chd|        INI_H3DTMAX_ENGINE            source/starter/contrl.F       
Chd|-- calls ---------------
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE READ_H3DTMAX_KEY(KEY_TM,KEY_LEN,IFUND,NTM_PART,IPART)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr15_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN):: KEY_LEN
      CHARACTER(len=KEY_LEN) :: KEY_TM
      INTEGER ,DIMENSION(LIPART1,NPART),INTENT(IN):: IPART
      INTEGER, INTENT(OUT):: IFUND
      INTEGER ,DIMENSION(NPART),INTENT(INOUT):: NTM_PART
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, N ,NELC , NELTG , IP , NPRT , IPRT
      INTEGER IH3D,NG,ITY,NFT,IO_ERR1,LEN_C,IC
      CHARACTER FILNAM*109, KEYA*80, KEYA2*80,CARTE*ncharline

      INTEGER :: LEN_TMP_NAME,TMAX_IPART(NPART)
      CHARACTER(len=4096) :: TMP_NAME
C-----------------------------------------------
      IFUND=0
      IO_ERR1 = 0
C-----------------------------------------------
C     Lecture des donnees dans le fichier engine
C-----------------------------------------------   
      FILNAM=ROOTNAM(1:ROOTLEN)//'_0001.rad'

      TMP_NAME=INFILE_NAME(1:INFILE_NAME_LEN)//FILNAM(1:ROOTLEN+9)
      LEN_TMP_NAME = INFILE_NAME_LEN+ROOTLEN+9
      OPEN(UNIT=71,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .        ACCESS='SEQUENTIAL',STATUS='OLD',IOSTAT=IO_ERR1)

      DO WHILE (IO_ERR1 == 0 .AND. IFUND==0)
C
        READ(71,FMT='(A)',END=20) KEYA
        IF(KEYA(1:KEY_LEN) == KEY_TM ) THEN
C---- read ipart_id if there are 
           IFUND = 1
           IC = 1
           NPRT = NPART
           DO WHILE(IC == 1)           
             READ(71,FMT='(A)',ERR=20,END=20) CARTE
             IF(CARTE(1:1) == '#' .OR. CARTE(1:1) == '$') CYCLE
             IC = 0
             LEN_C = LEN_TRIM(CARTE)
             IF (CARTE(1:1) == '/' .OR. LEN_C==0) THEN
             ELSE
               J=1
               NPRT = 0
               DO WHILE(CARTE(1:1) /= '/' .AND. LEN_C /= 0) 
                 DO WHILE (J <= LEN_C)
                  IF(CARTE(J:J) /= ' ') THEN
                    K=J
                    DO WHILE(CARTE(K:K) /= ' ' .AND. K <= LEN_C)
                      K=K+1
                    ENDDO
                    NPRT = NPRT + 1
                    READ(CARTE(J:K),'(I10)') IPRT
                    TMAX_IPART(NPRT) = IPRT
                    J = K
                  ENDIF
                  J = J +1
                 END DO
                 READ(71,FMT='(A)',ERR=20,END=20) CARTE
                 LEN_C = LEN_TRIM(CARTE)
               END DO
     	       IF (NPRT==0) THEN
                 NPRT = NPART
                 NTM_PART(1:NPRT) = 1
               END IF
             END IF
           END DO !(IC == 1) remove comments
        END IF        
C
      END DO
C
 20     CONTINUE

        CLOSE(71)
C        
        IF (IFUND>0 .AND. (NPRT == NPART .OR.NPRT ==0)) THEN   
          NTM_PART(1:NPRT) = 1
        ELSEIF (IFUND>0) THEN   
    	  DO I=1,NPRT
             IP=0
             IPRT = TMAX_IPART(I)
     	     DO J=1,NPART
     	        IF(IPART(4,J)==IPRT)IP=J
     	     END DO
     	     IF (IP > 0) NTM_PART(IP)=1
             IP = 0
     	     DO J=1,NPART
     	       IP = IP +NTM_PART(J)
     	     END DO
             IF (IP==0) NTM_PART(1:NPART) = 1 
          ENDDO
        END IF 
C------------------------------------------- 
      RETURN
      END
