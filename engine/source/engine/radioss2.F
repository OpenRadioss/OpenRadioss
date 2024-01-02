Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  RADIOSS2                      source/engine/radioss2.F      
Chd|-- called by -----------
Chd|        RADIOSS0                      source/engine/radioss0.F      
Chd|-- calls ---------------
Chd|        ALELAG_RREST                  source/output/restart/rdresb.F
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        BCS0                          source/constraints/general/bcs/bcs0.F
Chd|        BUILD_MSG                     source/output/message/build_msg.F
Chd|        CDK6BC3                       source/elements/sh3n/coquedk6/cdk6bc3.F
Chd|        CFIELDREST                    source/output/restart/rdresb.F
Chd|        CHECK_MESSAGE_DEFINITION      ../common_source/modules/check_mod.F
Chd|        CLOSE_C                       ../common_source/tools/input_output/write_routtines.c
Chd|        CONDRMAT                      source/materials/mat/mat013/condrmat.F
Chd|        CONVRREST                     source/output/restart/rdresb.F
Chd|        COQINI                        source/elements/shell/coqini.F
Chd|        CRKXFEM_RREST                 source/output/restart/rdresb.F
Chd|        CRKXFEM_RRESTANIM             source/output/restart/rdresb.F
Chd|        CUR_FIL_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        DELETE_USER_FILE              source/user_interface/dyn_userlib.c
Chd|        DRAPE_RREST                   source/output/restart/rdresb.F
Chd|        DYN_USERLIB_INIT              source/user_interface/dyn_userlib.c
Chd|        FVDEAL                        source/airbag/fvdeal.F        
Chd|        FVRREST                       source/output/restart/rdresb.F
Chd|        FXFLUXRREST                   source/constraints/thermic/fxfluxrrest.F
Chd|        FXTEMPRREST                   source/output/restart/rdresb.F
Chd|        GET_FILE_NAME_INFO            source/system/get_file_name_info.F
Chd|        HIST1                         source/output/th/hist1.F      
Chd|        HIST13                        source/output/th/hist13.F     
Chd|        IMPRREST                      source/output/restart/rdresb.F
Chd|        INICOD                        source/tools/univ/inicod.F    
Chd|        INICONSTANT                   source/system/iniconstant.F   
Chd|        INIPAR                        source/mpi/init/inipar.F      
Chd|        INIRESA                       source/output/restart/rdresa.F
Chd|        INITIME                       source/system/timer.F         
Chd|        INIT_DYN_LIB_STRUC            share/modules/dynlib_mod.F    
Chd|        INIT_TH0                      source/output/th/init_th0.F   
Chd|        INI_MSG                       source/output/message/ini_msg.F
Chd|        INTSTAMP_ZERO                 share/modules/intstamp_mod.F  
Chd|        KNOTLOCELREST                 source/output/restart/rdresb.F
Chd|        KNOTLOCPCREST                 source/output/restart/rdresb.F
Chd|        KNOTREST                      source/output/restart/rdresb.F
Chd|        LECINP                        source/input/lecinp.F         
Chd|        LECTUR                        source/input/lectur.F         
Chd|        LF_CONVERT_C_FLAT             source/engine/lf_convert_c.c  
Chd|        LOADPREST                     source/output/restart/rdresb.F
Chd|        MY_GETPID                     source/system/pid.c           
Chd|        NFRREST                       source/output/restart/rdresb.F
Chd|        NITSCHERREST                  source/output/restart/rdresb.F
Chd|        OPEN_C                        ../common_source/tools/input_output/write_routtines.c
Chd|        OPEN_F_SCRATCH_FILE           source/system/open_tempfile.F 
Chd|        PLYXFEM_RAVUPLY               source/output/restart/rdresb.F
Chd|        PLYXFEM_RREST                 source/output/restart/rdresb.F
Chd|        PLYXFEM_RRESTANIM             source/output/restart/rdresb.F
Chd|        PLY_INFO_REST                 source/output/restart/rdresb.F
Chd|        PRINTCENTER                   source/engine/radioss_title.F 
Chd|        PRINTCPU                      source/system/timer.F         
Chd|        R2R_INPUT_INIT                source/coupling/rad2rad/r2r_input_init.F
Chd|        RADIARREST                    source/output/restart/rdresb.F
Chd|        RADIOSS_TITLE                 source/engine/radioss_title.F 
Chd|        RDRESA                        source/output/restart/rdresa.F
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|        READ_C_C                      ../common_source/tools/input_output/write_routtines.c
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_DPDB                     source/output/tools/read_db.F 
Chd|        READ_UNITS                    ../common_source/comm/write_units.F
Chd|        RESOL_HEAD                    source/engine/resol_head.F    
Chd|        RESTALLOC                     source/output/restart/arralloc.F
Chd|        RMATRREST                     source/output/restart/rdresb.F
Chd|        RTHBUFREST                    source/output/restart/rdresb.F
Chd|        SETIGNORECORE                 source/system/traceback_handler.c
Chd|        SET_STACK_VALUES              source/system/machine.F       
Chd|        SPMD_ALLREDUCE_DB             source/mpi/generic/spmd_allreduce_db.F
Chd|        SPMD_REDUCE_DB                source/mpi/generic/spmd_reduce_db.F
Chd|        SPMD_RST_CHECK                source/mpi/init/spmd_rst_check.F
Chd|        STACK_RREST                   source/output/restart/rdresb.F
Chd|        SWITCH_TO_DTNODA              source/time_step/switch_to_dtnoda.F
Chd|        TABLE_ZERO                    source/tools/curve/table_tools.F
Chd|        THCRREST                      source/output/restart/rdresb.F
Chd|        TH_CLEAN                      ../common_source/modules/output/time_history_mod.F
Chd|        TRACE_IN                      source/system/trace_back.F    
Chd|        TRACE_OUT                     source/system/trace_back.F    
Chd|        UACCESS_DUM                   source/user_interface/uaccess.F
Chd|        UFUNC_DUM                     source/user_interface/ufunc.F 
Chd|        UPIDMID_DUM                   source/user_interface/upidmid.F
Chd|        USENSOR_DUM                   source/user_interface/usensor.F
Chd|        USER_WINDOWS_CLEAN            ../common_source/modules/user_windows_mod.F
Chd|        USRPLAS_DUM                   source/user_interface/usrplas.F
Chd|        UTABLE_DUM                    source/user_interface/utable.F
Chd|        WIGEREST                      source/output/restart/rdresb.F
Chd|        WRITE_TPL_FILE                source/implicit/imp_solv.F    
Chd|        ALEFVM_MOD                    ../common_source/modules/ale/alefvm_mod.F
Chd|        ALEMUSCL_MOD                  ../common_source/modules/ale/alemuscl_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|        CHECK_MOD                     ../common_source/modules/check_mod.F
Chd|        DIFFUSION_MOD                 share/modules/diffusion_mod.F 
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|        DT_MOD                        share/modules/dt_mod.F        
Chd|        DYNLIB_MOD                    share/modules/dynlib_mod.F    
Chd|        EBCS_MOD                      ../common_source/modules/boundary_conditions/ebcs_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        GROUP_MOD                     share/modules/group_mod.F     
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        INTBUFFRIC_MOD                share/modules/restart_mod.F   
Chd|        INTBUFMOD                     share/modules/restart_mod.F   
Chd|        INTBUF_MOD                    share/modules/restart_mod.F   
Chd|        INTERFACES_MOD                ../common_source/modules/interfaces/interfaces_mod.F
Chd|        INTSTAMP_GLOB_MOD             share/modules/intstamp_glob_mod.F
Chd|        LINEAR_SOLVER_MOD             share/modules/linear_solver_mod.F
Chd|        LOADS_MOD                     ../common_source/modules/loads/loads_mod.F
Chd|        MAT_ELEM_MOD                  ../common_source/modules/mat_elem/mat_elem_mod.F
Chd|        MDS_RESTART_MOD               ../common_source/modules/mds_rest.mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules/monvol_struct_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        OUTPUT_MOD                    ../common_source/modules/output/output_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        PYTHON_FUNCT_MOD              ../common_source/modules/python_mod.F
Chd|        RAD2R_MOD                     share/modules/rad2r.F         
Chd|        RBODY_MOD                     share/modules/rbody_mod.F     
Chd|        RESTMOD                       share/modules/restart_mod.F   
Chd|        SCRATCHFILE_MOD               share/modules/scratchfiles_mod.F
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|        SOLTOSPH_MOD                  share/modules/soltosph_mod.F  
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|        STATE_MOD                     ../common_source/modules/state_mod.F
Chd|        TABLE_GLOB_MOD                share/modules/table_glob_mod.F
Chd|        TIME_MOD                      share/modules/time_mod.F      
Chd|        UNITS_MOD                     share/modules/units_mod.F     
Chd|        USER_INTERFACE_MOD            share/modules/user_interface_mod.F
Chd|====================================================================
      SUBROUTINE RADIOSS2(IDATA,MIDATA,RDATA,MRDATA) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFMOD
      USE INTBUF_MOD
      USE DSGRAPH_MOD
      USE RESTMOD
      USE UNITS_MOD
      USE INTSTAMP_GLOB_MOD
      USE TABLE_GLOB_MOD
      USE MESSAGE_MOD
      USE RAD2R_MOD
      USE SOLTOSPH_MOD
      USE STACK_MOD
      USE ALEFVM_MOD
      USE MULTI_FVM_MOD
      USE H3D_MOD
      USE INTBUFFRIC_MOD
      USE GROUP_MOD
      USE GROUPDEF_MOD
      USE MAT_ELEM_MOD
      USE TIME_MOD
      USE PINCHTYPE_MOD
      USE CHECK_MOD
      USE INOUTFILE_MOD
      USE QA_OUT_MOD
      USE ALE_CONNECTIVITY_MOD
      USE DYNLIB_MOD
      USE MONVOL_STRUCT_MOD
      USE DRAPE_MOD
      USE RBODY_MOD
      USE SENSOR_MOD
      USE EBCS_MOD
      USE DIFFUSION_MOD
      USE LINEAR_SOLVER_MOD
      USE MDS_RESTART_MOD
      USE SEGVAR_MOD
      USE SCRATCHFILE_MOD
      USE STATE_MOD
      USE ALEMUSCL_MOD , only:ALEMUSCL_Param
      USE USER_INTERFACE_MOD
      USE ALE_MOD
      USE OUTPUT_MOD
      USE INTERFACES_MOD
      USE DT_MOD
      USE LOADS_MOD
      USE PYTHON_FUNCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "com10_c.inc"
#include      "com_xfem1.inc"
#include      "intstamp_c.inc"
#include      "sphcom.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr02_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "scr13_c.inc"
#include      "scr14_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "scr20_c.inc"
#include      "scr23_c.inc"
#include      "scr_fac_c.inc"
#include      "chara_c.inc"
#include      "task_c.inc"
#include      "warn_c.inc"
#include      "parit_c.inc"
#include      "titr_c.inc"
#include      "flowcom.inc"
#include      "impl1_c.inc"
#include      "tabsiz_c.inc"
#include      "sms_c.inc"
#include      "filescount_c.inc"
#include      "rad2r_c.inc"
#include      "userlib.inc"
#include      "drape_c.inc"
#include      "thermal_c.inc"
#include      "inter22.inc"
#include      "couple_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MRDATA,MIDATA
      INTEGER IDATA(*)
      my_real RDATA(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITASK, ITID(PARASIZ), NBTASK(PARASIZ+1),
     .        IFILNAM(2148), IFILNAM_TMP(2148)
      INTEGER ITSTV1, IEXPM, IRUNN, IRFL, IRFE, IRR, IT, JT, I,
     .        NPRELD, NPTS,NNODES,N,LEN, IOS,
     .        I13A,I13B,I13C,I13D,I13E,I13F,I13G,I13H,I76A,I88A,I89A,
     .        I40A,I40B,I40G,I78N,IO_ERR,LEN_G,LEN_M, IAD1, IAD2, IAD3,
     .        IUN,IFUN, ITH, I76ATH(9), IFIL, IO_ERRL,
     .        RADIOSSV,RADFLEXV,RDFLEXCOMP,ABFV,ABFCOMP,IFILABF,
     .        NB_OF_FILES,N_FAIL,IP,IRUNN_BIS,IFILTITL
      INTEGER OMP_GET_THREAD_NUM,LONG,LONG_TMP,
     .        LENV,FVERS,LFNAME,NB_INDEX_ABF
      CHARACTER FILNAM*100, VERSDP*36,CHRUNM1*2, INPUTNAM*100,
     .          CHR_OLD*2,FNAME*2048,IOFF1*3,IOFF2*3,IOFF3*3
      CHARACTER*2048 INAME
      CHARACTER*4 PROCNAM,CHRU_M1

      CHARACTER*10 CPID
      CHARACTER*6 CISPMD
      INTEGER MY_PID

      CHARACTER ABC(26)*1, FILNAMTH*100, FILTH*3, LINE*256,
     .          FILNAMABF*100,FILNAMABF_TMP*100
      DATA ABC/'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'/
  
      INTEGER LHEADER, LVARINT, LVARREA
      INTEGER PRADIOS,PRESFIL
      INTEGER I15A,I15B,I15C,I15D,I15E,I15F,I15G,I15H,I15I,I15J,
     .        I15ATH,ITHFLAG
      INTEGER ICH,MSGID
C Madymo Stuff
      INTEGER IERROR,MADSTRLEN
      CHARACTER MAD_OPTION_STRING*256,RMAD_OPTION_STRING*256
      INTEGER KMADPRT,KMADSH4,KMADSH3,KMADSOL,KMADNOD,KMADFAIL,K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11
      INTEGER BUFTEST(100),NB_MAN,NB_MAN_TOT,TOPINT,FLAG_MAIN,J, ILAW51,ILAW37,ILAW20

      my_real, DIMENSION(:), ALLOCATABLE :: MCP,TEMP
      my_real, DIMENSION(:),ALLOCATABLE :: FORNEQS
      TYPE(PRGRAPH), DIMENSION(:), ALLOCATABLE :: GRAPHE
      INTEGER, DIMENSION(:), ALLOCATABLE :: IFLOW
      CHARACTER*128 MESS1,MESS2,MESS3
      my_real, DIMENSION(:), ALLOCATABLE :: RFLOW
      my_real RNB_INDEX_ABF,RMAXINT
      SAVE ITASK, IRFE, ITID
      SAVE IXDP
      INTEGER MLN,IMID,MMALE,IFAILURE_NXT,FLAG_CST_AMS
      INTEGER  IPPID ,IPMAT,NPT, IPMAT_IPLY ,IPANG,IPTHK  ,IPPOS,IS,IPT

      TYPE (STACK_PLY) :: STACK
      TYPE (MULTI_FVM_STRUCT) :: MULTI_FVM
      TYPE (H3D_DATABASE) :: H3D_DATA

      TYPE (PINCH)  :: PINCH_DATA
      TYPE(DRAPE_), DIMENSION(:), ALLOCATABLE        :: DRAPE_SH4N,DRAPE_SH3N
      TYPE(DRAPEG_)                                  :: DRAPEG
      TYPE(OUTPUT_)                                  :: OUTPUT
      TYPE(PYTHON_)                                  :: PYTHON
C      
      INTEGER :: CURRENT_RUN,PREVIOUS_RUN
      REAL(kind=8) :: LOCAL_STARTER_TIME,LOCAL_ENGINE_TIME,RESULT

      INTEGER :: LEN_TMP_NAME,LEN_TMP_NAME2,BOOL_C
      CHARACTER(len=2048) :: TMP_NAME,TMP_NAME2
      INTEGER :: DYN_LIB_MODE
      INTEGER :: RADFLEX_PROCESS_PID
      INTEGER IUTITLHIS,IUTITLHI(9)
      TYPE (DYNAIN_DATABASE) :: DYNAIN_DATA
      TYPE (INTERFACES_)  :: INTERFACES
c      TYPE (SENSORS_)  :: SENSORS    ! defined as global in user_interface_mod
      TYPE (DT_)       :: DT
      TYPE (LOADS_)    :: LOADS
      TYPE (MAT_ELEM_) :: MAT_ELEM
C----------------------------------------------------------------------------------
C MDS Table
      INTEGER, DIMENSION(:), ALLOCATABLE :: MDS_OUTPUT_TABLE
      INTEGER MAX_DEPVAR
      TYPE(t_ale_connectivity) :: ALE_CONNECTIVITY
      TYPE(MONVOL_STRUCT_), DIMENSION(:), ALLOCATABLE :: T_MONVOL

      TYPE(t_ebcs_tab) :: EBCS_TAB
!      TYPE(T_CG_SOLVER) :: LINEAR_SOLVER
#ifdef MUMPS5
      TYPE(T_MUMPS_SOLVER) :: LINEAR_SOLVER
#else 
      TYPE(T_CG_SOLVER) :: LINEAR_SOLVER
#endif
      TYPE(T_DIFFUSION) :: DIFFUSION
      TYPE(t_segvar) :: SEGVAR
C------------------------------------------------------------------
C to build a IBM PARALLEL version
C In starter : define install parameter nspmd 
C uncomment:
C     In CUPDT3(COQUE) : @PROCESS DIRECTIVE ...
C------------------------------------------------------------------
#include      "build_info.inc"
C------------------------------------------------------------------
#include      "machine.inc"
C------------------------------------------------
C     Init Timer (from beginning of radioss2 (reading timer)
C------------------------------------------------    
      ISTDO=6
      CALL INI_MSG()
      CALL BUILD_MSG()
    
      CALL CHECK_MESSAGE_DEFINITION()
C
      CALL INITIME()
C------------------------------------------------
C   VERSIO(2) used for Time History output
c      DATA VERSIO(2)/'VERSION 5.2main,                    '/
      LENV=LEN_TRIM(VERS)
      WRITE(VERSIO(2),'(A)') '                                    '
      WRITE(VERSIO(2),'(A,A,A)') 'VERSION ',VERS(1:LENV),','
CRVERS for version in MADCL
      RVERS(1:LENV)=VERS(1:LENV)
C------------------------------------------------------------------
C Constant initialization
       CALL INICONSTANT
C------------------------------------------------------------------
      CODVERS= 101
      MSGID  =  38
      
      CALL ALE%init()
C CODVERS : VERSION CODE        = 44 IF RELEASE 4.4
C IMINVER : MINOR VERSION CODE  = 1 IF MINOR RELEASE A(A~1) OF 4.4 RELEASE
C ISRCVER : SOURCE VERSION CODE = 41*100+9
C                                   IF RELEASE WAS BUILT FROM SOURCE
      IMINVER=1
      ISRCVER=1
      ABINP = 0
      ABOUT = 0
      
#include      "machine2.inc"
      
C------------------------------------------------------------------
C     Version number & date
C      To be modified when structure of rst file is modified
C      should be identical to ITESTV in starter
C      ITESTV Format [Version][YY][MM][DD]
C-------------------------------------------------------------------
      ITSTV1=210290720
C
      IF(IR4R8==2) ITSTV1 = -ITSTV1
C------------------------------------------------------------------
C     Compatible radflex number
C         2 digits = equivalent radioss version
C         2 digits = release year
C         2 digits = release month
C         2 digits = release day
C------------------------------------------------------------------
C   D    place in machine.inc      RDFLEXCOMP = 50060828
C------------------------------------------------------------------
      INERI=0
      IPLAST=1
      IIN=8
      IOUT=0
      ISOLV=0
      ISTDI=5
      IUGEO=4
!       ----------------------------------------
!       in order to open a file, need a FILE 
!       pointer
!       FILE pointer already used :
!       0-->4
!       20 : reopen the _0001.rst in order to
!            add the real elapsed time
!       31-->41
!       42-->42+n for sectio (but I don't known
!       the value of n)
!       ----------------------------------------
      IUHIS=3
      IUHI(1)=31
      IUHI(2)=32
      IUHI(3)=33
      IUHI(4)=34
      IUHI(5)=35
      IUHI(6)=36
      IUHI(7)=37
      IUHI(8)=38
      IUHI(9)=39
      ICHECKD = 40
      ICHECKR = 41
      IUINIMAP=21
      IUREE=22
      IFICM=23
      IFICM2=24
      IFXM=25
      IFXS=26
      IEIGM=27
      IUSC1=9
      IUSC2=30
      IUSC3=50
      IUSC4=70
      IUNOI=10
      IDBG5=67
      IDBG8=68
      NODADT=0
      IFICDS=103
      NIXX=5
      NIXIG3D=15
      NANIM2D=0
      NANIM3D=0
      NRWLP = 26
      NBVELP = 6
      NRVOLU=24
      LFACLOAD =13
      LKJNI = 6
      LKJNR = 19
      KWASPH = 16
      NEXMAD=0
      NMADPRT=0
      NMADSH4=0
      NMADSH3=0
      NMADSOL=0
      NMADNOD=0
      NDSOLV=0
      NBUCK=0
      NRBE3L = 10
      NRBE2L = 11
      NSMSPCG=0
      IUDYNAIN = 19833333
      ALEMUSCL_Param%I_MUSCL_OFF = 0
      IUTITLHIS=110
      IUTITLHI(1)=110
      IUTITLHI(2)=111
      IUTITLHI(3)=112
      IUTITLHI(4)=113
      IUTITLHI(5)=114
      IUTITLHI(6)=115
      IUTITLHI(7)=116
      IUTITLHI(8)=117
      IUTITLHI(9)=118
      USE_IFXM = 0
      USE_IFXS = 0
      USE_IEIGM = 0
C--------------------------------------------------
C Initialize ProcessID variable for scratch filesqq
      CALL MY_GETPID(MY_PID)
C--------------------------------------------------
C Radioss userlibraries initialization
C--------------------------------------------------
        IF(GOT_USERL_ALTNAME==1)THEN
          DLIBFILE(1:LEN_USERL_ALTNAME)=USERL_ALTNAME(1:LEN_USERL_ALTNAME)
          DLIBFILE_SIZE=LEN_USERL_ALTNAME
        ELSE
          DLIBFILE='libraduser_'
          DLIBFILE_SIZE=LEN_TRIM(DLIBFILE)
        ENDIF
        USERL_AVAIL=0
        DLIB_ARRAY(1:NBR_DLIB) = 0
#ifndef SANITIZE 
        CALL DYN_USERLIB_INIT(DLIBFILE,DLIBFILE_SIZE,USERL_AVAIL,DLIBTKVERS,IRESP,GOT_USERL_ALTNAME,
     1                        DLIB_ARRAY)
#endif
        DYN_LIB_MODE = 1
        CALL INIT_DYN_LIB_STRUC(DYN_LIB_MODE)

C Linux Issue call dummy routines to avoid loader removing object files
        IERR=0
        CALL USRPLAS_DUM(IERR)
        CALL UTABLE_DUM(IERR)
        CALL USENSOR_DUM(IERR)
        CALL UPIDMID_DUM(IERR)
        CALL UFUNC_DUM(IERR)
        CALL UACCESS_DUM(IERR)
C--------------------------------------------------
C Radioss MDS initialization
C--------------------------------------------------
#ifndef SANITIZE
#ifdef DNC
        CALL MDS_USERLIB_INIT(IRESP,MDS_AVAIL,MDS_VER,MDS_PATH,MDS_PATH_LEN)
#else
        MDS_AVAIL=0
#endif
#endif
C--------------------------------------------------
C Error messages initialization
C--------------------------------------------------
C Common initialization for arrays size
      TOTALFILECOUNT = 0
      ANIMTOTALSIZE = 0
      THFILESIZE = 0
      MULTITHFILESIZE = 0
      OUTPFILESIZE = 0
      MULTIRESTS = 0
      RESTARTFILESIZE = 0
      BCSFILESIZE = 0
      MUMPSFILESIZE = 0
      H3DTOTALSIZE = 0
C--------------------------------------------------
      IF (GOT_INPUT == 1) THEN
C Get input numer & run number

        CALL GET_FILE_NAME_INFO(INPUT,LENINPUT,
     *                          ROOTN,LENROOTN,
     *                          RUNN,FVERS)

C Open input file for reading
         ISTDI = 80
         INPUTNAM(1:LENINPUT)=INPUT(1:LENINPUT)
         IF (GOT_PATH==1)THEN
           FNAME=PATH(1:LENPATH)//INPUTNAM(1:LENINPUT)
         ELSE
           FNAME(1:LENINPUT) = INPUTNAM(1:LENINPUT)
         ENDIF
         LFNAME=LENINPUT+LENPATH

C Save Filename with path for use in user library
         RAD_INPUTNAME(1:LFNAME)=FNAME(1:LFNAME)
         LEN_RAD_INPUTNAME=LFNAME
c
        TMP_NAME(1:2048)  =''
        LEN_TMP_NAME = INFILE_NAME_LEN
        TMP_NAME=INFILE_NAME(1:INFILE_NAME_LEN)
        TMP_NAME2(1:2048)  =''
        LEN_TMP_NAME2 = OUTFILE_NAME_LEN
        TMP_NAME2=OUTFILE_NAME(1:OUTFILE_NAME_LEN)
        BOOL_C = 0
        IF(OUTFILE_BOOL) BOOL_C = 1

        CALL LF_CONVERT_C_FLAT(GOT_INPUT,ROOTN,LENROOTN,FNAME,LFNAME,
     .                         INAME, IERR,1024,LEN_TMP_NAME,TMP_NAME,
     .                         LEN_TMP_NAME2,TMP_NAME2)

        IF (IERR == 0) THEN
                ISTDI = 80
                OPEN(UNIT=ISTDI,FORM='FORMATTED',FILE=INAME(1:LFNAME),
     .               ACTION='READ',STATUS='OLD',IOSTAT=IO_ERR)
        ELSE 
           CALL ANCMSG(MSGID=144,ANMODE=ANINFO,
     .            C1=FNAME(1:LFNAME))
           CALL ARRET(5)
         ENDIF
      ENDIF

C--------------------------------------------------
C initialization bool variables for smp termination
      PARALLEL_END = 0
      PARALLEL_SECTION = 0
C--------------------------------------------------
      CALL INICOD
      CALL COQINI
C--------------------------------------------------
C Get Hardware characteristics before init // (system incompatible with infiniband)
      CALL PRINTCPU(1)
C Init NTHREAD(=NPROC) tasks
      CALL INIPAR(ITID,1,NNODES,INPUTNAM,GOT_INPUT,NBTASK)
C Hardcode KMP_STACKSIZE Environment variable
      CALL SET_STACK_VALUES()
C--------------------------------------------------
      HELAS=HALF
      HVISC=HALF
      HVLIN=ZERO
C--------------------------------------------------
      CALL OPEN_F_SCRATCH_FILE(8,1,ROOTN,LENROOTN,IINFNA)

      IF ( GOT_INPUT == 0)THEN
       IF (ISPMD/=0) THEN
C redirect IOUT to tmp file  fichier temporaire when SPMD on pi <> 0
         IOUT = IUSC4
         CALL OPEN_F_SCRATCH_FILE(IOUT,1,ROOTN,LENROOTN,IUSC4_FNAM)
         LEN_IUSC4_FNAM=LEN_TRIM(IUSC4_FNAM)

         WRITE(UNIT=IOUT,IOSTAT=IOS,FMT='(A)')
         OPEN(UNIT=ISTDI,FORM='FORMATTED',FILE=INPUTNAM,STATUS='OLD',IOSTAT=IO_ERR)
         IF (IO_ERR/=0) THEN
           CALL ANCMSG(MSGID=145,ANMODE=ANINFO,
     .            C1=INPUTNAM)
           CALL ARRET(2)
         ENDIF
       ENDIF
      ELSE
       IF (ISPMD/=0) THEN
         IOUT = IUSC4
         CALL OPEN_F_SCRATCH_FILE(IOUT,1,ROOTN,LENROOTN,IUSC4_FNAM)
         LEN_IUSC4_FNAM=LEN_TRIM(IUSC4_FNAM)
         WRITE(UNIT=IOUT,IOSTAT=IOS,FMT='(A)')
       ENDIF
      ENDIF
C------------------------------------------------
C     Standard input reading
C------------------------------------------------
      CALL LECINP(IRUNN,IRFL,IRFE,H3D_DATA,FLAG_CST_AMS,DYNAIN_DATA,
     .            SENSORS,DT,OUTPUT)
      NFSKYI=5
      IF(KDTINT==0 )NFSKYI=4
      
      IF (GOT_INPUT==0)THEN
C --------------------------------
C when stdi indirection is used need to rebuild Engine input deck
C For dynamical user libraries
C --------------------------------
      WRITE(CHRUN,'(I2.2)')IRUNN
      RAD_INPUTNAME=ROOTNAM(1:ROOTLEN)//'D'//CHRUN
      LEN_RAD_INPUTNAME=LEN_TRIM(RAD_INPUTNAME)
      ENDIF
C------------------------------------------------
C only reading/writing ieee validated for all versions
      IRFORM = 12
C------------------------------------------------
C     Restart
C------------------------------------------------
      IF(ISPMD==ZERO) THEN
C       Create info, hist, monitor, progress and tpl files for writing implicit nonlinear solver information
        IF (IMPL_S>ZERO.AND.ILINE==ZERO.AND.SOLVNFO>ZERO) THEN
          ISOLPGRS = 666666
          FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'.progress'
          LEN_TMP_NAME = OUTFILE_NAME_LEN+ROOTLEN+14
          TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:ROOTLEN+14)          
          OPEN(UNIT=ISOLPGRS,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .         ACCESS='SEQUENTIAL',
     .         FORM='FORMATTED',STATUS='UNKNOWN')
          WRITE(ISOLPGRS,'(I4)') 0     
          WRITE(ISOLPGRS,'(E11.4)') ZERO
          WRITE(ISOLPGRS,'(I4)') 0
          WRITE(ISOLPGRS,'(I4)') 0
          WRITE(ISOLPGRS,'(I4)') 0
          CALL FLUSH(ISOLPGRS)       
          ISOLMNTR = 66666
          FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'.monitor'
          LEN_TMP_NAME = OUTFILE_NAME_LEN+ROOTLEN+13
          TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:ROOTLEN+13)          
          OPEN(UNIT=ISOLMNTR,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .         ACCESS='SEQUENTIAL',
     .         FORM='FORMATTED',STATUS='UNKNOWN')         
          ISOLHIST = 66
          FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_implicit.hist'
          LEN_TMP_NAME = OUTFILE_NAME_LEN+ROOTLEN+19
          TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:ROOTLEN+19)          
          OPEN(UNIT=ISOLHIST,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .         ACCESS='SEQUENTIAL',
     .         FORM='FORMATTED',STATUS='UNKNOWN')
          IF (IDTC==3) THEN
          WRITE(ISOLHIST,1007) 'Cumulative iterations','Cycle',
     .    'Iteration per cycle',
     .    'Residual disp.','Residual force','Residual energy',
     .    'Tolerance disp.','Tolerance force','Tolerance energy',
     .    'Converged step','Diverged step',
     .    'Time','Line search coefficient','Load factor','Arc length' 
          ELSE
          WRITE(ISOLHIST,1003) 'Cumulative iterations','Cycle',
     .    'Iteration per cycle',
     .    'Residual disp.','Residual force','Residual energy',
     .    'Tolerance disp.','Tolerance force','Tolerance energy',
     .    'Converged step','Diverged step',
     .    'Time','Line search coefficient'           
          ENDIF     
1003      FORMAT(A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,
     .           ',',A,',',A,',',A,',',A) 
1007      FORMAT(A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,
     .           ',',A,',',A,',',A,',',A,',',A,',',A)     
          ISOLINFO = 666
          FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_implicit.info'
          INPUTNAM='CHECK_DATA'
          LEN_TMP_NAME = OUTFILE_NAME_LEN+ROOTLEN+19
          TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:ROOTLEN+19)  
          OPEN(UNIT=ISOLINFO,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .         ACCESS='SEQUENTIAL',
     .         FORM='FORMATTED',STATUS='UNKNOWN')    
          ISOLTPL = 6666
          FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_implicit.tpl'
          LEN_TMP_NAME = OUTFILE_NAME_LEN+ROOTLEN+18
          TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:ROOTLEN+18)  
          INPUTNAM='CHECK_DATA'
          OPEN(UNIT=ISOLTPL,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .         ACCESS='SEQUENTIAL',
     .         FORM='FORMATTED',STATUS='UNKNOWN')
          FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_implicit'
          WRITE(ISOLTPL,1004) FILNAM(1:ROOTLEN+14)
          WRITE(ISOLTPL,1005)
          FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_implicit.hist'
          WRITE(ISOLTPL,1006) FILNAM(1:ROOTLEN+19)
          IOFF1 = 'Off'
          IOFF2 = 'Off'
          IOFF3 = 'Off'
          IF (NITOL == 1 .OR. NITOL == 12 .OR. NITOL == 13 .OR. NITOL == 123) IOFF1 = 'On'
          IF (NITOL == 0 .OR. NITOL == 2 .OR. NITOL == 12 .OR. NITOL == 23 .OR. NITOL == 123) IOFF2 = 'On'
          IF (NITOL == 3 .OR. NITOL == 13 .OR. NITOL == 23 .OR. NITOL == 123) IOFF3 = 'On'
          FILNAM=ROOTNAM(1:ROOTLEN)
          CALL WRITE_TPL_FILE(FILNAM(1:ROOTLEN),IOFF1,IOFF2,IOFF3)
1004      FORMAT('*DefineReport(',A,', PLOT_FILE_1)')
1005      FORMAT('    *Id("HyperWorks", "Report")')           
1006      FORMAT('    *Parameter(PLOT_FILE_1, PLOT_FILE_1, FILENAME, "*", "',
     .           A,'")')
        ENDIF
      ENDIF
        IF(IOUT==0)IOUT=7
        WRITE(CHRUN,'(I4.4)')IRUNN
        IF(MCHECK/=0)THEN
         IF(ISPMD==0) THEN
            FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'.out'
            LEN_TMP_NAME = OUTFILE_NAME_LEN+ROOTLEN+9
            TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:ROOTLEN+9)
            INPUTNAM='CHECK_DATA'
            OPEN(UNIT=IOUT,FILE=TMP_NAME(1:LEN_TMP_NAME),ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='UNKNOWN')

             IO_ERRL=0
            DO WHILE (IO_ERRL>=0)
              READ(IOUT,'(A)',END = 1000,IOSTAT=IO_ERRL)LINE
            ENDDO
 1000      BACKSPACE(IOUT)
           WRITE(IOUT,'(1X,A)')FILNAM(1:ROOTLEN+9)
           WRITE(PROCNAM,'(I4.4)')ISPMD
           FILNAM =ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_'//PROCNAM//'.rst'
           LEN = ROOTLEN + 14
         ENDIF
         WRITE(PROCNAM,'(I4.4)')ISPMD+1
         FILNAM =ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_'//PROCNAM//'.rst'
         LEN = ROOTLEN + 14
        ELSE
         IF (ISPMD==0) THEN
           WRITE(CHR_OLD,'(I2.2)')IRUNN
           FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'.out'

           IF (GOT_INPUT == 0)THEN
             INPUTNAM=ROOTNAM(1:ROOTLEN)//'D'//CHR_OLD
             IF(INVERS>=50)
     .       INPUTNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'.rad'
           ELSE
             INPUTNAM(1:LENINPUT)=INPUT(1:LENINPUT)
           ENDIF
           LEN_TMP_NAME = OUTFILE_NAME_LEN+ROOTLEN+9
           TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:ROOTLEN+9)
           OPEN(UNIT=IOUT,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .       ACCESS='SEQUENTIAL',
     .       FORM='FORMATTED',STATUS='UNKNOWN')
C
           WRITE(IOUT,'(1X,A)')FILNAM(1:ROOTLEN+9)
         ENDIF
         WRITE(CHRU_M1,'(I4.4)')IRUNN-1
         WRITE(PROCNAM,'(I4.4)')ISPMD
         FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRU_M1//'_'//PROCNAM
         LEN = ROOTLEN+10
C
         WRITE(CHRU_M1,'(I4.4)')IRUNN-1
         WRITE(PROCNAM,'(I4.4)')ISPMD+1
          FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRU_M1//'_'//PROCNAM
          LEN = ROOTLEN+10
         IF(CHRUN0(1:1)=='_')THEN
           FILNAM=FILNAM(1:LEN)//CHRUN0
           LEN = LEN+2
         ENDIF
         FILNAM=FILNAM(1:LEN)//'.rst'
         LEN = LEN + 4
       ENDIF
       !    ---------------------------------
       !    restarts are read into outfile folder 
       !    even if -inline is used in order to
       !    satisfy the PO :-(
       LEN_TMP_NAME = OUTFILE_NAME_LEN + LEN
       TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN)
       !    ---------------------------------
C
C restart file written with binary format ieee
C
      DO I=1,LEN_TMP_NAME
        IFILNAM(I)=ICHAR(TMP_NAME(I:I))
      END DO
      CALL CUR_FIL_C(0)
      CALL OPEN_C(IFILNAM,LEN_TMP_NAME,1)
      CALL READ_C_C(IFILNAM,LEN)
      DO I=1,LEN
        FILNAM(I:I) = CHAR(IFILNAM(I))
      END DO
C
      IF (ISPMD==0) THEN
        CALL  RADIOSS_TITLE(IOUT ,CPUNAM,ARCHTITLE,
     *                      FILNAM,ROOTLEN,CHRUN,IRESP,0)
        CALL  RADIOSS_TITLE(ISTDO,CPUNAM,ARCHTITLE,
     *                      FILNAM,ROOTLEN,CHRUN,IRESP,1)
      ENDIF
C------------------------------------------------------------------
C     IGNORE CORE
C------------------------------------------------------------------
      IF(ITRACE==1) CALL SETIGNORECORE (1)
      ITASKP1_DEF=0
      CALL TRACE_IN(1,0,ZERO)
      CALL TRACE_IN(17,1,ZERO)
C------------------------------------------------
C     .rst file reading 1
C------------------------------------------------
      CALL RDRESA(ITSTV1 ,LHEADER ,LVARINT ,LVARREA ,PRADIOS ,
     .            PRESFIL, MULTI_FVM, H3D_DATA,
     .            PINCH_DATA,DYNAIN_DATA,INTERFACES,SENSORS,LOADS,
     .            MAT_ELEM )
C------------------------------------------------
C     Test of .rst file compatibility
C------------------------------------------------
      IF(ISPMD==0) THEN
        IF(PRADIOS/=IRADIOS.OR.PRESFIL/=IRESFIL)THEN
          CALL ANCMSG(MSGID=146,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF
      ENDIF

      IF (ISPMD==0) THEN
        IF(ITESTV/=ITSTV1.OR.NVSIZ>MVSIZ)THEN
          CALL ANCMSG(MSGID=147,ANMODE=ANINFO)
          IF(DEBUG(2)<2)CALL ARRET(2)
        ENDIF
      ENDIF

      CALL TRACE_OUT(17)


C      CALL TRACE_IN(19,0,ZERO)
C      IF(ISPMD==0) CALL XTIME0(NNODES)
C      CALL TRACE_OUT(19)

      CALL TRACE_IN(17,1,ZERO)
C end init rdresa after call xtime0
      CALL INIRESA(H3D_DATA,PINCH_DATA)
      CALL TRACE_OUT(17)

      CALL INIPAR(ITID,3,NNODES,INPUTNAM,GOT_INPUT,NBTASK)
      IF(ISPMD==0) THEN

       WRITE(IOUT, '(1X,A,I5)')'NUMBER OF SPMD DOMAINS       ',
     .                           NSPMD
       IF(NBTASK(NSPMD+1) == NSPMD*NTHREAD)THEN
C Default case with same number of THREADS per MPI domain 
        WRITE(IOUT, '(1X,A,I5)')'NUMBER OF THREADS PER DOMAIN ',
     .                           NTHREAD
        WRITE(IOUT, '(1X,A,I5)')'NUMBER OF HMPP PROCESSES     ',
     .                           NSPMD*NTHREAD
        WRITE(ISTDO,'(1X,A,I5)')'NUMBER OF HMPP PROCESSES ',
     .                           NSPMD*NTHREAD
       ELSE
C Specific case with different number of threads per MPI domain
        DO I = 1, NSPMD
          WRITE(IOUT, '(1X,A,I5,A,I5)')'DOMAIN',I,
     .                                 ',  NUMBER OF THREADS ',
     .                                 NBTASK(I)
        END DO
        WRITE(IOUT, '(1X,A,I5)')'NUMBER OF HMPP PROCESSES     ',
     .                           NBTASK(NSPMD+1)
        WRITE(ISTDO,'(1X,A,I5)')'NUMBER OF HMPP PROCESSES ',
     .                           NBTASK(NSPMD+1)
      
       END IF
C Open the qaprint file
       CALL QAOPEN('engine')
      ENDIF
C
      IF (STACKSIZ>0 .AND. STACKSIZ<=512)THEN
          CALL ANCMSG(MSGID=240,
     .        ANMODE=ANINFO)
      ENDIF

      CALL PRINTCPU(2)

      IF(ISPMD==0)
     *  CALL PRINTCENTER(" ",0,IOUT,1)
C------------------------------------------------
      IF(IFIF>0)ALLOCATE (IAF(IFIF),STAT=IADIF)
      IF(MFIF>0)ALLOCATE (AF(MFIF),STAT=IADRF)
      IF(IADRF/=0.OR.IADIF/=0)THEN
        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
C
      IF (IRESP==1) THEN
        SXDP = 3*NUMNOD
        ALLOCATE(XDP(SXDP),STAT=IXDP)
        IF(IXDP/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF
        ALLOCATE(DDP(3*NUMNOD),STAT=IXDP)
        IF(IXDP/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF
      ELSE
        SXDP = 3
        ALLOCATE(XDP(SXDP),STAT=IXDP)
        IF(IXDP/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF
        ALLOCATE(DDP(1),STAT=IXDP)
        IF(IXDP/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF
      ENDIF
      SIGEO = NPROPGI*NUMGEO
      SIPM  = NPROPMI*NUMMAT

      IF (MULTI_FVM%IS_USED) THEN
           MULTI_FVM%MUSCL = ALEMUSCL_Param%IALEMUSCL
      ENDIF

      CALL ALE_CONNECTIVITY%ALE_CONNECTIVITY_INIT()

C------------------------------------------------
C     .rst file reading 2
C------------------------------------------------
      CALL TRACE_IN(17,2,ZERO)
      CALL RESTALLOC(MULTI_FVM,H3D_DATA,PINCH_DATA,ALE_CONNECTIVITY,SEGVAR,LOADS)
      ALLOCATE(T_MONVOL(NVOLU))
      IGEO = 0
      IPM = 0
C
C     allocations structures INTSTAMP
      IF(NINTSTAMP/=0)THEN
        ALLOCATE(INTSTAMP(NINTSTAMP),STAT=IERR)
        IF (IERR/=0)THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF
        CALL INTSTAMP_ZERO(INTSTAMP)
      ELSE
        ALLOCATE(INTSTAMP(0))
      END IF
C
C     allocations structure TABLE
      IF(NTABLE/=0)THEN
        ALLOCATE(TABLE(NTABLE),STAT=IERR)
        IF (IERR/=0)THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF
        CALL TABLE_ZERO(TABLE)
      ELSE
        ALLOCATE(TABLE(0))
      END IF
!     allocation of engine time array
      ALLOCATE(GLOBAL_COMP_TIME%ENGINE_TIME(IRUNN))
c
      GLOBAL_COMP_TIME%ENGINE_TIME(1:IRUNN) = ZERO
      GLOBAL_COMP_TIME%RUN_NBR = IRUNN
      GLOBAL_COMP_TIME%RST_NAME(1:1000)=''
      GLOBAL_COMP_TIME%RST_NAME=
     .   ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_0001.rst'
c
      CALL RDRESB(AF                ,IAF      ,LVARREA      ,XDP             ,IRUNN     , 
     .            MULTI_FVM        ,H3D_DATA  ,PINCH_DATA,
     .            ALE_CONNECTIVITY ,T_MONVOL  ,SENSORS      ,EBCS_TAB        ,DYNAIN_DATA,
     .            USER_WINDOWS     ,OUTPUT    ,INTERFACES   ,LOADS           ,MAT_ELEM, PYTHON)

      IF (NSPMD > 1)  CALL SPMD_RST_CHECK()
c----------

      IF (IMACH==3) NSPROC=1
      ALLOCATE(GRAPHE(0))
      IF (NFLOW>0) THEN
         ALLOCATE(IFLOW(LIFLOW), RFLOW(LRFLOW))
         CALL NFRREST(IFLOW, RFLOW)
      ELSE
         ALLOCATE(IFLOW(0), RFLOW(0))
      ENDIF

      CALL FVRREST(MONVOL)

      IF (IMPL_S0>0) CALL IMPRREST(IRFORM,IMPL_S0)
C heat  transfer
      IF(ITHERM_FE > 0   ) THEN
        ALLOCATE(MCP(NUMNOD),TEMP(NUMNOD))
        CALL THCRREST(MCP,TEMP)
      ELSE
         ALLOCATE(MCP(0),TEMP(0))
      ENDIF
C Nitsche Method : Equivalent Nodal Force allocation
      IF(NITSCHE > 0   ) THEN
        ALLOCATE(FORNEQS(3*NUMNOD))
        CALL NITSCHERREST(FORNEQS)
      ELSE
         ALLOCATE(FORNEQS(0))
      ENDIF
C
      ALLOCATE(DMSPH(SDMSPH),STAT=IERR)
      IF (IERR/=0)THEN
        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
C--------------------------------------------
C     Multimaterial law update (/ANIM/BRIC/VFRAC) (TRIMAT common value unknown in freanim.F since its read after it)
C       Consistency with material law number is also checked here
C
C      283 : Volumetric Fraction 1 (all multimaterial laws, difference is made in dfunc6.F)
C      284 : Volumetric Fraction 2 (all multimaterial laws, difference is made in dfunc6.F)
C      285 : Volumetric Fraction 3 (law51 only)
C      286 : Volumetric Fraction 4 (law51 only)                 
C--------------------------------------------
      !TABVINT not yet read in FREANIM.F so treatment is done here.
      !----------------------------------!
      ! Testing if multimaterial law is  !
      ! linked to a PART                 !
      !  YES: MMALE=1                    !
      !  NO : MMALE=0                    !
      ! +JWL EOS for burn fraction       !      
      !----------------------------------!      
      MMALE = 0
      ILAW37 = 0     
      ILAW51 = 0
      ILAW20 = 0
      DO I=1,NPART
         !IID = IPART(LIPART1*(I-1)+1) !internal ID
         !UID = IPART(LIPART1*(I-1)+5) !user ID
         !MLN = IPM(NPROPMI*(IMID-1)+2)!material law number
         IMID = IPART(LIPART1*(I-1)+1)
         MLN  = IPM(NPROPMI*(IMID-1)+2)
         IF (MLN==51 .OR. MLN==37 .OR. MLN==20 .OR. MLN == 151) MMALE = 1
         IF (MLN==51) ILAW51=1
         IF (MLN==37) ILAW37=1
         IF (MLN==20) ILAW20=1 
      ENDDO
      !----------------------------------!
      !  Updating contour output request !
      !  according multimaterial law type!
      !  and usage                       !
      !----------------------------------!      
      !COMMUN LAW51/LAW37
      !Removing /ANIM/ELEM/VFRAC if no multimaterial law
      IF(MMALE==0)THEN  
        DO I=283,286
          IF(ANIM_SE(I)==1)THEN
            ANIM_SE(I)=0
            NSE_ANI=NSE_ANI-1
          END IF      
        ENDDO 
        DO I=10248,10251
          IF(ANIM_CE(I)==1)THEN
            ANIM_CE(I)=0
            NCE_ANI=NCE_ANI-1
          END IF      
        ENDDO
        !/ANIM/ELEM/VFRAC not relevant in this case
        NN_ANI     = NN_ANI - ANIM_N(20)
        NN_ANI     = NN_ANI - ANIM_N(21)
        NN_ANI     = NN_ANI - ANIM_N(22)
        NN_ANI     = NN_ANI - ANIM_N(23)                         
        ANIM_N(20) = 0
        ANIM_N(21) = 0
        ANIM_N(22) = 0
        ANIM_N(23) = 0                 
      ENDIF

      IF(MMALE==0 .OR. INT22==0)THEN
        !/ANIM/ELEM/ZVFRAC not relevant in this case               
        NN_ANI     = NN_ANI - ANIM_N(24)
        NN_ANI     = NN_ANI - ANIM_N(25)
        NN_ANI     = NN_ANI - ANIM_N(26) 
        NN_ANI     = NN_ANI - ANIM_N(27)                                         
        ANIM_N(24) = 0
        ANIM_N(25) = 0
        ANIM_N(26) = 0
        ANIM_N(27) = 0                       
      ENDIF
      
      IF(INT22==0)THEN 
        !/ANIM/ELEM/ZVOLD  not relevant in this case
        !/ANIM/ELEM/ZVNEW  not relevant in this case       
        NN_ANI     = NN_ANI - ANIM_N(28) 
        NN_ANI     = NN_ANI - ANIM_N(29)       
        ANIM_N(28) = 0
        ANIM_N(29) = 0   
        !/ANIM/VECT/ZVEL  not relevant in this case 
        NV_ANI     = NV_ANI - ANIM_V(21)
        ANIM_V(21) = 0    
        !/ANIM/VECT/ZFVEL  not relevant in this case 
        NV_ANI     = NV_ANI - ANIM_V(22)
        ANIM_V(22) = 0 
        !/ANIM/VECT/ZMOM  not relevant in this case 
        NV_ANI     = NV_ANI - ANIM_V(23)
        ANIM_V(23) = 0       
        !/ANIM/VECT/ZFP  not relevant in this case 
        NV_ANI     = NV_ANI - ANIM_V(24)
        ANIM_V(24) = 0    
        !/ANIM/VECT/ZFINT  not relevant in this case 
        NV_ANI     = NV_ANI - ANIM_V(25)
        ANIM_V(25) = 0                                    
      ENDIF     

      !COMMUN LAW51/LAW37
      !if multimaterial law then removing useless phases output (phases 3 or/and 4)
      IF(MMALE==1)THEN     
          IF(N2D==0)THEN 
            !no 2D output 
          DO I=10248,10251
            IF(ANIM_CE(I)==1)THEN
              ANIM_CE(I)=0
              NCE_ANI=NCE_ANI-1
            END IF      
          ENDDO  
          !multimaterial with law51
          IF(TRIMAT/=4)THEN  !if anim keyword present : removing phasis which are not defined        
            IF(ANIM_SE(286)==1)THEN       !removing phase4 (jwl)
              ANIM_SE(286)=0
              NSE_ANI=NSE_ANI-1
            END IF
          ENDIF
          !multimaterial with law37
          IF(TRIMAT==-2)THEN 
            IF(ANIM_SE(285)==1)THEN       !removing phase3 (only 2 phases with law37 or law20), phase 4 already removed
              ANIM_SE(285)=0
              NSE_ANI=NSE_ANI-1
            END IF      
          ENDIF
          ELSEIF(N2D>0)THEN 
            !no 3D output
          DO I=283,286
            IF(ANIM_SE(I)==1)THEN
              ANIM_SE(I)=0
              NSE_ANI=NSE_ANI-1
            END IF      
          ENDDO 
          !!multimaterial with law51
          !IF(TRIMAT/=4)THEN  !if anim keyword present : removing phasis which are not defined        
          !  IF(ANIM_CE(10251)==1)THEN       !removing phase4 (jwl)
          !    ANIM_CE(10251)=0
          !    NCE_ANI=NCE_ANI-1
          !  END IF
          !0ENDIF  
          !IF(TRIMAT==0)THEN        
          !  DO I=10250,10251
          !    IF(ANIM_CE(I)==1)THEN
          !      ANIM_CE(I) = 0
          !      NCE_ANI    = NCE_ANI-1
          !    END IF      
          !  ENDDO
          !ENDIF          
          ENDIF
      ENDIF
      
      IF(MMALE==1)THEN 
        IF(TRIMAT==-2)TRIMAT=0  !skip law51 loops with law37
      ENDIF
      
      !no need to output zero contour for ALE ANIM keywords if ALE not defined. (example: schlieren +SH3N)
      IF(IALE+IEULER+ITHERM == 0)THEN
        NCE_ANI = NCE_ANI - ANIM_CE(10672)
        NSE_ANI = NSE_ANI - ANIM_SE(4892)
        ANIM_CE(10672) = 0 !quad schlieren
        ANIM_SE(4892)  = 0 !solid schlieren
      ENDIF
            
      !no need to output phase contour if no law51
      IF(ILAW51 == 0)THEN
        !densities
        DO I=4897,4900
          NSE_ANI    = NSE_ANI - ANIM_SE(I)
          ANIM_SE(I) = 0
        ENDDO
        !specific energies
        DO I=4901,4904
          NSE_ANI    = NSE_ANI - ANIM_SE(I)
          ANIM_SE(I) = 0
        ENDDO
        !temperatures
        DO I=4905,4908
          NSE_ANI    = NSE_ANI - ANIM_SE(I)
          ANIM_SE(I) = 0
        ENDDO
        !pressure
        DO I=4909,4912
          NSE_ANI    = NSE_ANI - ANIM_SE(I)
          ANIM_SE(I) = 0
        ENDDO
        !plastic strain
        DO I=4913,4916
          NSE_ANI    = NSE_ANI - ANIM_SE(I)
          ANIM_SE(I) = 0
        ENDDO
        !sound speed
        DO I=4917,4920
          NSE_ANI    = NSE_ANI - ANIM_SE(I)
          ANIM_SE(I) = 0
        ENDDO
        !volumes
        DO I=4922,4925
          NSE_ANI    = NSE_ANI - ANIM_SE(I)
          ANIM_SE(I) = 0
        ENDDO
        !masses
        DO I=4926,4929
          NSE_ANI    = NSE_ANI - ANIM_SE(I)
          ANIM_SE(I) = 0
        ENDDO     
        !artificial viscosities
        DO I=4931,4934
          NSE_ANI    = NSE_ANI - ANIM_SE(I)
          ANIM_SE(I) = 0
        ENDDO       
        !nodal volumetric fraction : no phase 3 & 4
        NN_ANI       = NN_ANI - ANIM_N(22) 
        NN_ANI       = NN_ANI - ANIM_N(23)                         
        ANIM_N(22)   = 0
        ANIM_N(23)   = 0 
        !centroid volumetric fraction : no phase 3 & 4
        NN_ANI       = NN_ANI - ANIM_N(26) 
        NN_ANI       = NN_ANI - ANIM_N(27)                         
        ANIM_N(26)   = 0
        ANIM_N(27)   = 0                          
      ENDIF !IF(ILAW51 == 0)
      
      IF(ILAW37==0)THEN
        !submat densities
        NSE_ANI       = NSE_ANI - ANIM_SE(4935)
        NSE_ANI       = NSE_ANI - ANIM_SE(4936)          
        ANIM_SE(4935) = 0
        ANIM_SE(4936) = 0            
      ENDIF    
                          
      !no need to output phase contour if no law20
      IF(ILAW20 == 0 .AND. ILAW51 == 0)THEN
        DO I= 11890,11925
          NCE_ANI    = NCE_ANI - ANIM_CE(I)
          ANIM_CE(I) = 0
        ENDDO
      ENDIF !IF(ILAW20 == 0)

      IF(ALE%GLOBAL%I_DT_NODA_ALE_ON_KEY==1) ALE%GLOBAL%I_DT_NODA_ALE_ON = 1
                          
C--------------------------------------------
C     UNIT restart info
      CALL READ_UNITS(UNITAB)
C--------------------------------------------
C     rigid material
C
      IF(IRIGID_MAT > 0) CALL RMATRREST(RBYM,IRBYM, LNRBYM,WEIGHT_RM)
C   thermal bc
      IF(NUMCONV > 0)  CALL CONVRREST(IBCV, FCONV)
      IF(NUMRADIA > 0) CALL RADIARREST(IBCR, FRADIA)
      IF(NFXFLUX > 0)  CALL FXFLUXRREST(IBFFLUX, FBFFLUX)
      IF(NFXTEMP > 0)  CALL FXTEMPRREST(IBFTEMP, FBFTEMP)
      IF (IPLYXFEM > 0) THEN
        CALL PLYXFEM_RREST(MS_PLY,ZI_PLY,INOD_PXFEM,IEL_PXFEM,
     .                        ICODE_PLY,ISKEW_PLY,MSZ2)
      ENDIF
      CALL  PLYXFEM_RRESTANIM()
      IF(IPLYXFEM > 0) CALL  PLYXFEM_RAVUPLY()
      IF(NLOADC   > 0) CALL CFIELDREST(CFIELD)
      IF(NLOADP   > 0) CALL LOADPREST(LOADP)
C--------------------------------------------
C LOAD/PRESSURE interfaces : Gaps
C--------------------------------------------
      IF (NINTLOADP>0) THEN
         CALL READ_DB(DGAPLOADINT, NINTER*NLOADP_HYD )
      ENDIF
      IF(ICRACK3D > 0) CALL CRKXFEM_RREST(INOD_CRKXFEM,IEL_CRKXFEM,NODLEVXF)
      CALL CRKXFEM_RRESTANIM()
      IF(ALEFVM_Param%IEnabled  > 0) CALL READ_DB(ALEFVM_Buffer%FCELL ,6*NUMELS)
      IF(IALELAG  > 0) CALL ALELAG_RREST()
C--------------------------------------
      IF(SRTHBUF > 0) CALL RTHBUFREST(RTHBUF,SRTHBUF)
C--------------------------------------------
      IF(SKNOT > 0) CALL KNOTREST(KNOT)
      IF(SKNOTLOCPC > 0) CALL KNOTLOCPCREST(KNOTLOCPC)
      IF(SKNOTLOCEL > 0) CALL KNOTLOCELREST(KNOTLOCEL)
      IF(NUMELIG3D > 0) CALL WIGEREST(WIGE)
      IF(IPART_STACK >0) THEN
          ALLOCATE(STACK%IGEO(4*NPT_STACK+2,NS_STACK),STAT=IERR)
          IF(IERR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='(/PROP/STACK/...)')
            CALL ARRET(2)
          ENDIF
          STACK%IGEO = 0
          ALLOCATE(STACK%GEO(6*NPT_STACK+1,NS_STACK),STAT=IERR)
           IF(IERR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='(/PROP/STACK/...)')
            CALL ARRET(2)
          ENDIF
          STACK%GEO = ZERO
          ALLOCATE(STACK%PM(20,NS_STACK),STAT=IERR)
           IF(IERR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='(/PROP/STACK/...)')
            CALL ARRET(2)
          ENDIF
          STACK%PM = ZERO
        CALL STACK_RREST(STACK%IGEO,STACK%GEO, STACK%PM )
      ELSE
         ALLOCATE(STACK%IGEO(0,0),STACK%GEO(0,0),STACK%PM(0,0))
      ENDIF 
      SCDRAPE = 0
      STDRAPE = 0
      IF (NDRAPE > 0) THEN
         ALLOCATE(DRAPE_SH4N(NUMELC_DRAPE),DRAPE_SH3N(NUMELTG_DRAPE))
         ALLOCATE(DRAPEG%INDX_SH4N(NUMELC),DRAPEG%INDX_SH3N(NUMELTG))
         DRAPEG%INDX_SH4N = 0
         DRAPEG%INDX_SH3N = 0
         CALL DRAPE_RREST(DRAPE_SH4N,DRAPE_SH3N, DRAPEG)  
      ELSE
       ALLOCATE(DRAPE_SH4N(0),DRAPE_SH3N(0),DRAPEG%INDX_SH4N(0),
     .                                      DRAPEG%INDX_SH3N(0) )
      ENDIF 
C--------------------------------------------
      IF (NUMPLY > 0) CALL PLY_INFO_REST(PLY_INFO)
C--------------------------------------------
!       read the starter elapsed time
        CALL READ_DPDB(GLOBAL_COMP_TIME%STARTER_TIME,1)
!       read the previous engine elapsed time
        IF(GLOBAL_COMP_TIME%RUN_NBR>1) THEN
                CALL READ_DPDB(GLOBAL_COMP_TIME%ENGINE_TIME,GLOBAL_COMP_TIME%RUN_NBR-1)
        ENDIF      

C     Close restart file
      CALL CLOSE_C

      IF (NSPMD > 1)  THEN
        PREVIOUS_RUN = GLOBAL_COMP_TIME%RUN_NBR-1
        CALL SPMD_ALLREDUCE_DB(GLOBAL_COMP_TIME%STARTER_TIME,LOCAL_STARTER_TIME,1,"MAX ")
        GLOBAL_COMP_TIME%STARTER_TIME = LOCAL_STARTER_TIME
        IF(PREVIOUS_RUN>0) THEN
            CALL SPMD_ALLREDUCE_DB(GLOBAL_COMP_TIME%ENGINE_TIME(PREVIOUS_RUN),LOCAL_ENGINE_TIME,1,"MAX ")
            GLOBAL_COMP_TIME%ENGINE_TIME(PREVIOUS_RUN) = LOCAL_ENGINE_TIME
        ENDIF
      ENDIF
C--------------------------------------------
C     Automatic element selection for AMS - AMS deactivated if no element selected
      IF(ISMS_SELEC==0.AND.FLAG_CST_AMS==1)THEN
        IDTMINS = 0
        CALL ANCMSG(MSGID=278,ANMODE=ANINFO_BLIND)
      END IF
C
      IF(IDTMINS==1)THEN
        CALL ANCMSG(MSGID=245,ANMODE=ANINFO_BLIND)
        CALL ARRET(2)
      END IF
C
C--------------------------------------------
      IF(IDTMINS==1.OR.(IDTMINS_OLD==1.AND.MCHECK/=0))THEN
        IF(IDTMINS_OLD /= 1)THEN
          ALLOCATE(ADMSMS(NUMNOD),RES_SMS(3*NUMNOD),STAT=IERR)
          IF(IERR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='(/DT/.../AMS)')
            CALL ARRET(2)
          ENDIF
        ENDIF
        IF(IDTMINS_OLD /= 2)THEN
          ALLOCATE(DIAG_SMS(NUMNOD),STAT=IERR)
          IF(IERR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='(/DT/.../AMS)')
            CALL ARRET(2)
          ENDIF
          IF(MCHECK==0)DIAG_SMS(1:NUMNOD)=MS(1:NUMNOD)
        ENDIF
      ELSEIF(IDTMINS==2)THEN
        IF(IDTMINS_OLD /= 2)THEN
          ALLOCATE(DMELC(NUMELC),DMELTG(NUMELTG),DMELS(NUMELS),
     .             DMELTR(NUMELT),DMELP(NUMELP),DMELRT(NUMELR),
     .             DIAG_SMS(NUMNOD),DMINT2(4*I2NSN25),
     .             STAT=IERR)
          IF(IERR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='(/DT/.../AMS)')
            CALL ARRET(2)
          ENDIF
          DMELC (1:NUMELC)=ZERO
          DMELTG(1:NUMELTG)=ZERO
          DMELS (1:NUMELS)=ZERO
          DMELTR(1:NUMELT)=ZERO
          DMELP (1:NUMELP)=ZERO
          DMELRT(1:NUMELR)=ZERO
          DMINT2(1:4*I2NSN25)=ZERO
          DIAG_SMS(1:NUMNOD)=MS(1:NUMNOD)
        ENDIF
        IF(IDTMINS_OLD /= 1 .AND. IDTMINS_OLD /= 2)THEN
          ALLOCATE(RES_SMS(3*NUMNOD),STAT=IERR)
          IF(IERR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='(/DT/.../AMS)')
            CALL ARRET(2)
          ENDIF
        END IF
      ELSEIF(IDTMINS_INT/=0)THEN
        IF(IDTMINS_INT_OLD==0)THEN
          ALLOCATE(RES_SMS(3*NUMNOD),STAT=IERR)
          ALLOCATE(DIAG_SMS(NUMNOD),DMINT2(4*I2NSN25),STAT=IERR)
          IF(IERR/=0) THEN
            CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='(/DT/.../AMS)')
            CALL ARRET(2)
          ENDIF
          DIAG_SMS(1:NUMNOD)=MS(1:NUMNOD)
        END IF
      END IF
      IF(.NOT.ALLOCATED(ADMSMS))ALLOCATE(ADMSMS(0))
      IF(.NOT.ALLOCATED(DMELC ))ALLOCATE(DMELC (0))
      IF(.NOT.ALLOCATED(DMELTG))ALLOCATE(DMELTG(0))
      IF(.NOT.ALLOCATED(DMELS ))ALLOCATE(DMELS (0))
      IF(.NOT.ALLOCATED(DMELTR))ALLOCATE(DMELTR(0))
      IF(.NOT.ALLOCATED(DMELP ))ALLOCATE(DMELP (0))
      IF(.NOT.ALLOCATED(DMELRT))ALLOCATE(DMELRT(0))
      IF(.NOT.ALLOCATED(RES_SMS))ALLOCATE(RES_SMS(0))
      IF(.NOT.ALLOCATED(DIAG_SMS))ALLOCATE(DIAG_SMS(0))
      IF(.NOT.ALLOCATED(DMINT2))ALLOCATE(DMINT2(0))
C--------------------------------------------
      CALL TRACE_OUT(17)
      IRUN=IRUNN
C------------------------------------------------
C     multimat law 151 + Navier Stokes
C------------------------------------------------
#if !defined(MUMPS5)
      !If Viscosity is defined (/MAT/LAW6:NU parameter) with collocated scheme (/MAT/LAW151 , viscosity is solved with linear solver
      !   Consequently if MUMPS is not defined then it will work only with -nspmd=1 
      IF(NSPMD>1 .AND. MULTI_FVM%NS_DIFF)THEN
        WRITE(6,FMT='(A)') " Fatal error: MUMPS is required to solve Navier-Stokes viscosity"
        CALL FLUSH(6)
        CALL ARRET(5)        
       ENDIF
#endif       
C------------------------------------------------
C     Restart indicator for multimat law 151 + symmetry
C------------------------------------------------
        IF (MULTI_FVM%IS_USED) THEN
           MULTI_FVM%IS_RESTART = .TRUE.
           MULTI_FVM%SYM = N2D
           IF (MULTI_FVM%NS_DIFF) THEN
              CALL DIFFUSION%INIT_DIFFUSION(LINEAR_SOLVER, 3, IPARG, ALE_CONNECTIVITY)
           ENDIF
        ENDIF
C------------------------------------------------
C     Check for springs with stiffness but no mass
C     ... Switch for cohesive elements should also be done here ...
C-----------------------------------------------------
      IF(MCHECK==0)THEN
        CALL SWITCH_TO_DTNODA(
     .                    IXR      ,GEO      ,PM       ,IPARG    ,MAT_ELEM%ELBUF,
     .                    MS       ,IN       ,ITAB     ,IGEO     ,IPM    ,
     .                    BUFMAT   ,IPART    ,IGRNOD   ,IGRPART)
      END IF
C------------------------------------------------
C     initializations MDS
C------------------------------------------------
#ifdef DNC
      IF(MDS_AVAIL==1) THEN
        NSPMD_USER = NSPMD
        NTASK_USER = NTHREAD
        ISPMD_USER = ISPMD

        MAX_DEPVAR=0
        DO I=1,MDS_NMAT
           MAX_DEPVAR=MAX(MAX_DEPVAR,MDS_NDEPSVAR(I) )
        ENDDO
        ALLOCATE (MDS_OUTPUT_TABLE(MDS_NMAT*MAX_DEPVAR) ) 

        CALL MDS_ENGINE_USER_INITIALIZE(NSPMD_USER,NTASK_USER,ISPMD_USER,TSTOP,
     *                                  MDS_NMAT,MDS_MATID,MDS_FILES,MDS_LABEL,MDS_NDEPSVAR,MAX_DEPVAR,MDS_OUTPUT_TABLE)
      ENDIF
#endif
C------------------------------------------------
C     engine file reading
C------------------------------------------------
      CALL TRACE_IN(16,2,ZERO)
      CALL LECTUR(  ICODE ,  ISKEW ,  ISKWN ,  IXTG  ,   IXS ,
     2              IXQ ,    IXC ,    IXT ,    IXP ,     IXR ,
     3              ITAB ,   ITABM1  ,NPC,     IPARG,    IGRV ,
     4              LGRAV ,IPARI,
     5              NPBY ,   LPBY    ,ILINK,   LLINK  ,LINALE ,
     6              NEFLSW , NNFLSW , ICUT ,   IAF(IF01),
     7              X  ,     V  ,     VR  ,    MS   ,IN  ,
     8              SKEW  ,TF       ,RBY  ,
     9              WA  ,CRFLSW  ,XCUT  ,ANIN  ,DAMPR  ,
     A              IGRNOD ,KXSP,WEIGHT ,FR_RBY2 ,FR_RL,
     B              PARTSAV,IPART,PM,
     C              MONVOL ,VOLMON, IPART_STATE,GEO,TABLE,
     D              IFRAME,XFRAME,MAT_ELEM%ELBUF,IGEO,INTBUF_TAB,
     E              IPM   ,H3D_DATA, MULTI_FVM,
     F              IGRPART,TAG_SKINS6,ICFIELD,LCFIELD,TAGSLV_RBY,
     G              MDS_LABEL,MDS_OUTPUT_TABLE,MDS_NMAT,MAX_DEPVAR,
     H              MDS_NDEPSVAR,STACK,IBCL,ILOADP,LLOADP,SENSORS,
     I              DYNAIN_DATA,DT ,LOADS ,OUTPUT)

C------------------------------------------------
C Close & Delete scratch file
      CLOSE (UNIT=IIN)
      LEN_IINFNA=LEN_TRIM(IINFNA)
      CALL DELETE_USER_FILE(IINFNA,LEN_IINFNA)
C------------------------------------------------
      CALL TRACE_OUT(16)
C------------------------------------------------
      CALL TRACE_IN(19,0,ZERO)
      CALL TRACE_OUT(19)
C------------------------------------------------
      IF ((IRAD2R==1).AND.(R2R_SIU==1))THEN
        CALL R2R_INPUT_INIT(IRUNN,OUTPUT)          
      ENDIF         
C------------------------------------------------
C     Check NXT FAIL for output 
C------------------------------------------------
      IFAILURE_NXT = 0
       DO N=1,NUMMAT
          N_FAIL = IPM(NPROPMI*(N-1)+220)
          DO J=1,N_FAIL
              IP = (J - 1)*15
             IF(IPM(NPROPMI*(N-1)+111+IP)==25 ) IFAILURE_NXT = 1
          ENDDO
       ENDDO
      IF(IFAILURE_NXT == 1) THEN
            NCE_ANI=NCE_ANI + 3           
           ANIM_CE(10253) = 1
           ANIM_CE(10254) = 1
           ANIM_CE(10255) = 1
        ENDIF
      CALL BCS0 (ICODE,ICODT,ICODR, ICODE_PLY, ICODT_PLY,IBC_PLY)
      IT = 1
      IF (NUMELTG6>0) THEN
        CALL CDK6BC3(ICODT ,ISKEW,ISKWN,IXTG  ,
     1                  IXTG1   ,NPBY,
     2                  X ,SKEW)
      ENDIF
C
C bcs rigid material
C
      IF(IRIGID_MAT > 0)
     .   CALL CONDRMAT(ICODT,ICODR, IRBYM, LNRBYM,ICODRBYM)

C
      CALL TRACE_IN(18,0,ZERO)

C------------------------------------------------
C     ABF file check if external abf program available & initpipes 
C------------------------------------------------
C
C number of written abf_files .abf, _a.abf .....
      IF (ISPMD==0) THEN
C
      NB_OF_FILES = 1
      DO I=1,9
        IF(NTHGRP01(I) /= 0 .AND. ABFILE(I+1) /= 0)
     .           NB_OF_FILES = NB_OF_FILES + 1
      ENDDO
      IF (NABFILE /= 0) THEN
#ifdef DNC
        ABFV = 0
        IABFPIPE=1
        ABFCOMP=180182702
        
        IF((IMACH/=3).OR.(IMACH==3.AND.ISPMD==0)) THEN
        
          CALL OPEN_ABFPIPE(ABINP,ABOUT,IABFPIPE,1,ABFV,NB_OF_FILES)
          
          IF (IABFPIPE < 0) THEN
            SELECT CASE (IABFPIPE)
               CASE (-1)
C---        Error: abf pas trouve
                  CALL ANCMSG(MSGID=150,ANMODE=ANINFO)
               CASE (-2)
                  CALL ANCMSG(MSGID=151,ANMODE=ANINFO,I1=ABFCOMP)
            END SELECT
            CALL CHECK_ABF(ABINP,ABOUT,-1)
            CALL ARRET(2)
C---
          ELSEIF (ABFV < ABFCOMP) THEN
            CALL ANCMSG(MSGID=152,ANMODE=ANINFO)
            CALL CHECK_ABF(ABINP,ABOUT,-1)
            CALL ARRET(2)
          ENDIF
          
          CALL CHECK_ABF(ABINP,ABOUT,IABFPIPE)
          
        ENDIF
#else 
      ! ABF converter is not available
      WRITE(6,'(A)') '*** ERROR /ABF is not available'
      CALL ARRET(2)
#endif
      ENDIF
      ENDIF
C------------------------------------------------
C Write th file structure
C------------------------------------------------
      IF (ISPMD==0) THEN
        IF(TH_VERS>=40)THEN
         WRITE(CHR_OLD,'(I2.2)')IRUNN
         FILNAMTH=ROOTNAM(1:ROOTLEN)//'T'//CHR_OLD
         LONG= 3
         IF(TH_VERS>=50)THEN
          FILNAMTH=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'.thy'
          LONG = 9
         ENDIF
          IUNIT=IUHIS
          IFIL = 1
          IFILTITL = IUTITLHIS
          ITHFLAG = 10
          CALL HIST1( FILNAMTH ,IFIL              ,NTHGRP            ,LONG,
     2                 WA      ,PM                ,GEO               ,IPART,
     3                 SUBSETS ,OUTPUT%TH%ITHGRP  ,OUTPUT%TH%ITHBUF  ,IGEO,
     4                 IPM     ,IPART   ,LIPART1  ,8,
     5                 12      ,ITFORM  ,ITHFLAG ,ITHVAR,
     6                 IFILTITL,OUTPUT%TH%SITHBUF)
          WA(1:SWA) = ZERO
C   Multiple th files
            IF(NTHGRP01(1)/=0)THEN
              WRITE(CHR_OLD,'(I2.2)')IRUNN
              FILTH  = CHR_OLD
              FILTH(3:3)= ABC(1)
              FILNAMTH=ROOTNAM(1:ROOTLEN)//'T'//FILTH
              LONG = 4
              IF(TH_VERS>=50) THEN
               FILNAMTH=ROOTNAM(1:ROOTLEN)
     .             //'_'//CHRUN//'_'//ABC(1)//'.thy'
               LONG = 11
              ENDIF
              IUNIT= IUHI(1)
              IFIL= IUNIT
              IFILTITL = IUTITLHI(1)
              ITHFLAG = 1
              CALL HIST1( FILNAMTH ,IFIL              ,NTHGRP1(1)        ,LONG ,
     2                    WA       ,PM                ,GEO               ,IPART,
     3                    SUBSETS  ,OUTPUT%TH%ITHGRPA ,OUTPUT%TH%ITHBUFA ,IGEO ,
     4                    IPM      ,IPART(1+LIPART1*(NPART+NTHPART)) ,2  ,1    ,
     5                    1       ,AFORM(1)           ,ITHFLAG       ,ITHVAR,
     6                    IFILTITL,OUTPUT%TH%SITHBUFA )
            END IF

            IF(NTHGRP01(2)/=0)THEN
              WRITE(CHR_OLD,'(I2.2)')IRUNN
              FILTH  = CHR_OLD
              FILTH(3:3)= ABC(2)
              FILNAMTH=ROOTNAM(1:ROOTLEN)//'T'//FILTH
              LONG = 4
              IF(TH_VERS>=50) THEN
               FILNAMTH=ROOTNAM(1:ROOTLEN)
     .             //'_'//CHRUN//'_'//ABC(2)//'.thy'
               LONG = 11
              ENDIF
              IUNIT= IUHI(2)
              IFIL= IUNIT
              IFILTITL = IUTITLHI(2)
              ITHFLAG = 2
              CALL HIST1(FILNAMTH,IFIL              ,NTHGRP1(2)        ,LONG,
     2                   WA      ,PM                ,GEO               ,IPART ,
     3                   SUBSETS ,OUTPUT%TH%ITHGRPB ,OUTPUT%TH%ITHBUFB ,IGEO ,
     4                   IPM     ,IPART(1+LIPART1*(NPART+NTHPART)+2*(NPART+NTHPART)),2,1,
     5                   1       ,AFORM(2)          ,ITHFLAG          ,ITHVAR,
     6                   IFILTITL,OUTPUT%TH%SITHBUFB)
            END IF

            IF(NTHGRP01(3)/=0)THEN
              WRITE(CHR_OLD,'(I2.2)')IRUNN
              FILTH  = CHR_OLD
              FILTH(3:3)= ABC(3)
              FILNAMTH=ROOTNAM(1:ROOTLEN)//'T'//FILTH
              LONG = 4
              IF(TH_VERS>=50) THEN
               FILNAMTH=ROOTNAM(1:ROOTLEN)
     .             //'_'//CHRUN//'_'//ABC(3)//'.thy'
               LONG = 11
              ENDIF
              IUNIT= IUHI(3)
              IFIL= IUNIT
              IFILTITL = IUTITLHI(3)
              ITHFLAG = 3
              CALL HIST1(FILNAMTH ,IFIL             ,NTHGRP1(3)        ,LONG,
     2                   WA       ,PM               ,GEO               ,IPART ,
     3                   SUBSETS ,OUTPUT%TH%ITHGRPC ,OUTPUT%TH%ITHBUFC ,IGEO ,
     4                   IPM     ,IPART(1+LIPART1*(NPART+NTHPART)+4*(NPART+NTHPART)) ,2 ,1,
     5                   1       ,AFORM(3)          ,ITHFLAG           ,ITHVAR,
     6                   IFILTITL,OUTPUT%TH%SITHBUFC)
            END IF

            IF(NTHGRP01(4)/=0)THEN
              WRITE(CHR_OLD,'(I2.2)')IRUNN
              FILTH  = CHR_OLD
              FILTH(3:3)= ABC(4)
              FILNAMTH=ROOTNAM(1:ROOTLEN)//'T'//FILTH
              LONG = 4
              IF(TH_VERS>=50) THEN
               FILNAMTH=ROOTNAM(1:ROOTLEN)
     .             //'_'//CHRUN//'_'//ABC(4)//'.thy'
               LONG = 11
              ENDIF
              IUNIT= IUHI(4)
              IFILTITL = IUTITLHI(4)
              IFIL= IUNIT
              ITHFLAG = 4
              CALL HIST1(FILNAMTH,IFIL,NTHGRP1(4),LONG,
     2                   WA  ,PM  ,GEO ,IPART ,
     3                   SUBSETS ,OUTPUT%TH%ITHGRPD ,OUTPUT%TH%ITHBUFD ,IGEO,
     4                   IPM     ,IPART(1+LIPART1*(NPART+NTHPART)+6*(NPART+NTHPART)) ,2 ,1,
     5                   1       ,AFORM(4)          ,ITHFLAG           ,ITHVAR,
     6                   IFILTITL ,OUTPUT%TH%SITHBUFD)
            END IF

            IF(NTHGRP01(5)/=0)THEN
              WRITE(CHR_OLD,'(I2.2)')IRUNN
              FILTH  = CHR_OLD
              FILTH(3:3)= ABC(5)
              FILNAMTH=ROOTNAM(1:ROOTLEN)//'T'//FILTH
              LONG = 4
              IF(TH_VERS>=50) THEN
               FILNAMTH=ROOTNAM(1:ROOTLEN)
     .             //'_'//CHRUN//'_'//ABC(5)//'.thy'
               LONG = 11
              ENDIF
              IUNIT= IUHI(5)
              IFIL= IUNIT
              IFILTITL = IUTITLHI(5)
              ITHFLAG = 5
              CALL HIST1(FILNAMTH,IFIL              ,NTHGRP1(5)        ,LONG,
     2                   WA      ,PM                ,GEO               ,IPART,
     3                   SUBSETS ,OUTPUT%TH%ITHGRPE ,OUTPUT%TH%ITHBUFE ,IGEO,
     4                   IPM     ,IPART(1+LIPART1*(NPART+NTHPART)+8*(NPART+NTHPART)),2,1,
     5                   1       ,AFORM(5)          ,ITHFLAG           ,ITHVAR,
     6                   IFILTITL,OUTPUT%TH%SITHBUFE)
            END IF

            IF(NTHGRP01(6)/=0)THEN
              WRITE(CHR_OLD,'(I2.2)')IRUNN
              FILTH  = CHR_OLD
              FILTH(3:3)= ABC(6)
              FILNAMTH=ROOTNAM(1:ROOTLEN)//'T'//FILTH
              LONG = 4
              IF(TH_VERS>=50) THEN
               FILNAMTH=ROOTNAM(1:ROOTLEN)
     .             //'_'//CHRUN//'_'//ABC(6)//'.thy'
               LONG = 11
              ENDIF
              IUNIT= IUHI(6)
              IFIL= IUNIT
              IFILTITL = IUTITLHI(6)
              ITHFLAG = 6
              CALL HIST1(FILNAMTH,IFIL              ,NTHGRP1(6)        ,LONG,
     2                   WA      ,PM                ,GEO               ,IPART ,
     3                   SUBSETS ,OUTPUT%TH%ITHGRPF ,OUTPUT%TH%ITHBUFF ,IGEO ,
     4                   IPM     ,IPART(1+LIPART1*(NPART+NTHPART)+10*(NPART+NTHPART)),2,1,
     5                   1       ,AFORM(6)          ,ITHFLAG           ,ITHVAR,
     6                   IFILTITL,OUTPUT%TH%SITHBUFF)
            END IF

            IF(NTHGRP01(7)/=0)THEN
              WRITE(CHR_OLD,'(I2.2)')IRUNN
              FILTH  = CHR_OLD
              FILTH(3:3)= ABC(7)
              FILNAMTH=ROOTNAM(1:ROOTLEN)//'T'//FILTH
              LONG = 4
              IF(TH_VERS>=50) THEN
               FILNAMTH=ROOTNAM(1:ROOTLEN)
     .             //'_'//CHRUN//'_'//ABC(7)//'.thy'
               LONG = 11
              ENDIF
              IUNIT= IUHI(7)
              IFIL= IUNIT
              IFILTITL = IUTITLHI(7)
              ITHFLAG = 7
              CALL HIST1(FILNAMTH,IFIL              ,NTHGRP1(7)        ,LONG,
     2                   WA      ,PM                ,GEO               ,IPART ,
     3                   SUBSETS ,OUTPUT%TH%ITHGRPG ,OUTPUT%TH%ITHBUFG ,IGEO ,
     4                   IPM,IPART(1+LIPART1*(NPART+NTHPART)+12*(NPART+NTHPART)),2,1,
     5                   1       ,AFORM(7)          ,ITHFLAG           ,ITHVAR,
     6                   IFILTITL,OUTPUT%TH%SITHBUFG )
            END IF

            IF(NTHGRP01(8)/=0)THEN
              WRITE(CHR_OLD,'(I2.2)')IRUNN
              FILTH  = CHR_OLD
              FILTH(3:3)= ABC(8)
              FILNAMTH=ROOTNAM(1:ROOTLEN)//'T'//FILTH
              LONG = 4
              IF(TH_VERS>=50) THEN
               FILNAMTH=ROOTNAM(1:ROOTLEN)
     .             //'_'//CHRUN//'_'//ABC(8)//'.thy'
               LONG = 11
              ENDIF
              IUNIT= IUHI(8)
              IFIL= IUNIT
              IFILTITL = IUTITLHI(8)
              ITHFLAG = 8
              CALL HIST1(FILNAMTH ,IFIL              ,NTHGRP1(8),LONG,
     2                   WA       ,PM                ,GEO               ,IPART,
     3                   SUBSETS  ,OUTPUT%TH%ITHGRPH ,OUTPUT%TH%ITHBUFH ,IGEO,
     4                   IPM      ,IPART(1+LIPART1*(NPART+NTHPART)+14*(NPART+NTHPART)),2,1,
     5                   1       ,AFORM(8)           ,ITHFLAG           ,ITHVAR,
     6                   IFILTITL,OUTPUT%TH%SITHBUFH)
            END IF

            IF(NTHGRP01(9)/=0)THEN
              WRITE(CHR_OLD,'(I2.2)')IRUNN
              FILTH  = CHR_OLD
              FILTH(3:3)= ABC(9)
              FILNAMTH=ROOTNAM(1:ROOTLEN)//'T'//FILTH
              LONG = 4
              IF(TH_VERS>=50) THEN
               FILNAMTH=ROOTNAM(1:ROOTLEN)
     .             //'_'//CHRUN//'_'//ABC(9)//'.thy'
               LONG = 11
              ENDIF
              IUNIT= IUHI(9)
              IFIL= IUNIT
              IFILTITL = IUTITLHI(9)
              ITHFLAG = 9
              CALL HIST1(FILNAMTH,IFIL,NTHGRP1(9),LONG,
     2                   WA  ,PM  ,GEO ,IPART ,
     3                   SUBSETS ,OUTPUT%TH%ITHGRPI ,OUTPUT%TH%ITHBUFI ,IGEO,
     4                   IPM     ,IPART(1+LIPART1*(NPART+NTHPART)+16*(NPART+NTHPART)),2,1,
     5                   1       ,AFORM(9)          ,ITHFLAG           ,ITHVAR,
     6                   IFILTITL,OUTPUT%TH%SITHBUFI)
            END IF
        ELSE
          WRITE(CHR_OLD,'(I2.2)')IRUNN
          CALL HIST13(
     1           IPARG ,IXS ,IXQ ,IXC ,IXT ,
     2           IXP ,IXR ,WA  ,ITAB ,PM  ,
     3           NPBYL ,IXTG ,IRFE    ,LACCELM ,
     4           IPARI ,IPART ,OUTPUT%TH%ITHGRP ,OUTPUT%TH%ITHBUF ,CHR_OLD)
        ENDIF

        IF(NABFILE /= 0) THEN
#ifdef DNC
          IF(ABFILE(1) /=0)THEN
            FILNAMABF_TMP=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'.tmp'
            LONG_TMP = 9
            IFILABF = 11
            ITHFLAG = 10
            CALL ABFHIST1(FILNAMABF_TMP ,IFILABF          ,NTHGRP           ,LONG_TMP,
     2                    WA            ,PM               ,GEO              ,IPART,
     3                    SUBSETS       ,OUTPUT%TH%ITHGRP ,OUTPUT%TH%ITHBUF ,IGEO,
     4                    IPM           ,IPART            ,LIPART1          ,8,
     5                    12            ,ITHVAR           ,ITHFLAG )
            FILNAMABF=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'.abf'
            LONG = 9
            LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + LONG        
            TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF(1:ROOTLEN+LONG)

            LEN_TMP_NAME2 = OUTFILE_NAME_LEN + ROOTLEN + LONG_TMP        
            TMP_NAME2=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF_TMP(1:ROOTLEN+LONG_TMP)
            DO I=1,LEN_TMP_NAME!ROOTLEN+LONG
              IFILNAM(I)=ICHAR(TMP_NAME(I:I))
            ENDDO
            DO I=1,LEN_TMP_NAME2!ROOTLEN+LONG_TMP
              IFILNAM_TMP(I)=ICHAR(TMP_NAME2(I:I))
            ENDDO            
C            NB_INDEX_ABF = MAX(2147483647,NINT((TSTOP-TT)/DTABF(1))+1)
C           avoid SIGFPE
            RMAXINT=1073741824
            RNB_INDEX_ABF = MIN(RMAXINT,(TSTOP-TT)/DTABF(1))
            NB_INDEX_ABF = INT(RNB_INDEX_ABF)+1
            CALL BUILD_ABFFILE(ABINP,ABOUT,IABFPIPE,IFILNAM,LEN_TMP_NAME,
     .                      IFILNAM_TMP,LEN_TMP_NAME2,NB_INDEX_ABF)
          ENDIF
c
          IF(NTHGRP01(1)/=0 .AND. ABFILE(2) /=0)THEN
              FILNAMABF_TMP=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_a.tmp'
              LONG_TMP = 11
              IFILABF = 12
              IFIL= IUNIT
              ITHFLAG = 1
              CALL ABFHIST1(FILNAMABF_TMP ,IFILABF           ,NTHGRP1(1)        ,LONG_TMP,
     2                      WA            ,PM                ,GEO               ,IPART,
     3                      SUBSETS       ,OUTPUT%TH%ITHGRPA ,OUTPUT%TH%ITHBUFA ,IGEO,
     4                      IPM           ,IPART(1+LIPART1*(NPART+NTHPART))     ,2,1,
     5                      1             ,ITHVAR            ,ITHFLAG)
              FILNAMABF=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_a.abf'
              LONG = 11
              LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + LONG        
              TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF(1:ROOTLEN+LONG)

              LEN_TMP_NAME2 = OUTFILE_NAME_LEN + ROOTLEN + LONG_TMP        
              TMP_NAME2=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF_TMP(1:ROOTLEN+LONG_TMP)
              DO I=1,LEN_TMP_NAME!ROOTLEN+LONG
                IFILNAM(I)=ICHAR(TMP_NAME(I:I))
              ENDDO
              
              DO I=1,LEN_TMP_NAME2!ROOTLEN+LONG_TMP
                IFILNAM_TMP(I)=ICHAR(TMP_NAME2(I:I))
              ENDDO
              NB_INDEX_ABF = NINT((TSTOP-TT)/DTABF(2))+1
              CALL BUILD_ABFFILE(ABINP,ABOUT,IABFPIPE,IFILNAM,LEN_TMP_NAME,
     .                          IFILNAM_TMP,LEN_TMP_NAME2,NB_INDEX_ABF)
          END IF
          
          IF(NTHGRP01(2)/=0 .AND. ABFILE(3) /=0)THEN
              FILNAMABF_TMP=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_b.tmp'
              LONG_TMP = 11
              IFILABF = 13
              IFIL= IUNIT
              ITHFLAG = 2
              CALL ABFHIST1(FILNAMABF_TMP ,IFILAB F         ,NTHGRP1(2)        ,LONG_TMP,
     2                      WA            ,PM               ,GEO               ,IPART,
     3                      SUBSETS       ,OUTPUT%TH%ITHGRPB,OUTPUT%TH%ITHBUFB ,IGEO,
     4                      IPM           ,IPART(1+LIPART1*(NPART+NTHPART)+2*(NPART+NTHPART)),2,1,
     5                      1             ,ITHVAR           ,ITHFLAG)
              FILNAMABF=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_b.abf'
              LONG = 11
              LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + LONG        
              TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF(1:ROOTLEN+LONG)

              LEN_TMP_NAME2 = OUTFILE_NAME_LEN + ROOTLEN + LONG_TMP        
              TMP_NAME2=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF_TMP(1:ROOTLEN+LONG_TMP)

              DO I=1,LEN_TMP_NAME!ROOTLEN+LONG
                IFILNAM(I)=ICHAR(TMP_NAME(I:I))
              ENDDO
              
              DO I=1,LEN_TMP_NAME2!ROOTLEN+LONG_TMP
                IFILNAM_TMP(I)=ICHAR(TMP_NAME2(I:I))
              ENDDO
              NB_INDEX_ABF = NINT((TSTOP-TT)/DTABF(3))+1
              CALL BUILD_ABFFILE(ABINP,ABOUT,IABFPIPE,IFILNAM,LEN_TMP_NAME,
     .                           IFILNAM_TMP,LEN_TMP_NAME2,NB_INDEX_ABF)
          END IF
          
          IF(NTHGRP01(3)/=0 .AND. ABFILE(4) /=0)THEN
              FILNAMABF_TMP=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_c.tmp'
              LONG_TMP = 11
              IFILABF = 14
              IFIL= IUNIT
              ITHFLAG = 3
              CALL ABFHIST1(FILNAMABF_TMP ,IFILABF ,NTHGRP1(3) ,LONG_TMP,
     2                      WA            ,PM      ,GEO        ,IPART ,
     3                      SUBSETS       ,OUTPUT%TH%ITHGRPC   ,OUTPUT%TH%ITHBUFC ,IGEO,
     4                      IPM           ,IPART               ,2 ,1,
     5                      1             ,ITHVAR              ,ITHFLAG)
              FILNAMABF=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_c.abf'
              LONG = 11
              LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + LONG        
              TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF(1:ROOTLEN+LONG)

              LEN_TMP_NAME2 = OUTFILE_NAME_LEN + ROOTLEN + LONG_TMP        
              TMP_NAME2=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF_TMP(1:ROOTLEN+LONG_TMP)
              DO I=1,LEN_TMP_NAME!ROOTLEN+LONG
                IFILNAM(I)=ICHAR(TMP_NAME(I:I))
              ENDDO
              
              DO I=1,LEN_TMP_NAME2!ROOTLEN+LONG_TMP
                IFILNAM_TMP(I)=ICHAR(TMP_NAME2(I:I))
              ENDDO
              NB_INDEX_ABF = NINT((TSTOP-TT)/DTABF(4))+1
              CALL BUILD_ABFFILE(ABINP,ABOUT,IABFPIPE,IFILNAM,LEN_TMP_NAME2,
     .                           IFILNAM_TMP,LEN_TMP_NAME2,NB_INDEX_ABF)
          END IF
          
          IF(NTHGRP01(4)/=0 .AND. ABFILE(5) /=0)THEN
              FILNAMABF_TMP=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_d.tmp'
              LONG_TMP = 11
              IFILABF = 15
              IFIL= IUNIT
              ITHFLAG = 4
              CALL ABFHIST1( FILNAMABF_TMP ,IFILABF ,NTHGRP1(4) ,LONG_TMP,
     2                       WA            ,PM      ,GEO        ,IPART,
     3                       SUBSETS       ,OUTPUT%TH%ITHGRPD   ,OUTPUT%TH%ITHBUFD ,IGEO,
     4                       IPM           ,IPART   ,2     ,1   ,
     5                       1             ,ITHVAR  ,ITHFLAG)
              FILNAMABF=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_d.abf'
              LONG = 11
            LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + LONG        
            TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF(1:ROOTLEN+LONG)

            LEN_TMP_NAME2 = OUTFILE_NAME_LEN + ROOTLEN + LONG_TMP        
            TMP_NAME2=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF_TMP(1:ROOTLEN+LONG_TMP)
            DO I=1,LEN_TMP_NAME!ROOTLEN+LONG
              IFILNAM(I)=ICHAR(TMP_NAME(I:I))
              ENDDO
            DO I=1,LEN_TMP_NAME2!ROOTLEN+LONG_TMP
              IFILNAM_TMP(I)=ICHAR(TMP_NAME2(I:I))
              ENDDO
              NB_INDEX_ABF = NINT((TSTOP-TT)/DTABF(5))+1
           CALL BUILD_ABFFILE(ABINP,ABOUT,IABFPIPE,IFILNAM,LEN_TMP_NAME,
     .                      IFILNAM_TMP,LEN_TMP_NAME2,NB_INDEX_ABF)
            END IF
            IF(NTHGRP01(5)/=0 .AND. ABFILE(6) /=0)THEN
              FILNAMABF_TMP=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_e.tmp'
              LONG_TMP = 11
              IFILABF = 16
              IFIL= IUNIT
              ITHFLAG = 5
              CALL ABFHIST1( FILNAMABF_TMP ,IFILABF,NTHGRP1(5),LONG_TMP,
     2                       WA            ,PM     ,GEO       ,IPART,
     3                       SUBSETS       ,OUTPUT%TH%ITHGRPE ,OUTPUT%TH%ITHBUFE ,IGEO,
     4                       IPM           ,IPART             ,2     ,1     ,
     5                       1             ,ITHVAR            ,ITHFLAG)
              FILNAMABF=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_e.abf'
              LONG = 11
            LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + LONG        
            TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF(1:ROOTLEN+LONG)

            LEN_TMP_NAME2 = OUTFILE_NAME_LEN + ROOTLEN + LONG_TMP        
            TMP_NAME2=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF_TMP(1:ROOTLEN+LONG_TMP)
            DO I=1,LEN_TMP_NAME!ROOTLEN+LONG
              IFILNAM(I)=ICHAR(TMP_NAME(I:I))
              ENDDO
            DO I=1,LEN_TMP_NAME2!ROOTLEN+LONG_TMP
              IFILNAM_TMP(I)=ICHAR(TMP_NAME2(I:I))
              ENDDO
              NB_INDEX_ABF = NINT((TSTOP-TT)/DTABF(6))+1
           CALL BUILD_ABFFILE(ABINP,ABOUT,IABFPIPE,IFILNAM,LEN_TMP_NAME,
     .                      IFILNAM_TMP,LEN_TMP_NAME2,NB_INDEX_ABF)
          END IF

          IF(NTHGRP01(6)/=0 .AND. ABFILE(7) /=0)THEN
              FILNAMABF_TMP=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_f.tmp'
              LONG_TMP = 11
              IFILABF = 17
              IFIL= IUNIT
              ITHFLAG = 6
              CALL ABFHIST1(FILNAMABF_TMP,IFILABF,NTHGRP1(6),LONG_TMP,
     2                      WA  ,PM  ,GEO ,IPART ,
     3                      SUBSETS ,OUTPUT%TH%ITHGRPF,OUTPUT%TH%ITHBUFF,IGEO    ,
     4                      IPM   ,IPART   ,2     ,1     ,
     5                      1   ,ITHVAR,ITHFLAG)
              FILNAMABF=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_f.abf'
              LONG = 11
            LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + LONG        
            TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF(1:ROOTLEN+LONG)

            LEN_TMP_NAME2 = OUTFILE_NAME_LEN + ROOTLEN + LONG_TMP        
            TMP_NAME2=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF_TMP(1:ROOTLEN+LONG_TMP)
            DO I=1,LEN_TMP_NAME!ROOTLEN+LONG
              IFILNAM(I)=ICHAR(TMP_NAME(I:I))
              ENDDO
            DO I=1,LEN_TMP_NAME2!ROOTLEN+LONG_TMP
              IFILNAM_TMP(I)=ICHAR(TMP_NAME2(I:I))
              ENDDO
              NB_INDEX_ABF = NINT((TSTOP-TT)/DTABF(7))+1
              CALL BUILD_ABFFILE(ABINP,ABOUT,IABFPIPE,IFILNAM,LEN_TMP_NAME,
     .                           IFILNAM_TMP,LEN_TMP_NAME2,NB_INDEX_ABF)
          END IF

          IF(NTHGRP01(7)/=0 .AND. ABFILE(8) /=0)THEN
              FILNAMABF_TMP=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_g.tmp'
              LONG_TMP = 11
              IFILABF = 18
              IFIL= IUNIT
              ITHFLAG = 7
              CALL ABFHIST1(FILNAMABF_TMP,IFILABF,NTHGRP1(7),LONG_TMP,
     2                      WA  ,PM  ,GEO ,IPART ,
     3                      SUBSETS ,OUTPUT%TH%ITHGRPG,OUTPUT%TH%ITHBUFG,IGEO    ,
     4                      IPM     ,IPART   ,2     ,1     ,
     5                      1       ,ITHVAR,ITHFLAG)
     
              FILNAMABF=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_g.abf'
              LONG = 11
              LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + LONG        
              TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF(1:ROOTLEN+LONG)

              LEN_TMP_NAME2 = OUTFILE_NAME_LEN + ROOTLEN + LONG_TMP        
              TMP_NAME2=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF_TMP(1:ROOTLEN+LONG_TMP)
              
              DO I=1,LEN_TMP_NAME!ROOTLEN+LONG
                IFILNAM(I)=ICHAR(TMP_NAME(I:I))
              ENDDO
              
              DO I=1,LEN_TMP_NAME2!ROOTLEN+LONG_TMP
                IFILNAM_TMP(I)=ICHAR(TMP_NAME2(I:I))
              ENDDO
              
              NB_INDEX_ABF = NINT((TSTOP-TT)/DTABF(8))+1
              CALL BUILD_ABFFILE(ABINP,ABOUT,IABFPIPE,IFILNAM,LEN_TMP_NAME,
     .                           IFILNAM_TMP,LEN_TMP_NAME2,NB_INDEX_ABF)
          END IF

          IF(NTHGRP01(8)/=0 .AND. ABFILE(9) /=0)THEN
              FILNAMABF_TMP=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_h.tmp'
              LONG_TMP = 11
              IFILABF = 19
              IFIL= IUNIT
              ITHFLAG = 8
              
              CALL ABFHIST1(FILNAMABF_TMP ,IFILABF ,NTHGRP1(8) ,LONG_TMP,
     2                      WA            ,PM                  ,GEO              ,IPART,
     3                      SUBSETS       ,OUTPUT%TH%ITHGRPH  ,OUTPUT%TH%ITHBUFH ,IGEO,
     4                      IPM   ,IPART  ,2                  ,1     ,
     5                      1     ,ITHVAR ,ITHFLAG )
     
              FILNAMABF=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_h.abf'
              LONG = 11
              LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + LONG        
              TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF(1:ROOTLEN+LONG)

              LEN_TMP_NAME2 = OUTFILE_NAME_LEN + ROOTLEN + LONG_TMP        
              TMP_NAME2=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF_TMP(1:ROOTLEN+LONG_TMP)
              
              DO I=1,LEN_TMP_NAME!ROOTLEN+LONG
                IFILNAM(I)=ICHAR(TMP_NAME(I:I))
              ENDDO
              
              DO I=1,LEN_TMP_NAME2!ROOTLEN+LONG_TMP
                 IFILNAM_TMP(I)=ICHAR(TMP_NAME2(I:I))
              ENDDO
              NB_INDEX_ABF = NINT((TSTOP-TT)/DTABF(9))+1
              
              CALL BUILD_ABFFILE(ABINP,ABOUT,IABFPIPE,IFILNAM,LEN_TMP_NAME,
     .                      IFILNAM_TMP,LEN_TMP_NAME2,NB_INDEX_ABF)
          END IF
            
          IF(NTHGRP01(9)/=0 .AND. ABFILE(10) /=0)THEN
              FILNAMABF_TMP=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_i.tmp'
              LONG_TMP = 11
              IFILABF = 20
              IFIL= IUNIT
              ITHFLAG = 9
              CALL ABFHIST1(FILNAMABF_TMP ,IFILABF,NTHGRP1(9) ,LONG_TMP ,
     2                      WA            ,PM                 ,GEO               ,IPART,
     3                      SUBSETS       ,OUTPUT%TH%ITHGRPI  ,OUTPUT%TH%ITHBUFI ,IGEO    ,
     4                      IPM           ,IPART(1+LIPART1*(NPART+NTHPART)+16*(NPART+NTHPART)),2,1,
     5                      1             ,ITHVAR                                ,ITHFLAG)
     
              FILNAMABF=ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_i.abf'
              LONG = 11
              LEN_TMP_NAME = OUTFILE_NAME_LEN + ROOTLEN + LONG        
              TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF(1:ROOTLEN+LONG)

              LEN_TMP_NAME2 = OUTFILE_NAME_LEN + ROOTLEN + LONG_TMP        
              TMP_NAME2=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAMABF_TMP(1:ROOTLEN+LONG_TMP)
              DO I=1,LEN_TMP_NAME!ROOTLEN+LONG
                IFILNAM(I)=ICHAR(TMP_NAME(I:I))
              ENDDO
              
              DO I=1,LEN_TMP_NAME2!ROOTLEN+LONG_TMP
                IFILNAM_TMP(I)=ICHAR(TMP_NAME2(I:I))
              ENDDO
              
              NB_INDEX_ABF = NINT((TSTOP-TT)/DTABF(10))+1
              CALL BUILD_ABFFILE(ABINP,ABOUT,IABFPIPE,IFILNAM,LEN_TMP_NAME,
     .                      IFILNAM_TMP,LEN_TMP_NAME2,NB_INDEX_ABF)
          END IF
#else
      ! ABF converter is not available
      WRITE(6,'(A)') '*** ERROR /ABF is not available'
      CALL ARRET(2)
#endif
        ENDIF
      ENDIF
      CALL INIT_TH0( IPARG,MAT_ELEM%ELBUF,IGEO,IXR,OUTPUT%TH,WEIGHT)
      CALL TRACE_OUT(18)
C---------------------------------------------------------------------
#ifdef DNC
C New MADYMO coupling
      IF (IMADCPL==1)THEN
        CALL INIT_MADCPL()

C Send Data / Fem Structures

        K1=1+LIPART1*(NPART+NTHPART)+2*9*(NPART+NTHPART)
        K2=K1+NUMELS
        K3=K2+NUMELQ
        K4=K3+NUMELC
        K5=K4+NUMELT
        K6=K5+NUMELP
        K7=K6+NUMELR
        K8=K7
        K9=K8+NUMELTG
        K10=K9+NUMELX
        K11=K10+NUMSPH
        CALL RAD_INIT_MADCPL(IPART, PM, GEO, ITAB,
     *                       X,     MS,
     *                       IXC ,IXTG ,IXS ,
     *                       IPART(K3),IPART(K8),IPART(K1),
     *                       MADPRT,   MADCLNOD,  MADSH4,  MADSH3,
     *                       MADSOL )
      END IF
#endif
C---------------------------------------------------------------------
         ITASK=0
         IRUNN_BIS = IRUNN

         CALL RESOL_HEAD(ITASK        ,AF              ,IAF             ,IDATA      ,RDATA,XDP ,
     .                   GRAPHE       ,IFLOW           ,RFLOW           ,MCP        ,TEMP      ,
     .                   INTBUF_TAB   ,STACK           ,IRUNN_BIS       ,
     .                   MULTI_FVM    ,H3D_DATA        ,INTBUF_FRIC_TAB ,SUBSETS    ,IGRNOD    ,
     .                   IGRBRIC      ,IGRQUAD         ,IGRSH4N         ,IGRSH3N    ,IGRTRUSS  ,
     .                   IGRBEAM      ,IGRSPRING       ,IGRPART         ,IGRSURF    ,IGRSLIN   ,
     .                   FORNEQS      ,PINCH_DATA      ,ALE_CONNECTIVITY,
     .                   DRAPE_SH4N   ,DRAPE_SH3N      ,T_MONVOL        ,SENSORS    ,EBCS_TAB        ,
     .                   DIFFUSION    ,SEGVAR          ,DYNAIN_DATA     ,DRAPEG     ,USER_WINDOWS    ,
     .                   OUTPUT       ,INTERFACES      ,DT              ,LOADS      ,MAT_ELEM  , PYTHON)
C---------------------------------------------------------------------
      CURRENT_RUN = GLOBAL_COMP_TIME%RUN_NBR
      CALL SPMD_REDUCE_DB(GLOBAL_COMP_TIME%ENGINE_TIME(CURRENT_RUN),RESULT,1,0,"MAX ")
#ifdef DNC
C New MADYMO coupling
      IF (IMADCPL==1)THEN
        CALL TERMINATE_MADCPL(N)
      ENDIF
#endif
C----------------------------------------------------------------------X
C End tasks
      CALL INIPAR(ITID,2,NNODES,INPUTNAM,GOT_INPUT,NBTASK)
C---------------------------------------------------------------------
      IF(MSTOP==1) THEN
        N = 3
      ELSE
        N = 1
      ENDIF
      
      CALL USER_WINDOWS_CLEAN(USER_WINDOWS)
      CALL TH_CLEAN(OUTPUT%TH)
      DEALLOCATE(GRAPHE)
      DEALLOCATE(IFLOW, RFLOW)
      CALL FVDEAL()
      CALL ARRET(N)
C---------------------------------------------------------------------

      RETURN
      END
