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
!||    starter0                          ../starter/source/starter/starter0.F
!||--- called by ------------------------------------------------------
!||    starter                           ../starter/source/starter/starter.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                            ../starter/source/output/message/message.F
!||    anend                             ../starter/source/output/analyse/analyse_arret.F
!||    aninit                            ../starter/source/output/analyse/analyse.c
!||    build_msg                         ../starter/source/output/message/build_msg.F
!||    checksum_check                    ../starter/source/output/checksum/checksum_check.F90
!||    contrl                            ../starter/source/starter/contrl.F
!||    execargcheck                      ../starter/source/starter/execargcheck.F
!||    f_anend                           ../starter/source/output/analyse/analyse_arret.F
!||    get_file_name_info                ../starter/source/system/get_file_name_info.F
!||    glob_therm_init                   ../starter/source/constraints/thermic/glob_therm_init.F90
!||    hm_convert_2d_elements_seatbelt   ../starter/source/devtools/hm_reader/hm_convert_2d_elements_seatbelt.F
!||    hm_convert_fail_tab               ../starter/source/devtools/hm_reader/hm_convert_fail_tab.F
!||    hm_convert_inter_type19           ../starter/source/devtools/hm_reader/hm_convert_inter_type19.F
!||    hm_count_2d_element_seatbelt      ../starter/source/devtools/hm_reader/hm_count_2d_element_seatbelt.F
!||    hm_get_max_id                     ../starter/source/devtools/hm_reader/hm_get_max_id.F
!||    hm_messages                       ../starter/source/devtools/hm_reader/hm_messages.F
!||    hm_read_checksum                  ../starter/source/output/checksum/checksum_option.F90
!||    hm_read_submodel                  ../starter/source/model/assembling/hm_read_submodel.F
!||    ini_msg                           ../starter/source/output/message/inimsg.F
!||    inicod                            ../starter/source/tools/univ/inicod.F
!||    iniconstant                       ../starter/source/system/iniconstant.F
!||    initime                           ../starter/source/system/timer.F
!||    lectur                            ../starter/source/starter/lectur.F
!||    my_exit                           ../starter/source/output/analyse/analyse.c
!||    prhelpinfo                        ../starter/source/starter/execargcheck.F
!||    printcenter                       ../starter/source/starter/radioss_title.F
!||    printcpu                          ../starter/source/system/printcpu.F
!||    printime                          ../starter/source/system/timer.F
!||    r2r_fork                          ../starter/source/coupling/rad2rad/r2r_fork.F
!||    radioss_title                     ../starter/source/starter/radioss_title.F
!||    setignorecore                     ../starter/source/system/traceback_handler.c
!||    st_uaccess_dum                    ../starter/source/user_interface/uaccess.F
!||    st_utable_dum                     ../starter/source/user_interface/utable.F
!||    startime                          ../starter/source/system/timer.F
!||    stoptime                          ../starter/source/system/timer.F
!||    titre1                            ../starter/source/output/outfile/titre1.F
!||    trace_in1                         ../starter/source/system/trace_back.F
!||    trace_out1                        ../starter/source/system/trace_back.F
!||--- uses       -----------------------------------------------------
!||    checksum_check_mod                ../starter/source/output/checksum/checksum_check.F90
!||    checksum_starter_option_mod       ../starter/source/output/checksum/checksum_option.F90
!||    defaults_mod                      ../starter/source/modules/defaults_mod.F90
!||    detonators_mod                    ../starter/share/modules1/detonators_mod.F
!||    glob_therm_init_mod               ../starter/source/constraints/thermic/glob_therm_init.F90
!||    message_mod                       ../starter/share/message_module/message_mod.F
!||    submodel_mod                      ../starter/share/modules1/submodel_mod.F
!||    th_mod                            ../starter/share/modules1/th_mod.F90
!||    user_interface_mod                ../starter/source/modules/user_interface_mod.F90
!||    user_sensor_mod                   ../starter/source/modules/user_interface_mod.F90
!||====================================================================
      SUBROUTINE STARTER0   
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE SUBMODEL_MOD
      USE CHECK_MOD
      USE INOUTFILE_MOD
      USE DETONATORS_MOD
      USE EBCS_MOD
      USE ALEMUSCL_MOD , only:ALEMUSCL_Param
      USE ALEFVM_MOD , only:ALEFVM_Param
      USE USER_WINDOWS_MOD
      USE USER_SENSOR_MOD
      USE USER_INTERFACE_MOD
      USE ALE_MOD
      USE OUTPUT_MOD
      USE MAT_ELEM_MOD
      USE NAMES_AND_TITLES_MOD
      USE DEFAULTS_MOD, only: DEFAULTS_
      use glob_therm_mod
      use glob_therm_init_mod
      USE PBLAST_MOD
      use checksum_starter_option_mod
      use checksum_check_mod
      use th_mod , only : TH_HAS_NODA_PEXT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "analyse_name.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "com09_c.inc"
#include      "com10_c.inc"
#include      "sphcom.inc"
#include      "titr_c.inc"
#include      "units_c.inc"
#include      "units_fxbody_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr12_c.inc"
#include      "scr15_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
#include      "param_c.inc"
#include      "lagmult.inc"
#include      "flowcom.inc"
#include      "xtimscr_c.inc"
#include      "sysunit.inc"
#include      "build_info.inc"
#include      "altdoctag.inc"
#include      "execinp.inc"
#include      "r2r_c.inc"
#include      "commandline.inc"
#include      "userlib.inc"
#include      "ngr2usr_c.inc"
#include      "inter22.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K, STAT,INP,OUT
      INTEGER IFILNAM(2148),LEN,IDMAX_INTER,
     .        IDMAX_GRNOD,IDMAX_LINE,IDMAX_TABLE,IDMAX_FAIL,IDMAX_FUNCT,
     .        IDMAX_PART,IDMAX_PROP,IDMAX_MAT,IDMAX_ELEM,IDMAX_TH,
     .        NB_SEATBELT_SHELLS,RADIOSSV,IFL
      INTEGER LENR,RUNN,FVERS,IO_ERR,TAGLEN,CHECKSUMLEN,IS_DYNA
      INTEGER LFNAME,LEN_ENV,STATUS,ISUB_HIERARCHY,TRALL_MAXVAL(7),EDI_RES,
     .        LOAD_ERROR,IBID
      INTEGER :: LEN_TMP_NAME
      INTEGER :: ANERROR
      INTEGER :: RADFLEX_PROCESS_PID
C
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: SEATBELT_CONVERTED_ELEMENTS
C
      CHARACTER(LEN=2148) :: FILNAM  ! Maximum is SIZE(OUTFILE_CHAR_LEN) + 100
      CHARACTER CHRUN*4,CPUNAM*20,ROOTN*80,CHRUNR*4,ARCHTITLE*66
      CHARACTER ALTDOCTAG*256,CHECKSUM*256
      CHARACTER*2048 FNAME,VAL
C
      CHARACTER(LEN=NCHARLINE) :: ERR_MSG
      CHARACTER(len=8)  :: STARTDATE
      CHARACTER(len=10) :: STARTTIME
      CHARACTER(len=8)  :: ENDDATE
      CHARACTER(len=10) :: ENDTIME      
      CHARACTER(len=2048) :: TMP_NAME
      CHARACTER(len=2048) :: OUT_FILE_NAME
      INTEGER :: NB_DYNA_INCLUDE
C
      my_real UNITE, RTITR
C
      TYPE(MULTI_FVM_STRUCT)                          :: MULTI_FVM
      TYPE(SUBMODEL_DATA) , DIMENSION(:), ALLOCATABLE :: LSUBMODEL
      TYPE(DETONATORS_STRUCT_),TARGET                 :: DETONATORS
      TYPE(t_ebcs_tab)                                :: EBCS_TAB
      TYPE(OUTPUT_),TARGET                            :: OUTPUT
c      TYPE(SENSORS_) :: SENSORS    ! declared as global in USER_SENSOR_MOD
      TYPE(MAT_ELEM_)                                 :: MAT_ELEM
      CHARACTER(LEN=NCHARLINE) :: CWD
      CHARACTER GLOBAL_PATH*(ncharline+2048)
      INTEGER :: LEN_CWD
      INTEGER*4 GETCWD, STATUS_CWD
      TYPE(NAMES_AND_TITLES_)   :: NAMES_AND_TITLES    !< NAMES_AND_TITLES host the input deck names and titles for outputs
      TYPE(DEFAULTS_)           :: DEFAULTS            !< Hosts the variables from /DEF/ option
      TYPE(glob_therm_) :: glob_therm
      TYPE(pblast_) :: PBLAST
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER,EXTERNAL ::  ANEND
C--------------------------------------------------------
C     LAM MAX LENGTH REAL ARRAY
C     LMA MAX LENGTH INTEGER ARRAY
C------------------------------------------------
C     MACHINE.INC CONTAINS MACHINE SPECIFICATIONS
C     (MALLOC,TYPE MACHINE...)
C------------------------------------------------
#include      "machine.inc"
C=======================================================================
#include      "archloops.inc"
C -------------------------------------------------
C First task check the exec arguments (fill common)
      CALL EXECARGCHECK(OUTPUT)

      ! Options not requiering Starter deck read
      ! ----------------------------------------
      
      ! -checksum_read [rootname]
      IF ( OUTPUT%CHECKSUM%ST_CHECKSUM_READ == 1 ) THEN
         CALL CHECKSUM_CHECK(OUTPUT%CHECKSUM%ROOTNAME,PATH,CPUNAM,ARCHTITLE,IRESP)
         STOP
      ENDIF
C -------------------------------------------------
C     Constants
      CALL INICONSTANT
    
C     timers initialization
      CALL INITIME()
c
      CALL NAMES_AND_TITLE_INIT(NAMES_AND_TITLES)
C=======================================================================
C while waiting IOUT to be initialized set to stdout 
C Needs to be done in the early beginning
C ===================================================    
      IOUT=6 
      ISTDO=6
C=======================================================================
      CALL  RADIOSS_TITLE(ISTDO,CPUNAM,ARCHTITLE,FILNAM,ROOTLEN,CHRUN,IRESP,1)
C=======================================================================
c

C-------------------------------------------------------------------
C     Version number
C      To be modified when structure of rst file is modified
C-------------------------------------------------------------------
C
C CODVERS : VERSION CODE        = 2019 IF RELEASE 2019
C IMINVER : MINOR VERSION CODE  = 1 IF MINOR RELEASE A(A~1) OF 4.4 RELEASE
C ISRCVER : SOURCE VERSION CODE = 41*100+9
C-------------------------------------------------------------------
      CODVERS          = 2026
      IMINVER          = 1
      ISRCVER          = 1
C-------------------------------------------------------------------
      DYNA_MESSAGE = 0           ! Variable for message
      LOAD_ERROR = 0
      CALL  hm_reader_variables(LOAD_ERROR,CODVERS)
      IF(LOAD_ERROR /= 0) THEN
        WRITE(ISTDO,'(A)') ' '
        WRITE(ISTDO,'(A)') '------------------------------------------------------------------------'
        WRITE(ISTDO,'(A)') '  ERROR : '
        WRITE(ISTDO,'(A)') '  Reader configuration files are not available'
        WRITE(ISTDO,'(A)') '  Check and set RAD_CFG_PATH variable.'
        WRITE(ISTDO,'(A)') '  '
        WRITE(ISTDO,'(A)') '  Standard configuration file installation is '
#ifdef _WIN64
        WRITE(ISTDO,'(A)') '  %ALTAIR_HOME%\hwsolvers\radioss\cfg'
#else
        WRITE(ISTDO,'(A)') '  $ALTAIR_HOME/hwsolvers/radioss/cfg'
#endif
        WRITE(ISTDO,'(A)') '------------------------------------------------------------------------'
        WRITE(ISTDO,'(A)') ' '
        CALL MY_EXIT(2)
      ENDIF

!     ----------------------------
!     if -infile or outfile cdl are used, then one needs to change $TMPDIR 
!     in order to write all scratch files in the user folder
      IF(INOUT_BOOL) CALL RADIOSS_SET_ENV_VARIABLE(OUTFILE_NAME , OUTFILE_NAME_LEN)
!     ----------------------------
      CALL DATE_AND_TIME(STARTDATE,STARTTIME) 
c
      RES_MES = 123456
      RES_TMP = 123457
      RES_CHECK = 123499
      OPEN (UNIT=RES_MES,STATUS='SCRATCH',FORM='FORMATTED')
      OPEN (UNIT=RES_CHECK,STATUS='SCRATCH',FORM='FORMATTED')
C
      ISKIP_NGR2USR_ERROR = 0 !set to 1 before calling NGR2USR enables to skip error message (otherwise they are often duplicated).
C
      CALL INI_MSG()
      CALL BUILD_MSG()
      CALL CHECK_MESSAGE_DEFINITION() 
C-------------------------------------------------------------------
C     initialization for Analyse Module
C-------------------------------------------------------------------
      IERR    = 0
      CALL ANINIT(AN_STARTER, AN_LIVE)
      CALL SETIGNORECORE (ITRACE)

      IOUTP   = 0
      IOUTPUT = 0
C-------------------------------------------------------------------
C     Multidomains -> environment variable reading(windows)
C-------------------------------------------------------------------
      IPID = 1
      IDDOM = 0
      FLG_SWALE = 0
#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
      CALL GET_ENVIRONMENT_VARIABLE("R2R_ENV_IPID",VAL,LEN_ENV,STATUS,.TRUE.)
C---------> IPID = 0 --> Multidomains starter fils--------------
      IF (LEN_ENV==1) IPID = 0
      CALL GET_ENVIRONMENT_VARIABLE("R2R_ENV_SWALE",VAL,LEN_ENV,STATUS,.TRUE.)
C---------> IPID = 0 --> Multidomains switch ALE----------------
      IF (LEN_ENV==1) FLG_SWALE = 1
#endif

C-------------------------------------------------------------------
C     Date
C      To be modified when structure of rst file is modified
C-------------------------------------------------------------------
C ITESTV Format   [Version][YY][MM][DD]
      ITESTV           = 210290720
      
      CALL ALE%init()
      
      ALEFVM_Param%IEnabled = 0
      ALEFVM_Param%ISOLVER  = 0
      ALEFVM_Param%IPRINT_1 = 0
      ALEFVM_Param%IPRINT_2 = 0
C MUSCL Activation
      ALEMUSCL_Param%IALEMUSCL = 1
C MUSCL compression coefficient, default value set to 1.
      ALEMUSCL_Param%BETA = TWO
      RATIO22 = ONE + TEN/HUNDRED
      RADIOSSV = 100090402
      IFLEXPIPE = 1
      LEN = ncharfield*2
C
      TH_HAS_NODA_PEXT = 0
C
C No Decay for Starter
      TAGLEN      = 0
      CHECKSUMLEN = 0
      ALTDOCTAG   = '0'
      CHECKSUM    = '0'
      UNITMAX     = 0
C---
C init to 0 variable position writing double precision (XDP)
      IRXDP       = 0
C
      IF(IR4R8==2) ITESTV=-ITESTV
C------------------------------------------------
C Dynamic Libraries initialization
C------------------------------------------------
        CALL USER_WINDOWS_INIT(USER_WINDOWS)
        USERLIB_LIST(1:100)=0
        IF(GOT_USERL_ALTNAME==1)THEN
          DLIBFILE(1:LEN_USERL_ALTNAME)=USERL_ALTNAME(1:LEN_USERL_ALTNAME)
          DLIBFILE_SIZE=LEN_USERL_ALTNAME
        ELSE
                DLIBFILE='libraduser_'
          DLIBFILE_SIZE=LEN_TRIM(DLIBFILE)
        ENDIF
        USERL_AVAIL=0
        CALL DYN_USERLIB_INIT(DLIBFILE,DLIBFILE_SIZE,USERL_AVAIL,DLIBTKVERS,IRESP,GOT_USERL_ALTNAME)
C------------------------------------------------
C Mds lib initialization
C------------------------------------------------
#ifdef DNC
       CALL MDS_USERLIB_INIT (IRESP, MDS_AVAIL, MDS_VER, MDS_PATH, MDS_PATH_LEN)
#else
      MDS_AVAIL=0
#endif
C-----------------------------------------------------
C Avoid linking issue with dynamical libraries
C-----------------------------------------------------
        IERR=0
        CALL ST_UACCESS_DUM(IERR)      
        CALL ST_UTABLE_DUM(IERR)
C-----------------------------------------------------
C     initializations
C-----------------------------------------------------
      IS_DYNA = 0
      IF (GOT_INPUT == 1)THEN
C       SEARCH RUN NUMBER
        CALL GET_FILE_NAME_INFO(INPUT, LENI, ROOTN, LENR, RUNN, FVERS, IS_DYNA)
        IF (GOT_PATH==1) THEN
          FNAME=PATH(1:LENP)//INPUT(1:LENI)
        ELSE
          FNAME=INPUT(1:LENI)
        ENDIF
        LFNAME=LENI+LENP
        ISTDI = 81

#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32

            TMP_NAME(1:2048) =''
            LEN_TMP_NAME = INFILE_NAME_LEN+LFNAME
            TMP_NAME=INFILE_NAME(1:INFILE_NAME_LEN)//FNAME(1:LFNAME)
             OPEN(UNIT=ISTDI,FORM='FORMATTED',FILE=TMP_NAME(1:LEN_TMP_NAME),
     .            ACTION='READ',STATUS='OLD',IOSTAT=IO_ERR)
             IF (IO_ERR/=0) THEN
               WRITE(6,*)' '
               WRITE(6,*)'*** ERROR INPUT FILE "',FNAME(1:LFNAME),
     *                   '" NOT FOUND'
               WRITE(6,*)' '
               CALL PRHELPINFO()
               CALL MY_EXIT(2)
             ENDIF
             CLOSE(UNIT=ISTDI)
#endif
      ELSE
        FNAME  = " "
        ROOTN  = "RADIOSS_STARTER_INPUT"
        LENR   = 21
        LFNAME = 1
        ISTDI = 5
      ENDIF
C------------------------------------------------
C Read DYNA model & convert DYNA -> Radioss objects
C------------------------------------------------
      CALL STARTIME(15,1)

      NB_DYNA_INCLUDE = 0
      IF(IS_DYNA == 1) THEN
        CALL CPP_READ_DYNA_AND_CONVERT(FNAME, LFNAME,EDI_RES,FILNAM,ROOTLEN+9+OUTFILE_NAME_LEN)
      ENDIF
      CALL STOPTIME(15,1)
C----------------------------------------------------------------------
C Open starter output file as soon as possible
C----------------------------------------------------------------------
      ERR_MSG='OPEN OUTPUT FILE'
      ERR_CATEGORY='OPEN OUTPUT FILE'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C----------------------------------------------------------------------
      IF (GOT_INPUT == 1)THEN
C Rootname is not taken from /INPUT
        IF (LENR <=80)THEN
            ROOTNAM(1:LENR)=ROOTN(1:LENR)
        ENDIF
        ROOTLEN = LENR
C Runnumber is taken from input name
        IRUN = RUNN
      ENDIF

      IF(IRUN > 0)THEN
          WRITE(ISTDO,'(A)') ' '
          WRITE(ISTDO,'(A,I4)') '** ERROR : INPUT FILE RUN NUMBER SET TO ',IRUN
          WRITE(ISTDO,'(A)') '           MODIF FILES OPTION IS DEPRECATED'
          WRITE(ISTDO,'(A)') ' '
          CALL MY_EXIT(2)
       ENDIF

C-------------------------------------------------------------------
      IOUT=7
C     message file id
C-------------------------------------------------------------------
      WRITE(CHRUN,'(I4.4)')IRUN
      OUT_FILE_NAME=''
C    
      IF (IPID==1) THEN
      FILNAM  =OUTFILE_NAME(1:OUTFILE_NAME_LEN)//
     .         ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'.out'
       OPEN(UNIT=IOUT,FILE=FILNAM(1:OUTFILE_NAME_LEN+ROOTLEN+9),
     .     ACCESS='SEQUENTIAL',
     .     FORM='FORMATTED',STATUS='UNKNOWN')

      OUT_FILE_NAME(1:LEN_TRIM(FILNAM))=FILNAM(1:LEN_TRIM(FILNAM))

      WRITE(IOUT, '(1X,A)')TRIM(FILNAM)
C
      CALL TRACE_OUT1()
C
      CALL  RADIOSS_TITLE(IOUT,CPUNAM,ARCHTITLE,
     *                      FILNAM,ROOTLEN,CHRUN,IRESP,1)

      CALL PRINTCPU()
      CALL PRINTCENTER(" ",0,IOUT,1)
      ELSEIF (FLG_SWALE==1) THEN
       FILNAM  =OUTFILE_NAME(1:OUTFILE_NAME_LEN)//ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'.out'
       OPEN(UNIT=IOUT,FILE=FILNAM(1:ROOTLEN+9+OUTFILE_NAME_LEN),
     .     ACCESS='SEQUENTIAL',
     .     FORM='FORMATTED',STATUS='UNKNOWN')

      OUT_FILE_NAME(1:ROOTLEN+9+OUTFILE_NAME_LEN)=FILNAM(1:ROOTLEN+9+OUTFILE_NAME_LEN)

      ENDIF
      CALL TITRE1
C------------------------------------------------
      ERR_MSG='OPEN OUTPUT FILE'
      ERR_CATEGORY='OPEN OUTPUT FILE'
C------------------------------------------------
      IUGEO=4
      IIN=8
      IIN2=11
      IIN3=12
      IIN4=13
      IIN5=14
      IIN6=15
      IIN7=17
      IOU2=16
      IIN8 =18
      IIN9 =19
      IUINIMAP=21
      IUREE=22
      IFICM=23
      IFXM=24
      IFXS=25
      IEIGM=26
c use in ddsplit, will be re-set in ddsplit for multi threading purpose
      IFXM_L=27
      IFXS_L=28
c
      IUSBM =29
      IUSC1=30
      TSUB =31
      TSUB_TMP =32
C     IUSC2=9 ou 10
      IUSC3=50

C new composite shell pid max nbs 200
      NPROPG =1000
      NPROPM =250
      NPARG  = 100
      NPARI  = 100
      NPARIR=48
      LFXVELR = 9
      NIBCLD = 9
      LFACCLD = 2
      NIGRV   = 6 
      LFACGRV = 2
      LFACLOAD = 13
      NRVOLU =73
      NICBAG=5
      NRCBAG=9
      NIBJET=14
      NRBJET=420
      NRBHOL=22
      NIBHOL=19+15
      NIXIG3D= 15
      NIXX   = 5
      NIXFR1 = 0
      NIXFR2 = 0
      NISX   = 6
      NPEBC  =14
      NPEBCR =10
      NIMV   = 75 ! array size for Monitored Volumes
      NRBY   = 30
      NRBYLAG= 0
      NNPBY  = 19
      NMGRBY = 7
      NRWLP  = 26
      NIBVEL = 0
      NBVELP = 6
      NRCNX  = 12
      NTHVKI = 40
      NRBE2L = 12
      NRBE3L = 10
C     Parameters
C      LTITR   = 40
      RTITR = nchartitle/3.
      LTITR = ceiling(RTITR)
      LNOPT1  = 20 + LTITR
      LIGRN1  = 20 + LTITR
      LISURF1 = 20 + LTITR
      LISLIN1 = 20 + LTITR
      NITHGR  = 20 + LTITR
      LILSET1 = 20 + LTITR
      LISUB1  = 20 + LTITR
      LIBOX1  = 20 + LTITR
      LIPART1 = 20 + LTITR
      NPROPGI = 750 + LTITR
      NPROPMI = 300 + LTITR
C     npsav not used by starter
      NPSAV  = 0
      NR2R = 2
      NISP    = 8
      NSPBUF  = 14
      NISPCOND= 6
      KVOISPH = 0
c 2 new values for ISPHIO, NSEG and IAS for inlet outlet in spmd
      NISPHIO = 16
      NRIVF = 13
      NXFRAME = 36
      LSKEW = 12
      LISKN = 6
      NBCSLAG  = 0
      LACTIV = 10
      LRACTIV = 2
      LKJNI = 6
      LKJNR = 19
      NFRBYM = 28
      NIRBYM = 2
      NNPRW = 8
      SEGINDX=0
      NHIN2 =0
      NIFV = 16
      ALE%GLOBAL%NALENOVS = 6
      NLEDGE  = 15
      NRTRANS = 22
C
      NEXMAD=0
      NMADPRT=0
      NMADSH4=0
      NMADSH3=0
      NMADSOL=0
      NMADNOD=0
      I7STIFS=0
      NRDAMP=37
      NIFLOW=28
      NRFLOW=24
      NIIOFLOW=4
      NRIOFLOW=3
      MULTI_FVM%IS_USED = .FALSE.
      MULTI_FVM%NS_DIFF = .FALSE.
C
      KSH4TREE =4
      KSH3TREE =4
      KPADMESH =2
      KCONTACT =0
      KIPADMESH=1
C
      SIZFIELD = 13
      SIZLOADP = 13
c constant for access in tab IBUFSSG_IO, use in engine
      NIBSPH = 4
C
      LLACCELM = 25
      LLGAUGE  = 37
C    
      ALE%GLOBAL%I_DT_NODA_ALE_ON = 0 ! by default DT NODA * options are no longer available for ALE grid points.
C
      INP = ISTDI
      FLAG_KEY_M = 0
      FLAG_KEY_L = 0
      FLAG_KEY_T = 0
C------------------------------------------------------------------
      IF( GOT_INSPIRE_ALM == 1)THEN
        ERR_MSG='SOLVER'
      ELSE
        ERR_MSG='RADIOSS STARTER'
      ENDIF
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      ERR_MSG='GLOBAL UNITS'
      ERR_CATEGORY='GLOBAL UNITS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C-------------------------------------------------
      CALL TRACE_OUT1()
C-------------------------------------------------
      CALL INICOD
C-------------------------------------------------------------------
C Runnumber is taken from input name or previous treatments
C-------------------------------------------------------------------
      IRUN = RUNN
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      ALLOCATE(LSUBMODEL(1))
      IF(ALLOCATED(LSUBMODEL)) DEALLOCATE(LSUBMODEL)

C------------------------------------------------
C Build model in memory (Radioss format)
C------------------------------------------------
      IF (GOT_INPUT == 1 .AND. IS_DYNA == 0)THEN
C
        CALL STARTIME(16,1)
        STATUS_CWD = GETCWD(CWD)
        LEN_CWD = LEN_TRIM(CWD)
        GLOBAL_PATH = TRIM(PATH)
c
        CALL CPP_BUILD_MODEL_INC(FNAME,LFNAME,EDI_RES,NB_DYNA_INCLUDE,GLOBAL_PATH,LEN_TRIM(GLOBAL_PATH) )
c
        IF(NB_DYNA_INCLUDE .NE. 0) THEN
          DYNA_MESSAGE=1
          CLOSE(IOUT)
          CALL CPP_PRINT_DYNA(FILNAM,ROOTLEN+9+OUTFILE_NAME_LEN)
          OPEN(UNIT=IOUT,FILE=FILNAM(1:ROOTLEN+9+OUTFILE_NAME_LEN),
     .    ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='UNKNOWN',POSITION="APPEND")   
          WRITE(IOUT,'(A)')''
          WRITE(IOUT,'(A)')''
          WRITE(IOUT,'(A)')'************************************************************************'
          WRITE(IOUT,'(A)')'* RADIOSS STARTER PROCESS'
          WRITE(IOUT,'(A)')'************************************************************************'
        ENDIF
        CALL STOPTIME(16,1)
      ENDIF
C-------------------------------------------------------------------
C     Keyword mapping header output ( DYNA to RADIOSS)
C-------------------------------------------------------------------
        IF (IS_DYNA == 1) THEN
          DYNA_MESSAGE=1
          CLOSE(IOUT)
          CALL CPP_PRINT_DYNA(FILNAM,ROOTLEN+9+OUTFILE_NAME_LEN)
          OPEN(UNIT=IOUT,FILE=FILNAM(1:ROOTLEN+9+OUTFILE_NAME_LEN),
     .    ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='UNKNOWN',POSITION="APPEND")   
          WRITE(IOUT,'(A)')''
          WRITE(IOUT,'(A)')''
          WRITE(IOUT,'(A)')'************************************************************************'
          WRITE(IOUT,'(A)')'* RADIOSS STARTER PROCESS'
          WRITE(IOUT,'(A)')'************************************************************************'
        ENDIF
C------------------------------------------------
C Output errors/warning message (input reading phase)
C------------------------------------------------
      IF (GOT_INPUT == 1) CALL HM_MESSAGES(IS_DYNA)
C------------------------------------------------
C Write list of replaced parameters
C------------------------------------------------
      IF(IPID /= 0 .AND. GOT_HSTP_READ == 0) THEN
        CLOSE(IOUT)
        CALL CPP_PRINT_PARAMETERS(FILNAM,ROOTLEN+9+OUTFILE_NAME_LEN)
        OPEN(UNIT=IOUT,FILE=FILNAM(1:ROOTLEN+9+OUTFILE_NAME_LEN),
     .    ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='UNKNOWN',POSITION="APPEND") 
      ENDIF 
C------------------------------------------------
C count submodels & dimension LSUBMODEL structure 
C------------------------------------------------
      IF(ALLOCATED(LSUBMODEL)) DEALLOCATE(LSUBMODEL)
      NSUBMOD = 0
      IBID = 0
      CALL CPP_SUBMODEL_COUNT(NSUBMOD,IBID)
      IF(NSUBMOD > 0)THEN
        ALLOCATE (LSUBMODEL(NSUBMOD),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='LSUBMODEL')
C------------------------------------------------
C Build submodel hierarchy
C------------------------------------------------
        LSUBMODEL(1:NSUBMOD)%SKEW = 0
        CALL HM_READ_SUBMODEL(LSUBMODEL)
      ELSE
        ALLOCATE (LSUBMODEL(0),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='LSUBMODEL')
      ENDIF
C-------------------------------------------------------------------
C Convert /INTER/TYPE19 in memory to /INTER/TYPE7 & /INTER/TYPE11
C-------------------------------------------------------------------
      CALL HM_GET_MAX_ID('/INTER',IDMAX_INTER,LSUBMODEL)
      CALL HM_GET_MAX_ID('/GRNOD',IDMAX_GRNOD,LSUBMODEL)
      CALL HM_GET_MAX_ID('/LINE',IDMAX_LINE,LSUBMODEL)
      CALL HM_CONVERT_INTER_TYPE19(IDMAX_INTER,IDMAX_GRNOD,IDMAX_LINE,LSUBMODEL)
C-------------------------------------------------------------------
C Convert /FAIL/TAB in memory to /FAIL/TAB1 & /TABLE
C-------------------------------------------------------------------
      CALL HM_GET_MAX_ID('/TABLE',IDMAX_TABLE,LSUBMODEL)
      CALL HM_GET_MAX_ID('/FUNCT',IDMAX_FUNCT,LSUBMODEL)
      CALL HM_GET_MAX_ID('/FAIL',IDMAX_FAIL,LSUBMODEL)
      IF (IS_DYNA == 0) CALL HM_CONVERT_FAIL_TAB(MAX(IDMAX_FUNCT,IDMAX_TABLE),IDMAX_FAIL,LSUBMODEL)
C-------------------------------------------------------------------
C Convert /SHELL & /MAT/LAW119 to /SPRING /PART /PROP/TYPE23 & /MAT/LAW114 
C Convert /TH & /SECT making reference to springs translated from shells
C-------------------------------------------------------------------
c Count number of translated shell elements
      NB_SEATBELT_SHELLS = 0
      CALL HM_COUNT_2D_ELEMENT_SEATBELT(NB_SEATBELT_SHELLS,LSUBMODEL)
      ALLOCATE (SEATBELT_CONVERTED_ELEMENTS(3,NB_SEATBELT_SHELLS),STAT=stat)
      SEATBELT_CONVERTED_ELEMENTS(1:3,1:NB_SEATBELT_SHELLS) = 0
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='SEATBELT_CONVERTED_ELEMENTS')
c Get max ids in the input deck
      IDMAX_PART = 0
      IDMAX_PROP = 0
      IDMAX_MAT = 0
      IDMAX_ELEM = 0
      IDMAX_TH = 0
      CALL HM_GET_MAX_ID('/PART',IDMAX_PART,LSUBMODEL)
      CALL HM_GET_MAX_ID('/PROP',IDMAX_PROP,LSUBMODEL)
      CALL HM_GET_MAX_ID('/MAT',IDMAX_MAT,LSUBMODEL)
      CALL HM_GET_MAX_ID('/SPRING',IDMAX_ELEM,LSUBMODEL)
      CALL HM_GET_MAX_ID('/TH',IDMAX_TH,LSUBMODEL)
c 2d seatbelt translation
      IF (NB_SEATBELT_SHELLS > 0) 
     .  CALL HM_CONVERT_2D_ELEMENTS_SEATBELT(IDMAX_PART,IDMAX_PROP,IDMAX_MAT,IDMAX_ELEM,IDMAX_TH,
     .                                           SEATBELT_CONVERTED_ELEMENTS,NB_SEATBELT_SHELLS,LSUBMODEL)
C------------------------------------------------------------------
      ! Initialisation of glob_therm module
C------------------------------------------------------------------
      call glob_therm_init(glob_therm)
C-------------------------------------------------------------------
C     Control variables (count options)
C-------------------------------------------------------------------
      CALL CONTRL(MULTI_FVM,LSUBMODEL,IS_DYNA,DETONATORS,USER_WINDOWS,MAT_ELEM,
     .            NAMES_AND_TITLES,LIPART1,DEFAULTS,GLOB_THERM,PBLAST,OUTPUT)
C-------------------------------------------------------------------
C     Parameters modification and parameters write (-HSTP_READ -HSTP_WRITE ARGUMENTS)
C-------------------------------------------------------------------
#ifdef DNC
      IF (GOT_HSTP_READ == 1 .OR. GOT_HSTP_WRITE == 1) THEN
        CALL HSTP(ROOTN,ROOTLEN,LSUBMODEL,STARTDATE,STARTTIME,FILNAM,OUTFILE_NAME_LEN)
      ENDIF
#endif
C-----------------------------------------------------
      CALL TRACE_OUT1()
C-----------------------------------------------------
c     stop CPU timer for interval between translator and lectur
c     (freeform)
      CALL STOPTIME(4,1)

      IF(IRUN==0)THEN
C--------------------------------------------------
C      Initial run
C--------------------------------------------------
        IRESMD=0
C
        INVSTR=INVERS
        ERR_MSG='DECK READING'
        ERR_CATEGORY='DECK READING'
        CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))

c----------- Child process creation for multidomains-----
        IF (NSUBDOM>0) THEN
            CALL R2R_FORK(CHRUN,FILNAM,LSUBMODEL)
        ENDIF

c       start CPU timer for lectur
        CALL STARTIME(2,1)
C--------------------------------------------------
C      Checksum option
C--------------------------------------------------
       call hm_read_checksum(LENI,INPUT,LENP,PATH,OUTPUT)
C--------------------------------------------------
C      Main reading routine
C--------------------------------------------------
        CALL LECTUR(
     .       MULTI_FVM                   ,LSUBMODEL         ,IS_DYNA          ,DETONATORS   ,EBCS_TAB,
     .       SEATBELT_CONVERTED_ELEMENTS ,NB_SEATBELT_SHELLS,NB_DYNA_INCLUDE  ,USER_WINDOWS ,OUTPUT  ,
     .       MAT_ELEM,NAMES_AND_TITLES,DEFAULTS,glob_therm,PBLAST,SENSOR_USER_STRUCT)

c       stop CPU timer for lectur
        CALL STOPTIME(2,1)

        IF (IPID/=0) CLOSE(IIN2)
        CALL TRACE_OUT1()

      ELSE
C--------------------------------------------------
C     Modification run (deprecated)
C --------------------------------------------------

        CALL ANCMSG(MSGID=1621,
     .            MSGTYPE=MSGERROR,
     .            ANMODE=ANSTOP)

      ENDIF
C
      IF(ALLOCATED(SEATBELT_CONVERTED_ELEMENTS)) DEALLOCATE(SEATBELT_CONVERTED_ELEMENTS)  
      CALL USER_WINDOWS_CLEAN(USER_WINDOWS)
      CALL TH_CLEAN(OUTPUT%TH)
C
      IF (IERR==0)THEN
        !   -----------------------------
        !   write the message only if restart 
        !   files are generated
        IF(RESTART_FILE==1) THEN
                WRITE(CHRUNR,'(I4.4)')IRUN
                FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHRUNR
                WRITE (IOUT,80) FILNAM(1:ROOTLEN+5)
 80             FORMAT (/4X,14H RESTART FILES:,1X,A,8H WRITTEN/
     .                  4X,14H -------------/)
        END IF
        !   -----------------------------
      ELSE
        WRITE(IOUT,'(A)')TITRE(47)
      ENDIF
c     print timers
      CALL PRINTIME(1,GOT_TIMER,STARTDATE,STARTTIME,ENDDATE,ENDTIME)
C
      IF (IPID/=0) THEN
#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
        CLOSE(UNIT=ISTDI, STATUS='DELETE', IOSTAT=IO_ERR)
#else
        CLOSE(UNIT=ISTDI, STATUS='DELETE', IOSTAT=IO_ERR)
#endif
        IF (IO_ERR /= 0) THEN
          WRITE(6,*)' '
          WRITE(6,*)'*** ERROR CLOSING TEMPORARY INPUT FILE'
        ENDIF

        CALL CPP_DELETE_MODEL()
      ENDIF
C
      CALL TRACE_OUT1()

      CALL F_ANEND(OUT_FILE_NAME,LEN_TRIM(OUT_FILE_NAME),ROOTNAM,
     *             ROOTLEN,ENDDATE,ENDTIME,OUTPUT)
C---  
      print*,'done'
      RETURN
C---  
      END
