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
Chd|  HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|-- called by -----------
Chd|        READ_MATERIAL_MODELS          source/materials/read_material_models.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_READ_MAT00                 source/materials/mat/mat000/hm_read_mat00.F
Chd|        HM_READ_MAT01                 source/materials/mat/mat001/hm_read_mat01.F
Chd|        HM_READ_MAT02                 source/materials/mat/mat002/hm_read_mat02.F
Chd|        HM_READ_MAT03                 source/materials/mat/mat003/hm_read_mat03.F
Chd|        HM_READ_MAT04                 source/materials/mat/mat004/hm_read_mat04.F
Chd|        HM_READ_MAT05                 source/materials/mat/mat005/hm_read_mat05.F
Chd|        HM_READ_MAT06                 source/materials/mat/mat006/hm_read_mat06.F
Chd|        HM_READ_MAT06_KEPS            source/materials/mat/mat006/hm_read_mat06_keps.F
Chd|        HM_READ_MAT10                 source/materials/mat/mat010/hm_read_mat10.F
Chd|        HM_READ_MAT100                source/materials/mat/mat100/hm_read_mat100.F
Chd|        HM_READ_MAT101                source/materials/mat/mat101/hm_read_mat101.F
Chd|        HM_READ_MAT102                source/materials/mat/mat102/hm_read_mat102.F
Chd|        HM_READ_MAT103                source/materials/mat/mat103/hm_read_mat103.F
Chd|        HM_READ_MAT104                source/materials/mat/mat104/hm_read_mat104.F
Chd|        HM_READ_MAT106                source/materials/mat/mat106/hm_read_mat106.F
Chd|        HM_READ_MAT107                source/materials/mat/mat107/hm_read_mat107.F
Chd|        HM_READ_MAT108                source/materials/mat/mat108/hm_read_mat108.F
Chd|        HM_READ_MAT109                source/materials/mat/mat109/hm_read_mat109.F
Chd|        HM_READ_MAT11                 source/materials/mat/mat011/hm_read_mat11.F
Chd|        HM_READ_MAT110                source/materials/mat/mat110/hm_read_mat110.F
Chd|        HM_READ_MAT111                source/materials/mat/mat111/hm_read_mat111.F
Chd|        HM_READ_MAT112                source/materials/mat/mat112/hm_read_mat112.F
Chd|        HM_READ_MAT113                source/materials/mat/mat113/hm_read_mat113.F
Chd|        HM_READ_MAT114                source/materials/mat/mat114/hm_read_mat114.F
Chd|        HM_READ_MAT115                source/materials/mat/mat115/hm_read_mat115.F
Chd|        HM_READ_MAT116                source/materials/mat/mat116/hm_read_mat116.F
Chd|        HM_READ_MAT117                source/materials/mat/mat117/hm_read_mat117.F
Chd|        HM_READ_MAT119                source/materials/mat/mat119/hm_read_mat119.F
Chd|        HM_READ_MAT11_K_EPS           source/materials/mat/mat011/hm_read_mat11_k_eps.F
Chd|        HM_READ_MAT12                 source/materials/mat/mat012/hm_read_mat12.F
Chd|        HM_READ_MAT120                source/materials/mat/mat120/hm_read_mat120.F
Chd|        HM_READ_MAT121                source/materials/mat/mat121/hm_read_mat121.F
Chd|        HM_READ_MAT13                 source/materials/mat/mat013/hm_read_mat13.F
Chd|        HM_READ_MAT14                 source/materials/mat/mat014/hm_read_mat14.F
Chd|        HM_READ_MAT15                 source/materials/mat/mat015/hm_read_mat15.F
Chd|        HM_READ_MAT151                source/materials/mat/mat151/hm_read_mat151.F
Chd|        HM_READ_MAT16                 source/materials/mat/mat016/hm_read_mat16.F
Chd|        HM_READ_MAT18                 source/materials/mat/mat018/hm_read_mat18.F
Chd|        HM_READ_MAT19                 source/materials/mat/mat019/hm_read_mat19.F
Chd|        HM_READ_MAT20                 source/materials/mat/mat020/hm_read_mat20.F
Chd|        HM_READ_MAT21                 source/materials/mat/mat021/hm_read_mat21.F
Chd|        HM_READ_MAT22                 source/materials/mat/mat022/hm_read_mat22.F
Chd|        HM_READ_MAT23                 source/materials/mat/mat023/hm_read_mat23.F
Chd|        HM_READ_MAT24                 source/materials/mat/mat024/hm_read_mat24.F
Chd|        HM_READ_MAT25                 source/materials/mat/mat025/hm_read_mat25.F
Chd|        HM_READ_MAT26                 source/materials/mat/mat026/hm_read_mat26.F
Chd|        HM_READ_MAT27                 source/materials/mat/mat027/hm_read_mat27.F
Chd|        HM_READ_MAT28                 source/materials/mat/mat028/hm_read_mat28.F
Chd|        HM_READ_MAT29_31              source/materials/mat/matuser/hm_read_mat_user29_31.F
Chd|        HM_READ_MAT32                 source/materials/mat/mat032/hm_read_mat32.F
Chd|        HM_READ_MAT33                 source/materials/mat/mat033/hm_read_mat33.F
Chd|        HM_READ_MAT34                 source/materials/mat/mat034/hm_read_mat34.F
Chd|        HM_READ_MAT35                 source/materials/mat/mat035/hm_read_mat35.F
Chd|        HM_READ_MAT36                 source/materials/mat/mat036/hm_read_mat36.F
Chd|        HM_READ_MAT37                 source/materials/mat/mat037/hm_read_mat37.F
Chd|        HM_READ_MAT38                 source/materials/mat/mat038/hm_read_mat38.F
Chd|        HM_READ_MAT40                 source/materials/mat/mat040/hm_read_mat40.F
Chd|        HM_READ_MAT41                 source/materials/mat/mat041/hm_read_mat41.F.F
Chd|        HM_READ_MAT42                 source/materials/mat/mat042/hm_read_mat42.F
Chd|        HM_READ_MAT43                 source/materials/mat/mat043/hm_read_mat43.F
Chd|        HM_READ_MAT44                 source/materials/mat/mat044/hm_read_mat44.F
Chd|        HM_READ_MAT46                 source/materials/mat/mat046/hm_read_mat46.F
Chd|        HM_READ_MAT48                 source/materials/mat/mat048/hm_read_mat48.F
Chd|        HM_READ_MAT49                 source/materials/mat/mat049/hm_read_mat49.F
Chd|        HM_READ_MAT50                 source/materials/mat/mat050/hm_read_mat50.F
Chd|        HM_READ_MAT51                 source/materials/mat/mat051/hm_read_mat51.F
Chd|        HM_READ_MAT52                 source/materials/mat/mat052/hm_read_mat52.F
Chd|        HM_READ_MAT53                 source/materials/mat/mat053/hm_read_mat53.F
Chd|        HM_READ_MAT54                 source/materials/mat/mat054/hm_read_mat54.F
Chd|        HM_READ_MAT57                 source/materials/mat/mat057/hm_read_mat57.F
Chd|        HM_READ_MAT58                 source/materials/mat/mat058/hm_read_mat58.F
Chd|        HM_READ_MAT59                 source/materials/mat/mat059/hm_read_mat59.F
Chd|        HM_READ_MAT60                 source/materials/mat/mat060/hm_read_mat60.F
Chd|        HM_READ_MAT62                 source/materials/mat/mat062/hm_read_mat62.F
Chd|        HM_READ_MAT63                 source/materials/mat/mat063/hm_read_mat63.F
Chd|        HM_READ_MAT64                 source/materials/mat/mat064/hm_read_mat64.F
Chd|        HM_READ_MAT65                 source/materials/mat/mat065/hm_read_mat65.F
Chd|        HM_READ_MAT66                 source/materials/mat/mat066/hm_read_mat66.F
Chd|        HM_READ_MAT68                 source/materials/mat/mat068/hm_read_mat68.F
Chd|        HM_READ_MAT69                 source/materials/mat/mat069/hm_read_mat69.F
Chd|        HM_READ_MAT70                 source/materials/mat/mat070/hm_read_mat70.F
Chd|        HM_READ_MAT71                 source/materials/mat/mat071/hm_read_mat71.F
Chd|        HM_READ_MAT72                 source/materials/mat/mat072/hm_read_mat72.F
Chd|        HM_READ_MAT73                 source/materials/mat/mat073/hm_read_mat73.F
Chd|        HM_READ_MAT74                 source/materials/mat/mat074/hm_read_mat74.F
Chd|        HM_READ_MAT75                 source/materials/mat/mat075/hm_read_mat75.F
Chd|        HM_READ_MAT76                 source/materials/mat/mat076/hm_read_mat76.F
Chd|        HM_READ_MAT77                 source/materials/mat/mat077/hm_read_mat77.F
Chd|        HM_READ_MAT78                 source/materials/mat/mat078/hm_read_mat78.F
Chd|        HM_READ_MAT79                 source/materials/mat/mat079/hm_read_mat79.F
Chd|        HM_READ_MAT80                 source/materials/mat/mat080/hm_read_mat80.F
Chd|        HM_READ_MAT81                 source/materials/mat/mat081/hm_read_mat81.F
Chd|        HM_READ_MAT82                 source/materials/mat/mat082/hm_read_mat82.F
Chd|        HM_READ_MAT83                 source/materials/mat/mat083/hm_read_law83.F
Chd|        HM_READ_MAT84                 source/materials/mat/mat084/hm_read_mat84.F
Chd|        HM_READ_MAT87                 source/materials/mat/mat087/hm_read_mat87.F
Chd|        HM_READ_MAT88                 source/materials/mat/mat088/hm_read_mat88.F
Chd|        HM_READ_MAT90                 source/materials/mat/mat090/hm_read_mat90.F
Chd|        HM_READ_MAT92                 source/materials/mat/mat092/hm_read_mat92.F
Chd|        HM_READ_MAT93                 source/materials/mat/mat093/hm_read_mat93.F
Chd|        HM_READ_MAT94                 source/materials/mat/mat094/hm_read_mat94.F
Chd|        HM_READ_MAT95                 source/materials/mat/mat095/hm_read_mat95.F
Chd|        HM_READ_MAT97                 source/materials/mat/mat097/hm_read_mat97.F
Chd|        HM_READ_MATGAS                source/materials/mat/matgas/hm_read_matgas.F
Chd|        HM_READ_MAT_99                source/materials/mat/matuser/hm_read_mat_user_99.F
Chd|        INIT_MAT_KEYWORD              source/materials/mat/init_mat_keyword.F
Chd|        VDOUBLE                       source/system/sysfus.F        
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        LAW_USER                      source/user_interface/law_user.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE HM_READ_MAT(
     .   MATPARAM_TAB,MLAW_TAG    ,FAIL_TAG    ,EOS_TAG     ,BUFMAT      ,
     .   BUFLEN      ,IADBUF      ,MAXEOS      ,IPM         ,PM          ,
     .   MULTI_FVM   ,UNITAB      ,LSUBMODEL   ,TABLE)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ MATERIALS WITH HM READER
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD
      USE LAW_USER
      USE MULTI_FVM_MOD
      USE SUBMODEL_MOD
      USE MATPARAM_DEF_MOD
      USE HM_OPTION_READ_MOD
      USE TABLE_MOD
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "tablen_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
#include      "warn_c.inc"
#include      "sysunit.inc"
#include      "userlib.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "alefvm.inc"
#include      "mmale20_c.inc"
#include      "mmale51_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER ,INTENT(IN)    :: MAXEOS
      INTEGER ,INTENT(INOUT) :: BUFLEN,IADBUF
      INTEGER ,DIMENSION(NPROPMI,NUMMAT), INTENT(INOUT) :: IPM
      my_real ,DIMENSION(NPROPM ,NUMMAT), INTENT(INOUT) :: PM
      my_real ,DIMENSION(*), INTENT(INOUT) :: BUFMAT

      TYPE(MLAW_TAG_), TARGET, DIMENSION(NUMMAT)      :: MLAW_TAG
      TYPE(EOS_TAG_) , TARGET, DIMENSION(0:MAXEOS) ,INTENT(INOUT) :: EOS_TAG
      TYPE(FAIL_TAG_), TARGET, DIMENSION(0:MAXFAIL),INTENT(INOUT) :: FAIL_TAG
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      TYPE(MATPARAM_STRUCT_) , TARGET, DIMENSION(NUMMAT),INTENT(INOUT) :: MATPARAM_TAB
      TYPE(SUBMODEL_DATA),INTENT(IN) ::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,I11,MAT_ID,UID,ILAW,JALE,JTUR,JTHE,
     .   IMATVIS,ISRATE,IUSER_LAW,NFUNC,NUMTABL,NUPARAM,NUVAR,NVARTMP,
     .    MAXUPARAM,MAXFUNC,MAXTABL,IUNIT,IFLAGUNIT,K
      PARAMETER (MAXUPARAM = 1048576) 
      PARAMETER (MAXFUNC  = 128, MAXTABL = 9)
      my_real :: RHO,RHO0,RHOR,YOUNG,NU,C1,G,SSP,ASRATE,RBID
      INTEGER ,DIMENSION(MAXFUNC)  :: IFUNC
      INTEGER ,DIMENSION(MAXTABL)  :: ITABLE
      my_real ,DIMENSION(:), ALLOCATABLE :: UPARAM
      my_real ,DIMENSION(128)      :: PARMAT   
      CHARACTER TITR*nchartitle,KEY*ncharline,SOLVERKEYWORD*ncharline,MESS*40
      CHARACTER(LEN = ncharline) :: KEY2
C
      TYPE(ULAWBUF) :: USERBUF
      TYPE(MATPARAM_STRUCT_) , POINTER :: MATPARAM
      TYPE(MLAW_TAG_) ,POINTER         :: MTAG
      TYPE(TTABLE) TABLE(NTABLE)
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
      DATA MESS/'MATERIAL DEFINITION                     '/
C=======================================================================

      ALLOCATE( UPARAM(MAXUPARAM) )

      ILAW          = 0
      IUSER_LAW     = 0
      IMATVIS       = 0
      TRIMAT        = 0
      lSET_51_IFLG6 = 0
      lSET_51_IFLG6 = 0
            
      SSP0MAX       = ZERO
      LC0MAX        = ZERO
      TCP_REF       = ZERO

      MULTI_FVM%IS_USED = .FALSE.
      DISCRETE_FILL     = .FALSE.  ! BIMAT : PARTIAL FILL ENABLED BY DEFAULT
c     
C--------------------------------------------------
C START BROWSING MODEL MATERIALS
C--------------------------------------------------
c
      CALL HM_OPTION_START('MATERIAL')
c
c--------------------------------------------      
c     CALL MATERIAL LAW READING ROUTINES
c--------------------------------------------      
c
      DO I=1,HM_NUMMAT
c
        MAT_NUMBER    = I 
        SOLVERKEYWORD = ''
        TITR = ''
        KEY  = ''
            
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = MAT_ID,
     .                          OPTION_TITR = TITR  ,
     .                          UNIT_ID     = UID   ,
     .                          KEYWORD2    = KEY   ,
     .                          KEYWORD3    = KEY2)

C--------------------------------------------------
C       CHECK IF UID EXISTS
C--------------------------------------------------
        IFLAGUNIT = 0
        DO IUNIT=1,NUNITS
          IF (UNITAB%UNIT_ID(IUNIT) == UID) THEN
            IFLAGUNIT = 1
            EXIT
          ENDIF
        ENDDO
        IF (UID > 0 .AND. IFLAGUNIT == 0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I2=UID,I1=MAT_ID,
     .                C1='MATERIAL',
     .                C2='MATERIAL',
     .                C3='TITR')                              
        ENDIF
c----        
        CALL FRETITL(TITR,IPM(NPROPMI-LTITR+1,I),LTITR)
        USERBUF%ID   = MAT_ID   !IPM(1,I)
        USERBUF%NAME = TITR(1:nchartitle)          
c
        PARMAT(:) = ZERO
        UPARAM(:) = ZERO
        IFUNC(:)  = 0
        ITABLE(:) = 0
        JTUR    = 0
        JALE    = 0
        JTHE    = 0
c---------------------------------------------------      
        ISRATE  = -1
c           strain rate flag :
c               ISRATE =-1 => no strain rate computation (default)
c               ISRATE = 0 => strain rate computation (for output only), no filtering
c               ISRATE > 0 => strain rate filtering using Fcut and exponential average
c---------------------------------------------------      
        IMATVIS = 0
        NFUNC   = 0
        NUMTABL = 0
        NUVAR   = 0
        NVARTMP = 0
        NUPARAM = 0
        MTAG => MLAW_TAG(MAT_NUMBER)
        MATPARAM => MATPARAM_TAB(MAT_NUMBER)
        MATPARAM%TITLE = ' '
        MATPARAM%TITLE = TITR(1:LEN_TRIM(TITR)-1)
c-----------------------------------------------------------------------
c
        IF (LEN_TRIM(KEY2) == 0) THEN
!     In case of /MAT/GAS, there is a third keyword,hence, KEY does not end with \000 character
           KEY = KEY(1:LEN_TRIM(KEY))
        ENDIF
        SELECT CASE(KEY)
c-------
          CASE ('LAW0','VOID')
            ILAW = 0  
            CALL HM_READ_MAT00(MTAG   ,
     .           IPM(1,I), PM(1,I), UNITAB, MAT_ID, TITR, LSUBMODEL,ISRATE)
c-------
          CASE ('LAW1','LAW01','ELAST')
            ILAW = 1  
            CALL HM_READ_MAT01(
     .           IPM(1,I), PM(1,I), UNITAB, MAT_ID, TITR, LSUBMODEL,ISRATE,
     .           MATPARAM)
c-------
          CASE ('LAW2','LAW02','PLAS_JOHNS','JOHNS')
            ILAW  = 2  
            CALL HM_READ_MAT02(
     .                       UPARAM ,MAXUPARAM ,NUPARAM  ,NUVAR     ,IFUNC   ,
     .                       MAXFUNC,NFUNC     ,PARMAT   ,IMATVIS   ,0       ,
     .                       UNITAB ,MAT_ID    ,TITR     ,LSUBMODEL ,MTAG    ,
     .                       PM(1,I),IPM(1,I)  ,ISRATE   ,MATPARAM  )
c-------
          CASE ('ZERIL','PLAS_ZERIL')
            ILAW  = 2 
            CALL HM_READ_MAT02(
     .                       UPARAM ,MAXUPARAM ,NUPARAM  ,NUVAR     ,IFUNC   ,
     .                       MAXFUNC,NFUNC     ,PARMAT   ,IMATVIS   ,1       ,
     .                       UNITAB ,MAT_ID    ,TITR     ,LSUBMODEL ,MTAG    ,
     .                       PM(1,I),IPM(1,I)  ,ISRATE   ,MATPARAM  )
c-------
          CASE ('PLAS_PREDEF')
            ILAW  = 2 
            CALL HM_READ_MAT02(
     .                       UPARAM ,MAXUPARAM ,NUPARAM  ,NUVAR     ,IFUNC   ,
     .                       MAXFUNC,NFUNC     ,PARMAT   ,IMATVIS   ,2       ,
     .                       UNITAB ,MAT_ID    ,TITR     ,LSUBMODEL ,MTAG    ,
     .                       PM(1,I),IPM(1,I)  ,ISRATE   ,MATPARAM  )
c-------
          CASE ('LAW3','LAW03', 'HYDPLA')
            ILAW=3
            CALL HM_READ_MAT03(
     .           LSUBMODEL ,MTAG     ,UNITAB   ,IPM(1,I)  ,PM(1,I),
     .           MAT_ID    ,TITR     ,EOS_TAG  ,MAXEOS    ,ISRATE)
c-------
          CASE ('LAW4','LAW04', 'HYD_JCOOK')
            ILAW=4
            CALL HM_READ_MAT04(
     .           LSUBMODEL ,MTAG     ,UNITAB   ,IPM(1,I)  ,PM(1,I),
     .           MAT_ID    ,TITR     ,EOS_TAG  ,MAXEOS    ,ISRATE)
c-------
          CASE ('JWL','LAW5','LAW05')
            ILAW = 5
            CALL HM_READ_MAT05(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  ,IPM(1,I)  ,UID)
c-------
          CASE ('LAW06','LAW6', 'HYDRO','HYD_VISC')
            ILAW  = 6  
            CALL HM_READ_MAT06(
     .           LSUBMODEL ,MTAG     ,UNITAB   ,IPM(1,I)  ,PM(1,I),
     .           MAT_ID    ,TITR     ,EOS_TAG  ,MAXEOS    ,ISRATE,
     .           1 , MTAG   )
c-------
          CASE ('K-EPS')
            ILAW = 6  
            JTUR=1
            PM(70,I) =JTUR+EM01
            CALL HM_READ_MAT06_KEPS(
     .           LSUBMODEL ,MTAG     ,UNITAB   ,IPM(1,I)  ,PM(1,I),
     .           MAT_ID    ,TITR     ,EOS_TAG  ,MAXEOS    ,ISRATE,
     .           1 , MTAG   )      
c-------
          CASE ('LAW10','DPRAG1')
            ILAW = 10  
            CALL HM_READ_MAT10(
     .           LSUBMODEL ,MTAG     ,UNITAB   ,IPM(1,I)  ,PM(1,I),
     .           MAT_ID    ,TITR     ,EOS_TAG  ,MAXEOS    ,ISRATE)
c-------
          CASE ('LAW11','BOUND')
            ILAW = 11  
            CALL HM_READ_MAT11(
     .           LSUBMODEL ,MTAG     ,UNITAB   ,IPM(1,I)  ,PM(1,I),
     .           MAT_ID    ,TITR)
c-------
          CASE ('B-K-EPS')
            ILAW = 11 
            JTUR=1 
            PM(70,I) =JTUR+EM01
            CALL HM_READ_MAT11_K_EPS(
     .           LSUBMODEL ,MTAG     ,UNITAB   ,IPM(1,I)  ,PM(1,I),
     .           MAT_ID    ,TITR)     
c-------
          CASE ('LAW12','3D_COMP')
            ILAW = 12  
            CALL HM_READ_MAT12(
     .           LSUBMODEL ,MTAG     ,UNITAB   ,IPM(1,I)  ,PM(1,I),
     .           MAT_ID    ,TITR     ,EOS_TAG  ,MAXEOS    ,ISRATE,
     .           MATPARAM  )
c-------
          CASE ('LAW13','RIGID')
            ILAW = 13  
            CALL HM_READ_MAT13(
     .           LSUBMODEL ,MTAG     ,UNITAB   ,IPM(1,I)  ,PM(1,I),
     .           MAT_ID    ,TITR     ,MATPARAM )
c-------
          CASE ('LAW14','COMPSO')
            ILAW  = 14  
            CALL HM_READ_MAT14(
     .           LSUBMODEL ,MTAG     ,UNITAB   ,IPM(1,I)  ,PM(1,I),
     .           MAT_ID    ,TITR     ,MATPARAM )
c-------
          CASE ('LAW15','CHANG')
            ILAW  = 15  
            CALL HM_READ_MAT15(
     .           LSUBMODEL ,MTAG     ,UNITAB   ,IPM(1,I)  ,PM(1,I),
     .           MAT_ID    ,TITR     ,MATPARAM )
c-------
          CASE ('LAW16','GRAY')
            ILAW  = 16  
            CALL HM_READ_MAT16(
     .           MTAG     ,PM(1,I)    ,MAT_ID   ,TITR  ,IPM(1,I)   ,
     .           LSUBMODEL,UNITAB     ,MATPARAM )
c-------
          CASE ('LAW18','THERM')
            ILAW  = 18  
            CALL HM_READ_MAT18(
     .           NUPARAM  ,NUVAR    ,NFUNC    ,MAT_ID   ,TITR     ,
     .           UNITAB   ,LSUBMODEL,MTAG     ,PM(1,I)  ,IPM(1,I) ,
     .           JTHE     )
c-------
          CASE ('LAW19','FABRI')
            ILAW = 19 
            CALL HM_READ_MAT19(
     .           MTAG     ,UPARAM   ,MAXUPARAM,NUPARAM  ,PM(1,I)  ,
     .           MATPARAM ,PARMAT   ,NUVAR    ,MAT_ID   ,TITR     ,
     .           UNITAB   ,LSUBMODEL,ISRATE   )
c-------
          CASE ('LAW20','BIMAT')
            ILAW  = 20 
            CALL HM_READ_MAT20(
     .           IPM(1,I), PM(1,I), UNITAB, MAT_ID, TITR, LSUBMODEL, MTAG)         
c-------
          CASE ('LAW21','DPRAG')
            ILAW = 21  
            CALL HM_READ_MAT21(
     .           LSUBMODEL ,MTAG     ,UNITAB   ,IPM(1,I)  ,PM(1,I),
     .           MAT_ID    ,TITR     )
          CASE ('LAW22','DAMA')
            ILAW  = 22  
            CALL HM_READ_MAT22(
     .                       UPARAM ,MAXUPARAM ,NUPARAM  ,NUVAR     ,IFUNC   ,
     .                       MAXFUNC,NFUNC     ,PARMAT   ,IMATVIS   ,0       ,
     .                       UNITAB ,MAT_ID    ,TITR     ,LSUBMODEL ,MTAG    ,
     .                       PM(1,I),IPM(1,I)  ,ISRATE  ) 
c-------
         CASE ('LAW23','PLAS_DAM')
            ILAW  = 23  
            CALL HM_READ_MAT23(
     .                       UPARAM ,MAXUPARAM ,NUPARAM  ,NUVAR     ,IFUNC   ,
     .                       MAXFUNC,NFUNC     ,PARMAT   ,IMATVIS   ,0       ,
     .                       UNITAB ,MAT_ID    ,TITR     ,LSUBMODEL ,MTAG    ,
     .                       PM(1,I),IPM(1,I)  ,ISRATE  )
c-------
          CASE ('LAW24','CONC')
            ILAW = 24  
            CALL HM_READ_MAT24(
     .           NUPARAM  ,NUVAR    ,NFUNC    ,IPM(1,I) ,PM(1,I)  ,
     .           MTAG     ,MAT_ID   ,TITR     ,UNITAB   ,LSUBMODEL,
     .           ISRATE   )
c-------
          CASE ('LAW25','COMPSH')
            ILAW = 25  
            CALL HM_READ_MAT25(
     .           UPARAM   ,MAXUPARAM,NUPARAM   ,UNITAB   ,LSUBMODEL,
     .           MTAG     ,MAT_ID   ,TITR      ,PM(1,I)  ,ISRATE   ,
     .           PARMAT   ,MATPARAM )
c-------
          CASE ('LAW26','SESAM')
            ILAW = 26  
            CALL HM_READ_MAT26(
     .              MTAG     ,PM(1,I),MAT_ID   ,TITR  ,IPM(1,I) ,
     .              JTHE     ,BUFMAT ,BUFLEN  ,IADBUF ,LSUBMODEL,
     .              UNITAB   )
c-------
          CASE ('LAW27','PLAS_BRIT')
            ILAW = 27  
            CALL HM_READ_MAT27(
     .           MTAG     ,PARMAT   ,UNITAB   ,PM(1,I)  ,LSUBMODEL,
     .           ISRATE   ,MAT_ID   ,TITR     )
c-------
          CASE ('LAW28','HONEYCOMB')
            ILAW = 28  
            CALL HM_READ_MAT28(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  ,MATPARAM  )
c-------
          CASE ('LAW32','HILL')
            ILAW = 32  
            CALL HM_READ_MAT32(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  ,IPM(1,I) )
c-------
          CASE ('LAW33','FOAM_PLAS')
            ILAW = 33  
            CALL HM_READ_MAT33(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC    ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID   ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  ,MATPARAM )
c-------
          CASE ('LAW34','BOLTZMAN')
            ILAW  = 34 
            CALL HM_READ_MAT34(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC    ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID   ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  )
c-------
          CASE ('LAW35','FOAM_VISC')
            ILAW = 35  
            CALL HM_READ_MAT35(
     .           UPARAM   ,MAXUPARAM,NUPARAM   ,NUVAR    ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,PARMAT    ,UNITAB   ,PM(1,I)  ,
     .           ISRATE   ,MAT_ID   ,TITR      ,LSUBMODEL,IMATVIS  ,
     .           MTAG     )
c-------
          CASE ('LAW36','PLAS_TAB')
            ILAW  = 36             
            CALL HM_READ_MAT36(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NVARTMP  ,
     .           IFUNC    ,MAXFUNC  ,NFUNC    ,PARMAT   ,UNITAB   ,
     .           MAT_ID   ,MTAG     ,TITR     ,LSUBMODEL,PM(1,I)  ,
     .           ISRATE   )
c-------
          CASE ('LAW37','BIPHAS')
            ILAW   = 37  
            ISRATE = 1
            CALL HM_READ_MAT37(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  ,PARMAT    ,MAT_ID)
c-------
          CASE ('LAW38','VISC_TAB')
            ILAW  = 38             
            CALL HM_READ_MAT38(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,PARMAT   ,UNITAB   ,MAT_ID   ,
     .           MTAG     ,TITR     ,LSUBMODEL,PM(1,I)  ,IMATVIS  ,
     .           MATPARAM )
c-------
          CASE ('LAW40','KELVINMAX')
            ILAW  = 40             
            CALL HM_READ_MAT40(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,
     .           MAXFUNC  ,NFUNC    ,PARMAT   ,UNITAB   ,MAT_ID   ,
     .           MTAG     ,TITR     ,LSUBMODEL,PM(1,I)  ,IMATVIS  ,
     .           MATPARAM )
c-------
          CASE ('LAW41', 'LEE_TARVER')
            ILAW = 41 
            CALL HM_READ_MAT41(UPARAM, MAXUPARAM, NUPARAM,
     .           NUVAR, IFUNC, MAXFUNC, NFUNC, PARMAT,
     .           MAT_ID, TITR, UNITAB, LSUBMODEL, PM(1, I),
     .           MATPARAM )
c-------
          CASE ('LAW42','OGDEN')
            ILAW = 42  
            CALL HM_READ_MAT42(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,PARMAT   ,IMATVIS  ,UNITAB   ,
     .           MAT_ID   ,TITR     ,LSUBMODEL,PM(1,I)  ,MATPARAM )
c-------
          CASE ('LAW43','HILL_TAB')
            ILAW = 43
            CALL HM_READ_MAT43(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  ,IPM(1,I) )
c-------
          CASE ('LAW44','COWPER')
            ILAW = 44  
            CALL HM_READ_MAT44(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NFUNC    ,
     .           MTAG     ,PARMAT   ,UNITAB   ,PM(1,I)  ,LSUBMODEL,
     .           ISRATE   ,MAT_ID   ,TITR     ,IFUNC    ,MAXFUNC  ,
     .           NVARTMP  )
c-------
          CASE ('LAW46','LES_FLUID')
            ILAW = 46
            CALL HM_READ_MAT46(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  ,IPM(1,I) )
c-------
          CASE ('LAW48','ZHAO')
            ILAW = 48
            CALL HM_READ_MAT48(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,PARMAT   ,
     .           MAT_ID   ,PM(1,I)  ,TITR     ,UNITAB   ,LSUBMODEL,
     .           ISRATE   ,NFUNC    ,MTAG     )
c-------
          CASE ('LAW49','STEINB')
            ILAW=49
            CALL HM_READ_MAT49(
     .           LSUBMODEL ,MTAG     ,UNITAB   ,IPM(1,I)  ,PM(1,I),
     .           MAT_ID    ,TITR     ,EOS_TAG  ,MAXEOS    ,ISRATE)
c-------
          CASE ('LAW50','VISC_HONEY')
            ILAW  = 50  
            CALL HM_READ_MAT50(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NFUNC    ,
     .           MAXFUNC  ,IFUNC    ,MTAG     ,PARMAT   ,UNITAB   ,
     .           PM(1,I)  ,LSUBMODEL,ISRATE   ,MAT_ID   ,TITR     ,
     .           MATPARAM )
c-------
          CASE ('LAW51','MULTIMAT','TRIMAT')
            ILAW = 51
            CALL HM_READ_MAT51(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  ,IPM(1,I) )
c-------
          CASE ('LAW52','GURSON')
            ILAW  = 52 
            CALL HM_READ_MAT52(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NFUNC    ,
     .           MAXFUNC  ,IFUNC    ,MTAG     ,PARMAT   ,UNITAB   ,
     .           PM(1,I)  ,LSUBMODEL,ISRATE   ,MAT_ID   ,TITR     ,
     .           ITABLE   ,MAXTABL  ,NUMTABL)
c-------
          CASE ('LAW53','TSAI_TAB')
            ILAW = 53
            CALL HM_READ_MAT53(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  ,IPM(1,I)  ,MATPARAM )
c-------
          CASE('LAW54','PREDIT')
             ILAW = 54
             CALL HM_READ_MAT54(
     .            UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,IFUNC    ,
     .            MAXFUNC  ,NFUNC    ,PARMAT   ,UNITAB   ,MAT_ID   ,
     .            MTAG     ,TITR     ,LSUBMODEL,PM(1,I)  )
c-------
          CASE ('LAW57','BARLAT3','BARLAT')
            ILAW  = 57  
            CALL HM_READ_MAT57(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NFUNC    ,
     .           MAXFUNC  ,IFUNC    ,MTAG     ,PARMAT   ,UNITAB   ,
     .           ISRATE   ,PM(1,I)  ,LSUBMODEL,MAT_ID   ,TITR     )
c-------
          CASE ('LAW58','FABR_A')
            ILAW  = 58  
            CALL HM_READ_MAT58(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NFUNC    ,
     .           MAXFUNC  ,IFUNC    ,MTAG     ,PARMAT   ,UNITAB   ,
     .           PM(1,I)  ,LSUBMODEL,MAT_ID   ,TITR     )
c-------
          CASE ('LAW59','CONNECT')
            ILAW  = 59  
            CALL HM_READ_MAT59(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  )
c-------
          CASE ('LAW60','PLAS_T3')
            ILAW = 60
            CALL HM_READ_MAT60(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  ,IPM(1,I) )
c-------
          CASE ('LAW62','VISC_HYP')
            ILAW = 62  
            CALL HM_READ_MAT62(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,NUVAR    ,NFUNC    ,
     .           PARMAT   ,UNITAB    ,PM(1,I)  ,MAT_ID   ,TITR     ,
     .           IMATVIS  ,LSUBMODEL ,MATPARAM )
c-------
          CASE ('LAW63','HANSEL')
            ILAW = 63
            CALL HM_READ_MAT63(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,PARMAT   ,
     .           MAT_ID   ,PM(1,I)  ,TITR     ,UNITAB   ,LSUBMODEL,
     .           MTAG     )
c-------
          CASE ('LAW64','UGINE_ALZ')
            ILAW = 64
            CALL HM_READ_MAT64(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  ,IPM(1,I) )
c-------
          CASE ('LAW65','ELASTOMER')
            ILAW  = 65  
            CALL HM_READ_MAT65(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NFUNC    ,
     .           MAXFUNC  ,IFUNC    ,MTAG     ,PARMAT   ,UNITAB   ,
     .           PM(1,I)  ,LSUBMODEL,ISRATE   ,MAT_ID   ,TITR     )
c-------
          CASE ('LAW66')
            ILAW  = 66             
            CALL HM_READ_MAT66(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,PARMAT   ,UNITAB   ,MAT_ID   ,
     .           MTAG     ,TITR     ,LSUBMODEL,PM(1,I)  )
            ISRATE = 1
c-------
          CASE ('LAW68','COSSER')
            ILAW = 68
            CALL HM_READ_MAT68(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  ,IPM(1,I)  ,MATPARAM )
c-------
          CASE ('LAW69')
            ILAW  = 69  
            CALL HM_READ_MAT69(
     .           UPARAM ,MAXUPARAM,NUPARAM  ,ISRATE   ,IMATVIS  ,
     .           NUVAR  ,IFUNC    ,MAXFUNC  ,NFUNC    ,PARMAT   , 
     .           UNITAB ,MAT_ID   ,TITR     ,MTAG     ,LSUBMODEL,
     .           PM(1,I),MATPARAM )
c-------
          CASE ('LAW70','FOAM_TAB')
            ILAW = 70  
            CALL HM_READ_MAT70(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC    ,MAXFUNC  ,NFUNC    ,PARMAT   ,
     .           UNITAB   ,MAT_ID   ,TITR     ,MTAG     ,LSUBMODEL,
     .           PM(1,I)  ,IPM(1,I) ,MATPARAM )
c-------
          CASE ('LAW71')
            ILAW  = 71             
            CALL HM_READ_MAT71(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NFUNC    ,
     .           MAXFUNC  ,MTAG     ,PARMAT   ,UNITAB   ,       
     .           PM(1,I)  ,LSUBMODEL,MAT_ID   ,TITR     ,IMATVIS)                 
c-------
          CASE ('LAW72','HILL_MMC')
            ILAW = 72
            CALL HM_READ_MAT72(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  ,MATPARAM  )
c-------
          CASE ('LAW73')
            ILAW = 73
            CALL HM_READ_MAT73(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,ISRATE   ,IMATVIS  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC    ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG     ,LSUBMODEL,
     .           ITABLE   ,MAXTABL  ,NUMTABL   ,PM(1,I)  ,IPM(1,I) )
c-------
          CASE ('LAW74')
            ILAW = 74
            CALL HM_READ_MAT74(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG    ,LSUBMODEL,
     .           ITABLE   ,MAXTABL  ,NUMTABL   ,PM(1,I)  ,IPM(1,I)  )
c-------
          CASE ('LAW75','POROUS')
            ILAW = 75
            CALL HM_READ_MAT75(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I))
c-------
          CASE ('LAW76','SAMP')
            ILAW  = 76             
            CALL HM_READ_MAT76(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,PARMAT   ,UNITAB   ,MAT_ID   ,
     .           MTAG     ,TITR     ,LSUBMODEL,PM(1,I)  ,ISRATE   ,
     .           MATPARAM ,MAXTABL  ,NUMTABL  ,ITABLE   )
c-------
          CASE ('LAW77')
            ILAW  = 77             
            IALELAG = 1
            CALL HM_READ_MAT77(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NFUNC    ,
     .           MAXFUNC  ,IFUNC    ,PARMAT   ,MAT_ID   ,PM(1,I)  ,
     .           ISRATE   ,IMATVIS  ,TITR     ,UNITAB   ,LSUBMODEL,
     .           MATPARAM ,JALE     )
c-------
          CASE ('LAW78','YUMODEL')
            ILAW = 78
            CALL HM_READ_MAT78(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,ISRATE   ,IMATVIS  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC    ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG     ,LSUBMODEL,
     .           PM(1,I)  ,NVARTMP  )
c-------
          CASE ('LAW79','JOHN_HOLM')
            ILAW = 79
            CALL HM_READ_MAT79(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NFUNC    ,
     .           MAXFUNC  ,IFUNC    ,PARMAT   ,MAT_ID   ,PM(1,I)  ,
     .           ISRATE   ,MTAG     ,TITR     ,UNITAB   ,LSUBMODEL)
c-------
          CASE ('LAW80')
            ILAW = 80
            CALL HM_READ_MAT80(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NFUNC    ,
     .           MAXFUNC  ,IFUNC    ,PARMAT   ,MAT_ID   ,PM(1,I)  ,
     .           ISRATE   ,MTAG     ,TITR     ,UNITAB   ,LSUBMODEL,
     .           ITABLE   ,MAXTABL  ,NUMTABL  ,NVARTMP  ,TABLE)
c-------
          CASE ('LAW81')
            ILAW = 81
            CALL HM_READ_MAT81(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,PARMAT   ,MAT_ID   ,PM(1,I)  ,
     .           TITR     ,UNITAB   ,LSUBMODEL,MTAG     )
c-------
          CASE ('LAW82')
            ILAW = 82 
            CALL HM_READ_MAT82(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,NUVAR    ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,PARMAT   ,IMATVIS  ,UNITAB    ,
     .           MAT_ID   ,TITR     ,LSUBMODEL,PM(1,I)  ,MATPARAM  )
c-------
          CASE ('LAW83')
            ILAW = 83
            CALL HM_READ_MAT83(
     .           UPARAM   ,MAXUPARAM  ,NUPARAM  ,MTAG    ,PM(1,I)  ,
     .           NUVAR    ,IFUNC      ,MAXFUNC  ,NFUNC   ,PARMAT   , 
     .           UNITAB   ,MAT_ID     ,TITR     ,ISRATE  ,LSUBMODEL)
c-------
          CASE ('LAW84')
            ILAW = 84
            CALL HM_READ_MAT84(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG    ,LSUBMODEL,
     .           ITABLE   ,MAXTABL  ,NUMTABL   ,PM(1,I)  ,IPM(1,I)  )
c-------
          CASE ('LAW87','BARLAT2000')
            ILAW  = 87  
            CALL HM_READ_MAT87(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,ISRATE   ,  NUVAR ,
     .           NFUNC    ,MAXFUNC  ,IFUNC    ,MTAG     ,PARMAT  ,
     .           UNITAB   ,PM(1,I)  ,LSUBMODEL,MAT_ID   ,TITR     )    
c-------
          CASE ('LAW88','MLAW88')
            ILAW = 88  
            CALL HM_READ_MAT88(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  ,IPM(1,I)  ,MATPARAM )
c-------
          CASE ('LAW90')
            ILAW = 90  
            CALL HM_READ_MAT90(
     .           UPARAM  ,MAXUPARAM  ,NUPARAM ,NUVAR    ,IFUNC ,
     .           MAXFUNC ,NFUNC      ,PARMAT  ,UNITAB   ,MAT_ID ,
     .           TITR    ,ISRATE     ,PM(1,I) ,IMATVIS  ,LSUBMODEL,
     .           MTAG    ,MATPARAM   )
c-------
          CASE ('LAW92')
            ILAW = 92
            CALL HM_READ_MAT92(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  ,IPM(1,I)  ,MATPARAM )
c-------
          CASE ('LAW93','ORTH_HILL','CONVERSE')
            ILAW = 93
            CALL HM_READ_MAT93(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  ,IPM(1,I)  ,MATPARAM ,NVARTMP )
c-------
          CASE ('LAW94','YEOH')
            ILAW = 94
            CALL HM_READ_MAT94(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  ,IPM(1,I)  ,MATPARAM )
c-------
          CASE ('LAW95','BERGSTROM_BOYCE')
            ILAW  = 95  
            CALL HM_READ_MAT95(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NFUNC    ,
     .           MAXFUNC  ,MTAG     ,PARMAT   ,UNITAB   ,IMATVIS  ,
     .           PM(1,I)  ,LSUBMODEL,MAT_ID   ,TITR     ,MATPARAM )               
c-------
          CASE ('LAW97','JWLB')
            ILAW = 97  
            CALL HM_READ_MAT97(
     .           UPARAM   ,MAXUPARAM ,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC     ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID    ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  ,IPM(1,I)  )
c-------
          CASE ('LAW100','MNF')
            ILAW  = 100  
            CALL HM_READ_MAT100(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,IFUNC     ,
     .           NFUNC    ,MAXFUNC  ,MTAG     ,PARMAT   ,UNITAB    ,
     .           IMATVIS  ,PM(1,I)  ,LSUBMODEL,MAT_ID   ,TITR      ,
     .           MATPARAM )       
c-------
          CASE ('LAW101','PP')
            ILAW  = 101  
            CALL HM_READ_MAT101(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,IFUNC     ,
     .           MAXFUNC  ,NFUNC    ,PARMAT   ,UNITAB   ,IMATVIS   ,
     .           PM(1,I)  ,LSUBMODEL,MAT_ID   ,TITR     ,MATPARAM  )
c-------
          CASE ('LAW102','DPRAG2')
            ILAW  = 102  
            CALL HM_READ_MAT102(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC    ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID   ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  ,MATPARAM )
c-------     
          CASE ('LAW103','HENSEL-SPI', 'FLAW103')
            ILAW  = 103  
            CALL HM_READ_MAT103(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC    ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID   ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  ,IPM(1,I))
c-------     
          CASE ('LAW104','JOHNS_VOCE_DRUCKER')
            ILAW  = 104  
            CALL HM_READ_MAT104(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NFUNC    ,
     .           MAXFUNC  ,IFUNC    ,PARMAT   ,UNITAB   ,MAT_ID   ,
     .           PM(1,I)  ,TITR     ,MTAG     ,LSUBMODEL,MATPARAM )
c-------     
          CASE ('LAW106','JCOOK_ALM')
            ILAW  = 106  
            CALL HM_READ_MAT106(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC    ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID   ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  ,IPM(1,I))
c-------
          CASE ('LAW107','PAPER_LIGHT','PFEIFFER')
            ILAW = 107
            CALL HM_READ_MAT107(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NUMTABL  ,
     .           MTAG     ,PARMAT   ,UNITAB   ,PM(1,I)  ,LSUBMODEL,
     .           ISRATE   ,MAT_ID   ,TITR     ,ITABLE   ,MAXTABL  ,
     .           NVARTMP  ,MATPARAM )
c-------     
          CASE ('LAW108','SPR_GENE')
            ILAW  = 108
            CALL HM_READ_MAT108(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NFUNC    ,PARMAT   ,
     .           UNITAB   ,PM(1,I)  ,LSUBMODEL,ISRATE   ,MAT_ID   ,
     .           TITR     ,IFUNC    ,MAXFUNC  ,MTAG     )
c-------     
          CASE ('LAW109')
            ILAW  = 109 
            CALL HM_READ_MAT109(
     .           UPARAM   ,MAXUPARAM,NUPARAM   ,NUVAR    ,NVARTMP  ,
     .           ITABLE   ,MAXTABL  ,NUMTABL   ,PARMAT   ,UNITAB   ,
     .           MAT_ID   ,TITR     ,RHO       ,MTAG     ,MATPARAM ,
     .           LSUBMODEL ) 
                 PM(1 ,I) = RHO               
                 PM(89,I) = RHO               
c-------     
          CASE ('LAW110','VEGTER')
            ILAW = 110
            CALL HM_READ_MAT110(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NUMTABL  ,
     .           MTAG     ,PARMAT   ,UNITAB   ,PM(1,I)  ,LSUBMODEL,
     .           ISRATE   ,MAT_ID   ,TITR     ,ITABLE   ,MAXTABL  ,
     .           NVARTMP  ,MATPARAM )
c-------     
          CASE ('LAW111', 'MARLOW')
            ILAW = 111
            CALL HM_READ_MAT111(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,ISRATE  ,IMATVIS  ,
     .           NUVAR    ,IFUNC    ,MAXFUNC  ,NFUNC   ,PARMAT   ,
     .           UNITAB   ,MAT_ID   ,TITR     ,MTAG    ,LSUBMODEL,
     .           PM(1,I)  ,IPM(1,I)  ,MATPARAM )
c-------     
          CASE ('LAW112','PAPER','XIA')
            ILAW = 112
            CALL HM_READ_MAT112(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NUMTABL  ,
     .           MTAG     ,PARMAT   ,UNITAB   ,PM(1,I)  ,LSUBMODEL,
     .           ISRATE   ,MAT_ID   ,TITR     ,ITABLE   ,MAXTABL  ,
     .           NVARTMP  ,MATPARAM )
c-------     
          CASE ('LAW113','SPR_BEAM')
            ILAW  = 113
            CALL HM_READ_MAT113(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NFUNC    ,PARMAT   ,
     .           UNITAB   ,PM(1,I)  ,LSUBMODEL,ISRATE   ,MAT_ID   ,
     .           TITR     ,IFUNC    ,MAXFUNC  ,MTAG     )
c-------
          CASE ('LAW114','SPR_SEATBELT')
            ILAW  = 114
            CALL HM_READ_MAT114(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NFUNC    ,PARMAT   ,
     .           UNITAB   ,PM(1,I)  ,LSUBMODEL,ISRATE   ,MAT_ID   ,
     .           TITR     ,IFUNC    ,MAXFUNC  ,MTAG     )
c-------     
          CASE ('LAW115','DESHFLECK')
            ILAW = 115
            CALL HM_READ_MAT115(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,NUMTABL  ,
     .           MTAG     ,PARMAT   ,UNITAB   ,PM(1,I)  ,LSUBMODEL,
     .           ISRATE   ,MAT_ID   ,TITR     ,ITABLE   ,MAXTABL  ,
     .           NVARTMP  ,MATPARAM )
c-------     
          CASE ('LAW116')
            ILAW  = 116
            CALL HM_READ_MAT116(
     .           MTAG     ,UPARAM   ,MAXUPARAM,NUPARAM  ,PM(1,I)  ,
     .           PARMAT   ,NUVAR    ,IFUNC    ,NFUNC    ,MAXFUNC  ,
     .           UNITAB   ,MAT_ID   ,TITR     ,LSUBMODEL)
c-------     
          CASE ('LAW117')
            ILAW  = 117
            CALL HM_READ_MAT117(
     .           MTAG     ,UPARAM   ,MAXUPARAM,NUPARAM  ,PM(1,I)  ,
     .           PARMAT   ,NUVAR    ,MAXFUNC  ,NFUNC    ,IFUNC    ,
     .           UNITAB   ,MAT_ID   ,TITR     ,LSUBMODEL)
c-------
          CASE ('LAW119','SH_SEATBELT')
            ILAW = 119 
            CALL HM_READ_MAT119(
     .           MTAG     ,UPARAM   ,MAXUPARAM,NUPARAM  ,PM(1,I)  ,
     .           MATPARAM ,PARMAT   ,NUVAR    ,MAT_ID   ,TITR     ,
     .           MAXTABL  ,NUMTABL  ,ITABLE   ,UNITAB   ,LSUBMODEL,
     .           ISRATE   )
         
c-------     
          CASE ('LAW120','TAPO')
            ILAW  = 120
            CALL HM_READ_MAT120(
     .           MTAG     ,UPARAM   ,MAXUPARAM,MAXTABL  ,NUPARAM  ,
     .           NUVAR    ,NVARTMP  ,NUMTABL  ,ITABLE   ,PARMAT   ,
     .           MATPARAM ,PM(1,I)  ,MAT_ID   ,TITR     ,ISRATE   ,
     .           UNITAB   ,LSUBMODEL)
c-------     
          CASE ('LAW121','PLAS_RATE')
            ILAW  = 121
            CALL HM_READ_MAT121(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,MAXFUNC  ,
     .           NFUNC    ,IFUNC    ,MTAG     ,PARMAT   ,UNITAB   ,
     .           PM(1,I)  ,LSUBMODEL,ISRATE   ,MAT_ID   ,TITR     ,
     .           MATPARAM )
c-------     
          CASE ('LAW124','CDPM2')
           ILAW  = 124
           CALL HM_READ_MAT124(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR    ,MTAG     ,
     .           PARMAT   ,UNITAB   ,PM(1,I)  ,LSUBMODEL,ISRATE   ,
     .           ASRATE   ,MAT_ID   ,TITR     ,MATPARAM )
c-------
          CASE ('LAW151','MULTIFLUID')
            ILAW  = 151
            MULTI_FVM%IS_USED = .TRUE. 
            CALL HM_READ_MAT151(MTAG   , PM(1, I), IPM(1, I), MAT_ID, TITR, 
     .                          MULTI_FVM,UNITAB,LSUBMODEL)
c-------
          CASE ('GAS')
             ILAW = 999
             CALL HM_READ_MATGAS(PM(1, I), IPM(1, I),MAT_ID, TITR, KEY2, UNITAB, LSUBMODEL)
c-------
          CASE ('LAW29','USER1')
            ILAW  = 29
            ISRATE = 1
            PM(9,I) = EP20
            MTAG%G_TEMP = 1
            MTAG%L_TEMP = 1
            MTAG%G_EPSD = 1
            MTAG%L_EPSD = 1
            MTAG%G_PLA  = 1
            MTAG%L_PLA  = 1
            CALL HM_READ_MAT29_31(ILAW,KEY,
     .                            USERL_AVAIL,
     .                            UPARAM,MAXUPARAM,NUPARAM,
     .                            NUVAR,IFUNC,MAXFUNC,NFUNC,
     .                            PARMAT,
     .                            LSUBMODEL,
     .                            PM(1,I) )
c-------
          CASE ('LAW30','USER2')
            ILAW  = 30
            ISRATE = 1
            PM(9,I) = EP20
            MTAG%G_TEMP = 1
            MTAG%L_TEMP = 1
            MTAG%G_EPSD = 1
            MTAG%L_EPSD = 1
            MTAG%G_PLA  = 1
            MTAG%L_PLA  = 1
            CALL HM_READ_MAT29_31(ILAW,KEY,
     .                            USERL_AVAIL,
     .                            UPARAM,MAXUPARAM,NUPARAM,
     .                            NUVAR,IFUNC,MAXFUNC,NFUNC,
     .                            PARMAT,
     .                            LSUBMODEL,
     .                            PM(1,I) )
c-------
          CASE ('LAW31','USER3')
            ILAW  = 31
            ISRATE = 1
            PM(9,I) = EP20
            MTAG%G_TEMP = 1
            MTAG%L_TEMP = 1
            MTAG%G_EPSD = 1
            MTAG%L_EPSD = 1
            MTAG%G_PLA  = 1
            MTAG%L_PLA  = 1
            CALL HM_READ_MAT29_31(ILAW,KEY,
     .                            USERL_AVAIL,
     .                            UPARAM,MAXUPARAM,NUPARAM,
     .                            NUVAR,IFUNC,MAXFUNC,NFUNC,
     .                            PARMAT,
     .                            LSUBMODEL,
     .                            PM(1,I) )           

c-------
          CASE ('USER01','USER02','USER03','USER04','USER05','USER06',
     .          'USER07','USER08','USER09','USER10','USER11','USER12',
     .          'USER13','USER14','USER15','USER16','USER17','USER18',
     .          'USER19','USER20','USER21','USER22','USER23','USER24',
     .          'USER25','USER26','USER27','USER28','USER29','USER30',
     .          'USER31','USER32','USER33','USER34','USER35','USER36',
     .          'USER37','USER38','USER39','USER40','USER41','USER42',
     .          'USER43','USER44','USER45','USER46','USER47','USER48',
     .          'USER49','USER50','USER51','USER52','USER53','USER54',
     .          'USER55','USER56','USER57','USER58','USER59','USER60',
     .          'USER61','USER62','USER63','USER64','USER65','USER66',
     .          'USER67','USER68','USER69','USER70','USER71','USER72',
     .          'USER73','USER74','USER75','USER76','USER77','USER78',
     .          'USER79','USER80','USER81','USER82','USER83','USER84',
     .          'USER85','USER86','USER87','USER88','USER89','USER90',
     .          'USER91','USER92','USER93','USER94','USER95','USER96',
     .          'USER97','USER98','USER99')
!
            ILAW  = 99
!!            ISRATE = 1
            PM(9,I) = EP20
            READ(KEY(5:6), '(I2)') IUSER_LAW
!
            CALL HM_READ_MAT_99(ILAW,IUSER_LAW,KEY,
     .                          USERL_AVAIL,
     .                          UPARAM,MAXUPARAM,NUPARAM,
     .                          NUVAR,IFUNC,MAXFUNC,NFUNC,
     .                          PARMAT,USERBUF,
     .                          LSUBMODEL,
     .                          PM(1,I) )
            MTAG%G_TEMP = 1
            MTAG%L_TEMP = 1
            MTAG%G_EPSD = 1
            MTAG%L_EPSD = 1
            MTAG%G_PLA  = 1
            MTAG%L_PLA  = 1
c-------
#ifdef DNC
          CASE ('LAW200', 'MDS')
            READ(KEY(5:6), '(I2)') IUSER_LAW
            ILAW  = 200
!!            ISRATE = 1
            MTAG%G_TEMP = 1
            MTAG%L_TEMP = 1
            MTAG%G_EPSD = 1
            MTAG%L_EPSD = 1
            MTAG%G_PLA  = 1
            MTAG%L_PLA  = 1
!
            CALL HM_READ_MAT_MDS(ILAW,I,MAT_ID,
     .                           UPARAM,MAXUPARAM,NUPARAM,
     .                           NUVAR,IFUNC,MAXFUNC,NFUNC,
     .                           LSUBMODEL,
     .                           PM(1,I) ,PARMAT)
#endif        
        END SELECT
c-----------------------------------------------------------------------
        IF (ILAW == 99) THEN
          WRITE(IOUT,2000) TITR,MAT_ID,IUSER_LAW
        ENDIF
c--------------------------------------------      
        ISRATE = MAX(ISRATE, NINT(PARMAT(4)))  ! just in case ...
        ASRATE = PARMAT(5)*TWO*PI             ! ASRATE = 2*PI*FCUT
!!        IF (ASRATE == ZERO) ASRATE = EP20
c
        MATPARAM%ILAW   = ILAW
        MATPARAM%MAT_ID = MAT_ID        
c
        MTAG%NUVAR   = NUVAR
        MTAG%NVARTMP = NVARTMP
c--------------------------------------------      
c       for user type laws (lecmuser)
c---------------------------------------------------------
        IF (ILAW > 27 .and. ILAW /= 32 .and. ILAW /= 49 .and. ILAW /= 151 .AND. 
     .      ILAW /= 999) THEN
          MTAG%L_STRA = 6        ! all user type laws calculate total strain
c
          C1    = PARMAT(1)
          YOUNG = PARMAT(2)
          NU    = PARMAT(3)
          G     = HALF*YOUNG/(ONE + NU)
          IF (ILAW==34) G=PM(22,I)
          IF (ILAW==42) G=PARMAT(1)
          IF (ILAW==62) G=PARMAT(2)/(ONE + NU)
          IF (ILAW==69) G=PARMAT(1)
          IF (ILAW==82) G=PARMAT(2)/(ONE + NU)
          PM(20,I) = YOUNG
          PM(21,I) = NU
          PM(22,I) = G
          PM(24,I) = YOUNG/(ONE - NU**2)
          PM(32,I) = C1     
          IF (ILAW==71 ) PM(27,I)=SQRT(YOUNG/MAX(PM(1,I),EM20))  ! Sound speed
C---------
C         For solid elements time step computation :
          IPM(252,I)= NINT(PARMAT(16))   ! IFORMDT = 0,1,2  
          PM(105,I) = PARMAT(17)         ! GFAC factor
C---------
          IPM(7,I)   = IADBUF
          IPM(8,I)   = NUVAR
          IPM(9,I)   = NUPARAM
          IPM(10,I)  = NFUNC
          IPM(216,I) = IMATVIS
          IPM(226,I) = NUMTABL
c
c---------------------------------------------------------
          DO J=1,NFUNC
            IPM(10+J,I) = IFUNC(J)
          ENDDO
c
          DO J=1,NUMTABL
            IPM(226+J,I) = ITABLE(J)
          ENDDO
c---------------------------------------------------------
c         Fill UPARAM buffer
c---------------------------------------------------------
          DO J=1,NUPARAM
            BUFMAT(IADBUF+J-1) = UPARAM(J)
          ENDDO
          IADBUF = IADBUF + NUPARAM
          BUFLEN = BUFLEN + NUPARAM
c
        ELSE IF (ILAW == 19) THEN
          MTAG%L_STRA = 6
          IPM(7,I) = IADBUF   
          IPM(8,I) = NUVAR    
          IPM(9,I) = NUPARAM  
          DO J=1,NUPARAM
            BUFMAT(IADBUF+J-1) = UPARAM(J)
          ENDDO
          IADBUF = IADBUF + NUPARAM
          BUFLEN = BUFLEN + NUPARAM
c           
        ENDIF ! ILAW>=28 
c
        IPM(1,I)   = MAT_ID
        IPM(2,I)   = ILAW
        IPM(3,I)   = ISRATE
c
        PM(8,I)    = ISRATE          ! double stockage - a nettoyer
        IF (PM(9,I) == ZERO) PM(9,I) = ASRATE    ! old mat laws fill it directly
        PM(19,I)   = ILAW + EM01     ! double stockage - a nettoyer
        PM(70,I)   = JTUR + EM01
        PM(71,I)   = JTHE + EM01
        PM(72,I)   = JALE + EM01    
        IPM(217,I) = IUSER_LAW
c---------------------------------------------------------
c
        RHOR = PM(1 ,I)
        RHO0 = PM(89,I)
        IF (RHOR == ZERO .AND. ILAW /= 37) THEN
          RHOR = RHO0
          PM(1,I) = RHOR
        ENDIF
        IF(ILAW/=0 .AND. ILAW/=20 .AND. ILAW/=51 .AND. ILAW/=151 .AND. ILAW/=108 .AND. 
     .     ILAW /= 999)THEN
          IF(RHO0<=ZERO)THEN
            CALL ANCMSG(MSGID=683,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=MAT_ID,
     .                  C1=TITR,
     .                  C2='DENSITY')
          ENDIF
        ENDIF
c
        IF (ISRATE >= 0) THEN
          IF (MTAG%G_EPSD == 0) MTAG%G_EPSD = 1
          IF (MTAG%L_EPSD == 0) MTAG%L_EPSD = 1
        ENDIF
C for QEPH (shell formulation)       
        IF(IPM(2,I) /= 999)THEN ! IF ipm(2,) == 999 possible negative square root with PM(25)=CPE(gas)
          PM(12,I) = SQRT(MAX(ZERO, PM(22,I)))    ! GSR
          PM(13,I) = SQRT(MAX(ZERO, PM(24,I)))    ! A11SR
          PM(14,I) = SQRT(MAX(ZERO, PM(25,I)))    ! A12SR
          PM(190,I)= SQRT(MAX(ZERO, PM(21,I)))    ! NUSR
        ENDIF  
!PM(100)=Bulk
!pour interface type 7  Ki=PM(32)*...
!pour interface type 20 Ki=PM(100)*...        
c
        IF (PM(100,I) == ZERO) PM(100,I) = PM(32,I) ! bulk used for interf 20 stiffness
C        
c---------------------------------------------------------
C-----initialization MATPARAM%COMPRESSIBILITY by keywords (default :ELASTO_PLASTIC)
c---------------------------------------------------------
            SELECT CASE (ILAW)
C--------no-langrangian laws can be added later(not used now, so not important)         
             CASE(6,11,17,18,19,20,26,49,151)                                             
                CALL INIT_MAT_KEYWORD(MATPARAM,"INCOMPRESSIBLE")
C------ elasto-plastic, the call is not needed (default value), just for info             
             CASE(10,21,22,23,24,27,32)  
               CALL INIT_MAT_KEYWORD(MATPARAM,"ELASTO_PLASTIC")
            END SELECT 
C                       
C---------------  fin boucle sur les lois
      ENDDO   ! HM_NUMMAT
C---------------   
C-------------------------------------
C     Recherche des ID doubles
C-------------------------------------
      I=79
      J=0
      K=0
      RBID = ZERO
      CALL VDOUBLE(IPM(1,1),NPROPMI,NUMMAT-1,MESS,0,RBID)
C------------------------------
C     Precalcul SQRT
C------------------------------
      DO I = 1, NUMMAT-1
        IF (IPM(2,I)==999) CYCLE !possible negative square root with PM(25)=CPE(gas)
        PM(12,I) = SQRT(MAX(ZERO, PM(22,I)))    ! GSR
        PM(13,I) = SQRT(MAX(ZERO, PM(24,I)))    ! A11SR
        PM(14,I) = SQRT(MAX(ZERO, PM(25,I)))    ! A12SR
        PM(190,I)= SQRT(MAX(ZERO, PM(21,I)))    ! NUSR
      ENDDO

      DO I = 1, NUMMAT-1
        ILAW = IPM(2,I)
        IF (ILAW /= 42) THEN
          PM(100,I) = PM(32,I)
        ENDIF
      END DO

      DEALLOCATE( UPARAM )
c------------------------------
      RETURN
 999  CALL ANCMSG(MSGID=55,
     .            ANMODE=ANINFO,
     .            MSGTYPE=MSGERROR,
     .            C1=KEY0(KCUR),
     .            C2=KLINE,
     .            C3=LINE)
         CALL ARRET(2)
      RETURN
c------------------------------
 2000 FORMAT(//
     & 5X,A,/,
     & 5X,'MATERIAL NUMBER . . . . . . . . . . . .=',I10/,
     & 5X,'USER MATERIAL LAW . . . . . . . . . . .=',I10/)
c------------------------------
      END
