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
Chd|  HM_READ_INTERFACES            source/interfaces/reader/hm_read_interfaces.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_READ_INTER_FSI             source/interfaces/reader/hm_read_inter_fsi.F
Chd|        HM_READ_INTER_LAGDT_TYPE07    source/interfaces/int07/hm_read_inter_lagdt_type07.F
Chd|        HM_READ_INTER_LAGMUL          source/interfaces/reader/hm_read_inter_lagmul.F
Chd|        HM_READ_INTER_STRUCT          source/interfaces/reader/hm_read_inter_struct.F
Chd|        HM_SZ_R2R                     source/coupling/rad2rad/routines_r2r.F
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        INTERFACES_MOD                ../common_source/modules/interfaces/interfaces_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INTERFACES(
     1           IPARI    ,FRIGAP   ,LINTER    ,ITAB       ,ITABM1     ,
     2           IGRNOD   ,IGRSURF  ,IGRSLIN   ,IGRBRIC    ,IGRSH3N    ,
     3           IGRTRUSS ,NPC      ,ISKN      ,XFILTR     ,STFAC      , 
     4           FRIC_P   ,I2RUPT   ,AREASL    ,UNITAB     ,NOM_OPT    ,
     5           DEF_INTER,NPC1     ,SENSORS   ,MULTI_FVM  ,NOM_OPTFRIC,
     6           INTBUF_FRIC_TAB    ,IDDLEVEL  ,LSUBMODEL  ,TF         ,
     7           KLOADPINTER        ,DGAPINT   ,INTERFACES)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE R2R_MOD
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE INTBUF_FRIC_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE SENSOR_MOD
      USE INTERFACES_MOD
C-----------------------------------------------      
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "r2r_c.inc"
#include      "inter22.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)::IDDLEVEL
      INTEGER NOM_OPT(LNOPT1,*)
      INTEGER LINTER
      INTEGER IPARI(NPARI,*), ITAB(NUMNOD), ITABM1(*),NPC(*),ISKN(*),
     .        DEF_INTER(*),NPC1(*),NOM_OPTFRIC(LNOPT1,*),KLOADPINTER(NINTER+1)
      my_real
     .   XFILTR(*),STFAC(*),FRIC_P(10,*),I2RUPT(6,*),
     .   FRIGAP(NPARIR,*),AREASL(*),TF(*)
      my_real , INTENT(IN) :: DGAPINT(NINTER)
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
      TYPE (SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (SURF_)   , DIMENSION(NSURF) ,TARGET   :: IGRSURF
      TYPE (SURF_)   , DIMENSION(NSLIN) ,TARGET   :: IGRSLIN
      TYPE (INTERFACES_) ,INTENT(INOUT):: INTERFACES
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,P,NI,NIN,ILAGM,NOINT,NB,UID,SUB_ID,IDINT7,IDINT7B,IDINT11,
     .   IFLAGUNIT,K
      CHARACTER KEY*ncharkey, KEY2*ncharkey, STR_TMP*ncharkey
      CHARACTER*nchartitle,
     .   TITR

C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C
      CHARACTER MESS*40
      DATA MESS/'INTERFACE INPUT                         '/
C--------------------------------------------
C=======================================================================
C READING INTERFACES
C=======================================================================


      NI        = 0
      NB        = 0 
      IDINT7    = 0
      IDINT7B   = 0
      IDINT11   = 0
      INTHEAT   = 0
      FTEMPVAR21= 0
      NITSCHE   = 0
      INTERADHESION = 0
      I22LEN = 0
      INT22 = 0

C--------------------------------------------------
C START BROWSING MODEL INTERFACES
C--------------------------------------------------
      CALL HM_OPTION_START('/INTER')
      NI = 0
C--------------------------------------------------
C BROWSING MODEL INTERFACES 1->HM_NINTER
C--------------------------------------------------
      DO NIN=1,HM_NINTER

         NB = NB+1
C----------Multidomaines --> on ignore les interfaces et sous-interfaces non tagees--- 
         IF (NSUBDOM > 0) THEN
           IF(TAGINT(NB)==0) CALL HM_SZ_R2R(TAGINT,NB,LSUBMODEL)
         ENDIF
c
         ILAGM = 0
         KEY = ''   
C
C--------------------------------------------------
C EXTRACT DATAS OF /INTER/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = NOINT,
     .                       UNIT_ID = UID,
     .                       SUBMODEL_ID = SUB_ID,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY,
     .                       KEYWORD3 = KEY2)
C--------------------------------------------------
C CHECK IF READ OPTION IS /INTER/SUB
C--------------------------------------------------
        IF(KEY(1:LEN_TRIM(KEY)) == 'SUB') CYCLE 
C--------------------------------------------------
         NI=NI+1
C                                      
         DO I = 1,NPARI
            IPARI(I,NI) = 0
         ENDDO
         IPARI(23,NI) = 1                               
C--------------------------------------------------
C CHECK IF UID IS EXISTING
C--------------------------------------------------
      IFLAGUNIT = 0
      DO J=1,NUNITS
          IF (UNITAB%UNIT_ID(J) == UID) THEN    
            IFLAGUNIT = 1
            EXIT
          ENDIF
      ENDDO
      IF (UID /= 0.AND.IFLAGUNIT == 0) THEN
           CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .  	       I2=UID,I1=NOINT,C1='INTERFACE',
     .  	       C2='INTERFACE',
     .                 C3=TITR)
      ENDIF
C--------------------------------------------------
C WRITE TITLE IN OUT FILE
C--------------------------------------------------
        WRITE(IOUT,1300) 

        STR_TMP = KEY(1:LEN_TRIM(KEY))
C--------------------------------------------------
C CHECK INTERFACE AVAILABILITY WITH 2D ANALYSIS
C--------------------------------------------------
        IF(N2D /= 0 .AND.
     .     STR_TMP /= 'TYPE1' .AND. STR_TMP /= 'TYPE2' .AND.
     .     STR_TMP /= 'TYPE3' .AND. STR_TMP /= 'TYPE5') THEN
              CALL ANCMSG(MSGID=2097,ANMODE=ANSTOP,MSGTYPE=MSGERROR,
     .  	       I1=NOINT,C1=TITR,C2=STR_TMP)
        ENDIF
C--------------------------------------------------
        SELECT CASE(STR_TMP)
        CASE ('KINE', 'TYPE1', 'TYPE9', 'TYPE12','TYPE18',  'TYPE22')
C--------------------------------------------------
C     FSI INTERFACES READING ROUTINES
C--------------------------------------------------
           CALL HM_READ_INTER_FSI(
     1          IPARI(1,NI) ,STFAC(NI) ,FRIGAP(1,NI) ,XFILTR(NI),FRIC_P(1,NI),
     2          IGRNOD      ,IGRSURF   ,ILAGM        ,UNITAB    ,NI          ,
     3          NOM_OPT     ,TITR      ,IGRBRIC      ,IGRSH3N   ,IGRTRUSS    ,       
     4          IDDLEVEL    ,NOINT     ,KEY          ,LSUBMODEL ,ITAB        ,
     5          ITABM1      ,ISKN      ,MULTI_FVM)

        CASE ('LAGMUL')
C---------------------------------------------------------------
C  /INTER/LAGMUL
C---------------------------------------------------------------
           CALL HM_READ_INTER_LAGMUL(
     1          IPARI(1,NI)   ,STFAC(NI)  ,FRIGAP(1,NI)  ,IGRNOD   ,IGRSURF    ,
     2          ILAGM         ,NI         ,NOM_OPT       ,TITR     ,DEF_INTER  ,
     3          IGRBRIC       ,UNITAB     ,LSUBMODEL     ,NOINT    ,KEY2       )

        CASE ('LAGDT')
C---------------------------------------------------------------
C  /INTER/LAGDT
C---------------------------------------------------------------
           CALL HM_READ_INTER_LAGDT_TYPE07(
     1          IPARI(1,NI) ,STFAC(NI)  ,FRIGAP(1,NI)  ,XFILTR(NI),FRIC_P(1,NI),
     2          IGRNOD      ,IGRSURF    ,ILAGM         ,UNITAB   ,NI           ,
     4          NOM_OPT     ,TITR       ,DEF_INTER     ,NOINT    ,LSUBMODEL    )

        CASE DEFAULT
C---------------------------------------------------------------
C     STRUCTURE INTERFACES + TYPE2  
C---------------------------------------------------------------
           CALL HM_READ_INTER_STRUCT(
     1          IPARI(1,NI)  ,STFAC(NI)  ,FRIGAP(1,NI)   ,XFILTR(NI),FRIC_P(1,NI),
     2          IGRNOD       ,IGRSURF    ,IGRSLIN        ,NPC       ,ILAGM       ,
     3          ITABM1       ,ITAB       ,ISKN           ,UNITAB    ,I2RUPT(1,NI),
     4          AREASL(NI)   ,NI         ,NOM_OPT        ,TITR      ,DEF_INTER   ,
     5          IDINT7       ,IDINT7B    ,IDINT11        ,NPC1     , SENSORS     ,
     6          NOM_OPTFRIC,INTBUF_FRIC_TAB,IGRBRIC      ,NOINT    , KEY         ,
     7          LSUBMODEL    ,TF         ,INTERFACES  )

        END SELECT

         NOINT  = IPARI(15,NI)
         IF (IPARI(71,NI) <= 0) THEN
           DO K=1,NI-1
             IF (NOINT == IPARI(15,K)) THEN
                 CALL ANCMSG(MSGID=117,
     .                       MSGTYPE=MSGERROR,
     .                       ANMODE=ANINFO_BLIND_1,
     .                       I1=NOINT,C1=TITR,I2=NOINT)
             ENDIF
           ENDDO
         ENDIF
         
               
C-------
         IF(IPARI(39,NI) == 1)THEN
           IPARI(40,NI)=USR2SYS(IPARI(40,NI),ITABM1,MESS,IPARI(15,NI))
           IF(IMACH == 3)THEN
             DO P = 1, NSPMD
	       CALL IFRONTPLUS(IPARI(40,NI),P) 
             END DO
           END IF
         ELSEIF(IPARI(39,NI) == 2)THEN
           IPARI(40,NI)=USR2SYS(IPARI(40,NI),ITABM1,MESS,IPARI(15,NI))
           IPARI(41,NI)=USR2SYS(IPARI(41,NI),ITABM1,MESS,IPARI(15,NI))
           IF(IMACH == 3)THEN
              DO P = 1, NSPMD
                CALL IFRONTPLUS(IPARI(40,NI),P)
                CALL IFRONTPLUS(IPARI(41,NI),P)	      
              END DO
            END IF
         ENDIF

        IF(NINTLOADP > 0)IPARI(95,NI) = KLOADPINTER(NI+1)  - KLOADPINTER(NI)
        IF(NINTLOADP > 0)FRIGAP(46,NI) = DGAPINT(NI) 

C--------
      ENDDO ! NI

C=======================================================================
C--------------------------------------------------------------
 1300 FORMAT(    /1X,'   INTERFACES               ' /
     .            1X,' --------------             '// )
C-----
      RETURN
C-----
      END

