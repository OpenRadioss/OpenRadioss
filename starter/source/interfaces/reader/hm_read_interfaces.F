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
      !||====================================================================
      !||    hm_read_interfaces           ../starter/source/interfaces/reader/hm_read_interfaces.F
      !||--- called by ------------------------------------------------------
      !||    lectur                       ../starter/source/starter/lectur.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                       ../starter/source/output/message/message.F
      !||    hm_option_read_key           ../starter/source/devtools/hm_reader/hm_option_read_key.F
      !||    hm_option_start              ../starter/source/devtools/hm_reader/hm_option_start.F
      !||    hm_read_inter_fsi            ../starter/source/interfaces/reader/hm_read_inter_fsi.F
      !||    hm_read_inter_lagdt_type07   ../starter/source/interfaces/int07/hm_read_inter_lagdt_type07.F
      !||    hm_read_inter_lagmul         ../starter/source/interfaces/reader/hm_read_inter_lagmul.F
      !||    hm_read_inter_struct         ../starter/source/interfaces/reader/hm_read_inter_struct.F
      !||    hm_sz_r2r                    ../starter/source/coupling/rad2rad/routines_r2r.F
      !||    ifrontplus                   ../starter/source/spmd/node/frontplus.F
      !||    usr2sys                      ../starter/source/system/sysfus.F
      !||--- uses       -----------------------------------------------------
      !||    hm_option_read_mod           ../starter/share/modules1/hm_option_read_mod.F
      !||    message_mod                  ../starter/share/message_module/message_mod.F
      !||    r2r_mod                      ../starter/share/modules1/r2r_mod.F
      !||    sensor_mod                   ../starter/share/modules1/sensor_mod.F
      !||    submodel_mod                 ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE HM_READ_INTERFACES(
     1           IPARI           ,FRIGAP   ,ITAB       ,ITABM1     ,
     2           IGRNOD          ,IGRSURF  ,IGRSLIN    ,IGRBRIC    ,IGRSH3N    ,
     3           IGRTRUSS        ,NPC      ,ISKN       ,XFILTR     ,STFAC      ,
     4           FRIC_P          ,I2RUPT   ,AREASL     ,UNITAB     ,NOM_OPT    ,
     5           DEF_INTER       ,NPC1     ,SENSORS    ,MULTI_FVM  ,NOM_OPTFRIC,
     6           INTBUF_FRIC_TAB ,LSUBMODEL,TF         ,NPTS       ,NPARI      ,
     7           KLOADPINTER     ,DGAPINT  ,INTERFACES ,SITAB      ,NPARIR     ,
     8           SITABM1         ,SISKWN   ,LISKN      ,SNPC       ,SNPC1      )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C General Subroutine which manages interface Readers (/INTER/TYPE..)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD , ONLY : UNIT_TYPE_
      USE R2R_MOD
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD , ONLY : MULTI_FVM_STRUCT
      USE INTBUF_FRIC_MOD , ONLY : INTBUF_FRIC_STRUCT_
      USE GROUPDEF_MOD , ONLY : GROUP_, SURF_
      USE SUBMODEL_MOD , ONLY : NSUBMOD, SUBMODEL_DATA
      USE HM_OPTION_READ_MOD
      USE SENSOR_MOD , ONLY : SENSORS_
      USE INTERFACES_MOD , ONLY : INTERFACES_
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY
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
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "r2r_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NPARI, NPARIR !< array sizes (IPARI and FRIGAP)
      INTEGER,INTENT(IN) :: NPTS !< array size TF
      INTEGER,INTENT(IN) :: SNPC, SNPC1 !< array size (NPC and NPC1)
      INTEGER,INTENT(IN) :: SITAB, SITABM1 !< array size (ITAB, ITABM1)
      INTEGER,INTENT(IN) :: LISKN, SISKWN !< array size (ISKN)
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      INTEGER NOM_OPT(LNOPT1,*)
      INTEGER IPARI(NPARI,NINTER), ITAB(NUMNOD), ITABM1(SITABM1),NPC(SNPC),ISKN(SISKWN),
     .        DEF_INTER(100),NPC1(SNPC1),NOM_OPTFRIC(LNOPT1,*),KLOADPINTER(NINTER+1)
      my_real XFILTR(NINTER),STFAC(NINTER),FRIC_P(10,NINTER),I2RUPT(6,NINTER),FRIGAP(NPARIR,NINTER),AREASL(NINTER),TF(NPTS)
      my_real,INTENT(IN) :: DGAPINT(NINTER)
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(NINTERFRIC)
      TYPE (SUBMODEL_DATA), DIMENSION(NSUBMOD),INTENT(IN) :: LSUBMODEL
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
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
      INTEGER I,J,P,NI,NIN,ILAGM,NOINT,NB,UID,SUB_ID,IFLAGUNIT,K
      CHARACTER(LEN=NCHARKEY) :: KEY, KEY2, STR_TMP
      CHARACTER(LEN=NCHARTITLE) :: TITR
      CHARACTER(LEN=40) :: MESS
      DATA MESS/'INTERFACE INPUT                         '/
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER,EXTERNAL :: USR2SYS
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C READING INTERFACES

      NI = 0
      NB = 0
      INTHEAT = 0
      FTEMPVAR21 = 0
      NITSCHE = 0
      INTERADHESION = 0
      I22LEN = 0
      INT22 = 0

      ! START BROWSING MODEL INTERFACES
      CALL HM_OPTION_START('/INTER')
      NI = 0
      ! BROWSING MODEL INTERFACES 1->HM_NINTER
      DO NIN=1,HM_NINTER
         NB = NB+1
         !Multidomains --> ignoring untaged interface and sub-intergaces
         IF (NSUBDOM > 0) THEN
           IF(TAGINT(NB)==0) CALL HM_SZ_R2R(TAGINT,NB,LSUBMODEL)
         ENDIF
         ILAGM = 0
         KEY = ''
         ! EXTRACT DATAS OF /INTER/... LINE
         CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID=NOINT,UNIT_ID=UID,SUBMODEL_ID=SUB_ID,
     .                           OPTION_TITR=TITR,KEYWORD2=KEY,KEYWORD3=KEY2)
         ! CHECK IF READ OPTION IS /INTER/SUB
         IF(KEY(1:LEN_TRIM(KEY)) == 'SUB') CYCLE
         NI=NI+1
         DO I = 1,NPARI
            IPARI(I,NI) = 0
         ENDDO
         IPARI(23,NI) = 1
         ! CHECK IF UID IS EXISTING
         IFLAGUNIT = 0
         DO J=1,UNITAB%NUNITS
           IF (UNITAB%UNIT_ID(J) == UID) THEN
             IFLAGUNIT = 1
             EXIT
           ENDIF
         ENDDO
         IF (UID /= 0.AND.IFLAGUNIT == 0) THEN
           CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,I2=UID,I1=NOINT,C1='INTERFACE',C2='INTERFACE',C3=TITR)
         ENDIF
         ! WRITE TITLE IN OUT FILE
         WRITE(IOUT,1300)
         STR_TMP = KEY(1:LEN_TRIM(KEY))
         ! CHECK INTERFACE AVAILABILITY WITH 2D ANALYSIS
         IF(N2D /= 0 .AND.
     .       STR_TMP /= 'TYPE1' .AND. STR_TMP /= 'TYPE2' .AND.
     .       STR_TMP /= 'TYPE3' .AND. STR_TMP /= 'TYPE5') THEN
              CALL ANCMSG(MSGID=2097,ANMODE=ANSTOP,MSGTYPE=MSGERROR,I1=NOINT,C1=TITR,C2=STR_TMP)
         ENDIF
C--------------------------------------------------
        SELECT CASE(STR_TMP)
          CASE ('KINE', 'TYPE1', 'TYPE9', 'TYPE12', 'TYPE18', 'TYPE22')
           ! SPECIFIC CASE OF FSI INTERFACES READING ROUTINES
           CALL HM_READ_INTER_FSI(
     1          IPARI(1,NI) ,STFAC(NI) ,FRIGAP(1,NI) ,XFILTR(NI) ,FRIC_P(1,NI),
     2          IGRNOD      ,IGRSURF   ,ILAGM        ,UNITAB     ,NI          ,
     3          NOM_OPT     ,TITR      ,IGRBRIC      ,IGRSH3N    ,IGRTRUSS    ,
     4          NOINT       ,KEY       ,LSUBMODEL    ,ITAB        ,
     5          ITABM1      ,ISKN      ,MULTI_FVM    ,NPARI      ,NPARIR      ,
     6          NGRNOD      ,NGRBRIC   ,NGRSH3N      ,NGRTRUS    ,NSURF       ,
     7          LNOPT1      ,SITABM1   ,SISKWN       ,LISKN      ,
     8          LTITR       ,SITAB)

        CASE ('LAGMUL')
         ! /INTER/LAGMUL
           CALL HM_READ_INTER_LAGMUL(
     1          IPARI(1,NI) ,STFAC(NI) ,FRIGAP(1,NI) ,IGRNOD ,IGRSURF   ,
     2          ILAGM       ,NI        ,NOM_OPT      ,TITR   ,DEF_INTER ,
     3          IGRBRIC     ,UNITAB    ,LSUBMODEL    ,NOINT  ,KEY2      ,
     4          NPARI       ,NPARIR)

        CASE ('LAGDT')
         ! /INTER/LAGDT
           CALL HM_READ_INTER_LAGDT_TYPE07(
     1          IPARI(1,NI) ,STFAC(NI)  ,FRIGAP(1,NI)  ,XFILTR(NI),FRIC_P(1,NI),
     2          IGRNOD      ,IGRSURF    ,ILAGM         ,UNITAB   ,NI           ,
     4          NOM_OPT     ,TITR       ,DEF_INTER     ,NOINT    ,LSUBMODEL    ,
     5          NPARI       ,NPARIR)

        CASE DEFAULT
         ! ALL OTHER INTERFACES : STRUCTURE + TIED TYPE2
           CALL HM_READ_INTER_STRUCT(
     1          IPARI(1,NI)  ,STFAC(NI)      ,FRIGAP(1,NI)   ,XFILTR(NI)   ,FRIC_P(1,NI),
     2          IGRNOD       ,IGRSURF        ,IGRSLIN        ,NPC          ,ILAGM       ,
     3          UNITAB       ,I2RUPT(1,NI)   ,
     4          AREASL(NI)   ,NI             ,NOM_OPT        ,TITR         ,DEF_INTER   ,
     5          NPC1         ,SENSORS        ,
     6          NOM_OPTFRIC  ,INTBUF_FRIC_TAB,IGRBRIC        ,NOINT        ,KEY         ,
     7          LSUBMODEL    ,TF             ,INTERFACES     ,SNPC         ,SNPC1       ,
     8          NPARI        ,NPARIR         ,NPTS           ,
     9          LNOPT1       ,LTITR          ,NINTER25       ,
     .          NGRNOD       ,NGRBRIC        ,NSURF          ,NSLIN  )

        END SELECT

        NOINT  = IPARI(15,NI)
        IF (IPARI(71,NI) <= 0) THEN
          DO K=1,NI-1
            IF (NOINT == IPARI(15,K)) THEN
                CALL ANCMSG(MSGID=117,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1,I1=NOINT,C1=TITR,I2=NOINT)
            ENDIF
          ENDDO
        ENDIF

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

      ENDDO ! NI

C=======================================================================
 1300 FORMAT(    /1X,'   INTERFACES               ' /
     .            1X,' --------------             '// )
      RETURN
      END

