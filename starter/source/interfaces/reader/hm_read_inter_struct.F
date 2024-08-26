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
      !||    hm_read_inter_struct         ../starter/source/interfaces/reader/hm_read_inter_struct.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_interfaces           ../starter/source/interfaces/reader/hm_read_interfaces.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                       ../starter/source/output/message/message.F
      !||    fretitl                      ../starter/source/starter/freform.F
      !||    hm_get_intv                  ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_read_inter_hertz_type17   ../starter/source/interfaces/int17/hm_read_inter_hertz_type17.F
      !||    hm_read_inter_type02         ../starter/source/interfaces/int02/hm_read_inter_type02.F
      !||    hm_read_inter_type03         ../starter/source/interfaces/int03/hm_read_inter_type03.F
      !||    hm_read_inter_type05         ../starter/source/interfaces/int05/hm_read_inter_type05.F
      !||    hm_read_inter_type06         ../starter/source/interfaces/int06/hm_read_inter_type06.F
      !||    hm_read_inter_type07         ../starter/source/interfaces/int07/hm_read_inter_type07.F
      !||    hm_read_inter_type08         ../starter/source/interfaces/int08/hm_read_inter_type08.F
      !||    hm_read_inter_type10         ../starter/source/interfaces/int10/hm_read_inter_type10.F
      !||    hm_read_inter_type11         ../starter/source/interfaces/int11/hm_read_inter_type11.F
      !||    hm_read_inter_type14         ../starter/source/interfaces/int14/hm_read_inter_type14.F
      !||    hm_read_inter_type15         ../starter/source/interfaces/int15/hm_read_inter_type15.F
      !||    hm_read_inter_type20         ../starter/source/interfaces/int20/hm_read_inter_type20.F
      !||    hm_read_inter_type21         ../starter/source/interfaces/int21/hm_read_inter_type21.F
      !||    hm_read_inter_type23         ../starter/source/interfaces/int23/hm_read_inter_type23.F
      !||    hm_read_inter_type24         ../starter/source/interfaces/int24/hm_read_inter_type24.F
      !||    hm_read_inter_type25         ../starter/source/interfaces/int25/hm_read_inter_type25.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod                  ../starter/share/message_module/message_mod.F
      !||    sensor_mod                   ../starter/share/modules1/sensor_mod.F
      !||    submodel_mod                 ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE HM_READ_INTER_STRUCT(
     1        IPARI       ,STFAC          ,FRIGAP     ,XFILTR   ,FRIC_P    ,
     2        IGRNOD      ,IGRSURF        ,IGRSLIN    ,NPC      ,ILAGM     ,
     3        UNITAB      ,I2RUPT         ,
     4        AREASL      ,NI             ,NOM_OPT    ,TITR     ,DEF_INTER ,
     5        NPC1        ,SENSORS        ,
     6        NOM_OPTFRIC ,INTBUF_FRIC_TAB,IGRBRIC    ,NOINT    ,KEY       ,
     7        LSUBMODEL   ,TF             ,INTERFACES ,SNPC     ,SNPC1     ,
     8        NPARI       ,NPARIR         ,NPTS       ,
     9        LNOPT1      ,LTITR          ,NINTER25   ,
     .        NGRNOD      ,NGRBRIC        ,NSURF      ,NSLIN    ,
     .        ITHERM_FE   ,INTHEAT   )
C-----------------------------------------------
C   D e s r i p t i o n
C-----------------------------------------------
C CONTACT INTERFACES : READER SUBROUTINES
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE INTBUF_FRIC_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD , ONLY : SUBMODEL_DATA, NSUBMOD
      USE SENSOR_MOD
      USE INTERFACES_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NPTS !array size TF
      INTEGER,INTENT(IN) :: NGRNOD,NGRBRIC,NSURF,NSLIN  !< array size (elem data structure)
      INTEGER,INTENT(IN) :: LNOPT1,LTITR !< array size NOM_OPT
      INTEGER,INTENT(IN) :: NPARI, NPARIR  !< array size IPARI and FRIGAP
      INTEGER,INTENT(IN) :: SNPC, SNPC1 !< array size (NPC and NPC1)
      INTEGER,INTENT(IN) :: ITHERM_FE
      INTEGER,INTENT(INOUT) :: INTHEAT
      INTEGER,INTENT(INOUT) :: NINTER25
      TYPE (SUBMODEL_DATA), DIMENSION(NSUBMOD),INTENT(IN) :: LSUBMODEL
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER NOM_OPT(LNOPT1,*), NOM_OPTFRIC(LNOPT1,*)
      INTEGER ILAGM,NI,NOINT
      INTEGER IPARI(NPARI),NPC(SNPC),DEF_INTER(100),NPC1(SNPC1)
      my_real STFAC,XFILTR,AREASL
      my_real FRIGAP(NPARIR),FRIC_P(10),I2RUPT(6),TF(NPTS)
      CHARACTER(LEN=NCHARTITLE) :: TITR
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
      TYPE (SURF_)   ,TARGET , DIMENSION(NSLIN)   :: IGRSLIN
      TYPE (INTERFACES_) ,INTENT(INOUT):: INTERFACES
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NTYP,UID,IFLAGUNIT,FLAG_FMT,FLAG_FMT_TMP,IERR1,SUB_ID,IFIX_TMP,ID_TYPE19
      CHARACTER(LEN=NCHARKEY) :: KEY
      LOGICAL IS_AVAILABLE
!

C=======================================================================
C     INTERFACES FOR SOLID AND STRUCTURE READING ROUTINES
C=======================================================================

      NTYP = 0
C
      ILAGM = 0
C
      IS_AVAILABLE = .FALSE.
C
c flag from TYPE19 convert
      ID_TYPE19 = 0
      CALL HM_GET_INTV('ID_TYPE19',ID_TYPE19,IS_AVAILABLE,LSUBMODEL)
C
      IF (ID_TYPE19 > 0) THEN
C--   No printout for interfaces generated from type19 - noint set to id of the type19
        NOINT = ID_TYPE19
      ELSE
        WRITE(IOUT,1000) NOINT,TRIM(TITR)
      ENDIF
C
      NOM_OPT(1,NI)=NOINT
C
C---------------------------------
      CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,NI),LTITR)

C---------------------------------------------------------------
C  INTERFACES TYPE : READING ROUTINES
C---------------------------------------------------------------

      SELECT CASE(KEY(1:LEN_TRIM(KEY)))

          CASE ('TYPE25')
              NTYP=25
              NINTER25 = NINTER25 +1
              CALL HM_READ_INTER_TYPE25(
     1         IPARI                ,STFAC           ,FRIGAP     ,NOINT      ,NI         ,
     2         IGRNOD               ,IGRSURF         ,XFILTR     ,FRIC_P     ,DEF_INTER  ,
     3         SENSORS              ,NOM_OPT         ,UNITAB     ,LSUBMODEL  ,TITR       ,
     4         NOM_OPTFRIC          ,INTBUF_FRIC_TAB ,NPC        ,NPC1       ,TF         ,
     5         INTERFACES%PARAMETERS,NPARI           ,NPARIR     ,SNPC       ,SNPC1      ,
     6         NPTS                 ,ITHERM_FE       ,INTHEAT   )

          CASE ('TYPE24')
              NTYP=24
              CALL HM_READ_INTER_TYPE24(
     1        IPARI      ,STFAC          ,FRIGAP                ,NOINT      ,NI        ,
     2        IGRNOD     ,IGRSURF        ,XFILTR                ,FRIC_P     ,DEF_INTER ,
     3        SENSORS    ,NOM_OPT        ,UNITAB                ,LSUBMODEL  ,TITR      ,
     4        NOM_OPTFRIC,INTBUF_FRIC_TAB,INTERFACES%PARAMETERS ,NPARI      ,NPARIR    ,
     5        ITHERM_FE  ,INTHEAT)

          CASE ('TYPE23')
              NTYP=23
              CALL HM_READ_INTER_TYPE23(
     1             IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,
     2             IGRSURF    ,XFILTR     ,FRIC_P     ,NPC1      ,TITR    ,
     3             LSUBMODEL  ,UNITAB     ,NPARI      ,NPARIR    ,SNPC1   )

          CASE ('TYPE21')
              NTYP=21
              CALL HM_READ_INTER_TYPE21(
     1          IPARI      ,STFAC      ,FRIGAP     ,NOINT    ,NI        ,
     2          IGRSURF    ,XFILTR     ,FRIC_P     ,NPC1     ,SENSORS   ,
     3          NOM_OPT    ,UNITAB     ,LSUBMODEL  ,TITR     ,NPC       ,
     4          TF         ,NPARI      ,NPARIR     ,SNPC     ,SNPC1     ,
     5          LNOPT1     ,ITHERM_FE  ,INTHEAT)

          CASE ('TYPE20')
              NTYP=20
              CALL HM_READ_INTER_TYPE20(
     1          IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,
     2          IGRNOD     ,IGRSURF    ,IGRSLIN    ,XFILTR    ,FRIC_P   ,  
     3          UNITAB     ,LSUBMODEL  ,TITR       )    

          CASE ('HERTZ')
              NTYP  = 17
              ILAGM = -1
              CALL HM_READ_INTER_HERTZ_TYPE17(
     1          IPARI      ,FRIGAP     ,NOINT      ,NTYP    ,
     2          IGRBRIC    ,UNITAB     ,LSUBMODEL  ,TITR     )

          CASE ('TYPE16')
             NTYP = 16
             ILAGM = 1 

          CASE ('TYPE15')
             NTYP = 15
             CALL HM_READ_INTER_TYPE15(
     1            IPARI     ,STFAC       ,FRIGAP     ,NOINT     ,
     2            IGRSURF   ,TITR        ,LSUBMODEL  ,UNITAB    )

          CASE ('TYPE14')
             NTYP = 14
             CALL HM_READ_INTER_TYPE14(
     1            IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,
     2            IGRNOD     ,IGRSURF    ,NPC        ,TITR      ,LSUBMODEL,
     3            UNITAB     )

          CASE ('TYPE11')
             NTYP = 11
             CALL HM_READ_INTER_TYPE11(
     1        IPARI      ,STFAC      ,FRIGAP     ,NOINT      ,NI       ,
     2        IGRSLIN    ,SENSORS    ,DEF_INTER  ,TITR       ,
     3        NOM_OPT    ,UNITAB     ,LSUBMODEL  ,NOM_OPTFRIC,INTBUF_FRIC_TAB,
     4        ID_TYPE19  ,NPARI      ,NPARIR     ,LNOPT1  ,ITHERM_FE  ,INTHEAT)

          CASE ('TYPE10')
             NTYP = 10
             CALL HM_READ_INTER_TYPE10(
     .            IPARI    ,STFAC    ,FRIGAP   ,IGRNOD   ,IGRSURF  ,
     .            LSUBMODEL,UNITAB   ,TITR     ,NOINT    )

          CASE ('TYPE8')
             NTYP = 8
             CALL HM_READ_INTER_TYPE08(
     1            IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,
     2            IGRNOD     ,IGRSURF    ,UNITAB    ,LSUBMODEL  ,TITR     )

          CASE ('TYPE7')
             NTYP = 7
             CALL HM_READ_INTER_TYPE07(
     1           IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI         ,    
     2           IGRNOD     ,IGRSURF    ,XFILTR     ,FRIC_P    ,NOM_OPT    ,
     3           DEF_INTER  ,NPC1       ,SENSORS    ,UNITAB    ,LSUBMODEL  ,
     4           TITR       ,NOM_OPTFRIC,INTBUF_FRIC_TAB,NPC   ,TF         ,
     5           ID_TYPE19  ,NPARI      ,NPARIR     ,SNPC      ,SNPC1      ,
     6           LNOPT1     ,NPTS       ,ITHERM_FE  ,INTHEAT   )

          CASE ('TYPE6')
             NTYP = 6
             CALL HM_READ_INTER_TYPE06(
     1            IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,
     2            IGRSURF    ,NPC1       ,TITR       ,LSUBMODEL ,UNITAB   )

          CASE ('TYPE5')
             NTYP = 5
           CALL HM_READ_INTER_TYPE05(
     1          IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,    
     2          IGRNOD     ,IGRSURF    ,NOM_OPT    ,XFILTR    ,FRIC_P   ,
     3          SENSORS    ,UNITAB     ,LSUBMODEL  ,TITR      ,NPARI    ,
     4          NPARIR)

          CASE ('TYPE4')
          CALL ANCMSG(MSGID=1615,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=NOINT,C1=TITR)

          CASE ('TYPE3')
             NTYP = 3
             CALL HM_READ_INTER_TYPE03(
     1          IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,
     2          IGRSURF    ,UNITAB     ,LSUBMODEL  ,NPARI     ,
     3          NPARIR)

          CASE ('TYPE2')
             NTYP = 2
             CALL HM_READ_INTER_TYPE02(
     1          IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,IGRNOD   ,
     2          IGRSURF    ,I2RUPT     ,AREASL     ,DEF_INTER ,NPC1     ,
     3          TITR       ,UNITAB     ,LSUBMODEL  ,NPARI     ,NPARIR   ,
     4          SNPC1      ,INTHEAT    )

      END SELECT ! SELECT CASE(NTYP)
C
      IPARI(33) = MAX(0,ILAGM)
C

C=======================================================================
 1000 FORMAT(/1X,'  INTERFACE NUMBER :',I10,1X,A)
C------------
      RETURN


      END
