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
Chd|  HM_READ_INTER_STRUCT          source/interfaces/reader/hm_read_inter_struct.F
Chd|-- called by -----------
Chd|        HM_READ_INTERFACES            source/interfaces/reader/hm_read_interfaces.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_READ_INTER_HERTZ_TYPE17    source/interfaces/int17/hm_read_inter_hertz_type17.F
Chd|        HM_READ_INTER_TYPE02          source/interfaces/int02/hm_read_inter_type02.F
Chd|        HM_READ_INTER_TYPE03          source/interfaces/int03/hm_read_inter_type03.F
Chd|        HM_READ_INTER_TYPE05          source/interfaces/int05/hm_read_inter_type05.F
Chd|        HM_READ_INTER_TYPE06          source/interfaces/int06/hm_read_inter_type06.F
Chd|        HM_READ_INTER_TYPE07          source/interfaces/int07/hm_read_inter_type07.F
Chd|        HM_READ_INTER_TYPE08          source/interfaces/int08/hm_read_inter_type08.F
Chd|        HM_READ_INTER_TYPE10          source/interfaces/int10/hm_read_inter_type10.F
Chd|        HM_READ_INTER_TYPE11          source/interfaces/int11/hm_read_inter_type11.F
Chd|        HM_READ_INTER_TYPE14          source/interfaces/int14/hm_read_inter_type14.F
Chd|        HM_READ_INTER_TYPE15          source/interfaces/int15/hm_read_inter_type15.F
Chd|        HM_READ_INTER_TYPE20          source/interfaces/int20/hm_read_inter_type20.F
Chd|        HM_READ_INTER_TYPE21          source/interfaces/int21/hm_read_inter_type21.F
Chd|        HM_READ_INTER_TYPE23          source/interfaces/int23/hm_read_inter_type23.F
Chd|        HM_READ_INTER_TYPE24          source/interfaces/int24/hm_read_inter_type24.F
Chd|        HM_READ_INTER_TYPE25          source/interfaces/int25/hm_read_inter_type25.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        INTERFACES_MOD                ../common_source/modules/interfaces/interfaces_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INTER_STRUCT(
     1        IPARI       ,STFAC      ,FRIGAP         ,XFILTR   ,FRIC_P    ,
     2        IGRNOD      ,IGRSURF    ,IGRSLIN        ,NPC      ,ILAGM     ,   
     3        ITABM1      ,ITAB       ,ISKN           ,UNITAB   ,I2RUPT    ,
     4        AREASL      ,NI         ,NOM_OPT        ,TITR     ,DEF_INTER ,
     5        IDINT7      ,IDINT7B    ,IDINT11        ,NPC1     ,SENSOR_TAB,
     6        NOM_OPTFRIC,INTBUF_FRIC_TAB,IGRBRIC     ,NOINT    ,KEY       ,
     7        LSUBMODEL  ,TF          ,INTERFACES  )
C============================================================================

C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE INTBUF_FRIC_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE SENSOR_MOD
      USE INTERFACES_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER NOM_OPT(LNOPT1,*), NOM_OPTFRIC(LNOPT1,*)
      INTEGER ILAGM,NI,NOINT
      INTEGER IPARI(*),NPC(*),ITABM1(*),ISKN(LISKN,*),ITAB(*),
     .        DEF_INTER(*),IDINT7,IDINT7B,IDINT11,NPC1(*)
      my_real
     .   STFAC,XFILTR,AREASL
      my_real
     .   FRIGAP(*),FRIC_P(*),I2RUPT(*),TF(*)
      CHARACTER TITR*nchartitle
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
      TYPE (SURF_)   ,TARGET , DIMENSION(NSLIN)   :: IGRSLIN
      TYPE (INTERFACES_) ,INTENT(INOUT):: INTERFACES
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .   NTYP,UID,IFLAGUNIT,FLAG_FMT,FLAG_FMT_TMP,IERR1,
     .   SUB_ID,IFIX_TMP,ID_TYPE19
      CHARACTER KEY*ncharkey
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
C--------------------------------------------------
C            HM READING INTER TYPE25 
C--------------------------------------------------
              NTYP=25
              CALL HM_READ_INTER_TYPE25(
     1         IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI         ,
     2         IGRNOD     ,IGRSURF    ,XFILTR     ,FRIC_P    ,DEF_INTER  ,
     3         SENSOR_TAB ,NOM_OPT    ,UNITAB    ,LSUBMODEL  ,TITR       ,
     4         NOM_OPTFRIC,INTBUF_FRIC_TAB,NPC   ,NPC1       ,TF         ,
     5         INTERFACES%PARAMETERS)

          CASE ('TYPE24')
C--------------------------------------------------
C            HM READING INTER TYPE24 
C--------------------------------------------------
              NTYP=24
              CALL HM_READ_INTER_TYPE24(
     1        IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI          ,
     2        IGRNOD     ,IGRSURF    ,XFILTR     ,FRIC_P     ,DEF_INTER  ,
     3        SENSOR_TAB ,NOM_OPT     ,UNITAB     ,LSUBMODEL  ,TITR      ,
     4        NOM_OPTFRIC,INTBUF_FRIC_TAB,INTERFACES%PARAMETERS )

          CASE ('TYPE23')
C--------------------------------------------------
C            HM READING INTER TYPE23 
C--------------------------------------------------
              NTYP=23
              CALL HM_READ_INTER_TYPE23(
     1             IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,
     2             IGRSURF    ,XFILTR     ,FRIC_P     ,NPC1      , TITR    ,
     3             LSUBMODEL  ,UNITAB     )


          CASE ('TYPE21')
C--------------------------------------------------
C            HM READING INTER TYPE21 
C--------------------------------------------------
              NTYP=21
              CALL HM_READ_INTER_TYPE21(
     1          IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,
     2          IGRSURF    ,XFILTR     ,FRIC_P     ,NPC1     ,SENSOR_TAB,
     3          NOM_OPT    ,UNITAB     ,LSUBMODEL  ,TITR     ,NPC       ,
     4          TF         )

          CASE ('TYPE20')
C--------------------------------------------------
C            HM READING INTER TYPE20 
C--------------------------------------------------
              NTYP=20
              CALL HM_READ_INTER_TYPE20(
     1          IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,
     2          IGRNOD     ,IGRSURF    ,IGRSLIN    ,XFILTR    ,FRIC_P   ,  
     3          UNITAB     ,LSUBMODEL  ,TITR       )    

          CASE ('HERTZ')
C--------------------------------------------------
C            HM READING INTER TYPE17 
C--------------------------------------------------
              NTYP  = 17
              ILAGM = -1
              CALL HM_READ_INTER_HERTZ_TYPE17(
     1          IPARI      ,FRIGAP     ,NOINT      ,NI       ,NTYP       ,
     2          IGRBRIC    ,UNITAB     ,LSUBMODEL  ,TITR     )

          CASE ('TYPE16')
C--------------------------------------------------
C            HM READING INTER TYPE16 
C--------------------------------------------------
             NTYP = 16
             ILAGM = 1 

          CASE ('TYPE15')
C--------------------------------------------------
C            HM READING INTER TYPE15 
C--------------------------------------------------
             NTYP = 15
             CALL HM_READ_INTER_TYPE15(
     1            IPARI     ,STFAC       ,FRIGAP     ,NOINT     ,NI       ,
     2            IGRSURF   ,TITR        ,LSUBMODEL  ,UNITAB    )

          CASE ('TYPE14')
C--------------------------------------------------
C            HM READING INTER TYPE14 
C--------------------------------------------------
             NTYP = 14
             CALL HM_READ_INTER_TYPE14(
     1            IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,
     2            IGRNOD     ,IGRSURF    ,NPC        ,TITR      ,LSUBMODEL,
     3            UNITAB     )

          CASE ('TYPE12')
C--------------------------------------------------
C            HM READING INTER TYPE12 
C--------------------------------------------------
             NTYP = 12

          CASE ('TYPE11')
C--------------------------------------------------
C            HM READING INTER TYPE11 
C--------------------------------------------------
             NTYP = 11
             CALL HM_READ_INTER_TYPE11(
     1        IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,
     2        IGRSURF    ,IGRSLIN    ,SENSOR_TAB ,DEF_INTER ,TITR      ,
     3        NOM_OPT    ,UNITAB     ,LSUBMODEL  ,NOM_OPTFRIC,INTBUF_FRIC_TAB,
     4        ID_TYPE19)

          CASE ('TYPE10')
C--------------------------------------------------
C            HM READING INTER TYPE10 
C--------------------------------------------------
             NTYP = 10
             CALL HM_READ_INTER_TYPE10(
     .            IPARI    ,STFAC    ,FRIGAP   ,IGRNOD   ,IGRSURF  ,
     .            LSUBMODEL,UNITAB   ,TITR     ,NOINT    ,KEY      )
c

          CASE ('TYPE8')
C--------------------------------------------------
C            HM READING INTER TYPE8 
C--------------------------------------------------
             NTYP = 8
             CALL HM_READ_INTER_TYPE08(
     1            IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,
     2            IGRNOD     ,IGRSURF    ,UNITAB    ,LSUBMODEL  ,TITR     )
          CASE ('TYPE7')
C--------------------7------------------------------
C            HM READING INTER TYPE7 
C--------------------------------------------------
             NTYP = 7
             CALL HM_READ_INTER_TYPE07(
     1           IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI         ,    
     2           IGRNOD     ,IGRSURF    ,XFILTR     ,FRIC_P    ,NOM_OPT    ,
     3           DEF_INTER  ,NPC1       ,SENSOR_TAB ,UNITAB    ,LSUBMODEL  ,
     4           TITR       ,NOM_OPTFRIC,INTBUF_FRIC_TAB,NPC   ,TF         ,
     5           ID_TYPE19)

          CASE ('TYPE6')
C--------------------------------------------------
C            HM READING INTER TYPE6 
C--------------------------------------------------
             NTYP = 6
             CALL HM_READ_INTER_TYPE06(
     1            IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,    
     2            IGRSURF    ,NPC1       ,TITR       ,LSUBMODEL ,UNITAB   )

          CASE ('TYPE5')
C--------------------------------------------------
C            HM READING INTER TYPE5 
C--------------------------------------------------
             NTYP = 5
           CALL HM_READ_INTER_TYPE05(
     1          IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,    
     2          IGRNOD     ,IGRSURF    ,NOM_OPT    ,XFILTR    ,FRIC_P   ,
     3          SENSOR_TAB ,UNITAB     ,LSUBMODEL  ,TITR       )

          CASE ('TYPE4')
C--------------------------------------------------
C            HM READING INTER TYPE4 
C--------------------------------------------------
          CALL ANCMSG(MSGID=1615,               
     .                MSGTYPE=MSGERROR,        
     .                ANMODE=ANINFO,           
     .                I1=NOINT,                
     .                C1=TITR)      
c        WRITE(IOUT,1504) STFAC,FRIC,GAP,STARTT,STOPT,IBUC 

          CASE ('TYPE3')
C--------------------------------------------------
C            HM READING INTER TYPE3 
C--------------------------------------------------
             NTYP = 3
             CALL HM_READ_INTER_TYPE03(
     1          IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,
     2          IGRSURF    ,UNITAB     ,LSUBMODEL  ,TITR     )
          CASE ('TYPE2')
C--------------------------------------------------
C            HM READING INTER TYPE2 
C--------------------------------------------------
             NTYP = 2
             CALL HM_READ_INTER_TYPE02(
     1          IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,IGRNOD   ,
     2          IGRSURF    ,I2RUPT     ,AREASL     ,DEF_INTER ,NPC1     ,
     3          TITR       ,UNITAB     ,LSUBMODEL)

      END SELECT ! SELECT CASE(NTYP)
C
      IPARI(33) = MAX(0,ILAGM)
C

C=======================================================================
 1000 FORMAT(/1X,'  INTERFACE NUMBER :',I10,1X,A)
C------------
      RETURN


      END
