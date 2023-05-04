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
Chd|  HM_READ_SENSORS               source/tools/sensor/hm_read_sensors.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        READ_SENSOR_ACC               source/tools/sensor/read_sensor_acc.F
Chd|        READ_SENSOR_AND               source/tools/sensor/read_sensor_and.F
Chd|        READ_SENSOR_CONTACT           source/tools/sensor/read_sensor_contact.F
Chd|        READ_SENSOR_DISP              source/tools/sensor/read_sensor_disp.F
Chd|        READ_SENSOR_DIST_SURF         source/tools/sensor/read_sensor_dist_surf.F
Chd|        READ_SENSOR_ENERGY            source/tools/sensor/read_sensor_energy.F
Chd|        READ_SENSOR_GAUGE             source/tools/sensor/read_sensor_gauge.F
Chd|        READ_SENSOR_HIC               source/tools/sensor/read_sensor_hic.F
Chd|        READ_SENSOR_NIC               source/tools/sensor/read_sensor_nic.F
Chd|        READ_SENSOR_NOT               source/tools/sensor/read_sensor_not.F
Chd|        READ_SENSOR_OR                source/tools/sensor/read_sensor_or.F
Chd|        READ_SENSOR_RBODY             source/tools/sensor/read_sensor_rbody.F
Chd|        READ_SENSOR_RWALL             source/tools/sensor/read_sensor_rwall.F
Chd|        READ_SENSOR_SECT              source/tools/sensor/read_sensor_sect.F
Chd|        READ_SENSOR_SENS              source/tools/sensor/read_sensor_sens.F
Chd|        READ_SENSOR_TEMP              source/tools/sensor/read_sensor_temp.F
Chd|        READ_SENSOR_TIME              source/tools/sensor/read_sensor_time.F
Chd|        READ_SENSOR_USER              source/tools/sensor/read_sensor_user.F
Chd|        READ_SENSOR_VEL               source/tools/sensor/read_sensor_vel.F
Chd|        READ_SENSOR_WORK              source/tools/sensor/read_sensor_work.F
Chd|        SENSOR_TAB_INIT               source/tools/sensor/sensor_tab_init.F
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        SURF_MOD                      share/modules1/surf_mod.F     
Chd|====================================================================
      SUBROUTINE HM_READ_SENSORS(
     .           SENSORS   ,LACCELM   ,ITABM1    ,IPART     ,LGAUGE    ,
     .           SUBSET    ,NSETS     ,IGRSURF   ,IGRNOD    ,BUFSF     ,
     .           SKEW      ,ISKWN     ,UNITAB    ,LSUBMODEL ,HM_NSENS  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE INOUTFILE_MOD
      USE SURF_MOD
      USE SUBMODEL_MOD
      USE SENSOR_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
#include      "submod_c.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN)  :: NSETS
      INTEGER ,INTENT(OUT) :: HM_NSENS
      INTEGER ,DIMENSION(*)   ,INTENT(IN) :: ITABM1
      INTEGER ,DIMENSION(3,*) ,INTENT(IN) :: LACCELM,LGAUGE
      INTEGER ,DIMENSION(LISKN,*)   ,INTENT(IN) :: ISKWN
      INTEGER ,DIMENSION(LIPART1,*) ,INTENT(IN) :: IPART
      my_real ,DIMENSION(LSKEW,*) :: SKEW
      my_real ,DIMENSION(*)       :: BUFSF
      TYPE (SENSORS_) ,INTENT(INOUT) ,TARGET   :: SENSORS
      TYPE (SURF_)       ,DIMENSION(NSURF+NSETS)     :: IGRSURF
      TYPE (GROUP_)      ,DIMENSION(NGRNOD)    :: IGRNOD
      TYPE (SUBSET_)     ,DIMENSION(NSUBS)     :: SUBSET
      TYPE (SUBMODEL_DATA) ,DIMENSION(NSUBMOD) :: LSUBMODEL
      TYPE (UNIT_TYPE_)  ,INTENT(IN) ::UNITAB 
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISEN,IUNIT,IFLAGUNIT,NS,TYP,UNIT_ID,SENS_ID
      my_real :: BID
      CHARACTER TITR*nchartitle,KEY*40,MESS*40
      INTEGER ,DIMENSION(:) ,ALLOCATABLE   :: SID
      TYPE (SENSOR_STR_)    ,POINTER :: SENSOR_PTR
      DATA MESS/'SENSOR DEFINITION                       '/
C=======================================================================
c     Initialize sensor data structure
c---------------------------------------------
      CALL HM_OPTION_COUNT('/SENSOR', HM_NSENS)

      IF (HM_NSENS > 0) THEN
        WRITE(ISTDO,'(A)')' .. SENSORS'
        WRITE (IOUT,'(///,A)')'          SENSORS'
        WRITE (IOUT,'(A/)')   '          -------'
      END IF
c---------------------------------------------
      IF (ALLOCATED(SENSORS%SENSOR_TAB)) DEALLOCATE(SENSORS%SENSOR_TAB)
      MY_ALLOCATE (SENSORS%SENSOR_TAB,HM_NSENS)
      MY_ALLOCATE (SID,HM_NSENS)
      SENSORS%NSENSOR = HM_NSENS
c
      CALL SENSOR_TAB_INIT(SENSORS)
c
      CALL HM_OPTION_START('/SENSOR')
c
c---------------------------------------------------
c
      DO ISEN = 1,HM_NSENS
        SENSOR_PTR => SENSORS%SENSOR_TAB(ISEN)
c            
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = SENS_ID,
     .                          OPTION_TITR = TITR  ,
     .                          UNIT_ID     = UNIT_ID   ,
     .                          KEYWORD2    = KEY   )
c--------------------------------------------------
c       CHECK IF UNIT_ID EXISTS
c--------------------------------------------------
        IFLAGUNIT = 0
        DO IUNIT=1,NUNITS
          IF (UNITAB%UNIT_ID(IUNIT) == UNIT_ID) THEN
            IFLAGUNIT = 1
            EXIT
          ENDIF
        ENDDO
        IF (UNIT_ID > 0 .AND. IFLAGUNIT == 0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I2=UNIT_ID,I1=SENS_ID,
     .                C1='SENSOR', C2='SENSOR', C3='TITR')                              
        ENDIF
c
        KEY = KEY(1:LEN_TRIM(KEY))
c-----------------------
        SELECT CASE(KEY)
c-----------------------
          CASE ('TIME' ,'TYPE0')
            CALL READ_SENSOR_TIME(SENSOR_PTR ,SENS_ID  ,UNITAB   ,LSUBMODEL)  
c
          CASE ('ACCE' ,'TYPE1')
            CALL READ_SENSOR_ACC(SENSOR_PTR ,SENS_ID  ,TITR     , 
     .           LACCELM  ,UNITAB   ,LSUBMODEL)  
c
          CASE ('DIST' ,'TYPE2')
            CALL READ_SENSOR_DISP(SENSOR_PTR ,SENS_ID  ,
     .           ITABM1   ,UNITAB   ,LSUBMODEL)   
c
          CASE ('SENS' ,'TYPE3')
            CALL READ_SENSOR_SENS(SENSOR_PTR ,SENS_ID  ,UNITAB   ,LSUBMODEL)
c
          CASE ('AND' ,'TYPE4')
            CALL READ_SENSOR_AND(SENSOR_PTR ,SENS_ID  ,UNITAB   ,LSUBMODEL)
c
          CASE ('OR' ,'TYPE5')
            CALL READ_SENSOR_OR(SENSOR_PTR ,SENS_ID  ,UNITAB   ,LSUBMODEL)
c
          CASE ('INTER' ,'TYPE6')
            CALL READ_SENSOR_CONTACT(SENSOR_PTR ,SENS_ID  ,TITR     ,
     .           UNITAB   ,LSUBMODEL)
c
          CASE ('RWALL' ,'TYPE7')
            CALL READ_SENSOR_RWALL(SENSOR_PTR ,SENS_ID  ,TITR     ,
     .           UNITAB   ,LSUBMODEL)
c
          CASE ('NOT' ,'TYPE8')
            CALL READ_SENSOR_NOT(SENSOR_PTR ,SENS_ID  ,UNITAB   ,LSUBMODEL)
c
          CASE ('VEL' ,'TYPE9')
            CALL READ_SENSOR_VEL(SENSOR_PTR ,SENS_ID  ,
     .           ITABM1   ,UNITAB   ,LSUBMODEL)   
c
          CASE ('GAUGE' ,'TYPE10')            
            CALL READ_SENSOR_GAUGE(SENSOR_PTR ,SENS_ID  ,TITR     ,
     .           LGAUGE   ,UNITAB   ,LSUBMODEL)
c
          CASE ('RBODY' ,'TYPE11')
            CALL READ_SENSOR_RBODY(SENSOR_PTR ,SENS_ID  ,TITR     ,
     .           UNITAB   ,LSUBMODEL)
c
          CASE ('SECT' ,'TYPE12')
            CALL READ_SENSOR_SECT(SENSOR_PTR ,SENS_ID  ,TITR     ,
     .           UNITAB   ,LSUBMODEL)
c
          CASE ('WORK' ,'TYPE13')
            CALL READ_SENSOR_WORK(SENSOR_PTR ,SENS_ID  ,
     .           ITABM1   ,UNITAB   ,LSUBMODEL)   
c
          CASE ('ENERGY' ,'TYPE14')
            CALL READ_SENSOR_ENERGY(SENSOR_PTR ,SENS_ID  ,TITR     ,
     .           IPART   ,SUBSET   ,UNITAB  ,LSUBMODEL)
c
          CASE ('DIST_SURF' ,'TYPE15')
            CALL READ_SENSOR_DIST_SURF(SENSOR_PTR ,SENS_ID  ,
     .           ITABM1   ,UNITAB   ,LSUBMODEL)   
c
          CASE ('HIC' ,'TYPE16')
            CALL READ_SENSOR_HIC(SENSOR_PTR ,SENS_ID  ,TITR     , 
     .           LACCELM  ,UNITAB   ,LSUBMODEL)  
c
          CASE ('TEMP')
            CALL READ_SENSOR_TEMP(SENSOR_PTR  ,SENS_ID     ,TITR     , 
     .           IGRNOD   ,UNITAB   ,LSUBMODEL)   
c
          CASE ('NIC_NIJ')
            CALL READ_SENSOR_NIC(
     .           SENSOR_PTR  ,SENS_ID    ,TITR     ,ISKWN    , 
     .           UNITAB      ,LSUBMODEL  )   
c
          CASE ('USER1' ,'USER2','USER3')

            SENSOR_PTR%TSTART = INFINITY

            CALL READ_SENSOR_USER(HM_NSENS ,ISEN    ,TITR,
     .           UNITAB    ,LSUBMODEL      ,KEY     ,UNIT_ID ,SENS_ID ,
     .           SENSOR_PTR)
            SENSOR_PTR%SENS_ID = SENS_ID
c---
          CASE DEFAULT
            CALL ANCMSG(MSGID=43, ANMODE=ANINFO, MSGTYPE=MSGERROR,
     .                  C2=KEY(1:LEN_TRIM(KEY)),
     .                  I1=SENS_ID,
     .                  C1=TITR)        
        END SELECT 
c
        SENSOR_PTR%TITLE = TITR
c-----------------
      END DO  ! NSENSOR
c-------------------------------------
c     Recherche des ID doubles  : activate this code when lecsen is removed
c-------------------------------------
      DO ISEN = 1,SENSORS%NSENSOR
        IF (SENSORS%SENSOR_TAB(ISEN)%SENS_ID > 0)  THEN
          SID(ISEN) = SENSORS%SENSOR_TAB(ISEN)%SENS_ID
        END IF
      END DO
      CALL UDOUBLE(SID,1,SENSORS%NSENSOR,MESS,0,BID)
      DEALLOCATE(SID)
c-----------
      RETURN
      END
