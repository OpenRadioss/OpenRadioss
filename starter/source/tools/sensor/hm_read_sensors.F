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
!||    hm_read_sensors          ../starter/source/tools/sensor/hm_read_sensors.F
!||--- called by ------------------------------------------------------
!||    lectur                   ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_option_count          ../starter/source/devtools/hm_reader/hm_option_count.F
!||    hm_option_read_key       ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start          ../starter/source/devtools/hm_reader/hm_option_start.F
!||    read_sensor_acc          ../starter/source/tools/sensor/read_sensor_acc.F
!||    read_sensor_and          ../starter/source/tools/sensor/read_sensor_and.F
!||    read_sensor_contact      ../starter/source/tools/sensor/read_sensor_contact.F
!||    read_sensor_disp         ../starter/source/tools/sensor/read_sensor_disp.F
!||    read_sensor_dist_surf    ../starter/source/tools/sensor/read_sensor_dist_surf.F
!||    read_sensor_energy       ../starter/source/tools/sensor/read_sensor_energy.F
!||    read_sensor_gauge        ../starter/source/tools/sensor/read_sensor_gauge.F
!||    read_sensor_hic          ../starter/source/tools/sensor/read_sensor_hic.F
!||    read_sensor_nic          ../starter/source/tools/sensor/read_sensor_nic.F
!||    read_sensor_not          ../starter/source/tools/sensor/read_sensor_not.F
!||    read_sensor_or           ../starter/source/tools/sensor/read_sensor_or.F
!||    read_sensor_python       ../starter/source/tools/sensor/hm_read_sensor_python.F90
!||    read_sensor_rbody        ../starter/source/tools/sensor/read_sensor_rbody.F
!||    read_sensor_rwall        ../starter/source/tools/sensor/read_sensor_rwall.F
!||    read_sensor_sect         ../starter/source/tools/sensor/read_sensor_sect.F
!||    read_sensor_sens         ../starter/source/tools/sensor/read_sensor_sens.F
!||    read_sensor_temp         ../starter/source/tools/sensor/read_sensor_temp.F
!||    read_sensor_time         ../starter/source/tools/sensor/read_sensor_time.F
!||    read_sensor_user         ../starter/source/tools/sensor/read_sensor_user.F
!||    read_sensor_vel          ../starter/source/tools/sensor/read_sensor_vel.F
!||    read_sensor_work         ../starter/source/tools/sensor/read_sensor_work.F
!||    sensor_tab_init          ../starter/source/tools/sensor/sensor_tab_init.F
!||    udouble                  ../starter/source/system/sysfus.F
!||    usr2sys                  ../starter/source/system/sysfus.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    read_sensor_python_mod   ../starter/source/tools/sensor/hm_read_sensor_python.F90
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||    surf_mod                 ../starter/share/modules1/surf_mod.F
!||====================================================================
      SUBROUTINE HM_READ_SENSORS(PYTHON,
     .           SENSORS   ,LACCELM   ,ITABM1    ,IPART     ,LGAUGE    ,
     .           SUBSET    ,NSETS     ,IGRSURF   ,IGRNOD    ,BUFSF     ,
     .           SKEW      ,ISKWN     ,UNITAB    ,LSUBMODEL ,HM_NSENS  ,
     .           SENSOR_USER_STRUCT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PYTHON_FUNCT_MOD
      USE MY_ALLOC_MOD  
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE INOUTFILE_MOD
      USE SURF_MOD
      USE SUBMODEL_MOD
      USE SENSOR_MOD
      USE HM_OPTION_READ_MOD
      USE READ_SENSOR_PYTHON_MOD, ONLY : READ_SENSOR_PYTHON
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(PYTHON_), intent(inout) :: PYTHON
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
      TYPE(SENSOR_USER_STRUCT_) ,INTENT(INOUT) :: SENSOR_USER_STRUCT
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISEN,IUNIT,IFLAGUNIT,NS,TYP,UNIT_ID,SENS_ID
      my_real :: BID
      CHARACTER(LEN=NCHARTITLE) :: TITR
      CHARACTER KEY*40,MESS*40
      INTEGER ,DIMENSION(:) ,ALLOCATABLE   :: SID
      TYPE(PYTHON_FUNCTION), DIMENSION(:), ALLOCATABLE :: temp
      DATA MESS/'SENSOR DEFINITION                       '/
C=======================================================================
c     Initialize sensor data structure
c---------------------------------------------
      CALL HM_OPTION_COUNT('/SENSOR', HM_NSENS)
      CALL HM_OPTION_COUNT('/SENSOR/PYTHON', NS)
      IF(NS > 0) THEN
        call python_initialize(python_error)
        if(.not. allocated(python%functs)) then 
          allocate(python%functs(ns))
        else
          allocate(temp(PYTHON%nb_functs + NS))
          temp(1:PYTHON%nb_functs) = PYTHON%functs
          CALL MOVE_ALLOC(from=temp,to=PYTHON%functs)
        endif
      END IF
      IF (HM_NSENS > 0) THEN
        WRITE(ISTDO,'(A)')' .. SENSORS'
        WRITE (IOUT,'(///,A)')'          SENSORS'
        WRITE (IOUT,'(A/)')   '          -------'
      END IF
c---------------------------------------------
      IF (ALLOCATED(SENSORS%SENSOR_TAB)) DEALLOCATE(SENSORS%SENSOR_TAB)
      ALLOCATE(SENSORS%SENSOR_TAB(HM_NSENS))
      CALL MY_ALLOC (SID,HM_NSENS)
      SENSORS%NSENSOR = HM_NSENS
c
      CALL SENSOR_TAB_INIT(SENSORS)
c
      CALL HM_OPTION_START('/SENSOR')
c
c---------------------------------------------------
c
      DO ISEN = 1,HM_NSENS
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
        DO IUNIT=1,UNITAB%NUNITS
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
            CALL READ_SENSOR_TIME(SENSORS%SENSOR_TAB(ISEN) ,SENS_ID  ,UNITAB   ,LSUBMODEL)  
c
          CASE ('ACCE' ,'TYPE1')
            CALL READ_SENSOR_ACC(SENSORS%SENSOR_TAB(ISEN) ,SENS_ID  ,TITR     , 
     .           LACCELM  ,UNITAB   ,LSUBMODEL)  
c
          CASE ('DIST' ,'TYPE2')
            CALL READ_SENSOR_DISP(SENSORS%SENSOR_TAB(ISEN) ,SENS_ID  ,
     .           ITABM1   ,UNITAB   ,LSUBMODEL)   
c
          CASE ('SENS' ,'TYPE3')
            CALL READ_SENSOR_SENS(SENSORS%SENSOR_TAB(ISEN) ,SENS_ID  ,UNITAB   ,LSUBMODEL)
c
          CASE ('AND' ,'TYPE4')
            CALL READ_SENSOR_AND(SENSORS%SENSOR_TAB(ISEN) ,SENS_ID  ,UNITAB   ,LSUBMODEL)
c
          CASE ('OR' ,'TYPE5')
            CALL READ_SENSOR_OR(SENSORS%SENSOR_TAB(ISEN) ,SENS_ID  ,UNITAB   ,LSUBMODEL)
c
          CASE ('INTER' ,'TYPE6')
            CALL READ_SENSOR_CONTACT(SENSORS%SENSOR_TAB(ISEN) ,SENS_ID  ,TITR     ,
     .           UNITAB   ,LSUBMODEL)
c
          CASE ('RWALL' ,'TYPE7')
            CALL READ_SENSOR_RWALL(SENSORS%SENSOR_TAB(ISEN) ,SENS_ID  ,TITR     ,
     .           UNITAB   ,LSUBMODEL)
c
          CASE ('NOT' ,'TYPE8')
            CALL READ_SENSOR_NOT(SENSORS%SENSOR_TAB(ISEN) ,SENS_ID  ,UNITAB   ,LSUBMODEL)
c
          CASE ('VEL' ,'TYPE9')
            CALL READ_SENSOR_VEL(SENSORS%SENSOR_TAB(ISEN) ,SENS_ID  ,
     .           ITABM1   ,UNITAB   ,LSUBMODEL)   
c
          CASE ('GAUGE' ,'TYPE10')            
            CALL READ_SENSOR_GAUGE(SENSORS%SENSOR_TAB(ISEN) ,SENS_ID  ,TITR     ,
     .           LGAUGE   ,UNITAB   ,LSUBMODEL)
c
          CASE ('RBODY' ,'TYPE11')
            CALL READ_SENSOR_RBODY(SENSORS%SENSOR_TAB(ISEN) ,SENS_ID  ,TITR     ,
     .           UNITAB   ,LSUBMODEL)
c
          CASE ('SECT' ,'TYPE12')
            CALL READ_SENSOR_SECT(SENSORS%SENSOR_TAB(ISEN) ,SENS_ID  ,TITR     ,
     .           UNITAB   ,LSUBMODEL)
c
          CASE ('WORK' ,'TYPE13')
            CALL READ_SENSOR_WORK(SENSORS%SENSOR_TAB(ISEN) ,SENS_ID  ,
     .           ITABM1   ,UNITAB   ,LSUBMODEL)   
c
          CASE ('ENERGY' ,'TYPE14')
            CALL READ_SENSOR_ENERGY(SENSORS%SENSOR_TAB(ISEN) ,SENS_ID  ,TITR     ,
     .           IPART   ,SUBSET   ,UNITAB  ,LSUBMODEL)
c
          CASE ('DIST_SURF' ,'TYPE15')
            CALL READ_SENSOR_DIST_SURF(SENSORS%SENSOR_TAB(ISEN) ,SENS_ID  ,
     .           ITABM1   ,UNITAB   ,LSUBMODEL)   
c
          CASE ('HIC' ,'TYPE16')
            CALL READ_SENSOR_HIC(SENSORS%SENSOR_TAB(ISEN) ,SENS_ID  ,TITR     , 
     .           LACCELM  ,UNITAB   ,LSUBMODEL)  
c
          CASE ('TEMP')
            CALL READ_SENSOR_TEMP(SENSORS%SENSOR_TAB(ISEN)  ,SENS_ID     ,TITR     , 
     .           IGRNOD   ,UNITAB   ,LSUBMODEL)   
c
          CASE ('NIC_NIJ')
            CALL READ_SENSOR_NIC(
     .           SENSORS%SENSOR_TAB(ISEN)  ,SENS_ID    ,TITR     ,ISKWN    , 
     .           UNITAB      ,LSUBMODEL  )   
c
          CASE ('USER1' ,'USER2','USER3')

            SENSORS%SENSOR_TAB(ISEN)%TSTART = INFINITY

            CALL READ_SENSOR_USER(HM_NSENS ,ISEN    ,TITR,
     .           UNITAB    ,LSUBMODEL      ,KEY     ,UNIT_ID ,SENS_ID ,
     .           SENSORS%SENSOR_TAB(ISEN),SENSOR_USER_STRUCT)
            SENSORS%SENSOR_TAB(ISEN)%SENS_ID = SENS_ID
          CASE('PYTHON')
            CALL READ_SENSOR_PYTHON(PYTHON,
     .           SENSORS%SENSOR_TAB(ISEN)  ,SENS_ID    , LSUBMODEL  )   

          CASE DEFAULT
            CALL ANCMSG(MSGID=43, ANMODE=ANINFO, MSGTYPE=MSGERROR,
     .                  C2=KEY(1:LEN_TRIM(KEY)),
     .                  I1=SENS_ID,
     .                  C1=TITR)        
        END SELECT 
c
        SENSORS%SENSOR_TAB(ISEN)%TITLE = TITR
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
