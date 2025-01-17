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
      !||    sensor_base             ../engine/source/tools/sensor/sensor_base.F
      !||--- called by ------------------------------------------------------
      !||    resol                   ../engine/source/engine/resol.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                  ../engine/source/output/message/message.F
      !||    arret                   ../engine/source/system/arret.F
      !||    sensor_acc              ../engine/source/tools/sensor/sensor_acc.F
      !||    sensor_contact          ../engine/source/tools/sensor/sensor_contact.F
      !||    sensor_dist             ../engine/source/tools/sensor/sensor_dist.F
      !||    sensor_dist_surf        ../engine/source/tools/sensor/sensor_dist_surf.F
      !||    sensor_energy           ../engine/source/tools/sensor/sensor_energy.F
      !||    sensor_gauge            ../engine/source/tools/sensor/sensor_gauge.F
      !||    sensor_hic              ../engine/source/tools/sensor/sensor_hic.F
      !||    sensor_nic              ../engine/source/tools/sensor/sensor_nic.F
      !||    sensor_python           ../engine/source/tools/sensor/sensor_python.F90
      !||    sensor_rbody            ../engine/source/tools/sensor/sensor_rbody.F
      !||    sensor_rwall            ../engine/source/tools/sensor/sensor_rwall.F
      !||    sensor_section          ../engine/source/tools/sensor/sensor_section.F
      !||    sensor_temp             ../engine/source/tools/sensor/sensor_temp.F
      !||    sensor_time             ../engine/source/tools/sensor/sensor_time.F
      !||    sensor_vel              ../engine/source/tools/sensor/sensor_vel.F
      !||    sensor_work             ../engine/source/tools/sensor/sensor_work.F
      !||--- uses       -----------------------------------------------------
      !||    groupdef_mod            ../common_source/modules/groupdef_mod.F
      !||    message_mod             ../engine/share/message_module/message_mod.F
      !||    python_funct_mod        ../common_source/modules/python_mod.F90
      !||    sensor_mod              ../common_source/modules/sensor_mod.F90
      !||    sensor_python_mod       ../engine/source/tools/sensor/sensor_python.F90
      !||====================================================================
      SUBROUTINE SENSOR_BASE(SENSORS  ,NSENSOR   ,TIME      ,TIMESTEP  ,
     .           XSENS     ,IPARI     ,PARTSAV2  ,GAUGE     ,FSAV      ,
     .           X         ,V         ,A         ,ACC       ,NPRW      ,
     .           SUBSET    ,IGRSURF   ,IGRNOD    ,PYTHON)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PYTHON_FUNCT_MOD
      USE SENSOR_PYTHON_MOD
      USE GROUPDEF_MOD
      USE SENSOR_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "userlib.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER                           ,INTENT(IN) :: NSENSOR
      INTEGER ,DIMENSION(NPARI,NINTER)  ,INTENT(IN) :: IPARI
      INTEGER ,DIMENSION(*)             ,INTENT(IN) :: NPRW
      my_real ,DIMENSION(3,NUMNOD)      ,INTENT(IN) :: X,V,A
      my_real ,DIMENSION(12,NSENSOR)    ,INTENT(INOUT) :: XSENS
      my_real ,DIMENSION(LLACCELM,*)    ,INTENT(IN) :: ACC
      my_real ,DIMENSION(NTHVKI,*)      ,INTENT(IN) :: FSAV
      my_real ,DIMENSION(LLGAUGE,*)     ,INTENT(IN) :: GAUGE
      my_real ,DIMENSION(2,*)           ,INTENT(IN) :: PARTSAV2
      my_real ,INTENT(IN) :: TIME,TIMESTEP
      TYPE (SUBSET_) ,DIMENSION(NSUBS)  ,INTENT(IN) :: SUBSET
      TYPE(SURF_)    ,DIMENSION(NSURF)  ,INTENT(IN) :: IGRSURF
      TYPE(GROUP_)   ,DIMENSION(NGRNOD) ,INTENT(IN) :: IGRNOD
      TYPE (SENSORS_) ,INTENT(INOUT) :: SENSORS
      TYPE(PYTHON_), INTENT(INOUT) :: PYTHON
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER TYP,ISENS,SENS_ID,SIZE,STABS,SFSAV
      CHARACTER(LEN=256) :: OPTION
      LOGICAL :: ANY_PYTHON_SENSOR
      INTEGER, DIMENSION(:), ALLOCATABLE :: UIDS
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: RESULTS
      INTEGER, DIMENSION(:), ALLOCATABLE :: STATUSES
      INTEGER, DIMENSION(:), ALLOCATABLE :: TYPES
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     this routine checks activation condition of all basic sensor types
C=======================================================================
      ANY_PYTHON_SENSOR = .FALSE.
      STABS   = SENSORS%STABSEN 
      SFSAV   = SENSORS%SFSAV
c--------------------------------------------------
c     Parallel loop over sensor list
c--------------------------------------------------

!$OMP PARALLEL DO PRIVATE(ISENS,TYP)
c--------------------------------------------------
      DO ISENS = 1,NSENSOR
        SENSORS%SENSOR_TAB(ISENS)%RESULTS(SENSOR_RESULT_SIZE) = ZERO
        TYP = SENSORS%SENSOR_TAB(ISENS)%TYPE
C--------------------------------
C       SENSOR - TIME
C--------------------------------
        IF (TYP == 0) THEN

          CALL SENSOR_TIME(SENSORS%SENSOR_TAB(ISENS) ,TIME   ,TIMESTEP)

C--------------------------------
C       SENSOR - ACCELEROMETER
C--------------------------------
        ELSEIF (TYP == 1) THEN
c
          CALL SENSOR_ACC(SENSORS%SENSOR_TAB(ISENS) ,ACC)
c
C--------------------------------
C       SENSOR - DISTANCE
C--------------------------------
        ELSEIF (TYP == 2) THEN
c
          CALL SENSOR_DIST(SENSORS%SENSOR_TAB(ISENS) ,X    ,XSENS(1,ISENS))
c
C--------------------------------
C       SENSOR - CONTACT
C--------------------------------
        ELSEIF (TYP == 6)THEN
          CALL SENSOR_CONTACT(SENSORS%SENSOR_TAB(ISENS),
     .         IPARI    ,SFSAV   ,STABS   ,SENSORS%TABSENSOR   ,SENSORS%FSAV)

C--------------------------------
C       SENSOR - RWALL
C--------------------------------
        ELSEIF(TYP == 7)THEN

          CALL SENSOR_RWALL(SENSORS%SENSOR_TAB(ISENS) ,
     .         NPRW    ,SFSAV   ,STABS   ,SENSORS%TABSENSOR   ,SENSORS%FSAV)

C--------------------------------
C       SENSOR - NODAL VELOCITY
C--------------------------------
        ELSEIF(TYP == 9)THEN

          CALL SENSOR_VEL(SENSORS%SENSOR_TAB(ISENS),V)

C--------------------------------
C       SENSOR - GAUGE
C--------------------------------
        ELSEIF (TYP == 10) THEN

          CALL SENSOR_GAUGE(SENSORS%SENSOR_TAB(ISENS) ,GAUGE)

C--------------------------------
C       SENSOR - RBODY
C--------------------------------
        ELSEIF (TYP == 11) THEN

          CALL SENSOR_RBODY(SENSORS%SENSOR_TAB(ISENS),SFSAV   ,STABS   ,SENSORS%TABSENSOR,SENSORS%FSAV)


C--------------------------------
C       SENSOR - SECTION
C--------------------------------
        ELSEIF (TYP == 12) THEN

          CALL SENSOR_SECTION(SENSORS%SENSOR_TAB(ISENS),
     .         SFSAV   ,STABS   ,SENSORS%TABSENSOR   ,FSAV   ,SENSORS%FSAV  )

C--------------------------------
C       SENSOR WORK
C--------------------------------
        ELSEIF (TYP == 13)THEN

          CALL SENSOR_WORK(SENSORS%SENSOR_TAB(ISENS),X   ,XSENS(1,ISENS)   ,
     .         SFSAV   ,STABS   ,SENSORS%TABSENSOR      ,SENSORS%FSAV  )
c
c--------------------------------
c       SENSOR --- ENERGY ---
c--------------------------------        
        ELSEIF (TYP == 14) THEN
 
          CALL SENSOR_ENERGY(SENSORS%SENSOR_TAB(ISENS),ISENS ,SUBSET,PARTSAV2,
     *                       SENSORS%NSENSOR,SENSORS%SENSOR_STRUCT )
c
c--------------------------------
c       SENSOR Distance to surface
C--------------------------------
        ELSEIF (TYP == 15) THEN
c
          CALL SENSOR_DIST_SURF(SENSORS%SENSOR_TAB(ISENS) ,X   ,IGRSURF  )
c
c--------------------------------
c       SENSOR HIC
c--------------------------------
        ELSEIF (TYP == 16) THEN
c
          CALL SENSOR_HIC(SENSORS%SENSOR_TAB(ISENS) ,A   ,ACC  )
c
c--------------------------------
c       SENSOR TEMPERATURE
c--------------------------------
        ELSEIF (TYP == 17) THEN
c
          CALL SENSOR_TEMP(SENSORS%SENSOR_TAB(ISENS) ,ISENS ,IGRNOD)
c
c--------------------------------
        ELSEIF (TYP == 19) THEN
c--------------------------------
c       SENSOR NIC-Nij
c
          CALL SENSOR_NIC(SENSORS%SENSOR_TAB(ISENS) )
c
c--------------------------------
        ELSEIF (TYP == 29) THEN
c--------------------------------
c       USER SENSOR TYPE 29

          SENS_ID  = SENSORS%SENSOR_TAB(ISENS)%SENS_ID

          IF (USERL_AVAIL==1) THEN
              CALL ENG_USERLIB_USER_SENS(TYP,SENS_ID)
          ELSE
              ! ----------------
              ! ERROR to be printed & exit
              OPTION='USER SENSOR 29'
              SIZE=LEN_TRIM(OPTION)
              CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
              CALL ARRET(2)
             ! ----------------
          ENDIF
c--------------------------------
        ELSEIF (TYP == 30) THEN
c--------------------------------
c       USER SENSOR TYPE 30

          SENS_ID  = SENSORS%SENSOR_TAB(ISENS)%SENS_ID

          IF (USERL_AVAIL==1) THEN
              CALL ENG_USERLIB_USER_SENS(TYP,SENS_ID)
          ELSE
              ! ----------------
              ! ERROR to be printed & exit
              OPTION='USER SENSOR 30'
              SIZE=LEN_TRIM(OPTION)
              CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
              CALL ARRET(2)
             ! ----------------
          ENDIF
c--------------------------------
        ELSEIF (TYP == 31) THEN
c--------------------------------
c       USER SENSOR TYPE 31
          SENS_ID  = SENSORS%SENSOR_TAB(ISENS)%SENS_ID

          IF (USERL_AVAIL==1) THEN
              CALL ENG_USERLIB_USER_SENS(TYP,SENS_ID)
          ELSE
              ! ----------------
              ! ERROR to be printed & exit
              OPTION='USER SENSOR 31'
              SIZE=LEN_TRIM(OPTION)
              CALL ANCMSG(MSGID=257,C1=OPTION(1:SIZE),ANMODE=ANINFO)
              CALL ARRET(2)
             ! ----------------
          ENDIF
       ELSEIF (TYP == SENSOR_TYPE_PYTHON) THEN
c         CALL SENSOR_PYTHON(SENSORS%SENSOR_TAB(ISENS) )
         ANY_PYTHON_SENSOR = .TRUE.
       ENDIF  ! TYP 
c----
      END DO  ! ISENS
c--------------------------------------------------
c     END of parallel loop
!$OMP END PARALLEL DO
c--------------------------------------------------
      IF(PYTHON%NB_FUNCTS > 0) THEN
        ALLOCATE(UIDS(NSENSOR))
        ALLOCATE(RESULTS(SENSOR_RESULT_SIZE,NSENSOR))
        ALLOCATE(STATUSES(NSENSOR))
        ALLOCATE(TYPES(NSENSOR))
        DO ISENS = 1,NSENSOR
          TYP = SENSORS%SENSOR_TAB(ISENS)%TYPE
          TYPES(ISENS) = TYP
          UIDS(ISENS) = SENSORS%SENSOR_TAB(ISENS)%SENS_ID
          STATUSES(ISENS) = SENSORS%SENSOR_TAB(ISENS)%STATUS 
          RESULTS(1:SENSOR_RESULT_SIZE,ISENS) = SENSORS%SENSOR_TAB(ISENS)%RESULTS(1:SENSOR_RESULT_SIZE)
        END DO  ! ISENS
        CALL PYTHON_UPDATE_SENSORS(TYPES,UIDS,STATUSES,RESULTS,NSENSOR)
        DEALLOCATE(TYPES)
        DEALLOCATE(UIDS)
        DEALLOCATE(RESULTS)
        DEALLOCATE(STATUSES)
      ENDIF
      IF (ANY_PYTHON_SENSOR) THEN
        DO ISENS = 1,NSENSOR
          TYP = SENSORS%SENSOR_TAB(ISENS)%TYPE
          IF (TYP == SENSOR_TYPE_PYTHON) THEN
            CALL SENSOR_PYTHON(SENSORS%SENSOR_TAB(ISENS))
          ENDIF  ! TYP 
        END DO  ! ISENS
      ENDIF
c
c--------------------------------------------------
      IF (STABS > 0) SENSORS%FSAV(1:12,1:6,1:SFSAV) = ZERO

c-----------
      RETURN
      END
