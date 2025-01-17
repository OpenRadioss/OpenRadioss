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
      !||    sensor_init             ../engine/source/tools/sensor/sensor_init.F
      !||--- called by ------------------------------------------------------
      !||    resol                   ../engine/source/engine/resol.F
      !||--- calls      -----------------------------------------------------
      !||    quicksort_i             ../common_source/tools/sort/quicksort.F
      !||    sensor_logical          ../engine/source/tools/sensor/sensor_logical.F
      !||    sensor_python           ../engine/source/tools/sensor/sensor_python.F90
      !||    sensor_time             ../engine/source/tools/sensor/sensor_time.F
      !||--- uses       -----------------------------------------------------
      !||    groupdef_mod            ../common_source/modules/groupdef_mod.F
      !||    my_alloc_mod            ../common_source/tools/memory/my_alloc.F90
      !||    python_funct_mod        ../common_source/modules/python_mod.F90
      !||    sensor_mod              ../common_source/modules/sensor_mod.F90
      !||    sensor_python_mod       ../engine/source/tools/sensor/sensor_python.F90
      !||====================================================================
        SUBROUTINE SENSOR_INIT( SUBSET,IPARG,NGROUC,
     .                          IPARTC,IPARTTG,IPARTS,IPARTQ,IPARTT,
     .                          IPARTP,IPARTR,SENSORS,TIME  ,TIMESTEP,
     .                          IOUT ,PYTHON,NTASK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE SENSOR_PYTHON_MOD
        USE PYTHON_FUNCT_MOD
        USE GROUPDEF_MOD
        USE SENSOR_MOD
        USE my_alloc_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "mvsiz_p.inc"
#include "param_c.inc"
#include "com01_c.inc"
#include "com04_c.inc"
#include "task_c.inc"
#include "parit_c.inc"
#include "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real ,INTENT(IN) :: TIME,TIMESTEP
      TYPE (SUBSET_) , DIMENSION(NSUBS), INTENT(IN) :: SUBSET
      INTEGER, INTENT(IN) :: NGROUC
      INTEGER, INTENT(IN) :: IOUT
      INTEGER, DIMENSION(*), TARGET, INTENT(IN) :: IPARTC,IPARTTG,IPARTS,IPARTQ
      INTEGER, DIMENSION(*), TARGET, INTENT(IN) :: IPARTT,IPARTP,IPARTR
      INTEGER, DIMENSION(NPARG,*), INTENT(IN) :: IPARG
      TYPE (SENSORS_) ,INTENT(INOUT) :: SENSORS
      TYPE(PYTHON_), INTENT(INOUT) :: PYTHON
      INTEGER, INTENT(IN) :: NTASK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: BOOL
      INTEGER :: I,K,J,IC,IG,NG,NP,IJK,KJI
      INTEGER :: TYP,IPART,IPART_2,ISUBS,IGRN,NBR_GROUP,NSENSOR
      INTEGER :: NFT,NF1,ITY,NEL
      INTEGER, DIMENSION(:), POINTER :: IPART_POINTER
      INTEGER :: NUMBER_PART_GROUP,NUMBER_PART_GROUP_2
      INTEGER, DIMENSION(MVSIZ) :: INDX_PART,INDX_PART_2
      INTEGER :: NUMBER_SENSOR_PER_GROUP,NUMBER_SENSOR,TOT_NUMBER_PART_GROUP
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: CHECK_BOOL
      LOGICAL, DIMENSION(:), ALLOCATABLE :: BOOL_SENSOR

      INTEGER :: ISURF
      INTEGER :: SURF_DIST_SENSOR,ENERGY_SENSOR,SENSOR_TEMP
      INTEGER, DIMENSION(SENSORS%NSENSOR) :: INDX_SURF_DIST_SENSOR,INDX_ENERGY,INDX_TEMP
      INTEGER :: SIZE_COMM17_MIN_MAX,SIZE_COMM17_MEAN
      LOGICAL :: ANY_PYTHON_SENSOR
      INTEGER, DIMENSION(:), ALLOCATABLE :: UIDS
      INTEGER, DIMENSION(:), ALLOCATABLE :: STATUSES
      INTEGER, DIMENSION(:), ALLOCATABLE :: TYPES
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: RESULTS
      INTEGER :: ISENS
!   *********************************************
!       In 1 element group : 1 or several PART 
!          --> one or several /SENSOR linked to the PARTs
!       structure of SENS_GROUP(NG)%PART
!       for energy sensor type 1 : only 1 part per sensor
!       for energy sensor type 2 : several part per sensor
!       %PART(:,1): id of the part         
!       %PART(:,2): type of energy sensor  
!       %PART(:,3): id of the sensor      
!
!       Example with one element group with :
!           * 2 /PARTs
!           * one sensor type=1 per PART
!       %PART(:,1): id of the part         |   32  |   89   |   
!       %PART(:,2): type of energy sensor  |   1   |   1    |
!       %PART(:,3): id of the sensor       |   3   |   67   |

!       Example with one element group with :
!           * 2 /PARTs
!           * 2 sensors type= 1 & 2
!       %PART(:,1): id of the part         |  32   |   32 89 |
!       %PART(:,2): type of energy sensor  |   1   |   2     |
!       %PART(:,3): id of the sensor       |   3   |   67    |
!
c=======================================================================
      ! Allocate SENSORS%ngr_sensor
      CALL MY_ALLOC(SENSORS%NGR_SENSOR,NTASK)
C
      SENSORS%COMM_SENS14%BOOL = .FALSE.
      SENSORS%COMM_SENS16%BOOL = .FALSE.
      SENSORS%COMM_SENS17%BOOL = .FALSE.
      ANY_PYTHON_SENSOR = .FALSE.

      SENSORS%COMM_SENS14%BUFFER_SIZE_MIN_MAX = 0
      SENSORS%COMM_SENS14%BUFFER_SIZE_MEAN = 0

      SENSORS%COMM_SENS16%BUFFER_SIZE_MIN_MAX = 0
      SENSORS%COMM_SENS16%BUFFER_SIZE_MEAN = 0

      SENSORS%COMM_SENS17%BUFFER_SIZE_MIN_MAX = 0
      SENSORS%COMM_SENS17%BUFFER_SIZE_MEAN = 0
c
      SURF_DIST_SENSOR = 0
      ENERGY_SENSOR    = 0
      SENSOR_TEMP      = 0
      NSENSOR = SENSORS%NSENSOR
      INDX_SURF_DIST_SENSOR(1:NSENSOR) = 0
      INDX_ENERGY(1:NSENSOR) = 0
      INDX_TEMP(1:NSENSOR)   = 0
      !   -----------------------------------------
      !   count the number of sensors by type
      DO K=1,NSENSOR
          SENSORS%SENSOR_TAB(K)%RESULTS(1:SENSOR_RESULT_SIZE) = ZERO
          TYP = SENSORS%SENSOR_TAB(K)%TYPE
          !   -------------------- 
          IF (TYP==14) THEN
              !   sensor energy
              ENERGY_SENSOR = ENERGY_SENSOR + 1
              INDX_ENERGY(ENERGY_SENSOR) = K
          ELSE IF (TYP==16) THEN
              !   distance to surface
              ISURF = SENSORS%SENSOR_TAB(K)%IPARAM(2)         
              IF (ISURF/=0) THEN
                  SURF_DIST_SENSOR = SURF_DIST_SENSOR + 1
                  INDX_SURF_DIST_SENSOR(SURF_DIST_SENSOR) = K
              ENDIF
          ELSE IF (TYP==17) THEN
              !   sensor temperature
              IGRN = SENSORS%SENSOR_TAB(K)%IPARAM(1)
              IF (IGRN > 0) THEN
                  SENSOR_TEMP = SENSOR_TEMP + 1
                  INDX_TEMP(SENSOR_TEMP) = K
              ENDIF
          ENDIF
          !   --------------------        
      ENDDO
      !   -----------------------------------------
      !   activation of the mpi communication
      IF (ENERGY_SENSOR>0) SENSORS%COMM_SENS14%BOOL = .TRUE.
      SENSORS%COMM_SENS14%NUM_SENS = ENERGY_SENSOR   

      IF (SURF_DIST_SENSOR>0) SENSORS%COMM_SENS16%BOOL = .TRUE.
      SENSORS%COMM_SENS16%NUM_SENS = SURF_DIST_SENSOR 

      IF (SENSOR_TEMP>0) SENSORS%COMM_SENS17%BOOL = .TRUE.
      SENSORS%COMM_SENS17%NUM_SENS = SENSOR_TEMP

      !   ---------------------
      !   ENERGY SENSOR
      IF (IPARIT>0) THEN
         ALLOCATE( SENSORS%COMM_SENS14%ID_SENS(ENERGY_SENSOR) )
         IF(ENERGY_SENSOR>0) SENSORS%COMM_SENS14%ID_SENS(1:ENERGY_SENSOR) = INDX_ENERGY(1:ENERGY_SENSOR)
         ALLOCATE( SENSORS%SENS_GROUP(NGROUP) )
         SENSORS%SENS_GROUP(1:NGROUP)%NUM_PART = 0
         ALLOCATE( SENSORS%SENSOR_STRUCT(NSENSOR) )
         SENSORS%SENSOR_STRUCT(:)%TYP = 0     !   type of part : subset (=2) or not (=1)
         SENSORS%SENSOR_STRUCT(:)%PART = 0    !   part id
         SENSORS%SENSOR_STRUCT(:)%NB_SUB = 0  !   number of subset

         IF(ENERGY_SENSOR>0) THEN
             !   -----------------------------------------
             !   get the /PART id for each sensor
             DO I=1,ENERGY_SENSOR
                    K = INDX_ENERGY(I)
                    TYP = SENSORS%SENSOR_TAB(K)%TYPE
                    !   --------------------    
                    !   sensor energy
                    IPART  = SENSORS%SENSOR_TAB(K)%IPARAM(1)
                    ISUBS  = SENSORS%SENSOR_TAB(K)%IPARAM(2)
                    IF(IPART>0) THEN
                    !   only one part per sensor
                        SENSORS%SENSOR_STRUCT(K)%TYP = 1
                        SENSORS%SENSOR_STRUCT(K)%PART = IPART !   part id
                    ELSEIF(ISUBS>0) THEN
                    !   several subparts per sensor
                      SENSORS%SENSOR_STRUCT(K)%TYP = 2
                        NP = SUBSET(ISUBS)%NTPART   !   number of part 
                        SENSORS%SENSOR_STRUCT(K)%NB_SUB = NP
                        ALLOCATE( SENSORS%SENSOR_STRUCT(K)%SUB(NP) )
                        DO J=1,NP
                          SENSORS%SENSOR_STRUCT(K)%SUB(J)%PART = SUBSET(ISUBS)%TPART(J) ! part id
                        ENDDO
                    ENDIF    
                    !   -------------------                                   
             ENDDO

             !   -----------------------------------------
             !   count the number of element group for each part
             !   and create a pointer to the FBSAV6 double precision array (parith/on array)
             DO NG = 1, NGROUP
                  ITY = IPARG(5,NG)
                  NFT = IPARG(3,NG)
                  NEL = IPARG(2,NG)
                  NF1 = NFT + 1
                  IPART = 0
                  IF(ITY==3) THEN
                      IPART = IPARTC(NF1)     !   shell
                      IPART_POINTER => IPARTC(NF1:NF1+NEL)
                  ELSEIF(ITY==7) THEN
                      IPART = IPARTTG(NF1)    !   shell3n
                      IPART_POINTER => IPARTTG(NF1:NF1+NEL)
                  ELSEIF(ITY==1.OR.ITY==101) THEN
                      IPART = IPARTS(NF1)     !   solid & ig3dsolid element
                      IPART_POINTER => IPARTS(NF1:NF1+NEL)
                  ELSEIF(ITY==2) THEN
                      IPART = IPARTQ(NF1)     !   quad
                      IPART_POINTER => IPARTQ(NF1:NF1+NEL)
                  ELSEIF(ITY==4) THEN
                      IPART = IPARTT(NF1)     !   truss
                      IPART_POINTER => IPARTT(NF1:NF1+NEL)
                  ELSEIF(ITY==5) THEN
                      IPART = IPARTP(NF1)     !   poutre
                      IPART_POINTER => IPARTP(NF1:NF1+NEL)
                  ELSEIF(ITY==6) THEN
                      IPART = IPARTR(NF1)     !   poutre
                      IPART_POINTER => IPARTP(NF1:NF1+NEL)
                  ENDIF

                  IF(IPART/=0) THEN
                    INDX_PART(1:MVSIZ) = 0
                    INDX_PART_2(1:MVSIZ) = 0

                    NUMBER_PART_GROUP = 1     !   number of part in a element group
                    INDX_PART(NUMBER_PART_GROUP) =  IPART
                    DO I=2,NEL
                      IPART_2 = IPART_POINTER(I)
                      IF(IPART/=IPART_2) THEN
                          NUMBER_PART_GROUP = NUMBER_PART_GROUP + 1   
                          INDX_PART(NUMBER_PART_GROUP) =  IPART_2
                          IPART = IPART_2
                      ENDIF
                    ENDDO
                    CALL QUICKSORT_I(INDX_PART,1,NUMBER_PART_GROUP)
                    INDX_PART_2(1:NUMBER_PART_GROUP) = INDX_PART(1:NUMBER_PART_GROUP)

                    NUMBER_PART_GROUP_2 = NUMBER_PART_GROUP
                    NUMBER_PART_GROUP = 1
                    INDX_PART(NUMBER_PART_GROUP) = INDX_PART_2(1)
                    DO I=2,NUMBER_PART_GROUP_2
                      IF( INDX_PART_2(I-1)/=INDX_PART_2(I) ) THEN
                          NUMBER_PART_GROUP = NUMBER_PART_GROUP + 1
                          INDX_PART(NUMBER_PART_GROUP) = INDX_PART_2(I)
                      ENDIF
                    ENDDO

                    SENSORS%SENS_GROUP(NG)%NUM_PART = -NUMBER_PART_GROUP       !   number of part in one groupe
                  !   count the number of sensor for a element group 
                    ALLOCATE( BOOL_SENSOR(ENERGY_SENSOR) )
                    BOOL_SENSOR(1:ENERGY_SENSOR) = .TRUE.

                    NUMBER_SENSOR_PER_GROUP = 0
                    DO IJK=1,NUMBER_PART_GROUP
                      IPART = INDX_PART(IJK)
                      DO I=1,ENERGY_SENSOR
                        K = INDX_ENERGY(I)
                        IF(SENSORS%SENSOR_STRUCT(K)%TYP == 1) THEN
                              IF(SENSORS%SENSOR_STRUCT(K)%PART == IPART) THEN
                                  NUMBER_SENSOR_PER_GROUP = NUMBER_SENSOR_PER_GROUP + 1
                              ENDIF
                        ELSEIF(SENSORS%SENSOR_STRUCT(K)%TYP == 2) THEN
                            DO J=1,SENSORS%SENSOR_STRUCT(K)%NB_SUB
                              IF(BOOL_SENSOR(I).AND.SENSORS%SENSOR_STRUCT(K)%SUB(J)%PART == IPART) THEN
                                  NUMBER_SENSOR_PER_GROUP = NUMBER_SENSOR_PER_GROUP + 1
                                  BOOL_SENSOR(I)=.FALSE.
                              ENDIF                                     
                            ENDDO
                        ENDIF
                      ENDDO
                    ENDDO
                    DEALLOCATE( BOOL_SENSOR )
                    ! total size of SENS_GROUP(NG)%PART
                    TOT_NUMBER_PART_GROUP = NUMBER_PART_GROUP * NUMBER_SENSOR_PER_GROUP

                    ALLOCATE( SENSORS%SENS_GROUP(NG)%PART(TOT_NUMBER_PART_GROUP,3) )
                    SENSORS%SENS_GROUP(NG)%PART(1:TOT_NUMBER_PART_GROUP,1:3) = 0

                    KJI = 0
                    DO IJK=1,NUMBER_PART_GROUP
                      DO I=1,NUMBER_SENSOR_PER_GROUP
                      KJI = KJI + 1
                      SENSORS%SENS_GROUP(NG)%PART(KJI,1) = -INDX_PART(IJK)!   part id
                      ENDDO
                    ENDDO

                    ALLOCATE( CHECK_BOOL(ENERGY_SENSOR,NUMBER_PART_GROUP) )
                    DO IJK=1,ENERGY_SENSOR
                      CHECK_BOOL(IJK,1:NUMBER_PART_GROUP) = -INDX_PART(1:NUMBER_PART_GROUP)!   part id
                    ENDDO

                    BOOL = .FALSE.
                    KJI = 0
                    DO IJK=1,NUMBER_PART_GROUP
                      IPART = INDX_PART(IJK)
                      NUMBER_SENSOR = 0
                      DO I=1,ENERGY_SENSOR
                        K = INDX_ENERGY(I)
                        IF(SENSORS%SENSOR_STRUCT(K)%TYP == 1) THEN
                          IF(CHECK_BOOL(I,IJK)<0) THEN !   check if already done (loop over the part of an element group!!!)
                              IF(SENSORS%SENSOR_STRUCT(K)%PART == IPART) THEN
                                  KJI = KJI + 1
                                  !   number of group per part for a given sensor
                                  SENSORS%SENSOR_STRUCT(K)%NUM_GROUP_PART = SENSORS%SENSOR_STRUCT(K)%NUM_GROUP_PART + 1
                                  SENSORS%SENS_GROUP(NG)%PART(KJI,2) = 1    !   type of energy sensor
                                  SENSORS%SENS_GROUP(NG)%PART(KJI,3) = K   !   index of the current sensor

                                  SENSORS%SENS_GROUP(NG)%PART(KJI,1) = ABS(SENSORS%SENS_GROUP(NG)%PART(KJI,1))
                                  CHECK_BOOL(I,IJK) = ABS(CHECK_BOOL(I,IJK))
                                  BOOL = .TRUE.
                              ENDIF
                          ENDIF
                        ELSEIF(SENSORS%SENSOR_STRUCT(K)%TYP == 2) THEN
                          IF(CHECK_BOOL(I,IJK)<0) THEN !   check if already done (loop over the part of an element group!!!)
                            DO J=1,SENSORS%SENSOR_STRUCT(K)%NB_SUB
                              IF(SENSORS%SENSOR_STRUCT(K)%SUB(J)%PART == IPART) THEN
                                  KJI = KJI + 1
                                  !   number of group per subpart
                                  SENSORS%SENSOR_STRUCT(K)%NUM_GROUP_PART = SENSORS%SENSOR_STRUCT(K)%NUM_GROUP_PART + 1
                                  SENSORS%SENS_GROUP(NG)%PART(KJI,2) = 2    !   type of energy sensor
                                  SENSORS%SENS_GROUP(NG)%PART(KJI,3) = K   !   index of the current sensor
                                  SENSORS%SENS_GROUP(NG)%PART(KJI,1) = ABS(SENSORS%SENS_GROUP(NG)%PART(KJI,1))
                                  CHECK_BOOL(I,IJK) = ABS(CHECK_BOOL(I,IJK))
                                  BOOL = .TRUE.
                              ENDIF                                     
                            ENDDO
                          ENDIF
                        ENDIF
                      ENDDO
                    ENDDO

                    IF(BOOL) SENSORS%SENS_GROUP(NG)%NUM_PART = KJI       !   number of part in one groupe * number of sensor        
                    DEALLOCATE( CHECK_BOOL )             
                  ENDIF
             ENDDO
             !   -----------------------------------------
             !   allocation of FBSAV6 double precision array (parith/on array)
             DO I=1,ENERGY_SENSOR
                  K = INDX_ENERGY(I)
                  ALLOCATE( SENSORS%SENSOR_STRUCT(K)%FBSAV6_SENS(2,6,NTHREAD) )
                  SENSORS%SENSOR_STRUCT(K)%FBSAV6_SENS(1:2,1:6,1:NTHREAD) = ZERO
             ENDDO    
             !   -----------------------------------------           
         ENDIF
      ELSE
        ALLOCATE( SENSORS%SENS_GROUP(0) )
        ALLOCATE( SENSORS%SENSOR_STRUCT(0) )
        ALLOCATE( SENSORS%COMM_SENS14%ID_SENS(0) )
      ENDIF

      !   ---------------------
      !   DISTANCE TO SURFACE SENSOR
      ALLOCATE( SENSORS%COMM_SENS16%ID_SENS(SURF_DIST_SENSOR) )
      IF(SURF_DIST_SENSOR>0) THEN
          SENSORS%COMM_SENS16%ID_SENS(1:SURF_DIST_SENSOR) = INDX_SURF_DIST_SENSOR(1:SURF_DIST_SENSOR)
      ENDIF
      !   ---------------------
      !   SENSOR TEMPERATURE
      SIZE_COMM17_MIN_MAX = 0
      SIZE_COMM17_MEAN = 0
      ALLOCATE( SENSORS%COMM_SENS17%ID_SENS(SENSOR_TEMP) )
      IF (SENSOR_TEMP > 0) THEN
          DO I=1,SENSOR_TEMP
              K = INDX_TEMP(I)
              SENSORS%COMM_SENS17%ID_SENS(I) = K
              !   Temp max and min : 1 data per sensor
              SIZE_COMM17_MIN_MAX = SIZE_COMM17_MIN_MAX + 1
              !   Temp mean  : 6 data per sensor (parith/on) / 1 data per sensor (parith/off) 
              IF (IPARIT>0) THEN
                  ALLOCATE( SENSORS%SENSOR_STRUCT(K)%FBSAV6_SENS(1,6,1) )
                  SENSORS%SENSOR_STRUCT(K)%FBSAV6_SENS(1,1:6,1:1) = ZERO
                  SIZE_COMM17_MEAN = SIZE_COMM17_MEAN + 6
              ELSE
                  ALLOCATE( SENSORS%SENSOR_STRUCT(K)%FBSAV6_SENS(1,1,1) )
                  SENSORS%SENSOR_STRUCT(K)%FBSAV6_SENS(1,1,1) = ZERO
                  SIZE_COMM17_MEAN = SIZE_COMM17_MEAN + 1
              ENDIF
          ENDDO    
          SENSORS%COMM_SENS17%BUFFER_SIZE_MIN_MAX = SIZE_COMM17_MIN_MAX
          SENSORS%COMM_SENS17%BUFFER_SIZE_MEAN    = SIZE_COMM17_MEAN
      ENDIF
      !   ---------------------

C ---------------------------------------------------------------------
C   Reinitialize sensors defined in /SENS/RESET
c----------------------------------------------------------------------
      IF (NSENSOR > 0 ) THEN
        IF (SENSORS%NRESET == NSENSOR) THEN
          DO I=1,NSENSOR
            SENSORS%SENSOR_TAB(I)%STATUS = 0        
            SENSORS%SENSOR_TAB(I)%TSTART = 1.E20
            SENSORS%SENSOR_TAB(I)%TCRIT  = 1.E20
            SENSORS%SENSOR_TAB(I)%VALUE  = ZERO     
            SENSORS%SENSOR_TAB(I)%VAR(:) = ZERO
            IF (ISPMD == 0) WRITE (IOUT,100) SENSORS%SENSOR_TAB(I)%SENS_ID
          ENDDO
        ELSE IF (SENSORS%NRESET > 0) THEN
          DO J=1,SENSORS%NRESET
            DO I=1,NSENSOR
              IF (SENSORS%RESET(J) == SENSORS%SENSOR_TAB(I)%SENS_ID) THEN
                SENSORS%SENSOR_TAB(I)%STATUS = 0        
                SENSORS%SENSOR_TAB(I)%TSTART = 1.E20
                SENSORS%SENSOR_TAB(I)%TCRIT  = 1.E20
                SENSORS%SENSOR_TAB(I)%VALUE  = ZERO     
                SENSORS%SENSOR_TAB(I)%VAR(:) = ZERO
                IF (ISPMD == 0) WRITE (IOUT,100) SENSORS%SENSOR_TAB(I)%SENS_ID
                EXIT
              END IF
            ENDDO
          ENDDO
        END IF
        IF (ALLOCATED(SENSORS%RESET)) DEALLOCATE (SENSORS%RESET)
        SENSORS%NRESET = 0
      ENDIF

      

C ---------------------------------------------------------------------
C Play Sensors which could be active at TT=0
c----------------------------------------------------------------------
      IF (NSENSOR > 0 ) THEN
        DO I=1,NSENSOR
            ! Time Sensor if TT=0
            TYP    = SENSORS%SENSOR_TAB(I)%TYPE
            IF (TYP == 0) THEN 
              CALL SENSOR_TIME(SENSORS%SENSOR_TAB(I) ,TIME   ,TIMESTEP)
            ENDIF
            IF( TYP == SENSOR_TYPE_PYTHON ) THEN
              ANY_PYTHON_SENSOR = .TRUE.
            ENDIF
        ENDDO
     
        ! Logical Sensors can be active at TT=0
        CALL SENSOR_LOGICAL(SENSORS)
      ELSE
        SENSORS%LOGICAL_SENSOR_COUNT=0
      ENDIF

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

c-----------------------------------------------------------------------      
100   FORMAT(' SENSOR NUMBER ',I10,' ,IS RESET TO THE INITIAL STATE')
c-----------
      RETURN
      END SUBROUTINE SENSOR_INIT
