Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  SENSOR_TEMP0                  source/tools/sensor/sensor_temp0.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        SPMD_ALLREDUCE_DB             source/mpi/generic/spmd_allreduce_db.F
Chd|        SUM_6_FLOAT                   source/system/parit.F         
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR_TEMP0(SENSOR,IGRNOD,TEMP,WEIGHT)
c-----------------------------------------------------------------------
!$COMMENT
!       SENSOR_TEMP description :
!           computation of min/max or mean temperature 
!       SENSOR_TEMP organization :
!           - computation of local min/max/mean temperature
!           - communication between processor and globalization
!           - of min/max/mean
!$ENDCOMMENT
c-----------------------------------------------
C   M o d u l e s
c-----------------------------------------------
      USE GROUPDEF_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real :: TEMP(NUMNOD)
      TYPE(GROUP_)  ,DIMENSION(NGRNOD) ,INTENT(IN) :: IGRNOD
      TYPE (SENSOR_STR_),DIMENSION(NSENSOR), INTENT(INOUT) :: SENSOR
      INTEGER, DIMENSION(NUMNOD), INTENT(in) :: WEIGHT
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
      INTEGER :: ISENS,INDEX_SUM6FLOAT
      INTEGER :: I,IGN,INOD,NNOD,IJK,KJI
      my_real, DIMENSION(:), ALLOCATABLE :: TEMP_MAX,TEMP_MIN,TEMP_AVG
      INTEGER :: INDEX_MIN_MAX,INDEX_MEAN,TOTAL_NUMBER_NOD,DIM_FOR_MEAN
      my_real, DIMENSION(:), ALLOCATABLE :: TEMP_NODE
      REAL(kind=8), DIMENSION(:), ALLOCATABLE :: SBUFF_MIN,SBUFF_MAX,SBUFF_MEAN
      REAL(kind=8), DIMENSION(:), ALLOCATABLE :: RBUFF_MIN,RBUFF_MAX,RBUFF_MEAN
c----------------------------------------------------------
c     SENSOR_TEMP%VAR(1)   = TEMP_MAX
c     SENSOR_TEMP%VAR(2)   = TEMP_MIN 
c     SENSOR_TEMP%VAR(3)   = TEMP_AVG 
c=======================================================================
        ! -----------------
        ! local arrays for min/max/mean
        ALLOCATE( TEMP_MAX(COMM_SENS17%NUM_SENS) )
        ALLOCATE( TEMP_MIN(COMM_SENS17%NUM_SENS) )
        ALLOCATE( TEMP_AVG(COMM_SENS17%NUM_SENS) )

        TEMP_MAX(1:COMM_SENS17%NUM_SENS) = ZERO
        TEMP_MIN(1:COMM_SENS17%NUM_SENS) = ZERO
        TEMP_AVG(1:COMM_SENS17%NUM_SENS) = ZERO
        
        !   --------------------------
        !   loop over the sensor temp
        DO IJK=1,COMM_SENS17%NUM_SENS
          ISENS = COMM_SENS17%ID_SENS(IJK)     ! id of sensor temp
          IF (SENSOR(ISENS)%STATUS == 3) CYCLE   ! already activated
c
          IF (TT == ZERO) SENSOR(ISENS)%VAR(2) = EP20   ! initialize Temp min
          IGN    = SENSOR(ISENS)%IPARAM(1)
c      
          NNOD   = IGRNOD(IGN)%NENTITY
          TEMP_MAX(IJK) = SENSOR(ISENS)%VAR(1)
          TEMP_MIN(IJK) = SENSOR(ISENS)%VAR(2)
          TEMP_AVG(IJK) = ZERO
          ALLOCATE( TEMP_NODE(NNOD) )
          INDEX_SUM6FLOAT = 0
          !   -----------------
          !   loop over the local nodes
          !   the computation is done only for main nodes (weight = 1)
          DO I=1,NNOD
            INOD = IGRNOD(IGN)%ENTITY(I)
            IF (WEIGHT(INOD)==1) THEN
                TEMP_MAX(IJK) = MAX(TEMP_MAX(IJK), TEMP(INOD))
                TEMP_MIN(IJK) = MIN(TEMP_MIN(IJK), TEMP(INOD))
                TEMP_AVG(IJK) = TEMP_AVG(IJK) + TEMP(INOD)
                INDEX_SUM6FLOAT = INDEX_SUM6FLOAT + 1
                TEMP_NODE(INDEX_SUM6FLOAT) = TEMP(INOD)
            ENDIF
          ENDDO
          !   -----------------
          !   for mean temp sensor : need to use sum_6_float to guarantee the parith/on
           IF (IPARIT > 0) THEN
             CALL SUM_6_FLOAT(1,INDEX_SUM6FLOAT,TEMP_NODE,SENSOR_STRUCT(ISENS)%FBSAV6_SENS(1,1,1),1)
           ELSE
             SENSOR_STRUCT(ISENS)%FBSAV6_SENS(1,1,1) = TEMP_AVG(IJK)
           ENDIF
          !   -----------------
          DEALLOCATE( TEMP_NODE )    
        ENDDO
        !   --------------------------

        !   --------------------------
        !   communication for min/max sensor
        IF (COMM_SENS17%BUFFER_SIZE_MIN_MAX > 0) THEN
            ALLOCATE( SBUFF_MIN(COMM_SENS17%BUFFER_SIZE_MIN_MAX) )
            ALLOCATE( RBUFF_MIN(COMM_SENS17%BUFFER_SIZE_MIN_MAX) )
            ALLOCATE( SBUFF_MAX(COMM_SENS17%BUFFER_SIZE_MIN_MAX) )
            ALLOCATE( RBUFF_MAX(COMM_SENS17%BUFFER_SIZE_MIN_MAX) )
            INDEX_MIN_MAX = 0
            !   --------------------
            !   initialize the buffer for mpi comm
            DO IJK=1,COMM_SENS17%NUM_SENS
                ISENS = COMM_SENS17%ID_SENS(IJK)
                INDEX_MIN_MAX = INDEX_MIN_MAX+ 1
                SBUFF_MAX(INDEX_MIN_MAX) = TEMP_MAX(IJK)
                SBUFF_MIN(INDEX_MIN_MAX) =-TEMP_MIN(IJK)
            ENDDO
            !   --------------------
            ! reduction with mpi comm
            IF (NSPMD > 1) THEN
                CALL SPMD_ALLREDUCE_DB(SBUFF_MAX,RBUFF_MAX,COMM_SENS17%BUFFER_SIZE_MIN_MAX,"MAX" )
                CALL SPMD_ALLREDUCE_DB(SBUFF_MIN,RBUFF_MIN,COMM_SENS17%BUFFER_SIZE_MIN_MAX,"MIN" )
            ELSE
                RBUFF_MAX(:) = SBUFF_MAX(:)
                RBUFF_MIN(:) = SBUFF_MIN(:)
            ENDIF
            !   --------------------

            !   --------------------
            !   save the data in SENSOR structure
            INDEX_MIN_MAX = 0
            DO IJK=1,COMM_SENS17%NUM_SENS
              ISENS = COMM_SENS17%ID_SENS(IJK)
              INDEX_MIN_MAX = INDEX_MIN_MAX + 1
              SENSOR(ISENS)%VAR(1) = RBUFF_MAX(INDEX_MIN_MAX) ! max temp
              SENSOR(ISENS)%VAR(2) =-RBUFF_MIN(INDEX_MIN_MAX) ! min temp 
            ENDDO
            !   --------------------
            DEALLOCATE( SBUFF_MAX )
            DEALLOCATE( RBUFF_MAX )
            DEALLOCATE( SBUFF_MIN )
            DEALLOCATE( RBUFF_MIN )
        ENDIF
        !   --------------------------
        !   mpi communication for mean temperature
        IF (COMM_SENS17%BUFFER_SIZE_MEAN > 0) THEN
          ALLOCATE( SBUFF_MEAN(COMM_SENS17%BUFFER_SIZE_MEAN) )
          ALLOCATE( RBUFF_MEAN(COMM_SENS17%BUFFER_SIZE_MEAN) )
          !   parith/on : 6 values per sensor / parith/off : only 1 value
          IF (IPARIT > 0) THEN
              DIM_FOR_MEAN = 6
          ELSE
              DIM_FOR_MEAN = 1
          ENDIF
          !   ----------------
          !   initialiaze the buffer
          INDEX_MEAN = 0
          DO IJK=1,COMM_SENS17%NUM_SENS
              ISENS = COMM_SENS17%ID_SENS(IJK)
              DO KJI=1,DIM_FOR_MEAN
                  INDEX_MEAN = INDEX_MEAN + 1
                  SBUFF_MEAN(INDEX_MEAN) = SENSOR_STRUCT(ISENS)%FBSAV6_SENS(1,KJI,1)
              ENDDO
          ENDDO
          !   ----------------
          ! reduction with mpi comm
          IF (NSPMD > 1) THEN
              CALL SPMD_ALLREDUCE_DB( SBUFF_MEAN,RBUFF_MEAN,COMM_SENS17%BUFFER_SIZE_MEAN,"SUM" )
          ELSE
              RBUFF_MEAN(:) = SBUFF_MEAN(:)
          ENDIF
          !   ----------------
          !   save the Rbuffer in %FBSAV6_SENS
          INDEX_MEAN = 0
          DO IJK=1,COMM_SENS17%NUM_SENS
              ISENS = COMM_SENS17%ID_SENS(IJK)
              DO KJI=1,DIM_FOR_MEAN
                  INDEX_MEAN = INDEX_MEAN + 1
                  SENSOR_STRUCT(ISENS)%FBSAV6_SENS(1,KJI,1) = RBUFF_MEAN(INDEX_MEAN)
              ENDDO
          ENDDO
          !   ----------------

          !   ----------------
          !   computation of mean : Temp_mean = sum(FBSAV6_SENS[i],i=1,6) for parith/on
          DO IJK=1,COMM_SENS17%NUM_SENS
            ISENS = COMM_SENS17%ID_SENS(IJK)
            IF (IPARIT > 0) THEN
                DO KJI=2,DIM_FOR_MEAN
                    SENSOR_STRUCT(ISENS)%FBSAV6_SENS(1,1,1) = SENSOR_STRUCT(ISENS)%FBSAV6_SENS(1,1,1) +
     .                  SENSOR_STRUCT(ISENS)%FBSAV6_SENS(1,KJI,1)
                ENDDO
            ENDIF
            TOTAL_NUMBER_NOD = SENSOR(ISENS)%IPARAM(2)
            SENSOR(ISENS)%VAR(3) = SENSOR_STRUCT(ISENS)%FBSAV6_SENS(1,1,1) / TOTAL_NUMBER_NOD
            DO KJI=1,DIM_FOR_MEAN
                SENSOR_STRUCT(ISENS)%FBSAV6_SENS(1,KJI,1) = ZERO
            ENDDO
          ENDDO            
          !   ----------------
          DEALLOCATE( SBUFF_MEAN )
          DEALLOCATE( RBUFF_MEAN )
        ENDIF
        !   --------------------------

        DEALLOCATE( TEMP_MAX )
        DEALLOCATE( TEMP_MIN )
        DEALLOCATE( TEMP_AVG )
c----------------------------------------------------------
      RETURN
      END SUBROUTINE SENSOR_TEMP0
