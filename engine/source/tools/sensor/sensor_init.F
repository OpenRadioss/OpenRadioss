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
Chd|  SENSOR_INIT                   source/tools/sensor/sensor_init.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        SENSOR_LOGICAL                source/tools/sensor/sensor_logical.F
Chd|        SENSOR_TIME                   source/tools/sensor/sensor_time.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
        SUBROUTINE SENSOR_INIT( SUBSET,IPARG,NGROUC,
     .                          IPARTC,IPARTTG,IPARTS,IPARTQ,IPARTT,
     .                          IPARTP,IPARTR,SENSOR_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE GROUPDEF_MOD
        USE SENSOR_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE (SUBSET_) , DIMENSION(NSUBS), INTENT(IN) :: SUBSET
        INTEGER, INTENT(IN) :: NGROUC
        INTEGER, DIMENSION(*), TARGET, INTENT(IN) :: IPARTC,IPARTTG,IPARTS,IPARTQ
        INTEGER, DIMENSION(*), TARGET, INTENT(IN) :: IPARTT,IPARTP,IPARTR
        INTEGER, DIMENSION(NPARG,*), INTENT(IN) :: IPARG
        TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR), INTENT(IN) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        LOGICAL :: BOOL
        INTEGER :: I,K,J,IC,IG,NG,NP,IJK,KJI
        INTEGER :: TYP,IPART,IPART_2,ISUBS,IGRN,NBR_GROUP
        INTEGER :: NFT,NF1,ITY,NEL
        INTEGER, DIMENSION(:), POINTER :: IPART_POINTER
        INTEGER :: NUMBER_PART_GROUP,NUMBER_PART_GROUP_2
        INTEGER, DIMENSION(MVSIZ) :: INDX_PART,INDX_PART_2
        INTEGER :: NUMBER_SENSOR_PER_GROUP,NUMBER_SENSOR,TOT_NUMBER_PART_GROUP
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: CHECK_BOOL
        LOGICAL, DIMENSION(:), ALLOCATABLE :: BOOL_SENSOR

        INTEGER :: ISURF
        INTEGER :: SURF_DIST_SENSOR,ENERGY_SENSOR,SENSOR_TEMP
        INTEGER, DIMENSION(NSENSOR) :: INDX_SURF_DIST_SENSOR,INDX_ENERGY,INDX_TEMP
        INTEGER :: SIZE_COMM17_MIN_MAX,SIZE_COMM17_MEAN

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

        COMM_SENS14%BOOL = .FALSE.
        COMM_SENS16%BOOL = .FALSE.
        COMM_SENS17%BOOL = .FALSE.

        COMM_SENS14%BUFFER_SIZE_MIN_MAX = 0
        COMM_SENS14%BUFFER_SIZE_MEAN = 0

        COMM_SENS16%BUFFER_SIZE_MIN_MAX = 0
        COMM_SENS16%BUFFER_SIZE_MEAN = 0

        COMM_SENS17%BUFFER_SIZE_MIN_MAX = 0
        COMM_SENS17%BUFFER_SIZE_MEAN = 0
c
        SURF_DIST_SENSOR = 0
        ENERGY_SENSOR    = 0
        SENSOR_TEMP      = 0
        INDX_SURF_DIST_SENSOR(1:NSENSOR) = 0
        INDX_ENERGY(1:NSENSOR) = 0
        INDX_TEMP(1:NSENSOR)   = 0
        !   -----------------------------------------
        !   count the number of sensors by type
        DO K=1,NSENSOR
            TYP = SENSOR_TAB(K)%TYPE
            !   -------------------- 
            IF (TYP==14) THEN
                !   sensor energy
                ENERGY_SENSOR = ENERGY_SENSOR + 1
                INDX_ENERGY(ENERGY_SENSOR) = K
            ELSE IF (TYP==16) THEN
                !   distance to surface
                ISURF = SENSOR_TAB(K)%IPARAM(2)         
                IF (ISURF/=0) THEN
                    SURF_DIST_SENSOR = SURF_DIST_SENSOR + 1
                    INDX_SURF_DIST_SENSOR(SURF_DIST_SENSOR) = K
                ENDIF
            ELSE IF (TYP==17) THEN
                !   sensor temperature
                IGRN = SENSOR_TAB(K)%IPARAM(1)
                IF (IGRN > 0) THEN
                    SENSOR_TEMP = SENSOR_TEMP + 1
                    INDX_TEMP(SENSOR_TEMP) = K
                ENDIF
            ENDIF
            !   --------------------        
        ENDDO
        !   -----------------------------------------
        !   activation of the mpi communication
        IF (ENERGY_SENSOR>0) COMM_SENS14%BOOL = .TRUE.
        COMM_SENS14%NUM_SENS = ENERGY_SENSOR   

        IF (SURF_DIST_SENSOR>0) COMM_SENS16%BOOL = .TRUE.
        COMM_SENS16%NUM_SENS = SURF_DIST_SENSOR 

        IF (SENSOR_TEMP>0) COMM_SENS17%BOOL = .TRUE.
        COMM_SENS17%NUM_SENS = SENSOR_TEMP

        !   ---------------------
        !   ENERGY SENSOR
        IF (IPARIT>0) THEN
           ALLOCATE( COMM_SENS14%ID_SENS(ENERGY_SENSOR) )
           IF(ENERGY_SENSOR>0) COMM_SENS14%ID_SENS(1:ENERGY_SENSOR) = INDX_ENERGY(1:ENERGY_SENSOR)
           ALLOCATE( SENS_GROUP(NGROUP) )
           SENS_GROUP(1:NGROUP)%NUM_PART = 0
           ALLOCATE( SENSOR_STRUCT(NSENSOR) )
           SENSOR_STRUCT(:)%TYP = 0     !   type of part : subset (=2) or not (=1)
           SENSOR_STRUCT(:)%PART = 0    !   part id
           SENSOR_STRUCT(:)%NB_SUB = 0  !   number of subset

           IF(ENERGY_SENSOR>0) THEN
               !   -----------------------------------------
               !   get the /PART id for each sensor
               DO I=1,ENERGY_SENSOR
                      K = INDX_ENERGY(I)
                      TYP = SENSOR_TAB(K)%TYPE
                      !   --------------------    
                      !   sensor energy
                      IPART  = SENSOR_TAB(K)%IPARAM(1)
                      ISUBS  = SENSOR_TAB(K)%IPARAM(2)
                      IF(IPART>0) THEN
                      !   only one part per sensor
                          SENSOR_STRUCT(K)%TYP = 1
                          SENSOR_STRUCT(K)%PART = IPART !   part id
                      ELSEIF(ISUBS>0) THEN
                      !   several subparts per sensor
                          SENSOR_STRUCT(K)%TYP = 2
                          NP = SUBSET(ISUBS)%NTPART   !   number of part 
                          SENSOR_STRUCT(K)%NB_SUB = NP
                          ALLOCATE( SENSOR_STRUCT(K)%SUB(NP) )
                          DO J=1,NP
                            SENSOR_STRUCT(K)%SUB(J)%PART = SUBSET(ISUBS)%TPART(J) ! part id
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

                      SENS_GROUP(NG)%NUM_PART = -NUMBER_PART_GROUP       !   number of part in one groupe
                    !   count the number of sensor for a element group 
                      ALLOCATE( BOOL_SENSOR(ENERGY_SENSOR) )
                      BOOL_SENSOR(1:ENERGY_SENSOR) = .TRUE.

                      NUMBER_SENSOR_PER_GROUP = 0
                      DO IJK=1,NUMBER_PART_GROUP
                        IPART = INDX_PART(IJK)
                        DO I=1,ENERGY_SENSOR
                          K = INDX_ENERGY(I)
                          IF(SENSOR_STRUCT(K)%TYP == 1) THEN
                                IF(SENSOR_STRUCT(K)%PART == IPART) THEN
                                    NUMBER_SENSOR_PER_GROUP = NUMBER_SENSOR_PER_GROUP + 1
                                ENDIF
                          ELSEIF(SENSOR_STRUCT(K)%TYP == 2) THEN
                              DO J=1,SENSOR_STRUCT(K)%NB_SUB
                                IF(BOOL_SENSOR(I).AND.SENSOR_STRUCT(K)%SUB(J)%PART == IPART) THEN
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

                      ALLOCATE( SENS_GROUP(NG)%PART(TOT_NUMBER_PART_GROUP,3) )
                      SENS_GROUP(NG)%PART(1:TOT_NUMBER_PART_GROUP,1:3) = 0

                      KJI = 0
                      DO IJK=1,NUMBER_PART_GROUP
                        DO I=1,NUMBER_SENSOR_PER_GROUP
                        KJI = KJI + 1
                        SENS_GROUP(NG)%PART(KJI,1) = -INDX_PART(IJK)!   part id
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
                          IF(SENSOR_STRUCT(K)%TYP == 1) THEN
                            IF(CHECK_BOOL(I,IJK)<0) THEN !   check if already done (loop over the part of an element group!!!)
                                IF(SENSOR_STRUCT(K)%PART == IPART) THEN
                                    KJI = KJI + 1
                                    !   number of group per part for a given sensor
                                    SENSOR_STRUCT(K)%NUM_GROUP_PART = SENSOR_STRUCT(K)%NUM_GROUP_PART + 1
                                    SENS_GROUP(NG)%PART(KJI,2) = 1    !   type of energy sensor
                                    SENS_GROUP(NG)%PART(KJI,3) = K   !   index of the current sensor

                                    SENS_GROUP(NG)%PART(KJI,1) = ABS(SENS_GROUP(NG)%PART(KJI,1))
                                    CHECK_BOOL(I,IJK) = ABS(CHECK_BOOL(I,IJK))
                                    BOOL = .TRUE.
                                ENDIF
                            ENDIF
                          ELSEIF(SENSOR_STRUCT(K)%TYP == 2) THEN
                            IF(CHECK_BOOL(I,IJK)<0) THEN !   check if already done (loop over the part of an element group!!!)
                              DO J=1,SENSOR_STRUCT(K)%NB_SUB
                                IF(SENSOR_STRUCT(K)%SUB(J)%PART == IPART) THEN
                                    KJI = KJI + 1
                                    !   number of group per subpart
                                    SENSOR_STRUCT(K)%NUM_GROUP_PART = SENSOR_STRUCT(K)%NUM_GROUP_PART + 1
                                    SENS_GROUP(NG)%PART(KJI,2) = 2    !   type of energy sensor
                                    SENS_GROUP(NG)%PART(KJI,3) = K   !   index of the current sensor
                                    SENS_GROUP(NG)%PART(KJI,1) = ABS(SENS_GROUP(NG)%PART(KJI,1))
                                    CHECK_BOOL(I,IJK) = ABS(CHECK_BOOL(I,IJK))
                                    BOOL = .TRUE.
                                ENDIF                                     
                              ENDDO
                            ENDIF
                          ENDIF
                        ENDDO
                      ENDDO

                      IF(BOOL) SENS_GROUP(NG)%NUM_PART = KJI       !   number of part in one groupe * number of sensor        
                      DEALLOCATE( CHECK_BOOL )             
                    ENDIF
               ENDDO
               !   -----------------------------------------
               !   allocation of FBSAV6 double precision array (parith/on array)
               DO I=1,ENERGY_SENSOR
                    K = INDX_ENERGY(I)
                    ALLOCATE( SENSOR_STRUCT(K)%FBSAV6_SENS(2,6,NTHREAD) )
                    SENSOR_STRUCT(K)%FBSAV6_SENS(1:2,1:6,1:NTHREAD) = ZERO
               ENDDO    
               !   -----------------------------------------           
           ENDIF
        ELSE
          ALLOCATE( SENS_GROUP(0) )
          ALLOCATE( SENSOR_STRUCT(0) )
          ALLOCATE( COMM_SENS14%ID_SENS(0) )
        ENDIF

        !   ---------------------
        !   DISTANCE TO SURFACE SENSOR
        ALLOCATE( COMM_SENS16%ID_SENS(SURF_DIST_SENSOR) )
        IF(SURF_DIST_SENSOR>0) THEN
            COMM_SENS16%ID_SENS(1:SURF_DIST_SENSOR) = INDX_SURF_DIST_SENSOR(1:SURF_DIST_SENSOR)
        ENDIF
        !   ---------------------
        !   SENSOR TEMPERATURE
        SIZE_COMM17_MIN_MAX = 0
        SIZE_COMM17_MEAN = 0
        ALLOCATE( COMM_SENS17%ID_SENS(SENSOR_TEMP) )
        IF (SENSOR_TEMP > 0) THEN
            DO I=1,SENSOR_TEMP
                K = INDX_TEMP(I)
                COMM_SENS17%ID_SENS(I) = K
                !   Temp max and min : 1 data per sensor
                SIZE_COMM17_MIN_MAX = SIZE_COMM17_MIN_MAX + 1
                !   Temp mean  : 6 data per sensor (parith/on) / 1 data per sensor (parith/off) 
                IF (IPARIT>0) THEN
                    ALLOCATE( SENSOR_STRUCT(K)%FBSAV6_SENS(1,6,1) )
                    SENSOR_STRUCT(K)%FBSAV6_SENS(1,1:6,1:1) = ZERO
                    SIZE_COMM17_MEAN = SIZE_COMM17_MEAN + 6
                ELSE
                    ALLOCATE( SENSOR_STRUCT(K)%FBSAV6_SENS(1,1,1) )
                    SENSOR_STRUCT(K)%FBSAV6_SENS(1,1,1) = ZERO
                    SIZE_COMM17_MEAN = SIZE_COMM17_MEAN + 1
                ENDIF
            ENDDO    
            COMM_SENS17%BUFFER_SIZE_MIN_MAX = SIZE_COMM17_MIN_MAX
            COMM_SENS17%BUFFER_SIZE_MEAN    = SIZE_COMM17_MEAN
        ENDIF
        !   ---------------------

C ---------------------------------------------------------------------
C Play Sensors which could be active at TT=0
c----------------------------------------------------------------------
        IF (NSENSOR > 0 ) THEN
           DO I=1,NSENSOR
               ! Time Sensor if TT=0
               TYP    = SENSOR_TAB(I)%TYPE
               IF (TYP == 0) THEN 
                 CALL SENSOR_TIME(SENSOR_TAB(I))
               ENDIF
           ENDDO
     
           ! Logical Sensors can be active at TT=0
           CALL SENSOR_LOGICAL(SENSOR_TAB)
        ELSE
           LOGICAL_SENSOR_COUNT=0
        ENDIF

        RETURN
        END SUBROUTINE SENSOR_INIT
