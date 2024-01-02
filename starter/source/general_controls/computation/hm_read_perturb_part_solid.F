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
Chd|====================================================================
Chd|  HM_READ_PERTURB_PART_SOLID    source/general_controls/computation/hm_read_perturb_part_solid.F
Chd|-- called by -----------
Chd|        HM_READ_PERTURB               source/general_controls/computation/hm_read_perturb.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        PLOT_DISTRIB                  source/general_controls/computation/plot_distrib.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PERTURB_PART_SOLID(
     1      IPART   ,RNOISE   ,IGRPART ,IPM     ,IPARTS   ,
     2      PERTURB ,LSUBMODEL,UNITAB  ,IDPERTURB,INDEX   ,
     3      INDEX_ITYP,NPART_SOLID     ,OFFS  ,QP_IPERTURB,
     4      QP_RPERTURB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      my_real
     .   RNOISE(NPERTURB,NUMELC+NUMELTG+NUMELS+NUMSPH),
     .   QP_RPERTURB(NPERTURB,4)
      INTEGER IPART(LIPART1,*),
     .   IPM(NPROPMI,*),OFFS,
     .   IPARTS(*),PERTURB(NPERTURB),
     .   IDPERTURB(NPERTURB),INDEX(NUMELC+NUMELTG+NUMELS+NUMSPH),
     .   INDEX_ITYP(NUMELC+NUMELTG+NUMELS+NUMSPH),NPART_SOLID,
     .   QP_IPERTURB(NPERTURB,6)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRPART)  :: IGRPART
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,NUMA,I_METHOD,MAX_PART,
     .   CPT_PART,NB_RANDOM,I_SEED,DISTRIB(50),
     .   II,NB_INTERV,N,IOK,SEED,SEED_RANDOM,
     .   ITYP,L,I_PERTURB_VAR,IGRPRTS,SIZEY
      CHARACTER TITR*nchartitle,KEY*ncharkey,MES*40
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TAB_PART
      INTEGER, DIMENSION(:),   ALLOCATABLE :: A_SEED
      INTEGER, DIMENSION(1:8) :: DT_SEED
      my_real
     . MEAN,SD,MEAN_INPUT,SD_INPUT,MAX_DISTRIB,TEMP,MIN_VALUE,
     . MAX_VALUE,INTERV,VALUE,MAX_VALUE1,MINVAL,MAXVAL,BID
      my_real, DIMENSION(:), ALLOCATABLE :: ARRAY
      CHARACTER*100 CHAR(100)
      CHARACTER*100 CHAR1(100)
      CHARACTER*100 CHAR2
      CHARACTER MESS*40,CHVAR*(2*ncharfield)
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      DATA MESS/'PERTURBATION DEFINITION            '/
C=======================================================================
C-----------------------------------------------
C     B e g i n n i n g   o f   s o u r c e
C-----------------------------------------------
      ! Initialization and allocation of tables
      MAX_PART = 0
      ITYP     = 0
      BID      = 0
      IS_AVAILABLE = .FALSE.
      CALL HM_OPTION_START('/PERTURB/PART/SOLID')
      !----------------------------------------------------------------------
      ! 1st Loop over /PERTURB/PART for computing table dimension
      !----------------------------------------------------------------------
      DO I=1+OFFS,NPART_SOLID+OFFS
C      
        ! Reading the option
        TITR = ''   
        CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                          OPTION_ID   = IDPERTURB(I), 
     .                          OPTION_TITR = TITR)         
C
        ! Checking perturbation type
        ITYP = 3     
c
        I_PERTURB_VAR = 0
        CPT_PART = 0
        IGRPRTS  = 0
        IOK      = 0
C
        ! Reading the number of the Group of Part + the perturbed variable
        CALL HM_GET_INTV('grpart_ID'    ,IGRPRTS ,IS_AVAILABLE,LSUBMODEL) 
        CALL HM_GET_STRING('chvar',CHVAR   ,2*ncharfield,IS_AVAILABLE)
        IF (CHVAR(1:4) == 'dens' .OR. CHVAR(1:4) == 'DENS') I_PERTURB_VAR = 1
c
        ! Checking the perturbed variable
        IF (I_PERTURB_VAR /= 1) CALL ANCMSG(MSGID=1194,
     .      MSGTYPE=MSGERROR,
     .      ANMODE=ANINFO,
     .      I1=IDPERTURB(I),
     .      C1=TITR,
     .      C2=CHVAR)
c
        ! Checking solid part group
        IF (IGRPRTS /= 0)THEN
          DO N=1,NGRPART
            IF (IGRPART(N)%ID == IGRPRTS) THEN
              IGRPRTS = N
              IOK  = 1
              ITYP = 3
              EXIT
            END IF
          END DO
        ENDIF
c
        ! Saving the perturbation type
        PERTURB(I) = ITYP
c
        ! Error messages or counting
        IF (IOK == 0) THEN
          CALL ANCMSG(MSGID=1137,
     .      MSGTYPE=MSGERROR,
     .      ANMODE=ANINFO,
     .      I1=IDPERTURB(I),
     .      C1=TITR,
     .      I2=IGRPRTS,
     .      C2='GROUP OF PART')
        ELSEIF (IOK == 1)THEN
          CPT_PART = IGRPART(IGRPRTS)%NENTITY
        ENDIF
        MAX_PART = MAX (MAX_PART,CPT_PART)
      ENDDO
      ! Allocation of tables
      ALLOCATE(TAB_PART(NPERTURB,MAX_PART))
c
      !----------------------------------------------------------------------
      ! 2nd Loop over /PERTURB/PART for reading and computing perturbation
      !----------------------------------------------------------------------
      CALL HM_OPTION_START('/PERTURB/PART/SOLID')
      DO I=1+OFFS,NPART_SOLID+OFFS
C
        ! Resetting index tables
        INDEX(1:(NUMELC+NUMELTG+NUMELS+NUMSPH))    = 0
        INDEX_ITYP(1:NUMELC+NUMELTG+NUMELS+NUMSPH) = 0
C      
        ! Reading the option
        TITR = ''   
        CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                          OPTION_ID      = IDPERTURB(I), 
     .                          OPTION_TITR    = TITR)         
C
        ! Perturbation type
        ITYP = 3   
C
        ! Reading the card
        CALL HM_GET_FLOATV('F_Mean'    ,MEAN    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('Deviation' ,SD      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('Min_cut'   ,MINVAL  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('Max_cut'   ,MAXVAL  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_INTV('Seed'        ,SEED    ,IS_AVAILABLE, LSUBMODEL)  
        CALL HM_GET_INTV('Idistri'     ,I_METHOD,IS_AVAILABLE, LSUBMODEL)
c
        ! Default value
        IF(I_METHOD == 0) I_METHOD = 2
        IF(MINVAL == ZERO .AND. MAXVAL == ZERO) THEN
          IF(I_METHOD == 1) THEN
          ELSEIF(I_METHOD == 2)THEN
            MINVAL = -EP30
            MAXVAL = EP30
          ENDIF
        ENDIF
        SD_INPUT = SD
        MEAN_INPUT = MEAN
c
        ! QAPRINT table
        QP_IPERTURB(I,1) = IDPERTURB(I)
        QP_IPERTURB(I,2) = ITYP
        QP_IPERTURB(I,3) = SEED
        QP_IPERTURB(I,4) = I_METHOD
        QP_RPERTURB(I,1) = MEAN
        QP_RPERTURB(I,2) = SD
        QP_RPERTURB(I,3) = MINVAL
        QP_RPERTURB(I,4) = MAXVAL
c
        ! Initialization flag and counter
        CPT_PART = 0
        IGRPRTS  = 0
        IOK      = 0
C
        ! Reading the number of the Group of Part + the perturbed variable
        CALL HM_GET_INTV('grpart_ID' ,IGRPRTS ,IS_AVAILABLE,LSUBMODEL)
        QP_IPERTURB(I,5) = IGRPRTS
        CALL HM_GET_STRING('chvar',CHVAR   ,2*ncharfield,IS_AVAILABLE)
        IF (CHVAR(1:4) == 'dens' .OR. CHVAR(1:4) == 'DENS') QP_IPERTURB(I,6) = 1
c
        ! Checking solid part group
        IF (IGRPRTS /= 0)THEN
          DO N=1,NGRPART
            IF (IGRPART(N)%ID == IGRPRTS) THEN
              IGRPRTS = N
              IOK  = 1
              ITYP = 3
              EXIT
            END IF
          END DO
        ENDIF
c
        ! Tag the parts
        IF (IOK == 1) THEN
          CPT_PART = 0
          DO J=1,IGRPART(IGRPRTS)%NENTITY
            CPT_PART = CPT_PART + 1
            NUMA = IGRPART(IGRPRTS)%ENTITY(J)
            TAB_PART(I,CPT_PART) = NUMA
          END DO              
        ENDIF
c
        ! Printing out the information
        IF(I_METHOD == 2) THEN
          WRITE (IOUT,6000)
     .      IDPERTURB(I),'GAUSSIAN',MEAN_INPUT,SD_INPUT,SEED
          WRITE (IOUT,'(10I10)') IPART(4,TAB_PART(I,1:CPT_PART))
          WRITE(IOUT,*) ' '
          WRITE(IOUT,*) ' '
        ELSEIF(I_METHOD == 1) THEN
          WRITE (IOUT,6100)
     .      IDPERTURB(I),'RANDOM',SEED
          WRITE (IOUT,'(10I10)') IPART(4,TAB_PART(I,1:CPT_PART))
          WRITE(IOUT,*) ' '
          WRITE(IOUT,*) ' '
        ENDIF
c        
        ! Filling the index table
        NB_RANDOM = 0
        DO II=1,NUMELS
          DO K=1,CPT_PART
            IF(IPARTS(II) == TAB_PART(I,K)) THEN
              NB_RANDOM = NB_RANDOM + 1
              INDEX(NB_RANDOM) = II
              INDEX_ITYP(NB_RANDOM) = 1
            ENDIF
          ENDDO 
        ENDDO   
C
        ! Set up random seed
        IF( SEED == 0 )THEN
          CALL RANDOM_SEED(SIZE=I_SEED)
          ALLOCATE(A_SEED(1:I_SEED))
          CALL RANDOM_SEED(GET=A_SEED)
          CALL DATE_AND_TIME(values=DT_SEED)
          A_SEED=DT_SEED(8)*DT_SEED(7)*DT_SEED(6)
          SEED=DT_SEED(8)*DT_SEED(7)*DT_SEED(6)
          CALL RANDOM_SEED(PUT=A_SEED)
          SEED_RANDOM = 1
          DEALLOCATE(A_SEED)
        ELSE
          CALL RANDOM_SEED(SIZE=I_SEED)
          ALLOCATE(A_SEED(1:I_SEED))
          A_SEED=SEED
          CALL RANDOM_SEED(PUT=A_SEED)
          SEED_RANDOM = 0
          DEALLOCATE(A_SEED) 
        ENDIF
C
        ! Build uniform distribution
        CHAR=''
        CHAR1=''
        CHAR2=''
        DISTRIB(1:50) = 0
        ALLOCATE(ARRAY(NB_RANDOM+2))
        CALL RANDOM_NUMBER(ARRAY)
C
        ! Build normal distribution
        MAX_VALUE = -EP30
        MIN_VALUE = EP30
        IF ( I_METHOD == 2) THEN
          DO II = 1, NB_RANDOM+1, 2
            TEMP = SD * SQRT(-2.0*LOG(ARRAY(II))) * COS(2*pi*array(II+1)) +
     .             MEAN
            ARRAY(II+1) = 
     .        SD * SQRT(-2.0*LOG(ARRAY(II))) * SIN(2*pi*ARRAY(II+1)) + MEAN
            ARRAY(II) = TEMP
          END DO
          DO II = 1, NB_RANDOM
            ARRAY(II) = MAX(MIN(MAXVAL,ARRAY(II)),MINVAL)
            MAX_VALUE = MAX(ARRAY(II),MAX_VALUE)
            MIN_VALUE = MIN(ARRAY(II),MIN_VALUE)
          END DO
        ELSEIF(I_METHOD == 1)THEN
          DO II = 1, NB_RANDOM
            ARRAY(II) = ARRAY(II)*(MAXVAL-MINVAL)+MINVAL
            MAX_VALUE = MAX(ARRAY(II),MAX_VALUE)
            MIN_VALUE = MIN(ARRAY(II),MIN_VALUE)
          END DO
        ENDIF
c
        ! Filling RNOISE table
        DO II = 1, NB_RANDOM
          IF (INDEX_ITYP(II) == 1) THEN
            RNOISE(I,INDEX(II)+NUMELC+NUMELTG) = ARRAY(II)
          ENDIF
        ENDDO
c
        ! Check mean and standard deviation
        MEAN = SUM(ARRAY)/NB_RANDOM
        SD = SQRT(SUM((ARRAY - MEAN)**2)/NB_RANDOM)
c
        ! Plot the normal distribution
        IF(I_METHOD == 2) THEN
          MAX_DISTRIB = ONE /(SD*SQRT(TWO * pi))
        ELSEIF(I_METHOD == 1) THEN
          MAX_DISTRIB = ONE /(MAX_VALUE-MIN_VALUE)
        ENDIF
        WRITE (IOUT,4500)
        WRITE(IOUT,*) ' '
        NB_INTERV = 50
        SIZEY     = 20
        IF (MINVAL /= -EP30 .AND. MAXVAL /= EP30)THEN
          MIN_VALUE = MINVAL
          MAX_VALUE = MAXVAL
        ENDIF
        CALL PLOT_DISTRIB( ARRAY,NB_RANDOM, NB_INTERV,SIZEY,MIN_VALUE,
     .  MAX_VALUE,MAX_DISTRIB,'#')
        IF(I_METHOD == 2) THEN 
          WRITE (IOUT,2000) MEAN,SD
        ELSEIF(I_METHOD == 1) THEN 
          WRITE (IOUT,2050) MEAN
        ENDIF
        IF(SEED_RANDOM == 1) WRITE (IOUT,2100) SEED
        WRITE(IOUT,*) ' '
        WRITE(IOUT,*) ' '
        IF (ALLOCATED(ARRAY)) DEALLOCATE(ARRAY)        
      ENDDO
      DEALLOCATE(TAB_PART)      
C-------------------------------------------------------------
 6000 FORMAT(/' PERTURBATION  ID',I10/
     +        ' ---------------'/
     + '        TYPE . . . . . . . . . . . . . . .',A/
     + '        INPUT MEAN VALUE . . . . . . . . .',1PG20.13/
     + '        INPUT STANDARD DEVIATION . . . . .',1PG20.13/
     + '        INPUT SEED VALUE . . . . . . . . .',I10/
     + '        SOLID DENSITIES, PARTS:')
 6100 FORMAT(/' PERTURBATION  ID',I10/
     +        ' ---------------'/
     + '        TYPE . . . . . . . . . . . . . . .',A/
     + '        INPUT SEED VALUE . . . . . . . . .',I10/
     + '        SOLID DENSITIES, PARTS:')
C-------------------------------------------------------------
 2000 FORMAT(/
     + '        GENERATED MEAN VALUE . . . . . . .',1PG20.13/
     + '        GENERATED STANDARD DEVIATION . . .',1PG20.13)
 2050 FORMAT(/
     + '        GENERATED MEAN VALUE . . . . . . .',1PG20.13)
 2100 FORMAT(/
     + '        GENERATED SEED VALUE . . . . . . .',I10/)
C-------------------------------------------------------------
 4500 FORMAT(/
     + '        DISTRIBUTION OF SCALE FACTORS APPLIED TO DENSITIES OF SOLIDS')
C------------------------------ 
C------------------------------ 
      END
