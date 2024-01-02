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
Chd|  HM_READ_PERTURB_FAIL          source/general_controls/computation/hm_read_perturb_fail.F
Chd|-- called by -----------
Chd|        HM_READ_PERTURB               source/general_controls/computation/hm_read_perturb.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        PLOT_DISTRIB                  source/general_controls/computation/plot_distrib.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PERTURB_FAIL(
     .      IPART    ,RNOISE   ,IPARTC ,IPARTG    ,IPARTSP  ,
     .      IGRPART  ,IPARTS   ,IPM    ,PERTURB   ,IDPERTURB,
     .      INDEX   ,INDEX_ITYP,NPART_SHELL,OFFS ,QP_IPERTURB,
     .      QP_RPERTURB,LSUBMODEL,UNITAB)
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
      INTEGER OFFS
      my_real :: RNOISE(NPERTURB,NUMELC+NUMELTG+NUMELS+NUMSPH),
     .   QP_RPERTURB(NPERTURB,4)
      INTEGER IPART(LIPART1,*),IPARTC(*),IPARTSP(*),IPARTG(*),IPARTS(*),
     .   IPM(NPROPMI,*),PERTURB(NPERTURB),
     .   IDPERTURB(NPERTURB),INDEX(NUMELC+NUMELTG+NUMELS+NUMSPH),
     .   INDEX_ITYP(NUMELC+NUMELTG+NUMELS+NUMSPH),NPART_SHELL,
     .   QP_IPERTURB(NPERTURB,6)
      TYPE (UNIT_TYPE_)    ,INTENT(IN)  :: UNITAB
      TYPE (SUBMODEL_DATA) ,INTENT(IN)  :: LSUBMODEL(*)
      TYPE (GROUP_)  ,DIMENSION(NGRPART):: IGRPART
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ICOUNT,II,J,K,N,I_METHOD,MAX_PART,OPT_ID,FAIL_ID,UNIT_ID,KLEN,
     .   CPT_PART,NB_RANDOM,I_SEED,NPERTURB_FAIL,
     .   NB_INTERV,SEED,SEED_RANDOM,IFAILMAT,IFAILTYPE,ITYP,I_PERTURB_VAR,SIZEY
      INTEGER, DIMENSION(50) :: DISTRIB
      INTEGER, DIMENSION(8)  :: DT_SEED
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TAB_PART
      INTEGER, DIMENSION(:),   ALLOCATABLE :: A_SEED
      my_real
     . MEAN,STDEV,MEAN_INPUT,SD_INPUT,MAX_DISTRIB,TEMP,MIN_VALUE,
     . MAX_VALUE,INTERV,VALUE,MAX_VALUE1,MINVAL,MAXVAL,BID
      my_real, DIMENSION(:), ALLOCATABLE :: ARRAY
      CHARACTER TITR*nchartitle
      CHARACTER*100 KEY1,KEY2
      CHARACTER PARAM*ncharkey
      CHARACTER MESS*40
      LOGICAL IS_AVAILABLE
      DATA MESS/'PERTURBATION DEFINITION            '/
C=======================================================================
      CALL HM_OPTION_COUNT('/PERTURB/FAIL',NPERTURB_FAIL)
c-----------------------------------------------------------------------
c     1st loop over /PERTURB/FAIL/BIQUAD for computing table dimension
c-----------------------------------------------------------------------
      CALL HM_OPTION_START('/PERTURB/FAIL')
      MAX_PART = 0
      DO ICOUNT = 1+OFFS,NPERTURB_FAIL+OFFS
        IFAILTYPE = 0
        CPT_PART  = 0
        ITYP      = 2
        CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                          OPTION_ID   = OPT_ID, 
     .                          UNIT_ID     = UNIT_ID   ,
     .                          KEYWORD2    = KEY1, 
     .                          KEYWORD3    = KEY2, 
     .                          OPTION_TITR = TITR) 
c
        IDPERTURB(ICOUNT) = OPT_ID
        KLEN = LEN_TRIM(KEY2)
        IF (KEY2(1:KLEN) == 'BIQUAD') THEN
          IFAILTYPE = 30
        ELSE
          CALL ANCMSG(MSGID=1192, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .                I1=OPT_ID,
     .                C1=TITR,
     .                C2=KEY2)
          CYCLE
        ENDIF
c      
        CALL HM_GET_INTV  ('fail_ID' ,FAIL_ID ,IS_AVAILABLE,LSUBMODEL)
c      
        IFAILMAT = 0
        IF (FAIL_ID > 0) THEN
          DO N=1,NUMMAT
            DO J=1,IPM(220, N)
              IF( IPM(236+J, N) == FAIL_ID)THEN
                IF (IFAILTYPE /= IPM(241+J, N)) THEN
                  CALL ANCMSG(MSGID=1193, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .                 I1=OPT_ID,
     .                 C1=TITR,
     .                 I2=FAIL_ID,
     .                 C2=KEY2)
                ENDIF
                IFAILMAT = N
                EXIT
              END IF
            END DO
            IF (IFAILMAT > 0) EXIT
          END DO
        ENDIF
c
        PERTURB(ICOUNT) = ITYP
c
        IF (IFAILMAT > 0) THEN
          DO N=1,NPART 
            IF(IPART(1,N) == IFAILMAT) THEN 
              CPT_PART = CPT_PART + 1
            ENDIF
          ENDDO
        ELSE
          CALL ANCMSG(MSGID=1137, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .          I1=OPT_ID,
     .          C1=TITR,
     .          I2=FAIL_ID,
     .          C2='FAILURE CRITERIA')
        ENDIF
        MAX_PART = MAX (MAX_PART,CPT_PART)
      ENDDO
c--------  end first loop over /PERTURB/FAIL
c      
      ALLOCATE(TAB_PART(NPERTURB,MAX_PART))
c
c-----------------------------------------------------------------------
c     2nd loop over /PERTURB/FAIL/BIQUAD for reading and computing perturbation
c-----------------------------------------------------------------------      
      CALL HM_OPTION_START('/PERTURB/FAIL')
      DO ICOUNT = 1+OFFS,NPERTURB_FAIL+OFFS
        I_PERTURB_VAR = 0
        CPT_PART = 0      
        ITYP     = 2
        CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                          OPTION_ID   = OPT_ID, 
     .                          UNIT_ID     = UNIT_ID   ,
     .                          KEYWORD2    = KEY1, 
     .                          KEYWORD3    = KEY2, 
     .                          OPTION_TITR = TITR) 
        IDPERTURB(ICOUNT) = OPT_ID
c
        KLEN = LEN_TRIM(KEY2)
        IF (KEY2(1:KLEN) == 'BIQUAD') THEN
          IFAILTYPE = 30
card1
          CALL HM_GET_FLOATV('F_Mean'    ,MEAN     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Deviation' ,STDEV    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Min_cut'   ,MINVAL   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('Max_cut'   ,MAXVAL   ,IS_AVAILABLE,LSUBMODEL,UNITAB)     
          CALL HM_GET_INTV  ('Seed'      ,SEED     ,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV  ('Idistri'   ,I_METHOD ,IS_AVAILABLE,LSUBMODEL)
card2
          CALL HM_GET_INTV  ('fail_ID'   ,FAIL_ID  ,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_STRING('parameter' ,PARAM    ,ncharkey,IS_AVAILABLE)
c--------------------------------------------
c         Input check
c--------------------------------------------
          IF (I_METHOD == 0) I_METHOD = 2
          IF (I_METHOD == 2) THEN
            IF (MINVAL == ZERO .AND. MAXVAL == ZERO) THEN
              MINVAL =-EP20
              MAXVAL = EP20
            ENDIF
          ENDIF
          SD_INPUT   = STDEV
          MEAN_INPUT = MEAN
c
          QP_IPERTURB(ICOUNT,1) = OPT_ID
          QP_IPERTURB(ICOUNT,2) = ITYP
          QP_IPERTURB(ICOUNT,3) = SEED
          QP_IPERTURB(ICOUNT,4) = I_METHOD
          QP_IPERTURB(ICOUNT,5)  = FAIL_ID
          QP_RPERTURB(ICOUNT,1) = MEAN
          QP_RPERTURB(ICOUNT,2) = STDEV
          QP_RPERTURB(ICOUNT,3) = MINVAL
          QP_RPERTURB(ICOUNT,4) = MAXVAL
c          
          IF (PARAM(1:2) == 'c3' .or. PARAM(1:2) == 'C3') THEN
            I_PERTURB_VAR = 1 
            QP_IPERTURB(ICOUNT,6) = I_PERTURB_VAR
          ELSE
            CALL ANCMSG(MSGID=1194,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                  I1=OPT_ID,
     .                  C1=TITR,
     .                  C2=PARAM)
          END IF
c
          IFAILMAT = 0
          IF (FAIL_ID > 0)THEN
            DO N=1,NUMMAT
              DO J=1,IPM(220, N)
                IF (IPM(236+J, N) == FAIL_ID) THEN
                  IFAILMAT = N
                  EXIT
                END IF
              END DO
              IF (IFAILMAT > 0) EXIT
            END DO
          ENDIF
          IF (IFAILMAT > 0) THEN
            CPT_PART = 0
            DO N=1,NPART 
              IF(IPART(1,N) == IFAILMAT) THEN 
                CPT_PART = CPT_PART + 1
                TAB_PART(ICOUNT,CPT_PART) = N
              ENDIF
            ENDDO            
          END IF
c-------------------------------
c         PRINT OUT
c-------------------------------
          IF(I_METHOD == 2) THEN
            WRITE (IOUT,4000)
     .          OPT_ID,'GAUSSIAN',MEAN_INPUT,SD_INPUT,SEED,KEY2,FAIL_ID,PARAM
            WRITE (IOUT,'(10I10)') IPART(4,TAB_PART(ICOUNT,1:CPT_PART))
            WRITE(IOUT,*) ' '
            WRITE(IOUT,*) ' '
          ELSEIF(I_METHOD == 1) THEN
            WRITE (IOUT,4100) OPT_ID,'RANDOM',SEED,KEY2,FAIL_ID,PARAM
            WRITE (IOUT,'(10I10)') IPART(4,TAB_PART(ICOUNT,1:CPT_PART))
            WRITE(IOUT,*) ' '
            WRITE(IOUT,*) ' '
          ENDIF
c-------------------------------
          NB_RANDOM = 0
          DO II=1,NUMELC
            DO K=1,CPT_PART
              IF (IPARTC(II) == TAB_PART(ICOUNT,K)) THEN
                NB_RANDOM = NB_RANDOM + 1
                INDEX(NB_RANDOM) = II
                INDEX_ITYP(NB_RANDOM) = 3
              ENDIF
            ENDDO 
          ENDDO
          DO II=1,NUMELTG
            DO K=1,CPT_PART
              IF(IPARTG(II) == TAB_PART(ICOUNT,K)) THEN
                NB_RANDOM = NB_RANDOM + 1
                INDEX(NB_RANDOM) = II
                INDEX_ITYP(NB_RANDOM) = 7
              ENDIF
            ENDDO 
          ENDDO
          DO II=1,NUMELS
            DO K=1,CPT_PART
              IF (IPARTS(II) == TAB_PART(ICOUNT,K)) THEN
                NB_RANDOM = NB_RANDOM + 1
                INDEX(NB_RANDOM) = II
                INDEX_ITYP(NB_RANDOM) = 1
              ENDIF
            ENDDO 
          ENDDO
          DO II=1,NUMSPH
            DO K=1,CPT_PART
              IF (IPARTSP(II) == TAB_PART(ICOUNT,K)) THEN
                NB_RANDOM = NB_RANDOM + 1
                INDEX(NB_RANDOM) = II
                INDEX_ITYP(NB_RANDOM) = 51
              ENDIF
            ENDDO 
          ENDDO
c-------------------------------------------------------
c         Set up random seed
c-------------------------------------------------------
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
c-------------------------------------------------------
c         build uniform distributions
c-------------------------------------------------------
          DISTRIB(1:50) = 0                                  
          ALLOCATE(ARRAY(NB_RANDOM+2))                       
          CALL RANDOM_NUMBER(ARRAY) ! Uniform distribution   
c-------------------------------------------------------
c         build normal distribution
c-------------------------------------------------------
          MAX_VALUE = -EP30                                                     
          MIN_VALUE = EP30                                                      
          IF ( I_METHOD == 2) THEN                                              
            DO II = 1, NB_RANDOM+1, 2                                           
              TEMP = STDEV * SQRT(-2.0*LOG(ARRAY(II))) * COS(2*pi*array(II+1)) +   
     .               MEAN                                                       
              ARRAY(II+1) =                                                     
     .          STDEV * SQRT(-2.0*LOG(ARRAY(II))) * SIN(2*pi*ARRAY(II+1)) + MEAN   
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
          DO II = 1, NB_RANDOM                                                  
            IF (INDEX_ITYP(II) == 3) THEN                                       
              RNOISE(ICOUNT,INDEX(II)) = ARRAY(II)                                   
            ELSEIF (INDEX_ITYP(II) == 7) THEN                                   
              RNOISE(ICOUNT,INDEX(II)+NUMELC) = ARRAY(II)                            
            ELSEIF (INDEX_ITYP(II) == 1) THEN                                   
              RNOISE(ICOUNT,INDEX(II)+NUMELC+NUMELTG) = ARRAY(II)                    
            ELSEIF (INDEX_ITYP(II) == 51) THEN                                   
              RNOISE(ICOUNT,INDEX(II)+NUMELC+NUMELTG+NUMELS) = ARRAY(II)                  
            ENDIF                                                               
          ENDDO                                                                 
c-------------------------------------------------------
c         check mean and standard deviation
c-------------------------------------------------------
          MEAN  = SUM(ARRAY)/NB_RANDOM
          STDEV = SQRT(SUM((ARRAY - MEAN)**2)/NB_RANDOM)
c-------------------------------------------------------
c         plot normal distribution
c-------------------------------------------------------
          IF (I_METHOD == 2) THEN
            MAX_DISTRIB = ONE /(STDEV*SQRT(TWO * pi))
          ELSEIF(I_METHOD == 1) THEN
            MAX_DISTRIB = ONE /(MAX_VALUE-MIN_VALUE)
          ENDIF
          IF(ITYP == 2)THEN
            WRITE (IOUT,5000)'C3',FAIL_ID
          ENDIF
          WRITE(IOUT,*) ' '
          NB_INTERV = 50
          SIZEY = 20
          IF (MINVAL /= -EP30 .AND. MAXVAL /= EP30) THEN
            MIN_VALUE = MINVAL
            MAX_VALUE = MAXVAL
          ENDIF
          CALL PLOT_DISTRIB( ARRAY,NB_RANDOM, NB_INTERV,SIZEY,MIN_VALUE,
     .         MAX_VALUE,MAX_DISTRIB,'#')
c 
          IF (I_METHOD == 2) THEN 
            WRITE (IOUT,2000) MEAN,STDEV
          ELSEIF (I_METHOD == 1) THEN 
            WRITE (IOUT,2050) MEAN
          ENDIF
      
          IF (SEED_RANDOM == 1) WRITE (IOUT,2100) SEED
          WRITE(IOUT,*) ' '
          WRITE(IOUT,*) ' '
          IF (ALLOCATED(ARRAY)) DEALLOCATE(ARRAY)
c
        END IF  ! biquad
c------------------------
      ENDDO     ! NPERTURB_FAIL
c
      DEALLOCATE(TAB_PART)
C-------------------------------------------------------------
C-------------------------------------------------------------
 4000 FORMAT(/' PERTURBATION  ID',I10/
     .        ' ---------------'/
     . '        TYPE . . . . . . . . . . . . . . .',A/
     . '        INPUT MEAN VALUE . . . . . . . . .',1PG20.13/
     . '        INPUT STANDARD DEVIATION . . . . .',1PG20.13/
     . '        INPUT SEED VALUE . . . . . . . . .',I10/
     . '        FAILURE CRITERIA . . . . . . . . .',A/
     . '        FAILURE CRITERIA ID. . . . . . . .',I10/
     . '        APPLIED ON PARAMETER . . . . . . .',A/
     . '        PARTS:')
 4100 FORMAT(/' PERTURBATION  ID',I10/
     .        ' ---------------'/
     . '        TYPE . . . . . . . . . . . . . . .',A/
     . '        INPUT SEED VALUE . . . . . . . . .',I10/
     . '        FAILURE CRITERIA . . . . . . . . .',A/
     . '        FAILURE CRITERIA ID. . . . . . . .',I10/
     . '        APPLIED ON PARAMETER . . . . . . .',A/
     . '        PARTS:')
C-------------------------------------------------------------
 2000 FORMAT(/
     . '        GENERATED MEAN VALUE . . . . . . .',1PG20.13/
     . '        GENERATED STANDARD DEVIATION . . .',1PG20.13)
 2050 FORMAT(/
     . '        GENERATED MEAN VALUE . . . . . . .',1PG20.13)
 2100 FORMAT(/
     . '        GENERATED SEED VALUE . . . . . . .',I10/)
C-------------------------------------------------------------
 5000 FORMAT(/
     . '        DISTRIBUTION OF SCALE FACTORS APPLIED TO ',A,' VALUE'/
     . '        OF FAILURE CRITERIA ID= . . . . . .',I10)
C------------------------------ 
      RETURN
      END
