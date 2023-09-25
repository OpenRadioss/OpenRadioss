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
Chd|  HM_READ_LOAD_PRESSURE         source/loads/general/load_pressure/hm_read_load_pressure.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_LOAD_PRESSURE(
     .           NUMLOADP   ,ILOADP     ,LLOADP    ,INTERLOADP ,FACLOADP   ,
     .           KLOADPINTER,LOADPINTER ,NPC       ,SENSORS    ,IGRSURF    ,
     .           UNITAB     ,ISKN      ,LSUBMODEL  ,DGAPINT    ,INTGAPLOADP,
     .           DGAPLOADINT,S_LOADPINTER )
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE UNITAB_MOD
        USE R2R_MOD
        USE MESSAGE_MOD
        USE GROUPDEF_MOD
        USE SUBMODEL_MOD
        USE HM_OPTION_READ_MOD
        USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "r2r_c.inc"
#include      "sysunit.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
        INTEGER NUMLOADP
        INTEGER NPC(*),ISKN(LISKN,*),
     .          ILOADP(SIZLOADP,*), LLOADP(*)
        INTEGER, INTENT(IN) :: S_LOADPINTER
        INTEGER, INTENT(INOUT) ::  KLOADPINTER(NINTER+1)  ,LOADPINTER(S_LOADPINTER),
     .           INTERLOADP(NINTLOADP)
        my_real FACLOADP(LFACLOAD,*)
        my_real , INTENT(INOUT) :: DGAPINT(NINTER),
     .          INTGAPLOADP(NINTLOADP),DGAPLOADINT(S_LOADPINTER )
C-----------------------------------------------
        TYPE (SURF_)   ,TARGET, DIMENSION(NSURF)   :: IGRSURF
        TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
        TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------

        INTEGER I,J,K, N,NI,
     .          SUB_INDEX, SUB_ID, UID, ID, IFLAGUNIT,NIP,
     .          NN,IAD,TSENS,ISENS,IS,ISU,NOSKEW,IDIR,IFUNC,NINTERP,INORM,
     .          H, CAPT,TINTER,IDINT,NIK,NBINTER,NB,NOINT,NL,STAT,IFUNCL,
     .          IDSENS,IDSKEW,ILOAD,NINTERS,NIDXLOAD
        INTEGER, DIMENSION(:), ALLOCATABLE :: ID_INTER,INTER_TYP
        my_real
     .          FCX,FCY,FAC_FCX,FAC_FCY,GAP_I
        CHARACTER MESS*40, DIR*ncharfield,
     .            char_X*1, char_Y*1, char_Z*1
        CHARACTER TITR*nchartitle,KEY*ncharline
        LOGICAL IS_AVAILABLE


        DATA char_X/'X'/
        DATA char_Y/'Y'/
        DATA char_Z/'Z'/
        DATA MESS/'PRESSURE LOAD DEFINITION          '/
C----------------------------------------------------------------------------------
C   C o m m e n t s
C----------------------------------------------------------------------------------
C
C                     /LOAD/PRESSURE
C
C----------------------------------------------------------------------------------

C     ILOADP(SIZLOADP,*)
C     ILOADP(1,K) :   NN (Nb of segments)                             NN
C     ILOADP(2,K) :   ID                                              IS
C     ILOADP(3,K) :   Pressure Function number                        IFUNC
C     ILOADP(4,K) :   IAD  Address of segments in LLOADP              IAD
C     ILOADP(5,K) :   Number of interfaces associated                 NINTERP
C     ILOADP(6,K) :   Direction                                       IDIR
C     ILOADP(7,K) :   Sensor Number                                   ISENS
C     ILOADP(8,K) :   Skew Number                                     NOSKEW
C     ILOADP(9,K) :   Inorm flag                                      INORM
C     ILOADP(10,K):   Iload flag                                      ILOAD

C----------------------------------------------------------------------------------
C     LLOADP(1,K) :   NODE1                                           NODE1
C     LLOADP(2,K) :   NODE2                                           NODE2
C     LLOADP(3,K) :   NODE3                                           NODE3
C     LLOADP(4,K) :   NODE4                                           NODE4
C----------------------------------------------------------------------------------
C     FACLOADP(LFACLOAD,*)
C     FACLOADP( 1,K) = y_scalep                                       FCY
C     FACLOADP( 2,K) = ONE/x_scalep                                    1/FCX
C----------------------------------------------------------------------------------
C   S o u r c e   L i n e s
C----------------------------------------------------------------------------------


C*******   Interface reading  **************

        ALLOCATE (ID_INTER(HM_NINTER ),STAT=stat)
        ID_INTER(1:HM_NINTER ) = 0
        ALLOCATE (INTER_TYP(HM_NINTER),STAT=stat)
        INTER_TYP(1:HM_NINTER ) = 0
C
        CALL HM_OPTION_START('/INTER')
C
        NBINTER = 0
C
        DO I =1,HM_NINTER

C--------------------------------------------------
C EXTRACT DATAS OF /INTER/... LINE
C--------------------------------------------------
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                         OPTION_ID = NOINT,
     .                         KEYWORD2 = KEY)

          IF(KEY(1:LEN_TRIM(KEY))/='SUB') THEN
            NBINTER=NBINTER+1
C
            ID_INTER(NBINTER) = NOINT

            IF(KEY(1:LEN_TRIM(KEY))=='TYPE21') INTER_TYP(NBINTER)=21

          ENDIF
        ENDDO

C*******   /LOAD/PRESSURE reading  **************

        NN           = 0
C--------------------------------------------------
C START BROWSING MODEL /LOAD/PRESSURE
C--------------------------------------------------
        CALL HM_OPTION_START('/LOAD/PRESSURE')
C--------------------------------------------------
C BROWSING MODEL 1..NLOADP_F
C--------------------------------------------------
        NIDXLOAD = NLOADP_F+NLOADP_B
        DO K=1,NLOADP_HYD
C--------------------------------------------------
C EXTRACT DATAS OF /LOAD/PFLUID... LINE
C--------------------------------------------------
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                         OPTION_ID = ID,
     .                         UNIT_ID = UID,
     .                         SUBMODEL_ID = SUB_ID,
     .                         SUBMODEL_INDEX = SUB_INDEX,
     .                         OPTION_TITR = TITR)

          IFLAGUNIT = 0

          DO J=1,NUNITS
            IF (UNITAB%UNIT_ID(J) == UID) THEN
              IFLAGUNIT = 1
              EXIT
            ENDIF
          ENDDO
          IF (UID/=0.AND.IFLAGUNIT==0) THEN
            CALL ANCMSG(MSGID   = 659,
     .                  ANMODE  = ANINFO,
     .                  MSGTYPE = MSGERROR,
     .                  I2      = UID,
     .                  I1      = ID,
     .                  C1      = 'PRESSURE LOAD',
     .                  C2      = 'PRESSURE LOAD',
     .                  C3      = TITR)
          ENDIF
C-----------
          ILOADP(2,K+NIDXLOAD) = ID
C-----------
          IAD      = NUMLOADP + 1

          PDEL = 1   ! Idel option is default optioon

          DIR = ''
          NINTERP = 0
          IDSENS = 0
          IDSKEW = 0
          IDIR = 0
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------

          CALL HM_GET_INTV('surf_ID',ISU,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('fct_ID',IFUNC,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('Inorm',INORM,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('sens_ID',IDSENS,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('Iload',ILOAD,IS_AVAILABLE,LSUBMODEL)
          IF(INORM == 0) INORM = 1 ! default value
          IF(ILOAD == 0) ILOAD = 1 ! default value

          IF (INORM > 1) THEN
            CALL HM_GET_INTV('skew_ID',IDSKEW,IS_AVAILABLE,LSUBMODEL)
            !-----------EXTRACT DATAS (STRING VALUES)
            CALL HM_GET_STRING('Direction',DIR,ncharfield,IS_AVAILABLE)

            IF(DIR(1:1)=='X') IDIR=1
            IF(DIR(1:1)=='Y') IDIR=2
            IF(DIR(1:1)=='Z') IDIR=3
          ENDIF

          !-----------
          IS=0
          DO J=1,NSURF
            IF (ISU==IGRSURF(J)%ID) IS=J
          ENDDO
          IF(IS/=0)THEN
            NN=IGRSURF(IS)%NSEG
            DO J=1,NN
              LLOADP(IAD+4*(J-1))  =IGRSURF(IS)%NODES(J,1)
              LLOADP(IAD+4*(J-1)+1)=IGRSURF(IS)%NODES(J,2)
              LLOADP(IAD+4*(J-1)+2)=IGRSURF(IS)%NODES(J,3)
              IF(IGRSURF(IS)%NODES(J,2)==IGRSURF(IS)%NODES(J,3))THEN
                LLOADP(IAD+4*(J-1)+3)=0
              ELSE
                LLOADP(IAD+4*(J-1)+3)=IGRSURF(IS)%NODES(J,4)
              ENDIF
            ENDDO
          ENDIF
          ILOADP(1,K+NIDXLOAD)=4*NN

C-----------
          IFUNCL=0
          DO J=1,NFUNCT
            IF(NPC(NFUNCT+J+1)==IFUNC)IFUNCL=J
          ENDDO
          IF(IFUNCL==0)THEN
            CALL ANCMSG(MSGID=883,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=IFUNC)
          ENDIF
          !-----------
          ISENS = 0
          IF(IDSENS > 0) THEN
            DO J=1,SENSORS%NSENSOR
              IF(IDSENS/=0) THEN
                IF(IDSENS == SENSORS%SENSOR_TAB(J)%SENS_ID) THEN
                  ISENS=J
                  EXIT
                ENDIF
              ENDIF
            ENDDO
            IF(ISENS==0)THEN
              CALL ANCMSG(MSGID=930,
     .             MSGTYPE=MSGERROR,
     .             ANMODE=ANINFO_BLIND_1,
     .             I1=ID,
     .             C1=TITR,
     .             I2=IDSENS)
            ENDIF
          ENDIF

          !-----------

          NOSKEW = 0
          IF(IDSKEW == 0 .AND. SUB_INDEX /= 0 ) IDSKEW = LSUBMODEL(SUB_INDEX)%SKEW

          IF(IDSKEW > 0) THEN
            DO J=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
              IF(IDSKEW == ISKN(4,J+1)) THEN
                NOSKEW=J+1
              ENDIF
            ENDDO
            IF(NOSKEW==0)THEN
              CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                 C1='LOAD PRESSURE',
     .                 C2='LOAD PRESSURE',
     .                 I2=IDSKEW,I1=ID,C3=TITR)
            ENDIF
          ENDIF

C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
          CALL HM_GET_FLOATV('xscale_p',FCX,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV_DIM('xscale_p',FAC_FCX,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('yscale_p',FCY,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV_DIM('yscale_p',FAC_FCY,IS_AVAILABLE,LSUBMODEL,UNITAB)

          IF (FCX == ZERO) FCX = FAC_FCX
          IF (FCY == ZERO) FCY = FAC_FCY
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES) : Interfaces reading
C--------------------------------------------------
          CALL HM_GET_INTV('N_inter_P',NINTERP,IS_AVAILABLE,LSUBMODEL)

          NINTERS = 0
          IF(NINTERP > 0) THEN
            DO NIP=1,NINTERP

C EXTRACT DATAS (INTEGER VALUES)

              CALL HM_GET_INT_ARRAY_INDEX('Inter_IDs',IDINT,NIP,IS_AVAILABLE,LSUBMODEL)
              CALL HM_GET_FLOAT_ARRAY_INDEX('Gap_shift_i',GAP_I,NIP,IS_AVAILABLE,LSUBMODEL,UNITAB)
              TINTER = 0
              IF(IDINT > 0 ) THEN
                DO NI=1,NBINTER
                  IF(ID_INTER(NI) == IDINT)THEN
                    INTERLOADP(NINTLOADP+NIP)= NI
                    IF(INTER_TYP(NI) == 21) NINTLOADP21 = NINTLOADP21 + 1
                    TINTER = 1
                    DGAPINT(NI)=MAX(DGAPINT(NI),GAP_I)
                    INTGAPLOADP(NINTLOADP+NIP)= GAP_I
                  END IF
                END DO
                IF(TINTER==0)THEN
                  CALL ANCMSG(MSGID=2021,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO,
     .                        I1=ID,
     .                        C1=TITR,
     .                        I2=IDINT)
                ENDIF
                NINTERS = NINTERS + 1
              ENDIF
            ENDDO
          ENDIF
C--------------
C Storage
C--------------
          IF(IS/=0)THEN
            ILOADP  ( 3,K+NIDXLOAD) = IFUNCL
            ILOADP  ( 4,K+NIDXLOAD) = IAD
            ILOADP  ( 5,K+NIDXLOAD) = NINTERS
            ILOADP  ( 6,K+NIDXLOAD) = IDIR
            ILOADP  ( 7,K+NIDXLOAD) = ISENS
            ILOADP  ( 8,K+NIDXLOAD) = NOSKEW
            ILOADP  ( 9,K+NIDXLOAD) = INORM
            ILOADP  (10,K+NIDXLOAD) = ILOAD

            FACLOADP( 1,K+NIDXLOAD) = FCY
            FACLOADP( 2,K+NIDXLOAD) = ONE/FCX
            !Multidomaines -> on decompte les seg communs, on ne les compte qu'une foi---
            IF (IDDOM>0) NN = NN-ISURF_R2R(1,IS)
          ENDIF

          WRITE (IOUT,2002)
          WRITE (IOUT,'(I10,2X,I10,2X,I10,2X,I10,9X,A1,2X,I10,2X,
     .                1PG20.13,2X,1PG20.13)')ISU,IFUNC,IDSENS,INORM,DIR(1:1),IDSKEW,FCX,FCY
          IF(NINTERS > 0) THEN
            WRITE (IOUT,2003)
            J = 0
            DO I=1,(NINTERS/10)
              WRITE (IOUT,'(10(6X,I10,4X))')ID_INTER(INTERLOADP(NINTLOADP+1:NINTLOADP+10))
              WRITE (IOUT,'(10G20.13)')INTGAPLOADP(NINTLOADP+1:NINTLOADP+10)
              J = J +10
            ENDDO
            IF(MODULO(NINTERS,10) > 0) THEN
              WRITE (IOUT,'(10(6X,I10,4X))')ID_INTER(INTERLOADP(NINTLOADP+J+1:NINTLOADP+NINTERP))
              WRITE (IOUT,'(10G20.13)')INTGAPLOADP(NINTLOADP+J+1:NINTLOADP+NINTERS)
            ENDIF
          ENDIF
          NUMLOADP = NUMLOADP + 4*NN
          NINTLOADP = NINTLOADP + NINTERS

        ENDDO ! next K (next /LOAD/PRESSURE)


        IF(NINTLOADP > 0) THEN
          NIK = 0
          DO K=1,NLOADP_HYD
            NINTERP = ILOADP(5,K+NIDXLOAD)
            DO N=1,NINTERP
              NI = INTERLOADP(NIK + N)
              KLOADPINTER(NI) = KLOADPINTER(NI)+1
            ENDDO
            NIK = NIK + NINTERP
          ENDDO

          DO N=1,NINTER
            KLOADPINTER(N+1) = KLOADPINTER(N+1) + KLOADPINTER(N)
          END DO

          DO N=NINTER,1,-1
            KLOADPINTER(N+1) = KLOADPINTER(N)
          END DO
          KLOADPINTER(1)=0

          NIK = 0
          DO K=1,NLOADP_HYD
            NINTERP = ILOADP(5,K+NIDXLOAD)
            DO N=1,NINTERP
              NI = INTERLOADP(NIK + N)
              KLOADPINTER(NI) = KLOADPINTER(NI)+1
              LOADPINTER(KLOADPINTER(NI)) = K
              DGAPLOADINT(KLOADPINTER(NI)) = INTGAPLOADP(NIK + N)

            ENDDO
            NIK = NIK + NINTERP
          ENDDO

          DO N=NINTER,1,-1
            KLOADPINTER(N+1) = KLOADPINTER(N)
          END DO
          KLOADPINTER(1)=0

        ENDIF

        DEALLOCATE (ID_INTER,INTER_TYP)

C-------------------------------------------------------------------------------C
        RETURN
C--------------------------------------------------------------------------------C
 2002   FORMAT(//

     .  '     PRESSURE LOADS (GENERAL)  '/
     .  '     ------------------  '/
     .  '   SURFACE       CURVE      SENSOR       INORM DIRECTION        SKEW',
     .  '             SCALE_X              SCALE_Y')

 2003   FORMAT(//
     .  '   INTERFACES AND GAP SHIFTS')

C-----------------------------------------------
      END SUBROUTINE HM_READ_LOAD_PRESSURE
