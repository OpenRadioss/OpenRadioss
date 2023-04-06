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
Chd|  HM_READ_ACTIV                 source/tools/activ/hm_read_activ.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_ACTIV(IACTIV   ,FACTIV   ,SENSOR_TAB,IGRBRIC,
     .                   IGRQUAD  ,IGRSH4N  ,IGRSH3N ,IGRTRUSS ,IGRBEAM,
     .                   IGRSPRING,LSUBMODEL,UNITAB)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
      USE UNITAB_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IACTIV(LACTIV,*) 
      my_real FACTIV(LRACTIV,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (GROUP_)  , DIMENSION(NGRBEAM) :: IGRBEAM
      TYPE (GROUP_)  , DIMENSION(NGRSPRI) :: IGRSPRING
      TYPE (SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
      TYPE (UNIT_TYPE_),INTENT(IN) :: UNITAB 
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,ISEN,IGSH,IGSH3,IGBR,IGQU,IGBM,IGTR,IGSP,IERR1
      INTEGER IDUM,IFORM, NPRI
      my_real STARTT, STOPT
      LOGICAL :: IS_AVAILABLE
      CHARACTER  TITR*nchartitle
C======================================================================|
      CALL HM_OPTION_START('/ACTIV')
      DO I=1,NACTIV
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       OPTION_TITR = TITR)

        CALL HM_GET_INTV('ISENSOR',ISEN,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('GR_BRICK_SET',IGBR,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('GR_QUAD_SET',IGQU,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('GRSHEL_SET',IGSH,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('GR_TRUSS_SET',IGTR,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('GR_BEAM_SET',IGBM,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('GR_SPRING_SET',IGSP,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('GR_SH3N_SET',IGSH3,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('ACTIV_Iform',IFORM,IS_AVAILABLE,LSUBMODEL)
C
        IF(IFORM == 0) IFORM = 1
        FACTIV(1,I) = ZERO
        FACTIV(2,I) = ZERO
C        
        IACTIV(1,I) =  1   ! activation initiale
        IACTIV(2,I) =  ISEN
        IACTIV(3,I) =  IGBR
        IACTIV(4,I) =  IGQU
        IACTIV(5,I) =  IGSH
        IACTIV(6,I) =  IGTR
        IACTIV(7,I) =  IGBM
        IACTIV(8,I) =  IGSP
        IACTIV(9,I) =  IGSH3
        IACTIV(10,I) = IFORM
C
        IF (ISEN  >  0) THEN
          IERR1 = 1
          DO J=1,NSENSOR
            IF (ISEN  ==  SENSOR_TAB(J)%SENS_ID) THEN
              IACTIV(2,I) = J
              IERR1 = 0
              EXIT
            ENDIF
          ENDDO
          IF (IERR1 == 1) THEN
            CALL ANCMSG(MSGID=470,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=ISEN)
          ENDIF
        ENDIF
C
        IF (IFORM == 2) THEN
          CALL HM_GET_FLOATV('ACTIV_Tstart' ,STARTT ,IS_AVAILABLE ,LSUBMODEL ,UNITAB)
          CALL HM_GET_FLOATV('ACTIV_Tstop' ,STOPT ,IS_AVAILABLE ,LSUBMODEL ,UNITAB)
          IF (STOPT == ZERO) STOPT= INFINITY
          IF (ISEN > 0) THEN
             J = IACTIV(2,I)
             FACTIV(1,I) = STARTT + SENSOR_TAB(J)%TCRIT
             FACTIV(2,I) = STOPT  + SENSOR_TAB(J)%TCRIT
          ELSE
             FACTIV(1,I) = STARTT  
             FACTIV(2,I) = STOPT
          ENDIF			 
        ENDIF
C
        IF (IGBR > 0) THEN
          IERR1 = 1
          DO J=1,NGRBRIC
            IF (IGBR == IGRBRIC(J)%ID) THEN
              IACTIV(3,I) = J
              IERR1 = 0
              EXIT
            ENDIF
          ENDDO
          IF (IERR1 == 1) THEN
            CALL ANCMSG(MSGID=471,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=IGBR)
          ENDIF
        ENDIF
C
        IF (IGQU > 0) THEN
          IERR1 = 1
          DO J=1,NGRQUAD
            IF (IGQU == IGRQUAD(J)%ID) THEN
              IACTIV(4,I) = J
              IERR1 = 0
              EXIT
            ENDIF
          ENDDO
          IF (IERR1 == 1) THEN
            CALL ANCMSG(MSGID=471,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=IGQU)
          ENDIF
        ENDIF
C         
        IF (IGSH > 0) THEN
          IERR1 = 1
          DO J=1,NGRSHEL
            IF (IGSH == IGRSH4N(J)%ID) THEN
              IACTIV(5,I) = J
              IERR1 = 0
              EXIT
            ENDIF
          ENDDO
          IF (IERR1 == 1) THEN
            CALL ANCMSG(MSGID=471,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=IGSH)
          ENDIF
        ENDIF
C         
        IF (IGTR > 0) THEN
          IERR1 = 1
          DO J=1,NGRTRUS
            IF (IGTR == IGRTRUSS(J)%ID) THEN
              IACTIV(6,I) = J
              IERR1 = 0
              EXIT
            ENDIF
          ENDDO
          IF (IERR1 == 1) THEN
            CALL ANCMSG(MSGID=471,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=IGTR)
          ENDIF
        ENDIF
C         
        IF (IGBM > 0) THEN
          IERR1 = 1
          DO J=1,NGRBEAM
            IF (IGBM == IGRBEAM(J)%ID) THEN
              IACTIV(7,I) = J
              IERR1 = 0
              EXIT
            ENDIF
          ENDDO
          IF (IERR1 == 1) THEN
            CALL ANCMSG(MSGID=471,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=IGBM)
          ENDIF
        ENDIF
C         
        IF (IGSP > 0) THEN
          IERR1 = 1
          DO J=1,NGRSPRI
            IF (IGSP == IGRSPRING(J)%ID) THEN
              IACTIV(8,I) = J
              IERR1 = 0
              EXIT
            ENDIF
          ENDDO
          IF (IERR1 == 1) THEN
            CALL ANCMSG(MSGID=471,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=IGSP)
          ENDIF
        ENDIF
C        
        IF (IGSH3 > 0) THEN
          IERR1 = 1
          DO J=1,NGRSH3N
            IF (IGSH3 == IGRSH3N(J)%ID) THEN
              IACTIV(9,I) = J
              IERR1 = 0
              EXIT
            ENDIF
          ENDDO
          IF (IERR1 == 1) THEN
            CALL ANCMSG(MSGID=471,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=IGSH3)
          ENDIF
        ENDIF
C 
        IF(MOD(I,50) == 1) WRITE(IOUT,1000)
        IF(IFORM == 1) THEN
          WRITE(IOUT,'(1X,10(1X,I10))')
     .          ID,ISEN,IGBR,IGQU,IGSH,IGTR,IGBM,IGSP,IGSH3,IFORM
        ELSEIF(IFORM == 2) THEN
          WRITE(IOUT,'(1X,10(1X,I10),1X,G20.13,1X,G20.13)')
     .          ID,ISEN,IGBR,IGQU,IGSH,IGTR,IGBM,IGSP,IGSH3,IFORM,STARTT,STOPT
        ENDIF
      ENDDO      
C
      RETURN
C
 1000 FORMAT(//
     . '      ELEMENT ACTIVATION-DEACTIVATION   '/
     . '      -------------------------------   '/ 4X,
     . '  OPTION-NB     SENSOR',
     . '   BRICK-GR    QUAD-GR   SHELL-GR   TRUSS-GR',
     . '    BEAM-GR  SPRING-GR    SH3N-GR  FORM-FLAG',2X,
     . ' START-TIME            STOP-TIME')
C
      RETURN
      END
      
