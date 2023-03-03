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
Chd|  INISEN                        source/tools/sensor/inisen.F  
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        SORT_LOGICAL_SENSORS          source/tools/sensor/sort_logical_sensors.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|====================================================================
      SUBROUTINE INISEN(SENSOR_TAB, IPARI ,NOM_OPT,PTR_NOPT_RWALL,
     .                  PTR_NOPT_SECT,NBR_SENS,IPARSENS,PTR_NOPT_INTER,
     .                  IXR    ,R_SKEW   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE MESSAGE_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,NINTER)
      INTEGER NOM_OPT(LNOPT1,*),PTR_NOPT_RWALL,PTR_NOPT_SECT,
     .        PTR_NOPT_INTER
      INTEGER ,DIMENSION(NUMELR)      :: R_SKEW
      INTEGER ,DIMENSION(NIXR,NUMELR) :: IXR
      INTEGER NBR_SENS,IPARSENS,JSUB
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IN,IS1,IS2,SENS_TYPE,SENS_ID,SPRING_ID
      CHARACTER TITR*nchartitle
c=======================================================================
C     INIT DES PARAMETRES DE SENSOR
C---------------------------------------------
      DO K=1,NSENSOR
        IS1  = 0
        IS2  = 0
        IN   = 0
        TITR = 'SENSOR '     
        SENS_ID   = SENSOR_TAB(K)%SENS_ID
        SENS_TYPE = SENSOR_TAB(K)%TYPE
c
        IF (SENS_TYPE == 3) THEN
C-------------------------------------
C         SENSOR de SENSOR
C-------------------------------------
          DO I=1,NSENSOR
            IF (SENSOR_TAB(K)%IPARAM(1) == SENSOR_TAB(I)%SENS_ID) IS1=I
            IF (SENSOR_TAB(K)%IPARAM(2) == SENSOR_TAB(I)%SENS_ID) IS2=I
          ENDDO
          IF (IS1 == 0) THEN
            CALL ANCMSG(MSGID=339,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=SENS_ID,
     .                  C1=TITR,
     .                  I2=SENSOR_TAB(K)%IPARAM(1),C2='SENSOR')
          ENDIF
          SENSOR_TAB(K)%IPARAM(1) = IS1
          SENSOR_TAB(K)%IPARAM(2) = IS2
c
        ELSEIF (SENS_TYPE == 4 .OR. SENS_TYPE == 5) THEN
C-------------------------------------
C         SENSOR of SENSOR 'AND' , 'OR'
C-------------------------------------
          DO I=1,NSENSOR
            IF (SENSOR_TAB(K)%IPARAM(1) == SENSOR_TAB(I)%SENS_ID) IS1=I
            IF (SENSOR_TAB(K)%IPARAM(2) == SENSOR_TAB(I)%SENS_ID) IS2=I
          ENDDO
          IF (IS1 == 0) THEN
            CALL ANCMSG(MSGID=339,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=SENS_ID,
     .                  C1=TITR,
     .                  I2=SENSOR_TAB(K)%IPARAM(1),C2='SENSOR')
          ENDIF
          IF (IS2 == 0) THEN
            CALL ANCMSG(MSGID=339,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=SENS_ID,
     .                  C1=TITR,
     .                  I2=SENSOR_TAB(K)%IPARAM(2),C2='SENSOR')
          ENDIF
          SENSOR_TAB(K)%IPARAM(1) = IS1
          SENSOR_TAB(K)%IPARAM(2) = IS2
c
        ELSEIF (SENS_TYPE == 6) THEN
C-------------------------------------
C         Contact sensor
C-------------------------------------
          DO I=1,NINTER
            IF (IPARI(15,I) == SENSOR_TAB(K)%IPARAM(1)) THEN 
              IN=I
              EXIT
            ENDIF
          ENDDO
          DO I=1,NINTSUB
            IF (NOM_OPT(1,PTR_NOPT_INTER+I) == SENSOR_TAB(K)%IPARAM(1)) THEN
              DO J=1,NINTER
                 IF (IPARI(15,J) == NOM_OPT(2,PTR_NOPT_INTER+I))THEN
                   IN = I + NINTER
                   SENSOR_TAB(K)%IPARAM(2) = J
                 ENDIF
              ENDDO
            ENDIF
          ENDDO
          IF (IN == 0)THEN

            CALL ANCMSG(MSGID=339,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=SENS_ID,
     .                  C1=TITR,
     .                  I2=SENSOR_TAB(K)%IPARAM(1),C2='INTERFACE')
          ENDIF
          SENSOR_TAB(K)%IPARAM(1) = IN
c
        ELSEIF (SENS_TYPE == 7) THEN
C-------------------------------------
C         RWALL
C-------------------------------------
          DO I=1,NRWALL
            IF (NOM_OPT(1,PTR_NOPT_RWALL+I) == SENSOR_TAB(K)%IPARAM(1)) IN=I
          ENDDO
          IF (IN == 0)THEN
            CALL ANCMSG(MSGID=339,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=SENS_ID,
     .                  C1=TITR,
     .                  I2=SENSOR_TAB(K)%IPARAM(1),C2='RIGID WALL')
          ENDIF
          SENSOR_TAB(K)%IPARAM(1) = IN
c
        ELSEIF (SENS_TYPE == 8)THEN
C-------------------------------------
C         SENSOR of SENSOR
C-------------------------------------
          DO I=1,NSENSOR
            IF (SENSOR_TAB(K)%IPARAM(1) == SENSOR_TAB(I)%SENS_ID) IS1=I
          ENDDO
          IF(IS1 == 0)THEN
            CALL ANCMSG(MSGID=339,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=SENS_ID,
     .                  C1=TITR,
     .                  I2=SENSOR_TAB(K)%IPARAM(1),C2='SENSOR')
          ENDIF
          SENSOR_TAB(K)%IPARAM(1) = IS1
c
        ELSEIF (SENS_TYPE == 11)THEN
C-------------------------------------
C         RBODY
C-------------------------------------
          DO I=1,NRBODY
            IF (NOM_OPT(1,I) == SENSOR_TAB(K)%IPARAM(1)) IN=I
          ENDDO
          IF (IN == 0)THEN
             CALL ANCMSG(MSGID=339,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                   I1=SENS_ID,
     .                   C1=TITR,
     .                   I2=SENSOR_TAB(K)%IPARAM(1),C2='RIGID BODY')
          ENDIF
          SENSOR_TAB(K)%IPARAM(1) = IN
c
        ELSEIF (SENS_TYPE == 12)THEN
C-------------------------------------
C         SECTION
C-------------------------------------
          DO I=1,NSECT
            IF (NOM_OPT(1,PTR_NOPT_SECT +I) == SENSOR_TAB(K)%IPARAM(1)) IN=I
          ENDDO
          IF (IN == 0)THEN
             CALL ANCMSG(MSGID=339,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                   I1=SENS_ID,
     .                   C1=TITR,
     .                   I2=SENSOR_TAB(K)%IPARAM(1),C2='SECTION')
          ENDIF
          SENSOR_TAB(K)%IPARAM(1) = IN
c---
        ELSEIF (SENS_TYPE == 13) THEN
C-------------------------------------
C         WORK
C-------------------------------------
c         ID section
          IN = 0
          DO I=1,NSECT
            IF (NOM_OPT(1,PTR_NOPT_SECT +I) == SENSOR_TAB(K)%IPARAM(3)) IN=I
          ENDDO
          SENSOR_TAB(K)%IPARAM(3) = IN
c
c         ID INTERFACE
          IN = 0
          DO I=1,NINTER
            IF (IPARI(15,I) == SENSOR_TAB(K)%IPARAM(4)) THEN
              IN=I
              EXIT
            ENDIF
          ENDDO
          DO I=1,NINTSUB
            IF (NOM_OPT(1,PTR_NOPT_INTER+I) == SENSOR_TAB(K)%IPARAM(5)) THEN
              DO J=1,NINTER
                 IF (IPARI(15,J) == NOM_OPT(2,PTR_NOPT_INTER+I))THEN
                   IN = I + NINTER
                   SENSOR_TAB(K)%IPARAM(5) = J
                 ENDIF
              ENDDO
            ENDIF
          ENDDO
          SENSOR_TAB(K)%IPARAM(4) = IN
c
c         ID RWALL
          IN = 0
          DO I=1,NRWALL
            IF (NOM_OPT(1,PTR_NOPT_RWALL+I) == SENSOR_TAB(K)%IPARAM(6)) IN=I
          ENDDO
          SENSOR_TAB(K)%IPARAM(6) = IN
c
c         ID RBODY
          IN = 0
          DO I=1,NRBODY
            IF (NOM_OPT(1,I) == SENSOR_TAB(K)%IPARAM(7)) IN=I
          ENDDO
          SENSOR_TAB(K)%IPARAM(7) = IN
c
          IF (SENSOR_TAB(K)%IPARAM(3) == 0 .and. SENSOR_TAB(K)%IPARAM(4) == 0 .and. 
     .        SENSOR_TAB(K)%IPARAM(5) == 0 .and. SENSOR_TAB(K)%IPARAM(6) == 0 .and. 
     .        SENSOR_TAB(K)%IPARAM(7) == 0) THEN
            CALL ANCMSG(MSGID=339,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=IN,
     .                  C1=TITR,
     .                  I2=SENSOR_TAB(K)%SENS_ID,C2='ID in SENSOR WORK')
          ENDIF
c
        ELSEIF (SENS_TYPE == 19) THEN
C-------------------------------------
C         NIC_NIJ
C-------------------------------------
          SPRING_ID = SENSOR_TAB(K)%IPARAM(1)
          DO I=1,NUMELR
            IF (IXR(6,I) == SPRING_ID) THEN
              SENSOR_TAB(K)%IPARAM(2) = I
              SENSOR_TAB(K)%IPARAM(4) = R_SKEW(I)
              EXIT
            END IF
          ENDDO
c---
        ENDIF ! SENSOR TYPE
      ENDDO
C-------------------------------------
C   Traitement special sensors type force + parit/on
C-------------------------------------
      IPARSENS=0        ! Flag pour sensor type force
      NBR_SENS=0
      DO I=1,NSENSOR
        SENS_TYPE = SENSOR_TAB(I)%TYPE
        IF (SENS_TYPE== 6  .OR. SENS_TYPE== 7 .OR. SENS_TYPE== 11 .OR.
     .      SENS_TYPE== 12 .OR. SENS_TYPE== 13) THEN 
          NBR_SENS = NBR_SENS+1
          IPARSENS = 1
        ENDIF
      ENDDO
c----------------------------------------------------------------------
c     Logical sensors : AND, OR, NOT, SENS - create a dependancy order
c----------------------------------------------------------------------
      CALL SORT_LOGICAL_SENSORS()


c-----------
      RETURN
      END
