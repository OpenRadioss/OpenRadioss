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
Chd|  INISEN                        source/tools/sensor/inisen.F  
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|====================================================================
      SUBROUTINE INISEN(SENSOR_TAB, LSENSOR,IPARI ,NOM_OPT,PTR_NOPT_RWALL,
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
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LSENSOR(NSENSI,NSENSOR),IPARI(NPARI,NINTER)
      INTEGER NOM_OPT(LNOPT1,*),PTR_NOPT_RWALL,PTR_NOPT_SECT,
     .        PTR_NOPT_INTER
      INTEGER ,DIMENSION(NUMELR)      :: R_SKEW
      INTEGER ,DIMENSION(NIXR,NUMELR) :: IXR
      INTEGER NBR_SENS,IPARSENS,JSUB
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IN,IS1,IS2,TYP,ID,SPRING_ID
      CHARACTER TITR*nchartitle
c=======================================================================
C     INIT DES PARAMETRES DE SENSOR
C---------------------------------------------
      DO K=1,NSENSOR
        IS1=0
        IS2=0
        IN =0
        ID=LSENSOR(1,K)
c        CALL FRETITL2(TITR,LSENSOR(NSENSI-LTITR+1,K),LTITR)
         TITR = 'SENSOR '

        TYP = LSENSOR(2,K)
        IF (TYP == 3)THEN
C-------------------------------------
C         SENSOR de SENSOR
C-------------------------------------
          DO I=1,NSENSOR
            IF(LSENSOR(4,K) == LSENSOR(1,I))IS1=I
            IF(LSENSOR(5,K) == LSENSOR(1,I))IS2=I
          ENDDO
          IF(IS1 == 0)THEN
            CALL ANCMSG(MSGID=339,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=LSENSOR(4,K),C2='SENSOR')
          ENDIF
          LSENSOR(4,K)=IS1
          LSENSOR(5,K)=IS2
        ELSEIF(TYP == 4.OR.TYP == 5)THEN
C-------------------------------------
C         SENSOR de SENSOR et ou
C-------------------------------------
          DO I=1,NSENSOR
            IF(LSENSOR(4,K) == LSENSOR(1,I))IS1=I
            IF(LSENSOR(5,K) == LSENSOR(1,I))IS2=I
          ENDDO
          IF(IS1 == 0)THEN
            CALL ANCMSG(MSGID=339,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=LSENSOR(4,K),C2='SENSOR')
          ENDIF
          IF(IS2 == 0)THEN
            CALL ANCMSG(MSGID=339,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=LSENSOR(5,K),C2='SENSOR')
          ENDIF
          LSENSOR(4,K)=IS1
          LSENSOR(5,K)=IS2
        ELSEIF(TYP == 6)THEN
C-------------------------------------
C         Interfaces
C-------------------------------------
          DO I=1,NINTER
            IF (IPARI(15,I) == LSENSOR(4,K)) THEN
              IN=I
              EXIT
            ENDIF
          ENDDO
          DO I=1,NINTSUB
            IF (NOM_OPT(1,PTR_NOPT_INTER+I) == LSENSOR(4,K)) THEN
              DO J=1,NINTER
                 IF (IPARI(15,J) == NOM_OPT(2,PTR_NOPT_INTER+I))THEN
                   IN = I + NINTER
                   LSENSOR(5,K) = J
                 ENDIF
              ENDDO
            ENDIF
          ENDDO
          IF (IN == 0)THEN

            CALL ANCMSG(MSGID=339,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=LSENSOR(4,K),C2='INTERFACE')
          ENDIF
          LSENSOR(4,K)=IN
        ELSEIF(TYP == 7)THEN
C-------------------------------------
C         RWALL
C-------------------------------------
          DO I=1,NRWALL
            IF(NOM_OPT(1,PTR_NOPT_RWALL+I) == LSENSOR(4,K))IN=I
          ENDDO
          IF (IN == 0)THEN
            CALL ANCMSG(MSGID=339,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=LSENSOR(4,K),C2='RIGID WALL')
          ENDIF
          LSENSOR(4,K)=IN
        ELSEIF (TYP == 8)THEN
C-------------------------------------
C         SENSOR de !SENSOR
C-------------------------------------
          DO I=1,NSENSOR
            IF(LSENSOR(4,K) == LSENSOR(1,I))IS1=I
          ENDDO
          IF(IS1 == 0)THEN
            CALL ANCMSG(MSGID=339,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=LSENSOR(4,K),C2='SENSOR')
          ENDIF
          LSENSOR(4,K)=IS1
        ELSEIF (TYP == 11)THEN
C-------------------------------------
C         RBODY
C-------------------------------------
          DO I=1,NRBODY
            IF (NOM_OPT(1,I) == LSENSOR(4,K)) IN=I
          ENDDO
          IF (IN == 0)THEN
             CALL ANCMSG(MSGID=339,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=LSENSOR(4,K),C2='RIGID BODY')
          ENDIF
          LSENSOR(4,K)=IN
        ELSEIF (TYP == 12)THEN
C-------------------------------------
C         SECTION
C-------------------------------------
          DO I=1,NSECT
            IF (NOM_OPT(1,PTR_NOPT_SECT +I) == LSENSOR(4,K)) IN=I
          ENDDO
          IF (IN == 0)THEN
             CALL ANCMSG(MSGID=339,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=LSENSOR(4,K),C2='SECTION')
          ENDIF
          LSENSOR(4,K)=IN
c---
        ELSEIF (TYP == 13)THEN
C-------------------------------------
C         WORK
C-------------------------------------
c         ID section
          IN = 0
          DO I=1,NSECT
            IF (NOM_OPT(1,PTR_NOPT_SECT +I) == LSENSOR(6,K)) IN=I
          ENDDO
          LSENSOR(6,K)=IN
c
c         ID INTERFACE
          IN = 0
          DO I=1,NINTER
            IF (IPARI(15,I) == LSENSOR(7,K)) THEN
              IN=I
              EXIT
            ENDIF
          ENDDO
          DO I=1,NINTSUB
            IF (NOM_OPT(1,PTR_NOPT_INTER+I) == LSENSOR(8,K)) THEN
              DO J=1,NINTER
                 IF (IPARI(15,J) == NOM_OPT(2,PTR_NOPT_INTER+I))THEN
                   IN = I + NINTER
                   LSENSOR(8,K) = J
                 ENDIF
              ENDDO
            ENDIF
          ENDDO
          LSENSOR(7,K)=IN
c
c         ID RWALL
          IN = 0
          DO I=1,NRWALL
            IF (NOM_OPT(1,PTR_NOPT_RWALL+I) == LSENSOR(9,K)) IN=I
          ENDDO
          LSENSOR(9,K)=IN
c
c         ID RBODY
          IN = 0
          DO I=1,NRBODY
            IF (NOM_OPT(1,I) == LSENSOR(10,K)) IN=I
          ENDDO
          LSENSOR(10,K)=IN
c
          IF (LSENSOR(6,K) == 0 .and. LSENSOR(7,K) == 0 .and. LSENSOR(8,K) == 0 .and.
     .        LSENSOR(9,K) == 0 .and. LSENSOR(10,K) == 0) THEN
            CALL ANCMSG(MSGID=339,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=IN,
     .                  C1=TITR,
     .                  I2=LSENSOR(10,K),C2='ID in SENSOR WORK')
          ENDIF
c
        ELSEIF (TYP == 19)THEN
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
         IF (LSENSOR(2,I)== 6 .OR. LSENSOR(2,I)== 12 .OR.
     .       LSENSOR(2,I)== 7 .OR. LSENSOR(2,I)== 11 .OR.
     .       LSENSOR(2,I)== 13) THEN
            NBR_SENS=NBR_SENS+1
            IPARSENS=1
         ENDIF
      ENDDO
c-----------
      RETURN
      END
