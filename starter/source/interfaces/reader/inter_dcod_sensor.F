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
      !||    inter_dcod_sensor      ../starter/source/interfaces/reader/inter_dcod_sensor.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_inter_type05   ../starter/source/interfaces/int05/hm_read_inter_type05.F
      !||    hm_read_inter_type07   ../starter/source/interfaces/int07/hm_read_inter_type07.F
      !||    hm_read_inter_type11   ../starter/source/interfaces/int11/hm_read_inter_type11.F
      !||    hm_read_inter_type21   ../starter/source/interfaces/int21/hm_read_inter_type21.F
      !||    hm_read_inter_type24   ../starter/source/interfaces/int24/hm_read_inter_type24.F
      !||    hm_read_inter_type25   ../starter/source/interfaces/int25/hm_read_inter_type25.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||    fretitl2               ../starter/source/starter/freform.F
      !||--- uses       -----------------------------------------------------
      !||    intstamp_mod           ../starter/share/modules1/intstamp_mod.F
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE INTER_DCOD_SENSOR(NTYP,NI,IPARI,NOM_OPT,SENSORS)

C-----------------------------------------------
C     DECODE USER NUMBERS  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTSTAMP_MOD
      USE SENSOR_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NTYP,NI,IPARI(*),NOM_OPT(LNOPT1,*)
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,IERR1,IERR2
      CHARACTER(LEN=NCHARTITLE) :: TITR
C
C=======================================================================
C
      ID  = NOM_OPT(1,NI)
      CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,NI),LTITR)
C
C------------------------------------------------
C     SENSOR RENUMBERING FOR INTERFACE ACTIVATION
C-------------------------------------------
C
      IF(NTYP==5.OR.NTYP==7.OR.NTYP==11.OR.NTYP==19
     .          .OR.NTYP==21.OR.NTYP==24.OR.NTYP==25) THEN
C
         IERR1 = 0
         IF(IPARI(64)/=0) THEN
              IERR1 = 1
            DO J=1,SENSORS%NSENSOR
               IF (SENSORS%SENSOR_TAB(J)%SENS_ID == IPARI(64) ) THEN  
                  IPARI(64) = J
                  IERR1 = 0
               ENDIF
            ENDDO
         ENDIF
C                
         IF(IERR1 == 1) THEN    
            CALL ANCMSG(MSGID=1010,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=ID,
     .              C1= TITR,
     .              I2=IPARI(64))
         ENDIF
C
      ENDIF
C
      RETURN
C-----
      END
C
C=======================================================================
C
