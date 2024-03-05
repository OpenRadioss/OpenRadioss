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
C=======================================================================
Chd|====================================================================
Chd|  LECACC                        source/tools/accele/lecacc.F  
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ANODSET                       source/output/analyse/analyse_node.c
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        VDOUBLE                       source/system/sysfus.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE LECACC(LACCELM,ACCELM,ITABM1,UNITAB,IXC,
     .                  ISKN, NOM_OPT, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "analyse_name.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER ITABM1(*), LACCELM(3,*),
     .   IXC(NIXC,*),ISKN(LISKN,*)
      INTEGER NOM_OPT(LNOPT1,*)
C     REAL
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      my_real ACCELM(LLACCELM,*)
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, ID, NOD, ISK, UID, IG, L
      INTEGER N, NS
      my_real :: F, BID ,DIST
      CHARACTER MESS*40
      CHARACTER TITR*nchartitle, KEY2*ncharkey
      LOGICAL :: IS_AVAILABLE, FOUND
C-----------------------------------------------
C   D a t a
C-----------------------------------------------
      DATA MESS/'ACCELEROMETER DEFINITION                '/  
C---------------------------------------------------
C     B e g i n n i n g   o f   S u b r o u t i n e 
C---------------------------------------------------
      WRITE(ISTDO,'(A)')' .. ACCELEROMETERS'
      IS_AVAILABLE = .FALSE.
      CALL HM_OPTION_START('/ACCEL')
      DO I = 1, NACCELM
        CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_TITR = TITR, OPTION_ID = ID, UNIT_ID = UID)
        NOM_OPT(1, I) = ID
        CALL FRETITL(TITR, NOM_OPT(LNOPT1-LTITR+1, I), LTITR)
        FOUND = .FALSE.
          DO J=1,UNITAB%NUNITS
            IF (UNITAB%UNIT_ID(J) == UID) THEN  
              FOUND = .TRUE.  
              EXIT
           ENDIF
        ENDDO
        IF (.NOT. (UID == 0 .OR. FOUND)) THEN
           CALL ANCMSG(MSGID = 659, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .          C1 = 'ACCELEROMETER', C2 = 'ACCELEROMETER', C3 = TITR, 
     .          I2 = UID, I1 = ID)
        ENDIF
        DIST = ZERO
        CALL HM_GET_INTV('nodeid', NOD, IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_INTV('skewid', ISK, IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_FLOATV('cutoff', F, IS_AVAILABLE, LSUBMODEL, UNITAB)
C
        FOUND = .FALSE.
        DO J = 0, NUMSKW + MIN(1, NSPCOND) * NUMSPH + NSUBMOD
          IF(ISK == ISKN(4, J + 1)) THEN
            ISK = J + 1
            FOUND = .TRUE.
            EXIT
          ENDIF
        ENDDO
        IF (.NOT. FOUND) THEN
           CALL ANCMSG(MSGID = 137, ANMODE = ANINFO, MSGTYPE = MSGERROR,
     .          C1 = 'ACCELEROMETER', C2 = 'ACCELEROMETER', C3 = TITR,
     .          I1 = ID, I2 = ISK)
        ENDIF

C
        LACCELM(1,I)=USR2SYS(NOD,ITABM1,MESS,ID)
        CALL ANODSET(LACCELM(1,I), CHECK_USED)
        LACCELM(2,I)=ID
        LACCELM(3,I)=ISK
        ACCELM(1,I)=F
C-------------------------------------
        WRITE (IOUT,'(///,A)')'          ACCELEROMETER'
        WRITE (IOUT,'(A/)')   '          -------------'
        WRITE (IOUT,'(A,I10)')
     .    ' ACCELEROMETER NUMBER . . . . . . . . .',ID,
     .    ' NODE NUMBER. . . . . . . . . . . . . .',NOD,
     .    ' SKEW FRAME NUMBER. . . . . . . . . . .',ISKN(4,ISK)
        WRITE (IOUT,'(A,1PG20.13)')
     .    ' 4-POLE BUTTERWORTH CORNER FREQUENCY. .',F
      ENDDO
C-------------------------------------
C     Searching for duplicate ID
C-------------------------------------
      NACCELM=NACCELM
      CALL VDOUBLE(NOM_OPT,LNOPT1,NACCELM,MESS,0,BID)
C----
      RETURN
      END
