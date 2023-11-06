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
Chd|  SCHKJABT3                     source/elements/solid/solide4/schkjabt3.F
Chd|-- called by -----------
Chd|        S4DERI3                       source/elements/solid/solide4/s4deri3.F
Chd|        S8EDERIC3                     source/elements/solid/solide8e/s8ederic3.F
Chd|        S8ZDERIC3                     source/elements/solid/solide8z/s8zderic3.F
Chd|        SDERI3                        source/elements/solid/solide/sderi3.F
Chd|        SZDERI3                       source/elements/solid/solidez/szderi3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SCHKJABT3(
     1   OFF,     DET,     NGL,     OFFG,
     2   NNEGA,   INDEX,   NEL,     ISMSTR,
     3   JLAG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
#include      "units_c.inc"
#include      "scr07_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN) :: NEL
        INTEGER, INTENT(IN) :: ISMSTR
        INTEGER, INTENT(IN) :: JLAG
        INTEGER NGL(*),NNEGA,INDEX(*)
C     REAL
        my_real
     .     OFF(*),DET(*),OFFG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J ,ICOR
C-----------------------------------------------
        NNEGA = 0
        IF(JLAG/=0)THEN
C--- case total strain switched to small due to nega_v before
          IF(ISMSTR==10.OR.(ISMSTR==12.AND.IDTMIN(1)/=3)) THEN
            DO I=1,NEL
              IF(OFFG(I) > ONE)THEN
                NNEGA=NNEGA+1
                INDEX(NNEGA)=I
              END IF
            ENDDO
          END IF
          ICOR = 0
          DO I=1,NEL
            IF(OFF(I) ==ZERO)THEN
              DET(I)=ONE
            ELSEIF(OFFG(I) > ONE)THEN
C
            ELSEIF((DET(I)<=VOLMIN).OR.(DET(I)<=ZERO))THEN
              ICOR = 1
            ENDIF
          ENDDO
          IF (ICOR>0.AND.INCONV==1) THEN
            DO I=1,NEL
              IF(DET(I)<=ZERO.AND.OFFG(I) <= ONE.AND.OFFG(I) /= ZERO)THEN
                NNEGA=NNEGA+1
                INDEX(NNEGA)=I
#include "lockon.inc"
                IF(ISMSTR<10) THEN
                  CALL ANCMSG(MSGID=259,ANMODE=ANINFO,
     .                        I1=NGL(I))
                ELSE
                  CALL ANCMSG(MSGID=261,ANMODE=ANINFO,
     .                        I1=NGL(I))
                END IF
#include "lockoff.inc"
              ENDIF
              IF (INEG_V==0) THEN
                CALL ANCMSG(MSGID=280,ANMODE=ANINFO)
                MSTOP = 1
              ENDIF
            ENDDO
          END IF !(ICOR>0.AND.IMCONV==1) THEN
        ELSE
          IF(IDTMIN(1)==1)THEN
            ICOR = 0
            DO I=1,NEL
              IF(OFF(I) ==ZERO)THEN
                DET(I)=1.
              ELSEIF((DET(I)<=VOLMIN).OR.(DET(I)<=ZERO))THEN
                ICOR = 1
              ENDIF
            ENDDO
            IF (ICOR>0.AND.IMCONV==1) THEN
              DO I=1,NEL
                IF(OFF(I)/=ZERO)THEN
                  IF(DET(I)<=VOLMIN)THEN
                    DET(I)=ONE
                    OFF(I)=ZERO
#include "lockon.inc"
                    WRITE(ISTDO,2000) NGL(I)
                    WRITE(IOUT ,2000) NGL(I)
#include "lockoff.inc"
                    IDEL7NOK = 1
                  ELSEIF(DET(I)<=ZERO)THEN
                    CALL ANCMSG(MSGID=166,ANMODE=ANINFO,
     .                          I1=NGL(I))
                    MSTOP = 1
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ELSEIF(IDTMIN(1)==2)THEN
            ICOR = 0
            DO I=1,NEL
              IF(OFF(I) ==ZERO)THEN
                DET(I)=ONE
              ELSEIF((DET(I)<=VOLMIN).OR.(DET(I)<=ZERO))THEN
                ICOR=1
              ENDIF
            ENDDO
            IF (ICOR>0.AND.IMCONV==1) THEN
              DO I=1,NEL
                IF((OFF(I)/=ZERO).AND.
     .             (DET(I)<=VOLMIN.OR.DET(I)<=ZERO))THEN
                  DET(I)=ONE
                  OFF(I)=ZERO
#include "lockon.inc"
                  WRITE(ISTDO,2000) NGL(I)
                  WRITE(IOUT ,2000) NGL(I)
#include "lockoff.inc"
                  IDEL7NOK = 1
                ENDIF
              ENDDO
            ENDIF
          ELSE
            ICOR = 0
            DO I=1,NEL
              IF(OFF(I) ==ZERO)THEN
                DET(I)=ONE
              ELSEIF(DET(I)<=ZERO)THEN
                ICOR=1
              ENDIF
            ENDDO
            IF (ICOR>0.AND.IMCONV==1) THEN
              DO I=1,NEL
                IF(OFF(I)/=ZERO)THEN
                  IF(DET(I)<=ZERO)THEN
                    CALL ANCMSG(MSGID=166,ANMODE=ANINFO,
     .                          I1=NGL(I))
                    MSTOP = 1
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        END IF!(JLAG/=0)THEN
        RETURN
C
 1000   FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB',I10/)
 2000   FORMAT(/' ZERO OR NEGATIVE VOLUME : DELETE 3D-ELEMENT NB',I10/)
 3000   FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB:',I10/,
     +          ' ELEMENT IS SWITCHED TO SMALL STRAIN OPTION'/)
 4000   FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB:',I10/,
     +          ' TOTAL STRAIN ELEMENT IS SWITCHED TO SMALL STRAIN OPTION'/)
      END
