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
Chd|  SCHKJAB3                      source/elements/solid/solide/sderi3.F
Chd|-- called by -----------
Chd|        FDERI3                        source/elements/solid/solide/fderi3.F
Chd|        S4DERIT3                      source/elements/solid/solide4/s4deri3.F
Chd|        S8EDERICT3                    source/elements/solid/solide8e/s8ederi3.F
Chd|        S8ZDERICT3                    source/elements/solid/solide8z/s8zderi3.F
Chd|        SDERIT3                       source/elements/solid/solide/sderi3.F
Chd|        SZDERIT3                      source/elements/solid/solidez/szderi3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SCHKJAB3(
     1   OFF,     DET,     NGL,     NEL)
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
      INTEGER :: NGL(*)
      my_real :: OFF(*),DET(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,ICOR
C-----------------------------------------------
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
              CALL ANCMSG(MSGID=166,ANMODE=ANINFO,I1=NGL(I))
              DET(I)= ONE
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
     .         (DET(I)<=VOLMIN.OR.DET(I)<=ZERO))THEN
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
              CALL ANCMSG(MSGID=166,ANMODE=ANINFO,I1=NGL(I))
              DET(I)= ONE
              MSTOP = 1
            ENDIF
           ENDIF
          ENDDO
        ENDIF
      ENDIF
      RETURN
C
 1000 FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB',I10/)
 2000 FORMAT(/' ZERO OR NEGATIVE VOLUME : DELETE 3D-ELEMENT NB',I10/)
      END
