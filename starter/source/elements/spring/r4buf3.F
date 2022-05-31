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
C
Chd|====================================================================
Chd|  R4BUF3                        source/elements/spring/r4buf3.F
Chd|-- called by -----------
Chd|        RINIT3                        source/elements/spring/rinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE R4BUF3(OFF  ,GEO  ,X     ,X0    ,Y0    ,
     2                  Z0   ,IX   ,SKEW  ,RLOC  ,IPOSX ,
     3                  IPOSY,IPOSZ,IPOSXX,IPOSYY,IPOSZZ,
     4                  ITAB ,EINT6,IGEO  ,IPM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "random_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IX(NIXR,*),ITAB(*),IGEO(NPROPGI,*),IPM(NPROPMI,*)
      my_real
     .   OFF(*), GEO(NPROPG,*), X(3,*), X0(*), Y0(*), Z0(*), SKEW(LSKEW,*)
      my_real
     .   RLOC(3,*),IPOSX(5,*) ,IPOSY(5,*),
     .   IPOSZ(5,*),IPOSXX(5,*),IPOSYY(5,*),IPOSZZ(5,*), EINT6(6,*),
     .   X1PHI,Y1PHI,Z1PHI
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NG, I1, I2, I3, ISK, IALIGN, K, USENS, MID, MTYP, IGTYP
C     REAL ou REAL*8
      my_real
     .   X1, Y1, Z1,
     .   NRLOC(MVSIZ),PRVC(3,MVSIZ),NPRVC(MVSIZ)
      my_real
     .     NOISE
C-----------------------------------------------
      NOISE = TWO*SQRT(THREE)*XALEA
C
      DO I=LFT,LLT
        J=I+NFT
        USENS=IGEO(3,IX(1,J))
        IF (USENS <= 0) THEN
C         no sensor or Isflag=1
          OFF(I)=ONE
        ELSE
          OFF(I)=-TEN
        ENDIF
      ENDDO
C
      IF (CODVERS >= 44) THEN
        DO J=1,6
          DO I=LFT,LLT
            EINT6(J,I)=ZERO
          ENDDO
        ENDDO
      ENDIF
C
      DO J=1,5
        DO I=LFT,LLT
          IPOSX(J,I)=ZERO
          IPOSY(J,I)=ZERO
          IPOSZ(J,I)=ZERO
          IPOSXX(J,I)=ZERO
          IPOSYY(J,I)=ZERO
          IPOSZZ(J,I)=ZERO
        ENDDO
      ENDDO
C
      DO I=LFT,LLT
        J=I+NFT
        NG=IX(1,J)
        ISK=IGEO(2,NG)
        I1=IX(2,J)
        I2=IX(3,J)
        I3=IX(4,J)
        X1=X(1,I2)-X(1,I1)
        Y1=X(2,I2)-X(2,I1)
        Z1=X(3,I2)-X(3,I1)
        X0(I)=SQRT(X1**2+Y1**2+Z1**2)
        IALIGN=0
        IF (X0(I) < EM15 .OR. X0(I) <= NOISE) THEN
C         IWARN=IWARN+1
         RLOC(1,I)= ONE
         RLOC(2,I)= ZERO
         RLOC(3,I)= ZERO
C       WRITE(ISTDO,*) '** WARNING: SPRING LENGTH IS NULL',
C     .                ', CANNOT DEFINE FRAME'
C       WRITE(IOUT,1000)IX(NIXR,J)
C
          IGTYP = IGEO(11,IX(1,J))
          IF (IGTYP == 23) THEN
            MTYP = IPM(2,IX(5,J))
          ELSE
            MTYP = 0
          ENDIF
C
          IF (MTYP /= 114) THEN
C--         message deactivated for seatbelts
            CALL ANCMSG(MSGID=325,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=IX(NIXR,J))
          ENDIF
        ELSE
          IF (I3 /= 0) THEN
            RLOC(1,I)=X(1,I3)-X(1,I1)
            RLOC(2,I)=X(2,I3)-X(2,I1)
            RLOC(3,I)=X(3,I3)-X(3,I1)
            NRLOC(I)=RLOC(1,I)**2+RLOC(2,I)**2+RLOC(3,I)**2
            PRVC(1,I)=Y1*RLOC(3,I)-Z1*RLOC(2,I)
            PRVC(2,I)=Z1*RLOC(1,I)-X1*RLOC(3,I)
            PRVC(3,I)=X1*RLOC(2,I)-Y1*RLOC(1,I)
            NPRVC(I)=PRVC(1,I)**2+PRVC(2,I)**2+PRVC(3,I)**2
            IF (SQRT(NPRVC(I))/NRLOC(I)/X0(I) < EM5) THEN
C         IWARN=IWARN+1
C         WRITE(ISTDO,*) '** WARNING: THREE SPRING NODES ON A LINE',
C     .                  ', CANNOT DEFINE FRAME'
              CALL ANCMSG(MSGID=326,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=IX(NIXR,J),
     .                    I2=ITAB(I1),
     .                    I3=ITAB(I2),
     .                    I4=ITAB(I3))
              IF (ISK /= 1) THEN
                RLOC(1,I)=SKEW(4,ISK)
                RLOC(2,I)=SKEW(5,ISK)
                RLOC(3,I)=SKEW(6,ISK)
                NRLOC(I)=RLOC(1,I)**2+RLOC(2,I)**2+RLOC(3,I)**2
                PRVC(1,I)=Y1*RLOC(3,I)-Z1*RLOC(2,I)
                PRVC(2,I)=Z1*RLOC(1,I)-X1*RLOC(3,I)
                PRVC(3,I)=X1*RLOC(2,I)-Y1*RLOC(1,I)
                NPRVC(I)=PRVC(1,I)**2+PRVC(2,I)**2+PRVC(3,I)**2
                IF (SQRT(NPRVC(I)/NRLOC(I))/X0(I) < EM5) THEN
C           WRITE(ISTDO,*) '** WARNING: SECOND AXIS OF SKEW FRAME',
C     .     '  IS PARALLEL TO SPRING AXIS, CANNOT DEFINE FRAME'
C           WRITE(IOUT,1200)IX(NIXR,J)
                CALL ANCMSG(MSGID=327,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=IX(NIXR,J))
                ELSE
                  WRITE(IOUT,1300)IX(NIXR,J)
                  RLOC(1,I)=PRVC(2,I)*Z1-PRVC(3,I)*Y1
                  RLOC(2,I)=PRVC(3,I)*X1-PRVC(1,I)*Z1
                  RLOC(3,I)=PRVC(1,I)*Y1-PRVC(2,I)*X1
                  NRLOC(I)=SQRT(RLOC(1,I)**2+RLOC(2,I)**2+RLOC(3,I)**2)
                  RLOC(1,I)=RLOC(1,I)/NRLOC(I)
                  RLOC(2,I)=RLOC(2,I)/NRLOC(I)
                  RLOC(3,I)=RLOC(3,I)/NRLOC(I)
                  IALIGN=1
                ENDIF
              ENDIF
            ELSE
              RLOC(1,I)=PRVC(2,I)*Z1-PRVC(3,I)*Y1
              RLOC(2,I)=PRVC(3,I)*X1-PRVC(1,I)*Z1
              RLOC(3,I)=PRVC(1,I)*Y1-PRVC(2,I)*X1
              NRLOC(I)=SQRT(RLOC(1,I)**2+RLOC(2,I)**2+RLOC(3,I)**2)
              RLOC(1,I)=RLOC(1,I)/NRLOC(I)
              RLOC(2,I)=RLOC(2,I)/NRLOC(I)
              RLOC(3,I)=RLOC(3,I)/NRLOC(I)
              IALIGN=1
            ENDIF
          ELSEIF (ISK /= 1) THEN
            RLOC(1,I)=SKEW(4,ISK)
            RLOC(2,I)=SKEW(5,ISK)
            RLOC(3,I)=SKEW(6,ISK)
            NRLOC(I)=RLOC(1,I)**2+RLOC(2,I)**2+RLOC(3,I)**2
            PRVC(1,I)=Y1*RLOC(3,I)-Z1*RLOC(2,I)
            PRVC(2,I)=Z1*RLOC(1,I)-X1*RLOC(3,I)
            PRVC(3,I)=X1*RLOC(2,I)-Y1*RLOC(1,I)
            NPRVC(I)=PRVC(1,I)**2+PRVC(2,I)**2+PRVC(3,I)**2
            IF (SQRT(NPRVC(I)/NRLOC(I))/X0(I) < EM5) THEN
C         WRITE(ISTDO,*) '** WARNING: SECOND AXIS OF SKEW FRAME',
C     .   '  IS PARALLEL TO SPRING AXIS, CANNOT DEFINE FRAME'
C         WRITE(IOUT,1200)IX(NIXR,J)
              CALL ANCMSG(MSGID=327,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=IX(NIXR,J))
            ELSE
              WRITE(IOUT,1300)IX(NIXR,J)
              RLOC(1,I)=PRVC(2,I)*Z1-PRVC(3,I)*Y1
              RLOC(2,I)=PRVC(3,I)*X1-PRVC(1,I)*Z1
              RLOC(3,I)=PRVC(1,I)*Y1-PRVC(2,I)*X1
              NRLOC(I)=SQRT(RLOC(1,I)**2+RLOC(2,I)**2+RLOC(3,I)**2)
              RLOC(1,I)=RLOC(1,I)/NRLOC(I)
              RLOC(2,I)=RLOC(2,I)/NRLOC(I)
              RLOC(3,I)=RLOC(3,I)/NRLOC(I)
              IALIGN=1
            ENDIF
          ENDIF ! IF (I3 /= 0)
C
          IF (IALIGN /= 1)THEN
            IF (ABS(Y1) < HALF*X0(I)) THEN
              RLOC(1,I)=ZERO
              RLOC(2,I)=ONE
              RLOC(3,I)=ZERO
              WRITE(IOUT,1400)IX(NIXR,J)
            ELSE
              RLOC(1,I)=ONE
              RLOC(2,I)=ZERO
              RLOC(3,I)=ZERO
              WRITE(IOUT,1450)IX(NIXR,J)
            ENDIF
          ENDIF ! IF (IALIGN /= 1)
        ENDIF ! IF (X0(I) < EM15 .OR. X0(I) <= NOISE)
      ENDDO
C-----------------------------------------------
      RETURN
C-----------------------------------------------
 1300     FORMAT(/,'  ** INFO: SPRING ELEMENT:',I10,/,
     .           ' SECOND AXIS OF SKEW FRAME AND SPRING AXIS ARE USED',
     .           ' TO DEFINE SPRING FRAME')
 1400     FORMAT(/,'  ** INFO: SPRING ELEMENT:',I10,/,
     .           ' GLOBAL Y AXIS AND SPRING AXIS ARE USED',
     .           ' TO DEFINE SPRING FRAME'/)
 1450     FORMAT(/,'  ** INFO: SPRING ELEMENT:',I10,/,
     .           ' GLOBAL X AXIS AND SPRING AXIS ARE USED',
     .           ' TO DEFINE SPRING FRAME'/)
C-----------------------------------------------
      END
