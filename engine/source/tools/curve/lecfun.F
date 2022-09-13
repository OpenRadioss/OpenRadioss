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
Chd|  LECFUN                        source/tools/curve/lecfun.F   
Chd|-- called by -----------
Chd|        LECTUR                        source/input/lectur.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE LECFUN(NPC,PLD,NFCT,NPTST,TABLE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFCT, NPTST
      INTEGER NPC(*)
C     REAL
      my_real
     .   PLD(*)
      TYPE(TTABLE) TABLE(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER L, LL, NPTS, I, J, IK, K, LLNEW, NPTSNEW,
     .        OK
C     REAL
      my_real
     .   TIME, FUNCT
C-----------------------------------------------
      IF(ISPMD==0)
     .  WRITE (IOUT,2000) NFCT
C
      DO K=1,NFCT
        READ (IIN,'(2I10)') LLNEW,NPTSNEW
        IF(ISPMD==0)
     .    WRITE (IOUT,2200) LLNEW,NPTSNEW
        OK=0
        DO L=1,NFUNCT
         LL=NPC(NFUNCT+L+1)
         IF(LL==LLNEW)THEN
          OK=1
          NPTS = (NPC(L+1)-NPC(L))/2
          IF(NPTS==NPTSNEW)THEN
            I=NPC(L)
            DO J=1,NPTSNEW
C
              READ(IIN,'(2F16.0)')         TIME,FUNCT
              IF(ISPMD==0)
     .          WRITE(IOUT,'(3X,G12.4,2X,G12.4)') TIME,FUNCT
C
              PLD(I)=TIME
              IF(J>1.AND.PLD(I)<=PLD(I-2)) THEN
                IERR=IERR+1
                IK=J/2+1
                IF(ISPMD==0)
     .          CALL ANCMSG(MSGID=105,ANMODE=ANINFO_BLIND,
     .            I1=LL,I2=IK,I3=IK-1)
                CALL ARRET(2)
              ENDIF
              I=I+1
              PLD(I)=FUNCT
              I=I+1
            ENDDO
          ELSE
            IERR=IERR+1
            IF(ISPMD==0)THEN
              CALL ANCMSG(MSGID=106,ANMODE=ANINFO_BLIND,
     .            I1=LL)
            END IF
            CALL ARRET(2)
          ENDIF
         ENDIF
        ENDDO
        IF(OK==0)THEN
          IF(ISPMD==0)THEN
            IERR=IERR+1
            CALL ANCMSG(MSGID=107,ANMODE=ANINFO,
     .            I1=LL)
          END IF
        ENDIF
C
        OK=0
        DO L=1,NFUNCT
C
C par construction TABLE(L) == Function no L
         LL=TABLE(L)%NOTABLE
         IF(LL==LLNEW)THEN
          OK=1
          NPTS = SIZE(TABLE(L)%X(1)%VALUES)
          IF(NPTS==NPTSNEW)THEN
            I=NPC(L)
            DO J=1,NPTSNEW/2
              TABLE(L)%X(1)%VALUES(J)=PLD(I)
              IF(J>1.AND.
     .        TABLE(L)%X(1)%VALUES(J)<=TABLE(L)%X(1)%VALUES(J-1))THEN
                IERR=IERR+1
                IK=J
                IF(ISPMD==0)
     .           CALL ANCMSG(MSGID=105,ANMODE=ANINFO_BLIND,
     .            I1=LL,I2=IK,I3=IK-1)
                CALL ARRET(2)
              ENDIF
              I=I+1
              TABLE(L)%Y%VALUES(J)=PLD(I)
              I=I+1
            ENDDO
          ELSE
            IERR=IERR+1
            IF(ISPMD==0)THEN
              CALL ANCMSG(MSGID=106,ANMODE=ANINFO_BLIND,
     .            I1=LL)
            END IF
            CALL ARRET(2)
          ENDIF
         ENDIF
        ENDDO
        IF(OK==0)THEN
          IF(ISPMD==0)THEN
            IERR=IERR+1
            CALL ANCMSG(MSGID=107,ANMODE=ANINFO,
     .            I1=LL)
          END IF
        ENDIF
      ENDDO
C
      RETURN
C
2000  FORMAT(' NEW LOAD CURVES'  //
     .  '  NUMBER OF NEW LOAD CURVES. . . . . . =' ,I10 /)
2100  FORMAT(' ** ERROR: LOAD CURVE NUMBER' )
2200  FORMAT(///' LOAD FUNCTION NUMBER. . . . . =',I10/
     .          ' NUMBER OF TIME POINTS . . . . =',I8//
     .          ' TIME            VALUE          '   /)
      END
