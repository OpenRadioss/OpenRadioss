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
C
Chd|====================================================================
Chd|  CDK6INX                       source/elements/sh3n/coquedk6/cdk6inx.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE CDK6INX(IXTG,IXTG1,ICNOD)
      USE MESSAGE_MOD
C--------------------------------------------------------
C     Construction les connectivites sup DES ELEMENTS COQUES TRIANGULAIRE 
C--------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C      V a r i a b l e s
C-----------------------------------------------
      INTEGER IXTG(NIXTG,*),IXTG1(4,*),ICNOD(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NEL1,N1,N2,N3,NJ1,NJ2,NJ3,IMIN,IMAX,JMIN,JMAX,
     .        II,JJ
C-----------------------------------------------
C
      NEL1=0
      DO I=1,NUMELTG
       IF (ICNOD(I)==6) THEN
        NEL1 = NEL1+1
        IXTG1(1,NEL1) = 0
        IXTG1(2,NEL1) = 0
        IXTG1(3,NEL1) = 0
        IXTG1(4,NEL1) = I
       ENDIF
      ENDDO
      IF (NEL1/=NUMELTG6) THEN
       CALL ANCMSG(MSGID=364,
     *             ANMODE=ANSTOP,
     *             MSGTYPE=MSGERROR,C1='S3N6')
      ENDIF
C
      DO I=1,NUMELTG6
       IF (IXTG1(1,I)==0.OR.IXTG1(2,I)==0
     .     .OR.IXTG1(3,I)==0) THEN
        II=IXTG1(4,I)
        N1= IXTG(2,II)  
        N2= IXTG(3,II)  
        N3= IXTG(4,II)
C        IMIN= MIN(N1,N2,N3)
C        IMAX= MAX(N1,N2,N3)
        DO J =I+1,NUMELTG6 
         JJ=IXTG1(4,J)
         NJ1= IXTG(2,JJ)  
         NJ2= IXTG(3,JJ)  
         NJ3= IXTG(4,JJ)
C         JMIN= MIN(NJ1,NJ2,NJ3)
C         JMAX= MAX(NJ1,NJ2,NJ3)
C---------   ligne 1----------
          IF (IXTG1(1,I)==0) THEN
           IF ((N1+N2)==(NJ1+NJ2)) THEN
            IF (ABS(N1-N2)==ABS(NJ1-NJ2)) THEN
             IXTG1(1,I) = NJ3
             IXTG1(1,J) = N3
            ENDIF 
           ELSEIF ((N1+N2)==(NJ2+NJ3)) THEN
            IF (ABS(N1-N2)==ABS(NJ2-NJ3)) THEN
             IXTG1(1,I) = NJ1
             IXTG1(2,J) = N3
            ENDIF 
           ELSEIF ((N1+N2)==(NJ3+NJ1)) THEN
            IF (ABS(N1-N2)==ABS(NJ3-NJ1)) THEN
             IXTG1(1,I) = NJ2
             IXTG1(3,J) = N3
            ENDIF 
           ENDIF 
          ENDIF 
C---------   ligne 2----------
          IF (IXTG1(2,I)==0) THEN
           IF ((N2+N3)==(NJ1+NJ2)) THEN
            IF (ABS(N2-N3)==ABS(NJ1-NJ2)) THEN
             IXTG1(2,I) = NJ3
             IXTG1(1,J) = N1
            ENDIF 
           ELSEIF ((N2+N3)==(NJ2+NJ3)) THEN
            IF (ABS(N2-N3)==ABS(NJ2-NJ3)) THEN
             IXTG1(2,I) = NJ1
             IXTG1(2,J) = N1
            ENDIF 
           ELSEIF ((N2+N3)==(NJ3+NJ1)) THEN
            IF (ABS(N2-N3)==ABS(NJ3-NJ1)) THEN
             IXTG1(2,I) = NJ2
             IXTG1(3,J) = N1
            ENDIF 
           ENDIF 
          ENDIF 
C---------   ligne 3----------
          IF (IXTG1(3,I)==0) THEN
           IF ((N1+N3)==(NJ1+NJ2)) THEN
            IF (ABS(N1-N3)==ABS(NJ1-NJ2)) THEN
             IXTG1(3,I) = NJ3
             IXTG1(1,J) = N2
            ENDIF 
           ELSEIF ((N1+N3)==(NJ2+NJ3)) THEN
            IF (ABS(N1-N3)==ABS(NJ2-NJ3)) THEN
             IXTG1(3,I) = NJ1
             IXTG1(2,J) = N2
            ENDIF 
           ELSEIF ((N1+N3)==(NJ3+NJ1)) THEN
            IF (ABS(N1-N3)==ABS(NJ3-NJ1)) THEN
             IXTG1(3,I) = NJ2
             IXTG1(3,J) = N2
            ENDIF 
           ENDIF 
          ENDIF 
        ENDDO 
C        write(*,*)I,IXTG1(1,I),IXTG1(2,I),IXTG1(3,I)
       ENDIF 
      ENDDO
      RETURN
      END
