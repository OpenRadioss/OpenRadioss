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
Chd|  I10SMS2                       source/interfaces/int10/i10sms2.F
Chd|-- called by -----------
Chd|        I10FOR3                       source/interfaces/int10/i10for3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I10SMS2(JLT   ,IX1   ,IX2  ,IX3  ,IX4   ,
     2                  NSVG  ,H1    ,H2   ,H3   ,H4    ,STIF   ,
     3                  NIN   ,NOINT ,MSKYI_SMS ,ISKYI_SMS,NSMS ,
     4                  KT    ,C     ,DTI  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "sms_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,NIN,NOINT,
     .        IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),NSVG(MVSIZ),
     .        NSMS(*), ISKYI_SMS(LSKYI_SMS,*)
      my_real
     .    H1(MVSIZ),H2(MVSIZ),H3(MVSIZ),H4(MVSIZ),STIF(MVSIZ),
     .    MSKYI_SMS(*), KT(MVSIZ), C(MVSIZ), DTI
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG, NISKYL1, NISKYL, NN
      my_real
     .        MAS, DTS
C
C
      NISKYL1 = 0
      DO I=1,JLT
       IF(NSMS(I)==0.OR.STIF(I)==ZERO) CYCLE
       IF (H1(I)/=ZERO) NISKYL1 = NISKYL1 + 1
       IF (H2(I)/=ZERO) NISKYL1 = NISKYL1 + 1
       IF (H3(I)/=ZERO) NISKYL1 = NISKYL1 + 1
       IF (H4(I)/=ZERO) NISKYL1 = NISKYL1 + 1
      ENDDO
#include "lockon.inc"
      NISKYL     = NISKY_SMS
      NISKY_SMS  = NISKY_SMS + NISKYL1
#include "lockoff.inc"
C
      IF (NISKYL+NISKYL1 > LSKYI_SMS) THEN
        CALL ANCMSG(MSGID=26,ANMODE=ANINFO_BLIND)         
        CALL ARRET(2)
      ENDIF
C
      DO I=1,JLT
       IF(NSMS(I)==0.OR.STIF(I)==ZERO) CYCLE
C
       IF(NSMS(I)>0)THEN
         DTS = DTMINS/DTFACS
         DTI=MIN(DTI,DTMINS)
       ELSE
         DTS = DTMINS_INT/DTFACS_INT
         DTI=MIN(DTI,DTMINS_INT)
       END IF
C
       MAS= HALF * DTS * ( DTS * KT(I) + C(I) )
C
       IG =NSVG(I)
       IF(IG >  0)THEN
        IF(H1(I)/=ZERO)THEN
         NISKYL=NISKYL+1
         MSKYI_SMS(NISKYL)=ABS(H1(I))*MAS
         ISKYI_SMS(NISKYL,1)=IG
         ISKYI_SMS(NISKYL,2)=IX1(I)
         ISKYI_SMS(NISKYL,3)=ISPMD+1
        END IF
        IF(H2(I)/=ZERO)THEN
         NISKYL=NISKYL+1
         MSKYI_SMS(NISKYL)=ABS(H2(I))*MAS
         ISKYI_SMS(NISKYL,1)=IG
         ISKYI_SMS(NISKYL,2)=IX2(I)
         ISKYI_SMS(NISKYL,3)=ISPMD+1
        END IF
        IF(H3(I)/=ZERO)THEN
         NISKYL=NISKYL+1
         MSKYI_SMS(NISKYL)=ABS(H3(I))*MAS
         ISKYI_SMS(NISKYL,1)=IG
         ISKYI_SMS(NISKYL,2)=IX3(I)
         ISKYI_SMS(NISKYL,3)=ISPMD+1
        END IF
        IF(H4(I)/=ZERO)THEN
         NISKYL=NISKYL+1
         MSKYI_SMS(NISKYL)=ABS(H4(I))*MAS
         ISKYI_SMS(NISKYL,1)=IG
         ISKYI_SMS(NISKYL,2)=IX4(I)
         ISKYI_SMS(NISKYL,3)=ISPMD+1
        END IF
       ELSE
        NN = -IG
        IF(H1(I)/=ZERO)THEN
         NISKYL=NISKYL+1
         MSKYI_SMS(NISKYL)=ABS(H1(I))*MAS
         ISKYI_SMS(NISKYL,1)=NODAMSFI(NIN)%P(NN)
         ISKYI_SMS(NISKYL,2)=IX1(I)
         ISKYI_SMS(NISKYL,3)=PROCAMSFI(NIN)%P(NN)
        END IF
        IF(H2(I)/=ZERO)THEN
         NISKYL=NISKYL+1
         MSKYI_SMS(NISKYL)=ABS(H2(I))*MAS
         ISKYI_SMS(NISKYL,1)=NODAMSFI(NIN)%P(NN)
         ISKYI_SMS(NISKYL,2)=IX2(I)
         ISKYI_SMS(NISKYL,3)=PROCAMSFI(NIN)%P(NN)
        END IF
        IF(H3(I)/=ZERO)THEN
         NISKYL=NISKYL+1
         MSKYI_SMS(NISKYL)=ABS(H3(I))*MAS
         ISKYI_SMS(NISKYL,1)=NODAMSFI(NIN)%P(NN)
         ISKYI_SMS(NISKYL,2)=IX3(I)
         ISKYI_SMS(NISKYL,3)=PROCAMSFI(NIN)%P(NN)
        END IF
        IF(H4(I)/=ZERO)THEN
         NISKYL=NISKYL+1
         MSKYI_SMS(NISKYL)=ABS(H4(I))*MAS
         ISKYI_SMS(NISKYL,1)=NODAMSFI(NIN)%P(NN)
         ISKYI_SMS(NISKYL,2)=IX4(I)
         ISKYI_SMS(NISKYL,3)=PROCAMSFI(NIN)%P(NN)
        END IF
       END IF
      ENDDO                
C      
      RETURN
      END
C
