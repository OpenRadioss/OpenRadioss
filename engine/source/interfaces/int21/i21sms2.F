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
!||    i21sms2       ../engine/source/interfaces/int21/i21sms2.F
!||--- called by ------------------------------------------------------
!||    i21mainf      ../engine/source/interfaces/int21/i21mainf.F
!||--- calls      -----------------------------------------------------
!||    ancmsg        ../engine/source/output/message/message.F
!||    arret         ../engine/source/system/arret.F
!||--- uses       -----------------------------------------------------
!||    message_mod   ../engine/share/message_module/message_mod.F
!||    tri7box       ../engine/share/modules/tri7box.F
!||====================================================================
      SUBROUTINE I21SMS2(JLT   ,MSTR         ,NSVG ,NIN   ,NOINT ,
     2                   MSKYI_SMS ,ISKYI_SMS,NSMS ,KT    ,C     ,
     3                   DTI   )
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
#include      "parit_c.inc"
#include      "task_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT, NIN, NOINT, MSTR, NSVG(*), NSMS(*), 
     .        ISKYI_SMS(LSKYI_SMS,*)
      my_real
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
       IF(NSMS(I)==0) CYCLE
       NISKYL1 = NISKYL1 + 1
      ENDDO
#include "lockon.inc"
      NISKYL     = NISKY_SMS
      NISKY_SMS  = NISKY_SMS + NISKYL1
#include "lockoff.inc"
C
      IF (NISKYL+NISKYL1 > LSKYI_SMS) THEN
         CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
         CALL ARRET(2)
      ENDIF
C
      DO I=1,JLT
       IF(NSMS(I)==0) CYCLE
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
       NISKYL=NISKYL+1
       MSKYI_SMS(NISKYL)=MAS
       ISKYI_SMS(NISKYL,1)=IG
       ISKYI_SMS(NISKYL,2)=MSTR
       ISKYI_SMS(NISKYL,3)=ISPMD+1
      ENDDO                
C      
      RETURN
      END
C
