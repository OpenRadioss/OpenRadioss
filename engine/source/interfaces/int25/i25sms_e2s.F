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
Chd|  I25SMS_E2S                    source/interfaces/int25/i25sms_e2s.F
Chd|-- called by -----------
Chd|        I25FOR3_E2S                   source/interfaces/int25/i25for3_e2s.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I25SMS_E2S(JLT   ,CS_LOC ,N1    ,N2     ,M1     ,
     2                    M2    ,HS1    ,HS2   ,HM1    ,HM2    ,
     3               STIF  ,NIN    ,NOINT ,MSKYI_SMS ,ISKYI_SMS,
     4               NSMS  ,K1     ,K2    ,K3     ,K4     ,
     5               C1    ,C2     ,C3    ,C4     ,NEDGE   , EDGE_ID)
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
#include      "assert.inc"
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
      INTEGER, INTENT(IN) :: EDGE_ID(2,4*MVSIZ)
      INTEGER JLT, NIN, NOINT, CS_LOC(*), 
     .        N1(*), N2(*), M1(*), M2(*),
     .        NSMS(4*MVSIZ), ISKYI_SMS(LSKYI_SMS,*), NEDGE
      my_real
     .    HS1(*),HS2(*),HM1(*),HM2(*),STIF(*), MSKYI_SMS(*), 
     .    K1(*), K2(*), K3(*), K4(*), 
     .    C1(*), C2(*), C3(*), C4(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IG, NISKYL1, NISKYL, NN
      my_real
     .        MAS1, MAS2, MAS3, MAS4, DTS
C
C
      NISKYL1 = 0
      DO I=1,JLT
        IF(NSMS(I)==0.OR.STIF(I)==ZERO) CYCLE
        IF (HM1(I)/=ZERO) NISKYL1 = NISKYL1 + 2
        IF (HM2(I)/=ZERO) NISKYL1 = NISKYL1 + 2
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
       DEBUG_E2E(EDGE_ID(1,I)==D_EM .AND. EDGE_ID(2,I) ==  D_ES,NSMS(I))

       IF(NSMS(I)==0.OR.STIF(I)==ZERO) CYCLE
C
       IF(NSMS(I)>0)THEN
         ASSERT(DTFACS > 0) 
         DTS = DTMINS/DTFACS
       ELSE
         DTS = DTMINS_INT/DTFACS_INT
       END IF
C
       MAS1= HALF * DTS * ( DTS * K1(I) + C1(I) )
       MAS2= HALF * DTS * ( DTS * K2(I) + C2(I) )
       MAS3= HALF * DTS * ( DTS * K3(I) + C3(I) )
       MAS4= HALF * DTS * ( DTS * K4(I) + C4(I) )
C
       IF(CS_LOC(I)<=NEDGE) THEN
        IF(HM1(I)/=ZERO)THEN
         NISKYL=NISKYL+1
         MSKYI_SMS(NISKYL)=MAX(MAS1,MAS3)
         ISKYI_SMS(NISKYL,1)=N1(I)
         ISKYI_SMS(NISKYL,2)=M1(I)
         ISKYI_SMS(NISKYL,3)=ISPMD+1
         NISKYL=NISKYL+1
         MSKYI_SMS(NISKYL)=MAX(MAS2,MAS3)
         ISKYI_SMS(NISKYL,1)=N2(I)
         ISKYI_SMS(NISKYL,2)=M1(I)
         ISKYI_SMS(NISKYL,3)=ISPMD+1
        END IF
        IF(HM2(I)/=ZERO)THEN
         NISKYL=NISKYL+1
         MSKYI_SMS(NISKYL)=MAX(MAS1,MAS4)
         ISKYI_SMS(NISKYL,1)=N1(I)
         ISKYI_SMS(NISKYL,2)=M2(I)
         ISKYI_SMS(NISKYL,3)=ISPMD+1
         NISKYL=NISKYL+1
         MSKYI_SMS(NISKYL)=MAX(MAS2,MAS4)
         ISKYI_SMS(NISKYL,1)=N2(I)
         ISKYI_SMS(NISKYL,2)=M2(I)
         ISKYI_SMS(NISKYL,3)=ISPMD+1
        END IF
       ELSE
        IF(HM1(I)/=ZERO)THEN
         NISKYL=NISKYL+1
         MSKYI_SMS(NISKYL)=MAX(MAS1,MAS3)
         ISKYI_SMS(NISKYL,1)=NODAMSFIE(NIN)%P(N1(I))
         ISKYI_SMS(NISKYL,2)=M1(I)
         ISKYI_SMS(NISKYL,3)=PROCAMSFIE(NIN)%P(N1(I))
         NISKYL=NISKYL+1
         MSKYI_SMS(NISKYL)=MAX(MAS2,MAS3)
         ISKYI_SMS(NISKYL,1)=NODAMSFIE(NIN)%P(N2(I))
         ISKYI_SMS(NISKYL,2)=M1(I)
         ISKYI_SMS(NISKYL,3)=PROCAMSFIE(NIN)%P(N2(I))
        END IF
        IF(HM2(I)/=ZERO)THEN
         NISKYL=NISKYL+1
         MSKYI_SMS(NISKYL)=MAX(MAS1,MAS4)
         ISKYI_SMS(NISKYL,1)=NODAMSFIE(NIN)%P(N1(I))
         ISKYI_SMS(NISKYL,2)=M2(I)
         ISKYI_SMS(NISKYL,3)=PROCAMSFIE(NIN)%P(N1(I))
         NISKYL=NISKYL+1
         MSKYI_SMS(NISKYL)=MAX(MAS2,MAS4)
         ISKYI_SMS(NISKYL,1)=NODAMSFIE(NIN)%P(N2(I))
         ISKYI_SMS(NISKYL,2)=M2(I)
         ISKYI_SMS(NISKYL,3)=PROCAMSFIE(NIN)%P(N2(I))
        END IF
       END IF
      ENDDO                
C      
      RETURN
      END
