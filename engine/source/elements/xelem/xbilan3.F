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
Chd|  XBILAN3                       source/elements/xelem/xbilan3.F
Chd|-- called by -----------
Chd|        XFORC3                        source/elements/xelem/xforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE XBILAN3(NX    ,KXX   ,IXX   ,X       ,V    ,
     2                   VR    ,UMASS ,UINER ,FORC    ,TORQ  ,
     3                   KEUSR , EUSR ,EINT  ,PARTSAV ,IPART ,
     4                   GRESAV,GRTH  ,IGRTH)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "scr23_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXX(NIXX),IXX(*),NX,KEUSR,IPART,GRTH(*),IGRTH(*)
C     REAL
      my_real
     .   X(3,*),V(3,*),VR(3,*),UMASS(*), UINER(*),FORC(3,*),
     .   TORQ(3,*),EUSR,EINT,PARTSAV(NPSAV,*),GRESAV(NPSAV,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, K, IP, I1, IADNOD,J
C     REAL
      my_real
     .   EMS, XI
C--------------------------------------------
      IADNOD=KXX(4)
      IF (KEUSR.EQ.0) THEN
       DO K=1,NX
        I1 = IXX(IADNOD+K-1)
        EMS=UMASS(K)
        XI =UINER(K)
C       ENERGY has not been provided by user routine.
Clm     Integration en rectangle ..
        EINT = EINT
     .   -DT1*(V(1,I1)*FORC(1,K)+V(2,I1)*FORC(2,K)+V(3,I1)*FORC(3,K)
     .     +VR(1,I1)*TORQ(1,K)+VR(2,I1)*TORQ(2,K)+VR(3,I1)*TORQ(3,K))
        IP=IPART
        PARTSAV(2,IP)=PARTSAV(2,IP) + HALF * EMS *
     .     (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1))
        PARTSAV(3,IP)=PARTSAV(3,IP) + EMS*V(1,I1)
        PARTSAV(4,IP)=PARTSAV(4,IP) + EMS*V(2,I1)
        PARTSAV(5,IP)=PARTSAV(5,IP) + EMS*V(3,I1)
        PARTSAV(6,IP)=PARTSAV(6,IP) + EMS
        PARTSAV(7,IP)=PARTSAV(7,IP) + HALF * XI *
     .     (VR(1,I1)*VR(1,I1)+VR(2,I1)*VR(2,I1)+VR(3,I1)*VR(3,I1))
       ENDDO
       IF (IGRE /= 0) THEN
         IF (IGRTH(1) /= IGRTH(2)) THEN
           DO J = IGRTH(1),IGRTH(2)-1
             GRESAV(1,GRTH(J)) = GRESAV(1,GRTH(J)) + EINT
             GRESAV(2,GRTH(J)) = GRESAV(2,GRTH(J)) + HALF * EMS *
     .     (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1))
             GRESAV(3,GRTH(J)) = GRESAV(3,GRTH(J)) + EMS*V(1,I1)
             GRESAV(4,GRTH(J)) = GRESAV(4,GRTH(J)) + EMS*V(2,I1)
             GRESAV(5,GRTH(J)) = GRESAV(5,GRTH(J)) + EMS*V(3,I1)
             GRESAV(6,GRTH(J)) = GRESAV(6,GRTH(J)) + EMS
           ENDDO
         ENDIF
       ENDIF
      ELSE
       DO K=1,NX
        I1 = IXX(IADNOD+K-1)
        EMS=UMASS(K)
        XI =UINER(K)
        IP=IPART
        PARTSAV(2,IP)=PARTSAV(2,IP) + HALF * EMS *
     .     (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1))
        PARTSAV(3,IP)=PARTSAV(3,IP) + EMS*V(1,I1)
        PARTSAV(4,IP)=PARTSAV(4,IP) + EMS*V(2,I1)
        PARTSAV(5,IP)=PARTSAV(5,IP) + EMS*V(3,I1)
        PARTSAV(6,IP)=PARTSAV(6,IP) + EMS
        PARTSAV(7,IP)=PARTSAV(7,IP) + HALF * XI *
     .     (VR(1,I1)*VR(1,I1)+VR(2,I1)*VR(2,I1)+VR(3,I1)*VR(3,I1))
       ENDDO
       IF (IGRE /= 0) THEN
         IF (IGRTH(1) /= IGRTH(2)) THEN
           DO J = IGRTH(1),IGRTH(2)-1
             GRESAV(1,GRTH(J)) = GRESAV(1,GRTH(J)) + EINT
             GRESAV(2,GRTH(J)) = GRESAV(2,GRTH(J)) + HALF * EMS *
     .     (V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1))
             GRESAV(3,GRTH(J)) = GRESAV(3,GRTH(J)) + EMS*V(1,I1)
             GRESAV(4,GRTH(J)) = GRESAV(4,GRTH(J)) + EMS*V(2,I1)
             GRESAV(5,GRTH(J)) = GRESAV(5,GRTH(J)) + EMS*V(3,I1)
             GRESAV(6,GRTH(J)) = GRESAV(6,GRTH(J)) + EMS
           ENDDO
         ENDIF
       ENDIF
       EINT=EUSR
      ENDIF
      PARTSAV(1,IP)=PARTSAV(1,IP) + EINT
C
      RETURN
      END
