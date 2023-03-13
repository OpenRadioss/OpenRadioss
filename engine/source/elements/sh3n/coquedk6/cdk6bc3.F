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
Chd|  CDK6BC3                       source/elements/sh3n/coquedk6/cdk6bc3.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CDK6BC3(ICODR ,ISKEW ,ISKWN ,IXTG  ,IXTG1   ,
     1                   NPBY ,X     ,SKEW )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICODR(*),ISKEW(*),ISKWN(LISKN,*),IXTG(NIXTG,*),
     .        IXTG1(4,*),NPBY(NNPBY,*)
      my_real
     .  X(3,*),SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,IC1,IC2,IC3,N1,N2,N3,J1(3),IS,NELTG3
      my_real
     .  LX,LY,LZ,LL,LL1,E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z
C-----------------------------------------------
        NELTG3 = NUMELTG-NUMELTG6
       DO I=1,NUMELTG6
C        II=IXTG1(4,I)
        II = I + NELTG3
        N1=IXTG(2,II)
        N2=IXTG(3,II)
        N3=IXTG(4,II)
        IC1=ICODR(N1)
        IC2=ICODR(N2)
        IC3=ICODR(N3)
C--------ligne 1--------------
        IF (IC1>0.AND.IC2>0)THEN
         LX=X(1,N1)-X(1,N2)
         LY=X(2,N1)-X(2,N2)
         LZ=X(3,N1)-X(3,N2)
         LL =SQRT(LX*LX+LY*LY+LZ*LZ)
         J1(1)=IC1/4
         J1(2)=(IC1-4*J1(1))/2
         J1(3)=(IC1-4*J1(1)-2*J1(2))
         IS=ISKEW(N1)
         IF (IS==1) THEN
          E1X=SKEW(1,IS)
          E1Y=SKEW(2,IS)
          E1Z=SKEW(3,IS)
          E2X=SKEW(4,IS)
          E2Y=SKEW(5,IS)
          E2Z=SKEW(6,IS)
          E3X=SKEW(7,IS)
          E3Y=SKEW(8,IS)
          E3Z=SKEW(9,IS)
          LL1=LX*(E1X+E2X+E3X)+LY*(E1Y+E2Y+E3Y)+LZ*(E1Z+E2Z+E3Z)
         ELSE
          LL1=LX*J1(1)+LY*J1(2)+LZ*J1(3)
         ENDIF
         IF (ABS(LL1)/LL>EM6) THEN
          J1(1)=IC2/4
          J1(2)=(IC2-4*J1(1))/2
          J1(3)=(IC2-4*J1(1)-2*J1(2))
          IS=ISKEW(N2)
          IF (IS==1) THEN
           E1X=SKEW(1,IS)
           E1Y=SKEW(2,IS)
           E1Z=SKEW(3,IS)
           E2X=SKEW(4,IS)
           E2Y=SKEW(5,IS)
           E2Z=SKEW(6,IS)
           E3X=SKEW(7,IS)
           E3Y=SKEW(8,IS)
           E3Z=SKEW(9,IS)
           LL1=LX*(E1X+E2X+E3X)+LY*(E1Y+E2Y+E3Y)+LZ*(E1Z+E2Z+E3Z)
          ELSE
           LL1=LX*J1(1)+LY*J1(2)+LZ*J1(3)
          ENDIF
          IF (ABS(LL1)/LL>EM6) IXTG1(1,I)=-1
         ENDIF
C--------ligne 2--------------
        ELSEIF (IC2>0.AND.IC3>0)THEN
         LX=X(1,N3)-X(1,N2)
         LY=X(2,N3)-X(2,N2)
         LZ=X(3,N3)-X(3,N2)
         LL =SQRT(LX*LX+LY*LY+LZ*LZ)
         J1(1)=IC3/4
         J1(2)=(IC3-4*J1(1))/2
         J1(3)=(IC3-4*J1(1)-2*J1(2))
         IS=ISKEW(N3)
         IF (IS==1) THEN
          E1X=SKEW(1,IS)
          E1Y=SKEW(2,IS)
          E1Z=SKEW(3,IS)
          E2X=SKEW(4,IS)
          E2Y=SKEW(5,IS)
          E2Z=SKEW(6,IS)
          E3X=SKEW(7,IS)
          E3Y=SKEW(8,IS)
          E3Z=SKEW(9,IS)
          LL1=LX*(E1X+E2X+E3X)+LY*(E1Y+E2Y+E3Y)+LZ*(E1Z+E2Z+E3Z)
         ELSE
          LL1=LX*J1(1)+LY*J1(2)+LZ*J1(3)
         ENDIF
         IF (ABS(LL1)/LL>EM6) THEN
          J1(1)=IC2/4
          J1(2)=(IC2-4*J1(1))/2
          J1(3)=(IC2-4*J1(1)-2*J1(2))
          IS=ISKEW(N2)
          IF (IS==1) THEN
           E1X=SKEW(1,IS)
           E1Y=SKEW(2,IS)
           E1Z=SKEW(3,IS)
           E2X=SKEW(4,IS)
           E2Y=SKEW(5,IS)
           E2Z=SKEW(6,IS)
           E3X=SKEW(7,IS)
           E3Y=SKEW(8,IS)
           E3Z=SKEW(9,IS)
           LL1=LX*(E1X+E2X+E3X)+LY*(E1Y+E2Y+E3Y)+LZ*(E1Z+E2Z+E3Z)
          ELSE
           LL1=LX*J1(1)+LY*J1(2)+LZ*J1(3)
          ENDIF
          IF (ABS(LL1)/LL>EM6) IXTG1(2,I)=-1
         ENDIF
C--------ligne 3--------------
        ELSEIF (IC1>0.AND.IC3>0)THEN
         LX=X(1,N3)-X(1,N1)
         LY=X(2,N3)-X(2,N1)
         LZ=X(3,N3)-X(3,N1)
         LL =SQRT(LX*LX+LY*LY+LZ*LZ)
         J1(1)=IC3/4
         J1(2)=(IC3-4*J1(1))/2
         J1(3)=(IC3-4*J1(1)-2*J1(2))
         IS=ISKEW(N3)
         IF (IS==1) THEN
          E1X=SKEW(1,IS)
          E1Y=SKEW(2,IS)
          E1Z=SKEW(3,IS)
          E2X=SKEW(4,IS)
          E2Y=SKEW(5,IS)
          E2Z=SKEW(6,IS)
          E3X=SKEW(7,IS)
          E3Y=SKEW(8,IS)
          E3Z=SKEW(9,IS)
          LL1=LX*(E1X+E2X+E3X)+LY*(E1Y+E2Y+E3Y)+LZ*(E1Z+E2Z+E3Z)
         ELSE
          LL1=LX*J1(1)+LY*J1(2)+LZ*J1(3)
         ENDIF
         IF (ABS(LL1)/LL>EM6) THEN
          J1(1)=IC1/4
          J1(2)=(IC1-4*J1(1))/2
          J1(3)=(IC1-4*J1(1)-2*J1(2))
          IS=ISKEW(N1)
          IF (IS==1) THEN
           E1X=SKEW(1,IS)
           E1Y=SKEW(2,IS)
           E1Z=SKEW(3,IS)
           E2X=SKEW(4,IS)
           E2Y=SKEW(5,IS)
           E2Z=SKEW(6,IS)
           E3X=SKEW(7,IS)
           E3Y=SKEW(8,IS)
           E3Z=SKEW(9,IS)
           LL1=LX*(E1X+E2X+E3X)+LY*(E1Y+E2Y+E3Y)+LZ*(E1Z+E2Z+E3Z)
          ELSE
           LL1=LX*J1(1)+LY*J1(2)+LZ*J1(3)
          ENDIF
          IF (ABS(LL1)/LL>EM6) IXTG1(3,I)=-1
         ENDIF
        ENDIF
       ENDDO
C-----------------------------------------------
      RETURN
      END
