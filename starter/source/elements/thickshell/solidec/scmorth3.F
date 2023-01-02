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
Chd|  SCMORTH3                      source/elements/thickshell/solidec/scmorth3.F
Chd|-- called by -----------
Chd|        S6CINIT3                      source/elements/thickshell/solide6c/s6cinit3.F
Chd|        S8CINIT3                      source/elements/thickshell/solide8c/s8cinit3.F
Chd|        SCINIT3                       source/elements/thickshell/solidec/scinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SCMORTH3(PID  ,GEO  ,IGEO ,SKEW ,IREP ,GAMA ,
     .       RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .       E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .       NGL  ,ANGLE,NSIGI,SIGSP,NSIGS,SIGI ,IXS  ,ILAY ,
     .       ORTHOGLOB  ,PT,NEL)
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PID(*),IGEO(NPROPGI,*),IREP,NGL(*),NSIGI,NSIGS,
     .   IXS(NIXS,*),ILAY,IPID,ORTHOGLOB(*),PT(*),NEL
C     REAL
      my_real
     .   GEO(NPROPG,*),SKEW(LSKEW,*),GAMA(NEL,6),ANGLE(*),
     .   RX(*) ,RY(*) ,RZ(*) ,SX(*) ,SY(*) ,SZ(*) ,TX(*) ,TY(*) ,TZ(*),
     .   E1X(*),E1Y(*),E1Z(*),E2X(*),E2Y(*),E2Z(*),E3X(*),E3Y(*),E3Z(*),
     .   SIGSP(NSIGI,*),SIGI(NSIGS,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IG,IGTYP,ISK,IPNUM,ISKV,IIS,II,J,JJ,N,IFLAGINI,INIORTH(MVSIZ)
      my_real
     .   VX,VY,VZ,VR,VS,VN,V,PHI,CP,SP,CPN,SPN,
     .   S,D1,D2,U1X,U1Y,U2X,U2Y,DET,W1X,W2X,W1Y,W2Y
      my_real GAMATMP(6)
C=======================================================================
C---- tag elm /w /INIBRI/ORTHO
      INIORTH(LFT:LLT)=0
      IF (NVSOLID3 /= 0) THEN
        IIS= NVSOLID1  + NVSOLID2 +  4 +NUSOLID 
        DO I=LFT,LLT
         JJ=PT(NFT+I)
         IF(JJ ==0 ) CYCLE
         IF(ORTHOGLOB(I) == 0) THEN  ! IF COS(PHI), SIN(PHI)
           IF(SIGSP((ILAY-1)*6+IIS+1,JJ) /=ZERO.OR.
     .        SIGSP((ILAY-1)*6+IIS+2,JJ)/=ZERO  ) THEN
             INIORTH(I) = 1
           ENDIF        
         ELSE            ! IF ORTHOTROPIC AXES IN GLOBAL REFERENCE
          IF(
     .     SIGSP((ILAY-1)*6+IIS+1,JJ) /= ZERO .OR.
     .     SIGSP((ILAY-1)*6+IIS+2,JJ) /= ZERO .OR.
     .     SIGSP((ILAY-1)*6+IIS+3,JJ) /= ZERO .OR. 
     .     SIGSP((ILAY-1)*6+IIS+4,JJ) /= ZERO .OR.
     .     SIGSP((ILAY-1)*6+IIS+5,JJ) /= ZERO .OR. 
     .     SIGSP((ILAY-1)*6+IIS+6,JJ) /= ZERO )THEN
             INIORTH(I) = 1
          ENDIF         
         ENDIF
        ENDDO
      ENDIF
C---
C     Repere orthotrope pour thick shells-------
C     Stockage de Transpose(G) p.r.a. corotationel systems comme les coques
C     GAMA(1)= sina , GAMA(2) = cosa, only first 2 are useful --
C---
C------------HA8: variable thickness direction ---------

       DO I=LFT,LLT
C
        IF(INIORTH(I) ==1 ) CYCLE
        IG = PID(I)
        ID=IGEO(1,IG)
        CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,IG),LTITR)
        IF (IG > 0) THEN
          IGTYP = IGEO(11,IG)
          IPNUM = IGEO(2,IG)
          ISKV = IGEO(7,IG)
           PHI = ANGLE(I) * PI/HUNDRED80
           CP  = COS(PHI)
           SP  = SIN(PHI)
C------------------Vx,Vy,VZ defined by skew-----
         IF (ISKV==0) THEN
          VX=GEO(7,IG)
          VY=GEO(8,IG)
          VZ=GEO(9,IG)
         ELSE
          VX=SKEW(1,ISKV)
          VY=SKEW(2,ISKV)
          VZ=SKEW(3,ISKV)
         ENDIF
         SELECT CASE (IPNUM)
         CASE (1)
          VN=VX*E1X(I)+VY*E1Y(I)+VZ*E1Z(I)
          VX=VX-VN*E1X(I)
          VY=VY-VN*E1Y(I)
          VZ=VZ-VN*E1Z(I)
          V=SQRT(VX*VX+VY*VY+VZ*VZ)
          IF(V<EM3)THEN
           CALL ANCMSG(MSGID=526,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=NGL(I))
          ENDIF
C
          V=ONE/MAX(V,EM20)
          VX=VX*V
          VY=VY*V
          VZ=VZ*V
          VR=VX*E2X(I)+VY*E2Y(I)+VZ*E2Z(I)
          VS=VX*E3X(I)+VY*E3Y(I)+VZ*E3Z(I)
          CPN=VR*CP-VS*SP
          SPN=VS*CP+VR*SP
C
         CASE (2)
          VN=VX*E2X(I)+VY*E2Y(I)+VZ*E2Z(I)
          VX=VX-VN*E2X(I)
          VY=VY-VN*E2Y(I)
          VZ=VZ-VN*E2Z(I)
          V=SQRT(VX*VX+VY*VY+VZ*VZ)
          IF(V<EM3)THEN
           CALL ANCMSG(MSGID=526,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=NGL(I))
          ENDIF
C
          V=ONE/MAX(V,EM20)
          VX=VX*V
          VY=VY*V
          VZ=VZ*V
          VR=VX*E3X(I)+VY*E3Y(I)+VZ*E3Z(I)
          VS=VX*E1X(I)+VY*E1Y(I)+VZ*E1Z(I)
          CPN=VR*CP-VS*SP
          SPN=VS*CP+VR*SP
C
        CASE (3)
          VN=VX*E3X(I)+VY*E3Y(I)+VZ*E3Z(I)
          VX=VX-VN*E3X(I)
          VY=VY-VN*E3Y(I)
          VZ=VZ-VN*E3Z(I)
          V=SQRT(VX*VX+VY*VY+VZ*VZ)
          IF(V<EM3)THEN
           CALL ANCMSG(MSGID=526,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=NGL(I))
          ENDIF
C
          V=ONE/MAX(V,EM20)
          VX=VX*V
          VY=VY*V
          VZ=VZ*V
          VR=VX*E1X(I)+VY*E1Y(I)+VZ*E1Z(I)
          VS=VX*E2X(I)+VY*E2Y(I)+VZ*E2Z(I)
          CPN=VR*CP-VS*SP
          SPN=VS*CP+VR*SP
C
         END SELECT
         GAMA(I,1)=CPN
         GAMA(I,2)=SPN
        ENDIF
C
       ENDDO
C------Uij-->gij; Wij->[g]-1-----
       IF (IREP==1) THEN
        DO I=LFT,LLT
          IG = PID(I)
          IPNUM = IGEO(2,IG)
C----par rapport à HA8, permitation si s16----
          SELECT CASE (IPNUM)
          CASE (1)
           U1X = RX(I)*E2X(I)+RY(I)*E2Y(I)+RZ(I)*E2Z(I)
           U1Y = RX(I)*E3X(I)+RY(I)*E3Y(I)+RZ(I)*E3Z(I)
           U2X = SX(I)*E2X(I)+SY(I)*E2Y(I)+SZ(I)*E2Z(I)
           U2Y = SX(I)*E3X(I)+SY(I)*E3Y(I)+SZ(I)*E3Z(I)
           DET = U1X*U2Y-U1Y*U2X
	   W1X = U2Y/DET
           W2Y = U1X/DET
	   W1Y = -U1Y/DET
	   W2X = -U2X/DET
           D1=GAMA(I,1)
           D2=GAMA(I,2)
           CPN= W1X*D1 + W2X*D2
           SPN= W1Y*D1 + W2Y*D2
           S=MAX(EM20,SQRT(CPN*CPN+SPN*SPN))
           CPN = CPN/S
           SPN = SPN/S
C
          CASE (2)
           U1X = SX(I)*E3X(I)+SY(I)*E3Y(I)+SZ(I)*E3Z(I)
           U1Y = SX(I)*E1X(I)+SY(I)*E1Y(I)+SZ(I)*E1Z(I)
           U2X = TX(I)*E3X(I)+TY(I)*E3Y(I)+TZ(I)*E3Z(I)
           U2Y = TX(I)*E1X(I)+TY(I)*E1Y(I)+TZ(I)*E1Z(I)
           DET = U1X*U2Y-U1Y*U2X
	   W1X = U2Y/DET
           W2Y = U1X/DET
	   W1Y = -U1Y/DET
	   W2X = -U2X/DET
           D1=GAMA(I,1)
           D2=GAMA(I,2)
           CPN= W1X*D1 + W2X*D2
           SPN= W1Y*D1 + W2Y*D2
           S=MAX(EM20,SQRT(CPN*CPN+SPN*SPN))
           CPN = CPN/S
           SPN = SPN/S
C
          CASE (3)
           U1X = TX(I)*E1X(I)+TY(I)*E1Y(I)+TZ(I)*E1Z(I)
           U1Y = TX(I)*E2X(I)+TY(I)*E2Y(I)+TZ(I)*E2Z(I)
           U2X = RX(I)*E1X(I)+RY(I)*E1Y(I)+RZ(I)*E1Z(I)
           U2Y = RX(I)*E2X(I)+RY(I)*E2Y(I)+RZ(I)*E2Z(I)
           DET = U1X*U2Y-U1Y*U2X
	   W1X = U2Y/DET
           W2Y = U1X/DET
	   W1Y = -U1Y/DET
	   W2X = -U2X/DET
           D1=GAMA(I,1)
           D2=GAMA(I,2)
           CPN= W1X*D1 + W2X*D2
           SPN= W1Y*D1 + W2Y*D2
           S=MAX(EM20,SQRT(CPN*CPN+SPN*SPN))
           CPN = CPN/S
           SPN = SPN/S
C
          END SELECT
          GAMA(I,1)=CPN
          GAMA(I,2)=SPN
        ENDDO
       ENDIF
C
C---
      IF (NVSOLID3 /= 0) THEN
        IIS= NVSOLID1  + NVSOLID2 +  4 +NUSOLID 
        DO I=LFT,LLT
         IF(ORTHOGLOB(I) == 0) THEN  ! IF COS(PHI), SIN(PHI)
           II=NFT+I
           JJ=PT(II)
           IFLAGINI = 1
           IF(JJ==0)IFLAGINI = 0
           IF(IFLAGINI == 1 .AND. 
     .       ( SIGSP((ILAY-1)*6+IIS+1,JJ) /= ZERO.OR.
     .        SIGSP((ILAY-1)*6+IIS+2,JJ)/=ZERO)  ) THEN
           GAMA(I,1) = SIGSP((ILAY-1)*6+IIS+1,JJ)
           GAMA(I,2) = SIGSP((ILAY-1)*6+IIS+2,JJ)
          ENDIF        
         ELSE            ! IF ORTHOTROPIC AXES IN GLOBAL REFERENCE
          II=NFT+I
          JJ=PT(II)
          IG = PID(I)
          IPNUM = IGEO(2,IG)
          IFLAGINI = 1
          IF(JJ==0)IFLAGINI = 0
          IF(IFLAGINI == 1 .AND.
     .   ( SIGSP((ILAY-1)*6+IIS+1,JJ) /= ZERO .OR.
     .     SIGSP((ILAY-1)*6+IIS+2,JJ) /= ZERO .OR.
     .     SIGSP((ILAY-1)*6+IIS+3,JJ) /= ZERO .OR. 
     .     SIGSP((ILAY-1)*6+IIS+4,JJ) /= ZERO .OR.
     .     SIGSP((ILAY-1)*6+IIS+5,JJ) /= ZERO .OR. 
     .     SIGSP((ILAY-1)*6+IIS+6,JJ) /= ZERO) )THEN
               GAMATMP(1) = SIGSP((ILAY-1)*6+IIS+1,JJ)
               GAMATMP(2) = SIGSP((ILAY-1)*6+IIS+2,JJ)
               GAMATMP(3) = SIGSP((ILAY-1)*6+IIS+3,JJ)
               GAMATMP(4) = SIGSP((ILAY-1)*6+IIS+4,JJ)
               GAMATMP(5) = SIGSP((ILAY-1)*6+IIS+5,JJ)
               GAMATMP(6) = SIGSP((ILAY-1)*6+IIS+6,JJ)
               GAMA(I,1:6) = ZERO
               SELECT CASE (IPNUM)
               CASE (1)
                 GAMA(I,1) = GAMATMP(1)*E2X(I)+
     .                       GAMATMP(2)*E2Y(I)+GAMATMP(3)*E2Z(I)
                 GAMA(I,2) = GAMATMP(1)*E3X(I)+
     .                       GAMATMP(2)*E3Y(I)+GAMATMP(3)*E3Z(I)
              CASE (2)
                 GAMA(I,1) = GAMATMP(1)*E3X(I)+
     .                       GAMATMP(2)*E3Y(I)+GAMATMP(3)*E3Z(I)
                 GAMA(I,2) = GAMATMP(1)*E1X(I)+
     .                       GAMATMP(2)*E1Y(I)+GAMATMP(3)*E1Z(I)
              CASE (3)
                 GAMA(I,1) = GAMATMP(1)*E1X(I)+
     .                       GAMATMP(2)*E1Y(I)+GAMATMP(3)*E1Z(I)
                 GAMA(I,2) = GAMATMP(1)*E2X(I)+
     .                       GAMATMP(2)*E2Y(I)+GAMATMP(3)*E2Z(I)
              END SELECT
          ENDIF         
         ENDIF
        ENDDO
      ENDIF
C---
      RETURN
      END SUBROUTINE SCMORTH3
