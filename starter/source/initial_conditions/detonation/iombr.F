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
Chd|  IOMBR                         source/initial_conditions/detonation/iombr.F
Chd|-- called by -----------
Chd|        ECRAN1                        source/initial_conditions/detonation/ecran1.F
Chd|        ECRAN2                        source/initial_conditions/detonation/ecran2.F
Chd|-- calls ---------------
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|====================================================================
      INTEGER FUNCTION IOMBR(DETONATORS,X,IECR,DDMX)
      USE DETONATORS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real DDMX
      INTEGER IECR(*)
      my_real X(3,*)
      TYPE(DETONATOR_STRUCT_) :: DETONATORS
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr11_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JOMBR, KOMBR, II, I,NPE
      my_real
     .   YLD, ZLD, DD, EPS, DPE, DME, Y2, Z2, PV2, D2, Y1, Z1, PV1,
     .   D1, Y12, Z12, A1, A2, A3, Y3, Z3, D3
C-----------------------------------------------

      NPE = DETONATORS%NPE
      IOMBR=0
      JOMBR=0
      KOMBR=0
      YLD=YL-YD
      ZLD=ZL-ZD
      DD=YLD**2+ZLD**2
      
      IF(DD > DDMX)THEN
       IOMBR=2
       GOTO 999
      ENDIF

      EPS=EM5*DD
      DPE=DD+EPS
      DME=DD-EPS
      II=IECR(1)
      Y2=X(2,II)
      Z2=X(3,II)
      PV2=YLD*(Z2-ZD)-ZLD*(Y2-YD)
      D2 =(Y2-YD)*YLD+(Z2-ZD)*ZLD

      DO I=2,NPE
        Y1=Y2
        Z1=Z2
        PV1=PV2
        D1 =D2
        II=IECR(I)
        Y2=X(2,II)
        Z2=X(3,II)
        PV2=YLD*(Z2-ZD)-ZLD*(Y2-YD)
        D2=(Y2-YD)*YLD+(Z2-ZD)*ZLD
        IF(PV1*PV2.LE.EPS)THEN 
		!Pi-1 et Pi dans demi plan de frontiere (DL) differents
         IF(D2 > DPE .AND. D1 > DPE)THEN
           !Pi-1 ET Pi APRES L
           IOMBR=0
         ELSEIF(D2 < -EPS.AND.D1 < -EPS)THEN
           !Pi-1 ET Pi AVANT D
           IOMBR=0
         ELSE
           !RECHERCHE INTERSECTION [Pi-1,Pi] AVEC [DL]
           Y12=Y1-Y2
           Z12=Z1-Z2
           A1=YL*ZLD-ZL*YLD
           A2=Y1*Z12-Z1*Y12
           A3=Y12*ZLD-Z12*YLD
           IF(ABS(A3) > EPS)THEN
             Y3=(Y12*A1-YLD*A2)/A3
             Z3=(Z12*A1-ZLD*A2)/A3
             D3=(Y3-YD)*YLD+(Z3-ZD)*ZLD
             IF(D3 > DPE.OR.D3 < -EPS)THEN
               IOMBR=0
             ELSEIF(D3 > EPS.AND.D3 < DME)THEN
               !intersection a linterieur
               IOMBR=1
               GOTO 999
             ELSEIF(ABS(D3).LE.EPS)THEN
               !intersection en D
               IF(A3 > EPS)JOMBR=1
             ELSEIF(ABS(D3-DD).LE.EPS)THEN               
               !intersection en L
               IF(A3 < -EPS)KOMBR=1
             ELSE
               !PB DE PRECISION
               IOMBR=0
             ENDIF !(D3 > DPE.OR.D3 < -EPS)
           ELSE
             IOMBR=0
           ENDIF !(ABS(A3) > EPS)
         ENDIF !(D2 > DPE.AND.D1 > DPE)
        ELSE
		  !(PV1*PV2 > EPS) Pi-1 et Pi dans le meme demi plan de frontiere (DL)
          IOMBR=0
        ENDIF !(PV1*PV2.LE.EPS)
      END DO !I=2,NPE

      IF(JOMBR+KOMBR.EQ.2)THEN
        IOMBR=1
      ELSE
        DTO=DTO0+SQRT(DD)/VDTO
      ENDIF

 999  CONTINUE
      IF(IOMBR == 0)DDMX=DD

      RETURN
      END
