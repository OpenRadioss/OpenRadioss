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
Chd|  GRAY21                        source/materials/mat/mat016/gray21.F
Chd|-- called by -----------
Chd|        GRAY20                        source/materials/mat/mat016/gray20.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GRAY21(PM,RHO,TEMP,XIST,MAT,RHO0,DSP,ALP,PCR,P1,EGG,
     .     XIST0, XLAM, EM0, EM1, EM2,
     .     ESPE, GEAX, G0AX, TM,
     .     DELT, RP3, X, GP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .     PM(NPROPM,*), RHO(*), TEMP(*), XIST(*), RHO0(*), DSP(*), ALP(*), PCR(*), P1(*),
     .     EGG(*), XIST0(*), XLAM(*), EM0(*), EM1(*), EM2(*),
     .     ESPE(*), GEAX(*), G0AX(*), TM(*),
     .     DELT(*), RP3(*), X(*), GP(*)
      INTEGER MAT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, MX
      my_real
     .   P(MVSIZ), XNU(MVSIZ),
     .   VJ, XJ, C1,
     .   C2, C3, D1, D2, D3, THET, APY, VB, Z, ZJ, TZ, TZJ, FE, A2, BB,
     .   CC, FP, V1, XM, UNMM2
C-----------------------------------------------
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      !-----------------------------
      !
      !     BRANCHEMENTS
      !
      !-----------------------------
      UNMM2 = -O88P9844 + DEUX
      XM    = NEUFP38
         
      DO I=LFT,LLT
        MX           = MAT(I)
        VJ           = PM(56,MX)
        XJ           = UN-RHO0(I)*VJ
        
        !--------------------------------------
        IF(X(I).LT.XJ.AND.XIST0(I).NE.CINQ)THEN
        !--------------------
        !      LIQUIDE-VAPEUR
        !--------------------
          XIST(I)    = QUATRE
          C1         = PM(57,MX)
          C2         = PM(58,MX)
          C3         = PM(59,MX)
          D1         = PM(60,MX)
          D2         = PM(24,MX)
          D3         = PM(25,MX)
          THET       = PM(26,MX)
          APY        = PM(27,MX)
          VB         = PM(28,MX)
          Z          = VB*RHO(I)
          ZJ         = VB/VJ
          TZ         = THET-Z
          TZJ        = THET-ZJ
          FE         = (TZJ**3*(DEUX*Z-DEUX+THET)/TZ**2-TZ*(DEUX*ZJ-DEUX+THET))*UNDEMI*VB/ZJ**3
          A2         = QUATRE*(D3-C3*FE)
          BB         = RP3(I)+DEUX*D2
          CC         = DEUX*(D1+C1*FE-ESPE(I)-APY*Z/VB)
          IF(A2.EQ.ZERO)THEN
            TEMP(I)  = -CC/BB
          ELSE
           TEMP(I)   =(-BB+SQRT(BB**2- DEUX*A2*CC))/A2
          ENDIF
          FP         = (Z*TZJ/(ZJ*TZ))**3
          P(I)       = RP3(I)*TEMP(I)*Z*(UN+Z+Z**2-Z**3)/(TROIS*VB*(UN-Z)**3)
     .                 -APY*Z**2/VB**2
     .                 +FP*(C1+C2*TEMP(I)+C3*TEMP(I)**2)
          P1(I)      = ZERO
          PCR(I)     = P(I)*DEUX/RHO(I)

         ELSEIF(ESPE(I).LE.EM1(I).OR.XIST0(I).EQ.CINQ) THEN
         !--------------------
         !      SOLIDE
         !--------------------
           XIST(I)   = ZERO
           A2        = GP(I)
           BB        = RP3(I)
           CC        = -EM0(I)
           IF(A2.EQ.ZERO)THEN
             TEMP(I) = -CC/BB
             PCR(I)  = ZERO
           ELSE
             TEMP(I) = (-BB+SQRT(BB**2-DEUX*A2*CC))/A2
             PCR(I)  = GEAX(I)*TEMP(I)**2
           ENDIF

         ELSEIF(ESPE(I).LT.EM2(I)) THEN
         !--------------------
         !      FUSION 2 PHASES
         !--------------------
           XIST(I)   = UN
           XNU(I)    = (ESPE(I)-EM1(I))/(EM2(I)-EM1(I))
           V1        = DSP(I)-ALP(I)
           A2        = GP(I)
           BB        = RP3(I)+XNU(I)*V1
           CC        = -(EM0(I)+XNU(I)**2*DELT(I)*V1)
           IF(A2.EQ.ZERO)THEN
             TEMP(I) = -CC/BB
             PCR(I)  = -DEUX*XNU(I)*V1*(XLAM(I)*TM(I)+(TEMP(I)-XNU(I)*DELT(I))*G0AX(I))
           ELSE
             TEMP(I) = (-BB+SQRT(BB**2-DEUX*A2*CC))/A2
             PCR(I)  = GEAX(I)*TEMP(I)**2-DEUX*XNU(I)*V1*(XLAM(I)*TM(I)+(TEMP(I)-XNU(I)*DELT(I))*G0AX(I))
           ENDIF

         ELSEIF(ESPE(I).LE.EGG(I)) THEN
         !--------------------
         !      LIQUIDE
         !--------------------
           XIST(I)   = DEUX
           A2        = GP(I)-ALP(I)/TM(I)
           BB        = RP3(I)
           CC        = -(EM0(I)-TM(I)*(DSP(I)-UNDEMI*ALP(I)))
           IF(A2.EQ.ZERO)THEN
             TEMP(I) = -CC/BB
             PCR(I)  = -TM(I)*(XLAM(I)+G0AX(I))*(DEUX*DSP(I)-ALP(I)*(UN+TEMP(I)**2/TM(I)**2))
           ELSE
             TEMP(I) = (-BB+SQRT(BB**2-2.*A2*CC))/A2
             PCR(I)  = GEAX(I)*TEMP(I)**2-TM(I)*(XLAM(I)+G0AX(I))*(DEUX*DSP(I)-ALP(I)*(UN+TEMP(I)**2/TM(I)**2))
           ENDIF

         ELSE
         !--------------------
         !      LIQUIDE-CHAUD
         !--------------------
           XIST(I)   = TROIS
           A2        = GP(I)
           BB        = RP3(I)-XM*ALP(I)
           CC        = -(EM0(I)-TM(I)*(DSP(I)-UNDEMI*ALP(I)*UNMM2))
           IF(A2.EQ.ZERO)THEN
             TEMP(I) = -CC/BB
             PCR(I)  = -TM(I)*( DEUX*DSP(I)-ALP(I)*(UNMM2+DEUX*XM*TEMP(I)/TM(I)) )*(XLAM(I)+G0AX(I))
           ELSE
             TEMP(I) = (-BB+SQRT(BB**2-DEUX*A2*CC))/A2
             PCR(I)  = GEAX(I)*TEMP(I)**2-TM(I)*(DEUX*DSP(I)-ALP(I)*(UNMM2+2.*XM*TEMP(I)/TM(I)))*(XLAM(I)+G0AX(I))
           ENDIF
           
           !--------------------
           
        ENDIF
        !--------------------------------------      
      ENDDO !next I

      RETURN
      END
