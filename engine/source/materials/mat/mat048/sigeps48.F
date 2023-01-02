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
Chd|  SIGEPS48                      source/materials/mat/mat048/sigeps48.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGEPS48(
     1     NEL    ,NUPARAM,NUVAR   ,MFUNC   ,KFUNC   ,NPF    ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0    ,RHO    ,
     3     VOLUME ,EINT   ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVZZ  ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,UVAR    ,OFF     ,NGL     ,IPT    ,
     B     IPM    ,MAT    ,EPSP    ,SIGY    ,DEFP    ,DPLA1  ,
     C     AMU    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C MFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW not used
C KFUNC   | NFUNC   | I | R | FUNCTION INDEX not used
C NPF     |  *      | I | R | FUNCTION ARRAY   
C TF      |  *      | F | R | FUNCTION ARRAY 
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL     | F | R | INITIAL DENSITY
C RHO     | NEL     | F | R | DENSITY
C VOLUME  | NEL     | F | R | VOLUME
C EINT    | NEL     | F | R | TOTAL INTERNAL ENERGY
C EPSPXX  | NEL     | F | R | STRAIN RATE XX
C EPSPYY  | NEL     | F | R | STRAIN RATE YY
C ...     |         |   |   |
C DEPSXX  | NEL     | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL     | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C EPSXX   | NEL     | F | R | STRAIN XX
C EPSYY   | NEL     | F | R | STRAIN YY
C ...     |         |   |   |
C SIGOXX  | NEL     | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIGOYY  | NEL     | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL     | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL     | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL     | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL     | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"
C
      INTEGER NEL, NUPARAM, NUVAR,IPT,
     .   IPM(NPROPMI,*),NGL(NEL),MAT(NEL)
      my_real TIME,TIMESTEP,UPARAM(*),
     .   RHO(NEL),RHO0(NEL),VOLUME(NEL),EINT(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   EPSP(NEL),AMU(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),SIGVZZ(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL),SIGY(NEL),DEFP(NEL),
     .    DPLA1(MVSIZ)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real
     . UVAR(NEL,NUVAR), OFF(NEL)
C-------------------------
C    variables non utilisees (fonctions utilisateur)
C-------------------------
      INTEGER NPF(*),MFUNC,KFUNC(MFUNC)
      my_real
     . FINTER,TF(*)
C      EXTERNAL FINTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IADBUF,ICC(MVSIZ),MX
      my_real 
     .        E,NU,P,DAV,VM,R,DPLA,EPST,E1,E2,E3,E4,E5,E6,C,CD,D,
     .        Y,YP,E42,E52,E62,EPST2,PA,PB,PC,PDA,PDB,YY,
     .        C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),
     .        G(MVSIZ),G2(MVSIZ),G3(MVSIZ),EPSGM(MVSIZ),
     .        EPMAX(MVSIZ),SMAX(MVSIZ),PLA(MVSIZ),FAIL(MVSIZ),
     .        EPSR1(MVSIZ),EPSR2(MVSIZ),YLD(MVSIZ),H(MVSIZ),
     .        CA(MVSIZ),CB(MVSIZ),CN(MVSIZ),CC(MVSIZ),CO(MVSIZ),
     .        CM(MVSIZ),CE(MVSIZ),CK(MVSIZ),EPS0(MVSIZ)
C==================================================================|
      IF(TIME==ZERO)THEN
        DO I=1,NEL
          UVAR(I,1)=ZERO
        ENDDO
      ENDIF
C
C---
      MX = MAT(1)
      IADBUF  = IPM(7,MX)-1
      DO I=1,NEL
        E       = UPARAM(IADBUF+1)
        NU      = UPARAM(IADBUF+2)
        CA(I)   = UPARAM(IADBUF+3)
        SMAX(I) = UPARAM(IADBUF+4)
        EPMAX(I)= UPARAM(IADBUF+5)
        EPSR1(I)= UPARAM(IADBUF+6)
        EPSR2(I)= UPARAM(IADBUF+7)
        CB(I)   = UPARAM(IADBUF+8)
        CN(I)   = UPARAM(IADBUF+9)
        CC(I)   = UPARAM(IADBUF+10)
        CO(I)   = UPARAM(IADBUF+11)
        CM(I)   = UPARAM(IADBUF+12)
        G(I)    = UPARAM(IADBUF+16)
        G2(I)   = UPARAM(IADBUF+17)
        G3(I)   = UPARAM(IADBUF+18)
        C1(I)   = UPARAM(IADBUF+19)
        C2(I)   = UPARAM(IADBUF+20)
        C3(I)   = UPARAM(IADBUF+21)
        EPSGM(I)= UPARAM(IADBUF+22)
        EPS0(I) = UPARAM(IADBUF+23)
        CE(I)   = UPARAM(IADBUF+24)
        CK(I)   = UPARAM(IADBUF+25)
      ENDDO
C
C---   DEVIATORIC STRESS ESTIMATE
C
      DO I=1,NEL
        PLA(I) = UVAR(I,1)
        P = -(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I))* THIRD
        DAV = (DEPSXX(I)+DEPSYY(I)+DEPSZZ(I))* THIRD
        SIGNXX(I)=SIGOXX(I) + P + G2(I)*(DEPSXX(I)-DAV)
        SIGNYY(I)=SIGOYY(I) + P + G2(I)*(DEPSYY(I)-DAV)
        SIGNZZ(I)=SIGOZZ(I) + P + G2(I)*(DEPSZZ(I)-DAV)
        SIGNXY(I)=SIGOXY(I) + G(I)*DEPSXY(I)
        SIGNYZ(I)=SIGOYZ(I) + G(I)*DEPSYZ(I)
        SIGNZX(I)=SIGOZX(I) + G(I)*DEPSZX(I)
C
        SOUNDSP(I) = SQRT((C1(I)+FOUR*G(I)/THREE)/RHO0(I))
        VISCMAX(I) = ZERO
         DPLA1(I) = ZERO                
      ENDDO
C__________
C
C---    STRAIN principal 1, 4 newton iterations 
C
      DO I=1,NEL
C
        DAV = (EPSXX(I)+EPSYY(I)+EPSZZ(I))*THIRD
        E1 = EPSXX(I) - DAV
        E2 = EPSYY(I) - DAV
        E3 = EPSZZ(I) - DAV
        E4 = HALF*EPSXY(I)
        E5 = HALF*EPSYZ(I)
        E6 = HALF*EPSZX(I)
C        -y = (e1-x)(e2-x)(e3-x)
C           - e5^2(e1-x) - e6^2(e2-x) - e4^2(e3-x)
C           + 2e4 e5 e6
C         e1 + e2 + e3 = 0 => terme en x^2 = 0
C         y = x^3 + c x + d
c         yp= 3 x^2 + c
        E42 = E4*E4
        E52 = E5*E5
        E62 = E6*E6
        C = - E1*E1 - E2*E2 - E3*E3 - E42 - E52 - E62
        D = - E1*E2*E3 + E1*E52 + E2*E62 + E3*E42
     &      - TWO*E4*E5*E6 
        CD = C*THIRD
        EPST = SQRT(-CD)
        EPST2 = EPST * EPST
        Y = (EPST2 + C)* EPST + D
        IF(ABS(Y)>EM8)THEN
          EPST = ONEP75 * EPST
          EPST2 = EPST * EPST
          Y = (EPST2 + C)* EPST + D
          YP = THREE*EPST2 + C
          IF(YP/=ZERO)EPST = EPST - Y/YP
          EPST2 = EPST * EPST
          Y = (EPST2 + C)* EPST + D
          YP = THREE*EPST2 + C
          IF(YP/=ZERO)EPST = EPST - Y/YP
          EPST2 = EPST * EPST
          Y = (EPST2 + C)* EPST + D
          YP = THREE*EPST2 + C
          IF(YP/=ZERO)EPST = EPST - Y/YP
          EPST2 = EPST * EPST
          Y = (EPST2 + C)* EPST + D
          YP = 3*EPST2 + C
          IF(YP/=ZERO)EPST = EPST - Y/YP
          EPST = EPST + DAV
        ENDIF
C
C---    tension failure 
C
        FAIL(I) = MAX(ZERO,MIN(ONE,
     .            (EPSR2(I)-EPST)/(EPSR2(I)-EPSR1(I)) ))
C
      ENDDO
C
C--- STRAIN RATE EFFECT, CURRENT YIELD & HARDENING
C---------------------
C Law formulation:
C sig = ca*SigY + cb*epla^cn + (cc-co*epla^cm)*ln(epsp/eps0) + ce*epsp^ck
C---------------------
      DO I=1,NEL
        IF(PLA(I)<=ZERO) THEN
         PA=CA(I)
        ELSEIF(PLA(I)>EPMAX(I)) THEN
         PA=CA(I)+CB(I)*EPMAX(I)**CN(I)
        ELSE
         PA=CA(I)+CB(I)*PLA(I)**CN(I)
        ENDIF
        IF(EPSP(I)<=EPS0(I)) THEN
         PB=ZERO
        ELSEIF(PLA(I)<=ZERO) THEN
         PB=CC(I)*LOG(EPSP(I)/EPS0(I))
        ELSE
         PB=(CC(I)-CO(I)*PLA(I)**CM(I))*LOG(EPSP(I)/EPS0(I))
        ENDIF
        IF(EPSP(I)<=ZERO) THEN
         PC=ZERO
        ELSE
         PC=CE(I)*EPSP(I)**CK(I)
        ENDIF
C-----
        IF(PLA(I)>ZERO. AND .CN(I)>=ONE) THEN
         PDA = CB(I)*CN(I)*PLA(I)**(CN(I)- ONE)
        ELSEIF(PLA(I)>ZERO. AND .CN(I)<ONE)THEN
         PDA = CB(I)*CN(I)*PLA(I)**(ONE-CN(I))
        ELSE
         PDA = E
        ENDIF
        IF(PLA(I)<=ZERO. OR .EPSP(I)<=EPS0(I)) THEN
         PDB = ZERO
        ELSEIF(CM(I)>=ONE) THEN
         PDB = CO(I)*CM(I)*PLA(I)**(CM(I)- ONE)*LOG(EPSP(I)/EPS0(I))
        ELSE
         PDB = CO(I)*CM(I)*PLA(I)**(ONE - CM(I))*LOG(EPSP(I)/EPS0(I))
        ENDIF
C-----
        YY    = PA + PB + PC
        YLD(I)= MIN(SMAX(I)+PC, YY)
        H(I) = PDA + PDB
        IF (YLD(I)<YY) H(I)  = ZERO
        YLD(I)= FAIL(I)*YLD(I)
        H(I)  = FAIL(I)*H(I)
        SIGY(I)=YLD(I)
        IF(PLA(I)>EPMAX(I)) YLD(I)=ZERO
      ENDDO        
C
C-------------------
C     VON MISES & RADIAL RETURN
C-------------------
      DO I=1,NEL
        VM =HALF*(SIGNXX(I)**2+SIGNYY(I)**2+SIGNZZ(I)**2)
     1          +SIGNXY(I)**2+SIGNYZ(I)**2+SIGNZX(I)**2
        VM =SQRT(THREE*VM)
        R = MIN(ONE,YLD(I)/ MAX(VM,EM20))
C       plastic strain increment.
        DPLA=(ONE -R)*VM/MAX(G3(I)+H(I),EM20)
C       actual yield stress.
        YLD(I)=YLD(I)+DPLA*H(I)
        IF(PLA(I)>EPMAX(I)) YLD(I)=ZERO
        R = MIN(ONE,YLD(I)/ MAX(VM,EM20))
C
c        P = C1(I) * (RHO(I)/RHO0(I)- ONE)
        P = C1(I) * AMU(I)
        SIGNXX(I)=SIGNXX(I)*R-P
        SIGNYY(I)=SIGNYY(I)*R-P
        SIGNZZ(I)=SIGNZZ(I)*R-P
        SIGNXY(I)=SIGNXY(I)*R
        SIGNYZ(I)=SIGNYZ(I)*R
        SIGNZX(I)=SIGNZX(I)*R
        PLA(I)=PLA(I) + DPLA
        UVAR(I,1)=PLA(I)
        DPLA1(I) = DPLA        
      ENDDO
C
      DO I=1,NEL
        IF(OFF(I)<ONE) IDEL7NOK = 1
      ENDDO
      DO I=1,NEL
        SIGY(I)=MAX(SIGY(I),YLD(I))
        DEFP(I)=UVAR(I,1)
      ENDDO
C________________________________________________________
      RETURN
      END
C
