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
Chd|  SIGEPS44                      source/materials/mat/mat044/sigeps44.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|-- calls ---------------
Chd|        MSTRAIN_RATE                  source/materials/mat_share/mstrain_rate.F
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS44(
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
     B     IPM    ,MAT    ,EPSP    ,IPLA    ,SIGY    ,PLA    ,
     C     DPLA1  ,AMU    ,ISRATE  ,ASRATE  ,NVARTMP ,VARTMP )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
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
C VISCMAX | NEL     | F | W | MAXIMUN DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR,IPT,NVARTMP,
     .   NGL(NEL),MAT(NEL),IPM(NPROPMI,*), IPLA,IEXPAN,
     .   ISRATE
      my_real
     .   TIME,TIMESTEP,UPARAM(NUPARAM),
     .   RHO(NEL),RHO0(NEL),VOLUME(NEL),EINT(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   DEPSXX(NEL),DEPSYY(NEL),DEPSZZ(NEL),
     .   DEPSXY(NEL),DEPSYZ(NEL),DEPSZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   SIGOXX(NEL),SIGOYY(NEL),SIGOZZ(NEL),
     .   SIGOXY(NEL),SIGOYZ(NEL),SIGOZX(NEL),
     .   EPSP(NEL),AMU(NEL),ASRATE
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),SIGVZZ(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL),SIGY(NEL),
     .    PLA(NEL),DPLA1(MVSIZ),VARTMP(NEL,NVARTMP)  
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real 
     .    UVAR(NEL,NUVAR), OFF(NEL) 
C-------------------------
C    variables non utilisees (fonctions utilisateur)
C-------------------------
      INTEGER NPF(*),MFUNC,KFUNC(MFUNC)
      my_real
     .    TF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IADBUF,ICC,VFLAG,IPOS(MVSIZ),IAD(MVSIZ),ILEN(MVSIZ),IDEV
      my_real
     . E,NU,P,DAV,VM,R,EPST,E1,E2,E3,E4,E5,E6,C,CD,D,
     .        Y,YP,E42,E52,E62,EPST2,RQ,C1,C2,C3,
     .        G,G2,G3,EPSGM,EPMAX,SMAX(MVSIZ),FAIL(MVSIZ),
     .        EPSR1,EPSR2,YLD(MVSIZ),H(MVSIZ),DFDPLA(MVSIZ),
     .        CA,CB,CN,CC,CP,DPLA,YSCALE,DPDT
C=======================================================================
C-----------------------------------------------
C     MATERIAL PARAMETERS
C-----------------------------------------------
      E         = UPARAM(1)
      NU        = UPARAM(2)
      CA        = UPARAM(3)
      SMAX(1:NEL) = UPARAM(4)
      EPMAX     = UPARAM(5)
      EPSR1     = UPARAM(6)
      EPSR2     = UPARAM(7)
      CB        = UPARAM(8)
      CN        = UPARAM(9)
      ICC       = NINT(UPARAM(10))
      CC        = UPARAM(11)
      CP        = UPARAM(12)
      G         = UPARAM(16)
      G2        = UPARAM(17)
      G3        = UPARAM(18)
      C1        = UPARAM(19)
      C2        = UPARAM(20)
      C3        = UPARAM(21)
      EPSGM     = UPARAM(22)
      VFLAG     = NINT(UPARAM(23))
      YSCALE    = UPARAM(24)
C-----------------------------------------------
C     TOTAL OR DEVIATORIC STRAIN-RATE COMPUTATION
C-----------------------------------------------  
      IDEV = VFLAG-2
      IF ((VFLAG == 2) .OR. (VFLAG == 3)) THEN
        CALL MSTRAIN_RATE(NEL    ,ISRATE ,ASRATE ,EPSP   ,IDEV   ,
     .                    EPSPXX ,EPSPYY ,EPSPZZ ,EPSPXY ,EPSPYZ ,
     .                    EPSPZX )
      ENDIF
C-----------------------------------------------
C     DEVIATORIC STRESS ESTIMATION
C-----------------------------------------------
      DO I=1,NEL
        P          = -(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I))* THIRD
        DAV        = (DEPSXX(I)+DEPSYY(I)+DEPSZZ(I)) * THIRD
        SIGNXX(I)  = SIGOXX(I)+P+G2*(DEPSXX(I)-DAV)
        SIGNYY(I)  = SIGOYY(I)+P+G2*(DEPSYY(I)-DAV)
        SIGNZZ(I)  = SIGOZZ(I)+P+G2*(DEPSZZ(I)-DAV)
        SIGNXY(I)  = SIGOXY(I)+G *DEPSXY(I)
        SIGNYZ(I)  = SIGOYZ(I)+G *DEPSYZ(I)
        SIGNZX(I)  = SIGOZX(I)+G *DEPSZX(I)
C
        SOUNDSP(I) = SQRT((C1+ FOUR*G/THREE)/RHO0(I))
        VISCMAX(I) = ZERO
        DPLA1(I)   = ZERO        
      ENDDO
C
C-----------------------------------------------
C     STRAIN principal 1, 4 newton iterations 
C-----------------------------------------------
      DO I=1,NEL
C
        DAV = (EPSXX(I)+EPSYY(I)+EPSZZ(I)) * THIRD
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
        EPST  = SQRT(-CD)
        EPST2 = EPST * EPST
        Y = (EPST2 + C)* EPST + D
        IF(ABS(Y).GT.EM8)THEN
          EPST = ONEP75 * EPST
          EPST2 = EPST * EPST
          Y = (EPST2 + C)* EPST + D
           YP = THREE*EPST2 + C
         IF(YP.NE.ZERO)EPST = EPST - Y/YP
          EPST2 = EPST * EPST
          Y = (EPST2 + C)* EPST + D
          YP = THREE*EPST2 + C
          IF(YP.NE.ZERO)EPST = EPST - Y/YP
          EPST2 = EPST * EPST
          Y = (EPST2 + C)* EPST + D
          YP = THREE*EPST2 + C
          IF(YP.NE.ZERO)EPST = EPST - Y/YP
          EPST2 = EPST * EPST
          Y = (EPST2 + C)* EPST + D
          YP = THREE*EPST2 + C
          IF(YP.NE.ZERO)EPST = EPST - Y/YP
          EPST = EPST + DAV
        ENDIF
C
C---    tension failure 
C
        FAIL(I) = MAX(ZERO,MIN(ONE,
     .            (EPSR2-EPST)/(EPSR2-EPSR1) ))
C
      ENDDO
C-----------------------------------------------
C     STRAIN RATE EFFECT, CURRENT YIELD & HARDENING
C-----------------------------------------------
      IF (MFUNC > 0) THEN
        IPOS(1:NEL)     = NINT(VARTMP(1:NEL,1))
        IAD (1:NEL)     = NPF(KFUNC(1)) / 2 + 1
        ILEN(1:NEL)     = NPF(KFUNC(1)+1) / 2 - IAD(1:NEL) - IPOS(1:NEL)
        CALL VINTER(TF,IAD,IPOS,ILEN,NEL,PLA,DFDPLA,YLD) 
        VARTMP(1:NEL,1) = IPOS(1:NEL)
      ENDIF

      IF (IPLA /= 2) THEN
       DO I=1,NEL
        RQ = ONE
        IF (CC /= ZERO) RQ = ONE + (CC*EPSP(I))**CP
        IF (ICC == 1)   SMAX(I) = SMAX(I)*RQ
        IF (PLA(I) > ZERO) THEN
          IF ((MFUNC > 0) .AND. (CA == ZERO)) THEN
            YLD(I) = YSCALE  * YLD(I) * RQ
            H(I)   = FAIL(I) * (YSCALE*DFDPLA(I)*RQ)
          ELSEIF ((MFUNC > 0) .AND. (CA > ZERO)) THEN
            YLD(I) = YSCALE  * YLD(I)
     .                        + (CA + CB*PLA(I)**CN) * (RQ-ONE)     
            H(I)   = FAIL(I) * (YSCALE*DFDPLA(I) + CN*CB*(RQ-ONE)/(PLA(I)**(ONE-CN)))
          ELSE 
            YLD(I) = (CA + CB*PLA(I)**CN) * RQ
            H(I)   = FAIL(I)*CN*CB*RQ/(PLA(I)**(ONE-CN))
          ENDIF
          YLD(I) = FAIL(I) * MIN(SMAX(I),YLD(I))
        ELSE
          IF ((MFUNC > 0) .AND. (CA == ZERO)) THEN
            YLD(I) = FAIL(I) * YSCALE * YLD(I) * RQ
          ELSEIF ((MFUNC > 0) .AND. (CA > ZERO)) THEN
            YLD(I) = FAIL(I) * (YSCALE * YLD(I) + CA * (RQ-ONE))
          ELSE
            YLD(I) = FAIL(I) * CA * RQ
          ENDIF
          H(I)   = E
        ENDIF
        IF (PLA(I) >= EPSGM) THEN
          YLD(I) = FAIL(I) * SMAX(I)
          H(I)   = ZERO
        ENDIF
        SIGY(I)  = YLD(I)
        IF (PLA(I) >= EPMAX) YLD(I) = ZERO
       ENDDO
      ELSE 
       DO I=1,NEL
        RQ = ONE
        IF (CC /= ZERO) RQ = ONE + (CC*EPSP(I))**CP
        IF (ICC == 1)   SMAX(I) = SMAX(I)*RQ
        IF (PLA(I) > ZERO) THEN
          IF ((MFUNC > 0) .AND. (CA == ZERO)) THEN
            YLD(I) = YSCALE * YLD(I) * RQ
            H(I)   = FAIL(I)*(YSCALE*DFDPLA(I)*RQ)
          ELSEIF ((MFUNC > 0) .AND. (CA > ZERO)) THEN
            YLD(I) = YSCALE * YLD(I)
     .                        + (CA + CB*PLA(I)**CN) * (RQ-ONE)
            H(I)   = FAIL(I)* (YSCALE*DFDPLA(I) + CN*CB*(RQ-ONE)/(PLA(I)**(ONE-CN)))
          ELSE 
            YLD(I) = (CA + CB*PLA(I)**CN) * RQ
            H(I)   = FAIL(I)*CN*CB*RQ/(PLA(I)**(ONE-CN))
          ENDIF
          YLD(I) = FAIL(I) * MIN(SMAX(I),YLD(I))
        ELSE
          IF ((MFUNC > 0) .AND. (CA == ZERO)) THEN
            YLD(I) = FAIL(I) * YSCALE * YLD(I) * RQ
          ELSEIF ((MFUNC > 0) .AND. (CA > ZERO)) THEN
            YLD(I) = FAIL(I) * (YSCALE * YLD(I) + CA * (RQ-ONE))
          ELSE
            YLD(I) = FAIL(I) * CA * RQ
          ENDIF
          H(I)   = E
        ENDIF
        IF (PLA(I) >= EPSGM) THEN
          YLD(I) = FAIL(I) * SMAX(I)
          YLD(I) = ZERO
        ENDIF
        SIGY(I) = YLD(I)
        IF (PLA(I) >= EPMAX) YLD(I)=ZERO
       ENDDO
      ENDIF       
C
C-----------------------------------------------
C     VON MISES & RADIAL RETURN
C-----------------------------------------------
      IF (IPLA == 0) THEN
       DO I = 1,NEL
        VM = HALF*(SIGNXX(I)**2+SIGNYY(I)**2+SIGNZZ(I)**2)
     1          +SIGNXY(I)**2+SIGNYZ(I)**2+SIGNZX(I)**2     
        VM = SQRT(THREE*VM)
        R  = MIN(ONE,YLD(I)/ MAX(VM,EM20))
c        P = C1(I) * (RHO(I)/RHO0(I)-1.)
        P  = C1 * AMU(I) ! AMU is computed by taking into account thermal strain
        SIGNXX(I) = SIGNXX(I)*R-P
        SIGNYY(I) = SIGNYY(I)*R-P
        SIGNZZ(I) = SIGNZZ(I)*R-P
        SIGNXY(I) = SIGNXY(I)*R
        SIGNYZ(I) = SIGNYZ(I)*R
        SIGNZX(I) = SIGNZX(I)*R
        PLA(I)    = PLA(I) + (ONE -R)*VM/MAX(G3+H(I),EM20)
        DPLA1(I)  = (ONE -R)*VM/MAX(G3+H(I),EM20)       
       ENDDO
      ELSEIF (IPLA == 2) THEN
       DO I = 1,NEL
        VM = HALF*(SIGNXX(I)**2+SIGNYY(I)**2+SIGNZZ(I)**2)
     1              +SIGNXY(I)**2+SIGNYZ(I)**2+SIGNZX(I)**2
        VM = SQRT(THREE*VM)
        R  = MIN(ONE,YLD(I)/ MAX(VM,EM20))
c        P = C1(I) * (RHO(I)/RHO0(I)-ONE)
        P  = C1 * AMU(I) ! AMU is computed by taking into account thermal strain
        SIGNXX(I) = SIGNXX(I)*R-P
        SIGNYY(I) = SIGNYY(I)*R-P
        SIGNZZ(I) = SIGNZZ(I)*R-P
        SIGNXY(I) = SIGNXY(I)*R
        SIGNYZ(I) = SIGNYZ(I)*R
        SIGNZX(I) = SIGNZX(I)*R
        PLA(I)    = PLA(I) + (ONE -R)*VM/MAX(G3,EM20)
        DPLA1(I)  = (ONE -R)*VM/MAX(G3,EM20)        
       ENDDO
      ELSEIF (IPLA == 1) THEN
       DO I = 1,NEL
        VM = HALF*(SIGNXX(I)**2+SIGNYY(I)**2+SIGNZZ(I)**2)
     1              +SIGNXY(I)**2+SIGNYZ(I)**2+SIGNZX(I)**2
        VM = SQRT(THREE*VM)
        R  = MIN(ONE,YLD(I)/ MAX(VM,EM20))
Clm     plastic strain increment.
        DPLA = (ONE - R)*VM/MAX(G3+H(I),EM20)
Clm     actual yeld stress.
        YLD(I) = YLD(I) + DPLA*H(I)
        IF (PLA(I) >= EPMAX) YLD(I)=ZERO
        R = MIN(ONE,YLD(I)/ MAX(VM,EM20))
c        P = C1(I) * (RHO(I)/RHO0(I) - ONE)
        P = C1 * AMU(I) ! AMU is computed by taking into account thermal strain
        SIGNXX(I) = SIGNXX(I)*R-P
        SIGNYY(I) = SIGNYY(I)*R-P
        SIGNZZ(I) = SIGNZZ(I)*R-P
        SIGNXY(I) = SIGNXY(I)*R
        SIGNYZ(I) = SIGNYZ(I)*R
        SIGNZX(I) = SIGNZX(I)*R
        PLA(I)    = PLA(I) + DPLA
        DPLA1(I)  = DPLA
       ENDDO
      ENDIF
C-----------
      DO I=1,NEL
C
C      PLASTIC STRAIN RATE
       IF (VFLAG == 1) THEN 
         DPDT    = DPLA1(I) / MAX(EM20,TIMESTEP)
         EPSP(I) = ASRATE * DPDT + (ONE - ASRATE) * EPSP(I)
       ENDIF
C
       SIGY(I) = MAX(SIGY(I),YLD(I))
      ENDDO
      DO I=1,NEL
        IF(OFF(I).LT.ONE) IDEL7NOK = 1
      ENDDO
C-----------
      RETURN
      END
C
