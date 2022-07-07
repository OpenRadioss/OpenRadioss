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
Chd|  SIGEPS56                      source/materials/mat/mat056/sigeps56.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|-- calls ---------------
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|        VINTER2                       source/tools/curve/vinter.F   
Chd|        FINTER2                       source/tools/curve/vinter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS56(
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
     B     IPM    ,MAT    ,EPSP    ,IPLA    ,YLD     ,PLA    ,
     C    DPLA    ,AMU    )
CGW087     B     PM     ,MAT    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
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
C VISCMAX | NEL     | F | W | MAXIMUN DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr17_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
C
      INTEGER NEL, NUPARAM, NUVAR,IPT,IPLA,
     .   NGL(NEL),MAT(NEL),IPM(NPROPMI,*)
      my_real
     .   TIME,TIMESTEP,UPARAM(*),
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
     .    SOUNDSP(NEL),VISCMAX(NEL),DPLA(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real 
     .        UVAR(NEL,NUVAR), OFF(NEL),  YLD(NEL),PLA(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
      my_real
     .   FINTER2, TF(*)
      EXTERNAL FINTER2
C        Y = FINTER(IFUNC(J),X,NPF,TF,DYDX)
C        Y       : y = f(x)
C        X       : x
C        DYDX    : f'(x) = dy/dx
C        IFUNC(J): FUNCTION INDEX
C              J : FIRST(J=1), SECOND(J=2) .. FUNCTION USED FOR THIS LAW
C        NPF,TF  : FUNCTION PARAMETER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IADBUFV,J1,J2,JJ(MVSIZ),NFUNC,
     .        NRATE,IPOS1(MVSIZ),IPOS2(MVSIZ),IAD1(MVSIZ),
     .        ILEN1(MVSIZ),IAD2(MVSIZ),ILEN2(MVSIZ),
     .        IFUNC(100),NFUNCV,PFUN,
     .        IPOSP(MVSIZ),IADP(MVSIZ),ILENP(MVSIZ),NFUNCM,NRATEM,
     .        IPFLAG,IPARAM,NPAR,
     .        NINDX,INDX(MVSIZ),MX
      my_real
     .        E,NU,P,DAV,VM,R,FAC,EPST,EP1,EP2,EP3,EP4,EP5,EP6,
     .        E1,E2,E3,E4,E5,E6,C,CC,D,Y,YP,E42,E52,E62,EPST2,
     .        C1,G,G2,G3,
     .        EPSMAX,RATE(MVSIZ,2),YFAC(MVSIZ,2),
     .        Y1(MVSIZ),Y2(MVSIZ),H(MVSIZ),DYDX1(MVSIZ),
     .        DYDX2(MVSIZ),FAIL(MVSIZ),EPSR1,
     .        EPSR2,P0(MVSIZ),PFAC(MVSIZ),
     .        DFDP(MVSIZ)
c     .        ONE,ZERO,EM20
c      DATA ZERO/0.0/,ONE/1.0/,EM20/1.E-20/
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      IF (IVECTOR.NE.1) THEN
       MX = MAT(1)
       NFUNC  = IPM(10,MX)
       DO J=1,NFUNC
         IFUNC(J)=IPM(10+J,MX)
       ENDDO
      ELSE
        MX = MAT(1)
        NFUNCM = 0
        NFUNCV = IPM(10,MX)
        NFUNCM = MAX(NFUNCM,NFUNCV)

        DO J=1,NFUNCM
          IF(NFUNCV.GE.J) THEN
            IFUNC(J)=IPM(10+J,MX)
          ENDIF
        ENDDO
      ENDIF
C
      NRATEM = 0
C
      MX = MAT(1)
      IADBUFV = IPM(7,MX)-1
      E   = UPARAM(IADBUFV+2)
      G   = UPARAM(IADBUFV+5)
      G2  = TWO*G
      G3  = THREE*G
      NU  = UPARAM(IADBUFV+6)
      C1  = E/THREE/(ONE - TWO*NU)
C
      NRATE = NINT(UPARAM(IADBUFV+1))
      NRATEM = MAX(NRATEM,NRATE)
      EPSMAX=UPARAM(IADBUFV+6+2*NRATE+1)
      IF(EPSMAX.EQ.ZERO)THEN
        IF(TF(NPF(IFUNC(1)+1)-1).EQ.ZERO)THEN
         EPSMAX=TF(NPF(IFUNC(1)+1)-2)
        ELSE
         EPSMAX= EP30
        ENDIF
      ENDIF
      EPSR1 =UPARAM(IADBUFV+6+2*NRATE+2)
      IF(EPSR1.EQ.ZERO)EPSR1=EP30
      EPSR2 =UPARAM(IADBUFV+6+2*NRATE+3)
      IF(EPSR2.EQ.ZERO)EPSR2=TWOEP30
      IPFLAG = 0
      MX = MAT(1)
      NPAR   = IPM(9,MX)
      IPARAM = 15 + 2*NRATE
      DO I=1,NEL
        PFAC(I) = ONE
        PFUN = 0
        IF (NPAR.GE.IPARAM) THEN
          J = NRATE+1
          IFUNC(J)=IPM(10+J,MX)
          PFUN=NINT(UPARAM(IADBUFV+IPARAM))
          IF (PFUN.GT.0) IPFLAG = IPFLAG + 1
         ENDIF
      ENDDO
C
      IF (ISIGI.EQ.0) THEN
      IF(TIME.EQ.ZERO)THEN
        DO I=1,NEL
         UVAR(I,1)=ZERO
         UVAR(I,2)=ZERO
         DO J=1,NRATE
           UVAR(I,J+2)=ZERO
         ENDDO
         IF (PFUN.GT.0) UVAR(I,NRATE+5)=ZERO
        ENDDO
      ENDIF
      ENDIF
C-----------------------------------------------
C
      DO I=1,NEL
C
        PLA(I) = UVAR(I,1)
        P0(I) = -(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I))*THIRD
        DAV = (DEPSXX(I)+DEPSYY(I)+DEPSZZ(I))*THIRD
        SIGNXX(I)=SIGOXX(I)+P0(I)+G2*(DEPSXX(I)-DAV)
        SIGNYY(I)=SIGOYY(I)+P0(I)+G2*(DEPSYY(I)-DAV)
        SIGNZZ(I)=SIGOZZ(I)+P0(I)+G2*(DEPSZZ(I)-DAV)
        SIGNXY(I)=SIGOXY(I)+G *DEPSXY(I)
        SIGNYZ(I)=SIGOYZ(I)+G *DEPSYZ(I)
        SIGNZX(I)=SIGOZX(I)+G *DEPSZX(I)
C
        SOUNDSP(I) = SQRT((C1+FOUR*G/THREE)/RHO0(I))
        VISCMAX(I) = ZERO
        DPLA(I) = ZERO       
      ENDDO
C-------------------
C     STRAIN RATE
C-------------------
      DO I=1,NEL
C  087        DAV = (EPSPXX(I)+EPSPYY(I)+EPSPZZ(I))/3.
C  087        E1 = EPSPXX(I) - DAV
C  087        E2 = EPSPYY(I) - DAV
C  087        E3 = EPSPZZ(I) - DAV
C  087        E4 = 0.5*EPSPXY(I)
C  087        E5 = 0.5*EPSPYZ(I)
C  087        E6 = 0.5*EPSPZX(I)
C  087        EPSP(I) =.5*(E1**2+E2**2+E3**2) +E4**2+E5**2+E6**2
C  087        EPSP(I) = SQRT(3.*EPSP(I))/1.5
C-------------------
C     STRAIN principal 1, 4 newton iterations 
C-------------------
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
        CC = C*THIRD
        EPST = SQRT(-CC)
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
C-------------------
C     tension failure 
C-------------------
        FAIL(I) = MAX(ZERO,MIN(ONE,
     .            (EPSR2-EPST)/(EPSR2-EPSR1) ))
C
c jbm023
c
c        FAIL(I)=1.
      ENDDO
C-------------------
C     CRITERE
C-------------------
      DO I=1,NEL
            JJ(I) = 1
      ENDDO
      IF (IVECTOR.NE.1) THEN
       DO I=1,NEL
        DO J=2,NRATE-1
          IF(EPSP(I).GE.UPARAM(IADBUFV+6+J)) JJ(I) = J
        ENDDO
       ENDDO
      ELSE
        DO J=2,NRATEM-1
          DO I=1,NEL
            IF(NRATE-1.GE.J) THEN
              IF(EPSP(I).GE.UPARAM(IADBUFV+6+J)) JJ(I) = J
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C
      DO I=1,NEL
        RATE(I,1)=UPARAM(IADBUFV+6+JJ(I))
        RATE(I,2)=UPARAM(IADBUFV+6+JJ(I)+1)
        YFAC(I,1)=UPARAM(IADBUFV+6+NRATE+JJ(I))
        YFAC(I,2)=UPARAM(IADBUFV+6+NRATE+JJ(I)+1)
      ENDDO
C
      DO I=1,NEL
        J1 = JJ(I)
        J2 = J1+1
C  037        IPOS1(I) = NINT(UVAR(I,J1))
        IPOS1(I) = NINT(UVAR(I,J1+2))
        IAD1(I)  = NPF(IFUNC(J1)) / 2 + 1
        ILEN1(I) = NPF(IFUNC(J1)+1) / 2 - IAD1(I) - IPOS1(I)
C  037        IPOS2(I) = NINT(UVAR(I,J2))
        IPOS2(I) = NINT(UVAR(I,J2+2))
        IAD2(I)  = NPF(IFUNC(J2)) / 2 + 1
        ILEN2(I) = NPF(IFUNC(J2)+1) / 2 - IAD2(I) - IPOS2(I)
      ENDDO
C
      CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,PLA,DYDX1,Y1)
      CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL,PLA,DYDX2,Y2)
C
C---  PRESSURE DEPENDENT YIELD FUNCTION FACTOR
      IF (IPFLAG.EQ.NEL.AND.(IMACH.NE.3.OR.IPARIT.EQ.0)) THEN
C------- to optimize in SPMD & PARIT/ON case add a global flag 
C        in a case of homogenuous element groups (to do if needed)
        DO I=1,NEL
          IPOSP(I) = NINT(UVAR(I,NRATE+5))
          IADP(I)  = NPF(IFUNC(PFUN)) / 2 + 1
          ILENP(I) = NPF(IFUNC(PFUN)+1) / 2 - IADP(I) - IPOSP(I)
        ENDDO
        CALL VINTER2(TF,IADP,IPOSP,ILENP,NEL,P0,DFDP,PFAC)
        DO I=1,NEL
          UVAR(I,NRATE+5) = IPOSP(I)
        ENDDO
      ELSEIF (IPFLAG.GT.0) THEN
        DO I=1,NEL
          IF (PFUN.GT.0) THEN
            IPOSP(I) = NINT(UVAR(I,NRATE+5))
            IADP(I)  = NPF(IFUNC(PFUN)) / 2 + 1
            ILENP(I) = NPF(IFUNC(PFUN)+1) / 2 
     .                           - IADP(I) - IPOSP(I)
            PFAC(I)  = FINTER2(TF   ,IADP(I),IPOSP(I),ILENP(I),
     .                         P0(I),DFDP(I))
            UVAR(I,NRATE+5) = IPOSP(I)
          ENDIF
        ENDDO
      ENDIF
      DO I=1,NEL
        J1 = JJ(I)
        J2 = J1+1
        Y1(I)=Y1(I)*YFAC(I,1)
        Y2(I)=Y2(I)*YFAC(I,2)
        FAC   = (EPSP(I) - RATE(I,1))/(RATE(I,2) - RATE(I,1))
        YLD(I) = FAIL(I)*(Y1(I)    + FAC*(Y2(I)-Y1(I)))
        DYDX1(I)=DYDX1(I)*YFAC(I,1)
        DYDX2(I)=DYDX2(I)*YFAC(I,2)
        H(I)   = FAIL(I)*(DYDX1(I) + FAC*(DYDX2(I)-DYDX1(I)))
        H(I) = H(I) * MAX(ZERO, PFAC(I))
        YLD(I) = YLD(I) * MAX(ZERO, PFAC(I))

C  037        UVAR(I,J1) = IPOS1(I)
C  037        UVAR(I,J2) = IPOS2(I)
        UVAR(I,J1+2) = IPOS1(I)
        UVAR(I,J2+2) = IPOS2(I)
      ENDDO
C-------------------
C     PROJECTION
C-------------------
      IF(IPLA.EQ.0)THEN
       DO I=1,NEL
        VM =HALF*(SIGNXX(I)**2+SIGNYY(I)**2+SIGNZZ(I)**2)
     1          +SIGNXY(I)**2+SIGNYZ(I)**2+SIGNZX(I)**2
        VM =SQRT(THREE*VM)
        R = MIN(ONE,YLD(I)/ MAX(VM,EM20))
c        P = C1 * (RHO(I)/RHO0(I)- ONE)
        P = C1 * AMU(I)
        SIGNXX(I)=SIGNXX(I)*R-P
        SIGNYY(I)=SIGNYY(I)*R-P
        SIGNZZ(I)=SIGNZZ(I)*R-P
        SIGNXY(I)=SIGNXY(I)*R
        SIGNYZ(I)=SIGNYZ(I)*R
        SIGNZX(I)=SIGNZX(I)*R
        PLA(I)=PLA(I) + (ONE - R)*VM/MAX(G3+H(I),EM20)
        UVAR(I,1)=PLA(I)
        DPLA(I) = (ONE - R)*VM/MAX(G3+H(I),EM20)        
       ENDDO
      ELSEIF(IPLA.EQ.2)THEN
       DO I=1,NEL
        VM =HALF*(SIGNXX(I)**2+SIGNYY(I)**2+SIGNZZ(I)**2)
     1          +SIGNXY(I)**2+SIGNYZ(I)**2+SIGNZX(I)**2
        VM =SQRT(THREE*VM)
        R = MIN(ONE,YLD(I)/ MAX(VM,EM20))
c        P = C1 * (RHO(I)/RHO0(I)-ONE)
        P = C1 * AMU(I)
        SIGNXX(I)=SIGNXX(I)*R-P
        SIGNYY(I)=SIGNYY(I)*R-P
        SIGNZZ(I)=SIGNZZ(I)*R-P
        SIGNXY(I)=SIGNXY(I)*R
        SIGNYZ(I)=SIGNYZ(I)*R
        SIGNZX(I)=SIGNZX(I)*R
        PLA(I)=PLA(I) + (ONE-R)*VM/MAX(G3,EM20)
        UVAR(I,1)=PLA(I)
        DPLA(I) = (ONE-R)*VM/MAX(G3,EM20)        
       ENDDO
      ELSEIF(IPLA.EQ.1)THEN
       DO I=1,NEL
        VM =HALF*(SIGNXX(I)**2+SIGNYY(I)**2+SIGNZZ(I)**2)
     1          +SIGNXY(I)**2+SIGNYZ(I)**2+SIGNZX(I)**2
        VM =SQRT(THREE*VM)
        R = MIN(ONE,YLD(I)/ MAX(VM,EM20))
C       plastic strain increment.
        DPLA(I)=(ONE - R)*VM/MAX(G3+H(I),EM20)
C       actual yeld stress.
        YLD(I)=MAX(YLD(I)+DPLA(I)*H(I),ZERO)
        R = MIN(ONE,YLD(I)/ MAX(VM,EM20))
C
c        P = C1 * (RHO(I)/RHO0(I)-ONE)
        P = C1 * AMU(I)
        SIGNXX(I)=SIGNXX(I)*R-P
        SIGNYY(I)=SIGNYY(I)*R-P
        SIGNZZ(I)=SIGNZZ(I)*R-P
        SIGNXY(I)=SIGNXY(I)*R
        SIGNYZ(I)=SIGNYZ(I)*R
        SIGNZX(I)=SIGNZX(I)*R
       PLA(I)=PLA(I) + DPLA(I)
        UVAR(I,1)=PLA(I)
       ENDDO
      ENDIF
C-------------------
      DO I=1,NEL
        IF(OFF(I).LT.EM01) OFF(I)=ZERO
        IF(OFF(I).LT.ONE) OFF(I)=OFF(I)*FOUR_OVER_5
      ENDDO
C
      NINDX=0
      DO I=1,NEL
        IF(PLA(I).GT.EPSMAX.AND.OFF(I).EQ.ONE) THEN
          OFF(I)=FOUR_OVER_5
          NINDX=NINDX+1
          INDX(NINDX)=I
          IDEL7NOK = 1
        ENDIF
      ENDDO
      IF(NINDX.GT.0)THEN
        DO J=1,NINDX
#include "lockon.inc"
        WRITE(IOUT, 1000) NGL(INDX(J))
        WRITE(ISTDO,1100) NGL(INDX(J)),TT
#include "lockoff.inc"
        ENDDO
      ENDIF
C
 1000 FORMAT(1X,'RUPTURE OF SOLID ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'RUPTURE OF SOLID ELEMENT NUMBER ',I10,
     .          ' AT TIME :',G11.4)
C
      RETURN
      END
C
