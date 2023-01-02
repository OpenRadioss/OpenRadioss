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
Chd|  SIGEPS74                      source/materials/mat/mat074/sigeps74.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|-- calls ---------------
Chd|        TABLE_VINTERP                 source/tools/curve/table_tools.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE SIGEPS74(
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
     C     DPLA   ,ETSE   ,JTHE    ,TEMPEL  ,TABLE   ,SEQ_OUTPUT,
     D     AMU    ,ISEQ   )
C-----------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
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
C VISCMAX | NEL     | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr17_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
C=======================================================================
      INTEGER NEL, NUPARAM, NUVAR,IPT,IPLA, JTHE,
     .   NGL(NEL),MAT(NEL),IPM(NPROPMI,*),ISEQ
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
     .   EPSP(NEL),ETSE(NEL), TEMPEL(NEL),SEQ_OUTPUT(NEL),
     .   AMU(NEL)
      TYPE(TTABLE) TABLE(*)
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
     .        UVAR(NEL,NUVAR), OFF(NEL),  YLD(NEL), PLA(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
      my_real
     .   FINTER2, TF(*),FINTER
      EXTERNAL FINTER2,FINTER
C-----------------------------------------------
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
      INTEGER I,J,IADBUF,J1,J2,ITABLE,
     .        IPARAM,NPAR,OPTE,IFUNCE,
     .        NINDX,INDX(MVSIZ)
      my_real
     .        E,NU,P,DAV,R,FAC,EPST,EP1,EP2,EP3,EP4,EP5,EP6,
     .        E1,E2,E3,E4,E5,E6,C,CC,D,Y,YP,E42,E52,E62,EPST2,
     .        C1,G,G2,G3,FF,GG,HH,NN,LL,MM,
     .        EPSMAX,H(MVSIZ),CRI(MVSIZ),
     .        FAIL(MVSIZ),EPSR1,EPSR2,P0(MVSIZ),HKIN,
     .       FISOKIN,SIGEXX(MVSIZ),SIGEYY(MVSIZ),SIGEZZ(MVSIZ),
     .       SIGEXY(MVSIZ),SIGEYZ(MVSIZ),SIGEZX(MVSIZ),
     .       DEXX,DEYY,DEXY,DEZZ,DEYZ,DEZX,DSXX,DSYY,DSZZ,DSXY,DSYZ,
     .       DSZX,SIGPXX,SIGPYY,SIGPXY,SIGPYZ,SIGPZX,SIGPZZ,ALPHA,
     .       XFAC, YFAC,E0(MVSIZ), G1(MVSIZ), G21(MVSIZ), G31(MVSIZ), 
     .       C11(MVSIZ),ESCALE(MVSIZ),EINF,DYDXE(MVSIZ),CE
      my_real 
     .        YK(MVSIZ), DYDX(MVSIZ),
     .        COEF(MVSIZ), TEMP(MVSIZ), VOL0, RHOCP
      my_real,
     .        DIMENSION(NEL,3) :: XX3
      my_real,
     .        DIMENSION(NEL,2) :: XX2
      INTEGER,DIMENSION(NEL,3) :: IPOS3
      INTEGER,DIMENSION(NEL,2) :: IPOS2
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------

      DO I=1,NEL
        ETSE(I) = ONE
      ENDDO
C
      ITABLE = IPM(227,MAT(1))
      IADBUF = IPM(7,MAT(1))-1
      FISOKIN=UPARAM(IADBUF+1)
      OPTE    = UPARAM(IADBUF+25)
      CE    = UPARAM(IADBUF+27)
      EINF  = UPARAM(IADBUF+26)

      IF (OPTE==1.OR. CE > ZERO)THEN
       DO I=1,NEL     
         E0(I)   = UPARAM(IADBUF+2)
         G1(I)   = UPARAM(IADBUF+5)
         G21(I)  = UPARAM(IADBUF+18)
         G31(I)  = UPARAM(IADBUF+19)
         C11(I)  = UPARAM(IADBUF+20)
       ENDDO
       NU  = UPARAM(IADBUF+6)
       EPSMAX= UPARAM(IADBUF+15)
       IF(EPSMAX==ZERO)THEN
c      NXK=SIZE(TABLE(ITABLE)%X(1)%VALUES)
c      IF(TABLE(ITABLE)%Y%VALUES(NXK)==ZERO)THEN
c       EPSMAX = TABLE(ITABLE)%X(1)%VALUES(NXK)
c      ELSE
        EPSMAX = EP30
c      ENDIF
       ENDIF
C 
       IF (OPTE == 1)THEN  
        DO I=1,NEL
          PLA(I) = UVAR(I,1)     
          IF(PLA(I) > ZERO)THEN 
             IFUNCE  =  UPARAM(IADBUF+24)                                                    
             ESCALE(I) = FINTER(KFUNC(IFUNCE),PLA(I),NPF,TF,DYDXE(I))     
             E0(I) =  ESCALE(I)* E0(I)   
             G1(I) =  HALF*E0(I)/(ONE+NU) 
             G21(I) = TWO*G1(I)                                  
             G31(I) = THREE*G1(I) 
             C11(I) =E0(I)/THREE/(ONE - TWO*NU)                          
           ENDIF 
         ENDDO                                               
       ELSEIF ( CE /= ZERO) THEN   
         DO I=1,NEL       
           PLA(I) = UVAR(I,1)                                   
           IF(PLA(I) > ZERO)THEN                                                        
             E0(I) = E0(I)-(E0(I)-EINF)*(ONE-EXP(-CE*PLA(I)))              
             G1(I) =  HALF*E0(I)/(ONE+NU)   
             G21(I) = TWO*G1(I)                                  
             G31(I) = THREE*G1(I)                                               
             C11(I) = E0(I)/THREE/(ONE - TWO*NU)               
           ENDIF 
          ENDDO                                                                   
       ENDIF
       XFAC  =UPARAM(IADBUF+13)
       YFAC  =UPARAM(IADBUF+14)
C
       EPSR1 =UPARAM(IADBUF+16)
       IF(EPSR1==ZERO)EPSR1=EP30
       EPSR2 =UPARAM(IADBUF+17)
       IF(EPSR2==ZERO)EPSR2=TWOEP30
C
      FF        = UPARAM(IADBUF+7)
      GG        = UPARAM(IADBUF+8)
      HH        = UPARAM(IADBUF+9)
      LL        = UPARAM(IADBUF+10)
      MM        = UPARAM(IADBUF+11)
      NN        = UPARAM(IADBUF+12)
C
      IF (ISIGI==0) THEN
        IF(TIME==ZERO)THEN
          DO I=1,NEL
            UVAR(I,1)=ZERO
            UVAR(I,2)=ZERO
            UVAR(I,3)=ZERO
            UVAR(I,4)=ZERO
            DO J=1,6
             UVAR(I,4 + J ) = ZERO
            ENDDO
          ENDDO
        ENDIF
      ENDIF
C------------------------------------------
C     ECROUISSAGE CINE
C------------------------------------------
      IF (FISOKIN/=ZERO) THEN
        DO I=1,NEL
          SIGOXX(I) = SIGOXX(I) - UVAR(I,5)
          SIGOYY(I) = SIGOYY(I) - UVAR(I,6)
          SIGOZZ(I) = SIGOZZ(I) - UVAR(I,7)
          SIGOXY(I) = SIGOXY(I) - UVAR(I,8)
          SIGOYZ(I) = SIGOYZ(I) - UVAR(I,9)
          SIGOZX(I) = SIGOZX(I) - UVAR(I,10)
        ENDDO
      ENDIF           
C------------------------------------------
      DO I=1,NEL
C
        PLA(I) = UVAR(I,1)
        P0(I) = -(SIGOXX(I)+SIGOYY(I)+SIGOZZ(I))*THIRD
        DAV = (DEPSXX(I)+DEPSYY(I)+DEPSZZ(I))*THIRD
        SIGNXX(I)=SIGOXX(I)+P0(I)+G21(I)*(DEPSXX(I)-DAV)
        SIGNYY(I)=SIGOYY(I)+P0(I)+G21(I)*(DEPSYY(I)-DAV)
        SIGNZZ(I)=SIGOZZ(I)+P0(I)+G21(I)*(DEPSZZ(I)-DAV)
        SIGNXY(I)=SIGOXY(I)+G1(I) *DEPSXY(I)
        SIGNYZ(I)=SIGOYZ(I)+G1(I) *DEPSYZ(I)
        SIGNZX(I)=SIGOZX(I)+G1(I) *DEPSZX(I)
C
        SIGEXX(I) = SIGNXX(I)
        SIGEYY(I) = SIGNYY(I)
        SIGEZZ(I) = SIGNZZ(I)               
        SIGEXY(I) = SIGNXY(I)
        SIGEYZ(I) = SIGNYZ(I)
        SIGEZX(I) = SIGNZX(I)
        SOUNDSP(I) = SQRT((C11(I)+FOUR*G1(I)/THREE)/RHO0(I))
        VISCMAX(I) = ZERO
        DPLA(I) =ZERO
                
      ENDDO
C-------------------
C     STRAIN RATE
C-------------------
      DO I=1,NEL
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
        C = - HALF *(E1*E1 + E2*E2 + E3*E3) - E42 - E52 - E62
        D = - E1*E2*E3 + E1*E52 + E2*E62 + E3*E42
     &      - TWO*E4*E5*E6 
        CC = C*THIRD
        EPST = SQRT(-CC)
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
          YP = THREE*EPST2 + C
          IF(YP/=ZERO)EPST = EPST - Y/YP
        ENDIF
        EPST = EPST + DAV
C-------------------
C     tension failure 
C-------------------
        FAIL(I) = MAX(ZERO,MIN(ONE,
     .            (EPSR2-EPST)/(EPSR2-EPSR1) ))
      ENDDO
C-------------------
C     CRITERE
C-------------------
      IF(TABLE(ITABLE)%NDIM==2)THEN
        DO I=1,NEL
          IPOS2(I,1) = NINT(UVAR(I,2))
          IPOS2(I,2) = NINT(UVAR(I,3))
C
          XX2(I,1)=PLA (I)
          XX2(I,2)=EPSP(I)*XFAC
        END DO      
C
        CALL TABLE_VINTERP(TABLE(ITABLE),NEL,IPOS2,XX2,YLD,DYDX)
C
        DO I=1,NEL
          YLD(I) = YFAC*YLD(I)
          YLD(I) = FAIL(I)*YLD(I)
          H(I)   = FAIL(I)*DYDX(I)
          UVAR(I,2) = IPOS2(I,1)
          UVAR(I,3) = IPOS2(I,2)
        END DO
C-----
        IF(FISOKIN/=ZERO)THEN
         DO I=1,NEL
           IPOS2(I,1) = 1
C
           XX2(I,1)=ZERO
           XX2(I,2)=EPSP(I)*XFAC
         END DO      
         CALL TABLE_VINTERP(TABLE(ITABLE),NEL,IPOS2,XX2,YK,DYDX)

         DO I=1,NEL
C          ECROUISSAGE CINEMATIQUE
           YLD(I) = (ONE-FISOKIN) * YLD(I) + 
     .         FISOKIN * FAIL(I) * YFAC * YK(I)
           YLD(I) = MAX(YLD(I),EM20)
         ENDDO
        END IF
      ELSE
C-----------------------------------------------
C  Calcul de la tempurature. (conduction ou adiabatique)
C--------------------    
        DO I=1,NEL    
          COEF(I) = ONE
        END DO
C
        IF(JTHE < 0 ) THEN
         DO I=1,NEL     
           TEMP(I) = TEMPEL(I)
         ENDDO
        ELSE
         RHOCP=UPARAM(IADBUF+23)
         DO I=1,NEL
           TEMP(I) = UPARAM(IADBUF + 22)
           TEMP(I) = TEMP(I) 
     .             + COEF(I)*RHOCP*EINT(I)/MAX(EM15,VOLUME(I))
         ENDDO
        ENDIF
C
        DO I=1,NEL
          IPOS3(I,1) = NINT(UVAR(I,2))
          IPOS3(I,2) = NINT(UVAR(I,3))
          IPOS3(I,3) = NINT(UVAR(I,4))
C
          XX3(I,1)=PLA (I)
          XX3(I,2)=EPSP(I)*XFAC
          XX3(I,3)=TEMP(I)
        END DO      
C
        CALL TABLE_VINTERP(TABLE(ITABLE),NEL,IPOS3,XX3,YLD,DYDX)
C
        DO I=1,NEL
          YLD(I) = YFAC*YLD(I)
          YLD(I) = FAIL(I)*YLD(I)
          H(I)   = FAIL(I)*DYDX(I)
          UVAR(I,2) = IPOS3(I,1)
          UVAR(I,3) = IPOS3(I,2)
          UVAR(I,4) = IPOS3(I,3)
        END DO
C-----
        IF(FISOKIN/=ZERO)THEN
         DO I=1,NEL
           IPOS3(I,1) = 1
C
           XX3(I,1)=ZERO
           XX3(I,2)=EPSP(I)*XFAC
           XX3(I,3)=TEMP(I)
         END DO      
         CALL TABLE_VINTERP(TABLE(ITABLE),NEL,IPOS3,XX3,YK,DYDX)

         DO I=1,NEL
C          ECROUISSAGE CINEMATIQUE
           YLD(I) = (ONE-FISOKIN) * YLD(I) + 
     .        FISOKIN * FAIL(I) * YFAC * YK(I)
           YLD(I) = MAX(YLD(I),EM20)
         ENDDO
        END IF
      END IF
C-------------------
C     PROJECTION
C-------------------
      IF(IPLA==0)THEN
       DO I=1,NEL
        CRI(I) = 
     .    FF*(SIGNYY(I) - SIGNZZ(I))**2 + GG*(SIGNZZ(I) - SIGNXX(I))**2
     .  + HH*(SIGNXX(I) - SIGNYY(I))**2 + TWO*LL*SIGNYZ(I)**2 
     .  + TWO*MM*SIGNZX(I)**2 + TWO*NN*SIGNXY(I)**2
        CRI(I) = SQRT(CRI(I))
        R = MIN(ONE,YLD(I)/ MAX(CRI(I),EM20))
c        P = C11(I) * (RHO(I)/RHO0(I)- ONE)
        P = C11(I) * AMU(I)
        SIGNXX(I)=SIGNXX(I)*R-P
        SIGNYY(I)=SIGNYY(I)*R-P
        SIGNZZ(I)=SIGNZZ(I)*R-P
        SIGNXY(I)=SIGNXY(I)*R
        SIGNYZ(I)=SIGNYZ(I)*R
        SIGNZX(I)=SIGNZX(I)*R
        PLA(I)=PLA(I) + (ONE - R)*CRI(I)/MAX(G31(I)+H(I),EM20)
        UVAR(I,1)=PLA(I)
        DPLA(I) = (ONE - R)*CRI(I)/MAX(G31(I)+H(I),EM20)         
       ENDDO
      ELSEIF(IPLA==2)THEN
       DO I=1,NEL
        CRI(I) = 
     .    FF*(SIGNYY(I) - SIGNZZ(I))**2 + GG*(SIGNZZ(I) - SIGNXX(I))**2
     .  + HH*(SIGNXX(I) - SIGNYY(I))**2 + TWO*LL*SIGNYZ(I)**2 
     .  + TWO*MM*SIGNZX(I)**2 + TWO*NN*SIGNXY(I)**2
        CRI(I) = SQRT(CRI(I))
        R = MIN(ONE,YLD(I)/ MAX(CRI(I),EM20))
c        P = C11(I) * (RHO(I)/RHO0(I)-ONE)
        P = C11(I) * AMU(I)
        SIGNXX(I)=SIGNXX(I)*R-P
        SIGNYY(I)=SIGNYY(I)*R-P
        SIGNZZ(I)=SIGNZZ(I)*R-P
        SIGNXY(I)=SIGNXY(I)*R
        SIGNYZ(I)=SIGNYZ(I)*R
        SIGNZX(I)=SIGNZX(I)*R
        PLA(I)=PLA(I) + (ONE-R)*CRI(I)/MAX(G31(I),EM20)
        UVAR(I,1)=PLA(I)
        DPLA(I) =  (ONE-R)*CRI(I)/MAX(G31(I),EM20)       
       ENDDO
      ELSEIF(IPLA==1)THEN
       DO I=1,NEL
        CRI(I) = 
     .    FF*(SIGNYY(I) - SIGNZZ(I))**2 + GG*(SIGNZZ(I) - SIGNXX(I))**2
     .  + HH*(SIGNXX(I) - SIGNYY(I))**2 + TWO*LL*SIGNYZ(I)**2 
     .  + TWO*MM*SIGNZX(I)**2 + TWO*NN*SIGNXY(I)**2
        CRI(I) = SQRT(CRI(I))
        R = MIN(ONE,YLD(I)/ MAX(CRI(I),EM20))
C       plastic strain increment.
        DPLA(I)=(ONE - R)*CRI(I)/MAX(G31(I)+H(I),EM20)
C       actual yield stress.
        YLD(I)=MAX(YLD(I)+(ONE - FISOKIN)*DPLA(I)*H(I),ZERO)
        R = MIN(ONE,YLD(I)/ MAX(CRI(I),EM20))
C
c        P = C11(I) * (RHO(I)/RHO0(I)-ONE)
        P = C11(I) * AMU(I)
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
C------------------------------------------
C     ECROUISSAGE CINE
C------------------------------------------
      IF (FISOKIN/=ZERO) THEN
        DO I=1,NEL
c         P = C11(I) * (RHO(I)/RHO0(I)-ONE)
         P = C11(I) * AMU(I)
         DSXX = SIGEXX(I) - SIGNXX(I) - P 
         DSYY = SIGEYY(I) - SIGNYY(I) - P
         DSZZ = SIGEZZ(I) - SIGNZZ(I) - P
         DSXY = SIGEXY(I) - SIGNXY(I)
         DSYZ = SIGEYZ(I) - SIGNYZ(I)
         DSZX = SIGEZX(I) - SIGNZX(I)
C        
         HKIN = TWO_THIRD*FISOKIN*H(I)
         ALPHA = HKIN/(G21(I)+HKIN)  
         SIGPXX = ALPHA*DSXX 
         SIGPYY = ALPHA*DSYY
         SIGPZZ = ALPHA*DSZZ 
         SIGPXY = ALPHA*DSXY
         SIGPYZ = ALPHA*DSYZ
         SIGPZX = ALPHA*DSZX
C        
         UVAR(I, 5) = UVAR(I, 5) + SIGPXX 
         UVAR(I, 6) = UVAR(I, 6) + SIGPYY
         UVAR(I, 7) = UVAR(I, 7) + SIGPZZ
         UVAR(I, 8) = UVAR(I, 8) + SIGPXY
         UVAR(I, 9) = UVAR(I, 9) + SIGPYZ
         UVAR(I,10) = UVAR(I,10) + SIGPZX         
C         
         SIGNXX(I) = SIGNXX(I) + UVAR(I, 5)
         SIGNYY(I) = SIGNYY(I) + UVAR(I, 6)
         SIGNZZ(I) = SIGNZZ(I) + UVAR(I, 7)
         SIGNXY(I) = SIGNXY(I) + UVAR(I, 8)
         SIGNYZ(I) = SIGNYZ(I) + UVAR(I, 9)
         SIGNZX(I) = SIGNZX(I) + UVAR(I,10)
       ENDDO
      ENDIF

      IF (IMPL_S>0) THEN
       DO I=1,NEL
         IF(DPLA(I)>0) ETSE(I)= H(I)/G21(I)
       ENDDO
c       IF (IKT==4)
c     .     CALL PUT_SIGE0(NEL,SIGEXX,SIGEYY,SIGEZZ,SIGEXY,
c     .                   SIGEYZ,SIGEZX,DPLA,G   ,H     )
      ENDIF
      ELSE !opte==1 CE/=0
      ITABLE = IPM(227,MAT(1))
      IADBUF = IPM(7,MAT(1))-1
      FISOKIN=UPARAM(IADBUF+1)
      
      E   = UPARAM(IADBUF+2)
      G   = UPARAM(IADBUF+5)
      G2  = UPARAM(IADBUF+18)
      G3  = UPARAM(IADBUF+19)
      NU  = UPARAM(IADBUF+6)
      C1  = UPARAM(IADBUF+20)
      EPSMAX= UPARAM(IADBUF+15)
      IF(EPSMAX==ZERO)THEN
c      NXK=SIZE(TABLE(ITABLE)%X(1)%VALUES)
c      IF(TABLE(ITABLE)%Y%VALUES(NXK)==ZERO)THEN
c       EPSMAX = TABLE(ITABLE)%X(1)%VALUES(NXK)
c      ELSE
        EPSMAX = EP30
c      ENDIF
      ENDIF
C
      XFAC  =UPARAM(IADBUF+13)
      YFAC  =UPARAM(IADBUF+14)
C
      EPSR1 =UPARAM(IADBUF+16)
      IF(EPSR1==ZERO)EPSR1=EP30
      EPSR2 =UPARAM(IADBUF+17)
      IF(EPSR2==ZERO)EPSR2=TWOEP30
C
      FF        = UPARAM(IADBUF+7)
      GG        = UPARAM(IADBUF+8)
      HH        = UPARAM(IADBUF+9)
      LL        = UPARAM(IADBUF+10)
      MM        = UPARAM(IADBUF+11)
      NN        = UPARAM(IADBUF+12)
C
      IF (ISIGI==0) THEN
        IF(TIME==ZERO)THEN
          DO I=1,NEL
            UVAR(I,1)=ZERO
            UVAR(I,2)=ZERO
            UVAR(I,3)=ZERO
            UVAR(I,4)=ZERO
            DO J=1,6
             UVAR(I,4 + J ) = ZERO
            ENDDO
          ENDDO
        ENDIF
      ENDIF
C------------------------------------------
C     ECROUISSAGE CINE
C------------------------------------------
      IF (FISOKIN/=ZERO) THEN
        DO I=1,NEL
          SIGOXX(I) = SIGOXX(I) - UVAR(I,5)
          SIGOYY(I) = SIGOYY(I) - UVAR(I,6)
          SIGOZZ(I) = SIGOZZ(I) - UVAR(I,7)
          SIGOXY(I) = SIGOXY(I) - UVAR(I,8)
          SIGOYZ(I) = SIGOYZ(I) - UVAR(I,9)
          SIGOZX(I) = SIGOZX(I) - UVAR(I,10)
        ENDDO
      ENDIF           
C------------------------------------------
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
        SIGEXX(I) = SIGNXX(I)
        SIGEYY(I) = SIGNYY(I)
        SIGEZZ(I) = SIGNZZ(I)               
        SIGEXY(I) = SIGNXY(I)
        SIGEYZ(I) = SIGNYZ(I)
        SIGEZX(I) = SIGNZX(I)
        SOUNDSP(I) = SQRT((C1+FOUR*G/THREE)/RHO0(I))
        VISCMAX(I) = ZERO
        DPLA(I) =ZERO
                
      ENDDO
C-------------------
C     STRAIN RATE
C-------------------
      DO I=1,NEL
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
        C = - HALF *(E1*E1 + E2*E2 + E3*E3) - E42 - E52 - E62
        D = - E1*E2*E3 + E1*E52 + E2*E62 + E3*E42
     &      - TWO*E4*E5*E6 
        CC = C*THIRD
        EPST = SQRT(-CC)
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
          YP = THREE*EPST2 + C
          IF(YP/=ZERO)EPST = EPST - Y/YP
        ENDIF
        EPST = EPST + DAV
C-------------------
C     tension failure 
C-------------------
        FAIL(I) = MAX(ZERO,MIN(ONE,
     .            (EPSR2-EPST)/(EPSR2-EPSR1) ))
      ENDDO
C-------------------
C     CRITERE
C-------------------
      IF(TABLE(ITABLE)%NDIM==2)THEN
        DO I=1,NEL
          IPOS2(I,1) = NINT(UVAR(I,2))
          IPOS2(I,2) = NINT(UVAR(I,3))
C
          XX2(I,1)=PLA (I)
          XX2(I,2)=EPSP(I)*XFAC
        END DO      
C
        CALL TABLE_VINTERP(TABLE(ITABLE),NEL,IPOS2,XX2,YLD,DYDX)
C
        DO I=1,NEL
          YLD(I) = YFAC*YLD(I)
          YLD(I) = FAIL(I)*YLD(I)
          H(I)   = FAIL(I)*DYDX(I)
          UVAR(I,2) = IPOS2(I,1)
          UVAR(I,3) = IPOS2(I,2)
        END DO
C-----
        IF(FISOKIN/=ZERO)THEN
         DO I=1,NEL
           IPOS2(I,1) = 1
C
           XX2(I,1)=ZERO
           XX2(I,2)=EPSP(I)*XFAC
         END DO      
         CALL TABLE_VINTERP(TABLE(ITABLE),NEL,IPOS2,XX2,YK,DYDX)

         DO I=1,NEL
C          ECROUISSAGE CINEMATIQUE
           YLD(I) = (ONE-FISOKIN) * YLD(I) + 
     .         FISOKIN * FAIL(I) * YFAC * YK(I)
           YLD(I) = MAX(YLD(I),EM20)
         ENDDO
        END IF
      ELSE
C-----------------------------------------------
C  Calcul de la tempurature. (conduction ou adiabatique)
C--------------------    
        DO I=1,NEL    
          COEF(I) = ONE
        END DO
C
        IF(JTHE < 0 ) THEN
         DO I=1,NEL     
           TEMP(I) = TEMPEL(I)
         ENDDO
        ELSE
         RHOCP=UPARAM(IADBUF+23)
         DO I=1,NEL
           TEMP(I) = UPARAM(IADBUF + 22)
           TEMP(I) = TEMP(I) 
     .             + COEF(I)*RHOCP*EINT(I)/MAX(EM15,VOLUME(I))
         ENDDO
        ENDIF
C
        DO I=1,NEL
          IPOS3(I,1) = NINT(UVAR(I,2))
          IPOS3(I,2) = NINT(UVAR(I,3))
          IPOS3(I,3) = NINT(UVAR(I,4))
C
          XX3(I,1)=PLA (I)
          XX3(I,2)=EPSP(I)*XFAC
          XX3(I,3)=TEMP(I)
        END DO      
C
        CALL TABLE_VINTERP(TABLE(ITABLE),NEL,IPOS3,XX3,YLD,DYDX)
C
        DO I=1,NEL
          YLD(I) = YFAC*YLD(I)
          YLD(I) = FAIL(I)*YLD(I)
          H(I)   = FAIL(I)*DYDX(I)
          UVAR(I,2) = IPOS3(I,1)
          UVAR(I,3) = IPOS3(I,2)
          UVAR(I,4) = IPOS3(I,3)
        END DO
C-----
        IF(FISOKIN/=ZERO)THEN
         DO I=1,NEL
           IPOS3(I,1) = 1
C
           XX3(I,1)=ZERO
           XX3(I,2)=EPSP(I)*XFAC
           XX3(I,3)=TEMP(I)
         END DO      
         CALL TABLE_VINTERP(TABLE(ITABLE),NEL,IPOS3,XX3,YK,DYDX)

         DO I=1,NEL
C          ECROUISSAGE CINEMATIQUE
           YLD(I) = (ONE-FISOKIN) * YLD(I) + 
     .        FISOKIN * FAIL(I) * YFAC * YK(I)
           YLD(I) = MAX(YLD(I),EM20)
         ENDDO
        END IF
      END IF
C-------------------
C     PROJECTION
C-------------------
      IF(IPLA==0)THEN
       DO I=1,NEL
        CRI(I) = 
     .    FF*(SIGNYY(I) - SIGNZZ(I))**2 + GG*(SIGNZZ(I) - SIGNXX(I))**2
     .  + HH*(SIGNXX(I) - SIGNYY(I))**2 + TWO*LL*SIGNYZ(I)**2 
     .  + TWO*MM*SIGNZX(I)**2 + TWO*NN*SIGNXY(I)**2
        CRI(I) = SQRT(CRI(I))
        R = MIN(ONE,YLD(I)/ MAX(CRI(I),EM20))
c        P = C1 * (RHO(I)/RHO0(I)- ONE)
        P = C1 * AMU(I)
        SIGNXX(I)=SIGNXX(I)*R-P
        SIGNYY(I)=SIGNYY(I)*R-P
        SIGNZZ(I)=SIGNZZ(I)*R-P
        SIGNXY(I)=SIGNXY(I)*R
        SIGNYZ(I)=SIGNYZ(I)*R
        SIGNZX(I)=SIGNZX(I)*R
        PLA(I)=PLA(I) + (ONE - R)*CRI(I)/MAX(G3+H(I),EM20)
        UVAR(I,1)=PLA(I)
        DPLA(I) = (ONE - R)*CRI(I)/MAX(G3+H(I),EM20)         
       ENDDO
      ELSEIF(IPLA==2)THEN
       DO I=1,NEL
        CRI(I) = 
     .    FF*(SIGNYY(I) - SIGNZZ(I))**2 + GG*(SIGNZZ(I) - SIGNXX(I))**2
     .  + HH*(SIGNXX(I) - SIGNYY(I))**2 + TWO*LL*SIGNYZ(I)**2 
     .  + TWO*MM*SIGNZX(I)**2 + TWO*NN*SIGNXY(I)**2
        CRI(I) = SQRT(CRI(I))
        R = MIN(ONE,YLD(I)/ MAX(CRI(I),EM20))
c        P = C1 * (RHO(I)/RHO0(I)-ONE)
        P = C1 * AMU(I)
        SIGNXX(I)=SIGNXX(I)*R-P
        SIGNYY(I)=SIGNYY(I)*R-P
        SIGNZZ(I)=SIGNZZ(I)*R-P
        SIGNXY(I)=SIGNXY(I)*R
        SIGNYZ(I)=SIGNYZ(I)*R
        SIGNZX(I)=SIGNZX(I)*R
        PLA(I)=PLA(I) + (ONE-R)*CRI(I)/MAX(G3,EM20)
        UVAR(I,1)=PLA(I)
        DPLA(I) =  (ONE-R)*CRI(I)/MAX(G3,EM20)       
       ENDDO
      ELSEIF(IPLA==1)THEN
       DO I=1,NEL
        CRI(I) = 
     .    FF*(SIGNYY(I) - SIGNZZ(I))**2 + GG*(SIGNZZ(I) - SIGNXX(I))**2
     .  + HH*(SIGNXX(I) - SIGNYY(I))**2 + TWO*LL*SIGNYZ(I)**2 
     .  + TWO*MM*SIGNZX(I)**2 + TWO*NN*SIGNXY(I)**2
        CRI(I) = SQRT(CRI(I))
        R = MIN(ONE,YLD(I)/ MAX(CRI(I),EM20))
C       plastic strain increment.
        DPLA(I)=(ONE - R)*CRI(I)/MAX(G3+H(I),EM20)
C       actual yield stress.
        YLD(I)=MAX(YLD(I)+(ONE - FISOKIN)*DPLA(I)*H(I),ZERO)
        R = MIN(ONE,YLD(I)/ MAX(CRI(I),EM20))
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
C------------------------------------------
C     ECROUISSAGE CINE
C------------------------------------------
      IF (FISOKIN/=ZERO) THEN
        DO I=1,NEL
         P = C1 * (RHO(I)/RHO0(I)-ONE)
         DSXX = SIGEXX(I) - SIGNXX(I) - P 
         DSYY = SIGEYY(I) - SIGNYY(I) - P
         DSZZ = SIGEZZ(I) - SIGNZZ(I) - P
         DSXY = SIGEXY(I) - SIGNXY(I)
         DSYZ = SIGEYZ(I) - SIGNYZ(I)
         DSZX = SIGEZX(I) - SIGNZX(I)
C        
         HKIN = TWO_THIRD*FISOKIN*H(I)
         ALPHA = HKIN/(G2+HKIN)  
         SIGPXX = ALPHA*DSXX 
         SIGPYY = ALPHA*DSYY
         SIGPZZ = ALPHA*DSZZ 
         SIGPXY = ALPHA*DSXY
         SIGPYZ = ALPHA*DSYZ
         SIGPZX = ALPHA*DSZX
C        
         UVAR(I, 5) = UVAR(I, 5) + SIGPXX 
         UVAR(I, 6) = UVAR(I, 6) + SIGPYY
         UVAR(I, 7) = UVAR(I, 7) + SIGPZZ
         UVAR(I, 8) = UVAR(I, 8) + SIGPXY
         UVAR(I, 9) = UVAR(I, 9) + SIGPYZ
         UVAR(I,10) = UVAR(I,10) + SIGPZX         
C         
         SIGNXX(I) = SIGNXX(I) + UVAR(I, 5)
         SIGNYY(I) = SIGNYY(I) + UVAR(I, 6)
         SIGNZZ(I) = SIGNZZ(I) + UVAR(I, 7)
         SIGNXY(I) = SIGNXY(I) + UVAR(I, 8)
         SIGNYZ(I) = SIGNYZ(I) + UVAR(I, 9)
         SIGNZX(I) = SIGNZX(I) + UVAR(I,10)
       ENDDO
      ENDIF

      IF (IMPL_S>0) THEN
       DO I=1,NEL
         IF(DPLA(I)>0) ETSE(I)= H(I)/G2
       ENDDO
c       IF (IKT==4)
c     .     CALL PUT_SIGE0(NEL,SIGEXX,SIGEYY,SIGEZZ,SIGEXY,
c     .                   SIGEYZ,SIGEZX,DPLA,G   ,H     )
      ENDIF
c
      ENDIF ! opte==1 CE/=0
C-------------------
      DO I=1,NEL
        IF(OFF(I)<EM01) OFF(I)=ZERO
        IF(OFF(I)<ONE) OFF(I)=OFF(I)*FOUR_OVER_5
      ENDDO
C
      NINDX=0
      DO I=1,NEL
        IF(PLA(I)>EPSMAX.AND.OFF(I)==ONE) THEN
          OFF(I)=FOUR_OVER_5
          NINDX=NINDX+1
          INDX(NINDX)=I
          IDEL7NOK = 1
        ENDIF
      ENDDO
      IF(NINDX>0.AND.IMCONV==1)THEN
        DO J=1,NINDX
#include "lockon.inc"
        WRITE(IOUT, 1000) NGL(INDX(J))
        WRITE(ISTDO,1100) NGL(INDX(J)),TT
#include "lockoff.inc"
        ENDDO
      ENDIF
!
 1000 FORMAT(1X,'RUPTURE OF SOLID ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'RUPTURE OF SOLID ELEMENT NUMBER ',I10,
     .          ' AT TIME :',G11.4)
!
!!-------------------------
!!     EQUIVALENT STRESS FOR OUTPUT - THERMAL HILL ORTHOTROPIC 3D CRITERION -
!!-------------------------
!!      IF (ISEQ > 0) THEN
!!        DO I=1,NEL
!!          CRI(I) = 
!!     .       FF*(SIGNYY(I) - SIGNZZ(I))**2 + GG*(SIGNZZ(I) - SIGNXX(I))**2
!!     .    +  HH*(SIGNXX(I) - SIGNYY(I))**2 + TWO*LL*SIGNYZ(I)**2 
!!     .    +  TWO*MM*SIGNZX(I)**2 + TWO*NN*SIGNXY(I)**2
!!          SEQ_OUTPUT(I) = SQRT(CRI(I))
!!        ENDDO
!!      ENDIF
!---
      RETURN
      END
C
