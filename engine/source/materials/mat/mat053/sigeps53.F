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
Chd|  SIGEPS53                      source/materials/mat/mat053/sigeps53.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        VINTER                        source/tools/curve/vinter.F   
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
       SUBROUTINE SIGEPS53(
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
     B     IPM    ,MAT    ,EPSP    ,IPLA    ,SEQ_OUTPUT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
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
C  EPSXX   | NEL     | F | R | STRAIN XX
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
C#include      "parit_c.inc"
C#include      "scr17_c.inc"
C#include      "scr05_c.inc"
C#include      "com01_c.inc"
       INTEGER NEL, NUPARAM, NUVAR,IPT,IPLA
       INTEGER NGL(NEL),MAT(NEL),IPM(NPROPMI,*)
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
     .   EPSP(NEL),SEQ_OUTPUT(NEL)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
        my_real
     .    SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .    SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),
     .    SIGVXX(NEL),SIGVYY(NEL),SIGVZZ(NEL),
     .    SIGVXY(NEL),SIGVYZ(NEL),SIGVZX(NEL),
     .    SOUNDSP(NEL),VISCMAX(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
        my_real 
     .        UVAR(NEL,NUVAR), OFF(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), MFUNC, KFUNC(MFUNC)
       my_real
     .   FINTER2, TF(*)
        EXTERNAL FINTER2
C
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
       INTEGER I,J,IADBUFV(MVSIZ),J1,J2,JJ(MVSIZ),NFUNC,
     .        NRATE(MVSIZ),IPOS1(MVSIZ),IPOS2(MVSIZ),IAD1(MVSIZ),
     .        ILEN1(MVSIZ),IAD2(MVSIZ),ILEN2(MVSIZ),
     .        IFUNC(MVSIZ,100),NFUNCV(MVSIZ),PFUN(MVSIZ),
     .        IPOSP(MVSIZ),IADP(MVSIZ),ILENP(MVSIZ),NFUNCM,NRATEM,
     .        IPFLAG,IPARAM,NPAR, IFLAG(MVSIZ)
       my_real   
     .        RATE(MVSIZ,2),YFAC(MVSIZ,2),
     .        Y1(MVSIZ),Y2(MVSIZ),H(MVSIZ),DYDX1(MVSIZ),
     .        DYDX2(MVSIZ),PLA(MVSIZ),FAIL(MVSIZ),EPSR1(MVSIZ),
     .        P0(MVSIZ),PFAC(MVSIZ),
     .        DFDP(MVSIZ),DPLA,
     .        EVL(MVSIZ),
     .        E11(MVSIZ), E22(MVSIZ), G12(MVSIZ),G23(MVSIZ),
     .        F1(MVSIZ), F2(MVSIZ), F11(MVSIZ), F22(MVSIZ), F12(MVSIZ), 
     .        F44(MVSIZ), F55(MVSIZ), F23(MVSIZ),F(MVSIZ), S1C(MVSIZ),   
     .        S2C(MVSIZ),S2T(MVSIZ), S3C(MVSIZ),S3T(MVSIZ), S4T(MVSIZ),
     .        S4C(MVSIZ), S45C(MVSIZ),S45T(MVSIZ),SCALE,BB,AA,DD,CC,SS1,
     .        SS2,S1T(MVSIZ), EVOL(MVSIZ)
          
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
C
       DO I=1,NEL
         IADBUFV(I) = IPM(7,MAT(I))-1
         E11(I)   =  UPARAM(IADBUFV(I)+1)
         E22(I)   =  UPARAM(IADBUFV(I)+2)
         G12(I)   =  UPARAM(IADBUFV(I)+3)
         G23(I)   =  UPARAM(IADBUFV(I)+4)
         IFLAG(I) = NINT(UPARAM(IADBUFV(I)+5))
C
         NFUNC  = IPM(10,MAT(I))
         DO J=1,NFUNC
           IFUNC(I,J) = IPM(10+J,MAT(I))
         ENDDO    
       ENDDO
C
       IF(TIME==ZERO)THEN
         DO I=1,NEL
            UVAR(I,1)=ZERO
          DO J=1,NFUNC
           UVAR(I,J+1)=ZERO
          ENDDO
         ENDDO
       ENDIF
C-----------------------------------------------
C
       DO I=1,NEL
          PLA(I) = UVAR(I,1)
        PLA(I) = PLA(I) +  DEPSXX(I) + DEPSYY(I) + DEPSZZ(I)
         SIGNXX(I)=SIGOXX(I)+ E11(I) * DEPSXX(I)
         SIGNYY(I)=SIGOYY(I)+ E22(I) * DEPSYY(I)
         SIGNZZ(I)=SIGOZZ(I)+ E22(I) * DEPSZZ(I)
         SIGNXY(I)=SIGOXY(I)+ G12(I) * DEPSXY(I)
         SIGNYZ(I)=SIGOYZ(I)+ G23(I) * DEPSYZ(I)
        SIGNZX(I)=SIGOZX(I)+ G12(I) * DEPSZX(I)
C
      SOUNDSP(I) = SQRT(MAX(E11(I),E22(I),G12(I),G23(I))/RHO0(I))
         EVOL(I) =ONE - EXP(PLA(I))
        VISCMAX(I) = ZERO
        UVAR(I,1)= PLA(I)
       ENDDO
C-------------------
C     CRITERE
C-------------------
        DO I=1,NEL
            JJ(I) = 1
             EVL(I)=ZERO
        ENDDO
C
        DO J=1, NFUNC
        DO I=1,NEL
C---first direction--
         IPOS1(I) = NINT(UVAR(I,1+J))
         IAD1(I)  = NPF(IFUNC(I,J)) / 2 + 1
         ILEN1(I) = NPF(IFUNC(I,J)+1) / 2 - IAD1(I) - IPOS1(I)
C--- second direction
C        IPOS2(I) = NINT(UVAR(I,3))
C        IAD2(I)  = 0.5*NPF(IFUNC(I,2)) + 1
C        ILEN2(I) = 0.5*NPF(IFUNC(I,3)+1) - IAD2(I) - IPOS2(I)
C--- third direction
C       IPOS3(I) = NINT(UVAR(I,4))
C        IAD3(I)  = 0.5*NPF(IFUNC(I,3)) + 1
C        ILEN3(I) = 0.5*NPF(IFUNC(I,4)+1) - IAD3(I) - IPOS3(I) 
C....fourth direction
C       IPOS4(I) = NINT(UVAR(I,5))
C        IAD4(I)  = 0.5*NPF(IFUNC(I,4)) + 1
C        ILE4(I)  = 0.5*NPF(IFUNC(I,5)+1) - IAD4(I) - IPOS4(I)
C... 45 direction curve
C       IPOS5(I) = NINT(UVAR(I,6))
C        IAD5(I)  = 0.5*NPF(IFUNC(I,5)) + 1
C        ILE5(I)  = 0.5*NPF(IFUNC(I,6)+1) - IAD5(I) - IPOS5(I)
        ENDDO
C  extrapollation dans chaque direction
C---first direction--
        CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,EVOL,DYDX1,Y1)
       CALL VINTER(TF,IAD1,IPOS1,ILEN1,NEL,EVL,DYDX1,Y2)
C
        IF(J==1) THEN
          DO I=1, NEL
          IF(IFLAG(I)/=1)THEN
           IF(EVOL(I)>ZERO)THEN
            S1C(I)= Y1(I) 
            S1T(I)= Y2(I)
            ELSE
            S1C(I)= Y2(I) 
            S1T(I)= Y1(I)
           ENDIF
           ELSE
          S1C(I)= Y1(I) 
            S1T(I)= Y1(I)
         ENDIF
          UVAR(I,1+J)= IPOS1(I)
          ENDDO
C second direction
         ELSEIF(J==2)THEN
C          CALL vINTER(TF,IAD2,IPOS2,ILEN2,NEL,PLA,DYDX1,Y1)
C           CALL VINTER(TF,IAD2,IPOS2,ILEN2,NEL,EVL,DYDX1,Y2)
C
        DO I=1, NEL
          IF(IFLAG(I)/=1)THEN
          IF(EVOL(I)>ZERO)THEN
            S2C(I)= Y1(I) 
           S2T(I)= Y2(I)
            ELSE
            S2C(I)= Y2(I) 
           S2T(I)= Y1(I)
          ENDIF
         ELSE
         S2C(I)= Y1(I) 
          S2T(I)= Y1(I)
          ENDIF
        UVAR(I,1+J)= IPOS1(I)
        ENDDO
CC third direction
       ELSEIF(J==3)THEN
C          CALL VINTER(TF,IAD3,IPOS3,ILEN3,NEL,PLA,DYDX1,Y1)
C           CALL VINTER(TF,IAD3,IPOS3,ILEN3,NEL,EVL,DYDX1,Y2)
C 
       DO I=1, NEL
          IF(IFLAG(I)/=1)THEN
          IF(EVOL(I)>ZERO)THEN
            S3C(I)= Y1(I) 
            S3T(I)= Y2(I)
          ELSE
           S3C(I)= Y2(I) 
            S3T(I)= Y1(I)
           ENDIF
         ELSE
           S3C(I)= Y1(I) 
           S3T(I)= Y1(I)
          ENDIF
         UVAR(I,1+J)= IPOS1(I)
         ENDDO
CCC fourth direction
        ELSEIF(J==4)THEN
C          CALL VINTER(TF,IAD4,IPOS4,ILEN4,NEL,PLA,DYDX1,Y1)
C           CALL VINTER(TF,IAD4,IPOS4,ILEN4,NEL,EVL,DYDX1,Y2)
C
      DO I=1, NEL
        IF(IFLAG(I)/=1)THEN
         IF(EVOL(I)>ZERO)THEN
          S4C(I)= Y1(I) 
           S4T(I)= Y2(I)
             ELSE
            S4C(I)= Y2(I) 
            S4T(I)= Y1(I)
           ENDIF
          ELSE
           S4C(I)= Y1(I) 
           S4T(I)= Y1(I)
         ENDIF
           UVAR(I,1+J)= IPOS1(I)
         ENDDO      
CCCC 45 direction
        ELSEIF(J==5)THEN
C          CALL VINTER(TF,IAD5,IPOS5,ILEN5,NEL,PLA,DYDX1,Y1)
C           CALL VINTER(TF,IAD5,IPOS5,ILEN5,NEL,EVL,DYDX1,Y2)
C
          DO I=1, NEL
            IF(IFLAG(I)/=1)THEN
              IF(EVOL(I)>ZERO)THEN
               S45C(I)= Y1(I) 
               S45T(I)= Y2(I)
              ELSE
               S45C(I)= Y2(I) 
               S45T(I)= Y1(I)
              ENDIF
            ELSE
              S45C(I)= Y1(I) 
              S45T(I)= Y1(I)
            ENDIF   
            UVAR(I,1+J)= IPOS1(I)   
          ENDDO
        ENDIF
      ENDDO
C--YEILD SURFACE COEFFICIENTS
        DO I=1,NEL         
          F1(I) = -ONE/S1C(I) + ONE/S1T(I)
          F2(I) = -ONE/S2C(I) + ONE/S2T(I)
          F11(I)= ONE/(S1C(I)*S1T(I))
          F22(I)= ONE/(S2C(I)*S2T(I))
          F44(I)= ONE/(S3C(I)*S3T(I))
          F55(I)= ONE/(S4C(I)*S4T(I))
          F12(I)=(-HALF)*SQRT(F11(I)*F22(I))
          F23(I)=(-HALF)*F22(I)     
C... CORRECTION
           IF(IFUNC(I,5)>0)THEN
           F12(I)= TWO/(S45C(I)*S45C(I)) - 
     .        (HALF)*(F11(I)+ F22(I) + F44(I)) +
     .         (F1(I) + F2(I))/S45C(I)     
           ENDIF
C... Check the yield condition according to Tsay Wu.
       F(I) = F1(I)*SIGNXX(I) + F2(I)* SIGNYY(I) + F2(I)*SIGNZZ(I)+
     . F11(I)*SIGNXX(I)**2 + F22(I)*SIGNYY(I)**2 + F22(I)*SIGNZZ(I)**2
     .+F44(I)*SIGNXY(I)**2 + F55(I)*SIGNYZ(I)**2 + F44(I)*SIGNZX(I)**2
     .+TWO*F12(I)*SIGNXX(I)*SIGNYY(I) + TWO*F23(I)*SIGNYY(I)*SIGNZZ(I)
     .+TWO*F12(I)*SIGNZZ(I)*SIGNXX(I)
C
!!        SEQ_OUTPUT(I) = F(I)
C             
        IF(F(I)<=ONE) SCALE=ONE
        IF(F(I)>ONE)THEN
          BB= F1(I)*SIGNXX(I) + F2(I)*SIGNYY(I)+ F2(I)*SIGNZZ(I)
          CC = -ONE
          AA =
     . F11(I)*SIGNXX(I)**2 + F22(I)*SIGNYY(I)**2 + F22(I)*SIGNZZ(I)**2
     .+F44(I)*SIGNXY(I)**2 + F55(I)*SIGNYZ(I)**2 + F44(I)*SIGNZX(I)**2
     .+TWO*F12(I)*SIGNXX(I)*SIGNYY(I) + TWO*F23(I)*SIGNYY(I)*SIGNZZ(I)
     .+TWO*F12(I)*SIGNZZ(I)*SIGNXX(I)
C
          DD= BB**2 - FOUR*AA*CC
          IF(DD<ZERO) THEN
            CALL ANCMSG(MSGID=136,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
C...
          SS1 = (-BB + SQRT(DD))/(TWO*AA)
          SS2 = (-BB - SQRT(DD))/(TWO*AA)
C
          IF(SS1<=ZERO) SCALE = SS2 
          IF(SS2<=ZERO) SCALE = SS1 
C
          IF(SS1>0.AND.SS2>0) THEN
            WRITE(*,*)' TWO POSITIVE ROOTS IN STRANDFOAM'
            IF(SS1<SS2) SCALE = SS1
            IF(SS2<SS1) SCALE = SS2
          ENDIF
        ENDIF
CCC STRESS SCALING
C radial in stress space
        SIGNXX(I) = SIGNXX(I) * SCALE   
        SIGNYY(I) = SIGNYY(I) * SCALE   
        SIGNZZ(I) = SIGNZZ(I) * SCALE   
        SIGNXY(I) = SIGNXY(I) * SCALE   
        SIGNYZ(I) = SIGNYZ(I) * SCALE   
        SIGNZX(I) = SIGNZX(I) * SCALE   
 
      ENDDO
C-----
      RETURN
      END
