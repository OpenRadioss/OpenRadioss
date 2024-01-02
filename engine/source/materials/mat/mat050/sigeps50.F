Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  SIGEPS50                      source/materials/mat/mat050/sigeps50.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|-- calls ---------------
Chd|        INTEREPSP                     source/materials/mat/mat050/sigeps50.F
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS50 (
     1     NEL    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,NPF    ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0    ,RHO    ,
     3     VOLUME ,EINT   ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVZZ  ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,UVAR    ,OFF     ,AMU     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C NFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW
C IFUNC   | NFUNC   | I | R | FUNCTION INDEX 
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
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL, NUPARAM, NUVAR
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
     .   AMU(NEL)
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
      my_real UVAR(NEL,NUVAR), OFF(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real FINTER ,TF(*)
      EXTERNAL FINTER
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
      INTEGER I,J,IF1,IF2,NMOINS(MVSIZ),NPLUS(MVSIZ)
      my_real E11,E22,E33,G12,G23,G31,
     .        DEP1(MVSIZ),DEP2(MVSIZ),DEP3(MVSIZ),DEP4(MVSIZ),
     .        DEP5(MVSIZ),DEP6(MVSIZ),
     .        DEP1F(MVSIZ),DEP2F(MVSIZ),DEP3F(MVSIZ),DEP4F(MVSIZ),
     .        DEP5F(MVSIZ),DEP6F(MVSIZ),
     .        DYDX,
     .        EP1(MVSIZ),EP2(MVSIZ),EP3(MVSIZ),
     .        EP4(MVSIZ),EP5(MVSIZ),EP6(MVSIZ),
     .        EMX11,EMX22,EMX33,EMX12,EMX23,EMX31,COEF(MVSIZ),
     .        Y11,Y22,Y33,Y12,Y23,Y31,
     .        Y11M,Y22M,Y33M,Y12M,Y23M,Y31M,
     .        Y11P,Y22P,Y33P,Y12P,Y23P,Y31P
      my_real 
     . DEP11(5),DEP22(5),DEP33(5),DEP12(5),DEP23(5),DEP31(5),
     . FAC11(5),FAC22(5),FAC33(5),FAC12(5),FAC23(5),FAC31(5),
     . MUOLD, DMUDT, ASRATE,  DMUDTF
      INTEGER I11(5),I22(5),I33(5),I12(5),I23(5),I31(5)
      INTEGER N11,N22,N33,N12,N23,N31
C=======================================================================
      E11 = UPARAM(1)
      E22 = UPARAM(2)
      E33 = UPARAM(3)
      G12 = UPARAM(4)
      G23 = UPARAM(5)
      G31 = UPARAM(6)
      IF1=NINT(UPARAM(7))
      IF2=NINT(UPARAM(8))
C
      DO I=1,5
        DEP11(I)=UPARAM(14+I)
        I11(I)=IFUNC(I)
        DEP22(I)=UPARAM(19+I)
        I22(I)=IFUNC(I+5)
        DEP33(I)=UPARAM(24+I)
        I33(I)=IFUNC(I+10)
        DEP12(I)=UPARAM(29+I)
        I12(I)=IFUNC(I+15)
        DEP23(I)=UPARAM(34+I)
        I23(I)=IFUNC(I+20)
        DEP31(I)=UPARAM(39+I)
        I31(I)=IFUNC(I+25)
        FAC11(I)=UPARAM(51+I)
        FAC22(I)=UPARAM(56+I)
        FAC33(I)=UPARAM(61+I)
        FAC12(I)=UPARAM(66+I)
        FAC23(I)=UPARAM(71+I)
        FAC31(I)=UPARAM(76+I)
      ENDDO
      N11 = NINT(UPARAM(45))
      N22 = NINT(UPARAM(46))
      N33 = NINT(UPARAM(47))
      N12 = NINT(UPARAM(48))
      N23 = NINT(UPARAM(49))
      N31 = NINT(UPARAM(50))
      ASRATE=MIN(ONE,UPARAM(51)*TIMESTEP)
C
      EMX11 = UPARAM(9)
      EMX22 = UPARAM(10)
      EMX33 = UPARAM(11)
      EMX12 = UPARAM(12)
      EMX23 = UPARAM(13)
      EMX31 = UPARAM(14)
      DO I=1,NEL
        SIGNXX(I) = SIGOXX(I) + E11 * DEPSXX(I)
        SIGNYY(I) = SIGOYY(I) + E22 * DEPSYY(I)
        SIGNZZ(I) = SIGOZZ(I) + E33 * DEPSZZ(I)
        SIGNXY(I) = SIGOXY(I) + G12 * DEPSXY(I)
        SIGNYZ(I) = SIGOYZ(I) + G23 * DEPSYZ(I)
        SIGNZX(I) = SIGOZX(I) + G31 * DEPSZX(I)
        SOUNDSP(I) = SQRT(MAX(E11,E22,E33,G12,G23,G31)/RHO(I))
        VISCMAX(I) = ZERO
      ENDDO
C
      DO I=1,NEL
          IF(EPSXX(I)>EMX11.OR.
     .       EPSYY(I)>EMX22.OR.
     .       EPSZZ(I)>EMX33.OR.
     .       ABS(EPSXY(I)/TWO)>EMX12.OR.
     .       ABS(EPSYZ(I)/TWO)>EMX23.OR.
     .       ABS(EPSZX(I)/TWO)>EMX31) OFF(I) = ZERO
      ENDDO
c
      DO I=1,NEL
       MUOLD=UVAR(I,1)
       DMUDTF=UVAR(I,2)
c       AMU = RHO(I)/RHO0(I) - ONE
       DMUDT=(AMU(I)-MUOLD)/MAX(TIMESTEP,EM20)
       DMUDTF=ASRATE*DMUDT+(1.-ASRATE)*DMUDTF
       UVAR(I,1)=AMU(I)
       UVAR(I,2)=DMUDTF
       DMUDT=ABS(DMUDTF)
       EP1(I) = AMU(I)
       EP2(I) = AMU(I)
       EP3(I) = AMU(I)
       EP4(I) = AMU(I)
       EP5(I) = AMU(I)
       EP6(I) = AMU(I)
      ENDDO
c
      IF(IF1==1)THEN
       DO I=1,NEL
           EP1(I) = EPSXX(I)
           EP2(I) = EPSYY(I)
           EP3(I) = EPSZZ(I)
       ENDDO
      ELSEIF(IF1==-1)THEN
       DO I=1,NEL
           EP1(I) = -EPSXX(I)
           EP2(I) = -EPSYY(I)
           EP3(I) = -EPSZZ(I)
       ENDDO
      ENDIF
      IF(IF2==1)THEN
       DO I=1,NEL
           EP4(I) = EPSXY(I)
           EP5(I) = EPSYZ(I)
           EP6(I) = EPSZX(I)
        ENDDO
      ELSEIF(IF2==-1)THEN
       DO I=1,NEL
           EP4(I) = -EPSXY(I)
           EP5(I) = -EPSYZ(I)
           EP6(I) = -EPSZX(I)
       ENDDO
      ENDIF
      DO I=1,NEL
           UVAR(I,5) = ASRATE*EPSPXX(I)+(ONE -ASRATE)*UVAR(I,5)
           DEP1(I) = ABS(UVAR(I,5))
           UVAR(I,6) = ASRATE*EPSPYY(I)+(ONE -ASRATE)*UVAR(I,6)
           DEP2(I) = ABS(UVAR(I,6))
           UVAR(I,7) = ASRATE*EPSPZZ(I)+(ONE -ASRATE)*UVAR(I,7)
           DEP3(I) = ABS(UVAR(I,7))
           UVAR(I,8) = ASRATE*EPSPXY(I)+(ONE -ASRATE)*UVAR(I,8)
           DEP4(I) = ABS( UVAR(I,8))
           UVAR(I,9) = ASRATE*EPSPYZ(I)+(ONE -ASRATE)*UVAR(I,9)
           DEP5(I) = ABS(UVAR(I,9))
           UVAR(I,10) = ASRATE*EPSPZX(I)+(ONE -ASRATE)*UVAR(I,10)
           DEP6(I) = ABS(UVAR(I,10))
      ENDDO
C
      DO I=1,NEL
       UVAR(I,3) =HALF*(EPSXX(I)**2+EPSYY(I)**2+EPSZZ(I)**2)
     .          +EPSXY(I)**2+EPSYZ(I)**2+EPSZX(I)**2
       UVAR(I,3) =SQRT(THREE*UVAR(I,3))
       UVAR(I,4) = HALF*(UVAR(I,5)**2+UVAR(I,6)**2+UVAR(I,7)**2) +
     .                 UVAR(I,8)**2+UVAR(I,9)**2+UVAR(I,10)**2
       UVAR(I,4) = SQRT(THREE*UVAR(I,4))/THREE_HALF
      ENDDO
C
       IF (I11(1)/=0)THEN
        CALL INTEREPSP(NEL,DEP1,DEP11,N11,NMOINS,NPLUS,COEF)
        DO I=1,NEL
          Y11M = FINTER(I11(NMOINS(I)),EP1(I),NPF,TF,DYDX)
          Y11P = FINTER(I11(NPLUS(I)) ,EP1(I),NPF,TF,DYDX)
          Y11M = Y11M * FAC11(NMOINS(I))
          Y11P = Y11P * FAC11(NPLUS(I))
          Y11  = Y11M + (Y11P - Y11M)*COEF(I)
          SIGNXX(I) = SIGN(MIN(ABS(SIGNXX(I)),Y11),SIGNXX(I))
        ENDDO
       ENDIF
       IF (I22(1)/=0)THEN
        CALL INTEREPSP(NEL,DEP2,DEP22,N22,NMOINS,NPLUS,COEF)
        DO I=1,NEL
          Y22M = FINTER(I22(NMOINS(I)),EP2(I),NPF,TF,DYDX)
          Y22P = FINTER(I22(NPLUS(I)),EP2(I),NPF,TF,DYDX)
          Y22M = Y22M * FAC22(NMOINS(I))
          Y22P = Y22P * FAC22(NPLUS(I))
          Y22 = Y22M+(Y22P-Y22M)*COEF(I)
          SIGNYY(I) = SIGN(MIN(ABS(SIGNYY(I)),Y22),SIGNYY(I))
        ENDDO
       ENDIF
       IF (I33(1)/=0)THEN
        CALL INTEREPSP(NEL,DEP3,DEP33,N33,NMOINS,NPLUS,COEF)
        DO I=1,NEL
          Y33M = FINTER(I33(NMOINS(I)),EP3(I),NPF,TF,DYDX)
          Y33P = FINTER(I33(NPLUS(I)),EP3(I),NPF,TF,DYDX)
          Y33M = Y33M * FAC33(NMOINS(I))
          Y33P = Y33P * FAC33(NPLUS(I))
          Y33 = Y33M+(Y33P-Y33M)*COEF(I)
          SIGNZZ(I) = SIGN(MIN(ABS(SIGNZZ(I)),Y33),SIGNZZ(I))
        ENDDO
       ENDIF
       IF (I12(1)/=0)THEN
        CALL INTEREPSP(NEL,DEP4,DEP12,N12,NMOINS,NPLUS,COEF)
        DO I=1,NEL
          Y12M = FINTER(I12(NMOINS(I)),EP4(I),NPF,TF,DYDX)
          Y12P = FINTER(I12(NPLUS(I)),EP4(I),NPF,TF,DYDX)
          Y12M = Y12M * FAC12(NMOINS(I))
          Y12P = Y12P * FAC12(NPLUS(I))
          Y12 = Y12M+(Y12P-Y12M)*COEF(I)
          SIGNXY(I) = SIGN(MIN(ABS(SIGNXY(I)),Y12),SIGNXY(I))
        ENDDO
       ENDIF
       IF (I23(1)/=0)THEN
        CALL INTEREPSP(NEL,DEP5,DEP23,N23,NMOINS,NPLUS,COEF)
        DO I=1,NEL
          Y23M = FINTER(I23(NMOINS(I)),EP5(I),NPF,TF,DYDX)
          Y23P = FINTER(I23(NPLUS(I)),EP5(I),NPF,TF,DYDX)
          Y23M = Y23M * FAC23(NMOINS(I))
          Y23P = Y23P * FAC23(NPLUS(I))
          Y23 = Y23M+(Y23P-Y23M)*COEF(I)
          SIGNYZ(I) = SIGN(MIN(ABS(SIGNYZ(I)),Y23),SIGNYZ(I))
        ENDDO
       ENDIF
       IF (I31(1)/=0)THEN
        CALL INTEREPSP(NEL,DEP6,DEP31,N31,NMOINS,NPLUS,COEF)
        DO I=1,NEL
          Y31M = FINTER(I31(NMOINS(I)),EP6(I),NPF,TF,DYDX)
          Y31P = FINTER(I31(NPLUS(I)),EP6(I),NPF,TF,DYDX)
          Y31M = Y31M * FAC31(NMOINS(I))
          Y31P = Y31P * FAC31(NPLUS(I))
          Y31 = Y31M+(Y31P-Y31M)*COEF(I)
          SIGNZX(I) = SIGN(MIN(ABS(SIGNZX(I)),Y31),SIGNZX(I))
        ENDDO
       ENDIF
C-----------
      RETURN
      END
      
Chd|====================================================================
Chd|  INTEREPSP                     source/materials/mat/mat050/sigeps50.F
Chd|-- called by -----------
Chd|        SIGEPS50                      source/materials/mat/mat050/sigeps50.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTEREPSP(NEL,EP,EPSP,NI,NMOINS,NPLUS,COEF)
#include      "implicit_f.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NI,NEL
      my_real EPSP(5),EP(NEL)
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NMOINS(NEL),NPLUS(NEL)
      my_real COEF(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real VAL
C=======================================================================
      DO J=1,NEL
        IF (EP(J)<=EPSP(1)) THEN
          NPLUS(J)=2
          NMOINS(J)=1
          COEF(J)=ZERO
        ELSE IF(EP(J)>=EPSP(NI)) THEN
          NPLUS(J)=NI
          NMOINS(J)=NI-1
          COEF(J)=(EP(J)-EPSP(NI-1))/(MAX(EM20,EPSP(NI)-EPSP(NI-1)))
        ELSE
          DO I=2,NI
            VAL= (EP(J)-EPSP(I-1))/(MAX(EM20,EPSP(I)-EPSP(I-1)))
            IF ((VAL>=0).AND.(VAL<=1)) THEN
              COEF(J)=VAL
              NPLUS(J)=I
              NMOINS(J)=I-1
            ENDIF
          ENDDO
        ENDIF
      ENDDO
c-----------
      RETURN
      END
