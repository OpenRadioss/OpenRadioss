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
Chd|  SIGEPS28                      source/materials/mat/mat028/sigeps28.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|-- calls ---------------
Chd|        VINTER2                       source/tools/curve/vinter.F   
Chd|        VINTER2DP                     source/tools/curve/vinter.F   
Chd|====================================================================
      SUBROUTINE SIGEPS28 (
     1     NEL    ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC   ,NPF    ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0    ,RHO    ,
     3     VOLUME ,EINT   ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVZZ  ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,UVAR    ,OFF     ,NGL     ,IPT    ,
     B     IPM    ,MAT    ,AMU    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
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
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "scr05_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL, NUPARAM, NUVAR,IPT,
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
      my_real
     . UVAR(NEL,NUVAR), OFF(NEL)
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real
     .        FINTER ,TF(*)
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
      INTEGER I,J,IADBUF,IF1,IF2,AUX,IC,II,K,
     .        IAD1(MVSIZ),IPOS1(MVSIZ),ILEN1(MVSIZ),
     .        INDEX(MVSIZ),
     .        NINDX,INDX(MVSIZ)
      my_real
     .        E11,E22,E33,G12,G23,G31,
     .        Y11,Y22,Y33,Y12,Y23,Y31,EP1,EP2,EP3,EP4,EP5,EP6,
     .        YC(MVSIZ),FAC1(MVSIZ),FAC2(MVSIZ),
     .        FAC3(MVSIZ),FAC4(MVSIZ),FAC5(MVSIZ),FAC6(MVSIZ),
     .        DYDX,DYDXV(MVSIZ),EP(MVSIZ,6),EPC(MVSIZ),
     .        EMX11,EMX22,EMX33,EMX12,EMX23,EMX31,AMUV
C=======================================================================
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      IF(TIME==ZERO)THEN
       IF (NUVAR>0) THEN
        DO 100 J=1,NUVAR
        DO 100 I=1,NEL
           UVAR(I,J)=ZERO
 100    CONTINUE
       ENDIF
      ENDIF
C-----------------------------------------------
      DO I=1,NEL
C
        IADBUF = IPM(7,MAT(I))
        E11 = UPARAM(IADBUF)
        E22 = UPARAM(IADBUF+1)
        E33 = UPARAM(IADBUF+2)
        G12 = UPARAM(IADBUF+3)
        G23 = UPARAM(IADBUF+4)
        G31 = UPARAM(IADBUF+5)
C
        SIGNXX(I) = SIGOXX(I) + E11 * DEPSXX(I)
        SIGNYY(I) = SIGOYY(I) + E22 * DEPSYY(I)
        SIGNZZ(I) = SIGOZZ(I) + E33 * DEPSZZ(I)
        SIGNXY(I) = SIGOXY(I) + G12 * DEPSXY(I)
        SIGNYZ(I) = SIGOYZ(I) + G23 * DEPSYZ(I)
        SIGNZX(I) = SIGOZX(I) + G31 * DEPSZX(I)
C
        SOUNDSP(I) = SQRT(MAX(E11,E22,E33,G12,G23,G31)/RHO0(I))
        VISCMAX(I) = ZERO
C
      ENDDO

      NINDX=0
      DO I=1,NEL
        IADBUF = IPM(7,MAT(I)) - 1  
        EMX11 = UPARAM(IADBUF+9)    
        EMX22 = UPARAM(IADBUF+10)   
        EMX33 = UPARAM(IADBUF+11)   
        EMX12 = UPARAM(IADBUF+12)   
        EMX23 = UPARAM(IADBUF+13)   
        EMX31 = UPARAM(IADBUF+14)
        FAC1(I) = UPARAM(IADBUF+15) 
        FAC2(I) = UPARAM(IADBUF+16) 
        FAC3(I) = UPARAM(IADBUF+17) 
        FAC4(I) = UPARAM(IADBUF+18) 
        FAC5(I) = UPARAM(IADBUF+19) 
        FAC6(I) = UPARAM(IADBUF+20) 
        IF((EPSXX(I)>EMX11.OR.                                  
     .      EPSYY(I)>EMX22.OR.                                  
     .      EPSZZ(I)>EMX33.OR.                                  
     .      ABS(EPSXY(I)/TWO)>EMX12.OR.                             
     .      ABS(EPSYZ(I)/TWO)>EMX23.OR.                             
     .      ABS(EPSZX(I)/TWO)>EMX31).AND.OFF(I)/=ZERO) THEN       
          OFF(I) = ZERO                                            
          IDEL7NOK = 1                                             
          NINDX=NINDX+1  
          INDX(NINDX)=I  
        ENDIF                                                      
      ENDDO
      IF(NINDX>0)THEN
        DO J=1,NINDX
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(INDX(J))
          WRITE(ISTDO,1100) NGL(INDX(J)),TT
#include "lockoff.inc"
        ENDDO
      ENDIF
C
      DO I=1,NEL
C
c        AMU = RHO(I)/RHO0(I) - ONE ! Change using AMU(I) to take into account THERM_STRESS
        EP(I,1) = AMU(I)
        EP(I,2) = AMU(I)
        EP(I,3) = AMU(I)
        EP(I,4) = AMU(I)
        EP(I,5) = AMU(I)
        EP(I,6) = AMU(I)
        IADBUF = IPM(7,MAT(I))     
        IF1=NINT(UPARAM(IADBUF+6)) 
        IF2=NINT(UPARAM(IADBUF+7)) 
        IF(IF1==1)THEN           
          EP(I,1) = EPSXX(I)       
          EP(I,2) = EPSYY(I)       
          EP(I,3) = EPSZZ(I)       
        ELSEIF(IF1==-1)THEN      
          EP(I,1) = -EPSXX(I)      
          EP(I,2) = -EPSYY(I)      
          EP(I,3) = -EPSZZ(I)      
        ENDIF                      
        IF(IF2==1)THEN           
          EP(I,4) = EPSXY(I)       
          EP(I,5) = EPSYZ(I)       
          EP(I,6) = EPSZX(I)       
        ELSEIF(IF2==-1)THEN      
          EP(I,4) = -EPSXY(I)      
          EP(I,5) = -EPSYZ(I)      
          EP(I,6) = -EPSZX(I)      
        ENDIF                      
       ENDDO
C---------------------------
      DO J = 1, 6
        IC = 0
        DO I = 1, NEL
          NFUNC  = IPM(10,MAT(I))
          IF (NFUNC>=J) THEN
           AUX = IPM(10+J,MAT(I))
           IF (AUX/=0) THEN
            IC = IC + 1
            INDEX(IC) = I
            IPOS1(IC) = NINT(UVAR(I,J))
            IAD1(IC)  = NPF(AUX) / 2 + 1
            ILEN1(IC) = NPF(AUX+1) / 2 - IAD1(IC) - IPOS1(IC)
            EPC(IC) = EP(I,J)
           ENDIF
          ENDIF
        ENDDO

        IF (IRESP==1) THEN
          CALL VINTER2DP(TF,IAD1,IPOS1,ILEN1,IC,EPC,DYDXV,YC)
        ELSE
          CALL VINTER2(TF,IAD1,IPOS1,ILEN1,IC,EPC,DYDXV,YC)
        ENDIF
C
        IF (J==1) THEN
#include "vectorize.inc"
          DO II = 1, IC
            I = INDEX(II)
            UVAR(I,J)=IPOS1(II)
            SIGNXX(I)=SIGN(MIN(ABS(SIGNXX(I)),YC(II)*FAC1(I)),SIGNXX(I))
          ENDDO
        ELSEIF (J==2) THEN
#include "vectorize.inc"
          DO II = 1, IC
            I = INDEX(II)
            UVAR(I,J)=IPOS1(II) 
            SIGNYY(I)=SIGN(MIN(ABS(SIGNYY(I)),YC(II)*FAC2(I)),SIGNYY(I))
          ENDDO
        ELSEIF (J==3) THEN
#include "vectorize.inc"
          DO II = 1, IC
            I = INDEX(II)
            UVAR(I,J)=IPOS1(II) 
            SIGNZZ(I)=SIGN(MIN(ABS(SIGNZZ(I)),YC(II)*FAC3(I)),SIGNZZ(I))
          ENDDO
        ELSEIF (J==4) THEN
#include "vectorize.inc"
          DO II = 1, IC
            I = INDEX(II)
            UVAR(I,J)=IPOS1(II) 
            SIGNXY(I)=SIGN(MIN(ABS(SIGNXY(I)),YC(II)*FAC4(I)),SIGNXY(I))
          ENDDO
        ELSEIF (J==5) THEN
#include "vectorize.inc"
          DO II = 1, IC
            I = INDEX(II)
            UVAR(I,J)=IPOS1(II) 
            SIGNYZ(I)=SIGN(MIN(ABS(SIGNYZ(I)),YC(II)*FAC5(I)),SIGNYZ(I))
          ENDDO
        ELSEIF (J==6) THEN
#include "vectorize.inc"
          DO II = 1, IC
            I = INDEX(II)
            UVAR(I,J)=IPOS1(II) 
            SIGNZX(I)=SIGN(MIN(ABS(SIGNZX(I)),YC(II)*FAC6(I)),SIGNZX(I))
          ENDDO
        ENDIF
C
      ENDDO

 1000 FORMAT(1X,'RUPTURE OF SOLID ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'RUPTURE OF SOLID ELEMENT NUMBER ',I10,
     .          ' AT TIME :',G11.4)
      RETURN
      END
