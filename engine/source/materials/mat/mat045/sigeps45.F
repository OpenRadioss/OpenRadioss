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
Chd|  SIGEPS45                      source/materials/mat/mat045/sigeps45.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SIGEPS45 (
     1     NEL0   ,NUPARAM,NUVAR   ,NFUNC   ,IFUNC  ,
     2     NPF    ,
     2     TF     ,TIME   ,TIMESTEP,UPARAM  ,RHO0   ,
     3     RHO    ,VOLUME ,EINT    ,
     4     EPSPXX ,EPSPYY ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX ,
     5     DEPSXX ,DEPSYY ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX ,
     6     EPSXX  ,EPSYY  ,EPSZZ   ,EPSXY   ,EPSYZ   ,EPSZX  ,
     7     SIGOXX ,SIGOYY ,SIGOZZ  ,SIGOXY  ,SIGOYZ  ,SIGOZX ,
     8     SIGNXX ,SIGNYY ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX ,
     9     SIGVXX ,SIGVYY ,SIGVZZ  ,SIGVXY  ,SIGVYZ  ,SIGVZX ,
     A     SOUNDSP,VISCMAX,UVAR    ,OFF     ,SIGY    ,DEFP   ,
     B     AMU    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "task_c.inc"
#include      "com08_c.inc"
#include      "scr20_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL0    |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL0 
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
C RHO0    | NEL0    | F | R | INITIAL DENSITY
C RHO     | NEL     | F | R | DENSITY
C VOLUME  | NEL     | F | R | VOLUME
C EINT    | 2*NEL0  | F | R | INTERNAL ENERGY(MEMBRANE,BENDING)
C EPSPXX  | NEL0    | F | R | STRAIN RATE XX
C EPSPYY  | NEL0    | F | R | STRAIN RATE YY
C ...     |         |   |   |
C DEPSXX  | NEL0    | F | R | STRAIN INCREMENT XX
C DEPSYY  | NEL0    | F | R | STRAIN INCREMENT YY
C ...     |         |   |   |
C EPSXX   | NEL0    | F | R | STRAIN XX
C EPSYY   | NEL0    | F | R | STRAIN YY
C ...     |         |   |   |
C SIGOXX  | NEL0    | F | R | OLD ELASTO PLASTIC STRESS XX 
C SIGOYY  | NEL0    | F | R | OLD ELASTO PLASTIC STRESS YY
C ...     |         |   |   |    
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL0    | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL0    | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C SIGVXX  | NEL0    | F | W | VISCOUS STRESS XX
C SIGVYY  | NEL0    | F | W | VISCOUS STRESS YY
C ...     |         |   |   |
C SOUNDSP | NEL0    | F | W | SOUND SPEED (NEEDED FOR TIME STEP)
C VISCMAX | NEL0    | F | W | MAXIMUM DAMPING MODULUS(NEEDED FOR TIME STEP)
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL0*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL0    | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
C
      INTEGER NEL0, NUPARAM, NUVAR
      my_real 
     .   TIME,TIMESTEP,UPARAM(NUPARAM),
     .   RHO(NEL0),RHO0(NEL0),VOLUME(NEL0),EINT(NEL0),
     .   EPSPXX(NEL0),EPSPYY(NEL0),EPSPZZ(NEL0),
     .   EPSPXY(NEL0),EPSPYZ(NEL0),EPSPZX(NEL0),
     .   DEPSXX(NEL0),DEPSYY(NEL0),DEPSZZ(NEL0),
     .   DEPSXY(NEL0),DEPSYZ(NEL0),DEPSZX(NEL0),
     .   EPSXX(NEL0) ,EPSYY(NEL0) ,EPSZZ(NEL0) ,
     .   EPSXY(NEL0) ,EPSYZ(NEL0) ,EPSZX(NEL0) ,
     .   SIGOXX(NEL0),SIGOYY(NEL0),SIGOZZ(NEL0),
     .   SIGOXY(NEL0),SIGOYZ(NEL0),SIGOZX(NEL0),
     .   AMU(NEL0)
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      my_real
     .    SIGNXX(NEL0),SIGNYY(NEL0),SIGNZZ(NEL0),
     .    SIGNXY(NEL0),SIGNYZ(NEL0),SIGNZX(NEL0),
     .    SIGVXX(NEL0),SIGVYY(NEL0),SIGVZZ(NEL0),
     .    SIGVXY(NEL0),SIGVYZ(NEL0),SIGVZX(NEL0),
     .    SOUNDSP(NEL0),VISCMAX(NEL0),SIGY(NEL0),DEFP(NEL0)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real UVAR(NEL0,NUVAR), OFF(NEL0)
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
      INTEGER I
      my_real 
     .        G,CA,CB,CN,EPSM,SIGM,CC,CD,CM,EPS0,
     .        CE,CK,C1,C14G3,G3,
     .        CH1,CH2,CH3,QH1,QH2,SVM,
     .        DT,DTG2,PRESS,DF,
     .  EPRNXX,EPRNYY,EPRNZZ,EPRNXY,EPRNYZ,EPRNZX,
     .  EVR,EDRNXX,EDRNYY,EDRNZZ,EDRNXY,EDRNYZ,EDRNZX,
     .        DPLA_I,R,UMR,
c jbm035
     .        CUTFRE,BETA
C
c jbm035
C=======================================================================
C
C     ZHAO CONSTITUTIVE LAW 
C
C=======================================================================
C-----------------------------------------------
C     PARAMETERS READING
C-----------------------------------------------
        G = UPARAM(3)
        CA = UPARAM(4)
        CB = UPARAM(5)
        CN = UPARAM(6)
        EPSM = UPARAM(7)
        SIGM = UPARAM(8) 
        CC = UPARAM(9) 
        CD = UPARAM(10)
        CM = UPARAM(11)
        EPS0 = UPARAM(12)
        CE = UPARAM(13)
        CK = UPARAM(14)
        C1 = UPARAM(15) 
        C14G3 = UPARAM(16)
c jbm035
        CUTFRE = UPARAM(19) 
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      IF(TIME==ZERO)THEN
        DO I=1,NEL0
           UVAR(I,1)=ZERO
           UVAR(I,2)=ZERO
           UVAR(I,3)=ZERO
           UVAR(I,4)=ZERO   
c jbm035
           UVAR(I,5)=ZERO
        ENDDO
      ENDIF
C-----------------------------------------------
        DT = TIMESTEP 
        DTG2 = TWO*DT*G
c jbm035
        BETA = TIMESTEP*TWO*PI*CUTFRE
        BETA = MIN(ONE,BETA)
c jbm035 (fin)
C
        G3 = THREE * G
C-----------------------------------------------
C     ELASTIC SOLUTION (DTG2 OK)
C-----------------------------------------------
       DO I=1,NEL0
C
         PRESS = - (SIGOXX(I) + SIGOYY(I) + SIGOZZ(I)) * THIRD
C
         EPRNXX = EPSPXX(I)
         EPRNYY = EPSPYY(I)
         EPRNZZ = EPSPZZ(I)
         EPRNXY = EPSPXY(I)
         EPRNYZ = EPSPYZ(I)
         EPRNZX = EPSPZX(I)
C
         EVR = - (EPRNXX + EPRNYY + EPRNZZ) * THIRD
         EDRNXX = EPRNXX + EVR
         EDRNYY = EPRNYY + EVR
         EDRNZZ = EPRNZZ + EVR
         EDRNXY = EPRNXY*HALF
         EDRNYZ = EPRNYZ*HALF
         EDRNZX = EPRNZX*HALF
C
         SIGNXX(I)=SIGOXX(I) + PRESS + DTG2*EDRNXX
         SIGNYY(I)=SIGOYY(I) + PRESS + DTG2*EDRNYY
         SIGNZZ(I)=SIGOZZ(I) + PRESS + DTG2*EDRNZZ
         SIGNXY(I)=SIGOXY(I)         + DTG2*EDRNXY
         SIGNYZ(I)=SIGOYZ(I)         + DTG2*EDRNYZ
         SIGNZX(I)=SIGOZX(I)         + DTG2*EDRNZX
C
         SOUNDSP(I) = SQRT(C14G3/RHO0(I))
         VISCMAX(I) = ZERO
C----------------------------------------
C     STRAIN RATE (LAW 2 COMMENTS / LAW 36 BRICKS USED)
C----------------------------------------
C
C         UVAR(I,2) = OFF(I)*MAX( ABS(EPRNXX), ABS(EPRNYY), 
C     .                           ABS(EPRNZZ), ABS(EDRNXY),
C     .                           ABS(EDRNYZ), ABS(EDRNZX))
        UVAR(I,2) = HALF*(EDRNXX*EDRNXX+EDRNYY*EDRNYY+EDRNZZ*EDRNZZ)
     .             +EDRNXY*EDRNXY+EDRNYZ*EDRNYZ+EDRNZX*EDRNZX
         UVAR(I,2) = OFF(I) * SQRT(THREE*UVAR(I,2)) / THREE_HALF
c jbm035
         UVAR(I,2) = BETA*UVAR(I,2) + (ONE - BETA)*UVAR(I,5)
         UVAR(I,5) = UVAR(I,2)
c jbm035 (fin)
       ENDDO
C-------------------
C     CRITERIA
C-------------------
       DO I=1,NEL0
        IF(UVAR(I,1)<=ZERO) THEN
         CH1=CA
        ELSEIF(UVAR(I,1)>EPSM) THEN
         CH1=CA+CB*EPSM**CN
        ELSE
         CH1=CA+CB*UVAR(I,1)**CN
        ENDIF
        IF(UVAR(I,2)<=EPS0) THEN
         CH2=ZERO
        ELSEIF(UVAR(I,1)<=ZERO) THEN
         CH2=CC*LOG(UVAR(I,2)/EPS0)
        ELSE
         CH2=(CC-CD*UVAR(I,1)**CM)*LOG(UVAR(I,2)/EPS0)
        ENDIF
        IF(UVAR(I,2)<=ZERO) THEN
         CH3=ZERO
        ELSE
         CH3=CE*UVAR(I,2)**CK
        ENDIF
c jbm033        UVAR(I,3)=MIN(SIGM,CH1+CH2+CH3)
        UVAR(I,3)=MIN(SIGM+CH3,CH1+CH2+CH3)
        SIGY(I)=UVAR(I,3)
        IF(UVAR(I,1)>EPSM) UVAR(I,3)=ZERO
       ENDDO
C------------------------
C     HARDENING MODULUS
C------------------------
       DO I=1,NEL0
        IF(UVAR(I,1)>ZERO. AND .CN>=ONE) THEN
         QH1= CB*CN*UVAR(I,1)**(CN- ONE)
        ELSEIF(UVAR(I,1)>ZERO. AND .CN<ONE)THEN
         QH1= CB*CN*UVAR(I,1)**(ONE - CN)
        ELSE
         QH1=ZERO
        ENDIF
        IF(UVAR(I,1)<=ZERO. OR .UVAR(I,2)<=EPS0) THEN
         QH2=ZERO
        ELSEIF(CM>=ONE) THEN
         QH2=CD*CM*UVAR(I,1)**(CM- ONE)*LOG(UVAR(I,2)/EPS0)
        ELSE
         QH2=CD*CM*UVAR(I,1)**(ONE - CM)*LOG(UVAR(I,2)/EPS0)
        ENDIF
        UVAR(I,4)=QH1+QH2
       ENDDO
C-----------------------------
C     PROJECTION RADIAL RETURN
C-----------------------------
         DO I=1,NEL0
           SVM = HALF*(SIGNXX(I)*SIGNXX(I)
     .              +SIGNYY(I)*SIGNYY(I)
     .              +SIGNZZ(I)*SIGNZZ(I)
     .              +SIGNXY(I)*SIGNXY(I)
     .              +SIGNYZ(I)*SIGNYZ(I)
     .              +SIGNZX(I)*SIGNZX(I))
           SVM = SQRT(THREE * SVM)
           R  = MIN(ONE,UVAR(I,3)/MAX(EM20,SVM))
           SIGNXX(I)=SIGNXX(I)*R
           SIGNYY(I)=SIGNYY(I)*R
           SIGNZZ(I)=SIGNZZ(I)*R
           SIGNXY(I)=SIGNXY(I)*R
           SIGNYZ(I)=SIGNYZ(I)*R
           SIGNZX(I)=SIGNZX(I)*R
           UMR = ONE - R
           DPLA_I = OFF(I)*SVM*UMR/(G3+UVAR(I,4))
           UVAR(I,1) = UVAR(I,1) + DPLA_I
         ENDDO
C--------------------------------------------
C   NEW PRESSURE AND STRESS TENSOR EVALUATION
C--------------------------------------------
       DO I=1,NEL0
         DF = RHO0(I)/RHO(I)
c         AMU = ONE /DF - ONE
         PRESS = C1*AMU(I)
         SIGNXX(I) = (SIGNXX(I) - PRESS) * OFF(I)
         SIGNYY(I) = (SIGNYY(I) - PRESS) * OFF(I)
         SIGNZZ(I) = (SIGNZZ(I) - PRESS) * OFF(I)
         SIGNXY(I) =  SIGNXY(I) * OFF(I)
         SIGNYZ(I) =  SIGNYZ(I) * OFF(I)
         SIGNZX(I) =  SIGNZX(I) * OFF(I)
       ENDDO
C-----------------------------
C  A VOIR .....
      DO I=1,NEL0
        IF(OFF(I)<ONE) IDEL7NOK = 1
      ENDDO
      DO I=1,NEL0
        DEFP(I)=UVAR(I,1)
      ENDDO
C-------------------------
      RETURN
      END

