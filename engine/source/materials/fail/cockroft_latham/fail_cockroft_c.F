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
Chd|  FAIL_COCKROFT_C               source/materials/fail/cockroft_latham/fail_cockroft_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE FAIL_COCKROFT_C(
     1     NEL    ,NUVAR  ,
     2     TIME   ,UPARAM ,NGL    ,IPT    ,ILAY   ,
     3     NPT0   ,IPTT   ,IPG    ,
     4     SIGNXX ,SIGNYY ,SIGNXY ,
     5     EPSXX  ,EPSYY  ,EPSXY  ,EPSYZ  ,EPSZX  ,
     6     DPLA   ,UVAR   ,UEL    ,FOFF   ,
     7     OFF    ,DFMAX  ,TDEL   )
C--------------------------------------------------------------------
C   /FAIL/COCKROFT - Cockroft-Latham failure criteria for shells and solids
C--------------------------------------------------------------------
C-----------------------------------------------
C I m p l i c i t T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "units_c.inc"
#include "param_c.inc"
#include "scr17_c.inc"
#include "scr19_c.inc"
#include "comlock.inc"
#include "mvsiz_p.inc"
C---------+--------+---+---+-------------------------------------------
C VAR     | SIZE   |TYP| RW| DEFINITION
C---------+--------+---+---+-------------------------------------------
C NEL     | 1      | I | R | SIZE OF THE ELEMENT GROUP NEL
C NUPARAM | 1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   | 1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C---------+--------+---+---+-------------------------------------------
C NFUNC   | 1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW
C IFUNC   | NFUNC  | I | R | FUNCTION INDEX
C NPF     | *      | I | R | FUNCTION ARRAY
C NPT0    | 1      | I | R | NUMBER OF LAYERS OR INTEGRATION POINTS
C IPT     | 1      | I | R | LAYER OR INTEGRATION POINT NUMBER
C TF      | *      | F | R | FUNCTION ARRAY
C NGL     | NEL    | I | R | ELEMEMT NUMBER
C---------+--------+---+---+-------------------------------------------
C TIME    | 1      | F | R | CURRENT TIME
C TIMESTEP| 1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM| F | R | USER MATERIAL PARAMETER ARRAY
C EPSPXX  | NEL    | F | R | STRAIN RATE XX
C EPSPYY  | NEL    | F | R | STRAIN RATE YY
C ...     |        |   |   |
C EPSXX   | NEL    | F | R | STRAIN XX
C EPSYY   | NEL    | F | R | STRAIN YY
C---------+--------+---+---+-------------------------------------------
C SIGNXX  | NEL    | F |R/W| NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL    | F |R/W| NEW ELASTO PLASTIC STRESS YY
C ...     |        |   |   |
C---------+--------+---+---+-------------------------------------------
C PLA     | NEL    | F | R | PLASTIC STRAIN
C DPLA    | NEL    | F | R | INCREMENTAL PLASTIC STRAIN
C EPSP    | NEL    | F | R | EQUIVALENT STRAIN RATE
C UVAR    |NEL*NUVAR| F|R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL    | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C FOFF    | NEL     | I |R/W| DELETED INTEGRATION POINT FLAG (=1 ON, =0 OFF)
C---------+--------+--+--+-------------------------------------------
C I N P U T A r g u m e n t s
C-----------------------------------------------
       INTEGER, INTENT(IN) :: ILAY,IPTT,IPG
       INTEGER NEL,  NUVAR, NGL(NEL),
     .   IPT,NPT0,NOFF(NEL)
       my_real TIME,TIMESTEP(NEL),UPARAM(*),DPLA(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,EPSXY(NEL),
     .   EPSYZ(NEL) ,EPSZX(NEL),
     .   DFMAX(NEL) ,TDEL(NEL)

C
C-----------------------------------------------
C I N P U T O U T P U T A r g u m e n t s
C-----------------------------------------------
       INTEGER ,DIMENSION(NEL), INTENT(INOUT) :: FOFF
       my_real UVAR(NEL,NUVAR), OFF(NEL),OFFL(NEL),
     .   SIGNXX(NEL),SIGNYY(NEL),UEL(NEL),
     .   SIGNXY(NEL)

C-----------------------------------------------
C L o c a l V a r i a b l e s
C-----------------------------------------------
       INTEGER I,J,NINDX,LEN
       INTEGER, DIMENSION(NEL) :: INDX
       my_real   E_HYD,E_11,E_22,E_33,E_12,E_23,E_13,
     .           EEQ,D_EEQ,SIG_11,SIG_A,SIG_B
C-----------------------------------------------
C Ex : ELEMENT IS OFF iF COLA > C0
C-----------------------------------------------
       my_real   C0 , EMA

C-----------------------------------------------
C Ex : ELEMENT IS OFF number if IPs due to Cockroft-Latham criteria reached P_thick
C-----------------------------------------------
C!      UVAR(I,1) contains previous equivalent strain value increment
C!      UVAR(I,2) contains the Cockroft-Latham acumulated value
c!      UVAR(I,3) contains the previous first principal stress
       C0                  = UPARAM(1)
       EMA                 = UPARAM(2)
       NINDX               = 0


c!       PTHKF               = P_THICK
C-----------------------------------------------
       DO I =1,NEL
         IF(OFF(I) >= ONE .AND. FOFF(I) == 1 ) THEN
c! equivalent strain calculation (negative = total strain ; positive = plastic strain)
          IF(C0 < ZERO)THEN
            E_HYD = THIRD * (EPSXX(I) + EPSYY(I))
            E_11  = EPSXX(I) - E_HYD
            E_22  = EPSYY(I) - E_HYD
            E_33  =  -E_HYD
            E_12  = HALF*EPSXY(I)
            E_23  = HALF*EPSYZ(I)
            E_13  = HALF*EPSZX(I)

            EEQ   = E_11**2 + E_22**2 + E_33**2
            EEQ   = EEQ + TWO * (E_12**2) + TWO * (E_23**2) + TWO * (E_13**2)
            EEQ   = 0.8164965809 * SQRT(EEQ) ! sqrt (2/3)*sqrt(...)

            D_EEQ = EEQ - UVAR(I,1)
            IF (D_EEQ <= ZERO) D_EEQ = ZERO
            UVAR(I,1) = EEQ
          ELSE                               ! positive = plastic strain increment
            D_EEQ = DPLA(I)
            UVAR(I,1) = UVAR(I,1) + DPLA(I)
          ENDIF

c! first principal stress Sigma_1 calculation
          SIG_A  = (SIGNXX(I) + SIGNYY(I))/TWO
          SIG_B  = SQRT(((SIGNXX(I)-SIGNYY(I))/TWO)**2+SIGNXY(I)**2)
          SIG_11 = SIG_A + SIG_B
          SIG_11   = SIG_11 * EMA + (ONE-EMA)* UVAR(I,3)
          UVAR(I,3)= SIG_11
          UVAR(I,2) = UVAR(I,2) + MAX(SIG_11,ZERO) * D_EEQ

          DFMAX(I) =  MIN(UVAR(I,2) / MAX(EM20,ABS(C0)),ONE)

          IF (DFMAX(I) >= ONE) THEN
            NINDX = NINDX + 1
            INDX(NINDX) = I
            FOFF(I) = 0
            TDEL(I) = TIME
          ENDIF
         ENDIF
       ENDDO
C!
       DO J=1,NINDX
         I = INDX(J)
#include "lockon.inc"
          WRITE(IOUT, 2000) NGL(I),IPG,ILAY,IPTT
          WRITE(ISTDO,2100) NGL(I),IPG,ILAY,IPTT,TIME
#include "lockoff.inc"

       ENDDO
c------------------
 2000 FORMAT(1X,'FAILURE (COCKROFT-LATHAM) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',I2,1X,',LAYER',I3,
     .       1X,',INTEGRATION PT',I3)
 2100 FORMAT(1X,'FAILURE (COCKROFT-LATHAM) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',I2,1X,',LAYER',I3,
     .       1X,',INTEGRATION PT',I3,1X,'AT TIME :',1PE12.4)
c------------------

       RETURN
       END
