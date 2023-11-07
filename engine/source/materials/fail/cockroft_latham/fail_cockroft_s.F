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
Chd|  FAIL_COCKROFT_S               source/materials/fail/cockroft_latham/fail_cockroft_s.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE FAIL_COCKROFT_S(
     1     NEL ,NUPARAM ,NUVAR ,
     2     TIME ,TIMESTEP ,UPARAM ,NGL ,
     4     EPSPXX ,EPSPYY ,EPSPZZ ,EPSPXY ,EPSPYZ ,EPSPZX ,
     5     EPSXX ,EPSYY ,EPSZZ ,EPSXY ,EPSYZ ,EPSZX ,
     6     SIGNXX ,SIGNYY ,SIGNZZ ,SIGNXY ,SIGNYZ ,SIGNZX ,
     7     PLA ,DPLA ,EPSP ,UVAR ,OFF ,
     8     DFMAX,TDELE )

C--------------------------------------------------------------------
C   /FAIL/COCKROFT - Cockroft-Latham failure criteria for solids
C--------------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include    "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C---------+--------+--+--+-------------------------------------------
C VAR | SIZE |TYP| RW| DEFINITION
C---------+--------+--+--+-------------------------------------------
C NEL | 1 | I | R | SIZE OF THE ELEMENT GROUP NEL
C NUPARAM | 1 | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR | 1 | I | R | NUMBER OF FAILURE ELEMENT VARIABLES
C---------+--------+--+--+-------------------------------------------
C TIME | 1 | F | R | CURRENT TIME
C TIMESTEP| 1 | F | R | CURRENT TIME STEP
C UPARAM | NUPARAM | F | R | USER FAILURE PARAMETER ARRAY
C---------+--------+--+--+-------------------------------------------
C EPSPXX | NEL | F | R | STRAIN RATE XX
C EPSPYY | NEL | F | R | STRAIN RATE YY
C ... | | | |
C EPSXX | NEL | F | R | STRAIN XX
C EPSYY | NEL | F | R | STRAIN YY
C ... | | | |
C SIGNXX | NEL | F |R/W| NEW ELASTO PLASTIC STRESS XX
C SIGNYY | NEL | F |R/W| NEW ELASTO PLASTIC STRESS YY
C ... | | | |
C ... | | | |
C PLA | NEL | F | R | PLASTIC STRAIN
C DPLA | NEL | F | R | INCREMENTAL PLASTIC STRAIN
C EPSP | NEL | F | R | EQUIVALENT STRAIN RATE
C---------+--------+--+--+-------------------------------------------
C UVAR |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF | NEL | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+--------+--+--+-------------------------------------------
C I N P U T A r g u m e n t s
C-----------------------------------------------
#include "units_c.inc"
#include "scr17_c.inc"
#include "comlock.inc"

C
        INTEGER NEL, NUPARAM, NUVAR,NGL(NEL)

        my_real TIME,TIMESTEP,UPARAM(NUPARAM),
     .   DPLA(NEL),EPSP(NEL),PLA(NEL),
     .   EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL),
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   EPSXX(NEL) ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,
     .   TDELE(NEL) ,DFMAX(NEL)     
C-----------------------------------------------
C I N P U T O U T P U T A r g u m e n t s
C-----------------------------------------------
        my_real UVAR(NEL,NUVAR), OFF(NEL),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL)
C-----------------------------------------------
C L o c a l V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,LENG,NINDX,INDX(NEL),NCYC
        my_real C0,EMA,E_HYD,E_11,E_22,E_33,E_12,E_23,
     .          E_13,EEQ,D_EEQ
        my_real I1,I2,I3,S11,S22,S33,
     .                   Q,R,R_INTER,PHI,LOCKROFT

C--------------------------------------------------------------
C-----------------------------------------------
C! USER VARIABLES INITIALIZATION
c!
c! from:
c! Calibration and evaluation of seven fracture models
c! Tomasz Wierzbicki, Yingbin Bao, Young-Woong Lee, Yuanli Bai
c!
c! The well known Cockcroft   Latham, [28] fracture criterion is also studied here. It
c! was developed for the bulk forming operations and therefore is applicable only to the range
c! of small and negative stress triaxilaity. According to this criterion, fracture occurs when
c! the accumulated equivalent strain modified by maximum principal tensile stress reaches a
c! critical value.
c!
C-----------------------------------------------
C...    
C!      UVAR(I,1) contains previous equivalent strain value increment
C!      UVAR(I,2) contains the Cockroft-Latham accumulated value
c!      UVAR(I,3) contains the previous first principal stress

      C0       = UPARAM(1)
      EMA      = UPARAM(2)

      LOCKROFT = ZERO
      E_HYD    = ZERO
      EEQ      = ZERO
      D_EEQ    = ZERO
      R_INTER  = ZERO
      PHI      = ZERO
      
      LENG     = 0
      
      
c!  ***********************      
      NINDX = 0

C-----------------------------------------------
c! fast degradation
        DO I=1,NEL
         IF(OFF(I)<ONE .AND. OFF(I) > ZERO) THEN 
           SIGNXX(I) = SIGNXX(I) * OFF(I)
           SIGNYY(I) = SIGNYY(I) * OFF(I)
           SIGNZZ(I) = SIGNZZ(I) * OFF(I)
           SIGNXY(I) = SIGNXY(I) * OFF(I)
           SIGNYZ(I) = SIGNYZ(I) * OFF(I)
           SIGNZX(I) = SIGNZX(I) * OFF(I)
           OFF(I)    = OFF(I) * FOUR_OVER_5
         ENDIF
         IF(OFF(I)<EM01) OFF(I) = ZERO
        END DO
C-------------------  
      
      
      DO I=1,NEL
        IF(OFF(I)==ONE) THEN
        
c!          COCKROFT = UVAR(I,2)
c! equivalent strain calculation (negative = total strain ; positive = plastic strain)
          IF(C0 < ZERO)THEN
            E_HYD = THIRD * (EPSXX(I) + EPSYY(I) + EPSZZ(I))
            E_11  = EPSXX(I) - E_HYD
            E_22  = EPSYY(I) - E_HYD
            E_33  = EPSZZ(I) - E_HYD
            E_12  = HALF*EPSXY(I)
            E_23  = HALF*EPSYZ(I)
            E_13  = HALF*EPSZX(I)
          
            EEQ   = E_11**2 + E_22**2 + E_33**2
            EEQ   = EEQ + TWO * (E_12**2) + TWO * (E_23**2) + TWO * (E_13**2)
            EEQ   = 0.8164965809 * SQRT(EEQ) ! sqrt (2/3)*sqrt(...)

            D_EEQ = EEQ - UVAR(I,1)
            IF (D_EEQ <= ZERO) D_EEQ = ZERO
            UVAR(I,1) = EEQ  
          ELSE
            D_EEQ = DPLA(I)
            UVAR(I,1) = UVAR(I,1) + DPLA(I)
          ENDIF
        
c!   principal stress calculation

          I1 = SIGNXX(I)+SIGNYY(I)+SIGNZZ(I)
          I2 = SIGNXX(I)*SIGNYY(I)+SIGNYY(I)*SIGNZZ(I)+SIGNZZ(I)*SIGNXX(I)-
     .         SIGNXY(I)*SIGNXY(I)-SIGNZX(I)*SIGNZX(I)-SIGNYZ(I)*SIGNYZ(I)
          I3 = SIGNXX(I)*SIGNYY(I)*SIGNZZ(I)-SIGNXX(I)*SIGNYZ(I)*SIGNYZ(I)-
     .         SIGNYY(I)*SIGNZX(I)*SIGNZX(I)-SIGNZZ(I)*SIGNXY(I)*SIGNXY(I)+
     .         TWO*SIGNXY(I)*SIGNZX(I)*SIGNYZ(I)
          Q  = (THREE*I2 - I1*I1)/NINE
          R  = (TWO*I1*I1*I1-NINE*I1*I2+TWENTY7*I3)/CINQUANTE4     ! (2*I3^3-9*I1*I2+27*I3)/54
           
          R_INTER = MIN(R/SQRT(MAX(EM20,(-Q**3))),ONE)
          PHI = ACOS(MAX(R_INTER,-ONE))
           
          S11 = TWO*SQRT(-Q)*COS(PHI/THREE)+THIRD*I1
          S22 = TWO*SQRT(-Q)*COS((PHI+TWO*PI)/THREE)+THIRD*I1
          S33 = TWO*SQRT(-Q)*COS((PHI+FOUR*PI)/THREE)+THIRD*I1

          IF (S11<S22) THEN 
            R_INTER = S11
            S11     = S22
            S22     = R_INTER
          ENDIF 
          IF (S22<S33)THEN
            R_INTER = S22
            S22     = S33
            S33     = R_INTER
          ENDIF
          IF (S11<S22)THEN
            R_INTER = S11
            S11     = S22
            S22     = R_INTER
          ENDIF
          
          IF(S11 > ZERO) THEN
            S11       = S11 * EMA + (ONE-EMA)* UVAR(I,3)
            UVAR(I,3) = S11

            UVAR(I,2) = UVAR(I,2) + MAX(S11,ZERO) * D_EEQ
          ENDIF 
          
          DFMAX(I) = UVAR(I,2) / MAX(ABS(C0),EM20)
          DFMAX(I) = MIN(ONE,DFMAX(I))
         
          IF (UVAR(I,2) >= ABS(C0)) THEN
            TDELE(I)    = TIME  
            NINDX       = NINDX+1
            INDX(NINDX) = I              
            IDEL7NOK    = 1   
            
            OFF(I) = FOUR_OVER_5

          ENDIF
        
c!        PRINT *,' UVAR(I,2) =',UVAR(I,2),' el#= ',NGL(I)
          
        ENDIF
                     
      ENDDO
      
      DO J=1,NINDX
        I = INDX(J)
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(I),TIME
          WRITE(ISTDO,1100) NGL(I),TIME
#include "lockoff.inc"

      ENDDO

 1000 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER (COCKROFT-LATHAM) el#',I10,
     .          ' AT TIME :',1PE12.4)     
 1100 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER (COCKROFT-LATHAM) el#',I10,
     .          ' AT TIME :',1PE12.4)     

      RETURN
      END

       
