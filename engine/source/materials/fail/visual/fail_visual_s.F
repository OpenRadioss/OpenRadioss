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
Chd|  FAIL_VISUAL_S                 source/materials/fail/visual/fail_visual_s.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE FAIL_VISUAL_S(
     .            NEL    ,NUPARAM ,NUVAR  ,TIME   ,TIMESTEP,UPARAM ,
     .            EPSXX  ,EPSYY   ,EPSZZ  ,EPSXY  ,EPSYZ   ,EPSZX  ,
     .            SIGNXX ,SIGNYY  ,SIGNZZ ,SIGNXY ,SIGNYZ  ,SIGNZX ,
     .            UVAR   ,OFF     ,NGL    ,DFMAX  ,ISMSTR  )
C--------------------------------------------------------------------
C   /FAIL/VISUAL - Visua failure criteria
C--------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include    "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
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
C EPSXX | NEL | F | R | STRAIN XX
C EPSYY | NEL | F | R | STRAIN YY
C ... | | | |
C SIGNXX | NEL | F |R/W| NEW ELASTO PLASTIC STRESS XX
C SIGNYY | NEL | F |R/W| NEW ELASTO PLASTIC STRESS YY
C ... | | | |
C---------+--------+--+--+-------------------------------------------
C UVAR |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF | NEL | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+--------+--+--+-------------------------------------------
C I N P U T A r g u m e n t s
C-----------------------------------------------
#include "units_c.inc"
#include "param_c.inc"
#include "scr17_c.inc"
#include "scr19_c.inc"
#include "comlock.inc"
C-----------------------------------------------
        INTEGER NEL,NUPARAM,NUVAR,ISMSTR
        INTEGER  :: NGL(NEL)
        my_real TIME,TIMESTEP,UPARAM(NUPARAM),
     .   EPSXX(NEL) ,EPSYY(NEL) ,EPSZZ(NEL) ,
     .   EPSXY(NEL) ,EPSYZ(NEL) ,EPSZX(NEL) ,DFMAX(NEL)     
C-----------------------------------------------
C I N P U T O U T P U T A r g u m e n t s
C-----------------------------------------------
        my_real UVAR(NEL,NUVAR)
        my_real ,DIMENSION(NEL) :: OFF,
     .   SIGNXX,SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX
C-----------------------------------------------
C L o c a l V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,NINDX,INDX(NEL),TYPE_MAX,F_FLAG,STRDEF,STRFLAG
        my_real PLAMAX,SCAL_1,SCAL_2,SCAL_3,SOC
        my_real I1,I2,I3,E11,E22,E33,E_EQ,E_EQ1,E_EQ2,Q,R,R_INTER,PHI,
     .    C_MIN,C_MAX,EMA,DAMAGE,ALPHA,F,FF,
     .    PI1,PI8,PI38,SPI8,SPI38,C0,C1,C2,C3,C4,C5,C6,C7,C8,C9,
     .    X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,D,DD,D2,DP,E,G
        my_real A0(2),A1(2),A2(2),AS(3),VS(3)      
        my_real ,DIMENSION(NEL) :: EPS1,EPS2,EPS3
C=======================================================================
C USER VARIABLES

c! User variable # 1, to store the previous damage value
c! User variable # 2, to store the previous stress or strain value (for EMA filtering)
c! User variable # 3-8, Storage values for the Butterworth filter
C-----------------------------------------------
C...    
      TYPE_MAX  = INT(UPARAM(1))
      C_MIN     = UPARAM(2)
      C_MAX     = UPARAM(3)
      EMA       = UPARAM(4)
      FF        = UPARAM(5)
      F_FLAG    = INT(UPARAM(6))
      STRDEF    = NINT(UPARAM(7))
      NINDX     = 0
      F         = MIN(FF,ZEP4/MAX(EM20,TIMESTEP))
c----------------------------------------------
c     strain transformation flag following input definition
c-------------------
      STRFLAG = 0
      IF (STRDEF == 2) THEN        ! failure defined as engineering strain
        IF (ISMSTR == 10 .or. ISMSTR == 12) THEN
          STRFLAG = 1
        ELSE IF (ISMSTR == 0 .or. ISMSTR == 2 .or. ISMSTR == 4) THEN
          STRFLAG = 2
        END IF
      ELSE IF (STRDEF == 3) THEN   ! failure defined as true strain
        IF (ISMSTR == 1 .or. ISMSTR == 3 .or. ISMSTR == 11) THEN
          STRFLAG = 3
        ELSE IF (ISMSTR == 10 .or. ISMSTR == 12) THEN
          STRFLAG = 4
        END IF
      END IF           
             
c!  ***********************       
c!  *** NEW ... 26.06.2019  
c!  ***********************      
      DO I=1,NEL
        IF (UVAR(I,1) < ONE) THEN
           
          IF (TYPE_MAX == 1) THEN   ! stress based
c
             I1 = SIGNXX(I)+SIGNYY(I)+SIGNZZ(I)
             I2 = SIGNXX(I)*SIGNYY(I)+SIGNYY(I)*SIGNZZ(I)+SIGNZZ(I)*SIGNXX(I)-
     .            SIGNXY(I)*SIGNXY(I)-SIGNZX(I)*SIGNZX(I)-SIGNYZ(I)*SIGNYZ(I)
             I3 = SIGNXX(I)*SIGNYY(I)*SIGNZZ(I)-SIGNXX(I)*SIGNYZ(I)*SIGNYZ(I)-
     .            SIGNYY(I)*SIGNZX(I)*SIGNZX(I)-SIGNZZ(I)*SIGNXY(I)*SIGNXY(I)+
     .            TWO*SIGNXY(I)*SIGNZX(I)*SIGNYZ(I)
             Q  = (THREE*I2 - I1*I1)/9.0
             Q = MIN(Q, ZERO)
             R  = (TWO*I1*I1*I1-9.0*I1*I2+27.0*I3)/54.0     ! (2*I3^3-9*I1*I2+27*I3)/54
             R_INTER = MIN(R/SQRT(MAX(EM20,(-Q**3))),ONE)
             PHI = ACOS(MAX(R_INTER,-ONE))
             E11 = TWO*SQRT(-Q)*COS(PHI/THREE)+THIRD*I1
             E22 = TWO*SQRT(-Q)*COS((PHI+TWO*PI)/THREE)+THIRD*I1
             E33 = TWO*SQRT(-Q)*COS((PHI+4.0*PI)/THREE)+THIRD*I1
             
             IF (E11 < E22) THEN 
                R_INTER = E11
                E11     = E22
                E22     = R_INTER
             ENDIF 
             IF (E22 < E33)THEN
                R_INTER = E22
                E22     = E33
                E33     = R_INTER
             ENDIF
             IF (E11 < E22)THEN
                R_INTER = E11
                E11     = E22
                E22     = R_INTER
             ENDIF
c
          ELSE ! TYPE_MAX   - strain based criterion
           
c! strains (deviatoric strain are in vector notation ==> gamma...==> division by 2 to get eps tensor components)
           
             I1 = EPSXX(I)+EPSYY(I)+EPSZZ(I)
             I2 = EPSXX(I)*EPSYY(I)+EPSYY(I)*EPSZZ(I)+EPSZZ(I)*EPSXX(I)-
     .            HALF*EPSXY(I)*HALF*EPSXY(I)-HALF*EPSZX(I)*HALF*EPSZX(I)-
     .            HALF*EPSYZ(I)*HALF*EPSYZ(I)
             I3 = EPSXX(I)*EPSYY(I)*EPSZZ(I)-EPSXX(I)*HALF*EPSYZ(I)*HALF*EPSYZ(I)-
     .            EPSYY(I)*HALF*EPSZX(I)*HALF*EPSZX(I)-EPSZZ(I)*HALF*EPSXY(I)*HALF*EPSXY(I)+
     .            TWO*HALF*EPSXY(I)*HALF*EPSZX(I)*HALF*EPSYZ(I)
             Q  = (THREE*I2 - I1*I1)/9.0
             R  = (TWO*I1*I1*I1-9.0*I1*I2+27.0*I3)/54.0     ! (2*I3^3-9*I1*I2+27*I3)/54
             
             R_INTER = MIN(R/SQRT(MAX(EM20,(-Q**3))),ONE)
             PHI = ACOS(MAX(R_INTER,-ONE))
             
             E11 = TWO*SQRT(-Q)*COS(PHI/THREE)+THIRD*I1
             E22 = TWO*SQRT(-Q)*COS((PHI+TWO*PI)/THREE)+THIRD*I1
             E33 = TWO*SQRT(-Q)*COS((PHI+4.0*PI)/THREE) +THIRD*I1
c
             IF (STRFLAG == 1) THEN
               E11 = SQRT(E11 + ONE) - ONE
               E22 = SQRT(E22 + ONE) - ONE
               E33 = SQRT(E33 + ONE) - ONE
             ELSE IF (STRFLAG == 2) THEN
               E11 = EXP(E11) - ONE
               E22 = EXP(E22) - ONE
               E33 = EXP(E33) - ONE
             ELSE IF (STRFLAG == 3) THEN
               E11 = LOG(E11 + ONE)
               E22 = LOG(E22 + ONE)
               E33 = LOG(E33 + ONE)
             ELSE IF (STRFLAG == 4) THEN
               E11 = LOG(SQRT(E11 + ONE))
               E22 = LOG(SQRT(E22 + ONE))
               E33 = LOG(SQRT(E33 + ONE))
             END IF
c             
             IF (E11 < E22) THEN 
                R_INTER = E11
                E11     = E22
                E22     = R_INTER
             ENDIF 
             IF (E22 < E33)THEN
                R_INTER = E22
                E22     = E33
                E33     = R_INTER
             ENDIF
             IF (E11 < E22)THEN
                R_INTER = E11
                E11     = E22
                E22     = R_INTER
             ENDIF                     
c
          ENDIF ! TYPE_MAX
c
c! ---    EMA or Butterworth filtering
          IF (EMA == ONE .and. FF /= ZERO .and. F_FLAG > 1) THEN
C-----------------------------------------------
C           INITIALIALISATION OF THE FILTER-COEFFICIENTS 
C-----------------------------------------------

            PI1     = TWO*ATAN2(ONE,ZERO)
            PI8     = PI1*ONE_OVER_8
            PI38    = THREE*PI8
            SPI8    = SIN(PI8)
            SPI38   = SIN(PI38)
C         
            D  = TAN(PI1*F*TIMESTEP)
          
            DD = D*D
            D2 = TWO*D
            DP = ONE + DD
            E  = D2*SPI8
            G  = E + DP
            G  = ONE/G
C         
            C0 = DD * G
            C1 = TWO* C0
            C2 = C0
            C3 = TWO * G - C1
            C4 = (E - DP) * G
C         
            E  = D2*SPI38
            G  = E + DP
            G  = ONE/G
C         
            C5 = DD * G
            C6 = TWO * C5
            C7 = C5
            C8 = TWO * G - C6
            C9 = (E - DP) * G
          
C-----------------------------------------------
C BUTTERWORTH FILTERING
C-----------------------------------------------
         
            A0(1)     = UVAR(I,3)
            A0(2)     = UVAR(I,4) 
            A1(1)     = UVAR(I,5) 
            A1(2)     = UVAR(I,6) 
            A2(1)     = UVAR(I,7) 
            A2(2)     = UVAR(I,8) 

            X1 = A0(2)
            X2 = A0(1)
          
            X3 = E11
            Y1 = A1(2)
            Y2 = A1(1)
            Y3 = C0 * X3 + C1 * X2 + C2 * X1
     .         + C3 * Y2 + C4 * Y1
            Z1 = A2(2)
            Z2 = A2(1)
            Z3 = C5 * Y3 + C6 * Y2 + C7 * Y1
     .         + C8 * Z2 + C9 * Z1
C         
            A0(2) = X2
            A0(1) = X3
            A1(2) = Y2
            A1(1) = Y3
            A2(2) = Z2
            A2(1) = Z3
          
            UVAR(I,3) = A0(1)
            UVAR(I,4) = A0(2)
            UVAR(I,5) = A1(1)
            UVAR(I,6) = A1(2)
            UVAR(I,7) = A2(1)
            UVAR(I,8) = A2(2)
          
            E11       = A2(1)
          
           
          ELSE           
c!    
c! What it should be is like this:
c!
c! Value = USER_INPUT * 2 * Pi * DT1
c! Alpha = Value / (Value + 1)
c! Actual_filtered_stress = Alpha * actual_Stress + (1-Alpha) * previous_filtered_stress
c!
c!           ALPHA      = EMA / ( EMA + ONE)
c!           E11        = ALPHA * E11 + ( ONE - ALPHA ) * UVAR(I,2)
c!           UVAR(I,2)  = E11

c! EMA = 0 ==> 1 ==> no filtering  ;  EMA = 1e+20 extreme filtering
c!           E11        = E11*(TWO/(ONE+EMA)) + UVAR(I,2) * (ONE-(TWO/(ONE+EMA))) 
           E11        = EMA * E11 + ( ONE - EMA ) * UVAR(I,2)
           UVAR(I,2)  = E11
          ENDIF
c!      
          DAMAGE       =  MAX(ZERO , MIN(ONE ,(E11-C_MIN)/MAX(EM6,(C_MAX-C_MIN)) ))
          UVAR(I,1)    =  MAX(UVAR(I,1),DAMAGE)
          DFMAX(I)     =  UVAR(I,1)

          IF (UVAR(I,1) >= ONE) THEN
             NINDX       = NINDX+1
             INDX(NINDX) = I              
          ENDIF

        ENDIF ! UVAR(I,1) < ONE
        
      ENDDO   ! NEL
c---------------------------------------------       
      DO J=1,NINDX
        I = INDX(J)
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(I),TIME
          WRITE(ISTDO,1100) NGL(I),TIME
#include "lockoff.inc"

      ENDDO
c---------------------------------------------       
 1000 FORMAT(1X,'SOLID ELEMENT NUMBER (VISUAL) el#',I10,
     .          ' LIMIT REACHED  AT TIME :',1PE12.4)     
 1100 FORMAT(1X,'SOLID ELEMENT NUMBER (VISUAL) el#',I10,
     .          ' LIMIT REACHED  AT TIME :',1PE12.4)     
c-----------
      RETURN
      END
