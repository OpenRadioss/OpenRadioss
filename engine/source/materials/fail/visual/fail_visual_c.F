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
Chd|  FAIL_VISUAL_C                 source/materials/fail/visual/fail_visual_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE FAIL_VISUAL_C(
     .     NEL    ,NUVAR   ,TIME   ,TIMESTEP,UPARAM  ,NGL    ,
     .     SIGNXX ,SIGNYY  ,SIGNXY ,EPSXX   ,EPSYY   ,EPSXY  ,
     .     UVAR   ,OFF     ,DFMAX  ,ISMSTR  )
C--------------------------------------------------------------------
C   /FAIL/VISUAL - Visua failure criteria
C--------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include    "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "units_c.inc"
#include "com08_c.inc"
#include "param_c.inc"
#include "scr17_c.inc"
#include "scr19_c.inc"
#include "comlock.inc"
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
C---------+--------+--+--+-------------------------------------------
C UVAR |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF | NEL | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+--------+--+--+-------------------------------------------
C I N P U T A r g u m e n t s
C-----------------------------------------------
       INTEGER NEL,NUVAR,ISMSTR
       INTEGER NGL(NEL)
       my_real TIME,TIMESTEP
       my_real UPARAM(*),EPSXX(NEL) ,EPSYY(NEL),EPSXY(NEL),DFMAX(NEL)
C-----------------------------------------------
C I N P U T O U T P U T A r g u m e n t s
C-----------------------------------------------
       my_real UVAR(NEL,NUVAR), OFF(NEL),OFFL(NEL),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNXY(NEL)
C-----------------------------------------------
C VARIABLES FOR FUNCTION INTERPOLATION
C-----------------------------------------------
C L o c a l V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,LENG,NINDX,INDX(NEL),TYPE_MAX,F_FLAG,STRDEF,STRFLAG
        my_real E11,SIG_A,SIG_B,SIG_11,EPS_A,EPS_B,
     .          C_MIN,C_MAX,EMA,DAMAGE,ALPHA,F,FF,
     .          PI1,PI8,PI38,SPI8,SPI38,C0,C1,C2,C3,C4,C5,C6,C7,C8,C9,
     .          X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,D,DD,D2,DP,E,G
        my_real A0(2),A1(2),A2(2),AS(3),VS(3)
C-----------------------------------------------
C USER VARIABLES INITIALIZATION

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
      STRDEF    = INT(UPARAM(7))

      NINDX     = 0
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
c-------------------
c
c!  ***********************       
c!  *** NEW ... 03.07.2019  
c!  ***********************      
        DO I=1,NEL
         IF (UVAR(I,1) .LT. 1.0d0 .AND. OFF(I) .EQ. UN)THEN
           
           IF (TYPE_MAX == 1) THEN  

c! stress         
             SIG_A      = (SIGNXX(I) + SIGNYY(I))/DEUX
             SIG_B      = SQRT(((SIGNXX(I)-SIGNYY(I))/DEUX)**2+SIGNXY(I)**2)
             SIG_11     = SIG_A + SIG_B
             E11        = SIG_11
             
           ELSE 
           
c! strains EPSXX  ,EPSYY  ,EPSXY
             EPS_A      = (EPSXX(I) + EPSYY(I)) * UNDEMI
             EPS_B      = SQRT(((EPSXX(I)-EPSYY(I))/DEUX)**2+(UNDEMI*EPSXY(I))**2)
             E11        = EPS_A + EPS_B
             IF (STRFLAG == 1) THEN
               E11 = SQRT(E11 + UN) - UN
             ELSE IF (STRFLAG == 2) THEN
               E11 = EXP(E11) - UN
             ELSE IF (STRFLAG == 3) THEN
               E11 = LOG(E11 + UN)
             ELSE IF (STRFLAG == 4) THEN
               E11 = LOG(SQRT(E11+UN))
             END IF
           ENDIF    
c!  ***********************       
c!  *** NEW ... 23.02.2020  
c!  ***********************      
c! --- EMA or Butterworth filtering
        IF (EMA == UN .and. FF /= ZERO .and. F_FLAG > 1) THEN
C-----------------------------------------------
C INITIALIALISATION OF THE FILTER-COEFFICIENTS 
C-----------------------------------------------
          F       = MIN(FF,ZEP4 / MAX(TIMESTEP,EM20))
          PI1     = DEUX*ATAN2(UN,ZERO)
          PI8     = PI1*UNHUIT
          PI38    = TROIS*PI8
          SPI8    = SIN(PI8)
          SPI38   = SIN(PI38)
C         
          D  = TAN(PI1*F*MAX(TIMESTEP,EM20))
          
          DD = D*D
          D2 = DEUX*D
          DP = UN + DD
          E  = D2*SPI8
          G  = E + DP
          G  = UN/G
C         
          C0 = DD * G
          C1 = DEUX* C0
          C2 = C0
          C3 = DEUX * G - C1
          C4 = (E - DP) * G
C         
          E  = D2*SPI38
          G  = E + DP
          G  = UN/G
C         
          C5 = DD * G
          C6 = DEUX * C5
          C7 = C5
          C8 = DEUX * G - C6
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
     .       + C3 * Y2 + C4 * Y1
          Z1 = A2(2)
          Z2 = A2(1)
          Z3 = C5 * Y3 + C6 * Y2 + C7 * Y1
     .       + C8 * Z2 + C9 * Z1
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

c! EMA = 0 ==> 1 ==> no filtering  ;  EMA = 1e+20 extreme filtering
           
c!           E11        = E11*(DEUX/(UN+EMA)) + UVAR(I,2)*(UN-(DEUX/(UN+EMA)))
          E11        = EMA * E11 + ( UN - EMA ) * UVAR(I,2)
          UVAR(I,2)  = E11       
        ENDIF
c!      
c! What it should be is like this:
c!
c! Value = USER_INPUT * 2 * Pi * DT
c! Alpha = Value / (Value + 1)
c! Actual_filtered_stress = Alpha * actual_Stress + (1-Alpha) * previous_filtered_stress
c!
c!           ALPHA      = EMA / ( EMA + UN)
c!           E11        = ALPHA * E11 + ( UN - ALPHA ) * UVAR(I,2)
c!           UVAR(I,2)  = E11
    
           DAMAGE       =  MAX(ZERO , MIN(UN ,(E11-C_MIN)/MAX(EM6,(C_MAX-C_MIN)) ))
           UVAR(I,1)    =  MAX(UVAR(I,1),DAMAGE)
           DFMAX(I)     =  UVAR(I,1)

           IF (UVAR(I,1) .GE. UN) THEN
             NINDX       = NINDX+1
             INDX(NINDX) = I              
           ENDIF

        ENDIF
        
c!  ***********************  
c!  *** NEW ... 26.06.2019           
c!  ***********************  
        
       ENDDO
       
      DO J=1,NINDX
        I = INDX(J)
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(I),TIME
          WRITE(ISTDO,1100) NGL(I),TIME
#include "lockoff.inc"

      ENDDO

 1000 FORMAT(1X,'SHELL ELEMENT NUMBER (VISUAL) el#',I10,
     .          ' LIMIT REACHED  AT TIME :',1PE12.4)     
 1100 FORMAT(1X,'SHELL ELEMENT NUMBER (VISUAL) el#',I10,
     .          ' LIMIT REACHED  AT TIME :',1PE12.4)     

      RETURN
      END
