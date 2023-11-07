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
Chd|  FAIL_WILKINS_C                source/materials/fail/wilkins/fail_wilkins_c.F
Chd|-- called by -----------
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FAIL_WILKINS_C(
     1     NEL      ,NUPARAM  ,NUVAR    ,UPARAM   ,UVAR     ,
     2     TIME     ,NGL      ,IPG      ,ILAY     ,IPT      ,
     3     SIGNXX   ,SIGNYY   ,SIGNXY   ,SIGNYZ   ,SIGNZX   ,
     4     DPLA     ,FOFF     ,DFMAX    ,TDEL     )
C-----------------------------------------------
C     2D Wilkins Failure model
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include "units_c.inc"
#include "comlock.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF USER ELEMENT VARIABLES
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | R | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | R | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C DPLA    | NEL     | F |R/W| PLASTIC STRAIN
C---------+---------+---+---+--------------------------------------------
C OFF     | NEL     | F | R | DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C FOFF    | NEL     | I |R/W| DELETED INTEGRATION POINT FLAG (=1 ON, =0 OFF)
C DFMAX   | NEL     | F |R/W| MAX DAMAGE FACTOR 
C TDEL    | NEL     | F | W | FAILURE TIME
C---------+---------+---+---+--------------------------------------------
C NGL                         ELEMENT ID
C IPG                         CURRENT GAUSS POINT (in plane)
C ILAY                        CURRENT LAYER
C IPT                         CURRENT INTEGRATION POINT IN THE LAYER (FOR OUTPUT ONLY)
C---------+---------+---+---+--------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NUPARAM,NUVAR,IPG,ILAY,IPT
      INTEGER ,DIMENSION(NEL) ,INTENT(IN) :: NGL
      my_real ,INTENT(IN) :: TIME
      my_real ,DIMENSION(NEL) ,INTENT(IN) :: DPLA,
     .   SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX
      my_real,DIMENSION(NUPARAM) ,INTENT(IN) :: UPARAM
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      INTEGER ,DIMENSION(NEL) ,INTENT(INOUT) :: FOFF
      my_real ,DIMENSION(NEL) ,INTENT(INOUT) :: DFMAX
      my_real ,DIMENSION(NEL) ,INTENT(OUT)   :: TDEL
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,NINDX
      INTEGER ,DIMENSION(NEL) :: INDX
      my_real :: AL,BE,PC,DC,P,W1,W2,SS,S1,S2,S3,SS0,SS1,SS2,SIG1,SIG2,
     .           A,A1,A2
C=======================================================================
      AL = UPARAM(1)
      BE = UPARAM(2)
      PC = UPARAM(3)
      DC = UPARAM(4)
      NINDX  = 0  
C-----------------------------------------------
      DO I=1,NEL
        IF (FOFF(I) == 1) THEN                               
          SS1 = HALF*(SIGNXX(I) + SIGNYY(I))  
          SS2 = HALF*(SIGNXX(I) - SIGNYY(I))  
          SS0 = SQRT(SS2**2 + SIGNXY(I)**2)     
          SIG1 = SS1  + SS0                       
          SIG2 = SS1  - SS0                       
          SS = SIG1                               
          IF (SIG2 > SIG1) THEN                     
            SIG1 = SIG2                              
            SIG2  = SS                             
          ENDIF
          P   = THIRD*(SIGNXX(I) + SIGNYY(I))
          SS1 = SS1 - P
          S1  = SS1  + SS0
          S2  = SS1  - SS0
          S3  = -P
          IF (SIG2 > ZERO) THEN
            A1 = S2 / S1
            A2 = S2 / S3
          ELSEIF (SIG1 < ZERO) THEN
            A1 = S1 / S3
            A2 = S1 / S2
          ELSEIF (SIG1 /= ZERO) THEN
            A1 = S3 / S1
            A2 = S3 / S2
          ELSE
            A1 = ONE
            A2 = ONE
          ENDIF
          A = MAX (A1,A2)
          W1 = MAX(EM20,(TWO - A))**BE
          W2 = ONE - P/PC
          W2 = (MAX(EM20,ONE/W2))**AL
          UVAR(I,1) = UVAR(I,1) + W1*W2*DPLA(I)             
          IF (UVAR(I,1) >= DC) THEN
            NINDX = NINDX + 1
            INDX(NINDX) = I
            FOFF(I) = 0
            TDEL(I) = TIME
          ENDIF          
        ENDIF 
      ENDDO      
c
c---  Maximum Damage storing for output : 0 < DFMAX < 1
      DO I=1,NEL
        DFMAX(I) = MIN(ONE,MAX(DFMAX(I),UVAR(I,1)/DC))
      ENDDO      
c------------------------
      IF (NINDX > 0) THEN
        DO J=1,NINDX
          I = INDX(J)
#include  "lockon.inc"
          WRITE(IOUT, 2000) NGL(I),IPG,ILAY,IPT
          WRITE(ISTDO,2100) NGL(I),IPG,ILAY,IPT,TIME
#include  "lockoff.inc"
        END DO
      END IF              
c------------------------
 2000 FORMAT(1X,'FAILURE (WILKINS) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3)
 2100 FORMAT(1X,'FAILURE (WILKINS) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',
     .       I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3,1X,'AT TIME :',1PE12.4)
c------------------------
      RETURN
      END
