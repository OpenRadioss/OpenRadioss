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
Chd|  FAIL_MULLINS_OR_S             source/materials/fail/mullins_or/fail_mullins_OR_s.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|        MULLINS_OR                    source/materials/fail/mullins_or/fail_mullins_OR_s.F
Chd|====================================================================
      SUBROUTINE FAIL_MULLINS_OR_S(
     1     NEL    ,NUPARAM  ,NUVAR   ,
     2     TIME   ,TIMESTEP ,UPARAM  ,NGL    ,  
     3     SIGNXX ,SIGNYY   ,SIGNZZ  ,SIGNXY  ,
     4     SIGNYZ ,SIGNZX   ,UVAR    ,OFF    ,
     5     DFMAX  ,WW)
C---------+---------+---+---+--------------------------------------------
c---------+---------+---+---+--------------------------------------------
C   /FAIL/EMC - tabulated rupture criteria for solids
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF FAILURE ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER FAILURE PARAMETER ARRAY
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER,  INTENT(IN) ::  NEL,NUPARAM  ,NUVAR
      my_real,  INTENT(IN) ::  TIME,TIMESTEP
      my_real, DIMENSION(NUPARAM),  INTENT(IN) :: UPARAM
      my_real, DIMENSION(NEL), INTENT(IN) ::NGL, WW
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------
cc      my_real
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real, DIMENSION(NEL), INTENT(INOUT) :: SIGNXX,
     .   SIGNYY,SIGNZZ,SIGNXY,SIGNYZ,SIGNZX,DFMAX, OFF
      my_real, DIMENSION(NEL,NUVAR), INTENT(INOUT) :: UVAR      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,JJ,K,IR,IADBUF,NINDX,IFAIL
      INTEGER, DIMENSION(NEL) :: INDX
      my_real 
     .    COEFR, BETAF ,COEFM,SXX,SYY,SZZ,TRACE,ETA(NEL)
C=======================================================================
C     INITIALIZATIONS
C-----------------------------------------------
      COEFR = UPARAM(1)    
      BETAF = UPARAM(2)    
      COEFM = UPARAM(3)   

       CALL MULLINS_OR(
     1                NEL  ,NUVAR,  COEFR,BETAF , 
     2                COEFM,WW  , UVAR, ETA )

c-----------------------------------------------
      DO I = 1,NEL
       TRACE =   THIRD * (SIGNXX(I)+SIGNYY(I)+SIGNZZ(I))
       SXX = SIGNXX(I) - TRACE
       SYY = SIGNYY(I) - TRACE
       SZZ = SIGNZZ(I) - TRACE
       SIGNXX(I) = ETA(I) * SXX + TRACE
       SIGNYY(I) = ETA(I) * SYY + TRACE
       SIGNZZ(I) = ETA(I) * SZZ + TRACE

       SIGNXY(I) = ETA(I) * SIGNXY(I)
       SIGNYZ(I) = ETA(I) * SIGNYZ(I) 
       SIGNZX(I) = ETA(I) * SIGNZX(I) 

      ENDDO 


c-----------           
      RETURN
      END
Chd|====================================================================
Chd|  MULLINS_OR                    source/materials/fail/mullins_or/fail_mullins_OR_s.F
Chd|-- called by -----------
Chd|        FAIL_MULLINS_OR_S             source/materials/fail/mullins_or/fail_mullins_OR_s.F
Chd|        NEO_HOOK_T                    source/materials/mat/mat100/neo_hook_t.F
Chd|        POLYSTRESS2                   source/materials/mat/mat100/sigpoly.F
Chd|        POLYSTREST2                   source/materials/mat/mat100/sigpoly.F
Chd|        SIGABOYCE                     source/materials/mat/mat100/sigaboyce.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MULLINS_OR(
     1                NEL ,NUVAR,  COEFR,BETAF ,
     2                COEFM, WW  , UVAR, ETA )
C---------+---------+---+---+--------------------------------------------
c---------+---------+---+---+--------------------------------------------
C   /FAIL/EMC - tabulated rupture criteria for solids
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C NUVAR   |  1      | I | R | NUMBER OF FAILURE ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C UPARAM  | NUPARAM | F | R | USER FAILURE PARAMETER ARRAY
C---------+---------+---+---+--------------------------------------------
C SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C ...     |         |   |   |
C ...     |         |   |   |
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) ::  NEL,NUVAR
      my_real, INTENT(IN) ::  COEFR,BETAF ,COEFM 
      my_real, DIMENSION(NEL), INTENT(IN) :: WW
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real, DIMENSION(NEL), INTENT(OUT) :: ETA(NEL)
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------
      my_real, DIMENSION(NEL,NUVAR ), INTENT(INOUT) ::  UVAR
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,JJ,K,IR,IADBUF,NINDX,IFAIL
      INTEGER, DIMENSION(NEL) :: 
     .   INDX
C
      my_real, DIMENSION(NEL)  ::  WMAX
      my_real :: DF12, DF23, DF13,TERM
C=======================================================================
      DO I = 1, NEL 
       WMAX(I)  = MAX (UVAR(I,1),WW(I))
       UVAR(I,1)= WMAX(I)
      ENDDO 
      DO I=1,NEL !erf(x) computes the error function of x integral from 0 to x

        TERM = ERF((WMAX(I) - WW(I))/(COEFM + BETAF *WMAX(I)))

        ETA(I) = ONE - TERM/COEFR 
      ENDDO 

C-----------------------------------------------


c-----------           
      RETURN
      END






