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
Chd|  M77INIT                       source/materials/mat/mat077/m77init.F
Chd|-- called by -----------
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE M77INIT(
     1      NEL    , NUPARAM, NUVAR   , NFUNC  , IFUNC  , NPF   ,
     2      TF     , UPARAM , RHO0    , VOLUME , EINT   ,UVAR   ) 
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include "implicit_f.inc"
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
C UPARAM  | NUPARAM | F | R | USER MATERIAL PARAMETER ARRAY
C RHO0    | NEL     | F | R | INITIAL DENSITY
C VOLUME  | NEL     | F | R | VOLUME
C EINT    | NEL     | F | R | TOTAL INTERNAL ENERGY
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL, NUPARAM, NUVAR
      my_real
     .      UPARAM(NUPARAM), RHO0(NEL), VOLUME(NEL), EINT(NEL)
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
C  I N P U T  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
      my_real
     . UVAR(NEL,NUVAR)
C----------------------------------------------------------------
C  VARIABLES FOR FUNCTION INTERPOLATION 
C----------------------------------------------------------------
      INTEGER NPF(*), NFUNC, IFUNC(NFUNC)
      my_real
     .   FINTER,TF(*)
      EXTERNAL FINTER
C----------------------------------------------------------------
C  L O C A L  V A R I B L E S
C----------------------------------------------------------------
      INTEGER         IADBUF, I, J,IFUNCR,IFUNCK
C     REAL
      my_real
     .        E0,RHO_AIR0,ALPHA,EINT0,KK,MU,DF
C=======================================================================
      E0 = UPARAM(2) 
      RHO_AIR0 =  UPARAM(14 + 2*NFUNC)
      ALPHA   =  UPARAM(17 + 2*NFUNC)
      EINT0    =  UPARAM(20 + 2*NFUNC)
      KK       =  UPARAM(21 + 2*NFUNC)
C
C initialisation of Alpha and KK if Ifuncr and Ifunck is defined
C
      IFUNCR = IFUNC(NFUNC)
      IFUNCK = IFUNC(NFUNC-1)
      MU = ONE
      IF(IFUNCR > 0) ALPHA= ALPHA*FINTER(IFUNCR,MU,NPF,TF,DF)
      IF(IFUNCK > 0) KK= KK*FINTER(IFUNCK,MU,NPF,TF,DF)
C      
      DO I=1,NEL                    
        DO J=1,NUVAR
          UVAR(I,J)=ZERO
        ENDDO
        UVAR(I,1) = RHO_AIR0
        UVAR(I,2) = EINT0
        UVAR(I,3) = ALPHA*VOLUME(I)
        UVAR(I,12) = E0
        UVAR(I,21) = ALPHA
        UVAR(I,22) = KK
      ENDDO

      RETURN
      END
