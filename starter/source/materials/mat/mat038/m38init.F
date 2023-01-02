Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  M38INIT                       source/materials/mat/mat038/m38init.F
Chd|-- called by -----------
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|        SIGIN3B                       source/elements/solid/solid8p/sigin3b.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M38INIT(
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
C---------+---------+---+---+--------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "scr05_c.inc"
C----------------------------------------------------------------
C  I N P U T   A R G U M E N T S
C----------------------------------------------------------------
      INTEGER       NEL, NUPARAM, NUVAR
      my_real
     .      UPARAM(NUPARAM), RHO0(NEL), VOLUME(NEL), EINT(NEL)
C----------------------------------------------------------------
C  O U T P U T   A R G U M E N T S
C----------------------------------------------------------------
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
      INTEGER          MFUNC,IUNLOAD,NUPARAM0,IFLAG,ITOTAL,IMSTA,
     .                 NFUNC1,NFUNCUL,NFUNCP,KCOMPAIR,KRECOVER,KDECAY
      INTEGER          I, J
C     REAL
      my_real
     .   DECAY,TENSIONCUT,TOLERANCE,LAMDA,EFINAL,EPSFIN,
     .   E0,VT,VC,RV,BETA,HYSTER,
     .   RATEDAMP,THETA,P0,RELAXP,MAXPRES,PHI,
     .   GAMMA,VISCOSITY,EXPONAS,EXPONBS,FUNLOAD,RUNLOAD,
     .   PSCALE   
C=======================================================================
C SET INITIAL MATERIAL CONSTANTS

      NUPARAM0= UPARAM(1)

      E0        = UPARAM(2)
      VT        = UPARAM(3)
      VC        = UPARAM(4)
      RV        = UPARAM(5)
      IFLAG     = UPARAM(6)
      ITOTAL    = UPARAM(7)

      BETA      = UPARAM(8)
      HYSTER    = UPARAM(9)
      RATEDAMP  = UPARAM(10)
      KRECOVER  = UPARAM(11)
      KDECAY    = UPARAM(12)
      THETA     = UPARAM(13)

      KCOMPAIR  = UPARAM(14)
      P0        = UPARAM(15)
      GAMMA     = UPARAM(16)
      RELAXP    = UPARAM(17)
      MAXPRES   = UPARAM(18)
      PHI       = UPARAM(19)

      IUNLOAD   = UPARAM(20)
      FUNLOAD   = UPARAM(21)
      RUNLOAD   = UPARAM(22)
      EXPONAS   = UPARAM(23)
      EXPONBS   = UPARAM(24)

      MFUNC     = UPARAM(25)
      IMSTA     = UPARAM(26)
      TENSIONCUT= UPARAM(27)

      EFINAL    = UPARAM(28)
      EPSFIN    = UPARAM(29)
      LAMDA     = UPARAM(30)
      VISCOSITY = UPARAM(31)
      TOLERANCE = UPARAM(32)
      PSCALE    = UPARAM(33)
      NFUNC1=(NFUNC-2)/2
* unloading function number
      NFUNCUL=NFUNC-1
* function number for enclosed air pressure
      NFUNCP=NFUNC

C......................................................................
C INITIALIZE
      DO I=1,NEL
C STRAIN(1-3),STRESS(4-6),STRAIN RATE(7-9)
        DO J=1,9
          UVAR(I,J)=ZERO
        ENDDO
C MODULE (EYN INITIALIZED TO E0)
        DO J=10,12
          UVAR(I,J)=E0
        ENDDO
C POISSON'S RATIO/MODULE
        DO J=13,15
          UVAR(I,J)=VT/E0
        ENDDO
C PRESSURE
        UVAR(I,16)=ZERO
C PRINCIPAL DIRECTIONS
        DO J=17,25
          UVAR(I,J)=ZERO
        ENDDO
        UVAR(I,17)=ONE
        UVAR(I,21)=ONE
        UVAR(I,25)=ONE
C LAW STORAGE FOR DECAY AND HYSTERESIS
        DO J=26,31
          UVAR(I,J)=ZERO
        ENDDO
      ENDDO

      RETURN
      END
