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
Chd|  LECM85_VOID                   source/materials/mat/mat085/lecm85_void.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LECM85_VOID(IIN  ,IOUT ,UPARAM ,MAXUPARAM,NUPARAM,
     .                  NUVAR,IFUNC,MAXFUNC,NFUNC    ,PARMAT )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C IIN      |  1      | I | R | INPUT FILE UNIT (D00 file) 
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C UPARAM   | NUPARAM | F | W | USER MATERIAL PARAMETER ARRAY
C MAXUPARAM|  1      | I | R | MAXIMUM SIZE OF UPARAM 
C NUPARAM  |  1      | I | W | SIZE OF UPARAM =< MAXUPARAM
C NUVAR    |  1      | I | W | NUMBER OF USER ELEMENT VARIABLES
C----------+---------+---+---+--------------------------------------------
C IFUNC    | NFUNC   | I | W | FUNCTION NUMBER ARRAY
C MAXFUNC  |  1      | I | R | MAXIMUM SIZE OF IFUNC
C NFUNC    |  1      | I | W | SIZE OF IFUNC =< MAXFUNC
C----------+---------+---+---+--------------------------------------------
C (STIFINT)|                 | Replaced by PARMAT(1)
C PARMAT   |  3      | F | W | 1)STIFNESS MODULUS FOR INTERFACE
C          |(in v31) |   |   |       (AND INITIAL TIME STEP COMPUTATION)
C          |         |   |   | 2)YOUNG MODULUS (only needed for shells)
C          |         |   |   | 3)POISSON'S RATIO (only needed for shells)
C          |         |   |   | 4... Possible future use
C----------+---------+---+---+--------------------------------------------
C
C     This subroutine read the user material parameters.
C     The material cards that are common for all materials
C     (card 1 to 7 in version 2.2) have previously been read.
C     The NUPARAM material datas have to bee stored in UPARAM array.
C     If some standard radioss functions (time function or 
C     x,y function) are needed, this NFUNC function numbers have to 
C     bee stored in IFUNC array.  
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IIN,IOUT,MAXUPARAM,NUPARAM,NUVAR,MAXFUNC,NFUNC,
     .        IFUNC(MAXFUNC)
      my_real   UPARAM(MAXUPARAM),PARMAT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real E,NU,A11,A12,A44
C=======================================================================
C
C     EXEMPLE : ELASTIC LAW 
C
C=======================================================================
C
      READ(IIN,'(2F16.0)')E,NU
      A11 = E * (ONE - NU) / (ONE + NU) / (ONE - TWO*NU)
      A12 = E * NU / (ONE + NU) / (ONE - TWO*NU)
      A44 = E / TWO / (ONE+NU)
C
      IF(NU.LT.ZERO.OR.NU.GE.HALF)THEN
         WRITE(IOUT,*)' ** ERROR : WRONG NU VALUE'
      ENDIF
      NUPARAM = 7
      IF(NUPARAM.GT.MAXUPARAM)THEN
        WRITE(IOUT,*)' ** ERROR : NUPARAM GT MAXUPARAM'
        WRITE(IOUT,*)'      NUPARAM =',NUPARAM,
     .                ' MAXUPARAM =',MAXUPARAM
      ELSE
C used in sigeps29 (solid 2d,3d)
        UPARAM(1) = A11 
        UPARAM(2) = A12
        UPARAM(3) = A44
        UPARAM(4) = E/(ONE-NU*NU)
        UPARAM(5) = NU*E/(ONE - NU*NU)
        UPARAM(6) = HALF*E/(ONE + NU)

        UPARAM(7) = E
      ENDIF
C
      NUVAR = 0
      NFUNC = 0
C used for interface (solid+shell)
      PARMAT(1) = A11
C used for shell
      PARMAT(2) = E
      PARMAT(3) = NU
C
      WRITE(IOUT,1000)
      WRITE(IOUT,1100)E,NU
C
 1000 FORMAT(
     & 5X,'  ELASTIC USER LAW ',/,
     & 5X,'  ---------------- ',//)
 1100 FORMAT(
     & 5X,'E . . . . . . . . . . . . . . . . . . .=',E12.4/
     & 5X,'NU. . . . . . . . . . . . . . . . . . .=',E12.4//)
C
      RETURN
      END
