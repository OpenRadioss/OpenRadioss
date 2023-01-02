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
Chd|  LECM91                        source/materials/mat/mat091/lecm91.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LECM91 (IIN  ,IOUT ,UPARAM ,MAXUPARAM ,NUPARAM,
     .                   NUVAR,IFUNC,MAXFUNC,NFUNC     ,PARMAT , 
     .                   UNITAB,IUNIT,ID,TITR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD                 
C----------------------------------------------------------------
C  I M P L I C I T   T Y P E S 
C----------------------------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C----------------------------------------------------------------
C  D U M M Y   A R G U M E N T S
C----------------------------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      INTEGER, INTENT(IN)  :: IIN,IOUT,MAXUPARAM,MAXFUNC,IUNIT
      INTEGER, INTENT(OUT) :: NUVAR,NFUNC,NUPARAM
      INTEGER, DIMENSION(MAXFUNC) :: IFUNC
      my_real  UPARAM(MAXUPARAM),PARMAT(*)
      INTEGER ID
      CHARACTER*nchartitle, TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   BULK, VIS, YOUNG, ANU, SSP,HH,SIGY,
     .   FAC_M, FAC_L, FAC_T, FAC_C
C=======================================================================
C R E A D I N G
C=======================================================================       
C     
      FAC_M = UNITAB%FAC_M(IUNIT)
      FAC_L = UNITAB%FAC_L(IUNIT)
      FAC_T = UNITAB%FAC_T(IUNIT)
      FAC_C = FAC_M/ (FAC_L*FAC_T*FAC_T)

      READ(IIN,FMT='(A)')LINE           
      READ(LINE,FMT=FMT_2F)YOUNG, ANU
      READ(IIN,FMT='(A)')LINE  
      READ(LINE,FMT=FMT_2F)SIGY, HH

      NUPARAM = 4

      UPARAM(1) = YOUNG
      UPARAM(2) = ANU
      UPARAM(3) = SIGY
      UPARAM(4) = HH

      PARMAT(1) = YOUNG
      PARMAT(2) = YOUNG
      PARMAT(3) = ANU

      NUVAR = 2
      NFUNC = 0 


      WRITE(IOUT,1000)
      WRITE(IOUT,1300)YOUNG,ANU,SIGY,HH

 1000 FORMAT(
     & 5X,' PLASTIC LAW PINCHING SHELL',/,
     & 5X,' -------------------------------------',//)
 1300 FORMAT(
     & 5X,'YOUNG''S MODULUS . . . . . . . .=',1PG20.13/,
     & 5X,'POISSON''S RATIO . . . . . . . .=',1PG20.13/,
     & 5X,'YIELD STRESS . . . . . . . . . .=',1PG20.13/,
     & 5X,'HARDENING MODULUS. . . . . . . .=',1PG20.13//)
      RETURN
      END
