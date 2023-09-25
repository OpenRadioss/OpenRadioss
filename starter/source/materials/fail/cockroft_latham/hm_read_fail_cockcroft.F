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
Chd|  HM_READ_FAIL_COCKCROFT        source/materials/fail/cockroft_latham/hm_read_fail_cockcroft.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_COCKCROFT(UPARAM, MAXUPARAM, NUPARAM,
     .     NUVAR, IFUNC, MAXFUNC, NFUNC, FAIL_ID, 
     .     LSUBMODEL, UNITAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE SUBMODEL_MOD
C---------------------------------------------------------------------
C I m p l i c i t  T y p e s
C---------------------------------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C     IOUT
#include "units_c.inc"
C-----------------------------------------------
C     D u m m y  A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: MAXUPARAM, MAXFUNC
      INTEGER, INTENT(INOUT) :: NUPARAM, NUVAR, NFUNC, 
     .     IFUNC(MAXFUNC)
      my_real, INTENT(INOUT) ::  UPARAM(MAXUPARAM)
      INTEGER, INTENT(INOUT) :: FAIL_ID
      TYPE (UNIT_TYPE_), INTENT(IN) ::UNITAB
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C     L o c a l  V a r i a b l e s
C-----------------------------------------------
      my_real :: C0, EMA
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE
C-------------------------------------------------
C     B e g i n n i n g   o f  S u b r o u t i n e
C-------------------------------------------------
!     =================================    !
!     COCKROFT-LATHAM Rupture Criterion    !
!     =================================    !
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
!     Initializations
      NFUNC  = 0
      NUPARAM = 2
      NUVAR = 3
      C0  = ZERO
!     Encryption flag
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)      
!     Line 1
      CALL HM_GET_FLOATV('C0', C0, IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('ALPHA', EMA, IS_AVAILABLE, LSUBMODEL, UNITAB)  
!     Default values
      IF (EMA == ZERO) EMA = ONE

      UPARAM(1) = C0
      UPARAM(2) = EMA

      IF (IS_ENCRYPTED) THEN
         WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
         WRITE(IOUT, 1000) ABS(C0), EMA
         IF(C0 < ZERO) THEN
            WRITE(IOUT, 1100)
         ELSE
            WRITE(IOUT, 1200)       
         ENDIF       
      ENDIF  
      
 1000 FORMAT(
     &     5X,40H    COCKROFT-LATHAM FAILURE CRITERIA          /,
     &     5X,40H    --------------------------------          /,
     &     5X,' COEFFICIENT C0. . . . . . . . . . . . .=',E12.4,/,
     &     5X,' FILTER COEFFICIENT EMA. . . . . . . . .=',E12.4,/,
     &     5X,'       ( EMA = 1.0: NO FILTERING )',/)
 1100 FORMAT(
     &     5X,' TOTAL STRAIN INCTEMENT USED',/
     &     5X,' --------------------------------------------------------- ',//)
 1200 FORMAT(
     &     5X,' PLASTIC STRAIN INCTEMENT USED',/
     &     5X,' --------------------------------------------------------- ',//)
      RETURN
      END
