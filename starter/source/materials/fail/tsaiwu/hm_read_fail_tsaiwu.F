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
Chd|  HM_READ_FAIL_TSAIWU           source/materials/fail/tsaiwu/hm_read_fail_tsaiwu.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_TSAIWU(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR  ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,UNITAB   ,FAIL_ID,MAT_ID   ,
     .           TITR     ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD 
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN)   ::UNITAB 
      INTEGER,INTENT(IN)             ::MAT_ID,FAIL_ID
      TYPE(SUBMODEL_DATA),INTENT(IN) ::LSUBMODEL(*)
      CHARACTER,INTENT(IN)           ::TITR*nchartitle
      INTEGER,INTENT(INOUT)          ::IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM
      my_real,INTENT(INOUT)          ::UPARAM(MAXUPARAM)
      INTEGER,INTENT(OUT)            ::NUPARAM,NUVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IFAIL_SH,IFAIL_SO
      my_real 
     .    SIGT1,SIGT2,SIGT12,SIGC1,SIGC2,
     .    ALPHA,TMAX,FCUT,F1,F2,F11,F22,F66,F12
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C--------------------------------------------------
C   S o u r c e   L i n e s
C--------------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      ! 1st line of the card
      CALL HM_GET_FLOATV    ('SIGMA_1T'   ,SIGT1      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('SIGMA_2T'   ,SIGT2      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('SIGMA_12'   ,SIGT12     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('SIGMA_1C'   ,SIGC1      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('SIGMA_2C'   ,SIGC2      ,IS_AVAILABLE,LSUBMODEL,UNITAB)

      ! 2nd line of the card
      CALL HM_GET_FLOATV    ('ALPHA'      ,ALPHA      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('TAU_MAX'    ,TMAX       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('FCUT'       ,FCUT       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV      ('IFAIL_SH'   ,IFAIL_SH   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV      ('IFAIL_SO'   ,IFAIL_SO   ,IS_AVAILABLE,LSUBMODEL)
      
      ! Check parameters and set defaults value
      IF (SIGT1 == ZERO)  SIGT1  = INFINITY
      IF (SIGT2 == ZERO)  SIGT2  = INFINITY
      IF (SIGC1 == ZERO)  SIGC1  = INFINITY
      IF (SIGC2 == ZERO)  SIGC2  = INFINITY
      IF (TMAX == ZERO)   TMAX   = INFINITY
      IF (SIGT12 == ZERO) SIGT12 = INFINITY
      IFAIL_SH = MIN(IFAIL_SH,2)
      IFAIL_SH = MAX(0,IFAIL_SH)
      IFAIL_SO = MIN(IFAIL_SO,2)
      IFAIL_SO = MAX(0,IFAIL_SO)
      FCUT = MAX(ZERO,FCUT)  
C
      ! Compute Tsai-Wu criterion parameters
      F1  = ONE/SIGT1-(ONE/SIGC1)
      F2  = ONE/SIGT2-(ONE/SIGC2)
      F11 = ONE/(SIGT1*SIGC1)
      F22 = ONE/(SIGT2*SIGC2)
      F66 = ONE/(SIGT12**2)
      F12 = -ALPHA*HALF*SQRT(F11*F22)
C  
      ! Number of parameters and save in UPARAM table
      NUPARAM    = 10
      UPARAM(1)  = F1
      UPARAM(2)  = F2
      UPARAM(3)  = F11
      UPARAM(4)  = F22
      UPARAM(5)  = F66
      UPARAM(6)  = F12
      UPARAM(7)  = TMAX
      UPARAM(8)  = FCUT  
      UPARAM(9)  = IFAIL_SH
      UPARAM(10) = IFAIL_SO
C
      ! Number of user variables and functions
      IF (FCUT > ZERO) THEN 
        NUVAR = 7
      ELSE
        NUVAR = 1
      ENDIF
      NFUNC = 0
C
      ! Print parameter in 0.out file
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1000)
        WRITE(IOUT,1100) SIGT1,SIGT2,SIGC1,SIGC2,SIGT12,ALPHA
        IF (IFAIL_SH > 0 .OR. IFAIL_SO > 0) THEN 
          WRITE(IOUT,1200) TMAX
        ENDIF
        IF (FCUT > ZERO) THEN 
          WRITE(IOUT,1300) FCUT 
        ENDIF 
        WRITE(IOUT,1400) IFAIL_SH
        WRITE(IOUT,1500) IFAIL_SO   
      ENDIF
c----------- 
 1000 FORMAT(
     & 5X,'  ----------------------------------------------------   ',/
     & 5X,'             FAILURE CRITERION : TSAI-WU                 ',/,
     & 5X,'  ----------------------------------------------------   ',/)
 1100 FORMAT(
     & 5X,'TENSILE STRENGTH IN DIRECTION 1 SIGMA_1T  . . . . . . . =',1PG20.13/
     & 5X,'TENSILE STRENGTH IN DIRECTION 2 SIGMA_2T  . . . . . . . =',1PG20.13/
     & 5X,'COMPRESSIVE STRENGTH IN DIRECTION 1 SIGMA_1C  . . . . . =',1PG20.13/
     & 5X,'COMPRESSIVE STRENGTH IN DIRECTION 2 SIGMA_2C  . . . . . =',1PG20.13/
     & 5X,'SHEAR STRENGTH SIGMA_12 . . . . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'INTERACTION PARAMETER ALPHA . . . . . . . . . . . . . . =',1PG20.13/)
 1200 FORMAT(
     & 5X,'STRESS SOFTENING ACTIVATED:                              ',/,
     & 5X,'RELAXATION TIME TAU_MAX . . . . . . . . . . . . . . . . =',1PG20.13/)
 1300 FORMAT(
     & 5X,'STRESS TENSOR FILTERING ACTIVATED:                       ',/,
     & 5X,'FILTERING FREQUENCY FCUT  . . . . . . . . . . . . . . . =',1PG20.13/)
 1400 FORMAT(
     & 5X,'SHELL DELETION FLAG IFAIL_SH  . . . . . . . . . . . . . =',I10,/
     & 5X,'    = 0: SHELL NEVER DELETED AND NO STRESS SOFTENING      ',/,
     & 5X,'    = 1: SHELL DELETED IF DAMAGE IS REACHED FOR ONE LAYER ',/,
     & 5X,'    = 2: SHELL DELETED IF DAMAGE IS REACHED FOR ALL LAYERS',/) 
 1500 FORMAT(
     & 5X,'SOLID DELETION FLAG IFAIL_SO  . . . . . . . . . . . . . =',I10,/
     & 5X,'    = 0: SOLID NEVER DELETED AND NO STRESS SOFTENING                  ',/,
     & 5X,'    = 1: SOLID DELETED IF DAMAGE IS REACHED FOR ONE INTEGRATION POINT ',/,
     & 5X,'    = 2: SOLID DELETED IF DAMAGE IS REACHED FOR ALL INTEGRATION POINTS',/)   
c----------- 
      RETURN
      END
