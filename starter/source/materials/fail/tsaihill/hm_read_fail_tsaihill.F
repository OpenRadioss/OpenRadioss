Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    hm_read_fail_tsaihill    ../starter/source/materials/fail/tsaihill/hm_read_fail_tsaihill.F
!||--- called by ------------------------------------------------------
!||    hm_read_fail             ../starter/source/materials/fail/hm_read_fail.F
!||--- calls      -----------------------------------------------------
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_FAIL_TSAIHILL(FAIL   ,
     .           MAT_ID   ,FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB   ,
     .           FAIL_TAG )
C-----------------------------------------------
c    ROUTINE DESCRIPTION :
c    Read Tsai-Hill failure model parameters
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAIL_PARAM_MOD
      USE UNITAB_MOD
      USE MESSAGE_MOD 
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD 
      USE ELBUFTAG_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER             ,INTENT(IN) :: FAIL_ID       !< failure model ID
      INTEGER             ,INTENT(IN) :: MAT_ID        !< material law ID
      INTEGER             ,INTENT(IN) :: IRUPT         !< failure model number
      TYPE (UNIT_TYPE_)   ,INTENT(IN) :: UNITAB        !< table of input units
      TYPE (SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)  !< submodel table 
      TYPE (FAIL_PARAM_)  ,INTENT(INOUT) :: FAIL       !< failure model data structure
      TYPE (FAIL_TAG_)    ,INTENT(INOUT) :: FAIL_TAG   !< failure model tag for buffer allocation
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IFAIL_SH,IFAIL_SO,NUVAR
      my_real :: X11,X22,S12,TMAX,FCUT,PTHKF
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C--------------------------------------------------
C   S o u r c e   L i n e s
C--------------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      ! 1st line of the card
      CALL HM_GET_FLOATV('X11'      ,X11      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('X22'      ,X22      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('S12'      ,S12      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV  ('IFAIL_SH' ,IFAIL_SH ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('IFAIL_SO' ,IFAIL_SO ,IS_AVAILABLE,LSUBMODEL)

      ! 2nd line of the card
      CALL HM_GET_FLOATV('TAU_MAX'  ,TMAX     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('FCUT'     ,FCUT     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      
      ! Check parameters and set defaults value
      IF (X11 == ZERO)  X11  = INFINITY
      IF (X22 == ZERO)  X22  = INFINITY
      IF (S12 == ZERO)  S12  = INFINITY
      IF (TMAX == ZERO) TMAX = INFINITY
      IFAIL_SH = MIN(IFAIL_SH,2)
      IFAIL_SH = MAX(0,IFAIL_SH)
      IFAIL_SO = MIN(IFAIL_SO,2)
      IFAIL_SO = MAX(0,IFAIL_SO)
      FCUT = MAX(ZERO,FCUT)  
c
      ! Shell element deletion flag
      IF (IFAIL_SH == 1) THEN
        PTHKF = EM06
      ELSEIF (IFAIL_SH == 2) THEN 
        PTHKF = ONE
      ENDIF
c---------------------------
      IF (FCUT > ZERO) THEN 
        NUVAR = 7
      ELSE
        NUVAR = 1
      ENDIF
c---------------------------
      FAIL%KEYWORD = 'TSAI-HILL' 
      FAIL%IRUPT   = IRUPT 
      FAIL%FAIL_ID = FAIL_ID 
      FAIL%NUPARAM = 7
      FAIL%NIPARAM = 0
      FAIL%NUVAR   = NUVAR
      FAIL%NFUNC   = 0
      FAIL%NTABLE  = 0
      FAIL%NMOD    = 1
      FAIL%PTHK    = PTHKF
c            
      ALLOCATE (FAIL%UPARAM(FAIL%NUPARAM))
      ALLOCATE (FAIL%IPARAM(FAIL%NIPARAM))
      ALLOCATE (FAIL%IFUNC (FAIL%NFUNC))
      ALLOCATE (FAIL%TABLE (FAIL%NTABLE))
c
      ! Modes of failure
      FAIL_TAG%LF_DAMMX = FAIL_TAG%LF_DAMMX + FAIL%NMOD
      ALLOCATE (FAIL%MODE(FAIL%NMOD))
      FAIL%MODE(1) = "Reserve factor"
c
      FAIL%NUPARAM   = 7
      FAIL%UPARAM(1) = X11
      FAIL%UPARAM(2) = X22
      FAIL%UPARAM(3) = S12
      FAIL%UPARAM(4) = TMAX
      FAIL%UPARAM(5) = FCUT  
      FAIL%UPARAM(6) = IFAIL_SH
      FAIL%UPARAM(7) = IFAIL_SO
c---------------------------
C
      ! Print parameter in 0.out file
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1000)
        WRITE(IOUT,1100) X11,X22,S12
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
     & 5X,'            FAILURE CRITERION : TSAI-HILL                ',/,
     & 5X,'  ----------------------------------------------------   ',/)
 1100 FORMAT(
     & 5X,'CRITICAL STRENGTH IN DIRECTION 1 X11  . . . . . . . . . =',1PG20.13/
     & 5X,'CRITICAL STRENGTH IN DIRECTION 2 X22  . . . . . . . . . =',1PG20.13/
     & 5X,'CRITICAL SHEAR STRENGTH S12 . . . . . . . . . . . . . . =',1PG20.13/)
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
