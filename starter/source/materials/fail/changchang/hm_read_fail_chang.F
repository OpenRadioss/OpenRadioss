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
Chd|  HM_READ_FAIL_CHANG            source/materials/fail/changchang/hm_read_fail_chang.F
Chd|-- called by -----------
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        FAIL_PARAM_MOD                ../common_source/modules/mat_elem/fail_param_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_FAIL_CHANG(FAIL , 
     .           MAT_ID   ,FAIL_ID  ,IRUPT    ,
     .           LSUBMODEL,UNITAB   ,FAIL_TAG )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ FAILURE CHANG
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAIL_PARAM_MOD
      USE UNITAB_MOD
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
      INTEGER :: IFAIL_SH,FAILIP
      my_real :: SIGT1,SIGT2,SIGT12,SIGC1,SIGC2,BETA,TMAX,PTHKF
      LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C--------------------------------------------------
C   S o u r c e   L i n e s
C--------------------------------------------------
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)

      CALL HM_GET_FLOATV    ('Sigma_1t'   ,SIGT1      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('Sigma_2t'   ,SIGT2      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('Sigma_12'   ,SIGT12     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('Sigma_1c'   ,SIGC1      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('Sigma_2c'   ,SIGC2      ,IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV    ('Beta'       ,BETA       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV    ('Tau_max'    ,TMAX       ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV      ('Ifail_sh'   ,IFAIL_SH     ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV      ('Failip'     ,FAILIP     ,IS_AVAILABLE,LSUBMODEL)
c---------------------------
            
      IF (SIGT1  == ZERO) SIGT1  = INFINITY
      IF (SIGT2  == ZERO) SIGT2  = INFINITY
      IF (SIGT12 == ZERO) SIGT12 = INFINITY
      IF (SIGC1  == ZERO) SIGC1  = INFINITY
      IF (SIGC2  == ZERO) SIGC2  = INFINITY
      IF (TMAX   == ZERO) TMAX   = INFINITY
      IF (IFAIL_SH == 0)    IFAIL_SH = 1    
      IF (FAILIP == 0)    FAILIP = 1           
c
      IF (IFAIL_SH == 1) THEN        ! matrix or fiber failure
        PTHKF = EM06
      ELSEIF (IFAIL_SH == 2) THEN    ! matrix or fiber failure
        PTHKF = ONE
      ELSEIF (IFAIL_SH == 3) THEN    ! fiber only failure
        PTHKF = EM06
      ELSEIF (IFAIL_SH == 4) THEN    ! fiber only failure
        PTHKF = ONE
      END IF                  
c---------------------------
      FAIL%KEYWORD = 'CHANG' 
      FAIL%IRUPT   = IRUPT 
      FAIL%FAIL_ID = FAIL_ID 
      FAIL%NUPARAM = 9
      FAIL%NIPARAM = 0
      FAIL%NUVAR   = 1
      FAIL%NFUNC   = 0
      FAIL%NTABLE  = 0
      FAIL%NMOD    = 4
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
      FAIL%MODE(1) = "Failure index tension dir. 1"
      FAIL%MODE(2) = "Failure index compression dir. 1"
      FAIL%MODE(3) = "Failure index tension dir. 2"
      FAIL%MODE(4) = "Failure index compression dir. 2"
c
      FAIL%UPARAM(1) = SIGT1
      FAIL%UPARAM(2) = SIGT2
      FAIL%UPARAM(3) = SIGT12
      FAIL%UPARAM(4) = SIGC1
      FAIL%UPARAM(5) = SIGC2
      FAIL%UPARAM(6) = BETA
      FAIL%UPARAM(7) = TMAX
      FAIL%UPARAM(8) = IFAIL_SH
      FAIL%UPARAM(9) = FAILIP
c---------------------------
      IF(IS_ENCRYPTED)THEN
      
      ELSE
       WRITE(IOUT, 1000)SIGT1,SIGT2,SIGT12,SIGC1,SIGC2,BETA,TMAX    
       IF(IFAIL_SH==1)THEN
        WRITE(IOUT, 1100)
       ELSEIF(IFAIL_SH==2)THEN
        WRITE(IOUT, 1200)
       ENDIF   
       WRITE(IOUT,1300) FAILIP    
      ENDIF
c----------- 
 1000 FORMAT(
     & 5X,'---------------------------------------------------------',/,
     & 5X,'              CHANG CHANG FAILURE MODEL                  ',/,
     & 5X,'---------------------------------------------------------',/,
     & 5X,'LONGITUDINAL TENSILE STRENGTH SIGMA1_T . . . . . . . . .=',1PG20.13/
     & 5X,'TRANSVERSAL  TENSILE STRENGTH SIGMA2_T . . . . . . . . .=',1PG20.13/
     & 5X,'SHEAR STRENGTH SIGMA_12. . . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'LONGITUDINAL COMPRESSIVE STRENGTH SIGMA1_C . . . . . . .=',1PG20.13/
     & 5X,'TRANSVERSAL  COMPRESSIVE STRENGTH SIGMA2_C . . . . . . .=',1PG20.13/
     & 5X,'SHEAR SCALING FACTOR BETA. . . . . . . . . . . . . . . .=',1PG20.13/
     & 5X,'RELAXATION TIME TAU_MAX. . . . . . . . . . . . . . . . .=',1PG20.13/)  
 1100 FORMAT(
     & 5X,'SHELL IS DELETED IF CRITERIA IS REACHED FOR ONE LAYER    ') 
 1200 FORMAT(
     & 5X,'SHELL TENSOR STRESS IS SET TO ZERO AND SHELL IS DELETED  ',/  
     & 5X,'IF CRITERIA IS REACHED FOR ALL LAYERS' )     
 1300 FORMAT(
     & 5X,'NUMBER OF FAILED INTG. POINT PRIOR TO SOLID DELETION . .=',I5/)  
c----------- 
      RETURN
      END
