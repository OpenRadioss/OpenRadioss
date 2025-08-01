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
!||    hm_read_fail_hashin      ../starter/source/materials/fail/hashin/hm_read_fail_hashin.F
!||--- called by ------------------------------------------------------
!||    hm_read_fail             ../starter/source/materials/fail/hm_read_fail.F
!||--- calls      -----------------------------------------------------
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_FAIL_HASHIN(FAIL    , 
     .           MAT_ID   ,FAIL_ID  ,IRUPT   ,FRWAVE    ,
     .           LSUBMODEL,UNITAB   ,FAIL_TAG)
C-----------------------------------------------
c    ROUTINE DESCRIPTION :
c    Read Hashin failure model parameters
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
      INTEGER            ,INTENT(IN) :: FAIL_ID       !< failure model ID
      INTEGER            ,INTENT(IN) :: MAT_ID        !< material law ID
      INTEGER            ,INTENT(IN) :: IRUPT         !< failure model type number
      TYPE(UNIT_TYPE_)   ,INTENT(IN) :: UNITAB        !< table of input units
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*)  !< submodel table
      INTEGER            ,INTENT(INOUT) :: FRWAVE     !< failure model type number
      TYPE(FAIL_PARAM_)  ,INTENT(INOUT) :: FAIL       !< failure model data structure
      TYPE (FAIL_TAG_)   ,INTENT(INOUT) :: FAIL_TAG   !< failure model tag for buffer allocation
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IFAIL_SH,IMODEL,ISOLID,DMG_FLAG,TMOD
      my_real :: SIGT1,SIGT2,SIGT3,SIGT12,SIGC1,SIGC2,SOFT1,SOFT2,SOFT3,
     .    FSIG12,MSIG12,MSIG23,MSIG13,ANGLE,SDELAM,CSIG,PTHKF,
     .    TMAX,RATIO,TCUT,EPSP_REF,EPSP_MIN
      LOGICAL  :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
c
      EPSP_MIN    = ZERO
      SOFT1 = ONE
      SOFT2 = ONE
      SOFT3 = ONE
C--------------------------------------------------
C     check crypting
C--------------------------------------------------
c
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c
C--------------------------------------------------
C     Read DATA
C--------------------------------------------------
      CALL HM_GET_INTV   ('Iform'       ,IMODEL    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV   ('Ifail_sh'    ,IFAIL_SH    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV   ('Ifail_so'    ,ISOLID    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('ratio'       ,RATIO     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('I_Dam'       ,DMG_FLAG  ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV   ('Imod'        ,TMOD      ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV   ('I_frwave'    ,FRWAVE    ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV ('Epsp_min'    ,EPSP_MIN  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c
      CALL HM_GET_FLOATV ('Sigma_1t'    ,SIGT1     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Sigma_2t'    ,SIGT2     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Sigma_3t'    ,SIGT3     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Sigma_1c'    ,SIGC1     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Sigma_2c'    ,SIGC2     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c
      CALL HM_GET_FLOATV ('Sigma_c'     ,CSIG      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Sigma_12f'   ,FSIG12    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Sigma_12m'   ,MSIG12    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Sigma_23m'   ,MSIG23    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Sigma_13m'   ,MSIG13    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c
      CALL HM_GET_FLOATV ('Phi'         ,ANGLE     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Sdel'        ,SDELAM    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Tau_max'     ,TMAX      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Epsp_ref'    ,EPSP_REF    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Tcut'        ,TCUT      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c
      CALL HM_GET_FLOATV ('Soft'        ,SOFT1     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c--------------------------------------------------
c     Default values
c--------------------------------------------------
      IF (FRWAVE == 0) FRWAVE = 1
      IF (FRWAVE > 1)  FRWAVE = 1
      IF (SIGT1  == ZERO) SIGT1  = EP20
      IF (SIGT2  == ZERO) SIGT2  = EP20
      IF (SIGT3  == ZERO) SIGT3  = EP20
      IF (SIGC1  == ZERO) SIGC1  = EP20
      IF (SIGC2  == ZERO) SIGC2  = EP20
      IF (CSIG   == ZERO) CSIG   = EP20
      IF (FSIG12 == ZERO) FSIG12 = EP20
      IF (MSIG12 == ZERO) MSIG12 = EP20
      IF (MSIG13 == ZERO) MSIG13 = EP20
      IF (MSIG23 == ZERO) MSIG23 = EP20
      IF (TMAX == ZERO)   TMAX   = EP20
      IF (TCUT == ZERO)   TCUT   = TMAX
      IF (EPSP_REF == ZERO) EPSP_REF = EM20
      IF (SDELAM == ZERO) SDELAM = ONE
      IF (IMODEL == 0) IMODEL = 1
      IF (IFAIL_SH == 0) IFAIL_SH = 1
      IF (ISOLID == 0) ISOLID = 1 
      IF (RATIO == ZERO) RATIO = ONE - EM06
      RATIO = MIN(RATIO, ONE)
      RATIO = MAX(RATIO,-ONE)
      IF (DMG_FLAG == 0) DMG_FLAG = 1  ! DMG_FLAG = 1    => damage reduces forces after criteria is met
      IF (DMG_FLAG == 2) DMG_FLAG = 0  ! DMG_FLAG = 2->0 => damage reduces saved stress (old method)
      SOFT1 = MIN(ONE,MAX(ZERO,SOFT1))
c
      IF (IFAIL_SH == 1) THEN
        PTHKF = EM06
      ELSEIF (IFAIL_SH == 2) THEN
        PTHKF = RATIO
      ELSE   !  IFAIL_SH = 3
        PTHKF = ONE - EM06
      ENDIF
c--------------------------------------------------
      FAIL%KEYWORD = 'HASHIN' 
      FAIL%IRUPT   = IRUPT 
      FAIL%FAIL_ID = FAIL_ID 
      FAIL%NUPARAM = 26
      FAIL%NIPARAM = 0
      FAIL%NUVAR   = 12
      FAIL%NFUNC   = 0
      FAIL%NTABLE  = 0
      FAIL%PTHK    = PTHKF
c            
      ALLOCATE (FAIL%UPARAM(FAIL%NUPARAM))
      ALLOCATE (FAIL%IPARAM(FAIL%NIPARAM))
      ALLOCATE (FAIL%IFUNC (FAIL%NFUNC))
      ALLOCATE (FAIL%TABLE (FAIL%NTABLE))
c
      ! Modes of failure
      ! -> Uni-directional lamina model 
      IF (IMODEL == 1) THEN 
        FAIL%NMOD = 5
        ALLOCATE (FAIL%MODE(FAIL%NMOD))
        FAIL%MODE(1) = "Tensile/Shear fiber"
        FAIL%MODE(2) = "Compression fiber"
        FAIL%MODE(3) = "Crush"
        FAIL%MODE(4) = "Matrix failure"
        FAIL%MODE(5) = "Delamination"
      ! -> Fabric lamina model 
      ELSE 
        FAIL%NMOD = 7
        ALLOCATE (FAIL%MODE(FAIL%NMOD))
        FAIL%MODE(1) = "Tensile/Shear fiber 1"
        FAIL%MODE(2) = "Tensile/Shear fiber 2"
        FAIL%MODE(3) = "Compression fiber 1"
        FAIL%MODE(4) = "Compression fiber 2"
        FAIL%MODE(5) = "Crush"
        FAIL%MODE(6) = "Shear failure matrix"
        FAIL%MODE(7) = "Matrix failure"
      ENDIF
      FAIL_TAG%LF_DAMMX = FAIL_TAG%LF_DAMMX + FAIL%NMOD
c
      FAIL%UPARAM(1)  = IMODEL
      FAIL%UPARAM(2)  = SIGT1
      FAIL%UPARAM(3)  = SIGT2
      FAIL%UPARAM(4)  = SIGT3
      FAIL%UPARAM(5)  = SIGC1
      FAIL%UPARAM(6)  = SIGC2
      FAIL%UPARAM(7)  = CSIG
      FAIL%UPARAM(8)  = FSIG12
      FAIL%UPARAM(9)  = MSIG12
      FAIL%UPARAM(10) = MSIG13
      FAIL%UPARAM(11) = MSIG23
      FAIL%UPARAM(12) = ANGLE*PI/HUNDRED80   ! degre ---> radian 
      FAIL%UPARAM(13) = SDELAM
      FAIL%UPARAM(14) = TMAX
      FAIL%UPARAM(15) = IFAIL_SH
      FAIL%UPARAM(16) = ISOLID
      FAIL%UPARAM(17) = RATIO
      FAIL%UPARAM(18) = DMG_FLAG
      FAIL%UPARAM(19) = TMOD
      FAIL%UPARAM(20) = EPSP_REF 
      FAIL%UPARAM(21) = TCUT 
      FAIL%UPARAM(22) = EPSP_MIN 
      FAIL%UPARAM(23) = 0         !not used 
      FAIL%UPARAM(24) = SOFT1 
      FAIL%UPARAM(25) = SOFT2 
      FAIL%UPARAM(26) = SOFT3 
c-----------------------------------------------------
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT, 1000)
      ELSE
         WRITE(IOUT, 1100) MAT_ID,FAIL_ID,SIGT1,SIGT2,SIGT3,SIGC1,SIGC2,
     .                  CSIG,FSIG12,MSIG12,MSIG23,MSIG13,
     .                  ANGLE,SDELAM, IMODEL,RATIO,TMAX, 
     .                  TMOD,EPSP_REF,TCUT,EPSP_MIN,FRWAVE
        IF (FRWAVE > 1) WRITE(IOUT, 1200) SOFT1
      ENDIF
c-----------
      RETURN
c-------------------------------------------------- 
 1000 FORMAT(
     & 5X,'    CRYPTED DATA IN FAILURE MODEL            '/,
     & 5X,'    -----------------------------            '/)
 1100 FORMAT(
     & 5X,'    HASHIN FAILURE MODEL                      ',/
     & 5X,'    ------------------------                  ',/
     & 5X,'    MAT_ID  =',I10/
     & 5X,'    FAIL_ID =',I10/
     & 5X,'Longitudinal tensile strength. . . . . . . . =',1PG20.13/
     & 5X,'Transversal tensile strength . . . . . . . . =',1PG20.13/
     & 5X,'Through thickness strength.  . . . . . . . . =',1PG20.13/
     & 5X,'Longitudinal compressive strength. . . . . . =',1PG20.13/
     & 5X,'Transversal compressive  strength. . . . . . =',1PG20.13/
     & 5X,'Crush  strength    . . . . . . . . . . . . . =',1PG20.13/
     & 5X,'Fiber Mode shear strength  . . . . . . . . . =',1PG20.13/
     & 5X,'Matrix mode shear strength (plane 12 ) . . . =',1PG20.13/
     & 5X,'Matrix mode shear strength (plane 23 ) . . . =',1PG20.13/
     & 5X,'Matrix mode shear strength (plane 13 ) . . . =',1PG20.13/
     & 5X,'Coulomb friction angle and delamination' /,
     & 5X, '                              failure < 90° =',1PG20.13/
     & 5X,'Scale factor for delamination criteria . . . =',1PG20.13/     
     & 5X,'Material failure model: . .  . . . . . . . . =',I10 /
     & 15X,          '1: Unidirectional layer model'  ,/
     & 15X,          '2: Fabric layer model        '  ,/
     & 5X,'Element deletion parameter ratio. . . . . . .=',1PG20.13,/,
     & 5X,'  > 0.0 : Fraction of failed thickness             ',/,
     & 5X,'  < 0.0 : Fraction of failed intg. points or layers',/,
     & 5X,'Relaxation time . . . . . .  : . .  . . . . .=',1PG20.13 /
     & 5X,'Relaxation time calculation  : . .  . . . . .=',I10 /
     & 15X,          '1     : Based on timestep'  ,/
     & 15X,           'Other value : Constant   ' ,/
     & 5X,'Ref strain rate. . . . . . . . . . . . . . . =',1PG20.13/     
     & 5X,'Strain rate cutoff period. . . . . . . . . . =',1PG20.13/      
     & 5X,'Low strain rate limit . . .. . . . . . . . . =',1PG20.13/
     & 5X,'Front Wave :  . . . .  . . . . . . . . . . . =',I10 /
     & 15X,          '1: No frontwave propagation        ',/
     & 15X,          '2: Isotropic propagation           ',/)
 1200 FORMAT(
     & 5X,'Softening factor . . . .  . . . . . . . . . .=',1PG20.13//)
c-----------
      END
