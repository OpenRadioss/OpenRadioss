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
Chd|  HM_READ_FAIL_HASHIN           source/materials/fail/hashin/hm_read_fail_hashin.F
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
      SUBROUTINE HM_READ_FAIL_HASHIN(
     .           UPARAM   ,MAXUPARAM,NUPARAM  ,NUVAR  ,IFUNC    ,
     .           MAXFUNC  ,NFUNC    ,UNITAB   ,MAT_ID ,FAIL_ID  ,
     .           IFAILWAVE,LSUBMODEL)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ HASHIN FAILURE MODEL
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
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)             ::MAT_ID,FAIL_ID
      TYPE(SUBMODEL_DATA),INTENT(IN) ::LSUBMODEL(*)
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT)          ::IFUNC(MAXFUNC),NFUNC,MAXFUNC,MAXUPARAM
      my_real,INTENT(INOUT)          ::UPARAM(MAXUPARAM)
      INTEGER,INTENT(INOUT)            ::NUPARAM,NUVAR,IFAILWAVE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: ISHELL,IMODEL,ISOLID,DMG_FLAG,TMOD,FRWAVE
      my_real :: SIGT1,SIGT2,SIGT3,SIGT12,SIGC1,SIGC2,SOFT1,SOFT2,SOFT3,
     .    FSIG12,MSIG12,MSIG23,MSIG13,ANGLE,SDELAM,CSIG,
     .    TMAX,RATIO,TCUT,EPSP_REF,EPSP_MIN
      LOGICAL  :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED   = .FALSE.
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
      CALL HM_GET_INTV   ('Ifail_sh'    ,ISHELL    ,IS_AVAILABLE,LSUBMODEL)
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
      IF (FRWAVE > 1)  IFAILWAVE = 1
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
      IF (ISHELL == 0) ISHELL = 1
      IF (ISOLID == 0) ISOLID = 1 
      IF (RATIO == ZERO) RATIO = ONE
      IF (DMG_FLAG == 0) DMG_FLAG = 1  ! DMG_FLAG = 1    => damage reduces forces after criteria is met
      IF (DMG_FLAG == 2) DMG_FLAG = 0  ! DMG_FLAG = 2->0 => damage reduces saved stress (old method)
      SOFT1 = MIN(ONE,MAX(ZERO,SOFT1))
c--------------------------------------------------
      UPARAM(1)  = IMODEL
      UPARAM(2)  = SIGT1
      UPARAM(3)  = SIGT2
      UPARAM(4)  = SIGT3
      UPARAM(5)  = SIGC1
      UPARAM(6)  = SIGC2
      UPARAM(7)  = CSIG
      UPARAM(8)  = FSIG12
      UPARAM(9)  = MSIG12
      UPARAM(10) = MSIG13
      UPARAM(11) = MSIG23
      UPARAM(12) = ANGLE*PI/HUNDRED80   ! degre ---> radian 
      UPARAM(13) = SDELAM
      UPARAM(14) = TMAX
      UPARAM(15) = ISHELL
      UPARAM(16) = ISOLID
      UPARAM(17) = RATIO
      UPARAM(18) = DMG_FLAG
      UPARAM(19) = TMOD
      UPARAM(20) = EPSP_REF 
      UPARAM(21) = TCUT 
      UPARAM(22) = EPSP_MIN 
      UPARAM(23) = FRWAVE 
      UPARAM(24) = SOFT1 
      UPARAM(25) = SOFT2 
      UPARAM(26) = SOFT3 
c-----------------------------------------------------
      NUPARAM = 26
      NFUNC   = 0
      NUVAR   = 12
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
     & 5X,'Longitudinal tensile strength. . . . . . . . =',E12.4/
     & 5X,'Transversal tensile strength . . . . . . . . =',E12.4/
     & 5X,'Through thickness strength.  . . . . . . . . =',E12.4/
     & 5X,'Longitudinal compressive strength. . . . . . =',E12.4/
     & 5X,'Transversal compressive  strength. . . . . . =',E12.4/
     & 5X,'Crush  strength    . . . . . . . . . . . . . =',E12.4/
     & 5X,'Fiber Mode shear strength  . . . . . . . . . =',E12.4/
     & 5X,'Matrix mode shear strength (plane 12 ) . . . =',E12.4/
     & 5X,'Matrix mode shear strength (plane 23 ) . . . =',E12.4/
     & 5X,'Matrix mode shear strength (plane 13 ) . . . =',E12.4/
     & 5X,'Coulomb friction angle and delamination' /,
     & 5X, '                              failure < 90°  =',E12.4/
     & 5X,'Scale factor for delamination criteria . . . =',E12.4/     
     & 5X,'Material failure model: . .  . . . . . . . . =',I10 /
     & 15X,          '1: Unidirectional layer model'  ,/
     & 15X,          '2: Fabric layer model        '  ,/
     & 5X,'Failed layer ratio for element deletion . . .=',E12.4 /
     & 5X,'Relaxation time . . . . . .  : . .  . . . . .=',E12.4 /
     & 5X,'Relaxation time calculation  : . .  . . . . .=',I10 /
     & 15X,          '1     : Based on timestep'  ,/
     & 15X,           'Other value : Constant   ' ,/
     & 5X,'Ref strain rate. . . . . . . . . . . . . . . =',E12.4/     
     & 5X,'Strain rate cutoff period. . . . . . . . . . =',E12.4/      
     & 5X,'Low strain rate limit . . .. . . . . . . . . =',E12.4/
     & 5X,'Front Wave :  . . . .  . . . . . . . . . . . =',I10 /
     & 15X,          '1: No frontwave propagation        ',/
     & 15X,          '2: Isotropic propagation           ',/)
 1200 FORMAT(
     & 5X,'Softening factor . . . .  . . . . . . . . . .=',E12.4//)
c-----------
      END
