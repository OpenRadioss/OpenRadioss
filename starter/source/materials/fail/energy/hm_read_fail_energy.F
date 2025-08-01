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
!||    hm_read_fail_energy      ../starter/source/materials/fail/energy/hm_read_fail_energy.F
!||--- called by ------------------------------------------------------
!||    hm_read_fail             ../starter/source/materials/fail/hm_read_fail.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_floatv_dim        ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_FAIL_ENERGY(FAIL , 
     .           MAT_ID   ,FAIL_ID  ,IRUPT    ,LSUBMODEL,UNITAB   )
C-----------------------------------------------
c    ROUTINE DESCRIPTION :
c    Read Energy based failure model parameters
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAIL_PARAM_MOD
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
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER             ,INTENT(IN) :: FAIL_ID       ! failure model ID
      INTEGER             ,INTENT(IN) :: MAT_ID        ! material law ID
      INTEGER             ,INTENT(IN) :: IRUPT         ! failure model number
      TYPE (UNIT_TYPE_)   ,INTENT(IN) :: UNITAB        ! table of input units
      TYPE (SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*) 
      TYPE (FAIL_PARAM_)  ,INTENT(INOUT) :: FAIL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  :: IRFUN,DMG_FLAG,FAILIP 
      my_real  :: RF1,RF2,RIEF1,RIEF2,SCALE_EPSP,PTHK
C----------------------------------------
       LOGICAL :: IS_AVAILABLE,IS_ENCRYPTED
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C=======================================================================
c     ENERGY Rupture Criterion
C=======================================================================
Card1
      CALL HM_GET_FLOATV         ('E1'         ,RIEF1      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV         ('E2'         ,RIEF2      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('fct_ID'     ,IRFUN      ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV         ('xscale'     ,SCALE_EPSP ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV           ('I_Dam'      ,DMG_FLAG   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV           ('FAILIP'       ,FAILIP  ,IS_AVAILABLE,LSUBMODEL) 
      IF (FAILIP == 0)  FAILIP  = 1
C=======================================================================
      IF (RIEF1 <= ZERO) RIEF1 = EP30
      IF (RIEF2 <= ZERO) RIEF2 = TWO*EP30      
      IF (DMG_FLAG == 0) DMG_FLAG = 1  ! DMG_FLAG = 1    => damage reduces forces after criteria is met
      IF (DMG_FLAG == 2) DMG_FLAG = 0  ! DMG_FLAG = 2->0 => damage reduces saved stress (old method)
C=======================================================================
      IF (RIEF1 > RIEF2) THEN
        CALL ANCMSG(MSGID=618, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,
     .              I1=MAT_ID)
      ENDIF 
C
      PTHK = EM06
      IF(SCALE_EPSP == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('xscale' ,SCALE_EPSP   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        SCALE_EPSP = ONE * SCALE_EPSP
      ENDIF
c---------------------------
      FAIL%KEYWORD = 'ENERGY' 
      FAIL%IRUPT   = IRUPT 
      FAIL%FAIL_ID = FAIL_ID 
      FAIL%NUPARAM = 6
      FAIL%NIPARAM = 0
      FAIL%NUVAR   = 1
      FAIL%NFUNC   = 1
      FAIL%NTABLE  = 0
      FAIL%NMOD    = 0
c            
      FAIL%PTHK    = PTHK
c            
      ALLOCATE (FAIL%UPARAM(FAIL%NUPARAM))
      ALLOCATE (FAIL%IPARAM(FAIL%NIPARAM))
      ALLOCATE (FAIL%IFUNC (FAIL%NFUNC))
      ALLOCATE (FAIL%TABLE (FAIL%NTABLE))

      FAIL%UPARAM(1) = RIEF1
      FAIL%UPARAM(2) = RIEF2
      FAIL%UPARAM(3) = PTHK
      FAIL%UPARAM(4) = ONE/SCALE_EPSP
      FAIL%UPARAM(5) = DMG_FLAG   
      FAIL%UPARAM(6)= FAILIP
C
      FAIL%IFUNC(1)  = IRFUN
C=============================================  
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT, 1000)
      ELSE       
       WRITE(IOUT, 2000) RIEF1,RIEF2,IRFUN,SCALE_EPSP,FAILIP
      ENDIF
      RETURN
C=============================================  
 1000 FORMAT(
     & 5X,40H    CRYPTED DATA IN FAILURE MODEL             /,
     & 5X,40H    -----------------------------             /)
 2000 FORMAT(
     & 5X,40H    ENERGY   FAILURE MODEL                  /,
     & 5X,40H    ------------------------                  /,
     & 5X,'MAXIMUM SPECIFIC ENERGY 1. . . . . . . =',E12.4/
     & 5X,'MAXIMUM SPECIFIC ENERGY 2. . . . . . . =',E12.4/
     & 5X,'MAXIMUM ENERGIES SCALING FUNCTION. . . =',I8/
     & 5X,'ABSCISSA SCALE FACTOR FOR FUNCTION . . =',E12.4/,
     & 5X,'NUMBER OF FAILED INTG. POINTS PRIOR TO ELEM DELETION  .=',I10/)      
c----------- 
      RETURN
      END
