Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    func_table_copy          ../starter/source/materials/tools/func_table_copy.F90
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_floatv_dim        ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||--- uses       -----------------------------------------------------
!||    func_table_copy_mod      ../starter/source/materials/tools/func_table_copy.F90
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||    table_mod                ../starter/share/modules1/table_mod.F
!||====================================================================
      SUBROUTINE HM_READ_FAIL_ENERGY(FAIL    ,TITLE    ,MAT_ID   ,FAIL_ID  ,IRUPT    ,
     .                               NTABLE  ,TABLE    ,LSUBMODEL,UNITAB   )
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
      use table_mod
      use func_table_copy_mod
      use names_and_titles_mod , only : nchartitle
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
      INTEGER             ,INTENT(IN) :: MAT_ID        ! material model ID
      INTEGER             ,INTENT(IN) :: FAIL_ID       ! failure model ID
      INTEGER             ,INTENT(IN) :: IRUPT         ! failure model number
      TYPE (UNIT_TYPE_)   ,INTENT(IN) :: UNITAB        ! table of input units
      TYPE (SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(*) 
      TYPE (FAIL_PARAM_)  ,INTENT(INOUT) :: FAIL
      integer             ,intent(in)    :: ntable     !< number of function tables in Radioss input
      type(ttable) ,dimension(ntable) ,intent(in)    :: table      !< input table array
      character(len=nchartitle) :: title
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  :: IRFUN,DMG_FLAG,FAILIP,IERROR 
      my_real  :: RIEF1,RIEF2,SCALE_EPSP,PTHK
      integer  :: niparam,nuparam
      integer  :: nfunc
      integer, dimension(1) :: func_id
      my_real :: x1scale,x2scale
      my_real, dimension(1) :: x2vect,yscale
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
      CALL HM_GET_INTV           ('FAILIP'     ,FAILIP     ,IS_AVAILABLE,LSUBMODEL) 
      IF (FAILIP == 0)  FAILIP  = 1
C=======================================================================
      IF (RIEF1 <= ZERO) RIEF1 = EP20
      IF (RIEF2 <= ZERO) RIEF2 = TWO*EP20      
      IF (DMG_FLAG == 0) DMG_FLAG = 1  ! DMG_FLAG = 1    => damage reduces forces after criteria is met
      IF (DMG_FLAG == 2) DMG_FLAG = 0  ! DMG_FLAG = 2->0 => damage reduces saved stress (old method)
C=======================================================================
      IF (RIEF1 > RIEF2) THEN
        CALL ANCMSG(MSGID=618, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,  I1=MAT_ID)
      ENDIF 
C
      PTHK = EM06
      IF(SCALE_EPSP == ZERO) THEN
        CALL HM_GET_FLOATV_DIM('xscale' ,SCALE_EPSP   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        SCALE_EPSP = ONE * SCALE_EPSP
      ENDIF
c---------------------------
      NIPARAM = 1
      NUPARAM = 3
      FAIL%KEYWORD = 'ENERGY' 
      FAIL%IRUPT   = IRUPT 
      FAIL%FAIL_ID = FAIL_ID 
      FAIL%NIPARAM = NIPARAM
      FAIL%NUPARAM = NUPARAM
      FAIL%NUVAR   = 1
      FAIL%NVARTMP = 1
      FAIL%NTABLE4D= 1
      FAIL%NTABLE  = 0
      FAIL%NFUNC   = 0
      FAIL%NMOD    = 0
      FAIL%PTHK    = PTHK
      FAIL%FAIL_IP = FAILIP
c            
      ALLOCATE (FAIL%UPARAM(NUPARAM))
      ALLOCATE (FAIL%IPARAM(NIPARAM))
      ALLOCATE (FAIL%IFUNC(FAIL%NFUNC))
      ALLOCATE (FAIL%TABLE(FAIL%NTABLE))
      ALLOCATE (FAIL%TABLE4D(FAIL%NTABLE4D))

      FAIL%IPARAM(1) = DMG_FLAG   
!
      FAIL%UPARAM(1) = RIEF1
      FAIL%UPARAM(2) = RIEF2
      FAIL%UPARAM(3) = PTHK
! ----------------------------------------------------------------------------------------------------------------------
      ! create local function table using input function ID
      nfunc = 1
      func_id(1) = irfun
      x1scale    = scale_epsp
      x2scale    = one
      x2vect(:)  = zero
      yscale(1)  = one
      call func_table_copy(fail%table4d(1) ,title   ,mat_id   ,      
     .           nfunc   ,func_id ,x2vect  ,x1scale ,x2scale  ,yscale  ,      
     .           ntable  ,table   ,ierror  )
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
