Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    hm_read_fail_fabric      ../starter/source/materials/fail/fabric/hm_read_fail_fabric.F
!||--- called by ------------------------------------------------------
!||    hm_read_fail             ../starter/source/materials/fail/hm_read_fail.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_FAIL_FABRIC(FAIL     ,
     .           TITLE    ,MAT_ID   ,FAIL_ID  ,IRUPT    ,UNIT_ID ,
     .           NTABLE   ,TABLE    ,LSUBMODEL,UNITAB   )
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ ANISOTROPIC FABRIC FAILURE MODEL
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAIL_PARAM_MOD
      USE UNITAB_MOD
      USE MESSAGE_MOD 
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD 
      use my_alloc_mod
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
      integer             ,intent(in) :: fail_id            !< failure model id
      integer             ,intent(in) :: mat_id             !< material law id
      integer             ,intent(in) :: irupt              !< failure model number
      integer             ,intent(in) :: unit_id            !< 
      type (unit_type_)   ,intent(in) :: unitab             !< table of input units
      type (submodel_data),intent(in) :: lsubmodel(*)       !< submodel table 
      type (fail_param_)  ,intent(inout) :: fail            !< failure model data structure
      integer             ,intent(in)    :: ntable          !< number of function tables in Radioss input
      type(ttable) ,dimension(ntable) ,intent(in) :: table  !< input table array
      character(len=nchartitle),intent(in)  :: title        !< material model title
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      integer  :: func_id,ndir,nf,ierror
      integer ,dimension(1) :: ifunc
      my_real  :: epsf1,epsr1,epsf2,epsr2,fac_t
      my_real  :: x1scale,x2scale
      my_real ,dimension(1) :: yscale,x2vect
      logical  :: is_available,is_encrypted
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
c
      IF (UNIT_ID > 0) THEN
        FAC_T = UNITAB%FAC_T(UNIT_ID)
      ELSE
        FAC_T = ONE
      ENDIF     
C--------------------------------------------------
C     check crypting
C--------------------------------------------------
c
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c
C--------------------------------------------------
C     Read DATA
C--------------------------------------------------
      CALL HM_GET_FLOATV ('Epsilon_f1'  ,EPSF1     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Epsilon_r1'  ,EPSR1     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Epsilon_f2'  ,EPSF2     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV ('Epsilon_r2'  ,EPSR2     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV   ('NDIR'        ,NDIR      ,IS_AVAILABLE,LSUBMODEL)
c
      CALL HM_GET_INTV   ('fct_ID'      ,FUNC_ID   ,IS_AVAILABLE,LSUBMODEL)
c--------------------------------------------------
c     Default values
c--------------------------------------------------
      IF (EPSF1 <= ZERO) EPSF1 = EP20
      IF (EPSR1 <= ZERO) EPSR1 = EP20*TWO
      IF (EPSF2 <= ZERO) EPSF2 = EP20
      IF (EPSR2 <= ZERO) EPSR2 = EP20*TWO
      IF (NDIR == 0) NDIR = 2
      NDIR = MIN(NDIR,2)
      NDIR = MAX(NDIR,1)
c          
      IF (EPSF1 > EPSR1 .or. EPSF2 > EPSR2) THEN
        CALL ANCMSG(MSGID=617,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=MAT_ID)
      ENDIF     
c--------------------------------------------------
      fail%keyword = 'fabric' 
      fail%irupt   = irupt 
      fail%fail_id = fail_id 
      fail%nuparam = 4
      fail%niparam = 1
      fail%nuvar   = 2
      fail%nvartmp = 2
      fail%nfunc   = 0
      fail%ntable  = 0
      fail%ntable4d= 1
      fail%nmod    = 0
c            
      call my_alloc(fail%uparam, fail%nuparam, "fail%uparam")
      call my_alloc(fail%iparam, fail%niparam, "fail%iparam")
      call my_alloc(fail%ifunc, fail%nfunc,    "fail%ifunc")
      call my_alloc(fail%table, fail%ntable,   "fail%table")
      if (allocated(fail%table4d)) deallocate(fail%table4d )
      allocate(fail%table4d(fail%ntable4d))
c
      fail%iparam(1) = ndir
      fail%uparam(1) = epsf1 
      fail%uparam(2) = epsr1
      fail%uparam(3) = epsf2
      fail%uparam(4) = epsr2
! ----------------------------------------------------------------------------------------------------------------------
      ! create local function table using input function ID
      nf = 1
      x1scale   = fac_t
      x2scale   = one
      x2vect(:) = zero
      yscale(:) = one
      ifunc(1)  = func_id
      call func_table_copy(fail%table4d(1) ,title   ,mat_id   ,      
     .           nf      ,ifunc(1),x2vect  ,x1scale ,x2scale  ,yscale  ,      
     .           ntable  ,table   ,ierror  )
c-----------------------------------------------------
      IF (IS_ENCRYPTED)THEN
        WRITE(IOUT, 1000)
      ELSE
        WRITE(IOUT, 1100) EPSF1,EPSR1,EPSF2,EPSR2,NDIR,FUNC_ID
      ENDIF
c-----------
      RETURN
c-------------------------------------------------- 
 1000 FORMAT(
     & 5X,'    CRYPTED DATA IN FAILURE MODEL             '/,
     & 5X,'    -----------------------------             '/)
 1100 FORMAT(
     & 5X,'    ANISOTROPIC FABRIC FAILURE MODEL          ',/
     & 5X,'    ------------------------                  ',/
     & 5X,'FAILURE TENSION STRAIN DIRECTION 1 . . =',E12.4/
     & 5X,'RUPTURE TENSION STRAIN DIRECTION 1 . . =',E12.4/
     & 5X,'FAILURE TENSION STRAIN DIRECTION 2 . . =',E12.4/
     & 5X,'RUPTURE TENSION STRAIN DIRECTION 2 . . =',E12.4/
     & 5X,'NUMBER OF BROKEN DIR. FOR EL. DELETION =',I10/
     & 5X,'STRAIN RATE SCALING FUNCTION . . . . . =',I10)
c-------------------------------------------------- 
      END
