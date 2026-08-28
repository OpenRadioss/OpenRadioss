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
!||    hm_read_fail_snconnect   ../starter/source/materials/fail/snconnect/hm_read_fail_snconnect.F
!||--- called by ------------------------------------------------------
!||    hm_read_fail             ../starter/source/materials/fail/hm_read_fail.F
!||--- calls      -----------------------------------------------------
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_floatv_dim        ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_FAIL_SNCONNECT(
     .           FAIL     ,TITLE    ,MAT_ID   ,FAIL_ID  ,IRUPT    ,
     .           NTABLE   ,TABLE    ,LSUBMODEL,UNITAB   )
C-----------------------------------------------
c    ROUTINE DESCRIPTION :
c    Read SNCONNECT cohesive failure model parameters
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FAIL_PARAM_MOD
      USE UNITAB_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD 
      use table_mod
      use func_table_copy_mod
      use names_and_titles_mod , only : nchartitle
      use my_alloc_mod
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
      integer            ,intent(in) :: fail_id       ! failure model id
      integer            ,intent(in) :: mat_id        ! material law id
      integer            ,intent(in) :: irupt         ! failure model type number
      integer            ,intent(in)    :: ntable     !< number of function tables in radioss input
      type(ttable) ,dimension(ntable) ,intent(in) :: table  !< input table array
      type(unit_type_)   ,intent(in) :: unitab        !< table of input units
      type(submodel_data),intent(in) :: lsubmodel(*)  !< submodel table
      type(fail_param_)  ,intent(inout) :: fail       !< failure model data structure
      character(len=nchartitle) ,intent(in) :: title
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      integer :: i,id_0n,id_0s,id_fn,id_fs,isolid,nfail,isym,ierror
      my_real :: a2, b2 , a3, b3,xscale0,xscalef,areascale,xscale0_unit,xscalef_unit
      integer :: nf
      integer ,parameter :: nfunc = 4
      integer ,dimension(nfunc) :: ifunc
      my_real :: x2scale
      my_real ,dimension(nfunc) :: x1scale,yscale
      my_real, dimension(1) :: x2vect
c-----------------------------------------------
      logical :: is_available,is_encrypted
C=======================================================================
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C    (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C     EXTRACT DATA 
C--------------------------------------------------
Card1
      CALL HM_GET_FLOATV  ('Alpha_0' ,A2    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV  ('Beta_0'  ,B2    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV  ('Alpha_f' ,A3    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV  ('Beta_f'  ,B3    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV    ('Ifail_so',ISOLID,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV    ('ISYM'    ,ISYM  ,IS_AVAILABLE,LSUBMODEL)
Card2     
      CALL HM_GET_INTV    ('fct_IDON'  ,ID_0N     ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV    ('fct_IDOS'  ,ID_0S     ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV    ('fct_IDFN'  ,ID_FN     ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV    ('fct_IDFS'  ,ID_FS     ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV  ('XSCALE_0'  ,XSCALE0   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV  ('XSCALE_F'  ,XSCALEF   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV  ('AREA_scale',AREASCALE ,IS_AVAILABLE,LSUBMODEL,UNITAB)
C-------------------------------------------- 
      IF (XSCALE0 == ZERO) THEN 
         CALL HM_GET_FLOATV_DIM('XSCALE0' ,XSCALE0_UNIT  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
         XSCALE0 = ONE * XSCALE0_UNIT
      ENDIF

      IF (XSCALEF == ZERO)THEN 
         CALL HM_GET_FLOATV_DIM('XSCALEF' ,XSCALEF_UNIT  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
         XSCALEF = ONE * XSCALEF_UNIT
      ENDIF 
C-------------------------------------------- 
      NFAIL  = 1
      IF (ISOLID == 0) THEN
        ISOLID = 1
      ELSE IF (ISOLID == 2) THEN
        NFAIL = 4                ! all Gauss points must fail
      END IF  
      IF (B2 == ZERO) B2 = ONE
      IF (B3 == ZERO) B3 = ONE
c-----------------------------------------------------
      FAIL%KEYWORD = 'SNCONNECT' 
      FAIL%IRUPT   = IRUPT 
      FAIL%FAIL_ID = FAIL_ID 
      FAIL%NUPARAM = 5
      FAIL%NIPARAM = 2
      FAIL%NUVAR   = 3
      FAIL%NVARTMP = nfunc
      FAIL%NFUNC   = 0
      FAIL%NTABLE  = 0
      FAIL%NTABLE4D= nfunc
      FAIL%NMOD    = 0
c            
      call my_alloc(FAIL%UPARAM, FAIL%NUPARAM, "FAIL%UPARAM")
      call my_alloc(FAIL%IPARAM, FAIL%NIPARAM, "FAIL%IPARAM")
      call my_alloc(FAIL%IFUNC, FAIL%NFUNC, "FAIL%IFUNC")
      call my_alloc(FAIL%TABLE, FAIL%NTABLE, "FAIL%TABLE")
      allocate(FAIL%TABLE4D(FAIL%NTABLE4D))
c
      FAIL%iparam(1) = ISYM
      FAIL%iparam(2) = NFAIL
!
      FAIL%UPARAM(1) = A2
      FAIL%UPARAM(2) = B2
      FAIL%UPARAM(3) = A3
      FAIL%UPARAM(4) = B3
      FAIL%UPARAM(5) = AREASCALE
c    
      IFUNC(1)  = ID_0N
      IFUNC(2)  = ID_0S
      IFUNC(3)  = ID_FN
      IFUNC(4)  = ID_FS
!
      nf = 1
      x1scale(1:2) = xscale0
      x1scale(3:4) = xscalef
      x2scale      = one
      x2vect(:)    = zero
      yscale(:)    = one
      do i = 1,nfunc
        call func_table_copy(fail%table4d(i),title  ,mat_id   ,      
     .           nf      ,ifunc(i),x2vect  ,x1scale(i) ,x2scale  ,yscale(i)  ,      
     .           ntable  ,table   ,ierror  )
      end do
c-----------------------------------------------------------------------
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT, 1000)
      ELSE
        WRITE(IOUT, 1100) A2,B2,A3,B3,ISOLID,ISYM,
     .                    ID_0N,ID_0S,ID_FN,ID_FS,XSCALE0,XSCALEF                  
      ENDIF
C-----------
      RETURN
C-----------
 1000 FORMAT(
     & 5X,'    CRYPTED DATA IN FAILURE MODEL        '/,
     & 5X,'    -----------------------------        '/)
 1100 FORMAT(
     & 5X,'    SNCONNECT                            '/,
     & 5X,'    ------------------------             '/,
     & 5X,'CONSTANT ALPHA INITIAL . . . . . . . . .=',1PG20.13/,
     & 5X,'CONSTANT BETA INITIAL. . . . . . . . . .=',1PG20.13/,
     & 5X,'CONSTANT ALPHA FINAL . . . . . . . . . .=',1PG20.13/,
     & 5X,'CONSTANT BETA FINAL . .. . . . . . . . .=',1PG20.13/,
     & 5X,'ISOLID FLAG  . . . . . . . . . . . . . .=',I10/,
     & 5X,'RUPTURE DEACTIVATION FLAG IN COMPRESSION=',I10/,
     & 5X,'FUCNCTION ID INITIAL NORMAL. . . . . . .=',I10/,
     & 5X,'FUCNCTION ID INITIAL TANGENTIAL  . . . .=',I10/,
     & 5X,'FUCNCTION ID FINAL NORMAL .  . . . . . .=',I10/,
     & 5X,'FUCNCTION ID FINAL TANGENTIAL  . . . . .=',I10/,
     & 5X,'ABSCISSA SCALE FOR INITIAL CURVES  . . .=',1PG20.13/,
     & 5X,'ABSCISSA SCALE FOR FINAL CURVES  . . . .=',1PG20.13)
C-----------       
      RETURN
      END
