!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
! ==================================================================================================

      !||====================================================================
      !||    hm_read_mat129_mod   ../starter/source/materials/mat/mat129/hm_read_mat129.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat          ../starter/source/materials/mat/hm_read_mat.F90
      !||====================================================================
      module hm_read_mat129_mod
      contains
  

      !||====================================================================
      !||    hm_read_mat129             ../starter/source/materials/mat/mat129/hm_read_mat129.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat                ../starter/source/materials/mat/hm_read_mat.F90
      !||--- calls      -----------------------------------------------------
      !||    func_table_copy            ../starter/source/materials/tools/func_table_copy.F90
      !||    hm_get_floatv              ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_floatv_dim          ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
      !||    hm_get_intv                ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted     ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||    init_mat_keyword           ../starter/source/materials/mat/init_mat_keyword.F
      !||    mat_table_table_copy       ../starter/source/materials/tools/mat_table_table_copy.F90
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod               ../starter/share/modules1/elbuftag_mod.F
      !||    func_table_copy_mod        ../starter/source/materials/tools/func_table_copy.F90
      !||    mat_table_table_copy_mod   ../starter/source/materials/tools/mat_table_table_copy.F90
      !||    message_mod                ../starter/share/message_module/message_mod.F
      !||    submodel_mod               ../starter/share/modules1/submodel_mod.F
      !||    table_mod                  ../starter/share/modules1/table_mod.F
      !||====================================================================
      subroutine hm_read_mat129(                                       &
                 mat_param,mtag     ,parmat   ,nuvar    ,nvartmp  ,    &
                 ntable   ,table    ,iout     ,unitab   ,lsubmodel)                      

!! \brief read and store input parameters of material law 129

  ! ---------------------------------------------------------------------------------
  !                modules
  ! ---------------------------------------------------------------------------------
      use message_mod
      use elbuftag_mod
      use matparam_def_mod
      use table_mod
      use unitab_mod
      use submodel_mod
      use constant_mod , only : pi,zero,half,three_half,one,two,three,four_over_3
      use constant_mod , only : infinity,em20,em3
      use mat_table_table_copy_mod
      use func_table_copy_mod
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
      implicit none
!-----------------------------------------------
!   included files
! ----------------------------------------------
#include "my_real.inc"
!-----------------------------------------------
!   D u m m y   a r g u m e n t s
!-----------------------------------------------
      integer                     ,intent(in)     :: iout
      integer                     ,intent(in)     :: ntable
      integer                     ,intent(out)    :: nuvar 
      integer                     ,intent(out)    :: nvartmp
      my_real, dimension(128)     ,intent(inout)  :: parmat        
      type(ttable) ,dimension(ntable) ,intent(in) :: table
      type(unit_type_)           ,intent(in)      :: unitab 
      type(matparam_struct_)     ,intent(inout)   :: mat_param
      type(mlaw_tag_)            ,intent(inout)   :: mtag
      type(submodel_data), dimension(nsubmod),intent(in) :: lsubmodel
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      logical :: is_available,is_encrypted
      integer :: ilaw,nfunc,ierror
      integer :: func_sig,func_young,func_nu,func_yld
      integer :: func_qr,func_qx
      integer :: func_cc,func_cp
      integer :: func_a,func_n,func_q,func_m,func_alpha
      integer :: crp_law,sens_id
      my_real :: rho0,young,shear,bulk,nu,sigy
      my_real :: qr1,qr2,qx1,qx2,cr1,cr2,cx1,cx2
      my_real :: crpa,crpn,crpm,crpq,sig_crp,time_crp    
      my_real :: cc,cp,fcut,asrate        
      my_real :: alpha,tref      
      my_real :: kboltz      
      my_real :: epsp_unit,pres_unit,time_unit,energy_unit
      my_real :: x1scale,x2scale,x3scale,x4scale,xfac,yfac
      my_real, dimension(1) :: x2vect,yscale
      integer, dimension(1) :: ifunc        
!-----------------------------------------------
!   S o u r c e   L i n e s 
!===============================================================================    
      is_encrypted = .false.
      is_available = .false.
      ilaw  = 129
!-----------------------------------------------             
      call hm_option_is_encrypted(is_encrypted)
!-----------------------------------------------
!
      ! line1  Density
      call hm_get_floatv('MAT_RHO'     ,rho0     ,is_available, lsubmodel, unitab)
      ! line 2
      call hm_get_floatv('MAT_E'       ,young    ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_NU'      ,nu       ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_SIGY'    ,sigy     ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_ALPHA'   ,alpha    ,is_available, lsubmodel, unitab) !< thermal expansion coef
      call hm_get_floatv('MAT_TREF'    ,tref     ,is_available, lsubmodel, unitab) !< reference temperature
      ! line 3
      call hm_get_intv  ('MAT_f_young' ,func_young,is_available, lsubmodel)     
      call hm_get_intv  ('MAT_f_nu'    ,func_nu   ,is_available, lsubmodel)     
      call hm_get_intv  ('MAT_f_yld'   ,func_yld  ,is_available, lsubmodel)     
      call hm_get_intv  ('MAT_f_alpha' ,func_alpha,is_available, lsubmodel)     
      ! line 4
      call hm_get_intv  ('MAT_ITAB'    ,func_sig ,is_available, lsubmodel)     
      call hm_get_floatv('MAT_FACY'    ,yfac     ,is_available, lsubmodel, unitab)
      ! line 5
      call hm_get_floatv('MAT_QR1'     ,qr1      ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_CR1'     ,cr1      ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_QR2'     ,qr2      ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_CR2'     ,cr2      ,is_available, lsubmodel, unitab)
      call hm_get_intv  ('MAT_f_qr'    ,func_qr  ,is_available, lsubmodel)     
      ! line 6
      call hm_get_floatv('MAT_QX1'     ,qx1      ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_CX1'     ,cx1      ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_QX2'     ,qx2      ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_CX2'     ,cx2      ,is_available, lsubmodel, unitab)
      call hm_get_intv  ('MAT_f_qx'    ,func_qx  ,is_available, lsubmodel)     
      ! line 7 
      call hm_get_floatv('MAT_EPSP0'   ,cc       ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_CP'      ,cp       ,is_available, lsubmodel, unitab)
      call hm_get_intv  ('MAT_f_cc'    ,func_cc  ,is_available, lsubmodel)     
      call hm_get_intv  ('MAT_f_cp'    ,func_cp  ,is_available, lsubmodel)     
      ! line 8 
      call hm_get_floatv('MAT_CRPA'    ,crpa     ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_CRPN'    ,crpn     ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_CRPM'    ,crpm     ,is_available, lsubmodel, unitab)
      call hm_get_intv  ('MAT_fa'      ,func_a   ,is_available, lsubmodel)     
      call hm_get_intv  ('MAT_fn'      ,func_n   ,is_available, lsubmodel)     
      call hm_get_intv  ('MAT_fm'      ,func_m   ,is_available, lsubmodel)     
      call hm_get_intv  ('MAT_CRPL'    ,crp_law  ,is_available, lsubmodel)     
      ! line 9 
      call hm_get_floatv('MAT_CRSIG'   ,sig_crp  ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_CRT'     ,time_crp ,is_available, lsubmodel, unitab)
      call hm_get_floatv('MAT_CRPQ'    ,crpq     ,is_available, lsubmodel, unitab)
      call hm_get_intv  ('MAT_fq'      ,func_q   ,is_available, lsubmodel)     
      call hm_get_intv  ('ISENSOR'     ,sens_id  ,is_available, lsubmodel)     
!---------------------------------------------------------------------------------------
      ! stress and strain rate units
      call hm_get_floatv_dim('MAT_CC'   ,epsp_unit  ,is_available, lsubmodel, unitab)
      call hm_get_floatv_dim('MAT_FACY' ,pres_unit  ,is_available, lsubmodel, unitab)
      call hm_get_floatv_dim('MAT_CRT'  ,time_unit  ,is_available, lsubmodel, unitab)
!---------------------------------------------------------------------------------------
      !  DEFAULT VALUES
!---------------------------------------------------------------------------------------
      shear = young / (two * (one + nu))
      bulk  = young / (three*(one - two*nu))
!
      if (sig_crp  == zero) sig_crp  = young
      if (time_crp == zero) time_crp = time_unit
      if (sigy     == zero) sigy     = infinity
      if (yfac     == zero) yfac     = pres_unit
      if (crp_law  == 0) crp_law     = 2       ! Garfallo steady state law by default
      xfac = one / epsp_unit
      ! Boltzmann constant
      energy_unit = unitab%fac_m_work*unitab%fac_l_work**2/unitab%fac_t_work**2
      kboltz = 1.380649 * em3  / energy_unit          ! eV/K (~J/K)
      kboltz = kboltz   * em20
!
      fcut   = 1.044*unitab%fac_t_work
      asrate = two*pi*fcut
!-------------------------------------
!     sensors are not yet read in lectur() 
!     => conversion of sensor_id to internal numbet is done in updmat()
!-------------------------------------
      nuvar   = 2
      nvartmp = 14
      mat_param%nfunc  = 0
      mat_param%ntable = 13
      allocate (mat_param%table(mat_param%ntable))           ! allocate material table array
!
      mat_param%table(1)%notable  = func_sig
      mat_param%table(2)%notable  = func_young
      mat_param%table(3)%notable  = func_nu
      mat_param%table(4)%notable  = func_yld
      mat_param%table(5)%notable  = func_qr
      mat_param%table(6)%notable  = func_qx
      mat_param%table(7)%notable  = func_cc
      mat_param%table(8)%notable  = func_cp
      mat_param%table(9)%notable  = func_a
      mat_param%table(10)%notable = func_n
      mat_param%table(11)%notable = func_m
      mat_param%table(12)%notable = func_q
      mat_param%table(13)%notable = func_alpha
!-----------------------------------------------------------------------------
      ! create local function table for tabulated yield hardening
!-----------------------------------------------------------------------------
      if (func_sig > 0) then
        x1scale = one
        x2scale = one
        x3scale = one
        x4scale = one
        cc = zero
        cp = zero ! Cowper-Symonds strain rate is not used with tabulated input          
        
        call mat_table_table_copy(                                           &
             mat_param%table(1),func_sig ,mat_param%title,mat_param%mat_id , &
             x1scale  ,x2scale   ,x3scale  ,x4scale  ,                       &
             yfac     ,ntable    ,table    ,ierror     )
      end if
!--------------------------------------------------------------------------
      ! create local function tables for parameter temperature dependencies
!--------------------------------------------------------------------------
      nfunc = 1
      x1scale   = one
      x2scale   = one
      x2vect(:) = zero
!
      if (func_young > 0) then
        ifunc(1)  = func_young
        yscale(1) = young
        call func_table_copy(mat_param%table(2),mat_param%title ,mat_param%mat_id  ,     &
                             nfunc   ,ifunc   ,x2vect  ,x1scale ,x2scale  ,yscale  ,     &
                             ntable  ,table   ,ierror    )
      end if
      if (func_nu > 0) then
        ifunc(1)  = func_nu
        yscale(1) = nu
        call func_table_copy(mat_param%table(3),mat_param%title ,mat_param%mat_id  ,     &
                             nfunc   ,ifunc   ,x2vect  ,x1scale ,x2scale  ,yscale  ,     &
                             ntable  ,table   ,ierror    )
        mat_param%table(3)%notable = func_nu
      end if
      if (func_yld > 0) then
        ifunc(:)  = func_yld
        yscale(:) = nu
        call func_table_copy(mat_param%table(4),mat_param%title ,mat_param%mat_id  ,     &
                             nfunc   ,ifunc   ,x2vect  ,x1scale ,x2scale  ,yscale  ,     &
                             ntable  ,table   ,ierror    )
        mat_param%table(4)%notable = func_yld
      end if
      if (func_qr > 0) then
        ifunc(1)  = func_qr
        yscale(1) = one
        call func_table_copy(mat_param%table(5),mat_param%title ,mat_param%mat_id  ,     &
                             nfunc   ,ifunc   ,x2vect  ,x1scale ,x2scale  ,yscale  ,     &
                             ntable  ,table   ,ierror    )
        mat_param%table(5)%notable = func_qr
      end if
      if (func_qx > 0) then
        ifunc(1)  = func_qx
        yscale(1) = one
        call func_table_copy(mat_param%table(6),mat_param%title ,mat_param%mat_id  ,     &
                             nfunc   ,ifunc   ,x2vect  ,x1scale ,x2scale  ,yscale  ,     &
                             ntable  ,table   ,ierror    )
        mat_param%table(6)%notable = func_qx
      end if
      if (func_cc > 0) then
        ifunc(1)  = func_cc
        yscale(1) = cc
        call func_table_copy(mat_param%table(7),mat_param%title ,mat_param%mat_id  ,     &
                             nfunc   ,ifunc   ,x2vect  ,x1scale ,x2scale  ,yscale  ,     &
                             ntable  ,table   ,ierror    )
        mat_param%table(7)%notable = func_cc
      end if
      if (func_cp > 0) then
        ifunc(1)  = func_cp
        yscale(1) = cp
        call func_table_copy(mat_param%table(8),mat_param%title ,mat_param%mat_id  ,     &
                             nfunc   ,ifunc   ,x2vect  ,x1scale ,x2scale  ,yscale  ,     &
                             ntable  ,table   ,ierror    )
        mat_param%table(8)%notable = func_cp
      end if
      if (func_a > 0) then
        ifunc(1)  = func_a
        yscale(1) = one
        call func_table_copy(mat_param%table(9),mat_param%title ,mat_param%mat_id  ,     &
                             nfunc   ,ifunc   ,x2vect  ,x1scale ,x2scale  ,yscale  ,     &
                             ntable  ,table   ,ierror    )
        mat_param%table(9)%notable = func_a
      end if
      if (func_n > 0) then
        ifunc(1)  = func_n
        yscale(1) = one
        call func_table_copy(mat_param%table(10),mat_param%title ,mat_param%mat_id  ,     &
                             nfunc   ,ifunc   ,x2vect  ,x1scale ,x2scale  ,yscale  ,     &
                             ntable  ,table   ,ierror    )
        mat_param%table(10)%notable = func_n
      end if
      if (func_m > 0) then
        ifunc(1)  = func_m
        yscale(1) = one
        call func_table_copy(mat_param%table(11),mat_param%title,mat_param%mat_id  ,     &
                             nfunc   ,ifunc   ,x2vect  ,x1scale ,x2scale  ,yscale  ,     &
                             ntable  ,table   ,ierror    )
        mat_param%table(11)%notable = func_m
      end if
      if (func_q > 0) then
        ifunc(1)  = func_q
        yscale(1) = one
        call func_table_copy(mat_param%table(12),mat_param%title,mat_param%mat_id  ,     &
                             nfunc   ,ifunc   ,x2vect  ,x1scale ,x2scale  ,yscale  ,     &
                             ntable  ,table   ,ierror    )
        mat_param%table(12)%notable = func_q
      end if
      if (func_alpha > 0) then
        ifunc(1)  = func_alpha
        yscale(1) = alpha
        call func_table_copy(mat_param%table(13),mat_param%title,mat_param%mat_id  ,     &
                             nfunc   ,ifunc   ,x2vect  ,x1scale ,x2scale  ,yscale  ,     &
                             ntable  ,table   ,ierror    )
        mat_param%table(13)%notable = func_alpha
      end if
!-------------------------------------
      mat_param%niparam = 2
      mat_param%nuparam = 20
      allocate (mat_param%iparam(mat_param%niparam))
      allocate (mat_param%uparam(mat_param%nuparam))
!-------------------------------------
      mat_param%iparam(1)  = crp_law 
      mat_param%iparam(2)  = sens_id
!
      mat_param%uparam(1)  = sigy 
      mat_param%uparam(2)  = qr1  
      mat_param%uparam(3)  = qr2  
      mat_param%uparam(4)  = qx1
      mat_param%uparam(5)  = qx2
      mat_param%uparam(6)  = cr1  
      mat_param%uparam(7)  = cr2  
      mat_param%uparam(8)  = cx1
      mat_param%uparam(9)  = cx2
      mat_param%uparam(10) = cc      
      mat_param%uparam(11) = cp       
      mat_param%uparam(12) = alpha   
      mat_param%uparam(13) = tref   
      mat_param%uparam(14) = crpa   
      mat_param%uparam(15) = crpn   
      mat_param%uparam(16) = crpm   
      mat_param%uparam(17) = crpq / kboltz   ! [K]
      mat_param%uparam(18) = sig_crp
      mat_param%uparam(19) = time_crp
      mat_param%uparam(20) = asrate
!-------------------------------------
      ! mat_param common parameters

      mat_param%rho   = rho0
      mat_param%rho0  = rho0
      mat_param%young = young
      mat_param%bulk  = bulk
      mat_param%shear = shear
      mat_param%nu    = nu
!-------------------------------------
      ! PARMAT transfer table

      parmat(1)  = bulk
      parmat(2)  = young
      parmat(3)  = nu
      parmat(16) = 1
      parmat(17) = (one-two*nu)/(one-nu)  !   2G / (bulk + G*4/3)
!-------------------------------------
      ! Element buffer variable activation      

      mtag%g_thk  = 1
      mtag%g_pla  = 1
      mtag%l_pla  = 1
      mtag%g_epsd = 1
      mtag%l_epsd = 1
      mtag%g_temp = 1
      mtag%l_temp = 1
!-------------------------------------
      ! mterial model keywords

      call init_mat_keyword(mat_param,"ELASTO_PLASTIC")       
      call init_mat_keyword(mat_param,"INCREMENTAL")       
      call init_mat_keyword(mat_param,"LARGE_STRAIN")       
      call init_mat_keyword(mat_param,"HOOK")       
      call init_mat_keyword(mat_param,"ISOTROPIC")       

      ! property compatibility  
      call init_mat_keyword(mat_param,"SOLID_ISOTROPIC")       
      call init_mat_keyword(mat_param,"SPH")       
!-------------------------------------------------------------------------------
!     Parameters printout
!-------------------------------------------------------------------------------
      write(iout,1050) trim(mat_param%title),mat_param%mat_id,ilaw
      write(iout,1000)
      if (is_encrypted) then
        write(iout,'(5x,a,//)')'CONFIDENTIAL DATA'
      else
        write(iout,1100) rho0,young,nu,alpha,tref
        if (func_sig > 0) then
          write(iout,1200) func_sig,yfac
        else
          write(iout,1300) qr1,cr1,qr2,cr2,qx1,cx1,qx2,cx2,func_qr,func_qx
        endif       
        write(iout,1400) cc,cp,crp_law,crpa,crpn,crpm,crpq,sig_crp,time_crp
        write(iout,1500) func_young,func_nu,func_yld,func_alpha,             &  
                         func_cc,func_cp,func_a,func_n,func_m,func_q,sens_id    
      endif       
!-------------------------------------------------------------------------------
      return
!-----------
 1000 format(                                                                &
      5x,a,/,                                                                & 
      5x,'     THERMO-ELASTO-VISCOPLASTIC MATERIAL WITH CREEP',/,            & 
      5x,'     ----------------------------------------------',//)           
 1050 format(/                                                               &
      5x,a,/,                                                                &
      5x,'MATERIAL NUMBER . . . . . . . . . . . . .=',i10/,                  &
      5x,'MATERIAL LAW. . . . . . . . . . . . . . .=',i10/)       
 1100 format(                                                                & 
      5x,'INITIAL DENSITY. . . . . . . . . . . . . . . . . . .=',1pg20.13/,  &  
      5x,'YOUNG MODULUS. . . . . . . . . . . . . . . . . . . .=',1pg20.13/,  &
      5x,'POISSON RATIO. . . . . . . . . . . . . . . . . . . .=',1pg20.13/,  &
      5x,'THERMAL EXPANSION COEFFICIENT. . . . . . . . . . . .=',1pg20.13/,  &
      5x,'REFERENCE TEMPERATURE. . . . . . . . . . . . . . . .=',1pg20.13)
 1200 format(                                                                & 
      5x,'TABULATED YIELD STRESS FUNCTION ID . . . . . . . . .=',i10     /   &
      5x,'YIELD STRESS SCALE FACTOR. . . . . . . . . . . . . .=',1pg20.13/)

 1300 format(                                                                & 
      5x,'ISOTROPIC HARDENING PARAMETER QR1. . . . . . . . . .=',1pg20.13/,  &
      5x,'ISOTROPIC HARDENING PARAMETER CR1. . . . . . . . . .=',1pg20.13/,  &
      5x,'ISOTROPIC HARDENING PARAMETER QR2. . . . . . . . . .=',1pg20.13/,  &
      5x,'ISOTROPIC HARDENING PARAMETER CR2. . . . . . . . . .=',1pg20.13/,  &
      5x,'KINEMATIC HARDENING PARAMETER QX1. . . . . . . . . .=',1pg20.13/,  &
      5x,'KINEMATIC HARDENING PARAMETER CX1. . . . . . . . . .=',1pg20.13/,  &
      5x,'KINEMATIC HARDENING PARAMETER QX2. . . . . . . . . .=',1pg20.13/,  &
      5x,'KINEMATIC HARDENING PARAMETER CX2. . . . . . . . . .=',1pg20.13/,  &
      5x,'FUNCTION OF HARDENING PARAMETERS QR VS TEMPERATURE .=',i10     /   &
      5x,'FUNCTION OF HARDENING PARAMETERS QX VS TEMPERATURE .=',i10     /)
 1400 format(                                                                & 
      5x,'COWPER-SYMONDS STRAIN RATE PARAMETER CC. . . . . . .=',1pg20.13/,  &
      5x,'COWPER-SYMONDS STRAIN RATE EXPONENT CP . . . . . . .=',1pg20.13/,  &
      5x,'CREEP LAW SELECTION FLAG . . . . . . . . . . . . . .=',i10     /   &
      5x,'CREEP RATE A . . . . . . . . . . . . . . . . . . . .=',1pg20.13/,  &
      5x,'CREEP EXPONENT N . . . . . . . . . . . . . . . . . .=',1pg20.13/,  &
      5x,'CREEP EXPONENT M . . . . . . . . . . . . . . . . . .=',1pg20.13/,  &
      5x,'CREEP ACTIVATION ENERGY Q. . . . . . . . . . . . . .=',1pg20.13/,  &
      5x,'CREEP REFERENCE STRESS . . . . . . . . . . . . . . .=',1pg20.13/,  &
      5x,'CREEP REFERENCE TIME . . . . . . . . . . . . . . . .=',1pg20.13/)
 1500 format(                                                                & 
      5x,'FUNCTION OF YOUNG MODULUS VS TEMPERATURE . . . . . .=',i10     /   &
      5x,'FUNCTION OF POISSON RATIO VS TEMPERATURE . . . . . .=',i10     /   &
      5x,'FUNCTION OF YIELD STRESS VS TEMPERATURE. . . . . . .=',i10     /   &
      5x,'FUNCTION OF THERMAL EXPANSION COEF VS TEMPERATURE. .=',i10     /   &
      5x,'FUNCTION OF STRAIN RATE PARAMETER CC VS TEMPERATURE.=',i10     /   &
      5x,'FUNCTION OF STRAIN RATE PARAMETER CP VS TEMPERATURE.=',i10     /   &
      5x,'FUNCTION OF CREEP RATE VS TEMPERATURE. . . . . . . .=',i10     /   &
      5x,'FUNCTION OF CREEP EXPONENT N VS TEMPERATURE. . . . .=',i10     /   &
      5x,'FUNCTION OF CREEP EXPONENT M VS TEMPERATURE. . . . .=',i10     /   &
      5x,'FUNCTION OF CREEP ENERGY Q VS TEMPERATURE. . . . . .=',i10     /   &
      5x,'SENSOR ID TO DEACTIVATE CREEP EVOLUTION. . . . . . .=',i10     /)
!----------------------------------
      end subroutine hm_read_mat129
!    
      end module hm_read_mat129_mod
