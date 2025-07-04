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
      !||    hm_read_mat128_mod   ../starter/source/materials/mat/mat128/hm_read_mat128.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat          ../starter/source/materials/mat/hm_read_mat.F90
      !||====================================================================
      module hm_read_mat128_mod
        implicit none
      contains


      !||====================================================================
      !||    hm_read_mat128           ../starter/source/materials/mat/mat128/hm_read_mat128.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
      !||--- calls      -----------------------------------------------------
      !||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_floatv_dim        ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
      !||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
      !||    mat_table_copy           ../starter/source/materials/tools/mat_table_copy.F90
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
      !||    mat_table_copy_mod       ../starter/source/materials/tools/mat_table_copy.F90
      !||    submodel_mod             ../starter/share/modules1/submodel_mod.F
      !||    table_mod                ../starter/share/modules1/table_mod.F
      !||====================================================================
        subroutine hm_read_mat128(                                       &
          mat_param,mtag     ,parmat   ,nuvar    ,nvartmp  ,    &
          ntable   ,table    ,mat_id   ,titr     ,iout     ,    &
          unitab   ,lsubmodel)

!! \brief read and store input parameters of material law 128

          ! ---------------------------------------------------------------------------------
          !                modules
          ! ---------------------------------------------------------------------------------
          use elbuftag_mod
          use matparam_def_mod
          use table_mod
          use unitab_mod
          use submodel_mod
          use constant_mod , only : pi,zero,half,fourth,three_half,one,two,three,four_over_3
          use constant_mod , only : infinity,ep20
          use mat_table_copy_mod
          use precision_mod, only : WP
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
          implicit none
!-----------------------------------------------
!   D u m m y   a r g u m e n t s
!-----------------------------------------------
          integer                     ,intent(in)     :: mat_id
          integer                     ,intent(in)     :: iout
          integer                     ,intent(in)     :: ntable
          integer                     ,intent(out)    :: nuvar
          integer                     ,intent(out)    :: nvartmp
          character(len=nchartitle)   ,intent(in)     :: titr
          real(kind=WP), dimension(128)     ,intent(inout)  :: parmat
          type(ttable) ,dimension(ntable) ,intent(in) :: table
          type(unit_type_)           ,intent(in)      :: unitab
          type(matparam_struct_)     ,intent(inout)   :: mat_param
          type(mlaw_tag_)            ,intent(inout)   :: mtag
          type(submodel_data), dimension(nsubmod),intent(in) :: lsubmodel
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          logical :: is_available,is_encrypted
          integer :: ilaw,func_id,ndim,ierr
          real(kind=WP) :: rho0,young,shear,bulk,nu
          real(kind=WP) :: qr1,qr2,qx1,qx2,cr1,cr2,cx1,cx2
          real(kind=WP) :: r00,r45,r90,rr
          real(kind=WP) :: epsp_ref,cc,cp
          real(kind=WP) :: sigy,lam,cii,cij
          real(kind=WP) :: ff,gg,hh,ll,mm,nn
          real(kind=WP) :: fcut,asrate,xfac,yfac,fisokin
          real(kind=WP) :: epsp_unit,pres_unit
          real(kind=WP) :: x1scale,x2scale,x3scale,x4scale
          real(kind=WP) :: x2vect(1),x3vect(1),x4vect(1),fscale(1)
!-----------------------------------------------
!   S o u r c e   L i n e s
!===============================================================================
          is_encrypted = .false.
          is_available = .false.
          ilaw  = 128
!-----------------------------------------------
          call hm_option_is_encrypted(is_encrypted)
!-----------------------------------------------
!
          ! line1  Density
          call hm_get_floatv('MAT_RHO'        ,rho0    ,is_available, lsubmodel, unitab)
          ! line 2
          call hm_get_floatv('LAW128_E'       ,young   ,is_available, lsubmodel, unitab)
          call hm_get_floatv('LAW128_NU'      ,nu      ,is_available, lsubmodel, unitab)
          call hm_get_floatv('LAW128_SIGY'    ,sigy    ,is_available, lsubmodel, unitab)
          call hm_get_floatv('LAW128_KIN'     ,fisokin ,is_available, lsubmodel, unitab)
          ! line 3
          call hm_get_intv  ('LAW128_ITAB'    ,func_id ,is_available, lsubmodel)
          call hm_get_floatv('LAW128_FACY'    ,yfac    ,is_available, lsubmodel, unitab)
          call hm_get_floatv('LAW128_FACX'    ,xfac    ,is_available, lsubmodel, unitab)
          ! line 4
          call hm_get_floatv('LAW128_QR1'     ,qr1     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('LAW128_CR1'     ,cr1     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('LAW128_QR2'     ,qr2     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('LAW128_CR2'     ,cr2     ,is_available, lsubmodel, unitab)
          ! line 5
          call hm_get_floatv('LAW128_QX1'     ,qx1     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('LAW128_CX1'     ,cx1     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('LAW128_QX2'     ,qx2     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('LAW128_CX2'     ,cx2     ,is_available, lsubmodel, unitab)
          ! line 6
          call hm_get_floatv('LAW128_EPSP0'   ,epsp_ref,is_available, lsubmodel, unitab)
          call hm_get_floatv('LAW128_CP'      ,cp      ,is_available, lsubmodel, unitab)
          ! line 7
          call hm_get_floatv('LAW128_R00'     ,r00     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('LAW128_R45'     ,r45     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('LAW128_R90'     ,r90     ,is_available, lsubmodel, unitab)

          ! line 8
          call hm_get_floatv('LAW128_F'       ,ff     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('LAW128_G'       ,gg     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('LAW128_H'       ,hh     ,is_available, lsubmodel, unitab)

          !line 9
          call hm_get_floatv('LAW128_L'       ,ll     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('LAW128_M'       ,mm     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('LAW128_N'       ,nn     ,is_available, lsubmodel, unitab)
!
          ! stress and strain rate units
          call hm_get_floatv_dim('LAW128_FACX' ,epsp_unit  ,is_available, lsubmodel, unitab)
          call hm_get_floatv_dim('LAW128_FACY' ,pres_unit  ,is_available, lsubmodel, unitab)
!---------------------------------------------------------------------------------------
          !  DEFAULT VALUES
!---------------------------------------------------------------------------------------
          if (r00 == zero .or. r90 == zero) then   ! use Hill parameters
            if (ff   == zero) ff   = half
            if (gg   == zero) gg   = half
            if (hh   == zero) hh   = half
            if (ll   == zero) ll   = three_half
            if (mm   == zero) mm   = three_half
            if (nn   == zero) nn   = three_half
          else                                     ! use Lankford coefficients
            rr = (r00 + r45*two + r90) * fourth
            hh = rr / (one + rr)
            gg = hh / r00
            ff = hh / r90
            nn = (ff + gg) * (r45 + half)
          end if
          if (sigy == zero) sigy = infinity
!
          shear = young / (two * (one + nu))
          bulk  = young / (three*(one - two*nu))
          lam   = young*nu / (one+nu) / (one - two*nu)
          cii   = lam + shear*two
          cij   = lam
!
          if (yfac == zero) yfac = one * pres_unit
          if (xfac == zero) xfac = one * epsp_unit
          xfac = one / xfac
!
          fcut   = 1.044*unitab%fac_t_work
          asrate = two*pi*fcut
!
!-------------------------------------
          nuvar   = 1
          nvartmp = 0
          mat_param%nfunc  = 0
          mat_param%ntable = 0
!-------------------------------------
          ! create local function table in case of tabulated yield function input
!-------------------------------------
          ndim = 0
          if (func_id > 0) then
            allocate (mat_param%table(1))           ! allocate material table array
            mat_param%ntable  = 1
            mat_param%table(1)%notable = func_id
            x1scale   = one
            x2scale   = one
            x3scale   = one
            x4scale   = one
            x2vect(1) = xfac
            x3vect(1) = one
            x4vect(1) = one
            fscale(1) = yfac
            call mat_table_copy(mat_param,x2vect    ,x3vect   ,x4vect   ,          &
              x1scale  ,x2scale   ,x3scale  ,x4scale  ,          &
              fscale   ,ntable    ,table    ,ierr     )
            if (ierr == 0) then
              cc = zero
              cp = zero ! Cowper-Symonds strain rate is not used with tabulated input
              nvartmp = mat_param%table(1)%ndim
              if (mat_param%table(1)%ndim == 1) then
                sigy = mat_param%table(1)%y1d(1)    !< initial yield stress from hardening function
              end if
            endif
          endif
!
          ! reference (static) strain rate
          if (func_id > 0 .and. ndim == 2) then
            cc = mat_param%table(1)%x(2)%values(1)
          else if (epsp_ref > zero) then
            cc = epsp_ref
          else
            cc = zero
          end if
!
!-------------------------------------
          mat_param%niparam = 0
          mat_param%nuparam = 19
          allocate (mat_param%iparam(mat_param%niparam))
          allocate (mat_param%uparam(mat_param%nuparam))
!-------------------------------------
          mat_param%uparam(1)  = sigy
          mat_param%uparam(2)  = qr1
          mat_param%uparam(3)  = cr1
          mat_param%uparam(4)  = qr2
          mat_param%uparam(5)  = cr2
          mat_param%uparam(6)  = qx1
          mat_param%uparam(7)  = cx1
          mat_param%uparam(8)  = qx2
          mat_param%uparam(9)  = cx2
          mat_param%uparam(10) = cc
          mat_param%uparam(11) = cp
          mat_param%uparam(12) = ff
          mat_param%uparam(13) = gg
          mat_param%uparam(14) = hh
          mat_param%uparam(15) = ll
          mat_param%uparam(16) = mm
          mat_param%uparam(17) = nn
          mat_param%uparam(18) = asrate
          mat_param%uparam(19) = fisokin
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
          !< activate allocation of state variables in element buffer

          mtag%g_thk  = 1
          mtag%g_pla  = 1
          mtag%l_pla  = 1
          mtag%g_seq  = 1
          mtag%l_seq  = 1
          mtag%l_epsd = 1
          !< allocate backstresses in case of kinematic hardening
          if (fisokin > zero) then
            mtag%l_sigb = 6
          endif
!-------------------------------------
          !< material model keywords

          call init_mat_keyword(mat_param,"ELASTO_PLASTIC")
          call init_mat_keyword(mat_param,"INCREMENTAL")
          call init_mat_keyword(mat_param,"LARGE_STRAIN")
          call init_mat_keyword(mat_param,"HOOK")
          call init_mat_keyword(mat_param,"ORTHOTROPIC")

          !< compatibility flags with element properties
          call init_mat_keyword(mat_param,"SHELL_ORTHOTROPIC")
          call init_mat_keyword(mat_param,"SOLID_ORTHOTROPIC")
          call init_mat_keyword(mat_param,"SPH")
!-------------------------------------------------------------------------------
!     Parameters printout
!-------------------------------------------------------------------------------
          write(iout,1100) trim(titr),mat_id,ilaw
          write(iout,1000)
          if (is_encrypted) then
            write(iout,'(5x,a,//)')'CONFIDENTIAL DATA'
          else
            write(iout,1200) rho0,young,nu,fisokin
            if (func_id > 0) then
              write(iout,1300) func_id,yfac,xfac
            else
              write(iout,1400) qr1,cr1,qr2,cr2,qx1,cx1,qx2,cx2,cc,cp
            endif
            write(iout,1500) ff,gg,hh,ll,mm,nn
          endif
!-------------------------------------------------------------------------------
          return
!-----------
1000      format(                                                                &
            5x,a,/,                                                                &
            5x,40h  ELASTOPLASTIC ORTHOTROPIC HILL MATERIAL,/,                     &
            5x,40h  -----------------------------------   ,//)
1100      format(/                                                               &
            5x,a,/,                                                                &
            5x,'MATERIAL NUMBER . . . . . . . . . . . . .=',i10/,                  &
            5x,'MATERIAL LAW. . . . . . . . . . . . . . .=',i10/)
1200      format(                                                                &
            5x,'INITIAL DENSITY. . . . . . . . . . . . . . . . . . .=',1pg20.13/,  &
            5x,'YOUNG MODULUS. . . . . . . . . . . . . . . . . . . .=',1pg20.13/,  &
            5x,'POISSON RATIO. . . . . . . . . . . . . . . . . . . .=',1pg20.13/,  &
            5x,'KINEMATIC HARDENING FACTOR . . . . . . . . . . . . .=',1pg20.13/)
1300      format(                                                                &
            5x,'TABULATED YIELD STRESS FUNCTION ID . . . . . . . . .=',i10     /,  &
            5x,'SCALE FACTOR FOR STRESS FUNCTION . . . . . . . . . .=',1pg20.13/,  &
            5x,'SCALE FACTOR FOR STRAIN RATE . . . . . . . . . . . .=',1pg20.13/)
1400      format(                                                                &
            5x,'ISOTROPIC HARDENING PARAMETER QR1. . . . . . . . . .=',1pg20.13/,  &
            5x,'ISOTROPIC HARDENING PARAMETER CR1. . . . . . . . . .=',1pg20.13/,  &
            5x,'ISOTROPIC HARDENING PARAMETER QR2. . . . . . . . . .=',1pg20.13/,  &
            5x,'ISOTROPIC HARDENING PARAMETER CR2. . . . . . . . . .=',1pg20.13/,  &
            5x,'KINEMATIC HARDENING PARAMETER QX1. . . . . . . . . .=',1pg20.13/,  &
            5x,'KINEMATIC HARDENING PARAMETER CX1. . . . . . . . . .=',1pg20.13/,  &
            5x,'KINEMATIC HARDENING PARAMETER QX2. . . . . . . . . .=',1pg20.13/,  &
            5x,'KINEMATIC HARDENING PARAMETER CX2. . . . . . . . . .=',1pg20.13/,  &
            5x,'COWPER-SYMONDS STRAIN RATE PARAMETER CC. . . . . . .=',1pg20.13/,  &
            5x,'COWPER-SYMONDS STRAIN RATE EXPONENT CP . . . . . . .=',1pg20.13/)
1500      format(                                                                &
            5x,'YIELD PARAMETER F. . . . . . . . . . . . . . . . . .=',1pg20.13/,  &
            5x,'YIELD PARAMETER G. . . . . . . . . . . . . . . . . .=',1pg20.13/,  &
            5x,'YIELD PARAMETER H. . . . . . . . . . . . . . . . . .=',1pg20.13/,  &
            5x,'YIELD PARAMETER L. . . . . . . . . . . . . . . . . .=',1pg20.13/,  &
            5x,'YIELD PARAMETER M. . . . . . . . . . . . . . . . . .=',1pg20.13/,  &
            5x,'YIELD PARAMETER N. . . . . . . . . . . . . . . . . .=',1pg20.13/)
!----------------------------------
        end subroutine hm_read_mat128
!
      end module hm_read_mat128_mod
