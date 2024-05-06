!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
!chd|====================================================================
!chd|  read_mat25_tsaiwu                 source/materials/mat/mat025/read_mat25_tsaiwu.F90
!chd|-- called by -----------
!chd|        hm_read_mat25               source/materials/mat/mat025/hm_read_mat25.F
!chd|-- calls ---------------
!chd|====================================================================

      module read_mat25_tsaiwu_mod
      contains

! ========================================================================================
! \brief read config file for material law25 with Tsai-Wu formulation
!! \details

! ========================================================================================

        subroutine read_mat25_tsaiwu(                           &
          mat_param ,parmat   ,unitab   ,lsubmodel,      &
          mat_id   ,titr     ,pm       ,israte   ,      &
          iout     ,npropm   )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
          use matparam_def_mod
          use unitab_mod
          use message_mod
          use submodel_mod
          use constant_mod ,only : half,one,zero,two,four,pi,em3,em20,ep20
          use constant_mod ,only : onep1,onep2,zep999,four,six_over_5,five_over_6
! ---------------------------------------------------------------------------------------------
          implicit none
! ---------------------------------------------------------------------------------------------
!     included files
! ---------------------------------------------------------------------------------------------

#include "my_real.inc"

!-----------------------------------------------
!     d u m m y   a r g u m e n t s
!-----------------------------------------------
          integer                                ,intent(in)    :: mat_id      !< material law ID
          integer                                ,intent(in)    :: iout        !< output file
          integer                                ,intent(in)    :: npropm      !< size of PM table
          type (unit_type_)                      ,intent(in)    :: unitab      !< Radioss input units structure
          type(submodel_data), dimension(nsubmod),intent(in)    :: lsubmodel   !< Radioss submodel structure
          character(len=nchartitle)              ,intent(in)    :: titr        !< material law title
!
          integer                                ,intent(inout) :: israte      !< strain rate flag
          my_real, dimension(100)                ,intent(inout) :: parmat      !< temporary material parameter table
          my_real, dimension(npropm)             ,intent(inout) :: pm          !< material parameter table
          type(matparam_struct_)                 ,intent(inout) :: mat_param   !< material parameter structure
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
          logical :: is_available,is_encrypted
          integer :: ioff,icc,iflag
          my_real :: rho0,rhor,e11,e22,e33,g12,g23,g31,young,gmax,nu,n12,n21,   &
            cb,cn,epst1,epst2,asrate,                                           &
            fmax, sigyt1, sigyt2, sigyc1, sigyc2, sigyt12, sigyc12,             &
            c1, ssp, f1, f2, f11, f22, f33, f12, ft1, wplamx,                   &
            epsm1, epsm2, dmax,dmx2,shrdam, shrmax, shrdmax,alpha,cc,epdr,      &
            cbt1,cnt1,sigmxt1,cct1,cbt2,cnt2,sigmxt2,cct2,detc,                 &
            cbc1,cnc1,sigmxc1,ccc1,cbc2,cnc2,sigmxc2,ccc2,fcut,                 &
            cbt12,cnt12,sigmxt12,cct12,cbc12,cnc12,sigmxc12,ccc12,              &
            eps1t1,eps2t1,sigrst1,wplamxt1,eps1t2,eps2t2,sigrst2,               &
            wplamxt2,eps1c1,eps2c1,sigrsc1,wplamxc1,eps1c2,eps2c2,              &
            sigrsc2,wplamxc2,eps1t12,eps2t12,sigrst12,wplamxt12,wplaref,        &
            epsf1,epsf2,ratio,d11,d22,d12,dmin,fac,fscal_unit
          ! -------------------------
          ! simple precision issue
          real(kind=8) :: sigyt1_db,sigyc1_db,sigyt2_db,sigyc2_db,sigyt12_db,sigyc12_db
          real(kind=8) :: f1_db,f2_db,f11_db,f22_db,f33_db,f12_db,ft1_db
          ! -------------------------
!=======================================================================
          is_encrypted = .false.
          is_available = .false.
          iflag = 0      ! Tsai-Wu formulation
!--------------------------------------------------------
!
          call hm_option_is_encrypted(is_encrypted)
!
!--------------------------------------------------------
!     read input fields
!--------------------------------------------------------
          call hm_get_floatv('MAT_RHO'   ,rho0     ,is_available, lsubmodel, unitab)
          call hm_get_floatv('Refer_Rho' ,rhor     ,is_available, lsubmodel, unitab)
!
!     elasticity and failure limits
!
!
          call hm_get_floatv('MAT_EA'    ,e11      ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_EB'    ,e22      ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_EC'    ,e33      ,is_available, lsubmodel, unitab)
!
          call hm_get_floatv('MAT_PRAB'  ,n12      ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_GAB'   ,g12      ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_GBC'   ,g23      ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_GCA'   ,g31      ,is_available, lsubmodel, unitab)
!
          call hm_get_floatv('MAT_EPSF1' ,epsf1    ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_EPSF2' ,epsf2    ,is_available, lsubmodel, unitab)
!
          call hm_get_floatv('MAT_EPST1' ,epst1    ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_EPSM1' ,epsm1    ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_EPST2' ,epst2    ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_EPSM2' ,epsm2    ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_DAMAGE',dmax     ,is_available, lsubmodel, unitab)
!
!     composite plasticity hardening
!
          call hm_get_floatv('WPMAX'     ,wplamx   ,is_available, lsubmodel, unitab)
          call hm_get_intv  ('Itype'     ,ioff     ,is_available, lsubmodel)
          call hm_get_floatv('MAT_R00'   ,ratio    ,is_available, lsubmodel, unitab)
!
!--------------------------------------------------------
!--   composite plasticity hardening
!
          call hm_get_floatv('WPREF'        ,wplaref  ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_BETA'     ,cb       ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_HARD'     ,cn       ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_SIG'      ,fmax     ,is_available, lsubmodel, unitab)
!
!--   composite plasticity in tension / compression
!
          call hm_get_floatv('MAT_SIGYT1'   ,sigyt1   ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_SIGYT2'   ,sigyt2   ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_SIGYC1'   ,sigyc1   ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_SIGYC2'   ,sigyc2   ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_ALPHA '   ,alpha    ,is_available, lsubmodel, unitab)
!
!--   yield stress in shear and strain rate
!
          call hm_get_floatv('MAT_SIGC12'   ,sigyc12  ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_SIGT12'   ,sigyt12  ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_SRC'      ,cc       ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_SRP'      ,epdr     ,is_available, lsubmodel, unitab)
          call hm_get_intv  ('STRFLAG'      ,icc      ,is_available,lsubmodel)
!
!--------------------------------------------------------
!     Delamination
!
          call hm_get_floatv('MAT_GAMAi' ,shrdam   ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_GAMAm' ,shrmax   ,is_available, lsubmodel, unitab)
          call hm_get_floatv('MAT_DAMm'  ,shrdmax  ,is_available, lsubmodel, unitab)
!
!     Strain rate filtering
!
          call hm_get_intv  ('Fsmooth'   ,israte    ,is_available, lsubmodel)
          call hm_get_floatv('Fcut'      ,fcut      ,is_available, lsubmodel, unitab)
!
          call hm_get_floatv_dim('WPREF',fscal_unit,is_available,lsubmodel,unitaB)
!--------------------------------------------------------
          if (cc > zero .and. epdr > zero  .and. fcut == zero) then
            call ancmsg(msgid=1220, msgtype=msgwarning, anmode=aninfo_blind_1,     &
              i1=mat_id,                                                  &
              c1=titr)
          endif
!
          if (e11 == zero .or. e22 == zero .or.                                      &
            g12 == zero .or. g23 == zero .or. g31 == zero) then
            call ancmsg(msgid=306, msgtype=msgerror, anmode=aninfo,                 &
              i1=25,                                                       &
              i2=mat_id,                                                   &
              c1=titr,                                                     &
              c2='e11, e22, g12, g23, g31')
          endif
          if (e33 <= zero) e33 = max(e11, e22)
!
!----------------------------------------
!     check constants for the yield function
!----------------------------------------
          if (sigyt1 <= zero) then
            call ancmsg(msgid=198, msgtype=msgerror, anmode=aninfo,     &
              i1=mat_id,                                        &
              c1=titr,                                          &
              c2='sigyt1')
          endif
          if (sigyc1 <= zero) then
            call ancmsg(msgid=198, msgtype=msgerror, anmode=aninfo,     &
              i1=mat_id,                                       &
              c1=titr,                                         &
              c2='sigyc1')
          endif
          if (sigyt2 <= zero) then
            call ancmsg(msgid=198, msgtype=msgerror, anmode=aninfo,     &
              i1=mat_id,                                         &
              c1=titr,                                           &
              c2='sigyt2')
          endif
          if (sigyc2 <= zero) then
            call ancmsg(msgid=198, msgtype=msgerror, anmode=aninfo,     &
              i1=mat_id,                                        &
              c1=titr,                                          &
              c2='sigyc2')
          endif
          if (sigyt12 <= zero) then
            call ancmsg(msgid=198, msgtype=msgerror, anmode=aninfo,     &
              i1=mat_id,                                        &
              c1=titr,                                          &
              c2='sigyt12')
          endif
          if (sigyc12 <= zero) then
            call ancmsg(msgid=198, msgtype=msgerror, anmode=aninfo,     &
              i1=mat_id,                                       &
              c1=titr,                                         &
              c2='sigyc12')
          endif
!
          if (cn > one) then
            call ancmsg(msgid=213, msgtype=msgerror, anmode=aninfo,       &
              i1=25,                                            &
              i2=mat_id,                                        &
              c1=titr)
          endif
!----------------------------------------
          if (israte == zero) israte = 1    ! for backward compatibility
          if (fcut == zero)   fcut   = ep20
          if (cn == zero)     cn     = one
          if (dmax  == zero)  dmax   = zep999
          if (fmax  == zero)  fmax   = ep20
          if (epst1 == zero)  epst1  = ep20
          if (epst2 == zero)  epst2  = ep20
          if (epsm1 == zero)  epsm1  = onep1*ep20
          if (epsm2 == zero)  epsm2  = onep1*ep20
          if (epsf1 == zero)  epsf1  = onep2*ep20
          if (epsf2 == zero)  epsf2  = onep2*ep20
          fmax   = min(fmax  ,ep20)
          epst1  = min(epst1 ,ep20)
          epst2  = min(epst2 ,ep20)
          epsm1  = min(epsm1 ,onep1*ep20)
          epsm2  = min(epsm2 ,onep1*ep20)
          epsf1  = min(epsf1 ,onep2*ep20)
          epsf2  = min(epsf2 ,onep2*ep20)
!
          if (wplamx == zero) wplamx  = ep20
          if (shrdam == zero) shrdam  = ep20
          if (shrmax == zero) shrmax  = onep1*ep20
          if (shrdmax == zero)shrdmax = one
          if (alpha == zero)  alpha   = one
          if (epdr ==  zero)  epdr    = one
          if (icc == zero)    icc     = 1
          if (wplaref == zero) wplaref = one *fscal_unit
          wplamx = wplamx / wplaref
!
          if (ratio == zero .or. ratio > one ) ratio = one
          if (ratio < zero) ratio = -one
!
          n21   = n12*e22/e11
          nu    = sqrt(n12*n21)
          detc  = one-n12*n21
          c1    = max(e11,e22)/detc
          young = max(e11,e22,e33)
          gmax  = max(g12,g23,g31)
          ssp   = sqrt(max(c1,gmax)/ rho0)
          asrate = fcut*two*pi
!--------------------------------
          if (detc <= zero) then
            call ancmsg(msgid=307, msgtype=msgerror, anmode=aninfo,      &
              i1=mat_id,                                        &
              c1=titr)
          endif
!--------------------------------

          ! -------------------------
          ! simple precision issue : if sigyt1/... are not defined
          ! in the material card, the default value is 1.d+20
          ! some computation can lead to nan in simple precision
          sigyt1_db  = sigyt1
          sigyc1_db  = sigyc1
          sigyt2_db  = sigyt2
          sigyc2_db  = sigyc2
          sigyt12_db = sigyt12
          sigyc12_db = sigyc12
          ! -------------------------

          f1_db  = one / sigyt1_db - one / sigyc1_db
          f2_db  = one / sigyt2_db - one / sigyc2_db
          f11_db = one / max(em20,min(ep20,(sigyt1_db*sigyc1_db)))
          f22_db = one / max(em20,min(ep20,(sigyt2_db*sigyc2_db)))
          f33_db = one / max(em20,min(ep20,(sigyt12_db*sigyc12_db)))
          f12_db = -alpha/(two*sqrt(max(em20,min(ep20,sigyt1_db*sigyc1_db*sigyt2_DB*SIGYC2_DB))))
          ft1_db = f11_db*f22_db - four*f12_db**2

          f1  = f1_db
          f2  = f2_db
          f11 = f11_db
          f22 = f22_db
          f33 = f33_db
          f12 = f12_db
          ft1 = ft1_db
!-------------------------------------------------
          mat_param%niparam = 3
          mat_param%nuparam = 29
          mat_param%ntable  = 0
          mat_param%nfunc   = 0
!
          allocate (mat_param%uparam(mat_param%nuparam))
          allocate (mat_param%iparam(mat_param%niparam))
!      allocate (mat_param%ifunc (mat_param%nfunc))
          allocate (mat_param%table (mat_param%ntable))
!
!------------------------------------------
          ! material parameters used in the law
!------------------------------------------
          mat_param%iparam(1) = iflag
          mat_param%iparam(2) = ioff
          mat_param%iparam(3) = icc
!------------------------------------------
          mat_param%uparam(1)  = e11          !          pm(33)
          mat_param%uparam(2)  = e22          !          pm(34)
          mat_param%uparam(3)  = e33          !          pm(186)
          mat_param%uparam(4)  = n12          !          pm(35)
          mat_param%uparam(5)  = n21          !          pm(36)
          mat_param%uparam(6)  = g12          !          pm(37)
          mat_param%uparam(7)  = g23          !          pm(38)
          mat_param%uparam(8)  = g31          !          pm(39)
          mat_param%uparam(9)  = wplaref      !          pm(68)
          mat_param%uparam(10) = cc           !          pm(50)
          mat_param%uparam(11) = epdr         !          pm(51)

          mat_param%uparam(12) = epst1        !          pm(60)
          mat_param%uparam(13) = epst2        !          pm(61)
          mat_param%uparam(14) = epsm1        !          pm(62)
          mat_param%uparam(15) = epsm2        !          pm(63)
          mat_param%uparam(16) = epsf1        !          pm(98)
          mat_param%uparam(17) = epsf2        !          pm(99)
          mat_param%uparam(18) = dmax         !          pm(64)
          mat_param%uparam(19) = ratio        !          pm(188)

          mat_param%uparam(20) = wplamx       !          pm(41)
          mat_param%uparam(21) = f1           !          pm(54)
          mat_param%uparam(22) = f2           !          pm(55)
          mat_param%uparam(23) = f11          !          pm(56)
          mat_param%uparam(24) = f22          !          pm(57)
          mat_param%uparam(25) = f33          !          pm(58)
          mat_param%uparam(26) = f12          !          pm(59)

!-----------------     tsai-wu only

          mat_param%uparam(27) = cb            !         pm(46)
          mat_param%uparam(28) = cn            !         pm(47)
          mat_param%uparam(29) = fmax          !         pm(49)

!--------------------------

          ! for ply xfem

          pm(65)   = shrdam         !
          pm(66)   = shrmax         !
          pm(67)   = shrdmax        !

!-----------------------------------------
          ! parameters used outside the law
!-----------------------------------------
          pm(1)  = rhor           !  mat_param%rho
          pm(89) = rho0           !  mat_param%rho0
          pm(9)  = asrate         !  mat_param%asrate
          pm(20) = c1             !  mat_param%young
          pm(21) = nu             !  mat_param%nu
          pm(22) = gmax           !  mat_param%shear
          pm(24) = c1
          pm(26) = five_over_6
          pm(27) = ssp            !  mat_param%ssp
          pm(32) = c1             !  mat_param%bulk
!
!     ! still used in elements
          pm(33) = e11            !  mat_param%e11
          pm(34) = e22            !  mat_param%e22
          pm(186)= e33            !  mat_param%e33
          pm(35) = n12            !  mat_param%n12
          pm(36) = n21            !  mat_param%n21
          pm(37) = g12            !  mat_param%g12
          pm(38) = g23            !  mat_param%g23
          pm(39) = g31            !  mat_param%g31
!     ! still used in outputs
          pm(60) = epst1          !  mat_param%uparam(12)
          pm(61) = epst2          !  mat_param%uparam(13)
          pm(98) = epsf1          !  mat_param%uparam(16)
          pm(99) = epsf2          !  mat_param%uparam(17)
          pm(41) = wplamx         !  mat_param%uparam(20)
          pm(64) = dmax           !  mat_param%uparam(18)
!-----------------------------------------
!     formulation for solid elements time step computation.
          fac  = one/(one - n12*n21)
          d11  = e11*fac
          d22  = e22*fac
          d12  = n21*d11
          dmin = d11*d22  - d12**2
          dmx2 = max(d11, d22)**2
!
          pm(105) = dmin/dmx2
!
!-----------------------------------------
          parmat(1)  = c1
          parmat(2)  = c1    ! young
          parmat(3)  = nu
          parmat(4)  = israte
          parmat(5)  = fcut
          parmat(16) = 1     ! iformdt  (solid element timestep computation flag)
!-------------------------------------
          ! Output
!-------------------------------------
          write(iout,1050) trim(titr),mat_id,25
          write(iout,1000)
          if (is_encrypted) then
            write(iout,'(5x,a,//)')'confidential data'
          else
            write(iout,1060) rho0
            write(iout,1100) e11,e22,n12,e33
            write(iout,1300) g12,g23,g31
            write(iout,1500) epst1,epsm1,epsf1,epst2,epsm2,epsf2,dmax
            write(iout,1805) wplamx,wplaref
            write(iout,1807) ioff,ratio
            write(iout,1510) cb,cn,fmax
            write(iout,1550) sigyt1,sigyt2,sigyc1,sigyc2,alpha
            write(iout,1560) sigyt12,sigyc12
            write(iout,1610) cc,epdr,icc
!
            write(iout,1600) shrdam,shrmax,shrdmax
            write(iout,1611) israte,fcut
            write(iout,1650) f1,f2,f11,f22,f33,f12
          endif
!-----------
          return
!-----------
1000      format(                                                             &
            5x,A,/,                                                             &
            5x,40H  ORTHOTROPIC MATERIAL FOR SHELLS       ,/,                   &
            5x,40H  -------------------------------       ,//)
1050      forMAT(/                                                            &
            5x,A,/,                                                             &
            5x,'MATERIAL NUMBER . . . . . . . . . . . . .=',i10/,               &
            5x,'MATERIAL LAW. . . . . . . . . . . . . . .=',i10/)
1060      format(5x,'INITIAL DENSITY . . . . . . . . . . . . .=',1pg20.13/)
1100      format(                                                             &
            5x,40HYOUNG'S MODULUS E11 . . . . . . . . . .=,e12.4/,              &
            5x,40HYOUNG'S MODULUS E22 . . . . . . . . . .=,e12.4/,              &
            5x,40HPOISSON'S RATIO N12 . . . . . . . . . .=,e12.4/,              &
            5x,40HYOUNG'S MODULUS E33 . . . . . . . . . .=,e12.4/)
1300      format(                                                             &
            5x,40HSHEAR MODULUS   G12 . . . . . . . . . .=,e12.4/,              &
            5x,40HSHEAR MODULUS   G23 . . . . . . . . . .=,e12.4/,              &
            5x,40HSHEAR MODULUS   G31 . . . . . . . . . .=,e12.4/)
1500      format(                                                             &
            5x,'TENSILE FAILURE STRAIN DIR-1. . . . . .=',1pg20.13/,            &
            5x,'MAXIMUM TENSILE STRAIN DIR-1. . . . . .=',1pg20.13/,            &
            5x,'TOTAL FAILURE TENSILE STRAIN DIR-1. . .=',1pg20.13/,            &
            5x,'TENSILE FAILURE STRAIN DIR-2. . . . . .=',1pg20.13/,            &
            5x,'MAXIMUM TENSILE STRAIN DIR-2. . . . . .=',1pg20.13/,            &
            5x,'TOTAL FAILURE TENSILE STRAIN DIR-2. . .=',1pg20.13/,            &
            5x,'MAXIMUM DAMAGE DIR. . . . . . . . . . .=',1pg20.13/)
1510      format(                                                             &
            5x,'COMPOSITE HARDENING PARAMETER  (B). . .=',1pg20.13/,            &
            5x,'COMPOSITE HARDENING PARAMETER  (N). . .=',1pg20.13/,            &
            5x,'COMPOSITE MAXIMUM YIELD FUNCTION FMAX .=',1pg20.13//)
1550      format(                                                             &
            5x,'COMPOSITE YIELD IN TENSION DIR-1  . . . . . .=',1pg20.13/,      &
            5x,'COMPOSITE YIELD IN TENSION DIR-2  . . . . . .=',1pg20.13/,      &
            5x,'COMPOSITE YIELD IN COMPRESSION DIR-1  . . . .=',1pg20.13/,      &
            5x,'COMPOSITE YIELD IN COMPRESSION DIR-2  . . . .=',1pg20.13/,      &
            5x,'F12 REDUCTION FACTOR . . . . . . . .  . . . .=',1pg20.13//)
1560      format(                                                             &
            5x,'COMPOSITE YIELD IN SHEAR (+12) . . . .=',1pg20.13/,             &
            5x,'COMPOSITE YIELD IN SHEAR (-12) . . . .=',1pg20.13//)
1600      format(                                                             &
            5x,'SHEAR DELAMINATION                      '/,                     &
            5x,'INITIATION STRAIN . . . . . . . . . . .=',1pg20.13/,            &
            5x,'MAXIMUM STRAIN. . . . . . . . . . . . .=',1pg20.13/,            &
            5x,'MAXIMUM DAMAGE. . . . . . . . . . . . .=',1pg20.13/)
1650      format(                                                             &
            5x,'  YIELD FUNCTION CONSTANTS              '/,                     &
            5x,'  ------------------------              '//,                    &
            5x,'F1 , F2 . . . . . . . . . . . . . . . .=',2e12.4/,              &
            5x,'F11, F22. . . . . . . . . . . . . . . .=',2e12.4/,              &
            5x,'F33 . . . . . . . . . . . . . . . . . .=', e12.4/,              &
            5x,'F12 . . . . . . . . . . . . . . . . . .=', e12.4//)
1610      format(                                                             &
            5x,'STRAIN RATE COEFFICIENT CC. . . . . . .=',1pg20.13/,            &
            5x,'REFERENCE STRAIN RATE . . . . . . . . .=',1pg20.13/,            &
            5x,'FLAG FOR STRAIN RATE ON F-MAX WP-MAX. .=',i10//)
1611      format(                                                             &
            5x,'SMOOTH STRAIN RATE OPTION . . . . . . .=',i10/,                 &
            5x,'STRAIN RATE CUTTING FREQUENCY . . . . .=',1pg20.13/)
1805      format(                                                             &
            5x,'COMPOSITE MAXIMUM PLASTIC WORK WPLAMX .=',1pg20.13/,            &
            5x,'REFERENCE PLASTIC WORK WPLAREF        .=',1pg20.13)
1807      format(                                                             &
            5x,'TOTAL ELEMENT FAILURE CRITERIA    IOFF =',i10/,                 &
            5x,' IOFF=0: SHELL DELETE IF WPLA >= WPLAMX FOR 1 LAYER'/,          &
            5x,' IOFF=1: SHELL DELETE IF WPLA >= WPLAMX FOR ALL LAYERS'/,       &
            5x,' IOFF=2: SHELL DELETE IF FOR EACH LAYER :'/,                    &
            5x,'         WPLA >= WPLAMX OR TENSILE FAILURE DIR 1'/,             &
            5x,' IOFF=3: SHELL DELETE IF FOR EACH LAYER :'/,                    &
            5x,'         WPLA >= WPLAMX OR TENSILE FAILURE DIR 2'/,             &
            5x,' IOFF=4: SHELL DELETE IF FOR EACH LAYER :'/,                    &
            5x,'         WPLA >= WPLAMX OR TENSILE FAILURE DIR 1 AND 2'/,       &
            5x,' IOFF=5: SHELL DELETE IF FOR ALL LAYERS :'/,                    &
            5x,'         WPLA >= WPLAMX OR TENSILE FAILURE DIR 1'/,             &
            5x,'         OR'/,                                                  &
            5x,'         WPLA >= WPLAMX OR TENSILE FAILURE DIR 2'/,             &
            5x,' IOFF=6: SHELL DELETE IF FOR EACH LAYER :'/,                    &
            5x,'         WPLA >= WPLAMX OR TENSILE FAILURE DIR 1 OR 2'//,       &
            5x,'LAYER FAILURE RATIO FOR ELEMENT DELETION. =',E12.4 )
!-----------------
        end subroutine read_mat25_tsaiwu
      end module read_mat25_tsaiwu_mod
