!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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

! ======================================================================================================================
! \brief read config file for material law25 with Tsai-Wu formulation
!! \details 

! ======================================================================================================================

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
      use constant_mod !, only : onep8333,twop444,twop6666666667,twop6667,fivep333,two,half,em20,ep20
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
!     included files
! ----------------------------------------------------------------------------------------------------------------------

#include "my_real.inc"

!-----------------------------------------------
!     d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer                                ,intent(in)    :: mat_id
      integer                                ,intent(in)    :: iout
      integer                                ,intent(in)    :: npropm
      type (unit_type_)                      ,intent(in)    :: unitab 
      type(submodel_data), dimension(nsubmod),intent(in)    :: lsubmodel
      character(len=nchartitle)              ,intent(in)    :: titr
!
      integer                                ,intent(inout) :: israte
      my_real, dimension(100)                ,intent(inout) :: parmat  
      my_real, dimension(npropm)             ,intent(inout) :: pm     
      type(matparam_struct_)                 ,intent(inout) :: mat_param
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      logical :: is_available,is_encrypted
      integer :: ioff,icc,iflag,imodwp
      my_real :: rho0,rhor,e11,e22,e33,g12,g23,g31,gmax,nu,n12,n21,         &
        cb,cn,epst1,epst2,asrate,                                           &
        fmax, sigyt1, sigyt2, sigyc1, sigyc2, sigyt12, sigyc12,             &
        c1, ssp, f1, f2, f11, f22, f33, f12, ft1, wplamx,                   &
        epsm1, epsm2, dmax,dmx2,shrdam, shrmax, shrdmax,alpha,cc,eps0,      &
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
      cb=zero          
      cn=zero          
      fmax=zero        
      shrdam =zero     
      shrmax =zero     
      shrdmax=zero     
      wplaref=zero
!
      sigyt1=zero      
      cbt1=zero        
      cnt1=zero        
      sigmxt1=zero     
      cct1=zero        
!
      sigyt2=zero      
      cbt2=zero        
      cnt2=zero        
      sigmxt2=zero     
      cct2=zero        
!
      sigyc1=zero      
      cbc1=zero        
      cnc1=zero        
      sigmxc1=zero     
      ccc1=zero        
!
      sigyc2=zero      
      cbc2=zero        
      cnc2=zero        
      sigmxc2=zero     
      ccc2=zero        
!
      sigyc12=zero     
      cbc12=zero       
      cnc12=zero       
      sigmxc12=zero    
      ccc12=zero       
!
      eps1t1=zero      
      eps2t1=zero      
      sigrst1=zero     
      wplamxt1=zero    
!
      eps1t2=zero      
      eps2t2=zero      
      sigrst2=zero     
      wplamxt2=zero    
!
      eps1c1=zero      
      eps2c1=zero      
      sigrsc1=zero     
      wplamxc1=zero    
!
      eps1c2=zero      
      eps2c2=zero      
      sigrsc2=zero     
      wplamxc2=zero    
!
      eps1t12=zero     
      eps2t12=zero     
      sigrst12=zero    
      wplamxt12=zero
!
      israte = 0
      fcut   = zero
      ratio  = zero
      imodwp = 0
!--------------------------------------------------------
!
      call hm_option_is_encrypted(is_encrypted)
!
!--------------------------------------------------------
!     Common input fields
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
      call hm_get_floatv('MAT_SRP'      ,eps0     ,is_available, lsubmodel, unitab)
      call hm_get_intv  ('STRFLAG'      ,icc      ,is_available,lsubmodel)      
!
    
!--------------------------------------------------------
!     Common input fields
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
      if (cc > zero .and. eps0 > zero  .and. fcut == zero) then                  
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
      asrate = fcut*two*pi
!                                                                                 
!--------------------------------
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
      if (eps0 ==  zero)  eps0    = one 
      if (icc == zero)    icc     = 1
      if (wplaref == zero) wplaref = one *fscal_unit
      wplamx = wplamx / wplaref
!
      if (ratio == zero .or. ratio > one ) ratio = one
      if (ratio < zero) ratio = -one
!
      n21  = n12*e22/e11
      nu   = sqrt(n12*n21)
      detc = one-n12*n21
      c1   = max(e11,e22)/detc
      gmax = max(g12,g23,g31)
      ssp  = sqrt(max(c1,gmax)/ rho0)
!--------------------------------
      if (((epst1 >= epsm1) .and.((epst1 /= zero) .and.                            &
          (epsm1 /= zero))).or. ((epst2 >= epsm2).and.(epst2 /= zero).and.         &
          (epsm2 /= zero))) then                                                    
       call ancmsg(msgid=562, msgtype=msgerror, anmode=aninfo_blind_1,             &
                   i1=mat_id,                                                      &
                   c1=titr)                                                         
      endif                                                                          
!                                                                                   
      if ((epsf1 <= epst1 .and. epst1 /= zero) .or.                                &
         (epsf2 <= epst2 .and. epst2 /= zero)) then                                 
       call ancmsg(msgid=616, msgtype=msgerror, anmode=aninfo_blind_1,             &
                   i1=mat_id,                                                      &
                   c1=titr)
      end if
!
      if (detc <= zero) then
         call ancmsg(msgid=307, msgtype=msgerror, anmode=aninfo,      &
                    i1=mat_id,                                            &
                    c1=titr)                                           
      endif
!                                                                      
      if (cn > one) then                                                
        call ancmsg(msgid=213, msgtype=msgerror, anmode=aninfo,       &
                    i1=25,                                            &
                    i2=mat_id,                                            &
                    c1=titr)
      endif
!----------------------------------------
!     constants for the yield function
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
      mat_param%niparam = 4
      mat_param%nuparam = 32
      mat_param%ntable  = 0
!      mat_param%nfunc   = 0
!          
      allocate (mat_param%uparam(mat_param%nuparam))
      allocate (mat_param%iparam(mat_param%niparam))
!      allocate (mat_param%ifunc (mat_param%nfunc))
      allocate (mat_param%table (mat_param%ntable))
!     
      mat_param%iparam(1) = iflag
      mat_param%iparam(2) = ioff
      mat_param%iparam(3) = icc
      mat_param%iparam(4) = imodwp
!------------------------------------------
      ! material parameters used in the law
!------------------------------------------
      ! integer
!-----------------
      pm(40)  = iflag
      pm(42)  = ioff
      pm(53)  = icc
      pm(189) = imodwp
!-----------------
      ! real
!-----------------      
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
      mat_param%uparam(11) = eps0         !          pm(51) 

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
 ! for ply xfem
      mat_param%uparam(27) = shrdam         !        pm(65)
      mat_param%uparam(28) = shrmax         !        pm(66)
      mat_param%uparam(29) = shrdmax        !        pm(67)

!-----------------     tsai-wu only 

      mat_param%uparam(30) = cb             !       pm(46)
      mat_param%uparam(31) = cn             !       pm(47)
      mat_param%uparam(32) = fmax           !       pm(49)
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
        write(iout,1650) f1,f2,f11,f22,f33,f12
        write(iout,1500) epst1,epsm1,epsf1,epst2,epsm2,epsf2,dmax
        write(iout,1805) wplamx,wplaref
        write(iout,1807) ioff,imodwp,ratio
        write(iout,1510) cb,cn,fmax
        write(iout,1550) sigyt1,sigyt2,sigyc1,sigyc2,alpha
        write(iout,1560) sigyt12,sigyc12
        write(iout,1610) cc,eps0,icc
!
        write(iout,1600) shrdam,shrmax,shrdmax
        write(iout,1611) israte,fcut
      endif
!-----------
      return
!-----------
 1000 format(                                                             &
      5x,a,/,                                                             &
      5x,40h  orthotropic material for shells       ,/,                   &
      5x,40h  -------------------------------       ,//)                  
 1050 format(/                                                            &
      5x,a,/,                                                             &
      5x,'material number . . . . . . . . . . . . .=',i10/,               &
      5x,'material law. . . . . . . . . . . . . . .=',i10/)               
 1060 format(5x,'initial density . . . . . . . . . . . . .=',1pg20.13/)  
 1100 format(                                                             &
      5x,40hyoung's modulus e11 . . . . . . . . . .=,e12.4/,              &
      5x,40hyoung's modulus e22 . . . . . . . . . .=,e12.4/,              &
      5x,40hpoisson's ratio n12 . . . . . . . . . .=,e12.4/,              &
      5x,40hyoung's modulus e33 . . . . . . . . . .=,e12.4/)
 1300 format(                                                             &
      5x,40hshear modulus   g12 . . . . . . . . . .=,e12.4/,              &
      5x,40hshear modulus   g23 . . . . . . . . . .=,e12.4/,              &
      5x,40hshear modulus   g31 . . . . . . . . . .=,e12.4/)
 1500 format(                                                             &
      5x,'tensile failure strain dir-1. . . . . .=',1pg20.13/,            &
      5x,'maximum tensile strain dir-1. . . . . .=',1pg20.13/,            &
      5x,'total failure tensile strain dir-1. . .=',1pg20.13/,            &
      5x,'tensile failure strain dir-2. . . . . .=',1pg20.13/,            &
      5x,'maximum tensile strain dir-2. . . . . .=',1pg20.13/,            &
      5x,'total failure tensile strain dir-2. . .=',1pg20.13/,            &
      5x,'maximum damage dir. . . . . . . . . . .=',1pg20.13/)
 1510 format(                                                             &
      5x,'composite hardening parameter  (b). . .=',1pg20.13/,            &
      5x,'composite hardening parameter  (n). . .=',1pg20.13/,            &
      5x,'composite maximum yield function fmax .=',1pg20.13//)
 1550 format(                                                             &
      5x,'composite yield in tension dir-1  . . . . . .=',1pg20.13/,      &
      5x,'composite yield in tension dir-2  . . . . . .=',1pg20.13/,      &
      5x,'composite yield in compression dir-1  . . . .=',1pg20.13/,      &
      5x,'composite yield in compression dir-2  . . . .=',1pg20.13/,      &
      5x,'f12 reduction factor . . . . . . . .  . . . .=',1pg20.13//)
 1560 format(                                                             &
      5x,'composite yield in shear (+12) . . . .=',1pg20.13/,             &
      5x,'composite yield in shear (-12) . . . .=',1pg20.13//)
 1600 format(                                                             &
      5x,'shear delamination                      '/,                     &
      5x,'initiation strain . . . . . . . . . . .=',1pg20.13/,            &
      5x,'maximum strain. . . . . . . . . . . . .=',1pg20.13/,            &
      5x,'maximum damage. . . . . . . . . . . . .=',1pg20.13/)
 1650 format(                                                             &
      5x,'  yield function constants              '/,                     &
      5x,'  ------------------------              '//,                    &
      5x,'f1 , f2 . . . . . . . . . . . . . . . .=',2e12.4/,              &
      5x,'f11, f22. . . . . . . . . . . . . . . .=',2e12.4/,              &
      5x,'f33 . . . . . . . . . . . . . . . . . .=', e12.4/,              &
      5x,'f12 . . . . . . . . . . . . . . . . . .=', e12.4//)
 1610 format(                                                             &
      5x,'strain rate coefficient cc. . . . . . .=',1pg20.13/,            &
      5x,'reference strain rate . . . . . . . . .=',1pg20.13/,            &
      5x,'flag for strain rate on f-max wp-max. .=',i10//)
 1611 format(                                                             &
      5x,'smooth strain rate option . . . . . . .=',i10/,                 &
      5x,'strain rate cutting frequency . . . . .=',1pg20.13/)
 1805 format(                                                             &
      5x,'composite maximum plastic work wplamx .=',1pg20.13/,            &
      5x,'reference plastic work wplaref        .=',1pg20.13)
 1807 format(                                                             &
      5x,'total element failure criteria    ioff =',i10/,                 &
      5x,' ioff=0: shell delete if wpla >= wplamx for 1 layer'/,          &
      5x,' ioff=1: shell delete if wpla >= wplamx for all layers'/,       &
      5x,' ioff=2: shell delete if for each layer :'/,                    &
      5x,'         wpla >= wplamx or tensile failure dir 1'/,             &
      5x,' ioff=3: shell delete if for each layer :'/,                    &
      5x,'         wpla >= wplamx or tensile failure dir 2'/,             &
      5x,' ioff=4: shell delete if for each layer :'/,                    &
      5x,'         wpla >= wplamx or tensile failure dir 1 and 2'/,       &
      5x,' ioff=5: shell delete if for all layers :'/,                    &
      5x,'         wpla >= wplamx or tensile failure dir 1'/,             &
      5x,'         or'/,                                                  &
      5x,'         wpla >= wplamx or tensile failure dir 2'/,             &
      5x,' ioff=6: shell delete if for each layer :'/,                    &
      5x,'         wpla >= wplamx or tensile failure dir 1 or 2'//,       &
      5x,' directional wpmax failure formulation    =', i10 //,           &
      5x,'layer failure ratio for element deletion. =',e12.4 )
!-----------------
      end subroutine read_mat25_tsaiwu
      end module read_mat25_tsaiwu_mod
