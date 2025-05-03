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
      !||====================================================================
      !||    hm_read_mat127_mod   ../starter/source/materials/mat/mat127/hm_read_mat127.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat          ../starter/source/materials/mat/hm_read_mat.F90
      !||====================================================================
      module hm_read_mat127_mod
      contains
! ======================================================================================================================
! \brief Reading material parameters of /MAT/LAW127
! \details Reading material parameters of /MAT/LAW127
! ======================================================================================================================
      !||====================================================================
      !||    hm_read_mat127           ../starter/source/materials/mat/mat127/hm_read_mat127.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_mat              ../starter/source/materials/mat/hm_read_mat.F90
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                   ../starter/source/output/message/message.F
      !||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||    init_mat_keyword         ../starter/source/materials/mat/init_mat_keyword.F
      !||--- uses       -----------------------------------------------------
      !||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
      !||    message_mod              ../starter/share/message_module/message_mod.F
      !||    submodel_mod             ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      subroutine hm_read_mat127(                                      &       
               nuvar    ,maxfunc  ,npropm   ,iout,                    &
               nfunc    ,ifunc    ,mtag     ,parmat   ,unitab   ,     &
               pm       ,lsubmodel,israte   ,mat_id   ,titr     ,     &
               matparam ,nvartmp  )
 !-----------------------------------------------
 !   M o d u l e s
 !-----------------------------------------------
          use unitab_mod
          use message_mod
          use submodel_mod
          use matparam_def_mod    
          use elbuftag_mod      
          use constant_mod    
 !-----------------------------------------------
 !   I m p l i c i t   T y p e sXM
 !-----------------------------------------------
         implicit none 
#include  "my_real.inc"
 !-----------------------------------------------
 !   D u m m y   A r g u m e n t s
 !-----------------------------------------------
      integer, intent(in)                          :: mat_id,maxfunc,npropm,iout
      integer, intent(inout)                       :: nuvar,nfunc,nvartmp
      integer, intent(inout)                       :: israte
      integer, dimension(maxfunc), intent(inout)   :: ifunc
      type(mlaw_tag_), intent(inout)               :: mtag
      my_real, dimension(100),intent(inout)        :: parmat
      type (unit_type_),intent(in)                 :: unitab 
      my_real, dimension(npropm) ,intent(inout)    :: pm   
      type(submodel_data), dimension(*),intent(in) :: lsubmodel  
      character(len=nchartitle),intent(in)         :: titr 
      type(matparam_struct_) ,intent(inout)        :: matparam    
 !-----------------------------------------------
 !   L o c a l   V a r i a b l e s
 !-----------------------------------------------
      integer ifxc,ifxt,ifyc,ifyt,ifsc,ilaw, crit, twoway, ti,ncyred
      my_real                                                                &
         ! Material properties
         rho0, e1, e2, e3, g12, g23, g13, nu12, nu21, nu23, nu31, nu13, nu32, &
         young, nu, g31, gmax, ssp, asrate, ms13, fac, fbrt, ycfac,           &
         beta, alpha, epsf, epsr, tsmd, c1,                                      &
         ! Strength parameters
         xc, xt, yc, yt, sc,                                                  &
         ! Failure criteria
         dfailc, dfailm, dfails, dfailt, efs, ratio,                          &
         ! Scaling factors
         yfac_xt, yfac_xc, yfac_yt, yfac_yc, yfac_sc,                         &
         ! Softening parameters
         soft,                                                                &
         ! Slenderness limits
         slimt1, slimc1, slimt2, slimc2, slims,                               &
         ! Matrix coefficients
         a11, a22, a12, c11, c22, c33, c12, c13, c23, detc,  c21,c32,c31 ,     &
         ! Inverse matrix elements
         d11, d22, d33, d12, d13, d23, d21,d32,d31, invd ,                     &
         e2_mod, nu12_mod, nu31_mod, nu13_mod, nu23_mod,                       &
         ! Cut-off frequency
         fcut
      logical :: is_available,is_encrypted
 !=======================================================================
      is_encrypted = .false.
      is_available = .false.
      ilaw         = 127
      g31         = zero  !is not initialized elsewhere
 !------------------------------------------
      call hm_option_is_encrypted(is_encrypted)
 !------------------------------------------
! - Density
      call hm_get_floatv('MAT_RHO'   ,rho0     ,is_available, lsubmodel, unitab)
! - young's moduli + shear moduli
      call hm_get_floatv('LSDYNA_EA'    ,e1       ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSDYNA_EB'    ,e2       ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSDYNA_EC'    ,e3       ,is_available, lsubmodel, unitab)
!  - shear modulus +
      call hm_get_floatv('LSDYNA_GAB'   ,g12      ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSDYNA_GBC'   ,g23      ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSDYNA_GCA'   ,g13      ,is_available, lsubmodel, unitab)     
! -  poisson's ratio ! tocheck
      call hm_get_floatv('LSDYNA_PRBA'  ,nu21     ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSDYNA_PRCB'  ,nu32     ,is_available, lsubmodel, unitab)
      call hm_get_floatv('LSDYNA_PRCA'  ,nu31    ,is_available, lsubmodel, unitab) 
! - dir 11 tension 
      call hm_get_floatv  ('LSD_MAT_XT'       ,xt         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_SLIMT1'   ,slimt1     ,is_available, lsubmodel, unitab)
      call hm_get_intv  ('LSD_LCXT'           ,ifxt     ,is_available, lsubmodel)  
      call hm_get_floatv  ('MAT_SCALCXT'     ,yfac_xt     ,is_available, lsubmodel, unitab)
!  - dir 11 compression 
      call hm_get_floatv  ('LSD_MAT_XC'       ,xc         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_SLIMC1'   ,slimc1     ,is_available, lsubmodel, unitab)
      call hm_get_intv  ('LSD_LCXC'           ,ifxc  ,is_available, lsubmodel)  
      call hm_get_floatv  ('MAT_SCALCXC'     ,yfac_xc     ,is_available, lsubmodel, unitab)
!  - dir 22 tension 
      call hm_get_floatv  ('LSD_MAT_YT'       ,yt         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_SLIMT2'   ,slimt2     ,is_available, lsubmodel, unitab)
      call hm_get_intv  ('LSD_LCYT'           ,ifyt     ,is_available, lsubmodel)
      call hm_get_floatv  ('MAT_SCALCYT'     ,yfac_yt    ,is_available, lsubmodel, unitab)
!  - dir 22 compression 
      call hm_get_floatv  ('LSD_MAT_YC'       ,yc         ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_SLIMC2'   ,slimc2     ,is_available, lsubmodel, unitab)
      call hm_get_intv  ('LSD_LCYC'           ,ifyc  ,is_available, lsubmodel)   
      call hm_get_floatv  ('MAT_SCALCYC'     ,yfac_yc    ,is_available, lsubmodel, unitab)
! - shear 12 
      call hm_get_floatv  ('LSD_MAT_SC'       ,sc        ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_SLIMS'    ,slims     ,is_available, lsubmodel, unitab)
      call hm_get_intv  ('LSD_LCSC'           ,ifsc  ,is_available, lsubmodel)   
      call hm_get_floatv  ('MAT_SCALCSC'     ,yfac_sc     ,is_available, lsubmodel, unitab)
! -  failure parameters 
      call hm_get_floatv  ('LSD_FBRT'     ,fbrt       ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_YCFAC'    ,ycfac      ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_DFAILT'   ,dfailt     ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_DFAILC'   ,dfailc     ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_DFAILM'   ,dfailm     ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_DFAILS'   ,dfails     ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_EFS'      ,efs        ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LRD_RATIO'    ,ratio        ,is_available, lsubmodel, unitab)
 ! parameters 
      call hm_get_floatv  ('LSD_BETA'      ,beta        ,is_available, lsubmodel, unitab)
      call hm_get_floatv  ('LSD_MAT_ALPH'  ,alpha       ,is_available, lsubmodel, unitab)    
 ! out of plane failure parameters
      call hm_get_floatv  ('LSD_MAT_EPSF'      ,epsf        ,is_available, lsubmodel, unitab)   
      call hm_get_floatv  ('LSD_MAT_EPSR'      ,epsr        ,is_available, lsubmodel, unitab)    
      call hm_get_floatv  ('LSD_MAT_TSMD'      ,tsmd        ,is_available, lsubmodel, unitab)  
      call hm_get_intv  ('LSD_MAT_NCYRED'      ,ncyred  ,is_available, lsubmodel)   
      call hm_get_intv  ('LSD_2WAY'           ,twoway  ,is_available, lsubmodel)   
      call hm_get_intv  ('LSD_TI'             ,TI  ,is_available, lsubmodel)    
! - equivalent strain rate cutoff frequency 
      call hm_get_floatv('fcut'      ,fcut     ,is_available, lsubmodel, unitab)
!
      ! young modulus initialization
      if (e2 == zero)  e2  = e1
      if (e3 == zero)  e3  = e2
      ! shear modulus
      if (g13 == zero) g13 = g12
      if (g23 == zero) g23 = g13
!-----------------------------
 !     check and default values
 !-----------------------------
      ! poisson's ratio 
      nu12 = nu21*e1/e2 
      ! checking poisson's ratio 
      if(nu12*nu21 >= one ) then
        call ancmsg(msgid=3068,                        &
                  msgtype=msgerror,                    &
                  anmode=aninfo_blind_2,               &
                  i1=mat_id,                           &
                  c1=titr)
      endif
      ! to be checked if are used for 3D as nu12 & nu21
      nu23 = nu32*e2/e3
      nu13 = nu31*e1/e3
      detc = one - nu12*nu21
      if (detc <= zero) then
        call ancmsg(msgid=307,                        &
                    msgtype=msgerror,                 &
                    anmode=aninfo,                    &
                    i1=mat_id,                        &
                    c1=titr)
      endif 
      ! elasticity matrix for 2d plane stress
      fac = one/(one - nu12*nu21)
      a11 = e1*fac
      a12 = nu21*a11
      a22 = e2*fac
      ! compliance matrix for 3 
      if(ti > 0) then
         e2_mod = e1
         nu12_mod = nu21
         nu31_mod = nu32
         nu13_mod = nu31_mod*e1/e3
         nu23_mod = nu13_mod
      else
         e2_mod  = e2
         nu12_mod = nu12
         nu31_mod = nu31
         nu13_mod = nu13
         nu23_mod = nu23
      endif
      c11 = one / e1
      c22 = one / e2_mod
      c33 = one / e3
      c12 = -nu21 / e2_mod
      c13 = -nu31_mod / e3 
      c21 = -nu12_mod / e1
      c23 = -nu32 / e3 
      c31 = -nu13_mod / e1 
      c32 = -nu23_mod / e2_mod 
     ! Calculate the determinant
      detc = c11 * (c22 * c33 - c23 * c32) - &
                   c12 * (c21 * c33 - c23 * c31) + &
                   c13 * (c21 * c32 - c22 * c31)
      ! Ensure the determinant is not zero
      if(detc <= zero) then
         call ancmsg(msgid=307,                                  &
                    msgtype=msgerror,                            &     
                    anmode=aninfo,                               &
                    i1=mat_id,                                   &
                    c1=titr) 
       endif
      ! Calculate the inverse determinant
       invd = 1.0 / detc
      ! ! 3d elastic matrix
      d11 =  invd * (c22 * c33 - c23 * c32)
      d12 = -invd * (c12 * c33 - c13 * c32)
      d13 =  invd * (c12 * c23 - c13 * c22)
      d22 =  invd * (c11 * c33 - c13 * c31)
      d23 = -invd * (c11 * c23 - c13 * c21)
      d33 =  invd * (c11 * c22 - c12 * c21)
      !
      !!d21 = -invd * (c21 * c33 - c23 * c31)
      !!d31 =  invd * (c21 * c32 - c22 * c31)
      !!d32 = -invd * (c11 * c32 - c12 * c31)
      ! default strain rate cutoff frequency
      if (fcut == zero) fcut = 5000.0d0*unitab%fac_t_work
 !--------------------------
 !     filling buffer tables
 !-------------------------- 
      ! number of material parameters
      matparam%nuparam = 49
      matparam%niparam  = 3

      allocate (matparam%uparam(matparam%nuparam))
      allocate (matparam%iparam(matparam%niparam))

      ! number of functions
      nfunc   = 5
      ! number of user variables 
      nuvar   = 3
      ! number of temporary variable for interpolation
      nvartmp = 0
!     
      ! default values 
      if(sc == zero ) sc = ep20
      if(xt == zero ) xt = ep20
      if(xc == zero ) xc = ep20
      if(yt == zero ) yt = ep20
      if(yc == zero ) yc = ep20
      !
      if(fbrt == zero ) fbrt= one
      if(ycfac == zero) ycfac= two
      ! scale for minimum stress limit 
      if(slimt1 == zero) slimt1 = one
      if(slimc1 == zero) slimc1 = one
      if(slimt2 == zero) slimt2 = one
      if(slimc2 == zero) slimc2 = one
      if(slims == zero) slims = one
      ! scale for function of strain rate dependency
      if(yfac_xc == zero) yfac_xc= one
      if(yfac_xt == zero) yfac_xt= one
      if(yfac_yc == zero) yfac_yc= one
      if(yfac_yt == zero) yfac_yt= one
      if(yfac_sc == zero) yfac_sc= one
      ! failure parameters 
      if(dfailc == zero) dfailc = ep10
      if(dfailm == zero) dfailm = ep10
      if(dfails == zero) dfails = ep10
      if(efs == zero) efs  = ep10
      if(ncyred == 0) ncyred = 1
      if(ratio <= zero .or. ratio > one ) ratio = one
      ! outp damage parameters
      if(epsf == zero) epsf= ep10
      if(epsr == zero) epsr = two*epsf
      if(tsmd == zero) tsmd = zep9
      ! material parameters
      matparam%uparam(1)  = e1
      matparam%uparam(2)  = e2
      matparam%uparam(3)  = e3
      matparam%uparam(4)  = g12
      matparam%uparam(5)  = g13
      matparam%uparam(6)  = g23
      !
      matparam%uparam(7)  = nu12
      matparam%uparam(8)  = nu21
      matparam%uparam(9)  = nu13
      matparam%uparam(10)  = nu23
      matparam%uparam(11)  = nu31
      matparam%uparam(12)  = nu32  
      !
      matparam%uparam(13)   = xt
      matparam%uparam(14)  = slimt1
      matparam%uparam(15)  = xc
      matparam%uparam(16)  = slimc1
     !
      matparam%uparam(17)  = yt
      matparam%uparam(18)  = slimt2
      matparam%uparam(19)  = yc
      matparam%uparam(20)  = slimc2
    !
      matparam%uparam(21)  = sc
      matparam%uparam(22)  = slims
     !
      matparam%uparam(23)  = fbrt
      matparam%uparam(24)  = ycfac  

      matparam%uparam(25)  = dfailt
      matparam%uparam(26)  = dfailc
      matparam%uparam(27)  = dfailm
      matparam%uparam(28)  = dfails
      matparam%uparam(29)  = efs
      matparam%uparam(30)  = ratio
      !
      matparam%uparam(31)  = beta
      matparam%uparam(32)  = alpha
      matparam%uparam(33)  = epsf
      matparam%uparam(34)  = epsr
      matparam%uparam(35)  = tsmd
      !
      matparam%uparam(36)  = yfac_xt
      matparam%uparam(37)  = yfac_xc
      matparam%uparam(38)  = yfac_yt
      matparam%uparam(39)  = yfac_yc
      matparam%uparam(40)  = yfac_sc    
      !
      matparam%uparam(41)  = d11
      matparam%uparam(42)  = d22
      matparam%uparam(43)  = d33
      matparam%uparam(44)  = d12
      matparam%uparam(45)  = d13 
      matparam%uparam(46)  = d23 
      if(ti > 0) then  ! only used by solid an thick shell
        matparam%uparam(47)  = half*e1/(one + nu21)
        matparam%uparam(48)  = g23 
        matparam%uparam(49)  = g23 
      else
        matparam%uparam(47)  = g12
        matparam%uparam(48)  = g13
        matparam%uparam(49)  = g23
      endif
      ! integer flag
      matparam%iparam(1)  = twoway
      matparam%iparam(2)  = ti
      matparam%iparam(3)  = ncyred
      ! function ids 
      ifunc(1)  = ifxt
      ifunc(2)  = ifxc
      ifunc(3)  = ifyt
      ifunc(4)  = ifyc
      ifunc(5)  = ifsc
      !
       nu    = min(sqrt(nu12*nu21), sqrt(nu13*nu31), sqrt(nu23*nu32))
       detc  = one - nu*nu
       young = max(e1,e2,e3)
       c1    = third*young/(one - two*nu)
       a11   = max(e1,e2,e3)/detc
       gmax  = max(g12,g23,g31)
       ssp   = sqrt(max(a11,gmax)/ rho0)
       asrate = two*pi*fcut

       ! mat_param%asrate = asrate 
       ! mat_param%young = c1
       ! mat_param%nu = nu
       ! mat_param%shear = gmax
       ! mat_param%ssp  = sqrt(c1/rho0
       ! mat_param%bulk = c1
!-----------------------------------------
          ! parameters used outside the law
!-----------------------------------------
       pm(9)  = asrate            !  mat_param%asrate
       pm(20) = young             !  mat_param%young
       pm(21) = nu                !  mat_param%nu
       pm(22) = max(g12,g12,g23)  !  mat_param%shear
       pm(24) = a11
       pm(26) = five_over_6
       pm(27) = ssp     !  mat_param%ssp
       pm(32) = a11                !  max(e1,e2,e3)/detc
      ! still used in elements
       pm(33) = e1                !  mat_param%e11
       pm(34) = e2                !  mat_param%e22
       pm(186)= e2                !  mat_param%e33
       pm(35) = nu12              !  mat_param%n12
       pm(36) = nu21              !  mat_param%n21
       pm(37) = g12               !  mat_param%g12
       pm(38) = g23               !  mat_param%g23
       pm(39) = g31               !  mat_param%g31   
          ! parmat table
        israte     = 1
        parmat(1)  =  a11
        parmat(2)  = young
        parmat(3)  = nu 
        parmat(4)  = israte
        parmat(5)  = fcut
        parmat(16) = 1
! 
      ! pm table
        pm(1)  = rho0
        pm(89) = rho0
!     
      ! mtag variable activation
       mtag%g_pla  = 1
       mtag%l_pla  = 1
       mtag%g_epsd = 1
       mtag%l_epsd = 1
       mtag%g_seq  = 1
       mtag%l_seq  = 1
       mtag%g_dmg  = 1
       mtag%l_dmg  = 8
     ! number of output mod
      matparam%nmod = 5
      allocate(matparam%mode(matparam%nmod))
      matparam%mode(1) = "tension fiber damage "
      matparam%mode(2) = "compressive fiber damage "
      matparam%mode(3) = "tension transverse matrix damage"
      matparam%mode(4) = "compressive transverse matrix damage"
      matparam%mode(5) = "shear matrix damage"
!
      call init_mat_keyword(matparam ,"ELASTO_PLASTIC")
      call init_mat_keyword(matparam ,"incremental"   )
      call init_mat_keyword(matparam ,"HOOK")
      call init_mat_keyword(matparam ,"ORTHOTROPIC")
!
      ! properties compatibility
      call init_mat_keyword(matparam,"SHELL_ORTHOTROPIC")
      call init_mat_keyword(matparam,"SOLID_ORTHOTROPIC")
      call init_mat_keyword(matparam,"SOLID_ALL")
!------------------------- 
!     parameters printout
!-------------------------- 
      write(iout,1000) trim(titr),mat_id,ilaw
      write(iout,1050)
      if (is_encrypted) then
        write(iout,'(5x,a,//)')'confidential data'
      else
        write(iout,1200) rho0
        write(iout,1300) e1,e2,e3,g12,g23,g13,nu21,nu32,nu31
        write(iout,1400)  xt,slimt1,xc,slimc1
        write(iout,1500)  yt,slimt2,yc,slimc2
        write(iout,1700) sc, slims
        write(iout,1800) beta, alpha,fbrt,ycfac
        !
        write(iout,2000) ifxt,yfac_xt,ifxc,yfac_xc
        write(iout,2100) ifyt, yfac_yt, ifyc,yfac_yc
        write(iout,2300) ifsc,yfac_sc
        write(iout,2400 ) dfailt, dfailc,dfailm, dfails, efs,ncyred,ratio
        write(iout,2500) epsf, epsr, tsmd
        write(iout,2600) ti,twoway
        write(iout,2800) fcut
      endif     
!-----------------------------------------------------------------------
 1000 format(/                                                               &
       5x,a,/,                                                               &
       5x,'material number. . . . . . . . . . . . =',i10/,                   &
       5x,'material law . . . . . . . . . . . . . =',i10/)
 1050 format                                                                 &
      (5x,'material model : enhanced composite ',/,                         &
       5x,'----------------------------------',/)
 1200 format(                                                                &
       5x,'initial density . . . . . . . . . . . . . . . . .=',1pg20.13/) 
 1300 format(                                                                &
       5x,'elasticity parameters:                            ',/             &
       5x,'----------------------                            ',/             &
       7x,'young modulus in dir. 1 (fiber)  e1 . . . . . . .=',1pg20.13/     &
       7x,'young modulus in dir. 2 (matrix) e2 . . . . . . .=',1pg20.13/     &
       7x,'young modulus in dir. 3 (matrix) e3 . . . . . . .=',1pg20.13/     &
       7x,'shear modulus in plane 12 g12 . . . . . . . . . .=',1pg20.13/     &
       7x,'shear modulus in plane 23 g23 . . . . . . . . . .=',1pg20.13/     &
       7x,'shear modulus in plane 31 g13 . . . . . . . . . .=',1pg20.13/     &
       7x,'poisson ratio in plane 21 nu21. . . . . . . . . .=',1pg20.13/     &
       7x,'poisson ratio in plane 32 nu32. . . . . . . . . .=',1pg20.13/     &
       7x,'poisson ratio in plane 31 nu31. . . . . . . . . .=',1pg20.13/ )
 1400 format(                                                                     &
       5x,' fiber (dir. 1) parameters   :                            ',/          &
       5x,'---------------------------                               ',/          &
       7x,'longitudinal tensile strength  . . . .  . . . . . . . . =',1pg20.13/   &
       7x,'scale for minimum longitudinal tensile stress limit. . .=',1pg20.13/   &
       7x,'longitudinal compressive strength  . . . .  . . . . . . =',1pg20.13/   &
       7x,'scale for minimum compressive tensile stress limit. . . =',1pg20.13/ )

 1500 format(                                                                     &
       5x,'matrix (dir. 2) parameters   :                            ',/          &
       5x,'---------------------------                               ',/          &   
       7x,'transverse tensile strength  . . . .  . . . . . . . . =',1pg20.13/     &
       7x,'scale for minimum transverse tensile stress limit. . .=',1pg20.13/     &   
       7x,'transverse compressive strength  . . . .  . . . . . . =',1pg20.13/     &
       7x,'scale for minimum compressive tensile stress limit. . . =',1pg20.13/ )

 1700 format(                                                                    &
       5x,' shear (12) parameters   :                            ',/               &
       5x,'---------------------------                               ',/           &
       7x,'shear strength 12 . . . . . . . . . . . . . . . . . . . =',1pg20.13/    &
       7x,'scale for minimum shear stress limit 12 . . . . .. . . =',1pg20.13/ )
 1800 format(                                                                    &
       5x,'  parameters   :                            ',/               &
       5x,'---------------------------                               ',/           &
       7x,'Weighting factor for shear term in tensile fiber mode . =',1pg20.13/    &
       7x,'Shear stress parameter for the nonlinear term . . . . . =',1pg20.13/    &
       7x,'Softening for fiber tensile strength: . . . . . . . . . =',1pg20.13/    &
       7x, '   = 0.0: Tensile strength = XT',/                                      &
       7x, '   > 0.0: Tensile strength = XT, reduced to XT*FBRT after ',/           &
       7x, '       failure has occurred in compressive matrix mode',/                  &
       7x,'Reduction factor for compressive fiber strength . . . .=',1pg20.13 /  &
       10x,    ' after matrix compressive failure xc=yfac*yc ',/  )
       
 2000 format(                                                                   &
       5x,' fiber (dir. 1) strain rate dependency  :                 ',/          &
       5x,'---------------------------                               ',/          &
       7x,'curve id defining longitudinal tensile strength xt . . .  =',i10/      &
       7X,'scale of the curve defining xt . . . . . . . . . . . . .  =',1pg20.13 /&
       7x,'curve id defining longitudinal compresssive strength xc . =',i10/      &
       7X,'scale of the curve defining xc . . . . . . . . . . . . . .=',1pg20.13/ )
 2100 format(                                                                   &                                                              
       5x,' matrix (dir. 2) strain rate dependency  :                ',/        &
       5x,'---------------------------                               ',/        &
       7x,'curve id defining longitudinal tensile strength yt . . .  =',i10/    &
       7X,'scale of the curve defining yt . . . . . . . . . . . . .  =',1pg20.13 /&
       7x,'curve id defining longitudinal compresssive strength yc . =',i10/      &
       7X,'scale of the curve defining yc. . . . . . . . . . . . .  =',1pg20.13 /)
 2300 format(                                                                   &
       5x,' dir 12 - strain rate dependency  :                       ',/        &
       5x,'---------------------------                               ',/        &
       7x,'curve id defining shear strength sc . . . . . . .  . . . =',i10/     &
       7X,'scale of the curve defining sc. . . . . . . . . . . . .  =',1pg20.13 /) 

! Format for failure parameters used if dfailt > 0
 2400 format(                                                                   &                                                              
       5x,' failure parameters  used if dfailt > 0 :                ',/         &
       5x,'------------------------------------                              ',/           &
       7x,'Maximum strain for fiber tension. . . . . . . .. .  . . . . . . =',1pg20.13/    &
       7X,'Maximum strain for fiber compression  . . . . . .. . . . . . .  =',1pg20.13 /   &
       7x,'Maximum strain for matrix straining in tension or compression . =',1pg20.13 /   &
       7X,'Maximum tensorial shear strain. . . . . . . . . . . . . . . . . =',1pg20.13 /   &
       7x,'Effective failure strain . . . . . . . . . . . . . . . . . . . .=',1pg20.13 /   &
       7x,'Number of cycles for stress reduction from maximum to minimum . =',i10 /        &
       7x, 'Ratio Parameter Control to Delete Shell Elements . . . . . . . =',1pg20.13 /)         

! Format for transverse damage parameters
 2500 format(                                                                   &                                                              
       5x,' transverse damage parameters :                ',/                   &
       5x,'------------------------------------                              ',/           &
       7x,'Damage initiation transverse shear strain. . . . . . . .. .  .  =',1pg20.13/    &
       7X,'Final rupture transverse shear strain . . . . . .. . . . . . .  =',1pg20.13 /   &
       7x,'Transverse shear maximum damage  . . . . . . . . . . . . . . .  =',1pg20.13 /   )

! Format for formulation flags
 2600 format(                                                                   &                                                              
       5x,' formulation Flag  :                ',/                             &
       5x,'------------------------------------                              ',/           &
       7x,'Flag to turn on transversal isotropic behavior for solid   . .  =',i10/         &
       7x,'      =0 : Standard unidirectional behavior  ',/                                &    
       7x,'      =1 : Transversal isotropic behavior ',/                                   & 
       7X,'Flag to turn on 2-way fiber action: . . . . . . .. . . . . . .  =',i10 /        &
       7x,'      =0 : Standard unidirectional behavior, meaning fibers run',/             &
       7x,'            only in the a-direction' ,/                                        & 
       7x,'      =1 : fiber action, meaning fibers run in both the a and b',/             &
       7x,'            direction ', /)      

! Format for strain rate filtering cutoff frequency
 2800 format(                                                                    &
       5x,'strain rate filtering cutoff frequency fcut . . .=',1pg20.13/)

!-----------------------------------------------------------------------
        end subroutine hm_read_mat127
!-------------------
      end module hm_read_mat127_mod
